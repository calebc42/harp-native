;;; harp-profile.el --- Read Harp profile Org files -*- lexical-binding: t; -*-

;; The Harp schema read layer.  A profile is one Org file with a file-level
;; :ID:, :HARP_TYPE:, and a #+TITLE:/:HARP_NAME:, holding the sections
;; Metadata (Sources, Metrics), Journals (`** Name' subtrees of `*** Entry'),
;; Documents, and Reminders.  This module discovers such files under
;; `harp-directory' and parses one into a structured alist.
;;
;; It replaces harp-kmp's Kotlin OrgLexer + hand-written parser with native
;; org-mode: `org-map-entries' walks the tree, `org-get-outline-path' routes
;; each heading to its section, and `org-entry-get' reads properties.  Pure
;; elisp; no bridge dependencies (the views layer memoises calls here through
;; the core's `jetpacs-org-with-cache').

;;; Code:

(require 'org)
(require 'org-id)
(require 'cl-lib)
(require 'jetpacs-org)
(require 'harp-metrics-parse)

(defgroup harp nil
  "Harp — a privacy-centric Personal Health Record org app."
  :group 'applications
  :prefix "harp-")

(defcustom harp-directory (expand-file-name "harp/" "~/")
  "Directory holding Harp profile Org files (searched recursively).
Each profile is a `.org' file with a file-level :ID: and the Harp sections."
  :type 'directory
  :group 'harp)

(defconst harp-profile-types '("human" "cat" "dog")
  "Recognised :HARP_TYPE: values; anything else falls back to \"human\".")

;; ─── low-level helpers ───────────────────────────────────────────────────────

(defun harp--time-of (ts)
  "Float-time of Org timestamp string TS, or nil."
  (and (stringp ts) (not (string-empty-p ts))
       (ignore-errors (float-time (org-time-string-to-time ts)))))

(defun harp--visit-file (file)
  "Visit FILE for reading, silently re-reading it if it changed on disk.
Reads run from the background notify timer as well as from user actions;
an external sync edit (Syncthing, git) must never raise a blocking
\"changed on disk\" prompt — nor return stale content (the CLAUDE.md
gotcha).  NOWARN suppresses the prompt; the modtime check then reverts
without confirmation when the disk copy is newer."
  (let ((buf (find-file-noselect file t)))
    (with-current-buffer buf
      (unless (verify-visited-file-modtime buf)
        (let ((revert-without-query (list (regexp-quote file))))
          (revert-buffer :ignore-auto :noconfirm :preserve-modes))))
    buf))

(defun harp--encode-local (decoded)
  "Encode DECODED (a `decode-time' list) re-resolving the local DST offset.
Clearing the zone/dst fields makes `encode-time' recompute the UTC offset
for the (possibly DST-shifted) date, so stepping calendar fields — day,
month, year — preserves the wall-clock time across a DST transition rather
than drifting an hour."
  (setf (decoded-time-dst decoded) -1
        (decoded-time-zone decoded) nil)
  (encode-time decoded))

(defun harp--sort-by-time-desc (items)
  "ITEMS (alists with a `time' key) sorted newest first."
  (sort (copy-sequence items)
        (lambda (a b) (> (or (alist-get 'time a) 0) (or (alist-get 'time b) 0)))))

(defun harp--attachment-links (text)
  "File names in [[attachment:NAME]] links within TEXT, first-seen order."
  (let ((names '()) (start 0))
    (while (and (stringp text)
                (string-match "\\[\\[attachment:\\([^]]+\\)\\]\\]" text start))
      ;; Bind the group AND the scan position BEFORE `string-trim' — it calls
      ;; `string-match' and would clobber the match data, so reading
      ;; `(match-end 0)' after it walks `start' backwards and hangs Emacs.
      (let ((n (match-string 1 text))
            (end (match-end 0)))
        (setq start end
              n (string-trim n))
        (unless (or (string-empty-p n) (member n names)) (push n names))))
    (nreverse names)))

(defun harp--split-files (s)
  "Split a comma-separated :FILES: property S into trimmed, non-empty names."
  (when (stringp s)
    (cl-remove-if #'string-empty-p (mapcar #'string-trim (split-string s "," t)))))

(defun harp--type-of (raw)
  "Normalise a :HARP_TYPE: value RAW to one of `harp-profile-types'."
  (let ((tp (and (stringp raw) (downcase (string-trim raw)))))
    (if (member tp harp-profile-types) tp "human")))

(defun harp--tags-at-point ()
  "Local heading tags at point, minus org-attach's ATTACH marker."
  (cl-remove "ATTACH" (org-get-tags nil t) :test #'equal))

(defun harp--body-at-point ()
  "Body text of the subtree at point: content after meta-data, trimmed."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t))))
      (org-end-of-meta-data t)
      (if (>= (point) end)
          ""
        (string-trim (buffer-substring-no-properties (point) end))))))

(defun harp--content-before-children ()
  "Body text of the entry at point, excluding any child subtrees.
Content after the heading's meta-data (property and log drawers) up to the
first child heading, or the subtree end when there are no children."
  (save-excursion
    (org-back-to-heading t)
    (let ((limit (save-excursion
                   (or (and (outline-next-heading) (point))
                       (org-end-of-subtree t t)))))
      (org-end-of-meta-data t)
      (if (>= (point) limit)
          ""
        (string-trim (buffer-substring-no-properties (point) limit))))))

(defun harp--strip-logbook (text)
  "Remove any `:LOGBOOK: … :END:' drawer block from TEXT.
A hand-authored medication with notes ABOVE its logbook would otherwise
capture the drawer into `notes', and an edit would then re-emit it twice."
  (if (stringp text)
      (string-trim
       (replace-regexp-in-string
        "^[ \t]*:LOGBOOK:[ \t]*\n\\(?:.*\n\\)*?[ \t]*:END:[ \t]*\n?" ""
        text))
    text))

(defun harp--prop-bool (val default)
  "Interpret property string VAL as a boolean; nil/absent -> DEFAULT.
Note: `org-entry-get' returns nil for a literal `nil' property value
\(org treats it as unset), so inactive is written as `no', not `nil'."
  (cond ((null val) default)
        ((member (downcase (string-trim val)) '("no" "false" "off" "0" "n" "")) nil)
        (t t)))

(defun harp--metric-values (body dt time)
  "Metric-reading alists harvested from BODY, stamped with DT/TIME."
  (mapcar (lambda (m)
            `((id . ,(car m)) (value . ,(cdr m)) (datetime . ,dt) (time . ,time)))
          (harp-inline-metrics body)))

(defun harp--merge-tags (heading-tags body-tags)
  "Sorted union of HEADING-TAGS and BODY-TAGS (strings)."
  (sort (cl-remove-duplicates (append heading-tags body-tags) :test #'equal)
        #'string<))

(defun harp--resolve-ref (ref)
  "Resolve REF to a live marker at its org heading, ID-first and file-scoped.
Unlike `jetpacs-org-resolve-ref' this NEVER trusts REF's recorded position
for an ID-bearing item — every Harp journal entry has the headline
\"Entry\", so a positional guess after an unrelated byte shift would delete
and overwrite the WRONG subtree.  It also searches only REF's own file for
its `:ID:', so it never triggers `org-id''s full-tree location rescan (Harp
writes IDs without registering them).  Falls back to the core resolver only
for ID-less refs."
  (let ((id (alist-get 'id ref))
        (file (alist-get 'file ref)))
    (if (and (stringp id) (not (string-empty-p id))
             (stringp file) (file-readable-p file))
        (with-current-buffer (harp--visit-file file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (if (re-search-forward
                (format "^[ \t]*:ID:[ \t]+%s[ \t]*$" (regexp-quote id)) nil t)
               (save-excursion (org-back-to-heading t) (point-marker))
             (error "Harp: item %s not found in %s" id file))))
      (jetpacs-org-resolve-ref ref))))

;; ─── per-heading readers (point is on the heading) ──────────────────────────

(defun harp--read-metric ()
  "Read a metric definition at point (`*** Name' under Metadata/Metrics)."
  (let ((tag-id (org-entry-get (point) "TAG_ID"))
        (unit (org-entry-get (point) "UNIT")))
    (when (and tag-id unit)              ; harp-kmp requires TAG_ID and UNIT
      `((id . ,(string-trim tag-id))
        (name . ,(nth 4 (org-heading-components)))
        (unit . ,(string-trim unit))
        (tags . ,(harp--tags-at-point))
        (range . ,(harp-parse-range (org-entry-get (point) "RANGE")))
        (healthy-range . ,(harp-parse-range (org-entry-get (point) "HEALTHY_RANGE")))
        (ref . ,(jetpacs-org-heading-ref))))))

(defun harp--read-entry ()
  "Read a journal entry at point (`*** Entry' under a journal)."
  (let ((id (org-entry-get (point) "ID"))
        (dt (org-entry-get (point) "DATETIME")))
    (when (and id dt)                    ; harp-kmp requires ID and DATETIME
      (let* ((time (harp--time-of dt))
             (body (harp--body-at-point))
             (scan (harp-inline-scan body)))
        `((id . ,(string-trim id))
          (datetime . ,dt)
          (time . ,time)
          (private . ,(equal (org-entry-get (point) "PRIVATE") "t"))
          (tags . ,(harp--merge-tags (harp--tags-at-point) (plist-get scan :tags)))
          (heading-tags . ,(harp--tags-at-point))
          (body . ,body)
          (metric-values . ,(harp--metric-values body dt time))
          (assets . ,(harp--attachment-links body))
          (ref . ,(jetpacs-org-heading-ref)))))))

(defun harp--read-document ()
  "Read a document at point (`** Name' under Documents)."
  (let ((id (org-entry-get (point) "ID"))
        (dt (org-entry-get (point) "DATETIME"))
        (files (org-entry-get (point) "FILES")))
    ;; harp-kmp requires FILES too, but a document can legitimately exist
    ;; before its scan is attached, so we require only ID + DATETIME.
    (when (and id dt)
      (let* ((time (harp--time-of dt))
             (source (org-entry-get (point) "SOURCE"))
             (annotation (harp--body-at-point))
             (scan (harp-inline-scan annotation)))
        `((id . ,(string-trim id))
          (name . ,(nth 4 (org-heading-components)))
          (datetime . ,dt)
          (time . ,time)
          (source . ,(and source (not (string-empty-p (string-trim source)))
                          (string-trim source)))
          (files . ,(harp--split-files files))
          (assets . ,(harp--split-files files))
          (private . ,(equal (org-entry-get (point) "PRIVATE") "t"))
          (tags . ,(harp--merge-tags (harp--tags-at-point) (plist-get scan :tags)))
          (heading-tags . ,(harp--tags-at-point))
          (annotation . ,annotation)
          (metric-values . ,(harp--metric-values annotation dt time))
          (ref . ,(jetpacs-org-heading-ref)))))))

(defun harp--read-reminder ()
  "Read a reminder at point (`** Title' under Reminders)."
  (let ((id (org-entry-get (point) "ID"))
        (sched (org-entry-get (point) "SCHEDULED"))
        (deadline (org-entry-get (point) "DEADLINE")))
    (when (and id (or sched deadline))   ; harp-kmp requires ID + a stamp
      (let ((c (org-heading-components)))
        `((id . ,(string-trim id))
          (title . ,(nth 4 c))
          (todo . ,(nth 2 c))
          (priority . ,(and (nth 3 c) (char-to-string (nth 3 c))))
          (tags . ,(harp--tags-at-point))
          (scheduled . ,sched)
          (deadline . ,deadline)
          (scheduled-time . ,(harp--time-of sched))
          (deadline-time . ,(harp--time-of deadline))
          (body . ,(harp--body-at-point))
          (ref . ,(jetpacs-org-heading-ref)))))))

;; ─── medications (a `* Medications' section) ────────────────────────────────

(defun harp--split-times (s)
  "Split a :TIMES: property S (comma/space separated HH:MM) into a list."
  (when (stringp s)
    (mapcar #'harp--normalize-hm
            (cl-remove-if-not
             (lambda (x) (string-match-p "\\`[0-9]\\{1,2\\}:[0-9]\\{2\\}\\'" x))
             (mapcar #'string-trim (split-string s "[ ,]+" t))))))

(defun harp--normalize-hm (hm)
  "Zero-pad the hour of an \"H:MM\"/\"HH:MM\" string HM (\"9:00\" -> \"09:00\").
Dose times are sorted as strings, so an unpadded hour would sort after
\"21:00\" and misorder the due list."
  (if (string-match "\\`\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\'" hm)
      (format "%02d:%s" (string-to-number (match-string 1 hm)) (match-string 2 hm))
    hm))

(defun harp--split-days (s)
  "Split a :DAYS: property S into weekday abbrevs; \"all\"/blank -> nil."
  (when (and (stringp s) (not (string-empty-p (string-trim s)))
             (not (string-equal-ignore-case (string-trim s) "all")))
    (mapcar #'capitalize (split-string s "[ ,]+" t))))

(defun harp--dose-events-at-point ()
  "Parse the med's :LOGBOOK: dose lines: `- STATE [ts] AMOUNT'.
Scans only the med heading's own content (before its schedule children)."
  (save-excursion
    (org-back-to-heading t)
    (let* ((start (point))
           (region-end (save-excursion
                         (or (and (outline-next-heading) (point))
                             (org-end-of-subtree t t))))
           events)
      (goto-char start)
      (when (re-search-forward "^[ \t]*:LOGBOOK:[ \t]*$" region-end t)
        (let ((drawer-end (save-excursion
                            (if (re-search-forward "^[ \t]*:END:[ \t]*$" region-end t)
                                (line-beginning-position)
                              region-end))))
          (while (re-search-forward
                  "^[ \t]*- \\(taken\\|skipped\\)[ \t]+\\[\\([^]]+\\)\\]\\(?:[ \t]+\\(.*?\\)\\)?[ \t]*$"
                  drawer-end t)
            ;; Grab all three groups before any `string-trim' (it clobbers
            ;; the match data, which would drop AMOUNT).
            (let ((state (match-string 1))
                  (ts (match-string 2))
                  (amount (match-string 3)))
              (setq ts (string-trim ts)
                    amount (and amount (string-trim amount)))
              (push `((state . ,state)
                      (stamp . ,ts)
                      (time . ,(harp--time-of (concat "[" ts "]")))
                      (amount . ,(and amount (not (string-empty-p amount)) amount)))
                    events)))))
      (nreverse events))))

(defun harp--read-schedule ()
  "Read a `*** Schedule' entry under a medication."
  `((amount . ,(org-entry-get (point) "AMOUNT"))
    (times . ,(harp--split-times (org-entry-get (point) "TIMES")))
    (days . ,(harp--split-days (org-entry-get (point) "DAYS")))
    (cycle-start . ,(org-entry-get (point) "CYCLE_START"))
    (active-days . ,(harp-parse--num-or-nil (org-entry-get (point) "ACTIVE_DAYS")))
    (pause-days . ,(harp-parse--num-or-nil (org-entry-get (point) "PAUSE_DAYS")))
    (interval . ,(org-entry-get (point) "INTERVAL"))
    (window . ,(org-entry-get (point) "WINDOW"))
    (from . ,(org-entry-get (point) "FROM"))
    (until . ,(org-entry-get (point) "UNTIL"))
    (ref . ,(jetpacs-org-heading-ref))))

(defun harp--read-medication ()
  "Read a medication at point (`** Name' under Medications); nil without :ID:."
  (let ((id (org-entry-get (point) "ID")))
    (when (and id (not (string-empty-p (string-trim id))))
      `((id . ,(string-trim id))
        (name . ,(nth 4 (org-heading-components)))
        (unit . ,(let ((u (org-entry-get (point) "UNIT")))
                   (and u (string-trim u))))
        (stock . ,(harp-parse--num-or-nil (org-entry-get (point) "STOCK")))
        (refill . ,(harp-parse--num-or-nil (org-entry-get (point) "REFILL")))
        (low-stock . ,(harp-parse--num-or-nil (org-entry-get (point) "LOW_STOCK")))
        (expires . ,(org-entry-get (point) "EXPIRES"))
        ;; Absent :ACTIVE: means active; write `no' (not `nil') to deactivate.
        (active . ,(harp--prop-bool (org-entry-get (point) "ACTIVE") t))
        (notes . ,(harp--strip-logbook (harp--content-before-children)))
        (tags . ,(harp--tags-at-point))
        (doses . ,(harp--dose-events-at-point))
        (schedules . nil)
        (ref . ,(jetpacs-org-heading-ref))))))

;; ─── whole-file read ────────────────────────────────────────────────────────

(defun harp-profile-read (file)
  "Read FILE into a Harp profile alist, or nil if it is not a profile.
The alist has keys `uuid' `name' `type' `file' `metrics' `journals'
`documents' `reminders'.  Journals are (`name' . `entries'); entries and
documents are sorted newest-first, mirroring harp-kmp."
  (when (and file (file-readable-p file))
    (with-current-buffer (harp--visit-file file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (let ((uuid (org-entry-get (point-min) "ID")))
         (when (and uuid (not (string-empty-p (string-trim uuid))))
           (let ((type (harp--type-of (org-entry-get (point-min) "HARP_TYPE")))
                 (name (or (org-entry-get (point-min) "HARP_NAME")
                           (org-get-title) ""))
                 (metrics '()) (documents '()) (reminders '())
                 (journal-order '()) (entries '())    ; entries: (JNAME . ENTRY)
                 (meds '()))                          ; schedules attach in-walk
             (org-map-entries
              (lambda ()
                (let* ((c (org-heading-components))
                       (lvl (nth 0 c))
                       (path (org-get-outline-path)))
                  (cond
                   ((and (= lvl 3) (equal path '("Metadata" "Metrics")))
                    (when-let ((m (harp--read-metric))) (push m metrics)))
                   ((and (= lvl 2) (equal path '("Journals")))
                    (let ((jn (nth 4 c)))
                      (unless (member jn journal-order) (push jn journal-order))))
                   ((and (= lvl 3) (equal (car path) "Journals"))
                    (when-let ((e (harp--read-entry)))
                      (push (cons (nth 1 path) e) entries)))
                   ((and (= lvl 2) (equal (car path) "Documents"))
                    (when-let ((d (harp--read-document))) (push d documents)))
                   ((and (= lvl 2) (equal (car path) "Reminders"))
                    (when-let ((r (harp--read-reminder))) (push r reminders)))
                   ((and (= lvl 2) (equal path '("Medications")))
                    (when-let ((m (harp--read-medication))) (push m meds)))
                   ((and (= lvl 3) (equal (car path) "Medications"))
                    ;; Attach to the med just walked past — traversal order
                    ;; guarantees it is the parent — so two meds sharing a
                    ;; name don't each collect the other's schedules.  Rebuild
                    ;; the med with a FRESH schedules pair rather than mutating
                    ;; in place: `harp--read-medication's placeholder is a
                    ;; backquote shared-constant cons reused across meds, so an
                    ;; in-place setcdr would corrupt every medication at once.
                    (when meds
                      (let* ((m (car meds))
                             (scheds (append (alist-get 'schedules m)
                                             (list (harp--read-schedule)))))
                        (setcar meds
                                (cons (cons 'schedules scheds)
                                      (assq-delete-all 'schedules (copy-alist m))))))))))
              nil nil)
             (setq journal-order (nreverse journal-order))
             (let ((journals
                    (mapcar
                     (lambda (jn)
                       `((name . ,jn)
                         (entries . ,(harp--sort-by-time-desc
                                      (mapcar #'cdr
                                              (cl-remove-if-not
                                               (lambda (pair) (equal (car pair) jn))
                                               entries))))))
                     journal-order)))
               ;; Schedules were attached to each med in document order during
               ;; the walk; the meds just need restoring to document order.
               (let ((medications (nreverse meds)))
                 `((uuid . ,(string-trim uuid))
                   (name . ,(string-trim name))
                   (type . ,type)
                   (file . ,file)
                   (metrics . ,(nreverse metrics))
                   (journals . ,journals)
                   (documents . ,(harp--sort-by-time-desc (nreverse documents)))
                   (reminders . ,(nreverse reminders))
                   (medications . ,medications)))))))))))

;; ─── discovery ──────────────────────────────────────────────────────────────

(defun harp-profile--head-props (file)
  "Head-read of FILE's file-level property drawer, or nil when not a profile.
Reads only the first 4KB into a temp buffer — discovery sweeps every `.org'
under `harp-directory' recursively, and `find-file-noselect' per candidate
would leave a visiting buffer behind for each.  Returns an alist with `id',
`name', `type', `title' (any of the last three may be nil)."
  (when (and (stringp file) (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file nil 0 4096)
      (goto-char (point-min))
      ;; The FILE-level drawer must open the file (matching the previous
      ;; `org-entry-get (point-min)' semantics).
      (when (looking-at "[ \t]*:PROPERTIES:")
        (let ((end (save-excursion
                     (or (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                         (point-max))))
              (grab (lambda (re bound)
                      (save-excursion
                        (when (re-search-forward re bound t)
                          (match-string 1)))))
              )
          (let ((id (funcall grab "^[ \t]*:ID:[ \t]*\\(.+?\\)[ \t]*$" end)))
            (when (and id (not (string-empty-p (string-trim id))))
              `((id . ,id)
                (name . ,(funcall grab "^[ \t]*:HARP_NAME:[ \t]*\\(.+?\\)[ \t]*$" end))
                (type . ,(funcall grab "^[ \t]*:HARP_TYPE:[ \t]*\\(.+?\\)[ \t]*$" end))
                (title . ,(funcall grab "^[ \t]*#\\+TITLE:[ \t]*\\(.+?\\)[ \t]*$" nil))))))))))

(defun harp-profile-file-p (file)
  "Non-nil if FILE looks like a Harp profile (has a file-level :ID:)."
  (and (harp-profile--head-props file) t))

(defun harp-profile-files ()
  "List of Harp profile `.org' files found under `harp-directory'."
  (when (and harp-directory (file-directory-p harp-directory))
    (cl-remove-if-not #'harp-profile-file-p
                      (directory-files-recursively harp-directory "\\.org\\'"))))

(defun harp-profile-info (file)
  "Cheap profile summary alist (`uuid' `name' `type' `file') for FILE, or nil.
Head-read only — the launcher list opens no buffers and runs no org parse."
  (when-let ((head (harp-profile--head-props file)))
    `((uuid . ,(string-trim (alist-get 'id head)))
      (name . ,(string-trim (or (alist-get 'name head)
                                (alist-get 'title head) "")))
      (type . ,(harp--type-of (alist-get 'type head)))
      (file . ,file))))

(defun harp-profiles ()
  "Summary alists for every discovered profile, name-sorted."
  (sort (delq nil (mapcar #'harp-profile-info (harp-profile-files)))
        (lambda (a b) (string< (downcase (alist-get 'name a ""))
                               (downcase (alist-get 'name b ""))))))

;; ─── derived queries ────────────────────────────────────────────────────────

(defun harp-profile-metric-values (profile id)
  "All readings for metric ID across PROFILE's entries and documents.
Sorted oldest-first — the shape a time-series chart wants."
  (let ((vals '()))
    (dolist (j (alist-get 'journals profile))
      (dolist (e (alist-get 'entries j))
        (dolist (mv (alist-get 'metric-values e))
          (when (equal (alist-get 'id mv) id) (push mv vals)))))
    (dolist (d (alist-get 'documents profile))
      (dolist (mv (alist-get 'metric-values d))
        (when (equal (alist-get 'id mv) id) (push mv vals))))
    (sort vals (lambda (a b) (< (or (alist-get 'time a) 0)
                                (or (alist-get 'time b) 0))))))

(defun harp-profile-tags (profile)
  "Sorted list of every unique tag used anywhere in PROFILE (minus ATTACH)."
  (let ((tags '()))
    (dolist (j (alist-get 'journals profile))
      (dolist (e (alist-get 'entries j))
        (setq tags (append (alist-get 'tags e) tags))))
    (dolist (d (alist-get 'documents profile))
      (setq tags (append (alist-get 'tags d) tags)))
    (dolist (m (alist-get 'metrics profile))
      (setq tags (append (alist-get 'tags m) tags)))
    (dolist (r (alist-get 'reminders profile))
      (setq tags (append (alist-get 'tags r) tags)))
    (sort (cl-remove-duplicates (cl-remove "ATTACH" tags :test #'equal) :test #'equal)
          #'string<)))

(provide 'harp-profile)
;;; harp-profile.el ends here
