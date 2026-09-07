;;; harp.el --- Harp Personal Health Record (Jetpacs Tier-1 app), single-file bundle -*- lexical-binding: t; -*-
;;
;; GENERATED FILE -- do not edit by hand.
;; Produced by emacs/build-bundle.el from the emacs/apps/ sources.
;; Concatenated in dependency order; each part keeps its own `provide',
;; and the bundle-internal `require' forms are commented out so the
;; file BYTE-COMPILES standalone: the foundation compiles an adopted
;; bundle before loading it, when none of these features exist yet —
;; a surviving hard require is a compile error and a broken .elc.
;;
;; Requires the Jetpacs core (jetpacs-core.el) on `load-path' first.
;;
;;; Code:

(require 'jetpacs-core)

;;; ==================================================================
;;; BEGIN apps/harp/harp-metrics-parse.el
;;; ==================================================================

;;; harp-metrics-parse.el --- Inline #metric(value)/#tag and range parsing -*- lexical-binding: t; -*-

;; The one Harp-specific micro-parser.  Harp entry and document bodies carry
;; two inline tokens borrowed from harp-kmp's org dialect:
;;
;;   #tag            a bare hashtag (e.g. #routine)
;;   #id(value)      a metric reading (e.g. #hr(102), #pain(4))
;;
;; where `id' matches a metric's :TAG_ID: in the profile's Metadata.  This
;; module scans a body string into structured metric readings and tags, and
;; parses the "lo-hi" RANGE / HEALTHY_RANGE property strings.  Pure elisp.

;;; Code:

(require 'cl-lib)

(defconst harp-inline--regexp
  "#\\([A-Za-z0-9_-]+\\)\\(?:(\\([-+]?[0-9]*\\.?[0-9]+\\))\\)?"
  "Match an inline #tag or #metric(value).
Group 1 is the id/tag word; group 2 is the numeric value, present only for
the #id(value) form.")

(defun harp-inline-scan (text)
  "Scan TEXT for inline #tag and #metric(value) tokens.
Return a plist (:metrics ((ID . VALUE)...) :tags (TAG...)): ids and tags are
strings, values numbers.  Metric readings keep every occurrence in
first-seen order (mirroring harp-kmp); tags are de-duplicated."
  (let ((metrics '()) (tags '()) (start 0))
    (while (and (stringp text) (string-match harp-inline--regexp text start))
      (let ((word (match-string 1 text))
            (val (match-string 2 text)))
        (setq start (match-end 0))
        (if val
            (push (cons word (string-to-number val)) metrics)
          (unless (member word tags) (push word tags)))))
    (list :metrics (nreverse metrics) :tags (nreverse tags))))

(defun harp-inline-metrics (text)
  "Metric readings ((ID . VALUE)...) harvested from TEXT."
  (plist-get (harp-inline-scan text) :metrics))

(defun harp-inline-tags (text)
  "Hashtag strings harvested from TEXT."
  (plist-get (harp-inline-scan text) :tags))

(defun harp-parse--num-or-nil (s)
  "Parse S as a number, or nil when blank, \"null\", or non-numeric."
  (when (stringp s)
    (setq s (string-trim s))
    (when (and (not (string-empty-p s))
               (not (string-equal (downcase s) "null"))
               (string-match-p "\\`[-+]?[0-9]*\\.?[0-9]+\\'" s))
      (string-to-number s))))

(defun harp-parse-range (str)
  "Parse a RANGE / HEALTHY_RANGE string \"lo-hi\" into a cons (LO . HI).
Either bound may be nil.  Mirrors harp-kmp: split on \"-\" into exactly two
parts, otherwise (nil . nil)."
  (if (not (stringp str))
      (cons nil nil)
    (let ((parts (split-string (string-trim str) "-")))
      (if (= (length parts) 2)
          (cons (harp-parse--num-or-nil (nth 0 parts))
                (harp-parse--num-or-nil (nth 1 parts)))
        (cons nil nil)))))

(provide 'harp-metrics-parse)
;;; harp-metrics-parse.el ends here

;;; ==================================================================
;;; BEGIN apps/harp/harp-profile.el
;;; ==================================================================

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
;; (require 'harp-metrics-parse) — bundle-internal, provided above

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

;;; ==================================================================
;;; BEGIN apps/harp/harp-ui.el
;;; ==================================================================

;;; harp-ui.el --- Harp shared UI state and spec helpers -*- lexical-binding: t; -*-

;; The current-profile state and the small spec helpers every Harp view
;; shares: type icons, friendly timestamps, tag/metric/attachment chips, the
;; tag filter bar, and the "no profile open" placeholder.  Reading a profile
;; is memoised on (file . mtime) so one push renders every tab from a single
;; parse; the memo busts when the file changes on disk or a mutation calls
;; `harp-ui-invalidate'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'jetpacs-widgets)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
;; (require 'harp-profile) — bundle-internal, provided above

;; ─── current profile state ──────────────────────────────────────────────────

(defvar harp-ui--current-file nil
  "Absolute path of the open profile Org file, or nil when none is open.")

(defvar harp-ui--cache nil
  "Memo cons (KEY . PROFILE) for the current profile read.")

(defvar harp-ui--derived nil
  "Push-scoped memo (GEN . HASH) for derived queries.
GEN mirrors `harp-ui--cache''s key, so the memo busts on the same
\(file . mtime) change that re-reads the profile; `harp-ui-invalidate'
clears it outright for the no-profile-open case (where GEN stays nil).
A shell push builds every visible tab, so a derived query (tags,
due-today, the launcher sweep) would otherwise recompute several times.")

(defvar harp-ui--tag-filter nil
  "Active tag filter string, or nil for no filter.")

(defcustom harp-hide-private nil
  "When non-nil, entries and documents marked :PRIVATE: are hidden from views."
  :type 'boolean
  :group 'harp)

(defun harp-ui-invalidate ()
  "Drop the memoised profile read and derived memos (call after any mutation)."
  (setq harp-ui--cache nil
        harp-ui--derived nil))

(defun harp-ui-set-current (file)
  "Open FILE as the current profile, resetting the read memo and tag filter."
  (setq harp-ui--current-file file
        harp-ui--cache nil
        harp-ui--derived nil
        harp-ui--tag-filter nil))

(defun harp-ui-memo (key thunk)
  "Return (funcall THUNK), memoised under KEY for the current profile read.
Every builder in one shell push shares the result, and the memo busts on
the same (file . mtime) change or explicit invalidate as `harp-ui-current'."
  ;; Force the profile read first so GEN reflects the current (file . mtime),
  ;; independent of whether a builder called `harp-ui-current' before us — else
  ;; a memo call ordered first would key the table under a stale generation.
  (harp-ui-current)
  (let ((gen (car harp-ui--cache)))
    (unless (and harp-ui--derived (equal (car harp-ui--derived) gen))
      (setq harp-ui--derived (cons gen (make-hash-table :test 'equal))))
    (let* ((tbl (cdr harp-ui--derived))
           (cell (gethash key tbl)))
      (if cell (car cell)
        (car (puthash key (list (funcall thunk)) tbl))))))

(defun harp-ui-current-tags ()
  "Tags of the current profile, memoised for the push (built by two tabs)."
  (harp-ui-memo 'tags (lambda () (harp-profile-tags (harp-ui-current)))))

(defun harp-ui-profiles ()
  "Discovered profiles, memoised for the push (the launcher's directory sweep)."
  (harp-ui-memo 'profiles #'harp-profiles))

(defun harp-ui-profile-open-p ()
  "Non-nil when a profile is open — the `:when' gate for section tabs."
  (and (harp-ui-current) t))

(defun harp-ui-current ()
  "The current profile alist, memoised on (file . mtime); nil when none open."
  (when (and harp-ui--current-file (file-readable-p harp-ui--current-file))
    (let ((key (cons harp-ui--current-file
                     (file-attribute-modification-time
                      (file-attributes harp-ui--current-file)))))
      (unless (equal (car harp-ui--cache) key)
        (setq harp-ui--cache
              (cons key (harp-profile-read harp-ui--current-file))))
      (cdr harp-ui--cache))))

;; ─── formatting helpers ─────────────────────────────────────────────────────

(defun harp-ui-type-icon (type)
  "Material icon name for a profile TYPE."
  (pcase type ("cat" "pets") ("dog" "pets") (_ "person")))

(defun harp-ui-type-label (type)
  "Human label for a profile TYPE."
  (pcase type ("cat" "Cat") ("dog" "Dog") (_ "Human")))

(defun harp-ui-format-stamp (dt &optional with-time)
  "Friendly label for Org timestamp string DT (e.g. \"Jan 3, 2026\").
With WITH-TIME non-nil, append the clock time when present."
  (if-let* ((time (harp--time-of dt)))
      (let* ((has-hm (and (stringp dt) (string-match-p "[0-9]:[0-9]" dt)))
             (fmt (if (and with-time has-hm) "%b %-d, %Y · %H:%M" "%b %-d, %Y")))
        (format-time-string fmt (seconds-to-time time)))
    (or dt "")))

;; ─── chips ──────────────────────────────────────────────────────────────────

(defun harp-ui-tag-chips (tags)
  "A flow-row of tappable #TAGS (each filters the active view), or nil."
  (when tags
    (apply #'jetpacs-flow-row
           (mapcar (lambda (tg)
                     (jetpacs-assist-chip (concat "#" tg)
                                          :on-tap (jetpacs-action
                                                   "harp.filter.tag"
                                                   :args `((tag . ,tg))
                                                   :when-offline "drop")))
                   tags))))

(defun harp-ui-metric-chips (metric-values)
  "A flow-row of metric readings as chips (\"id = value\"), or nil."
  (when metric-values
    (apply #'jetpacs-flow-row
           (mapcar (lambda (mv)
                     (jetpacs-assist-chip
                      (format "%s = %s" (alist-get 'id mv) (alist-get 'value mv))
                      :icon "monitoring"))
                   metric-values))))

(defun harp-ui-attachment-chips (assets)
  "A flow-row of attachment file names as chips, or nil."
  (when assets
    (apply #'jetpacs-flow-row
           (mapcar (lambda (name)
                     (jetpacs-assist-chip name :icon "attach_file"))
                   assets))))

(defun harp-ui-body-nodes (text)
  "Spec nodes for an entry/annotation body TEXT (org markup), or nil."
  (when (and (stringp text) (not (string-empty-p (string-trim text))))
    (list (jetpacs-markup (string-trim text) :syntax "org"))))

;; ─── tag filter ─────────────────────────────────────────────────────────────

(defun harp-ui-item-visible-p (item)
  "Non-nil when ITEM passes the private-hiding setting and the tag filter."
  (and (not (and harp-hide-private (alist-get 'private item)))
       (or (null harp-ui--tag-filter)
           (member harp-ui--tag-filter (alist-get 'tags item)))))

(defun harp-ui-filter-bar (tags)
  "A filter chip row over TAGS, with the active one selected; nil if no tags."
  (when tags
    (apply #'jetpacs-flow-row
           (jetpacs-chip "All"
                         :selected (null harp-ui--tag-filter)
                         :on-tap (jetpacs-action "harp.filter.clear"
                                                 :when-offline "drop"))
           (mapcar (lambda (tg)
                     (jetpacs-chip (concat "#" tg)
                                   :selected (equal tg harp-ui--tag-filter)
                                   :on-tap (jetpacs-action "harp.filter.tag"
                                                           :args `((tag . ,tg))
                                                           :when-offline "drop")))
                   tags))))

;; ─── placeholders ───────────────────────────────────────────────────────────

(defun harp-ui-no-profile-body ()
  "Placeholder body shown on a section tab when no profile is open."
  (jetpacs-empty-state
   :icon "folder_open"
   :title "No profile open"
   :caption "Open a profile from the Home tab to see its records."))

;; ─── filter actions (owned by the app) ──────────────────────────────────────

(with-jetpacs-owner "harp"
  (jetpacs-defaction "harp.filter.tag"
    (lambda (args _)
      (let ((tag (alist-get 'tag args)))
        (setq harp-ui--tag-filter
              (if (equal tag harp-ui--tag-filter) nil tag))
        (jetpacs-shell-push)))
    :doc "Toggle the active view's tag filter."
    :args '((:name tag :type "text" :required t)))

  (jetpacs-defaction "harp.filter.clear"
    (lambda (_args _)
      (setq harp-ui--tag-filter nil)
      (jetpacs-shell-push))
    :doc "Clear the active view's tag filter."))

;; Leaving a section resets its tag filter — a fresh view starts unfiltered.
(defun harp-ui--on-view-switched (_view)
  "Reset the tag filter when the shell switches views."
  (setq harp-ui--tag-filter nil))

(add-hook 'jetpacs-shell-view-switched-hook #'harp-ui--on-view-switched)

;; Pull-to-refresh and queue drains drop the profile memo.
(add-hook 'jetpacs-shell-refresh-hook #'harp-ui-invalidate)

(provide 'harp-ui)
;;; harp-ui.el ends here

;;; ==================================================================
;;; BEGIN apps/harp/harp-write.el
;;; ==================================================================

;;; harp-write.el --- Harp mutation layer -*- lexical-binding: t; -*-

;; Every write path into a profile Org file: add/replace a journal entry, a
;; document, or a metric definition; toggle an item's privacy; create a new
;; profile from the template.  Pure buffer edits under the profile file, each
;; saving synchronously and dropping the read memo so the next push reflects
;; the change.  The UI (harp-form.el) collects the fields; this module owns
;; the org text.
;;
;; Deferred, like OCR: attaching a phone-side file needs a companion→Emacs
;; transfer the base protocol has no primitive for, so `:FILES:' is entered
;; as text for now.

;;; Code:

(require 'org)
(require 'org-id)
(require 'cl-lib)
(require 'subr-x)
(require 'jetpacs-org)
;; (require 'harp-profile) — bundle-internal, provided above
;; (require 'harp-ui) — bundle-internal, provided above

(defun harp-write--buffer ()
  "The live buffer for the current profile file (error when none open)."
  (unless harp-ui--current-file (error "No profile open"))
  (find-file-noselect harp-ui--current-file))

(defun harp-write--save (buffer)
  "Save BUFFER synchronously and drop the read memo."
  (with-current-buffer buffer
    (let ((save-silently t)) (save-buffer)))
  (harp-ui-invalidate))

(defun harp-write--stamp (date time)
  "An active Org timestamp for DATE (\"YYYY-MM-DD\") and TIME (\"HH:MM\" or nil)."
  (if (and (stringp time) (string-match-p "\\`[0-9]\\{1,2\\}:[0-9]\\{2\\}\\'" time))
      (format "<%s %s>" date time)
    (format "<%s>" date)))

(defun harp-write--sanitize-tag (tag)
  "Map TAG onto org's tag charset ([[:alnum:]_@#%]); other chars -> `_'.
Hyphens are the everyday casualty (`blood-pressure'): an unsanitised tag
never parses back as a tag and would be silently dropped and pollute the
heading title on the next edit."
  (replace-regexp-in-string "[^[:alnum:]_@#%]" "_" tag))

(defun harp-write--tag-suffix (tags)
  "The trailing \"   :a:b:\" tag string for TAGS, or empty."
  (if tags
      (concat "    :" (mapconcat #'harp-write--sanitize-tag tags ":") ":")
    ""))

(defun harp-write--escape-body (text)
  "Neutralise lines in TEXT that org would parse as headings.
A `*' at column 0 followed by whitespace starts a heading; prefixing the
line with one space keeps it as body text (org does not treat an indented
`*' as a heading).  Applied to phone-typed entry bodies, document
annotations, and medication notes, where an innocent `* bullet' would
otherwise become a real heading and re-parent every sibling below it."
  (if (stringp text)
      (replace-regexp-in-string "^\\(\\*+[ \t]\\)" " \\1" text)
    text))

;; ─── text builders (mirror harp-kmp's formatString) ─────────────────────────

(defun harp-write--entry-text (id datetime tags private body)
  "Org text for a `*** Entry' subtree."
  (concat
   (format "*** Entry%s\n" (harp-write--tag-suffix tags))
   ":PROPERTIES:\n"
   (format ":ID: %s\n" id)
   (format ":DATETIME: %s\n" datetime)
   (format ":PRIVATE: %s\n" (if private "t" "nil"))
   ":END:\n"
   (if (and body (not (string-empty-p (string-trim body))))
       (concat "\n" (harp-write--escape-body (string-trim body)) "\n")
     "")))

(defun harp-write--document-text (id name datetime source files tags private annotation)
  "Org text for a `** Document' subtree."
  (concat
   (format "** %s%s\n" name (harp-write--tag-suffix tags))
   ":PROPERTIES:\n"
   (format ":ID: %s\n" id)
   (when (and source (not (string-empty-p source))) (format ":SOURCE: %s\n" source))
   (format ":FILES: %s\n" (mapconcat #'identity files ", "))
   (format ":DATETIME: %s\n" datetime)
   (format ":PRIVATE: %s\n" (if private "t" "nil"))
   ":END:\n"
   (if (and annotation (not (string-empty-p (string-trim annotation))))
       (concat "\n" (harp-write--escape-body (string-trim annotation)) "\n")
     "")))

(defun harp-write--metric-text (name tag-id unit range healthy tags)
  "Org text for a `*** Metric' definition subtree."
  (concat
   (format "*** %s%s\n" name (harp-write--tag-suffix tags))
   ":PROPERTIES:\n"
   (format ":TAG_ID: %s\n" tag-id)
   (format ":UNIT: %s\n" unit)
   (format ":RANGE: %s\n" range)
   (format ":HEALTHY_RANGE: %s\n" healthy)
   ":END:\n"))

;; ─── insertion / replacement primitives ─────────────────────────────────────

(defun harp-write--goto-section (section)
  "Move point to the `* SECTION' heading; error if absent.  Point stays there."
  (goto-char (point-min))
  (unless (re-search-forward (format "^\\* %s[ \t]*$" (regexp-quote section)) nil t)
    (error "No %s section in profile" section))
  (org-back-to-heading t))

(defun harp-write--insert-at-subtree-end ()
  "Insert position = end of the subtree at point; leaves a leading newline."
  (let ((end (save-excursion (org-end-of-subtree t t))))
    (goto-char end)
    (unless (bolp) (insert "\n"))))

(defun harp-write--append-under (section text &optional subheading)
  "Append TEXT at the end of `* SECTION' (or its `** SUBHEADING' child).
The child is matched by its cleaned heading title, so a journal heading
carrying tags or a TODO keyword — `** Rehab :physio:' — is still found;
the old bare-line regexp missed those and every save errored."
  (harp-write--goto-section section)
  (when subheading
    (let ((bound (save-excursion (org-end-of-subtree t t)))
          (found nil))
      (save-excursion
        (while (and (not found) (re-search-forward "^\\*\\* " bound t))
          (goto-char (match-beginning 0))
          (when (equal (nth 4 (org-heading-components)) subheading)
            (setq found (point)))
          (end-of-line)))
      (unless found (error "Not found under %s: %s" section subheading))
      (goto-char found)))
  (harp-write--insert-at-subtree-end)
  (insert text))

(defun harp-write--replace-subtree (ref text)
  "Replace the subtree resolved from REF with TEXT.  Return the buffer."
  (let ((marker (harp--resolve-ref ref)))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (org-back-to-heading t)
       (let ((beg (point))
             (end (save-excursion (org-end-of-subtree t t))))
         (delete-region beg end)
         (goto-char beg)
         (insert text))))
    (marker-buffer marker)))

(defun harp-write--update-in-place (ref title tags props body)
  "Update the heading at REF in place, preserving everything unmodeled.
TITLE (nil = keep the current headline) sets the heading text; TAGS is the
modeled tag list, sanitised and merged with org-attach's ATTACH when it is
present; PROPS is an alist of (KEY . VALUE) properties to set (a nil VALUE
deletes the key); BODY (nil = leave it) replaces the subtree body BELOW all
drawers.  Because it edits in place rather than rebuilding from fields,
planning lines (SCHEDULED/DEADLINE), non-modeled properties (`:CREATED:',
`:ATTACH_DIR:', …), and hand-added leading drawers all survive.  Returns
the buffer."
  (let ((marker (harp--resolve-ref ref)))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (org-back-to-heading t)
       (when title (org-edit-headline title))
       (org-back-to-heading t)
       ;; Tags: the modeled set, keeping org-attach's ATTACH so attachments
       ;; are not orphaned; drop everything else the form owns.
       (let ((attach (and (member "ATTACH" (org-get-tags nil t)) '("ATTACH"))))
         (org-set-tags (append (mapcar #'harp-write--sanitize-tag tags) attach)))
       (org-back-to-heading t)
       (dolist (p props)
         (if (cdr p)
             (org-entry-put (point) (car p) (cdr p))
           (org-entry-delete (point) (car p))))
       (when body
         (org-back-to-heading t)
         (let ((limit (save-excursion
                        (or (and (outline-next-heading) (point))
                            (org-end-of-subtree t t)))))
           (org-back-to-heading t)
           (org-end-of-meta-data t)     ; past planning + property + log drawers
           (when (< (point) limit) (delete-region (point) limit))
           (unless (bolp) (insert "\n"))
           (let ((b (string-trim body)))
             (unless (string-empty-p b)
               (insert "\n" (harp-write--escape-body b) "\n")))))))
    (marker-buffer marker)))

;; ─── medications ────────────────────────────────────────────────────────────

(defun harp-write--prop-line (key val)
  "A `:KEY: VAL' property line, or empty when VAL is blank."
  (if (and (stringp val) (not (string-empty-p (string-trim val))))
      (format ":%s: %s\n" key (string-trim val))
    ""))

(defun harp-write--maybe-stamp (date)
  "Wrap a bare \"YYYY-MM-DD\" DATE in <> for an Org timestamp; pass through <>."
  (cond ((or (null date) (string-empty-p (string-trim date))) nil)
        ((string-prefix-p "<" (string-trim date)) (string-trim date))
        (t (format "<%s>" (string-trim date)))))

(defun harp-write--schedule-text (fields)
  "A `*** Schedule' subtree for the schedule keys in FIELDS, or empty."
  (let ((props (concat
                (harp-write--prop-line "AMOUNT" (alist-get 'amount fields))
                (harp-write--prop-line "TIMES" (alist-get 'times fields))
                (harp-write--prop-line "DAYS" (alist-get 'days fields))
                (harp-write--prop-line "INTERVAL" (alist-get 'interval fields))
                (harp-write--prop-line "WINDOW" (alist-get 'window fields))
                (harp-write--prop-line "CYCLE_START" (harp-write--maybe-stamp
                                                      (alist-get 'cycle-start fields)))
                (harp-write--prop-line "ACTIVE_DAYS" (alist-get 'active-days fields))
                (harp-write--prop-line "PAUSE_DAYS" (alist-get 'pause-days fields))
                (harp-write--prop-line "FROM" (harp-write--maybe-stamp (alist-get 'from fields)))
                (harp-write--prop-line "UNTIL" (harp-write--maybe-stamp (alist-get 'until fields))))))
    (if (string-empty-p props)
        ""
      (concat "*** Schedule\n:PROPERTIES:\n" props ":END:\n"))))

(defun harp-write--medication-text (id fields &optional logbook)
  "Org text for a `** Medication' subtree from FIELDS.
LOGBOOK, when non-nil, is a raw drawer block re-inserted to preserve history."
  (let ((notes (alist-get 'notes fields)))
    (concat
     (format "** %s%s\n" (alist-get 'name fields)
             (harp-write--tag-suffix (alist-get 'tags fields)))
     ":PROPERTIES:\n"
     (format ":ID: %s\n" id)
     ":HARP_MED: t\n"
     (harp-write--prop-line "UNIT" (alist-get 'unit fields))
     (harp-write--prop-line "STOCK" (alist-get 'stock fields))
     (harp-write--prop-line "LOW_STOCK" (alist-get 'low-stock fields))
     (harp-write--prop-line "REFILL" (alist-get 'refill fields))
     (harp-write--prop-line "EXPIRES" (harp-write--maybe-stamp (alist-get 'expires fields)))
     (format ":ACTIVE: %s\n" (if (alist-get 'active fields) "t" "no"))
     ":END:\n"
     (if (and logbook (not (string-empty-p (string-trim logbook))))
         (concat (string-trim logbook) "\n") "")
     (if (and notes (not (string-empty-p (string-trim notes))))
         ;; Escape body-injected headings BUT keep them clear of the schedule
         ;; children: an un-escaped `* ' line in notes would detach the
         ;; `*** Schedule' subtrees and silently stop dose tracking.
         (concat (harp-write--escape-body (string-trim notes)) "\n") "")
     (harp-write--schedule-text fields))))

(defun harp-write--extract-logbook (marker)
  "Raw `:LOGBOOK:…:END:' block of the med at MARKER, or nil.
Only the med heading's own content (before its schedule children) is scanned."
  (with-current-buffer (marker-buffer marker)
    (org-with-wide-buffer
     (goto-char marker)
     (org-back-to-heading t)
     (let ((limit (save-excursion
                    (or (and (outline-next-heading) (point))
                        (org-end-of-subtree t t)))))
       (org-back-to-heading t)
       (when (re-search-forward "^[ \t]*:LOGBOOK:[ \t]*$" limit t)
         (let ((beg (line-beginning-position)))
           (when (re-search-forward "^[ \t]*:END:[ \t]*$" limit t)
             (buffer-substring-no-properties beg (line-end-position)))))))))

(defun harp-write--extract-extra-schedules (marker)
  "Raw text of the 2nd..nth `*** Schedule' subtrees of the med at MARKER.
The med form edits only the FIRST schedule; an update re-emits the rest
verbatim — a taper is several adjacent Schedule subtrees, and rebuilding
from the form's single schedule would silently destroy them."
  (with-current-buffer (marker-buffer marker)
    (org-with-wide-buffer
     (goto-char marker)
     (org-back-to-heading t)
     (let ((end (save-excursion (org-end-of-subtree t t)))
           (n 0) (chunks '()))
       (while (re-search-forward "^\\*\\*\\* Schedule" end t)
         (setq n (1+ n))
         (when (> n 1)
           (let* ((beg (line-beginning-position))
                  (send (save-excursion (org-end-of-subtree t t)))
                  (text (buffer-substring-no-properties beg send)))
             (push (if (string-suffix-p "\n" text) text (concat text "\n"))
                   chunks)
             (goto-char send))))
       (nreverse chunks)))))

(defun harp-write-add-medication (fields)
  "Append a new medication built from FIELDS under `* Medications'.
Returns the new id."
  (let ((buf (harp-write--buffer)) (id (org-id-new)))
    (with-current-buffer buf
      (org-with-wide-buffer
       (harp-write--ensure-section "Medications")
       (harp-write--append-under "Medications" (harp-write--medication-text id fields))))
    (harp-write--save buf)
    id))

(defun harp-write-update-medication (ref fields)
  "Replace the medication at REF with FIELDS.
Preserves its dose LOGBOOK and any `*** Schedule' subtrees beyond the
first (the form edits only the first — see
`harp-write--extract-extra-schedules')."
  (let* ((id (or (alist-get 'id ref) (org-id-new)))
         (marker (harp--resolve-ref ref))
         (logbook (harp-write--extract-logbook marker))
         (extra (harp-write--extract-extra-schedules marker))
         (buf (harp-write--replace-subtree
               ref (concat (harp-write--medication-text id fields logbook)
                           (mapconcat #'identity extra "")))))
    (harp-write--save buf)
    id))

(defun harp-write-delete-medication (ref)
  "Delete the medication subtree at REF."
  (let ((marker (harp--resolve-ref ref)))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (org-back-to-heading t)
       (delete-region (point) (save-excursion (org-end-of-subtree t t)))))
    (harp-write--save (marker-buffer marker))))

;; ─── entries ────────────────────────────────────────────────────────────────

(defun harp-write-add-entry (journal date time tags private body)
  "Append a new entry under JOURNAL.  Return the new id."
  (let ((buf (harp-write--buffer)) (id (org-id-new)))
    (with-current-buffer buf
      (org-with-wide-buffer
       (harp-write--append-under
        "Journals"
        (harp-write--entry-text id (harp-write--stamp date time) tags private body)
        journal)))
    (harp-write--save buf)
    id))

(defun harp-write-update-entry (ref date time tags private body)
  "Update the entry at REF in place, preserving unmodeled org content."
  (let ((buf (harp-write--update-in-place
              ref nil tags
              `(("DATETIME" . ,(harp-write--stamp date time))
                ("PRIVATE" . ,(if private "t" "nil")))
              (or body ""))))
    (harp-write--save buf)
    (alist-get 'id ref)))

;; ─── documents ──────────────────────────────────────────────────────────────

(defun harp-write-add-document (name date time source files tags private annotation)
  "Append a new document.  Return the new id."
  (let ((buf (harp-write--buffer)) (id (org-id-new)))
    (with-current-buffer buf
      (org-with-wide-buffer
       (harp-write--append-under
        "Documents"
        (harp-write--document-text id name (harp-write--stamp date time)
                                   source files tags private annotation))))
    (harp-write--save buf)
    id))

(defun harp-write-update-document (ref name date time source files tags private annotation)
  "Update the document at REF in place, preserving unmodeled org content."
  (let ((buf (harp-write--update-in-place
              ref name tags
              `(("SOURCE" . ,(and source (not (string-empty-p (string-trim source)))
                                  (string-trim source)))
                ("FILES" . ,(mapconcat #'identity files ", "))
                ("DATETIME" . ,(harp-write--stamp date time))
                ("PRIVATE" . ,(if private "t" "nil")))
              (or annotation ""))))
    (harp-write--save buf)
    (alist-get 'id ref)))

;; ─── metrics ────────────────────────────────────────────────────────────────

(defun harp-write-add-metric (name tag-id unit range healthy tags)
  "Append a new metric definition under Metadata/Metrics."
  (let ((buf (harp-write--buffer)))
    (with-current-buffer buf
      (org-with-wide-buffer
       (harp-write--goto-section "Metadata")
       (let ((bound (save-excursion (org-end-of-subtree t t))))
         (unless (re-search-forward "^\\*\\* Metrics[ \t]*$" bound t)
           (error "No Metrics subsection under Metadata"))
         (org-back-to-heading t)
         (harp-write--insert-at-subtree-end)
         (insert (harp-write--metric-text name tag-id unit range healthy tags)))))
    (harp-write--save buf)
    tag-id))

(defun harp-write-update-metric (ref name tag-id unit range healthy tags)
  "Update the metric at REF in place, preserving its description body."
  (let ((buf (harp-write--update-in-place
              ref name tags
              `(("TAG_ID" . ,tag-id)
                ("UNIT" . ,unit)
                ("RANGE" . ,range)
                ("HEALTHY_RANGE" . ,healthy))
              nil)))                    ; body untouched — metrics don't model it
    (harp-write--save buf)
    tag-id))

;; ─── privacy toggle ─────────────────────────────────────────────────────────

(defun harp-write-toggle-private (ref)
  "Flip the :PRIVATE: property of the item at REF.  Return the new value."
  (let ((marker (harp--resolve-ref ref)) new)
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (org-back-to-heading t)
       (setq new (not (equal (org-entry-get (point) "PRIVATE") "t")))
       (org-entry-put (point) "PRIVATE" (if new "t" "nil"))))
    (harp-write--save (marker-buffer marker))
    new))

;; ─── new profile ────────────────────────────────────────────────────────────

(defun harp-write--profile-template (name id type)
  "The Org text for a fresh profile (mirrors harp-kmp `newProfileText')."
  (concat
   ":PROPERTIES:\n"
   (format ":ID: %s\n" id)
   (format ":HARP_TYPE: %s\n" type)
   ":END:\n"
   (format "#+TITLE: %s\n\n" name)
   "* Metadata\n** Sources\n\n** Metrics\n\n"
   "* Journals\n** Main\n\n"
   "* Documents\n\n"
   "* Medications\n"))

(defun harp-write--ensure-section (section)
  "Ensure a top-level `* SECTION' exists in the current widened buffer.
Creates it at end of file when missing.  Point is left unspecified."
  (goto-char (point-min))
  (unless (re-search-forward (format "^\\* %s[ \t]*$" (regexp-quote section)) nil t)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert (format "* %s\n" section))))

(defun harp-write--profile-slug (name)
  "A filesystem-safe base name for profile NAME."
  (let ((slug (downcase (replace-regexp-in-string
                         "[^a-z0-9]+" "-" (downcase (string-trim name))))))
    (setq slug (string-trim slug "-+" "-+"))
    (if (string-empty-p slug) "profile" slug)))

(defun harp-write-new-profile (name type)
  "Create a new profile file under `harp-directory'.  Return its path."
  (make-directory harp-directory t)
  (let* ((base (harp-write--profile-slug name))
         (file (expand-file-name (concat base ".org") harp-directory))
         (n 2))
    (while (file-exists-p file)
      (setq file (expand-file-name (format "%s-%d.org" base n) harp-directory))
      (cl-incf n))
    (with-temp-file file
      (insert (harp-write--profile-template name (org-id-new)
                                            (if (member type harp-profile-types)
                                                type "human"))))
    file))

(provide 'harp-write)
;;; harp-write.el ends here

;;; ==================================================================
;;; BEGIN apps/harp/harp-med.el
;;; ==================================================================

;;; harp-med.el --- Harp medications domain layer -*- lexical-binding: t; -*-

;; Accessors and predicates over the `medications' of a parsed Harp profile
;; (the `* Medications' section, read in `harp-profile.el').  A medication is
;; an alist: id, name, unit, stock, refill, low-stock, expires, active, notes,
;; tags, schedules, doses, ref.  A schedule is amount/times/days/cyclic/
;; interval/period; a dose is state (taken|skipped)/stamp/time/amount.
;;
;; MedTimer inspiration: Medicine.isOutOfStock / hasExpired / isStockManagement.
;; The scheduling engine lives in harp-med-schedule.el; logging in harp-med-log.el.

;;; Code:

(require 'cl-lib)
;; (require 'harp-profile) — bundle-internal, provided above
;; (require 'harp-ui) — bundle-internal, provided above

(defun harp-med-medications (&optional profile)
  "Medications of PROFILE (default: the current profile)."
  (alist-get 'medications (or profile (harp-ui-current))))

(defun harp-med-by-id (id &optional profile)
  "The medication with ID in PROFILE, or nil."
  (cl-find id (harp-med-medications profile)
           :key (lambda (m) (alist-get 'id m)) :test #'equal))

;; ─── stock & expiration (MedTimer parity) ────────────────────────────────────

(defun harp-med-format-amount (n)
  "Format numeric N for display, keeping fractions: 2.0 → \"2\", 2.5 → \"2.5\".
Half a tablet of stock must not read as a whole one — round is wrong here."
  (cond ((not (numberp n)) (format "%s" n))
        ((= n (truncate n)) (number-to-string (truncate n)))
        (t (number-to-string n))))

(defun harp-med-active-p (med)
  "Non-nil when MED is active."
  (alist-get 'active med))

(defun harp-med-manages-stock-p (med)
  "Non-nil when MED tracks stock (a numeric stock or low-stock threshold)."
  (or (numberp (alist-get 'stock med)) (numberp (alist-get 'low-stock med))))

(defun harp-med-out-of-stock-p (med)
  "Non-nil when MED's stock has reached zero (or below)."
  (let ((stock (alist-get 'stock med)))
    (and (numberp stock) (<= stock 0))))

(defun harp-med-low-stock-p (med)
  "Non-nil when MED's stock is at or below its low-stock threshold."
  (let ((stock (alist-get 'stock med)) (low (alist-get 'low-stock med)))
    (and (numberp stock) (numberp low) (<= stock low))))

(defun harp-med-expired-p (med)
  "Non-nil when MED's expiration date is in the past."
  (let ((time (harp--time-of (alist-get 'expires med))))
    (and time (< time (float-time)))))

(defun harp-med-warnings (med)
  "A list of human-readable warning strings for MED (may be empty)."
  (delq nil
        (list (when (harp-med-expired-p med) "Expired")
              (cond ((harp-med-out-of-stock-p med) "Out of stock")
                    ((harp-med-low-stock-p med) "Low stock")))))

(defun harp-med-dose-dedupe-key (state med time)
  "The offline-queue dedupe key for a STATE tap on MED's TIME dose today.
One key per med/dose/day: a double-tap queued offline must replay once —
a second replay would double the LOGBOOK line and the stock decrement.
The date is the RENDER day (baked into the action).  Residual limitation:
a surface cached unrendered for >24h and tapped on two different real days
shares one key, so the companion keeps only the last — acceptable because
any app interaction re-renders the surface with the new day's key, and the
alternative (a date-less key) would collapse every day's dose into one."
  (format "harp.med.%s/%s/%s/%s" state (alist-get 'id med)
          (format-time-string "%Y-%m-%d") (or time "-")))

;; ─── dose events ─────────────────────────────────────────────────────────────

(defun harp-med-doses (med)
  "All logged dose events for MED."
  (alist-get 'doses med))

(defun harp-med-doses-on (med date-string)
  "MED's dose events whose stamp falls on DATE-STRING (\"YYYY-MM-DD\")."
  (cl-remove-if-not
   (lambda (d) (let ((s (alist-get 'stamp d)))
                 (and (stringp s) (string-prefix-p date-string s))))
   (harp-med-doses med)))

(defun harp-med-last-dose (med)
  "The most recent dose event for MED, or nil."
  (car (sort (copy-sequence (harp-med-doses med))
             (lambda (a b) (> (or (alist-get 'time a) 0)
                              (or (alist-get 'time b) 0))))))

(provide 'harp-med)
;;; harp-med.el ends here

;;; ==================================================================
;;; BEGIN apps/harp/harp-med-schedule.el
;;; ==================================================================

;;; harp-med-schedule.el --- Harp medication scheduling engine -*- lexical-binding: t; -*-

;; Computes which doses a medication is due on a given day, the next future
;; dose (for arming alarms), and which of today's doses are still pending.
;; Composes MedTimer's schedule modifiers on top of a schedule's :TIMES: (or
;; :INTERVAL:): weekday filter (:DAYS:), cyclic active/pause cycles
;; (:CYCLE_START:/:ACTIVE_DAYS:/:PAUSE_DAYS:), and a date period
;; (:FROM:/:UNTIL:, for tapering). Pure elisp; date math via `time-to-days'
;; and `decode-time'.

;;; Code:

(require 'cl-lib)
;; (require 'harp-profile)                 ; harp--time-of — bundle-internal, provided above
;; (require 'harp-med)                     ; accessors, harp-med-doses-on — bundle-internal, provided above
;; (require 'harp-ui)                      ; harp-ui-memo (push-scoped memo) — bundle-internal, provided above

(defconst harp-med--weekday-abbrevs ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"]
  "English weekday abbreviations indexed by `decoded-time-weekday' (0=Sun).")

(defun harp-med--duration-seconds (s)
  "Parse a duration string like \"6h\" / \"90m\" / \"3d\" / \"2w\" into seconds."
  (when (stringp s)
    ;; Match and extract against the SAME (trimmed) string — matching the
    ;; trimmed one but extracting groups from the original returns shifted
    ;; substrings on any padded input.
    (let ((s (string-trim s)))
      (when (string-match "\\`\\([0-9]+\\)\\([mhdw]\\)\\'" s)
        (* (string-to-number (match-string 1 s))
           (pcase (aref (match-string 2 s) 0)
             (?m 60) (?h 3600) (?d 86400) (?w 604800)))))))

(defun harp-med--hm-to-min (hm)
  "Minutes-since-midnight for an \"HH:MM\" string HM."
  (let ((p (mapcar #'string-to-number (split-string hm ":"))))
    (+ (* 60 (nth 0 p)) (nth 1 p))))

(defun harp-med-schedule--weekday (date)
  "English weekday abbrev (\"Mon\") for Lisp time DATE."
  (aref harp-med--weekday-abbrevs (decoded-time-weekday (decode-time date))))

(defun harp-med-schedule--datetime (date hm)
  "Float-time of DATE (a Lisp time) at clock time HM (\"HH:MM\").
DST-safe: re-resolves the local offset for DATE's day, so a dose time on
the far side of a DST transition is not computed an hour off."
  (let ((d (decode-time date))
        (p (mapcar #'string-to-number (split-string hm ":"))))
    (setf (decoded-time-hour d) (nth 0 p)
          (decoded-time-minute d) (nth 1 p)
          (decoded-time-second d) 0)
    (float-time (harp--encode-local d))))

;; ─── activity of a schedule on a date ────────────────────────────────────────

(defun harp-med-schedule--in-period-p (schedule date)
  "Non-nil when DATE is within SCHEDULE's :FROM:/:UNTIL: period (inclusive)."
  (let ((from (harp--time-of (alist-get 'from schedule)))
        (until (harp--time-of (alist-get 'until schedule)))
        (d (time-to-days date)))
    (and (or (null from) (>= d (time-to-days (seconds-to-time from))))
         (or (null until) (<= d (time-to-days (seconds-to-time until)))))))

(defun harp-med-schedule--weekday-ok-p (schedule date)
  "Non-nil when DATE's weekday passes SCHEDULE's :DAYS: filter."
  (let ((days (alist-get 'days schedule)))
    (or (null days) (member (harp-med-schedule--weekday date) days))))

(defun harp-med-schedule--cycle-ok-p (schedule date)
  "Non-nil when DATE lands on an active day of SCHEDULE's cyclic pattern.
`pos = (date − CYCLE_START) mod (ACTIVE + PAUSE)`; active when `pos < ACTIVE'."
  (let ((start (harp--time-of (alist-get 'cycle-start schedule)))
        (active (alist-get 'active-days schedule))
        (pause (alist-get 'pause-days schedule)))
    (if (not (and start (numberp active) (numberp pause) (> (+ active pause) 0)))
        t
      (< (mod (- (time-to-days date) (time-to-days (seconds-to-time start)))
              (+ active pause))
         active))))

(defun harp-med-schedule-active-on-p (schedule med date)
  "Non-nil when SCHEDULE of MED yields doses on DATE (a Lisp time)."
  (and (harp-med-active-p med)
       (harp-med-schedule--in-period-p schedule date)
       (harp-med-schedule--weekday-ok-p schedule date)
       (harp-med-schedule--cycle-ok-p schedule date)))

;; ─── the times a schedule fires on a date ────────────────────────────────────

(defun harp-med-schedule--window-bounds (window)
  "Parse WINDOW \"HH:MM-HH:MM\" to (START-MIN . END-MIN); default 06:00–23:00."
  (let ((w (and (stringp window) (string-trim window))))
    (if (and w (string-match
                "\\`\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)-\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)\\'" w))
        ;; Bind both groups BEFORE `harp-med--hm-to-min' (which calls
        ;; `split-string' and would clobber the match data).
        (let ((start (match-string 1 w)) (end (match-string 2 w)))
          (cons (harp-med--hm-to-min start) (harp-med--hm-to-min end)))
      (cons 360 1380))))

(defun harp-med-schedule--interval-multiday (schedule date secs bounds)
  "\"HH:MM\" times a ≥1-day interval SCHEDULE (period SECS) fires on DATE.
Continuous from an anchor at the window-start of the schedule's anchor day
\(:CYCLE_START:, else :FROM:, else DATE itself), so a 36h interval steps
08:00 → 20:00 → 08:00 across days — a whole-day interval (24h/48h/72h)
reduces to a fixed daily time every N days.  Day membership uses
`time-to-days' (DST-aware); the exotic multi-day case may drift an hour
across a transition, which is immaterial for a dose reminder."
  (let* ((anchor-src (or (harp--time-of (alist-get 'cycle-start schedule))
                         (harp--time-of (alist-get 'from schedule))
                         (float-time date)))
         (start-min (car bounds))
         (anchor (harp-med-schedule--datetime
                  (seconds-to-time anchor-src)
                  (format "%02d:%02d" (/ start-min 60) (% start-min 60))))
         (target-day (time-to-days date))
         (day-start (harp-med-schedule--datetime date "00:00"))
         (tt (+ anchor (* (max 0 (ceiling (- day-start anchor) secs)) secs)))
         (guard 0) out)
    (while (and (< (time-to-days (seconds-to-time tt)) target-day) (< guard 800))
      (setq tt (+ tt secs) guard (1+ guard)))
    (while (and (= (time-to-days (seconds-to-time tt)) target-day) (< guard 800))
      (let ((d (decode-time (seconds-to-time tt))))
        (push (format "%02d:%02d" (decoded-time-hour d) (decoded-time-minute d)) out))
      (setq tt (+ tt secs) guard (1+ guard)))
    (nreverse out)))

(defun harp-med-schedule--interval-times (schedule &optional date)
  "The \"HH:MM\" dose times for an interval SCHEDULE on DATE.
Sub-daily intervals tile the daily :WINDOW:; intervals of a day or more are
a continuous sequence from an anchor (`harp-med-schedule--interval-multiday')."
  (let ((secs (harp-med--duration-seconds (alist-get 'interval schedule))))
    (when (and secs (> secs 0))
      (let ((bounds (harp-med-schedule--window-bounds (alist-get 'window schedule))))
        (if (>= secs 86400)
            (and date (harp-med-schedule--interval-multiday schedule date secs bounds))
          (let ((step (/ secs 60)) (m (car bounds)) (end (cdr bounds)) out)
            (while (<= m end)
              (push (format "%02d:%02d" (/ m 60) (% m 60)) out)
              (setq m (+ m step)))
            (nreverse out)))))))

(defun harp-med-schedule--times-on (schedule &optional date)
  "The \"HH:MM\" times SCHEDULE fires on DATE (`:TIMES:' or interval)."
  (or (alist-get 'times schedule)
      (and (alist-get 'interval schedule)
           (harp-med-schedule--interval-times schedule date))))

;; ─── public: doses on a date, pending, next ──────────────────────────────────

(defun harp-med-schedule-doses-on (med date)
  "Doses MED is due on DATE (a Lisp time), sorted by time.
Each dose is an alist: `time' (\"HH:MM\"), `at' (float-time), `amount',
`unit', `schedule'."
  (let (out)
    (dolist (s (alist-get 'schedules med))
      (when (harp-med-schedule-active-on-p s med date)
        (dolist (hm (harp-med-schedule--times-on s date))
          (push `((time . ,hm)
                  (at . ,(harp-med-schedule--datetime date hm))
                  (amount . ,(alist-get 'amount s))
                  (unit . ,(alist-get 'unit med))
                  (schedule . ,s))
                out))))
    (sort out (lambda (a b) (string< (alist-get 'time a) (alist-get 'time b))))))

(defun harp-med-schedule-pending (med &optional date)
  "MED's still-pending doses on DATE (default today).
Count-based: the first K scheduled doses (K = doses logged that day) are
treated as done, the rest are pending."
  (let* ((date (or date (current-time)))
         (dstr (format-time-string "%Y-%m-%d" date))
         (scheduled (harp-med-schedule-doses-on med date))
         (logged (length (harp-med-doses-on med dstr))))
    (nthcdr (min logged (length scheduled)) scheduled)))

(defun harp-med-schedule-next-dose (med &optional after horizon-days)
  "The next dose (full alist, carrying `at') for MED strictly after AFTER.
Searches up to HORIZON-DAYS ahead (default 400); nil if none.  Steps by
CALENDAR days (not 86400 s), so no day is skipped across a DST transition."
  (let* ((base (or after (current-time)))
         (now (float-time base))
         (base-day (decode-time base)))
    (catch 'found
      (dotimes (i (or horizon-days 400))
        (let ((day (let ((d (copy-sequence base-day)))
                     (setf (decoded-time-day d) (+ (decoded-time-day base-day) i))
                     (harp--encode-local d))))
          (dolist (d (harp-med-schedule-doses-on med day))
            (when (> (alist-get 'at d) now)
              (throw 'found d)))))
      nil)))

(defun harp-med-schedule-next (med &optional after horizon-days)
  "The next dose datetime (float) for MED strictly after AFTER, or nil."
  (alist-get 'at (harp-med-schedule-next-dose med after horizon-days)))

(defun harp-med-schedule-due-today (&optional profile)
  "All pending doses today across PROFILE's medications, each with its `med'.
Sorted by time — the shape the Due-today list wants."
  (let (out)
    (dolist (m (harp-med-medications profile))
      (dolist (d (harp-med-schedule-pending m))
        (push (cons (cons 'med m) d) out)))
    (sort out (lambda (a b) (string< (alist-get 'time a) (alist-get 'time b))))))

(defun harp-med-due-today ()
  "Pending doses today across the current profile, memoised for the push.
Three surfaces ask for this each push — the due list, its badge count, and
the notification sync — so compute it once.  Keyed on the calendar day so
it recomputes across midnight; busts with the profile read on any dose log."
  (harp-ui-memo (cons 'due-today (format-time-string "%Y-%m-%d"))
                (lambda () (harp-med-schedule-due-today))))

(provide 'harp-med-schedule)
;;; harp-med-schedule.el ends here

;;; ==================================================================
;;; BEGIN apps/harp/harp-med-log.el
;;; ==================================================================

;;; harp-med-log.el --- Harp dose logging -*- lexical-binding: t; -*-

;; Recording a dose Taken or Skipped: prepend a `- STATE [ts] AMOUNT' line to
;; the medication's :LOGBOOK: drawer and, on Taken, decrement its :STOCK: by
;; the dose amount (MedTimer's ReminderEvent + stockBefore/stockAfter, org-native).
;; The allowlisted actions `harp.med.taken' / `harp.med.skipped' resolve a
;; medication by id in the current profile and write through here.

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'subr-x)
(require 'jetpacs-org)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
;; (require 'harp-profile) — bundle-internal, provided above
;; (require 'harp-write)                   ; harp-write--save — bundle-internal, provided above
;; (require 'harp-ui) — bundle-internal, provided above
;; (require 'harp-med) — bundle-internal, provided above
;; (require 'harp-metrics-parse)           ; harp-parse--num-or-nil — bundle-internal, provided above

(defun harp-med-log--fmt-num (n)
  "Format number N without a trailing \".0\" for integers."
  (if (and (numberp n) (= n (truncate n)))
      (number-to-string (truncate n))
    (number-to-string n)))

(defun harp-med-log--amount-label (amount unit)
  "A dose amount label from AMOUNT and UNIT (either may be nil/blank)."
  (let ((a (and (stringp amount) (not (string-empty-p (string-trim amount)))
                (string-trim amount)))
        (u (and (stringp unit) (not (string-empty-p (string-trim unit)))
                (string-trim unit))))
    (cond ((and a u) (concat a " " u))
          (a a)
          (t ""))))

(defun harp-med-log--insert (state amount unit &optional at)
  "At the medication heading at point, prepend a dose log line.
AT (a Lisp time, default now) stamps the line — a dose taken offline is
logged at its `queued_at' time, not at reconnect.  Creates the :LOGBOOK:
drawer after the property drawer when absent."
  (org-back-to-heading t)
  (let* ((ts (format-time-string "[%Y-%m-%d %H:%M]" (or at (current-time))))
         (label (harp-med-log--amount-label amount unit))
         (line (concat "- " state " " ts
                       (if (string-empty-p label) "" (concat " " label))))
         (limit (save-excursion
                  (or (and (outline-next-heading) (point))
                      (org-end-of-subtree t t)))))
    (org-back-to-heading t)
    (if (re-search-forward "^[ \t]*:LOGBOOK:[ \t]*$" limit t)
        (progn (forward-line 1) (beginning-of-line) (insert line "\n"))
      (org-back-to-heading t)
      (org-end-of-meta-data)            ; past planning + property drawer
      ;; When the med is the last heading in a file with no trailing newline,
      ;; point lands mid-line at eob — without this the drawer fuses onto the
      ;; property drawer's :END: (":END::LOGBOOK:") and the med becomes
      ;; unparseable, vanishing from the app entirely.
      (unless (bolp) (insert "\n"))
      (insert ":LOGBOOK:\n" line "\n:END:\n"))))

(defun harp-med-log-write (ref state amount unit decrement &optional at)
  "Log a STATE dose for the med at REF; decrement :STOCK: by DECREMENT if set.
AMOUNT/UNIT annotate the log line; AT stamps it (default now).  Saves and
drops the read memo."
  (let ((marker (harp--resolve-ref ref)))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       ;; Decrement stock FIRST, while point is reliably on the heading —
       ;; inserting the LOGBOOK can shift the outline out from under a later
       ;; `org-back-to-heading'.  The marker (at the heading) survives both.
       (when (numberp decrement)
         (goto-char marker)
         (org-back-to-heading t)
         (let ((cur (harp-parse--num-or-nil (org-entry-get (point) "STOCK"))))
           (when (numberp cur)
             (org-entry-put (point) "STOCK"
                            (harp-med-log--fmt-num (max 0 (- cur decrement)))))))
       (goto-char marker)
       (org-back-to-heading t)
       (harp-med-log--insert state amount unit at)))
    (harp-write--save (marker-buffer marker))))

(defun harp-med-log--resolve (args)
  "Resolve the medication targeted by dose ARGS, across profiles.
Prefers the profile FILE baked into the action at render time (validated
against discovery) so a queued tap survives an Emacs restart or a different
profile being open; falls back to the current profile."
  (let ((id (alist-get 'id args))
        (file (alist-get 'file args)))
    (when (stringp id)
      (if (and (stringp file) (member file (harp-profile-files)))
          (harp-med-by-id id (harp-profile-read file))
        (harp-med-by-id id)))))

(defun harp-med-log--dose (args payload state)
  "Handle a taken/skipped ARGS dispatch for STATE.
PAYLOAD carries `queued_at' for events replayed from the offline queue."
  (let* ((med (harp-med-log--resolve args))
         (amount (alist-get 'amount args))
         (unit (or (alist-get 'unit args) (and med (alist-get 'unit med))))
         (queued (alist-get 'queued_at payload))
         (at (and (numberp queued) (seconds-to-time (/ queued 1000.0)))))
    (if med
        (progn
          (harp-med-log-write (alist-get 'ref med) state amount unit
                              ;; Only Taken consumes stock.
                              (and (equal state "taken")
                                   (harp-parse--num-or-nil amount))
                              at)
          (jetpacs-shell-notify
           (format "%s: %s" (alist-get 'name med) (capitalize state)))
          (jetpacs-shell-push))
      ;; Never drop a dose silently — the adherence record is the point.
      (display-warning
       'harp (format "Dose (%s) not recorded: medication %s not found%s"
                     state (alist-get 'id args)
                     (if (alist-get 'file args)
                         (format " in %s" (alist-get 'file args)) ""))
       :warning)
      (jetpacs-shell-notify "Dose not recorded — its medication wasn't found")
      (jetpacs-shell-push))))

(defun harp-med-log-refill (med)
  "Add MED's `:REFILL:' size to its `:STOCK:'.  Return the new stock, or nil.
The one-tap \"picked up a new pack\" affordance (MedTimer's refill)."
  (let ((refill (alist-get 'refill med)))
    (when (numberp refill)
      (let ((marker (harp--resolve-ref (alist-get 'ref med)))
            new)
        (with-current-buffer (marker-buffer marker)
          (org-with-wide-buffer
           (goto-char marker)
           (org-back-to-heading t)
           (let ((cur (or (harp-parse--num-or-nil (org-entry-get (point) "STOCK")) 0)))
             (setq new (+ cur refill))
             (org-entry-put (point) "STOCK" (harp-med-log--fmt-num new)))))
        (harp-write--save (marker-buffer marker))
        new))))

(with-jetpacs-owner "harp"
  (jetpacs-defaction "harp.med.refill"
    (lambda (args _)
      (let ((med (harp-med-log--resolve args)))
        (when med
          (when-let ((new (harp-med-log-refill med)))
            (jetpacs-shell-notify
             (format "%s: stock now %s %s" (alist-get 'name med)
                     (harp-med-log--fmt-num new) (or (alist-get 'unit med) "")))
            (jetpacs-shell-push)))))
    :doc "Add a medication's refill size to its stock."
    :args '((:name id :type "text" :required t)
            (:name file :type "text")))

  (jetpacs-defaction "harp.med.taken"
    (lambda (args payload) (harp-med-log--dose args payload "taken"))
    :doc "Log a medication dose as taken (decrements stock)."
    :args '((:name id :type "text" :required t)
            (:name amount :type "text")
            (:name unit :type "text")
            (:name file :type "text")))

  (jetpacs-defaction "harp.med.skipped"
    (lambda (args payload) (harp-med-log--dose args payload "skipped"))
    :doc "Log a medication dose as skipped."
    :args '((:name id :type "text" :required t)
            (:name amount :type "text")
            (:name unit :type "text")
            (:name file :type "text"))))

(provide 'harp-med-log)
;;; harp-med-log.el ends here

;;; ==================================================================
;;; BEGIN apps/harp/harp-med-view.el
;;; ==================================================================

;;; harp-med-view.el --- Harp medications tab and detail -*- lexical-binding: t; -*-

;; The Meds tab: today's still-pending doses with Taken/Skipped buttons over
;; the medication list (stock and expiry warnings surfaced per card), and a
;; per-medication detail overlay (schedules, stock, recent doses, pending
;; today).  Mirrors MedTimer's Overview + Medicine screens.  Read + log only;
;; add/edit lives in harp-med-form.el.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'jetpacs-widgets)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
;; (require 'harp-ui) — bundle-internal, provided above
;; (require 'harp-med) — bundle-internal, provided above
;; (require 'harp-med-schedule) — bundle-internal, provided above

(defvar harp-med-view--detail-id nil
  "Id of the medication shown in the detail overlay, or nil.")

(defun harp-med-view--dose-label (amount unit)
  "\"AMOUNT UNIT\" label (either may be blank)."
  (string-trim (concat (or amount "") " " (or unit ""))))

;; ─── Due-today doses (Taken / Skipped) ───────────────────────────────────────

(defun harp-med-view--dose-actions (med amount &optional time)
  "A Skip/Taken button row for MED's dose of AMOUNT at TIME."
  (let ((args `((id . ,(alist-get 'id med))
                (amount . ,amount)
                (unit . ,(alist-get 'unit med))
                ;; Bake the profile file so a queued tap resolves the med even
                ;; after a restart / with another profile open.
                (file . ,harp-ui--current-file))))
    (jetpacs-row
     (jetpacs-spacer :weight 1)
     (jetpacs-button "Skip"
                     (jetpacs-action "harp.med.skipped" :args args
                                     :when-offline "queue"
                                     :dedupe (harp-med-dose-dedupe-key "skipped" med time))
                     :variant "text")
     (jetpacs-button "Taken"
                     (jetpacs-action "harp.med.taken" :args args
                                     :when-offline "queue"
                                     :dedupe (harp-med-dose-dedupe-key "taken" med time))
                     :variant "tonal")
     :spacing 8)))

(defun harp-med-view--due-card (entry)
  "A due-today card for ENTRY (a pending dose with its `med')."
  (let* ((med (alist-get 'med entry))
         (label (harp-med-view--dose-label (alist-get 'amount entry)
                                           (alist-get 'unit entry))))
    (jetpacs-card
     (list (jetpacs-column
            (jetpacs-row
             (jetpacs-box
              (list (jetpacs-column
                     (jetpacs-text (alist-get 'name med) 'title)
                     (jetpacs-text (string-trim
                                    (concat (alist-get 'time entry)
                                            (unless (string-empty-p label)
                                              (concat " · " label))))
                                   'caption)))
              :weight 1)
             :align "center")
            (harp-med-view--dose-actions med (alist-get 'amount entry)
                                         (alist-get 'time entry)))))))

;; ─── Medication list ─────────────────────────────────────────────────────────

(defun harp-med-view--summary (med)
  "A one-line caption for MED: stock, schedule count, warnings."
  (let ((parts (delq nil
                     (list (when (numberp (alist-get 'stock med))
                             (harp-med-view--dose-label
                              (harp-med-format-amount (alist-get 'stock med))
                              (alist-get 'unit med)))
                           (unless (harp-med-active-p med) "inactive")))))
    (string-join (append parts (harp-med-warnings med)) " · ")))

(defun harp-med-view--med-card (med)
  "A tappable medication list card for MED."
  (jetpacs-card
   (list (apply #'jetpacs-row
                (append
                 (delq nil
                       (list
                        (jetpacs-icon (if (harp-med-warnings med) "warning" "medication"))
                        (jetpacs-box
                         (list (jetpacs-column
                                (jetpacs-text (alist-get 'name med) 'title)
                                (jetpacs-text (harp-med-view--summary med) 'caption)))
                         :weight 1)))
                 '(:spacing 12 :align "center"))))
   :on-tap (jetpacs-action "harp.med.tap"
                           :args `((id . ,(alist-get 'id med))) :when-offline "drop")))

(defun harp-med-view--body ()
  "The Meds tab body: Due today over the medication list."
  (let ((meds (harp-med-medications))
        (due (harp-med-due-today)))
    (if (null meds)
        (jetpacs-empty-state
         :icon "medication"
         :title "No medications"
         :caption "Add a medication to track doses, stock, and reminders.")
      (apply #'jetpacs-lazy-column
             (append
              (list (jetpacs-section-header (format "Due today (%d)" (length due))))
              (if due
                  (mapcar #'harp-med-view--due-card due)
                (list (jetpacs-text "All caught up for today." 'caption)))
              (list (jetpacs-divider)
                    (jetpacs-section-header (format "Medications (%d)" (length meds))))
              (mapcar #'harp-med-view--med-card meds))))))

(defun harp-med-view--view (snackbar)
  "The Meds tab."
  (jetpacs-shell-tab-view
   "harp.meds"
   (if (harp-ui-current)
       (harp-med-view--body)
     (harp-ui-no-profile-body))
   :snackbar snackbar))

;; ─── Detail overlay ──────────────────────────────────────────────────────────

(defun harp-med-view--schedule-line (s)
  "A caption describing schedule S."
  (let ((times (alist-get 'times s))
        (interval (alist-get 'interval s))
        (days (alist-get 'days s))
        (cyc (alist-get 'active-days s))
        (from (alist-get 'from s)))
    (string-join
     (delq nil
           (list (concat "Take " (or (alist-get 'amount s) "?"))
                 (cond (times (concat "at " (string-join times ", ")))
                       (interval (concat "every " interval)))
                 (when days (string-join days " "))
                 (when cyc (format "%s on / %s off"
                                   cyc (or (alist-get 'pause-days s) 0)))
                 (when from (concat "from " (harp-ui-format-stamp from)))))
     " · ")))

(defun harp-med-view--dose-line (d)
  "A caption for a logged dose event D."
  (format "%s — %s%s"
          (capitalize (alist-get 'state d))
          (harp-ui-format-stamp (concat "[" (alist-get 'stamp d) "]") t)
          (if (alist-get 'amount d) (concat " · " (alist-get 'amount d)) "")))

(defun harp-med-view--detail-body (med)
  "The detail body for MED."
  (apply #'jetpacs-lazy-column
         (append
          ;; header
          (list (jetpacs-card
                 (list (jetpacs-column
                        (jetpacs-text (alist-get 'name med) 'title)
                        (jetpacs-text (harp-med-view--summary med) 'caption)
                        (or (harp-ui-tag-chips (alist-get 'tags med))
                            (jetpacs-spacer :height 0))))))
          ;; pending doses today
          (let ((pending (harp-med-schedule-pending med)))
            (when pending
              (cons (jetpacs-section-header "Due today")
                    (mapcar (lambda (d)
                              (jetpacs-card
                               (list (jetpacs-column
                                      (jetpacs-text (harp-med-view--dose-label
                                                     (alist-get 'time d)
                                                     (harp-med-view--dose-label
                                                      (alist-get 'amount d)
                                                      (alist-get 'unit d)))
                                                    'body)
                                      (harp-med-view--dose-actions
                                       med (alist-get 'amount d)
                                       (alist-get 'time d))))))
                            pending))))
          ;; schedules
          (list (jetpacs-section-header "Schedule"))
          (or (mapcar (lambda (s) (jetpacs-text (harp-med-view--schedule-line s) 'body))
                      (alist-get 'schedules med))
              (list (jetpacs-text "No schedule." 'caption)))
          ;; notes
          (harp-ui-body-nodes (alist-get 'notes med))
          ;; recent doses
          (list (jetpacs-section-header "History"))
          (let ((doses (seq-take (harp-med-doses med) 12)))
            (or (mapcar (lambda (d) (jetpacs-text (harp-med-view--dose-line d) 'caption))
                        doses)
                (list (jetpacs-text "No doses logged yet." 'caption))))
          (list (apply #'jetpacs-row
                       (append
                        (delq nil
                              (list
                               (when (numberp (alist-get 'refill med))
                                 (jetpacs-button
                                  (format "Refill +%s"
                                          (harp-med-format-amount
                                           (alist-get 'refill med)))
                                  (jetpacs-action
                                   "harp.med.refill"
                                   :args `((id . ,(alist-get 'id med))
                                           (file . ,harp-ui--current-file))
                                   :when-offline "queue"
                                   :dedupe (harp-med-dose-dedupe-key "refill" med nil))
                                  :variant "outlined"))
                               (jetpacs-button "Edit"
                                               (jetpacs-action "harp.med.form.open"
                                                               :args `((id . ,(alist-get 'id med)))
                                                               :when-offline "drop")
                                               :variant "tonal")))
                        '(:spacing 8)))))))

(defun harp-med-view--detail (snackbar)
  "The medication detail overlay."
  (let ((med (and harp-med-view--detail-id
                  (harp-med-by-id harp-med-view--detail-id))))
    (jetpacs-shell-nav-view
     (if med (alist-get 'name med) "Medication")
     (if med (harp-med-view--detail-body med)
       (jetpacs-empty-state :icon "medication" :title "Not found"))
     :nav-action (jetpacs-action "harp.med.back" :when-offline "drop")
     :snackbar snackbar)))

;; ─── registration ────────────────────────────────────────────────────────────

(defun harp-med-view--badge ()
  "Due-dose count overlaid on the Meds tab icon; nil when all caught up.
Called per push; a signaling badge renders nothing, never a broken push."
  (let ((n (length (harp-med-due-today))))
    (and (> n 0) n)))

(with-jetpacs-owner "harp"

  (jetpacs-shell-define-view "harp.meds"
    :builder #'harp-med-view--view
    :tab (list :icon "medication" :label "Meds" :badge #'harp-med-view--badge)
    :when #'harp-ui-profile-open-p
    :order 15)

  (jetpacs-shell-define-view "harp.med.detail"
    :builder #'harp-med-view--detail
    :overlay (lambda () harp-med-view--detail-id)
    :order 92)

  (jetpacs-defaction "harp.med.tap"
    (lambda (args _)
      (let ((id (alist-get 'id args)))
        (when (harp-med-by-id id)
          (setq harp-med-view--detail-id id)
          (jetpacs-shell-push nil :switch-to "harp.med.detail"))))
    :doc "Open a medication's detail view."
    :args '((:name id :type "text" :required t)))

  (jetpacs-defaction "harp.med.back"
    (lambda (_args _)
      (setq harp-med-view--detail-id nil)
      (jetpacs-shell-push nil :switch-to (jetpacs-shell-current-tab)))
    :doc "Close the medication detail view."))

;; Leaving the Meds tab clears the detail selection.
(defun harp-med-view--on-view-switched (view)
  "Reset the detail selection when leaving medications."
  (unless (member view '("harp.meds" "harp.med.detail"))
    (setq harp-med-view--detail-id nil)))

(add-hook 'jetpacs-shell-view-switched-hook #'harp-med-view--on-view-switched)

(provide 'harp-med-view)
;;; harp-med-view.el ends here

;;; ==================================================================
;;; BEGIN apps/harp/harp-med-form.el
;;; ==================================================================

;;; harp-med-form.el --- Harp add/edit medication form -*- lexical-binding: t; -*-

;; An overlay form for creating and editing a medication and its (single)
;; schedule: identity/stock/expiry plus the full schedule vocabulary (times,
;; weekdays, interval, cyclic active/pause, tapering period).  Editing
;; preserves the dose LOGBOOK (see `harp-write-update-medication').  Follows
;; the same jetpacs-form round-trip and `:switch-to' navigation as harp-form.el.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'jetpacs-widgets)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
;; (require 'harp-ui) — bundle-internal, provided above
;; (require 'harp-write) — bundle-internal, provided above
;; (require 'harp-med) — bundle-internal, provided above
;; (require 'harp-med-view)                ; harp-med-view--detail-id (return target) — bundle-internal, provided above

(defvar harp-med-form--open nil
  "Plist (:ref REF) describing the open medication form, or nil.")

(defun harp-med-form--form () (jetpacs-form "harpmedform" "harp"))
(defun harp-med-form--fid (field) (jetpacs-form-field-id (harp-med-form--form) field))

(defun harp-med-form--text (field &optional default)
  (let ((v (jetpacs-form-value (harp-med-form--form) field)))
    (if (and (stringp v) (not (string-empty-p (string-trim v)))) (string-trim v) default)))

(defun harp-med-form--bool (field)
  (let ((v (jetpacs-form-value (harp-med-form--form) field)))
    (and v (not (eq v :false)))))

(defun harp-med-form--seed (field value) (jetpacs-ui-state-put (harp-med-form--fid field) value))

(defun harp-med-form--split-tags (s)
  (when (stringp s)
    (cl-remove-if #'string-empty-p
                  (mapcar (lambda (tg) (string-trim (string-remove-prefix "#" tg)))
                          (split-string s "[ ,]+" t)))))

(defun harp-med-form--num-str (n)
  (when (numberp n)
    (if (= n (truncate n)) (number-to-string (truncate n)) (number-to-string n))))

(defun harp-med-form--stamp-date (stamp)
  "Bare \"YYYY-MM-DD\" from an org STAMP, or nil."
  (and (stringp stamp) (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" stamp)
       (match-string 1 stamp)))

;; ─── open ────────────────────────────────────────────────────────────────────

(defun harp-med-form-open (&optional med)
  "Open the medication form, prefilled from MED when editing."
  (jetpacs-form-reset (harp-med-form--form))
  (let ((s (car (alist-get 'schedules med))))
    (harp-med-form--seed "name" (or (alist-get 'name med) ""))
    (harp-med-form--seed "unit" (or (alist-get 'unit med) ""))
    (harp-med-form--seed "stock" (or (harp-med-form--num-str (alist-get 'stock med)) ""))
    (harp-med-form--seed "low-stock" (or (harp-med-form--num-str (alist-get 'low-stock med)) ""))
    (harp-med-form--seed "refill" (or (harp-med-form--num-str (alist-get 'refill med)) ""))
    (harp-med-form--seed "expires" (or (harp-med-form--stamp-date (alist-get 'expires med)) ""))
    (harp-med-form--seed "tags" (string-join (alist-get 'tags med) " "))
    (harp-med-form--seed "active" (if med (alist-get 'active med) t))
    (harp-med-form--seed "notes" (or (alist-get 'notes med) ""))
    (harp-med-form--seed "amount" (or (alist-get 'amount s) ""))
    (harp-med-form--seed "times" (string-join (alist-get 'times s) ", "))
    (harp-med-form--seed "days" (string-join (alist-get 'days s) " "))
    (harp-med-form--seed "interval" (or (alist-get 'interval s) ""))
    (harp-med-form--seed "window" (or (alist-get 'window s) ""))
    (harp-med-form--seed "cycle-start" (or (harp-med-form--stamp-date (alist-get 'cycle-start s)) ""))
    (harp-med-form--seed "active-days" (or (harp-med-form--num-str (alist-get 'active-days s)) ""))
    (harp-med-form--seed "pause-days" (or (harp-med-form--num-str (alist-get 'pause-days s)) ""))
    (harp-med-form--seed "from" (or (harp-med-form--stamp-date (alist-get 'from s)) ""))
    (harp-med-form--seed "until" (or (harp-med-form--stamp-date (alist-get 'until s)) "")))
  (setq harp-med-form--open (list :ref (alist-get 'ref med)))
  (jetpacs-shell-push nil :switch-to "harp.med.form"))

(defun harp-med-form--close ()
  "Close the form; return to the med detail if it was open, else the tab.
Editing a med from its detail overlay should land back on the (updated)
detail, and the active-view must not be left pointing at a dead overlay."
  (setq harp-med-form--open nil)
  (jetpacs-shell-push nil :switch-to
                      (if harp-med-view--detail-id "harp.med.detail"
                        (jetpacs-shell-current-tab))))

;; ─── widgets ─────────────────────────────────────────────────────────────────

(defun harp-med-form--input (field label &rest kw)
  (apply #'jetpacs-text-input (harp-med-form--fid field)
         :label label :value (harp-med-form--text field) kw))

(defun harp-med-form--date (field label)
  (let ((v (harp-med-form--text field)))
    (jetpacs-date-button (format "%s: %s" label (or v "—"))
                         (jetpacs-action "harp.med.form.set"
                                         :args `((field . ,field)) :when-offline "drop")
                         :value v)))

(defun harp-med-form--view (snackbar)
  (let ((ref (plist-get harp-med-form--open :ref)))
    (jetpacs-shell-nav-view
     (if ref "Edit medication" "New medication")
     (apply #'jetpacs-lazy-column
            (append
             (list
              (harp-med-form--input "name" "Name")
              (harp-med-form--input "unit" "Unit" :hint "tab, mg, ml")
              (jetpacs-row (jetpacs-box (list (harp-med-form--input
                                               "stock" "Stock" :keyboard "number")) :weight 1)
                           (jetpacs-box (list (harp-med-form--input
                                               "low-stock" "Low at" :keyboard "number")) :weight 1)
                           (jetpacs-box (list (harp-med-form--input
                                               "refill" "Refill" :keyboard "number")) :weight 1)
                           :spacing 8)
              (harp-med-form--date "expires" "Expires")
              (harp-med-form--input "tags" "Tags" :hint "space separated")
              (jetpacs-switch (harp-med-form--fid "active") :label "Active"
                              :checked (harp-med-form--bool "active")
                              :on-change (jetpacs-action "harp.med.form.set"
                                                         :args '((field . "active"))
                                                         :when-offline "drop"))
              (harp-med-form--input "notes" "Notes" :multi-line t :min-lines 2)
              (jetpacs-divider)
              (jetpacs-section-header "Schedule")
              (harp-med-form--input "amount" "Amount per dose")
              (harp-med-form--input "times" "Times" :hint "08:00, 20:00")
              (harp-med-form--input "days" "Weekdays" :hint "Mon Tue … (blank = every day)")
              (harp-med-form--input "interval" "Interval" :hint "6h / 3d (instead of times)")
              (harp-med-form--input "window" "Interval window" :hint "08:00-22:00")
              (harp-med-form--date "cycle-start" "Cycle start")
              (jetpacs-row (jetpacs-box (list (harp-med-form--input
                                               "active-days" "Active days" :keyboard "number")) :weight 1)
                           (jetpacs-box (list (harp-med-form--input
                                               "pause-days" "Pause days" :keyboard "number")) :weight 1)
                           :spacing 8)
              (harp-med-form--date "from" "From")
              (harp-med-form--date "until" "Until")
              (jetpacs-button "Save"
                              ;; Generation-keyed dedupe — see harp-form.el.
                              (jetpacs-action "harp.med.form.save"
                                              :when-offline "queue"
                                              :dedupe (harp-med-form--fid "save"))
                              :variant "filled"))
             (when ref
               (list (jetpacs-button "Delete medication"
                                     (jetpacs-action "harp.med.delete"
                                                     :args `((id . ,(alist-get 'id ref)))
                                                     :when-offline "queue"
                                                     :dedupe (format "harp.med.delete/%s"
                                                                     (alist-get 'id ref)))
                                     :variant "text")))))
     :nav-action (jetpacs-action "harp.med.form.cancel" :when-offline "drop")
     :snackbar snackbar)))

;; ─── save ────────────────────────────────────────────────────────────────────

(defun harp-med-form--fields ()
  `((name . ,(harp-med-form--text "name"))
    (unit . ,(harp-med-form--text "unit"))
    (stock . ,(harp-med-form--text "stock"))
    (low-stock . ,(harp-med-form--text "low-stock"))
    (refill . ,(harp-med-form--text "refill"))
    (expires . ,(harp-med-form--text "expires"))
    (tags . ,(harp-med-form--split-tags (harp-med-form--text "tags")))
    (active . ,(harp-med-form--bool "active"))
    (notes . ,(harp-med-form--text "notes"))
    (amount . ,(harp-med-form--text "amount"))
    (times . ,(harp-med-form--text "times"))
    (days . ,(harp-med-form--text "days"))
    (interval . ,(harp-med-form--text "interval"))
    (window . ,(harp-med-form--text "window"))
    (cycle-start . ,(harp-med-form--text "cycle-start"))
    (active-days . ,(harp-med-form--text "active-days"))
    (pause-days . ,(harp-med-form--text "pause-days"))
    (from . ,(harp-med-form--text "from"))
    (until . ,(harp-med-form--text "until"))))

(defun harp-med-form--save ()
  "Persist the medication form; non-nil on success."
  (if (null (harp-med-form--text "name"))
      (progn (jetpacs-shell-notify "Name is required") nil)
    (let ((ref (plist-get harp-med-form--open :ref))
          (fields (harp-med-form--fields)))
      (if ref
          (harp-write-update-medication ref fields)
        (harp-write-add-medication fields))
      (jetpacs-shell-notify (if ref "Medication updated" "Medication added"))
      t)))

;; ─── FAB + registration ──────────────────────────────────────────────────────

(with-jetpacs-owner "harp"

  (jetpacs-shell-define-view "harp.med.form"
    :builder #'harp-med-form--view
    :overlay (lambda () harp-med-form--open)
    :order 93)

  (jetpacs-defaction "harp.med.form.open"
    (lambda (args _)
      (harp-med-form-open (and (alist-get 'id args) (harp-med-by-id (alist-get 'id args)))))
    :doc "Open the add/edit medication form (optional id to edit)."
    :args '((:name id :type "text")))

  (jetpacs-defaction "harp.med.form.cancel"
    (lambda (_args _) (harp-med-form--close))
    :doc "Close the medication form without saving.")

  (jetpacs-defaction "harp.med.form.set"
    (lambda (args _)
      (let ((field (alist-get 'field args)) (value (alist-get 'value args)))
        (when (stringp field)
          (jetpacs-ui-state-put (harp-med-form--fid field)
                                (pcase field
                                  ("active" (and value (not (eq value :false))))
                                  (_ value)))
          (jetpacs-shell-push))))
    :doc "Update a medication-form picker/switch field."
    :args '((:name field :type "text" :required t)
            (:name value :type "text")))

  (jetpacs-defaction "harp.med.form.save"
    (lambda (_args _)
      (cond
       ((null harp-med-form--open)
        ;; Queued save whose form state didn't survive a restart — be loud.
        (jetpacs-shell-notify "Saved form expired — reopen and re-enter")
        (jetpacs-shell-push))
       ((harp-med-form--save) (harp-med-form--close))))
    :doc "Persist the medication form.")

  (jetpacs-defaction "harp.med.delete"
    (lambda (args _)
      (let ((med (harp-med-by-id (alist-get 'id args))))
        (when (and med (yes-or-no-p (format "Delete %s? " (alist-get 'name med))))
          (harp-write-delete-medication (alist-get 'ref med))
          (setq harp-med-form--open nil
                ;; the med is gone — its detail overlay must not linger active
                harp-med-view--detail-id nil)
          (jetpacs-shell-notify "Medication deleted")
          (jetpacs-shell-push nil :switch-to "harp.meds"))))
    :doc "Delete a medication (with confirmation)."
    :args '((:name id :type "text" :required t))))

(provide 'harp-med-form)
;;; harp-med-form.el ends here

;;; ==================================================================
;;; BEGIN apps/harp/harp-med-notify.el
;;; ==================================================================

;;; harp-med-notify.el --- Interactive dose notifications -*- lexical-binding: t; -*-

;; When a medication dose is due, push an interactive notification surface
;; carrying Taken / Skip / Snooze action buttons (SPEC §9 `meta.actions', new
;; in the fadeeb2 core).  Tapping a button logs the dose (or snoozes) without
;; opening the app.  This is the Emacs-connected companion to the scheduled
;; `reminders.set' alarm in harp-reminders.el (which stays as the Emacs-dead
;; nudge): the alarm says "it's time", this lets you act on it.
;;
;; A dose is "due now" when its scheduled time has passed, it is still pending
;; (count-based, `harp-med-schedule'), and it is not snoozed.  One notification
;; per medication (its earliest due dose); re-pushed only when its content
;; changes, so a live dose does not re-alert every tick.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'jetpacs-widgets)
(require 'jetpacs-surfaces)
(require 'jetpacs-shell)
(require 'jetpacs-settings)
(require 'jetpacs)                      ; jetpacs--session
;; (require 'harp-ui) — bundle-internal, provided above
;; (require 'harp-med) — bundle-internal, provided above
;; (require 'harp-med-schedule) — bundle-internal, provided above

(defcustom harp-med-notify-enabled t
  "When non-nil, due medication doses raise interactive notifications."
  :type 'boolean :group 'harp)

(defcustom harp-med-notify-snooze-minutes 15
  "Minutes to silence a dose notification after Snooze."
  :type 'integer :group 'harp)

(defcustom harp-med-notify-interval 60
  "Seconds between due-dose checks while connected; nil disables the timer.
The connect and after-push hooks refresh immediately; this keeps a due dose
timely between user interactions (a resident Emacs is assumed — see the
jetpacs execution model)."
  :type '(choice (const :tag "no timer" nil) integer)
  :group 'harp)

(defvar harp-med-notify--snoozed (make-hash-table :test 'equal)
  "Map of \"MEDID\\0TIME\" -> snooze-until float-time.")

(defvar harp-med-notify--pushed nil
  "Alist of pushed surface-id -> content signature, to avoid re-alerting.")

(defun harp-med-notify--surface-id (med)
  (format "notification:harp.dose.%s" (alist-get 'id med)))

(defun harp-med-notify--label (amount unit)
  (string-trim (concat (or amount "") " " (or unit ""))))

;; ─── which doses are due now ─────────────────────────────────────────────────

(defun harp-med-notify--due-now (&optional profile)
  "Currently-due dose entries: past scheduled time, pending, not snoozed.
One entry per medication (its earliest due dose)."
  (let ((now (float-time)) (seen (make-hash-table :test 'equal)) out)
    (dolist (e (if profile
                   (harp-med-schedule-due-today profile)
                 (harp-med-due-today)))
      (let* ((med (alist-get 'med e))
             (mid (alist-get 'id med))
             (at (alist-get 'at e))
             (key (format "%s\0%s" mid (alist-get 'time e)))
             (snooze (gethash key harp-med-notify--snoozed)))
        (when (and at (<= at now)
                   (or (null snooze) (>= now snooze))
                   (not (gethash mid seen)))
          (puthash mid t seen)
          (push e out))))
    (nreverse out)))

;; ─── the notification spec ───────────────────────────────────────────────────

(defun harp-med-notify--spec (med entry)
  "An interactive notification spec for a due dose ENTRY of MED."
  (let* ((label (harp-med-notify--label (alist-get 'amount entry)
                                        (alist-get 'unit entry)))
         (args `((id . ,(alist-get 'id med))
                 (amount . ,(alist-get 'amount entry))
                 (unit . ,(alist-get 'unit med))
                 (file . ,harp-ui--current-file)))
         (snooze-args (append args `((time . ,(alist-get 'time entry)))))
         (time (alist-get 'time entry)))
    (jetpacs-notification-spec
     :channel "harp.doses"
     :priority "high"
     :category "reminder"
     :body (list (jetpacs-text (alist-get 'name med) 'title)
                 (jetpacs-text (string-trim
                                (concat "Due " (alist-get 'time entry)
                                        (unless (string-empty-p label)
                                          (concat " · " label))))
                               'body))
     :actions
     (list (jetpacs-notification-action
            "Taken" (jetpacs-action "harp.med.taken" :args args
                                    :when-offline "queue"
                                    :dedupe (harp-med-dose-dedupe-key "taken" med time))
            :dismiss t)
           (jetpacs-notification-action
            "Skip" (jetpacs-action "harp.med.skipped" :args args
                                   :when-offline "queue"
                                   :dedupe (harp-med-dose-dedupe-key "skipped" med time))
            :dismiss t)
           (jetpacs-notification-action
            "Snooze" (jetpacs-action "harp.med.snooze" :args snooze-args
                                     :when-offline "queue"
                                     :dedupe (harp-med-dose-dedupe-key "snooze" med time))
            :dismiss t)))))

(defun harp-med-notify--signature (med entry)
  "A content signature so an unchanged notification is not re-pushed."
  (format "%s|%s|%s" (alist-get 'name med) (alist-get 'time entry)
          (harp-med-notify--label (alist-get 'amount entry) (alist-get 'unit med))))

;; ─── sync ────────────────────────────────────────────────────────────────────

(defun harp-med-notify--prune-snoozed ()
  "Drop expired snoozes — the map would otherwise grow one entry per snooze."
  (let ((now (float-time)))
    (maphash (lambda (k until)
               (when (< until now) (remhash k harp-med-notify--snoozed)))
             harp-med-notify--snoozed)))

(defun harp-med-notify-sync ()
  "Reconcile due-dose notifications with the current state (connected only)."
  (when (and harp-med-notify-enabled jetpacs--session (harp-ui-current))
    (harp-med-notify--prune-snoozed)
    (let ((want '()))
      (dolist (e (harp-med-notify--due-now))
        (let* ((med (alist-get 'med e))
               (sid (harp-med-notify--surface-id med))
               (sig (harp-med-notify--signature med e)))
          (push (cons sid sig) want)
          ;; Push only when new or changed — never re-alert a live dose.
          (unless (equal sig (cdr (assoc sid harp-med-notify--pushed)))
            (jetpacs-surface-push sid (harp-med-notify--spec med e)))))
      ;; Remove notifications for doses no longer due.
      (dolist (old harp-med-notify--pushed)
        (unless (assoc (car old) want)
          (jetpacs-surface-remove (car old))))
      (setq harp-med-notify--pushed want))))

(defun harp-med-notify--clear-med (med)
  "Remove MED's dose notification, if any."
  (let ((sid (harp-med-notify--surface-id med)))
    (when (assoc sid harp-med-notify--pushed)
      (jetpacs-surface-remove sid)
      (setq harp-med-notify--pushed
            (assoc-delete-all sid harp-med-notify--pushed)))))

;; ─── timer ───────────────────────────────────────────────────────────────────

(defvar harp-med-notify--timer nil)

(defun harp-med-notify--start-timer (&rest _)
  "Start the periodic due-dose check (idempotent)."
  (when (and harp-med-notify-enabled harp-med-notify-interval
             (not (timerp harp-med-notify--timer)))
    (setq harp-med-notify--timer
          (run-with-timer harp-med-notify-interval harp-med-notify-interval
                          #'harp-med-notify-sync))))

(defun harp-med-notify--stop-timer (&rest _)
  (when (timerp harp-med-notify--timer)
    (cancel-timer harp-med-notify--timer)
    (setq harp-med-notify--timer nil)))

(defun harp-med-notify--on-enabled-change (value)
  "React to the `harp-med-notify-enabled' toggle arriving from the wire.
Turning it on mid-session must arm the timer and reconcile now — waiting
for the next connect would leave doses silent for a whole session."
  (if value
      (progn (harp-med-notify--start-timer)
             (harp-med-notify-sync))
    (harp-med-notify--stop-timer)
    ;; Clear any live dose notifications so the phone doesn't keep them.
    (dolist (old harp-med-notify--pushed)
      (jetpacs-surface-remove (car old)))
    (setq harp-med-notify--pushed nil)))

;; ─── actions + wiring ────────────────────────────────────────────────────────

(with-jetpacs-owner "harp"
  (jetpacs-defaction "harp.med.snooze"
    (lambda (args _)
      (let* ((id (alist-get 'id args))
             (time (alist-get 'time args))
             (key (format "%s\0%s" id time))
             (med (harp-med-by-id id)))
        (puthash key (+ (float-time) (* 60 harp-med-notify-snooze-minutes))
                 harp-med-notify--snoozed)
        (when med (harp-med-notify--clear-med med))
        (jetpacs-shell-notify
         (format "Snoozed %s min" harp-med-notify-snooze-minutes))
        ;; notify only queues the snackbar — push so it actually shows.
        (jetpacs-shell-push)))
    :doc "Snooze a due-dose notification."
    :args '((:name id :type "text" :required t)
            (:name time :type "text")
            (:name amount :type "text")
            (:name unit :type "text")))

  (jetpacs-settings-register-section
   "Medications"
   '((harp-med-notify-enabled :label "Due-dose notifications"
                              :after-set harp-med-notify--on-enabled-change)
     (harp-med-notify-snooze-minutes :label "Snooze minutes")
     (harp-med-notify-interval :label "Due check interval (seconds)"))))

;; Refresh on connect (and start the timer) and after every app push.  The
;; timer keeps ticking while disconnected but `harp-med-notify-sync' no-ops
;; without a session, so there is no disconnect hook to stop it.
(add-hook 'jetpacs-connected-hook #'harp-med-notify--start-timer)
(add-hook 'jetpacs-connected-hook #'harp-med-notify-sync)
(add-hook 'jetpacs-shell-after-push-hook #'harp-med-notify-sync)

(provide 'harp-med-notify)
;;; harp-med-notify.el ends here

;;; ==================================================================
;;; BEGIN apps/harp/harp-home.el
;;; ==================================================================

;;; harp-home.el --- Harp profile launcher (Home tab) -*- lexical-binding: t; -*-

;; The landing tab: a list of discovered profiles.  Tapping one opens it as
;; the current profile and jumps to the Journals tab.  Replaces harp-kmp's
;; HomeScreen / OpenProfileDialog / RecentProfiles.

;;; Code:

(require 'jetpacs-widgets)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
(require 'jetpacs-apps)
;; (require 'harp-profile) — bundle-internal, provided above
;; (require 'harp-ui) — bundle-internal, provided above

(defun harp-home--profile-card (info)
  "A tappable card for profile summary INFO."
  (let* ((file (alist-get 'file info))
         (type (alist-get 'type info))
         (current (equal file harp-ui--current-file)))
    (jetpacs-card
     (list (apply #'jetpacs-row
                  (append
                   (delq nil
                         (list
                          (jetpacs-icon (harp-ui-type-icon type))
                          (jetpacs-box
                           (list (jetpacs-column
                                  (jetpacs-text (alist-get 'name info) 'title)
                                  (jetpacs-text (harp-ui-type-label type) 'caption)))
                           :weight 1)
                          (when current (jetpacs-icon "check_circle"))))
                   '(:spacing 12 :align "center"))))
     :on-tap (jetpacs-action "harp.profile.open"
                             :args `((file . ,file)) :when-offline "drop"))))

(defun harp-home--body ()
  "The Home view body: header card plus the profile list."
  (let ((profiles (harp-ui-profiles)))
    (apply #'jetpacs-lazy-column
           (append
            (list (jetpacs-card
                   (list (jetpacs-column
                          (jetpacs-text "Harp" 'title)
                          (jetpacs-text "Personal Health Record" 'caption)))))
            (if profiles
                (cons (jetpacs-section-header
                       (format "Profiles (%d)" (length profiles)))
                      (mapcar #'harp-home--profile-card profiles))
              (list (jetpacs-empty-state
                     :icon "person_add"
                     :title "No profiles yet"
                     :caption (format "Add a profile .org file under %s, or load the samples to explore."
                                      (abbreviate-file-name (or harp-directory "")))
                     :on-tap (jetpacs-action "harp.demo.setup" :when-offline "drop")
                     :action-label "Load sample profiles")))))))

(with-jetpacs-owner "harp"

  ;; The empty-state button only exists while no profiles do; this keeps the
  ;; sample corpus reachable afterwards (harp.demo.setup offers a refresh
  ;; when the files already exist).
  (jetpacs-shell-add-drawer-item
   60
   (lambda ()
     (jetpacs-drawer-item "dataset" "Sample profiles"
                          (jetpacs-action "harp.demo.setup"
                                          :when-offline "drop"))))

  (jetpacs-shell-define-view "harp.home"
    :builder (lambda (snackbar)
               (jetpacs-shell-tab-view "harp.home" (harp-home--body)
                                       :snackbar snackbar))
    :tab '(:icon "favorite" :label "Home")
    :order 0)

  (jetpacs-defaction "harp.profile.open"
    (lambda (args _)
      (let ((file (alist-get 'file args)))
        ;; Allowlist rule: only open a path that is actually a discovered
        ;; profile — the wire never opens arbitrary files.
        (when (and (stringp file) (member file (harp-profile-files)))
          (harp-ui-set-current file)
          (let ((p (harp-ui-current)))
            (jetpacs-shell-notify
             (format "Opened %s" (or (alist-get 'name p) "profile"))))
          (jetpacs-shell-set-current-tab "harp.journal"))))
    :doc "Open a discovered profile by file path."
    :args '((:name file :type "text" :required t))))

(provide 'harp-home)
;;; harp-home.el ends here

;;; ==================================================================
;;; BEGIN apps/harp/harp-journal.el
;;; ==================================================================

;;; harp-journal.el --- Harp journals view -*- lexical-binding: t; -*-

;; The Journals tab: a selector across the profile's journals, then the
;; selected journal's entries newest-first as cards — timestamp, body,
;; metric readings, attachments, tags — with a tag filter bar.  Read-only
;; for now; capture lands in a later pass.  Mirrors harp-kmp's JournalView /
;; JournalSelector / JournalEntryView.

;;; Code:

(require 'cl-lib)
(require 'jetpacs-widgets)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
;; (require 'harp-ui) — bundle-internal, provided above

(defvar harp-journal--selected nil
  "Name of the selected journal, or nil to fall back to the first.")

(defun harp-journal--journals ()
  "Journals of the current profile."
  (alist-get 'journals (harp-ui-current)))

(defun harp-journal--current-name ()
  "The selected journal name, or the first journal's name."
  (let ((journals (harp-journal--journals)))
    (or (and harp-journal--selected
             (cl-find harp-journal--selected journals
                      :key (lambda (j) (alist-get 'name j)) :test #'equal)
             harp-journal--selected)
        (alist-get 'name (car journals)))))

(defun harp-journal--selector (journals current)
  "A chip row over JOURNALS with CURRENT selected; nil for a single journal."
  (when (> (length journals) 1)
    (apply #'jetpacs-flow-row
           (mapcar (lambda (j)
                     (let ((name (alist-get 'name j)))
                       (jetpacs-chip name
                                     :selected (equal name current)
                                     :on-tap (jetpacs-action "harp.journal.select"
                                                             :args `((name . ,name))
                                                             :when-offline "drop"))))
                   journals))))

(defun harp-journal--entry-card (e)
  "A card rendering journal entry E; tap to edit."
  (let* ((header (apply #'jetpacs-row
                        (append
                         (delq nil
                               (list
                                (jetpacs-text (harp-ui-format-stamp
                                               (alist-get 'datetime e) t)
                                              'caption)
                                (jetpacs-spacer :weight 1)
                                (when (alist-get 'private e)
                                  (jetpacs-icon "lock" :size 16))))
                         '(:align "center"))))
         (column (apply #'jetpacs-column
                        (delq nil
                              (append
                               (list header)
                               (harp-ui-body-nodes (alist-get 'body e))
                               (list (harp-ui-metric-chips (alist-get 'metric-values e))
                                     (harp-ui-attachment-chips (alist-get 'assets e))
                                     (harp-ui-tag-chips (alist-get 'tags e))))))))
    (jetpacs-card
     (list column)
     :on-tap (jetpacs-action "harp.form.open"
                             :args `((kind . "entry") (id . ,(alist-get 'id e)))
                             :when-offline "drop"))))

(defun harp-journal--view (snackbar)
  "The Journals screen for the current profile."
  (jetpacs-shell-tab-view
   "harp.journal"
   (if (null (harp-ui-current))
       (harp-ui-no-profile-body)
     (let* ((journals (harp-journal--journals))
            (current (harp-journal--current-name))
            (journal (cl-find current journals
                              :key (lambda (j) (alist-get 'name j)) :test #'equal))
            (entries (cl-remove-if-not #'harp-ui-item-visible-p
                                       (alist-get 'entries journal))))
       (apply #'jetpacs-lazy-column
              (delq nil
                    (append
                     (list (harp-journal--selector journals current)
                           (harp-ui-filter-bar (harp-ui-current-tags)))
                     (if entries
                         (mapcar #'harp-journal--entry-card entries)
                       (list (jetpacs-empty-state
                              :icon "edit_note"
                              :title "No entries"
                              :caption "This journal has no entries yet."))))))) )
   :snackbar snackbar))

(with-jetpacs-owner "harp"

  (jetpacs-shell-define-view "harp.journal"
    :builder #'harp-journal--view
    :tab '(:icon "menu_book" :label "Journals")
    :when #'harp-ui-profile-open-p
    :order 10)

  (jetpacs-defaction "harp.journal.select"
    (lambda (args _)
      (let ((name (alist-get 'name args)))
        (when (stringp name)
          (setq harp-journal--selected name)
          (jetpacs-shell-push))))
    :doc "Select which journal to show."
    :args '((:name name :type "text" :required t))))

;; Switching away resets the journal selection to the first.
(defun harp-journal--on-view-switched (view)
  "Reset the selected journal when leaving the Journals tab."
  (unless (equal view "harp.journal")
    (setq harp-journal--selected nil)))

(add-hook 'jetpacs-shell-view-switched-hook #'harp-journal--on-view-switched)

(provide 'harp-journal)
;;; harp-journal.el ends here

;;; ==================================================================
;;; BEGIN apps/harp/harp-documents.el
;;; ==================================================================

;;; harp-documents.el --- Harp documents view -*- lexical-binding: t; -*-

;; The Documents tab: medical documents newest-first as cards — title,
;; date, source, annotation, attachment files, metric readings, tags —
;; with a tag filter bar.  Read-only for now; OCR and capture land later.
;; Mirrors harp-kmp's DocumentsView / DocumentCard.

;;; Code:

(require 'cl-lib)
(require 'jetpacs-widgets)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
;; (require 'harp-ui) — bundle-internal, provided above

(defun harp-documents--card (d)
  "A card rendering document D; tap to edit."
  (let* ((source (alist-get 'source d))
         (header (apply #'jetpacs-row
                        (append
                         (delq nil
                               (list
                                (jetpacs-box
                                 (list (jetpacs-text (alist-get 'name d) 'title))
                                 :weight 1)
                                (when (alist-get 'private d)
                                  (jetpacs-icon "lock" :size 16))))
                         '(:align "center"))))
         (column (apply #'jetpacs-column
                        (delq nil
                              (append
                               (list header
                                     (jetpacs-text
                                      (concat (harp-ui-format-stamp (alist-get 'datetime d))
                                              (when source (concat " · " source)))
                                      'caption))
                               (harp-ui-body-nodes (alist-get 'annotation d))
                               (list (harp-ui-attachment-chips (alist-get 'assets d))
                                     (harp-ui-metric-chips (alist-get 'metric-values d))
                                     (harp-ui-tag-chips (alist-get 'tags d))))))))
    (jetpacs-card
     (list column)
     :on-tap (jetpacs-action "harp.form.open"
                             :args `((kind . "document") (id . ,(alist-get 'id d)))
                             :when-offline "drop"))))

(defun harp-documents--view (snackbar)
  "The Documents screen for the current profile."
  (jetpacs-shell-tab-view
   "harp.documents"
   (if (null (harp-ui-current))
       (harp-ui-no-profile-body)
     (let ((docs (cl-remove-if-not #'harp-ui-item-visible-p
                                   (alist-get 'documents (harp-ui-current)))))
       (apply #'jetpacs-lazy-column
              (delq nil
                    (append
                     (list (harp-ui-filter-bar (harp-ui-current-tags)))
                     (if docs
                         (mapcar #'harp-documents--card docs)
                       (list (jetpacs-empty-state
                              :icon "description"
                              :title "No documents"
                              :caption "This profile has no documents yet."))))))))
   :snackbar snackbar))

(with-jetpacs-owner "harp"
  (jetpacs-shell-define-view "harp.documents"
    :builder #'harp-documents--view
    :tab '(:icon "description" :label "Documents")
    :when #'harp-ui-profile-open-p
    :order 20))

(provide 'harp-documents)
;;; harp-documents.el ends here

;;; ==================================================================
;;; BEGIN apps/harp/harp-metrics.el
;;; ==================================================================

;;; harp-metrics.el --- Harp metrics view -*- lexical-binding: t; -*-

;; The Metrics tab: each metric definition as a card with its unit and
;; healthy range, plus a line chart of the readings harvested from journal
;; entries and documents (the #id(value) tokens).  Read-only for now.
;; Mirrors harp-kmp's MetricsView / MetricsPlot.

;;; Code:

(require 'cl-lib)
(require 'jetpacs-widgets)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
;; (require 'harp-ui) — bundle-internal, provided above

(defun harp-metrics--bound (n)
  "Format numeric bound N compactly, or nil."
  (when (numberp n)
    (number-to-string n)))

(defun harp-metrics--range-label (range)
  "A \"lo–hi\" label for a (LO . HI) RANGE cons, honouring nil bounds."
  (let ((lo (harp-metrics--bound (car range)))
        (hi (harp-metrics--bound (cdr range))))
    (cond ((and lo hi) (format "%s–%s" lo hi))
          (lo (format "≥ %s" lo))
          (hi (format "≤ %s" hi))
          (t nil))))

(defun harp-metrics--chart (metric values)
  "A line chart node for METRIC's VALUES, or nil when there are none."
  (when (and values (jetpacs-node-supported-p "chart"))
    (let ((points (mapcar (lambda (mv) (alist-get 'value mv)) values))
          (range (alist-get 'range metric)))
      (jetpacs-chart
       (list (jetpacs-chart-series points :label (alist-get 'name metric)))
       :kind "line"
       :height 160
       :y-range (when (and (numberp (car range)) (numberp (cdr range)))
                  (list (car range) (cdr range)))
       :summary (format "%s readings" (alist-get 'name metric))))))

(defun harp-metrics--card (metric profile)
  "A card for METRIC within PROFILE: header, healthy range, chart."
  (let* ((values (harp-profile-metric-values profile (alist-get 'id metric)))
         (latest (car (last values)))
         (healthy (harp-metrics--range-label (alist-get 'healthy-range metric)))
         (chart (harp-metrics--chart metric values)))
    (jetpacs-card
     (list (apply #'jetpacs-column
                  (delq nil
                        (list
                         (apply #'jetpacs-row
                                (append
                                 (delq nil
                                       (list
                                        (jetpacs-box
                                         (list (jetpacs-text (alist-get 'name metric) 'title))
                                         :weight 1)
                                        (when latest
                                          (jetpacs-text
                                           (format "%s %s"
                                                   (alist-get 'value latest)
                                                   (alist-get 'unit metric))
                                           'headline))))
                                 '(:align "center")))
                         (when healthy
                           (jetpacs-text (format "Healthy: %s %s"
                                                 healthy (alist-get 'unit metric))
                                         'caption))
                         (harp-ui-tag-chips (alist-get 'tags metric))
                         (or chart
                             (unless values
                               (jetpacs-text "No readings yet." 'caption)))))))
     :on-tap (jetpacs-action "harp.form.open"
                             :args `((kind . "metric") (id . ,(alist-get 'id metric)))
                             :when-offline "drop"))))

(defun harp-metrics--view (snackbar)
  "The Metrics screen for the current profile."
  (jetpacs-shell-tab-view
   "harp.metrics"
   (let ((profile (harp-ui-current)))
     (if (null profile)
         (harp-ui-no-profile-body)
       (let ((metrics (alist-get 'metrics profile)))
         (if metrics
             (apply #'jetpacs-lazy-column
                    (mapcar (lambda (m) (harp-metrics--card m profile)) metrics))
           (jetpacs-empty-state
            :icon "monitoring"
            :title "No metrics"
            :caption "Define metrics in the profile's Metadata section.")))))
   :snackbar snackbar))

(with-jetpacs-owner "harp"
  (jetpacs-shell-define-view "harp.metrics"
    :builder #'harp-metrics--view
    :tab '(:icon "monitoring" :label "Metrics")
    :when #'harp-ui-profile-open-p
    :order 30))

(provide 'harp-metrics)
;;; harp-metrics.el ends here

;;; ==================================================================
;;; BEGIN apps/harp/harp-reminders.el
;;; ==================================================================

;;; harp-reminders.el --- Harp reminders view -*- lexical-binding: t; -*-

;; The Reminders tab: the profile's scheduled/deadline TODOs as cards, over
;; a month calendar marking the days that carry a reminder.  Read-only for
;; now; scheduling edits and companion notifications land in a later pass.
;; Mirrors harp-kmp's RemindersView.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'jetpacs-widgets)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
(require 'jetpacs-device)
;; (require 'harp-ui) — bundle-internal, provided above
;; (require 'harp-med) — bundle-internal, provided above
;; (require 'harp-med-schedule) — bundle-internal, provided above

(defun harp-reminders--date-of (stamp)
  "The \"YYYY-MM-DD\" date embedded in Org timestamp STAMP, or nil."
  (and (stringp stamp)
       (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" stamp)
       (match-string 1 stamp)))

(defvar harp-reminders--month nil
  "The \"YYYY-MM\" month shown on the calendar, or nil for the current one.")

(defun harp-reminders--month-bounds (month)
  "A (START . END) cons of Lisp times covering \"YYYY-MM\" MONTH.
END is the first instant of the following month."
  (let* ((y (string-to-number (substring month 0 4)))
         (m (string-to-number (substring month 5 7))))
    (cons (encode-time 0 0 0 1 m y)
          (if (= m 12)
              (encode-time 0 0 0 1 1 (1+ y))
            (encode-time 0 0 0 1 (1+ m) y)))))

(defun harp-reminders--occurrences-in-month (stamp month)
  "Dates (\"YYYY-MM-DD\") STAMP lands on within MONTH, honoring a repeater.
A non-repeating stamp contributes at most its own date; a `+N[hdwmy]'
repeater is stepped from the base date through the month."
  (let ((base (and (stringp stamp)
                   (ignore-errors (org-time-string-to-time stamp))))
        (rep (harp-reminders--parse-repeater stamp)))
    (when base
      (let* ((bounds (harp-reminders--month-bounds month))
             (start (car bounds)) (end (cdr bounds))
             ;; Jump the base up close to the month first, so a small repeater
             ;; on an old stamp doesn't burn the step cap before reaching it.
             (tt (if rep
                     (let* ((per (harp-reminders--seconds-per (car rep) (cdr rep)))
                            (gap (- (float-time start) (float-time base)))
                            (jumps (if (and (> per 0) (> gap 0))
                                       (max 0 (1- (floor gap per))) 0)))
                       (harp-reminders--step-n base (car rep) (cdr rep) jumps))
                   base))
             (i 0) dates)
        (while (and (time-less-p tt end) (< i 4000))
          (unless (time-less-p tt start)
            (push (format-time-string "%Y-%m-%d" tt) dates))
          (if rep
              (setq tt (harp-reminders--step tt (car rep) (cdr rep)))
            (setq tt end))
          (setq i (1+ i)))
        (nreverse dates)))))

(defun harp-reminders--marks (reminders &optional month)
  "Month-grid marks for MONTH (default: this month) over REMINDERS.
Repeating reminders mark every occurrence in the month, so navigating
ahead shows the future cadence, not just base dates."
  (let ((month (or month (format-time-string "%Y-%m")))
        (counts (make-hash-table :test #'equal)))
    (dolist (r reminders)
      (dolist (stamp (list (alist-get 'scheduled r) (alist-get 'deadline r)))
        (dolist (d (harp-reminders--occurrences-in-month stamp month))
          (puthash d (1+ (gethash d counts 0)) counts))))
    (let (marks)
      (maphash (lambda (d n) (push (cons d (min n 3)) marks)) counts)
      marks)))

(defun harp-reminders--when-label (r)
  "A friendly \"when\" caption for reminder R."
  (let ((sched (alist-get 'scheduled r))
        (deadline (alist-get 'deadline r)))
    (cond
     ((and sched deadline)
      (format "Scheduled %s · Due %s"
              (harp-ui-format-stamp sched) (harp-ui-format-stamp deadline)))
     (sched (format "Scheduled %s" (harp-ui-format-stamp sched)))
     (deadline (format "Due %s" (harp-ui-format-stamp deadline)))
     (t ""))))

(defun harp-reminders--card (r)
  "A card rendering reminder R."
  (jetpacs-card
   (list (apply #'jetpacs-column
                (delq nil
                      (append
                       (list (apply #'jetpacs-row
                                    (append
                                     (delq nil
                                           (list
                                            (when (alist-get 'todo r)
                                              (jetpacs-assist-chip (alist-get 'todo r)))
                                            (jetpacs-box
                                             (list (jetpacs-text (alist-get 'title r) 'title))
                                             :weight 1)))
                                     '(:align "center")))
                             (jetpacs-text (harp-reminders--when-label r) 'caption))
                       (harp-ui-body-nodes (alist-get 'body r))
                       (list (harp-ui-tag-chips (alist-get 'tags r)))))))))

(defun harp-reminders--view (snackbar)
  "The Reminders screen for the current profile."
  (jetpacs-shell-tab-view
   "harp.reminders"
   (let ((profile (harp-ui-current)))
     (if (null profile)
         (harp-ui-no-profile-body)
       (let ((reminders (alist-get 'reminders profile)))
         (if reminders
             (apply #'jetpacs-lazy-column
                    (append
                     (when (jetpacs-node-supported-p "month_grid")
                       (let ((month (or harp-reminders--month
                                        (format-time-string "%Y-%m"))))
                         (list (jetpacs-month-grid
                                month
                                :marks (harp-reminders--marks reminders month)
                                :on-month-change
                                (jetpacs-action "harp.reminders.month"
                                                :when-offline "drop")))))
                     (mapcar #'harp-reminders--card reminders)))
           (jetpacs-empty-state
            :icon "notifications"
            :title "No reminders"
            :caption "This profile has no reminders yet.")))))
   :snackbar snackbar))

(with-jetpacs-owner "harp"
  (jetpacs-shell-define-view "harp.reminders"
    :builder #'harp-reminders--view
    :tab '(:icon "notifications" :label "Reminders")
    :when #'harp-ui-profile-open-p
    :order 40)

  (jetpacs-defaction "harp.reminders.month"
    (lambda (args _)
      (let ((m (alist-get 'value args)))
        (when (and (stringp m)
                   (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}\\'" m))
          (setq harp-reminders--month m)
          (jetpacs-shell-push))))
    :doc "Show a different month on the reminders calendar."
    :args '((:name value :type "text" :required t))))

;; Leaving the tab resets the calendar to the current month.
(defun harp-reminders--on-view-switched (view)
  "Reset the shown month when leaving the Reminders tab."
  (unless (equal view "harp.reminders")
    (setq harp-reminders--month nil)))

(add-hook 'jetpacs-shell-view-switched-hook #'harp-reminders--on-view-switched)

;; ─── companion alarms (SPEC §7 reminders) ────────────────────────────────────
;; The current profile's future reminders are armed as exact alarms on the
;; companion, replacing only Harp's set and surviving reboots.  Re-armed
;; whenever the arming set changes (mutations, profile switch) and on connect.

(defvar harp-reminders--last 'unset
  "Last reminder set armed, to avoid re-sending an unchanged set.")

(defun harp-reminders--parse-repeater (stamp)
  "Return (N . UNIT-CHAR) for STAMP's +N[hdwmy] repeater, or nil."
  (when (and (stringp stamp)
             (string-match "[.+]\\{1,2\\}\\([0-9]+\\)\\([hdwmy]\\)" stamp))
    (cons (string-to-number (match-string 1 stamp))
          (aref (match-string 2 stamp) 0))))

(defun harp-reminders--step (time n unit)
  "Advance TIME by N of UNIT (a char h/d/w/m/y), DST-safe.
Steps the wall-clock date/time fields and re-resolves the local offset, so
`+1d' keeps the same clock time across a DST transition rather than
drifting an hour (or, for a date-only stamp, sliding to 23:00 the day before)."
  (let ((d (decode-time time)))
    (pcase unit
      (?h (setf (decoded-time-hour d) (+ (decoded-time-hour d) n)))
      (?d (setf (decoded-time-day d) (+ (decoded-time-day d) n)))
      (?w (setf (decoded-time-day d) (+ (decoded-time-day d) (* 7 n))))
      (?m (setf (decoded-time-month d) (+ (decoded-time-month d) n)))
      (?y (setf (decoded-time-year d) (+ (decoded-time-year d) n)))
      (_ (setf (decoded-time-day d) (+ (decoded-time-day d) n))))
    (harp--encode-local d)))

(defun harp-reminders--seconds-per (n unit)
  "Approximate seconds in N of UNIT — only for a jump estimate, never a stamp."
  (* n (pcase unit (?h 3600) (?d 86400) (?w 604800)
              (?m 2592000) (?y 31536000) (_ 86400))))

(defun harp-reminders--next-time (stamp)
  "Float-time of STAMP's next future occurrence, honoring a repeater; else nil."
  (let ((base (and (stringp stamp) (ignore-errors (org-time-string-to-time stamp))))
        (rep (harp-reminders--parse-repeater stamp))
        (now (current-time)))
    (when base
      (if (time-less-p now base)
          (float-time base)
        (when rep
          ;; Jump most of the gap mathematically first, so a small (e.g. +1h)
          ;; repeater on a stamp years old doesn't exhaust the step cap and
          ;; silently yield no alarm; then step precisely (DST-correct).
          (let* ((per (harp-reminders--seconds-per (car rep) (cdr rep)))
                 (gap (- (float-time now) (float-time base)))
                 (jumps (if (> per 0) (max 0 (1- (floor gap per))) 0))
                 (tt (harp-reminders--step-n base (car rep) (cdr rep) jumps))
                 (i 0))
            (while (and (not (time-less-p now tt)) (< i 4000))
              (setq tt (harp-reminders--step tt (car rep) (cdr rep))
                    i (1+ i)))
            (and (time-less-p now tt) (float-time tt))))))))

(defun harp-reminders--step-n (time n unit jumps)
  "Advance TIME by JUMPS applications of the (N . UNIT) repeater, in one op.
`encode-time' normalises the (possibly huge) field, so this is O(1) — the
mathematical jump that keeps a small repeater on an old stamp from
exhausting the precise-step cap."
  (if (<= jumps 0)
      time
    (let ((d (decode-time time)) (total (* n jumps)))
      (pcase unit
        (?h (setf (decoded-time-hour d) (+ (decoded-time-hour d) total)))
        (?d (setf (decoded-time-day d) (+ (decoded-time-day d) total)))
        (?w (setf (decoded-time-day d) (+ (decoded-time-day d) (* 7 total))))
        (?m (setf (decoded-time-month d) (+ (decoded-time-month d) total)))
        (?y (setf (decoded-time-year d) (+ (decoded-time-year d) total)))
        (_ (setf (decoded-time-day d) (+ (decoded-time-day d) total))))
      (harp--encode-local d))))

(defun harp-reminders--specs (profile)
  "Reminder alarm specs for PROFILE (future occurrences only)."
  (let (specs)
    (dolist (r (alist-get 'reminders profile))
      (let ((time (harp-reminders--next-time
                   (or (alist-get 'scheduled r) (alist-get 'deadline r)))))
        (when time
          (push `((id . ,(format "harp/%s" (alist-get 'id r)))
                  (at_ms . ,(truncate (* time 1000)))
                  (title . ,(alist-get 'title r))
                  (body . ,(harp-reminders--when-label r)))
                specs))))
    (nreverse specs)))

(defun harp-reminders--med-specs (profile)
  "Alarm specs for the next dose of each active medication in PROFILE."
  (let (specs)
    (dolist (m (alist-get 'medications profile))
      (when (harp-med-active-p m)
        (when-let ((d (harp-med-schedule-next-dose m)))
          (push `((id . ,(format "harp/med/%s" (alist-get 'id m)))
                  (at_ms . ,(truncate (* (alist-get 'at d) 1000)))
                  (title . ,(alist-get 'name m))
                  (body . ,(string-trim
                            (concat "Dose " (or (alist-get 'amount d) "")
                                    " " (or (alist-get 'unit d) "")))))
                specs))))
    (nreverse specs)))

(defun harp-reminders-sync ()
  "Arm the current profile's upcoming reminders and medication doses.
Only runs while connected — arming an absent companion is a no-op that
would trip the capability-negotiation warning on every offline push."
  (when jetpacs--session
    (let ((specs (and (harp-ui-current)
                      (append (harp-reminders--specs (harp-ui-current))
                              (harp-reminders--med-specs (harp-ui-current))))))
      (unless (equal specs harp-reminders--last)
        (setq harp-reminders--last specs)
        (jetpacs-reminders-owner-set (vconcat specs) "harp")))))

(defun harp-reminders--on-connect (&rest _)
  "Force a re-arm on a fresh connection."
  (setq harp-reminders--last 'unset)
  (harp-reminders-sync))

;; Ride pushes (memo-guarded by the set comparison) and connects.
(add-hook 'jetpacs-shell-after-push-hook #'harp-reminders-sync)
(add-hook 'jetpacs-connected-hook #'harp-reminders--on-connect)

(provide 'harp-reminders)
;;; harp-reminders.el ends here

;;; ==================================================================
;;; BEGIN apps/harp/harp-form.el
;;; ==================================================================

;;; harp-form.el --- Harp capture and edit forms -*- lexical-binding: t; -*-

;; The write UI: one overlay nav-view that, while a form is open, shows the
;; fields for a new/edited entry, document, metric, or profile, plus the FAB
;; that opens it per tab.  Fields round-trip through `jetpacs-form' (text
;; fields publish to UI state; pickers and the switch write it through
;; `harp.form.set'); Save reads them back and calls `harp-write.el'.  Mirrors
;; harp-kmp's JournalEntryForm / DocumentForm / MetricForm / NewProfileForm.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'jetpacs-widgets)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
(require 'jetpacs-apps)
;; (require 'harp-ui) — bundle-internal, provided above
;; (require 'harp-write) — bundle-internal, provided above
;; (require 'harp-journal) — bundle-internal, provided above

(defvar harp-form--open nil
  "Plist describing the open form: (:kind KIND :ref REF :journal NAME), or nil.")

(defun harp-form--form ()
  "Harp's field registry."
  (jetpacs-form "harpform" "harp"))

(defun harp-form--fid (field)
  "Companion widget id for FIELD."
  (jetpacs-form-field-id (harp-form--form) field))

;; ─── field access ────────────────────────────────────────────────────────────

(defun harp-form--text (field &optional default)
  "Trimmed string value of FIELD, or DEFAULT when blank."
  (let ((v (jetpacs-form-value (harp-form--form) field)))
    (if (and (stringp v) (not (string-empty-p (string-trim v))))
        (string-trim v)
      default)))

(defun harp-form--bool (field)
  "Boolean value of FIELD (JSON false and nil both mean off)."
  (let ((v (jetpacs-form-value (harp-form--form) field)))
    (and v (not (eq v :false)))))

(defun harp-form--seed (field value)
  "Force FIELD's UI-state to VALUE (open-time prefill)."
  (jetpacs-ui-state-put (harp-form--fid field) value))

(defun harp-form--split-tags (s)
  "Split a tags string S (space/comma separated, leading # ok) into names."
  (when (stringp s)
    (cl-remove-if #'string-empty-p
                  (mapcar (lambda (tg) (string-trim (string-remove-prefix "#" tg)))
                          (split-string s "[ ,]+" t)))))

(defun harp-form--range-string (range)
  "Format a (LO . HI) RANGE cons back to \"lo-hi\", or empty."
  (let ((lo (car range)) (hi (cdr range)))
    (if (or lo hi) (format "%s-%s" (or lo "") (or hi "")) "")))

(defun harp-form--stamp-parts (dt)
  "Split Org timestamp DT into a (DATE . TIME) cons of strings."
  (if (and (stringp dt)
           (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\\(?: \\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)\\)?" dt))
      (cons (match-string 1 dt) (match-string 2 dt))
    (cons (format-time-string "%Y-%m-%d") nil)))

;; ─── lookup for edit ─────────────────────────────────────────────────────────

(defun harp-form--find-entry (id)
  "The entry alist with ID in the current profile, or nil."
  (cl-loop for j in (alist-get 'journals (harp-ui-current))
           thereis (cl-find id (alist-get 'entries j)
                            :key (lambda (e) (alist-get 'id e)) :test #'equal)))

(defun harp-form--find-document (id)
  "The document alist with ID in the current profile, or nil."
  (cl-find id (alist-get 'documents (harp-ui-current))
           :key (lambda (d) (alist-get 'id d)) :test #'equal))

(defun harp-form--find-metric (id)
  "The metric alist with tag-id ID in the current profile, or nil."
  (cl-find id (alist-get 'metrics (harp-ui-current))
           :key (lambda (m) (alist-get 'id m)) :test #'equal))

;; ─── open ────────────────────────────────────────────────────────────────────

(defun harp-form-open (kind &optional item)
  "Open the form for KIND, prefilled from ITEM when editing."
  (jetpacs-form-reset (harp-form--form))
  (pcase kind
    ("entry"
     (let ((parts (if item (harp-form--stamp-parts (alist-get 'datetime item))
                    (cons (format-time-string "%Y-%m-%d")
                          (format-time-string "%H:%M")))))
       (harp-form--seed "date" (car parts))
       (harp-form--seed "time" (or (cdr parts) (format-time-string "%H:%M")))
       (harp-form--seed "tags" (string-join (alist-get 'heading-tags item) " "))
       (harp-form--seed "private" (alist-get 'private item))
       (harp-form--seed "body" (or (alist-get 'body item) ""))))
    ("document"
     (let ((parts (if item (harp-form--stamp-parts (alist-get 'datetime item))
                    (cons (format-time-string "%Y-%m-%d")
                          (format-time-string "%H:%M")))))
       (harp-form--seed "name" (or (alist-get 'name item) ""))
       (harp-form--seed "date" (car parts))
       (harp-form--seed "time" (or (cdr parts) (format-time-string "%H:%M")))
       (harp-form--seed "source" (or (alist-get 'source item) ""))
       (harp-form--seed "files" (string-join (alist-get 'files item) ", "))
       (harp-form--seed "tags" (string-join (alist-get 'heading-tags item) " "))
       (harp-form--seed "private" (alist-get 'private item))
       (harp-form--seed "annotation" (or (alist-get 'annotation item) ""))))
    ("metric"
     (harp-form--seed "name" (or (alist-get 'name item) ""))
     (harp-form--seed "tagid" (or (alist-get 'id item) ""))
     (harp-form--seed "unit" (or (alist-get 'unit item) ""))
     (harp-form--seed "range" (harp-form--range-string (alist-get 'range item)))
     (harp-form--seed "healthy" (harp-form--range-string (alist-get 'healthy-range item)))
     (harp-form--seed "tags" (string-join (alist-get 'tags item) " ")))
    ("profile"
     (harp-form--seed "pname" "")
     (harp-form--seed "ptype" (vector "human"))))
  (setq harp-form--open
        (list :kind kind :ref (alist-get 'ref item)
              :journal (and (harp-ui-current) (harp-journal--current-name))))
  ;; Opening the form IS a navigation: force the companion onto the overlay,
  ;; not just update the surface it is already showing.
  (jetpacs-shell-push nil :switch-to "harp.form"))

(defun harp-form--close ()
  "Close the form and navigate the companion back to the current tab."
  (setq harp-form--open nil)
  (jetpacs-shell-push nil :switch-to (jetpacs-shell-current-tab)))

;; ─── field widgets ───────────────────────────────────────────────────────────

(defun harp-form--date-row ()
  "A date + time picker row, current values baked into the labels."
  (let ((date (harp-form--text "date" (format-time-string "%Y-%m-%d")))
        (time (harp-form--text "time")))
    (jetpacs-row
     (jetpacs-box
      (list (jetpacs-date-button
             (format "Date: %s" date)
             (jetpacs-action "harp.form.set" :args '((field . "date")) :when-offline "drop")
             :value date))
      :weight 1)
     (jetpacs-box
      (list (jetpacs-time-button
             (format "Time: %s" (or time "—"))
             (jetpacs-action "harp.form.set" :args '((field . "time")) :when-offline "drop")
             :value time))
      :weight 1)
     :spacing 8)))

(defun harp-form--private-switch ()
  "A Private toggle wired to `harp.form.set'."
  (jetpacs-switch (harp-form--fid "private")
                  :label "Private"
                  :checked (harp-form--bool "private")
                  :on-change (jetpacs-action "harp.form.set"
                                             :args '((field . "private"))
                                             :when-offline "drop")))

(defun harp-form--field-nodes (kind)
  "The list of field widgets for KIND, values read from UI state."
  (pcase kind
    ("entry"
     (list (jetpacs-text (format "Journal: %s" (plist-get harp-form--open :journal))
                         'caption)
           (harp-form--date-row)
           (jetpacs-text-input (harp-form--fid "tags")
                               :label "Tags" :hint "space separated"
                               :value (harp-form--text "tags"))
           (harp-form--private-switch)
           (jetpacs-text-input (harp-form--fid "body")
                               :label "Entry" :multi-line t :min-lines 4
                               :value (harp-form--text "body"))))
    ("document"
     (list (jetpacs-text-input (harp-form--fid "name")
                               :label "Title" :value (harp-form--text "name"))
           (harp-form--date-row)
           (jetpacs-text-input (harp-form--fid "source")
                               :label "Source" :value (harp-form--text "source"))
           (jetpacs-text-input (harp-form--fid "files")
                               :label "Files" :hint "comma separated"
                               :value (harp-form--text "files"))
           (jetpacs-text-input (harp-form--fid "tags")
                               :label "Tags" :hint "space separated"
                               :value (harp-form--text "tags"))
           (harp-form--private-switch)
           (jetpacs-text-input (harp-form--fid "annotation")
                               :label "Annotation" :multi-line t :min-lines 3
                               :value (harp-form--text "annotation"))))
    ("metric"
     (list (jetpacs-text-input (harp-form--fid "name")
                               :label "Name" :value (harp-form--text "name"))
           (jetpacs-text-input (harp-form--fid "tagid")
                               :label "Tag id" :hint "used in #id(value)"
                               :value (harp-form--text "tagid"))
           (jetpacs-text-input (harp-form--fid "unit")
                               :label "Unit" :value (harp-form--text "unit"))
           (jetpacs-text-input (harp-form--fid "range")
                               :label "Range" :hint "lo-hi"
                               :value (harp-form--text "range"))
           (jetpacs-text-input (harp-form--fid "healthy")
                               :label "Healthy range" :hint "lo-hi"
                               :value (harp-form--text "healthy"))
           (jetpacs-text-input (harp-form--fid "tags")
                               :label "Tags" :hint "space separated"
                               :value (harp-form--text "tags"))))
    ("profile"
     (list (jetpacs-text-input (harp-form--fid "pname")
                               :label "Name" :value (harp-form--text "pname"))
           (jetpacs-enum-list (harp-form--fid "ptype")
                              '("human" "cat" "dog")
                              :value (jetpacs-ui-state (harp-form--fid "ptype"))
                              :on-change (jetpacs-action "harp.form.set"
                                                         :args '((field . "ptype"))
                                                         :when-offline "drop"))))))

(defun harp-form--title (kind ref)
  "The nav-view title for KIND (edit when REF is non-nil)."
  (concat (if ref "Edit " "New ")
          (pcase kind ("entry" "entry") ("document" "document")
                 ("metric" "metric") ("profile" "profile"))))

(defun harp-form--view (snackbar)
  "The overlay form screen."
  (let* ((kind (plist-get harp-form--open :kind))
         (ref (plist-get harp-form--open :ref)))
    (jetpacs-shell-nav-view
     (harp-form--title kind ref)
     (apply #'jetpacs-lazy-column
            (append
             (harp-form--field-nodes kind)
             (list (jetpacs-button "Save"
                                   ;; Dedupe on the form generation: a queued
                                   ;; double-tap of Save replays once, and the
                                   ;; next form-open rotates the key.
                                   (jetpacs-action "harp.form.save"
                                                   :when-offline "queue"
                                                   :dedupe (harp-form--fid "save"))
                                   :variant "filled"))))
     :nav-action (jetpacs-action "harp.form.cancel" :when-offline "drop")
     :snackbar snackbar)))

;; ─── save dispatch ───────────────────────────────────────────────────────────

(defun harp-form--save ()
  "Persist the open form.  Return non-nil on success (the form should close)."
  (let ((kind (plist-get harp-form--open :kind))
        (ref (plist-get harp-form--open :ref)))
    (pcase kind
      ("entry"
       (let ((body (harp-form--text "body")))
         (if (null body)
             (progn (jetpacs-shell-notify "Entry text is required") nil)
           (let ((date (harp-form--text "date" (format-time-string "%Y-%m-%d")))
                 (time (harp-form--text "time"))
                 (tags (harp-form--split-tags (harp-form--text "tags")))
                 (private (harp-form--bool "private")))
             (if ref
                 (harp-write-update-entry ref date time tags private body)
               (harp-write-add-entry (plist-get harp-form--open :journal)
                                     date time tags private body))
             (jetpacs-shell-notify (if ref "Entry updated" "Entry added"))
             t))))
      ("document"
       (let ((name (harp-form--text "name")))
         (if (null name)
             (progn (jetpacs-shell-notify "Title is required") nil)
           (let ((date (harp-form--text "date" (format-time-string "%Y-%m-%d")))
                 (time (harp-form--text "time"))
                 (source (harp-form--text "source"))
                 (files (harp-form--split-files (harp-form--text "files")))
                 (tags (harp-form--split-tags (harp-form--text "tags")))
                 (private (harp-form--bool "private"))
                 (annotation (harp-form--text "annotation")))
             (if ref
                 (harp-write-update-document ref name date time source files tags private annotation)
               (harp-write-add-document name date time source files tags private annotation))
             (jetpacs-shell-notify (if ref "Document updated" "Document added"))
             t))))
      ("metric"
       (let ((name (harp-form--text "name"))
             (tagid (harp-form--text "tagid"))
             (unit (harp-form--text "unit")))
         (if (not (and name tagid unit))
             (progn (jetpacs-shell-notify "Name, tag id and unit are required") nil)
           (let ((range (harp-form--text "range" ""))
                 (healthy (harp-form--text "healthy" ""))
                 (tags (harp-form--split-tags (harp-form--text "tags"))))
             (if ref
                 (harp-write-update-metric ref name tagid unit range healthy tags)
               (harp-write-add-metric name tagid unit range healthy tags))
             (jetpacs-shell-notify (if ref "Metric updated" "Metric added"))
             t))))
      ("profile"
       (let ((name (harp-form--text "pname")))
         (if (null name)
             (progn (jetpacs-shell-notify "Name is required") nil)
           (let* ((type (or (car (jetpacs-ui-state-list (harp-form--fid "ptype"))) "human"))
                  (file (harp-write-new-profile name type)))
             (harp-ui-set-current file)
             (jetpacs-shell-notify (format "Created %s" name))
             ;; Land in the new profile.
             (setq harp-form--open nil)
             (jetpacs-shell-set-current-tab "harp.journal")
             'handled)))))))

;; `harp--split-files' lives in harp-profile; alias for readability here.
(defalias 'harp-form--split-files #'harp--split-files)

;; ─── FAB (per tab) + registration ────────────────────────────────────────────

(defun harp-form--fab (name)
  "The context FAB for tab view NAME, or nil."
  (pcase name
    ("harp.home"
     (jetpacs-fab "person_add" :label "New profile"
                  :on-tap (jetpacs-action "harp.form.open" :args '((kind . "profile"))
                                          :when-offline "drop")))
    ("harp.journal"
     (when (harp-ui-current)
       (jetpacs-fab "add" :label "Entry"
                    :on-tap (jetpacs-action "harp.form.open" :args '((kind . "entry"))
                                            :when-offline "drop"))))
    ("harp.documents"
     (when (harp-ui-current)
       (jetpacs-fab "add" :label "Document"
                    :on-tap (jetpacs-action "harp.form.open" :args '((kind . "document"))
                                            :when-offline "drop"))))
    ("harp.metrics"
     (when (harp-ui-current)
       (jetpacs-fab "add" :label "Metric"
                    :on-tap (jetpacs-action "harp.form.open" :args '((kind . "metric"))
                                            :when-offline "drop"))))
    ("harp.meds"
     (when (harp-ui-current)
       (jetpacs-fab "add" :label "Medication"
                    :on-tap (jetpacs-action "harp.med.form.open" :when-offline "drop"))))))

(with-jetpacs-owner "harp"

  (jetpacs-apps-set-default-fab "harp" #'harp-form--fab)

  (jetpacs-shell-define-view "harp.form"
    :builder #'harp-form--view
    :overlay (lambda () harp-form--open)
    :order 90)

  (jetpacs-defaction "harp.form.open"
    (lambda (args _)
      (let* ((kind (alist-get 'kind args))
             (id (alist-get 'id args))
             (item (and id (pcase kind
                             ("entry" (harp-form--find-entry id))
                             ("document" (harp-form--find-document id))
                             ("metric" (harp-form--find-metric id))))))
        (when (member kind '("entry" "document" "metric" "profile"))
          (harp-form-open kind item))))
    :doc "Open the capture/edit form for a kind (optionally an item id to edit)."
    :args '((:name kind :type "text" :required t)
            (:name id :type "text")))

  (jetpacs-defaction "harp.form.cancel"
    (lambda (_args _)
      (harp-form--close))
    :doc "Close the capture/edit form without saving.")

  (jetpacs-defaction "harp.form.set"
    (lambda (args _)
      (let ((field (alist-get 'field args))
            (value (alist-get 'value args)))
        (when (stringp field)
          (jetpacs-ui-state-put
           (harp-form--fid field)
           (pcase field
             ("private" (and value (not (eq value :false))))
             (_ value)))
          (jetpacs-shell-push))))
    :doc "Update one form field (picker/switch/enum) from the wire."
    :args '((:name field :type "text" :required t)
            (:name value :type "text")))

  (jetpacs-defaction "harp.form.save"
    (lambda (_args _)
      (if (null harp-form--open)
          ;; A queued Save whose form-open state didn't survive (an Emacs
          ;; restart between tap and reconnect) — say so rather than swallow it.
          (progn
            (jetpacs-shell-notify "Saved form expired — reopen and re-enter")
            (jetpacs-shell-push))
        (let ((result (harp-form--save)))
          (cond
           ;; The profile path already navigated (set-current-tab pushed).
           ((eq result 'handled) nil)
           ;; A successful save closes and returns to the tab.
           (result (harp-form--close))
           ;; A validation failure re-renders the form in place.
           (t (jetpacs-shell-push))))))
    :doc "Persist the open capture/edit form."))

(provide 'harp-form)
;;; harp-form.el ends here

;;; ==================================================================
;;; BEGIN apps/harp/harp-export.el
;;; ==================================================================

;;; harp-export.el --- Harp profile export & share -*- lexical-binding: t; -*-

;; Export a profile for sharing with a care provider: as Org text (shared
;; straight over the bridge via ACTION_SEND — host-agnostic, no file
;; transfer) or as a self-contained HTML timeline written to
;; `harp-export-directory'.  Ports harp-kmp's Export.kt `exportText` /
;; `exportHTML`; media embedding (the zip) is deferred with attachments.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'jetpacs-widgets)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
(require 'jetpacs-device)
;; (require 'harp-ui) — bundle-internal, provided above
;; (require 'harp-write) — bundle-internal, provided above

(defcustom harp-export-directory nil
  "Directory HTML exports are written to.
nil means an `exports/' subdirectory of `harp-directory'."
  :type '(choice (const :tag "exports/ under harp-directory" nil) directory)
  :group 'harp)

(defun harp-export--dir ()
  "The resolved export directory."
  (or harp-export-directory (expand-file-name "exports/" harp-directory)))

;; ─── privacy filter ──────────────────────────────────────────────────────────

(defun harp-export--filter-profile (profile include-private)
  "PROFILE with `:PRIVATE: t' entries and documents removed.
INCLUDE-PRIVATE non-nil returns PROFILE unchanged.  An export exists to be
handed to a care provider — private items stay home unless asked for."
  (if include-private
      profile
    (let ((copy (copy-alist profile)))
      (setf (alist-get 'journals copy)
            (mapcar (lambda (j)
                      (let ((jc (copy-alist j)))
                        (setf (alist-get 'entries jc)
                              (cl-remove-if (lambda (e) (alist-get 'private e))
                                            (alist-get 'entries j)))
                        jc))
                    (alist-get 'journals profile)))
      (setf (alist-get 'documents copy)
            (cl-remove-if (lambda (d) (alist-get 'private d))
                          (alist-get 'documents profile)))
      copy)))

;; ─── Org text export (mirrors harp-kmp exportText) ──────────────────────────

(defun harp-export--entry-text (e)
  "Org text for entry E, reusing the write layer's subtree builder."
  (harp-write--entry-text (alist-get 'id e) (alist-get 'datetime e)
                          (alist-get 'heading-tags e) (alist-get 'private e)
                          (alist-get 'body e)))

(defun harp-export--journal-text (j)
  "Org text for journal J and its entries."
  (concat (format "** %s\n" (alist-get 'name j))
          (mapconcat #'harp-export--entry-text (alist-get 'entries j) "\n")))

(defun harp-export--document-text (d)
  "Org text for document D, reusing the write layer's subtree builder."
  (harp-write--document-text (alist-get 'id d) (alist-get 'name d)
                             (alist-get 'datetime d) (alist-get 'source d)
                             (alist-get 'files d) (alist-get 'heading-tags d)
                             (alist-get 'private d) (alist-get 'annotation d)))

(defun harp-export-text (profile &optional description include-private)
  "Build a shareable Org document for PROFILE (journals + documents).
Private items are excluded unless INCLUDE-PRIVATE."
  (setq profile (harp-export--filter-profile profile include-private))
  (let ((journals (cl-remove-if (lambda (j) (null (alist-get 'entries j)))
                                (alist-get 'journals profile))))
    (concat
     ":PROPERTIES:\n"
     (format ":ID: %s\n" (alist-get 'uuid profile))
     (format ":HARP_TYPE: %s\n" (alist-get 'type profile))
     ":END:\n"
     (format "#+TITLE: %s\n" (alist-get 'name profile))
     (if (and description (not (string-empty-p description)))
         (format "#+DESCRIPTION: %s\n" description) "")
     "\n* Journals\n"
     (mapconcat #'harp-export--journal-text journals "\n")
     "\n\n* Documents\n"
     (mapconcat #'harp-export--document-text (alist-get 'documents profile) "\n")
     "\n")))

;; ─── HTML export (mirrors harp-kmp exportHTML, no embedded media) ────────────

(defun harp-export--escape (s)
  "HTML-escape S."
  (if (stringp s)
      (replace-regexp-in-string
       ">" "&gt;"
       (replace-regexp-in-string
        "<" "&lt;"
        (replace-regexp-in-string "&" "&amp;" s)))
    ""))

(defconst harp-export--css "\
*{box-sizing:border-box}
body{margin:0;padding:1.25rem;font-family:system-ui,-apple-system,\"Segoe UI\",sans-serif;line-height:1.6;color:#111;background:#fff}
main{max-width:42rem;margin:auto}
article{margin-bottom:2rem;padding:1.25rem 1.5rem;border-radius:12px;background:#fafafa}
h1{font-size:1.8rem;margin-bottom:1rem}
h3{margin:0 0 .25rem;font-size:1.1rem}
time{display:block;font-size:.85rem;color:#666;margin-bottom:.75rem}
.content{white-space:pre-wrap}
.files{margin-top:.75rem;font-size:.85rem;color:#555}
@media (max-width:480px){body{padding:1rem}h1{font-size:1.5rem}}")

(defun harp-export--timeline (profile)
  "Entries and documents of PROFILE as timeline items, newest first."
  (let (items)
    (dolist (j (alist-get 'journals profile))
      (dolist (e (alist-get 'entries j))
        (push (cons 'entry e) items)))
    (dolist (d (alist-get 'documents profile))
      (push (cons 'document d) items))
    (sort items (lambda (a b) (> (or (alist-get 'time (cdr a)) 0)
                                 (or (alist-get 'time (cdr b)) 0))))))

(defun harp-export--article (item)
  "One HTML <article> for timeline ITEM (a (KIND . ALIST) cons)."
  (let* ((kind (car item)) (x (cdr item)))
    (concat
     "<article>\n"
     (if (eq kind 'document)
         (format "<h3>%s</h3>\n" (harp-export--escape (alist-get 'name x)))
       "")
     (format "<time>%s</time>\n"
             (harp-export--escape (harp-ui-format-stamp (alist-get 'datetime x) t)))
     (format "<div class=\"content\">%s</div>\n"
             (harp-export--escape (or (alist-get (if (eq kind 'document) 'annotation 'body) x) "")))
     (let ((files (alist-get 'assets x)))
       (if files
           (format "<div class=\"files\">Attachments: %s</div>\n"
                   (harp-export--escape (string-join files ", ")))
         ""))
     "</article>")))

(defun harp-export-html (profile &optional description include-private)
  "Build a self-contained HTML timeline for PROFILE.
Private items are excluded unless INCLUDE-PRIVATE."
  (setq profile (harp-export--filter-profile profile include-private))
  (concat
   "<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n"
   "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
   (format "<title>%s | Harp Export</title>\n" (harp-export--escape (alist-get 'name profile)))
   "<style>" harp-export--css "</style>\n</head>\n<body>\n<main>\n"
   (format "<h1>%s</h1>\n" (harp-export--escape (alist-get 'name profile)))
   (if (and description (not (string-empty-p description)))
       (format "<p>%s</p>\n" (harp-export--escape description)) "")
   (format "<h4>Exported via Harp on %s</h4>\n"
           (format-time-string "%b %-d, %Y %H:%M"))
   (mapconcat #'harp-export--article (harp-export--timeline profile) "\n")
   "\n</main>\n</body>\n</html>\n"))

(defun harp-export--timestamped-file (profile ext)
  "A fresh export path for PROFILE with extension EXT under `harp-export--dir'."
  (let ((dir (harp-export--dir)))
    (make-directory dir t)
    (expand-file-name
     (format "%s-%s.%s"
             (harp-write--profile-slug (alist-get 'name profile))
             (format-time-string "%Y%m%d-%H%M%S")
             ext)
     dir)))

(defun harp-export-html-to-file (profile &optional include-private)
  "Write PROFILE's HTML export under `harp-export--dir'; return the path."
  (let ((file (harp-export--timestamped-file profile "html")))
    (with-temp-file file
      (insert (harp-export-html profile nil include-private)))
    file))

;; ─── share / dialog / chrome ─────────────────────────────────────────────────

(defcustom harp-export-share-max-bytes 200000
  "Largest Org export shared inline via ACTION_SEND EXTRA_TEXT.
Intent extras ride a bounded binder transaction (~1MB hard, less in
practice); over this cap the export is written to `harp-export-directory'
instead of firing the intent."
  :type 'integer :group 'harp)

(defun harp-export--share-text (text subject profile)
  "Share TEXT via ACTION_SEND with SUBJECT — host-agnostic.
Over `harp-export-share-max-bytes' the intent would be dropped by the
binder, so the text is saved under `harp-export--dir' instead.  Returns
the symbol `shared', or the file path when it fell back to a file."
  (if (> (string-bytes text) harp-export-share-max-bytes)
      (let ((file (harp-export--timestamped-file profile "org")))
        (with-temp-file file (insert text))
        file)
    (jetpacs-device-intent
     :action "android.intent.action.SEND"
     :mime "text/plain"
     :extras `(("android.intent.extra.TEXT" . ,text)
               ("android.intent.extra.SUBJECT" . ,subject)))
    'shared))

(defconst harp-export--private-field "harp-export-private"
  "UI-state id backing the export dialog's Include-private switch.")

(defun harp-export--private-p ()
  "Whether the export dialog's Include-private switch is on.
Read from UI state, not an on_change action: the reference companion emits
only `state.changed' for a switch (no action dispatch), so the switch's
action would never fire — but the value is faithfully tracked in UI state."
  (let ((v (jetpacs-ui-state harp-export--private-field)))
    (and v (not (eq v :false)))))

(defun harp-export--dialog-body ()
  "The export format chooser's spec column."
  (jetpacs-column
   (jetpacs-text "Export" 'title)
   (jetpacs-text (or (alist-get 'name (harp-ui-current)) "") 'caption)
   (jetpacs-switch harp-export--private-field
                   :label "Include private items"
                   :checked (harp-export--private-p))
   (jetpacs-button "Share as Org text"
                   (jetpacs-action "harp.export.run" :args '((format . "org"))
                                   :when-offline "drop")
                   :variant "outlined")
   (jetpacs-button "Save as HTML"
                   (jetpacs-action "harp.export.run" :args '((format . "html"))
                                   :when-offline "drop")
                   :variant "outlined")
   (jetpacs-button "Cancel"
                   (jetpacs-action "harp.export.cancel" :when-offline "drop")
                   :variant "text")))

(defun harp-export--dialog ()
  "Show the export format chooser as a bottom sheet."
  (jetpacs-send-dialog (harp-export--dialog-body) "sheet"))

(with-jetpacs-owner "harp"

  (jetpacs-shell-add-top-action
   20
   (lambda ()
     (when (harp-ui-current)
       (jetpacs-icon-button "ios_share"
                            (jetpacs-action "harp.export.open" :when-offline "drop")
                            :content-description "Export"))))

  (jetpacs-defaction "harp.export.open"
    (lambda (_args _)
      (when (harp-ui-current)
        ;; Every export starts private-safe; including private items is a
        ;; per-export opt-in, never a sticky preference — clear the switch's
        ;; UI state so a prior export can't silently re-enable it.
        (jetpacs-ui-state-put harp-export--private-field nil)
        (harp-export--dialog)))
    :doc "Open the export format chooser.")

  (jetpacs-defaction "harp.export.cancel"
    (lambda (_args _) nil)
    :doc "Dismiss the export chooser.")

  (jetpacs-defaction "harp.export.run"
    (lambda (args _)
      (let ((profile (harp-ui-current))
            (format (alist-get 'format args))
            (private (harp-export--private-p)))
        (when profile
          (pcase format
            ("org"
             (let ((result (harp-export--share-text
                            (harp-export-text profile nil private)
                            (format "%s — Harp export" (alist-get 'name profile))
                            profile)))
               (jetpacs-shell-notify
                (if (eq result 'shared)
                    "Shared as Org text"
                  (format "Too large to share — saved to %s"
                          (abbreviate-file-name result))))))
            ("html"
             (let ((file (harp-export-html-to-file profile private)))
               (jetpacs-shell-notify
                (format "HTML saved to %s" (abbreviate-file-name file))))))
          ;; notify only queues the snackbar — push so the result actually
          ;; shows (the export sheet dispatches no re-render on its own).
          (jetpacs-shell-push))))
    :doc "Export the current profile in the given format (org shares, html saves)."
    :args '((:name format :type "text" :required t))))

(provide 'harp-export)
;;; harp-export.el ends here

;;; ==================================================================
;;; BEGIN apps/harp/harp-demo.el
;;; ==================================================================

;;; harp-demo.el --- Bundled sample profiles for trying Harp out -*- lexical-binding: t; -*-

;; GENERATED FILE -- do not edit by hand.  Produced by emacs/build-demo.el from
;; the test/fixtures/*.org samples.
;;
;; `harp-demo-setup' writes a few ready-made profiles (a person, a pair of cats,
;; a dog) into `harp-directory' and opens the first, so a fresh install has
;; something to explore.  The human profile carries a couple of medications.
;; Attachment media are not bundled, so attachment chips show file names only.

;;; Code:

(require 'cl-lib)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
;; (require 'harp-profile) — bundle-internal, provided above
;; (require 'harp-ui) — bundle-internal, provided above

(defconst harp-demo--profiles
  (list
   (cons "diego-recovery"
":PROPERTIES:
:ID: b8f2c0ad-6a2c-4f97-9ad0-0d9222220002
:HARP_NAME: Diego Alvarez
:HARP_TYPE: human
:END:
#+TITLE: Diego Recovery Notes

* Metadata
** Sources

** Metrics
*** Heart Rate    :cardio:
:PROPERTIES:
:TAG_ID: hr
:UNIT: bpm
:RANGE: 35-220
:HEALTHY_RANGE: 55-95
:END:

*** Systolic BP    :cardio:
:PROPERTIES:
:TAG_ID: sbp
:UNIT: mmHg
:RANGE: 70-220
:HEALTHY_RANGE: 95-125
:END:

*** Pain Scale    :rehab:
:PROPERTIES:
:TAG_ID: pain
:UNIT: score
:RANGE: 0-10
:HEALTHY_RANGE: 0-2
:END:

* Journals
** Rehab
*** Entry    :physio:ATTACH:
:PROPERTIES:
:ID: 44f11210-a050-4d1b-8aa1-900200000001
:DATETIME: <2026-01-03 08:20>
:PRIVATE: nil
:END:

Knee mobility session #pain(4) #hr(102)
Video for gait review [[attachment:gait-session.mp4]]

*** Entry
:PROPERTIES:
:ID: 44f11210-a050-4d1b-8aa1-900200000002
:DATETIME: <2026-01-10 08:10>
:PRIVATE: nil
:END:

Lower pain with better balance #pain(2) #sbp(122)

** Travel
*** Entry    :ATTACH:
:PROPERTIES:
:ID: 44f11210-a050-4d1b-8aa1-900200000003
:DATETIME: <2026-01-11 18:40>
:PRIVATE: nil
:END:

Carried meds and support brace [[attachment:trip-kit.jpg]] #routine

* Documents
** MRI Report    :ATTACH:imaging:
:PROPERTIES:
:SOURCE: Centro Medico Norte
:FILES: knee-mri-summary.pdf, knee-mri-slice-1.png
:ID: 55cc914a-4889-4f62-98c4-900200000001
:DATETIME: <2025-12-29 11:15>
:PRIVATE: nil
:END:

Consultant notes meniscus inflammation.

** Insurance Approval    :ATTACH:admin:
:PROPERTIES:
:SOURCE: Seguro Vida
:FILES: approval-letter.pdf
:ID: 55cc914a-4889-4f62-98c4-900200000002
:DATETIME: <2026-01-04 12:00>
:PRIVATE: nil
:END:

Approved 8 physio sessions.

* Reminders
** TODO Stretch Session    :rehab:
SCHEDULED: <2026-02-02 +2d>
:PROPERTIES:
:ID: 66fbe8a2-5ec3-4ff8-b990-900200000001
:END:
Maintain quadriceps and hamstrings routine.

** TODO Renew Brace    :admin:
DEADLINE: <2026-04-15>
:PROPERTIES:
:ID: 66fbe8a2-5ec3-4ff8-b990-900200000002
:END:
Order replacement knee brace before travel.

* Medications
** Ibuprofen    :pain:
:PROPERTIES:
:ID: 77a1b2c3-0000-4000-8000-900200000001
:HARP_MED: t
:UNIT: tab
:STOCK: 24
:LOW_STOCK: 6
:ACTIVE: t
:END:
Take with food for knee pain.
*** Schedule
:PROPERTIES:
:AMOUNT: 1
:TIMES: 08:00, 20:00
:END:

** Vitamin D    :supplement:
:PROPERTIES:
:ID: 77a1b2c3-0000-4000-8000-900200000002
:HARP_MED: t
:UNIT: tab
:STOCK: 90
:ACTIVE: t
:END:
*** Schedule
:PROPERTIES:
:AMOUNT: 1
:TIMES: 09:00
:DAYS: Mon Wed Fri
:END:
")
   (cons "kenji-and-luna"
":PROPERTIES:
:ID: e8f2c0ad-6a2c-4f97-9ad0-0d9555550005
:HARP_NAME: Kenji and Luna
:HARP_TYPE: cat
:END:
#+TITLE: Kenji and Luna Household Record

* Metadata
** Sources

** Metrics
*** Weight    :household:
:PROPERTIES:
:TAG_ID: weight
:UNIT: kg
:RANGE: 1-10
:HEALTHY_RANGE: 2.8-5.8
:END:

*** Water Intake    :hydration:
:PROPERTIES:
:TAG_ID: water
:UNIT: ml
:RANGE: 0-500
:HEALTHY_RANGE: 140-320
:END:

*** Activity Minutes    :play:
:PROPERTIES:
:TAG_ID: active
:UNIT: min
:RANGE: 0-240
:HEALTHY_RANGE: 20-90
:END:

* Journals
** Shared Routine
*** Entry    :ATTACH:
:PROPERTIES:
:ID: ddd8aa8a-9d9a-4d22-a3b8-900500000001
:DATETIME: <2026-01-02 08:15>
:PRIVATE: nil
:END:

Both cats used new fountain #water(280) #active(35)
Morning photo [[attachment:fountain-morning.jpg]]

*** Entry
:PROPERTIES:
:ID: ddd8aa8a-9d9a-4d22-a3b8-900500000002
:DATETIME: <2026-01-06 21:00>
:PRIVATE: nil
:END:

Evening play with feather toy #active(42)

*** Entry    :ATTACH:
:PROPERTIES:
:ID: ddd8aa8a-9d9a-4d22-a3b8-900500000003
:DATETIME: <2026-01-09 09:40>
:PRIVATE: nil
:END:

Weight check after grooming #weight(4.9)
Scale snapshot [[attachment:scale-reading.png]]

* Documents
** Dental Cleaning Advice    :ATTACH:dental:
:PROPERTIES:
:SOURCE: Midtown Animal Care
:FILES: dental-note.pdf, tooth-chart.jpg
:ID: eee1e15a-4ccc-4e23-9c00-900500000001
:DATETIME: <2026-01-09 12:30>
:PRIVATE: nil
:END:

Recommend brushing 3 times a week.

** Food Rotation Plan    :ATTACH:
:PROPERTIES:
:SOURCE: Midtown Animal Care
:FILES: food-rotation-plan.pdf
:ID: eee1e15a-4ccc-4e23-9c00-900500000002
:DATETIME: <2026-01-10 10:00>
:PRIVATE: nil
:END:

Switch proteins every two weeks.

* Reminders
** TODO Replace Water Filter    :hydration:
SCHEDULED: <2026-02-01 +1m>
:PROPERTIES:
:ID: fff5b1e5-3b6f-4d7e-8461-900500000001
:END:
Replace fountain filter and sanitize tank.

** TODO Dental Brushing Cycle    :dental:
SCHEDULED: <2026-01-20 +1w>
:PROPERTIES:
:ID: fff5b1e5-3b6f-4d7e-8461-900500000002
:END:
Track compliance for both cats.
")
   (cons "bruno-health"
":PROPERTIES:
:ID: d8f2c0ad-6a2c-4f97-9ad0-0d9444440004
:HARP_NAME: Bruno Oliveira
:HARP_TYPE: dog
:END:
#+TITLE: Bruno Dog Health Record

* Metadata
** Sources

** Metrics
*** Weight    :canine:
:PROPERTIES:
:TAG_ID: weight
:UNIT: kg
:RANGE: 2-60
:HEALTHY_RANGE: 18-32
:END:

*** Resting Pulse    :cardio:
:PROPERTIES:
:TAG_ID: pulse
:UNIT: bpm
:RANGE: 40-220
:HEALTHY_RANGE: 60-120
:END:

*** Scratch Severity    :skin:
:PROPERTIES:
:TAG_ID: itch
:UNIT: score
:RANGE: 0-10
:HEALTHY_RANGE: 0-3
:END:

* Journals
** Walks
*** Entry    :ATTACH:
:PROPERTIES:
:ID: aaf9ee2a-9ed8-4f3b-8db9-900400000001
:DATETIME: <2026-01-04 06:55>
:PRIVATE: nil
:END:

Energetic walk with no limping #weight(24.5) #pulse(88)
Trail photo [[attachment:park-run.jpg]]

*** Entry
:PROPERTIES:
:ID: aaf9ee2a-9ed8-4f3b-8db9-900400000002
:DATETIME: <2026-01-07 19:35>
:PRIVATE: nil
:END:

Mild itching today #itch(4)

** Treatment
*** Entry    :ATTACH:
:PROPERTIES:
:ID: aaf9ee2a-9ed8-4f3b-8db9-900400000003
:DATETIME: <2026-01-08 20:10>
:PRIVATE: nil
:END:

Topical ointment applied [[attachment:ointment-box.png]] #itch(2)

* Documents
** Skin Panel Results    :ATTACH:lab:
:PROPERTIES:
:SOURCE: Rio Pet Diagnostics
:FILES: skin-panel.pdf, allergen-chart.png
:ID: bb2243e1-24d2-4f57-a7ad-900400000001
:DATETIME: <2026-01-08 12:45>
:PRIVATE: nil
:END:

No critical allergy markers.

** Vaccination Passport    :ATTACH:
:PROPERTIES:
:SOURCE: Rio Pet Clinic
:FILES: passport-page-1.jpg, passport-page-2.jpg
:ID: bb2243e1-24d2-4f57-a7ad-900400000002
:DATETIME: <2026-01-01 10:15>
:PRIVATE: nil
:END:

Rabies and DHPP current.

* Reminders
** TODO Tick Prevention    :preventive:
SCHEDULED: <2026-02-05 +1m>
:PROPERTIES:
:ID: cc9c9f70-6f4f-4dc7-8ac4-900400000001
:END:
Apply spot-on treatment.

** TODO Grooming Session    :grooming:
DEADLINE: <2026-02-20>
:PROPERTIES:
:ID: cc9c9f70-6f4f-4dc7-8ac4-900400000002
:END:
Book grooming with regular partner.
")
   )
  "Sample profiles as (SLUG . ORG-TEXT), written by `harp-demo-setup'.")

(defun harp-demo--file (slug)
  (expand-file-name (concat slug ".org") harp-directory))

(defun harp-demo--exists-p ()
  "Non-nil when any bundled sample profile is already on disk."
  (cl-some (lambda (p) (file-exists-p (harp-demo--file (car p)))) harp-demo--profiles))

(defun harp-demo-setup (&optional force)
  "Write the bundled sample profiles into `harp-directory'; open the first.
Existing files are left alone unless FORCE (or a prefix arg) is non-nil, so a
re-run refreshes the samples to the latest bundled version.  Returns the
opened profile file."
  (interactive "P")
  (make-directory harp-directory t)
  (let (first)
    (dolist (p harp-demo--profiles)
      (let ((file (harp-demo--file (car p))))
        (when (or force (not (file-exists-p file)))
          (with-temp-file file (insert (cdr p)))
          ;; `with-temp-file' writes disk directly; drop any stale buffer
          ;; still visiting this path so the next read reflects the refresh.
          (when-let ((buf (find-buffer-visiting file)))
            (with-current-buffer buf (set-buffer-modified-p nil))
            (kill-buffer buf)))
        (unless first (setq first file))))
    (harp-ui-invalidate)
    (when first (harp-ui-set-current first))
    first))

(with-jetpacs-owner "harp"
  (jetpacs-defaction "harp.demo.setup"
    (lambda (_args _)
      ;; When samples already exist, offer to refresh them to the latest
      ;; bundled version (bridged to a dialog on the phone).  Declining must
      ;; leave the current profile alone — never switch or claim a write.
      (cond
       ((not (harp-demo--exists-p))
        (harp-demo-setup)
        (jetpacs-shell-notify "Sample profiles added")
        (jetpacs-shell-set-current-tab "harp.journal"))
       ((yes-or-no-p "Sample profiles already exist — refresh to the latest samples? ")
        (harp-demo-setup t)
        (jetpacs-shell-notify "Sample profiles refreshed")
        (jetpacs-shell-set-current-tab "harp.journal"))
       (t
        (jetpacs-shell-notify "Kept your existing profiles")
        (jetpacs-shell-push))))
    :doc "Write (or refresh) the bundled sample profiles and open the first."))

(provide 'harp-demo)
;;; harp-demo.el ends here

;;; ==================================================================
;;; BEGIN apps/harp/harp-settings.el
;;; ==================================================================

;;; harp-settings.el --- Harp settings section -*- lexical-binding: t; -*-

;; Exposes Harp's defcustoms on the stock Emacs Settings screen, rendered from
;; their schemas.  The registry is the wire allowlist: only these symbols can
;; be set from the phone, values are validated against their `custom-type', and
;; changing one drops the read memo and re-renders.  Mirrors harp-kmp's
;; AppSettingsScreen (OCR settings are omitted — OCR is deferred).

;;; Code:

(require 'jetpacs-settings)
(require 'jetpacs-shell)
;; (require 'harp-ui) — bundle-internal, provided above
;; (require 'harp-profile)                 ; harp-directory — bundle-internal, provided above
;; (require 'harp-export)                  ; harp-export-directory — bundle-internal, provided above

(defun harp-settings--after-set (symbol _value)
  "Drop caches and re-render after a Harp setting SYMBOL changes."
  (when (memq symbol '(harp-directory harp-export-directory harp-hide-private))
    (harp-ui-invalidate)))

(add-hook 'jetpacs-settings-after-set-hook #'harp-settings--after-set)

(with-jetpacs-owner "harp"
  (jetpacs-settings-register-section
   "Harp"
   '((harp-directory        :label "Profiles directory")
     (harp-export-directory  :label "HTML export directory")
     (harp-hide-private      :label "Hide private items"))))

(provide 'harp-settings)
;;; harp-settings.el ends here

;;; ==================================================================
;;; BEGIN apps/harp/harp-pack.el
;;; ==================================================================

;;; harp-pack.el --- The Harp engine-pack manifest -*- lexical-binding: t; -*-

;; Harp ships as a jetpacs engine pack: the Tier-1 elisp plus a
;; machine-readable manifest, `harp-pack.json', telling the launcher/composer
;; what it exposes without reading elisp — the composer-facing ACTIONS its
;; cards fire, and the Emacs packages it relies on.  The manifest is built from
;; LIVE registrations (`jetpacs-action-catalog'), so it can never drift from
;; what the app actually registers; `emacs/build-pack.el' regenerates the
;; committed JSON and a test asserts the two agree.
;;
;; Harp registers no declarative data SOURCES or layouts (its screens are
;; `:builder' views over the org profile), so those arrays are empty.

;;; Code:

(require 'cl-lib)
(require 'json)                          ; json-pretty-print-buffer
(require 'jetpacs-surfaces)              ; jetpacs-action-catalog

(defconst harp-pack-id "harp"
  "The pack id the launcher keys Harp by.")

(defconst harp-pack-version "0.1.0"
  "Version of the Harp pack (distinct from `jetpacs-api-version').")

(defconst harp-pack-min-jetpacs-api "1.5.0"
  "Minimum jetpacs api the Harp pack requires.
Optional capabilities (owner-scoped reminders, chart / month_grid nodes) are
negotiated at runtime and degrade, so they do not raise the floor.")

(defconst harp-pack-depends
  '(((name . "org")    (min_version . "9.6"))
    ((name . "cl-lib") (min_version . "1.0")))
  "Emacs packages the Harp engine relies on, for the composer to install.")

(defun harp-pack--sort-by (key entries)
  "ENTRIES (a list of alists) sorted by their KEY value, for a stable manifest."
  (sort (copy-sequence entries)
        (lambda (a b) (string< (format "%s" (alist-get key a))
                               (format "%s" (alist-get key b))))))

(defun harp-pack-manifest ()
  "The Harp engine-pack manifest, built from live registrations.
A JSON-serializable alist; actions are name-sorted so the generated
`harp-pack.json' is byte-stable."
  (list (cons 'pack_id         harp-pack-id)
        (cons 'pack_version    harp-pack-version)
        (cons 'min_jetpacs_api harp-pack-min-jetpacs-api)
        (cons 'feature         harp-pack-id)
        (cons 'depends         (vconcat harp-pack-depends))
        (cons 'layouts         (vconcat nil))
        (cons 'sources         (vconcat nil))
        (cons 'actions         (vconcat (harp-pack--sort-by
                                         'action (jetpacs-action-catalog "harp"))))))

(defun harp-pack-json ()
  "The manifest as pretty-printed, newline-terminated JSON text."
  (with-temp-buffer
    (insert (json-serialize (harp-pack-manifest)
                            :null-object :null :false-object :false))
    (json-pretty-print-buffer)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (buffer-string)))

(defun harp-pack-write (file)
  "Write the manifest JSON to FILE.  Returns FILE."
  (let ((coding-system-for-write 'utf-8))
    (with-temp-file file (insert (harp-pack-json))))
  file)

(provide 'harp-pack)
;;; harp-pack.el ends here

;;; ==================================================================
;;; BEGIN apps/harp/harp.el
;;; ==================================================================

;;; harp.el --- Harp: a Personal Health Record org app on Jetpacs -*- lexical-binding: t; -*-

;; Harp is a privacy-centric, on-device Personal Health Record app.  It
;; stores journals, health metrics, medical documents, and reminders in
;; plain Org files that stay entirely under the user's control.  This is
;; the Jetpacs Tier-1 rebuild of harp-kmp: the phone is a thin renderer,
;; Emacs + org-mode is the source of truth, and the whole app is elisp on
;; top of the Jetpacs core.
;;
;;   (require 'jetpacs-core)   ; from the jetpacs repo / submodule
;;   (require 'harp)           ; this app
;;
;; The pre-built single-file bundle at the repo root carries the same
;; feature name, so init files work unchanged with either install option.
;;
;; This is the app entry point: it pulls in every Harp module (each
;; self-registers its views and actions under the "harp" owner) and
;; declares the app identity.

;;; Code:

(require 'jetpacs-shell)
(require 'jetpacs-apps)
(require 'jetpacs-widgets)
(require 'jetpacs-surfaces)

;; (require 'harp-metrics-parse) — bundle-internal, provided above
;; (require 'harp-profile) — bundle-internal, provided above
;; (require 'harp-ui) — bundle-internal, provided above
;; (require 'harp-write) — bundle-internal, provided above
;; (require 'harp-med) — bundle-internal, provided above
;; (require 'harp-med-schedule) — bundle-internal, provided above
;; (require 'harp-med-log) — bundle-internal, provided above
;; (require 'harp-med-view) — bundle-internal, provided above
;; (require 'harp-med-form) — bundle-internal, provided above
;; (require 'harp-med-notify) — bundle-internal, provided above
;; (require 'harp-home) — bundle-internal, provided above
;; (require 'harp-journal) — bundle-internal, provided above
;; (require 'harp-documents) — bundle-internal, provided above
;; (require 'harp-metrics) — bundle-internal, provided above
;; (require 'harp-reminders) — bundle-internal, provided above
;; (require 'harp-form) — bundle-internal, provided above
;; (require 'harp-export) — bundle-internal, provided above
;; (require 'harp-demo) — bundle-internal, provided above
;; (require 'harp-settings) — bundle-internal, provided above
;; (require 'harp-pack) — bundle-internal, provided above

(defconst harp-app-id "harp"
  "App id keying Harp's ownership, config subtree, and views.")

(defconst harp-version "0.0.1"
  "Harp app version.")

(with-jetpacs-owner "harp"
  (jetpacs-defapp harp-app-id
    :label "Harp"
    :icon "favorite"
    :views '("harp.home" "harp.journal" "harp.meds" "harp.documents"
             "harp.metrics" "harp.reminders" "harp.form"
             "harp.med.detail" "harp.med.form")
    :order 10))

(provide 'harp)
;;; harp.el ends here

(provide 'harp)
;;; harp.el ends here
