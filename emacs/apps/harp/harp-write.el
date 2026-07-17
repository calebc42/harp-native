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
(require 'harp-profile)
(require 'harp-ui)

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
