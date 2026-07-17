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
(require 'harp-ui)
(require 'harp-write)
(require 'harp-journal)

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
