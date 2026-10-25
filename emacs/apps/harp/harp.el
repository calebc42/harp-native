;;; harp.el --- Optional Harp Personal Health Record applet -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1"))
;;; Commentary:
;; Loading trusted applet code registers nothing and reads no health files;
;; the `harp' command or the Apps screen registers the UI.
;; Actions commit before acceptance; only presentation is deferred.
;;; Code:
(require 'harp-ui)
(require 'harp-write)
(require 'harp-med)
(require 'harp-med-schedule)
(require 'harp-med-log)
(require 'harp-export)
(require 'jetpacs-apps)
(defvar harp--timers nil "Presentation continuations owned by Harp.")
(defconst harp-views '("profiles" "journal" "documents" "metrics" "reminders" "medications" "settings") "Closed Harp destinations.")

(defun harp--later (fn)
  "Schedule presentation FN outside action dispatch and track its timer."
  (let (timer)
    (setq timer (jetpacs-flow-continue
                 (lambda () (setq harp--timers (delq timer harp--timers))
                   (condition-case err (funcall fn) (error (message "Harp presentation: %s" (error-message-string err)))))))
    (push timer harp--timers)))

(defun harp--open (route)
  "Defer presentation of ROUTE without deferring a durable effect."
  (setq harp-state-page 0 harp-state-route route)
  (harp--later (lambda ()
                (with-jetpacs-owner "harp"
                  (jetpacs-chrome-reset-screens "harp" t)
                  (if (equal (plist-get route :view) "profiles") (jetpacs-shell-push "harp")
                    (jetpacs-chrome-push-screen "harp" "harp-screen"
                      (lambda (back) (harp-ui-screen route back)))))))
  'accepted)

(defun harp--guard (fn)
  "Execute FN with explicit stale or rejected outcomes and no prompts."
  (condition-case err (funcall fn)
    ((harp-stale ebp-org-unresolved) 'stale)
    (error (message "Harp: %s" (error-message-string err)) 'rejected)))

(defun harp--committed (&optional view)
  "Refresh after a committed effect and present VIEW without changing acceptance."
  (condition-case err (progn (harp-refresh) (harp--open (list :view (or view "journal"))))
    (error (message "Harp saved; refresh needed: %s" (error-message-string err))))
  'accepted)

(defun harp--transaction (fn)
  "Run a single-profile mutation FN with in-memory rollback on save failure."
  (with-current-buffer (harp--visit-file harp-ui--current-file)
    (let ((ebp-org-roots (list harp-directory)))
      (atomic-change-group (funcall fn)))))

(defun harp--on-home (_args _params)
  "Refresh local profiles and open Harp."
  (harp--guard (lambda () (harp-refresh) (harp--open '(:view "profiles")))))
(defun harp--on-open (args _params)
  "Open the closed destination named by ARGS."
  (if (member (plist-get args :view) harp-views) (harp--open args) 'rejected))
(defun harp--on-refresh (_args _params)
  "Produce a fresh snapshot and defer presentation."
  (harp--guard (lambda () (harp-refresh) (harp--later (lambda () (jetpacs-shell-push "harp"))) 'accepted)))
(defun harp--on-profile (args _params)
  "Open the profile selected by opaque ARGS."
  (harp--guard (lambda ()
                (setq harp-ui--current-file (plist-get (harp-resolve (plist-get args :id) "profile" t) :file)
                      harp-state-tag nil)
                (harp-refresh) (harp--open '(:view "journal")))))
(defun harp--on-form (args _params)
  "Open the form kind in ARGS after checking its record identity."
  (harp--guard
   (lambda ()
     (let ((kind (plist-get args :form))
           ;; A record card inside the bound list carries no `:id': the
           ;; Companion injects the row's key as `:item' (SPEC 14.3).
           (token (or (plist-get args :id) (plist-get args :item))))
       (unless (assoc kind harp-form-schema) (error "Unknown form"))
       (when token (harp-resolve token kind t))
       (unless (or (equal kind "profile") (harp-ui-current)) (error "Open a profile"))
       (harp--open (list :form kind :id token :revision (or (plist-get harp-state :revision) (format "new-%d" harp-state-generation))))))))

(defun harp--valid-date (value)
  "Whether VALUE is an exact, real ISO calendar date."
  (and (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" value)
       (condition-case nil
           (equal value (format-time-string "%Y-%m-%d"
                           (encode-time 0 0 12 (string-to-number (substring value 8 10))
                                        (string-to-number (substring value 5 7)) (string-to-number (substring value 0 4)))))
         (error nil))))

(defun harp--fields (kind params)
  "Validate and collect KIND's captured form fields from PARAMS."
  (mapcar
   (lambda (schema)
     (let* ((key (car schema)) (value (plist-get (plist-get params :fields) (intern (concat ":" (harp-field-id key)))))
            (multiline (nth 2 schema)))
       (unless (and (stringp value) (<= (string-bytes value) (if multiline 16000 512))
                    (not (string-match-p "[\0\r]" value))
                    (or multiline (not (string-match-p "[\n]" value))))
         (error "Missing, invalid or oversized %s" key))
       (when (and (memq key '(name title journal id unit date)) (string-empty-p (string-trim value)))
         (error "%s is required" key))
       (when (and (memq key '(date expires cycle-start from until)) (not (string-empty-p value)) (not (harp--valid-date value)))
         (error "Invalid date"))
       (when (and (memq key '(stock refill low-stock amount active-days pause-days)) (not (string-empty-p value))
                  (not (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" value))) (error "Invalid nonnegative number"))
       (when (and (eq key 'time) (not (string-match-p "\\`\\(?:[01][0-9]\\|2[0-3]\\):[0-5][0-9]\\'" value))) (error "Invalid time"))
       (when (and (memq key '(private active)) (not (member value '("yes" "no")))) (error "Expected yes or no"))
       (when (and (eq key 'type) (not (member value harp-profile-types))) (error "Invalid profile type"))
       (when (and (eq key 'id) (not (string-match-p "\\`[A-Za-z][A-Za-z0-9_-]*\\'" value))) (error "Invalid metric token"))
       (when (and (eq key 'interval) (not (string-empty-p value))
                  (or (not (string-match-p "\\`[1-9][0-9]*[mhd]\\'" value))
                      (< (or (harp-med--duration-seconds value) 0) 60))) (error "Invalid interval"))
       (when (and (eq key 'times) (not (string-empty-p value)))
         (dolist (time (split-string value "[, ]+" t))
           (unless (string-match-p "\\`\\(?:[01][0-9]\\|2[0-3]\\):[0-5][0-9]\\'" time) (error "Invalid dose time"))))
       (cons key (pcase key
                   ((or 'tags 'files) (split-string value "[, ]+" t))
                   ((or 'private 'active) (equal value "yes"))
                   (_ value))))) (cdr (assoc kind harp-form-schema))))

(defun harp--save-record (kind target item fields)
  "Write domain KIND at TARGET using original ITEM and validated FIELDS."
  (cl-flet ((v (key) (alist-get key fields)))
    (pcase kind
      ("entry"
       (unless (cl-find (v 'journal) (alist-get 'journals (harp-ui-current)) :key (lambda (j) (alist-get 'name j)) :test #'equal)
         (error "Unknown journal"))
       (when (and target (not (equal (v 'journal) (alist-get 'journal item)))) (error "Moving between journals is not supported by this form"))
       (if target (harp-write-update-entry target (v 'date) (v 'time) (v 'tags) (v 'private) (v 'body))
         (harp-write-add-entry (v 'journal) (v 'date) (v 'time) (v 'tags) (v 'private) (v 'body))))
      ("document"
       (if target (harp-write-update-document target (v 'name) (v 'date) (v 'time) (v 'source) (v 'files) (v 'tags) (v 'private) (v 'annotation))
         (harp-write-add-document (v 'name) (v 'date) (v 'time) (v 'source) (v 'files) (v 'tags) (v 'private) (v 'annotation))))
      ("metric"
       (when (cl-find-if
              (lambda (metric)
                (and (equal (v 'id) (alist-get 'id metric))
                     (not (eq metric item))))
              (alist-get 'metrics (harp-ui-current)))
         (error "Metric token already exists"))
       (if target (harp-write-update-metric target (v 'name) (v 'id) (v 'unit) (v 'range) (v 'healthy-range) (v 'tags))
         (harp-write-add-metric (v 'name) (v 'id) (v 'unit) (v 'range) (v 'healthy-range) (v 'tags))))
      ("medication" (if target (harp-write-update-medication target fields) (harp-write-add-medication fields)))
      ("reminder"
       (when target (error "Use the reminder toggle to update an existing reminder"))
       (harp-write--ensure-section "Reminders")
       (harp-write--append-under "Reminders"
         (format "** TODO %s\nSCHEDULED: %s\n:PROPERTIES:\n:ID: %s\n:END:\n%s\n"
                 (v 'title) (harp-write--stamp (v 'date) (v 'time)) (org-id-new) (harp-write--escape-body (v 'body))))
       (harp-write--save (current-buffer)))
      (_ (error "Unknown form")))))

(defun harp--on-save (args params)
  "Commit ARGS' revision-bound form with validated captured PARAMS."
  (harp--guard
   (lambda ()
     (let* ((kind (plist-get args :form)) (token (plist-get args :id))
            (ref (and token (harp-resolve token kind t)))
            (item (plist-get ref :item))
            (harp-field-scope (harp-form-scope args)))
       (unless (assoc kind harp-form-schema) (error "Unknown form"))
       (unless (equal kind "profile")
         (unless (and (equal (plist-get args :revision) (plist-get harp-state :revision))
                      (equal (plist-get harp-state :revision) (harp-file-revision harp-ui--current-file)))
           (signal 'harp-stale nil)))
       (let ((fields (harp--fields kind params)))
         (if (equal kind "profile")
             (setq harp-ui--current-file (harp-write-new-profile (alist-get 'name fields) (alist-get 'type fields)))
           (harp--transaction (lambda () (harp--save-record kind (alist-get 'ref item) item fields)))))
       (harp--committed (cdr (assoc kind '(("profile" . "journal") ("entry" . "journal") ("document" . "documents")
                                         ("metric" . "metrics") ("medication" . "medications") ("reminder" . "reminders")))))))))

(defun harp--on-delete (args _params)
  "Delete the confirmed record identified by ARGS and save before acceptance."
  (harp--guard (lambda ()
    (let ((ref (harp-resolve (plist-get args :id) nil t)))
      (unless (member (plist-get ref :kind) '("entry" "document" "metric" "medication" "reminder")) (error "Invalid deletion"))
      (harp--transaction (lambda ()
        (goto-char (harp--resolve-ref (alist-get 'ref (plist-get ref :item)))) (org-back-to-heading t)
        (delete-region (point) (save-excursion (org-end-of-subtree t t))) (harp-write--save (current-buffer))))
      (harp--committed)))))
(defun harp--on-dose (args _params)
  "Commit the scheduled dose in ARGS; derive amount and time from Emacs state."
  (harp--guard (lambda ()
    (let* ((ref (harp-resolve (plist-get args :id) "dose" t)) (med (plist-get ref :item))
           (dose (plist-get ref :dose)) (state (plist-get args :state)) (amount (format "%s" (alist-get 'amount dose))))
      (unless (member state '("taken" "skipped")) (error "Invalid dose disposition"))
      (harp--transaction (lambda () (harp-med-log-write (alist-get 'ref med) state amount (alist-get 'unit med)
                                  (and (equal state "taken") (string-to-number amount)) (seconds-to-time (alist-get 'at dose)))))
      (harp--committed "medications")))))
(defun harp--on-refill (args _params)
  "Commit the configured refill for ARGS' medication."
  (harp--guard (lambda ()
    (let ((med (plist-get (harp-resolve (plist-get args :id) "medication" t) :item)))
      (unless (and (numberp (alist-get 'refill med)) (> (alist-get 'refill med) 0)) (error "Set a positive refill amount"))
      (harp--transaction (lambda () (harp-med-log-refill med))) (harp--committed "medications")))))
(defun harp--on-reminder (args _params)
  "Toggle ARGS' reminder through Org and save it before acceptance."
  (harp--guard (lambda ()
    (let ((item (plist-get (harp-resolve (plist-get args :id) "reminder" t) :item)))
      (harp--transaction (lambda ()
        (goto-char (harp--resolve-ref (alist-get 'ref item)))
        (let ((org-log-done nil) (org-log-repeat nil) (org-inhibit-logging t))
          (org-todo (if (equal (alist-get 'todo item) "DONE") "TODO" "DONE")))
        (harp-write--save (current-buffer)))) (harp--committed "reminders")))))
(defun harp--on-export (args _params)
  "Save a public-only export using ARGS' closed format, never a device path."
  (harp--guard (lambda ()
    (let* ((format (plist-get args :format)) (profile (harp-ui-current)) (file (and profile (harp-safe-file harp-ui--current-file))))
      (unless (and file (member format '("org" "html"))) (error "Open a profile and choose an export format"))
      (unless (equal (plist-get harp-state :revision) (harp-file-revision file)) (signal 'harp-stale nil))
      (let ((out (make-temp-file (expand-file-name "harp-export-" harp-directory) nil (concat "." format)))
            (text (if (equal format "org") (harp-export-text profile) (harp-export-html profile))))
        (with-temp-file out (insert text))) 'accepted))))
(defun harp--on-filter (args params)
  "Set or clear ARGS' bounded tag filter; PARAMS name the tapped surface.
The filter moves only values the record list binds to the surface data
document (`harp-ui-document'), so on the first page it patches the
document instead of re-pushing the whole stack.  From a later page the
paging buttons' arguments change too, which is a shape change: the
screen is re-pushed as before."
  (let ((tag (plist-get args :tag)))
    (if (or (null tag) (member tag (harp-profile-tags (harp-ui-current))))
        (let ((first-page (zerop harp-state-page)))
          (setq harp-state-tag tag harp-state-page 0)
          ;; Clear filters clears the text search too.
          (unless tag (setq harp-state-query ""))
          (if (and first-page (harp-ui-list-view-p harp-state-route)
                   (not (equal (plist-get harp-state-route :view) "journal")))
              (jetpacs-app-defer-patch (harp-ui-document-ops) params)
            (harp--later (lambda () (jetpacs-shell-push "harp"))))
          'accepted)
      'rejected)))
(defun harp--on-page (args _params)
  "Select ARGS' bounded record page."
  (let ((page (plist-get args :page)))
    (if (and (integerp page) (<= 0 page 100))
        (progn (setq harp-state-page page) (harp--later (lambda () (jetpacs-shell-push "harp"))) 'accepted) 'rejected)))
(defun harp--push-later ()
  "Re-present Harp's surface after this action."
  (harp--later (lambda () (jetpacs-shell-push "harp"))))

(defun harp--on-filter-open (_args _params)
  "Show or hide the filter bar's tag chips."
  (setq harp-state-filter-open (not harp-state-filter-open))
  (harp--push-later)
  'accepted)

(defun harp--on-search (_args params)
  "Filter the entries by the filter bar's text search captured in PARAMS."
  (let* ((harp-field-scope (harp-form-scope harp-state-route))
         (query (plist-get (plist-get params :fields) (intern (concat ":" (harp-field-id "query"))))))
    (if (not (and (stringp query) (<= (length query) 256))) 'rejected
      (setq harp-state-query query)
      (harp--push-later)
      'accepted)))

(defun harp--on-sort (_args _params)
  "Flip the Journals tab between newest and oldest first."
  (setq harp-state-sort (if (equal harp-state-sort "date_asc") "date_desc" "date_asc"))
  (harp--push-later)
  'accepted)

(defun harp--on-journal (_args _params)
  "Show the profile's next journal."
  (let ((count (length (alist-get 'journals (harp-ui-current)))))
    (setq harp-state-journal (if (> count 0) (% (1+ harp-state-journal) count) 0))
    (harp--push-later)
    'accepted))

(defun harp--sheet-field (params name)
  "The entry sheet's captured field NAME from PARAMS, or nil."
  (let ((harp-field-scope "harp-entry-sheet"))
    (plist-get (plist-get params :fields) (intern (concat ":" (harp-field-id name))))))

(defun harp--on-sheet (args _params)
  "Open the entry form sheet, for ARGS' entry `:id' or a new one."
  (let ((token (plist-get args :id)))
    (if (and token (not (assoc token (plist-get harp-state :refs)))) 'stale
      (setq harp-state-sheet (list :id token) harp-state-sheet-body nil)
      (harp--push-later)
      'accepted)))

(defun harp--on-sheet-close (_args _params)
  "Dismiss the entry form sheet."
  (setq harp-state-sheet nil harp-state-sheet-body nil)
  (harp--push-later)
  'accepted)

(defun harp--on-sheet-body (_args params)
  "Remember the entry form's text from PARAMS, so its error state follows it."
  (let ((body (harp--sheet-field params "body")))
    (if (not (and (stringp body) (<= (string-bytes body) 16000))) 'rejected
      (let ((was-blank (string-blank-p (or harp-state-sheet-body ""))))
        (setq harp-state-sheet-body body)
        (unless (eq was-blank (string-blank-p body)) (harp--push-later)))
      'accepted)))

(defun harp--on-sheet-tag (args params)
  "Insert or remove ARGS' `:tag' in the entry form's text captured in PARAMS."
  (let ((tag (plist-get args :tag)) (body (or (harp--sheet-field params "body") "")))
    (if (not (and (stringp tag) (member tag (harp-profile-tags (harp-ui-current))))) 'rejected
      (setq harp-state-sheet-body
            (if (harp-ui-text-has-tag body tag)
                (string-trim (replace-regexp-in-string
                              (concat "[[:space:]]*#" (regexp-quote tag) "\\b") "" body))
              (concat (string-trim-right body) (if (string-blank-p body) "" " ") "#" tag " ")))
      (harp--later (lambda ()
                     (jetpacs-shell-push "harp" :reset-input-ids
                                         (let ((harp-field-scope "harp-entry-sheet"))
                                           (list (harp-field-id "body"))))))
      'accepted)))

(defun harp--on-sheet-attach (_args _params)
  "Attachments need the Companion's file and camera pickers, not yet offered."
  (message "Harp: attachments are not available yet")
  'rejected)

(defun harp--on-sheet-save (args params)
  "Save the entry form's captured PARAMS as ARGS' entry or a new one."
  (harp--guard
   (lambda ()
     (let* ((token (plist-get args :id))
            (ref (and token (harp-resolve token "entry" t)))
            (item (plist-get ref :item))
            (body (harp--sheet-field params "body"))
            (time (harp--sheet-field params "time"))
            (date (harp--sheet-field params "date"))
            (journals (alist-get 'journals (harp-ui-current)))
            (journal (nth (min harp-state-journal (max 0 (1- (length journals)))) journals)))
       (unless (equal (plist-get args :revision) (plist-get harp-state :revision)) (signal 'harp-stale nil))
       (unless (and (stringp body) (not (string-blank-p body)) (<= (string-bytes body) 16000)
                    (not (string-match-p "[\0\r]" body)))
         (error "Text can't be empty"))
       (unless (and (stringp time) (string-match-p "\\`\\(?:[01][0-9]\\|2[0-3]\\):[0-5][0-9]\\'" time))
         (error "Invalid time"))
       (unless (and (stringp date) (harp--valid-date date)) (error "Invalid date"))
       (harp--transaction
        (lambda ()
          (if ref (harp-write-update-entry (alist-get 'ref item) date time (alist-get 'heading-tags item)
                                           (alist-get 'private item) body)
            (harp-write-add-entry (alist-get 'name journal) date time nil nil body))))
       (setq harp-state-sheet nil harp-state-sheet-body nil)
       (harp--committed "journal")))))

(defun harp--on-theme (_args _params)
  "Toggle the explicit Harp appearance state."
  (setq harp-state-dark (not harp-state-dark)) (harp--later (lambda () (jetpacs-shell-push "harp"))) 'accepted)
(defun harp--on-privacy (_args _params)
  "Toggle visibility of private records in this Emacs session."
  (setq harp-hide-private (not harp-hide-private)) (harp--later (lambda () (jetpacs-shell-push "harp"))) 'accepted)

(defun harp-register ()
  "Register Harp without reading profiles or enabling alarms."
  (harp-design-register)
  (with-jetpacs-owner "harp"
    (jetpacs-chrome-define-root "harp" "profiles" (lambda (back) (harp-ui-screen '(:view "profiles") back))
                                :required t :data-builder #'harp-ui-document)
    (jetpacs-defaction "harp.home" #'harp--on-home :doc "Refresh and open Harp profiles")
    (jetpacs-defaction "harp.open" #'harp--on-open :doc "Open a Harp destination" :args '((:name view :type "enum" :required t)))
    (jetpacs-defaction "harp.refresh" #'harp--on-refresh :doc "Refresh the bounded health-record snapshot")
    (jetpacs-defaction "harp.profile" #'harp--on-profile :doc "Open the profile selected by an opaque identity" :args '((:name id :type "ref" :required t)))
    (jetpacs-defaction "harp.form" #'harp--on-form :doc "Open a record capture or edit form" :args '((:name form :type "enum" :required t) (:name id :type "ref") (:name item :type "ref")))
    (jetpacs-defaction "harp.save" #'harp--on-save :doc "Validate captured fields and save the Org record before acceptance" :args '((:name form :type "enum" :required t) (:name id :type "ref") (:name revision :type "text" :required t)))
    (jetpacs-defaction "harp.delete" #'harp--on-delete :doc "Delete a confirmed current record" :args '((:name id :type "ref" :required t)))
    (jetpacs-defaction "harp.dose" #'harp--on-dose :doc "Record a scheduled dose and atomically adjust stock" :args '((:name id :type "ref" :required t) (:name state :type "enum" :required t)))
    (jetpacs-defaction "harp.refill" #'harp--on-refill :doc "Add the configured refill amount to medication stock" :args '((:name id :type "ref" :required t)))
    (jetpacs-defaction "harp.reminder" #'harp--on-reminder :doc "Toggle and save a reminder through Org" :args '((:name id :type "ref" :required t)))
    (jetpacs-defaction "harp.export" #'harp--on-export :doc "Save public journal and document records as Org or HTML" :args '((:name format :type "enum" :required t)))
    (jetpacs-defaction "harp.filter" #'harp--on-filter :doc "Set or clear the tag filter" :args '((:name tag :type "text")))
    (jetpacs-defaction "harp.page" #'harp--on-page :doc "Show a bounded record page" :args '((:name page :type "number" :required t)))
    (jetpacs-defaction "harp.sheet" #'harp--on-sheet :doc "Open the entry form sheet for an entry or a new one" :args '((:name id :type "ref")))
    (jetpacs-defaction "harp.sheet-close" #'harp--on-sheet-close :doc "Dismiss the entry form sheet")
    (jetpacs-defaction "harp.sheet-body" #'harp--on-sheet-body :doc "Remember the entry form's captured text")
    (jetpacs-defaction "harp.sheet-tag" #'harp--on-sheet-tag :doc "Insert or remove a tag in the entry form's text" :args '((:name tag :type "string" :required t)))
    (jetpacs-defaction "harp.sheet-attach" #'harp--on-sheet-attach :doc "Attach a photo or file to the entry (not yet available)")
    (jetpacs-defaction "harp.sheet-save" #'harp--on-sheet-save :doc "Save the entry form" :args '((:name id :type "ref") (:name revision :type "string")))
    (jetpacs-defaction "harp.filter-open" #'harp--on-filter-open :doc "Show or hide the filter bar's tag chips")
    (jetpacs-defaction "harp.search" #'harp--on-search :doc "Filter the entries by the captured text search")
    (jetpacs-defaction "harp.sort" #'harp--on-sort :doc "Flip the Journals tab between newest and oldest first")
    (jetpacs-defaction "harp.journal" #'harp--on-journal :doc "Show the profile's next journal")
    (jetpacs-defaction "harp.theme" #'harp--on-theme :doc "Toggle Harp light and dark appearance")
    (jetpacs-defaction "harp.privacy" #'harp--on-privacy :doc "Toggle private-record visibility")
)
  (jetpacs-defapp "harp" :label "Harp" :icon "favorite" :surfaces '("harp") :chrome 'standalone
    :home-route "profiles" :order 47 :destinations '((:key "profiles" :label "Profiles" :icon "favorite" :verb "harp.home"))
    :requires-extensions '("jetpacs.components" "jetpacs.design")) t)

(defun harp-unregister ()
  "Remove Harp's actions, chrome, presets, timers and private snapshot.
`jetpacs-apps-unregister' sweeps everything attributed to the owner."
  (dolist (timer harp--timers) (when (timerp timer) (cancel-timer timer)))
  (setq harp--timers nil harp-state nil harp-ui--current-file nil harp-state-tag nil harp-state-page 0)
  (jetpacs-apps-unregister "harp")
  (dolist (id '("harp.light" "harp.dark")) (jetpacs-design-unregister-profile id)) t)

;;;###autoload
(defun harp ()
  "Open trusted Harp with profiles from `harp-directory'."
  (interactive) (harp-register) (harp-refresh)
  (jetpacs-apps-seed-current "harp" "profiles") (jetpacs-chrome-reset-screens "harp"))
(defun harp-unload-function ()
  "Remove Harp registrations before feature unloading."
  (harp-unregister) nil)
(provide 'harp)
;;; harp.el ends here
