;;; harp-ui.el --- Harp Foundation screens and form schema -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1"))
;;; Commentary:
;; All UI is real Jetpacs IR, read from explicit state and route inputs.
;;; Code:
(require 'harp-state)
(require 'harp-export)
(require 'harp-design)
(require 'jetpacs-chrome)
(defvar harp-field-scope nil "Dynamic form identity namespace.")
(defconst harp-form-schema
  '(("profile" (name "Name") (type "Profile type: human, cat, dog"))
    ("entry" (journal "Journal") (date "Date (YYYY-MM-DD)") (time "Time (HH:MM)")
     (tags "Tags") (private "Private: yes or no") (body "Journal entry" multiline))
    ("document" (name "Document name") (date "Date (YYYY-MM-DD)") (time "Time (HH:MM)")
     (source "Source") (files "Attachment names (comma separated)") (tags "Tags")
     (private "Private: yes or no") (annotation "Annotation" multiline))
    ("metric" (name "Metric name") (id "Metric token") (unit "Unit") (range "Plot range (low-high)")
     (healthy-range "Reference range (low-high)") (tags "Tags"))
    ("reminder" (title "Reminder") (date "Date (YYYY-MM-DD)") (time "Time (HH:MM)") (body "Notes" multiline))
    ("medication" (name "Medication name") (unit "Unit") (stock "Stock") (refill "Refill amount")
     (low-stock "Low stock threshold") (expires "Expiry (YYYY-MM-DD)") (active "Active: yes or no")
     (amount "Dose amount") (times "Dose times (HH:MM, HH:MM)") (days "Weekdays (mon, tue, ...)")
     (interval "Interval (e.g. 8h)") (window "Daily window (HH:MM-HH:MM)")
     (cycle-start "Cycle start (YYYY-MM-DD)") (active-days "Active days") (pause-days "Pause days")
     (from "Schedule starts (YYYY-MM-DD)") (until "Schedule ends (YYYY-MM-DD)")
     (tags "Tags") (notes "Notes" multiline)))
  "App-owned editable fields; capture handlers use exactly the same schema.")

(defun harp-form-scope (route)
  "Return a bounded namespace for ROUTE's form kind and revision-bound identity."
  (concat "harp-" (substring (secure-hash 'sha256
                        (format "%s:%s" (plist-get route :form)
                                (or (plist-get route :id) (plist-get route :revision) "new"))) 0 24)))

(defun harp-field-id (name)
  "Return NAME's wire identity in the current form namespace."
  (jetpacs-wire-id (or harp-field-scope "harp") (if (symbolp name) (symbol-name name) name)))

(defun harp-action (name &optional args fields confirm)
  "Build named semantic NAME with opaque ARGS, captured FIELDS and CONFIRM."
  (jetpacs-action name :args args :when-offline "drop" :confirm confirm
                  :capture-fields (mapcar #'harp-field-id fields)))

(defun harp-ui-nav (view)
  "Build horizontally scrollable app destinations with VIEW selected."
  (apply #'jetpacs-row
         (append (mapcar (lambda (pair)
                           (jetpacs-chip (cdr pair) :selected (equal (car pair) view)
                             :on-tap (harp-action "harp.open" (list :view (car pair)))))
                         '(("profiles" . "Profiles") ("journal" . "Journal") ("metrics" . "Metrics")
                           ("documents" . "Documents") ("medications" . "Medications")
                           ("reminders" . "Reminders") ("settings" . "Settings"))) '(:scroll t :spacing 4))))

(defun harp-ui-text (value &optional style)
  "Build readable VALUE with optional typography STYLE."
  (jetpacs-text (format "%s" (or value "")) :style (or style "body")))

(defun harp-ui-range (range)
  "Format the recovered numeric RANGE cons."
  (if range (format "%s-%s" (or (car range) "") (or (cdr range) "")) ""))

(defun harp-ui-default (field item)
  "Return FIELD's initial form string from ITEM and the snapshot clock."
  (let* ((stamp (or (alist-get 'datetime item) (alist-get 'scheduled item)))
         (date (and stamp (string-match "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" stamp) (match-string 0 stamp)))
         (schedule (car (alist-get 'schedules item)))
         (value (or (alist-get field item) (alist-get field schedule))))
    (pcase field
      ('date (or date (format-time-string "%Y-%m-%d" (plist-get harp-state :now))))
      ('time (if (and stamp (string-match "[0-9]\\{2\\}:[0-9]\\{2\\}" stamp)) (match-string 0 stamp) "12:00"))
      ('journal (or value (alist-get 'name (car (alist-get 'journals (harp-ui-current)))) "Main"))
      ('type (or value "human"))
      ('private (if value "yes" "no"))
      ('active (if (or (null item) value) "yes" "no"))
      ((or 'range 'healthy-range) (harp-ui-range value))
      ((or 'tags 'files 'times 'days) (mapconcat (lambda (x) (format "%s" x)) value ", "))
      ((or 'expires 'cycle-start 'from 'until)
       (if (and (stringp value) (string-match "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" value)) (match-string 0 value) ""))
      (_ (if value (format "%s" value) "")))))

(defun harp-ui-form (route)
  "Build the controlled form for ROUTE with revision-scoped field identities."
  (let* ((kind (plist-get route :form)) (token (plist-get route :id))
         (ref (cdr (assoc token (plist-get harp-state :refs)))) (item (plist-get ref :item))
         (schema (cdr (assoc kind harp-form-schema)))
         (fields (mapcar #'car schema)))
    (if (and token (null ref)) (list (harp-ui-text "This form is stale. Refresh and reopen it."))
      (append
       (list (harp-ui-text (concat (if token "Edit " "New ") kind) "title"))
       (mapcar (lambda (field)
                 (jetpacs-text-input (harp-field-id (car field)) :label (cadr field)
                   :value (harp-ui-default (car field) item) :single-line (if (nth 2 field) :json-false t)
                   :min-lines (if (nth 2 field) 4 1))) schema)
       (list (jetpacs-button "Save" (harp-action "harp.save"
                         (list :form kind :id token :revision (plist-get route :revision)) fields)))
       (when token (list (jetpacs-button "Delete record"
                          (harp-action "harp.delete" (list :id token) nil "Delete this record permanently?")
                          :variant "text")))))))

(defun harp-ui-record (item kind)
  "Build ITEM as a Foundation card for domain KIND."
  (jetpacs-card
   (delq nil (list (harp-ui-text (or (alist-get 'name item) (alist-get 'title item)
                          (alist-get 'datetime item)) "title")
         (harp-ui-text (string-join (delq nil (list (alist-get 'journal item)
                                       (alist-get 'datetime item) (alist-get 'source item))) " · ") "caption")
         (harp-ui-text (truncate-string-to-width (or (alist-get 'body item) (alist-get 'annotation item)
                                                    (alist-get 'notes item) "") 500 nil nil "…"))
         (harp-ui-text (mapconcat (lambda (tag) (concat "#" tag)) (alist-get 'tags item) " ") "caption")
         (when (alist-get 'files item) (harp-ui-text (string-join (alist-get 'files item) " · ") "caption"))))
   :on-tap (harp-action "harp.form" (list :form kind :id (alist-get 'token item)))))

(defun harp-ui-page (items builder)
  "Render a bounded page of ITEMS using BUILDER with visible paging controls."
  (let* ((total (length items)) (page (min harp-state-page (max 0 (/ (1- total) harp-page-size))))
         (start (* page harp-page-size)) (end (min total (+ start harp-page-size))))
    (append (mapcar builder (cl-subseq items start end))
            (unless items (list (harp-ui-text "No records to show.")))
            (when (> total harp-page-size)
              (list (jetpacs-row
                     (jetpacs-button "Previous" (harp-action "harp.page" (list :page (max 0 (1- page)))))
                     (harp-ui-text (format "%d–%d / %d" (1+ start) end total) "caption")
                     (jetpacs-button "Next" (harp-action "harp.page" (list :page (1+ page)))) :spacing 8))))))

(defun harp-ui-metric (metric)
  "Render METRIC with a bounded chart and accessible reading labels."
  (let* ((profile (harp-export--filter-profile (harp-ui-current) (not harp-hide-private)))
         (values (harp-profile-metric-values profile (alist-get 'id metric)))
         (values (last values 60)) (latest (car (last values)))
         (label (format "%s: %s %s" (alist-get 'name metric)
                        (if latest (alist-get 'value latest) "No readings") (alist-get 'unit metric))))
    (jetpacs-card
     (delq nil (list (harp-ui-text label "title")
                    (harp-ui-text (concat "Reference range: " (harp-ui-range (alist-get 'healthy-range metric))) "caption")
                    (when values
                      (jetpacs-chart (list (jetpacs-chart-series
                                          (cl-loop for value in values for i from 0
                                                   collect (jetpacs-chart-point i (alist-get 'value value)))
                                          :name (alist-get 'name metric) :color "primary"))
                                     :kind "line" :height 160 :summary label
                                     :y-range (let ((range (alist-get 'range metric)))
                                                (when (and (numberp (car range)) (numberp (cdr range))
                                                           (< (car range) (cdr range)))
                                                  (list (car range) (cdr range))))))
                    (harp-ui-text (mapconcat (lambda (v) (format "%s: %s" (alist-get 'datetime v) (alist-get 'value v)))
                                             (last values 3) " · ") "caption")
                    (jetpacs-button "Edit metric" (harp-action "harp.form" (list :form "metric" :id (alist-get 'token metric))) :variant "text"))))))

(defun harp-ui-medication (med)
  "Render MED with stock, schedule, history and explicit record actions."
  (jetpacs-card
   (list (harp-ui-text (alist-get 'name med) "title")
         (harp-ui-text (format "Stock: %s %s   ·   Refill: %s" (or (alist-get 'stock med) "untracked")
                              (or (alist-get 'unit med) "") (or (alist-get 'refill med) "unset")))
         (harp-ui-text (mapconcat (lambda (s) (format "%s %s · %s" (or (alist-get 'amount s) "")
                                                     (or (alist-get 'unit med) "")
                                                     (if (alist-get 'times s) (string-join (alist-get 'times s) ", ")
                                                       (or (alist-get 'interval s) "unscheduled"))))
                                 (alist-get 'schedules med) "; ") "caption")
         (harp-ui-text (format "Recorded doses: %d" (length (alist-get 'doses med))) "caption")
         (jetpacs-row
          (jetpacs-button "Edit" (harp-action "harp.form" (list :form "medication" :id (alist-get 'token med))))
          (when (alist-get 'refill med)
            (jetpacs-button "Refill" (harp-action "harp.refill" (list :id (alist-get 'token med)) nil "Add a refill to stock?"))) :spacing 8))))

(defun harp-ui-dose (entry)
  "Render pending dose ENTRY; amounts come from the producer's schedule."
  (let ((med (plist-get entry :med)) (dose (plist-get entry :dose)))
    (jetpacs-card
     (list (harp-ui-text (format "%s · %s" (alist-get 'time dose) (alist-get 'name med)) "title")
           (harp-ui-text (format "%s %s" (alist-get 'amount dose) (alist-get 'unit dose)))
           (jetpacs-row
            (jetpacs-button "Taken" (harp-action "harp.dose" (list :id (plist-get entry :token) :state "taken")))
            (jetpacs-button "Skip" (harp-action "harp.dose" (list :id (plist-get entry :token) :state "skipped"))) :spacing 8)))))

(defun harp-ui-body (view)
  "Build destination VIEW solely from the published snapshot."
  (let ((profile (harp-ui-current)))
    (append
     (list (harp-ui-nav view))
     (cond
      ((equal view "profiles")
       (append (harp-ui-page (plist-get harp-state :profiles)
                            (lambda (p) (jetpacs-component-list-item
                                         (alist-get 'name p) :subtitle (alist-get 'type p) :leading (jetpacs-icon "favorite")
                                         :on-tap (harp-action "harp.profile" (list :id (alist-get 'token p))))))
               (list (jetpacs-button "New profile" (harp-action "harp.form" '(:form "profile"))))))
      ((equal view "settings")
       (list (harp-ui-text "Harp" "title")
             (jetpacs-button (if harp-state-dark "Use light appearance" "Use dark appearance") (harp-action "harp.theme"))
             (jetpacs-button (if harp-hide-private "Show private records" "Hide private records") (harp-action "harp.privacy"))
             (jetpacs-button "Export public records as Org" (harp-action "harp.export" '(:format "org")))
             (jetpacs-button "Export public timeline as HTML" (harp-action "harp.export" '(:format "html")))
             (harp-ui-text "Exports are saved in the profile directory. Attachments remain file references." "caption")))
      ((null profile) (list (harp-ui-text "Open or create a profile first.")))
      (t
       (append
        (list (harp-ui-text (alist-get 'name profile) "title")
              (jetpacs-row
               (jetpacs-chip (if harp-state-tag (concat "#" harp-state-tag " ×") "All tags")
                             :on-tap (harp-action "harp.filter"))
               (apply #'jetpacs-row
                      (append (mapcar (lambda (tag) (jetpacs-chip (concat "#" tag) :on-tap (harp-action "harp.filter" (list :tag tag))))
                                      (cl-subseq (harp-profile-tags profile) 0 (min 16 (length (harp-profile-tags profile))))) '(:scroll t))) :spacing 4))
        (pcase view
          ("journal" (harp-ui-page (cl-remove-if-not #'harp-visible-p (apply #'append (mapcar (lambda (j) (alist-get 'entries j)) (alist-get 'journals profile))))
                                    (lambda (item) (harp-ui-record item "entry"))))
          ("documents" (harp-ui-page (cl-remove-if-not #'harp-visible-p (alist-get 'documents profile)) (lambda (item) (harp-ui-record item "document"))))
          ("metrics" (harp-ui-page (cl-remove-if-not #'harp-visible-p (alist-get 'metrics profile)) #'harp-ui-metric))
          ("medications" (append (list (harp-ui-text "Due today" "title"))
                                  (harp-ui-page (plist-get harp-state :doses) #'harp-ui-dose)
                                  (list (harp-ui-text "Medications" "title"))
                                  (harp-ui-page (alist-get 'medications profile) #'harp-ui-medication)))
          ("reminders" (append
                         (list (jetpacs-month-grid (format-time-string "%Y-%m" (plist-get harp-state :now))))
                         (harp-ui-page (alist-get 'reminders profile)
                           (lambda (item) (jetpacs-component-list-item (alist-get 'title item)
                              :subtitle (format "%s · %s" (alist-get 'todo item) (or (alist-get 'scheduled item) (alist-get 'deadline item)))
                              :on-tap (harp-action "harp.reminder" (list :id (alist-get 'token item)))
                              :trailing (jetpacs-icon "check")))))))
        (list (jetpacs-button "Add record" (harp-action "harp.form"
                  (list :form (cdr (assoc view '(("journal" . "entry") ("documents" . "document") ("metrics" . "metric")
                                                ("medications" . "medication") ("reminders" . "reminder")))))) :icon "add"))))))))

(defun harp-ui-screen (route &optional back)
  "Render ROUTE and BACK using Foundation and the current Styles profile."
  (let* ((harp-field-scope (harp-form-scope route))
         (nodes (if (plist-get route :form) (harp-ui-form route) (harp-ui-body (or (plist-get route :view) "profiles"))))
         (screen (jetpacs-chrome-screen "Harp"
                   (jetpacs-with-attrs (apply #'jetpacs-lazy-column (append (delq nil nodes) '(:spacing 12))) :padding 16)
                   :back back :scaffold '(:top-bar-style nil)
                   :actions (list (jetpacs-icon-button "refresh" (harp-action "harp.refresh") :content-description "Refresh profiles")))))
    (harp-design-wrap screen harp-state-dark)))

(provide 'harp-ui)
;;; harp-ui.el ends here
