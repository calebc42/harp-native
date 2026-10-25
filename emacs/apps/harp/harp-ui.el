;;; harp-ui.el --- Harp Foundation screens and form schema -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1"))
;;; Commentary:
;; All UI is real Jetpacs IR, read from explicit state and route inputs.
;;; Code:
(require 'harp-state)
(require 'harp-export)
(require 'harp-design)
(require 'harp-icons)
(require 'jetpacs-chrome)
(require 'jetpacs-widgets)
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
  (jetpacs-opaque-id "harp-"
                     (format "%s:%s" (plist-get route :form)
                             (or (plist-get route :id) (plist-get route :revision)
                                 "new"))
                     24))

(defun harp-field-id (name)
  "Return NAME's wire identity in the current form namespace."
  (jetpacs-wire-id (or harp-field-scope "harp") (if (symbolp name) (symbol-name name) name)))

(defun harp-action (name &optional args fields confirm)
  "Build named semantic NAME with opaque ARGS, captured FIELDS and CONFIRM."
  (jetpacs-action name :args args :when-offline "drop" :confirm confirm
                  :capture-fields (mapcar #'harp-field-id fields)))

(defun harp-ui-text (value &optional style)
  "Build readable VALUE with optional typography STYLE.
A `jetpacs-ref' VALUE passes through: the Companion resolves it against
the surface data document (SPEC 13.8.3)."
  (jetpacs-text (if (jetpacs-ref-p value) value (format "%s" (or value "")))
                :style (or style "body")))

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

(defconst harp-ui-list-views '(("journal" . "entry") ("documents" . "document"))
  "The record views whose page the surface data document carries, by form kind.")

(defun harp-ui-list-view-p (route)
  "Non-nil for a ROUTE whose view is a record list bound to the document."
  (and (not (plist-get route :form))
       (assoc (plist-get route :view) harp-ui-list-views)
       t))

(defun harp-ui-list-items (view profile)
  "The visible records of list VIEW in PROFILE, in page order."
  (cl-remove-if-not
   #'harp-visible-p
   (if (equal view "journal")
       (apply #'append (mapcar (lambda (j) (alist-get 'entries j)) (alist-get 'journals profile)))
     (alist-get 'documents profile))))

(defun harp-ui-row (item)
  "ITEM as a surface data document row: the values its card binds."
  (list :id (alist-get 'token item)
        :title (format "%s" (or (alist-get 'name item) (alist-get 'title item) (alist-get 'datetime item) ""))
        :caption (string-join (delq nil (list (alist-get 'journal item) (alist-get 'datetime item)
                                             (alist-get 'source item))) " · ")
        :body (truncate-string-to-width (or (alist-get 'body item) (alist-get 'annotation item)
                                            (alist-get 'notes item) "") 500 nil nil "…")
        :tags (mapconcat (lambda (tag) (concat "#" tag)) (alist-get 'tags item) " ")
        :files (string-join (alist-get 'files item) " · ")))

(defun harp-ui-document ()
  "The surface data document: the current list page and its labels.
Built beside every push (the chrome root's `:data-builder') from
`harp-state-route' and the published snapshot, never from files; a
filter change on the first page patches it (`harp--on-filter') instead
of re-pushing the stack (SPEC 13.8.4)."
  (let* ((profile (harp-ui-current))
         (route harp-state-route)
         (view (and profile (harp-ui-list-view-p route) (plist-get route :view)))
         (items (and view (harp-ui-list-items view profile)))
         (total (length items))
         (page (min harp-state-page (max 0 (/ (1- total) harp-page-size))))
         (start (* page harp-page-size))
         (end (min total (+ start harp-page-size))))
    (list :rows (vconcat (mapcar #'harp-ui-row (cl-subseq items start end)))
          :filter_label (if harp-state-tag (concat "#" harp-state-tag " ×") "All tags")
          :page_label (if (> total harp-page-size) (format "%d–%d / %d" (1+ start) end total) "")
          :paging (if (> total harp-page-size) t :json-false))))

(defun harp-ui-document-ops ()
  "DataOps replacing every value `harp-ui-document' authors."
  (let ((doc (harp-ui-document)))
    (list (jetpacs-data-set "/rows" (plist-get doc :rows))
          (jetpacs-data-set "/filter_label" (plist-get doc :filter_label))
          (jetpacs-data-set "/page_label" (plist-get doc :page_label))
          (jetpacs-data-set "/paging" (plist-get doc :paging)))))

(defun harp-ui-record-template (kind)
  "The record card as a row template over the document (SPEC 13.8.5).
Every text binds a member of the item; the tap opens form KIND for the
row whose key the Companion injects as `:item'."
  (jetpacs-with-attrs
   (jetpacs-card
    (list (jetpacs-with-attrs (harp-ui-text (jetpacs-ref "title") "title") :key "title")
          (jetpacs-with-attrs (harp-ui-text (jetpacs-ref "caption") "caption") :key "caption")
          (jetpacs-with-attrs (harp-ui-text (jetpacs-ref "body")) :key "body")
          (jetpacs-with-attrs (harp-ui-text (jetpacs-ref "tags") "caption") :key "tags")
          (jetpacs-with-attrs (harp-ui-text (jetpacs-ref "files") "caption") :key "files"
                              :visible_when (vector (jetpacs-when "files" "non_empty"))))
    :on-tap (harp-action "harp.form" (list :form kind)))
   :key (jetpacs-ref "id")))

(defun harp-ui-template-page (kind)
  "The bound record page for form KIND: rows, the empty note, paging.
One column whose children are all keyed, so its conditional members sit
among keyed siblings (SPEC 13.8.6); the paging buttons carry the pages
beside `harp-state-page', so a later page is re-pushed, never patched."
  (let ((page harp-state-page))
    (jetpacs-column
     (jetpacs-with-attrs
      (jetpacs-column (jetpacs-template "/rows" (harp-ui-record-template kind)) :spacing 12)
      :key "rows")
     (jetpacs-with-attrs (harp-ui-text "No records to show.") :key "empty"
                         :visible_when (vector (jetpacs-when "/rows" "empty")))
     (jetpacs-with-attrs
      (jetpacs-row (jetpacs-button "Previous" (harp-action "harp.page" (list :page (max 0 (1- page)))))
                   (harp-ui-text (jetpacs-ref "/page_label") "caption")
                   (jetpacs-button "Next" (harp-action "harp.page" (list :page (1+ page)))) :spacing 8)
      :key "paging" :visible_when (vector (jetpacs-when "/paging" "eq" t)))
     :spacing 12)))

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
     nil
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
               (jetpacs-chip (jetpacs-ref "/filter_label") :on-tap (harp-action "harp.filter"))
               (apply #'jetpacs-row
                      (append (mapcar (lambda (tag) (jetpacs-chip (concat "#" tag) :on-tap (harp-action "harp.filter" (list :tag tag))))
                                      (cl-subseq (harp-profile-tags profile) 0 (min 16 (length (harp-profile-tags profile))))) '(:scroll t))) :spacing 4))
        (pcase view
          ("journal" (list (harp-ui-template-page "entry")))
          ("documents" (list (harp-ui-template-page "document")))
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

;;; The upstream Harp look: HomeScreen and ProfileScreen

(defun harp-ui-dark ()
  "Non-nil when the dark Harp palette is selected."
  harp-state-dark)

(defun harp-ui-color (name)
  "Harp color NAME in the selected palette."
  (harp-design-color name (harp-ui-dark)))

(defun harp-ui-styled (style &rest children)
  "CHILDREN under Harp STYLE, a style name or a list of them."
  (jetpacs-design-styled (if (listp style) style (list style)) children))

(defun harp-ui-t (style text &rest attrs)
  "TEXT in Harp text STYLE, with ATTRS on the text.
ATTRS are universal attributes, except :max-lines, which the text takes."
  (let ((max-lines (plist-get attrs :max-lines))
        (universal (cl-loop for (k v) on attrs by #'cddr unless (eq k :max-lines) append (list k v))))
    (harp-ui-styled style (apply #'jetpacs-with-attrs (jetpacs-text text :max-lines max-lines)
                                 universal))))

(defun harp-ui-fill (node)
  "NODE filling its pressable's width.
A pressable's children take no weights, so its layout lives in one
full-width row or box."
  (jetpacs-with-attrs node :fill_fraction 1))

(defun harp-ui-icon-button (icon size color action description &optional box)
  "An IconButton running ACTION, drawing Font Awesome ICON at SIZE dp.
COLOR tints it, DESCRIPTION names it, and the button is BOX dp square,
48 by default."
  (let ((box (or box 48)))
    (jetpacs-with-attrs
     (jetpacs-design-pressable
      '("harp.icon-button") action
      (list (jetpacs-with-attrs (jetpacs-box (harp-icon icon size color description)
                                             :alignment "center")
                                :width box :height box)))
     :width box :height box)))

(defun harp-ui-profile-icon (profile)
  "The Font Awesome icon for PROFILE's type."
  (pcase (alist-get 'type profile) ("cat" "solid/cat") ("dog" "solid/dog") (_ "solid/user")))

(defun harp-ui-home-title ()
  "HarpTitle: the 30dp logo, the italic title, settings and about."
  (jetpacs-row
   (harp-icon "harp/logo" 30 (harp-ui-color 'on-surface) "Logo")
   (jetpacs-spacer :width 10)
   (harp-ui-t "harp.t.title" "Harp" :max-lines 1)
   (jetpacs-spacer :weight 1)
   (harp-ui-icon-button "solid/cog" 16 (harp-ui-color 'outline)
                        (harp-action "harp.open" '(:view "settings")) "Settings")
   (harp-ui-icon-button "solid/info-circle" 16 (harp-ui-color 'outline)
                        (harp-action "harp.open" '(:view "settings")) "About")
   :align "center"))

(defun harp-ui-profile-card (profile)
  "A recent PROFILE: ElevatedCard(small) with its icon, name and Forget."
  (jetpacs-with-attrs
   (jetpacs-box
    (harp-ui-styled
     "harp.profile-card"
     (jetpacs-row
      ;; The row opens the profile; the Forget button beside it is its own.
      (jetpacs-with-attrs
       (jetpacs-design-pressable
        '("harp.row") (harp-action "harp.profile" (list :id (alist-get 'token profile)))
        (list (harp-ui-fill
               (jetpacs-row (jetpacs-spacer :width 12)
                            (harp-icon (harp-ui-profile-icon profile) 12 (harp-ui-color 'outline)
                                       "Profile icon")
                            (jetpacs-with-attrs (harp-ui-t "harp.t.profile" (alist-get 'name profile)
                                                           :max-lines 1)
                                                :padding 12)
                            :align "center"))))
       :weight 1)
      (harp-ui-icon-button "solid/eraser" 12 (harp-ui-color 'error)
                           (harp-action "harp.refresh") "Forget profile")
      :align "center")))
   :pad '(:vertical 8)))

(defun harp-ui-home-button (icon label action)
  "A filled Button running ACTION: Font Awesome ICON at 20dp, then LABEL."
  (jetpacs-with-attrs
   (jetpacs-design-pressable
    '("harp.button") action
    (list (harp-ui-fill
           (jetpacs-box (jetpacs-row (harp-icon icon 20 (harp-ui-color 'on-primary) label)
                                     (jetpacs-spacer :width 6)
                                     (harp-ui-t "harp.t.button" label :max-lines 1)
                                     :align "center")
                        :alignment "center"))))
   :weight 1))

(defun harp-ui-home ()
  "HomeScreen: the title, recent profiles, and Open and Create below."
  (let ((profiles (plist-get harp-state :profiles)))
    (list
     :top-bar (jetpacs-with-attrs (harp-ui-styled "harp.home" (harp-ui-home-title)) :weight 1)
     :body (apply #'jetpacs-lazy-column
                  (append
                   (list (jetpacs-with-attrs
                          (harp-ui-styled
                           "harp.home"
                           (apply #'jetpacs-column
                                  (append
                                   (list (jetpacs-spacer :height 10)
                                         (harp-ui-t "harp.t.section" "Recent profiles")
                                         (jetpacs-spacer :height 8))
                                   (if profiles (mapcar #'harp-ui-profile-card profiles)
                                     (list (harp-ui-placeholder "No recently opened profiles"
                                                                "solid/exclamation"))))))
                          :key "home"))
                   nil))
     :bottom-bar (harp-ui-styled
                  "harp.home"
                  (jetpacs-column
                   (jetpacs-row
                    (harp-ui-home-button "regular/folder-open" "Open" (harp-action "harp.refresh"))
                    (jetpacs-spacer :width 12)
                    (harp-ui-home-button "regular/plus-square" "Create"
                                         (harp-action "harp.form" '(:form "profile"))))
                   ;; 12dp, plus the 4dp under a Button's 40dp pill in
                   ;; its 48dp touch target.
                   (jetpacs-spacer :height 16))))))

(defun harp-ui-placeholder (text icon)
  "PlaceholderBackground: a large faint ICON over TEXT."
  (harp-ui-styled
   "harp.placeholder"
   (jetpacs-column (harp-icon icon 96 (harp-ui-color 'surface-variant))
                   (jetpacs-spacer :height 8)
                   (harp-ui-t "harp.t.placeholder" text)
                   :align "center")))

(defun harp-ui-profile-header (profile)
  "ProfileScreen's header: logo and title, settings, then PROFILE's name."
  (harp-ui-styled
   "harp.header"
   (jetpacs-column
    (jetpacs-row
     (harp-icon "harp/logo" 20 (harp-ui-color 'on-surface) "Logo")
     (jetpacs-design-pressable
      '("harp.t.title-small" "harp.center") (harp-action "harp.home")
      (list (jetpacs-with-attrs (jetpacs-text "Harp" :max-lines 1) :pad '(:start 5))))
     (jetpacs-spacer :weight 1)
     (harp-ui-icon-button "solid/cog" 16 (harp-ui-color 'outline)
                          (harp-action "harp.open" '(:view "settings")) "Settings")
     :align "center")
    (harp-ui-styled
     "harp.name-row"
     (jetpacs-row
      (harp-icon (harp-ui-profile-icon profile) 24 (harp-ui-color 'outline) "Profile icon")
      (jetpacs-with-attrs (harp-ui-t "harp.t.name" (alist-get 'name profile) :max-lines 1)
                          :pad '(:start 12))
      :align "center")))))

(defun harp-ui--search-field ()
  "The filter bar's 180x32 search field: icon, then chromeless text."
  (harp-ui-styled
   "harp.search"
   (jetpacs-row
    (harp-icon "solid/search" 12 (harp-ui-color 'gray) "Search")
    (jetpacs-spacer :width 6)
    (jetpacs-with-attrs
     (jetpacs-design-scope
      nil nil
      (list (jetpacs-text-input (harp-field-id "query") :value harp-state-query
                                :hint "Text search" :single-line t :max-length 256
                                :on-change (harp-action "harp.search" nil '("query"))))
      :component-styles
      (list (jetpacs-design-component-style "text-field.outlined" '("harp.field.box"))
            (jetpacs-design-component-style "text-field.filled" '("harp.field.box"))
            (jetpacs-design-component-style "text-field.text" '("harp.field.text"))
            (jetpacs-design-component-style "text-field.placeholder" '("harp.field.hint"))))
     :weight 1)
    :align "center")))

(defun harp-ui-filter-bar (profile)
  "FilterBar for PROFILE: Filter by tags, the text search and the sort button."
  (let ((tags (harp-profile-tags profile)))
    (harp-ui-styled
     "harp.filter-bar"
     (apply
      #'jetpacs-column
      (delq nil
            (list
             (jetpacs-row
              (jetpacs-design-pressable
               '("harp.filter-button") (harp-action "harp.filter-open")
               (list (harp-icon "solid/filter" 10 (harp-ui-color 'gray) "Filter icon")
                     (jetpacs-with-attrs (harp-ui-t "harp.t.filter" "Filter by tags" :max-lines 1)
                                         :pad '(:start 4))))
              (jetpacs-spacer :weight 1)
              (harp-ui--search-field)
              (jetpacs-spacer :width 8)
              (jetpacs-design-pressable
               '("harp.sort-button") (harp-action "harp.sort")
               (list (jetpacs-with-attrs (jetpacs-box (harp-icon "solid/sort" 12 (harp-ui-color 'gray)
                                                                 "Sort options")
                                                      :alignment "center")
                                         :width 26 :height 26)))
              :align "center")
             (when (and harp-state-filter-open tags)
               (jetpacs-with-attrs
                (apply #'jetpacs-flow-row
                       (append (mapcar #'harp-ui-filter-chip tags) '(:spacing 6)))
                :pad '(:bottom 6)))))))))

(defun harp-ui-filter-chip (tag)
  "A FilterChip for TAG, checked while it filters."
  (let ((on (equal tag harp-state-tag)))
    (jetpacs-design-pressable
     (list (if on "harp.metric-chip" "harp.tag-chip"))
     (harp-action "harp.filter" (and (not on) (list :tag tag)))
     (delq nil (list (when on (harp-icon "solid/check" 8 (harp-ui-color 'on-secondary-container)))
                     (when on (jetpacs-spacer :width 8))
                     (harp-ui-t "harp.t.label" (concat "#" tag) :max-lines 1))))))

(defconst harp-ui-month-colors
  '((1 primary on-primary) (2 secondary on-secondary) (3 tertiary on-tertiary)
    (4 primary-container on-primary-container) (5 secondary-container on-secondary-container)
    (6 tertiary-container on-tertiary-container) (7 error-container on-error-container)
    (8 primary on-primary) (9 secondary on-secondary) (10 tertiary on-tertiary)
    (11 primary-container on-primary-container) (12 secondary-container on-secondary-container))
  "DTStampView's month band: (MONTH BACKGROUND TEXT) roles.")

(defun harp-ui-stamp (datetime)
  "DTStampView for Org DATETIME: the date card, and the time card unless midnight."
  (let* ((time (harp--time-of datetime))
         (decoded (decode-time time))
         (month (nth 4 decoded))
         (roles (cdr (assq month harp-ui-month-colors)))
         (hm (format-time-string "%H:%M" time)))
    (apply
     #'jetpacs-column
     (delq nil
           (list
            (harp-ui-styled
             "harp.stamp"
             (jetpacs-with-attrs
              (jetpacs-column
               (harp-ui-styled
                (list "harp.stamp-head" (format "harp.bg.%s" (car roles)))
                (harp-ui-styled "harp.t.month"
                                (jetpacs-with-attrs
                                 (jetpacs-text (format-time-string "%b" time) :max-lines 1
                                               :color (harp-ui-color (cadr roles)))
                                 :fill_fraction 1)))
               (jetpacs-with-attrs (harp-ui-t "harp.t.day" (number-to-string (nth 3 decoded)))
                                   :pad '(:vertical 2))
               (jetpacs-with-attrs (harp-ui-t "harp.t.year" (number-to-string (nth 5 decoded)))
                                   :pad '(:bottom 2))
               :align "center")
              :corner 6 :clip t))
            (unless (equal hm "00:00")
              (jetpacs-with-attrs
               (harp-ui-styled
                "harp.stamp"
                (jetpacs-with-attrs
                 (jetpacs-column
                  (harp-ui-styled "harp.stamp-head"
                                  (harp-ui-t "harp.t.time-label" "TIME" :fill_fraction 1))
                  (jetpacs-with-attrs (harp-ui-t "harp.t.time" hm) :pad '(:top 4 :bottom 2))
                  :align "center")
                 :corner 6 :clip t))
               :pad '(:top 8))))))))

(defconst harp-ui--inline-re
  (concat "\\(#\\([[:alnum:]_-]+\\)(\\([^)\n]*\\))\\)"
          "\\|\\(#\\([[:alnum:]_-]+\\)\\)"
          "\\|\\(\\[\\[attachment:\\([^]\n]+\\)\\]\\]\\)"
          "\\|\\(\\[\\[\\([^]\n]+\\)\\]\\(?:\\[\\([^]\n]+\\)\\]\\)?\\]\\)")
  "Harp's inline elements: #metric(value), #tag, attachment and other links.")

(defun harp-ui-body-items (paragraph)
  "PARAGRAPH as Harp's inline items.
They are strings, (tag T), (metric ID VALUE) and (attach FILE).  A hash
starts a tag or metric only after a space or at the start, as Harp's
parser reads them."
  (let ((pos 0) items)
    (while (string-match harp-ui--inline-re paragraph pos)
      (let ((start (match-beginning 0)) (end (match-end 0))
            (metric (match-string 2 paragraph)) (value (match-string 3 paragraph))
            (tag (match-string 5 paragraph)) (file (match-string 7 paragraph))
            (target (match-string 9 paragraph)) (desc (match-string 10 paragraph)))
        (if (and (or metric tag) (> start 0)
                 (not (memq (aref paragraph (1- start)) '(?\s ?\t ?\n))))
            (progn (push (substring paragraph pos (1+ start)) items) (setq pos (1+ start)))
          (when (> start pos) (push (substring paragraph pos start) items))
          (push (cond (metric (list 'metric metric value))
                      (tag (list 'tag tag))
                      (file (list 'attach (string-trim file)))
                      (t (or desc target)))
                items)
          (setq pos end))))
    (when (< pos (length paragraph)) (push (substring paragraph pos) items))
    ;; Adjacent text runs, split where a # did not start a tag, join again.
    (let (merged)
      (dolist (item (nreverse items) (nreverse merged))
        (if (and (stringp item) (stringp (car merged)))
            (setcar merged (concat (car merged) item))
          (push item merged))))))

(defun harp-ui-tag-chip (tag)
  "AssistChip for TAG: 24dp, outlined, its #label in labelLarge."
  (jetpacs-design-pressable
   '("harp.tag-chip") (harp-action "harp.filter" (list :tag tag))
   (list (harp-ui-t "harp.t.chip" (concat "#" tag) :max-lines 1))))

(defun harp-ui-metric-chip (id value)
  "MetricValueChip: the chart icon, ID and = VALUE on secondary-container."
  (harp-ui-styled
   "harp.metric-chip"
   (jetpacs-row
    (harp-icon "solid/chart-bar" 14 (harp-ui-color 'on-secondary-container))
    (jetpacs-spacer :width 6)
    (harp-ui-t "harp.t.metric" id :max-lines 1)
    (harp-ui-t "harp.t.metric" (concat " = " value) :max-lines 1)
    :align "center")))

(defun harp-ui-attach-icon (file)
  "AssetIcon's Font Awesome icon for FILE's name."
  (let ((name (downcase file)))
    (cond ((string-suffix-p ".pdf" name) "regular/file-pdf")
          ((string-match-p "\\.\\(png\\|jpe?g\\|webp\\)\\'" name) "regular/file-image")
          ((string-match-p "\\.\\(mp3\\|wav\\)\\'" name) "regular/file-audio")
          ((string-match-p "\\.\\(mp4\\|mkv\\)\\'" name) "regular/file-video")
          ((string-match-p "\\.\\(zip\\|tar\\|gz\\)\\'" name) "regular/file-archive")
          ((string-match-p "\\.\\(md\\|txt\\)\\'" name) "regular/file-alt")
          ((string-match-p "\\.\\(json\\|ya?ml\\|xml\\)\\'" name) "regular/file-code")
          (t "regular/file"))))

(defun harp-ui-attach-chip (file)
  "The compact AssetChip for FILE: icon and monospace name in a pill."
  (harp-ui-styled
   "harp.attach-gap"
   (harp-ui-styled
    "harp.attach-chip"
    (jetpacs-row
     (harp-icon (harp-ui-attach-icon file) 16 (harp-ui-color 'on-surface-variant))
     (jetpacs-with-attrs
      (harp-ui-t "harp.t.attach" (if (> (length file) 22) (concat (substring file 0 22) "...") file)
                 :max-lines 1)
      :pad '(:start 5))
     :align "center"))))

(defun harp-ui-paragraph (paragraph)
  "One RichTextView PARAGRAPH: text with its chips riding inline."
  (harp-ui-styled
   "harp.t.body"
   (jetpacs-design-inline-text
    (mapcar (lambda (item)
              (pcase item
                ((pred stringp) (jetpacs-text item))
                (`(tag ,tag)
                 (jetpacs-design-inline (harp-ui-tag-chip tag) 24
                                        :reserve-text (concat "#" tag) :reserve-extra 40))
                (`(metric ,id ,value)
                 (jetpacs-design-inline (harp-ui-metric-chip id value) 24
                                        :reserve-text (format "%s = %s" id value) :reserve-extra 56))
                (`(attach ,file)
                 (jetpacs-design-inline (harp-ui-attach-chip file) 30
                                        :reserve-text file :reserve-extra 34 :reserve-max 180))))
            (harp-ui-body-items paragraph)))))

(defun harp-ui-entry (entry)
  "JournalEntryView for ENTRY: an ElevatedCard with stamp, body and tags."
  (let* ((body (or (alist-get 'body entry) ""))
         (paragraphs (split-string body "\n[ \t]*\n" t "[ \t\n]+"))
         (in-body (cl-loop for p in paragraphs
                           append (cl-loop for item in (harp-ui-body-items p)
                                           when (eq (car-safe item) 'tag) collect (cadr item)
                                           when (eq (car-safe item) 'metric) collect (cadr item))))
         (out (cl-remove-if (lambda (tag) (or (equal tag "ATTACH") (member tag in-body)))
                            (alist-get 'heading-tags entry))))
    (jetpacs-with-attrs
     (jetpacs-box
      (harp-ui-styled
       "harp.entry"
       (jetpacs-row
        (harp-ui-stamp (alist-get 'datetime entry))
        (jetpacs-with-attrs
         (harp-ui-styled
          "harp.entry-body"
          (apply #'jetpacs-column
                 (append (mapcar #'harp-ui-paragraph paragraphs)
                         (when out
                           ;; Each AssistChip keeps a 48dp touch height: 12dp
                           ;; above and below its 24dp outline.
                           (list (jetpacs-spacer :height 27)
                                 (apply #'jetpacs-flow-row
                                        (append (mapcar (lambda (tag)
                                                          (jetpacs-with-attrs (harp-ui-tag-chip tag)
                                                                              :pad '(:vertical 12)))
                                                        out)
                                                '(:spacing 6))))))))
         :weight 1)
        (jetpacs-menu
         (list (jetpacs-menu-item "Edit" (harp-action "harp.sheet" (list :id (alist-get 'token entry))))
               (jetpacs-menu-item "Delete" (harp-action "harp.delete" (list :id (alist-get 'token entry))
                                                        nil "Delete this entry permanently?")))
         :icon "more_vert")
        :align "top")))
     :pad '(:vertical 11) :key (alist-get 'token entry))))

(defun harp-ui-journal-entries (journal)
  "JOURNAL's visible entries, filtered and ordered as the filter bar asks."
  (let* ((query (downcase (string-trim harp-state-query)))
         (entries (cl-remove-if-not
                   (lambda (e)
                     (and (harp-visible-p e)
                          (or (null harp-state-tag) (member harp-state-tag (alist-get 'tags e)))
                          (or (string-empty-p query)
                              (string-search query (downcase (or (alist-get 'body e) ""))))))
                   (alist-get 'entries journal))))
    (if (equal harp-state-sort "date_asc") (reverse entries) entries)))

(defun harp-ui-journal (profile)
  "JournalView of PROFILE: the selector card, the count and the entries."
  (let* ((journals (alist-get 'journals profile))
         (index (min harp-state-journal (max 0 (1- (length journals)))))
         (journal (nth index journals))
         (all (cl-remove-if-not #'harp-visible-p (alist-get 'entries journal)))
         (shown (and journal (harp-ui-journal-entries journal))))
    (append
     (list
      (jetpacs-with-attrs
       (jetpacs-box
        (jetpacs-design-pressable
         '("harp.selector") (harp-action "harp.journal")
         (list (harp-ui-fill
                (jetpacs-row
                 (jetpacs-with-attrs
                  (jetpacs-column (harp-ui-t "harp.t.label-variant" "Selected Journal")
                                  (jetpacs-spacer :height 4)
                                  (harp-ui-t "harp.t.body-variant" (or (alist-get 'name journal) "")))
                  :weight 1)
                 (harp-icon "solid/bars" 20 (harp-ui-color 'on-surface-variant) "Journals")
                 :align "center")))))
       :pad '(:top 10 :bottom 6) :key "selector"))
     (if (null all)
         (list (jetpacs-with-attrs (harp-ui-placeholder "No entries in this Journal" "regular/sticky-note")
                                   :key "empty"))
       (cons (jetpacs-with-attrs
              (harp-ui-styled
               "harp.count"
               (jetpacs-row
                (harp-ui-t "harp.t.label" (format "Total %d of %d %s displayed" (length shown) (length all)
                                                  (if (= (length all) 1) "entry" "entries")))
                (jetpacs-spacer :weight 1)
                (when (or harp-state-tag (not (string-empty-p harp-state-query)))
                  (jetpacs-design-pressable '("harp.t.filter") (harp-action "harp.filter")
                                            (list (jetpacs-text "Clear filters"))))))
              :key "count")
             (mapcar #'harp-ui-entry shown))))))

(defun harp-ui-nav (view)
  "NavigationBar: Journals, Metrics and Documents with VIEW selected."
  (harp-ui-styled
   "harp.nav"
   (apply
    #'jetpacs-row
    (append
     (mapcar
     (lambda (item)
       (pcase-let ((`(,key ,label ,icon) item))
         (let ((on (equal key view)))
           (jetpacs-with-attrs
            (jetpacs-design-pressable
             '("harp.nav-item") (harp-action "harp.open" (list :view key))
             (list (harp-ui-fill
                    (jetpacs-column
                     (harp-ui-styled (if on "harp.nav-indicator-on" "harp.nav-indicator")
                                     (jetpacs-with-attrs
                                      (jetpacs-box (harp-icon icon 18 (harp-ui-color
                                                                        (if on 'on-secondary-container
                                                                          'on-surface-variant))
                                                              label)
                                                   :alignment "center")
                                      :width 50 :height 26))
                     (jetpacs-spacer :height 4)
                     (harp-ui-t (if on "harp.t.nav-selected" "harp.t.nav") label :max-lines 1)
                     :align "center"))))
            :weight 1))))
     '(("journal" "Journals" "regular/sticky-note")
       ("metrics" "Metrics" "regular/chart-bar")
       ("documents" "Documents" "solid/notes-medical")))
     ;; Material's NavigationBar spaces its items 8dp apart.
     '(:spacing 8 :align "top")))))

(defun harp-ui-fab (view)
  "The FloatingActionButton adding a record to VIEW."
  (jetpacs-design-pressable
   '("harp.fab")
   (if (equal view "journal") (harp-action "harp.sheet")
     (harp-action "harp.form" (list :form (pcase view ("metrics" "metric") ("documents" "document")
                                            (_ "entry")))))
   (list (jetpacs-with-attrs (jetpacs-box (harp-icon "solid/plus" 20 (harp-ui-color 'on-primary-container)
                                                     "Add entry")
                                          :alignment "center")
                             :width 56 :height 56))))

(defun harp-ui-tags-by-frequency (profile)
  "PROFILE's tags, most used first, as Harp's tag suggestions order them."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (items (list (cl-loop for j in (alist-get 'journals profile) append (alist-get 'entries j))
                         (alist-get 'documents profile) (alist-get 'metrics profile)
                         (alist-get 'reminders profile)))
      (dolist (item items)
        (dolist (tag (alist-get 'tags item))
          (unless (equal tag "ATTACH") (puthash tag (1+ (gethash tag counts 0)) counts)))))
    (let (tags)
      (maphash (lambda (k _) (push k tags)) counts)
      (sort tags (lambda (a b) (let ((ca (gethash a counts)) (cb (gethash b counts)))
                                 (if (= ca cb) (string< a b) (> ca cb))))))))

(defun harp-ui-text-has-tag (text tag)
  "Whether TEXT carries #TAG, as a whole tag."
  (string-match-p (concat "\\(?:^\\|[[:space:]]\\)#" (regexp-quote tag) "\\(?:$\\|[^[:alnum:]_(-]\\)")
                  text))

(defun harp-ui--field-scope (child)
  "CHILD, an outlined text field, under the entry form's field styles."
  (jetpacs-design-scope
   nil nil (list child)
   :component-styles
   (list (jetpacs-design-component-style "text-field.outlined" '("harp.outlined"))
         (jetpacs-design-component-style "text-field.text" '("harp.t.body"))
         (jetpacs-design-component-style "text-field.label" '("harp.t.field-label"))
         (jetpacs-design-component-style "text-field.placeholder" '("harp.t.field-hint"))
         (jetpacs-design-component-style "text-field.supporting" '("harp.t.field-error")))))

(defun harp-ui-sheet-chip (tag on)
  "A tag suggestion FilterChip for TAG, ON while the text carries it."
  (jetpacs-design-pressable
   (list (if on "harp.suggest-on" "harp.suggest"))
   (harp-action "harp.sheet-tag" (list :tag tag) '("body"))
   (delq nil (list (when on (harp-icon "solid/check" 8 (harp-ui-color 'on-secondary-container)))
                   (when on (jetpacs-spacer :width 8))
                   (harp-ui-t "harp.t.label" (concat "#" tag) :max-lines 1)))))

(defun harp-ui-outlined-button (icon description)
  "An OutlinedButton carrying a 14dp Font Awesome ICON named DESCRIPTION."
  (jetpacs-design-pressable
   '("harp.outlined-button") (harp-action "harp.sheet-attach")
   (list (harp-icon icon 14 (harp-ui-color 'primary) description))))

(defun harp-ui-entry-sheet (profile)
  "JournalEntryForm for PROFILE in its ModalBottomSheet.
The text, tag suggestions, attachment buttons, time and date, Cancel and
Save."
  (let* ((token (plist-get harp-state-sheet :id))
         (entry (and token (plist-get (cdr (assoc token (plist-get harp-state :refs))) :item)))
         (journals (alist-get 'journals profile))
         (journal (nth (min harp-state-journal (max 0 (1- (length journals)))) journals))
         (text (or harp-state-sheet-body (alist-get 'body entry) ""))
         (time (if entry (harp--time-of (alist-get 'datetime entry)) (plist-get harp-state :now)))
         (tags (harp-ui-tags-by-frequency profile))
         (blank (string-blank-p text)))
    (harp-ui-styled
     "harp.sheet"
     (apply
      #'jetpacs-column
      (delq nil
            (list
             (jetpacs-with-attrs
              (harp-ui-t "harp.t.sheet-title"
                         (if entry "Edit entry" (format "Add entry to '%s'" (alist-get 'name journal))))
              :pad '(:bottom 10))
             (jetpacs-with-attrs
              (harp-ui--field-scope
               (jetpacs-text-input (harp-field-id "body") :value text :label "Text"
                                   :hint "Use #hashtags for tagging and #metric-id(number) to log metric values."
                                   :min-lines 6 :max-lines 6 :max-length 16000
                                   :is-error (if blank t :json-false)
                                   :supporting-text (and blank "Text can't be empty")
                                   :autofocus t
                                   :on-change (harp-action "harp.sheet-body" nil '("body"))))
              :fill_fraction 1)
             (when tags
               (jetpacs-column
                (harp-ui-t "harp.t.suggest-label" "Insert tag")
                (jetpacs-spacer :height 4)
                (apply #'jetpacs-row
                       (append (mapcar (lambda (tag) (harp-ui-sheet-chip tag (harp-ui-text-has-tag text tag)))
                                       tags)
                               '(:scroll t :spacing 6)))))
             (jetpacs-row (harp-ui-outlined-button "solid/camera-retro" "Add from camera")
                          (harp-ui-outlined-button "solid/link" "Add file")
                          :spacing 8)
             (jetpacs-row
              (jetpacs-with-attrs
               (harp-ui--field-scope
                (jetpacs-text-input (harp-field-id "time") :value (format-time-string "%H:%M" time)
                                    :label "Time" :single-line t :max-length 5 :trailing-icon "schedule"))
               :weight 1)
              (jetpacs-with-attrs
               (harp-ui--field-scope
                (jetpacs-text-input (harp-field-id "date") :value (format-time-string "%Y-%m-%d" time)
                                    :label "Date" :single-line t :max-length 10
                                    :trailing-icon "calendar_today"))
               :weight 1)
              :spacing 12)
             (jetpacs-row
              (jetpacs-with-attrs
               (jetpacs-design-pressable
                '("harp.outlined-button") (harp-action "harp.sheet-close")
                (list (harp-ui-fill (jetpacs-box (harp-ui-t "harp.t.button-outlined" "Cancel")
                                                 :alignment "center"))))
               :weight 1)
              (jetpacs-with-attrs
               (jetpacs-design-pressable
                '("harp.button") (harp-action "harp.sheet-save" (list :id token :revision (plist-get harp-state :revision))
                                              '("body" "time" "date"))
                (list (harp-ui-fill (jetpacs-box (harp-ui-t "harp.t.button" "Save") :alignment "center"))))
               :weight 1)
              :spacing 12)
             (jetpacs-spacer :height 0)))
      '(:spacing 10)))))

(defun harp-ui-profile-screen (profile view)
  "ProfileScreen for PROFILE on tab VIEW.
The header and filter bar, the tab, the navigation bar and the FAB."
  (list
   :top-bar (jetpacs-with-attrs
             (jetpacs-column (harp-ui-profile-header profile) (harp-ui-filter-bar profile))
             :weight 1)
   :body (apply #'jetpacs-lazy-column
                (mapcar (lambda (node) (harp-ui-styled "harp.content" node))
                        (delq nil
                              (pcase view
                                ("journal" (harp-ui-journal profile))
                                (_ (harp-ui-body view))))))
   :bottom-bar (harp-ui-nav view)
   :fab (harp-ui-fab view)
   :sheet (and harp-state-sheet (equal view "journal")
               (let ((harp-field-scope "harp-entry-sheet")) (harp-ui-entry-sheet profile)))
   :sheet-state (and harp-state-sheet (equal view "journal") "expanded")
   :on-sheet-change (and harp-state-sheet (equal view "journal") (harp-action "harp.sheet-close"))))

(defun harp-ui-screen (route &optional back)
  "Render ROUTE and BACK using Foundation and the current Styles profile."
  (let* ((harp-field-scope (harp-form-scope route))
         (view (or (plist-get route :view) "profiles"))
         (profile (harp-ui-current))
         (slots
          (cond
           ((plist-get route :form)
            (list :top-bar (jetpacs-with-attrs (harp-ui-styled "harp.home" (harp-ui-home-title)) :weight 1)
                  :body (jetpacs-with-attrs
                         (apply #'jetpacs-lazy-column (append (delq nil (harp-ui-form route)) '(:spacing 12)))
                         :padding 16)))
           ((or (equal view "profiles") (null profile)) (harp-ui-home))
           ((member view '("journal" "metrics" "documents")) (harp-ui-profile-screen profile view))
           (t (list :top-bar (jetpacs-with-attrs (harp-ui-styled "harp.home" (harp-ui-home-title)) :weight 1)
                    :body (jetpacs-with-attrs
                           (apply #'jetpacs-lazy-column (append (delq nil (harp-ui-body view)) '(:spacing 12)))
                           :padding 16)))))
         (home (or (equal view "profiles") (null profile)))
         (scaffold (jetpacs-with-semantics
                    (apply #'jetpacs-scaffold (append slots (list :on-back back)))
                    :pane-title "Harp")))
    (harp-design-wrap
     (if home
         ;; The home screen's buttons sit on the background, not a bar.
         (jetpacs-design-scope
          nil nil (list scaffold)
          :component-styles (list (jetpacs-design-component-style "chrome.bottom-bar" '("harp.home-bar"))))
       scaffold)
     harp-state-dark)))

(provide 'harp-ui)
;;; harp-ui.el ends here
