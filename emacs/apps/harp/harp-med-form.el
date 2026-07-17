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
(require 'harp-ui)
(require 'harp-write)
(require 'harp-med)
(require 'harp-med-view)                ; harp-med-view--detail-id (return target)

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
