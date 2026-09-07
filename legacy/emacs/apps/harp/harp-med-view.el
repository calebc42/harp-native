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
(require 'harp-ui)
(require 'harp-med)
(require 'harp-med-schedule)

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
