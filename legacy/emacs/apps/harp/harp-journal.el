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
(require 'harp-ui)

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
