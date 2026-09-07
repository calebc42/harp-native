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
(require 'harp-ui)

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
