;;; harp-state.el --- Bounded Harp snapshots and references -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1"))
;;; Commentary:
;; Actions produce snapshots; rendering never performs I/O or mints references.
;;; Code:
(require 'harp-profile)
(declare-function harp-med-schedule-pending "harp-med-schedule" (med &optional date))
(defvar harp-state nil "Published, immutable profile snapshot.")
(defvar harp-ui--current-file nil "Emacs-owned current profile path.")
(defvar harp-state-dark nil "Whether to use the dark app preset.")
(defvar harp-hide-private t "Whether private records are hidden from screens.")
(defvar harp-state-tag nil "Current tag filter, or nil.")
(defvar harp-state-page 0 "Current record page.")
(defvar harp-state-journal 0 "Index of the journal the Journals tab shows.")
(defvar harp-state-query "" "The filter bar's text search.")
(defvar harp-state-filter-open nil "Non-nil while the filter bar shows the tag chips.")
(defvar harp-state-sort "date_desc" "The Journals tab's order: date_desc or date_asc.")
(defvar harp-state-sheet nil
  "The entry form open in the bottom sheet: (:id TOKEN), TOKEN nil for a new entry.")
(defvar harp-state-sheet-body nil "The entry form's text as last captured, or nil.")
(defvar harp-state-route nil "The route last opened, as `harp--open' received it.")
(defvar harp-state-epoch nil "Producer-minted process identity preventing token reuse after restart.")
(defvar harp-state-generation 0 "Monotonic process-local snapshot generation.")
(defconst harp-page-size 20 "Maximum records rendered on one page.")
(define-error 'harp-stale "Harp snapshot is stale")

(defun harp-safe-file (file)
  "Validate FILE as a bounded, local regular file inside `harp-directory'."
  (unless (and (stringp file) (not (file-remote-p file))
               (not (file-remote-p harp-directory))) (error "Local profile required"))
  (let ((true (file-truename file)) (root (file-name-as-directory (file-truename harp-directory))))
    (unless (and (file-in-directory-p true root) (file-regular-p true)
                 (<= (file-attribute-size (file-attributes true)) (* 1024 1024)))
      (error "Profile outside root or larger than 1 MiB"))
    true))

(defun harp-heading-ref ()
  "Adapt the real Org engine's internal reference to the recovered domain alist."
  (let ((ref (ebp-org-ref-at-point)))
    (mapcar (lambda (key) (cons key (plist-get ref (intern (concat ":" (symbol-name key))))))
            '(id file pos headline))))

(defun harp-ref-plist (ref)
  "Translate internal domain REF for the owning Org engine."
  (let (out) (dolist (pair ref) (setq out (plist-put out (intern (concat ":" (symbol-name (car pair)))) (cdr pair)))) out))

(defun harp-file-revision (file)
  "Hash bounded FILE after rejecting unsaved edits in its visiting buffer."
  (setq file (harp-safe-file file))
  (when-let* ((buf (get-file-buffer file)))
    (when (buffer-modified-p buf) (error "Save the open profile buffer first")))
  (with-temp-buffer (insert-file-contents-literally file) (secure-hash 'sha256 (current-buffer))))

(defun harp-discover ()
  "Discover at most 32 profiles in a bounded tree without following symlinks."
  (unless (file-remote-p harp-directory)
    (let ((todo (and (file-directory-p harp-directory) (list harp-directory)))
          (seen 0) files)
      (while todo
        (let ((dir (pop todo)))
          (dolist (file (directory-files dir t directory-files-no-dot-files-regexp nil 513))
            (when (> (cl-incf seen) 512) (error "Harp directory exceeds 512 entries"))
            (unless (file-symlink-p file)
              (cond ((file-directory-p file) (push file todo))
                    ((and (string-suffix-p ".org" file)
                          (not (string-prefix-p "harp-export-" (file-name-nondirectory file)))
                          (harp-profile-file-p file))
                     (push (harp-safe-file file) files)
                     (when (> (length files) 32) (error "Harp supports 32 profiles"))))))))
      (sort files #'string<))))

(defun harp-refresh (&optional now)
  "Publish a bounded snapshot at NOW, defaulting to the current clock.
Only this producer reads files and computes dose schedules."
  (unless harp-state-epoch
    (setq harp-state-epoch (substring (secure-hash 'sha256 (format "%s-%s" (emacs-pid) (current-time))) 0 16)))
  (let* ((now (or now (current-time))) (files (harp-discover))
         (generation (cl-incf harp-state-generation)) (refs nil)
         (profiles (mapcar (lambda (file)
                             (let ((info (harp-profile-info file)))
                               (setf (alist-get 'token info) (format "hp-%s-%d-%d" harp-state-epoch generation (length refs)))
                               (push (cons (alist-get 'token info) (list :kind "profile" :file file :revision (harp-file-revision file) :item info)) refs)
                               info)) files))
         (file (and (member harp-ui--current-file files) harp-ui--current-file))
         (revision (and file (harp-file-revision file)))
         (profile (and file (harp-profile-read file))) doses)
    (when profile
      (cl-labels ((record (item kind)
                   (let ((token (format "hr-%s-%d-%d" harp-state-epoch generation (length refs))))
                     (nconc item (list (cons 'token token)))
                     (push (cons token (list :kind kind :file file :revision revision :item item)) refs))))
        (dolist (journal (alist-get 'journals profile))
          (dolist (item (alist-get 'entries journal))
            (nconc item (list (cons 'journal (alist-get 'name journal)))) (record item "entry")))
        (dolist (pair '((documents . "document") (metrics . "metric") (reminders . "reminder") (medications . "medication")))
          (dolist (item (alist-get (car pair) profile)) (record item (cdr pair))))
      (when (> (length refs) 2000) (error "Harp supports 2000 records per snapshot"))
      (dolist (med (alist-get 'medications profile))
        (dolist (dose (harp-med-schedule-pending med now))
          (let ((token (format "hd-%s-%d-%d" harp-state-epoch generation (length refs))))
            (push (cons token (list :kind "dose" :file file :revision revision :item med :dose dose)) refs)
            (push (list :token token :med med :dose dose) doses)
            (when (> (length doses) 200) (error "Harp supports 200 doses per day")))))))
    (setq harp-state (list :profiles profiles :profile profile :file file :revision revision :refs refs
                           :now now :doses (nreverse doses)))
    harp-state))

(defun harp-resolve (token &optional kind check)
  "Resolve opaque TOKEN, optionally requiring KIND and freshness CHECK."
  (let ((ref (and (stringp token) (cdr (assoc token (plist-get harp-state :refs))))))
    (unless ref (signal 'harp-stale nil))
    (when (and kind (not (equal kind (plist-get ref :kind)))) (error "Wrong record kind"))
    (when check
      (unless (equal (plist-get ref :revision) (harp-file-revision (plist-get ref :file)))
        (signal 'harp-stale nil)))
    ref))

(defun harp-ui-current ()
  "Return the already published current profile without reading files."
  (plist-get harp-state :profile))

(defun harp-ui-invalidate ()
  "Mark no cache dirty: snapshots change only through `harp-refresh'."
  nil)

(defun harp-visible-p (item)
  "Whether ITEM passes the explicit privacy and tag filters."
  (and (not (and harp-hide-private (alist-get 'private item)))
       (or (null harp-state-tag) (member harp-state-tag (alist-get 'tags item)))))

(provide 'harp-state)
;;; harp-state.el ends here
