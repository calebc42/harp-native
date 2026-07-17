;;; build-demo.el --- Regenerate emacs/apps/harp/harp-demo.el from the fixtures -*- lexical-binding: t; -*-

;; Emit the demo seeder (`harp-demo.el') by embedding the sample profile Org
;; files under test/fixtures/ as string constants:
;;
;;   emacs --batch -l emacs/build-demo.el
;;
;; harp-demo.el is generated (not hand-edited) so the embedded sample corpus
;; stays byte-identical to the fixtures the tests exercise.  Edit a sample by
;; editing its fixture and re-running this; edit the seeder's behaviour here in
;; the template below.

;;; Code:

(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       (repo (expand-file-name ".." here))
       (profiles '(("diego-recovery"  . "test/fixtures/diego.org")
                   ("kenji-and-luna"  . "test/fixtures/kenji-luna.org")
                   ("bruno-health"    . "test/fixtures/bruno.org")))
       (out (expand-file-name "apps/harp/harp-demo.el" here)))
  (with-temp-file out
    (insert ";;; harp-demo.el --- Bundled sample profiles for trying Harp out -*- lexical-binding: t; -*-

;; GENERATED FILE -- do not edit by hand.  Produced by emacs/build-demo.el from
;; the test/fixtures/*.org samples.
;;
;; `harp-demo-setup' writes a few ready-made profiles (a person, a pair of cats,
;; a dog) into `harp-directory' and opens the first, so a fresh install has
;; something to explore.  The human profile carries a couple of medications.
;; Attachment media are not bundled, so attachment chips show file names only.

;;; Code:

(require 'cl-lib)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
(require 'harp-profile)
(require 'harp-ui)

(defconst harp-demo--profiles
  (list
")
    (dolist (p profiles)
      (let ((text (with-temp-buffer
                    (insert-file-contents (expand-file-name (cdr p) repo))
                    (buffer-string))))
        (when (string-match-p "[\"\\]" text)
          (error "Unsafe character in %s — cannot embed raw" (cdr p)))
        (insert (format "   (cons %S\n\"%s\")\n" (car p) text))))
    (insert "   )
  \"Sample profiles as (SLUG . ORG-TEXT), written by `harp-demo-setup'.\")

(defun harp-demo--file (slug)
  (expand-file-name (concat slug \".org\") harp-directory))

(defun harp-demo--exists-p ()
  \"Non-nil when any bundled sample profile is already on disk.\"
  (cl-some (lambda (p) (file-exists-p (harp-demo--file (car p)))) harp-demo--profiles))

(defun harp-demo-setup (&optional force)
  \"Write the bundled sample profiles into `harp-directory'; open the first.
Existing files are left alone unless FORCE (or a prefix arg) is non-nil, so a
re-run refreshes the samples to the latest bundled version.  Returns the
opened profile file.\"
  (interactive \"P\")
  (make-directory harp-directory t)
  (let (first)
    (dolist (p harp-demo--profiles)
      (let ((file (harp-demo--file (car p))))
        (when (or force (not (file-exists-p file)))
          (with-temp-file file (insert (cdr p)))
          ;; `with-temp-file' writes disk directly; drop any stale buffer
          ;; still visiting this path so the next read reflects the refresh.
          (when-let ((buf (find-buffer-visiting file)))
            (with-current-buffer buf (set-buffer-modified-p nil))
            (kill-buffer buf)))
        (unless first (setq first file))))
    (harp-ui-invalidate)
    (when first (harp-ui-set-current first))
    first))

(with-jetpacs-owner \"harp\"
  (jetpacs-defaction \"harp.demo.setup\"
    (lambda (_args _)
      ;; When samples already exist, offer to refresh them to the latest
      ;; bundled version (bridged to a dialog on the phone).  Declining must
      ;; leave the current profile alone — never switch or claim a write.
      (cond
       ((not (harp-demo--exists-p))
        (harp-demo-setup)
        (jetpacs-shell-notify \"Sample profiles added\")
        (jetpacs-shell-set-current-tab \"harp.journal\"))
       ((yes-or-no-p \"Sample profiles already exist — refresh to the latest samples? \")
        (harp-demo-setup t)
        (jetpacs-shell-notify \"Sample profiles refreshed\")
        (jetpacs-shell-set-current-tab \"harp.journal\"))
       (t
        (jetpacs-shell-notify \"Kept your existing profiles\")
        (jetpacs-shell-push))))
    :doc \"Write (or refresh) the bundled sample profiles and open the first.\"))

(provide 'harp-demo)
;;; harp-demo.el ends here
"))
  (message "Wrote %s" out))

;;; build-demo.el ends here
