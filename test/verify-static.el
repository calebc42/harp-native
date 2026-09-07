;;; -*- lexical-binding: t; -*-
(setq load-prefer-newer t)
(require 'jetpacs-applet-tooling)
(setq jetpacs-applet-tooling-repositories-root
      (file-name-directory (directory-file-name (getenv "JETPACS_ROOT"))))
(let* ((root (getenv "HARP_SOURCE_ROOT"))
       (platform (expand-file-name "emacs" (getenv "JETPACS_ROOT")))
       (workspace (expand-file-name "static-sources" (getenv "HARP_VERIFY_OUTPUT")))
       (applet (expand-file-name "emacs/apps/harp" workspace))
       (providers (expand-file-name "platform" workspace)))
  ;; Static reconciliation reads byte-identical copies of the actual platform
  ;; providers, never an applet relabeled as a platform or an expanded allowlist.
  (make-directory applet t) (make-directory providers t)
  (dolist (pair (list (cons (expand-file-name "emacs/apps/harp" root) applet)
                      (cons platform providers)))
    (dolist (file (directory-files (car pair) t (rx ".el" eos)))
      (copy-file file (expand-file-name (file-name-nondirectory file) (cdr pair)) t)))
  (setq jetpacs-applet-tooling-workspace-root workspace
        jetpacs-applet-tooling-platform-source-directory "platform")
  (dolist (operation '(jetpacs-applet-tooling-inspect-applet
                       jetpacs-applet-tooling-applet-manifest
                       jetpacs-applet-tooling-validate-applet))
    (let ((report (funcall operation "emacs/apps/harp")))
      (with-temp-file (concat (getenv "HARP_VERIFY_OUTPUT") "/" (symbol-name operation) ".txt")
        (insert report))))
  (setq jetpacs-applet-tooling-workspace-root root
        jetpacs-applet-tooling-platform-source-directory nil))
