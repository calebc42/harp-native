;;; -*- lexical-binding: t; -*-
;; Explicit trusted test execution; use only after authorizing this source.
(setq load-prefer-newer t)
(require 'jetpacs-applet-tooling)
(require 'harp-current-test)
(require 'jetpacs-devtools)
(setq jetpacs-applet-tooling-workspace-root (getenv "HARP_SOURCE_ROOT")
      jetpacs-applet-tooling-repositories-root (file-name-directory (directory-file-name (getenv "JETPACS_ROOT"))))
(harp-test-with-snapshot
 (with-temp-file (expand-file-name "render-determinism.txt" (getenv "HARP_VERIFY_OUTPUT"))
   (insert (jetpacs-applet-tooling-check-render-determinism "harp")))
 (let ((output (expand-file-name "screens" (getenv "HARP_VERIFY_OUTPUT"))))
   (make-directory output t)
   (dolist (dark '(nil t))
     (let ((harp-state-dark dark))
       (dolist (route (append (mapcar (lambda (view) (list :view view)) harp-views)
                             (list (harp-test-args "entry" (harp-test-entry)))))
         (with-temp-file (expand-file-name
                          (concat (or (plist-get route :view) "entry-form") (if dark "-dark" "-light") ".json") output)
           (set-buffer-multibyte nil)
           (insert (jetpacs-node->canonical-json (harp-ui-screen route (jetpacs-view-switch "profiles"))))))))))
