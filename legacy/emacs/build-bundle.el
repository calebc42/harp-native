;;; build-bundle.el --- Regenerate the Harp single-file bundle -*- lexical-binding: t; -*-

;; Concatenate the Harp app sources into one loadable bundle at the repo
;; root.  Run after editing any source file:
;;
;;   emacs --batch -l emacs/build-bundle.el
;;
;; Output:
;;   harp.el  — the Harp Tier-1 app (from emacs/apps/harp/).
;;
;; The bundle does NOT inline the Jetpacs core.  It opens with
;; `(require 'jetpacs-core)', so the foundation bundle (jetpacs-core.el, from
;; the separate jetpacs repo / the `jetpacs' submodule) must be on
;; `load-path' first.  That is the point of the split: Harp is pure elisp
;; that `(require 's the core, never a copy of it.
;;
;; Files are emitted in dependency order.  Because every source ends with a
;; `(provide 'FEATURE)', the inter-file `(require ...)' forms for Harp's own
;; features become no-ops once the providing chunk has loaded earlier in the
;; bundle; the core `(require 'jetpacs-...)' forms resolve against the
;; already-loaded jetpacs-core.  External requires (org, org-ql, cl-lib, ...)
;; resolve normally.

;;; Code:

(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       ;; Dependency order.  Do not reorder without re-checking the require
       ;; graph.  Harp's app entry (harp.el) comes last: it requires the
       ;; rest.
       (app-files '("apps/harp/harp-metrics-parse.el"
                    "apps/harp/harp-profile.el"
                    "apps/harp/harp-ui.el"
                    "apps/harp/harp-write.el"
                    "apps/harp/harp-med.el"
                    "apps/harp/harp-med-schedule.el"
                    "apps/harp/harp-med-log.el"
                    "apps/harp/harp-med-view.el"
                    "apps/harp/harp-med-form.el"
                    "apps/harp/harp-med-notify.el"
                    "apps/harp/harp-home.el"
                    "apps/harp/harp-journal.el"
                    "apps/harp/harp-documents.el"
                    "apps/harp/harp-metrics.el"
                    "apps/harp/harp-reminders.el"
                    "apps/harp/harp-form.el"
                    "apps/harp/harp-export.el"
                    "apps/harp/harp-demo.el"
                    "apps/harp/harp-settings.el"
                    "apps/harp/harp-pack.el"
                    "apps/harp/harp.el"))
       (out (expand-file-name "../harp.el" here)))
  (with-temp-file out
    (insert ";;; harp.el --- Harp Personal Health Record (Jetpacs Tier-1 app), single-file bundle -*- lexical-binding: t; -*-\n"
            ";;\n"
            ";; GENERATED FILE -- do not edit by hand.\n"
            ";; Produced by emacs/build-bundle.el from the emacs/apps/ sources.\n"
            ";; Concatenated in dependency order; each part keeps its own `provide',\n"
            ";; and the bundle-internal `require' forms are commented out so the\n"
            ";; file BYTE-COMPILES standalone: the foundation compiles an adopted\n"
            ";; bundle before loading it, when none of these features exist yet —\n"
            ";; a surviving hard require is a compile error and a broken .elc.\n"
            ";;\n"
            ";; Requires the Jetpacs core (jetpacs-core.el) on `load-path' first.\n"
            ";;\n"
            ";;; Code:\n\n"
            "(require 'jetpacs-core)\n\n")
    ;; Features this bundle itself provides: a hard `(require 'X)' on one of
    ;; these must not survive into the output.  At load time it was already a
    ;; no-op (the providing chunk sits earlier in the file); at compile time
    ;; it would hit the load-path, where these features never exist as files.
    ;; Soft requires — (require 'x nil t) — are left alone.
    (let ((internal (mapcar (lambda (f) (file-name-base f)) app-files)))
      (dolist (f app-files)
        (insert ";;; ==================================================================\n"
                (format ";;; BEGIN %s\n" f)
                ";;; ==================================================================\n\n")
        (let ((start (point)))
          (insert-file-contents (expand-file-name f here))
          (goto-char (point-max))
          (save-excursion
            (goto-char start)
            (while (re-search-forward
                    "^(require '\\([-a-z]+\\))[ \t]*\\(?:;.*\\)?$" nil t)
              (when (member (match-string 1) internal)
                (replace-match ";; \\& — bundle-internal, provided above"
                               t nil)))))
        (insert "\n")))
    (insert "(provide 'harp)\n"
            ";;; harp.el ends here\n"))
  (message "Wrote %s" out))

;;; build-bundle.el ends here
