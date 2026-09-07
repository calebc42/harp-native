;;; harp-settings.el --- Harp settings section -*- lexical-binding: t; -*-

;; Exposes Harp's defcustoms on the stock Emacs Settings screen, rendered from
;; their schemas.  The registry is the wire allowlist: only these symbols can
;; be set from the phone, values are validated against their `custom-type', and
;; changing one drops the read memo and re-renders.  Mirrors harp-kmp's
;; AppSettingsScreen (OCR settings are omitted — OCR is deferred).

;;; Code:

(require 'jetpacs-settings)
(require 'jetpacs-shell)
(require 'harp-ui)
(require 'harp-profile)                 ; harp-directory
(require 'harp-export)                  ; harp-export-directory

(defun harp-settings--after-set (symbol _value)
  "Drop caches and re-render after a Harp setting SYMBOL changes."
  (when (memq symbol '(harp-directory harp-export-directory harp-hide-private))
    (harp-ui-invalidate)))

(add-hook 'jetpacs-settings-after-set-hook #'harp-settings--after-set)

(with-jetpacs-owner "harp"
  (jetpacs-settings-register-section
   "Harp"
   '((harp-directory        :label "Profiles directory")
     (harp-export-directory  :label "HTML export directory")
     (harp-hide-private      :label "Hide private items"))))

(provide 'harp-settings)
;;; harp-settings.el ends here
