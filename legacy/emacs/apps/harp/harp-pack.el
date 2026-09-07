;;; harp-pack.el --- The Harp engine-pack manifest -*- lexical-binding: t; -*-

;; Harp ships as a jetpacs engine pack: the Tier-1 elisp plus a
;; machine-readable manifest, `harp-pack.json', telling the launcher/composer
;; what it exposes without reading elisp — the composer-facing ACTIONS its
;; cards fire, and the Emacs packages it relies on.  The manifest is built from
;; LIVE registrations (`jetpacs-action-catalog'), so it can never drift from
;; what the app actually registers; `emacs/build-pack.el' regenerates the
;; committed JSON and a test asserts the two agree.
;;
;; Harp registers no declarative data SOURCES or layouts (its screens are
;; `:builder' views over the org profile), so those arrays are empty.

;;; Code:

(require 'cl-lib)
(require 'json)                          ; json-pretty-print-buffer
(require 'jetpacs-surfaces)              ; jetpacs-action-catalog

(defconst harp-pack-id "harp"
  "The pack id the launcher keys Harp by.")

(defconst harp-pack-version "0.1.0"
  "Version of the Harp pack (distinct from `jetpacs-api-version').")

(defconst harp-pack-min-jetpacs-api "1.5.0"
  "Minimum jetpacs api the Harp pack requires.
Optional capabilities (owner-scoped reminders, chart / month_grid nodes) are
negotiated at runtime and degrade, so they do not raise the floor.")

(defconst harp-pack-depends
  '(((name . "org")    (min_version . "9.6"))
    ((name . "cl-lib") (min_version . "1.0")))
  "Emacs packages the Harp engine relies on, for the composer to install.")

(defun harp-pack--sort-by (key entries)
  "ENTRIES (a list of alists) sorted by their KEY value, for a stable manifest."
  (sort (copy-sequence entries)
        (lambda (a b) (string< (format "%s" (alist-get key a))
                               (format "%s" (alist-get key b))))))

(defun harp-pack-manifest ()
  "The Harp engine-pack manifest, built from live registrations.
A JSON-serializable alist; actions are name-sorted so the generated
`harp-pack.json' is byte-stable."
  (list (cons 'pack_id         harp-pack-id)
        (cons 'pack_version    harp-pack-version)
        (cons 'min_jetpacs_api harp-pack-min-jetpacs-api)
        (cons 'feature         harp-pack-id)
        (cons 'depends         (vconcat harp-pack-depends))
        (cons 'layouts         (vconcat nil))
        (cons 'sources         (vconcat nil))
        (cons 'actions         (vconcat (harp-pack--sort-by
                                         'action (jetpacs-action-catalog "harp"))))))

(defun harp-pack-json ()
  "The manifest as pretty-printed, newline-terminated JSON text."
  (with-temp-buffer
    (insert (json-serialize (harp-pack-manifest)
                            :null-object :null :false-object :false))
    (json-pretty-print-buffer)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (buffer-string)))

(defun harp-pack-write (file)
  "Write the manifest JSON to FILE.  Returns FILE."
  (let ((coding-system-for-write 'utf-8))
    (with-temp-file file (insert (harp-pack-json))))
  file)

(provide 'harp-pack)
;;; harp-pack.el ends here
