;;; build-pack.el --- Regenerate harp-pack.json from live registrations -*- lexical-binding: t; -*-

;; Emit the Harp engine-pack manifest at the repo root, from the app's LIVE
;; action registrations (never a hand-maintained list):
;;
;;   emacs --batch -l emacs/build-pack.el
;;
;; Output:
;;   harp-pack.json  — {pack_id, pack_version, min_jetpacs_api, feature,
;;                      depends, layouts, sources, actions}
;;
;; Loads the Jetpacs core (from the `jetpacs' submodule) and the whole Harp
;; app so every `jetpacs-defaction' has run, then calls `harp-pack-write'.  A
;; committed snapshot plus a regen-and-assert test keep the checked-in JSON
;; honest.

;;; Code:

(let ((here (file-name-directory (or load-file-name buffer-file-name))))
  (dolist (dir '("../jetpacs/emacs/core" "apps/harp"))
    (add-to-list 'load-path (expand-file-name dir here)))
  (require 'harp)
  (require 'harp-pack)
  (let ((out (expand-file-name "../harp-pack.json" here)))
    (harp-pack-write out)
    (message "Wrote %s" out)))

;;; build-pack.el ends here
