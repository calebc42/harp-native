;;; harp-design.el --- Harp Foundation style profiles -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "30.1"))
;;; Commentary:
;; App-owned palettes and density; all rendering uses reusable Foundation nodes.
;;; Code:
(require 'jetpacs-components)
(require 'jetpacs-design-profiles)

(defconst harp-design-light
  '(("primary" . "#466A60") ("on_primary" . "#FFFFFF")
    ("secondary" . "#657E75") ("on_secondary" . "#FFFFFF")
    ("background" . "#F5FAF6") ("on_background" . "#212121")
    ("surface" . "#FFFFFF") ("on_surface" . "#212121")
    ("outline" . "#E0E0E0") ("error" . "#B71C1C")
    ("on_error" . "#FFFFFF") ("success" . "#1B5E20") ("warning" . "#8D5900"))
  "Harp light palette, owned by the app.")
(defconst harp-design-dark
  '(("primary" . "#A8CEBC") ("on_primary" . "#212121")
    ("secondary" . "#90A4AE") ("on_secondary" . "#212121")
    ("background" . "#212121") ("on_background" . "#FFFFFF")
    ("surface" . "#242424") ("on_surface" . "#FFFFFF")
    ("outline" . "#424242") ("error" . "#EF9A9A")
    ("on_error" . "#212121") ("success" . "#A5D6A7") ("warning" . "#FFCC80"))
  "Harp dark palette for low-light reading.")

(defun harp-design-profile (dark)
  "Build Harp's immutable Foundation profile, using DARK when non-nil."
  (let* ((palette (if dark harp-design-dark harp-design-light))
         (roles (mapcar (lambda (p) (cons (car p) (jetpacs-design-color (cdr p)))) palette)))
    (jetpacs-design-make-profile
     (if dark "harp.dark" "harp.light") (if dark "Harp Dark" "Harp Light")
     :tokens roles :theme-roles roles
     :typography
     '(("body" . (:family "system" :size 16 :weight 400 :line-height 22 :letter-spacing 0))
       ("title" . (:family "system" :size 20 :weight 500 :line-height 28 :letter-spacing 0))
       ("caption" . (:family "system" :size 13 :weight 400 :line-height 18 :letter-spacing 0))
       ("label" . (:family "system" :size 14 :weight 500 :line-height 20 :letter-spacing 0)))
     :styles
     `(("harp.screen" . ,(jetpacs-design-style
                            `(("background_color" . ,(jetpacs-design-token "surface"))
                              ("content_color" . ,(jetpacs-design-token "on_surface"))
                              ("fill_width" . ,(jetpacs-design-boolean t)))))
       ("harp.toolbar" . ,(jetpacs-design-style
                             `(("background_color" . ,(jetpacs-design-color (if dark "#292929" "#466A60")))
                               ("content_color" . ,(jetpacs-design-color "#FFFFFF"))
                               ("padding_horizontal" . ,(jetpacs-design-dimension 8))
                               ("min_height" . ,(jetpacs-design-dimension 56))
                               ("fill_width" . ,(jetpacs-design-boolean t)))))
       ("harp.row" . ,(jetpacs-design-style
                         `(("corner_radius" . ,(jetpacs-design-dimension 0))
                           ("padding_horizontal" . ,(jetpacs-design-dimension 12))
                           ("padding_vertical" . ,(jetpacs-design-dimension 8))
                           ("min_height" . ,(jetpacs-design-dimension 48))
                           ("fill_width" . ,(jetpacs-design-boolean t)))
                         :rules (list (jetpacs-design-rule "pressed"
                                `(("background_color" . ,(jetpacs-design-token "outline")))))))
       ("harp.done" . ,(jetpacs-design-style `(("alpha" . ,(jetpacs-design-number 0.55)))))
       ("harp.form" . ,(jetpacs-design-style
                          `(("padding" . ,(jetpacs-design-dimension 16))
                            ("fill_width" . ,(jetpacs-design-boolean t))))))
     :component-styles
     '(("text.body" . ("typography.body")) ("text.title" . ("typography.title"))
       ("text.caption" . ("typography.caption")) ("text.label" . ("typography.label"))
       ("list-item.container" . ("harp.row"))
       ("text-field.label" . ("typography.caption"))
       ("text-field.text" . ("typography.body"))
       ("section-header.title" . ("typography.label"))
       ("list-item.title" . ("typography.body"))
       ("list-item.subtitle" . ("typography.caption"))))))

(defun harp-design-register ()
  "Register both Harp style presets idempotently."
  (dolist (dark '(nil t)) (jetpacs-design-register-profile (harp-design-profile dark))))

(defun harp-design-wrap (screen dark)
  "Wrap SCREEN in the selected profile, defaulting to DARK's Harp preset."
  (let* ((active (jetpacs-design-active-profile))
         (profile (if (assoc "harp.row" (plist-get active :styles)) active
                    (jetpacs-design-profile-get (if dark "harp.dark" "harp.light")))))
    (jetpacs-design-profile-scope profile (list screen))))

(provide 'harp-design)
;;; harp-design.el ends here
