;;; harp-design.el --- Harp Foundation style profiles -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "30.1"))
;;; Commentary:
;; App-owned palettes, type scale and component styles; all rendering uses
;; reusable Foundation nodes.  The look is upstream Harp 1.6.16, a Compose
;; Multiplatform app on Material 3 with dynamic colors: its colorScheme
;; roles as drawn on the reference Pixel Tablet, the Material type scale,
;; and the dimensions of its composables (ProfileScreen, FilterBar,
;; JournalEntryView, DTStampView, MetricValueChip, AssetChip).
;;; Code:
(require 'cl-lib)
(require 'jetpacs-components)
(require 'jetpacs-design-profiles)

(defconst harp-design-light-colors
  '((background . "#F8F9FE") (on-surface . "#2D333A") (on-surface-variant . "#5A6067")
    (outline . "#757B83") (outline-variant . "#C1C7CF")
    (primary . "#396286") (on-primary . "#F6F9FF")
    (primary-container . "#A4CDF6") (on-primary-container . "#174567")
    (secondary . "#535F70") (on-secondary . "#FFFFFF")
    (secondary-container . "#D5E4F7") (on-secondary-container . "#455362")
    (tertiary . "#6B5778") (on-tertiary . "#FFFFFF")
    (tertiary-container . "#F2DAFF") (on-tertiary-container . "#251431")
    (error . "#BA1A1A") (error-container . "#FFDAD6") (on-error-container . "#410002")
    (surface-variant . "#DDE3EB") (surface-container-low . "#F1F4FA")
    (surface-container . "#EBEEF5") (filter-bar . "#E8ECF3") (filter-border . "#E1E5EC")
    (search-border . "#CDD0D6") (search-hint . "#93989F") (gray . "#888888")
    (nav-selected . "#526170"))
  "Harp's light Material 3 roles as the Pixel Tablet draws them.
`filter-bar' is surface-variant at 60% over the background, as FilterBar
paints it; `filter-border' and `search-border' are the outline at 6% and
30% over that.")

(defconst harp-design-dark-colors
  '((background . "#111418") (on-surface . "#E1E2E8") (on-surface-variant . "#C1C7CF")
    (outline . "#8B9199") (outline-variant . "#41474E")
    (primary . "#A4CAF3") (on-primary . "#003353")
    (primary-container . "#1F4A6C") (on-primary-container . "#D0E4FF")
    (secondary . "#BAC8DA") (on-secondary . "#243140")
    (secondary-container . "#3B4858") (on-secondary-container . "#D6E4F7")
    (tertiary . "#D6BEE4") (on-tertiary . "#3B2948")
    (tertiary-container . "#523F5F") (on-tertiary-container . "#F2DAFF")
    (error . "#FFB4AB") (error-container . "#93000A") (on-error-container . "#FFDAD6")
    (surface-variant . "#41474E") (surface-container-low . "#191C20")
    (surface-container . "#1D2024") (filter-bar . "#2C3136") (filter-border . "#32373C")
    (search-border . "#4B5157") (search-hint . "#8D9399") (gray . "#888888")
    (nav-selected . "#D6E4F7"))
  "Harp's dark roles: the dark scheme of the same seed, not yet measured.")

(defun harp-design-color (name &optional dark)
  "Harp color NAME for DARK or the light palette."
  (alist-get name (if dark harp-design-dark-colors harp-design-light-colors)))

(defconst harp-design-type
  '((display-medium 45 52 400 0) (display-small 36 44 400 0)
    (headline-medium 28 36 400 0) (headline-small 24 32 400 0) (title-large 22 28 400 0)
    (title-medium 16 24 500 0.15) (body-large 16 24 400 0.5)
    (body-medium 14 20 400 0.25) (body-small 12 16 400 0.4) (label-large 14 20 500 0.1)
    (label-medium 12 16 500 0.5) (label-small 11 16 500 0.5))
  "The Material 3 type scale: (NAME SIZE LINE-HEIGHT WEIGHT LETTER-SPACING).")

(defun harp-design--pairs (plist)
  "Design properties from PLIST of :property value, numbers as dimensions."
  (cl-loop for (key value) on plist by #'cddr
           collect (cons (substring (symbol-name key) 1)
                         (cond ((numberp value) (jetpacs-design-dimension value))
                               ((eq value t) (jetpacs-design-boolean t))
                               ((and (stringp value) (string-prefix-p "#" value))
                                (jetpacs-design-color value))
                               (t value)))))

(defun harp-design--box (&rest pairs)
  "A style from keyword PAIRS."
  (jetpacs-design-style (harp-design--pairs pairs)))

(defun harp-design--type (name color &rest pairs)
  "Material type style NAME in COLOR plus keyword PAIRS.
Every line keeps its full line height with the text centred, as the
Material type scale lays text out."
  (pcase-let ((`(,_ ,size ,line ,weight ,spacing) (assq name harp-design-type)))
    (let ((extra (harp-design--pairs pairs)))
      (jetpacs-design-style
       (append (cl-remove-if (lambda (cell) (assoc (car cell) extra))
                             `(("font_size" . ,(jetpacs-design-dimension size))
                               ("line_height" . ,(jetpacs-design-dimension line))
                               ("letter_spacing" . ,(jetpacs-design-dimension spacing))
                               ("font_weight" . ,(jetpacs-design-font-weight weight))
                               ("line_height_centered" . ,(jetpacs-design-boolean t))
                               ("content_color" . ,(jetpacs-design-color color))))
               extra)))))

(defun harp-design-profile (dark)
  "Build Harp's immutable Foundation profile, using DARK when non-nil."
  (let* ((c (lambda (name) (harp-design-color name dark)))
         (roles `(("primary" . ,(funcall c 'primary)) ("on_primary" . ,(funcall c 'on-primary))
                  ("secondary" . ,(funcall c 'secondary)) ("on_secondary" . ,(funcall c 'on-secondary))
                  ("background" . ,(funcall c 'background)) ("on_background" . ,(funcall c 'on-surface))
                  ("surface" . ,(funcall c 'background)) ("on_surface" . ,(funcall c 'on-surface))
                  ("outline" . ,(funcall c 'outline)) ("error" . ,(funcall c 'error))
                  ("on_error" . "#FFFFFF") ("success" . "#1B5E20") ("warning" . "#8D5900")))
         (roles (mapcar (lambda (p) (cons (car p) (jetpacs-design-color (cdr p)))) roles)))
    (jetpacs-design-make-profile
     (if dark "harp.dark" "harp.light") (if dark "Harp Dark" "Harp Light")
     :tokens roles :theme-roles roles
     :typography
     '(("body" . (:family "system" :size 16 :weight 400 :line-height 24 :letter-spacing 0.5))
       ("title" . (:family "system" :size 22 :weight 400 :line-height 28 :letter-spacing 0))
       ("caption" . (:family "system" :size 12 :weight 500 :line-height 16 :letter-spacing 0.5))
       ("label" . (:family "system" :size 14 :weight 500 :line-height 20 :letter-spacing 0.1)))
     :styles
     `(;; Type: the Material scale in the roles Harp's composables use.
       ("harp.t.title" . ,(harp-design--type 'display-medium (funcall c 'outline)
                                             :font_weight (jetpacs-design-font-weight 700)
                                             :italic t))
       ("harp.t.title-small" . ,(harp-design--type 'display-small (funcall c 'outline)
                                                   :font_weight (jetpacs-design-font-weight 700)
                                                   :italic t))
       ("harp.t.name" . ,(harp-design--type 'display-small (funcall c 'on-surface)))
       ("harp.t.section" . ,(harp-design--type 'title-medium (funcall c 'on-surface)))
       ("harp.t.profile" . ,(harp-design--type 'title-large (funcall c 'on-surface)
                                               :font_weight (jetpacs-design-font-weight 600)))
       ("harp.t.label" . ,(harp-design--type 'label-medium (funcall c 'on-surface)))
       ("harp.t.label-variant" . ,(harp-design--type 'label-medium (funcall c 'on-surface-variant)))
       ("harp.t.filter" . ,(harp-design--type 'label-medium (funcall c 'primary)))
       ("harp.t.search-hint" . ,(harp-design--type 'label-medium (funcall c 'search-hint)))
       ("harp.t.body" . ,(harp-design--type 'body-large (funcall c 'on-surface)))
       ("harp.t.body-variant" . ,(harp-design--type 'body-large (funcall c 'on-surface-variant)))
       ("harp.t.button" . ,(harp-design--type 'label-large (funcall c 'on-primary)))
       ("harp.t.chip" . ,(harp-design--type 'label-large (funcall c 'on-surface)))
       ("harp.t.metric" . ,(harp-design--type 'body-large (funcall c 'on-secondary-container)))
       ("harp.t.attach" . ,(harp-design--type 'label-medium (funcall c 'on-surface-variant)
                                              :font_family (jetpacs-design-font-family "system-mono")
                                              :middle_ellipsis t))
       ("harp.t.month" . ,(harp-design--type 'label-medium (funcall c 'on-primary)
                                             :text_align (jetpacs-design-text-align "center")))
       ("harp.t.time-label" . ,(harp-design--type 'label-small (funcall c 'on-primary)
                                                  :text_align (jetpacs-design-text-align "center")))
       ("harp.t.day" . ,(harp-design--type 'headline-medium (funcall c 'on-surface)))
       ("harp.t.year" . ,(harp-design--type 'label-small (funcall c 'outline)))
       ("harp.t.time" . ,(harp-design--type 'title-medium (funcall c 'on-surface)))
       ("harp.t.nav" . ,(harp-design--type 'label-medium (funcall c 'on-surface-variant)
                                           :text_align (jetpacs-design-text-align "center")))
       ("harp.t.nav-selected" . ,(harp-design--type 'label-medium (funcall c 'nav-selected)
                                                    :text_align (jetpacs-design-text-align "center")))
       ("harp.t.placeholder" . ,(harp-design--type 'body-large (funcall c 'outline)))
       ;; Screens: Surface(background) with 20dp sides.
       ("harp.screen" . ,(harp-design--box :background_color (funcall c 'background)
                                           :fill_width t))
       ("harp.home-bar" . ,(harp-design--box :background_color (funcall c 'background)
                                             :padding 0))
       ("harp.home" . ,(harp-design--box :padding_horizontal 20 :fill_width t))
       ("harp.header" . ,(harp-design--box :padding_horizontal 20 :fill_width t))
       ("harp.name-row" . ,(harp-design--box :padding_vertical 10))
       ;; A 48dp IconButton, square, with its glyph centred.
       ("harp.icon-button" . ,(harp-design--box :center_vertically t :width 48 :height 48 :corner_radius 24))
       ;; Home: ElevatedCard(shape small) rows and the two filled buttons.
       ("harp.profile-card" . ,(harp-design--box :background_color (funcall c 'surface-container-low)
                                                 :corner_radius 8 :elevation 1 :fill_width t))
       ("harp.row" . ,(harp-design--box :center_vertically t :min_height 0 :fill_width t))
       ("harp.center" . ,(harp-design--box :center_vertically t))
       ("harp.button" . ,(harp-design--box :center_vertically t :background_color (funcall c 'primary)
                                           :corner_radius 20 :min_height 40
                                           :padding_horizontal 24 :fill_width t))
       ;; FilterBar: the tinted band, its 1dp edge, and the search field.
       ("harp.filter-bar" . ,(harp-design--box :background_color (funcall c 'filter-bar)
                                               :border_width 1 :border_color (funcall c 'filter-border)
                                               :padding_vertical 2 :padding_horizontal 19
                                               :fill_width t))
       ("harp.filter-button" . ,(harp-design--box :center_vertically t :padding 4 :min_height 46 :corner_radius 20))
       ("harp.search" . ,(harp-design--box :width 180 :height 32 :border_width 1
                                           :border_color (funcall c 'search-border)
                                           :corner_radius 6 :padding_horizontal 8))
       ("harp.sort-button" . ,(harp-design--box :center_vertically t :width 26 :height 26 :corner_radius 13))
       ;; JournalView: the selector Card and the entry list.
       ("harp.content" . ,(harp-design--box :padding_horizontal 20 :fill_width t))
       ("harp.selector" . ,(harp-design--box :center_vertically t :background_color (funcall c 'surface-variant)
                                             :corner_radius 12 :padding 16 :fill_width t))
       ("harp.count" . ,(harp-design--box :padding_vertical 8))
       ;; JournalEntryView: ElevatedCard(12dp) with a 10dp inset row.
       ("harp.entry" . ,(harp-design--box :background_color (funcall c 'surface-container-low)
                                          :corner_radius 12 :elevation 1 :padding 10
                                          :fill_width t))
       ("harp.entry-body" . ,(harp-design--box :padding_start 15))
       ;; DTStampView: 64dp elevated cards on surface-variant.
       ("harp.stamp" . ,(harp-design--box :width 64 :corner_radius 6 :elevation 1
                                          :background_color (funcall c 'surface-variant)))
       ("harp.stamp-head" . ,(harp-design--box :background_color (funcall c 'primary)
                                               :padding_vertical 2 :fill_width t))
       ,@(mapcar (lambda (role)
                   (cons (format "harp.bg.%s" role)
                         (harp-design--box :background_color (funcall c role))))
                 '(primary secondary tertiary primary-container secondary-container
                   tertiary-container error-container))
       ("harp.stamp-day" . ,(harp-design--box :padding_vertical 2))
       ("harp.stamp-year" . ,(harp-design--box :padding_bottom 2))
       ("harp.stamp-time" . ,(harp-design--box :padding_top 4 :padding_bottom 2))
       ;; Inline chips: AssistChip, MetricValueChip and the compact AssetChip.
       ("harp.tag-chip" . ,(harp-design--box :center_vertically t :height 24 :corner_radius 8 :border_width 1
                                             :border_color (funcall c 'outline-variant)
                                             :padding_horizontal 15))
       ("harp.metric-chip" . ,(harp-design--box :center_vertically t :height 24 :corner_radius 12
                                                :background_color (funcall c 'secondary-container)
                                                :padding_horizontal 8))
       ("harp.attach-chip" . ,(harp-design--box :background_color (funcall c 'surface-variant)
                                                :corner_radius 999 :padding_horizontal 7
                                                :padding_vertical 3))
       ("harp.attach-gap" . ,(harp-design--box :padding 2))
       ;; NavigationBar: 80dp, items with a 64x32 indicator.
       ("harp.nav-bar" . ,(harp-design--box :background_color (funcall c 'surface-container)
                                            :padding 0))
       ("harp.nav" . ,(harp-design--box :height 80 :fill_width t))
       ;; Items share the width with 8dp between them; the 50x26 indicator
       ;; sits 15dp down, the label 48dp down.
       ("harp.nav-item" . ,(harp-design--box :padding_top 15 :fill_width t :min_height 0))
       ("harp.nav-indicator" . ,(harp-design--box :width 50 :height 26 :corner_radius 13))
       ("harp.nav-indicator-on" . ,(harp-design--box :width 50 :height 26 :corner_radius 13
                                                     :background_color (funcall c 'secondary-container)))
       ;; FloatingActionButton: 56dp, 16dp corners, 6dp elevation.
       ("harp.fab" . ,(harp-design--box :center_vertically t :width 56 :height 56 :corner_radius 16 :elevation 6
                                        :background_color (funcall c 'primary-container)))
       ("harp.placeholder" . ,(harp-design--box :padding 40 :fill_width t))
       ;; JournalEntryForm in its sheet: 15dp in, 10dp between rows.
       ("harp.sheet" . ,(harp-design--box :padding 15 :fill_width t))
       ("harp.t.sheet-title" . ,(harp-design--type 'headline-small (funcall c 'outline)))
       ("harp.t.suggest-label" . ,(harp-design--type 'label-small (funcall c 'outline)))
       ("harp.suggest" . ,(harp-design--box :center_vertically t :height 28 :corner_radius 8 :border_width 1
                                            :border_color (funcall c 'outline-variant)
                                            :padding_horizontal 12))
       ("harp.suggest-on" . ,(harp-design--box :center_vertically t :height 28 :corner_radius 8
                                               :background_color (funcall c 'secondary-container)
                                               :padding_start 8 :padding_end 12))
       ("harp.outlined-button" . ,(harp-design--box :center_vertically t :min_height 40 :corner_radius 20
                                                    :border_width 1 :border_color (funcall c 'outline-variant)
                                                    :padding_horizontal 24))
       ("harp.t.button-outlined" . ,(harp-design--type 'label-large (funcall c 'primary)))
       ;; OutlinedTextField: 4dp corners, the outline turning primary and 2dp
       ;; when focused, error red with its supporting line.
       ("harp.outlined" . ,(jetpacs-design-style
                            (harp-design--pairs (list :border_width 1 :border_color (funcall c 'outline)
                                                      :corner_radius 4 :background_color "#00000000"))
                            :rules (list (jetpacs-design-rule
                                          "focused" `(("border_width" . ,(jetpacs-design-dimension 2))
                                                      ("border_color" . ,(jetpacs-design-color (funcall c 'primary))))))))
       ("harp.t.field-label" . ,(harp-design--type 'body-small (funcall c 'on-surface-variant)))
       ("harp.t.field-hint" . ,(harp-design--type 'body-large (funcall c 'on-surface-variant)))
       ("harp.t.field-error" . ,(harp-design--type 'body-small (funcall c 'error)))
       ("harp.form" . ,(harp-design--box :padding 16 :fill_width t))
       ;; The search field's chromeless text.
       ("harp.field.box" . ,(jetpacs-design-style
                             (harp-design--pairs '(:border_width 0 :border_color "#00000000"
                                                   :background_color "#00000000" :padding 0
                                                   :corner_radius 0))
                             :rules (list (jetpacs-design-rule
                                           "focused" `(("border_width" . ,(jetpacs-design-dimension 0))
                                                       ("border_color" . ,(jetpacs-design-color "#00000000")))))))
       ("harp.field.text" . ,(harp-design--type 'label-medium (funcall c 'on-surface)
                                                :min_height 0))
       ("harp.field.hint" . ,(harp-design--type 'label-medium (funcall c 'search-hint)))
       ("harp.bottom-bar" . ,(harp-design--box :background_color (funcall c 'surface-container)
                                               :padding 0)))
     :component-styles
     '(("chrome.bottom-bar" . ("harp.bottom-bar"))
       ("text.body" . ("typography.body")) ("text.title" . ("typography.title"))
       ("text.caption" . ("typography.caption")) ("text.label" . ("typography.label"))
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
         (profile (if (assoc "harp.entry" (plist-get active :styles)) active
                    (jetpacs-design-profile-get (if dark "harp.dark" "harp.light")))))
    (jetpacs-design-profile-scope profile (list screen))))

(provide 'harp-design)
;;; harp-design.el ends here
