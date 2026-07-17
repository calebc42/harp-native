;;; harp.el --- Harp: a Personal Health Record org app on Jetpacs -*- lexical-binding: t; -*-

;; Harp is a privacy-centric, on-device Personal Health Record app.  It
;; stores journals, health metrics, medical documents, and reminders in
;; plain Org files that stay entirely under the user's control.  This is
;; the Jetpacs Tier-1 rebuild of harp-kmp: the phone is a thin renderer,
;; Emacs + org-mode is the source of truth, and the whole app is elisp on
;; top of the Jetpacs core.
;;
;;   (require 'jetpacs-core)   ; from the jetpacs repo / submodule
;;   (require 'harp)           ; this app
;;
;; The pre-built single-file bundle at the repo root carries the same
;; feature name, so init files work unchanged with either install option.
;;
;; This is the app entry point: it pulls in every Harp module (each
;; self-registers its views and actions under the "harp" owner) and
;; declares the app identity.

;;; Code:

(require 'jetpacs-shell)
(require 'jetpacs-apps)
(require 'jetpacs-widgets)
(require 'jetpacs-surfaces)

(require 'harp-metrics-parse)
(require 'harp-profile)
(require 'harp-ui)
(require 'harp-write)
(require 'harp-med)
(require 'harp-med-schedule)
(require 'harp-med-log)
(require 'harp-med-view)
(require 'harp-med-form)
(require 'harp-med-notify)
(require 'harp-home)
(require 'harp-journal)
(require 'harp-documents)
(require 'harp-metrics)
(require 'harp-reminders)
(require 'harp-form)
(require 'harp-export)
(require 'harp-demo)
(require 'harp-settings)
(require 'harp-pack)

(defconst harp-app-id "harp"
  "App id keying Harp's ownership, config subtree, and views.")

(defconst harp-version "0.0.1"
  "Harp app version.")

(with-jetpacs-owner "harp"
  (jetpacs-defapp harp-app-id
    :label "Harp"
    :icon "favorite"
    :views '("harp.home" "harp.journal" "harp.meds" "harp.documents"
             "harp.metrics" "harp.reminders" "harp.form"
             "harp.med.detail" "harp.med.form")
    :order 10))

(provide 'harp)
;;; harp.el ends here
