;;; harp-home.el --- Harp profile launcher (Home tab) -*- lexical-binding: t; -*-

;; The landing tab: a list of discovered profiles.  Tapping one opens it as
;; the current profile and jumps to the Journals tab.  Replaces harp-kmp's
;; HomeScreen / OpenProfileDialog / RecentProfiles.

;;; Code:

(require 'jetpacs-widgets)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
(require 'jetpacs-apps)
(require 'harp-profile)
(require 'harp-ui)

(defun harp-home--profile-card (info)
  "A tappable card for profile summary INFO."
  (let* ((file (alist-get 'file info))
         (type (alist-get 'type info))
         (current (equal file harp-ui--current-file)))
    (jetpacs-card
     (list (apply #'jetpacs-row
                  (append
                   (delq nil
                         (list
                          (jetpacs-icon (harp-ui-type-icon type))
                          (jetpacs-box
                           (list (jetpacs-column
                                  (jetpacs-text (alist-get 'name info) 'title)
                                  (jetpacs-text (harp-ui-type-label type) 'caption)))
                           :weight 1)
                          (when current (jetpacs-icon "check_circle"))))
                   '(:spacing 12 :align "center"))))
     :on-tap (jetpacs-action "harp.profile.open"
                             :args `((file . ,file)) :when-offline "drop"))))

(defun harp-home--body ()
  "The Home view body: header card plus the profile list."
  (let ((profiles (harp-ui-profiles)))
    (apply #'jetpacs-lazy-column
           (append
            (list (jetpacs-card
                   (list (jetpacs-column
                          (jetpacs-text "Harp" 'title)
                          (jetpacs-text "Personal Health Record" 'caption)))))
            (if profiles
                (cons (jetpacs-section-header
                       (format "Profiles (%d)" (length profiles)))
                      (mapcar #'harp-home--profile-card profiles))
              (list (jetpacs-empty-state
                     :icon "person_add"
                     :title "No profiles yet"
                     :caption (format "Add a profile .org file under %s, or load the samples to explore."
                                      (abbreviate-file-name (or harp-directory "")))
                     :on-tap (jetpacs-action "harp.demo.setup" :when-offline "drop")
                     :action-label "Load sample profiles")))))))

(with-jetpacs-owner "harp"

  ;; The empty-state button only exists while no profiles do; this keeps the
  ;; sample corpus reachable afterwards (harp.demo.setup offers a refresh
  ;; when the files already exist).
  (jetpacs-shell-add-drawer-item
   60
   (lambda ()
     (jetpacs-drawer-item "dataset" "Sample profiles"
                          (jetpacs-action "harp.demo.setup"
                                          :when-offline "drop"))))

  (jetpacs-shell-define-view "harp.home"
    :builder (lambda (snackbar)
               (jetpacs-shell-tab-view "harp.home" (harp-home--body)
                                       :snackbar snackbar))
    :tab '(:icon "favorite" :label "Home")
    :order 0)

  (jetpacs-defaction "harp.profile.open"
    (lambda (args _)
      (let ((file (alist-get 'file args)))
        ;; Allowlist rule: only open a path that is actually a discovered
        ;; profile — the wire never opens arbitrary files.
        (when (and (stringp file) (member file (harp-profile-files)))
          (harp-ui-set-current file)
          (let ((p (harp-ui-current)))
            (jetpacs-shell-notify
             (format "Opened %s" (or (alist-get 'name p) "profile"))))
          (jetpacs-shell-set-current-tab "harp.journal"))))
    :doc "Open a discovered profile by file path."
    :args '((:name file :type "text" :required t))))

(provide 'harp-home)
;;; harp-home.el ends here
