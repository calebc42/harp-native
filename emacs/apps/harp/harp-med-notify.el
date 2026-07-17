;;; harp-med-notify.el --- Interactive dose notifications -*- lexical-binding: t; -*-

;; When a medication dose is due, push an interactive notification surface
;; carrying Taken / Skip / Snooze action buttons (SPEC §9 `meta.actions', new
;; in the fadeeb2 core).  Tapping a button logs the dose (or snoozes) without
;; opening the app.  This is the Emacs-connected companion to the scheduled
;; `reminders.set' alarm in harp-reminders.el (which stays as the Emacs-dead
;; nudge): the alarm says "it's time", this lets you act on it.
;;
;; A dose is "due now" when its scheduled time has passed, it is still pending
;; (count-based, `harp-med-schedule'), and it is not snoozed.  One notification
;; per medication (its earliest due dose); re-pushed only when its content
;; changes, so a live dose does not re-alert every tick.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'jetpacs-widgets)
(require 'jetpacs-surfaces)
(require 'jetpacs-shell)
(require 'jetpacs-settings)
(require 'jetpacs)                      ; jetpacs--session
(require 'harp-ui)
(require 'harp-med)
(require 'harp-med-schedule)

(defcustom harp-med-notify-enabled t
  "When non-nil, due medication doses raise interactive notifications."
  :type 'boolean :group 'harp)

(defcustom harp-med-notify-snooze-minutes 15
  "Minutes to silence a dose notification after Snooze."
  :type 'integer :group 'harp)

(defcustom harp-med-notify-interval 60
  "Seconds between due-dose checks while connected; nil disables the timer.
The connect and after-push hooks refresh immediately; this keeps a due dose
timely between user interactions (a resident Emacs is assumed — see the
jetpacs execution model)."
  :type '(choice (const :tag "no timer" nil) integer)
  :group 'harp)

(defvar harp-med-notify--snoozed (make-hash-table :test 'equal)
  "Map of \"MEDID\\0TIME\" -> snooze-until float-time.")

(defvar harp-med-notify--pushed nil
  "Alist of pushed surface-id -> content signature, to avoid re-alerting.")

(defun harp-med-notify--surface-id (med)
  (format "notification:harp.dose.%s" (alist-get 'id med)))

(defun harp-med-notify--label (amount unit)
  (string-trim (concat (or amount "") " " (or unit ""))))

;; ─── which doses are due now ─────────────────────────────────────────────────

(defun harp-med-notify--due-now (&optional profile)
  "Currently-due dose entries: past scheduled time, pending, not snoozed.
One entry per medication (its earliest due dose)."
  (let ((now (float-time)) (seen (make-hash-table :test 'equal)) out)
    (dolist (e (if profile
                   (harp-med-schedule-due-today profile)
                 (harp-med-due-today)))
      (let* ((med (alist-get 'med e))
             (mid (alist-get 'id med))
             (at (alist-get 'at e))
             (key (format "%s\0%s" mid (alist-get 'time e)))
             (snooze (gethash key harp-med-notify--snoozed)))
        (when (and at (<= at now)
                   (or (null snooze) (>= now snooze))
                   (not (gethash mid seen)))
          (puthash mid t seen)
          (push e out))))
    (nreverse out)))

;; ─── the notification spec ───────────────────────────────────────────────────

(defun harp-med-notify--spec (med entry)
  "An interactive notification spec for a due dose ENTRY of MED."
  (let* ((label (harp-med-notify--label (alist-get 'amount entry)
                                        (alist-get 'unit entry)))
         (args `((id . ,(alist-get 'id med))
                 (amount . ,(alist-get 'amount entry))
                 (unit . ,(alist-get 'unit med))
                 (file . ,harp-ui--current-file)))
         (snooze-args (append args `((time . ,(alist-get 'time entry)))))
         (time (alist-get 'time entry)))
    (jetpacs-notification-spec
     :channel "harp.doses"
     :priority "high"
     :category "reminder"
     :body (list (jetpacs-text (alist-get 'name med) 'title)
                 (jetpacs-text (string-trim
                                (concat "Due " (alist-get 'time entry)
                                        (unless (string-empty-p label)
                                          (concat " · " label))))
                               'body))
     :actions
     (list (jetpacs-notification-action
            "Taken" (jetpacs-action "harp.med.taken" :args args
                                    :when-offline "queue"
                                    :dedupe (harp-med-dose-dedupe-key "taken" med time))
            :dismiss t)
           (jetpacs-notification-action
            "Skip" (jetpacs-action "harp.med.skipped" :args args
                                   :when-offline "queue"
                                   :dedupe (harp-med-dose-dedupe-key "skipped" med time))
            :dismiss t)
           (jetpacs-notification-action
            "Snooze" (jetpacs-action "harp.med.snooze" :args snooze-args
                                     :when-offline "queue"
                                     :dedupe (harp-med-dose-dedupe-key "snooze" med time))
            :dismiss t)))))

(defun harp-med-notify--signature (med entry)
  "A content signature so an unchanged notification is not re-pushed."
  (format "%s|%s|%s" (alist-get 'name med) (alist-get 'time entry)
          (harp-med-notify--label (alist-get 'amount entry) (alist-get 'unit med))))

;; ─── sync ────────────────────────────────────────────────────────────────────

(defun harp-med-notify--prune-snoozed ()
  "Drop expired snoozes — the map would otherwise grow one entry per snooze."
  (let ((now (float-time)))
    (maphash (lambda (k until)
               (when (< until now) (remhash k harp-med-notify--snoozed)))
             harp-med-notify--snoozed)))

(defun harp-med-notify-sync ()
  "Reconcile due-dose notifications with the current state (connected only)."
  (when (and harp-med-notify-enabled jetpacs--session (harp-ui-current))
    (harp-med-notify--prune-snoozed)
    (let ((want '()))
      (dolist (e (harp-med-notify--due-now))
        (let* ((med (alist-get 'med e))
               (sid (harp-med-notify--surface-id med))
               (sig (harp-med-notify--signature med e)))
          (push (cons sid sig) want)
          ;; Push only when new or changed — never re-alert a live dose.
          (unless (equal sig (cdr (assoc sid harp-med-notify--pushed)))
            (jetpacs-surface-push sid (harp-med-notify--spec med e)))))
      ;; Remove notifications for doses no longer due.
      (dolist (old harp-med-notify--pushed)
        (unless (assoc (car old) want)
          (jetpacs-surface-remove (car old))))
      (setq harp-med-notify--pushed want))))

(defun harp-med-notify--clear-med (med)
  "Remove MED's dose notification, if any."
  (let ((sid (harp-med-notify--surface-id med)))
    (when (assoc sid harp-med-notify--pushed)
      (jetpacs-surface-remove sid)
      (setq harp-med-notify--pushed
            (assoc-delete-all sid harp-med-notify--pushed)))))

;; ─── timer ───────────────────────────────────────────────────────────────────

(defvar harp-med-notify--timer nil)

(defun harp-med-notify--start-timer (&rest _)
  "Start the periodic due-dose check (idempotent)."
  (when (and harp-med-notify-enabled harp-med-notify-interval
             (not (timerp harp-med-notify--timer)))
    (setq harp-med-notify--timer
          (run-with-timer harp-med-notify-interval harp-med-notify-interval
                          #'harp-med-notify-sync))))

(defun harp-med-notify--stop-timer (&rest _)
  (when (timerp harp-med-notify--timer)
    (cancel-timer harp-med-notify--timer)
    (setq harp-med-notify--timer nil)))

(defun harp-med-notify--on-enabled-change (value)
  "React to the `harp-med-notify-enabled' toggle arriving from the wire.
Turning it on mid-session must arm the timer and reconcile now — waiting
for the next connect would leave doses silent for a whole session."
  (if value
      (progn (harp-med-notify--start-timer)
             (harp-med-notify-sync))
    (harp-med-notify--stop-timer)
    ;; Clear any live dose notifications so the phone doesn't keep them.
    (dolist (old harp-med-notify--pushed)
      (jetpacs-surface-remove (car old)))
    (setq harp-med-notify--pushed nil)))

;; ─── actions + wiring ────────────────────────────────────────────────────────

(with-jetpacs-owner "harp"
  (jetpacs-defaction "harp.med.snooze"
    (lambda (args _)
      (let* ((id (alist-get 'id args))
             (time (alist-get 'time args))
             (key (format "%s\0%s" id time))
             (med (harp-med-by-id id)))
        (puthash key (+ (float-time) (* 60 harp-med-notify-snooze-minutes))
                 harp-med-notify--snoozed)
        (when med (harp-med-notify--clear-med med))
        (jetpacs-shell-notify
         (format "Snoozed %s min" harp-med-notify-snooze-minutes))
        ;; notify only queues the snackbar — push so it actually shows.
        (jetpacs-shell-push)))
    :doc "Snooze a due-dose notification."
    :args '((:name id :type "text" :required t)
            (:name time :type "text")
            (:name amount :type "text")
            (:name unit :type "text")))

  (jetpacs-settings-register-section
   "Medications"
   '((harp-med-notify-enabled :label "Due-dose notifications"
                              :after-set harp-med-notify--on-enabled-change)
     (harp-med-notify-snooze-minutes :label "Snooze minutes")
     (harp-med-notify-interval :label "Due check interval (seconds)"))))

;; Refresh on connect (and start the timer) and after every app push.  The
;; timer keeps ticking while disconnected but `harp-med-notify-sync' no-ops
;; without a session, so there is no disconnect hook to stop it.
(add-hook 'jetpacs-connected-hook #'harp-med-notify--start-timer)
(add-hook 'jetpacs-connected-hook #'harp-med-notify-sync)
(add-hook 'jetpacs-shell-after-push-hook #'harp-med-notify-sync)

(provide 'harp-med-notify)
;;; harp-med-notify.el ends here
