;;; harp-reminders.el --- Harp reminders view -*- lexical-binding: t; -*-

;; The Reminders tab: the profile's scheduled/deadline TODOs as cards, over
;; a month calendar marking the days that carry a reminder.  Read-only for
;; now; scheduling edits and companion notifications land in a later pass.
;; Mirrors harp-kmp's RemindersView.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'jetpacs-widgets)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
(require 'jetpacs-device)
(require 'harp-ui)
(require 'harp-med)
(require 'harp-med-schedule)

(defun harp-reminders--date-of (stamp)
  "The \"YYYY-MM-DD\" date embedded in Org timestamp STAMP, or nil."
  (and (stringp stamp)
       (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" stamp)
       (match-string 1 stamp)))

(defvar harp-reminders--month nil
  "The \"YYYY-MM\" month shown on the calendar, or nil for the current one.")

(defun harp-reminders--month-bounds (month)
  "A (START . END) cons of Lisp times covering \"YYYY-MM\" MONTH.
END is the first instant of the following month."
  (let* ((y (string-to-number (substring month 0 4)))
         (m (string-to-number (substring month 5 7))))
    (cons (encode-time 0 0 0 1 m y)
          (if (= m 12)
              (encode-time 0 0 0 1 1 (1+ y))
            (encode-time 0 0 0 1 (1+ m) y)))))

(defun harp-reminders--occurrences-in-month (stamp month)
  "Dates (\"YYYY-MM-DD\") STAMP lands on within MONTH, honoring a repeater.
A non-repeating stamp contributes at most its own date; a `+N[hdwmy]'
repeater is stepped from the base date through the month."
  (let ((base (and (stringp stamp)
                   (ignore-errors (org-time-string-to-time stamp))))
        (rep (harp-reminders--parse-repeater stamp)))
    (when base
      (let* ((bounds (harp-reminders--month-bounds month))
             (start (car bounds)) (end (cdr bounds))
             ;; Jump the base up close to the month first, so a small repeater
             ;; on an old stamp doesn't burn the step cap before reaching it.
             (tt (if rep
                     (let* ((per (harp-reminders--seconds-per (car rep) (cdr rep)))
                            (gap (- (float-time start) (float-time base)))
                            (jumps (if (and (> per 0) (> gap 0))
                                       (max 0 (1- (floor gap per))) 0)))
                       (harp-reminders--step-n base (car rep) (cdr rep) jumps))
                   base))
             (i 0) dates)
        (while (and (time-less-p tt end) (< i 4000))
          (unless (time-less-p tt start)
            (push (format-time-string "%Y-%m-%d" tt) dates))
          (if rep
              (setq tt (harp-reminders--step tt (car rep) (cdr rep)))
            (setq tt end))
          (setq i (1+ i)))
        (nreverse dates)))))

(defun harp-reminders--marks (reminders &optional month)
  "Month-grid marks for MONTH (default: this month) over REMINDERS.
Repeating reminders mark every occurrence in the month, so navigating
ahead shows the future cadence, not just base dates."
  (let ((month (or month (format-time-string "%Y-%m")))
        (counts (make-hash-table :test #'equal)))
    (dolist (r reminders)
      (dolist (stamp (list (alist-get 'scheduled r) (alist-get 'deadline r)))
        (dolist (d (harp-reminders--occurrences-in-month stamp month))
          (puthash d (1+ (gethash d counts 0)) counts))))
    (let (marks)
      (maphash (lambda (d n) (push (cons d (min n 3)) marks)) counts)
      marks)))

(defun harp-reminders--when-label (r)
  "A friendly \"when\" caption for reminder R."
  (let ((sched (alist-get 'scheduled r))
        (deadline (alist-get 'deadline r)))
    (cond
     ((and sched deadline)
      (format "Scheduled %s · Due %s"
              (harp-ui-format-stamp sched) (harp-ui-format-stamp deadline)))
     (sched (format "Scheduled %s" (harp-ui-format-stamp sched)))
     (deadline (format "Due %s" (harp-ui-format-stamp deadline)))
     (t ""))))

(defun harp-reminders--card (r)
  "A card rendering reminder R."
  (jetpacs-card
   (list (apply #'jetpacs-column
                (delq nil
                      (append
                       (list (apply #'jetpacs-row
                                    (append
                                     (delq nil
                                           (list
                                            (when (alist-get 'todo r)
                                              (jetpacs-assist-chip (alist-get 'todo r)))
                                            (jetpacs-box
                                             (list (jetpacs-text (alist-get 'title r) 'title))
                                             :weight 1)))
                                     '(:align "center")))
                             (jetpacs-text (harp-reminders--when-label r) 'caption))
                       (harp-ui-body-nodes (alist-get 'body r))
                       (list (harp-ui-tag-chips (alist-get 'tags r)))))))))

(defun harp-reminders--view (snackbar)
  "The Reminders screen for the current profile."
  (jetpacs-shell-tab-view
   "harp.reminders"
   (let ((profile (harp-ui-current)))
     (if (null profile)
         (harp-ui-no-profile-body)
       (let ((reminders (alist-get 'reminders profile)))
         (if reminders
             (apply #'jetpacs-lazy-column
                    (append
                     (when (jetpacs-node-supported-p "month_grid")
                       (let ((month (or harp-reminders--month
                                        (format-time-string "%Y-%m"))))
                         (list (jetpacs-month-grid
                                month
                                :marks (harp-reminders--marks reminders month)
                                :on-month-change
                                (jetpacs-action "harp.reminders.month"
                                                :when-offline "drop")))))
                     (mapcar #'harp-reminders--card reminders)))
           (jetpacs-empty-state
            :icon "notifications"
            :title "No reminders"
            :caption "This profile has no reminders yet.")))))
   :snackbar snackbar))

(with-jetpacs-owner "harp"
  (jetpacs-shell-define-view "harp.reminders"
    :builder #'harp-reminders--view
    :tab '(:icon "notifications" :label "Reminders")
    :when #'harp-ui-profile-open-p
    :order 40)

  (jetpacs-defaction "harp.reminders.month"
    (lambda (args _)
      (let ((m (alist-get 'value args)))
        (when (and (stringp m)
                   (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}\\'" m))
          (setq harp-reminders--month m)
          (jetpacs-shell-push))))
    :doc "Show a different month on the reminders calendar."
    :args '((:name value :type "text" :required t))))

;; Leaving the tab resets the calendar to the current month.
(defun harp-reminders--on-view-switched (view)
  "Reset the shown month when leaving the Reminders tab."
  (unless (equal view "harp.reminders")
    (setq harp-reminders--month nil)))

(add-hook 'jetpacs-shell-view-switched-hook #'harp-reminders--on-view-switched)

;; ─── companion alarms (SPEC §7 reminders) ────────────────────────────────────
;; The current profile's future reminders are armed as exact alarms on the
;; companion, replacing only Harp's set and surviving reboots.  Re-armed
;; whenever the arming set changes (mutations, profile switch) and on connect.

(defvar harp-reminders--last 'unset
  "Last reminder set armed, to avoid re-sending an unchanged set.")

(defun harp-reminders--parse-repeater (stamp)
  "Return (N . UNIT-CHAR) for STAMP's +N[hdwmy] repeater, or nil."
  (when (and (stringp stamp)
             (string-match "[.+]\\{1,2\\}\\([0-9]+\\)\\([hdwmy]\\)" stamp))
    (cons (string-to-number (match-string 1 stamp))
          (aref (match-string 2 stamp) 0))))

(defun harp-reminders--step (time n unit)
  "Advance TIME by N of UNIT (a char h/d/w/m/y), DST-safe.
Steps the wall-clock date/time fields and re-resolves the local offset, so
`+1d' keeps the same clock time across a DST transition rather than
drifting an hour (or, for a date-only stamp, sliding to 23:00 the day before)."
  (let ((d (decode-time time)))
    (pcase unit
      (?h (setf (decoded-time-hour d) (+ (decoded-time-hour d) n)))
      (?d (setf (decoded-time-day d) (+ (decoded-time-day d) n)))
      (?w (setf (decoded-time-day d) (+ (decoded-time-day d) (* 7 n))))
      (?m (setf (decoded-time-month d) (+ (decoded-time-month d) n)))
      (?y (setf (decoded-time-year d) (+ (decoded-time-year d) n)))
      (_ (setf (decoded-time-day d) (+ (decoded-time-day d) n))))
    (harp--encode-local d)))

(defun harp-reminders--seconds-per (n unit)
  "Approximate seconds in N of UNIT — only for a jump estimate, never a stamp."
  (* n (pcase unit (?h 3600) (?d 86400) (?w 604800)
              (?m 2592000) (?y 31536000) (_ 86400))))

(defun harp-reminders--next-time (stamp)
  "Float-time of STAMP's next future occurrence, honoring a repeater; else nil."
  (let ((base (and (stringp stamp) (ignore-errors (org-time-string-to-time stamp))))
        (rep (harp-reminders--parse-repeater stamp))
        (now (current-time)))
    (when base
      (if (time-less-p now base)
          (float-time base)
        (when rep
          ;; Jump most of the gap mathematically first, so a small (e.g. +1h)
          ;; repeater on a stamp years old doesn't exhaust the step cap and
          ;; silently yield no alarm; then step precisely (DST-correct).
          (let* ((per (harp-reminders--seconds-per (car rep) (cdr rep)))
                 (gap (- (float-time now) (float-time base)))
                 (jumps (if (> per 0) (max 0 (1- (floor gap per))) 0))
                 (tt (harp-reminders--step-n base (car rep) (cdr rep) jumps))
                 (i 0))
            (while (and (not (time-less-p now tt)) (< i 4000))
              (setq tt (harp-reminders--step tt (car rep) (cdr rep))
                    i (1+ i)))
            (and (time-less-p now tt) (float-time tt))))))))

(defun harp-reminders--step-n (time n unit jumps)
  "Advance TIME by JUMPS applications of the (N . UNIT) repeater, in one op.
`encode-time' normalises the (possibly huge) field, so this is O(1) — the
mathematical jump that keeps a small repeater on an old stamp from
exhausting the precise-step cap."
  (if (<= jumps 0)
      time
    (let ((d (decode-time time)) (total (* n jumps)))
      (pcase unit
        (?h (setf (decoded-time-hour d) (+ (decoded-time-hour d) total)))
        (?d (setf (decoded-time-day d) (+ (decoded-time-day d) total)))
        (?w (setf (decoded-time-day d) (+ (decoded-time-day d) (* 7 total))))
        (?m (setf (decoded-time-month d) (+ (decoded-time-month d) total)))
        (?y (setf (decoded-time-year d) (+ (decoded-time-year d) total)))
        (_ (setf (decoded-time-day d) (+ (decoded-time-day d) total))))
      (harp--encode-local d))))

(defun harp-reminders--specs (profile)
  "Reminder alarm specs for PROFILE (future occurrences only)."
  (let (specs)
    (dolist (r (alist-get 'reminders profile))
      (let ((time (harp-reminders--next-time
                   (or (alist-get 'scheduled r) (alist-get 'deadline r)))))
        (when time
          (push `((id . ,(format "harp/%s" (alist-get 'id r)))
                  (at_ms . ,(truncate (* time 1000)))
                  (title . ,(alist-get 'title r))
                  (body . ,(harp-reminders--when-label r)))
                specs))))
    (nreverse specs)))

(defun harp-reminders--med-specs (profile)
  "Alarm specs for the next dose of each active medication in PROFILE."
  (let (specs)
    (dolist (m (alist-get 'medications profile))
      (when (harp-med-active-p m)
        (when-let ((d (harp-med-schedule-next-dose m)))
          (push `((id . ,(format "harp/med/%s" (alist-get 'id m)))
                  (at_ms . ,(truncate (* (alist-get 'at d) 1000)))
                  (title . ,(alist-get 'name m))
                  (body . ,(string-trim
                            (concat "Dose " (or (alist-get 'amount d) "")
                                    " " (or (alist-get 'unit d) "")))))
                specs))))
    (nreverse specs)))

(defun harp-reminders-sync ()
  "Arm the current profile's upcoming reminders and medication doses.
Only runs while connected — arming an absent companion is a no-op that
would trip the capability-negotiation warning on every offline push."
  (when jetpacs--session
    (let ((specs (and (harp-ui-current)
                      (append (harp-reminders--specs (harp-ui-current))
                              (harp-reminders--med-specs (harp-ui-current))))))
      (unless (equal specs harp-reminders--last)
        (setq harp-reminders--last specs)
        (jetpacs-reminders-owner-set (vconcat specs) "harp")))))

(defun harp-reminders--on-connect (&rest _)
  "Force a re-arm on a fresh connection."
  (setq harp-reminders--last 'unset)
  (harp-reminders-sync))

;; Ride pushes (memo-guarded by the set comparison) and connects.
(add-hook 'jetpacs-shell-after-push-hook #'harp-reminders-sync)
(add-hook 'jetpacs-connected-hook #'harp-reminders--on-connect)

(provide 'harp-reminders)
;;; harp-reminders.el ends here
