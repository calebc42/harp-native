;;; harp-med-schedule.el --- Harp medication scheduling engine -*- lexical-binding: t; -*-

;; Computes which doses a medication is due on a given day, the next future
;; dose (for arming alarms), and which of today's doses are still pending.
;; Composes MedTimer's schedule modifiers on top of a schedule's :TIMES: (or
;; :INTERVAL:): weekday filter (:DAYS:), cyclic active/pause cycles
;; (:CYCLE_START:/:ACTIVE_DAYS:/:PAUSE_DAYS:), and a date period
;; (:FROM:/:UNTIL:, for tapering). Pure elisp; date math via `time-to-days'
;; and `decode-time'.

;;; Code:

(require 'cl-lib)
(require 'harp-profile)                 ; harp--time-of
(require 'harp-med)                     ; accessors, harp-med-doses-on
(require 'harp-ui)                      ; harp-ui-memo (push-scoped memo)

(defconst harp-med--weekday-abbrevs ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"]
  "English weekday abbreviations indexed by `decoded-time-weekday' (0=Sun).")

(defun harp-med--duration-seconds (s)
  "Parse a duration string like \"6h\" / \"90m\" / \"3d\" / \"2w\" into seconds."
  (when (stringp s)
    ;; Match and extract against the SAME (trimmed) string — matching the
    ;; trimmed one but extracting groups from the original returns shifted
    ;; substrings on any padded input.
    (let ((s (string-trim s)))
      (when (string-match "\\`\\([0-9]+\\)\\([mhdw]\\)\\'" s)
        (* (string-to-number (match-string 1 s))
           (pcase (aref (match-string 2 s) 0)
             (?m 60) (?h 3600) (?d 86400) (?w 604800)))))))

(defun harp-med--hm-to-min (hm)
  "Minutes-since-midnight for an \"HH:MM\" string HM."
  (let ((p (mapcar #'string-to-number (split-string hm ":"))))
    (+ (* 60 (nth 0 p)) (nth 1 p))))

(defun harp-med-schedule--weekday (date)
  "English weekday abbrev (\"Mon\") for Lisp time DATE."
  (aref harp-med--weekday-abbrevs (decoded-time-weekday (decode-time date))))

(defun harp-med-schedule--datetime (date hm)
  "Float-time of DATE (a Lisp time) at clock time HM (\"HH:MM\").
DST-safe: re-resolves the local offset for DATE's day, so a dose time on
the far side of a DST transition is not computed an hour off."
  (let ((d (decode-time date))
        (p (mapcar #'string-to-number (split-string hm ":"))))
    (setf (decoded-time-hour d) (nth 0 p)
          (decoded-time-minute d) (nth 1 p)
          (decoded-time-second d) 0)
    (float-time (harp--encode-local d))))

;; ─── activity of a schedule on a date ────────────────────────────────────────

(defun harp-med-schedule--in-period-p (schedule date)
  "Non-nil when DATE is within SCHEDULE's :FROM:/:UNTIL: period (inclusive)."
  (let ((from (harp--time-of (alist-get 'from schedule)))
        (until (harp--time-of (alist-get 'until schedule)))
        (d (time-to-days date)))
    (and (or (null from) (>= d (time-to-days (seconds-to-time from))))
         (or (null until) (<= d (time-to-days (seconds-to-time until)))))))

(defun harp-med-schedule--weekday-ok-p (schedule date)
  "Non-nil when DATE's weekday passes SCHEDULE's :DAYS: filter."
  (let ((days (alist-get 'days schedule)))
    (or (null days) (member (harp-med-schedule--weekday date) days))))

(defun harp-med-schedule--cycle-ok-p (schedule date)
  "Non-nil when DATE lands on an active day of SCHEDULE's cyclic pattern.
`pos = (date − CYCLE_START) mod (ACTIVE + PAUSE)`; active when `pos < ACTIVE'."
  (let ((start (harp--time-of (alist-get 'cycle-start schedule)))
        (active (alist-get 'active-days schedule))
        (pause (alist-get 'pause-days schedule)))
    (if (not (and start (numberp active) (numberp pause) (> (+ active pause) 0)))
        t
      (< (mod (- (time-to-days date) (time-to-days (seconds-to-time start)))
              (+ active pause))
         active))))

(defun harp-med-schedule-active-on-p (schedule med date)
  "Non-nil when SCHEDULE of MED yields doses on DATE (a Lisp time)."
  (and (harp-med-active-p med)
       (harp-med-schedule--in-period-p schedule date)
       (harp-med-schedule--weekday-ok-p schedule date)
       (harp-med-schedule--cycle-ok-p schedule date)))

;; ─── the times a schedule fires on a date ────────────────────────────────────

(defun harp-med-schedule--window-bounds (window)
  "Parse WINDOW \"HH:MM-HH:MM\" to (START-MIN . END-MIN); default 06:00–23:00."
  (let ((w (and (stringp window) (string-trim window))))
    (if (and w (string-match
                "\\`\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)-\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)\\'" w))
        ;; Bind both groups BEFORE `harp-med--hm-to-min' (which calls
        ;; `split-string' and would clobber the match data).
        (let ((start (match-string 1 w)) (end (match-string 2 w)))
          (cons (harp-med--hm-to-min start) (harp-med--hm-to-min end)))
      (cons 360 1380))))

(defun harp-med-schedule--interval-multiday (schedule date secs bounds)
  "\"HH:MM\" times a ≥1-day interval SCHEDULE (period SECS) fires on DATE.
Continuous from an anchor at the window-start of the schedule's anchor day
\(:CYCLE_START:, else :FROM:, else DATE itself), so a 36h interval steps
08:00 → 20:00 → 08:00 across days — a whole-day interval (24h/48h/72h)
reduces to a fixed daily time every N days.  Day membership uses
`time-to-days' (DST-aware); the exotic multi-day case may drift an hour
across a transition, which is immaterial for a dose reminder."
  (let* ((anchor-src (or (harp--time-of (alist-get 'cycle-start schedule))
                         (harp--time-of (alist-get 'from schedule))
                         (float-time date)))
         (start-min (car bounds))
         (anchor (harp-med-schedule--datetime
                  (seconds-to-time anchor-src)
                  (format "%02d:%02d" (/ start-min 60) (% start-min 60))))
         (target-day (time-to-days date))
         (day-start (harp-med-schedule--datetime date "00:00"))
         (tt (+ anchor (* (max 0 (ceiling (- day-start anchor) secs)) secs)))
         (guard 0) out)
    (while (and (< (time-to-days (seconds-to-time tt)) target-day) (< guard 800))
      (setq tt (+ tt secs) guard (1+ guard)))
    (while (and (= (time-to-days (seconds-to-time tt)) target-day) (< guard 800))
      (let ((d (decode-time (seconds-to-time tt))))
        (push (format "%02d:%02d" (decoded-time-hour d) (decoded-time-minute d)) out))
      (setq tt (+ tt secs) guard (1+ guard)))
    (nreverse out)))

(defun harp-med-schedule--interval-times (schedule &optional date)
  "The \"HH:MM\" dose times for an interval SCHEDULE on DATE.
Sub-daily intervals tile the daily :WINDOW:; intervals of a day or more are
a continuous sequence from an anchor (`harp-med-schedule--interval-multiday')."
  (let ((secs (harp-med--duration-seconds (alist-get 'interval schedule))))
    (when (and secs (> secs 0))
      (let ((bounds (harp-med-schedule--window-bounds (alist-get 'window schedule))))
        (if (>= secs 86400)
            (and date (harp-med-schedule--interval-multiday schedule date secs bounds))
          (let ((step (/ secs 60)) (m (car bounds)) (end (cdr bounds)) out)
            (while (<= m end)
              (push (format "%02d:%02d" (/ m 60) (% m 60)) out)
              (setq m (+ m step)))
            (nreverse out)))))))

(defun harp-med-schedule--times-on (schedule &optional date)
  "The \"HH:MM\" times SCHEDULE fires on DATE (`:TIMES:' or interval)."
  (or (alist-get 'times schedule)
      (and (alist-get 'interval schedule)
           (harp-med-schedule--interval-times schedule date))))

;; ─── public: doses on a date, pending, next ──────────────────────────────────

(defun harp-med-schedule-doses-on (med date)
  "Doses MED is due on DATE (a Lisp time), sorted by time.
Each dose is an alist: `time' (\"HH:MM\"), `at' (float-time), `amount',
`unit', `schedule'."
  (let (out)
    (dolist (s (alist-get 'schedules med))
      (when (harp-med-schedule-active-on-p s med date)
        (dolist (hm (harp-med-schedule--times-on s date))
          (push `((time . ,hm)
                  (at . ,(harp-med-schedule--datetime date hm))
                  (amount . ,(alist-get 'amount s))
                  (unit . ,(alist-get 'unit med))
                  (schedule . ,s))
                out))))
    (sort out (lambda (a b) (string< (alist-get 'time a) (alist-get 'time b))))))

(defun harp-med-schedule-pending (med &optional date)
  "MED's still-pending doses on DATE (default today).
Count-based: the first K scheduled doses (K = doses logged that day) are
treated as done, the rest are pending."
  (let* ((date (or date (current-time)))
         (dstr (format-time-string "%Y-%m-%d" date))
         (scheduled (harp-med-schedule-doses-on med date))
         (logged (length (harp-med-doses-on med dstr))))
    (nthcdr (min logged (length scheduled)) scheduled)))

(defun harp-med-schedule-next-dose (med &optional after horizon-days)
  "The next dose (full alist, carrying `at') for MED strictly after AFTER.
Searches up to HORIZON-DAYS ahead (default 400); nil if none.  Steps by
CALENDAR days (not 86400 s), so no day is skipped across a DST transition."
  (let* ((base (or after (current-time)))
         (now (float-time base))
         (base-day (decode-time base)))
    (catch 'found
      (dotimes (i (or horizon-days 400))
        (let ((day (let ((d (copy-sequence base-day)))
                     (setf (decoded-time-day d) (+ (decoded-time-day base-day) i))
                     (harp--encode-local d))))
          (dolist (d (harp-med-schedule-doses-on med day))
            (when (> (alist-get 'at d) now)
              (throw 'found d)))))
      nil)))

(defun harp-med-schedule-next (med &optional after horizon-days)
  "The next dose datetime (float) for MED strictly after AFTER, or nil."
  (alist-get 'at (harp-med-schedule-next-dose med after horizon-days)))

(defun harp-med-schedule-due-today (&optional profile)
  "All pending doses today across PROFILE's medications, each with its `med'.
Sorted by time — the shape the Due-today list wants."
  (let (out)
    (dolist (m (harp-med-medications profile))
      (dolist (d (harp-med-schedule-pending m))
        (push (cons (cons 'med m) d) out)))
    (sort out (lambda (a b) (string< (alist-get 'time a) (alist-get 'time b))))))

(defun harp-med-due-today ()
  "Pending doses today across the current profile, memoised for the push.
Three surfaces ask for this each push — the due list, its badge count, and
the notification sync — so compute it once.  Keyed on the calendar day so
it recomputes across midnight; busts with the profile read on any dose log."
  (harp-ui-memo (cons 'due-today (format-time-string "%Y-%m-%d"))
                (lambda () (harp-med-schedule-due-today))))

(provide 'harp-med-schedule)
;;; harp-med-schedule.el ends here
