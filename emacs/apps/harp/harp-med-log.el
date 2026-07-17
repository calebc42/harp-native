;;; harp-med-log.el --- Harp dose logging -*- lexical-binding: t; -*-

;; Recording a dose Taken or Skipped: prepend a `- STATE [ts] AMOUNT' line to
;; the medication's :LOGBOOK: drawer and, on Taken, decrement its :STOCK: by
;; the dose amount (MedTimer's ReminderEvent + stockBefore/stockAfter, org-native).
;; The allowlisted actions `harp.med.taken' / `harp.med.skipped' resolve a
;; medication by id in the current profile and write through here.

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'subr-x)
(require 'jetpacs-org)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
(require 'harp-profile)
(require 'harp-write)                   ; harp-write--save
(require 'harp-ui)
(require 'harp-med)
(require 'harp-metrics-parse)           ; harp-parse--num-or-nil

(defun harp-med-log--fmt-num (n)
  "Format number N without a trailing \".0\" for integers."
  (if (and (numberp n) (= n (truncate n)))
      (number-to-string (truncate n))
    (number-to-string n)))

(defun harp-med-log--amount-label (amount unit)
  "A dose amount label from AMOUNT and UNIT (either may be nil/blank)."
  (let ((a (and (stringp amount) (not (string-empty-p (string-trim amount)))
                (string-trim amount)))
        (u (and (stringp unit) (not (string-empty-p (string-trim unit)))
                (string-trim unit))))
    (cond ((and a u) (concat a " " u))
          (a a)
          (t ""))))

(defun harp-med-log--insert (state amount unit &optional at)
  "At the medication heading at point, prepend a dose log line.
AT (a Lisp time, default now) stamps the line — a dose taken offline is
logged at its `queued_at' time, not at reconnect.  Creates the :LOGBOOK:
drawer after the property drawer when absent."
  (org-back-to-heading t)
  (let* ((ts (format-time-string "[%Y-%m-%d %H:%M]" (or at (current-time))))
         (label (harp-med-log--amount-label amount unit))
         (line (concat "- " state " " ts
                       (if (string-empty-p label) "" (concat " " label))))
         (limit (save-excursion
                  (or (and (outline-next-heading) (point))
                      (org-end-of-subtree t t)))))
    (org-back-to-heading t)
    (if (re-search-forward "^[ \t]*:LOGBOOK:[ \t]*$" limit t)
        (progn (forward-line 1) (beginning-of-line) (insert line "\n"))
      (org-back-to-heading t)
      (org-end-of-meta-data)            ; past planning + property drawer
      ;; When the med is the last heading in a file with no trailing newline,
      ;; point lands mid-line at eob — without this the drawer fuses onto the
      ;; property drawer's :END: (":END::LOGBOOK:") and the med becomes
      ;; unparseable, vanishing from the app entirely.
      (unless (bolp) (insert "\n"))
      (insert ":LOGBOOK:\n" line "\n:END:\n"))))

(defun harp-med-log-write (ref state amount unit decrement &optional at)
  "Log a STATE dose for the med at REF; decrement :STOCK: by DECREMENT if set.
AMOUNT/UNIT annotate the log line; AT stamps it (default now).  Saves and
drops the read memo."
  (let ((marker (harp--resolve-ref ref)))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       ;; Decrement stock FIRST, while point is reliably on the heading —
       ;; inserting the LOGBOOK can shift the outline out from under a later
       ;; `org-back-to-heading'.  The marker (at the heading) survives both.
       (when (numberp decrement)
         (goto-char marker)
         (org-back-to-heading t)
         (let ((cur (harp-parse--num-or-nil (org-entry-get (point) "STOCK"))))
           (when (numberp cur)
             (org-entry-put (point) "STOCK"
                            (harp-med-log--fmt-num (max 0 (- cur decrement)))))))
       (goto-char marker)
       (org-back-to-heading t)
       (harp-med-log--insert state amount unit at)))
    (harp-write--save (marker-buffer marker))))

(defun harp-med-log--resolve (args)
  "Resolve the medication targeted by dose ARGS, across profiles.
Prefers the profile FILE baked into the action at render time (validated
against discovery) so a queued tap survives an Emacs restart or a different
profile being open; falls back to the current profile."
  (let ((id (alist-get 'id args))
        (file (alist-get 'file args)))
    (when (stringp id)
      (if (and (stringp file) (member file (harp-profile-files)))
          (harp-med-by-id id (harp-profile-read file))
        (harp-med-by-id id)))))

(defun harp-med-log--dose (args payload state)
  "Handle a taken/skipped ARGS dispatch for STATE.
PAYLOAD carries `queued_at' for events replayed from the offline queue."
  (let* ((med (harp-med-log--resolve args))
         (amount (alist-get 'amount args))
         (unit (or (alist-get 'unit args) (and med (alist-get 'unit med))))
         (queued (alist-get 'queued_at payload))
         (at (and (numberp queued) (seconds-to-time (/ queued 1000.0)))))
    (if med
        (progn
          (harp-med-log-write (alist-get 'ref med) state amount unit
                              ;; Only Taken consumes stock.
                              (and (equal state "taken")
                                   (harp-parse--num-or-nil amount))
                              at)
          (jetpacs-shell-notify
           (format "%s: %s" (alist-get 'name med) (capitalize state)))
          (jetpacs-shell-push))
      ;; Never drop a dose silently — the adherence record is the point.
      (display-warning
       'harp (format "Dose (%s) not recorded: medication %s not found%s"
                     state (alist-get 'id args)
                     (if (alist-get 'file args)
                         (format " in %s" (alist-get 'file args)) ""))
       :warning)
      (jetpacs-shell-notify "Dose not recorded — its medication wasn't found")
      (jetpacs-shell-push))))

(defun harp-med-log-refill (med)
  "Add MED's `:REFILL:' size to its `:STOCK:'.  Return the new stock, or nil.
The one-tap \"picked up a new pack\" affordance (MedTimer's refill)."
  (let ((refill (alist-get 'refill med)))
    (when (numberp refill)
      (let ((marker (harp--resolve-ref (alist-get 'ref med)))
            new)
        (with-current-buffer (marker-buffer marker)
          (org-with-wide-buffer
           (goto-char marker)
           (org-back-to-heading t)
           (let ((cur (or (harp-parse--num-or-nil (org-entry-get (point) "STOCK")) 0)))
             (setq new (+ cur refill))
             (org-entry-put (point) "STOCK" (harp-med-log--fmt-num new)))))
        (harp-write--save (marker-buffer marker))
        new))))

(with-jetpacs-owner "harp"
  (jetpacs-defaction "harp.med.refill"
    (lambda (args _)
      (let ((med (harp-med-log--resolve args)))
        (when med
          (when-let ((new (harp-med-log-refill med)))
            (jetpacs-shell-notify
             (format "%s: stock now %s %s" (alist-get 'name med)
                     (harp-med-log--fmt-num new) (or (alist-get 'unit med) "")))
            (jetpacs-shell-push)))))
    :doc "Add a medication's refill size to its stock."
    :args '((:name id :type "text" :required t)
            (:name file :type "text")))

  (jetpacs-defaction "harp.med.taken"
    (lambda (args payload) (harp-med-log--dose args payload "taken"))
    :doc "Log a medication dose as taken (decrements stock)."
    :args '((:name id :type "text" :required t)
            (:name amount :type "text")
            (:name unit :type "text")
            (:name file :type "text")))

  (jetpacs-defaction "harp.med.skipped"
    (lambda (args payload) (harp-med-log--dose args payload "skipped"))
    :doc "Log a medication dose as skipped."
    :args '((:name id :type "text" :required t)
            (:name amount :type "text")
            (:name unit :type "text")
            (:name file :type "text"))))

(provide 'harp-med-log)
;;; harp-med-log.el ends here
