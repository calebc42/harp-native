;;; harp-med-log.el --- Harp dose logging -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1"))
;;; Commentary:
;; Recording a dose Taken or Skipped: prepend a `- STATE [ts] AMOUNT' line to
;; the medication's :LOGBOOK: drawer and, on Taken, decrement its :STOCK: by
;; the dose amount (MedTimer's ReminderEvent + stockBefore/stockAfter, org-native).
;; The allowlisted actions `harp.med.taken' / `harp.med.skipped' resolve a
;; medication by id in the current profile and write through here.

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'subr-x)
(require 'ebp-org)


(require 'harp-profile)
(require 'harp-write)                   ; harp-write--save
(require 'harp-state)
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
  "Prepend a STATE dose with AMOUNT and UNIT at the medication heading.
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

(provide 'harp-med-log)
;;; harp-med-log.el ends here
