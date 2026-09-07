;;; harp-med.el --- Harp medications domain layer -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1"))
;;; Commentary:
;; Accessors and predicates over the `medications' of a parsed Harp profile
;; (the `* Medications' section, read in `harp-profile.el').  A medication is
;; an alist: id, name, unit, stock, refill, low-stock, expires, active, notes,
;; tags, schedules, doses, ref.  A schedule is amount/times/days/cyclic/
;; interval/period; a dose is state (taken|skipped)/stamp/time/amount.
;;
;; MedTimer inspiration: Medicine.isOutOfStock / hasExpired / isStockManagement.
;; The scheduling engine lives in harp-med-schedule.el; logging in harp-med-log.el.

;;; Code:

(require 'cl-lib)
(require 'harp-profile)
(require 'harp-state)

(defun harp-med-medications (&optional profile)
  "Medications of PROFILE (default: the current profile)."
  (alist-get 'medications (or profile (harp-ui-current))))

(defun harp-med-by-id (id &optional profile)
  "The medication with ID in PROFILE, or nil."
  (cl-find id (harp-med-medications profile)
           :key (lambda (m) (alist-get 'id m)) :test #'equal))

;; ─── stock & expiration (MedTimer parity) ────────────────────────────────────

(defun harp-med-format-amount (n)
  "Format numeric N for display, keeping fractions: 2.0 → \"2\", 2.5 → \"2.5\".
Half a tablet of stock must not read as a whole one — round is wrong here."
  (cond ((not (numberp n)) (format "%s" n))
        ((= n (truncate n)) (number-to-string (truncate n)))
        (t (number-to-string n))))

(defun harp-med-active-p (med)
  "Non-nil when MED is active."
  (alist-get 'active med))

(defun harp-med-manages-stock-p (med)
  "Non-nil when MED tracks stock (a numeric stock or low-stock threshold)."
  (or (numberp (alist-get 'stock med)) (numberp (alist-get 'low-stock med))))

(defun harp-med-out-of-stock-p (med)
  "Non-nil when MED's stock has reached zero (or below)."
  (let ((stock (alist-get 'stock med)))
    (and (numberp stock) (<= stock 0))))

(defun harp-med-low-stock-p (med)
  "Non-nil when MED's stock is at or below its low-stock threshold."
  (let ((stock (alist-get 'stock med)) (low (alist-get 'low-stock med)))
    (and (numberp stock) (numberp low) (<= stock low))))

(defun harp-med-expired-p (med)
  "Non-nil when MED's expiration date is in the past."
  (let ((time (harp--time-of (alist-get 'expires med))))
    (and time (< time (float-time)))))

(defun harp-med-warnings (med)
  "A list of human-readable warning strings for MED (may be empty)."
  (delq nil
        (list (when (harp-med-expired-p med) "Expired")
              (cond ((harp-med-out-of-stock-p med) "Out of stock")
                    ((harp-med-low-stock-p med) "Low stock")))))

;; ─── dose events ─────────────────────────────────────────────────────────────

(defun harp-med-doses (med)
  "All logged dose events for MED."
  (alist-get 'doses med))

(defun harp-med-doses-on (med date-string)
  "MED's dose events whose stamp falls on DATE-STRING (\"YYYY-MM-DD\")."
  (cl-remove-if-not
   (lambda (d) (let ((s (alist-get 'stamp d)))
                 (and (stringp s) (string-prefix-p date-string s))))
   (harp-med-doses med)))

(defun harp-med-last-dose (med)
  "The most recent dose event for MED, or nil."
  (car (sort (copy-sequence (harp-med-doses med))
             (lambda (a b) (> (or (alist-get 'time a) 0)
                              (or (alist-get 'time b) 0))))))

(provide 'harp-med)
;;; harp-med.el ends here
