;;; harp-metrics-parse.el --- Inline #metric(value)/#tag and range parsing -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "30.1"))
;;; Commentary:
;; The one Harp-specific micro-parser.  Harp entry and document bodies carry
;; two inline tokens borrowed from harp-kmp's org dialect:
;;
;;   #tag            a bare hashtag (e.g. #routine)
;;   #id(value)      a metric reading (e.g. #hr(102), #pain(4))
;;
;; where `id' matches a metric's :TAG_ID: in the profile's Metadata.  This
;; module scans a body string into structured metric readings and tags, and
;; parses the "lo-hi" RANGE / HEALTHY_RANGE property strings.  Pure elisp.

;;; Code:

(require 'cl-lib)

(defconst harp-inline--regexp
  "#\\([A-Za-z0-9_-]+\\)\\(?:(\\([-+]?[0-9]*\\.?[0-9]+\\))\\)?"
  "Match an inline #tag or #metric(value).
Group 1 is the id/tag word; group 2 is the numeric value, present only for
the #id(value) form.")

(defun harp-inline-scan (text)
  "Scan TEXT for inline #tag and #metric(value) tokens.
Return a plist (:metrics ((ID . VALUE)...) :tags (TAG...)): ids and tags are
strings, values numbers.  Metric readings keep every occurrence in
first-seen order (mirroring harp-kmp); tags are de-duplicated."
  (let ((metrics '()) (tags '()) (start 0))
    (while (and (stringp text) (string-match harp-inline--regexp text start))
      (let ((word (match-string 1 text))
            (val (match-string 2 text)))
        (setq start (match-end 0))
        (if val
            (push (cons word (string-to-number val)) metrics)
          (unless (member word tags) (push word tags)))))
    (list :metrics (nreverse metrics) :tags (nreverse tags))))

(defun harp-inline-metrics (text)
  "Metric readings ((ID . VALUE)...) harvested from TEXT."
  (plist-get (harp-inline-scan text) :metrics))

(defun harp-inline-tags (text)
  "Hashtag strings harvested from TEXT."
  (plist-get (harp-inline-scan text) :tags))

(defun harp-parse--num-or-nil (s)
  "Parse S as a number, or nil when blank, \"null\", or non-numeric."
  (when (stringp s)
    (setq s (string-trim s))
    (when (and (not (string-empty-p s))
               (not (string-equal (downcase s) "null"))
               (string-match-p "\\`[-+]?[0-9]*\\.?[0-9]+\\'" s))
      (string-to-number s))))

(defun harp-parse-range (str)
  "Parse RANGE / HEALTHY_RANGE string STR \"lo-hi\" into a cons (LO . HI).
Either bound may be nil.  Mirrors harp-kmp: split on \"-\" into exactly two
parts, otherwise (nil . nil)."
  (if (not (stringp str))
      (cons nil nil)
    (let ((parts (split-string (string-trim str) "-")))
      (if (= (length parts) 2)
          (cons (harp-parse--num-or-nil (nth 0 parts))
                (harp-parse--num-or-nil (nth 1 parts)))
        (cons nil nil)))))

(provide 'harp-metrics-parse)
;;; harp-metrics-parse.el ends here
