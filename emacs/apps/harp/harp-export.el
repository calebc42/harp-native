;;; harp-export.el --- Harp profile export & share -*- lexical-binding: t; -*-

;; Export a profile for sharing with a care provider: as Org text (shared
;; straight over the bridge via ACTION_SEND — host-agnostic, no file
;; transfer) or as a self-contained HTML timeline written to
;; `harp-export-directory'.  Ports harp-kmp's Export.kt `exportText` /
;; `exportHTML`; media embedding (the zip) is deferred with attachments.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'jetpacs-widgets)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
(require 'jetpacs-device)
(require 'harp-ui)
(require 'harp-write)

(defcustom harp-export-directory nil
  "Directory HTML exports are written to.
nil means an `exports/' subdirectory of `harp-directory'."
  :type '(choice (const :tag "exports/ under harp-directory" nil) directory)
  :group 'harp)

(defun harp-export--dir ()
  "The resolved export directory."
  (or harp-export-directory (expand-file-name "exports/" harp-directory)))

;; ─── privacy filter ──────────────────────────────────────────────────────────

(defun harp-export--filter-profile (profile include-private)
  "PROFILE with `:PRIVATE: t' entries and documents removed.
INCLUDE-PRIVATE non-nil returns PROFILE unchanged.  An export exists to be
handed to a care provider — private items stay home unless asked for."
  (if include-private
      profile
    (let ((copy (copy-alist profile)))
      (setf (alist-get 'journals copy)
            (mapcar (lambda (j)
                      (let ((jc (copy-alist j)))
                        (setf (alist-get 'entries jc)
                              (cl-remove-if (lambda (e) (alist-get 'private e))
                                            (alist-get 'entries j)))
                        jc))
                    (alist-get 'journals profile)))
      (setf (alist-get 'documents copy)
            (cl-remove-if (lambda (d) (alist-get 'private d))
                          (alist-get 'documents profile)))
      copy)))

;; ─── Org text export (mirrors harp-kmp exportText) ──────────────────────────

(defun harp-export--entry-text (e)
  "Org text for entry E, reusing the write layer's subtree builder."
  (harp-write--entry-text (alist-get 'id e) (alist-get 'datetime e)
                          (alist-get 'heading-tags e) (alist-get 'private e)
                          (alist-get 'body e)))

(defun harp-export--journal-text (j)
  "Org text for journal J and its entries."
  (concat (format "** %s\n" (alist-get 'name j))
          (mapconcat #'harp-export--entry-text (alist-get 'entries j) "\n")))

(defun harp-export--document-text (d)
  "Org text for document D, reusing the write layer's subtree builder."
  (harp-write--document-text (alist-get 'id d) (alist-get 'name d)
                             (alist-get 'datetime d) (alist-get 'source d)
                             (alist-get 'files d) (alist-get 'heading-tags d)
                             (alist-get 'private d) (alist-get 'annotation d)))

(defun harp-export-text (profile &optional description include-private)
  "Build a shareable Org document for PROFILE (journals + documents).
Private items are excluded unless INCLUDE-PRIVATE."
  (setq profile (harp-export--filter-profile profile include-private))
  (let ((journals (cl-remove-if (lambda (j) (null (alist-get 'entries j)))
                                (alist-get 'journals profile))))
    (concat
     ":PROPERTIES:\n"
     (format ":ID: %s\n" (alist-get 'uuid profile))
     (format ":HARP_TYPE: %s\n" (alist-get 'type profile))
     ":END:\n"
     (format "#+TITLE: %s\n" (alist-get 'name profile))
     (if (and description (not (string-empty-p description)))
         (format "#+DESCRIPTION: %s\n" description) "")
     "\n* Journals\n"
     (mapconcat #'harp-export--journal-text journals "\n")
     "\n\n* Documents\n"
     (mapconcat #'harp-export--document-text (alist-get 'documents profile) "\n")
     "\n")))

;; ─── HTML export (mirrors harp-kmp exportHTML, no embedded media) ────────────

(defun harp-export--escape (s)
  "HTML-escape S."
  (if (stringp s)
      (replace-regexp-in-string
       ">" "&gt;"
       (replace-regexp-in-string
        "<" "&lt;"
        (replace-regexp-in-string "&" "&amp;" s)))
    ""))

(defconst harp-export--css "\
*{box-sizing:border-box}
body{margin:0;padding:1.25rem;font-family:system-ui,-apple-system,\"Segoe UI\",sans-serif;line-height:1.6;color:#111;background:#fff}
main{max-width:42rem;margin:auto}
article{margin-bottom:2rem;padding:1.25rem 1.5rem;border-radius:12px;background:#fafafa}
h1{font-size:1.8rem;margin-bottom:1rem}
h3{margin:0 0 .25rem;font-size:1.1rem}
time{display:block;font-size:.85rem;color:#666;margin-bottom:.75rem}
.content{white-space:pre-wrap}
.files{margin-top:.75rem;font-size:.85rem;color:#555}
@media (max-width:480px){body{padding:1rem}h1{font-size:1.5rem}}")

(defun harp-export--timeline (profile)
  "Entries and documents of PROFILE as timeline items, newest first."
  (let (items)
    (dolist (j (alist-get 'journals profile))
      (dolist (e (alist-get 'entries j))
        (push (cons 'entry e) items)))
    (dolist (d (alist-get 'documents profile))
      (push (cons 'document d) items))
    (sort items (lambda (a b) (> (or (alist-get 'time (cdr a)) 0)
                                 (or (alist-get 'time (cdr b)) 0))))))

(defun harp-export--article (item)
  "One HTML <article> for timeline ITEM (a (KIND . ALIST) cons)."
  (let* ((kind (car item)) (x (cdr item)))
    (concat
     "<article>\n"
     (if (eq kind 'document)
         (format "<h3>%s</h3>\n" (harp-export--escape (alist-get 'name x)))
       "")
     (format "<time>%s</time>\n"
             (harp-export--escape (harp-ui-format-stamp (alist-get 'datetime x) t)))
     (format "<div class=\"content\">%s</div>\n"
             (harp-export--escape (or (alist-get (if (eq kind 'document) 'annotation 'body) x) "")))
     (let ((files (alist-get 'assets x)))
       (if files
           (format "<div class=\"files\">Attachments: %s</div>\n"
                   (harp-export--escape (string-join files ", ")))
         ""))
     "</article>")))

(defun harp-export-html (profile &optional description include-private)
  "Build a self-contained HTML timeline for PROFILE.
Private items are excluded unless INCLUDE-PRIVATE."
  (setq profile (harp-export--filter-profile profile include-private))
  (concat
   "<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n"
   "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
   (format "<title>%s | Harp Export</title>\n" (harp-export--escape (alist-get 'name profile)))
   "<style>" harp-export--css "</style>\n</head>\n<body>\n<main>\n"
   (format "<h1>%s</h1>\n" (harp-export--escape (alist-get 'name profile)))
   (if (and description (not (string-empty-p description)))
       (format "<p>%s</p>\n" (harp-export--escape description)) "")
   (format "<h4>Exported via Harp on %s</h4>\n"
           (format-time-string "%b %-d, %Y %H:%M"))
   (mapconcat #'harp-export--article (harp-export--timeline profile) "\n")
   "\n</main>\n</body>\n</html>\n"))

(defun harp-export--timestamped-file (profile ext)
  "A fresh export path for PROFILE with extension EXT under `harp-export--dir'."
  (let ((dir (harp-export--dir)))
    (make-directory dir t)
    (expand-file-name
     (format "%s-%s.%s"
             (harp-write--profile-slug (alist-get 'name profile))
             (format-time-string "%Y%m%d-%H%M%S")
             ext)
     dir)))

(defun harp-export-html-to-file (profile &optional include-private)
  "Write PROFILE's HTML export under `harp-export--dir'; return the path."
  (let ((file (harp-export--timestamped-file profile "html")))
    (with-temp-file file
      (insert (harp-export-html profile nil include-private)))
    file))

;; ─── share / dialog / chrome ─────────────────────────────────────────────────

(defcustom harp-export-share-max-bytes 200000
  "Largest Org export shared inline via ACTION_SEND EXTRA_TEXT.
Intent extras ride a bounded binder transaction (~1MB hard, less in
practice); over this cap the export is written to `harp-export-directory'
instead of firing the intent."
  :type 'integer :group 'harp)

(defun harp-export--share-text (text subject profile)
  "Share TEXT via ACTION_SEND with SUBJECT — host-agnostic.
Over `harp-export-share-max-bytes' the intent would be dropped by the
binder, so the text is saved under `harp-export--dir' instead.  Returns
the symbol `shared', or the file path when it fell back to a file."
  (if (> (string-bytes text) harp-export-share-max-bytes)
      (let ((file (harp-export--timestamped-file profile "org")))
        (with-temp-file file (insert text))
        file)
    (jetpacs-device-intent
     :action "android.intent.action.SEND"
     :mime "text/plain"
     :extras `(("android.intent.extra.TEXT" . ,text)
               ("android.intent.extra.SUBJECT" . ,subject)))
    'shared))

(defconst harp-export--private-field "harp-export-private"
  "UI-state id backing the export dialog's Include-private switch.")

(defun harp-export--private-p ()
  "Whether the export dialog's Include-private switch is on.
Read from UI state, not an on_change action: the reference companion emits
only `state.changed' for a switch (no action dispatch), so the switch's
action would never fire — but the value is faithfully tracked in UI state."
  (let ((v (jetpacs-ui-state harp-export--private-field)))
    (and v (not (eq v :false)))))

(defun harp-export--dialog-body ()
  "The export format chooser's spec column."
  (jetpacs-column
   (jetpacs-text "Export" 'title)
   (jetpacs-text (or (alist-get 'name (harp-ui-current)) "") 'caption)
   (jetpacs-switch harp-export--private-field
                   :label "Include private items"
                   :checked (harp-export--private-p))
   (jetpacs-button "Share as Org text"
                   (jetpacs-action "harp.export.run" :args '((format . "org"))
                                   :when-offline "drop")
                   :variant "outlined")
   (jetpacs-button "Save as HTML"
                   (jetpacs-action "harp.export.run" :args '((format . "html"))
                                   :when-offline "drop")
                   :variant "outlined")
   (jetpacs-button "Cancel"
                   (jetpacs-action "harp.export.cancel" :when-offline "drop")
                   :variant "text")))

(defun harp-export--dialog ()
  "Show the export format chooser as a bottom sheet."
  (jetpacs-send-dialog (harp-export--dialog-body) "sheet"))

(with-jetpacs-owner "harp"

  (jetpacs-shell-add-top-action
   20
   (lambda ()
     (when (harp-ui-current)
       (jetpacs-icon-button "ios_share"
                            (jetpacs-action "harp.export.open" :when-offline "drop")
                            :content-description "Export"))))

  (jetpacs-defaction "harp.export.open"
    (lambda (_args _)
      (when (harp-ui-current)
        ;; Every export starts private-safe; including private items is a
        ;; per-export opt-in, never a sticky preference — clear the switch's
        ;; UI state so a prior export can't silently re-enable it.
        (jetpacs-ui-state-put harp-export--private-field nil)
        (harp-export--dialog)))
    :doc "Open the export format chooser.")

  (jetpacs-defaction "harp.export.cancel"
    (lambda (_args _) nil)
    :doc "Dismiss the export chooser.")

  (jetpacs-defaction "harp.export.run"
    (lambda (args _)
      (let ((profile (harp-ui-current))
            (format (alist-get 'format args))
            (private (harp-export--private-p)))
        (when profile
          (pcase format
            ("org"
             (let ((result (harp-export--share-text
                            (harp-export-text profile nil private)
                            (format "%s — Harp export" (alist-get 'name profile))
                            profile)))
               (jetpacs-shell-notify
                (if (eq result 'shared)
                    "Shared as Org text"
                  (format "Too large to share — saved to %s"
                          (abbreviate-file-name result))))))
            ("html"
             (let ((file (harp-export-html-to-file profile private)))
               (jetpacs-shell-notify
                (format "HTML saved to %s" (abbreviate-file-name file))))))
          ;; notify only queues the snackbar — push so the result actually
          ;; shows (the export sheet dispatches no re-render on its own).
          (jetpacs-shell-push))))
    :doc "Export the current profile in the given format (org shares, html saves)."
    :args '((:name format :type "text" :required t))))

(provide 'harp-export)
;;; harp-export.el ends here
