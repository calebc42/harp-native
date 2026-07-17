;;; harp-ui.el --- Harp shared UI state and spec helpers -*- lexical-binding: t; -*-

;; The current-profile state and the small spec helpers every Harp view
;; shares: type icons, friendly timestamps, tag/metric/attachment chips, the
;; tag filter bar, and the "no profile open" placeholder.  Reading a profile
;; is memoised on (file . mtime) so one push renders every tab from a single
;; parse; the memo busts when the file changes on disk or a mutation calls
;; `harp-ui-invalidate'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'jetpacs-widgets)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
(require 'harp-profile)

;; ─── current profile state ──────────────────────────────────────────────────

(defvar harp-ui--current-file nil
  "Absolute path of the open profile Org file, or nil when none is open.")

(defvar harp-ui--cache nil
  "Memo cons (KEY . PROFILE) for the current profile read.")

(defvar harp-ui--derived nil
  "Push-scoped memo (GEN . HASH) for derived queries.
GEN mirrors `harp-ui--cache''s key, so the memo busts on the same
\(file . mtime) change that re-reads the profile; `harp-ui-invalidate'
clears it outright for the no-profile-open case (where GEN stays nil).
A shell push builds every visible tab, so a derived query (tags,
due-today, the launcher sweep) would otherwise recompute several times.")

(defvar harp-ui--tag-filter nil
  "Active tag filter string, or nil for no filter.")

(defcustom harp-hide-private nil
  "When non-nil, entries and documents marked :PRIVATE: are hidden from views."
  :type 'boolean
  :group 'harp)

(defun harp-ui-invalidate ()
  "Drop the memoised profile read and derived memos (call after any mutation)."
  (setq harp-ui--cache nil
        harp-ui--derived nil))

(defun harp-ui-set-current (file)
  "Open FILE as the current profile, resetting the read memo and tag filter."
  (setq harp-ui--current-file file
        harp-ui--cache nil
        harp-ui--derived nil
        harp-ui--tag-filter nil))

(defun harp-ui-memo (key thunk)
  "Return (funcall THUNK), memoised under KEY for the current profile read.
Every builder in one shell push shares the result, and the memo busts on
the same (file . mtime) change or explicit invalidate as `harp-ui-current'."
  ;; Force the profile read first so GEN reflects the current (file . mtime),
  ;; independent of whether a builder called `harp-ui-current' before us — else
  ;; a memo call ordered first would key the table under a stale generation.
  (harp-ui-current)
  (let ((gen (car harp-ui--cache)))
    (unless (and harp-ui--derived (equal (car harp-ui--derived) gen))
      (setq harp-ui--derived (cons gen (make-hash-table :test 'equal))))
    (let* ((tbl (cdr harp-ui--derived))
           (cell (gethash key tbl)))
      (if cell (car cell)
        (car (puthash key (list (funcall thunk)) tbl))))))

(defun harp-ui-current-tags ()
  "Tags of the current profile, memoised for the push (built by two tabs)."
  (harp-ui-memo 'tags (lambda () (harp-profile-tags (harp-ui-current)))))

(defun harp-ui-profiles ()
  "Discovered profiles, memoised for the push (the launcher's directory sweep)."
  (harp-ui-memo 'profiles #'harp-profiles))

(defun harp-ui-profile-open-p ()
  "Non-nil when a profile is open — the `:when' gate for section tabs."
  (and (harp-ui-current) t))

(defun harp-ui-current ()
  "The current profile alist, memoised on (file . mtime); nil when none open."
  (when (and harp-ui--current-file (file-readable-p harp-ui--current-file))
    (let ((key (cons harp-ui--current-file
                     (file-attribute-modification-time
                      (file-attributes harp-ui--current-file)))))
      (unless (equal (car harp-ui--cache) key)
        (setq harp-ui--cache
              (cons key (harp-profile-read harp-ui--current-file))))
      (cdr harp-ui--cache))))

;; ─── formatting helpers ─────────────────────────────────────────────────────

(defun harp-ui-type-icon (type)
  "Material icon name for a profile TYPE."
  (pcase type ("cat" "pets") ("dog" "pets") (_ "person")))

(defun harp-ui-type-label (type)
  "Human label for a profile TYPE."
  (pcase type ("cat" "Cat") ("dog" "Dog") (_ "Human")))

(defun harp-ui-format-stamp (dt &optional with-time)
  "Friendly label for Org timestamp string DT (e.g. \"Jan 3, 2026\").
With WITH-TIME non-nil, append the clock time when present."
  (if-let* ((time (harp--time-of dt)))
      (let* ((has-hm (and (stringp dt) (string-match-p "[0-9]:[0-9]" dt)))
             (fmt (if (and with-time has-hm) "%b %-d, %Y · %H:%M" "%b %-d, %Y")))
        (format-time-string fmt (seconds-to-time time)))
    (or dt "")))

;; ─── chips ──────────────────────────────────────────────────────────────────

(defun harp-ui-tag-chips (tags)
  "A flow-row of tappable #TAGS (each filters the active view), or nil."
  (when tags
    (apply #'jetpacs-flow-row
           (mapcar (lambda (tg)
                     (jetpacs-assist-chip (concat "#" tg)
                                          :on-tap (jetpacs-action
                                                   "harp.filter.tag"
                                                   :args `((tag . ,tg))
                                                   :when-offline "drop")))
                   tags))))

(defun harp-ui-metric-chips (metric-values)
  "A flow-row of metric readings as chips (\"id = value\"), or nil."
  (when metric-values
    (apply #'jetpacs-flow-row
           (mapcar (lambda (mv)
                     (jetpacs-assist-chip
                      (format "%s = %s" (alist-get 'id mv) (alist-get 'value mv))
                      :icon "monitoring"))
                   metric-values))))

(defun harp-ui-attachment-chips (assets)
  "A flow-row of attachment file names as chips, or nil."
  (when assets
    (apply #'jetpacs-flow-row
           (mapcar (lambda (name)
                     (jetpacs-assist-chip name :icon "attach_file"))
                   assets))))

(defun harp-ui-body-nodes (text)
  "Spec nodes for an entry/annotation body TEXT (org markup), or nil."
  (when (and (stringp text) (not (string-empty-p (string-trim text))))
    (list (jetpacs-markup (string-trim text) :syntax "org"))))

;; ─── tag filter ─────────────────────────────────────────────────────────────

(defun harp-ui-item-visible-p (item)
  "Non-nil when ITEM passes the private-hiding setting and the tag filter."
  (and (not (and harp-hide-private (alist-get 'private item)))
       (or (null harp-ui--tag-filter)
           (member harp-ui--tag-filter (alist-get 'tags item)))))

(defun harp-ui-filter-bar (tags)
  "A filter chip row over TAGS, with the active one selected; nil if no tags."
  (when tags
    (apply #'jetpacs-flow-row
           (jetpacs-chip "All"
                         :selected (null harp-ui--tag-filter)
                         :on-tap (jetpacs-action "harp.filter.clear"
                                                 :when-offline "drop"))
           (mapcar (lambda (tg)
                     (jetpacs-chip (concat "#" tg)
                                   :selected (equal tg harp-ui--tag-filter)
                                   :on-tap (jetpacs-action "harp.filter.tag"
                                                           :args `((tag . ,tg))
                                                           :when-offline "drop")))
                   tags))))

;; ─── placeholders ───────────────────────────────────────────────────────────

(defun harp-ui-no-profile-body ()
  "Placeholder body shown on a section tab when no profile is open."
  (jetpacs-empty-state
   :icon "folder_open"
   :title "No profile open"
   :caption "Open a profile from the Home tab to see its records."))

;; ─── filter actions (owned by the app) ──────────────────────────────────────

(with-jetpacs-owner "harp"
  (jetpacs-defaction "harp.filter.tag"
    (lambda (args _)
      (let ((tag (alist-get 'tag args)))
        (setq harp-ui--tag-filter
              (if (equal tag harp-ui--tag-filter) nil tag))
        (jetpacs-shell-push)))
    :doc "Toggle the active view's tag filter."
    :args '((:name tag :type "text" :required t)))

  (jetpacs-defaction "harp.filter.clear"
    (lambda (_args _)
      (setq harp-ui--tag-filter nil)
      (jetpacs-shell-push))
    :doc "Clear the active view's tag filter."))

;; Leaving a section resets its tag filter — a fresh view starts unfiltered.
(defun harp-ui--on-view-switched (_view)
  "Reset the tag filter when the shell switches views."
  (setq harp-ui--tag-filter nil))

(add-hook 'jetpacs-shell-view-switched-hook #'harp-ui--on-view-switched)

;; Pull-to-refresh and queue drains drop the profile memo.
(add-hook 'jetpacs-shell-refresh-hook #'harp-ui-invalidate)

(provide 'harp-ui)
;;; harp-ui.el ends here
