;;; harp-current-test.el --- Harp snapshot and durability contracts -*- lexical-binding: t; -*-
;;; Code:
(require 'ert)
(require 'harp)
(harp-register)  ; `require' only loads
(require 'jetpacs-shell)
(defconst harp-test-profile
  ":PROPERTIES:\n:ID: synthetic-example\n:HARP_TYPE: human\n:END:\n#+TITLE: Example Profile\n* Metadata\n** Sources\n** Metrics\n*** Example reading\n:PROPERTIES:\n:TAG_ID: sample\n:UNIT: units\n:HEALTHY_RANGE: 0-10\n:END:\n* Journals\n** Main\n*** Entry\n:PROPERTIES:\n:ID: example-entry\n:DATETIME: <2026-09-06 12:00>\n:END:\nSynthetic journal #sample(3).\n*** Entry\n:PROPERTIES:\n:ID: example-earlier-one\n:DATETIME: <2026-09-04 12:00>\n:END:\nSynthetic earlier reading #sample(2).\n*** Entry\n:PROPERTIES:\n:ID: example-earlier-two\n:DATETIME: <2026-09-05 10:00>\n:END:\nSynthetic earlier reading #sample(4).\n*** Entry\n:PROPERTIES:\n:ID: example-private\n:DATETIME: <2026-09-05 12:00>\n:PRIVATE: t\n:END:\nPRIVATE_SENTINEL #sample(99)\n* Documents\n** Example report\n:PROPERTIES:\n:ID: example-document\n:DATETIME: <2026-09-04 12:00>\n:SOURCE: Example source\n:FILES: example.pdf\n:END:\nSynthetic annotation.\n* Reminders\n** TODO Example reminder\nSCHEDULED: <2026-09-06 12:00>\n:PROPERTIES:\n:ID: example-reminder\n:END:\n* Medications\n** Example medication\n:PROPERTIES:\n:ID: example-med\n:UNIT: unit\n:STOCK: 10\n:REFILL: 5\n:ACTIVE: t\n:END:\n*** Schedule\n:PROPERTIES:\n:AMOUNT: 1\n:TIMES: 09:00, 20:00\n:END:\n"
  "Entirely synthetic records used only in temporary test directories.")

(defmacro harp-test-with-snapshot (&rest body)
  "Run BODY with synthetic files and a fixed producer clock."
  (declare (indent 0))
  `(let* ((harp-directory (make-temp-file "harp-test-" t))
          (harp-ui--current-file (expand-file-name "example.org" harp-directory))
          (harp-state nil) (harp-state-epoch "synthetic-fixture") (harp-state-generation 0) (harp-state-page 0) (harp-state-tag nil)
          (harp-hide-private t) (harp-state-dark nil) (harp--timers nil)
          (org-id-track-globally nil) (org-agenda-files nil)
          (ebp-org-roots (list harp-directory)))
     (unwind-protect
         (progn
           (with-temp-file harp-ui--current-file (insert harp-test-profile))
           (harp-refresh (encode-time 0 0 12 6 9 2026))
           (cl-letf (((symbol-function 'harp--later) (lambda (_fn) nil))) ,@body))
       (dolist (buf (buffer-list))
         (when (and (buffer-file-name buf) (file-in-directory-p (buffer-file-name buf) harp-directory))
           (with-current-buffer buf (set-buffer-modified-p nil)) (kill-buffer buf)))
       (delete-directory harp-directory t))))

(defun harp-test-entry ()
  "Return the public fixture entry."
  (car (alist-get 'entries (car (alist-get 'journals (harp-ui-current))))))
(defun harp-test-args (&optional kind item)
  "Return current form arguments for KIND and optional ITEM."
  (list :form (or kind "entry") :id (and item (alist-get 'token item)) :revision (plist-get harp-state :revision)))
(defun harp-test-fields (args &optional changes)
  "Return captured fields for ARGS, overriding logical keys with CHANGES."
  (let* ((kind (plist-get args :form))
         (item (and (plist-get args :id) (plist-get (harp-resolve (plist-get args :id)) :item)))
         (harp-field-scope (harp-form-scope args)) fields)
    (dolist (field (cdr (assoc kind harp-form-schema)))
      (setq fields (plist-put fields (intern (concat ":" (harp-field-id (car field))))
                             (if-let* ((pair (assq (car field) changes))) (cdr pair) (harp-ui-default (car field) item)))))
    (list :fields fields)))

(ert-deftest harp-current-snapshot-uses-org ()
  (harp-test-with-snapshot
    (should (equal "Example Profile" (alist-get 'name (harp-ui-current))))
    (should (= 4 (length (alist-get 'entries (car (alist-get 'journals (harp-ui-current)))))))
    (should (= 2 (length (plist-get harp-state :doses))))))

(ert-deftest harp-current-builders-do-not-read-or-mutate ()
  (harp-test-with-snapshot
    (let ((before (copy-tree harp-state)) (native-comp-enable-subr-trampolines nil))
      (cl-letf (((symbol-function 'insert-file-contents) (lambda (&rest _) (error "Builder read file")))
                ((symbol-function 'current-time) (lambda () (error "Builder read clock")))
                ((symbol-function 'harp-refresh) (lambda (&rest _) (error "Builder refreshed"))))
        (dolist (view harp-views)
          (let* ((one (harp-ui-screen (list :view view))) (two (harp-ui-screen (list :view view))))
            (should (equal (jetpacs-node->canonical-json one) (jetpacs-node->canonical-json two)))
            (jetpacs-shell--gate-ids one nil))))
      (should (equal before harp-state)))))

(ert-deftest harp-current-all-forms-build-and-scope-fields ()
  (harp-test-with-snapshot
    (dolist (schema harp-form-schema)
      (let ((screen (harp-ui-screen (harp-test-args (car schema)))))
        (should (stringp (jetpacs-node->canonical-json screen)))
        (jetpacs-shell--gate-ids screen nil)))))

(ert-deftest harp-current-save-is-durable-and-old-token-stale ()
  (harp-test-with-snapshot
    (let* ((args (harp-test-args "entry" (harp-test-entry)))
           (fields (harp-test-fields args '((body . "Changed synthetic content")))))
      (should (eq 'accepted (harp--on-save args fields)))
      (with-temp-buffer (insert-file-contents harp-ui--current-file) (should (search-forward "Changed synthetic content" nil t)))
      (should (eq 'stale (harp--on-save args fields))))))

(ert-deftest harp-current-save-failure-rolls-back-buffer ()
  (harp-test-with-snapshot
    (let* ((args (harp-test-args "entry" (harp-test-entry))) (fields (harp-test-fields args '((body . "Must roll back")))))
      (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) (error "Synthetic disk failure"))))
        (should (eq 'rejected (harp--on-save args fields))))
      (with-current-buffer (get-file-buffer harp-ui--current-file)
        (should-not (string-match-p "Must roll back" (buffer-string)))))))

(ert-deftest harp-current-refresh-failure-does-not-reject-saved-effect ()
  (harp-test-with-snapshot
    (let ((args (harp-test-args "entry" (harp-test-entry))))
      (cl-letf (((symbol-function 'harp-refresh) (lambda (&rest _) (error "Refresh unavailable"))))
        (should (eq 'accepted (harp--on-save args (harp-test-fields args))))))))

(ert-deftest harp-current-external-edits-are-stale ()
  (harp-test-with-snapshot
    (let* ((args (harp-test-args "entry" (harp-test-entry))) (fields (harp-test-fields args)))
      (write-region "\nExternal change\n" nil harp-ui--current-file t 'silent)
      (should (eq 'stale (harp--on-save args fields))))))

(ert-deftest harp-current-unsaved-buffer-is-preserved ()
  (harp-test-with-snapshot
    (with-current-buffer (get-file-buffer harp-ui--current-file)
      (goto-char (point-max)) (insert "UNSAVED")
      (should-error (harp-refresh)) (should (string-suffix-p "UNSAVED" (buffer-string))))))

(ert-deftest harp-current-rejects-missing-fields-and-date-injection ()
  (harp-test-with-snapshot
    (let ((args (harp-test-args "entry" (harp-test-entry))))
      (should (eq 'rejected (harp--on-save args nil)))
      (dolist (date '("2026-02-31" "2026-09-06\n* Injected"))
        (should (eq 'rejected (harp--on-save args (harp-test-fields args (list (cons 'date date))))))))))

(ert-deftest harp-current-private-readings-and-exports-stay-hidden ()
  (harp-test-with-snapshot
    (should-not (string-match-p "PRIVATE_SENTINEL" (jetpacs-node->canonical-json (harp-ui-screen '(:view "journal")))))
    (should-not (string-match-p "PRIVATE_SENTINEL" (harp-export-text (harp-ui-current))))
    (should-not (string-match-p "PRIVATE_SENTINEL" (harp-export-html (harp-ui-current))))))

(ert-deftest harp-current-dose-selects-exact-time-and-rejects-replay ()
  (harp-test-with-snapshot
    (let* ((dose (cadr (plist-get harp-state :doses))) (token (plist-get dose :token))
           (args (list :id token :state "taken")))
      (should (eq 'accepted (harp--on-dose args nil)))
      (should (= 9 (alist-get 'stock (car (alist-get 'medications (harp-ui-current))))))
      (should (eq 'stale (harp--on-dose args nil)))
      (harp-refresh (encode-time 0 0 12 6 9 2026))
      (should (equal '("09:00") (mapcar (lambda (x) (alist-get 'time (plist-get x :dose))) (plist-get harp-state :doses)))))))

(ert-deftest harp-current-dose-amount-is-not-device-controlled ()
  (harp-test-with-snapshot
    (should (eq 'accepted (harp--on-dose (list :id (plist-get (car (plist-get harp-state :doses)) :token) :state "taken" :amount "999") nil)))
    (should (= 9 (alist-get 'stock (car (alist-get 'medications (harp-ui-current))))))))

(ert-deftest harp-current-canonical-containment-rejects-escape ()
  (harp-test-with-snapshot
    (let ((outside (make-temp-file "harp-outside-" nil ".org")))
      (unwind-protect
          (progn (make-symbolic-link outside (expand-file-name "escape.org" harp-directory))
                 (should-error (harp-safe-file (expand-file-name "escape.org" harp-directory))))
        (delete-file outside)))))

(ert-deftest harp-current-lifecycle-removes-owned-state ()
  (harp-test-with-snapshot
    (harp-register) (harp-register) (harp-unregister)
    (should-not harp-state) (should-not harp--timers)
    (should-not (assoc "harp" jetpacs-apps--registry))
    (harp-register)))

(ert-deftest harp-current-capture-every-record-kind ()
  (harp-test-with-snapshot
    (dolist (example '(("entry" (body . "New synthetic entry"))
                       ("document" (name . "New synthetic report"))
                       ("metric" (name . "New reading") (id . "newreading") (unit . "units"))
                       ("medication" (name . "New medication") (unit . "unit") (amount . "1") (times . "12:00"))
                       ("reminder" (title . "New reminder"))))
      (let ((args (harp-test-args (car example))))
        (should (eq 'accepted (harp--on-save args (harp-test-fields args (cdr example)))))))
    (should (= 2 (length (alist-get 'documents (harp-ui-current)))))
    (should (= 2 (length (alist-get 'metrics (harp-ui-current)))))
    (should (= 2 (length (alist-get 'medications (harp-ui-current)))))))

(ert-deftest harp-current-metric-edit-resolves-idless-org-heading ()
  (harp-test-with-snapshot
    (let* ((metric (car (alist-get 'metrics (harp-ui-current))))
           (args (harp-test-args "metric" metric)))
      (should (eq 'accepted (harp--on-save args (harp-test-fields args '((name . "Renamed reading"))))))
      (should (equal "Renamed reading" (alist-get 'name (car (alist-get 'metrics (harp-ui-current)))))))))

(ert-deftest harp-current-medication-edit-preserves-unmodeled-properties ()
  (harp-test-with-snapshot
    (with-current-buffer (get-file-buffer harp-ui--current-file)
      (goto-char (point-min)) (search-forward "** Example medication")
      (org-entry-put (point) "EXAMPLE_CUSTOM" "preserve") (save-buffer))
    (harp-refresh)
    (let* ((med (car (alist-get 'medications (harp-ui-current))))
           (args (harp-test-args "medication" med)))
      (should (eq 'accepted (harp--on-save args (harp-test-fields args '((name . "Renamed medication")))))))
    (with-current-buffer (get-file-buffer harp-ui--current-file)
      (goto-char (point-min)) (search-forward "** Renamed medication")
      (should (equal "preserve" (org-entry-get (point) "EXAMPLE_CUSTOM"))))))

(ert-deftest harp-current-refill-and-skip-have-distinct-stock-effects ()
  (harp-test-with-snapshot
    (should (eq 'accepted (harp--on-refill (list :id (alist-get 'token (car (alist-get 'medications (harp-ui-current))))) nil)))
    (should (= 15 (alist-get 'stock (car (alist-get 'medications (harp-ui-current))))))
    (harp-refresh (encode-time 0 0 12 6 9 2026))
    (should (eq 'accepted (harp--on-dose (list :id (plist-get (car (plist-get harp-state :doses)) :token) :state "skipped") nil)))
    (should (= 15 (alist-get 'stock (car (alist-get 'medications (harp-ui-current))))))))

(ert-deftest harp-current-export-files-are-not-discovered-as-profiles ()
  (harp-test-with-snapshot
    (dolist (format '("org" "html"))
      (should (eq 'accepted (harp--on-export (list :format format) nil))))
    (should (= 1 (length (harp-discover))))
    (dolist (file (directory-files harp-directory t "harp-export-"))
      (with-temp-buffer (insert-file-contents file)
        (should-not (string-match-p "PRIVATE_SENTINEL" (buffer-string)))))))

(ert-deftest harp-current-new-profile-is-durable ()
  (harp-test-with-snapshot
    (let ((args (harp-test-args "profile")))
      (should (eq 'accepted (harp--on-save args (harp-test-fields args '((name . "Another example") (type . "cat")))))))
    (should (= 2 (length (plist-get harp-state :profiles))))
    (should (equal "cat" (alist-get 'type (harp-ui-current))))))

(ert-deftest harp-current-reminder-toggle-and-delete-commit ()
  (harp-test-with-snapshot
    (should (eq 'accepted (harp--on-reminder (list :id (alist-get 'token (car (alist-get 'reminders (harp-ui-current))))) nil)))
    (should (equal "DONE" (alist-get 'todo (car (alist-get 'reminders (harp-ui-current))))))
    (should (eq 'accepted (harp--on-delete (list :id (alist-get 'token (car (alist-get 'documents (harp-ui-current))))) nil)))
    (should-not (alist-get 'documents (harp-ui-current)))))

(ert-deftest harp-current-selected-destination-and-plot-range ()
  (harp-test-with-snapshot
    (let* ((metric (car (alist-get 'metrics (harp-ui-current))))
           (navigation (jetpacs-node->canonical-json (harp-ui-nav "metrics"))))
      ;; One selected item: its indicator and label wear the selected styles.
      (should (= 1 (cl-count-if (lambda (_) t) (split-string navigation "harp.nav-indicator-on") :start 1)))
      (should (string-match-p "harp.t.nav-selected\"\\]\\(?:,\"children\":\\[\\)?.*Metrics" navigation))
      (setf (alist-get 'range metric) '(0 . 10))
      (should (equal [0 10]
                     (plist-get
                      (cl-find "chart" (append (plist-get (harp-ui-metric metric) :children) nil)
                               :key (lambda (node) (plist-get node :t)) :test #'equal)
                      :y_range))))))

(ert-deftest harp-lifecycle-is-symmetric ()
  "Loading harp.el registers nothing, and harp-unregister undoes
everything harp-register did: the trusted launcher's two lifecycle
checks, run from the foundation's applet tooling."
  (require 'jetpacs-devtools)
  (load (expand-file-name "../jetpacs-applet-mcp/jetpacs-applet-tooling.el"
                          (file-name-directory
                           (locate-library "jetpacs-surfaces")))
        nil 'nomessage)
  (should-not (jetpacs-applet-tooling-load-recording-registrations
               (locate-library "harp.el" t)))
  (let ((report (jetpacs-applet-tooling-check-lifecycle-symmetry "harp")))
    (should (string-match-p "Status: SYMMETRIC" report))
    (should (string-match-p "Registered on entry: yes" report)))
  (should (assoc "harp" jetpacs-apps--registry)))

(provide 'harp-current-test)
;;; harp-current-test.el ends here

(ert-deftest harp-current-journal-cards-follow-harp ()
  "The Journals tab is upstream Harp's: a selector card, the count, and an
elevated card per visible entry with its date stamp and a paragraph whose
tags, metrics and attachments ride inline; private entries stay out, and
a tag filter re-presents the screen."
  (harp-test-with-snapshot
    (let* ((harp-state-route '(:view "journal"))
           (json (jetpacs-node->canonical-json (harp-ui-screen '(:view "journal")))))
      (should (string-match-p "Selected Journal" json))
      (should (string-match-p "\"jetpacs.inline_text\"" json))
      (should (string-match-p "harp.stamp" json))
      (should (string-match-p "Synthetic journal" json))
      (should-not (string-match-p "PRIVATE_SENTINEL" json))
      (jetpacs-shell--gate-ids (harp-ui-screen '(:view "journal")) nil))
    (should (equal (harp-ui-body-items "Walk #fitness #weight(63.8) and [[attachment:a.pdf]]")
                   '("Walk " (tag "fitness") " " (metric "weight" "63.8") " and " (attach "a.pdf"))))
    (should (equal (harp-ui-body-items "issue#3 stays") '("issue#3 stays")))
    (let ((pushed 0) (tag "sample") (harp-state-route '(:view "journal")))
      (cl-letf (((symbol-function 'jetpacs-shell-push) (lambda (&rest _) (cl-incf pushed)))
                ((symbol-function 'harp--later) (lambda (fn) (funcall fn)))
                ((symbol-function 'harp-profile-tags) (lambda (_profile) (list tag))))
        (should (eq 'accepted (harp--on-filter (list :tag tag) '(:surface "app:harp"))))
        (should (equal harp-state-tag tag))
        (should (= 1 pushed))
        (should (eq 'accepted (harp--on-filter nil '(:surface "app:harp"))))
        (should-not harp-state-tag)))))
