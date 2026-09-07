;;; harp-tests.el --- ERT suite for Harp -*- lexical-binding: t; -*-

;; Run against the vendored jetpacs core:
;;
;;   ./test/run-tests.sh
;;
;; or directly:
;;
;;   emacs -Q --batch -L jetpacs/emacs/core -L emacs/apps/harp -L test \
;;     -l harp-tests -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'harp)
(require 'jetpacs-lint)

(defvar harp-tests--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this test file.")

(defun harp-tests--fixture (name)
  "Absolute path of fixture NAME under test/fixtures/."
  (expand-file-name (concat "fixtures/" name) harp-tests--dir))

(defmacro harp-tests--with-profile (name &rest body)
  "Run BODY with fixture NAME open as the current profile.
Node support is stubbed on so gated nodes (chart, month_grid) build."
  (declare (indent 1))
  `(let ((harp-ui--current-file (harp-tests--fixture ,name))
         (harp-ui--cache nil)
         (harp-ui--derived nil)
         (harp-ui--tag-filter nil))
     (cl-letf (((symbol-function 'jetpacs-node-supported-p) (lambda (_) t)))
       ,@body)))

(defun harp-tests--date (str)
  "A Lisp time at noon on \"YYYY-MM-DD\" STR (noon avoids DST edges)."
  (seconds-to-time (harp--time-of (concat "<" str " 12:00>"))))

(defun harp-tests--all-views ()
  "Every built view node for the current profile."
  (list (harp-home--body)
        (harp-journal--view nil)
        (harp-documents--view nil)
        (harp-metrics--view nil)
        (harp-reminders--view nil)))

(defmacro harp-tests--with-temp-profile (fixture &rest body)
  "Copy FIXTURE into a temp dir, open it as the writable current profile, run
BODY, then clean up.  Never mutates the repo fixtures."
  (declare (indent 1))
  `(let* ((tmp (make-temp-file "harp-test" t))
          (file (expand-file-name ,fixture tmp))
          (harp-directory tmp)
          (harp-ui--current-file file)
          (harp-ui--cache nil)
          (harp-ui--derived nil)
          (harp-ui--tag-filter nil)
          (harp-form--open nil)
          ;; Keep the org-id UUID generation from scanning the real org tree.
          (org-id-track-globally nil)
          (org-agenda-files nil))
     (copy-file (harp-tests--fixture ,fixture) file)
     (unwind-protect (progn ,@body)
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string-prefix-p tmp (buffer-file-name b)))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (delete-directory tmp t))))

;; ─── app registration (Phase 0) ─────────────────────────────────────────────

(ert-deftest harp-loads-and-provides ()
  "The app entry loads and provides its feature."
  (should (featurep 'harp))
  (should (stringp harp-version))
  (should (equal harp-app-id "harp")))

(ert-deftest harp-registers-app ()
  "Loading Harp registers the app with a home view."
  (let ((entry (assoc "harp" jetpacs-apps--registry)))
    (should entry)
    (should (equal (plist-get (cdr entry) :label) "Harp"))
    (should (member "harp.home" (plist-get (cdr entry) :views)))))

(ert-deftest harp-home-body-builds ()
  "The Home view body builds a spec tree without signaling."
  (should (listp (harp-home--body))))

(ert-deftest harp-registers-section-tabs ()
  "The section views register under the app."
  (let ((views (plist-get (cdr (assoc "harp" jetpacs-apps--registry)) :views)))
    (dolist (v '("harp.home" "harp.journal" "harp.documents"
                 "harp.metrics" "harp.reminders"))
      (should (member v views)))))

(ert-deftest harp-section-views-build-with-profile ()
  "Every section view builder returns a scaffold when a profile is open."
  (harp-tests--with-profile "diego.org"
    (should (harp-ui-profile-open-p))
    (dolist (builder '(harp-journal--view harp-documents--view
                       harp-metrics--view harp-reminders--view))
      (should (funcall builder nil))))
  ;; And the no-profile path renders too.
  (let ((harp-ui--current-file nil) (harp-ui--cache nil))
    (should (harp-journal--view nil))))

(ert-deftest harp-views-lint-clean-and-serialize ()
  "Every view is wire-valid (lint clean) and JSON-serializable — the headless
equivalent of a live push (`jetpacs-render-to-json')."
  (harp-tests--with-profile "diego.org"
    (dolist (view (harp-tests--all-views))
      (should (null (jetpacs-lint-spec view)))
      (should (jetpacs-render-to-json view)))))

(ert-deftest harp-views-lint-clean-without-node-support ()
  "The fallback path (gated nodes absent) is also wire-valid."
  (let ((harp-ui--current-file (harp-tests--fixture "diego.org"))
        (harp-ui--cache nil) (harp-ui--tag-filter nil))
    (cl-letf (((symbol-function 'jetpacs-node-supported-p) (lambda (_) nil)))
      (dolist (view (harp-tests--all-views))
        (should (null (jetpacs-lint-spec view)))
        (should (jetpacs-render-to-json view))))))

(ert-deftest harp-metrics-view-has-chart ()
  "With chart support, the Metrics view emits a chart node."
  (harp-tests--with-profile "diego.org"
    (should (string-match-p "\"chart\"" (json-serialize (harp-metrics--view nil))))))

(ert-deftest harp-reminders-view-has-month-grid ()
  "With month_grid support, the Reminders view emits a calendar node."
  (harp-tests--with-profile "diego.org"
    (should (string-match-p "month_grid" (json-serialize (harp-reminders--view nil))))))

(ert-deftest harp-tag-filter-narrows-entries ()
  "Setting the tag filter hides entries lacking that tag."
  (harp-tests--with-profile "diego.org"
    ;; The 2026-01-03 Rehab entry carries #physio; the 2026-01-10 one does not.
    (let ((harp-ui--tag-filter "physio"))
      (should (string-match-p "physio"
                              (json-serialize (harp-journal--view nil)))))))

;; ─── inline metric/tag parsing (Phase 1) ────────────────────────────────────

(ert-deftest harp-inline-scan-metrics-and-tags ()
  "#id(value) becomes a metric reading; bare #tag becomes a tag."
  (let ((scan (harp-inline-scan "Knee mobility session #pain(4) #hr(102)")))
    (should (equal (plist-get scan :metrics) '(("pain" . 4) ("hr" . 102))))
    (should (null (plist-get scan :tags))))
  (let ((scan (harp-inline-scan "Carried meds and support brace #routine")))
    (should (null (plist-get scan :metrics)))
    (should (equal (plist-get scan :tags) '("routine"))))
  ;; Floats and a mix.
  (should (equal (harp-inline-metrics "am #weight(4.2) and #mood then #hr(60)")
                 '(("weight" . 4.2) ("hr" . 60))))
  (should (equal (harp-inline-tags "am #weight(4.2) and #mood then #hr(60)")
                 '("mood")))
  ;; Non-strings and empty are harmless.
  (should (null (harp-inline-metrics nil)))
  (should (null (harp-inline-tags ""))))

(ert-deftest harp-parse-range-cases ()
  "RANGE strings parse to (LO . HI); floats, nulls, and malformed inputs."
  (should (equal (harp-parse-range "35-220") '(35 . 220)))
  (should (equal (harp-parse-range "0-10") '(0 . 10)))
  (should (equal (harp-parse-range "2.8-5.8") '(2.8 . 5.8)))
  (should (equal (harp-parse-range "null-5") '(nil . 5)))
  (should (equal (harp-parse-range "") '(nil . nil)))
  (should (equal (harp-parse-range nil) '(nil . nil)))
  ;; Not two parts -> (nil . nil), matching harp-kmp.
  (should (equal (harp-parse-range "35") '(nil . nil))))

;; ─── profile reading (Phase 1) ──────────────────────────────────────────────

(ert-deftest harp-profile-read-human ()
  "The human fixture parses into the expected schema."
  (let ((p (harp-profile-read (harp-tests--fixture "diego.org"))))
    (should p)
    (should (equal (alist-get 'uuid p) "b8f2c0ad-6a2c-4f97-9ad0-0d9222220002"))
    (should (equal (alist-get 'name p) "Diego Alvarez"))
    (should (equal (alist-get 'type p) "human"))
    ;; Metrics: hr, sbp, pain.
    (let ((metrics (alist-get 'metrics p)))
      (should (= (length metrics) 3))
      (should (member "hr" (mapcar (lambda (m) (alist-get 'id m)) metrics)))
      (let ((hr (cl-find "hr" metrics :key (lambda (m) (alist-get 'id m)) :test #'equal)))
        (should (equal (alist-get 'unit hr) "bpm"))
        (should (equal (alist-get 'range hr) '(35 . 220)))
        (should (equal (alist-get 'healthy-range hr) '(55 . 95)))))
    ;; Journals: Rehab (2 entries, newest first) and Travel (1).
    (let* ((journals (alist-get 'journals p))
           (rehab (cl-find "Rehab" journals :key (lambda (j) (alist-get 'name j)) :test #'equal)))
      (should (= (length journals) 2))
      (should rehab)
      (let ((entries (alist-get 'entries rehab)))
        (should (= (length entries) 2))
        ;; Newest first: the 2026-01-10 entry precedes 2026-01-03.
        (should (> (alist-get 'time (nth 0 entries))
                   (alist-get 'time (nth 1 entries))))
        ;; The older entry carries #pain(4) #hr(102) and one attachment.
        (let ((old (nth 1 entries)))
          (should (equal (alist-get 'assets old) '("gait-session.mp4")))
          (should (equal (alist-get 'metric-values old)
                         `(((id . "pain") (value . 4)
                            (datetime . ,(alist-get 'datetime old))
                            (time . ,(alist-get 'time old)))
                           ((id . "hr") (value . 102)
                            (datetime . ,(alist-get 'datetime old))
                            (time . ,(alist-get 'time old)))))))))
    ;; Documents: 2, newest first, with source + files.
    (let ((docs (alist-get 'documents p)))
      (should (= (length docs) 2))
      (let ((mri (cl-find "MRI Report" docs :key (lambda (d) (alist-get 'name d)) :test #'equal)))
        (should (equal (alist-get 'source mri) "Centro Medico Norte"))
        (should (equal (alist-get 'files mri)
                       '("knee-mri-summary.pdf" "knee-mri-slice-1.png")))
        (should (member "imaging" (alist-get 'tags mri)))))
    ;; Reminders: 2 TODO headings with scheduling.
    (let ((rem (alist-get 'reminders p)))
      (should (= (length rem) 2))
      (let ((stretch (cl-find "Stretch Session" rem :key (lambda (r) (alist-get 'title r)) :test #'equal)))
        (should (equal (alist-get 'todo stretch) "TODO"))
        (should (alist-get 'scheduled stretch))))))

(ert-deftest harp-profile-read-cat-type ()
  "The cat fixture reports type \"cat\" and float healthy ranges."
  (let ((p (harp-profile-read (harp-tests--fixture "kenji-luna.org"))))
    (should (equal (alist-get 'type p) "cat"))
    (let ((weight (cl-find "weight" (alist-get 'metrics p)
                           :key (lambda (m) (alist-get 'id m)) :test #'equal)))
      (should (equal (alist-get 'healthy-range weight) '(2.8 . 5.8))))))

(ert-deftest harp-profile-metric-values-series ()
  "Metric readings are collected across entries oldest-first."
  (let* ((p (harp-profile-read (harp-tests--fixture "diego.org")))
         (pain (harp-profile-metric-values p "pain")))
    ;; #pain(4) at 2026-01-03 and #pain(2) at 2026-01-10.
    (should (= (length pain) 2))
    (should (< (alist-get 'time (nth 0 pain)) (alist-get 'time (nth 1 pain))))
    (should (equal (mapcar (lambda (mv) (alist-get 'value mv)) pain) '(4 2)))))

(ert-deftest harp-profiles-discovery ()
  "Discovery finds the fixtures when `harp-directory' points at them."
  (let ((harp-directory (expand-file-name "fixtures/" harp-tests--dir)))
    (let ((infos (harp-profiles)))
      (should (= (length infos) 4))
      (should (member "Diego Alvarez" (mapcar (lambda (i) (alist-get 'name i)) infos)))
      (should (member "human" (mapcar (lambda (i) (alist-get 'type i)) infos)))
      (should (member "cat" (mapcar (lambda (i) (alist-get 'type i)) infos)))
      (should (member "dog" (mapcar (lambda (i) (alist-get 'type i)) infos))))))

;; ─── capture / edit (Phase 3) ───────────────────────────────────────────────

(ert-deftest harp-form-add-entry-round-trip ()
  "Opening the entry form, filling it, and saving appends a parsed entry."
  (harp-tests--with-temp-profile "diego.org"
    (harp-form-open "entry")
    (jetpacs-ui-state-put (harp-form--fid "body") "Checkup done #pain(3) #hr(90)")
    (jetpacs-ui-state-put (harp-form--fid "tags") "checkup routine")
    (should (harp-form--save))
    (let* ((p (harp-profile-read file))
           (rehab (cl-find "Rehab" (alist-get 'journals p)
                           :key (lambda (j) (alist-get 'name j)) :test #'equal))
           (newest (car (alist-get 'entries rehab))))
      ;; New entry is today's, so it sorts newest-first.
      (should (string-match-p "Checkup done" (alist-get 'body newest)))
      (should (member "checkup" (alist-get 'heading-tags newest)))
      (should (equal (alist-get 'metric-values newest)
                     `(((id . "pain") (value . 3)
                        (datetime . ,(alist-get 'datetime newest))
                        (time . ,(alist-get 'time newest)))
                       ((id . "hr") (value . 90)
                        (datetime . ,(alist-get 'datetime newest))
                        (time . ,(alist-get 'time newest)))))))))

(ert-deftest harp-form-edit-entry-round-trip ()
  "Opening an existing entry for edit and saving updates it in place."
  (harp-tests--with-temp-profile "diego.org"
    (let* ((p (harp-profile-read file))
           (rehab (cl-find "Rehab" (alist-get 'journals p)
                           :key (lambda (j) (alist-get 'name j)) :test #'equal))
           (entry (car (alist-get 'entries rehab)))
           (id (alist-get 'id entry)))
      (harp-form-open "entry" entry)
      (jetpacs-ui-state-put (harp-form--fid "body") "Edited body #pain(0)")
      (should (harp-form--save))
      (let* ((p2 (harp-profile-read file))
             (updated (harp-form--find-entry id)))
        ;; still exactly one entry with that id, now edited
        (should updated)
        (should (string-match-p "Edited body" (alist-get 'body updated)))))))

(ert-deftest harp-form-add-metric-round-trip ()
  "The metric form appends a metric definition."
  (harp-tests--with-temp-profile "diego.org"
    (harp-form-open "metric")
    (jetpacs-ui-state-put (harp-form--fid "name") "Weight")
    (jetpacs-ui-state-put (harp-form--fid "tagid") "wt")
    (jetpacs-ui-state-put (harp-form--fid "unit") "kg")
    (jetpacs-ui-state-put (harp-form--fid "range") "40-150")
    (jetpacs-ui-state-put (harp-form--fid "healthy") "60-90")
    (should (harp-form--save))
    (let ((m (cl-find "wt" (alist-get 'metrics (harp-profile-read file))
                      :key (lambda (x) (alist-get 'id x)) :test #'equal)))
      (should m)
      (should (equal (alist-get 'unit m) "kg"))
      (should (equal (alist-get 'healthy-range m) '(60 . 90))))))

(ert-deftest harp-form-new-profile-round-trip ()
  "The profile form writes a new, parseable profile file."
  (harp-tests--with-temp-profile "diego.org"
    (harp-form-open "profile")
    (jetpacs-ui-state-put (harp-form--fid "pname") "Luna Cat")
    (jetpacs-ui-state-put (harp-form--fid "ptype") (vector "cat"))
    (should (harp-form--save))
    ;; A second profile file now exists and parses as a cat.
    (let ((infos (harp-profiles)))
      (should (cl-find "Luna Cat" infos
                       :key (lambda (i) (alist-get 'name i)) :test #'equal))
      (should (cl-find "cat" infos
                       :key (lambda (i) (alist-get 'type i)) :test #'equal)))))

(ert-deftest harp-form-validation-blocks-empty ()
  "Saving an empty required field is refused (returns nil, writes nothing)."
  (harp-tests--with-temp-profile "diego.org"
    (harp-form-open "metric")             ; name/tagid/unit all blank
    (should-not (harp-form--save))
    (should (= (length (alist-get 'metrics (harp-profile-read file))) 3))))

(ert-deftest harp-form-views-lint-and-serialize ()
  "Every form kind builds a wire-valid, serializable screen."
  (harp-tests--with-temp-profile "diego.org"
    (dolist (kind '("entry" "document" "metric" "profile"))
      (harp-form-open kind)
      (let ((view (harp-form--view nil)))
        (should (null (jetpacs-lint-spec view)))
        (should (jetpacs-render-to-json view))))))

(ert-deftest harp-form-fab-per-tab ()
  "The FAB appears on capture tabs and is absent elsewhere."
  (harp-tests--with-temp-profile "diego.org"
    (should (harp-form--fab "harp.home"))
    (should (harp-form--fab "harp.journal"))
    (should (harp-form--fab "harp.documents"))
    (should (harp-form--fab "harp.metrics"))
    (should (harp-form--fab "harp.meds"))
    (should-not (harp-form--fab "harp.reminders"))))

(ert-deftest harp-form-open-and-close-navigate ()
  "Opening the form forces the companion onto the overlay; closing returns
to a tab.  Regression: a plain push only updates content, so the overlay
must be reached with :switch-to."
  (harp-tests--with-temp-profile "diego.org"
    (let (pushes)
      (cl-letf (((symbol-function 'jetpacs-shell-push)
                 (lambda (&rest args) (push args pushes))))
        (harp-form-open "entry")
        (should (equal (car pushes) '(nil :switch-to "harp.form")))
        (harp-form--close)
        ;; back to a real tab, not the overlay
        (let ((to (plist-get (cdr (car pushes)) :switch-to)))
          (should (stringp to))
          (should-not (equal to "harp.form")))))))

;; ─── export & reminders (Phase 4) ───────────────────────────────────────────

(ert-deftest harp-export-text-round-trips ()
  "Org export of a profile re-parses to the same journals and documents."
  (harp-tests--with-temp-profile "diego.org"
    (let* ((profile (harp-profile-read file))
           (text (harp-export-text profile))
           (out (expand-file-name "export-out.org" (file-name-directory file))))
      (with-temp-file out (insert text))
      (let ((re (harp-profile-read out)))
        (should (equal (alist-get 'uuid re) (alist-get 'uuid profile)))
        (should (= (length (alist-get 'documents re)) 2))
        (let ((rehab (cl-find "Rehab" (alist-get 'journals re)
                              :key (lambda (j) (alist-get 'name j)) :test #'equal)))
          (should (= (length (alist-get 'entries rehab)) 2))
          ;; metric readings survive the round trip
          (should (cl-find "pain" (alist-get 'metric-values (car (last (alist-get 'entries rehab))))
                           :key (lambda (mv) (alist-get 'id mv)) :test #'equal)))))))

(ert-deftest harp-export-html-builds-and-escapes ()
  "HTML export carries the title, content, attachments, and escapes markup."
  (harp-tests--with-temp-profile "diego.org"
    (let* ((profile (harp-profile-read file))
           (html (harp-export-html profile)))
      (should (string-match-p "<title>Diego Alvarez" html))
      (should (string-match-p "Knee mobility session" html))
      (should (string-match-p "gait-session\\.mp4" html))
      ;; ampersand escaping proves the escaper runs
      (should (string-match-p "&amp;\\|&lt;\\|&gt;"
                              (harp-export--escape "a & b < c > d")))
      (let ((f (harp-export-html-to-file profile)))
        (should (file-exists-p f))
        (should (string-suffix-p ".html" f))))))

(ert-deftest harp-export-dialog-lints ()
  "The export chooser spec is wire-valid."
  (harp-tests--with-temp-profile "diego.org"
    (should (null (jetpacs-lint-spec (harp-export--dialog-body))))
    (should (jetpacs-render-to-json (harp-export--dialog-body)))))

(ert-deftest harp-reminders-next-time ()
  "Future stamps resolve; past ones need a repeater to resolve."
  (let ((future (format-time-string "<%Y-%m-%d %H:%M>" (time-add nil 86400))))
    (should (> (harp-reminders--next-time future) (float-time))))
  (should-not (harp-reminders--next-time "<2000-01-01>"))
  ;; A past weekly/monthly repeater still yields a future occurrence.
  (should (> (harp-reminders--next-time "<2000-01-03 +1w>") (float-time)))
  (should (> (harp-reminders--next-time "<2000-01-03 +1m>") (float-time))))

(ert-deftest harp-reminders-specs-are-future-and-shaped ()
  "Every armed reminder spec is future and carries id/at_ms/title."
  (harp-tests--with-temp-profile "diego.org"
    (let ((specs (harp-reminders--specs (harp-profile-read file)))
          (now-ms (* 1000 (float-time))))
      ;; The fixture's "Stretch Session" is a `+2d' repeater, so a future
      ;; occurrence always exists — without this the `dolist' below would pass
      ;; vacuously once the one-shot deadlines age into the past.
      (should specs)
      (dolist (s specs)
        (should (stringp (alist-get 'id s)))
        (should (string-prefix-p "harp/" (alist-get 'id s)))
        (should (stringp (alist-get 'title s)))
        (should (> (alist-get 'at_ms s) now-ms))))))

(ert-deftest harp-reminders-sync-arms-when-connected ()
  "Sync arms the owner-scoped set while connected, and is a no-op offline."
  (harp-tests--with-temp-profile "diego.org"
    (let (armed)
      (cl-letf (((symbol-function 'jetpacs-reminders-owner-set)
                 (lambda (specs owner) (setq armed (cons owner specs)) t)))
        ;; offline: nothing armed
        (let ((jetpacs--session nil) (harp-reminders--last 'unset))
          (harp-reminders-sync)
          (should-not armed))
        ;; connected: the harp-owned set is armed
        (let ((jetpacs--session '((connected . t))) (harp-reminders--last 'unset))
          (harp-reminders-sync)
          (should (equal (car armed) "harp"))
          (should (> (length (cdr armed)) 0)))))))

;; ─── settings & pack (Phase 5) ──────────────────────────────────────────────

(ert-deftest harp-hide-private-filters-items ()
  "`harp-hide-private' hides private items; the tag filter still applies."
  (let ((private '((private . t) (tags . ("x"))))
        (public  '((private . nil) (tags . ("x"))))
        (harp-ui--tag-filter nil))
    (let ((harp-hide-private nil))
      (should (harp-ui-item-visible-p private)))
    (let ((harp-hide-private t))
      (should-not (harp-ui-item-visible-p private))
      (should (harp-ui-item-visible-p public)))))

(ert-deftest harp-settings-section-registered ()
  "The Harp settings section exposes its allowlisted defcustoms."
  (require 'harp-settings)
  (should (assoc "Harp" jetpacs-settings-registry)))

(ert-deftest harp-pack-manifest-well-formed ()
  "The pack manifest carries the app's actions and serializes."
  (require 'harp-pack)
  (let ((m (harp-pack-manifest)))
    (should (equal (alist-get 'pack_id m) "harp"))
    (should (> (length (alist-get 'actions m)) 0))
    ;; Every action is a harp.* name (owner-filtered catalog).
    (should (cl-every (lambda (a) (string-prefix-p "harp." (alist-get 'action a)))
                      (alist-get 'actions m)))
    (should (jetpacs-render-to-json m))))

(ert-deftest harp-pack-json-committed-is-current ()
  "The checked-in harp-pack.json matches the live registrations."
  (require 'harp-pack)
  (let ((committed (expand-file-name "../harp-pack.json" harp-tests--dir)))
    (when (file-readable-p committed)
      (should (equal (harp-pack-json)
                     (with-temp-buffer
                       (insert-file-contents committed)
                       (buffer-string)))))))

;; ─── demo seeding ───────────────────────────────────────────────────────────

(ert-deftest harp-demo-setup-writes-profiles ()
  "The seeder writes three parseable profiles (human/cat/dog) and opens one."
  (require 'harp-demo)
  (let* ((tmp (make-temp-file "harp-demo" t))
         (harp-directory tmp)
         (harp-ui--current-file nil)
         (harp-ui--cache nil))
    (unwind-protect
        (let ((opened (harp-demo-setup)))
          (should opened)
          (should (file-exists-p opened))
          (should (harp-profile-read opened))
          (let ((types (mapcar (lambda (i) (alist-get 'type i)) (harp-profiles))))
            (should (= (length types) 3))
            (should (member "human" types))
            (should (member "cat" types))
            (should (member "dog" types)))
          ;; idempotent: a second run without FORCE writes nothing new
          (harp-demo-setup)
          (should (= (length (harp-profiles)) 3)))
      (dolist (b (buffer-list))
        (when (and (buffer-file-name b) (string-prefix-p tmp (buffer-file-name b)))
          (with-current-buffer b (set-buffer-modified-p nil))
          (kill-buffer b)))
      (delete-directory tmp t))))

;; ─── medications: read layer (Phase M1) ─────────────────────────────────────

(ert-deftest harp-med-parses-section ()
  "The Medications section parses into medication records with schedules,
dose logs, stock, and active flags."
  (require 'harp-med)
  (let* ((p (harp-profile-read (harp-tests--fixture "meds.org")))
         (meds (harp-med-medications p)))
    (should (= (length meds) 5))
    (let ((ibu (harp-med-by-id "11111111-0000-4000-8000-000000000001" p)))
      (should (equal (alist-get 'name ibu) "Ibuprofen"))
      (should (equal (alist-get 'unit ibu) "tab"))
      (should (= (alist-get 'stock ibu) 30))
      (should (member "pain" (alist-get 'tags ibu)))
      (should (equal (alist-get 'notes ibu) "Take with food."))
      (should (harp-med-active-p ibu))
      ;; one schedule: two daily times on weekdays
      (let ((s (car (alist-get 'schedules ibu))))
        (should (equal (alist-get 'times s) '("08:00" "20:00")))
        (should (equal (alist-get 'days s) '("Mon" "Tue" "Wed" "Thu" "Fri"))))
      ;; two dose events; the taken one carries an amount
      (let ((doses (harp-med-doses ibu)))
        (should (= (length doses) 2))
        (should (equal (alist-get 'state (nth 0 doses)) "taken"))
        (should (equal (alist-get 'amount (nth 0 doses)) "1 tab"))
        (should (equal (alist-get 'state (nth 1 doses)) "skipped"))))))

(ert-deftest harp-med-cyclic-and-interval-and-taper-parse ()
  "Cyclic, interval, and tapering schedule properties parse."
  (require 'harp-med)
  (let* ((p (harp-profile-read (harp-tests--fixture "meds.org")))
         (bc (car (alist-get 'schedules (harp-med-by-id "22222222-0000-4000-8000-000000000002" p))))
         (amox (car (alist-get 'schedules (harp-med-by-id "33333333-0000-4000-8000-000000000003" p))))
         (taper (harp-med-by-id "44444444-0000-4000-8000-000000000004" p)))
    (should (= (alist-get 'active-days bc) 21))
    (should (= (alist-get 'pause-days bc) 7))
    (should (alist-get 'cycle-start bc))
    (should (equal (alist-get 'interval amox) "8h"))
    (should (equal (alist-get 'window amox) "08:00-22:00"))
    ;; tapering = two adjacent schedules with different amounts
    (let ((scheds (alist-get 'schedules taper)))
      (should (= (length scheds) 2))
      (should (equal (mapcar (lambda (s) (alist-get 'amount s)) scheds) '("4" "2"))))))

(ert-deftest harp-med-stock-and-expiration-predicates ()
  "Low-stock, out-of-stock, expired, and inactive predicates."
  (require 'harp-med)
  (let* ((p (harp-profile-read (harp-tests--fixture "meds.org")))
         (vit (harp-med-by-id "55555555-0000-4000-8000-000000000005" p))
         (ibu (harp-med-by-id "11111111-0000-4000-8000-000000000001" p)))
    ;; Old Vitamin: inactive, expired, low stock (3 <= 10)
    (should-not (harp-med-active-p vit))
    (should (harp-med-expired-p vit))
    (should (harp-med-low-stock-p vit))
    (should (member "Expired" (harp-med-warnings vit)))
    (should (member "Low stock" (harp-med-warnings vit)))
    ;; Ibuprofen: fine on all counts
    (should-not (harp-med-expired-p ibu))
    (should-not (harp-med-low-stock-p ibu))
    (should (null (harp-med-warnings ibu)))))

;; ─── medications: scheduling engine (Phase M2) ──────────────────────────────

(defvar harp-tests--med-profile nil)
(defun harp-tests--med (id)
  "Medication ID from the meds fixture (cached per test run)."
  (unless harp-tests--med-profile
    (setq harp-tests--med-profile (harp-profile-read (harp-tests--fixture "meds.org"))))
  (harp-med-by-id id harp-tests--med-profile))

(ert-deftest harp-med-schedule-daily-weekdays ()
  "Ibuprofen fires twice on weekdays and never on weekends."
  (require 'harp-med-schedule)
  (let ((ibu (harp-tests--med "11111111-0000-4000-8000-000000000001")))
    (dotimes (i 7)
      (let* ((date (time-add (harp-tests--date "2026-07-13") (* i 86400)))
             (wd (harp-med-schedule--weekday date))
             (doses (harp-med-schedule-doses-on ibu date)))
        (if (member wd '("Sat" "Sun"))
            (should (null doses))
          (should (equal (mapcar (lambda (d) (alist-get 'time d)) doses)
                         '("08:00" "20:00"))))))))

(ert-deftest harp-med-schedule-cyclic-birth-control ()
  "21-active / 7-pause cycle: active days 0..20, paused 21..27, active again 28."
  (require 'harp-med-schedule)
  (let ((bc (harp-tests--med "22222222-0000-4000-8000-000000000002")))
    (cl-flet ((n (d) (length (harp-med-schedule-doses-on bc (harp-tests--date d)))))
      (should (= (n "2026-01-01") 1))    ; pos 0  active
      (should (= (n "2026-01-21") 1))    ; pos 20 active
      (should (= (n "2026-01-22") 0))    ; pos 21 pause
      (should (= (n "2026-01-28") 0))    ; pos 27 pause
      (should (= (n "2026-01-29") 1)))))  ; pos 28 -> 0 active

(ert-deftest harp-med-schedule-interval-windowed ()
  "8h interval within 08:00-22:00 fires at 08:00 and 16:00."
  (require 'harp-med-schedule)
  (let* ((amox (harp-tests--med "33333333-0000-4000-8000-000000000003"))
         (doses (harp-med-schedule-doses-on amox (harp-tests--date "2026-07-15"))))
    (should (equal (mapcar (lambda (d) (alist-get 'time d)) doses) '("08:00" "16:00")))
    (should (equal (alist-get 'amount (car doses)) "1"))))

(ert-deftest harp-med-schedule-tapering-periods ()
  "Adjacent periods hand off: 40mg in week 1, 20mg in week 2, nothing after."
  (require 'harp-med-schedule)
  (let ((taper (harp-tests--med "44444444-0000-4000-8000-000000000004")))
    (let ((w1 (harp-med-schedule-doses-on taper (harp-tests--date "2026-07-03"))))
      (should (= (length w1) 1))
      (should (equal (alist-get 'amount (car w1)) "4")))
    (let ((w2 (harp-med-schedule-doses-on taper (harp-tests--date "2026-07-10"))))
      (should (= (length w2) 1))
      (should (equal (alist-get 'amount (car w2)) "2")))
    (should (null (harp-med-schedule-doses-on taper (harp-tests--date "2026-07-20"))))))

(ert-deftest harp-med-schedule-inactive-med ()
  "An inactive medication yields no doses."
  (require 'harp-med-schedule)
  (let ((vit (harp-tests--med "55555555-0000-4000-8000-000000000005")))
    (should (null (harp-med-schedule-doses-on vit (harp-tests--date "2026-07-15"))))))

(ert-deftest harp-med-schedule-pending-count-based ()
  "Pending = scheduled minus the first K by time (K = doses logged that day)."
  (require 'harp-med-schedule)
  (let ((date (harp-tests--date "2026-07-13")))
    (cl-flet ((med (logged)
                `((active . t) (unit . "mg")
                  (schedules . (((amount . "1") (times "08:00" "20:00"))))
                  (doses . ,(mapcar (lambda (_)
                                      '((state . "taken") (stamp . "2026-07-13 08:05")))
                                    (number-sequence 1 logged))))))
      (should (= (length (harp-med-schedule-pending (med 0) date)) 2))
      (let ((p1 (harp-med-schedule-pending (med 1) date)))
        (should (= (length p1) 1))
        (should (equal (alist-get 'time (car p1)) "20:00")))
      (should (null (harp-med-schedule-pending (med 2) date))))))

(ert-deftest harp-med-schedule-next-occurrence ()
  "The next dose is strictly in the future."
  (require 'harp-med-schedule)
  (let* ((amox (harp-tests--med "33333333-0000-4000-8000-000000000003"))
         (next (harp-med-schedule-next amox)))
    (should next)
    (should (> next (float-time)))))

;; ─── medications: dose logging + stock (Phase M3) ───────────────────────────

(ert-deftest harp-med-log-taken-decrements-stock ()
  "Logging Taken appends a dose and reduces stock by the amount."
  (require 'harp-med-log)
  (harp-tests--with-temp-profile "meds.org"
    (let* ((id "11111111-0000-4000-8000-000000000001")
           (ibu (harp-med-by-id id (harp-profile-read file))))
      (harp-med-log-write (alist-get 'ref ibu) "taken" "3" "tab" 3)
      (let ((ibu2 (harp-med-by-id id (harp-profile-read file))))
        (should (= (alist-get 'stock ibu2) 27))       ; 30 - 3
        (should (= (length (harp-med-doses ibu2)) 3)) ; was 2
        (should (equal (alist-get 'state (car (harp-med-doses ibu2))) "taken"))
        (should (equal (alist-get 'amount (car (harp-med-doses ibu2))) "3 tab"))))))

(ert-deftest harp-med-log-skipped-keeps-stock ()
  "Logging Skipped appends a dose but leaves stock untouched."
  (require 'harp-med-log)
  (harp-tests--with-temp-profile "meds.org"
    (let* ((id "11111111-0000-4000-8000-000000000001")
           (ibu (harp-med-by-id id (harp-profile-read file))))
      (harp-med-log-write (alist-get 'ref ibu) "skipped" nil nil nil)
      (let ((ibu2 (harp-med-by-id id (harp-profile-read file))))
        (should (= (alist-get 'stock ibu2) 30))
        (should (equal (alist-get 'state (car (harp-med-doses ibu2))) "skipped"))))))

(ert-deftest harp-med-log-creates-logbook-when-absent ()
  "A med with no LOGBOOK gets one on first dose."
  (require 'harp-med-log)
  (harp-tests--with-temp-profile "meds.org"
    (let* ((id "22222222-0000-4000-8000-000000000002")
           (lev (harp-med-by-id id (harp-profile-read file))))
      (should (= (length (harp-med-doses lev)) 0))
      (harp-med-log-write (alist-get 'ref lev) "taken" "1" "tab" 1)
      (let ((lev2 (harp-med-by-id id (harp-profile-read file))))
        (should (= (length (harp-med-doses lev2)) 1))
        (should (= (alist-get 'stock lev2) 83))))))

(ert-deftest harp-med-log-action-path ()
  "The taken action resolves a med by id and logs through the write layer."
  (require 'harp-med-log)
  (harp-tests--with-temp-profile "meds.org"
    (harp-med-log--dose '((id . "33333333-0000-4000-8000-000000000003")
                          (amount . "2"))
                        nil "taken")
    (let ((amox (harp-med-by-id "33333333-0000-4000-8000-000000000003"
                                (harp-profile-read file))))
      (should (= (alist-get 'stock amox) 19))          ; 21 - 2
      (should (equal (alist-get 'state (car (harp-med-doses amox))) "taken")))))

;; ─── medications: form, delete, reminders (Phase M4) ────────────────────────

(ert-deftest harp-med-form-add-round-trip ()
  "Filling and saving the medication form appends a parseable medication."
  (require 'harp-med-form)
  (harp-tests--with-temp-profile "meds.org"
    (harp-med-form-open)
    (jetpacs-ui-state-put (harp-med-form--fid "name") "Metformin")
    (jetpacs-ui-state-put (harp-med-form--fid "unit") "mg")
    (jetpacs-ui-state-put (harp-med-form--fid "stock") "60")
    (jetpacs-ui-state-put (harp-med-form--fid "low-stock") "10")
    (jetpacs-ui-state-put (harp-med-form--fid "tags") "diabetes")
    (jetpacs-ui-state-put (harp-med-form--fid "amount") "1")
    (jetpacs-ui-state-put (harp-med-form--fid "times") "08:00, 20:00")
    (should (harp-med-form--save))
    (let ((m (cl-find "Metformin" (harp-med-medications (harp-profile-read file))
                      :key (lambda (x) (alist-get 'name x)) :test #'equal)))
      (should m)
      (should (= (alist-get 'stock m) 60))
      (should (member "diabetes" (alist-get 'tags m)))
      (should (harp-med-active-p m))
      (should (equal (alist-get 'times (car (alist-get 'schedules m)))
                     '("08:00" "20:00"))))))

(ert-deftest harp-med-form-edit-preserves-logbook ()
  "Editing a medication updates its fields but keeps the dose history."
  (require 'harp-med-form)
  (harp-tests--with-temp-profile "meds.org"
    (let* ((id "11111111-0000-4000-8000-000000000001")
           (ibu (harp-med-by-id id (harp-profile-read file))))
      (should (= (length (harp-med-doses ibu)) 2))
      (harp-med-form-open ibu)
      (jetpacs-ui-state-put (harp-med-form--fid "stock") "99")
      (should (harp-med-form--save))
      (let ((ibu2 (harp-med-by-id id (harp-profile-read file))))
        (should (= (alist-get 'stock ibu2) 99))
        (should (= (length (harp-med-doses ibu2)) 2))))))

(ert-deftest harp-med-delete-removes-med ()
  "Deleting a medication removes its subtree."
  (require 'harp-med-form)
  (harp-tests--with-temp-profile "meds.org"
    (let ((vit (harp-med-by-id "55555555-0000-4000-8000-000000000005"
                               (harp-profile-read file))))
      (harp-write-delete-medication (alist-get 'ref vit))
      (should (= (length (harp-med-medications (harp-profile-read file))) 4))
      (should-not (harp-med-by-id "55555555-0000-4000-8000-000000000005"
                                  (harp-profile-read file))))))

(ert-deftest harp-med-reminder-specs-future ()
  "Active medications contribute future dose alarm specs."
  (require 'harp-reminders)
  (let ((p (harp-profile-read (harp-tests--fixture "meds.org")))
        (now-ms (* 1000 (float-time))))
    (let ((specs (harp-reminders--med-specs p)))
      (should specs)
      (dolist (s specs)
        (should (string-prefix-p "harp/med/" (alist-get 'id s)))
        (should (> (alist-get 'at_ms s) now-ms))))))

(ert-deftest harp-med-form-view-lints ()
  "The medication form screen is wire-valid and serializable (add + edit)."
  (require 'harp-med-form)
  (let ((harp-ui--current-file (harp-tests--fixture "meds.org"))
        (harp-ui--cache nil))
    (cl-letf (((symbol-function 'jetpacs-node-supported-p) (lambda (_) t)))
      (let ((harp-med-form--open '(:ref nil)))
        (should (null (jetpacs-lint-spec (harp-med-form--view nil))))
        (should (jetpacs-render-to-json (harp-med-form--view nil))))
      (let ((harp-med-form--open '(:ref ((id . "x")))))
        (should (null (jetpacs-lint-spec (harp-med-form--view nil))))))))

;; ─── medications: interactive dose notifications ────────────────────────────

(defun harp-tests--due-med-profile ()
  "A profile with one active med due at 00:00 today and nothing logged.
Scheduled at midnight so its `at' is always <= now — a 00:01 dose is not
yet past during the first minute of the day and flakes the due-now check."
  '((medications . (((id . "m1") (name . "TestMed") (unit . "tab") (active . t)
                     (schedules . (((amount . "1") (times "00:00"))))
                     (doses . nil))))))

(ert-deftest harp-med-notify-spec-has-action-buttons ()
  "The dose notification carries Taken / Skip / Snooze action buttons."
  (require 'harp-med-notify)
  (let* ((med '((id . "m1") (name . "TestMed") (unit . "tab")))
         (entry '((time . "08:00") (amount . "1") (unit . "tab") (at . 1)))
         (spec (harp-med-notify--spec med entry)))
    (should (jetpacs-render-to-json spec))
    (let ((json (json-serialize spec)))
      (should (string-match-p "Taken" json))
      (should (string-match-p "Skip" json))
      (should (string-match-p "Snooze" json))
      (should (string-match-p "harp.med.taken" json))
      (should (string-match-p "harp.med.snooze" json)))))

(ert-deftest harp-med-notify-due-now-filtering ()
  "Due-now = past scheduled time, pending, not snoozed."
  (require 'harp-med-notify)
  (clrhash harp-med-notify--snoozed)
  (let ((profile (harp-tests--due-med-profile)))
    (should (= 1 (length (harp-med-notify--due-now profile))))
    ;; snoozing that dose removes it from due-now
    (puthash (format "m1\0%s" "00:00") (+ (float-time) 3600) harp-med-notify--snoozed)
    (should (= 0 (length (harp-med-notify--due-now profile))))
    (clrhash harp-med-notify--snoozed))
  ;; a logged dose is no longer pending, so not due
  (let ((profile '((medications . (((id . "m1") (name . "T") (unit . "tab") (active . t)
                                    (schedules . (((amount . "1") (times "00:00"))))
                                    (doses . (((state . "taken")
                                               (stamp . "2000-01-01 00:00"))))))))))
    ;; the logged dose is on a past date, so today's dose is still pending —
    ;; count-based pending is per-day, so this remains due:
    (should (= 1 (length (harp-med-notify--due-now profile))))))

(ert-deftest harp-med-notify-snooze-action-registered ()
  "The snooze action is registered."
  (require 'harp-med-notify)
  (should (gethash "harp.med.snooze" jetpacs-action-handlers)))

(ert-deftest harp-med-notify-sync-reconciles ()
  "Sync pushes a due dose once (no re-alert), then removes it when resolved."
  (require 'harp-med-notify)
  (let ((pushed '()) (removed '())
        (harp-med-notify--pushed nil)
        (harp-med-notify-enabled t)
        (jetpacs--session '((connected . t)))
        (entry '((med . ((id . "m1") (name . "T") (unit . "tab")))
                 (time . "08:00") (amount . "1") (unit . "tab") (at . 1))))
    (cl-letf (((symbol-function 'harp-ui-current) (lambda () t))
              ((symbol-function 'jetpacs-surface-push) (lambda (sid _spec) (push sid pushed)))
              ((symbol-function 'jetpacs-surface-remove) (lambda (sid) (push sid removed))))
      ;; due -> one push
      (cl-letf (((symbol-function 'harp-med-notify--due-now) (lambda (&optional _) (list entry))))
        (harp-med-notify-sync)
        (should (equal pushed '("notification:harp.dose.m1")))
        ;; same content again -> no re-push (no re-alert)
        (harp-med-notify-sync)
        (should (equal pushed '("notification:harp.dose.m1"))))
      ;; resolved -> removed
      (cl-letf (((symbol-function 'harp-med-notify--due-now) (lambda (&optional _) nil)))
        (harp-med-notify-sync)
        (should (member "notification:harp.dose.m1" removed))))))

;; ─── medications: views (Phase M3) ──────────────────────────────────────────

(ert-deftest harp-med-view-lints-and-serializes ()
  "The Meds tab and detail overlay are wire-valid and serializable."
  (require 'harp-med-view)
  (let ((harp-ui--current-file (harp-tests--fixture "meds.org"))
        (harp-ui--cache nil))
    (cl-letf (((symbol-function 'jetpacs-node-supported-p) (lambda (_) t)))
      (let ((meds (harp-med-view--view nil)))
        (should (null (jetpacs-lint-spec meds)))
        (should (jetpacs-render-to-json meds)))
      ;; detail for a real medication
      (let ((harp-med-view--detail-id "11111111-0000-4000-8000-000000000001"))
        (let ((detail (harp-med-view--detail nil)))
          (should (null (jetpacs-lint-spec detail)))
          (should (jetpacs-render-to-json detail)))))))

(ert-deftest harp-med-view-registers-tab ()
  "The Meds tab and detail overlay register under the app."
  (require 'harp-med-view)
  (let ((views (plist-get (cdr (assoc "harp" jetpacs-apps--registry)) :views)))
    (should (member "harp.meds" views))
    (should (member "harp.med.detail" views))))

(ert-deftest harp-demo-setup-refreshes-with-force ()
  "A re-run with force refreshes stale sample files (e.g. seeded before a
new field like medications existed); without force it leaves them alone."
  (require 'harp-demo)
  (let* ((tmp (make-temp-file "harp-demo-r" t))
         (harp-directory tmp)
         (harp-ui--current-file nil) (harp-ui--cache nil)
         (file (expand-file-name "diego-recovery.org" tmp)))
    (unwind-protect
        (progn
          ;; a stale sample with no medications, and a live buffer visiting it
          (with-temp-file file
            (insert ":PROPERTIES:\n:ID: old\n:HARP_TYPE: human\n:END:\n"
                    "#+TITLE: Old\n\n* Journals\n** Main\n* Medications\n"))
          (find-file-noselect file)
          (should (harp-demo--exists-p))
          ;; non-force leaves it untouched
          (harp-demo-setup)
          (should (= 0 (length (harp-med-medications (harp-profile-read file)))))
          ;; force refreshes to the bundled sample (which has 2 meds), and the
          ;; stale visiting buffer must not mask the new content
          (harp-demo-setup t)
          (should (= 2 (length (harp-med-medications (harp-profile-read file))))))
      (dolist (b (buffer-list))
        (when (and (buffer-file-name b) (string-prefix-p tmp (buffer-file-name b)))
          (with-current-buffer b (set-buffer-modified-p nil))
          (kill-buffer b)))
      (delete-directory tmp t))))

;; ─── hardening pass (re-review fixes) ───────────────────────────────────────

(ert-deftest harp-med-edit-preserves-extra-schedules ()
  "Editing a multi-schedule med keeps schedules 2..n verbatim.
Regression: the form rebuilds only the first schedule; the taper's second
subtree used to be silently destroyed."
  (harp-tests--with-temp-profile "meds.org"
    (let* ((id "44444444-0000-4000-8000-000000000004")
           (taper (harp-med-by-id id (harp-profile-read file))))
      (should (= 2 (length (alist-get 'schedules taper))))
      (harp-med-form-open taper)
      (jetpacs-ui-state-put (harp-med-form--fid "stock") "41")
      (should (harp-med-form--save))
      (let* ((after (harp-med-by-id id (harp-profile-read file)))
             (scheds (alist-get 'schedules after)))
        (should (= 41 (alist-get 'stock after)))
        (should (= 2 (length scheds)))
        (should (equal (mapcar (lambda (s) (alist-get 'amount s)) scheds)
                       '("4" "2")))))))

(ert-deftest harp-med-edit-preserves-refill ()
  "Editing a med round-trips :REFILL: (it used to be erased on every save)."
  (harp-tests--with-temp-profile "meds.org"
    (let* ((id "11111111-0000-4000-8000-000000000001")
           (ibu (harp-med-by-id id (harp-profile-read file))))
      (should (= 30 (alist-get 'refill ibu)))
      (harp-med-form-open ibu)
      (jetpacs-ui-state-put (harp-med-form--fid "stock") "25")
      (should (harp-med-form--save))
      (should (= 30 (alist-get 'refill
                               (harp-med-by-id id (harp-profile-read file)))))
      ;; and the field itself is editable
      (harp-med-form-open (harp-med-by-id id (harp-profile-read file)))
      (jetpacs-ui-state-put (harp-med-form--fid "refill") "60")
      (should (harp-med-form--save))
      (should (= 60 (alist-get 'refill
                               (harp-med-by-id id (harp-profile-read file))))))))

(ert-deftest harp-med-schedule-multiday-interval ()
  "A `3d' interval fires every third day from its anchor, not daily."
  (require 'harp-med-schedule)
  (let ((med '((active . t) (unit . "tab")
               (schedules . (((amount . "1") (interval . "3d")
                              (from . "<2026-07-01>")
                              (window . "08:00-22:00")))))))
    (cl-flet ((doses (d) (harp-med-schedule-doses-on
                          med (harp-tests--date d))))
      (should (= 1 (length (doses "2026-07-01"))))
      (should (equal "08:00" (alist-get 'time (car (doses "2026-07-01")))))
      (should (= 0 (length (doses "2026-07-02"))))
      (should (= 0 (length (doses "2026-07-03"))))
      (should (= 1 (length (doses "2026-07-04"))))
      (should (= 1 (length (doses "2026-07-07")))))))

(ert-deftest harp-dose-actions-carry-dedupe ()
  "Dose taps and notification buttons carry per-dose dedupe keys."
  (require 'harp-med-notify)
  (let* ((med '((id . "m1") (name . "T") (unit . "tab")))
         (row (json-serialize (harp-med-view--dose-actions med "1" "08:00")))
         (spec (json-serialize
                (harp-med-notify--spec med '((time . "08:00") (amount . "1")
                                             (unit . "tab") (at . 1))))))
    (should (string-match-p "\"dedupe\":\"harp\\.med\\.taken/m1/" row))
    (should (string-match-p "\"dedupe\":\"harp\\.med\\.skipped/m1/" row))
    (should (string-match-p "harp\\.med\\.taken/m1/" spec))
    (should (string-match-p "harp\\.med\\.snooze/m1/" spec))))

(ert-deftest harp-export-excludes-private-by-default ()
  "Private items stay out of both export formats unless opted in."
  (harp-tests--with-temp-profile "diego.org"
    (let* ((p (harp-profile-read file))
           (rehab (cl-find "Rehab" (alist-get 'journals p)
                           :key (lambda (j) (alist-get 'name j)) :test #'equal))
           (newest (car (alist-get 'entries rehab))))
      (should (string-match-p "Lower pain" (alist-get 'body newest)))
      (harp-write-toggle-private (alist-get 'ref newest))
      (let ((p2 (harp-profile-read file)))
        (should-not (string-match-p "Lower pain" (harp-export-text p2)))
        (should-not (string-match-p "Lower pain" (harp-export-html p2)))
        (should (string-match-p "Lower pain" (harp-export-text p2 nil t)))
        (should (string-match-p "Lower pain" (harp-export-html p2 nil t)))
        ;; non-private content still exports
        (should (string-match-p "Knee mobility" (harp-export-text p2)))))))

(ert-deftest harp-export-share-size-guard ()
  "Oversized org shares fall back to a file instead of a doomed intent."
  (let* ((tmp (make-temp-file "harp-exp" t))
         (harp-export-directory tmp)
         (profile '((name . "Big")))
         (intents 0))
    (unwind-protect
        (cl-letf (((symbol-function 'jetpacs-device-intent)
                   (lambda (&rest _) (cl-incf intents))))
          ;; small: shared inline
          (should (eq 'shared (harp-export--share-text "small" "s" profile)))
          (should (= 1 intents))
          ;; big: written to a file, no intent
          (let ((result (harp-export--share-text
                         (make-string (1+ harp-export-share-max-bytes) ?x)
                         "s" profile)))
            (should (stringp result))
            (should (file-exists-p result))
            (should (= 1 intents))))
      (delete-directory tmp t))))

(ert-deftest harp-profiles-discovery-opens-no-buffers ()
  "Discovery head-reads candidates without leaving visiting buffers."
  (let ((harp-directory (expand-file-name "fixtures/" harp-tests--dir)))
    ;; start clean: no fixture is being visited
    (dolist (f (directory-files-recursively harp-directory "\\.org\\'"))
      (when-let ((b (find-buffer-visiting f)))
        (with-current-buffer b (set-buffer-modified-p nil))
        (kill-buffer b)))
    (let ((infos (harp-profiles)))
      (should (= 4 (length infos)))
      (dolist (f (harp-profile-files))
        (should-not (find-buffer-visiting f))))))

(ert-deftest harp-med-notify-snooze-pruned ()
  "Expired snoozes are dropped; live ones are kept."
  (require 'harp-med-notify)
  (clrhash harp-med-notify--snoozed)
  (puthash "old\0t" (- (float-time) 10) harp-med-notify--snoozed)
  (puthash "new\0t" (+ (float-time) 600) harp-med-notify--snoozed)
  (harp-med-notify--prune-snoozed)
  (should-not (gethash "old\0t" harp-med-notify--snoozed))
  (should (gethash "new\0t" harp-med-notify--snoozed))
  (clrhash harp-med-notify--snoozed))

(ert-deftest harp-med-refill-round-trip ()
  "Refill adds the pack size to stock (MedTimer's refill)."
  (harp-tests--with-temp-profile "meds.org"
    (let* ((id "11111111-0000-4000-8000-000000000001")
           (ibu (harp-med-by-id id (harp-profile-read file))))
      (should (= 60 (harp-med-log-refill ibu)))
      (should (= 60 (alist-get 'stock (harp-med-by-id id (harp-profile-read file)))))
      ;; a med without :REFILL: refuses quietly
      (let ((taper (harp-med-by-id "44444444-0000-4000-8000-000000000004"
                                   (harp-profile-read file))))
        (should-not (harp-med-log-refill taper))))))

(ert-deftest harp-med-view-badge-counts-due ()
  "The Meds tab badge shows the due-dose count, nil when caught up."
  (require 'harp-med-view)
  (let ((due-profile '((medications . (((id . "m1") (name . "T") (unit . "tab")
                                        (active . t)
                                        (schedules . (((amount . "1")
                                                       (times "00:01"))))
                                        (doses . nil))))))
        (done-profile `((medications . (((id . "m1") (name . "T") (unit . "tab")
                                         (active . t)
                                         (schedules . (((amount . "1")
                                                        (times "00:01"))))
                                         (doses . (((state . "taken")
                                                    (stamp . ,(format-time-string
                                                               "%Y-%m-%d 00:05")))))))))))
    ;; `harp-med-due-today' memoises via `harp-ui-memo'; stubbing
    ;; `harp-ui-current' bypasses the (file . mtime) generation that would
    ;; normally bust it, so invalidate between the two profiles the way a real
    ;; profile swap (through `harp-ui-set-current') does.
    (cl-letf (((symbol-function 'harp-ui-current) (lambda () due-profile)))
      (harp-ui-invalidate)
      (should (= 1 (harp-med-view--badge))))
    (cl-letf (((symbol-function 'harp-ui-current) (lambda () done-profile)))
      (harp-ui-invalidate)
      (should-not (harp-med-view--badge)))))

(ert-deftest harp-ui-memo-shares-within-push-busts-on-invalidate ()
  "`harp-ui-memo' computes once per key, then recomputes after invalidate.
This is what lets one push (which builds every visible tab) share a single
tags/due-today/launcher computation instead of repeating it per tab."
  (let ((harp-ui--current-file nil) (harp-ui--cache nil) (harp-ui--derived nil)
        (calls 0))
    (let ((thunk (lambda () (cl-incf calls) 'value)))
      ;; Same key inside one generation: thunk runs exactly once.
      (should (eq 'value (harp-ui-memo 'k thunk)))
      (should (eq 'value (harp-ui-memo 'k thunk)))
      (should (eq 'value (harp-ui-memo 'k thunk)))
      (should (= 1 calls))
      ;; A different key computes independently.
      (should (eq 'value (harp-ui-memo 'other thunk)))
      (should (= 2 calls))
      ;; Invalidate (as a mutation does) busts the memo — recompute.
      (harp-ui-invalidate)
      (should (eq 'value (harp-ui-memo 'k thunk)))
      (should (= 3 calls)))))

(ert-deftest harp-reminders-occurrences-expand-repeaters ()
  "A +2d repeater marks every occurrence in a later month; plain stamps once."
  (require 'harp-reminders)
  (let ((march (harp-reminders--occurrences-in-month "<2026-02-02 +2d>" "2026-03")))
    (should march)
    (should (cl-every (lambda (d) (string-prefix-p "2026-03" d)) march))
    (should (member "2026-03-02" march)))
  (should (equal '("2026-04-15")
                 (harp-reminders--occurrences-in-month "<2026-04-15>" "2026-04")))
  (should-not (harp-reminders--occurrences-in-month "<2026-04-15>" "2026-05")))

(ert-deftest harp-medications-settings-registered ()
  "The Medications settings section exposes the notify defcustoms."
  (require 'harp-med-notify)
  (should (assoc "Medications" jetpacs-settings-registry)))

;; ─── Hardening II — P0: file corruption & hangs ─────────────────────────────

(ert-deftest harp-p0-body-star-line-does-not-corrupt-outline ()
  "An entry body line starting with `* ' is escaped, not read as a heading."
  (harp-tests--with-temp-profile "diego.org"
    (let* ((p (harp-profile-read file))
           (rehab (cl-find "Rehab" (alist-get 'journals p)
                           :key (lambda (j) (alist-get 'name j)) :test #'equal))
           (e (car (alist-get 'entries rehab)))
           (journals-before (length (alist-get 'journals p))))
      (harp-form-open "entry" e)
      (jetpacs-ui-state-put (harp-form--fid "body")
                            "felt ok\n* dizzy after lunch\nmore text")
      (should (harp-form--save))
      (let* ((p2 (harp-profile-read file))
             (rehab2 (cl-find "Rehab" (alist-get 'journals p2)
                              :key (lambda (j) (alist-get 'name j)) :test #'equal))
             (e2 (cl-find (alist-get 'id e) (alist-get 'entries rehab2)
                          :key (lambda (x) (alist-get 'id x)) :test #'equal)))
        ;; no journal vanished, and the whole body survived intact
        (should (= journals-before (length (alist-get 'journals p2))))
        (should (string-match-p "dizzy after lunch" (alist-get 'body e2)))
        (should (string-match-p "more text" (alist-get 'body e2)))))))

(ert-deftest harp-p0-med-notes-star-line-keeps-schedule ()
  "A `* ' line in medication notes does not detach the *** Schedule subtree."
  (harp-tests--with-temp-profile "meds.org"
    (let* ((id "11111111-0000-4000-8000-000000000001")
           (ibu (harp-med-by-id id (harp-profile-read file))))
      (harp-med-form-open ibu)
      (jetpacs-ui-state-put (harp-med-form--fid "notes")
                            "take with food\n* do not crush")
      (should (harp-med-form--save))
      (let ((ibu2 (harp-med-by-id id (harp-profile-read file))))
        (should (= 1 (length (alist-get 'schedules ibu2))))
        (should (string-match-p "do not crush" (alist-get 'notes ibu2)))))))

(ert-deftest harp-p0-attachment-links-no-hang ()
  "A leading-space attachment link returns promptly (was an infinite loop)."
  (with-timeout (5 (ert-fail "harp--attachment-links hung"))
    (should (equal '("x.jpg")
                   (harp--attachment-links "see [[attachment: x.jpg]] ok")))
    (should (equal '("a.png" "b.pdf")
                   (harp--attachment-links
                    "[[attachment:a.png]] and [[attachment:  b.pdf ]]")))
    t))

(ert-deftest harp-p0-dose-amount-survives-padded-timestamp ()
  "The dose AMOUNT is not dropped when the logbook timestamp has padding."
  (let* ((tmp (make-temp-file "harp-ts" t))
         (file (expand-file-name "m.org" tmp)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ":PROPERTIES:\n:ID: ts-1\n:HARP_TYPE: human\n:END:\n"
                    "#+TITLE: T\n\n* Journals\n** Main\n\n* Medications\n"
                    "** Aspirin\n:PROPERTIES:\n:ID: a1\n:HARP_MED: t\n:UNIT: tab\n:END:\n"
                    ":LOGBOOK:\n- taken [ 2026-07-14 08:05 ] 2 tab\n:END:\n"))
          (let* ((med (car (alist-get 'medications (harp-profile-read file))))
                 (dose (car (harp-med-doses med))))
            (should (equal "taken" (alist-get 'state dose)))
            (should (equal "2 tab" (alist-get 'amount dose)))))
      (dolist (b (buffer-list))
        (when (and (buffer-file-name b) (string-prefix-p tmp (buffer-file-name b)))
          (with-current-buffer b (set-buffer-modified-p nil)) (kill-buffer b)))
      (delete-directory tmp t))))

(ert-deftest harp-p0-logbook-last-heading-no-newline ()
  "First dose on a med that is the last heading of a newline-less file keeps
the med parseable (no `:END::LOGBOOK:' fusion)."
  (harp-tests--with-temp-profile "meds.org"
    ;; strip the trailing newline so org-end-of-meta-data lands mid-line at eob
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-max))
      (while (and (> (point) (point-min)) (eq (char-before) ?\n))
        (delete-char -1))
      (let ((save-silently t)) (save-buffer)))
    (let ((vit (harp-med-by-id "55555555-0000-4000-8000-000000000005"
                               (harp-profile-read file))))
      (harp-med-log-write (alist-get 'ref vit) "taken" "1" "tab" nil)
      (let ((vit2 (harp-med-by-id "55555555-0000-4000-8000-000000000005"
                                  (harp-profile-read file))))
        (should vit2)                                  ; med still parses
        (should (= 1 (length (harp-med-doses vit2)))))))) ; dose recorded

(ert-deftest harp-p0-hyphenated-tags-round-trip ()
  "A tag with a hyphen is sanitised so it parses back as a tag."
  (harp-tests--with-temp-profile "diego.org"
    (harp-form-open "entry")
    (jetpacs-ui-state-put (harp-form--fid "body") "reading")
    (jetpacs-ui-state-put (harp-form--fid "tags") "blood-pressure")
    (should (harp-form--save))
    (let* ((p (harp-profile-read file))
           (main (cl-find "Rehab" (alist-get 'journals p)
                          :key (lambda (j) (alist-get 'name j)) :test #'equal))
           (newest (car (alist-get 'entries main))))
      (should (member "blood_pressure" (alist-get 'heading-tags newest))))))

(ert-deftest harp-p0-add-entry-to-decorated-journal ()
  "Adding an entry to a journal heading that carries tags succeeds."
  (harp-tests--with-temp-profile "diego.org"
    ;; diego's Rehab journal heading carries no tags; give it one on disk
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (re-search-forward "^\\*\\* Rehab")
       (end-of-line) (insert "    :active:")
       (let ((save-silently t)) (save-buffer))))
    (setq harp-journal--selected "Rehab")
    (let ((n (length (alist-get 'entries
                                (cl-find "Rehab" (alist-get 'journals (harp-profile-read file))
                                         :key (lambda (j) (alist-get 'name j)) :test #'equal)))))
      (harp-form-open "entry")
      (jetpacs-ui-state-put (harp-form--fid "body") "new note")
      (should (harp-form--save))
      (let ((rehab (cl-find "Rehab" (alist-get 'journals (harp-profile-read file))
                            :key (lambda (j) (alist-get 'name j)) :test #'equal)))
        (should (= (1+ n) (length (alist-get 'entries rehab))))))))

(ert-deftest harp-p0-same-named-meds-no-doubled-schedules ()
  "Two medications with the same name each keep only their own schedule."
  (let* ((tmp (make-temp-file "harp-dup" t))
         (file (expand-file-name "dup.org" tmp)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ":PROPERTIES:\n:ID: dup-1\n:HARP_TYPE: human\n:END:\n"
                    "#+TITLE: Dup\n\n* Journals\n** Main\n\n* Medications\n"
                    "** Aspirin\n:PROPERTIES:\n:ID: a1\n:HARP_MED: t\n:END:\n"
                    "*** Schedule\n:PROPERTIES:\n:AMOUNT: 1\n:TIMES: 08:00\n:END:\n"
                    "** Aspirin\n:PROPERTIES:\n:ID: a2\n:HARP_MED: t\n:END:\n"
                    "*** Schedule\n:PROPERTIES:\n:AMOUNT: 2\n:TIMES: 20:00\n:END:\n"))
          (let ((meds (alist-get 'medications (harp-profile-read file))))
            (should (= 2 (length meds)))
            (should (equal '("1") (mapcar (lambda (s) (alist-get 'amount s))
                                          (alist-get 'schedules (nth 0 meds)))))
            (should (equal '("2") (mapcar (lambda (s) (alist-get 'amount s))
                                          (alist-get 'schedules (nth 1 meds)))))))
      (dolist (b (buffer-list))
        (when (and (buffer-file-name b) (string-prefix-p tmp (buffer-file-name b)))
          (with-current-buffer b (set-buffer-modified-p nil)) (kill-buffer b)))
      (delete-directory tmp t))))

;; ─── Hardening II — P1: edit round-trips preserve unmodeled content ──────────

(defun harp-p1--write (file text) (with-temp-file file (insert text)))

(ert-deftest harp-p1-entry-edit-preserves-meta ()
  "Editing an entry keeps its planning line, extra properties, hand drawer,
and ATTACH tag; the org-attach dir is not orphaned."
  (let* ((tmp (make-temp-file "harp-p1e" t))
         (file (expand-file-name "p.org" tmp))
         (harp-directory tmp) (harp-ui--current-file file) (harp-ui--cache nil)
         (org-id-track-globally nil) (org-agenda-files nil))
    (unwind-protect
        (progn
          (harp-p1--write file
            (concat ":PROPERTIES:\n:ID: prof\n:HARP_TYPE: human\n:END:\n#+TITLE: P\n\n"
                    "* Journals\n** Main\n"
                    "*** Entry    :physio:ATTACH:\n"
                    "SCHEDULED: <2026-07-20>\n"
                    ":PROPERTIES:\n:ID: e1\n:DATETIME: <2026-07-01 09:00>\n:PRIVATE: nil\n"
                    ":CREATED: [2026-07-01 Wed]\n:ATTACH_DIR: ./data/e1/\n:END:\n"
                    ":LOGBOOK:\n- Note taken on [2026-07-02 Thu]\n:END:\n"
                    "original body\n* Documents\n"))
          (let ((e (car (alist-get 'entries
                                   (car (alist-get 'journals (harp-profile-read file)))))))
            (harp-form-open "entry" e)
            (jetpacs-ui-state-put (harp-form--fid "body") "revised body")
            (should (harp-form--save)))
          (let ((raw (with-temp-buffer (insert-file-contents file) (buffer-string))))
            (should (string-match-p "SCHEDULED: <2026-07-20>" raw))
            (should (string-match-p ":CREATED:" raw))
            (should (string-match-p ":ATTACH_DIR: ./data/e1/" raw))
            (should (string-match-p "Note taken on" raw))
            (should (string-match-p ":physio:ATTACH:" raw))
            (should (string-match-p "revised body" raw))
            (should-not (string-match-p "original body" raw))))
      (dolist (b (buffer-list))
        (when (and (buffer-file-name b) (string-prefix-p tmp (buffer-file-name b)))
          (with-current-buffer b (set-buffer-modified-p nil)) (kill-buffer b)))
      (delete-directory tmp t))))

(ert-deftest harp-p1-metric-edit-preserves-body ()
  "Editing a metric keeps its description body (never modeled)."
  (harp-tests--with-temp-profile "diego.org"
    ;; give the hr metric a description body on disk
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (re-search-forward "^:TAG_ID: hr$")
       (re-search-forward "^:END:$")
       (end-of-line) (insert "\nMeasured at rest, seated.")
       (let ((save-silently t)) (save-buffer))))
    (let ((hr (cl-find "hr" (alist-get 'metrics (harp-profile-read file))
                       :key (lambda (m) (alist-get 'id m)) :test #'equal)))
      (harp-form-open "metric" hr)
      (jetpacs-ui-state-put (harp-form--fid "unit") "beats/min")
      (should (harp-form--save)))
    (let ((raw (with-temp-buffer (insert-file-contents file) (buffer-string))))
      (should (string-match-p "Measured at rest, seated." raw))
      ;; org-entry-put re-aligns property values, so allow flexible spacing
      (should (string-match-p ":UNIT:[ \t]+beats/min" raw)))))

(ert-deftest harp-p1-resolve-ref-id-first ()
  "harp--resolve-ref finds an item by its :ID: even when the recorded
position is stale, and never overwrites a same-headline sibling."
  (harp-tests--with-temp-profile "diego.org"
    (let* ((p (harp-profile-read file))
           (rehab (cl-find "Rehab" (alist-get 'journals p)
                           :key (lambda (j) (alist-get 'name j)) :test #'equal))
           (entries (alist-get 'entries rehab))
           (target (nth 1 entries))            ; the OLDER Rehab entry
           (target-id (alist-get 'id target))
           (ref (alist-get 'ref target)))
      ;; corrupt the recorded position — shift everything below the file-level
      ;; drawer down (must not disturb the top :PROPERTIES: drawer itself)
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (re-search-forward "^#\\+TITLE:.*$")
         (end-of-line)
         (insert "\n\npadding paragraph that shifts every heading downward\n")
         (let ((save-silently t)) (save-buffer))))
      ;; edit via that now-stale ref
      (harp-write-update-entry ref "2026-01-03" "08:20" '("physio") nil "EDITED marker")
      (let* ((p2 (harp-profile-read file))
             (rehab2 (cl-find "Rehab" (alist-get 'journals p2)
                              :key (lambda (j) (alist-get 'name j)) :test #'equal))
             (edited (cl-find target-id (alist-get 'entries rehab2)
                              :key (lambda (e) (alist-get 'id e)) :test #'equal)))
        ;; the RIGHT entry got the edit, and no entry was destroyed
        (should (= (length entries) (length (alist-get 'entries rehab2))))
        (should (string-match-p "EDITED marker" (alist-get 'body edited)))))))

(ert-deftest harp-p1-med-notes-strip-logbook ()
  "A logbook drawer sitting below hand-authored notes is not captured into
`notes' (which would duplicate it on the next edit)."
  (let* ((tmp (make-temp-file "harp-p1m" t))
         (file (expand-file-name "m.org" tmp)))
    (unwind-protect
        (progn
          (harp-p1--write file
            (concat ":PROPERTIES:\n:ID: prof\n:HARP_TYPE: human\n:END:\n#+TITLE: M\n\n"
                    "* Journals\n** Main\n* Medications\n"
                    "** Aspirin\n:PROPERTIES:\n:ID: a1\n:HARP_MED: t\n:UNIT: tab\n:END:\n"
                    "take with water\n"
                    ":LOGBOOK:\n- taken [2026-07-14 08:05] 1 tab\n:END:\n"
                    "*** Schedule\n:PROPERTIES:\n:AMOUNT: 1\n:TIMES: 08:00\n:END:\n"))
          (let ((med (car (alist-get 'medications (harp-profile-read file)))))
            (should (equal "take with water" (alist-get 'notes med)))
            (should (= 1 (length (harp-med-doses med))))))
      (delete-directory tmp t))))

;; ─── Hardening II — P2: offline queue integrity ─────────────────────────────

(ert-deftest harp-p2-dose-logged-at-queued-time ()
  "A replayed dose is stamped with its `queued_at' time, not drain time."
  (require 'harp-med-log)
  (harp-tests--with-temp-profile "meds.org"
    (let ((queued-ms (* 1000 (float-time (harp-tests--date "2020-03-04")))))
      (harp-med-log--dose `((id . "11111111-0000-4000-8000-000000000001")
                            (amount . "1"))
                          `((queued_at . ,queued-ms)) "taken")
      (let* ((ibu (harp-med-by-id "11111111-0000-4000-8000-000000000001"
                                  (harp-profile-read file)))
             (dose (car (harp-med-doses ibu))))
        (should (equal "taken" (alist-get 'state dose)))
        (should (string-prefix-p "2020-03-04" (alist-get 'stamp dose)))))))

(ert-deftest harp-p2-dose-resolves-via-file-arg ()
  "A dose tap resolves its med from the baked-in profile file even when no
profile (or a different one) is open."
  (require 'harp-med-log)
  (harp-tests--with-temp-profile "meds.org"
    (let ((target file))
      ;; simulate a restart: nothing open
      (let ((harp-ui--current-file nil) (harp-ui--cache nil))
        (harp-med-log--dose `((id . "11111111-0000-4000-8000-000000000001")
                              (amount . "1") (unit . "tab")
                              (file . ,target))
                            nil "taken"))
      (let ((ibu (harp-med-by-id "11111111-0000-4000-8000-000000000001"
                                 (harp-profile-read target))))
        (should (equal "taken" (alist-get 'state (car (harp-med-doses ibu)))))
        (should (= 29 (alist-get 'stock ibu)))))))  ; 30 - 1

(ert-deftest harp-p2-dose-unresolvable-is-loud ()
  "An unresolvable dose warns and writes nothing (never silently dropped)."
  (require 'harp-med-log)
  (harp-tests--with-temp-profile "meds.org"
    (let ((harp-ui--current-file nil) (harp-ui--cache nil)
          (warned nil))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (&rest _) (setq warned t))))
        (harp-med-log--dose '((id . "no-such-med") (amount . "1")) nil "taken"))
      (should warned))))

(ert-deftest harp-p2-export-private-reads-ui-state ()
  "The export include-private flag is read from UI state (state.changed),
not an on_change action that never fires."
  (require 'harp-export)
  (jetpacs-ui-state-put harp-export--private-field nil)
  (should-not (harp-export--private-p))
  (jetpacs-ui-state-put harp-export--private-field t)
  (should (harp-export--private-p))
  (jetpacs-ui-state-put harp-export--private-field :false)
  (should-not (harp-export--private-p))
  (jetpacs-ui-state-put harp-export--private-field nil))

(ert-deftest harp-p2-form-save-expired-is-loud ()
  "A queued form save with no open form notifies rather than silently no-ops."
  (require 'harp-form)
  (let ((harp-form--open nil) (msg nil))
    (cl-letf (((symbol-function 'jetpacs-shell-notify)
               (lambda (m) (setq msg m)))
              ((symbol-function 'jetpacs-shell-push) #'ignore))
      (funcall (gethash "harp.form.save" jetpacs-action-handlers) nil nil)
      (should (string-match-p "expired" (or msg ""))))))

;; ─── Hardening II — P3: scheduler & DST correctness ─────────────────────────

(ert-deftest harp-p3-fractional-day-interval ()
  "A 36h interval alternates 08:00/20:00 across days, not once every day."
  (require 'harp-med-schedule)
  (let ((med '((active . t) (unit . "tab")
               (schedules . (((amount . "1") (interval . "36h")
                              (from . "<2026-07-01>") (window . "08:00-22:00")))))))
    (cl-flet ((times (d) (mapcar (lambda (x) (alist-get 'time x))
                                 (harp-med-schedule-doses-on med (harp-tests--date d)))))
      (should (equal '("08:00") (times "2026-07-01")))
      (should (equal '("20:00") (times "2026-07-02")))
      (should (equal '() (times "2026-07-03")))
      (should (equal '("08:00") (times "2026-07-04"))))))

(ert-deftest harp-p3-whole-day-interval-still-daily-time ()
  "A 48h interval keeps firing at a fixed daily time every 2 days."
  (require 'harp-med-schedule)
  (let ((med '((active . t) (unit . "tab")
               (schedules . (((amount . "1") (interval . "48h")
                              (from . "<2026-07-01>") (window . "09:00-21:00")))))))
    (cl-flet ((times (d) (mapcar (lambda (x) (alist-get 'time x))
                                 (harp-med-schedule-doses-on med (harp-tests--date d)))))
      (should (equal '("09:00") (times "2026-07-01")))
      (should (equal '() (times "2026-07-02")))
      (should (equal '("09:00") (times "2026-07-03"))))))

(ert-deftest harp-p3-old-subdaily-repeater-resolves ()
  "A small repeater on a stamp years old still yields a future occurrence
\(the mathematical jump keeps it from exhausting the step cap)."
  (require 'harp-reminders)
  (let ((next (harp-reminders--next-time "<2015-01-01 08:00 +6h>")))
    (should next)
    (should (> next (float-time))))
  ;; and it lands on a real +6h boundary relative to the base (00/06/12/18h)
  (let* ((next (harp-reminders--next-time "<2015-01-01 08:00 +6h>"))
         (h (decoded-time-hour (decode-time (seconds-to-time next)))))
    (should (memq (mod h 6) '(0 2 4)))))  ; 08:00 + k*6h -> hours 8,14,20,2 → mod 6 ∈ {2,0,4}

(ert-deftest harp-p3-old-repeater-marks-in-month ()
  "An old +2d repeater marks its occurrences in a far-future month."
  (require 'harp-reminders)
  (let ((dates (harp-reminders--occurrences-in-month "<2015-01-01 +2d>" "2030-06")))
    (should dates)
    (should (cl-every (lambda (d) (string-prefix-p "2030-06" d)) dates))
    (should (> (length dates) 10))))       ; ~15 occurrences in a 30-day month

(ert-deftest harp-p3-dst-step-keeps-wall-clock ()
  "Stepping a repeater by a day across a DST transition keeps the clock time."
  (require 'harp-reminders)
  (let ((orig (getenv "TZ")))
    (unwind-protect
        (progn
          (setenv "TZ" "America/New_York")
          ;; US spring-forward is 2026-03-08; +1d from the 7th must stay 08:00
          (let ((after (harp-reminders--step
                        (org-time-string-to-time "<2026-03-07 08:00>") 1 ?d)))
            (should (equal "2026-03-08 08:00"
                           (format-time-string "%Y-%m-%d %H:%M" after))))
          ;; and a dose datetime on the far side is not an hour off
          (let* ((date (seconds-to-time (harp--time-of "<2026-03-08 12:00>")))
                 (at (harp-med-schedule--datetime date "08:00")))
            (should (equal "2026-03-08 08:00"
                           (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time at))))))
      (setenv "TZ" orig))))

(ert-deftest harp-p3-times-zero-padded ()
  "Unpadded schedule times are normalised so string ordering is correct."
  ;; normalised on parse (order preserved, not sorted here)
  (should (equal '("21:00" "09:00") (harp--split-times "21:00, 9:00")))
  ;; and the due list then sorts them right (9am before 9pm)
  (let ((med `((active . t) (unit . "tab")
               (schedules . (((amount . "1")
                              (times . ,(harp--split-times "21:00, 9:00"))))))))
    (should (equal '("09:00" "21:00")
                   (mapcar (lambda (x) (alist-get 'time x))
                           (harp-med-schedule-doses-on med (harp-tests--date "2026-07-01")))))))

;; ─── Hardening II — P5: coverage holes ──────────────────────────────────────

(ert-deftest harp-p5-document-add-edit-roundtrip ()
  "A document survives add → read → edit → read with every field intact."
  (harp-tests--with-temp-profile "diego.org"
    ;; ── add a new document ──
    (harp-form-open "document")
    (jetpacs-ui-state-put (harp-form--fid "name") "Blood Panel")
    (jetpacs-ui-state-put (harp-form--fid "date") "2026-05-10")
    (jetpacs-ui-state-put (harp-form--fid "time") "09:30")
    (jetpacs-ui-state-put (harp-form--fid "source") "Lab Central")
    (jetpacs-ui-state-put (harp-form--fid "files") "panel.pdf, chart.png")
    (jetpacs-ui-state-put (harp-form--fid "tags") "labs bloodwork")
    (jetpacs-ui-state-put (harp-form--fid "annotation") "All values in range.")
    (should (harp-form--save))
    (let ((doc (cl-find "Blood Panel" (alist-get 'documents (harp-profile-read file))
                        :key (lambda (d) (alist-get 'name d)) :test #'equal)))
      (should doc)
      (should (equal (alist-get 'source doc) "Lab Central"))
      (should (equal (alist-get 'files doc) '("panel.pdf" "chart.png")))
      (should (member "labs" (alist-get 'heading-tags doc)))
      (should (equal (alist-get 'annotation doc) "All values in range."))
      ;; ── edit it: change the annotation and add a tag ──
      (harp-form-open "document" doc)
      (jetpacs-ui-state-put (harp-form--fid "annotation") "Follow-up in 6 months.")
      (jetpacs-ui-state-put (harp-form--fid "tags") "labs bloodwork urgent")
      (should (harp-form--save)))
    (let* ((docs (alist-get 'documents (harp-profile-read file)))
           (doc2 (cl-find "Blood Panel" docs
                          :key (lambda (d) (alist-get 'name d)) :test #'equal)))
      (should doc2)
      (should (equal (alist-get 'annotation doc2) "Follow-up in 6 months."))
      (should (member "urgent" (alist-get 'heading-tags doc2)))
      ;; unedited fields survived the in-place update …
      (should (equal (alist-get 'source doc2) "Lab Central"))
      (should (equal (alist-get 'files doc2) '("panel.pdf" "chart.png")))
      ;; … and the edit updated in place rather than duplicating the heading.
      (should (= 1 (cl-count "Blood Panel" docs
                             :key (lambda (d) (alist-get 'name d)) :test #'equal))))))

(ert-deftest harp-p5-profile-open-path-allowlist ()
  "`harp.profile.open' only opens a discovered profile file — never an
arbitrary path off the wire (the app's one explicit wire-security check)."
  (harp-tests--with-temp-profile "diego.org"
    (let* ((handler (gethash "harp.profile.open" jetpacs-action-handlers))
           (opened nil))
      (should handler)
      (cl-letf (((symbol-function 'harp-ui-set-current)
                 (lambda (f) (setq opened f)))
                ((symbol-function 'jetpacs-shell-set-current-tab) #'ignore)
                ((symbol-function 'jetpacs-shell-push) #'ignore))
        ;; a real discovered profile opens
        (funcall handler `((file . ,file)) nil)
        (should (equal opened file))
        ;; an off-wire path is rejected — the setter is never reached
        (setq opened nil)
        (funcall handler '((file . "/etc/passwd")) nil)
        (should-not opened)
        (setq opened nil)
        (funcall handler `((file . ,(expand-file-name "nope.org" harp-directory))) nil)
        (should-not opened)))))

(provide 'harp-tests)
;;; harp-tests.el ends here
