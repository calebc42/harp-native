;;; harp-demo.el --- Bundled sample profiles for trying Harp out -*- lexical-binding: t; -*-

;; GENERATED FILE -- do not edit by hand.  Produced by emacs/build-demo.el from
;; the test/fixtures/*.org samples.
;;
;; `harp-demo-setup' writes a few ready-made profiles (a person, a pair of cats,
;; a dog) into `harp-directory' and opens the first, so a fresh install has
;; something to explore.  The human profile carries a couple of medications.
;; Attachment media are not bundled, so attachment chips show file names only.

;;; Code:

(require 'cl-lib)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
(require 'harp-profile)
(require 'harp-ui)

(defconst harp-demo--profiles
  (list
   (cons "diego-recovery"
":PROPERTIES:
:ID: b8f2c0ad-6a2c-4f97-9ad0-0d9222220002
:HARP_NAME: Diego Alvarez
:HARP_TYPE: human
:END:
#+TITLE: Diego Recovery Notes

* Metadata
** Sources

** Metrics
*** Heart Rate    :cardio:
:PROPERTIES:
:TAG_ID: hr
:UNIT: bpm
:RANGE: 35-220
:HEALTHY_RANGE: 55-95
:END:

*** Systolic BP    :cardio:
:PROPERTIES:
:TAG_ID: sbp
:UNIT: mmHg
:RANGE: 70-220
:HEALTHY_RANGE: 95-125
:END:

*** Pain Scale    :rehab:
:PROPERTIES:
:TAG_ID: pain
:UNIT: score
:RANGE: 0-10
:HEALTHY_RANGE: 0-2
:END:

* Journals
** Rehab
*** Entry    :physio:ATTACH:
:PROPERTIES:
:ID: 44f11210-a050-4d1b-8aa1-900200000001
:DATETIME: <2026-01-03 08:20>
:PRIVATE: nil
:END:

Knee mobility session #pain(4) #hr(102)
Video for gait review [[attachment:gait-session.mp4]]

*** Entry
:PROPERTIES:
:ID: 44f11210-a050-4d1b-8aa1-900200000002
:DATETIME: <2026-01-10 08:10>
:PRIVATE: nil
:END:

Lower pain with better balance #pain(2) #sbp(122)

** Travel
*** Entry    :ATTACH:
:PROPERTIES:
:ID: 44f11210-a050-4d1b-8aa1-900200000003
:DATETIME: <2026-01-11 18:40>
:PRIVATE: nil
:END:

Carried meds and support brace [[attachment:trip-kit.jpg]] #routine

* Documents
** MRI Report    :ATTACH:imaging:
:PROPERTIES:
:SOURCE: Centro Medico Norte
:FILES: knee-mri-summary.pdf, knee-mri-slice-1.png
:ID: 55cc914a-4889-4f62-98c4-900200000001
:DATETIME: <2025-12-29 11:15>
:PRIVATE: nil
:END:

Consultant notes meniscus inflammation.

** Insurance Approval    :ATTACH:admin:
:PROPERTIES:
:SOURCE: Seguro Vida
:FILES: approval-letter.pdf
:ID: 55cc914a-4889-4f62-98c4-900200000002
:DATETIME: <2026-01-04 12:00>
:PRIVATE: nil
:END:

Approved 8 physio sessions.

* Reminders
** TODO Stretch Session    :rehab:
SCHEDULED: <2026-02-02 +2d>
:PROPERTIES:
:ID: 66fbe8a2-5ec3-4ff8-b990-900200000001
:END:
Maintain quadriceps and hamstrings routine.

** TODO Renew Brace    :admin:
DEADLINE: <2026-04-15>
:PROPERTIES:
:ID: 66fbe8a2-5ec3-4ff8-b990-900200000002
:END:
Order replacement knee brace before travel.

* Medications
** Ibuprofen    :pain:
:PROPERTIES:
:ID: 77a1b2c3-0000-4000-8000-900200000001
:HARP_MED: t
:UNIT: tab
:STOCK: 24
:LOW_STOCK: 6
:ACTIVE: t
:END:
Take with food for knee pain.
*** Schedule
:PROPERTIES:
:AMOUNT: 1
:TIMES: 08:00, 20:00
:END:

** Vitamin D    :supplement:
:PROPERTIES:
:ID: 77a1b2c3-0000-4000-8000-900200000002
:HARP_MED: t
:UNIT: tab
:STOCK: 90
:ACTIVE: t
:END:
*** Schedule
:PROPERTIES:
:AMOUNT: 1
:TIMES: 09:00
:DAYS: Mon Wed Fri
:END:
")
   (cons "kenji-and-luna"
":PROPERTIES:
:ID: e8f2c0ad-6a2c-4f97-9ad0-0d9555550005
:HARP_NAME: Kenji and Luna
:HARP_TYPE: cat
:END:
#+TITLE: Kenji and Luna Household Record

* Metadata
** Sources

** Metrics
*** Weight    :household:
:PROPERTIES:
:TAG_ID: weight
:UNIT: kg
:RANGE: 1-10
:HEALTHY_RANGE: 2.8-5.8
:END:

*** Water Intake    :hydration:
:PROPERTIES:
:TAG_ID: water
:UNIT: ml
:RANGE: 0-500
:HEALTHY_RANGE: 140-320
:END:

*** Activity Minutes    :play:
:PROPERTIES:
:TAG_ID: active
:UNIT: min
:RANGE: 0-240
:HEALTHY_RANGE: 20-90
:END:

* Journals
** Shared Routine
*** Entry    :ATTACH:
:PROPERTIES:
:ID: ddd8aa8a-9d9a-4d22-a3b8-900500000001
:DATETIME: <2026-01-02 08:15>
:PRIVATE: nil
:END:

Both cats used new fountain #water(280) #active(35)
Morning photo [[attachment:fountain-morning.jpg]]

*** Entry
:PROPERTIES:
:ID: ddd8aa8a-9d9a-4d22-a3b8-900500000002
:DATETIME: <2026-01-06 21:00>
:PRIVATE: nil
:END:

Evening play with feather toy #active(42)

*** Entry    :ATTACH:
:PROPERTIES:
:ID: ddd8aa8a-9d9a-4d22-a3b8-900500000003
:DATETIME: <2026-01-09 09:40>
:PRIVATE: nil
:END:

Weight check after grooming #weight(4.9)
Scale snapshot [[attachment:scale-reading.png]]

* Documents
** Dental Cleaning Advice    :ATTACH:dental:
:PROPERTIES:
:SOURCE: Midtown Animal Care
:FILES: dental-note.pdf, tooth-chart.jpg
:ID: eee1e15a-4ccc-4e23-9c00-900500000001
:DATETIME: <2026-01-09 12:30>
:PRIVATE: nil
:END:

Recommend brushing 3 times a week.

** Food Rotation Plan    :ATTACH:
:PROPERTIES:
:SOURCE: Midtown Animal Care
:FILES: food-rotation-plan.pdf
:ID: eee1e15a-4ccc-4e23-9c00-900500000002
:DATETIME: <2026-01-10 10:00>
:PRIVATE: nil
:END:

Switch proteins every two weeks.

* Reminders
** TODO Replace Water Filter    :hydration:
SCHEDULED: <2026-02-01 +1m>
:PROPERTIES:
:ID: fff5b1e5-3b6f-4d7e-8461-900500000001
:END:
Replace fountain filter and sanitize tank.

** TODO Dental Brushing Cycle    :dental:
SCHEDULED: <2026-01-20 +1w>
:PROPERTIES:
:ID: fff5b1e5-3b6f-4d7e-8461-900500000002
:END:
Track compliance for both cats.
")
   (cons "bruno-health"
":PROPERTIES:
:ID: d8f2c0ad-6a2c-4f97-9ad0-0d9444440004
:HARP_NAME: Bruno Oliveira
:HARP_TYPE: dog
:END:
#+TITLE: Bruno Dog Health Record

* Metadata
** Sources

** Metrics
*** Weight    :canine:
:PROPERTIES:
:TAG_ID: weight
:UNIT: kg
:RANGE: 2-60
:HEALTHY_RANGE: 18-32
:END:

*** Resting Pulse    :cardio:
:PROPERTIES:
:TAG_ID: pulse
:UNIT: bpm
:RANGE: 40-220
:HEALTHY_RANGE: 60-120
:END:

*** Scratch Severity    :skin:
:PROPERTIES:
:TAG_ID: itch
:UNIT: score
:RANGE: 0-10
:HEALTHY_RANGE: 0-3
:END:

* Journals
** Walks
*** Entry    :ATTACH:
:PROPERTIES:
:ID: aaf9ee2a-9ed8-4f3b-8db9-900400000001
:DATETIME: <2026-01-04 06:55>
:PRIVATE: nil
:END:

Energetic walk with no limping #weight(24.5) #pulse(88)
Trail photo [[attachment:park-run.jpg]]

*** Entry
:PROPERTIES:
:ID: aaf9ee2a-9ed8-4f3b-8db9-900400000002
:DATETIME: <2026-01-07 19:35>
:PRIVATE: nil
:END:

Mild itching today #itch(4)

** Treatment
*** Entry    :ATTACH:
:PROPERTIES:
:ID: aaf9ee2a-9ed8-4f3b-8db9-900400000003
:DATETIME: <2026-01-08 20:10>
:PRIVATE: nil
:END:

Topical ointment applied [[attachment:ointment-box.png]] #itch(2)

* Documents
** Skin Panel Results    :ATTACH:lab:
:PROPERTIES:
:SOURCE: Rio Pet Diagnostics
:FILES: skin-panel.pdf, allergen-chart.png
:ID: bb2243e1-24d2-4f57-a7ad-900400000001
:DATETIME: <2026-01-08 12:45>
:PRIVATE: nil
:END:

No critical allergy markers.

** Vaccination Passport    :ATTACH:
:PROPERTIES:
:SOURCE: Rio Pet Clinic
:FILES: passport-page-1.jpg, passport-page-2.jpg
:ID: bb2243e1-24d2-4f57-a7ad-900400000002
:DATETIME: <2026-01-01 10:15>
:PRIVATE: nil
:END:

Rabies and DHPP current.

* Reminders
** TODO Tick Prevention    :preventive:
SCHEDULED: <2026-02-05 +1m>
:PROPERTIES:
:ID: cc9c9f70-6f4f-4dc7-8ac4-900400000001
:END:
Apply spot-on treatment.

** TODO Grooming Session    :grooming:
DEADLINE: <2026-02-20>
:PROPERTIES:
:ID: cc9c9f70-6f4f-4dc7-8ac4-900400000002
:END:
Book grooming with regular partner.
")
   )
  "Sample profiles as (SLUG . ORG-TEXT), written by `harp-demo-setup'.")

(defun harp-demo--file (slug)
  (expand-file-name (concat slug ".org") harp-directory))

(defun harp-demo--exists-p ()
  "Non-nil when any bundled sample profile is already on disk."
  (cl-some (lambda (p) (file-exists-p (harp-demo--file (car p)))) harp-demo--profiles))

(defun harp-demo-setup (&optional force)
  "Write the bundled sample profiles into `harp-directory'; open the first.
Existing files are left alone unless FORCE (or a prefix arg) is non-nil, so a
re-run refreshes the samples to the latest bundled version.  Returns the
opened profile file."
  (interactive "P")
  (make-directory harp-directory t)
  (let (first)
    (dolist (p harp-demo--profiles)
      (let ((file (harp-demo--file (car p))))
        (when (or force (not (file-exists-p file)))
          (with-temp-file file (insert (cdr p)))
          ;; `with-temp-file' writes disk directly; drop any stale buffer
          ;; still visiting this path so the next read reflects the refresh.
          (when-let ((buf (find-buffer-visiting file)))
            (with-current-buffer buf (set-buffer-modified-p nil))
            (kill-buffer buf)))
        (unless first (setq first file))))
    (harp-ui-invalidate)
    (when first (harp-ui-set-current first))
    first))

(with-jetpacs-owner "harp"
  (jetpacs-defaction "harp.demo.setup"
    (lambda (_args _)
      ;; When samples already exist, offer to refresh them to the latest
      ;; bundled version (bridged to a dialog on the phone).  Declining must
      ;; leave the current profile alone — never switch or claim a write.
      (cond
       ((not (harp-demo--exists-p))
        (harp-demo-setup)
        (jetpacs-shell-notify "Sample profiles added")
        (jetpacs-shell-set-current-tab "harp.journal"))
       ((yes-or-no-p "Sample profiles already exist — refresh to the latest samples? ")
        (harp-demo-setup t)
        (jetpacs-shell-notify "Sample profiles refreshed")
        (jetpacs-shell-set-current-tab "harp.journal"))
       (t
        (jetpacs-shell-notify "Kept your existing profiles")
        (jetpacs-shell-push))))
    :doc "Write (or refresh) the bundled sample profiles and open the first."))

(provide 'harp-demo)
;;; harp-demo.el ends here
