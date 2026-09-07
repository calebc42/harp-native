# Harp

A privacy-centric, on-device **Personal Health Record** app — journals, health
metrics, medical documents, and reminders in plain Org files you fully control.

This is the **Jetpacs Tier-1 rebuild** of
[harp-kmp](https://github.com/lepisma/harp): instead of a standalone Kotlin app
that hand-parses Org, Harp here is **pure elisp** running in Emacs (the source of
truth) with the generic Jetpacs Android companion as a thin renderer. All Kotlin
lives in the [jetpacs](jetpacs/) core, vendored as a git submodule; this repo is
elisp only. It follows the shape of the
[Glasspane](https://github.com/calebc42/glasspane) reference app.

## Features

- **Profiles** — each profile is one Org file (human, cat, or dog); the Home
  tab lists them, tap to open.
- **Journals** — multiple journals per profile, entries newest-first with
  inline `#id(value)` metric readings and `#tags`.
- **Documents** — medical documents with source, annotation, and attachment
  file names.
- **Metrics** — definitions with units and healthy ranges, plotted as charts
  from the readings harvested across entries and documents.
- **Reminders** — scheduled/deadline TODOs over a month calendar, armed as
  exact OS alarms on the companion (repeater-aware, reboot-surviving).
- **Medications** (MedTimer-inspired) — meds with dose, stock, and expiry; a
  flexible scheduler (daily times, weekdays, intervals, cyclic active/pause
  cycles, tapering periods); a "Due today" list where you mark each dose
  **Taken** or **Skipped** (logged to the med's org LOGBOOK, stock decremented
  on Taken); low-stock and expiry warnings; scheduled dose alarms; and **due
  doses raise an interactive notification with Taken / Skip / Snooze buttons**
  you can act on without opening the app.
- **Capture & edit** — forms for entries, documents, metrics, medications, and
  new profiles; a per-tab FAB and tap-a-card-to-edit.
- **Tag filtering** and an optional **hide-private** setting.
- **Export** — share a profile as Org text (over the bridge, no file transfer)
  or save an HTML timeline.

## Layout

- `harp.el` — the single-file bundle, generated (do not edit by hand)
- `harp-pack.json` — the engine-pack manifest, generated from live registrations
- `emacs/apps/harp/harp-*.el` — the app:
  - `harp-profile` / `harp-metrics-parse` — the Org schema read layer
  - `harp-ui` — shared state and spec helpers
  - `harp-home` / `harp-journal` / `harp-documents` / `harp-metrics` /
    `harp-reminders` — the tabs
  - `harp-med` / `harp-med-schedule` / `harp-med-log` / `harp-med-view` /
    `harp-med-form` / `harp-med-notify` — medications (domain, scheduler, dose
    logging, tab, form, interactive due-dose notifications)
  - `harp-write` / `harp-form` — capture and edit
  - `harp-export` — export & share; `harp-settings` — settings; `harp-pack` —
    the manifest
- `harp-demo` — the bundled sample profiles, generated from `test/fixtures/*.org`
- `emacs/build-demo.el` / `emacs/build-bundle.el` / `emacs/build-pack.el` —
  regenerate `harp-demo.el` / `harp.el` / `harp-pack.json` (all build against
  `jetpacs-core`)
- `jetpacs/` — the Jetpacs core (foundation + companion), a git submodule
- `test/harp-tests.el` — the ERT suite, run against the submodule core
- `deploy.sh` — rebuild + push the bundle to a connected device

## Data model (Org schema)

One **profile = one `.org` file**: `:ID:`, `:HARP_TYPE: human|cat|dog`,
`#+TITLE:`/`:HARP_NAME:`, and sections `Metadata` (Sources, Metrics), `Journals`
(`** Name` holding `*** Entry` subtrees), `Documents`, `Reminders`, and
`Medications`. Metric values are `#id(value)` tokens harvested from
entry/document bodies; attachments use `org-attach` (whose default
`<id[0:2]>/<id[2:]>/` layout is Harp's `data/` scheme).

A **medication** is `** Name` under `Medications` with properties `:UNIT:`,
`:STOCK:`, `:REFILL:`, `:LOW_STOCK:`, `:EXPIRES:`, `:ACTIVE:`; one or more
`*** Schedule` children (`:AMOUNT:` plus `:TIMES:`/`:INTERVAL:`, filtered by
`:DAYS:`/`:CYCLE_START:`/`:FROM:`/`:UNTIL:`); and a `:LOGBOOK:` of
`- taken|skipped [timestamp] AMOUNT` dose lines that the "Due today" flow
appends to.

## Build & test

```sh
git submodule update --init
emacs --batch -l emacs/build-demo.el       # regenerate harp-demo.el (from test/fixtures/*.org)
emacs --batch -l emacs/build-bundle.el     # regenerate harp.el
emacs --batch -l emacs/build-pack.el       # regenerate harp-pack.json
./test/run-tests.sh                        # ERT against the submodule core
```

CI runs the same steps and fails if `harp-demo.el`, `harp.el`, or
`harp-pack.json` is stale — a PR that touches `emacs/` sources or
`test/fixtures/*.org` must ship them regenerated.

## Install

Load order matters — the core first:

```elisp
(require 'jetpacs-core)   ; from the jetpacs repo / submodule
(require 'harp)           ; from this repo
```

Profiles are discovered under `harp-directory` (default `~/harp/`). On a device,
`./deploy.sh --core` stages the core and the Harp bundle over adb.

## Status

Feature-complete against harp-kmp's core surface, verified headlessly (99 ERT
tests: schema round-trips, view wire-validity, capture/edit/export round-trips,
reminder and medication scheduling with DST correctness, offline-queue replay
integrity, and the wire-security allowlists). Two deferred pieces, both blocked
on the same companion→Emacs file-transfer primitive the base protocol lacks:

- **OCR** (harp-kmp's Tesseract document scan) — documents attach file *names*
  for now, entered as text.
- **Media embedding in HTML export** (the zip) — the timeline exports without
  bundled media.

PDF/FHIR export remain unsupported (as in harp-kmp).
