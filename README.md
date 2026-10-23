# Harp Native

The restored Jetpacs Harp applet, rebuilt on the
current Foundation components and Styles API. The Org data adapter calls
canonical Org mode APIs; Harp interprets its own profile schema, metric tokens
and medication scheduling conventions, not Org syntax independently.

Foundation components live in `jetpacs-poc/jetpacs-components/`.
The repository is a sibling of `jetpacs-poc`, `ebp.el`
and `ebp-org`. Historical commit `8396d22` and the original branches are
preserved. `legacy/` contains the retired UI, generated bundles and integration
code. There is no nested Jetpacs submodule and no configured GitHub remote.

## Implemented workflows

- Profiles; journal/document capture and editing; tag and privacy filters.
- Metric definitions, accessible ordinal charts, and recent reading labels.
- Medication forms, schedules, exact-time dose logging and stock/refill actions.
- Reminder calendar/list, creation and Org TODO toggling.
- Public journal/document export as Org or HTML, saved below `harp-directory`.
- Light/dark Foundation styles; optional packaged-app entry, inactive by default.

`harp-directory` defaults to `~/harp/`. Loading the entrypoint registers
nothing; the `harp` command (or the app manager) registers the UI and
explicitly reads that configured directory. The
`harp-kmp` checkout and its profiles are never loaded automatically.

The implemented workflows have been verified with synthetic records in Emacs
and in the Companion's tablet renderer. See [RESTORATION.md](docs/RESTORATION.md)
for the remaining historical features and [VERIFICATION.md](docs/VERIFICATION.md)
for the exact test scope and screenshots.

## Verification

`test/verify-static.sh` reads source without loading the applet.
For this trusted applet, `test/run-tests.sh` exercises temporary,
synthetic records, Checkdoc and warning-as-error compilation outside source
folders. `test/verify-applet.sh` adds the real offline determinism/gate check
and exports synthetic native-screen fixtures. The Android instrumentation test is `HarpPresentedScreenTest`.

## Open in an existing trusted Jetpacs session

Add this repository's `emacs/apps/harp` to `load-path`, with the sibling
Foundation and EBP Org libraries available. Set `harp-directory` to the local
profile directory you intend to use, then run `(require 'harp)` and `M-x harp`.
The packaged app can also be enabled through Jetpacs' app manager.
