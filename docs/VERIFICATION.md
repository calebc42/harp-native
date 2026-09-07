# Harp verification — 2026-09-06

The user explicitly authorized trusted Harp execution. All applet runtime
checks used isolated temporary directories containing synthetic records.
The reference harp-kmp profiles were not loaded or changed.

| Check | Result |
|---|---|
| `test/run-tests.sh` | 22 ERT tests passed; Checkdoc and warning-as-error compilation passed |
| `test/verify-static.sh` | 11 files, 15 actions, zero errors or warnings |
| `test/verify-applet.sh` | STABLE; real offline identity/variant gates passed |
| Foundation Kotlin unit suite | 80 passed, including three chart projection tests |
| Companion Kotlin unit suite | 66 passed |
| POC package-entry ERT | 3 passed; Harp metadata exists without loading or activating Harp |
| Debug app and Android test APKs | Build passed; all 11 staged Harp source modules match |
| `HarpPresentedScreenTest` | 5 passed on Pixel Tablet, Android 17, at a 400 dp test viewport |

## Runtime envelope

The trusted root check used a fixed synthetic snapshot clock, 2026-09-06 at
12:00 in the test process timezone, and a fixed test-only reference epoch.
It ran with offline compact/medium fallback context and no negotiated profile.
Both canonical hashes were
`7753a0d1e00a967d5d3e4ec6a8db85649c7cbab3d3a41fd1723e850702fb16db`.
The root contained 32 nodes, depth 7, and 7007 canonical bytes. ERT separately
builds every destination twice with clock/file reads prohibited, checks that
the snapshot is unchanged, and exercises every form's real identity gate.

The tablet tests use actual Elisp-produced JSON and the Companion renderer
installation with an inert recording action host. They cover opaque profile
selection, native text editing and captured field IDs, accessible light/dark
metric charts, and dose action identity without device-authored amounts.
They do not establish live bridge transport, reconnect/offline replay, alarm
delivery, or a full device-to-Emacs save round trip. Synchronous saves, failed
save rollback, stale rejection, dose stock changes, schema edits and export
privacy are exercised separately by the real Emacs handlers in ERT.

## Fixes found during execution

- Corrected form identity construction and publication of record tokens.
- Fixed optional card child serialization and recovered export formatting.
- Bound id-less metric resolution through the actual `ebp-org-roots` seam.
- Kept medication markers across atomic edits, preserving unmodeled properties.
- Corrected metric uniqueness validation, selected navigation state and plot ranges.
- Added capture coverage for all record kinds, refill/skip effects, reminder
  toggling/deletion, profile creation and export discovery exclusion.

## Screenshots

- [Profiles](screenshots/profiles-light.png)
- [Edited form, scrolled to Save](screenshots/entry-form-light.png)
- [Metric chart, light](screenshots/metrics-light.png)
- [Metric chart, dark](screenshots/metrics-dark.png)
- [Medication actions](screenshots/medications-light.png)

Temporary packages `com.calebc42.jetpacs.companion` and its `.test` package
were absent before testing and removed afterward. The existing `dev.jetpacs`
installation was preserved. No sources were compiled to `.elc` in the applet
source directory. No commits or pushes were made.
