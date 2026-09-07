# Restoration ledger

Source: archived `harp-native`, branch
`claude/harp-kmp-jetpacks-rebuild-00aed7`, commit `8396d22`.
The archive default branch contained only the scaffold. The restored `main`
starts at the implementation commit; no history was rewritten or pushed.
Reference: `../emacs/harp-kmp`, inspected as source only. Its existing dirty
profile file was preserved and not used for fixtures.

## Ownership and invariants

`harp-profile.el` maps canonical Org mode API results into Harp records.
`harp-metrics-parse.el` handles Harp's `#id(value)` convention, not Org parsing.
`harp-state.el` owns bounded disk reads, revision hashes, opaque references and
an explicit clock. Builders only read that snapshot. No raw file paths or Org
positions are emitted as action arguments. Mutations validate captured fields,
check revisions, save synchronously and only then return accepted. Presentation
failures after save cannot convert a committed edit into rejection.

Discovery is limited to 512 directory entries, 32 profiles, 1 MiB per file and
2000 headings. Screens paginate at 20 records; each metric charts its latest
60 readings. Dose generation is bounded to 200 daily doses. Symlinks escaping
the configured root and unsaved visiting buffers are refused. New process
epochs prevent stale token reuse after restart. Edits and exports use the
Emacs-owned configured directory. Actions currently use drop while offline;
there is no claim of restored queued medication actions.

The independent Foundation addition is a passive canonical chart override:
line, bar, area and sparkline use Canvas and profile colors. Charts with
`on_point_tap` retain the host renderer because its injection seam supplies the
mandatory ordinal index and complete point payload. No EBP or extension
vocabulary was changed. Foundation does not refer to Harp.

## Remaining restoration work

Trusted execution was explicitly authorized on 2026-09-06. The current applet
passes its isolated synthetic-record suite, warning-as-error compilation,
Checkdoc and offline determinism/identity gates. Native renderer flows were
also exercised on the connected tablet; see VERIFICATION.md for exact scope.

Historical alarm synchronization, interactive dose notifications and snooze
need migration to current capability/durability APIs and device verification.
They are preserved under legacy, not silently enabled. OCR/camera import and
embedded attachment export were already deferred historically. Reminder edits,
journal creation/movement, selection export and broader harp-kmp parity require
follow-up implementation; this restoration does not claim full parity.

## Verification performed

- Native-reader static inspect, manifest and validation: 11 files, 15 actions,
  zero errors/warnings; one dynamic action-descriptor helper.
- 22 synthetic ERT tests, Checkdoc and warning-as-error byte compilation.
- Five tablet instrumentation tests; three package-entry tests.
- Foundation Kotlin unit suite including three new chart-contract tests.
- Foundation Kotlin: 80 tests passed; Companion Kotlin: 66 tests passed.
- Companion debug APK build passed.

The precise runtime envelope and native-rendering evidence are in VERIFICATION.md.
