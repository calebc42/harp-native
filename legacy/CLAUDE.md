# Harp (harp-native) — working notes

Harp is a **Jetpacs Tier-1 app**: pure elisp on the `jetpacs-core` foundation
(the `jetpacs/` git submodule); the Android companion is a thin renderer and
Emacs + org-mode is the source of truth. No Kotlin lives in this repo.

## Build & test

```sh
git submodule update --init
./test/run-tests.sh                        # ERT vs the submodule core + standalone byte-compile of the bundle
emacs --batch -l emacs/build-demo.el       # regenerate emacs/apps/harp/harp-demo.el (from test/fixtures/*.org)
emacs --batch -l emacs/build-bundle.el     # regenerate harp.el
emacs --batch -l emacs/build-pack.el       # regenerate harp-pack.json (from live action registrations)
./deploy.sh [--core] [--ssh] [--apk]       # rebuild + push to a device
```

**Generated files — never hand-edit:** `harp.el`, `harp-pack.json`,
`emacs/apps/harp/harp-demo.el`. CI regenerates all three and fails on a diff,
so any PR touching `emacs/` sources or `test/fixtures/*.org` must ship them
regenerated. New `harp-*.el` modules must be added to the `app-files` list in
`emacs/build-bundle.el` (dependency order; `harp.el` entry last) and required
from `emacs/apps/harp/harp.el`.

## Conventions

- Every registration (views, actions, settings, chrome, forms) goes inside
  `(with-jetpacs-owner "harp")`; views are `harp.<name>`, actions `harp.<verb>`.
- Actions are the wire's security boundary: validate args, never run
  code/paths off the wire. Queued **mutations** need `:when-offline "queue"`
  **and a `:dedupe` key** (see `harp-med-dose-dedupe-key`); navigation uses
  `"drop"`.
- Views memoise through `harp-ui-current` (keyed on file mtime); every
  mutation path must end in `harp-write--save` (which invalidates) or call
  `harp-ui-invalidate` explicitly.
- Headless verification: `jetpacs-lint-spec` (nil = wire-valid) and
  `jetpacs-render-to-json` (the exact serialize round-trip a push does). Gated
  nodes (chart, month_grid) need `jetpacs-node-supported-p` stubbed in batch —
  see `harp-tests--with-profile`.

## Gotchas (each cost a debugging session)

- **Navigating to an overlay/detail view requires
  `(jetpacs-shell-push nil :switch-to "view")`** — a plain push only updates
  content and the phone stays where it is. Symptom: action fires, "nothing
  happens".
- `org-entry-get` returns **nil for a literal `nil` property value** (org
  treats it as unset) — write `no` for booleans (see `harp--prop-bool`).
- After writing a file **externally** (`with-temp-file` over an existing
  path), kill any buffer still visiting it or the next
  `find-file-noselect` read returns stale content. In-app writes go through
  the visiting buffer + save and are fine.
- `split-string` (and most string fns) clobber regex **match data** — bind
  `(match-string N)` results *before* calling them.
- The ERT suite must not mutate `test/fixtures/` — use
  `harp-tests--with-temp-profile`, which copies to a temp dir and kills
  visiting buffers afterwards.

## Data model (one profile = one .org file)

File-level `:ID:`/`:HARP_TYPE:`/`:HARP_NAME:`+`#+TITLE:`; top-level sections
`Metadata` (Metrics: `:TAG_ID: :UNIT: :RANGE: lo-hi :HEALTHY_RANGE:`),
`Journals` (`** Name` → `*** Entry` with `:ID: :DATETIME: :PRIVATE:`),
`Documents` (`:FILES: :SOURCE:`), `Reminders` (TODO + SCHEDULED/DEADLINE),
`Medications` (`** Name` with `:UNIT: :STOCK: :REFILL: :LOW_STOCK: :EXPIRES:
:ACTIVE:`, `*** Schedule` children, and a `:LOGBOOK:` of `- taken|skipped
[ts] AMOUNT` dose lines). Inline body tokens: `#id(value)` = metric reading,
`#tag` = hashtag. Attachments use org-attach's `<id[0:2]>/<id[2:]>/` layout.
