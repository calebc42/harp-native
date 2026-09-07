# Harp Native implementation guide

Harp is a trusted, optional Elisp applet using the sibling jetpacs-poc repository and its
jetpacs-components module. Read the POC applet guide and determinism
contract. Keep health data, the canonical-Org schema adapter and scheduling policy here; reusable
Foundation rendering belongs in jetpacs-components. Builders only read the
published snapshot. Wire actions use revision-bound opaque references, never
paths or Org positions. Commit writes before accepted; defer only presentation.

Use synthetic fixtures in temporary directories. Never modify harp-kmp or load
its profiles automatically. legacy/ preserves the historical implementation and
is not on the runtime load path. Run test/run-tests.sh and test/verify-applet.sh.
