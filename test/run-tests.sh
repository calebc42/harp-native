#!/usr/bin/env bash
# Run the Harp ERT suite against the vendored jetpacs core, then confirm the
# shipped bundle byte-compiles standalone against the core bundle alone.
set -euo pipefail
cd "$(dirname "$0")/.."

EMACS="${EMACS:-emacs}"

"$EMACS" --batch \
  -L jetpacs/emacs/core \
  -L emacs/apps/harp \
  -L test \
  -l harp-tests \
  -f ert-run-tests-batch-and-exit </dev/null

# The foundation byte-compiles an adopted bundle BEFORE loading it, when none
# of the bundle's own features exist on the load-path — a surviving
# bundle-internal hard require is a compile error and a broken .elc on device.
# Compile the shipped bundle against the core bundle alone (exactly the device
# situation) to pin that.
if [ -f harp.el ] && [ -f jetpacs/jetpacs-core.el ]; then
  tmp=$(mktemp -d)
  trap 'rm -rf "$tmp"' EXIT
  cp jetpacs/jetpacs-core.el harp.el "$tmp"/
  "$EMACS" -Q --batch -L "$tmp" --eval "(progn (require 'bytecomp)
    (let ((byte-compile-warnings nil))
      (unless (byte-compile-file \"$tmp/harp.el\")
        (kill-emacs 1))))"
  echo "bundle byte-compiles standalone: OK"
fi
