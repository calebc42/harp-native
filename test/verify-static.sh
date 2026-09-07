#!/usr/bin/env bash
set -euo pipefail
harp_root=$(cd -- "$(dirname "$0")/.." && pwd)
repositories_root=${JETPACS_REPOSITORIES_ROOT:-$(dirname "$harp_root")}
export JETPACS_ROOT=${JETPACS_ROOT:-$repositories_root/jetpacs-poc}
export HARP_SOURCE_ROOT="$harp_root"
export HARP_VERIFY_OUTPUT=${1:-$(mktemp -d /tmp/harp-verification.XXXXXX)}
mkdir -p "$HARP_VERIFY_OUTPUT"
emacs -Q --batch -L "$JETPACS_ROOT/jetpacs-applet-mcp" -l "$harp_root/test/verify-static.el"
rg -q 'Status: PASS \(0 errors, 0 warnings\)' "$HARP_VERIFY_OUTPUT/jetpacs-applet-tooling-validate-applet.txt"
printf 'Static Harp reports: %s\n' "$HARP_VERIFY_OUTPUT"
