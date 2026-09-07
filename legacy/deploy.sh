#!/usr/bin/env bash
#
# Deploy the Harp bundle (and optionally the companion APK) to the connected
# Android device. Rebuilds harp.el from emacs/apps/harp/*.el via Emacs, then
# adb-pushes it (and harp-pack.json) to /sdcard/Documents/jetpacs/. Termux is
# not debuggable, so adb cannot write into /data/data/com.termux directly —
# the jetpacs init adopts a newer staged bundle from that slot on startup.
#
# The bundle `require`s jetpacs-core — pass --core to also deploy the vendored
# jetpacs/jetpacs-core.el alongside it (first deploy, or after a jetpacs update).
#
# Usage:
#   ./deploy.sh                 # rebuild + stage to /sdcard/Documents/jetpacs
#   ./deploy.sh --core          # also stage jetpacs-core.el
#   ./deploy.sh --ssh           # rebuild + scp straight into Termux's ~/.emacs.d/elisp
#   ./deploy.sh --apk           # also build + install the companion APK
#
# --ssh one-time setup inside Termux:
#   pkg install openssh && passwd && sshd

set -euo pipefail

use_ssh=0
use_apk=0
use_core=0
for arg in "$@"; do
  case "$arg" in
    --ssh) use_ssh=1 ;;
    --apk) use_apk=1 ;;
    --core) use_core=1 ;;
    -h|--help) grep '^#' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) echo "Unknown option: $arg" >&2; exit 2 ;;
  esac
done

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
bundle="$repo/harp.el"
pack="$repo/harp-pack.json"
core="$repo/jetpacs/jetpacs-core.el"

adb=adb
command -v adb >/dev/null 2>&1 || adb=adb.exe

echo '-- Rebuilding harp.el from emacs/apps/harp/*.el ...'
# harp-demo.el is generated from test/fixtures/*.org and bundled into harp.el,
# so regenerate it FIRST or the bundle ships a stale demo corpus.
emacs --batch -l "$repo/emacs/build-demo.el"
emacs --batch -l "$repo/emacs/build-bundle.el"
emacs --batch -l "$repo/emacs/build-pack.el"

echo '-- Checking device ...'
"$adb" get-state >/dev/null

if [ "$use_ssh" -eq 1 ]; then
  echo '-- Pushing directly into Termux home via sshd (port 8022) ...'
  "$adb" forward tcp:8022 tcp:8022 >/dev/null
  ssh -p 8022 termux@127.0.0.1 'mkdir -p .emacs.d/elisp'
  scp -P 8022 "$bundle" termux@127.0.0.1:.emacs.d/elisp/harp.el
  scp -P 8022 "$pack" termux@127.0.0.1:.emacs.d/elisp/harp-pack.json
  if [ "$use_core" -eq 1 ]; then
    scp -P 8022 "$core" termux@127.0.0.1:.emacs.d/elisp/jetpacs-core.el
  fi
  echo '   Installed to ~/.emacs.d/elisp/ - reload or restart Emacs.'
else
  stage=/sdcard/Documents/jetpacs
  echo "-- Staging to $stage (adopted by the jetpacs init on Emacs restart) ..."
  "$adb" shell mkdir -p "$stage"
  "$adb" push "$bundle" "$stage/harp.el"
  "$adb" push "$pack" "$stage/harp-pack.json"
  if [ "$use_core" -eq 1 ]; then
    "$adb" push "$core" "$stage/jetpacs-core.el"
  fi
  echo '   Staged. Restart Emacs on the device to pick it up.'
fi

if [ "$use_apk" -eq 1 ]; then
  echo '-- Building + installing the jetpacs companion APK ...'
  "$repo/jetpacs/gradlew" -p "$repo/jetpacs" installDebug
fi

echo 'Deploy complete.'
