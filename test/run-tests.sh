#!/usr/bin/env bash
set -euo pipefail
harp_root=$(cd -- "$(dirname "$0")/.." && pwd)
repositories_root=${JETPACS_REPOSITORIES_ROOT:-$(dirname "$harp_root")}
jetpacs_root=${JETPACS_ROOT:-$repositories_root/jetpacs-poc}
paths=(-L "$repositories_root/ebp.el/lisp" -L "$repositories_root/ebp-org/lisp" -L "$jetpacs_root/emacs"
       -L "$jetpacs_root/jetpacs-components/lisp/jetpacs-components"
       -L "$harp_root/emacs/apps/harp" -L "$harp_root/test")
"${EMACS:-emacs}" -Q --batch "${paths[@]}" --eval '(setq load-prefer-newer t)' \
  -l harp-current-test \
  -f ert-run-tests-batch-and-exit
compile_dir=$(mktemp -d /tmp/harp-byte-compile.XXXXXX)
trap 'rm -rf "$compile_dir"' EXIT
HARP_SOURCE_ROOT="$harp_root" "${EMACS:-emacs}" -Q --batch "${paths[@]}" \
  --eval '(setq load-prefer-newer t)' -l harp \
  --eval '(progn (require (quote checkdoc)) (let ((files (directory-files (concat (getenv "HARP_SOURCE_ROOT") "/emacs/apps/harp") t (rx ".el" eos)))) (unless files (error "No source files for Checkdoc")) (dolist (file files) (checkdoc-file file))))' \
  > "$compile_dir/checkdoc.log" 2>&1
if test -s "$compile_dir/checkdoc.log"; then
  cat "$compile_dir/checkdoc.log"
  exit 1
fi
HARP_COMPILE_DIR="$compile_dir" "${EMACS:-emacs}" -Q --batch "${paths[@]}" \
  --eval '(setq load-prefer-newer t byte-compile-error-on-warn t byte-compile-dest-file-function (lambda (file) (expand-file-name (concat (file-name-nondirectory file) "c") (getenv "HARP_COMPILE_DIR"))))' \
  -f batch-byte-compile "$harp_root"/emacs/apps/harp/*.el
