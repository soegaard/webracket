#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

FILES=(
  "$SCRIPT_DIR/test-browser-theme-token-contract.html"
  "$SCRIPT_DIR/test-browser-theme-vars.html"
  "$SCRIPT_DIR/test-browser-theme-dialog-vars.html"
  "$SCRIPT_DIR/test-browser-theme-menu-vars.html"
  "$SCRIPT_DIR/test-browser-theme-progress-vars.html"
  "$SCRIPT_DIR/test-browser-parity-theme-vars.html"
  "$SCRIPT_DIR/test-browser-parity-theme-dialog-vars.html"
  "$SCRIPT_DIR/test-browser-parity-theme-menu-vars.html"
  "$SCRIPT_DIR/test-browser-parity-theme-progress-vars.html"
)

FAIL=0

for f in "${FILES[@]}"; do
  if ! rg -q '<script src="./theme-contract-helper.js"></script>' "$f"; then
    echo "helper-lint: missing theme-contract-helper import in $(basename "$f")"
    FAIL=1
  fi

  if rg -q 'function (sleep|frameDocFrom|normalizedCssColor|assertTrue|ensureThemeLayer)\(' "$f"; then
    echo "helper-lint: duplicated helper function found in $(basename "$f")"
    FAIL=1
  fi
done

if [ "$FAIL" -ne 0 ]; then
  exit 1
fi

echo "helper-lint: ok (theme contract pages use shared helper)"
