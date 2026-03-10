#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
CSS="$SCRIPT_DIR/theme-solar-2.css"

echo "[1/1] plaintext selector scope contract"
if rg -n "showcase-main .*we-form-control-plaintext|we-form-control-plaintext.*showcase-main" "$CSS" >/dev/null; then
  echo "FAIL: plaintext selectors must not be scoped by showcase-main"
  exit 1
fi

if ! rg -n "we-form-control-plaintext" "$CSS" >/dev/null; then
  echo "FAIL: missing we-form-control-plaintext selectors"
  exit 1
fi

echo "PASS"
echo "plaintext form control is theme-level (not showcase-scoped)"

