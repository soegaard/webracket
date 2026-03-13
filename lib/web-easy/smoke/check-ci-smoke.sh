#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/../../.." && pwd)"

echo "[0/2] keyword negative tests"
(cd "$ROOT_DIR/lib/web-easy/test" && ./test-define-key-negative.sh)

echo "[1/2] headless doctor"
"$SCRIPT_DIR/headless.sh" doctor
echo "[2/2] headless ci"
"$SCRIPT_DIR/headless.sh" ci

echo
echo "ci smoke checks passed"
