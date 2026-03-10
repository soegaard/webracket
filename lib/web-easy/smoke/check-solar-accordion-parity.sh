#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "[1/4] compile solar showcase"
"$SCRIPT_DIR/run-browser-solar-showcase-compile.sh"

echo "[2/4] accordion strict computed contract"
node "$SCRIPT_DIR/check-solar-accordion-contract.mjs"

echo "[3/4] accordion computed-style diff (generated vs reference)"
node "$SCRIPT_DIR/check-solar-accordion-computed-diff.mjs"

echo "[4/4] accordion screenshot diff (generated vs reference)"
node "$SCRIPT_DIR/check-solar-accordion-screenshot-diff.mjs"
