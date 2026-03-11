#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "[1/9] compile solar showcase"
"$SCRIPT_DIR/run-browser-solar-showcase-compile.sh"

echo "[2/9] accordion parity"
"$SCRIPT_DIR/check-solar-accordion-parity.sh"

echo "[3/9] navbar parity"
"$SCRIPT_DIR/check-solar-navbar-parity.sh"

echo "[4/9] forms plaintext contract"
"$SCRIPT_DIR/check-solar-forms-plaintext-contract.sh"

echo "[5/9] forms visual/computed diff"
node "$SCRIPT_DIR/check-solar-forms-diff.mjs"

echo "[6/9] table computed contract"
node "$SCRIPT_DIR/check-solar-table-computed.mjs"

echo "[7/9] list-group computed contract"
node "$SCRIPT_DIR/check-solar-list-group-contract.mjs"

echo "[8/9] list-group zoom + alignment contract"
node "$SCRIPT_DIR/check-solar-listgroup-zoom-contract.mjs"

echo "[9/9] progress computed contract"
node "$SCRIPT_DIR/check-solar-progress-computed.mjs"
