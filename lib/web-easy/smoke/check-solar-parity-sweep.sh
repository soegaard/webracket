#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "[1/8] compile solar showcase"
"$SCRIPT_DIR/run-browser-solar-showcase-compile.sh"

echo "[2/8] accordion parity"
"$SCRIPT_DIR/check-solar-accordion-parity.sh"

echo "[3/8] navbar parity"
"$SCRIPT_DIR/check-solar-navbar-parity.sh"

echo "[4/8] forms plaintext contract"
"$SCRIPT_DIR/check-solar-forms-plaintext-contract.sh"

echo "[5/8] forms visual/computed diff"
node "$SCRIPT_DIR/check-solar-forms-diff.mjs"

echo "[6/8] table computed contract"
node "$SCRIPT_DIR/check-solar-table-computed.mjs"

echo "[7/8] list-group zoom + alignment contract"
node "$SCRIPT_DIR/check-solar-listgroup-zoom-contract.mjs"

echo "[8/8] progress computed contract"
node "$SCRIPT_DIR/check-solar-progress-computed.mjs"

