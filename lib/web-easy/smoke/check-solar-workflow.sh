#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "[1/6] compile solar showcase"
"$SCRIPT_DIR/run-browser-solar-showcase-compile.sh"

echo "[2/6] list-group contract"
node "$SCRIPT_DIR/check-solar-list-group-contract.mjs"

echo "[3/6] list-group zoom/DPR contract"
node "$SCRIPT_DIR/check-solar-listgroup-zoom-contract.mjs"

echo "[4/6] overlays contract"
node "$SCRIPT_DIR/check-solar-overlays-contract.mjs"

echo "[5/6] accordion parity (computed + screenshot)"
"$SCRIPT_DIR/check-solar-accordion-parity.sh"

echo "[6/6] post-cards section diff"
"$SCRIPT_DIR/check-solar-post-cards.sh"
