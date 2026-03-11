#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "[1/2] capture section screenshots + computed metrics"
"$SCRIPT_DIR/check-solar-sections-diff.sh"

echo "[2/2] summarize per-section deltas (includes RMSE)"
node "$SCRIPT_DIR/check-solar-polish-summary.mjs"
