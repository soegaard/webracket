#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "[1/3] compile solar showcase"
"$SCRIPT_DIR/run-browser-solar-showcase-compile.sh"

echo "[2/3] section diff screenshots/report"
node "$SCRIPT_DIR/check-solar-sections-diff.mjs"

echo "[3/3] summarize section deltas"
node "$SCRIPT_DIR/check-solar-polish-summary.mjs"

