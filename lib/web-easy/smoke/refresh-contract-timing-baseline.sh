#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
BASELINE="$SCRIPT_DIR/contract-timings-baseline.tsv"
TMP_OUT="/tmp/web-easy-contract-timings.tsv"

SMOKE_TIMING_OUT="$TMP_OUT" "$SCRIPT_DIR/check-contract-headless.sh"
cp "$TMP_OUT" "$BASELINE"
echo "Refreshed contract timing baseline: $BASELINE"
