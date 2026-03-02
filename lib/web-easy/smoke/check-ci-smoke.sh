#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "[1/2] full smoke headless"
"$SCRIPT_DIR/check-all.sh" --headless

echo
echo "[2/2] dashboard guard"
"$SCRIPT_DIR/smoke.sh" guard

echo
echo "ci smoke checks passed"
