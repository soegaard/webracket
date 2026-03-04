#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "[1/2] full smoke headless"
"$SCRIPT_DIR/headless.sh" all

echo
echo "[2/2] dashboard guard"
"$SCRIPT_DIR/headless.sh" guard

echo
echo "ci smoke checks passed"
