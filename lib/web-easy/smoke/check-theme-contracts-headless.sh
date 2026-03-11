#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

"$SCRIPT_DIR/check-theme-contract-helper.sh"
SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-theme-headless.sh"
