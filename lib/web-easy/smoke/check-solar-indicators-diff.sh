#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SMOKE_BASE_URL="${SMOKE_BASE_URL:-http://localhost:8000}" \
  node "$SCRIPT_DIR/check-solar-indicators-diff.mjs"
