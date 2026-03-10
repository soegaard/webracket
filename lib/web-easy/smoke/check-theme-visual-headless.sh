#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Visual lane: screenshot/computed-diff checks are intentionally separate from
# contract dashboards so theme tuning does not block behavior regressions.
if [ "${SMOKE_SKIP_COMPILE:-0}" = "1" ]; then
  SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-theme-showcase-diff.sh"
else
  "$SCRIPT_DIR/check-theme-showcase-diff.sh"
fi

SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-theme-showcase-compile.sh test-browser-solar2-navbar-variants-contract.html
SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-theme-showcase-compile.sh test-browser-parity-solar2-navbar-variants-contract.html
SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-theme-showcase-compile.sh test-browser-solar2-button-disabled-contract.html
SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-theme-showcase-compile.sh test-browser-solar2-button-row-display-contract.html
SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-theme-showcase-compile.sh test-browser-solar2-indicators-contract.html
