#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

usage() {
  cat <<'USAGE'
Usage: ./headless.sh <mode> [args]

Modes:
  smoke                    Run full smoke dashboard headless
  parity                   Run parity-only dashboard headless
  contract                 Run contract-only dashboard headless
  guard                    Run dashboard guard self-test
  all                      Run check-all.sh --headless
  single <compile> <page>  Run one page with check-single-headless.sh

Examples:
  ./headless.sh smoke
  ./headless.sh parity
  ./headless.sh contract
  ./headless.sh guard
  ./headless.sh all
  SMOKE_SKIP_COMPILE=1 ./headless.sh single run-browser-smoke-all-compile.sh test-browser-tab-panel-dynamic.html
USAGE
}

if [ "$#" -lt 1 ]; then
  usage
  exit 2
fi

case "$1" in
  smoke)
    shift
    exec "$SCRIPT_DIR/check-smoke-headless.sh" "$@"
    ;;
  parity)
    shift
    exec "$SCRIPT_DIR/check-parity-headless.sh" "$@"
    ;;
  contract)
    shift
    exec "$SCRIPT_DIR/check-contract-headless.sh" "$@"
    ;;
  guard)
    shift
    exec "$SCRIPT_DIR/check-dashboard-guard.sh" "$@"
    ;;
  all)
    shift
    exec "$SCRIPT_DIR/check-all.sh" --headless "$@"
    ;;
  single)
    shift
    if [ "$#" -ne 2 ]; then
      echo "single mode requires: <compile-script> <test-page>"
      usage
      exit 2
    fi
    exec "$SCRIPT_DIR/check-single-headless.sh" "$1" "$2"
    ;;
  -h|--help|help)
    usage
    ;;
  *)
    echo "Unknown mode: $1"
    usage
    exit 2
    ;;
esac
