#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/../../.." && pwd)"
LOCAL_TOOLS_DIR="${SMOKE_LOCAL_TOOLS_DIR:-$ROOT_DIR/.local-tools}"

usage() {
  cat <<'USAGE'
Usage: ./headless.sh <mode> [args]

Modes:
  list                     Print available modes (stable, scriptable)
  verify                   Run doctor + COMMANDS.tsv freshness check
  doctor                   Check headless prerequisites
  smoke                    Run full smoke dashboard headless
  parity                   Run parity-only dashboard headless
  contract                 Run contract-only dashboard headless
  dashboards               Run contract dashboard and full smoke dashboard
  ci                       Run dashboards and guard self-test
  guard                    Run dashboard guard self-test
  all                      Run check-all.sh --headless
  single <compile> <page>  Run one page with check-single-headless.sh

Examples:
  ./headless.sh list
  ./headless.sh verify
  ./headless.sh smoke
  ./headless.sh parity
  ./headless.sh contract
  ./headless.sh dashboards
  ./headless.sh ci
  ./headless.sh guard
  ./headless.sh all
  ./headless.sh doctor
  SMOKE_SKIP_COMPILE=1 ./headless.sh single run-browser-smoke-all-compile.sh test-browser-tab-panel-dynamic.html
USAGE
}

list_modes() {
  cat <<'LIST'
list	Print available modes (stable, scriptable)
verify	Run doctor + COMMANDS.tsv freshness check
doctor	Check headless prerequisites
smoke	Run full smoke dashboard headless
parity	Run parity-only dashboard headless
contract	Run contract-only dashboard headless
dashboards	Run contract dashboard and full smoke dashboard
ci	Run dashboards and guard self-test
guard	Run dashboard guard self-test
all	Run check-all.sh --headless
single	Run one page with check-single-headless.sh
LIST
}

doctor() {
  local ok=1

  if command -v racket >/dev/null 2>&1; then
    echo "ok: racket found ($(racket --version | head -n 1))"
  else
    echo "missing: racket"
    ok=0
  fi

  if command -v raco >/dev/null 2>&1; then
    if raco static-web --help >/dev/null 2>&1; then
      echo "ok: raco static-web available"
    else
      echo "missing: raco static-web"
      ok=0
    fi
  else
    echo "missing: raco"
    ok=0
  fi

  if command -v node >/dev/null 2>&1; then
    echo "ok: node found ($(node --version))"
  else
    echo "missing: node"
    ok=0
  fi

  if [ -d "$LOCAL_TOOLS_DIR/node_modules/playwright" ]; then
    echo "ok: Playwright package found at $LOCAL_TOOLS_DIR/node_modules/playwright"
  else
    echo "missing: Playwright package at $LOCAL_TOOLS_DIR/node_modules/playwright"
    echo "hint: npm --prefix .local-tools install --save-dev playwright"
    ok=0
  fi

  if [ "$ok" -eq 1 ]; then
    echo "headless doctor: ready"
    return 0
  fi

  echo "headless doctor: missing prerequisites"
  return 1
}

verify() {
  doctor
  "$SCRIPT_DIR/gen-commands.sh"
  git -C "$ROOT_DIR" diff --exit-code -- "$SCRIPT_DIR/COMMANDS.tsv"
  echo "headless verify: COMMANDS.tsv is up to date"
}

if [ "$#" -lt 1 ]; then
  usage
  exit 2
fi

case "$1" in
  list)
    shift
    list_modes "$@"
    ;;
  verify)
    shift
    verify "$@"
    ;;
  doctor)
    shift
    doctor "$@"
    ;;
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
  dashboards)
    shift
    if [ "${SMOKE_SKIP_COMPILE:-0}" = "1" ]; then
      "$SCRIPT_DIR/check-contract-headless.sh" "$@"
      "$SCRIPT_DIR/check-smoke-headless.sh" "$@"
    else
      "$SCRIPT_DIR/check-smoke.sh"
      SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-contract-headless.sh" "$@"
      SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-smoke-headless.sh" "$@"
    fi
    ;;
  ci)
    shift
    "$SCRIPT_DIR/headless.sh" dashboards "$@"
    "$SCRIPT_DIR/headless.sh" guard "$@"
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
