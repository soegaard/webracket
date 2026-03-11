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
  core-structure           Run focused core-structure contracts (smoke+parity)
  core-structure-ci        Run aggregate core-structure contracts only (fast lane)
  deep                     Run deep keyboard contract pages only
  style                    Run style-hook contract pages only
  theme                    Run theme-only dashboard headless
  theme-visual             Run theme visual-diff lane (separate from contracts)
  solar-token              Run Solar showcase token parity checks
  dashboards               Run contract dashboard and full smoke dashboard
  ci-fast                  Run lean CI path (smoke dashboard + key core contracts)
  ci                       Run dashboards + theme dashboard and guard self-test
  timings                  Run contract/smoke/parity/theme/style (skip compile) and print combined top timings
  guard                    Run dashboard guard self-test
  all                      Run check-all.sh --headless
  single <compile> <page>  Run one page with check-single-headless.sh

Examples:
  ./headless.sh list
  ./headless.sh verify
  ./headless.sh smoke
  ./headless.sh parity
  ./headless.sh contract
  ./headless.sh core-structure
  ./headless.sh core-structure-ci
  ./headless.sh deep
  ./headless.sh style
  ./headless.sh theme
  ./headless.sh theme-visual
  ./headless.sh solar-token
  ./headless.sh dashboards
  ./headless.sh ci-fast
  ./headless.sh ci
  ./headless.sh timings
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
core-structure	Run focused core-structure contracts (smoke+parity)
core-structure-ci	Run aggregate core-structure contracts only (fast lane)
deep	Run deep keyboard contract pages only
style	Run style-hook contract pages only
theme	Run theme-only dashboard headless
theme-visual	Run theme visual-diff lane (separate from contracts)
solar-token	Run Solar showcase token parity checks
dashboards	Run contract dashboard and full smoke dashboard
ci-fast	Run lean CI path (smoke dashboard + key core contracts)
ci	Run dashboards + theme dashboard and guard self-test
timings	Run contract/smoke/parity/theme/style with compile skipped and print combined top timings
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
  core-structure)
    shift
    if [ "${SMOKE_SKIP_COMPILE:-0}" = "1" ]; then
      "$SCRIPT_DIR/check-single-headless.sh" run-browser-smoke-all-compile.sh test-browser-menu-core-structure-contract.html
    else
      "$SCRIPT_DIR/check-smoke.sh"
      SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-smoke-all-compile.sh test-browser-menu-core-structure-contract.html
    fi
    SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-smoke-all-compile.sh test-browser-collapse-core-structure-contract.html
    SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-smoke-all-compile.sh test-browser-tab-core-structure-contract.html
    SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-smoke-all-compile.sh test-browser-core-structure-contract.html
    SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-parity-all-compile.sh test-browser-parity-menu-core-structure-contract.html
    SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-parity-all-compile.sh test-browser-parity-collapse-core-structure-contract.html
    SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-parity-all-compile.sh test-browser-parity-tab-core-structure-contract.html
    SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-parity-all-compile.sh test-browser-parity-core-structure-contract.html
    ;;
  core-structure-ci)
    shift
    if [ "${SMOKE_SKIP_COMPILE:-0}" = "1" ]; then
      "$SCRIPT_DIR/check-single-headless.sh" run-browser-smoke-all-compile.sh test-browser-core-structure-contract.html
    else
      "$SCRIPT_DIR/check-smoke.sh"
      SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-smoke-all-compile.sh test-browser-core-structure-contract.html
    fi
    SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-parity-all-compile.sh test-browser-parity-core-structure-contract.html
    ;;
  deep)
    shift
    "$SCRIPT_DIR/check-single-headless.sh" run-browser-smoke-all-compile.sh test-browser-dropdown-keyboard-deep.html
    SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-smoke-all-compile.sh test-browser-scrollspy-keyboard-deep.html
    SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-parity-all-compile.sh test-browser-parity-dropdown-keyboard-deep.html
    SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-parity-all-compile.sh test-browser-parity-scrollspy-keyboard-deep.html
    ;;
  style)
    shift
    exec "$SCRIPT_DIR/check-style-headless.sh" "$@"
    ;;
  theme)
    shift
    exec "$SCRIPT_DIR/check-theme-headless.sh" "$@"
    ;;
  theme-visual)
    shift
    exec "$SCRIPT_DIR/check-theme-visual-headless.sh" "$@"
    ;;
  solar-token)
    shift
    exec "$SCRIPT_DIR/check-solar-token-parity.sh" "$@"
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
  ci-fast)
    shift
    if [ "${SMOKE_SKIP_COMPILE:-0}" = "1" ]; then
      "$SCRIPT_DIR/check-smoke-headless.sh" "$@"
    else
      "$SCRIPT_DIR/check-smoke.sh"
      SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-smoke-headless.sh" "$@"
    fi
    SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-smoke-all-compile.sh test-browser-a11y-contract.html
    SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-smoke-all-compile.sh test-browser-keyboard-contract.html
    SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-smoke-all-compile.sh test-browser-dropdown-contract.html
    SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-smoke-all-compile.sh test-browser-dialog-contract.html
    SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-smoke-all-compile.sh test-browser-modal-contract.html
    SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-smoke-all-compile.sh test-browser-tooltip-contract.html
    SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-smoke-all-compile.sh test-browser-toast-behavior-contract.html
    SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-smoke-all-compile.sh test-browser-menu-typeahead-contract.html
    SMOKE_SKIP_COMPILE=1 "$SCRIPT_DIR/check-single-headless.sh" run-browser-parity-all-compile.sh test-browser-parity-menu-core-structure-contract.html
    ;;
  ci)
    shift
    "$SCRIPT_DIR/headless.sh" dashboards "$@"
    "$SCRIPT_DIR/headless.sh" theme "$@"
    "$SCRIPT_DIR/headless.sh" guard "$@"
    ;;
  timings)
    shift
    contract_file="/tmp/web-easy-contract-timings.tsv"
    smoke_file="/tmp/web-easy-smoke-timings.tsv"
    parity_file="/tmp/web-easy-parity-timings.tsv"
    theme_file="/tmp/web-easy-theme-timings.tsv"
    style_file="/tmp/web-easy-style-timings.tsv"
    core_structure_file="/tmp/web-easy-core-structure-timings.tsv"
    all_file="/tmp/web-easy-all-timings.tsv"
    rm -f "$all_file"

    SMOKE_SKIP_COMPILE=1 SMOKE_TIMING_OUT="$contract_file" "$SCRIPT_DIR/check-contract-headless.sh" "$@"
    SMOKE_SKIP_COMPILE=1 SMOKE_TIMING_OUT="$smoke_file" "$SCRIPT_DIR/check-smoke-headless.sh" "$@"
    SMOKE_SKIP_COMPILE=1 SMOKE_TIMING_OUT="$parity_file" "$SCRIPT_DIR/check-parity-headless.sh" "$@"
    SMOKE_SKIP_COMPILE=1 SMOKE_TIMING_OUT="$theme_file" "$SCRIPT_DIR/check-theme-headless.sh" "$@"
    SMOKE_SKIP_COMPILE=1 SMOKE_TIMING_OUT="$style_file" "$SCRIPT_DIR/check-style-headless.sh" "$@"
    SMOKE_SKIP_COMPILE=1 SMOKE_TIMING_OUT="$core_structure_file" "$SCRIPT_DIR/headless.sh" core-structure-ci "$@"

    {
      echo -e "suite\tpage\tduration_ms"
      for suite in contract smoke parity theme style core_structure; do
        file_var="${suite}_file"
        file_path="${!file_var}"
        if [ -f "$file_path" ]; then
          awk -F $'\t' -v s="$suite" 'NR > 1 { print s "\t" $1 "\t" $2 }' "$file_path"
        fi
      done
    } > "$all_file"

    echo "Combined timing TSV written: $all_file"
    echo "Top 10 slowest pages (all suites):"
    awk -F $'\t' 'NR > 1 { print $3 "\t" $1 "\t" $2 }' "$all_file" \
      | sort -nr \
      | head -n 10 \
      | awk -F $'\t' '{ printf("- %s [%s]: %sms\n", $3, $2, $1) }'
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
