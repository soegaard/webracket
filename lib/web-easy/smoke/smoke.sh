#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

usage() {
  cat <<'USAGE'
Usage: ./smoke.sh <command>

Commands:
  check      Compile all smoke examples
  parity     Compile parity smoke examples, then print parity URLs
  parity-check Compile parity smoke examples only
  dashboards Print full/parity dashboard URLs
  rebuild    Clean generated artifacts, then compile all smoke examples
  status     Print tools/artifacts status and suggested next command
  urls       Print smoke URLs without starting a server
  parity-open Print parity test URLs
  open       Start local smoke server (raco static-web)
  doctor     Check local prerequisites and print setup hints
  clean      Remove generated smoke artifacts
  clean-dry  Show generated smoke artifacts that would be removed
USAGE
}

print_urls() {
  local port="${SMOKE_PORT:-8000}"
  local base_url="http://localhost:${port}"
  printf '  %-10s %-13s %s\n' "index" "(manual):" "$base_url/index.html"
  printf '  %-10s %-13s %s\n' "visual" "(manual):" "$base_url/test-browser-visual-check.html"
  printf '  %-10s %-13s %s\n' "dashboard" "(automatic):" "$base_url/test-browser-dashboard.html"
  printf '  %-10s %-13s %s\n' "parity" "(automatic):" "$base_url/test-browser-parity-dashboard.html"
  printf '  %-10s %-13s %s\n' "contract" "(automatic):" "$base_url/test-browser-contract-dashboard.html"
  printf '  %-10s %-13s %s\n' "theme" "(automatic):" "$base_url/test-browser-theme-contract-dashboard.html"
}

print_dashboards() {
  local port="${SMOKE_PORT:-8000}"
  local base_url="http://localhost:${port}"
  echo "Dashboards:"
  echo "  full:   $base_url/test-browser-dashboard.html"
  echo "  parity: $base_url/test-browser-parity-dashboard.html"
  echo "  contract: $base_url/test-browser-contract-dashboard.html"
  echo "  theme:  $base_url/test-browser-theme-contract-dashboard.html"
}

print_parity_urls() {
  local port="${SMOKE_PORT:-8000}"
  local base_url="http://localhost:${port}"
  echo "Parity test URLs:"
  echo "  $base_url/test-browser-parity-hello.html"
  echo "  $base_url/test-browser-parity-counter.html"
  echo "  $base_url/test-browser-parity-dynamic-list.html"
  echo "  $base_url/test-browser-parity-counters.html"
  echo "  $base_url/test-browser-parity-button-group.html"
  echo "  $base_url/test-browser-parity-button-group-contract.html"
  echo "  $base_url/test-browser-parity-button-toolbar.html"
  echo "  $base_url/test-browser-parity-button-toolbar-contract.html"
  echo "  $base_url/test-browser-parity-card.html"
  echo "  $base_url/test-browser-parity-card-contract.html"
  echo "  $base_url/test-browser-parity-alert.html"
  echo "  $base_url/test-browser-parity-alert-contract.html"
  echo "  $base_url/test-browser-parity-badge.html"
  echo "  $base_url/test-browser-parity-badge-contract.html"
  echo "  $base_url/test-browser-parity-spinner.html"
  echo "  $base_url/test-browser-parity-spinner-contract.html"
  echo "  $base_url/test-browser-parity-pagination.html"
  echo "  $base_url/test-browser-parity-pagination-contract.html"
  echo "  $base_url/test-browser-parity-breadcrumb.html"
  echo "  $base_url/test-browser-parity-breadcrumb-contract.html"
  echo "  $base_url/test-browser-parity-list-group.html"
  echo "  $base_url/test-browser-parity-list-group-contract.html"
  echo "  $base_url/test-browser-parity-toast.html"
  echo "  $base_url/test-browser-parity-toast-contract.html"
  echo "  $base_url/test-browser-parity-collapse.html"
  echo "  $base_url/test-browser-parity-collapse-contract.html"
  echo "  $base_url/test-browser-parity-accordion.html"
  echo "  $base_url/test-browser-parity-accordion-contract.html"
  echo "  $base_url/test-browser-parity-tabs.html"
  echo "  $base_url/test-browser-parity-tabs-dynamic.html"
  echo "  $base_url/test-browser-parity-profile.html"
  echo "  $base_url/test-browser-parity-settings.html"
  echo "  $base_url/test-browser-parity-table.html"
  echo "  $base_url/test-browser-parity-menu-keys.html"
  echo "  $base_url/test-browser-parity-menu-full.html"
  echo "  $base_url/test-browser-parity-dropdown.html"
  echo "  $base_url/test-browser-parity-navigation-bar.html"
  echo "  $base_url/test-browser-parity-tooltip.html"
  echo "  $base_url/test-browser-parity-popover.html"
  echo "  $base_url/test-browser-parity-dialog.html"
  echo "  $base_url/test-browser-parity-a11y-contract.html"
  echo "  $base_url/test-browser-parity-keyboard-contract.html"
  echo "  $base_url/test-browser-parity-focus-order.html"
  echo "  $base_url/test-browser-parity-disabled-contract.html"
  echo "  $base_url/test-browser-parity-dialog-contract.html"
  echo "  $base_url/test-browser-parity-popover-contract.html"
  echo "  $base_url/test-browser-parity-dropdown-contract.html"
  echo "  $base_url/test-browser-parity-navigation-bar-contract.html"
  echo "  $base_url/test-browser-parity-menu-single-open-contract.html"
  echo "  $base_url/test-browser-parity-menu-roving-focus-contract.html"
  echo "  $base_url/test-browser-parity-menu-close-reason-contract.html"
  echo "  $base_url/test-browser-parity-dialog-close-reason-contract.html"
  echo "  $base_url/test-browser-parity-tab-close-style-contract.html"
  echo "  $base_url/test-browser-parity-menu-typeahead-contract.html"
  echo "  $base_url/test-browser-parity-tab-aria-linkage-contract.html"
  echo "  $base_url/test-browser-parity-menu-aria-state-contract.html"
  echo "  $base_url/test-browser-parity-progress-contract.html"
  echo "  $base_url/test-browser-parity-list.html"
  echo "  $base_url/test-browser-parity-todo.html"
  echo "  $base_url/test-browser-parity-incident.html"
  echo "  $base_url/test-browser-parity-release.html"
  echo "  $base_url/test-browser-parity-workspace.html"
}

parity_check() {
  "$SCRIPT_DIR/run-browser-parity-all-compile.sh"
}

count_artifacts() {
  local generated_count
  local legacy_count

  generated_count="$(
    find "$SCRIPT_DIR/generated" -maxdepth 1 -type f 2>/dev/null \
      \( -name 'example-browser-*.html' \
      -o -name 'example-browser-*.js' \
      -o -name 'example-browser-*.wasm' \
      -o -name 'example-browser-*.wasm.map.sexp' \
      -o -name 'example-browser-*.wat' \) \
      | wc -l | tr -d ' '
  )"

  legacy_count="$(
    find "$SCRIPT_DIR" -maxdepth 1 -type f \
      \( -name 'example-browser-*.html' \
      -o -name 'example-browser-*.js' \
      -o -name 'example-browser-*.wasm' \
      -o -name 'example-browser-*.wasm.map.sexp' \
      -o -name 'example-browser-*.wat' \) \
      | wc -l | tr -d ' '
  )"

  echo "$((generated_count + legacy_count))"
}

status() {
  local missing=0
  local artifacts
  local recommend

  if command -v racket >/dev/null 2>&1; then
    echo "tool: racket            ok"
  else
    echo "tool: racket            missing"
    missing=1
  fi

  if command -v raco >/dev/null 2>&1 && raco static-web --help >/dev/null 2>&1; then
    echo "tool: raco static-web   ok"
  else
    echo "tool: raco static-web   missing"
    missing=1
  fi

  artifacts="$(count_artifacts)"
  echo "artifacts: generated smoke files = $artifacts"

  if [ "$missing" -eq 1 ]; then
    recommend="./smoke.sh doctor"
  else
    if [ "$artifacts" -eq 0 ]; then
      recommend="./smoke.sh rebuild"
    else
      recommend="./smoke.sh check"
    fi
  fi

  echo "next: $recommend"
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

  if [ "$ok" -eq 1 ]; then
    echo "doctor: required tools are available"
    return 0
  fi

  echo "doctor: missing required tools"
  return 1
}

open_server() {
  local port="${SMOKE_PORT:-8000}"
  local -a launch_args=()

  if ! command -v raco >/dev/null 2>&1; then
    echo "raco is required for 'open'."
    exit 2
  fi
  if ! raco static-web --help >/dev/null 2>&1; then
    echo "raco static-web is required for 'open'."
    exit 2
  fi

  echo "Serving smoke pages from: $SCRIPT_DIR"
  print_urls

  if [ "${SMOKE_LAUNCH:-0}" = "1" ]; then
    launch_args+=(--launch)
  fi

  raco static-web --port "$port" --dir "$SCRIPT_DIR" "${launch_args[@]}"
}

if [ "$#" -lt 1 ]; then
  usage
  exit 2
fi

case "$1" in
  check)
    "$SCRIPT_DIR/check-smoke.sh"
    ;;
  parity)
    parity_check
    print_parity_urls
    ;;
  parity-check)
    parity_check
    ;;
  status)
    status
    ;;
  rebuild)
    "$SCRIPT_DIR/clean-smoke.sh"
    "$SCRIPT_DIR/check-smoke.sh"
    ;;
  urls)
    print_urls
    ;;
  dashboards)
    print_dashboards
    ;;
  parity-open)
    print_parity_urls
    ;;
  open)
    open_server
    ;;
  doctor)
    doctor
    ;;
  clean)
    "$SCRIPT_DIR/clean-smoke.sh"
    ;;
  clean-dry)
    "$SCRIPT_DIR/clean-smoke.sh" --dry-run
    ;;
  -h|--help|help)
    usage
    ;;
  *)
    echo "Unknown command: $1"
    usage
    exit 2
    ;;
esac
