#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/../../.." && pwd)"
LOCAL_TOOLS_DIR="${SMOKE_LOCAL_TOOLS_DIR:-$ROOT_DIR/.local-tools}"

usage() {
  cat <<'USAGE'
Usage: ./smoke.sh <command>

Commands:
  check      Compile all smoke examples
  parity     Compile parity smoke examples, then print parity URLs
  parity-check Compile parity smoke examples only
  parity-headless Run doctor preflight, then parity-only headless dashboard
  all        Run core tests + webracket run + smoke compile
  headless   Run doctor preflight, then core + smoke + headless dashboard
  dashboards Print full/parity dashboard URLs
  rebuild    Clean generated artifacts, then compile all smoke examples
  quick      Run doctor preflight, then core tests + smoke compile
  ci         Stable non-headless CI entrypoint (doctor + all)
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
  printf '  %-10s %-13s %s\n' "dashboard" "(automatic):" "$base_url/test-browser-dashboard.html"
  printf '  %-10s %-13s %s\n' "parity" "(automatic):" "$base_url/test-browser-parity-dashboard.html"
}

print_dashboards() {
  local port="${SMOKE_PORT:-8000}"
  local base_url="http://localhost:${port}"
  echo "Dashboards:"
  echo "  full:   $base_url/test-browser-dashboard.html"
  echo "  parity: $base_url/test-browser-parity-dashboard.html"
}

print_parity_urls() {
  local port="${SMOKE_PORT:-8000}"
  local base_url="http://localhost:${port}"
  echo "Parity test URLs:"
  echo "  $base_url/test-browser-parity-hello.html"
  echo "  $base_url/test-browser-parity-counter.html"
  echo "  $base_url/test-browser-parity-dynamic-list.html"
  echo "  $base_url/test-browser-parity-counters.html"
  echo "  $base_url/test-browser-parity-tabs.html"
  echo "  $base_url/test-browser-parity-tabs-dynamic.html"
  echo "  $base_url/test-browser-parity-list.html"
  echo "  $base_url/test-browser-parity-todo.html"
}

parity_check() {
  "$SCRIPT_DIR/run-browser-parity-hello-compile.sh"
  "$SCRIPT_DIR/run-browser-parity-counter-compile.sh"
  "$SCRIPT_DIR/run-browser-parity-dynamic-list-compile.sh"
  "$SCRIPT_DIR/run-browser-parity-counters-compile.sh"
  "$SCRIPT_DIR/run-browser-parity-tabs-compile.sh"
  "$SCRIPT_DIR/run-browser-parity-tabs-dynamic-compile.sh"
  "$SCRIPT_DIR/run-browser-parity-list-compile.sh"
  "$SCRIPT_DIR/run-browser-parity-todo-compile.sh"
}

count_artifacts() {
  find "$SCRIPT_DIR" -maxdepth 1 -type f \
    \( -name 'example-browser-*.html' \
    -o -name 'example-browser-*.wasm' \
    -o -name 'example-browser-*.wasm.map.sexp' \
    -o -name 'example-browser-*.wat' \) \
    | wc -l | tr -d ' '
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

  if command -v node >/dev/null 2>&1; then
    echo "tool: node              ok"
  else
    echo "tool: node              missing"
    missing=1
  fi

  if [ -d "$LOCAL_TOOLS_DIR/node_modules/playwright" ]; then
    echo "tool: playwright        ok ($LOCAL_TOOLS_DIR/node_modules/playwright)"
  else
    echo "tool: playwright        missing ($LOCAL_TOOLS_DIR/node_modules/playwright)"
  fi

  artifacts="$(count_artifacts)"
  echo "artifacts: generated smoke files = $artifacts"

  if [ "$missing" -eq 1 ]; then
    recommend="./smoke.sh doctor"
  else
    if [ "$artifacts" -eq 0 ]; then
      recommend="./smoke.sh rebuild"
    else
      recommend="./smoke.sh all"
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

  if command -v node >/dev/null 2>&1; then
    echo "ok: node found ($(node --version))"
  else
    echo "missing: node (required for headless)"
    ok=0
  fi

  if [ -d "$LOCAL_TOOLS_DIR/node_modules/playwright" ]; then
    echo "ok: Playwright found at $LOCAL_TOOLS_DIR/node_modules/playwright"
  else
    echo "missing: Playwright in $LOCAL_TOOLS_DIR"
    echo "hint: npm --prefix .local-tools install --save-dev playwright"
  fi

  if [ "$ok" -eq 1 ]; then
    echo "doctor: required tools are available"
    return 0
  fi

  echo "doctor: missing required tools"
  return 1
}

doctor_headless() {
  doctor

  if [ ! -d "$LOCAL_TOOLS_DIR/node_modules/playwright" ]; then
    echo "headless preflight: Playwright is required."
    echo "hint: npm --prefix .local-tools install --save-dev playwright"
    return 1
  fi

  echo "headless preflight: ready"
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

if [ "$#" -ne 1 ]; then
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
  all)
    "$SCRIPT_DIR/check-all.sh"
    ;;
  headless)
    doctor_headless
    "$SCRIPT_DIR/check-all.sh" --headless
    ;;
  parity-headless)
    doctor_headless
    "$SCRIPT_DIR/check-parity-headless.sh"
    ;;
  quick)
    doctor
    "$SCRIPT_DIR/check-all.sh"
    ;;
  ci)
    doctor
    "$SCRIPT_DIR/check-all.sh"
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
