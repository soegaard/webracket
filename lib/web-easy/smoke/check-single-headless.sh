#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/../../.." && pwd)"
PORT="${SMOKE_PORT:-8765}"
HOST="${SMOKE_HOST:-127.0.0.1}"
BASE_URL="http://${HOST}:${PORT}"
LOCAL_TOOLS_DIR="${SMOKE_LOCAL_TOOLS_DIR:-$ROOT_DIR/.local-tools}"

usage() {
  cat <<'USAGE'
Usage:
  ./check-single-headless.sh <compile-script> <test-page>

Examples:
  ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-profile.html
  ./check-single-headless.sh run-browser-parity-all-compile.sh test-browser-parity-todo.html
USAGE
}

if [ "$#" -ne 2 ]; then
  usage
  exit 2
fi

COMPILE_SCRIPT="$1"
TEST_PAGE="$2"

if [[ "$COMPILE_SCRIPT" != /* ]]; then
  COMPILE_SCRIPT="$SCRIPT_DIR/$COMPILE_SCRIPT"
fi

if [ ! -x "$COMPILE_SCRIPT" ]; then
  echo "Compile script is missing or not executable: $COMPILE_SCRIPT"
  exit 2
fi

if [ ! -f "$SCRIPT_DIR/$TEST_PAGE" ]; then
  echo "Test page not found: $SCRIPT_DIR/$TEST_PAGE"
  exit 2
fi

if [ "${SMOKE_SKIP_COMPILE:-0}" = "1" ]; then
  echo "Skipping compile (SMOKE_SKIP_COMPILE=1): $COMPILE_SCRIPT"
else
  "$COMPILE_SCRIPT"
fi

if ! command -v raco >/dev/null 2>&1; then
  echo "raco is required to serve smoke files via static-web."
  exit 2
fi

if ! command -v node >/dev/null 2>&1; then
  echo "node is required for headless smoke runner."
  exit 2
fi

if ! raco static-web --help >/dev/null 2>&1; then
  echo "raco static-web is required to serve smoke files."
  exit 2
fi

raco static-web --port "$PORT" --dir "$SCRIPT_DIR" >/tmp/web-easy-single-headless.log 2>&1 &
SERVER_PID=$!
trap 'kill "$SERVER_PID" >/dev/null 2>&1 || true' EXIT

sleep 1

SMOKE_BASE_URL="$BASE_URL" \
SMOKE_TEST_PAGE="$TEST_PAGE" \
SMOKE_NODE_MODULES="$LOCAL_TOOLS_DIR/node_modules" \
node "$SCRIPT_DIR/check-single-headless.mjs"
