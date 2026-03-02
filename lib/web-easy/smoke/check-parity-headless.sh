#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/../../.." && pwd)"
PORT="${SMOKE_PORT:-8765}"
HOST="${SMOKE_HOST:-127.0.0.1}"
BASE_URL="http://${HOST}:${PORT}"
LOCAL_TOOLS_DIR="${SMOKE_LOCAL_TOOLS_DIR:-$ROOT_DIR/.local-tools}"

"$SCRIPT_DIR/run-browser-parity-hello-compile.sh"
"$SCRIPT_DIR/run-browser-parity-counter-compile.sh"
"$SCRIPT_DIR/run-browser-parity-dynamic-list-compile.sh"
"$SCRIPT_DIR/run-browser-parity-counters-compile.sh"
"$SCRIPT_DIR/run-browser-parity-tabs-compile.sh"
"$SCRIPT_DIR/run-browser-parity-tabs-dynamic-compile.sh"
"$SCRIPT_DIR/run-browser-parity-list-compile.sh"
"$SCRIPT_DIR/run-browser-parity-todo-compile.sh"

if ! command -v raco >/dev/null 2>&1; then
  echo "raco is required to serve smoke files via static-web."
  exit 2
fi

if ! command -v node >/dev/null 2>&1; then
  echo "node is required for headless parity runner."
  exit 2
fi

if ! raco static-web --help >/dev/null 2>&1; then
  echo "raco static-web is required to serve smoke files."
  exit 2
fi

raco static-web --port "$PORT" --dir "$SCRIPT_DIR" >/tmp/web-easy-parity-headless.log 2>&1 &
SERVER_PID=$!
trap 'kill "$SERVER_PID" >/dev/null 2>&1 || true' EXIT

sleep 1

SMOKE_BASE_URL="$BASE_URL" \
SMOKE_NODE_MODULES="$LOCAL_TOOLS_DIR/node_modules" \
node "$SCRIPT_DIR/check-parity-headless.mjs"
