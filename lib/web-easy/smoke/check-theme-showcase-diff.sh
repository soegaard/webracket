#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
PORT="${SMOKE_PORT:-8765}"
BASE_URL="${SMOKE_BASE_URL:-http://127.0.0.1:${PORT}}"
DEFAULT_NODE_MODULES="${SCRIPT_DIR}/../../../.local-tools/node_modules"

if [ "${SMOKE_SKIP_COMPILE:-0}" != "1" ]; then
  "$SCRIPT_DIR/run-browser-theme-showcase-compile.sh"
else
  echo "Skipping showcase compile (SMOKE_SKIP_COMPILE=1)."
fi

if ! command -v node >/dev/null 2>&1; then
  echo "node is required for theme showcase diff runner."
  exit 2
fi

raco static-web --port "$PORT" --dir "$SCRIPT_DIR" >/tmp/web-easy-theme-showcase-diff.log 2>&1 &
SERVER_PID=$!

cleanup() {
  if kill -0 "$SERVER_PID" >/dev/null 2>&1; then
    kill "$SERVER_PID" >/dev/null 2>&1 || true
    wait "$SERVER_PID" 2>/dev/null || true
  fi
}
trap cleanup EXIT INT TERM

sleep 1
if [ -z "${SMOKE_NODE_MODULES:-}" ] && [ -d "$DEFAULT_NODE_MODULES" ]; then
  export SMOKE_NODE_MODULES="$DEFAULT_NODE_MODULES"
fi
SMOKE_BASE_URL="$BASE_URL" node "$SCRIPT_DIR/check-theme-showcase-diff.mjs"
