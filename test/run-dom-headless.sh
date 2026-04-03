#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
PORT="${DOM_HEADLESS_PORT:-9991}"
BASE_URL="http://127.0.0.1:${PORT}"
LOCAL_NODE_MODULES="${DOM_HEADLESS_NODE_MODULES:-$ROOT_DIR/.local-tools/node_modules}"
CHROME_EXECUTABLE="${DOM_HEADLESS_EXECUTABLE:-/Applications/Google Chrome.app/Contents/MacOS/Google Chrome}"

compile_test() {
  local file="$1"
  racket ../webracket.rkt \
    --browser \
    --ffi ../ffi/standard.ffi \
    --ffi ../ffi/js.ffi \
    --ffi ../ffi/dom.ffi \
    "$file"
}

compile_test test-dom-window-document.rkt
compile_test test-dom-canvas-media-image.rkt
compile_test test-dom-image.rkt
compile_test test-dom-event.rkt

if ! command -v raco >/dev/null 2>&1; then
  echo "raco is required to serve DOM browser tests."
  exit 2
fi

if ! command -v node >/dev/null 2>&1; then
  echo "node is required for the DOM browser runner."
  exit 2
fi

if ! raco static-web --help >/dev/null 2>&1; then
  echo "raco static-web is required to serve DOM browser tests."
  exit 2
fi

raco static-web --port "$PORT" --dir "$SCRIPT_DIR" >/tmp/webracket-dom-headless.log 2>&1 &
SERVER_PID=$!
trap 'kill "$SERVER_PID" >/dev/null 2>&1 || true' EXIT

sleep 1

SMOKE_BASE_URL="$BASE_URL" \
SMOKE_NODE_MODULES="$LOCAL_NODE_MODULES" \
SMOKE_CHROME_EXECUTABLE="$CHROME_EXECUTABLE" \
SMOKE_TESTS="test-dom-window-document.html,test-dom-canvas-media-image.html,test-dom-image.html,test-dom-event.html" \
node "$SCRIPT_DIR/run-dom-headless.mjs"
