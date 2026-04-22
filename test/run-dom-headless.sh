#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
PORT="${DOM_HEADLESS_PORT:-9991}"
BASE_URL=""
LOCAL_NODE_MODULES="${DOM_HEADLESS_NODE_MODULES:-$ROOT_DIR/.local-tools/node_modules}"
CHROME_EXECUTABLE="${DOM_HEADLESS_EXECUTABLE:-/Applications/Google Chrome.app/Contents/MacOS/Google Chrome}"

compile_test() {
  local file="$1"
  shift || true
  racket ../webracket.rkt \
    --browser \
    --ffi ../ffi/dom.ffi \
    "$@" \
    "$file"
}

compile_test test-dom-window-document.rkt
compile_test test-dom-canvas-media-image.rkt
compile_test test-dom-image.rkt
compile_test test-dom-event.rkt
compile_test test-console-bridge-smoke.rkt --console-bridge

if ! command -v node >/dev/null 2>&1; then
  echo "node is required for the DOM browser runner."
  exit 2
fi

if ! command -v python3 >/dev/null 2>&1; then
  echo "python3 is required to serve DOM browser tests."
  exit 2
fi

cd "$SCRIPT_DIR"

SERVER_PID=""
for candidate in "$PORT" 9992 9993 9994 9995 9996 9997 9998 9999; do
  : >/tmp/webracket-dom-headless.log
  python3 -m http.server "$candidate" >/tmp/webracket-dom-headless.log 2>&1 &
  SERVER_PID=$!
  sleep 1
  if grep -q "Address already in use" /tmp/webracket-dom-headless.log; then
    kill "$SERVER_PID" >/dev/null 2>&1 || true
    SERVER_PID=""
    continue
  fi
  PORT="$candidate"
  BASE_URL="http://127.0.0.1:${PORT}"
  break
done

trap 'kill "$SERVER_PID" >/dev/null 2>&1 || true' EXIT

if [ -z "$SERVER_PID" ]; then
  echo "DOM browser server failed to bind an available port."
  cat /tmp/webracket-dom-headless.log
  exit 1
fi

for _ in 1 2 3 4 5 6 7 8 9 10; do
  if curl -sf "$BASE_URL/test-dom-window-document.html" >/dev/null; then
    break
  fi
  sleep 1
done

if grep -q "Address already in use" /tmp/webracket-dom-headless.log; then
  echo "DOM browser server failed to bind port $PORT."
  cat /tmp/webracket-dom-headless.log
  exit 1
fi

if ! curl -sf "$BASE_URL/test-dom-window-document.html" >/dev/null; then
  echo "DOM browser server did not become ready at $BASE_URL."
  cat /tmp/webracket-dom-headless.log
  exit 1
fi

SMOKE_BASE_URL="$BASE_URL" \
SMOKE_NODE_MODULES="$LOCAL_NODE_MODULES" \
SMOKE_CHROME_EXECUTABLE="$CHROME_EXECUTABLE" \
SMOKE_TESTS="test-dom-window-document.html,test-dom-canvas-media-image.html,test-dom-image.html,test-dom-event.html,test-console-bridge-smoke.html" \
node "$SCRIPT_DIR/run-dom-headless.mjs"
