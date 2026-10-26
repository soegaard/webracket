#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
RACKET_BIN="${RACKET:-racket}"
REPRO_SOURCE="${MATH_EDITOR_DIRECT_FIELD_REPRO:-$ROOT_DIR/../math-editor/examples/math-editor-a1/repro/wr-direct-field-one-mount-control.rkt}"
RUN_ID="$(date +%Y%m%d-%H%M%S)-$$"
RUN_ROOT="${MATH_EDITOR_DIRECT_FIELD_TMP:-$ROOT_DIR/tmp/math-editor-direct-field-frame}/$RUN_ID"
OUT_DIR="$RUN_ROOT/out"
PROVIDER_CACHE_DIR="$RUN_ROOT/providers"
RUNTIME_PROVIDER_CACHE_DIR="$RUN_ROOT/runtime"
PORT="${MATH_EDITOR_DIRECT_FIELD_PORT:-10061}"
LOCAL_NODE_MODULES="${DOM_HEADLESS_NODE_MODULES:-$ROOT_DIR/.local-tools/node_modules}"
CHROME_EXECUTABLE="${DOM_HEADLESS_EXECUTABLE:-}"
SERVER_PID=""

if [ ! -f "$REPRO_SOURCE" ]; then
  echo "Direct-field repro source not found: $REPRO_SOURCE" >&2
  echo "Set MATH_EDITOR_DIRECT_FIELD_REPRO to wr-direct-field-one-mount-control.rkt." >&2
  exit 2
fi

if [ ! -x "$RACKET_BIN" ] && ! command -v "$RACKET_BIN" >/dev/null 2>&1; then
  echo "Racket executable not found: $RACKET_BIN" >&2
  exit 2
fi

if [ ! -d "$LOCAL_NODE_MODULES" ]; then
  echo "Playwright node modules not found: $LOCAL_NODE_MODULES" >&2
  exit 2
fi

mkdir -p "$OUT_DIR" "$PROVIDER_CACHE_DIR" "$RUNTIME_PROVIDER_CACHE_DIR"

echo "START direct-field compile $REPRO_SOURCE"
"$RACKET_BIN" "$ROOT_DIR/webracket.rkt" \
  --browser \
  --dest "$OUT_DIR" \
  --provider-cache-dir "$PROVIDER_CACHE_DIR" \
  --runtime-provider-cache-dir "$RUNTIME_PROVIDER_CACHE_DIR" \
  "$REPRO_SOURCE"

PAGE_NAME="$(basename "${REPRO_SOURCE%.rkt}").html"

for candidate in "$PORT" 10062 10063 10064 10065; do
  python3 -m http.server "$candidate" --directory "$OUT_DIR" >"$RUN_ROOT/server.log" 2>&1 &
  SERVER_PID=$!
  sleep 1
  if ! kill -0 "$SERVER_PID" >/dev/null 2>&1; then
    SERVER_PID=""
    continue
  fi
  PORT="$candidate"
  break
done

trap 'if [ -n "$SERVER_PID" ]; then kill "$SERVER_PID" >/dev/null 2>&1 || true; fi' EXIT

if [ -z "$SERVER_PID" ]; then
  echo "Direct-field browser server failed to bind." >&2
  cat "$RUN_ROOT/server.log" >&2
  exit 1
fi

echo "START direct-field browser $PAGE_NAME"
SMOKE_BASE_URL="http://127.0.0.1:$PORT" \
SMOKE_NODE_MODULES="$LOCAL_NODE_MODULES" \
SMOKE_CHROME_EXECUTABLE="$CHROME_EXECUTABLE" \
SMOKE_TESTS="$PAGE_NAME" \
node "$SCRIPT_DIR/run-dom-headless.mjs"
echo "PASS direct-field completion $PAGE_NAME"
