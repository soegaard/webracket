#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
PROBE_ID="${MATH_EDITOR_BINDINGS_PROBE_ID:-b1}"
OUTPUT_DIR="$ROOT_DIR/local/math-editor-browser-bindings/$PROBE_ID"
PORT="${MATH_EDITOR_BINDINGS_B1_PORT:-9991}"
case "$PROBE_ID" in
  b1)
    DEFAULT_SOURCE="$ROOT_DIR/test/test-math-editor-bindings-b1.rkt"
    DEFAULT_ASSERTIONS=20
    ;;
  b2)
    DEFAULT_SOURCE="$ROOT_DIR/test/test-math-editor-bindings-b2.rkt"
    DEFAULT_ASSERTIONS=28
    ;;
  b3)
    DEFAULT_SOURCE="$ROOT_DIR/test/test-math-editor-bindings-b3.rkt"
    DEFAULT_ASSERTIONS=15
    ;;
  *)
    DEFAULT_SOURCE="$ROOT_DIR/test/test-math-editor-bindings-b1.rkt"
    DEFAULT_ASSERTIONS=20
    ;;
esac
SOURCE_FILE="${MATH_EDITOR_BINDINGS_B1_SOURCE:-$DEFAULT_SOURCE}"
EXPECTED_ASSERTIONS="${MATH_EDITOR_BINDINGS_B1_EXPECT_ASSERTIONS:-$DEFAULT_ASSERTIONS}"
PROBE_SCRIPT="${MATH_EDITOR_BINDINGS_PROBE_SCRIPT:-$ROOT_DIR/test/run-math-editor-binding-probes.mjs}"
PAGE_NAME="$(basename "${SOURCE_FILE%.rkt}").html"
SERVER_PID=""

cleanup() {
  if [[ -n "$SERVER_PID" ]]; then
    kill "$SERVER_PID" >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT

env "PLTCOLLECTS=$ROOT_DIR/pkgs${PLTCOLLECTS:+:$PLTCOLLECTS}:" \
  racket -y "$ROOT_DIR/webracket.rkt" \
    --browser \
    --dest "$OUTPUT_DIR" \
    --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi" \
    "$SOURCE_FILE"

for candidate in "$PORT" 9992 9993 9994 9995; do
  python3 -m http.server "$candidate" --directory "$OUTPUT_DIR" >/tmp/webracket-math-editor-bindings-b1-server.log 2>&1 &
  SERVER_PID=$!
  sleep 1
  if ! kill -0 "$SERVER_PID" >/dev/null 2>&1; then
    SERVER_PID=""
    continue
  fi
  PORT="$candidate"
  break
done

if [[ -z "$SERVER_PID" ]]; then
  cat /tmp/webracket-math-editor-bindings-b1-server.log
  exit 1
fi

for _ in {1..20}; do
  if curl -sf "http://127.0.0.1:$PORT/$PAGE_NAME" >/dev/null; then
    break
  fi
  sleep 1
done

curl -sf "http://127.0.0.1:$PORT/$PAGE_NAME" >/dev/null
MATH_EDITOR_BINDINGS_B1_URL="http://127.0.0.1:$PORT/$PAGE_NAME" \
MATH_EDITOR_BINDINGS_PROBE_ID="$PROBE_ID" \
MATH_EDITOR_BINDINGS_B1_EXPECT_ASSERTIONS="$EXPECTED_ASSERTIONS" \
NODE_PATH="$ROOT_DIR/.local-tools/node_modules${NODE_PATH:+:$NODE_PATH}" \
node "$PROBE_SCRIPT"
