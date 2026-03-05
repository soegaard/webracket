#!/usr/bin/env bash
set -euo pipefail

# Compile a browser smoke/parity example and move generated artifacts to ./generated.
if [ "$#" -ne 2 ]; then
  echo "Usage: $0 <example-rkt-file> <success-message>"
  exit 2
fi

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/../../.." && pwd)"
WEB_EASY_DIR="$ROOT_DIR/lib/web-easy"
EXAMPLE_RKT="$1"
SUCCESS_MESSAGE="$2"

cd "$WEB_EASY_DIR/smoke"
mkdir -p generated

BASENAME="${EXAMPLE_RKT%.rkt}"

# Lock per compile target so concurrent jobs cannot clobber shared outputs.
LOCK_DIR=".compile-lock-${BASENAME}"
LOCK_WAIT_SECONDS=60
lock_start="$(date +%s)"
while ! mkdir "$LOCK_DIR" 2>/dev/null; do
  now="$(date +%s)"
  if [ $((now - lock_start)) -ge "$LOCK_WAIT_SECONDS" ]; then
    echo "Timed out waiting for compile lock: $LOCK_DIR"
    exit 1
  fi
  sleep 0.05
done
cleanup_lock() {
  rmdir "$LOCK_DIR" >/dev/null 2>&1 || true
}
trap cleanup_lock EXIT

mtime_seconds() {
  local path="$1"
  if stat -f "%m" "$path" >/dev/null 2>&1; then
    stat -f "%m" "$path"
  else
    stat -c "%Y" "$path"
  fi
}

# Fast path: if key generated outputs already exist, skip recompilation.
if [ "${SMOKE_FORCE_COMPILE:-0}" != "1" ]; then
  if [ -f "generated/$BASENAME.html" ] \
    && [ -f "generated/$BASENAME.wasm" ] \
    && [ -f "generated/$BASENAME.wat" ]; then
    newest_source=0
    for src in "$WEB_EASY_DIR"/*.rkt "$WEB_EASY_DIR/smoke"/*.rkt; do
      src_mtime="$(mtime_seconds "$src")"
      if [ "$src_mtime" -gt "$newest_source" ]; then
        newest_source="$src_mtime"
      fi
    done

    artifact_mtime="$(mtime_seconds "generated/$BASENAME.wasm")"
    if [ "$artifact_mtime" -ge "$newest_source" ]; then
      echo "$SUCCESS_MESSAGE (cached)"
      exit 0
    fi
  fi
fi

racket ../../../webracket.rkt --browser --ffi dom --ffi standard --ffi js "$EXAMPLE_RKT"

for ext in html js wasm wasm.map.sexp wat; do
  SRC="$BASENAME.$ext"
  if [ -f "$SRC" ]; then
    mv -f "$SRC" "generated/$SRC"
  fi
done

echo "$SUCCESS_MESSAGE"
