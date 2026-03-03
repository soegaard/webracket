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

racket ../../../webracket.rkt --browser --ffi dom --ffi standard "$EXAMPLE_RKT"

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

for ext in html js wasm wasm.map.sexp wat; do
  SRC="$BASENAME.$ext"
  if [ -f "$SRC" ]; then
    mv -f "$SRC" "generated/$SRC"
  fi
done

echo "$SUCCESS_MESSAGE"
