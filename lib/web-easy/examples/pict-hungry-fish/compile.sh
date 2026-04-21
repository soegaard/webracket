#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/../../../.." && pwd)"

cd "$SCRIPT_DIR"
mkdir -p generated

BASENAME="pict-hungry-fish"

racket "$ROOT_DIR/webracket.rkt" --browser --ffi dom "$BASENAME.rkt"

for ext in html js wasm wasm.map.sexp wat; do
  src="$BASENAME.$ext"
  if [ -f "$src" ]; then
    mv -f "$src" "generated/$src"
  fi
done

cp -f "$ROOT_DIR/lib/web-easy/themes/web-easy-core.css" "generated/web-easy-core.css"
cp -f "$ROOT_DIR/lib/web-easy/themes/theme-light.css"   "generated/theme-light.css"

echo "compiled: generated/$BASENAME.html"
