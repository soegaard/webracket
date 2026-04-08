#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/../../.." && pwd)"
EXAMPLE_DIR="$ROOT_DIR/lib/web-easy/examples/7gui-circle"
TARGET_DIR="$SCRIPT_DIR/generated"

mkdir -p "$TARGET_DIR"
if [ ! -f "$EXAMPLE_DIR/generated/7gui-circle.html" ]; then
  echo "Compiling 7gui-circle example artifacts"
  "$EXAMPLE_DIR/compile.sh"
fi

cp -f "$EXAMPLE_DIR/generated/7gui-circle.html" "$TARGET_DIR/7gui-circle.html"
cp -f "$EXAMPLE_DIR/generated/7gui-circle.wasm" "$TARGET_DIR/7gui-circle.wasm"
cp -f "$EXAMPLE_DIR/generated/7gui-circle.wasm.map.sexp" "$TARGET_DIR/7gui-circle.wasm.map.sexp"
cp -f "$EXAMPLE_DIR/generated/7gui-circle.wat" "$TARGET_DIR/7gui-circle.wat"
cp -f "$EXAMPLE_DIR/generated/web-easy-core.css" "$TARGET_DIR/web-easy-core.css"
cp -f "$EXAMPLE_DIR/generated/theme-light.css" "$TARGET_DIR/theme-light.css"
if [ -f "$EXAMPLE_DIR/generated/theme-external-light.css" ]; then
  cp -f "$EXAMPLE_DIR/generated/theme-external-light.css" "$TARGET_DIR/theme-external-light.css"
fi

echo "compiled: generated/7gui-circle.html"
