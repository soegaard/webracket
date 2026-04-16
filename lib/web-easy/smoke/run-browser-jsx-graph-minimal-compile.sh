#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/../../.." && pwd)"
EXAMPLE_DIR="$ROOT_DIR/lib/web-easy/examples/jsx-graph-minimal"
TARGET_DIR="$SCRIPT_DIR/generated"

mkdir -p "$TARGET_DIR"
if [ ! -f "$EXAMPLE_DIR/generated/jsx-graph-minimal.html" ]; then
  echo "Compiling jsx-graph-minimal example artifacts"
  "$EXAMPLE_DIR/compile.sh"
fi

cp -f "$EXAMPLE_DIR/generated/jsx-graph-minimal.html" "$TARGET_DIR/jsx-graph-minimal.html"
cp -f "$EXAMPLE_DIR/generated/jsx-graph-minimal.wasm" "$TARGET_DIR/jsx-graph-minimal.wasm"
cp -f "$EXAMPLE_DIR/generated/jsx-graph-minimal.wasm.map.sexp" "$TARGET_DIR/jsx-graph-minimal.wasm.map.sexp"
cp -f "$EXAMPLE_DIR/generated/jsx-graph-minimal.wat" "$TARGET_DIR/jsx-graph-minimal.wat"
cp -f "$EXAMPLE_DIR/generated/web-easy-core.css" "$TARGET_DIR/web-easy-core.css"
cp -f "$EXAMPLE_DIR/generated/theme-light.css" "$TARGET_DIR/theme-light.css"

echo "compiled: generated/jsx-graph-minimal.html"
