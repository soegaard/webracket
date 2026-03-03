#!/usr/bin/env bash
set -euo pipefail

# Compile a canonical browser target and optionally print a compatibility message.
# Usage: ./run-browser-target-compile.sh <target> [compat-message]

if [ "$#" -lt 1 ] || [ "$#" -gt 2 ]; then
  echo "Usage: $0 <target> [compat-message]"
  echo "Targets: smoke-all | parity-all | visual-check"
  exit 2
fi

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TARGET="$1"
COMPAT_MSG="${2:-}"

case "$TARGET" in
  smoke-all)
    "$SCRIPT_DIR/compile-browser-example.sh" \
      "example-browser-smoke-all.rkt" \
      "browser smoke-all compile: ok"
    ;;
  parity-all)
    "$SCRIPT_DIR/compile-browser-example.sh" \
      "example-browser-parity-all.rkt" \
      "browser parity-all compile: ok"
    ;;
  visual-check)
    "$SCRIPT_DIR/compile-browser-example.sh" \
      "example-browser-visual-check.rkt" \
      "browser visual-check compile: ok"
    ;;
  *)
    echo "Unknown target: $TARGET"
    echo "Targets: smoke-all | parity-all | visual-check"
    exit 2
    ;;
esac

if [ -n "$COMPAT_MSG" ]; then
  echo "$COMPAT_MSG"
fi
