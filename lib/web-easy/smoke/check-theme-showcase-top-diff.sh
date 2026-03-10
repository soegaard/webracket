#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

SMOKE_SHOWCASE_TOP_ONLY=1 \
SMOKE_SHOWCASE_TOP_CLIP_HEIGHT="${SMOKE_SHOWCASE_TOP_CLIP_HEIGHT:-360}" \
  "$SCRIPT_DIR/check-theme-showcase-diff.sh"
