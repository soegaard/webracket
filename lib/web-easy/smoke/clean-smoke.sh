#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
DRY_RUN=0

for arg in "$@"; do
  case "$arg" in
    --dry-run) DRY_RUN=1 ;;
    *)
      echo "Unknown argument: $arg"
      echo "Usage: $0 [--dry-run]"
      exit 2
      ;;
  esac
done

mapfile -t FILES < <(
  find "$SCRIPT_DIR/generated" -maxdepth 1 -type f 2>/dev/null \
    \( -name 'example-browser-*.html' \
    -o -name 'example-browser-*.js' \
    -o -name 'example-browser-*.wasm' \
    -o -name 'example-browser-*.wasm.map.sexp' \
    -o -name 'example-browser-*.wat' \) \
    | sort
)

mapfile -t LEGACY_FILES < <(
  find "$SCRIPT_DIR" -maxdepth 1 -type f \
    \( -name 'example-browser-*.html' \
    -o -name 'example-browser-*.js' \
    -o -name 'example-browser-*.wasm' \
    -o -name 'example-browser-*.wasm.map.sexp' \
    -o -name 'example-browser-*.wat' \) \
    | sort
)

FILES+=("${LEGACY_FILES[@]}")

if [ "${#FILES[@]}" -eq 0 ]; then
  echo "No generated smoke artifacts found."
  exit 0
fi

if [ "$DRY_RUN" -eq 1 ]; then
  echo "Would remove ${#FILES[@]} generated smoke artifact(s):"
  printf '  %s\n' "${FILES[@]}"
  exit 0
fi

echo "Removing ${#FILES[@]} generated smoke artifact(s)..."
rm -f -- "${FILES[@]}"
echo "Smoke artifacts removed."
