#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
WEB_SITE_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
EXAMPLES_DIR="${WEB_SITE_DIR}/../lib/web-easy/examples"
PUBLISH_DIR="${WEB_SITE_DIR}/public/web-easy-examples"
INDEX_PAGE="${WEB_SITE_DIR}/public/web-easy-examples.html"
MAX_DRIFT_SECONDS="${MAX_DRIFT_SECONDS:-600}"

mtime() {
  local path="$1"
  if stat -f %m "$path" >/dev/null 2>&1; then
    stat -f %m "$path"
  else
    stat -c %Y "$path"
  fi
}

echo "-- Publishing web-easy examples --"
rm -rf "$PUBLISH_DIR"
mkdir -p "$PUBLISH_DIR"

while IFS= read -r compile_sh; do
  example_dir="$(dirname "$compile_sh")"
  example_name="$(basename "$example_dir")"
  source_file="$example_dir/$example_name.rkt"
  generated_dir="$example_dir/generated"
  main_html="$generated_dir/$example_name.html"
  target_dir="$PUBLISH_DIR/$example_name"

  if [[ ! -f "$main_html" ]]; then
    echo "Error: missing generated HTML for $example_name: $main_html" >&2
    exit 1
  fi

  if [[ ! -f "$source_file" ]]; then
    echo "Error: missing source file for $example_name: $source_file" >&2
    exit 1
  fi

  source_mtime="$(mtime "$source_file")"
  html_mtime="$(mtime "$main_html")"

  if (( source_mtime > html_mtime + MAX_DRIFT_SECONDS )); then
    echo "Error: stale generated output for $example_name" >&2
    echo "  source:   $source_file ($source_mtime)" >&2
    echo "  generated: $main_html ($html_mtime)" >&2
    echo "  drift:    $((source_mtime - html_mtime))s (limit ${MAX_DRIFT_SECONDS}s)" >&2
    exit 1
  fi

  echo "Publishing $example_name"
  mkdir -p "$target_dir"
  cp -R "$generated_dir/." "$target_dir/"
  cp "$main_html" "$target_dir/index.html"
  cp "$main_html" "$target_dir/$example_name.html"
done < <(find "$EXAMPLES_DIR" -mindepth 2 -maxdepth 2 -name compile.sh | sort)

bash "$SCRIPT_DIR/generate-web-easy-examples-page.sh"
echo "-- Done --"
