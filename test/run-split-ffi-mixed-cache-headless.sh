#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
PORT="${SPLIT_FFI_MIXED_CACHE_PORT:-10041}"
TIMEOUT_SEC="${SPLIT_FFI_MIXED_CACHE_TIMEOUT_SEC:-720}"
RSS_LIMIT_MB="${SPLIT_FFI_MIXED_CACHE_RSS_LIMIT_MB:-3500}"
RACKET_BIN="${RACKET:-racket}"
RUN_ID="$(date +%Y%m%d-%H%M%S)-$$"
RUN_ROOT="${SPLIT_FFI_MIXED_CACHE_TMP:-$ROOT_DIR/tmp/split-ffi-mixed-cache-headless}/$RUN_ID"
OUT_DIR="$RUN_ROOT/out"
COMPILED_ROOT="$RUN_ROOT/compiled-root"
PROVIDER_CACHE_DIR="$RUN_ROOT/provider-cache"
RUNTIME_PROVIDER_CACHE_DIR="$RUN_ROOT/runtime-provider-cache"
SERVER_LOG="$RUN_ROOT/server.log"
BASE_URL=""
LOCAL_NODE_MODULES="${DOM_HEADLESS_NODE_MODULES:-$ROOT_DIR/.local-tools/node_modules}"
CHROME_EXECUTABLE="${DOM_HEADLESS_EXECUTABLE:-}"
PLAYWRIGHT_CACHE_ROOT="${PLAYWRIGHT_BROWSERS_PATH:-$HOME/Library/Caches/ms-playwright}"
SMOKE_PAGES=()
SPLIT_FFI_MODE="${SPLIT_FFI_MIXED_CACHE_MODE:-default}"
SPLIT_FFI_MODE_ARGS=()
LIST_CASES=0
ONLY_CASES=()
SKIP_CASES=()
GROUP_CASES=()
CHUNK_INDEX=0
CHUNK_COUNT=0
ALL_CASE_IDS=()
ALL_GROUPS=()
CANDIDATE_CASES=0
SELECTED_CASES=0
CASE_PASS=collect

case "$RACKET_BIN" in
  */*)
    if [[ "$RACKET_BIN" != /* ]]; then
      RACKET_BIN="$ROOT_DIR/$RACKET_BIN"
    fi
    ;;
esac

usage() {
  cat <<'USAGE'
Usage: test/run-split-ffi-mixed-cache-headless.sh [options]

Options:
  --list-cases         List available browser FFI cases and exit.
  --only ID            Run only the named case; repeatable.
  --skip ID            Skip the named case; repeatable.
  --group GROUP        Run cases in the named group; repeatable.
  --chunk N/M          Run chunk N of M after other selectors.
  --help               Show this help.

Useful environment:
  SPLIT_FFI_MIXED_CACHE_MODE=default|explicit|inline
  SPLIT_FFI_MIXED_CACHE_CHUNK=N/M
USAGE
}

contains_value() {
  local needle="$1"
  shift
  local value
  for value in "$@"; do
    if [ "$value" = "$needle" ]; then
      return 0
    fi
  done
  return 1
}

join_by_comma() {
  local IFS=,
  echo "$*"
}

parse_chunk_spec() {
  local spec="$1"
  if [[ ! "$spec" =~ ^([0-9]+)/([0-9]+)$ ]]; then
    echo "Invalid chunk spec: $spec" >&2
    echo "Expected N/M, for example 1/4." >&2
    exit 2
  fi
  CHUNK_INDEX="${BASH_REMATCH[1]}"
  CHUNK_COUNT="${BASH_REMATCH[2]}"
  if [ "$CHUNK_INDEX" -lt 1 ] || [ "$CHUNK_COUNT" -lt 1 ] || [ "$CHUNK_INDEX" -gt "$CHUNK_COUNT" ]; then
    echo "Invalid chunk spec: $spec" >&2
    echo "Expected 1 <= N <= M." >&2
    exit 2
  fi
}

if [ -n "${SPLIT_FFI_MIXED_CACHE_CHUNK:-}" ]; then
  parse_chunk_spec "$SPLIT_FFI_MIXED_CACHE_CHUNK"
fi

while [ "$#" -gt 0 ]; do
  case "$1" in
    --help)
      usage
      exit 0
      ;;
    --list-cases)
      LIST_CASES=1
      shift
      ;;
    --only)
      if [ "$#" -lt 2 ]; then
        echo "--only requires a case id." >&2
        exit 2
      fi
      ONLY_CASES+=("$2")
      shift 2
      ;;
    --skip)
      if [ "$#" -lt 2 ]; then
        echo "--skip requires a case id." >&2
        exit 2
      fi
      SKIP_CASES+=("$2")
      shift 2
      ;;
    --group)
      if [ "$#" -lt 2 ]; then
        echo "--group requires a group name." >&2
        exit 2
      fi
      GROUP_CASES+=("$2")
      shift 2
      ;;
    --chunk)
      if [ "$#" -lt 2 ]; then
        echo "--chunk requires N/M." >&2
        exit 2
      fi
      parse_chunk_spec "$2"
      shift 2
      ;;
    *)
      echo "Unknown option: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

case "$SPLIT_FFI_MODE" in
  default)
    ;;
  explicit)
    SPLIT_FFI_MODE_ARGS=(--split-runtime-ffi-module)
    ;;
  inline)
    SPLIT_FFI_MODE_ARGS=(--no-split-runtime-ffi-module)
    ;;
  *)
    echo "Unknown SPLIT_FFI_MIXED_CACHE_MODE: $SPLIT_FFI_MODE" >&2
    echo "Expected one of: default, explicit, inline" >&2
    exit 2
    ;;
esac

mkdir -p "$OUT_DIR" "$COMPILED_ROOT" "$PROVIDER_CACHE_DIR" "$RUNTIME_PROVIDER_CACHE_DIR"

if [[ -z "$CHROME_EXECUTABLE" && -d "$PLAYWRIGHT_CACHE_ROOT" ]]; then
  CHROME_EXECUTABLE="$(
    find "$PLAYWRIGHT_CACHE_ROOT" \
      -type f \
      -path '*/chrome-headless-shell' \
      2>/dev/null \
      | sort -V \
      | tail -n 1
  )"
fi

compile_root_test() {
  local file="$1"
  shift || true
  echo "START split-ffi compile $file"
  python3 "$ROOT_DIR/tools/run-guarded.py" \
    --timeout-sec "$TIMEOUT_SEC" \
    --rss-limit-mb "$RSS_LIMIT_MB" \
    -- \
    /usr/bin/env PLTCOMPILEDROOTS="$COMPILED_ROOT" \
    "$RACKET_BIN" "$ROOT_DIR/webracket.rkt" \
      --browser \
      "${SPLIT_FFI_MODE_ARGS[@]}" \
      --dest "$OUT_DIR" \
      --provider-cache-dir "$PROVIDER_CACHE_DIR" \
      --runtime-provider-cache-dir "$RUNTIME_PROVIDER_CACHE_DIR" \
      "$@" \
      "$ROOT_DIR/$file"
  echo "PASS split-ffi compile $file"
}

compile_test_dir_test() {
  local file="$1"
  shift || true
  echo "START split-ffi compile test/$file"
  (
    cd "$SCRIPT_DIR"
    python3 "$ROOT_DIR/tools/run-guarded.py" \
      --timeout-sec "$TIMEOUT_SEC" \
      --rss-limit-mb "$RSS_LIMIT_MB" \
      -- \
      /usr/bin/env PLTCOMPILEDROOTS="$COMPILED_ROOT" \
      "$RACKET_BIN" "$ROOT_DIR/webracket.rkt" \
        --browser \
        "${SPLIT_FFI_MODE_ARGS[@]}" \
        --dest "$OUT_DIR" \
        --provider-cache-dir "$PROVIDER_CACHE_DIR" \
        --runtime-provider-cache-dir "$RUNTIME_PROVIDER_CACHE_DIR" \
        "$@" \
        "$file"
  )
  echo "PASS split-ffi compile test/$file"
}

join_smoke_pages() {
  local IFS=,
  echo "${SMOKE_PAGES[*]}"
}

case_id_from_page() {
  local page="$1"
  echo "${page%.html}"
}

case_groups() {
  local kind="$1"
  local page="$2"
  local file="$3"
  shift 3
  local groups=(all "$kind")
  local arg

  if [[ "$page" == *missing-host* || "$file" == *missing-host* ]]; then
    groups+=(missing-host)
  fi
  if [[ "$page" == *surface* || "$file" == *surface* ]]; then
    groups+=(surface)
  fi
  if [[ "$page" == test-js-* || "$file" == test/test-js-* ]]; then
    groups+=(js)
  fi
  if [[ "$page" == test-browser-* || "$file" == test/test-browser-* ]]; then
    groups+=(browser)
  fi
  if [[ "$page" == test-dom-* || "$file" == test-dom* || "$file" == test/test-dom-* ]]; then
    groups+=(dom)
  fi
  if [[ "$page" == test-rquery-* || "$file" == *rquery* ]]; then
    groups+=(rquery)
  fi
  if [[ "$page" == *web-easy* || "$file" == *web-easy* ]]; then
    groups+=(web-easy)
  fi

  for arg in "$@"; do
    case "$arg" in
      *standard.ffi) groups+=(standard) ;;
      *dom.ffi) groups+=(dom) ;;
      *audio.ffi) groups+=(audio) ;;
      *array.ffi) groups+=(array) ;;
      *console.ffi) groups+=(console) ;;
      *image.ffi) groups+=(image) ;;
      *websocket.ffi) groups+=(websocket) ;;
    esac
  done

  printf '%s\n' "${groups[@]}"
}

remember_group() {
  local group="$1"
  if ! contains_value "$group" "${ALL_GROUPS[@]}"; then
    ALL_GROUPS+=("$group")
  fi
}

case_selector_matches() {
  local case_id="$1"
  shift
  local groups=("$@")
  local group

  if contains_value "$case_id" "${SKIP_CASES[@]}"; then
    return 1
  fi

  if [ "${#ONLY_CASES[@]}" -eq 0 ] && [ "${#GROUP_CASES[@]}" -eq 0 ]; then
    return 0
  fi

  if contains_value "$case_id" "${ONLY_CASES[@]}"; then
    return 0
  fi

  for group in "${groups[@]}"; do
    if contains_value "$group" "${GROUP_CASES[@]}"; then
      return 0
    fi
  done

  return 1
}

case_selected_for_run() {
  local case_id="$1"
  shift
  local groups=("$@")
  if ! case_selector_matches "$case_id" "${groups[@]}"; then
    return 1
  fi

  CANDIDATE_CASES=$((CANDIDATE_CASES + 1))
  if [ "$CHUNK_COUNT" -gt 0 ]; then
    local offset=$((CHUNK_INDEX - 1))
    local candidate_offset=$(((CANDIDATE_CASES - 1) % CHUNK_COUNT))
    if [ "$candidate_offset" -ne "$offset" ]; then
      return 1
    fi
  fi

  return 0
}

handle_case() {
  local kind="$1"
  local page="$2"
  local file="$3"
  shift 3
  local case_id
  case_id="$(case_id_from_page "$page")"
  local groups=()
  local group
  while IFS= read -r group; do
    groups+=("$group")
  done < <(case_groups "$kind" "$page" "$file" "$@")
  local unique_groups=()
  for group in "${groups[@]}"; do
    if ! contains_value "$group" "${unique_groups[@]}"; then
      unique_groups+=("$group")
    fi
  done
  groups=("${unique_groups[@]}")

  case "$CASE_PASS" in
    collect)
      ALL_CASE_IDS+=("$case_id")
      for group in "${groups[@]}"; do
        remember_group "$group"
      done
      if [ "$LIST_CASES" -eq 1 ]; then
        printf "%s\tgroups=%s\tpage=%s\tsource=%s\n" \
          "$case_id" \
          "$(join_by_comma "${groups[@]}")" \
          "$page" \
          "$file"
      fi
      ;;
    run)
      if case_selected_for_run "$case_id" "${groups[@]}"; then
        SELECTED_CASES=$((SELECTED_CASES + 1))
        if [ "$kind" = "test-dir" ]; then
          compile_test_dir_test "$file" "$@"
        else
          compile_root_test "$file" "$@"
        fi
        SMOKE_PAGES+=("$page")
      fi
      ;;
  esac
}

compile_root_case() {
  handle_case root "$@"
}

compile_test_dir_case() {
  handle_case test-dir "$@"
}

register_cases() {
compile_test_dir_case test-audio.html test-audio.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/audio.ffi"
compile_root_case test-audio-method-missing-host-ffi.html test/test-audio-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/audio.ffi"
compile_test_dir_case test-console.html test-console.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/console.ffi"
compile_root_case test-console-method-missing-host-ffi.html test/test-console-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/console.ffi"
compile_test_dir_case test-array.html test-array.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/array.ffi"
compile_root_case test-js-array-method-missing-host-ffi.html test/test-js-array-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/array.ffi"
compile_root_case test-standard-send-missing-host-ffi.html test/test-standard-send-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi"
compile_root_case test-standard-property-missing-host-ffi.html test/test-standard-property-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi"
compile_root_case test-js-throw-external-callback.html test/test-js-throw-external-callback.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi"
compile_root_case test-js-standard-global-missing-host-ffi.html test/test-js-standard-global-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi"
compile_root_case test-js-standard-function-missing-host-ffi.html test/test-js-standard-function-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi"
compile_root_case test-js-standard-generic-missing-host-ffi.html test/test-js-standard-generic-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi"
compile_root_case test-js-promise-surface.html test/test-js-promise-surface.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi"
compile_root_case test-js-promise-method-missing-host-ffi.html test/test-js-promise-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi"
compile_root_case test-js-iterator-method-missing-host-ffi.html test/test-js-iterator-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi"
compile_root_case test-js-math-ffi.html test/test-js-math-ffi.rkt
compile_root_case test-js-number-method-missing-host-ffi.html test/test-js-number-method-missing-host-ffi.rkt
compile_root_case test-js-performance-ffi.html test/test-js-performance-ffi.rkt
compile_root_case test-performance-missing-host-ffi.html test/test-performance-missing-host-ffi.rkt
compile_root_case test-runtime-date-global-boundary.html test/test-runtime-date-global-boundary.rkt
compile_root_case test-js-temp-ffi.html test/test-js-temp-ffi.rkt
compile_root_case test-xtermjs-missing-host-ffi.html test/test-xtermjs-missing-host-ffi.rkt
compile_root_case test-xtermjs-ffi.html test/test-xtermjs-ffi.rkt
compile_root_case test-jsxgraph-missing-host-ffi.html test/test-jsxgraph-missing-host-ffi.rkt
compile_root_case test-jsxgraph-call-missing-host-ffi.html test/test-jsxgraph-call-missing-host-ffi.rkt
compile_root_case test-jsxgraph-ffi.html test/test-jsxgraph-ffi.rkt
compile_root_case test-browser-constructor-missing-host-ffi.html test/test-browser-constructor-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/audio.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/image.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/websocket.ffi"
compile_root_case test-event-constructor-missing-host-ffi.html test/test-event-constructor-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-event-method-missing-host-ffi.html test/test-event-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-window-missing-host-ffi.html test/test-window-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-window-method-missing-host-ffi.html test/test-window-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-window-value-missing-host-ffi.html test/test-window-value-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-document-missing-host-ffi.html test/test-document-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-object-method-missing-host-ffi.html test/test-object-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-element-method-missing-host-ffi.html test/test-element-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-browser-element-method-missing-host-ffi.html test/test-browser-element-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-browser-shared-dom-method-missing-host-ffi.html test/test-browser-shared-dom-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_test_dir_case test-fetch.html test-fetch.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-fetch-method-missing-host-ffi.html test/test-fetch-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-compression-stream-method-missing-host-ffi.html test/test-compression-stream-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_test_dir_case test-storage.html test-storage.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_test_dir_case test-storage-method-missing-host-ffi.html test-storage-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_test_dir_case test-websocket.html test-websocket.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/websocket.ffi"
compile_root_case test-websocket-surface.html test/test-websocket-surface.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/websocket.ffi"
compile_root_case test-websocket-method-missing-host-ffi.html test/test-websocket-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/websocket.ffi"
compile_test_dir_case test-indexed-db.html test-indexed-db.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_test_dir_case test-indexed-db-method-missing-host-ffi.html test-indexed-db-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-history-surface.html test/test-history-surface.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-history-method-missing-host-ffi.html test/test-history-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-fullscreen-surface.html test/test-fullscreen-surface.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-fullscreen-method-missing-host-ffi.html test/test-fullscreen-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-web-workers-surface.html test/test-web-workers-surface.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-web-workers-method-missing-host-ffi.html test/test-web-workers-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-geolocation-surface.html test/test-geolocation-surface.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-geolocation-method-missing-host-ffi.html test/test-geolocation-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-clipboard-surface.html test/test-clipboard-surface.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-clipboard-mock.html test/test-clipboard-mock.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-clipboard-multi-mime.html test/test-clipboard-multi-mime.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-clipboard-method-missing-host-ffi.html test/test-clipboard-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-notifications-surface.html test/test-notifications-surface.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-notifications-method-missing-host-ffi.html test/test-notifications-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-web-speech-surface.html test/test-web-speech-surface.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-web-speech-method-missing-host-ffi.html test/test-web-speech-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-screen-orientation-surface.html test/test-screen-orientation-surface.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-screen-orientation-method-missing-host-ffi.html test/test-screen-orientation-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-gamepad-surface.html test/test-gamepad-surface.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-gamepad-method-missing-host-ffi.html test/test-gamepad-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-intersection-observer-surface.html test/test-intersection-observer-surface.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-resize-observer-surface.html test/test-resize-observer-surface.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-observer-method-missing-host-ffi.html test/test-observer-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_test_dir_case test-dom-facade.html test-dom-facade.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-rquery-surface-ffi.html test/test-rquery-surface-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-rquery-method-missing-host-ffi.html test/test-rquery-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-web-easy-scrollspy-reflect-boundary.html test/test-web-easy-scrollspy-reflect-boundary.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-js-string-symbol-ffi.html test/test-js-string-symbol-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-browser-canvas-draw-image-integers.html test/test-browser-canvas-draw-image-integers.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-web-pict-method-missing-host-ffi.html test/test-web-pict-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-browser-window-surface.html test/test-browser-window-surface.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-browser-event-surface.html test/test-browser-event-surface.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-browser-canvas-surface.html test/test-browser-canvas-surface.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-browser-canvas-method-missing-host-ffi.html test/test-browser-canvas-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-browser-image-surface.html test/test-browser-image-surface.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-browser-image-method-missing-host-ffi.html test/test-browser-image-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/standard.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi" \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/image.ffi"
compile_root_case test-browser-media-method-missing-host-ffi.html test/test-browser-media-method-missing-host-ffi.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-dom-window-document.html test/test-dom-window-document.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-dom-event.html test/test-dom-event.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-dom-image.html test/test-dom-image.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
compile_root_case test-dom-canvas-media-image.html test/test-dom-canvas-media-image.rkt \
  --ffi "$ROOT_DIR/pkgs/webracket-lib/compiler/ffi/dom.ffi"
}

validate_case_selectors() {
  local id
  for id in "${ONLY_CASES[@]}"; do
    if ! contains_value "$id" "${ALL_CASE_IDS[@]}"; then
      echo "Unknown split-FFI browser case id: $id" >&2
      exit 2
    fi
  done
  for id in "${SKIP_CASES[@]}"; do
    if ! contains_value "$id" "${ALL_CASE_IDS[@]}"; then
      echo "Unknown split-FFI browser case id: $id" >&2
      exit 2
    fi
  done

  local group
  for group in "${GROUP_CASES[@]}"; do
    if ! contains_value "$group" "${ALL_GROUPS[@]}"; then
      echo "Unknown split-FFI browser group: $group" >&2
      echo "Known groups: $(join_by_comma "${ALL_GROUPS[@]}")" >&2
      exit 2
    fi
  done
}

register_cases
validate_case_selectors

if [ "$LIST_CASES" -eq 1 ]; then
  echo "Total cases: ${#ALL_CASE_IDS[@]}"
  echo "Groups: $(join_by_comma "${ALL_GROUPS[@]}")"
  exit 0
fi

CASE_PASS=run
register_cases

if [ "$SELECTED_CASES" -eq 0 ]; then
  echo "No split-FFI browser cases selected." >&2
  exit 1
fi

echo "Selected split-FFI browser cases: $SELECTED_CASES"
if [ "$CHUNK_COUNT" -gt 0 ]; then
  echo "Chunk: $CHUNK_INDEX/$CHUNK_COUNT from $CANDIDATE_CASES matching cases"
fi

if ! command -v node >/dev/null 2>&1; then
  echo "node is required for the DOM browser runner."
  exit 2
fi

if ! command -v python3 >/dev/null 2>&1; then
  echo "python3 is required to serve split-FFI browser tests."
  exit 2
fi

SERVER_PID=""
for candidate in "$PORT" 10042 10043 10044 10045 10046 10047 10048 10049 9991 9992 9993 9994 9995 9996 9997 9998 9999; do
  : >"$SERVER_LOG"
  python3 -m http.server "$candidate" --directory "$OUT_DIR" >"$SERVER_LOG" 2>&1 &
  SERVER_PID=$!
  sleep 1
  if ! kill -0 "$SERVER_PID" >/dev/null 2>&1; then
    SERVER_PID=""
    continue
  fi
  if grep -q "Address already in use" "$SERVER_LOG"; then
    kill "$SERVER_PID" >/dev/null 2>&1 || true
    SERVER_PID=""
    continue
  fi
  PORT="$candidate"
  BASE_URL="http://127.0.0.1:${PORT}"
  break
done

trap 'kill "$SERVER_PID" >/dev/null 2>&1 || true' EXIT

if [ -z "$SERVER_PID" ]; then
  echo "Split-FFI browser server failed to bind an available port."
  cat "$SERVER_LOG"
  exit 1
fi

READY_PAGE="${SMOKE_PAGES[0]}"
for _ in 1 2 3 4 5 6 7 8 9 10; do
  if curl -sf "$BASE_URL/$READY_PAGE" >/dev/null; then
    break
  fi
  sleep 1
done

if grep -q "Address already in use" "$SERVER_LOG"; then
  echo "Split-FFI browser server failed to bind port $PORT."
  cat "$SERVER_LOG"
  exit 1
fi

if ! curl -sf "$BASE_URL/$READY_PAGE" >/dev/null; then
  echo "Split-FFI browser server did not become ready at $BASE_URL."
  cat "$SERVER_LOG"
  exit 1
fi

SMOKE_BASE_URL="$BASE_URL" \
SMOKE_NODE_MODULES="$LOCAL_NODE_MODULES" \
SMOKE_CHROME_EXECUTABLE="$CHROME_EXECUTABLE" \
SMOKE_TESTS="$(join_smoke_pages)" \
node "$SCRIPT_DIR/run-dom-headless.mjs"

echo "PASS split-ffi mixed-cache browser smoke"
echo "OUTPUT $RUN_ROOT"
