#!/bin/bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
EXAMPLES_DIR="${ROOT_DIR}/examples"
COMPILER="${ROOT_DIR}/webracket.rkt"

build_example() {
  local dir="$1"
  local entry="$2"
  shift 2
  echo "  - ${dir}/${entry}"
  (
    cd "${EXAMPLES_DIR}/${dir}"
    racket "${COMPILER}" "$@" --stdlib -b "${entry}"
  )
}

echo "-- Building runnable examples --"
build_example "mathjax4"       "mathjax.rkt"        --ffi dom --ffi standard
build_example "matrix-rain"    "matrix-rain.rkt"    --ffi xtermjs --ffi dom --ffi standard
build_example "xtermjs-demo"   "xtermjs-demo.rkt"   --ffi xtermjs --ffi dom --ffi standard
build_example "space-invaders" "space-invaders.rkt" --ffi dom --ffi standard
build_example "minischeme"     "minischeme.rkt"     --ffi standard --ffi xtermjs --ffi js --ffi dom
echo "-- Examples built --"
