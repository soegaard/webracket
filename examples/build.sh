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
    racket "${COMPILER}" "$@" -b "${entry}"
  )
}

build_example_node() {
  local dir="$1"
  local entry="$2"
  shift 2
  echo "  - ${dir}/${entry} (node)"
  (
    cd "${EXAMPLES_DIR}/${dir}"
    racket "${COMPILER}" --node "$@" "${entry}"
  )
}

echo "-- Building runnable examples --"
build_example "mathjax4"       "mathjax.rkt"        --ffi dom --ffi standard
build_example "hello-world-2"  "hello-world.rkt"    --ffi dom --ffi standard
build_example "hello-world-3"  "hello-world-3.rkt"  --ffi dom --ffi standard
build_example "matrix-rain"    "matrix-rain.rkt"    --ffi xtermjs --ffi dom --ffi standard
build_example "xtermjs-demo"   "xtermjs-demo.rkt"   --ffi xtermjs --ffi dom --ffi standard
build_example "space-invaders" "space-invaders.rkt" --ffi dom --ffi standard
build_example "minischeme"     "minischeme.rkt"     --ffi standard --ffi xtermjs --ffi js --ffi dom
build_example_node "hello-world-1" "hello-world-1.rkt"
echo "-- Examples built --"
