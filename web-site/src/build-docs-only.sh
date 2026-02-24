#!/bin/bash
set -euo pipefail

echo "-- Generating FFI doc pages --"
if [[ "${SKIP_FFI_DOC_GEN:-0}" != "1" ]]; then
  racket generate-ffi-doc-pages.rkt
  racket generate-ffi-doc-pages-structured.rkt
else
  echo "Skipping FFI doc page generation (SKIP_FFI_DOC_GEN=1)"
fi

echo "-- Compiling website-docs-only.rkt --"
racket ../../webracket.rkt --browser --ffi dom --ffi standard --ffi js --stdlib website-docs-only.rkt

echo "-- Copying Docs Assets --"
mkdir -p ../public/assets/vendor/jsxgraph
cp website-docs-only.wasm ../public
cp website-docs-only.html ../public/documentation.html
cp website-docs-only.html ../public/documentation-compiler-overview.html
cp website-docs-only.html ../public/documentation-js-ffi.html
cp website-docs-only.html ../public/documentation-ffi-standard.html
cp website-docs-only.html ../public/documentation-ffi-dom.html
cp website-docs-only.html ../public/documentation-ffi-js.html
cp website-docs-only.html ../public/documentation-ffi-math.html
cp website-docs-only.html ../public/documentation-ffi-jsxgraph.html
cp website-docs-only.html ../public/documentation-ffi-xtermjs.html
cp website-docs-only.html ../public/documentation-extended-example-jsxgraph-board-points.html
cp website-docs-only.html ../public/documentation-extended-example-jsxgraph-geometry-constructors.html
cp ../../extras/jsxgraph/docs/static/jsxgraphcore.js ../public/assets/vendor/jsxgraph/jsxgraphcore.js
cp ../../extras/jsxgraph/docs/static/jsxgraph.css ../public/assets/vendor/jsxgraph/jsxgraph.css

echo "-- Done --"
if [[ "${SKIP_SOUND:-0}" != "1" && "$(uname)" == "Darwin" ]]; then
  afplay /System/Library/Sounds/Glass.aiff || true
fi
