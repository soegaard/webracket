#lang scribble/manual

@(require "browser-api-docs.rkt"
          (lib "scribblings/ffi-jsxgraph-labels.rkt" "webracket")
          (for-label (lib "scribblings/ffi-jsxgraph-labels.rkt" "webracket"))
          "webracket-scribble-utils.rkt")

@title{JSXGraph}
@declare-exporting[(lib "scribblings/ffi-jsxgraph-labels.rkt" "webracket")]

@(jsx-bar "JXG"
          (jsx-doc-url "JXG"))

JSXGraph is a browser-based JavaScript library for interactive
geometry. These bindings expose the low-level @racket[js-jsx-*] and
@racket[js-jsx-board-*] functions exported by @tt{ffi/jsxgraph.ffi},
including the board-creation entry point @racket[JXG.JSXGraph] and the
basic geometry constructors such as @racket[JXG.Point], @racket[JXG.Line],
@racket[JXG.Circle], and @racket[JXG.Text].

The library is aimed at programs that want to work directly with the
JSXGraph board and point APIs from WebRacket. It is especially useful
for constructing boards, creating geometry elements, and updating the
board after changes.

This page documents the low-level bindings from @tt{ffi/jsxgraph.ffi}.
The page is intentionally reference-oriented: it mirrors the FFI export
surface and links each function to the corresponding JSXGraph API.

@(render-ffi-docs "ffi/jsxgraph.ffi" "jsxgraph.ffi")
