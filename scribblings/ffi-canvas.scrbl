#lang scribble/manual

@(require "browser-api-docs.rkt")

@section{Canvas}

This page documents the low-level @racket[js-canvas-*] and
@racket[js-canvas2d-*] bindings from @tt{ffi/canvas.ffi}.

@(for/list ([spec (in-list canvas-doc-specs)])
   (render-canvas-defproc spec))
