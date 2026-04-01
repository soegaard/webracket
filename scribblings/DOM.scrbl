#lang scribble/manual

@(require "browser-api-docs.rkt")

@section{DOM}

These entries come from @tt{ffi/dom.ffi}, and the recommended Rackety
entry point is @racket[include-lib dom], which reexports the DOM family
wrappers.

@(for/list ([spec (in-list dom-doc-specs)])
   (render-dom-defproc spec))
