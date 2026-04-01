#lang scribble/manual

@(require "browser-api-docs.rkt")

@section{Performance}

This page documents the low-level @racket[js-performance-*] bindings
from @tt{ffi/performance.ffi}.

@(for/list ([spec (in-list performance-doc-specs)])
   (render-performance-defproc spec))
