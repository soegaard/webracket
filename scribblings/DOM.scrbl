#lang scribble/manual

@(require "browser-api-docs.rkt")

@section{DOM}

These entries come from @tt{ffi/dom.ffi}.

@(for/list ([spec (in-list dom-doc-specs)])
   (render-dom-defproc spec))
