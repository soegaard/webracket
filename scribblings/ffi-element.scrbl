#lang scribble/manual

@(require "browser-api-docs.rkt")

@section{Element}

This page documents the low-level @racket[js-*] bindings for generic DOM
elements from @tt{ffi/element.ffi}.

@(for/list ([spec (in-list element-doc-specs)])
   (render-element-defproc spec))
