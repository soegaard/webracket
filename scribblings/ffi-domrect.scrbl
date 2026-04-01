#lang scribble/manual

@(require "browser-api-docs.rkt")

@section{DOMRect}

This page documents the low-level @racket[js-dom-rect-*] bindings from
@tt{ffi/domrect.ffi}.

@(for/list ([spec (in-list domrect-doc-specs)])
   (render-domrect-defproc spec))
