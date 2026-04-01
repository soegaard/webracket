#lang scribble/manual

@(require "browser-api-docs.rkt")

@section{Image}

This page documents the low-level @racket[js-image-*] bindings from
@tt{ffi/image.ffi}.

@(for/list ([spec (in-list image-doc-specs)])
   (render-image-defproc spec))
