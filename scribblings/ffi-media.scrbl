#lang scribble/manual

@(require "browser-api-docs.rkt")

@section{Media}

This page documents the low-level @racket[js-media-*] bindings from
@tt{ffi/media.ffi}.

@(for/list ([spec (in-list media-doc-specs)])
   (render-media-defproc spec))
