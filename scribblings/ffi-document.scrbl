#lang scribble/manual

@(require "browser-api-docs.rkt")

@section{Document}

This page documents the low-level @racket[js-document-*] bindings from
@tt{ffi/document.ffi}.

@(for/list ([spec (in-list document-doc-specs)])
   (render-document-defproc spec))
