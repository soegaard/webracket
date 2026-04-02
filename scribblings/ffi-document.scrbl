#lang scribble/manual

@(require "browser-api-docs.rkt"
          (lib "scribblings/ffi-document-labels.rkt" "webracket")
          (for-label (lib "scribblings/ffi-document-labels.rkt" "webracket")))

@declare-exporting[(lib "scribblings/ffi-document-labels.rkt" "webracket")]

@section{Document}

This page documents the low-level @racket[js-document-*] bindings from
@tt{ffi/document.ffi}.

@(render-ffi-docs "ffi/document.ffi" "document.ffi")
