#lang scribble/manual

@(require "browser-api-docs.rkt"
          (lib "scribblings/ffi-image-labels.rkt" "webracket")
          (for-label (lib "scribblings/ffi-image-labels.rkt" "webracket")))

@declare-exporting[(lib "scribblings/ffi-image-labels.rkt" "webracket")]

@section{Image}

This page documents the low-level @racket[js-image-*] bindings from
@tt{ffi/image.ffi}.

@(render-ffi-docs "ffi/image.ffi" "image.ffi")
