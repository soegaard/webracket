#lang scribble/manual

@(require "browser-api-docs.rkt"
          (lib "scribblings/ffi-image-labels.rkt" "webracket")
          (for-label (lib "scribblings/ffi-image-labels.rkt" "webracket")))

@section{Image}
@declare-exporting[(lib "ffi/image.ffi" "webracket")]

This page documents the low-level @racket[js-image-*] bindings from
@tt{ffi/image.ffi}.

@(render-ffi-docs "ffi/image.ffi" "image.ffi")
