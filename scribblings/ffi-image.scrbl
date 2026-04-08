#lang scribble/manual

@(require "browser-api-docs.rkt"
          (lib "scribblings/ffi-image-labels.rkt" "webracket")
          (for-label (lib "scribblings/ffi-image-labels.rkt" "webracket")))

@title{Image}
@declare-exporting[(lib "scribblings/ffi-image-labels.rkt" "webracket")]

This page documents the low-level @racket[js-image-*] bindings from
@tt{ffi/image.ffi}.

@(render-ffi-docs "ffi/image.ffi" "image.ffi")
