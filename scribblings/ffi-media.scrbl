#lang scribble/manual

@(require "browser-api-docs.rkt"
          (lib "scribblings/ffi-media-labels.rkt" "webracket")
          (for-label (lib "scribblings/ffi-media-labels.rkt" "webracket")))

@title{Media}
@declare-exporting[(lib "scribblings/ffi-media-labels.rkt" "webracket")]

This page documents the low-level @racket[js-media-*] bindings from
@tt{ffi/media.ffi}.

@(render-ffi-docs "ffi/media.ffi" "media.ffi")
