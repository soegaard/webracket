#lang scribble/manual

@(require "browser-api-docs.rkt"
          (lib "scribblings/ffi-domrect-labels.rkt" "webracket")
          (for-label (lib "scribblings/ffi-domrect-labels.rkt" "webracket")))

@section{DOMRect}
@declare-exporting[(lib "ffi/domrect.ffi" "webracket")]

This page documents the low-level @racket[js-dom-rect-*] bindings from
@tt{ffi/domrect.ffi}.

@(render-ffi-docs "ffi/domrect.ffi" "domrect.ffi")
