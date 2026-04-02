#lang scribble/manual

@(require "browser-api-docs.rkt"
          (lib "scribblings/ffi-domrect-labels.rkt" "webracket")
          (for-label (lib "scribblings/ffi-domrect-labels.rkt" "webracket")))

@declare-exporting[(lib "scribblings/ffi-domrect-labels.rkt" "webracket")]

@section{DOMRect}

This page documents the low-level @racket[js-dom-rect-*] bindings from
@tt{ffi/domrect.ffi}.

@(render-ffi-docs "ffi/domrect.ffi" "domrect.ffi")
