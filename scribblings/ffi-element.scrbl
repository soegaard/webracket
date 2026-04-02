#lang scribble/manual

@(require "browser-api-docs.rkt"
          (lib "scribblings/ffi-element-labels.rkt" "webracket")
          (for-label (lib "scribblings/ffi-element-labels.rkt" "webracket")))

@section{Element}
@declare-exporting[(lib "scribblings/ffi-element-labels.rkt" "webracket")]

This page documents the low-level @racket[js-*] bindings for generic DOM
elements from @tt{ffi/element.ffi}.

@(render-ffi-docs "ffi/element.ffi" "element.ffi")
