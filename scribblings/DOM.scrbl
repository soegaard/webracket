#lang scribble/manual

@(require "browser-api-docs.rkt"
          (lib "scribblings/dom-family-labels.rkt" "webracket")
          (for-label (lib "scribblings/dom-family-labels.rkt" "webracket")))

@declare-exporting[(lib "scribblings/dom-family-labels.rkt" "webracket")]

@section{DOM}

These entries come from @tt{ffi/dom.ffi}, and the recommended Rackety
entry point is @racket[include-lib dom], which reexports the DOM family
wrappers.

@(render-ffi-docs "ffi/dom.ffi" "dom.ffi")
