#lang scribble/manual

@(require "browser-api-docs.rkt"
          (lib "scribblings/lib-dom-labels.rkt" "webracket")
          (for-label (lib "scribblings/lib-dom-labels.rkt" "webracket")))

@section{DOM}
@declare-exporting[(lib "ffi/dom.ffi" "webracket")]

These entries come from @tt{ffi/dom.ffi}, and the recommended Rackety
entry point is @racket[include-lib dom], which reexports the DOM family
wrappers.

@(render-ffi-docs "ffi/dom.ffi" "dom.ffi")
