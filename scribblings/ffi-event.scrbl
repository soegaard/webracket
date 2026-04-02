#lang scribble/manual

@(require "browser-api-docs.rkt"
          (lib "scribblings/ffi-event-labels.rkt" "webracket")
          (for-label (lib "scribblings/ffi-event-labels.rkt" "webracket")))

@section{Event}
@declare-exporting[(lib "ffi/event.ffi" "webracket")]

This page documents the low-level @racket[js-event-*] and
@racket[js-mouse-event-*] bindings from @tt{ffi/event.ffi}.

@(render-ffi-docs "ffi/event.ffi" "event.ffi")
