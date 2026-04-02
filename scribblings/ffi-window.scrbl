#lang scribble/manual

@(require "browser-api-docs.rkt"
          (lib "scribblings/ffi-window-labels.rkt" "webracket")
          (for-label (lib "scribblings/ffi-window-labels.rkt" "webracket")))

@section{Window}
@declare-exporting[(lib "ffi/window.ffi" "webracket")]

The Window API is the browser entry point for dialogs, timers, scrolling,
and access to the current document.

This page documents the low-level @racket[js-window-*] bindings from
@tt{ffi/window.ffi}.

@(render-ffi-docs "ffi/window.ffi" "window.ffi")
