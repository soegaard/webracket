#lang scribble/manual

@(require "browser-api-docs.rkt"
          (for-label (lib "scribblings/ffi-audio-labels.rkt" "webracket")))

@section{Audio}
@declare-exporting[(lib "scribblings/ffi-audio-labels.rkt" "webracket")]

The Audio API exposes the browser Web Audio graph model. It lets a page
create audio contexts, connect nodes, inspect parameters, and drive
playback or synthesis from WebRacket.

This page documents the low-level @racket[js-audio-*] bindings from
@tt{ffi/audio.ffi}. The higher-level checked wrapper library is
documented separately in the @racketid[audio] library chapter.

@subsection{Low-Level FFI Surface: Audio}

These bindings follow the browser API directly. They are useful when you
want to work with the JavaScript object model as-is, or when you are
building a higher-level abstraction on top of the raw interop layer.

@(render-ffi-docs "ffi/audio.ffi" "audio.ffi")
