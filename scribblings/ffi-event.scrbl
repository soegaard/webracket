#lang scribble/manual

@(require "browser-api-docs.rkt")

@section{Event}

This page documents the low-level @racket[js-event-*] and
@racket[js-mouse-event-*] bindings from @tt{ffi/event.ffi}.

@(for/list ([spec (in-list event-doc-specs)])
   (render-event-defproc spec))
