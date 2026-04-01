#lang scribble/manual

@(require "browser-api-docs.rkt")

@section{Window}

The Window API is the browser entry point for dialogs, timers, scrolling,
and access to the current document.

This page documents the low-level @racket[js-window-*] bindings from
@tt{ffi/window.ffi}.

@(for/list ([spec (in-list window-doc-specs)])
   (render-window-defproc spec))
