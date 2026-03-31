#lang scribble/manual

@(require "browser-api-docs.rkt")

@section{WebSocket}

The browser WebSocket support is exposed in two layers:

@itemlist[
  @item{@tt{ffi/websocket.ffi} provides the low-level @racket[js-websocket-*] bindings.}
  @item{@racket[(include-lib websocket)] provides the checked @racket[websocket-*] wrapper library for application code.}
]

The table below documents the low-level FFI surface.

@(for/list ([spec (in-list websocket-doc-specs)])
   (render-websocket-defproc spec))
