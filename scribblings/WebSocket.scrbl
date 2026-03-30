#lang scribble/manual

@(require "browser-api-docs.rkt")

@section{WebSocket}

These entries come from @tt{ffi/websocket.ffi}.

@(for/list ([spec (in-list websocket-doc-specs)])
   (render-websocket-defproc spec))
