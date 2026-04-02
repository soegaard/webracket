#lang scribble/manual

@(require "browser-api-docs.rkt"
          (for-label (lib "scribblings/ffi-websocket-labels.rkt" "webracket")))

@declare-exporting[(lib "scribblings/ffi-websocket-labels.rkt" "webracket")]

@section{WebSocket}

WebSocket is the browser API for keeping a persistent, two-way
connection open between the browser and a server.

The browser starts with an initial handshake. After that, either side
can send messages whenever it has something to say.

This page documents the low-level @racket[js-websocket-*] bindings from
@tt{ffi/websocket.ffi}. The higher-level checked wrapper library is
documented separately in the @racketid[websocket] library chapter.

@subsection{Low-Level FFI Surface: WebSocket}

These bindings follow the browser API directly. They are useful when you
want to work with the JavaScript object model as-is, or when you are
building a higher-level abstraction on top of the raw interop layer.

@(render-ffi-docs "ffi/websocket.ffi" "websocket.ffi")
