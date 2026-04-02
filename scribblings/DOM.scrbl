#lang scribble/manual

@(require "browser-api-docs.rkt"
          (lib "scribblings/lib-dom-labels.rkt" "webracket"))

@section{DOM}
@declare-exporting[(lib "ffi/dom.ffi" "webracket")]

The @tt{dom} library brings together the browser-facing libraries for
windows, documents, elements, events, geometry values, canvas, media,
and images.

Use @tt{dom} when you want a single import point for browser DOM work.

@(render-ffi-docs "ffi/dom.ffi" "dom.ffi")
