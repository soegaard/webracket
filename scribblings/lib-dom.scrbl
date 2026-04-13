#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          )

@title{Library: dom}
@declare-exporting[(lib "scribblings/lib-dom-labels.rkt" "webracket")]

@(how-to-require include-lib dom (lib "libs/dom.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @tt{dom} library brings together the browser-facing libraries for
windows, documents, elements, events, geometry values, canvas, media,
images, fetch, storage, and IndexedDB.

Use @tt{dom} when you want a single import point for browser DOM work.
