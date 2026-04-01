#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/dom-family-labels.rkt" "webracket")))

@title{Library: @racketid[dom]}
@declare-exporting[(lib "scribblings/dom-family-labels.rkt" "webracket")]

@(how-to-require include-lib dom (lib "libs/dom.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @racket[dom] library is the top-level Rackety facade for the split DOM
wrapper surface. It reexports the family libraries for windows, documents,
events, geometry, canvas, media, and images.

Use @racket[dom] when you want one import point for the browser DOM surface.
