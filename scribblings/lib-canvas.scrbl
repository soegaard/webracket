#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/dom-family-labels.rkt" "webracket")))

@title{Library: @racketid[canvas]}
@declare-exporting[(lib "scribblings/dom-family-labels.rkt" "webracket")]

@(how-to-require include-lib canvas (lib "libs/canvas.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @racket[canvas] library provides checked wrappers for canvas
elements and the 2D rendering context.

@defproc[(canvas-get-context [canvas external?] [context-id string?] [options any/c (void)])
         (or/c #f external?)]{
Returns the requested drawing context.
}

@defproc[(canvas-width [canvas external?]) exact-nonnegative-integer?]{
Returns the canvas width in CSS pixels.
}

@defproc[(canvas-2d-fill-rect [ctx external?] [x real?] [y real?] [w real?] [h real?]) void?]{
Fills a rectangle in a 2D canvas context.
}
