#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/dom-family-labels.rkt" "webracket")))

@title{Library: @racketid[domrect]}
@declare-exporting[(lib "scribblings/dom-family-labels.rkt" "webracket")]

@(how-to-require include-lib domrect (lib "libs/domrect.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @racket[domrect] library provides checked accessors for DOMRect
geometry values.

@defproc[(dom-rect-left [rect external?]) real?]{
Returns the left coordinate.
}

@defproc[(dom-rect-top [rect external?]) real?]{
Returns the top coordinate.
}
