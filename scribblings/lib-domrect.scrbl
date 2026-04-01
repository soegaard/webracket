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
@(mdn-bar "DOMRect: left property"
          "https://developer.mozilla.org/en-US/docs/Web/API/DOMRect/left")
Returns the left coordinate.
}

@defproc[(dom-rect-top [rect external?]) real?]{
@(mdn-bar "DOMRect: top property"
          "https://developer.mozilla.org/en-US/docs/Web/API/DOMRect/top")
Returns the top coordinate.
}
