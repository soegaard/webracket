#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/dom-family-labels.rkt" "webracket")))

@title{Library: @racketid[performance]}
@declare-exporting[(lib "scribblings/dom-family-labels.rkt" "webracket")]

@(how-to-require include-lib performance (lib "libs/performance.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @racket[performance] library exposes a checked high-resolution
timestamp helper for the browser Performance API.

@defproc[(performance-now) real?]{
Returns a monotonic browser timestamp suitable for measuring elapsed
time.
}
