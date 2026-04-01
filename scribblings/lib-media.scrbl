#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/dom-family-labels.rkt" "webracket")))

@title{Library: @racketid[media]}
@declare-exporting[(lib "scribblings/dom-family-labels.rkt" "webracket")]

@(how-to-require include-lib media (lib "libs/media.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @racket[media] library wraps HTMLMediaElement properties and the
common play/pause controls.

@defproc[(media-current-time [media external?]) real?]{
@(mdn-bar "HTMLMediaElement: currentTime property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/currentTime")
Returns the current playback time.
}

@defproc[(media-set-current-time! [media external?] [t real?]) void?]{
@(mdn-bar "HTMLMediaElement: currentTime property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/currentTime")
Seeks to a playback time.
}

@defproc[(media-play [media external?]) external/raw]{
@(mdn-bar "HTMLMediaElement: play() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/play")
Starts playback and returns the browser promise.
}
