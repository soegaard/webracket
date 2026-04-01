#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/dom-family-labels.rkt" "webracket")))

@title{Library: @racketid[media]}
@declare-exporting[(lib "scribblings/dom-family-labels.rkt" "webracket")]

@(how-to-require include-lib media (lib "libs/media.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @racket[media] library is the checked wrapper for HTML media
elements such as audio and video.

Media elements are what the browser uses for sound and video playback.
They can point at a source file, report the current playback time, and
play or pause when the page is ready.

Use @racket[media] when you want to:

@itemlist[
  @item{set up audio or video playback on the page}
  @item{read or set the current playback position}
  @item{toggle browser controls on a media element}
  @item{start or pause playback}
]

The @racket[media] library wraps HTMLMediaElement properties and the
common play/pause controls.

@section{Media Quick Start}

Start by creating a media element, giving it a source, and checking its
playback position.

@racketblock[
(code:comment "Include the document and media wrapper libraries.")
(include-lib document)
(include-lib media)

(code:comment "Create an audio element we can configure.")
(define player
  (document-create-element "audio"))

(code:comment "Point the element at some media and show controls.")
(media-set-src! player "/audio/theme.ogg")
(media-set-controls! player #t)

(code:comment "Read the current playback time.")
(media-current-time player)
]

The quick start shows the basic pattern: create a media element, point
it at a source, and read or adjust playback state.

@section{Media Example}

This example shows a small media setup that prepares an audio element
and seeks it to a new position.

@racketblock[
(code:comment "Include the wrapper libraries used for the page and media.")
(include-lib document)
(include-lib media)

(code:comment "Create a media element and configure it for playback.")
(define player
  (document-create-element "audio"))
(media-set-src! player "/audio/theme.ogg")
(media-set-controls! player #t)

(code:comment "Read and then adjust the playback position.")
(define before
  (media-current-time player))
(media-set-current-time! player (+ before 5))

(code:comment "Check whether the browser still considers the element controlled.")
(define controls? (media-controls? player))

(void controls? player)
]

When you only need a few helpers, the most useful entry points are
@racket[media-src], @racket[media-set-src!], @racket[media-current-time],
@racket[media-set-current-time!], @racket[media-controls?], and
@racket[media-set-controls!].

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

@defproc[(media-src [media external?]) string?]{
@(mdn-bar "HTMLMediaElement: src property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/src")
Returns the current media source URL.
}

@defproc[(media-set-src! [media external?] [src string?]) void?]{
@(mdn-bar "HTMLMediaElement: src property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/src")
Sets the media source URL.
}

@defproc[(media-controls? [media external?]) boolean?]{
@(mdn-bar "HTMLMediaElement: controls property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/controls")
Reports whether browser controls are shown.
}

@defproc[(media-set-controls! [media external?] [enabled any/c]) void?]{
@(mdn-bar "HTMLMediaElement: controls property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/controls")
Shows or hides the browser's media controls.
}
