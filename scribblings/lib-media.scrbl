#lang scribble/manual

@(require scribble/manual
          (for-label (only-in racket/base struct))
          "webracket-scribble-utils.rkt"
          )

@title{Library: @racketid[media]}
@declare-exporting[(lib "scribblings/lib-media-labels.rkt" "webracket")]

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

When a helper expects a browser string such as a media source, preload
hint, codec type, or sink id, the wrapper also accepts a symbol and
normalizes it to a string before calling the browser.

Some of the browser values that media elements expose are wrapped too:
track lists, time ranges, media errors, and captured media streams are
all represented with checked structs instead of raw browser objects.
Individual AudioTrack, TextTrack, and VideoTrack values are wrapped
the same way, so the track lists and addTextTrack() helper stay within
WebRacket values.
The attached MediaKeys object and the current source object are wrapped
too, so the media wrapper stays within checked values at those edges as
well.

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

@subsection{Media Properties}

These helpers cover common playback, source, and browser state
properties on HTML media elements.

@defproc[(media-autoplay? [media external?]) boolean?]{
@(mdn-bar "HTMLMediaElement: autoplay property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/autoplay")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Reports whether autoplay is enabled.
}

@defproc[(media-set-autoplay! [media external?] [enabled boolean?]) void?]{
@(mdn-bar "HTMLMediaElement: autoplay property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/autoplay")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Enables or disables autoplay.
}

@defproc[(media-current-src [media external?]) string?]{
@(mdn-bar "HTMLMediaElement: currentSrc property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/currentSrc")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Returns the resolved media source URL.
}

@defproc[(media-cross-origin [media external?]) (or/c #f string?)]{
@(mdn-bar "HTMLMediaElement: crossOrigin property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/crossOrigin")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Returns the CORS mode for media requests.
}

@defproc[(media-set-cross-origin! [media external?]
                                  [cross-origin (or/c string? symbol?)]) void?]{
@(mdn-bar "HTMLMediaElement: crossOrigin property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/crossOrigin")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Symbols are normalized to strings
before the browser CORS mode is updated.
}

@defproc[(media-ended? [media external?]) boolean?]{
@(mdn-bar "HTMLMediaElement: ended property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/ended")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Reports whether playback has reached the end.
}

@defproc[(media-paused? [media external?]) boolean?]{
@(mdn-bar "HTMLMediaElement: paused property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/paused")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Reports whether playback is paused.
}

@defproc[(media-media-group [media external?]) string?]{
@(mdn-bar "HTMLMediaElement: mediaGroup property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/mediaGroup")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Returns the media group identifier.
}

@defproc[(media-set-media-group! [media external?]
                                 [media-group (or/c string? symbol?)]) void?]{
@(mdn-bar "HTMLMediaElement: mediaGroup property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/mediaGroup")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Symbols are normalized to strings
before the media group is updated.
}

@defproc[(media-disable-remote-playback? [media external?]) boolean?]{
@(mdn-bar "HTMLMediaElement: disableRemotePlayback property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/disableRemotePlayback")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Reports whether remote playback is disabled.
}

@defproc[(media-set-disable-remote-playback! [media external?]
                                             [disabled boolean?]) void?]{
@(mdn-bar "HTMLMediaElement: disableRemotePlayback property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/disableRemotePlayback")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Enables or disables remote playback.
}

@defproc[(media-network-state [media external?]) symbol?]{
@(mdn-bar "HTMLMediaElement: networkState property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/networkState")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Returns the network state as a symbol.
The usual symbols are @racket['empty], @racket['idle], @racket['loading],
and @racket['no-source].
}

@defproc[(media-network-state-number [media external?]) exact-nonnegative-integer?]{
@(mdn-bar "HTMLMediaElement: networkState property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/networkState")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Returns the network state code.
}

@defproc[(media-sink-id [media external?]) string?]{
@(mdn-bar "HTMLMediaElement: sinkId property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/sinkId")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Returns the current sink id.
}

@defproc[(media-current-time [media external?]) real?]{
@(mdn-bar "HTMLMediaElement: currentTime property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/currentTime")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Returns the current playback time.
}

@defproc[(media-set-current-time! [media external?] [t real?]) void?]{
@(mdn-bar "HTMLMediaElement: currentTime property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/currentTime")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Seeks to a playback time.
}

@defproc[(media-play [media external?]) external/raw]{
@(mdn-bar "HTMLMediaElement: play() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/play")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Starts playback and returns the
browser promise.
}

@defproc[(media-src [media external?]) string?]{
@(mdn-bar "HTMLMediaElement: src property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/src")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Returns the current media source URL.
}

@defproc[(media-set-src! [media external?] [src (or/c string? symbol?)]) void?]{
@(mdn-bar "HTMLMediaElement: src property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/src")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Symbols are normalized to strings
before the media source URL is updated.
}

@defproc[(media-controls? [media external?]) boolean?]{
@(mdn-bar "HTMLMediaElement: controls property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/controls")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Reports whether browser controls are shown.
}

@defproc[(media-set-controls! [media external?] [enabled any/c]) void?]{
@(mdn-bar "HTMLMediaElement: controls property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/controls")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Shows or hides the browser's media controls.
}

@defproc[(media-preload [media external?]) string?]{
@(mdn-bar "HTMLMediaElement: preload property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/preload")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Returns the preload hint.
}

@defproc[(media-set-preload! [media external?] [preload (or/c string? symbol?)]) void?]{
@(mdn-bar "HTMLMediaElement: preload property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/preload")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Symbols are normalized to strings
before the browser property is updated.
}

@defproc[(media-can-play-type [media external?] [type (or/c string? symbol?)]) string?]{
@(mdn-bar "HTMLMediaElement: canPlayType() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/canPlayType")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Symbols are normalized to strings
before the browser is asked about codec support.
}

@defproc[(media-controls-list [media external?]) dom-token-list?]{
@(mdn-bar "HTMLMediaElement: controlsList property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/controlsList")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Returns the wrapped browser
@racketid[DOMTokenList] controls policy object.
}

@defstruct[media-keys-info ([raw external/raw])]{
Wraps a browser @racketid[MediaKeys] value.
}

@defproc[(media-keys [media external?]) (or/c #f media-keys-info?)]{
@(mdn-bar "HTMLMediaElement: mediaKeys property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/mediaKeys")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Returns the wrapped browser
@racketid[MediaKeys] object, or @racket[#f] when none is attached.
}

@defproc[(media-set-media-keys! [media external?] [keys (or/c media-keys-info? external/raw)]) external/raw]{
@(mdn-bar "HTMLMediaElement: setMediaKeys() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/setMediaKeys")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. The @racket[keys] argument can be a
wrapped @racketid[MediaKeys] object or a raw browser object.
Returns the browser promise from the underlying call.
}

@defstruct[media-source-info ([raw external/raw])]{
Wraps a browser @racketid[MediaSource] value.
}

@defproc[(media-src-object [media external?]) (or/c #f media-stream? media-source-info?)]{
@(mdn-bar "HTMLMediaElement: srcObject property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/srcObject")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Returns a wrapped browser
@racketid[MediaStream] or @racketid[MediaSource] value, or @racket[#f]
when the source object is empty.
}

@defproc[(media-set-src-object! [media external?]
                                [src-object (or/c #f media-stream? media-source-info? external/raw)])
         void?]{
@(mdn-bar "HTMLMediaElement: srcObject property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/srcObject")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. The source object can be a wrapped
@racketid[MediaStream], a wrapped @racketid[MediaSource], a raw browser
object, or @racket[#f].
}

@defproc[(media-add-text-track! [media external?]
                                [kind (or/c string? symbol?)]
                                [label (or/c string? symbol? #f) #f]
                                [language (or/c string? symbol? #f) #f])
         text-track?]{
@(mdn-bar "HTMLMediaElement: addTextTrack() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/addTextTrack")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. The @racket[kind] argument accepts
strings or symbols, and @racket[label] / @racket[language] are optional
string-like values. Use @racket[#f] to omit an optional argument.
Returns a wrapped browser @racketid[TextTrack] value.
}

@defstruct[media-stream ([raw external/raw])]{
Wraps a browser @racketid[MediaStream] value.
}

@defproc[(media-capture-stream [media external?] [frame-rate any/c #f])
         media-stream?]{
@(mdn-bar "HTMLMediaElement: captureStream() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/captureStream")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Use @racket[#f] to omit the optional
frame rate. Returns a wrapped browser @racketid[MediaStream] that
captures the media element.
}

@defstruct[media-error-info ([raw external/raw])]{
Wraps a browser @racketid[MediaError] value.
}

@defproc[(media-error [media external?]) (or/c #f media-error-info?)]{
@(mdn-bar "HTMLMediaElement: error property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/error")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Returns a wrapped browser error
object when one is present.
}

@defproc[(media-error-info-code [err media-error-info?]) exact-nonnegative-integer?]{
Returns the browser error code.
}

@defproc[(media-error-info-message [err media-error-info?]) string?]{
Returns the browser error message.
}

@defstruct[audio-track-list ([raw external/raw])]{
Wraps a browser @racketid[AudioTrackList] value.
}

@defproc[(audio-track-list-length [tracks audio-track-list?]) exact-nonnegative-integer?]{
Returns the number of audio tracks.
}

@defproc[(audio-track-list-item [tracks audio-track-list?] [index exact-nonnegative-integer?])
         (or/c #f audio-track?)]{
Returns the wrapped browser @racketid[AudioTrack] at @racket[index] or
@racket[#f].
}

@defstruct[audio-track ([raw external/raw])]{
Wraps a browser @racketid[AudioTrack] value.
}

@defproc[(audio-track-kind [track audio-track?]) string?]{
Returns the track kind.
}

@defproc[(audio-track-label [track audio-track?]) string?]{
Returns the track label.
}

@defproc[(audio-track-language [track audio-track?]) string?]{
Returns the track language.
}

@defproc[(audio-track-id [track audio-track?]) string?]{
Returns the track id.
}

@defproc[(audio-track-enabled? [track audio-track?]) boolean?]{
Returns whether the audio track is enabled.
}

@defproc[(audio-track-set-enabled! [track audio-track?] [enabled boolean?]) void?]{
Sets whether the audio track is enabled.
}

@defstruct[text-track-list ([raw external/raw])]{
Wraps a browser @racketid[TextTrackList] value.
}

@defproc[(text-track-list-length [tracks text-track-list?]) exact-nonnegative-integer?]{
Returns the number of text tracks.
}

@defproc[(text-track-list-item [tracks text-track-list?] [index exact-nonnegative-integer?])
         (or/c #f text-track?)]{
Returns the wrapped browser @racketid[TextTrack] at @racket[index] or
@racket[#f].
}

@defstruct[text-track ([raw external/raw])]{
Wraps a browser @racketid[TextTrack] value.
}

@defproc[(text-track-kind [track text-track?]) string?]{
Returns the track kind.
}

@defproc[(text-track-label [track text-track?]) string?]{
Returns the track label.
}

@defproc[(text-track-language [track text-track?]) string?]{
Returns the track language.
}

@defproc[(text-track-id [track text-track?]) string?]{
Returns the track id.
}

@defproc[(text-track-mode [track text-track?]) string?]{
Returns the current track mode.
}

@defproc[(text-track-set-mode! [track text-track?] [mode (or/c string? symbol?)]) void?]{
Sets the track mode. Symbols are normalized to strings.
}

@defstruct[video-track-list ([raw external/raw])]{
Wraps a browser @racketid[VideoTrackList] value.
}

@defproc[(video-track-list-length [tracks video-track-list?]) exact-nonnegative-integer?]{
Returns the number of video tracks.
}

@defproc[(video-track-list-item [tracks video-track-list?] [index exact-nonnegative-integer?])
         (or/c #f video-track?)]{
Returns the wrapped browser @racketid[VideoTrack] at @racket[index] or
@racket[#f].
}

@defstruct[video-track ([raw external/raw])]{
Wraps a browser @racketid[VideoTrack] value.
}

@defproc[(video-track-kind [track video-track?]) string?]{
Returns the track kind.
}

@defproc[(video-track-label [track video-track?]) string?]{
Returns the track label.
}

@defproc[(video-track-language [track video-track?]) string?]{
Returns the track language.
}

@defproc[(video-track-id [track video-track?]) string?]{
Returns the track id.
}

@defproc[(video-track-selected? [track video-track?]) boolean?]{
Returns whether the video track is selected.
}

@defstruct[time-ranges ([raw external/raw])]{
Wraps a browser @racketid[TimeRanges] value.
}

@defproc[(time-ranges-length [ranges time-ranges?]) exact-nonnegative-integer?]{
Returns the number of time ranges.
}

@defproc[(time-ranges-start [ranges time-ranges?] [index exact-nonnegative-integer?]) real?]{
Returns the start time for a range.
}

@defproc[(time-ranges-end [ranges time-ranges?] [index exact-nonnegative-integer?]) real?]{
Returns the end time for a range.
}

@defproc[(media-audio-tracks [media external?]) (or/c #f audio-track-list?)]{
@(mdn-bar "HTMLMediaElement: audioTracks property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/audioTracks")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Returns the wrapped browser audio
track list.
}

@defproc[(media-buffered [media external?]) time-ranges?]{
@(mdn-bar "HTMLMediaElement: buffered property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/buffered")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Returns the wrapped buffered time
ranges.
}

@defproc[(media-played [media external?]) time-ranges?]{
@(mdn-bar "HTMLMediaElement: played property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/played")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Returns the wrapped played time
ranges.
}

@defproc[(media-seekable [media external?]) time-ranges?]{
@(mdn-bar "HTMLMediaElement: seekable property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/seekable")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Returns the wrapped seekable time
ranges.
}

@defproc[(media-text-tracks [media external?]) (or/c #f text-track-list?)]{
@(mdn-bar "HTMLMediaElement: textTracks property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/textTracks")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Returns the wrapped browser text
track list.
}

@defproc[(media-video-tracks [media external?]) (or/c #f video-track-list?)]{
@(mdn-bar "HTMLMediaElement: videoTracks property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/videoTracks")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Returns the wrapped browser video
track list.
}

@defproc[(media-set-sink-id! [media external?] [sink-id (or/c string? symbol?)]) external/raw]{
@(mdn-bar "HTMLMediaElement: setSinkId() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/setSinkId")
The raw @racket[media] argument should be a browser
@racketid[HTMLMediaElement] value. Symbols are normalized to strings
before the browser sink is updated.
}
