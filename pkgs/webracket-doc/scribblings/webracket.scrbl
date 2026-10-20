#lang scribble/manual

@(require racket/runtime-path
          scribble/core
          scribble/html-properties
          "webracket-scribble-utils.rkt")

@(define-runtime-path webracket-layout-css "webracket-layout.css")

@title[#:style (style #f (list (make-css-style-addition webracket-layout-css)))]{WebRacket Manual}
@table-of-contents[]


@include-section["Introduction.scrbl"]

@include-section["getting-started.scrbl"]

@include-section["Installation.scrbl"]

@include-section["tools.scrbl"]

@include-section["command-line-tool.scrbl"]

@include-section["files-and-vfs.scrbl"]

@include-section["webracket-for-racket-programmers.scrbl"]

@include-section["webracket-for-racket-programmers-revised.scrbl"]

@include-section["console-bridge.scrbl"]

@include-section["console-bridge-revised.scrbl"]

@include-section["Callback_Source_Map_Tool.scrbl"]

@include-section["Callback_Source_Map_Tool-revised.scrbl"]


@;-------------------------------------------------------------------

@section{Libraries}

@(legacy-module-warning)

WebRacket libraries are grouped into three categories:

@itemlist[
  @item{@racket[(include-lib lib-id)] for libraries whose code is inserted
        textually into your program. This form can be used only at the module
        top level, and including the same library more than once has no extra
        effect.}

  @item{@racket[(require-lib lib-id)] for libraries that export syntactic forms.}

  @item{The library docs below are grouped as Racket libraries, other
        libraries, and Web APIs.}
]

@;-------------------------------------------------------------------

@section{Racket Libraries}

These are the libraries that extend Racket itself.

@itemlist[
  @item{@racket[(require-lib define)]}
  @item{@racket[(require-lib threading)]}
]

@include-section["define.scrbl"]

@;-------------------------------------------------------------------

@include-section["threading.scrbl"]

@;-------------------------------------------------------------------

@section{Other Libraries}

These libraries are not Web APIs, but they are still useful building blocks.

@itemlist[
  @item{@racket[(include-lib array)]}
  @item{@racket[(include-lib iterator)]}
  @item{@racket[(require js/promise)]}
  @item{@racket[(include-lib jsx-graph)]}
  @item{@racket[(include-lib rquery)]}
  @item{@racket[(include-lib sxml)]}
  @item{@racket[(include-lib web-easy)]}
]

@include-section["lib-array.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-js-promise.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-iterator.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-jsx-graph.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-rquery.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-sxml.scrbl"]

@;-------------------------------------------------------------------

@include-section["web-easy.scrbl"]

@;-------------------------------------------------------------------

@section{Web APIs}

These are the MDN-style browser APIs that WebRacket wraps.

@itemlist[
  @item{@racket[(include-lib audio)]}
  @item{@racket[(include-lib canvas)]}
  @item{@racket[(include-lib console)]}
  @item{@racket[(require clipboard)]}
  @item{@racket[(require browser/abort)]}
  @item{@racket[(require browser/event-target)]}
  @item{@racket[(require browser/font)]}
  @item{@racket[(require browser/range)]}
  @item{@racket[(require browser/segmenter)]}
  @item{@racket[(require compression-streams)]}
  @item{@racket[(include-lib document)]}
  @item{@racket[(require device-orientation)]}
  @item{@racket[(include-lib dom)]}
  @item{@racket[(include-lib domrect)]}
  @item{@racket[(include-lib element)]}
  @item{@racket[(include-lib event)]}
  @item{@racket[(require file-system)]}
  @item{@racket[(include-lib fetch)]}
  @item{@racket[(require fullscreen)]}
  @item{@racket[(require gamepad)]}
  @item{@racket[(require geolocation)]}
  @item{@racket[(require history)]}
  @item{@racket[(include-lib image)]}
  @item{@racket[(include-lib indexed-db)]}
  @item{@racket[(require intersection-observer)]}
  @item{@racket[(include-lib media)]}
  @item{@racket[(require notifications)]}
  @item{@racket[(include-lib performance)]}
  @item{@racket[(require resize-observer)]}
  @item{@racket[(require screen-orientation)]}
  @item{@racket[(include-lib storage)]}
  @item{@racket[(require streams)]}
  @item{@racket[(require web-midi)]}
  @item{@racket[(require web-speech)]}
  @item{@racket[(require web-workers)]}
  @item{@racket[(include-lib websocket)]}
  @item{@racket[(include-lib window)]}
]

See their respective documentation pages.

@include-section["lib-audio.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-canvas.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-console.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-clipboard.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-compression-streams.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-document.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-device-orientation.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-dom.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-domrect.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-element.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-event.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-browser-binding-foundation.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-file-system.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-fetch.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-fullscreen.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-gamepad.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-geolocation.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-history.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-image.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-indexed-db.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-intersection-observer.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-media.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-notifications.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-performance.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-resize-observer.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-screen-orientation.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-storage.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-streams.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-web-midi.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-web-speech.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-web-workers.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-websocket.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-window.scrbl"]

@;-------------------------------------------------------------------

@include-section["raw-accessors.scrbl"]

@;-------------------------------------------------------------------

@include-section["dom-family-labels.scrbl"]

@;-------------------------------------------------------------------
@include-section["special-forms.scrbl"]

@;-------------------------------------------------------------------

@include-section["implemented-primitives.scrbl"]

@;-------------------------------------------------------------------

@include-section["Browser_API.scrbl"]

@;-------------------------------------------------------------------

@include-section["appendix-smoke-tests.scrbl"]

@include-section["WebRacket_at_a_Glance.scrbl"]
