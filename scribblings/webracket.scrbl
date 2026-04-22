#lang scribble/manual

@title{WebRacket Manual}
@table-of-contents[]


@include-section["Introduction.scrbl"]

@include-section["getting-started.scrbl"]

@include-section["Installation.scrbl"]

@include-section["command-line-tool.scrbl"]

@include-section["webracket-for-racket-programmers.scrbl"]

@include-section["console-bridge.scrbl"]

@include-section["Callback_Source_Map_Tool.scrbl"]


@;-------------------------------------------------------------------

@section{Libraries}

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
  @item{@racket[(include-lib jsx-graph)]}
  @item{@racket[(include-lib rquery)]}
  @item{@racket[(include-lib simple-pretty)]}
  @item{@racket[(include-lib sxml)]}
  @item{@racket[(include-lib web-easy)]}
]

@include-section["lib-array.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-iterator.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-jsx-graph.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-rquery.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-simple-pretty.scrbl"]

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
  @item{@racket[(include-lib document)]}
  @item{@racket[(include-lib dom)]}
  @item{@racket[(include-lib domrect)]}
  @item{@racket[(include-lib element)]}
  @item{@racket[(include-lib event)]}
  @item{@racket[(include-lib fetch)]}
  @item{@racket[(include-lib image)]}
  @item{@racket[(include-lib indexed-db)]}
  @item{@racket[(include-lib media)]}
  @item{@racket[(include-lib performance)]}
  @item{@racket[(include-lib storage)]}
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

@include-section["lib-document.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-dom.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-domrect.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-element.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-event.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-fetch.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-image.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-indexed-db.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-media.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-performance.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-storage.scrbl"]

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
