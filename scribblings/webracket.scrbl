#lang scribble/manual

@title{WebRacket Manual}
@table-of-contents[]


@include-section["Introduction.scrbl"]
@include-section["WebRacket_at_a_Glance.scrbl"]

@include-section["Installation.scrbl"]

@include-section["Command-Line_Tool.scrbl"]

@;-------------------------------------------------------------------

@section{Libraries}

WebRacket libraries come in two forms:

@itemlist[
  @item{@racket[(include-lib lib-id)] for libraries whose code is inserted
        textually into your program. This form can be used only at the module
        top level, and including the same library more than once has no extra
        effect.}
  @item{@racket[(require-lib lib-id)] for libraries that export syntactic forms.}
]

Currently available libraries include:

@itemlist[
  @item{@racket[(require-lib define)]}
  @item{@racket[(require-lib threading)]}
  @item{@racket[(include-lib dom)]}
  @item{@racket[(include-lib window)]}
  @item{@racket[(include-lib performance)]}
  @item{@racket[(include-lib document)]}
  @item{@racket[(include-lib event)]}
  @item{@racket[(include-lib domrect)]}
  @item{@racket[(include-lib element)]}
  @item{@racket[(include-lib canvas)]}
  @item{@racket[(include-lib media)]}
  @item{@racket[(include-lib image)]}
  @item{@racket[(include-lib iterator)]}
  @item{@racket[(include-lib web-easy)]}
  @item{@racket[(include-lib audio)]}
  @item{@racket[(include-lib console)]}
  @item{@racket[(include-lib websocket)]}
]

See their respective documentation pages.


@;-------------------------------------------------------------------

@include-section["define.scrbl"]

@;-------------------------------------------------------------------

@include-section["threading.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-dom.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-window.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-performance.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-document.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-event.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-domrect.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-element.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-canvas.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-media.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-image.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-iterator.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-audio.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-console.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-websocket.scrbl"]

@;-------------------------------------------------------------------

@include-section["raw-accessors.scrbl"]

@;-------------------------------------------------------------------

@include-section["web-easy.scrbl"]

@;-------------------------------------------------------------------
@include-section["special-forms.scrbl"]

@;-------------------------------------------------------------------

@include-section["implemented-primitives.scrbl"]

@;-------------------------------------------------------------------

@include-section["Browser_API.scrbl"]
