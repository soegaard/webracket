#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/dom-family-labels.rkt" "webracket")))

@title{Library: @racketid[domrect]}
@declare-exporting[(lib "scribblings/dom-family-labels.rkt" "webracket")]

@(how-to-require include-lib domrect (lib "libs/domrect.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @racket[domrect] library is the checked wrapper for rectangle
geometry values coming back from browser layout APIs.

A DOMRect describes the position and size of a box on the page. It is
useful when you want to know where something appears on screen and how
wide or tall it is.

Use @racket[domrect] when you want to:

@itemlist[
  @item{read the left and top coordinates of a rectangle}
  @item{read its width and height}
  @item{work with geometry returned by browser layout calls}
]

The @racket[domrect] library provides checked accessors for DOMRect
geometry values.

@section{DOMRect Quick Start}

Start by taking a rectangle returned by another DOM helper and reading
its basic measurements.

@racketblock[
(code:comment "Include the DOM, element, and DOMRect wrapper libraries.")
(include-lib document)
(include-lib element)
(include-lib domrect)

(code:comment "Find an element whose rectangle we want to inspect.")
(define card
  (document-query-selector ".card"))

(code:comment "Ask the browser for the rectangle around that element.")
(define rect
  (get-bounding-client-rect card))

(code:comment "Read the rectangle's position and size.")
(list (dom-rect-left rect)
      (dom-rect-top rect)
      (dom-rect-width rect)
      (dom-rect-height rect))
]

The quick start shows the basic pattern: keep the rectangle, then read
the coordinates you need.

@section{DOMRect Example}

This example shows the normal flow: find an element, ask the browser for
its bounding rectangle, and then read the measurements with the
@racket[domrect] helpers.

@racketblock[
(code:comment "Include the DOM and rectangle libraries.")
(include-lib document)
(include-lib element)
(include-lib domrect)

(code:comment "Find a page element whose geometry we want to inspect.")
(define card
  (document-query-selector ".card"))

(code:comment "Ask the browser for the rectangle around that element.")
(define rect
  (get-bounding-client-rect card))

(code:comment "Read the rectangle values in a Racket-friendly way.")
(list (dom-rect-left rect)
      (dom-rect-top rect)
      (dom-rect-width rect)
      (dom-rect-height rect))
]

If all you need is geometry, the main entry points are
@racket[dom-rect-left], @racket[dom-rect-top], @racket[dom-rect-width],
and @racket[dom-rect-height].

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
