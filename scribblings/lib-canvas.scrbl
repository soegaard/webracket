#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/dom-family-labels.rkt" "webracket")))

@title{Library: @racketid[canvas]}
@declare-exporting[(lib "scribblings/dom-family-labels.rkt" "webracket")]

@(how-to-require include-lib canvas (lib "libs/canvas.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @racket[canvas] library is the checked wrapper for drawing on an
HTML canvas.

Canvas is the browser API for painting pixels with shapes, text, and
images. It is useful when you want to draw directly on the page instead
of building the interface out of HTML elements.

Use @racket[canvas] when you want to:

@itemlist[
  @item{create or inspect a canvas element}
  @item{get a 2D rendering context}
  @item{draw rectangles or text}
  @item{clear or reshape the canvas surface}
]

The @racket[canvas] library provides checked wrappers for canvas
elements and the 2D rendering context.

@section{Canvas Quick Start}

Start by creating a canvas element, getting its 2D context, and drawing
a filled rectangle.

@racketblock[
(code:comment "Include the document and canvas wrapper libraries.")
(include-lib document)
(include-lib canvas)

(code:comment "Create a new canvas element.")
(define canvas-el
  (document-create-element "canvas"))

(code:comment "Get the 2D drawing context from the canvas.")
(define ctx
  (canvas-get-context canvas-el "2d" (void)))

(code:comment "Draw a simple filled rectangle.")
(canvas-2d-fill-rect ctx 10 10 120 50)
]

The quick start shows the usual pattern: create a canvas, ask for a
context, and draw something simple.

@section{Canvas Example}

This example shows how to add a canvas to the page and then paint it.

@racketblock[
(code:comment "Include the libraries needed for page insertion and drawing.")
(include-lib document)
(include-lib element)
(include-lib canvas)

(code:comment "Create and size a canvas element.")
(define canvas-el
  (document-create-element "canvas"))
(set-attribute! canvas-el "width" "240")
(set-attribute! canvas-el "height" "120")

(code:comment "Insert the canvas into the page body.")
(append-child! (document-body) canvas-el)

(code:comment "Get the 2D drawing context and paint a box.")
(define ctx
  (canvas-get-context canvas-el "2d" (void)))
(canvas-2d-fill-rect ctx 20 20 80 40)
]

If you are just getting started, the most useful entry points are
@racket[canvas-get-context], @racket[canvas-width], and
@racket[canvas-2d-fill-rect].

@defproc[(canvas-get-context [canvas external?] [context-id string?] [options any/c (void)])
         (or/c #f external?)]{
@(mdn-bar "HTMLCanvasElement: getContext() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/getContext")
The raw @racket[canvas] argument should be a browser
@racketid[HTMLCanvasElement] value. Returns the requested drawing
context.
}

@defproc[(canvas-width [canvas external?]) exact-nonnegative-integer?]{
@(mdn-bar "HTMLCanvasElement: width property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/width")
The raw @racket[canvas] argument should be a browser
@racketid[HTMLCanvasElement] value. Returns the canvas width in CSS
pixels.
}

@defproc[(canvas-2d-fill-rect [ctx external?] [x real?] [y real?] [w real?] [h real?]) void?]{
@(mdn-bar "CanvasRenderingContext2D: fillRect() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/fillRect")
The raw @racket[ctx] argument should be a browser
@racketid[CanvasRenderingContext2D] value. Fills a rectangle in a 2D
canvas context.
}

@defproc[(canvas-capture-stream [canvas external?] [frame-rate any/c (void)])
         media-stream?]{
@(mdn-bar "HTMLCanvasElement: captureStream() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/captureStream")
The raw @racket[canvas] argument should be a browser
@racketid[HTMLCanvasElement] value. Returns a wrapped browser
@racketid[MediaStream] that captures the canvas rendering.
}
