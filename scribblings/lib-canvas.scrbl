#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/lib-canvas-labels.rkt" "webracket"))
          (for-label (lib "scribblings/lib-document-labels.rkt" "webracket"))
          (for-label (lib "scribblings/lib-element-labels.rkt" "webracket"))
          (for-label (lib "scribblings/lib-media-labels.rkt" "webracket"))
          )

@title{Library: @racketid[canvas]}
@declare-exporting[(lib "scribblings/lib-canvas-labels.rkt" "webracket")]

@(how-to-require include-lib canvas (lib "libs/canvas.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @racket[canvas] library wraps HTML canvas elements and their 2D
drawing contexts in checked values.

Canvas lets you draw pixels with shapes, text, and images. Use it when
you want to paint directly on the page instead of building the
interface out of HTML elements.

Use @racket[canvas] when you want to:

@itemlist[
  @item{create or inspect a canvas element}
  @item{get a 2D rendering context}
  @item{draw rectangles or text}
  @item{clear or reshape the canvas surface}
]

The library keeps canvas elements, 2D contexts, and the richer values
they produce on the WebRacket side.

@section{Canvas Quick Start}

Start by creating a canvas element, getting its 2D context, and drawing
a filled rectangle.

@racketblock[
(code:comment "Include the document and canvas wrapper libraries.")
(include-lib document)
(include-lib canvas)

(code:comment "Create a new canvas element.")
(define canvas-el (document-create-element "canvas"))

(code:comment "Get the 2D drawing context from the canvas.")
(define ctx (canvas-get-context canvas-el '2d #f))

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
(define canvas-el (document-create-element "canvas"))
(set-attribute! canvas-el "width" "240")
(set-attribute! canvas-el "height" "120")

(code:comment "Insert the canvas into the page body.")
(append-child! (document-body) canvas-el)

(code:comment "Get the 2D drawing context and paint a box.")
(define ctx (canvas-get-context canvas-el '2d #f))
(canvas-2d-fill-rect ctx 20 20 80 40)
]

The main values are @racket[canvas?] and @racket[canvas-2d-context?].
Use @racket[canvas-raw] or @racket[canvas-2d-context-raw] only when you
need to drop back to the browser object.

String-like arguments such as context identifiers and drawing modes
accept either strings or symbols. Optional arguments use @racket[#f]
to mean that the argument is omitted.

@section{Canvas Values}

@defstruct[canvas ([raw external/raw])]{
Wraps a browser @tt{HTMLCanvasElement} object.
}

@defproc[(canvas-raw [canvas canvas?]) external/raw]{
Returns the wrapped browser @tt{HTMLCanvasElement} object.
}

@defstruct[canvas-2d-context ([raw external/raw])]{
Wraps a browser @tt{CanvasRenderingContext2D} object.
}

@defproc[(canvas-2d-context-raw [ctx canvas-2d-context?]) external/raw]{
Returns the wrapped browser @tt{CanvasRenderingContext2D} object.
}

@defstruct[canvas-image-data ([raw external/raw])]{
Wraps a browser @tt{ImageData} object.
}

@defproc[(canvas-image-data-raw [data canvas-image-data?]) external/raw]{
Returns the wrapped browser @tt{ImageData} object.
}

@defstruct[canvas-gradient ([raw external/raw])]{
Wraps a browser @tt{CanvasGradient} object.
}

@defproc[(canvas-gradient-raw [gradient canvas-gradient?]) external/raw]{
Returns the wrapped browser @tt{CanvasGradient} object.
}

@defstruct[canvas-pattern ([raw external/raw])]{
Wraps a browser @tt{CanvasPattern} object.
}

@defproc[(canvas-pattern-raw [pattern canvas-pattern?]) external/raw]{
Returns the wrapped browser @tt{CanvasPattern} object.
}

@defstruct[canvas-text-metrics ([raw external/raw])]{
Wraps a browser @tt{TextMetrics} object.
}

@defproc[(canvas-text-metrics-raw [metrics canvas-text-metrics?]) external/raw]{
Returns the wrapped browser @tt{TextMetrics} object.
}

@defstruct[canvas-dom-matrix ([raw external/raw])]{
Wraps a browser @tt{DOMMatrix} object.
}

@defproc[(canvas-dom-matrix-raw [matrix canvas-dom-matrix?]) external/raw]{
Returns the wrapped browser @tt{DOMMatrix} object.
}

@defstruct[offscreen-canvas ([raw external/raw])]{
Wraps a browser @tt{OffscreenCanvas} object.
}

@defproc[(offscreen-canvas-raw [canvas offscreen-canvas?]) external/raw]{
Returns the wrapped browser @tt{OffscreenCanvas} object.
}

If you are just getting started, the most useful entry points are
@racket[canvas-get-context], @racket[canvas-width], and
@racket[canvas-2d-fill-rect].

@defproc[(canvas-get-context [canvas (or/c canvas? external?)]
                             [context-id (or/c string? symbol?)]
                             [options any/c #f])
         (or/c #f canvas-2d-context?)]{
@(mdn-bar "HTMLCanvasElement: getContext() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/getContext")
The @racket[canvas] argument should be a wrapped canvas value or a
browser @racketid[HTMLCanvasElement] value. Returns the requested
drawing context.
}

@defproc[(canvas-width [canvas (or/c canvas? external?)]) exact-nonnegative-integer?]{
@(mdn-bar "HTMLCanvasElement: width property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/width")
The @racket[canvas] argument should be a wrapped canvas value or a
browser @racketid[HTMLCanvasElement] value. Returns the canvas width in
CSS pixels.
}

@defproc[(canvas-2d-fill-rect [ctx (or/c canvas-2d-context? external?)]
                              [x real?] [y real?] [w real?] [h real?]) void?]{
@(mdn-bar "CanvasRenderingContext2D: fillRect() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/fillRect")
The @racket[ctx] argument should be a wrapped context value or a
browser @racketid[CanvasRenderingContext2D] value. Fills a rectangle in
a 2D canvas context.
}

@defproc[(canvas-capture-stream [canvas (or/c canvas? external?)] [frame-rate any/c #f])
         media-stream?]{
@(mdn-bar "HTMLCanvasElement: captureStream() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/captureStream")
The @racket[canvas] argument should be a wrapped canvas value or a
browser @racketid[HTMLCanvasElement] value. Returns a wrapped browser
@racketid[MediaStream] that captures the canvas rendering.
}

@section{Canvas Value Wrappers}

The canvas library now also wraps the browser values that its richer 2D
methods produce. The most useful accessors are:

@defproc[(canvas-image-data-width [data canvas-image-data?]) exact-nonnegative-integer?]{
Reads the width of an image buffer.
}

@defproc[(canvas-image-data-height [data canvas-image-data?]) exact-nonnegative-integer?]{
Reads the height of an image buffer.
}

@defproc[(canvas-image-data-data [data canvas-image-data?]) bytes?]{
Reads the pixel bytes of an image buffer as a Racket @racket[bytes]
value.
}

@defproc[(canvas-text-metrics-width [metrics canvas-text-metrics?]) real?]{
Reads the measured text width.
}

@defproc[(canvas-dom-matrix-a [matrix canvas-dom-matrix?]) real?]{
Reads the @racket[a] component of a canvas transform matrix.
}

@defproc[(canvas-dom-matrix-b [matrix canvas-dom-matrix?]) real?]{
Reads the @racket[b] component of a canvas transform matrix.
}

@defproc[(canvas-dom-matrix-c [matrix canvas-dom-matrix?]) real?]{
Reads the @racket[c] component of a canvas transform matrix.
}

@defproc[(canvas-dom-matrix-d [matrix canvas-dom-matrix?]) real?]{
Reads the @racket[d] component of a canvas transform matrix.
}

@defproc[(canvas-dom-matrix-e [matrix canvas-dom-matrix?]) real?]{
Reads the @racket[e] component of a canvas transform matrix.
}

@defproc[(canvas-dom-matrix-f [matrix canvas-dom-matrix?]) real?]{
Reads the @racket[f] component of a canvas transform matrix.
}

@defproc[(canvas-gradient-add-color-stop! [gradient (or/c canvas-gradient? external?)]
                                          [offset real?]
                                          [color (or/c string? symbol?)])
         void?]{
Adds a stop to a gradient.
}

@defproc[(canvas-pattern-set-transform! [pattern (or/c canvas-pattern? external?)]
                                        [matrix (or/c canvas-dom-matrix? external?)])
         void?]{
Sets the transform used when tiling a canvas pattern.
}

@defproc[(canvas-2d-filter [ctx (or/c canvas-2d-context? external?)]) string?]{
Returns the current filter string for the 2D context.
}

@defproc[(canvas-2d-set-filter! [ctx (or/c canvas-2d-context? external?)]
                                [filter (or/c string? symbol?)])
         void?]{
Sets the filter string for the 2D context.
}

@defproc[(canvas-2d-line-join [ctx (or/c canvas-2d-context? external?)]) string?]{
Returns the current line-join setting for the 2D context.
}

@defproc[(canvas-2d-set-line-join! [ctx (or/c canvas-2d-context? external?)]
                                   [value (or/c string? symbol?)])
         void?]{
Sets the line-join setting for the 2D context.
}

@section{Canvas 2D State and Geometry}

The 2D context now also exposes the usual MDN state and drawing helpers,
including @racket[canvas-2d-fill-style], @racket[canvas-2d-stroke-style],
@racket[canvas-2d-filter], @racket[canvas-2d-font],
@racket[canvas-2d-global-alpha],
@racket[canvas-2d-global-composite-operation],
@racket[canvas-2d-image-smoothing-enabled?],
@racket[canvas-2d-image-smoothing-quality],
@racket[canvas-2d-line-cap], @racket[canvas-2d-line-dash-offset],
@racket[canvas-2d-line-join], @racket[canvas-2d-line-width],
@racket[canvas-2d-miter-limit], @racket[canvas-2d-shadow-blur],
@racket[canvas-2d-shadow-color], @racket[canvas-2d-shadow-offset-x],
@racket[canvas-2d-shadow-offset-y], @racket[canvas-2d-text-align],
@racket[canvas-2d-text-baseline], @racket[canvas-2d-text-rendering],
@racket[canvas-2d-font-kerning], @racket[canvas-2d-font-stretch],
@racket[canvas-2d-font-variant-caps],
@racket[canvas-2d-font-variant-ligatures],
@racket[canvas-2d-font-variant-numeric],
@racket[canvas-2d-letter-spacing], @racket[canvas-2d-word-spacing],
@racket[canvas-2d-arc-to], @racket[canvas-2d-bezier-curve-to],
@racket[canvas-2d-clip], @racket[canvas-2d-create-image-data],
@racket[canvas-2d-create-image-data-from], @racket[canvas-2d-create-pattern],
@racket[canvas-2d-create-linear-gradient],
@racket[canvas-2d-create-radial-gradient],
@racket[canvas-2d-create-conic-gradient],
@racket[canvas-2d-draw-focus-if-needed!],
@racket[canvas-2d-draw-focus-if-needed-path!], @racket[canvas-2d-draw-image],
@racket[canvas-2d-ellipse], @racket[canvas-2d-get-image-data],
@racket[canvas-2d-get-line-dash], @racket[canvas-2d-get-transform],
@racket[canvas-2d-is-point-in-path], @racket[canvas-2d-is-point-in-stroke],
@racket[canvas-2d-put-image-data], @racket[canvas-2d-quadratic-curve-to],
@racket[canvas-2d-rect], @racket[canvas-2d-reset],
@racket[canvas-2d-reset-transform], @racket[canvas-2d-round-rect],
@racket[canvas-2d-set-line-dash], @racket[canvas-2d-set-transform!],
@racket[canvas-2d-set-transform-matrix!], and @racket[canvas-2d-transform].

@defproc[(canvas-2d-filter [ctx (or/c canvas-2d-context? external?)]) string?]{
@(mdn-bar "CanvasRenderingContext2D: filter property"
          "https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/filter")
Returns the current filter string for the 2D context.
}

@defproc[(canvas-2d-line-join [ctx (or/c canvas-2d-context? external?)]) string?]{
@(mdn-bar "CanvasRenderingContext2D: lineJoin property"
          "https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/lineJoin")
Returns the current line-join setting for the 2D context.
}

@defproc[(canvas-2d-get-transform [ctx (or/c canvas-2d-context? external?)])
         canvas-dom-matrix?]{
@(mdn-bar "CanvasRenderingContext2D: getTransform() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/getTransform")
Returns the current transform matrix as a wrapped @racket[canvas-dom-matrix]
value.
}

@defproc[(canvas-2d-reset [ctx (or/c canvas-2d-context? external?)]) void?]{
@(mdn-bar "CanvasRenderingContext2D: reset() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/reset")
Resets the 2D context to its default drawing state.
}

@section{Canvas 2D Extras}

The remaining non-deprecated 2D helpers are documented here so the
canvas family has a single canonical home for its public API surface.

@defproc[(canvas-2d-global-alpha [ctx (or/c canvas-2d-context? external?)]) real?]{
Reads the alpha multiplier used for compositing.
}

@defproc[(canvas-2d-font [ctx (or/c canvas-2d-context? external?)]) string?]{
Reads the current font shorthand.
}

@defproc[(canvas-2d-global-composite-operation [ctx (or/c canvas-2d-context? external?)]) string?]{
Reads the current compositing mode.
}

@defproc[(canvas-2d-image-smoothing-enabled? [ctx (or/c canvas-2d-context? external?)]) boolean?]{
Checks whether image smoothing is enabled.
}

@defproc[(canvas-2d-fill-style [ctx (or/c canvas-2d-context? external?)]) any/c]{
Reads the current fill style.
}

@defproc[(canvas-2d-stroke-style [ctx (or/c canvas-2d-context? external?)]) any/c]{
Reads the current stroke style.
}

@defproc[(canvas-2d-image-smoothing-quality [ctx (or/c canvas-2d-context? external?)]) string?]{
Reads the smoothing quality hint.
}

@defproc[(canvas-2d-line-cap [ctx (or/c canvas-2d-context? external?)]) string?]{
Reads the line cap setting.
}

@defproc[(canvas-2d-line-dash-offset [ctx (or/c canvas-2d-context? external?)]) real?]{
Reads the current dash offset.
}

@defproc[(canvas-2d-line-width [ctx (or/c canvas-2d-context? external?)]) real?]{
Reads the current stroke width.
}

@defproc[(canvas-2d-miter-limit [ctx (or/c canvas-2d-context? external?)]) real?]{
Reads the current miter limit.
}

@defproc[(canvas-2d-shadow-blur [ctx (or/c canvas-2d-context? external?)]) real?]{
Reads the shadow blur radius.
}

@defproc[(canvas-2d-shadow-color [ctx (or/c canvas-2d-context? external?)]) string?]{
Reads the shadow color.
}

@defproc[(canvas-2d-shadow-offset-x [ctx (or/c canvas-2d-context? external?)]) real?]{
Reads the shadow X offset.
}

@defproc[(canvas-2d-shadow-offset-y [ctx (or/c canvas-2d-context? external?)]) real?]{
Reads the shadow Y offset.
}

@defproc[(canvas-2d-text-align [ctx (or/c canvas-2d-context? external?)]) string?]{
Reads the current text alignment.
}

@defproc[(canvas-2d-text-baseline [ctx (or/c canvas-2d-context? external?)]) string?]{
Reads the current text baseline.
}

@defproc[(canvas-2d-text-rendering [ctx (or/c canvas-2d-context? external?)]) string?]{
Reads the current text rendering hint.
}

@defproc[(canvas-2d-font-kerning [ctx (or/c canvas-2d-context? external?)]) string?]{
Reads the font kerning mode.
}

@defproc[(canvas-2d-font-stretch [ctx (or/c canvas-2d-context? external?)]) string?]{
Reads the font stretch setting.
}

@defproc[(canvas-2d-font-variant-caps [ctx (or/c canvas-2d-context? external?)]) string?]{
Reads the font variant caps setting.
}

@defproc[(canvas-2d-font-variant-ligatures [ctx (or/c canvas-2d-context? external?)]) string?]{
Reads the font variant ligatures setting.
}

@defproc[(canvas-2d-font-variant-numeric [ctx (or/c canvas-2d-context? external?)]) string?]{
Reads the font variant numeric setting.
}

@defproc[(canvas-2d-letter-spacing [ctx (or/c canvas-2d-context? external?)]) string?]{
Reads the current letter spacing.
}

@defproc[(canvas-2d-word-spacing [ctx (or/c canvas-2d-context? external?)]) string?]{
Reads the current word spacing.
}

@defproc[(canvas-2d-arc-to [ctx (or/c canvas-2d-context? external?)]
                           [x1 real?]
                           [y1 real?]
                           [x2 real?]
                           [y2 real?]
                           [radius real?])
         void?]{
Adds an arc between two tangent lines.
}

@defproc[(canvas-2d-bezier-curve-to [ctx (or/c canvas-2d-context? external?)]
                                    [cp1x real?]
                                    [cp1y real?]
                                    [cp2x real?]
                                    [cp2y real?]
                                    [x real?]
                                    [y real?])
         void?]{
Adds a cubic Bézier curve to the current path.
}

@defproc[(canvas-2d-clip [ctx (or/c canvas-2d-context? external?)]
                         [path any/c]
                         [fill-rule any/c])
         void?]{
Clips the canvas to the current path.
}

@defproc[(canvas-2d-create-image-data [ctx (or/c canvas-2d-context? external?)]
                                      [width exact-nonnegative-integer?]
                                      [height exact-nonnegative-integer?])
         canvas-image-data?]{
Creates a fresh image-data buffer.
}

@defproc[(canvas-2d-create-image-data-from [ctx (or/c canvas-2d-context? external?)]
                                          [data (or/c canvas-image-data? external?)])
         canvas-image-data?]{
Creates image data from an existing buffer.
}

@defproc[(canvas-2d-create-pattern [ctx (or/c canvas-2d-context? external?)]
                                   [source any/c]
                                   [repetition (or/c string? symbol?)])
         (or/c #f canvas-pattern?)]{
Creates a tiling pattern from an image source.
}

@defproc[(canvas-2d-create-linear-gradient [ctx (or/c canvas-2d-context? external?)]
                                           [x0 real?]
                                           [y0 real?]
                                           [x1 real?]
                                           [y1 real?])
         canvas-gradient?]{
Creates a linear gradient.
}

@defproc[(canvas-2d-create-radial-gradient [ctx (or/c canvas-2d-context? external?)]
                                           [x0 real?]
                                           [y0 real?]
                                           [r0 real?]
                                           [x1 real?]
                                           [y1 real?]
                                           [r1 real?])
         canvas-gradient?]{
Creates a radial gradient.
}

@defproc[(canvas-2d-create-conic-gradient [ctx (or/c canvas-2d-context? external?)]
                                          [angle real?]
                                          [x real?]
                                          [y real?])
         canvas-gradient?]{
Creates a conic gradient.
}

@defproc[(canvas-2d-draw-focus-if-needed! [ctx (or/c canvas-2d-context? external?)]
                                          [element any/c])
         void?]{
Draws a focus ring when the browser decides it is needed.
}

@defproc[(canvas-2d-draw-focus-if-needed-path! [ctx (or/c canvas-2d-context? external?)]
                                               [path any/c]
                                               [element any/c])
         void?]{
Draws a focus ring for the given path and element.
}

@defproc[(canvas-2d-draw-image [ctx (or/c canvas-2d-context? external?)]
                               [source any/c]
                               [dx real?]
                               [dy real?])
         void?]{
Draws an image source at a point.
}

@defproc[(canvas-2d-ellipse [ctx (or/c canvas-2d-context? external?)]
                            [x real?]
                            [y real?]
                            [rx real?]
                            [ry real?]
                            [rotation real?]
                            [start-angle real?]
                            [end-angle real?]
                            [anticlockwise any/c])
         void?]{
Adds an ellipse to the current path.
}

@defproc[(canvas-2d-get-image-data [ctx (or/c canvas-2d-context? external?)]
                                   [sx real?]
                                   [sy real?]
                                   [sw real?]
                                   [sh real?]
                                   [settings any/c])
         canvas-image-data?]{
Reads pixels from the canvas.
}

@defproc[(canvas-2d-get-line-dash [ctx (or/c canvas-2d-context? external?)]) vector?]{
Reads the current dash pattern.
}

@defproc[(canvas-2d-is-point-in-path [ctx (or/c canvas-2d-context? external?)]
                                     [path any/c]
                                     [x real?]
                                     [y real?]
                                     [fill-rule any/c])
         boolean?]{
Checks whether a point lies inside the current path.
}

@defproc[(canvas-2d-is-point-in-stroke [ctx (or/c canvas-2d-context? external?)]
                                       [path any/c]
                                       [x real?]
                                       [y real?])
         boolean?]{
Checks whether a point lies inside the current stroke.
}

@defproc[(canvas-2d-put-image-data [ctx (or/c canvas-2d-context? external?)]
                                   [data (or/c canvas-image-data? external?)]
                                   [dx real?]
                                   [dy real?]
                                   [dirty-x any/c]
                                   [dirty-y any/c]
                                   [dirty-width any/c]
                                   [dirty-height any/c])
         void?]{
Writes pixels back to the canvas.
}

@defproc[(canvas-2d-quadratic-curve-to [ctx (or/c canvas-2d-context? external?)]
                                       [cpx real?]
                                       [cpy real?]
                                       [x real?]
                                       [y real?])
         void?]{
Adds a quadratic Bézier curve to the current path.
}

@defproc[(canvas-2d-rect [ctx (or/c canvas-2d-context? external?)]
                         [x real?]
                         [y real?]
                         [width real?]
                         [height real?])
         void?]{
Adds a rectangle to the current path.
}

@defproc[(canvas-2d-round-rect [ctx (or/c canvas-2d-context? external?)]
                               [x real?]
                               [y real?]
                               [width real?]
                               [height real?]
                               [radii any/c])
         void?]{
Adds a rounded rectangle to the current path.
}

@defproc[(canvas-2d-set-line-dash [ctx (or/c canvas-2d-context? external?)]
                                  [segments any/c])
         void?]{
Sets the dash pattern.
}

@defproc[(canvas-2d-set-transform! [ctx (or/c canvas-2d-context? external?)]
                                   [a real?]
                                   [b real?]
                                   [c real?]
                                   [d real?]
                                   [e real?]
                                   [f real?])
         void?]{
Sets the transform matrix from six numbers.
}

@defproc[(canvas-2d-set-transform-matrix! [ctx (or/c canvas-2d-context? external?)]
                                          [matrix (or/c canvas-dom-matrix? external?)])
         void?]{
Sets the transform matrix from a DOMMatrix.
}

@defproc[(canvas-2d-transform [ctx (or/c canvas-2d-context? external?)]
                              [a real?]
                              [b real?]
                              [c real?]
                              [d real?]
                              [e real?]
                              [f real?])
         void?]{
Applies an affine transform to the current matrix.
}

@defproc[(canvas-2d-reset-transform [ctx (or/c canvas-2d-context? external?)]) void?]{
Resets the transform matrix to the identity matrix.
}

For the 2D canvas API, string-like arguments continue to accept strings
or symbols, and @racket[#f] continues to mean that an optional argument
is omitted.
