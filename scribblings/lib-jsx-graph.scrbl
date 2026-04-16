#lang scribble/manual

@(require scribble/manual
          (for-label (lib "scribblings/lib-jsx-graph-labels.rkt" "webracket"))
          "webracket-scribble-utils.rkt")

@title{Library: @racketid[jsx-graph]}
@declare-exporting[(lib "scribblings/lib-jsx-graph-labels.rkt" "webracket")]

@(how-to-require include-lib jsx-graph (lib "libs/jsx-graph.rkt"))
@(compile-option-bar "Compile option: " "--ffi jsxgraph")

The @racket[jsx-graph] library provides a small Rackety wrapper around
the @racketid[JXG.JSXGraph] board-creation entry point from JSXGraph.
JSXGraph is an interactive geometry system for drawing points, lines,
circles, and other constructions on a browser board.

Use @racket[jsx-graph] when you want to:

@itemlist[
  @item{create a @racket[JXG.JSXGraph] board in the current page}
  @item{create arbitrary JSXGraph elements with @racket[jsx-create] or the specialized constructors}
  @item{build geometry objects such as @racket[JXG.Point], @racket[JXG.Line], @racket[JXG.Circle], and @racket[JXG.Text]}
  @item{inspect or adjust point properties from Racket code}
  @item{attach browser event handlers to JSXGraph elements}
]

The library keeps the low-level browser FFI bindings tucked away behind
checked helper functions. For the underlying @racketid[js-jsx-*] FFI
bindings, see @racket[ffi/jsxgraph.ffi] and the corresponding browser
API reference page.

The main constructors return checked wrapper structs:
@racket[jsx-board] for @racket[JXG.JSXGraph] boards, @racket[jsx-point]
for @racket[JXG.Point] values, and @racket[jsx-element] for other
geometry objects.

@section{Quick Start}

Start by including the library, making a board, and adding a pair of
points and a line between them.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define p (jsx-create-point board (jsx-parents -1 0)))
(define q (jsx-create-point board (jsx-parents 1 0)))
(define l (jsx-create-line board (jsx-parents p q)))

(void board p q l)
]

The quick start uses @racket[jsx-parents] to pack the parent values into
the vector shape JSXGraph expects.

@section{Examples}

This example shows the same construction with a few helper calls added
for inspection. The board is still a browser board, but the wrapper
functions keep the code fairly Rackety.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define p (jsx-create-point board (jsx-parents -1 0)))
(define q (jsx-create-point board (jsx-parents 1 0)))
(define l (jsx-create-line board (jsx-parents p q)))

(define label
  (jsx-create board 'text (jsx-parents -6 6 "A line through two points")))

(define px (jsx-point-x p))
(define py (jsx-point-y p))
(void board p q l label px py)
]

The board-introspection helpers are useful when you want to check the
result of a construction from Racket. For example, after creating the
board and its objects you can ask how many objects were created and get
the board objects back in construction order:

@racketblock[
(define total (jsx-board-num-objects board))
(define objects (jsx-board-objects-list board))
(void total objects)
]

In the richer demo, these helpers are used to confirm that the board
contains the expected objects before the page reports that the board is
ready.

@section{API Reference}

@defstruct[jsx-board ([raw external/raw])]{
Wraps a JSXGraph board object.
}

@defstruct[jsx-element ([raw external/raw])]{
Wraps a generic JSXGraph geometry object.
}

@defstruct[jsx-point ([raw external/raw])]{
Wraps a JSXGraph point object.
}

@defproc[(jsx-create-board [container-id string?]
                           [maybe-attributes (or/c #f any/c) #f])
         jsx-board?]{
@(jsx-bar "JXG.JSXGraph"
          (jsx-doc-url "JXG.JSXGraph"))
Creates a JSXGraph board for the container with the given id.
}

@defproc[(jsx-create [board jsx-board?]
                     [element-type (or/c string? symbol?)]
                     [parents any/c]
                     [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "JXG.Board"
          (jsx-doc-url "JXG.Board"))
Creates a JSXGraph element of the requested type on @racket[board].
}

@defproc[(jsx-create-point [board jsx-board?]
                           [parents any/c]
                           [attributes (or/c #f any/c) #f])
         jsx-point?]{
@(jsx-bar "Point"
          (jsx-doc-url "Point"))
Creates a point on @racket[board].
}

@defproc[(jsx-create-line [board jsx-board?]
                          [parents any/c]
                          [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Line"
          (jsx-doc-url "Line"))
Creates a line on @racket[board].
}

@defproc[(jsx-create-segment [board jsx-board?]
                             [parents any/c]
                             [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Segment"
          (jsx-doc-url "Segment"))
Creates a segment on @racket[board].
}

@defproc[(jsx-create-circle [board jsx-board?]
                            [parents any/c]
                            [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Circle"
          (jsx-doc-url "Circle"))
Creates a circle on @racket[board].
}

@defproc[(jsx-create-perpendicular [board jsx-board?]
                                   [parents any/c]
                                   [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "PerpendicularPoint"
          (jsx-doc-url "PerpendicularPoint"))
Creates a perpendicular line on @racket[board].
}

@defproc[(jsx-create-intersection [board jsx-board?]
                                  [parents any/c]
                                  [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Intersection"
          (jsx-doc-url "Intersection"))
Creates an intersection point on @racket[board].
}

@defproc[(jsx-create-text [board jsx-board?]
                          [parents any/c]
                          [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Text"
          (jsx-doc-url "Text"))
Creates a text element on @racket[board].
}

@defproc[(jsx-board-count-children [board jsx-board?])
         number?]{
@(jsx-bar "countChildren"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#countChildren"))
Counts the direct children on @racket[board].
}

@defproc[(jsx-board-num-objects [board jsx-board?])
         exact-nonnegative-integer?]{
@(jsx-bar "numObjects"
          (string-append (jsx-doc-url "JXG.Board")
                         "#numObjects"))
Counts how many objects have been created on @racket[board] in total.
}

@defproc[(jsx-board-objects-list [board jsx-board?])
         vector?]{
@(jsx-bar "objectsList"
          (string-append (jsx-doc-url "JXG.Board")
                         "#objectsList"))
Returns the geometric objects on @racket[board] in construction order.
}

@defproc[(jsx-board-id [board jsx-board?])
         any/c]{
Reads the JSXGraph board id.
}

@defproc[(jsx-board-container [board jsx-board?])
         any/c]{
Reads the board container element.
}

@defproc[(jsx-board-renderer [board jsx-board?])
         any/c]{
Reads the renderer used by the board.
}

@defproc[(jsx-board-canvas-width [board jsx-board?])
         any/c]{
Reads the board canvas width.
}

@defproc[(jsx-board-canvas-height [board jsx-board?])
         any/c]{
Reads the board canvas height.
}

@defproc[(jsx-board-bounding-box [board jsx-board?])
         vector?]{
Reads the board bounding box.
}

@defproc[(jsx-board-set-attribute! [board jsx-board?]
                                   [attributes any/c])
         void?]{
Sets arbitrary board attributes.
}

@defproc[(jsx-board-set-bounding-box! [board jsx-board?]
                                      [bbox any/c]
                                      [keepaspectratio any/c]
                                      [setZoom any/c])
         void?]{
Sets the board bounding box.
}

@defproc[(jsx-board-set-zoom! [board jsx-board?]
                              [fX any/c]
                              [fY any/c])
         void?]{
Sets the board zoom.
}

@defproc[(jsx-board-resize-container! [board jsx-board?]
                                      [canvasWidth any/c]
                                      [canvasHeight any/c]
                                      [dontset any/c]
                                      [dontSetBoundingBox any/c])
         void?]{
Resizes the board container.
}

@defproc[(jsx-board-zoom100! [board jsx-board?])
         void?]{
Resets the board zoom to 100%.
}

@defproc[(jsx-board-zoom-all-points! [board jsx-board?])
         void?]{
Zooms the board so every visible point fits in the viewport.
}

@defproc[(jsx-board-zoom-in! [board jsx-board?] [x any/c] [y any/c])
         void?]{
Zooms in around a point.
}

@defproc[(jsx-board-zoom-out! [board jsx-board?] [x any/c] [y any/c])
         void?]{
Zooms out around a point.
}

@defproc[(jsx-board-start-selection-mode! [board jsx-board?])
         void?]{
Enables board selection mode.
}

@defproc[(jsx-board-stop-selection-mode! [board jsx-board?])
         void?]{
Disables board selection mode.
}

@defproc[(jsx-board-stop-all-animation! [board jsx-board?])
         void?]{
Stops all running board animations.
}

@defproc[(jsx-board-to-fullscreen! [board jsx-board?] [id any/c])
         void?]{
Expands the board to fullscreen.
}

@defproc[(jsx-board-start-resize-observer! [board jsx-board?])
         void?]{
Starts watching the container size.
}

@defproc[(jsx-board-stop-resize-observer! [board jsx-board?])
         void?]{
Stops watching the container size.
}

@defproc[(jsx-board-start-intersection-observer! [board jsx-board?])
         void?]{
Starts watching whether the board is visible.
}

@defproc[(jsx-board-stop-intersection-observer! [board jsx-board?])
         void?]{
Stops watching board visibility.
}

@defproc[(jsx-parents [v any/c] ...)
         vector?]{
Packs parent values into a vector for JSXGraph.
}

@defproc[(jsx-coordinates [p external/raw])
         (values flonum? flonum?)]{
Returns the @racket[x] and @racket[y] coordinates of @racket[p].
}

@defproc[(jsx-on [element external/raw]
                 [event string?]
                 [handler procedure?])
         void?]{
Installs a JSXGraph event handler on @racket[element].
}

@defproc[(jsx-point? [v any/c]) boolean?]{
Returns @racket[#t] when @racket[v] is a wrapped JSXGraph point.
}

@defproc[(jsx-point-x [p jsx-point?]) flonum?]{
Returns the x coordinate of a JSXGraph point.
}

@defproc[(jsx-point-y [p jsx-point?]) flonum?]{
Returns the y coordinate of a JSXGraph point.
}

@defproc[(jsx-point-size [p jsx-point?]) flonum?]{
Returns the point size.
}

@defproc[(jsx-set-point-size! [p jsx-point?] [size flonum?]) void?]{
Sets the point size.
}
