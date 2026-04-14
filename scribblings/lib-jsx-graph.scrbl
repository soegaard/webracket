#lang scribble/manual

@(require scribble/manual
          (for-label (lib "scribblings/lib-jsx-graph-labels.rkt" "webracket"))
          "webracket-scribble-utils.rkt")

@title{Library: @racketid[jsx-graph]}
@declare-exporting[(lib "scribblings/lib-jsx-graph-labels.rkt" "webracket")]

@(how-to-require include-lib jsx-graph (lib "libs/jsx-graph.rkt"))
@(compile-option-bar "Compile option: " "--ffi jsxgraph")

The @racket[jsx-graph] library provides a small Rackety wrapper around
the JSXGraph browser library. JSXGraph is an interactive geometry
system for drawing points, lines, circles, and other constructions on a
browser board.

Use @racket[jsx-graph] when you want to:

@itemlist[
  @item{create a JSXGraph board in the current page}
  @item{build geometry objects such as points, lines, circles, and texts}
  @item{inspect or adjust point properties from Racket code}
  @item{attach browser event handlers to JSXGraph elements}
]

The library keeps the low-level browser FFI bindings tucked away behind
checked helper functions. For the underlying @racketid[js-jsx-*] FFI
bindings, see @racket[ffi/jsxgraph.ffi] and the corresponding browser
API reference page.

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

(define-values (px py) (jsx-coordinates p))
(void board p q l px py)
]

@section{API Reference}

@defproc[(jsx-create-board [container-id string?]
                           [maybe-attributes (or/c #f any/c) #f])
         external/raw]{
Creates a JSXGraph board for the container with the given id.
}

@defproc[(jsx-create-point [board external/raw]
                           [parents any/c]
                           [attributes (or/c #f any/c) #f])
         external/raw]{
Creates a point on @racket[board].
}

@defproc[(jsx-create-line [board external/raw]
                          [parents any/c]
                          [attributes (or/c #f any/c) #f])
         external/raw]{
Creates a line on @racket[board].
}

@defproc[(jsx-create-segment [board external/raw]
                             [parents any/c]
                             [attributes (or/c #f any/c) #f])
         external/raw]{
Creates a segment on @racket[board].
}

@defproc[(jsx-create-circle [board external/raw]
                            [parents any/c]
                            [attributes (or/c #f any/c) #f])
         external/raw]{
Creates a circle on @racket[board].
}

@defproc[(jsx-create-perpendicular [board external/raw]
                                   [parents any/c]
                                   [attributes (or/c #f any/c) #f])
         external/raw]{
Creates a perpendicular line on @racket[board].
}

@defproc[(jsx-create-intersection [board external/raw]
                                  [parents any/c]
                                  [attributes (or/c #f any/c) #f])
         external/raw]{
Creates an intersection point on @racket[board].
}

@defproc[(jsx-create-text [board external/raw]
                          [parents any/c]
                          [attributes (or/c #f any/c) #f])
         external/raw]{
Creates a text element on @racket[board].
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

@defproc[(jsx-point-x [p external/raw]) flonum?]{
Returns the x coordinate of a JSXGraph point.
}

@defproc[(jsx-point-y [p external/raw]) flonum?]{
Returns the y coordinate of a JSXGraph point.
}

@defproc[(jsx-point-size [p external/raw]) flonum?]{
Returns the point size.
}

@defproc[(jsx-set-point-size! [p external/raw] [size flonum?]) void?]{
Sets the point size.
}
