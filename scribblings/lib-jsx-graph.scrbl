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
  @item{build geometry objects such as @racket[JXG.Point], @racket[JXG.Line], @racket[JXG.Arc], @racket[JXG.Circle], @racket[JXG.Polygon], and @racket[JXG.Text]}
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

@section{Line Helpers}

@defproc[(jsx-line-direction [line jsx-element?])
         any/c]{
@(jsx-bar "Direction"
          (string-append (jsx-doc-url "Line")
                         "#Direction"))
Returns the direction vector of @racket[line].
}

@defproc[(jsx-line-get-angle [line jsx-element?])
         any/c]{
@(jsx-bar "getAngle"
          (string-append (jsx-doc-url "Line")
                         "#getAngle"))
Returns the angle of @racket[line].
}

@defproc[(jsx-line-get-rise [line jsx-element?])
         any/c]{
@(jsx-bar "getRise"
          (string-append (jsx-doc-url "Line")
                         "#getRise"))
Returns the rise of @racket[line].
}

@defproc[(jsx-line-get-slope [line jsx-element?])
         any/c]{
@(jsx-bar "getSlope"
          (string-append (jsx-doc-url "Line")
                         "#getSlope"))
Returns the slope of @racket[line].
}

@defproc[(jsx-line-horizontal? [line jsx-element?])
         boolean?]{
@(jsx-bar "isHorizontal"
          (string-append (jsx-doc-url "Line")
                         "#isHorizontal"))
Returns @racket[#t] when @racket[line] is horizontal.
}

@defproc[(jsx-line-vertical? [line jsx-element?])
         boolean?]{
@(jsx-bar "isVertical"
          (string-append (jsx-doc-url "Line")
                         "#isVertical"))
Returns @racket[#t] when @racket[line] is vertical.
}

@defproc[(jsx-line-l [line jsx-element?])
         any/c]{
@(jsx-bar "L"
          (string-append (jsx-doc-url "Line")
                         "#L"))
Returns the @racket[L] helper for @racket[line].
}

@defproc[(jsx-line-slope [line jsx-element?])
         any/c]{
@(jsx-bar "Slope"
          (string-append (jsx-doc-url "Line")
                         "#Slope"))
Returns the slope alias for @racket[line].
}

@defproc[(jsx-line-set-fixed-length! [line jsx-element?]
                                     [length any/c])
         void?]{
@(jsx-bar "setFixedLength"
          (string-append (jsx-doc-url "Line")
                         "#setFixedLength"))
Sets a fixed length on @racket[line].
}

@defproc[(jsx-line-x [line jsx-element?]
                     [t any/c])
         any/c]{
@(jsx-bar "X"
          (string-append (jsx-doc-url "Line")
                         "#X"))
Evaluates the @racket[X] function on @racket[line].
}

@defproc[(jsx-line-y [line jsx-element?]
                     [t any/c])
         any/c]{
@(jsx-bar "Y"
          (string-append (jsx-doc-url "Line")
                         "#Y"))
Evaluates the @racket[Y] function on @racket[line].
}

@defproc[(jsx-line-z [line jsx-element?]
                     [t any/c])
         any/c]{
@(jsx-bar "Z"
          (string-append (jsx-doc-url "Line")
                         "#Z"))
Evaluates the @racket[Z] function on @racket[line].
}

@defproc[(jsx-create-segment [board jsx-board?]
                             [parents any/c]
                             [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Segment"
          (jsx-doc-url "Segment"))
Creates a segment on @racket[board].
}

@defproc[(jsx-create-arc [board jsx-board?]
                         [parents any/c]
                         [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Arc"
          (jsx-doc-url "Arc"))
Creates an arc on @racket[board].
}

@defproc[(jsx-create-circle [board jsx-board?]
                            [parents any/c]
                            [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Circle"
          (jsx-doc-url "Circle"))
Creates a circle on @racket[board].
}

@defproc[(jsx-create-curve [board jsx-board?]
                           [parents any/c]
                           [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Curve"
          (jsx-doc-url "Curve"))
Creates a curve on @racket[board].
}

@section{Arc Helpers}

@defproc[(jsx-arc-get-radius [arc jsx-element?])
         any/c]{
@(jsx-bar "getRadius"
          (string-append (jsx-doc-url "Arc")
                         "#getRadius"))
Returns the deprecated radius getter for @racket[arc].
}

@defproc[(jsx-arc-has-point-sector? [arc jsx-element?]
                                    [x any/c]
                                    [y any/c])
         boolean?]{
@(jsx-bar "hasPointSector"
          (string-append (jsx-doc-url "Arc")
                         "#hasPointSector"))
Returns @racket[#t] when the point @racket[(x, y)] lies inside the arc sector.
}

@defproc[(jsx-arc-radius [arc jsx-element?])
         any/c]{
@(jsx-bar "Radius"
          (string-append (jsx-doc-url "Arc")
                         "#Radius"))
Returns the current radius of @racket[arc].
}

@defproc[(jsx-arc-value [arc jsx-element?]
                        [unit any/c]
                        [rad any/c])
         any/c]{
@(jsx-bar "Value"
          (string-append (jsx-doc-url "Arc")
                         "#Value"))
Returns the arc length or angle value for @racket[arc].
}

@section{Circle Helpers}

@defproc[(jsx-circle-area [circle jsx-element?])
         any/c]{
@(jsx-bar "Area"
          (string-append (jsx-doc-url "Circle")
                         "#Area"))
Returns the area of @racket[circle].
}

@defproc[(jsx-circle-bounds [circle jsx-element?])
         any/c]{
@(jsx-bar "bounds"
          (string-append (jsx-doc-url "Circle")
                         "#bounds"))
Returns the bounding box of @racket[circle].
}

@defproc[(jsx-circle-diameter [circle jsx-element?])
         any/c]{
@(jsx-bar "Diameter"
          (string-append (jsx-doc-url "Circle")
                         "#Diameter"))
Returns the diameter of @racket[circle].
}

@defproc[(jsx-circle-get-radius [circle jsx-element?])
         any/c]{
@(jsx-bar "getRadius"
          (string-append (jsx-doc-url "Circle")
                         "#getRadius"))
Returns the radius helper value of @racket[circle].
}

@defproc[(jsx-circle-perimeter [circle jsx-element?])
         any/c]{
@(jsx-bar "Perimeter"
          (string-append (jsx-doc-url "Circle")
                         "#Perimeter"))
Returns the perimeter of @racket[circle].
}

@defproc[(jsx-circle-radius! [circle jsx-element?]
                             [radius any/c])
         void?]{
@(jsx-bar "Radius"
          (string-append (jsx-doc-url "Circle")
                         "#Radius"))
Sets the radius of @racket[circle].
}

@defproc[(jsx-circle-set-radius! [circle jsx-element?]
                                 [radius any/c])
         void?]{
@(jsx-bar "setRadius"
          (string-append (jsx-doc-url "Circle")
                         "#setRadius"))
Sets the radius of @racket[circle].
}

@defproc[(jsx-circle-update-quadraticform! [circle jsx-element?])
         void?]{
@(jsx-bar "updateQuadraticform"
          (string-append (jsx-doc-url "Circle")
                         "#updateQuadraticform"))
Updates the circle quadratic form.
}

@defproc[(jsx-circle-update-renderer! [circle jsx-element?])
         void?]{
@(jsx-bar "updateRenderer"
          (string-append (jsx-doc-url "Circle")
                         "#updateRenderer"))
Refreshes the circle renderer.
}

@defproc[(jsx-circle-update-stdform! [circle jsx-element?])
         void?]{
@(jsx-bar "updateStdform"
          (string-append (jsx-doc-url "Circle")
                         "#updateStdform"))
Updates the circle standard form.
}

@defproc[(jsx-circle-x [circle jsx-element?]
                       [t any/c])
         any/c]{
@(jsx-bar "X"
          (string-append (jsx-doc-url "Circle")
                         "#X"))
Evaluates the @racket[X] function on @racket[circle].
}

@defproc[(jsx-circle-y [circle jsx-element?]
                       [t any/c])
         any/c]{
@(jsx-bar "Y"
          (string-append (jsx-doc-url "Circle")
                         "#Y"))
Evaluates the @racket[Y] function on @racket[circle].
}

@defproc[(jsx-circle-z [circle jsx-element?]
                       [t any/c])
         any/c]{
@(jsx-bar "Z"
          (string-append (jsx-doc-url "Circle")
                         "#Z"))
Evaluates the @racket[Z] function on @racket[circle].
}

@section{Curve Helpers}

@defproc[(jsx-curve-allocate-points! [curve jsx-element?])
         any/c]{
@(jsx-bar "allocatePoints"
          (string-append (jsx-doc-url "Curve")
                         "#allocatePoints"))
Allocates the point cache for @racket[curve].
}

@defproc[(jsx-curve-generate-term [curve jsx-element?])
         any/c]{
@(jsx-bar "generateTerm"
          (string-append (jsx-doc-url "Curve")
                         "#generateTerm"))
Generates the curve term helper.
}

@defproc[(jsx-curve-get-label-position [curve jsx-element?])
         any/c]{
@(jsx-bar "getLabelPosition"
          (string-append (jsx-doc-url "Curve")
                         "#getLabelPosition"))
Returns the label position helper for @racket[curve].
}

@defproc[(jsx-curve-get-transformation-source [curve jsx-element?])
         any/c]{
@(jsx-bar "getTransformationSource"
          (string-append (jsx-doc-url "Curve")
                         "#getTransformationSource"))
Returns the transformation source of @racket[curve].
}

@defproc[(jsx-curve-has-point? [curve jsx-element?]
                               [x any/c]
                               [y any/c])
         boolean?]{
@(jsx-bar "hasPoint"
          (string-append (jsx-doc-url "Curve")
                         "#hasPoint"))
Returns @racket[#t] when screen coordinates hit @racket[curve].
}

@defproc[(jsx-curve-interpolation-function-from-array [curve jsx-element?]
                                                      [data any/c])
         any/c]{
@(jsx-bar "interpolationFunctionFromArray"
          (string-append (jsx-doc-url "Curve")
                         "#interpolationFunctionFromArray"))
Builds an interpolation function from sample data.
}

@defproc[(jsx-curve-max-x [curve jsx-element?])
         any/c]{
@(jsx-bar "maxX"
          (string-append (jsx-doc-url "Curve")
                         "#maxX"))
Returns the maximum x-value of @racket[curve].
}

@defproc[(jsx-curve-min-x [curve jsx-element?])
         any/c]{
@(jsx-bar "minX"
          (string-append (jsx-doc-url "Curve")
                         "#minX"))
Returns the minimum x-value of @racket[curve].
}

@defproc[(jsx-curve-move-to! [curve jsx-element?]
                             [where any/c])
         any/c]{
@(jsx-bar "moveTo"
          (string-append (jsx-doc-url "Curve")
                         "#moveTo"))
Moves @racket[curve] to a new location.
}

@defproc[(jsx-curve-notify-parents! [curve jsx-element?])
         any/c]{
@(jsx-bar "notifyParents"
          (string-append (jsx-doc-url "Curve")
                         "#notifyParents"))
Notifies parents of a curve change.
}

@defproc[(jsx-curve-update-curve! [curve jsx-element?])
         any/c]{
@(jsx-bar "updateCurve"
          (string-append (jsx-doc-url "Curve")
                         "#updateCurve"))
Updates the curve data.
}

@defproc[(jsx-curve-update-data-array! [curve jsx-element?])
         any/c]{
@(jsx-bar "updateDataArray"
          (string-append (jsx-doc-url "Curve")
                         "#updateDataArray"))
Updates the curve data array.
}

@defproc[(jsx-curve-update-renderer! [curve jsx-element?])
         any/c]{
@(jsx-bar "updateRenderer"
          (string-append (jsx-doc-url "Curve")
                         "#updateRenderer"))
Refreshes the curve renderer.
}

@defproc[(jsx-curve-update-transform! [curve jsx-element?])
         any/c]{
@(jsx-bar "updateTransform"
          (string-append (jsx-doc-url "Curve")
                         "#updateTransform"))
Updates a curve transformation.
}

@defproc[(jsx-create-polygon [board jsx-board?]
                             [parents any/c]
                             [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Polygon"
          (jsx-doc-url "JXG.Polygon"))
Creates a polygon on @racket[board].
}

@section{Polygon Helpers}

@defproc[(jsx-polygon-add-points! [polygon jsx-element?]
                                  [point any/c] ...)
         jsx-element?]{
@(jsx-bar "addPoints"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#addPoints"))
Adds vertices to @racket[polygon].
}

@defproc[(jsx-polygon-area [polygon jsx-element?])
         any/c]{
@(jsx-bar "Area"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#Area"))
Returns the area of @racket[polygon].
}

@defproc[(jsx-polygon-bounding-box [polygon jsx-element?])
         any/c]{
@(jsx-bar "boundingBox"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#boundingBox"))
Returns the bounding box of @racket[polygon].
}

@defproc[(jsx-polygon-find-point [polygon jsx-element?]
                                 [point any/c])
         any/c]{
@(jsx-bar "findPoint"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#findPoint"))
Returns the index for @racket[point] in @racket[polygon].
}

@defproc[(jsx-polygon-has-point? [polygon jsx-element?]
                                 [x any/c]
                                 [y any/c])
         boolean?]{
@(jsx-bar "hasPoint"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#hasPoint"))
Returns @racket[#t] when screen coordinates hit @racket[polygon].
}

@defproc[(jsx-polygon-hide-element! [polygon jsx-element?]
                                    [borderless (or/c #f any/c) #f])
         void?]{
@(jsx-bar "hideElement"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#hideElement"))
Hides @racket[polygon], optionally leaving the borders visible.
}

@defproc[(jsx-polygon-insert-points! [polygon jsx-element?]
                                     [point any/c] ...)
         jsx-element?]{
@(jsx-bar "insertPoints"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#insertPoints"))
Inserts vertices into @racket[polygon].
}

@defproc[(jsx-polygon-intersect [polygon jsx-element?]
                                [other any/c])
         any/c]{
@(jsx-bar "intersect"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#intersect"))
Intersects @racket[polygon] with @racket[other].
}

@defproc[(jsx-polygon-l [polygon jsx-element?])
         any/c]{
@(jsx-bar "L"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#L"))
Returns the perimeter alias for @racket[polygon].
}

@defproc[(jsx-polygon-perimeter [polygon jsx-element?])
         any/c]{
@(jsx-bar "Perimeter"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#Perimeter"))
Returns the perimeter of @racket[polygon].
}

@defproc[(jsx-polygon-pnpoly [polygon jsx-element?]
                             [x any/c]
                             [y any/c]
                             [coord-type (or/c #f any/c) #f])
         boolean?]{
@(jsx-bar "pnpoly"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#pnpoly"))
Checks whether @racket[x] and @racket[y] fall inside @racket[polygon].
}

@defproc[(jsx-polygon-remove-points! [polygon jsx-element?]
                                     [point any/c] ...)
         jsx-element?]{
@(jsx-bar "removePoints"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#removePoints"))
Removes vertices from @racket[polygon].
}

@defproc[(jsx-polygon-set-position-directly! [polygon jsx-element?]
                                             [method any/c]
                                             [coords any/c]
                                             [oldcoords any/c])
         jsx-element?]{
@(jsx-bar "setPositionDirectly"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#setPositionDirectly"))
Moves @racket[polygon] directly by coordinate differences.
}

@defproc[(jsx-polygon-show-element! [polygon jsx-element?]
                                    [borderless (or/c #f any/c) #f])
         void?]{
@(jsx-bar "showElement"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#showElement"))
Shows @racket[polygon], optionally leaving the borders visible.
}

@defproc[(jsx-polygon-update-renderer! [polygon jsx-element?])
         void?]{
@(jsx-bar "updateRenderer"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#updateRenderer"))
Refreshes the polygon renderer.
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

@section{Geometry Elements}

The @racket[jsx-element] wrapper covers JSXGraph geometry elements and
compositions. The methods below mirror the documented
@racketid[JXG.GeometryElement] API where it is useful from Racket.

@defproc[(jsx-element-get-attribute [element any/c]
                                    [key any/c])
         any/c]{
@(jsx-bar "getAttribute"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#getAttribute"))
Reads a geometry element attribute.
}

@defproc[(jsx-element-get-attributes [element any/c])
         any/c]{
@(jsx-bar "getAttributes"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#getAttributes"))
Reads all geometry element attributes.
}

@defproc[(jsx-element-get-label-anchor [element any/c])
         any/c]{
@(jsx-bar "getLabelAnchor"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#getLabelAnchor"))
Reads the label anchor for a geometry element.
}

@defproc[(jsx-element-get-name [element any/c])
         any/c]{
@(jsx-bar "getName"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#getName"))
Reads the element name.
}

@defproc[(jsx-element-get-parents [element any/c])
         any/c]{
@(jsx-bar "getParents"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#getParents"))
Reads the element parents.
}

@defproc[(jsx-element-get-property [element any/c]
                                   [key any/c])
         any/c]{
@(jsx-bar "getProperty"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#getProperty"))
Reads the deprecated geometry element property alias.
}

@defproc[(jsx-element-get-snap-sizes [element any/c])
         any/c]{
@(jsx-bar "getSnapSizes"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#getSnapSizes"))
Reads the element snap sizes.
}

@defproc[(jsx-element-get-text-anchor [element any/c])
         any/c]{
@(jsx-bar "getTextAnchor"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#getTextAnchor"))
Reads the text anchor for a geometry element.
}

@defproc[(jsx-element-get-type [element any/c])
         any/c]{
@(jsx-bar "getType"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#getType"))
Reads the element type.
}

@defproc[(jsx-element-has-point? [element any/c]
                                 [x any/c]
                                 [y any/c])
         boolean?]{
@(jsx-bar "hasPoint"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#hasPoint"))
Checks whether screen coordinates hit the element.
}

@defproc[(jsx-element-hide! [element any/c])
         void?]{
@(jsx-bar "hide"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#hide"))
Hides a geometry element.
}

@defproc[(jsx-element-hide-element! [element any/c])
         void?]{
@(jsx-bar "hideElement"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#hideElement"))
Hides a geometry element using the documented alias.
}

@defproc[(jsx-element-no-highlight! [element any/c])
         void?]{
@(jsx-bar "noHighlight"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#noHighlight"))
Removes highlighting from a geometry element.
}

@defproc[(jsx-element-prepare-update! [element any/c])
         void?]{
@(jsx-bar "prepareUpdate"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#prepareUpdate"))
Prepares a geometry element for update.
}

@defproc[(jsx-element-remove! [element any/c])
         void?]{
@(jsx-bar "remove"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#remove"))
Removes a geometry element.
}

@defproc[(jsx-element-remove-all-ticks! [element any/c])
         void?]{
@(jsx-bar "removeAllTicks"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#removeAllTicks"))
Removes all ticks from a geometry element.
}

@defproc[(jsx-element-remove-child! [element any/c]
                                    [child any/c])
         void?]{
@(jsx-bar "removeChild"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#removeChild"))
Removes a dependent child from a geometry element.
}

@defproc[(jsx-element-remove-descendants! [element any/c]
                                          [obj any/c])
         void?]{
@(jsx-bar "removeDescendants"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#removeDescendants"))
Removes a descendant from a geometry element.
}

@defproc[(jsx-element-remove-event! [element any/c]
                                    [handler procedure?])
         void?]{
@(jsx-bar "removeEvent"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#removeEvent"))
Removes a geometry element event handler.
}

@defproc[(jsx-element-remove-ticks! [element any/c]
                                    [tick any/c])
         void?]{
@(jsx-bar "removeTicks"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#removeTicks"))
Removes ticks from a geometry element.
}

@defproc[(jsx-element-set-attribute! [element any/c]
                                     [attributes any/c])
         void?]{
@(jsx-bar "setAttribute"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#setAttribute"))
Sets geometry element attributes.
}

@defproc[(jsx-element-set-label! [element any/c]
                                 [str any/c])
         void?]{
@(jsx-bar "setLabel"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#setLabel"))
Sets the geometry element label.
}

@defproc[(jsx-element-set-label-text! [element any/c]
                                      [str any/c])
         void?]{
@(jsx-bar "setLabelText"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#setLabelText"))
Sets the label text.
}

@defproc[(jsx-element-set-name! [element any/c]
                                [str any/c])
         void?]{
@(jsx-bar "setName"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#setName"))
Sets the geometry element name.
}

@defproc[(jsx-element-set-parents! [element any/c]
                                   [parents any/c])
         void?]{
@(jsx-bar "setParents"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#setParents"))
Sets the geometry element parents.
}

@defproc[(jsx-element-set-position! [element any/c]
                                    [method any/c]
                                    [coords any/c])
         void?]{
@(jsx-bar "setPosition"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#setPosition"))
Sets the geometry element position.
}

@defproc[(jsx-element-set-position-directly! [element any/c]
                                             [method any/c]
                                             [coords any/c]
                                             [oldcoords any/c])
         void?]{
@(jsx-bar "setPositionDirectly"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#setPositionDirectly"))
Sets the geometry element position directly.
}

@defproc[(jsx-element-set-property! [element any/c]
                                    [attributes any/c])
         void?]{
@(jsx-bar "setProperty"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#setProperty"))
Sets the deprecated geometry element property alias.
}

@defproc[(jsx-element-show! [element any/c])
         void?]{
@(jsx-bar "show"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#show"))
Shows a geometry element.
}

@defproc[(jsx-element-show-element! [element any/c])
         void?]{
@(jsx-bar "showElement"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#showElement"))
Shows a geometry element using the documented alias.
}

@defproc[(jsx-element-update! [element any/c])
         void?]{
@(jsx-bar "update"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#update"))
Updates a geometry element.
}

@defproc[(jsx-element-update-renderer! [element any/c])
         void?]{
@(jsx-bar "updateRenderer"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#updateRenderer"))
Updates the element renderer.
}

@defproc[(jsx-element-update-visibility! [element any/c]
                                         [parent-val any/c])
         void?]{
@(jsx-bar "updateVisibility"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#updateVisibility"))
Updates the visibility state of a geometry element.
}

@defproc[(jsx-element-use-locale! [element any/c])
         void?]{
@(jsx-bar "useLocale"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#useLocale"))
Enables locale-aware number formatting on a geometry element.
}

@defproc[(jsx-element-add-child! [element any/c]
                                 [obj any/c])
         void?]{
@(jsx-bar "addChild"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#addChild"))
Adds a dependent child to a geometry element.
}

@defproc[(jsx-element-add-descendants! [element any/c]
                                       [obj any/c])
         void?]{
@(jsx-bar "addDescendants"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#addDescendants"))
Adds descendants to a geometry element.
}

@defproc[(jsx-element-add-parents! [element any/c]
                                   [parents any/c])
         void?]{
@(jsx-bar "addParents"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#addParents"))
Adds parents to a geometry element.
}

@defproc[(jsx-element-add-parents-from-jc-functions! [element any/c]
                                                     [function-array any/c])
         void?]{
@(jsx-bar "addParentsFromJCFunctions"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#addParentsFromJCFunctions"))
Adds parents derived from JC functions.
}

@defproc[(jsx-element-add-rotation! [element any/c]
                                    [angle any/c])
         void?]{
@(jsx-bar "addRotation"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#addRotation"))
Adds a rotation to a geometry element.
}

@defproc[(jsx-element-add-ticks! [element any/c]
                                 [ticks any/c])
         void?]{
@(jsx-bar "addTicks"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#addTicks"))
Adds ticks to a geometry element.
}

@defproc[(jsx-element-add-transform! [element any/c]
                                     [transform any/c])
         void?]{
@(jsx-bar "addTransform"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#addTransform"))
Adds a transform to a geometry element.
}

@defproc[(jsx-element-animate! [element any/c]
                               [hash any/c]
                               [time any/c]
                               [options any/c])
         any/c]{
@(jsx-bar "animate"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#animate"))
Animates a geometry element.
}

@defproc[(jsx-element-bounds [element any/c])
         any/c]{
@(jsx-bar "bounds"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#bounds"))
Reads the bounds of a geometry element.
}

@defproc[(jsx-element-clear-trace! [element any/c])
         void?]{
@(jsx-bar "clearTrace"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#clearTrace"))
Clears the trace of a geometry element.
}

@defproc[(jsx-element-clone-to-background! [element any/c])
         any/c]{
@(jsx-bar "cloneToBackground"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#cloneToBackground"))
Clones a geometry element to the background.
}

@defproc[(jsx-element-count-children [element any/c])
         exact-nonnegative-integer?]{
@(jsx-bar "countChildren"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#countChildren"))
Counts the direct children of a geometry element.
}

@defproc[(jsx-element-create-gradient! [element any/c])
         void?]{
@(jsx-bar "createGradient"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#createGradient"))
Creates a gradient for a geometry element.
}

@defproc[(jsx-element-create-label! [element any/c])
         void?]{
@(jsx-bar "createLabel"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#createLabel"))
Creates a label for a geometry element.
}

@defproc[(jsx-element-draggable? [element any/c])
         boolean?]{
@(jsx-bar "draggable"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#draggable"))
Checks whether a geometry element is draggable.
}

@defproc[(jsx-element-eval [element any/c]
                           [val any/c])
         any/c]{
@(jsx-bar "eval"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#eval"))
Evaluates a geometry-element-specific value.
}

@defproc[(jsx-element-eval-vis-prop [element any/c]
                                    [key any/c])
         any/c]{
@(jsx-bar "evalVisProp"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#evalVisProp"))
Evaluates a visual property for a geometry element.
}

@defproc[(jsx-element-format-number-locale [element any/c]
                                           [value any/c]
                                           [digits any/c])
         any/c]{
@(jsx-bar "formatNumberLocale"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#formatNumberLocale"))
Formats a number using the element locale settings.
}

@defproc[(jsx-element-full-update! [element any/c])
         any/c]{
@(jsx-bar "fullUpdate"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#fullUpdate"))
Runs the full update chain for a geometry element.
}

@defproc[(jsx-element-generate-polynomial [element any/c])
         any/c]{
@(jsx-bar "generatePolynomial"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#generatePolynomial"))
Generates the polynomial for a geometry element.
}

@defproc[(jsx-element-handle-snap-to-grid! [element any/c]
                                           [force any/c]
                                           [from-parent any/c])
         any/c]{
@(jsx-bar "handleSnapToGrid"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#handleSnapToGrid"))
Handles snapping a geometry element to the grid.
}

@defproc[(jsx-element-normalize! [element any/c])
         any/c]{
@(jsx-bar "normalize"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#normalize"))
Normalizes a geometry element.
}

@defproc[(jsx-element-resolve-shortcuts! [element any/c]
                                         [attributes any/c])
         any/c]{
@(jsx-bar "resolveShortcuts"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#resolveShortcuts"))
Resolves geometry-element attribute shortcuts.
}

@defproc[(jsx-element-set-arrow! [element any/c]
                                 [first-arrow any/c]
                                 [last-arrow any/c])
         void?]{
@(jsx-bar "setArrow"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#setArrow"))
Sets arrow flags on a geometry element.
}

@defproc[(jsx-element-set-dash! [element any/c]
                                [dash any/c])
         void?]{
@(jsx-bar "setDash"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#setDash"))
Sets the dash style on a geometry element.
}

@defproc[(jsx-element-set-display-rend-node! [element any/c]
                                             [val any/c])
         void?]{
@(jsx-bar "setDisplayRendNode"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#setDisplayRendNode"))
Sets the display renderer node on a geometry element.
}

@defproc[(jsx-element-snap-to-points! [element any/c])
         any/c]{
@(jsx-bar "snapToPoints"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#snapToPoints"))
Snaps a geometry element to nearby points.
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

@defproc[(jsx-board-add-grid! [board jsx-board?])
         void?]{
@(jsx-bar "addGrid"
          (string-append (jsx-doc-url "JXG.Board")
                         "#addGrid"))
Adds the default grid to the board.
}

@defproc[(jsx-board-add-hook! [board jsx-board?]
                              [hook procedure?]
                              [m (or/c #f string?) "update"]
                              [context any/c board])
         exact-integer?]{
@(jsx-bar "addHook"
          (string-append (jsx-doc-url "JXG.Board")
                         "#addHook"))
Registers a hook on @racket[board] and returns its id.
}

@defproc[(jsx-board-add-keyboard-event-handlers! [board jsx-board?])
         void?]{
@(jsx-bar "addKeyboardEventHandlers"
          (string-append (jsx-doc-url "JXG.Board")
                         "#addKeyboardEventHandlers"))
Registers keyboard event handlers for @racket[board].
}

@defproc[(jsx-board-add-mouse-event-handlers! [board jsx-board?])
         void?]{
@(jsx-bar "addMouseEventHandlers"
          (string-append (jsx-doc-url "JXG.Board")
                         "#addMouseEventHandlers"))
Registers mouse event handlers for @racket[board].
}

@defproc[(jsx-board-add-pointer-event-handlers! [board jsx-board?])
         void?]{
@(jsx-bar "addPointerEventHandlers"
          (string-append (jsx-doc-url "JXG.Board")
                         "#addPointerEventHandlers"))
Registers pointer event handlers for @racket[board].
}

@defproc[(jsx-board-add-resize-event-handlers! [board jsx-board?])
         void?]{
@(jsx-bar "addResizeEventHandlers"
          (string-append (jsx-doc-url "JXG.Board")
                         "#addResizeEventHandlers"))
Registers resize event handlers for @racket[board].
}

@defproc[(jsx-board-add-touch-event-handlers! [board jsx-board?]
                                              [apple-gestures any/c])
         void?]{
@(jsx-bar "addTouchEventHandlers"
          (string-append (jsx-doc-url "JXG.Board")
                         "#addTouchEventHandlers"))
Registers touch event handlers for @racket[board].
}

@defproc[(jsx-board-add-wheel-event-handlers! [board jsx-board?])
         void?]{
@(jsx-bar "addWheelEventHandlers"
          (string-append (jsx-doc-url "JXG.Board")
                         "#addWheelEventHandlers"))
Registers wheel event handlers for @racket[board].
}

@defproc[(jsx-board-add-fullscreen-event-handlers! [board jsx-board?])
         void?]{
@(jsx-bar "addFullscreenEventHandlers"
          (string-append (jsx-doc-url "JXG.Board")
                         "#addFullscreenEventHandlers"))
Registers fullscreen event handlers for @racket[board].
}

@defproc[(jsx-board-add-event-handlers! [board jsx-board?])
         void?]{
@(jsx-bar "addEventHandlers"
          (string-append (jsx-doc-url "JXG.Board")
                         "#addEventHandlers"))
Registers all board event handlers for @racket[board].
}

@defproc[(jsx-board-add-child! [board jsx-board?]
                               [child jsx-board?])
         void?]{
@(jsx-bar "addChild"
          (string-append (jsx-doc-url "JXG.Board")
                         "#addChild"))
Registers a dependent board.
}

@defproc[(jsx-board-add-animation! [board jsx-board?]
                                   [element any/c])
         void?]{
@(jsx-bar "addAnimation"
          (string-append (jsx-doc-url "JXG.Board")
                         "#addAnimation"))
Registers an animated board element.
}

@defproc[(jsx-board-add-conditions! [board jsx-board?]
                                    [str string?])
         void?]{
@(jsx-bar "addConditions"
          (string-append (jsx-doc-url "JXG.Board")
                         "#addConditions"))
Adds conditional updates to the board.
}

@defproc[(jsx-board-apply-zoom! [board jsx-board?])
         void?]{
@(jsx-bar "applyZoom"
          (string-append (jsx-doc-url "JXG.Board")
                         "#applyZoom"))
Applies the current zoom factors to all objects.
}

@defproc[(jsx-board-calculate-snap-sizes! [board jsx-board?])
         void?]{
@(jsx-bar "calculateSnapSizes"
          (string-append (jsx-doc-url "JXG.Board")
                         "#calculateSnapSizes"))
Recomputes the board snap sizes.
}

@defproc[(jsx-board-add-event! [board jsx-board?]
                               [event (or/c string? symbol?)]
                               [handler procedure?])
         void?]{
@(jsx-bar "on"
          (string-append (jsx-doc-url "JXG.Board")
                         "#addEvent"))
Registers a JSXGraph board event handler.
}

@defproc[(jsx-board-get-mouse-position [board jsx-board?]
                                       [evt any/c]
                                       [i (or/c #f exact-integer?) #f])
         vector?]{
@(jsx-bar "getMousePosition"
          (string-append (jsx-doc-url "JXG.Board")
                         "#getMousePosition"))
Returns the mouse position in screen coordinates.
}

@defproc[(jsx-board-get-usr-coords-of-mouse [board jsx-board?]
                                            [evt any/c])
         vector?]{
@(jsx-bar "getUsrCoordsOfMouse"
          (string-append (jsx-doc-url "JXG.Board")
                         "#getUsrCoordsOfMouse"))
Returns the mouse position in user coordinates.
}

@defproc[(jsx-board-get-coords-top-left-corner [board jsx-board?])
         vector?]{
@(jsx-bar "getCoordsTopLeftCorner"
          (string-append (jsx-doc-url "JXG.Board")
                         "#getCoordsTopLeftCorner"))
Returns the board coordinates of the top-left corner.
}

@defproc[(jsx-board-get-bounding-box [board jsx-board?])
         vector?]{
@(jsx-bar "getBoundingBox"
          (string-append (jsx-doc-url "JXG.Board")
                         "#getBoundingBox"))
Returns the board bounding box via the JSXGraph method.
}

@defproc[(jsx-board-get-scr-coords-of-mouse [board jsx-board?]
                                            [x any/c]
                                            [y any/c])
         vector?]{
@(jsx-bar "getScrCoordsOfMouse"
          (string-append (jsx-doc-url "JXG.Board")
                         "#getScrCoordsOfMouse"))
Returns the screen coordinates that JSXGraph uses for mouse tracking.
}

@defproc[(jsx-board-get-all-objects-under-mouse [board jsx-board?]
                                                 [evt any/c])
         vector?]{
@(jsx-bar "getAllObjectsUnderMouse"
          (string-append (jsx-doc-url "JXG.Board")
                         "#getAllObjectsUnderMouse"))
Returns all objects under a pointer event.
}

@defproc[(jsx-board-get-all-under-mouse [board jsx-board?]
                                        [evt any/c])
         vector?]{
@(jsx-bar "getAllUnderMouse"
          (string-append (jsx-doc-url "JXG.Board")
                         "#getAllUnderMouse"))
Returns the objects and coordinates under a pointer event.
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

@defproc[(jsx-board-remove-grids! [board jsx-board?])
         void?]{
@(jsx-bar "removeGrids"
          (string-append (jsx-doc-url "JXG.Board")
                         "#removeGrids"))
Removes all grids from the board.
}

@defproc[(jsx-board-remove-hook! [board jsx-board?]
                                 [id exact-integer?])
         void?]{
@(jsx-bar "removeHook"
          (string-append (jsx-doc-url "JXG.Board")
                         "#removeHook"))
Removes a previously registered board hook.
}

@defproc[(jsx-board-remove-keyboard-event-handlers! [board jsx-board?])
         void?]{
@(jsx-bar "removeKeyboardEventHandlers"
          (string-append (jsx-doc-url "JXG.Board")
                         "#removeKeyboardEventHandlers"))
Removes keyboard event handlers from @racket[board].
}

@defproc[(jsx-board-remove-mouse-event-handlers! [board jsx-board?])
         void?]{
@(jsx-bar "removeMouseEventHandlers"
          (string-append (jsx-doc-url "JXG.Board")
                         "#removeMouseEventHandlers"))
Removes mouse event handlers from @racket[board].
}

@defproc[(jsx-board-remove-pointer-event-handlers! [board jsx-board?])
         void?]{
@(jsx-bar "removePointerEventHandlers"
          (string-append (jsx-doc-url "JXG.Board")
                         "#removePointerEventHandlers"))
Removes pointer event handlers from @racket[board].
}

@defproc[(jsx-board-remove-resize-event-handlers! [board jsx-board?])
         void?]{
@(jsx-bar "removeResizeEventHandlers"
          (string-append (jsx-doc-url "JXG.Board")
                         "#removeResizeEventHandlers"))
Removes resize event handlers from @racket[board].
}

@defproc[(jsx-board-remove-event-handlers! [board jsx-board?])
         void?]{
@(jsx-bar "removeEventHandlers"
          (string-append (jsx-doc-url "JXG.Board")
                         "#removeEventHandlers"))
Removes all board event handlers from @racket[board].
}

@defproc[(jsx-board-remove-touch-event-handlers! [board jsx-board?])
         void?]{
@(jsx-bar "removeTouchEventHandlers"
          (string-append (jsx-doc-url "JXG.Board")
                         "#removeTouchEventHandlers"))
Removes touch event handlers from @racket[board].
}

@defproc[(jsx-board-select [board jsx-board?]
                           [str any/c]
                           [only-by-id-or-name (or/c #f boolean?) #f])
         (or/c jsx-point? jsx-element?)]{
@(jsx-bar "select"
          (string-append (jsx-doc-url "JXG.Board")
                         "#select"))
Selects one or more objects on the board.
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

@defproc[(jsx-board-clear-traces! [board jsx-board?])
         void?]{
@(jsx-bar "clearTraces"
          (string-append (jsx-doc-url "JXG.Board")
                         "#clearTraces"))
Removes all traced elements from @racket[board].
}

@defproc[(jsx-board-dehighlight-all! [board jsx-board?])
         void?]{
@(jsx-bar "dehighlightAll"
          (string-append (jsx-doc-url "JXG.Board")
                         "#dehighlightAll"))
Removes highlighting from all elements on @racket[board].
}

@defproc[(jsx-board-update-coords! [board jsx-board?])
         void?]{
@(jsx-bar "updateCoords"
          (string-append (jsx-doc-url "JXG.Board")
                         "#updateCoords"))
Updates the coordinates of elements that need it.
}

@defproc[(jsx-board-update-container-dims! [board jsx-board?]
                                           [width (or/c #f any/c) #f]
                                           [height (or/c #f any/c) #f])
         void?]{
@(jsx-bar "updateContainerDims"
          (string-append (jsx-doc-url "JXG.Board")
                         "#updateContainerDims"))
Updates the board container dimensions.
}

@defproc[(jsx-board-update-csstransforms! [board jsx-board?])
         void?]{
@(jsx-bar "updateCSSTransforms"
          (string-append (jsx-doc-url "JXG.Board")
                         "#updateCSSTransforms"))
Refreshes CSS transforms on @racket[board].
}

@defproc[(jsx-board-update-elements! [board jsx-board?]
                                     [drag any/c])
         void?]{
@(jsx-bar "updateElements"
          (string-append (jsx-doc-url "JXG.Board")
                         "#updateElements"))
Updates the board elements.
}

@defproc[(jsx-board-update-hooks! [board jsx-board?]
                                  [m any/c])
         void?]{
@(jsx-bar "updateHooks"
          (string-append (jsx-doc-url "JXG.Board")
                         "#updateHooks"))
Runs hooked board callbacks.
}

@defproc[(jsx-board-update-conditions! [board jsx-board?])
         void?]{
@(jsx-bar "updateConditions"
          (string-append (jsx-doc-url "JXG.Board")
                         "#updateConditions"))
Updates the conditional board elements.
}

@defproc[(jsx-board-suppress-default! [board jsx-board?]
                                      [e any/c])
         void?]{
@(jsx-bar "suppressDefault"
          (string-append (jsx-doc-url "JXG.Board")
                         "#suppressDefault"))
Suppresses the default event action.
}

@defproc[(jsx-board-init-infobox! [board jsx-board?]
                                  [attributes any/c])
         void?]{
@(jsx-bar "initInfobox"
          (string-append (jsx-doc-url "JXG.Board")
                         "#initInfobox"))
Initializes the board infobox.
}

@defproc[(jsx-board-init-move-object! [board jsx-board?]
                                      [x any/c]
                                      [y any/c]
                                      [evt any/c]
                                      [type any/c])
         void?]{
@(jsx-bar "initMoveObject"
          (string-append (jsx-doc-url "JXG.Board")
                         "#initMoveObject"))
Prepares a board object move.
}

@defproc[(jsx-board-init-move-origin! [board jsx-board?]
                                      [x any/c]
                                      [y any/c])
         void?]{
@(jsx-bar "initMoveOrigin"
          (string-append (jsx-doc-url "JXG.Board")
                         "#initMoveOrigin"))
Prepares moving the board origin.
}

@defproc[(jsx-board-highlight-custom-infobox! [board jsx-board?]
                                              [text string?]
                                              [el (or/c #f any/c) #f])
         void?]{
@(jsx-bar "highlightCustomInfobox"
          (string-append (jsx-doc-url "JXG.Board")
                         "#highlightCustomInfobox"))
Updates the info box text on @racket[board].
}

@defproc[(jsx-board-highlight-infobox! [board jsx-board?]
                                       [x any/c]
                                       [y any/c]
                                       [el (or/c #f any/c) #f])
         void?]{
@(jsx-bar "highlightInfobox"
          (string-append (jsx-doc-url "JXG.Board")
                         "#highlightInfobox"))
Shows the given coordinates in the board info box.
}

@defproc[(jsx-board-move-object! [board jsx-board?]
                                 [x any/c]
                                 [y any/c]
                                 [o any/c]
                                 [evt any/c]
                                 [type any/c])
         void?]{
@(jsx-bar "moveObject"
          (string-append (jsx-doc-url "JXG.Board")
                         "#moveObject"))
Moves a board object.
}

@defproc[(jsx-board-show-dependencies! [board jsx-board?])
         void?]{
@(jsx-bar "showDependencies"
          (string-append (jsx-doc-url "JXG.Board")
                         "#showDependencies"))
Shows the dependency graph for the board.
}

@defproc[(jsx-board-create-roulette! [board jsx-board?]
                                     [c1 any/c]
                                     [c2 any/c]
                                     [start-c1 any/c]
                                     [stepsize any/c]
                                     [direction any/c]
                                     [time any/c]
                                     [pointlist any/c])
         any/c]{
@(jsx-bar "createRoulette"
          (string-append (jsx-doc-url "JXG.Board")
                         "#createRoulette"))
Creates a roulette animation on the board.
}

@defproc[(jsx-board-generate-id [board jsx-board?])
         string?]{
@(jsx-bar "generateId"
          (string-append (jsx-doc-url "JXG.Board")
                         "#generateId"))
Generates a fresh board id.
}

@defproc[(jsx-board-generate-name [board jsx-board?]
                                  [object any/c])
         string?]{
@(jsx-bar "generateName"
          (string-append (jsx-doc-url "JXG.Board")
                         "#generateName"))
Generates a fresh object name for a board object.
}

@defproc[(jsx-board-init-geonext-board! [board jsx-board?])
         void?]{
@(jsx-bar "initGeonextBoard"
          (string-append (jsx-doc-url "JXG.Board")
                         "#initGeonextBoard"))
Initializes the default GEONExT board objects.
}

@defproc[(jsx-board-remove-event! [board jsx-board?]
                                  [event (or/c string? symbol?)]
                                  [handler procedure?])
         void?]{
@(jsx-bar "off"
          (string-append (jsx-doc-url "JXG.Board")
                         "#removeEvent"))
Removes a JSXGraph board event handler.
}

@defproc[(jsx-board-show-xml! [board jsx-board?])
         void?]{
@(jsx-bar "showXML"
          (string-append (jsx-doc-url "JXG.Board")
                         "#showXML"))
Shows the board XML in a separate window.
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

@defproc[(jsx-board-update-infobox! [board jsx-board?]
                                    [el any/c])
         void?]{
@(jsx-bar "updateInfobox"
          (string-append (jsx-doc-url "JXG.Board")
                         "#updateInfobox"))
Updates the info box for @racket[el].
}

@defproc[(jsx-board-has-point? [board jsx-board?]
                               [x any/c]
                               [y any/c])
         boolean?]{
@(jsx-bar "hasPoint"
          (string-append (jsx-doc-url "JXG.Board")
                         "#hasPoint"))
Checks whether the point @racket[x], @racket[y] lies inside the board viewport.
}

@defproc[(jsx-board-move-origin! [board jsx-board?]
                                 [x any/c]
                                 [y any/c]
                                 [diff any/c])
         void?]{
@(jsx-bar "moveOrigin"
          (string-append (jsx-doc-url "JXG.Board")
                         "#moveOrigin"))
Moves the origin of the board.
}

@defproc[(jsx-board-set-id [board jsx-board?]
                           [obj any/c]
                           [type any/c])
         string?]{
@(jsx-bar "setId"
          (string-append (jsx-doc-url "JXG.Board")
                         "#setId"))
Composes an id for an element on the board.
}

@defproc[(jsx-board-update-renderer! [board jsx-board?])
         void?]{
@(jsx-bar "updateRenderer"
          (string-append (jsx-doc-url "JXG.Board")
                         "#updateRenderer"))
Refreshes the board renderer.
}

@defproc[(jsx-board-update-renderer-canvas! [board jsx-board?])
         void?]{
@(jsx-bar "updateRendererCanvas"
          (string-append (jsx-doc-url "JXG.Board")
                         "#updateRendererCanvas"))
Refreshes the board renderer in Canvas mode.
}

@defproc[(jsx-board-zoom-elements! [board jsx-board?]
                                   [elements any/c])
         void?]{
@(jsx-bar "zoomElements"
          (string-append (jsx-doc-url "JXG.Board")
                         "#zoomElements"))
Zooms the board so the given elements fit in the viewport.
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
                 [event (or/c string? symbol?)]
                 [handler procedure?])
         void?]{
@(jsx-bar "on"
          (jsx-doc-url "JXG.Board"))
Installs a JSXGraph event handler on a board or geometry element.
}

@defproc[(jsx-element-add-event! [element external/raw]
                                 [event (or/c string? symbol?)]
                                 [handler procedure?])
         void?]{
@(jsx-bar "addEvent"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#addEvent"))
Registers a GeometryElement event handler through the JSXGraph alias
for @racket[JXG.EventEmitter.on].
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
