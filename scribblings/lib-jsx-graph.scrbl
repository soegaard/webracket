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
  @item{build geometry objects such as @racket[JXG.Point], @racket[JXG.Line], @racket[JXG.Arc], @racket[JXG.Angle], @racket[JXG.Sector], @racket[JXG.Circle], @racket[JXG.Conic], @racket[JXG.Ellipse], @racket[JXG.Functiongraph], @racket[JXG.Riemannsum], @racket[JXG.Slopefield], @racket[JXG.Vectorfield], @racket[JXG.ImplicitCurve], @racket[JXG.Spline], @racket[JXG.Cardinalspline], @racket[JXG.Comb], @racket[JXG.Metapostspline], @racket[JXG.PolygonalChain], @racket[JXG.RegularPolygon], @racket[JXG.Hyperbola], @racket[JXG.Parabola], @racket[JXG.Stepfunction], @racket[JXG.Inequality], @racket[JXG.Turtle], @racket[JXG.Polygon], @racket[JXG.Text], @racket[JXG.ForeignObject], @racket[JXG.Tapemeasure], @racket[JXG.Measurement], @racket[JXG.Circumcenter], and @racket[JXG.MirrorElement]}
  @item{create chart objects with @racket[JXG.Chart]}
  @item{create legends and smart labels}
  @item{create widget-like elements such as buttons, checkboxes, sliders, and inputs}
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

@defproc[(jsx-create-angle [board jsx-board?]
                           [parents any/c]
                           [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Angle"
          (jsx-doc-url "Angle"))
Creates an angle on @racket[board].
}

@defproc[(jsx-create-sector [board jsx-board?]
                            [parents any/c]
                            [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Sector"
          (jsx-doc-url "Sector"))
Creates a sector on @racket[board].
}

@defproc[(jsx-create-glider [board jsx-board?]
                            [parents any/c]
                            [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Glider"
          (jsx-doc-url "Glider"))
Creates a glider on @racket[board].
}

@defproc[(jsx-create-circle [board jsx-board?]
                            [parents any/c]
                            [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Circle"
          (jsx-doc-url "Circle"))
Creates a circle on @racket[board].
}

@defproc[(jsx-create-conic [board jsx-board?]
                           [parents any/c]
                           [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Conic"
          (jsx-doc-url "Conic"))
Creates a conic section on @racket[board].
}

@defproc[(jsx-create-ellipse [board jsx-board?]
                             [parents any/c]
                             [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Ellipse"
          (jsx-doc-url "Ellipse"))
Creates an ellipse on @racket[board].
}

@defproc[(jsx-create-curve [board jsx-board?]
                           [parents any/c]
                           [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Curve"
          (jsx-doc-url "Curve"))
Creates a curve on @racket[board].
}

@defproc[(jsx-create-functiongraph [board jsx-board?]
                                   [parents any/c]
                                   [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Functiongraph"
          (jsx-doc-url "Functiongraph"))
Creates a function graph on @racket[board].
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

@section{Angle Helpers}

@defproc[(jsx-angle-free? [angle jsx-element?])
         boolean?]{
@(jsx-bar "free"
          (string-append (jsx-doc-url "Angle")
                         "#free"))
Returns @racket[#t] when the angle is free.
}

@defproc[(jsx-angle-set-angle! [angle jsx-element?]
                               [val any/c])
         void?]{
@(jsx-bar "setAngle"
          (string-append (jsx-doc-url "Angle")
                         "#setAngle"))
Sets the angle value on @racket[angle].
}

@defproc[(jsx-angle-value [angle jsx-element?]
                          [unit any/c])
         any/c]{
@(jsx-bar "Value"
          (string-append (jsx-doc-url "Angle")
                         "#Value"))
Returns the current angle value for @racket[angle].
}

@section{Sector Helpers}

@defproc[(jsx-sector-area [sector jsx-element?])
         any/c]{
@(jsx-bar "Area"
          (string-append (jsx-doc-url "Sector")
                         "#Area"))
Returns the area of @racket[sector].
}

@defproc[(jsx-sector-has-point-sector? [sector jsx-element?]
                                       [x any/c]
                                       [y any/c])
         boolean?]{
@(jsx-bar "hasPointSector"
          (string-append (jsx-doc-url "Sector")
                         "#hasPointSector"))
Returns @racket[#t] when the point lies inside @racket[sector].
}

@defproc[(jsx-sector-l [sector jsx-element?])
         any/c]{
@(jsx-bar "L"
          (string-append (jsx-doc-url "Sector")
                         "#L"))
Returns the arc length of @racket[sector].
}

@defproc[(jsx-sector-perimeter [sector jsx-element?])
         any/c]{
@(jsx-bar "Perimeter"
          (string-append (jsx-doc-url "Sector")
                         "#Perimeter"))
Returns the perimeter of @racket[sector].
}

@defproc[(jsx-sector-radius [sector jsx-element?])
         any/c]{
@(jsx-bar "Radius"
          (string-append (jsx-doc-url "Sector")
                         "#Radius"))
Returns the radius of @racket[sector].
}

@defproc[(jsx-sector-set-position-directly! [sector jsx-element?]
                                            [method any/c]
                                            [coords any/c]
                                            [oldcoords any/c])
         any/c]{
@(jsx-bar "setPositionDirectly"
          (string-append (jsx-doc-url "Sector")
                         "#setPositionDirectly"))
Moves @racket[sector] by direct coordinates.
}

@defproc[(jsx-sector-set-radius! [sector jsx-element?]
                                 [value any/c])
         any/c]{
@(jsx-bar "setRadius"
          (string-append (jsx-doc-url "Sector")
                         "#setRadius"))
Sets the radius of @racket[sector].
}

@section{Glider Helpers}

@defproc[(jsx-glider-start-animation! [glider jsx-element?]
                                      [direction any/c]
                                      [step-count any/c]
                                      [delay any/c])
         void?]{
@(jsx-bar "startAnimation"
          (string-append (jsx-doc-url "Glider")
                         "#startAnimation"))
Starts the glider animation loop.
}

@defproc[(jsx-glider-stop-animation! [glider jsx-element?])
         void?]{
@(jsx-bar "stopAnimation"
          (string-append (jsx-doc-url "Glider")
                         "#stopAnimation"))
Stops the glider animation loop.
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

@defproc[(jsx-create-midpoint [board jsx-board?]
                              [parents any/c]
                              [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Midpoint"
          (jsx-doc-url "Midpoint"))
Creates a midpoint on @racket[board].
}

@defproc[(jsx-create-parallel [board jsx-board?]
                              [parents any/c]
                              [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Parallel"
          (jsx-doc-url "Parallel"))
Creates a parallel line on @racket[board].
}

@defproc[(jsx-create-arrowparallel [board jsx-board?]
                                   [parents any/c]
                                   [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Arrowparallel"
          (jsx-doc-url "Arrowparallel"))
Creates an arrowparallel on @racket[board].
}

@defproc[(jsx-create-axis [board jsx-board?]
                          [parents any/c]
                          [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Axis"
          (jsx-doc-url "Axis"))
Creates an axis on @racket[board].
}

@defproc[(jsx-create-grid [board jsx-board?]
                          [parents any/c]
                          [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Grid"
          (jsx-doc-url "Grid"))
Creates a grid on @racket[board].
}

@defproc[(jsx-create-boxplot [board jsx-board?]
                             [parents any/c]
                             [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Boxplot"
          (jsx-doc-url "Boxplot"))
Creates a boxplot on @racket[board].
}

@defproc[(jsx-create-tangent [board jsx-board?]
                             [parents any/c]
                             [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Tangent"
          (jsx-doc-url "Tangent"))
Creates a tangent line on @racket[board].
}

@defproc[(jsx-create-tangentto [board jsx-board?]
                               [parents any/c]
                               [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "TangentTo"
          (jsx-doc-url "TangentTo"))
Creates a tangent-to line on @racket[board].
}

@defproc[(jsx-create-polarline [board jsx-board?]
                               [parents any/c]
                               [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "PolarLine"
          (jsx-doc-url "PolarLine"))
Creates a polar line on @racket[board].
}

@defproc[(jsx-create-polepoint [board jsx-board?]
                               [parents any/c]
                               [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "PolePoint"
          (jsx-doc-url "PolePoint"))
Creates a pole point on @racket[board].
}

@defproc[(jsx-create-radicalaxis [board jsx-board?]
                                 [parents any/c]
                                 [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "RadicalAxis"
          (jsx-doc-url "RadicalAxis"))
Creates a radical axis on @racket[board].
}

@defproc[(jsx-create-circumcircle [board jsx-board?]
                                  [parents any/c]
                                  [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Circumcircle"
          (jsx-doc-url "Circumcircle"))
Creates a circumcircle on @racket[board].
}

@defproc[(jsx-create-incircle [board jsx-board?]
                              [parents any/c]
                              [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Incircle"
          (jsx-doc-url "Incircle"))
Creates an incircle on @racket[board].
}

@defproc[(jsx-create-circumcirclearc [board jsx-board?]
                                     [parents any/c]
                                     [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "CircumcircleArc"
          (jsx-doc-url "CircumcircleArc"))
Creates a circumcircle arc on @racket[board].
}

@defproc[(jsx-create-circumcirclesector [board jsx-board?]
                                        [parents any/c]
                                        [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "CircumcircleSector"
          (jsx-doc-url "CircumcircleSector"))
Creates a circumcircle sector on @racket[board].
}

@defproc[(jsx-create-semicircle [board jsx-board?]
                                [parents any/c]
                                [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Semicircle"
          (jsx-doc-url "Semicircle"))
Creates a semicircle on @racket[board].
}

@defproc[(jsx-create-majorarc [board jsx-board?]
                              [parents any/c]
                              [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "MajorArc"
          (jsx-doc-url "MajorArc"))
Creates a major arc on @racket[board].
}

@defproc[(jsx-create-majorsector [board jsx-board?]
                                 [parents any/c]
                                 [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "MajorSector"
          (jsx-doc-url "MajorSector"))
Creates a major sector on @racket[board].
}

@defproc[(jsx-create-curveintersection [board jsx-board?]
                                       [parents any/c]
                                       [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "CurveIntersection"
          (jsx-doc-url "CurveIntersection"))
Creates a curve intersection on @racket[board].
}

@defproc[(jsx-create-curvedifference [board jsx-board?]
                                     [parents any/c]
                                     [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "CurveDifference"
          (jsx-doc-url "CurveDifference"))
Creates a curve difference on @racket[board].
}

@defproc[(jsx-create-curveunion [board jsx-board?]
                                [parents any/c]
                                [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "CurveUnion"
          (jsx-doc-url "CurveUnion"))
Creates a curve union on @racket[board].
}

@defproc[(jsx-create-derivative [board jsx-board?]
                                [parents any/c]
                                [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Derivative"
          (jsx-doc-url "Derivative"))
Creates a derivative curve on @racket[board].
}

@defproc[(jsx-create-integral [board jsx-board?]
                              [parents any/c]
                              [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Integral"
          (jsx-doc-url "Integral"))
Creates an integral on @racket[board].
}

@defproc[(jsx-create-riemannsum [board jsx-board?]
                                [parents any/c]
                                [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Riemannsum"
          (jsx-doc-url "Riemannsum"))
Creates a Riemann sum visualization on @racket[board].
}

@defproc[(jsx-riemannsum-value [riemannsum jsx-element?])
         any/c]{
@(jsx-bar "Value"
          (string-append (jsx-doc-url "Riemannsum")
                         "#Value"))
Reads the current value of a Riemann sum visualization.
}

@defproc[(jsx-create-slopefield [board jsx-board?]
                                [parents any/c]
                                [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Slopefield"
          (jsx-doc-url "Slopefield"))
Creates a slope field on @racket[board].
}

@defproc[(jsx-slopefield-set-f! [field jsx-element?]
                                [func any/c])
         void?]{
@(jsx-bar "setF"
          (string-append (jsx-doc-url "Slopefield")
                         "#setF"))
Updates the defining function of a slope field.
}

@defproc[(jsx-create-vectorfield [board jsx-board?]
                                 [parents any/c]
                                 [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Vectorfield"
          (jsx-doc-url "Vectorfield"))
Creates a vector field on @racket[board].
}

@defproc[(jsx-vectorfield-set-f! [field jsx-element?]
                                 [func any/c])
         void?]{
@(jsx-bar "setF"
          (string-append (jsx-doc-url "Vectorfield")
                         "#setF"))
Updates the defining function of a vector field.
}

@defproc[(jsx-create-implicitcurve [board jsx-board?]
                                   [parents any/c]
                                   [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "ImplicitCurve"
          (jsx-doc-url "ImplicitCurve"))
Creates an implicit curve on @racket[board].
}

@defproc[(jsx-create-spline [board jsx-board?]
                            [parents any/c]
                            [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Spline"
          (jsx-doc-url "Spline"))
Creates a spline on @racket[board].
}

@defproc[(jsx-create-cardinalspline [board jsx-board?]
                                    [parents any/c]
                                    [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Cardinalspline"
          (jsx-doc-url "Cardinalspline"))
Creates a cardinal spline on @racket[board].
}

@defproc[(jsx-create-comb [board jsx-board?]
                          [parents any/c]
                          [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Comb"
          (jsx-doc-url "Comb"))
Creates a comb on @racket[board].
}

@defproc[(jsx-create-metapostspline [board jsx-board?]
                                    [parents any/c]
                                    [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Metapostspline"
          (jsx-doc-url "Metapostspline"))
Creates a metapost spline on @racket[board].
}

@defproc[(jsx-create-polygonalchain [board jsx-board?]
                                    [parents any/c]
                                    [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "PolygonalChain"
          (jsx-doc-url "PolygonalChain"))
Creates a polygonal chain on @racket[board].
}

@defproc[(jsx-create-regularpolygon [board jsx-board?]
                                    [parents any/c]
                                    [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "RegularPolygon"
          (jsx-doc-url "RegularPolygon"))
Creates a regular polygon on @racket[board].
}

@defproc[(jsx-create-hyperbola [board jsx-board?]
                               [parents any/c]
                               [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Hyperbola"
          (jsx-doc-url "Hyperbola"))
Creates a hyperbola on @racket[board].
}

@defproc[(jsx-create-parabola [board jsx-board?]
                              [parents any/c]
                              [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Parabola"
          (jsx-doc-url "Parabola"))
Creates a parabola on @racket[board].
}

@defproc[(jsx-create-stepfunction [board jsx-board?]
                                  [parents any/c]
                                  [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Stepfunction"
          (jsx-doc-url "Stepfunction"))
Creates a step function on @racket[board].
}

@defproc[(jsx-create-inequality [board jsx-board?]
                                [parents any/c]
                                [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Inequality"
          (jsx-doc-url "Inequality"))
Creates an inequality visualization on @racket[board].
}

@defproc[(jsx-create-turtle [board jsx-board?]
                            [parents any/c]
                            [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Turtle"
          (jsx-doc-url "Turtle"))
Creates a turtle on @racket[board].
}

@defproc[(jsx-create-perpendicular [board jsx-board?]
                                   [parents any/c]
                                   [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Perpendicular"
          (jsx-doc-url "Perpendicular"))
Creates a perpendicular line on @racket[board].
}

@defproc[(jsx-create-reflection [board jsx-board?]
                                [parents any/c]
                                [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Reflection"
          (jsx-doc-url "Reflection"))
Creates a reflection on @racket[board].
}

@defproc[(jsx-create-bisector [board jsx-board?]
                              [parents any/c]
                              [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Bisector"
          (jsx-doc-url "Bisector"))
Creates a bisector on @racket[board].
}

@defproc[(jsx-create-normal [board jsx-board?]
                            [parents any/c]
                            [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Normal"
          (jsx-doc-url "Normal"))
Creates a normal line on @racket[board].
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

@defproc[(jsx-create-intersection [board jsx-board?]
                                  [parents any/c]
                                  [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Intersection"
          (jsx-doc-url "Intersection"))
Creates an intersection point on @racket[board].
}

@defproc[(jsx-create-arrow [board jsx-board?]
                           [parents any/c]
                           [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Arrow"
          (jsx-doc-url "Arrow"))
Creates an arrow on @racket[board].
}

@defproc[(jsx-create-button [board jsx-board?]
                            [parents any/c]
                            [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Button"
          (jsx-doc-url "Button"))
Creates a button on @racket[board].
}

@defproc[(jsx-create-checkbox [board jsx-board?]
                              [parents any/c]
                              [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Checkbox"
          (jsx-doc-url "Checkbox"))
Creates a checkbox on @racket[board].
}

@section{Checkbox Helpers}

@defproc[(jsx-checkbox-value [checkbox jsx-element?])
         boolean?]{
@(jsx-bar "Value"
          (string-append (jsx-doc-url "Checkbox")
                         "#Value"))
Returns the current checkbox value.
}

@defproc[(jsx-create-input [board jsx-board?]
                           [parents any/c]
                           [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Input"
          (jsx-doc-url "Input"))
Creates an input field on @racket[board].
}

@section{Input Helpers}

@defproc[(jsx-input-set! [input jsx-element?]
                         [value any/c])
         jsx-element?]{
@(jsx-bar "set"
          (string-append (jsx-doc-url "Input")
                         "#set"))
Sets the current value of @racket[input].
}

@defproc[(jsx-input-value [input jsx-element?])
         string?]{
@(jsx-bar "Value"
          (string-append (jsx-doc-url "Input")
                         "#Value"))
Returns the current input value.
}

@defproc[(jsx-create-slider [board jsx-board?]
                            [parents any/c]
                            [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Slider"
          (jsx-doc-url "Slider"))
Creates a slider on @racket[board].
}

@section{Slider Helpers}

@defproc[(jsx-slider-set-max! [slider jsx-element?]
                              [value any/c])
         jsx-element?]{
@(jsx-bar "setMax"
          (string-append (jsx-doc-url "Slider")
                         "#setMax"))
Sets the maximum slider value.
}

@defproc[(jsx-slider-set-min! [slider jsx-element?]
                              [value any/c])
         jsx-element?]{
@(jsx-bar "setMin"
          (string-append (jsx-doc-url "Slider")
                         "#setMin"))
Sets the minimum slider value.
}

@defproc[(jsx-slider-set-value! [slider jsx-element?]
                                [value any/c])
         jsx-element?]{
@(jsx-bar "setValue"
          (string-append (jsx-doc-url "Slider")
                         "#setValue"))
Sets the current slider value.
}

@defproc[(jsx-slider-value [slider jsx-element?])
         number?]{
@(jsx-bar "Value"
          (string-append (jsx-doc-url "Slider")
                         "#Value"))
Returns the current slider value.
}

@defproc[(jsx-create-chart [board jsx-board?]
                           [parents any/c]
                           [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Chart"
          (jsx-doc-url "Chart"))
Creates a chart on @racket[board].
}

@section{Chart Helpers}

The @racket[jsx-chart] wrappers expose the documented
@racketid[JXG.Chart] draw and update methods. These methods mirror the
JSXGraph API closely and return the raw browser result.

@defproc[(jsx-chart-draw-bar [chart jsx-element?]
                             [args any/c] ...)
         any/c]{
@(jsx-bar "drawBar"
          (string-append (jsx-doc-url "Chart")
                         "#drawBar"))
Draws a bar chart rendering.
}

@defproc[(jsx-chart-draw-fit [chart jsx-element?]
                             [args any/c] ...)
         any/c]{
@(jsx-bar "drawFit"
          (string-append (jsx-doc-url "Chart")
                         "#drawFit"))
Draws a fit chart rendering.
}

@defproc[(jsx-chart-draw-line [chart jsx-element?]
                              [args any/c] ...)
         any/c]{
@(jsx-bar "drawLine"
          (string-append (jsx-doc-url "Chart")
                         "#drawLine"))
Draws a line chart rendering.
}

@defproc[(jsx-chart-draw-pie [chart jsx-element?]
                             [args any/c] ...)
         any/c]{
@(jsx-bar "drawPie"
          (string-append (jsx-doc-url "Chart")
                         "#drawPie"))
Draws a pie chart rendering.
}

@defproc[(jsx-chart-draw-points [chart jsx-element?]
                                [args any/c] ...)
         any/c]{
@(jsx-bar "drawPoints"
          (string-append (jsx-doc-url "Chart")
                         "#drawPoints"))
Draws a point chart rendering.
}

@defproc[(jsx-chart-draw-radar [chart jsx-element?]
                               [args any/c] ...)
         any/c]{
@(jsx-bar "drawRadar"
          (string-append (jsx-doc-url "Chart")
                         "#drawRadar"))
Draws a radar chart rendering.
}

@defproc[(jsx-chart-draw-spline [chart jsx-element?]
                                [args any/c] ...)
         any/c]{
@(jsx-bar "drawSpline"
          (string-append (jsx-doc-url "Chart")
                         "#drawSpline"))
Draws a spline chart rendering.
}

@defproc[(jsx-chart-update-data-array! [chart jsx-element?]
                                       [args any/c] ...)
         any/c]{
@(jsx-bar "updateDataArray"
          (string-append (jsx-doc-url "Chart")
                         "#updateDataArray"))
Updates the chart data array.
}

@defproc[(jsx-chart-update-renderer! [chart jsx-element?]
                                     [args any/c] ...)
         any/c]{
@(jsx-bar "updateRenderer"
          (string-append (jsx-doc-url "Chart")
                         "#updateRenderer"))
Updates the chart renderer.
}

@defproc[(jsx-create-legend [board jsx-board?]
                            [parents any/c]
                            [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Legend"
          (jsx-doc-url "Legend"))
Creates a legend on @racket[board].
}

@defproc[(jsx-create-smartlabel [board jsx-board?]
                                [parents any/c]
                                [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Smartlabel"
          (jsx-doc-url "Smartlabel"))
Creates a smart label on @racket[board].
}

@defproc[(jsx-create-text [board jsx-board?]
                          [parents any/c]
                          [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Text"
          (jsx-doc-url "Text"))
Creates a text element on @racket[board].
}

@defproc[(jsx-create-image [board jsx-board?]
                           [parents any/c]
                           [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Image"
          (jsx-doc-url "Image"))
Creates an image element on @racket[board].
}

@defproc[(jsx-create-group [board jsx-board?]
                           [parents any/c]
                           [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Group"
          (jsx-doc-url "JXG.Group"))
Creates a group on @racket[board].
}

@defproc[(jsx-create-foreignobject [board jsx-board?]
                                   [parents any/c]
                                   [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "ForeignObject"
          (jsx-doc-url "ForeignObject"))
Creates a foreign object on @racket[board].
}

@defproc[(jsx-create-tapemeasure [board jsx-board?]
                                 [parents any/c]
                                 [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Tapemeasure"
          (jsx-doc-url "Tapemeasure"))
Creates a tape measure on @racket[board].
}

@defproc[(jsx-tapemeasure-value [tapemeasure jsx-element?])
         any/c]{
@(jsx-bar "Value"
          (string-append (jsx-doc-url "Tapemeasure")
                         "#Value"))
Returns the current tape-measure value.
}

@defproc[(jsx-create-measurement [board jsx-board?]
                                 [parents any/c]
                                 [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Measurement"
          (jsx-doc-url "Measurement"))
Creates a measurement on @racket[board].
}

@defproc[(jsx-create-circumcenter [board jsx-board?]
                                  [parents any/c]
                                  [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Circumcenter"
          (jsx-doc-url "Circumcenter"))
Creates a circumcenter point on @racket[board].
}

@defproc[(jsx-create-mirrorelement [board jsx-board?]
                                   [parents any/c]
                                   [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "MirrorElement"
          (jsx-doc-url "MirrorElement"))
Creates a mirrored element on @racket[board].
}

@defproc[(jsx-create-mirrorpoint [board jsx-board?]
                                 [parents any/c]
                                 [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "MirrorPoint"
          (jsx-doc-url "MirrorPoint"))
Creates a mirror point on @racket[board].
}

@defproc[(jsx-create-otherintersection [board jsx-board?]
                                       [parents any/c]
                                       [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "OtherIntersection"
          (jsx-doc-url "OtherIntersection"))
Creates the other intersection on @racket[board].
}

@defproc[(jsx-create-orthogonalprojection [board jsx-board?]
                                          [parents any/c]
                                          [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Orthogonalprojection"
          (jsx-doc-url "Orthogonalprojection"))
Creates an orthogonal projection on @racket[board].
}

@defproc[(jsx-create-parallelpoint [board jsx-board?]
                                   [parents any/c]
                                   [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Parallelpoint"
          (jsx-doc-url "Parallelpoint"))
Creates a parallel point on @racket[board].
}

@defproc[(jsx-create-perpendicularpoint [board jsx-board?]
                                        [parents any/c]
                                        [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "PerpendicularPoint"
          (jsx-doc-url "PerpendicularPoint"))
Creates a perpendicular point on @racket[board].
}

@section{ForeignObject Helpers}

@defproc[(jsx-foreignobject-H [foreignobject jsx-element?]
                              [args any/c] ...)
         any/c]{
@(jsx-bar "H"
          (string-append (jsx-doc-url "ForeignObject")
                         "#H"))
Returns the foreign object H helper.
}

@defproc[(jsx-foreignobject-W [foreignobject jsx-element?]
                              [args any/c] ...)
         any/c]{
@(jsx-bar "W"
          (string-append (jsx-doc-url "ForeignObject")
                         "#W"))
Returns the foreign object W helper.
}

@defproc[(jsx-foreignobject-has-point? [foreignobject jsx-element?]
                                       [args any/c] ...)
         boolean?]{
@(jsx-bar "hasPoint"
          (string-append (jsx-doc-url "ForeignObject")
                         "#hasPoint"))
Checks whether a point hits @racket[foreignobject].
}

@defproc[(jsx-foreignobject-set-size! [foreignobject jsx-element?]
                                      [args any/c] ...)
         void?]{
@(jsx-bar "setSize"
          (string-append (jsx-doc-url "ForeignObject")
                         "#setSize"))
Sets the size of @racket[foreignobject].
}

@defproc[(jsx-foreignobject-update! [foreignobject jsx-element?]
                                    [args any/c] ...)
         void?]{
@(jsx-bar "update"
          (string-append (jsx-doc-url "ForeignObject")
                         "#update"))
Updates @racket[foreignobject].
}

@defproc[(jsx-foreignobject-update-renderer! [foreignobject jsx-element?]
                                             [args any/c] ...)
         void?]{
@(jsx-bar "updateRenderer"
          (string-append (jsx-doc-url "ForeignObject")
                         "#updateRenderer"))
Refreshes the foreign object renderer.
}

@defproc[(jsx-foreignobject-update-size! [foreignobject jsx-element?]
                                         [args any/c] ...)
         void?]{
@(jsx-bar "updateSize"
          (string-append (jsx-doc-url "ForeignObject")
                         "#updateSize"))
Updates the foreign object size.
}

@defproc[(jsx-foreignobject-update-span! [foreignobject jsx-element?]
                                         [args any/c] ...)
         void?]{
@(jsx-bar "updateSpan"
          (string-append (jsx-doc-url "ForeignObject")
                         "#updateSpan"))
Updates the foreign object span.
}

@section{Text Helpers}

The @racket[jsx-text] wrappers expose the documented
@racketid[JXG.Text] methods that are specific to text elements.
Inherited geometry and coordinate helpers are provided through the
generic @racket[jsx-element] wrappers.

@defproc[(jsx-text-_createFctUpdateText [text jsx-element?]
                                        [args any/c] ...)
         any/c]{
@(jsx-bar "_createFctUpdateText"
          (string-append (jsx-doc-url "JXG.Text")
                         "#_createFctUpdateText"))
Creates the internal update function used for text rendering.
}

@defproc[(jsx-text-_setText [text jsx-element?]
                            [args any/c] ...)
         any/c]{
@(jsx-bar "_setText"
          (string-append (jsx-doc-url "JXG.Text")
                         "#_setText"))
Sets the internal text representation directly.
}

@defproc[(jsx-text-bounds [text jsx-element?]
                          [args any/c] ...)
         any/c]{
@(jsx-bar "bounds"
          (string-append (jsx-doc-url "JXG.Text")
                         "#bounds"))
Returns the text bounds.
}

@defproc[(jsx-text-checkForSizeUpdate [text jsx-element?]
                                      [args any/c] ...)
         any/c]{
@(jsx-bar "checkForSizeUpdate"
          (string-append (jsx-doc-url "JXG.Text")
                         "#checkForSizeUpdate"))
Checks whether the text size needs recomputation.
}

@defproc[(jsx-text-convertGeonext2CSS [text jsx-element?]
                                      [args any/c] ...)
         any/c]{
@(jsx-bar "convertGeonext2CSS"
          (string-append (jsx-doc-url "JXG.Text")
                         "#convertGeonext2CSS"))
Converts GEONExT markup to CSS.
}

@defproc[(jsx-text-convertGeonextAndSketchometry2CSS [text jsx-element?]
                                                     [args any/c] ...)
         any/c]{
@(jsx-bar "convertGeonextAndSketchometry2CSS"
          (string-append (jsx-doc-url "JXG.Text")
                         "#convertGeonextAndSketchometry2CSS"))
Converts GEONExT and Sketchometry markup to CSS.
}

@defproc[(jsx-text-convertSketchometry2CSS [text jsx-element?]
                                           [args any/c] ...)
         any/c]{
@(jsx-bar "convertSketchometry2CSS"
          (string-append (jsx-doc-url "JXG.Text")
                         "#convertSketchometry2CSS"))
Converts Sketchometry markup to CSS.
}

@defproc[(jsx-text-crudeSizeEstimate [text jsx-element?]
                                     [args any/c] ...)
         any/c]{
@(jsx-bar "crudeSizeEstimate"
          (string-append (jsx-doc-url "JXG.Text")
                         "#crudeSizeEstimate"))
Returns a crude size estimate for the text.
}

@defproc[(jsx-text-escapeTicks [text jsx-element?]
                               [args any/c] ...)
         any/c]{
@(jsx-bar "escapeTicks"
          (string-append (jsx-doc-url "JXG.Text")
                         "#escapeTicks"))
Escapes tick marks in the text.
}

@defproc[(jsx-text-expandShortMath [text jsx-element?]
                                   [args any/c] ...)
         any/c]{
@(jsx-bar "expandShortMath"
          (string-append (jsx-doc-url "JXG.Text")
                         "#expandShortMath"))
Expands short math notation in the text.
}

@defproc[(jsx-text-generateTerm [text jsx-element?]
                                [args any/c] ...)
         any/c]{
@(jsx-bar "generateTerm"
          (string-append (jsx-doc-url "JXG.Text")
                         "#generateTerm"))
Generates the text term.
}

@defproc[(jsx-text-getAnchorX [text jsx-element?]
                              [args any/c] ...)
         any/c]{
@(jsx-bar "getAnchorX"
          (string-append (jsx-doc-url "JXG.Text")
                         "#getAnchorX"))
Returns the X anchor position for the text.
}

@defproc[(jsx-text-getAnchorY [text jsx-element?]
                              [args any/c] ...)
         any/c]{
@(jsx-bar "getAnchorY"
          (string-append (jsx-doc-url "JXG.Text")
                         "#getAnchorY"))
Returns the Y anchor position for the text.
}

@defproc[(jsx-text-getNumberOfConflicts [text jsx-element?]
                                        [args any/c] ...)
         any/c]{
@(jsx-bar "getNumberOfConflicts"
          (string-append (jsx-doc-url "JXG.Text")
                         "#getNumberOfConflicts"))
Returns the number of text placement conflicts.
}

@defproc[(jsx-text-getSize [text jsx-element?]
                           [args any/c] ...)
         any/c]{
@(jsx-bar "getSize"
          (string-append (jsx-doc-url "JXG.Text")
                         "#getSize"))
Returns the size of the text.
}

@defproc[(jsx-text-hasPoint [text jsx-element?]
                            [args any/c] ...)
         any/c]{
@(jsx-bar "hasPoint"
          (string-append (jsx-doc-url "JXG.Text")
                         "#hasPoint"))
Checks whether screen coordinates hit the text.
}

@defproc[(jsx-text-notifyParents [text jsx-element?]
                                 [args any/c] ...)
         any/c]{
@(jsx-bar "notifyParents"
          (string-append (jsx-doc-url "JXG.Text")
                         "#notifyParents"))
Notifies parent elements that the text changed.
}

@defproc[(jsx-text-poorMansTeX [text jsx-element?]
                               [args any/c] ...)
         any/c]{
@(jsx-bar "poorMansTeX"
          (string-append (jsx-doc-url "JXG.Text")
                         "#poorMansTeX"))
Renders poor-man's TeX markup.
}

@defproc[(jsx-text-replaceSub [text jsx-element?]
                              [args any/c] ...)
         any/c]{
@(jsx-bar "replaceSub"
          (string-append (jsx-doc-url "JXG.Text")
                         "#replaceSub"))
Replaces a subscript fragment.
}

@defproc[(jsx-text-replaceSup [text jsx-element?]
                              [args any/c] ...)
         any/c]{
@(jsx-bar "replaceSup"
          (string-append (jsx-doc-url "JXG.Text")
                         "#replaceSup"))
Replaces a superscript fragment.
}

@defproc[(jsx-text-setAutoPosition [text jsx-element?]
                                   [args any/c] ...)
         any/c]{
@(jsx-bar "setAutoPosition"
          (string-append (jsx-doc-url "JXG.Text")
                         "#setAutoPosition"))
Turns automatic positioning on or off.
}

@defproc[(jsx-text-setCoords [text jsx-element?]
                             [args any/c] ...)
         any/c]{
@(jsx-bar "setCoords"
          (string-append (jsx-doc-url "JXG.Text")
                         "#setCoords"))
Sets the text coordinates.
}

@defproc[(jsx-text-setText [text jsx-element?]
                           [args any/c] ...)
         any/c]{
@(jsx-bar "setText"
          (string-append (jsx-doc-url "JXG.Text")
                         "#setText"))
Sets the displayed text.
}

@defproc[(jsx-text-setTextJessieCode [text jsx-element?]
                                     [args any/c] ...)
         any/c]{
@(jsx-bar "setTextJessieCode"
          (string-append (jsx-doc-url "JXG.Text")
                         "#setTextJessieCode"))
Sets the text from JessieCode.
}

@defproc[(jsx-text-unescapeTicks [text jsx-element?]
                                 [args any/c] ...)
         any/c]{
@(jsx-bar "unescapeTicks"
          (string-append (jsx-doc-url "JXG.Text")
                         "#unescapeTicks"))
Unescapes tick marks in the text.
}

@defproc[(jsx-text-updateSize [text jsx-element?]
                              [args any/c] ...)
         any/c]{
@(jsx-bar "updateSize"
          (string-append (jsx-doc-url "JXG.Text")
                         "#updateSize"))
Updates the text size.
}

@defproc[(jsx-text-updateText [text jsx-element?]
                              [args any/c] ...)
         any/c]{
@(jsx-bar "updateText"
          (string-append (jsx-doc-url "JXG.Text")
                         "#updateText"))
Refreshes the rendered text.
}

@defproc[(jsx-text-utf8_decode [text jsx-element?]
                               [args any/c] ...)
         any/c]{
@(jsx-bar "utf8_decode"
          (string-append (jsx-doc-url "JXG.Text")
                         "#utf8_decode"))
Decodes UTF-8 text data.
}

@defproc[(jsx-text-valueTagToJessieCode [text jsx-element?]
                                        [args any/c] ...)
         any/c]{
@(jsx-bar "valueTagToJessieCode"
          (string-append (jsx-doc-url "JXG.Text")
                         "#valueTagToJessieCode"))
Converts a value tag to JessieCode.
}

@section{Image Helpers}

The @racket[jsx-image] wrappers expose the documented
@racketid[JXG.Image] methods that are specific to image elements.
Inherited geometry and coordinate helpers are provided through the
generic @racket[jsx-element] wrappers.

@defproc[(jsx-image-H [image any/c]
                      [args any/c] ...)
         any/c]{
@(jsx-bar "H"
          (string-append (jsx-doc-url "JXG.Image")
                         "#H"))
Reads the image H helper.
}

@defproc[(jsx-image-W [image any/c]
                      [args any/c] ...)
         any/c]{
@(jsx-bar "W"
          (string-append (jsx-doc-url "JXG.Image")
                         "#W"))
Reads the image W helper.
}

@defproc[(jsx-image-has-point? [image any/c]
                               [args any/c] ...)
         boolean?]{
@(jsx-bar "hasPoint"
          (string-append (jsx-doc-url "JXG.Image")
                         "#hasPoint"))
Tests whether a screen position hits the image.
}

@defproc[(jsx-image-set-size! [image any/c]
                              [args any/c] ...)
         void?]{
@(jsx-bar "setSize"
          (string-append (jsx-doc-url "JXG.Image")
                         "#setSize"))
Sets the image size.
}

@defproc[(jsx-image-update! [image any/c]
                            [args any/c] ...)
         void?]{
@(jsx-bar "update"
          (string-append (jsx-doc-url "JXG.Image")
                         "#update"))
Updates the image.
}

@defproc[(jsx-image-update-renderer! [image any/c]
                                     [args any/c] ...)
         void?]{
@(jsx-bar "updateRenderer"
          (string-append (jsx-doc-url "JXG.Image")
                         "#updateRenderer"))
Refreshes the image renderer.
}

@defproc[(jsx-image-update-size! [image any/c]
                                 [args any/c] ...)
         void?]{
@(jsx-bar "updateSize"
          (string-append (jsx-doc-url "JXG.Image")
                         "#updateSize"))
Updates the image size.
}

@defproc[(jsx-image-update-span! [image any/c]
                                 [args any/c] ...)
         void?]{
@(jsx-bar "updateSpan"
          (string-append (jsx-doc-url "JXG.Image")
                         "#updateSpan"))
Updates the image span.
}

@section{Group Helpers}

The @racket[jsx-group] wrappers expose the documented
@racketid[JXG.Group] methods that are useful for the gallery example.

@defproc[(jsx-group-add-parents! [group jsx-element?]
                                 [parents any/c])
         void?]{
@(jsx-bar "addParents"
          (string-append (jsx-doc-url "JXG.Group")
                         "#addParents"))
Adds parents to a group.
}

@defproc[(jsx-group-add-point! [group jsx-element?]
                               [object any/c])
         void?]{
@(jsx-bar "addPoint"
          (string-append (jsx-doc-url "JXG.Group")
                         "#addPoint"))
Adds a point to a group.
}

@defproc[(jsx-group-add-points! [group jsx-element?]
                                [objects any/c])
         void?]{
@(jsx-bar "addPoints"
          (string-append (jsx-doc-url "JXG.Group")
                         "#addPoints"))
Adds multiple points to a group.
}

@defproc[(jsx-group-add-rotation-point! [group jsx-element?]
                                        [object any/c])
         void?]{
@(jsx-bar "addRotationPoint"
          (string-append (jsx-doc-url "JXG.Group")
                         "#addRotationPoint"))
Adds a rotation point to a group.
}

@defproc[(jsx-group-add-scale-point! [group jsx-element?]
                                     [object any/c])
         void?]{
@(jsx-bar "addScalePoint"
          (string-append (jsx-doc-url "JXG.Group")
                         "#addScalePoint"))
Adds a scale point to a group.
}

@defproc[(jsx-group-add-translation-point! [group jsx-element?]
                                            [object any/c])
         void?]{
@(jsx-bar "addTranslationPoint"
          (string-append (jsx-doc-url "JXG.Group")
                         "#addTranslationPoint"))
Adds a translation point to a group.
}

@defproc[(jsx-group-set-scale-center! [group jsx-element?]
                                      [object any/c])
         void?]{
@(jsx-bar "setScaleCenter"
          (string-append (jsx-doc-url "JXG.Group")
                         "#setScaleCenter"))
Sets the scale center for a group.
}

@defproc[(jsx-group-set-rotation-center! [group jsx-element?]
                                         [object any/c])
         void?]{
@(jsx-bar "setRotationCenter"
          (string-append (jsx-doc-url "JXG.Group")
                         "#setRotationCenter"))
Sets the rotation center for a group.
}

@defproc[(jsx-group-set-rotation-points! [group jsx-element?]
                                         [objects any/c])
         void?]{
@(jsx-bar "setRotationPoints"
          (string-append (jsx-doc-url "JXG.Group")
                         "#setRotationPoints"))
Sets the rotation points for a group.
}

@defproc[(jsx-group-set-translation-points! [group jsx-element?]
                                            [objects any/c])
         void?]{
@(jsx-bar "setTranslationPoints"
          (string-append (jsx-doc-url "JXG.Group")
                         "#setTranslationPoints"))
Sets the translation points for a group.
}

@defproc[(jsx-group-update! [group jsx-element?])
         void?]{
@(jsx-bar "update"
          (string-append (jsx-doc-url "JXG.Group")
                         "#update"))
Updates the group members.
}

@section{Helper Constructors}

These constructors cover helper classes that are easiest to exercise as
standalone boards in the gallery.

@defproc[(jsx-create-ticks [board jsx-board?]
                           [parents any/c]
                           [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Ticks"
          (jsx-doc-url "Ticks"))
Creates a ticks object on a board.
}

@defproc[(jsx-create-transformation [board jsx-board?]
                                    [parents any/c]
                                    [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Transformation"
          (jsx-doc-url "Transformation"))
Creates a transformation object on a board.
}

@defproc[(jsx-create-tracecurve [board jsx-board?]
                                [parents any/c]
                                [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Tracecurve"
          (jsx-doc-url "Tracecurve"))
Creates a tracecurve object on a board.
}

@defproc[(jsx-create-parallelogram [board jsx-board?]
                                   [parents any/c]
                                   [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "Parallelogram"
          (jsx-doc-url "Parallelogram"))
Creates a parallelogram object on a board.
}

@defproc[(jsx-create-reflexangle [board jsx-board?]
                                 [parents any/c]
                                 [attributes (or/c #f any/c) #f])
         jsx-element?]{
@(jsx-bar "ReflexAngle"
          (jsx-doc-url "ReflexAngle"))
Creates a reflex angle object on a board.
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
