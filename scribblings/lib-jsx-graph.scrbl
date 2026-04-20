#lang scribble/manual

@(require scribble/manual
          (for-label (lib "scribblings/lib-jsx-graph-labels.rkt" "webracket"))
          "webracket-scribble-utils.rkt")

@title{Library: @racketid[jsx-graph]}
@declare-exporting[(lib "scribblings/lib-jsx-graph-labels.rkt" "webracket")]

@(how-to-require include-lib jsx-graph (lib "libs/jsx-graph.rkt"))
@(compile-option-bar "Compile option: " "--ffi jsxgraph")

The @racket[jsx-graph] library provides a small Rackety wrapper around
JSXGraph board and element construction. JSXGraph is an interactive
geometry system for drawing points, lines, circles, surfaces, and other
constructions on a browser board.

Use @racket[jsx-graph] when you want to:

@itemlist[
  @item{create a @racket[JXG.JSXGraph] board in the current page}
  @item{create arbitrary JSXGraph elements with @racket[jsx-create] or the specialized constructors}
  @item{create 3D scenes with @racket[jsx-create-view3d] and the 3D helpers for points, lines, circles, planes, axes, function graphs, spheres, surfaces, curves, polyhedra, text, ticks, transformations, and vector fields}
  @item{build geometry objects such as @racket[JXG.Point], @racket[JXG.Line], @racket[JXG.Arc], @racket[JXG.Angle], @racket[JXG.Sector], @racket[JXG.Circle], @racket[JXG.Conic], @racket[JXG.Ellipse], @racket[JXG.Functiongraph], @racket[JXG.Riemannsum], @racket[JXG.Slopefield], @racket[JXG.Vectorfield], @racket[JXG.ImplicitCurve], @racket[JXG.Spline], @racket[JXG.Cardinalspline], @racket[JXG.Comb], @racket[JXG.Metapostspline], @racket[JXG.PolygonalChain], @racket[JXG.RegularPolygon], @racket[JXG.Hyperbola], @racket[JXG.Parabola], @racket[JXG.Stepfunction], @racket[JXG.Inequality], @racket[JXG.Turtle], @racket[JXG.Polygon], @racket[JXG.Text], @racket[JXG.ForeignObject], @racket[JXG.Tapemeasure], @racket[JXG.Hatch], @racket[JXG.Measurement], @racket[JXG.Circumcenter], @racket[JXG.Incenter], @racket[JXG.MinorArc], @racket[JXG.MinorSector], @racket[JXG.NonReflexAngle], @racket[JXG.Slopetriangle], and @racket[JXG.MirrorElement]}
  @item{create chart objects with @racket[JXG.Chart]}
  @item{create legends and smart labels}
  @item{create widget-like elements such as buttons, checkboxes, sliders, and inputs}
  @item{inspect or adjust point properties from Racket code}
  @item{attach browser event handlers to JSXGraph elements}
]

The library keeps the browser FFI bindings behind checked helper
functions. For the underlying @racketid[js-jsx-*] bindings, see
@racket[ffi/jsxgraph.ffi] and the corresponding browser API reference
page.

The main constructors return checked wrapper structs:
@racket[jsx-board] for JSXGraph boards, @racket[jsx-point] for point
values, and @racket[jsx-element] for other geometry objects.

Type notes for the wrapped API:

@itemlist[
  @item{Board and element arguments use the checked wrappers in the public API.}
  @item{Constructor @racket[parents] are usually packed with @racket[jsx-parents] and therefore arrive as vectors.}
  @item{Special constructors that accept callable parents are documented explicitly below.}
  @item{Constructor @racket[attributes] are JSXGraph option objects, usually built with @racket[jsx-attributes].}
  @item{A few low-level geometry hooks intentionally stay broad because JSXGraph forwards browser values through them.}
  @item{@racket[jsx-parents] is the intended variadic packing boundary, so it accepts mixed raw values and checked wrappers.}
  @item{jsx-point? is a predicate over any value, so its argument stays broad by design.}
]

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
the vector shape JSXGraph expects, and @racket[jsx-attributes] to build
option objects when you need them.

Use @racket[jsx-parents] for constructor inputs that describe geometry
relationships, such as points, parent elements, or callable parents.
Use @racket[jsx-attributes] for option objects that control appearance
or behavior, such as names, colors, sizes, grids, and board settings.

A board with explicit options looks like this:

@racketblock[
(include-lib jsx-graph)

(define board
  (jsx-create-board "box"
                    (jsx-attributes
                      [boundingbox #[-5 5 5 -5]]
                      [axis #t]
                      [grid #t]
                      [keepaspectratio #t])))
]

@section{Common Patterns}

The most common JSXGraph tasks in this library fall into a few simple
patterns.

@subsection{Board, Points, And Line}

Create a board, place two points, and connect them with a line.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define p (jsx-create-point board (jsx-parents -1 0)))
(define q (jsx-create-point board (jsx-parents 1 0)))
(define l (jsx-create-line board (jsx-parents p q)))

(void board p q l)
]

@subsection{Board Options}

Use @racket[jsx-attributes] for board options such as the bounding box,
axes, and grid.

@racketblock[
(include-lib jsx-graph)

(define board
  (jsx-create-board "box"
                    (jsx-attributes
                      [boundingbox #[-5 5 5 -5]]
                      [axis #t]
                      [grid #t]
                      [keepaspectratio #t])))
]

@subsection{3D View}

Create a 3D scene inside a normal board, then add a 3D point.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box3d"))
(define view
  (jsx-create-view3d board
                     (jsx-parents (vector -4 -3)
                                  (vector 8 8)
                                  (vector (vector -5 5)
                                          (vector -5 5)
                                          (vector -5 5)))
                     (jsx-attributes [projection "parallel"])))
(define p
  (jsx-view3d-create-point3d view (vector 1 2 2)
                             (jsx-attributes [name "A"])))
(void board view p)
]

@defproc[(jsx-create-view3d [board jsx-board?]
                            [parents any/c]
                            [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "View3D"
          (jsx-doc-url "View3D"))
Creates a 3D scene inside a board.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box3d"))
(define view
  (jsx-create-view3d board
                     (jsx-parents (vector -4 -3)
                                  (vector 8 8)
                                  (vector (vector -5 5)
                                          (vector -5 5)
                                          (vector -5 5)))
                     (jsx-attributes [projection "parallel"])))
]
}

@subsection{Event Handler}

Attach a browser event handler to a board or geometry element.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(jsx-board-add-event!
 board
 "mousedown"
 (lambda args
   (console-log "board clicked")))
]

@section{Parent Shapes}

Use @racket[jsx-parents] when a constructor needs geometry parents.

@itemlist[
  @item{For points, pass a coordinate pair such as @racket[(jsx-parents -1 0)].}
  @item{For lines and polygons, pass parent points or a vector/list of points.}
  @item{For @racket[Curve] and @racket[Functiongraph], a callable parent is also allowed.}
]

@section{Option Objects}

Use @racket[jsx-attributes] for JSXGraph option objects.

@itemlist[
  @item{@bold{Board:} @racket[boundingbox], @racket[axis], @racket[grid], @racket[keepaspectratio].}
  @item{@bold{Point and line:} @racket[name], @racket[size], @racket[face], @racket[strokeColor], @racket[dash], @racket[straightFirst], @racket[straightLast].}
  @item{@bold{3D view:} @racket[projection], @racket[trackball], and the plane visibility flags used by JSXGraph's 3D view options.}
]

@defform[(jsx-attributes [key value] ...)]{
Build a JSXGraph option object from option-style pairs.

Each @racket[key] should be an option name. In practice that is usually
written as an identifier, but strings and symbols also work. Each
@racket[value] may be any expression, including nested
@racket[jsx-attributes] forms.

Kebab-style option names such as @racket[straight-first] and
@racket[stroke-color] are rewritten to the camelCase spellings that
JSXGraph expects. Plain all-lowercase names such as
@racket[boundingbox] stay unchanged.

Common board options include:
@itemlist[
  @item{@racket[boundingbox] for the visible board rectangle}
  @item{@racket[axis] to show or hide the coordinate axes}
  @item{@racket[grid] to show or hide the board grid}
  @item{@racket[keepaspectratio] to lock the board aspect ratio}
]

The same form also works for element option objects, for example:
@racketblock[
(jsx-attributes
  [name "A"]
  [size 4]
  [visible #t]
  [stroke-color "crimson"])
]

For a point, the same style can be used with the point constructor:

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define p
  (jsx-create-point board
                    (jsx-parents -1 0)
                    (jsx-attributes
                      [name "A"]
                      [size 4]
                      [face "o"]
                      [stroke-color "crimson"])))
]

For a line, the same pattern gives you a styled segment or infinite
line depending on the line options:

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define p (jsx-create-point board (jsx-parents -1 0)))
(define q (jsx-create-point board (jsx-parents 1 0)))
(define l
  (jsx-create-line board
                   (jsx-parents p q)
                   (jsx-attributes
                     [straight-first #t]
                     [straight-last #t]
                     [stroke-color "crimson"]
[dash 2])))
]
]
}

@section{Gallery Map}

The gallery in @racket[lib/web-easy/examples/jsx-graph-gallery/jsx-graph-gallery.rkt] shows the same ideas in separate boards.
Use it as a visual index when you want to see a constructor family in
action before reading the full reference entry.

@itemlist[
  @item{@bold{Geometry basics:} the geometry, point, line, arc, angle, sector, circle, and glider boards show the core 2D constructors.}
  @item{@bold{Construction helpers:} midpoint, parallel, perpendicular, reflection, bisector, incircle, incenter, minor arc, minor sector, non-reflex angle, hatch, measurement, circumcenter, and related helper boards show the derived constructions.}
  @item{@bold{Widgets and text:} the widgets, annotation, button, checkbox, input, slider, smartlabel, foreign object, image, tape measure, and text boards show the UI-style elements.}
  @item{@bold{Analysis and curves:} the functiongraph, curve, derivative, integral, riemannsum, slopefield, vectorfield, implicitcurve, spline, cardinalspline, comb, metapostspline, and polygonal chain boards show sampled and analytic curves.}
  @item{@bold{3D:} the view3d, point3d, line3d, circle3d, plane3d, axes3d, axis3d, functiongraph3d, curve3d, surface3d, parametricsurface3d, polyhedron3d, face3d, ticks3d, transformation3d, and vectorfield3d boards show the 3D view and its scene elements.}
]

@itemlist[
  @item{Use @racket[jsx-attributes] for styling and behavior options, not for geometry parents.}
  @item{Use @racket[jsx-parents] for geometry relationships, not for appearance options.}
  @item{Put 3D view options on @racket[jsx-create-view3d], not on the outer board.}
]

@section{Wrapped 2D Classes}

The table below lists the wrapped 2D constructors in alphabetical
order and gives a short description of what each class is for.

@tabular[
 (list (list @bold{Class} @bold{What it is for})
       (list @racketid{Angle} "Filled angle wedge.")
       (list @racketid{Arc} "Circular arc defined by three points.")
       (list @racketid{Arrow} "Arrow annotation or vector.")
       (list @racketid{Arrowparallel} "Parallel arrow construction.")
       (list @racketid{Axis} "Axis decoration and labeling.")
       (list @racketid{Bisector} "Angle bisector.")
       (list @racketid{Bisectorlines} "Angle bisector helper lines.")
       (list @racketid{Boxplot} "Box-and-whisker plot.")
       (list @racketid{Button} "Clickable button widget.")
       (list @racketid{Cardinalspline} "Cardinal spline curve.")
       (list @racketid{Chart} "Chart/plot container.")
       (list @racketid{Checkbox} "Boolean toggle widget.")
       (list @racketid{Circle} "Circle from center/radius parents.")
       (list @racketid{Circumcenter} "Circumcenter of a triangle.")
       (list @racketid{Circumcircle} "Circle through triangle vertices.")
       (list @racketid{CircumcircleArc} "Arc on a circumcircle.")
       (list @racketid{CircumcircleSector} "Sector on a circumcircle.")
       (list @racketid{Comb} "Comb-like curve helper.")
       (list @racketid{Conic} "General conic section.")
       (list @racketid{Curve} "Sampled or parametric curve.")
       (list @racketid{CurveDifference} "Difference of two curve regions.")
       (list @racketid{CurveIntersection} "Intersection of two curves.")
       (list @racketid{CurveUnion} "Union of two curve regions.")
       (list @racketid{Derivative} "Derived function graph.")
       (list @racketid{Ellipse} "Ellipse construction.")
       (list @racketid{ForeignObject} "Embedded foreign DOM content.")
       (list @racketid{Functiongraph} "Graph of a function.")
       (list @racketid{Glider} "Point constrained to a curve or line.")
       (list @racketid{Grid} "Board grid decoration.")
       (list @racketid{Group} "Grouped geometry elements.")
       (list @racketid{Hatch} "Hatched line decoration.")
       (list @racketid{Hyperbola} "Hyperbola graph.")
       (list @racketid{Image} "Placed image element.")
       (list @racketid{ImplicitCurve} "Implicitly defined curve.")
       (list @racketid{Incenter} "Incenter of a triangle.")
       (list @racketid{Incircle} "Circle tangent to triangle sides.")
       (list @racketid{Inequality} "Inequality shading or region.")
       (list @racketid{Input} "Text input widget.")
       (list @racketid{Integral} "Integral graph or area helper.")
       (list @racketid{Intersection} "Intersection point of objects.")
       (list @racketid{Legend} "Chart legend object.")
       (list @racketid{Line} "Infinite line through parent points.")
       (list @racketid{MajorArc} "Major arc between circle points.")
       (list @racketid{MajorSector} "Major circular sector.")
       (list @racketid{Measurement} "Measurement label and helper.")
       (list @racketid{Midpoint} "Midpoint of two points.")
       (list @racketid{MinorArc} "Minor arc between circle points.")
       (list @racketid{MinorSector} "Minor sector of a circle.")
       (list @racketid{MirrorElement} "Mirror of an element.")
       (list @racketid{MirrorPoint} "Mirror of a point.")
       (list @racketid{NonReflexAngle} "Non-reflex angle helper.")
       (list @racketid{Normal} "Normal line construction.")
       (list @racketid{OtherIntersection} "The second intersection of two objects.")
       (list @racketid{Parabola} "Parabola graph.")
       (list @racketid{Parallel} "Parallel line construction.")
       (list @racketid{Parallelogram} "Parallelogram construction.")
       (list @racketid{Parallelpoint} "Point on a line parallel to another.")
       (list @racketid{Perpendicular} "Perpendicular line construction.")
       (list @racketid{PerpendicularPoint} "Point dropped perpendicularly to a line.")
       (list @racketid{PerpendicularSegment} "Perpendicular segment helper.")
       (list @racketid{Point} "Basic draggable point.")
       (list @racketid{PolarLine} "Polar line of a point and circle.")
       (list @racketid{PolePoint} "Pole point of a line and circle.")
       (list @racketid{Polygon} "Polygon built from vertices.")
       (list @racketid{PolygonalChain} "Chain of connected line segments.")
       (list @racketid{RadicalAxis} "Radical axis of two circles.")
       (list @racketid{Reflection} "Reflection across a line.")
       (list @racketid{ReflexAngle} "Reflex angle helper.")
       (list @racketid{RegularPolygon} "Regular polygon construction.")
       (list @racketid{Riemannsum} "Riemann sum visualizer.")
       (list @racketid{Sector} "Filled circular sector.")
       (list @racketid{Segment} "Finite line segment.")
       (list @racketid{Semicircle} "Half-circle construction.")
       (list @racketid{Slider} "Numeric slider widget.")
       (list @racketid{Slopefield} "Slope field for differential equations.")
       (list @racketid{Slopetriangle} "Slope triangle helper.")
       (list @racketid{Smartlabel} "Automatically updated label.")
       (list @racketid{Spline} "Spline curve.")
       (list @racketid{Stepfunction} "Step function graph.")
       (list @racketid{Tangent} "Tangent line construction.")
       (list @racketid{TangentTo} "Tangent-to-point construction.")
       (list @racketid{Tapemeasure} "Interactive tape-measure tool.")
       (list @racketid{Text} "Text element anchored to the board.")
       (list @racketid{Ticks} "Tick mark decoration.")
       (list @racketid{Tracecurve} "Trace curve helper.")
       (list @racketid{Transformation} "Geometry transformation helper.")
       (list @racketid{Turtle} "Turtle-graphics drawing helper.")
       (list @racketid{Vectorfield} "Vector field visualization."))]

@section{Wrapped 3D Classes}

The table below lists the wrapped 3D view and element classes in
alphabetical order with a short description for each one.

@tabular[
 (list (list @bold{Class} @bold{What it is for})
       (list @racketid{Axes3D} "Three-axis 3D coordinate frame.")
       (list @racketid{Axis3D} "Single 3D axis.")
       (list @racketid{Circle3D} "Circle inside a 3D view.")
       (list @racketid{Curve3D} "3D parametric curve.")
       (list @racketid{Face3D} "Face of a polyhedron.")
       (list @racketid{Functiongraph3D} "3D function graph.")
       (list @racketid{IntersectionCircle3D} "Intersection circle in 3D.")
       (list @racketid{IntersectionLine3D} "Intersection line in 3D.")
       (list @racketid{Line3D} "Line in 3D space.")
       (list @racketid{ParametricSurface3D} "Parametric surface helper.")
       (list @racketid{Plane3D} "Plane in 3D space.")
       (list @racketid{Point3D} "Point in three-dimensional space.")
       (list @racketid{Polyhedron3D} "Polyhedron in 3D space.")
       (list @racketid{Sphere3D} "Sphere in 3D space.")
       (list @racketid{Surface3D} "3D parametric surface.")
       (list @racketid{Text3D} "3D text label.")
       (list @racketid{Ticks3D} "3D tick mark helper.")
       (list @racketid{Transformation3D} "3D transformation helper.")
       (list @racketid{Vectorfield3D} "Vector field in 3D.")
       (list @racketid{View3D} "3D view container inside a board."))]

@section{3D Geometry}

The 3D helpers build on top of a normal JSXGraph board and a 3D view
inside that board. The outer board provides the browser container, and
the 3D view supplies the actual 3D scene. A compact example is:

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box3d"))
(define view
  (jsx-create-view3d board
                     (jsx-parents (vector -4 -3)
                                  (vector 8 8)
                                  (vector (vector -5 5)
                                          (vector -5 5)
                                          (vector -5 5)))
                     (jsx-attributes [projection "parallel"])))
(define p
  (jsx-view3d-create-point3d view (vector 1 2 2)
                             (jsx-attributes [name "A"])))
(void board view p)
]

The dedicated gallery section uses the 3D helpers to show views,
primitives, intersections, curves, surfaces, solids, and other 3D
element types in separate example boards.

When you configure a 3D view, use @racket[jsx-attributes] for the
options object on the view itself. The surrounding board still uses the
usual board options if you want to change its 2D container behavior.

In practice that means:
@itemlist[
  @item{@racket[jsx-create-board] takes the outer container options such as @racket[boundingbox], @racket[axis], and @racket[grid].}
  @item{@racket[jsx-create-view3d] takes the inner scene options such as @racket[projection], @racket[trackball], and the 3D plane visibility flags.}
]

Common 3D view options include @racket[projection] for the projection
mode, @racket[trackball] for interactive rotation, and the rear-plane
visibility options that control how much of the 2D backdrop you see.
For example:

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box3d"))
(jsx-create-view3d board
                   (jsx-parents (vector -4 -3)
                                (vector 8 8)
                                (vector (vector -5 5)
                                        (vector -5 5)
                                        (vector -5 5)))
                   (jsx-attributes
                     [projection "central"]
                     [trackball (jsx-attributes [enabled #t])]))
]

Common 3D view attributes are:

@tabular[
 (list (list @bold{Attribute} @bold{Meaning})
       (list @racket[projection] "Chooses the projection mode, such as parallel or central.")
       (list @racket[trackball] "Controls interactive 3D rotation and related interaction settings.")
       (list @racket[axes-position] "Chooses where the 3D axes appear.")
       (list @racket[x-plane-rear] "Configures the rear x-plane backdrop.")
       (list @racket[y-plane-rear] "Configures the rear y-plane backdrop.")
       (list @racket[z-plane-rear] "Configures the rear z-plane backdrop."))]

@subsection{3D Helper Constructors}

These constructors cover the 3D helpers that are most naturally used
as scene-local elements inside a 3D view.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box3d"))
(define view
  (jsx-create-view3d board
                     (jsx-parents (vector -4 -3)
                                  (vector 8 8)
                                  (vector (vector -5 5)
                                          (vector -5 5)
                                          (vector -5 5)))))
(jsx-view3d-create-point3d view (vector 1 2 2))
]

@subsubsection{3D Primitives}

@itemlist[
  @item{@tt{jsx-view3d-create-point3d} creates a 3D point. Minimal example: @racket[(jsx-view3d-create-point3d view (vector 1 2 2))].}
  @item{@tt{jsx-view3d-create-line3d} creates a 3D line. Minimal example: @racket[(jsx-view3d-create-line3d view (jsx-parents a b))].}
  @item{@tt{jsx-view3d-create-circle3d} creates a 3D circle. Minimal example: @racket[(jsx-view3d-create-circle3d view (jsx-parents a (vector 0 0 0 1) 2))].}
  @item{@tt{jsx-view3d-create-plane3d} creates a 3D plane. Minimal example: @racket[(jsx-view3d-create-plane3d view (jsx-parents a b c))].}
  @item{@tt{jsx-view3d-create-axes3d} creates a 3D axes frame. Minimal example: @racket[(jsx-view3d-create-axes3d view (jsx-parents))].}
  @item{@tt{jsx-view3d-create-axis3d} creates a single 3D axis. Minimal example: @racket[(jsx-view3d-create-axis3d view (jsx-parents (vector -2 -2 -2) (vector 3 3 3)))].}
  @item{@tt{jsx-view3d-create-functiongraph3d} creates a 3D function graph. Minimal example: @racket[(jsx-view3d-create-functiongraph3d view (jsx-parents (lambda (x y) (sin (/ (* x y) 4))) (vector -5 5) (vector -5 5)))].}
  @item{@tt{jsx-view3d-create-sphere3d} creates a 3D sphere. Minimal example: @racket[(jsx-view3d-create-sphere3d view (jsx-parents a b))].}
  @item{@tt{jsx-view3d-create-curve3d} creates a 3D curve. Minimal example: @racket[(jsx-view3d-create-curve3d view (jsx-parents (lambda (t) (cos t)) (lambda (t) (sin t)) (lambda (t) (/ t 4)) (vector 0 (* 2 pi))))].}
  @item{@tt{jsx-view3d-create-text3d} creates 3D text. Minimal example: @racket[(jsx-view3d-create-text3d view (vector 0 0 0 "3D text"))].}
  @item{@tt{jsx-view3d-create-ticks3d} creates 3D tick marks. Minimal example: @racket[(jsx-view3d-create-ticks3d view (jsx-parents (vector -4 0 0) (vector 1 0 0) 6 (vector 0 0 1)))].}
  @item{@tt{jsx-view3d-create-vectorfield3d} creates a 3D vector field. Minimal example: @racket[(jsx-view3d-create-vectorfield3d view (jsx-parents (vector (lambda (x y z) (cos y)) (lambda (x y z) (sin x)) (lambda (x y z) z)) (vector -2 5 2) (vector -2 5 2) (vector -2 5 2)))].}
]

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box3d"))
(define view
  (jsx-create-view3d board
                     (jsx-parents (vector -4 -3)
                                  (vector 8 8)
                                  (vector (vector -5 5)
                                          (vector -5 5)
                                          (vector -5 5)))))
(define a (jsx-view3d-create-point3d view (vector 1 2 2)))
(define b (jsx-view3d-create-point3d view (vector -2 -1 -1)))
(define c (jsx-view3d-create-point3d view (vector 0 3 0)))

;; Point3D
(jsx-view3d-create-point3d view (vector 1 2 2))

;; Line3D
(jsx-view3d-create-line3d view (jsx-parents a b))

;; Circle3D
(jsx-view3d-create-circle3d view (jsx-parents a (vector 0 0 0 1) 2))

;; Plane3D
(jsx-view3d-create-plane3d view (jsx-parents a b c))

;; Axes3D
(jsx-view3d-create-axes3d view (jsx-parents))

;; Axis3D
(jsx-view3d-create-axis3d view (jsx-parents (vector -2 -2 -2)
                                            (vector 3 3 3)))

;; Functiongraph3D
(jsx-view3d-create-functiongraph3d
 view
 (jsx-parents (lambda (x y) (sin (/ (* x y) 4)))
              (vector -5 5)
              (vector -5 5)))

;; Sphere3D
(jsx-view3d-create-sphere3d view (jsx-parents a b))

;; Curve3D
(jsx-view3d-create-curve3d
 view
 (jsx-parents (lambda (t) (cos t))
              (lambda (t) (sin t))
              (lambda (t) (/ t 4))
              (vector 0 (* 2 pi))))

;; Text3D
(jsx-view3d-create-text3d view (vector 0 0 0 "3D text"))

;; Ticks3D
(jsx-view3d-create-ticks3d view
                           (jsx-parents (vector -4 0 0)
                                        (vector 1 0 0)
                                        6
                                        (vector 0 0 1)))

;; Vectorfield3D
(jsx-view3d-create-vectorfield3d
 view
 (jsx-parents (vector (lambda (x y z) (cos y))
                      (lambda (x y z) (sin x))
                      (lambda (x y z) z))
              (vector -2 5 2)
              (vector -2 5 2)
              (vector -2 5 2)))
]

@defproc[(jsx-view3d-create-point3d [view jsx-element?]
                                    [parents any/c]
                                    [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "point3d"
          (jsx-doc-url "View3D"))
Creates a 3D point inside a 3D view.
}

@defproc[(jsx-view3d-create-line3d [view jsx-element?]
                                   [parents any/c]
                                   [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "line3d"
          (jsx-doc-url "View3D"))
Creates a 3D line inside a 3D view.
}

@defproc[(jsx-view3d-create-circle3d [view jsx-element?]
                                     [parents any/c]
                                     [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "circle3d"
          (jsx-doc-url "View3D"))
Creates a 3D circle inside a 3D view.
}

@defproc[(jsx-view3d-create-plane3d [view jsx-element?]
                                    [parents any/c]
                                    [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "plane3d"
          (jsx-doc-url "View3D"))
Creates a 3D plane inside a 3D view.
}

@defproc[(jsx-view3d-create-axes3d [view jsx-element?]
                                   [parents any/c]
                                   [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "axes3d"
          (jsx-doc-url "View3D"))
Creates 3D axes inside a 3D view.
}

@defproc[(jsx-view3d-create-axis3d [view jsx-element?]
                                   [parents any/c]
                                   [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "axis3d"
          (jsx-doc-url "View3D"))
Creates a single 3D axis inside a 3D view.
}

@defproc[(jsx-view3d-create-functiongraph3d [view jsx-element?]
                                            [parents any/c]
                                            [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "functiongraph3d"
          (jsx-doc-url "View3D"))
Creates a 3D function graph inside a 3D view.
}

@defproc[(jsx-view3d-create-sphere3d [view jsx-element?]
                                     [parents any/c]
                                     [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "sphere3d"
          (jsx-doc-url "View3D"))
Creates a 3D sphere inside a 3D view.
}

@defproc[(jsx-view3d-create-curve3d [view jsx-element?]
                                    [parents any/c]
                                    [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "curve3d"
          (jsx-doc-url "View3D"))
Creates a 3D curve inside a 3D view.
}

@defproc[(jsx-view3d-create-text3d [view jsx-element?]
                                   [parents any/c]
                                   [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "text3d"
          (jsx-doc-url "View3D"))
Creates 3D text inside a 3D view.
}

@defproc[(jsx-view3d-create-ticks3d [view jsx-element?]
                                    [parents any/c]
                                    [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "ticks3d"
          (jsx-doc-url "View3D"))
Creates 3D tick marks inside a 3D view.
}

@defproc[(jsx-view3d-create-vectorfield3d [view jsx-element?]
                                          [parents any/c]
                                          [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "vectorfield3d"
          (jsx-doc-url "View3D"))
Creates a 3D vector field inside a 3D view.
}

@subsubsection{3D Surfaces And Solids}

@itemlist[
  @item{@tt{jsx-view3d-create-face3d} creates a polyhedron face. Minimal example: @racket[(jsx-view3d-create-face3d view (jsx-parents polyhedron-def 0))].}
  @item{@tt{jsx-view3d-create-surface3d} creates a 3D parametric surface. Minimal example: @racket[(jsx-view3d-create-surface3d view (jsx-parents (lambda (u v) (* 2 (cos u) (cos v))) (lambda (u v) (* 1.5 (sin u) (cos v))) (lambda (u v) (* 0.75 (sin v))) (vector 0 (* 2 pi)) (vector -1.5 1.5)))].}
  @item{@tt{jsx-view3d-create-parametricsurface3d} creates a 3D parametric surface. Minimal example: @racket[(jsx-view3d-create-parametricsurface3d view (jsx-parents (lambda (u v) (* 2 (sin u) (cos v))) (lambda (u v) (* 2 (sin u) (sin v))) (lambda (u v) (* 2 (cos u))) (vector 0 (* 2 pi)) (vector 0 pi)))].}
  @item{@tt{jsx-view3d-create-polyhedron3d} creates a 3D polyhedron. Minimal example: @racket[(jsx-view3d-create-polyhedron3d view (jsx-parents (vector (vector -3 -3 -3) (vector 3 -3 -3) (vector 3 3 -3) (vector -3 3 -3) (vector -3 -3 3) (vector 3 -3 3) (vector 3 3 3) (vector -3 3 3)) (vector (vector 0 1 2 3) (vector 4 5 6 7) (vector 0 1 5 4) (vector 1 2 6 5) (vector 2 3 7 6) (vector 3 0 4 7))))].}
  @item{@tt{jsx-view3d-create-transformation3d} creates a 3D transformation helper. Minimal example: @racket[(jsx-view3d-create-transformation3d view (vector 1.5 0.75 0.5) (jsx-attributes [type "translate"]))].}
]

@defproc[(jsx-view3d-create-face3d [view jsx-element?]
                                   [parents any/c]
                                   [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "face3d"
          (jsx-doc-url "View3D"))
Creates a 3D face inside a 3D view.
}

@defproc[(jsx-view3d-create-surface3d [view jsx-element?]
                                      [parents any/c]
                                      [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "surface3d"
          (jsx-doc-url "View3D"))
Creates a 3D surface inside a 3D view.
}

@defproc[(jsx-view3d-create-parametricsurface3d [view jsx-element?]
                                                [parents any/c]
                                                [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "parametricsurface3d"
          (jsx-doc-url "View3D"))
Creates a 3D parametric surface inside a 3D view.
}

@defproc[(jsx-view3d-create-polyhedron3d [view jsx-element?]
                                         [parents any/c]
                                         [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "polyhedron3d"
          (jsx-doc-url "View3D"))
Creates a 3D polyhedron inside a 3D view.
}

@defproc[(jsx-view3d-create-transformation3d [view jsx-element?]
                                             [parents any/c]
                                             [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "transform3d"
          (jsx-doc-url "View3D"))
Creates a 3D transformation inside a 3D view.
}

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box3d"))
(define view
  (jsx-create-view3d board
                     (jsx-parents (vector -4 -3)
                                  (vector 8 8)
                                  (vector (vector -5 5)
                                          (vector -5 5)
                                          (vector -5 5)))))
(define sphere-a (jsx-view3d-create-point3d view (vector -1 0 0)))
(define sphere-b (jsx-view3d-create-point3d view (vector 1 0 0)))
(define polyhedron
  (jsx-view3d-create-polyhedron3d
   view
   (jsx-parents (vector (vector -3 -3 -3)
                        (vector 3 -3 -3)
                        (vector 3 3 -3)
                        (vector -3 3 -3)
                        (vector -3 -3 3)
                        (vector 3 -3 3)
                        (vector 3 3 3)
                        (vector -3 3 3))
                (vector (vector 0 1 2 3)
                        (vector 4 5 6 7)
                        (vector 0 1 5 4)
                        (vector 1 2 6 5)
                        (vector 2 3 7 6)
                        (vector 3 0 4 7)))))
(define polyhedron-def (js-ref (jsx-element-raw polyhedron) "def"))

;; Face3D
(jsx-view3d-create-face3d view (jsx-parents polyhedron-def 0))

;; Surface3D
(jsx-view3d-create-surface3d
 view
 (jsx-parents (lambda (u v) (* 2 (cos u) (cos v)))
              (lambda (u v) (* 1.5 (sin u) (cos v)))
              (lambda (u v) (* 0.75 (sin v)))
              (vector 0 (* 2 pi))
              (vector -1.5 1.5)))

;; ParametricSurface3D
(jsx-view3d-create-parametricsurface3d
 view
 (jsx-parents (lambda (u v) (* 2 (sin u) (cos v)))
              (lambda (u v) (* 2 (sin u) (sin v)))
              (lambda (u v) (* 2 (cos u)))
              (vector 0 (* 2 pi))
              (vector 0 pi)))

;; Polyhedron3D
(jsx-view3d-create-polyhedron3d
 view
 (jsx-parents (vector (vector -3 -3 -3)
                      (vector 3 -3 -3)
                      (vector 3 3 -3)
                      (vector -3 3 -3)
                      (vector -3 -3 3)
                      (vector 3 -3 3)
                      (vector 3 3 3)
                      (vector -3 3 3))
              (vector (vector 0 1 2 3)
                      (vector 4 5 6 7)
                      (vector 0 1 5 4)
                      (vector 1 2 6 5)
                      (vector 2 3 7 6)
                      (vector 3 0 4 7))))

;; Transformation3D
(define transform (jsx-view3d-create-transformation3d
                    view
                    (vector 1.5 0.75 0.5)
                    (jsx-attributes [type "translate"])))
(jsx-view3d-create-point3d view
                           (jsx-parents sphere-a transform))
]

@subsubsection{3D Intersections}

@itemlist[
  @item{@tt{jsx-view3d-create-intersectioncircle3d} creates the circle intersection of two 3D solids. Minimal example: @racket[(jsx-view3d-create-intersectioncircle3d view (jsx-parents sphere-1 sphere-2))].}
  @item{@tt{jsx-view3d-create-intersectionline3d} creates the line intersection of two 3D planes. Minimal example: @racket[(jsx-view3d-create-intersectionline3d view (jsx-parents plane-1 plane-2))].}
]

@defproc[(jsx-view3d-create-intersectioncircle3d [view jsx-element?]
                                                 [parents any/c]
                                                 [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "intersectioncircle3d"
          (jsx-doc-url "View3D"))
Creates the circle intersection of two 3D solids inside a 3D view.
}

@defproc[(jsx-view3d-create-intersectionline3d [view jsx-element?]
                                               [parents any/c]
                                               [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "intersectionline3d"
          (jsx-doc-url "View3D"))
Creates the line intersection of two 3D planes inside a 3D view.
}

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box3d"))
(define view
  (jsx-create-view3d board
                     (jsx-parents (vector -4 -3)
                                  (vector 8 8)
                                  (vector (vector -5 5)
                                          (vector -5 5)
                                          (vector -5 5)))))
(define sphere-1 (jsx-view3d-create-sphere3d view (jsx-parents (vector -1 0 0) 2)))
(define sphere-2 (jsx-view3d-create-sphere3d view (jsx-parents (vector 1 0 0) 2)))
(define plane-1 (jsx-view3d-create-plane3d view (jsx-parents (vector 2 2 0)
                                                              (vector 1 0 0)
                                                              (vector 0 1 0))))
(define plane-2 (jsx-view3d-create-plane3d view (jsx-parents (vector 2 2 0)
                                                              (vector -2 1 1)
                                                              (vector 1 -2 1))))

;; IntersectionCircle3D
(jsx-view3d-create-intersectioncircle3d view
                                        (jsx-parents sphere-1 sphere-2))

;; IntersectionLine3D
(jsx-view3d-create-intersectionline3d view
                                      (jsx-parents plane-1 plane-2))
]

@section{2D Geometry}

This section groups the core 2D constructors by family, from the
smallest geometric objects up through curves, graphs, polygons, and
special relation objects.

@subsection{Boards, Points, And Lines}

This family covers the board container and the basic point and line
constructors.

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
                           [maybe-attributes (or/c #f external/raw) #f])
         jsx-board?]{
@(jsx-bar "JXG.JSXGraph"
          (jsx-doc-url "JXG.JSXGraph"))
Creates a JSXGraph board for the container with the given id.
Pass @racket[#f] for the default board options or a raw JSXGraph option
object, usually built with @racket[jsx-attributes].

Board options commonly include:
@itemlist[
  @item{@racket[boundingbox] for the visible board rectangle}
  @item{@racket[axis] to show or hide the coordinate axes}
  @item{@racket[grid] to show or hide the board grid}
  @item{@racket[keepaspectratio] to lock the board aspect ratio}
]

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
]
}

@defproc[(jsx-create [board jsx-board?]
                     [element-type (or/c string? symbol?)]
                     [parents vector?]
                     [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "JXG.Board"
          (jsx-doc-url "JXG.Board"))
Creates a JSXGraph element of the requested type on @racket[board].
Pass @racket[jsx-attributes] output for @racket[attributes] when you
want to set names, colors, sizes, or other element options.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define p (jsx-create-point board (jsx-parents 0 0)))
(define q (jsx-create-point board (jsx-parents 1 0)))
(define l (jsx-create board 'line (jsx-parents p q)))
]
}

@defproc[(jsx-create-point [board jsx-board?]
                           [parents vector?]
                           [attributes (or/c #f external/raw) #f])
         jsx-point?]{
@(jsx-bar "Point"
          (jsx-doc-url "Point"))
Creates a point, either free or constrained by parents.
Use @racket[jsx-attributes] for the optional point attributes.

Point attributes commonly include:
@itemlist[
  @item{@racket[name] for the displayed label}
  @item{@racket[size] for the marker size}
  @item{@racket[face] for the marker shape}
  @item{@racket[fill-color] for the marker fill color}
  @item{@racket[stroke-color] for the marker outline color}
  @item{@racket[visible] to show or hide the point}
]

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define p (jsx-create-point board (jsx-parents 0 0)))
]
}

@defproc[(jsx-create-line [board jsx-board?]
                          [parents vector?]
                          [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Line"
          (jsx-doc-url "Line"))
Creates a line from parent points or a callable parent.
Use @racket[jsx-attributes] for the optional line attributes.

Line attributes commonly include:
@itemlist[
  @item{@racket[straight-first] to extend the line beyond the first parent point}
  @item{@racket[straight-last] to extend the line beyond the second parent point}
  @item{@racket[stroke-color] for the line color}
  @item{@racket[dash] for the dash style}
]

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define p (jsx-create-point board (jsx-parents -1 0)))
(define q (jsx-create-point board (jsx-parents 1 0)))
(define l (jsx-create-line board (jsx-parents p q)))
]
}

@subsection{Curves, Circles, And Graphs}

This family covers the common continuous geometry constructors such as
segments, arcs, circles, curves, and function graphs.

These constructors are the ones you reach for when the object is a
single geometric shape or a graph-like curve rather than a helper
construction.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define p (jsx-create-point board (jsx-parents -1 0)))
(define q (jsx-create-point board (jsx-parents 1 0)))
(define line (jsx-create-line board (jsx-parents p q)))
]

@subsubsection{Segments, Arcs, Angles, And Gliders}

@subsection{Line Helpers}

The line helpers mostly return numeric values or booleans derived from a
line object.

@defproc[(jsx-line-direction [line jsx-element?])
         real?]{
@(jsx-bar "Direction"
          (string-append (jsx-doc-url "JXG.Line")
                         "#Direction"))
Returns the direction vector of @racket[line].
}

@defproc[(jsx-line-get-angle [line jsx-element?])
         real?]{
@(jsx-bar "getAngle"
          (string-append (jsx-doc-url "JXG.Line")
                         "#getAngle"))
Returns the angle of @racket[line].
}

@defproc[(jsx-line-get-rise [line jsx-element?])
         real?]{
@(jsx-bar "getRise"
          (string-append (jsx-doc-url "JXG.Line")
                         "#getRise"))
Returns the rise of @racket[line].
}

@defproc[(jsx-line-get-slope [line jsx-element?])
         real?]{
@(jsx-bar "getSlope"
          (string-append (jsx-doc-url "JXG.Line")
                         "#getSlope"))
Returns the slope of @racket[line].
}

@defproc[(jsx-line-horizontal? [line jsx-element?])
         boolean?]{
@(jsx-bar "isHorizontal"
          (string-append (jsx-doc-url "JXG.Line")
                         "#isHorizontal"))
Returns @racket[#t] when @racket[line] is horizontal.
}

@defproc[(jsx-line-vertical? [line jsx-element?])
         boolean?]{
@(jsx-bar "isVertical"
          (string-append (jsx-doc-url "JXG.Line")
                         "#isVertical"))
Returns @racket[#t] when @racket[line] is vertical.
}

@defproc[(jsx-line-l [line jsx-element?])
         real?]{
@(jsx-bar "L"
          (string-append (jsx-doc-url "JXG.Line")
                         "#L"))
Returns the @racket[L] helper for @racket[line].
}

@defproc[(jsx-line-slope [line jsx-element?])
         real?]{
@(jsx-bar "Slope"
          (string-append (jsx-doc-url "JXG.Line")
                         "#Slope"))
Returns the slope alias for @racket[line].
}

@defproc[(jsx-line-set-fixed-length! [line jsx-element?]
                                     [length real?])
         void?]{
@(jsx-bar "setFixedLength"
          (string-append (jsx-doc-url "JXG.Line")
                         "#setFixedLength"))
Sets a fixed length on @racket[line].
}

@defproc[(jsx-line-x [line jsx-element?]
                     [t real?])
         real?]{
@(jsx-bar "X"
          (string-append (jsx-doc-url "JXG.Line")
                         "#X"))
Evaluates the @racket[X] function on @racket[line].
}

@defproc[(jsx-line-y [line jsx-element?]
                     [t real?])
         real?]{
@(jsx-bar "Y"
          (string-append (jsx-doc-url "JXG.Line")
                         "#Y"))
Evaluates the @racket[Y] function on @racket[line].
}

@defproc[(jsx-line-z [line jsx-element?]
                     [t real?])
         real?]{
@(jsx-bar "Z"
          (string-append (jsx-doc-url "JXG.Line")
                         "#Z"))
Evaluates the @racket[Z] function on @racket[line].
}

@defproc[(jsx-create-segment [board jsx-board?]
                             [parents vector?]
                             [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Segment"
          (jsx-doc-url "Segment"))
Creates a finite line segment between two parent points.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define p (jsx-create-point board (jsx-parents -1 0)))
(define q (jsx-create-point board (jsx-parents 1 0)))
(define s (jsx-create-segment board (jsx-parents p q)))
]
}

@defproc[(jsx-create-arc [board jsx-board?]
                         [parents vector?]
                         [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Arc"
          (jsx-doc-url "Arc"))
Creates a circular arc through three parent points.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define arc (jsx-create-arc board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-angle [board jsx-board?]
                           [parents vector?]
                           [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Angle"
          (jsx-doc-url "Angle"))
Creates a filled angle wedge from three parent points.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define angle (jsx-create-angle board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-sector [board jsx-board?]
                            [parents vector?]
                            [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Sector"
          (jsx-doc-url "Sector"))
Creates a filled circular sector from three parent points.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define sector (jsx-create-sector board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-glider [board jsx-board?]
                            [parents vector?]
                            [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Glider"
          (jsx-doc-url "Glider"))
Creates a point constrained to a supporting line or curve.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define p (jsx-create-point board (jsx-parents -1 0)))
(define q (jsx-create-point board (jsx-parents 1 0)))
(define g (jsx-create-glider board (jsx-parents p q)))
]
}

@defproc[(jsx-create-circle [board jsx-board?]
                            [parents vector?]
                            [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Circle"
          (jsx-doc-url "Circle"))
Creates a circle from a center and radius parent.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define c (jsx-create-point board (jsx-parents 0 0)))
(define p (jsx-create-point board (jsx-parents 1 0)))
(define circle (jsx-create-circle board (jsx-parents c p)))
]
}

@defproc[(jsx-create-conic [board jsx-board?]
                           [parents vector?]
                           [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Conic"
          (jsx-doc-url "Conic"))
Creates a general conic section.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define d (jsx-create-point board (jsx-parents 0 -1)))
(define conic (jsx-create-conic board (jsx-parents a b c d)))
]
}

@defproc[(jsx-create-ellipse [board jsx-board?]
                             [parents vector?]
                             [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Ellipse"
          (jsx-doc-url "Ellipse"))
Creates an ellipse from its defining parents.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 1 0)))
(define c (jsx-create-point board (jsx-parents 0 1)))
(define ellipse (jsx-create-ellipse board (jsx-parents a b c)))
]
}

@subsubsection{Curves And Function Graphs}

@defproc[(jsx-create-curve [board jsx-board?]
                           [parents (or/c vector? procedure?)]
                           [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Curve"
          (jsx-doc-url "Curve"))
Creates a sampled or parametric curve.
Curve attributes commonly include:
@itemlist[
  @item{@racket[name] for the curve label}
  @item{@racket[stroke-color] for the curve color}
  @item{@racket[dash] for the curve dash style}
  @item{@racket[visible] to show or hide the curve}
]

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define curve
  (jsx-create-curve board
                    (lambda (t) (vector t (* t t)))))
]
}

@defproc[(jsx-create-functiongraph [board jsx-board?]
                                   [parents (or/c vector? procedure?)]
                                   [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Functiongraph"
          (jsx-doc-url "Functiongraph"))
Creates the graph of a function over x.
Function graph attributes commonly include the same keys as curves:
@itemlist[
  @item{@racket[name] for the graph label}
  @item{@racket[stroke-color] for the graph color}
  @item{@racket[dash] for the graph dash style}
  @item{@racket[visible] to show or hide the graph}
]

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define graph
  (jsx-create-functiongraph board
                            (lambda (x) (* x x))))
]
}

@subsection{Arc Helpers}

The arc helpers expose radius and sector-related measurements.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define arc (jsx-create-arc board (jsx-parents a b c)))
(jsx-arc-radius arc)
]

@defproc[(jsx-arc-get-radius [arc jsx-element?])
         real?]{
@(jsx-bar "getRadius"
          (string-append (jsx-doc-url "JXG.Arc")
                         "#getRadius"))
Returns the deprecated radius getter for @racket[arc].
}

@defproc[(jsx-arc-has-point-sector? [arc jsx-element?]
                                    [x real?]
                                    [y real?])
         boolean?]{
@(jsx-bar "hasPointSector"
          (string-append (jsx-doc-url "JXG.Arc")
                         "#hasPointSector"))
Returns @racket[#t] when the point @racket[(x, y)] lies inside the arc sector.
}

@defproc[(jsx-arc-radius [arc jsx-element?])
         real?]{
@(jsx-bar "Radius"
          (string-append (jsx-doc-url "JXG.Arc")
                         "#Radius"))
Returns the current radius of @racket[arc].
}

@defproc[(jsx-arc-value [arc jsx-element?]
                        [unit real?]
                        [rad real?])
         real?]{
@(jsx-bar "Value"
          (string-append (jsx-doc-url "JXG.Arc")
                         "#Value"))
Returns the arc length or angle value for @racket[arc].
}

@subsection{Angle Helpers}

The angle helpers report or adjust the current angle value.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define ang (jsx-create-angle board (jsx-parents a b c)))
(jsx-angle-value ang 1)
]

@defproc[(jsx-angle-free? [angle jsx-element?])
         boolean?]{
@(jsx-bar "free"
          (string-append (jsx-doc-url "JXG.Angle")
                         "#free"))
Returns @racket[#t] when the angle is free.
}

@defproc[(jsx-angle-set-angle! [angle jsx-element?]
                               [val real?])
         void?]{
@(jsx-bar "setAngle"
          (string-append (jsx-doc-url "JXG.Angle")
                         "#setAngle"))
Sets the angle value on @racket[angle].
}

@defproc[(jsx-angle-value [angle jsx-element?]
                          [unit real?])
         real?]{
@(jsx-bar "Value"
          (string-append (jsx-doc-url "JXG.Angle")
                         "#Value"))
Returns the current angle value for @racket[angle].
}

@subsection{Sector Helpers}

The sector helpers expose area, perimeter, radius, and containment
queries.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define sec (jsx-create-sector board (jsx-parents a b c)))
(jsx-sector-area sec)
]

@defproc[(jsx-sector-area [sector jsx-element?])
         real?]{
@(jsx-bar "Area"
          (string-append (jsx-doc-url "JXG.Sector")
                         "#Area"))
Returns the area of @racket[sector].
}

@defproc[(jsx-sector-has-point-sector? [sector jsx-element?]
                                       [x real?]
                                       [y real?])
         boolean?]{
@(jsx-bar "hasPointSector"
          (string-append (jsx-doc-url "JXG.Sector")
                         "#hasPointSector"))
Returns @racket[#t] when the point lies inside @racket[sector].
}

@defproc[(jsx-sector-l [sector jsx-element?])
         real?]{
@(jsx-bar "L"
          (string-append (jsx-doc-url "JXG.Sector")
                         "#L"))
Returns the arc length of @racket[sector].
}

@defproc[(jsx-sector-perimeter [sector jsx-element?])
         real?]{
@(jsx-bar "Perimeter"
          (string-append (jsx-doc-url "JXG.Sector")
                         "#Perimeter"))
Returns the perimeter of @racket[sector].
}

@defproc[(jsx-sector-radius [sector jsx-element?])
         real?]{
@(jsx-bar "Radius"
          (string-append (jsx-doc-url "JXG.Sector")
                         "#Radius"))
Returns the radius of @racket[sector].
}

@defproc[(jsx-sector-set-position-directly! [sector jsx-element?]
                                            [method (or/c string? symbol?)]
                                            [coords (or/c vector? list?)]
                                            [oldcoords (or/c vector? list?)])
         jsx-element?]{
@(jsx-bar "setPositionDirectly"
          (string-append (jsx-doc-url "JXG.Sector")
                         "#setPositionDirectly"))
Moves @racket[sector] by direct coordinates.
}

@defproc[(jsx-sector-set-radius! [sector jsx-element?]
                                 [value real?])
         jsx-element?]{
@(jsx-bar "setRadius"
          (string-append (jsx-doc-url "JXG.Sector")
                         "#setRadius"))
Sets the radius of @racket[sector].
}

@subsection{Glider Helpers}

The glider helpers are animation controls for gliders.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define p (jsx-create-point board (jsx-parents -1 0)))
(define q (jsx-create-point board (jsx-parents 1 0)))
(define g (jsx-create-glider board (jsx-parents p q)))
(jsx-glider-stop-animation! g)
]

@defproc[(jsx-glider-start-animation! [glider jsx-element?]
                                      [direction real?]
                                      [step-count exact-nonnegative-integer?]
                                      [delay exact-nonnegative-integer?])
         void?]{
@(jsx-bar "startAnimation"
          (string-append (jsx-doc-url "JXG.Glider")
                         "#startAnimation"))
Starts the glider animation loop.
}

@defproc[(jsx-glider-stop-animation! [glider jsx-element?])
         void?]{
@(jsx-bar "stopAnimation"
          (string-append (jsx-doc-url "JXG.Glider")
                         "#stopAnimation"))
Stops the glider animation loop.
}

@subsection{Circle Helpers}

The circle helpers expose circle measurements and form updates.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define c (jsx-create-point board (jsx-parents 0 0)))
(define p (jsx-create-point board (jsx-parents 1 0)))
(define circle (jsx-create-circle board (jsx-parents c p)))
(jsx-circle-area circle)
]

@defproc[(jsx-circle-area [circle jsx-element?])
         real?]{
@(jsx-bar "Area"
          (string-append (jsx-doc-url "JXG.Circle")
                         "#Area"))
Returns the area of @racket[circle].
}

@defproc[(jsx-circle-bounds [circle jsx-element?])
         vector?]{
@(jsx-bar "bounds"
          (string-append (jsx-doc-url "JXG.Circle")
                         "#bounds"))
Returns the bounding box of @racket[circle].
}

@defproc[(jsx-circle-diameter [circle jsx-element?])
         real?]{
@(jsx-bar "Diameter"
          (string-append (jsx-doc-url "JXG.Circle")
                         "#Diameter"))
Returns the diameter of @racket[circle].
}

@defproc[(jsx-circle-get-radius [circle jsx-element?])
         real?]{
@(jsx-bar "getRadius"
          (string-append (jsx-doc-url "JXG.Circle")
                         "#getRadius"))
Returns the radius helper value of @racket[circle].
}

@defproc[(jsx-circle-perimeter [circle jsx-element?])
         real?]{
@(jsx-bar "Perimeter"
          (string-append (jsx-doc-url "JXG.Circle")
                         "#Perimeter"))
Returns the perimeter of @racket[circle].
}

@defproc[(jsx-circle-radius! [circle jsx-element?]
                             [radius real?])
         void?]{
@(jsx-bar "Radius"
          (string-append (jsx-doc-url "JXG.Circle")
                         "#Radius"))
Sets the radius of @racket[circle].
}

@defproc[(jsx-circle-set-radius! [circle jsx-element?]
                                 [radius real?])
         void?]{
@(jsx-bar "setRadius"
          (string-append (jsx-doc-url "JXG.Circle")
                         "#setRadius"))
Sets the radius of @racket[circle].
}

@defproc[(jsx-circle-update-quadraticform! [circle jsx-element?])
         void?]{
@(jsx-bar "updateQuadraticform"
          (string-append (jsx-doc-url "JXG.Circle")
                         "#updateQuadraticform"))
Updates the circle quadratic form.
}

@defproc[(jsx-circle-update-renderer! [circle jsx-element?])
         void?]{
@(jsx-bar "updateRenderer"
          (string-append (jsx-doc-url "JXG.Circle")
                         "#updateRenderer"))
Refreshes the circle renderer.
}

@defproc[(jsx-circle-update-stdform! [circle jsx-element?])
         void?]{
@(jsx-bar "updateStdform"
          (string-append (jsx-doc-url "JXG.Circle")
                         "#updateStdform"))
Updates the circle standard form.
}

@defproc[(jsx-circle-x [circle jsx-element?]
                       [t real?])
         real?]{
@(jsx-bar "X"
          (string-append (jsx-doc-url "JXG.Circle")
                         "#X"))
Evaluates the @racket[X] function on @racket[circle].
}

@defproc[(jsx-circle-y [circle jsx-element?]
                       [t real?])
         real?]{
@(jsx-bar "Y"
          (string-append (jsx-doc-url "JXG.Circle")
                         "#Y"))
Evaluates the @racket[Y] function on @racket[circle].
}

@defproc[(jsx-circle-z [circle jsx-element?]
                       [t real?])
         real?]{
@(jsx-bar "Z"
          (string-append (jsx-doc-url "JXG.Circle")
                         "#Z"))
Evaluates the @racket[Z] function on @racket[circle].
}

@subsection{Curve Helpers}

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define curve (jsx-create-curve board (lambda (t) (vector t (* t t)))))
(jsx-curve-has-point? curve 1 1)
]

@defproc[(jsx-curve-allocate-points! [curve jsx-element?])
         void?]{
@(jsx-bar "allocatePoints"
          (string-append (jsx-doc-url "JXG.Curve")
                         "#allocatePoints"))
Allocates the point cache for @racket[curve].
}

@defproc[(jsx-curve-generate-term [curve jsx-element?])
         external/raw]{
@(jsx-bar "generateTerm"
          (string-append (jsx-doc-url "JXG.Curve")
                         "#generateTerm"))
Generates the curve term helper.
}

@defproc[(jsx-curve-get-label-position [curve jsx-element?])
         external/raw]{
@(jsx-bar "getLabelPosition"
          (string-append (jsx-doc-url "JXG.Curve")
                         "#getLabelPosition"))
Returns the label position helper for @racket[curve].
}

@defproc[(jsx-curve-get-transformation-source [curve jsx-element?])
         external/raw]{
@(jsx-bar "getTransformationSource"
          (string-append (jsx-doc-url "JXG.Curve")
                         "#getTransformationSource"))
Returns the transformation source of @racket[curve].
}

@defproc[(jsx-curve-has-point? [curve jsx-element?]
                               [x real?]
                               [y real?])
         boolean?]{
@(jsx-bar "hasPoint"
          (string-append (jsx-doc-url "JXG.Curve")
                         "#hasPoint"))
Returns @racket[#t] when screen coordinates hit @racket[curve].
}

@defproc[(jsx-curve-interpolation-function-from-array [curve jsx-element?]
                                                      [data vector?])
         external/raw]{
@(jsx-bar "interpolationFunctionFromArray"
          (string-append (jsx-doc-url "JXG.Curve")
                         "#interpolationFunctionFromArray"))
Builds an interpolation function from sample data.
}

@defproc[(jsx-curve-max-x [curve jsx-element?])
         real?]{
@(jsx-bar "maxX"
          (string-append (jsx-doc-url "JXG.Curve")
                         "#maxX"))
Returns the maximum x-value of @racket[curve].
}

@defproc[(jsx-curve-min-x [curve jsx-element?])
         real?]{
@(jsx-bar "minX"
          (string-append (jsx-doc-url "JXG.Curve")
                         "#minX"))
Returns the minimum x-value of @racket[curve].
}

@defproc[(jsx-curve-move-to! [curve jsx-element?]
                             [where (or/c vector? list?)])
         jsx-element?]{
@(jsx-bar "moveTo"
          (string-append (jsx-doc-url "JXG.Curve")
                         "#moveTo"))
Moves @racket[curve] to a new location.
}

@defproc[(jsx-curve-notify-parents! [curve jsx-element?])
         void?]{
@(jsx-bar "notifyParents"
          (string-append (jsx-doc-url "JXG.Curve")
                         "#notifyParents"))
Notifies parents of a curve change.
}

@defproc[(jsx-curve-update-curve! [curve jsx-element?])
         void?]{
@(jsx-bar "updateCurve"
          (string-append (jsx-doc-url "JXG.Curve")
                         "#updateCurve"))
Updates the curve data.
}

@defproc[(jsx-curve-update-data-array! [curve jsx-element?])
         void?]{
@(jsx-bar "updateDataArray"
          (string-append (jsx-doc-url "JXG.Curve")
                         "#updateDataArray"))
Updates the curve data array.
}

@defproc[(jsx-curve-update-renderer! [curve jsx-element?])
         void?]{
@(jsx-bar "updateRenderer"
          (string-append (jsx-doc-url "JXG.Curve")
                         "#updateRenderer"))
Refreshes the curve renderer.
}

@defproc[(jsx-curve-update-transform! [curve jsx-element?])
         void?]{
@(jsx-bar "updateTransform"
          (string-append (jsx-doc-url "JXG.Curve")
                         "#updateTransform"))
Updates a curve transformation.
}

@subsection{Polygons And Related Constructions}

This family covers polygon-based objects and the constructions that are
most naturally exercised alongside them.

@defproc[(jsx-create-polygon [board jsx-board?]
                             [parents vector?]
                             [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Polygon"
          (jsx-doc-url "JXG.Polygon"))
Creates a polygon from its vertices.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define poly (jsx-create-polygon board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-midpoint [board jsx-board?]
                              [parents vector?]
                              [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Midpoint"
          (jsx-doc-url "Midpoint"))
Creates the midpoint of two parent points.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 1 0)))
(define mid (jsx-create-midpoint board (jsx-parents a b)))
]
}

@defproc[(jsx-create-parallel [board jsx-board?]
                              [parents vector?]
                              [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Parallel"
          (jsx-doc-url "Parallel"))
Creates a line through a point that is parallel to a base line.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 1 0)))
(define c (jsx-create-point board (jsx-parents 0 1)))
(define par (jsx-create-parallel board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-arrowparallel [board jsx-board?]
                                   [parents vector?]
                                   [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Arrowparallel"
          (jsx-doc-url "Arrowparallel"))
Creates a parallel line with arrow styling.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 1 0)))
(define c (jsx-create-point board (jsx-parents 0 1)))
(define par (jsx-create-arrowparallel board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-axis [board jsx-board?]
                          [parents vector?]
                          [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Axis"
          (jsx-doc-url "Axis"))
Creates an axis decoration for the board.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define axis (jsx-create-axis board (jsx-parents)))
]
}

@defproc[(jsx-create-grid [board jsx-board?]
                          [parents vector?]
                          [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Grid"
          (jsx-doc-url "Grid"))
Creates a grid decoration for the board.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define grid (jsx-create-grid board (jsx-parents)))
]
}

@defproc[(jsx-create-boxplot [board jsx-board?]
                             [parents vector?]
                             [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Boxplot"
          (jsx-doc-url "Boxplot"))
Creates a box-and-whisker plot object from data parents.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define plot (jsx-create-boxplot board (jsx-parents (vector 1 2 3 4 5))))
]
}

@defproc[(jsx-create-tangent [board jsx-board?]
                             [parents vector?]
                             [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Tangent"
          (jsx-doc-url "Tangent"))
Creates a tangent line to a curve or circle.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define curve (jsx-create-curve board (lambda (t) (vector t (* t t)))))
(define p (jsx-create-point board (jsx-parents 1 1)))
(define tan (jsx-create-tangent board (jsx-parents p curve)))
]
}

@defproc[(jsx-create-tangentto [board jsx-board?]
                               [parents vector?]
                               [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "TangentTo"
          (jsx-doc-url "TangentTo"))
Creates the tangent through a point to a curve or circle.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define curve (jsx-create-curve board (lambda (t) (vector t (* t t)))))
(define p (jsx-create-point board (jsx-parents 1 1)))
(define tan (jsx-create-tangentto board (jsx-parents p curve)))
]
}

@defproc[(jsx-create-polarline [board jsx-board?]
                               [parents vector?]
                               [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "PolarLine"
          (jsx-doc-url "PolarLine"))
Creates the polar line of a point with respect to a circle.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define c (jsx-create-point board (jsx-parents 0 0)))
(define p (jsx-create-point board (jsx-parents 1 1)))
(define circle (jsx-create-circle board (jsx-parents c p)))
(define polar (jsx-create-polarline board (jsx-parents p circle)))
]
}

@defproc[(jsx-create-polepoint [board jsx-board?]
                               [parents vector?]
                               [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "PolePoint"
          (jsx-doc-url "PolePoint"))
Creates the pole point of a line with respect to a circle.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define c (jsx-create-point board (jsx-parents 0 0)))
(define p (jsx-create-point board (jsx-parents 1 0)))
(define circle (jsx-create-circle board (jsx-parents c p)))
(define line (jsx-create-line board (jsx-parents c p)))
(define pole (jsx-create-polepoint board (jsx-parents line circle)))
]
}

@defproc[(jsx-create-radicalaxis [board jsx-board?]
                                 [parents vector?]
                                 [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "RadicalAxis"
          (jsx-doc-url "RadicalAxis"))
Creates the radical axis of two circles.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define c1 (jsx-create-point board (jsx-parents -1 0)))
(define p1 (jsx-create-point board (jsx-parents 0 0)))
(define c2 (jsx-create-point board (jsx-parents 1 0)))
(define p2 (jsx-create-point board (jsx-parents 1 1)))
(define a (jsx-create-circle board (jsx-parents c1 p1)))
(define b (jsx-create-circle board (jsx-parents c2 p2)))
(define axis (jsx-create-radicalaxis board (jsx-parents a b)))
]
}

@defproc[(jsx-create-circumcircle [board jsx-board?]
                                  [parents vector?]
                                  [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Circumcircle"
          (jsx-doc-url "Circumcircle"))
Creates the circle through the vertices of a triangle.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define circ (jsx-create-circumcircle board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-incircle [board jsx-board?]
                              [parents vector?]
                              [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Incircle"
          (jsx-doc-url "Incircle"))
Creates the inscribed circle of a triangle.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define inc (jsx-create-incircle board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-incenter [board jsx-board?]
                              [parents vector?]
                              [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Incenter"
          (jsx-doc-url "Incenter"))
Creates the center of the inscribed circle of a triangle.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define ic (jsx-create-incenter board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-minorarc [board jsx-board?]
                              [parents vector?]
                              [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "MinorArc"
          (jsx-doc-url "MinorArc"))
Creates the shorter arc between two points on a circle.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define arc (jsx-create-minorarc board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-minorsector [board jsx-board?]
                                 [parents vector?]
                                 [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "MinorSector"
          (jsx-doc-url "MinorSector"))
Creates the smaller sector bounded by two circle radii.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define sec (jsx-create-minorsector board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-circumcirclearc [board jsx-board?]
                                     [parents vector?]
                                     [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "CircumcircleArc"
          (jsx-doc-url "CircumcircleArc"))
Creates an arc on the circumcircle of a triangle.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define arc (jsx-create-circumcirclearc board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-circumcirclesector [board jsx-board?]
                                        [parents vector?]
                                        [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "CircumcircleSector"
          (jsx-doc-url "CircumcircleSector"))
Creates a sector on the circumcircle of a triangle.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define sec (jsx-create-circumcirclesector board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-semicircle [board jsx-board?]
                                [parents vector?]
                                [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Semicircle"
          (jsx-doc-url "Semicircle"))
Creates a half-circle through two endpoints.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 1 0)))
(define semi (jsx-create-semicircle board (jsx-parents a b)))
]
}

@defproc[(jsx-create-majorarc [board jsx-board?]
                              [parents vector?]
                              [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "MajorArc"
          (jsx-doc-url "MajorArc"))
Creates the longer arc between two circle points.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define arc (jsx-create-majorarc board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-majorsector [board jsx-board?]
                                 [parents vector?]
                                 [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "MajorSector"
          (jsx-doc-url "MajorSector"))
Creates the larger sector bounded by two circle radii.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define sec (jsx-create-majorsector board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-curveintersection [board jsx-board?]
                                       [parents vector?]
                                       [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "CurveIntersection"
          (jsx-doc-url "CurveIntersection"))
Creates the intersection of two curves.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-curve board (lambda (t) (vector t (* t t)))))
(define b (jsx-create-curve board (lambda (t) (vector t (- (* t t) 1)))))
(define pt (jsx-create-curveintersection board (jsx-parents a b)))
]
}

@defproc[(jsx-create-curvedifference [board jsx-board?]
                                     [parents vector?]
                                     [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "CurveDifference"
          (jsx-doc-url "CurveDifference"))
Creates the difference of two curve regions.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-curve board (lambda (t) (vector t (* t t)))))
(define b (jsx-create-curve board (lambda (t) (vector t (- (* t t) 1)))))
(define d (jsx-create-curvedifference board (jsx-parents a b)))
]
}

@defproc[(jsx-create-curveunion [board jsx-board?]
                                [parents vector?]
                                [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "CurveUnion"
          (jsx-doc-url "CurveUnion"))
Creates the union of two curve regions.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-curve board (lambda (t) (vector t (* t t)))))
(define b (jsx-create-curve board (lambda (t) (vector t (- (* t t) 1)))))
(define u (jsx-create-curveunion board (jsx-parents a b)))
]
}

@subsubsection{Curve Compositions}

@defproc[(jsx-create-derivative [board jsx-board?]
                                [parents vector?]
                                [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Derivative"
          (jsx-doc-url "Derivative"))
Creates the derivative graph of a function or curve.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define d (jsx-create-derivative board (jsx-parents (lambda (x) (* x x)))))
]
}

@defproc[(jsx-create-integral [board jsx-board?]
                              [parents vector?]
                              [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Integral"
          (jsx-doc-url "Integral"))
Creates an integral or area visualization.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define i (jsx-create-integral board (jsx-parents (lambda (x) (* x x)))))
]
}

@defproc[(jsx-create-riemannsum [board jsx-board?]
                                [parents vector?]
                                [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Riemannsum"
          (jsx-doc-url "Riemannsum"))
Creates a Riemann sum visualization for a function.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define r (jsx-create-riemannsum board (jsx-parents (lambda (x) (* x x)))))
]
}

@defproc[(jsx-riemannsum-value [riemannsum jsx-element?])
         real?]{
@(jsx-bar "Value"
          (string-append (jsx-doc-url "Riemannsum")
                         "#Value"))
Reads the current value of a Riemann sum visualization.
}

@defproc[(jsx-create-slopefield [board jsx-board?]
                                [parents vector?]
                                [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Slopefield"
          (jsx-doc-url "Slopefield"))
Creates a slope field for a differential equation.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define s (jsx-create-slopefield board (jsx-parents (lambda (x y) (+ x y)))))
]
}

@defproc[(jsx-slopefield-set-f! [field jsx-element?]
                                [func procedure?])
         void?]{
@(jsx-bar "setF"
          (string-append (jsx-doc-url "Slopefield")
                         "#setF"))
Updates the defining function of a slope field.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define s (jsx-create-slopefield board (jsx-parents (lambda (x y) (+ x y)))))
(jsx-slopefield-set-f! s (lambda (x y) (- x y)))
]
}

@defproc[(jsx-create-vectorfield [board jsx-board?]
                                 [parents vector?]
                                 [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Vectorfield"
          (jsx-doc-url "Vectorfield"))
Creates a vector field visualization.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define v (jsx-create-vectorfield board (jsx-parents (lambda (x y) (vector x y)))))
]
}

@defproc[(jsx-vectorfield-set-f! [field jsx-element?]
                                 [func procedure?])
         void?]{
@(jsx-bar "setF"
          (string-append (jsx-doc-url "Vectorfield")
                         "#setF"))
Updates the defining function of a vector field.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define v (jsx-create-vectorfield board (jsx-parents (lambda (x y) (vector x y)))))
(jsx-vectorfield-set-f! v (lambda (x y) (vector (- x) (- y))))
]
}

@defproc[(jsx-create-implicitcurve [board jsx-board?]
                                   [parents vector?]
                                   [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "ImplicitCurve"
          (jsx-doc-url "ImplicitCurve"))
Creates a curve defined by an implicit relation.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define c (jsx-create-implicitcurve board (jsx-parents (lambda (x y) (- (+ (* x x) (* y y)) 1)))))
]
}

@defproc[(jsx-create-spline [board jsx-board?]
                            [parents vector?]
                            [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Spline"
          (jsx-doc-url "Spline"))
Creates a spline curve through control points.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define s (jsx-create-spline board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-cardinalspline [board jsx-board?]
                                    [parents vector?]
                                    [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Cardinalspline"
          (jsx-doc-url "Cardinalspline"))
Creates a cardinal spline with adjustable tension.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define s (jsx-create-cardinalspline board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-comb [board jsx-board?]
                          [parents vector?]
                          [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Comb"
          (jsx-doc-url "Comb"))
Creates a comb-like helper curve.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define s (jsx-create-comb board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-metapostspline [board jsx-board?]
                                    [parents vector?]
                                    [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Metapostspline"
          (jsx-doc-url "Metapostspline"))
Creates a MetaPost-style spline curve.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define s (jsx-create-metapostspline board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-polygonalchain [board jsx-board?]
                                    [parents vector?]
                                    [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "PolygonalChain"
          (jsx-doc-url "PolygonalChain"))
Creates a chain of connected line segments.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define chain (jsx-create-polygonalchain board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-regularpolygon [board jsx-board?]
                                    [parents vector?]
                                    [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "RegularPolygon"
          (jsx-doc-url "RegularPolygon"))
Creates a regular polygon from a center and radius-like parents.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define c (jsx-create-point board (jsx-parents 0 0)))
(define p (jsx-create-point board (jsx-parents 1 0)))
(define poly (jsx-create-regularpolygon board (jsx-parents c p 5)))
]
}

@defproc[(jsx-create-hyperbola [board jsx-board?]
                               [parents vector?]
                               [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Hyperbola"
          (jsx-doc-url "Hyperbola"))
Creates a hyperbola graph.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define h (jsx-create-hyperbola board (jsx-parents 1 1 2 2)))
]
}

@defproc[(jsx-create-parabola [board jsx-board?]
                              [parents vector?]
                              [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Parabola"
          (jsx-doc-url "Parabola"))
Creates a parabola graph.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define p (jsx-create-parabola board (jsx-parents 0 0 1 1)))
]
}

@defproc[(jsx-create-stepfunction [board jsx-board?]
                                  [parents vector?]
                                  [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Stepfunction"
          (jsx-doc-url "Stepfunction"))
Creates a step-function graph.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define s (jsx-create-stepfunction board (jsx-parents (vector 0 1 2) (vector 1 2 3))))
]
}

@defproc[(jsx-create-inequality [board jsx-board?]
                                [parents vector?]
                                [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Inequality"
          (jsx-doc-url "Inequality"))
Creates a shaded inequality region or boundary.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define ineq (jsx-create-inequality board (jsx-parents (lambda (x) (- x 1)))))
]
}

@defproc[(jsx-create-turtle [board jsx-board?]
                            [parents vector?]
                            [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Turtle"
          (jsx-doc-url "Turtle"))
Creates a turtle-graphics drawing helper.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define t (jsx-create-turtle board (jsx-parents)))
]
}

@defproc[(jsx-create-slopetriangle [board jsx-board?]
                                   [parents vector?]
                                   [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Slopetriangle"
          (jsx-doc-url "Slopetriangle"))
Creates the slope triangle used for rise-over-run visuals.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define s (jsx-create-slopetriangle board (jsx-parents (vector 0 0) (vector 1 1))))
]
}

@defproc[(jsx-create-perpendicular [board jsx-board?]
                                   [parents vector?]
                                   [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Perpendicular"
          (jsx-doc-url "Perpendicular"))
Creates a line perpendicular to a base line through a point.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 1 0)))
(define c (jsx-create-point board (jsx-parents 0 1)))
(define p (jsx-create-perpendicular board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-reflection [board jsx-board?]
                                [parents vector?]
                                [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Reflection"
          (jsx-doc-url "Reflection"))
Creates the reflection of a point or element across a line.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 1 0)))
(define c (jsx-create-point board (jsx-parents 0 1)))
(define r (jsx-create-reflection board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-bisector [board jsx-board?]
                              [parents vector?]
                              [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Bisector"
          (jsx-doc-url "Bisector"))
Creates an angle or perpendicular bisector construction.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define bis (jsx-create-bisector board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-normal [board jsx-board?]
                            [parents vector?]
                            [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Normal"
          (jsx-doc-url "Normal"))
Creates the normal line through a point on a curve or line.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define curve (jsx-create-curve board (lambda (t) (vector t (* t t)))))
(define p (jsx-create-point board (jsx-parents 1 1)))
(define n (jsx-create-normal board (jsx-parents p curve)))
]
}

@subsection{Polygon Helpers}

The @racket[jsx-polygon] wrappers expose the documented
@racketid[JXG.Polygon] methods that are useful for editing polygon
vertices and querying polygon geometry.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents 0 0)))
(define b (jsx-create-point board (jsx-parents 2 0)))
(define c (jsx-create-point board (jsx-parents 1 1)))
(define poly (jsx-create-polygon board (jsx-parents a b c)))
(jsx-polygon-area poly)
]

@defproc[(jsx-polygon-add-points! [polygon jsx-element?]
                                  [point jsx-element?] ...)
         jsx-element?]{
@(jsx-bar "addPoints"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#addPoints"))
Adds vertices to @racket[polygon].
}

@defproc[(jsx-polygon-area [polygon jsx-element?])
         real?]{
@(jsx-bar "Area"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#Area"))
Returns the area of @racket[polygon].
}

@defproc[(jsx-polygon-bounding-box [polygon jsx-element?])
         vector?]{
@(jsx-bar "boundingBox"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#boundingBox"))
Returns the bounding box of @racket[polygon].
}

@defproc[(jsx-polygon-find-point [polygon jsx-element?]
                                 [point jsx-element?])
         exact-nonnegative-integer?]{
@(jsx-bar "findPoint"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#findPoint"))
Returns the index for @racket[point] in @racket[polygon].
}

@defproc[(jsx-polygon-has-point? [polygon jsx-element?]
                                 [x real?]
                                 [y real?])
         boolean?]{
@(jsx-bar "hasPoint"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#hasPoint"))
Returns @racket[#t] when screen coordinates hit @racket[polygon].
}

@defproc[(jsx-polygon-hide-element! [polygon jsx-element?]
                                    [borderless (or/c #f boolean?) #f])
         void?]{
@(jsx-bar "hideElement"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#hideElement"))
Hides @racket[polygon], optionally leaving the borders visible.
}

@defproc[(jsx-polygon-insert-points! [polygon jsx-element?]
                                     [point jsx-element?] ...)
         jsx-element?]{
@(jsx-bar "insertPoints"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#insertPoints"))
Inserts vertices into @racket[polygon].
}

@defproc[(jsx-polygon-intersect [polygon jsx-element?]
                                [other jsx-element?])
         external/raw]{
@(jsx-bar "intersect"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#intersect"))
Intersects @racket[polygon] with @racket[other].
}

@defproc[(jsx-polygon-l [polygon jsx-element?])
         real?]{
@(jsx-bar "L"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#L"))
Returns the perimeter alias for @racket[polygon].
}

@defproc[(jsx-polygon-perimeter [polygon jsx-element?])
         real?]{
@(jsx-bar "Perimeter"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#Perimeter"))
Returns the perimeter of @racket[polygon].
}

@defproc[(jsx-polygon-pnpoly [polygon jsx-element?]
                             [x real?]
                             [y real?]
                             [coord-type (or/c #f string? symbol?) #f])
         boolean?]{
@(jsx-bar "pnpoly"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#pnpoly"))
Checks whether @racket[x] and @racket[y] fall inside @racket[polygon].
}

@defproc[(jsx-polygon-remove-points! [polygon jsx-element?]
                                     [point jsx-element?] ...)
         jsx-element?]{
@(jsx-bar "removePoints"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#removePoints"))
Removes vertices from @racket[polygon].
}

@defproc[(jsx-polygon-set-position-directly! [polygon jsx-element?]
                                             [method (or/c string? symbol?)]
                                             [coords (or/c vector? list?)]
                                             [oldcoords (or/c vector? list?)])
         jsx-element?]{
@(jsx-bar "setPositionDirectly"
          (string-append (jsx-doc-url "JXG.Polygon")
                         "#setPositionDirectly"))
Moves @racket[polygon] directly by coordinate differences.
}

@defproc[(jsx-polygon-show-element! [polygon jsx-element?]
                                    [borderless (or/c #f boolean?) #f])
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
                                  [parents vector?]
                                  [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Intersection"
          (jsx-doc-url "Intersection"))
Creates the intersection point of two objects.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 1 0)))
(define c (jsx-create-point board (jsx-parents 0 1)))
(define i (jsx-create-intersection board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-arrow [board jsx-board?]
                           [parents vector?]
                           [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Arrow"
          (jsx-doc-url "Arrow"))
Creates an arrow annotation or vector.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define arrow (jsx-create-arrow board (jsx-parents (vector 0 0) (vector 1 1))))
]
}

@section{Widgets and Annotation}

This section covers the widget-like and annotation-style objects that
sit alongside the core geometry constructors.

The widget and annotation constructors cover the elements that present
controls, labels, and board-attached UI.

@subsection{Form Controls}

@defproc[(jsx-create-button [board jsx-board?]
                            [parents vector?]
                            [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Button"
          (jsx-doc-url "Button"))
Creates a clickable button widget.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define btn (jsx-create-button board (jsx-parents 0 0 "Click")))
]
}

@defproc[(jsx-create-checkbox [board jsx-board?]
                              [parents vector?]
                              [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Checkbox"
          (jsx-doc-url "Checkbox"))
Creates a boolean toggle widget.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define cb (jsx-create-checkbox board (jsx-parents 0 0 "Show")))
]
}

@subsection{Checkbox Helpers}

@defproc[(jsx-checkbox-value [checkbox jsx-element?])
         boolean?]{
@(jsx-bar "Value"
          (string-append (jsx-doc-url "JXG.Checkbox")
                         "#Value"))
Returns the current checkbox value.
}

@defproc[(jsx-create-input [board jsx-board?]
                           [parents vector?]
                           [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Input"
          (jsx-doc-url "Input"))
Creates a text input widget.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define input (jsx-create-input board (jsx-parents 0 0)))
]
}

@subsection{Input Helpers}

@defproc[(jsx-input-set! [input jsx-element?]
                         [value string?])
         jsx-element?]{
@(jsx-bar "set"
          (string-append (jsx-doc-url "JXG.Input")
                         "#set"))
Sets the current value of @racket[input].
}

@defproc[(jsx-input-value [input jsx-element?])
         string?]{
@(jsx-bar "Value"
          (string-append (jsx-doc-url "JXG.Input")
                         "#Value"))
Returns the current input value.
}

@defproc[(jsx-create-slider [board jsx-board?]
                            [parents vector?]
                            [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Slider"
          (jsx-doc-url "Slider"))
Creates a numeric slider widget.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define slider (jsx-create-slider board (jsx-parents 0 1 0 10)))
]
}

@subsection{Slider Helpers}

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define slider (jsx-create-slider board (jsx-parents 0 1 0 10)))
(jsx-slider-value slider)
]

@defproc[(jsx-slider-set-max! [slider jsx-element?]
                              [value real?])
         jsx-element?]{
@(jsx-bar "setMax"
          (string-append (jsx-doc-url "JXG.Slider")
                         "#setMax"))
Sets the maximum slider value.
}

@defproc[(jsx-slider-set-min! [slider jsx-element?]
                              [value real?])
         jsx-element?]{
@(jsx-bar "setMin"
          (string-append (jsx-doc-url "JXG.Slider")
                         "#setMin"))
Sets the minimum slider value.
}

@defproc[(jsx-slider-set-value! [slider jsx-element?]
                                [value real?])
         jsx-element?]{
@(jsx-bar "setValue"
          (string-append (jsx-doc-url "JXG.Slider")
                         "#setValue"))
Sets the current slider value.
}

@defproc[(jsx-slider-value [slider jsx-element?])
         number?]{
@(jsx-bar "Value"
          (string-append (jsx-doc-url "JXG.Slider")
                         "#Value"))
Returns the current slider value.
}

@subsection{Display Objects}

@defproc[(jsx-create-chart [board jsx-board?]
                           [parents vector?]
                           [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Chart"
          (jsx-doc-url "Chart"))
Creates a chart container for plotted data.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define chart (jsx-create-chart board (jsx-parents (vector 1 2 3) (vector 2 4 6))))
]
}

@subsection{Chart Helpers}

The @racket[jsx-chart] wrappers expose the documented
@racketid[JXG.Chart] draw and update methods. These methods mirror the
JSXGraph API closely and return the raw browser result.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define chart (jsx-create-chart board (jsx-parents (vector 1 2 3)
                                                   (vector 2 4 6))))
(jsx-chart-draw-line chart)
]

@defproc[(jsx-chart-draw-bar [chart jsx-element?]
                             [args external/raw] ...)
         external/raw]{
@(jsx-bar "drawBar"
          (string-append (jsx-doc-url "Chart")
                         "#drawBar"))
Draws a bar chart rendering.
}

@defproc[(jsx-chart-draw-fit [chart jsx-element?]
                             [args external/raw] ...)
         external/raw]{
@(jsx-bar "drawFit"
          (string-append (jsx-doc-url "Chart")
                         "#drawFit"))
Draws a fit chart rendering.
}

@defproc[(jsx-chart-draw-line [chart jsx-element?]
                              [args external/raw] ...)
         external/raw]{
@(jsx-bar "drawLine"
          (string-append (jsx-doc-url "Chart")
                         "#drawLine"))
Draws a line chart rendering.
}

@defproc[(jsx-chart-draw-pie [chart jsx-element?]
                             [args external/raw] ...)
         external/raw]{
@(jsx-bar "drawPie"
          (string-append (jsx-doc-url "Chart")
                         "#drawPie"))
Draws a pie chart rendering.
}

@defproc[(jsx-chart-draw-points [chart jsx-element?]
                                [args external/raw] ...)
         external/raw]{
@(jsx-bar "drawPoints"
          (string-append (jsx-doc-url "Chart")
                         "#drawPoints"))
Draws a point chart rendering.
}

@defproc[(jsx-chart-draw-radar [chart jsx-element?]
                               [args external/raw] ...)
         external/raw]{
@(jsx-bar "drawRadar"
          (string-append (jsx-doc-url "Chart")
                         "#drawRadar"))
Draws a radar chart rendering.
}

@defproc[(jsx-chart-draw-spline [chart jsx-element?]
                                [args external/raw] ...)
         external/raw]{
@(jsx-bar "drawSpline"
          (string-append (jsx-doc-url "Chart")
                         "#drawSpline"))
Draws a spline chart rendering.
}

@defproc[(jsx-chart-update-data-array! [chart jsx-element?]
                                       [args external/raw] ...)
         external/raw]{
@(jsx-bar "updateDataArray"
          (string-append (jsx-doc-url "Chart")
                         "#updateDataArray"))
Updates the chart data array.
}

@defproc[(jsx-chart-update-renderer! [chart jsx-element?]
                                     [args external/raw] ...)
         external/raw]{
@(jsx-bar "updateRenderer"
          (string-append (jsx-doc-url "Chart")
                         "#updateRenderer"))
Updates the chart renderer.
}

@defproc[(jsx-create-legend [board jsx-board?]
                            [parents vector?]
                            [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Legend"
          (jsx-doc-url "Legend"))
Creates a legend for chart or plot data.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define legend (jsx-create-legend board (jsx-parents)))
]
}

@defproc[(jsx-create-smartlabel [board jsx-board?]
                                [parents vector?]
                                [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Smartlabel"
          (jsx-doc-url "Smartlabel"))
Creates a label that updates from a value or expression.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define label (jsx-create-smartlabel board (jsx-parents 0 0 "A")))
]
}

@defproc[(jsx-create-text [board jsx-board?]
                          [parents vector?]
                          [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Text"
          (jsx-doc-url "Text"))
Creates a text element anchored to the board.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define text (jsx-create-text board (jsx-parents 0 0 "Hello")))
]
}

@defproc[(jsx-create-image [board jsx-board?]
                           [parents vector?]
                           [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Image"
          (jsx-doc-url "Image"))
Creates an image element anchored to the board.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define img (jsx-create-image board (jsx-parents "example.png" (vector 0 0) (vector 1 1))))
]
}

@defproc[(jsx-create-group [board jsx-board?]
                           [parents vector?]
                           [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Group"
          (jsx-doc-url "JXG.Group"))
Creates a grouped geometry object.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents 0 0)))
(define b (jsx-create-point board (jsx-parents 1 1)))
(define group (jsx-create-group board (jsx-parents a b)))
]
}

@defproc[(jsx-create-foreignobject [board jsx-board?]
                                   [parents vector?]
                                   [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "ForeignObject"
          (jsx-doc-url "ForeignObject"))
Creates embedded foreign DOM content.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define fo (jsx-create-foreignobject board (jsx-parents "<div>Hi</div>" (vector 0 0))))
]
}

@defproc[(jsx-create-tapemeasure [board jsx-board?]
                                 [parents vector?]
                                 [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Tapemeasure"
          (jsx-doc-url "Tapemeasure"))
Creates an interactive tape-measure tool.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define tm (jsx-create-tapemeasure board (jsx-parents (vector 0 0) (vector 1 1))))
]
}

@defproc[(jsx-tapemeasure-value [tapemeasure jsx-element?])
         real?]{
@(jsx-bar "Value"
          (string-append (jsx-doc-url "JXG.Tapemeasure")
                         "#Value"))
Returns the current tape-measure value.
}

@defproc[(jsx-create-hatch [board jsx-board?]
                           [parents vector?]
                           [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Hatch"
          (jsx-doc-url "Hatch"))
Creates hatch marks on a line or segment.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents 0 0)))
(define b (jsx-create-point board (jsx-parents 1 0)))
(define h (jsx-create-hatch board (jsx-parents a b)))
]
}

@defproc[(jsx-create-measurement [board jsx-board?]
                                 [parents vector?]
                                 [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Measurement"
          (jsx-doc-url "Measurement"))
Creates a measurement label or ruler-like helper.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define m (jsx-create-measurement board (jsx-parents (vector 0 0) (vector 1 1))))
]
}

@defproc[(jsx-create-bisectorlines [board jsx-board?]
                                   [parents vector?]
                                   [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Bisectorlines"
          (jsx-doc-url "Bisectorlines"))
Creates helper lines for angle bisectors.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define bl (jsx-create-bisectorlines board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-perpendicularsegment [board jsx-board?]
                                         [parents vector?]
                                         [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "PerpendicularSegment"
          (jsx-doc-url "PerpendicularSegment"))
Creates a segment dropped perpendicularly to a line.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 1 0)))
(define c (jsx-create-point board (jsx-parents 0 1)))
(define seg (jsx-create-perpendicularsegment board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-circumcenter [board jsx-board?]
                                  [parents vector?]
                                  [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Circumcenter"
          (jsx-doc-url "Circumcenter"))
Creates the center of the circle through a triangle's vertices.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define cc (jsx-create-circumcenter board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-mirrorelement [board jsx-board?]
                                   [parents vector?]
                                   [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "MirrorElement"
          (jsx-doc-url "MirrorElement"))
Creates an element mirrored across a line or point.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 1 0)))
(define c (jsx-create-point board (jsx-parents 0 1)))
(define m (jsx-create-mirrorelement board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-mirrorpoint [board jsx-board?]
                                 [parents vector?]
                                 [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "MirrorPoint"
          (jsx-doc-url "MirrorPoint"))
Creates the reflected copy of a point.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 1 0)))
(define c (jsx-create-point board (jsx-parents 0 1)))
(define m (jsx-create-mirrorpoint board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-otherintersection [board jsx-board?]
                                       [parents vector?]
                                       [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "OtherIntersection"
          (jsx-doc-url "OtherIntersection"))
Creates the second intersection point of two objects.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 1 0)))
(define c (jsx-create-point board (jsx-parents 0 1)))
(define i (jsx-create-otherintersection board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-orthogonalprojection [board jsx-board?]
                                          [parents vector?]
                                          [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Orthogonalprojection"
          (jsx-doc-url "Orthogonalprojection"))
Creates the foot of an orthogonal projection.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 1 0)))
(define c (jsx-create-point board (jsx-parents 0 1)))
(define p (jsx-create-orthogonalprojection board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-parallelpoint [board jsx-board?]
                                   [parents vector?]
                                   [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Parallelpoint"
          (jsx-doc-url "Parallelpoint"))
Creates a point constrained by a parallel construction.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 1 0)))
(define c (jsx-create-point board (jsx-parents 0 1)))
(define p (jsx-create-parallelpoint board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-perpendicularpoint [board jsx-board?]
                                        [parents vector?]
                                        [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "PerpendicularPoint"
          (jsx-doc-url "PerpendicularPoint"))
Creates the foot of a perpendicular from a point to a line.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 1 0)))
(define c (jsx-create-point board (jsx-parents 0 1)))
(define p (jsx-create-perpendicularpoint board (jsx-parents a b c)))
]
}

@subsection{ForeignObject Helpers}

The @racket[jsx-foreignobject] wrappers expose the documented
@racketid[JXG.ForeignObject] methods that are specific to embedded HTML
content inside a JSXGraph board.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define fo
  (jsx-create-foreignobject
   board
   (jsx-parents "<div>ForeignObject</div>"
                (jsx-parents -3 1)
                (jsx-parents 220 80))))
(jsx-foreignobject-update! fo)
]

@defproc[(jsx-foreignobject-H [foreignobject jsx-element?]
                              [args external/raw] ...)
         real?]{
@(jsx-bar "H"
          (string-append (jsx-doc-url "JXG.ForeignObject")
                         "#H"))
Returns the foreign object H helper.
}

@defproc[(jsx-foreignobject-W [foreignobject jsx-element?]
                              [args external/raw] ...)
         real?]{
@(jsx-bar "W"
          (string-append (jsx-doc-url "JXG.ForeignObject")
                         "#W"))
Returns the foreign object W helper.
}

@defproc[(jsx-foreignobject-has-point? [foreignobject jsx-element?]
                                       [args external/raw] ...)
         boolean?]{
@(jsx-bar "hasPoint"
          (string-append (jsx-doc-url "JXG.ForeignObject")
                         "#hasPoint"))
Checks whether a point hits @racket[foreignobject].
}

@defproc[(jsx-foreignobject-set-size! [foreignobject jsx-element?]
                                      [args external/raw] ...)
         void?]{
@(jsx-bar "setSize"
          (string-append (jsx-doc-url "JXG.ForeignObject")
                         "#setSize"))
Sets the size of @racket[foreignobject].
}

@defproc[(jsx-foreignobject-update! [foreignobject jsx-element?]
                                    [args external/raw] ...)
         void?]{
@(jsx-bar "update"
          (string-append (jsx-doc-url "JXG.ForeignObject")
                         "#update"))
Updates @racket[foreignobject].
}

@defproc[(jsx-foreignobject-update-renderer! [foreignobject jsx-element?]
                                             [args external/raw] ...)
         void?]{
@(jsx-bar "updateRenderer"
          (string-append (jsx-doc-url "JXG.ForeignObject")
                         "#updateRenderer"))
Refreshes the foreign object renderer.
}

@defproc[(jsx-foreignobject-update-size! [foreignobject jsx-element?]
                                         [args external/raw] ...)
         void?]{
@(jsx-bar "updateSize"
          (string-append (jsx-doc-url "JXG.ForeignObject")
                         "#updateSize"))
Updates the foreign object size.
}

@defproc[(jsx-foreignobject-update-span! [foreignobject jsx-element?]
                                         [args external/raw] ...)
         void?]{
@(jsx-bar "updateSpan"
          (string-append (jsx-doc-url "JXG.ForeignObject")
                         "#updateSpan"))
Updates the foreign object span.
}

@subsection{Text Helpers}

The @racket[jsx-text] wrappers expose the documented
@racketid[JXG.Text] methods that are specific to text elements.
Inherited geometry and coordinate helpers are provided through the
generic @racket[jsx-element] wrappers.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define p (jsx-create-point board (jsx-parents 0 0)))
(define txt (jsx-create-text board (jsx-parents p 1 1 "Hello")))
(jsx-text-set-text! txt "World")
]

@defproc[(jsx-text-_createFctUpdateText [text jsx-element?]
                                        [args external/raw] ...)
         external/raw]{
@(jsx-bar "_createFctUpdateText"
          (string-append (jsx-doc-url "JXG.Text")
                         "#_createFctUpdateText"))
Creates the internal update function used for text rendering.
}

@defproc[(jsx-text-_setText [text jsx-element?]
                            [args external/raw] ...)
         external/raw]{
@(jsx-bar "_setText"
          (string-append (jsx-doc-url "JXG.Text")
                         "#_setText"))
Sets the internal text representation directly.
}

@defproc[(jsx-text-bounds [text jsx-element?]
                          [args external/raw] ...)
         external/raw]{
@(jsx-bar "bounds"
          (string-append (jsx-doc-url "JXG.Text")
                         "#bounds"))
Returns the text bounds.
}

@defproc[(jsx-text-checkForSizeUpdate [text jsx-element?]
                                      [args external/raw] ...)
         external/raw]{
@(jsx-bar "checkForSizeUpdate"
          (string-append (jsx-doc-url "JXG.Text")
                         "#checkForSizeUpdate"))
Checks whether the text size needs recomputation.
}

@defproc[(jsx-text-convertGeonext2CSS [text jsx-element?]
                                      [args external/raw] ...)
         external/raw]{
@(jsx-bar "convertGeonext2CSS"
          (string-append (jsx-doc-url "JXG.Text")
                         "#convertGeonext2CSS"))
Converts GEONExT markup to CSS.
}

@defproc[(jsx-text-convertGeonextAndSketchometry2CSS [text jsx-element?]
                                                     [args external/raw] ...)
         external/raw]{
@(jsx-bar "convertGeonextAndSketchometry2CSS"
          (string-append (jsx-doc-url "JXG.Text")
                         "#convertGeonextAndSketchometry2CSS"))
Converts GEONExT and Sketchometry markup to CSS.
}

@defproc[(jsx-text-convertSketchometry2CSS [text jsx-element?]
                                           [args external/raw] ...)
         external/raw]{
@(jsx-bar "convertSketchometry2CSS"
          (string-append (jsx-doc-url "JXG.Text")
                         "#convertSketchometry2CSS"))
Converts Sketchometry markup to CSS.
}

@defproc[(jsx-text-crudeSizeEstimate [text jsx-element?]
                                     [args external/raw] ...)
         external/raw]{
@(jsx-bar "crudeSizeEstimate"
          (string-append (jsx-doc-url "JXG.Text")
                         "#crudeSizeEstimate"))
Returns a crude size estimate for the text.
}

@defproc[(jsx-text-escapeTicks [text jsx-element?]
                               [args external/raw] ...)
         external/raw]{
@(jsx-bar "escapeTicks"
          (string-append (jsx-doc-url "JXG.Text")
                         "#escapeTicks"))
Escapes tick marks in the text.
}

@defproc[(jsx-text-expandShortMath [text jsx-element?]
                                   [args external/raw] ...)
         external/raw]{
@(jsx-bar "expandShortMath"
          (string-append (jsx-doc-url "JXG.Text")
                         "#expandShortMath"))
Expands short math notation in the text.
}

@defproc[(jsx-text-generateTerm [text jsx-element?]
                                [args external/raw] ...)
         external/raw]{
@(jsx-bar "generateTerm"
          (string-append (jsx-doc-url "JXG.Text")
                         "#generateTerm"))
Generates the text term.
}

@defproc[(jsx-text-getAnchorX [text jsx-element?]
                              [args external/raw] ...)
         external/raw]{
@(jsx-bar "getAnchorX"
          (string-append (jsx-doc-url "JXG.Text")
                         "#getAnchorX"))
Returns the X anchor position for the text.
}

@defproc[(jsx-text-getAnchorY [text jsx-element?]
                              [args external/raw] ...)
         external/raw]{
@(jsx-bar "getAnchorY"
          (string-append (jsx-doc-url "JXG.Text")
                         "#getAnchorY"))
Returns the Y anchor position for the text.
}

@defproc[(jsx-text-getNumberOfConflicts [text jsx-element?]
                                        [args external/raw] ...)
         exact-nonnegative-integer?]{
@(jsx-bar "getNumberOfConflicts"
          (string-append (jsx-doc-url "JXG.Text")
                         "#getNumberOfConflicts"))
Returns the number of text placement conflicts.
}

@defproc[(jsx-text-getSize [text jsx-element?]
                           [args external/raw] ...)
         external/raw]{
@(jsx-bar "getSize"
          (string-append (jsx-doc-url "JXG.Text")
                         "#getSize"))
Returns the size of the text.
}

@defproc[(jsx-text-hasPoint [text jsx-element?]
                            [args external/raw] ...)
         boolean?]{
@(jsx-bar "hasPoint"
          (string-append (jsx-doc-url "JXG.Text")
                         "#hasPoint"))
Checks whether screen coordinates hit the text.
}

@defproc[(jsx-text-notifyParents [text jsx-element?]
                                 [args external/raw] ...)
         void?]{
@(jsx-bar "notifyParents"
          (string-append (jsx-doc-url "JXG.Text")
                         "#notifyParents"))
Notifies parent elements that the text changed.
}

@defproc[(jsx-text-poorMansTeX [text jsx-element?]
                               [args external/raw] ...)
         external/raw]{
@(jsx-bar "poorMansTeX"
          (string-append (jsx-doc-url "JXG.Text")
                         "#poorMansTeX"))
Renders poor-man's TeX markup.
}

@defproc[(jsx-text-replaceSub [text jsx-element?]
                              [args external/raw] ...)
         external/raw]{
@(jsx-bar "replaceSub"
          (string-append (jsx-doc-url "JXG.Text")
                         "#replaceSub"))
Replaces a subscript fragment.
}

@defproc[(jsx-text-replaceSup [text jsx-element?]
                              [args external/raw] ...)
         external/raw]{
@(jsx-bar "replaceSup"
          (string-append (jsx-doc-url "JXG.Text")
                         "#replaceSup"))
Replaces a superscript fragment.
}

@defproc[(jsx-text-setAutoPosition [text jsx-element?]
                                   [args external/raw] ...)
         void?]{
@(jsx-bar "setAutoPosition"
          (string-append (jsx-doc-url "JXG.Text")
                         "#setAutoPosition"))
Turns automatic positioning on or off.
}

@defproc[(jsx-text-setCoords [text jsx-element?]
                             [args external/raw] ...)
         void?]{
@(jsx-bar "setCoords"
          (string-append (jsx-doc-url "JXG.Text")
                         "#setCoords"))
Sets the text coordinates.
}

@defproc[(jsx-text-setText [text jsx-element?]
                           [args external/raw] ...)
         void?]{
@(jsx-bar "setText"
          (string-append (jsx-doc-url "JXG.Text")
                         "#setText"))
Sets the displayed text.
}

@defproc[(jsx-text-setTextJessieCode [text jsx-element?]
                                     [args external/raw] ...)
         void?]{
@(jsx-bar "setTextJessieCode"
          (string-append (jsx-doc-url "JXG.Text")
                         "#setTextJessieCode"))
Sets the text from JessieCode.
}

@defproc[(jsx-text-unescapeTicks [text jsx-element?]
                                 [args external/raw] ...)
         external/raw]{
@(jsx-bar "unescapeTicks"
          (string-append (jsx-doc-url "JXG.Text")
                         "#unescapeTicks"))
Unescapes tick marks in the text.
}

@defproc[(jsx-text-updateSize [text jsx-element?]
                              [args external/raw] ...)
         void?]{
@(jsx-bar "updateSize"
          (string-append (jsx-doc-url "JXG.Text")
                         "#updateSize"))
Updates the text size.
}

@defproc[(jsx-text-updateText [text jsx-element?]
                              [args external/raw] ...)
         void?]{
@(jsx-bar "updateText"
          (string-append (jsx-doc-url "JXG.Text")
                         "#updateText"))
Refreshes the rendered text.
}

@defproc[(jsx-text-utf8_decode [text jsx-element?]
                               [args external/raw] ...)
         external/raw]{
@(jsx-bar "utf8_decode"
          (string-append (jsx-doc-url "JXG.Text")
                         "#utf8_decode"))
Decodes UTF-8 text data.
}

@defproc[(jsx-text-valueTagToJessieCode [text jsx-element?]
                                        [args external/raw] ...)
         external/raw]{
@(jsx-bar "valueTagToJessieCode"
          (string-append (jsx-doc-url "JXG.Text")
                         "#valueTagToJessieCode"))
Converts a value tag to JessieCode.
}

@subsection{Image Helpers}

The @racket[jsx-image] wrappers expose the documented
@racketid[JXG.Image] methods that are specific to image elements.
Inherited geometry and coordinate helpers are provided through the
generic @racket[jsx-element] wrappers.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define img
  (jsx-create-image
   board
   (jsx-parents "https://jsxgraph.org/jsxgraph/distrib/images/uccellino.jpg"
                (jsx-parents -4 -3)
                (jsx-parents 2 2))))
(jsx-image-update-size! img)
]

@defproc[(jsx-image-H [image jsx-element?]
                      [args external/raw] ...)
         real?]{
@(jsx-bar "H"
          (string-append (jsx-doc-url "JXG.Image")
                         "#H"))
Reads the image H helper.
}

@defproc[(jsx-image-W [image jsx-element?]
                      [args external/raw] ...)
         real?]{
@(jsx-bar "W"
          (string-append (jsx-doc-url "JXG.Image")
                         "#W"))
Reads the image W helper.
}

@defproc[(jsx-image-has-point? [image jsx-element?]
                               [args external/raw] ...)
         boolean?]{
@(jsx-bar "hasPoint"
          (string-append (jsx-doc-url "JXG.Image")
                         "#hasPoint"))
Tests whether a screen position hits the image.
}

@defproc[(jsx-image-set-size! [image jsx-element?]
                              [args external/raw] ...)
         void?]{
@(jsx-bar "setSize"
          (string-append (jsx-doc-url "JXG.Image")
                         "#setSize"))
Sets the image size.
}

@defproc[(jsx-image-update! [image jsx-element?]
                            [args external/raw] ...)
         void?]{
@(jsx-bar "update"
          (string-append (jsx-doc-url "JXG.Image")
                         "#update"))
Updates the image.
}

@defproc[(jsx-image-update-renderer! [image jsx-element?]
                                     [args external/raw] ...)
         void?]{
@(jsx-bar "updateRenderer"
          (string-append (jsx-doc-url "JXG.Image")
                         "#updateRenderer"))
Refreshes the image renderer.
}

@defproc[(jsx-image-update-size! [image jsx-element?]
                                 [args external/raw] ...)
         void?]{
@(jsx-bar "updateSize"
          (string-append (jsx-doc-url "JXG.Image")
                         "#updateSize"))
Updates the image size.
}

@defproc[(jsx-image-update-span! [image jsx-element?]
                                 [args external/raw] ...)
         void?]{
@(jsx-bar "updateSpan"
          (string-append (jsx-doc-url "JXG.Image")
                         "#updateSpan"))
Updates the image span.
}

@subsection{Group Helpers}

The @racket[jsx-group] wrappers expose the documented
@racketid[JXG.Group] methods that are useful for the gallery example.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define p (jsx-create-point board (jsx-parents 0 0)))
(define q (jsx-create-point board (jsx-parents 1 1)))
(define g (jsx-create-group board (jsx-parents p)))
(jsx-group-add-point! g q)
]

@defproc[(jsx-group-add-parents! [group jsx-element?]
                                 [parents vector?])
         void?]{
@(jsx-bar "addParents"
          (string-append (jsx-doc-url "JXG.Group")
                         "#addParents"))
Adds parents to a group.
}

@defproc[(jsx-group-add-point! [group jsx-element?]
                               [object jsx-element?])
         void?]{
@(jsx-bar "addPoint"
          (string-append (jsx-doc-url "JXG.Group")
                         "#addPoint"))
Adds a point to a group.
}

@defproc[(jsx-group-add-points! [group jsx-element?]
                                [objects vector?])
         void?]{
@(jsx-bar "addPoints"
          (string-append (jsx-doc-url "JXG.Group")
                         "#addPoints"))
Adds multiple points to a group.
}

@defproc[(jsx-group-add-rotation-point! [group jsx-element?]
                                        [object jsx-element?])
         void?]{
@(jsx-bar "addRotationPoint"
          (string-append (jsx-doc-url "JXG.Group")
                         "#addRotationPoint"))
Adds a rotation point to a group.
}

@defproc[(jsx-group-add-scale-point! [group jsx-element?]
                                     [object jsx-element?])
         void?]{
@(jsx-bar "addScalePoint"
          (string-append (jsx-doc-url "JXG.Group")
                         "#addScalePoint"))
Adds a scale point to a group.
}

@defproc[(jsx-group-add-translation-point! [group jsx-element?]
                                            [object jsx-element?])
         void?]{
@(jsx-bar "addTranslationPoint"
          (string-append (jsx-doc-url "JXG.Group")
                         "#addTranslationPoint"))
Adds a translation point to a group.
}

@defproc[(jsx-group-set-scale-center! [group jsx-element?]
                                      [object jsx-element?])
         void?]{
@(jsx-bar "setScaleCenter"
          (string-append (jsx-doc-url "JXG.Group")
                         "#setScaleCenter"))
Sets the scale center for a group.
}

@defproc[(jsx-group-set-rotation-center! [group jsx-element?]
                                         [object jsx-element?])
         void?]{
@(jsx-bar "setRotationCenter"
          (string-append (jsx-doc-url "JXG.Group")
                         "#setRotationCenter"))
Sets the rotation center for a group.
}

@defproc[(jsx-group-set-rotation-points! [group jsx-element?]
                                         [objects vector?])
         void?]{
@(jsx-bar "setRotationPoints"
          (string-append (jsx-doc-url "JXG.Group")
                         "#setRotationPoints"))
Sets the rotation points for a group.
}

@defproc[(jsx-group-set-translation-points! [group jsx-element?]
                                            [objects vector?])
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

@section{2D Helper Constructors}

This section covers helper constructors that are easiest to exercise as
standalone boards in the gallery.

These constructors cover helper classes that are easiest to exercise as
standalone boards in the gallery.

@subsection{Ticks, Transformations, And Traces}

@defproc[(jsx-create-ticks [board jsx-board?]
                           [parents vector?]
                           [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Ticks"
          (jsx-doc-url "Ticks"))
Creates a ticks object on a board.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define ticks (jsx-create-ticks board (jsx-parents)))
]
}

@defproc[(jsx-create-transformation [board jsx-board?]
                                    [parents vector?]
                                    [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Transformation"
          (jsx-doc-url "Transformation"))
Creates a transformation object on a board.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define t (jsx-create-transformation board (jsx-parents)))
]
}

@defproc[(jsx-create-tracecurve [board jsx-board?]
                                [parents vector?]
                                [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Tracecurve"
          (jsx-doc-url "Tracecurve"))
Creates a tracecurve object on a board.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define t (jsx-create-tracecurve board (jsx-parents (lambda (x) (* x x)))))
]
}

@subsection{Parallelograms And Angle Helpers}

@defproc[(jsx-create-parallelogram [board jsx-board?]
                                   [parents vector?]
                                   [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "Parallelogram"
          (jsx-doc-url "Parallelogram"))
Creates a parallelogram object on a board.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents 0 0)))
(define b (jsx-create-point board (jsx-parents 1 0)))
(define c (jsx-create-point board (jsx-parents 0 1)))
(define d (jsx-create-parallelogram board (jsx-parents a b c)))
]
}

@defproc[(jsx-create-reflexangle [board jsx-board?]
                                 [parents vector?]
                                 [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "ReflexAngle"
          (jsx-doc-url "ReflexAngle"))
Creates a reflex angle object on a board.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define ang (jsx-create-reflexangle board (jsx-parents a b c)))
]
}

@subsection{Reflex And Non-Reflex Angles}

@defproc[(jsx-create-nonreflexangle [board jsx-board?]
                                     [parents vector?]
                                     [attributes (or/c #f external/raw) #f])
         jsx-element?]{
@(jsx-bar "NonReflexAngle"
          (jsx-doc-url "NonReflexAngle"))
Creates a non-reflex angle on a board.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents -1 0)))
(define b (jsx-create-point board (jsx-parents 0 1)))
(define c (jsx-create-point board (jsx-parents 1 0)))
(define ang (jsx-create-nonreflexangle board (jsx-parents a b c)))
]
}

@section{Board and Element Methods}

This section collects the wrappers that operate on boards, geometry
elements, and the shared geometry-element API.

The @racket[jsx-element] wrapper covers JSXGraph geometry elements and
compositions. The methods below mirror the documented
@racketid[JXG.GeometryElement] API where it is useful from Racket.
Some low-level hooks are intentionally left broad because JSXGraph
itself forwards raw browser values through them.

@subsection{Geometry Element Methods}

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents 0 0)))
(jsx-element-get-name a)
(jsx-element-hide! a)
]

@defproc[(jsx-element-get-attribute [element jsx-element?]
                                    [key (or/c string? symbol?)])
         external/raw]{
@(jsx-bar "getAttribute"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#getAttribute"))
Reads a geometry element attribute.
}

@defproc[(jsx-element-get-attributes [element jsx-element?])
         external/raw]{
@(jsx-bar "getAttributes"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#getAttributes"))
Reads all geometry element attributes.
}

@defproc[(jsx-element-get-label-anchor [element jsx-element?])
         external/raw]{
@(jsx-bar "getLabelAnchor"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#getLabelAnchor"))
Reads the label anchor for a geometry element.
}

@defproc[(jsx-element-get-name [element jsx-element?])
         string?]{
@(jsx-bar "getName"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#getName"))
Reads the element name.
}

@defproc[(jsx-element-get-parents [element jsx-element?])
         external/raw]{
@(jsx-bar "getParents"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#getParents"))
Reads the element parents.
}

@defproc[(jsx-element-get-property [element jsx-element?]
                                   [key (or/c string? symbol?)])
         external/raw]{
@(jsx-bar "getProperty"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#getProperty"))
Reads the deprecated geometry element property alias.
}

@defproc[(jsx-element-get-snap-sizes [element jsx-element?])
         external/raw]{
@(jsx-bar "getSnapSizes"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#getSnapSizes"))
Reads the element snap sizes.
}

@defproc[(jsx-element-get-text-anchor [element jsx-element?])
         external/raw]{
@(jsx-bar "getTextAnchor"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#getTextAnchor"))
Reads the text anchor for a geometry element.
}

@defproc[(jsx-element-get-type [element jsx-element?])
         string?]{
@(jsx-bar "getType"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#getType"))
Reads the element type.
}

@defproc[(jsx-element-has-point? [element jsx-element?]
                                 [x real?]
                                 [y real?])
         boolean?]{
@(jsx-bar "hasPoint"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#hasPoint"))
Checks whether screen coordinates hit the element.
}

@defproc[(jsx-element-hide! [element jsx-element?])
         void?]{
@(jsx-bar "hide"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#hide"))
Hides a geometry element.
}

@defproc[(jsx-element-hide-element! [element jsx-element?])
         void?]{
@(jsx-bar "hideElement"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#hideElement"))
Hides a geometry element using the documented alias.
}

@defproc[(jsx-element-no-highlight! [element jsx-element?])
         void?]{
@(jsx-bar "noHighlight"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#noHighlight"))
Removes highlighting from a geometry element.
}

@defproc[(jsx-element-prepare-update! [element jsx-element?])
         void?]{
@(jsx-bar "prepareUpdate"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#prepareUpdate"))
Prepares a geometry element for update.
}

@defproc[(jsx-element-remove! [element jsx-element?])
         void?]{
@(jsx-bar "remove"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#remove"))
Removes a geometry element.
}

@defproc[(jsx-element-remove-all-ticks! [element jsx-element?])
         void?]{
@(jsx-bar "removeAllTicks"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#removeAllTicks"))
Removes all ticks from a geometry element.
}

@defproc[(jsx-element-remove-child! [element jsx-element?]
                                    [child jsx-element?])
         void?]{
@(jsx-bar "removeChild"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#removeChild"))
Removes a dependent child from a geometry element.
}

@defproc[(jsx-element-remove-descendants! [element jsx-element?]
                                          [obj jsx-element?])
         void?]{
@(jsx-bar "removeDescendants"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#removeDescendants"))
Removes a descendant from a geometry element.
}

@defproc[(jsx-element-remove-event! [element jsx-element?]
                                    [handler procedure?])
         void?]{
@(jsx-bar "removeEvent"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#removeEvent"))
Removes a geometry element event handler.
}

@defproc[(jsx-element-remove-ticks! [element jsx-element?]
                                    [tick jsx-element?])
         void?]{
@(jsx-bar "removeTicks"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#removeTicks"))
Removes ticks from a geometry element.
}

@defproc[(jsx-element-set-attribute! [element jsx-element?]
                                     [attributes external/raw])
         void?]{
@(jsx-bar "setAttribute"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#setAttribute"))
Sets geometry element attributes.
}

@defproc[(jsx-element-set-label! [element jsx-element?]
                                 [str string?])
         void?]{
@(jsx-bar "setLabel"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#setLabel"))
Sets the geometry element label.
}

@defproc[(jsx-element-set-label-text! [element jsx-element?]
                                      [str string?])
         void?]{
@(jsx-bar "setLabelText"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#setLabelText"))
Sets the label text.
}

@defproc[(jsx-element-set-name! [element jsx-element?]
                                [str string?])
         void?]{
@(jsx-bar "setName"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#setName"))
Sets the geometry element name.
}

@defproc[(jsx-element-set-parents! [element jsx-element?]
                                   [parents (or/c vector? list?)])
         void?]{
@(jsx-bar "setParents"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#setParents"))
Sets the geometry element parents.
}

@defproc[(jsx-element-set-position! [element jsx-element?]
                                    [method (or/c string? symbol?)]
                                    [coords (or/c vector? list?)])
         void?]{
@(jsx-bar "setPosition"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#setPosition"))
Sets the geometry element position.
}

@defproc[(jsx-element-set-position-directly! [element jsx-element?]
                                             [method (or/c string? symbol?)]
                                             [coords (or/c vector? list?)]
                                             [oldcoords (or/c vector? list?)])
         void?]{
@(jsx-bar "setPositionDirectly"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#setPositionDirectly"))
Sets the geometry element position directly.
}

@defproc[(jsx-element-set-property! [element jsx-element?]
                                    [attributes external/raw])
         void?]{
@(jsx-bar "setProperty"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#setProperty"))
Sets the deprecated geometry element property alias.
}

@defproc[(jsx-element-show! [element jsx-element?])
         void?]{
@(jsx-bar "show"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#show"))
Shows a geometry element.
}

@defproc[(jsx-element-show-element! [element jsx-element?])
         void?]{
@(jsx-bar "showElement"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#showElement"))
Shows a geometry element using the documented alias.
}

@defproc[(jsx-element-update! [element jsx-element?])
         void?]{
@(jsx-bar "update"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#update"))
Updates a geometry element.
}

@defproc[(jsx-element-update-renderer! [element jsx-element?])
         void?]{
@(jsx-bar "updateRenderer"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#updateRenderer"))
Updates the element renderer.
}

@defproc[(jsx-element-update-visibility! [element jsx-element?]
                                         [parent-val external/raw])
         void?]{
@(jsx-bar "updateVisibility"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#updateVisibility"))
Updates the visibility state of a geometry element.
}

@defproc[(jsx-element-use-locale! [element jsx-element?])
         void?]{
@(jsx-bar "useLocale"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#useLocale"))
Enables locale-aware number formatting on a geometry element.
}

@defproc[(jsx-element-add-child! [element jsx-element?]
                                 [obj jsx-element?])
         void?]{
@(jsx-bar "addChild"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#addChild"))
Adds a dependent child to a geometry element.
}

@defproc[(jsx-element-add-descendants! [element jsx-element?]
                                       [obj jsx-element?])
         void?]{
@(jsx-bar "addDescendants"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#addDescendants"))
Adds descendants to a geometry element.
}

@defproc[(jsx-element-add-parents! [element jsx-element?]
                                   [parents (or/c vector? list?)])
         void?]{
@(jsx-bar "addParents"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#addParents"))
Adds parents to a geometry element.
}

@defproc[(jsx-element-add-parents-from-jc-functions! [element jsx-element?]
                                                     [function-array external/raw])
         void?]{
@(jsx-bar "addParentsFromJCFunctions"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#addParentsFromJCFunctions"))
Adds parents derived from JC functions.
}

@defproc[(jsx-element-add-rotation! [element jsx-element?]
                                    [angle real?])
         void?]{
@(jsx-bar "addRotation"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#addRotation"))
Adds a rotation to a geometry element.
}

@defproc[(jsx-element-add-ticks! [element jsx-element?]
                                 [ticks jsx-element?])
         void?]{
@(jsx-bar "addTicks"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#addTicks"))
Adds ticks to a geometry element.
}

@defproc[(jsx-element-add-transform! [element jsx-element?]
                                     [transform jsx-element?])
         void?]{
@(jsx-bar "addTransform"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#addTransform"))
Adds a transform to a geometry element.
}

@defproc[(jsx-element-animate! [element jsx-element?]
                               [hash external/raw]
                               [time real?]
                               [options external/raw])
         external/raw]{
@(jsx-bar "animate"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#animate"))
Animates a geometry element.
}

@defproc[(jsx-element-bounds [element jsx-element?])
         vector?]{
@(jsx-bar "bounds"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#bounds"))
Reads the bounds of a geometry element.
}

@defproc[(jsx-element-clear-trace! [element jsx-element?])
         void?]{
@(jsx-bar "clearTrace"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#clearTrace"))
Clears the trace of a geometry element.
}

@defproc[(jsx-element-clone-to-background! [element jsx-element?])
         jsx-element?]{
@(jsx-bar "cloneToBackground"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#cloneToBackground"))
Clones a geometry element to the background.
}

@defproc[(jsx-element-count-children [element jsx-element?])
         exact-nonnegative-integer?]{
@(jsx-bar "countChildren"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#countChildren"))
Counts the direct children of a geometry element.
}

@defproc[(jsx-element-create-gradient! [element jsx-element?])
         void?]{
@(jsx-bar "createGradient"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#createGradient"))
Creates a gradient for a geometry element.
}

@defproc[(jsx-element-create-label! [element jsx-element?])
         void?]{
@(jsx-bar "createLabel"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#createLabel"))
Creates a label for a geometry element.
}

@defproc[(jsx-element-draggable? [element jsx-element?])
         boolean?]{
@(jsx-bar "draggable"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#draggable"))
Checks whether a geometry element is draggable.
}

@defproc[(jsx-element-eval [element jsx-element?]
                           [val external/raw])
         external/raw]{
@(jsx-bar "eval"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#eval"))
Evaluates a geometry-element-specific value.
}

@defproc[(jsx-element-eval-vis-prop [element jsx-element?]
                                    [key (or/c string? symbol?)])
         external/raw]{
@(jsx-bar "evalVisProp"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#evalVisProp"))
Evaluates a visual property for a geometry element.
}

@defproc[(jsx-element-format-number-locale [element jsx-element?]
                                           [value real?]
                                           [digits exact-nonnegative-integer?])
         string?]{
@(jsx-bar "formatNumberLocale"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#formatNumberLocale"))
Formats a number using the element locale settings.
}

@defproc[(jsx-element-full-update! [element jsx-element?])
         void?]{
@(jsx-bar "fullUpdate"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#fullUpdate"))
Runs the full update chain for a geometry element.
}

@defproc[(jsx-element-generate-polynomial [element jsx-element?])
         external/raw]{
@(jsx-bar "generatePolynomial"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#generatePolynomial"))
Generates the polynomial for a geometry element.
}

@defproc[(jsx-element-handle-snap-to-grid! [element jsx-element?]
                                           [force boolean?]
                                           [from-parent external/raw])
         external/raw]{
@(jsx-bar "handleSnapToGrid"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#handleSnapToGrid"))
Handles snapping a geometry element to the grid.
}

@defproc[(jsx-element-normalize! [element jsx-element?])
         void?]{
@(jsx-bar "normalize"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#normalize"))
Normalizes a geometry element.
}

@defproc[(jsx-element-resolve-shortcuts! [element jsx-element?]
                                         [attributes external/raw])
         external/raw]{
@(jsx-bar "resolveShortcuts"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#resolveShortcuts"))
Resolves geometry-element attribute shortcuts.
}

@defproc[(jsx-element-set-arrow! [element jsx-element?]
                                 [first-arrow boolean?]
                                 [last-arrow boolean?])
         void?]{
@(jsx-bar "setArrow"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#setArrow"))
Sets arrow flags on a geometry element.
}

@defproc[(jsx-element-set-dash! [element jsx-element?]
                                [dash exact-integer?])
         void?]{
@(jsx-bar "setDash"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#setDash"))
Sets the dash style on a geometry element.
}

@defproc[(jsx-element-set-display-rend-node! [element jsx-element?]
                                             [val boolean?])
         void?]{
@(jsx-bar "setDisplayRendNode"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#setDisplayRendNode"))
Sets the display renderer node on a geometry element.
}

@defproc[(jsx-element-snap-to-points! [element jsx-element?])
         external/raw]{
@(jsx-bar "snapToPoints"
          (string-append (jsx-doc-url "JXG.GeometryElement")
                         "#snapToPoints"))
Snaps a geometry element to nearby points.
}

@subsection{Board Methods}

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define a (jsx-create-point board (jsx-parents 0 0)))
(jsx-board-count-children board)
(jsx-board-zoom-in! board 0 0)
]

@defproc[(jsx-board-count-children [board jsx-board?])
         exact-nonnegative-integer?]{
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
         string?]{
Reads the JSXGraph board id.
}

@defproc[(jsx-board-container [board jsx-board?])
         external/raw]{
Reads the board container element.
}

@defproc[(jsx-board-renderer [board jsx-board?])
         external/raw]{
Reads the renderer used by the board.
}

@defproc[(jsx-board-canvas-width [board jsx-board?])
         exact-nonnegative-integer?]{
Reads the board canvas width.
}

@defproc[(jsx-board-canvas-height [board jsx-board?])
         exact-nonnegative-integer?]{
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
                              [context (or/c jsx-board? external/raw) board])
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
                                              [apple-gestures boolean?])
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
                                   [element jsx-element?])
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
                                       [evt external/raw]
                                       [i (or/c #f exact-integer?) #f])
         vector?]{
@(jsx-bar "getMousePosition"
          (string-append (jsx-doc-url "JXG.Board")
                         "#getMousePosition"))
Returns the mouse position in screen coordinates.
}

@defproc[(jsx-board-get-usr-coords-of-mouse [board jsx-board?]
                                            [evt external/raw])
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
                                            [x real?]
                                            [y real?])
         vector?]{
@(jsx-bar "getScrCoordsOfMouse"
          (string-append (jsx-doc-url "JXG.Board")
                         "#getScrCoordsOfMouse"))
Returns the screen coordinates that JSXGraph uses for mouse tracking.
}

@defproc[(jsx-board-get-all-objects-under-mouse [board jsx-board?]
                                                 [evt external/raw])
         vector?]{
@(jsx-bar "getAllObjectsUnderMouse"
          (string-append (jsx-doc-url "JXG.Board")
                         "#getAllObjectsUnderMouse"))
Returns all objects under a pointer event.
}

@defproc[(jsx-board-get-all-under-mouse [board jsx-board?]
                                        [evt external/raw])
         vector?]{
@(jsx-bar "getAllUnderMouse"
          (string-append (jsx-doc-url "JXG.Board")
                         "#getAllUnderMouse"))
Returns the objects and coordinates under a pointer event.
}

@defproc[(jsx-board-set-attribute! [board jsx-board?]
                                   [attributes external/raw])
         void?]{
Sets arbitrary board attributes.
}

@defproc[(jsx-board-set-bounding-box! [board jsx-board?]
                                      [bbox vector?]
                                      [keepaspectratio boolean?]
                                      [setZoom boolean?])
         void?]{
Sets the board bounding box.
}

@defproc[(jsx-board-set-zoom! [board jsx-board?]
                              [fX real?]
                              [fY real?])
         void?]{
Sets the board zoom.
}

@defproc[(jsx-board-resize-container! [board jsx-board?]
                                      [canvasWidth exact-nonnegative-integer?]
                                      [canvasHeight exact-nonnegative-integer?]
                                      [dontset boolean?]
                                      [dontSetBoundingBox boolean?])
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
                           [str (or/c string? symbol?)]
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

@defproc[(jsx-board-zoom-in! [board jsx-board?] [x real?] [y real?])
         void?]{
Zooms in around a point.
}

@defproc[(jsx-board-zoom-out! [board jsx-board?] [x real?] [y real?])
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
                                           [width (or/c #f exact-nonnegative-integer?) #f]
                                           [height (or/c #f exact-nonnegative-integer?) #f])
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
                                     [drag boolean?])
         void?]{
@(jsx-bar "updateElements"
          (string-append (jsx-doc-url "JXG.Board")
                         "#updateElements"))
Updates the board elements.
}

@defproc[(jsx-board-update-hooks! [board jsx-board?]
                                  [m string?])
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
                                      [e external/raw])
         void?]{
@(jsx-bar "suppressDefault"
          (string-append (jsx-doc-url "JXG.Board")
                         "#suppressDefault"))
Suppresses the default event action.
}

@defproc[(jsx-board-init-infobox! [board jsx-board?]
                                  [attributes external/raw])
         void?]{
@(jsx-bar "initInfobox"
          (string-append (jsx-doc-url "JXG.Board")
                         "#initInfobox"))
Initializes the board infobox.
}

@defproc[(jsx-board-init-move-object! [board jsx-board?]
                                      [x real?]
                                      [y real?]
                                      [evt external/raw]
                                      [type string?])
         void?]{
@(jsx-bar "initMoveObject"
          (string-append (jsx-doc-url "JXG.Board")
                         "#initMoveObject"))
Prepares a board object move.
}

@defproc[(jsx-board-init-move-origin! [board jsx-board?]
                                      [x real?]
                                      [y real?])
         void?]{
@(jsx-bar "initMoveOrigin"
          (string-append (jsx-doc-url "JXG.Board")
                         "#initMoveOrigin"))
Prepares moving the board origin.
}

@defproc[(jsx-board-highlight-custom-infobox! [board jsx-board?]
                                              [text string?]
                                              [el (or/c #f jsx-element?) #f])
         void?]{
@(jsx-bar "highlightCustomInfobox"
          (string-append (jsx-doc-url "JXG.Board")
                         "#highlightCustomInfobox"))
Updates the info box text on @racket[board].
}

@defproc[(jsx-board-highlight-infobox! [board jsx-board?]
                                       [x real?]
                                       [y real?]
                                       [el (or/c #f jsx-element?) #f])
         void?]{
@(jsx-bar "highlightInfobox"
          (string-append (jsx-doc-url "JXG.Board")
                         "#highlightInfobox"))
Shows the given coordinates in the board info box.
}

@defproc[(jsx-board-move-object! [board jsx-board?]
                                 [x real?]
                                 [y real?]
                                 [o jsx-element?]
                                 [evt external/raw]
                                 [type string?])
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
                                     [c1 external/raw]
                                     [c2 external/raw]
                                     [start-c1 external/raw]
                                     [stepsize real?]
                                     [direction real?]
                                     [time real?]
                                     [pointlist vector?])
         external/raw]{
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
                                  [object external/raw])
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

@defproc[(jsx-board-to-fullscreen! [board jsx-board?] [id string?])
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
                                    [el jsx-element?])
         void?]{
@(jsx-bar "updateInfobox"
          (string-append (jsx-doc-url "JXG.Board")
                         "#updateInfobox"))
Updates the info box for @racket[el].
}

@defproc[(jsx-board-has-point? [board jsx-board?]
                               [x real?]
                               [y real?])
         boolean?]{
@(jsx-bar "hasPoint"
          (string-append (jsx-doc-url "JXG.Board")
                         "#hasPoint"))
Checks whether the point @racket[x], @racket[y] lies inside the board viewport.
}

@defproc[(jsx-board-move-origin! [board jsx-board?]
                                 [x real?]
                                 [y real?]
                                 [diff real?])
         void?]{
@(jsx-bar "moveOrigin"
          (string-append (jsx-doc-url "JXG.Board")
                         "#moveOrigin"))
Moves the origin of the board.
}

@defproc[(jsx-board-set-id [board jsx-board?]
                           [obj jsx-element?]
                           [type (or/c string? symbol?)])
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
                                   [elements (or/c vector? list?)])
         void?]{
@(jsx-bar "zoomElements"
          (string-append (jsx-doc-url "JXG.Board")
                         "#zoomElements"))
Zooms the board so the given elements fit in the viewport.
}

@section{Low-Level Notes}

These forms are the lowest-level wrappers in the library and are kept
intentionally broad when JSXGraph itself accepts a wide range of values.

The forms below are the intentionally broad boundary helpers. They
accept raw browser values or pass them through to JSXGraph with only
light wrapping.

@racketblock[
(include-lib jsx-graph)

(define board (jsx-create-board "box"))
(define p (jsx-create-point board (jsx-parents 0 0)))
(jsx-parents p (vector 1 1))
(jsx-coordinates p)
]

@defproc[(jsx-parents [v any/c] ...)
         vector?]{
Packs parent values into a vector for JSXGraph.

This is the intended variadic packing boundary, so it accepts checked
wrappers, raw browser values, vectors, lists, and callable parent
forms.
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

@para{The @racket[jsx-point?] predicate returns @racket[#t] when its
argument is a wrapped JSXGraph point. As a predicate, it accepts any
value.}

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
