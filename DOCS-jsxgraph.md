# Reference: `jsxgraph.ffi`

## Chapter 1 — Introduction

This document describes the JSXGraph bindings exported by `ffi/jsxgraph.ffi` in WebRacket.

JSXGraph is a browser-based JavaScript library for interactive mathematics and visualization. In WebRacket, `ffi/jsxgraph.ffi` gives direct access to selected board and point APIs.

Current scope:
- Board lifecycle and element creation
- Board properties and object lists
- GeometryElement bridge helpers
- Point predicates
- Point attributes (getters/setters)
- Point methods for hit-testing, style updates, and renderer updates
- Line, arc, angle, sector, glider, circle, conic, ellipse, curve, functiongraph,
  polygon, midpoint, parallel, perpendicular, reflection, bisector, normal,
  grid, boxplot, tangent, tangentto, polarline, polepoint, radicalaxis,
  circumcircle, incircle, circumcirclearc, circumcirclesector, semicircle,
  majorarc, majorsector, curveintersection, curvedifference, curveunion,
  derivative, integral, riemannsum, slopefield, vectorfield, implicitcurve,
  spline, cardinalspline, comb, metapostspline, polygonalchain, regularpolygon,
  hyperbola, parabola, stepfunction, inequality, turtle, slider,
  chart, legend, smartlabel, arrow, button, checkbox, input,
  foreignobject, tapemeasure, measurement, circumcenter, bisectorlines,
  perpendicularsegment, and mirrorelement
  bridge helpers
- Chart bridge helpers

Assumption in examples: the program is compiled with `--ffi jsxgraph`.

All function names are linked to JSXGraph API documentation.

### Table of Contents

- [Chapter 1 — Introduction](#chapter-1--introduction)
- [Chapter 2 — Conventions](#chapter-2--conventions)
- [2.1 Type Legend](#21-type-legend)
- [2.2 Boolean and Numeric Conventions](#22-boolean-and-numeric-conventions)
- [2.3 Common Setup Helpers](#23-common-setup-helpers)
- [Chapter 3 — Predicates](#chapter-3--predicates)
- [Chapter 4 — Point Properties (Getters)](#chapter-4--point-properties-getters)
- [Chapter 5 — Point Properties (Setters)](#chapter-5--point-properties-setters)
- [Chapter 6 — Point Methods](#chapter-6--point-methods)
- [Chapter 7 — Board API](#chapter-7--board-api)
- [7.1 Construction](#71-construction)
- [7.2 Lifecycle and Updates](#72-lifecycle-and-updates)
- [7.3 Board Properties](#73-board-properties)
- [Chapter 8 — GeometryElement Bridge](#chapter-8--geometryelement-bridge)
- [Chapter 9 — Line Bridge](#chapter-9--line-bridge)
- [Chapter 10 — Circle Bridge](#chapter-10--circle-bridge)
- [Chapter 11 — Curve Bridge](#chapter-11--curve-bridge)
- [Chapter 12 — Polygon Bridge](#chapter-12--polygon-bridge)
- [Chapter 13 — Arc Bridge](#chapter-13--arc-bridge)
- [Chapter 14 — Angle Bridge](#chapter-14--angle-bridge)
- [Chapter 15 — Sector Bridge](#chapter-15--sector-bridge)
- [Chapter 16 — Glider Bridge](#chapter-16--glider-bridge)
- [Chapter 17 — Text Bridge](#chapter-17--text-bridge)
- [Chapter 18 — Image Bridge](#chapter-18--image-bridge)
- [Chapter 19 — Chart Bridge](#chapter-19--chart-bridge)
- [Chapter 20 — Checkbox Bridge](#chapter-20--checkbox-bridge)
- [Chapter 21 — Input Bridge](#chapter-21--input-bridge)
- [Chapter 22 — Slider Bridge](#chapter-22--slider-bridge)
- [Chapter 23 — Mini Workflows](#chapter-23--mini-workflows)
- [Configure Point Snapping](#configure-point-snapping)
- [Hit-Testing and Projection](#hit-testing-and-projection)
- [Style and Renderer Refresh](#style-and-renderer-refresh)
- [Minimal Geometry Constructors](#minimal-geometry-constructors)
- [Chapter 24 — Coverage Checklist](#chapter-24--coverage-checklist)
- [Chapter 25 — ForeignObject Bridge](#chapter-25--foreignobject-bridge)
- [Chapter 26 — Tapemeasure Bridge](#chapter-26--tapemeasure-bridge)
- [Chapter 27 — Measurement Bridge](#chapter-27--measurement-bridge)
- [Chapter 28 — Circumcenter Bridge](#chapter-28--circumcenter-bridge)
- [Chapter 29 — MirrorElement Bridge](#chapter-29--mirrorelement-bridge)
- [Chapter 30 — MirrorPoint Bridge](#chapter-30--mirrorpoint-bridge)
- [Chapter 31 — OtherIntersection Bridge](#chapter-31--otherintersection-bridge)
- [Chapter 32 — Orthogonalprojection Bridge](#chapter-32--orthogonalprojection-bridge)
- [Chapter 33 — Parallelpoint Bridge](#chapter-33--parallelpoint-bridge)
- [Chapter 34 — PerpendicularPoint Bridge](#chapter-34--perpendicularpoint-bridge)
- [Chapter 35 — Bisectorlines Bridge](#chapter-35--bisectorlines-bridge)
- [Chapter 36 — PerpendicularSegment Bridge](#chapter-36--perpendicularsegment-bridge)
- [Chapter 37 — Incenter Bridge](#chapter-37--incenter-bridge)
- [Chapter 38 — MinorArc Bridge](#chapter-38--minorarc-bridge)
- [Chapter 39 — MinorSector Bridge](#chapter-39--minorsector-bridge)
- [Chapter 40 — NonReflexAngle Bridge](#chapter-40--nonreflexangle-bridge)
- [Chapter 41 — Slopetriangle Bridge](#chapter-41--slopetriangle-bridge)
- [Chapter 42 — Hatch Bridge](#chapter-42--hatch-bridge)

## Chapter 2 — Conventions

### 2.1 Type Legend

| Type | Meaning |
|---|---|
| `(extern)` | External JavaScript object/reference (typically used for input parameters). |
| `(extern/raw)` | Raw JavaScript return object/reference (no `null`/`undefined` mapping). |
| `(value)` | WebRacket value converted through the FFI value bridge. |
| `(string)` | JavaScript string mapped to WebRacket string. |
| `(i32)` | 32-bit integer. In this API it is often used as boolean flag (`0`/`1`). |
| `(f64)` | Double-precision floating-point number. |
| `()` | No arguments (input) or no value / void (output). |

### 2.2 Boolean and Numeric Conventions

- Several point flags are `(i32)` instead of `(boolean)`. Use `0` for false and `1` for true.
- Coordinates and distances are `(f64)`.
- `js-jsx-point-style` and infobox digit settings are `(i32)`.

### 2.3 Common Setup Helpers

```racket
(define board (js-eval "window.board"))
(define pt    (js-eval "window.pt"))
(define pt2   (js-eval "window.pt2"))
(define el1   (js-eval "window.el1"))
(define el2   (js-eval "window.el2"))
(define attrs  (js-object (vector)))
```

## Chapter 3 — Predicates

Reference root: [JXG.Point](https://jsxgraph.org/docs/symbols/JXG.Point.html)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-point?`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(boolean)` | `(js-jsx-point? pt)` | validate an external object is a JSXGraph point. |

## Chapter 4 — Point Properties (Getters)

Reference root: [JXG.Point](https://jsxgraph.org/docs/symbols/JXG.Point.html)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-point-attractor-distance`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(f64)` | `(js-jsx-point-attractor-distance pt)` | read the attractor capture distance. |
| [`js-jsx-point-attractors`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(extern/raw)` | `(js-jsx-point-attractors pt)` | inspect the point attractor list. |
| [`js-jsx-point-attractor-unit`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(string)` | `(js-jsx-point-attractor-unit pt)` | read the unit used for attractor distance. |
| [`js-jsx-point-attract-to-grid`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(i32)` | `(js-jsx-point-attract-to-grid pt)` | check whether attract-to-grid is enabled. |
| [`js-jsx-point-face`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(string)` | `(js-jsx-point-face pt)` | read the point marker face. |
| [`js-jsx-point-ignored-snap-to-points`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(extern/raw)` | `(js-jsx-point-ignored-snap-to-points pt)` | read the ignored points for snap-to-points. |
| [`js-jsx-point-infobox-digits`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(i32)` | `(js-jsx-point-infobox-digits pt)` | read the infobox precision setting. |
| [`js-jsx-point-show-infobox`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(i32)` | `(js-jsx-point-show-infobox pt)` | check whether the infobox display is enabled. |
| [`js-jsx-point-size`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(f64)` | `(js-jsx-point-size pt)` | read the point marker size. |
| [`js-jsx-point-size-unit`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(string)` | `(js-jsx-point-size-unit pt)` | read the unit for point size. |
| [`js-jsx-point-snap-size-x`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(f64)` | `(js-jsx-point-snap-size-x pt)` | read the x-axis snap step. |
| [`js-jsx-point-snap-size-y`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(f64)` | `(js-jsx-point-snap-size-y pt)` | read the y-axis snap step. |
| [`js-jsx-point-snap-to-grid`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(i32)` | `(js-jsx-point-snap-to-grid pt)` | check whether snap-to-grid is enabled. |
| [`js-jsx-point-snap-to-points`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(i32)` | `(js-jsx-point-snap-to-points pt)` | check whether snap-to-points is enabled. |
| [`js-jsx-point-snatch-distance`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(f64)` | `(js-jsx-point-snatch-distance pt)` | read the maximum snapping distance. |
| [`js-jsx-point-style`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(i32)` | `(js-jsx-point-style pt)` | read the numeric point style index. |
| [`js-jsx-point-x`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(f64)` | `(js-jsx-point-x pt)` | read the point x-coordinate. |
| [`js-jsx-point-y`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(f64)` | `(js-jsx-point-y pt)` | read the point y-coordinate. |
| [`js-jsx-point-zoom`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `(i32)` | `(js-jsx-point-zoom pt)` | check whether point size scales with zoom. |

## Chapter 5 — Point Properties (Setters)

Reference root: [JXG.Point](https://jsxgraph.org/docs/symbols/JXG.Point.html)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-set-point-attractor-distance!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern f64)` | `()` | `(js-jsx-set-point-attractor-distance! pt 12.0)` | set the attractor capture distance. |
| [`js-jsx-set-point-attractors!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern extern)` | `()` | `(js-jsx-set-point-attractors! pt attractors)` | replace the point attractor list. |
| [`js-jsx-set-point-attractor-unit!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern string)` | `()` | `(js-jsx-set-point-attractor-unit! pt "px")` | set the unit for attractor distance. |
| [`js-jsx-set-point-attract-to-grid!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(js-jsx-set-point-attract-to-grid! pt 1)` | enable or disable attract-to-grid. |
| [`js-jsx-set-point-face!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern string)` | `()` | `(js-jsx-set-point-face! pt "circle")` | change the marker face. |
| [`js-jsx-set-point-ignored-snap-to-points!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern extern)` | `()` | `(js-jsx-set-point-ignored-snap-to-points! pt points)` | set the points ignored during snap-to-points. |
| [`js-jsx-set-point-infobox-digits!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(js-jsx-set-point-infobox-digits! pt 3)` | set the infobox precision. |
| [`js-jsx-set-point-show-infobox!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(js-jsx-set-point-show-infobox! pt 1)` | show or hide the infobox. |
| [`js-jsx-set-point-size!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern f64)` | `()` | `(js-jsx-set-point-size! pt 4.0)` | set the point marker size. |
| [`js-jsx-set-point-size-unit!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern string)` | `()` | `(js-jsx-set-point-size-unit! pt "px")` | set the unit for point size. |
| [`js-jsx-set-point-snap-size-x!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern f64)` | `()` | `(js-jsx-set-point-snap-size-x! pt 0.5)` | set the x-axis snap step. |
| [`js-jsx-set-point-snap-size-y!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern f64)` | `()` | `(js-jsx-set-point-snap-size-y! pt 0.5)` | set the y-axis snap step. |
| [`js-jsx-set-point-snap-to-grid!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(js-jsx-set-point-snap-to-grid! pt 1)` | enable or disable snap-to-grid. |
| [`js-jsx-set-point-snap-to-points!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(js-jsx-set-point-snap-to-points! pt 1)` | enable or disable snap-to-points. |
| [`js-jsx-set-point-snatch-distance!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern f64)` | `()` | `(js-jsx-set-point-snatch-distance! pt 12.0)` | set the maximum snapping distance. |
| [`js-jsx-set-point-style!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(js-jsx-set-point-style! pt 2)` | set the numeric point style index. |
| [`js-jsx-set-point-zoom!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(js-jsx-set-point-zoom! pt 1)` | enable or disable zoom scaling of point size. |

## Chapter 6 — Point Methods

Reference root: [JXG.Point](https://jsxgraph.org/docs/symbols/JXG.Point.html)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-point-has-point`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern f64 f64)` | `(i32)` | `(js-jsx-point-has-point pt 100.0 120.0)` | hit-test screen coordinates against the point marker. |
| [`js-jsx-point-is-on`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern extern f64)` | `(i32)` | `(js-jsx-point-is-on pt el1 0.1)` | test whether a point lies on another element within tolerance. |
| [`js-jsx-point-make-intersection!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern extern extern i32 i32)` | `()` | `(js-jsx-point-make-intersection! pt el1 el2 0 0)` | turn a point into an intersection definition. |
| [`js-jsx-point-normalize-face`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern string)` | `(string)` | `(js-jsx-point-normalize-face pt "x")` | normalize or validate a face token. |
| [`js-jsx-point-set-style!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(js-jsx-point-set-style! pt 2)` | invoke the point style setter method directly. |
| [`js-jsx-point-update!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `()` | `(js-jsx-point-update! pt 1)` | recompute the point position or state. |
| [`js-jsx-point-update-renderer!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern)` | `()` | `(js-jsx-point-update-renderer! pt)` | trigger a renderer refresh for the point. |
| [`js-jsx-point-update-transform!`](https://jsxgraph.org/docs/symbols/JXG.Point.html) | `(extern i32)` | `(extern/raw)` | `(js-jsx-point-update-transform! pt 1)` | apply transformations and return the transformed base element. |

## Chapter 7 — Board API

Reference roots:
- [JXG.JSXGraph](https://jsxgraph.org/docs/symbols/JXG.JSXGraph.html)
- [JXG.Board](https://jsxgraph.org/docs/symbols/JXG.Board.html)

### 7.1 Construction

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-init-board`](https://jsxgraph.org/docs/symbols/JXG.JSXGraph.html) | `(string value)` | `(extern/raw)` | `(js-jsx-init-board "box" attrs)` | initialize a JSXGraph board in a DOM container. |
| [`js-jsx-board-create`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern string value value)` | `(extern/raw)` | `(js-jsx-board-create board "point" parents attrs)` | create a generic JSXGraph element on an existing board. |
| [`js-jsx-board-create-point`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-point board #[-3 1] attrs)` | create a point using the board helper. |
| [`js-jsx-board-create-line`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-line board #[p q] attrs)` | create a line from parent points or coordinates. |
| [`js-jsx-board-create-segment`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-segment board #[p q] attrs)` | create a finite segment from parent points or coordinates. |
| [`js-jsx-board-create-arc`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-arc board #[p q r] attrs)` | create an arc from parent points or coordinates. |
| [`js-jsx-board-create-angle`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-angle board #[p q r] attrs)` | create an angle from parent points or coordinates. |
| [`js-jsx-board-create-sector`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-sector board #[p q r] attrs)` | create a sector from parent points or coordinates. |
| [`js-jsx-board-create-glider`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-glider board #[p q] attrs)` | create a glider constrained to a parent. |
| [`js-jsx-board-create-circle`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-circle board #[p q] attrs)` | create a circle from parent geometry. |
| [`js-jsx-board-create-conic`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-conic board #[A B C D E] attrs)` | create a conic from parent points or coefficients. |
| [`js-jsx-board-create-ellipse`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-ellipse board #[f1 f2 p] attrs)` | create an ellipse from parent points or axis data. |
| [`js-jsx-board-create-curve`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-curve board #[f g] attrs)` | create a curve from parent data or functions. |
| [`js-jsx-board-create-functiongraph`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-functiongraph board #[f -10 10] attrs)` | create a function graph from a function and interval. |
| [`js-jsx-board-create-polygon`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-polygon board #[p q r] attrs)` | create a polygon from parent points or coordinate arrays. |
| [`js-jsx-board-create-midpoint`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-midpoint board #[p q] attrs)` | create a midpoint from parent points or a segment. |
| [`js-jsx-board-create-parallel`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-parallel board #[l p] attrs)` | create a parallel line from a reference line and point. |
| [`js-jsx-board-create-arrowparallel`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-arrowparallel board #[p q r] attrs)` | create an arrowparallel from three parent points. |
| [`js-jsx-board-create-axis`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-axis board #[p q] attrs)` | create an axis from parent points. |
| [`js-jsx-board-create-grid`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-grid board #[] attrs)` | create a grid on the board. |
| [`js-jsx-board-create-boxplot`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-boxplot board #[Q 2 4] attrs)` | create a boxplot from quantiles and placement data. |
| [`js-jsx-board-create-tangent`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-tangent board #[g1] attrs)` | create a tangent line from a parent curve or glider. |
| [`js-jsx-board-create-tangentto`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-tangentto board #[c p 0] attrs)` | create a tangent line from a conic/circle and an external point. |
| [`js-jsx-board-create-polarline`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-polarline board #[c p] attrs)` | create a polar line from a conic/circle and a point. |
| [`js-jsx-board-create-polepoint`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-polepoint board #[c l] attrs)` | create a pole point from a conic/circle and a line. |
| [`js-jsx-board-create-radicalaxis`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-radicalaxis board #[c1 c2] attrs)` | create a radical axis from two circles. |
| [`js-jsx-board-create-circumcircle`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-circumcircle board #[p1 p2 p3] attrs)` | create a circumcircle from three points. |
| [`js-jsx-board-create-incircle`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-incircle board #[p1 p2 p3] attrs)` | create an incircle from three points. |
| [`js-jsx-board-create-incenter`](https://jsxgraph.org/docs/symbols/Incenter.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-incenter board #[p1 p2 p3] attrs)` | create an incenter from three points. |
| [`js-jsx-board-create-minorarc`](https://jsxgraph.org/docs/symbols/MinorArc.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-minorarc board #[p1 p2 p3] attrs)` | create a minor arc from three points. |
| [`js-jsx-board-create-minorsector`](https://jsxgraph.org/docs/symbols/MinorSector.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-minorsector board #[p1 p2 p3] attrs)` | create a minor sector from three points. |
| [`js-jsx-board-create-nonreflexangle`](https://jsxgraph.org/docs/symbols/NonReflexAngle.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-nonreflexangle board #[p1 p2 p3] attrs)` | create a non-reflex angle from three points. |
| [`js-jsx-board-create-slopetriangle`](https://jsxgraph.org/docs/symbols/Slopetriangle.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-slopetriangle board #[line glider] attrs)` | create a slope triangle from a line or tangent. |
| [`js-jsx-board-create-circumcirclearc`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-circumcirclearc board #[p1 p2 p3] attrs)` | create a circumcircle arc from three points. |
| [`js-jsx-board-create-circumcirclesector`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-circumcirclesector board #[p1 p2 p3] attrs)` | create a circumcircle sector from three points. |
| [`js-jsx-board-create-semicircle`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-semicircle board #[p1 p2] attrs)` | create a semicircle from two points. |
| [`js-jsx-board-create-majorarc`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-majorarc board #[p1 p2 p3] attrs)` | create a major arc from three points. |
| [`js-jsx-board-create-majorsector`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-majorsector board #[p1 p2 p3] attrs)` | create a major sector from three points. |
| [`js-jsx-board-create-curveintersection`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-curveintersection board #[c1 c2] attrs)` | create a curve intersection from two closed paths. |
| [`js-jsx-board-create-curvedifference`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-curvedifference board #[c1 c2] attrs)` | create a curve difference from two closed paths. |
| [`js-jsx-board-create-curveunion`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-curveunion board #[c1 c2] attrs)` | create a curve union from two closed paths. |
| [`js-jsx-board-create-derivative`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-derivative board #[curve] attrs)` | create a derivative curve from a parent curve. |
| [`js-jsx-board-create-integral`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-integral board #[[-2 2] curve] attrs)` | create an integral visualization from an interval and curve. |
| [`js-jsx-board-create-riemannsum`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-riemannsum board #[f 8 "upper" -2 5] attrs)` | create a Riemann sum visualization from a function, bar count, and interval. |
| [`js-jsx-board-create-slopefield`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-slopefield board #[f xData yData] attrs)` | create a slope field from a defining function and mesh data. |
| [`js-jsx-board-create-vectorfield`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-vectorfield board #[f xData yData] attrs)` | create a vector field from a defining function and mesh data. |
| [`js-jsx-board-create-implicitcurve`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-implicitcurve board #[f] attrs)` | create an implicit curve from a two-variable equation. |
| [`js-jsx-board-create-spline`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-spline board #[p1 p2 p3 p4] attrs)` | create a spline interpolating sample points. |
| [`js-jsx-board-create-cardinalspline`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-cardinalspline board #[points tau "centripetal"] attrs)` | create a cardinal spline from sample points and tension parameters. |
| [`js-jsx-board-create-comb`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-comb board #[p1 p2] attrs)` | create a comb from two points. |
| [`js-jsx-board-create-metapostspline`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-metapostspline board #[points controls] attrs)` | create a metapost spline from sample points and control data. |
| [`js-jsx-board-create-polygonalchain`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-polygonalchain board #[points] attrs)` | create a polygonal chain from a point array. |
| [`js-jsx-board-create-regularpolygon`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-regularpolygon board #[p1 p2 5] attrs)` | create a regular polygon from two points and a vertex count. |
| [`js-jsx-board-create-hyperbola`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-hyperbola board #[A B C] attrs)` | create a hyperbola from three parent points. |
| [`js-jsx-board-create-parabola`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-parabola board #[C l] attrs)` | create a parabola from a focus point and a line. |
| [`js-jsx-board-create-stepfunction`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-stepfunction board #[xs ys] attrs)` | create a step function from sample points. |
| [`js-jsx-board-create-inequality`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-inequality board #[l] attrs)` | create an inequality shading from a line or function. |
| [`js-jsx-board-create-turtle`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-turtle board #[0 0] attrs)` | create a turtle drawing object. |
| [`js-jsx-board-create-perpendicular`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-perpendicular board #[l p] attrs)` | create a perpendicular line from a reference line and point. |
| [`js-jsx-board-create-reflection`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-reflection board #[obj axis] attrs)` | create a reflection of an object across a line or axis. |
| [`js-jsx-board-create-bisector`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-bisector board #[p q r] attrs)` | create an angle bisector from three parent points. |
| [`js-jsx-board-create-normal`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-normal board #[c p] attrs)` | create a normal line to a curve or circle. |
| [`js-jsx-board-create-intersection`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-intersection board #[l1 l2 0] attrs)` | create an intersection point from parent elements. |
| [`js-jsx-board-create-arrow`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-arrow board #[p q] attrs)` | create an arrow on the board. |
| [`js-jsx-board-create-button`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-button board #[-4 1 "Go"] attrs)` | create a button widget on the board. |
| [`js-jsx-board-create-checkbox`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-checkbox board #[0 3 "Check"] attrs)` | create a checkbox widget on the board. |
| [`js-jsx-board-create-input`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-input board #[0 1 "f(x)"] attrs)` | create an input widget on the board. |
| [`js-jsx-board-create-slider`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-slider board #[-4 0 4] attrs)` | create a slider widget on the board. |
| [`js-jsx-board-create-chart`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-chart board #[f] attrs)` | create a chart object on the board. |
| [`js-jsx-board-create-legend`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-legend board #[8 45] attrs)` | create a legend on the board. |
| [`js-jsx-board-create-smartlabel`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-smartlabel board #[p] attrs)` | create a smartlabel on the board. |
| [`js-jsx-board-create-text`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-text board #[-5 5 "A"] attrs)` | create a text label or annotation on the board. |
| [`js-jsx-board-create-image`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-image board #[-5 5 "url"] attrs)` | create an image object on the board. |
| [`js-jsx-board-create-foreignobject`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-foreignobject board #["<div>foo</div>" #[-5 5] #[140 60]] attrs)` | create a foreign object on the board. |
| [`js-jsx-board-create-tapemeasure`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-tapemeasure board #[#[-4 0] #[4 0]] attrs)` | create a tape measure on the board. |
| [`js-jsx-board-create-hatch`](https://jsxgraph.org/docs/symbols/Hatch.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-hatch board #[line 3] attrs)` | create a hatch object on a line or curve. |
| [`js-jsx-board-create-ticks`](https://jsxgraph.org/docs/symbols/Ticks.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-ticks board #[line [1 4 5]] attrs)` | create a ticks object on a line or axis. |
| [`js-jsx-board-create-transformation`](https://jsxgraph.org/docs/symbols/Transformation.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-transformation board #[2 0.5] attrs)` | create a transformation object on a board. |
| [`js-jsx-board-create-tracecurve`](https://jsxgraph.org/docs/symbols/Tracecurve.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-tracecurve board #[g1 p2] attrs)` | create a trace curve from a glider and a point. |
| [`js-jsx-board-create-parallelogram`](https://jsxgraph.org/docs/symbols/Parallelogram.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-parallelogram board #[p1 p2 p3] attrs)` | create a parallelogram from three points. |
| [`js-jsx-board-create-reflexangle`](https://jsxgraph.org/docs/symbols/ReflexAngle.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-reflexangle board #[p1 p2 p3] attrs)` | create a reflex angle from three points. |
| [`js-jsx-board-create-measurement`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-measurement board #[0 0 ["Radius" circle]] attrs)` | create a measurement on the board. |
| [`js-jsx-board-create-circumcenter`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-circumcenter board #[p1 p2 p3] attrs)` | create a circumcenter point on the board. |
| [`js-jsx-board-create-mirrorelement`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-mirrorelement board #[obj mirr] attrs)` | create a mirrored element on the board. |
| [`js-jsx-board-create-mirrorpoint`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-mirrorpoint board #[p1 p2] attrs)` | create a mirrored point on the board. |
| [`js-jsx-board-create-otherintersection`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-otherintersection board #[el1 el2 p] attrs)` | create the other intersection of two elements. |
| [`js-jsx-board-create-orthogonalprojection`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-orthogonalprojection board #[p l] attrs)` | create the orthogonal projection of a point onto a line. |
| [`js-jsx-board-create-parallelpoint`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-parallelpoint board #[p1 p2 p3] attrs)` | create a point parallel to a base vector. |
| [`js-jsx-board-create-perpendicularpoint`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-perpendicularpoint board #[p l] attrs)` | create a perpendicular projection point. |

### 7.2 Lifecycle and Updates

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-board-update!`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(extern/raw)` | `(js-jsx-board-update! board)` | update the board state and redraw as needed. |
| [`js-jsx-board-full-update!`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(extern/raw)` | `(js-jsx-board-full-update! board)` | force a full board update pass. |
| [`js-jsx-board-remove-object!`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern extern)` | `()` | `(js-jsx-board-remove-object! board obj)` | remove a previously created object from the board. |
| [`js-jsx-board-suspend-update!`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(extern/raw)` | `(js-jsx-board-suspend-update! board)` | suspend automatic redraws while batching changes. |
| [`js-jsx-board-unsuspend-update!`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(extern/raw)` | `(js-jsx-board-unsuspend-update! board)` | resume automatic redraws after batching changes. |

### 7.3 Board Properties

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-board-id`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(string)` | `(js-jsx-board-id board)` | read the board id. |
| [`js-jsx-board-container`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(extern/raw)` | `(js-jsx-board-container board)` | read the board container element. |
| [`js-jsx-board-renderer`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(extern/raw)` | `(js-jsx-board-renderer board)` | read the board renderer. |
| [`js-jsx-board-canvas-width`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(i32)` | `(js-jsx-board-canvas-width board)` | read the board canvas width. |
| [`js-jsx-board-canvas-height`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(i32)` | `(js-jsx-board-canvas-height board)` | read the board canvas height. |
| [`js-jsx-board-bounding-box`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(extern/raw)` | `(js-jsx-board-bounding-box board)` | read the board bounding box. |
| [`js-jsx-board-num-objects`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(i32)` | `(js-jsx-board-num-objects board)` | read the total number of objects ever created. |
| [`js-jsx-board-objects-list`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern)` | `(extern/raw)` | `(js-jsx-board-objects-list board)` | read the board objects in construction order. |

## Chapter 8 — GeometryElement Bridge

Reference root: [JXG.GeometryElement](https://jsxgraph.org/docs/symbols/JXG.GeometryElement.html)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-element-call`](https://jsxgraph.org/docs/symbols/JXG.GeometryElement.html) | `(extern string/symbol value)` | `(extern/raw)` | `(js-jsx-element-call element "setAttribute" args)` | call a GeometryElement method and keep the raw JavaScript result. |
| [`js-jsx-element-call/nullish`](https://jsxgraph.org/docs/symbols/JXG.GeometryElement.html) | `(extern string/symbol value)` | `(extern)` | `(js-jsx-element-call/nullish element "remove" args)` | call a GeometryElement method and treat nullish results as absence. |
| [`js-jsx-element-add-event`](https://jsxgraph.org/docs/symbols/JXG.GeometryElement.html) | `(extern string/symbol value)` | `(void)` | `(js-jsx-element-add-event element "up" handler)` | register a GeometryElement event handler via the JSXGraph `addEvent` alias. |

## Chapter 9 — Line Bridge

Reference root: [JXG.Line](https://jsxgraph.org/docs/symbols/JXG.Line.html)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-line-call`](https://jsxgraph.org/docs/symbols/JXG.Line.html) | `(extern string/symbol value)` | `(extern/raw)` | `(js-jsx-line-call line "getSlope" args)` | call a line method and keep the raw JavaScript result. |
| [`js-jsx-line-call/nullish`](https://jsxgraph.org/docs/symbols/JXG.Line.html) | `(extern string/symbol value)` | `(extern)` | `(js-jsx-line-call/nullish line "setFixedLength" args)` | call a line mutator and treat nullish results as absence. |

## Chapter 10 — Circle Bridge

Reference root: [JXG.Circle](https://jsxgraph.org/docs/symbols/JXG.Circle.html)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-circle-call`](https://jsxgraph.org/docs/symbols/JXG.Circle.html) | `(extern string/symbol value)` | `(extern/raw)` | `(js-jsx-circle-call circle "getRadius" args)` | call a circle method and keep the raw JavaScript result. |
| [`js-jsx-circle-call/nullish`](https://jsxgraph.org/docs/symbols/JXG.Circle.html) | `(extern string/symbol value)` | `(extern)` | `(js-jsx-circle-call/nullish circle "setRadius" args)` | call a circle mutator and treat nullish results as absence. |

## Chapter 11 — Curve Bridge

Reference root: [JXG.Curve](https://jsxgraph.org/docs/symbols/JXG.Curve.html)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-curve-call`](https://jsxgraph.org/docs/symbols/JXG.Curve.html) | `(extern string/symbol value)` | `(extern/raw)` | `(js-jsx-curve-call curve "updateCurve" args)` | call a curve method and keep the raw JavaScript result. |
| [`js-jsx-curve-call/nullish`](https://jsxgraph.org/docs/symbols/JXG.Curve.html) | `(extern string/symbol value)` | `(extern)` | `(js-jsx-curve-call/nullish curve "updateTransform" args)` | call a curve mutator and treat nullish results as absence. |

### Curve-family specifics

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-riemannsum-Value`](https://jsxgraph.org/docs/symbols/Riemannsum.html#Value) | `(extern)` | `(f64)` | `(js-jsx-riemannsum-Value rs)` | read the current Riemann sum value. |
| [`js-jsx-slopefield-setF`](https://jsxgraph.org/docs/symbols/Slopefield.html#setF) | `(extern value)` | `()` | `(js-jsx-slopefield-setF field f)` | update the defining function of a slope field. |
| [`js-jsx-vectorfield-setF`](https://jsxgraph.org/docs/symbols/Vectorfield.html#setF) | `(extern value)` | `()` | `(js-jsx-vectorfield-setF field f)` | update the defining function of a vector field. |

## Chapter 12 — Polygon Bridge

Reference root: [JXG.Polygon](https://jsxgraph.org/docs/symbols/JXG.Polygon.html)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-polygon-call`](https://jsxgraph.org/docs/symbols/JXG.Polygon.html) | `(extern string/symbol value)` | `(extern/raw)` | `(js-jsx-polygon-call polygon "Perimeter" args)` | call a polygon method and keep the raw JavaScript result. |
| [`js-jsx-polygon-call/nullish`](https://jsxgraph.org/docs/symbols/JXG.Polygon.html) | `(extern string/symbol value)` | `(extern)` | `(js-jsx-polygon-call/nullish polygon "updateRenderer" args)` | call a polygon mutator and treat nullish results as absence. |

## Chapter 13 — Arc Bridge

Reference root: [JXG.Arc](https://jsxgraph.org/docs/symbols/JXG.Arc.html)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-arc-get-radius`](https://jsxgraph.org/docs/symbols/JXG.Arc.html#getRadius) | `(extern)` | `(f64)` | `(js-jsx-arc-get-radius arc)` | read the deprecated radius getter. |
| [`js-jsx-arc-has-point-sector`](https://jsxgraph.org/docs/symbols/JXG.Arc.html#hasPointSector) | `(extern f64 f64)` | `(boolean)` | `(js-jsx-arc-has-point-sector arc 1.0 2.0)` | test whether a point lies in the arc sector. |
| [`js-jsx-arc-radius`](https://jsxgraph.org/docs/symbols/JXG.Arc.html#Radius) | `(extern)` | `(f64)` | `(js-jsx-arc-radius arc)` | read the current radius of the arc. |
| [`js-jsx-arc-value`](https://jsxgraph.org/docs/symbols/JXG.Arc.html#Value) | `(extern value value)` | `(f64)` | `(js-jsx-arc-value arc "length" #f)` | read the arc length or angle value. |

## Chapter 14 — Angle Bridge

Reference root: [JXG.Angle](https://jsxgraph.org/docs/symbols/JXG.Angle.html)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-angle-free`](https://jsxgraph.org/docs/symbols/JXG.Angle.html#free) | `(extern)` | `(boolean)` | `(js-jsx-angle-free angle)` | check whether the angle is free. |
| [`js-jsx-angle-set-angle!`](https://jsxgraph.org/docs/symbols/JXG.Angle.html#setAngle) | `(extern value)` | `()` | `(js-jsx-angle-set-angle! angle 1.57)` | set the angle value. |
| [`js-jsx-angle-value`](https://jsxgraph.org/docs/symbols/JXG.Angle.html#Value) | `(extern value)` | `(f64)` | `(js-jsx-angle-value angle "rad")` | read the current angle value. |

## Chapter 15 — Sector Bridge

Reference root: [JXG.Sector](https://jsxgraph.org/docs/symbols/JXG.Sector.html)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-sector-area`](https://jsxgraph.org/docs/symbols/JXG.Sector.html#Area) | `(extern)` | `(f64)` | `(js-jsx-sector-area sector)` | read the area of a sector. |
| [`js-jsx-sector-has-point-sector`](https://jsxgraph.org/docs/symbols/JXG.Sector.html#hasPointSector) | `(extern f64 f64)` | `(boolean)` | `(js-jsx-sector-has-point-sector sector 1.0 2.0)` | test whether a point lies inside the sector. |
| [`js-jsx-sector-l`](https://jsxgraph.org/docs/symbols/JXG.Sector.html#L) | `(extern)` | `(f64)` | `(js-jsx-sector-l sector)` | read the arc length of the sector. |
| [`js-jsx-sector-perimeter`](https://jsxgraph.org/docs/symbols/JXG.Sector.html#Perimeter) | `(extern)` | `(f64)` | `(js-jsx-sector-perimeter sector)` | read the perimeter of the sector. |
| [`js-jsx-sector-radius`](https://jsxgraph.org/docs/symbols/JXG.Sector.html#Radius) | `(extern)` | `(f64)` | `(js-jsx-sector-radius sector)` | read the sector radius. |
| [`js-jsx-sector-set-position-directly!`](https://jsxgraph.org/docs/symbols/JXG.Sector.html#setPositionDirectly) | `(extern value value value)` | `(extern/raw)` | `(js-jsx-sector-set-position-directly! sector method coords oldcoords)` | move the sector by direct coordinates. |
| [`js-jsx-sector-set-radius!`](https://jsxgraph.org/docs/symbols/JXG.Sector.html#setRadius) | `(extern value)` | `(extern/raw)` | `(js-jsx-sector-set-radius! sector value)` | set the sector radius. |

## Chapter 16 — Glider Bridge

Reference root: [JXG.Glider](https://jsxgraph.org/docs/symbols/JXG.Glider.html)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-glider-start-animation!`](https://jsxgraph.org/docs/symbols/JXG.Glider.html#startAnimation) | `(extern value value value)` | `()` | `(js-jsx-glider-start-animation! glider 1 60 10)` | start a glider animation. |
| [`js-jsx-glider-stop-animation!`](https://jsxgraph.org/docs/symbols/JXG.Glider.html#stopAnimation) | `(extern)` | `()` | `(js-jsx-glider-stop-animation! glider)` | stop a glider animation. |

## Chapter 17 — Text Bridge

The `Text` bridge covers the methods that JSXGraph documents on
`JXG.Text` itself. Inherited `GeometryElement` and `CoordsElement`
helpers are exposed elsewhere in this reference.

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-text-_createFctUpdateText`](https://jsxgraph.org/docs/symbols/JXG.Text.html#_createFctUpdateText) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-_createFctUpdateText text args)` | build the internal update function for a text element. |
| [`js-jsx-text-_setText`](https://jsxgraph.org/docs/symbols/JXG.Text.html#_setText) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-_setText text args)` | set the internal text content directly. |
| [`js-jsx-text-bounds`](https://jsxgraph.org/docs/symbols/JXG.Text.html#bounds) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-bounds text args)` | read the text bounds. |
| [`js-jsx-text-checkForSizeUpdate`](https://jsxgraph.org/docs/symbols/JXG.Text.html#checkForSizeUpdate) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-checkForSizeUpdate text args)` | check whether the text size needs recomputing. |
| [`js-jsx-text-convertGeonext2CSS`](https://jsxgraph.org/docs/symbols/JXG.Text.html#convertGeonext2CSS) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-convertGeonext2CSS text args)` | convert GEONExT markup to CSS. |
| [`js-jsx-text-convertGeonextAndSketchometry2CSS`](https://jsxgraph.org/docs/symbols/JXG.Text.html#convertGeonextAndSketchometry2CSS) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-convertGeonextAndSketchometry2CSS text args)` | convert GEONExT and Sketchometry markup to CSS. |
| [`js-jsx-text-convertSketchometry2CSS`](https://jsxgraph.org/docs/symbols/JXG.Text.html#convertSketchometry2CSS) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-convertSketchometry2CSS text args)` | convert Sketchometry markup to CSS. |
| [`js-jsx-text-crudeSizeEstimate`](https://jsxgraph.org/docs/symbols/JXG.Text.html#crudeSizeEstimate) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-crudeSizeEstimate text args)` | estimate the rendered size crudely. |
| [`js-jsx-text-escapeTicks`](https://jsxgraph.org/docs/symbols/JXG.Text.html#escapeTicks) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-escapeTicks text args)` | escape tick marks in text. |
| [`js-jsx-text-expandShortMath`](https://jsxgraph.org/docs/symbols/JXG.Text.html#expandShortMath) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-expandShortMath text args)` | expand short math notation. |
| [`js-jsx-text-generateTerm`](https://jsxgraph.org/docs/symbols/JXG.Text.html#generateTerm) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-generateTerm text args)` | generate the text term. |
| [`js-jsx-text-getAnchorX`](https://jsxgraph.org/docs/symbols/JXG.Text.html#getAnchorX) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-getAnchorX text args)` | read the X anchor position. |
| [`js-jsx-text-getAnchorY`](https://jsxgraph.org/docs/symbols/JXG.Text.html#getAnchorY) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-getAnchorY text args)` | read the Y anchor position. |
| [`js-jsx-text-getNumberOfConflicts`](https://jsxgraph.org/docs/symbols/JXG.Text.html#getNumberOfConflicts) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-getNumberOfConflicts text args)` | read the number of placement conflicts. |
| [`js-jsx-text-getSize`](https://jsxgraph.org/docs/symbols/JXG.Text.html#getSize) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-getSize text args)` | read the current text size. |
| [`js-jsx-text-hasPoint`](https://jsxgraph.org/docs/symbols/JXG.Text.html#hasPoint) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-hasPoint text args)` | test whether screen coordinates hit the text. |
| [`js-jsx-text-notifyParents`](https://jsxgraph.org/docs/symbols/JXG.Text.html#notifyParents) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-notifyParents text args)` | notify parent elements that the text changed. |
| [`js-jsx-text-poorMansTeX`](https://jsxgraph.org/docs/symbols/JXG.Text.html#poorMansTeX) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-poorMansTeX text args)` | render poor-man's TeX markup. |
| [`js-jsx-text-replaceSub`](https://jsxgraph.org/docs/symbols/JXG.Text.html#replaceSub) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-replaceSub text args)` | replace a subscript fragment. |
| [`js-jsx-text-replaceSup`](https://jsxgraph.org/docs/symbols/JXG.Text.html#replaceSup) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-replaceSup text args)` | replace a superscript fragment. |
| [`js-jsx-text-setAutoPosition`](https://jsxgraph.org/docs/symbols/JXG.Text.html#setAutoPosition) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-setAutoPosition text args)` | toggle automatic positioning. |
| [`js-jsx-text-setCoords`](https://jsxgraph.org/docs/symbols/JXG.Text.html#setCoords) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-setCoords text args)` | set the text coordinates. |
| [`js-jsx-text-setText`](https://jsxgraph.org/docs/symbols/JXG.Text.html#setText) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-setText text args)` | set the displayed text. |
| [`js-jsx-text-setTextJessieCode`](https://jsxgraph.org/docs/symbols/JXG.Text.html#setTextJessieCode) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-setTextJessieCode text args)` | set the text from JessieCode. |
| [`js-jsx-text-unescapeTicks`](https://jsxgraph.org/docs/symbols/JXG.Text.html#unescapeTicks) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-unescapeTicks text args)` | unescape tick marks in text. |
| [`js-jsx-text-updateSize`](https://jsxgraph.org/docs/symbols/JXG.Text.html#updateSize) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-updateSize text args)` | update the text size. |
| [`js-jsx-text-updateText`](https://jsxgraph.org/docs/symbols/JXG.Text.html#updateText) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-updateText text args)` | refresh the rendered text. |
| [`js-jsx-text-utf8_decode`](https://jsxgraph.org/docs/symbols/JXG.Text.html#utf8_decode) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-utf8_decode text args)` | decode UTF-8 text data. |
| [`js-jsx-text-valueTagToJessieCode`](https://jsxgraph.org/docs/symbols/JXG.Text.html#valueTagToJessieCode) | `(extern value)` | `(extern/raw)` | `(js-jsx-text-valueTagToJessieCode text args)` | convert a value tag to JessieCode. |

## Chapter 18 — Image Bridge

The `Image` bridge covers the documented `JXG.Image` methods that are
specific to image elements. The shared geometry and coordinate helpers
are already documented elsewhere in this reference.

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-image-H`](https://jsxgraph.org/docs/symbols/JXG.Image.html#H) | `(extern value)` | `(extern/raw)` | `(js-jsx-image-H image args)` | read the image H helper. |
| [`js-jsx-image-W`](https://jsxgraph.org/docs/symbols/JXG.Image.html#W) | `(extern value)` | `(extern/raw)` | `(js-jsx-image-W image args)` | read the image W helper. |
| [`js-jsx-image-has-point`](https://jsxgraph.org/docs/symbols/JXG.Image.html#hasPoint) | `(extern value)` | `(boolean)` | `(js-jsx-image-has-point image args)` | test whether a point hits the image. |
| [`js-jsx-image-set-size`](https://jsxgraph.org/docs/symbols/JXG.Image.html#setSize) | `(extern value)` | `(extern/raw)` | `(js-jsx-image-set-size image args)` | set the image size. |
| [`js-jsx-image-update`](https://jsxgraph.org/docs/symbols/JXG.Image.html#update) | `(extern value)` | `(extern/raw)` | `(js-jsx-image-update image args)` | update the image. |
| [`js-jsx-image-update-renderer`](https://jsxgraph.org/docs/symbols/JXG.Image.html#updateRenderer) | `(extern value)` | `(extern/raw)` | `(js-jsx-image-update-renderer image args)` | refresh the image renderer. |
| [`js-jsx-image-update-size`](https://jsxgraph.org/docs/symbols/JXG.Image.html#updateSize) | `(extern value)` | `(extern/raw)` | `(js-jsx-image-update-size image args)` | update the image size. |
| [`js-jsx-image-update-span`](https://jsxgraph.org/docs/symbols/JXG.Image.html#updateSpan) | `(extern value)` | `(extern/raw)` | `(js-jsx-image-update-span image args)` | update the image span. |

## Chapter 19 — Chart Bridge

The `Chart` bridge covers the documented chart draw and update methods.

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-chart-drawBar`](https://jsxgraph.org/docs/symbols/Chart.html#drawBar) | `(extern value)` | `(extern/raw)` | `(js-jsx-chart-drawBar chart '#[])` | draw a bar chart rendering. |
| [`js-jsx-chart-drawFit`](https://jsxgraph.org/docs/symbols/Chart.html#drawFit) | `(extern value)` | `(extern/raw)` | `(js-jsx-chart-drawFit chart '#[])` | draw a fit chart rendering. |
| [`js-jsx-chart-drawLine`](https://jsxgraph.org/docs/symbols/Chart.html#drawLine) | `(extern value)` | `(extern/raw)` | `(js-jsx-chart-drawLine chart '#[])` | draw a line chart rendering. |
| [`js-jsx-chart-drawPie`](https://jsxgraph.org/docs/symbols/Chart.html#drawPie) | `(extern value)` | `(extern/raw)` | `(js-jsx-chart-drawPie chart '#[])` | draw a pie chart rendering. |
| [`js-jsx-chart-drawPoints`](https://jsxgraph.org/docs/symbols/Chart.html#drawPoints) | `(extern value)` | `(extern/raw)` | `(js-jsx-chart-drawPoints chart '#[])` | draw a point chart rendering. |
| [`js-jsx-chart-drawRadar`](https://jsxgraph.org/docs/symbols/Chart.html#drawRadar) | `(extern value)` | `(extern/raw)` | `(js-jsx-chart-drawRadar chart '#[])` | draw a radar chart rendering. |
| [`js-jsx-chart-drawSpline`](https://jsxgraph.org/docs/symbols/Chart.html#drawSpline) | `(extern value)` | `(extern/raw)` | `(js-jsx-chart-drawSpline chart '#[])` | draw a spline chart rendering. |
| [`js-jsx-chart-updateDataArray`](https://jsxgraph.org/docs/symbols/Chart.html#updateDataArray) | `(extern value)` | `(extern/raw)` | `(js-jsx-chart-updateDataArray chart '#[])` | refresh the chart data array. |
| [`js-jsx-chart-updateRenderer`](https://jsxgraph.org/docs/symbols/Chart.html#updateRenderer) | `(extern value)` | `(extern/raw)` | `(js-jsx-chart-updateRenderer chart '#[])` | refresh the chart renderer. |

## Chapter 20 — Checkbox Bridge

The `Checkbox` bridge covers the documented `JXG.Checkbox` value
accessor.

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-checkbox-Value`](https://jsxgraph.org/docs/symbols/Checkbox.html#Value) | `(extern)` | `(boolean)` | `(js-jsx-checkbox-Value checkbox)` | read the checkbox value. |

## Chapter 21 — Input Bridge

The `Input` bridge covers the documented `JXG.Input` value and setter
methods.

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-input-set`](https://jsxgraph.org/docs/symbols/Input.html#set) | `(extern value)` | `(extern/raw)` | `(js-jsx-input-set input "g(x)")` | set the current input value. |
| [`js-jsx-input-Value`](https://jsxgraph.org/docs/symbols/Input.html#Value) | `(extern)` | `(string)` | `(js-jsx-input-Value input)` | read the input content. |

## Chapter 22 — Slider Bridge

The `Slider` bridge covers the documented `JXG.Slider` methods that
control the slider range and value.

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-slider-setMax`](https://jsxgraph.org/docs/symbols/Slider.html#setMax) | `(extern value)` | `(extern/raw)` | `(js-jsx-slider-setMax slider 10)` | set the slider maximum. |
| [`js-jsx-slider-setMin`](https://jsxgraph.org/docs/symbols/Slider.html#setMin) | `(extern value)` | `(extern/raw)` | `(js-jsx-slider-setMin slider 0)` | set the slider minimum. |
| [`js-jsx-slider-setValue`](https://jsxgraph.org/docs/symbols/Slider.html#setValue) | `(extern value)` | `(extern/raw)` | `(js-jsx-slider-setValue slider 5)` | set the current slider value. |
| [`js-jsx-slider-Value`](https://jsxgraph.org/docs/symbols/Slider.html#Value) | `(extern)` | `(f64)` | `(js-jsx-slider-Value slider)` | read the current slider value. |

## Chapter 23 — Mini Workflows

### Configure Point Snapping

```racket
(js-jsx-set-point-snap-to-grid! pt 1)
(js-jsx-set-point-snap-size-x! pt 0.25)
(js-jsx-set-point-snap-size-y! pt 0.25)
(js-jsx-set-point-snatch-distance! pt 6.0)
```

### Hit-Testing and Projection

```racket
(define hits? (js-jsx-point-has-point pt 120.0 80.0))
(define on-curve? (js-jsx-point-is-on pt el1 0.05))
(list hits? on-curve?)
```

### Style and Renderer Refresh

```racket
(js-jsx-set-point-face! pt "cross")
(js-jsx-set-point-size! pt 5.0)
(js-jsx-point-update! pt 1)
(js-jsx-point-update-renderer! pt)
```

### Minimal Geometry Constructors

```racket
(define p (js-jsx-board-create-point board #[-3 1] (js-object (vector (vector "name" "P")))))
(define q (js-jsx-board-create-point board #[2 2] (js-object (vector (vector "name" "Q")))))
(define l (js-jsx-board-create-line board #[p q] (js-object (vector))))
(define s (js-jsx-board-create-segment board #[p q] (js-object (vector))))
(define c (js-jsx-board-create-circle board #[p q] (js-object (vector))))
(define t (js-jsx-board-create-text board #[-5 5 "PQ"] (js-object (vector))))
```

## Chapter 24 — Coverage Checklist

- This document covers **214** functions from `ffi/jsxgraph.ffi`.
- Total documented functions: **224**
- `board api`: 77 functions
- `board properties`: 8 functions
- `geometryelement bridge`: 3 functions
- `line bridge`: 2 functions
- `arc bridge`: 4 functions
- `angle bridge`: 3 functions
- `sector bridge`: 7 functions
- `glider bridge`: 2 functions
- `circle bridge`: 2 functions
- `curve bridge`: 5 functions
- `polygon bridge`: 2 functions
- `chart bridge`: 9 functions
- `foreignobject bridge`: 8 functions
- `tapemeasure bridge`: 1 function
- `checkbox bridge`: 1 function
- `input bridge`: 2 functions
- `slider bridge`: 5 functions
- `predicates`: 1 function
- `point getters`: 19 functions
- `point setters`: 17 functions
- `point methods`: 8 functions

## Chapter 25 — ForeignObject Bridge

The `ForeignObject` bridge covers the JSXGraph methods specific to
foreign object elements.

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-foreignobject-H`](https://jsxgraph.org/docs/symbols/ForeignObject.html#H) | `(extern)` | `(extern/raw)` | `(js-jsx-foreignobject-H foreignobject)` | read the foreign object H helper. |
| [`js-jsx-foreignobject-W`](https://jsxgraph.org/docs/symbols/ForeignObject.html#W) | `(extern)` | `(extern/raw)` | `(js-jsx-foreignobject-W foreignobject)` | read the foreign object W helper. |
| [`js-jsx-foreignobject-has-point`](https://jsxgraph.org/docs/symbols/ForeignObject.html#hasPoint) | `(extern value value)` | `(boolean)` | `(js-jsx-foreignobject-has-point foreignobject 10 10)` | test whether a point hits the foreign object. |
| [`js-jsx-foreignobject-set-size`](https://jsxgraph.org/docs/symbols/ForeignObject.html#setSize) | `(extern value)` | `(extern/raw)` | `(js-jsx-foreignobject-set-size foreignobject size)` | set the foreign object size. |
| [`js-jsx-foreignobject-update`](https://jsxgraph.org/docs/symbols/ForeignObject.html#update) | `(extern)` | `(extern/raw)` | `(js-jsx-foreignobject-update foreignobject)` | update the foreign object. |
| [`js-jsx-foreignobject-update-renderer`](https://jsxgraph.org/docs/symbols/ForeignObject.html#updateRenderer) | `(extern)` | `(extern/raw)` | `(js-jsx-foreignobject-update-renderer foreignobject)` | refresh the foreign object renderer. |
| [`js-jsx-foreignobject-update-size`](https://jsxgraph.org/docs/symbols/ForeignObject.html#updateSize) | `(extern)` | `(extern/raw)` | `(js-jsx-foreignobject-update-size foreignobject)` | update the foreign object size. |
| [`js-jsx-foreignobject-update-span`](https://jsxgraph.org/docs/symbols/ForeignObject.html#updateSpan) | `(extern)` | `(extern/raw)` | `(js-jsx-foreignobject-update-span foreignobject)` | update the foreign object span. |

## Chapter 26 — Tapemeasure Bridge

The `Tapemeasure` bridge currently exposes the tape measure value
helper.

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-jsx-tapemeasure-Value`](https://jsxgraph.org/docs/symbols/Tapemeasure.html#Value) | `(extern)` | `(f64)` | `(js-jsx-tapemeasure-Value tapemeasure)` | read the tape measure value. |

## Chapter 27 — Measurement Bridge

The `Measurement` wrapper is constructor-only in this batch. Use
`js-jsx-board-create-measurement` or `jsx-create-measurement` to add a
measurement board element.

## Chapter 28 — Circumcenter Bridge

The `Circumcenter` wrapper is constructor-only in this batch. Use
`js-jsx-board-create-circumcenter` or `jsx-create-circumcenter` to add
the circumcenter point to a triangle construction.

## Chapter 29 — MirrorElement Bridge

The `MirrorElement` wrapper is constructor-only in this batch. Use
`js-jsx-board-create-mirrorelement` or `jsx-create-mirrorelement` to
add a mirrored object to a board.

## Chapter 30 — MirrorPoint Bridge

The `MirrorPoint` wrapper is constructor-only in this batch. Use
`js-jsx-board-create-mirrorpoint` or `jsx-create-mirrorpoint` to add a
mirrored point to a board.

## Chapter 31 — OtherIntersection Bridge

The `OtherIntersection` wrapper is constructor-only in this batch. Use
`js-jsx-board-create-otherintersection` or `jsx-create-otherintersection`
to add the other intersection of two elements to a board.

## Chapter 32 — Orthogonalprojection Bridge

The `Orthogonalprojection` wrapper is constructor-only in this batch.
Use `js-jsx-board-create-orthogonalprojection` or
`jsx-create-orthogonalprojection` to add the orthogonal projection of a
point onto a line.

## Chapter 33 — Parallelpoint Bridge

The `Parallelpoint` wrapper is constructor-only in this batch. Use
`js-jsx-board-create-parallelpoint` or `jsx-create-parallelpoint` to
add a point parallel to a base vector.

## Chapter 34 — PerpendicularPoint Bridge

The `PerpendicularPoint` wrapper is constructor-only in this batch.
Use `js-jsx-board-create-perpendicularpoint` or
`jsx-create-perpendicularpoint` to add the perpendicular projection of
a point onto a line.

## Chapter 35 — Bisectorlines Bridge

The `Bisectorlines` wrapper is constructor-only in this batch. Use
`js-jsx-board-create-bisectorlines` or `jsx-create-bisectorlines` to
add the bisector between two lines.

## Chapter 36 — PerpendicularSegment Bridge

The `PerpendicularSegment` wrapper is constructor-only in this batch.
Use `js-jsx-board-create-perpendicularsegment` or
`jsx-create-perpendicularsegment` to add the perpendicular segment
from a point to a line.

## Chapter 37 — Incenter Bridge

The `Incenter` wrapper is constructor-only in this batch. Use
`js-jsx-board-create-incenter` or `jsx-create-incenter` to add the
incenter of a triangle.

## Chapter 38 — MinorArc Bridge

The `MinorArc` wrapper is constructor-only in this batch. Use
`js-jsx-board-create-minorarc` or `jsx-create-minorarc` to add a minor
arc from three points.

## Chapter 39 — MinorSector Bridge

The `MinorSector` wrapper is constructor-only in this batch. Use
`js-jsx-board-create-minorsector` or `jsx-create-minorsector` to add a
minor sector from three points.

## Chapter 40 — NonReflexAngle Bridge

The `NonReflexAngle` wrapper is constructor-only in this batch. Use
`js-jsx-board-create-nonreflexangle` or `jsx-create-nonreflexangle` to
add a non-reflex angle from three points.

## Chapter 41 — Slopetriangle Bridge

The `Slopetriangle` wrapper is constructor-only in this batch. Use
`js-jsx-board-create-slopetriangle` or `jsx-create-slopetriangle` to
add a slope triangle from a line or tangent.

## Chapter 42 — Hatch Bridge

The `Hatch` wrapper is constructor-only in this batch. Use
`js-jsx-board-create-hatch` or `jsx-create-hatch` to add hatch marks
to a line or curve.
