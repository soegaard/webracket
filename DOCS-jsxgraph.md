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
- Line, arc, angle, sector, glider, circle, curve, polygon, midpoint, parallel,
  perpendicular, reflection, bisector, and normal bridge helpers

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
- [Chapter 18 — Mini Workflows](#chapter-18--mini-workflows)
- [Configure Point Snapping](#configure-point-snapping)
- [Hit-Testing and Projection](#hit-testing-and-projection)
- [Style and Renderer Refresh](#style-and-renderer-refresh)
- [Minimal Geometry Constructors](#minimal-geometry-constructors)
- [Chapter 19 — Coverage Checklist](#chapter-19--coverage-checklist)

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
| [`js-jsx-board-create-curve`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-curve board #[f g] attrs)` | create a curve from parent data or functions. |
| [`js-jsx-board-create-polygon`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-polygon board #[p q r] attrs)` | create a polygon from parent points or coordinate arrays. |
| [`js-jsx-board-create-midpoint`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-midpoint board #[p q] attrs)` | create a midpoint from parent points or a segment. |
| [`js-jsx-board-create-parallel`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-parallel board #[l p] attrs)` | create a parallel line from a reference line and point. |
| [`js-jsx-board-create-perpendicular`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-perpendicular board #[l p] attrs)` | create a perpendicular line from a reference line and point. |
| [`js-jsx-board-create-reflection`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-reflection board #[obj axis] attrs)` | create a reflection of an object across a line or axis. |
| [`js-jsx-board-create-bisector`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-bisector board #[p q r] attrs)` | create an angle bisector from three parent points. |
| [`js-jsx-board-create-normal`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-normal board #[c p] attrs)` | create a normal line to a curve or circle. |
| [`js-jsx-board-create-intersection`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-intersection board #[l1 l2 0] attrs)` | create an intersection point from parent elements. |
| [`js-jsx-board-create-text`](https://jsxgraph.org/docs/symbols/JXG.Board.html) | `(extern value value)` | `(extern/raw)` | `(js-jsx-board-create-text board #[-5 5 "A"] attrs)` | create a text label or annotation on the board. |

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

## Chapter 18 — Mini Workflows

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

## Chapter 19 — Coverage Checklist

- This document covers **134** functions from `ffi/jsxgraph.ffi`.
- Total documented functions: **134**
- `board api`: 24 functions
- `board properties`: 8 functions
- `geometryelement bridge`: 3 functions
- `line bridge`: 2 functions
- `arc bridge`: 4 functions
- `angle bridge`: 3 functions
- `sector bridge`: 7 functions
- `glider bridge`: 2 functions
- `circle bridge`: 2 functions
- `curve bridge`: 3 functions
- `polygon bridge`: 2 functions
- `predicates`: 1 function
- `point getters`: 19 functions
- `point setters`: 17 functions
- `point methods`: 8 functions
