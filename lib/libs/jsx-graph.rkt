#lang webracket

(include-lib element)

;;;
;;; JSXGraph helpers
;;;

;; This library provides a small Rackety layer on top of the low-level
;; `js-jsx-*` FFI bindings. The public `jsx-*` names are intended for
;; everyday JSXGraph work: creating boards, building geometry objects,
;; and querying or updating point state.

;;; -------------------------------------------------------------------
;;; Checked wrappers
;;; -------------------------------------------------------------------

;; jsx-board : external/raw -> jsx-board?
;;   Wrap a JSXGraph board object.
(struct jsx-board (raw) #:transparent)
(define jsx-board-wrapper? jsx-board?)

;; jsx-element : external/raw -> jsx-element?
;;   Wrap a generic JSXGraph element.
(struct jsx-element (raw) #:transparent)
(define jsx-element-wrapper? jsx-element?)

;; jsx-point : external/raw -> jsx-point?
;;   Wrap a JSXGraph point.
(struct jsx-point (raw) #:transparent)
(define jsx-point-wrapper? jsx-point?)

;; jsx-wrap-board : external/raw -> jsx-board?
;;   Wrap a raw board value.
(define (jsx-wrap-board raw)
  (jsx-board raw))

;; jsx-wrap-element : external/raw -> jsx-element?
;;   Wrap a raw generic element value.
(define (jsx-wrap-element raw)
  (jsx-element raw))

;; jsx-wrap-point : external/raw -> jsx-point?
;;   Wrap a raw point value.
(define (jsx-wrap-point raw)
  (jsx-point raw))

;; jsx-unwrap : any/c -> any/c
;;   Extract the raw browser value from a checked wrapper.
(define (jsx-unwrap value)
  (cond
    [(jsx-board-wrapper? value)   (jsx-board-raw value)]
    [(jsx-element-wrapper? value) (jsx-element-raw value)]
    [(jsx-point-wrapper? value)   (jsx-point-raw value)]
    [else                 value]))

;; jsx-unpack-array : any/c -> any/c
;;   Recursively unwrap checked JSXGraph values inside array-like data.
(define (jsx-unpack-array value)
  (cond
    [(vector? value)
     (for/vector #:length (vector-length value)
         ([item (in-vector value)])
       (jsx-unpack-array item))]
    [(list? value)
     (list->vector (map jsx-unpack-array value))]
    [else
     (jsx-unwrap value)]))

;; jsx-functiongraph-parents : any/c -> any/c
;;   Convert functiongraph parents into raw browser values.
(define (jsx-functiongraph-parents value)
  (cond
    [(procedure? value)
     (procedure->external
      (lambda args
        (cond
          [(null? args)
           (error 'jsx-create-functiongraph
                  "functiongraph callback expects at least one argument")]
          [else
           (value (car args))])))]
    [(vector? value)
     (for/vector #:length (vector-length value)
         ([item (in-vector value)])
       (jsx-functiongraph-parents item))]
    [(list? value)
     (list->vector (map jsx-functiongraph-parents value))]
    [else
     (jsx-unwrap value)]))

;; jsx-callable-pack : any/c -> any/c
;;   Convert nested procedures into raw browser callbacks.
(define (jsx-callable-pack value)
  (cond
    [(procedure? value) (procedure->external value)]
    [(vector? value)
     (for/vector #:length (vector-length value)
         ([item (in-vector value)])
       (jsx-callable-pack item))]
    [(list? value)
     (list->vector (map jsx-callable-pack value))]
    [else
     (jsx-unwrap value)]))

;; jsx-curve-parents : any/c -> any/c
;;   Convert curve parents into raw browser values.
(define (jsx-curve-parents value)
  (cond
    [(procedure? value)
     (procedure->external
      (lambda args
        (cond
          [(null? args)
           (error 'jsx-create-curve
                  "curve callback expects at least one argument")]
          [else
           (value (car args))])))]
    [(vector? value)
     (for/vector #:length (vector-length value)
         ([item (in-vector value)])
       (jsx-curve-parents item))]
    [(list? value)
     (list->vector (map jsx-curve-parents value))]
    [else
     (jsx-unwrap value)]))

;; jsx-element-call : any/c string? vector? -> any/c
;;   Call a GeometryElement method on a wrapped element.
(define (jsx-element-call element method args)
  (js-jsx-element-call (jsx-unwrap element) method args))

;; jsx-element-call/nullish : any/c string? vector? -> void?
;;   Call a GeometryElement mutator on a wrapped element.
(define (jsx-element-call/nullish element method args)
  (js-jsx-element-call/nullish (jsx-unwrap element) method args)
  (void))

;; jsx-line-call : any/c string? vector? -> any/c
;;   Call a Line method on a wrapped line element.
(define (jsx-line-call line method args)
  (js-jsx-line-call (jsx-unwrap line) method args))

;; jsx-line-call/nullish : any/c string? vector? -> void?
;;   Call a Line mutator on a wrapped line element.
(define (jsx-line-call/nullish line method args)
  (js-jsx-line-call/nullish (jsx-unwrap line) method args)
  (void))

;; jsx-circle-call : any/c string? vector? -> any/c
;;   Call a Circle method on a wrapped circle element.
(define (jsx-circle-call circle method args)
  (js-jsx-circle-call (jsx-unwrap circle) method args))

;; jsx-circle-call/nullish : any/c string? vector? -> void?
;;   Call a Circle mutator on a wrapped circle element.
(define (jsx-circle-call/nullish circle method args)
  (js-jsx-circle-call/nullish (jsx-unwrap circle) method args)
  (void))

;; jsx-arc-get-radius : any/c -> f64
;;   Read the deprecated arc radius getter.
(define (jsx-arc-get-radius arc)
  (js-jsx-arc-get-radius (jsx-unwrap arc)))

;; jsx-arc-has-point-sector? : any/c f64 f64 -> boolean?
;;   Check whether a point lies inside the arc sector.
(define (jsx-arc-has-point-sector? arc x y)
  (js-jsx-arc-has-point-sector (jsx-unwrap arc) x y))

;; jsx-arc-radius : any/c -> f64
;;   Read the current arc radius.
(define (jsx-arc-radius arc)
  (js-jsx-arc-radius (jsx-unwrap arc)))

;; jsx-arc-value : any/c any/c any/c -> f64
;;   Read the arc length or angle value.
(define (jsx-arc-value arc unit rad)
  (js-jsx-arc-value (jsx-unwrap arc) unit rad))

;; jsx-angle-free? : any/c -> boolean?
;;   Check whether the angle is free.
(define (jsx-angle-free? angle)
  (js-jsx-angle-free (jsx-unwrap angle)))

;; jsx-angle-set-angle! : any/c any/c -> void?
;;   Set the angle value.
(define (jsx-angle-set-angle! angle val)
  (js-jsx-angle-set-angle! (jsx-unwrap angle) val))

;; jsx-angle-value : any/c any/c -> f64
;;   Read the angle value in the requested unit.
(define (jsx-angle-value angle unit)
  (js-jsx-angle-value (jsx-unwrap angle) unit))

;; jsx-sector-area : any/c -> f64
;;   Read the sector area.
(define (jsx-sector-area sector)
  (js-jsx-sector-area (jsx-unwrap sector)))

;; jsx-sector-has-point-sector? : any/c f64 f64 -> boolean?
;;   Check whether a point lies inside the sector.
(define (jsx-sector-has-point-sector? sector x y)
  (js-jsx-sector-has-point-sector (jsx-unwrap sector) x y))

;; jsx-sector-l : any/c -> f64
;;   Read the sector arc length.
(define (jsx-sector-l sector)
  (js-jsx-sector-l (jsx-unwrap sector)))

;; jsx-sector-perimeter : any/c -> f64
;;   Read the sector perimeter.
(define (jsx-sector-perimeter sector)
  (js-jsx-sector-perimeter (jsx-unwrap sector)))

;; jsx-sector-radius : any/c -> f64
;;   Read the sector radius.
(define (jsx-sector-radius sector)
  (js-jsx-sector-radius (jsx-unwrap sector)))

;; jsx-sector-set-position-directly! : any/c any/c any/c any/c -> any/c
;;   Move the sector by direct coordinates.
(define (jsx-sector-set-position-directly! sector method coords oldcoords)
  (js-jsx-sector-set-position-directly! (jsx-unwrap sector) method coords oldcoords))

;; jsx-sector-set-radius! : any/c any/c -> any/c
;;   Set the sector radius.
(define (jsx-sector-set-radius! sector value)
  (js-jsx-sector-set-radius! (jsx-unwrap sector) value))

;; jsx-glider-start-animation! : any/c any/c any/c any/c -> void?
;;   Start glider animation.
(define (jsx-glider-start-animation! glider direction step-count delay)
  (js-jsx-glider-start-animation! (jsx-unwrap glider) direction step-count delay))

;; jsx-glider-stop-animation! : any/c -> void?
;;   Stop glider animation.
(define (jsx-glider-stop-animation! glider)
  (js-jsx-glider-stop-animation! (jsx-unwrap glider)))

;; jsx-curve-call : any/c string? vector? -> any/c
;;   Call a Curve method on a wrapped curve element.
(define (jsx-curve-call curve method args)
  (js-jsx-curve-call (jsx-unwrap curve) method args))

;; jsx-curve-call/nullish : any/c string? vector? -> void?
;;   Call a Curve mutator on a wrapped curve element.
(define (jsx-curve-call/nullish curve method args)
  (js-jsx-curve-call/nullish (jsx-unwrap curve) method args)
  (void))

;; jsx-riemannsum-value/raw : external/raw -> f64
;;   Read the value of a riemann sum.
(define (jsx-riemannsum-value/raw riemannsum)
  (js-jsx-riemannsum-Value riemannsum))

;; jsx-slopefield-set-f!/raw : external/raw any/c -> void?
;;   Set the slope field function.
(define (jsx-slopefield-set-f!/raw field func)
  (js-jsx-slopefield-setF field func))

;; jsx-vectorfield-set-f!/raw : external/raw any/c -> void?
;;   Set the vector field function.
(define (jsx-vectorfield-set-f!/raw field func)
  (js-jsx-vectorfield-setF field func))

;; jsx-polygon-call : any/c string? vector? -> any/c
;;   Call a Polygon method on a wrapped polygon element.
(define (jsx-polygon-call polygon method args)
  (js-jsx-polygon-call (jsx-unwrap polygon) method args))

;; jsx-polygon-call/nullish : any/c string? vector? -> void?
;;   Call a Polygon mutator on a wrapped polygon element.
(define (jsx-polygon-call/nullish polygon method args)
  (js-jsx-polygon-call/nullish (jsx-unwrap polygon) method args)
  (void))

;;; -------------------------------------------------------------------
;;; Low-level aliases
;;; -------------------------------------------------------------------

;; define-jsx-alias : (name arg ...) ffi -> syntax
;;   Define a wrapper that forwards directly to a foreign binding.
(define-syntax-rule (define-jsx-alias (name arg ...) ffi)
  (define (name arg ...) (ffi arg ...)))

;; jsx-init-board/raw : string? any/c -> external/raw
;;   Create a JSXGraph board with explicit attributes.
(define-jsx-alias (jsx-init-board/raw container-id attrs) js-jsx-init-board)

;; jsx-board-create/raw : external/raw string? any/c any/c -> external/raw
;;   Create an arbitrary JSXGraph element on a board.
(define-jsx-alias (jsx-board-create/raw board element-type parents attrs)
  js-jsx-board-create)

;; jsx-board-create-point/raw : external/raw any/c any/c -> external/raw
;;   Create a point on a board.
(define-jsx-alias (jsx-board-create-point/raw board parents attrs)
  js-jsx-board-create-point)

;; jsx-board-create-line/raw : external/raw any/c any/c -> external/raw
;;   Create a line on a board.
(define-jsx-alias (jsx-board-create-line/raw board parents attrs)
  js-jsx-board-create-line)

;; jsx-board-create-segment/raw : external/raw any/c any/c -> external/raw
;;   Create a segment on a board.
(define-jsx-alias (jsx-board-create-segment/raw board parents attrs)
  js-jsx-board-create-segment)

;; jsx-board-create-arc/raw : external/raw any/c any/c -> external/raw
;;   Create an arc on a board.
(define-jsx-alias (jsx-board-create-arc/raw board parents attrs)
  js-jsx-board-create-arc)

;; jsx-board-create-angle/raw : external/raw any/c any/c -> external/raw
;;   Create an angle on a board.
(define-jsx-alias (jsx-board-create-angle/raw board parents attrs)
  js-jsx-board-create-angle)

;; jsx-board-create-sector/raw : external/raw any/c any/c -> external/raw
;;   Create a sector on a board.
(define-jsx-alias (jsx-board-create-sector/raw board parents attrs)
  js-jsx-board-create-sector)

;; jsx-board-create-glider/raw : external/raw any/c any/c -> external/raw
;;   Create a glider on a board.
(define-jsx-alias (jsx-board-create-glider/raw board parents attrs)
  js-jsx-board-create-glider)

;; jsx-board-create-circle/raw : external/raw any/c any/c -> external/raw
;;   Create a circle on a board.
(define-jsx-alias (jsx-board-create-circle/raw board parents attrs)
  js-jsx-board-create-circle)

;; jsx-board-create-conic/raw : external/raw any/c any/c -> external/raw
;;   Create a conic on a board.
(define-jsx-alias (jsx-board-create-conic/raw board parents attrs)
  js-jsx-board-create-conic)

;; jsx-board-create-ellipse/raw : external/raw any/c any/c -> external/raw
;;   Create an ellipse on a board.
(define-jsx-alias (jsx-board-create-ellipse/raw board parents attrs)
  js-jsx-board-create-ellipse)

;; jsx-board-create-curve/raw : external/raw any/c any/c -> external/raw
;;   Create a curve on a board.
(define-jsx-alias (jsx-board-create-curve/raw board parents attrs)
  js-jsx-board-create-curve)

;; jsx-board-create-functiongraph/raw : external/raw any/c any/c -> external/raw
;;   Create a functiongraph on a board.
(define-jsx-alias (jsx-board-create-functiongraph/raw board parents attrs)
  js-jsx-board-create-functiongraph)

;; jsx-board-create-polygon/raw : external/raw any/c any/c -> external/raw
;;   Create a polygon on a board.
(define-jsx-alias (jsx-board-create-polygon/raw board parents attrs)
  js-jsx-board-create-polygon)

;; jsx-board-create-midpoint/raw : external/raw any/c any/c -> external/raw
;;   Create a midpoint on a board.
(define-jsx-alias (jsx-board-create-midpoint/raw board parents attrs)
  js-jsx-board-create-midpoint)

;; jsx-board-create-parallel/raw : external/raw any/c any/c -> external/raw
;;   Create a parallel line on a board.
(define-jsx-alias (jsx-board-create-parallel/raw board parents attrs)
  js-jsx-board-create-parallel)

;; jsx-board-create-arrowparallel/raw : external/raw any/c any/c -> external/raw
;;   Create an arrowparallel on a board.
(define-jsx-alias (jsx-board-create-arrowparallel/raw board parents attrs)
  js-jsx-board-create-arrowparallel)

;; jsx-board-create-axis/raw : external/raw any/c any/c -> external/raw
;;   Create an axis on a board.
(define-jsx-alias (jsx-board-create-axis/raw board parents attrs)
  js-jsx-board-create-axis)

;; jsx-board-create-grid/raw : external/raw any/c any/c -> external/raw
;;   Create a grid on a board.
(define-jsx-alias (jsx-board-create-grid/raw board parents attrs)
  js-jsx-board-create-grid)

;; jsx-board-create-boxplot/raw : external/raw any/c any/c -> external/raw
;;   Create a boxplot on a board.
(define-jsx-alias (jsx-board-create-boxplot/raw board parents attrs)
  js-jsx-board-create-boxplot)

;; jsx-board-create-tangent/raw : external/raw any/c any/c -> external/raw
;;   Create a tangent on a board.
(define-jsx-alias (jsx-board-create-tangent/raw board parents attrs)
  js-jsx-board-create-tangent)

;; jsx-board-create-tangentto/raw : external/raw any/c any/c -> external/raw
;;   Create a tangentto on a board.
(define-jsx-alias (jsx-board-create-tangentto/raw board parents attrs)
  js-jsx-board-create-tangentto)

;; jsx-board-create-polarline/raw : external/raw any/c any/c -> external/raw
;;   Create a polarline on a board.
(define-jsx-alias (jsx-board-create-polarline/raw board parents attrs)
  js-jsx-board-create-polarline)

;; jsx-board-create-polepoint/raw : external/raw any/c any/c -> external/raw
;;   Create a polepoint on a board.
(define-jsx-alias (jsx-board-create-polepoint/raw board parents attrs)
  js-jsx-board-create-polepoint)

;; jsx-board-create-radicalaxis/raw : external/raw any/c any/c -> external/raw
;;   Create a radicalaxis on a board.
(define-jsx-alias (jsx-board-create-radicalaxis/raw board parents attrs)
  js-jsx-board-create-radicalaxis)

;; jsx-board-create-circumcircle/raw : external/raw any/c any/c -> external/raw
;;   Create a circumcircle on a board.
(define-jsx-alias (jsx-board-create-circumcircle/raw board parents attrs)
  js-jsx-board-create-circumcircle)

;; jsx-board-create-incircle/raw : external/raw any/c any/c -> external/raw
;;   Create an incircle on a board.
(define-jsx-alias (jsx-board-create-incircle/raw board parents attrs)
  js-jsx-board-create-incircle)

;; jsx-board-create-circumcirclearc/raw : external/raw any/c any/c -> external/raw
;;   Create a circumcirclearc on a board.
(define-jsx-alias (jsx-board-create-circumcirclearc/raw board parents attrs)
  js-jsx-board-create-circumcirclearc)

;; jsx-board-create-circumcirclesector/raw : external/raw any/c any/c -> external/raw
;;   Create a circumcirclesector on a board.
(define-jsx-alias (jsx-board-create-circumcirclesector/raw board parents attrs)
  js-jsx-board-create-circumcirclesector)

;; jsx-board-create-semicircle/raw : external/raw any/c any/c -> external/raw
;;   Create a semicircle on a board.
(define-jsx-alias (jsx-board-create-semicircle/raw board parents attrs)
  js-jsx-board-create-semicircle)

;; jsx-board-create-majorarc/raw : external/raw any/c any/c -> external/raw
;;   Create a majorarc on a board.
(define-jsx-alias (jsx-board-create-majorarc/raw board parents attrs)
  js-jsx-board-create-majorarc)

;; jsx-board-create-majorsector/raw : external/raw any/c any/c -> external/raw
;;   Create a majorsector on a board.
(define-jsx-alias (jsx-board-create-majorsector/raw board parents attrs)
  js-jsx-board-create-majorsector)

;; jsx-board-create-curveintersection/raw : external/raw any/c any/c -> external/raw
;;   Create a curveintersection on a board.
(define-jsx-alias (jsx-board-create-curveintersection/raw board parents attrs)
  js-jsx-board-create-curveintersection)

;; jsx-board-create-curvedifference/raw : external/raw any/c any/c -> external/raw
;;   Create a curvedifference on a board.
(define-jsx-alias (jsx-board-create-curvedifference/raw board parents attrs)
  js-jsx-board-create-curvedifference)

;; jsx-board-create-curveunion/raw : external/raw any/c any/c -> external/raw
;;   Create a curveunion on a board.
(define-jsx-alias (jsx-board-create-curveunion/raw board parents attrs)
  js-jsx-board-create-curveunion)

;; jsx-board-create-derivative/raw : external/raw any/c any/c -> external/raw
;;   Create a derivative on a board.
(define-jsx-alias (jsx-board-create-derivative/raw board parents attrs)
  js-jsx-board-create-derivative)

;; jsx-board-create-integral/raw : external/raw any/c any/c -> external/raw
;;   Create an integral on a board.
(define-jsx-alias (jsx-board-create-integral/raw board parents attrs)
  js-jsx-board-create-integral)

;; jsx-board-create-riemannsum/raw : external/raw any/c any/c -> external/raw
;;   Create a riemannsum on a board.
(define-jsx-alias (jsx-board-create-riemannsum/raw board parents attrs)
  js-jsx-board-create-riemannsum)

;; jsx-board-create-slopefield/raw : external/raw any/c any/c -> external/raw
;;   Create a slopefield on a board.
(define-jsx-alias (jsx-board-create-slopefield/raw board parents attrs)
  js-jsx-board-create-slopefield)

;; jsx-board-create-vectorfield/raw : external/raw any/c any/c -> external/raw
;;   Create a vectorfield on a board.
(define-jsx-alias (jsx-board-create-vectorfield/raw board parents attrs)
  js-jsx-board-create-vectorfield)

;; jsx-board-create-implicitcurve/raw : external/raw any/c any/c -> external/raw
;;   Create an implicitcurve on a board.
(define-jsx-alias (jsx-board-create-implicitcurve/raw board parents attrs)
  js-jsx-board-create-implicitcurve)

;; jsx-board-create-spline/raw : external/raw any/c any/c -> external/raw
;;   Create a spline on a board.
(define-jsx-alias (jsx-board-create-spline/raw board parents attrs)
  js-jsx-board-create-spline)

;; jsx-board-create-cardinalspline/raw : external/raw any/c any/c -> external/raw
;;   Create a cardinalspline on a board.
(define-jsx-alias (jsx-board-create-cardinalspline/raw board parents attrs)
  js-jsx-board-create-cardinalspline)

;; jsx-board-create-comb/raw : external/raw any/c any/c -> external/raw
;;   Create a comb on a board.
(define-jsx-alias (jsx-board-create-comb/raw board parents attrs)
  js-jsx-board-create-comb)

;; jsx-board-create-metapostspline/raw : external/raw any/c any/c -> external/raw
;;   Create a metapostspline on a board.
(define-jsx-alias (jsx-board-create-metapostspline/raw board parents attrs)
  js-jsx-board-create-metapostspline)

;; jsx-board-create-polygonalchain/raw : external/raw any/c any/c -> external/raw
;;   Create a polygonalchain on a board.
(define-jsx-alias (jsx-board-create-polygonalchain/raw board parents attrs)
  js-jsx-board-create-polygonalchain)

;; jsx-board-create-regularpolygon/raw : external/raw any/c any/c -> external/raw
;;   Create a regularpolygon on a board.
(define-jsx-alias (jsx-board-create-regularpolygon/raw board parents attrs)
  js-jsx-board-create-regularpolygon)

;; jsx-board-create-hyperbola/raw : external/raw any/c any/c -> external/raw
;;   Create a hyperbola on a board.
(define-jsx-alias (jsx-board-create-hyperbola/raw board parents attrs)
  js-jsx-board-create-hyperbola)

;; jsx-board-create-parabola/raw : external/raw any/c any/c -> external/raw
;;   Create a parabola on a board.
(define-jsx-alias (jsx-board-create-parabola/raw board parents attrs)
  js-jsx-board-create-parabola)

;; jsx-board-create-stepfunction/raw : external/raw any/c any/c -> external/raw
;;   Create a stepfunction on a board.
(define-jsx-alias (jsx-board-create-stepfunction/raw board parents attrs)
  js-jsx-board-create-stepfunction)

;; jsx-board-create-inequality/raw : external/raw any/c any/c -> external/raw
;;   Create an inequality on a board.
(define-jsx-alias (jsx-board-create-inequality/raw board parents attrs)
  js-jsx-board-create-inequality)

;; jsx-board-create-turtle/raw : external/raw any/c any/c -> external/raw
;;   Create a turtle on a board.
(define-jsx-alias (jsx-board-create-turtle/raw board parents attrs)
  js-jsx-board-create-turtle)

;; jsx-board-create-curveintersection/raw : external/raw any/c any/c -> external/raw
;;   Create a curveintersection on a board.
(define-jsx-alias (jsx-board-create-curveintersection/raw board parents attrs)
  js-jsx-board-create-curveintersection)

;; jsx-board-create-curvedifference/raw : external/raw any/c any/c -> external/raw
;;   Create a curvedifference on a board.
(define-jsx-alias (jsx-board-create-curvedifference/raw board parents attrs)
  js-jsx-board-create-curvedifference)

;; jsx-board-create-curveunion/raw : external/raw any/c any/c -> external/raw
;;   Create a curveunion on a board.
(define-jsx-alias (jsx-board-create-curveunion/raw board parents attrs)
  js-jsx-board-create-curveunion)

;; jsx-board-create-derivative/raw : external/raw any/c any/c -> external/raw
;;   Create a derivative on a board.
(define-jsx-alias (jsx-board-create-derivative/raw board parents attrs)
  js-jsx-board-create-derivative)

;; jsx-board-create-integral/raw : external/raw any/c any/c -> external/raw
;;   Create an integral on a board.
(define-jsx-alias (jsx-board-create-integral/raw board parents attrs)
  js-jsx-board-create-integral)

;; jsx-board-create-perpendicular/raw : external/raw any/c any/c -> external/raw
;;   Create a perpendicular line on a board.
(define-jsx-alias (jsx-board-create-perpendicular/raw board parents attrs)
  js-jsx-board-create-perpendicular)

;; jsx-board-create-reflection/raw : external/raw any/c any/c -> external/raw
;;   Create a reflection on a board.
(define-jsx-alias (jsx-board-create-reflection/raw board parents attrs)
  js-jsx-board-create-reflection)

;; jsx-board-create-bisector/raw : external/raw any/c any/c -> external/raw
;;   Create a bisector on a board.
(define-jsx-alias (jsx-board-create-bisector/raw board parents attrs)
  js-jsx-board-create-bisector)

;; jsx-board-create-normal/raw : external/raw any/c any/c -> external/raw
;;   Create a normal line on a board.
(define-jsx-alias (jsx-board-create-normal/raw board parents attrs)
  js-jsx-board-create-normal)

;; jsx-board-create-intersection/raw : external/raw any/c any/c -> external/raw
;;   Create an intersection point on a board.
(define-jsx-alias (jsx-board-create-intersection/raw board parents attrs)
  js-jsx-board-create-intersection)

;; jsx-board-create-arrow/raw : external/raw any/c any/c -> external/raw
;;   Create an arrow on a board.
(define-jsx-alias (jsx-board-create-arrow/raw board parents attrs)
  js-jsx-board-create-arrow)

;; jsx-board-create-button/raw : external/raw any/c any/c -> external/raw
;;   Create a button on a board.
(define-jsx-alias (jsx-board-create-button/raw board parents attrs)
  js-jsx-board-create-button)

;; jsx-board-create-checkbox/raw : external/raw any/c any/c -> external/raw
;;   Create a checkbox on a board.
(define-jsx-alias (jsx-board-create-checkbox/raw board parents attrs)
  js-jsx-board-create-checkbox)

;; jsx-board-create-input/raw : external/raw any/c any/c -> external/raw
;;   Create an input on a board.
(define-jsx-alias (jsx-board-create-input/raw board parents attrs)
  js-jsx-board-create-input)

;; jsx-board-create-slider/raw : external/raw any/c any/c -> external/raw
;;   Create a slider on a board.
(define-jsx-alias (jsx-board-create-slider/raw board parents attrs)
  js-jsx-board-create-slider)

;; jsx-board-create-chart/raw : external/raw any/c any/c -> external/raw
;;   Create a chart on a board.
(define-jsx-alias (jsx-board-create-chart/raw board parents attrs)
  js-jsx-board-create-chart)

;; jsx-board-create-legend/raw : external/raw any/c any/c -> external/raw
;;   Create a legend on a board.
(define-jsx-alias (jsx-board-create-legend/raw board parents attrs)
  js-jsx-board-create-legend)

;; jsx-board-create-smartlabel/raw : external/raw any/c any/c -> external/raw
;;   Create a smartlabel on a board.
(define-jsx-alias (jsx-board-create-smartlabel/raw board parents attrs)
  js-jsx-board-create-smartlabel)

;; jsx-board-create-foreignobject/raw : external/raw any/c any/c -> external/raw
;;   Create a foreignobject on a board.
(define-jsx-alias (jsx-board-create-foreignobject/raw board parents attrs)
  js-jsx-board-create-foreignobject)

;; jsx-board-create-tapemeasure/raw : external/raw any/c any/c -> external/raw
;;   Create a tapemeasure on a board.
(define-jsx-alias (jsx-board-create-tapemeasure/raw board parents attrs)
  js-jsx-board-create-tapemeasure)

;; jsx-board-create-ticks/raw : external/raw any/c any/c -> external/raw
;;   Create a ticks object on a board.
(define-jsx-alias (jsx-board-create-ticks/raw board parents attrs)
  js-jsx-board-create-ticks)

;; jsx-board-create-transformation/raw : external/raw any/c any/c -> external/raw
;;   Create a transformation object on a board.
(define-jsx-alias (jsx-board-create-transformation/raw board parents attrs)
  js-jsx-board-create-transformation)

;; jsx-board-create-tracecurve/raw : external/raw any/c any/c -> external/raw
;;   Create a tracecurve object on a board.
(define-jsx-alias (jsx-board-create-tracecurve/raw board parents attrs)
  js-jsx-board-create-tracecurve)

;; jsx-board-create-parallelogram/raw : external/raw any/c any/c -> external/raw
;;   Create a parallelogram object on a board.
(define-jsx-alias (jsx-board-create-parallelogram/raw board parents attrs)
  js-jsx-board-create-parallelogram)

;; jsx-board-create-reflexangle/raw : external/raw any/c any/c -> external/raw
;;   Create a reflexangle object on a board.
(define-jsx-alias (jsx-board-create-reflexangle/raw board parents attrs)
  js-jsx-board-create-reflexangle)

;; jsx-board-create-measurement/raw : external/raw any/c any/c -> external/raw
;;   Create a measurement on a board.
(define-jsx-alias (jsx-board-create-measurement/raw board parents attrs)
  js-jsx-board-create-measurement)

;; jsx-board-create-circumcenter/raw : external/raw any/c any/c -> external/raw
;;   Create a circumcenter on a board.
(define-jsx-alias (jsx-board-create-circumcenter/raw board parents attrs)
  js-jsx-board-create-circumcenter)

;; jsx-board-create-mirrorelement/raw : external/raw any/c any/c -> external/raw
;;   Create a mirrorelement on a board.
(define-jsx-alias (jsx-board-create-mirrorelement/raw board parents attrs)
  js-jsx-board-create-mirrorelement)

;; jsx-board-create-mirrorpoint/raw : external/raw any/c any/c -> external/raw
;;   Create a mirror point on a board.
(define-jsx-alias (jsx-board-create-mirrorpoint/raw board parents attrs)
  js-jsx-board-create-mirrorpoint)

;; jsx-board-create-otherintersection/raw : external/raw any/c any/c -> external/raw
;;   Create an other intersection on a board.
(define-jsx-alias (jsx-board-create-otherintersection/raw board parents attrs)
  js-jsx-board-create-otherintersection)

;; jsx-board-create-orthogonalprojection/raw : external/raw any/c any/c -> external/raw
;;   Create an orthogonal projection on a board.
(define-jsx-alias (jsx-board-create-orthogonalprojection/raw board parents attrs)
  js-jsx-board-create-orthogonalprojection)

;; jsx-board-create-parallelpoint/raw : external/raw any/c any/c -> external/raw
;;   Create a parallel point on a board.
(define-jsx-alias (jsx-board-create-parallelpoint/raw board parents attrs)
  js-jsx-board-create-parallelpoint)

;; jsx-board-create-perpendicularpoint/raw : external/raw any/c any/c -> external/raw
;;   Create a perpendicular point on a board.
(define-jsx-alias (jsx-board-create-perpendicularpoint/raw board parents attrs)
  js-jsx-board-create-perpendicularpoint)

;; jsx-board-create-text/raw : external/raw any/c any/c -> external/raw
;;   Create a text element on a board.
(define-jsx-alias (jsx-board-create-text/raw board parents attrs)
  js-jsx-board-create-text)

;; jsx-board-create-image/raw : external/raw any/c any/c -> external/raw
;;   Create an image element on a board.
(define-jsx-alias (jsx-board-create-image/raw board parents attrs)
  js-jsx-board-create-image)

;; jsx-board-create-group/raw : external/raw any/c any/c -> external/raw
;;   Create a group on a board.
(define-jsx-alias (jsx-board-create-group/raw board parents attrs)
  js-jsx-board-create-group)

;; jsx-text-_createFctUpdateText/raw : external/raw any/c -> external/raw
;;   Create the update function for text rendering.
(define-jsx-alias (jsx-text-_createFctUpdateText/raw text args)
  js-jsx-text-_createFctUpdateText)

;; jsx-text-_setText/raw : external/raw any/c -> external/raw
;;   Set text internals directly.
(define-jsx-alias (jsx-text-_setText/raw text args)
  js-jsx-text-_setText)

;; jsx-text-bounds/raw : external/raw any/c -> external/raw
;;   Read the text bounding box.
(define-jsx-alias (jsx-text-bounds/raw text args)
  js-jsx-text-bounds)

;; jsx-text-checkForSizeUpdate/raw : external/raw any/c -> external/raw
;;   Check whether the text size needs an update.
(define-jsx-alias (jsx-text-checkForSizeUpdate/raw text args)
  js-jsx-text-check-for-size-update)

;; jsx-text-convertGeonext2CSS/raw : external/raw any/c -> external/raw
;;   Convert GEONExT markup to CSS.
(define-jsx-alias (jsx-text-convertGeonext2CSS/raw text args)
  js-jsx-text-convert-geonext2-css)

;; jsx-text-convertGeonextAndSketchometry2CSS/raw : external/raw any/c -> external/raw
;;   Convert GEONExT and Sketchometry markup to CSS.
(define-jsx-alias (jsx-text-convertGeonextAndSketchometry2CSS/raw text args)
  js-jsx-text-convert-geonext-and-sketchometry2-css)

;; jsx-text-convertSketchometry2CSS/raw : external/raw any/c -> external/raw
;;   Convert Sketchometry markup to CSS.
(define-jsx-alias (jsx-text-convertSketchometry2CSS/raw text args)
  js-jsx-text-convert-sketchometry2-css)

;; jsx-text-crudeSizeEstimate/raw : external/raw any/c -> external/raw
;;   Estimate the text size crudely.
(define-jsx-alias (jsx-text-crudeSizeEstimate/raw text args)
  js-jsx-text-crude-size-estimate)

;; jsx-text-escapeTicks/raw : external/raw any/c -> external/raw
;;   Escape tick marks in text.
(define-jsx-alias (jsx-text-escapeTicks/raw text args)
  js-jsx-text-escape-ticks)

;; jsx-text-expandShortMath/raw : external/raw any/c -> external/raw
;;   Expand short math notation in text.
(define-jsx-alias (jsx-text-expandShortMath/raw text args)
  js-jsx-text-expand-short-math)

;; jsx-text-generateTerm/raw : external/raw any/c -> external/raw
;;   Generate the text term.
(define-jsx-alias (jsx-text-generateTerm/raw text args)
  js-jsx-text-generate-term)

;; jsx-text-getAnchorX/raw : external/raw any/c -> external/raw
;;   Read the X anchor position for text.
(define-jsx-alias (jsx-text-getAnchorX/raw text args)
  js-jsx-text-get-anchor-x)

;; jsx-text-getAnchorY/raw : external/raw any/c -> external/raw
;;   Read the Y anchor position for text.
(define-jsx-alias (jsx-text-getAnchorY/raw text args)
  js-jsx-text-get-anchor-y)

;; jsx-text-getNumberOfConflicts/raw : external/raw any/c -> external/raw
;;   Read the number of text placement conflicts.
(define-jsx-alias (jsx-text-getNumberOfConflicts/raw text args)
  js-jsx-text-get-number-of-conflicts)

;; jsx-text-getSize/raw : external/raw any/c -> external/raw
;;   Read the text size.
(define-jsx-alias (jsx-text-getSize/raw text args)
  js-jsx-text-get-size)

;; jsx-text-hasPoint/raw : external/raw any/c -> external/raw
;;   Test whether a screen position hits the text.
(define-jsx-alias (jsx-text-hasPoint/raw text args)
  js-jsx-text-has-point)

;; jsx-text-notifyParents/raw : external/raw any/c -> external/raw
;;   Notify parent elements that the text changed.
(define-jsx-alias (jsx-text-notifyParents/raw text args)
  js-jsx-text-notify-parents)

;; jsx-text-poorMansTeX/raw : external/raw any/c -> external/raw
;;   Render poor-man's TeX markup.
(define-jsx-alias (jsx-text-poorMansTeX/raw text args)
  js-jsx-text-poor-mans-tex)

;; jsx-text-replaceSub/raw : external/raw any/c -> external/raw
;;   Replace a subscript fragment.
(define-jsx-alias (jsx-text-replaceSub/raw text args)
  js-jsx-text-replace-sub)

;; jsx-text-replaceSup/raw : external/raw any/c -> external/raw
;;   Replace a superscript fragment.
(define-jsx-alias (jsx-text-replaceSup/raw text args)
  js-jsx-text-replace-sup)

;; jsx-text-setAutoPosition/raw : external/raw any/c -> external/raw
;;   Toggle automatic positioning.
(define-jsx-alias (jsx-text-setAutoPosition/raw text args)
  js-jsx-text-set-auto-position)

;; jsx-text-setCoords/raw : external/raw any/c -> external/raw
;;   Set the text coordinates.
(define-jsx-alias (jsx-text-setCoords/raw text args)
  js-jsx-text-set-coords)

;; jsx-text-setText/raw : external/raw any/c -> external/raw
;;   Set the displayed text.
(define-jsx-alias (jsx-text-setText/raw text args)
  js-jsx-text-set-text)

;; jsx-text-setTextJessieCode/raw : external/raw any/c -> external/raw
;;   Set the text from JessieCode.
(define-jsx-alias (jsx-text-setTextJessieCode/raw text args)
  js-jsx-text-set-text-jessie-code)

;; jsx-text-unescapeTicks/raw : external/raw any/c -> external/raw
;;   Unescape tick marks in text.
(define-jsx-alias (jsx-text-unescapeTicks/raw text args)
  js-jsx-text-unescape-ticks)

;; jsx-text-updateSize/raw : external/raw any/c -> external/raw
;;   Update the text size.
(define-jsx-alias (jsx-text-updateSize/raw text args)
  js-jsx-text-update-size)

;; jsx-text-updateText/raw : external/raw any/c -> external/raw
;;   Refresh the rendered text.
(define-jsx-alias (jsx-text-updateText/raw text args)
  js-jsx-text-update-text)

;; jsx-text-utf8_decode/raw : external/raw any/c -> external/raw
;;   Decode UTF-8 text data.
(define-jsx-alias (jsx-text-utf8_decode/raw text args)
  js-jsx-text-utf8-decode)

;; jsx-text-valueTagToJessieCode/raw : external/raw any/c -> external/raw
;;   Convert a value tag to JessieCode.
(define-jsx-alias (jsx-text-valueTagToJessieCode/raw text args)
  js-jsx-text-value-tag-to-jessie-code)

;; jsx-board-update!/raw : external/raw -> external/raw
;;   Update a board and redraw as needed.
(define-jsx-alias (jsx-board-update!/raw board) js-jsx-board-update!)

;; jsx-board-full-update!/raw : external/raw -> external/raw
;;   Force a full board update.
(define-jsx-alias (jsx-board-full-update!/raw board) js-jsx-board-full-update!)

;; jsx-board-remove-object!/raw : external/raw external/raw -> void?
;;   Remove an object from a board.
(define-jsx-alias (jsx-board-remove-object!/raw board object)
  js-jsx-board-remove-object!)

;; jsx-board-suspend-update!/raw : external/raw -> external/raw
;;   Suspend automatic board updates.
(define-jsx-alias (jsx-board-suspend-update!/raw board)
  js-jsx-board-suspend-update!)

;; jsx-board-unsuspend-update!/raw : external/raw -> external/raw
;;   Resume automatic board updates.
(define-jsx-alias (jsx-board-unsuspend-update!/raw board)
  js-jsx-board-unsuspend-update!)

;; jsx-point?/raw : any/c -> boolean?
;;   Test whether a value is a JSXGraph point.
(define-jsx-alias (jsx-point?/raw x) js-jsx-point?)

;; jsx-point-attractor-distance/raw : external/raw -> flonum?
;;   Read the point's attractor distance.
(define-jsx-alias (jsx-point-attractor-distance/raw point)
  js-jsx-point-attractor-distance)

;; jsx-set-point-attractor-distance!/raw : external/raw flonum? -> void?
;;   Set the point's attractor distance.
(define-jsx-alias (jsx-set-point-attractor-distance!/raw point distance)
  js-jsx-set-point-attractor-distance!)

;; jsx-point-attractors/raw : external/raw -> external/raw
;;   Read the point's attractors.
(define-jsx-alias (jsx-point-attractors/raw point) js-jsx-point-attractors)

;; jsx-set-point-attractors!/raw : external/raw external/raw -> void?
;;   Set the point's attractors.
(define-jsx-alias (jsx-set-point-attractors!/raw point attractors)
  js-jsx-set-point-attractors!)

;; jsx-point-attractor-unit/raw : external/raw -> string?
;;   Read the point's attractor unit.
(define-jsx-alias (jsx-point-attractor-unit/raw point) js-jsx-point-attractor-unit)

;; jsx-set-point-attractor-unit!/raw : external/raw string? -> void?
;;   Set the point's attractor unit.
(define-jsx-alias (jsx-set-point-attractor-unit!/raw point unit)
  js-jsx-set-point-attractor-unit!)

;; jsx-point-attract-to-grid/raw : external/raw -> boolean?
;;   Read whether the point attracts to grid.
(define-jsx-alias (jsx-point-attract-to-grid/raw point)
  js-jsx-point-attract-to-grid)

;; jsx-set-point-attract-to-grid!/raw : external/raw boolean? -> void?
;;   Enable or disable grid attraction.
(define-jsx-alias (jsx-set-point-attract-to-grid!/raw point enable)
  js-jsx-set-point-attract-to-grid!)

;; jsx-point-face/raw : external/raw -> string?
;;   Read the point face.
(define-jsx-alias (jsx-point-face/raw point) js-jsx-point-face)

;; jsx-set-point-face!/raw : external/raw string? -> void?
;;   Set the point face.
(define-jsx-alias (jsx-set-point-face!/raw point face) js-jsx-set-point-face!)

;; jsx-point-ignored-snap-to-points/raw : external/raw -> external/raw
;;   Read the point's ignored snap-to points.
(define-jsx-alias (jsx-point-ignored-snap-to-points/raw point)
  js-jsx-point-ignored-snap-to-points)

;; jsx-set-point-ignored-snap-to-points!/raw : external/raw external/raw -> void?
;;   Set the point's ignored snap-to points.
(define-jsx-alias (jsx-set-point-ignored-snap-to-points!/raw point points)
  js-jsx-set-point-ignored-snap-to-points!)

;; jsx-point-infobox-digits/raw : external/raw -> exact-integer?
;;   Read the number of infobox digits.
(define-jsx-alias (jsx-point-infobox-digits/raw point) js-jsx-point-infobox-digits)

;; jsx-set-point-infobox-digits!/raw : external/raw exact-integer? -> void?
;;   Set the number of infobox digits.
(define-jsx-alias (jsx-set-point-infobox-digits!/raw point digits)
  js-jsx-set-point-infobox-digits!)

;; jsx-point-show-infobox/raw : external/raw -> boolean?
;;   Read whether the infobox is shown.
(define-jsx-alias (jsx-point-show-infobox/raw point) js-jsx-point-show-infobox)

;; jsx-set-point-show-infobox!/raw : external/raw boolean? -> void?
;;   Enable or disable the point infobox.
(define-jsx-alias (jsx-set-point-show-infobox!/raw point show?)
  js-jsx-set-point-show-infobox!)

;; jsx-point-size/raw : external/raw -> flonum?
;;   Read the point size.
(define-jsx-alias (jsx-point-size/raw point) js-jsx-point-size)

;; jsx-set-point-size!/raw : external/raw flonum? -> void?
;;   Set the point size.
(define-jsx-alias (jsx-set-point-size!/raw point size) js-jsx-set-point-size!)

;; jsx-point-size-unit/raw : external/raw -> string?
;;   Read the point size unit.
(define-jsx-alias (jsx-point-size-unit/raw point) js-jsx-point-size-unit)

;; jsx-set-point-size-unit!/raw : external/raw string? -> void?
;;   Set the point size unit.
(define-jsx-alias (jsx-set-point-size-unit!/raw point unit)
  js-jsx-set-point-size-unit!)

;; jsx-point-snap-size-x/raw : external/raw -> flonum?
;;   Read the x snap size.
(define-jsx-alias (jsx-point-snap-size-x/raw point) js-jsx-point-snap-size-x)

;; jsx-set-point-snap-size-x!/raw : external/raw flonum? -> void?
;;   Set the x snap size.
(define-jsx-alias (jsx-set-point-snap-size-x!/raw point step)
  js-jsx-set-point-snap-size-x!)

;; jsx-point-snap-size-y/raw : external/raw -> flonum?
;;   Read the y snap size.
(define-jsx-alias (jsx-point-snap-size-y/raw point) js-jsx-point-snap-size-y)

;; jsx-set-point-snap-size-y!/raw : external/raw flonum? -> void?
;;   Set the y snap size.
(define-jsx-alias (jsx-set-point-snap-size-y!/raw point step)
  js-jsx-set-point-snap-size-y!)

;; jsx-point-snap-to-grid/raw : external/raw -> boolean?
;;   Read whether grid snapping is enabled.
(define-jsx-alias (jsx-point-snap-to-grid/raw point) js-jsx-point-snap-to-grid)

;; jsx-set-point-snap-to-grid!/raw : external/raw boolean? -> void?
;;   Enable or disable grid snapping.
(define-jsx-alias (jsx-set-point-snap-to-grid!/raw point enable)
  js-jsx-set-point-snap-to-grid!)

;; jsx-point-snap-to-points/raw : external/raw -> boolean?
;;   Read whether point snapping is enabled.
(define-jsx-alias (jsx-point-snap-to-points/raw point) js-jsx-point-snap-to-points)

;; jsx-set-point-snap-to-points!/raw : external/raw boolean? -> void?
;;   Enable or disable point snapping.
(define-jsx-alias (jsx-set-point-snap-to-points!/raw point enable)
  js-jsx-set-point-snap-to-points!)

;; jsx-point-snatch-distance/raw : external/raw -> flonum?
;;   Read the snap distance threshold.
(define-jsx-alias (jsx-point-snatch-distance/raw point) js-jsx-point-snatch-distance)

;; jsx-set-point-snatch-distance!/raw : external/raw flonum? -> void?
;;   Set the snap distance threshold.
(define-jsx-alias (jsx-set-point-snatch-distance!/raw point distance)
  js-jsx-set-point-snatch-distance!)

;; jsx-point-style/raw : external/raw -> exact-integer?
;;   Read the point style.
(define-jsx-alias (jsx-point-style/raw point) js-jsx-point-style)

;; jsx-set-point-style!/raw : external/raw exact-integer? -> void?
;;   Set the point style.
(define-jsx-alias (jsx-set-point-style!/raw point style) js-jsx-set-point-style!)

;; jsx-point-x/raw : external/raw -> flonum?
;;   Read the x coordinate of a point.
(define-jsx-alias (jsx-point-x/raw point) js-jsx-point-x)

;; jsx-point-y/raw : external/raw -> flonum?
;;   Read the y coordinate of a point.
(define-jsx-alias (jsx-point-y/raw point) js-jsx-point-y)

;; jsx-point-zoom/raw : external/raw -> boolean?
;;   Read whether point size scales with zoom.
(define-jsx-alias (jsx-point-zoom/raw point) js-jsx-point-zoom)

;; jsx-set-point-zoom!/raw : external/raw boolean? -> void?
;;   Enable or disable zoom scaling.
(define-jsx-alias (jsx-set-point-zoom!/raw point enable) js-jsx-set-point-zoom!)

;; jsx-point-has-point/raw : external/raw flonum? flonum? -> boolean?
;;   Check whether screen coordinates hit the point.
(define-jsx-alias (jsx-point-has-point/raw point x y) js-jsx-point-has-point)

;; jsx-point-is-on/raw : external/raw external/raw flonum? -> boolean?
;;   Check whether the point lies on another element.
(define-jsx-alias (jsx-point-is-on/raw point element tol) js-jsx-point-is-on)

;; jsx-point-make-intersection!/raw : external/raw external/raw external/raw exact-integer? exact-integer? -> void?
;;   Convert a point into an intersection definition.
(define-jsx-alias (jsx-point-make-intersection!/raw point el1 el2 i j)
  js-jsx-point-make-intersection!)

;; jsx-point-normalize-face/raw : external/raw string? -> string?
;;   Normalize a point face token.
(define-jsx-alias (jsx-point-normalize-face/raw point face)
  js-jsx-point-normalize-face)

;; jsx-point-set-style!/raw : external/raw exact-integer? -> void?
;;   Set the point style directly.
(define-jsx-alias (jsx-point-set-style!/raw point style) js-jsx-point-set-style!)

;; jsx-point-update!/raw : external/raw exact-integer? -> void?
;;   Update point state.
(define-jsx-alias (jsx-point-update!/raw point finalize) js-jsx-point-update!)

;; jsx-point-update-renderer!/raw : external/raw -> void?
;;   Refresh the point renderer.
(define-jsx-alias (jsx-point-update-renderer!/raw point)
  js-jsx-point-update-renderer!)

;; jsx-point-update-transform!/raw : external/raw exact-integer? -> external/raw
;;   Apply transformations to the point base element.
(define-jsx-alias (jsx-point-update-transform!/raw point finalize)
  js-jsx-point-update-transform!)

;;; -------------------------------------------------------------------
;;; Geometry and DOM helpers
;;; -------------------------------------------------------------------

;; jsx-key? : any/c -> boolean?
;;   Check whether x can be used as a JSXGraph key.
(define (jsx-key? x)
  (or (symbol? x) (string? x)))

;; jsx-key->string : any/c -> string?
;;   Convert a key-like value into a browser string.
(define (jsx-key->string x)
  (cond
    [(string? x) x]
    [(symbol? x) (symbol->string x)]
    [else
     (raise-argument-error 'jsx-key->string "(or/c string? symbol?)" x)]))

;; jsx-parents : any/c ... -> vector?
;;   Pack parent values into the vector shape JSXGraph expects.
(define (jsx-parents . xs)
  (list->vector (map jsx-unwrap xs)))

;; jsx-polygon-args : any/c ... -> vector?
;;   Pack polygon arguments into the vector shape JSXGraph expects.
(define (jsx-polygon-args . xs)
  (list->vector (map jsx-unpack-array xs)))

;; jsx-set-attribute! : external/raw any/c any/c -> void?
;;   Set a DOM attribute using JSXGraph-friendly key handling.
(define (jsx-set-attribute! elem key value)
  (js-set-attribute! elem (jsx-key->string key) value))

;; jsx-dot : any/c any/c ... -> any/c
;;   Follow a chain of object properties.
(define (jsx-dot obj . props)
  (let loop ([val obj]
             [ps  props])
    (cond
      [(null? ps) val]
      [else       (loop (js-ref val (car ps))
                        (cdr ps))])))

;; jsx-on : any/c (or/c string? symbol?) procedure? -> void?
;;   Install a JSXGraph object event handler.
(define (jsx-on target event handler)
  (define event* (jsx-key->string event))
  (define handler* (procedure->external handler))
  (js-jsx-element-call/nullish (jsx-unwrap target) "on" (vector event* handler*))
  (void))

;; jsx-element-add-event! : any/c (or/c string? symbol?) procedure? -> void?
;;   Register a GeometryElement event handler.
(define (jsx-element-add-event! element event handler)
  (define event* (jsx-key->string event))
  (define handler* (procedure->external handler))
  (js-jsx-element-add-event (jsx-unwrap element) event* handler*)
  (void))

;; jsx-create-board : string? [any/c #f] -> jsx-board?
;;   Create a board with the standard JSXGraph defaults.
(define (jsx-create-board container-id [maybe-attributes #f])
  (define attrs
    (or maybe-attributes
        (js-object
         (vector (vector "boundingbox" #[-5 5 5 -5])
                 (vector "axis" #t)
                 (vector "keepaspectratio" #t)))))
  (jsx-wrap-board (jsx-init-board/raw container-id attrs)))

;; jsx-create : jsx-board? (or/c string? symbol?) any/c [any/c #f] -> jsx-element?
;;   Create a JSXGraph object by name.
(define (jsx-create board element-type parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create/raw (jsx-board-raw board)
                         (jsx-key->string element-type)
                         parents
                         (or attributes '#[]))))

;; jsx-create-point : jsx-board? any/c [any/c #f] -> jsx-point?
;;   Create a point on a board.
(define (jsx-create-point board parents [attributes #f])
  (jsx-wrap-point
   (jsx-board-create-point/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-line : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a line on a board.
(define (jsx-create-line board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-line/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-line-direction : jsx-element? -> any/c
;;   Read the direction vector of a line.
(define (jsx-line-direction line)
  (jsx-line-call line "Direction" (vector)))

;; jsx-line-get-angle : jsx-element? -> any/c
;;   Read the line angle.
(define (jsx-line-get-angle line)
  (jsx-line-call line "getAngle" (vector)))

;; jsx-line-get-rise : jsx-element? -> any/c
;;   Read the line rise.
(define (jsx-line-get-rise line)
  (jsx-line-call line "getRise" (vector)))

;; jsx-line-get-slope : jsx-element? -> any/c
;;   Read the line slope.
(define (jsx-line-get-slope line)
  (jsx-line-call line "getSlope" (vector)))

;; jsx-line-horizontal? : jsx-element? -> boolean?
;;   Test whether the line is horizontal.
(define (jsx-line-horizontal? line)
  (jsx-line-call line "isHorizontal" (vector)))

;; jsx-line-vertical? : jsx-element? -> boolean?
;;   Test whether the line is vertical.
(define (jsx-line-vertical? line)
  (jsx-line-call line "isVertical" (vector)))

;; jsx-line-l : jsx-element? -> any/c
;;   Read the line L helper.
(define (jsx-line-l line)
  (jsx-line-call line "L" (vector)))

;; jsx-line-slope : jsx-element? -> any/c
;;   Read the line slope alias.
(define (jsx-line-slope line)
  (jsx-line-call line "Slope" (vector)))

;; jsx-line-set-fixed-length! : jsx-element? any/c -> void?
;;   Set the fixed length on a line.
(define (jsx-line-set-fixed-length! line length)
  (jsx-line-call/nullish line "setFixedLength" (vector length)))

;; jsx-line-x : jsx-element? any/c -> any/c
;;   Evaluate the X coordinate function on the line.
(define (jsx-line-x line t)
  (jsx-line-call line "X" (vector t)))

;; jsx-line-y : jsx-element? any/c -> any/c
;;   Evaluate the Y coordinate function on the line.
(define (jsx-line-y line t)
  (jsx-line-call line "Y" (vector t)))

;; jsx-line-z : jsx-element? any/c -> any/c
;;   Evaluate the Z coordinate function on the line.
(define (jsx-line-z line t)
  (jsx-line-call line "Z" (vector t)))

;; jsx-create-segment : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a segment on a board.
(define (jsx-create-segment board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-segment/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-arc : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create an arc on a board.
(define (jsx-create-arc board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-arc/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-angle : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create an angle on a board.
(define (jsx-create-angle board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-angle/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-sector : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a sector on a board.
(define (jsx-create-sector board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-sector/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-glider : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a glider on a board.
(define (jsx-create-glider board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-glider/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-circle : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a circle on a board.
(define (jsx-create-circle board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-circle/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-conic : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a conic on a board.
(define (jsx-create-conic board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-conic/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-ellipse : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create an ellipse on a board.
(define (jsx-create-ellipse board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-ellipse/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-curve : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a curve on a board.
(define (jsx-create-curve board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-curve/raw (jsx-board-raw board)
                               (jsx-curve-parents parents)
                               (or attributes '#[]))))

;; jsx-create-functiongraph : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a functiongraph on a board.
(define (jsx-create-functiongraph board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-functiongraph/raw (jsx-board-raw board)
                                        (jsx-functiongraph-parents parents)
                                        (or attributes '#[]))))

;; jsx-create-polygon : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a polygon on a board.
(define (jsx-create-polygon board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-polygon/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-midpoint : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a midpoint on a board.
(define (jsx-create-midpoint board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-midpoint/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-parallel : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a parallel line on a board.
(define (jsx-create-parallel board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-parallel/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-arrowparallel : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create an arrowparallel on a board.
(define (jsx-create-arrowparallel board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-arrowparallel/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-axis : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create an axis on a board.
(define (jsx-create-axis board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-axis/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-grid : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a grid on a board.
(define (jsx-create-grid board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-grid/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-boxplot : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a boxplot on a board.
(define (jsx-create-boxplot board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-boxplot/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-tangent : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a tangent on a board.
(define (jsx-create-tangent board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-tangent/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-tangentto : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a tangentto on a board.
(define (jsx-create-tangentto board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-tangentto/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-polarline : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a polarline on a board.
(define (jsx-create-polarline board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-polarline/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-polepoint : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a polepoint on a board.
(define (jsx-create-polepoint board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-polepoint/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-radicalaxis : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a radicalaxis on a board.
(define (jsx-create-radicalaxis board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-radicalaxis/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-circumcircle : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a circumcircle on a board.
(define (jsx-create-circumcircle board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-circumcircle/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-incircle : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create an incircle on a board.
(define (jsx-create-incircle board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-incircle/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-circumcirclearc : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a circumcirclearc on a board.
(define (jsx-create-circumcirclearc board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-circumcirclearc/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-circumcirclesector : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a circumcirclesector on a board.
(define (jsx-create-circumcirclesector board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-circumcirclesector/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-semicircle : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a semicircle on a board.
(define (jsx-create-semicircle board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-semicircle/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-majorarc : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a majorarc on a board.
(define (jsx-create-majorarc board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-majorarc/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-majorsector : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a majorsector on a board.
(define (jsx-create-majorsector board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-majorsector/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-curveintersection : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a curveintersection element.
(define (jsx-create-curveintersection board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-curveintersection/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-curvedifference : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a curvedifference element.
(define (jsx-create-curvedifference board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-curvedifference/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-curveunion : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a curveunion element.
(define (jsx-create-curveunion board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-curveunion/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-derivative : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a derivative element.
(define (jsx-create-derivative board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-derivative/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-integral : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create an integral element.
(define (jsx-create-integral board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-integral/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-riemannsum : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a riemannsum element.
(define (jsx-create-riemannsum board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-riemannsum/raw (jsx-board-raw board)
                                    (jsx-callable-pack parents)
                                    (or attributes '#[]))))

;; jsx-create-slopefield : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a slopefield element.
(define (jsx-create-slopefield board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-slopefield/raw (jsx-board-raw board)
                                    (jsx-callable-pack parents)
                                    (or attributes '#[]))))

;; jsx-create-vectorfield : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a vectorfield element.
(define (jsx-create-vectorfield board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-vectorfield/raw (jsx-board-raw board)
                                     (jsx-callable-pack parents)
                                     (or attributes '#[]))))

;; jsx-create-implicitcurve : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create an implicitcurve element.
(define (jsx-create-implicitcurve board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-implicitcurve/raw (jsx-board-raw board)
                                       (jsx-callable-pack parents)
                                       (or attributes '#[]))))

;; jsx-create-spline : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a spline element.
(define (jsx-create-spline board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-spline/raw (jsx-board-raw board)
                                (jsx-callable-pack parents)
                                (or attributes '#[]))))

;; jsx-create-cardinalspline : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a cardinal spline element.
(define (jsx-create-cardinalspline board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-cardinalspline/raw (jsx-board-raw board)
                                        (jsx-callable-pack parents)
                                        (or attributes '#[]))))

;; jsx-create-comb : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a comb element.
(define (jsx-create-comb board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-comb/raw (jsx-board-raw board)
                              (jsx-callable-pack parents)
                              (or attributes '#[]))))

;; jsx-create-metapostspline : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a metapost spline element.
(define (jsx-create-metapostspline board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-metapostspline/raw (jsx-board-raw board)
                                        (jsx-callable-pack parents)
                                        (or attributes '#[]))))

;; jsx-create-polygonalchain : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a polygonal chain element.
(define (jsx-create-polygonalchain board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-polygonalchain/raw (jsx-board-raw board)
                                        (jsx-callable-pack parents)
                                        (or attributes '#[]))))

;; jsx-create-regularpolygon : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a regular polygon element.
(define (jsx-create-regularpolygon board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-regularpolygon/raw (jsx-board-raw board)
                                        (jsx-callable-pack parents)
                                        (or attributes '#[]))))

;; jsx-create-hyperbola : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a hyperbola element.
(define (jsx-create-hyperbola board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-hyperbola/raw (jsx-board-raw board)
                                   (jsx-callable-pack parents)
                                   (or attributes '#[]))))

;; jsx-create-parabola : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a parabola element.
(define (jsx-create-parabola board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-parabola/raw (jsx-board-raw board)
                                  (jsx-callable-pack parents)
                                  (or attributes '#[]))))

;; jsx-create-stepfunction : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a stepfunction element.
(define (jsx-create-stepfunction board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-stepfunction/raw (jsx-board-raw board)
                                      (jsx-callable-pack parents)
                                      (or attributes '#[]))))

;; jsx-create-inequality : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create an inequality element.
(define (jsx-create-inequality board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-inequality/raw (jsx-board-raw board)
                                    (jsx-callable-pack parents)
                                    (or attributes '#[]))))

;; jsx-create-turtle : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a turtle element.
(define (jsx-create-turtle board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-turtle/raw (jsx-board-raw board)
                                (jsx-callable-pack parents)
                                (or attributes '#[]))))

;; jsx-riemannsum-value : jsx-element? -> any/c
;;   Read the value of a riemann sum.
(define (jsx-riemannsum-value riemannsum)
  (jsx-riemannsum-value/raw (jsx-unwrap riemannsum)))

;; jsx-slopefield-set-f! : jsx-element? any/c -> void?
;;   Set the slope field function.
(define (jsx-slopefield-set-f! field func)
  (jsx-slopefield-set-f!/raw (jsx-unwrap field) (jsx-callable-pack func))
  (void))

;; jsx-vectorfield-set-f! : jsx-element? any/c -> void?
;;   Set the vector field function.
(define (jsx-vectorfield-set-f! field func)
  (jsx-vectorfield-set-f!/raw (jsx-unwrap field) (jsx-callable-pack func))
  (void))

;; jsx-circle-area : jsx-element? -> any/c
;;   Read the area of a circle.
(define (jsx-circle-area circle)
  (jsx-circle-call circle "Area" (vector)))

;; jsx-circle-bounds : jsx-element? -> any/c
;;   Read the circle bounding box.
(define (jsx-circle-bounds circle)
  (jsx-circle-call circle "bounds" (vector)))

;; jsx-circle-diameter : jsx-element? -> any/c
;;   Read the diameter of a circle.
(define (jsx-circle-diameter circle)
  (jsx-circle-call circle "Diameter" (vector)))

;; jsx-circle-get-radius : jsx-element? -> any/c
;;   Read the radius of a circle.
(define (jsx-circle-get-radius circle)
  (jsx-circle-call circle "getRadius" (vector)))

;; jsx-circle-perimeter : jsx-element? -> any/c
;;   Read the perimeter of a circle.
(define (jsx-circle-perimeter circle)
  (jsx-circle-call circle "Perimeter" (vector)))

;; jsx-circle-radius : jsx-element? any/c -> any/c
;;   Set or read the circle radius helper.
(define (jsx-circle-radius circle value)
  (jsx-circle-call circle "Radius" (vector value)))

;; jsx-circle-set-radius! : jsx-element? any/c -> void?
;;   Set the circle radius.
(define (jsx-circle-set-radius! circle radius)
  (jsx-circle-call/nullish circle "setRadius" (vector radius)))

;; jsx-circle-update-quadraticform! : jsx-element? -> void?
;;   Update the circle quadratic form.
(define (jsx-circle-update-quadraticform! circle)
  (jsx-circle-call/nullish circle "updateQuadraticform" (vector)))

;; jsx-circle-update-renderer! : jsx-element? -> void?
;;   Refresh the circle renderer.
(define (jsx-circle-update-renderer! circle)
  (jsx-circle-call/nullish circle "updateRenderer" (vector)))

;; jsx-circle-update-stdform! : jsx-element? -> void?
;;   Update the circle standard form.
(define (jsx-circle-update-stdform! circle)
  (jsx-circle-call/nullish circle "updateStdform" (vector)))

;; jsx-circle-x : jsx-element? any/c -> any/c
;;   Evaluate the X coordinate function on the circle.
(define (jsx-circle-x circle t)
  (jsx-circle-call circle "X" (vector t)))

;; jsx-circle-y : jsx-element? any/c -> any/c
;;   Evaluate the Y coordinate function on the circle.
(define (jsx-circle-y circle t)
  (jsx-circle-call circle "Y" (vector t)))

;; jsx-circle-z : jsx-element? any/c -> any/c
;;   Evaluate the Z coordinate function on the circle.
(define (jsx-circle-z circle t)
  (jsx-circle-call circle "Z" (vector t)))

;; jsx-curve-allocate-points! : jsx-element? -> any/c
;;   Allocate the point cache for a curve.
(define (jsx-curve-allocate-points! curve)
  (jsx-curve-call curve "allocatePoints" (vector)))

;; jsx-curve-generate-term : jsx-element? -> any/c
;;   Generate the curve term function.
(define (jsx-curve-generate-term curve)
  (jsx-curve-call curve "generateTerm" (vector)))

;; jsx-curve-get-label-position : jsx-element? -> any/c
;;   Read the label position helper for a curve.
(define (jsx-curve-get-label-position curve)
  (jsx-curve-call curve "getLabelPosition" (vector)))

;; jsx-curve-get-transformation-source : jsx-element? -> any/c
;;   Read the transformation source of a curve.
(define (jsx-curve-get-transformation-source curve)
  (jsx-curve-call curve "getTransformationSource" (vector)))

;; jsx-curve-has-point? : jsx-element? any/c any/c -> boolean?
;;   Check whether screen coordinates hit a curve.
(define (jsx-curve-has-point? curve x y)
  (jsx-curve-call curve "hasPoint" (vector x y)))

;; jsx-curve-interpolation-function-from-array : jsx-element? any/c -> any/c
;;   Build an interpolation function from sample data.
(define (jsx-curve-interpolation-function-from-array curve data)
  (jsx-curve-call curve "interpolationFunctionFromArray" (vector data)))

;; jsx-curve-max-x : jsx-element? -> any/c
;;   Read the maximum x-value of a curve.
(define (jsx-curve-max-x curve)
  (jsx-curve-call curve "maxX" (vector)))

;; jsx-curve-min-x : jsx-element? -> any/c
;;   Read the minimum x-value of a curve.
(define (jsx-curve-min-x curve)
  (jsx-curve-call curve "minX" (vector)))

;; jsx-curve-move-to! : jsx-element? any/c -> any/c
;;   Move a curve to a new location.
(define (jsx-curve-move-to! curve where)
  (jsx-curve-call curve "moveTo" (vector where)))

;; jsx-curve-notify-parents! : jsx-element? -> any/c
;;   Notify parent elements of a curve change.
(define (jsx-curve-notify-parents! curve)
  (jsx-curve-call curve "notifyParents" (vector)))

;; jsx-curve-update-curve! : jsx-element? -> any/c
;;   Update the curve data.
(define (jsx-curve-update-curve! curve)
  (jsx-curve-call curve "updateCurve" (vector)))

;; jsx-curve-update-data-array! : jsx-element? -> any/c
;;   Update the curve data array.
(define (jsx-curve-update-data-array! curve)
  (jsx-curve-call curve "updateDataArray" (vector)))

;; jsx-curve-update-renderer! : jsx-element? -> any/c
;;   Refresh the curve renderer.
(define (jsx-curve-update-renderer! curve)
  (jsx-curve-call curve "updateRenderer" (vector)))

;; jsx-curve-update-transform! : jsx-element? -> any/c
;;   Update a curve transformation.
(define (jsx-curve-update-transform! curve)
  (jsx-curve-call curve "updateTransform" (vector)))

;; jsx-polygon-add-points! : jsx-element? any/c ... -> jsx-element?
;;   Add vertices to a polygon.
(define (jsx-polygon-add-points! polygon . points)
  (jsx-wrap-element
   (jsx-polygon-call polygon "addPoints" (apply jsx-polygon-args points))))

;; jsx-polygon-area : jsx-element? -> any/c
;;   Read the polygon area.
(define (jsx-polygon-area polygon)
  (jsx-polygon-call polygon "Area" (vector)))

;; jsx-polygon-bounding-box : jsx-element? -> any/c
;;   Read the polygon bounding box.
(define (jsx-polygon-bounding-box polygon)
  (jsx-polygon-call polygon "boundingBox" (vector)))

;; jsx-polygon-find-point : jsx-element? any/c -> any/c
;;   Find the index of a polygon vertex.
(define (jsx-polygon-find-point polygon point)
  (jsx-polygon-call polygon "findPoint" (vector (jsx-unwrap point))))

;; jsx-polygon-has-point? : jsx-element? any/c any/c -> boolean?
;;   Check whether screen coordinates hit a polygon.
(define (jsx-polygon-has-point? polygon x y)
  (jsx-polygon-call polygon "hasPoint" (vector x y)))

;; jsx-polygon-hide-element! : jsx-element? [any/c #f] -> void?
;;   Hide a polygon and optionally keep the borders visible.
(define (jsx-polygon-hide-element! polygon [borderless #f])
  (jsx-polygon-call/nullish polygon "hideElement" (vector borderless)))

;; jsx-polygon-insert-points! : jsx-element? any/c ... -> jsx-element?
;;   Insert vertices into a polygon.
(define (jsx-polygon-insert-points! polygon . points)
  (jsx-wrap-element
   (jsx-polygon-call polygon "insertPoints" (apply jsx-polygon-args points))))

;; jsx-polygon-intersect : jsx-element? any/c -> any/c
;;   Intersect a polygon with another polygon.
(define (jsx-polygon-intersect polygon other)
  (jsx-polygon-call polygon "intersect" (vector (jsx-unwrap other))))

;; jsx-polygon-l : jsx-element? -> any/c
;;   Read the polygon perimeter alias.
(define (jsx-polygon-l polygon)
  (jsx-polygon-call polygon "L" (vector)))

;; jsx-polygon-perimeter : jsx-element? -> any/c
;;   Read the polygon perimeter.
(define (jsx-polygon-perimeter polygon)
  (jsx-polygon-call polygon "Perimeter" (vector)))

;; jsx-polygon-pnpoly : jsx-element? any/c any/c [any/c #f] -> boolean?
;;   Test whether coordinates are inside a polygon.
(define (jsx-polygon-pnpoly polygon x y [coord-type #f])
  (jsx-polygon-call polygon "pnpoly"
                    (if coord-type
                        (vector x y coord-type)
                        (vector x y (void)))))

;; jsx-polygon-remove-points! : jsx-element? any/c ... -> jsx-element?
;;   Remove vertices from a polygon.
(define (jsx-polygon-remove-points! polygon . points)
  (jsx-wrap-element
   (jsx-polygon-call polygon "removePoints" (apply jsx-polygon-args points))))

;; jsx-polygon-set-position-directly! : jsx-element? any/c any/c any/c -> jsx-element?
;;   Move a polygon directly by coordinate delta.
(define (jsx-polygon-set-position-directly! polygon method coords oldcoords)
  (jsx-wrap-element
   (jsx-polygon-call polygon "setPositionDirectly"
                     (vector method
                             (jsx-unpack-array coords)
                             (jsx-unpack-array oldcoords)))))

;; jsx-polygon-show-element! : jsx-element? [any/c #f] -> void?
;;   Show a polygon and optionally keep the borders visible.
(define (jsx-polygon-show-element! polygon [borderless #f])
  (jsx-polygon-call/nullish polygon "showElement" (vector borderless)))

;; jsx-polygon-update-renderer! : jsx-element? -> void?
;;   Refresh the polygon renderer.
(define (jsx-polygon-update-renderer! polygon)
  (jsx-polygon-call/nullish polygon "updateRenderer" (vector)))

;; jsx-create-perpendicular : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a perpendicular line on a board.
(define (jsx-create-perpendicular board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-perpendicular/raw (jsx-board-raw board) parents
                                       (or attributes '#[]))))

;; jsx-create-reflection : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a reflection on a board.
(define (jsx-create-reflection board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-reflection/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-bisector : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a bisector on a board.
(define (jsx-create-bisector board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-bisector/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-normal : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a normal line on a board.
(define (jsx-create-normal board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-normal/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-intersection : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create an intersection point on a board.
(define (jsx-create-intersection board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-intersection/raw (jsx-board-raw board) parents
                                      (or attributes '#[]))))

;; jsx-create-arrow : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create an arrow on a board.
(define (jsx-create-arrow board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-arrow/raw (jsx-board-raw board) parents
                               (or attributes '#[]))))

;; jsx-create-button : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a button on a board.
(define (jsx-create-button board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-button/raw (jsx-board-raw board) parents
                                (or attributes '#[]))))

;; jsx-create-checkbox : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a checkbox on a board.
(define (jsx-create-checkbox board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-checkbox/raw (jsx-board-raw board) parents
                                  (or attributes '#[]))))

;; jsx-checkbox-value : jsx-element? -> boolean?
;;   Read the current checkbox value.
(define (jsx-checkbox-value checkbox)
  (js-jsx-checkbox-Value (jsx-unwrap checkbox)))

;; jsx-create-input : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create an input field on a board.
(define (jsx-create-input board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-input/raw (jsx-board-raw board) parents
                               (or attributes '#[]))))

;; jsx-create-slider : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a slider on a board.
(define (jsx-create-slider board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-slider/raw (jsx-board-raw board) parents
                                (or attributes '#[]))))

;; jsx-create-chart : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a chart on a board.
(define (jsx-create-chart board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-chart/raw (jsx-board-raw board) parents
                               (or attributes '#[]))))

;; jsx-create-legend : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a legend on a board.
(define (jsx-create-legend board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-legend/raw (jsx-board-raw board) parents
                                (or attributes '#[]))))

;; jsx-create-smartlabel : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a smartlabel on a board.
(define (jsx-create-smartlabel board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-smartlabel/raw (jsx-board-raw board) parents
                                    (or attributes '#[]))))

;; jsx-create-foreignobject : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a foreignobject on a board.
(define (jsx-create-foreignobject board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-foreignobject/raw (jsx-board-raw board) parents
                                       (or attributes '#[]))))

;; jsx-create-tapemeasure : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a tapemeasure on a board.
(define (jsx-create-tapemeasure board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-tapemeasure/raw (jsx-board-raw board) parents
                                     (or attributes '#[]))))

;; jsx-create-ticks : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a ticks object on a board.
(define (jsx-create-ticks board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-ticks/raw (jsx-board-raw board) parents
                               (or attributes '#[]))))

;; jsx-create-transformation : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a transformation object on a board.
(define (jsx-create-transformation board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-transformation/raw (jsx-board-raw board) parents
                                        (or attributes '#[]))))

;; jsx-create-tracecurve : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a tracecurve object on a board.
(define (jsx-create-tracecurve board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-tracecurve/raw (jsx-board-raw board) parents
                                    (or attributes '#[]))))

;; jsx-create-parallelogram : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a parallelogram object on a board.
(define (jsx-create-parallelogram board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-parallelogram/raw (jsx-board-raw board) parents
                                       (or attributes '#[]))))

;; jsx-create-reflexangle : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a reflexangle object on a board.
(define (jsx-create-reflexangle board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-reflexangle/raw (jsx-board-raw board) parents
                                     (or attributes '#[]))))

;; jsx-create-measurement : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a measurement on a board.
(define (jsx-create-measurement board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-measurement/raw (jsx-board-raw board) parents
                                     (or attributes '#[]))))

;; jsx-create-circumcenter : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a circumcenter on a board.
(define (jsx-create-circumcenter board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-circumcenter/raw (jsx-board-raw board) parents
                                      (or attributes '#[]))))

;; jsx-create-mirrorelement : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a mirrorelement on a board.
(define (jsx-create-mirrorelement board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-mirrorelement/raw (jsx-board-raw board) parents
                                       (or attributes '#[]))))

;; jsx-create-mirrorpoint : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a mirror point on a board.
(define (jsx-create-mirrorpoint board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-mirrorpoint/raw (jsx-board-raw board) parents
                                     (or attributes '#[]))))

;; jsx-create-otherintersection : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create an other intersection on a board.
(define (jsx-create-otherintersection board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-otherintersection/raw (jsx-board-raw board) parents
                                           (or attributes '#[]))))

;; jsx-create-orthogonalprojection : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create an orthogonal projection on a board.
(define (jsx-create-orthogonalprojection board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-orthogonalprojection/raw (jsx-board-raw board) parents
                                              (or attributes '#[]))))

;; jsx-create-parallelpoint : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a parallel point on a board.
(define (jsx-create-parallelpoint board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-parallelpoint/raw (jsx-board-raw board) parents
                                       (or attributes '#[]))))

;; jsx-create-perpendicularpoint : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a perpendicular point on a board.
(define (jsx-create-perpendicularpoint board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-perpendicularpoint/raw (jsx-board-raw board) parents
                                            (or attributes '#[]))))

;; jsx-input-set! : jsx-element? any/c -> jsx-element?
;;   Set the current input value.
(define (jsx-input-set! input value)
  (jsx-wrap-element
   (js-jsx-input-set (jsx-unwrap input) value)))

;; jsx-input-value : jsx-element? -> string?
;;   Read the current input value.
(define (jsx-input-value input)
  (js-jsx-input-Value (jsx-unwrap input)))

;; jsx-slider-set-max! : jsx-element? any/c -> jsx-element?
;;   Set the maximum slider value.
(define (jsx-slider-set-max! slider value)
  (jsx-wrap-element
   (js-jsx-slider-setMax (jsx-unwrap slider) value)))

;; jsx-slider-set-min! : jsx-element? any/c -> jsx-element?
;;   Set the minimum slider value.
(define (jsx-slider-set-min! slider value)
  (jsx-wrap-element
   (js-jsx-slider-setMin (jsx-unwrap slider) value)))

;; jsx-slider-set-value! : jsx-element? any/c -> jsx-element?
;;   Set the current slider value.
(define (jsx-slider-set-value! slider value)
  (jsx-wrap-element
   (js-jsx-slider-setValue (jsx-unwrap slider) value)))

;; jsx-slider-value : jsx-element? -> f64?
;;   Read the current slider value.
(define (jsx-slider-value slider)
  (js-jsx-slider-Value (jsx-unwrap slider)))

;; jsx-chart-draw-bar : jsx-element? any/c ... -> any/c
;;   Draw a bar chart rendering.
(define (jsx-chart-draw-bar chart . args)
  (js-jsx-chart-drawBar (jsx-unwrap chart) (list->vector args)))

;; jsx-chart-draw-fit : jsx-element? any/c ... -> any/c
;;   Draw a fit chart rendering.
(define (jsx-chart-draw-fit chart . args)
  (js-jsx-chart-drawFit (jsx-unwrap chart) (list->vector args)))

;; jsx-chart-draw-line : jsx-element? any/c ... -> any/c
;;   Draw a line chart rendering.
(define (jsx-chart-draw-line chart . args)
  (js-jsx-chart-drawLine (jsx-unwrap chart) (list->vector args)))

;; jsx-chart-draw-pie : jsx-element? any/c ... -> any/c
;;   Draw a pie chart rendering.
(define (jsx-chart-draw-pie chart . args)
  (js-jsx-chart-drawPie (jsx-unwrap chart) (list->vector args)))

;; jsx-chart-draw-points : jsx-element? any/c ... -> any/c
;;   Draw a point chart rendering.
(define (jsx-chart-draw-points chart . args)
  (js-jsx-chart-drawPoints (jsx-unwrap chart) (list->vector args)))

;; jsx-chart-draw-radar : jsx-element? any/c ... -> any/c
;;   Draw a radar chart rendering.
(define (jsx-chart-draw-radar chart . args)
  (js-jsx-chart-drawRadar (jsx-unwrap chart) (list->vector args)))

;; jsx-chart-draw-spline : jsx-element? any/c ... -> any/c
;;   Draw a spline chart rendering.
(define (jsx-chart-draw-spline chart . args)
  (js-jsx-chart-drawSpline (jsx-unwrap chart) (list->vector args)))

;; jsx-chart-update-data-array! : jsx-element? any/c ... -> any/c
;;   Update the chart data array.
(define (jsx-chart-update-data-array! chart . args)
  (js-jsx-chart-updateDataArray (jsx-unwrap chart) (list->vector args)))

;; jsx-chart-update-renderer! : jsx-element? any/c ... -> any/c
;;   Update the chart renderer.
(define (jsx-chart-update-renderer! chart . args)
  (js-jsx-chart-updateRenderer (jsx-unwrap chart) (list->vector args)))

;; jsx-create-text : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a text object on a board.
(define (jsx-create-text board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-text/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-image : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create an image object on a board.
(define (jsx-create-image board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-image/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-group : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a group object on a board.
(define (jsx-create-group board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-group/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-group-add-parents!/raw : external/raw any/c -> external/raw
;;   Add parents to a group.
(define (jsx-group-add-parents!/raw group args)
  (js-jsx-group-addParents (jsx-unwrap group) (jsx-unpack-array args)))

;; jsx-group-add-point!/raw : external/raw any/c -> external/raw
;;   Add one point to a group.
(define (jsx-group-add-point!/raw group args)
  (js-jsx-group-addPoint (jsx-unwrap group) (jsx-unpack-array args)))

;; jsx-group-add-points!/raw : external/raw any/c -> external/raw
;;   Add multiple points to a group.
(define (jsx-group-add-points!/raw group args)
  (js-jsx-group-addPoints (jsx-unwrap group) (jsx-unpack-array args)))

;; jsx-group-add-rotation-point!/raw : external/raw any/c -> external/raw
;;   Add a rotation point to a group.
(define (jsx-group-add-rotation-point!/raw group args)
  (js-jsx-group-addRotationPoint (jsx-unwrap group) (jsx-unpack-array args)))

;; jsx-group-add-scale-point!/raw : external/raw any/c -> external/raw
;;   Add a scale point to a group.
(define (jsx-group-add-scale-point!/raw group args)
  (js-jsx-group-addScalePoint (jsx-unwrap group) (jsx-unpack-array args)))

;; jsx-group-add-translation-point!/raw : external/raw any/c -> external/raw
;;   Add a translation point to a group.
(define (jsx-group-add-translation-point!/raw group args)
  (js-jsx-group-addTranslationPoint (jsx-unwrap group) (jsx-unpack-array args)))

;; jsx-group-add-group!/raw : external/raw any/c -> external/raw
;;   Add a nested group.
(define (jsx-group-add-group!/raw group args)
  (js-jsx-group-addGroup (jsx-unwrap group) (jsx-unpack-array args)))

;; jsx-group-remove-point!/raw : external/raw any/c -> external/raw
;;   Remove a point from a group.
(define (jsx-group-remove-point!/raw group args)
  (js-jsx-group-removePoint (jsx-unwrap group) (jsx-unpack-array args)))

;; jsx-group-remove-rotation-point!/raw : external/raw any/c -> external/raw
;;   Remove a rotation point from a group.
(define (jsx-group-remove-rotation-point!/raw group args)
  (js-jsx-group-removeRotationPoint (jsx-unwrap group) (jsx-unpack-array args)))

;; jsx-group-remove-scale-point!/raw : external/raw any/c -> external/raw
;;   Remove a scale point from a group.
(define (jsx-group-remove-scale-point!/raw group args)
  (js-jsx-group-removeScalePoint (jsx-unwrap group) (jsx-unpack-array args)))

;; jsx-group-remove-translation-point!/raw : external/raw any/c -> external/raw
;;   Remove a translation point from a group.
(define (jsx-group-remove-translation-point!/raw group args)
  (js-jsx-group-removeTranslationPoint (jsx-unwrap group) (jsx-unpack-array args)))

;; jsx-group-set-parents!/raw : external/raw any/c -> external/raw
;;   Replace the group's parents.
(define (jsx-group-set-parents!/raw group args)
  (js-jsx-group-setParents (jsx-unwrap group) (jsx-unpack-array args)))

;; jsx-group-set-rotation-center!/raw : external/raw any/c -> external/raw
;;   Set the rotation center for a group.
(define (jsx-group-set-rotation-center!/raw group args)
  (js-jsx-group-setRotationCenter (jsx-unwrap group) (jsx-unpack-array args)))

;; jsx-group-set-rotation-points!/raw : external/raw any/c -> external/raw
;;   Set the rotation points for a group.
(define (jsx-group-set-rotation-points!/raw group args)
  (js-jsx-group-setRotationPoints (jsx-unwrap group) (jsx-unpack-array args)))

;; jsx-group-set-scale-center!/raw : external/raw any/c -> external/raw
;;   Set the scale center for a group.
(define (jsx-group-set-scale-center!/raw group args)
  (js-jsx-group-setScaleCenter (jsx-unwrap group) (jsx-unpack-array args)))

;; jsx-group-set-scale-points!/raw : external/raw any/c -> external/raw
;;   Set the scale points for a group.
(define (jsx-group-set-scale-points!/raw group args)
  (js-jsx-group-setScalePoints (jsx-unwrap group) (jsx-unpack-array args)))

;; jsx-group-set-translation-points!/raw : external/raw any/c -> external/raw
;;   Set the translation points for a group.
(define (jsx-group-set-translation-points!/raw group args)
  (js-jsx-group-setTranslationPoints (jsx-unwrap group) (jsx-unpack-array args)))

;; jsx-group-ungroup!/raw : external/raw -> external/raw
;;   Ungroup all members.
(define (jsx-group-ungroup!/raw group args)
  (js-jsx-group-ungroup (jsx-unwrap group)))

;; jsx-group-update!/raw : external/raw -> external/raw
;;   Update a group.
(define (jsx-group-update!/raw group args)
  (js-jsx-group-update (jsx-unwrap group)))

;; jsx-group-add-parents! : jsx-element? any/c ... -> void?
;;   Add parents to a group.
(define (jsx-group-add-parents! group . args)
  (jsx-group-add-parents!/raw group (list->vector args))
  (void))

;; jsx-group-add-point! : jsx-element? any/c ... -> void?
;;   Add a point to a group.
(define (jsx-group-add-point! group . args)
  (jsx-group-add-point!/raw group (list->vector args))
  (void))

;; jsx-group-add-points! : jsx-element? any/c ... -> void?
;;   Add points to a group.
(define (jsx-group-add-points! group . args)
  (jsx-group-add-points!/raw group (list->vector args))
  (void))

;; jsx-group-add-rotation-point! : jsx-element? any/c ... -> void?
;;   Add a rotation point to a group.
(define (jsx-group-add-rotation-point! group . args)
  (jsx-group-add-rotation-point!/raw group (list->vector args))
  (void))

;; jsx-group-add-scale-point! : jsx-element? any/c ... -> void?
;;   Add a scale point to a group.
(define (jsx-group-add-scale-point! group . args)
  (jsx-group-add-scale-point!/raw group (list->vector args))
  (void))

;; jsx-group-add-translation-point! : jsx-element? any/c ... -> void?
;;   Add a translation point to a group.
(define (jsx-group-add-translation-point! group . args)
  (jsx-group-add-translation-point!/raw group (list->vector args))
  (void))

;; jsx-group-add-group! : jsx-element? any/c ... -> void?
;;   Add a nested group.
(define (jsx-group-add-group! group . args)
  (jsx-group-add-group!/raw group (list->vector args))
  (void))

;; jsx-group-remove-point! : jsx-element? any/c ... -> void?
;;   Remove a point from a group.
(define (jsx-group-remove-point! group . args)
  (jsx-group-remove-point!/raw group (list->vector args))
  (void))

;; jsx-group-remove-rotation-point! : jsx-element? any/c ... -> void?
;;   Remove a rotation point from a group.
(define (jsx-group-remove-rotation-point! group . args)
  (jsx-group-remove-rotation-point!/raw group (list->vector args))
  (void))

;; jsx-group-remove-scale-point! : jsx-element? any/c ... -> void?
;;   Remove a scale point from a group.
(define (jsx-group-remove-scale-point! group . args)
  (jsx-group-remove-scale-point!/raw group (list->vector args))
  (void))

;; jsx-group-remove-translation-point! : jsx-element? any/c ... -> void?
;;   Remove a translation point from a group.
(define (jsx-group-remove-translation-point! group . args)
  (jsx-group-remove-translation-point!/raw group (list->vector args))
  (void))

;; jsx-group-set-parents! : jsx-element? any/c ... -> void?
;;   Replace a group's parents.
(define (jsx-group-set-parents! group . args)
  (jsx-group-set-parents!/raw group (list->vector args))
  (void))

;; jsx-group-set-rotation-center! : jsx-element? any/c ... -> void?
;;   Set the group rotation center.
(define (jsx-group-set-rotation-center! group . args)
  (jsx-group-set-rotation-center!/raw group (list->vector args))
  (void))

;; jsx-group-set-rotation-points! : jsx-element? any/c ... -> void?
;;   Set the group rotation points.
(define (jsx-group-set-rotation-points! group . args)
  (jsx-group-set-rotation-points!/raw group (list->vector args))
  (void))

;; jsx-group-set-scale-center! : jsx-element? any/c ... -> void?
;;   Set the group scale center.
(define (jsx-group-set-scale-center! group . args)
  (jsx-group-set-scale-center!/raw group (list->vector args))
  (void))

;; jsx-group-set-scale-points! : jsx-element? any/c ... -> void?
;;   Set the group scale points.
(define (jsx-group-set-scale-points! group . args)
  (jsx-group-set-scale-points!/raw group (list->vector args))
  (void))

;; jsx-group-set-translation-points! : jsx-element? any/c ... -> void?
;;   Set the group translation points.
(define (jsx-group-set-translation-points! group . args)
  (jsx-group-set-translation-points!/raw group (list->vector args))
  (void))

;; jsx-group-ungroup! : jsx-element? -> void?
;;   Ungroup all members.
(define (jsx-group-ungroup! group)
  (jsx-group-ungroup!/raw group (vector))
  (void))

;; jsx-group-update! : jsx-element? -> void?
;;   Update a group.
(define (jsx-group-update! group)
  (jsx-group-update!/raw group (vector))
  (void))

;; jsx-text-_createFctUpdateText : jsx-element? any/c ... -> any/c
;;   Create the update function for text rendering.
(define (jsx-text-_createFctUpdateText text . args)
  (jsx-text-_createFctUpdateText/raw text (list->vector args)))

;; jsx-text-_setText : jsx-element? any/c ... -> any/c
;;   Set text internals directly.
(define (jsx-text-_setText text . args)
  (jsx-text-_setText/raw text (list->vector args)))

;; jsx-text-bounds : jsx-element? any/c ... -> any/c
;;   Read the text bounding box.
(define (jsx-text-bounds text . args)
  (jsx-text-bounds/raw text (list->vector args)))

;; jsx-text-checkForSizeUpdate : jsx-element? any/c ... -> any/c
;;   Check whether the text size needs an update.
(define (jsx-text-checkForSizeUpdate text . args)
  (jsx-text-checkForSizeUpdate/raw text (list->vector args)))

;; jsx-text-convertGeonext2CSS : jsx-element? any/c ... -> any/c
;;   Convert GEONExT markup to CSS.
(define (jsx-text-convertGeonext2CSS text . args)
  (jsx-text-convertGeonext2CSS/raw text (list->vector args)))

;; jsx-text-convertGeonextAndSketchometry2CSS : jsx-element? any/c ... -> any/c
;;   Convert GEONExT and Sketchometry markup to CSS.
(define (jsx-text-convertGeonextAndSketchometry2CSS text . args)
  (jsx-text-convertGeonextAndSketchometry2CSS/raw text (list->vector args)))

;; jsx-text-convertSketchometry2CSS : jsx-element? any/c ... -> any/c
;;   Convert Sketchometry markup to CSS.
(define (jsx-text-convertSketchometry2CSS text . args)
  (jsx-text-convertSketchometry2CSS/raw text (list->vector args)))

;; jsx-text-crudeSizeEstimate : jsx-element? any/c ... -> any/c
;;   Estimate the text size crudely.
(define (jsx-text-crudeSizeEstimate text . args)
  (jsx-text-crudeSizeEstimate/raw text (list->vector args)))

;; jsx-text-escapeTicks : jsx-element? any/c ... -> any/c
;;   Escape tick marks in text.
(define (jsx-text-escapeTicks text . args)
  (jsx-text-escapeTicks/raw text (list->vector args)))

;; jsx-text-expandShortMath : jsx-element? any/c ... -> any/c
;;   Expand short math notation in text.
(define (jsx-text-expandShortMath text . args)
  (jsx-text-expandShortMath/raw text (list->vector args)))

;; jsx-text-generateTerm : jsx-element? any/c ... -> any/c
;;   Generate the text term.
(define (jsx-text-generateTerm text . args)
  (jsx-text-generateTerm/raw text (list->vector args)))

;; jsx-text-getAnchorX : jsx-element? any/c ... -> any/c
;;   Read the X anchor position for text.
(define (jsx-text-getAnchorX text . args)
  (jsx-text-getAnchorX/raw text (list->vector args)))

;; jsx-text-getAnchorY : jsx-element? any/c ... -> any/c
;;   Read the Y anchor position for text.
(define (jsx-text-getAnchorY text . args)
  (jsx-text-getAnchorY/raw text (list->vector args)))

;; jsx-text-getNumberOfConflicts : jsx-element? any/c ... -> any/c
;;   Read the number of text placement conflicts.
(define (jsx-text-getNumberOfConflicts text . args)
  (jsx-text-getNumberOfConflicts/raw text (list->vector args)))

;; jsx-text-getSize : jsx-element? any/c ... -> any/c
;;   Read the text size.
(define (jsx-text-getSize text . args)
  (jsx-text-getSize/raw text (list->vector args)))

;; jsx-text-hasPoint : jsx-element? any/c ... -> any/c
;;   Test whether a screen position hits the text.
(define (jsx-text-hasPoint text . args)
  (jsx-text-hasPoint/raw text (list->vector args)))

;; jsx-text-notifyParents : jsx-element? any/c ... -> any/c
;;   Notify parent elements that the text changed.
(define (jsx-text-notifyParents text . args)
  (jsx-text-notifyParents/raw text (list->vector args)))

;; jsx-text-poorMansTeX : jsx-element? any/c ... -> any/c
;;   Render poor-man's TeX markup.
(define (jsx-text-poorMansTeX text . args)
  (jsx-text-poorMansTeX/raw text (list->vector args)))

;; jsx-text-replaceSub : jsx-element? any/c ... -> any/c
;;   Replace a subscript fragment.
(define (jsx-text-replaceSub text . args)
  (jsx-text-replaceSub/raw text (list->vector args)))

;; jsx-text-replaceSup : jsx-element? any/c ... -> any/c
;;   Replace a superscript fragment.
(define (jsx-text-replaceSup text . args)
  (jsx-text-replaceSup/raw text (list->vector args)))

;; jsx-text-setAutoPosition : jsx-element? any/c ... -> any/c
;;   Toggle automatic positioning.
(define (jsx-text-setAutoPosition text . args)
  (jsx-text-setAutoPosition/raw text (list->vector args)))

;; jsx-text-setCoords : jsx-element? any/c ... -> any/c
;;   Set the text coordinates.
(define (jsx-text-setCoords text . args)
  (jsx-text-setCoords/raw text (list->vector args)))

;; jsx-text-setText : jsx-element? any/c ... -> any/c
;;   Set the displayed text.
(define (jsx-text-setText text . args)
  (jsx-text-setText/raw text (list->vector args)))

;; jsx-text-setTextJessieCode : jsx-element? any/c ... -> any/c
;;   Set the text from JessieCode.
(define (jsx-text-setTextJessieCode text . args)
  (jsx-text-setTextJessieCode/raw text (list->vector args)))

;; jsx-text-unescapeTicks : jsx-element? any/c ... -> any/c
;;   Unescape tick marks in text.
(define (jsx-text-unescapeTicks text . args)
  (jsx-text-unescapeTicks/raw text (list->vector args)))

;; jsx-text-updateSize : jsx-element? any/c ... -> any/c
;;   Update the text size.
(define (jsx-text-updateSize text . args)
  (jsx-text-updateSize/raw text (list->vector args)))

;; jsx-text-updateText : jsx-element? any/c ... -> any/c
;;   Refresh the rendered text.
(define (jsx-text-updateText text . args)
  (jsx-text-updateText/raw text (list->vector args)))

;; jsx-text-utf8_decode : jsx-element? any/c ... -> any/c
;;   Decode UTF-8 text data.
(define (jsx-text-utf8_decode text . args)
  (jsx-text-utf8_decode/raw text (list->vector args)))

;; jsx-text-valueTagToJessieCode : jsx-element? any/c ... -> any/c
;;   Convert a value tag to JessieCode.
(define (jsx-text-valueTagToJessieCode text . args)
  (jsx-text-valueTagToJessieCode/raw text (list->vector args)))

;; jsx-image-H/raw : external/raw any/c -> external/raw
;;   Read the image H helper.
(define-jsx-alias (jsx-image-H/raw image args) js-jsx-image-H)

;; jsx-image-W/raw : external/raw any/c -> external/raw
;;   Read the image W helper.
(define-jsx-alias (jsx-image-W/raw image args) js-jsx-image-W)

;; jsx-image-has-point?/raw : external/raw any/c -> boolean?
;;   Check whether a point hits the image.
(define-jsx-alias (jsx-image-has-point?/raw image args) js-jsx-image-has-point)

;; jsx-image-set-size!/raw : external/raw any/c -> external/raw
;;   Set the image size directly.
(define-jsx-alias (jsx-image-set-size!/raw image args) js-jsx-image-set-size)

;; jsx-image-update!/raw : external/raw any/c -> external/raw
;;   Update an image.
(define-jsx-alias (jsx-image-update!/raw image args) js-jsx-image-update)

;; jsx-image-update-renderer!/raw : external/raw any/c -> external/raw
;;   Refresh the image renderer.
(define-jsx-alias (jsx-image-update-renderer!/raw image args)
  js-jsx-image-update-renderer)

;; jsx-image-update-size!/raw : external/raw any/c -> external/raw
;;   Update the image size.
(define-jsx-alias (jsx-image-update-size!/raw image args) js-jsx-image-update-size)

;; jsx-image-update-span!/raw : external/raw any/c -> external/raw
;;   Update the image span.
(define-jsx-alias (jsx-image-update-span!/raw image args) js-jsx-image-update-span)

;; jsx-image-H : jsx-element? any/c ... -> any/c
;;   Read the image H helper.
(define (jsx-image-H image . args)
  (jsx-image-H/raw image (list->vector args)))

;; jsx-image-W : jsx-element? any/c ... -> any/c
;;   Read the image W helper.
(define (jsx-image-W image . args)
  (jsx-image-W/raw image (list->vector args)))

;; jsx-image-has-point? : jsx-element? any/c ... -> boolean?
;;   Check whether a point hits the image.
(define (jsx-image-has-point? image . args)
  (jsx-image-has-point?/raw image (list->vector args)))

;; jsx-image-set-size! : jsx-element? any/c ... -> void?
;;   Set the image size directly.
(define (jsx-image-set-size! image . args)
  (jsx-image-set-size!/raw image (list->vector args))
  (void))

;; jsx-image-update! : jsx-element? any/c ... -> void?
;;   Update an image.
(define (jsx-image-update! image . args)
  (jsx-image-update!/raw image (list->vector args))
  (void))

;; jsx-image-update-renderer! : jsx-element? any/c ... -> void?
;;   Refresh the image renderer.
(define (jsx-image-update-renderer! image . args)
  (jsx-image-update-renderer!/raw image (list->vector args))
  (void))

;; jsx-image-update-size! : jsx-element? any/c ... -> void?
;;   Update the image size.
(define (jsx-image-update-size! image . args)
  (jsx-image-update-size!/raw image (list->vector args))
  (void))

;; jsx-image-update-span! : jsx-element? any/c ... -> void?
;;   Update the image span.
(define (jsx-image-update-span! image . args)
  (jsx-image-update-span!/raw image (list->vector args))
  (void))

;; jsx-foreignobject-H/raw : external/raw any/c -> external/raw
;;   Read the foreign object H helper.
(define-jsx-alias (jsx-foreignobject-H/raw foreignobject args)
  js-jsx-foreignobject-H)

;; jsx-foreignobject-W/raw : external/raw any/c -> external/raw
;;   Read the foreign object W helper.
(define-jsx-alias (jsx-foreignobject-W/raw foreignobject args)
  js-jsx-foreignobject-W)

;; jsx-foreignobject-has-point?/raw : external/raw any/c -> boolean?
;;   Check whether a point hits the foreign object.
(define-jsx-alias (jsx-foreignobject-has-point?/raw foreignobject args)
  js-jsx-foreignobject-has-point)

;; jsx-foreignobject-set-size!/raw : external/raw any/c -> external/raw
;;   Set the foreign object size directly.
(define-jsx-alias (jsx-foreignobject-set-size!/raw foreignobject args)
  js-jsx-foreignobject-set-size)

;; jsx-foreignobject-update!/raw : external/raw any/c -> external/raw
;;   Update a foreign object.
(define-jsx-alias (jsx-foreignobject-update!/raw foreignobject args)
  js-jsx-foreignobject-update)

;; jsx-foreignobject-update-renderer!/raw : external/raw any/c -> external/raw
;;   Refresh the foreign object renderer.
(define-jsx-alias (jsx-foreignobject-update-renderer!/raw foreignobject args)
  js-jsx-foreignobject-update-renderer)

;; jsx-foreignobject-update-size!/raw : external/raw any/c -> external/raw
;;   Update the foreign object size.
(define-jsx-alias (jsx-foreignobject-update-size!/raw foreignobject args)
  js-jsx-foreignobject-update-size)

;; jsx-foreignobject-update-span!/raw : external/raw any/c -> external/raw
;;   Update the foreign object span.
(define-jsx-alias (jsx-foreignobject-update-span!/raw foreignobject args)
  js-jsx-foreignobject-update-span)

;; jsx-foreignobject-H : jsx-element? any/c ... -> any/c
;;   Read the foreign object H helper.
(define (jsx-foreignobject-H foreignobject . args)
  (jsx-foreignobject-H/raw foreignobject (list->vector args)))

;; jsx-foreignobject-W : jsx-element? any/c ... -> any/c
;;   Read the foreign object W helper.
(define (jsx-foreignobject-W foreignobject . args)
  (jsx-foreignobject-W/raw foreignobject (list->vector args)))

;; jsx-foreignobject-has-point? : jsx-element? any/c ... -> boolean?
;;   Check whether a point hits the foreign object.
(define (jsx-foreignobject-has-point? foreignobject . args)
  (jsx-foreignobject-has-point?/raw foreignobject (list->vector args)))

;; jsx-foreignobject-set-size! : jsx-element? any/c ... -> void?
;;   Set the foreign object size directly.
(define (jsx-foreignobject-set-size! foreignobject . args)
  (jsx-foreignobject-set-size!/raw foreignobject (list->vector args))
  (void))

;; jsx-foreignobject-update! : jsx-element? any/c ... -> void?
;;   Update a foreign object.
(define (jsx-foreignobject-update! foreignobject . args)
  (jsx-foreignobject-update!/raw foreignobject (list->vector args))
  (void))

;; jsx-foreignobject-update-renderer! : jsx-element? any/c ... -> void?
;;   Refresh the foreign object renderer.
(define (jsx-foreignobject-update-renderer! foreignobject . args)
  (jsx-foreignobject-update-renderer!/raw foreignobject (list->vector args))
  (void))

;; jsx-foreignobject-update-size! : jsx-element? any/c ... -> void?
;;   Update the foreign object size.
(define (jsx-foreignobject-update-size! foreignobject . args)
  (jsx-foreignobject-update-size!/raw foreignobject (list->vector args))
  (void))

;; jsx-foreignobject-update-span! : jsx-element? any/c ... -> void?
;;   Update the foreign object span.
(define (jsx-foreignobject-update-span! foreignobject . args)
  (jsx-foreignobject-update-span!/raw foreignobject (list->vector args))
  (void))

;; jsx-tapemeasure-value/raw : external/raw any/c -> any/c
;;   Read the tapemeasure value.
(define-jsx-alias (jsx-tapemeasure-value/raw tapemeasure args)
  js-jsx-tapemeasure-Value)

;; jsx-tapemeasure-value : jsx-element? -> any/c
;;   Read the tapemeasure value.
(define (jsx-tapemeasure-value tapemeasure)
  (jsx-tapemeasure-value/raw tapemeasure (vector)))

;;; -------------------------------------------------------------------
;;; Geometry element helpers
;;; -------------------------------------------------------------------

;; jsx-element-get-attribute : any/c any/c -> any/c
;;   Read a geometry element attribute.
(define (jsx-element-get-attribute element key)
  (jsx-element-call element "getAttribute" (vector key)))

;; jsx-element-get-attributes : any/c -> any/c
;;   Read all geometry element attributes.
(define (jsx-element-get-attributes element)
  (jsx-element-call element "getAttributes" (vector)))

;; jsx-element-get-label-anchor : any/c -> any/c
;;   Read the label anchor for a geometry element.
(define (jsx-element-get-label-anchor element)
  (jsx-element-call element "getLabelAnchor" (vector)))

;; jsx-element-get-name : any/c -> any/c
;;   Read the element name.
(define (jsx-element-get-name element)
  (jsx-element-call element "getName" (vector)))

;; jsx-element-get-parents : any/c -> any/c
;;   Read the element parents.
(define (jsx-element-get-parents element)
  (jsx-element-call element "getParents" (vector)))

;; jsx-element-get-property : any/c any/c -> any/c
;;   Read a deprecated geometry element property.
(define (jsx-element-get-property element key)
  (jsx-element-call element "getProperty" (vector key)))

;; jsx-element-get-snap-sizes : any/c -> any/c
;;   Read the element snap sizes.
(define (jsx-element-get-snap-sizes element)
  (jsx-element-call element "getSnapSizes" (vector)))

;; jsx-element-get-text-anchor : any/c -> any/c
;;   Read the text anchor for a geometry element.
(define (jsx-element-get-text-anchor element)
  (jsx-element-call element "getTextAnchor" (vector)))

;; jsx-element-get-type : any/c -> any/c
;;   Read the element type.
(define (jsx-element-get-type element)
  (jsx-element-call element "getType" (vector)))

;; jsx-element-has-point? : any/c flonum? flonum? -> boolean?
;;   Check whether screen coordinates hit a geometry element.
(define (jsx-element-has-point? element x y)
  (jsx-element-call element "hasPoint" (vector x y)))

;; jsx-element-hide! : any/c -> void?
;;   Hide a geometry element.
(define (jsx-element-hide! element)
  (jsx-element-call/nullish element "hide" (vector)))

;; jsx-element-hide-element! : any/c -> void?
;;   Hide a geometry element using the alias method.
(define (jsx-element-hide-element! element)
  (jsx-element-call/nullish element "hideElement" (vector)))

;; jsx-element-no-highlight! : any/c -> void?
;;   Remove highlighting from a geometry element.
(define (jsx-element-no-highlight! element)
  (jsx-element-call/nullish element "noHighlight" (vector)))

;; jsx-element-prepare-update! : any/c -> void?
;;   Prepare a geometry element for update.
(define (jsx-element-prepare-update! element)
  (jsx-element-call/nullish element "prepareUpdate" (vector)))

;; jsx-element-remove! : any/c -> void?
;;   Remove a geometry element.
(define (jsx-element-remove! element)
  (jsx-element-call/nullish element "remove" (vector)))

;; jsx-element-remove-all-ticks! : any/c -> void?
;;   Remove all ticks from a geometry element.
(define (jsx-element-remove-all-ticks! element)
  (jsx-element-call/nullish element "removeAllTicks" (vector)))

;; jsx-element-remove-child! : any/c any/c -> void?
;;   Remove a dependent child from a geometry element.
(define (jsx-element-remove-child! element child)
  (jsx-element-call/nullish element "removeChild" (vector (jsx-unwrap child))))

;; jsx-element-remove-descendants! : any/c any/c -> void?
;;   Remove a descendant from a geometry element.
(define (jsx-element-remove-descendants! element obj)
  (jsx-element-call/nullish element "removeDescendants" (vector (jsx-unwrap obj))))

;; jsx-element-remove-event! : any/c procedure? -> void?
;;   Remove a geometry element event handler.
(define (jsx-element-remove-event! element handler)
  (jsx-element-call/nullish element "removeEvent"
                            (vector (procedure->external handler)))
  (void))

;; jsx-element-remove-ticks! : any/c any/c -> void?
;;   Remove ticks from a geometry element.
(define (jsx-element-remove-ticks! element tick)
  (jsx-element-call/nullish element "removeTicks" (vector tick)))

;; jsx-element-set-attribute! : any/c any/c -> void?
;;   Set geometry element attributes.
(define (jsx-element-set-attribute! element attributes)
  (jsx-element-call/nullish element "setAttribute" (vector attributes)))

;; jsx-element-set-label! : any/c any/c -> void?
;;   Set the geometry element label.
(define (jsx-element-set-label! element str)
  (jsx-element-call/nullish element "setLabel" (vector str)))

;; jsx-element-set-label-text! : any/c any/c -> void?
;;   Set the label text.
(define (jsx-element-set-label-text! element str)
  (jsx-element-call/nullish element "setLabelText" (vector str)))

;; jsx-element-set-name! : any/c any/c -> void?
;;   Set the geometry element name.
(define (jsx-element-set-name! element str)
  (jsx-element-call/nullish element "setName" (vector str)))

;; jsx-element-set-parents! : any/c any/c -> void?
;;   Set the geometry element parents.
(define (jsx-element-set-parents! element parents)
  (jsx-element-call/nullish element "setParents" (vector (jsx-unpack-array parents))))

;; jsx-element-set-position! : any/c any/c any/c -> void?
;;   Set the geometry element position.
(define (jsx-element-set-position! element method coords)
  (jsx-element-call/nullish element "setPosition" (vector method coords)))

;; jsx-element-set-position-directly! : any/c any/c any/c -> void?
;;   Set the geometry element position directly.
(define (jsx-element-set-position-directly! element method coords oldcoords)
  (jsx-element-call/nullish element "setPositionDirectly"
                            (vector method coords oldcoords)))

;; jsx-element-set-property! : any/c any/c -> void?
;;   Set a deprecated geometry element property.
(define (jsx-element-set-property! element attributes)
  (jsx-element-call/nullish element "setProperty" (vector attributes)))

;; jsx-element-show! : any/c -> void?
;;   Show a geometry element.
(define (jsx-element-show! element)
  (jsx-element-call/nullish element "show" (vector)))

;; jsx-element-show-element! : any/c -> void?
;;   Show a geometry element using the alias method.
(define (jsx-element-show-element! element)
  (jsx-element-call/nullish element "showElement" (vector)))

;; jsx-element-update! : any/c -> void?
;;   Update a geometry element.
(define (jsx-element-update! element)
  (jsx-element-call/nullish element "update" (vector)))

;; jsx-element-update-renderer! : any/c -> void?
;;   Update the geometry element renderer.
(define (jsx-element-update-renderer! element)
  (jsx-element-call/nullish element "updateRenderer" (vector)))

;; jsx-element-update-visibility! : any/c any/c -> void?
;;   Update the visibility of a geometry element.
(define (jsx-element-update-visibility! element parent-val)
  (jsx-element-call/nullish element "updateVisibility" (vector parent-val)))

;; jsx-element-use-locale! : any/c -> void?
;;   Enable locale-aware number formatting on a geometry element.
(define (jsx-element-use-locale! element)
  (jsx-element-call/nullish element "useLocale" (vector)))

;; jsx-element-add-child! : any/c any/c -> void?
;;   Add a dependent child to a geometry element.
(define (jsx-element-add-child! element obj)
  (jsx-element-call/nullish element "addChild" (vector (jsx-unwrap obj))))

;; jsx-element-add-descendants! : any/c any/c -> void?
;;   Add descendants to a geometry element.
(define (jsx-element-add-descendants! element obj)
  (jsx-element-call/nullish element "addDescendants" (vector (jsx-unwrap obj))))

;; jsx-element-add-parents! : any/c any/c -> void?
;;   Add parents to a geometry element.
(define (jsx-element-add-parents! element parents)
  (jsx-element-call/nullish element "addParents" (vector (jsx-unpack-array parents))))

;; jsx-element-add-parents-from-jc-functions! : any/c any/c -> void?
;;   Add parents derived from JC functions.
(define (jsx-element-add-parents-from-jc-functions! element function-array)
  (jsx-element-call/nullish element "addParentsFromJCFunctions"
                            (vector (jsx-unpack-array function-array))))

;; jsx-element-add-rotation! : any/c any/c -> void?
;;   Add a rotation to a geometry element.
(define (jsx-element-add-rotation! element angle)
  (jsx-element-call/nullish element "addRotation" (vector angle)))

;; jsx-element-add-ticks! : any/c any/c -> void?
;;   Add ticks to a geometry element.
(define (jsx-element-add-ticks! element ticks)
  (jsx-element-call/nullish element "addTicks" (vector (jsx-unwrap ticks))))

;; jsx-element-add-transform! : any/c any/c -> void?
;;   Add a transform to a geometry element.
(define (jsx-element-add-transform! element transform)
  (jsx-element-call/nullish element "addTransform" (vector (jsx-unwrap transform))))

;; jsx-element-animate! : any/c any/c any/c -> any/c
;;   Animate a geometry element.
(define (jsx-element-animate! element hash time options)
  (jsx-element-call element "animate" (vector hash time options)))

;; jsx-element-bounds : any/c -> any/c
;;   Read the element bounds.
(define (jsx-element-bounds element)
  (jsx-element-call element "bounds" (vector)))

;; jsx-element-clear-trace! : any/c -> void?
;;   Clear the trace of a geometry element.
(define (jsx-element-clear-trace! element)
  (jsx-element-call/nullish element "clearTrace" (vector)))

;; jsx-element-clone-to-background! : any/c -> any/c
;;   Clone a geometry element to the background.
(define (jsx-element-clone-to-background! element)
  (jsx-element-call element "cloneToBackground" (vector)))

;; jsx-element-count-children : any/c -> exact-nonnegative-integer?
;;   Count the direct children of a geometry element.
(define (jsx-element-count-children element)
  (jsx-element-call element "countChildren" (vector)))

;; jsx-element-create-gradient! : any/c -> void?
;;   Create a gradient for a geometry element.
(define (jsx-element-create-gradient! element)
  (jsx-element-call/nullish element "createGradient" (vector)))

;; jsx-element-create-label! : any/c -> void?
;;   Create a label for a geometry element.
(define (jsx-element-create-label! element)
  (jsx-element-call/nullish element "createLabel" (vector)))

;; jsx-element-draggable? : any/c -> any/c
;;   Read whether a geometry element is draggable.
(define (jsx-element-draggable? element)
  (jsx-element-call element "draggable" (vector)))

;; jsx-element-eval : any/c any/c -> any/c
;;   Evaluate a geometry-element-specific value.
(define (jsx-element-eval element val)
  (jsx-element-call element "eval" (vector val)))

;; jsx-element-eval-vis-prop : any/c any/c -> any/c
;;   Evaluate a visual property for a geometry element.
(define (jsx-element-eval-vis-prop element key)
  (jsx-element-call element "evalVisProp" (vector key)))

;; jsx-element-format-number-locale : any/c any/c any/c -> any/c
;;   Format a number using the element locale settings.
(define (jsx-element-format-number-locale element value digits)
  (jsx-element-call element "formatNumberLocale" (vector value digits)))

;; jsx-element-full-update! : any/c -> any/c
;;   Run the full update chain for a geometry element.
(define (jsx-element-full-update! element)
  (jsx-element-call element "fullUpdate" (vector)))

;; jsx-element-generate-polynomial : any/c -> any/c
;;   Generate the polynomial for a geometry element.
(define (jsx-element-generate-polynomial element)
  (jsx-element-call element "generatePolynomial" (vector)))

;; jsx-element-handle-snap-to-grid! : any/c any/c any/c -> any/c
;;   Handle snapping of a geometry element to the grid.
(define (jsx-element-handle-snap-to-grid! element force from-parent)
  (jsx-element-call element "handleSnapToGrid" (vector force from-parent)))

;; jsx-element-normalize! : any/c -> any/c
;;   Normalize a geometry element.
(define (jsx-element-normalize! element)
  (jsx-element-call element "normalize" (vector)))

;; jsx-element-resolve-shortcuts! : any/c any/c -> any/c
;;   Resolve geometry-element attribute shortcuts.
(define (jsx-element-resolve-shortcuts! element attributes)
  (jsx-element-call element "resolveShortcuts" (vector attributes)))

;; jsx-element-set-arrow! : any/c any/c any/c -> void?
;;   Set arrow flags on a geometry element.
(define (jsx-element-set-arrow! element first-arrow last-arrow)
  (jsx-element-call/nullish element "setArrow" (vector first-arrow last-arrow)))

;; jsx-element-set-dash! : any/c any/c -> void?
;;   Set the dash style on a geometry element.
(define (jsx-element-set-dash! element dash)
  (jsx-element-call/nullish element "setDash" (vector dash)))

;; jsx-element-set-display-rend-node! : any/c any/c -> void?
;;   Set the display renderer node on a geometry element.
(define (jsx-element-set-display-rend-node! element val)
  (jsx-element-call/nullish element "setDisplayRendNode" (vector val)))

;; jsx-element-snap-to-points! : any/c -> any/c
;;   Snap a geometry element to nearby points.
(define (jsx-element-snap-to-points! element)
  (jsx-element-call element "snapToPoints" (vector)))

;; jsx-board-update! : jsx-board? -> external/raw
;;   Update a board and redraw as needed.
(define (jsx-board-update! board)
  (jsx-board-update!/raw (jsx-board-raw board)))

;; jsx-board-full-update! : jsx-board? -> external/raw
;;   Force a full board update.
(define (jsx-board-full-update! board)
  (jsx-board-full-update!/raw (jsx-board-raw board)))

;; jsx-board-id : jsx-board? -> any/c
;;   Read the board id.
(define (jsx-board-id board)
  (js-jsx-board-id (jsx-board-raw board)))

;; jsx-board-container : jsx-board? -> any/c
;;   Read the board container element.
(define (jsx-board-container board)
  (js-jsx-board-container (jsx-board-raw board)))

;; jsx-board-renderer : jsx-board? -> any/c
;;   Read the renderer used by the board.
(define (jsx-board-renderer board)
  (js-jsx-board-renderer (jsx-board-raw board)))

;; jsx-board-canvas-width : jsx-board? -> exact-nonnegative-integer?
;;   Read the board canvas width.
(define (jsx-board-canvas-width board)
  (js-jsx-board-canvas-width (jsx-board-raw board)))

;; jsx-board-canvas-height : jsx-board? -> exact-nonnegative-integer?
;;   Read the board canvas height.
(define (jsx-board-canvas-height board)
  (js-jsx-board-canvas-height (jsx-board-raw board)))

;; jsx-board-bounding-box : jsx-board? -> vector?
;;   Read the board bounding box.
(define (jsx-board-bounding-box board)
  (js-array->vector (js-jsx-board-bounding-box (jsx-board-raw board))))

;; jsx-board-add-grid! : jsx-board? -> void?
;;   Add the default grid to the board.
(define (jsx-board-add-grid! board)
  (js-jsx-board-add-grid! (jsx-board-raw board))
  (void))

;; jsx-board-add-hook! : jsx-board? procedure? [string?] [any/c] -> exact-integer?
;;   Register a board hook and return its identifier.
(define (jsx-board-add-hook! board hook [m "update"] [context board])
  (js-jsx-board-add-hook! (jsx-board-raw board) hook m (jsx-unwrap context)))

;; jsx-board-add-keyboard-event-handlers! : jsx-board? -> void?
;;   Register keyboard event handlers.
(define (jsx-board-add-keyboard-event-handlers! board)
  (js-jsx-board-add-keyboard-event-handlers! (jsx-board-raw board))
  (void))

;; jsx-board-add-mouse-event-handlers! : jsx-board? -> void?
;;   Register mouse event handlers.
(define (jsx-board-add-mouse-event-handlers! board)
  (js-jsx-board-add-mouse-event-handlers! (jsx-board-raw board))
  (void))

;; jsx-board-add-pointer-event-handlers! : jsx-board? -> void?
;;   Register pointer event handlers.
(define (jsx-board-add-pointer-event-handlers! board)
  (js-jsx-board-add-pointer-event-handlers! (jsx-board-raw board))
  (void))

;; jsx-board-add-resize-event-handlers! : jsx-board? -> void?
;;   Register resize event handlers.
(define (jsx-board-add-resize-event-handlers! board)
  (js-jsx-board-add-resize-event-handlers! (jsx-board-raw board))
  (void))

;; jsx-board-add-touch-event-handlers! : jsx-board? any/c -> void?
;;   Register touch event handlers.
(define (jsx-board-add-touch-event-handlers! board apple-gestures)
  (js-jsx-board-add-touch-event-handlers! (jsx-board-raw board) apple-gestures)
  (void))

;; jsx-board-add-wheel-event-handlers! : jsx-board? -> void?
;;   Register wheel event handlers.
(define (jsx-board-add-wheel-event-handlers! board)
  (js-jsx-board-add-wheel-event-handlers! (jsx-board-raw board))
  (void))

;; jsx-board-add-fullscreen-event-handlers! : jsx-board? -> void?
;;   Register fullscreen event handlers.
(define (jsx-board-add-fullscreen-event-handlers! board)
  (js-jsx-board-add-fullscreen-event-handlers! (jsx-board-raw board))
  (void))

;; jsx-board-add-event-handlers! : jsx-board? -> void?
;;   Register all board event handlers.
(define (jsx-board-add-event-handlers! board)
  (js-jsx-board-add-event-handlers! (jsx-board-raw board))
  (void))

;; jsx-board-add-event! : jsx-board? (or/c string? symbol?) procedure? -> void?
;;   Register a JSXGraph board event handler.
(define (jsx-board-add-event! board event handler)
  (define event* (jsx-key->string event))
  (define handler* (procedure->external handler))
  (js-jsx-board-on (jsx-board-raw board) event* handler*)
  (void))

;; jsx-board-add-child! : jsx-board? jsx-board? -> void?
;;   Register a dependent board.
(define (jsx-board-add-child! board child)
  (js-jsx-board-add-child! (jsx-board-raw board) (jsx-board-raw child))
  (void))

;; jsx-wrap-board-object : external/raw -> (or/c jsx-point? jsx-element?)
;;   Wrap a board object using the most specific checked wrapper.
(define (jsx-wrap-board-object raw)
  (cond
    [(jsx-point?/raw raw) (jsx-wrap-point raw)]
    [else                 (jsx-wrap-element raw)]))

;; jsx-board-count-children : jsx-board? -> number?
;;   Count the direct children on a board.
(define (jsx-board-count-children board)
  (vector-length (jsx-board-objects-list board)))

;; jsx-board-num-objects : jsx-board? -> exact-nonnegative-integer?
;;   Read the total number of objects ever created on a board.
(define (jsx-board-num-objects board)
  (js-jsx-board-num-objects (jsx-board-raw board)))

;; jsx-board-get-mouse-position : jsx-board? any/c [exact-integer?] -> vector?
;;   Read the mouse position in screen coordinates.
(define (jsx-board-get-mouse-position board evt [i #f])
  (if i
      (js-array->vector
       (js-jsx-board-get-mouse-position/index (jsx-board-raw board) evt i))
      (js-array->vector
       (js-jsx-board-get-mouse-position (jsx-board-raw board) evt))))

;; jsx-board-get-usr-coords-of-mouse : jsx-board? any/c -> vector?
;;   Read the mouse position in user coordinates.
(define (jsx-board-get-usr-coords-of-mouse board evt)
  (js-array->vector
   (js-jsx-board-get-usr-coords-of-mouse (jsx-board-raw board) evt)))

;; jsx-board-get-coords-top-left-corner : jsx-board? -> vector?
;;   Read the board coordinates of the top-left corner.
(define (jsx-board-get-coords-top-left-corner board)
  (js-array->vector
   (js-jsx-board-get-coords-top-left-corner (jsx-board-raw board))))

;; jsx-board-get-bounding-box : jsx-board? -> vector?
;;   Read the board bounding box using the board method.
(define (jsx-board-get-bounding-box board)
  (js-array->vector
   (js-jsx-board-get-bounding-box (jsx-board-raw board))))

;; jsx-board-get-scr-coords-of-mouse : jsx-board? any/c any/c -> vector?
;;   Read screen coordinates using the board method.
(define (jsx-board-get-scr-coords-of-mouse board x y)
  (js-array->vector
   (js-jsx-board-get-scr-coords-of-mouse (jsx-board-raw board) x y)))

;; jsx-board-get-all-objects-under-mouse : jsx-board? any/c -> vector?
;;   Collect the objects under a pointer event.
(define (jsx-board-get-all-objects-under-mouse board evt)
  (js-array->vector
   (js-jsx-board-get-all-objects-under-mouse (jsx-board-raw board) evt)))

;; jsx-board-get-all-under-mouse : jsx-board? any/c -> vector?
;;   Collect the objects and user coordinates under a pointer event.
(define (jsx-board-get-all-under-mouse board evt)
  (js-array->vector
   (js-jsx-board-get-all-under-mouse (jsx-board-raw board) evt)))

;; jsx-board-has-point? : jsx-board? flonum? flonum? -> boolean?
;;   Check whether a point lies inside the board bounding box.
(define (jsx-board-has-point? board x y)
  (js-jsx-board-has-point! (jsx-board-raw board) x y))

;; jsx-board-add-animation! : jsx-board? any/c -> void?
;;   Register an animated element with the board.
(define (jsx-board-add-animation! board element)
  (js-jsx-board-add-animation! (jsx-board-raw board) (jsx-unwrap element))
  (void))

;; jsx-board-add-conditions! : jsx-board? string? -> void?
;;   Add conditional updates to the board.
(define (jsx-board-add-conditions! board str)
  (js-jsx-board-add-conditions! (jsx-board-raw board) str)
  (void))

;; jsx-board-apply-zoom! : jsx-board? -> void?
;;   Apply the current zoom factors to all objects.
(define (jsx-board-apply-zoom! board)
  (js-jsx-board-apply-zoom! (jsx-board-raw board))
  (void))

;; jsx-board-calculate-snap-sizes! : jsx-board? -> void?
;;   Recompute the point snap sizes on the board.
(define (jsx-board-calculate-snap-sizes! board)
  (js-jsx-board-calculate-snap-sizes! (jsx-board-raw board))
  (void))

;; jsx-board-create-roulette! : jsx-board? any/c any/c any/c any/c any/c any/c any/c -> any/c
;;   Create a roulette animation on the board.
(define (jsx-board-create-roulette! board c1 c2 start-c1 stepsize direction time pointlist)
  (js-jsx-board-create-roulette! (jsx-board-raw board)
                                 (jsx-unwrap c1)
                                 (jsx-unwrap c2)
                                 start-c1
                                 stepsize
                                 direction
                                 time
                                 (jsx-unpack-array pointlist)))

;; jsx-board-objects-list : jsx-board? -> vector?
;;   Read the board objects in construction order.
(define (jsx-board-objects-list board)
  (define objects (js-jsx-board-objects-list (jsx-board-raw board)))
  (for/vector #:length (js-array-length objects)
              ([obj (in-vector (js-array->vector objects))])
    (jsx-wrap-board-object obj)))

;; jsx-board-highlight-custom-infobox! : jsx-board? string? [any/c] -> void?
;;   Change the board infobox text.
(define (jsx-board-highlight-custom-infobox! board text [el #f])
  (if el
      (js-jsx-board-highlight-custom-infobox/element! (jsx-board-raw board) text el)
      (js-jsx-board-highlight-custom-infobox! (jsx-board-raw board) text))
  (void))

;; jsx-board-highlight-infobox! : jsx-board? any/c any/c [any/c] -> void?
;;   Show the given coordinates in the board infobox.
(define (jsx-board-highlight-infobox! board x y [el #f])
  (if el
      (js-jsx-board-highlight-infobox/element! (jsx-board-raw board) x y el)
      (js-jsx-board-highlight-infobox! (jsx-board-raw board) x y))
  (void))

;; jsx-board-move-object! : jsx-board? any/c any/c any/c any/c any/c -> void?
;;   Move a board object.
(define (jsx-board-move-object! board x y o evt type)
  (js-jsx-board-move-object! (jsx-board-raw board) x y (jsx-unwrap o) evt type)
  (void))

;; jsx-board-move-origin! : jsx-board? any/c any/c any/c -> void?
;;   Move the board origin.
(define (jsx-board-move-origin! board x y diff)
  (js-jsx-board-move-origin! (jsx-board-raw board) x y diff)
  (void))

;; jsx-board-finalize-adding! : jsx-board? any/c -> void?
;;   Finalize a newly added board object.
(define (jsx-board-finalize-adding! board obj)
  (js-jsx-board-finalize-adding! (jsx-board-raw board) (jsx-unwrap obj))
  (void))

;; jsx-board-set-attribute! : jsx-board? any/c -> void?
;;   Set board attributes.
(define (jsx-board-set-attribute! board attributes)
  (js-jsx-board-set-attribute! (jsx-board-raw board) attributes)
  (void))

;; jsx-board-set-bounding-box! : jsx-board? any/c any/c any/c -> void?
;;   Set the board bounding box.
(define (jsx-board-set-bounding-box! board bbox keepaspectratio setZoom)
  (js-jsx-board-set-bounding-box! (jsx-board-raw board) bbox keepaspectratio setZoom)
  (void))

;; jsx-board-set-zoom! : jsx-board? any/c any/c -> void?
;;   Set the board zoom.
(define (jsx-board-set-zoom! board fX fY)
  (js-jsx-board-set-zoom! (jsx-board-raw board) fX fY)
  (void))

;; jsx-board-resize-container! : jsx-board? any/c any/c any/c any/c -> void?
;;   Resize the board container.
(define (jsx-board-resize-container! board canvasWidth canvasHeight dontset dontSetBoundingBox)
  (js-jsx-board-resize-container! (jsx-board-raw board) canvasWidth canvasHeight dontset dontSetBoundingBox)
  (void))

;; jsx-board-remove-grids! : jsx-board? -> void?
;;   Remove all grids from the board.
(define (jsx-board-remove-grids! board)
  (js-jsx-board-remove-grids! (jsx-board-raw board))
  (void))

;; jsx-board-remove-hook! : jsx-board? exact-integer? -> void?
;;   Remove a board hook.
(define (jsx-board-remove-hook! board id)
  (js-jsx-board-remove-hook! (jsx-board-raw board) id)
  (void))

;; jsx-board-remove-event! : jsx-board? (or/c string? symbol?) procedure? -> void?
;;   Remove a JSXGraph board event handler.
(define (jsx-board-remove-event! board event handler)
  (define event* (jsx-key->string event))
  (define handler* (procedure->external handler))
  (js-jsx-board-off (jsx-board-raw board) event* handler*)
  (void))

;; jsx-board-remove-keyboard-event-handlers! : jsx-board? -> void?
;;   Remove keyboard event handlers.
(define (jsx-board-remove-keyboard-event-handlers! board)
  (js-jsx-board-remove-keyboard-event-handlers! (jsx-board-raw board))
  (void))

;; jsx-board-remove-mouse-event-handlers! : jsx-board? -> void?
;;   Remove mouse event handlers.
(define (jsx-board-remove-mouse-event-handlers! board)
  (js-jsx-board-remove-mouse-event-handlers! (jsx-board-raw board))
  (void))

;; jsx-board-remove-pointer-event-handlers! : jsx-board? -> void?
;;   Remove pointer event handlers.
(define (jsx-board-remove-pointer-event-handlers! board)
  (js-jsx-board-remove-pointer-event-handlers! (jsx-board-raw board))
  (void))

;; jsx-board-remove-resize-event-handlers! : jsx-board? -> void?
;;   Remove resize event handlers.
(define (jsx-board-remove-resize-event-handlers! board)
  (js-jsx-board-remove-resize-event-handlers! (jsx-board-raw board))
  (void))

;; jsx-board-remove-event-handlers! : jsx-board? -> void?
;;   Remove all board event handlers.
(define (jsx-board-remove-event-handlers! board)
  (js-jsx-board-remove-event-handlers! (jsx-board-raw board))
  (void))

;; jsx-board-remove-touch-event-handlers! : jsx-board? -> void?
;;   Remove touch event handlers.
(define (jsx-board-remove-touch-event-handlers! board)
  (js-jsx-board-remove-touch-event-handlers! (jsx-board-raw board))
  (void))

;; jsx-board-select : jsx-board? any/c [boolean?] -> (or/c jsx-point? jsx-element?)
;;   Select one or more objects on the board.
(define (jsx-board-select board str [only-by-id-or-name #f])
  (jsx-wrap-board-object
   (js-jsx-board-select (jsx-board-raw board) str only-by-id-or-name)))

;; jsx-board-zoom100! : jsx-board? -> void?
;;   Reset the board zoom to 100%.
(define (jsx-board-zoom100! board)
  (js-jsx-board-zoom100! (jsx-board-raw board))
  (void))

;; jsx-board-zoom-all-points! : jsx-board? -> void?
;;   Zoom so every visible point fits in the viewport.
(define (jsx-board-zoom-all-points! board)
  (js-jsx-board-zoom-all-points! (jsx-board-raw board))
  (void))

;; jsx-board-zoom-in! : jsx-board? any/c any/c -> void?
;;   Zoom in around a point.
(define (jsx-board-zoom-in! board x y)
  (js-jsx-board-zoom-in! (jsx-board-raw board) x y)
  (void))

;; jsx-board-zoom-out! : jsx-board? any/c any/c -> void?
;;   Zoom out around a point.
(define (jsx-board-zoom-out! board x y)
  (js-jsx-board-zoom-out! (jsx-board-raw board) x y)
  (void))

;; jsx-board-start-selection-mode! : jsx-board? -> void?
;;   Enable board selection mode.
(define (jsx-board-start-selection-mode! board)
  (js-jsx-board-start-selection-mode! (jsx-board-raw board))
  (void))

;; jsx-board-stop-selection-mode! : jsx-board? -> void?
;;   Disable board selection mode.
(define (jsx-board-stop-selection-mode! board)
  (js-jsx-board-stop-selection-mode! (jsx-board-raw board))
  (void))

;; jsx-board-stop-all-animation! : jsx-board? -> void?
;;   Stop all running animations on a board.
(define (jsx-board-stop-all-animation! board)
  (js-jsx-board-stop-all-animation! (jsx-board-raw board))
  (void))

;; jsx-board-clear-traces! : jsx-board? -> void?
;;   Clear all traces from the board.
(define (jsx-board-clear-traces! board)
  (js-jsx-board-clear-traces! (jsx-board-raw board))
  (void))

;; jsx-board-dehighlight-all! : jsx-board? -> void?
;;   Remove highlighting from all elements.
(define (jsx-board-dehighlight-all! board)
  (js-jsx-board-dehighlight-all! (jsx-board-raw board))
  (void))

;; jsx-board-update-coords! : jsx-board? -> void?
;;   Update the coordinates of all elements that need them.
(define (jsx-board-update-coords! board)
  (js-jsx-board-update-coords! (jsx-board-raw board))
  (void))

;; jsx-board-update-container-dims! : jsx-board? [any/c] [any/c] -> void?
;;   Update the board container dimensions.
(define (jsx-board-update-container-dims! board [width #f] [height #f])
  (if (and (eq? width #f) (eq? height #f))
      (js-jsx-board-update-container-dims! (jsx-board-raw board))
      (js-jsx-board-update-container-dims/size! (jsx-board-raw board) width height))
  (void))

;; jsx-board-update-csstransforms! : jsx-board? -> void?
;;   Refresh CSS transforms.
(define (jsx-board-update-csstransforms! board)
  (js-jsx-board-update-csstransforms! (jsx-board-raw board))
  (void))

;; jsx-board-update-elements! : jsx-board? any/c -> void?
;;   Update the board elements.
(define (jsx-board-update-elements! board drag)
  (js-jsx-board-update-elements! (jsx-board-raw board) drag)
  (void))

;; jsx-board-update-hooks! : jsx-board? any/c -> void?
;;   Run hooked board callbacks.
(define (jsx-board-update-hooks! board m)
  (js-jsx-board-update-hooks! (jsx-board-raw board) m)
  (void))

;; jsx-board-update-conditions! : jsx-board? -> void?
;;   Update all conditional board elements.
(define (jsx-board-update-conditions! board)
  (js-jsx-board-update-conditions! (jsx-board-raw board))
  (void))

;; jsx-board-suppress-default! : jsx-board? any/c -> void?
;;   Suppress the default event action.
(define (jsx-board-suppress-default! board e)
  (js-jsx-board-suppress-default! (jsx-board-raw board) e)
  (void))

;; jsx-board-init-infobox! : jsx-board? any/c -> void?
;;   Initialize the board infobox.
(define (jsx-board-init-infobox! board attributes)
  (js-jsx-board-init-infobox! (jsx-board-raw board) attributes)
  (void))

;; jsx-board-init-move-object! : jsx-board? any/c any/c any/c any/c -> void?
;;   Prepare a board object move.
(define (jsx-board-init-move-object! board x y evt type)
  (js-jsx-board-init-move-object! (jsx-board-raw board) x y evt type)
  (void))

;; jsx-board-init-move-origin! : jsx-board? any/c any/c -> void?
;;   Prepare moving the board origin.
(define (jsx-board-init-move-origin! board x y)
  (js-jsx-board-init-move-origin! (jsx-board-raw board) x y)
  (void))

;; jsx-board-show-dependencies! : jsx-board? -> void?
;;   Show the dependency graph for the board.
(define (jsx-board-show-dependencies! board)
  (js-jsx-board-show-dependencies! (jsx-board-raw board))
  (void))

;; jsx-board-generate-id : jsx-board? -> string?
;;   Generate a fresh board id.
(define (jsx-board-generate-id board)
  (js-jsx-board-generate-id (jsx-board-raw board)))

;; jsx-board-generate-name : jsx-board? any/c -> string?
;;   Generate a fresh board object name.
(define (jsx-board-generate-name board object)
  (js-jsx-board-generate-name (jsx-board-raw board) (jsx-unwrap object)))

;; jsx-board-init-geonext-board! : jsx-board? -> void?
;;   Initialize the default GEONExT board objects.
(define (jsx-board-init-geonext-board! board)
  (js-jsx-board-init-geonext-board! (jsx-board-raw board))
  (void))

;; jsx-board-show-xml! : jsx-board? -> void?
;;   Show the board XML in a separate window.
(define (jsx-board-show-xml! board)
  (js-jsx-board-show-xml! (jsx-board-raw board))
  (void))

;; jsx-board-to-fullscreen! : jsx-board? any/c -> void?
;;   Expand the board to fullscreen.
(define (jsx-board-to-fullscreen! board id)
  (js-jsx-board-to-fullscreen! (jsx-board-raw board) id)
  (void))

;; jsx-board-start-resize-observer! : jsx-board? -> void?
;;   Start watching the container size.
(define (jsx-board-start-resize-observer! board)
  (js-jsx-board-start-resize-observer! (jsx-board-raw board))
  (void))

;; jsx-board-stop-resize-observer! : jsx-board? -> void?
;;   Stop watching the container size.
(define (jsx-board-stop-resize-observer! board)
  (js-jsx-board-stop-resize-observer! (jsx-board-raw board))
  (void))

;; jsx-board-start-intersection-observer! : jsx-board? -> void?
;;   Start watching whether the board is visible.
(define (jsx-board-start-intersection-observer! board)
  (js-jsx-board-start-intersection-observer! (jsx-board-raw board))
  (void))

;; jsx-board-stop-intersection-observer! : jsx-board? -> void?
;;   Stop watching board visibility.
(define (jsx-board-stop-intersection-observer! board)
  (js-jsx-board-stop-intersection-observer! (jsx-board-raw board))
  (void))

;; jsx-board-update-infobox! : jsx-board? any/c -> void?
;;   Update the board infobox for a geometry element.
(define (jsx-board-update-infobox! board el)
  (js-jsx-board-update-infobox! (jsx-board-raw board) (jsx-unwrap el))
  (void))

;; jsx-board-set-id : jsx-board? any/c any/c -> string?
;;   Compose a unique id for an element on the board.
(define (jsx-board-set-id board obj type)
  (js-jsx-board-set-id (jsx-board-raw board) (jsx-unwrap obj) type))

;; jsx-board-update-renderer! : jsx-board? -> void?
;;   Refresh the board renderer.
(define (jsx-board-update-renderer! board)
  (js-jsx-board-update-renderer! (jsx-board-raw board))
  (void))

;; jsx-board-update-renderer-canvas! : jsx-board? -> void?
;;   Refresh the board renderer in Canvas mode.
(define (jsx-board-update-renderer-canvas! board)
  (js-jsx-board-update-renderer-canvas! (jsx-board-raw board))
  (void))

;; jsx-board-zoom-elements! : jsx-board? any/c -> void?
;;   Zoom the board so a set of elements fits in the viewport.
(define (jsx-board-zoom-elements! board elements)
  (js-jsx-board-zoom-elements! (jsx-board-raw board) (jsx-unpack-array elements))
  (void))

;; jsx-board-remove-object! : jsx-board? any/c -> void?
;;   Remove an object from a board.
(define (jsx-board-remove-object! board object)
  (jsx-board-remove-object!/raw (jsx-board-raw board) (jsx-unwrap object))
  (void))

;; jsx-board-suspend-update! : jsx-board? -> external/raw
;;   Suspend automatic board updates.
(define (jsx-board-suspend-update! board)
  (jsx-board-suspend-update!/raw (jsx-board-raw board)))

;; jsx-board-unsuspend-update! : jsx-board? -> external/raw
;;   Resume automatic board updates.
(define (jsx-board-unsuspend-update! board)
  (jsx-board-unsuspend-update!/raw (jsx-board-raw board)))

;; jsx-point? : any/c -> boolean?
;;   Test whether a value is a JSXGraph point.
(define (jsx-point? x)
  (jsx-point?/raw x))

;; jsx-point-attractor-distance : jsx-point? -> flonum?
;;   Read the point's attractor distance.
(define (jsx-point-attractor-distance point)
  (jsx-point-attractor-distance/raw (jsx-point-raw point)))

;; jsx-set-point-attractor-distance! : jsx-point? flonum? -> void?
;;   Set the point's attractor distance.
(define (jsx-set-point-attractor-distance! point distance)
  (jsx-set-point-attractor-distance!/raw (jsx-point-raw point) distance)
  (void))

;; jsx-point-attractors : jsx-point? -> external/raw
;;   Read the point's attractors.
(define (jsx-point-attractors point)
  (jsx-point-attractors/raw (jsx-point-raw point)))

;; jsx-set-point-attractors! : jsx-point? external/raw -> void?
;;   Set the point's attractors.
(define (jsx-set-point-attractors! point attractors)
  (jsx-set-point-attractors!/raw (jsx-point-raw point) (jsx-unwrap attractors))
  (void))

;; jsx-point-attractor-unit : jsx-point? -> string?
;;   Read the point's attractor unit.
(define (jsx-point-attractor-unit point)
  (jsx-point-attractor-unit/raw (jsx-point-raw point)))

;; jsx-set-point-attractor-unit! : jsx-point? string? -> void?
;;   Set the point's attractor unit.
(define (jsx-set-point-attractor-unit! point unit)
  (jsx-set-point-attractor-unit!/raw (jsx-point-raw point) unit)
  (void))

;; jsx-point-attract-to-grid : jsx-point? -> boolean?
;;   Read whether the point attracts to grid.
(define (jsx-point-attract-to-grid point)
  (jsx-point-attract-to-grid/raw (jsx-point-raw point)))

;; jsx-set-point-attract-to-grid! : jsx-point? boolean? -> void?
;;   Enable or disable grid attraction.
(define (jsx-set-point-attract-to-grid! point enable)
  (jsx-set-point-attract-to-grid!/raw (jsx-point-raw point) enable)
  (void))

;; jsx-point-face : jsx-point? -> string?
;;   Read the point face.
(define (jsx-point-face point)
  (jsx-point-face/raw (jsx-point-raw point)))

;; jsx-set-point-face! : jsx-point? string? -> void?
;;   Set the point face.
(define (jsx-set-point-face! point face)
  (jsx-set-point-face!/raw (jsx-point-raw point) face)
  (void))

;; jsx-point-ignored-snap-to-points : jsx-point? -> external/raw
;;   Read the point's ignored snap-to points.
(define (jsx-point-ignored-snap-to-points point)
  (jsx-point-ignored-snap-to-points/raw (jsx-point-raw point)))

;; jsx-set-point-ignored-snap-to-points! : jsx-point? external/raw -> void?
;;   Set the point's ignored snap-to points.
(define (jsx-set-point-ignored-snap-to-points! point points)
  (jsx-set-point-ignored-snap-to-points!/raw (jsx-point-raw point)
                                              (jsx-unwrap points))
  (void))

;; jsx-point-infobox-digits : jsx-point? -> exact-integer?
;;   Read the number of infobox digits.
(define (jsx-point-infobox-digits point)
  (jsx-point-infobox-digits/raw (jsx-point-raw point)))

;; jsx-set-point-infobox-digits! : jsx-point? exact-integer? -> void?
;;   Set the number of infobox digits.
(define (jsx-set-point-infobox-digits! point digits)
  (jsx-set-point-infobox-digits!/raw (jsx-point-raw point) digits)
  (void))

;; jsx-point-show-infobox : jsx-point? -> boolean?
;;   Read whether the infobox is shown.
(define (jsx-point-show-infobox point)
  (jsx-point-show-infobox/raw (jsx-point-raw point)))

;; jsx-set-point-show-infobox! : jsx-point? boolean? -> void?
;;   Enable or disable the point infobox.
(define (jsx-set-point-show-infobox! point show?)
  (jsx-set-point-show-infobox!/raw (jsx-point-raw point) show?)
  (void))

;; jsx-point-size : jsx-point? -> flonum?
;;   Read the point size.
(define (jsx-point-size point)
  (jsx-point-size/raw (jsx-point-raw point)))

;; jsx-set-point-size! : jsx-point? flonum? -> void?
;;   Set the point size.
(define (jsx-set-point-size! point size)
  (jsx-set-point-size!/raw (jsx-point-raw point) size)
  (void))

;; jsx-point-size-unit : jsx-point? -> string?
;;   Read the point size unit.
(define (jsx-point-size-unit point)
  (jsx-point-size-unit/raw (jsx-point-raw point)))

;; jsx-set-point-size-unit! : jsx-point? string? -> void?
;;   Set the point size unit.
(define (jsx-set-point-size-unit! point unit)
  (jsx-set-point-size-unit!/raw (jsx-point-raw point) unit)
  (void))

;; jsx-point-snap-size-x : jsx-point? -> flonum?
;;   Read the x snap size.
(define (jsx-point-snap-size-x point)
  (jsx-point-snap-size-x/raw (jsx-point-raw point)))

;; jsx-set-point-snap-size-x! : jsx-point? flonum? -> void?
;;   Set the x snap size.
(define (jsx-set-point-snap-size-x! point step)
  (jsx-set-point-snap-size-x!/raw (jsx-point-raw point) step)
  (void))

;; jsx-point-snap-size-y : jsx-point? -> flonum?
;;   Read the y snap size.
(define (jsx-point-snap-size-y point)
  (jsx-point-snap-size-y/raw (jsx-point-raw point)))

;; jsx-set-point-snap-size-y! : jsx-point? flonum? -> void?
;;   Set the y snap size.
(define (jsx-set-point-snap-size-y! point step)
  (jsx-set-point-snap-size-y!/raw (jsx-point-raw point) step)
  (void))

;; jsx-point-snap-to-grid : jsx-point? -> boolean?
;;   Read whether grid snapping is enabled.
(define (jsx-point-snap-to-grid point)
  (jsx-point-snap-to-grid/raw (jsx-point-raw point)))

;; jsx-set-point-snap-to-grid! : jsx-point? boolean? -> void?
;;   Enable or disable grid snapping.
(define (jsx-set-point-snap-to-grid! point enable)
  (jsx-set-point-snap-to-grid!/raw (jsx-point-raw point) enable)
  (void))

;; jsx-point-snap-to-points : jsx-point? -> boolean?
;;   Read whether point snapping is enabled.
(define (jsx-point-snap-to-points point)
  (jsx-point-snap-to-points/raw (jsx-point-raw point)))

;; jsx-set-point-snap-to-points! : jsx-point? boolean? -> void?
;;   Enable or disable point snapping.
(define (jsx-set-point-snap-to-points! point enable)
  (jsx-set-point-snap-to-points!/raw (jsx-point-raw point) enable)
  (void))

;; jsx-point-snatch-distance : jsx-point? -> flonum?
;;   Read the snap distance threshold.
(define (jsx-point-snatch-distance point)
  (jsx-point-snatch-distance/raw (jsx-point-raw point)))

;; jsx-set-point-snatch-distance! : jsx-point? flonum? -> void?
;;   Set the snap distance threshold.
(define (jsx-set-point-snatch-distance! point distance)
  (jsx-set-point-snatch-distance!/raw (jsx-point-raw point) distance)
  (void))

;; jsx-point-style : jsx-point? -> exact-integer?
;;   Read the point style.
(define (jsx-point-style point)
  (jsx-point-style/raw (jsx-point-raw point)))

;; jsx-set-point-style! : jsx-point? exact-integer? -> void?
;;   Set the point style.
(define (jsx-set-point-style! point style)
  (jsx-set-point-style!/raw (jsx-point-raw point) style)
  (void))

;; jsx-point-x : jsx-point? -> flonum?
;;   Read the x coordinate of a point.
(define (jsx-point-x point)
  (jsx-point-x/raw (jsx-point-raw point)))

;; jsx-point-y : jsx-point? -> flonum?
;;   Read the y coordinate of a point.
(define (jsx-point-y point)
  (jsx-point-y/raw (jsx-point-raw point)))

;; jsx-point-zoom : jsx-point? -> boolean?
;;   Read whether point size scales with zoom.
(define (jsx-point-zoom point)
  (jsx-point-zoom/raw (jsx-point-raw point)))

;; jsx-set-point-zoom! : jsx-point? boolean? -> void?
;;   Enable or disable zoom scaling.
(define (jsx-set-point-zoom! point enable)
  (jsx-set-point-zoom!/raw (jsx-point-raw point) enable)
  (void))

;; jsx-point-has-point : jsx-point? flonum? flonum? -> boolean?
;;   Check whether screen coordinates hit the point.
(define (jsx-point-has-point point x y)
  (jsx-point-has-point/raw (jsx-point-raw point) x y))

;; jsx-point-is-on : jsx-point? external/raw flonum? -> boolean?
;;   Check whether the point lies on another element.
(define (jsx-point-is-on point element tol)
  (jsx-point-is-on/raw (jsx-point-raw point) (jsx-unwrap element) tol))

;; jsx-point-make-intersection! : jsx-point? external/raw external/raw exact-integer? exact-integer? -> void?
;;   Convert a point into an intersection definition.
(define (jsx-point-make-intersection! point el1 el2 i j)
  (jsx-point-make-intersection!/raw (jsx-point-raw point)
                                    (jsx-unwrap el1)
                                    (jsx-unwrap el2)
                                    i
                                    j)
  (void))

;; jsx-point-normalize-face : jsx-point? string? -> string?
;;   Normalize a point face token.
(define (jsx-point-normalize-face point face)
  (jsx-point-normalize-face/raw (jsx-point-raw point) face))

;; jsx-point-set-style! : jsx-point? exact-integer? -> void?
;;   Set the point style directly.
(define (jsx-point-set-style! point style)
  (jsx-point-set-style!/raw (jsx-point-raw point) style)
  (void))

;; jsx-point-update! : jsx-point? exact-integer? -> void?
;;   Update point state.
(define (jsx-point-update! point finalize)
  (jsx-point-update!/raw (jsx-point-raw point) finalize)
  (void))

;; jsx-point-update-renderer! : jsx-point? -> void?
;;   Refresh the point renderer.
(define (jsx-point-update-renderer! point)
  (jsx-point-update-renderer!/raw (jsx-point-raw point))
  (void))

;; jsx-point-update-transform! : jsx-point? exact-integer? -> external/raw
;;   Apply transformations to the point base element.
(define (jsx-point-update-transform! point finalize)
  (jsx-point-update-transform!/raw (jsx-point-raw point) finalize))
