#lang webracket

;;;
;;; JSXGraph helpers
;;;

;; This library provides a small Rackety layer on top of the low-level
;; `js-jsx-*` FFI bindings.  The public `jsx-*` names are intended for
;; everyday JSXGraph work: creating boards, building geometry objects,
;; and querying or updating point state.

;;; -------------------------------------------------------------------
;;; Low-level aliases
;;; -------------------------------------------------------------------

;; define-jsx-alias : (name arg ...) foreign-name -> syntax
;;   Define a wrapper that forwards directly to a foreign binding.
(define-syntax-rule (define-jsx-alias (name arg ...) ffi)
  (define (name arg ...) (ffi arg ...)))

;; jsx-init-board : string? any/c -> external/raw
;;   Create a JSXGraph board with explicit attributes.
(define-jsx-alias (jsx-init-board container-id attrs) js-jsx-init-board)

;; jsx-board-create : external/raw string? any/c any/c -> external/raw
;;   Create an arbitrary JSXGraph element on a board.
(define-jsx-alias (jsx-board-create board element-type parents attrs)
  js-jsx-board-create)

;; jsx-board-create-point : external/raw any/c any/c -> external/raw
;;   Create a point on a board.
(define-jsx-alias (jsx-board-create-point board parents attrs)
  js-jsx-board-create-point)

;; jsx-board-create-line : external/raw any/c any/c -> external/raw
;;   Create a line on a board.
(define-jsx-alias (jsx-board-create-line board parents attrs)
  js-jsx-board-create-line)

;; jsx-board-create-segment : external/raw any/c any/c -> external/raw
;;   Create a segment on a board.
(define-jsx-alias (jsx-board-create-segment board parents attrs)
  js-jsx-board-create-segment)

;; jsx-board-create-circle : external/raw any/c any/c -> external/raw
;;   Create a circle on a board.
(define-jsx-alias (jsx-board-create-circle board parents attrs)
  js-jsx-board-create-circle)

;; jsx-board-create-intersection : external/raw any/c any/c -> external/raw
;;   Create an intersection point on a board.
(define-jsx-alias (jsx-board-create-intersection board parents attrs)
  js-jsx-board-create-intersection)

;; jsx-board-create-text : external/raw any/c any/c -> external/raw
;;   Create a text element on a board.
(define-jsx-alias (jsx-board-create-text board parents attrs)
  js-jsx-board-create-text)

;; jsx-board-update! : external/raw -> external/raw
;;   Update a board and redraw as needed.
(define-jsx-alias (jsx-board-update! board) js-jsx-board-update!)

;; jsx-board-full-update! : external/raw -> external/raw
;;   Force a full board update.
(define-jsx-alias (jsx-board-full-update! board) js-jsx-board-full-update!)

;; jsx-board-remove-object! : external/raw external/raw -> void?
;;   Remove an object from a board.
(define-jsx-alias (jsx-board-remove-object! board object)
  js-jsx-board-remove-object!)

;; jsx-board-suspend-update! : external/raw -> external/raw
;;   Suspend automatic board updates.
(define-jsx-alias (jsx-board-suspend-update! board)
  js-jsx-board-suspend-update!)

;; jsx-board-unsuspend-update! : external/raw -> external/raw
;;   Resume automatic board updates.
(define-jsx-alias (jsx-board-unsuspend-update! board)
  js-jsx-board-unsuspend-update!)

;; jsx-point? : any/c -> boolean?
;;   Test whether a value is a JSXGraph point.
(define-jsx-alias (jsx-point? x) js-jsx-point?)

;; jsx-point-attractor-distance : external/raw -> flonum?
;;   Read the point's attractor distance.
(define-jsx-alias (jsx-point-attractor-distance point)
  js-jsx-point-attractor-distance)

;; jsx-set-point-attractor-distance! : external/raw flonum? -> void?
;;   Set the point's attractor distance.
(define-jsx-alias (jsx-set-point-attractor-distance! point distance)
  js-jsx-set-point-attractor-distance!)

;; jsx-point-attractors : external/raw -> external/raw
;;   Read the point's attractors.
(define-jsx-alias (jsx-point-attractors point) js-jsx-point-attractors)

;; jsx-set-point-attractors! : external/raw external/raw -> void?
;;   Set the point's attractors.
(define-jsx-alias (jsx-set-point-attractors! point attractors)
  js-jsx-set-point-attractors!)

;; jsx-point-attractor-unit : external/raw -> string?
;;   Read the point's attractor unit.
(define-jsx-alias (jsx-point-attractor-unit point) js-jsx-point-attractor-unit)

;; jsx-set-point-attractor-unit! : external/raw string? -> void?
;;   Set the point's attractor unit.
(define-jsx-alias (jsx-set-point-attractor-unit! point unit)
  js-jsx-set-point-attractor-unit!)

;; jsx-point-attract-to-grid : external/raw -> boolean?
;;   Read whether the point attracts to grid.
(define-jsx-alias (jsx-point-attract-to-grid point)
  js-jsx-point-attract-to-grid)

;; jsx-set-point-attract-to-grid! : external/raw boolean? -> void?
;;   Enable or disable grid attraction.
(define-jsx-alias (jsx-set-point-attract-to-grid! point enable)
  js-jsx-set-point-attract-to-grid!)

;; jsx-point-face : external/raw -> string?
;;   Read the point face.
(define-jsx-alias (jsx-point-face point) js-jsx-point-face)

;; jsx-set-point-face! : external/raw string? -> void?
;;   Set the point face.
(define-jsx-alias (jsx-set-point-face! point face) js-jsx-set-point-face!)

;; jsx-point-ignored-snap-to-points : external/raw -> external/raw
;;   Read the point's ignored snap-to points.
(define-jsx-alias (jsx-point-ignored-snap-to-points point)
  js-jsx-point-ignored-snap-to-points)

;; jsx-set-point-ignored-snap-to-points! : external/raw external/raw -> void?
;;   Set the point's ignored snap-to points.
(define-jsx-alias (jsx-set-point-ignored-snap-to-points! point points)
  js-jsx-set-point-ignored-snap-to-points!)

;; jsx-point-infobox-digits : external/raw -> exact-integer?
;;   Read the number of infobox digits.
(define-jsx-alias (jsx-point-infobox-digits point) js-jsx-point-infobox-digits)

;; jsx-set-point-infobox-digits! : external/raw exact-integer? -> void?
;;   Set the number of infobox digits.
(define-jsx-alias (jsx-set-point-infobox-digits! point digits)
  js-jsx-set-point-infobox-digits!)

;; jsx-point-show-infobox : external/raw -> boolean?
;;   Read whether the infobox is shown.
(define-jsx-alias (jsx-point-show-infobox point) js-jsx-point-show-infobox)

;; jsx-set-point-show-infobox! : external/raw boolean? -> void?
;;   Enable or disable the point infobox.
(define-jsx-alias (jsx-set-point-show-infobox! point show?)
  js-jsx-set-point-show-infobox!)

;; jsx-point-size : external/raw -> flonum?
;;   Read the point size.
(define-jsx-alias (jsx-point-size point) js-jsx-point-size)

;; jsx-set-point-size! : external/raw flonum? -> void?
;;   Set the point size.
(define-jsx-alias (jsx-set-point-size! point size) js-jsx-set-point-size!)

;; jsx-point-size-unit : external/raw -> string?
;;   Read the point size unit.
(define-jsx-alias (jsx-point-size-unit point) js-jsx-point-size-unit)

;; jsx-set-point-size-unit! : external/raw string? -> void?
;;   Set the point size unit.
(define-jsx-alias (jsx-set-point-size-unit! point unit)
  js-jsx-set-point-size-unit!)

;; jsx-point-snap-size-x : external/raw -> flonum?
;;   Read the x snap size.
(define-jsx-alias (jsx-point-snap-size-x point) js-jsx-point-snap-size-x)

;; jsx-set-point-snap-size-x! : external/raw flonum? -> void?
;;   Set the x snap size.
(define-jsx-alias (jsx-set-point-snap-size-x! point step)
  js-jsx-set-point-snap-size-x!)

;; jsx-point-snap-size-y : external/raw -> flonum?
;;   Read the y snap size.
(define-jsx-alias (jsx-point-snap-size-y point) js-jsx-point-snap-size-y)

;; jsx-set-point-snap-size-y! : external/raw flonum? -> void?
;;   Set the y snap size.
(define-jsx-alias (jsx-set-point-snap-size-y! point step)
  js-jsx-set-point-snap-size-y!)

;; jsx-point-snap-to-grid : external/raw -> boolean?
;;   Read whether grid snapping is enabled.
(define-jsx-alias (jsx-point-snap-to-grid point) js-jsx-point-snap-to-grid)

;; jsx-set-point-snap-to-grid! : external/raw boolean? -> void?
;;   Enable or disable grid snapping.
(define-jsx-alias (jsx-set-point-snap-to-grid! point enable)
  js-jsx-set-point-snap-to-grid!)

;; jsx-point-snap-to-points : external/raw -> boolean?
;;   Read whether point snapping is enabled.
(define-jsx-alias (jsx-point-snap-to-points point) js-jsx-point-snap-to-points)

;; jsx-set-point-snap-to-points! : external/raw boolean? -> void?
;;   Enable or disable point snapping.
(define-jsx-alias (jsx-set-point-snap-to-points! point enable)
  js-jsx-set-point-snap-to-points!)

;; jsx-point-snatch-distance : external/raw -> flonum?
;;   Read the snap distance threshold.
(define-jsx-alias (jsx-point-snatch-distance point) js-jsx-point-snatch-distance)

;; jsx-set-point-snatch-distance! : external/raw flonum? -> void?
;;   Set the snap distance threshold.
(define-jsx-alias (jsx-set-point-snatch-distance! point distance)
  js-jsx-set-point-snatch-distance!)

;; jsx-point-style : external/raw -> exact-integer?
;;   Read the point style.
(define-jsx-alias (jsx-point-style point) js-jsx-point-style)

;; jsx-set-point-style! : external/raw exact-integer? -> void?
;;   Set the point style.
(define-jsx-alias (jsx-set-point-style! point style) js-jsx-set-point-style!)

;; jsx-point-x : external/raw -> flonum?
;;   Read the x coordinate of a point.
(define-jsx-alias (jsx-point-x point) js-jsx-point-x)

;; jsx-point-y : external/raw -> flonum?
;;   Read the y coordinate of a point.
(define-jsx-alias (jsx-point-y point) js-jsx-point-y)

;; jsx-point-zoom : external/raw -> boolean?
;;   Read whether point size scales with zoom.
(define-jsx-alias (jsx-point-zoom point) js-jsx-point-zoom)

;; jsx-set-point-zoom! : external/raw boolean? -> void?
;;   Enable or disable zoom scaling.
(define-jsx-alias (jsx-set-point-zoom! point enable) js-jsx-set-point-zoom!)

;; jsx-point-has-point : external/raw flonum? flonum? -> boolean?
;;   Check whether screen coordinates hit the point.
(define-jsx-alias (jsx-point-has-point point x y) js-jsx-point-has-point)

;; jsx-point-is-on : external/raw external/raw flonum? -> boolean?
;;   Check whether the point lies on another element.
(define-jsx-alias (jsx-point-is-on point element tol) js-jsx-point-is-on)

;; jsx-point-make-intersection! : external/raw external/raw external/raw exact-integer? exact-integer? -> void?
;;   Convert a point into an intersection definition.
(define-jsx-alias (jsx-point-make-intersection! point el1 el2 i j)
  js-jsx-point-make-intersection!)

;; jsx-point-normalize-face : external/raw string? -> string?
;;   Normalize a point face token.
(define-jsx-alias (jsx-point-normalize-face point face)
  js-jsx-point-normalize-face)

;; jsx-point-set-style! : external/raw exact-integer? -> void?
;;   Set the point style directly.
(define-jsx-alias (jsx-point-set-style! point style) js-jsx-point-set-style!)

;; jsx-point-update! : external/raw exact-integer? -> void?
;;   Update point state.
(define-jsx-alias (jsx-point-update! point finalize) js-jsx-point-update!)

;; jsx-point-update-renderer! : external/raw -> void?
;;   Refresh the point renderer.
(define-jsx-alias (jsx-point-update-renderer! point)
  js-jsx-point-update-renderer!)

;; jsx-point-update-transform! : external/raw exact-integer? -> external/raw
;;   Apply transformations to the point base element.
(define-jsx-alias (jsx-point-update-transform! point finalize)
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
  (list->vector xs))

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

;; jsx-create-board : string? [any/c #f] -> external/raw
;;   Create a board with the standard JSXGraph defaults.
(define (jsx-create-board container-id [maybe-attributes #f])
  (define attrs
    (or maybe-attributes
        (js-object
         (vector (vector "boundingbox" #[-5 5 5 -5])
                 (vector "axis" #t)
                 (vector "keepaspectratio" #t)))))
  (jsx-init-board container-id attrs))

;; jsx-create-point : external/raw any/c [any/c #f] -> external/raw
;;   Create a point on a board.
(define (jsx-create-point board parents [attributes #f])
  (jsx-board-create-point board parents (or attributes '#[])))

;; jsx-create-line : external/raw any/c [any/c #f] -> external/raw
;;   Create a line on a board.
(define (jsx-create-line board parents [attributes #f])
  (jsx-board-create-line board parents (or attributes '#[])))

;; jsx-create-segment : external/raw any/c [any/c #f] -> external/raw
;;   Create a segment on a board.
(define (jsx-create-segment board parents [attributes #f])
  (jsx-board-create-segment board parents (or attributes '#[])))

;; jsx-create-circle : external/raw any/c [any/c #f] -> external/raw
;;   Create a circle on a board.
(define (jsx-create-circle board parents [attributes #f])
  (jsx-board-create-circle board parents (or attributes '#[])))

;; jsx-create-perpendicular : external/raw any/c [any/c #f] -> external/raw
;;   Create a perpendicular line on a board.
(define (jsx-create-perpendicular board parents [attributes #f])
  (jsx-board-create board "perpendicular" parents (or attributes '#[])))

;; jsx-create-intersection : external/raw any/c [any/c #f] -> external/raw
;;   Create an intersection point on a board.
(define (jsx-create-intersection board parents [attributes #f])
  (jsx-board-create-intersection board parents (or attributes '#[])))

;; jsx-create-text : external/raw any/c [any/c #f] -> external/raw
;;   Create a text object on a board.
(define (jsx-create-text board parents [attributes #f])
  (jsx-board-create-text board parents (or attributes '#[])))

;; jsx-coordinates : external/raw -> (values flonum? flonum?)
;;   Return a point's coordinates as two values.
(define (jsx-coordinates p)
  (values (jsx-point-x p) (jsx-point-y p)))

;; jsx-on : external/raw string? procedure? -> void?
;;   Install a JSXGraph event handler on an element.
(define (jsx-on element event handler)
  (js-send element "on" (vector event handler)))
