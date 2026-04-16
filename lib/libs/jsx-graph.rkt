#lang webracket

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

;; jsx-board-create-circle/raw : external/raw any/c any/c -> external/raw
;;   Create a circle on a board.
(define-jsx-alias (jsx-board-create-circle/raw board parents attrs)
  js-jsx-board-create-circle)

;; jsx-board-create-intersection/raw : external/raw any/c any/c -> external/raw
;;   Create an intersection point on a board.
(define-jsx-alias (jsx-board-create-intersection/raw board parents attrs)
  js-jsx-board-create-intersection)

;; jsx-board-create-text/raw : external/raw any/c any/c -> external/raw
;;   Create a text element on a board.
(define-jsx-alias (jsx-board-create-text/raw board parents attrs)
  js-jsx-board-create-text)

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

;; jsx-create-segment : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a segment on a board.
(define (jsx-create-segment board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-segment/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-circle : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a circle on a board.
(define (jsx-create-circle board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-circle/raw (jsx-board-raw board) parents (or attributes '#[]))))

;; jsx-create-perpendicular : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a perpendicular line on a board.
(define (jsx-create-perpendicular board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create/raw (jsx-board-raw board) "perpendicular" parents
                         (or attributes '#[]))))

;; jsx-create-intersection : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create an intersection point on a board.
(define (jsx-create-intersection board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-intersection/raw (jsx-board-raw board) parents
                                      (or attributes '#[]))))

;; jsx-create-text : jsx-board? any/c [any/c #f] -> jsx-element?
;;   Create a text object on a board.
(define (jsx-create-text board parents [attributes #f])
  (jsx-wrap-element
   (jsx-board-create-text/raw (jsx-board-raw board) parents (or attributes '#[]))))

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
  (js-ref (jsx-board-raw board) "id"))

;; jsx-board-container : jsx-board? -> any/c
;;   Read the board container element.
(define (jsx-board-container board)
  (js-ref (jsx-board-raw board) "container"))

;; jsx-board-renderer : jsx-board? -> any/c
;;   Read the renderer used by the board.
(define (jsx-board-renderer board)
  (js-ref (jsx-board-raw board) "renderer"))

;; jsx-board-canvas-width : jsx-board? -> exact-nonnegative-integer?
;;   Read the board canvas width.
(define (jsx-board-canvas-width board)
  (js-ref (jsx-board-raw board) "canvasWidth"))

;; jsx-board-canvas-height : jsx-board? -> exact-nonnegative-integer?
;;   Read the board canvas height.
(define (jsx-board-canvas-height board)
  (js-ref (jsx-board-raw board) "canvasHeight"))

;; jsx-board-bounding-box : jsx-board? -> vector?
;;   Read the board bounding box.
(define (jsx-board-bounding-box board)
  (js-array->vector (js-ref (jsx-board-raw board) "boundingBox")))

;; jsx-board-add-grid! : jsx-board? -> void?
;;   Add the default grid to the board.
(define (jsx-board-add-grid! board)
  (js-send/extern/nullish (jsx-board-raw board) "addGrid" (vector))
  (void))

;; jsx-board-add-hook! : jsx-board? procedure? [string?] [any/c] -> exact-integer?
;;   Register a board hook and return its identifier.
(define (jsx-board-add-hook! board hook [m "update"] [context board])
  (js-send/extern (jsx-board-raw board) "addHook"
                  (vector hook m (jsx-unwrap context))))

;; jsx-board-add-keyboard-event-handlers! : jsx-board? -> void?
;;   Register keyboard event handlers.
(define (jsx-board-add-keyboard-event-handlers! board)
  (js-send/extern/nullish (jsx-board-raw board) "addKeyboardEventHandlers" (vector))
  (void))

;; jsx-board-add-mouse-event-handlers! : jsx-board? -> void?
;;   Register mouse event handlers.
(define (jsx-board-add-mouse-event-handlers! board)
  (js-send/extern/nullish (jsx-board-raw board) "addMouseEventHandlers" (vector))
  (void))

;; jsx-board-add-pointer-event-handlers! : jsx-board? -> void?
;;   Register pointer event handlers.
(define (jsx-board-add-pointer-event-handlers! board)
  (js-send/extern/nullish (jsx-board-raw board) "addPointerEventHandlers" (vector))
  (void))

;; jsx-board-add-resize-event-handlers! : jsx-board? -> void?
;;   Register resize event handlers.
(define (jsx-board-add-resize-event-handlers! board)
  (js-send/extern/nullish (jsx-board-raw board) "addResizeEventHandlers" (vector))
  (void))

;; jsx-board-add-touch-event-handlers! : jsx-board? any/c -> void?
;;   Register touch event handlers.
(define (jsx-board-add-touch-event-handlers! board apple-gestures)
  (js-send/extern/nullish (jsx-board-raw board) "addTouchEventHandlers" (vector apple-gestures))
  (void))

;; jsx-board-add-wheel-event-handlers! : jsx-board? -> void?
;;   Register wheel event handlers.
(define (jsx-board-add-wheel-event-handlers! board)
  (js-send/extern/nullish (jsx-board-raw board) "addWheelEventHandlers" (vector))
  (void))

;; jsx-board-add-fullscreen-event-handlers! : jsx-board? -> void?
;;   Register fullscreen event handlers.
(define (jsx-board-add-fullscreen-event-handlers! board)
  (js-send/extern/nullish (jsx-board-raw board) "addFullscreenEventHandlers" (vector))
  (void))

;; jsx-board-add-log-entry! : jsx-board? any/c any/c any/c -> void?
;;   Add a log entry to the board user log.
(define (jsx-board-add-log-entry! board type obj pos)
  (js-send/extern/nullish (jsx-board-raw board) "addLogEntry"
                          (vector type (jsx-unwrap obj) pos))
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
  (js-ref (jsx-board-raw board) "numObjects"))

;; jsx-board-get-mouse-position : jsx-board? any/c [exact-integer?] -> vector?
;;   Read the mouse position in screen coordinates.
(define (jsx-board-get-mouse-position board evt [i #f])
  (define args (if i
                   (vector evt i)
                   (vector evt)))
  (js-array->vector
   (js-send/extern (jsx-board-raw board) "getMousePosition" args)))

;; jsx-board-get-usr-coords-of-mouse : jsx-board? any/c -> vector?
;;   Read the mouse position in user coordinates.
(define (jsx-board-get-usr-coords-of-mouse board evt)
  (js-array->vector
   (js-send/extern (jsx-board-raw board) "getUsrCoordsOfMouse" (vector evt))))

;; jsx-board-get-coords-top-left-corner : jsx-board? -> vector?
;;   Read the board coordinates of the top-left corner.
(define (jsx-board-get-coords-top-left-corner board)
  (js-array->vector
   (js-send/extern (jsx-board-raw board) "getCoordsTopLeftCorner" (vector))))

;; jsx-board-get-all-objects-under-mouse : jsx-board? any/c -> vector?
;;   Collect the objects under a pointer event.
(define (jsx-board-get-all-objects-under-mouse board evt)
  (js-array->vector
   (js-send/extern (jsx-board-raw board) "getAllObjectsUnderMouse" (vector evt))))

;; jsx-board-get-all-under-mouse : jsx-board? any/c -> vector?
;;   Collect the objects and user coordinates under a pointer event.
(define (jsx-board-get-all-under-mouse board evt)
  (js-array->vector
   (js-send/extern (jsx-board-raw board) "getAllUnderMouse" (vector evt))))

;; jsx-board-has-point? : jsx-board? flonum? flonum? -> boolean?
;;   Check whether a point lies inside the board bounding box.
(define (jsx-board-has-point? board x y)
  (js-send/extern (jsx-board-raw board) "hasPoint" (vector x y)))

;; jsx-board-objects-list : jsx-board? -> vector?
;;   Read the board objects in construction order.
(define (jsx-board-objects-list board)
  (define objects (js-ref (jsx-board-raw board) "objects"))
  (define keys (js-array->vector (js-send/extern (js-Object) "keys" (vector objects))))
  (for/vector #:length (vector-length keys) ([key (in-vector keys)])
    (jsx-wrap-board-object (js-ref objects key))))

;; jsx-board-highlight-custom-infobox! : jsx-board? string? [any/c] -> void?
;;   Change the board infobox text.
(define (jsx-board-highlight-custom-infobox! board text [el #f])
  (js-send/extern/nullish (jsx-board-raw board) "highlightCustomInfobox"
                          (vector text el))
  (void))

;; jsx-board-highlight-infobox! : jsx-board? any/c any/c [any/c] -> void?
;;   Show the given coordinates in the board infobox.
(define (jsx-board-highlight-infobox! board x y [el #f])
  (js-send/extern/nullish (jsx-board-raw board) "highlightInfobox"
                          (vector x y el))
  (void))

;; jsx-board-move-object! : jsx-board? any/c any/c any/c any/c any/c -> void?
;;   Move a board object.
(define (jsx-board-move-object! board x y o evt type)
  (js-send/extern/nullish (jsx-board-raw board) "moveObject"
                          (vector x y (jsx-unwrap o) evt type))
  (void))

;; jsx-board-move-origin! : jsx-board? any/c any/c any/c -> void?
;;   Move the board origin.
(define (jsx-board-move-origin! board x y diff)
  (js-send/extern/nullish (jsx-board-raw board) "moveOrigin" (vector x y diff))
  (void))

;; jsx-board-set-attribute! : jsx-board? any/c -> void?
;;   Set board attributes.
(define (jsx-board-set-attribute! board attributes)
  (js-send/extern/nullish (jsx-board-raw board) "setAttribute" (vector attributes))
  (void))

;; jsx-board-set-bounding-box! : jsx-board? any/c any/c any/c -> void?
;;   Set the board bounding box.
(define (jsx-board-set-bounding-box! board bbox keepaspectratio setZoom)
  (js-send/extern/nullish (jsx-board-raw board) "setBoundingBox"
                          (vector bbox keepaspectratio setZoom))
  (void))

;; jsx-board-set-zoom! : jsx-board? any/c any/c -> void?
;;   Set the board zoom.
(define (jsx-board-set-zoom! board fX fY)
  (js-send/extern/nullish (jsx-board-raw board) "setZoom" (vector fX fY))
  (void))

;; jsx-board-resize-container! : jsx-board? any/c any/c any/c any/c -> void?
;;   Resize the board container.
(define (jsx-board-resize-container! board canvasWidth canvasHeight dontset dontSetBoundingBox)
  (js-send/extern/nullish (jsx-board-raw board) "resizeContainer"
                          (vector canvasWidth canvasHeight dontset dontSetBoundingBox))
  (void))

;; jsx-board-remove-grids! : jsx-board? -> void?
;;   Remove all grids from the board.
(define (jsx-board-remove-grids! board)
  (js-send/extern/nullish (jsx-board-raw board) "removeGrids" (vector))
  (void))

;; jsx-board-remove-hook! : jsx-board? exact-integer? -> void?
;;   Remove a board hook.
(define (jsx-board-remove-hook! board id)
  (js-send/extern/nullish (jsx-board-raw board) "removeHook" (vector id))
  (void))

;; jsx-board-remove-keyboard-event-handlers! : jsx-board? -> void?
;;   Remove keyboard event handlers.
(define (jsx-board-remove-keyboard-event-handlers! board)
  (js-send/extern/nullish (jsx-board-raw board) "removeKeyboardEventHandlers" (vector))
  (void))

;; jsx-board-remove-mouse-event-handlers! : jsx-board? -> void?
;;   Remove mouse event handlers.
(define (jsx-board-remove-mouse-event-handlers! board)
  (js-send/extern/nullish (jsx-board-raw board) "removeMouseEventHandlers" (vector))
  (void))

;; jsx-board-remove-pointer-event-handlers! : jsx-board? -> void?
;;   Remove pointer event handlers.
(define (jsx-board-remove-pointer-event-handlers! board)
  (js-send/extern/nullish (jsx-board-raw board) "removePointerEventHandlers" (vector))
  (void))

;; jsx-board-remove-resize-event-handlers! : jsx-board? -> void?
;;   Remove resize event handlers.
(define (jsx-board-remove-resize-event-handlers! board)
  (js-send/extern/nullish (jsx-board-raw board) "removeResizeEventHandlers" (vector))
  (void))

;; jsx-board-remove-touch-event-handlers! : jsx-board? -> void?
;;   Remove touch event handlers.
(define (jsx-board-remove-touch-event-handlers! board)
  (js-send/extern/nullish (jsx-board-raw board) "removeTouchEventHandlers" (vector))
  (void))

;; jsx-board-select : jsx-board? any/c [boolean?] -> (or/c jsx-point? jsx-element?)
;;   Select one or more objects on the board.
(define (jsx-board-select board str [only-by-id-or-name #f])
  (jsx-wrap-board-object
   (js-send/extern (jsx-board-raw board) "select" (vector str only-by-id-or-name))))

;; jsx-board-zoom100! : jsx-board? -> void?
;;   Reset the board zoom to 100%.
(define (jsx-board-zoom100! board)
  (js-send/extern/nullish (jsx-board-raw board) "zoom100" (vector))
  (void))

;; jsx-board-zoom-all-points! : jsx-board? -> void?
;;   Zoom so every visible point fits in the viewport.
(define (jsx-board-zoom-all-points! board)
  (js-send/extern/nullish (jsx-board-raw board) "zoomAllPoints" (vector))
  (void))

;; jsx-board-zoom-in! : jsx-board? any/c any/c -> void?
;;   Zoom in around a point.
(define (jsx-board-zoom-in! board x y)
  (js-send/extern/nullish (jsx-board-raw board) "zoomIn" (vector x y))
  (void))

;; jsx-board-zoom-out! : jsx-board? any/c any/c -> void?
;;   Zoom out around a point.
(define (jsx-board-zoom-out! board x y)
  (js-send/extern/nullish (jsx-board-raw board) "zoomOut" (vector x y))
  (void))

;; jsx-board-start-selection-mode! : jsx-board? -> void?
;;   Enable board selection mode.
(define (jsx-board-start-selection-mode! board)
  (js-send/extern/nullish (jsx-board-raw board) "startSelectionMode" (vector))
  (void))

;; jsx-board-stop-selection-mode! : jsx-board? -> void?
;;   Disable board selection mode.
(define (jsx-board-stop-selection-mode! board)
  (js-send/extern/nullish (jsx-board-raw board) "stopSelectionMode" (vector))
  (void))

;; jsx-board-stop-all-animation! : jsx-board? -> void?
;;   Stop all running animations on a board.
(define (jsx-board-stop-all-animation! board)
  (js-send/extern/nullish (jsx-board-raw board) "stopAllAnimation" (vector))
  (void))

;; jsx-board-clear-traces! : jsx-board? -> void?
;;   Clear all traces from the board.
(define (jsx-board-clear-traces! board)
  (js-send/extern/nullish (jsx-board-raw board) "clearTraces" (vector))
  (void))

;; jsx-board-dehighlight-all! : jsx-board? -> void?
;;   Remove highlighting from all elements.
(define (jsx-board-dehighlight-all! board)
  (js-send/extern/nullish (jsx-board-raw board) "dehighlightAll" (vector))
  (void))

;; jsx-board-update-coords! : jsx-board? -> void?
;;   Update the coordinates of all elements that need them.
(define (jsx-board-update-coords! board)
  (js-send/extern/nullish (jsx-board-raw board) "updateCoords" (vector))
  (void))

;; jsx-board-update-csstransforms! : jsx-board? -> void?
;;   Refresh CSS transforms.
(define (jsx-board-update-csstransforms! board)
  (js-send/extern/nullish (jsx-board-raw board) "updateCSSTransforms" (vector))
  (void))

;; jsx-board-update-elements! : jsx-board? any/c -> void?
;;   Update the board elements.
(define (jsx-board-update-elements! board drag)
  (js-send/extern/nullish (jsx-board-raw board) "updateElements" (vector drag))
  (void))

;; jsx-board-update-hooks! : jsx-board? any/c -> void?
;;   Run hooked board callbacks.
(define (jsx-board-update-hooks! board m)
  (js-send/extern/nullish (jsx-board-raw board) "updateHooks" (vector m))
  (void))

;; jsx-board-suppress-default! : jsx-board? any/c -> void?
;;   Suppress the default event action.
(define (jsx-board-suppress-default! board e)
  (js-send/extern/nullish (jsx-board-raw board) "suppressDefault" (vector e))
  (void))

;; jsx-board-init-infobox! : jsx-board? any/c -> void?
;;   Initialize the board infobox.
(define (jsx-board-init-infobox! board attributes)
  (js-send/extern/nullish (jsx-board-raw board) "initInfobox" (vector attributes))
  (void))

;; jsx-board-init-move-object! : jsx-board? any/c any/c any/c any/c -> void?
;;   Prepare a board object move.
(define (jsx-board-init-move-object! board x y evt type)
  (js-send/extern/nullish (jsx-board-raw board) "initMoveObject" (vector x y evt type))
  (void))

;; jsx-board-init-move-origin! : jsx-board? any/c any/c -> void?
;;   Prepare moving the board origin.
(define (jsx-board-init-move-origin! board x y)
  (js-send/extern/nullish (jsx-board-raw board) "initMoveOrigin" (vector x y))
  (void))

;; jsx-board-show-dependencies! : jsx-board? -> void?
;;   Show the dependency graph for the board.
(define (jsx-board-show-dependencies! board)
  (js-send/extern/nullish (jsx-board-raw board) "showDependencies" (vector))
  (void))

;; jsx-board-show-xml! : jsx-board? -> void?
;;   Show the board XML in a separate window.
(define (jsx-board-show-xml! board)
  (js-send/extern/nullish (jsx-board-raw board) "showXML" (vector))
  (void))

;; jsx-board-to-fullscreen! : jsx-board? any/c -> void?
;;   Expand the board to fullscreen.
(define (jsx-board-to-fullscreen! board id)
  (js-send/extern/nullish (jsx-board-raw board) "toFullscreen" (vector id))
  (void))

;; jsx-board-start-resize-observer! : jsx-board? -> void?
;;   Start watching the container size.
(define (jsx-board-start-resize-observer! board)
  (js-send/extern/nullish (jsx-board-raw board) "startResizeObserver" (vector))
  (void))

;; jsx-board-stop-resize-observer! : jsx-board? -> void?
;;   Stop watching the container size.
(define (jsx-board-stop-resize-observer! board)
  (js-send/extern/nullish (jsx-board-raw board) "stopResizeObserver" (vector))
  (void))

;; jsx-board-start-intersection-observer! : jsx-board? -> void?
;;   Start watching whether the board is visible.
(define (jsx-board-start-intersection-observer! board)
  (js-send/extern/nullish (jsx-board-raw board) "startIntersectionObserver" (vector))
  (void))

;; jsx-board-stop-intersection-observer! : jsx-board? -> void?
;;   Stop watching board visibility.
(define (jsx-board-stop-intersection-observer! board)
  (js-send/extern/nullish (jsx-board-raw board) "stopIntersectionObserver" (vector))
  (void))

;; jsx-board-update-infobox! : jsx-board? any/c -> void?
;;   Update the board infobox for a geometry element.
(define (jsx-board-update-infobox! board el)
  (js-send/extern/nullish (jsx-board-raw board) "updateInfobox" (vector (jsx-unwrap el)))
  (void))

;; jsx-board-set-id : jsx-board? any/c any/c -> string?
;;   Compose a unique id for an element on the board.
(define (jsx-board-set-id board obj type)
  (js-send/extern (jsx-board-raw board) "setId" (vector (jsx-unwrap obj) type)))

;; jsx-board-update-renderer! : jsx-board? -> void?
;;   Refresh the board renderer.
(define (jsx-board-update-renderer! board)
  (js-send/extern/nullish (jsx-board-raw board) "updateRenderer" (vector))
  (void))

;; jsx-board-update-renderer-canvas! : jsx-board? -> void?
;;   Refresh the board renderer in Canvas mode.
(define (jsx-board-update-renderer-canvas! board)
  (js-send/extern/nullish (jsx-board-raw board) "updateRendererCanvas" (vector))
  (void))

;; jsx-board-zoom-elements! : jsx-board? any/c -> void?
;;   Zoom the board so a set of elements fits in the viewport.
(define (jsx-board-zoom-elements! board elements)
  (js-send/extern/nullish (jsx-board-raw board) "zoomElements"
                          (vector (jsx-unpack-array elements)))
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
