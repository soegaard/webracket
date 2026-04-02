#lang webracket

;;;
;;; Canvas wrappers
;;;

(struct canvas (raw) #:transparent)
(struct canvas-2d-context (raw) #:transparent)

;; canvas-wrap : any/c -> canvas?
;;   Wrap a browser canvas value.
(define (canvas-wrap value)
  (canvas value))

;; canvas-unwrap : any/c -> any/c
;;   Unwrap a browser canvas value.
(define (canvas-unwrap value)
  (if (canvas? value)
      (canvas-raw value)
      value))

;; canvas-2d-context-wrap : any/c -> canvas-2d-context?
;;   Wrap a browser 2D context value.
(define (canvas-2d-context-wrap value)
  (canvas-2d-context value))

;; canvas-2d-context-unwrap : any/c -> any/c
;;   Unwrap a browser 2D context value.
(define (canvas-2d-context-unwrap value)
  (if (canvas-2d-context? value)
      (canvas-2d-context-raw value)
      value))

;; canvas-i32->boolean : integer? -> boolean?
;;   Convert a browser i32 flag to a boolean.
(define (canvas-i32->boolean v)
  (not (zero? v)))

;; canvas-stringish->string : symbol? any/c -> string?
;;   Normalize a browser string argument.
(define (canvas-stringish->string who value)
  (cond
    [(string? value) value]
    [(symbol? value) (symbol->string value)]
    [else (raise-argument-error who "(or/c string? symbol?)" value)]))

;; canvas-resolve-optional : any/c -> any/c
;;   Treat #f as omitted for optional browser arguments.
(define (canvas-resolve-optional value)
  (if (eq? value #f)
      (void)
      value))

;; canvas-capture-stream : (or/c canvas? external?) [real? #f] -> media-stream?
;;   Capture the canvas stream.
(define (canvas-capture-stream canvas [frame-rate #f])
  (media-stream-wrap
   (js-canvas-capture-stream (canvas-unwrap canvas)
                             (canvas-resolve-optional frame-rate))))

;; canvas-get-context : (or/c canvas? external?) (or/c string? symbol?) [any/c #f]
;;   -> (or/c #f canvas-2d-context?)
;;   Read a drawing context from a canvas.
(define (canvas-get-context canvas context-id [options #f])
  (define context-id* (canvas-stringish->string 'canvas-get-context context-id))
  (define ctx (js-canvas-get-context (canvas-unwrap canvas)
                                     context-id*
                                     (canvas-resolve-optional options)))
  (and ctx (canvas-2d-context-wrap ctx)))

;; canvas-width : (or/c canvas? external?) -> exact-nonnegative-integer?
;;   Read the canvas width.
(define (canvas-width canvas)
  (js-canvas-width (canvas-unwrap canvas)))

;; canvas-set-width! : (or/c canvas? external?) exact-nonnegative-integer? -> void?
;;   Set the canvas width.
(define (canvas-set-width! canvas width)
  (js-set-canvas-width! (canvas-unwrap canvas) width)
  (void))

;; canvas-height : (or/c canvas? external?) -> exact-nonnegative-integer?
;;   Read the canvas height.
(define (canvas-height canvas)
  (js-canvas-height (canvas-unwrap canvas)))

;; canvas-set-height! : (or/c canvas? external?) exact-nonnegative-integer? -> void?
;;   Set the canvas height.
(define (canvas-set-height! canvas height)
  (js-set-canvas-height! (canvas-unwrap canvas) height)
  (void))

;; canvas-to-data-url : (or/c canvas? external?) (or/c string? symbol?) [any/c #f] -> string?
;;   Encode a canvas as a data URL.
(define (canvas-to-data-url canvas [type "image/png"] [quality #f])
  (define type* (canvas-stringish->string 'canvas-to-data-url type))
  (js-canvas-to-data-url (canvas-unwrap canvas) type* (canvas-resolve-optional quality)))

;; canvas-to-blob : (or/c canvas? external?) external? (or/c string? symbol?) [any/c #f] -> void?
;;   Encode a canvas into a Blob callback.
(define (canvas-to-blob canvas callback [type "image/png"] [quality #f])
  (define type* (canvas-stringish->string 'canvas-to-blob type))
  (js-canvas-to-blob (canvas-unwrap canvas) callback type* (canvas-resolve-optional quality))
  (void))

;; canvas-transfer-control-to-offscreen : (or/c canvas? external?) -> external/raw
;;   Transfer control to an OffscreenCanvas.
(define (canvas-transfer-control-to-offscreen canvas)
  (js-canvas-transfer-control-to-offscreen (canvas-unwrap canvas)))

;; canvas-2d-canvas : (or/c canvas-2d-context? external?) -> canvas?
;;   Read the backing canvas from a 2D context.
(define (canvas-2d-canvas ctx)
  (canvas-wrap (js-canvas2d-canvas (canvas-2d-context-unwrap ctx))))

;; canvas-2d-direction : (or/c canvas-2d-context? external?) -> string?
;;   Read the canvas direction.
(define (canvas-2d-direction ctx)
  (js-canvas2d-direction (canvas-2d-context-unwrap ctx)))

;; canvas-2d-set-direction! : (or/c canvas-2d-context? external?) (or/c string? symbol?) -> void?
;;   Set the canvas direction.
(define (canvas-2d-set-direction! ctx direction)
  (js-set-canvas2d-direction! (canvas-2d-context-unwrap ctx)
                              (canvas-stringish->string 'canvas-2d-set-direction!
                                                        direction))
  (void))

;; canvas-2d-fill-style : (or/c canvas-2d-context? external?) -> string?
;;   Read the fill style.
(define (canvas-2d-fill-style ctx)
  (js-ref (canvas-2d-context-unwrap ctx) "fillStyle"))

;; canvas-2d-set-fill-style! : (or/c canvas-2d-context? external?) any/c -> void?
;;   Set the fill style.
(define (canvas-2d-set-fill-style! ctx style)
  (js-set-canvas2d-fill-style! (canvas-2d-context-unwrap ctx) style)
  (void))

;; canvas-2d-stroke-style : (or/c canvas-2d-context? external?) -> string?
;;   Read the stroke style.
(define (canvas-2d-stroke-style ctx)
  (js-ref (canvas-2d-context-unwrap ctx) "strokeStyle"))

;; canvas-2d-set-stroke-style! : (or/c canvas-2d-context? external?) any/c -> void?
;;   Set the stroke style.
(define (canvas-2d-set-stroke-style! ctx style)
  (js-set-canvas2d-stroke-style! (canvas-2d-context-unwrap ctx) style)
  (void))

;; canvas-2d-fill-rect : (or/c canvas-2d-context? external?) real? real? real? real? -> void?
;;   Fill a rectangle.
(define (canvas-2d-fill-rect ctx x y w h)
  (js-canvas2d-fill-rect (canvas-2d-context-unwrap ctx) x y w h)
  (void))

;; canvas-2d-stroke-rect : (or/c canvas-2d-context? external?) real? real? real? real? -> void?
;;   Stroke a rectangle.
(define (canvas-2d-stroke-rect ctx x y w h)
  (js-canvas2d-stroke-rect (canvas-2d-context-unwrap ctx) x y w h)
  (void))

;; canvas-2d-begin-path : (or/c canvas-2d-context? external?) -> void?
;;   Begin a new path.
(define (canvas-2d-begin-path ctx)
  (js-canvas2d-begin-path (canvas-2d-context-unwrap ctx))
  (void))

;; canvas-2d-close-path : (or/c canvas-2d-context? external?) -> void?
;;   Close the current path.
(define (canvas-2d-close-path ctx)
  (js-canvas2d-close-path (canvas-2d-context-unwrap ctx))
  (void))

;; canvas-2d-move-to : (or/c canvas-2d-context? external?) real? real? -> void?
;;   Move the current path cursor.
(define (canvas-2d-move-to ctx x y)
  (js-canvas2d-move-to (canvas-2d-context-unwrap ctx) x y)
  (void))

;; canvas-2d-line-to : (or/c canvas-2d-context? external?) real? real? -> void?
;;   Add a line segment to the path.
(define (canvas-2d-line-to ctx x y)
  (js-canvas2d-line-to (canvas-2d-context-unwrap ctx) x y)
  (void))

;; canvas-2d-arc : (or/c canvas-2d-context? external?) real? real? real? real? real? -> void?
;;   Add an arc to the path.
(define (canvas-2d-arc ctx x y radius start-angle end-angle)
  (js-canvas2d-arc (canvas-2d-context-unwrap ctx) x y radius start-angle end-angle)
  (void))

;; canvas-2d-clear-rect : (or/c canvas-2d-context? external?) real? real? real? real? -> void?
;;   Clear a rectangle.
(define (canvas-2d-clear-rect ctx x y w h)
  (js-canvas2d-clear-rect (canvas-2d-context-unwrap ctx) x y w h)
  (void))

;; canvas-2d-fill : (or/c canvas-2d-context? external?) [any/c #f] [any/c #f] -> void?
;;   Fill the current path.
(define (canvas-2d-fill ctx [path #f] [fill-rule #f])
  (js-canvas2d-fill (canvas-2d-context-unwrap ctx)
                    (canvas-resolve-optional path)
                    (canvas-resolve-optional fill-rule))
  (void))

;; canvas-2d-stroke : (or/c canvas-2d-context? external?) [any/c #f] -> void?
;;   Stroke the current path.
(define (canvas-2d-stroke ctx [path #f])
  (js-canvas2d-stroke (canvas-2d-context-unwrap ctx) (canvas-resolve-optional path))
  (void))

;; canvas-2d-save : (or/c canvas-2d-context? external?) -> void?
;;   Save drawing state.
(define (canvas-2d-save ctx)
  (js-canvas2d-save (canvas-2d-context-unwrap ctx))
  (void))

;; canvas-2d-restore : (or/c canvas-2d-context? external?) -> void?
;;   Restore drawing state.
(define (canvas-2d-restore ctx)
  (js-canvas2d-restore (canvas-2d-context-unwrap ctx))
  (void))

;; canvas-2d-translate : (or/c canvas-2d-context? external?) real? real? -> void?
;;   Translate the context.
(define (canvas-2d-translate ctx x y)
  (js-canvas2d-translate (canvas-2d-context-unwrap ctx) x y)
  (void))

;; canvas-2d-scale : (or/c canvas-2d-context? external?) real? real? -> void?
;;   Scale the context.
(define (canvas-2d-scale ctx x y)
  (js-canvas2d-scale (canvas-2d-context-unwrap ctx) x y)
  (void))

;; canvas-2d-rotate : (or/c canvas-2d-context? external?) real? -> void?
;;   Rotate the context.
(define (canvas-2d-rotate ctx angle)
  (js-canvas2d-rotate (canvas-2d-context-unwrap ctx) angle)
  (void))

;; canvas-2d-fill-text : (or/c canvas-2d-context? external?) string? real? real? [any/c #f] -> void?
;;   Draw filled text.
(define (canvas-2d-fill-text ctx text x y [max-width #f])
  (js-canvas2d-fill-text (canvas-2d-context-unwrap ctx) text x y (canvas-resolve-optional max-width))
  (void))

;; canvas-2d-stroke-text : (or/c canvas-2d-context? external?) string? real? real? [any/c #f] -> void?
;;   Draw stroked text.
(define (canvas-2d-stroke-text ctx text x y [max-width #f])
  (js-canvas2d-stroke-text (canvas-2d-context-unwrap ctx) text x y (canvas-resolve-optional max-width))
  (void))

;; canvas-2d-measure-text : (or/c canvas-2d-context? external?) string? -> external/raw
;;   Measure text width and metrics.
(define (canvas-2d-measure-text ctx text)
  (js-canvas2d-measure-text (canvas-2d-context-unwrap ctx) text))

;; canvas-2d-create-linear-gradient : (or/c canvas-2d-context? external?) real? real? real? real? -> external/raw
;;   Create a linear gradient.
(define (canvas-2d-create-linear-gradient ctx x0 y0 x1 y1)
  (js-canvas2d-create-linear-gradient (canvas-2d-context-unwrap ctx) x0 y0 x1 y1))

;; canvas-2d-create-radial-gradient : (or/c canvas-2d-context? external?) real? real? real? real? real? -> external/raw
;;   Create a radial gradient.
(define (canvas-2d-create-radial-gradient ctx x0 y0 r0 x1 y1 r1)
  (js-canvas2d-create-radial-gradient (canvas-2d-context-unwrap ctx) x0 y0 r0 x1 y1 r1))
