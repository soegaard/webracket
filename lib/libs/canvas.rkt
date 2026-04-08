#lang webracket

;;;
;;; Canvas wrappers
;;;

(include-lib media)
(include-lib array)

(struct canvas (raw) #:transparent)
(struct canvas-2d-context (raw) #:transparent)
(struct canvas-image-data (raw) #:transparent)
(struct canvas-gradient (raw) #:transparent)
(struct canvas-pattern (raw) #:transparent)
(struct canvas-text-metrics (raw) #:transparent)
(struct canvas-dom-matrix (raw) #:transparent)
(struct offscreen-canvas (raw) #:transparent)

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

;; canvas-boolean->i32 : boolean? -> exact-integer?
;;   Convert a Racket boolean to a browser i32 flag.
(define (canvas-boolean->i32 value)
  (if value 1 0))

;; canvas-source-unwrap : any/c -> any/c
;;   Unwrap browser canvas-like sources when available.
(define (canvas-source-unwrap value)
  (cond
    [(canvas? value) (canvas-raw value)]
    [(canvas-image-data? value) (canvas-image-data-raw value)]
    [(offscreen-canvas? value) (offscreen-canvas-raw value)]
    [else value]))

;; canvas-paint-style-unwrap : any/c -> any/c
;;   Unwrap wrapper values used for paint styles.
(define (canvas-paint-style-unwrap value)
  (cond
    [(canvas-gradient? value) (canvas-gradient-raw value)]
    [(canvas-pattern? value) (canvas-pattern-raw value)]
    [else value]))

;; canvas-paint-style-wrap : any/c -> any/c
;;   Wrap browser paint-style values when possible.
(define (canvas-paint-style-wrap value)
  (cond
    [(and (external? value) (js-ref value "addColorStop"))
     (canvas-gradient-wrap value)]
    [(and (external? value) (js-ref value "setTransform"))
     (canvas-pattern-wrap value)]
    [else value]))

;; canvas-fill-rule->js : any/c -> any/c
;;   Normalize optional fill rule values.
(define (canvas-fill-rule->js value)
  (cond
    [(eq? value #f) (void)]
    [(string? value) value]
    [(symbol? value) (symbol->string value)]
    [else (raise-argument-error 'canvas-fill-rule->js "(or/c string? symbol? #f)" value)]))

;; canvas-matrix->raw : any/c -> any/c
;;   Unwrap a DOMMatrix wrapper when available.
(define (canvas-matrix->raw value)
  (if (canvas-dom-matrix? value)
      (canvas-dom-matrix-raw value)
      value))

;; canvas-image-data-wrap : any/c -> canvas-image-data?
;;   Wrap a browser ImageData value.
(define (canvas-image-data-wrap value)
  (canvas-image-data value))

;; canvas-image-data-unwrap : any/c -> any/c
;;   Unwrap a browser ImageData value.
(define (canvas-image-data-unwrap value)
  (if (canvas-image-data? value)
      (canvas-image-data-raw value)
      value))

;; canvas-gradient-wrap : any/c -> canvas-gradient?
;;   Wrap a browser gradient value.
(define (canvas-gradient-wrap value)
  (canvas-gradient value))

;; canvas-gradient-unwrap : any/c -> any/c
;;   Unwrap a browser gradient value.
(define (canvas-gradient-unwrap value)
  (if (canvas-gradient? value)
      (canvas-gradient-raw value)
      value))

;; canvas-pattern-wrap : any/c -> canvas-pattern?
;;   Wrap a browser pattern value.
(define (canvas-pattern-wrap value)
  (canvas-pattern value))

;; canvas-pattern-unwrap : any/c -> any/c
;;   Unwrap a browser pattern value.
(define (canvas-pattern-unwrap value)
  (if (canvas-pattern? value)
      (canvas-pattern-raw value)
      value))

;; canvas-text-metrics-wrap : any/c -> canvas-text-metrics?
;;   Wrap a browser TextMetrics value.
(define (canvas-text-metrics-wrap value)
  (canvas-text-metrics value))

;; canvas-text-metrics-unwrap : any/c -> any/c
;;   Unwrap a browser TextMetrics value.
(define (canvas-text-metrics-unwrap value)
  (if (canvas-text-metrics? value)
      (canvas-text-metrics-raw value)
      value))

;; canvas-dom-matrix-wrap : any/c -> canvas-dom-matrix?
;;   Wrap a browser DOMMatrix value.
(define (canvas-dom-matrix-wrap value)
  (canvas-dom-matrix value))

;; canvas-dom-matrix-unwrap : any/c -> any/c
;;   Unwrap a browser DOMMatrix value.
(define (canvas-dom-matrix-unwrap value)
  (if (canvas-dom-matrix? value)
      (canvas-dom-matrix-raw value)
      value))

;; canvas-js-array->bytes : any/c -> bytes?
;;   Convert a browser byte array to a Racket bytes value.
(define (canvas-js-array->bytes arr)
  (array->bytes arr))

;; offscreen-canvas-wrap : any/c -> offscreen-canvas?
;;   Wrap a browser OffscreenCanvas value.
(define (offscreen-canvas-wrap value)
  (offscreen-canvas value))

;; offscreen-canvas-unwrap : any/c -> any/c
;;   Unwrap a browser OffscreenCanvas value.
(define (offscreen-canvas-unwrap value)
  (if (offscreen-canvas? value)
      (offscreen-canvas-raw value)
      value))

;; canvas-capture-stream : (or/c canvas? external?) [real? #f] -> media-stream?
;;   Capture the canvas stream.
(define (canvas-capture-stream canvas [frame-rate #f])
  (media-stream-wrap (js-canvas-capture-stream (canvas-unwrap canvas)
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
  (offscreen-canvas-wrap
   (js-canvas-transfer-control-to-offscreen (canvas-unwrap canvas))))

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
  (define ctx* (canvas-2d-context-unwrap ctx))
  (cond
    [(and (eq? path #f) (eq? fill-rule #f))
     (js-send/value ctx* "fill" (vector))]
    [(eq? path #f)
     (js-send/value ctx* "fill" (vector (canvas-fill-rule->js fill-rule)))]
    [(eq? fill-rule #f)
     (js-send/value ctx* "fill" (vector (canvas-resolve-optional path)))]
    [else
     (js-send/value ctx* "fill"
                    (vector (canvas-resolve-optional path)
                            (canvas-fill-rule->js fill-rule)))])
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
  (canvas-text-metrics-wrap (js-canvas2d-measure-text (canvas-2d-context-unwrap ctx) text)))

;; canvas-image-data-width : canvas-image-data? -> exact-nonnegative-integer?
;;   Read the width of an image buffer.
(define (canvas-image-data-width data)
  (js-canvas-image-data-width (canvas-image-data-raw data)))

;; canvas-image-data-height : canvas-image-data? -> exact-nonnegative-integer?
;;   Read the height of an image buffer.
(define (canvas-image-data-height data)
  (js-canvas-image-data-height (canvas-image-data-raw data)))

;; canvas-image-data-data : canvas-image-data? -> bytes?
;;   Read the pixel bytes of an image buffer.
(define (canvas-image-data-data data)
  (canvas-js-array->bytes (js-canvas-image-data-data (canvas-image-data-raw data))))

;; canvas-text-metrics-width : canvas-text-metrics? -> real?
;;   Read the measured text width.
(define (canvas-text-metrics-width metrics)
  (js-canvas-text-metrics-width (canvas-text-metrics-raw metrics)))

;; canvas-dom-matrix-a : canvas-dom-matrix? -> real?
;;   Read the A component of a transform matrix.
(define (canvas-dom-matrix-a matrix)
  (js-canvas-dom-matrix-a (canvas-dom-matrix-raw matrix)))

;; canvas-dom-matrix-b : canvas-dom-matrix? -> real?
;;   Read the B component of a transform matrix.
(define (canvas-dom-matrix-b matrix)
  (js-canvas-dom-matrix-b (canvas-dom-matrix-raw matrix)))

;; canvas-dom-matrix-c : canvas-dom-matrix? -> real?
;;   Read the C component of a transform matrix.
(define (canvas-dom-matrix-c matrix)
  (js-canvas-dom-matrix-c (canvas-dom-matrix-raw matrix)))

;; canvas-dom-matrix-d : canvas-dom-matrix? -> real?
;;   Read the D component of a transform matrix.
(define (canvas-dom-matrix-d matrix)
  (js-canvas-dom-matrix-d (canvas-dom-matrix-raw matrix)))

;; canvas-dom-matrix-e : canvas-dom-matrix? -> real?
;;   Read the E component of a transform matrix.
(define (canvas-dom-matrix-e matrix)
  (js-canvas-dom-matrix-e (canvas-dom-matrix-raw matrix)))

;; canvas-dom-matrix-f : canvas-dom-matrix? -> real?
;;   Read the F component of a transform matrix.
(define (canvas-dom-matrix-f matrix)
  (js-canvas-dom-matrix-f (canvas-dom-matrix-raw matrix)))

;; canvas-gradient-add-color-stop! : (or/c canvas-gradient? external?) real? (or/c string? symbol?) -> void?
;;   Add a color stop to a gradient.
(define (canvas-gradient-add-color-stop! gradient offset color)
  (js-send/value (canvas-gradient-unwrap gradient) "addColorStop"
                 (vector offset (canvas-stringish->string 'canvas-gradient-add-color-stop! color)))
  (void))

;; canvas-pattern-set-transform! : (or/c canvas-pattern? external?) (or/c canvas-dom-matrix? external?) -> void?
;;   Set a pattern transform.
(define (canvas-pattern-set-transform! pattern matrix)
  (js-send/value (canvas-pattern-unwrap pattern) "setTransform"
                 (vector (canvas-matrix->raw matrix)))
  (void))

;; canvas-2d-filter : (or/c canvas-2d-context? external?) -> string?
;;   Read the CSS filter value.
(define (canvas-2d-filter ctx)
  (js-canvas2d-filter (canvas-2d-context-unwrap ctx)))

;; canvas-2d-set-filter! : (or/c canvas-2d-context? external?) (or/c string? symbol?) -> void?
;;   Set the CSS filter value.
(define (canvas-2d-set-filter! ctx filter)
  (js-set-canvas2d-filter! (canvas-2d-context-unwrap ctx)
                           (canvas-stringish->string 'canvas-2d-set-filter! filter))
  (void))

;; canvas-2d-font : (or/c canvas-2d-context? external?) -> string?
;;   Read the CSS font value.
(define (canvas-2d-font ctx)
  (js-canvas2d-font (canvas-2d-context-unwrap ctx)))

;; canvas-2d-set-font! : (or/c canvas-2d-context? external?) (or/c string? symbol?) -> void?
;;   Set the CSS font value.
(define (canvas-2d-set-font! ctx font)
  (js-set-canvas2d-font! (canvas-2d-context-unwrap ctx)
                         (canvas-stringish->string 'canvas-2d-set-font! font))
  (void))

;; canvas-2d-global-alpha : (or/c canvas-2d-context? external?) -> real?
;;   Read the global alpha.
(define (canvas-2d-global-alpha ctx)
  (js-canvas2d-global-alpha (canvas-2d-context-unwrap ctx)))

;; canvas-2d-set-global-alpha! : (or/c canvas-2d-context? external?) real? -> void?
;;   Set the global alpha.
(define (canvas-2d-set-global-alpha! ctx alpha)
  (js-set-canvas2d-global-alpha! (canvas-2d-context-unwrap ctx) alpha)
  (void))

;; canvas-2d-global-composite-operation : (or/c canvas-2d-context? external?) -> string?
;;   Read the global composite operation.
(define (canvas-2d-global-composite-operation ctx)
  (js-canvas2d-global-composite-operation (canvas-2d-context-unwrap ctx)))

;; canvas-2d-set-global-composite-operation! : (or/c canvas-2d-context? external?) (or/c string? symbol?) -> void?
;;   Set the global composite operation.
(define (canvas-2d-set-global-composite-operation! ctx operation)
  (js-set-canvas2d-global-composite-operation!
   (canvas-2d-context-unwrap ctx)
   (canvas-stringish->string 'canvas-2d-set-global-composite-operation! operation))
  (void))

;; canvas-2d-image-smoothing-enabled? : (or/c canvas-2d-context? external?) -> boolean?
;;   Report whether image smoothing is enabled.
(define (canvas-2d-image-smoothing-enabled? ctx)
  (canvas-i32->boolean (js-canvas2d-image-smoothing-enabled (canvas-2d-context-unwrap ctx))))

;; canvas-2d-set-image-smoothing-enabled! : (or/c canvas-2d-context? external?) boolean? -> void?
;;   Set image smoothing on or off.
(define (canvas-2d-set-image-smoothing-enabled! ctx enabled)
  (js-set-canvas2d-image-smoothing-enabled! (canvas-2d-context-unwrap ctx)
                                            (canvas-boolean->i32 enabled))
  (void))

;; canvas-2d-image-smoothing-quality : (or/c canvas-2d-context? external?) -> string?
;;   Read the image smoothing quality.
(define (canvas-2d-image-smoothing-quality ctx)
  (js-canvas2d-image-smoothing-quality (canvas-2d-context-unwrap ctx)))

;; canvas-2d-set-image-smoothing-quality! : (or/c canvas-2d-context? external?) (or/c string? symbol?) -> void?
;;   Set the image smoothing quality.
(define (canvas-2d-set-image-smoothing-quality! ctx quality)
  (js-set-canvas2d-image-smoothing-quality! (canvas-2d-context-unwrap ctx)
                                            (canvas-stringish->string 'canvas-2d-set-image-smoothing-quality! quality))
  (void))

;; canvas-2d-line-cap : (or/c canvas-2d-context? external?) -> string?
;;   Read the line cap.
(define (canvas-2d-line-cap ctx)
  (js-canvas2d-line-cap (canvas-2d-context-unwrap ctx)))

;; canvas-2d-set-line-cap! : (or/c canvas-2d-context? external?) (or/c string? symbol?) -> void?
;;   Set the line cap.
(define (canvas-2d-set-line-cap! ctx value)
  (js-set-canvas2d-line-cap! (canvas-2d-context-unwrap ctx)
                             (canvas-stringish->string 'canvas-2d-set-line-cap! value))
  (void))

;; canvas-2d-line-dash-offset : (or/c canvas-2d-context? external?) -> real?
;;   Read the line dash offset.
(define (canvas-2d-line-dash-offset ctx)
  (js-canvas2d-line-dash-offset (canvas-2d-context-unwrap ctx)))

;; canvas-2d-set-line-dash-offset! : (or/c canvas-2d-context? external?) real? -> void?
;;   Set the line dash offset.
(define (canvas-2d-set-line-dash-offset! ctx offset)
  (js-set-canvas2d-line-dash-offset! (canvas-2d-context-unwrap ctx) offset)
  (void))

;; canvas-2d-line-join : (or/c canvas-2d-context? external?) -> string?
;;   Read the line join.
(define (canvas-2d-line-join ctx)
  (js-canvas2d-line-join (canvas-2d-context-unwrap ctx)))

;; canvas-2d-set-line-join! : (or/c canvas-2d-context? external?) (or/c string? symbol?) -> void?
;;   Set the line join.
(define (canvas-2d-set-line-join! ctx value)
  (js-set-canvas2d-line-join! (canvas-2d-context-unwrap ctx)
                              (canvas-stringish->string 'canvas-2d-set-line-join! value))
  (void))

;; canvas-2d-line-width : (or/c canvas-2d-context? external?) -> real?
;;   Read the line width.
(define (canvas-2d-line-width ctx)
  (js-canvas2d-line-width (canvas-2d-context-unwrap ctx)))

;; canvas-2d-set-line-width! : (or/c canvas-2d-context? external?) real? -> void?
;;   Set the line width.
(define (canvas-2d-set-line-width! ctx width)
  (js-set-canvas2d-line-width! (canvas-2d-context-unwrap ctx) width)
  (void))

;; canvas-2d-miter-limit : (or/c canvas-2d-context? external?) -> real?
;;   Read the miter limit.
(define (canvas-2d-miter-limit ctx)
  (js-canvas2d-miter-limit (canvas-2d-context-unwrap ctx)))

;; canvas-2d-set-miter-limit! : (or/c canvas-2d-context? external?) real? -> void?
;;   Set the miter limit.
(define (canvas-2d-set-miter-limit! ctx limit)
  (js-set-canvas2d-miter-limit! (canvas-2d-context-unwrap ctx) limit)
  (void))

;; canvas-2d-shadow-blur : (or/c canvas-2d-context? external?) -> real?
;;   Read the shadow blur.
(define (canvas-2d-shadow-blur ctx)
  (js-canvas2d-shadow-blur (canvas-2d-context-unwrap ctx)))

;; canvas-2d-set-shadow-blur! : (or/c canvas-2d-context? external?) real? -> void?
;;   Set the shadow blur.
(define (canvas-2d-set-shadow-blur! ctx blur)
  (js-set-canvas2d-shadow-blur! (canvas-2d-context-unwrap ctx) blur)
  (void))

;; canvas-2d-shadow-color : (or/c canvas-2d-context? external?) -> string?
;;   Read the shadow color.
(define (canvas-2d-shadow-color ctx)
  (js-canvas2d-shadow-color (canvas-2d-context-unwrap ctx)))

;; canvas-2d-set-shadow-color! : (or/c canvas-2d-context? external?) (or/c string? symbol?) -> void?
;;   Set the shadow color.
(define (canvas-2d-set-shadow-color! ctx color)
  (js-set-canvas2d-shadow-color! (canvas-2d-context-unwrap ctx)
                                 (canvas-stringish->string 'canvas-2d-set-shadow-color! color))
  (void))

;; canvas-2d-shadow-offset-x : (or/c canvas-2d-context? external?) -> real?
;;   Read the horizontal shadow offset.
(define (canvas-2d-shadow-offset-x ctx)
  (js-canvas2d-shadow-offset-x (canvas-2d-context-unwrap ctx)))

;; canvas-2d-set-shadow-offset-x! : (or/c canvas-2d-context? external?) real? -> void?
;;   Set the horizontal shadow offset.
(define (canvas-2d-set-shadow-offset-x! ctx dx)
  (js-set-canvas2d-shadow-offset-x! (canvas-2d-context-unwrap ctx) dx)
  (void))

;; canvas-2d-shadow-offset-y : (or/c canvas-2d-context? external?) -> real?
;;   Read the vertical shadow offset.
(define (canvas-2d-shadow-offset-y ctx)
  (js-canvas2d-shadow-offset-y (canvas-2d-context-unwrap ctx)))

;; canvas-2d-set-shadow-offset-y! : (or/c canvas-2d-context? external?) real? -> void?
;;   Set the vertical shadow offset.
(define (canvas-2d-set-shadow-offset-y! ctx dy)
  (js-set-canvas2d-shadow-offset-y! (canvas-2d-context-unwrap ctx) dy)
  (void))

;; canvas-2d-text-align : (or/c canvas-2d-context? external?) -> string?
;;   Read the text alignment.
(define (canvas-2d-text-align ctx)
  (js-canvas2d-text-align (canvas-2d-context-unwrap ctx)))

;; canvas-2d-set-text-align! : (or/c canvas-2d-context? external?) (or/c string? symbol?) -> void?
;;   Set the text alignment.
(define (canvas-2d-set-text-align! ctx alignment)
  (js-set-canvas2d-text-align! (canvas-2d-context-unwrap ctx)
                               (canvas-stringish->string 'canvas-2d-set-text-align! alignment))
  (void))

;; canvas-2d-text-baseline : (or/c canvas-2d-context? external?) -> string?
;;   Read the text baseline.
(define (canvas-2d-text-baseline ctx)
  (js-canvas2d-text-baseline (canvas-2d-context-unwrap ctx)))

;; canvas-2d-set-text-baseline! : (or/c canvas-2d-context? external?) (or/c string? symbol?) -> void?
;;   Set the text baseline.
(define (canvas-2d-set-text-baseline! ctx baseline)
  (js-set-canvas2d-text-baseline! (canvas-2d-context-unwrap ctx)
                                  (canvas-stringish->string 'canvas-2d-set-text-baseline! baseline))
  (void))

;; canvas-2d-text-rendering : (or/c canvas-2d-context? external?) -> string?
;;   Read the text rendering hint.
(define (canvas-2d-text-rendering ctx)
  (js-canvas2d-text-rendering (canvas-2d-context-unwrap ctx)))

;; canvas-2d-set-text-rendering! : (or/c canvas-2d-context? external?) (or/c string? symbol?) -> void?
;;   Set the text rendering hint.
(define (canvas-2d-set-text-rendering! ctx mode)
  (js-set-canvas2d-text-rendering! (canvas-2d-context-unwrap ctx)
                                   (canvas-stringish->string 'canvas-2d-set-text-rendering! mode))
  (void))

;; canvas-2d-font-kerning : (or/c canvas-2d-context? external?) -> string?
;;   Read the font kerning mode.
(define (canvas-2d-font-kerning ctx)
  (js-canvas2d-font-kerning (canvas-2d-context-unwrap ctx)))

;; canvas-2d-set-font-kerning! : (or/c canvas-2d-context? external?) (or/c string? symbol?) -> void?
;;   Set the font kerning mode.
(define (canvas-2d-set-font-kerning! ctx mode)
  (js-set-canvas2d-font-kerning! (canvas-2d-context-unwrap ctx)
                                 (canvas-stringish->string 'canvas-2d-set-font-kerning! mode))
  (void))

;; canvas-2d-font-stretch : (or/c canvas-2d-context? external?) -> string?
;;   Read the font stretch.
(define (canvas-2d-font-stretch ctx)
  (js-canvas2d-font-stretch (canvas-2d-context-unwrap ctx)))

;; canvas-2d-set-font-stretch! : (or/c canvas-2d-context? external?) (or/c string? symbol?) -> void?
;;   Set the font stretch.
(define (canvas-2d-set-font-stretch! ctx stretch)
  (js-set-canvas2d-font-stretch! (canvas-2d-context-unwrap ctx)
                                 (canvas-stringish->string 'canvas-2d-set-font-stretch! stretch))
  (void))

;; canvas-2d-font-variant-caps : (or/c canvas-2d-context? external?) -> string?
;;   Read the font variant caps.
(define (canvas-2d-font-variant-caps ctx)
  (js-canvas2d-font-variant-caps (canvas-2d-context-unwrap ctx)))

;; canvas-2d-set-font-variant-caps! : (or/c canvas-2d-context? external?) (or/c string? symbol?) -> void?
;;   Set the font variant caps.
(define (canvas-2d-set-font-variant-caps! ctx variant)
  (js-set-canvas2d-font-variant-caps! (canvas-2d-context-unwrap ctx)
                                      (canvas-stringish->string 'canvas-2d-set-font-variant-caps! variant))
  (void))

;; canvas-2d-font-variant-ligatures : (or/c canvas-2d-context? external?) -> string?
;;   Read the font variant ligatures.
(define (canvas-2d-font-variant-ligatures ctx)
  (js-canvas2d-font-variant-ligatures (canvas-2d-context-unwrap ctx)))

;; canvas-2d-set-font-variant-ligatures! : (or/c canvas-2d-context? external?) (or/c string? symbol?) -> void?
;;   Set the font variant ligatures.
(define (canvas-2d-set-font-variant-ligatures! ctx variant)
  (js-set-canvas2d-font-variant-ligatures! (canvas-2d-context-unwrap ctx)
                                           (canvas-stringish->string 'canvas-2d-set-font-variant-ligatures! variant))
  (void))

;; canvas-2d-font-variant-numeric : (or/c canvas-2d-context? external?) -> string?
;;   Read the font variant numeric value.
(define (canvas-2d-font-variant-numeric ctx)
  (js-canvas2d-font-variant-numeric (canvas-2d-context-unwrap ctx)))

;; canvas-2d-set-font-variant-numeric! : (or/c canvas-2d-context? external?) (or/c string? symbol?) -> void?
;;   Set the font variant numeric value.
(define (canvas-2d-set-font-variant-numeric! ctx variant)
  (js-set-canvas2d-font-variant-numeric! (canvas-2d-context-unwrap ctx)
                                         (canvas-stringish->string 'canvas-2d-set-font-variant-numeric! variant))
  (void))

;; canvas-2d-letter-spacing : (or/c canvas-2d-context? external?) -> string?
;;   Read the letter spacing.
(define (canvas-2d-letter-spacing ctx)
  (js-canvas2d-letter-spacing (canvas-2d-context-unwrap ctx)))

;; canvas-2d-set-letter-spacing! : (or/c canvas-2d-context? external?) (or/c string? symbol?) -> void?
;;   Set the letter spacing.
(define (canvas-2d-set-letter-spacing! ctx spacing)
  (js-set-canvas2d-letter-spacing! (canvas-2d-context-unwrap ctx)
                                   (canvas-stringish->string 'canvas-2d-set-letter-spacing! spacing))
  (void))

;; canvas-2d-word-spacing : (or/c canvas-2d-context? external?) -> string?
;;   Read the word spacing.
(define (canvas-2d-word-spacing ctx)
  (js-canvas2d-word-spacing (canvas-2d-context-unwrap ctx)))

;; canvas-2d-set-word-spacing! : (or/c canvas-2d-context? external?) (or/c string? symbol?) -> void?
;;   Set the word spacing.
(define (canvas-2d-set-word-spacing! ctx spacing)
  (js-set-canvas2d-word-spacing! (canvas-2d-context-unwrap ctx)
                                 (canvas-stringish->string 'canvas-2d-set-word-spacing! spacing))
  (void))

;; canvas-2d-arc-to : (or/c canvas-2d-context? external?) real? real? real? real? real? -> void?
;;   Add an arc-to segment to the current path.
(define (canvas-2d-arc-to ctx x1 y1 x2 y2 radius)
  (js-canvas2d-arc-to (canvas-2d-context-unwrap ctx) x1 y1 x2 y2 radius)
  (void))

;; canvas-2d-bezier-curve-to : (or/c canvas-2d-context? external?) real? real? real? real? real? real? -> void?
;;   Add a cubic Bezier curve to the current path.
(define (canvas-2d-bezier-curve-to ctx cp1x cp1y cp2x cp2y x y)
  (js-canvas2d-bezier-curve-to (canvas-2d-context-unwrap ctx) cp1x cp1y cp2x cp2y x y)
  (void))

;; canvas-2d-clip : (or/c canvas-2d-context? external?) [any/c #f] [any/c #f] -> void?
;;   Clip drawing to the current path or the provided path.
(define (canvas-2d-clip ctx [path #f] [fill-rule #f])
  (js-canvas2d-clip (canvas-2d-context-unwrap ctx)
                    (canvas-resolve-optional path)
                    (canvas-fill-rule->js fill-rule))
  (void))

;; canvas-2d-create-image-data : (or/c canvas-2d-context? external?) exact-nonnegative-integer? exact-nonnegative-integer? -> canvas-image-data?
;;   Create a new blank image buffer.
(define (canvas-2d-create-image-data ctx width height)
  (canvas-image-data-wrap
   (js-send/extern (canvas-2d-context-unwrap ctx) "createImageData" (vector width height))))

;; canvas-2d-create-image-data-from : (or/c canvas-2d-context? external?) (or/c canvas-image-data? external?) -> canvas-image-data?
;;   Copy an image buffer.
(define (canvas-2d-create-image-data-from ctx data)
  (canvas-image-data-wrap
   (js-send/extern (canvas-2d-context-unwrap ctx)
                   "createImageData"
                   (vector (canvas-image-data-unwrap data)))))

;; canvas-2d-create-pattern : (or/c canvas-2d-context? external?) any/c (or/c string? symbol?) -> (or/c #f canvas-pattern?)
;;   Create a pattern from an image or canvas source.
(define (canvas-2d-create-pattern ctx source repetition)
  (define pattern (js-canvas2d-create-pattern (canvas-2d-context-unwrap ctx)
                                              (canvas-source-unwrap source)
                                              (canvas-stringish->string 'canvas-2d-create-pattern repetition)))
  (and pattern (canvas-pattern-wrap pattern)))

;; canvas-2d-create-conic-gradient : (or/c canvas-2d-context? external?) real? real? real? -> canvas-gradient?
;;   Create a conic gradient.
(define (canvas-2d-create-conic-gradient ctx angle x y)
  (canvas-gradient-wrap
   (js-send/extern (canvas-2d-context-unwrap ctx) "createConicGradient" (vector angle x y))))

;; canvas-2d-draw-focus-if-needed! : (or/c canvas-2d-context? external?) any/c -> void?
;;   Draw a focus ring when needed.
(define (canvas-2d-draw-focus-if-needed! ctx element)
  (js-canvas2d-draw-focus-if-needed! (canvas-2d-context-unwrap ctx) element)
  (void))

;; canvas-2d-draw-focus-if-needed-path! : (or/c canvas-2d-context? external?) any/c any/c -> void?
;;   Draw a focus ring for the given path and element.
(define (canvas-2d-draw-focus-if-needed-path! ctx path element)
  (js-canvas2d-draw-focus-if-needed-path! (canvas-2d-context-unwrap ctx) path element)
  (void))

;; canvas-2d-draw-image : (or/c canvas-2d-context? external?) any/c real? real? -> void?
;;   Draw an image source at a point.
(define (canvas-2d-draw-image ctx source dx dy)
  (js-canvas2d-draw-image (canvas-2d-context-unwrap ctx)
                          (canvas-source-unwrap source)
                          dx
                          dy)
  (void))

;; canvas-2d-draw-image-5 : (or/c canvas-2d-context? external?) any/c real? real? real? real? -> void?
;;   Draw an image source with destination sizing.
(define (canvas-2d-draw-image-5 ctx source dx dy dwidth dheight)
  (js-canvas2d-draw-image-5 (canvas-2d-context-unwrap ctx)
                            (canvas-source-unwrap source)
                            dx
                            dy
                            dwidth
                            dheight)
  (void))

;; canvas-2d-draw-image-9 : (or/c canvas-2d-context? external?) any/c real? real? real? real? real? real? real? real? -> void?
;;   Draw an image source with source and destination rectangles.
(define (canvas-2d-draw-image-9 ctx source sx sy sw sh dx dy dw dh)
  (js-canvas2d-draw-image-9 (canvas-2d-context-unwrap ctx)
                            (canvas-source-unwrap source)
                            sx
                            sy
                            sw
                            sh
                            dx
                            dy
                            dw
                            dh)
  (void))

;; canvas-2d-ellipse : (or/c canvas-2d-context? external?) real? real? real? real? real? real? real? -> void?
;;   Add an ellipse to the current path.
(define (canvas-2d-ellipse ctx x y rx ry rotation start-angle end-angle [anticlockwise #f])
  (js-canvas2d-ellipse (canvas-2d-context-unwrap ctx)
                       x y rx ry rotation start-angle end-angle
                       (canvas-boolean->i32 anticlockwise))
  (void))

;; canvas-2d-get-image-data : (or/c canvas-2d-context? external?) real? real? real? real? [any/c #f] -> canvas-image-data?
;;   Read pixels from the canvas.
(define (canvas-2d-get-image-data ctx sx sy sw sh [settings #f])
  (canvas-image-data-wrap
   (if (eq? settings #f)
       (js-send/extern (canvas-2d-context-unwrap ctx) "getImageData" (vector sx sy sw sh))
       (js-send/extern (canvas-2d-context-unwrap ctx) "getImageData" (vector sx sy sw sh settings)))))

;; canvas-2d-get-line-dash : (or/c canvas-2d-context? external?) -> vector?
;;   Read the current dash pattern.
(define (canvas-2d-get-line-dash ctx)
  (array->vector (js-canvas2d-get-line-dash (canvas-2d-context-unwrap ctx))))

;; canvas-2d-get-transform : (or/c canvas-2d-context? external?) -> canvas-dom-matrix?
;;   Read the current transform matrix.
(define (canvas-2d-get-transform ctx)
  (canvas-dom-matrix-wrap (js-canvas2d-get-transform (canvas-2d-context-unwrap ctx))))

;; canvas-2d-is-point-in-path : (or/c canvas-2d-context? external?) [any/c #f] real? real? [any/c #f] -> boolean?
;;   Check whether a point lies within the path.
(define canvas-2d-is-point-in-path
  (case-lambda
    [(ctx x y)
     (canvas-i32->boolean
      (js-canvas2d-is-point-in-path (canvas-2d-context-unwrap ctx)
                                    (void)
                                    x
                                    y
                                    (void)))]
    [(ctx path x y)
     (canvas-i32->boolean
      (js-canvas2d-is-point-in-path (canvas-2d-context-unwrap ctx)
                                    (canvas-resolve-optional path)
                                    x
                                    y
                                    (void)))]
    [(ctx path x y fill-rule)
     (canvas-i32->boolean
      (js-canvas2d-is-point-in-path (canvas-2d-context-unwrap ctx)
                                    (canvas-resolve-optional path)
                                    x
                                    y
                                    (canvas-fill-rule->js fill-rule)))]))

;; canvas-2d-is-point-in-stroke : (or/c canvas-2d-context? external?) [any/c #f] real? real? -> boolean?
;;   Check whether a point lies within the stroke.
(define canvas-2d-is-point-in-stroke
  (case-lambda
    [(ctx x y)
     (canvas-i32->boolean
      (js-canvas2d-is-point-in-stroke (canvas-2d-context-unwrap ctx)
                                      (void)
                                      x
                                      y))]
    [(ctx path x y)
     (canvas-i32->boolean
      (js-canvas2d-is-point-in-stroke (canvas-2d-context-unwrap ctx)
                                      (canvas-resolve-optional path)
                                      x
                                      y))]))

;; canvas-2d-put-image-data : (or/c canvas-2d-context? external?) (or/c canvas-image-data? external?) real? real? [any/c #f] [any/c #f] [any/c #f] [any/c #f] -> void?
;;   Write pixels back to the canvas.
(define (canvas-2d-put-image-data ctx data dx dy [dirty-x #f] [dirty-y #f] [dirty-width #f] [dirty-height #f])
  (js-canvas2d-put-image-data (canvas-2d-context-unwrap ctx)
                              (canvas-image-data-unwrap data)
                              dx
                              dy
                              (canvas-resolve-optional dirty-x)
                              (canvas-resolve-optional dirty-y)
                              (canvas-resolve-optional dirty-width)
                              (canvas-resolve-optional dirty-height))
  (void))

;; canvas-2d-quadratic-curve-to : (or/c canvas-2d-context? external?) real? real? real? real? -> void?
;;   Add a quadratic curve to the current path.
(define (canvas-2d-quadratic-curve-to ctx cpx cpy x y)
  (js-canvas2d-quadratic-curve-to (canvas-2d-context-unwrap ctx) cpx cpy x y)
  (void))

;; canvas-2d-rect : (or/c canvas-2d-context? external?) real? real? real? real? -> void?
;;   Add a rectangle to the current path.
(define (canvas-2d-rect ctx x y width height)
  (js-canvas2d-rect (canvas-2d-context-unwrap ctx) x y width height)
  (void))

;; canvas-2d-reset : (or/c canvas-2d-context? external?) -> void?
;;   Reset the entire drawing state.
(define (canvas-2d-reset ctx)
  (js-canvas2d-reset (canvas-2d-context-unwrap ctx))
  (void))

;; canvas-2d-reset-transform : (or/c canvas-2d-context? external?) -> void?
;;   Reset the transform matrix.
(define (canvas-2d-reset-transform ctx)
  (js-canvas2d-reset-transform (canvas-2d-context-unwrap ctx))
  (void))

;; canvas-2d-round-rect : (or/c canvas-2d-context? external?) real? real? real? real? [any/c #f] -> void?
;;   Add a rounded rectangle to the current path.
(define (canvas-2d-round-rect ctx x y width height [radii #f])
  (js-canvas2d-round-rect (canvas-2d-context-unwrap ctx)
                          x
                          y
                          width
                          height
                          (if (or (list? radii) (vector? radii))
                              (sequence->array radii)
                              radii))
  (void))

;; canvas-2d-set-line-dash : (or/c canvas-2d-context? external?) any/c -> void?
;;   Set the dash pattern.
(define (canvas-2d-set-line-dash ctx segments)
  (js-canvas2d-set-line-dash (canvas-2d-context-unwrap ctx)
                             (sequence->array segments))
  (void))

;; canvas-2d-set-transform! : (or/c canvas-2d-context? external?) real? real? real? real? real? real? -> void?
;;   Set the transform matrix using six numbers.
(define (canvas-2d-set-transform! ctx a b c d e f)
  (js-canvas2d-set-transform! (canvas-2d-context-unwrap ctx) a b c d e f)
  (void))

;; canvas-2d-set-transform-matrix! : (or/c canvas-2d-context? external?) (or/c canvas-dom-matrix? external?) -> void?
;;   Set the transform matrix from a DOMMatrix.
(define (canvas-2d-set-transform-matrix! ctx matrix)
  (js-canvas2d-set-transform-matrix! (canvas-2d-context-unwrap ctx)
                                     (canvas-matrix->raw matrix))
  (void))

;; canvas-2d-transform : (or/c canvas-2d-context? external?) real? real? real? real? real? real? -> void?
;;   Apply an affine transform to the current matrix.
(define (canvas-2d-transform ctx a b c d e f)
  (js-canvas2d-transform (canvas-2d-context-unwrap ctx) a b c d e f)
  (void))

;; canvas-2d-create-linear-gradient : (or/c canvas-2d-context? external?) real? real? real? real? -> canvas-gradient?
;;   Create a linear gradient.
(define (canvas-2d-create-linear-gradient ctx x0 y0 x1 y1)
  (canvas-gradient-wrap
   (js-send/extern (canvas-2d-context-unwrap ctx) "createLinearGradient" (vector x0 y0 x1 y1))))

;; canvas-2d-create-radial-gradient : (or/c canvas-2d-context? external?) real? real? real? real? real? real? -> canvas-gradient?
;;   Create a radial gradient.
(define (canvas-2d-create-radial-gradient ctx x0 y0 r0 x1 y1 r1)
  (canvas-gradient-wrap
   (js-send/extern (canvas-2d-context-unwrap ctx) "createRadialGradient" (vector x0 y0 r0 x1 y1 r1))))

;; canvas-2d-fill-style : (or/c canvas-2d-context? external?) -> any/c
;;   Read the fill style.
(define (canvas-2d-fill-style ctx)
  (canvas-paint-style-wrap (js-canvas2d-fill-style (canvas-2d-context-unwrap ctx))))

;; canvas-2d-set-fill-style! : (or/c canvas-2d-context? external?) any/c -> void?
;;   Set the fill style.
(define (canvas-2d-set-fill-style! ctx style)
  (js-set-canvas2d-fill-style! (canvas-2d-context-unwrap ctx)
                               (canvas-paint-style-unwrap style))
  (void))

;; canvas-2d-stroke-style : (or/c canvas-2d-context? external?) -> any/c
;;   Read the stroke style.
(define (canvas-2d-stroke-style ctx)
  (canvas-paint-style-wrap (js-canvas2d-stroke-style (canvas-2d-context-unwrap ctx))))

;; canvas-2d-set-stroke-style! : (or/c canvas-2d-context? external?) any/c -> void?
;;   Set the stroke style.
(define (canvas-2d-set-stroke-style! ctx style)
  (js-set-canvas2d-stroke-style! (canvas-2d-context-unwrap ctx)
                                 (canvas-paint-style-unwrap style))
  (void))
