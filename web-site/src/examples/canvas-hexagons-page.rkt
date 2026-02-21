;;;
;;; Glowing Hexagons demo page
;;; Original JavaScript version by Matei Copot:
;;; https://codepen.io/towc/pen/mJzOWJ
;;;

(define (canvas-hexagons-page)
  `(div (@ (class "page page--canvas-hexagons"))
        ,(navbar)
        (section (@ (class "mathjax-hero"))
                 (div (@ (class "hero-panel"))
                      (div (@ (class "pill-row"))
                           (span (@ (class "pill")) "Canvas")
                           (span (@ (class "pill")) "Animation")
                           (span (@ (class "pill")) "Glow"))
                      (h1 (@ (class "hero-title")) "Glowing Hexagons")
                      (p (@ (class "hero-lead"))
                         "Animated glowing hexagons arranged in a honeycomb pattern.")))
        (section (@ (class "section section--mathjax"))
                 (div (@ (class "section-content"))
                      (div (@ (id "canvas-hexagons-root")
                              (class "canvas-hexagons-stage")))))
        (section (@ (class "section section--mathjax-details"))
                 (div (@ (class "section-content"))
                      (div (@ (class "mathjax-details canvas-credits"))
                           (p (@ (class "canvas-credits-label")) "Credits")
                           (p "Original JavaScript animation ported to WebRacket.")
                           (p "Original JavaScript version by "
                              (a (@ (href "https://github.com/towc")
                                    (target "_blank")
                                    (rel "noreferrer noopener"))
                                 "Matei Copot")
                              ".")
                           (div (@ (class "mathjax-actions canvas-credits-actions"))
                                ,(code-pill "https://codepen.io/towc/pen/mJzOWJ"
                                            "Original JavaScript source")
                                ,(code-pill (gh-file "web-site/src/examples/canvas-hexagons-page.rkt")
                                            "Web-site source")))))
        ,(footer-section)))

;; inexact : Number -> Inexact-Number
;;   Converts an exact or inexact number to inexact.
(define (inexact x)
  (if (exact? x) (exact->inexact x) x))

;; hsl-color : Number Number -> String
;;   Builds an HSL color string with fixed saturation for the glow effect.
(define (hsl-color hue light)
  (string-append "hsl("
                 (number->string (inexact hue))
                 ",100%,"
                 (number->string (inexact light))
                 "%)"))

;; Opts from the JS version
(define opts-len                         20.)   ; Pixel scale factor for the hex-grid coordinate system.
(define opts-count                       50)    ; Maximum number of active animated lines.
(define opts-base-time                   10.)   ; Minimum frame count for a line phase.
(define opts-added-time                  10.)   ; Extra random phase duration added to base-time.
(define opts-die-chance                   0.05) ; Per-phase chance that a line resets.
(define opts-spawn-chance                 1.0)  ; Per-frame probability to spawn a new line.
(define opts-spark-chance                 0.1)  ; Per-step chance to draw a spark particle.
(define opts-spark-dist                  10.)   ; Maximum spark offset distance in pixels.
(define opts-spark-size                   2.)   ; Spark rectangle size in pixels.
(define opts-base-light                  50.)   ; Baseline HSL lightness.
(define opts-added-light                 10.)   ; Oscillation amplitude added to base lightness.
(define opts-shadow-to-time-prop-mult     6.)   ; Shadow blur multiplier based on phase progress.
(define opts-base-light-input-multiplier  0.01) ; Base frequency for light oscillation.
(define opts-added-light-input-multiplier 0.02) ; Random extra oscillation frequency.
(define opts-repaint-alpha                0.04) ; Trail persistence via translucent repaint.
(define opts-hue-change                   0.1)  ; Hue increment per global tick.

(define base-rad (/ (* pi 2.0) 6.0))

;; Runtime state
(define hex-started?        #f) ; Guards one-time page initialization.
(define hex-canvas          #f) ; Canvas element used for rendering.
(define hex-ctx             #f) ; 2D drawing context for the canvas.
(define hex-w               0.) ; Current canvas width in pixels.
(define hex-h               0.) ; Current canvas height in pixels.
(define hex-cx              0.) ; Cached horizontal center coordinate.
(define hex-cy              0.) ; Cached vertical center coordinate.
(define hex-die-x           0.) ; Horizontal bound in grid-space before reset.
(define hex-die-y           0.) ; Vertical bound in grid-space before reset.
(define hex-tick            0.) ; Global frame counter used for hue progression.
(define hex-loop-external   #f) ; External callback for requestAnimationFrame loop.
(define hex-resize-external #f) ; External callback for window resize listener.

;; line fields:
;;   x/y:                     current base position in hex-grid coordinates.
;;   added-x/added-y:         unit direction vector for the active phase.
;;   rad:                     current travel angle in radians (changes in 60-degree steps).
;;   light-input-multiplier:  per-line sine frequency for light pulsing.
;;   hue:                     line hue value used in HSL color generation.
;;   cumulative-time:         total step count used for light oscillation.
;;   time:                    elapsed frames in current phase.
;;   target-time:             phase duration threshold before turning to next segment.
(struct line (x y added-x added-y rad light-input-multiplier hue cumulative-time time target-time)
  #:mutable)

(define hex-lines '())

(define (line-new)
  (line 0. 0. 0. 0. 0. 0. 0. 0. 0 1))

;; update-hex-bounds! : -> Void
;;   Recomputes center and line-death bounds from current canvas size.
(define (update-hex-bounds!)
  (set! hex-cx    (/ hex-w 2.0))
  (set! hex-cy    (/ hex-h 2.0))
  (set! hex-die-x (/ (/ hex-w 2.0) opts-len))
  (set! hex-die-y (/ (/ hex-h 2.0) opts-len)))

;; line-reset! : Line -> Void
;;   Initializes a line to origin state and starts its first movement phase.
(define (line-reset! ln)
  (set-line-x!       ln 0.0)
  (set-line-y!       ln 0.0)
  (set-line-added-x! ln 0.0)
  (set-line-added-y! ln 0.0)
  (set-line-rad!     ln 0.0)
  (set-line-light-input-multiplier!
   ln
   (+ opts-base-light-input-multiplier
      (* opts-added-light-input-multiplier (random))))
  (set-line-hue! ln (* (inexact hex-tick) opts-hue-change))
  (set-line-cumulative-time! ln 0.0)
  (line-begin-phase! ln))

;; line-begin-phase! : Line -> Void
;;   Advances a line to a new hex-direction segment and restarts phase timing.
(define (line-begin-phase! ln)
  (set-line-x!    ln (+ (line-x ln) (line-added-x ln)))
  (set-line-y!    ln (+ (line-y ln) (line-added-y ln)))
  (set-line-time! ln 0)
  (set-line-target-time!
   ln
   (inexact->exact
    (floor (+ opts-base-time (* opts-added-time (random))))))
  (set-line-rad!     ln (+ (line-rad ln) (* base-rad (if (< (random) 0.5) 1.0 -1.0))))
  (set-line-added-x! ln (cos (line-rad ln)))
  (set-line-added-y! ln (sin (line-rad ln)))
  (when (or (< (random) opts-die-chance)
            (> (line-x ln)    hex-die-x)
            (< (line-x ln) (- hex-die-x))
            (> (line-y ln)    hex-die-y)
            (< (line-y ln) (- hex-die-y)))
    (line-reset! ln)))

;; line-step! : Line -> Void
;;   Updates line animation state and renders one glowing point (plus optional spark).
(define (line-step! ln)
  (set-line-time! ln (+ (line-time ln) 1))
  (set-line-cumulative-time! ln (+ (line-cumulative-time ln) 1.0))
  (when (>= (line-time ln) (line-target-time ln))
    (line-begin-phase! ln))
  (define prop (/ (inexact (line-time ln)) (max 1.0 (inexact (line-target-time ln)))))
  (define wave (sin (/ (* prop pi) 2.0)))
  (define x    (* (line-added-x ln) wave))
  (define y    (* (line-added-y ln) wave))
  (define light
    (+ opts-base-light
       (* opts-added-light
          (sin (* (line-cumulative-time ln)
                  (line-light-input-multiplier ln))))))
  (define color (hsl-color (line-hue ln) light))
  (define px    (+ hex-cx (* (+ (line-x ln) x) opts-len)))
  (define py    (+ hex-cy (* (+ (line-y ln) y) opts-len)))
  (js-set-canvas2d-shadow-blur! hex-ctx (* prop opts-shadow-to-time-prop-mult))
  (js-set-canvas2d-fill-style! hex-ctx color)
  (js-set-canvas2d-shadow-color! hex-ctx color)
  (js-canvas2d-fill-rect hex-ctx px py 2.0 2.0)
  (when (< (random) opts-spark-chance)
    (define spark-x
      (+ px
         (* (random) opts-spark-dist (if (< (random) 0.5) 1.0 -1.0))
         (/ opts-spark-size -2.0)))
    (define spark-y
      (+ py
         (* (random) opts-spark-dist (if (< (random) 0.5) 1.0 -1.0))
         (/ opts-spark-size -2.0)))
    (js-canvas2d-fill-rect hex-ctx spark-x spark-y opts-spark-size opts-spark-size)))

;; hex-loop : Any -> Void
;;   Runs one animation frame: fade background, spawn lines, step all lines, reschedule.
(define (hex-loop _)
  (js-window-request-animation-frame hex-loop-external)
  (set! hex-tick (+ hex-tick 1))
  (js-set-canvas2d-global-composite-operation! hex-ctx "source-over")
  (js-set-canvas2d-shadow-blur! hex-ctx 0.0)
  (js-set-canvas2d-fill-style!
   hex-ctx
   (string-append "rgba(0,0,0," (number->string opts-repaint-alpha) ")"))
  (js-canvas2d-fill-rect hex-ctx 0.0 0.0 hex-w hex-h)
  (js-set-canvas2d-global-composite-operation! hex-ctx "lighter")
  (when (and (< (length hex-lines) opts-count)
             (< (random) opts-spawn-chance))
    (define ln (line-new))
    (line-reset! ln)
    (set! hex-lines (cons ln hex-lines)))
  (for ([ln (in-list hex-lines)])
    (line-step! ln)))

;; resize-hex-canvas! : Any -> Void
;;   Resizes canvas to viewport, clears background, and updates derived bounds.
(define (resize-hex-canvas! _)
  (define w-int
    (inexact->exact (floor (inexact (js-window-inner-width)))))
  (define h-int
    (inexact->exact (floor (inexact (js-window-inner-height)))))
  (set! hex-w (inexact w-int))
  (set! hex-h (inexact h-int))
  (js-set-canvas-width!  hex-canvas w-int)
  (js-set-canvas-height! hex-canvas h-int)
  (js-set-canvas2d-fill-style! hex-ctx "black")
  (js-canvas2d-fill-rect hex-ctx 0.0 0.0 hex-w hex-h)
  (update-hex-bounds!))

;; init-canvas-hexagons-page! : -> Void
;;   Creates canvas DOM state, hooks resize handler, and starts RAF animation loop.
(define (init-canvas-hexagons-page!)
  (when (not hex-started?)
    (set! hex-started? #t)
    (js-set! (js-var "document") "title" "Glowing Hexagons")
    (define root (js-get-element-by-id "canvas-hexagons-root"))
    (when (js-nullish? root)
      (error 'canvas-hexagons "missing #canvas-hexagons-root container"))
    (set! hex-canvas (js-create-element "canvas"))
    ;; From original CSS:
    ;; canvas { position: absolute; top: 0; left: 0; }
    (js-set-attribute! hex-canvas "style"
                       (string-append
                        "position: absolute; "
                        "top: 0; left: 0; "
                        "width: 100%; height: 100%; "
                        "display: block;"))
    (js-append-child! root hex-canvas)
    (set! hex-ctx (js-canvas-get-context hex-canvas "2d" (js-undefined)))
    (resize-hex-canvas! (void))
    (set! hex-lines '())
    (set! hex-tick 0.)
    (set! hex-loop-external (procedure->external hex-loop))
    (set! hex-resize-external (procedure->external resize-hex-canvas!))
    (js-add-event-listener! (js-window-window) "resize" hex-resize-external)
    (js-window-request-animation-frame hex-loop-external)))
