;;;
;;; Pict Fish School
;;;

(include-lib web-easy)
(define ui-window window)
(define ui-container container)
(define ui-vpanel vpanel)
(define ui-hpanel hpanel)
(define ui-group group)
(define ui-h1 h1)
(define ui-text text)
(define ui-slider slider)
(define ui-button button)
(define ui-div Div)
(define ui-canvas Canvas)
(define ui-theme theme)
(define ui-install-theme-manager! install-theme-manager!)
(define ui-render render)
(define ui-mount-renderer! mount-renderer!)

(include-lib web-pict)

(define canvas-name "pict-fish-school-canvas")
(define canvas-width 860.)
(define canvas-height 500.)
(define frame-delay-ms 33.0)
(define splash-radius 230.0)
(define floor-height 82.0)

(define @fish-count (@ 8))
(define @speed-percent (@ 100))
(define @mouth-open (@ 24))
(define @paused (@ #f))
(define @status (@ "Click the water to make the school scatter."))

(struct fish (id x y width height direction base-speed amplitude phase color burst)
  #:transparent)
(struct ripple (x y age max-age)
  #:transparent)

(define fish-colors
  '("royalblue" "steelblue" "darkcyan" "teal" "salmon" "goldenrod" "mediumpurple"))

(define fishes '())
(define ripples '())
(define next-fish-id 0)
(define frame-callback #f)
(define click-callback #f)
(define animation-running? #f)
(define click-listener-installed? #f)

;; next-id : -> exact-integer?
;;   Produce a fresh fish identifier.
(define (next-id)
  (begin0 next-fish-id
    (set! next-fish-id (add1 next-fish-id))))

;; clamp : real? real? real? -> real?
;;   Clamp v to the inclusive range [lo, hi].
(define (clamp v lo hi)
  (max lo (min hi v)))

;; rand-between : real? real? -> real?
;;   Return an inexact random number between lo and hi.
(define (rand-between lo hi)
  (+ lo (* (random) (- hi lo))))

;; random-ref : (non-empty-listof any/c) -> any/c
;;   Pick a random element from xs.
(define (random-ref xs)
  (list-ref xs (random (length xs))))

;; speed-factor : -> real?
;;   Convert the speed slider to a multiplicative factor.
(define (speed-factor)
  (/ (obs-peek @speed-percent) 100.0))

;; mouth-factor : -> real?
;;   Convert the mouth slider to a 0..1 openness value.
(define (mouth-factor)
  (/ (obs-peek @mouth-open) 100.0))

;; fish-center-y : fish? -> real?
;;   Compute the fish center with vertical bobbing applied.
(define (fish-center-y f)
  (+ (fish-y f)
     (* (fish-amplitude f)
        (sin (fish-phase f)))))

;; fish-open-mouth : fish? -> real?
;;   Modulate mouth openness so the school feels animated.
(define (fish-open-mouth f)
  (define pulse (* 0.5 (+ 1.0 (sin (+ (fish-phase f) 0.9)))))
  (clamp (* (mouth-factor) (+ 0.35 (* 0.65 pulse))) 0.0 1.0))

;; make-random-fish : -> fish?
;;   Create one fish with a random lane, size, and cruise speed.
(define (make-random-fish)
  (define width (rand-between 90.0 160.0))
  (define height (* width (rand-between 0.42 0.58)))
  (define direction (if (zero? (random 2)) 'left 'right))
  (define x (rand-between 0.0 (- canvas-width width)))
  (define y (rand-between 100.0 (- canvas-height floor-height 55.0)))
  (fish (next-id)
        x
        y
        width
        height
        direction
        (rand-between 0.7 1.7)
        (rand-between 6.0 18.0)
        (rand-between 0.0 (* 2.0 pi))
        (random-ref fish-colors)
        0.0))

;; take/list : list? exact-nonnegative-integer? -> list?
;;   Return the first n elements of xs.
(define (take/list xs n)
  (cond
    [(or (zero? n) (null? xs)) '()]
    [else (cons (car xs)
                (take/list (cdr xs) (sub1 n)))]))

;; sync-fish-population! : -> void?
;;   Match the live school to the requested fish count.
(define (sync-fish-population!)
  (define target (obs-peek @fish-count))
  (define current (length fishes))
  (cond
    [(< current target)
     (set! fishes
           (append fishes
                   (for/list ([i (in-range (- target current))])
                     (make-random-fish))))]
    [(> current target)
     (set! fishes (take/list fishes target))])
  (void))

;; reset-aquarium! : -> void?
;;   Rebuild the school and clear active ripples.
(define (reset-aquarium!)
  (set! fishes
        (for/list ([i (in-range (obs-peek @fish-count))])
          (make-random-fish)))
  (set! ripples '())
  (:= @status "Fresh water. Click the canvas to scatter the school.")
  (void))

;; wrap-fish-x : fish? real? -> real?
;;   Wrap x so fish re-enter from the opposite side.
(define (wrap-fish-x f x)
  (define w (fish-width f))
  (cond
    [(and (eq? (fish-direction f) 'right)
          (> x (+ canvas-width w)))
     (- 0.0 w)]
    [(and (eq? (fish-direction f) 'left)
          (< x (- 0.0 w)))
     canvas-width]
    [else x]))

;; step-fish : fish? -> fish?
;;   Advance one fish by a frame of swimming motion.
(define (step-fish f)
  (define dir-sign (if (eq? (fish-direction f) 'right) 1.0 -1.0))
  (define velocity (* dir-sign (fish-base-speed f) (speed-factor) (+ 1.0 (fish-burst f))))
  (define x1 (+ (fish-x f) velocity))
  (define phase1 (+ (fish-phase f) (* 0.045 (+ 0.8 (fish-base-speed f)))))
  (define burst1 (* (fish-burst f) 0.91))
  (struct-copy fish f
    [x (wrap-fish-x f x1)]
    [phase phase1]
    [burst (if (< burst1 0.03) 0.0 burst1)]))

;; step-ripple : ripple? -> (or/c ripple? #f)
;;   Advance one ripple and drop it when it has faded out.
(define (step-ripple r)
  (define age1 (+ (ripple-age r) 1.0))
  (and (< age1 (ripple-max-age r))
       (struct-copy ripple r [age age1])))

;; react-fish-to-splash : fish? real? real? -> fish?
;;   Turn fish away from a splash and add a short speed burst.
(define (react-fish-to-splash f splash-x splash-y)
  (define dx (- (fish-x f) splash-x))
  (define dy (- (fish-center-y f) splash-y))
  (define distance (sqrt (+ (* dx dx) (* dy dy))))
  (cond
    [(> distance splash-radius) f]
    [else
     (define strength (- 1.0 (/ distance splash-radius)))
     (struct-copy fish f
       [direction (if (>= dx 0.0) 'right 'left)]
       [burst (max (fish-burst f)
                   (+ 0.4 (* 2.6 strength)))] )]))

;; advance-world! : -> void?
;;   Step fish and ripple state for the next animation frame.
(define (advance-world!)
  (sync-fish-population!)
  (set! fishes (map step-fish fishes))
  (set! ripples
        (filter values
                (map step-ripple ripples)))
  (void))

;; get-canvas : -> any/c
;;   Return the mounted aquarium canvas, if any.
(define (get-canvas)
  (define doc (js-var "document"))
  (js-send/extern/nullish doc "getElementById" (vector canvas-name)))

;; get-drawing-context : -> any/c
;;   Return the aquarium 2D drawing context, if mounted.
(define (get-drawing-context)
  (define canvas (get-canvas))
  (and canvas
       (js-canvas-get-context canvas "2d" (js-var "undefined"))))

;; client-position->canvas-position : real? real? -> real? real?
;;   Convert viewport coordinates into canvas-space coordinates.
(define (client-position->canvas-position client-x client-y)
  (define canvas (get-canvas))
  (define rect (js-get-bounding-client-rect canvas))
  (define left (js-dom-rect-left rect))
  (define top (js-dom-rect-top rect))
  (define width (js-dom-rect-width rect))
  (define height (js-dom-rect-height rect))
  (define canvas-width* (js-canvas-width canvas))
  (define canvas-height* (js-canvas-height canvas))
  (values (* (/ (- client-x left) width) canvas-width*)
          (* (/ (- client-y top) height) canvas-height*)))

;; draw-background! : any/c -> void?
;;   Paint the aquarium backdrop, floor, and light bands.
(define (draw-background! dc)
  (dc 'fill-style "#d7f3ff")
  (dc 'fill-rect 0. 0. canvas-width canvas-height)
  (dc 'fill-style "#b8e7fa")
  (dc 'fill-rect 0. 0. canvas-width (* canvas-height 0.45))
  (dc 'fill-style "rgba(255,255,255,0.28)")
  (dc 'fill-rect 0. 28. canvas-width 12.)
  (dc 'fill-style "rgba(255,255,255,0.18)")
  (dc 'fill-rect 0. 64. canvas-width 8.)
  (dc 'fill-style "#d5c193")
  (dc 'fill-rect 0. (- canvas-height floor-height) canvas-width floor-height)
  (dc 'stroke-style "rgba(34,74,112,0.20)")
  (dc 'line-width 1.0)
  (dc 'stroke-rect 0.5 0.5 (- canvas-width 1.0) (- canvas-height 1.0))
  (dc 'fill-style "rgba(33,111,140,0.17)")
  (dc 'begin-path)
  (dc 'arc 86. (- canvas-height 34.) 38. 0. (* 2.0 pi))
  (dc 'fill)
  (dc 'begin-path)
  (dc 'arc 738. (- canvas-height 28.) 48. 0. (* 2.0 pi))
  (dc 'fill)
  (void))

;; draw-seaweed! : any/c real? real? real? string? -> void?
;;   Draw one simple strand of seaweed with quadratic bends.
(define (draw-seaweed! dc x y height color)
  (dc 'save)
  (dc 'stroke-style color)
  (dc 'line-width 8.0)
  (dc 'line-cap "round")
  (dc 'begin-path)
  (dc 'move-to x y)
  (dc 'quadratic-curve-to (- x 18.0) (- y (* height 0.34))
      (+ x 8.0) (- y (* height 0.68)))
  (dc 'quadratic-curve-to (+ x 22.0) (- y (* height 0.88))
      x (- y height))
  (dc 'stroke)
  (dc 'restore)
  (void))

;; draw-ripple! : any/c ripple? -> void?
;;   Draw one expanding splash ripple.
(define (draw-ripple! dc r)
  (define progress (/ (ripple-age r) (ripple-max-age r)))
  (define radius (+ 10.0 (* 120.0 progress)))
  (define alpha (* 0.45 (- 1.0 progress)))
  (dc 'save)
  (dc 'stroke-style (format "rgba(255,255,255,~a)" alpha))
  (dc 'line-width (+ 1.0 (* 3.0 (- 1.0 progress))))
  (dc 'begin-path)
  (dc 'arc (ripple-x r) (ripple-y r) radius 0. (* 2.0 pi))
  (dc 'stroke)
  (dc 'restore)
  (void))

;; make-fish-pict : fish? -> pict?
;;   Convert a fish model into a pict for drawing.
(define (make-fish-pict f)
  (standard-fish (fish-width f)
                 (fish-height f)
                 #:direction (fish-direction f)
                 #:color (fish-color f)
                 #:eye-color "black"
                 #:open-mouth (fish-open-mouth f)))

;; draw-fish! : any/c fish? -> void?
;;   Draw one fish at its current animated position.
(define (draw-fish! dc f)
  (define p (make-fish-pict f))
  (draw-pict p
             dc
             (fish-x f)
             (- (fish-center-y f)
                (/ (fish-height f) 2.0))))

;; draw-scene! : -> void?
;;   Redraw the aquarium for the current world state.
(define (draw-scene!)
  (define ctx (get-drawing-context))
  (cond
    [(not ctx)
     (:= @status "Waiting for the aquarium canvas...")
     (void)]
    [else
     (define dc (canvas-context->dc ctx))
     (draw-background! dc)
     (draw-seaweed! dc 118. (- canvas-height 18.0) 96. "#2f8f5b")
     (draw-seaweed! dc 162. (- canvas-height 20.0) 74. "#4a9d6b")
     (draw-seaweed! dc 690. (- canvas-height 18.0) 90. "#2c7f56")
     (draw-seaweed! dc 732. (- canvas-height 22.0) 68. "#41946a")
     (for-each (lambda (r) (draw-ripple! dc r)) ripples)
     (for-each (lambda (f) (draw-fish! dc f)) fishes)
     (void)]))

;; schedule-next-frame! : -> void?
;;   Queue the next animation step when the loop is active.
(define (schedule-next-frame!)
  (when animation-running?
    (js-window-set-timeout/delay frame-callback frame-delay-ms)))

;; step-frame! : -> void?
;;   Advance and redraw the aquarium, then schedule the next frame.
(define (step-frame!)
  (ensure-click-listener!)
  (unless (obs-peek @paused)
    (advance-world!))
  (draw-scene!)
  (schedule-next-frame!))

;; start-animation! : -> void?
;;   Start the animation loop once.
(define (start-animation!)
  (unless frame-callback
    (set! frame-callback
          (procedure->external
           (lambda _
             (step-frame!)))))
  (unless animation-running?
    (set! animation-running? #t)
    (schedule-next-frame!))
  (void))

;; add-ripple! : real? real? -> void?
;;   Register a new splash ripple at x/y.
(define (add-ripple! x y)
  (set! ripples
        (cons (ripple x y 0.0 26.0)
              (take/list ripples 5))))

;; splash! : real? real? -> void?
;;   Create a splash and make nearby fish dart away.
(define (splash! x y)
  (add-ripple! x y)
  (set! fishes
        (map (lambda (f)
               (react-fish-to-splash f x y))
             fishes))
  (:= @status
      (format "Splash at (~a, ~a). The school scattered."
              (inexact->exact (round x))
              (inexact->exact (round y))))
  (draw-scene!)
  (void))

;; handle-canvas-click! : any/c -> void?
;;   Convert a click event to canvas coordinates and trigger a splash.
(define (handle-canvas-click! event)
  (define-values (x y)
    (client-position->canvas-position
     (mouse-event-client-x event)
     (mouse-event-client-y event)))
  (splash! x y))

;; ensure-click-listener! : -> void?
;;   Attach the click listener to the canvas once it exists.
(define (ensure-click-listener!)
  (unless click-callback
    (set! click-callback
          (procedure->external handle-canvas-click!)))
  (unless click-listener-installed?
    (define canvas (get-canvas))
    (when canvas
      (js-add-event-listener! canvas "click" click-callback)
      (set! click-listener-installed? #t)))
  (void))

;; queue-draw! : -> void?
;;   Schedule a redraw after the DOM and observables settle.
(define (queue-draw!)
  (void
   (js-window-set-timeout/delay
    (procedure->external
     (lambda _
       (draw-scene!)))
    0.0)))

;; set-fish-count! : any/c -> void?
;;   Update the fish count control and resync the school.
(define (set-fish-count! n)
  (:= @fish-count n)
  (sync-fish-population!)
  (queue-draw!))

;; toggle-paused! : -> void?
;;   Pause or resume the animation loop.
(define (toggle-paused!)
  (:= @paused (not (obs-peek @paused)))
  (:= @status
      (if (obs-peek @paused)
          "Aquarium paused."
          "Aquarium resumed."))
  (queue-draw!))

(define control-style
  "width:290px; min-width:290px;")

(define preview-style
  "width:min(900px, 100%);")

(define canvas-style
  "display:block; width:min(860px, 100%); height:auto; border-radius:24px; box-shadow:0 18px 42px rgba(18,58,92,0.16); background:#d7f3ff; cursor:crosshair;")

(define action-row-style
  "display:grid; grid-template-columns: 8rem 1fr; gap:0.75rem; align-items:start;")

(define status-style
  "width:100%; min-height:3.25rem;")

(define pict-fish-school-app
  (ui-window
   (ui-container #:style "width:min(1240px, calc(100vw - 28px));"
    (ui-vpanel
     (ui-h1 "Pict Fish School")
     (ui-text "An animated aquarium built with web-easy controls and web-pict fish.")
     (ui-text "Click the water to make the school react.")
     (ui-hpanel
      (ui-group
       "Aquarium"
       (ui-div
        #:attrs `((style ,control-style))
        (ui-vpanel
         (ui-text "Fish count")
         (ui-slider @fish-count set-fish-count! 3 14)
         (ui-text (@fish-count . ~> .
                               (lambda (n)
                                 (format "~a fish" n))))
         (ui-text "Cruise speed")
         (ui-slider @speed-percent
                    (lambda (n)
                      (:= @speed-percent n))
                    40
                    180)
         (ui-text (@speed-percent . ~> .
                                   (lambda (n)
                                     (format "~a%%" n))))
         (ui-text "Mouth openness")
         (ui-slider @mouth-open
                    (lambda (n)
                      (:= @mouth-open n))
                    0
                    100)
         (ui-text (@mouth-open . ~> .
                                (lambda (n)
                                  (format "~a%%" n))))
         (ui-div
          #:attrs `((style ,action-row-style))
          (ui-button (@paused . ~> .
                              (lambda (paused?)
                                (if paused? "Resume" "Pause")))
                     toggle-paused!
                     #:attrs '((style "width:100%;")))
         (ui-button "Reset aquarium"
                     reset-aquarium!
                     #:attrs '((style "width:100%;"))))
         (ui-div
          #:attrs `((style ,status-style))
          (ui-text @status)))))
      (ui-group
       "Preview"
       (ui-div
        #:attrs `((style ,preview-style))
        (ui-vpanel
         (ui-text "The fish school swims continuously; clicks create ripples and a short burst away from the splash.")
         (ui-canvas #:id canvas-name
                    #:width canvas-width
                    #:height canvas-height
                    #:style canvas-style)
         (ui-text "Tip: increase the fish count, then click near the center to watch the school peel away.")))))))))

(define app-renderer
  (ui-render pict-fish-school-app))

(define light-theme
  (ui-theme 'light
            "we-theme-light"
            "web-easy-core.css"
            "theme-light.css"
            #f))

(define theme-manager
  (ui-install-theme-manager! light-theme))

(ui-mount-renderer! app-renderer)
(reset-aquarium!)
(ensure-click-listener!)
(draw-scene!)
(start-animation!)
