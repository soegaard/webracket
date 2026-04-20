;;;
;;; Web Pict Playground
;;;

(include-lib web-easy)
(define ui-window window)
(define ui-container container)
(define ui-vpanel vpanel)
(define ui-hpanel hpanel)
(define ui-group group)
(define ui-h1 h1)
(define ui-text text)
(define ui-choice choice)
(define ui-slider slider)
(define ui-button button)
(define ui-div Div)
(define ui-canvas Canvas)
(define ui-theme theme)
(define ui-install-theme-manager! install-theme-manager!)
(define ui-render render)
(define ui-mount-renderer! mount-renderer!)

(include-lib web-pict)

(define canvas-name "pict-playground-canvas")
(define canvas-width 720.)
(define canvas-height 480.)

(define @preset (@ "Fish"))
(define @direction (@ "left"))
(define @fish-color (@ "royalblue"))
(define @eye-color (@ "black"))
(define @mouth-open (@ 0))
(define @fish-width (@ 220))
(define @fish-height (@ 120))
(define @rotation-deg (@ 0))
(define @scale-percent (@ 100))
(define @status (@ "Ready."))

;; get-canvas : -> any/c
;;   Look up the preview canvas by id.
(define (get-canvas)
  (define doc (js-var "document"))
  (js-send/extern/nullish doc "getElementById" (vector canvas-name)))

;; get-drawing-context : -> any/c
;;   Produce the browser 2D drawing context for the preview canvas.
(define (get-drawing-context)
  (define canvas (get-canvas))
  (and canvas
       (js-canvas-get-context canvas "2d" (js-var "undefined"))))

;; fill-background! : any/c -> void?
;;   Paint the playground backdrop and a simple frame.
(define (fill-background! dc)
  (dc 'fill-style "#f8fafc")
  (dc 'fill-rect 0. 0. canvas-width canvas-height)
  (dc 'stroke-style "#d6dde8")
  (dc 'line-width 1.0)
  (dc 'stroke-rect 0.5 0.5 (- canvas-width 1.0) (- canvas-height 1.0))
  (dc 'fill-style "#8aa0b7")
  (dc 'font "12px Georgia")
  (dc 'fill-text "web-easy hosts the canvas; web-pict renders the scene." 18. 26.))

;; string->direction : string? -> symbol?
;;   Normalize a direction choice to the symbol that `standard-fish` expects.
(define (string->direction s)
  (if (string=? s "right") 'right 'left))

;; rotation-radians : -> real?
;;   Convert the rotation slider to radians.
(define (rotation-radians)
  (* pi (/ (obs-peek @rotation-deg) 180.0)))

;; scale-factor : -> real?
;;   Convert the percentage slider to a multiplicative scale.
(define (scale-factor)
  (/ (obs-peek @scale-percent) 100.0))

;; fish-pict : -> pict?
;;   Build the interactive fish preset.
(define (fish-pict)
  (standard-fish (obs-peek @fish-width)
                 (obs-peek @fish-height)
                 #:direction  (string->direction (obs-peek @direction))
                 #:color      (obs-peek @fish-color)
                 #:eye-color  (obs-peek @eye-color)
                 #:open-mouth (/ (obs-peek @mouth-open) 100.0)))

;; poster-pict : -> pict?
;;   Build a composed poster scene around the fish.
(define (poster-pict)
  (define fish (standard-fish 180 92
                              #:direction 'right
                              #:color (obs-peek @fish-color)
                              #:eye-color (obs-peek @eye-color)
                              #:open-mouth (/ (obs-peek @mouth-open) 100.0)))
  (define badge
    (cc-superimpose
     (colorize (disk 54 #:draw-border? #f) "gold")
     (colorize (text "web-pict" null 17) "#183153")))
  (define label
    (colorize
     (frame (inset (text "Functional fish poster" null 22) 14 10))
     "#30507a"))
  (vc-append 18
             label
             (hc-append 18 fish badge)))

;; explain-pict : -> pict?
;;   Build a small composition and overlay pict metrics.
(define (explain-pict)
  (define fish (standard-fish 160 84
                              #:direction 'right
                              #:color "tomato"
                              #:eye-color "midnightblue"
                              #:open-mouth #f))
  (define base
    (vc-append 10
               (text "Pict anatomy" null 20)
               (hc-append 16
                          fish
                          (colorize (filled-rounded-rectangle 110 64 14) "lightsteelblue"))))
  (explain base #:scale 2))

;; current-pict : -> pict?
;;   Choose the currently selected preset scene.
(define (current-pict)
  (case (string->symbol (string-downcase (obs-peek @preset)))
    [(fish)    (fish-pict)]
    [(poster)  (poster-pict)]
    [(explain) (explain-pict)]
    [else      (fish-pict)]))

;; rotate-around-center : pict? real? -> pict?
;;   Rotate p around its visual center instead of its top-left origin.
(define (rotate-around-center p theta)
  (define cx (/ (pict-width p) 2.0))
  (define cy (/ (pict-height p) 2.0))
  (translate (rotate (translate p (- cx) (- cy)) theta)
             cx
             cy))

;; transformed-pict : -> pict?
;;   Apply the shared rotation and scaling controls.
(define (transformed-pict)
  (scale (rotate-around-center (current-pict) (rotation-radians))
         (scale-factor)))

;; draw-current-pict! : -> void?
;;   Redraw the preview canvas using the current control values.
(define (draw-current-pict!)
  (define ctx (get-drawing-context))
  (cond
    [(not ctx)
     (:= @status "Waiting for the preview canvas...")
     (void)]
    [else
     (define dc (canvas-context->dc ctx))
     (define p  (transformed-pict))
     (define x  (max 0.0 (/ (- canvas-width (pict-width p)) 2.0)))
     (define y  (max 0.0 (/ (- canvas-height (pict-height p)) 2.0)))
     (fill-background! dc)
     (draw-pict p dc x y)
     (:= @status
         (format "~a preset, ~ax~a pict, scale ~a%, rotation ~a°"
                 (obs-peek @preset)
                 (inexact->exact (round (pict-width p)))
                 (inexact->exact (round (pict-height p)))
                 (obs-peek @scale-percent)
                 (obs-peek @rotation-deg)))
     (void)]))

;; queue-redraw! : -> void?
;;   Schedule a repaint after the DOM has had a chance to update.
(define (queue-redraw!)
  (void
   (js-window-set-timeout/delay
    (procedure->external draw-current-pict!)
    0.0)))

;; update! : observable? any/c -> void?
;;   Set an observable and redraw the preview.
(define (update! @obs value)
  (:= @obs value)
  (queue-redraw!))

(define control-style
  "width: min(280px, 100%);")

(define preview-style
  "width: min(760px, 100%);")

(define canvas-style
  "display:block; width:min(720px, 100%); height:auto; border-radius:18px; box-shadow:0 12px 32px rgba(20,40,80,0.12); background:#f8fafc;")

(define playground-app
  (ui-window
   (ui-container #:style "width:min(1180px, calc(100vw - 28px));"
    (ui-vpanel
     (ui-h1 "web-pict Playground")
     (ui-text "Use web-easy controls to drive a canvas preview rendered with web-pict.")
     (ui-hpanel
      (ui-group "Controls"
                (ui-div #:attrs `((style ,control-style))
                        (ui-vpanel
                           (ui-text "Preset")
                           (ui-choice '("Fish" "Poster" "Explain")
                                      @preset
                                      (lambda (new-value)
                                        (update! @preset new-value)))
                           (ui-text "Direction")
                           (ui-choice '("left" "right")
                                      @direction
                                      (lambda (new-value)
                                        (update! @direction new-value)))
                           (ui-text "Fish color")
                           (ui-choice '("royalblue" "salmon" "olive" "chocolate" "darkcyan")
                                      @fish-color
                                      (lambda (new-value)
                                        (update! @fish-color new-value)))
                           (ui-text "Eye color")
                           (ui-choice '("black" "saddlebrown" "midnightblue" "white")
                                      @eye-color
                                      (lambda (new-value)
                                        (update! @eye-color new-value)))
                           (ui-text "Mouth openness")
                           (ui-slider @mouth-open
                                      (lambda (new-value)
                                        (update! @mouth-open new-value))
                                      0
                                      100)
                           (ui-text (@mouth-open . ~> . (lambda (n) (format "~a%" n))))
                           (ui-text "Fish width")
                           (ui-slider @fish-width
                                      (lambda (new-value)
                                        (update! @fish-width new-value))
                                      80
                                      320)
                           (ui-text (@fish-width . ~> . (lambda (n) (format "~a px" n))))
                           (ui-text "Fish height")
                           (ui-slider @fish-height
                                      (lambda (new-value)
                                        (update! @fish-height new-value))
                                      40
                                      180)
                           (ui-text (@fish-height . ~> . (lambda (n) (format "~a px" n))))
                           (ui-text "Scale")
                           (ui-slider @scale-percent
                                      (lambda (new-value)
                                        (update! @scale-percent new-value))
                                      50
                                      180)
                           (ui-text (@scale-percent . ~> . (lambda (n) (format "~a%" n))))
                           (ui-text "Rotation")
                           (ui-slider @rotation-deg
                                      (lambda (new-value)
                                        (update! @rotation-deg new-value))
                                      -180
                                      180)
                           (ui-text (@rotation-deg . ~> . (lambda (n) (format "~a°" n))))
                           (ui-button "Redraw now" draw-current-pict!))))
      (ui-group "Preview"
                (ui-div #:attrs `((style ,preview-style))
                        (ui-vpanel
                           (ui-text @status)
                           (ui-canvas #:id canvas-name
                                      #:width canvas-width
                                      #:height canvas-height
                                      #:style canvas-style)
                           (ui-text "Try the Explain preset to inspect pict bounds and baselines.")))))))))

(define app-renderer
  (ui-render playground-app))

(define light-theme
  (ui-theme 'light
            "we-theme-light"
            "web-easy-core.css"
            "theme-light.css"
            #f))

(define theme-manager
  (ui-install-theme-manager! light-theme))

(ui-mount-renderer! app-renderer)

(define (main)
  (queue-redraw!))

(main)
