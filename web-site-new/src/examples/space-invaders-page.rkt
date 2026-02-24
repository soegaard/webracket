;;;
;;; Space Invaders demo page (WebRacket site page)
;;;

(define (space-invaders-page)
  `(div (@ (class "page page--space-invaders"))
        ,(navbar)
        (section (@ (class "mathjax-hero"))
                 (div (@ (class "hero-panel"))
                      (div (@ (class "pill-row"))
                           (span (@ (class "pill")) "Canvas")
                           (span (@ (class "pill")) "Animation")
                           (span (@ (class "pill")) "DOM + JS FFI"))
                      (h1 (@ (class "hero-title")) "Space Invaders")
                      (p (@ (class "hero-lead"))
                         "Classic arcade action powered by WebRacket and the HTML canvas.")))
        (section (@ (class "section section--mathjax"))
                 (div (@ (class "section-content"))
                      (div (@ (class "space-invaders-frame"))
                           (p (@ (class "space-invaders-hint"))
                              "Use ←/→ or A/D to move, Space to shoot, M to toggle easy/hard mode.")
                           (div (@ (class "arcade-frame"))
                                (div (@ (class "arcade-bezel"))
                                     (div (@ (id "space-invaders-root"))))))))
        (section (@ (class "section section--mathjax-details"))
                 (div (@ (class "section-content"))
                      (div (@ (class "mathjax-details"))
                           (p "This example is adapted from "
                              (code "examples/space-invaders/space-invaders.rkt")
                              ".")
                           (div (@ (class "mathjax-actions"))
                                ,(code-pill (gh-file "examples/space-invaders/space-invaders.rkt")
                                            "Example source code")
                                ,(code-pill (gh-file "web-site/src/examples/space-invaders-page.rkt")
                                            "Web-site source")))))
        ,(footer-section)))


;;;
;;; Utilities
;;;

(define (inexact x)
  (if (exact? x) (exact->inexact x) x))

;;;
;;; Audio (WebAudio, old-school bleeps)
;;;

(define audio-context #f)

(define (audio-context-constructor)
  (define ctor (js-var "AudioContext"))
  (cond
    [(js-nullish? ctor)
     (define webkit (js-var "webkitAudioContext"))
     (if (js-nullish? webkit)
         #f
         webkit)]
    [else ctor]))

(define (ensure-audio!)
  (unless audio-context
    (define ctor (audio-context-constructor))
    (when ctor
      (set! audio-context (js-new ctor (vector)))))
  (when audio-context
    (define state (js-ref audio-context "state"))
    (when (and (string? state) (string=? state "suspended"))
      (js-send audio-context "resume" (vector)))))

(define (play-tone freq dur type volume [delay 0.0])
  (when audio-context
    (define osc      (js-send/extern audio-context "createOscillator" (vector)))
    (define gain     (js-send/extern audio-context "createGain" (vector)))
    (define now      (js-ref audio-context "currentTime"))
    (define start    (+ (inexact now) (inexact delay)))
    (define end      (+ start (inexact dur)))
    (define freq-obj (js-ref osc "frequency"))
    (define gain-obj (js-ref gain "gain"))
    (js-set! osc      "type" type)
    (js-set! freq-obj "value" (inexact freq))
    (js-send gain-obj "setValueAtTime" (vector (inexact volume) start))
    (js-send gain-obj "linearRampToValueAtTime" (vector 0.0 end))
    (js-send osc      "connect" (vector gain))
    (js-send gain     "connect" (vector (js-ref audio-context "destination")))
    (js-send osc      "start" (vector start))
    (js-send osc      "stop" (vector end))))

(define (play-sfx kind)
  (ensure-audio!)
  (when audio-context
    (case kind
      [(shoot)
       (play-tone 880. 0.07 "square" 0.14)]
      [(hit)
       (play-tone 220. 0.12 "sawtooth" 0.18)]
      [(loss)
       (play-tone 220. 0.18 "square" 0.2)
       (play-tone 110. 0.24 "square" 0.18 0.12)]
      [(win)
       (play-tone 440. 0.1 "triangle" 0.16)
       (play-tone 660. 0.1 "triangle" 0.16 0.12)
       (play-tone 880. 0.12 "triangle" 0.16 0.24)]
      [(restart)
       (play-tone 520. 0.08 "square" 0.12)]
      [(drop)
       (play-tone 180. 0.08 "square" 0.12)]
      [else (void)])))

;;;
;;; Game Area
;;;

(define canvas-width  480)
(define canvas-height 640)
(define width         (inexact canvas-width))
(define height        (inexact canvas-height))

;;;
;;; Representation of the player, the bullets and the enemies.
;;;

(struct bullet (x y) #:mutable)
(struct enemy  (x y row) #:mutable)
(struct player (x y) #:mutable)

(define player-width      48.)
(define player-height     20.)
(define half-player-width (/ player-width 2.))
(define player-pos        (player (/ width 2.) (- height (+ player-height 24.))))
(define player-speed      260.)

(define bullet-speed      520.)
(define bullet-width        4.)
(define bullet-half-width (/ bullet-width 2.))
(define bullet-height     12.)
(define shoot-cooldown     0.35)

(define enemy-rows       4)
(define enemy-cols       8)
(define enemy-width     32.)
(define enemy-height    22.)
(define enemy-spacing-x 52.)
(define enemy-spacing-y 36.)
(define enemy-start-x   60.)
(define enemy-start-y   80.)
(define enemy-drop      26.)
(define enemy-speed     40.)
(define enemy-margin    16.)

(define bullets '())
(define enemies
  (for*/list ([row (in-range enemy-rows)]
              [col (in-range enemy-cols)])
    (enemy (+ enemy-start-x (* col enemy-spacing-x))
           (+ enemy-start-y (* row enemy-spacing-y))
           row)))
(define enemies-dir 1)



;;;
;;; GUI Status
;;;

(define status               'playing)        ; 'playing, 'won or 'lost
(define left-pressed?        #f)              ; set on key down, reset on key up
(define right-pressed?       #f)              ; ditto
(define space-pressed?       #f)              ; ditto
(define mode                 'easy)           ; 'easy or 'hard
(define hard-shot-lock?      #f)              ; in hard mode: true until the shot reaches top
(define hard-shot-y          0.)              ; virtual y-position for lock timing
(define time-since-last-shot shoot-cooldown)  ; time passed since last shot
(define last-time            #f)              ; timestamp from last tick
(define win-sound-played?    #f)
(define loss-sound-played?   #f)

;;;
;;; DOM
;;;

(define canvas #f)
(define ctx    #f)

(define (init-space-invaders-canvas!)
  (define root (js-get-element-by-id "space-invaders-root"))
  (unless root
    (error 'space-invaders "missing #space-invaders-root container"))
  (set! canvas (js-create-element "canvas"))
  (js-set-canvas-width!  canvas canvas-width)
  (js-set-canvas-height! canvas canvas-height)
  (js-set-attribute! canvas "style"
                     (string-append
                      "background: black; image-rendering: pixelated; "
                      "display: block;"))
  (js-append-child! root canvas)
  (set! ctx (js-canvas-get-context canvas "2d" (js-undefined))))

;;;
;;; Key Event Handlers
;;;

(define (left-key? key)
  (or (string=? key "ArrowLeft")
      (string=? key "Left")
      (string=? key "a")
      (string=? key "A")))

(define (right-key? key)
  (or (string=? key "ArrowRight")
      (string=? key "Right")
      (string=? key "d")
      (string=? key "D")))

(define (space-key? key)
  (or (string=? key " ")
      (string=? key "Space")
      (string=? key "space")
      (string=? key "Spacebar")
      (string=? key "spacebar")))

(define (mode-key? key)
  (or (string=? key "m")
      (string=? key "M")))

(define (enforce-hard-mode-shot-state!)
  (cond
    [(null? bullets)
     (set! hard-shot-lock? #f)
     (set! hard-shot-y 0.)]
    [else
     (define b (car bullets))
     (set! bullets (list b))
     (set! hard-shot-lock? #t)
     (set! hard-shot-y (bullet-y b))]))

(define (on-key-down evt)
  (ensure-audio!)
  (define key      (js-ref evt "key"))
  (define handled? #f)
  (when (string? key)
    (cond
      [(restart-key? key)  (reset-game!)
                           (set! handled? #t)]
      [(left-key? key)     (set! left-pressed? #t)
                           (set! handled? #t)]
      [(right-key? key)    (set! right-pressed? #t)
                           (set! handled? #t)]
      [(space-key? key)    (set! space-pressed? #t)
                           (set! handled? #t)
                           (fire!)]
      [(mode-key? key)     (set! mode (if (eq? mode 'easy) 'hard 'easy))
                           (if (eq? mode 'hard)
                               (enforce-hard-mode-shot-state!)
                               (set! hard-shot-lock? #f))
                           (set! handled? #t)]))
  (when handled?
    (js-event-prevent-default evt)))

(define (on-key-up evt)
  (define key      (js-ref evt "key"))
  (define handled? #f)
  (when (string? key)
    (cond
      [(left-key? key)   (set! left-pressed? #f)
                         (set! handled? #t)]
      [(right-key? key)  (set! right-pressed? #f)
                         (set! handled? #t)]
      [(space-key? key)  (set! space-pressed? #f)
                         (set! handled? #t)]))
  (when handled?
    (js-event-prevent-default evt)))

(define key-down-handler #f)
(define key-up-handler   #f)

(define (attach-key-handlers!)
  (set! key-down-handler (procedure->external on-key-down))
  (set! key-up-handler   (procedure->external on-key-up))
  (js-add-event-listener! (js-window-window) "keydown" key-down-handler)
  (js-add-event-listener! (js-window-window) "keyup"   key-up-handler))

;;;
;;; Game State
;;;

(define (clamp-player x)
  (min (- width half-player-width)
       (max half-player-width x)))

(define (fire!)
  (when (and (eq? status 'playing)
             (>= time-since-last-shot shoot-cooldown)
             (or (eq? mode 'easy)
                 (not hard-shot-lock?)))
    (define new-bullet (bullet (player-x player-pos)
                               (- (player-y player-pos) bullet-height)))
    (set! bullets (cons new-bullet bullets))
    (when (eq? mode 'hard)
      (set! hard-shot-lock? #t)
      (set! hard-shot-y (bullet-y new-bullet)))
    (set! time-since-last-shot 0.)
    (play-sfx 'shoot)))

(define (bullet-hits-enemy? b e)
  (define b-left   (- (bullet-x b) bullet-half-width))
  (define b-right  (+ (bullet-x b) bullet-half-width))
  (define b-top    (bullet-y b))
  (define b-bottom (+ b-top bullet-height))
  (define e-left   (enemy-x e))
  (define e-right  (+ e-left enemy-width))
  (define e-top    (enemy-y e))
  (define e-bottom (+ e-top enemy-height))
  (and (< b-left e-right)
       (> b-right e-left)
       (< b-top e-bottom)
       (> b-bottom e-top)))

(define (find-hit-enemy b)
  (let loop ([rest enemies])
    (cond
      [(null? rest)                      #f]
      [(bullet-hits-enemy? b (car rest)) (car rest)]
      [else                              (loop (cdr rest))])))

(define (update-bullets dt)
  (define new-bullets '())
  (when hard-shot-lock?
    (set! hard-shot-y (- hard-shot-y (* bullet-speed dt)))
    (when (<= (+ hard-shot-y bullet-height) 0.)
      (set! hard-shot-lock? #f)))
  (for ([b (in-list bullets)])
    (set-bullet-y! b (- (bullet-y b) (* bullet-speed dt)))
    (cond
      [(<= (+ (bullet-y b) bullet-height) 0.) (void)]
      [else
       (define hit (find-hit-enemy b))
       (cond
         [hit   (set! enemies (remove hit enemies eq?))
                (play-sfx 'hit)]
         [else  (set! new-bullets (cons b new-bullets))])]))
  (set! bullets (reverse new-bullets)))

(define (update-enemies dt)
  (when (pair? enemies)
    (define effective-enemy-speed (if (eq? mode 'hard)
                                      (* enemy-speed 1.0)
                                      enemy-speed))
    (define dx (* effective-enemy-speed dt enemies-dir))
    (define drop?
      (for/or ([e (in-list enemies)])
        (define new-x (+ (enemy-x e) dx))
        (or (< new-x enemy-margin)
            (> (+ new-x enemy-width) (- width enemy-margin)))))
    (when drop?
      (set! enemies-dir (- enemies-dir))
      (set! dx (- dx))
      (play-sfx 'drop)
      (for ([e (in-list enemies)])
        (set-enemy-y! e (+ (enemy-y e) enemy-drop))))
    (for ([e (in-list enemies)])
      (set-enemy-x! e (+ (enemy-x e) dx)))))

(define (enemy-reached-bottom?)
  (for/or ([e (in-list enemies)])
    (>= (+ (enemy-y e) enemy-height) height)))

(define (player-hit-enemy?)
  (define p-left (- (player-x player-pos) half-player-width))
  (define p-right (+ p-left player-width))
  (define p-top (player-y player-pos))
  (define p-bottom (+ p-top player-height))
  (for/or ([e (in-list enemies)])
    (define e-left (enemy-x e))
    (define e-right (+ e-left enemy-width))
    (define e-top (enemy-y e))
    (define e-bottom (+ e-top enemy-height))
    (and (< p-left e-right)
         (> p-right e-left)
         (< p-top e-bottom)
         (> p-bottom e-top))))

(define (update dt)
  (when (eq? status 'playing)
    (set! time-since-last-shot (+ time-since-last-shot dt))
    (define movement 0.)
    (when left-pressed?  (set! movement (- movement 1.)))
    (when right-pressed? (set! movement (+ movement 1.)))
    (unless (zero? movement)
      (set-player-x! player-pos
                     (clamp-player
                      (+ (player-x player-pos)
                         (* movement player-speed dt)))))
    (when space-pressed?
      (fire!))
    (update-bullets dt)
    (when (null? enemies)
      (set! status 'won))
    (when (and (eq? status 'won) (not win-sound-played?))
      (set! win-sound-played? #t)
      (play-sfx 'win))
    (when (eq? status 'playing)
      (update-enemies dt)
      (when (enemy-reached-bottom?)
        (set! status 'lost)))
    (when (and (eq? status 'playing) (player-hit-enemy?))
      (set! status 'lost))
    (when (and (eq? status 'lost) (not loss-sound-played?))
      (set! loss-sound-played? #t)
      (play-sfx 'loss))))

;;;
;;; Render game state to canvas
;;; 

(define instruction-text "←/→ or A/D to move, Space to shoot")

;;;
;;; Pixel sprites (classic Space Invaders style)
;;;

(define ship-sprite
  (list
   "0010000100"
   "0011111100"
   "0111111110"
   "1111111111"
   "1111111111"
   "1011111101"
   "0010000100"))

(define alien-sprite
  (list
   "00100100"
   "01011010"
   "11111111"
   "10111101"
   "11111111"
   "01000010"
   "10100101"
   "01000010"))

(define (sprite-width sprite)
  (string-length (car sprite)))

(define (sprite-height sprite)
  (length sprite))

(define (draw-sprite! sprite x y scale color)
  (js-set-canvas2d-fill-style! ctx color)

  (define x0  (inexact->exact (round x)))
  (define y0  (inexact->exact (round y)))
  (define s   (max 1 (inexact->exact (round scale))))
  (define x0f (inexact x0))
  (define y0f (inexact y0))
  (define sf  (inexact s))
  
  (for ([row     (in-list sprite)]
        [row-idx (in-naturals)])
    (for ([col-idx (in-range (string-length row))])
      (when (char=? (string-ref row col-idx) #\1)
        (js-canvas2d-fill-rect ctx
                               (+ x0f (* col-idx sf))
                               (+ y0f (* row-idx sf))
                               sf
                               sf)))))

(define (render!)
  (when ctx
    ; Background
    (js-set-canvas2d-fill-style! ctx "#000")
    (js-canvas2d-fill-rect ctx 0. 0. (inexact width) (inexact height))

    ; Player (pixel ship)
    (define ship-scale      (ceiling (/ player-width (sprite-width ship-sprite))))
    (define ship-draw-width (* ship-scale (sprite-width ship-sprite)))
    (define ship-left       (- (player-x player-pos) (/ ship-draw-width 2.)))
    (draw-sprite! ship-sprite ship-left (player-y player-pos) ship-scale "#0ff")

    ; Bullets
    (js-set-canvas2d-fill-style! ctx "#ff0")
    (for ([b (in-list bullets)])
      (js-canvas2d-fill-rect ctx
                             (inexact (- (bullet-x b) bullet-half-width))
                             (inexact (bullet-y b))
                             (inexact bullet-width)
                             (inexact bullet-height)))

    ; Enemies (pixel aliens)
    (define alien-scale (ceiling (/ enemy-width (sprite-width alien-sprite))))
    (for ([e (in-list enemies)])
      (define alien-color
        (case (modulo (enemy-row e) 3)
          [(0) "#b4232f"] ; logo red
          [(1) "#4a6cff"] ; logo blue
          [else "#5d4bea"])) ; logo purple
      (draw-sprite! alien-sprite (enemy-x e) (enemy-y e) alien-scale alien-color))

    ; Instruction Text
    (js-set-canvas2d-fill-style!    ctx "#fff")
    (js-set-canvas2d-font!          ctx "16px sans-serif")
    (js-set-canvas2d-text-align!    ctx "center")
    (js-set-canvas2d-text-baseline! ctx "top")
    (js-canvas2d-fill-text          ctx instruction-text (/ width 2.) 16. (void))
    (js-set-canvas2d-fill-style! ctx (if (eq? mode 'hard) "#ff6961" "#73d673"))
    (js-set-canvas2d-font! ctx "bold 14px sans-serif")
    (js-set-canvas2d-text-align! ctx "left")
    (js-canvas2d-fill-text ctx
                           (string-append "MODE: " (if (eq? mode 'hard) "hard" "easy"))
                           14.
                           40.
                           (void))

    ; Final Message
    (unless (eq? status 'playing)
      (js-set-canvas2d-text-align!    ctx "left")
      (js-set-canvas2d-font!          ctx "32px sans-serif")
      (js-set-canvas2d-text-baseline! ctx "middle")
      (define message (if (eq? status 'won)
                          "You saved the day!"
                          "The invaders landed!"))
      (js-canvas2d-fill-text ctx message 32. (/ height 2.) (void))
      (js-set-canvas2d-font! ctx "18px sans-serif")
      (js-canvas2d-fill-text ctx "Hit \"R\" to play again."
                             32.
                             (+ (/ height 2.) 36.)
                             (void)))))

;;;
;;; Timer Events
;;;

(define tick-external #f)

(define (tick timestamp)
  (define delta (if last-time (- timestamp last-time) 0.))
  (define dt    (min 0.05 (* 0.001 delta)))
  (set! last-time timestamp)
  (update dt)
  (render!)
  (js-window-request-animation-frame tick-external))

;;;
;;; Reset / Restart
;;;

(define (reset-game!)
  (set! bullets '())
  (set! enemies (for*/list ([row (in-range enemy-rows)]
                            [col (in-range enemy-cols)])
                  (enemy (+ enemy-start-x (* col enemy-spacing-x))
                         (+ enemy-start-y (* row enemy-spacing-y))
                         row)))
  (set! enemies-dir          1)
  (set! status               'playing)
  (set! left-pressed?        #f)
  (set! right-pressed?       #f)
  (set! space-pressed?       #f)
  (set! hard-shot-lock?      #f)
  (set! hard-shot-y          0.)
  (set! time-since-last-shot shoot-cooldown)
  (set! last-time            #f)
  (set! win-sound-played?    #f)
  (set! loss-sound-played?   #f) 
  (set-player-x! player-pos (/ width 2.))
  (set-player-y! player-pos (- height (+ player-height 24.)))
  (play-sfx 'restart))

;;;
;;; Restart Key
;;;

(define (restart-key? key)
  (or (string=? key "r")
      (string=? key "R")))

(define (init-space-invaders-page!)
  (js-set! (js-var "document") "title" "Space Invaders")
  (init-space-invaders-canvas!)
  (attach-key-handlers!)
  (render!)
  (set! tick-external (procedure->external tick))
  (js-window-request-animation-frame tick-external))
