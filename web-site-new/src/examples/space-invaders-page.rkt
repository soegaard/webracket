;;;
;;; Space Invaders
;;;

;; This is a version of the classic arcade game Space Invaders from 1978.
;; At the top of the screen, you will see rows of aliens moving
;; back and forth. The player is at the bottom of the screen.
;; When the aliens touch the edge of the screen, they descend.
;; The goal of the game is to shoot all aliens before the aliens
;; land on the ground (the bottom of the screen).
;; To stop the invasion, the player can shoot.
;; In hard mode, one shot at a time is on screen.
;; In easy mode, multiple shots (bullets) are fine.


;;;
;;; Data Representation
;;;


;; A Bullet is a (bullet Real Real)

(struct bullet (x y))

;; Interpretation:
;;    (bullet x y)
;; is one projectile centered at x with top at y.


;; An Enemy is a (enemy Real Real Natural)

(struct enemy  (x y row))

;; Interpretation:
;;   (enemy x y row)
;; is one invader at (x, y), with row used for color.

;; A BulletList is one of:
;; - '()
;; - (cons Bullet BulletList)

;; An EnemyList is one of:
;; - '()
;; - (cons Enemy EnemyList)

;; A Status is one of: 'playing, 'won, 'lost.
;; A Mode   is one of: 'easy, 'hard.

;; A GameState is a
;;   (game-state player-x player-y bullets enemies enemies-dir status
;;               left-pressed? right-pressed? space-pressed?
;;               mode hard-shot-lock? hard-shot-y
;;               time-since-last-shot win-sound-played? loss-sound-played?)

(struct game-state
  (player-x
   player-y
   bullets
   enemies
   enemies-dir
   status
   left-pressed?
   right-pressed?
   space-pressed?
   mode
   hard-shot-lock?
   hard-shot-y
   time-since-last-shot
   win-sound-played?
   loss-sound-played?))

;; Interpretation:
;;  - player-x, player-y        : player position
;;  - bullets                   : BulletList
;;  - enemies                   : EnemyList
;;  - enemies-dir               : -1 or 1 (horizontal enemy direction)
;;  - status                    : Status
;;  - left/right/space-pressed? : keyboard latch state
;;  - mode                      : Mode
;;  - hard-shot-lock?           : in hard mode, whether shooting is currently locked
;;  - hard-shot-y               : virtual y used to release hard-mode shot lock at top
;;  - time-since-last-shot      : cooldown accumulator in seconds
;;  - win/loss-sound-played?    : one-shot audio guards


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

;;;
;;; Model
;;;

;; inexact : number? -> inexact-real?
;;   Convert exact numeric values to inexact values for canvas/DOM APIs.
(define (inexact x)
  (if (exact? x) (exact->inexact x) x))

;; Canvas geometry.
(define canvas-width  480)
(define canvas-height 640)
(define width         (inexact canvas-width))
(define height        (inexact canvas-height))

;; Player tuning and geometry.
(define player-width      48.)
(define player-height     20.)
(define half-player-width (/ player-width 2.))
(define player-speed      260.)

;; Bullet tuning and geometry.
(define bullet-speed      520.)
(define bullet-width        4.)
(define bullet-half-width (/ bullet-width 2.))
(define bullet-height     12.)
(define shoot-cooldown     0.35)

;; Enemy formation geometry and movement tuning.
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

;; make-initial-enemies : -> (listof enemy?)
;;   Build the initial enemy formation in row-major order.
(define (make-initial-enemies)
  (for*/list ([row (in-range enemy-rows)]
              [col (in-range enemy-cols)])
    (enemy (+ enemy-start-x (* col enemy-spacing-x))
           (+ enemy-start-y (* row enemy-spacing-y))
           row)))

;; make-initial-game-state : -> game-state?
;;   Construct the initial game state.
(define (make-initial-game-state)
  (game-state
   (/ width 2.)                     ; player-x
   (- height (+ player-height 24.)) ; player-y
   '()                              ; bullets
   (make-initial-enemies)           ; enemies
   1                                ; enemies-dir
   'playing                         ; status
   #f                               ; left-pressed?
   #f                               ; right-pressed?
   #f                               ; space-pressed?
   'easy                            ; mode
   #f                               ; hard-shot-lock?
   0.                               ; hard-shot-y
   shoot-cooldown                   ; time-since-last-shot
   #f                               ; win-sound-played?
   #f))                             ; loss-sound-played?

;; clamp-player : real? -> real?
;;   Clamp a horizontal player position to the playable area bounds.
(define (clamp-player x)
  (min (- width half-player-width)
       (max half-player-width x)))

;; enforce-hard-mode-shot-state : game-state? -> game-state?
;;   Keep hard mode constrained to one active bullet and lock-state.
(define (enforce-hard-mode-shot-state state)
  (cond
    [(null? (game-state-bullets state))
     (struct-copy game-state state
                  [hard-shot-lock? #f]
                  [hard-shot-y     0.])]
    [else
     (define b (car (game-state-bullets state)))
     (struct-copy game-state state
                  [bullets         (list b)]
                  [hard-shot-lock? #t]
                  [hard-shot-y     (bullet-y b)])]))

;; fire : game-state? -> game-state?
;;   Spawn a bullet when firing is allowed by state, cooldown, and mode rules.
(define (fire state)
  (define status               (game-state-status state))
  (define time-since-last-shot (game-state-time-since-last-shot state))
  (define mode                 (game-state-mode state))
  (define hard-shot-lock?      (game-state-hard-shot-lock? state))
  (define player-x             (game-state-player-x state))
  (define player-y             (game-state-player-y state))
  (define bullets              (game-state-bullets state))

  (case status
    [(playing)
     (cond
       [(and (>= time-since-last-shot shoot-cooldown)
             (or (eq? mode 'easy)
                 (not hard-shot-lock?)))
        (define new-bullet   (bullet player-x (- player-y bullet-height)))
        (define state+bullet (struct-copy game-state state
                                          [bullets              (cons new-bullet bullets)]
                                          [time-since-last-shot 0.]))
        (play-sfx 'shoot)
        (case mode
          [(hard)
           (struct-copy game-state state+bullet
                        [hard-shot-lock? #t]
                        [hard-shot-y     (bullet-y new-bullet)])]
          [else
           state+bullet])]
       [else
        state])]
    [(won lost)
     state]
    [else
     (error 'fire "unexpected status: ~a" status)]))

;; bullet-hits-enemy? : bullet? enemy? -> boolean?
;;   Compute axis-aligned overlap between one bullet and one enemy.
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

;; find-hit-enemy : bullet? (listof enemy?) -> (or/c #f enemy?)
;;   Return the first enemy hit by bullet b, or #f when none are hit.
(define (find-hit-enemy b enemies)
  (let loop ([rest enemies])
    (cond
      [(null? rest)                      #f]
      [(bullet-hits-enemy? b (car rest)) (car rest)]
      [else                              (loop (cdr rest))])))

;; update-bullets : game-state? real? -> game-state?
;;   Advance bullet positions, resolve collisions, and keep surviving bullets.
(define (update-bullets state dt)
  (define new-bullets '())
  (define enemies     (game-state-enemies state))

  (define-values (hard-shot-lock? hard-shot-y)
    (if (game-state-hard-shot-lock? state)
        (let ([new-hard-shot-y (- (game-state-hard-shot-y state) (* bullet-speed dt))])
          (if (<= (+ new-hard-shot-y bullet-height) 0.)
              (values #f new-hard-shot-y)
              (values #t new-hard-shot-y)))
        (values (game-state-hard-shot-lock? state)
                (game-state-hard-shot-y state))))

  (for ([b (in-list (game-state-bullets state))])
    (define moved-bullet (bullet (bullet-x b)
                                 (- (bullet-y b) (* bullet-speed dt))))
    (cond
      [(<= (+ (bullet-y moved-bullet) bullet-height) 0.) (void)]
      [else
       (define hit (find-hit-enemy moved-bullet enemies))
       (cond
         [hit   (set! enemies (remove hit enemies eq?))
                (play-sfx 'hit)]
         [else  (set! new-bullets (cons moved-bullet new-bullets))])]))

  (struct-copy game-state state
               [bullets         (reverse new-bullets)]
               [enemies         enemies]
               [hard-shot-lock? hard-shot-lock?]
               [hard-shot-y     hard-shot-y]))

;; update-enemies : game-state? real? -> game-state?
;;   Advance enemy formation, reverse/drop at edges, and apply mode speed.
(define (update-enemies state dt)
  (define mode        (game-state-mode state))
  (define enemies     (game-state-enemies state))
  (define enemies-dir (game-state-enemies-dir state))
  (cond
    [(null? enemies)
     state]
    [else
     (define effective-enemy-speed
       (case mode
         [(hard) (* enemy-speed 1.0)]
         [else   enemy-speed]))
     (define base-dx
       (* effective-enemy-speed dt enemies-dir))
     (define drop?
       (for/or ([e (in-list enemies)])
         (define new-x (+ (enemy-x e) base-dx))
         (or (< new-x enemy-margin)
             (> (+ new-x enemy-width) (- width enemy-margin)))))
     (define next-enemies-dir
       (if drop?
           (- enemies-dir)
           enemies-dir))
     (define dx
       (if drop?
           (- base-dx)
           base-dx))
     (when drop?
       (play-sfx 'drop))
     (define moved-enemies
       (for/list ([e (in-list enemies)])
         (enemy (+ (enemy-x e) dx)
                (+ (enemy-y e) (if drop? enemy-drop 0.))
                (enemy-row e))))
     (struct-copy game-state state
                  [enemies     moved-enemies]
                  [enemies-dir next-enemies-dir])]))

;; enemy-reached-bottom? : (listof enemy?) -> boolean?
;;   Report whether any enemy has reached the defeat threshold.
(define (enemy-reached-bottom? enemies)
  (for/or ([e (in-list enemies)])
    (>= (+ (enemy-y e) enemy-height) height)))

;; player-hit-enemy? : game-state? -> boolean?
;;   Report whether any enemy overlaps the player's current sprite bounds.
(define (player-hit-enemy? state)
  (define p-left   (- (game-state-player-x state) half-player-width))
  (define p-right  (+ p-left player-width))
  (define p-top    (game-state-player-y state))
  (define p-bottom (+ p-top player-height))
  (for/or ([e (in-list (game-state-enemies state))])
    (define e-left   (enemy-x e))
    (define e-right  (+ e-left enemy-width))
    (define e-top    (enemy-y e))
    (define e-bottom (+ e-top enemy-height))
    (and (< p-left e-right)
         (> p-right e-left)
         (< p-top e-bottom)
         (> p-bottom e-top))))

;; update : game-state? real? -> game-state?
;;   Perform one simulation step: input, movement, collisions, and win/loss checks.
(define (update state dt)
  (define status (game-state-status state))
  (case status
    [(playing)
     (define state+time
       (struct-copy game-state state
                    [time-since-last-shot (+ (game-state-time-since-last-shot state) dt)]))
     (define movement
       (+ (if (game-state-right-pressed? state+time) 1. 0.)
          (if (game-state-left-pressed?  state+time) -1. 0.)))
     (define state+moved
       (if (zero? movement)
           state+time
           (struct-copy game-state state+time
                        [player-x (clamp-player
                                   (+ (game-state-player-x state+time)
                                      (* movement player-speed dt)))])))
     (define state+fired
       (if (game-state-space-pressed? state+moved)
           (fire state+moved)
           state+moved))
     (define state+bullets
       (update-bullets state+fired dt))
     (define state+win
       (if (null? (game-state-enemies state+bullets))
           (struct-copy game-state state+bullets
                        [status 'won])
           state+bullets))
     (define state+win-sfx
       (if (and (eq? (game-state-status state+win) 'won)
                (not (game-state-win-sound-played? state+win)))
           (begin
             (play-sfx 'win)
             (struct-copy game-state state+win
                          [win-sound-played? #t]))
           state+win))
     (define state+enemies
       (if (eq? (game-state-status state+win-sfx) 'playing)
           (update-enemies state+win-sfx dt)
           state+win-sfx))
     (define state+bottom
       (if (and (eq? (game-state-status state+enemies) 'playing)
                (enemy-reached-bottom? (game-state-enemies state+enemies)))
           (struct-copy game-state state+enemies
                        [status 'lost])
           state+enemies))
     (define state+collision
       (if (and (eq? (game-state-status state+bottom) 'playing)
                (player-hit-enemy? state+bottom))
           (struct-copy game-state state+bottom
                        [status 'lost])
           state+bottom))
     (if (and (eq? (game-state-status state+collision) 'lost)
              (not (game-state-loss-sound-played? state+collision)))
         (begin
           (play-sfx 'loss)
           (struct-copy game-state state+collision
                        [loss-sound-played? #t]))
         state+collision)]
    [(won lost)
     state]
    [else
     (error 'update "unexpected status: ~a" status)]))

;;;
;;; View
;;;

;; space-invaders-page : -> xexpr?
;;   Build the static page shell and mount point for the game canvas.
(define (space-invaders-page)
  `(div (@ (class "page page--space-invaders"))
        ,(navbar)
        (section (@ (class "examples-hero"))
                 (div (@ (class "hero-panel"))
                      (div (@ (class "pill-row"))
                           (span (@ (class "pill")) "Canvas")
                           (span (@ (class "pill")) "Animation")
                           (span (@ (class "pill")) "DOM + JS FFI"))
                      (h1 (@ (class "hero-title")) "Space Invaders")
                      (p (@ (class "hero-lead"))
                         "Classic arcade action powered by WebRacket and the HTML canvas.")))
        (section (@ (class "section section--examples"))
                 (div (@ (class "section-content"))
                      (div (@ (class "space-invaders-frame"))
                           (p (@ (class "space-invaders-hint"))
                              "Use ←/→ or A/D to move, Space to shoot, M to toggle easy/hard mode.")
                           (div (@ (class "arcade-frame"))
                                (div (@ (class "arcade-bezel"))
                                     (div (@ (id "space-invaders-root"))))))))
        (section (@ (class "section section--examples-details"))
                 (div (@ (class "section-content"))
                      (div (@ (class "examples-details"))
                           (p "This example is adapted from "
                              (code "examples/space-invaders/space-invaders.rkt")
                              ".")
                           (div (@ (class "examples-actions"))
                                ,(code-pill (gh-file "examples/space-invaders/space-invaders.rkt")
                                            "Example source code")
                                ,(code-pill (gh-file "web-site/src/examples/space-invaders-page.rkt")
                                            "Web-site source")))))
        ,(footer-section)))

(define instruction-text "←/→ or A/D to move, Space to shoot")

;; sprite-width : (listof string?) -> exact-nonnegative-integer?
;;   Return the width in pixels of a sprite row encoding.
(define (sprite-width sprite)
  (string-length (car sprite)))

;; sprite-height : (listof string?) -> exact-nonnegative-integer?
;;   Return the height in pixels of a sprite row encoding.
(define (sprite-height sprite)
  (length sprite))

;; draw-sprite! : (listof string?) real? real? real? string? -> void?
;;   Draw a bitmap-like sprite at (x, y) using a uniform integer scale and color.
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

;; render! : game-state? -> void?
;;   Draw the full current game frame (background, actors, HUD, and end message).
(define (render! state)
  (when ctx
    ; Background
    (js-set-canvas2d-fill-style! ctx "#000")
    (js-canvas2d-fill-rect ctx 0. 0. (inexact width) (inexact height))

    ; Player (pixel ship)
    (define ship-scale      (ceiling (/ player-width (sprite-width ship-sprite))))
    (define ship-draw-width (* ship-scale (sprite-width ship-sprite)))
    (define ship-left       (- (game-state-player-x state) (/ ship-draw-width 2.)))
    (draw-sprite! ship-sprite ship-left (game-state-player-y state) ship-scale "#0ff")

    ; Bullets
    (js-set-canvas2d-fill-style! ctx "#ff0")
    (for ([b (in-list (game-state-bullets state))])
      (js-canvas2d-fill-rect ctx
                             (inexact (- (bullet-x b) bullet-half-width))
                             (inexact (bullet-y b))
                             (inexact bullet-width)
                             (inexact bullet-height)))

    ; Enemies (pixel aliens)
    (define alien-scale (ceiling (/ enemy-width (sprite-width alien-sprite))))
    (for ([e (in-list (game-state-enemies state))])
      (define alien-color
        (case (modulo (enemy-row e) 3)
          [(0) "#b4232f"]
          [(1) "#4a6cff"]
          [else "#5d4bea"]))
      (draw-sprite! alien-sprite (enemy-x e) (enemy-y e) alien-scale alien-color))

    ; Instruction Text
    (js-set-canvas2d-fill-style!    ctx "#fff")
    (js-set-canvas2d-font!          ctx "16px sans-serif")
    (js-set-canvas2d-text-align!    ctx "center")
    (js-set-canvas2d-text-baseline! ctx "top")
    (js-canvas2d-fill-text          ctx instruction-text (/ width 2.) 16. (void))
    (js-set-canvas2d-fill-style! ctx (if (eq? (game-state-mode state) 'hard) "#ff6961" "#73d673"))
    (js-set-canvas2d-font! ctx "bold 14px sans-serif")
    (js-set-canvas2d-text-align! ctx "left")
    (js-canvas2d-fill-text ctx
                           (string-append "MODE: "
                                          (if (eq? (game-state-mode state) 'hard)
                                              "hard"
                                              "easy"))
                           14.
                           40.
                           (void))

    ; Final Message
    (unless (eq? (game-state-status state) 'playing)
      (js-set-canvas2d-text-align!    ctx "left")
      (js-set-canvas2d-font!          ctx "32px sans-serif")
      (js-set-canvas2d-text-baseline! ctx "middle")
      (define message (if (eq? (game-state-status state) 'won)
                          "You saved the day!"
                          "The invaders landed!"))
      (js-canvas2d-fill-text ctx message 32. (/ height 2.) (void))
      (js-set-canvas2d-font! ctx "18px sans-serif")
      (js-canvas2d-fill-text ctx "Hit \"R\" to play again."
                             32.
                             (+ (/ height 2.) 36.)
                             (void)))))

;;;
;;; Audio
;;;

(define audio-context #f)

;; audio-context-constructor : -> (or/c #f external?)
;;   Resolve an available browser AudioContext constructor.
(define (audio-context-constructor)
  (define ctor (js-var "AudioContext"))
  (cond
    [(js-nullish? ctor)
     (define webkit (js-var "webkitAudioContext"))
     (if (js-nullish? webkit)
         #f
         webkit)]
    [else ctor]))

;; ensure-audio! : -> void?
;;   Lazily create/resume the shared audio context before sound playback.
(define (ensure-audio!)
  (unless audio-context
    (define ctor (audio-context-constructor))
    (when ctor
      (set! audio-context (js-new ctor (vector)))))
  (when audio-context
    (define state (js-ref audio-context "state"))
    (when (and (string? state) (string=? state "suspended"))
      (js-send audio-context "resume" (vector)))))

;; play-tone : real? real? string? real? [real?] -> void?
;;   Play a short oscillator tone (optional delay defaults to 0.0 seconds).
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

;; play-sfx : symbol? -> void?
;;   Dispatch named game sound effects to one or more tones.
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
;;; Control
;;;

(define canvas #f)
(define ctx    #f)

(define key-down-handler #f)
(define key-up-handler   #f)
(define tick-external    #f)
(define last-time        #f)

(define current-state (make-initial-game-state))

;; init-space-invaders-canvas! : -> void?
;;   Create the canvas element and attach it to the page root container.
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

;; left-key? : string? -> boolean?
;;   Check whether a keyboard key value maps to "move left".
(define (left-key? key)
  (or (string=? key "ArrowLeft")
      (string=? key "Left")
      (string=? key "a")
      (string=? key "A")))

;; right-key? : string? -> boolean?
;;   Check whether a keyboard key value maps to "move right".
(define (right-key? key)
  (or (string=? key "ArrowRight")
      (string=? key "Right")
      (string=? key "d")
      (string=? key "D")))

;; space-key? : string? -> boolean?
;;   Check whether a keyboard key value maps to "fire".
(define (space-key? key)
  (or (string=? key " ")
      (string=? key "Space")
      (string=? key "space")
      (string=? key "Spacebar")
      (string=? key "spacebar")))

;; mode-key? : string? -> boolean?
;;   Check whether a keyboard key value maps to "toggle difficulty mode".
(define (mode-key? key)
  (or (string=? key "m")
      (string=? key "M")))

;; restart-key? : string? -> boolean?
;;   Check whether a keyboard key value maps to "restart game".
(define (restart-key? key)
  (or (string=? key "r")
      (string=? key "R")))

;; on-key-down : external? -> void?
;;   Handle key-down transitions and trigger gameplay actions.
(define (on-key-down evt)
  (ensure-audio!)
  (define key      (js-ref evt "key"))
  (define handled? #f)
  (when (string? key)
    (cond
      [(restart-key? key)
       (set! current-state (make-initial-game-state))
       (play-sfx 'restart)
       (set! handled? #t)]
      [(left-key? key)
       (set! current-state
             (struct-copy game-state current-state
                          [left-pressed? #t]))
       (set! handled? #t)]
      [(right-key? key)
       (set! current-state
             (struct-copy game-state current-state
                          [right-pressed? #t]))
       (set! handled? #t)]
      [(space-key? key)
       (set! current-state
             (struct-copy game-state current-state
                          [space-pressed? #t]))
       (set! current-state (fire current-state))
       (set! handled? #t)]
      [(mode-key? key)
       (define toggled-state
         (struct-copy game-state current-state
                      [mode (if (eq? (game-state-mode current-state) 'easy) 'hard 'easy)]))
       (set! current-state
             (if (eq? (game-state-mode toggled-state) 'hard)
                 (enforce-hard-mode-shot-state toggled-state)
                 (struct-copy game-state toggled-state
                              [hard-shot-lock? #f])))
       (set! handled? #t)]))
  (when handled?
    (js-event-prevent-default evt)))

;; on-key-up : external? -> void?
;;   Handle key-up transitions and release held movement/fire keys.
(define (on-key-up evt)
  (define key      (js-ref evt "key"))
  (define handled? #f)
  (when (string? key)
    (cond
      [(left-key? key)
       (set! current-state
             (struct-copy game-state current-state
                          [left-pressed? #f]))
       (set! handled? #t)]
      [(right-key? key)
       (set! current-state
             (struct-copy game-state current-state
                          [right-pressed? #f]))
       (set! handled? #t)]
      [(space-key? key)
       (set! current-state
             (struct-copy game-state current-state
                          [space-pressed? #f]))
       (set! handled? #t)]))
  (when handled?
    (js-event-prevent-default evt)))

;; attach-key-handlers! : -> void?
;;   Register key event handlers on the window object.
(define (attach-key-handlers!)
  (set! key-down-handler (procedure->external on-key-down))
  (set! key-up-handler   (procedure->external on-key-up))
  (js-add-event-listener! (js-window-window) "keydown" key-down-handler)
  (js-add-event-listener! (js-window-window) "keyup"   key-up-handler))

;; tick : real? -> void?
;;   Drive one animation-frame tick and queue the next frame callback.
(define (tick timestamp)
  (define delta (if last-time (- timestamp last-time) 0.))
  (define dt    (min 0.05 (* 0.001 delta)))
  (set! last-time timestamp)
  (set! current-state (update current-state dt))
  (render! current-state)
  (js-window-request-animation-frame tick-external))

;; init-space-invaders-page! : -> void?
;;   Initialize DOM/canvas/input state and start the animation loop.
(define (init-space-invaders-page!)
  (js-set! (js-var "document") "title" "Space Invaders")
  (set! current-state (make-initial-game-state))
  (set! last-time #f)
  (init-space-invaders-canvas!)
  (attach-key-handlers!)
  (render! current-state)
  (set! tick-external (procedure->external tick))
  (js-window-request-animation-frame tick-external))
