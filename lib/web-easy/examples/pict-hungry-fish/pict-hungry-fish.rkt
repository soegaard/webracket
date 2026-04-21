;;;
;;; Pict Hungry Fish
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
(include-lib audio)

(define canvas-name "pict-hungry-fish-canvas")
(define canvas-width 860.)
(define canvas-height 500.)
(define frame-delay-ms 33.0)
(define floor-height 84.0)

(define @traffic (@ 9))
(define @mouth-open (@ 30))
(define @paused (@ #f))
(define @sound-enabled (@ #f))
(define @score (@ 0))
(define @best-score (@ 0))
(define @status (@ "Move the cursor through the water. Eat smaller fish and avoid larger ones."))

(struct actor (id x y width height direction speed drift phase color)
  #:transparent)
(struct bubble (x y radius dx dy age max-age)
  #:transparent)
(struct score-pop (x y text age max-age)
  #:transparent)

(define fish-colors
  '("royalblue" "steelblue" "darkcyan" "teal" "salmon" "goldenrod" "mediumpurple"))

(define next-actor-id 0)
(define npcs '())
(define player #f)
(define bubbles '())
(define score-pops '())
(define target-x (/ canvas-width 2.0))
(define target-y (/ canvas-height 2.0))
(define frame-callback #f)
(define move-callback #f)
(define click-callback #f)
(define animation-running? #f)
(define pointer-listeners-installed? #f)
(define game-over? #f)
(define game-phase 'intro)
(define sound-context #f)

;; next-id : -> exact-integer?
;;   Produce a fresh actor identifier.
(define (next-id)
  (begin0 next-actor-id
    (set! next-actor-id (add1 next-actor-id))))

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

;; ensure-sound-context! : -> (or/c audio-context? #f)
;;   Lazily create and resume the shared audio context.
(define (ensure-sound-context!)
  (unless sound-context
    (set! sound-context (audio-context-new)))
  (when sound-context
    (audio-context-resume sound-context))
  sound-context)

;; play-tone! : string? real? real? real? real? -> void?
;;   Play one short synthesized cue when sound is enabled.
(define (play-tone! waveform f0 f1 peak-gain duration)
  (when (obs-peek @sound-enabled)
    (define ctx (ensure-sound-context!))
    (when ctx
      (define now (audio-context-current-time ctx))
      (define osc (audio-context-create-oscillator ctx))
      (define gain-node (audio-context-create-gain ctx))
      (define freq-param (audio-oscillator-node-frequency osc))
      (define gain-param (audio-gain-node-gain gain-node))
      (audio-oscillator-node-set-type! osc waveform)
      (audio-node-connect osc gain-node)
      (audio-node-connect gain-node (audio-context-destination ctx))
      (audio-param-set-value-at-time! freq-param f0 now)
      (audio-param-linear-ramp-to-value-at-time! freq-param f1 (+ now duration))
      (audio-param-set-value-at-time! gain-param 0.0 now)
      (audio-param-linear-ramp-to-value-at-time! gain-param peak-gain (+ now 0.01))
      (audio-param-linear-ramp-to-value-at-time! gain-param 0.0 (+ now duration))
      (audio-oscillator-node-start! osc now)
      (audio-oscillator-node-stop! osc (+ now duration 0.03)))))

;; play-eat-sound! : -> void?
;;   Play a quick chomp cue for a successful bite.
(define (play-eat-sound!)
  (play-tone! "triangle" 680.0 420.0 0.06 0.09))

;; play-tutorial-clear-sound! : -> void?
;;   Play a brighter cue when the real game starts.
(define (play-tutorial-clear-sound!)
  (play-tone! "triangle" 520.0 840.0 0.07 0.14))

;; play-game-over-sound! : -> void?
;;   Play a low bump cue on death.
(define (play-game-over-sound!)
  (play-tone! "sawtooth" 220.0 110.0 0.08 0.18))

;; toggle-sound! : -> void?
;;   Enable or mute game sounds. Enabling also unlocks browser audio.
(define (toggle-sound!)
  (if (obs-peek @sound-enabled)
      (:= @sound-enabled #f)
      (begin
        (ensure-sound-context!)
        (:= @sound-enabled #t)
        (play-tone! "sine" 660.0 660.0 0.035 0.06)))
  (void))

;; playable-height-min : -> real?
;;   Return the minimum y for active fish movement.
(define (playable-height-min)
  78.0)

;; playable-height-max : -> real?
;;   Return the maximum y for active fish movement.
(define (playable-height-max)
  (- canvas-height floor-height 24.0))

;; fish-center-x : actor? -> real?
;;   Compute the fish center x coordinate.
(define (fish-center-x a)
  (+ (actor-x a) (/ (actor-width a) 2.0)))

;; fish-center-y : actor? -> real?
;;   Compute the fish center y coordinate with drifting applied.
(define (fish-center-y a)
  (+ (actor-y a)
     (* (actor-drift a)
        (sin (actor-phase a)))))

;; fish-radius : actor? -> real?
;;   Compute a simple circular collision radius for a.
(define (fish-radius a)
  (* (actor-width a) 0.28))

;; player-mouth-factor : -> real?
;;   Convert the mouth slider to the player's open-mouth value.
(define (player-mouth-factor)
  (/ (obs-peek @mouth-open) 100.0))

;; npc-mouth-factor : actor? -> real?
;;   Give NPC fish a gentle mouth animation.
(define (npc-mouth-factor a)
  (define pulse (* 0.5 (+ 1.0 (sin (+ (actor-phase a) 0.8)))))
  (clamp (+ 0.1 (* 0.35 pulse)) 0.0 1.0))

;; make-player : -> actor?
;;   Construct the player fish in the center of the aquarium.
(define (make-player)
  (actor (next-id)
         160.0
         220.0
         108.0
         58.0
         'right
         0.0
         5.0
         0.0
         "goldenrod"))

;; make-random-npc : -> actor?
;;   Create one autonomous fish with a random size and lane.
(define (make-random-npc)
  (define width (rand-between 58.0 176.0))
  (define height (* width (rand-between 0.42 0.58)))
  (define direction (if (zero? (random 2)) 'left 'right))
  (define speed (rand-between 1.3 3.8))
  (define x (if (eq? direction 'right)
                (- 0.0 width (rand-between 0.0 280.0))
                (+ canvas-width (rand-between 0.0 280.0))))
  (define y (rand-between (playable-height-min) (playable-height-max)))
  (actor (next-id)
         x
         y
         width
         height
         direction
         speed
         (rand-between 4.0 16.0)
         (rand-between 0.0 (* 2.0 pi))
         (random-ref fish-colors)))

;; make-intro-fish : -> actor?
;;   Create the single tutorial fish that teaches the first bite.
(define (make-intro-fish)
  (actor (next-id)
         (- 0.0 72.0)
         212.0
         72.0
         38.0
         'right
         2.2
         3.0
         0.0
         "salmon"))

;; take/list : list? exact-nonnegative-integer? -> list?
;;   Return the first n elements of xs.
(define (take/list xs n)
  (cond
    [(or (zero? n) (null? xs)) '()]
    [else (cons (car xs)
                (take/list (cdr xs) (sub1 n)))]))

;; sync-npc-population! : -> void?
;;   Match the NPC school size to the traffic slider.
(define (sync-npc-population!)
  (when (eq? game-phase 'playing)
    (define target (obs-peek @traffic))
    (define current (length npcs))
    (cond
      [(< current target)
       (set! npcs
             (append npcs
                     (for/list ([i (in-range (- target current))])
                       (make-random-npc))))]
      [(> current target)
       (set! npcs (take/list npcs target))]))
  (void))

;; begin-main-game! : -> void?
;;   Transition from the tutorial bite into the full hungry-fish run.
(define (begin-main-game!)
  (set! game-phase 'playing)
  (set! npcs
        (for/list ([i (in-range (obs-peek @traffic))])
          (make-random-npc)))
  (:= @score 0)
  (:= @status "Nice. Now eat smaller fish, avoid larger ones, and keep growing.")
  (play-tutorial-clear-sound!)
  (void))

;; reset-game! : -> void?
;;   Restart the run with a fresh player and a new NPC school.
(define (reset-game!)
  (set! player (make-player))
  (set! npcs (list (make-intro-fish)))
  (set! bubbles '())
  (set! score-pops '())
  (set! target-x (fish-center-x player))
  (set! target-y (fish-center-y player))
  (set! game-over? #f)
  (set! game-phase 'intro)
  (:= @score 0)
  (:= @status "Tutorial: catch the little fish first.")
  (void))

;; get-canvas : -> any/c
;;   Return the mounted game canvas, if any.
(define (get-canvas)
  (define doc (js-var "document"))
  (js-send/extern/nullish doc "getElementById" (vector canvas-name)))

;; get-drawing-context : -> any/c
;;   Return the 2D drawing context for the game canvas.
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

;; wrap-npc-x : actor? real? -> real?
;;   Wrap NPC x positions so fish re-enter from the opposite side.
(define (wrap-npc-x a x)
  (define w (actor-width a))
  (cond
    [(and (eq? (actor-direction a) 'right)
          (> x (+ canvas-width w)))
     (- 0.0 w (rand-between 0.0 180.0))]
    [(and (eq? (actor-direction a) 'left)
          (< x (- 0.0 w 180.0)))
     (+ canvas-width (rand-between 0.0 180.0))]
    [else x]))

;; step-npc : actor? -> actor?
;;   Advance one NPC fish by a frame.
(define (step-npc a)
  (define dir-sign (if (eq? (actor-direction a) 'right) 1.0 -1.0))
  (define x1 (+ (actor-x a) (* dir-sign (actor-speed a))))
  (define phase1 (+ (actor-phase a) (* 0.035 (+ 1.0 (actor-speed a)))))
  (define y-mid (clamp (actor-y a) (playable-height-min) (playable-height-max)))
  (struct-copy actor a
    [x (wrap-npc-x a x1)]
    [y y-mid]
    [phase phase1]))

;; steer-player : actor? -> actor?
;;   Move the player fish toward the current cursor target.
(define (steer-player a)
  (define cx (fish-center-x a))
  (define cy (fish-center-y a))
  (define dx (- target-x cx))
  (define dy (- target-y cy))
  (define distance (sqrt (+ (* dx dx) (* dy dy))))
  (define step (clamp (+ 2.8 (* 0.015 (actor-width a))) 2.8 5.3))
  (define ratio (if (< distance 0.001)
                    0.0
                    (min 1.0 (/ step distance))))
  (define cx1 (+ cx (* dx ratio)))
  (define cy1 (+ cy (* dy ratio)))
  (define direction (if (>= dx 0.0) 'right 'left))
  (struct-copy actor a
    [x (- cx1 (/ (actor-width a) 2.0))]
    [y (clamp cy1
              (playable-height-min)
              (playable-height-max))]
    [direction direction]
    [phase (+ (actor-phase a) 0.09)]))

;; collision? : actor? actor? -> boolean?
;;   Check whether two fish overlap.
(define (collision? a b)
  (define dx (- (fish-center-x a) (fish-center-x b)))
  (define dy (- (fish-center-y a) (fish-center-y b)))
  (<= (sqrt (+ (* dx dx) (* dy dy)))
      (+ (fish-radius a) (fish-radius b))))

;; actor-edible? : actor? actor? -> boolean?
;;   Return whether prey is safely edible for player0.
(define (actor-edible? player0 prey)
  (> (actor-width player0)
     (* (actor-width prey) 1.12)))

;; grow-player : actor? actor? -> actor?
;;   Increase the player size after eating prey.
(define (grow-player player0 prey)
  (define width1 (clamp (+ (actor-width player0)
                           (* 0.16 (actor-width prey)))
                        92.0
                        230.0))
  (define height1 (* width1 0.54))
  (struct-copy actor player0
    [width width1]
    [height height1]))

;; respawn-npc : actor? -> actor?
;;   Replace an eaten or escaped NPC with a fresh fish.
(define (respawn-npc _a)
  (make-random-npc))

;; spawn-eat-effects! : actor? -> void?
;;   Create a small bubble burst and floating score popup for prey.
(define (spawn-eat-effects! prey)
  (define cx (fish-center-x prey))
  (define cy (fish-center-y prey))
  (set! bubbles
        (append
         (for/list ([i (in-range 5)])
           (bubble (+ cx (rand-between -10.0 10.0))
                   (+ cy (rand-between -8.0 8.0))
                   (rand-between 3.0 7.0)
                   (rand-between -0.8 0.8)
                   (rand-between -2.2 -0.8)
                   0.0
                   (rand-between 16.0 24.0)))
         bubbles))
  (set! score-pops
        (cons (score-pop cx (- cy 8.0) "+1" 0.0 22.0)
              score-pops)))

;; step-bubble : bubble? -> (or/c bubble? #f)
;;   Advance one bubble and drop it when it fades out.
(define (step-bubble b)
  (define age1 (+ (bubble-age b) 1.0))
  (and (< age1 (bubble-max-age b))
       (struct-copy bubble b
         [x (+ (bubble-x b) (bubble-dx b))]
         [y (+ (bubble-y b) (bubble-dy b))]
         [age age1])))

;; step-score-pop : score-pop? -> (or/c score-pop? #f)
;;   Advance one score popup and drop it when it expires.
(define (step-score-pop p)
  (define age1 (+ (score-pop-age p) 1.0))
  (and (< age1 (score-pop-max-age p))
       (struct-copy score-pop p
         [y (- (score-pop-y p) 1.2)]
         [age age1])))

;; handle-collisions! : -> void?
;;   Apply eat-or-die collisions between the player and NPC fish.
(define (handle-collisions!)
  (when player
    (define alive-player player)
    (define new-npcs '())
    (define ate-count 0)
    (define intro-cleared? #f)
    (define dead? #f)
    (for ([npc (in-list npcs)])
      (cond
        [(or dead? game-over?)
         (set! new-npcs (cons npc new-npcs))]
        [(collision? alive-player npc)
         (if (actor-edible? alive-player npc)
             (begin
               (spawn-eat-effects! npc)
               (play-eat-sound!)
               (set! alive-player (grow-player alive-player npc))
               (set! ate-count (add1 ate-count))
               (if (eq? game-phase 'intro)
                   (set! intro-cleared? #t)
                   (set! new-npcs (cons (respawn-npc npc) new-npcs))))
             (begin
               (set! dead? #t)
               (set! new-npcs (cons npc new-npcs))))]
        [else
         (set! new-npcs (cons npc new-npcs))]))
    (set! player alive-player)
    (set! npcs (reverse new-npcs))
    (when (> ate-count 0)
      (cond
        [intro-cleared?
         (begin-main-game!)]
        [else
         (:= @score (+ (obs-peek @score) ate-count))
         (:= @best-score (max (obs-peek @best-score)
                              (obs-peek @score)))
         (:= @status
             (format "Chomp. Score ~a. Keep eating smaller fish."
                     (obs-peek @score)))]))
    (when dead?
      (set! game-over? #t)
      (play-game-over-sound!)
      (:= @best-score (max (obs-peek @best-score)
                           (obs-peek @score)))
      (:= @status
          (format "Game over at score ~a. Click the canvas or press Reset to try again."
                  (obs-peek @score))))))

;; advance-world! : -> void?
;;   Advance the player and NPC fish for one frame.
(define (advance-world!)
  (sync-npc-population!)
  (when player
    (set! player (steer-player player)))
  (set! npcs (map step-npc npcs))
  (set! bubbles
        (filter values
                (map step-bubble bubbles)))
  (set! score-pops
        (filter values
                (map step-score-pop score-pops)))
  (handle-collisions!)
  (void))

;; draw-background! : any/c -> void?
;;   Paint the aquarium backdrop and sea floor.
(define (draw-background! dc)
  (dc 'fill-style "#d8f3ff")
  (dc 'fill-rect 0. 0. canvas-width canvas-height)
  (dc 'fill-style "#b4e3f7")
  (dc 'fill-rect 0. 0. canvas-width (* canvas-height 0.50))
  (dc 'fill-style "rgba(255,255,255,0.24)")
  (dc 'fill-rect 0. 28. canvas-width 14.)
  (dc 'fill-style "rgba(255,255,255,0.16)")
  (dc 'fill-rect 0. 64. canvas-width 8.)
  (dc 'fill-style "#d7c391")
  (dc 'fill-rect 0. (- canvas-height floor-height) canvas-width floor-height)
  (dc 'fill-style "rgba(26,94,122,0.10)")
  (dc 'begin-path)
  (dc 'arc 96. (- canvas-height 26.) 42. 0. (* 2.0 pi))
  (dc 'fill)
  (dc 'begin-path)
  (dc 'arc 752. (- canvas-height 22.) 46. 0. (* 2.0 pi))
  (dc 'fill)
  (dc 'stroke-style "rgba(34,74,112,0.20)")
  (dc 'line-width 1.0)
  (dc 'stroke-rect 0.5 0.5 (- canvas-width 1.0) (- canvas-height 1.0))
  (void))

;; draw-seaweed! : any/c real? real? real? string? -> void?
;;   Draw one simple strand of seaweed.
(define (draw-seaweed! dc x y height color)
  (dc 'save)
  (dc 'stroke-style color)
  (dc 'line-width 8.0)
  (dc 'line-cap "round")
  (dc 'begin-path)
  (dc 'move-to x y)
  (dc 'quadratic-curve-to (- x 18.0) (- y (* height 0.35))
      (+ x 10.0) (- y (* height 0.68)))
  (dc 'quadratic-curve-to (+ x 22.0) (- y (* height 0.88))
      x (- y height))
  (dc 'stroke)
  (dc 'restore)
  (void))

;; draw-cursor-target! : any/c -> void?
;;   Draw a faint lure marker where the player is steering.
(define (draw-cursor-target! dc)
  (dc 'save)
  (dc 'stroke-style "rgba(18,49,83,0.55)")
  (dc 'line-width 5.0)
  (dc 'begin-path)
  (dc 'arc target-x target-y 20. 0. (* 2.0 pi))
  (dc 'stroke)
  (dc 'stroke-style "rgba(255,255,255,0.92)")
  (dc 'line-width 2.5)
  (dc 'begin-path)
  (dc 'arc target-x target-y 18. 0. (* 2.0 pi))
  (dc 'stroke)
  (dc 'stroke-style "rgba(18,49,83,0.62)")
  (dc 'line-width 3.0)
  (dc 'begin-path)
  (dc 'move-to (- target-x 9.0) target-y)
  (dc 'line-to (+ target-x 9.0) target-y)
  (dc 'move-to target-x (- target-y 9.0))
  (dc 'line-to target-x (+ target-y 9.0))
  (dc 'stroke)
  (dc 'stroke-style "rgba(255,255,255,0.95)")
  (dc 'line-width 1.4)
  (dc 'begin-path)
  (dc 'move-to (- target-x 7.0) target-y)
  (dc 'line-to (+ target-x 7.0) target-y)
  (dc 'move-to target-x (- target-y 7.0))
  (dc 'line-to target-x (+ target-y 7.0))
  (dc 'stroke)
  (dc 'restore)
  (void))

;; draw-bubble! : any/c bubble? -> void?
;;   Draw one brief bubble from an eat effect.
(define (draw-bubble! dc b)
  (define alpha (* 0.75 (- 1.0 (/ (bubble-age b) (bubble-max-age b)))))
  (dc 'save)
  (dc 'stroke-style (format "rgba(255,255,255,~a)" alpha))
  (dc 'line-width 1.6)
  (dc 'begin-path)
  (dc 'arc (bubble-x b) (bubble-y b) (bubble-radius b) 0. (* 2.0 pi))
  (dc 'stroke)
  (dc 'restore)
  (void))

;; draw-score-pop! : any/c score-pop? -> void?
;;   Draw one floating score popup.
(define (draw-score-pop! dc p)
  (define alpha (- 1.0 (/ (score-pop-age p) (score-pop-max-age p))))
  (dc 'save)
  (dc 'fill-style (format "rgba(255,255,255,~a)" alpha))
  (dc 'stroke-style (format "rgba(24,49,83,~a)" (* 0.55 alpha)))
  (dc 'line-width 3.0)
  (dc 'font "bold 20px Georgia")
  (dc 'stroke-text (score-pop-text p) (score-pop-x p) (score-pop-y p))
  (dc 'fill-text (score-pop-text p) (score-pop-x p) (score-pop-y p))
  (dc 'restore)
  (void))

;; actor->pict : actor? boolean? -> pict?
;;   Convert one actor into a fish pict.
(define (actor->pict a player?)
  (standard-fish (actor-width a)
                 (actor-height a)
                 #:direction (actor-direction a)
                 #:color (actor-color a)
                 #:eye-color "black"
                 #:open-mouth (if player?
                                   (player-mouth-factor)
                                   (npc-mouth-factor a))))

;; draw-actor! : any/c actor? boolean? -> void?
;;   Draw an actor at its current world position.
(define (draw-actor! dc a player?)
  (define p (actor->pict a player?))
  (draw-pict p
             dc
             (actor-x a)
             (- (fish-center-y a)
                (/ (actor-height a) 2.0))))

;; draw-overlay! : any/c -> void?
;;   Draw score and game-over overlay text.
(define (draw-overlay! dc)
  (dc 'save)
  (dc 'fill-style "rgba(24,49,83,0.90)")
  (dc 'font "bold 20px Georgia")
  (dc 'fill-text (format "Score ~a" (obs-peek @score)) 24. 34.)
  (dc 'font "bold 16px Georgia")
  (dc 'fill-text (format "Best ~a" (obs-peek @best-score)) 24. 58.)
  (cond
    [game-over?
     (dc 'fill-style "rgba(24,49,83,0.74)")
     (dc 'fill-rect 0. 0. canvas-width canvas-height)
     (dc 'fill-style "#ffffff")
     (dc 'font "bold 38px Georgia")
     (dc 'fill-text "Hungry Fish" 278. 184.)
     (dc 'font "bold 22px Georgia")
     (dc 'fill-text "Too ambitious. Click to restart." 274. 226.)]
    [(eq? game-phase 'intro)
     (dc 'fill-style "rgba(24,49,83,0.52)")
     (dc 'fill-rect 168. 22. 524. 72.)
     (dc 'fill-style "#ffffff")
     (dc 'font "bold 24px Georgia")
     (dc 'fill-text "Tutorial: eat the small salmon fish" 198. 52.)
     (dc 'font "bold 16px Georgia")
     (dc 'fill-text "Then the full school arrives." 305. 78.)])
  (dc 'restore)
  (void))

;; draw-scene! : -> void?
;;   Redraw the full game scene.
(define (draw-scene!)
  (define ctx (get-drawing-context))
  (cond
    [(not ctx)
     (:= @status "Waiting for the hungry-fish canvas...")
     (void)]
    [else
     (define dc (canvas-context->dc ctx))
     (draw-background! dc)
     (draw-seaweed! dc 132. (- canvas-height 18.0) 94. "#31a16a")
     (draw-seaweed! dc 176. (- canvas-height 24.0) 70. "#42aa77")
     (draw-seaweed! dc 720. (- canvas-height 18.0) 92. "#2a935f")
     (draw-seaweed! dc 764. (- canvas-height 24.0) 68. "#44aa79")
     (draw-cursor-target! dc)
     (for-each (lambda (npc) (draw-actor! dc npc #f)) npcs)
     (when player
       (draw-actor! dc player #t))
     (for-each (lambda (b) (draw-bubble! dc b)) bubbles)
     (for-each (lambda (p) (draw-score-pop! dc p)) score-pops)
     (draw-overlay! dc)
     (void)]))

;; handle-pointer-move! : any/c -> void?
;;   Update the steering target from mouse movement.
(define (handle-pointer-move! event)
  (define-values (x y)
    (client-position->canvas-position
     (mouse-event-client-x event)
     (mouse-event-client-y event)))
  (set! target-x x)
  (set! target-y y))

;; handle-canvas-click! : any/c -> void?
;;   Restart the run from the canvas after game over.
(define (handle-canvas-click! _event)
  (when game-over?
    (reset-game!)
    (draw-scene!)))

;; ensure-pointer-listeners! : -> void?
;;   Attach pointer listeners to the canvas once it exists.
(define (ensure-pointer-listeners!)
  (unless move-callback
    (set! move-callback
          (procedure->external handle-pointer-move!)))
  (unless click-callback
    (set! click-callback
          (procedure->external handle-canvas-click!)))
  (unless pointer-listeners-installed?
    (define canvas (get-canvas))
    (when canvas
      (js-add-event-listener! canvas "mousemove" move-callback)
      (js-add-event-listener! canvas "click" click-callback)
      (set! pointer-listeners-installed? #t)))
  (void))

;; queue-draw! : -> void?
;;   Schedule a redraw after observables settle.
(define (queue-draw!)
  (void
   (js-window-set-timeout/delay
    (procedure->external
     (lambda _
       (draw-scene!)))
    0.0)))

;; schedule-next-frame! : -> void?
;;   Queue the next animation step when the loop is active.
(define (schedule-next-frame!)
  (when animation-running?
    (js-window-set-timeout/delay frame-callback frame-delay-ms)))

;; step-frame! : -> void?
;;   Advance and redraw the game, then schedule the next frame.
(define (step-frame!)
  (ensure-pointer-listeners!)
  (unless (or (obs-peek @paused) game-over?)
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

;; set-traffic! : any/c -> void?
;;   Update the traffic slider and resync the NPC school.
(define (set-traffic! n)
  (:= @traffic n)
  (sync-npc-population!)
  (queue-draw!))

;; toggle-paused! : -> void?
;;   Pause or resume the run when the player is alive.
(define (toggle-paused!)
  (unless game-over?
    (:= @paused (not (obs-peek @paused)))
    (:= @status
        (if (obs-peek @paused)
            "Game paused."
            "Game resumed. Chase smaller fish and stay away from larger ones."))
    (queue-draw!)))

(define control-style
  "width:290px; min-width:290px;")

(define preview-style
  "width:min(900px, 100%);")

(define canvas-style
  "display:block; width:min(860px, 100%); height:auto; border-radius:24px; box-shadow:0 18px 42px rgba(18,58,92,0.16); background:#d8f3ff; cursor:none;")

(define action-row-style
  "display:grid; grid-template-columns: 8rem 1fr; gap:0.75rem; align-items:start;")

(define status-style
  "width:100%; min-height:3.5rem;")

(define pict-hungry-fish-app
  (ui-window
   (ui-container #:style "width:min(1240px, calc(100vw - 28px));"
    (ui-vpanel
     (ui-h1 "Pict Hungry Fish")
     (ui-text "A mouse-only mini game: steer a hungry fish through the aquarium and eat smaller prey.")
     (ui-text "Larger fish end the run, but every meal helps you grow.")
     (ui-hpanel
      (ui-group
       "Game"
       (ui-div
        #:attrs `((style ,control-style))
        (ui-vpanel
         (ui-text "Traffic")
         (ui-slider @traffic set-traffic! 5 16)
         (ui-text (@traffic . ~> .
                             (lambda (n)
                               (format "~a roaming fish" n))))
         (ui-text "Player mouth openness")
         (ui-slider @mouth-open
                    (lambda (n)
                      (:= @mouth-open n))
                    0
                    100)
         (ui-text (@mouth-open . ~> .
                                (lambda (n)
                                  (format "~a%%" n))))
         (ui-text (@score . ~> .
                          (lambda (n)
                            (format "Current score: ~a" n))))
         (ui-text (@best-score . ~> .
                               (lambda (n)
                                 (format "Best score: ~a" n))))
         (ui-button (@sound-enabled . ~> .
                                     (lambda (enabled?)
                                       (if enabled? "Sound on" "Enable sound")))
                    toggle-sound!
                    #:attrs '((style "width:100%;")))
         (ui-div
          #:attrs `((style ,action-row-style))
          (ui-button (@paused . ~> .
                              (lambda (paused?)
                                (if paused? "Resume" "Pause")))
                     toggle-paused!
                     #:attrs '((style "width:100%;")))
          (ui-button "Reset run"
                     reset-game!
                     #:attrs '((style "width:100%;"))))
         (ui-div
          #:attrs `((style ,status-style))
          (ui-text @status)))))
      (ui-group
       "Preview"
       (ui-div
        #:attrs `((style ,preview-style))
        (ui-vpanel
         (ui-text "Move the cursor inside the water to steer. The lure ring marks your target.")
         (ui-canvas #:id canvas-name
                    #:width canvas-width
                    #:height canvas-height
                    #:style canvas-style)
         (ui-text "Tip: start by skimming the edges for tiny fish, then challenge the middle once you have grown.")))))))))

(define app-renderer
  (ui-render pict-hungry-fish-app))

(define light-theme
  (ui-theme 'light
            "we-theme-light"
            "web-easy-core.css"
            "theme-light.css"
            #f))

(define theme-manager
  (ui-install-theme-manager! light-theme))

(ui-mount-renderer! app-renderer)
(reset-game!)
(ensure-pointer-listeners!)
(draw-scene!)
(start-animation!)
