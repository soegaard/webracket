(let ()
  (struct bullet (x y) #:mutable)
  (struct enemy  (x y) #:mutable)
  (struct player (x y) #:mutable)

  (define (inexact x)
    (if (exact? x) (exact->inexact x) x))
  
  (define canvas-width  480)
  (define canvas-height 640)
  (define width         (inexact canvas-width))
  (define height        (inexact canvas-height))

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
             (+ enemy-start-y (* row enemy-spacing-y)))))
  (define enemies-dir 1)

  (define status         'playing)
  (define left-pressed?  #f)
  (define right-pressed? #f)
  (define space-pressed? #f)
  (define time-since-last-shot shoot-cooldown)

  (define last-time #f)

  (define canvas (js-create-element "canvas"))
  (js-set-canvas-width!  canvas canvas-width)
  (js-set-canvas-height! canvas canvas-height)
  (js-set-attribute!     canvas "style"
                         (string-append
                          "background: black; image-rendering: pixelated; "
                          "display: block; margin: 32px auto; border: 2px solid #0ff;"))
  (js-set-attribute! (js-document-body) "style"
                     "margin: 0; background: #000; font-family: sans-serif;")
  (js-append-child! (js-document-body) canvas)

  (define ctx (js-canvas-get-context canvas "2d" (js-undefined)))

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

  (define (on-key-down evt)
    (define key      (js-ref evt "key"))
    (define handled? #f)
    (when (string? key)
      (cond
        [(left-key? key)
         (set! left-pressed? #t)
         (set! handled? #t)]
        [(right-key? key)
         (set! right-pressed? #t)
         (set! handled? #t)]
        [(space-key? key)
         (set! space-pressed? #t)
         (set! handled? #t)
         (fire!)]))
    (when handled?
      (js-event-prevent-default evt))
    (void))

  (define (on-key-up evt)
    (define key      (js-ref evt "key"))
    (define handled? #f)
    (when (string? key)
      (cond
        [(left-key? key)
         (set! left-pressed? #f)
         (set! handled? #t)]
        [(right-key? key)
         (set! right-pressed? #f)
         (set! handled? #t)]
        [(space-key? key)
         (set! space-pressed? #f)
         (set! handled? #t)]))
    (when handled?
      (js-event-prevent-default evt))
    (void))

  (define key-down-handler (procedure->external on-key-down))
  (define key-up-handler   (procedure->external on-key-up))
  (js-add-event-listener! (js-window-window) "keydown" key-down-handler)
  (js-add-event-listener! (js-window-window) "keyup"   key-up-handler)

  (define (clamp-player x)
    (min (- width half-player-width)
         (max half-player-width x)))

  (define (fire!)
    (when (and (eq? status 'playing)
               (>= time-since-last-shot shoot-cooldown))
      (define new-bullet (bullet (player-x player-pos)
                                 (- (player-y player-pos) bullet-height)))
      (set! bullets (cons new-bullet bullets))
      (set! time-since-last-shot 0.)))

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
    (for ([b (in-list bullets)])
      (set-bullet-y! b (- (bullet-y b) (* bullet-speed dt)))
      (cond
        [(<= (+ (bullet-y b) bullet-height) 0.) (void)]
        [else
         (define hit (find-hit-enemy b))
         (if hit
             (set! enemies (remove hit enemies eq?))
             (set! new-bullets (cons b new-bullets)))]))
    (set! bullets (reverse new-bullets)))

  (define (update-enemies dt)
    (when (pair? enemies)
      (define dx (* enemy-speed dt enemies-dir))
      (js-log (vector enemy-speed dt enemies-dir))
      (js-log dx)
      (define drop?
        (for/or ([e (in-list enemies)])
          (define new-x (+ (enemy-x e) dx))
          (or (< new-x enemy-margin)
              (> (+ new-x enemy-width) (- width enemy-margin)))))
      (when drop?
        (set! enemies-dir (- enemies-dir))
        (for ([e (in-list enemies)])
          (set-enemy-y! e (+ (enemy-y e) enemy-drop))))
      (for ([e (in-list enemies)])
        (set-enemy-x! e (+ (enemy-x e) dx)))))

  (define (enemy-reached-bottom?)
    (for/or ([e (in-list enemies)])
      (>= (+ (enemy-y e) enemy-height) height)))

  (define (update dt)
    (when (eq? status 'playing)
      (set! time-since-last-shot (+ time-since-last-shot dt))
      (define movement 0.)
      (when left-pressed? (set! movement (- movement 1.)))
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
      (when (eq? status 'playing)
        (update-enemies dt)
        (when (enemy-reached-bottom?)
          (set! status 'lost)))))

  (define instruction-text "←/→ or A/D to move, Space to shoot")

  (define (render!)
    (js-set-canvas2d-fill-style! ctx "#000")
    (js-canvas2d-fill-rect ctx 0. 0. (inexact width) (inexact height))

    (js-set-canvas2d-fill-style! ctx "#0ff")
    (define player-left (- (player-x player-pos) half-player-width))
    (js-canvas2d-fill-rect ctx
                           (inexact player-left)  (inexact (player-y player-pos))
                           (inexact player-width) (inexact player-height))

    (js-set-canvas2d-fill-style! ctx "#ff0")
    (for ([b (in-list bullets)])
      (js-canvas2d-fill-rect ctx
                             (inexact (- (bullet-x b) bullet-half-width))
                             (inexact (bullet-y b))
                             (inexact bullet-width)
                             (inexact bullet-height)))

    (js-set-canvas2d-fill-style! ctx "#f66")
    (for ([e (in-list enemies)])
      (js-canvas2d-fill-rect ctx
                             (inexact (enemy-x e)) (inexact (enemy-y e))
                             (inexact enemy-width) (inexact enemy-height)))

    (js-set-canvas2d-fill-style!    ctx "#fff")
    (js-set-canvas2d-font!          ctx "16px sans-serif")
    (js-set-canvas2d-text-align!    ctx "center")
    (js-set-canvas2d-text-baseline! ctx "top")
    (js-canvas2d-fill-text          ctx instruction-text (/ width 2.) 16. (void))

    (unless (eq? status 'playing)
      (js-set-canvas2d-font!          ctx "32px sans-serif")
      (js-set-canvas2d-text-baseline! ctx "middle")
      (define message (if (eq? status 'won)
                          "You saved the day!"
                          "The invaders landed!"))
      (js-canvas2d-fill-text ctx message (/ width 2.) (/ height 2.) (void))
      (js-set-canvas2d-font! ctx "18px sans-serif")
      (js-canvas2d-fill-text ctx "Refresh the page to play again."
                             (/ width 2.)
                             (+ (/ height 2.) 36.)
                             (void))))

  (define tick-external #f)

  (define (tick timestamp)
    (define delta (if last-time (- timestamp last-time) 0.))
    (define dt    (min 0.05 (* 0.001 delta)))
    (set! last-time timestamp)
    (update dt)
    (render!)
    (js-window-request-animation-frame tick-external))

  (set! tick-external (procedure->external tick))
  
  (render!)
  (js-window-request-animation-frame tick-external))
