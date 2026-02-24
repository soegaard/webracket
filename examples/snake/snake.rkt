(let ()
  (struct pos (x y))
  
  (define board-cols 20)
  (define board-rows 20)
  (define cell-size 20)

  (define board-width (* board-cols cell-size))
  (define board-height (* board-rows cell-size))
  (define board-width-f64 (exact->inexact board-width))
  (define board-height-f64 (exact->inexact board-height))
  (define cell-size-f64 (exact->inexact cell-size))

  (define background-color "#0b0f16")
  (define snake-color "#2ecc71")
  (define apple-color "#e74c3c")

  (define body (js-document-body))
  (js-set-attribute! body "style"
                     "margin:0;display:flex;justify-content:center;background:#03070c;")

  (define container (js-create-element "div"))
  (js-set-attribute! container "style"
                     (string-append "display:flex;flex-direction:column;align-items:center;gap:12px;"
                                    "padding:32px 24px;min-height:100vh;box-sizing:border-box;"
                                    "font-family:'Segoe UI',sans-serif;color:#ecf0f1;text-align:center;"))

  (define title (js-create-element "h1"))
  (js-set! title "textContent" "Snake")
  (js-set-attribute! title "style" "margin:0;font-size:32px;letter-spacing:4px;")

  (define scoreboard (js-create-element "div"))
  (js-set-attribute! scoreboard "style" "font-size:22px;font-weight:700;min-height:28px;")

  (define status (js-create-element "div"))
  (js-set-attribute! status "style" "font-size:14px;color:#95a5a6;min-height:18px;")

  (define canvas (js-create-element "canvas"))
  (js-set-canvas-width! canvas board-width)
  (js-set-canvas-height! canvas board-height)
  (js-set-attribute! canvas "style"
                     (string-append "border:2px solid #2ecc71;border-radius:10px;"
                                    "background:" background-color ";"
                                    "image-rendering:pixelated;"
                                    "width:" (number->string board-width) "px;"
                                    "height:" (number->string board-height) "px;"
                                    "box-shadow:0 18px 36px rgba(0,0,0,0.35);"))

  (js-append-child! container title)
  (js-append-child! container scoreboard)
  (js-append-child! container status)
  (js-append-child! container canvas)
  (js-append-child! body container)

  (define ctx (js-canvas-get-context canvas "2d" (js-undefined)))

  (define snake '())
  (define apple #f)
  (define direction (cons 1 0))
  (define pending-direction direction)
  (define score 0)
  (define interval-id #f)

  (define (dir-dx dir) (car dir))
  (define (dir-dy dir) (cdr dir))

  (define (make-dir dx dy)
    (cons dx dy))

  (define (opposite-direction? a b)
    (and (= (dir-dx a) (- (dir-dx b)))
         (= (dir-dy a) (- (dir-dy b)))))

  (define (try-set-direction! dx dy)
    (define new-dir (make-dir dx dy))
    (unless (opposite-direction? pending-direction new-dir)
      (set! pending-direction new-dir)))

  (define (pos=? a b)
    (and (= (pos-x a) (pos-x b))
         (= (pos-y a) (pos-y b))))

  
  (define (advance-pos p dir)
    (pos (+ (pos-x p) (dir-dx dir))
         (+ (pos-y p) (dir-dy dir))))

  (define (out-of-bounds? p)
    (or (< (pos-x p) 0)
        (< (pos-y p) 0)
        (>= (pos-x p) board-cols)
        (>= (pos-y p) board-rows)))

  (define (snake-contains? p segments)
    (let loop ([rest segments])
      (cond
        [(null? rest) #f]
        [(pos=? (car rest) p) #t]
        [else (loop (cdr rest))])))

  (define (drop-last segments)
    (cond
      [(null? segments) '()]
      [(null? (cdr segments)) '()]
      [else
       (let loop ([rest segments] [acc '()])
         (if (null? (cdr rest))
             (reverse acc)
             (loop (cdr rest) (cons (car rest) acc))))]))

  (define (spawn-apple segments)
    (define total-cells (* board-cols board-rows))
    (if (>= (length segments) total-cells)
        #f
        (let loop ()
          (define candidate (pos (random board-cols) (random board-rows)))
          (if (snake-contains? candidate segments)
              (loop)
              candidate))))

  (define (update-score!)
    (js-set! scoreboard "textContent"
             (string-append "Score: " (number->string score))))

  (define (set-status! message)
    (js-set! status "textContent" message))

  (define (draw!)
    (js-set-canvas2d-fill-style! ctx background-color)
    (js-canvas2d-fill-rect ctx 0. 0. board-width-f64 board-height-f64)
    (when apple
      (js-set-canvas2d-fill-style! ctx apple-color)
      (js-canvas2d-fill-rect ctx
                             (exact->inexact (* (pos-x apple) cell-size))
                             (exact->inexact (* (pos-y apple) cell-size))
                             cell-size-f64
                             cell-size-f64))
    (js-set-canvas2d-fill-style! ctx snake-color)
    (for ([segment (in-list snake)])
      (js-canvas2d-fill-rect ctx
                             (exact->inexact (* (pos-x segment) cell-size))
                             (exact->inexact (* (pos-y segment) cell-size))
                             cell-size-f64
                             cell-size-f64)))

  (define (reset-state!)
    (set! score 0)
    (set! direction (make-dir 1 0))
    (set! pending-direction direction)
    (define center-x (quotient board-cols 2))
    (define center-y (quotient board-rows 2))
    (set! snake (list (pos center-x center-y)
                      (pos (sub1 center-x) center-y)
                      (pos (- center-x 2) center-y)))
    (set! apple (spawn-apple snake))
    (update-score!)
    (set-status! "Use the arrow keys or WASD to move.")
    (draw!))

  (define (step!)
    (set! direction pending-direction)
    (define head (car snake))
    (define new-head (advance-pos head direction))
    (define ate? (and apple (pos=? new-head apple)))
    (define trimmed (drop-last snake))
    (define body-to-check (if ate? snake trimmed))
    (if (or (out-of-bounds? new-head)
            (snake-contains? new-head body-to-check))
        (game-over! (string-append "Game over! Final score: " (number->string score)))
        (if ate?
            (let ()
              (define new-snake (cons new-head snake))
              (set! snake new-snake)
              (set! score (add1 score))
              (update-score!)
              (set! apple (spawn-apple new-snake))
              (draw!)
              (when (not apple)
                (game-over! (string-append "You win! Final score: "
                                           (number->string score)))))
            (begin
              (set! snake (cons new-head trimmed))
              (draw!)))))

  (define step-callback (procedure->external step!))

  (define (start-loop!)
    (when interval-id
      (js-window-clear-interval interval-id))
    (set! interval-id (js-window-set-interval step-callback 120.)))

  (define (restart-game!)
    (reset-state!)
    (start-loop!))

  (define restart-callback (procedure->external restart-game!))

  (define (game-over! message)
    (set-status! message)
    (when interval-id
      (js-window-clear-interval interval-id)
      (set! interval-id #f)
      (js-window-set-timeout/delay restart-callback 800.)))

  (define (handle-key event)
    (define key (js-ref event "key"))
    (when (string? key)
      (cond
        [(or (string=? key "ArrowUp") (string=? key "w") (string=? key "W"))
         (js-event-prevent-default event)
         (try-set-direction! 0 -1)]
        [(or (string=? key "ArrowDown") (string=? key "s") (string=? key "S"))
         (js-event-prevent-default event)
         (try-set-direction! 0 1)]
        [(or (string=? key "ArrowLeft") (string=? key "a") (string=? key "A"))
         (js-event-prevent-default event)
         (try-set-direction! -1 0)]
        [(or (string=? key "ArrowRight") (string=? key "d") (string=? key "D"))
         (js-event-prevent-default event)
         (try-set-direction! 1 0)]
        [else (void)])))

  (define keydown-callback (procedure->external handle-key))

  (js-add-event-listener! (js-window-window) "keydown" keydown-callback)

  (reset-state!)
  (start-loop!)

  )
