;;; 
;;; SNAKE
;;;

; This is an implementation of the classical snake game.

(define cell-size       20)
(define cell-size-f     (exact->inexact cell-size))
(define columns         20)
(define rows            20)
(define canvas-width    (* columns cell-size))
(define canvas-height   (* rows cell-size))
(define canvas-width-f  (exact->inexact canvas-width))
(define canvas-height-f (exact->inexact canvas-height))

;;;
;;; Helpers
;;;

(define (inexact x)
  (if (exact? x)
      (exact->inexact x)
      x))

(define (drop-last xs)
  (reverse (cdr (reverse xs))))

;;;
;;; Positions and Directions
;;;

(define (make-pos x y)
  (cons x y))

(define pos-x car)
(define pos-y cdr)

(define (pos=? a b)
  (and (= (pos-x a) (pos-x b))
       (= (pos-y a) (pos-y b))))

(define (pos-in-list? p lst)
  (cond
    [(null? lst) #f]
    [(pos=? (car lst) p) #t]
    [else (pos-in-list? p (cdr lst))]))


(define (within-grid? p)
  (and (<= 0 (pos-x p) (sub1 columns))
       (<= 0 (pos-y p) (sub1 rows))))

(define (next-pos head dir)
  (case dir
    [(up)    (make-pos (pos-x head) (sub1 (pos-y head)))]
    [(down)  (make-pos (pos-x head) (add1 (pos-y head)))]
    [(left)  (make-pos (sub1 (pos-x head)) (pos-y head))]
    [(right) (make-pos (add1 (pos-x head)) (pos-y head))]
    [else head]))

(define (opposite-direction? a b)
  (case a
    [(up)    (eq? b 'down)]
    [(down)  (eq? b 'up)]
    [(left)  (eq? b 'right)]
    [(right) (eq? b 'left)]
    [else #f]))

;;;
;;; DOM
;;;

(define body (js-document-body))

(define wrapper (js-create-element "div"))
(js-set-attribute! wrapper "style"
                   "display:flex;flex-direction:column;align-items:center;gap:16px;margin:24px auto;font-family:monospace;color:#0f172a;")

(define score-display (js-create-element "div"))
(js-set-attribute! score-display "style"
                   "font-size:20px;letter-spacing:1px;text-transform:uppercase;")
(js-set! score-display "textContent" "Score: 0")
(js-append-child! wrapper score-display)

(define canvas (js-create-element "canvas"))
(js-set-canvas-width! canvas canvas-width)
(js-set-canvas-height! canvas canvas-height)
(js-set-attribute! canvas "style"
                   "background:#0f172a;border:2px solid #1f2937;border-radius:4px;box-shadow:0 12px 30px rgba(15,23,42,0.35);image-rendering:pixelated;")
(js-append-child! wrapper canvas)
(js-append-child! body wrapper)

(define ctx (js-canvas-get-context canvas "2d" (js-undefined)))

;;;
;;; DOM UPDATERS
;;;

(define (update-score! score)
  (js-set! score-display "textContent"
          (string-append "Score: " (number->string score))))

(define (draw-background!)
  (js-set-canvas2d-fill-style! ctx "#0b1220")
  (js-canvas2d-fill-rect ctx
                         0. 0.
                         canvas-width-f canvas-height-f))

(define (draw-apple! apple)
  (js-set-canvas2d-fill-style! ctx "#f87171")
  (js-canvas2d-fill-rect ctx
                         (inexact (* cell-size-f (pos-x apple)))
                         (inexact (* cell-size-f (pos-y apple)))
                         cell-size-f
                         cell-size-f))

(define (draw-snake! snake)
  (when (pair? snake)
    (define head (car snake))
    (js-set-canvas2d-fill-style! ctx "#4ade80")
    (js-canvas2d-fill-rect ctx
                           (inexact (* cell-size-f (pos-x head)))
                           (inexact (* cell-size-f (pos-y head)))
                           cell-size-f
                           cell-size-f)
    (js-set-canvas2d-fill-style! ctx "#34d399")
    (for ([segment (in-list (cdr snake))])
      (js-canvas2d-fill-rect ctx
                             (inexact (* cell-size-f (pos-x segment)))
                             (inexact (* cell-size-f (pos-y segment)))
                             cell-size-f
                             cell-size-f))))
;;;
;;; GAME STATE
;;;

(define snake             '())
(define apple             (make-pos 0 0))
(define direction         'right)
(define pending-direction 'right)
(define score             0)


(define (random-empty-cell)
  (let loop ()
    (define candidate (make-pos (random columns) (random rows)))
    (if (pos-in-list? candidate snake)
        (loop)
        candidate)))

(define (randomize-apple!)
  (set! apple (random-empty-cell)))

(define (queue-direction! new-dir)
  (unless (opposite-direction? direction new-dir)
    (set! pending-direction new-dir)))


(define (reset-game!)
  (set! snake (list (make-pos 5 10)
                    (make-pos 4 10)
                    (make-pos 3 10)))
  (set! direction 'right)
  (set! pending-direction 'right)
  (set! score 0)
  (update-score! score)
  (randomize-apple!))


(define (advance!)
  (when (pair? snake)
    (set! direction  pending-direction)
    (define head     (car snake))
    (define new-head (next-pos head direction))
    (define grew?    (pos=? new-head apple))
    (define body     (if grew? snake (drop-last snake)))
    (if (or (not (within-grid? new-head))
            (pos-in-list? new-head body))
        (reset-game!)
        (begin
          (set! snake (cons new-head body))
          (when grew?
            (set! score (add1 score))
            (update-score! score)
            (randomize-apple!))))
    (render!)))

(define (render!)
  (draw-background!)
  (draw-apple! apple)
  (draw-snake! snake))


(define (handle-key evt)
  (define key (js-ref evt "key"))
  (cond
    [(or (string=? key "ArrowUp") (string=? key "w") (string=? key "W"))
     (queue-direction! 'up)
     (js-event-prevent-default evt)]
    [(or (string=? key "ArrowDown") (string=? key "s") (string=? key "S"))
     (queue-direction! 'down)
     (js-event-prevent-default evt)]
    [(or (string=? key "ArrowLeft") (string=? key "a") (string=? key "A"))
     (queue-direction! 'left)
     (js-event-prevent-default evt)]
    [(or (string=? key "ArrowRight") (string=? key "d") (string=? key "D"))
     (queue-direction! 'right)
     (js-event-prevent-default evt)]
    [else (void)]))


(define key-listener  (procedure->external handle-key))
(define tick-callback (procedure->external advance!))

(reset-game!)

(js-add-event-listener! (js-document) "keydown" key-listener)
(js-window-set-interval tick-callback 120.)

(js-log "Hello world!")
