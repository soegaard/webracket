#lang racket
(include "minischeme.rkt")

(define maze-program
"; S = Start
; G = Goal
(define maze
  '(\"┌─────────────┬─────────────┐\"
    \"│S            │            G│\"
    \"│ ┌─────┐ ┌───┴───┐ ┌─────┐ │\"
    \"│ │     │ │       │ │     │ │\"
    \"│ └─────┘ └─────┬─┘ └─────┘ │\"
    \"│               │           │\"
    \"│ ┌─────┐ ┌─────┼─────┐ ┌─┐ │\"
    \"│ │     │ │               │ │\"
    \"├─┼───┐ │ │ ┌─────┬─────┐ │ ├\"
    \"│ │   │ │ │ │     │     │ │ │\"
    \"│ │   └─┘ │ └─────┼─────┘ │ │\"
    \"│ │       │               │ │\"
    \"│ └─────┐ │ ┌─────┬─────┐ │ │\"
    \"│       │ │ │     │     │ │ │\"
    \"│ ┌─────┘ └───┼───┼─────┘ │ │\"
    \"│ │           │           │ │\"
    \"│ └─────┐ ┌───┼───┐ ┌─────┘ │\"
    \"│       │ │       │ │       │\"
    \"│ ┌─────┘ └───────┘ └─────┐ │\"
    \"│                         │ │\"
    \"└───────────────────────────┘\"))

(define height (length maze))
(define width (if (null? maze) 0 (string-length (car maze))))

(define (cell-at cell)
  (string-ref (list-ref maze (cadr cell)) (car cell)))

(define (inside? cell)
  (and (<= 0 (car cell))
       (< (car cell) width)
       (<= 0 (cadr cell))
       (< (cadr cell) height)))

(define wall-chars
  '(#\\─ #\\│ #\\┌ #\\┐ #\\└ #\\┘ #\\├ #\\┤ #\\┬ #\\┴ #\\┼))

(define (wall? ch)
  (member ch wall-chars))

(define (blocked? cell)
  (wall? (cell-at cell)))

(define (find-marker marker)
  (let loopy ((y 0))
    (if (= y height)
        #f
        (let loopx ((x 0))
          (cond
            ((= x width) (loopy (+ y 1)))
            ((char=? (string-ref (list-ref maze y) x) marker)
             (list x y))
            (else (loopx (+ x 1))))))))

(define start (find-marker #\\S))
(define goal  (find-marker #\\G))

(define (neighbors cell)
  (let* ((x (car cell))
         (y (cadr cell))
         (candidates (list (list (+ x 1) y)
                           (list (- x 1) y)
                           (list x (+ y 1))
                           (list x (- y 1)))))
    (let loop ((cs candidates))
      (cond
        ((null? cs) '())
        ((and (inside? (car cs))
              (not (blocked? (car cs))))
         (cons (car cs) (loop (cdr cs))))
        (else (loop (cdr cs)))))))

(define (enqueue-neighbors ns path q seen-now)
  (if (null? ns)
      (list q seen-now)
      (let ((n (car ns)))
        (if (member n seen-now)
            (enqueue-neighbors (cdr ns) path q seen-now)
            (enqueue-neighbors (cdr ns)
                               path
                               (append q (list (cons n path)))
                               (cons n seen-now))))))

(define (solve queue seen)
  (if (null? queue)
      #f
      (let* ((path (car queue))
             (node (car path)))
        (if (equal? node goal)
            (reverse path)
            (let* ((state (enqueue-neighbors (neighbors node)
                                             path
                                             (cdr queue)
                                             seen))
                   (new-queue (car state))
                   (new-seen (cadr state)))
              (solve new-queue new-seen))))))

(define (path-cell? c path)
  (and path (member c path)))

(define (render-cell cell path)
  (cond
    ((equal? cell start) #\\S)
    ((equal? cell goal) #\\G)
    ((wall? (cell-at cell)) (cell-at cell))
    ((path-cell? cell path) #\\*)
    (else #\\.)))

(define (render-row y path)
  (let loop ((x 0))
    (if (= x width)
        '()
        (cons (render-cell (list x y) path)
              (loop (+ x 1))))))

(define (join-lines lines)
  (if (null? lines)
      \"\"
      (if (null? (cdr lines))
          (car lines)
          (string-append (car lines) \"\\n\" (join-lines (cdr lines))))))

(define (render-maze path)
  (let loop ((y 0))
    (if (= y height)
        '()
        (cons (list->string (render-row y path))
              (loop (+ y 1))))))

(define path (if (and start goal)
                 (solve (list (list start)) (list start))
                 #f))

(if path
    (join-lines (render-maze path))
    \"No path found.\")
")

(displayln (minischeme-process-input maze-program))
