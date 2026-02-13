#lang racket/base
;; Helpers for timing output in the compiler driver.

(require (only-in racket/format ~r ~a)
         (only-in racket/contract contract-out -> any/c listof list/c)
         (only-in racket/list first second)
         (only-in racket/string string-join))

(provide
 (contract-out
  [now-ms (-> real?)]
  [ms->s (-> real? real?)]
  [pct (-> real? real? real?)]
  [with-timing (-> (-> any/c) (values any/c real?))]
  [format-timing-table (-> (listof (list/c string? real?)) string?)]))

;; Return the current time in milliseconds as an inexact real.
(define (now-ms) (current-inexact-milliseconds))
;; Convert milliseconds to seconds.
(define (ms->s ms) (/ ms 1000.0))
;; Compute percentage of part relative to total.
(define (pct part total)
  (if (zero? total) 0.0 (* 100.0 (/ part total))))

;; Run thunk and return its value and elapsed milliseconds.
(define (with-timing thunk)
  (define start (now-ms))
  (define value (thunk))
  (define elapsed (- (now-ms) start))
  (values value elapsed))

;; Format a timing table from (label ms) rows.
(define (format-timing-table rows)
  (define label-width
    (for/fold ([w 0]) ([row rows])
      (max w (string-length (first row)))))
  (define (row->string label ms total-ms)
    (define secs (ms->s ms))
    (define pct-val (pct ms total-ms))
    (define secs-str (~r secs #:min-width 6 #:precision '(= 1)))
    (define pct-str (~r pct-val #:min-width 5 #:precision '(= 1)))
    (define label-str (~a label #:min-width label-width))
    (format "  ~a :  ~a  ~a%" label-str secs-str pct-str))
  (define total-ms
    (let ([total-row (for/first ([row rows] #:when (string=? (first row) "total")) row)])
      (if total-row
          (second total-row)
          (for/sum ([row rows]) (second row)))))
  (define header
    (let* ([title "Timing breakdown"]
           [secs-right (+ 2 label-width 4 6)] ; "  " + label + " :  " + secs(6)
           [pct-right (+ secs-right 2 5)]     ; "  " + pct(5)
           [pad1 (max 1 (- secs-right (string-length title) 3))]
           [title+secs (string-append title (make-string pad1 #\space) "(s)")]
           [pad2 (max 1 (- pct-right (string-length title+secs) 5))])
      (string-append title+secs (make-string pad2 #\space) "(pct)")))
  (string-join
   (cons header
         (for/list ([row rows])
           (row->string (first row) (second row) total-ms)))
   "\n"))
