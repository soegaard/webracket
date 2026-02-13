#lang racket/base
;; Helpers for timing output in the compiler driver.

(require (only-in racket/format ~r ~a)
         (only-in racket/contract contract-out))

(provide
 (contract-out
  [now-ms (-> real?)]
  [ms->s (-> real? real?)]
  [pct (-> real? real? real?)]
  [format-timing-table (-> (listof (list/c string? real?)) string?)]))

;; Return the current time in milliseconds as an inexact real.
(define (now-ms) (current-inexact-milliseconds))
;; Convert milliseconds to seconds.
(define (ms->s ms) (/ ms 1000.0))
;; Compute percentage of part relative to total.
(define (pct part total)
  (if (zero? total) 0.0 (* 100.0 (/ part total))))

;; Format a timing table from (label ms) rows.
(define (format-timing-table rows)
  (define label-width 10)
  (define (row->string label ms total-ms)
    (define secs (ms->s ms))
    (define pct-val (pct ms total-ms))
    (define secs-str (~r secs #:min-width 6 #:precision '(= 1)))
    (define pct-str (~r pct-val #:min-width 5 #:precision '(= 1)))
    (define label-str (~a label #:min-width label-width))
    (format "  ~a:  ~a  ~a%%" label-str secs-str pct-str))
  (define total-ms (for/sum ([row rows]) (second row)))
  (string-join
   (cons "Timing breakdown  (s)   (pct)"
         (for/list ([row rows])
           (row->string (first row) (second row) total-ms)))
   "\n"))
