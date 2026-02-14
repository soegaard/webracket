#lang racket/base
;; Helpers for timing output in the compiler driver.

(require (only-in racket/format ~r ~a)
         (only-in racket/base exact-nonnegative-integer?)
         (only-in racket/contract
                  contract-out -> any/c listof list/c or/c)
         (only-in racket/list first second third)
         (only-in racket/string string-join))

(provide
 (contract-out
  [now-ms (-> real?)]
  [ms->s (-> real? real?)]
  [pct (-> real? real? real?)]
  [with-timing (-> (-> any/c) (values any/c real?))]
  [format-timing-table (-> (listof (list/c string? real?)) string?)]
  [format-timing-table/size
   (-> (listof (list/c string? real? (or/c #f exact-nonnegative-integer?)))
       string?)]))

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

;; Format a timing table from (label ms nodes) rows.
(define (format-timing-table/size rows)
  (define label-width
    (for/fold ([w 0]) ([row rows])
      (max w (string-length (first row)))))
  (define nodes-width
    (for/fold ([w 5]) ([row rows])
      (define nodes (third row))
      (define s (if nodes (~a nodes) "-"))
      (max w (string-length s))))
  (define secs-width 6)
  (define pct-width 5)
  (define (row->string label ms nodes total-ms)
    (define secs (ms->s ms))
    (define pct-val (pct ms total-ms))
    (define secs-str (~r secs #:min-width secs-width #:precision '(= 1)))
    (define pct-str (~r pct-val #:min-width pct-width #:precision '(= 1)))
    (define label-str (~a label #:min-width label-width))
    (define nodes-str (~a (if nodes nodes "-") #:min-width nodes-width #:align 'right))
    (format "  ~a :  ~a  ~a%  ~a" label-str secs-str pct-str nodes-str))
  (define total-ms
    (let ([total-row (for/first ([row rows] #:when (string=? (first row) "total")) row)])
      (if total-row
          (second total-row)
          (for/sum ([row rows]) (second row)))))
  (define secs-right (+ 2 label-width 4 secs-width))
  (define pct-right (+ secs-right 2 pct-width))
  (define nodes-right (+ pct-right 2 nodes-width))
  (define (pad-to-right current label right)
    (define needed (- right (string-length current) (string-length label)))
    (string-append current (make-string (max 1 needed) #\space) label))
  (define header
    (let* ([title "Timing breakdown"]
           [h1 (pad-to-right title "(s)" secs-right)]
           [h2 (pad-to-right h1 "(pct)" pct-right)])
      (pad-to-right h2 "(nodes)" nodes-right)))
  (string-join
   (cons header
         (for/list ([row rows])
           (row->string (first row) (second row) (third row) total-ms)))
   "\n"))
