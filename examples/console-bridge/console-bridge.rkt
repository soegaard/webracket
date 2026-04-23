(define counter 0)

(define (increment! amount)
  (set! counter (+ counter amount))
  counter)

(define (current-counter)
  counter)

(define (explode!)
  (error 'explode! "boom"))

(display "console bridge example ready")
