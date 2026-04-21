;; Bug repro: WebRacket currently accepts multiple values for a single target.
(define-values (x) (values 1 2))
x
