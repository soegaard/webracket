;; Bug repro: WebRacket currently accepts a value where define-values expects zero.
(define-values () 1)
42
