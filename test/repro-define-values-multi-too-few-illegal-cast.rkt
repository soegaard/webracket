;; Bug repro: WebRacket crashes with an illegal cast instead of reporting
;; a value-count mismatch for too few values.
(define-values (x y) (values 1))
(+ x y)
