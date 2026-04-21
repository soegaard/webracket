;; Bug repro: WebRacket currently crashes with an illegal cast instead of
;; reporting a value-count mismatch for too few values in let-values.
(let-values ([(x y) 1])
  (+ x y))
