;; Bug repro: WebRacket currently accepts multiple values for a single
;; target in let-values.
(let-values ([(x) (values 1 2)])
  x)
