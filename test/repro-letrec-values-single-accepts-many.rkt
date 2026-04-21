;; Bug repro: WebRacket currently accepts multiple values for a single
;; target in letrec-values.
(letrec-values ([(x) (values 1 2)])
  x)
