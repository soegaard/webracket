;; Bug repro: WebRacket currently accepts a value where let-values expects zero.
(let-values ([() (values 1)])
  3)
