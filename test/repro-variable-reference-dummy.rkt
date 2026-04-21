;; Bug repro: variable references are compiled as dummy placeholders, so
;; variable-reference predicates report incorrect results.
(list (variable-reference-constant? (#%variable-reference))
      (let ([x 1])
        (variable-reference-constant? (#%variable-reference x)))
      (let ([x 1])
        (variable-reference-from-unsafe? (#%variable-reference x))))
