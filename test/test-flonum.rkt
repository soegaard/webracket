;; Additional flonum tests focused on arithmetic operations.
(list
 (list "4.3.3 Flonums — Arithmetic"
       (list "exact->inexact"
             (let ([neg-fixnum (exact->inexact -12)]
                   [pos-fixnum (exact->inexact  12)]
                   [zero       (exact->inexact   0)])
               (and (list neg-fixnum -12.0)
                    (fl= pos-fixnum  12.0)
                    (fl= zero         0.0))))
       (list "fl+"
             (let* ([three-halves   (exact->inexact 3/2)]
                    [five-halves    (exact->inexact 5/2)]
                    [eleven-fourths (exact->inexact 11/4)]
                    [pos-values     (list three-halves five-halves 3.0 4.0)]
                    [mixed-values   (list 5.0 (- three-halves) (- eleven-fourths) -1.0)]
                    [nan-sum        (fl+ +nan.0 1.0)])
               (and (fl= (fl+) 0.0)
                    (fl= (fl+ 4.0) 4.0)
                    (fl= (fl+ 1.0 2.0) 3.0)
                    (fl= (fl+ 1.0 2.0 3.0 4.0) 10.0)
                    #;(fl= (apply fl+ pos-values) (exact->inexact 11))  ; todo
                    #;(fl= (apply fl+ (cons 0.0 mixed-values)) 0.0)     ; todo
                    (nan? nan-sum))))
       (list "fl-"
             (let* ([seven-halves      (exact->inexact 7/2)]
                    [twenty-one-halves (exact->inexact 21/2)]
                    [nine-halves       (exact->inexact 9/2)]
                    [five-halves       (exact->inexact 5/2)]
                    [eleven-fourths    (exact->inexact 11/4)]
                    [nan-diff          (fl- +nan.0 2.0)])
               (and (fl= (fl- seven-halves) (exact->inexact -7/2))
                     (fl= (fl- twenty-one-halves nine-halves) (exact->inexact 6))
                     (fl= (fl- 20.0 5.0 3.0) 12.0)
                     (fl= (fl- (- 5.0) (- five-halves)) (exact->inexact -5/2))
                     (fl= (fl- 0.0 five-halves eleven-fourths) -5.25)
                     (nan? nan-diff))))
       (list "fl*"
             (let* ([half         (exact->inexact 1/2)]
                    [three-halves (exact->inexact 3/2)]
                    [nan-prod     (fl* +nan.0 2.0)])
               (and (fl= (fl*) 1.0)
                     (fl= (fl* three-halves) three-halves)
                     (fl= (fl* 2.0 3.0) 6.0)
                     (fl= (fl* -2.0 4.0 three-halves) (exact->inexact -12))
                     (fl= (fl* half half half half) (exact->inexact 1/16))
                     (fl= (fl* -1.0 -1.0 -1.0 -1.0) 1.0)
                     (nan? nan-prod))))
       (list "fl/"
             (let* ([one-quarter  (exact->inexact 1/4)]
                    [one-eighth   (exact->inexact 1/8)]
                    [half         (exact->inexact 1/2)]
                    [positive-inf (fl/ 1.0 0.0)]
                    [negative-inf (fl/ -1.0 0.0)]
                    [nan-quotient (fl/ 0.0 0.0)])
               (and (fl= (fl/ 4.0) one-quarter)
                    (fl= (fl/ 18.0 3.0) 6.0)
                    (fl= (fl/ 64.0 4.0 2.0) 8.0)
                    (fl= (fl/ -45.0 -5.0 3.0) 3.0)
                    (fl= (fl/ one-eighth half) one-quarter)
                    (fl= (fl/ (- half)) (exact->inexact -2))
                    (fl= positive-inf +inf.0)
                    (fl= negative-inf -inf.0)
                    (nan? nan-quotient)))))
 )
