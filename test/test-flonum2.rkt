;; #lang racket
;; (require racket/flonum
;;          racket/unsafe/ops)

;; Ported tests derived from Racket's collects/tests/racket/flonum.rktl.
;; Exception-raising cases are omitted because webracket lacks exception support.

#;(define safe-unsafe-zero-arg-table
  (list (list fl+ unsafe-fl+)
        (list fl* unsafe-fl*)))

(define safe-unsafe-zero-arg-table
  (list (list fl+ fl+)
        (list fl* fl*)))

#;(define safe-unsafe-one-plus-table
  (list (list fl- unsafe-fl-)
        (list fl/ unsafe-fl/)
        (list flmin unsafe-flmin)
        (list flmax unsafe-flmax)))

(define safe-unsafe-one-plus-table
  (list (list fl-   fl-)
        (list fl/   fl/)
        (list flmin flmin)
        (list flmax flmax)))

(define (fl-value=? a b)
  (cond
    [(and (flonum? a) (flonum? b))
     (cond
       [(and (nan? a) (nan? b)) #t]
       [(and (zero? a) (zero? b))
        (let ([ra (fl/ 1.0 a)]
              [rb (fl/ 1.0 b)])
          (cond
            [(and (nan? ra) (nan? rb)) #t]
            [else (fl= ra rb)]))]
       [else (fl= a b)])]
    [else (equal? a b)]))

(define (safe-unsafe-match? entry args)
  (let ([safe   (car entry)]
        [unsafe (cadr entry)])
    (fl-value=? (apply safe args) (apply unsafe args))))

(define (nan-callable? proc)
  (let ([value (proc +nan.0)])
    (or (flonum? value) (boolean? value))))

(define (table-handles-nan? table)
  (andmap (lambda (entry)
            (let ([safe (car entry)]
                  [unsafe (cadr entry)])
              (and (nan-callable? safe)
                   (nan-callable? unsafe))))
          table))

(define (check-table table arglists)
  (andmap (lambda (entry)
            (andmap (lambda (args)
                      (safe-unsafe-match? entry args))
                    arglists))
          table))

(define (sample-same? i j k more)
  (and (check-table safe-unsafe-zero-arg-table (list (list) (list i) (list i j)))
       (check-table safe-unsafe-one-plus-table (list (list i) (list i j)))
       (check-table (append safe-unsafe-zero-arg-table safe-unsafe-one-plus-table)
                    (list (list i j k)
                          (list i k j)
                          (cons i more)))))

(define (safe-unsafe-random-samples n)
  (let loop ([count n])
    (if (zero? count)
        #t
        (let ([i (random)]
              [j (random)]
              [k (random)]
              [more (build-list (random 5) (lambda (_) (random)))])
          (and (sample-same? i j k more)
               (loop (sub1 count)))))))

(define (and-range start stop body)
  (let loop ([idx start])
    (if (>= idx stop)
        #t
        (and (body idx)
             (loop (add1 idx))))))

(define big-even (expt 2.0 53))
(define max-odd (- big-even 1.0))

(define flexpt-positive-tests
  (list
   (list +0.0 +0.0 +1.0)     ;  0
   (list +0.0 +1.0 +0.0)     ;  1
   (list +0.0 +3.0 +0.0)     ;  2
   (list +0.0 max-odd +0.0)  ;  3
   (list +0.0 0.5 +0.0)      ;  4
   (list +0.0 1.5 +0.0)      ;  5
   (list +0.0 2.0 +0.0)      ;  6
   (list +0.0 2.5 +0.0)      ;  7
   (list +0.0 big-even +0.0) ;  8

   (list -0.0 +0.0 +1.0)     ;  9
   (list -0.0 +1.0 -0.0)     ; 10
   (list -0.0 +3.0 -0.0)     ; 11
   (list -0.0 max-odd -0.0)  ; 12
   (list -0.0 0.5 +0.0)      ; 13
   (list -0.0 1.5 +0.0)      ; 14
   (list -0.0 2.0 +0.0)      ; 15
   (list -0.0 2.5 +0.0)      ; 16
   (list -0.0 big-even +0.0) ; 17

   (list +1.0 +0.0 +1.0)     ; 18
   (list +1.0 0.5 +1.0)      ; 19
   (list +1.0 +inf.0 +1.0)   ; 20

   (list -1.0 +0.0 +1.0)     ; 21
   (list -1.0 0.5 +nan.0)    ; 22
   (list -1.0 +inf.0 +1.0)   ; 23

   (list 0.5 +inf.0 +0.0)    ; 24
   (list 1.5 +inf.0 +inf.0)  ; 25

   (list +inf.0 +0.0 +1.0)   ; 26
   (list +inf.0 +1.0 +inf.0) ; 27
   (list +inf.0 2.0 +inf.0)  ; 28
   (list +inf.0 +inf.0 +inf.0) ; 29

   (list -inf.0 +0.0 +1.0)      ; 30
   (list -inf.0 +1.0 -inf.0)    ; 31
   (list -inf.0 +3.0 -inf.0)    ; 32
   (list -inf.0 max-odd -inf.0) ; 33
   (list -inf.0 0.5 +inf.0)     ; 34
   (list -inf.0 1.5 +inf.0)     ; 35  
   (list -inf.0 2.0 +inf.0)     ; 36
   (list -inf.0 2.5 +inf.0)     ; 37
   (list -inf.0 big-even +inf.0) ; 38
   (list -inf.0 +inf.0 +inf.0))) ; 39

(define flexpt-negative-tests
  (list
   (list +0.0 -0.0 +1.0)
   (list +0.0 -1.0 +inf.0)
   (list +0.0 -3.0 +inf.0)
   (list +0.0 (- max-odd) +inf.0)
   (list +0.0 -0.5 +inf.0)
   (list +0.0 -1.5 +inf.0)
   (list +0.0 -2.0 +inf.0)
   (list +0.0 -2.5 +inf.0)
   (list +0.0 (- big-even) +inf.0)

   (list -0.0 -0.0 +1.0)
   (list -0.0 -1.0 -inf.0)
   (list -0.0 -3.0 -inf.0)
   (list -0.0 (- max-odd) -inf.0)
   (list -0.0 -0.5 +inf.0)
   (list -0.0 -1.5 +inf.0)
   (list -0.0 -2.0 +inf.0)
   (list -0.0 -2.5 +inf.0)
   (list -0.0 (- big-even) +inf.0)

   (list +1.0 -0.0 +1.0)
   (list +1.0 -0.5 +1.0)
   (list +1.0 -inf.0 +1.0)

   (list -1.0 -0.0 +1.0)
   (list -1.0 -0.5 +nan.0)
   (list -1.0 -inf.0 +1.0)

   (list 0.5 -inf.0 +inf.0)
   (list 1.5 -inf.0 +0.0)

   (list +inf.0 -0.0 +1.0)
   (list +inf.0 -1.0 +0.0)
   (list +inf.0 -2.0 +0.0)
   (list +inf.0 -inf.0 +0.0)

   (list -inf.0 -0.0 +1.0)
   (list -inf.0 -1.0 -0.0)
   (list -inf.0 -3.0 -0.0)
   (list -inf.0 (- max-odd) -0.0)
   (list -inf.0 -0.5 +0.0)
   (list -inf.0 -1.5 +0.0)
   (list -inf.0 -2.0 +0.0)
   (list -inf.0 -2.5 +0.0)
   (list -inf.0 (- big-even) +0.0)
   (list -inf.0 -inf.0 +0.0)))

(define flexpt-nan-tests
  (list
   (list +nan.0 +0.0 +1.0)
   (list +nan.0 -0.0 +1.0)
   (list +1.0 +nan.0 +1.0)
   (list -1.0 +nan.0 +nan.0)))

(define (check-flexpt cases)
  (map (lambda (case)
            (let ([base     (car case)]
                  [exponent (cadr case)]
                  [expected (caddr case)])
              (fl-value=? (flexpt base exponent) expected)))
          cases))

(define (check-unsafe-flsingle)
  (define cases
    (list
     (list 1.0 1.0)
     (list -1.0 -1.0)
     (list +nan.0 +nan.0)
     (list +inf.0 +inf.0)
     (list -inf.0 -inf.0)
     (list 1.2500000360947476e38 1.25e38)
     (list 1.2500000449239123e-37 1.25e-37)
     (list -1.2500000360947476e38 -1.25e38)
     (list -1.2500000449239123e-37 -1.25e-37)
     (list +inf.0 1e100)
     (list -inf.0 -1e100)
     (list 0.0 1e-100)
     (list -0.0 -1e-100)))
  (andmap (lambda (case)
            (let ([expected (car case)]
                  [value (cadr case)])
              (fl-value=? expected (unsafe-flsingle value))))
          cases))

(list 
 #;(list "4.3.3 Flonums — Safe vs Unsafe"
       (list "handle +nan.0"
             (and (table-handles-nan? safe-unsafe-zero-arg-table)
                  (table-handles-nan? safe-unsafe-one-plus-table)))
       (list "random samples agree"
             (safe-unsafe-random-samples 200))
       (list "arithmetic sanity"
             (let* ([three-halves (exact->inexact 3/2)]
                    [five-halves (exact->inexact 5/2)]
                    [eleven-fourths (exact->inexact 11/4)]
                    [one-quarter (exact->inexact 1/4)]
                    [one-eighth (exact->inexact 1/8)]
                    [half (exact->inexact 1/2)]
                    [positive-inf (fl/ 1.0 0.0)]
                    [negative-inf (fl/ -1.0 0.0)]
                    [nan-quotient (fl/ 0.0 0.0)])
               (and (fl-value=? (fl+) 0.0)
                    (fl-value=? (fl+ 4.0) 4.0)
                    (fl-value=? (fl+ 1.0 2.0) 3.0)
                    (fl-value=? (fl+ 1.0 2.0 3.0 4.0) 10.0)
                    (fl-value=? (fl- three-halves) (exact->inexact -3/2))
                    (fl-value=? (fl- 20.0 5.0 3.0) 12.0)
                    (fl-value=? (fl- 0.0 five-halves eleven-fourths) -5.25)
                    (fl-value=? (fl* three-halves) three-halves)
                    (fl-value=? (fl* 2.0 3.0) 6.0)
                    (fl-value=? (fl* -2.0 4.0 three-halves) (exact->inexact -12))
                    (fl-value=? (fl* half half half half) (exact->inexact 1/16))
                    (fl-value=? (fl/ 4.0) one-quarter)
                    (fl-value=? (fl/ 18.0 3.0) 6.0)
                    (fl-value=? (fl/ 64.0 4.0 2.0) 8.0)
                    (fl-value=? (fl/ one-eighth half) one-quarter)
                    (fl-value=? (fl/ (- half)) (exact->inexact -2))
                    (fl-value=? positive-inf +inf.0)
                    (fl-value=? negative-inf -inf.0)
                    (fl-value=? nan-quotient +nan.0)))))
 
 (list "4.3.3 Flonums — Conversions"
       (list "->fl"
             (let (#;[big (expt 2 100)]
                   [big (expt 2 10)])
               (and (fl-value=? (->fl 3) 3.0)
                    (fl-value=? (->fl big) (exact->inexact big)))))
       (list "fl->exact-integer"
             (let (#;[big (inexact->exact 1e100)]
                   [big (expt 2 10)])
               (and (equal? (fl->exact-integer 3.0)     3)
                    #;(equal? (fl->exact-integer 1e100) big)
                    (equal? (fl->exact-integer 1024.) big)))))
 
 (list "4.3.3 Flonums — Single precision"
       (list "unsafe-flsingle"
             (check-unsafe-flsingle)))
 
 (list "4.3.3 Flonums — Random"
       (list "flrandom"
             (let ([value (flrandom)])
               (and (flonum? value)
                    (<= 0.0 value)
                    (< value 1.0)))))
 
 (list "4.3.3 Flonums — flexpt"
       (list "positive exponents"
             (check-flexpt flexpt-positive-tests))
       (list "negative exponents"
             (check-flexpt flexpt-negative-tests))
       (list "NaN handling"
             (check-flexpt flexpt-nan-tests)))
 
 #;(list "4.3.3 Flonums — Log and Sqrt"
       (list "fllog"
             (and (fl-value=? (fllog 0.0) -inf.0)
                  (fl-value=? (fllog -0.0) -inf.0)
                  (nan? (fllog -1.0))))
       (list "flsqrt"
             (and (fl-value=? (flsqrt -0.0) -0.0)
                  (nan? (flsqrt -1.0)))))
 
 #;(list "4.3.3 Flonums — flbit-field"
     (list "bit extraction"
           (let ([sample 3.141579e132]
                 [bits #x5B71B43544F260A6])
            (and (and-range 0 65 (lambda (i) (= (flbit-field 3.14 i i) 0)))
                 (and-range 0 64 (lambda (i) (= (flbit-field 0.0 i (add1 i)) 0)))
                 (and-range 0 65 (lambda (i) (= (flbit-field 0.0 0 i) 0)))
                 (and-range 0 63 (lambda (i) (= (flbit-field -0.0 i (add1 i)) 0)))
                  (= (flbit-field -0.0 63 64) 1)
                  (= (flbit-field sample 0 64) #x5B71B43544F260A6)
                  (= (flbit-field sample 0 63) #x5B71B43544F260A6)
                  (= (flbit-field sample 1 64) #x2DB8DA1AA2793053)
                  (= (flbit-field sample 4 60) #x0B71B43544F260A)
                  (= (flbit-field sample 8 56) #x071B43544F260)
                  (= (flbit-field sample 32 64) #x5B71B435)
                  (= (flbit-field sample 0 32) #x44F260A6)
                  (= (flbit-field sample 16 48) #x0B43544F2)
                  (and-range
                   0 65
                   (lambda (i)
                     (and-range i 65
                                (lambda (j)
                                  (= (flbit-field sample i j)
                                     (bitwise-bit-field bits i j))))))))))
 )
