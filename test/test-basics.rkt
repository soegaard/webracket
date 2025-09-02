;; The numbering follows the sections in "The Reference".
(list
 (list "3.8 Procedure Expressions: lambda and case-lambda"
       (list
        (list "lambda"
              (and (equal? (procedure? (lambda (x) (* x x))) #t)
                   (equal? (procedure? '(lambda (x) (* x x))) #f)
                   (equal? (apply (lambda (a b) (+ a b)) (list 3 4)) 7)
                   #;(let ([compose (lambda (f g) (lambda args (f (apply g args))))])
                       (equal? ((compose sqrt *) 12 75) 30)) ; todo : improve *
                   (let ([compose (lambda (f g) (lambda args (f (apply g args))))])
                     (equal? ((compose sqrt (λ (x y) (* x y))) 12 75) 30))
                   (equal? (procedure-arity (lambda x x)) -1)))
        (list "case-lambda"
              (list (equal? (procedure? (case-lambda)) #t)
                   (equal? (procedure? (case-lambda [(x) x])) #t)
                   (equal? (procedure? (case-lambda [(x) x] [(x y) (+ x y)])) #t)
                   (equal? ((case-lambda [(x) x] [(x y) (list x y)]) 11) 11)
                   (equal? ((case-lambda [(x) x] [(x y) (+ x y)]) 11 22) 33)
                   (equal? ((case-lambda [(x . y) x]) 11 22) 11)
                   (equal? ((case-lambda [(x . y) y]) 11 22) '(22))
                   (equal? (procedure-arity (case-lambda))                      '())
                   (equal? (procedure-arity (case-lambda [(x) x]))              1)
                   (equal? (procedure-arity (case-lambda [(x y) x]))            2)
                   (equal? (procedure-arity (case-lambda [(x . y) x]))         -2)
                   (equal? (procedure-arity (case-lambda [(x) x] [(x y) x]))   '(1 2))
                   (equal? (procedure-arity (case-lambda [(x y) x] [(x) x] ))  '(2 1))
                   (equal? (procedure-arity (case-lambda [(x) x] [(x . y) x])) '(1 -2))
                   (equal? (procedure-arity (case-lambda [(x) 0]
                                                         [(x y z) 1]
                                                         [(x y z w u . rest) 2])) '(1 3 -6))
                   ; todo - fails - result is (0 -1) instead of the expected -1
                   (list (procedure-arity (case-lambda [() 10] [x 1])) -1)))
        (list "procedure?"
              (and
                  (equal? (procedure? car) #t)
                  (equal? (procedure? 'car) #f)
                  (equal? (procedure? (lambda (x) (* x x))) #t)
                  (equal? (procedure? '(lambda (x) (* x x))) #f)
                  #;(equal? (procedure? call-with-current-continuation) #t)
                  #;(equal? (procedure? call-with-escape-continuation) #t)
                  (equal? (procedure? (case-lambda ((x) x) ((x y) (+ x y)))) #t)                  
                  (equal? (procedure-arity procedure?) 1)))))

 (list "4.1 Equality"
       (list "eqv?"
             (and (equal? (eqv? 'a 'a) #t)
                  (equal? (eqv? 'a 'b) #f)
                  (equal? (eqv? 2 2) #t)
                  (equal? (eqv? 2 2.0) #f)
                  (equal? (eqv? '() '()) #t)
                  (equal? (eqv? '10000 '10000) #t)
                  (equal? (eqv? 10000000000000000000 10000000000000000000) #t)
                  #;(equal? (eqv? 10000000000000000000 10000000000000000001) #f) ; todo - needs bignums
                  (equal? (eqv? 10000000000000000000 20000000000000000000) #f)
                  (equal? (eqv? (cons 1 2) (cons 1 2)) #f)
                  (equal? (eqv? (lambda () 1) (lambda () 2)) #f)
                  (equal? (eqv? #f 'nil) #f)
                  (let ((p (lambda (x) x)))
                    (eqv? p p))
                  (let ((g ((lambda ()
                              (let ((n 0))
                                (lambda () (set! n (+ n 1)) n))))))
                    (eqv? g g))
                  (equal? (eqv? ((lambda ()
                                   (let ((n 0))
                                     (lambda () (set! n (+ n 1)) n))))
                                ((lambda ()
                                   (let ((n 0))
                                     (lambda () (set! n (+ n 1)) n)))))
                          #f)
                  (equal? (letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
                                   (g (lambda () (if (eqv? f g) 'g 'both))))
                            (eqv? f g))
                          #f)
                  (equal? (procedure-arity eqv?) 2)))
       
       (list "eq?"
             (and (equal? (eq? 'a 'a)               #t)
                  (equal? (eq? (list 'a) (list 'a)) #f)
                  (equal? (eq? '() '())             #t)
                  (equal? (eq? car car)             #t)
                  (let ((x '(a))) (equal? (eq? x x) #t))
                  (let ((x '#())) (equal? (eq? x x) #t))
                  (let ((x (lambda (x) x)))
                    (equal? (eq? x x) #t))
                  (equal? (procedure-arity eq?) 2)))

       (list "equal?"
             (and
              ;; true cases
              (equal? (equal? 'a 'a) #t)
              (equal? (equal? '("a") '("a")) #t)
              (equal? (equal? '(a) '(a)) #t)
              (equal? (equal? '(a (b) c) '(a (b) c)) #t)
              (equal? (equal? '("a" ("b") "c") '("a" ("b") "c")) #t)
              (equal? (equal? "abc" "abc") #t)
              (equal? (equal? 2 2) #t)
              (equal? (equal? (make-vector 5 'a) (make-vector 5 'a)) #t)
              (equal? (equal? (box "a") (box "a")) #t)

              ;; TODO: flvector, fxvector and stencil-vector are not implemented yet
              ;; (equal? (equal? (make-flvector 5 0.0) (make-flvector 5 0.0)) #t)
              ;; (equal? (equal? (make-fxvector 5 0) (make-fxvector 5 0)) #t)
              ;; (equal? (equal? (stencil-vector #b10010 'a 'b)
              ;;                 (stencil-vector #b10010 'a 'b)) #t)
              ;; (eq? (equal-hash-code (make-flvector 5 0.0))
              ;;      (equal-hash-code (make-flvector 5 0.0)))
              ;; (eq? (equal-hash-code (make-fxvector 5 0))
              ;;      (equal-hash-code (make-fxvector 5 0)))
              ;; (eq? (equal-hash-code (stencil-vector #b10010 'a 'b))
              ;;      (equal-hash-code (stencil-vector #b10010 'a 'b)))

              ;; false cases
              (equal? (equal? "" (string #\null)) #f)
              (equal? (equal? 'a "a") #f)
              (equal? (equal? 'a 'b) #f)
              (equal? (equal? '(a) '(b)) #f)
              (equal? (equal? '(a (b) d) '(a (b) c)) #f)
              (equal? (equal? '(a (b) c) '(d (b) c)) #f)
              (equal? (equal? '(a (b) c) '(a (d) c)) #f)
              (equal? (equal? "abc" "abcd") #f)
              (equal? (equal? "abcd" "abc") #f)
              (equal? (equal? 2 3) #f)
              (equal? (equal? 2.0 2) #f)
              (equal? (equal? (make-vector 5 'b) (make-vector 5 'a)) #f)
              (equal? (equal? (box "a") (box "b")) #f)

              ;; characters
              (equal? (equal? #\a #\a)                                   #t)
              (equal? (equal? (integer->char 1024) (integer->char 1024)) #t)
              (equal? (equal? (integer->char 1024) (integer->char 1025)) #f)

              ;; arity
              (equal? (procedure-arity equal?) 2))))
 
 (list "4.2 Booleans"
       (list
        (list "not"
              (and (equal? (not #t)       #f)
                   (equal? (not 3)        #f)
                   (equal? (not (list 3)) #f)
                   (equal? (not #f)       #t)
                   (equal? (not '())      #f)
                   (equal? (not (list))   #f)
                   (equal? (not 'nil)     #f)
                   (equal? (procedure-arity not) 1)))

        (list "boolean?"
              (and (equal? (boolean? #f)  #t)
                   (equal? (boolean? #t)  #t)
                   (equal? (boolean? 0)   #f)
                   (equal? (boolean? '()) #f)
                   (equal? (procedure-arity boolean?) 1)))
        (list "immutable?"
              (and  (equal? (immutable? "a") #t)
                    (equal? (immutable? (string-copy "a")) #f)
                    (equal? (immutable? '#(1 2)) #t)
                    (equal? (immutable? (make-vector 2 0)) #f)
                    (equal? (immutable? (bytes 1 2)) #f)
                    (equal? (immutable? (make-bytes 2 0)) #f)
                    (equal? (immutable? (make-hasheq)) #f)
                    (equal? (immutable? (box 5)) #f)))))
 
 (list "4.3 Numbers"
       (list "4.3.1 Number Types"
             (list
              (list "number?"
                    (and (equal? (number? 1)   #t)
                         (equal? (number? 1.5) #t)
                         (equal? (number? 'a)  #f)
                         (equal? (number? #f)  #f)
                         (equal? (procedure-arity number?) 1)))
              (list "number->string"
                    (and  (equal? (number->string 42)     "42")
                          (equal? (number->string 16 16)  "10")
                          (equal? (number->string 1.25)   "1.25")
                          (equal? (number->string 1.0)    "1.0")
                          (equal? (number->string -42)    "-42")
                          (equal? (number->string -16 16) "-10")
                          (equal? (number->string -1.25)  "-1.25")
                          (equal? (number->string -1.0)   "-1.0")
                          (equal? (number->string 0.0001) "0.0001")))
              (list "string->number"
                    (and (equal? (string->number "42")     42)
                          (equal? (string->number "111" 7)  57)
                          (equal? (string->number "-42")    -42)
                          (equal? (string->number "-111" 7) -57)                         
                          (equal? (string->number "-2.3")   -2.3)                         
                          (equal? (string->number "hello") #f)))
              (list "inexact?"
                    (and (equal? (inexact? 1.0) #t)
                         (equal? (inexact? 1)   #f)
                         (equal? (procedure-arity inexact?) 1)))
              (list "inexact->exact"
                    (and (equal? (inexact->exact 1)   1)
                         (equal? (inexact->exact 1.0) 1)))
              (list "exact->inexact"
                    (and (equal? (exact->inexact 1)   1.0)
                         (equal? (exact->inexact 1.0) 1.0)))
              (list "nan?"
                    (and (equal? (nan? +nan.0) #t)
                         (equal? (nan? 0.0)   #f)))
              (list "infinite?"
                    (and (equal? (infinite? +inf.0) #t)
                         (equal? (infinite? -inf.0) #t)
                         (equal? (infinite? 0.0)    #f)))
              (list "positive-integer?"
                    (and (equal? (positive-integer? 1)   #t)
                         (equal? (positive-integer? 1.0) #t)
                         (equal? (positive-integer? 1.5) #f)
                         (equal? (positive-integer? -1)  #f)))
              (list "negative-integer?"
                    (and (equal? (negative-integer? -1)   #t)
                         (equal? (negative-integer? -1.0) #t)
                         (equal? (negative-integer? -1.5) #f)
                         (equal? (negative-integer? 1)    #f)))
              (list "nonpositive-integer?"
                    (and (equal? (nonpositive-integer? -1) #t)
                         (equal? (nonpositive-integer? 0)  #t)
                         (equal? (nonpositive-integer? 1)  #f)))
              (list "nonnegative-integer?"
                    (and (equal? (nonnegative-integer? 0)  #t)
                         (equal? (nonnegative-integer? 1)  #t)
                         (equal? (nonnegative-integer? -1) #f)))
              (list "natural?"
                    (and (equal? (natural? 0)   #t)
                         (equal? (natural? 1)   #t)
                         (equal? (natural? 1.0) #f)
                         (equal? (natural? -1)  #f)))))

       (list "4.3.2.1 Arithmetic"
             (list
              (list "+"
                    (and (equal? (+) 0)
                         (equal? (+ 1) 1)
                         (equal? (+ 1  2)  3)
                         (equal? (+ 1. 2)  3.)
                         (equal? (+ 1  2.) 3.)
                         (equal? (+ 1. 2.) 3.)))
              (list "-"
                    (and (equal? (- 1)      -1)
                         (equal? (- 5 3.0)  2.0)
                         (equal? (- 5 2)    3)
                         (equal? (- 5. 2)   3.)
                         (equal? (- 5. 2.)  3.)
                         (equal? (- 5 2 1)  2)))
              (list "*"
                    (and (equal? (*)        1)
                         (equal? (* 2)      2)
                         (equal? (* 2  3)   6)
                         (equal? (* 2  3 4) 24)
                         (equal? (* 2. 3)   6.)
                         (equal? (* 2  3.)  6.)
                         (equal? (* 2. 3.)  6.)
                         (equal? (* 0 8.0)  0)))
              (list "/"
                    (and (equal? (/ 10  2)  5)
                         (equal? (/ 10. 2)  5.)
                         (equal? (/ 10  2.) 5.)
                         (equal? (/ 10. 2.) 5.)))
              (list "quotient"
                    (list (equal? (quotient 10 3) 3)
                         (equal? (quotient -10.0 3) -3.0)
                         (equal? (quotient 10.0 -3) -3.0)
                         (equal? (quotient -10 -3) 3)))
             (list "remainder"
                   (and (equal? (remainder 10 3) 1)
                        (equal? (remainder -10.0 3) -1.0)
                        (equal? (remainder 10.0 -3) 1.0)
                         (equal? (remainder -10 -3) -1)))
             (list "modulo"
                   (and (equal? (modulo 10 3) 1)
                        (equal? (modulo -10.0 3) 2.0)
                        (equal? (modulo 10.0 -3) -2.0)
                        (equal? (modulo -10 -3) -1)))
             (list "quotient/remainder"
                   (and (let-values ([(q r) (quotient/remainder 10 3)])
                          (and (equal? q 3) (equal? r 1)))
                        (let-values ([(q r) (quotient/remainder -10.0 3)])
                          (and (equal? q -3.0) (equal? r -1.0)))
                        (let-values ([(q r) (quotient/remainder 10.0 -3)])
                          (and (equal? q -3.0) (equal? r 1.0)))))))

       (list "4.3.2.2 Number Comparison"
             (list
              (list)))
       
       (list "4.3.2 Generic Numerics"
             (list
             (list "abs"
                   (and (equal? (abs 1) 1)
                        (equal? (abs -1) 1)
                        (equal? (abs 1.0) 1.0)))
             (list "sgn"
                   (and (equal? (sgn 10) 1)
                        (equal? (sgn -10) -1)
                        (equal? (sgn 10.0) 1.0)
                        (equal? (sgn -10.0) -1.0)
                        (eqv? (sgn 0.0) 0.0)
                        (eqv? (sgn -0.0) -0.0)
                        (nan? (sgn +nan.0))))
            (list "max"
                  (and (equal? (max 1 3 2) 3)
                       (equal? (max 1 3 2.0) 3.0)
                       (eqv? (max -0.0 0.0) 0.0)
                       (nan? (max 1.0 +nan.0))))
            (list "min"
                  (and (equal? (min 1 3 2) 1)
                       (equal? (min 1 3 2.0) 1.0)
                       (eqv? (min -0.0 0.0) -0.0)
                       (nan? (min 1.0 +nan.0))))
            (list "round"
                  (and (equal? (round 1.2) 1.)
                       (equal? (round 2.5) 2.)))
              (list "floor"
                    (and (equal? (floor 1.2) 1.)
                         (equal? (floor -1.2) -2.)))
              (list "ceiling"
                    (and (equal? (ceiling 1.2) 2.)
                         (equal? (ceiling -1.2) -1.)))
              (list "truncate"
                    (and (equal? (truncate 1.8) 1.)
                         (equal? (truncate -1.8) -1.)))
              (list "exact-round"
                    (and (equal? (exact-round 1.2) 1)
                         (equal? (exact-round 2.5) 2)))
              (list "exact-floor"
                    (and (equal? (exact-floor 1.2) 1)
                         (equal? (exact-floor -1.2) -2)))
              (list "exact-ceiling"
                    (and (equal? (exact-ceiling 1.2) 2)
                         (equal? (exact-ceiling -1.2) -1)))
              (list "exact-truncate"
                    (and (equal? (exact-truncate 1.8) 1)
                         (equal? (exact-truncate -1.8) -1)))
              (list "even?"
                    (and (equal? (even? 10) #t)
                         (equal? (even? 11) #f)
                         (equal? (even? 10.0) #t)))
              (list "odd?"
                    (and (equal? (odd? 10) #f)
                         (equal? (odd? 11) #t)
                         (equal? (odd? 10.0) #f)))
              (list "gcd"
                    (and (equal? (gcd) 0)
                         (equal? (gcd 10)  10)
                         (equal? (gcd 10.) 10.)
                         (equal? (gcd 10  15)  5)
                         (equal? (gcd 10. 15)  5.)
                         (equal? (gcd 10  15.) 5.)
                         (equal? (gcd 10. 15.) 5.)
                         (equal? (gcd 10 20 6) 2)
                         (equal? (gcd 12 81.0) 3.0)))
              (list "lcm"
                    (list (equal? (lcm) 1)            
                          (equal? (lcm 10)  10)       
                          (equal? (lcm 10.) 10.)      
                          (equal? (lcm 10)  10)       
                          (equal? (lcm 3  4)  12)
                          (equal? (lcm 3  4.) 12.0)
                          (equal? (lcm 3. 4)  12.0)
                          (equal? (lcm 3. 4.) 12.0)
                          (equal? (lcm 10  3 4)  60)
                          (equal? (lcm 10  3 4.) 60.)
                          (equal? (lcm 10. 3 4)  60.)))
              ))

       (list "4.3.2.3 Powers and Roots"
            (list
             (list "sqr"
                   (and (equal? (sqr 5) 25)
                        (equal? (sqr 2.0) 4.0)))
             (list "sqrt"
                   (and (equal? (sqrt 9) 3)
                        (< (abs (- (sqrt 2.0) 1.4142135623730951)) 1e-12)))
             (list "integer-sqrt"
                   (and (equal? (integer-sqrt 9) 3)
                        (equal? (integer-sqrt 9.0) 3.0)))
             (list "integer-sqrt/remainder"
                   (and (let-values ([(s r) (integer-sqrt/remainder 9)])
                          (and (= s 3) (= r 0)))
                        (let-values ([(s r) (integer-sqrt/remainder 15.0)])
                          (and (= s 3.0) (= r 6.0)))))
             (list "expt"
                   (and (equal? (expt 2 5) 32)
                        (equal? (expt 9.0 0.5) 3.0)))
             (list "exp"
                   (and (equal? (exp 0) 1)
                        (< (abs (- (exp 1.0) 2.718281828459045)) 1e-12)))
             (list "log"
                   (and (equal? (log 1) 0)
                        (equal? (log 8 2) 3.)
                        (< (abs (- (log (exp 1.0)) 1.0)) 1e-12)
                        (< (abs (- (log 10.0) 2.302585092994046)) 1e-12)))))

       (list "4.3.2.4 Trigonometric Functions"
             (list
              (list "sin"
                    (and (equal? (sin 0) 0)
                         (equal? (sin 0.) 0.)))
              (list "cos"
                    (and (equal? (cos 0) 1)
                         (equal? (cos 0.) 1.)))
              (list "tan"
                    (and (equal? (tan 0) 0)
                         (equal? (tan 0.) 0.)))
              (list "asin"
                    (and (equal? (asin 0) 0)
                         (equal? (asin 0.) 0.)))
              (list "acos"
                    (and (equal? (acos 1) 0)
                         (equal? (acos 1.) 0.)))
              (list "atan"
                    (and (equal? (atan 0) 0)
                         (equal? (atan 0.) 0.)))
              (list "sinh"
                    (and (equal? (sinh 0) 0)
                         (equal? (sinh 0.) 0.)))
              (list "cosh"
                    (and (equal? (cosh 0) 1)
                         (equal? (cosh 0.) 1.)))
              (list "tanh"
                    (and (equal? (tanh 0) 0)
                         (equal? (tanh 0.) 0.)))
              (list "asinh"
                    (and (equal? (asinh 0) 0)
                         (equal? (asinh 0.) 0.)))
              (list "acosh"
                    (and (equal? (acosh 1) 0)
                         (equal? (acosh 1.) 0.)))
              (list "atanh"
                    (and (equal? (atanh 0) 0)
                         (equal? (atanh 0.) 0.)))))

       (list "4.3.2.6 Bitwise Operations"
             (list
              (list "bitwise-ior"
                    (and (equal? (bitwise-ior 1 2) 3)
                         (equal? (bitwise-ior -32 1) -31)))
              (list "bitwise-and"
                    (and (equal? (bitwise-and 1 2) 0)
                         (equal? (bitwise-and -32 -1) -32)))
              (list "bitwise-xor"
                    (and (equal? (bitwise-xor 1 5) 4)
                         (equal? (bitwise-xor -32 -1) 31)))
              (list "bitwise-not"
                    (and (equal? (bitwise-not 5) -6)
                         (equal? (bitwise-not -1) 0)))
              (list "bitwise-bit-set?"
                    (and (bitwise-bit-set? 5 0)
                         (bitwise-bit-set? 5 2)
                         (bitwise-bit-set? -5 20)))
              (list "bitwise-first-bit-set"  ; added in Racket 8.16
                    (and (equal? (bitwise-first-bit-set 128) 7)
                         (equal? (bitwise-first-bit-set -8) 3)))
              (list "integer-length"
                    (and (equal? (integer-length 8) 4)
                         (equal? (integer-length -8) 3)
                         (equal? (integer-length 0) 0)))))

        (list "4.3.2.7 Random Numbers"
              ;; Basic sanity checks for the `random` primitive.  These
              ;; tests intentionally avoid relying on a particular
              ;; sequence and instead just validate the ranges and
              ;; result types.
              (list
               (list "random"
                     (let ([v (random)])
                       (and (inexact? v) (> v 0.0) (< v 1.0))))
               (list "random k"
                     (let ([v (random 5)])
                       (and (exact-integer? v) (<= 0 v) (< v 5))))
               (list "random k edge" (= (random 1) 0))
               (list "random min max"
                     (let ([v (random -5 5)])
                       (and (exact-integer? v) (<= -5 v) (< v 5))))))

       (list "4.3.2.10 Extra Constants and Functions"
      (list
       (list "degrees->radians"
             (< (abs (- (degrees->radians 180) 3.141592653589793)) 1e-12))
       (list "radians->degrees"
             (let ([pi 3.141592653589793])
               (and (< (abs (- (radians->degrees pi) 180.0)) 1e-12)
                    (< (abs (- (radians->degrees (* 0.25 pi)) 45.0)) 1e-12))))
       (list "order-of-magnitude"
             (and (= (order-of-magnitude 999) 2)
                  (= (order-of-magnitude 1000) 3)
                  (= (order-of-magnitude 0.01) -2)
                  (= (order-of-magnitude 0.009) -3)))))

       (list "4.3.3 Flonums"
             (list
              (list "flonum?"
                    (and (equal? (flonum? 1.0) #t)
                         (equal? (flonum? 1) #f)
                         (equal? (procedure-arity flonum?) 1)))
              (list "fl+"
                    (and (fl= (fl+ 1.0 2.0) 3.0)
                         (fl= (fl+ 1.0 2.0 3.0) 6.0)))
              (list "fl-"
                    (and (fl= (fl- 3.0 1.0) 2.0)
                         (fl= (fl- 3.0) -3.0)))
              (list "fl*"
                    (and (fl= (fl* 2.0 3.0) 6.0)
                         (fl= (fl* 2.0 3.0 4.0) 24.0)))
              (list "fl/"
                    (and (fl= (fl/ 6.0 2.0) 3.0)
                         (fl= (fl* (fl/ 6.0) 6.0) 1.0)))
              (list "fl="
                    (and (equal? (fl= 1.0 1.0) #t)
                         (equal? (fl= 1.0 2.0) #f)))
              (list "fl<"
                    (and (equal? (fl< 1.0 2.0) #t)
                         (equal? (fl< 2.0 1.0) #f)))
              (list "fl>"
                    (and (equal? (fl> 2.0 1.0) #t)
                         (equal? (fl> 1.0 2.0) #f)))
              (list "fl<="
                    (and (equal? (fl<= 1.0 2.0) #t)
                         (equal? (fl<= 2.0 2.0) #t)
                         (equal? (fl<= 2.0 1.0) #f)))
              (list "fl>="
                    (and (equal? (fl>= 2.0 1.0) #t)
                         (equal? (fl>= 2.0 2.0) #t)
                         (equal? (fl>= 1.0 2.0) #f)))
              (list "flabs"
                    (and (fl= (flabs -2.5) 2.5)
                         (fl= (flabs 2.5) 2.5)))
              (list "flround"
                    (and (fl= (flround 1.2) 1.0)
                         (fl= (flround 1.6) 2.0)))
              (list "flfloor"
                    (and (fl= (flfloor 1.2) 1.0)
                         (fl= (flfloor -1.2) -2.0)))
              (list "flceiling"
                    (and (fl= (flceiling 1.2) 2.0)
                         (fl= (flceiling -1.2) -1.0)))
              (list "fltruncate"
                    (and (fl= (fltruncate 1.8) 1.0)
                         (fl= (fltruncate -1.8) -1.0)))
              (list "flsingle"
                    (and (flonum? (flsingle 1.0))
                         (fl= (flsingle 1.5) 1.5)))
              (list "flsin"
                    (fl= (flsin 0.0) 0.0))
              (list "flcos"
                    (fl= (flcos 0.0) 1.0))
              (list "fltan"
                    (fl= (fltan 0.0) 0.0))
              (list "flasin"
                    (fl= (flasin 0.0) 0.0))
              (list "flacos"
                    (fl= (flacos 1.0) 0.0))
              (list "flatan"
                    (fl= (flatan 0.0) 0.0))
              (list "fllog"
                    (fl= (fllog 1.0) 0.0))
              (list "flexp"
                    (fl= (flexp 0.0) 1.0))
              (list "flsqrt"
                    (fl= (flsqrt 9.0) 3.0))
              
              (list "flmin" 
                    (and (fl= (flmin 3.0 1.0 2.0) 1.0)
                         (fl= (flmin 3.0)         3.0)))
              (list "flmax" ; 
                    (and (fl= (flmax 3.0 1.0 2.0) 3.0)
                         (fl= (flmax 3.0)         3.0)))
              (list "flexpt"
                    (fl= (flexpt 2.0 3.0) 8.0))
              ))
       
       (list "4.3.4 Fixnums"
             (list
              (list "fixnum?"
                    (and (equal? (fixnum? 1) #t)
                         (equal? (fixnum? 1.0) #f)))
              (list "fxzero?"
                    (and (equal? (fxzero? 0) #t)
                         (equal? (fxzero? 1) #f)))

              (list "fx+"
                    (equal? (fx+ 1 2 3) 6))
              (list "fx-"
                    (equal? (fx- 5 1 1) 3))
              (list "fx*"
                    (equal? (fx* 2 3 4) 24))

              (list "fx="
                    (and (equal? (fx= 1)     #t)
                         (equal? (fx= 1 1 1) #t)
                         (equal? (fx= 1 2)   #f)))
              (list "fx>"
                    (and (equal? (fx> 3)     #t)
                         (equal? (fx> 3 2 1) #t)
                         (equal? (fx> 2 2)   #f)))
              (list "fx<"
                    (and (equal? (fx< 1)     #t)
                         (equal? (fx< 1 2 3) #t)
                         (equal? (fx< 2 2)   #f)))
              (list "fx<="
                    (and (equal? (fx<= 1)     #t)
                         (equal? (fx<= 1 1 2) #t)
                         (equal? (fx<= 2 1)   #f)))
              (list "fx>="
                    (and (equal? (fx>= 2)     #t)
                         (equal? (fx>= 2 2 1) #t)
                         (equal? (fx>= 1 2)   #f)))
              
              (list "fxquotient"
                    (equal? (fxquotient 13 4) 3))
              (list "unsafe-fxquotient"
                    (equal? (unsafe-fxquotient 13 4) 3))
              (list "fxremainder"
                    (equal? (fxremainder 13 4) 1))
              (list "fxmodulo"
                    (equal? (fxmodulo 13 4) 1))
              
              (list "fxabs"
                    (equal? (fxabs -5) 5))
              (list "fxand"
                    (equal? (fxand 12 10) 8))
              (list "fxior"
                    (equal? (fxior 12 5) 13))
              (list "fxxor"
                    (equal? (fxxor 12 5) 9))
              (list "fxnot"
                    (equal? (fxnot 0) -1))
              (list "fxlshift"
                    (equal? (fxlshift 1 2) 4))
              (list "fxrshift"
                    (and (equal? (fxrshift 4 1) 2)
                         (equal? (fxrshift -4 1) -2)))
              (list "fxpopcount"
                    (equal? (fxpopcount 7) 3))
              (list "fxpopcount32"
                    (equal? (fxpopcount32 7) 3))
              (list "fxpopcount16"
                    (equal? (fxpopcount16 7) 3))
              (list "fx+/wraparound"
                    (equal? (fx+/wraparound 1 2) 3))
              (list "fx-/wraparound"
                    (and (equal? (fx-/wraparound 10 3) 7)
                         (equal? (fx-/wraparound 3) -3)))
              (list "fx*/wraparound"
                    (equal? (fx*/wraparound 2 3) 6))
              (list "fxlshift/wraparound"
                    (equal? (fxlshift/wraparound 1 2) 4))
              (list "fxrshift/logical"
                    (and (equal? (fxrshift/logical 4 1) 2)
                         (equal? (fxrshift/logical -1 1) (most-positive-fixnum))))

              (list "fxmin"
                    (and (equal? (fxmin 3 1 2) 1)
                         (equal? (fxmin 3) 3)))
              (list "fxmax"
                    (and (equal? (fxmax 3 1 2) 3)
                         (equal? (fxmax 3) 3)))

              (list "fx->fl"
                    (equal? (fx->fl 3) 3.0))
              (list "->fl"
                    (equal? (->fl 3) 3.0))
              (list "fl->fx"
                    (equal? (fl->fx 3.0) 3)
                    (equal? (fl->fx 3.1) 3)
                    (equal? (fl->fx 3.9) 3)
                    (equal? (fl->fx -0.1) 0))
              (list "fl->exact-integer"
                    (equal? (fl->exact-integer 3.0) 3))
              )))

 (list "4.4 Strings"
       (list
        (list "string?"
              (and (equal? (string? "hello") #t)
                   (equal? (string? "") #t)
                   (equal? (string? 5) #f)
                   (equal? (procedure-arity string?) 1)))

        (list "make-string"
                (and #;(equal? (string-length (make-string 3)) 3)
                     (equal? (make-string 0) "")
                     #;(equal? (procedure-arity make-string) '(1 2)) ; todo - improve arities
                     ))

        (list "string-set!"
              (let ([f (make-string 3 #\*)])
                (and (equal? (begin (string-set! f 0 #\?) f) "?**")
                     (equal? (procedure-arity string-set!) 3))))

        (list "string-length"
              (and (equal? (string-length "abc") 3)
                   (equal? (string-length "") 0)
                   (equal? (procedure-arity string-length) 1)))

        (list "string-ref"
              (and (equal? (string-ref "abc" 0) #\a)
                   (equal? (string-ref "abc" 2) #\c)
                   (equal? (procedure-arity string-ref) 2)))

        (list "substring"
              (and (equal? (substring "ab" 0 1) "a")
                   (equal? (substring "ab" 1 2) "b")
                   (equal? (substring "ab" 1)   "b")
                   (equal? (substring "ab" 2)   "")
                   #;(equal? (procedure-arity substring) '(2 3)))) ; todo - improve arities

        (list "string-append"
              (and (equal? (string-append) "")
                   (equal? (string-append "A") "A")
                   (equal? (string-append "A" "B") "AB")
                   (equal? (string-append "A" "B" "C") "ABC")
                   #; (equal? (procedure-arity string-append) (arity-at-least 0)) ; todo
                   ))

        (list "string-append-immutable"
              (and (equal? (string-append-immutable "foo" "bar") "foobar")
                   (equal? (string-append-immutable) "")
                   (equal? (immutable? (string-append-immutable "foo")) #t)))

        (list "string-copy"
              (let* ([s (string-copy "hello")]
                     [s2 (string-copy s)])
                (string-set! s 0 #\H)
                (and (equal? s2 "hello")
                     (equal? (procedure-arity string-copy) 1))))

        (list "string-fill!"
              (let ([s (string-copy "hello")])
                (string-fill! s #\x)
                (and (equal? s "xxxxx")
                     (equal? (procedure-arity string-fill!) 2))))

        (list "string-copy!"
              (let ([s (make-string 10 #\x)])
                (string-copy! s 0 "hello")
                (list (equal? s "helloxxxxx")
                      #;(list (procedure-arity string-copy!) '(3 4 5))))) ; todo - got #f - improve arities

        (list "string->immutable-string"
              (and (equal? (string->immutable-string "hi") "hi")
                   (equal? (immutable? (string->immutable-string "hi")) #t)
                   (equal? (procedure-arity string->immutable-string) 1)))

        (list "string=?"
              '(todo 'string=? "make it variadic")
              (and #;(equal? (string=? "") #t)     ; todo - make string=? variadic
                   (equal? (string=? "A" "A") #t)
                   (equal? (string=? "A" "B") #f)
                   (equal? (string=? "A" "AB") #f)))

        (list "string<?"
              (and (equal? (string<? "" "") #f)
                   (equal? (string<? "A" "B") #t)
                   (equal? (string<? "AB" "A") #f)))

        (list "string>?"
              (and (equal? (string>? "" "") #f)
                   (equal? (string>? "B" "A") #t)
                   (equal? (string>? "A" "AB") #f)))

        (list "string<=?"
              (and (equal? (string<=? "" "") #t)
                   (equal? (string<=? "A" "B") #t)
                   (equal? (string<=? "AB" "A") #f)))

        (list "string>=?"
              (and (equal? (string>=? "" "") #t)
                   (equal? (string>=? "B" "A") #t)
                   (equal? (string>=? "A" "AB") #f)))

        #;(list "string-ci=?"
              (and (equal? (string-ci=? "A" "a") #t)
                   (equal? (string-ci=? "A" "B") #f)
                   (equal? (string-ci=? "A" "AB") #f)))

        (list "string->list"
              (and (equal? (string->list "P l") '(#\P #\space #\l))
                   (equal? (string->list "") '())))

        (list "list->string"
              (and (equal? (list->string '(#\1 #\\ #\")) "1\\\"")
                   (equal? (list->string '()) "")))
        
        ))

 (list "4.5 Byte Strings"
       (list
        (list "bytes->immutable-bytes"
              (let* ([m (bytes 65 66 67)]
                     [i (bytes->immutable-bytes m)]
                     [i2 (bytes->immutable-bytes i)])
                (and (equal? i m)
                     (not (eq? i m))
                     (eq? i2 i))))

        (list "byte?"
              (and (equal? (byte? 10)  #t)
                   (equal? (byte? 0)   #t)
                   (equal? (byte? 255) #t)
                   (equal? (byte? 256) #f)
                   (equal? (byte? -1)  #f)
                   (equal? (byte? #\newline) #f)))

        (list "bytes?"
              (and (equal? (bytes? #"hello") #t)
                   (equal? (bytes? #"") #t)
                   (equal? (procedure-arity bytes?) 1)))

        (list "make-bytes"
              (and (equal? (bytes-length (make-bytes 3)) 3)
                   (equal? (make-bytes 3 (char->integer #\*)) #"***")
                   (equal? (make-bytes 0) #"")))

        (list "bytes-set!"
              (and (let ([f (make-bytes 3 (char->integer #\*))])
                     (bytes-set! f 0 (char->integer #\?))
                     (equal? f #"?**"))))

        (list "bytes"
              (and (equal? (bytes 97 98 99) #"abc")
                   (equal? (bytes) #"")))

        (list "bytes-length"
              (and (equal? (bytes-length #"abc") 3)
                   (equal? (bytes-length #"") 0)))

        (list "bytes-ref"
              (and (equal? (bytes-ref #"abc" 0) 97)
                   (equal? (bytes-ref #"abc" 2) 99)))

        (list "subbytes"
              (let ([b (bytes 32 97 0 98 45)])
                (and (equal? (subbytes #"ab" 0 0) #"")
                     (equal? (subbytes #"ab" 1 2) #"b")
                     (equal? (subbytes #"ab" 0 2) #"ab")
                     (equal? (subbytes #"ab" 0)   #"ab")
                     (equal? (subbytes #"ab" 1)   #"b")
                     (equal? (subbytes #"ab" 2)   #"")
                     (equal? (subbytes b 1 4) (bytes 97 0 98)))))

        (list "bytes-append"
              (and (equal? (bytes-append #"foo" #"bar") #"foobar")
                   (equal? (bytes-append #"foo") #"foo")
                   (equal? (bytes-append #"foo" #"") #"foo")
                   (equal? (bytes-append #"foo" #"" #"goo") #"foogoo")
                   (equal? (bytes-append #"" #"foo") #"foo")
                   (equal? (bytes-append) #"")
                   (equal? (bytes-append (bytes 97 0 98) (bytes 99 0 100))
                           (bytes 97 0 98 99 0 100))))

        (list "bytes-copy"
              (let* ([s (bytes-copy #"hello")]
                     [s2 (bytes-copy s)])
                (and (equal? s2 #"hello")
                     (begin (bytes-set! s 2 (char->integer #\x)) (equal? s2 #"hello"))
                     (equal? (bytes-copy (bytes 97 0 98)) (bytes 97 0 98)))))

        (list "bytes-fill!"
              (and (let ([s (bytes-copy #"hello")])
                     (bytes-fill! s (char->integer #\x))
                     (equal? s #"xxxxx"))))

        (list "bytes-copy!"
              (let ([bstr (make-bytes 10)])
                (and (eq? (bytes-copy! bstr 1 #"testing" 2 6) (void))
                     (eq? (bytes-copy! bstr 0 #"testing") (void))
                     (equal? bstr (bytes 116 101 115 116 105 110 103 0 0 0)))))

        (list "list->bytes/bytes->list"
              (equal? (memq (list->bytes (bytes->list #"apple"))
                            '(#"apple")) #f))

        (list "bytes=?"
              '(todo "Make bytes=? variadic")
              (and #;(equal? (bytes=? #"a" #"a" #"a") #t)
                   (equal? (bytes=? #"a" #"a") #t)
                   #;(equal? (bytes=? #"a") #t)
                   #;(equal? (bytes=? #"a" #"a" #"c") #f)
                   #;(equal? (bytes=? #"a" #"b" #"c") #f)
                   (equal? (bytes=? #"a" #"b") #f)
                   #;(equal? (bytes=? #"c" #"a" #"a") #f)
                   #;(equal? (bytes=? #"c" #"b" #"a") #f)
                   (equal? (bytes=? #"b" #"a") #f)))
        ))

 (list "4.6 Characters"
       (list
        (list "char?"
              (and (equal? (char? #\a) #t)
                   (equal? (char? #\space) #t)
                   (equal? (char? #\newline) #t)
                   (equal? (char? 7) #f)
                   (equal? (char? #t) #f)
                   (equal? (char? 'x) #f)
                   (equal? (procedure-arity char?) 1)))

        (list "char->integer"
              (and (equal? (char->integer #\.) 46)
                   (equal? (char->integer #\A) 65)
                   (equal? (char->integer #\a) 97)
                   (equal? (char->integer #\space) 32)
                   (equal? (procedure-arity char->integer) 1)))

        (list "integer->char"
              (and (equal? (integer->char 46) #\.)
                   (equal? (integer->char 65) #\A)
                   (equal? (integer->char 97) #\a)
                   (equal? (integer->char 32) #\space)
                   (equal? (procedure-arity integer->char) 1)))

        (list "char=?"
              (and (equal? (char=? #\A #\A) #t)
                   (equal? (char=? #\A #\B) #f)
                   (equal? (char=? #\A #\A #\A) #t)))

        (list "char<?"
              (and (equal? (char<? #\A #\B) #t)
                   (equal? (char<? #\b #\A) #f)))

        (list "char<=?"
              (and (equal? (char<=? #\A #\B) #t)
                   (equal? (char<=? #\b #\A) #f)))

        (list "char>?"
              (and (equal? (char>? #\B #\A) #t)
                   (equal? (char>? #\A #\b) #f)))

        (list "char>=?"
              (and (equal? (char>=? #\B #\A) #t)
                   (equal? (char>=? #\A #\b) #f)))

        (list "char-whitespace?"
              (and (equal? (char-whitespace? #\space) #t)
                   (equal? (char-whitespace? #\tab) #t)
                   (equal? (char-whitespace? #\A) #f)
                   (equal? (procedure-arity char-whitespace?) 1)))
        (list "char-upcase"
              (and (equal? (char-upcase #\a) #\A)
                   (equal? (char-upcase #\u03BB) #\u039B)
                   (equal? (char-upcase #\space) #\space)
                   (equal? (procedure-arity char-upcase) 1)))

        (list "char-downcase"
              (and (equal? (char-downcase #\A) #\a)
                   (equal? (char-downcase #\u039B) #\u03BB)
                   (equal? (char-downcase #\space) #\space)
                   (equal? (procedure-arity char-downcase) 1)))

        (list "char-titlecase"
              (and (equal? (char-titlecase #\a) #\A)
                   (equal? (char-titlecase #\u03BB) #\u039B)
                   (equal? (char-titlecase #\space) #\space)
                   (equal? (procedure-arity char-titlecase) 1)))

        (list "char-foldcase"
              (list (equal? (char-foldcase #\A) #\a)
                    (equal? (char-foldcase #\u03A3) #\u03c3)
                    (equal? (char-foldcase #\u03c2) #\u03c3)
                    (equal? (char-foldcase #\space) #\space)
                    (equal? (procedure-arity char-foldcase) 1)))

        (list "char-ci=?"
              (and (equal? (char-ci=? #\A #\a) #t)
                   (equal? (char-ci=? #\A #\B) #f)))

        (list "char-ci<?"
              (and (equal? (char-ci<? #\A #\b) #t)
                   (equal? (char-ci<? #\b #\A) #f)))

        (list "char-ci<=?"
              (and (equal? (char-ci<=? #\A #\a) #t)
                   (equal? (char-ci<=? #\b #\A) #f)))

        (list "char-ci>?"
              (and (equal? (char-ci>? #\B #\a) #t)
                   (equal? (char-ci>? #\A #\b) #f)))

        (list "char-ci>=?"
              (and (equal? (char-ci>=? #\A #\a) #t)
                   (equal? (char-ci>=? #\a #\B) #f)))))

 (list "4.7 Symbols"
       (list
        (list "symbol?"
              (and (equal? (symbol? 'foo)         #t)
                   (equal? (symbol? (car '(a b))) #t)
                   (equal? (symbol? "bar")        #f)
                   (equal? (symbol? 'nil)         #t)
                   (equal? (symbol? '())          #f)
                   (equal? (symbol? #f)           #f)))

        (let ([x (string #\a #\b)]
              [y (string->symbol (string #\a #\b))])
          ;; mutate x after creating y
          (string-set! x 0 #\c)
          (list "symbol/string interop"
                (and (equal? x "cb")
                     (equal? (symbol->string y) "ab")
                     (equal? (string->symbol "ab") y)
                     ;; error cases
                     #;(with-handlers ([exn:fail? (λ _ #t)])
                         (string->symbol 10) #f)
                     #;(with-handlers ([exn:fail? (λ _ #t)])
                         (string->symbol 'oops) #f)
                     ;; symbol->string returns fresh strings (not eq?)
                     (equal? (eq? (symbol->string 'apple)
                                  (symbol->string 'apple))
                             #f)
                     (equal? (symbol->immutable-string 'apple) "apple")
                     (equal? (immutable? (symbol->immutable-string 'apple)) #t)
                     (equal? (immutable? (symbol->immutable-string 'box))   #t))))

        (let ([a (string->uninterned-symbol "a")]
              [b (string->uninterned-symbol "a")])
          (list "symbol=?"
                (and (equal? (symbol=? 'a 'a) #t)
                     (equal? (symbol=? 'a 'b) #f)
                     (equal? (symbol=? a b)   #f)
                     (equal? (eq? a b)        #f)
                     #;(equal? (symbol=? 'x 'x 'x) #t)    ; todo - make symbol=? variadic
                     #;(equal? (symbol=? 'x 'x 'y) #f))))

        (let ([s1 (string->uninterned-symbol "apple")]
              [s2 (string->uninterned-symbol "apple")])
          (list "string->uninterned-symbol"
                (and (equal? (symbol? s1)          #t)
                     (equal? (eq? s1 s2)           #f)
                     (equal? (symbol=? s1 s2)      #f)
                     (equal? (symbol-interned? s1) #f))))

        (let ([interned 'banana]
              [uninterned (string->uninterned-symbol "banana")])
          (list "symbol-interned?"
                (and (equal? (symbol-interned? interned) #t)
                     (equal? (symbol-interned? uninterned) #f)
                     (equal? (symbol-interned? (string->symbol "bar")) #t))))

        (list "symbol<?"
              (and (equal? (symbol<? 'a 'b)    #t)
                   ;(equal? (symbol<? 'a 'b 'c) #t)  ; todo - make symbol<? variadic
                   ;(equal? (symbol<? 'a 'c 'b) #f)
                   (equal? (symbol<? 'a 'aa)   #t)
                   (equal? (symbol<? 'aa 'a)   #f)
                   #;(equal? (procedure-arity symbol<?) 1)))))

 (list "4.9 Keywords"
       (list
        (list "keyword?"
              (and (equal? (keyword? '#:a) #t)
                   (equal? (keyword? 'a) #f)
                   (equal? (string->keyword "apple") '#:apple)
                   (equal? (keyword->string '#:apple) "apple")
                   ;; keyword->string returns fresh strings (not eq?)
                   #;(equal? (eq? (keyword->string '#:apple)
                                  (keyword->string '#:apple))
                             #f)
                   (equal? (keyword->immutable-string '#:apple) "apple")
                   (equal? (immutable? (keyword->immutable-string '#:apple)) #t)

                   #;(equal? (procedure-arity keyword?) 1)))
        (list "keyword<?"
              (and #;(equal? (keyword<? '#:a) #t)
                   (equal? (keyword<? '#:a '#:b) #t)
                   (equal? (keyword<? '#:b '#:b) #f)
                   (equal? (keyword<? '#:b '#:bb) #t)
                   (equal? (keyword<? '#:b '#:) #f)
                   #;(equal? (keyword<? '#:b '#:c '#:d) #t)
                   #;(equal? (keyword<? '#:b '#:c '#:c) #f)
                   (equal? (keyword<? (string->keyword "a")
                                      (string->keyword "\uA0")) #t)
                   (equal? (keyword<? (string->keyword "a")
                                      (string->keyword "\uFF")) #t)
                   (equal? (keyword<? (string->keyword "\uA0")
                                      (string->keyword "a")) #f)
                   (equal? (keyword<? (string->keyword "\uFF")
                                      (string->keyword "a")) #f)
                   (equal? (keyword<? (string->keyword "\uA0")
                                      (string->keyword "\uFF")) #t)
                   (equal? (keyword<? (string->keyword "\uFF")
                                      (string->keyword "\uA0")) #f)
                   (equal? (keyword<? (string->keyword "\uA0")
                                      (string->keyword "\uA0")) #f)

                   #;(equal? (procedure-arity keyword<?) 2)))))

(list "4.10 Pairs and Lists"
       (list
        (list
         "cons?"
         (and (equal? (cons? '(1 2)) #t)
              (equal? (cons? '())   #f)
              (equal? (procedure-arity cons?) 1)))
        (list
         "empty?"
         (and (equal? (empty? '(1 2)) #f)
              (equal? (empty? '())   #t)
              (equal? (procedure-arity empty?) 1)))
        (list
         "append"
         (and (equal? (append)                         '())
              (equal? (append '(x))                    '(x))
              (equal? (append '(x) '(y))               '(x y))
              (equal? (append '(a) '(b c d))           '(a b c d))
              (equal? (append '(a (b)) '((c)))         '(a (b) (c)))
              (equal? (append '(a b) '(c . d))         '(a b c . d))
              (equal? (append '() 'a)                  'a)
              (equal? (append 1)                       1)
              (equal? (append '(1) 2)                  '(1 . 2))
              (equal? (append '(1) 2)                  '(1 . 2))
              (let ([xs '(x)] [ys '(y)])
                (let ([zs (append xs ys)])
                  (eq? (cdr zs) ys)))
              
              ;; error cases
              #;(with-handlers ([exn:fail? (λ _ #t)])
                  (append '(1 2 . 3) 1) #f)
              #;(with-handlers ([exn:fail? (λ _ #t)])
                  (append '(1 2 3) 1 '(4 5 6)) #f)
              ;; arity: append accepts 0 or more args
              #;(procedure-arity-includes? append 0)
              #;(procedure-arity-includes? append 2)
              #;(procedure-arity-includes? append 3)))


        (list "reverse"
              (and (equal? (reverse '(a b c))             '(c b a))
                   (equal? (reverse '(a (b c) d (e (f)))) '((e (f)) d (b c) a))
                   (equal? (procedure-arity reverse)      1)))

        (list "list-ref"
              (and (equal? (list-ref '(a b c d) 2)    'c)
                   (equal? (list-ref '(a b c . d) 2)  'c)
                   (equal? (procedure-arity list-ref) 2)))

        (list "list-tail"
              (and (equal? (list-tail '(a b c d) 2)    '(c d))
                   (equal? (list-tail '(a b c d) 0)    '(a b c d))
                   (equal? (list-tail '(a b c . d) 1)  '(b c . d))
                   (equal? (list-tail 1 0)             1)
                   (equal? (procedure-arity list-tail) 2)))

        (list "first"
              (and (equal? (first '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) 1)
                   (equal? (procedure-arity first) 1)))
        (list "second"
              (and (equal? (second '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) 2)
                   (equal? (procedure-arity second) 1)))
        (list "third"
              (and (equal? (third '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) 3)
                   (equal? (procedure-arity third) 1)))
        (list "fourth"
              (and (equal? (fourth '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) 4)
                   (equal? (procedure-arity fourth) 1)))
        (list "fifth"
              (and (equal? (fifth '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) 5)
                   (equal? (procedure-arity fifth) 1)))
        (list "sixth"
              (and (equal? (sixth '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) 6)
                   (equal? (procedure-arity sixth) 1)))
        (list "seventh"
              (and (equal? (seventh '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) 7)
                   (equal? (procedure-arity seventh) 1)))
        (list "eighth"
              (and (equal? (eighth '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) 8)
                   (equal? (procedure-arity eighth) 1)))
        (list "ninth"
              (and (equal? (ninth '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) 9)
                   (equal? (procedure-arity ninth) 1)))
        (list "tenth"
              (and (equal? (tenth '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) 10)
                   (equal? (procedure-arity tenth) 1)))
        (list "eleventh"
              (and (equal? (eleventh '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) 11)
                   (equal? (procedure-arity eleventh) 1)))
        (list "twelfth"
              (and (equal? (twelfth '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) 12)
                   (equal? (procedure-arity twelfth) 1)))
        (list "thirteenth"
              (and (equal? (thirteenth '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) 13)
                   (equal? (procedure-arity thirteenth) 1)))
        (list "fourteenth"
              (and (equal? (fourteenth '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) 14)
                   (equal? (procedure-arity fourteenth) 1)))
        (list "fifteenth"
              (and (equal? (fifteenth '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) 15)
                   (equal? (procedure-arity fifteenth) 1)))

        (list "last"
              (and (equal? (last '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) 15)
                   (equal? (procedure-arity last) 1)))
        (list "last-pair"
              (and (equal? (last-pair '(1 2 3 4)) '(4))
                   (equal? (last-pair '(1 2 3 . 4)) '(3 . 4))
                   (equal? (procedure-arity last-pair) 1)))

        (list "list*"
              (and (equal? (list* 1 2 3) (cons 1 (cons 2 3)))
                   (equal? (list* 1 2 (list 3 4)) '(1 2 3 4))))

        (list "filter"
              #;(and (equal? (filter positive? '(1 -2 3 4 -5)) '(1 3 4))
                     (equal? (filter positive? '()) '()))
              (and (equal? (filter (λ (x) (positive? x)) '(1 -2 3 4 -5)) '(1 3 4))
                   (equal? (filter (λ (x) (positive? x)) '()) '())))

        (list "andmap"
              (and (equal? (andmap eq? '(a b c) '(a b c)) #t)
                   (equal? (andmap positive? '(1 2 3)) #t)
                   (equal? (andmap positive? '(1 -2 a)) #f)
                   (equal? (andmap + '(1 2 3) '(4 5 6)) 9)
                   (equal? (andmap positive? '()) #t)))

        (list "ormap"
              (and (equal? (ormap eq? '(a b c) '(a b c)) #t)
                   (equal? (ormap positive? '(1 2 a)) #t)
                   (equal? (ormap positive? '(-1 -2 -3)) #f)
                   (equal? (ormap + '(1 2 3) '(4 5 6)) 5)
                   (equal? (ormap positive? '()) #f)))

        (list "build-list"
              (and (equal? (build-list 10 values)
                           '(0 1 2 3 4 5 6 7 8 9))
                   (equal? (build-list 5 (lambda (x) (* x x)))
                           '(0 1 4 9 16))))

        (list "memq"
              (and (equal? (memq 'a '(a b c))   '(a b c))
                   (equal? (memq 'b '(a b c))   '(b c))
                   (equal? (memq 'b '(a b . c)) '(b . c))
                   (equal? (memq 'a '(b c d))   #f)

                   #;(equal? (memq  "apple" '( "apple"))         '("apple"))   ; todo - intern literals
                   #;(equal? (memq #"apple" '(#"apple"))         '(#"apple"))  ; todo - intern literals

                   (equal? (memq (list->string (string->list "apple"))
                                 '("apple"))
                           #f)                   
                   (equal? (procedure-arity memq) 2)))))

 (list "4.12 Vectors"
       (list
        (list "vector?"
              (equal? (vector? (make-vector 0)) #t))

        (list "make-vector"
              (and (equal? (make-vector 5) '#(0 0 0 0 0))
                   (equal? (make-vector 5 0) '#(0 0 0 0 0))))

        (list "vector"
              (and (equal? (vector 1 2 3) '#(1 2 3))
                   (equal? (vector) '#())))

        (list "vector-ref"
              (let ([v (vector 'a 'b 'c)])
                (and (equal? (vector-ref v 0) 'a)
                     (equal? (vector-ref v 2) 'c))))
        
        (list "vector-copy!"
              (and (let ([b (vector 1 2 3)])
                     (vector-copy! b 0 b 1)
                     (equal? b '#(2 3 3)))
                   (let ([b (vector 2 3 4)])
                     (vector-copy! b 1 b 0 2)
                     (equal? b '#(2 2 3)))))

        (list "vector-set!"
              (let ([v (make-vector 5)])
                (for-each (lambda (i) (vector-set! v i (* i i)))
                          '(0 1 2 3 4))
                (equal? v '#(0 1 4 9 16))))

        (list "vector->list"
              (and (equal? (vector->list '#(foo bar baz)) '(foo bar baz))
                   (equal? (vector->list '#()) '())))

        (list "list->vector"
              (and (equal? (list->vector '(foo bar baz))
                           '#(foo bar baz))
                   (equal? (list->vector '()) '#())))
        
        (list "vector-immutable"
              (let ([v (vector-immutable 5 'a)])
                (and (equal? v '#(5 a))
                     (equal? (immutable? v) #t))))

       (list "vector-copy"
              (and (equal? (vector-copy '#(1 2 3 4))     '#(1 2 3 4))
                   (equal? (vector-copy '#(1 2 3 4) 3)   '#(4))
                   (equal? (vector-copy '#(1 2 3 4) 2 3) '#(3))))


        (list "vector-length"
              (and (equal? (vector-length '#(1 2 3)) 3)
                   (equal? (vector-length '#()) 0)))

        (list "vector-fill!"
              (let ([v (vector 1 2 3)])
                (vector-fill! v 9)
                (equal? v '#(9 9 9))))

        (list "vector-empty?"
              (and (equal? (vector-empty? '#()) #t)
                   (equal? (vector-empty? '#(1)) #f)))

        (list "vector-take"
              (equal? (vector-take '#(a b c d) 2) '#(a b)))

        (list "vector-drop"
              (equal? (vector-drop '#(a b c d) 2) '#(c d)))

        (list "vector-drop-right"
              (and (equal? (vector-drop-right '#(1 2 3 4) 1) '#(1 2 3))
                   (equal? (vector-drop-right '#(1 2 3 4) 3) '#(1))))


        (list "vector-split-at"
              (and (let-values ([(v1 v2) (vector-split-at '#(a b c d) 2)])
                     (and (equal? v1 '#(a b))
                          (equal? v2 '#(c d))))
                   (let-values ([(a b) (vector-split-at '#(1 2 3 4 5) 2)])
                     (and (equal? a '#(1 2))
                          (equal? b '#(3 4 5))))))
        ))

 (list "4.15 Hash Tables"
       (list
        (list "make-empty-hasheq"
              (let ([h (make-empty-hasheq)])
                (and (hash? h)
                     (equal? (hash-has-key? h 'a) #f))))

        (list "hash-set!"
              (let ([h (make-hasheq)])
                (hash-set! h 'a 1)
                (equal? (hash-ref h 'a) 1)))

        (list "hash-ref"
              (let ([h (make-hasheq)])
                (hash-set! h 'a 1)
                (and (equal? (hash-ref h 'a) 1)
                     (equal? (hash-ref h 'b 2) 2)
                     (equal? (hash-ref h 'b (lambda () 3)) 3))))

        (list "hash-remove!"
              (let ([h (make-hasheq)])
                (hash-set! h 'a 1)
                (hash-remove! h 'a)
                (equal? (hash-has-key? h 'a) #f)))

        (list "hash-clear!"
              (let ([h (make-hasheq)])
                (hash-set! h 'a 1)
                (hash-set! h 'b 2)
                (hash-clear! h)
                (and (equal? (hash-has-key? h 'a) #f)
                     (equal? (hash-has-key? h 'b) #f))))

        (list "hash-has-key?"
              (let ([h (make-hasheq)])
                (hash-set! h 'a 1)
                (and (equal? (hash-has-key? h 'a) #t)
                     (equal? (hash-has-key? h 'b) #f))))

        (list "eq-hash-code"
              (and (let ([xs (list 1 2 3)]
                         [ys (list 1 2 4)])                         
                     (and      (eq? (eq-hash-code xs) (eq-hash-code xs))
                          (not (eq? (eq-hash-code xs) (eq-hash-code ys)))))))

        )))

