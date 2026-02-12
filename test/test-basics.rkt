;; The numbering follows the sections in "The Reference".

;; These basic tests needs to work without the standard library.
;; So avoid use --stdlib when compiling this test.

(list
 (list "3. Syntactic Forms"       
       (list "3.8 Procedure Expressions: lambda and case-lambda"
             (list
              (list "lambda"
                    (and (equal? (procedure? (lambda (x) (* x x))) #t)
                         (equal? (procedure? '(lambda (x) (* x x))) #f)
                         (equal? (apply (lambda (a b) (+ a b)) (list 3 4)) 7)
                         (let ([compose (lambda (f g) (lambda args (f (apply g args))))])
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
                     (equal? (procedure-arity procedure?) 1))))))

 (list "4. Datatypes"
       (list "4.1 Equality"
             (list "eqv?"
                   (and (equal? (eqv? 'a 'a) #t)
                        (equal? (eqv? 'a 'b) #f)
                        (equal? (eqv? 2 2) #t)
                        (equal? (eqv? 2 2.0) #f)
                        (equal? (eqv? '() '()) #t)
                        (equal? (eqv? '10000 '10000) #t)
                        (equal? (eqv? 10000000000000000000 10000000000000000000) #t)
                        (equal? (eqv? 10000000000000000000 10000000000000000001) #f) ; todo - needs bignums
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
                    (equal? (procedure-arity equal?) 2)))

             (list "eq-hash-code"
                   (and (let ([xs (list 1 2 3)]
                              [ys (list 1 2 4)])
                          (and      (eq? (eq-hash-code xs) (eq-hash-code xs))
                                    (not (eq? (eq-hash-code xs) (eq-hash-code ys)))))))

             (list "eqv-hash-code"
                   (let ([x1 (fl+ 1.0 0.0)]
                         [x2 (fl+ 0.5 0.5)]
                         [y  (fl+ 2.0 0.0)])
                     (and (eqv? x1 x2)
                          (not (eq? x1 x2))
                          (eq? (eqv-hash-code x1) (eqv-hash-code x2))
                          (not (eq? (eqv-hash-code x1) (eqv-hash-code y))))))
             
             (list "equal-hash-code"
                   (and (fixnum? (equal-hash-code '(1 2)))
                        (eq? (equal-hash-code '(1 2))         (equal-hash-code '(1 2)))
                        (eq? (equal-hash-code (vector 'a 'b)) (equal-hash-code (vector 'a 'b)))
                        (eq? (equal-hash-code (box 1))        (equal-hash-code (box 1)))))

             (list "prop:equal+hash"
                   (let ()
                     (struct fish (size)
                       #:property prop:equal+hash
                       (list
                        (lambda (self other recur)
                          (and (fish? other)
                               (recur (fish-size self) (fish-size other))))
                        (lambda (self hash) (hash (fish-size self)))
                        (lambda (self hash) (hash (fish-size self)))))
                     (define f1 (fish 10))
                     (define f2 (fish 10))
                     (define f3 (fish 11))
                     (struct coral (size)
                       #:property prop:equal+hash
                       (list
                        (lambda (self other recur mode)
                          (and (coral? other)
                               (if mode
                                   #f
                                   (recur (coral-size self) (coral-size other)))))
                        (lambda (self hash mode) (hash (coral-size self)))))
                     (define c1 (coral 5))
                     (define c2 (coral 5))
                     (define c3 (coral 6))
                     (list (equal? f1 f2)
                          (equal? f2 f1)
                          (eq? (equal-hash-code f1) (equal-hash-code f2))
                          (equal? (equal? f1 f3)        #f)
                          (equal? (equal? c1 c2)        #f)
                          (equal? (equal-always? c1 c2) #t)
                          (equal? (equal? c1 c3)        #f)
                          (equal? (equal-always? c1 c3) #f)))))
       
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
              (list "boolean=?"
                    (and (equal? (boolean=? #t #t) #t)
                         (equal? (boolean=? #t #f) #f)
                         (equal? (boolean=? #f #f) #t)
                         (equal? (procedure-arity boolean=?) 2)))
              (list "false?"
                    (and (equal? (false? #f) #t)
                         (equal? (false? #t) #f)
                         (equal? (false? 0)  #f)
                         (equal? (procedure-arity false?) 1)))
              (list "xor"
                    (and (equal? (xor 11 #f) 11)
                         (equal? (xor #f 22) 22)
                         (equal? (xor 11 22) #f)
                         (equal? (xor #f #f) #f)
                         (equal? (procedure-arity xor) 2)))
              (list "immutable?"
                    (and (equal? (immutable? "a") #t)
                         (equal? (immutable? (string-copy "a")) #f)
                         (equal? (immutable? '#(1 2)) #t)            
                         (equal? (immutable? (make-vector 2 0)) #f)
                         (equal? (immutable? (bytes 1 2)) #f)
                         (equal? (immutable? (make-bytes 2 0)) #f)
                         (equal? (immutable? (make-hasheq)) #f)
                         (equal? (immutable? (box 5)) #f)
                         (equal? (immutable? #&1) #t)))
              (list "Mutability Predicates"
                    (and (equal? (immutable-string? "hi") #t)
                         (equal? (mutable-string? (string-copy "hi")) #t)
                         (equal? (mutable-string? "hi") #f)
                         (equal? (immutable-string? (string-copy "hi")) #f)
                         (equal? (mutable-bytes? (bytes 1 2)) #t)
                         (equal? (immutable-bytes? (bytes 1 2)) #f)
                         (equal? (immutable-vector? #(1 2)) #t)
                         (equal? (mutable-vector? (vector 1 2)) #t)
                         (equal? (mutable-vector? #(1 2)) #f)
                         (equal? (immutable-vector? (vector 1 2)) #f)
                         (equal? (mutable-box? (box 1)) #t)
                         (equal? (immutable-box? (box 1)) #f)                    
                         (equal? (mutable-box? 1) #f)
                         (equal? (immutable-box? 1) #f)
                         (equal? (mutable-hash? (make-empty-hasheq)) #t)
                         (equal? (immutable-hash? (make-empty-hasheq)) #f)
                         (equal? (mutable-box? #&1) #f)
                         (equal? (immutable-box? #&1) #t)
                         (equal? (immutable-bytes? #"ab") #t)
                         (equal? (mutable-bytes?   #"ab") #f)))))
       
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
                    (list "real?"
                          (and (equal? (real? 1)      #t)
                               (equal? (real? 1.0)    #t)
                               (equal? (real? +nan.0) #t)
                               (equal? (real? 'a)     #f)
                               (equal? (procedure-arity real?) 1)))
                    (list "inexact-real?"
                          (and (equal? (inexact-real? 1.0)    #t)
                               (equal? (inexact-real? 1)      #f)
                               (equal? (inexact-real? +nan.0) #f)
                               (equal? (procedure-arity inexact-real?) 1)))
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
                    (list "real->double-flonum"
                          (and (equal? (real->double-flonum 1)   1.0)
                               (equal? (real->double-flonum 1.0) 1.0)))
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
                               (equal? (+ 1)     1)
                               (equal? (+ 1.)    1.)
                               (equal? (+ 1  2)  3)
                               (equal? (+ 1. 2)  3.)
                               (equal? (+ 1  2.) 3.)
                               (equal? (+ 1. 2.) 3.)
                               (equal? (+ 1  2  3)  6)
                               (equal? (+ 1. 2. 3.) 6.)
                               (equal? (+ 1  2. 3)  6.)))
                    (list "-"
                          (and (equal? (- 1)        -1)
                               (equal? (- 1.)       -1.)
                               (equal? (- 5  2)      3)
                               (equal? (- 5  2.)     3.)
                               (equal? (- 5. 2)      3.)
                               (equal? (- 5. 2.)     3.)
                               (equal? (- 5  2 1)    2)
                               (equal? (- 5.  2  1)  2.)
                               (equal? (- 5   2. 1)  2.)
                               (equal? (- 5   2  1.) 2.)
                               (equal? (- 4 1.6)     2.4)
                               ))
                    (list "*"
                          (and (equal? (*)          1)
                               (equal? (* 2)        2)
                               (equal? (* 2.)       2.)
                               (equal? (* 2  3)     6)
                               (equal? (* 2. 3)     6.)
                               (equal? (* 2  3.)    6.)
                               (equal? (* 2. 3.)    6.)
                               (equal? (* 2  3  4) 24)
                               (equal? (* 2  3. 4) 24.)
                               (equal? (* 0 8.0)    0)
                               (=      (* 40 0.007599999999999966 -1)
                                       -0.30399999999999866)))
                    (list "/"
                          (and (equal? (/ 2)       0.5)
                               (equal? (/ 2.)      0.5)
                               (equal? (/ 10.0)    0.1)
                               (equal? (/ 10  2)   5)
                               (equal? (/ 10. 2)   5.)
                               (equal? (/ 10  2.)  5.)
                               (equal? (/ 10. 2.)  5.)
                               (equal? (/ 10  2 5) 1)
                               (equal? (/ 81 3 3) 9)
                               (equal? (/ 0 3)    0)))
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
                      (list "="
                            (and (equal? (= 5) #t)
                                 (equal? (= 1 1 1.0) #t)
                                 (equal? (= 1 2) #f)
                                 (equal? (= 1 2 1) #f)))
                      (list "<"
                            (and (equal? (< 10) #t)
                                 (equal? (< 1 2 3) #t)
                                 (equal? (< 1 3 2) #f)
                                 (equal? (< 1 2.0 3.0) #t)))
                      (list ">"
                            (and (equal? (> 7) #t)
                                 (equal? (> 3 2 1) #t)
                                 (equal? (> 3 2 2) #f)
                                 (equal? (> 3.0 2 1) #t)))
                      (list "<="
                            (and (equal? (<= 4) #t)
                                 (equal? (<= 1 2 2) #t)
                                 (equal? (<= 1 3 2) #f)
                                 (equal? (<= 1 1.0 1.0) #t)))
                      (list ">="
                            (and (equal? (>= 9) #t)
                                 (equal? (>= 3 2 2) #t)
                                 (equal? (>= 3 4 5) #f)
                                 (equal? (>= 3.0 3 2) #t)))))
             
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
                               (equal? (expt 9.0 0.5) 3.0)
                               (= (- (expt 2 29)) -536870912)))
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
                    (list "bitwise-bit-field"
                          (and (equal? (bitwise-bit-field 13 1 1) 0)
                               (equal? (bitwise-bit-field 13 1 3) 2)
                               (equal? (bitwise-bit-field 13 1 4) 6)))
                    (list "arithmetic-shift"
                          (and (equal? (arithmetic-shift 1 10) 1024)
                               (equal? (arithmetic-shift 255 -3) 31)
                               (equal? (arithmetic-shift 1 2) 4)))
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

             (list "4.3.2.8 Other Randomness Utilities")

             (list "4.3.2.9 Byte String Conversions"
                   (list
                    (list "real->floating-point-bytes"
                          (and (bytes=? (real->floating-point-bytes 1.0 8 #t)
                                        #"\x3f\xf0\x00\x00\x00\x00\x00\x00")
                               (bytes=? (real->floating-point-bytes 1.0 8 #f)
                                        #"\x00\x00\x00\x00\x00\x00\xf0\x3f")))
                    (list "real->floating-point-bytes default"
                          (bytes=? (real->floating-point-bytes 1.0 8)
                                   #"\x00\x00\x00\x00\x00\x00\xf0\x3f"))
                    (list "real->floating-point-bytes start"
                          (let* ([dest (make-bytes 10)]
                                 [res  (real->floating-point-bytes 1.0 8 #f dest 2)])
                            (and (eq? res dest)
                                 (bytes=? dest
                                          #"\x00\x00\x00\x00\x00\x00\x00\x00\xf0\x3f"))))
                    (list "floating-point-bytes->real"
                          (and (= (floating-point-bytes->real (bytes 0 0 128 63)) 1.0)
                               (= (floating-point-bytes->real (bytes 63 240 0 0 0 0 0 0) #t 0 8) 1.0)))
                    (list "system-big-endian?"
                          (and (equal? (system-big-endian?) #f)
                               (equal? (procedure-arity system-big-endian?) 0)))))

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
                    (list "double-flonum?"
                          (and (equal? (double-flonum? 1.0) #t)
                               (equal? (double-flonum? 1) #f)
                               (equal? (procedure-arity double-flonum?) 1)))
                    (list "single-flonum?"
                          (and (equal? (single-flonum? 1.0) #f)
                               (equal? (single-flonum? 1) #f)
                               (equal? (procedure-arity single-flonum?) 1)))
                    (list "single-flonum-available?"
                          (and (equal? (single-flonum-available?) #f)
                               (equal? (procedure-arity single-flonum-available?) 0)))
                    (list "fl+"
                          (and (fl= (fl+ 1.0 2.0) 3.0)
                               (fl= (fl+ 1.0 2.0 3.0) 6.0)))
                    (list "fl-"
                          (and (fl= (fl- 3.0 1.0) 2.0)
                               (fl= (fl- 3.0)    -3.0)))
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
                    (list "flbit-field"
                          (and (= (flbit-field -0.0 63 64)   1)
                               (= (flbit-field  0.0 63 64)   0)
                               (= (flbit-field  1.0 52 63) 1023)
                               (= (flbit-field  1.5 51 52)   1)))
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
                    (list "flrandom"
                          (let ([v (flrandom)])
                            (and (flonum? v) (> v 0.0) (< v 1.0)))))
                   )
             
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

                    (list "fixnum-for-every-system?"
                          (and (equal? (fixnum-for-every-system? 0)          #t)
                               (equal? (fixnum-for-every-system? -536870912) #t)
                               (equal? (fixnum-for-every-system? 536870911)  #t)
                               ; (equal? (fixnum-for-every-system? 536870912)  #f)  ; no bignums yet
                               (equal? (fixnum-for-every-system? 1.0)        #f)))
                    )))

       (list "4.4 Strings"
             (list
              (list "string?"
                    (and (equal? (string? "hello") #t)
                         (equal? (string? "") #t)
                         (equal? (string? 5) #f)
                         (equal? (procedure-arity string?) 1)))

              (list "make-string"
                    (and (equal? (string-length (make-string 3)) 3)
                         (equal? (make-string 0) "")
                         (equal? (procedure-arity make-string) '(1 2)) ; todo - improve arities
                         ))

              (list "string-set!"
                    (let ([f (make-string 3 #\*)])
                      (and (equal? (begin (string-set! f 0 #\?) f) "?**")
                           (equal? (procedure-arity string-set!) 3)
                           ; string-set! doesn't affect the eq-hash-code
                           (let ([b (string #\a)])
                             (define h0 (eq-hash-code b))
                             (string-set! b 0 #\b)
                             (define h1 (eq-hash-code b))
                             (equal? h0 h1)))))

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
                         (equal? (procedure-arity substring) '(2 3)))) ; todo - improve arities

              (list "string-append"
                    (and (equal? (string-append) "")
                         (equal? (string-append "A") "A")
                         (equal? (string-append "A" "B") "AB")
                         (equal? (string-append "A" "B" "C") "ABC")
                          #;(equal? (procedure-arity string-append) (arity-at-least 0)) ; todo
                         ))

              (list "string-append-immutable"
                    (and (equal? (string-append-immutable "foo" "bar") "foobar")
                         (equal? (string-append-immutable) "")
                         (equal? (immutable? (string-append-immutable "foo")) #t)))

              (list "string-append*"
                    (and (equal? (string-append* (list))                  "")
                         (equal? (string-append* (list "A"))              "A")
                         (equal? (string-append* "A" (list "B" "C"))      "ABC")
                         (equal? (string-append* "A" "B" (list "C" "D"))  "ABCD")))

              (list "string-join"
                    (and (equal? (string-join '("one" "two" "three" "four"))
                                 "one two three four")
                         (equal? (string-join '("one" "two" "three" "four") ", ")
                                 "one, two, three, four")
                         (equal? (string-join '("one" "two" "three" "four") " potato ")
                                 "one potato two potato three potato four")))

              (list "string-split"
                    (and (equal? (string-split "foo bar baz")
                                 '("foo" "bar" "baz"))
                         (equal? (string-split "  foo  bar  " " " #t #t)
                                 '("foo" "bar"))
                         (equal? (string-split "  ") '())
                         (equal? (string-split "  " " " #f)
                                 '("" "" ""))
                         (equal? (string-split "" " " #f)
                                 '(""))
                         (equal? (string-split "abc" "")
                                 '("a" "b" "c"))))

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
                           (equal? (procedure-arity string-fill!) 2)
                           ; string-fill! doesn't affect the eq-hash-code
                           (let ([b (string #\a)])
                             (define h0 (eq-hash-code b))
                             (string-fill! b #\b)
                             (define h1 (eq-hash-code b))
                             (equal? h0 h1)))))

              (list "string-copy!"
                    (let ([s (make-string 10 #\x)])
                      (string-copy! s 0 "hello")
                      (list (equal? s "helloxxxxx")
                            (list (procedure-arity string-copy!) '(3 4 5))))) ; todo - got #f - improve arities

              (list "string->immutable-string"
                    (and (equal? (string->immutable-string "hi") "hi")
                         (equal? (immutable? (string->immutable-string "hi")) #t)
                         (equal? (procedure-arity string->immutable-string) 1)))

              (list "string=?"
                    (and (equal? (string=? "") #t)
                         (equal? (string=? "A" "A") #t)
                         (equal? (string=? "A" "B") #f)
                         (equal? (string=? "A" "AB") #f)
                         (equal? (string=? "A" "A" "A") #t)
                         (equal? (string=? "A" "A" "B") #f)))

              (list "string<?"
                    (and (equal? (string<? "A") #t)
                         (equal? (string<? "" "") #f)
                         (equal? (string<? "A" "B") #t)
                         (equal? (string<? "AB" "A") #f)
                         (equal? (string<? "A" "B" "C") #t)
                         (equal? (string<? "A" "C" "B") #f)))

              (list "string>?"
                    (and (equal? (string>? "A") #t)
                         (equal? (string>? "" "") #f)
                         (equal? (string>? "B" "A") #t)
                         (equal? (string>? "A" "AB") #f)
                         (equal? (string>? "C" "B" "A") #t)
                         (equal? (string>? "C" "A" "B") #f)))

              (list "string<=?"
                    (and (equal? (string<=? "A") #t)
                         (equal? (string<=? "" "") #t)
                         (equal? (string<=? "A" "B") #t)
                         (equal? (string<=? "AB" "A") #f)
                         (equal? (string<=? "A" "B" "B") #t)
                         (equal? (string<=? "A" "B" "A") #f)))

              (list "string>=?"
                    (and (equal? (string>=? "A") #t)
                         (equal? (string>=? "" "") #t)
                         (equal? (string>=? "B" "A") #t)
                         (equal? (string>=? "A" "AB") #f)
                         (equal? (string>=? "B" "B" "A") #t)
                         (equal? (string>=? "B" "A" "B") #f)))
              
              (list "string-ci=?"
                    (and (equal? (string-ci=? "A") #t)
                         (equal? (string-ci=? "A" "a") #t)
                         (equal? (string-ci=? "Hello" "hELLo") #t)
                         (equal? (string-ci=? "A" "B") #f)
                         (equal? (string-ci=? "A" "AB") #f)
                         (equal? (string-ci=? "A" "a" "A") #t)
                         (equal? (string-ci=? "A" "a" "B") #f)))
             (list "string-ci<?"
                   (and (equal? (string-ci<? "A") #t)
                        (equal? (string-ci<? "A" "b") #t)
                        (equal? (string-ci<? "a" "A") #f)
                        (equal? (string-ci<? "A" "B" "c") #t)
                        (equal? (string-ci<? "A" "c" "B") #f)))
             (list "string-ci<=?"
                   (and (equal? (string-ci<=? "A") #t)
                        (equal? (string-ci<=? "A" "a") #t)
                        (equal? (string-ci<=? "A" "b") #t)
                        (equal? (string-ci<=? "A" "b" "b") #t)
                        (equal? (string-ci<=? "A" "b" "a") #f)))
             (list "string-ci>?"
                   (and (equal? (string-ci>? "A") #t)
                        (equal? (string-ci>? "b" "A") #t)
                        (equal? (string-ci>? "A" "a") #f)
                        (equal? (string-ci>? "c" "B" "a") #t)
                        (equal? (string-ci>? "c" "a" "B") #f)))
             (list "string-ci>=?"
                   (and (equal? (string-ci>=? "A") #t)
                        (equal? (string-ci>=? "A" "a") #t)
                        (equal? (string-ci>=? "b" "A") #t)
                        (equal? (string-ci>=? "b" "a" "a") #t)
                        (equal? (string-ci>=? "b" "a" "c") #f)))

              (list "string-ci<?"
                    (and (equal? (string-ci<? "A" "b") #t)
                         (equal? (string-ci<? "apple" "BANANA") #t)
                         (equal? (string-ci<? "Apple" "apple") #f)))

              (list "string-ci>?"
                    (and (equal? (string-ci>? "b" "A") #t)
                         (equal? (string-ci>? "BANANA" "apple") #t)
                         (equal? (string-ci>? "Apple" "apple") #f)))

              (list "string-ci<=?"
                    (and (equal? (string-ci<=? "" "") #t)
                         (equal? (string-ci<=? "Apple" "apple") #t)
                         (equal? (string-ci<=? "banana" "Apple") #f)))

              (list "string-ci>=?"
                    (and (equal? (string-ci>=? "" "") #t)
                         (equal? (string-ci>=? "banana" "Apple") #t)
                         (equal? (string-ci>=? "apple" "BANANA") #f)))

              (list "string-upcase"
                    (and (equal? (string-upcase "abc!") "ABC!")
                         (equal? (procedure-arity string-upcase) 1)))

              (list "string-downcase"
                    (and (equal? (string-downcase "ABC!") "abc!")
                         (equal? (procedure-arity string-downcase) 1)))

              (list "string-titlecase" 
                    (and (equal? (string-titlecase "hello world")   "Hello World")
                         (equal? (string-titlecase "a\u0308bc")     "Äbc")
                         (equal? (string-titlecase "y\u200Dz")      "Y‍z")
                         (equal? (string-titlecase "ph\u02b0ysics") "Phʰysics")
                         (equal? (procedure-arity string-titlecase) 1)))

              (list "string-foldcase"
                    (and (equal? (string-foldcase "ABC!") "abc!")
                         (equal? (procedure-arity string-foldcase) 1)))

              (list "string->list"
                    (and (equal? (string->list "P l") '(#\P #\space #\l))
                         (equal? (string->list "") '())))

              (list "list->string"
                    (and (equal? (list->string '(#\1 #\\ #\")) "1\\\"")
                         (equal? (list->string '()) "")))

              (list "string-find"
                    (and (equal? (string-find "Racket" "ack") 1)
                         (equal? (string-find "Racket" "Rac") 0)
                         (equal? (string-find "Racket" "et")  4)
                         (equal? (string-find "Racket" "cat") #f)))

              (list "string-contains?"
                    (and (equal? (string-contains? "Racket" "ack") #t)
                         (equal? (string-contains? "Racket" "cat") #f)))

              (list "string-replace"
                    (and (equal? (string-replace "banana" "an" "oo")    "booooa")
                         (equal? (string-replace "banana" "an" "oo" #f) "booana")
                         (equal? (string-replace "ab" "" "-")           "-a-b-")))

              (list "string-trim"
                    (and (equal? (string-trim "  foo bar  baz \r\n\t")
                                 "foo bar  baz")
                         (equal? (string-trim "  foo bar  baz \r\n\t" " " #t #t #t)
                                 "foo bar  baz \r\n\t")
                         (equal? (string-trim "aaaxaayaa" "aa") "axaay")
                         (equal? (string-trim "aaaafooaaaa" "aa") "aafooaa")
                         (equal? (string-trim "aaaafooaaaa" "aa" #t #t #t) "foo")
                         (equal? (string-trim "xyzxyzabcxyzxyz" "xyz" #f #t #t)
                                 "xyzxyzabc")))
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
              (list "unsafe-bytes-length"
                    (and (equal? (unsafe-bytes-length #"abc") 3)
                         (equal? (unsafe-bytes-length #"") 0)))
              (list "unsafe-bytes-ref"
                    (and (equal? (unsafe-bytes-ref #"abc" 1) 98)
                         (equal? (unsafe-bytes-ref #"abc" 0) 97)))
              (list "unsafe-bytes-set!"
                    (let ([b (bytes 0 1 2)])
                      (unsafe-bytes-set! b 1 255)
                      (equal? b (bytes 0 255 2))))             
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

              (list "bytes-append*"
                    (and (equal? (bytes-append* (list))                      #"")
                         (equal? (bytes-append* (list #"A"))                 #"A")
                         (equal? (bytes-append* #"A" (list #"B" #"C"))       #"ABC")
                         (equal? (bytes-append* #"A" #"B" (list #"C" #"D"))  #"ABCD")))

              (list "bytes-join"
                    (equal? (bytes-join '(#"one" #"two" #"three" #"four") #" potato ")
                            #"one potato two potato three potato four"))

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

              (list "string-utf-8-length"
                    (let* ([s1 "çðö£"]
                           [s2 "aéllo"])
                      (and (equal? (string-utf-8-length s1) 8)
                           (equal? (string-utf-8-length "hello") 5)
                           (equal? (string-utf-8-length s2 1 4) 4))))

              (list "bytes-utf-8-length"
                    (let* ([b1 (bytes #xc3 #xa7 #xc3 #xb0 #xc3 #xb6 #xc2 #xa3)]
                           [b2 (make-bytes 5 65)]
                           [b3 (bytes 65 195 169 108 108 111)]
                           [invalid (bytes 65 255 66)])
                      (and (equal? (bytes-utf-8-length b1) 4)
                           (equal? (bytes-utf-8-length b2) 5)
                           (equal? (bytes-utf-8-length b3 #f 1 3) 1)
                           (equal? (bytes-utf-8-length invalid) #f)
                           (equal? (bytes-utf-8-length invalid #\uFFFD) 3))))

              (list "bytes->string/latin-1/basic"
                    (let* ([bstr (bytes #xfe #xd3 #xd1 #xa5)]
                           [result (bytes->string/latin-1 bstr)]
                           [expected (string (integer->char 254)
                                             (integer->char 211)
                                             (integer->char 209)
                                             (integer->char 165))])
                      (equal? result expected)))

              (list "bytes->string/latin-1/with-range"
                    (let* ([bstr (bytes 65 66 67 68)]
                           [slice (bytes->string/latin-1 bstr #\? 1 3)])
                      (and (equal? slice "BC")
                           (equal? (string-length slice) 2))))

              (list "bytes->string/latin-1/ignore-err-char"
                    (let* ([bstr (bytes 255 0 128)]
                           [default (bytes->string/latin-1 bstr)]
                           [with-err (bytes->string/latin-1 bstr #\X)])
                      (and (equal? default with-err)
                           (= (char->integer (string-ref default 0)) 255)
                           (= (char->integer (string-ref default 1)) 0)
                           (= (char->integer (string-ref default 2)) 128))))


              (list "bytes=?"
                    (and (equal? (bytes=? #"a" #"a" #"a") #t)
                         (equal? (bytes=? #"a" #"a") #t)
                         (equal? (bytes=? #"a") #t)
                         (equal? (bytes=? #"a" #"a" #"c") #f)
                         (equal? (bytes=? #"a" #"b" #"c") #f)
                         (equal? (bytes=? #"a" #"b") #f)
                         (equal? (bytes=? #"c" #"a" #"a") #f)
                         (equal? (bytes=? #"c" #"b" #"a") #f)
                         (equal? (bytes=? #"b" #"a") #f)))

              (list "bytes<?"
                    (and (equal? (bytes<? #""   #"")      #f)
                         (equal? (bytes<? #"A"  #"B")     #t)
                         (equal? (bytes<? #"AB" #"A")     #f)
                         (equal? (bytes<? #"a" #"b" #"c") #t)))

              (list "bytes>?"
                    (and (equal? (bytes>? #""   #"")       #f)
                         (equal? (bytes>? #"B"  #"A")      #t)
                         (equal? (bytes>? #"A"  #"AB")     #f)
                         (equal? (bytes>? #"ab" #"a")      #t)
                         (equal? (bytes>? #"c"  #"b" #"a") #t)))
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

              (list "char-utf-8-length"
                    (and (equal? (char-utf-8-length #\A) 1)
                         (equal? (char-utf-8-length (integer->char #x07FF)) 2)
                         (equal? (char-utf-8-length (integer->char #x0800)) 3)
                         (equal? (char-utf-8-length (integer->char #x10000)) 4)
                         (equal? (procedure-arity char-utf-8-length) 1)))

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

              (list "char-alphabetic?"
                    (and (equal? (char-alphabetic? #\A) #t)
                         (equal? (char-alphabetic? #\1) #f)
                         (equal? (procedure-arity char-alphabetic?) 1)))

              (list "char-lower-case?"
                    (and (equal? (char-lower-case? #\a) #t)
                         (equal? (char-lower-case? #\A) #f)
                         (equal? (procedure-arity char-lower-case?) 1)))

              (list "char-upper-case?"
                    (and (equal? (char-upper-case? #\A) #t)
                         (equal? (char-upper-case? #\a) #f)
                         (equal? (procedure-arity char-upper-case?) 1)))

              (list "char-title-case?"
                    (and (equal? (char-title-case? (integer->char #x01C5)) #t)
                         (equal? (char-title-case? #\A) #f)
                         (equal? (procedure-arity char-title-case?) 1)))

              (list "char-numeric?"
                    (and (equal? (char-numeric? #\1) #t)
                         (equal? (char-numeric? #\A) #f)
                         (equal? (procedure-arity char-numeric?) 1)))

              (list "char-symbolic?"
                    (and (equal? (char-symbolic? #\+) #t)
                         (equal? (char-symbolic? #\A) #f)
                         (equal? (procedure-arity char-symbolic?) 1)))

              (list "char-punctuation?"
                    (and (equal? (char-punctuation? #\,) #t)
                         (equal? (char-punctuation? #\A) #f)
                         (equal? (procedure-arity char-punctuation?) 1)))

              (list "char-graphic?"
                    (and (equal? (char-graphic? #\A) #t)
                         (equal? (char-graphic? #\space) #f)
                         (equal? (procedure-arity char-graphic?) 1)))

              (list "char-whitespace?"
                    (and (equal? (char-whitespace? #\space) #t)
                         (equal? (char-whitespace? #\tab) #t)
                         (equal? (char-whitespace? #\A) #f)
                         (equal? (procedure-arity char-whitespace?) 1)))

              (list "char-grapheme-break-property"
                    (and (equal? (char-grapheme-break-property #\a)       'Other)
                         (equal? (char-grapheme-break-property #\return)  'CR)
                         (equal? (char-grapheme-break-property #\newline) 'LF)
                         (equal? (char-grapheme-break-property #\u0300)   'Extend)
                         (equal? (char-grapheme-break-property #\u200D)   'ZWJ)
                         ; TODO - The test below fails. Why?
                         (equal? (char-grapheme-break-property #\U1F1E6)  'Regional_Indicator)
                         (equal? (char-grapheme-break-property #\u0600)   'Prepend)
                         (equal? (char-grapheme-break-property #\u0903)   'SpacingMark)
                         (equal? (char-grapheme-break-property #\u1100)   'L)
                         (equal? (char-grapheme-break-property #\u1161)   'V)
                         (equal? (char-grapheme-break-property #\u11A8)   'T)
                         (equal? (char-grapheme-break-property #\uAC00)   'LV)
                         (equal? (char-grapheme-break-property #\uAC01)   'LVT)
                         (equal? (procedure-arity char-grapheme-break-property) 1)))

              (list "char-grapheme-step"
                    (and (let*-values ([(consumed  state)  (char-grapheme-step #\a 0)]
                                       [(consumed2 state2) (char-grapheme-step #\b state)])
                           (and (eq? consumed #f)
                                (= state 1)
                                (eq? consumed2 #t)
                                (= state2 1)))
                         (let*-values ([(consumed  state)  (char-grapheme-step #\return 0)]
                                       [(consumed2 state2) (char-grapheme-step #\newline state)])
                           (and (eq? consumed  #f)
                                (eq? consumed2 #t)
                                (= state2 0)))
                         (let*-values ([(consumed  state)  (char-grapheme-step #\a 0)]
                                       [(consumed2 state2) (char-grapheme-step #\u0300 state)])
                           (and (eq? consumed  #f)
                                (eq? consumed2 #f)
                                (= state2 5)))
                         (let*-values ([(c1 s1) (char-grapheme-step #\U1F1E6 0)]
                                       [(c2 s2) (char-grapheme-step #\U1F1E7 s1)]
                                       [(c3 s3) (char-grapheme-step #\U1F1E8 s2)])
                           (and (eq? c1 #f)
                                (eq? c2 #f)
                                (eq? c3 #t)
                                (> s3 0)))
                         (let*-values ([(h1 hs1) (char-grapheme-step #\u1100 0)]
                                       [(h2 hs2) (char-grapheme-step #\u1161 hs1)]
                                       [(h3 hs3) (char-grapheme-step #\u11A8 hs2)])
                           (and (eq? h1 #f)
                                (eq? h2 #f)
                                (eq? h3 #f)))
                         ; TODO - this provokes an error - why ?!
                         (equal? (procedure-arity char-grapheme-step) 2)))

              (list "char-general-category"
                    (and (equal? (char-general-category #\A) 'lu)
                         (equal? (char-general-category #\a) 'll)
                         (equal? (char-general-category #\1) 'nd)
                         (equal? (char-general-category #\space) 'zs)
                         (equal? (procedure-arity char-general-category) 1)))

              (list "char-blank?"
                    (and (equal? (char-blank? #\space) #t)
                         (equal? (char-blank? #\newline) #f)
                         (equal? (procedure-arity char-blank?) 1)))

              (list "char-iso-control?"
                    (and (equal? (char-iso-control? #\nul) #t)
                         (equal? (char-iso-control? #\space) #f)
                         (equal? (procedure-arity char-iso-control?) 1)))

              (list "char-extended-pictographic?"
                    (and (equal? (char-extended-pictographic? (integer->char #x1F600)) #t)
                         (equal? (char-extended-pictographic? #\A) #f)
                         (equal? (procedure-arity char-extended-pictographic?) 1)))

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
                ;; (js-log y)
                ;; (js-log x)
                ;; (js-log (symbol->string y))
                (let ([and list] [equal? list])
                (list "symbol/string interop"
                      (and (equal? x "cb")
                           (equal? (symbol->string y) "ab")
                           (let ([ab1 (string->symbol "ab")]
                                 [ab2 (string->symbol "ab")])
                             (list (equal? ab1 ab2)
                                   (eq-hash-code ab1)
                                   (eq-hash-code ab2)
                                   (equal-hash-code ab1)
                                   (equal-hash-code ab2))) ; hash table problem?
                           (equal? (string->symbol "ab") y)
                           (list (string->symbol "ab") y
                                 (symbol->string (string->symbol "ab")) (symbol->string y))
                           ;; symbol->string returns fresh strings (not eq?)
                           (equal? (eq? (symbol->string 'apple)
                                        (symbol->string 'apple))
                                   #f)
                           (equal? (symbol->immutable-string 'apple) "apple")
                           (equal? (immutable? (symbol->immutable-string 'apple)) #t)
                           (equal? (immutable? (symbol->immutable-string 'box))   #t)))))

              (let ([a (string->uninterned-symbol "a")]
                    [b (string->uninterned-symbol "a")])
                (list "symbol=?"
                      (and (equal? (symbol=? 'a 'a) #t)
                           (equal? (symbol=? 'a 'b) #f)
                           (equal? (symbol=? a b)   #f)
                           (equal? (eq? a b)        #f)
                           )))

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
                         (equal? (procedure-arity symbol<?) 1)))

              (list "gensym"
                    (let* ([s0 (gensym)]
                           [s1 (gensym 'apple)]
                           [s2 (gensym "apple")]
                           [s3 (gensym 'apple)])
                      (and (equal? (symbol? s0)                                  #t)
                           (equal? (symbol-interned? s0)                         #f)
                           (equal? (string-prefix? (symbol->string s0) "g" )     #t)
                           (equal? (string-prefix? (symbol->string s1) "apple" ) #t)
                           (equal? (symbol-interned? s1)                         #f)
                           (equal? (string-prefix? (symbol->string s2) "apple" ) #t)
                           (equal? (eq? s1 s3)                                   #f))))))

       (list "4.9 Keywords"
             (list
              (list "keyword?"
                    (and (equal? (keyword? '#:a) #t)
                         (equal? (keyword? 'a) #f)
                         (equal? (string->keyword "apple") '#:apple)
                         (equal? (keyword->string '#:apple) "apple")
                         ;; keyword->string returns fresh strings (not eq?)
                         (equal? (eq? (keyword->string '#:apple)
                                        (keyword->string '#:apple))
                                   #f)
                         (equal? (keyword->immutable-string '#:apple) "apple")
                         (equal? (immutable? (keyword->immutable-string '#:apple)) #t)

                         (equal? (procedure-arity keyword?) 1)))
              (list "keyword<?"
                    (and (equal? (keyword<? '#:a) #t)
                         (equal? (keyword<? '#:a '#:b) #t)
                         (equal? (keyword<? '#:b '#:b) #f)
                         (equal? (keyword<? '#:b '#:bb) #t)
                         (equal? (keyword<? '#:b '#:) #f)
                         (equal? (keyword<? '#:b '#:c '#:d) #t)
                         (equal? (keyword<? '#:b '#:c '#:c) #f)
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

              (list "append*"
                    (and (equal? (append* '())                         '())
                         (equal? (append* '((a) (b c)))               '(a b c))
                         (equal? (append* '(a) '((b) (c)))            '(a b c))
                         (equal? (append* '(a) '(b) '((c d) (e)))     '(a b c d e))
                         (equal? (append* '(1 2) '((3 . 4)))          '(1 2 3 . 4))))

              (list "reverse"
                    (and (equal? (reverse '(a b c))             '(c b a))
                         (equal? (reverse '(a (b c) d (e (f)))) '((e (f)) d (b c) a))
                         (equal? (procedure-arity reverse)      1)))

              (list "flatten"
                    (and (equal? (flatten '((a) b (c (d) . e) ())) '(a b c d e))
                         (equal? (flatten 'a)                     '(a))
                         (equal? (procedure-arity flatten)        1)))

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

              (list "list-update"
                    (and (equal? (list-update '(zero one two) 1 symbol->string)
                                 '(zero "one" two))
                         (equal? (procedure-arity list-update) 3)))
              (list "list-set"
                    (and (equal? (list-set '(a b c d) 2 'x) '(a b x d))
                         (equal? (procedure-arity list-set) 3)))
              (list "first"
                    (and (equal? (first '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) 1)
                         (equal? (procedure-arity first) 1)))
              (list "rest"
                    (and (equal? (rest '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
                                 '(2 3 4 5 6 7 8 9 10 11 12 13 14 15))
                         (equal? (procedure-arity rest) 1)))
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

              (list "foldl"
                    (and (equal? (foldl cons '() '(1 2 3 4)) '(4 3 2 1))
                         (equal? (foldl + 0 '(1 2 3 4)) 10)
                         (equal? (foldl (λ (a b acc) (cons (cons a b) acc))
                                        '()
                                        '(a b c) '(x y z))
                                 '((c . z) (b . y) (a . x)))))
              (list "foldr"
                    (and (equal? (foldr cons '() '(1 2 3 4)) '(1 2 3 4))
                         (equal? (foldr + 0 '(1 2 3 4)) 10)
                         (equal? (foldr (λ (a b acc) (cons (cons a b) acc))
                                        '()
                                        '(a b c) '(x y z))
                                 '((a . x) (b . y) (c . z)))))
              (list "filter"
                    (and (equal? (filter positive? '(1 -2 3 4 -5)) '(1 3 4))
                           (equal? (filter positive? '()) '())
                           (with-handlers ([exn:fail:contract:arity? (λ _ #t)])
                             (filter positive?) #f))
                    (and (equal? (filter (λ (x) (positive? x)) '(1 -2 3 4 -5)) '(1 3 4))
                         (equal? (filter (λ (x) (positive? x)) '()) '())))
              (list "filter-map"
                    (and (equal? (filter-map (λ (x) (and (negative? x) (abs x)))
                                             '(1 2 -3 -4 8))
                                 '(3 4))
                         (equal? (filter-map (λ (x y) (and (< x y) (+ x y)))
                                             '(1 2 3)
                                             '(2 1 4))
                                 '(3 7))))
              (list "append-map"
                    (and (equal? (append-map (λ (x) (vector->list x)) '(#(1) #(2 3) #(4)))
                                 '(1 2 3 4))
                         (equal? (append-map (λ (x) (list x x)) '(1 2 3))
                                 '(1 1 2 2 3 3))))

              (list "filter-not"
                    (and (equal? (filter-not (λ (x) (positive? x)) '(1 -2 3 4 -5)) '(-2 -5))
                         (equal? (filter-not (λ (x) (positive? x)) '())
                                '())
                         (with-handlers ([exn:fail:contract:arity? (λ _ #t)])
                           (filter-not positive?) #f)))

              (list "shuffle"
                    (let ([l '(1 2 3 4 5 6)])
                      (let ([s (shuffle l)])
                        (and (equal? (length s) (length l))
                             (let loop ([xs s] [rem l])
                               (if (null? xs)
                                   (null? rem)
                                   (let ([rest (remove (car xs) rem)])
                                     (and (not (eq? rest rem))
                                          (loop (cdr xs) rest)))))))))

              (list "partition"
                    (and (let-values ([(pos neg)
                                       (partition (λ (x) (positive? x)) '(1 -2 3 4 -5))])
                           (and (equal? pos '(1 3 4))
                                (equal? neg '(-2 -5))))
                         (let-values ([(pos neg)
                                       (partition (λ (x) (positive? x)) '())])
                           (and (equal? pos '())
                                (equal? neg '())))))

              (list "remove"
                    (and (equal? (remove 2 (list 1 2 3 2 4)) '(1 3 2 4))
                         (equal? (remove '(2) (list '(1) '(2) '(3))) '((1) (3)))
                         (equal? (remove "2" (list "1" "2" "3")) '("1" "3"))
                         (equal? (remove #\c (list #\a #\b #\c)) '(#\a #\b))
                         (equal? (remove "b" (list "a" "A" "b" "B") (λ (x y) (string=? x y))) '("a" "A" "B"))
                         #;(equal? (remove "b" (list "a" "A" "b" "B") string=?) '("a" "A" "B")) ; todo - repair
                         (equal? (remove "b" (list "a" "A" "b" "B") equal?) '("a" "A" "B"))
                         (let ([lst (list 1 2 3 2 4)])
                           (and (eq? (remove 5 lst) lst)
                                (equal? (remove 5 lst) lst)))))
              (list "remf"
                    (and (equal? (remf negative? '(1 -2 3 4 -5)) '(1 3 4 -5))
                         (let ([lst (list 1 2 3)])
                           (and (eq? (remf negative? lst) lst)
                                (equal? (remf negative? lst) lst)))
                         (equal? (procedure-arity remf) 2)))
              (list "remf*"
                    (and (equal? (remf* negative? '(1 -2 3 4 -5)) '(1 3 4))
                         (let ([lst (list 1 2 3)])
                           (and (eq? (remf* negative? lst) lst)
                                (equal? (remf* negative? lst) lst)))
                         (equal? (procedure-arity remf*) 2)))
              (list "remq"
                    (and (equal? (remq 2 (list 1 2 3 2 4)) '(1 3 2 4))
                         (equal? (procedure-arity remq) 2)))

              (list "remv"
                    (and (equal? (remv 2 (list 1 2 3 2 4)) '(1 3 2 4))
                         (equal? (procedure-arity remv) 2)))

              (list "remw"
                    (and (equal? (remw 2 (list 1 2 3 2 4)) '(1 3 2 4))
                         (equal? (procedure-arity remw) 2)))

              (list "remove*"
                    (and (equal? (remove* '(1 2) (list 1 2 3 2 4 5 2)) '(3 4 5))
                         (equal? (remove* '((2)) '((1) (2) (3))) '((1) (3)))
                         (let ([lst (list 1 2 3)])
                           (and (eq? (remove* '(4 5) lst) lst)
                                (equal? (remove* '(4 5) lst) lst)))
                         (equal? (remove* '("b") (list "a" "A" "b" "B")
                                          (λ (x y) (string=? x y)))
                                 '("a" "A" "B"))))

              (list "remq*"
                    (and (equal? (remq* '(1 2) (list 1 2 3 2 4)) '(3 4))
                         (equal? (procedure-arity remq*) 2)))

              (list "remv*"
                    (and (equal? (remv* '(1 2) (list 1 2 3 2 4)) '(3 4))
                         (equal? (procedure-arity remv*) 2)))

              (list "remw*"
                    (and (equal? (remw* '(1 2) (list 1 2 3 2 4)) '(3 4))
                         (equal? (procedure-arity remw*) 2)))
              (list "sort"
                    (and (equal? (sort '(3 1 2)       (λ (x y) (<        x y))) '(1 2 3))
                         (equal? (sort '("c" "a" "b") (λ (x y) (string<? x y))) '("a" "b" "c"))))
              (list "count"
                    (and (equal? (count (λ (x)   (positive? x)) '(1 -1 2 3 -2 5))  4)
                         (equal? (count (λ (x y) (< x y))       '(1 2 3) '(2 2 4)) 2)))

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

              (list "range"
                    (and (equal? (range 10) '(0 1 2 3 4 5 6 7 8 9))
                         (equal? (range 10 20) '(10 11 12 13 14 15 16 17 18 19))
                         (equal? (range 20 10) '())
                         (equal? (range 20 10 -1) '(20 19 18 17 16 15 14 13 12 11))
                         (equal? (range 10 15 1.5) '(10.0 11.5 13.0 14.5))))

              (list "inclusive-range"
                    (and (equal? (inclusive-range 10 20)
                                 '(10 11 12 13 14 15 16 17 18 19 20))
                         (equal? (inclusive-range 20 10)
                                 '(20 19 18 17 16 15 14 13 12 11 10))
                         (equal? (inclusive-range 20 10 -1)
                                 '(20 19 18 17 16 15 14 13 12 11 10))
                         (equal? (inclusive-range 10 15 1.5)
                                 '(10.0 11.5 13.0 14.5))))

              (list "member"
                    (and (equal? (member 'a '(a b c))     '(a b c))
                         (equal? (member 'b '(a b c))     '(b c))
                         (equal? (member 'b '(a b . c))   '(b . c))
                         (equal? (member 'a '(b c d))     #f)
                         (equal? (member 2 '(1 2 1 2) (λ (x y) (= x y)))  '(2 1 2))
                         (equal? (member 2 '(3 4 5 6) (λ (x y) (= x y)))  #f)
                         (equal? (member #"b" '(#"a" #"b" #"c") (λ (x y) (bytes=? x y))) '(#"b" #"c"))
                         ; (procedure-arity member) ; returns #f   TODO
                         (equal? (procedure-arity member) '(2 3)))) ; fails, TODO
              (list "memq"
                    (and (equal? (memq 'a '(a b c))   '(a b c))
                         (equal? (memq 'b '(a b c))   '(b c))
                         (equal? (memq 'b '(a b . c)) '(b . c))
                         (equal? (memq 'a '(b c d))   #f)
                         (equal? (memq  "apple" '( "apple"))         '("apple"))   ; todo - intern literals
                         (equal? (memq #"apple" '(#"apple"))         '(#"apple"))  ; todo - intern literals
                         (equal? (memq (list->string (string->list "apple"))
                                       '("apple"))
                                 #f)
                         (equal? (procedure-arity memq) 2)))
              (list "memv"
                    (and (equal? (memv 'a '(a b c)) '(a b c))
                         (equal? (memv 'b '(a b c)) '(b c))
                         (equal? (memv 'b '(a b . c)) '(b . c))
                         (equal? (memv 'a '(b c d)) #f)
                         (equal? (memv (list->string (string->list "apple"))
                                       '("apple"))
                                 #f)
                         (equal? (memv 1/2 '(1/2)) '(1/2))
                         (equal? (procedure-arity memv) 2)))

              (list "memw" ; TODO - implement equal-always to get this test to pass
                    (and (equal? (memw 'a '(a b c)) '(a b c))
                         (equal? (memw 'b '(a b c)) '(b c))
                         (equal? (memw 'b '(a b . c)) '(b . c))
                         (equal? (memw 'a '(b c d)) #f)
                         (equal? (memw (list->string (string->list "apple"))
                                       '("apple"))
                                 #f)
                         (equal? (memw (string->immutable-string (list->string (string->list "apple")))
                                       '("apple"))
                                 '("apple"))
                         (equal? (memw 1/2 '(1/2)) '(1/2))
                         (equal? (memw '(1 2) '(1 2 (1 2))) '((1 2)))
                         (equal? (procedure-arity memw) 2)))
              (list "memf"
                    (and (equal? (memf (λ (x) (> x 9)) '(7 8 9 10 11)) '(10 11))
                         (equal? (memf (λ (x) (= x 7)) '(7 8 9 10 11)) '(7 8 9 10 11))
                         (equal? (memf (λ (x) (< x 0)) '(7 8 9 10 11)) #f)
                         (equal? (procedure-arity memf) 2)))
              (list "findf"
                    (and (equal? (findf (λ (x) (> x 9)) '(7 8 9 10 11)) 10)
                         (eq?    (findf (λ (x) (eq? x 'b)) '(a b c))    'b)
                         (equal? (findf (λ (x) (< x 0)) '(7 8 9 10 11)) #f)
                         (equal? (procedure-arity findf) 2)))
              (list "assq"
                    (and (equal? (assq 'b '((a 1) (b 2) (c 3))) '(b 2))
                         (equal? (assq 'd '((a 1) (b 2) (c 3))) #f)
                         (equal? (assq '(a) '(((a)) ((b)) ((c)))) #f)
                         (equal? (procedure-arity assq) 2)))
              (list "assv"
                    (and (equal? (assv 1/2 '((0 a) (1/2 b) (3 c))) '(1/2 b))
                         (equal? (assv 2 '((1 a) (3 b))) #f)
                         (equal? (procedure-arity assv) 2)))
              (list "assw"
                    (let ([b1 (box 0)]
                          [b2 (box 0)])
                      (and (equal? (assw '(b) '(((a)) ((b) 1) ((c)))) '((b) 1))
                           (equal? (assw 'd '((a 1) (b 2))) #f)
                           (equal? (assw b2 (list (cons b1 1) (cons b2 2))) (cons b2 2))
                           (equal? (procedure-arity assw) 2))))
              (list "assoc"
                    (and (equal? (assoc '(b) '(((a)) ((b) 1) ((c)))) '((b) 1))
                         (equal? (assoc 'd '((a 1) (b 2))) #f)
                         (equal? (assoc "B" '(("a" 1) ("b" 2)) string-ci=?) '("b" 2))))
              (list "assf"
                    (and (equal? (assf (λ (x) (> x 2)) '((1 a) (3 b) (5 c))) '(3 b))
                         (equal? (assf (λ (x) (< x 0)) '((1 a) (3 b))) #f)
                         (equal? (assf (λ (x) (and (> x 3) 'found)) '((1 a) (4 b))) '(4 b))
                         (equal? (procedure-arity assf) 2)))
              (list "argmax"
                    (and (equal? (argmax car '((3 pears) (1 banana) (2 apples))) '(3 pears))
                         (equal? (argmax car '((3 pears) (3 oranges))) '(3 pears))))

              (list "argmin"
                    (and (equal? (argmin car '((3 pears) (1 banana) (2 apples))) '(1 banana))
                         (equal? (argmin car '((1 banana) (1 orange))) '(1 banana))))
              (list "group-by"
                    (and (equal? (group-by (λ (x) (modulo x 3))
                                           '(1 2 1 2 54 2 5 43 7 2 643 1 2 0))
                                 '((1 1 43 7 643 1) (2 2 2 5 2 2) (54 0)))
                         (list (group-by (λ (x) x) '("A" "a" "b" "B")
                                           (λ (a b) (equal? (string-downcase a) (string-downcase b))))
                                 '(("B" "b") ("a" "A")))))
              (list "cartesian-product"
                    (and (equal? (cartesian-product) '(()))
                         (equal? (cartesian-product '(1 2) '(a b))
                                 '((1 a) (1 b) (2 a) (2 b)))
                         (equal? (cartesian-product '(1 2 3))
                                 '((1) (2) (3)))
                         (equal? (cartesian-product '(1 2) '()) '())))
              (list "permutations"
                    (and (equal? (permutations '(1 2 3))
                                 '((1 2 3) (2 1 3) (3 1 2) (1 3 2) (2 3 1) (3 2 1)))
                         (equal? (permutations '(x x)) '((x x) (x x)))))
              (list "index-of"
                    (let ([s1 (string #\a)]
                          [s2 (string #\a)])
                      (and (equal? (index-of '(1 2 3 4) 3) 2)
                           (equal? (index-of '(1 2 3 4) 5) #f)
                           (equal? (index-of (list s1) s2 eq?) #f))))
              (list "index-where"
                    (equal? (index-where '(1 2 3 4) even?) 1))
              (list "indexes-of"
                    (equal? (indexes-of '(1 2 1 2 1) 2) '(1 3)))
              (list "indexes-where"
                    (equal? (indexes-where '(1 2 3 4) even?) '(1 3)))
              (list "take"
                    (and (equal? (take '(1 2 3 4) 2) '(1 2))
                         (equal? (take '(a b . c) 1) '(a))
                         (equal? (take 'non-list 0) '())))
              (list "take-right"
                    (and (equal? (take-right '(1 2 3 4) 2) '(3 4))
                         (equal? (take-right '(a b . c) 1) '(b . c))
                         (equal? (take-right 'non-list 0) 'non-list)))
              (list "takef"
                    (and (equal? (takef '(2 4 5 8) even?) '(2 4))
                         (equal? (takef '(1 3 5) even?) '())
                         (equal? (takef 'non-list even?) '())))
              (list "takef-right"
                    (and (equal? (takef-right '(2 4 5 8) even?) '(8))
                         (equal? (takef-right '(1 3 5) even?) '())
                         (equal? (takef-right 'non-list even?) 'non-list)))
              (list "drop"
                    (and (equal? (drop '(1 2 3 4) 2) '(3 4))
                         (equal? (drop '(1 2 3 . 4) 3) 4)
                         (equal? (drop 'non-list 0) 'non-list)
                         (equal? (procedure-arity drop) 2)))
              (list "drop-right"
                    (and (equal? (drop-right '(1 2 3 4 5) 2) '(1 2 3))
                         (equal? (drop-right '(1 2 3 . 4) 1) '(1 2 . 4))
                         (equal? (drop-right 'non-list 0) 'non-list)
                         (equal? (procedure-arity drop-right) 2)))
              (list "dropf"
                    (and (equal? (dropf '(2 4 5 8) even?) '(5 8))
                         (equal? (dropf '(2 4 6 8) odd?) '(2 4 6 8))
                         (equal? (dropf 'non-list even?) 'non-list)
                         (equal? (procedure-arity dropf) 2)))
              (list "dropf-right"
                    (and (equal? (dropf-right '(1 2 3 4 5) odd?) '(1 2 3 4))
                         (equal? (dropf-right '(1 2 . 4) even?) '(1 . 4))
                         (equal? (dropf-right 'non-list even?) 'non-list)
                         (equal? (procedure-arity dropf-right) 2)))
              (list "split-at"
                    (and (let-values ([(prefix suffix) (split-at '(1 2 3 4) 2)])
                           (and (equal? prefix '(1 2))
                                (equal? suffix '(3 4))))
                         (let-values ([(prefix suffix) (split-at '(a b . c) 1)])
                           (and (equal? prefix '(a))
                                (equal? suffix '(b . c))))
                         (let-values ([(prefix suffix) (split-at 'non-list 0)])
                           (and (equal? prefix '())
                                (equal? suffix 'non-list)))))
              (list "split-at-right"
                    (and (let-values ([(prefix suffix) (split-at-right '(1 2 3 4 5) 2)])
                           (and (equal? prefix '(1 2 3))
                                (equal? suffix '(4 5))))
                         (let-values ([(prefix suffix) (split-at-right '(1 2 . 3) 1)])
                           (and (equal? prefix '(1))
                                (equal? suffix '(2 . 3))))
                         (let-values ([(prefix suffix) (split-at-right 'non-list 0)])
                           (and (equal? prefix '())
                                (equal? suffix 'non-list)))))
              (list "splitf-at"
                    (and (let-values ([(prefix suffix) (splitf-at '(2 4 5 8) even?)])
                           (and (equal? prefix '(2 4))
                                (equal? suffix '(5 8))))
                         (let-values ([(prefix suffix) (splitf-at '(1 3 5) even?)])
                           (and (equal? prefix '())
                                (equal? suffix '(1 3 5))))
                         (let-values ([(prefix suffix) (splitf-at 'non-list even?)])
                           (and (equal? prefix '())
                                (equal? suffix 'non-list)))
                         (let ([count 0])
                           (let-values ([(prefix suffix)
                                         (splitf-at '(1 2 3 4)
                                                    (lambda (x)
                                                      (set! count (add1 count))
                                                      (< x 3)))])
                             (and (= count 3)
                                  (equal? prefix '(1 2))
                                  (equal? suffix '(3 4)))))
                         (equal? (procedure-arity splitf-at) 2)))
              (list "splitf-at-right"
                    (and (let-values ([(prefix suffix) (splitf-at-right '(1 2 3 4 5) odd?)])
                           (and (equal? prefix '(1 2 3 4))
                                (equal? suffix '(5))))
                         (let-values ([(prefix suffix) (splitf-at-right '(1 2 . 4) even?)])
                           (and (equal? prefix '(1 . 4))
                                (equal? suffix '(2 . 4))))
                         (let-values ([(prefix suffix) (splitf-at-right 'non-list even?)])
                           (and (equal? prefix 'non-list)
                                (equal? suffix 'non-list)))
                         (let ([count 0])
                           (let-values ([(prefix suffix)
                                         (splitf-at-right '(1 2 3 4)
                                                          (lambda (x)
                                                            (set! count (add1 count))
                                                            (> x 2)))])
                             (and (= count 4)
                                  (equal? prefix '(1 2))
                                  (equal? suffix '(3 4)))))
                         (equal? (procedure-arity splitf-at-right) 2)))
              (list "list-prefix?"
                    (and (equal? (list-prefix? '(1 2) '(1 2 3 4 5)) #t)
                         (equal? (list-prefix? '(1 3) '(1 2 3 4 5)) #f)))
              (list "take-common-prefix"
                    (equal? (take-common-prefix '(a b c d) '(a b x y z)) '(a b)))
              (list "drop-common-prefix"
                    (let-values ([(l r)
                                  (drop-common-prefix '(a b c d) '(a b x y z))])
                      (and (equal? l '(c d))
                           (equal? r '(x y z)))))
              (list "split-common-prefix"
                    (let-values ([(p l r)
                                  (split-common-prefix '(a b c d) '(a b x y z))])
                      (and (equal? p '(a b))
                           (equal? l '(c d))
                           (equal? r '(x y z)))))
              (list "add-between"
                    (and (equal? (add-between '(x y z) 'and) '(x and y and z))
                         (equal? (add-between '(x) 'and)     '(x))
                         (equal? (add-between '() 'and)      '())))

              (list "caar..cddddr"
                    (let ([tree (letrec ([build (λ (depth path)
                                                  (if (zero? depth)
                                                      (string->symbol path)
                                                      (cons (build (sub1 depth) (string-append path "a"))
                                                            (build (sub1 depth) (string-append path "d")))))])
                                  (build 4 ""))])
                      (define (selector-result name)
                        (define str (symbol->string name))
                        (define letters
                          (reverse (string->list (substring str 1 (sub1 (string-length str))))))
                        (let loop ([v tree] [chs letters])
                          (if (null? chs)
                              v
                              (loop ((if (char=? (car chs) #\a) car cdr) v)
                                    (cdr chs)))))
                      (list
                       (list "caar"
                             (and (equal? (caar tree) (selector-result 'caar))
                                  (equal? (procedure-arity caar) 1)))
                       (list "cadr"
                             (and (equal? (cadr tree) (selector-result 'cadr))
                                  (equal? (procedure-arity cadr) 1)))
                       (list "cdar"
                             (and (equal? (cdar tree) (selector-result 'cdar))
                                  (equal? (procedure-arity cdar) 1)))
                       (list "cddr"
                             (and (equal? (cddr tree) (selector-result 'cddr))
                                  (equal? (procedure-arity cddr) 1)))
                       (list "caaar"
                             (and (equal? (caaar tree) (selector-result 'caaar))
                                  (equal? (procedure-arity caaar) 1)))
                       (list "caadr"
                             (and (equal? (caadr tree) (selector-result 'caadr))
                                  (equal? (procedure-arity caadr) 1)))
                       (list "cadar"
                             (and (equal? (cadar tree) (selector-result 'cadar))
                                  (equal? (procedure-arity cadar) 1)))
                       (list "caddr"
                             (and (equal? (caddr tree) (selector-result 'caddr))
                                  (equal? (procedure-arity caddr) 1)))
                       (list "cdaar"
                             (and (equal? (cdaar tree) (selector-result 'cdaar))
                                  (equal? (procedure-arity cdaar) 1)))
                       (list "cdadr"
                             (and (equal? (cdadr tree) (selector-result 'cdadr))
                                  (equal? (procedure-arity cdadr) 1)))
                       (list "cddar"
                             (and (equal? (cddar tree) (selector-result 'cddar))
                                  (equal? (procedure-arity cddar) 1)))
                       (list "cdddr"
                             (and (equal? (cdddr tree) (selector-result 'cdddr))
                                  (equal? (procedure-arity cdddr) 1)))
                       (list "caaaar"
                             (and (equal? (caaaar tree) (selector-result 'caaaar))
                                  (equal? (procedure-arity caaaar) 1)))
                       (list "caaadr"
                             (and (equal? (caaadr tree) (selector-result 'caaadr))
                                  (equal? (procedure-arity caaadr) 1)))
                       (list "caadar"
                             (and (equal? (caadar tree) (selector-result 'caadar))
                                  (equal? (procedure-arity caadar) 1)))
                       (list "caaddr"
                             (and (equal? (caaddr tree) (selector-result 'caaddr))
                                  (equal? (procedure-arity caaddr) 1)))
                       (list "cadaar"
                             (and (equal? (cadaar tree) (selector-result 'cadaar))
                                  (equal? (procedure-arity cadaar) 1)))
                       (list "cadadr"
                             (and (equal? (cadadr tree) (selector-result 'cadadr))
                                  (equal? (procedure-arity cadadr) 1)))
                       (list "caddar"
                             (and (equal? (caddar tree) (selector-result 'caddar))
                                  (equal? (procedure-arity caddar) 1)))
                       (list "cadddr"
                             (and (equal? (cadddr tree) (selector-result 'cadddr))
                                  (equal? (procedure-arity cadddr) 1)))
                       (list "cdaaar"
                             (and (equal? (cdaaar tree) (selector-result 'cdaaar))
                                  (equal? (procedure-arity cdaaar) 1)))
                       (list "cdaadr"
                             (and (equal? (cdaadr tree) (selector-result 'cdaadr))
                                  (equal? (procedure-arity cdaadr) 1)))
                       (list "cdadar"
                             (and (equal? (cdadar tree) (selector-result 'cdadar))
                                  (equal? (procedure-arity cdadar) 1)))
                       (list "cdaddr"
                             (and (equal? (cdaddr tree) (selector-result 'cdaddr))
                                  (equal? (procedure-arity cdaddr) 1)))
                       (list "cddaar"
                             (and (equal? (cddaar tree) (selector-result 'cddaar))
                                  (equal? (procedure-arity cddaar) 1)))
                       (list "cddadr"
                             (and (equal? (cddadr tree) (selector-result 'cddadr))
                                  (equal? (procedure-arity cddadr) 1)))
                       (list "cdddar"
                             (and (equal? (cdddar tree) (selector-result 'cdddar))
                                  (equal? (procedure-arity cdddar) 1)))
                       (list "cddddr"
                             (and (equal? (cddddr tree) (selector-result 'cddddr))
                                  (equal? (procedure-arity cddddr) 1))))))


              ; The test of map need to test all shapes (see $primitive-invoke)
              (list "map"
                      (and
                       ; fixed 1
                       (equal? (map add1 '(1 2 3))
                               '(2 3 4))
                       ; fixed 2
                       (equal? (map cons '(1 2) '(3 4))
                               '((1 . 3) (2 . 4)))
                       ; 2 or 3, has default
                       (equal? (map substring
                                      '("hello" "world")
                                      '(0 1)
                                      '(5 3))
                                 '("hello" "or"))
                       ; 3 or 4, has default
                       (equal? (map string-replace
                                      '("aba" "hello")
                                      '("a" "l")
                                      '("x" "L")
                                      '(#t #f))
                                 '("xbx" "heLlo"))
                       ; 1 to 5 arguments
                       (equal? (map string-trim
                                      '("  hi  " "--wow--")
                                      '(" " "-")
                                      '(#t #f)
                                      '(#t #t)
                                      '(#t #t))
                                 '("hi" "--wow"))
                       ; 0 or more
                       (equal? (map list
                                    '(1 2 3)
                                    '(4 5 6))
                               '((1 4) (2 5) (3 6)))
                       ; 1 or more
                       (equal? (map - '(1 2 3))
                                 '(-1 -2 -3))
                       ; 2 or more
                       (equal? (map filter-map
                                      (list (lambda (x) (and (positive? x) x))
                                            (lambda (x) (and (negative? x) x)))
                                      (list '(1 -2 3)
                                            '(-1 -2 5)))
                                 '((1 3) (-1 -2)))
                       ; 3 or more
                       (equal? (map foldl
                                      (list + *)
                                      (list 0 1)
                                      (list '(1 2 3)
                                            '(2 3 4)))
                                 '(6 24))))
              ))

       (list "4.11 Mutable Pairs and Lists"
             (list
              (list "mpair?"
                    (and (equal? (mpair? (mcons 1 2)) #t)
                         (equal? (mpair? 1) #f)
                         (equal? (procedure-arity mpair?) 1)))
              (list "mcons"
                    (let ([p (mcons 'a 'b)])
                      (and (mpair? p)
                           (equal? (procedure-arity mcons) 2))))
              (list "mcar"
                    (and (equal? (mcar (mcons 1 2)) 1)
                         (equal? (procedure-arity mcar) 1)))
              (list "mcdr"
                    (and (equal? (mcdr (mcons 1 2)) 2)
                         (equal? (procedure-arity mcdr) 1)))
              (list "set-mcar!"
                    (let ([p (mcons 1 2)])
                      (set-mcar! p 3)
                      (and (equal? (mcar p) 3)
                           (equal? (procedure-arity set-mcar!) 2))))
              (list "set-mcdr!"
                    (let ([p (mcons 1 2)])
                      (set-mcdr! p 3)
                      (and (equal? (mcdr p) 3)
                           (equal? (procedure-arity set-mcdr!) 2))))
              (list "equal-hash-code mpair"
                    (let ([p1 (mcons (mcons 'a '()) (mcons 'b '()))]
                          [p2 (mcons (mcons 'a '()) (mcons 'b '()))])
                      (eq? (equal-hash-code p1) (equal-hash-code p2))))
              ))

       (list "4.12 Vectors"
             (list
              (list "vector?"
                    (equal? (vector? (make-vector 0)) #t))

              (list "make-vector"
                    (and (equal? (make-vector 5) '#(0 0 0 0 0))
                         (equal? (make-vector 5 0) '#(0 0 0 0 0))))

              (list "build-vector"
                    (equal? (build-vector 5 (λ (x) (add1 x))) '#(1 2 3 4 5)))

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

              (list "vector->values"
                    (and (let-values ([() (vector->values '#())]) #t)
                         (let-values ([(a b c) (vector->values '#(a b c))])
                           (and (eq? a 'a) (eq? b 'b) (eq? c 'c)))
                         (let-values ([(b c) (vector->values '#(a b c d) 1 3)])
                           (and (eq? b 'b) (eq? c 'c)))
                         (let-values ([(b c d) (vector->values '#(a b c d) 1)])
                           (and (eq? b 'b) (eq? c 'c) (eq? d 'd)))
                         (let-values ([() (vector->values '#(a b c) 0 0)]) #t)))

              (list "vector->immutable-vector"
                    (and (let* ([v (vector 1 2)]
                                [w (vector->immutable-vector v)])
                           (and (equal? w '#(1 2))
                                (equal? (immutable? w) #t)
                                (not (eq? v w))))
                         (let ([v '#(3 4)])
                           (eq? (vector->immutable-vector v) v))))
              
              (list "vector-immutable"
                    (let ([v (vector-immutable 5 'a)])
                      (and (equal? v '#(5 a))
                           (equal? (immutable? v) #t))))

              (list "vector-copy"
                    (and (equal? (vector-copy '#(1 2 3 4))     '#(1 2 3 4))
                         (equal? (vector-copy '#(1 2 3 4) 3)   '#(4))
                         (equal? (vector-copy '#(1 2 3 4) 2 3) '#(3))
                         (equal? (vector-copy '#(1 2 3 4) 0 0) '#())
                         (equal? (vector-copy '#(1 2 3 4) 1 4) '#(2 3 4))))

              (list "vector-set/copy"
                    (let* ([v (vector 1 2 3)]
                           [w (vector-set/copy v 1 9)])
                      (and (equal? w '#(1 9 3))
                           (equal? v '#(1 2 3)))))

              (list "vector-extend"
                    (and (equal? (vector-extend '#(1 2 3) 5)    '#(1 2 3 0 0))
                         (equal? (vector-extend '#(1 2 3) 5 #f) '#(1 2 3 #f #f))
                         (equal? (vector-extend '#(1 2 3) 3 #f) '#(1 2 3))))
              
              (list "vector-append"
                    (let* ([v (vector 1 2)]
                           [w (vector-append v)])
                      (and (equal? (vector-append  #(1 2) #(3 4))          #(1 2 3 4))
                           (equal? (vector-append  #(1)   #()    #(2 3))   #(1 2 3))
                           (equal? (vector-append  #(1)   #(2 3) #(4 5 6)) #(1 2 3 4 5 6))
                           (equal? (vector-append  #(1 2))                 #(1 2))
                           (equal? (vector-append)                         #())
                           (not (eq? v w)))))

              (list "vector-count"
                    (let ([= (λ (x y) (= x y))])
                      (and (equal? (vector-count even? '#(1 2 3 4 5))           2)
                           (equal? (vector-count = '#(1 2 3 4 5) '#(5 4 3 2 1)) 1)
                           (equal? (vector-count = '#(1 2 3 4 5) '#(5 2 3 2 5)) 3))))

              (list "vector-length"
                    (and (equal? (vector-length '#(1 2 3)) 3)
                         (equal? (vector-length '#()) 0)))

              (list "unsafe-vector-length"
                    (equal? (unsafe-vector-length '#(1 2 3)) 3))

              (list "unsafe-vector-ref"
                    (equal? (unsafe-vector-ref '#(10 20 30) 1) 20))

              (list "vector-fill!"
                    (let ([v (vector 1 2 3)])
                      (vector-fill! v 9)
                      (equal? v '#(9 9 9))))

              (list "vector-empty?"
                    (and (equal? (vector-empty? '#()) #t)
                         (equal? (vector-empty? '#(1)) #f)))

              (list "vector-take"
                    (equal? (vector-take '#(a b c d) 2) '#(a b)))

              (list "vector-take-right"
                    (equal? (vector-take-right '#(a b c d) 2) '#(c d)))

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

              (list "vector-split-at-right"
                    (and (let-values ([(v1 v2) (vector-split-at-right '#(a b c d) 2)])
                           (and (equal? v1 '#(c d))
                                (equal? v2 '#(a b))))
                         (let-values ([(a b) (vector-split-at-right '#(1 2 3 4 5) 2)])
                           (and (equal? a '#(4 5))
                                (equal? b '#(1 2 3))))))

              (list "vector-map"
                    (and (equal? (vector-map + '#(1 2) '#(3 4))   '#(4 6))
                         (equal? (vector-map add1 '#(1 2 3))      '#(2 3 4))))

              (list "vector-map!"
                    (let ([v (vector 1 2 3 4)])
                      (and (equal? (vector-map! add1 v) '#(2 3 4 5))
                           (equal? v '#(2 3 4 5)))))

              (list "vector-argmin"
                    (let ([car (λ (x) (car x))])
                      (and (equal? (vector-argmin car (vector '(3 pears) '(1 banana) '(2 apples)))
                                   '(1 banana))
                           (equal? (vector-argmin car (vector '(1 banana) '(1 orange)))
                                   '(1 banana)))))

              (list "vector-argmax"
                    (let ([car (λ (x) (car x))])
                      (equal? (vector-argmax car (vector '(3 pears) '(1 banana) '(2 apples)))
                              '(3 pears))))

              (list "vector-filter"
                    (equal? (vector-filter even? '#(1 2 3 4 5 6)) '#(2 4 6)))
              
              (list "vector-filter-not"
                    (equal? (vector-filter-not even? '#(1 2 3 4 5 6)) '#(1 3 5)))

              (list "vector-memq"
                    (and (equal? (vector-memq 'b '#(a b c))    1)
                         (equal? (vector-memq 9 '#(1 2 3 4))   #f)
                         (equal? (procedure-arity vector-memq) 2)))

              (list "vector-memv"
                    (and (equal? (vector-memv 'b '#(a b c))    1)
                         (equal? (vector-memv 9 '#(1 2 3 4))   #f)
                         (equal? (procedure-arity vector-memv) 2)))
              (list "vector-member"
                    (let ([= (λ (x y) (= x y))])
                      (and (equal? (vector-member 2   '#(1 2 3 4))     1)
                           (equal? (vector-member 9   '#(1 2 3 4))     #f)
                           (equal? (vector-member 1.0 '#(3 2 1.0 4) =) 2))))

              (list "vector-sort!"
                    (let ([< (λ (x y) (< x y))])
                      (and (let ([v (vector 3 1 2 5 4)])
                             (vector-sort! v <)
                             (equal? v '#(1 2 3 4 5)))
                           (let ([v (vector 4 3 2 1 0)])
                             (vector-sort! v < 2)
                             (equal? v '#(4 3 0 1 2)))
                           (let ([v (vector 4 3 2 1 0)])
                             (vector-sort! v < 1 4)
                             (equal? v '#(4 1 2 3 0))))))

              (list "vector-sort"
                    (let ([< (λ (x y) (< x y))])
                      (and (equal? (vector-sort '#(3 1 2 5 4) <)
                                   '#(1 2 3 4 5))
                           (equal? (vector-sort '#(4 3 2 1 0) < 2)
                                   '#(0 1 2))
                           (equal? (vector-sort '#(4 3 2 1 0) < 1 4)
                                   '#(1 2 3)))))
              ))

       (list "4.15 Hash Tables"
             (list
              (list "make-empty-hash"
                    (let ([h (make-empty-hash)])
                      (and (hash? h)
                           (mutable-hash? h))))
              (list "make-empty-hasheq"
                    (let ([h (make-empty-hasheq)])
                      (and (hash? h)
                           (equal? (hash-has-key? h 'a) #f))))        
              (list "make-empty-hasheqv"
                    (let ([h (make-empty-hasheqv)])
                      (and (hash? h)
                           (mutable-hash? h))))
              (list "make-empty-hashalw"
                    (let ([h (make-empty-hashalw)])
                      (and (hash? h)
                           (mutable-hash? h))))        

              (list "hash-eq?"
                    (and (equal? (hash-eq? (make-hasheq))  #t)
                         (equal? (hash-eq? (make-hash))    #f)
                         (equal? (hash-eq? (make-hasheqv)) #f)
                         (equal? (hash-eq? (make-hashalw)) #f)))
              (list "hash-eqv?"
                    (and (equal? (hash-eqv? (make-hasheq))  #f)
                         (equal? (hash-eqv? (make-hash))    #f)
                         (equal? (hash-eqv? (make-hasheqv)) #t)
                         (equal? (hash-eqv? (make-hashalw)) #f)))
              (list "hash-equal?"
                    (and (equal? (hash-equal? (make-hasheq))  #f)
                         (equal? (hash-equal? (make-hash))    #t)
                         (equal? (hash-equal? (make-hasheqv)) #f)
                         (equal? (hash-equal? (make-hashalw)) #f)))
              (list "hash-equal-always?"
                    (list (equal? (hash-equal-always? (make-hasheq))  #f)
                          (equal? (hash-equal-always? (make-hash))    #f)
                          (equal? (hash-equal-always? (make-hasheqv)) #f)
                          (equal? (hash-equal-always? (make-hashalw)) #t)))

              (list "make-hash"
                    (let ([h (make-hash)])
                      (and (hash? h) (mutable-hash? h))))
              (list "make-hasheq"
                    (let ([h (make-hasheq)])
                      (and (hash? h) (mutable-hash? h))))
              (list "make-hasheqv"
                    (let ([h (make-hasheqv)])
                      (and (hash? h) (mutable-hash? h))))
              (list "make-hashalw"
                    (let ([h (make-hashalw)])
                      (and (hash? h) (mutable-hash? h))))

              (list "hash-set!"
                    (and (let ([h (make-hasheq)])
                           (hash-set! h 'a 1)
                           (equal? (hash-ref h 'a) 1))
                         (let ([h (make-hasheqv)])
                           (hash-set! h 'a 1)
                           (equal? (hash-ref h 'a) 1))
                         (let ([h (make-hash)])
                           (hash-set! h 'a 1)
                           (equal? (hash-ref h 'a) 1))
                         (let ([h (make-hashalw)])
                           (hash-set! h 'a 1)
                           (equal? (hash-ref h 'a) 1))))

              (list "hash-ref"
                    (and (let ([h (make-hasheq)])
                           (hash-set! h 'a 1)
                           (and (equal? (hash-ref h 'a)   1)
                                (equal? (hash-ref h 'b 2) 2)
                                (equal? (hash-ref h 'b (lambda () 3)) 3)))
                         (let ([h (make-hasheqv)])
                           (hash-set! h 'a 1)
                           (and (equal? (hash-ref h 'a) 1)
                                (equal? (hash-ref h 'b 2) 2)
                                (equal? (hash-ref h 'b (lambda () 3)) 3)))
                         (let ([h (make-hash)])
                           (hash-set! h 'a 1)
                           (and (equal? (hash-ref h 'a) 1)
                                (equal? (hash-ref h 'b 2) 2)
                                (equal? (hash-ref h 'b (lambda () 3)) 3)))
                         (let ([h (make-hashalw)])
                           (hash-set! h 'a 1)
                           (and (equal? (hash-ref h 'a) 1)
                                (equal? (hash-ref h 'b 2) 2)
                                (equal? (hash-ref h 'b (lambda () 3)) 3)))))

              (list "hash-ref!"
                    (and (let ([h (make-hasheq)])
                           (hash-set! h 'a 1)
                           (and (equal? (hash-ref! h 'a 'ignored) 1)
                                (equal? (hash-ref! h 'b 'two) 'two)
                                (equal? (hash-ref h 'b) 'two)
                                (equal? (hash-ref! h 'c (lambda () 'three)) 'three)
                                (equal? (hash-ref h 'c) 'three)))
                         (let ([h (make-hasheqv)])
                           (hash-set! h 'a 1)
                           (and (equal? (hash-ref! h 'a 'ignored) 1)
                                (equal? (hash-ref! h 'b 'two) 'two)
                                (equal? (hash-ref h 'b) 'two)
                                (equal? (hash-ref! h 'c (lambda () 'three)) 'three)
                                (equal? (hash-ref h 'c) 'three)))
                         (let ([h (make-hash)])
                           (hash-set! h 'a 1)
                           (and (equal? (hash-ref! h 'a 'ignored) 1)
                                (equal? (hash-ref! h 'b 'two) 'two)
                                (equal? (hash-ref h 'b) 'two)
                                (equal? (hash-ref! h 'c (lambda () 'three)) 'three)
                                (equal? (hash-ref h 'c) 'three)))
                         (let ([h (make-hashalw)])
                           (hash-set! h 'a 1)
                           (and (equal? (hash-ref! h 'a 'ignored) 1)
                                (equal? (hash-ref! h 'b 'two) 'two)
                                (equal? (hash-ref h 'b) 'two)
                                (equal? (hash-ref! h 'c (lambda () 'three)) 'three)
                                (equal? (hash-ref h 'c) 'three)))))

              (list "hash-update!"
                    (and (let ([h (make-hasheq)])
                           (hash-set! h 'a 1)
                           (hash-update! h 'a add1)
                           (hash-update! h 'b add1 0)
                           (hash-update! h 'c (lambda (x) (cons 'seed x)) (lambda () 'start))
                           (and (equal? (hash-ref h 'a) 2)
                                (equal? (hash-ref h 'b) 1)
                                (equal? (hash-ref h 'c) '(seed . start))))
                         (let ([h (make-hasheqv)])
                           (hash-set! h 'a 1)
                           (hash-update! h 'a add1)
                           (hash-update! h 'b add1 0)
                           (hash-update! h 'c (lambda (x) (cons 'seed x)) (lambda () 'start))
                           (and (equal? (hash-ref h 'a) 2)
                                (equal? (hash-ref h 'b) 1)
                                (equal? (hash-ref h 'c) '(seed . start))))
                         (let ([h (make-hash)])
                           (hash-set! h 'a 1)
                           (hash-update! h 'a add1)
                           (hash-update! h 'b add1 0)
                           (hash-update! h 'c (lambda (x) (cons 'seed x)) (lambda () 'start))
                           (and (equal? (hash-ref h 'a) 2)
                                (equal? (hash-ref h 'b) 1)
                                (equal? (hash-ref h 'c) '(seed . start))))
                         (let ([h (make-hashalw)])
                           (hash-set! h 'a 1)
                           (hash-update! h 'a add1)
                           (hash-update! h 'b add1 0)
                           (hash-update! h 'c (lambda (x) (cons 'seed x)) (lambda () 'start))
                           (and (equal? (hash-ref h 'a) 2)
                                (equal? (hash-ref h 'b) 1)
                                (equal? (hash-ref h 'c) '(seed . start))))))

              (list "hash-remove!"
                    (and (let ([h (make-hasheq)])
                           (hash-set! h 'a 1)
                           (hash-remove! h 'a)
                           (equal? (hash-has-key? h 'a) #f))
                         (let ([h (make-hasheqv)])
                           (hash-set! h 'a 1)
                           (hash-remove! h 'a)
                           (equal? (hash-has-key? h 'a) #f))
                         (let ([h (make-hash)])
                           (hash-set! h 'a 1)
                           (hash-remove! h 'a)
                           (equal? (hash-has-key? h 'a) #f))
                         (let ([h (make-hashalw)])
                           (hash-set! h 'a 1)
                           (hash-remove! h 'a)
                           (equal? (hash-has-key? h 'a) #f))))

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

              (list "hash-empty?"
                    (let ([h (make-hasheq)])
                      (and (equal? (hash-empty? h) #t)
                           (begin (hash-set! h 'a 1)
                                  (equal? (hash-empty? h) #f)))))

              (list "hash-count"
                    (let ([h (make-hasheq)])
                      (and (equal? (hash-count h) 0)
                           (begin
                             (hash-set! h 'a 1)
                             (hash-set! h 'b 2)
                             (equal? (hash-count h) 2)))))

              (list "hash->list"
                    (let ([<< (λ (x y) (< (car x) (car y)))])
                      (let ([h (make-hasheq)])
                        (hash-set! h 1 'a)
                        (hash-set! h 2 'b)
                        (hash-set! h 3 'c)
                        (let ([expected (list (cons 1 'a) (cons 2 'b) (cons 3 'c))])
                          (and (equal? (sort (hash->list h)    <<) expected)
                               (equal? (sort (hash->list h #t) <<) expected))))))

              (list "hash-for-each"
                    (let ([<< (λ (x y) (< (car x) (car y)))])
                      (let ([h   (make-hasheq)]
                            [acc (box '())])
                        (hash-set! h 1 'a)
                        (hash-set! h 2 'b)
                        (hash-set! h 3 'c)
                        (let ([r (hash-for-each
                                  h (λ (k v)
                                      (set-box! acc (cons (cons k v) (unbox acc)))))]
                              [expected (list (cons 1 'a) (cons 2 'b) (cons 3 'c))])
                          (and (eq? r (void))
                               (equal? (sort (unbox acc) <<) expected))))))

              (list "hash-map"
                    (let ([h (make-hasheq)])
                      (hash-set! h 1 'a)
                      (hash-set! h 2 'b)
                      (hash-set! h 3 'c)
                      (let ([keys  (sort  (hash-map h (λ (k v) k))     (λ (x y) (< x y)))]
                            [keys2 (sort  (hash-map h (λ (k v) k) #t)  (λ (x y) (< x y)))]
                            [vals  (sort  (hash-map h (λ (k v) v))     (λ (x y) (symbol<? x y)))])
                        (and (equal? keys  '(1 2 3))
                             (equal? keys2 '(1 2 3))
                             (equal? vals  '(a b c))))))

              (list "hash-map/copy"
                    (let ([h (make-hasheq)])
                      (hash-set! h 'a 1)
                      (hash-set! h 'b 2)
                      (let ([h2 (hash-map/copy h (lambda (k v) (values k (+ v 1))))])
                        (and (hash? h2)
                             (equal? (hash-ref h2 'a) 2)
                             (equal? (hash-ref h2 'b) 3)))))

              (list "hash-filter"
                    (let ([h (make-hasheq)])
                      (hash-set! h 1 'a)
                      (hash-set! h 2 'b)
                      (hash-set! h 3 'c)
                      (let ([res (hash-filter h (lambda (k v) (< k 3)))])
                        (and (hash? res)
                             (equal? (hash-eq? res) #t)
                             (equal? (hash-count res) 2)
                             (equal? (hash-ref res 1) 'a)
                             (equal? (hash-ref res 2) 'b)
                             (equal? (hash-has-key? res 3) #f)))))

              (list "hash-filter-keys"
                    (let ([h (make-hasheq)])
                      (hash-set! h 1 'one)
                      (hash-set! h 2 'two)
                      (hash-set! h 3 'three)
                      (let ([res (hash-filter-keys h (lambda (k) (<= k 2)))])
                        (and (hash? res)
                             (equal? (hash-eq? res) #t)
                             (equal? (hash-count res) 2)
                             (equal? (hash-ref res 1) 'one)
                             (equal? (hash-ref res 2) 'two)
                             (equal? (hash-has-key? res 3) #f)))))

              (list "hash-filter-values"
                    (let ([h (make-hasheq)])
                      (hash-set! h 'a 1)
                      (hash-set! h 'b 2)
                      (hash-set! h 'c 3)
                      (let ([res (hash-filter-values h (lambda (v) (< v 3)))])
                        (and (hash? res)
                             (equal? (hash-eq? res) #t)
                             (equal? (hash-count res) 2)
                             (equal? (hash-ref res 'a) 1)
                             (equal? (hash-ref res 'b) 2)
                             (equal? (hash-has-key? res 'c) #f)))))
              
              (list "hash-keys"
                    (let ([h (make-hasheq)])
                      (hash-set! h 'a 1)
                      (hash-set! h 'b 2)
                      (let ([expected '(a b)])
                        (and (equal? (sort (hash-keys h)    (λ (x y) (symbol<? x y))) expected)
                             (equal? (sort (hash-keys h #t) (λ (x y) (symbol<? x y))) expected)))))

              (list "hash-values"
                    (let ([h (make-hasheq)])
                      (hash-set! h 'a 1)
                      (hash-set! h 'b 2)
                      (let ([expected '(1 2)])
                        (and (equal? (sort (hash-values h)    (λ (x y) (< x y))) expected)
                             (equal? (sort (hash-values h #t) (λ (x y) (< x y))) expected)))))


              (list "mutable-hash-iterate"
                    (let ([empty (make-hash)]
                          [h     (make-hasheq)])
                      (hash-set! h 'x 10)
                      (hash-set! h 'y 20)

                      (define (unordered-equal? xs ys)
                        (and (= (length xs) (length ys))
                             (andmap (lambda (x) (member x ys)) xs)
                             (andmap (lambda (y) (member y xs)) ys)))

                      (define (collect f)
                        (let loop ([pos (mutable-hash-iterate-first h)] [acc '()])
                          (if pos
                              (loop (mutable-hash-iterate-next h pos)
                                    (cons (f pos) acc))
                              acc)))

                      (let* ([pairs    (collect (lambda (pos)
                                                  (mutable-hash-iterate-pair h pos 'bad)))]
                             [kvs      (collect (lambda (pos)
                                                  (call-with-values
                                                   (lambda ()
                                                     (mutable-hash-iterate-key+value h pos 'bad))
                                                   cons)))]
                             [keys     (collect (lambda (pos)
                                                  (mutable-hash-iterate-key h pos 'bad)))]
                             [vals     (collect (lambda (pos)
                                                  (mutable-hash-iterate-value h pos 'bad)))]
                             [expected '((x . 10) (y . 20))])
                        (and (equal? (mutable-hash-iterate-first empty) #f)
                             (unordered-equal? pairs expected)
                             (unordered-equal? kvs expected)
                             (equal? (sort keys symbol<?) '(x y))
                             (equal? (sort vals <) '(10 20))))))
              
              ))

       (list "4.20.2 Reflecting on Procedures"
             (list
              #;(list "arity-at-least? from procedure-arity"
                      (let ([arity (procedure-arity list)])
                        (and (equal? (arity-at-least? arity) #t)
                             (equal? (arity-at-least-value arity) 0))))

              #;(list "arity-at-least-value for rest lambda"
                      (let ([arity (procedure-arity (lambda (x . rest) x))])
                        (and (arity-at-least? arity)
                             (equal? (arity-at-least-value arity) 1))))

              #;(list "procedure-arity case-lambda mix"
                      (let ([arity (procedure-arity
                                     (case-lambda
                                       [(x) x]
                                       [(x y . rest) y]))])
                        (and (pair? arity)
                             (equal? (car arity) 1)
                             (let ([second (cadr arity)])
                               (and (arity-at-least? second)
                                    (equal? (arity-at-least-value second) 2))))))))
       
       ) ; ends "4. Datatypes"

 (list "5. Structures"
       (list "5.1 Structure Type Properties"
             (list
              (list "make-struct-type-property/basic"
                    (let*-values ([(prop:p prop:p? prop:p-get)
                                  (make-struct-type-property 'prop:p)]
                                 [(prop:q prop:q? prop:q-get)
                                  (make-struct-type-property
                                   'prop:q #f
                                   (list (cons prop:p (lambda (qv) (list 'p-from qv)))))]
                                 [(p-std make-p p? p-acc p-mut)
                                  (make-struct-type
                                   'type:p #f 0 0 #f
                                   (list (cons prop:p 'p-type-value))
                                   (current-inspector)
                                   #f #f #f 'make-p)]
                                 [(q-std make-q q? q-acc q-mut)
                                  (make-struct-type
                                   'type:q
                                   #f
                                   0
                                   0
                                   #f
                                   (list (cons prop:q 'q-type-value))
                                   (current-inspector)
                                   #f
                                   #f
                                   #f
                                   'make-q)])
                      (let* ([p-instance        (make-p)]
                             [q-instance        (make-q)]
                             [sentinel          '(fallback-value)]
                             [fallback          (lambda () sentinel)]
                             [p-from-q          (prop:p-get q-std)]
                             [p-from-q-instance (prop:p-get q-instance)])
                        (and
                         (struct-type-property? prop:p)
                         (struct-type-property? prop:q)
                         (struct-type-property-predicate-procedure? prop:p?)
                         (struct-type-property-predicate-procedure? prop:p? prop:p)
                         (equal? (struct-type-property-predicate-procedure? prop:p? prop:q) #f)
                         (struct-type-property-accessor-procedure? prop:p-get)
                         (struct-type-property-accessor-procedure? prop:q-get)
                         (prop:p? p-std)
                         (prop:p? p-instance)
                         (equal? (prop:p-get p-std) 'p-type-value)
                         (equal? (prop:p-get p-instance) 'p-type-value)
                         (equal? (prop:q? p-std) #f)
                         (equal? (prop:q? p-instance) #f)
                         (prop:q? q-std)
                         (prop:q? q-instance)
                         (prop:p? q-std)
                         (prop:p? q-instance)
                         (equal? (prop:q-get q-std) 'q-type-value)
                         (equal? (prop:q-get q-instance) 'q-type-value)
                         (equal? p-from-q '(p-from q-type-value))
                         (equal? p-from-q-instance '(p-from q-type-value))
                         (equal? (prop:q-get p-std fallback) sentinel)))))

              (list "make-struct-type-property/basic 2"
                    (let ()
                      (define-values (prop:p p? p-ref)
                        (make-struct-type-property 'p))
                      (define-values (prop:q q? q-ref)
                        (make-struct-type-property 'q
                                                   (lambda (v _si) (add1 v))
                                                   (list (cons prop:p sqrt))))
                      (define-values (struct:a make-a a? a-ref a-set!)
                        (make-struct-type 'a #f 1 0 #f (list (cons prop:p 8))))
                      (define-values (struct:plain make-plain plain? plain-ref plain-set!)
                        (make-struct-type 'plain #f 0 0))
                      (define-values (struct:c make-c c? c-ref c-set!)
                        (make-struct-type 'c #f 0 0 #f (list (cons prop:q 8))))
                      (define a-instance (make-a 'payload))
                      (define plain-instance (make-plain))
                      (define c-instance (make-c))
                      (define fallback-symbol 'no-prop)
                      (and (equal? (struct-type-property? prop:p) #t)
                           (equal? (struct-type-property? prop:q) #t)
                           (equal? (struct-type-property? 'p) #f)
                           (equal? (struct-type-property-predicate-procedure? p?) #t)
                           (equal? (struct-type-property-predicate-procedure? p? prop:p) #t)
                           (equal? (struct-type-property-predicate-procedure? p? prop:q) #f)
                           (equal? (struct-type-property-predicate-procedure? q? prop:q) #t)
                           (equal? (struct-type-property-accessor-procedure? p-ref) #t)
                           (equal? (struct-type-property-accessor-procedure? q-ref) #t)
                           (equal? (p? struct:a) #t)
                           (equal? (p? a-instance) #t)
                           (equal? (p? struct:plain) #f)
                           (equal? (p? plain-instance) #f)
                           (equal? (p-ref struct:a) 8)
                           (equal? (p-ref a-instance) 8)
                           (equal? (p-ref struct:plain (lambda () fallback-symbol)) fallback-symbol)
                           (equal? (p-ref plain-instance (lambda () 'instance-fallback)) 'instance-fallback)
                           (equal? (q? struct:c) #t)
                           (equal? (q? c-instance) #t)
                           (equal? (q-ref struct:c) 9)
                           (equal? (q-ref c-instance) 9)
                           (equal? (p? struct:c) #t)
                           (equal? (p? c-instance) #t)
                           (equal? (p-ref struct:c) 3)
                           (equal? (p-ref c-instance) 3))))

              (list "prop:procedure/builtin"
                    (list (equal? (struct-type-property? prop:procedure) #t)))

              (list "prop:procedure/apply-field"
                    (let ()
                      (struct callable (f) #:property prop:procedure 0)
                      (define wrapped (callable (lambda (x y) (+ x y))))
                      (and (procedure? wrapped)
                           (equal? (wrapped 2 3) 5))))
              
              (list "prop:procedure/apply-proc"
                    (let ()
                      (struct counter (n)
                        #:mutable
                        #:property prop:procedure
                        (lambda (self delta)
                          (set-counter-n! self (+ (counter-n self) delta))
                          (counter-n self)))
                      (define c (counter 10))
                      (and (equal? (c 5) 15)
                           (equal? (c -2) 13))))
              
              ))

       (list "5.1 Structure Type Properties"
             (list
              (list "struct-type-authentic?/basic"
                    (let ()
                      (define-values (plain-type make-plain plain? plain-ref plain-set!)
                        (make-struct-type 'plain #f 0 0))
                      (define-values (auth-type make-auth auth? auth-ref auth-set!)
                        (make-struct-type 'auth #f 0 0 #f (list (cons prop:authentic #t))))
                      (and (equal? (struct-type-authentic? plain-type) #f)
                           (equal? (struct-type-authentic? auth-type) #t))))

              (list "struct-type-authentic?/value-ignored"
                    (let ()
                      (define-values (auth-type make-auth auth? auth-ref auth-set!)
                        (make-struct-type 'authv #f 0 0 #f (list (cons prop:authentic 'ignored))))
                      (equal? (struct-type-authentic? auth-type) #t)))

              (list "custom-write?/struct-and-type"
                    (let ()
                      (struct fancy (label)
                        #:property prop:custom-write
                        (lambda (self port mode)
                          (if (eq? mode #f)
                              (write-string "disp:" port)
                              (write-string "write:" port))
                          (write-string (fancy-label self) port)))
                      (define fancy-value (fancy "ok"))
                      (and (equal? (custom-write? fancy-value) #t)
                           (equal? (custom-write? fancy)       #f)
                           (equal? (custom-write? 'plain)      #f))))))

       
       
       (list "5.6 Structure Utilities"
             (list
              (list "struct->list/basic"
                    (let ()
                      (struct point (x y) #:transparent)
                      (let ([p (point 1 'two)])
                        (equal? (struct->list p) '(1 two)))))

              (list "struct->list/on-opaque return-false"
                    (equal? (struct->list 'not-a-struct 'return-false) #f))

              (list "struct->list/on-opaque skip"
                    (equal? (struct->list 'not-a-struct 'skip) '()))

              (list "struct->vector/basic"
                    (let ()
                      (struct vect-point (x y) #:transparent)
                      (let ([p (vect-point 1 'two)])
                        (equal? (struct->vector p)
                                '#(struct:vect-point 1 two)))))

              (list "struct->vector/optional"
                    (let ()
                      (struct vect-point2 (x y) #:transparent)
                      (let ([p (vect-point2 3 4)])
                        (equal? (struct->vector p 'placeholder)
                                '#(struct:vect-point2 3 4))))))))


 (list "13. Input and Output"

       (list "13.1 Ports"
             (list
              (list "eof-object?"
                    (and (equal? (eof-object? eof) #t)
                         (equal? (eof-object? '()) #f)
                         (equal? (eof-object? 123) #f)))

              (list "port?/classification"
                     (let* ([in  (open-input-string "data")]
                            [out (open-output-string)])
                       (and (equal? (port? in) #t)
                            (equal? (port? out) #t)
                            (equal? (port? 42) #f))))

               (list "input-port?/only-input"
                     (let* ([in  (open-input-bytes (bytes 1 2 3))]
                            [out (open-output-string)])
                       (and (equal? (input-port? in) #t)
                            (equal? (input-port? out) #f)
                            (equal? (input-port? 'not-a-port) #f))))

               (list "output-port?/after-write"
                     (let ([out (open-output-string)])
                       (write-string "ok" out)
                       (and (equal? (output-port? out) #t)
                            (equal? (output-port? (open-input-string "x")) #f)
                            (equal? (output-port? #f) #f))))

               (list "make-input-port/read-in-proc"
                    (let* ([data (bytes 80 81 82)]
                           [index 0]
                           [port (make-input-port
                                  'reader
                                  (lambda (dest)
                                    (if (< index 3)
                                        (begin
                                          (bytes-set! dest 0 (bytes-ref data index))
                                          (set! index (add1 index))
                                          1)
                                        eof))
                                  #f
                                  (lambda () (void)))])
                      (and (equal? (read-byte port) 80)
                           (equal? (read-byte port) 81)
                           (equal? (read-byte port) 82)
                           (eof-object? (read-byte port)))))

              (list "make-input-port/peek-in-proc"
                    (let* ([data (bytes 90 91 92)]
                           [index 0]
                           [port (make-input-port
                                  'peeker
                                  (lambda (dest)
                                    (if (< index 3)
                                        (begin
                                          (bytes-set! dest 0 (bytes-ref data index))
                                          (set! index (add1 index))
                                          1)
                                        eof))
                                  (lambda (dest skip evt)
                                    (let ([pos (+ index skip)])
                                      (if (< pos 3)
                                          (begin
                                            (bytes-set! dest 0 (bytes-ref data pos))
                                            1)
                                          eof)))
                                  (lambda () (void)))])
                      (and (equal? (peek-byte port) 90)
                           (equal? (peek-byte port) 90)
                           (equal? (read-byte port) 90)
                           (equal? (peek-byte port) 91)
                           (equal? (read-byte port) 91)
                           (equal? (read-byte port) 92)
                           (eof-object? (peek-byte port))
                           (eof-object? (read-byte port)))))
              (list "read-char/custom-port"
                    (let* ([base (open-input-string "λA")]
                           [port (make-input-port 'proxy base base (lambda () (void)))]
                           [first (read-char port)]
                           [second (read-char port)]
                           [third (read-char port)])
                      (and (equal? first #\λ)
                           (equal? second #\A)
                           (eof-object? third))))

              (list "read-bytes!/custom-port"
                    (let* ([base (open-input-bytes (bytes 1 2 3 4 5))]
                           [port (make-input-port 'proxy base base (lambda () (void)))]
                           [buffer (make-bytes 5 0)]
                           [count  (read-bytes! buffer port 1 4)]
                           [next   (read-byte port)])
                      (and (equal? count 3)
                           (equal? buffer (bytes 0 1 2 3 0))
                           (equal? next 4))))

              (list "read-string!/custom-port"
                    (let* ([base (open-input-string "Racket")]
                           [port (make-input-port 'proxy base base (lambda () (void)))]
                           [buffer (make-string 5 #\_)]
                           [count  (read-string! buffer port 1 4)]
                           [rest   (read-char port)])
                      (and (equal? count 3)
                           (equal? buffer (string #\_ #\R #\a #\c #\_))
                           (equal? rest #\k))))

              (list "read-bytes/custom-port"
                    (let* ([base (open-input-bytes (bytes 10 11 12 13))]
                           [port (make-input-port 'proxy base base (lambda () (void)))]
                           [chunk (read-bytes 3 port)]
                           [tail  (read-bytes 2 port)]
                           [final (read-bytes 1 port)])
                      (and (equal? chunk (bytes 10 11 12))
                           (equal? tail (bytes 13))
                           (eof-object? final))))

              (list "read-string/custom-port"
                    (let* ([base (open-input-string "hello")]
                           [port (make-input-port 'proxy base base (lambda () (void)))]
                           [chunk (read-string 4 port)]
                           [tail  (read-string 4 port)]
                           [final (read-string 1 port)])
                      (and (equal? chunk "hell")
                           (equal? tail "o")
                           (eof-object? final))))

              (list "read-line/custom-port"
                    (let* ([base1 (open-input-string "one\ntwo")]
                           [port1 (make-input-port 'proxy base1 base1 (lambda () (void)))]
                           [first (read-line port1)]
                           [second (read-line port1)]
                           [third (read-line port1)]
                           [base2 (open-input-string "carriage\r\nreturn")]
                           [port2 (make-input-port 'proxy base2 base2 (lambda () (void)))]
                           [crlf  (read-line port2 'return-linefeed)]
                           [rest  (read-line port2 'return-linefeed)]
                           [done  (read-line port2 'return-linefeed)])
                      (and (equal? first "one")
                           (equal? second "two")
                           (eof-object? third)
                           (equal? crlf "carriage")
                           (equal? rest "return")
                           (eof-object? done))))
              
               ))

       
       (list "13.3 Byte and String Input"
             (list
              (list "read-byte/basic"
                    (let ([port (open-input-bytes (bytes 65 66))])
                      (and (equal? (read-byte port) 65)
                           (equal? (read-byte port) 66)
                           (eof-object? (read-byte port))
                           (eof-object? (read-byte port)))))
              
              (list "read-byte/location"
                    (let* ([port (open-input-bytes (bytes 65 10 9))]
                           [loc  (lambda ()
                                   (let-values ([(line col pos) (port-next-location port)])
                                     (list line col pos)))])
                      (and (equal? (loc) '(1 0 1))
                           (equal? (read-byte port) 65)
                           (equal? (loc) '(1 1 2))
                           (equal? (read-byte port) 10)
                           (equal? (loc) '(2 0 3))
                           (equal? (read-byte port) 9)
                           (equal? (loc) '(2 8 4))
                           (eof-object? (read-byte port)))))

              (list "read-char/basic"
                    (let ([port (open-input-string "AB")])
                      (and (equal? (read-char port) #\A)
                           (equal? (read-char port) #\B)
                           (eof-object? (read-char port))
                           (eof-object? (read-char port)))))

              (list "read-char/utf8"
                    (let ([port (open-input-string "λx")])
                      (and (equal? (read-char port) #\λ)
                           (equal? (read-char port) #\x)
                           (eof-object? (read-char port)))))

              (list "read-char/location"
                    (let* ([port (open-input-string "A\n\t")]
                           [loc  (lambda ()
                                   (let-values ([(line col pos) (port-next-location port)])
                                     (list line col pos)))])
                      (and (equal? (loc) '(1 0 1))
                           (equal? (read-char port) #\A)
                           (equal? (loc) '(1 1 2))
                           (equal? (read-char port) #\newline)
                           (equal? (loc) '(2 0 3))
                           (equal? (read-char port) #\tab)
                           (equal? (loc) '(2 8 4))
                           (eof-object? (read-char port)))))

              (list "read-bytes!/basic"
                    (let* ([buffer (make-bytes 6 (char->integer #\_))]
                           [port   (open-input-bytes (bytes 65 66 67 68))]
                           [count  (read-bytes! buffer port 1 5)])
                      (and (equal? count 4)
                           (equal? buffer (bytes 95 65 66 67 68 95))
                           (eof-object? (read-byte port)))))
              
              (list "read-bytes!/eof"
                    (let* ([buffer (make-bytes 4 0)]
                           [port   (open-input-bytes (bytes 1 2))]
                           [first  (read-bytes! buffer port 0 4)]
                           [second (read-bytes! buffer port 0 4)])
                      (and (equal? first 2)
                           (equal? buffer (bytes 1 2 0 0))
                           (eof-object? second))))

              (list "read-bytes-avail!/basic"
                    (let* ([buffer (make-bytes 6 (char->integer #\_))]
                           [port   (open-input-bytes (bytes 65 66 67 68))]
                           [count  (read-bytes-avail! buffer port 1 5)])
                      (and (equal? count 4)
                           (equal? buffer (bytes 95 65 66 67 68 95))
                           (eof-object? (read-byte port)))))

              (list "read-bytes-avail!/eof"
                    (let* ([buffer (make-bytes 4 0)]
                           [port   (open-input-bytes (bytes 1 2))]
                           [first  (read-bytes-avail! buffer port 0 4)]
                           [second (read-bytes-avail! buffer port 0 4)])
                      (and (equal? first 2)
                           (equal? buffer (bytes 1 2 0 0))
                           (eof-object? second))))

              (list "read-bytes-avail!*/basic"
                    (let* ([buffer (make-bytes 5 (char->integer #\_))]
                           [port   (open-input-bytes (bytes 70 71 72))]
                           [count  (read-bytes-avail!* buffer port 1 4)])
                      (and (equal? count 3)
                           (equal? buffer (bytes 95 70 71 72 95))
                           (eof-object? (read-bytes-avail!* buffer port 0 4)))))

              (list "read-bytes-avail!*/zero"
                    (let* ([buffer (make-bytes 4 0)]
                           [port   (open-input-bytes (bytes 1 2 3))]
                           [empty  (read-bytes-avail!* buffer port 2 2)]
                           [count  (read-bytes-avail!* buffer port 0 3)])
                      (and (equal? empty 0)
                           (equal? count 3)
                           (equal? buffer (bytes 1 2 3 0)))))

              (list "read-string!/basic"
                    (let* ([buffer (make-string 6 #\_)]
                           [port   (open-input-string "ABCD")]
                           [count  (read-string! buffer port 1 5)])
                      (and (equal? count 4)
                           (equal? buffer "_ABCD_")
                           (eof-object? (read-char port)))))

              (list "read-string!/eof"
                    (let* ([buffer (make-string 4 #\_)]
                           [port   (open-input-string "hi")]
                           [first  (read-string! buffer port 0 4)]
                           [second (read-string! buffer port 0 4)])
                      (and (equal? first 2)
                           (equal? buffer "hi__")
                           (eof-object? second))))

             (list "read-bytes/basic"
                    (let* ([port (open-input-bytes (bytes 65 66 67 68))]
                           [chunk (read-bytes 3 port)]
                           [tail  (read-bytes 1 port)]
                           [next  (read-bytes 1 port)])
                      (and (equal? chunk (bytes 65 66 67))
                           (equal? tail (bytes 68))
                           (eof-object? next))))

              (list "read-bytes/partial"
                    (let* ([port (open-input-bytes (bytes 1 2))]
                           [first  (read-bytes 4 port)]
                           [second (read-bytes 1 port)])
                      (and (equal? first (bytes 1 2))
                           (eof-object? second))))

              (list "read-bytes/zero"
                    (let* ([port (open-input-bytes (bytes 7 8 9))]
                           [empty (read-bytes 0 port)]
                           [rest  (read-bytes 3 port)])
                      (and (equal? empty (bytes))
                           (equal? rest (bytes 7 8 9)))))

              (list "read-string/basic"
                    (let* ([port (open-input-string "webRacket")]
                           [chunk (read-string 3 port)]
                           [tail  (read-string 2 port)]
                           [more  (read-string 4 port)]
                           [done  (read-string 1 port)])
                      (and (equal? chunk "web")
                           (equal? tail "Ra")
                           (equal? more "cket")
                           (eof-object? done))))

              (list "read-string/partial"
                    (let* ([port (open-input-string "λx")]
                           [first  (read-string 5 port)]
                           [second (read-string 1 port)])
                      (and (equal? first "λx")
                           (eof-object? second))))

              (list "read-string/zero"
                    (let* ([port (open-input-string "data")]
                           [empty (read-string 0 port)]
                           [rest  (read-string 4 port)])
                      (and (equal? empty "")
                           (equal? rest "data"))))

              (list "byte-ready?/basic"
                    (let ([port (open-input-bytes (bytes 65 66))])
                      (and (equal? (byte-ready? port) #t)
                           (equal? (read-byte port) 65)
                           (equal? (byte-ready? port) #t)
                           (equal? (read-byte port) 66)
                           (equal? (byte-ready? port) #f))))

              (list "char-ready?/basic"
                    (let ([port (open-input-string "λx")])
                      (and (equal? (char-ready? port) #t)
                           (equal? (read-char port) #\λ)
                           (equal? (char-ready? port) #t)
                           (equal? (read-char port) #\x)
                           (equal? (char-ready? port) #f))))

              (list "read-line/basic"
                    (let* ([port   (open-input-string "alpha\nbeta\n")]
                           [first  (read-line port)]
                           [second (read-line port)]
                           [third  (read-line port)])
                      (and (equal? first "alpha")
                           (equal? second "beta")
                           (eof-object? third))))

              (list "read-line/return"
                    (let* ([port (open-input-string "a\rb\rc")]
                           [first   (read-line port 'return)]
                           [second  (read-line port 'return)]
                           [third   (read-line port 'return)]
                           [fourth  (read-line port 'return)])
                      (and (equal? first "a")
                           (equal? second "b")
                           (equal? third "c")
                           (eof-object? fourth))))

              (list "read-line/return-linefeed"
                    (let* ([port (open-input-string "x\r\ny\r\nz")]
                           [first   (read-line port 'return-linefeed)]
                           [second  (read-line port 'return-linefeed)]
                           [third   (read-line port 'return-linefeed)]
                           [fourth  (read-line port 'return-linefeed)])
                      (and (equal? first "x")
                           (equal? second "y")
                           (equal? third "z")
                           (eof-object? fourth))))

              (list "read-line/any"
                    (let* ([port (open-input-string "x\r\ny\nz")]
                           [first   (read-line port 'any)]
                           [second  (read-line port 'any)]
                           [third   (read-line port 'any)]
                           [fourth  (read-line port 'any)])
                      (and (equal? first "x")
                           (equal? second "y")
                           (equal? third "z")
                           (eof-object? fourth))))

              (list "read-line/any-one"
                    (let* ([port (open-input-string "x\r\ny\r\n")]
                           [first   (read-line port 'any-one)]
                           [second  (read-line port 'any-one)]
                           [third   (read-line port 'any-one)]
                           [fourth  (read-line port 'any-one)]
                           [fifth   (read-line port 'any-one)])
                      (list (list first "x")
                            (list second "")
                            (list third "y")
                            (list fourth "")
                            (list (eof-object? fifth) #t))))

              (list "peek-bytes!/basic"
                    (let* ([buffer (make-bytes 6 (char->integer #\_))]
                           [port   (open-input-bytes (bytes 65 66 67 68))]
                           [count  (peek-bytes! buffer 0 port 1 5)]
                           [a      (read-byte port)]
                           [b      (read-byte port)]
                           [c      (read-byte port)]
                           [d      (read-byte port)]
                           [e      (read-byte port)])
                      (and (equal? count 4)
                           (equal? buffer (bytes 95 65 66 67 68 95))
                           (equal? a 65)
                           (equal? b 66)
                           (equal? c 67)
                           (equal? d 68)
                           (eof-object? e))))

              (list "peek-bytes!/skip"
                    (let* ([buffer (make-bytes 3 0)]
                           [port   (open-input-bytes (bytes 1 2 3 4))]
                           [count  (peek-bytes! buffer 2 port 0 3)]
                           [first  (read-byte port)])
                      (and (equal? count 2)
                           (equal? buffer (bytes 3 4 0))
                           (equal? first 1))))

              (list "peek-bytes!/eof"
                    (let* ([buffer (make-bytes 1 0)]
                           [port   (open-input-bytes (bytes 1 2))]
                           [result (peek-bytes! buffer 5 port 0 1)]
                           [first  (read-byte port)])
                      (and (eof-object? result)
                           (equal? buffer (bytes 0))
                           (equal? first 1))))

              (list "peek-string!/basic"
                    (let* ([buffer (make-string 6 #\_)]
                           [port   (open-input-string "ABCD")]
                           [count  (peek-string! buffer 0 port 1 5)]
                           [a      (read-char port)]
                           [b      (read-char port)]
                           [c      (read-char port)]
                           [d      (read-char port)]
                           [e      (read-char port)])
                      (and (equal? count 4)
                           (equal? buffer "_ABCD_")
                           (char=? a #\A)
                           (char=? b #\B)
                           (char=? c #\C)
                           (char=? d #\D)
                           (eof-object? e))))

              (list "peek-string!/skip"
                    (let* ([buffer (make-string 2 #\_)]
                           [port   (open-input-string "λx")]
                           [count  (peek-string! buffer 1 port 0 1)]
                           [first  (read-char port)])
                      (and (equal? count 1)
                           (equal? buffer "x_")
                           (char=? first #\λ))))

              (list "peek-string!/eof"
                    (let* ([buffer (make-string 1 #\_)]
                           [port   (open-input-string "λ")]
                           [result (peek-string! buffer 1 port 0 1)]
                           [first  (read-char port)])
                      (and (eof-object? result)
                           (equal? buffer "_")
                           (char=? first #\λ))))

              (list "peek-byte/basic"
                    (let* ([port       (open-input-bytes (bytes 65 66))]
                           [first      (peek-byte port)]
                           [second     (peek-byte port)]
                           [consumed   (read-byte port)]
                           [after-read (peek-byte port)])
                      (and (equal? first 65)
                           (equal? second 65)
                           (equal? consumed 65)
                           (equal? after-read 66))))

              (list "peek-byte/skip"
                    (let* ([port   (open-input-bytes (bytes 1 2 3))]
                           [value  (peek-byte port 1)]
                           [first  (read-byte port)])
                      (and (equal? value 2)
                           (equal? first 1))))

              (list "peek-byte/eof"
                    (let* ([port   (open-input-bytes (bytes 1 2))]
                           [value  (peek-byte port 5)]
                           [first  (read-byte port)])
                      (and (eof-object? value)
                           (equal? first 1))))

              (list "peek-char/basic"
                    (let* ([port       (open-input-string "λx")]
                           [first      (peek-char port)]
                           [second     (peek-char port)]
                           [consumed   (read-char port)]
                           [after-read (peek-char port)])
                      (and (char=? first #\λ)
                           (char=? second #\λ)
                           (char=? consumed #\λ)
                           (char=? after-read #\x))))

              (list "peek-char/skip"
                    (let* ([port   (open-input-string "λx")]
                           [value  (peek-char port 1)]
                           [first  (read-char port)])
                      (and (char=? value #\x)
                           (char=? first #\λ))))

              (list "peek-char/eof"
                    (let* ([port   (open-input-string "λ")]
                           [value  (peek-char port 1)]
                           [first  (read-char port)])
                      (and (eof-object? value)
                           (char=? first #\λ))))

              (list "peek-bytes/basic"
                    (let* ([port  (open-input-bytes (bytes 65 66 67 68))]
                           [chunk (peek-bytes 3 0 port)]
                           [a     (read-byte port)]
                           [b     (read-byte port)]
                           [c     (read-byte port)]
                           [d     (read-byte port)]
                           [e     (read-byte port)])
                      (and (equal? chunk (bytes 65 66 67))
                           (equal? a 65)
                           (equal? b 66)
                           (equal? c 67)
                           (equal? d 68)
                           (eof-object? e))))

              (list "peek-bytes/skip"
                    (let* ([port  (open-input-bytes (bytes 1 2 3 4))]
                           [chunk (peek-bytes 2 1 port)]
                           [first (read-byte port)]
                           [rest  (read-bytes 3 port)])
                      (and (equal? chunk (bytes 2 3))
                           (equal? first 1)
                           (equal? rest (bytes 2 3 4)))))

              (list "peek-bytes/eof"
                    (let* ([port   (open-input-bytes (bytes 9 10))]
                           [result (peek-bytes 1 5 port)]
                           [first  (read-byte port)]
                           [second (read-byte port)])
                      (and (eof-object? result)
                           (equal? first 9)
                           (equal? second 10))))

              (list "peek-bytes-avail!/basic"
                    (let* ([buffer (make-bytes 4 0)]
                           [port   (open-input-bytes (bytes 1 2 3))]
                           [count  (peek-bytes-avail! buffer 0 #f port 0 3)]
                           [a      (read-byte port)]
                           [b      (read-byte port)]
                           [c      (read-byte port)]
                           [d      (read-byte port)])
                      (and (equal? count 3)
                           (equal? buffer (bytes 1 2 3 0))
                           (equal? a 1)
                           (equal? b 2)
                           (equal? c 3)
                           (eof-object? d))))

              (list "peek-bytes-avail!/skip"
                    (let* ([buffer (make-bytes 2 0)]
                           [port   (open-input-bytes (bytes 9 8 7))]
                           [count  (peek-bytes-avail! buffer 1 #f port 0 2)]
                           [first  (read-byte port)]
                           [rest   (read-bytes 2 port)])
                      (and (equal? count 2)
                           (equal? buffer (bytes 8 7))
                           (equal? first 9)
                           (equal? rest (bytes 8 7)))))

              (list "peek-bytes-avail!*/zero"
                    (let* ([buffer (make-bytes 1 111)]
                           [port   (open-input-bytes (bytes 5 6))]
                           [count  (peek-bytes-avail!* buffer 0 #f port 0 0)]
                           [peeked (peek-byte port)])
                      (and (equal? count 0)
                           (equal? buffer (bytes 111))
                           (equal? peeked 5))))

              (list "peek-string/basic"
                    (let* ([port  (open-input-string "webRacket")]
                           [chunk (peek-string 3 0 port)]
                           [w     (read-char port)]
                           [e     (read-char port)]
                           [b     (read-char port)]
                           [rest  (read-string 6 port)])
                      (and (equal? chunk "web")
                           (equal? w #\w)
                           (equal? e #\e)
                           (equal? b #\b)
                           (equal? rest "Racket"))))

              (list "peek-string/skip"
                    (let* ([port  (open-input-string "abcd")]
                           [chunk (peek-string 2 1 port)]
                           [first (read-char port)]
                           [rest  (read-string 3 port)])
                      (and (equal? chunk "bc")
                           (equal? first #\a)
                           (equal? rest "bcd"))))

              (list "peek-string/eof"
                    (let* ([port   (open-input-string "hi")]
                           [result (peek-string 1 5 port)]
                           [first  (read-char port)]
                           [second (read-char port)])
                      (and (eof-object? result)
                           (equal? first  #\h)
                           (equal? second #\i))))
              
              (list "read-byte/custom-port"
                    (let* ([base (open-input-bytes (bytes 70 71))]
                           [port (make-input-port 'proxy base base (lambda () (void)))])
                      (and (equal? (read-byte port) 70)
                           (equal? (peek-byte port) 71)
                           (equal? (read-byte port) 71)
                           (eof-object? (read-byte port))
                           (eof-object? (peek-byte port)))))              
             ))
       
       (list "13.3 Byte and String Output"
             (list
              (list "write-byte/resizing"
                    (let* ([port (open-output-bytes)]
                           [data (build-list 40 (lambda (i) (modulo (+ 60 i) 256)))]
                           [expected (apply bytes data)])
                      (for-each (lambda (b) (write-byte b port)) data)
                      (equal? (get-output-bytes port) expected)))

              (list "open-output-string/write-char"
                    (let ([port (open-output-string)])
                      (and (void? (write-char #\A port))
                           (equal? (get-output-string port) "A"))))
              
              (list "write-char/utf8"
                    (let ([port (open-output-string)])
                      (and (void? (write-char #\λ port))
                           (equal? (get-output-string port) "λ"))))

              (list "open-output-string/write-byte"
                    (let ([port (open-output-string)])
                      (and (void? (write-byte 65 port))
                           (equal? (bytes->string/utf-8 (get-output-bytes port)) "A"))))

              (list "open-output-bytes/get-output-bytes"
                    (let ([port (open-output-bytes)])
                      (and (equal? (get-output-bytes port) (bytes))
                           (void? (write-byte 65 port))
                           (void? (write-byte 66 port))
                           (let ([bs (get-output-bytes port)])
                             (and (equal? bs (bytes 65 66))
                                  (equal? (immutable? bs) #t))))))

              (list "newline"
                    (let ([port (open-output-string)])
                      (and (void? (newline port))
                           (equal? (get-output-string port) "\n"))))

              (list "write-bytes/basic"
                    (let* ([port (open-output-bytes)]
                           [data (bytes 65 66 67)]
                           [count (write-bytes data port)])
                      (and (equal? count 3)
                           (equal? (get-output-bytes port) data))))

              (list "write-bytes/with-range"
                    (let* ([port (open-output-bytes)]
                           [data (bytes 70 71 72 73)]
                           [count (write-bytes data port 1 3)])
                      (and (equal? count 2)
                           (equal? (get-output-bytes port)
                                   (subbytes data 1 3)))))

              (list "write-string/basic"
                    (let* ([port (open-output-string)]
                           [count (write-string "hello" port)])
                      (and (equal? count 5)
                           (equal? (get-output-string port) "hello"))))

              (list "write-string/with-range"
                    (let* ([port  (open-output-string)]
                           [count (write-string "héllo" port 1 4)])
                      (and (equal? count 3)
                           (equal? (get-output-string port) "éll"))))
              ))


       
       (list "13.5 Writing"
             (list
              (list "custom-write-accessor/modes"
                    (let ()
                      (struct printable (text)
                        #:property prop:custom-write
                        (lambda (self port mode)
                          (write-string (if (eq? mode #f) "[" "<") port)
                          (write-string (printable-text self) port)
                          (write-string (if (eq? mode #f) "]" ">") port)))
                      (define value (printable "data"))
                      (define proc (custom-write-accessor value))
                      (define display-result
                        (call-with-output-string
                         (lambda (p)
                           (proc value p #f))))
                      (define write-result
                        (call-with-output-string
                         (lambda (p)
                           (proc value p #t))))
                      (and (equal? display-result "[data]")
                           (equal? write-result "<data>"))))))

       (list "13.7 String Ports"
             (list
              (list "string-port?"
                    (let ([port (open-output-bytes)])
                      (and (equal? (string-port? port) #t)
                           (equal? (string-port? #f) #f)
                           (equal? (string-port? (bytes 1 2)) #f))))

              (list "open-input-bytes default"
                    (string-port? (open-input-bytes (bytes 1 2 3))))

              (list "open-input-bytes custom name"
                    (string-port? (open-input-bytes (bytes 1 2 3) 'source)))

              (list "open-input-string default"
                    (string-port? (open-input-string "abc")))

              (list "open-input-string custom name"
                    (string-port? (open-input-string "abc" 'source)))

              (list "open-output-string default"
                    (let ([port (open-output-string)])
                      (and (string-port? port)
                           (equal? (get-output-bytes port) (bytes)))))

              (list "open-output-string custom name"
                    (string-port? (open-output-string 'sink)))

              (list "open-output-bytes default"
                    (let ([port (open-output-bytes)])
                      (and (string-port? port)
                           (equal? (get-output-bytes port) (bytes)))))

              (list "open-output-bytes custom name"
                    (string-port? (open-output-bytes 'sink)))

              (list "get-output-string"
                    (let* ([port (open-output-bytes)]
                           [data (bytes 72 101 108 108 111 32 206 187)])
                      (for-each (lambda (b) (write-byte b port)) (bytes->list data))
                      (equal? (get-output-string port) "Hello λ")))

              (list "call-with-output-string/basic"
                    (equal? (call-with-output-string
                             (lambda (p)
                               (write-char #\A p)
                               (write-string "BC" p)))
                            "ABC"))

              (list "call-with-output-string/ignore-result"
                    (let ([result (call-with-output-string
                                   (lambda (p)
                                     (write-string "hi" p)
                                     'done))])
                      (equal? result "hi")))
              
              (list "port-count-lines!"
                    (let ([port (open-input-string "abc")])
                      (void? (port-count-lines! port))))

              (list "port-count-lines!"
                    (let ([port (open-input-string "abc")])
                      (port-count-lines! port)
                      (equal? (port-counts-lines? port) #t)))

              (list "port-next-location"
                    (let* ([port (open-output-bytes)]
                           [loc (lambda ()
                                  (let-values ([(line col pos) (port-next-location port)])
                                    (list line col pos)))])
                      (and (equal? (loc) '(1 0 1))
                           (begin (write-byte 65 port)
                                  (equal? (loc) '(1 1 2)))
                           (begin (write-byte 10 port)
                                  (equal? (loc) '(2 0 3)))
                           (begin (write-byte 9 port)
                                  (equal? (loc) '(2 8 4)))
                           (equal? (port-next-location 42) #f))))))
       )

             (list "10.2 Exceptions"
                   (list
                    (list "unquoted-printing-string basics"
                          (let* ([s "hello"]
                                 [ups (unquoted-printing-string s)])
                            (and (equal? (unquoted-printing-string? ups)      #t)
                                 (equal? (unquoted-printing-string? s)        #f)
                                 (equal? (unquoted-printing-string-value ups) s))))

                    ; Note: display and write are in stdlib, so they
                    ;       are not available here.
                    #;(list "unquoted-printing-string display/write"
                            (let* ([ups (unquoted-printing-string "x\ny")]
                                   [displayed (call-with-output-string
                                               (lambda (p)
                                                 (display ups p)))]
                                   [written (call-with-output-string
                                              (lambda (p)
                                                (write ups p)))])
                              (and (equal? displayed "x\ny")
                                   (equal? written "x\ny"))))))

 (list "10.2 Exceptions"
       (list
        (list "srcloc basics"
              (let () 
                (let* ([loc   (make-srcloc 'src 3 0 10 5)]
                       [loc2  (srcloc "file" #f #f #f #f)]
                       [loc3  (make-srcloc (bytes->path #"foo") 3 0 10 5)])
                  (and (equal? (srcloc? loc)          #t)
                       (equal? (srcloc? 42)           #f)
                       (equal? (srcloc-source loc)    'src)
                       (equal? (srcloc-line loc)      3)
                       (equal? (srcloc-column loc)    0)
                       (equal? (srcloc-position loc)  10)
                       (equal? (srcloc-span loc)      5)
                       (equal? (srcloc->string loc)   "src:3:0")
                       (equal? (srcloc->string loc3)  "foo:3:0")
                       (equal? (srcloc? loc2)         #t)
                       (equal? (srcloc-line loc2)     #f)
                       (equal? (srcloc-column loc2)   #f)
                       (equal? (srcloc-position loc2) #f)
                       (equal? (srcloc-span loc2)     #f)
                       (equal? (srcloc->string loc2) "file")))))

        (list "with-handlers"
                    (let* ([no-exn-result
                            (with-handlers ([integer? (λ (_n) 'handled)])
                              (+ 1 2))]
                           [caught-result
                            (with-handlers ([integer? (λ (n) (list 'caught n))])
                              (raise 5))]
                           [escalated-result
                            (with-handlers ([list? (λ (lst) lst)])
                              (with-handlers ([integer? (λ (n) (raise (list 'escalated n)))])
                                (raise 9)))]
                           [ordered-result
                            (with-handlers ([symbol?  (λ (_s) 'symbol)]
                                            [integer? (λ (_n) 'integer)])
                              (raise 10))])
                      (and (equal? no-exn-result    3)
                           (equal? caught-result    '(caught 5))
                           (equal? escalated-result '(escalated 9))
                           (equal? ordered-result   'integer))))

        (list "call-with-exception-handler"
              (let* ([normal-result
                      (call-with-exception-handler
                       (λ (_value) 'should-not-run)
                       (λ () 'ok))]
                     [handled-result
                      (with-handlers ([list? (λ (v) v)])
                        (call-with-exception-handler
                         (λ (value) (list 'handled value))
                         (λ () (raise 'boom))))]
                     [nested-result
                      (with-handlers ([list? (λ (v) v)])
                        (call-with-exception-handler
                         (λ (outer-value) (list 'outer outer-value))
                         (λ ()
                           (call-with-exception-handler
                            (λ (inner-value) (list 'inner inner-value))
                            (λ () (raise 'boom))))))])
                (and (equal? normal-result  'ok)
                     (equal? handled-result '(handled boom))
                     (equal? nested-result  '(outer (inner boom))))))
        
        (list "raise-read-error"
              (let* ([basic
                      (with-handlers ([exn:fail:read?
                                       (λ (exn)
                                         (list (exn-message exn)
                                               (map srcloc->string
                                                    (exn:fail:read-srclocs exn))))])
                        (raise-read-error "message" 'src 3 4 5 6))]
                     [with-extra
                      (with-handlers ([exn:fail:read?
                                       (λ (exn)
                                         (map srcloc-source
                                              (exn:fail:read-srclocs exn)))])
                        (raise-read-error
                         "extra"
                         'src 1 2 3 4
                         (list (make-srcloc 'extra 9 8 7 6))))])
                (and (equal? basic (list "message" '("src:3:4")))
                     (equal? with-extra '(src extra)))))

        (list "raise-read-eof-error"
              (let* ([captured
                      (with-handlers ([exn:fail:read:eof?
                                       (λ (exn)
                                         (list (exn-message exn)
                                               (map srcloc->string
                                                    (exn:fail:read-srclocs exn))))])
                        (raise-read-eof-error "EOF" 'src 5 6 7 8))]
                     [msg  (car captured)]
                     [locs (cadr captured)])
                (and (equal? msg "EOF")
                     (equal? locs '("src:5:6")))))
        
        (list "exn constructors"
              (let* ([base (exn "message" #f)]
                     [made (make-exn "other" #f)]
                     [fail (exn:fail "boom" #f)]
                     [made-fail (make-exn:fail "kaboom" #f)])
                (and (equal? (exn? base) #t)
                     (string=? (exn-message base) "message")
                     (eq? (exn-continuation-marks base) #f)
                     (equal? (exn? made) #t)
                     (string=? (exn-message made) "other")
                     (eq? (exn-continuation-marks made) #f)
                     (equal? (exn:fail? fail) #t)
                     (equal? (exn? fail) #t)
                     (string=? (exn-message fail) "boom")
                     (eq? (exn-continuation-marks fail) #f)
                     (equal? (exn:fail? made-fail) #t)
                     (equal? (exn? made-fail) #t)
                     (string=? (exn-message made-fail) "kaboom")
                     (eq? (exn-continuation-marks made-fail) #f))))

        (list "exn:fail:contract structures"
              (let* ([marks         '()]
                     [contract      (exn:fail:contract "contract" marks)]
                     [contract-make (make-exn:fail:contract "made" marks)]
                     [arity         (exn:fail:contract:arity "arity" marks)]
                     [arity-make    (make-exn:fail:contract:arity "made-arity" marks)]
                     [divide        (exn:fail:contract:divide-by-zero "divide" marks)]
                     [divide-make   (make-exn:fail:contract:divide-by-zero "made-divide" marks)]
                     [non-fixnum    (exn:fail:contract:non-fixnum-result "non-fixnum" marks)]
                     [non-fixnum-make (make-exn:fail:contract:non-fixnum-result "made-non-fixnum" marks)]
                     [variable      (exn:fail:contract:variable "variable" marks 'id)]
                     [variable-make (make-exn:fail:contract:variable "made-variable" marks 'made-id)])
                (and (equal? (exn:fail? contract) #t)
                     (equal? (exn:fail:contract? contract) #t)
                     (equal? (exn:fail:contract? arity) #t)
                     (equal? (exn:fail:contract? divide) #t)
                     (equal? (exn:fail:contract? non-fixnum) #t)
                     (equal? (exn:fail:contract? variable) #t)
                     (equal? (exn:fail:contract? contract-make) #t)
                     (equal? (exn:fail:contract? arity-make) #t)
                     (equal? (exn:fail:contract? divide-make) #t)
                     (equal? (exn:fail:contract? non-fixnum-make) #t)
                     (equal? (exn:fail:contract? variable-make) #t)
                     (equal? (exn:fail:contract:arity? contract) #f)
                     (equal? (exn:fail:contract:arity? arity) #t)
                     (equal? (exn:fail:contract:arity? arity-make) #t)
                     (equal? (exn:fail:contract:divide-by-zero? divide) #t)
                     (equal? (exn:fail:contract:divide-by-zero? divide-make) #t)
                     (equal? (exn:fail:contract:non-fixnum-result? non-fixnum) #t)
                     (equal? (exn:fail:contract:non-fixnum-result? non-fixnum-make) #t)
                     (equal? (exn:fail:contract:variable? variable) #t)
                     (equal? (exn:fail:contract:variable? variable-make) #t)
                     (equal? (exn:fail:contract:variable-id variable) 'id)
                     (equal? (exn:fail:contract:variable-id variable-make) 'made-id)
                     (equal? (exn-message contract) "contract")
                     (equal? (exn-message arity) "arity")
                     (equal? (exn-message divide) "divide")
                     (equal? (exn-message non-fixnum) "non-fixnum")
                     (equal? (exn-message variable) "variable"))))

        (list "exn:fail:read structures"
              (let* ([marks         '()]
                     [loc           (srcloc 'src 1 2 3 4)]
                     [locs          (list loc)]
                     [read          (exn:fail:read "read" marks locs)]
                     [read-make     (make-exn:fail:read "made-read" marks locs)]
                     [eof           (exn:fail:read:eof "eof" marks locs)]
                     [eof-make      (make-exn:fail:read:eof "made-eof" marks locs)]
                     [non-char      (exn:fail:read:non-char "non" marks locs)]
                     [non-char-make (make-exn:fail:read:non-char "made-non" marks locs)])
                (and (equal? (exn:fail? read) #t)
                     (equal? (exn:fail:read? read) #t)
                     (equal? (exn:fail:read? read-make) #t)
                     (equal? (exn:fail:read-srclocs read) locs)
                     (equal? (exn:fail:read-srclocs read-make) locs)
                     (equal? (exn:fail:read:eof? eof) #t)
                     (equal? (exn:fail:read:eof? eof-make) #t)
                     (equal? (exn:fail:read:non-char? non-char) #t)
                     (equal? (exn:fail:read:non-char? non-char-make) #t)
                     (equal? (exn-message non-char) "non")
                     (equal? (exn-message eof) "eof"))))

        (list "exn:fail:syntax structures"
              (let* ([marks          '()]
                     [expr           (datum->syntax #f 'expr)]
                     [exprs          (list expr)]
                     [syntax-exn     (exn:fail:syntax "syntax" marks exprs)]
                     [syntax-make    (make-exn:fail:syntax "made-syntax" marks exprs)]
                     [missing        (exn:fail:syntax:missing-module "missing" marks exprs 'module)]
                     [missing-make   (make-exn:fail:syntax:missing-module "made-missing" marks exprs 'module)]
                     [unbound        (exn:fail:syntax:unbound "unbound" marks exprs)]
                     [unbound-make   (make-exn:fail:syntax:unbound "made-unbound" marks exprs)])
                (and (equal? (exn:fail? syntax-exn) #t)
                     (equal? (exn:fail:syntax? syntax-exn) #t)
                     (equal? (exn:fail:syntax? syntax-make) #t)
                     (equal? (exn:fail:syntax-exprs syntax-exn) exprs)
                     (equal? (exn:fail:syntax-exprs syntax-make) exprs)
                     (equal? (exn:fail:syntax? missing) #t)
                     (equal? (exn:fail:syntax:missing-module? missing) #t)
                     (equal? (exn:fail:syntax:missing-module? missing-make) #t)
                     (equal? (exn:fail:syntax:missing-module-path missing) 'module)
                     (equal? (exn:fail:syntax:missing-module-path missing-make) 'module)
                     (equal? (exn:fail:syntax:unbound? unbound) #t)
                     (equal? (exn:fail:syntax:unbound? unbound-make) #t)
                     (equal? (exn-message syntax-exn) "syntax")
                     (equal? (exn-message missing) "missing")
                     (equal? (exn-message unbound) "unbound"))))))
 

 (list "12. Macros"
       (list "12.2 Syntax Object Content"
             (list
              (list "datum->syntax/basic"
                    (let ([stx (datum->syntax #f 'apple)])
                      (and (equal? (syntax? stx) #t)
                           (equal? (syntax-e stx) 'apple))))

              (list "datum->syntax/srcloc"
                    (let* ([loc (make-srcloc 'src 1 1 1 1)]
                           [stx (datum->syntax #f 'x loc)])
                      (equal? (syntax-srcloc stx) loc)))

              (list "datum->syntax/srcloc-from-syntax"
                    (let* ([loc (make-srcloc 'src 2 1 10 3)]
                           [ctx (datum->syntax #f 'dummy loc)]
                           [stx (datum->syntax ctx 'value ctx)])
                      (and (equal? (syntax? stx) #t)
                           (equal? (syntax-srcloc stx) loc))))

              (list "datum->syntax/prop-ignored"
                    (let* ([base (datum->syntax #f 'base)]
                           [stx  (datum->syntax base 'value #f base base)])
                      (equal? (syntax? stx) #t))))
             
             (list "syntax location accessors"
                   (let* ([stx    (datum->syntax #f 'demo)]
                          [no-loc (datum->syntax #f 'placeholder)]
                          [loc    (make-srcloc 'src 3 0 10 5)])
                     (unsafe-struct-set! stx    3 loc)
                     (unsafe-struct-set! no-loc 3 #f)
                     (and (equal? (syntax-source stx)      'src)
                          (equal? (syntax-line stx)         3)
                          (equal? (syntax-column stx)       0)
                          (equal? (syntax-position stx)    10)
                          (equal? (syntax-span stx)         5)
                          (equal? (syntax-source no-loc)   #f)
                          (equal? (syntax-line no-loc)     #f)
                          (equal? (syntax-column no-loc)   #f)
                          (equal? (syntax-position no-loc) #f)
                          (equal? (syntax-span no-loc)     #f))))
             (list "identifier?"
                   (let* ([id-stx     (datum->syntax #f 'pear)]
                          [list-stx   (datum->syntax #f '(pear core))]
                          [number-stx (datum->syntax #f 5)])
                     (and (equal? (identifier? id-stx) #t)
                          (equal? (identifier? list-stx) #f)
                          (equal? (identifier? number-stx) #f)
                          (equal? (identifier? 'pear) #f)
                          (equal? (identifier? 5) #f))))

             (list "syntax->datum/basic"
                    (let* ([stx (datum->syntax #f 'pear)]
                           [datum (syntax->datum stx)])
                      (and (equal? datum 'pear)
                           (equal? (syntax? datum) #f))))

             (list "syntax->datum/nested"
                    (let* ([inner   (datum->syntax #f 'x)]
                           [payload (list inner 'y (datum->syntax #f '(1 2)))]
                           [stx     (datum->syntax #f payload)]
                           [datum   (syntax->datum stx)])
                      (equal? datum '(x y (1 2)))))

              (list "syntax->datum/vector-box"
                    (let* ([vec-stx (datum->syntax #f (vector (datum->syntax #f 'a) 'b))]
                           [box-stx (datum->syntax #f (box (datum->syntax #f 'c)))]
                           [vec     (syntax->datum vec-stx)]
                           [bx      (syntax->datum box-stx)])
                      (and (equal? (vector? vec) #t)
                           (equal? (vector-ref vec 0) 'a)
                           (equal? (vector-ref vec 1) 'b)
                           (equal? (unbox bx) 'c))))

              (list "syntax->list"
                    (let* ([stx   (datum->syntax #f '(a b c))]
                           [lst   (syntax->list stx)]
                           [empty (syntax->list (datum->syntax #f '()))])
                      (and (pair? lst)
                           (equal? (map (λ (stx) (syntax-e stx)) lst) '(a b c))
                           (equal? empty '())
                           (equal? (syntax->list (datum->syntax #f '(a . b))) #f)
                           (equal? (syntax->list (datum->syntax #f 'a))       #f))))
              ))

 (list "14. Reflection and Security"
       
       (list "14.9 Structure Inspectors"
               (list
                (list "object-name/procedure"
                      (let* ([anon      (lambda (x) x)]
                             [renamed   (procedure-rename +       'plus)]
                             [renamed-2 (procedure-rename renamed 'again)])
                        (list (equal? (object-name +)         '+)
                              (equal? (object-name renamed)   'plus)
                              (equal? (object-name renamed-2) 'again)
                              (equal? (object-name anon)      'anon))))

                (list "object-name/structure-default"
                      (let ()
                        (struct plain (value))
                        (let ([instance (plain 42)])
                          (and (equal? (object-name instance) 'plain)
                               (equal? (object-name struct:plain) 'plain)))))

                (list "object-name/structure-property-index"
                      (let ()
                        (struct labelled (name value)
                          #:property prop:object-name 0)
                        (let ([item (labelled 'custom-name 17)])
                          (equal? (object-name item) 'custom-name))))

                (list "object-name/structure-property-proc"
                      (let ()
                        (struct computed (value)
                          #:property prop:object-name (lambda (_self) 'via-prop))
                        (equal? (object-name (computed 'payload)) 'via-prop)))

                (list "object-name/string-port"
                      (let* ([default-port (open-output-string)]
                             [named-port   (open-output-string 'custom-port)]
                             [bytes-port   (open-output-bytes)]
                             [bytes-named  (open-output-bytes 'custom-bytes)])
                        (and (equal? (object-name default-port) 'string)
                             (equal? (object-name named-port) 'custom-port)
                             (equal? (object-name bytes-port) 'string)
                             (equal? (object-name bytes-named) 'custom-bytes))))

                (list "object-name/fallback"
                      (equal? (object-name 42) #f))
                ))

       (list "14.4 Linklets and the Core Compiler"
             (list
              (list "correlated basics"
                    (let* ([payload '(a b)]
                           [srcloc  #(source 10 2 100 5)]
                           [crlt    (datum->correlated payload srcloc)]
                           [bare    (datum->correlated 'plain)])
                      (list (equal? (correlated? crlt)         #t)
                            (equal? (correlated? payload)      #f)
                            (equal? (correlated-source crlt)   'source)
                            (equal? (correlated-line crlt)     10)
                            (equal? (correlated-column crlt)   2)
                            (equal? (correlated-position crlt) 100)
                            (equal? (correlated-span crlt)     5)
                            (equal? (correlated-e crlt)        payload)
                            (equal? (correlated-source bare)   #f)
                            (equal? (correlated-line bare)     #f)
                            (equal? (correlated-column bare)   #f)
                            (equal? (correlated-position bare) #f)
                            (equal? (correlated-span bare)     #f))))

              (list "correlated->datum"
                    (let* ([inner     (datum->correlated 'x #(src 1 0 1 1))]
                           [vec       (vector inner 'y)]
                           [lst       (list inner vec)]
                           [converted (correlated->datum lst)])
                      (list (pair? converted)
                            (equal? (car converted) 'x)
                            (let ([vec-result (cadr converted)])
                              (list (equal? (vector? vec-result) #t)
                                    ; Full Racket doesn't recur through vectors
                                    (equal? (vector-ref vec-result 0) 'x)
                                    (vector-ref vec-result 0)
                                    (equal? (correlated? (vector-ref vec-result 0)) #t)
                                    (list (vector-ref vec-result 1) 'y))))))

              (list "correlated properties"
                    (let* ([base        (datum->correlated 'seed)]
                           [with-tag    (correlated-property base     'tag 'value)]
                           [with-number (correlated-property with-tag 123  'number)]
                           [keys2       (correlated-property-symbol-keys with-number)]
                           [updated     (correlated-property with-tag 'tag 'new)]
                           [removed     (correlated-property updated  'tag #f)]
                           [prop-source (datum->correlated 'copy #f with-tag)]
                           [keys        (correlated-property-symbol-keys with-number)])
                      (list (equal? (correlated-property base        'tag) #f)
                            (equal? (correlated-property with-tag    'tag) 'value)
                            (equal? (correlated-property updated     'tag) 'new)
                            (equal? (correlated-property removed     'tag) #f)
                            (equal? (correlated-property with-number 123)  'number)
                            (equal? (correlated-property prop-source 'tag) 'value)
                            keys2
                            keys
                            (member 'tag keys)
                            (not (equal? (member 'tag keys) #f))
                            (list (member 123 keys) #t)
                            (member 'tag keys)
                            (member 123 keys))))

              (list "correlated properties"
                    (let* ([base        (datum->correlated 'seed)]
                           [with-tag    (correlated-property base     'tag 'value)]
                           [with-number (correlated-property with-tag 123  'number)]
                           [updated     (correlated-property with-tag 'tag 'new)]
                           [removed     (correlated-property updated  'tag #f)]
                           [prop-source (datum->correlated 'copy #f with-tag)]
                           [keys        (correlated-property-symbol-keys with-number)])
                      (and (equal? (correlated-property base        'tag) #f)
                           (equal? (correlated-property with-tag    'tag) 'value)
                           (equal? (correlated-property updated     'tag) 'new)
                           (equal? (correlated-property removed     'tag) #f)
                           (equal? (correlated-property with-number 123)  'number)
                           (equal? (correlated-property prop-source 'tag) 'value)
                           (equal? (and (member 'tag keys) #t)            #t)
                           (equal? (not (member 123  keys)) #t)           #t)))

              (list "make-instance/basic"
                    (let* ([inst   (make-instance 'custom-name 'payload #f 'first 1 'second 'two)]
                           [names  (instance-variable-names inst)]
                           [sorted (sort names symbol<?)])
                      (and (instance? inst)
                           (equal? (instance-name inst) 'custom-name)
                           (equal? (instance-data inst) 'payload)
                           (equal? sorted '(first second))
                           (equal? (instance-variable-value inst 'first) 1)
                           (equal? (instance-variable-value inst 'second) 'two)
                           (equal? (instance-variable-value inst 'missing (lambda () 'fallback))
                                   'fallback))))

              (list "instance variable mutation"
                    (let ([inst (make-instance 'mutable)])
                      (instance-set-variable-value! inst 'x 10)
                      (instance-set-variable-value! inst 'y 20)
                      (instance-set-variable-value! inst 'x 15)
                      (instance-unset-variable! inst 'y)
                      (let ([names (instance-variable-names inst)])
                        (and (equal? (instance-variable-value inst 'x) 15)
                             (equal? (instance-variable-value inst 'y (lambda () 'missing))
                                     'missing)
                             (equal? (sort names symbol<?) '(x))))))

              (list "compiled linklet shares linklet accessors"
                    (let* ([proc (lambda args (void))]
                           [cl   (make-compiled-linklet 'demo '() '() proc)])
                      (and (equal? (linklet? cl)                 #t)
                           (equal? (linklet-name cl)             'demo)
                           (equal? (linklet-import-variables cl) '())
                           (equal? (linklet-export-variables cl) '()))))

              (list "instantiate-linklet/fresh-instance"
                    (let* ([compiled (make-compiled-linklet
                                      'fresh-linklet
                                      '()
                                      '(v)
                                      (lambda (self)
                                        (instance-set-variable-value! self 'v 'fresh)
                                        'ignored))]
                           [result   (instantiate-linklet compiled '())])
                      (and (instance? result)
                           (equal? (instance-name result)              'fresh-linklet)
                           (equal? (instance-variable-value result 'v) 'fresh))))

              (list "instantiate-linklet/target-instance"
                    (let* ([compiled (make-compiled-linklet
                                      'targeted-linklet
                                      '()
                                      '(v)
                                      (lambda (self)
                                        (instance-set-variable-value! self 'v 'target-value)
                                        'body-result))]
                           [target   (make-instance 'target-instance)]
                           [result   (instantiate-linklet compiled '() target)])
                      (and (equal? result 'body-result)
                           (equal? (instance-variable-value target 'v) 'target-value))))

              (list "instantiate-linklet/imports"
                    (let* ([import-inst (make-instance 'provider)]
                           [_           (instance-set-variable-value! import-inst 'shared 42)]
                           [compiled    (make-compiled-linklet
                                         'uses-import
                                         '((shared))
                                         '(result)
                                         (lambda (self imported)
                                           (define shared-value (instance-variable-value imported 'shared))
                                           (instance-set-variable-value! self 'result shared-value)
                                           shared-value))]
                           [target      (make-instance 'target)]
                           [result      (instantiate-linklet compiled (list import-inst) target)])
                      (and (equal? result 42)
                           (equal? (instance-variable-value target 'result) 42))))

              
              
             )))


 
 (list "15. Operating System"
       (list "15. Paths"
             (list
              (list "bytes->path"
                    (let* ([p (bytes->path #"hello")]
                           [u (bytes->path #"unix" 'unix)]
                           [w (bytes->path #"win"  'windows)])
                      (and (path? p)
                           (path? u)
                           (equal? (path->bytes p) #"hello")
                           (equal? (path->bytes u) #"unix")
                           (equal? (path->bytes w) #"win"))))
              (list "path?"
                    (let ([unix-path (bytes->path #"hello")] ; default is unix
                          [win-path  (bytes->path #"world" 'windows)])
                      (and (equal? (path? unix-path)     #t)
                           (equal? (path? win-path)      #f)
                           (equal? (path? "a")           #f)
                           (equal? (path? 'a)            #f)
                           (equal? (path? (bytes 1 2 3)) #f))))
              (list "path-for-some-system?"
                    (let ([unix-path (bytes->path #"hello")]
                          [win-path  (bytes->path #"world" 'windows)])
                      (and (equal? (path-for-some-system? unix-path)     #t)
                           (equal? (path-for-some-system? win-path)      #t)
                           (equal? (path-for-some-system? "a")           #f)
                           (equal? (path-for-some-system? 'a)            #f)
                           (equal? (path-for-some-system? (bytes 1 2 3)) #f))))
              (list "path-string?"
                    (and (equal? (path-string? "hello")        #t)
                         (equal? (path-string? "")             #f)
                         (equal? (path-string? (string #\nul)) #f)
                         (equal? (path-string? 'a)             #f)
                         (equal? (path-string? (bytes 1 2 3))  #f))))))
       
 (list "Checkers"
       (list
        (list "check-list"          (equal? (begin (check-list '(1 2 3)) 'ok)            'ok))
        (list "check-mlist"         (equal? (begin (check-mlist '()) 'ok)                'ok))
        (list "check-range"         (equal? (begin (check-range 0 5 1) 'ok)              'ok))
        (list "check-range-generic" (equal? (begin (check-range-generic 'who 0 1 1) 'ok) 'ok))
        (list "check-naturals"      (equal? (begin (check-naturals 5) 'ok)               'ok))))

(list "FFI"
        (list
         #;(list "external-number->flonum"
                 (let ([e (js-eval "new Number(42)")])
                   (equal? (external-number->flonum e) 42.0))))
        )

 )
