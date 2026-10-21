;; The numbering follows the sections in "The Reference".

;; These basic tests needs to work without the standard library.
;; Compile this file with `--no-stdlib`.
;;
;; Known failing subtests currently kept enabled:
;; - none

(list
 (list "3. Syntactic Forms"       
       (list "3.8 Procedure Expressions: lambda and case-lambda"
             (list
              (list "lambda"
                    (and (equal? (procedure? (lambda (x) (* x x))) #t)
                         (equal? (procedure? '(lambda (x) (* x x))) #f)
                         (equal? (apply (lambda (a b) (+ a b)) (list 3 4)) 7)
                         (equal? ((lambda x x) 1 2 3) '(1 2 3))
                         (equal? ((lambda x x)) '())
                         (equal? ((lambda x (length x)) 1 2 3 4) 4)
                         (equal? ((lambda (a b . r) r) 1 2 3 4) '(3 4))
                         (let ([compose (lambda (f g) (lambda args (f (apply g args))))])
                           (equal? ((compose sqrt *) 12 75) 30)) ; todo : improve *
                         (let ([compose (lambda (f g) (lambda args (f (apply g args))))])
                           (equal? ((compose sqrt (λ (x y) (* x y))) 12 75) 30))
                         (equal? (let ([λ<~ (lambda (x) x)])
                                   (λ<~ 1))
                                 1)
                         (let ([a (procedure-arity (lambda x x))])
                           (and (arity-at-least? a)
                                (equal? (arity-at-least-value a) 0)))))
              (list "procedure-arity: prop:procedure field"
                    (let ()
                      (struct callable (proc) #:property prop:procedure 0)
                      (define p (callable (lambda xs xs)))
                      (define a (procedure-arity p))
                      (and (procedure? p)
                           (arity-at-least? a)
                           (equal? (arity-at-least-value a) 0)
                           (procedure-arity-includes? p 5))))
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
                          (let ([a (procedure-arity (case-lambda [(x . y) x]))])
                            (and (arity-at-least? a)
                                 (equal? (arity-at-least-value a) 1)))
                          (equal? (procedure-arity (case-lambda [(x) x] [(x y) x]))   '(1 2))
                          (equal? (procedure-arity (case-lambda [(x y) x] [(x) x] ))  '(1 2))
                          (let ([a (procedure-arity (case-lambda [(x) x] [(x . y) x]))])
                            (and (arity-at-least? a)
                                 (equal? (arity-at-least-value a) 1)))
                          (let ([a (procedure-arity (case-lambda [(x) 0]
                                                                 [(x y z) 1]
                                                                 [(x y z w u . rest) 2]))])
                            (and (list? a)
                                 (equal? (length a) 3)
                                 (equal? (car a) 1)
                                 (equal? (cadr a) 3)
                                 (arity-at-least? (caddr a))
                                 (equal? (arity-at-least-value (caddr a)) 5)))
                          (let ([a (procedure-arity (case-lambda [() 10] [x 1]))])
                            (and (arity-at-least? a)
                                 (equal? (arity-at-least-value a) 0)))))
              (list "procedure?"
                    (and
                     (equal? (procedure? car) #t)
                     (equal? (procedure? 'car) #f)
                     (equal? (procedure? (lambda (x) (* x x))) #t)
                     (equal? (procedure? '(lambda (x) (* x x))) #f)
                     #;(equal? (procedure? call-with-current-continuation) #t)
                     (equal? (procedure? call-with-escape-continuation) #t)
                     (equal? (procedure? (case-lambda ((x) x) ((x y) (+ x y)))) #t)                  
                     (equal? (procedure-arity procedure?) 1)))
              (list "identity"
                    (and (equal? (identity 'ok) 'ok)
                         (equal? (procedure? identity) #t)
                         (equal? (procedure-arity identity) 1)))
              (list "procedure-specialize"
                    (let ([proc (lambda (x) x)])
                      (and
                       (procedure? (procedure-specialize proc))
                       (eq? (procedure-specialize proc) proc)
                       (equal? ((procedure-specialize proc) 'ok) 'ok)
                       (equal? (procedure-arity procedure-specialize) 1))))
              (list "procedure-extract-target"
                    (and
                     (equal? (procedure-extract-target (lambda (x) x)) #f)
                     (with-handlers ([exn? (lambda (ex) #t)])
                       (procedure-extract-target 'not-a-procedure))))
              (list "call-with-escape-continuation/bootstrap"
                    (and
                     (equal? (call-with-escape-continuation
                              (lambda (_escape) 'ordinary-return))
                             'ordinary-return)
                     (equal? (call-with-escape-continuation
                              (lambda (escape)
                                (+ 1 (escape 42))))
                             42)))
              (list "procedure-reduce-arity"
                    (and
                     (procedure? (procedure-reduce-arity values 1))
                     (equal? (procedure-arity (procedure-reduce-arity values 1)) 1)
                     (equal? ((procedure-reduce-arity (lambda xs xs) 2) 'a 'b) '(a b))
                     (with-handlers ([exn? (lambda (ex) #t)])
                       ((procedure-reduce-arity (lambda xs xs) 2) 'a))))
              (list "procedure-reduce-arity-mask"
                    (and
                     (procedure? (procedure-reduce-arity-mask values 2))
                     (equal? (procedure-arity (procedure-reduce-arity-mask values 2)) 1)
                     (equal? ((procedure-reduce-arity-mask (lambda xs xs) 4) 'a 'b) '(a b))
                     (equal? (procedure-arity (procedure-reduce-arity-mask (lambda xs xs) -4))
                             (make-arity-at-least 2))
                     (with-handlers ([exn? (lambda (ex) #t)])
                       ((procedure-reduce-arity-mask (lambda xs xs) 4) 'a))))
              (list "with-handlers: success path keeps local handler bindings"
                    (and
                     (equal? (with-handlers ([exn? (lambda (_e) 'handled)])
                               'ok)
                             'ok)
                     (equal? (let ()
                               (define (return-string tag exp)
                                 (define tag-name (symbol->string tag))
                                 (define (handle-exn e)
                                   (list tag-name exp (exn-message e)))
                                 (with-handlers ([exn? handle-exn])
                                   tag-name))
                               (return-string 'div '(div)))
                             "div")))
              (list "closedapp: mutation with 0/1/2/3 free variables"
                    (and
                     ;; 0 free variables: mutation is local to the activation.
                     (equal? ((lambda (x) (set! x (+ x 1)) x) 10) 11)
                     ;; 1 free variable: mutation is shared across closures.
                     (equal? (let ([x 0])
                               (let ([inc (lambda () (set! x (+ x 1)) x)]
                                     [get (lambda () x)])
                                 (list (inc) (inc) (get))))
                             '(1 2 2))
                     ;; 2 free variables: both captured variables are mutated and observed.
                     (equal? (let ([x 1] [y 10])
                               (let ([bump (lambda () (set! x (+ x 1)) (set! y (+ y 2)) 0)]
                                     [get  (lambda () (+ x y))])
                                 (bump)
                                 (bump)
                                 (get)))
                             17)
                     ;; 3 free variables: chained mutation through three captured bindings.
                     (equal? (let ([a 1] [b 2] [c 3])
                               (let ([step (lambda (n)
                                             (set! a (+ a n))
                                             (set! b (+ b a))
                                             (set! c (+ c b))
                                             (+ a b c))])
                                 (list (step 4) a b c)))
                             '(22 5 7 10))))
              (list "mutation + closure conversion: boxed vars vs free refs"
                    (and
                     ;; Captured variable sees later mutation in enclosing scope.
                     (equal? (let ([x 1])
                               (let ([f (lambda () x)])
                                 (set! x 2)
                                 (f)))
                             2)
                     ;; Multiple closures share one mutable captured variable.
                     (equal? (let ([x 0])
                               (let ([inc! (lambda () (set! x (+ x 1)))]
                                     [get  (lambda () x)])
                                 (inc!)
                                 (inc!)
                                 (get)))
                             2)
                     ;; Shadowing must not corrupt free-variable resolution.
                     (equal? (let ([x 5])
                               (let ([f (lambda () x)])
                                 (let ([x 9])
                                   (set! x 10)
                                   (f))))
                             5)
                     ;; Procedure-valued captured variable remains mutable and callable.
                     (equal? (let ([f (lambda () 1)])
                               (let ([g (lambda () (f))])
                                 (set! f (lambda () 2))
                                 (g)))
                             2)
                     ;; Webify/Schemify modules use unsafe-undefined slots as
                     ;; placeholders that are filled before safe code observes
                     ;; them. The fill must not count as an early read.
                     (equal? (let ([x unsafe-undefined])
                               (set! x (lambda () x))
                               (eq? (x) x))
                             #t)
                     ;; Read/write interactions across three closures over same capture.
                     (equal? (let ([x 0])
                               (let ([setx! (lambda (v) (set! x v))]
                                     [addx! (lambda (d) (set! x (+ x d)))]
                                     [getx  (lambda () x)])
                                 (setx! 7)
                                 (addx! 5)
                                 (getx)))
                             12)))
              ;; Regression from the matrix-rain/browser examples:
              ;; this minimal shape helped fix an α-rename name-capture bug.
              (list "alpha-rename: internal values definition"
                    (equal? (let ()
                              (define values 0)
                              42
                              (define result 1)
                              result)
                            1))
              )))

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
                        ; bignums are not supported yet
                        #;(equal? (eqv? 10000000000000000000 10000000000000000001) #f)
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

                    ;; TODO: flvector/fxvector are not implemented yet, and
                    ;; stencil-vector equal?/hash-code support is still pending.
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

             (list "equal-always-hash-code"
                   (let ([b (box 'a)]
                         [v (vector 'a)]
                         [iv1 (vector-immutable 'a)]
                         [iv2 (vector-immutable 'a)]
                         [ib1 (box-immutable 'a)]
                         [ib2 (box-immutable 'a)])
                     (and (fixnum? (equal-always-hash-code '(1 2)))
                          (eq? (equal-always-hash-code b) (eq-hash-code b))
                          (eq? (equal-always-hash-code v) (eq-hash-code v))
                          (eq? (equal-always-hash-code iv1)
                               (equal-always-hash-code iv2))
                          (eq? (equal-always-hash-code ib1)
                               (equal-always-hash-code ib2)))))

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

       (list "named constants"
             (and (equal? null '())
                  (equal? empty '())
                  (equal? true #t)
                  (equal? false #f)
                  (eof-object? eof)
                  (< (abs (- pi 3.141592653589793)) 1e-12)
                  (eq? undefined undefined)
                  (eq? unsafe-undefined unsafe-undefined)
                  (not (eq? undefined unsafe-undefined))))
       
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
              (list "boolean=? argument errors are catchable with who"
                    (let ([catch-contract/who+expected+given?
                           (lambda (thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and (string? message)
                                    (string-contains? message "boolean=?")
                                    (string-contains? message "boolean?")
                                    (string-contains? message "42"))))])
                      (and (catch-contract/who+expected+given?
                            (lambda () (boolean=? 42 #t)))
                           (catch-contract/who+expected+given?
                            (lambda () (boolean=? #t 42))))))
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
                         (equal? (mutable-bytes?   #"ab") #f)))
              (list "weak boxes"
                    (let ([wb (make-weak-box 'cached)])
                      (and (weak-box? wb)
                           (not (weak-box? 'cached))
                           (equal? (weak-box-value wb) 'cached)
                           (equal? (weak-box-value wb 'missing) 'cached))))
              (list "box-cas!"
                    (let ([b (box 'old)])
                      (and (equal? (box-cas! b 'wrong 'new) #f)
                           (equal? (unbox b) 'old)
                           (equal? (box-cas! b 'old 'new) #t)
                           (equal? (unbox b) 'new))))
              (list "unbox*/set-box*!"
                    (let ([b (box 'old)])
                      (and (equal? (unbox* b) 'old)
                           (equal? (set-box*! b 'new) (void))
                           (equal? (unbox* b) 'new))))
              (list "unsafe-box*-cas!"
                    (let ([b (box 1)])
                      (and (unsafe-box*-cas! b 1 2)
                           (equal? (unbox b) 2))))
              (list "box operation argument errors are catchable with who"
                    (let ([catch-contract/who+expected?
                           (lambda (who expected thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and (string? message)
                                    (string-contains? message who)
                                    (string-contains? message expected))))])
                      (and (catch-contract/who+expected? "unbox"
                                                         "box?"
                                                         (lambda () (unbox 42)))
                           (catch-contract/who+expected? "unbox*"
                                                         "(and/c box? (not impersonator?))"
                                                         (lambda () (unbox* 42)))
                           (catch-contract/who+expected? "set-box!"
                                                         "(and/c box? (not/c immutable?))"
                                                         (lambda () (set-box! 42 'x)))
                           (catch-contract/who+expected? "set-box!"
                                                         "(and/c box? (not/c immutable?))"
                                                         (lambda () (set-box! (box-immutable 'x) 'y)))
                           (catch-contract/who+expected? "set-box*!"
                                                         "(and/c box? (not immutable?) (not impersonator?))"
                                                         (lambda () (set-box*! 42 'x)))
                           (catch-contract/who+expected? "set-box*!"
                                                         "(and/c box? (not immutable?) (not impersonator?))"
                                                         (lambda () (set-box*! (box-immutable 'x) 'y)))
                           (catch-contract/who+expected? "box-cas!"
                                                         "(and/c box? (not/c immutable?))"
                                                         (lambda () (box-cas! 42 'old 'new)))
                           (catch-contract/who+expected? "box-cas!"
                                                         "(and/c box? (not/c immutable?))"
                                                         (lambda () (box-cas! (box-immutable 'old) 'old 'new)))
                           (catch-contract/who+expected? "weak-box-value"
                                                         "weak-box?"
                                                         (lambda () (weak-box-value 42))))))
              (list "unsafe-root-continuation-prompt-tag placeholder"
                    (eq? (unsafe-root-continuation-prompt-tag)
                         (unsafe-root-continuation-prompt-tag)))
              (list "continuation-prompt-available? placeholder"
                    (equal? (continuation-prompt-available?
                             (unsafe-root-continuation-prompt-tag))
                            #t))
              (list "continuation-prompt-tag? placeholder"
                    (and (equal? (continuation-prompt-tag?
                                  (unsafe-root-continuation-prompt-tag))
                                 #t)
                         (equal? (continuation-prompt-tag? 'not-a-tag) #f)))
              (list "continuation? placeholder"
                    (equal? (continuation? (lambda (x) x)) #f))
              (list "dynamic-wind/basic"
                    (let ([seen '()])
                      (and (equal? (dynamic-wind
                                    (lambda () (set! seen (cons 'before seen)))
                                    (lambda ()
                                      (set! seen (cons 'body seen))
                                      'result)
                                    (lambda () (set! seen (cons 'after seen))))
                                   'result)
                           (equal? seen '(after body before)))))
              (list "call-with-continuation-barrier bootstrap"
                    (let ([seen '()])
                      (and (equal? (call-with-continuation-barrier
                                    (lambda ()
                                      (set! seen (cons 'body seen))
                                      'result))
                                   'result)
                           (equal? seen '(body)))))
              (list "continuation-mark-set-first bootstrap placeholder"
                    (let ([key-a (make-continuation-mark-key)]
                          [key-b (make-continuation-mark-key 'named)])
                      (and (continuation-mark-key? key-a)
                           (continuation-mark-key? key-b)
                           (equal? (continuation-mark-key? 'named) #f)
                           (equal? (equal? key-a key-b) #f)
                           (continuation-mark-set? (current-continuation-marks))
                           (equal? (continuation-mark-set-first
                                    (current-continuation-marks)
                                    key-a)
                                   #f)
                           (equal? (continuation-mark-set-first
                                    (current-continuation-marks)
                                    key-a
                                    'none)
                                   'none)
                           (equal? (continuation-mark-set-first
                                    (current-continuation-marks)
                                    key-b
                                    'none
                                    (unsafe-root-continuation-prompt-tag))
                                   'none)
                           (parameter? current-input-port)
                           (parameter? current-output-port)
                           (parameter? current-error-port)
                           (parameter? current-logger)
                           (parameter? current-security-guard)
                           (parameter? current-print)
                           (parameter? current-namespace)
                           (not (parameter? current-parameterization))
                           (not (parameter? add1))
                           (not (parameter? 42))
                           (equal? (reparameterize 'paramz) 'paramz)
                           (let* ([original (error-print-width)]
                                  [paramz (extend-parameterization
                                           (current-parameterization)
                                           error-print-width
                                           80
                                           error-print-width
                                           81)])
                             (and (equal? (error-print-width) original)
                                  (equal? (call-with-parameterization
                                           paramz
                                           (lambda ()
                                             (error-print-width)))
                                          81)
                                  (equal? (error-print-width) original)
                                  (equal? (call-with-parameterization
                                           paramz
                                           (lambda ()
                                             (error-print-width 90)
                                             (let loop ([i 50])
                                               (if (= i 0)
                                                   (error-print-width)
                                                   (begin
                                                     (error-print-width 80)
                                                     (error-print-width 90)
                                                     (loop (- i 1)))))))
                                          90)
                                  (equal? (error-print-width) original))))))))
       
       (list "4.3 Numbers"
             (list "4.3.1 Number Types"
                   (list
                    (list "number?"
                          (and (equal? (number? 1)   #t)
                               (equal? (number? 1.5) #t)
                               (equal? (number? 0+1i) #t)
                               (equal? (number? 'a)  #f)
                               (equal? (number? #f)  #f)
                               (equal? (procedure-arity number?) 1)))
                    (list "complex?"
                          (and (equal? (complex? 1)    #t)
                               (equal? (complex? 1.5)  #t)
                               (equal? (complex? 0+1i) #t)
                               (equal? (complex? 'a)   #f)
                               (equal? (procedure-arity complex?) 1)))
                    (list "number->string"
                          (and  (equal? (number->string 42)     "42")
                                (equal? (number->string 16 16)  "10")
                                (equal? (number->string 1.25)   "1.25")
                                (equal? (number->string 1.0)    "1.0")
                                (equal? (number->string -0.0)   "-0.0")
                                (equal? (number->string -42)    "-42")
                                (equal? (number->string -16 16) "-10")
                                (equal? (number->string -1.25)  "-1.25")
                                (equal? (number->string -1.0)   "-1.0")
                                (equal? (number->string 0.0001) "0.0001")
                                (equal? (number->string 0+1i)   "0+1i")
                                (equal? (number->string 0-1i)   "0-1i")
                                (equal? (number->string +inf.0) "+inf.0")
                                (equal? (number->string -inf.0) "-inf.0")
                                (equal? (number->string +nan.0) "+nan.0")))
                    (list "string->number"
                          (and (equal? (string->number "42")     42)
                               (equal? (string->number "111" 7)  57)
                               (equal? (string->number "-42")    -42)
                               (equal? (string->number "-111" 7) -57)
                               (equal? (string->number "AFe1" 16) 45025)
                               (equal? (string->number "afe1" 16) 45025)
                               (equal? (string->number "AFE1" 16) 45025)
                               (equal? (string->number "-2.3")   -2.3)
                               (equal? (string->number ".")      #f)
                               (equal? (string->number "1.")     1.0)
                               (equal? (string->number "78.")    78.0)
                               (equal? (string->number "79.0")   79.0)
                               (equal? (string->number "80.00")  80.0)
                               (equal? (string->number "1.10000000000000000000") 1.1)
                               (equal? (string->number "hello") #f)))
                    (list "real?"
                          (and (equal? (real? 1)      #t)
                               (equal? (real? 1.0)    #t)
                               (equal? (real? +nan.0) #t)
                               (equal? (real? 0+1i)   #f)
                               (equal? (real? 'a)     #f)
                               (equal? (procedure-arity real?) 1)))
                    (list "rational?"
                          (and (equal? (rational? 1)      #t)
                               (equal? (rational? 1.0)    #t)
                               (equal? (rational? +inf.0) #f)
                               (equal? (rational? +nan.0) #f)
                               (equal? (rational? 'a)     #f)
                               (equal? (procedure-arity rational?) 1)))
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
                               (equal? (+ 1  2. 3)  6.)
                               (equal? (+ 536870911 1) 536870912)
                               (equal? (+ 4294967295 1) 4294967296)
                               (equal? (+ 9007199254740993 7)
                                       9007199254741000)))
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
                               (equal? (- -536870912 1) -536870913)
                               (equal? (- 536870912 1) 536870911)
                               (equal? (- 9007199254740993 9007199254740993)
                                       0)))
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
                                       -0.30399999999999866)
                               (equal? (* 536870911 2) 1073741822)
                               (equal? (* 4294967296 4294967296)
                                       18446744073709551616)))
                    (list "/"
                          (and (equal? (/ 2)       1/2)
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
                                 (equal? (= 1 2 1) #f)
                                 (equal? (= 9007199254740993
                                            9007199254740993)
                                         #t)
                                 (equal? (= 9007199254740993
                                            9007199254740992)
                                         #f)))
                      (list "<"
                            (and (equal? (< 10) #t)
                                 (equal? (< 1 2 3) #t)
                                 (equal? (< 1 3 2) #f)
                                 (equal? (< 1 2.0 3.0) #t)
                                 (equal? (< 536870911 536870912) #t)
                                 (equal? (< 9007199254740992
                                            9007199254740993)
                                         #t)
                                 (equal? (< -9007199254740993
                                            -536870913
                                            0
                                            536870912
                                            9007199254740993)
                                         #t)))
                      (list ">"
                            (and (equal? (> 7) #t)
                                 (equal? (> 3 2 1) #t)
                                 (equal? (> 3 2 2) #f)
                                 (equal? (> 3.0 2 1) #t)
                                 (equal? (> -536870912 -536870913) #t)))
                      (list "<="
                            (and (equal? (<= 4) #t)
                                 (equal? (<= 1 2 2) #t)
                                 (equal? (<= 1 3 2) #f)
                                 (equal? (<= 1 1.0 1.0) #t)
                                 (equal? (<= 4294967296 4294967296)
                                         #t)))
                      (list ">="
                            (and (equal? (>= 9) #t)
                                 (equal? (>= 3 2 2) #t)
                                 (equal? (>= 3 4 5) #f)
                                 (equal? (>= 3.0 3 2) #t)
                                 (equal? (>= -9007199254740993
                                             -9007199254740992)
                                         #f)))))
             
             (list "4.3.2 Generic Numerics"
                   (list
                    (list "abs"
                          (and (equal? (abs 1) 1)
                               (equal? (abs -1) 1)
                               (equal? (abs 1.0) 1.0)
                               (equal? (abs -536870912) 536870912)))
                    (list "add1/sub1"
                          (and (equal? (add1 536870911) 536870912)
                               (equal? (sub1 -536870912) -536870913)))
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
                               (equal? (max 1/2) 1/2)
                               (equal? (max 1/2 3/2 1) 3/2)
                               (equal? (max 1/2 0.25) 0.5)
                               (equal? (max 0.25 1/2) 0.5)
                               (eqv? (max -0.0 0.0) 0.0)
                               (nan? (max 1.0 +nan.0))))
                    (list "min"
                          (and (equal? (min 1 3 2) 1)
                               (equal? (min 1 3 2.0) 1.0)
                               (equal? (min 1/2) 1/2)
                               (equal? (min 1/2 -3/2 -1) -3/2)
                               (equal? (min 1/2 0.75) 0.5)
                               (equal? (min 0.75 1/2) 0.5)
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
                               (equal? (sqr 2.0) 4.0)
                               (equal? (sqr 536870911)
                                       288230375077969921)))
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
                               (equal? (atan 0.) 0.)
                               (< (abs (- (atan 0 -1) 3.141592653589793)) 1e-12)))
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
                            (and (exact-integer? v) (<= -5 v) (< v 5))))
                    (list "pseudo-random-generator"
                          (let ([g (make-pseudo-random-generator)])
                            (and (pseudo-random-generator? g)
                                 (let ([v (random 5 g)])
                                   (and (exact-integer? v) (<= 0 v) (< v 5)))
                                 (let ([v (random g)])
                                   (and (inexact? v) (> v 0.0) (< v 1.0))))))
                    (list "current-pseudo-random-generator"
                          (let ([original (current-pseudo-random-generator)]
                                [g (make-pseudo-random-generator)])
                            (and (void? (current-pseudo-random-generator g))
                                 (eq? (current-pseudo-random-generator) g)
                                 (void? (random-seed 17))
                                 (void? (current-pseudo-random-generator original)))))
                    (list "random argument errors are catchable with who"
                          (let ([catch-contract/who+expected+given?
                                 (lambda (who expected given thunk)
                                   (let ([message
                                          (with-handlers ([exn:fail:contract?
                                                           (lambda (exn)
                                                             (exn-message exn))])
                                            (thunk)
                                            #f)])
                                     (and message
                                          (string-contains? message who)
                                          (string-contains? message expected)
                                          (string-contains? message given))))])
                            (and
                             (catch-contract/who+expected+given?
                              "random-seed"
                              "(integer-in 0 536870911)"
                              "#f"
                              (lambda () (random-seed #f)))
                             (catch-contract/who+expected+given?
                              "random-seed"
                              "(integer-in 0 536870911)"
                              "-1"
                              (lambda () (random-seed -1)))
                             (catch-contract/who+expected+given?
                              "random"
                              "(or/c (integer-in 1 536870911) pseudo-random-generator?)"
                              "#f"
                              (lambda () (random #f)))
                             (catch-contract/who+expected+given?
                              "random"
                              "(or/c (integer-in 1 536870911) pseudo-random-generator?)"
                              "0"
                              (lambda () (random 0)))
                             (catch-contract/who+expected+given?
                              "random"
                              "fixnum?"
                              "#f"
                              (lambda () (random #f 10)))
                             (catch-contract/who+expected+given?
                              "random"
                              "(or/c fixnum? pseudo-random-generator?)"
                              "#f"
                              (lambda () (random 1 #f)))
                             (catch-contract/who+expected+given?
                              "random"
                              "(integer-in 1 536870911)"
                              "#f"
                              (lambda ()
                                (random #f (make-pseudo-random-generator))))
                             (catch-contract/who+expected+given?
                              "random"
                              "(integer-in 1 536870911)"
                              "0"
                              (lambda ()
                                (random 0 (make-pseudo-random-generator))))
                             (catch-contract/who+expected+given?
                              "random"
                              "(>/c 10)"
                              "1"
                              (lambda () (random 10 1))))))))

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
                    (list "real->floating-point-bytes exact rationals"
                          (and (bytes=? (real->floating-point-bytes 1/2 8)
                                        #"\x00\x00\x00\x00\x00\x00\xe0\x3f")
                               (bytes=? (real->floating-point-bytes
                                         2106270354592/12789239550964971
                                         8)
                                        #"\xdd\xc5\x25\x60\x1b\x96\x25\x3f")))
                    (list "real->floating-point-bytes exact bignum"
                          (bytes=? (real->floating-point-bytes
                                    9007199254740993
                                    8)
                                   #"\x00\x00\x00\x00\x00\x00\x40\x43"))
                    (list "real->floating-point-bytes start"
                          (let* ([dest (make-bytes 10)]
                                 [res  (real->floating-point-bytes 1.0 8 #f dest 2)])
                            (and (eq? res dest)
                                 (bytes=? dest
                                          #"\x00\x00\x00\x00\x00\x00\x00\x00\xf0\x3f"))))
                    (list "real->floating-point-bytes dest"
                          (let* ([dest (make-bytes 8)]
                                 [res  (real->floating-point-bytes 1.0 8 #f dest)])
                            (and (eq? res dest)
                                 (bytes=? dest
                                          #"\x00\x00\x00\x00\x00\x00\xf0\x3f"))))
                    (list "real->floating-point-bytes destination errors are catchable with who"
                          (let ([catch-contract/message-contains?
                                 (lambda (parts thunk)
                                   (let ([message
                                          (with-handlers ([exn:fail:contract?
                                                           (lambda (exn)
                                                             (exn-message exn))])
                                            (thunk)
                                            #f)])
                                     (and message
                                          (andmap (lambda (part)
                                                    (string-contains? message part))
                                                  parts))))])
                            (and
                             (catch-contract/message-contains?
                              '("real->floating-point-bytes: contract violation"
                                "(and/c bytes? (not/c immutable?))"
                                "given: 42")
                              (lambda ()
                                (real->floating-point-bytes 1.0 8 #f 42)))
                             (catch-contract/message-contains?
                              '("real->floating-point-bytes: contract violation"
                                "(and/c bytes? (not/c immutable?))"
                                "given:")
                              (lambda ()
                                (real->floating-point-bytes
                                 1.0
                                 8
                                 #f
                                 (bytes->immutable-bytes
                                  (bytes 0 0 0 0 0 0 0 0))))))))
                    (list "real->floating-point-bytes size and range errors are catchable with who"
                          (let ([catch-contract/message-contains?
                                 (lambda (parts thunk)
                                   (let ([message
                                          (with-handlers ([exn:fail:contract?
                                                           (lambda (exn)
                                                             (exn-message exn))])
                                            (thunk)
                                            #f)])
                                     (and message
                                          (andmap (lambda (part)
                                                    (string-contains? message part))
                                                  parts))))])
                            (and
                             (catch-contract/message-contains?
                              '("real->floating-point-bytes: contract violation"
                                "(or/c 4 8)"
                                "given: 5")
                              (lambda ()
                                (real->floating-point-bytes 1.0 5)))
                             (catch-contract/message-contains?
                              '("real->floating-point-bytes: contract violation"
                                "(or/c 4 8)"
                                "given: 'bad")
                              (lambda ()
                                (real->floating-point-bytes 1.0 'bad)))
                             (catch-contract/message-contains?
                              '("real->floating-point-bytes: contract violation"
                                "exact-nonnegative-integer?"
                                "given: #f")
                              (lambda ()
                                (real->floating-point-bytes 1.0 8 #f (make-bytes 8) #f)))
                             (catch-contract/message-contains?
                              '("real->floating-point-bytes: contract violation"
                                "exact-nonnegative-integer?"
                                "given: -1")
                              (lambda ()
                                (real->floating-point-bytes 1.0 8 #f (make-bytes 8) -1)))
                             (catch-contract/message-contains?
                              '("real->floating-point-bytes: byte string length is shorter than starting position plus size"
                                "byte string length: 4"
                                "starting position: 0"
                                "size: 8")
                              (lambda ()
                                (real->floating-point-bytes 1.0 8 #f (make-bytes 4))))
                             (catch-contract/message-contains?
                              '("real->floating-point-bytes: byte string length is shorter than starting position plus size"
                                "byte string length: 8"
                                "starting position: 1"
                                "size: 8")
                              (lambda ()
                                (real->floating-point-bytes 1.0 8 #f (make-bytes 8) 1))))))
                    (list "floating-point-bytes->real"
                          (and (= (floating-point-bytes->real (bytes 0 0 128 63)) 1.0)
                               (= (floating-point-bytes->real (bytes 63 240 0 0 0 0 0 0) #t 0 8) 1.0)))
                    (list "floating-point-bytes->real errors are catchable with who"
                          (let ([catch-contract/message-contains?
                                 (lambda (parts thunk)
                                   (let ([message
                                          (with-handlers ([exn:fail:contract?
                                                           (lambda (exn)
                                                             (exn-message exn))])
                                            (thunk)
                                            #f)])
                                     (and message
                                          (andmap (lambda (part)
                                                    (string-contains? message part))
                                                  parts))))])
                            (and
                             (catch-contract/message-contains?
                              '("floating-point-bytes->real: contract violation"
                                "bytes?"
                                "given: 42")
                              (lambda ()
                                (floating-point-bytes->real 42)))
                             (catch-contract/message-contains?
                              '("floating-point-bytes->real: contract violation"
                                "exact-nonnegative-integer?"
                                "given: #f")
                              (lambda ()
                                (floating-point-bytes->real (bytes 0 0 128 63) #f #f 4)))
                             (catch-contract/message-contains?
                              '("floating-point-bytes->real: contract violation"
                                "exact-nonnegative-integer?"
                                "given: -1")
                              (lambda ()
                                (floating-point-bytes->real (bytes 0 0 128 63) #f -1 4)))
                             (catch-contract/message-contains?
                              '("floating-point-bytes->real: contract violation"
                                "exact-nonnegative-integer?"
                                "given: #f")
                              (lambda ()
                                (floating-point-bytes->real (bytes 0 0 128 63) #f 0 #f)))
                             (catch-contract/message-contains?
                              '("floating-point-bytes->real: contract violation"
                                "exact-nonnegative-integer?"
                                "given: -1")
                              (lambda ()
                                (floating-point-bytes->real (bytes 0 0 128 63) #f 0 -1)))
                             (catch-contract/message-contains?
                              '("floating-point-bytes->real: starting index is out of range"
                                "starting index: 5"
                                "valid range: [0, 4]"
                                "index:")
                              (lambda ()
                                (floating-point-bytes->real (bytes 0 0 128 63) #f 5)))
                             (catch-contract/message-contains?
                              '("floating-point-bytes->real: ending index is out of range"
                                "ending index: 5"
                                "valid range: [0, 4]"
                                "index:")
                              (lambda ()
                                (floating-point-bytes->real (bytes 0 0 128 63) #f 0 5)))
                             (catch-contract/message-contains?
                              '("floating-point-bytes->real: ending index is smaller than starting index"
                                "ending index: 1"
                                "starting index: 3"
                                "valid range: [0, 4]"
                                "index:")
                              (lambda ()
                                (floating-point-bytes->real (bytes 0 0 128 63) #f 3 1)))
                             (catch-contract/message-contains?
                              '("floating-point-bytes->real: length is not 4 or 8 bytes"
                                "length: 3")
                              (lambda ()
                                (floating-point-bytes->real (bytes 0 0 128))))
                             (catch-contract/message-contains?
                              '("floating-point-bytes->real: length is not 4 or 8 bytes"
                                "length: 0")
                              (lambda ()
                                (floating-point-bytes->real (bytes)))))))
                    (list "floating-point byte conversions accept truthy big-endian values"
                          (and (bytes=? (real->floating-point-bytes 1.0 8 'big)
                                        #"\x3f\xf0\x00\x00\x00\x00\x00\x00")
                               (bytes=? (real->floating-point-bytes 1.0 8 0)
                                        #"\x3f\xf0\x00\x00\x00\x00\x00\x00")
                               (= (floating-point-bytes->real
                                   #"\x3f\xf0\x00\x00\x00\x00\x00\x00"
                                   'big
                                   0
                                   8)
                                  1.0)
                               (= (floating-point-bytes->real
                                   #"\x3f\xf0\x00\x00\x00\x00\x00\x00"
                                   0
                                   0
                                   8)
                                  1.0)))
                    (list "system-big-endian?"
                          (and (equal? (system-big-endian?) #f)
                               (equal? (procedure-arity system-big-endian?) 0)))
                    (list "system-type"
                          (and (equal? (system-type) 'unix)
                               (equal? (system-type 'os) 'unix)
                               (equal? (system-type 'vm) 'chez-scheme)
                               (equal? (system-type 'gc) 'cs)
                               (equal? (vector-ref (system-type 'fs-change) 2) #f)))
                    (list "system-type argument errors are catchable with who"
                          (let ([catch-contract/who+expected+given?
                                 (lambda (who expected given thunk)
                                   (let ([message
                                          (with-handlers ([exn:fail:contract?
                                                           (lambda (exn)
                                                             (exn-message exn))])
                                            (thunk)
                                            #f)])
                                     (and message
                                          (string-contains? message who)
                                          (string-contains? message expected)
                                          (string-contains? message given))))])
                            (and
                             (catch-contract/who+expected+given?
                              "system-type"
                              "(or/c 'os 'os* 'arch"
                              "#f"
                              (lambda () (system-type #f)))
                             (catch-contract/who+expected+given?
                              "system-type"
                              "(or/c 'os 'os* 'arch"
                              "'bogus"
                              (lambda () (system-type 'bogus))))))
                    (list "current-compile-target-machine"
                          (let ([original (current-compile-target-machine)])
                            (and (equal? original (system-type 'target-machine))
                                 (equal? (current-compile-target-machine 'probe-target) (void))
                                 (equal? (current-compile-target-machine) 'probe-target)
                                 (equal? (current-compile-target-machine original) (void))
                                 (equal? (current-compile-target-machine) original))))
                    (list "current-compile-realm"
                          (let ([original (current-compile-realm)])
                            (and (equal? original 'racket)
                                 (equal? (current-compile-realm 'probe-realm) (void))
                                 (equal? (current-compile-realm) 'probe-realm)
                                 (equal? (current-compile-realm original) (void))
                                 (equal? (current-compile-realm) original))))
                    (list "version"
                          (and (string? (version))
                               (immutable? (version))
                               (positive? (string-length (version)))
                               (equal? (procedure-arity version) 0)))))

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
                    (list "flvector?"
                          (and (equal? (flvector? 1.0) #f)
                               (equal? (procedure-arity flvector?) 1)))
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
                    (list "flsinh"
                          (fl= (flsinh 0.0) 0.0))
                    (list "flcosh"
                          (fl= (flcosh 0.0) 1.0))
                    (list "fltanh"
                          (fl= (fltanh 0.0) 0.0))
                    (list "flasinh"
                          (fl= (flasinh 0.0) 0.0))
                    (list "flacosh"
                          (fl= (flacosh 1.0) 0.0))
                    (list "flatanh"
                          (fl= (flatanh 0.0) 0.0))
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
                            (and (flonum? v) (> v 0.0) (< v 1.0))))
                    (list "flonum operator errors are catchable with who"
                          (let ([catch-contract/who+expected+given?
                                 (lambda (who expected given thunk)
                                   (let ([message
                                          (with-handlers ([exn:fail:contract?
                                                           (lambda (exn)
                                                             (exn-message exn))])
                                            (thunk)
                                            #f)])
                                     (and message
                                          (string-contains? message who)
                                          (string-contains? message expected)
                                          (string-contains? message given))))])
                            (and
                             (catch-contract/who+expected+given?
                              "fl+"
                              "flonum?"
                              "given: 1"
                              (lambda () (fl+ 1.0 1)))
                             (catch-contract/who+expected+given?
                              "fl/"
                              "flonum?"
                              "given: 1"
                              (lambda () (fl/ 1.0 1)))
                             (catch-contract/who+expected+given?
                              "flabs"
                              "flonum?"
                              "given: 1"
                              (lambda () (flabs 1)))
                             (catch-contract/who+expected+given?
                              "fl="
                              "flonum?"
                              "given: 1"
                              (lambda () (fl= 1.0 1)))
                             (catch-contract/who+expected+given?
                              "flmin"
                              "flonum?"
                              "given: 1"
                              (lambda () (flmin 1.0 1)))))))
                   )
             
             (list "4.3.4 Fixnums"
                   (list
                    (list "fixnum?"
                          (and (equal? (fixnum? 1) #t)
                               (equal? (fixnum? 1.0) #f)))
                    (list "fxvector?"
                          (and (equal? (fxvector? 1) #f)
                               (equal? (procedure-arity fxvector?) 1)))
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
                               (equal? (fxrshift -4 1) -2)
                               (equal? (fxrshift 42 2) 10)
                               (equal? (fxrshift -42 2) -11)))
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

                    (list "unsafe fixnum bit operations"
                          (and (equal? (unsafe-fxnot 0) -1)
                               (equal? (unsafe-fxlshift 42 2) 168)
                               (equal? (unsafe-fxrshift 42 2) 10)
                               (equal? (unsafe-fxrshift -42 2) -11)
                               (equal? (unsafe-fxrshift/logical 42 2) 10)
                               (equal? (unsafe-fxpopcount 7) 3)
                               (equal? (unsafe-fxpopcount32 7) 3)
                               (equal? (unsafe-fxpopcount16 7) 3)
                               (equal? (unsafe-fx+/wraparound 1 2) 3)
                               (equal? (unsafe-fx*/wraparound 2 3) 6)
                               (equal? (unsafe-fxlshift/wraparound 42 2) 168)))

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
                    (list "flonum conversion errors are catchable with who"
                          (let ([catch-contract/who+expected+given?
                                 (lambda (who expected given thunk)
                                   (let ([message
                                          (with-handlers ([exn:fail:contract?
                                                           (lambda (exn)
                                                             (exn-message exn))])
                                            (thunk)
                                            #f)])
                                     (and message
                                          (string-contains? message who)
                                          (string-contains? message expected)
                                          (string-contains? message given))))])
                            (and
                             (catch-contract/who+expected+given?
                              "fx->fl"
                              "fixnum?"
                              "given: 1.0"
                              (lambda () (fx->fl 1.0)))
                             (catch-contract/who+expected+given?
                              "->fl"
                              "exact-integer?"
                              "given: 1.0"
                              (lambda () (->fl 1.0)))
                             (catch-contract/who+expected+given?
                              "fl->fx"
                              "flonum?"
                              "given: 1"
                              (lambda () (fl->fx 1)))
                             (catch-contract/who+expected+given?
                              "fl->exact-integer"
                              "(and/c flonum? integer?)"
                              "given: 1.2"
                              (lambda () (fl->exact-integer 1.2)))
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (fl->fx +nan.0)
                                      #f)])
                               (and message
                                    (string-contains? message "fl->fx")
                                    (string-contains? message "result for")
                                    (string-contains?
                                     message
                                     "would be outside of fixnum range"))))))

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

              (list "string-length argument errors are catchable with who"
                    (let ([message
                           (with-handlers ([exn:fail:contract?
                                            (lambda (exn)
                                              (exn-message exn))])
                             (string-length 42)
                             #f)])
                      (and (string? message)
                           (string-contains? message "string-length")
                           (string-contains? message "string?"))))

              (list "string-grapheme-span"
                    (and (equal? (string-grapheme-span "" 0) 0)
                         (equal? (string-grapheme-span "a" 0) 1)
                         (equal? (string-grapheme-span "ab" 0) 1)
                         (equal? (string-grapheme-span "\r\n" 0) 2)
                         (equal? (string-grapheme-span "\r\nx" 0) 2)
                         (equal? (string-grapheme-span "\r\nx" 2) 1)
                         (equal? (string-grapheme-span "\r\nx" 0 1) 1)
                         (equal? (string-grapheme-span "e\u0301x" 0) 2)
                         (equal? (string-grapheme-span "🇩🇰x" 0) 2)
                         (equal? (vector (string-grapheme-span "a\tb" 0)
                                         (string-grapheme-span "a\tb" 1)
                                         (string-grapheme-span "a\tb" 2))
                                 #(1 1 1))
                         (equal? (procedure-arity string-grapheme-span) '(2 3))))

              (list "string-grapheme-span argument errors are catchable with who"
                    (let ([catch-contract/who+expected?
                           (lambda (expected thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and (string? message)
                                    (string-contains? message "string-grapheme-span")
                                    (string-contains? message expected))))])
                      (and (catch-contract/who+expected? "string?"
                                                         (lambda ()
                                                           (string-grapheme-span 42 0)))
                           (catch-contract/who+expected? "exact-nonnegative-integer?"
                                                         (lambda ()
                                                           (string-grapheme-span "a" #f)))
                           (catch-contract/who+expected? "out of range"
                                                         (lambda ()
                                                           (string-grapheme-span "a" 2))))))

              (list "string-ref"
                    (and (equal? (string-ref "abc" 0) #\a)
                         (equal? (string-ref "abc" 2) #\c)
                         (equal? (procedure-arity string-ref) 2)))

              (list "string-ref argument errors are catchable with who"
                    (let ([catch-contract/who+expected?
                           (lambda (who expected thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and (string? message)
                                    (string-contains? message who)
                                    (string-contains? message expected))))]
                          [catch-contract/message-contains?
                           (lambda (thunk . parts)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and (string? message)
                                    (andmap (lambda (part)
                                              (string-contains? message part))
                                            parts))))])
                      (and (catch-contract/who+expected? "string-ref"
                                                         "string?"
                                                         (lambda () (string-ref 42 0)))
                           (catch-contract/who+expected? "string-ref"
                                                         "exact-nonnegative-integer?"
                                                         (lambda () (string-ref "abc" #f)))
                           (catch-contract/who+expected? "string-ref"
                                                         "exact-nonnegative-integer?"
                                                         (lambda () (string-ref "abc" -1)))
                           (catch-contract/message-contains? (lambda () (string-ref "abc" 3))
                                                             "string-ref"
                                                             "index is out of range"
                                                             "index: 3"
                                                             "valid range: [0, 2]"
                                                             "string: \"abc\"")
                           (catch-contract/message-contains? (lambda () (string-ref "" 0))
                                                             "string-ref"
                                                             "empty string"
                                                             "index: 0"
                                                             "string: \"\""))))

              (list "make-string and string-set! argument errors are catchable with who"
                    (let ([catch-contract/who+expected?
                           (lambda (who expected thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and (string? message)
                                    (string-contains? message who)
                                    (string-contains? message expected))))])
                      (and (catch-contract/who+expected? "make-string"
                                                         "exact-nonnegative-integer?"
                                                         (lambda () (make-string -1)))
                           (catch-contract/who+expected? "make-string"
                                                         "exact-nonnegative-integer?"
                                                         (lambda () (make-string #\a)))
                           (catch-contract/who+expected? "make-string"
                                                         "char?"
                                                         (lambda () (make-string 1 42)))
                           (catch-contract/who+expected? "string-set!"
                                                         "mutable-string?"
                                                         (lambda () (string-set! 42 0 #\a)))
                           (catch-contract/who+expected? "string-set!"
                                                         "mutable-string?"
                                                         (lambda () (string-set! "abc" 0 #\x)))
                           (catch-contract/who+expected? "string-set!"
                                                         "exact-nonnegative-integer?"
                                                         (lambda () (string-set! (string #\a) #f #\x)))
                           (catch-contract/who+expected? "string-set!"
                                                         "exact-nonnegative-integer?"
                                                         (lambda () (string-set! (string #\a) -1 #\x)))
                           (catch-contract/who+expected? "string-set!"
                                                         "char?"
                                                         (lambda () (string-set! (string #\a) 0 42))))))

              (list "string-fill! argument errors are catchable with who"
                    (let ([catch-contract/who+expected?
                           (lambda (who expected thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and (string? message)
                                    (string-contains? message who)
                                    (string-contains? message expected))))])
                      (and (catch-contract/who+expected? "string-fill!"
                                                         "mutable-string?"
                                                         (lambda () (string-fill! 42 #\x)))
                           (catch-contract/who+expected? "string-fill!"
                                                         "mutable-string?"
                                                         (lambda () (string-fill! "abc" #\x)))
                           (catch-contract/who+expected? "string-fill!"
                                                         "char?"
                                                         (lambda () (string-fill! (string #\a) 42))))))

              (list "substring"
                    (and (equal? (substring "ab" 0 1) "a")
                         (equal? (substring "ab" 1 2) "b")
                         (equal? (substring "ab" 1)   "b")
                         (equal? (substring "ab" 2)   "")
                         (equal? (procedure-arity substring) '(2 3)))) ; todo - improve arities

              (list "substring argument errors are catchable with who"
                    (let ([catch-contract/who+expected?
                           (lambda (who expected thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and (string? message)
                                    (string-contains? message who)
                                    (string-contains? message expected))))]
                          [catch-contract/message-contains?
                           (lambda (thunk . parts)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and (string? message)
                                    (andmap (lambda (part)
                                              (string-contains? message part))
                                            parts))))])
                      (and (catch-contract/who+expected? "substring"
                                                         "string?"
                                                         (lambda () (substring 42 0)))
                           (catch-contract/who+expected? "substring"
                                                         "exact-nonnegative-integer?"
                                                         (lambda () (substring "abc" #f)))
                           (catch-contract/who+expected? "substring"
                                                         "exact-nonnegative-integer?"
                                                         (lambda () (substring "abc" 0 #f)))
                           (catch-contract/who+expected? "substring"
                                                         "exact-nonnegative-integer?"
                                                         (lambda () (substring "abc" -1)))
                           (catch-contract/who+expected? "substring"
                                                         "exact-nonnegative-integer?"
                                                         (lambda () (substring "abc" 0 -1)))
                           (catch-contract/message-contains? (lambda () (substring "abc" 4))
                                                             "substring"
                                                             "starting index is out of range"
                                                             "starting index: 4"
                                                             "valid range: [0, 3]"
                                                             "string: \"abc\"")
                           (catch-contract/message-contains? (lambda () (substring "abc" 1 4))
                                                             "substring"
                                                             "ending index is out of range"
                                                             "ending index: 4"
                                                             "valid range: [0, 3]"
                                                             "string: \"abc\"")
                           (catch-contract/message-contains? (lambda () (substring "abc" 2 1))
                                                             "substring"
                                                             "ending index is smaller than starting index"
                                                             "ending index: 1"
                                                             "starting index: 2"
                                                             "valid range: [0, 3]"
                                                             "string: \"abc\""))))

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

              (list "string-append argument errors are catchable with who"
                    (let ([catch-contract/who+expected?
                           (lambda (who expected thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and (string? message)
                                    (string-contains? message who)
                                    (string-contains? message expected))))])
                      (and (catch-contract/who+expected? "string-append"
                                                         "string?"
                                                         (lambda () (string-append 42 "x")))
                           (catch-contract/who+expected? "string-append"
                                                         "string?"
                                                         (lambda () (string-append "x" 42)))
                           (catch-contract/who+expected? "string-append-immutable"
                                                         "string?"
                                                         (lambda () (string-append-immutable "x" 42)))
                           (catch-contract/who+expected? "apply"
                                                         "list?"
                                                         (lambda () (string-append* 42)))
                           (catch-contract/who+expected? "apply"
                                                         "list?"
                                                         (lambda () (string-append* "x" 42)))
                           (catch-contract/who+expected? "string-append"
                                                         "string?"
                                                         (lambda () (string-append* "x" (list 42)))))))

              (list "string-join"
                    (and (equal? (string-join '("one" "two" "three" "four"))
                                 "one two three four")
                         (equal? (string-join '("one" "two" "three" "four") ", ")
                                 "one, two, three, four")
                         (equal? (string-join '("one" "two" "three" "four") " potato ")
                                 "one potato two potato three potato four")))

              (list "string-join argument errors are catchable with who"
                    (let ([catch-contract/who+expected?
                           (lambda (who expected thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and (string? message)
                                    (string-contains? message who)
                                    (string-contains? message expected))))])
                      (and (catch-contract/who+expected? "string-join"
                                                         "(listof string?)"
                                                         (lambda () (string-join 42)))
                           (catch-contract/who+expected? "string-join"
                                                         "(listof string?)"
                                                         (lambda () (string-join (cons "a" 42))))
                           (catch-contract/who+expected? "string-join"
                                                         "(listof string?)"
                                                         (lambda () (string-join (list "a" 42))))
                           (catch-contract/who+expected? "string-join"
                                                         "(listof string?)"
                                                         (lambda () (string-join 42 99)))
                           (catch-contract/who+expected? "string-join"
                                                         "string?"
                                                         (lambda () (string-join (list) 42)))
                           (catch-contract/who+expected? "string-join"
                                                         "string?"
                                                         (lambda () (string-join (list "a") 42))))))

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

              (list "string-copy argument errors are catchable with who"
                    (let ([message
                           (with-handlers ([exn:fail:contract?
                                            (lambda (exn)
                                              (exn-message exn))])
                             (string-copy 42)
                             #f)])
                      (and (string? message)
                           (string-contains? message "string-copy")
                           (string-contains? message "string?"))))

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

              (list "string case conversion argument errors are catchable with who"
                    (let ([catch-contract/who+expected?
                           (lambda (who expected thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and (string? message)
                                    (string-contains? message who)
                                    (string-contains? message expected))))])
                      (and (catch-contract/who+expected? "string-upcase"
                                                         "string?"
                                                         (lambda () (string-upcase 42)))
                           (catch-contract/who+expected? "string-downcase"
                                                         "string?"
                                                         (lambda () (string-downcase 42)))
                           (catch-contract/who+expected? "string-titlecase"
                                                         "string?"
                                                         (lambda () (string-titlecase 42)))
                           (catch-contract/who+expected? "string-foldcase"
                                                         "string?"
                                                         (lambda () (string-foldcase 42))))))

              (list "string->list"
                    (and (equal? (string->list "P l") '(#\P #\space #\l))
                         (equal? (string->list "") '())))

              (list "list->string"
                    (and (equal? (list->string '(#\1 #\\ #\")) "1\\\"")
                         (equal? (list->string '()) "")))

              (list "string/list conversion argument errors are catchable with who"
                    (let ([catch-contract/who+expected?
                           (lambda (who expected thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and (string? message)
                                    (string-contains? message who)
                                    (string-contains? message expected))))])
                      (and (catch-contract/who+expected? "string->list"
                                                         "string?"
                                                         (lambda () (string->list 42)))
                           (catch-contract/who+expected? "list->string"
                                                         "list?"
                                                         (lambda () (list->string 42)))
                           (catch-contract/who+expected? "list->string"
                                                         "list?"
                                                         (lambda () (list->string (cons #\a 42))))
                           (catch-contract/who+expected? "list->string"
                                                         "char?"
                                                         (lambda () (list->string (list #\a 42)))))))

              (list "string-find"
                    (and (equal? (string-find "Racket" "ack") 1)
                         (equal? (string-find "Racket" "Rac") 0)
                         (equal? (string-find "Racket" "et")  4)
                         (equal? (string-find "Racket" "cat") #f)))

              (list "string-contains?"
                    (and (equal? (string-contains? "Racket" "ack") #t)
                         (equal? (string-contains? "Racket" "cat") #f)))

              (list "string prefix/suffix predicates"
                    (and (equal? (string-prefix? "Racket" "Rac") #t)
                         (equal? (string-prefix? "Racket" "ack") #f)
                         (equal? (string-suffix? "Racket" "ket") #t)
                         (equal? (string-suffix? "Racket" "ack") #f)))

              (list "string search argument errors are catchable with who"
                    (let ([catch-contract/who+expected?
                           (lambda (who expected thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and (string? message)
                                    (string-contains? message who)
                                    (string-contains? message expected))))])
                      (and (catch-contract/who+expected? "string-find"
                                                         "string?"
                                                         (lambda () (string-find 42 "x")))
                           (catch-contract/who+expected? "string-find"
                                                         "string?"
                                                         (lambda () (string-find "x" 42)))
                           (catch-contract/who+expected? "string-contains?"
                                                         "string?"
                                                         (lambda () (string-contains? 42 "x")))
                           (catch-contract/who+expected? "string-contains?"
                                                         "string?"
                                                         (lambda () (string-contains? "x" 42)))
                           (catch-contract/who+expected? "string-prefix?"
                                                         "string?"
                                                         (lambda () (string-prefix? 42 "x")))
                           (catch-contract/who+expected? "string-prefix?"
                                                         "string?"
                                                         (lambda () (string-prefix? "x" 42)))
                           (catch-contract/who+expected? "string-suffix?"
                                                         "string?"
                                                         (lambda () (string-suffix? 42 "x")))
                           (catch-contract/who+expected? "string-suffix?"
                                                         "string?"
                                                         (lambda () (string-suffix? "x" 42))))))

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

              (list "byte-string argument errors are catchable"
                    (let ([catch-contract?
                           (lambda (thunk)
                             (with-handlers ([exn:fail:contract?
                                              (lambda (exn)
                                                (string? (exn-message exn)))])
                               (thunk)
                               #f))])
                      (and (catch-contract? (lambda () (bytes-ref #"abc" 'zero)))
                           (catch-contract? (lambda () (bytes 0 256)))
                           (catch-contract? (lambda () (bytes-set! (bytes 0) 0 256)))
                           (catch-contract? (lambda () (make-bytes 1 256)))
                           (catch-contract? (lambda () (list->bytes '(0 256)))))))

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

              (list "byte-string mutable destination errors are catchable with who"
                    (let ([catch-contract/message-contains?
                           (lambda (parts thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (andmap (lambda (part)
                                              (string-contains? message part))
                                            parts))))])
                      (and
                       (catch-contract/message-contains?
                        '("bytes-set!: contract violation"
                          "(and/c bytes? (not/c immutable?))"
                          "given:")
                        (lambda ()
                          (bytes-set! (bytes->immutable-bytes (bytes 1 2)) 0 9)))
                       (catch-contract/message-contains?
                        '("bytes-set!: contract violation"
                          "(and/c bytes? (not/c immutable?))"
                          "given: 42")
                        (lambda ()
                          (bytes-set! 42 0 9)))
                       (catch-contract/message-contains?
                        '("bytes-fill!: contract violation"
                          "(and/c bytes? (not/c immutable?))"
                          "given:")
                        (lambda ()
                          (bytes-fill! (bytes->immutable-bytes (bytes 1 2)) 9)))
                       (catch-contract/message-contains?
                        '("bytes-fill!: contract violation"
                          "(and/c bytes? (not/c immutable?))"
                          "given: 42")
                        (lambda ()
                          (bytes-fill! 42 9)))
                       (catch-contract/message-contains?
                        '("bytes-copy!: contract violation"
                          "(and/c bytes? (not/c immutable?))"
                          "given:")
                        (lambda ()
                          (bytes-copy! (bytes->immutable-bytes (bytes 1 2 3))
                                       0
                                       (bytes 9 8))))
                       (catch-contract/message-contains?
                        '("bytes-copy!: contract violation"
                          "(and/c bytes? (not/c immutable?))"
                          "given: 42")
                        (lambda ()
                          (bytes-copy! 42 0 (bytes 9 8)))))))

              (list "list->bytes/bytes->list"
                    (equal? (memq (list->bytes (bytes->list #"apple"))
                                  '(#"apple")) #f))

              (list "string-utf-8-length"
                    (let* ([s1 "çðö£"]
                           [s2 "aéllo"])
                      (and (equal? (string-utf-8-length s1) 8)
                           (equal? (string-utf-8-length "hello") 5)
                           (equal? (string-utf-8-length s2 1 4) 4))))

              (list "string UTF-8 range errors are catchable with who"
                    (let ([catch-contract/who+expected+given?
                           (lambda (who expected given thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (string-contains? message who)
                                    (string-contains? message expected)
                                    (string-contains? message given))))]
                          [catch-range/who+reason+index?
                           (lambda (who reason index-line thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (string-contains? message who)
                                    (string-contains? message reason)
                                    (string-contains? message index-line)
                                    (string-contains? message "valid range: [0, 3]")
                                    (string-contains? message "byte string: \"abc\""))))])
                      (and
                       (catch-contract/who+expected+given?
                        "string->bytes/utf-8"
                        "string?"
                        "given: 42"
                        (lambda () (string->bytes/utf-8 42)))
                       (catch-contract/who+expected+given?
                        "string->bytes/utf-8"
                        "exact-nonnegative-integer?"
                        "given: 'bad"
                        (lambda () (string->bytes/utf-8 "abc" #f 'bad)))
                       (catch-contract/who+expected+given?
                        "string->bytes/utf-8"
                        "exact-nonnegative-integer?"
                        "given: -1"
                        (lambda () (string->bytes/utf-8 "abc" #f -1)))
                       (catch-range/who+reason+index?
                        "string->bytes/utf-8"
                        "starting index is out of range"
                        "starting index: 4"
                        (lambda () (string->bytes/utf-8 "abc" #f 4)))
                       (catch-range/who+reason+index?
                        "string->bytes/utf-8"
                        "ending index is smaller than starting index"
                        "ending index: 1"
                        (lambda () (string->bytes/utf-8 "abc" #f 2 1)))
                       (catch-contract/who+expected+given?
                        "string-utf-8-length"
                        "string?"
                        "given: 42"
                        (lambda () (string-utf-8-length 42)))
                       (catch-contract/who+expected+given?
                        "string-utf-8-length"
                        "exact-nonnegative-integer?"
                        "given: 'bad"
                        (lambda () (string-utf-8-length "abc" 'bad)))
                       (catch-contract/who+expected+given?
                        "string-utf-8-length"
                        "exact-nonnegative-integer?"
                        "given: -1"
                        (lambda () (string-utf-8-length "abc" -1)))
                       (catch-range/who+reason+index?
                        "string-utf-8-length"
                        "ending index is out of range"
                        "ending index: 4"
                        (lambda () (string-utf-8-length "abc" 1 4))))))

              (list "string->bytes/locale"
                    (and (bytes=? (string->bytes/locale "abc") #"abc")
                         (bytes=? (string->bytes/locale "aλ") #"a\xce\xbb")
                         (bytes=? (string->bytes/locale "abcdef" #f 1 4) #"bcd")))

              (list "byte-regexp/envvar-name"
                    (and (regexp-match? #rx#"^[^\0=]*$" #"PLTPLANETDIR")
                         (regexp-match? #rx#"^[^\0=]*$" #"")
                         (not (regexp-match? #rx#"^[^\0=]*$" #"A=B"))))

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

              (list "byte text decoding errors are catchable with who"
                    (let ([catch-contract/who+expected+given?
                           (lambda (who expected given thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (string-contains? message who)
                                    (string-contains? message expected)
                                    (string-contains? message given))))]
                          [catch-range/who+reason+index?
                           (lambda (who reason index-line thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (string-contains? message who)
                                    (string-contains? message reason)
                                    (string-contains? message index-line)
                                    (string-contains? message "valid range: [0, 3]")
                                    (string-contains? message "byte string:"))))])
                      (and
                       (catch-contract/who+expected+given?
                        "bytes-utf-8-length"
                        "bytes?"
                        "given: 42"
                        (lambda () (bytes-utf-8-length 42)))
                       (catch-contract/who+expected+given?
                        "bytes-utf-8-length"
                        "(or/c char? #f)"
                        "given: 42"
                        (lambda () (bytes-utf-8-length #"abc" 42)))
                       (catch-contract/who+expected+given?
                        "bytes-utf-8-length"
                        "exact-nonnegative-integer?"
                        "given: 'bad"
                        (lambda () (bytes-utf-8-length #"abc" #f 'bad)))
                       (catch-contract/who+expected+given?
                        "bytes-utf-8-length"
                        "exact-nonnegative-integer?"
                        "given: -1"
                        (lambda () (bytes-utf-8-length #"abc" #f -1)))
                       (catch-range/who+reason+index?
                        "bytes-utf-8-length"
                        "starting index is out of range"
                        "starting index: 4"
                        (lambda () (bytes-utf-8-length #"abc" #f 4)))
                       (catch-range/who+reason+index?
                        "bytes-utf-8-length"
                        "ending index is out of range"
                        "ending index: 4"
                        (lambda () (bytes-utf-8-length #"abc" #f 1 4)))
                       (catch-range/who+reason+index?
                        "bytes-utf-8-length"
                        "ending index is smaller than starting index"
                        "ending index: 1"
                        (lambda () (bytes-utf-8-length #"abc" #f 2 1)))
                       (catch-contract/who+expected+given?
                        "bytes->string/latin-1"
                        "bytes?"
                        "given: 42"
                        (lambda () (bytes->string/latin-1 42)))
                       (catch-contract/who+expected+given?
                        "bytes->string/latin-1"
                        "(or/c char? #f)"
                        "given: 42"
                        (lambda () (bytes->string/latin-1 #"abc" 42)))
                       (catch-contract/who+expected+given?
                        "bytes->string/latin-1"
                        "exact-nonnegative-integer?"
                        "given: -1"
                        (lambda () (bytes->string/latin-1 #"abc" #f -1)))
                       (catch-range/who+reason+index?
                        "bytes->string/latin-1"
                        "starting index is out of range"
                        "starting index: 4"
                        (lambda () (bytes->string/latin-1 #"abc" #f 4))))))

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

              (list "character scalar conversion argument errors are catchable with who"
                    (let ([catch-contract/who+expected?
                           (lambda (who expected thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and (string? message)
                                    (string-contains? message who)
                                    (string-contains? message expected))))])
                      (and (catch-contract/who+expected? "char->integer"
                                                         "char?"
                                                         (lambda () (char->integer 42)))
                           (catch-contract/who+expected? "char-utf-8-length"
                                                         "char?"
                                                         (lambda () (char-utf-8-length 42))))))

              (list "char=?"
                    (and (equal? (char=? #\A #\A) #t)
                         (equal? (char=? #\A #\B) #f)
                         (equal? (char=? #\A #\A #\A) #t)))

              (list "char<?"
                    (and (equal? (char<? #\A #\B) #t)
                         (equal? (char<? #\b #\A) #f)
                         ;; regression: variadic char<? via apply must dispatch correctly
                         (equal? (apply char<? (list #\a #\b)) #t)))

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
                         (let*-values ([(consumed  state)  (char-grapheme-step #\a 0)]
                                       [(consumed2 state2) (char-grapheme-step #\tab state)])
                           (and (eq? consumed  #f)
                                (eq? consumed2 #t)
                                (= state2 4)))
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

              (list "character case conversion argument errors are catchable with who"
                    (let ([catch-contract/who+expected?
                           (lambda (who expected thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and (string? message)
                                    (string-contains? message who)
                                    (string-contains? message expected))))])
                      (and (catch-contract/who+expected? "char-upcase"
                                                         "char?"
                                                         (lambda () (char-upcase 42)))
                           (catch-contract/who+expected? "char-downcase"
                                                         "char?"
                                                         (lambda () (char-downcase 42)))
                           (catch-contract/who+expected? "char-titlecase"
                                                         "char?"
                                                         (lambda () (char-titlecase 42)))
                           (catch-contract/who+expected? "char-foldcase"
                                                         "char?"
                                                         (lambda () (char-foldcase 42))))))

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
                (let (#;[and list] #;[equal? list])
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

              (list "string->unreadable-symbol"
                    (let ([s (string->unreadable-symbol "apple")])
                      (and (symbol? s)
                           (equal? (symbol->string s) "apple")
                           (equal? (symbol-interned? s) #f)
                           (equal? (symbol-unreadable? s) #t)
                           (equal? (symbol-unreadable? 'apple) #f))))

              (list "symbol<?"
                    (and (equal? (symbol<? 'a 'b)    #t)
                         ;(equal? (symbol<? 'a 'b 'c) #t)  ; todo - make symbol<? variadic
                         ;(equal? (symbol<? 'a 'c 'b) #f)
                         (equal? (symbol<? 'a 'aa)   #t)
                         (equal? (symbol<? 'aa 'a)   #f)
                         (let ([a (procedure-arity symbol<?)])
                           (and (arity-at-least? a)
                                (equal? (arity-at-least-value a) 1)))))

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
                           (equal? (eq? s1 s3)                                   #f))))

              (list "symbol argument errors are catchable with who"
                    (let ([catch-contract/who+expected+given?
                           (lambda (who expected thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and (string? message)
                                    (string-contains? message who)
                                    (string-contains? message expected)
                                    (string-contains? message "42"))))])
                      (and (catch-contract/who+expected+given? "symbol=?"
                                                               "symbol?"
                                                               (lambda () (symbol=? 42 'a)))
                           (catch-contract/who+expected+given? "symbol=?"
                                                               "symbol?"
                                                               (lambda () (symbol=? 'a 42)))
                           (catch-contract/who+expected+given? "symbol->string"
                                                               "symbol?"
                                                               (lambda () (symbol->string 42)))
                           (catch-contract/who+expected+given? "symbol->immutable-string"
                                                               "symbol?"
                                                               (lambda () (symbol->immutable-string 42)))
                           (catch-contract/who+expected+given? "string->symbol"
                                                               "string?"
                                                               (lambda () (string->symbol 42)))
                           (catch-contract/who+expected+given? "string->uninterned-symbol"
                                                               "string?"
                                                               (lambda () (string->uninterned-symbol 42)))
                           (catch-contract/who+expected+given? "string->unreadable-symbol"
                                                               "string?"
                                                               (lambda () (string->unreadable-symbol 42)))
                           (catch-contract/who+expected+given? "symbol-interned?"
                                                               "symbol?"
                                                               (lambda () (symbol-interned? 42)))
                           (catch-contract/who+expected+given? "symbol-unreadable?"
                                                               "symbol?"
                                                               (lambda () (symbol-unreadable? 42)))
                           (catch-contract/who+expected+given? "symbol<?"
                                                               "symbol?"
                                                               (lambda () (symbol<? 42 'a)))
                           (catch-contract/who+expected+given? "symbol<?"
                                                               "symbol?"
                                                               (lambda () (symbol<? 'a 42)))
                           (catch-contract/who+expected+given? "gensym"
                                                               "(or/c symbol? string?)"
                                                               (lambda () (gensym 42))))))))

       (list "4.8 Regular Expressions"
             (list
              (list "regexp literal values"
                    (and (equal? (object-name #rx"a") "a")
                         (equal? (object-name #px"a") "a")
                         (equal? (object-name #rx#"a") #"a")
                         (equal? (object-name #px#"a") #"a")
                         (equal? (equal? #rx"a" #rx"a") #t)
                         (equal? (equal? #rx"a" #px"a") #f)
                         (equal? (equal? #rx#"a" #rx#"a") #t)
                         (equal? (equal? #rx#"a" #px#"a") #f)))
              (list "regexp-match? bootstrap subset"
                    (and (equal? (regexp-match? #rx"a" "cat") #t)
                         (equal? (regexp-match? #rx"z" "cat") #f)
                         (equal? (regexp-match? #rx"[.]" "x.y") #t)
                         (equal? (regexp-match? #rx"[.]zo$" "x.zo") #t)
                         (equal? (regexp-match? #rx"[.]zo$" "x.zot") #f)
                         (equal? (regexp-match? #rx"^id(:|$)" "id:rest") #t)
                         (equal? (regexp-match? #rx"^id(:|$)" "idea") #f)))
              (list "byte-regexp bootstrap subset"
                    (and (byte-regexp? (byte-regexp #"a"))
                         (byte-pregexp? (byte-pregexp #"a"))
                         (equal? (regexp-match? (byte-regexp #"a") #"cat") #t)
                         (equal? (regexp-match? (byte-regexp #"z") #"cat") #f)
                         (equal? (regexp-match? #rx#"[.]" #"x.y") #t)
                         (equal? (regexp-match? #rx#"[.]zo$" #"x.zo") #t)
                         (equal? (regexp-match? #rx#"[.]zo$" #"x.zot") #f)
                         (equal? (regexp-match? #rx#"^id(:|$)" #"id:rest") #t)
                         (equal? (regexp-match? #rx#"^id(:|$)" #"idea") #f)
                         (equal? (regexp-match? #rx#"\0" #"abc") #f)))
              (list "regexp-match byte path-list bootstrap"
                    (and (equal? (regexp-match (byte-regexp #"([^:]*):(.*)") #"/a:/b")
                                 (list #"/a:/b" #"/a" #"/b"))
                         (equal? (regexp-match (byte-regexp #"([^:]*):(.*)") #"/a")
                                 #f)
                         (equal? (regexp-match (byte-regexp #"([^;]*);(.*)") #"c;d")
                                 (list #"c;d" #"c" #"d"))))
              (list "regexp-match string captures bootstrap"
                    (and (equal? (regexp-match #px"^([1-8]),([1-8])$" "2,2")
                                 (list "2,2" "2" "2"))
                         (equal? (regexp-match #px"^([1-8]),([1-8])$" "8,1")
                                 (list "8,1" "8" "1"))
                         (equal? (regexp-match #px"^([1-8]),([1-8])$" "2,9") #f)
                         (equal? (regexp-match #px"^([1-8]),([1-8])$" "22") #f)))
              (list "regexp-replace bootstrap subset"
                    (and (equal? (regexp-replace #rx"\n" "a\nb" " ") "a b")
                         (equal? (regexp-replace* #rx"-" "a-b-c" "_") "a_b_c")
                         (equal? (regexp-replace #rx"z" "abc" "x") "abc")))
              (list "datum-intern-literal bootstrap"
                    (let ([s (string #\a #\b)]
                          [b (bytes 1 2)])
                      (and (immutable-string? (datum-intern-literal s))
                           (immutable-bytes? (datum-intern-literal b))
                           (eq? (datum-intern-literal 'sym) 'sym))))))

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

                         #;(equal? (procedure-arity keyword<?) 2)))
              (list "keyword argument errors are catchable with who"
                    (let ([catch-contract/who+expected?
                           (lambda (who expected thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and (string? message)
                                    (string-contains? message who)
                                    (string-contains? message expected))))])
                      (and (catch-contract/who+expected? "string->keyword"
                                                         "string?"
                                                         (lambda () (string->keyword 42)))
                           (catch-contract/who+expected? "keyword->string"
                                                         "keyword?"
                                                         (lambda () (keyword->string 42)))
                           (catch-contract/who+expected? "keyword->immutable-string"
                                                         "keyword?"
                                                         (lambda () (keyword->immutable-string 42)))
                           (catch-contract/who+expected? "keyword<?"
                                                         "keyword?"
                                                         (lambda () (keyword<? 42 '#:a)))
                           (catch-contract/who+expected? "keyword<?"
                                                         "keyword?"
                                                         (lambda () (keyword<? '#:a 42))))))))

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

              (list "list and pair argument errors are catchable with who"
                    (let ([catch-contract/who?
                           (lambda (who thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and (string? message)
                                    (string-contains? message who))))])
                      (and (catch-contract/who? "car"
                                                (lambda () (car 42)))
                           (catch-contract/who? "cdr"
                                                (lambda () (cdr 42)))
                           (catch-contract/who? "length"
                                                (lambda () (length '(a . b))))
                           (catch-contract/who? "make-list"
                                                (lambda () (make-list -1 'x)))
                           (catch-contract/who? "make-list"
                                                (lambda () (make-list #\a 'x)))
                           (catch-contract/who? "build-list"
                                                (lambda () (build-list -1 values)))
                           (catch-contract/who? "build-list"
                                                (lambda () (build-list #\a values)))
                           (catch-contract/who? "list-ref"
                                                (lambda () (list-ref 42 0)))
                           (catch-contract/who? "list-ref"
                                                (lambda () (list-ref '(a) -1)))
                           (catch-contract/who? "list-ref"
                                                (lambda () (list-ref '(a) 1)))
                           (catch-contract/who? "list-tail"
                                                (lambda () (list-tail 42 1)))
                           (catch-contract/who? "list-tail"
                                                (lambda () (list-tail '(a . b) 2)))
                           (catch-contract/who? "list-set"
                                                (lambda () (list-set '(a . b) 0 'x)))
                           (catch-contract/who? "list-update"
                                                (lambda () (list-update 42 0 values)))
                           (catch-contract/who? "list-update"
                                                (lambda () (list-update '(a) 1 values)))
                           (catch-contract/who? "rest"
                                                (lambda () (rest '())))
                           (catch-contract/who? "take"
                                                (lambda () (take '(a) 2)))
                           (catch-contract/who? "take-right"
                                                (lambda () (take-right '(a) 2)))
                           (catch-contract/who? "drop"
                                                (lambda () (drop '(a) 2)))
                           (catch-contract/who? "drop-right"
                                                (lambda () (drop-right '(a) 2)))
                           (catch-contract/who? "split-at"
                                                (lambda () (split-at '(a) 2)))
                           (catch-contract/who? "split-at-right"
                                                (lambda () (split-at-right '(a) 2)))
                           (catch-contract/who? "list-prefix?"
                                                (lambda () (list-prefix? '(a . b) '(a))))
                           (catch-contract/who? "take-common-prefix"
                                                (lambda () (take-common-prefix '(a . b) '(a))))
                           (catch-contract/who? "drop-common-prefix"
                                                (lambda () (drop-common-prefix '(a . b) '(a))))
                           (catch-contract/who? "split-common-prefix"
                                                (lambda () (split-common-prefix '(a . b) '(a)))))))

              (list "list higher-order argument errors are catchable with who"
                    (let ([catch-contract/who?
                           (lambda (who thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and (string? message)
                                    (string-contains? message who))))])
                      (and (catch-contract/who? "map"
                                                (lambda () (map 42 '(a))))
                           (catch-contract/who? "map"
                                                (lambda () (map values '(a . b))))
                           (catch-contract/who? "andmap"
                                                (lambda () (andmap 42 '(a))))
                           (catch-contract/who? "ormap"
                                                (lambda () (ormap 42 '(a))))
                           (catch-contract/who? "foldl"
                                                (lambda () (foldl 42 'z '(a))))
                           (catch-contract/who? "foldl"
                                                (lambda () (foldl cons 'z '(a . b))))
                           (catch-contract/who? "foldr"
                                                (lambda () (foldr 42 'z '(a))))
                           (catch-contract/who? "foldr"
                                                (lambda () (foldr cons 'z '(a . b))))
                           (catch-contract/who? "filter"
                                                (lambda () (filter 42 '(a))))
                           (catch-contract/who? "filter"
                                                (lambda () (filter values '(a . b))))
                           (catch-contract/who? "filter-not"
                                                (lambda () (filter-not 42 '(a))))
                           (catch-contract/who? "filter-map"
                                                (lambda () (filter-map 42 '(a))))
                           (catch-contract/who? "filter-map"
                                                (lambda () (filter-map list '(a . b))))
                           (catch-contract/who? "partition"
                                                (lambda () (partition 42 '(a))))
                           (catch-contract/who? "map"
                                                (lambda () (append-map 42 '(a))))
                           (catch-contract/who? "map"
                                                (lambda () (append-map list '(a . b))))
                           (catch-contract/who? "sort"
                                                (lambda () (sort '(a . b) <)))
                           (catch-contract/who? "sort"
                                                (lambda () (sort '(a) 42)))
                           (catch-contract/who? "sort"
                                                (lambda () (sort '(a) < #:key 42)))
                           (catch-contract/who? "build-list"
                                                (lambda () (build-list 2 42)))
                           (catch-contract/who? "list-update"
                                                (lambda () (list-update '(a) 0 42)))
                           (catch-contract/who? "argmax"
                                                (lambda () (argmax 42 '(1))))
                           (catch-contract/who? "argmax"
                                                (lambda () (argmax values '())))
                           (catch-contract/who? "argmax"
                                                (lambda () (argmax values '(1 . 2))))
                           (catch-contract/who? "argmin"
                                                (lambda () (argmin 42 '(1))))
                           (catch-contract/who? "argmin"
                                                (lambda () (argmin values '())))
                           (catch-contract/who? "count"
                                                (lambda () (count 42 '(a))))
                           (catch-contract/who? "count"
                                                (lambda () (count values '(a . b))))
                           (catch-contract/who? "for-each"
                                                (lambda () (for-each 42 '(a))))
                           (catch-contract/who? "for-each"
                                                (lambda () (for-each values '(a . b))))
                           (catch-contract/who? "member"
                                                (lambda () (member 'a '(a) 42)))
                           (catch-contract/who? "member"
                                                (lambda () (member 'z '(a . b))))
                           (catch-contract/who? "memf"
                                                (lambda () (memf 42 '(a))))
                           (catch-contract/who? "assoc"
                                                (lambda () (assoc 'a '((a . 1)) 42)))
                           (catch-contract/who? "assoc"
                                                (lambda () (assoc 'z '((a . 1) . b))))
                           (catch-contract/who? "assoc"
                                                (lambda () (assoc 'a '(x))))
                           (catch-contract/who? "assf"
                                                (lambda () (assf 42 '((a . 1)))))
                           (catch-contract/who? "index-of"
                                                (lambda () (index-of '(a) 'a 42)))
                           (catch-contract/who? "index-of"
                                                (lambda () (index-of '(a . b) 'z)))
                           (catch-contract/who? "index-where"
                                                (lambda () (index-where '(a) 42)))
                           (catch-contract/who? "indexes-of"
                                                (lambda () (indexes-of '(a) 'a 42)))
                           (catch-contract/who? "indexes-where"
                                                (lambda () (indexes-where '(a) 42)))
                           (catch-contract/who? "group-by"
                                                (lambda () (group-by 42 '(a))))
                           (catch-contract/who? "group-by"
                                                (lambda () (group-by values '(a) 42)))
                           (catch-contract/who? "remove"
                                                (lambda () (remove 'a '(a) 42)))
                           (catch-contract/who? "remove"
                                                (lambda () (remove 'z '(a . b))))
                           (catch-contract/who? "remove*"
                                                (lambda () (remove* '(z . q) '(a))))
                           (catch-contract/who? "remf"
                                                (lambda () (remf 42 '(a))))
                           (catch-contract/who? "remf*"
                                                (lambda () (remf* 42 '(a))))
                           (catch-contract/who? "takef"
                                                (lambda () (takef '(a) 42)))
                           (catch-contract/who? "takef-right"
                                                (lambda () (takef-right '(a) 42)))
                           (catch-contract/who? "dropf"
                                                (lambda () (dropf '(a) 42)))
                           (catch-contract/who? "dropf-right"
                                                (lambda () (dropf-right '(a) 42)))
                           (catch-contract/who? "splitf-at"
                                                (lambda () (splitf-at '(a) 42)))
                           (catch-contract/who? "splitf-at-right"
                                                (lambda () (splitf-at-right '(a) 42)))
                           (catch-contract/who? "list-prefix?"
                                                (lambda () (list-prefix? '(a) '(a) 42)))
                           (catch-contract/who? "take-common-prefix"
                                                (lambda () (take-common-prefix '(a) '(a) 42)))
                           (catch-contract/who? "drop-common-prefix"
                                                (lambda () (drop-common-prefix '(a) '(a) 42)))
                           (catch-contract/who? "split-common-prefix"
                                                (lambda () (split-common-prefix '(a) '(a) 42))))))

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
                                 '((c . z) (b . y) (a . x)))
                         (with-handlers ([exn:fail:contract? (λ (ex) #t)])
                           (foldl 'list 0 10)
                           #f)))
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
                         (equal? (range 10 15 1.5) '(10 11.5 13.0 14.5))))

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
                         (equal? (memq  "apple" '( "apple"))         '("apple"))
                         ; depends on interning byte string literals
                         #;(equal? (memq #"apple" '(#"apple"))         '(#"apple"))
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
              (list "mutable-pair argument errors are catchable with who"
                    (let ([catch-contract/who+expected+given?
                           (lambda (who thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and (string? message)
                                    (string-contains? message who)
                                    (string-contains? message "mpair?")
                                    (string-contains? message "42"))))])
                      (and (catch-contract/who+expected+given?
                            "mcar"
                            (lambda () (mcar 42)))
                           (catch-contract/who+expected+given?
                            "mcdr"
                            (lambda () (mcdr 42)))
                           (catch-contract/who+expected+given?
                            "set-mcar!"
                            (lambda () (set-mcar! 42 'x)))
                           (catch-contract/who+expected+given?
                            "set-mcdr!"
                            (lambda () (set-mcdr! 42 'x))))))
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

              (list "vector/list conversion argument errors are catchable with who"
                    (let ([catch-contract/who+expected?
                           (lambda (who expected thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and (string? message)
                                    (string-contains? message who)
                                    (string-contains? message expected))))])
                      (and (catch-contract/who+expected? "vector->list"
                                                         "vector?"
                                                         (lambda () (vector->list 42)))
                           (catch-contract/who+expected? "list->vector"
                                                         "list?"
                                                         (lambda () (list->vector 42)))
                           (catch-contract/who+expected? "list->vector"
                                                         "list?"
                                                         (lambda () (list->vector (cons 'a 42)))))))

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

              (list "vector*/checked"
                    (let ([v (vector 'a 'b 'c)])
                      (and (equal? (vector*-length v) 3)
                           (equal? (vector*-ref v 1) 'b)
                           (equal? (vector*-set! v 1 'changed) (void))
                           (equal? (vector*-ref v 1) 'changed))))

              (list "vector argument errors are catchable with who"
                    (let ([catch-contract/who?
                           (lambda (who thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and (string? message)
                                    (string-contains? message who))))])
                      (and (catch-contract/who? "make-vector"
                                                (lambda () (make-vector -1)))
                           (catch-contract/who? "make-vector"
                                                (lambda () (make-vector #f)))
                           (catch-contract/who? "build-vector"
                                                (lambda () (build-vector #f values)))
                           (catch-contract/who? "build-vector"
                                                (lambda () (build-vector 1 #f)))
                           (catch-contract/who? "vector-length"
                                                (lambda () (vector-length 42)))
                           (catch-contract/who? "vector-ref"
                                                (lambda () (vector-ref (vector 'a) 'zero)))
                           (catch-contract/who? "vector-ref"
                                                (lambda () (vector-ref (vector 'a) 1)))
                           (catch-contract/who? "vector-set!"
                                                (lambda () (vector-set! 42 0 'x)))
                           (catch-contract/who? "vector-set!"
                                                (lambda () (vector-set! (vector-immutable 'a) 0 'x)))
                           (catch-contract/who? "vector-fill!"
                                                (lambda () (vector-fill! (vector-immutable 'a) 'x)))
                           (catch-contract/who? "vector*-ref"
                                                (lambda () (vector*-ref (vector 'a) 1)))
                           (catch-contract/who? "vector*-set!"
                                                (lambda () (vector*-set! (vector-immutable 'a) 0 'x)))
                           (catch-contract/who? "vector-map"
                                                (lambda () (vector-map #f '#(1))))
                           (catch-contract/who? "vector-map!"
                                                (lambda () (vector-map! #f (vector 1))))
                           (catch-contract/who? "vector-count"
                                                (lambda () (vector-count #f '#(1))))
                           (catch-contract/who? "vector-argmin"
                                                (lambda () (vector-argmin #f '#(1))))
                           (catch-contract/who? "vector-filter"
                                                (lambda () (vector-filter #f '#(1))))
                           (catch-contract/who? "vector-member"
                                                (lambda () (vector-member 1 '#(1) #f)))
                           (catch-contract/who? "vector-sort"
                                                (lambda () (vector-sort '#(1) #f)))
                           (catch-contract/who? "vector-sort!"
                                                (lambda () (vector-sort! (vector 1) #f))))))

              (list "vector range errors are catchable with who"
                    (let ([catch-contract/who?
                           (lambda (who thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and (string? message)
                                    (string-contains? message who))))])
                      (and (catch-contract/who? "vector-set/copy"
                                                (lambda () (vector-set/copy (vector 'a) 1 'x)))
                           (catch-contract/who? "vector-copy"
                                                (lambda () (vector-copy (vector 'a) 1 0)))
                           (catch-contract/who? "vector-copy!"
                                                (lambda () (vector-copy! (vector 'x) 1 (vector 'a 'b) 0 2)))
                           (catch-contract/who? "vector-copy!"
                                                (lambda () (vector-copy! (vector-immutable 'x) 0 (vector 'a))))
                           (catch-contract/who? "vector->immutable-vector"
                                                (lambda () (vector->immutable-vector 42)))
                           (catch-contract/who? "vector->values"
                                                (lambda () (vector->values (vector 'a) 1 0)))
                           (catch-contract/who? "vector-extend"
                                                (lambda () (vector-extend (vector 'a 'b) 1)))
                           (catch-contract/who? "vector-empty?"
                                                (lambda () (vector-empty? 42)))
                           (catch-contract/who? "vector-append"
                                                (lambda () (vector-append (vector 'a) 42)))
                           (catch-contract/who? "vector-take"
                                                (lambda () (vector-take (vector 'a) 2)))
                           (catch-contract/who? "vector-drop"
                                                (lambda () (vector-drop (vector 'a) 2)))
                           (catch-contract/who? "vector-take-right"
                                                (lambda () (vector-take-right (vector 'a) 2)))
                           (catch-contract/who? "vector-drop-right"
                                                (lambda () (vector-drop-right (vector 'a) 2)))
                           (catch-contract/who? "vector-split-at"
                                                (lambda () (vector-split-at (vector 'a) 2)))
                           (catch-contract/who? "vector-split-at-right"
                                                (lambda () (vector-split-at-right (vector 'a) 2)))
                           (catch-contract/who? "vector-sort!"
                                                (lambda () (vector-sort! (vector 2 1) < 2 1))))))

              (list "stencil-vector"
                    (let ([sv (stencil-vector #b101 'a 'b)])
                      (and (stencil-vector? sv)
                           ($stencil-vector? sv)
                           (equal? (stencil-vector-mask-width) 26)
                           (equal? (stencil-vector-mask sv) #b101)
                           (equal? (stencil-vector-length sv) 2)
                           (equal? (stencil-vector-ref sv 0) 'a)
                           (equal? (stencil-vector-ref sv 1) 'b))))

              (list "stencil-vector-set!"
                    (let ([sv (stencil-vector #b11 'a 'b)])
                      (stencil-vector-set! sv 1 'c)
                      (and (equal? (stencil-vector-ref sv 0) 'a)
                           (equal? (stencil-vector-ref sv 1) 'c))))

              (list "stencil-vector-update"
                    (let* ([sv  (stencil-vector #b101 'a 'b)]
                           [sv2 (stencil-vector-update sv #b001 #b010 'x)]
                           [sv3 (stencil-vector-update sv2 #b100 #b100 'z)])
                      (and (equal? (stencil-vector-mask sv2) #b110)
                           (equal? (stencil-vector-length sv2) 2)
                           (equal? (stencil-vector-ref sv2 0) 'x)
                           (equal? (stencil-vector-ref sv2 1) 'b)
                           (equal? (stencil-vector-mask sv3) #b110)
                           (equal? (stencil-vector-ref sv3 0) 'x)
                           (equal? (stencil-vector-ref sv3 1) 'z))))

              (list "system-stencil-vector"
                    (let* ([sv  ($system-stencil-vector #b101 'a 'b)]
                           [sv2 ($system-stencil-vector-update sv #b001 #b010 'x)])
                      (and (not (stencil-vector? sv))
                           ($stencil-vector? sv)
                           ($system-stencil-vector? sv)
                           (equal? ($stencil-vector-mask sv2) #b110)
                           (equal? ($stencil-vector-ref sv2 0) 'x)
                           (equal? ($stencil-vector-ref sv2 1) 'b))))

              (list "unsafe-vector-length"
                    (equal? (unsafe-vector-length '#(1 2 3)) 3))

              (list "impersonator?/none"
                    (equal? (impersonator? (vector 1 2 3)) #f))

              (list "procedure-impersonator*?/none"
                    (equal? (procedure-impersonator*? (lambda (x) x)) #f))

              (list "procedure-result-arity/unknown-closure"
                    (equal? (procedure-result-arity (lambda (x) x)) #f))

              (list "call-with-immediate-continuation-mark/default"
                    (equal? (call-with-immediate-continuation-mark
                             'k
                             (lambda (v) v)
                             'fallback)
                            'fallback))

              (list "chaperone?/none"
                    (equal? (chaperone? (vector 1 2 3)) #f))

              (list "chaperone-procedure/bridge"
                    (let* ([proc (lambda (x) (+ x 1))]
                           [wrapped (chaperone-procedure proc
                                                         (lambda (x) (+ x 2)))])
                      (and (procedure? wrapped)
                           (equal? (wrapped 40) 42)
                           (eq? (chaperone-procedure proc #f) proc))))

              (list "chaperone-struct/identity-bridge"
                    (let ()
                      (struct point (x))
                      (define p (point 42))
                      (eq? (chaperone-struct p point? #f) p)))

              (list "chaperone-vector-box-hash/identity-bridge"
                    (let ([v (vector 1 2)]
                          [b (box 1)]
                          [h (hash 'a 1)])
                      (and (eq? (chaperone-vector v #f #f) v)
                           (eq? (chaperone-box b
                                               (lambda (box v) v)
                                               (lambda (box v) v))
                                b)
                           (eq? (chaperone-hash h
                                                (lambda (hash key) (values key values))
                                                (lambda (hash key val) (values key val))
                                                (lambda (hash key) key)
                                                (lambda (hash key) key))
                                h))))

              (list "make-impersonator-property/inert"
                    (let-values ([(prop has-prop? get-prop)
                                  (make-impersonator-property 'web-impersonator-prop)])
                      (and (impersonator-property? prop)
                           (impersonator-property-accessor-procedure? get-prop)
                           (equal? (has-prop? 'plain-value) #f)
                           (equal? (get-prop 'plain-value 'fallback) 'fallback)
                           (equal? (get-prop 'plain-value (lambda () 'fallback-thunk))
                                   'fallback-thunk))))

              (list "unsafe-struct?"
                    (let-values ([(struct:parent make-parent parent? parent-ref parent-set!)
                                  (make-struct-type 'unsafe-parent #f 1 0 #f)]
                                 [(struct:other make-other other? other-ref other-set!)
                                  (make-struct-type 'unsafe-other #f 1 0 #f)])
                      (let ([parent (make-parent 1)]
                            [other  (make-other 3)])
                        (and (equal? (unsafe-struct? parent struct:parent) #t)
                             (equal? (unsafe-struct? other struct:parent) #f)
                             (equal? (unsafe-struct? 10 struct:parent) #f)))))

              (list "unsafe-sealed-struct?"
                    (let-values ([(struct:sealed make-sealed sealed? sealed-ref sealed-set!)
                                  (make-struct-type 'unsafe-sealed #f 1 0 #f)]
                                 [(struct:other make-other other? other-ref other-set!)
                                  (make-struct-type 'unsafe-sealed-other #f 1 0 #f)])
                      (let ([sealed (make-sealed 1)]
                            [other  (make-other 3)])
                        (and (equal? (unsafe-sealed-struct? sealed struct:sealed) #t)
                             (equal? (unsafe-sealed-struct? other struct:sealed) #f)
                             (equal? (unsafe-sealed-struct? 10 struct:sealed) #f)))))

              (list "unsafe-struct*-ref"
                    (let-values ([(struct:s make-s s? s-ref s-set!)
                                  (make-struct-type 'unsafe-star #f 3 0 #f)])
                      (let ([s (make-s 1 2 3)])
                        (and (equal? (unsafe-struct*-ref s 0) 1)
                             (equal? (unsafe-struct*-ref s 1) 2)
                             (equal? (unsafe-struct*-ref s 2) 3)))))

              (list "unsafe-struct*-set!"
                    (let-values ([(struct:s make-s s? s-ref s-set!)
                                  (make-struct-type 'unsafe-star-mutable #f 3 0 #f null #f #f '(0 1 2))])
                      (let ([s (make-s 1 2 3)])
                        ;; WebRacket has no impersonators yet; the starred
                        ;; operations are raw struct field operations.
                        (unsafe-struct*-set! s 0 10)
                        (unsafe-struct*-set! s 2 30)
                        (and (equal? (unsafe-struct*-ref s 0) 10)
                             (equal? (unsafe-struct*-ref s 1) 2)
                             (equal? (unsafe-struct*-ref s 2) 30)))))

              (list "unsafe-struct*-cas!"
                    (let-values ([(struct:s make-s s? s-ref s-set!)
                                  (make-struct-type 'unsafe-star-cas #f 1 0 #f null #f #f '(0))])
                      (let ([s (make-s 'old)])
                        (and (equal? (unsafe-struct*-cas! s 0 'wrong 'new) #f)
                             (equal? (unsafe-struct*-ref s 0) 'old)
                             (equal? (unsafe-struct*-cas! s 0 'old 'new) #t)
                             (equal? (unsafe-struct*-ref s 0) 'new)))))

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

       (list "Treelists"
             (list
              (list "treelist?"
                    (and (equal? (treelist? empty-treelist) #t)
                         (equal? (treelist? (treelist)) #t)
                         (equal? (treelist? (treelist 1 2 3)) #t)
                         (equal? (treelist? 0) #f)))

              (list "treelist-empty?"
                    (and (equal? (treelist-empty? empty-treelist) #t)
                         (equal? (treelist-empty? (treelist)) #t)
                         (equal? (treelist-empty? (treelist 1)) #f)))

              (list "treelist dense sizes"
                    (let ([sizes '(0 1 31 32 33 1024 1025)])
                      (andmap
                       (lambda (n)
                         (let* ([vec  (build-vector n (lambda (i) i))]
                                [tl   (vector->treelist vec)]
                                [mid  (if (zero? n) 0 (quotient n 2))]
                                [last (if (zero? n) 0 (sub1 n))])
                           (and (equal? (treelist-length tl) n)
                                (equal? (treelist->vector tl) vec)
                                (or (zero? n)
                                    (and (equal? (treelist-ref tl 0) 0)
                                         (equal? (treelist-ref tl mid) mid)
                                         (equal? (treelist-ref tl last) last)
                                         (equal? (treelist-first tl) 0)
                                         (equal? (treelist-last tl) last))))))
                       sizes)))

              (list "treelist direct 33"
                    (let ([tl (treelist 0 1 2 3 4 5 6 7 8 9 10
                                        11 12 13 14 15 16 17 18 19 20 21
                                        22 23 24 25 26 27 28 29 30 31 32)])
                      (and (equal? (treelist-length tl) 33)
                           (equal? (treelist-ref tl 0) 0)
                           (equal? (treelist-ref tl 16) 16)
                           (equal? (treelist-ref tl 32) 32))))

              (list "treelist apply small"
                    (equal? (treelist-length (apply treelist (list 0 1 2 3))) 4))

              (list "treelist apply 40"
                    (equal? (treelist-ref (apply treelist (build-list 40 values)) 39) 39))

              (list "list->treelist roundtrip"
                    (let ([sizes '(0 1 31 32 33 1024 1025)])
                      (andmap
                       (lambda (n)
                         (let ([xs (build-list n (lambda (i) i))])
                           (equal? (treelist->list (list->treelist xs)) xs)))
                       sizes)))

              (list "make-treelist explicit fill"
                    (let ([sizes '(0 1 31 32 33 1024 1025)])
                      (andmap
                       (lambda (n)
                         (let ([vec (treelist->vector (make-treelist n 'x))])
                           (and (equal? (vector-length vec) n)
                                (or (zero? n)
                                    (and (equal? (vector-ref vec 0) 'x)
                                         (equal? (vector-ref vec (sub1 n)) 'x))))))
                       sizes)))

              (list "make-treelist default fill"
                    (let ([sizes '(0 1 31 32 33 1024 1025)])
                      (andmap
                       (lambda (n)
                         (let ([vec (treelist->vector (make-treelist n))])
                           (and (equal? (vector-length vec) n)
                                (or (zero? n)
                                    (and (equal? (vector-ref vec 0) #f)
                                         (equal? (vector-ref vec (sub1 n)) #f))))))
                       sizes)))

              (list "treelist-add preserves original"
                    (let* ([tl0 (treelist 1 2 3)]
                           [tl1 (treelist-add tl0 4)])
                      (and (equal? (treelist->list tl0) '(1 2 3))
                           (equal? (treelist->list tl1) '(1 2 3 4))
                           (equal? (treelist-length tl0) 3)
                           (equal? (treelist-length tl1) 4))))

              (list "treelist-set preserves original"
                    (let* ([tl0 (treelist 10 20 30)]
                           [tl1 (treelist-set tl0 1 'x)])
                      (and (equal? (treelist->list tl0) '(10 20 30))
                           (equal? (treelist->list tl1) '(10 x 30))
                           (equal? (treelist-ref tl0 1) 20)
                           (equal? (treelist-ref tl1 1) 'x))))

              (list "treelist-add boundary sizes"
                    (let ([sizes '(31 32 1024)])
                      (andmap
                       (lambda (n)
                         (let* ([tl0  (list->treelist (build-list n values))]
                                [tl1  (treelist-add tl0 'tail)]
                                [vec0 (treelist->vector tl0)]
                                [vec1 (treelist->vector tl1)])
                           (and (equal? (treelist-length tl0) n)
                                (equal? (treelist-length tl1) (add1 n))
                                (equal? (vector-length vec1) (add1 n))
                                (equal? (vector-ref vec1 0) 0)
                                (equal? (vector-ref vec1 (sub1 n)) (sub1 n))
                                (equal? (vector-ref vec1 n) 'tail)
                                (equal? (vector-length vec0) n)
                                (equal? (vector-ref vec0 (sub1 n)) (sub1 n)))))
                       sizes)))

              (list "treelist-add repeated preserves original"
                    (let* ([base (treelist 1 2 3)]
                           [tl   (for/fold ([acc base])
                                           ([i (in-range 64)])
                                   (treelist-add acc (+ 10 i)))])
                      (and (equal? (treelist->list base) '(1 2 3))
                           (equal? (treelist-length tl) 67)
                           (equal? (treelist-ref tl 0) 1)
                           (equal? (treelist-ref tl 2) 3)
                           (equal? (treelist-ref tl 3) 10)
                           (equal? (treelist-ref tl 66) 73))))

              (list "treelist-set dense sizes"
                    (let ([sizes '(1 31 32 33 1024 1025)])
                      (andmap
                       (lambda (n)
                         (let* ([xs    (build-list n values)]
                                [tl    (list->treelist xs)]
                                [mid   (quotient n 2)]
                                [last  (sub1 n)]
                                [tl-f  (treelist-set tl 0 'first)]
                                [tl-m  (treelist-set tl mid 'middle)]
                                [tl-l  (treelist-set tl last 'last)])
                           (and (equal? (treelist->list tl) xs)
                                (equal? (treelist-ref tl-f 0) 'first)
                                (equal? (treelist-ref tl-f last)
                                        (if (zero? last) 'first last))
                                (equal? (treelist-ref tl-m 0)
                                        (if (zero? mid) 'middle 0))
                                (equal? (treelist-ref tl-m mid) 'middle)
                                (equal? (treelist-ref tl-m last)
                                        (if (= mid last) 'middle last))
                                (equal? (treelist-ref tl-l 0)
                                        (if (zero? last) 'last 0))
                                (equal? (treelist-ref tl-l mid)
                                        (if (= mid last) 'last mid))
                                (equal? (treelist-ref tl-l last) 'last))))
                       sizes)))

              (list "treelist-take/drop persistence"
                    (let* ([tl (treelist 0 1 2 3 4)]
                           [tk (treelist-take tl 2)]
                           [dp (treelist-drop tl 2)])
                      (and (equal? (treelist->list tl) '(0 1 2 3 4))
                           (equal? (treelist->list tk) '(0 1))
                           (equal? (treelist->list dp) '(2 3 4)))))

              (list "treelist split dense sizes"
                    (let ([sizes '(0 1 31 32 33 1024 1025)])
                      (andmap
                       (lambda (n)
                         (let* ([xs    (build-list n values)]
                                [tl    (list->treelist xs)]
                                [mid   (quotient n 2)]
                                [last  (if (zero? n) #f (sub1 n))])
                           (and
                            (equal? (treelist->list tl) xs)
                            (andmap
                             (lambda (pos)
                               (let-values ([(left right) (treelist-split-at tl pos)])
                                 (and (equal? (treelist->list left)  (take xs pos))
                                      (equal? (treelist->list right) (drop xs pos))
                                      (equal? (treelist-length left) pos)
                                      (equal? (treelist-length right) (- n pos))
                                      (or (zero? pos)
                                          (equal? (treelist-ref left 0) 0))
                                      (or (zero? (- n pos))
                                          (equal? (treelist-ref right 0) pos))
                                      (equal? (treelist->list (treelist-take tl pos))
                                              (take xs pos))
                                      (equal? (treelist->list (treelist-drop tl pos))
                                              (drop xs pos))
                                      (equal? (treelist->list tl) xs))))
                             (list 0 mid n)))))
                       sizes)))

              (list "treelist split relaxed shapes"
                    (let* ([pieces (list (list->treelist (build-list 20 values))
                                         (list->treelist (build-list 20 (lambda (i) (+ 20 i))))
                                         (list->treelist (build-list 20 (lambda (i) (+ 40 i)))))]
                           [tl (apply treelist-append pieces)]
                           [xs (build-list 60 values)])
                      (and (equal? (treelist->list tl) xs)
                           (andmap
                            (lambda (pos)
                              (let-values ([(left right) (treelist-split-at tl pos)])
                                (and (equal? (treelist->list left) (take xs pos))
                                     (equal? (treelist->list right) (drop xs pos))
                                     (equal? (treelist-length left) pos)
                                     (equal? (treelist-length right) (- 60 pos))
                                     (or (zero? pos)
                                         (equal? (treelist-ref left (sub1 pos)) (sub1 pos)))
                                     (or (= pos 60)
                                         (equal? (treelist-ref right 0) pos))
                                     (equal? (treelist->list (treelist-take tl pos))
                                             (take xs pos))
                                     (equal? (treelist->list (treelist-drop tl pos))
                                             (drop xs pos))
                                     (equal? (treelist->list tl) xs))))
                            '(0 17 20 37 40 60)))))

              (list "treelist split relaxed boundaries"
                    (let* ([left  (list->treelist (build-list 32 values))]
                           [right (list->treelist (build-list 33 (lambda (i) (+ 32 i))))]
                           [tl    (treelist-append left right)]
                           [xs    (build-list 65 values)])
                      (and (equal? (treelist->list tl) xs)
                           (let-values ([(a b) (treelist-split-at tl 32)])
                             (and (equal? (treelist->list a) (take xs 32))
                                  (equal? (treelist->list b) (drop xs 32))
                                  (equal? (treelist-ref a 31) 31)
                                  (equal? (treelist-ref b 0) 32)))
                           (let-values ([(a b) (treelist-split-at tl 33)])
                             (and (equal? (treelist->list a) (take xs 33))
                                  (equal? (treelist->list b) (drop xs 33))
                                  (equal? (treelist-ref a 32) 32)
                                  (equal? (treelist-ref b 0) 33)))
                           (equal? (treelist->list (treelist-take tl 33)) (take xs 33))
                           (equal? (treelist->list (treelist-drop tl 33)) (drop xs 33)))))

              (list "treelist-append basics"
                    (and (equal? (treelist->list (treelist-append)) '())
                         (equal? (treelist->list (treelist-append (treelist 1 2))) '(1 2))
                         (equal? (treelist->list (treelist-append (treelist 1 2)
                                                                  (treelist 3 4)))
                                 '(1 2 3 4))
                         (equal? (treelist->list (treelist-append (treelist 1)
                                                                  (treelist)
                                                                  (treelist 2 3)))
                                 '(1 2 3))))

              (list "treelist-append apply and persistence"
                    (let* ([tl0 (list->treelist (build-list 32 values))]
                           [tl1 (list->treelist (build-list 1 (lambda (i) (+ i 100))))]
                           [tl2 (list->treelist (build-list 33 (lambda (i) (+ i 200))))]
                           [app (apply treelist-append (list tl0 tl1 tl2))])
                      (and (equal? (treelist->list tl0) (build-list 32 values))
                           (equal? (treelist-length app) (+ 32 1 33))
                           (equal? (treelist-ref app 0) 0)
                           (equal? (treelist-ref app 31) 31)
                           (equal? (treelist-ref app 32) 100)
                           (equal? (treelist-ref app 65) 232))))

              (list "treelist-append relaxed traversal"
                    (let* ([left  (make-treelist 17 'a)]
                           [right (make-treelist 17 'b)]
                           [app   (treelist-append left right)])
                      (and (equal? (treelist-length app) 34)
                           (equal? (treelist-ref app 16) 'a)
                           (equal? (treelist-ref app 17) 'b)
                           (equal? (treelist-first app) 'a)
                           (equal? (treelist-last app) 'b)
                           (equal? (treelist->list app)
                                   (append (make-list 17 'a) (make-list 17 'b)))
                           (equal? (treelist->list left) (make-list 17 'a))
                           (equal? (treelist->list right) (make-list 17 'b)))))

              (list "treelist-append many medium treelists"
                    (let* ([piece-count 100]
                           [piece-size  20]
                           [pieces
                            (for/list ([j (in-range piece-count)])
                              (list->treelist
                               (build-list piece-size
                                           (lambda (i) (+ (* j piece-size) i)))))]
                           [app (apply treelist-append pieces)])
                      (and (equal? (treelist-length app) (* piece-count piece-size))
                           (equal? (treelist-ref app 0) 0)
                           (equal? (treelist-ref app 19) 19)
                           (equal? (treelist-ref app 20) 20)
                           (equal? (treelist-ref app 199) 199)
                           (equal? (treelist-ref app 999) 999)
                           (equal? (treelist-ref app 1999) 1999)
                           (equal? (treelist->list app) (build-list 2000 values))
                           (andmap
                            (lambda (j tl)
                              (equal? (treelist->list tl)
                                      (build-list piece-size
                                                  (lambda (i) (+ (* j piece-size) i)))))
                            (build-list piece-count values)
                            pieces))))

              (list "treelist-append uneven chunk boundaries"
                    (let* ([chunk-sizes '(17 20 31 32 33 17 20 31 32 33)]
                           [pieces
                            (let loop ([sizes chunk-sizes] [start 0])
                              (if (null? sizes)
                                  '()
                                  (let* ([n (car sizes)]
                                         [xs (build-list n (lambda (i) (+ start i)))])
                                    (cons (list->treelist xs)
                                          (loop (cdr sizes) (+ start n))))))]
                           [app (apply treelist-append pieces)]
                           [expected
                            (let loop ([sizes chunk-sizes] [start 0])
                              (if (null? sizes)
                                  '()
                                  (append (build-list (car sizes) (lambda (i) (+ start i)))
                                          (loop (cdr sizes) (+ start (car sizes))))))])
                      (and (equal? (treelist-length app) (length expected))
                           (equal? (treelist-ref app 0) 0)
                           (equal? (treelist-ref app 16) 16)
                           (equal? (treelist-ref app 17) 17)
                           (equal? (treelist-ref app 36) 36)
                           (equal? (treelist-ref app 68) 68)
                           (equal? (treelist-ref app (- (length expected) 1))
                                   (- (length expected) 1))
                           (equal? (treelist->list app) expected)
                           (andmap
                            (lambda (tl start n)
                              (equal? (treelist->list tl)
                                      (build-list n (lambda (i) (+ start i)))))
                            pieces
                            '(0 17 37 68 100 133 150 170 201 233)
                            chunk-sizes))))

              (list "treelist-append balanced redistribution counts"
                    (let* ([counts '(33 65 100)]
                           [checks
                            (for/list ([piece-count (in-list counts)])
                              (let* ([pieces
                                      (for/list ([i (in-range piece-count)])
                                        (treelist i))]
                                     [app (apply treelist-append pieces)]
                                     [expected (build-list piece-count values)])
                                (and (equal? (treelist-length app) piece-count)
                                     (equal? (treelist-ref app 0) 0)
                                     (equal? (treelist-ref app (quotient piece-count 2))
                                             (quotient piece-count 2))
                                     (equal? (treelist-ref app (sub1 piece-count))
                                             (sub1 piece-count))
                                     (equal? (treelist->list app) expected)
                                     (andmap (lambda (i tl)
                                               (equal? (treelist->list tl) (list i)))
                                             expected
                                             pieces))))])
                      (andmap values checks)))

              (list "treelist-append relaxed boundaries"
                    (let* ([tl0 (list->treelist (build-list 1025 values))]
                           [tl1 (treelist 'tail)]
                           [app (treelist-append tl0 tl1)])
                      (and (equal? (treelist-length app) 1026)
                           (equal? (treelist-ref app 1024) 1024)
                           (equal? (treelist-ref app 1025) 'tail)
                           (equal? (treelist->list tl0) (build-list 1025 values))
                           (equal? (treelist->list tl1) '(tail)))))

              (list "treelist-append full-ish boundaries"
                    (let* ([left  (list->treelist (build-list 1024 values))]
                           [right (list->treelist (build-list 1025 (lambda (i) (+ 1024 i))))]
                           [app   (treelist-append left right)]
                           [xs    (append (build-list 1024 values)
                                          (build-list 1025 (lambda (i) (+ 1024 i))))])
                      (and (equal? (treelist-length app) 2049)
                           (equal? (treelist-ref app 0) 0)
                           (equal? (treelist-ref app 1023) 1023)
                           (equal? (treelist-ref app 1024) 1024)
                           (equal? (treelist-ref app 2048) 2048)
                           (equal? (treelist->list app) xs)
                           (equal? (treelist->list left) (build-list 1024 values))
                           (equal? (treelist->list right)
                                   (build-list 1025 (lambda (i) (+ 1024 i)))))))

              (list "treelist-append taller-right boundaries"
                    (let* ([medium (apply treelist-append
                                          (for/list ([j (in-range 100)])
                                            (list->treelist
                                             (build-list 20
                                                         (lambda (i) (+ (* j 20) i))))))]
                           [cases
                            (list (cons (treelist 'x) medium)
                                  (cons (make-treelist 17 'a) medium)
                                  (cons (list->treelist (build-list 32 values)) medium)
                                  (cons (list->treelist (build-list 33 values)) medium)
                                  (cons (treelist 'front)
                                        (list->treelist
                                         (build-list 1025 (lambda (i) (+ 1000 i))))))]
                           [checks
                            (for/list ([entry (in-list cases)])
                              (let* ([left (car entry)]
                                     [right (cdr entry)]
                                     [xs (append (treelist->list left)
                                                 (treelist->list right))]
                                     [app (treelist-append left right)]
                                     [len (length xs)])
                                (and (equal? (treelist-length app) len)
                                     (equal? (treelist-ref app 0) (car xs))
                                     (equal? (treelist-ref app (quotient len 2))
                                             (list-ref xs (quotient len 2)))
                                     (equal? (treelist-ref app (sub1 len))
                                             (list-ref xs (sub1 len)))
                                     (equal? (treelist->list app) xs)
                                     (equal? (treelist->list left) (take xs (treelist-length left)))
                                     (equal? (treelist->list right) (drop xs (treelist-length left))))))])
                      (andmap values checks)))

              (list "treelist-append taller-left boundaries"
                    (let* ([medium (apply treelist-append
                                          (for/list ([j (in-range 100)])
                                            (list->treelist
                                             (build-list 20
                                                         (lambda (i) (+ (* j 20) i))))))]
                           [cases
                            (list (cons medium (treelist 'x))
                                  (cons medium (make-treelist 17 'a))
                                  (cons medium (list->treelist (build-list 32 values)))
                                  (cons medium (list->treelist (build-list 33 values)))
                                  (cons (list->treelist
                                         (build-list 1025 (lambda (i) (+ 1000 i))))
                                        (treelist 'tail)))]
                           [checks
                            (for/list ([entry (in-list cases)])
                              (let* ([left (car entry)]
                                     [right (cdr entry)]
                                     [xs (append (treelist->list left)
                                                 (treelist->list right))]
                                     [app (treelist-append left right)]
                                     [len (length xs)])
                                (and (equal? (treelist-length app) len)
                                     (equal? (treelist-ref app 0) (car xs))
                                     (equal? (treelist-ref app (quotient len 2))
                                             (list-ref xs (quotient len 2)))
                                     (equal? (treelist-ref app (sub1 len))
                                             (list-ref xs (sub1 len)))
                                     (equal? (treelist->list app) xs)
                                     (equal? (treelist->list left)
                                             (take xs (treelist-length left)))
                                     (equal? (treelist->list right)
                                             (drop xs (treelist-length left))))))])
                      (andmap values checks)))

              (list "treelist-set on relaxed tree"
                    (let* ([base (treelist-append (make-treelist 17 'a)
                                                  (make-treelist 17 'b))]
                           [tl   (treelist-set base 17 'x)])
                      (and (equal? (treelist-ref tl 16) 'a)
                           (equal? (treelist-ref tl 17) 'x)
                           (equal? (treelist-ref tl 18) 'b)
                           (equal? (treelist-ref base 17) 'b)
                           (equal? (treelist-length tl) 34)
                           (equal? (treelist-length base) 34))))

              (list "treelist-add on relaxed tree"
                    (let* ([base (treelist-append (make-treelist 17 'a)
                                                  (make-treelist 17 'b))]
                           [tl   (for/fold ([acc base])
                                           ([i (in-range 200)])
                                   (treelist-add acc i))])
                      (and (equal? (treelist-length base) 34)
                           (equal? (treelist-length tl) 234)
                           (equal? (treelist-ref tl 16) 'a)
                           (equal? (treelist-ref tl 17) 'b)
                           (equal? (treelist-ref tl 33) 'b)
                           (equal? (treelist-ref tl 34) 0)
                           (equal? (treelist-ref tl 134) 100)
                           (equal? (treelist-ref tl 233) 199)
                           (equal? (treelist-first tl) 'a)
                           (equal? (treelist-last tl) 199)
                           (equal? (treelist-ref base 17) 'b)
                           (equal? (treelist->list base)
                                   (append (make-list 17 'a) (make-list 17 'b))))))

              (list "treelist-cons boundaries"
                    (let* ([tl31   (list->treelist (build-list 31 values))]
                           [tl32   (list->treelist (build-list 32 values))]
                           [tl1024 (list->treelist (build-list 1024 values))]
                           [c31    (treelist-cons tl31 'head)]
                           [c32    (treelist-cons tl32 'head)]
                           [c1024  (treelist-cons tl1024 'head)])
                      (and (equal? (treelist->list tl31) (build-list 31 values))
                           (equal? (treelist-length c31) 32)
                           (equal? (treelist-ref c31 0) 'head)
                           (equal? (treelist-ref c31 1) 0)
                           (equal? (treelist-length c32) 33)
                           (equal? (treelist-ref c32 0) 'head)
                           (equal? (treelist-ref c32 32) 31)
                           (equal? (treelist-length c1024) 1025)
                           (equal? (treelist-ref c1024 0) 'head)
                           (equal? (treelist-ref c1024 1024) 1023))))

              (list "treelist-cons relaxed tree"
                    (let* ([base (apply treelist-append
                                        (list (list->treelist (build-list 20 values))
                                              (list->treelist (build-list 20 (lambda (i) (+ 20 i))))
                                              (list->treelist (build-list 20 (lambda (i) (+ 40 i))))))]
                           [tl   (treelist-cons base 'front)])
                      (and (equal? (treelist-length tl) 61)
                           (equal? (treelist-ref tl 0) 'front)
                           (equal? (treelist-ref tl 1) 0)
                           (equal? (treelist-ref tl 20) 19)
                           (equal? (treelist-ref tl 21) 20)
                           (equal? (treelist-ref tl 60) 59)
                           (equal? (treelist->list base) (build-list 60 values)))))

              (list "treelist-cons repeated boundaries"
                    (let* ([sizes '(32 33 1024 1025)]
                           [results
                            (for/list ([n (in-list sizes)])
                              (let* ([base (list->treelist (build-list n values))]
                                     [tl   (for/fold ([acc base])
                                                     ([i (in-range 64)])
                                             (treelist-cons acc (- -1 i)))])
                                (and (equal? (treelist->list base) (build-list n values))
                                     (equal? (treelist-length tl) (+ n 64))
                                     (equal? (treelist-ref tl 0) -64)
                                     (equal? (treelist-ref tl 63) -1)
                                     (equal? (treelist-ref tl 64) 0)
                                     (equal? (treelist-ref tl (+ 63 n)) (sub1 n)))) )])
                      (andmap values results)))

              (list "treelist-cons on relaxed tree"
                    (let* ([base (apply treelist-append
                                        (list (list->treelist (build-list 20 values))
                                              (list->treelist (build-list 20 (lambda (i) (+ 20 i))))
                                              (list->treelist (build-list 20 (lambda (i) (+ 40 i))))))]
                           [tl   (for/fold ([acc base])
                                           ([i (in-range 40)])
                                   (treelist-cons acc (- -1 i)))])
                      (and (equal? (treelist-length base) 60)
                           (equal? (treelist-length tl) 100)
                           (equal? (treelist-first tl) -40)
                           (equal? (treelist-ref tl 39) -1)
                           (equal? (treelist-ref tl 40) 0)
                           (equal? (treelist-ref tl 59) 19)
                           (equal? (treelist-ref tl 60) 20)
                           (equal? (treelist-ref tl 79) 39)
                           (equal? (treelist-ref tl 80) 40)
                           (equal? (treelist-last tl) 59)
                           (equal? (treelist->list base) (build-list 60 values))
                           (equal? (treelist->list tl)
                                   (append (build-list 40 (lambda (i) (- i 40)))
                                           (build-list 60 values))))))

              (list "treelist-insert boundaries"
                    (let* ([sizes '(31 32 33 1024 1025)])
                      (andmap
                       (lambda (n)
                         (let* ([xs  (build-list n values)]
                                [tl  (list->treelist xs)]
                                [mid (quotient n 2)]
                                [a   (treelist-insert tl 0 'front)]
                                [b   (treelist-insert tl mid 'middle)]
                                [c   (treelist-insert tl n 'back)])
                           (and (equal? (treelist->list tl) xs)
                                (equal? (treelist-length a) (add1 n))
                                (equal? (treelist-ref a 0) 'front)
                                (equal? (treelist-ref a 1) 0)
                                (equal? (treelist-length b) (add1 n))
                                (equal? (treelist-ref b mid) 'middle)
                                (equal? (treelist-ref b 0) 0)
                                (equal? (treelist-ref b (add1 mid)) mid)
                                (equal? (treelist-length c) (add1 n))
                                (equal? (treelist-ref c n) 'back))))
                       sizes)))

              (list "treelist-insert relaxed tree"
                    (let* ([tl (apply treelist-append
                                      (list (list->treelist (build-list 20 values))
                                            (list->treelist (build-list 20 (lambda (i) (+ 20 i))))
                                            (list->treelist (build-list 20 (lambda (i) (+ 40 i))))))]
                           [xs (build-list 60 values)]
                           [a  (treelist-insert tl 0 'front)]
                           [b  (treelist-insert tl 17 'mid-a)]
                           [c  (treelist-insert tl 20 'boundary)]
                           [d  (treelist-insert tl 33 'mid-b)]
                           [e  (treelist-insert tl 60 'back)])
                      (and (equal? (treelist->list tl) xs)
                           (equal? (treelist->list a) (cons 'front xs))
                           (equal? (treelist->list b) (append (take xs 17) '(mid-a) (drop xs 17)))
                           (equal? (treelist->list c) (append (take xs 20) '(boundary) (drop xs 20)))
                           (equal? (treelist->list d) (append (take xs 33) '(mid-b) (drop xs 33)))
                           (equal? (treelist->list e) (append xs '(back))))))

              (list "treelist-delete boundaries"
                    (let* ([sizes '(1 31 32 33 1024 1025)])
                      (andmap
                       (lambda (n)
                         (let* ([xs  (build-list n values)]
                                [tl  (list->treelist xs)]
                                [mid (quotient n 2)]
                                [last (sub1 n)]
                                [a   (treelist-delete tl 0)]
                                [b   (treelist-delete tl mid)]
                                [c   (treelist-delete tl last)])
                           (and (equal? (treelist->list tl) xs)
                                (equal? (treelist->list a) (drop xs 1))
                                (equal? (treelist->list b)
                                        (append (take xs mid) (drop xs (add1 mid))))
                                (equal? (treelist->list c) (take xs last))
                                (equal? (treelist-length a) (sub1 n))
                                (equal? (treelist-length b) (sub1 n))
                                (equal? (treelist-length c) (sub1 n))
                                (or (zero? (sub1 n))
                                    (equal? (treelist-ref c 0) 0)))))
                       sizes)))

              (list "treelist-delete relaxed tree"
                    (let* ([tl (apply treelist-append
                                      (list (list->treelist (build-list 20 values))
                                            (list->treelist (build-list 20 (lambda (i) (+ 20 i))))
                                            (list->treelist (build-list 20 (lambda (i) (+ 40 i))))))]
                           [xs (build-list 60 values)]
                           [a  (treelist-delete tl 0)]
                           [b  (treelist-delete tl 17)]
                           [c  (treelist-delete tl 20)]
                           [d  (treelist-delete tl 33)]
                           [e  (treelist-delete tl 59)])
                      (and (equal? (treelist->list tl) xs)
                           (equal? (treelist->list a) (drop xs 1))
                           (equal? (treelist->list b) (append (take xs 17) (drop xs 18)))
                           (equal? (treelist->list c) (append (take xs 20) (drop xs 21)))
                           (equal? (treelist->list d) (append (take xs 33) (drop xs 34)))
                           (equal? (treelist->list e) (take xs 59)))))

              (list "treelist-take-right/drop-right"
                    (let ([sizes '(0 1 31 32 33 1024 1025)])
                      (andmap
                       (lambda (n)
                         (let* ([xs  (build-list n values)]
                                [tl  (list->treelist xs)]
                                [mid (quotient n 2)])
                           (and (equal? (treelist->list (treelist-take-right tl 0)) '())
                                (equal? (treelist->list (treelist-take-right tl mid))
                                        (drop xs (- n mid)))
                                (equal? (treelist->list (treelist-take-right tl n)) xs)
                                (equal? (treelist->list (treelist-drop-right tl 0)) xs)
                                (equal? (treelist->list (treelist-drop-right tl mid))
                                        (take xs (- n mid)))
                                (equal? (treelist->list (treelist-drop-right tl n)) '())
                                (equal? (treelist->list tl) xs))))
                       sizes)))

              (list "treelist-sublist dense sizes"
                    (let ([sizes '(0 1 31 32 33 1024 1025)])
                      (andmap
                       (lambda (n)
                         (let* ([xs  (build-list n values)]
                                [tl  (list->treelist xs)]
                                [mid (quotient n 2)])
                           (and (equal? (treelist->list (treelist-sublist tl 0 0)) '())
                                (equal? (treelist->list (treelist-sublist tl 0 mid))
                                        (take xs mid))
                                (equal? (treelist->list (treelist-sublist tl mid mid)) '())
                                (equal? (treelist->list (treelist-sublist tl mid n))
                                        (drop xs mid))
                                (equal? (treelist->list (treelist-sublist tl 0 n)) xs)
                                (equal? (treelist->list tl) xs))))
                       sizes)))

              (list "treelist-reverse dense sizes"
                    (let ([sizes '(0 1 31 32 33 1024 1025)])
                      (andmap
                       (lambda (n)
                         (let* ([xs  (build-list n values)]
                                [tl  (list->treelist xs)]
                                [rev (treelist-reverse tl)])
                           (and (equal? (treelist->list rev) (reverse xs))
                                (equal? (treelist->list tl) xs)
                                (equal? (treelist->list (treelist-reverse rev)) xs)
                                (or (zero? n)
                                    (and (equal? (treelist-ref rev 0) (sub1 n))
                                         (equal? (treelist-ref rev (sub1 n)) 0))))))
                       sizes)))

              (list "treelist-reverse relaxed tree"
                    (let* ([tl (apply treelist-append
                                      (list (list->treelist (build-list 20 values))
                                            (list->treelist (build-list 20 (lambda (i) (+ 20 i))))
                                            (list->treelist (build-list 20 (lambda (i) (+ 40 i))))))]
                           [xs (build-list 60 values)]
                           [rev (treelist-reverse tl)])
                      (and (equal? (treelist->list tl) xs)
                           (equal? (treelist->list rev) (reverse xs))
                           (equal? (treelist-length rev) 60)
                           (equal? (treelist-ref rev 0) 59)
                           (equal? (treelist-ref rev 19) 40)
                           (equal? (treelist-ref rev 20) 39)
                           (equal? (treelist-ref rev 39) 20)
                           (equal? (treelist-ref rev 40) 19)
                           (equal? (treelist-ref rev 59) 0)
                           (equal? (treelist->list (treelist-reverse rev)) xs))))

              (list "treelist-reverse after repeated cons"
                    (let* ([base (apply treelist-append
                                        (list (list->treelist (build-list 20 values))
                                              (list->treelist (build-list 20 (lambda (i) (+ 20 i))))
                                              (list->treelist (build-list 20 (lambda (i) (+ 40 i))))))]
                           [tl   (for/fold ([acc base])
                                           ([i (in-range 40)])
                                   (treelist-cons acc (- -1 i)))]
                           [xs   (append (build-list 40 (lambda (i) (- i 40)))
                                         (build-list 60 values))]
                           [rev  (treelist-reverse tl)])
                      (and (equal? (treelist->list tl) xs)
                           (equal? (treelist->list rev) (reverse xs))
                           (equal? (treelist-length rev) 100)
                           (equal? (treelist-ref rev 0) 59)
                           (equal? (treelist-ref rev 19) 40)
                           (equal? (treelist-ref rev 20) 39)
                           (equal? (treelist-ref rev 59) 0)
                           (equal? (treelist-ref rev 60) -1)
                           (equal? (treelist-ref rev 99) -40)
                           (equal? (treelist->list base) (build-list 60 values))
                           (equal? (treelist->list (treelist-reverse rev)) xs))))

              (list "treelist-rest persistence"
                    (let* ([tl0 (treelist 9 8 7 6)]
                           [tl1 (treelist-rest tl0)])
                      (and (equal? (treelist->list tl0) '(9 8 7 6))
                           (equal? (treelist->list tl1) '(8 7 6))
                           (equal? (treelist-length tl1) 3)
                           (equal? (treelist-ref tl1 0) 8))))

              (list "treelist-append* basics"
                    (and (equal? (treelist->list (treelist-append* (treelist))) '())
                         (equal? (treelist->list (treelist-append*
                                                  (treelist (treelist 1 2))))
                                 '(1 2))
                         (equal? (treelist->list
                                  (treelist-append*
                                   (treelist (treelist 1)
                                             (treelist)
                                             (treelist 2 3)
                                             (list->treelist (build-list 32 values)))))
                                 (append '(1 2 3) (build-list 32 values)))))

              (list "treelist-append* relaxed outer"
                    (let* ([a (treelist 1 2)]
                           [b (treelist)]
                           [c (list->treelist (build-list 20 values))]
                           [d (treelist 'tail)]
                           [outer (treelist-append
                                   (treelist a b)
                                   (treelist c d))]
                           [flat (treelist-append* outer)])
                      (and (equal? (treelist->list flat)
                                   (append '(1 2) (build-list 20 values) '(tail)))
                           (equal? (treelist->list outer) (list a b c d))
                           (equal? (treelist->list a) '(1 2))
                           (equal? (treelist->list b) '())
                           (equal? (treelist->list c) (build-list 20 values))
                           (equal? (treelist->list d) '(tail)))))

              (list "treelist-append* mixed element error"
                    (with-handlers ([exn:fail?
                                     (lambda (e)
                                       (and (string-contains? (exn-message e) "treelist-append*")
                                            #t))])
                      (treelist-append*
                       (treelist-append
                        (treelist (treelist 1) (treelist 2))
                        (treelist 0 (treelist 3))))
                      #f))

              (list "treelist-map dense sizes"
                    (let ([sizes '(0 1 31 32 33 1024 1025)])
                      (andmap
                       (lambda (n)
                         (let* ([xs (build-list n values)]
                                [tl (list->treelist xs)]
                                [tl2 (treelist-map tl add1)])
                           (and (equal? (treelist-length tl2) n)
                                (equal? (treelist->list tl2) (map add1 xs))
                                (equal? (treelist->list tl) xs))))
                       sizes)))

              (list "treelist-for-each order"
                    (let ([seen '()])
                      (let ([tl (treelist 1 2 3 4)])
                        (and (void? (treelist-for-each tl (lambda (x) (set! seen (cons x seen)))))
                             (equal? (reverse seen) '(1 2 3 4))
                             (equal? (treelist->list tl) '(1 2 3 4))))))

              (list "treelist higher-order relaxed traversal"
                    (let* ([left  (list->treelist (build-list 20 values))]
                           [right (list->treelist (build-list 20 (lambda (i) (+ i 20))))]
                           [tl    (treelist-append left right)]
                           [seen  '()]
                           [mapped   (treelist-map tl add1)]
                           [filtered (treelist-filter even? tl)])
                      (and (void? (treelist-for-each tl (lambda (x) (set! seen (cons x seen)))))
                           (equal? (reverse seen) (build-list 40 values))
                           (equal? (treelist->list mapped) (build-list 40 add1))
                           (equal? (treelist->list filtered)
                                   (filter even? (build-list 40 values)))
                           (equal? (treelist->list tl) (build-list 40 values)))))

              (list "treelist-filter basics"
                    (let* ([tl (list->treelist (build-list 33 values))]
                           [evens (treelist-filter even? tl)]
                           [odds  (treelist-filter odd? tl)])
                      (and (equal? (treelist->list evens)
                                   (filter even? (build-list 33 values)))
                           (equal? (treelist->list odds)
                                   (filter odd? (build-list 33 values)))
                           (equal? (treelist->list tl) (build-list 33 values)))))

              (list "treelist-filter truthy predicates"
                    (let* ([tl (treelist-append
                                (treelist #f 0 '() 'keep)
                                (list->treelist (list (cons 'a 'b) (vector 1 2) 'drop)))]
                           [keep-0   (treelist-filter (lambda (x) (and (not (eq? x 'drop)) 0)) tl)]
                           [keep-null (treelist-filter (lambda (x) (and (not (eq? x 'drop)) '())) tl)]
                           [keep-sym (treelist-filter (lambda (x) (and (not (eq? x 'drop)) 'yes)) tl)]
                           [expected (list #f 0 '() 'keep (cons 'a 'b) (vector 1 2))])
                      (and (equal? (treelist->list keep-0) expected)
                           (equal? (treelist->list keep-null) expected)
                           (equal? (treelist->list keep-sym) expected)
                           (equal? (treelist->list tl)
                                   (list #f 0 '() 'keep (cons 'a 'b) (vector 1 2) 'drop)))))

              (list "treelist-find basics"
                    (let ([tl (treelist 7 8 9 10 11)])
                      (and (equal? (treelist-find tl (lambda (x) (> x 9))) 10)
                           (equal? (treelist-find tl (lambda (x) (< x 0))) #f)
                           (equal? (treelist-find (treelist) odd?) #f)
                           (let* ([tl2 (list->treelist (append (list #f) (build-list 40 values)))]
                                  [seen '()]
                                  [found (treelist-find tl2
                                                        (lambda (x)
                                                          (set! seen (cons x seen))
                                                          #t))])
                             (and (equal? found #f)
                                  (equal? (reverse seen) '(#f)))))))

              (list "treelist-find truthy values"
                    (let* ([pairv (cons 'a 'b)]
                           [vecv  (vector 1 2)]
                           [tlv   (treelist 9)]
                           [tl    (treelist-append
                                   (treelist #f 0 '() 'sym)
                                   (list->treelist (list pairv vecv tlv)))])
                      (and (equal? (treelist-find tl (lambda (x) (equal? x #f))) #f)
                           (equal? (treelist-find tl (lambda (x) (equal? x 0))) 0)
                           (equal? (treelist-find tl null?) '())
                           (equal? (treelist-find tl symbol?) 'sym)
                           (equal? (treelist-find tl pair?) pairv)
                           (equal? (treelist-find tl vector?) vecv)
                           (equal? (treelist->list (treelist-find tl treelist?)) '(9))
                           (equal? (treelist->list tl)
                                   (list #f 0 '() 'sym pairv vecv tlv)))))

              (list "treelist-find/member/index early exit"
                    (let ([seen-find   '()]
                          [seen-member '()]
                          [seen-index  '()]
                          [tl (treelist-append
                               (list->treelist (build-list 20 values))
                               (list->treelist (build-list 20 (lambda (i) (+ i 20)))))])
                      (and (equal? (treelist-find tl
                                                  (lambda (x)
                                                    (set! seen-find (cons x seen-find))
                                                    (= x 5)))
                                   5)
                           (equal? (reverse seen-find) '(0 1 2 3 4 5))
                           (equal? (treelist-member? tl 7
                                                     (lambda (a b)
                                                       (set! seen-member (cons b seen-member))
                                                       (= a b)))
                                   #t)
                           (equal? (reverse seen-member) '(0 1 2 3 4 5 6 7))
                           (equal? (treelist-index-of tl 9
                                                      (lambda (a b)
                                                        (set! seen-index (cons b seen-index))
                                                        (= a b)))
                                   9)
                           (equal? (reverse seen-index) '(0 1 2 3 4 5 6 7 8 9))
                           (equal? (treelist->list tl) (build-list 40 values)))))

              (list "treelist-member/index truthy comparators"
                    (let* ([tl (treelist-append
                                (list->treelist (build-list 20 values))
                                (list->treelist (build-list 20 (lambda (i) (+ i 20)))))]
                           [seen-null '()]
                           [seen-zero '()]
                           [seen-sym '()])
                      (and (equal? (treelist-member? tl 6
                                                     (lambda (a b)
                                                       (set! seen-null (cons b seen-null))
                                                       (and (= a b) '())))
                                   #t)
                           (equal? (reverse seen-null) '(0 1 2 3 4 5 6))
                           (equal? (treelist-index-of tl 8
                                                      (lambda (a b)
                                                        (set! seen-zero (cons b seen-zero))
                                                        (and (= a b) 0)))
                                   8)
                           (equal? (reverse seen-zero) '(0 1 2 3 4 5 6 7 8))
                           (equal? (treelist-member? tl 10
                                                     (lambda (a b)
                                                       (set! seen-sym (cons b seen-sym))
                                                       (and (= a b) 'match)))
                                   #t)
                           (equal? (reverse seen-sym) '(0 1 2 3 4 5 6 7 8 9 10)))))

              (list "treelist higher-order empty no-call"
                    (let ([calls '()])
                      (and (equal? (treelist->list
                                    (treelist-map empty-treelist
                                                  (lambda (x)
                                                    (set! calls (cons (list 'map x) calls))
                                                    x)))
                                   '())
                           (void? (treelist-for-each empty-treelist
                                                     (lambda (x)
                                                       (set! calls (cons (list 'for-each x) calls)))))
                           (equal? (treelist->list
                                    (treelist-filter (lambda (x)
                                                       (set! calls (cons (list 'filter x) calls))
                                                       #t)
                                                     empty-treelist))
                                   '())
                           (equal? (treelist-find empty-treelist
                                                  (lambda (x)
                                                    (set! calls (cons (list 'find x) calls))
                                                    #t))
                                   #f)
                           (equal? (treelist-member? empty-treelist
                                                     'x
                                                     (lambda (a b)
                                                       (set! calls (cons (list 'member a b) calls))
                                                       #t))
                                   #f)
                           (equal? (treelist-index-of empty-treelist
                                                      'x
                                                      (lambda (a b)
                                                        (set! calls (cons (list 'index a b) calls))
                                                        #t))
                                   #f)
                           (equal? calls '()))))

              (list "treelist-member?/index-of"
                    (let* ([s1 (string-append "ab" "c")]
                           [s2 (string-append "a" "bc")]
                           [tl (treelist 1 2 3 s1)])
                      (and (equal? (treelist-member? tl 2) #t)
                           (equal? (treelist-member? tl 99) #f)
                           (equal? (treelist-member? tl s2) #t)
                           (equal? (treelist-member? tl s2 eq?) #f)
                           (equal? (treelist-index-of tl 3) 2)
                           (equal? (treelist-index-of tl 99) #f)
                           (equal? (treelist-index-of tl s2) 3)
                           (equal? (treelist-index-of tl s2 eq?) #f)
                           (equal? (treelist-index-of (treelist) 1) #f))))

              (list "treelist-flatten basics"
                    (let* ([nested (treelist (treelist 1 2)
                                             3
                                             (treelist)
                                             (treelist (treelist 4) 5))]
                           [flat   (treelist-flatten nested)])
                      (and (equal? (treelist->list flat) '(1 2 3 4 5))
                           (equal? (treelist->list (treelist-flatten empty-treelist)) '())
                           (equal? (treelist->list (treelist-flatten 42)) '(42))
                           (equal? (treelist-length nested) 4)
                           (equal? (treelist-ref nested 1) 3))))

              (list "treelist-flatten relaxed nested"
                    (let* ([pairv   (cons 'a 'b)]
                           [vecv    (vector 1 2)]
                           [nested1 (treelist #f 0 '())]
                           [nested2 (treelist 'sym pairv vecv)]
                           [nested3 (treelist)]
                           [nested4 (treelist (treelist 'tail) (treelist))]
                           [outer   (treelist-append
                                     (treelist nested1 nested2)
                                     (treelist nested3 nested4))]
                           [flat    (treelist-flatten outer)])
                      (and (equal? (treelist->list flat)
                                   (list #f 0 '() 'sym pairv vecv 'tail))
                           (equal? (treelist->list (treelist-flatten pairv)) (list pairv))
                           (equal? (treelist-length outer) 4)
                           (equal? (treelist-ref outer 2) nested3))))

              (list "treelist-sort basics"
                    (let* ([nums     (treelist 5 1 4 2 3)]
                           [nums-s   (treelist-sort nums <)]
                           [strings  (treelist "c" "a" "b")]
                           [strings-s (treelist-sort strings string<?)])
                      (and (equal? (treelist->list nums-s) '(1 2 3 4 5))
                           (equal? (treelist->list strings-s) '("a" "b" "c"))
                           (equal? (treelist->list nums) '(5 1 4 2 3))
                           (equal? (treelist->list strings) '("c" "a" "b")))))

              (list "treelist-sort dense sizes"
                    (let ([sizes '(0 1 32 33 1025)])
                      (andmap
                       (lambda (n)
                         (let* ([xs (build-list n (lambda (i) (- (sub1 n) i)))]
                                [tl (list->treelist xs)]
                                [sorted (treelist-sort tl <)])
                           (and (equal? (treelist->list sorted) (build-list n values))
                                (equal? (treelist->list tl) xs))))
                       sizes)))

              (list "treelist front insert/delete regression"
                    (let* ([tl (for/fold ([tl (treelist)])
                                         ([_ (in-range 257)])
                                 (treelist-insert tl 0 0))]
                           [tl2 (for/fold ([tl tl])
                                          ([_ (in-range 193)])
                                  (treelist-delete tl 0))]
                           [tl3 (treelist-insert tl2 0 0)])
                      (and (equal? (treelist-length tl) 257)
                           (equal? (treelist-length tl2) 64)
                           (equal? (treelist-length tl3) 65)
                           (equal? (treelist-ref tl3 0) 0)
                           (equal? (treelist->list tl2) (make-list 64 0))
                           (equal? (treelist->list tl3) (make-list 65 0)))))

              (list "treelist front insert/delete moderate"
                    (let* ([tl (for/fold ([tl (treelist)])
                                         ([i (in-range 128)])
                                 (treelist-insert tl 0 i))]
                           [tl2 (for/fold ([tl tl])
                                          ([_ (in-range 96)])
                                  (treelist-delete tl 0))])
                      (and (equal? (treelist-length tl) 128)
                           (equal? (treelist-length tl2) 32)
                           (equal? (treelist-ref tl 0) 127)
                           (equal? (treelist-ref tl 127) 0)
                           (equal? (treelist-ref tl2 0) 31)
                           (equal? (treelist-ref tl2 31) 0))))

              (list "treelist-cons front regression moderate"
                    (let* ([base (apply treelist-append
                                        (list (list->treelist (build-list 20 values))
                                              (list->treelist (build-list 20 (lambda (i) (+ 20 i))))
                                              (list->treelist (build-list 20 (lambda (i) (+ 40 i))))))]
                           [tl   (for/fold ([acc base])
                                           ([i (in-range 128)])
                                   (treelist-cons acc i))])
                      (and (equal? (treelist-length tl) 188)
                           (equal? (treelist-ref tl 0) 127)
                           (equal? (treelist-ref tl 127) 0)
                           (equal? (treelist-ref tl 128) 0)
                           (equal? (treelist-ref tl 147) 19)
                           (equal? (treelist-ref tl 148) 20)
                           (equal? (treelist-ref tl 187) 59)
                           (equal? (treelist->list base) (build-list 60 values)))))

              (list "treelist-add repeated boundaries"
                    (let* ([sizes '(32 33 1024 1025 4096)]
                           [results
                            (for/list ([n (in-list sizes)])
                              (for/fold ([tl (treelist)])
                                        ([i (in-range n)])
                                (treelist-add tl i)))])
                      (andmap
                       (lambda (n tl)
                         (and (equal? (treelist-length tl) n)
                              (equal? (treelist-ref tl 0) 0)
                              (equal? (treelist-ref tl (quotient n 2)) (quotient n 2))
                              (equal? (treelist-ref tl (sub1 n)) (sub1 n))
                              (equal? (treelist->list tl) (build-list n values))))
                       sizes
                       results)))

              (list "treelist-set repeated preserves original"
                    (let* ([n 1025]
                           [base-xs (build-list n values)]
                           [base (list->treelist base-xs)]
                           [updates '((0 . 7000) (32 . 7032) (512 . 7512) (1024 . 8024))]
                           [updated
                            (for/fold ([tl base])
                                      ([entry (in-list updates)])
                              (treelist-set tl (car entry) (cdr entry)))])
                      (and (equal? (treelist->list base) base-xs)
                           (equal? (treelist-length updated) n)
                           (equal? (treelist-ref updated 0) 7000)
                           (equal? (treelist-ref updated 32) 7032)
                           (equal? (treelist-ref updated 512) 7512)
                           (equal? (treelist-ref updated 1024) 8024)
                           (equal? (treelist-ref base 0) 0)
                           (equal? (treelist-ref base 32) 32)
                           (equal? (treelist-ref base 512) 512)
                           (equal? (treelist-ref base 1024) 1024))))

              (list "treelist append/take/drop boundaries"
                    (let* ([left-xs  (build-list 32 values)]
                           [right-xs (build-list 33 (lambda (i) (+ 32 i)))]
                           [left     (list->treelist left-xs)]
                           [right    (list->treelist right-xs)]
                           [both     (treelist-append left right)]
                           [mid      (treelist-take both 33)]
                           [tail     (treelist-drop both 31)])
                      (and (equal? (treelist-length both) 65)
                           (equal? (treelist->list both) (append left-xs right-xs))
                           (equal? (treelist->list mid) (build-list 33 values))
                           (equal? (treelist->list tail) (build-list 34 (lambda (i) (+ 31 i))))
                           (equal? (treelist->list left) left-xs)
                           (equal? (treelist->list right) right-xs))))

              (list "treelist-append after split/take/drop"
                    (let* ([base (apply treelist-append
                                        (for/list ([j (in-range 100)])
                                          (list->treelist
                                           (build-list 20
                                                       (lambda (i) (+ (* j 20) i))))))]
                           [base-xs (build-list 2000 values)]
                           [parts   (call-with-values
                                        (lambda () (treelist-split-at base 777))
                                      list)]
                           [left    (first parts)]
                           [right   (second parts)]
                           [takep   (treelist-take base 1200)]
                           [dropp   (treelist-drop base 1200)]
                           [app1    (treelist-append left right)]
                           [app2    (treelist-append takep dropp)])
                      (and (equal? (treelist->list app1) base-xs)
                           (equal? (treelist->list app2) base-xs)
                           (equal? (treelist-ref app1 776) 776)
                           (equal? (treelist-ref app1 777) 777)
                           (equal? (treelist-ref app2 1199) 1199)
                           (equal? (treelist-ref app2 1200) 1200)
                           (equal? (treelist->list left) (take base-xs 777))
                           (equal? (treelist->list right) (drop base-xs 777))
                           (equal? (treelist->list takep) (take base-xs 1200))
                           (equal? (treelist->list dropp) (drop base-xs 1200))
                           (equal? (treelist->list base) base-xs))))

              (list "treelist error paths"
                    (let ([raises? (lambda (thunk)
                                     (with-handlers ([exn:fail? (lambda (_ex) #t)])
                                       (thunk)
                                       #f))]
                          [raises-who?
                           (lambda (thunk who)
                             (with-handlers ([exn:fail?
                                              (lambda (ex)
                                                (string-contains? (exn-message ex) who))])
                               (thunk)
                               #f))])
                      (and (raises? (lambda () (treelist-length 0)))
                           (raises? (lambda () (treelist-ref (treelist 1 2) 2)))
                           (raises? (lambda () (treelist-first empty-treelist)))
                           (raises? (lambda () (treelist-last empty-treelist)))
                           (raises? (lambda () (treelist-rest empty-treelist)))
                           (raises? (lambda () (treelist-split-at (treelist 1 2) 3)))
                           (raises? (lambda () (treelist-take (treelist 1 2) 3)))
                           (raises? (lambda () (treelist-drop (treelist 1 2) 3)))
                           (raises? (lambda () (treelist-insert (treelist 1 2) 3 'x)))
                           (raises? (lambda () (treelist-delete (treelist 1 2) 2)))
                           (raises? (lambda () (make-treelist -1)))
                           (raises-who? (lambda () (treelist-length 0)) "treelist-length")
                           (raises-who? (lambda () (treelist-ref 0 0)) "treelist-ref")
                           (raises-who? (lambda () (treelist-append 0)) "treelist-append")
                           (raises-who? (lambda () (treelist-append* (treelist 0))) "treelist-append*")
                           (raises-who? (lambda () (treelist-map 0 add1)) "treelist-map")
                           (raises-who? (lambda () (treelist->list 0)) "treelist->list"))))

              ;; Note: raw treelist formatting is covered by dedicated smoke runs.
              ;; `display`/`write` are unavailable in this `--no-stdlib` test file,
              ;; and printed treelists are not readable s-expressions for pretty.rkt.
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

              (list "hash-placeholder constructors"
                    (let ([h   (make-hash-placeholder '((a . 1)))]
                          [heq (make-hasheq-placeholder '((a . 1)))]
                          [hev (make-hasheqv-placeholder '((a . 1)))]
                          [hal (make-hashalw-placeholder '((a . 1)))])
                      (and (equal? (hash-placeholder? h) #t)
                           (equal? (hash-placeholder? heq) #t)
                           (equal? (hash-placeholder? hev) #t)
                           (equal? (hash-placeholder? hal) #t)
                           (equal? (hash-placeholder? (hash 'a 1)) #f)
                           (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                             (make-hash-placeholder '(a))
                             #f))))

              (list "reader placeholders"
                    (let ([ph (make-placeholder 'start)])
                      (and (placeholder? ph)
                           (not (placeholder? 'start))
                           (equal? (placeholder-get ph) 'start)
                           (void? (placeholder-set! ph 'done))
                           (equal? (placeholder-get ph) 'done)
                           (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                             (placeholder-get 'not-a-placeholder)
                             #f)
                           (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                             (placeholder-set! 'not-a-placeholder 'x)
                             #f))))

              (list "make-reader-graph acyclic placeholders"
                    (let ([ph (make-placeholder #f)]
                          [vph (make-placeholder #f)]
                          [bph (make-placeholder #f)])
                      (placeholder-set! ph (cons 40 2))
                      (placeholder-set! vph (vector ph 1))
                      (placeholder-set! bph (box vph))
                      (let ([resolved (make-reader-graph (list ph vph bph))])
                        (and (equal? (car resolved) '(40 . 2))
                             (equal? (vector-ref (cadr resolved) 0) '(40 . 2))
                             (equal? (vector-ref (cadr resolved) 1) 1)
                             (equal? (vector-ref (unbox (caddr resolved)) 0)
                                     '(40 . 2)))))))

              (list "hash-weak?/strong-fallbacks"
                    (and (equal? (hash-weak? (make-hash)) #f)
                         (equal? (hash-weak? (make-weak-hash)) #f)
                         (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                           (hash-weak? 'not-a-hash)
                           #f)))

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
              (list "immutable hasheqv"
                    (let* ([empty (hasheqv)]
                           [h     (hasheqv 'a 1 'b 2 'a 3)]
                           [h2    (hash-set h 'c 4)])
                      (and (hash? empty)
                           (immutable-hash? empty)
                           (not (mutable-hash? empty))
                           (hash-eqv? empty)
                           (equal? (hash-count empty) 0)
                           (equal? (hash-ref empty 'missing #f) #f)
                           (equal? (hash-count h) 2)
                           (equal? (hash-ref h 'a) 3)
                           (equal? (hash-ref h 'b) 2)
                           (equal? (hash-has-key? h 'a) #t)
                           (equal? (hash-has-key? h 'c) #f)
                           (equal? (hash-count h2) 3)
                           (equal? (hash-ref h2 'c) 4)
                           (equal? (hash-has-key? h 'c) #f))))
              (list "immutable hasheqv many entries"
                    (let loop ([i 0] [h (hasheqv)])
                      (if (equal? i 80)
                          (let check ([j 0] [h* (hash-remove h 17)])
                            (cond
                              [(equal? j 80)
                               (and (equal? (hash-count h) 80)
                                    (equal? (hash-count h*) 79)
                                    (not (hash-has-key? h* 17)))]
                              [(equal? j 17)
                               (and (equal? (hash-ref h j) (* j 10))
                                    (check (+ j 1) h*))]
                              [else
                               (and (equal? (hash-ref h j) (* j 10))
                                    (equal? (hash-ref h* j) (* j 10))
                                    (check (+ j 1) h*))]))
                          (loop (+ i 1) (hash-set h i (* i 10))))))
              (list "immutable hasheqv path-copy remove"
                    (let loop ([i 0] [h (hasheqv)])
                      (if (equal? i 80)
                          (let ([h* (hash-remove h 27)])
                            (and (equal? (hash-count h) 80)
                                 (equal? (hash-count h*) 79)
                                 (not (hash-has-key? h* 27))
                                 (equal? (hash-ref h 27 #f) 27)
                                 (eq? (hash-remove h* 27) h*)
                                 (andmap (lambda (j)
                                           (or (equal? j 27)
                                               (equal? (hash-ref h* j #f) j)))
                                         (let make ([j 0] [acc '()])
                                           (if (equal? j 80)
                                               acc
                                               (make (+ j 1) (cons j acc)))))))
                          (loop (+ i 1) (hash-set h i i)))))
              (list "immutable HAMT recursive bucket split"
                    (let ([keys '(0 8 16 24 32 40 48 56 64)])
                      (let loop ([ks keys] [h (hasheqv)])
                        (if (null? ks)
                            (let* ([h* (hash-remove h 24)]
                                   [h** (hash-set h* 72 72)])
                              (and (equal? (hash-count h) 9)
                                   (equal? (hash-count h*) 8)
                                   (equal? (hash-count h**) 9)
                                   (not (hash-has-key? h* 24))
                                   (equal? (hash-ref h** 72 #f) 72)
                                   (andmap (lambda (k)
                                             (or (equal? k 24)
                                                 (equal? (hash-ref h* k #f) k)))
                                           keys)))
                            (loop (cdr ks)
                                  (hash-set h (car ks) (car ks)))))))
              (list "immutable HAMT update replaces iteration entry"
                    (let* ([h  (hasheqv 0 'old 8 'eight 0 'new)]
                           [xs (hash->list h)])
                      (and (equal? (hash-count h) 2)
                           (equal? (hash-ref h 0) 'new)
                           (equal? (length xs) 2)
                           (equal? (length (filter (lambda (p)
                                                     (equal? (car p) 0))
                                                   xs))
                                   1))))
              (list "immutable HAMT removes emptied child branch"
                    (let ([keys '(0 8 16 24 32 40 48 56 64)])
                      (let build ([ks keys] [h (hasheqv)])
                        (if (null? ks)
                            (let remove ([ks keys] [h h])
                              (if (null? ks)
                                  (let ([h* (hash-set h 16 'again)])
                                    (and (equal? (hash-count h) 0)
                                         (null? (hash->list h))
                                         (equal? (hash-count h*) 1)
                                         (equal? (hash-ref h* 16) 'again)))
                                  (remove (cdr ks)
                                          (hash-remove h (car ks)))))
                            (build (cdr ks)
                                   (hash-set h (car ks) (car ks)))))))
              (list "hash immutable constructor"
                    (let* ([empty (hash)]
                           [h     (hash 'a 1 'b 2 'a 3)]
                           [h2    (hash-set h 'c 4)])
                      (and (hash? empty)
                           (immutable-hash? empty)
                           (hash-equal? empty)
                           (not (hash-eq? empty))
                           (not (hash-eqv? empty))
                           (equal? (hash-count empty) 0)
                           (equal? (hash-count h) 2)
                           (equal? (hash-ref h 'a) 3)
                           (equal? (hash-ref h 'b) 2)
                           (equal? (hash-ref h 'c #f) #f)
                           (equal? (hash-count h2) 3)
                           (equal? (hash-ref h2 'c) 4))))
              (list "hasheq immutable constructor"
                    (let* ([empty (hasheq)]
                           [h     (hasheq 'a 1 'b 2 'a 3)]
                           [h2    (hash-set h 'c 4)])
                      (and (hash? empty)
                           (immutable-hash? empty)
                           (hash-eq? empty)
                           (not (hash-eqv? empty))
                           (equal? (hash-count empty) 0)
                           (equal? (hash-ref h 'a) 3)
                           (equal? (hash-ref h 'b) 2)
                           (equal? (hash-ref h 'c #f) #f)
                           (equal? (hash-count h2) 3)
                           (equal? (hash-ref h2 'c) 4))))
              (list "immutable hasheq keeps eq? heap keys distinct"
                    (let* ([a (box 'same)]
                           [b (box 'same)]
                           [h (hasheq a 'a)])
                      (or (eq? a b)
                          (and (equal? a b)
                               (equal? (hash-ref h a #f) 'a)
                               (equal? (hash-ref h b #f) #f)
                               (equal? (hash-has-key? h b) #f)
                               (eq? (hash-ref-key h a) a)
                               (eq? (hash-remove h b) h)))))
              (list "immutable hasheq path-copy remove"
                    (let loop ([i 0] [keys '()] [h (hasheq)])
                      (if (equal? i 70)
                          (let* ([target (car (cdr keys))]
                                 [h*     (hash-remove h target)])
                            (and (equal? (hash-count h) 70)
                                 (equal? (hash-count h*) 69)
                                 (not (hash-has-key? h* target))
                                 (eq? (hash-remove h* target) h*)
                                 (andmap (lambda (k)
                                           (or (eq? k target)
                                               (equal? (hash-ref h* k #f)
                                                       (hash-ref h k))))
                                         keys)))
                          (let ([k (box i)])
                            (loop (+ i 1)
                                  (cons k keys)
                                  (hash-set h k i))))))
              (list "quoted hash literals are immutable"
                    (let* ([h-eq    '#hasheq((a . 1))]
                           [h-eqv   '#hasheqv((1 . one))]
                           [h-equal '#hash((a . 1))]
                           [h-alw   '#hashalw((a . 1))]
                           [h2      (hash-set '#hasheq() 'scope #t)])
                      (and (immutable-hash? h-eq)
                           (immutable-hash? h-eqv)
                           (immutable-hash? h-equal)
                           (immutable-hash? h-alw)
                           (hash-eq? h-eq)
                           (hash-eqv? h-eqv)
                           (hash-equal? h-equal)
                           (hash-equal-always? h-alw)
                           (equal? (hash-ref h2 'scope) #t))))
              (list "hashalw immutable constructor"
                    (let* ([empty (hashalw)]
                           [h     (hashalw 'a 1 'b 2 'a 3)]
                           [h2    (hash-set h 'c 4)])
                      (and (hash? empty)
                           (immutable-hash? empty)
                           (hash-equal-always? empty)
                           (not (hash-equal? empty))
                           (equal? (hash-count empty) 0)
                           (equal? (hash-ref h 'a) 3)
                           (equal? (hash-ref h 'b) 2)
                           (equal? (hash-ref h 'c #f) #f)
                           (equal? (hash-count h2) 3)
                           (equal? (hash-ref h2 'c) 4)
                           (hash-has-key? h2 'c))))
              (list "immutable hashalw uses structural keys"
                    (let* ([stored (list 'a)]
                           [lookup (list 'a)]
                           [h      (hashalw stored 'value)]
                           [h2     (hash-remove h lookup)])
                      (and (not (eq? stored lookup))
                           (equal? stored lookup)
                           (equal? (hash-ref h lookup #f) 'value)
                           (eq? (hash-ref-key h lookup) stored)
                           (hash-has-key? h lookup)
                           (equal? (hash-count h2) 0)
                           (not (hash-has-key? h2 stored)))))
              (list "immutable hashalw distinguishes mutable boxes"
                    (let* ([a  (box 'same)]
                           [b  (box 'same)]
                           [h  (hashalw a 'a b 'b)]
                           [h* (hash-remove h a)])
                      (and (equal? a b)
                           (not (equal-always? a b))
                           (equal? (hash-count h) 2)
                           (equal? (hash-ref h a #f) 'a)
                           (equal? (hash-ref h b #f) 'b)
                           (eq? (hash-ref-key h a) a)
                           (eq? (hash-ref-key h b) b)
                           (not (hash-has-key? h* a))
                           (equal? (hash-ref h* b #f) 'b))))
              (list "immutable equal HAMT caches packed inline key hashes"
                    (let ([hash-calls (box 0)])
                      (struct counted (n)
                        #:property prop:equal+hash
                        (list
                         (lambda (self other recur)
                           (and (counted? other)
                                (equal? (counted-n self) (counted-n other))))
                         (lambda (self hash)
                           (set-box! hash-calls (+ (unbox hash-calls) 1))
                           1)
                         (lambda (self hash)
                           (set-box! hash-calls (+ (unbox hash-calls) 1))
                           1)))
                      (let* ([k0 (counted 0)]
                             [h0 (hash-set (hash) k0 'zero)]
                             [calls-after-first (unbox hash-calls)]
                             [h1 (hash-set h0 (counted 1) 'one)]
                             [stored (hash-ref-key h1 (counted 0) #f)])
                        (and (equal? calls-after-first 1)
                             (equal? (unbox hash-calls) 3)
                             (counted? stored)
                             (equal? (counted-n stored) 0)
                             (equal? (hash-ref h1 (counted 1) #f) 'one)
                             (equal? (unbox hash-calls) 4)))))
              (list "immutable equal HAMT recursive bucket split"
                    (let ([keys '(0 8 16 24 32 40 48 56 64)])
                      (let loop ([ks keys] [h (hash)])
                        (if (null? ks)
                            (let* ([h* (hash-remove h 32)]
                                   [h** (hash-set h* 72 72)])
                              (and (equal? (hash-count h) 9)
                                   (equal? (hash-count h*) 8)
                                   (equal? (hash-count h**) 9)
                                   (not (hash-has-key? h* 32))
                                   (equal? (hash-ref h** 72 #f) 72)
                                   (andmap (lambda (k)
                                             (or (equal? k 32)
                                                 (equal? (hash-ref h* k #f) k)))
                                           keys)))
                            (loop (cdr ks)
                                  (hash-set h (car ks) (car ks)))))))
              (list "immutable hashalw HAMT recursive bucket split"
                    (let ([keys '(0 8 16 24 32 40 48 56 64)])
                      (let loop ([ks keys] [h (hashalw)])
                        (if (null? ks)
                            (let* ([h* (hash-remove h 40)]
                                   [h** (hash-set h* 72 72)])
                              (and (equal? (hash-count h) 9)
                                   (equal? (hash-count h*) 8)
                                   (equal? (hash-count h**) 9)
                                   (not (hash-has-key? h* 40))
                                   (equal? (hash-ref h** 72 #f) 72)
                                   (andmap (lambda (k)
                                             (or (equal? k 40)
                                                 (equal? (hash-ref h* k #f) k)))
                                           keys)))
                            (loop (cdr ks)
                                  (hash-set h (car ks) (car ks)))))))
              (list "immutable HAMT deep bnode stress"
                    (let ([keys '(0 8 16 24 32 40 48 56
                                    64 72 80 88 96 104 112 120
                                    128 136 144 152 160 168 176 184)])
                      (let build ([ks keys] [hqv (hasheqv)] [heq (hash)] [halw (hashalw)])
                        (if (null? ks)
                            (let* ([hqv* (hash-remove hqv 88)]
                                   [heq* (hash-remove heq 96)]
                                   [halw* (hash-remove halw 104)])
                              (and (equal? (hash-count hqv) 24)
                                   (equal? (hash-count hqv*) 23)
                                   (not (hash-has-key? hqv* 88))
                                   (equal? (hash-count heq*) 23)
                                   (not (hash-has-key? heq* 96))
                                   (equal? (hash-count halw*) 23)
                                   (not (hash-has-key? halw* 104))
                                   (andmap (lambda (k)
                                             (and (or (equal? k 88)
                                                      (equal? (hash-ref hqv* k #f) k))
                                                  (or (equal? k 96)
                                                      (equal? (hash-ref heq* k #f) k))
                                                  (or (equal? k 104)
                                                      (equal? (hash-ref halw* k #f) k))))
                                           keys)))
                            (build (cdr ks)
                                   (hash-set hqv (car ks) (car ks))
                                   (hash-set heq (car ks) (car ks))
                                   (hash-set halw (car ks) (car ks)))))))
              (list "immutable HAMT cnode full hash collision"
                    (let ()
                      (struct collide (n)
                        #:property prop:equal+hash
                        (list
                         (lambda (self other recur)
                           (and (collide? other)
                                (equal? (collide-n self) (collide-n other))))
                         (lambda (self hash) 7)
                         (lambda (self hash) 7)))
                      (let build ([i 0] [h (hash)])
                        (if (equal? i 7)
                            (let* ([h*  (hash-remove h (collide 3))]
                                   [h** (hash-set h* (collide 7) 70)]
                                   [h1  (hash-remove
                                         (hash-remove
                                          (hash-remove
                                           (hash-remove
                                            (hash-remove
                                             (hash-remove h (collide 0))
                                             (collide 1))
                                            (collide 2))
                                           (collide 3))
                                          (collide 4))
                                         (collide 5))]
                                   [h1* (hash-set h1 (collide 8) 80)]
                                   [xs  (hash->list h**)])
                              (and (equal? (hash-count h) 7)
                                   (equal? (hash-count h*) 6)
                                   (equal? (hash-count h**) 7)
                                   (equal? (hash-count h1) 1)
                                   (equal? (hash-ref h1 (collide 6) #f) 60)
                                   (equal? (hash-ref h1* (collide 8) #f) 80)
                                   (not (hash-has-key? h* (collide 3)))
                                   (equal? (hash-ref h** (collide 7) #f) 70)
                                   (equal? (length xs) 7)
                                   (andmap (lambda (j)
                                             (or (equal? j 3)
                                                 (equal? (hash-ref h* (collide j) #f)
                                                         (* j 10))))
                                           '(0 1 2 3 4 5 6))))
                            (build (+ i 1)
                                   (hash-set h (collide i) (* i 10)))))))
              (list "immutable HAMT hashalw cnode full hash collision"
                    (let ()
                      (struct collide-always (n)
                        #:property prop:equal+hash
                        (list
                         (lambda (self other recur mode)
                           (and (collide-always? other)
                                (if mode
                                    #f
                                    (recur (collide-always-n self)
                                           (collide-always-n other)))))
                         (lambda (self hash mode) 11)))
                      (let build ([i 0] [h (hashalw)])
                        (if (equal? i 7)
                            (let* ([h*  (hash-remove h (collide-always 2))]
                                   [h** (hash-set h* (collide-always 7) 70)]
                                   [xs  (hash->list h**)])
                              (and (equal? (hash-count h) 7)
                                   (equal? (hash-count h*) 6)
                                   (equal? (hash-count h**) 7)
                                   (not (hash-has-key? h* (collide-always 2)))
                                   (equal? (hash-ref h** (collide-always 7) #f) 70)
                                   (equal? (length xs) 7)
                                   (andmap (lambda (j)
                                             (or (equal? j 2)
                                                 (equal? (hash-ref h* (collide-always j) #f)
                                                         (* j 10))))
                                           '(0 1 2 3 4 5 6))))
                            (build (+ i 1)
                                   (hash-set h (collide-always i) (* i 10)))))))
              (list "hash-remove immutable"
                    (and (let* ([h  (hash 'a 1 'b 2)]
                                 [h2 (hash-remove h 'a)]
                                 [h3 (hash-remove h 'missing)])
                           (and (hash? h2)
                                (immutable-hash? h2)
                                (hash-equal? h2)
                                (equal? (hash-count h) 2)
                                (equal? (hash-count h2) 1)
                                (equal? (hash-has-key? h2 'a) #f)
                                (equal? (hash-ref h2 'b) 2)
                                (eq? h h3)))
                         (let* ([h  (hasheq 'a 1 'b 2)]
                                 [h2 (hash-remove h 'b)])
                           (and (hash-eq? h2)
                                (equal? (hash-count h2) 1)
                                (equal? (hash-ref h2 'a) 1)
                                (equal? (hash-has-key? h2 'b) #f)))
                         (let* ([h  (hasheqv 'a 1 'b 2)]
                                 [h2 (hash-remove h 'a)])
                           (and (hash-eqv? h2)
                                (equal? (hash-count h2) 1)
                                (equal? (hash-ref h2 'b) 2)
                                (equal? (hash-has-key? h2 'a) #f)))
                         (let* ([h  (hashalw 'a 1 'b 2)]
                                 [h2 (hash-remove h 'a)])
                           (and (hash-equal-always? h2)
                                (equal? (hash-count h2) 1)
                                (equal? (hash-ref h2 'b) 2)
                                (equal? (hash-has-key? h2 'a) #f)))))
              (list "make-immutable-hasheqv"
                    (let ([h (make-immutable-hasheqv '((1 . one) (2 . two) (1 . uno)))])
                      (and (immutable-hash? h)
                           (hash-eqv? h)
                           (equal? (hash-count h) 2)
                           (equal? (hash-ref h 1) 'uno)
                           (equal? (hash-ref h 2) 'two))))
              (list "make-immutable-hash variants"
                    (let ([h   (make-immutable-hash '((1 . one) (2 . two) (1 . uno)))]
                          [hal (make-immutable-hashalw '((1 . one)))]
                          [heq (make-immutable-hasheq '((a . 1)))])
                      (and (immutable-hash? h)
                           (hash-equal? h)
                           (hash-equal-always? hal)
                           (hash-eq? heq)
                           (equal? (hash-count h) 2)
                           (equal? (hash-ref h 1) 'uno)
                           (equal? (hash-ref h 2) 'two))))
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
              (list "hash predicates reject exception structs"
                    (let ([e (exn:fail "nope" #f)])
                      (and (not (hash? e))
                           (not (mutable-hash? e))
                           (not (immutable-hash? e)))))

              (list "make-hash"
                    (let ([h (make-hash)])
                      (and (hash? h) (mutable-hash? h))))
              (list "make-hasheq"
                    (let ([h (make-hasheq)])
                      (and (hash? h) (mutable-hash? h))))
              (list "make-ephemeron-hasheq strong fallback"
                    (let ([h (make-ephemeron-hasheq)])
                      (hash-set! h 'a 1)
                      (and (hash? h)
                           (mutable-hash? h)
                           (hash-eq? h)
                           (equal? (hash-ref h 'a) 1))))
              (list "make-ephemeron strong fallback"
                    (let ([e (make-ephemeron 'k 'v)])
                      (and (ephemeron? e)
                           (equal? (ephemeron-value e) 'v)
                           (equal? (ephemeron-value e 'gone) 'v)
                           (equal? (ephemeron-value e 'gone 'retain) 'v))))
              (list "make-hasheqv"
                    (let ([h (make-hasheqv)])
                      (and (hash? h) (mutable-hash? h))))
              (list "make-hashalw"
                    (let ([h (make-hashalw)])
                      (and (hash? h) (mutable-hash? h))))

              (list "mutable equal hash structural keys"
                    (let* ([stored (list 'a)]
                           [lookup (list 'a)]
                           [h      (make-hash)])
                      (hash-set! h stored 'value)
                      (and (not (eq? stored lookup))
                           (equal? stored lookup)
                           (equal? (hash-ref h lookup #f) 'value)
                           (eq? (hash-ref-key h lookup) stored)
                           (hash-has-key? h lookup)
                           (begin
                             (hash-remove! h lookup)
                             (not (hash-has-key? h stored))))))

              (list "mutable hasheqv numeric keys"
                    (let* ([x (fl+ 1.0 0.0)]
                           [y (fl+ 0.5 0.5)]
                           [h (make-hasheqv)])
                      (hash-set! h x 'one)
                      (hash-set! h 0.0 'zero)
                      (hash-set! h -0.0 'neg-zero)
                      (and (eqv? x y)
                           (not (eq? x y))
                           (equal? (hash-ref h y #f) 'one)
                           (equal? (hash-count h) 3)
                           (equal? (hash-ref h 0.0 #f) 'zero)
                           (equal? (hash-ref h -0.0 #f) 'neg-zero)
                           (begin
                             (hash-remove! h y)
                             (not (hash-has-key? h x))))))

              (list "mutable hashalw comparison keys"
                    (let* ([stored (list 'a)]
                           [lookup (list 'a)]
                           [a      (box 'same)]
                           [b      (box 'same)]
                           [h      (make-hashalw)])
                      (hash-set! h stored 'list)
                      (hash-set! h a 'a)
                      (hash-set! h b 'b)
                      (and (equal? (hash-ref h lookup #f) 'list)
                           (eq? (hash-ref-key h lookup) stored)
                           (equal? (hash-ref h a #f) 'a)
                           (equal? (hash-ref h b #f) 'b)
                           (not (equal-always? a b))
                           (equal? (hash-count h) 3))))

              (list "hash equality and hash codes"
                    (let* ([h1  (hash (list 'a) 1 'b 2)]
                           [h2  (hash (list 'a) 1 'b 2)]
                           [heq (hasheq (string-copy "a") 1)]
                           [hal1 (hashalw (box-immutable 'a) 1)]
                           [hal2 (hashalw (box-immutable 'a) 1)]
                           [m1  (make-hash)]
                           [m2  (make-hash)])
                      (hash-set! m1 (list 'a) 1)
                      (hash-set! m2 (list 'a) 1)
                      (and (equal? h1 h2)
                           (eq? (equal-hash-code h1) (equal-hash-code h2))
                           (not (equal? h1 heq))
                           (equal? hal1 hal2)
                           (eq? (equal-hash-code hal1) (equal-hash-code hal2))
                           (equal? m1 m2)
                           (eq? (equal-hash-code m1) (equal-hash-code m2))
                           (not (equal? h1 m1)))))

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
              (list "hash-ref missing key errors are catchable with key detail"
                    (let ([catch-contract/contains?
                           (lambda (thunk . needles)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (andmap (lambda (needle)
                                              (string-contains? message needle))
                                            needles))))])
                      (and
                       (catch-contract/contains?
                        (lambda () (hash-ref (make-hash) 42))
                        "hash-ref"
                        "no value found for key"
                        "key: 42")
                       (catch-contract/contains?
                        (lambda () (hash-ref (hash) 42))
                        "hash-ref"
                        "no value found for key"
                        "key: 42")
                       (catch-contract/contains?
                        (lambda () (hash-ref-key (make-hash) 42))
                        "hash-ref-key"
                        "hash does not contain key"
                        "key: 42")
                       (catch-contract/contains?
                        (lambda () (hash-ref-key (hash) 42))
                        "hash-ref-key"
                        "hash does not contain key"
                        "key: 42")
                       (catch-contract/contains?
                        (lambda () (hash-update! (make-hash) 42 add1))
                        "hash-update!"
                        "no value found for key: 42"))))

              (list "immutable hash varargs odd argument errors are catchable with who"
                    (let ([catch-contract/contains?
                           (lambda (thunk . needles)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (andmap (lambda (needle)
                                              (string-contains? message needle))
                                            needles))))])
                      (and
                       (catch-contract/contains?
                        (lambda () (hash 'a))
                        "hash"
                        "key does not have a value"
                        "key: a")
                       (catch-contract/contains?
                        (lambda () (hasheq 'a))
                        "hasheq"
                        "key does not have a value"
                        "key: a")
                       (catch-contract/contains?
                        (lambda () (hasheqv 'a))
                        "hasheqv"
                        "key does not have a value"
                        "key: a")
                       (catch-contract/contains?
                        (lambda () (hashalw 'a))
                        "hashalw"
                        "key does not have a value"
                        "key: a"))))

              (list "mutable hash constructor alist errors are catchable with who"
                    (let ([catch-contract/contains?
                           (lambda (thunk . needles)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (andmap (lambda (needle)
                                              (string-contains? message needle))
                                            needles))))])
                      (and
                       (catch-contract/contains?
                        (lambda () (make-hash 42))
                        "make-hash"
                        "(listof pair?)"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (make-hasheq '(x)))
                        "make-hasheq"
                        "(listof pair?)"
                        "given: '(x)")
                       (catch-contract/contains?
                        (lambda () (make-hasheqv '(x)))
                        "make-hasheqv"
                        "(listof pair?)"
                        "given: '(x)")
                       (catch-contract/contains?
                        (lambda () (make-hashalw '(x)))
                        "make-hashalw"
                        "(listof pair?)"
                        "given: '(x)"))))

              (list "immutable hash constructor alist errors are catchable with who"
                    (let ([catch-contract/contains?
                           (lambda (thunk . needles)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (andmap (lambda (needle)
                                              (string-contains? message needle))
                                            needles))))])
                      (and
                       (catch-contract/contains?
                        (lambda () (make-immutable-hash 42))
                        "make-immutable-hash"
                        "(listof pair?)"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (make-immutable-hasheq '(x)))
                        "make-immutable-hasheq"
                        "(listof pair?)"
                        "given: '(x)")
                       (catch-contract/contains?
                        (lambda () (make-immutable-hasheqv '(x)))
                        "make-immutable-hasheqv"
                        "(listof pair?)"
                        "given: '(x)")
                       (catch-contract/contains?
                        (lambda () (make-immutable-hashalw '(x)))
                        "make-immutable-hashalw"
                        "(listof pair?)"
                        "given: '(x)"))))

              (list "read-only hash argument errors are catchable with who"
                    (let ([catch-contract/contains?
                           (lambda (thunk . needles)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (andmap (lambda (needle)
                                              (string-contains? message needle))
                                            needles))))])
                      (and
                       (catch-contract/contains?
                        (lambda () (hash-ref 42 'x))
                        "hash-ref"
                        "hash?"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-ref-key 42 'x))
                        "hash-ref-key"
                        "hash?"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-has-key? 42 'x))
                        "hash-has-key?"
                        "hash?"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-count 42))
                        "hash-count"
                        "hash?"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-empty? 42))
                        "hash-empty?"
                        "hash?"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash->list 42))
                        "hash->list"
                        "hash?"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-keys 42))
                        "hash-keys"
                        "hash?"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-values 42))
                        "hash-values"
                        "hash?"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-copy 42))
                        "hash-copy"
                        "hash?"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-eq? 42))
                        "hash-eq?"
                        "hash?"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-eqv? 42))
                        "hash-eqv?"
                        "hash?"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-equal? 42))
                        "hash-equal?"
                        "hash?"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-equal-always? 42))
                        "hash-equal-always?"
                        "hash?"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-for-each 42 (lambda (k v) #t)))
                        "hash-for-each"
                        "hash?"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-map 42 (lambda (k v) k)))
                        "hash-map"
                        "hash?"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-map/copy 42 (lambda (k v) (values k v))))
                        "hash-map/copy"
                        "hash?"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-filter 42 (lambda (k v) #t)))
                        "hash-filter"
                        "hash?"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-filter-keys 42 (lambda (k) #t)))
                        "hash-filter-keys"
                        "hash?"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-filter-values 42 (lambda (v) #t)))
                        "hash-filter-values"
                        "hash?"
                        "given: 42"))))

              (list "immutable hash operation mutability errors are catchable with who"
                    (let ([catch-contract/contains?
                           (lambda (thunk . needles)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (andmap (lambda (needle)
                                              (string-contains? message needle))
                                            needles))))])
                      (and
                       (catch-contract/contains?
                        (lambda () (hash-set 42 'x 1))
                        "hash-set"
                        "(and/c hash? immutable?)"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-set (make-hash) 'x 1))
                        "hash-set"
                        "(and/c hash? immutable?)"
                        "given:")
                       (catch-contract/contains?
                        (lambda () (hash-remove (make-hash) 'x))
                        "hash-remove"
                        "(and/c hash? immutable?)"
                        "given:")
                       (catch-contract/contains?
                        (lambda () (hash-update (make-hash) 'x add1 0))
                        "hash-update"
                        "(and/c hash? immutable?)"
                        "given:")
                       (catch-contract/contains?
                        (lambda () (hash-clear (make-hash)))
                        "hash-clear"
                        "(and/c hash? immutable?)"
                        "given:"))))

              (list "mutable hash argument errors are catchable with who"
                    (let ([catch-contract/contains?
                           (lambda (thunk . needles)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (andmap (lambda (needle)
                                              (string-contains? message needle))
                                            needles))))])
                      (and
                       (catch-contract/contains?
                        (lambda () (hash-set! 42 'x 1))
                        "hash-set!"
                        "(and/c hash? (not/c immutable?))"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-set! (hash 'a 1) 'x 1))
                        "hash-set!"
                        "(and/c hash? (not/c immutable?))"
                        "given:")
                       (catch-contract/contains?
                        (lambda () (hash-ref! 42 'x 1))
                        "hash-ref!"
                        "(and/c hash? (not/c immutable?))"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-ref! (hash 'a 1) 'x 1))
                        "hash-ref!"
                        "(and/c hash? (not/c immutable?))"
                        "given:")
                       (catch-contract/contains?
                        (lambda () (hash-update! 42 'x add1))
                        "hash-update!"
                        "(and/c hash? (not/c immutable?))"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-update! (hash 'a 1) 'a add1))
                        "hash-update!"
                        "(and/c hash? (not/c immutable?))"
                        "given:")
                       (catch-contract/contains?
                        (lambda () (hash-remove! 42 'x))
                        "hash-remove!"
                        "(and/c hash? (not/c immutable?))"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-remove! (hash 'a 1) 'a))
                        "hash-remove!"
                        "(and/c hash? (not/c immutable?))"
                        "given:")
                       (catch-contract/contains?
                        (lambda () (hash-clear! 42))
                        "hash-clear!"
                        "(and/c hash? (not/c immutable?))"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-clear! (hash 'a 1)))
                        "hash-clear!"
                        "(and/c hash? (not/c immutable?))"
                        "given:"))))

              (list "hash-ref-key"
                    (and (let ([h (make-hasheq)])
                           (hash-set! h 'a 1)
                           (and (eq? (hash-ref-key h 'a) 'a)
                                (equal? (hash-ref-key h 'missing 'fallback) 'fallback)
                                (equal? (hash-ref-key h 'missing (lambda () 'thunked)) 'thunked)))
                         (let ([h (make-hasheqv)])
                           (hash-set! h 10 'ten)
                           (and (equal? (hash-ref-key h 10) 10)
                                (equal? (hash-ref-key h 11 'fallback) 'fallback)))
                         (let ([h (make-hash)])
                           (hash-set! h 'a 1)
                           (and (eq? (hash-ref-key h 'a) 'a)
                                (equal? (hash-ref-key h 'missing 'fallback) 'fallback)))
                         (let ([h (make-hashalw)])
                           (hash-set! h 'a 1)
                           (and (eq? (hash-ref-key h 'a) 'a)
                                (equal? (hash-ref-key h 'missing 'fallback) 'fallback)))
                         (let ([h (hasheqv 1 'one 2 'two)])
                           (and (equal? (hash-ref-key h 1) 1)
                                (equal? (hash-ref-key h 3 'fallback) 'fallback)))
                         (let* ([stored (string #\a)]
                                [lookup (string #\a)]
                                [h      (hash stored 'value)])
                           (and (not (eq? stored lookup))
                                (eq? (hash-ref-key h lookup) stored)))))

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

              (list "hash-copy"
                    (let* ([h (hash 'a 1 'b 2)]
                           [c (hash-copy h)])
                      (and (hash? c)
                           (not (immutable? c))
                           (hash-equal? c)
                           (equal? (hash-count c) 2)
                           (equal? (hash-ref c 'a) 1)
                           (begin (hash-set! c 'c 3)
                                  (and (equal? (hash-count c) 3)
                                       (equal? (hash-has-key? h 'c) #f))))))

              (list "hash->list"
                    (let ([<< (λ (x y) (< (car x) (car y)))])
                      (let ([h (make-hasheq)])
                        (hash-set! h 1 'a)
                        (hash-set! h 2 'b)
                        (hash-set! h 3 'c)
                        (let ([expected (list (cons 1 'a) (cons 2 'b) (cons 3 'c))])
                          (and (equal? (sort (hash->list h)    <<) expected)
                               (equal? (sort (hash->list h #t) <<) expected))))))

              (list "immutable hash->list"
                    (let ([<< (λ (x y) (symbol<? (car x) (car y)))])
                      (let* ([empty    (hash)]
                             [h        (hash 'a 1 'b 2)]
                             [expected (list (cons 'a 1) (cons 'b 2))])
                        (and (equal? (hash->list empty) '())
                             (equal? (sort (hash->list h) <<) expected)))))

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

              (list "higher-order hash procedure errors preserve who"
                    (let ([catch-contract/contains?
                           (lambda (thunk . needles)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (andmap (lambda (needle)
                                              (string-contains? message needle))
                                            needles))))])
                      (and
                       (catch-contract/contains?
                        (lambda () (hash-for-each (make-hash '((a . 1))) 42))
                        "hash-for-each"
                        "(procedure-arity-includes/c 2)"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-for-each (hash 'a 1) 42))
                        "hash-for-each"
                        "(procedure-arity-includes/c 2)"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-map (make-hash '((a . 1))) 42))
                        "hash-map"
                        "(procedure-arity-includes/c 2)"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-map (hash 'a 1) 42))
                        "hash-map"
                        "(procedure-arity-includes/c 2)"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-map/copy (make-hash '((a . 1))) 42))
                        "hash-map/copy"
                        "(procedure-arity-includes/c 2)"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-map/copy (hash 'a 1) 42))
                        "hash-map/copy"
                        "(procedure-arity-includes/c 2)"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-filter (make-hash '((a . 1))) 42))
                        "hash-filter"
                        "procedure?"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-filter (hash 'a 1) 42))
                        "hash-filter"
                        "procedure?"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-filter-keys (make-hash '((a . 1))) 42))
                        "hash-filter-keys"
                        "procedure?"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-filter-values (hash 'a 1) 42))
                        "hash-filter-values"
                        "procedure?"
                        "given: 42"))))

              (list "hash update procedure errors preserve who"
                    (let ([catch-contract/contains?
                           (lambda (thunk . needles)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (andmap (lambda (needle)
                                              (string-contains? message needle))
                                            needles))))])
                      (and
                       (catch-contract/contains?
                        (lambda () (hash-update (hash 'a 1) 'a 42))
                        "hash-update"
                        "(any/c . -> . any/c)"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-update! (make-hash '((a . 1))) 'a 42))
                        "hash-update!"
                        "(any/c . -> . any/c)"
                        "given: 42"))))

              (list "immutable hash map/copy and filters"
                    (let* ([h       (hash 'a 1 'b 2 'c 3)]
                           [copy    (hash-map/copy h (lambda (k v) (values k (+ v 10))))]
                           [by-pair (hash-filter h (lambda (k v) (< v 3)))]
                           [by-key  (hash-filter-keys h (lambda (k) (or (eq? k 'a) (eq? k 'c))))]
                           [by-val  (hash-filter-values h (lambda (v) (< v 3)))]
                           [eqh     (hasheq 'a 1 'b 2)]
                           [eqf     (hash-filter eqh (lambda (k v) (eq? k 'a)))])
                      (and (immutable-hash? copy)
                           (hash-equal? copy)
                           (equal? (hash-count copy) 3)
                           (equal? (hash-ref copy 'a) 11)
                           (equal? (hash-ref copy 'b) 12)
                           (immutable-hash? by-pair)
                           (hash-equal? by-pair)
                           (equal? (hash-count by-pair) 2)
                           (equal? (hash-ref by-pair 'a) 1)
                           (equal? (hash-ref by-pair 'b) 2)
                           (equal? (hash-ref by-pair 'c #f) #f)
                           (immutable-hash? by-key)
                           (equal? (hash-count by-key) 2)
                           (equal? (hash-ref by-key 'a) 1)
                           (equal? (hash-ref by-key 'c) 3)
                           (equal? (hash-ref by-key 'b #f) #f)
                           (immutable-hash? by-val)
                           (equal? (hash-count by-val) 2)
                           (equal? (hash-ref by-val 'a) 1)
                           (equal? (hash-ref by-val 'b) 2)
                           (equal? (hash-ref by-val 'c #f) #f)
                           (immutable-hash? eqf)
                           (hash-eq? eqf)
                           (equal? (hash-count eqf) 1)
                           (equal? (hash-ref eqf 'a) 1))))

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

              (list "hash-keys-subset?"
                    (let ([small (hasheq 'a 1 'b 2)]
                          [large (hasheq 'a 10 'b 20 'c 30)]
                          [miss  (hasheq 'a 10 'c 30)])
                      (and (equal? (hash-keys-subset? small large) #t)
                           (equal? (hash-keys-subset? small miss) #f)
                           (equal? (hash-keys-subset? (hasheq) miss) #t))))

              (list "hash-values"
                    (let ([h (make-hasheq)])
                      (hash-set! h 'a 1)
                      (hash-set! h 'b 2)
                      (let ([expected '(1 2)])
                        (and (equal? (sort (hash-values h)    (λ (x y) (< x y))) expected)
                             (equal? (sort (hash-values h #t) (λ (x y) (< x y))) expected)))))

              (list "hash-keys and hash-values first-class optional arity"
                    (let ([h (make-hasheq)])
                      (hash-set! h 'a 1)
                      (hash-set! h 'b 2)
                      (let ([ks hash-keys]
                            [vs hash-values])
                        (and (equal? (sort (ks h)    (λ (x y) (symbol<? x y))) '(a b))
                             (equal? (sort (ks h #t) (λ (x y) (symbol<? x y))) '(a b))
                             (equal? (sort (vs h)    (λ (x y) (< x y))) '(1 2))
                             (equal? (sort (vs h #t) (λ (x y) (< x y))) '(1 2))))))

              (list "hash-iterate public mutable and immutable"
                    (let ([m (make-hasheq)]
                          [i (hash 'a 1 'b 2)])
                      (hash-set! m 'a 1)
                      (hash-set! m 'b 2)
                      (define (unordered-equal? xs ys)
                        (and (= (length xs) (length ys))
                             (andmap (lambda (x) (member x ys)) xs)
                             (andmap (lambda (y) (member y xs)) ys)))
                      (define (collect h f)
                        (let loop ([pos (hash-iterate-first h)] [acc '()] [positions '()])
                          (if pos
                              (loop (hash-iterate-next h pos)
                                    (cons (f pos) acc)
                                    (cons pos positions))
                              (list acc positions))))
                      (let* ([m-pairs+positions
                              (collect m (lambda (pos)
                                           (hash-iterate-pair m pos 'bad)))]
                             [i-pairs+positions
                              (collect i (lambda (pos)
                                           (hash-iterate-pair i pos 'bad)))]
                             [i-kvs
                              (car (collect i
                                            (lambda (pos)
                                              (call-with-values
                                               (lambda ()
                                                 (hash-iterate-key+value i pos 'bad))
                                               cons))))]
                             [i-keys
                              (car (collect i
                                            (lambda (pos)
                                              (hash-iterate-key i pos 'bad))))]
                             [i-values
                              (car (collect i
                                            (lambda (pos)
                                              (hash-iterate-value i pos 'bad))))]
                             [expected '((a . 1) (b . 2))])
                        (and (equal? (hash-iterate-first (hash)) #f)
                             (andmap fixnum? (cadr i-pairs+positions))
                             (unordered-equal? (car m-pairs+positions) expected)
                             (unordered-equal? (car i-pairs+positions) expected)
                             (unordered-equal? i-kvs expected)
                             (equal? (sort i-keys symbol<?) '(a b))
                             (equal? (sort i-values <) '(1 2))
                             (equal? (hash-iterate-key i 999 'bad) 'bad)
                             (equal? (hash-iterate-value i 999 'bad) 'bad)
                             (equal? (hash-iterate-pair i 999 'bad) '(bad . bad))
                             (equal? (call-with-values
                                      (lambda ()
                                        (hash-iterate-key+value i 999 'bad))
                                      cons)
                                     '(bad . bad))))))

              (list "hash iteration type errors preserve who"
                    (let ([catch-contract/contains?
                           (lambda (thunk . needles)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (andmap (lambda (needle)
                                              (string-contains? message needle))
                                            needles))))])
                      (and
                       (catch-contract/contains?
                        (lambda () (hash-iterate-first 42))
                        "hash-iterate-first:"
                        "expected: hash?"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-iterate-next 42 0))
                        "hash-iterate-next:"
                        "expected: hash?"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-iterate-key 42 0))
                        "hash-iterate-key:"
                        "expected: hash?"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-iterate-value 42 0))
                        "hash-iterate-value:"
                        "expected: hash?"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (hash-iterate-pair 42 0))
                        "hash-iterate-pair:"
                        "expected: hash?"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda ()
                          (call-with-values
                           (lambda () (hash-iterate-key+value 42 0))
                           cons))
                        "hash-iterate-key+value:"
                        "expected: hash?"
                        "given: 42"))))

              (list "hash iteration bad-index errors are catchable with who"
                    (let ([catch-contract/contains?
                           (lambda (thunk . needles)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (andmap (lambda (needle)
                                              (string-contains? message needle))
                                            needles))))])
                      (let ([i (hash 'a 1)]
                            [m (make-hash '((a . 1)))])
                        (and
                         (catch-contract/contains?
                          (lambda () (hash-iterate-key i 99))
                          "hash-iterate-key: no element at index"
                          "no element at index"
                          "index: 99")
                         (catch-contract/contains?
                          (lambda () (hash-iterate-value i 99))
                          "hash-iterate-value: no element at index"
                          "no element at index"
                          "index: 99")
                         (catch-contract/contains?
                          (lambda () (hash-iterate-pair i 99))
                          "hash-iterate-pair: no element at index"
                          "no element at index"
                          "index: 99")
                         (catch-contract/contains?
                          (lambda ()
                            (call-with-values
                             (lambda () (hash-iterate-key+value i 99))
                             cons))
                          "hash-iterate-key+value: no element at index"
                          "no element at index"
                          "index: 99")
                         (catch-contract/contains?
                          (lambda () (hash-iterate-key m 99))
                          "hash-iterate-key: no element at index"
                          "no element at index"
                          "index: 99")))))

              (list "mutable hash iteration type errors preserve who"
                    (let ([catch-contract/contains?
                           (lambda (thunk . needles)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (andmap (lambda (needle)
                                              (string-contains? message needle))
                                            needles))))])
                      (and
                       (catch-contract/contains?
                        (lambda () (mutable-hash-iterate-first 42))
                        "mutable-hash-iterate-first:"
                        "expected: (and/c hash? (not/c immutable?))"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (mutable-hash-iterate-next 42 0))
                        "mutable-hash-iterate-next:"
                        "expected: (and/c hash? (not/c immutable?))"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (mutable-hash-iterate-key 42 0))
                        "mutable-hash-iterate-key:"
                        "expected: (and/c hash? (not/c immutable?))"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (mutable-hash-iterate-value 42 0))
                        "mutable-hash-iterate-value:"
                        "expected: (and/c hash? (not/c immutable?))"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda () (mutable-hash-iterate-pair 42 0))
                        "mutable-hash-iterate-pair:"
                        "expected: (and/c hash? (not/c immutable?))"
                        "given: 42")
                       (catch-contract/contains?
                        (lambda ()
                          (call-with-values
                           (lambda () (mutable-hash-iterate-key+value 42 0))
                           cons))
                        "mutable-hash-iterate-key+value:"
                        "expected: (and/c hash? (not/c immutable?))"
                        "given: 42"))))

              (list "mutable hash iteration position errors preserve who"
                    (let ([catch-contract/contains?
                           (lambda (thunk . needles)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (andmap (lambda (needle)
                                              (string-contains? message needle))
                                            needles))))])
                      (let ([m (make-hash '((a . 1)))])
                        (and
                         (catch-contract/contains?
                          (lambda () (mutable-hash-iterate-next m -1))
                          "mutable-hash-iterate-next:"
                          "exact-nonnegative-integer?"
                          "given: -1")
                         (catch-contract/contains?
                          (lambda () (mutable-hash-iterate-key m #f))
                          "mutable-hash-iterate-key:"
                          "exact-nonnegative-integer?"
                          "given: #f")
                         (catch-contract/contains?
                          (lambda () (mutable-hash-iterate-value m -1))
                          "mutable-hash-iterate-value:"
                          "exact-nonnegative-integer?"
                          "given: -1")
                         (catch-contract/contains?
                          (lambda () (mutable-hash-iterate-pair m -1))
                          "mutable-hash-iterate-pair:"
                          "exact-nonnegative-integer?"
                          "given: -1")
                         (catch-contract/contains?
                          (lambda ()
                            (call-with-values
                             (lambda () (mutable-hash-iterate-key+value m -1))
                             cons))
                          "mutable-hash-iterate-key+value:"
                          "exact-nonnegative-integer?"
                          "given: -1")))))

              (list "mutable hash iteration bad-index errors preserve who"
                    (let ([catch-contract/contains?
                           (lambda (thunk . needles)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (andmap (lambda (needle)
                                              (string-contains? message needle))
                                            needles))))])
                      (let ([m (make-hash '((a . 1)))])
                        (and
                         (catch-contract/contains?
                          (lambda () (mutable-hash-iterate-key m 99))
                          "mutable-hash-iterate-key: no element at index"
                          "no element at index"
                          "index: 99")
                         (catch-contract/contains?
                          (lambda () (mutable-hash-iterate-value m 99))
                          "mutable-hash-iterate-value: no element at index"
                          "no element at index"
                          "index: 99")
                         (catch-contract/contains?
                          (lambda () (mutable-hash-iterate-pair m 99))
                          "mutable-hash-iterate-pair: no element at index"
                          "no element at index"
                          "index: 99")
                         (catch-contract/contains?
                          (lambda ()
                            (call-with-values
                             (lambda () (mutable-hash-iterate-key+value m 99))
                             cons))
                          "mutable-hash-iterate-key+value: no element at index"
                          "no element at index"
                          "index: 99")))))

              (list "hash iteration position contract errors preserve who"
                    (let ([catch-contract/contains?
                           (lambda (thunk . needles)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (andmap (lambda (needle)
                                              (string-contains? message needle))
                                            needles))))])
                      (let ([i (hash 'a 1)]
                            [m (make-hash '((a . 1)))])
                        (and
                         (catch-contract/contains?
                          (lambda () (hash-iterate-next i 'x))
                          "hash-iterate-next"
                          "exact-nonnegative-integer?"
                          "given: 'x")
                         (catch-contract/contains?
                          (lambda () (hash-iterate-next m -1))
                          "hash-iterate-next"
                          "exact-nonnegative-integer?"
                          "given: -1")
                         (catch-contract/contains?
                          (lambda () (hash-iterate-key i #f))
                          "hash-iterate-key"
                          "exact-nonnegative-integer?"
                          "given: #f")
                         (catch-contract/contains?
                          (lambda () (hash-iterate-value i -1))
                          "hash-iterate-value"
                          "exact-nonnegative-integer?"
                          "given: -1")
                         (catch-contract/contains?
                          (lambda () (hash-iterate-pair i -1))
                          "hash-iterate-pair"
                          "exact-nonnegative-integer?"
                          "given: -1")
                         (catch-contract/contains?
                          (lambda ()
                            (call-with-values
                             (lambda () (hash-iterate-key+value i -1))
                             cons))
                          "hash-iterate-key+value"
                          "exact-nonnegative-integer?"
                          "given: -1")
                         (catch-contract/contains?
                          (lambda () (hash-iterate-key m -1))
                          "hash-iterate-key"
                          "exact-nonnegative-integer?"
                          "given: -1")))))

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

              (list "unsafe-immutable-hash-iterate"
                    (let ([empty (hasheq)]
                          [h     (hasheq 'x 10 'y 20)])
                      (define (unordered-equal? xs ys)
                        (and (= (length xs) (length ys))
                             (andmap (lambda (x) (member x ys)) xs)
                             (andmap (lambda (y) (member y xs)) ys)))

                      (define (collect f)
                        (let loop ([pos (unsafe-immutable-hash-iterate-first h)] [acc '()])
                          (if pos
                              (loop (unsafe-immutable-hash-iterate-next h pos)
                                    (cons (f pos) acc))
                              acc)))

                      (let* ([pairs    (collect (lambda (pos)
                                                  (unsafe-immutable-hash-iterate-pair h pos)))]
                             [kvs      (collect (lambda (pos)
                                                  (call-with-values
                                                   (lambda ()
                                                     (unsafe-immutable-hash-iterate-key+value h pos))
                                                   cons)))]
                             [keys     (collect (lambda (pos)
                                                  (unsafe-immutable-hash-iterate-key h pos)))]
                             [vals     (collect (lambda (pos)
                                                  (unsafe-immutable-hash-iterate-value h pos)))]
                             [expected '((x . 10) (y . 20))])
                        (and (equal? (unsafe-immutable-hash-iterate-first empty) #f)
                             (unordered-equal? pairs expected)
                             (unordered-equal? kvs expected)
                             (equal? (sort keys symbol<?) '(x y))
                             (equal? (sort vals <) '(10 20))))))

              (list "unsafe-immutable-hash-iterate cnode"
                    (let ()
                      (struct collide (n)
                        #:property prop:equal+hash
                        (list
                         (lambda (self other recur)
                           (and (collide? other)
                                (equal? (collide-n self) (collide-n other))))
                         (lambda (self hash) 13)
                         (lambda (self hash) 13)))

                      (define h
                        (let build ([i 0] [h (hash)])
                          (if (equal? i 7)
                              h
                              (build (+ i 1)
                                     (hash-set h (collide i) (* i 10))))))

                      (define (collect f)
                        (let loop ([pos (unsafe-immutable-hash-iterate-first h)] [acc '()])
                          (if pos
                              (loop (unsafe-immutable-hash-iterate-next h pos)
                                    (cons (f pos) acc))
                              acc)))

                      (let* ([pairs (collect (lambda (pos)
                                               (unsafe-immutable-hash-iterate-pair h pos)))]
                             [kvs   (collect (lambda (pos)
                                               (call-with-values
                                                (lambda ()
                                                  (unsafe-immutable-hash-iterate-key+value h pos))
                                                cons)))]
                             [keys  (collect (lambda (pos)
                                               (unsafe-immutable-hash-iterate-key h pos)))]
                             [vals  (collect (lambda (pos)
                                               (unsafe-immutable-hash-iterate-value h pos)))])
                        (and (equal? (length pairs) 7)
                             (equal? (length kvs) 7)
                             (equal? (length keys) 7)
                             (equal? (sort vals <) '(0 10 20 30 40 50 60))
                             (andmap (lambda (j)
                                       (and (member (cons (collide j) (* j 10)) pairs)
                                            (member (cons (collide j) (* j 10)) kvs)
                                            #t))
                                     '(0 1 2 3 4 5 6))))))

              (list "immutable-hash-debug-shape"
                    (let ()
                      (struct hashkey (mode n)
                        #:property prop:equal+hash
                        (list
                         (lambda (self other recur)
                           (and (hashkey? other)
                                (equal? (hashkey-mode self) (hashkey-mode other))
                                (equal? (hashkey-n self) (hashkey-n other))))
                         (lambda (self hash)
                           (cond
                             [(equal? (hashkey-mode self) 'split)
                              (* (hashkey-n self) 8)]
                             [(equal? (hashkey-mode self) 'single) 1]
                             [else 13]))
                         (lambda (self hash)
                           (cond
                             [(equal? (hashkey-mode self) 'split)
                              (* (hashkey-n self) 8)]
                             [(equal? (hashkey-mode self) 'single) 1]
                             [else 13]))))

                      (define (shape h)
                        (webracket-internal-immutable-hash-debug-shape h))
                      (define (entries s) (list-ref s 0))
                      (define (bnodes s)  (list-ref s 1))
                      (define (cnodes s)  (list-ref s 2))
                      (define (buckets s) (list-ref s 3))
                      (define (inlines s) (list-ref s 4))
                      (define (depth s)   (list-ref s 5))
                      (define (slots s)   (list-ref s 6))

                      (define packed0
                        (webracket-internal-immutable-hamt-packed-bits 0 0))
                      (define packed9
                        (webracket-internal-immutable-hamt-packed-bits 9 0))
                      (define packed8/3
                        (webracket-internal-immutable-hamt-packed-bits 8 3))
                      (define shallow (hasheq 'x 1 'y 2))
                      (define mixed
                        (let loop ([i 0] [h (hash)])
                          (if (equal? i 5)
                              (hash-set h (hashkey 'single 0) 'single)
                              (loop (+ i 1)
                                    (hash-set h (hashkey 'split i) i)))))
                      (define inline-cycle
                        (let* ([one   (hash-set (hash) (hashkey 'single 0) 0)]
                               [two   (hash-set one (hashkey 'single 1) 1)]
                               [one*  (hash-remove two (hashkey 'single 0))])
                          (list (shape one)
                                (webracket-internal-immutable-hash-root-metadata one)
                                (shape two)
                                (webracket-internal-immutable-hash-root-metadata two)
                                (shape one*)
                                (webracket-internal-immutable-hash-root-metadata one*)
                                (hash-ref one* (hashkey 'single 1) #f)
                                (let ([k (hash-ref-key one* (hashkey 'single 1) #f)])
                                  (list (hashkey-mode k) (hashkey-n k)))
                                (hash-has-key? one* (hashkey 'single 1))
                                (hash-has-key? one* (hashkey 'single 0)))))
                      (define churn
                        (let build ([i 0] [h (hash)])
                          (if (equal? i 16)
                              (let remove ([j 3] [h h])
                                (if (equal? j 6)
                                    h
                                    (remove (+ j 1)
                                            (hash-remove h
                                                         (hashkey
                                                          'split
                                                          j)))))
                              (build (+ i 1)
                                     (hash-set h
                                               (hashkey
                                                (if (< i 3) 'single
                                                    (if (< i 8) 'split 'collide))
                                                i)
                                               i)))))
                      (define deep
                        (let loop ([i 0] [h (hash)])
                          (if (equal? i 9)
                              h
                              (loop (+ i 1) (hash-set h (hashkey 'split i) i)))))
                      (define full-collision
                        (let loop ([i 0] [h (hash)])
                          (if (equal? i 7)
                              h
                              (loop (+ i 1)
                                    (hash-set h (hashkey 'collide i) (* i 10))))))
                      (define collapsed
                        (let loop ([i 0] [h full-collision])
                          (if (equal? i 6)
                              h
                              (loop (+ i 1) (hash-remove h (hashkey 'collide i))))))

                      (let ([empty-shape     (shape (hasheq))]
                            [shallow-shape   (shape shallow)]
                            [mixed-shape     (shape mixed)]
                            [mixed-meta      (webracket-internal-immutable-hash-root-metadata mixed)]
                            [churn-shape     (shape churn)]
                            [churn-meta      (webracket-internal-immutable-hash-root-metadata churn)]
                            [deep-shape      (shape deep)]
                            [deep-meta       (webracket-internal-immutable-hash-root-metadata deep)]
                            [collision-shape (shape full-collision)]
                            [collision-meta  (webracket-internal-immutable-hash-root-metadata full-collision)]
                            [collapsed-shape (shape collapsed)])
	                        (and (equal? inline-cycle
	                                     '((1 1 0 0 1 0 2)
	                                       263169
	                                       (2 11 1 0 0 11 11)
	                                       5
	                                       (1 0 0 0 1 0 0)
	                                       0
                                       1
                                       (single 1)
                                       #t
                                       #f))
                             (equal? packed0 '(0 2 512 131072 1))
                             (equal? packed9 '(1 4 1024 262144 2))
                             (equal? packed8/3 '(1 4 1024 262144 2))
                             (equal? empty-shape '(0 0 0 0 0 0 0))
                             (equal? (entries shallow-shape) 2)
                             (equal? (bnodes shallow-shape) 1)
                             (equal? (cnodes shallow-shape) 0)
	                             (equal? (buckets shallow-shape) 0)
	                             (equal? (inlines shallow-shape) 2)
	                             (equal? (slots shallow-shape) 4)
	                             (equal? mixed-shape '(6 2 0 0 6 1 13))
                             (equal? mixed-meta 263171)
	                             (equal? (entries churn-shape) 13)
	                             (>= (bnodes churn-shape) 2)
                             (>= (cnodes churn-shape) 1)
	                             (equal? (buckets churn-shape) 0)
	                             (>= (inlines churn-shape) 1)
                             (equal? churn-meta 71)
	                             (equal? (entries deep-shape) 9)
                             (>= (bnodes deep-shape) 2)
                             (equal? (cnodes deep-shape) 0)
                             (>= (depth deep-shape) 2)
	                             (>= (slots deep-shape) 9)
                             (equal? deep-meta 3)
	                             (equal? (entries collision-shape) 7)
	                             (>= (bnodes collision-shape) 1)
	                             (equal? (cnodes collision-shape) 1)
	                             (equal? (buckets collision-shape) 0)
	                             (>= (depth collision-shape) 1)
                             (equal? collision-meta 65)
                             (equal? (entries collapsed-shape) 1)
                             (equal? (cnodes collapsed-shape) 0)))))

              (list "immutable hash collision and iteration stress"
                    (let loop ([i 0] [h (hash)])
                      (if (equal? i 90)
                          (let* ([h*       (hash-remove h (list 26))]
                                 [pairs    (hash->list h*)]
                                 [keys     (hash-keys h*)]
                                 [values   (hash-values h*)]
                                 [mapped   (hash-map h* (lambda (k v) v))]
                                 [expected (reverse (let make ([j 0] [acc '()])
                                             (cond
                                               [(equal? j 90) acc]
                                               [(equal? j 26) (make (+ j 1) acc)]
                                               [else (make (+ j 1) (cons j acc))])))])
                            (and (equal? (hash-count h) 90)
                                 (equal? (hash-count h*) 89)
                                 (not (hash-has-key? h* (list 26)))
                                 (andmap (lambda (j)
                                           (equal? (hash-ref h (list j)) j))
                                         (let make ([j 0] [acc '()])
                                           (if (equal? j 90) acc (make (+ j 1) (cons j acc)))))
                                 (andmap (lambda (j)
                                           (equal? (hash-ref h* (list j)) j))
                                         expected)
                                 (equal? (length pairs) 89)
	                                 (equal? (length keys) 89)
	                                 (equal? (sort values <) expected)
	                                 (equal? (sort mapped <) expected)))
	                          (loop (+ i 1) (hash-set h (list i) i)))))

	              (list "immutable HAMT cross-kind add/remove stress"
	                    (let ()
	                      (define eq-keys
	                        '(k00 k01 k02 k03 k04 k05 k06 k07
	                              k08 k09 k10 k11 k12 k13 k14 k15
	                              k16 k17 k18 k19 k20 k21 k22 k23
	                              k24 k25 k26 k27 k28 k29 k30 k31))
	                      (define (keep? i)
	                        (not (equal? (remainder i 3) 1)))
	                      (define expected
	                        (let loop ([i 0] [acc '()])
	                          (cond
	                            [(equal? i 32) (reverse acc)]
	                            [(keep? i) (loop (+ i 1) (cons i acc))]
	                            [else (loop (+ i 1) acc)])))
	                      (define (exercise empty make-key same-key?)
	                        (let* ([built
	                                (let build ([i 0] [h empty])
	                                  (if (equal? i 32)
	                                      h
	                                      (build (+ i 1)
	                                             (hash-set h (make-key i) (* i 10)))))]
	                               [trimmed
	                                (let remove ([i 0] [h built])
	                                  (if (equal? i 32)
	                                      h
	                                      (remove (+ i 1)
	                                              (if (keep? i)
	                                                  h
	                                                  (hash-remove h (make-key i))))))]
	                               [pairs (hash->list trimmed)])
	                          (and (equal? (hash-count built) 32)
	                               (equal? (hash-count trimmed) (length expected))
	                               (andmap
	                                (lambda (i)
	                                  (and (hash-has-key? trimmed (make-key i))
	                                       (equal? (hash-ref trimmed (make-key i)) (* i 10))
	                                       (same-key? (hash-ref-key trimmed (make-key i))
	                                                  (make-key i))))
	                                expected)
	                               (andmap
	                                (lambda (i)
	                                  (not (hash-has-key? trimmed (make-key i))))
	                                '(1 4 7 10 13 16 19 22 25 28 31))
	                               (equal? (sort (map cdr pairs) <)
	                                       (map (lambda (i) (* i 10)) expected)))))
	                      (and (exercise (hasheq)
	                                     (lambda (i) (list-ref eq-keys i))
	                                     eq?)
	                           (exercise (hasheqv)
	                                     (lambda (i) i)
	                                     equal?)
	                           (exercise (hash)
	                                     (lambda (i) (list i))
	                                     equal?)
	                           (exercise (hashalw)
	                                     (lambda (i) (list i))
	                                     equal?))))

              (list "immutable HAMT larger deterministic cross-kind stress"
                    (let ()
                      (define (keep? i)
                        (not (equal? (remainder i 7) 3)))
                      (define (updated-value i)
                        (if (equal? (remainder i 5) 0)
                            (+ 1000 i)
                            (* i 10)))
                      (define expected
                        (let loop ([i 0] [acc '()])
                          (cond
                            [(equal? i 96) (reverse acc)]
                            [(keep? i) (loop (+ i 1) (cons i acc))]
                            [else (loop (+ i 1) acc)])))
                      (define (exercise empty make-key same-key?)
                        (let* ([built
                                (let build ([i 0] [h empty])
                                  (if (equal? i 96)
                                      h
                                      (build (+ i 1)
                                             (hash-set h (make-key i) (* i 10)))))]
                               [updated
                                (let update ([i 0] [h built])
                                  (if (equal? i 96)
                                      h
                                      (update (+ i 1)
                                              (if (equal? (remainder i 5) 0)
                                                  (hash-set h (make-key i) (updated-value i))
                                                  h))))]
                               [trimmed
                                (let remove ([i 0] [h updated])
                                  (if (equal? i 96)
                                      h
                                      (remove (+ i 1)
                                              (if (keep? i)
                                                  h
                                                  (hash-remove h (make-key i))))))]
                               [pairs (hash->list trimmed)])
                          (and (equal? (hash-count built) 96)
                               (equal? (hash-count updated) 96)
                               (equal? (hash-count trimmed) (length expected))
                               (andmap
                                (lambda (i)
                                  (and (hash-has-key? trimmed (make-key i))
                                       (equal? (hash-ref trimmed (make-key i))
                                               (updated-value i))
                                       (same-key? (hash-ref-key trimmed (make-key i))
                                                  (make-key i))))
                                expected)
                               (let absent ([i 0])
                                 (cond
                                   [(equal? i 96) #t]
                                   [(keep? i) (absent (+ i 1))]
                                   [else
                                    (and (not (hash-has-key? trimmed (make-key i)))
                                         (absent (+ i 1)))]))
                               (equal? (sort (map cdr pairs) <)
                                       (sort (map updated-value expected) <)))))
                      (and (exercise (hasheq)
                                     (lambda (i) i)
                                     equal?)
                           (exercise (hasheqv)
                                     (lambda (i) i)
                                     equal?)
                           (exercise (hash)
                                     (lambda (i) (list (remainder (* i 37) 101) i))
                                     equal?)
                           (exercise (hashalw)
                                     (lambda (i) (list (remainder (* i 37) 101) i))
                                     equal?))))

	              ))

       (list "4.20.2 Reflecting on Procedures"
             (list
              (list "arity-at-least? from procedure-arity"
                    (let ([arity (procedure-arity list)])
                      (and (equal? (arity-at-least? arity) #t)
                           (equal? (arity-at-least-value arity) 0))))

              (list "arity-at-least-value for rest lambda"
                    (let ([arity (procedure-arity (lambda (x . rest) x))])
                      (and (arity-at-least? arity)
                           (equal? (arity-at-least-value arity) 1))))

              (list "arity-at-least constructors"
                    (let ([arity1 (arity-at-least 2)]
                          [arity2 (make-arity-at-least 2)])
                      (and (arity-at-least? arity1)
                           (arity-at-least? arity2)
                           (equal? (arity-at-least-value arity1) 2)
                           (equal? (arity-at-least-value arity2) 2)
                           (equal? arity1 arity2))))

              (list "procedure-arity case-lambda mix"
                    (let ([arity (procedure-arity
                                   (case-lambda
                                     [(x) x]
                                     [(x y . rest) y]))])
                      (and (pair? arity)
                           (equal? (car arity) 1)
                           (let ([arity2 (cadr arity)])
                             (and (arity-at-least? arity2)
                                  (equal? (arity-at-least-value arity2) 2))))))

              (list "procedure->method identity semantics"
                    (let* ([proc   (lambda (self x) (+ x 1))]
                           [method (procedure->method proc)])
                      (and (procedure? method)
                           (eq? method proc)
                           (equal? (method 'self 41) 42))))))
       
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
                           (equal? (p-ref 42 (lambda () 'non-struct-fallback)) 'non-struct-fallback)
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
              (list "prop:evt/builtin"
                    (equal? (struct-type-property? prop:evt) #t))

              (list "prop:checked-procedure/check-and-extract"
                    (let ()
                      (struct checked (ok proc)
                        #:property prop:checked-procedure #t)
                      (define c
                        (checked
                         (lambda (kws argc)
                           (and (equal? kws '(#:x))
                                (equal? argc 2)))
                         (lambda args (cons 'fast args))))
                      (define (fallback v kws argc)
                        (list 'fallback v kws argc))
                      (and
                       (equal? ((checked-procedure-check-and-extract
                                  struct:checked c fallback '(#:x) 2)
                                'a 'b)
                               '(fast a b))
                       (equal? (checked-procedure-check-and-extract
                                struct:checked c fallback '(#:y) 2)
                               (list 'fallback c '(#:y) 2))
                       (equal? (checked-procedure-check-and-extract
                                struct:checked 'not-checked fallback '(#:x) 2)
                               '(fallback not-checked (#:x) 2)))))

              (list "procedure?/plain-struct"
                    (let ()
                      (struct plain (x))
                      (equal? (procedure? (plain 1)) #f)))

              (list "procedure-arity/plain-struct"
                    (let ()
                      (struct plain (x))
                      (with-handlers ([exn:fail:contract? (lambda (exn) #t)])
                        (procedure-arity (plain 1))
                        #f)))

              (list "procedure-struct-type?/plain"
                    (let ()
                      (struct plain (x))
                      (and (equal? (procedure-struct-type? struct:plain) #f)
                           (with-handlers ([exn:fail:contract? (lambda (_exn) #t)])
                             (procedure-struct-type? 42)
                             #f))))

              (list "prop:procedure/apply-field"
                    (let ()
                      (struct callable (f) #:property prop:procedure 0)
                      (define wrapped (callable (lambda (x y) (+ x y))))
                      (and (procedure? wrapped)
                           (equal? (procedure-struct-type? struct:callable) #t)
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
                           (equal? (procedure-struct-type? struct:counter) #t)
                           (equal? (c -2) 13))))

              (list "prop:procedure/proc-spec-field"
                    (let-values ([(struct:p make-p p? p-ref p-set!)
                                  (make-struct-type 'p #f 1 0 #f null #f 0 '(0))])
                      (define p (make-p (lambda (x) (+ x 1))))
                      (and (procedure? p)
                           (equal? (procedure-struct-type? struct:p) #t)
                           (equal? (p 41) 42))))

              (list "prop:procedure/proc-spec-procedure"
                    (let ()
                      (define-values (struct:p make-p p? p-ref p-set!)
                        (make-struct-type
                         'p #f 1 0 #f null #f
                         (lambda (self delta)
                           (+ (p-ref self 0) delta))))
                      (define p (make-p 10))
                      (and (procedure? p)
                           (equal? (procedure-struct-type? struct:p) #t)
                           (equal? (p 5) 15))))
              
              ))

       (list "5.2 Prefab Structures"
             (list
              (list "prefab utilities/simple"
                    (let ()
                      (define p (make-prefab-struct '(prefab-point 2 (0 #f) #()) 11 31))
                      (define p/auto (make-prefab-struct '(prefab-auto 1 (1 #f) #()) 42))
                      (define p/auto-zero (make-prefab-struct '(prefab-auto-value 1 (1 0) #()) 42))
                      (define p/auto-one (make-prefab-struct '(prefab-auto-value 1 (1 1) #()) 42))
                      (define p/auto-list (make-prefab-struct '(prefab-auto-list 1 (1 (0 marker)) #()) 42))
                      (define p/auto-vector (make-prefab-struct '(prefab-auto-vector 1 (1 #(0 marker)) #()) 42))
                      (define p/mutable (make-prefab-struct '(prefab-cell 2 (0 #f) #(1)) 10 20))
                      (define p/multi (make-prefab-struct '(prefab-multi 3 (0 #f) #(0 2)) 1 2 3))
                      (define p/child
                        (make-prefab-struct
                         '(prefab-child 1 (0 #f) #() prefab-parent 2 (0 #f) #())
                         10 20 30))
                      (define p/child-alias
                        (make-prefab-struct '(prefab-child prefab-parent 2) 10 20 30))
                      (define p/child-alias-explicit-count
                        (make-prefab-struct '(prefab-child 1 prefab-parent 2) 10 20 30))
                      (define p/child-alias-mutable
                        (make-prefab-struct '(prefab-child #(0) prefab-parent 2 #(0 1)) 10 20 30))
                      (define p/child-alias-explicit-mutable
                        (make-prefab-struct '(prefab-child 1 #(0) prefab-parent 2 #(0 1)) 10 20 30))
                      (define p/grandchild
                        (make-prefab-struct '(prefab-grandchild prefab-child 1 prefab-parent 2)
                                            10 20 30 40))
                      (define p/grandchild-explicit-count
                        (make-prefab-struct '(prefab-grandchild 1 prefab-child 1 prefab-parent 2)
                                            10 20 30 40))
                      (define p/deep-grandchild
                        (make-prefab-struct
                         '(prefab-deep-grandchild 1 (0 #f) #()
                           prefab-grandchild 1 (0 #f) #()
                           prefab-child 1 (0 #f) #()
                           prefab-parent 2 (0 #f) #())
                         10 20 30 40 50))
                      (define p/deep-grandchild-alias
                        (make-prefab-struct
                         '(prefab-deep-grandchild prefab-grandchild 1 prefab-child 1 prefab-parent 2)
                         10 20 30 40 50))
                      (define p/deep-grandchild-alias-count
                        (make-prefab-struct
                         '(prefab-deep-grandchild 1 prefab-grandchild 1 prefab-child 1 prefab-parent 2)
                         10 20 30 40 50))
                      (define-values (struct:prefab-point make-prefab-point prefab-point?
                                                          prefab-point-ref prefab-point-set!)
                        (make-struct-type 'prefab-point #f 2 0 #f #f 'prefab))
                      (define-values (struct:prefab-auto make-prefab-auto prefab-auto?
                                                         prefab-auto-ref prefab-auto-set!)
                        (make-struct-type 'prefab-auto #f 1 1 #f #f 'prefab))
                      (define-values (struct:prefab-auto-value/zero make-prefab-auto-value/zero prefab-auto-value/zero?
                                                                    prefab-auto-value/zero-ref prefab-auto-value/zero-set!)
                        (make-struct-type 'prefab-auto-value #f 1 1 0 #f 'prefab))
                      (define-values (struct:prefab-auto-value/one make-prefab-auto-value/one prefab-auto-value/one?
                                                                   prefab-auto-value/one-ref prefab-auto-value/one-set!)
                        (make-struct-type 'prefab-auto-value #f 1 1 1 #f 'prefab))
                      (define-values (struct:prefab-auto-list make-prefab-auto-list prefab-auto-list?
                                                              prefab-auto-list-ref prefab-auto-list-set!)
                        (make-struct-type 'prefab-auto-list #f 1 1 '(0 marker) #f 'prefab))
                      (define-values (struct:prefab-auto-vector make-prefab-auto-vector prefab-auto-vector?
                                                                prefab-auto-vector-ref prefab-auto-vector-set!)
                        (make-struct-type 'prefab-auto-vector #f 1 1 '#(0 marker) #f 'prefab))
                      (define-values (struct:prefab-cell make-prefab-cell prefab-cell?
                                                         prefab-cell-ref prefab-cell-set!)
                        (make-struct-type 'prefab-cell #f 2 0 #f #f 'prefab #f '(0)))
                      (define-values (struct:prefab-multi make-prefab-multi prefab-multi?
                                                          prefab-multi-ref prefab-multi-set!)
                        (make-struct-type 'prefab-multi #f 3 0 #f #f 'prefab #f '(1)))
                      (define-values (struct:prefab-parent make-prefab-parent prefab-parent?
                                                           prefab-parent-ref prefab-parent-set!)
                        (make-struct-type 'prefab-parent #f 2 0 #f #f 'prefab))
                      (define-values (struct:prefab-child make-prefab-child prefab-child?
                                                          prefab-child-ref prefab-child-set!)
                        (make-struct-type 'prefab-child struct:prefab-parent 1 0 #f #f 'prefab))
                      (define-values (struct:prefab-grandchild make-prefab-grandchild prefab-grandchild?
                                                               prefab-grandchild-ref prefab-grandchild-set!)
                        (make-struct-type 'prefab-grandchild struct:prefab-child 1 0 #f #f 'prefab))
                      (define-values (struct:prefab-deep-grandchild make-prefab-deep-grandchild prefab-deep-grandchild?
                                                                    prefab-deep-grandchild-ref prefab-deep-grandchild-set!)
                        (make-struct-type 'prefab-deep-grandchild struct:prefab-grandchild 1 0 #f #f 'prefab))
                      (define type-info (prefab-struct-type-key+field-count struct:prefab-point))
                      (define auto-type-info (prefab-struct-type-key+field-count struct:prefab-auto))
                      (prefab-cell-set! p/mutable 1 99)
                      (prefab-multi-set! p/multi 0 10)
                      (prefab-multi-set! p/multi 2 30)
                      (and (prefab-key? 'prefab-point)
                           (prefab-key? '(prefab-point))
                           (prefab-key? '(prefab-point 2))
                           (prefab-key? '(prefab-point 2 (0 #f) #()))
                           (prefab-key? '(prefab-auto 1 (1 #f) #()))
                           (prefab-key? '(prefab-auto-value 1 (1 0) #()))
                           (prefab-key? '(prefab-auto-value 1 (1 1) #()))
                           (prefab-key? '(prefab-auto-list 1 (1 (0 marker)) #()))
                           (prefab-key? '(prefab-auto-vector 1 (1 #(0 marker)) #()))
                           (prefab-key? '(prefab-cell 2 (0 #f) #(1)))
                           (prefab-key? '(prefab-multi 3 (0 #f) #(0 2)))
                           (prefab-key? '(prefab-child 1 (0 #f) #() prefab-parent 2 (0 #f) #()))
                           (prefab-key? '(prefab-child prefab-parent 2))
                           (prefab-key? '(prefab-child 1 prefab-parent 2))
                           (prefab-key? '(prefab-child #(0) prefab-parent 2 #(0 1)))
                           (prefab-key? '(prefab-child 1 #(0) prefab-parent 2 #(0 1)))
                           (prefab-key? '(prefab-grandchild prefab-child 1 prefab-parent 2))
                           (prefab-key? '(prefab-grandchild 1 prefab-child 1 prefab-parent 2))
                           (prefab-key? '(prefab-deep-grandchild 1 (0 #f) #()
                                          prefab-grandchild 1 (0 #f) #()
                                          prefab-child 1 (0 #f) #()
                                          prefab-parent 2 (0 #f) #()))
                           (prefab-key? '(prefab-deep-grandchild prefab-grandchild 1 prefab-child 1 prefab-parent 2))
                           (prefab-key? '(prefab-deep-grandchild 1 prefab-grandchild 1 prefab-child 1 prefab-parent 2))
                           (not (prefab-key? '(prefab-multi 3 (0 #f) #(0 0))))
                           (not (prefab-key? '(prefab-deep-grandchild #(0)
                                              prefab-grandchild 1 prefab-child 1 prefab-parent 2)))
                           (not (prefab-key? '(prefab-deep-grandchild 1 #(0)
                                              prefab-grandchild 1 prefab-child 1 prefab-parent 2)))
                           (not (prefab-key? '(prefab-deep-grandchild
                                              prefab-grandchild 1 #(0) prefab-child 1 prefab-parent 2)))
                           (equal? (prefab-struct-key p) 'prefab-point)
                           (equal? (prefab-struct-key p/auto) '(prefab-auto 1 (1 #f) #()))
                           (equal? (prefab-struct-key p/auto-zero) '(prefab-auto-value 1 (1 0) #()))
                           (equal? (prefab-struct-key p/auto-one) '(prefab-auto-value 1 (1 1) #()))
                           (equal? (prefab-struct-key p/auto-list) '(prefab-auto-list 1 (1 (0 marker)) #()))
                           (equal? (prefab-struct-key p/auto-vector) '(prefab-auto-vector 1 (1 #(0 marker)) #()))
                           (equal? (prefab-struct-key p/mutable) '(prefab-cell 2 (0 #f) #(1)))
                           (equal? (prefab-struct-key p/multi) '(prefab-multi 3 (0 #f) #(0 2)))
                           (equal? (prefab-struct-key p/child)
                                   '(prefab-child 1 (0 #f) #() prefab-parent 2 (0 #f) #()))
                           (equal? (prefab-struct-key p/child-alias)
                                   '(prefab-child 1 (0 #f) #() prefab-parent 2 (0 #f) #()))
                           (equal? (prefab-struct-key p/child-alias-explicit-count)
                                   '(prefab-child 1 (0 #f) #() prefab-parent 2 (0 #f) #()))
                           (equal? (prefab-struct-key p/child-alias-mutable)
                                   '(prefab-child 1 (0 #f) #(0) prefab-parent 2 (0 #f) #(0 1)))
                           (equal? (prefab-struct-key p/child-alias-explicit-mutable)
                                   '(prefab-child 1 (0 #f) #(0) prefab-parent 2 (0 #f) #(0 1)))
                           (equal? (prefab-struct-key p/grandchild)
                                   (prefab-struct-key p/grandchild-explicit-count))
                           (equal? (prefab-struct-key p/deep-grandchild)
                                   (prefab-struct-key p/deep-grandchild-alias))
                           (equal? (prefab-struct-key p/deep-grandchild)
                                   (prefab-struct-key p/deep-grandchild-alias-count))
                           (equal? (prefab-point-ref p 0) 11)
                           (equal? (prefab-point-ref p 1) 31)
                           (equal? (prefab-auto-ref p/auto 0) 42)
                           (equal? (prefab-auto-ref p/auto 1) #f)
                           (equal? (prefab-auto-value/zero-ref p/auto-zero 0) 42)
                           (equal? (prefab-auto-value/zero-ref p/auto-zero 1) 0)
                           (equal? (prefab-auto-value/one-ref p/auto-one 0) 42)
                           (equal? (prefab-auto-value/one-ref p/auto-one 1) 1)
                           (equal? (prefab-auto-list-ref p/auto-list 1) '(0 marker))
                           (equal? (prefab-auto-vector-ref p/auto-vector 1) '#(0 marker))
                           (equal? (prefab-cell-ref p/mutable 0) 10)
                           (equal? (prefab-cell-ref p/mutable 1) 99)
                           (equal? (prefab-multi-ref p/multi 0) 10)
                           (equal? (prefab-multi-ref p/multi 1) 2)
                           (equal? (prefab-multi-ref p/multi 2) 30)
                           (equal? (prefab-parent-ref p/child 0) 10)
                           (equal? (prefab-parent-ref p/child 1) 20)
                           (equal? (prefab-child-ref p/child 0) 30)
                           (equal? (prefab-parent-ref p/child-alias 0) 10)
                           (equal? (prefab-parent-ref p/child-alias 1) 20)
                           (equal? (prefab-child-ref p/child-alias 0) 30)
                           (equal? (prefab-parent-ref p/grandchild 0) 10)
                           (equal? (prefab-parent-ref p/grandchild 1) 20)
                           (equal? (prefab-child-ref p/grandchild 0) 30)
                           (equal? (prefab-grandchild-ref p/grandchild 0) 40)
                           (equal? (prefab-parent-ref p/grandchild-explicit-count 0) 10)
                           (equal? (prefab-parent-ref p/grandchild-explicit-count 1) 20)
                           (equal? (prefab-child-ref p/grandchild-explicit-count 0) 30)
                           (equal? (prefab-grandchild-ref p/grandchild-explicit-count 0) 40)
                           (equal? (prefab-parent-ref p/deep-grandchild 0) 10)
                           (equal? (prefab-parent-ref p/deep-grandchild 1) 20)
                           (equal? (prefab-child-ref p/deep-grandchild 0) 30)
                           (equal? (prefab-grandchild-ref p/deep-grandchild 0) 40)
                           (equal? (prefab-deep-grandchild-ref p/deep-grandchild 0) 50)
                           (equal? (prefab-parent-ref p/deep-grandchild-alias 0) 10)
                           (equal? (prefab-parent-ref p/deep-grandchild-alias 1) 20)
                           (equal? (prefab-child-ref p/deep-grandchild-alias 0) 30)
                           (equal? (prefab-grandchild-ref p/deep-grandchild-alias 0) 40)
                           (equal? (prefab-deep-grandchild-ref p/deep-grandchild-alias 0) 50)
                           (equal? (prefab-parent-ref p/deep-grandchild-alias-count 0) 10)
                           (equal? (prefab-parent-ref p/deep-grandchild-alias-count 1) 20)
                           (equal? (prefab-child-ref p/deep-grandchild-alias-count 0) 30)
                           (equal? (prefab-grandchild-ref p/deep-grandchild-alias-count 0) 40)
                           (equal? (prefab-deep-grandchild-ref p/deep-grandchild-alias-count 0) 50)
                           (equal? type-info '(prefab-point . 2))
                           (equal? auto-type-info '((prefab-auto 1 (1 #f) #()) . 2)))))))

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

              (list "input port constructor argument errors are catchable with who"
                    (let ([catch-contract/message-contains?
                           (lambda (parts thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (andmap (lambda (part)
                                              (string-contains? message part))
                                            parts))))])
                      (and
                       (catch-contract/message-contains?
                        '("open-input-bytes: contract violation"
                          "expected: bytes?"
                          "given: 42")
                        (lambda () (open-input-bytes 42)))
                       (catch-contract/message-contains?
                        '("open-input-string: contract violation"
                          "expected: string?"
                          "given: 42")
                        (lambda () (open-input-string 42))))))

              (list "file-position*/string-ports"
                    (let ([in  (open-input-string "λx")]
                          [out (open-output-string)])
                      (and (equal? (file-position* in) 0)
                           (equal? (read-byte in) 206)
                           (equal? (file-position* in) 1)
                           (equal? (read-byte in) 187)
                           (equal? (file-position* in) 2)
                           (equal? (write-string "ok" out) 2)
                           (equal? (file-position* out) 2)
                           (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                             (file-position* 42)
                             #f))))

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

               (list "file-stream-port?/string-ports"
                     (and (equal? (file-stream-port? (open-input-string "x")) #f)
                          (equal? (file-stream-port? (open-output-string)) #f)
                          (equal? (file-stream-port? 42) #f)))

               (list "terminal-port?/browser-ports"
                     (and (equal? (terminal-port? (open-input-string "x")) #f)
                          (equal? (terminal-port? (open-output-string)) #f)
                          (equal? (terminal-port? (current-output-port)) #f)
                          (equal? (terminal-port? 42) #f)))

               (list "file-stream-port?/vfs-file-ports"
                     (let ([path "/app/data/file-stream-port-test.txt"])
                       (call-with-output-file path
                         (lambda (out)
                           (write-string "ok" out))
                         'binary
                         'truncate/replace)
                       (let ([in  (open-input-file path)]
                             [out (open-output-file path 'binary 'append)])
                         (and (equal? (file-stream-port? in) #t)
                              (equal? (file-stream-port? out) #t)))))

               (list "port-closed?/close-input-port"
                     (let ([port (open-input-string "x")])
                       (and (equal? (port-closed? port) #f)
                            (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                              (port-closed? 42)
                              #f)
                            (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                              (close-input-port 42)
                              #f)
                            (void? (close-input-port port))
                            (equal? (port-closed? port) #t)
                            (with-handlers ([exn:fail? (lambda (_ex) #t)])
                              (read-byte port)
                              #f)
                            (with-handlers ([exn:fail? (lambda (_ex) #t)])
                              (peek-byte port)
                              #f))))

               (list "port-closed?/close-output-port"
                     (let ([port (open-output-string)])
                       (and (equal? (port-closed? port) #f)
                            (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                              (close-output-port 42)
                              #f)
                            (void? (close-output-port port))
                            (equal? (port-closed? port) #t)
                            (with-handlers ([exn:fail? (lambda (_ex) #t)])
                              (write-byte 65 port)
                              #f))))

               (list "current-output-port/runtime"
                     (let ([original (current-output-port)]
                           [port     (open-output-string)])
                       (and (output-port? original)
                            (void? (current-output-port port))
                            (eq? (current-output-port) port)
                            (void? (write-byte 65))
                            (void? (write-char #\B))
                            (equal? (write-bytes #"CD") 2)
                            (equal? (write-string "EF") 2)
                            (void? (newline))
                            (void? (flush-output))
                            (equal? (get-output-string port) "ABCDEF\n")
                            (void? (current-output-port original)))))

               (list "current-input-port/runtime"
                     (let ([original (current-input-port)]
                           [port     (open-input-string "ABCD\nEF")])
                       (and (input-port? original)
                            (void? (current-input-port port))
                            (eq? (current-input-port) port)
                            (equal? (peek-byte) 65)
                            (equal? (peek-char) #\A)
                            (equal? (read-byte) 65)
                            (equal? (read-char) #\B)
                            (equal? (read-bytes 2) #"CD")
                            (equal? (read-line) "")
                            (equal? (read-string 2) "EF")
                            (eof-object? (read-byte))
                            (void? (current-input-port original)))))

               (list "current-read-interaction/bootstrap"
                     (let ([original (current-read-interaction)])
                       (and (procedure? original)
                            (eof-object? (original 'source (open-input-string "")))
                            (void? (current-read-interaction
                                    (lambda (_source _in) 'interaction)))
                            (equal? ((current-read-interaction)
                                     'source
                                     (open-input-string "ignored"))
                                    'interaction)
                            (void? (current-read-interaction original)))))

               (list "port-read-handler/bootstrap"
                     (let* ([in       (open-input-string "ignored")]
                            [default  (port-read-handler in)])
                       (and (procedure? default)
                            (eof-object? (default in)))))

               (list "reader bootstrap parameters/defaults"
                     (let ([ok #t])
                       (unless (boolean? (load-on-demand-enabled)) (set! ok #f))
                       (unless (boolean? (use-user-specific-search-paths)) (set! ok #f))
                       (unless (boolean? (read-accept-reader)) (set! ok #f))
                       (unless (boolean? (read-accept-lang)) (set! ok #f))
                       (unless (boolean? (read-accept-compiled)) (set! ok #f))
                       (unless (boolean? (read-accept-box)) (set! ok #f))
                       (unless (boolean? (read-accept-bar-quote)) (set! ok #f))
                       (unless (boolean? (read-accept-graph)) (set! ok #f))
                       ok))

               (list "reader bracket bootstrap parameters"
                     (let ([square-paren (read-square-bracket-as-paren)]
                           [curly-paren (read-curly-brace-as-paren)]
                           [square-tag (read-square-bracket-with-tag)]
                           [curly-tag (read-curly-brace-with-tag)]
                           [ok #t])
                       (read-square-bracket-as-paren #f)
                       (unless (eq? (read-square-bracket-as-paren) #f) (set! ok #f))
                       (read-square-bracket-as-paren square-paren)
                       (read-curly-brace-as-paren #f)
                       (unless (eq? (read-curly-brace-as-paren) #f) (set! ok #f))
                       (read-curly-brace-as-paren curly-paren)
                       (read-square-bracket-with-tag #t)
                       (unless (eq? (read-square-bracket-with-tag) #t) (set! ok #f))
                       (read-square-bracket-with-tag square-tag)
                       (read-curly-brace-with-tag #t)
                       (unless (eq? (read-curly-brace-with-tag) #t) (set! ok #f))
                       (read-curly-brace-with-tag curly-tag)
                       ok))

               (list "reader graph and numeric bootstrap parameters"
                     (let ([syntax-graph (read-syntax-accept-graph)]
                           [cdot (read-cdot)]
                           [single-flo (read-single-flonum)]
                           [decimal-inexact (read-decimal-as-inexact)]
                           [ok #t])
                       (read-syntax-accept-graph #t)
                       (unless (eq? (read-syntax-accept-graph) #t) (set! ok #f))
                       (read-syntax-accept-graph syntax-graph)
                       (read-cdot #t)
                       (unless (eq? (read-cdot) #t) (set! ok #f))
                       (read-cdot cdot)
                       (read-single-flonum #t)
                       (unless (eq? (read-single-flonum) #t) (set! ok #f))
                       (read-single-flonum single-flo)
                       (read-decimal-as-inexact #f)
                       (unless (eq? (read-decimal-as-inexact) #f) (set! ok #f))
                       (read-decimal-as-inexact decimal-inexact)
                       ok))

               (list "reader current-readtable/bootstrap"
                     (let ([ok #t])
                       (unless (eq? (current-readtable) #f) (set! ok #f))
                       (current-readtable #f)
                       (unless (eq? (current-readtable) #f) (set! ok #f))
                       ok))

               (list "close-input-port/custom-close-order"
                     (let ([port #f])
                       (let ([closed-during-close #t]
                             [close-count 0])
                         (set! port
                               (make-input-port
                                'custom-close-order
                                (lambda (dest) eof)
                                #f
                                (lambda ()
                                  (set! close-count (add1 close-count))
                                  (set! closed-during-close (port-closed? port)))))
                         (and (void? (close-input-port port))
                              (equal? closed-during-close #f)
                              (equal? (port-closed? port) #t)
                              (void? (close-input-port port))
                              (equal? close-count 1)))))

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

              (list "read-byte-or-special/basic"
                    (let ([port (open-input-bytes (bytes 65))])
                      (and (equal? (read-byte-or-special port) 65)
                           (eof-object? (read-byte-or-special port)))))

              (list "read-byte-or-special/optional"
                    (let ([port (open-input-bytes (bytes 66))])
                      (and (equal? (read-byte-or-special port values 'source) 66)
                           (eof-object? (read-byte-or-special port #f #f)))))

              (list "read-char-or-special/basic"
                    (let ([port (open-input-string "λ")])
                      (and (char=? (read-char-or-special port) #\λ)
                           (eof-object? (read-char-or-special port)))))

              (list "read-char-or-special/optional"
                    (let ([port (open-input-string "x")])
                      (and (char=? (read-char-or-special port values 'source) #\x)
                           (eof-object? (read-char-or-special port #f #f)))))

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

              (list "read buffer wrappers accept immutable destinations"
                    (let* ([b1 (bytes->immutable-bytes (bytes 95 95 95))]
                           [p1 (open-input-bytes (bytes 65 66 67))]
                           [c1 (read-bytes! b1 p1 0 3)]
                           [e1 (read-byte p1)]
                           [b2 (bytes->immutable-bytes (bytes 95 95 95))]
                           [p2 (open-input-bytes (bytes 68 69 70))]
                           [c2 (read-bytes-avail! b2 p2 0 3)]
                           [e2 (read-byte p2)]
                           [b3 (bytes->immutable-bytes (bytes 95 95 95))]
                           [p3 (open-input-bytes (bytes 71 72 73))]
                           [c3 (read-bytes-avail!* b3 p3 0 3)]
                           [e3 (read-byte p3)]
                           [s  (string->immutable-string (string #\_ #\_ #\_))]
                           [ps (open-input-string "abc")]
                           [cs (read-string! s ps 0 3)]
                           [es (read-char ps)])
                      (and (equal? c1 3)
                           (equal? b1 (bytes 65 66 67))
                           (eof-object? e1)
                           (equal? c2 3)
                           (equal? b2 (bytes 68 69 70))
                           (eof-object? e2)
                           (equal? c3 3)
                           (equal? b3 (bytes 71 72 73))
                           (eof-object? e3)
                           (equal? cs 3)
                           (equal? s "abc")
                           (eof-object? es))))

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

              (list "read wrapper argument errors are catchable with who"
                    (let ([catch-contract/message-contains?
                           (lambda (parts thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (andmap (lambda (part)
                                              (string-contains? message part))
                                            parts))))])
                      (and
                       (catch-contract/message-contains?
                        '("read-bytes: contract violation"
                          "expected: exact-nonnegative-integer?"
                          "given: #f")
                        (lambda ()
                          (read-bytes #f (open-input-bytes #"abc"))))
                       (catch-contract/message-contains?
                        '("read-bytes: contract violation"
                          "expected: exact-nonnegative-integer?"
                          "given: -1")
                        (lambda ()
                          (read-bytes -1 (open-input-bytes #"abc"))))
                       (catch-contract/message-contains?
                        '("read-bytes: contract violation"
                          "expected: input-port?"
                          "given: 42")
                        (lambda ()
                          (read-bytes 1 42)))
                       (catch-contract/message-contains?
                        '("read-string: contract violation"
                          "expected: exact-nonnegative-integer?"
                          "given: -1")
                        (lambda ()
                          (read-string -1 (open-input-string "abc"))))
                       (catch-contract/message-contains?
                        '("read-string: contract violation"
                          "expected: exact-nonnegative-integer?"
                          "given: #f")
                        (lambda ()
                          (read-string #f (open-input-string "abc"))))
                       (catch-contract/message-contains?
                        '("read-string: contract violation"
                          "expected: input-port?"
                          "given: 42")
                        (lambda ()
                          (read-string 1 42)))
                       (catch-contract/message-contains?
                        '("read-bytes!: contract violation"
                          "expected: input-port?"
                          "given: 42")
                        (lambda ()
                          (read-bytes! (make-bytes 3 0) 42)))
                       (catch-contract/message-contains?
                        '("read-bytes!: contract violation"
                          "expected: bytes?"
                          "given: 42")
                        (lambda ()
                          (read-bytes! 42 (open-input-bytes #"abc"))))
                       (catch-contract/message-contains?
                        '("read-bytes-avail!: contract violation"
                          "expected: input-port?"
                          "given: 42")
                        (lambda ()
                          (read-bytes-avail! (make-bytes 3 0) 42)))
                       (catch-contract/message-contains?
                        '("read-bytes-avail!: contract violation"
                          "expected: bytes?"
                          "given: 42")
                        (lambda ()
                          (read-bytes-avail! 42 (open-input-bytes #"abc"))))
                       (catch-contract/message-contains?
                        '("read-bytes-avail!*: contract violation"
                          "expected: input-port?"
                          "given: 42")
                        (lambda ()
                          (read-bytes-avail!* (make-bytes 3 0) 42)))
                       (catch-contract/message-contains?
                        '("read-bytes-avail!*: contract violation"
                          "expected: bytes?"
                          "given: 42")
                        (lambda ()
                          (read-bytes-avail!* 42 (open-input-bytes #"abc"))))
                       (catch-contract/message-contains?
                        '("read-string!: contract violation"
                          "expected: input-port?"
                          "given: 42")
                        (lambda ()
                          (read-string! (make-string 3 #\x) 42)))
                       (catch-contract/message-contains?
                        '("read-string!: contract violation"
                          "expected: string?"
                          "given: 42")
                        (lambda ()
                          (read-string! 42 (open-input-string "abc"))))
                       (catch-contract/message-contains?
                        '("read-line: contract violation"
                          "expected: input-port?"
                          "given: 42")
                        (lambda ()
                          (read-line 42))))))

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

              (list "progress-evt?/one-arg"
                    (equal? (progress-evt? #f) #f))

              (list "progress-evt?/two-arg"
                    (let ([port (open-input-string "ok")])
                      (equal? (progress-evt? #f port) #f)))

              (list "port-provides-progress-evts?/fallback"
                    (and (equal? (port-provides-progress-evts?
                                  (open-input-string "ok"))
                                 #f)
                         (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                           (port-provides-progress-evts? 'not-a-port)
                           #f)))

              (list "port-writes-atomic?/fallback"
                    (and (equal? (port-writes-atomic? (open-output-string)) #f)
                         (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                           (port-writes-atomic? 'not-a-port)
                           #f)))

              (list "port-writes-special?/ordinary-ports"
                    (and (equal? (port-writes-special? (open-output-string)) #f)
                         (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                           (port-writes-special? 'not-a-port)
                           #f)))

              (list "prop:output-port predicates"
                    (let ()
                      (struct sink (port) #:property prop:output-port 0)
                      (define out (sink (open-output-string)))
                      (and (port? out)
                           (output-port? out)
                           (not (input-port? out))
                           (equal? (port-writes-special? out) #f))))

              (list "prop:output-port write-string"
                    (let ()
                      (struct sink (port) #:property prop:output-port 0)
                      (define under (open-output-string))
                      (define out (sink under))
                      (write-string "ok" out)
                      (and (equal? (get-output-string under) "ok")
                           (let-values ([(line col pos) (port-next-location out)])
                             (equal? (list line col pos) '(1 2 3))))))

              (list "prop:output-port handlers"
                    (let ()
                      (struct sink (port) #:property prop:output-port 0)
                      (define out (sink (open-output-string)))
                      (and (procedure? (port-write-handler out))
                           (procedure? (port-display-handler out))
                           (procedure? (port-print-handler out)))))

              (list "make-output-port/write-bytes"
                    (let ([seen #""])
                      (let ([port (make-output-port
                                   'sink
                                   always-evt
                                   (lambda (bs start end non-block? breakable?)
                                     (set! seen (subbytes bs start end))
                                     (- end start))
                                   void)])
                        (and (output-port? port)
                             (equal? (write-bytes #"abcdef" port 2 5) 3)
                             (equal? seen #"cde")
                             (equal? (file-position* port) 3)))))

              (list "printer boolean parameters"
                    (and (equal? (print-graph) #f)
                         (void? (print-graph #t))
                         (equal? (print-graph) #t)
                         (void? (print-struct #f))
                         (equal? (print-struct) #f)
                         (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                           (print-box 'yes)
                           #f)))

              (list "channel/basic"
                    (let ([ch (make-channel)])
                      (and (equal? (channel? ch) #t)
                           (equal? (channel-try-get ch) #f)
                           (equal? (channel-put ch 'ok) (void))
                           (equal? (channel-try-get ch) 'ok)
                           (equal? (channel-try-get ch) #f))))

              (list "channel/sync"
                    (let* ([ch (make-channel)]
                           [evt (channel-put-evt ch 'via-sync)])
                      (and (equal? (channel-put-evt? evt) #t)
                           (equal? (evt? ch) #t)
                           (equal? (evt? evt) #t)
                           (equal? (evt? 'not-an-event) #f)
                           (equal? (sync/timeout 0 ch) #f)
                           (equal? (sync/timeout 0 evt) evt)
                           (equal? (sync/timeout 0 ch) 'via-sync))))

              (list "handle-evt/basic"
                    (let* ([ch  (make-channel)]
                           [evt (handle-evt ch (lambda (v) (list 'handled v)))])
                      (and (equal? (handle-evt? evt) #t)
                           (equal? (handle-evt? ch) #f)
                           (equal? (evt? evt) #t)
                           (equal? (sync/timeout 0 evt) #f)
                           (equal? (channel-put ch 'ok) (void))
                           (equal? (sync/timeout 0 evt) '(handled ok)))))

              (list "logger/log-receiver"
                    (let* ([logger (make-logger 'goal1)]
                           [recv   (make-log-receiver logger 'info)])
                      (and (equal? (logger? logger) #t)
                           (equal? (logger-name logger) 'goal1)
                           (equal? (log-receiver? recv) #t)
                           (equal? (evt? recv) #t)
                           (equal? (sync/timeout 0 recv) #f)
                           (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                             (make-log-receiver 'not-a-logger 'info)
                             #f))))

              (list "security-guard/basic"
                    (let* ([old (current-security-guard)]
                           [g   (make-security-guard
                                 old
                                 (lambda (_who _path _modes) (void))
                                 (lambda (_who _host _port _mode) (void)))])
                      (and (equal? (security-guard? old) #t)
                           (equal? (security-guard? g) #t)
                           (equal? (security-guard? 'not-a-guard) #f)
                           (equal? (current-security-guard g) (void))
                           (eq? (current-security-guard) g)
                           (equal? (current-security-guard old) (void))
                           (eq? (current-security-guard) old)
                           (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                             (make-security-guard 'not-a-guard void void)
                             #f)
                           (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                             (current-security-guard 'not-a-guard)
                             #f))))

              (list "custodian/basic"
                    (let ([c (make-custodian)])
                      (and (equal? (custodian? c) #t)
                           (equal? (custodian-shut-down? c) #f)
                           (equal? (custodian-shutdown-all c) (void))
                           (equal? (custodian-shut-down? c) #t))))

              (list "custodian-box/basic"
                    (let* ([c (make-custodian)]
                           [b (make-custodian-box c 'live)])
                      (and (equal? (custodian-box? b) #t)
                           (equal? (custodian-box-value b) 'live)
                           (equal? (custodian-shutdown-all c) (void))
                           (equal? (custodian-box-value b) #f))))

              (list "date/basic"
                    (let ([d (date 6 7 8 9 10 2024 3 282 #f 0)])
                      (and (equal? (date? d) #t)
                           (equal? (date*? d) #f)
                           (equal? (date-second d) 6)
                           (equal? (date-minute d) 7)
                           (equal? (date-hour d) 8)
                           (equal? (date-day d) 9)
                           (equal? (date-month d) 10)
                           (equal? (date-year d) 2024)
                           (equal? (date-week-day d) 3)
                           (equal? (date-year-day d) 282)
                           (equal? (date-dst? d) #f)
                           (equal? (date-time-zone-offset d) 0))))

              (list "date*/basic"
                    (let* ([zone (string #\U #\T #\C)]
                           [d (date* 6 7 8 9 10 2024 3 282 #f 0 123 zone)])
                      (and (equal? (date? d) #t)
                           (equal? (date*? d) #t)
                           (equal? (date-second d) 6)
                           (equal? (date*-nanosecond d) 123)
                           (equal? (date*-time-zone-name d) "UTC")
                           (equal? (immutable-string? (date*-time-zone-name d)) #t))))

              (list "time/current-clock"
                    (and (equal? (exact-integer? (current-seconds)) #t)
                         (equal? (real? (current-inexact-milliseconds)) #t)
                         (equal? (inexact? (current-inexact-milliseconds)) #t)
                         (equal? (exact-integer? (current-milliseconds)) #t)
                         (equal? (exact-integer? (current-process-milliseconds)) #t)
                         (equal? (exact-integer? (current-gc-milliseconds)) #t)))

              (list "time/seconds->date-utc"
                    (let ([d (seconds->date 0 #f)])
                      (and (equal? (date? d) #t)
                           (equal? (date*? d) #t)
                           (equal? (date-second d) 0)
                           (equal? (date-minute d) 0)
                           (equal? (date-hour d) 0)
                           (equal? (date-day d) 1)
                           (equal? (date-month d) 1)
                           (equal? (date-year d) 1970)
                           (equal? (date-week-day d) 4)
                           (equal? (date-year-day d) 0)
                           (equal? (date-dst? d) #f)
                           (equal? (date-time-zone-offset d) 0)
                           (equal? (date*-nanosecond d) 0)
                           (equal? (date*-time-zone-name d) "UTC"))))

              (list "time/current-date"
                    (date*? (current-date)))

              (list "break-exn-structures"
                    (call-with-escape-continuation
                     (lambda (k)
                       (let* ([marks      '()]
                              [break      (exn:break "break" marks k)]
                              [made       (make-exn:break "made-break" marks k)]
                              [hang-up    (exn:break:hang-up "hang-up" marks k)]
                              [terminate  (make-exn:break:terminate "terminate" marks k)])
                         (and (equal? (exn? break) #t)
                              (equal? (exn:break? break) #t)
                              (equal? (exn:break? made) #t)
                              (equal? (exn:break? hang-up) #t)
                              (equal? (exn:break? terminate) #t)
                              (equal? (exn:break? 'not-a-break) #f)
                              (equal? (exn:break:hang-up? break) #f)
                              (equal? (exn:break:hang-up? hang-up) #t)
                              (equal? (exn:break:terminate? break) #f)
                              (equal? (exn:break:terminate? terminate) #t)
                              (equal? (exn:break-continuation break) k)
                              (equal? (exn-message hang-up) "hang-up")
                              (equal? (exn-message terminate) "terminate"))))))

              (list "semaphore/basic"
                    (let ([s (make-semaphore)])
                      (and (equal? (semaphore? s) #t)
                           (equal? (sync/timeout 0 s) #f)
                           (equal? (semaphore-try-wait? s) #f)
                           (equal? (semaphore-post s) (void))
                           (equal? (semaphore-try-wait? s) #t)
                           (equal? (semaphore-try-wait? s) #f)
                           (equal? (semaphore-post s) (void))
                           (equal? (sync/timeout 0 s) s)
                           (equal? (sync/timeout 0 s) #f))))

              (list "subprocess?/browser-stub"
                    (and (equal? (subprocess? 42) #f)
                         (equal? (subprocess? (current-thread)) #f)))

              (list "thread?/single-thread"
                    (let ([thd (current-thread)])
                      (and (equal? (thread? thd) #t)
                           (equal? (thread? 42) #f)
                           (equal? (thread-running? thd) #t)
                           (equal? (thread-dead? thd) #f))))

              (list "thread-group?/minimal"
                    (let* ([root (current-thread-group)]
                           [child (make-thread-group root)])
                      (and (equal? (thread-group? root) #t)
                           (equal? (thread-group? child) #t)
                           (equal? (thread-group? 42) #f)
                           (equal? (current-thread-group child) (void))
                           (eq? (current-thread-group) child)
                           (equal? (current-thread-group root) (void))
                           (eq? (current-thread-group) root))))

              (list "call-with-semaphore/basic"
                    (let ([s (make-semaphore 1)]
                          [seen #f])
                      (and (equal? (call-with-semaphore s
                                     (lambda (x)
                                       (set! seen x)
                                       (+ x 1))
                                     #f
                                     41)
                                   42)
                           (equal? seen 41)
                           (equal? (sync/timeout 0 s) s))))

              (list "semaphore-peek-evt/basic"
                    (let* ([s (make-semaphore)]
                           [e (semaphore-peek-evt s)])
                      (and (equal? (semaphore-peek-evt? e) #t)
                           (equal? (sync/timeout 0 e) #f)
                           (equal? (semaphore-post s) (void))
                           (equal? (sync/timeout 0 e) e)
                           (equal? (sync/timeout 0 e) e)
                           (equal? (sync/timeout 0 s) s)
                           (equal? (sync/timeout 0 e) #f))))

              (list "will-executor?/inert"
                    (let ([we (make-will-executor)])
                      (and (equal? (will-executor? we) #t)
                           (equal? (will-executor? 42) #f)
                           (equal? (evt? we) #t)
                           (equal? (sync/timeout 0 we) #f)
                           (equal? (will-register we 'value (lambda (v) v)) (void))
                           (equal? (will-try-execute we) #f)
                           (equal? (will-try-execute we 'none) 'none)
                           (equal? (will-execute we) #f))))

              (list "always-never-evt/basic"
                    (and (equal? (sync/timeout 0 always-evt) always-evt)
                         (equal? (sync/timeout 0 never-evt) #f)
                         (equal? (sync always-evt) always-evt)))

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
              (list "read-line mode argument errors are catchable with who"
                    (let ([catch-contract/who+expected+given?
                           (lambda (who expected given thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (string-contains? message who)
                                    (string-contains? message expected)
                                    (string-contains? message given))))])
                      (and
                       (catch-contract/who+expected+given?
                        "read-line"
                        "(or/c 'linefeed 'return 'return-linefeed 'any 'any-one)"
                        "'bogus"
                        (lambda () (read-line (open-input-string "x") 'bogus)))
                       (catch-contract/who+expected+given?
                        "read-line"
                        "(or/c 'linefeed 'return 'return-linefeed 'any 'any-one)"
                        "#f"
                        (lambda () (read-line (open-input-string "x") #f)))
                       (catch-contract/who+expected+given?
                        "read-bytes-line"
                        "(or/c 'linefeed 'return 'return-linefeed 'any 'any-one)"
                        "'bogus"
                        (lambda () (read-bytes-line (open-input-string "x") 'bogus))))))

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

              (list "single-value peek argument errors are catchable with who"
                    (let ([catch-contract/message-contains?
                           (lambda (parts thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (andmap (lambda (part)
                                              (string-contains? message part))
                                            parts))))])
                      (and
                       (catch-contract/message-contains?
                        '("peek-byte: contract violation"
                          "expected: exact-nonnegative-integer?"
                          "given: #f")
                        (lambda ()
                          (peek-byte (open-input-bytes #"abc") #f)))
                       (catch-contract/message-contains?
                        '("peek-byte: contract violation"
                          "expected: exact-nonnegative-integer?"
                          "given: -1")
                        (lambda ()
                          (peek-byte (open-input-bytes #"abc") -1)))
                       (catch-contract/message-contains?
                        '("peek-byte: contract violation"
                          "expected: input-port?"
                          "given: 42")
                        (lambda ()
                          (peek-byte 42)))
                       (catch-contract/message-contains?
                        '("peek-char: contract violation"
                          "expected: exact-nonnegative-integer?"
                          "given: -1")
                        (lambda ()
                          (peek-char (open-input-string "abc") -1)))
                       (catch-contract/message-contains?
                        '("peek-char: contract violation"
                          "expected: exact-nonnegative-integer?"
                          "given: #f")
                        (lambda ()
                          (peek-char (open-input-string "abc") #f)))
                       (catch-contract/message-contains?
                        '("peek-char: contract violation"
                          "expected: input-port?"
                          "given: 42")
                        (lambda ()
                          (peek-char 42))))))

              (list "peek-byte-or-special/basic"
                    (let* ([port     (open-input-bytes (bytes 65 66))]
                           [peeked   (peek-byte-or-special port)]
                           [skipped  (peek-byte-or-special port 1)]
                           [consumed (read-byte port)])
                      (and (equal? peeked 65)
                           (equal? skipped 66)
                           (equal? consumed 65))))

              (list "peek-char-or-special/basic"
                    (let* ([port     (open-input-string "λx")]
                           [peeked   (peek-char-or-special port)]
                           [skipped  (peek-char-or-special port 1)]
                           [consumed (read-char port)])
                      (and (char=? peeked #\λ)
                           (char=? skipped #\x)
                           (char=? consumed #\λ))))

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

              (list "peek-bytes-avail!/tail-position"
                    (let ([peek-count
                           (lambda ()
                             (let* ([buffer (make-bytes 4 0)]
                                    [port   (open-input-bytes (bytes 1 2 3))])
                               (peek-bytes-avail! buffer 0 #f port 0 3)))])
                      (equal? (peek-count) 3)))

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

              (list "peek buffer wrappers accept immutable destinations"
                    (let* ([b1 (bytes->immutable-bytes (bytes 95 95 95))]
                           [p1 (open-input-bytes (bytes 65 66 67))]
                           [c1 (peek-bytes! b1 0 p1 0 3)]
                           [r1 (read-bytes 3 p1)]
                           [b2 (bytes->immutable-bytes (bytes 95 95 95))]
                           [p2 (open-input-bytes (bytes 68 69 70))]
                           [c2 (peek-bytes-avail! b2 0 #f p2 0 3)]
                           [r2 (read-bytes 3 p2)]
                           [b3 (bytes->immutable-bytes (bytes 95 95 95))]
                           [p3 (open-input-bytes (bytes 71 72 73))]
                           [c3 (peek-bytes-avail!* b3 0 #f p3 0 3)]
                           [r3 (read-bytes 3 p3)]
                           [s  (string->immutable-string (string #\_ #\_ #\_))]
                           [ps (open-input-string "abc")]
                           [cs (peek-string! s 0 ps 0 3)]
                           [rs (read-string 3 ps)])
                      (and (equal? c1 3)
                           (equal? b1 (bytes 65 66 67))
                           (equal? r1 (bytes 65 66 67))
                           (equal? c2 3)
                           (equal? b2 (bytes 68 69 70))
                           (equal? r2 (bytes 68 69 70))
                           (equal? c3 3)
                           (equal? b3 (bytes 71 72 73))
                           (equal? r3 (bytes 71 72 73))
                           (equal? cs 3)
                           (equal? s "abc")
                           (equal? rs "abc"))))

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

              (list "peek wrapper argument errors are catchable with who"
                    (let ([catch-contract/message-contains?
                           (lambda (parts thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (andmap (lambda (part)
                                              (string-contains? message part))
                                            parts))))])
                      (and
                       (catch-contract/message-contains?
                        '("peek-bytes: contract violation"
                          "expected: exact-nonnegative-integer?"
                          "given: #f")
                        (lambda ()
                          (peek-bytes #f 0 (open-input-bytes #"abc"))))
                       (catch-contract/message-contains?
                        '("peek-bytes: contract violation"
                          "expected: exact-nonnegative-integer?"
                          "given: -1")
                        (lambda ()
                          (peek-bytes 1 -1 (open-input-bytes #"abc"))))
                       (catch-contract/message-contains?
                        '("peek-bytes: contract violation"
                          "expected: input-port?"
                          "given: 42")
                        (lambda ()
                          (peek-bytes 1 0 42)))
                       (catch-contract/message-contains?
                        '("peek-string: contract violation"
                          "expected: exact-nonnegative-integer?"
                          "given: -1")
                        (lambda ()
                          (peek-string -1 0 (open-input-string "abc"))))
                       (catch-contract/message-contains?
                        '("peek-string: contract violation"
                          "expected: exact-nonnegative-integer?"
                          "given: #f")
                        (lambda ()
                          (peek-string 1 #f (open-input-string "abc"))))
                       (catch-contract/message-contains?
                        '("peek-string: contract violation"
                          "expected: input-port?"
                          "given: 42")
                        (lambda ()
                          (peek-string 1 0 42)))
                       (catch-contract/message-contains?
                        '("peek-bytes!: contract violation"
                          "expected: input-port?"
                          "given: 42")
                        (lambda ()
                          (peek-bytes! (make-bytes 3 0) 0 42)))
                       (catch-contract/message-contains?
                        '("peek-bytes!: contract violation"
                          "expected: bytes?"
                          "given: 42")
                        (lambda ()
                          (peek-bytes! 42 0 (open-input-bytes #"abc"))))
                       (catch-contract/message-contains?
                        '("peek-bytes-avail!: contract violation"
                          "expected: input-port?"
                          "given: 42")
                        (lambda ()
                          (peek-bytes-avail! (make-bytes 3 0) 0 #f 42)))
                       (catch-contract/message-contains?
                        '("peek-bytes-avail!: contract violation"
                          "expected: bytes?"
                          "given: 42")
                        (lambda ()
                          (peek-bytes-avail! 42 0 #f (open-input-bytes #"abc"))))
                       (catch-contract/message-contains?
                        '("peek-bytes-avail!*: contract violation"
                          "expected: input-port?"
                          "given: 42")
                        (lambda ()
                          (peek-bytes-avail!* (make-bytes 3 0) 0 #f 42)))
                       (catch-contract/message-contains?
                        '("peek-bytes-avail!*: contract violation"
                          "expected: bytes?"
                          "given: 42")
                        (lambda ()
                          (peek-bytes-avail!* 42 0 #f (open-input-bytes #"abc"))))
                       (catch-contract/message-contains?
                        '("peek-string!: contract violation"
                          "expected: input-port?"
                          "given: 42")
                        (lambda ()
                          (peek-string! (make-string 3 #\x) 0 42)))
                       (catch-contract/message-contains?
                        '("peek-string!: contract violation"
                          "expected: string?"
                          "given: 42")
                        (lambda ()
                          (peek-string! 42 0 (open-input-string "abc")))))))
              
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

              (list "write-byte and write-char argument errors are catchable with who"
                    (let ([catch-contract/message-contains?
                           (lambda (parts thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (andmap (lambda (part)
                                              (string-contains? message part))
                                            parts))))])
                      (and
                       (catch-contract/message-contains?
                        '("write-byte: contract violation"
                          "expected: byte?"
                          "given: 'bad")
                        (lambda ()
                          (write-byte 'bad (open-output-bytes))))
                       (catch-contract/message-contains?
                        '("write-byte: contract violation"
                          "expected: byte?"
                          "given: -1")
                        (lambda ()
                          (write-byte -1 (open-output-bytes))))
                       (catch-contract/message-contains?
                        '("write-byte: contract violation"
                          "expected: byte?"
                          "given: 256")
                        (lambda ()
                          (write-byte 256 (open-output-bytes))))
                       (catch-contract/message-contains?
                        '("write-byte: contract violation"
                          "expected: output-port?"
                          "given: 42")
                        (lambda ()
                          (write-byte 65 42)))
                       (catch-contract/message-contains?
                        '("write-char: contract violation"
                          "expected: char?"
                          "given: 65")
                        (lambda ()
                          (write-char 65 (open-output-string))))
                       (catch-contract/message-contains?
                        '("write-char: contract violation"
                          "expected: output-port?"
                          "given: 42")
                        (lambda ()
                          (write-char #\A 42))))))

              (list "output helper argument errors are catchable with who"
                    (let ([catch-contract/message-contains?
                           (lambda (parts thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (andmap (lambda (part)
                                              (string-contains? message part))
                                            parts))))])
                      (and
                       (catch-contract/message-contains?
                        '("newline: contract violation"
                          "expected: output-port?"
                          "given: 42")
                        (lambda ()
                          (newline 42)))
                       (catch-contract/message-contains?
                        '("display: contract violation"
                          "expected: output-port?"
                          "given: 42")
                        (lambda ()
                          (display "x" 42)))
                       (catch-contract/message-contains?
                        '("displayln: contract violation"
                          "expected: output-port?"
                          "given: 42")
                        (lambda ()
                          (displayln "x" 42))))))

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

              (list "get-output-bytes reset and slice"
                    (let ([port (open-output-bytes)])
                      (and (equal? (write-bytes (bytes 65 66 67 68) port) 4)
                           (equal? (get-output-bytes port #f 1 3) (bytes 66 67))
                           (equal? (get-output-bytes port #f 1 #f) (bytes 66 67 68))
                           (equal? (get-output-bytes port #t) (bytes 65 66 67 68))
                           (equal? (get-output-bytes port) (bytes)))))

              (list "get-output-bytes errors are catchable with who"
                    (let ([catch-message-contains?
                           (lambda (parts thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (andmap (lambda (part)
                                              (string-contains? message part))
                                            parts))))])
                      (and
                       (catch-message-contains?
                        '("get-output-bytes"
                          "(and/c output-port? string-port?)"
                          "given: 42")
                        (lambda () (get-output-bytes 42)))
                       (catch-message-contains?
                        '("get-output-bytes"
                          "exact-nonnegative-integer?"
                          "given: -1")
                        (lambda ()
                          (get-output-bytes (open-output-bytes) #f -1)))
                       (catch-message-contains?
                        '("get-output-bytes"
                          "(or/c exact-nonnegative-integer? #f)"
                          "given: -1")
                        (lambda ()
                          (get-output-bytes (open-output-bytes) #f 0 -1)))
                       (catch-message-contains?
                        '("get-output-bytes"
                          "starting index is out of range"
                          "starting index: 4"
                          "valid range: [0, 3]"
                          "port content: #<output-port:sink>")
                        (lambda ()
                          (let ([port (open-output-bytes 'sink)])
                            (write-bytes #"abc" port)
                            (get-output-bytes port #f 4))))
                       (catch-message-contains?
                        '("get-output-bytes"
                          "ending index is out of range"
                          "ending index: 4"
                          "valid range: [1, 3]"
                          "port content: #<output-port:string>")
                        (lambda ()
                          (let ([port (open-output-bytes)])
                            (write-bytes #"abc" port)
                            (get-output-bytes port #f 1 4))))
                       (catch-message-contains?
                        '("get-output-bytes"
                          "ending index is out of range"
                          "ending index: 1"
                          "valid range: [2, 3]"
                          "port content: #<output-port:string>")
                        (lambda ()
                          (let ([port (open-output-bytes)])
                            (write-bytes #"abc" port)
                            (get-output-bytes port #f 2 1)))))))

              (list "newline"
                    (let ([port (open-output-string)])
                      (and (void? (newline port))
                           (equal? (get-output-string port) "\n"))))

              (list "flush-output/string-port"
                    (let ([port (open-output-string)])
                      (and (equal? (write-string "abc" port) 3)
                           (void? (flush-output port))
                           (equal? (get-output-string port) "abc")
                           (void? (close-output-port port))
                           (with-handlers ([exn:fail? (lambda (_ex) #t)])
                             (flush-output port)
                             #f))))

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

              (list "byte port range errors are catchable with who"
                    (let ([catch-range/who+reason+index?
                           (lambda (who reason index-line value-line thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (string-contains? message who)
                                    (string-contains? message reason)
                                    (string-contains? message index-line)
                                    (string-contains? message "valid range: [0, 3]")
                                    (string-contains? message value-line))))]
                          [catch-contract/who+expected+given?
                           (lambda (who expected given thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (string-contains? message who)
                                    (string-contains? message expected)
                                    (string-contains? message given))))])
                      (and
                       (catch-range/who+reason+index?
                        "write-bytes"
                        "starting index is out of range"
                        "starting index: 4"
                        "byte string: #\"abc\""
                        (lambda () (write-bytes #"abc" (open-output-bytes) 4)))
                       (catch-range/who+reason+index?
                        "read-bytes-avail!"
                        "ending index is out of range"
                        "ending index: 4"
                        "byte string:"
                        (lambda ()
                          (read-bytes-avail! (make-bytes 3 0)
                                             (open-input-bytes #"abc")
                                             1
                                             4)))
                       (catch-range/who+reason+index?
                        "peek-bytes-avail!"
                        "ending index is out of range"
                        "ending index: 4"
                        "byte string:"
                        (lambda ()
                          (peek-bytes-avail! (make-bytes 3 0)
                                             0
                                             #f
                                             (open-input-bytes #"abc")
                                             1
                                             4)))
                       (catch-contract/who+expected+given?
                        "write-bytes"
                        "exact-nonnegative-integer?"
                        "given: -1"
                        (lambda () (write-bytes #"abc" (open-output-bytes) -1)))
                       (catch-contract/who+expected+given?
                        "write-bytes"
                        "bytes?"
                        "given: 'bad"
                        (lambda () (write-bytes 'bad (open-output-bytes))))
                       (catch-contract/who+expected+given?
                        "write-bytes"
                        "output-port?"
                        "given: 42"
                        (lambda () (write-bytes #"abc" 42)))
                       (catch-contract/who+expected+given?
                        "read-bytes-avail!*"
                        "exact-nonnegative-integer?"
                        "given: -1"
                        (lambda ()
                          (read-bytes-avail!* (make-bytes 3 0)
                                              (open-input-bytes #"abc")
                                              -1)))
                       (catch-contract/who+expected+given?
                        "peek-bytes-avail!*"
                        "exact-nonnegative-integer?"
                        "given: -1"
                        (lambda ()
                          (peek-bytes-avail!* (make-bytes 3 0)
                                              -1
                                              #f
                                              (open-input-bytes #"abc")
                                              0
                                              3))))))

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

              (list "string port range errors are catchable with who"
                    (let ([catch-range/who+reason+index?
                           (lambda (who reason index-line value-line thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (string-contains? message who)
                                    (string-contains? message reason)
                                    (string-contains? message index-line)
                                    (string-contains? message "valid range: [0, 3]")
                                    (string-contains? message value-line))))]
                          [catch-contract/who+expected+given?
                           (lambda (who expected given thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (string-contains? message who)
                                    (string-contains? message expected)
                                    (string-contains? message given))))])
                      (and
                       (catch-range/who+reason+index?
                        "write-string"
                        "starting index is out of range"
                        "starting index: 4"
                        "byte string: \"abc\""
                        (lambda () (write-string "abc" (open-output-string) 4)))
                       (catch-range/who+reason+index?
                        "read-string!"
                        "ending index is smaller than starting index"
                        "ending index: 1"
                        "byte string: \"xxx\""
                        (lambda ()
                          (read-string! (make-string 3 #\x)
                                        (open-input-string "abc")
                                        2
                                        1)))
                       (catch-range/who+reason+index?
                        "peek-string!"
                        "ending index is out of range"
                        "ending index: 4"
                        "byte string: \"xxx\""
                        (lambda ()
                          (peek-string! (make-string 3 #\x)
                                        0
                                        (open-input-string "abc")
                                        1
                                        4)))
                       (catch-contract/who+expected+given?
                        "write-string"
                        "exact-nonnegative-integer?"
                        "given: -1"
                        (lambda () (write-string "abc" (open-output-string) -1)))
                       (catch-contract/who+expected+given?
                        "write-string"
                        "string?"
                        "given: 'bad"
                        (lambda () (write-string 'bad (open-output-string))))
                       (catch-contract/who+expected+given?
                        "write-string"
                        "output-port?"
                        "given: 42"
                        (lambda () (write-string "abc" 42)))
                       (catch-contract/who+expected+given?
                        "read-string!"
                        "exact-nonnegative-integer?"
                        "given: -1"
                        (lambda ()
                          (read-string! (make-string 3 #\x)
                                        (open-input-string "abc")
                                        0
                                        -1)))
                       (catch-contract/who+expected+given?
                        "peek-string!"
                        "exact-nonnegative-integer?"
                        "given: -1"
                        (lambda ()
                          (peek-string! (make-string 3 #\x)
                                        -1
                                        (open-input-string "abc")
                                        0
                                        3))))))

              (list "fprintf/bootstrap"
                    (let ([port (open-output-string)])
                      (and (void? (fprintf port "value=~a symbol=~s~n" 42 'ok))
                           (equal? (get-output-string port)
                                   "value=42 symbol=ok\n"))))
              ))


       
       (list "13.5 Writing"
             (list
              (list "current-print/default"
                    (procedure? (current-print)))
              (list "print-values/current-print"
                    (let ([seen '()]
                          [old (current-print)])
                      (dynamic-wind
                       (lambda () (current-print (lambda (v) (set! seen (cons v seen)))))
                       (lambda ()
                         (and (equal? (call-with-values
                                       (lambda ()
                                         (call-with-values
                                          (lambda () (values 'a 'b))
                                          print-values))
                                       list)
                                      '(a b))
                              (equal? seen '(b a))))
                       (lambda () (current-print old)))))
              (list "print-syntax-width parameter"
                    (let ([original (print-syntax-width)])
                      (and (equal? original 256)
                           (void? (print-syntax-width 0))
                           (equal? (print-syntax-width) 0)
                           (void? (print-syntax-width 3))
                           (equal? (print-syntax-width) 3)
                           (void? (print-syntax-width original)))))
              (list "error-print-width parameter"
                    (let ([original (error-print-width)])
                      (and (equal? original 256)
                           (void? (error-print-width 3))
                           (equal? (error-print-width) 3)
                           (void? (error-print-width original)))))
              (list "error-print-source-location parameter"
                    (let ([original (error-print-source-location)])
                      (and (equal? original #t)
                           (void? (error-print-source-location #f))
                           (equal? (error-print-source-location) #f)
                           (void? (error-print-source-location original)))))
              (list "error-syntax->name-handler parameter"
                    (let ([default-handler (error-syntax->name-handler)])
                      (and (procedure? default-handler)
                           (equal? (default-handler 'not-syntax) #f)
                           (equal? (error-syntax->name-handler
                                    (lambda (stx) 'demo-form))
                                   (void))
                           (equal? ((error-syntax->name-handler) 'ignored)
                                   'demo-form))))
              (list "error string handler parameters"
                    (let ([value-default  (error-value->string-handler)]
                          [syntax-default (error-syntax->string-handler)]
                          [module-default (error-module-path->string-handler)])
                      (and (procedure? value-default)
                           (procedure? syntax-default)
                           (procedure? module-default)
                           (string? (value-default 'value 20))
                           (string? (syntax-default '(lambda (x) x) #f))
                           (string? (module-default ''racket/base 20))
                           (equal? (error-value->string-handler
                                    (lambda (v len) "value!"))
                                   (void))
                           (equal? ((error-value->string-handler) 'ignored 10)
                                   "value!")
                           (equal? (error-syntax->string-handler
                                    (lambda (v len) "syntax!"))
                                   (void))
                           (equal? ((error-syntax->string-handler) 'ignored #f)
                                   "syntax!")
                           (equal? (error-module-path->string-handler
                                    (lambda (v len) "module!"))
                                   (void))
                           (equal? ((error-module-path->string-handler) 'ignored 10)
                                   "module!"))))
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
                           (equal? write-result "<data>")
                           (equal? (custom-write-accessor 'plain 'fallback)
                                   'fallback)
                           (with-handlers ([exn:fail:contract? (lambda (_exn) #t)])
                             (custom-write-accessor 'plain)
                             #f))))))

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
                           (let-values ([(line col pos) (port-next-location 42)])
                             (equal? (list line col pos) '(#f #f #f))))))))
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
                       [loc3  (make-srcloc (bytes->path #"foo") 3 0 10 5)]
                       [original-user (current-directory-for-user)])
                  (current-directory-for-user "/tmp/")
                  (let ([result
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
                              (equal? (srcloc->string loc2) "file")
                              (equal? (srcloc->string
                                       (make-srcloc (string->path "/tmp/project/main.rkt") 9 1 #f #f))
                                      "project/main.rkt:9:1")
                              (equal? (srcloc->string
                                       (make-srcloc (string->path "/app/project/main.rkt") 9 1 #f #f))
                                      "/app/project/main.rkt:9:1"))])
                    (current-directory-for-user original-user)
                    result))))

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

        (list "raise-argument-error"
              (let ([plain
                     (with-handlers ([exn:fail:contract?
                                      (lambda (exn)
                                        (and (exn:fail:contract? exn)
                                             (string? (exn-message exn))))])
                       (raise-argument-error 'demo "string?" 42)
                       #f)]
                    [indexed
                     (with-handlers ([exn:fail:contract?
                                      (lambda (exn)
                                        (and (exn:fail:contract? exn)
                                             (string? (exn-message exn))))])
                       (raise-argument-error 'demo "string?" 0 42 'extra)
                       #f)])
                (and plain indexed)))

        (list "raise-arguments-error"
              (with-handlers ([exn:fail:contract?
                               (lambda (exn)
                                 (and (exn:fail:contract? exn)
                                      (string? (exn-message exn))))])
                (raise-arguments-error 'demo "bad pair" "left" 1 "right" 2)
                #f))
        
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
              (let* ([basic
                      (with-handlers ([exn:fail:read:eof?
                                       (λ (exn)
                                         (list (exn-message exn)
                                               (map srcloc->string
                                                    (exn:fail:read-srclocs exn))))])
                        (raise-read-eof-error "EOF" 'src 5 6 7 8))]
                     [with-extra
                      (with-handlers ([exn:fail:read:eof?
                                       (λ (exn)
                                         (map srcloc-source
                                              (exn:fail:read-srclocs exn)))])
                        (raise-read-eof-error
                         "extra"
                         'src 1 2 3 4
                         (list (make-srcloc 'extra 9 8 7 6))))])
                (and (equal? basic (list "EOF" '("src:5:6")))
                     (equal? with-extra '(src extra)))))

        (list "result arity helper errors"
              (let ([binding
                     (with-handlers ([exn:fail:contract:arity?
                                      (λ (exn)
                                        (list 'binding (exn:fail:contract:arity? exn)))])
                       (raise-binding-result-arity-error 1 '(a b))
                       'missed)]
                    [definition
                     (with-handlers ([exn:fail:contract:arity?
                                      (λ (exn)
                                        (list 'definition (exn:fail:contract:arity? exn)))])
                       (raise-definition-result-arity-error '(x y) '(a))
                       'missed)])
                (and (equal? binding '(binding #t))
                     (equal? definition '(definition #t)))))
        
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
                     [continuation  (exn:fail:contract:continuation "continuation" marks)]
                     [continuation-make (make-exn:fail:contract:continuation "made-continuation" marks)]
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
                     (equal? (exn:fail:contract? continuation) #t)
                     (equal? (exn:fail:contract? continuation-make) #t)
                     (equal? (exn:fail:contract? divide-make) #t)
                     (equal? (exn:fail:contract? non-fixnum-make) #t)
                     (equal? (exn:fail:contract? variable-make) #t)
                     (equal? (exn:fail:contract:arity? contract) #f)
                     (equal? (exn:fail:contract:arity? arity) #t)
                     (equal? (exn:fail:contract:arity? arity-make) #t)
                     (equal? (exn:fail:contract:continuation? continuation) #t)
                     (equal? (exn:fail:contract:continuation? continuation-make) #t)
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
                     (equal? (exn-message continuation) "continuation")
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
                     (equal? (exn:srclocs? read) #t)
                     (equal? (exn:srclocs? eof) #t)
                     (equal? (exn:srclocs? non-char) #t)
                     (equal? ((exn:srclocs-accessor read) read) locs)
                     (equal? (exn:fail:read:eof? eof) #t)
                     (equal? (exn:fail:read:eof? eof-make) #t)
                     (equal? (exn:fail:read:non-char? non-char) #t)
                     (equal? (exn:fail:read:non-char? non-char-make) #t)
                     (equal? (exn-message non-char) "non")
                     (equal? (exn-message eof) "eof"))))

        (list "prop:exn:srclocs property"
              (let ([locs (list (srcloc 'custom 1 0 1 1))])
                (struct custom-exn exn (srclocs)
                  #:property prop:exn:srclocs
                  (lambda (e) (custom-exn-srclocs e)))
                (let ([ex (custom-exn "custom" '() locs)])
                  (and (struct-type-property? prop:exn:srclocs)
                       (equal? (exn:srclocs? ex) #t)
                       (equal? ((exn:srclocs-accessor ex) ex) locs)
                       (equal? (exn:srclocs? (exn "plain" '())) #f)))))

        (list "exn:fail:filesystem structures"
              (let* ([marks           '()]
                     [filesystem      (exn:fail:filesystem "filesystem" marks)]
                     [filesystem-make (make-exn:fail:filesystem "made-filesystem" marks)]
                     [exists          (exn:fail:filesystem:exists "exists" marks)]
                     [exists-make     (make-exn:fail:filesystem:exists "made-exists" marks)]
                     [version         (exn:fail:filesystem:version "version" marks)]
                     [version-make    (make-exn:fail:filesystem:version "made-version" marks)]
                     [errno           (exn:fail:filesystem:errno "errno" marks (cons 2 'posix))]
                     [errno-make      (make-exn:fail:filesystem:errno "made-errno" marks (cons 3 'windows))])
                (and (equal? (exn:fail? filesystem) #t)
                     (equal? (exn:fail? filesystem-make) #t)
                     (equal? (exn:fail:filesystem? filesystem) #t)
                     (equal? (exn:fail:filesystem? filesystem-make) #t)
                     (equal? (exn:fail:filesystem? exists) #t)
                     (equal? (exn:fail:filesystem? exists-make) #t)
                     (equal? (exn:fail:filesystem? version) #t)
                     (equal? (exn:fail:filesystem? version-make) #t)
                     (equal? (exn:fail:filesystem? errno) #t)
                     (equal? (exn:fail:filesystem? errno-make) #t)
                     (equal? (exn:fail:filesystem:exists? filesystem) #f)
                     (equal? (exn:fail:filesystem:exists? exists) #t)
                     (equal? (exn:fail:filesystem:exists? exists-make) #t)
                     (equal? (exn:fail:filesystem:version? filesystem) #f)
                     (equal? (exn:fail:filesystem:version? version) #t)
                     (equal? (exn:fail:filesystem:version? version-make) #t)
                     (equal? (exn:fail:filesystem:errno? filesystem) #f)
                     (equal? (exn:fail:filesystem:errno? errno) #t)
                     (equal? (exn:fail:filesystem:errno? errno-make) #t)
                     (equal? (exn:fail:filesystem:errno-errno errno) (cons 2 'posix))
                     (equal? (exn:fail:filesystem:errno-errno errno-make) (cons 3 'windows))
                     (equal? (exn:fail:filesystem? (exn:fail "fail" marks)) #f)
                     (equal? (exn-message filesystem) "filesystem")
                     (equal? (exn-message filesystem-make) "made-filesystem")
                     (equal? (exn-message exists) "exists")
                     (equal? (exn-message version) "version")
                     (equal? (exn-message errno) "errno"))))

        (list "exn:fail:network structures"
              (let* ([marks        '()]
                     [network      (exn:fail:network "network" marks)]
                     [network-make (make-exn:fail:network "made-network" marks)]
                     [errno        (exn:fail:network:errno "errno" marks (cons 61 'posix))]
                     [errno-make   (make-exn:fail:network:errno "made-errno" marks (cons 8 'gai))])
                (and (equal? (exn:fail? network) #t)
                     (equal? (exn:fail? network-make) #t)
                     (equal? (exn:fail:network? network) #t)
                     (equal? (exn:fail:network? network-make) #t)
                     (equal? (exn:fail:network? errno) #t)
                     (equal? (exn:fail:network? errno-make) #t)
                     (equal? (exn:fail:network? (exn:fail "fail" marks)) #f)
                     (equal? (exn:fail:network:errno? network) #f)
                     (equal? (exn:fail:network:errno? errno) #t)
                     (equal? (exn:fail:network:errno? errno-make) #t)
                     (equal? (exn:fail:network:errno-errno errno) (cons 61 'posix))
                     (equal? (exn:fail:network:errno-errno errno-make) (cons 8 'gai))
                     (equal? (exn-message network) "network")
                     (equal? (exn-message network-make) "made-network")
                     (equal? (exn-message errno) "errno"))))

        (list "exn:fail:out-of-memory structure"
              (let* ([marks '()]
                     [oom   (exn:fail:out-of-memory "oom" marks)]
                     [made  (make-exn:fail:out-of-memory "made-oom" marks)])
                (and (equal? (exn:fail? oom) #t)
                     (equal? (exn:fail? made) #t)
                     (equal? (exn:fail:out-of-memory? oom) #t)
                     (equal? (exn:fail:out-of-memory? made) #t)
                     (equal? (exn:fail:out-of-memory? (exn:fail "fail" marks)) #f)
                     (equal? (exn-message oom) "oom")
                     (equal? (exn-message made) "made-oom"))))

        (list "exn:fail:unsupported structure"
              (let* ([marks       '()]
                     [unsupported (exn:fail:unsupported "unsupported" marks)]
                     [made        (make-exn:fail:unsupported "made-unsupported" marks)])
                (and (equal? (exn:fail? unsupported) #t)
                     (equal? (exn:fail? made) #t)
                     (equal? (exn:fail:unsupported? unsupported) #t)
                     (equal? (exn:fail:unsupported? made) #t)
                     (equal? (exn:fail:unsupported? (exn:fail "fail" marks)) #f)
                     (equal? (exn-message unsupported) "unsupported")
                     (equal? (exn-message made) "made-unsupported"))))

        (list "exn:fail:user structure"
              (let* ([marks '()]
                     [user  (exn:fail:user "user" marks)]
                     [made  (make-exn:fail:user "made-user" marks)])
                (and (equal? (exn:fail? user) #t)
                     (equal? (exn:fail? made) #t)
                     (equal? (exn:fail:user? user) #t)
                     (equal? (exn:fail:user? made) #t)
                     (equal? (exn:fail:user? (exn:fail "fail" marks)) #f)
                     (equal? (exn-message user) "user")
                     (equal? (exn-message made) "made-user"))))

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

 (list "11. Concurrency and Parallelism"
       (list "11.3 Thread Cells"
             (list
              (list "thread-cell basics"
                    (let ([cell (make-thread-cell 'initial)])
                      (and (equal? (thread-cell? cell) #t)
                           (equal? (thread-cell? 'initial) #f)
                           (equal? (thread-cell-ref cell) 'initial)
                           (void? (thread-cell-set! cell 'updated))
                           (equal? (thread-cell-ref cell) 'updated))))
              (list "thread-cell preserved argument"
                    (let ([cell (make-thread-cell 10 #t)])
                      (and (equal? (thread-cell? cell) #t)
                           (equal? (thread-cell-ref cell) 10))))
              (list "thread-cell argument errors are catchable with who"
                    (let ([catch-contract/who+expected+given?
                           (lambda (who expected given thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (string-contains? message who)
                                    (string-contains? message expected)
                                    (string-contains? message given))))])
                      (and
                       (catch-contract/who+expected+given?
                        "thread-cell-ref"
                        "thread-cell?"
                        "#f"
                        (lambda () (thread-cell-ref #f)))
                       (catch-contract/who+expected+given?
                        "thread-cell-set!"
                        "thread-cell?"
                        "#f"
                        (lambda () (thread-cell-set! #f 'value))))))
              (list "uninterruptible lock basics"
                    (let ([lock (unsafe-make-uninterruptible-lock)])
                      (and (void? (unsafe-uninterruptible-lock-acquire lock))
                           (void? (unsafe-uninterruptible-lock-release lock)))))
              (list "uninterruptible lock argument errors are catchable with who"
                    (let ([catch-contract/who+expected+given?
                           (lambda (who expected given thunk)
                             (let ([message
                                    (with-handlers ([exn:fail:contract?
                                                     (lambda (exn)
                                                       (exn-message exn))])
                                      (thunk)
                                      #f)])
                               (and message
                                    (string-contains? message who)
                                    (string-contains? message expected)
                                    (string-contains? message given))))])
                      (and
                       (catch-contract/who+expected+given?
                        "unsafe-uninterruptible-lock-acquire"
                        "uninterruptible-lock?"
                        "42"
                        (lambda () (unsafe-uninterruptible-lock-acquire 42)))
                       (catch-contract/who+expected+given?
                        "unsafe-uninterruptible-lock-release"
                        "uninterruptible-lock?"
                        "42"
                        (lambda () (unsafe-uninterruptible-lock-release 42)))))))))
 

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
                          (equal? (syntax-source-module stx) #f)
                          (equal? (syntax-line stx)         3)
                          (equal? (syntax-column stx)       0)
                          (equal? (syntax-position stx)    10)
                          (equal? (syntax-span stx)         5)
                          (equal? (syntax-source no-loc)   #f)
                          (equal? (syntax-line no-loc)     #f)
                          (equal? (syntax-column no-loc)   #f)
                          (equal? (syntax-position no-loc) #f)
                          (equal? (syntax-span no-loc)     #f))))
             (list "syntax properties"
                   (let* ([stx      (datum->syntax #f 'demo)]
                          [with-key (syntax-property stx 'color 'blue)])
                     (and (equal? (syntax-property stx 'color) #f)
                          (equal? (syntax-property with-key 'color) 'blue)
                          (equal? (syntax-property-symbol-keys with-key) '(color)))))
             (list "primitive-table/#%kernel syntax primitives"
                   (let* ([kernel             (primitive-table '#%kernel)]
                          [stx                (datum->syntax #f 'demo (make-srcloc 'src 3 0 10 5))]
                          [stx+prop           ((hash-ref kernel 'syntax-property) stx 'color 'blue)]
                          [table-datum->syntax (hash-ref kernel 'datum->syntax)])
                     (and (hash? kernel)
                          (procedure? (hash-ref kernel 'syntax?))
                          (equal? ((hash-ref kernel 'syntax?) stx) #t)
                          (equal? ((hash-ref kernel 'syntax-e) stx) 'demo)
                          (equal? ((hash-ref kernel 'syntax->datum) stx) 'demo)
                          (equal? ((hash-ref kernel 'syntax-source) stx) 'src)
                          (equal? ((hash-ref kernel 'syntax-line) stx) 3)
                          (equal? ((hash-ref kernel 'syntax-column) stx) 0)
                          (equal? ((hash-ref kernel 'syntax-position) stx) 10)
                          (equal? ((hash-ref kernel 'syntax-span) stx) 5)
                          (equal? ((hash-ref kernel 'syntax-property) stx+prop 'color) 'blue)
                          (equal? ((hash-ref kernel 'syntax-property-symbol-keys) stx+prop) '(color))
                          (equal? ((hash-ref kernel 'syntax-e)
                                   (table-datum->syntax #f 'table-demo))
                                  'table-demo))))
             (list "primitive-table/#%linklet bootstrap metadata primitives"
                   (let ([linklet (primitive-table '#%linklet)])
                     (and (hash? linklet)
                          (procedure? (hash-ref linklet 'primitive->compiled-position #f))
                          (procedure? (hash-ref linklet 'compiled-position->primitive #f))
                          (procedure? (hash-ref linklet 'primitive-in-category? #f))
                          (procedure? (hash-ref linklet 'primitive-lookup #f))
                          (procedure? (hash-ref linklet 'compile-linklet #f))
                          (procedure? (hash-ref linklet 'recompile-linklet #f))
                          (procedure? (hash-ref linklet 'eval-linklet #f))
                          (procedure? (hash-ref linklet 'instance-describe-variable! #f))
                          (procedure? (hash-ref linklet 'linklet-virtual-machine-bytes #f))
                          (equal? ((hash-ref linklet 'primitive->compiled-position) '+) #f)
                          (equal? ((hash-ref linklet 'compiled-position->primitive) 0) #f)
                          (equal? ((hash-ref linklet 'primitive-in-category?) '+ 'kernel) #f)
                          (equal? ((hash-ref linklet 'primitive-lookup) '+) #f)
                          (linklet? ((hash-ref linklet 'compile-linklet) '(linklet () () (void))))
                          (equal? ((hash-ref linklet 'eval-linklet) 'demo-linklet) 'demo-linklet)
                          (equal? ((hash-ref linklet 'instance-describe-variable!) (make-instance 'demo) 'x 'desc) (void))
                          (bytes? ((hash-ref linklet 'linklet-virtual-machine-bytes))))))
             (list "primitive-table/#%paramz bootstrap keys"
                   (let ([paramz (primitive-table '#%paramz)])
                     (and (hash? paramz)
                          (eq? (hash-ref paramz 'parameterization-key #f)
                               (hash-ref paramz 'parameterization-key #f))
                          (eq? (hash-ref paramz 'break-enabled-key #f)
                               (hash-ref paramz 'break-enabled-key #f))
                          (eq? (hash-ref paramz 'exception-handler-key #f)
                               (hash-ref paramz 'exception-handler-key #f))
                          (procedure? (hash-ref paramz 'reparameterize #f))
                          (procedure? (hash-ref paramz 'cache-configuration #f))
                          (equal? ((hash-ref paramz 'reparameterize) 'paramz)
                                  'paramz)
                          (let ([installed (make-hasheq)])
                            (primitive-table 'demo installed)
                            (eq? (primitive-table 'demo) installed))
                          (let ([n 0]
                                [cache (hash-ref paramz 'cache-configuration)])
                            (and (equal? (cache 0 (lambda ()
                                                    (set! n (+ n 1))
                                                    'cached))
                                         'cached)
                                 (equal? (cache 0 (lambda () 'fresh))
                                         'cached)
                                 (equal? n 1))))))
             (list "identifier?"
                   (let* ([id-stx     (datum->syntax #f 'pear)]
                          [list-stx   (datum->syntax #f '(pear core))]
                          [number-stx (datum->syntax #f 5)])
                     (and (equal? (identifier? id-stx) #t)
                          (equal? (identifier? list-stx) #f)
                          (equal? (identifier? number-stx) #f)
                          (equal? (identifier? 'pear) #f)
                          (equal? (identifier? 5) #f))))

             (list "free-identifier=?/datum-bootstrap"
                   (let ([a1 (datum->syntax #f 'apple)]
                         [a2 (datum->syntax #f 'apple)]
                         [b  (datum->syntax #f 'banana)])
                     (and (equal? (free-identifier=? a1 a2) #t)
                          (equal? (free-identifier=? a1 b) #f)
                          (equal? (free-identifier=? a1 a2 #f) #t)
                          (equal? (free-identifier=? a1 a2 #f #f) #t))))

             (list "bound-identifier=?/datum-bootstrap"
                   (let ([a1 (datum->syntax #f 'apple)]
                         [a2 (datum->syntax #f 'apple)]
                         [b  (datum->syntax #f 'banana)])
                     (and (equal? (bound-identifier=? a1 a2) #t)
                          (equal? (bound-identifier=? a1 b) #f)
                          (equal? (bound-identifier=? a1 a2 #f) #t)
                          (equal? (bound-identifier=? a1 a2 1) #t)
                          (with-handlers ([exn:fail:contract? (lambda (_exn) #t)])
                            (bound-identifier=? a1 a2 'bad-phase)
                            #f))))

             (list "identifier-binding/bootstrap"
                   (let ([id (datum->syntax #f 'apple)])
                     (and (equal? (identifier-binding id) #f)
                          (equal? (identifier-binding id 0) #f)
                          (equal? (identifier-binding id 1 #t #t) #f)
                          (equal? (identifier-binding id #f #t) #f)
                          (equal? (identifier-binding id #f #t #t) #f)
                          (with-handlers ([exn:fail:contract? (lambda (_exn) #t)])
                            (identifier-binding id 'bad-phase)
                            #f))))

             (list "syntax-local-phase-level/bootstrap"
                   (equal? (syntax-local-phase-level) 0))

             (list "syntax-transforming?/bootstrap"
                   (equal? (syntax-transforming?) #f))

             (list "check-duplicate-identifier/datum-bootstrap"
                   (let ([a1 (datum->syntax #f 'apple)]
                         [a2 (datum->syntax #f 'apple)]
                         [b  (datum->syntax #f 'banana)])
                     (and (equal? (check-duplicate-identifier (list a1 b)) #f)
                          (equal? (check-duplicate-identifier (list a1 b a2)) a2)
                          (with-handlers ([exn:fail:contract? (lambda (_exn) #t)])
                            (check-duplicate-identifier (list a1 'banana))
                            #f))))

             (list "generate-temporaries/bootstrap"
                   (let ([empty (generate-temporaries '())]
                         [ids   (generate-temporaries (datum->syntax #f '(a b c)))])
                     (and (equal? (length ids) 3)
                          (equal? empty '())
                          (andmap identifier? ids)
                          (equal? (check-duplicate-identifier ids) #f)
                          (with-handlers ([exn:fail:contract? (lambda (_exn) #t)])
                            (generate-temporaries '(a . b))
                            #f))))

             (list "make-syntax-introducer/bootstrap"
                   (let* ([stx (datum->syntax #f 'x)]
                          [intro (make-syntax-introducer)]
                          [use-site-intro (make-syntax-introducer #t)])
                     (and (procedure? intro)
                          (equal? (intro stx) stx)
                          (equal? (intro stx 'add) stx)
                          (equal? (intro stx 'remove) stx)
                          (equal? (use-site-intro stx 'flip) stx))))

             (list "syntax-local-introduce/bootstrap"
                   (let ([stx (datum->syntax #f 'x)])
                     (equal? (syntax-local-introduce stx) stx)))

             (list "syntax-local-infer-name/bootstrap"
                   (let ([stx (datum->syntax #f 'x)])
                     (and (equal? (syntax-local-infer-name stx) #f)
                          (equal? (syntax-local-infer-name stx #f) #f))))

             (list "make-syntax-delta-introducer/bootstrap"
                   (let* ([stx (datum->syntax #f 'x)]
                          [intro (make-syntax-delta-introducer stx #f)])
                     (and (procedure? intro)
                          (equal? (intro stx) stx)
                          (equal? (intro stx 'flip) stx)
                          (equal? (intro stx 'remove) stx))))

             (list "make-rename-transformer/bootstrap"
                   (let ([stx (datum->syntax #f 'apple)])
                     (and (equal? (make-rename-transformer stx) stx)
                          (equal? (rename-transformer? stx) #f)
                          (with-handlers ([exn:fail:contract? (lambda (_exn) #t)])
                            (make-rename-transformer 'apple)
                            #f))))

             (list "module-name/module-path-fixup/bootstrap"
                   (and (equal? (module-name-fixup #f '()) 'top-level)
                        (equal? (module-name-fixup 'demo '()) 'demo)
                        (equal? (module-name-fixup '(demo inner) '(leaf))
                                '(demo inner leaf))
                        (equal? (module-path-fixup #f '()) 'top-level)
                        (equal? (module-path-fixup 'demo '())
                                '(quote demo))
                        (equal? (module-path-fixup '(demo inner) '(leaf))
                                '(submod (quote demo) inner leaf))))

             (list "syntax-local-value/failure-thunk-bootstrap"
                   (let ([id (datum->syntax #f 'missing-transformer)])
                     (equal? (syntax-local-value id (lambda () 'fallback)) 'fallback)))

             (list "namespace-call-with-registry-lock/bootstrap"
                   (equal?
                    (namespace-call-with-registry-lock
                     (make-empty-namespace)
                     (lambda () 'locked))
                    'locked))

             (list "variable-reference->namespace/bootstrap"
                   (namespace? (variable-reference->namespace (#%variable-reference))))

             (list "current-namespace/bootstrap"
                   (let ([ns (make-empty-namespace)])
                     (and (namespace? (current-namespace))
                          (parameterize ([current-namespace ns])
                            (equal? (current-namespace) ns)))))

             (list "dynamic-require/bootstrap-fallback"
                   (and (equal? (dynamic-require 'racket/base 'car
                                                (lambda () 'fallback))
                                'fallback)
                        (with-handlers ([exn:fail:contract? (lambda (_exn) #t)])
                          (dynamic-require 'racket/base 'car)
                          #f)
                        (with-handlers ([exn:fail:contract? (lambda (_exn) #t)])
                          (dynamic-require 'racket/base 'car 42)
                          #f)))

             (list "set!-transformer-procedure/bootstrap-contract"
                   (and (equal? (set!-transformer? (lambda (stx) stx)) #f)
                        (with-handlers ([exn:fail:contract? (lambda (_exn) #t)])
                          (set!-transformer-procedure (lambda (stx) stx))
                          #f)))

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

       (list "14.1 Namespaces"
             (list
              (list "variable-reference?"
                    (let ([vr (#%variable-reference)])
                      (and (variable-reference? vr)
                           (not (variable-reference? 'not-a-variable-reference))
                           (equal? (variable-reference->instance vr) #f)
                           (equal? (variable-reference->instance vr #t) #f)
                           (equal? (variable-reference->module-path-index vr) #f)
                           (equal? (variable-reference->module-source/submod vr) #f)
                           (equal? (variable-reference-constant? vr) #f)
                           (equal? (variable-reference-from-unsafe? vr) #f))))
              (list "bootstrap reflection predicates"
                    (and (equal? (compiled-expression? 42) #f)
                         (equal? (compiled-module-expression? 42) #f)
                         (equal? (internal-definition-context? 42) #f)
                         (equal? (liberal-define-context? 42) #f)
                         (equal? (module-path? 'racket/base) #t)
                         (equal? (module-path? "relative.rkt") #t)
                         (equal? (module-path? '(submod "." deserialize)) #t)
                         (equal? (module-path? 42) #f)
                         (module-path-index? (module-path-index-join 'racket/base #f))
                         (equal? (module-path-index? 42) #f)
                         (equal? (readtable? 42) #f)
                         (equal? (rename-transformer? 42) #f)
                         (equal? (resolved-module-path? 42) #f)
                         (equal? (set!-transformer? 42) #f)
                         (equal? (special-comment? 42) #f)
                         (equal? (syntax-original? (datum->syntax #f 'x)) #f)
                         (equal? (syntax-tainted? (datum->syntax #f 'x)) #f))))))
       
       (list "14.9 Structure Inspectors"
               (list
                (list "inspector placeholders"
                      (and (equal? (current-inspector) #f)
                           (equal? (inspector? (current-inspector)) #t)
                           (equal? (inspector? 'not-an-inspector) #f)
                           (equal? (current-code-inspector) #f)
                           (equal? (make-inspector (current-code-inspector)) #f)
                           (equal? (inspector-superior?
                                    (current-code-inspector)
                                    (make-inspector (current-code-inspector)))
                                   #t)))
                (list "object-name/procedure"
                      (let* ([anon      (lambda (x) x)]
                             [renamed   (procedure-rename +       'plus)]
                             [renamed-2 (procedure-rename renamed 'again)])
                        (list (equal? (object-name +)         '+)
                              (equal? (object-name renamed)   'plus)
                              (equal? (object-name renamed-2) 'again)
                              (equal? (object-name anon)      'anon))))

                (list "procedure-realm"
                      (let* ([anon    (lambda (x) x)]
                             [renamed (procedure-rename anon 'named 'webracket-test)])
                        (and (equal? (procedure-realm +)       'racket/primitive)
                             (equal? (procedure-realm anon)    'racket)
                             (equal? (procedure-realm renamed) 'webracket-test))))

                ;; display, write, and print are in stdlib, so keep this
                ;; disabled for the --no-stdlib test-basics run.
                #;(list "procedure printing names"
                        (let ([out (open-output-string)])
                          (display string out)
                          (newline out)
                          (write string? out)
                          (newline out)
                          (print string-append out)
                          (equal? (get-output-string out)
                                  "#<procedure:string>\n#<procedure:string?>\n#<procedure:string-append>")))

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
                           [vector-loc #(source 10 2 100 5)]
                           [crlt    (datum->correlated payload vector-loc)]
                           [struct-loc (srcloc 'struct-source 11 3 101 6)]
                           [crlt2   (datum->correlated 'struct-payload struct-loc)]
                           [bare    (datum->correlated 'plain)]
                           [again   (datum->correlated
                                     crlt
                                     #(ignored-source 1 0 1 1)
                                     (correlated-property bare 'ignored 'property))])
                      (list (equal? (correlated? crlt)         #t)
                            (equal? (correlated? payload)      #f)
                            (equal? (correlated-source crlt)   'source)
                            (equal? (correlated-line crlt)     10)
                            (equal? (correlated-column crlt)   2)
                            (equal? (correlated-position crlt) 100)
                            (equal? (correlated-span crlt)     5)
                            (equal? (correlated-e crlt)        payload)
                            (equal? (correlated-source crlt2)   'struct-source)
                            (equal? (correlated-line crlt2)     11)
                            (equal? (correlated-column crlt2)   3)
                            (equal? (correlated-position crlt2) 101)
                            (equal? (correlated-span crlt2)     6)
                            (equal? (correlated-e crlt2)        'struct-payload)
                            (equal? (correlated-source bare)   #f)
                            (equal? (correlated-line bare)     #f)
                            (equal? (correlated-column bare)   #f)
                            (equal? (correlated-position bare) #f)
                            (equal? (correlated-span bare)     #f)
                            (equal? (eq? again crlt)           #t)
                            (equal? (correlated-e again)       payload)
                            (equal? (correlated-source again)  'source)
                            (equal? (correlated-property again 'ignored) #f))))

              (list "correlated->datum"
                    (let* ([inner     (datum->correlated 'x #(src 1 0 1 1))]
                           [vec       (vector inner 'y)]
                           [boxed     (box inner)]
                           [lst       (list inner vec boxed)]
                           [converted (correlated->datum lst)])
                      (list (pair? converted)
                            (equal? (car converted) 'x)
                            (let ([vec-result (cadr converted)])
                              (list (equal? (vector? vec-result) #t)
                                    ; Full Racket doesn't recur through vectors
                                    (equal? (equal? (vector-ref vec-result 0) 'x) #f)
                                    (vector-ref vec-result 0)
                                    (equal? (correlated? (vector-ref vec-result 0)) #t)
                                    (list (vector-ref vec-result 1) 'y)))
                            (let ([box-result (caddr converted)])
                              (list (equal? (box? box-result) #t)
                                    (equal? (correlated? (unbox box-result)) #t))))))

              (list "correlated properties"
                    (let* ([base        (datum->correlated 'seed)]
                           [with-tag    (correlated-property base     'tag 'value)]
                           [with-number (correlated-property with-tag 123  'number)]
                           [keys2       (correlated-property-symbol-keys with-number)]
                           [updated     (correlated-property with-tag 'tag 'new)]
                           [with-false   (correlated-property updated  'tag #f)]
                           [prop-source (datum->correlated 'copy #f with-tag)]
                           [keys        (correlated-property-symbol-keys with-number)])
                      (list (equal? (correlated-property base        'tag) #f)
                            (equal? (correlated-property with-tag    'tag) 'value)
                            (equal? (correlated-property updated     'tag) 'new)
                            (equal? (correlated-property with-false  'tag) #f)
                            (equal? (correlated-property with-number 123)  'number)
                            (equal? (correlated-property prop-source 'tag) 'value)
                            (member 'tag (correlated-property-symbol-keys with-false))
                            keys2
                            keys
                            (member 'tag keys)
                            (not (equal? (member 'tag keys) #f))
                            (equal? (member 123 keys) #f)
                            (member 'tag keys)
                            (equal? (member 123 keys) #f))))

              (list "correlated properties"
                    (let* ([base        (datum->correlated 'seed)]
                           [with-tag    (correlated-property base     'tag 'value)]
                           [with-number (correlated-property with-tag 123  'number)]
                           [updated     (correlated-property with-tag 'tag 'new)]
                           [with-false   (correlated-property updated  'tag #f)]
                           [prop-source (datum->correlated 'copy #f with-tag)]
                           [key1        (string-copy "k")]
                           [key2        (string-copy "k")]
                           [with-key1   (correlated-property base      key1 'v1)]
                           [with-key2   (correlated-property with-key1 key2 'v2)]
                           [ordered      (correlated-property
                                          (correlated-property base 'a 1)
                                          'b
                                          2)]
                           [keys        (correlated-property-symbol-keys with-number)])
                      (and (equal? (correlated-property base        'tag) #f)
                           (equal? (correlated-property with-tag    'tag) 'value)
                           (equal? (correlated-property updated     'tag) 'new)
                           (equal? (correlated-property with-false  'tag) #f)
                           (equal? (correlated-property with-number 123)  'number)
                           (equal? (correlated-property prop-source 'tag) 'value)
                           (equal? (equal? key1 key2)                    #t)
                           (equal? (eq? key1 key2)                       #f)
                           (equal? (correlated-property with-key2 key1)   'v1)
                           (equal? (correlated-property with-key2 key2)   'v2)
                           (let ([ordered-keys (correlated-property-symbol-keys ordered)])
                             (and (equal? (length ordered-keys) 2)
                                  (equal? (and (member 'a ordered-keys) #t) #t)
                                  (equal? (and (member 'b ordered-keys) #t) #t)))
                           (equal? (and (member 'tag (correlated-property-symbol-keys with-false)) #t) #t)
                           (equal? (and (member 'tag keys) #t)            #t)
                           (equal? (member 123 keys)                      #f)
                           #t)))

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

              (list "instance constant reinstall"
                    (let ([inst (make-instance 'constant-namespace)])
                      (instance-set-variable-value! inst 'x 10 'constant)
                      (instance-set-variable-value! inst 'x 15 'constant)
                      (equal? (instance-variable-value inst 'x) 15)))

              (list "compiled linklet shares linklet accessors"
                    (let* ([proc (lambda args (void))]
                           [cl   (make-compiled-linklet 'demo '() '() proc)])
                      (and (equal? (linklet? cl)                 #t)
                           (equal? (linklet-name cl)             'demo)
                           (equal? (linklet-import-variables cl) '())
                           (equal? (linklet-export-variables cl) '()))))

              (list "linklet body reserved symbols"
                    (and (equal? (linklet-body-reserved-symbol? 'lambda) #t)
                         (equal? (linklet-body-reserved-symbol? '+) #t)
                         (equal? (linklet-body-reserved-symbol? 'quote-syntax) #f)
                         (equal? (linklet-body-reserved-symbol? 'ordinary-user-name) #f)))

              (list "linklet bundle mutable-hash wrapper"
                    (let* ([proc    (lambda args (void))]
                           [cl      (make-compiled-linklet 'bundle-demo '() '() proc)]
                           [content (make-hasheq)]
                           [_       (hash-set! content 'main cl)]
                           [bundle  (hash->linklet-bundle content)]
                           [back    (linklet-bundle->hash bundle)]
                           [_       (hash-set! content 0 'metadata)])
                      (and (equal? (linklet-bundle? bundle) #t)
                           (equal? (linklet-bundle? content) #f)
                           (equal? (eq? back content) #t)
                           (equal? (hash-ref back 'main) cl)
                           (equal? (hash-ref back 0) 'metadata))))

              (list "linklet directory mutable-hash wrapper"
                    (let* ([bundle-content (make-hasheq)]
                           [bundle         (hash->linklet-bundle bundle-content)]
                           [nested-content (make-hasheq)]
                           [nested         (hash->linklet-directory nested-content)]
                           [content        (make-hasheq)]
                           [_              (hash-set! content #f bundle)]
                           [_              (hash-set! content 'sub nested)]
                           [directory      (hash->linklet-directory content)]
                           [back           (linklet-directory->hash directory)])
                      (and (equal? (linklet-directory? directory) #t)
                           (equal? (linklet-directory? content) #f)
                           (equal? (eq? back content) #t)
                           (equal? (linklet-bundle? (hash-ref back #f)) #t)
                           (equal? (linklet-directory? (hash-ref back 'sub)) #t))))

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

              (list "compile-linklet/source define-values"
                    (let* ([compiled (compile-linklet
                                      '(linklet () (x)
                                         (define-values (x) '42))
                                      #f
                                      #f)]
                           [target   (make-instance 'source-target)]
                           [result   (instantiate-linklet compiled '() target)])
                      (and (instance? target)
                           (void? result)
                           (equal? (instance-variable-value target 'x) 42))))

              (list "compile-linklet/source imports"
                    (let* ([import-inst (make-instance 'source-provider)]
                           [_           (instance-set-variable-value! import-inst 'shared 41)]
                           [compiled    (compile-linklet
                                         '(linklet ((shared)) (result)
                                            (define-values (result) (#%app + shared '1)))
                                         #f
                                         #f)]
                           [target      (make-instance 'source-import-target)]
                           [result      (instantiate-linklet compiled (list import-inst) target)])
                      (and (void? result)
                           (equal? (instance-variable-value target 'result) 42))))

              (list "compile-linklet/source multiple values"
                    (let* ([compiled (compile-linklet
                                      '(linklet () (a b)
                                         (define-values (a b)
                                           (#%app values 'left 'right)))
                                      #f
                                      #f)]
                           [target   (make-instance 'source-values-target)]
                           [result   (instantiate-linklet compiled '() target)])
                      (and (void? result)
                           (equal? (instance-variable-value target 'a) 'left)
                           (equal? (instance-variable-value target 'b) 'right))))

              (list "compile-linklet/source list and cons"
                    (let* ([compiled (compile-linklet
                                      '(linklet () (x)
                                         (define-values (x)
                                           (#%app list (#%app cons '1 '2) '3)))
                                      #f
                                      #f)]
                           [target   (make-instance 'source-list-target)]
                           [result   (instantiate-linklet compiled '() target)])
                      (and (void? result)
                           (equal? (instance-variable-value target 'x)
                                   (list (cons 1 2) 3)))))

              (list "compile-linklet/source primitive dispatch"
                    (let* ([compiled (compile-linklet
                                      '(linklet () (x y z)
                                         (define-values (x) (#%app * '2 '3 '4))
                                         (define-values (y) (#%app - x '5))
                                         (define-values (z) (#%app car (#%app cons y 'ignored))))
                                      #f
                                      #f)]
                           [target   (make-instance 'source-primitive-target)]
                           [result   (instantiate-linklet compiled '() target)])
                      (and (void? result)
                           (equal? (instance-variable-value target 'x) 24)
                           (equal? (instance-variable-value target 'y) 19)
                           (equal? (instance-variable-value target 'z) 19))))

              (list "compile-linklet/source if"
                    (let* ([compiled (compile-linklet
                                      '(linklet () (x y)
                                         (define-values (x) (if 'yes 'then 'else))
                                         (define-values (y) (if #f 'then 'else)))
                                      #f
                                      #f)]
                           [target   (make-instance 'source-if-target)]
                           [result   (instantiate-linklet compiled '() target)])
                      (and (void? result)
                           (equal? (instance-variable-value target 'x) 'then)
                           (equal? (instance-variable-value target 'y) 'else))))

              
              
             ))


 
 (list "15. Operating System"
       (list "15.1 Environment Variables"
             (list
              (list "environment-variable empty set"
                    (let ([env (current-environment-variables)])
                      (and (equal? (environment-variables? env) #t)
                           (equal? (environment-variables? #"PATH") #f)
                           (equal? (environment-variables-ref env #"PATH") #f))))))
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
                         (equal? (path-string? (bytes 1 2 3))  #f)))
              (list "string->path"
                    (let ([p (string->path "app/main.rkt")])
                      (and (path? p)
                           (equal? (path->string p) "app/main.rkt")
                           (equal? (path->bytes p) #"app/main.rkt"))))
              (list "some-system path conversions"
                    (let ([unix-path (string->some-system-path "hello" 'unix)]
                          [win-path  (string->some-system-path "c:\\tmp\\file.txt" 'windows)])
                      (and (path? unix-path)
                           (equal? (path? win-path) #f)
                           (path-for-some-system? win-path)
                           (equal? (some-system-path->string unix-path) "hello")
                           (equal? (some-system-path->string win-path) "c:\\tmp\\file.txt")
                           (equal? (path->bytes win-path) #"c:\\tmp\\file.txt")
                           (equal? (path-convention-type unix-path) 'unix)
                           (equal? (path-convention-type win-path) 'windows)
                           (equal? (system-path-convention-type) 'unix)
                           (with-handlers ([exn:fail? (lambda (_ex) #t)])
                             (path-convention-type "hello")
                             #f))))
              (list "path<?"
                    (and (equal? (path<? (string->path "a")) #t)
                         (equal? (path<? (string->path "a") (string->path "b")) #t)
                         (equal? (path<? (string->path "a") (string->path "b") (string->path "c")) #t)
                         (equal? (path<? (string->path "a") (string->path "a")) #f)
                         (equal? (path<? (string->path "b") (string->path "a")) #f)))
              (list "path-element conversions"
                    (and (equal? (path-element->string (string->path-element "a")) "a")
                         (equal? (path-element->bytes (bytes->path-element #"a")) #"a")
                         (equal? (path-element->string (bytes->path-element #"a\\" 'windows)) "a")
                         (equal? (string->path-element "a/b" #t) #f)
                         (equal? (bytes->path-element #"a/b" 'unix #t) #f)
                         (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                           (string->path-element "a/b")
                           #f)
                         (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                           (bytes->path-element #"a/b")
                           #f)))
              (list "path shape predicates"
                    (and (equal? (absolute-path? "/app/main.rkt") #t)
                         (equal? (absolute-path? "app/main.rkt")  #f)
                         (equal? (relative-path? "app/main.rkt")  #t)
                         (equal? (relative-path? "/app/main.rkt") #f)
                         (equal? (complete-path? "/app/main.rkt") #t)
                         (equal? (complete-path? "app/main.rkt")  #f)))
              (list "current-directory"
                    (and (equal? (path->string (current-directory)) "/app/")
                         (begin
                           (current-directory "/tmp/")
                           (equal? (path->string (current-directory)) "/tmp/"))))
              (list "current-directory-for-user"
                    (let ([original-current (current-directory)]
                          [original-user    (current-directory-for-user)])
                      (current-directory "/tmp/")
                      (current-directory-for-user "/app/")
                      (let ([result (and (equal? (path->string original-user) "/app/")
                                         (equal? (path->string (current-directory)) "/tmp/")
                                         (equal? (path->string (current-directory-for-user)) "/app/")
                                         (void? (current-directory-for-user "/tmp/"))
                                         (equal? (path->string (current-directory-for-user)) "/tmp/"))])
                        (current-directory original-current)
                        (current-directory-for-user original-user)
                        result)))
              (list "current-drive"
                    (and (complete-path? (current-drive))
                         (equal? (path->string (current-drive)) "/")))
              (list "find-system-path"
                    (and (equal? (path->string (find-system-path 'home-dir)) "/home/")
                         (equal? (path->string (find-system-path 'temp-dir)) "/tmp/")
                         (equal? (path->string (find-system-path 'pref-file))
                                 "/home/.config/racket/racket-prefs.rktd")
                         (equal? (path->string (find-system-path 'pref-dir))
                                 "/home/.config/racket/")
                         (equal? (path->string (find-system-path 'init-file))
                                 "/home/.racketrc")
                         (equal? (path->string (find-system-path 'cache-dir))
                                 "/home/.cache/racket/")
                         (equal? (path->string (find-system-path 'sys-dir)) "/")
                         (equal? (path->string (find-system-path 'exec-file))
                                 "/app/webracket")
                         (equal? (path->string (find-system-path 'collects-dir))
                                 "/app/collects/")
                         (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                           (find-system-path "home-dir")
                           #f)
                         (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                           (find-system-path 'not-a-system-path)
                           #f)))
              (list "find-executable-path"
                    (and (equal? (find-executable-path "webracket") #f)
                         (equal? (find-executable-path "webracket" "collects" #t) #f)
                         (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                           (find-executable-path 'webracket)
                           #f)
                         (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                           (find-executable-path "webracket" "/absolute")
                           #f)))
              (list "path->complete-path"
                    (and (equal? (path->string (path->complete-path "notes.txt" "/app/"))
                                 "/app/notes.txt")
                         (equal? (path->string (path->complete-path "/assets/logo.png"))
                                 "/assets/logo.png")))
              (list "path->directory-path"
                    (and (equal? (path->string (path->directory-path "/app/data"))
                                 "/app/data/")
                         (equal? (path->string (path->directory-path "/app/data/"))
                                 "/app/data/")
                         (equal? (path->string (path->directory-path "relative"))
                                 "relative/")
                         (equal? (some-system-path->string
                                  (path->directory-path
                                   (string->some-system-path "a\\b" 'windows)))
                                 "a\\b\\")))
              (list "cleanse-path"
                    (and (equal? (path->string (cleanse-path "tiny//dancer"))
                                 "tiny/dancer")
                         (equal? (path->string (cleanse-path "a///b/"))
                                 "a/b/")
                         (equal? (path->string (cleanse-path "//a"))
                                 "/a")
                         (equal? (path->string (cleanse-path "a/./b"))
                                 "a/./b")
                         (equal? (some-system-path->string
                                  (cleanse-path
                                   (string->some-system-path "a\\\\b" 'windows)))
                                 "a\\b")
                         (equal? (some-system-path->string
                                  (cleanse-path
                                   (string->some-system-path "a//b/" 'windows)))
                                 "a/b/")))
              (list "simplify-path"
                    (let ([win-path (string->some-system-path "a/x/../b" 'windows)])
                      (and (equal? (path->string (simplify-path "a/./b" #f))
                                   "a/b")
                           (equal? (path->string (simplify-path "a/x/../b" #f))
                                   "a/b")
                           (equal? (path->string (simplify-path "../x" #f))
                                   "../x")
                           (equal? (path->string (simplify-path "/a/.." #f))
                                   "/")
                           (equal? (path->string (simplify-path "/a/../b/" #f))
                                   "/b/")
                           (equal? (path->string (simplify-path "a/.." #f))
                                   "./")
                           (equal? (path->string (simplify-path "." #f))
                                   "./")
                           (equal? (path->string (simplify-path ".." #f))
                                   "../")
                           (equal? (path->string (simplify-path "a//b" #f))
                                   "a/b")
                           (equal? (some-system-path->string (simplify-path win-path #f))
                                   "a\\b")
                           (equal? (path-convention-type (simplify-path win-path #f))
                                   'windows))))
              (list "resolve-path"
                    (and (equal? (path->string (resolve-path "/app/main.rkt"))
                                 "/app/main.rkt")
                         (equal? (path->string (resolve-path "relative.txt"))
                                 "relative.txt")))
              (list "normal-case-path"
                    (let ([win-path (string->some-system-path "A/B/C.rkt" 'windows)])
                      (and (equal? (path->string (normal-case-path "A/B/C.rkt"))
                                   "A/B/C.rkt")
                           (equal? (some-system-path->string (normal-case-path win-path))
                                   "a\\b\\c.rkt")
                           (equal? (path-convention-type (normal-case-path win-path))
                                   'windows))))
              (list "path-replace-extension"
                    (and (equal? (path->string (path-replace-extension "x/y.ss" #".rkt"))
                                 "x/y.rkt")
                         (equal? (path->string (path-replace-extension "x/y.ss" #""))
                                 "x/y")
                         (equal? (path->string (path-replace-extension "x/y" #".rkt"))
                                 "x/y.rkt")
                         (equal? (path->string (path-replace-extension "x/y.tar.gz" #".rkt"))
                                 "x/y.tar.rkt")
                         (equal? (path->string (path-replace-extension "x/.racketrc" #".rkt"))
                                 "x/.racketrc.rkt")))
              (list "path-add-extension"
                    (and (equal? (path->string (path-add-extension "x/y.ss" #".rkt"))
                                 "x/y_ss.rkt")
                         (equal? (path->string (path-add-extension "x/y" #".rkt"))
                                 "x/y.rkt")
                         (equal? (path->string (path-add-extension "x/y.tar.gz" #".rkt"))
                                 "x/y.tar_gz.rkt")
                         (equal? (path->string (path-add-extension "x/y.tar.gz" #".rkt" #"."))
                                 "x/y.tar.gz.rkt")
                         (equal? (path->string (path-add-extension "x/.racketrc" ".rkt"))
                                 "x/.racketrc.rkt")))
              (list "path suffix utilities"
                    (and (equal? (path->string (path-replace-suffix "x/.racketrc" #".rkt"))
                                 "x/.rkt")
                         (equal? (path->string (path-add-suffix "x/.racketrc" #".rkt"))
                                 "x/_racketrc.rkt")
                         (equal? (path->string (path-replace-suffix "x/y.tar.gz" #".rkt"))
                                 "x/y.tar.rkt")
                         (equal? (path->string (path-add-suffix "x/y.tar.gz" #".rkt"))
                                 "x/y.tar_gz.rkt")))
              (list "reroot-path"
                    (and (equal? (path->string (reroot-path "/home/caprica/baltar" "/earth"))
                                 "/earth/home/caprica/baltar")
                         (equal? (path->string (reroot-path "/" "/earth"))
                                 "/earth")
                         (equal? (path->string (reroot-path "a/b" "/earth"))
                                 "/earth/tmp/a/b")))
              (list "path-get-extension"
                    (and (equal? (path-get-extension "x/y.rkt") #".rkt")
                         (equal? (path-get-extension "x/y") #f)
                         (equal? (path-get-extension "x/y.tar.gz") #".gz")
                         (equal? (path-get-extension "x/.racketrc") #f)
                         (equal? (path-get-extension "x/y/") #f)))
              (list "filename-extension"
                    (and (equal? (filename-extension "x/y.rkt") #"rkt")
                         (equal? (filename-extension "x/y") #f)
                         (equal? (filename-extension "x/y.tar.gz") #"gz")
                         (equal? (filename-extension "x/.racketrc") #"racketrc")
                         (equal? (filename-extension "x/y/") #f)))
              (list "path-has-extension?"
                    (and (equal? (path-has-extension? "x/y.rkt" #".rkt") #t)
                         (equal? (path-has-extension? "x/y.ss" #".rkt") #f)
                         (equal? (path-has-extension? "x/y" #".rkt") #f)
                         (equal? (path-has-extension? "x/.racketrc" #".racketrc") #f)
                         (equal? (path-has-extension? "x/compiled/y_rkt.zo" #"_rkt.zo") #t)
                         (equal? (path-has-extension? "x/y/" #"y") #f)
                         (equal? (path-has-extension? "x/y" #"y") #f)))
              (list "file-name-from-path"
                    (and (equal? (path->string (file-name-from-path "a/b")) "b")
                         (equal? (path->string (file-name-from-path "a")) "a")
                         (equal? (file-name-from-path "a/") #f)
                         (equal? (file-name-from-path "/") #f)
                         (equal? (path->string (file-name-from-path "../x")) "x")
                         (equal? (file-name-from-path "..") #f)
                         (equal? (file-name-from-path ".") #f)
                         (equal? (file-name-from-path "a/..") #f)))
              (list "split-path"
                    (and (let-values ([(base name dir?) (split-path "a")])
                           (and (equal? base 'relative)
                                (equal? (path->string name) "a")
                                (equal? dir? #f)))
                         (let-values ([(base name dir?) (split-path "a/b")])
                           (and (equal? (path->string base) "a/")
                                (equal? (path->string name) "b")
                                (equal? dir? #f)))
                         (let-values ([(base name dir?) (split-path "a/")])
                           (and (equal? base 'relative)
                                (equal? (path->string name) "a")
                                (equal? dir? #t)))
                         (let-values ([(base name dir?) (split-path "/")])
                           (and (equal? base #f)
                                (equal? (path->string name) "/")
                                (equal? dir? #t)))
                         (let-values ([(base name dir?) (split-path "a/..")])
                           (and (equal? (path->string base) "a/")
                                (equal? name 'up)
                                (equal? dir? #t)))
                         (let-values ([(base name dir?)
                                       (split-path (string->some-system-path "a\\b" 'windows))])
                           (and (equal? (some-system-path->string base) "a\\")
                                (equal? (some-system-path->string name) "b")
                                (equal? dir? #f)))))
              (list "explode-path"
                    (let ([show (lambda (part)
                                  (if (path-for-some-system? part)
                                      (some-system-path->string part)
                                      part))])
                      (and (equal? (map show (explode-path "a/b"))
                                   '("a" "b"))
                           (equal? (map show (explode-path "a/"))
                                   '("a"))
                           (equal? (map show (explode-path "/a/b"))
                                   '("/" "a" "b"))
                           (equal? (map show (explode-path "../x"))
                                   '(up "x"))
                           (equal? (map show (explode-path "."))
                                   '(same))
                           (equal? (map show (explode-path "a/.."))
                                   '("a" up))
                           (equal? (map show (explode-path
                                              (string->some-system-path "a\\b" 'windows)))
                                   '("a" "b")))))
              (list "path-element?"
                    (and (equal? (path-element? (string->path "a")) #t)
                         (equal? (path-element? (string->path "a/")) #t)
                         (equal? (path-element? (string->path "a/b")) #f)
                         (equal? (path-element? (string->path ".")) #f)
                         (equal? (path-element? (string->path "..")) #f)
                         (equal? (path-element? (string->some-system-path "a" 'windows)) #t)
                         (equal? (path-element? (string->some-system-path "a\\b" 'windows)) #f)
                         (equal? (path-element? "a") #f)))
              (list "path-element accessors"
                    (let ([unix     (string->path "a")]
                          [unix-dir (string->path "a/")]
                          [win      (string->some-system-path "a\\" 'windows)])
                      (and (equal? (path-element->string unix) "a")
                           (equal? (path-element->bytes unix) #"a")
                           (equal? (path-element->string unix-dir) "a")
                           (equal? (path-element->bytes unix-dir) #"a")
                           (equal? (path-element->string win) "a")
                           (equal? (path-element->bytes win) #"a")
                           (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                             (path-element->string (string->path "a/b"))
                             #f))))
              (list "simple-form-path"
                    (and (equal? (path->string (simple-form-path "a/../b"))
                                 "/tmp/b")
                         (equal? (path->string (simple-form-path "./x"))
                                 "/tmp/x")
                         (complete-path? (simple-form-path "x"))))
              (list "normalize-path"
                    (and (equal? (path->string (normalize-path "a/../b"))
                                 "/tmp/b")
                         (equal? (path->string (normalize-path "./x" "/var/tmp/"))
                                 "/var/tmp/x")))
              (list "find-relative-path"
                    (and (equal? (path->string (find-relative-path "/a/b/" "/a/b/c"))
                                 "c")
                         (equal? (path->string (find-relative-path "/a/b/" "/a/d"))
                                 "../d")
                         (equal? (path->string (find-relative-path "/a/b/" "/x/y"))
                                 "../../x/y")
                         (equal? (path->string (find-relative-path "/" "/a/b"))
                                 "a/b")
                         (equal? (path->string (find-relative-path "/a/b" "/a/b/c"))
                                 "c")
                         (equal? (find-relative-path "/a/b" "/a/b")
                                 "/a/b")
                         (equal? (path->string (find-relative-path "/a/b" "/a/b" #f #f))
                                 ".")
                         (equal? (find-relative-path "/a/b/" "/x/y" #t)
                                 "/x/y")))
              (list "shrink-path-wrt"
                    (and (equal? (shrink-path-wrt (build-path "x.rkt") '()) #f)
                         (equal? (shrink-path-wrt (build-path "x.rkt")
                                                  (list (build-path "x.rkt")))
                                 #f)
                         (equal? (path->string
                                  (shrink-path-wrt
                                   (build-path "x.rkt")
                                   (list (build-path "x.rkt")
                                         (build-path "y.rkt"))))
                                 "x.rkt")
                         (equal? (path->string
                                  (shrink-path-wrt
                                   (build-path "a" "x.rkt")
                                   (list (build-path "a" "x.rkt")
                                         (build-path "b" "x.rkt"))))
                                 "a/x.rkt")
                         (equal? (path->string
                                  (shrink-path-wrt
                                   (build-path "d" "a" "x.rkt")
                                   (list (build-path "b" "x.rkt")
                                         (build-path "c" "a" "x.rkt")
                                         (build-path "d" "a" "x.rkt"))))
                                 "d/a/x.rkt")))
              (list "path-only"
                    (and (equal? (path->string (path-only "a/b")) "a/")
                         (equal? (path-only "a") #f)
                         (equal? (path->string (path-only "a/")) "a/")
                         (equal? (path->string (path-only "/")) "/")
                         (equal? (path->string (path-only "../x")) "../")
                         (equal? (path->string (path-only "..")) "..")
                         (equal? (path->string (path-only "a/..")) "a/..")
                         (equal? (path->string (path-only "/a/b")) "/a/")
                         (equal? (some-system-path->string
                                  (path-only (string->some-system-path "a\\b" 'windows)))
                                 "a\\")))
              (list "build-path"
                    (and (equal? (path->string (build-path "/app" "main.rkt"))
                                 "/app/main.rkt")
                         (equal? (path->string (build-path "/app/" "data" "notes.txt"))
                                 "/app/data/notes.txt")
                         (equal? (path->string (build-path 'same))
                                 ".")
                         (equal? (path->string (build-path 'up))
                                 "..")
                         (equal? (path->string (build-path "a" 'up))
                                 "a/..")
                         (equal? (path->string (build-path 'up "a"))
                                 "../a")))
              (list "build-path/convention-type"
                    (and (equal? (some-system-path->string
                                  (build-path/convention-type 'unix 'same))
                                 ".")
                         (equal? (some-system-path->string
                                  (build-path/convention-type 'unix 'up))
                                 "..")
                         (equal? (some-system-path->string
                                  (build-path/convention-type 'unix "a" 'up))
                                 "a/..")
                         (equal? (some-system-path->string
                                  (build-path/convention-type
                                   'windows
                                   (bytes->path #"a" 'windows)
                                   'up))
                                 "a\\..")
                         (equal? (some-system-path->string
                                  (build-path/convention-type
                                   'windows
                                   (bytes->path #"a" 'windows)
                                   (bytes->path #"b" 'windows)))
                                 "a\\b")))))
       (list "15.2 Filesystem"
             (list
              (list "webracket-vfs-write-file"
                    (begin
                      (webracket-vfs-write-file "/app/main.rkt" #"")
                      (webracket-vfs-write-file "/app/data/notes.txt" #"notes\n")
                      (void? (webracket-vfs-write-file "/app/data/more.txt" #"more"))))
              (list "file-exists?"
                    (and (equal? (file-exists? "/app/main.rkt") #t)
                         (equal? (file-exists? "/app") #f)
                         (equal? (file-exists? "/app/missing.rkt") #f)))
              (list "directory-exists?"
                    (and (equal? (directory-exists? "/app") #t)
                         (equal? (directory-exists? "/app/data") #t)
                         (equal? (directory-exists? "/app/main.rkt") #f)))
              (list "link-exists?"
                    (and (equal? (link-exists? "/app/main.rkt") #f)
                         (equal? (link-exists? "/app/data") #f)
                         (equal? (link-exists? "/app/missing-link") #f)))
              (list "file-or-directory-type"
                    (and (equal? (file-or-directory-type "/app/main.rkt") 'file)
                         (equal? (file-or-directory-type "/app/data") 'directory)
                         (equal? (file-or-directory-type "/app/missing-type") #f)
                         (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                          (lambda (_ex) #t)])
                           (file-or-directory-type "/app/missing-type" #t)
                           #f)))
              (list "directory-list"
                    (let ([original (current-directory)])
                      (make-directory "/app/list-dir")
                      (make-directory "/app/list-dir/z-dir")
                      (webracket-vfs-write-file "/app/list-dir/a.txt" #"a")
                      (webracket-vfs-write-file "/app/list-dir/m.txt" #"m")
                      (let ([result
                             (and (equal? (map path->string (directory-list "/app/list-dir"))
                                          '("a.txt" "m.txt" "z-dir"))
                                  (begin
                                    (current-directory "/app/list-dir/")
                                    (equal? (map path->string (directory-list))
                                            '("a.txt" "m.txt" "z-dir")))
                                  (begin
                                    (make-directory "/app/empty-list-dir")
                                    (equal? (directory-list "/app/empty-list-dir") '()))
                                  (equal? (map path->string
                                               (directory-list "/app/list-dir" #t))
                                          '("/app/list-dir/a.txt"
                                            "/app/list-dir/m.txt"
                                            "/app/list-dir/z-dir"))
                                  (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                                   (lambda (_ex) #t)])
                                    (directory-list "/app/main.rkt")
                                    #f)
                                  (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                                   (lambda (_ex) #t)])
                                    (directory-list "/app/missing-list-dir")
                                    #f))])
                        (current-directory original)
                        result)))
              (list "filesystem-root-list"
                    (equal? (map path->string (filesystem-root-list))
                            '("/app/" "/tmp/")))
              (list "make-temporary-file"
                    (let* ([p1 (make-temporary-file)]
                           [_ (webracket-vfs-write-file "/app/temp-source.txt" #"copied")]
                           [_ (make-directory "/app/temp-base")]
                           [p2 (make-temporary-file "wr~a.tmp" #f "/app/temp-base")]
                           [p3 (make-temporary-file "copy~a.dat" "/app/temp-source.txt" "/app/temp-base")]
                           [p4 (make-temporary-file "dir~a" 'directory "/app/temp-base")]
                           [p5 (make-temporary-file)])
                      (and (complete-path? p1)
                           (equal? (substring (path->string p1) 0 11) "/tmp/rkttmp")
                           (file-exists? p1)
                           (file-exists? p2)
                           (equal? (substring (path->string p2) 0 17) "/app/temp-base/wr")
                           (file-exists? p3)
                           (equal? (file->bytes p3) #"copied")
                           (directory-exists? p4)
                           (file-exists? p5)
                           (not (equal? (path->string p1) (path->string p5))))))
              (list "make-temporary-directory"
                    (let* ([d1 (make-temporary-directory)]
                           [_ (make-directory "/app/temp-dir-base")]
                           [d2 (make-temporary-directory "d~a" "/app/temp-dir-base")])
                      (and (complete-path? d1)
                           (directory-exists? d1)
                           (equal? (substring (path->string d1) 0 11) "/tmp/rkttmp")
                           (complete-path? d2)
                           (directory-exists? d2)
                           (equal? (substring (path->string d2) 0 20) "/app/temp-dir-base/d"))))
              (list "make-temporary-file*"
                    (let* ([_ (webracket-vfs-write-file "/app/temp-star-source.txt" #"star")]
                           [_ (make-directory "/app/temp-star-base")]
                           [p1 (make-temporary-file* #"b" #".tmp")]
                           [p2 (make-temporary-file* #"copy" #".dat" "/app/temp-star-source.txt" "/app/temp-star-base")]
                           [p3 (make-temporary-file* #"b" #".tmp")])
                      (and (complete-path? p1)
                           (equal? (substring (path->string p1) 0 6) "/tmp/b")
                           (file-exists? p1)
                           (file-exists? p2)
                           (equal? (substring (path->string p2) 0 19) "/app/temp-star-base")
                           (equal? (file->bytes p2) #"star")
                           (file-exists? p3)
                           (not (equal? (path->string p1) (path->string p3))))))
              (list "make-temporary-directory*"
                    (let* ([_ (make-directory "/app/temp-dir-star-base")]
                           [d1 (make-temporary-directory* #"d" #"")]
                           [d2 (make-temporary-directory* #"ds" #".dir" "/app/temp-dir-star-base")])
                      (and (complete-path? d1)
                           (directory-exists? d1)
                           (equal? (substring (path->string d1) 0 6) "/tmp/d")
                           (complete-path? d2)
                           (directory-exists? d2)
                           (equal? (substring (path->string d2) 0 23) "/app/temp-dir-star-base"))))
              (list "make-directory"
                    (and (void? (make-directory "/app/newdir"))
                         (equal? (directory-exists? "/app/newdir") #t)
                         (void? (make-directory "/app/newdir/child" #o700))
                         (equal? (directory-exists? "/app/newdir/child") #t)
                         (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                          (lambda (_ex) #t)])
                           (make-directory "/app/newdir")
                           #f)
                         (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                          (lambda (_ex) #t)])
                           (make-directory "/app/main.rkt")
                           #f)
                         (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                          (lambda (_ex) #t)])
                           (make-directory "/app/missing-parent/child")
                           #f)))
              (list "make-directory*"
                    (and (void? (make-directory* "/app/mkdir-star/a/b/c"))
                         (equal? (directory-exists? "/app/mkdir-star") #t)
                         (equal? (directory-exists? "/app/mkdir-star/a/b/c") #t)
                         (void? (make-directory* "/app/mkdir-star/a/b/c"))
                         (void? (make-directory* "/app/"))
                         (begin
                           (webracket-vfs-write-file "/app/mkdir-star-file" #"file")
                           #t)
                         (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                          (lambda (_ex) #t)])
                           (make-directory* "/app/mkdir-star-file/child")
                           #f)))
              (list "make-parent-directory*"
                    (and (void? (make-parent-directory* "/app/parent-star/a/b/file.txt"))
                         (equal? (directory-exists? "/app/parent-star/a/b") #t)
                         (equal? (file-exists? "/app/parent-star/a/b/file.txt") #f)
                         (void? (make-parent-directory* "/app/parent-star/a/b/file.txt"))
                         (void? (make-parent-directory* "/app/"))
                         (void? (make-parent-directory* "/app"))
                         (begin
                           (webracket-vfs-write-file "/app/parent-star-file" #"file")
                           #t)
                         (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                          (lambda (_ex) #t)])
                           (make-parent-directory* "/app/parent-star-file/child.txt")
                           #f)))
              (list "delete-directory"
                    (and (void? (make-directory "/app/delete-dir"))
                         (void? (make-directory "/app/delete-dir/child"))
                         (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                          (lambda (_ex) #t)])
                           (delete-directory "/app/delete-dir")
                           #f)
                         (void? (delete-directory "/app/delete-dir/child"))
                         (equal? (directory-exists? "/app/delete-dir/child") #f)
                         (void? (delete-directory "/app/delete-dir"))
                         (equal? (directory-exists? "/app/delete-dir") #f)
                         (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                          (lambda (_ex) #t)])
                           (delete-directory "/app/delete-dir")
                           #f)
                         (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                          (lambda (_ex) #t)])
                           (delete-directory "/app/main.rkt")
                           #f)))
              (list "delete-directory/files"
                    (begin
                      (make-directory* "/app/delete-tree/a/b")
                      (webracket-vfs-write-file "/app/delete-tree/root.txt" #"root")
                      (webracket-vfs-write-file "/app/delete-tree/a/b/leaf.txt" #"leaf")
                      (webracket-vfs-write-file "/app/delete-tree/a/side.txt" #"side")
                      (webracket-vfs-write-file "/app/delete-tree-file.txt" #"file")
                      (and (void? (delete-directory/files "/app/delete-tree"))
                           (equal? (directory-exists? "/app/delete-tree") #f)
                           (equal? (file-exists? "/app/delete-tree/root.txt") #f)
                           (equal? (file-exists? "/app/delete-tree/a/b/leaf.txt") #f)
                           (void? (delete-directory/files "/app/delete-tree-file.txt"))
                           (equal? (file-exists? "/app/delete-tree-file.txt") #f)
                           (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                            (lambda (_ex) #t)])
                             (delete-directory/files "/app/delete-tree-missing")
                             #f)
                           (void? (delete-directory/files "/app/delete-tree-missing"
                                                          #f)))))
              (list "rename-file-or-directory"
                    (begin
                      (webracket-vfs-write-file "/app/data/rename-source.txt" #"source")
                      (webracket-vfs-write-file "/app/data/rename-target.txt" #"target")
                      (make-directory "/app/rename-dir")
                      (webracket-vfs-write-file "/app/rename-dir/inside.txt" #"inside")
                      (and (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                            (lambda (_ex) #t)])
                             (rename-file-or-directory "/app/data/rename-source.txt"
                                                       "/app/data/rename-target.txt")
                             #f)
                           (void? (rename-file-or-directory "/app/data/rename-source.txt"
                                                            "/app/data/renamed.txt"))
                           (equal? (file-exists? "/app/data/rename-source.txt") #f)
                           (equal? (file->string "/app/data/renamed.txt") "source")
                           (void? (rename-file-or-directory "/app/data/renamed.txt"
                                                            "/app/data/rename-target.txt"
                                                            #t))
                           (equal? (file->string "/app/data/rename-target.txt") "source")
                           (void? (rename-file-or-directory "/app/rename-dir"
                                                            "/app/renamed-dir"))
                           (equal? (directory-exists? "/app/rename-dir") #f)
                           (equal? (file->string "/app/renamed-dir/inside.txt") "inside")
                           (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                            (lambda (_ex) #t)])
                             (rename-file-or-directory "/app/missing-rename.txt"
                                                       "/app/data/nope.txt")
                             #f)
                           (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                            (lambda (_ex) #t)])
                             (rename-file-or-directory "/app/renamed-dir"
                                                       "/app/data/rename-target.txt"
                                                       #t)
                             #f))))
              (list "copy-file"
                    (begin
                      (webracket-vfs-write-file "/app/data/copy-source.txt" #"copy")
                      (webracket-vfs-write-file "/app/data/copy-target.txt" #"target")
                      (and (void? (copy-file "/app/data/copy-source.txt"
                                             "/app/data/copied.txt"))
                           (equal? (file->string "/app/data/copied.txt") "copy")
                           (equal? (file->string "/app/data/copy-source.txt") "copy")
                           (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                            (lambda (_ex) #t)])
                             (copy-file "/app/data/copy-source.txt"
                                        "/app/data/copy-target.txt")
                             #f)
                           (equal? (file->string "/app/data/copy-target.txt") "target")
                           (void? (copy-file "/app/data/copy-source.txt"
                                             "/app/data/copy-target.txt"
                                             #t))
                           (equal? (file->string "/app/data/copy-target.txt") "copy")
                           (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                            (lambda (_ex) #t)])
                             (copy-file "/app/data/missing-copy.txt"
                                        "/app/data/nope.txt")
                             #f)
                           (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                            (lambda (_ex) #t)])
                             (copy-file "/app/data"
                                        "/app/data/copy-dir-source.txt")
                             #f))))
              (list "copy-directory/files"
                    (begin
                      (make-directory* "/app/copy-tree/a/b")
                      (webracket-vfs-write-file "/app/copy-tree/root.txt" #"root")
                      (webracket-vfs-write-file "/app/copy-tree/a/b/leaf.txt" #"leaf")
                      (webracket-vfs-write-file "/app/copy-tree/a/side.txt" #"side")
                      (webracket-vfs-write-file "/app/copy-tree-file.txt" #"file")
                      (and (void? (copy-directory/files "/app/copy-tree"
                                                        "/app/copied-tree"))
                           (equal? (file->string "/app/copied-tree/root.txt") "root")
                           (equal? (file->string "/app/copied-tree/a/b/leaf.txt") "leaf")
                           (equal? (file->string "/app/copied-tree/a/side.txt") "side")
                           (equal? (file->string "/app/copy-tree/a/side.txt") "side")
                           (void? (copy-directory/files "/app/copy-tree-file.txt"
                                                        "/app/copied-tree-file.txt"))
                           (equal? (file->string "/app/copied-tree-file.txt") "file")
                           (void? (copy-directory/files "/app/copy-tree"
                                                        "/app/copied-tree-options"
                                                        #t
                                                        #t))
                           (equal? (file->string "/app/copied-tree-options/root.txt")
                                   "root")
                           (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                            (lambda (_ex) #t)])
                             (copy-directory/files "/app/copy-tree"
                                                   "/app/copied-tree")
                             #f)
                           (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                            (lambda (_ex) #t)])
                             (copy-directory/files "/app/copy-tree"
                                                   "/app/copy-tree/inside")
                             #f)
                           (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                            (lambda (_ex) #t)])
                             (copy-directory/files "/app/missing-copy-tree"
                                                   "/app/nope")
                             #f))))
              (list "file-or-directory-modify-seconds"
                    (begin
                      (webracket-vfs-write-file "/app/data/mtime.txt" #"time")
                      (make-directory "/app/mtime-dir")
                      (and (equal? (file-or-directory-modify-seconds "/app/data/mtime.txt") 0)
                           (void? (file-or-directory-modify-seconds "/app/data/mtime.txt" 123))
                           (equal? (file-or-directory-modify-seconds "/app/data/mtime.txt") 123)
                           (void? (file-or-directory-modify-seconds "/app/mtime-dir" 456))
                           (equal? (file-or-directory-modify-seconds "/app/mtime-dir" #f) 456)
                           (void? (copy-file "/app/data/mtime.txt"
                                             "/app/data/mtime-copy.txt"))
                           (equal? (file-or-directory-modify-seconds "/app/data/mtime-copy.txt") 123)
                           (void? (rename-file-or-directory "/app/data/mtime-copy.txt"
                                                            "/app/data/mtime-renamed.txt"))
                           (equal? (file-or-directory-modify-seconds "/app/data/mtime-renamed.txt") 123)
                           (equal? (file-or-directory-modify-seconds "/app/data/missing-mtime.txt"
                                                                     #f
                                                                     (lambda () 'missing))
                                   'missing)
                           (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                            (lambda (_ex) #t)])
                             (file-or-directory-modify-seconds "/app/data/missing-mtime.txt")
                             #f)
                           (with-handlers ([(lambda (ex) (exn:fail:contract? ex))
                                            (lambda (_ex) #t)])
                             (file-or-directory-modify-seconds "/app/data/mtime.txt" -1)
                             #f))))
              (list "file-or-directory-permissions"
                    (begin
                      (webracket-vfs-write-file "/app/data/perms.txt" #"perms")
                      (make-directory "/app/perms-dir")
                      (and (equal? (file-or-directory-permissions "/app/data/perms.txt")
                                   '(read write))
                           (equal? (file-or-directory-permissions "/app/perms-dir")
                                   '(read write execute))
                           (equal? (file-or-directory-permissions "/app/data/perms.txt" 'bits)
                                   #o666)
                           (void? (file-or-directory-permissions "/app/data/perms.txt" #o600))
                           (equal? (file-or-directory-permissions "/app/data/perms.txt")
                                   '(read write))
                           (equal? (file-or-directory-permissions "/app/data/perms.txt" 'bits)
                                   #o600)
                           (void? (file-or-directory-permissions "/app/data/perms.txt" #o400))
                           (equal? (file-or-directory-permissions "/app/data/perms.txt")
                                   '(read))
                           (void? (copy-file "/app/data/perms.txt"
                                             "/app/data/perms-copy.txt"
                                             #t))
                           (equal? (file-or-directory-permissions "/app/data/perms-copy.txt" 'bits)
                                   #o400)
                           (void? (rename-file-or-directory "/app/data/perms-copy.txt"
                                                            "/app/data/perms-renamed.txt"))
                           (equal? (file-or-directory-permissions "/app/data/perms-renamed.txt")
                                   '(read))
                           (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                            (lambda (_ex) #t)])
                             (file-or-directory-permissions "/app/data/missing-perms.txt")
                             #f)
                           (with-handlers ([(lambda (ex) (exn:fail:contract? ex))
                                            (lambda (_ex) #t)])
                             (file-or-directory-permissions "/app/data/perms.txt" #o200000)
                             #f))))
              (list "file-or-directory-stat"
                    (begin
                      (webracket-vfs-write-file "/app/data/stat.txt" #"stat")
                      (make-directory "/app/stat-dir")
                      (file-or-directory-modify-seconds "/app/data/stat.txt" 321)
                      (file-or-directory-permissions "/app/data/stat.txt" #o600)
                      (let ([file-stat (file-or-directory-stat "/app/data/stat.txt")]
                            [dir-stat  (file-or-directory-stat "/app/stat-dir" #t)])
                        (and (hash-eq? file-stat)
                             (equal? (hash-ref file-stat 'size) 4)
                             (equal? (hash-ref file-stat 'mode) #o600)
                             (equal? (hash-ref file-stat 'modify-time-seconds) 321)
                             (equal? (hash-ref file-stat 'access-time-seconds) 321)
                             (equal? (hash-ref file-stat 'hardlink-count) 1)
                             (equal? (hash-ref dir-stat 'size) 0)
                             (equal? (hash-ref dir-stat 'mode) #o777)
                             (hash-has-key? file-stat 'creation-time-nanoseconds)
                             (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                              (lambda (_ex) #t)])
                               (file-or-directory-stat "/app/data/missing-stat.txt")
                               #f)
                             (with-handlers ([(lambda (ex) (exn:fail:contract? ex))
                                              (lambda (_ex) #t)])
                               (file-or-directory-stat "/app/data/stat.txt" 'not-boolean)
                               #f)))))
              (list "file-or-directory-identity"
                    (begin
                      (webracket-vfs-write-file "/app/data/identity.txt" #"id")
                      (let ([original (file-or-directory-identity "/app/data/identity.txt")]
                            [dir-id   (file-or-directory-identity "/app/data" #t)])
                        (rename-file-or-directory "/app/data/identity.txt"
                                                  "/app/data/identity-renamed.txt")
                        (copy-file "/app/data/identity-renamed.txt"
                                   "/app/data/identity-copy.txt")
                        (and (exact-positive-integer? original)
                             (exact-positive-integer? dir-id)
                             (equal? (file-or-directory-identity "/app/data/identity-renamed.txt")
                                     original)
                             (not (equal? (file-or-directory-identity "/app/data/identity-copy.txt")
                                          original))
                             (equal? (hash-ref (file-or-directory-stat "/app/data/identity-renamed.txt")
                                               'inode)
                                     original)
                             (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                              (lambda (_ex) #t)])
                               (file-or-directory-identity "/app/data/missing-identity.txt")
                               #f)))))
              (list "file-size"
                    (and (equal? (file-size "/app/data/notes.txt") 6)
                         (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                          (lambda (_ex) #t)])
                           (file-size "/app/data/missing.txt")
                           #f)))
              (list "delete-file"
                    (begin
                      (webracket-vfs-write-file "/app/data/delete-me.txt" #"gone")
                      (and (file-exists? "/app/data/delete-me.txt")
                           (void? (delete-file "/app/data/delete-me.txt"))
                           (equal? (file-exists? "/app/data/delete-me.txt") #f)
                           (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                            (lambda (_ex) #t)])
                             (delete-file "/app/data/delete-me.txt")
                             #f)
                           (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                            (lambda (_ex) #t)])
                             (delete-file "/app/data")
                             #f))))
              (list "file->bytes"
                    (and (equal? (file->bytes "/app/data/notes.txt") #"notes\n")
                         (equal? (file->bytes "/app/data/more.txt") #"more")
                         (equal? (file->bytes "/app/data/notes.txt" 'text) #"notes\n")
                         (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                           (file->bytes "/app/data/notes.txt" 'update)
                           #f)
                         (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                          (lambda (_ex) #t)])
                           (file->bytes "/app/data/missing.txt")
                           #f)))
              (list "file->string"
                    (and (equal? (file->string "/app/data/notes.txt") "notes\n")
                         (equal? (file->string "/app/data/more.txt") "more")
                         (equal? (file->string "/app/data/notes.txt" 'text) "notes\n")
                         (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                           (file->string "/app/data/notes.txt" 'update)
                           #f)
                         (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                          (lambda (_ex) #t)])
                           (file->string "/app/data/missing.txt")
                           #f)))
              (list "file->lines"
                    (begin
                      (webracket-vfs-write-file "/app/data/lines.txt" #"a\nb\n")
                      (webracket-vfs-write-file "/app/data/return-lines.txt" #"x\ry")
                      (and (equal? (file->lines "/app/data/lines.txt") '("a" "b"))
                           (equal? (file->lines "/app/data/lines.txt" 'text) '("a" "b"))
                           (equal? (file->lines "/app/data/return-lines.txt" 'binary 'return)
                                   '("x" "y"))
                           (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                             (file->lines "/app/data/lines.txt" 'update)
                             #f)
                           (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                            (lambda (_ex) #t)])
                             (file->lines "/app/data/missing-lines.txt")
                             #f))))
              (list "file->bytes-lines"
                    (begin
                      (webracket-vfs-write-file "/app/data/bytes-lines.txt" #"a\nb\n")
                      (webracket-vfs-write-file "/app/data/bytes-return-lines.txt" #"x\ry")
                      (webracket-vfs-write-file "/app/data/bytes-any-one-lines.txt" #"x\r\ny\r\n")
                      (and (equal? (file->bytes-lines "/app/data/bytes-lines.txt") '(#"a" #"b"))
                           (equal? (file->bytes-lines "/app/data/bytes-lines.txt" 'text) '(#"a" #"b"))
                           (equal? (file->bytes-lines "/app/data/bytes-return-lines.txt" 'binary 'return)
                                   '(#"x" #"y"))
                           (equal? (file->bytes-lines "/app/data/bytes-any-one-lines.txt" 'binary 'any-one)
                                   '(#"x" #"" #"y" #""))
                           (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                             (file->bytes-lines "/app/data/bytes-lines.txt" 'update)
                             #f)
                           (let ([message
                                  (with-handlers ([exn:fail:contract?
                                                   (lambda (exn)
                                                     (exn-message exn))])
                                    (file->bytes-lines "/app/data/bytes-lines.txt"
                                                       'binary
                                                       'bogus)
                                    #f)])
                             (and message
                                  (string-contains? message "file->bytes-lines")
                                  (string-contains?
                                   message
                                   "(or/c 'linefeed 'return 'return-linefeed 'any 'any-one)")
                                  (string-contains? message "'bogus")))
                           (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                            (lambda (_ex) #t)])
                             (file->bytes-lines "/app/data/missing-bytes-lines.txt")
                             #f))))
              (list "open-input-file"
                    (let ([port (open-input-file "/app/data/notes.txt")])
                      (and (input-port? port)
                           (path? (object-name port))
                           (equal? (path->string (object-name port))
                                   "/app/data/notes.txt")
                           (equal? (read-string 6 port) "notes\n")
                           (eof-object? (read-byte port)))))
              (list "open-input-file/mode"
                    (and (let ([port (open-input-file "/app/data/notes.txt" 'text)])
                           (equal? (read-string 6 port) "notes\n"))
                         (let ([port (open-input-file "/app/data/notes.txt" 'binary #t)])
                           (equal? (read-string 6 port) "notes\n"))
                         (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                           (open-input-file "/app/data/notes.txt" 'update)
                           #f)))
              (list "open-input-file/location"
                    (let* ([port (open-input-file "/app/data/notes.txt")]
                           [loc  (lambda ()
                                   (let-values ([(line col pos) (port-next-location port)])
                                     (list line col pos)))])
                      (and (equal? (loc) '(1 0 1))
                           (equal? (read-string 6 port) "notes\n")
                           (equal? (loc) '(2 0 7)))))
              (list "open-input-file/missing"
                    (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                     (lambda (_ex) #t)])
                      (open-input-file "/app/data/missing.txt")
                      #f))
              (list "open-output-file"
                    (let ([port (open-output-file "/app/data/out.txt")])
                      (and (output-port? port)
                           (path? (object-name port))
                           (equal? (path->string (object-name port))
                                   "/app/data/out.txt")
                           (equal? (write-string "out\n" port) 4)
                           (void? (write-byte 65 port))
                           (void? (close-output-port port))
                           (void? (close-output-port port))
                           (equal? (file->bytes "/app/data/out.txt") #"out\nA")
                           (equal? (file-size "/app/data/out.txt") 5)
                           (with-handlers ([exn:fail? (lambda (_ex) #t)])
                             (write-byte 66 port)
                             #f))))
              (list "open-output-file/exists"
                    (begin
                      (webracket-vfs-write-file "/app/data/out-exists.txt" #"old")
                      (and (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                           (lambda (_ex) #t)])
                             (open-output-file "/app/data/out-exists.txt")
                             #f)
                           (let ([port (open-output-file "/app/data/out-exists.txt"
                                                         'binary
                                                         'replace)])
                             (write-string "new" port)
                             (close-output-port port)
                             (equal? (file->string "/app/data/out-exists.txt") "new"))
                           (let ([port (open-output-file "/app/data/out-exists.txt"
                                                         'text
                                                         'append)])
                             (write-string "!" port)
                             (close-output-port port)
                             (equal? (file->string "/app/data/out-exists.txt") "new!"))
                           (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                           (lambda (_ex) #t)])
                             (open-output-file "/app/data/out-missing.txt"
                                               'binary
                                               'must-truncate)
                             #f))))
              (list "display-to-file"
                    (and (void? (display-to-file "hello" "/app/data/display-to-file.txt"))
                         (equal? (file->string "/app/data/display-to-file.txt") "hello")
                         (begin
                           (display-to-file 'sym "/app/data/display-to-file.txt" 'text 'replace)
                           (equal? (file->string "/app/data/display-to-file.txt") "sym"))
                         (begin
                           (display-to-file 42 "/app/data/display-to-file.txt" 'binary 'append)
                           (equal? (file->string "/app/data/display-to-file.txt") "sym42"))
                         (begin
                           (display-to-file #"A\n" "/app/data/display-to-file-bytes.txt")
                           (equal? (file->bytes "/app/data/display-to-file-bytes.txt") #"A\n"))
                         (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                                          (lambda (_ex) #t)])
                           (display-to-file "again" "/app/data/display-to-file.txt")
                           #f)
                         (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                           (display-to-file "bad" "/app/data/display-to-file-bad.txt" 'update)
                           #f)))
              (list "display-lines-to-file"
                    (and (void? (display-lines-to-file '("a" 1 #"b")
                                                       "/app/data/display-lines.txt"))
                         (equal? (file->bytes "/app/data/display-lines.txt")
                                 #"a\n1\nb\n")
                         (begin
                           (display-lines-to-file '(x y)
                                                  "/app/data/display-lines.txt"
                                                  ","
                                                  'text
                                                  'replace)
                           (equal? (file->string "/app/data/display-lines.txt")
                                   "x,y,"))
                         (begin
                           (display-lines-to-file '(z)
                                                  "/app/data/display-lines.txt"
                                                  #"|"
                                                  'binary
                                                  'append)
                           (equal? (file->string "/app/data/display-lines.txt")
                                   "x,y,z|"))
                         (with-handlers ([exn:fail:contract? (lambda (_ex) #t)])
                           (display-lines-to-file 'not-a-list
                                                  "/app/data/display-lines-bad.txt")
                           #f)))
              (list "flush-output/open-output-file"
                    (let ([port (open-output-file "/app/data/flushed.txt")])
                      (and (equal? (write-string "first" port) 5)
                           (void? (flush-output port))
                           (equal? (file->string "/app/data/flushed.txt") "first")
                           (equal? (write-string "!" port) 1)
                           (void? (flush-output port))
                           (equal? (file->string "/app/data/flushed.txt") "first!")
                           (void? (close-output-port port))
                           (equal? (file->string "/app/data/flushed.txt") "first!"))))
              (list "call-with-input-file"
                    (let ([saved-port #f])
                      (let ([result
                             (call-with-input-file "/app/data/notes.txt"
                               (lambda (port)
                                 (set! saved-port port)
                                 (and (input-port? port)
                                      (path? (object-name port))
                                      (equal? (path->string (object-name port))
                                              "/app/data/notes.txt")
                                      (equal? (read-string 6 port) "notes\n")
                                      (eof-object? (read-byte port)))))])
                        (and result
                             (port-closed? saved-port)))))
              (list "call-with-input-file/mode"
                    (equal? (call-with-input-file "/app/data/notes.txt"
                              (lambda (port)
                                (read-string 6 port))
                              'text)
                            "notes\n"))
              (list "call-with-output-file"
                    (let ([saved-port #f])
                      (let ([result
                             (call-with-output-file "/app/data/call-out.txt"
                               (lambda (port)
                                 (set! saved-port port)
                                 (and (output-port? port)
                                      (equal? (write-string "called" port) 6)
                                      'done)))])
                        (and (equal? result 'done)
                             (output-port? saved-port)
                             (port-closed? saved-port)
                             (equal? (file->string "/app/data/call-out.txt")
                                     "called")))))
              (list "call-with-output-file/exists"
                    (begin
                      (webracket-vfs-write-file "/app/data/call-out-exists.txt" #"old")
                      (and (equal? (call-with-output-file "/app/data/call-out-exists.txt"
                                     (lambda (port)
                                       (write-string "new" port)
                                       'done)
                                     'binary
                                     'replace)
                                   'done)
                           (equal? (file->string "/app/data/call-out-exists.txt")
                                   "new")
                           (equal? (call-with-output-file "/app/data/call-out-exists.txt"
                                     (lambda (port)
                                       (write-string "!" port)
                                       'appended)
                                     'binary
                                     'append)
                                   'appended)
                           (equal? (file->string "/app/data/call-out-exists.txt")
                                   "new!"))))
              (list "call-with-input-file*"
                    (let ([saved-port #f])
                      (let ([result
                             (call-with-input-file* "/app/data/notes.txt"
                               (lambda (port)
                                 (set! saved-port port)
                                 (read-string 6 port)))])
                        (and (equal? result "notes\n")
                             (input-port? saved-port)
                             (port-closed? saved-port)))))
              (list "call-with-input-file*/mode"
                    (equal? (call-with-input-file* "/app/data/notes.txt"
                              (lambda (port)
                                (read-string 6 port))
                              'text)
                            "notes\n"))
              (list "call-with-input-file*/exception"
                    (let ([saved-port #f])
                      (and (with-handlers ([symbol? (lambda (_ex) #t)])
                             (call-with-input-file* "/app/data/notes.txt"
                               (lambda (port)
                                 (set! saved-port port)
                                 (raise 'boom)))
                             #f)
                           (input-port? saved-port)
                           (port-closed? saved-port))))
              (list "call-with-output-file*"
                    (let ([saved-port #f])
                      (let ([result
                             (call-with-output-file* "/app/data/call-star-out.txt"
                               (lambda (port)
                                 (set! saved-port port)
                                 (write-string "star" port)
                                 'done))])
                        (and (equal? result 'done)
                             (output-port? saved-port)
                             (port-closed? saved-port)
                             (equal? (file->string "/app/data/call-star-out.txt")
                                     "star")))))
              (list "call-with-output-file*/exception"
                    (let ([saved-port #f])
                      (and (with-handlers ([symbol? (lambda (_ex) #t)])
                             (call-with-output-file* "/app/data/call-star-error.txt"
                               (lambda (port)
                                 (set! saved-port port)
                                 (write-string "star-error" port)
                                 (raise 'boom)))
                             #f)
                           (output-port? saved-port)
                           (port-closed? saved-port)
                           (equal? (file->string "/app/data/call-star-error.txt")
                                   "star-error"))))
              (list "call-with-output-file*/exists"
                    (begin
                      (webracket-vfs-write-file "/app/data/call-star-exists.txt" #"old")
                      (call-with-output-file* "/app/data/call-star-exists.txt"
                        (lambda (port)
                          (write-string "star-new" port))
                        'binary
                        'truncate)
                      (equal? (file->string "/app/data/call-star-exists.txt")
                              "star-new")))
              (list "with-input-from-file"
                    (let* ([original (current-input-port)]
                           [during   #f]
                           [result   (with-input-from-file "/app/data/notes.txt"
                                       (lambda ()
                                         (set! during (current-input-port))
                                         (read-string 6)))])
                      (and (equal? result "notes\n")
                           (input-port? during)
                           (port-closed? during)
                           (eq? (current-input-port) original))))
              (list "with-input-from-file/mode"
                    (equal? (with-input-from-file "/app/data/notes.txt"
                              (lambda ()
                                (read-string 6))
                              'text)
                            "notes\n"))
              (list "with-input-from-file/exception"
                    (let ([original (current-input-port)]
                          [during   #f])
                      (and (with-handlers ([symbol? (lambda (_ex) #t)])
                             (with-input-from-file "/app/data/notes.txt"
                               (lambda ()
                                 (set! during (current-input-port))
                                 (raise 'boom)))
                             #f)
                           (input-port? during)
                           (port-closed? during)
                           (eq? (current-input-port) original))))
              (list "with-output-to-file"
                    (let* ([original (current-output-port)]
                           [during   #f]
                           [result   (with-output-to-file "/app/data/with-out.txt"
                                       (lambda ()
                                         (set! during (current-output-port))
                                         (write-string "with")
                                         'ok))])
                      (and (equal? result 'ok)
                           (output-port? during)
                           (port-closed? during)
                           (eq? (current-output-port) original)
                           (equal? (file->string "/app/data/with-out.txt")
                                   "with"))))
              (list "with-output-to-file/exists"
                    (begin
                      (webracket-vfs-write-file "/app/data/with-out-exists.txt" #"old")
                      (and (equal? (with-output-to-file "/app/data/with-out-exists.txt"
                                     (lambda ()
                                       (write-string "with-new")
                                       'ok)
                                     'binary
                                     'replace)
                                   'ok)
                           (equal? (file->string "/app/data/with-out-exists.txt")
                                   "with-new"))))
              (list "with-output-to-file/exception"
                    (let ([original (current-output-port)]
                          [during   #f])
                      (and (with-handlers ([symbol? (lambda (_ex) #t)])
                             (with-output-to-file "/app/data/with-error.txt"
                               (lambda ()
                                 (set! during (current-output-port))
                                 (write-string "saved")
                                 (raise 'boom)))
                             #f)
                           (output-port? during)
                           (port-closed? during)
                           (eq? (current-output-port) original)
                           (equal? (file->string "/app/data/with-error.txt")
                                   "saved")))))))

 (list "Linklets"
       (list
        (list "compile-linklet import-key protocol"
              (and (let-values ([(linklet import-keys)
                                 (compile-linklet 'placeholder #f '#(a b))])
                     (and (eq? linklet #f)
                          (equal? import-keys '#(a b))))
                   (eq? (compile-linklet 'placeholder #f #f) #f)
                   (let ([compiled (compile-linklet '(linklet ((x)) (y) (void)) #f #f)])
                     (and (linklet? compiled)
                          (equal? (linklet-import-variables compiled) '((x)))
                          (equal? (linklet-export-variables compiled) '(y))))
                   (let ([compiled (compile-linklet '(linklet () () (void)) #f #f)])
                     (linklet? compiled))))
        (list "recompile-linklet import-key protocol"
              (and (let-values ([(linklet import-keys)
                                 (recompile-linklet 'compiled #f '#(k))])
                     (and (equal? linklet 'compiled)
                          (equal? import-keys '#(k))))
                   (equal? (recompile-linklet 'compiled #f #f)
                           'compiled)))))

 (list "Checkers"
       (list
        (list "check-list"          (equal? (begin (check-list '(1 2 3)) 'ok)            'ok))
        (list "check-list with who" (equal? (begin (check-list 'remove-duplicates '(1 2 3)) 'ok) 'ok))
        (list "check-mlist"         (equal? (begin (check-mlist '()) 'ok)                'ok))
        (list "check-range"         (equal? (begin (check-range 0 5 1) 'ok)              'ok))
        (list "check-range-generic" (equal? (begin (check-range-generic 'who 0 1 1) 'ok) 'ok))
        (list "check-naturals"      (equal? (begin (check-naturals 5) 'ok)               'ok))
        (list "check-naturals bignum"
              (equal? (begin (check-naturals 536870912) 'ok) 'ok))))

 (list "FFI"
       (list
        #;(list "external-number->flonum"
                (let ([e (js-eval "new Number(42)")])
                  (equal? (external-number->flonum e) 42.0))))
 )
