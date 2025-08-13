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
        (equal? (procedure-arity equal?) 2)))

 ; todo - implement equal-always?
 #;"equal-always?"
 #;(and (equal? (equal-always? 'a 'a) #t)
        (equal? (equal-always? '("a") '("a")) #t)
        (equal? (equal-always? '(a) '(a)) #t)
        (equal? (equal-always? '(a (b) c) '(a (b) c)) #t)
        (equal? (equal-always? '("a" ("b") "c") '("a" ("b") "c")) #t)
        (equal? (equal-always? "abc" "abc") #t)
        (equal? (equal-always? 2 2) #t)
        ;; immutable versions are equal-always
        (equal? (equal-always? (vector-immutable 5 'a)
                               (vector-immutable 5 'a)) #t)
        (equal? (equal-always? (box-immutable "a")
                               (box-immutable "a")) #t)
        (equal? (equal-always? (hash 'a 1) (hash 'a 1)) #t)
        (equal? (equal-always? (cons 'a '()) (cons 'a '())) #t)
        (equal? (equal-always?
                 (string->immutable-string (string #\a))
                 (string->immutable-string (string #\a))) #t)
        (equal? (equal-always? "" (string #\null)) #f)
        ;; mutable versions are not equal-always
        (equal? (equal-always? (make-vector 5 'a) (make-vector 5 'a)) #f)
        (equal? (equal-always? (box "a") (box "a")) #f)
        (equal? (equal-always? (make-hash '((a . 1)))
                               (make-hash '((a . 1)))) #f)
        (equal? (equal-always? (mcons 'a '()) (mcons 'a '())) #f)
        (equal? (equal-always? (string #\a) (string #\a)) #f)
        (equal? (equal-always? (make-flvector 5 0.0)
                               (make-flvector 5 0.0)) #f)
        (equal? (equal-always? (make-fxvector 5 0)
                               (make-fxvector 5 0)) #f)
        (equal? (equal-always? (stencil-vector #b10010 'a 'b)
                               (stencil-vector #b10010 'a 'b)) #f))

 ;; "equal-always-hash-code / eq?"
 ;; (and (equal? (eq? (equal-always-hash-code (make-flvector 5 0.0))
 ;;                   (equal-always-hash-code (make-flvector 5 0.0))) #f)
 ;;      (equal? (eq? (equal-always-hash-code (make-fxvector 5 0))
 ;;                   (equal-always-hash-code (make-fxvector 5 0))) #f)
 ;;      (equal? (eq? (equal-always-hash-code
 ;;                    (stencil-vector #b10010 'a 'b))
 ;;                   (equal-always-hash-code
 ;;                    (stencil-vector #b10010 'a 'b))) #f))

 #;(let ()
     (struct s (x) #:property prop:procedure 0)
     (list "equal-always?/recur"
           (and (equal? (equal-always?/recur '(1 . 2) '(1 . 2)
                                             (s (lambda (x y) #t))) #t)
                (equal? (equal-always?/recur '#(0) '#(0)
                                             (s (lambda (x y) #t))) #t)
                (equal? (equal-always?/recur '#&0 '#&0
                                             (s (lambda (x y) #t))) #t))))

 ; todo - implement mcons and friends
 
 ;; (test '(a b c d e) 'dot '(a . (b . (c . (d . (e . ()))))))
 ;; (define x (mcons 'a (mcons 'b (mcons 'c null))))
 ;; (define y x)
 ;; (set-mcdr! x 4)
 ;; (test (mcons 'a 4) 'set-mcdr! x)
 ;; (set-mcar! x 'z)
 ;; (test (mcons 'z 4) 'set-mcar! x)
 ;; (test #t eqv? x y)
 ;; (test '(a b c . d) 'dot '(a . (b . (c . d))))
 ;; (test #f list? y)
 ;; (test #f list? (cons 'a 4))
 ;; (arity-test list? 1 1)


 (list
  "append"
  (and (equal? (append '(x) '(y))               '(x y))
       (equal? (append '(a) '(b c d))           '(a b c d))
       (equal? (append '(a (b)) '((c)))         '(a (b) (c)))
       #;(equal? (append)                         '())            ; todo - only two argument append for now
       (equal? (append '(a b) '(c . d))         '(a b c . d))
       (equal? (append '() 'a)                  'a)
       #; (equal? (append 1)                       1)             ; todo - ditto
       (equal? (append '(1) 2)                  '(1 . 2))
       (equal? (append '(1) 2)                  '(1 . 2))
       
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

 )
