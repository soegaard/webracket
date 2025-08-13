(list "not"
      (and (equal? (not #t)       #f)
           (equal? (not 3)        #f)
           (equal? (not (list 3)) #f)
           (equal? (not #f)       #t)
           (equal? (not '())      #f)
           (equal? (not (list))   #f)
           (equal? (not 'nil)     #f)
           (equal? (procedure-arity not) 1))

      "boolean?"
      (and (equal? (boolean? #f)  #t)
           (equal? (boolean? #t)  #t)
           (equal? (boolean? 0)   #f)
           (equal? (boolean? '()) #f)
           (equal? (procedure-arity boolean?) 1))

      "eqv?"
      (and (equal? (eqv? 'a 'a) #t)
           (equal? (eqv? 'a 'b) #f)
           (equal? (eqv? 2 2) #t)
           (equal? (eqv? 2 2.0) #f)
           (equal? (eqv? '() '()) #t)
           (equal? (eqv? '10000 '10000) #t)
           (equal? (eqv? 10000000000000000000 10000000000000000000) #t)
           #;(equal? (eqv? 10000000000000000000 10000000000000000001) #f) ; needs bignums
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
           (equal? (procedure-arity eqv?) 2))

      "eq?"
      (and (equal? (eq? 'a 'a)               #t)
           (equal? (eq? (list 'a) (list 'a)) #f)
           (equal? (eq? '() '())             #t)
           (equal? (eq? car car)             #t)
           (let ((x '(a))) (equal? (eq? x x) #t))
           (let ((x '#())) (equal? (eq? x x) #t))
           (let ((x (lambda (x) x)))
             (equal? (eq? x x) #t))
           (equal? (procedure-arity eq?) 2))

      "equal?"
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
       (equal? (procedure-arity equal?) 2))


      )
