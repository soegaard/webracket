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

 (list "number->string"
       (and (equal? (number->string 42) "42")
            (equal? (number->string 16 16) "10")
            (equal? (number->string 1.25) "1.25")
            (equal? (number->string 1.0) "1.0")))

 (list "string-append"
       (and (equal? (string-append) "")
            (equal? (string-append "A") "A")
            (equal? (string-append "A" "B") "AB")
            (equal? (string-append "A" "B" "C") "ABC")))

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

(list "list*"
      (and (equal? (list* 1 2 3) (cons 1 (cons 2 3)))
           (equal? (list* 1 2 (list 3 4)) '(1 2 3 4))))

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
            
            ; todo - this one is peculiar
            ; (comp+ (equal? (procedure-arity memq) 2)) works
            ; but not as part in this file
            #; (equal? (procedure-arity memq) 2)))

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
              #;(equal? (immutable? (symbol->immutable-string 'apple)) #t)    ; todo - implement immutable?
              #;(equal? (immutable? (symbol->immutable-string 'box))   #t))))

 (list "symbol<?"
       (and (equal? (symbol<? 'a 'b)    #t)
            ;(equal? (symbol<? 'a 'b 'c) #t)  ; todo - make it variadic
            ;(equal? (symbol<? 'a 'c 'b) #f)
            (equal? (symbol<? 'a 'aa)   #t)
            (equal? (symbol<? 'aa 'a)   #f)
            #;(equal? (procedure-arity symbol<?) 1)))

 (list "keyword?"
       (and (equal? (keyword? '#:a) #t)
            (equal? (keyword? 'a) #f)
            (equal? (string->keyword "apple") '#:apple)
            (equal? (keyword->string '#:apple) "apple")
            ;; keyword->string returns fresh strings (not eq?)
            #;(equal? (eq? (keyword->string '#:apple)
                         (keyword->string '#:apple))
                    #f)
            #;(equal? (keyword->immutable-string '#:apple) "apple")            ; todo - implement keyword->immutable-string
            #;(equal? (immutable? (keyword->immutable-string '#:apple)) #t)

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

            #;(equal? (procedure-arity keyword<?) 2)))

 (list "case-lambda"
       (list (equal? (procedure? (case-lambda)) #t)
            (equal? (procedure? (case-lambda [(x) x])) #t)
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
            (equal? (procedure-arity (case-lambda [(x) x] [(x . y) x])) '(1 -2))))
 )
