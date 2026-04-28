;; Focused letrec/letrec-values regression suite.
;;
;; Keep this file in the same result-tree style as `test-basics.rkt`,
;; so it can be compiled and inspected the same way.
;;
;; Compile this file with `--no-stdlib`.

(list
 (list "3. Syntactic Forms"
       (list "3.x Recursive Bindings"
             (list
              (list "letrec basics"
                    (and
                     (equal? (letrec () 12) 12)
                     (equal? (letrec ([x 12]) x) 12)
                     (equal? (letrec ([x 12] [y 13]) (+ x y)) 25)))
              (list "letrec recursive procedures"
                    (and
                     (equal? (letrec ([fact (lambda (n)
                                              (if (= n 0)
                                                  1
                                                  (* n (fact (- n 1)))))])
                               (fact 5))
                             120)
                     (equal? (letrec ([even?
                                       (lambda (n)
                                         (if (= n 0) #t (odd? (- n 1))))]
                                      [odd?
                                       (lambda (n)
                                         (if (= n 0) #f (even? (- n 1))))])
                               (list (even? 10) (odd? 11)))
                             '(#t #t))))
              (list "letrec mixed simple and lambda bindings"
                    (and
                     (equal? (letrec ([x 12]
                                      [f (lambda () x)])
                               (f))
                             12)
                     (equal? (letrec ([f (lambda () 1)])
                               'ok)
                             'ok)
                     (equal? (letrec ([f (lambda () (f))])
                               'ok)
                             'ok)
                     (equal? (letrec ([f (case-lambda [() 1])])
                               'ok)
                             'ok)
                     (equal? (letrec ([f (case-lambda [() (f)])])
                               'ok)
                             'ok)
                     (equal? (letrec ([x 12]
                                      [f (lambda (n) (set! x n))]
                                      [g (lambda () x)])
                               (f 99)
                               (g))
                             99)))
              (list "letrec order-sensitive initialization"
                    (equal? (let ([events '()])
                              (letrec ([x (begin (set! events (cons 'x events))
                                                 12)]
                                       [y (begin (set! events (cons 'y events))
                                                 (+ x 1))]
                                       [z (begin (set! events (cons 'z events))
                                                 (+ y 1))])
                                 (list z (reverse events))))
                            '(14 (x y z))))
              (list "letrec pure begin0"
                    (and
                     (equal? (letrec ([x (begin0 12 13 14)]) x)
                             12)
                     (equal? (letrec ([x (begin0 (if #t 7 8)
                                                   (begin 1 2)
                                                   (+ 3 4))])
                               x)
                             7)))
              (list "letrec nested forms"
                    (and
                     (equal? (letrec ([f (letrec ([g (lambda (x) (* x 2))])
                                           (lambda (n) (g n)))])
                               (f 6))
                             12)
                     (equal? (letrec ([f (let-values ([(a) 10])
                                        (lambda () a))])
                               (f))
                             10)
                     (equal? (letrec ([x (let-values ([(a) (box 10)])
                                         a)])
                               (unbox x))
                             10)
                     (equal? (letrec ([x (letrec ([f (lambda () 1)])
                                           (cons (f) 2))])
                               x)
                             '(1 . 2))))
              (list "letrec constructor and self reference"
                    (equal? (letrec ([x (cons (lambda () x) '())])
                              (and (pair? x)
                                   (procedure? (car x))
                                   (eq? ((car x)) x)))
                            #t))
              (list "letrec assignment to recursive variable"
                    (equal? (letrec ([f (lambda (g)
                                          (set! f g)
                                          (f))])
                              (f (lambda () 12)))
                            12))
              (list "letrec constructor boundaries"
                    (and
                     (equal? (letrec ([x (box 12)])
                               (unbox x))
                             12)
                     (equal? (letrec ([x (box-immutable 13)])
                               (unbox x))
                             13)
                     (equal? (letrec ([x (make-hasheq)])
                               (hash-set! x 'a 14)
                               (hash-ref x 'a))
                             14)
                     (equal? (let ([events '()])
                               (letrec ([x (begin (set! events (cons 'x events))
                                                  (box 21))]
                                        [y (begin (set! events (cons 'y events))
                                                  (unbox x))])
                                 (list y (reverse events))))
                             '(21 (x y)))))
              (list "letrec allocation identity"
                    (and
                     (equal? (letrec ([x (box 12)]
                                      [y (box 12)])
                               (eq? x y))
                             #f)
                     (equal? (letrec ([x (list 1 2)]
                                      [y x])
                               (eq? x y))
                             #t)
                     (equal? (letrec ([x (vector 1)]
                                      [y (box x)])
                               (eq? (unbox y) x))
                             #t)))
              (list "letrec accessor boundaries"
                    (and
                     (equal? (letrec ([x (cons 1 2)]
                                      [y (car x)])
                               y)
                             1)
                     (equal? (let ([events '()])
                               (letrec ([x (begin (set! events (cons 'x events))
                                                  (cons 11 12))]
                                        [y (begin (set! events (cons 'y events))
                                                  (car x))]
                                        [z (begin (set! events (cons 'z events))
                                                  (cdr x))])
                                 (list y z (reverse events))))
                             '(11 12 (x y z)))))
              (list "letrec allocating chains"
                    (and
                     (equal? (let ([events '()])
                               (letrec ([x (begin (set! events (cons 'x events))
                                                  (list 1 2 3))]
                                        [y (begin (set! events (cons 'y events))
                                                  (vector (car x) (cadr x)))]
                                        [z (begin (set! events (cons 'z events))
                                                  (+ (vector-ref y 0)
                                                     (vector-ref y 1)))])
                                 (list z (reverse events))))
                             '(3 (x y z)))
                     (equal? (let ([events '()])
                               (letrec ([x (begin (set! events (cons 'x events))
                                                  (box 30))]
                                        [y (begin (set! events (cons 'y events))
                                                  (list (unbox x) (unbox x)))]
                                        [z (begin (set! events (cons 'z events))
                                                  (+ (car y) (cadr y)))])
                                 (list z (reverse events))))
                             '(60 (x y z)))))
              (list "letrec allocation before failing read"
                    (equal? (with-handlers ([exn? (lambda (_ex) 'raised)])
                              (let ([events '()])
                                (letrec ([x (begin (set! events (cons 'x events))
                                                   (vector 1 2))]
                                         [y (begin (set! events (cons 'y events))
                                                   (car 17))]
                                         [z (begin (set! events (cons 'z events))
                                                   (vector-ref x 0))])
                                  (list z (reverse events)))))
                            'raised))
              (list "letrec exception order"
                    (and
                     (equal? (let ([events '()])
                               (with-handlers ([exn? (lambda (_ex)
                                                       (reverse events))])
                                 (letrec ([x (begin (set! events (cons 'x events))
                                                    (box 1))]
                                          [y (begin (set! events (cons 'y events))
                                                    (car 17))]
                                          [z (begin (set! events (cons 'z events))
                                                    (unbox x))])
                                   z)))
                             '(x y))
                     (equal? (let ([events '()])
                               (with-handlers ([exn? (lambda (_ex)
                                                       (list 'raised (reverse events)))])
                                 (letrec-values ([(x) (begin (set! events (cons 'x events))
                                                             (vector 1 2))]
                                                 [(y) (begin (set! events (cons 'y events))
                                                             (car 17))])
                                   x)))
                             '(raised (x y)))))
              (list "letrec-values basics"
                    (and
                     (equal? (letrec-values ([(x) 1]) x) 1)
                     (equal? (letrec-values ([(x y) (values 1 2)])
                               'ok)
                             'ok)
                     (equal? (letrec-values ([(x y) (values 1 2)])
                               (+ x y))
                             3)
                     (equal? (letrec-values ([(x y z) (values 1 2 3)])
                               (+ x y z))
                             6)
                     (equal? (letrec-values ([(p q) (begin 0 (values 1 2))])
                               (+ p q))
                             3)
                     (equal? (letrec-values ([(p q) (if #t
                                                        (values 1 2)
                                                        (values 3 4))])
                               (+ p q))
                             3)
                     (equal? (letrec-values ([(p q) (begin0 (values 1 2) 9)])
                               (+ p q))
                             3)
                     (equal? (letrec-values ([(f g) (values (lambda () 1)
                                                            (lambda () 2))])
                               (list (f) (g)))
                             '(1 2))
                     (equal? (letrec-values ([(f g) (values (case-lambda
                                                              [() 1])
                                                             (case-lambda
                                                               [() 2]))])
                               (list (f) (g)))
                             '(1 2))
                     (equal? (letrec-values ([(u v) (let-values ([(a b) (values 1 2)])
                                                      (values a b))])
                               (+ u v))
                             3)
                     (equal? (letrec-values ([(f) (lambda () 11)])
                               (f))
                             11)))
              (list "letrec-values dead bindings"
                    (and
                     (equal? (let ([events '()])
                               (letrec-values ([(x y) (begin (set! events (cons 'xy events))
                                                             (values 1 2))])
                                 (list 'ok (reverse events))))
                             '(ok (xy)))
                     (equal? (letrec-values ([(f g) (values (lambda () 1)
                                                            (lambda () 2))])
                               'ok)
                             'ok)
                     (equal? (letrec-values ([(f g) (values (case-lambda
                                                              [() 1])
                                                             (case-lambda
                                                               [() 2]))])
                               'ok)
                             'ok)
                     (equal? (letrec ([f (case-lambda [() (g)])]
                                      [g (case-lambda [() (f)])])
                               'ok)
                             'ok)
                     (equal? (letrec-values ([(f g) (values (case-lambda
                                                              [() (g)])
                                                             (case-lambda
                                                               [() (f)]))])
                               'ok)
                             'ok)
                     (equal? (letrec ([f (lambda () (g))]
                                      [g (lambda () (f))])
                               'ok)
                             'ok)
                     (equal? (letrec-values ([(f g) (values (lambda () (g))
                                                            (lambda () (f)))])
                               'ok)
                             'ok)
                     (equal? (let ([events '()])
                               (letrec-values ([(x y) (values (begin (set! events (cons 'x events))
                                                                    1)
                                                              (begin (set! events (cons 'y events))
                                                                    2))])
                                 (list 'ok (reverse events))))
                             '(ok (x y)))))
              (list "letrec-values mixed recursive references"
                    (and
                     (equal? (letrec-values ([(x) 12]
                                             [(f) (lambda () x)])
                               (f))
                             12)
                     (equal? (letrec-values ([(f) (lambda () g)]
                                             [(g) 11])
                               (f))
                             11)
                     (equal? (letrec-values ([(f g) (letrec-values ([(a b) (values (lambda () 1)
                                                                                      (lambda () 2))])
                                                       (values a b))])
                               (list (f) (g)))
                             '(1 2))
                     (equal? (letrec-values ([(f g) (letrec-values ([(a b) (values (case-lambda
                                                                                       [() 1])
                                                                                      (case-lambda
                                                                                        [() 2]))])
                                                        (values a b))])
                               (list (f) (g)))
                             '(1 2))
                     (equal? (letrec-values ([(f) (begin 0 (lambda () 1))])
                               (f))
                             1)))
              (list "letrec-values order-sensitive initialization"
                    (equal? (let ([events '()])
                              (letrec-values ([(x) (begin (set! events (cons 'x events))
                                                          5)]
                                              [(y) (begin (set! events (cons 'y events))
                                                          (+ x 7))])
                                (list y (reverse events))))
                            '(12 (x y))))
              (list "letrec-values mixed allocation and identity"
                    (and
                     (equal? (letrec-values ([(bx) (box 7)]
                                             [(by) bx])
                               (eq? bx by))
                             #t)
                     (equal? (letrec-values ([(mx my) (values (box 7) 9)])
                               (list (unbox mx) my))
                             '(7 9))
                     (equal? (letrec-values ([(u v) (let-values ([(a b) (values (box 7) 9)])
                                                      (values a b))])
                               (list (unbox u) v))
                             '(7 9))
                     (equal? (letrec-values ([(x) (box 7)]
                                             [(y) (box 7)])
                               (eq? x y))
                             #f)
                     (equal? (let ([events '()])
                               (letrec-values ([(x) (begin (set! events (cons 'x events))
                                                           (list 1 2))]
                                               [(y) (begin (set! events (cons 'y events))
                                                           x)]
                                               [(z) (begin (set! events (cons 'z events))
                                                           (eq? x y))])
                                 (list z (reverse events))))
                             '(#t (x y z)))))
              ))))
