#lang webracket

(define (factorial n)
  (let loop ([n n]
             [a 1])
    (if (zero? n)
        a
        (loop (sub1 n) (* n a)))))

(define cc0 (compose factorial sqr))
(define cc1 (compose1 factorial sqr))
(define cc2 (compose sqr factorial))
(define cc3 (compose1 sqr factorial))

(unless (and (equal? (cc0 3) 362880)
             (equal? (cc1 3) 362880)
             (equal? (cc2 3) 36)
             (equal? (cc3 3) 36)
             (equal? (call-with-values (lambda () ((compose) 1 2)) list) '(1 2))
             (eq? (compose factorial) factorial)
             (equal? (call-with-values
                      (lambda () ((compose values (lambda (x) (values x (+ x 1)))) 3))
                      list)
                     '(3 4)))
  (error 'compose-basic "failed"))
