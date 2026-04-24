#lang webracket

(define (test expect fun . args)
  (equal? expect (apply fun args)))

(unless (and (test #f index-of '() 'a)
             (test 0 index-of '(a b c) 'a)
             (test 1 index-of '(a b c) 'b)
             (test #f index-of '(a b) 'c))
  (error 'index-of-first-class "failed"))
