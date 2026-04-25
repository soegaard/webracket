#lang webracket

(define (test expect fun . args)
  (equal? expect (apply fun args)))

(unless (and (test '(0 1 2 3) range 4)
             (test '() range 0)
             (test '(10 11.5 13.0 14.5) range 10 15 1.5))
  (error 'range-first-class-one-arg "failed"))
