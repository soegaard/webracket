#lang webracket

(define (caught? thunk)
  (with-handlers ([(lambda (ex) #t) (lambda (ex) #t)])
    (thunk)
    #f))

(unless (and (caught? (lambda () (list-prefix? #t '())))
             (caught? (lambda () (list-prefix? '() #t)))
             (caught? (lambda () (take-common-prefix 1 1))))
  (error 'list-prefix-invalid-list "failed"))
