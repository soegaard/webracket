#lang webracket

(define (caught? thunk)
  (with-handlers ([(lambda (ex) #t) (lambda (ex) #t)])
    (thunk)
    #f))

(unless (and (caught? (lambda () (list-set '(zero one two) 3 "two")))
             (caught? (lambda () (list-set '(zero one two) #f "two")))
             (caught? (lambda () (list-set '(zero one two) -1 "two")))
             (caught? (lambda () (list-update '(zero one two) 3 symbol->string)))
             (caught? (lambda () (list-update '(zero one two) #f symbol->string)))
             (caught? (lambda () (list-update '(zero one two) -1 symbol->string))))
  (error 'list-set-update-catchable "failed"))
