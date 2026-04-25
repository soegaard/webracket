#lang webracket

(with-handlers ([(lambda (ex) #t) (lambda (ex) (void))])
  (make-list -3 'x)
  (error 'make-list-negative "negative count did not fail"))
