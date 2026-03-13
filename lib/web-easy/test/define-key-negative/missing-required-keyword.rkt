#lang webracket
(include-lib define)

(define/key (bad-required x #:k k)
  (+ x k))

(bad-required 1)
