#lang webracket
(require-lib define)

(define/key (bad-missing x #:a [a 1])
  (+ x a))

(bad-missing 1 #:a)
