#lang webracket
(include-lib define)

(define/key (bad-unknown x #:a [a 1])
  (+ x a))

(bad-unknown 1 #:unknown 2)
