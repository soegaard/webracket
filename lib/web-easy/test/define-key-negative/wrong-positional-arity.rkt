#lang webracket
(require-lib define)

(define/key (bad-arity x y #:a [a 1])
  (+ x y a))

(bad-arity 1)
