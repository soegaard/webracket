#lang webracket
(include-lib define)

(define (id . xs)
  xs)

(call/key id 1 #:a)
