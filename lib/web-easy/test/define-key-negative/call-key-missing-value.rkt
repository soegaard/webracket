#lang webracket
(require-lib define)

(define (id . xs)
  xs)

(call/key id 1 #:a)
