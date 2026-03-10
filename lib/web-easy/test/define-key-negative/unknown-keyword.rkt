#lang webracket
(include/reader "../../define.rkt" read-syntax/skip-first-line)

(define/key (bad-unknown x #:a [a 1])
  (+ x a))

(bad-unknown 1 #:unknown 2)
