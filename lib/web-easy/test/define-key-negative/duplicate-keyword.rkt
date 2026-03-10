#lang webracket
(include/reader "../../define.rkt" read-syntax/skip-first-line)

(define/key (bad-dup x #:a [a 1])
  (+ x a))

(bad-dup 1 #:a 2 #:a 3)
