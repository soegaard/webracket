#lang webracket
(include/reader "../../define.rkt" read-syntax/skip-first-line)

(define/key (bad-arity x y #:a [a 1])
  (+ x y a))

(bad-arity 1)
