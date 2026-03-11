#lang webracket
(include/reader "../../define.rkt" read-syntax/skip-first-line)

(define/key (bad-required x #:k k)
  (+ x k))

(bad-required 1)
