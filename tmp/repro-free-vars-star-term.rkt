;#lang webracket
(include/reader "tmp/repro-free-vars-helper.rkt" read-syntax/skip-first-line)

(define (f)
  (define term 1)
  (set! *term* term)
  0)

(f)
