#lang racket/base
(provide fasl-fixnum
         fasl-character
         fasl-symbol
         fasl-string
         fasl-bytes
         fasl-boolean
         fasl-null
         fasl-pair
         fasl-vector
         fasl-flonum
         fasl-void
         fasl-eof)

(define fasl-fixnum     0)
(define fasl-character  1)
(define fasl-symbol     2)
(define fasl-string     3)
(define fasl-bytes      4)
(define fasl-boolean    5)
(define fasl-null       6)
(define fasl-pair       7)
(define fasl-vector     8)
(define fasl-flonum     9)
(define fasl-void      10)
(define fasl-eof       11)

