#lang racket/base

(require racket/contract/base)

(provide
  simple-pretty-print
  simple-pretty-write
  simple-pretty-display
  simple-pretty-format
  default-pretty-options
  (for-label (all-defined-out)))

(define simple-pretty-print any/c)
(define simple-pretty-write any/c)
(define simple-pretty-display any/c)
(define simple-pretty-format any/c)
(define default-pretty-options any/c)
