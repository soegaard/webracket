#lang racket/base

(require racket/contract/base
         "dom-family-labels.rkt")

(provide
  (all-from-out "dom-family-labels.rkt")
  window-scroll-by)

(define window-scroll-by any/c)
