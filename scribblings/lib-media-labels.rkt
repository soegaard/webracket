#lang racket/base

(require racket/contract/base
         "dom-family-labels.rkt")

(provide
  (all-from-out "dom-family-labels.rkt")
  media-set-current-time!)

(define media-set-current-time! any/c)
