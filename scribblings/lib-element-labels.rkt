#lang racket/base

(require racket/contract/base
         "dom-family-labels.rkt")

(provide
  (all-from-out "dom-family-labels.rkt")
  element-has-attributes?
  element-query-selector-all
  element-set-outer-html!)

(define element-has-attributes? any/c)
(define element-query-selector-all any/c)
(define element-set-outer-html! any/c)
