#lang racket/base

(require racket/contract/base)

;; Docs-only fake bindings for raw bridge accessors.

(provide
  window-raw
  document-raw
  text-raw
  node-raw
  attr-raw
  selection-raw
  element-raw
  dom-token-list-raw
  shadow-root-raw
  node-list-raw
  html-collection-raw
  dom-rect-list-raw
  computed-style-map-raw
  animation-raw
  iterator-raw
  (for-label (all-defined-out)))

(define window-raw any/c)
(define document-raw any/c)
(define text-raw any/c)
(define node-raw any/c)
(define attr-raw any/c)
(define selection-raw any/c)
(define element-raw any/c)
(define dom-token-list-raw any/c)
(define shadow-root-raw any/c)
(define node-list-raw any/c)
(define html-collection-raw any/c)
(define dom-rect-list-raw any/c)
(define computed-style-map-raw any/c)
(define animation-raw any/c)
(define iterator-raw any/c)
