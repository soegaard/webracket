#lang racket/base

(require racket/contract/base)

(provide
  Document
  (struct-out document)
  (struct-out node)
  (struct-out text)
  (struct-out attr)
  (struct-out selection)
  document-head
  document-body
  document-element
  document-create-element
  document-create-attribute
  document-create-attribute-ns
  document-create-text-node
  document-create-comment
  document-create-cdata-section
  document-create-document-fragment
  document-create-processing-instruction
  document-adopt-node
  document-get-element-by-id
  document-query-selector
  document-query-selector-all
  document-has-focus?
  document-get-selection
  document-close
  document-open
  document-element-from-point
  document-elements-from-point
  selection-range-count
  selection-is-collapsed?
  selection-anchor-node
  selection-focus-node
  selection-to-string
  selection-remove-all-ranges!
  (for-label (all-defined-out)))

(define Document any/c)

(struct document (raw) #:transparent)
(struct node (raw) #:transparent)
(struct text (raw) #:transparent)
(struct attr (raw) #:transparent)
(struct selection (raw) #:transparent)

(define document-head any/c)
(define document-body any/c)
(define document-element any/c)
(define document-create-element any/c)
(define document-create-attribute any/c)
(define document-create-attribute-ns any/c)
(define document-create-text-node any/c)
(define document-create-comment any/c)
(define document-create-cdata-section any/c)
(define document-create-document-fragment any/c)
(define document-create-processing-instruction any/c)
(define document-adopt-node any/c)
(define document-get-element-by-id any/c)
(define document-query-selector any/c)
(define document-query-selector-all any/c)
(define document-has-focus? any/c)
(define document-get-selection any/c)
(define document-close any/c)
(define document-open any/c)
(define document-element-from-point any/c)
(define document-elements-from-point any/c)
(define selection-range-count any/c)
(define selection-is-collapsed? any/c)
(define selection-anchor-node any/c)
(define selection-focus-node any/c)
(define selection-to-string any/c)
(define selection-remove-all-ranges! any/c)
