#lang racket/base

(require racket/contract/base)

;; Docs-only fake bindings for the public clipboard wrapper library.

(provide
  clipboard
  clipboard-raw
  clipboard?
  clipboard-supported?
  clipboard-read-text
  clipboard-write-text!
  data-transfer-wrap
  data-transfer?
  data-transfer-raw
  data-transfer-get-data
  data-transfer-set-data!
  data-transfer-types
  blob-supported?
  blob-wrap
  blob?
  blob-raw
  make-text-blob
  blob-size
  blob-type
  blob-text
  clipboard-item-supported?
  clipboard-item-wrap
  clipboard-item?
  clipboard-item-raw
  make-clipboard-item
  clipboard-item-types
  clipboard-item-get-type
  clipboard-read
  clipboard-write!
  (for-label (all-defined-out)))

(define clipboard any/c)
(define clipboard-raw any/c)
(define clipboard? any/c)
(define clipboard-supported? any/c)
(define clipboard-read-text any/c)
(define clipboard-write-text! any/c)
(define data-transfer-wrap any/c)
(define data-transfer? any/c)
(define data-transfer-raw any/c)
(define data-transfer-get-data any/c)
(define data-transfer-set-data! any/c)
(define data-transfer-types any/c)
(define blob-supported? any/c)
(define blob-wrap any/c)
(define blob? any/c)
(define blob-raw any/c)
(define make-text-blob any/c)
(define blob-size any/c)
(define blob-type any/c)
(define blob-text any/c)
(define clipboard-item-supported? any/c)
(define clipboard-item-wrap any/c)
(define clipboard-item? any/c)
(define clipboard-item-raw any/c)
(define make-clipboard-item any/c)
(define clipboard-item-types any/c)
(define clipboard-item-get-type any/c)
(define clipboard-read any/c)
(define clipboard-write! any/c)
