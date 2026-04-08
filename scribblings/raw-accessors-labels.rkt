#lang racket/base

(require racket/contract/base)

;; Docs-only fake bindings for raw bridge accessors.

(provide
  (struct-out array)
  window-raw
  window-document-info-raw
  window-location-info-raw
  media-query-list-raw
  css-style-declaration-raw
  document-raw
  text-node-raw
  node-raw
  attr-raw
  selection-raw
  element-raw
  dom-token-list-raw
  shadow-root-raw
  node-list-raw
  html-collection-raw
  dom-rect-list-raw
  dom-rect-raw
  audio-listener-raw
  performance-event-count-map-raw
  computed-style-map-raw
  animation-raw
  iterator-raw
  websocket-raw
  (for-label (all-defined-out)))

(struct array (raw) #:transparent)

(define window-raw any/c)
(define window-document-info-raw any/c)
(define window-location-info-raw any/c)
(define media-query-list-raw any/c)
(define css-style-declaration-raw any/c)
(define document-raw any/c)
(define text-node-raw any/c)
(define node-raw any/c)
(define attr-raw any/c)
(define selection-raw any/c)
(define element-raw any/c)
(define dom-token-list-raw any/c)
(define shadow-root-raw any/c)
(define node-list-raw any/c)
(define html-collection-raw any/c)
(define dom-rect-list-raw any/c)
(define dom-rect-raw any/c)
(define audio-listener-raw any/c)
(define performance-event-count-map-raw any/c)
(define computed-style-map-raw any/c)
(define animation-raw any/c)
(define iterator-raw any/c)
(define websocket-raw any/c)
