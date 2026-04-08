#lang racket/base

(require racket/contract/base)

;; Docs-only fake bindings for raw bridge accessors.

(provide
  (struct-out array)
  dom-window-raw
  window-document-info-raw
  window-location-info-raw
  media-query-list-raw
  css-style-declaration-raw
  dom-document-raw
  dom-text-raw
  dom-node-raw
  dom-attr-raw
  dom-selection-raw
  dom-element-raw
  dom-token-list-raw
  dom-shadow-root-raw
  dom-node-list-raw
  html-collection-raw
  dom-rect-list-raw
  dom-rect-raw
  audio-listener-raw
  performance-event-counts-info-raw
  dom-computed-style-map-raw
  dom-animation-raw
  iterator-raw
  websocket-raw
  (for-label (all-defined-out)))

(struct array (raw) #:transparent)

(define dom-window-raw any/c)
(define window-document-info-raw any/c)
(define window-location-info-raw any/c)
(define media-query-list-raw any/c)
(define css-style-declaration-raw any/c)
(define dom-document-raw any/c)
(define dom-text-raw any/c)
(define dom-node-raw any/c)
(define dom-attr-raw any/c)
(define dom-selection-raw any/c)
(define dom-element-raw any/c)
(define dom-token-list-raw any/c)
(define dom-shadow-root-raw any/c)
(define dom-node-list-raw any/c)
(define html-collection-raw any/c)
(define dom-rect-list-raw any/c)
(define dom-rect-raw any/c)
(define audio-listener-raw any/c)
(define performance-event-counts-info-raw any/c)
(define dom-computed-style-map-raw any/c)
(define dom-animation-raw any/c)
(define iterator-raw any/c)
(define websocket-raw any/c)
