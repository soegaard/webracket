#lang racket/base

(require racket/contract/base)

(provide
  (struct-out performance-event-count-map)
  (struct-out performance-memory-info)
  performance-event-count-map-size
  performance-event-count-map-entries
  performance-event-count-map-keys
  performance-event-count-map-values
  performance-event-count-map-get
  performance-event-count-map-has?
  performance-event-count-map-for-each
  performance-event-counts
  performance-interaction-count
  performance-memory-info-js-heap-size-limit
  performance-memory-info-total-js-heap-size
  performance-memory-info-used-js-heap-size
  performance-memory
  performance-time-origin
  performance-now
  performance-clear-marks
  performance-clear-measures
  performance-clear-resource-timings
  performance-get-entries
  performance-get-entries-by-name
  performance-get-entries-by-type
  performance-mark
  performance-measure
  performance-measure-user-agent-specific-memory
  performance-set-resource-timing-buffer-size
  performance-to-json)

(struct performance-event-count-map (raw) #:transparent)
(struct performance-memory-info (raw) #:transparent)

(define performance-event-count-map-size any/c)
(define performance-event-count-map-entries any/c)
(define performance-event-count-map-keys any/c)
(define performance-event-count-map-values any/c)
(define performance-event-count-map-get any/c)
(define performance-event-count-map-has? any/c)
(define performance-event-count-map-for-each any/c)
(define performance-event-counts any/c)
(define performance-interaction-count any/c)
(define performance-memory-info-js-heap-size-limit any/c)
(define performance-memory-info-total-js-heap-size any/c)
(define performance-memory-info-used-js-heap-size any/c)
(define performance-memory any/c)
(define performance-time-origin any/c)
(define performance-now any/c)
(define performance-clear-marks any/c)
(define performance-clear-measures any/c)
(define performance-clear-resource-timings any/c)
(define performance-get-entries any/c)
(define performance-get-entries-by-name any/c)
(define performance-get-entries-by-type any/c)
(define performance-mark any/c)
(define performance-measure any/c)
(define performance-measure-user-agent-specific-memory any/c)
(define performance-set-resource-timing-buffer-size any/c)
(define performance-to-json any/c)
