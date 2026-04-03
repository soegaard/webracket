#lang racket/base

(require racket/contract/base)

(provide
  (struct-out websocket)
  websocket-new
  websocket?
  check-websocket
  websocket-url
  websocket-ready-state
  websocket-ready-state-number
  websocket-buffered-amount
  websocket-protocol
  websocket-extensions
  websocket-send
  websocket-close
  websocket-onopen!
  websocket-onmessage!
  websocket-onclose!
  websocket-onerror!
  websocket-add-event-listener!
  websocket-remove-event-listener!
  (for-label (all-defined-out)))

(struct websocket (raw) #:transparent)

(define websocket-new any/c)
(define check-websocket any/c)
(define websocket-url any/c)
(define websocket-ready-state any/c)
(define websocket-ready-state-number any/c)
(define websocket-buffered-amount any/c)
(define websocket-protocol any/c)
(define websocket-extensions any/c)
(define websocket-send any/c)
(define websocket-close any/c)
(define websocket-onopen! any/c)
(define websocket-onmessage! any/c)
(define websocket-onclose! any/c)
(define websocket-onerror! any/c)
(define websocket-add-event-listener! any/c)
(define websocket-remove-event-listener! any/c)
