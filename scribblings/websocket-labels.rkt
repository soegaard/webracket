#lang racket/base

(require racket/contract/base)

;; Docs-only fake bindings for websocket Scribble links.
;;
;; These bindings are only for for-label use so @racketblock examples can
;; link to the documented websocket identifiers.

(provide
  websocket-new
  websocket-onopen!
  websocket-onmessage!
  websocket-send
  websocket-close
  (for-label (all-defined-out)))

(define websocket-new any/c)
(define websocket-onopen! any/c)
(define websocket-onmessage! any/c)
(define websocket-send any/c)
(define websocket-close any/c)
