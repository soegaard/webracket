#lang racket/base

(require racket/contract/base)

;; Docs-only fake bindings for event Scribble links.
;;
;; These bindings are only for for-label use so @racketblock examples can
;; link to the documented event identifiers.

(provide
  event?
  MessageEvent
  CloseEvent
  message-event?
  close-event?
  mouse-event?
  keyboard-event?
  pointer-event?
  focus-event?
  input-event?
  submit-event?
  touch-event?
  wheel-event?
  touch-list?
  touch?
  event-type
  message-event-data
  message-event-origin
  message-event-last-event-id
  message-event-source
  message-event-ports
  close-event-was-clean
  close-event-code
  close-event-reason
  event-target
  event-current-target
  prevent-default!
  stop-propagation!
  stop-immediate-propagation!
  mouse-event-offset-x
  mouse-event-offset-y
  mouse-event-client-x
  mouse-event-client-y
  keyboard-event-key
  keyboard-event-code
  touch-event-touches
  touch-event-target-touches
  touch-event-changed-touches
  touch-list-length
  touch-list-ref
  touch-identifier
  touch-client-x
  touch-client-y
  touch-page-x
  touch-page-y
  touch-screen-x
  touch-screen-y
  (for-label (all-defined-out)))

(define event? any/c)
(define MessageEvent any/c)
(define CloseEvent any/c)
(define message-event? any/c)
(define close-event? any/c)
(define mouse-event? any/c)
(define keyboard-event? any/c)
(define pointer-event? any/c)
(define focus-event? any/c)
(define input-event? any/c)
(define submit-event? any/c)
(define touch-event? any/c)
(define wheel-event? any/c)
(define touch-list? any/c)
(define touch? any/c)
(define event-type any/c)
(define message-event-data any/c)
(define message-event-origin any/c)
(define message-event-last-event-id any/c)
(define message-event-source any/c)
(define message-event-ports any/c)
(define close-event-was-clean any/c)
(define close-event-code any/c)
(define close-event-reason any/c)
(define event-target any/c)
(define event-current-target any/c)
(define prevent-default! any/c)
(define stop-propagation! any/c)
(define stop-immediate-propagation! any/c)
(define mouse-event-offset-x any/c)
(define mouse-event-offset-y any/c)
(define mouse-event-client-x any/c)
(define mouse-event-client-y any/c)
(define keyboard-event-key any/c)
(define keyboard-event-code any/c)
(define touch-event-touches any/c)
(define touch-event-target-touches any/c)
(define touch-event-changed-touches any/c)
(define touch-list-length any/c)
(define touch-list-ref any/c)
(define touch-identifier any/c)
(define touch-client-x any/c)
(define touch-client-y any/c)
(define touch-page-x any/c)
(define touch-page-y any/c)
(define touch-screen-x any/c)
(define touch-screen-y any/c)
