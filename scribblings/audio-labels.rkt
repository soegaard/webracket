#lang racket/base

(require racket/contract/base)

;; Docs-only fake bindings for audio Scribble links.
;;
;; These bindings are only for for-label use so @racketblock examples can
;; link to the documented audio identifiers.

(provide
  audio-context-new
  audio-context-destination
  audio-context-create-oscillator
  audio-node-connect
  audio-oscillator-node-set-type!
  audio-oscillator-node-start!
  (for-label (all-defined-out)))

(define audio-context-new any/c)
(define audio-context-destination any/c)
(define audio-context-create-oscillator any/c)
(define audio-node-connect any/c)
(define audio-oscillator-node-set-type! any/c)
(define audio-oscillator-node-start! any/c)
