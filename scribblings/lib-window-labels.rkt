#lang racket/base

(require racket/contract/base)

(provide
  Window
  (struct-out window)
  (struct-out window-document-info)
  (struct-out window-location-info)
  (struct-out window-scroll-options)
  (struct-out media-query-list)
  (struct-out css-style-declaration)
  window-self
  window-document
  window-name
  window-set-name!
  window-location
  window-set-location!
  window-open
  window-fetch
  window-confirm
  window-prompt
  window-alert
  window-print
  window-focus
  window-stop
  window-scroll-to
  window-scroll-by
  window-scroll
  window-resize-to
  window-resize-by
  window-move-to
  window-move-by
  window-set-timeout
  window-set-timeout/delay
  window-clear-timeout
  window-set-interval
  window-clear-interval
  window-request-animation-frame
  window-cancel-animation-frame
  window-request-idle-callback
  window-cancel-idle-callback
  window-match-media
  window-get-computed-style
  window-structured-clone
  (for-label (all-defined-out)))

(define Window any/c)

(struct window (raw) #:transparent)
(struct window-document-info (raw) #:transparent)
(struct window-location-info (raw) #:transparent)
(struct window-scroll-options (top left behavior) #:transparent)
(struct media-query-list (raw) #:transparent)
(struct css-style-declaration (raw) #:transparent)

(define window-self any/c)
(define window-document any/c)
(define window-name any/c)
(define window-set-name! any/c)
(define window-location any/c)
(define window-set-location! any/c)
(define window-open any/c)
(define window-fetch any/c)
(define window-confirm any/c)
(define window-prompt any/c)
(define window-alert any/c)
(define window-print any/c)
(define window-focus any/c)
(define window-stop any/c)
(define window-scroll-to any/c)
(define window-scroll-by any/c)
(define window-scroll any/c)
(define window-resize-to any/c)
(define window-resize-by any/c)
(define window-move-to any/c)
(define window-move-by any/c)
(define window-set-timeout any/c)
(define window-set-timeout/delay any/c)
(define window-clear-timeout any/c)
(define window-set-interval any/c)
(define window-clear-interval any/c)
(define window-request-animation-frame any/c)
(define window-cancel-animation-frame any/c)
(define window-request-idle-callback any/c)
(define window-cancel-idle-callback any/c)
(define window-match-media any/c)
(define window-get-computed-style any/c)
(define window-structured-clone any/c)
