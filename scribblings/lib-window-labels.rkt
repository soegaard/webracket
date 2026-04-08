#lang racket/base

(require racket/contract/base)

(provide
  Window
  (struct-out dom-window)
  (struct-out window-document-info)
  (struct-out window-location-info)
  (struct-out window-custom-elements-info)
  (struct-out window-history-info)
  (struct-out window-visual-viewport-info)
  (struct-out window-navigator-info)
  (struct-out window-screen-info)
  (struct-out window-performance-info)
  (struct-out window-local-storage-info)
  (struct-out window-session-storage-info)
  (struct-out window-indexed-db-info)
  (struct-out window-caches-info)
  (struct-out window-speech-synthesis-info)
  (struct-out window-style-media-info)
  (struct-out window-crypto-info)
  (struct-out window-scroll-options)
  (struct-out media-query-list)
  (struct-out css-style-declaration)
  window-self
  window-document
  window-name
  window-set-name!
  window-location
  window-custom-elements
  window-history
  window-visual-viewport
  window-navigator
  window-screen
  window-performance
  window-local-storage
  window-session-storage
  window-indexed-db
  window-caches
  window-speech-synthesis
  window-style-media
  window-crypto
  window-set-location!
  window-open
  window-fetch
  window-confirm
  window-prompt
  window-alert
  window-print
  window-focus
  window-stop
  window-closed?
  window-length
  window-origin
  window-device-pixel-ratio
  window-inner-height
  window-inner-width
  window-outer-height
  window-outer-width
  window-screen-x
  window-screen-y
  window-screen-left
  window-screen-top
  window-page-x-offset
  window-page-y-offset
  window-scroll-x
  window-scroll-y
  window-is-secure-context?
  window-cross-origin-isolated?
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

(struct dom-window (raw) #:transparent)
(struct window-document-info (raw) #:transparent)
(struct window-location-info (raw) #:transparent)
(struct window-custom-elements-info (raw) #:transparent)
(struct window-history-info (raw) #:transparent)
(struct window-visual-viewport-info (raw) #:transparent)
(struct window-navigator-info (raw) #:transparent)
(struct window-screen-info (raw) #:transparent)
(struct window-performance-info (raw) #:transparent)
(struct window-local-storage-info (raw) #:transparent)
(struct window-session-storage-info (raw) #:transparent)
(struct window-indexed-db-info (raw) #:transparent)
(struct window-caches-info (raw) #:transparent)
(struct window-speech-synthesis-info (raw) #:transparent)
(struct window-style-media-info (raw) #:transparent)
(struct window-crypto-info (raw) #:transparent)
(struct window-scroll-options (top left behavior) #:transparent)
(struct media-query-list (raw) #:transparent)
(struct css-style-declaration (raw) #:transparent)

(define window-self any/c)
(define window-document any/c)
(define window-name any/c)
(define window-set-name! any/c)
(define window-location any/c)
(define window-custom-elements any/c)
(define window-history any/c)
(define window-visual-viewport any/c)
(define window-navigator any/c)
(define window-screen any/c)
(define window-performance any/c)
(define window-local-storage any/c)
(define window-session-storage any/c)
(define window-indexed-db any/c)
(define window-caches any/c)
(define window-speech-synthesis any/c)
(define window-style-media any/c)
(define window-crypto any/c)
(define window-set-location! any/c)
(define window-open any/c)
(define window-fetch any/c)
(define window-confirm any/c)
(define window-prompt any/c)
(define window-alert any/c)
(define window-print any/c)
(define window-focus any/c)
(define window-stop any/c)
(define window-closed? any/c)
(define window-length any/c)
(define window-origin any/c)
(define window-device-pixel-ratio any/c)
(define window-inner-height any/c)
(define window-inner-width any/c)
(define window-outer-height any/c)
(define window-outer-width any/c)
(define window-screen-x any/c)
(define window-screen-y any/c)
(define window-screen-left any/c)
(define window-screen-top any/c)
(define window-page-x-offset any/c)
(define window-page-y-offset any/c)
(define window-scroll-x any/c)
(define window-scroll-y any/c)
(define window-is-secure-context? any/c)
(define window-cross-origin-isolated? any/c)
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
