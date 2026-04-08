#lang racket/base

(require racket/contract/base)

;; Docs-only fake bindings for shared DOM label values.

(provide
  Window
  (struct-out dom-window)
  (struct-out dom-node)
  (struct-out dom-text)
  (struct-out dom-attr)
  (struct-out media-query-list)
  (struct-out css-style-declaration)
  (struct-out media-keys-info)
  (struct-out media-source-info)
  (struct-out audio-track)
  (struct-out text-track)
  (struct-out video-track)
  (for-label (all-defined-out)))

(struct dom-window (raw) #:transparent)
(struct dom-node (raw) #:transparent)
(struct dom-text (raw) #:transparent)
(struct dom-attr (raw) #:transparent)
(struct media-query-list (raw) #:transparent)
(struct css-style-declaration (raw) #:transparent)
(struct media-keys-info (raw) #:transparent)
(struct media-source-info (raw) #:transparent)
(struct audio-track (raw) #:transparent)
(struct text-track (raw) #:transparent)
(struct video-track (raw) #:transparent)
(define Window any/c)
