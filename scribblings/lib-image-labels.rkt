#lang racket/base

(require racket/contract/base)

(provide
  image-new
  image-alt
  image-set-alt!
  image-src
  image-set-src!
  image-srcset
  image-set-srcset!
  image-sizes
  image-set-sizes!
  image-cross-origin
  image-set-cross-origin!
  image-use-map
  image-set-use-map!
  image-is-map
  image-set-is-map!
  image-width
  image-set-width!
  image-height
  image-set-height!
  image-natural-width
  image-natural-height
  image-complete?
  image-current-src
  image-decoding
  image-set-decoding!
  image-fetch-priority
  image-set-fetch-priority!
  image-loading
  image-set-loading!
  image-referrer-policy
  image-set-referrer-policy!
  image-name
  image-set-name!
  image-x
  image-y
  image-decode
  (for-label (all-defined-out)))

(define image-new any/c)
(define image-alt any/c)
(define image-set-alt! any/c)
(define image-src any/c)
(define image-set-src! any/c)
(define image-srcset any/c)
(define image-set-srcset! any/c)
(define image-sizes any/c)
(define image-set-sizes! any/c)
(define image-cross-origin any/c)
(define image-set-cross-origin! any/c)
(define image-use-map any/c)
(define image-set-use-map! any/c)
(define image-is-map any/c)
(define image-set-is-map! any/c)
(define image-width any/c)
(define image-set-width! any/c)
(define image-height any/c)
(define image-set-height! any/c)
(define image-natural-width any/c)
(define image-natural-height any/c)
(define image-complete? any/c)
(define image-current-src any/c)
(define image-decoding any/c)
(define image-set-decoding! any/c)
(define image-fetch-priority any/c)
(define image-set-fetch-priority! any/c)
(define image-loading any/c)
(define image-set-loading! any/c)
(define image-referrer-policy any/c)
(define image-set-referrer-policy! any/c)
(define image-name any/c)
(define image-set-name! any/c)
(define image-x any/c)
(define image-y any/c)
(define image-decode any/c)
