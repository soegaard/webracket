#lang webracket

;;;
;;; Image wrappers
;;;

;; i32->boolean : integer? -> boolean?
;;   Convert a browser i32 flag to a boolean.
(define (i32->boolean v)
  (not (zero? v)))

;; image-new : [any/c] [any/c] -> external/raw
;;   Create a new image element.
(define (image-new [width (void)] [height (void)])
  (js-image-new width height))

;; image-alt : external? -> string?
;;   Read the alt text.
(define (image-alt img)
  (js-image-alt img))

;; image-set-alt! : external? string? -> void?
;;   Set the alt text.
(define (image-set-alt! img alt)
  (js-set-image-alt! img alt)
  (void))

;; image-src : external? -> string?
;;   Read the image source.
(define (image-src img)
  (js-image-src img))

;; image-set-src! : external? string? -> void?
;;   Set the image source.
(define (image-set-src! img src)
  (js-set-image-src! img src)
  (void))

;; image-width : external? -> exact-nonnegative-integer?
;;   Read the rendered width.
(define (image-width img)
  (js-image-width img))

;; image-set-width! : external? exact-nonnegative-integer? -> void?
;;   Set the rendered width.
(define (image-set-width! img width)
  (js-set-image-width! img width)
  (void))

;; image-height : external? -> exact-nonnegative-integer?
;;   Read the rendered height.
(define (image-height img)
  (js-image-height img))

;; image-set-height! : external? exact-nonnegative-integer? -> void?
;;   Set the rendered height.
(define (image-set-height! img height)
  (js-set-image-height! img height)
  (void))

;; image-current-src : external? -> string?
;;   Read the selected source URL.
(define (image-current-src img)
  (js-image-current-src img))

;; image-decoding : external? -> string?
;;   Read the decoding hint.
(define (image-decoding img)
  (js-image-decoding img))

;; image-set-decoding! : external? string? -> void?
;;   Set the decoding hint.
(define (image-set-decoding! img decoding)
  (js-set-image-decoding! img decoding)
  (void))

;; image-loading : external? -> string?
;;   Read the loading strategy.
(define (image-loading img)
  (js-image-loading img))

;; image-set-loading! : external? string? -> void?
;;   Set the loading strategy.
(define (image-set-loading! img loading)
  (js-set-image-loading! img loading)
  (void))

;; image-complete? : external? -> boolean?
;;   Report whether loading completed.
(define (image-complete? img)
  (i32->boolean (js-image-complete img)))

;; image-cross-origin : external? -> any/c
;;   Read the CORS mode.
(define (image-cross-origin img)
  (js-image-cross-origin img))

;; image-set-cross-origin! : external? any/c -> void?
;;   Set the CORS mode.
(define (image-set-cross-origin! img mode)
  (js-set-image-cross-origin! img mode)
  (void))
