#lang webracket

;;;
;;; Iterator wrappers
;;;

;; iterator : -> external/raw
;;   Read the JavaScript Iterator global constructor.
(define (iterator)
  (js-Iterator))

;; iterator-from : any/c -> external/raw
;;   Convert an iterator or iterable into a standard Iterator object.
(define (iterator-from object)
  (js-send (iterator) "from" (vector object)))

;; iterator-next : external/raw -> external/raw
;;   Pull the next iteration result from an Iterator object.
(define (iterator-next iter)
  (js-send iter "next" (vector)))
