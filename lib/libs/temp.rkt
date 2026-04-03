#lang webracket

;;;
;;; Temporary benchmark wrappers
;;;

;; This library wraps the temporary `temp.ffi` binding used by the benchmark.
;; Keep it tiny so it is easy to remove after the benchmark is no longer needed.

;; temp-x : external? -> any/c
;;   Read the benchmark property directly.
(define (temp-x obj)
  (js-temp-x obj))

;; temp-set-x! : external? any/c -> void?
;;   Set the benchmark property directly.
(define (temp-set-x! obj value)
  (js-temp-set-x! obj value)
  (void))
