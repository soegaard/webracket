#lang webracket

;;;
;;; 13.5 Writing
;;;

;; This file contains an implementation of the non-primitives in
;; section "13.5 Writing" of "The Reference".

;; Since parameters are not supported by webracket yet, we define
;; each parameter as function locally.

(define print-pair-curly-braces       
  (let ([value #f]) ; default is #f
    (case-lambda
      [()    value]
      [(on?) (set! value on)
             value])))

(define print-mpair-curly-braces       
  (let ([value #f]) ; default is #f
    (case-lambda
      [()    value]
      [(on?) (set! value on)
             value])))



