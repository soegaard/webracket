#lang webracket

;;;
;;; Smoke Format Helpers
;;;

;; Minimal formatting helpers for smoke/parity examples.
;;
;; Exports:
;;   ~a      Concatenate values as display strings.

(define-values (~a)
  (let ()
    ;; ~a : any/c ... -> string?
    ;;   Concatenate display text for all parts.
    (define (~a . parts)
      (let loop ([remaining parts] [acc ""])
        (cond
          [(null? remaining) acc]
          [else
           (loop (cdr remaining)
                 (string-append acc (format "~a" (car remaining))))])))

    (values ~a)))
