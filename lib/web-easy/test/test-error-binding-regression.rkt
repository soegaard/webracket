;;;
;;; web-easy Error Binding Regression (include/reader path)
;;;

;; No-#lang entrypoint for `webracket.rkt -r`.

(include-lib web-easy)

;; failf : string? -> none/c
;;   Raise a deterministic failure without relying on `error`.
(define (failf msg)
  (raise (exn:fail msg (current-continuation-marks))))

;; check-true : any/c string? -> void?
;;   Assert v is true; otherwise fail with label.
(define (check-true v label)
  (unless v
    (failf label)))

;; Regression guard:
;; In the include/reader path, these must be procedures.
(check-true (procedure? error)
            "regression: error must be a procedure in include/reader mode")
(check-true (procedure? raise-argument-error)
            "regression: raise-argument-error must be a procedure in include/reader mode")

(void)
