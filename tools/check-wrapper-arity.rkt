#lang racket/base

;;;
;;; check-wrapper-arity
;;;

;; Backward-compatibility launcher.
;; Use tools/check-constructor-arity.rkt for new call sites.

(require racket/runtime-path)

(define-runtime-path tool-path "check-constructor-arity.rkt")

(parameterize ([current-command-line-arguments (current-command-line-arguments)])
  (dynamic-require tool-path #f))
