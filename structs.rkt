#lang racket/base
(provide (all-defined-out))

;;;
;;; Structure Definitions
;;;

; This module contains structure definitions used by multiple modules.

; See "compiler.rkt"
(struct variable (id)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc d port mode)
     ;; delegate to Racket's printer for the value field
     ; (write `(variable: ,(syntax-e (variable-id d))) port)
     (write (syntax-e (variable-id d)) port)
     )])


(struct foreign (racket-name    ; the Racket primitive, we are defining (symbol)
                 module-name    ; name of imported module
                 host-name      ; name in the imported module
                 argument-types ; list of argument types
                 result-types   ; list of result types
                 )
  #:transparent)

