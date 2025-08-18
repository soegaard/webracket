#lang racket/base
(provide current-ffi-foreigns
         current-ffi-imports-wat
         current-ffi-funcs-wat)

; The parameters are used by the driver, to send
; the generated imports and funcs to the compiler and runtime generator.

(define current-ffi-foreigns    (make-parameter '())) ; list of `foreign` structures
(define current-ffi-imports-wat (make-parameter '())) ; list of s-exprs
(define current-ffi-funcs-wat   (make-parameter '())) ; list of s-exprs

