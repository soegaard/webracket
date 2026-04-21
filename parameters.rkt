#lang racket/base
(provide current-ffi-foreigns
         current-ffi-imports-wat
         current-ffi-funcs-wat
         current-browser?
         current-tree-shake?
         current-runtime-primitive-report-path)

; The parameters are used by the driver to send
; the generated imports and funcs to the compiler and runtime generator.

(define current-ffi-foreigns    (make-parameter '())) ; list of `foreign` structures
(define current-ffi-imports-wat (make-parameter '())) ; list of s-exprs
(define current-ffi-funcs-wat   (make-parameter '())) ; list of s-exprs
(define current-browser?        (make-parameter #f))  ; boolean? browser mode flag
(define current-tree-shake?     (make-parameter #t))  ; boolean? tree shake runtime primitives
(define current-runtime-primitive-report-path
  (make-parameter #f))                                 ; path-string? or #f
