#lang racket/base
(provide current-ffi-foreigns
         current-ffi-imports-wat
         current-ffi-funcs-wat
         current-browser?
         current-console-bridge?
         current-letrec-strategy
         current-tree-shake?
         current-runtime-primitive-report-path
         current-runtime-timing-rows)

; The parameters are used by the driver to send
; the generated imports and funcs to the compiler and runtime generator.

(define current-ffi-foreigns    (make-parameter '())) ; list of `foreign` structures
(define current-ffi-imports-wat (make-parameter '())) ; list of s-exprs
(define current-ffi-funcs-wat   (make-parameter '())) ; list of s-exprs
(define current-browser?        (make-parameter #f))  ; boolean? browser mode flag
(define current-console-bridge? (make-parameter #f))  ; boolean? install browser console bridge
(define current-letrec-strategy (make-parameter 'basic)) ; 'basic or 'waddell
(define current-tree-shake?     (make-parameter #t))  ; boolean? tree shake runtime primitives
(define current-runtime-primitive-report-path
  (make-parameter #f))                                 ; path-string? or #f
(define current-runtime-timing-rows
  (make-parameter #f))                                 ; (or/c #f (listof (list/c string? real?)))
