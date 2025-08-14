#lang racket/base
;;;
;;; WebAssembly Utilities
;;;

; This module contains small helper function for generating WebAssembly.
; We are generating WebAssembly in the folded, textual format.

;;; Exports
(provide (all-defined-out))

;;; Imports
(require racket/format
         "immediates.rkt"
         "structs.rkt")


;;; Utilities

; 1. WebAssembly requires all identifiers to begin with $.

(define ($ x)
  (if (syntax? x)
      ($ (syntax-e x))
      (string->symbol (~a "$" x))))

; 2. Identifiers from the user program are prefixed with $$.
;    This way the compiler can use all other identifiers freely.

(define ($$ x) ($ ($ x)))

; 3. The runtime representation of a primitive, say, fx+ is stored
;    in a global variable $prim:fx+ that holds an $PrimitiveProcedure.

(define (prim: sym)
  (if (syntax? sym)
      (prim: (syntax-e sym))
      (string->symbol (~a "prim:" sym))))

; 4. All immediates are stored in an `ref.i31.

(define (I31 n)
  `(ref.i31 (i32.const ,n)))

; 5. Imm will given a immediate (Racket value) return a wasm expression,
;    that computes the wasm representation of the immediate.
(define (Imm x)
  (I31 (immediate-rep x)))

; 6. imm returns an i32
(define (imm x)
  (immediate-rep x))

(define (Undefined [_ #f])
  (I31 undefined-value)) ; it is mapable


; 7. Names of labels and primitives
(define (Label v)                ($  (variable-id v)))
(define (Prim pr)                ($  (variable-id pr)))

; 8. Variable names from the user program.
(define (Var v)                  ($$ (variable-id v)))
(define (TopVar v)               ($$ (variable-id v)))
(define (ModuleVar v)            ($$ (variable-id v)))
(define (LocalVar v)             ($$ (variable-id v)))

; 9. Reference to a variable
(define (Ref v)                  ($$ (variable-id v)))

; 10. Reference to a primitive
(define (PrimRef pr)             `(global.get ,($ (prim: (variable-id pr)))))
