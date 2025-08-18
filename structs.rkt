#lang racket/base
(provide (all-defined-out))

;;;
;;; Structure Definitions
;;;

; This module contains structure definitions used by multiple modules.

; See "compiler.rkt"
(struct variable (id) #:transparent)


(struct foreign (racket-name    ; the Racket primitive, we are defining (symbol)
                 module-name    ; name of imported module
                 host-name      ; name in the imported module
                 argument-types ; list of argument types
                 result-types   ; list of result types
                 )
  #:transparent)

