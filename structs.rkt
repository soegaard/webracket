#lang racket/base
(provide (all-defined-out))

;;;
;;; Structure Definitions
;;;

; This module contains structure definitions used by multiple modules.

; See "compiler.rkt"
(struct variable (id) #:transparent)
