#lang racket/base

(require "core.rkt"
         "browser-options.rkt"
         (for-syntax "browser-options.rkt"))
(provide (all-from-out "core.rkt")
         (for-syntax (all-from-out "browser-options.rkt")))

;; Uncommenting the line below disables the custom reader
;; in `lang/reader.rkt`.
#;(module reader syntax/module-reader webracket)
         
