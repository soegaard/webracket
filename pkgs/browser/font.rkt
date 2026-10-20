#lang racket/base

(require js/promise
         webracket/unsafe/document
         webracket/unsafe/standard
         (only-in webracket/primitives external?))

(provide (except-out (struct-out font-face-set-info) font-face-set-info?)
         (rename-out [font-face-set-info/surface? font-face-set?])
         document-font-face-set
         font-face-set-status
         font-face-set-check?
         font-face-set-ready)

;; font-face-set-info : external/raw -> font-face-set-info?
;;   Wrap a document FontFaceSet without applying a readiness policy.
(struct font-face-set-info (raw) #:transparent)

;; font-face-set-raw-host-value? : any/c -> boolean?
;;   Report whether value is a non-nullish browser host object.
(define (font-face-set-raw-host-value? value)
  (and (external? value)
       (not (js-nullish? value))))

;; font-face-set-method? : external/raw string? -> boolean?
;;   Report whether raw provides a callable FontFaceSet method.
(define (font-face-set-method? raw method-name)
  (and (font-face-set-raw-host-value? raw)
       (let ([method (js-ref/extern raw method-name)])
         (and (not (js-nullish? method))
              (string=? (js-typeof method) "function")))))

;; font-face-set-info/surface? : any/c -> boolean?
;;   Report whether value wraps a usable document FontFaceSet.
(define (font-face-set-info/surface? value)
  (and (font-face-set-info? value)
       (font-face-set-method? (font-face-set-info-raw value) "check")
       (not (js-nullish? (js-ref/extern (font-face-set-info-raw value) "ready")))))

;; document-font-face-set : -> (or/c #f font-face-set?)
;;   Read document.fonts when the browser exposes the Font Loading API.
(define (document-font-face-set)
  (define raw (js-ref/extern (js-document) "fonts"))
  (and (font-face-set-raw-host-value? raw)
       (font-face-set-info/surface? (font-face-set-info raw))
       (font-face-set-info raw)))

;; font-face-set-status : font-face-set? -> string?
;;   Read the native FontFaceSet status, such as loading or loaded.
(define (font-face-set-status set)
  (unless (font-face-set-info/surface? set)
    (raise-argument-error 'font-face-set-status "font-face-set?" set))
  (define value (js-ref (font-face-set-info-raw set) "status"))
  (unless (string? value)
    (raise-arguments-error 'font-face-set-status
                           "FontFaceSet.status did not return a string"))
  value)

;; font-face-set-check? : font-face-set? (or/c string? symbol?) [(or/c string? symbol?)] -> boolean?
;;   Test whether a CSS font shorthand can render optional text; text defaults to the empty string.
(define (font-face-set-check? set font [text ""])
  (unless (font-face-set-info/surface? set)
    (raise-argument-error 'font-face-set-check? "font-face-set?" set))
  (unless (or (string? font) (symbol? font))
    (raise-argument-error 'font-face-set-check? "(or/c string? symbol?)" font))
  (unless (or (string? text) (symbol? text))
    (raise-argument-error 'font-face-set-check? "(or/c string? symbol?)" text))
  (define value
    (js-send/value (font-face-set-info-raw set)
                   "check"
                   (vector (if (symbol? font) (symbol->string font) font)
                           (if (symbol? text) (symbol->string text) text))))
  (unless (boolean? value)
    (raise-arguments-error 'font-face-set-check?
                           "FontFaceSet.check did not return a boolean"))
  value)

;; font-face-set-ready : font-face-set? -> js-promise?
;;   Return the native readiness Promise; it is not made cancellable.
(define (font-face-set-ready set)
  (unless (font-face-set-info/surface? set)
    (raise-argument-error 'font-face-set-ready "font-face-set?" set))
  (js-promise-wrap
   (js-ref/extern (font-face-set-info-raw set) "ready")))
