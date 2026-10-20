#lang racket/base

(require webracket/unsafe/dom
         (only-in webracket/primitives external?))

(provide (except-out (struct-out intl-segmenter-info) intl-segmenter-info?)
         (except-out (struct-out intl-segments-info) intl-segments-info?)
         (except-out (struct-out intl-segment-data-info) intl-segment-data-info?)
         (rename-out [intl-segmenter-info/surface? intl-segmenter?]
                     [intl-segments-info/surface? intl-segments?]
                     [intl-segment-data-info/surface? intl-segment-data?])
         intl-segmenter-supported?
         make-grapheme-segmenter
         intl-segmenter-segment
         intl-segments-containing
         intl-segment-data-segment
         intl-segment-data-index
         intl-segment-data-input
         intl-segment-data-word-like?)

;; intl-segmenter-info : external/raw -> intl-segmenter-info?
;;   Wrap an Intl.Segmenter configured by this binding.
(struct intl-segmenter-info (raw) #:transparent)

;; intl-segments-info : external/raw -> intl-segments-info?
;;   Wrap the iterable segment result for one input string.
(struct intl-segments-info (raw) #:transparent)

;; intl-segment-data-info : external/raw -> intl-segment-data-info?
;;   Wrap one native segmentation record.
(struct intl-segment-data-info (raw) #:transparent)

;; segmenter-raw-host-value? : any/c -> boolean?
;;   Report whether value is a non-nullish browser host object.
(define (segmenter-raw-host-value? value)
  (and (external? value)
       (not (js-nullish? value))))

;; segmenter-host-method? : external/raw string? -> boolean?
;;   Report whether raw provides a callable host method.
(define (segmenter-host-method? raw method-name)
  (and (segmenter-raw-host-value? raw)
       (let ([method (js-ref/extern raw method-name)])
         (and (not (js-nullish? method))
              (string=? (js-typeof method) "function")))))

;; intl-segmenter-supported? : -> boolean?
;;   Report whether the browser exposes Intl.Segmenter.
(define (intl-segmenter-supported?)
  (js-constructible? (js-IntlSegmenter)))

;; intl-segmenter-info/surface? : any/c -> boolean?
;;   Report whether value wraps a usable Intl.Segmenter.
(define (intl-segmenter-info/surface? value)
  (and (intl-segmenter-info? value)
       (segmenter-host-method? (intl-segmenter-info-raw value) "segment")))

;; intl-segments-info/surface? : any/c -> boolean?
;;   Report whether value wraps a segment result with containing support.
(define (intl-segments-info/surface? value)
  (and (intl-segments-info? value)
       (segmenter-host-method? (intl-segments-info-raw value) "containing")))

;; intl-segment-data-info/surface? : any/c -> boolean?
;;   Report whether value wraps a native segment record.
(define (intl-segment-data-info/surface? value)
  (and (intl-segment-data-info? value)
       (segmenter-raw-host-value? (intl-segment-data-info-raw value))))

;; make-grapheme-segmenter : [(or/c #f string? symbol?)] -> intl-segmenter?
;;   Create a native grapheme segmenter; #f uses the browser default locale.
(define (make-grapheme-segmenter [locale #f])
  (unless (or (not locale) (string? locale) (symbol? locale))
    (raise-argument-error 'make-grapheme-segmenter
                          "(or/c #f string? symbol?)"
                          locale))
  (unless (intl-segmenter-supported?)
    (raise-arguments-error 'make-grapheme-segmenter "Intl.Segmenter is not available"))
  (intl-segmenter-info
   (js-new (js-IntlSegmenter)
           (vector (if locale
                       (if (symbol? locale) (symbol->string locale) locale)
                       (js-undefined))
                   (js-object (vector (vector "granularity" "grapheme")))))))

;; intl-segmenter-segment : intl-segmenter? string? -> intl-segments?
;;   Segment text natively; returned indexes remain UTF-16 code-unit indexes.
(define (intl-segmenter-segment segmenter text)
  (unless (intl-segmenter-info/surface? segmenter)
    (raise-argument-error 'intl-segmenter-segment "intl-segmenter?" segmenter))
  (unless (string? text)
    (raise-argument-error 'intl-segmenter-segment "string?" text))
  (intl-segments-info
   (js-send/extern (intl-segmenter-info-raw segmenter) "segment" (vector text))))

;; intl-segments-containing : intl-segments? exact-nonnegative-integer? -> (or/c #f intl-segment-data?)
;;   Return the native segment containing a UTF-16 input index, if any.
(define (intl-segments-containing segments index)
  (unless (intl-segments-info/surface? segments)
    (raise-argument-error 'intl-segments-containing "intl-segments?" segments))
  (unless (exact-nonnegative-integer? index)
    (raise-argument-error 'intl-segments-containing "exact-nonnegative-integer?" index))
  (define raw (js-send/extern/null (intl-segments-info-raw segments)
                                   "containing"
                                   (vector index)))
  (and raw (intl-segment-data-info raw)))

;; intl-segment-data-string-property : symbol? intl-segment-data? string? -> string?
;;   Read a required string from a native segmentation record.
(define (intl-segment-data-string-property who data property-name)
  (unless (intl-segment-data-info/surface? data)
    (raise-argument-error who "intl-segment-data?" data))
  (define value (js-ref (intl-segment-data-info-raw data) property-name))
  (unless (string? value)
    (raise-arguments-error who
                           (string-append "Intl.SegmentData." property-name
                                          " did not return a string")))
  value)

;; intl-segment-data-segment : intl-segment-data? -> string?
;;   Return the segment text.
(define (intl-segment-data-segment data)
  (intl-segment-data-string-property 'intl-segment-data-segment data "segment"))

;; intl-segment-data-index : intl-segment-data? -> exact-nonnegative-integer?
;;   Return the segment's native UTF-16 input index.
(define (intl-segment-data-index data)
  (unless (intl-segment-data-info/surface? data)
    (raise-argument-error 'intl-segment-data-index "intl-segment-data?" data))
  (define value (js-ref (intl-segment-data-info-raw data) "index"))
  (unless (exact-nonnegative-integer? value)
    (raise-arguments-error 'intl-segment-data-index
                           "Intl.SegmentData.index did not return a nonnegative integer"))
  value)

;; intl-segment-data-input : intl-segment-data? -> string?
;;   Return the original input string from the segment record.
(define (intl-segment-data-input data)
  (intl-segment-data-string-property 'intl-segment-data-input data "input"))

;; intl-segment-data-word-like? : intl-segment-data? -> (or/c #f boolean?)
;;   Return isWordLike when the selected segmenter granularity exposes it.
(define (intl-segment-data-word-like? data)
  (unless (intl-segment-data-info/surface? data)
    (raise-argument-error 'intl-segment-data-word-like? "intl-segment-data?" data))
  (define raw (js-ref/extern (intl-segment-data-info-raw data) "isWordLike"))
  (cond
    [(js-nullish? raw) #f]
    [else
     (define value (js-ref (intl-segment-data-info-raw data) "isWordLike"))
     (unless (boolean? value)
       (raise-arguments-error 'intl-segment-data-word-like?
                              "Intl.SegmentData.isWordLike did not return a boolean"))
     value]))
