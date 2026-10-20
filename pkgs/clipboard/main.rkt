#lang webracket/base

(require js/promise
         webracket/unsafe/dom
         (only-in webracket/primitives
                  external?))

(provide (all-defined-out))

;;;
;;; Clipboard wrappers
;;;

;; This library exposes the browser Clipboard API via `navigator.clipboard`.

;; clipboard-stringish->string : symbol? any/c -> string?
;;   Normalize clipboard text input to a browser string.
(define (clipboard-stringish->string who v)
  (cond
    [(string? v) v]
    [(symbol? v) (symbol->string v)]
    [else
     (raise-argument-error who "(or/c string? symbol?)" v)]))

;; clipboard-info : external/raw -> clipboard-info?
;;   Wrap a browser Clipboard object.
(struct clipboard-info (raw) #:transparent)

;; data-transfer-info : external/raw -> data-transfer-info?
;;   Wrap a native DataTransfer without copying its format list or payloads.
(struct data-transfer-info (raw) #:transparent)

;; blob-info : external/raw -> blob-info?
;;   Wrap a native Blob for MIME clipboard payloads.
(struct blob-info (raw) #:transparent)

;; clipboard-item-info : external/raw -> clipboard-item-info?
;;   Wrap a native ClipboardItem without reordering its MIME types.
(struct clipboard-item-info (raw) #:transparent)

;; clipboard-raw : clipboard-info? -> external/raw
;;   Extract the raw Clipboard object.
(define (clipboard-raw value)
  (clipboard-info-raw value))

;; data-transfer-raw : data-transfer-info? -> external/raw
;;   Extract the raw browser DataTransfer object.
(define (data-transfer-raw value)
  (data-transfer-info-raw value))

;; blob-raw : blob-info? -> external/raw
;;   Extract the raw browser Blob object.
(define (blob-raw value)
  (blob-info-raw value))

;; clipboard-item-raw : clipboard-item-info? -> external/raw
;;   Extract the raw browser ClipboardItem object.
(define (clipboard-item-raw value)
  (clipboard-item-info-raw value))

;; clipboard-method-present? : external/raw string? -> boolean?
;;   Report whether raw exposes a non-nullish method named method-name.
(define (clipboard-method-present? raw method-name)
  (and (clipboard-raw-host-value? raw)
       (let ([method (js-ref raw method-name)])
         (and (not (js-nullish? method))
              (string=? (js-typeof method) "function")))))

;; clipboard-core-methods-present? : external/raw -> boolean?
;;   Report whether raw has the required Clipboard text methods.
(define (clipboard-core-methods-present? raw)
  (and (clipboard-method-present? raw "readText")
       (clipboard-method-present? raw "writeText")))

;; clipboard-raw-host-value? : any/c -> boolean?
;;   Report whether raw is a non-nullish external host value.
(define (clipboard-raw-host-value? raw)
  (and (external? raw)
       (not (js-nullish? raw))))

;; clipboard-host-method? : external/raw string? -> boolean?
;;   Report whether raw exposes a callable browser method.
(define (clipboard-host-method? raw method-name)
  (and (clipboard-raw-host-value? raw)
       (let ([method (js-ref raw method-name)])
         (and (not (js-nullish? method))
              (string=? (js-typeof method) "function")))))

;; clipboard-array-like->vector : symbol? any/c -> vector?
;;   Convert an array-like list of browser strings without changing its order.
(define (clipboard-array-like->vector who raw)
  (cond
    [(vector? raw)
     (for ([value (in-vector raw)])
       (unless (string? value)
         (raise-arguments-error who "array-like browser value contains a non-string")))
     raw]
    [(clipboard-raw-host-value? raw)
     (define length (js-ref raw "length"))
     (unless (exact-nonnegative-integer? length)
       (raise-arguments-error who "array-like browser value has no numeric length"))
     (let loop ([index 0] [acc '()])
       (if (= index length)
           (list->vector (reverse acc))
           (let ([value (js-index raw index)])
             (unless (string? value)
               (raise-arguments-error who "array-like browser value contains a non-string"))
             (loop (add1 index) (cons value acc)))))]
    [else
     (raise-argument-error who "array-like browser value" raw)]))

;; clipboard-supported-raw : -> (or/c #f external/raw)
;;   Read navigator.clipboard when it is available.
(define (clipboard-supported-raw)
  (with-handlers ([exn:fail? (lambda (_e) #f)])
    (define raw (js-ref (js-window-navigator) "clipboard"))
    (and (not (js-nullish? raw))
         (clipboard-core-methods-present? raw)
         raw)))

;; clipboard-supported? : -> boolean?
;;   Report whether the browser exposes navigator.clipboard text helpers.
(define (clipboard-supported?)
  (not (eq? (clipboard-supported-raw) #f)))

;; clipboard : -> (or/c #f clipboard-info?)
;;   Read the browser Clipboard object when it is available.
(define (clipboard)
  (define raw (clipboard-supported-raw))
  (and raw (clipboard-info raw)))

;; clipboard? : any/c -> boolean?
;;   Report whether v is a wrapped Clipboard value.
(define (clipboard? v)
  (and (clipboard-host-value? v)
       (clipboard-core-methods-present? (clipboard-raw v))))

;; clipboard-host-value? : any/c -> boolean?
;;   Report whether v wraps an external Clipboard-like host value.
(define (clipboard-host-value? v)
  (and (clipboard-info? v)
       (clipboard-raw-host-value? (clipboard-raw v))))

;; check-clipboard : symbol? any/c -> void?
;;   Ensure value wraps an external Clipboard-like host value.
(define (check-clipboard who value)
  (unless (clipboard-host-value? value)
    (raise-argument-error who "clipboard-info?" value)))

;; check-clipboard-method : symbol? clipboard-info? string? -> void?
;;   Ensure a required Clipboard method is available.
(define (check-clipboard-method who value method-name)
  (unless (clipboard-method-present? (clipboard-raw value) method-name)
    (raise-arguments-error who
                           (string-append "Clipboard." method-name " is not available"))))

;; clipboard-read-text : clipboard-info? -> js-promise?
;;   Start an asynchronous clipboard text read and return a wrapped browser Promise.
(define (clipboard-read-text value)
  (check-clipboard 'clipboard-read-text value)
  (check-clipboard-method 'clipboard-read-text value "readText")
  (js-promise-wrap-result
   'clipboard-read-text
   "readText"
   (js-send/extern (clipboard-raw value) "readText" (vector))
   "Clipboard"))

;; clipboard-write-text! : clipboard-info? (or/c string? symbol?) -> js-promise?
;;   Start an asynchronous clipboard text write and return a wrapped browser Promise.
(define (clipboard-write-text! value text)
  (check-clipboard 'clipboard-write-text! value)
  (check-clipboard-method 'clipboard-write-text! value "writeText")
  (define text* (clipboard-stringish->string 'clipboard-write-text! text))
  (js-promise-wrap-result
   'clipboard-write-text!
   "writeText"
   (js-send/extern (clipboard-raw value) "writeText" (vector text*))
   "Clipboard"))

;; data-transfer-wrap : any/c -> (or/c #f data-transfer-info?)
;;   Wrap a nullable native DataTransfer value.
(define (data-transfer-wrap value)
  (and (clipboard-raw-host-value? value)
       (data-transfer-info value)))

;; data-transfer? : any/c -> boolean?
;;   Report whether value wraps a DataTransfer with synchronous text methods.
(define (data-transfer? value)
  (and (data-transfer-info? value)
       (clipboard-host-method? (data-transfer-raw value) "getData")
       (clipboard-host-method? (data-transfer-raw value) "setData")))

;; check-data-transfer : symbol? any/c -> void?
;;   Ensure value wraps a usable native DataTransfer object.
(define (check-data-transfer who value)
  (unless (data-transfer? value)
    (raise-argument-error who "data-transfer?" value)))

;; data-transfer-get-data : data-transfer? (or/c string? symbol?) -> string?
;;   Read a synchronous clipboard or drag payload for an exact MIME type.
(define (data-transfer-get-data transfer mime-type)
  (check-data-transfer 'data-transfer-get-data transfer)
  (define value
    (js-send/value (data-transfer-raw transfer)
                   "getData"
                   (vector (clipboard-stringish->string 'data-transfer-get-data mime-type))))
  (unless (string? value)
    (raise-arguments-error 'data-transfer-get-data
                           "DataTransfer.getData did not return a string"))
  value)

;; data-transfer-set-data! : data-transfer? (or/c string? symbol?) (or/c string? symbol?) -> void?
;;   Set a synchronous clipboard or drag payload during its native event lifetime.
(define (data-transfer-set-data! transfer mime-type data)
  (check-data-transfer 'data-transfer-set-data! transfer)
  (js-send/extern/nullish
   (data-transfer-raw transfer)
   "setData"
   (vector (clipboard-stringish->string 'data-transfer-set-data! mime-type)
           (clipboard-stringish->string 'data-transfer-set-data! data)))
  (void))

;; data-transfer-types : data-transfer? -> vector?
;;   Return native DataTransfer format names in browser-provided order.
(define (data-transfer-types transfer)
  (check-data-transfer 'data-transfer-types transfer)
  (clipboard-array-like->vector
   'data-transfer-types
   (js-ref (data-transfer-raw transfer) "types")))

;; blob-supported? : -> boolean?
;;   Report whether the browser exposes a constructible Blob constructor.
(define (blob-supported?)
  (js-constructible? (js-var "Blob")))

;; blob-wrap : any/c -> (or/c #f blob-info?)
;;   Wrap a nullable Blob-like browser value.
(define (blob-wrap value)
  (and (clipboard-raw-host-value? value)
       (blob-info value)))

;; blob? : any/c -> boolean?
;;   Report whether value wraps a Blob with native text access.
(define (blob? value)
  (and (blob-info? value)
       (clipboard-host-method? (blob-raw value) "text")))

;; make-text-blob : (or/c string? symbol?) [(or/c #f string? symbol?)] -> blob?
;;   Construct a text Blob; type defaults to the browser's empty MIME type.
(define (make-text-blob text [mime-type #f])
  (unless (blob-supported?)
    (raise-arguments-error 'make-text-blob "Blob is not available"))
  (unless (or (not mime-type) (string? mime-type) (symbol? mime-type))
    (raise-argument-error 'make-text-blob "(or/c #f string? symbol?)" mime-type))
  (blob-info
   (js-new
    (js-var "Blob")
    (vector (vector (clipboard-stringish->string 'make-text-blob text))
            (if mime-type
                (js-object
                 (vector (vector "type"
                                 (clipboard-stringish->string 'make-text-blob mime-type))))
                (js-object (vector)))))))

;; blob-size : blob? -> exact-nonnegative-integer?
;;   Read Blob size in bytes.
(define (blob-size value)
  (unless (blob? value)
    (raise-argument-error 'blob-size "blob?" value))
  (define size (js-ref (blob-raw value) "size"))
  (unless (exact-nonnegative-integer? size)
    (raise-arguments-error 'blob-size "Blob.size did not return a nonnegative integer"))
  size)

;; blob-type : blob? -> string?
;;   Read the Blob MIME type string.
(define (blob-type value)
  (unless (blob? value)
    (raise-argument-error 'blob-type "blob?" value))
  (define type (js-ref (blob-raw value) "type"))
  (unless (string? type)
    (raise-arguments-error 'blob-type "Blob.type did not return a string"))
  type)

;; blob-text : blob? -> js-promise?
;;   Start an asynchronous UTF-8 text read of a Blob.
(define (blob-text value)
  (unless (blob? value)
    (raise-argument-error 'blob-text "blob?" value))
  (js-promise-wrap-result
   'blob-text
   "text"
   (js-send/extern (blob-raw value) "text" (vector))
   "Blob"))

;; clipboard-item-supported? : -> boolean?
;;   Report whether the browser exposes a constructible ClipboardItem constructor.
(define (clipboard-item-supported?)
  (js-constructible? (js-var "ClipboardItem")))

;; clipboard-item-wrap : any/c -> (or/c #f clipboard-item-info?)
;;   Wrap a nullable native ClipboardItem value.
(define (clipboard-item-wrap value)
  (and (clipboard-raw-host-value? value)
       (clipboard-item-info value)))

;; clipboard-item? : any/c -> boolean?
;;   Report whether value wraps a ClipboardItem with native MIME access.
(define (clipboard-item? value)
  (and (clipboard-item-info? value)
       (clipboard-host-method? (clipboard-item-raw value) "getType")))

;; make-clipboard-item : (or/c string? symbol?) blob? -> clipboard-item?
;;   Construct one MIME ClipboardItem containing a Blob.
(define (make-clipboard-item mime-type value)
  (unless (clipboard-item-supported?)
    (raise-arguments-error 'make-clipboard-item "ClipboardItem is not available"))
  (unless (blob? value)
    (raise-argument-error 'make-clipboard-item "blob?" value))
  (clipboard-item-info
   (js-new (js-var "ClipboardItem")
           (vector
            (js-object
             (vector
              (vector (clipboard-stringish->string 'make-clipboard-item mime-type)
                      (blob-raw value))))))))

;; clipboard-item-types : clipboard-item? -> vector?
;;   Return MIME types in their native ClipboardItem order.
(define (clipboard-item-types item)
  (unless (clipboard-item? item)
    (raise-argument-error 'clipboard-item-types "clipboard-item?" item))
  (clipboard-array-like->vector
   'clipboard-item-types
   (js-ref (clipboard-item-raw item) "types")))

;; clipboard-item-get-type : clipboard-item? (or/c string? symbol?) -> js-promise?
;;   Start an asynchronous MIME lookup whose fulfillment value is a native Blob.
(define (clipboard-item-get-type item mime-type)
  (unless (clipboard-item? item)
    (raise-argument-error 'clipboard-item-get-type "clipboard-item?" item))
  (js-promise-wrap-result
   'clipboard-item-get-type
   "getType"
   (js-send/extern (clipboard-item-raw item)
                   "getType"
                   (vector (clipboard-stringish->string 'clipboard-item-get-type mime-type)))
   "ClipboardItem"))

;; clipboard-read : clipboard-info? -> js-promise?
;;   Start asynchronous native ClipboardItem enumeration; permission failures reject.
(define (clipboard-read value)
  (check-clipboard 'clipboard-read value)
  (check-clipboard-method 'clipboard-read value "read")
  (js-promise-wrap-result
   'clipboard-read
   "read"
   (js-send/extern (clipboard-raw value) "read" (vector))
   "Clipboard"))

;; clipboard-write! : clipboard-info? vector? -> js-promise?
;;   Start an activation-sensitive ClipboardItem write in the current native callback.
(define (clipboard-write! value items)
  (check-clipboard 'clipboard-write! value)
  (check-clipboard-method 'clipboard-write! value "write")
  (unless (vector? items)
    (raise-argument-error 'clipboard-write! "vector?" items))
  (define native-items
    (list->vector
     (let loop ([index 0])
       (if (= index (vector-length items))
           '()
           (let ([item (vector-ref items index)])
             (unless (clipboard-item? item)
               (raise-argument-error 'clipboard-write! "vectorof clipboard-item?" items))
             (cons (clipboard-item-raw item) (loop (add1 index))))))))
  (js-promise-wrap-result
   'clipboard-write!
   "write"
   (js-send/extern (clipboard-raw value) "write" (vector native-items))
   "Clipboard"))
