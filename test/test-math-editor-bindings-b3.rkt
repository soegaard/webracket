#lang webracket/base

;; B3 browser-binding consumer. The reusable nested module imports the public
;; binding surface; this root supplies only a controlled native DataTransfer
;; fixture and does not read or modify the operating-system clipboard.

(require browser/document
         browser/element
         clipboard
         js/promise
         "math-editor-bindings/b3-services.rktl"
         (only-in webracket/unsafe/standard
                  js-new
                  js-var))

(define assertion-count 0)

;; check-true : any/c string? -> void?
;;   Record a completed assertion or raise a labeled failure.
(define (check-true value label)
  (unless value
    (error 'test-math-editor-bindings-b3 label))
  (set! assertion-count (add1 assertion-count)))

;; check-equal : any/c any/c string? -> void?
;;   Record a completed equality assertion or raise a labeled failure.
(define (check-equal got expected label)
  (unless (equal? got expected)
    (error 'test-math-editor-bindings-b3 label))
  (set! assertion-count (add1 assertion-count)))

(define body (document-body))
(unless body
  (error 'test-math-editor-bindings-b3 "document.body is unavailable"))

(element-set-attribute! body 'data-math-editor-bindings-b3-state "started")

;; Blob and ClipboardItem preserve MIME strings, UTF-8 byte size, and the
;; asynchronous native result without exercising the permission-gated OS API.
(check-true (blob-supported?) "Blob constructor availability")
(check-true (clipboard-item-supported?) "ClipboardItem constructor availability")
(define text "𝑥á")
(define blob (make-text-blob text "text/plain"))
(check-true (blob? blob) "text Blob wrapper")
(check-equal (blob-type blob) "text/plain" "Blob MIME type")
(check-equal (blob-size blob) 7 "Blob UTF-8 byte size")

(define item (b3-make-text-clipboard-item "text/plain" text))
(check-true (clipboard-item? item) "ClipboardItem wrapper")
(check-equal (clipboard-item-types item) #( "text/plain") "ClipboardItem MIME order")
(check-true (boolean? (clipboard-supported?)) "Clipboard availability capability")

;; The DataTransfer fixture is native but synthetic. It proves the synchronous
;; transfer methods without claiming OS clipboard coverage.
(define transfer
  (data-transfer-wrap
   (js-new (js-var "DataTransfer") (vector))))
(check-true (data-transfer? transfer) "synthetic DataTransfer wrapper")
(data-transfer-set-data! transfer "text/plain" "plain")
(data-transfer-set-data! transfer "application/x-webracket" "payload")
(check-equal (data-transfer-get-data transfer "text/plain") "plain" "DataTransfer plain payload")
(check-equal (data-transfer-get-data transfer "application/x-webracket")
             "payload"
             "DataTransfer custom MIME payload")
(check-equal (data-transfer-types transfer)
             #("text/plain" "application/x-webracket")
             "DataTransfer MIME order")

(define blob-text-seen? #f)
(define item-text-seen? #f)

;; finish-if-ready : -> void?
;;   Publish the receipt after each native MIME promise fulfills.
(define (finish-if-ready)
  (when (and blob-text-seen? item-text-seen?)
    (element-set-attribute! body 'data-math-editor-bindings-b3-state "pass")
    (element-set-attribute! body
                            'data-math-editor-bindings-b3-assertions
                            (number->string assertion-count))))

(js-promise-then
 (blob-text blob)
 (lambda (value)
   (check-equal value text "Blob text fulfillment")
   (set! blob-text-seen? #t)
   (finish-if-ready)))

(js-promise-then
 (clipboard-item-get-type item "text/plain")
 (lambda (value)
   (define item-blob (blob-wrap value))
   (check-true (blob? item-blob) "ClipboardItem getType Blob")
   (js-promise-then
    (blob-text item-blob)
    (lambda (item-text)
      (check-equal item-text text "ClipboardItem Blob text fulfillment")
      (set! item-text-seen? #t)
      (finish-if-ready)))
   (void)))
