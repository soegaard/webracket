#lang webracket

;;;
;;; Document wrappers
;;;

;; document-stringish->string : symbol? any/c -> string?
;;   Normalize a string-like wrapper argument to a browser string.
(define (document-stringish->string who v)
  (cond
    [(string? v) v]
    [(symbol? v) (symbol->string v)]
    [else (raise-argument-error who "(or/c string? symbol?)" v)]))

;; document : external/raw -> document?
;;   Wrap a browser Document object.
(struct document (raw) #:transparent)

;; Document : -> document?
;;   Read the current document object.
(define (Document)
  (document (js-document)))

;; document-head : -> (or/c #f element?)
;;   Read the document head element, if present.
(define (document-head)
  (element-wrap (js-document-head)))

;; document-body : -> (or/c #f element?)
;;   Read the document body element, if present.
(define (document-body)
  (element-wrap (js-document-body)))

;; document-element : -> element?
;;   Read the root document element.
(define (document-element)
  (element-wrap (js-document-element)))

;; document-create-element : (or/c string? symbol?) -> element?
;;   Create an element for a tag name.
(define (document-create-element tag)
  (define tag* (document-stringish->string 'document-create-element tag))
  (element-wrap (js-create-element tag*)))

;; document-create-text-node : (or/c string? symbol?) -> external/raw
;;   Create a text node.
(define (document-create-text-node text)
  (define text* (document-stringish->string 'document-create-text-node text))
  (js-create-text-node text*))

;; document-get-element-by-id : (or/c string? symbol?) -> (or/c #f element?)
;;   Look up a single element by id.
(define (document-get-element-by-id id)
  (define id* (document-stringish->string 'document-get-element-by-id id))
  (element-wrap (js-get-element-by-id id*)))

;; document-query-selector : (or/c string? symbol?) -> (or/c #f element?)
;;   Return the first element matching a selector.
(define (document-query-selector selector)
  (define selector* (document-stringish->string 'document-query-selector selector))
  (element-wrap (js-query-selector selector*)))

;; document-query-selector-all : (or/c string? symbol?) -> external/raw
;;   Return all elements matching a selector.
(define (document-query-selector-all selector)
  (define selector* (document-stringish->string 'document-query-selector-all selector))
  (js-query-selector-all selector*))

;; document-has-focus? : -> boolean?
;;   Report whether the document currently has focus.
(define (document-has-focus?)
  (not (zero? (js-has-focus))))

;; document-get-selection : -> (or/c #f external?)
;;   Read the current selection.
(define (document-get-selection)
  (js-get-selection))

;; document-close : -> void?
;;   Close a document stream.
(define (document-close)
  (js-close)
  (void))

;; document-open : -> document?
;;   Open a document stream for writing.
(define (document-open)
  (document (js-open)))

;; document-element-from-point : real? real? -> (or/c #f element?)
;;   Find the topmost element at the given viewport coordinates.
(define (document-element-from-point x y)
  (element-wrap (js-element-from-point x y)))

;; document-elements-from-point : real? real? -> external/raw
;;   Find all elements at the given viewport coordinates.
(define (document-elements-from-point x y)
  (js-elements-from-point x y))
