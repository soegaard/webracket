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

;; document-create-attribute : (or/c string? symbol?) -> attr?
;;   Create an attribute node for a name.
(define (document-create-attribute name)
  (define name* (document-stringish->string 'document-create-attribute name))
  (attr-wrap (js-create-attribute name*)))

;; document-create-attribute-ns : (or/c string? symbol?) (or/c string? symbol?) -> attr?
;;   Create a namespaced attribute node.
(define (document-create-attribute-ns ns name)
  (define ns* (document-stringish->string 'document-create-attribute-ns ns))
  (define name* (document-stringish->string 'document-create-attribute-ns name))
  (attr-wrap (js-create-attribute-ns ns* name*)))

;; document-create-text-node : (or/c string? symbol?) -> external/raw
;;   Create a text node.
(define (document-create-text-node text)
  (define text* (document-stringish->string 'document-create-text-node text))
  (text-wrap (js-create-text-node text*)))

;; document-create-comment : (or/c string? symbol?) -> node?
;;   Create a comment node.
(define (document-create-comment text)
  (define text* (document-stringish->string 'document-create-comment text))
  (node-wrap (js-create-comment text*)))

;; document-create-cdata-section : (or/c string? symbol?) -> node?
;;   Create a CDATA section node.
(define (document-create-cdata-section text)
  (define text* (document-stringish->string 'document-create-cdata-section text))
  (node-wrap (js-create-cdata-section text*)))

;; document-create-document-fragment : -> node?
;;   Create an empty document fragment.
(define (document-create-document-fragment)
  (node-wrap (js-create-document-fragment)))

;; document-create-processing-instruction : (or/c string? symbol?) (or/c string? symbol?) -> node?
;;   Create a processing instruction node.
(define (document-create-processing-instruction target data)
  (define target* (document-stringish->string 'document-create-processing-instruction target))
  (define data* (document-stringish->string 'document-create-processing-instruction data))
  (node-wrap (js-create-processing-instruction target* data*)))

;; document-adopt-node : node? -> node?
;;   Adopt a node into the current document.
(define (document-adopt-node node)
  (node-wrap (js-adopt-node (node-unwrap node))))

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

;; document-query-selector-all : (or/c string? symbol?) -> vector?
;;   Return all elements matching a selector as a wrapped vector.
(define (document-query-selector-all selector)
  (define selector* (document-stringish->string 'document-query-selector-all selector))
  (array-like->vector 'document-query-selector-all
                      (js-query-selector-all selector*)
                      element-wrap))

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

;; document-elements-from-point : real? real? -> vector?
;;   Find all elements at the given viewport coordinates as a wrapped vector.
(define (document-elements-from-point x y)
  (array-like->vector 'document-elements-from-point
                      (js-elements-from-point x y)
                      element-wrap))
