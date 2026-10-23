#lang racket/base

(require browser/element
         webracket/unsafe/document
         (only-in webracket/unsafe/standard
                  js-ref
                  js-ref/extern))

(provide (except-out (all-defined-out) dom-document?)
         (rename-out [dom-document/surface? dom-document?])
         (all-from-out webracket/unsafe/document))

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

;; dom-document : external/raw -> dom-document?
;;   Wrap a browser Document object.
(struct dom-document (raw) #:transparent)

;; dom-document/surface? : any/c -> boolean?
;;   Check whether value is a Document wrapper with a usable host value.
(define (dom-document/surface? value)
  (and (dom-document? value)
       (shared-dom-raw-host-value? (dom-document-raw value))))

;; Document : -> dom-document?
;;   Read the current document object.
(define (Document)
  (dom-document (js-document)))

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

;; document-active-element : -> (or/c #f element?)
;;   Read the focused element, if the document has one.
(define (document-active-element)
  (element-wrap (js-ref/extern (js-document) "activeElement")))

;; document-create-element : (or/c string? symbol?) -> element?
;;   Create an element for a tag name.
(define (document-create-element tag)
  (define tag* (document-stringish->string 'document-create-element tag))
  (element-wrap (js-create-element tag*)))

;; document-create-element-ns : (or/c string? symbol?) (or/c string? symbol?) -> element?
;;   Create an element with name in the namespace identified by namespace-uri.
(define (document-create-element-ns namespace-uri name)
  (define namespace-uri*
    (document-stringish->string 'document-create-element-ns namespace-uri))
  (define name*
    (document-stringish->string 'document-create-element-ns name))
  (element-wrap (js-create-element-ns namespace-uri* name*)))

;; document-create-attribute : (or/c string? symbol?) -> dom-attr?
;;   Create an attribute node for a name.
(define (document-create-attribute name)
  (define name* (document-stringish->string 'document-create-attribute name))
  (dom-attr-wrap (js-create-attribute name*)))

;; document-create-attribute-ns : (or/c string? symbol?) (or/c string? symbol?) -> dom-attr?
;;   Create a namespaced attribute node.
(define (document-create-attribute-ns ns name)
  (define ns* (document-stringish->string 'document-create-attribute-ns ns))
  (define name* (document-stringish->string 'document-create-attribute-ns name))
  (dom-attr-wrap (js-create-attribute-ns ns* name*)))

;; document-create-text-node : (or/c string? symbol?) -> dom-text?
;;   Create a text node.
(define (document-create-text-node text)
  (define text* (document-stringish->string 'document-create-text-node text))
  (dom-text-wrap (js-create-text-node text*)))

;; document-create-comment : (or/c string? symbol?) -> dom-node?
;;   Create a comment node.
(define (document-create-comment text)
  (define text* (document-stringish->string 'document-create-comment text))
  (dom-node-wrap (js-create-comment text*)))

;; document-create-cdata-section : (or/c string? symbol?) -> dom-node?
;;   Create a CDATA section node.
(define (document-create-cdata-section text)
  (define text* (document-stringish->string 'document-create-cdata-section text))
  (dom-node-wrap (js-create-cdata-section text*)))

;; document-create-document-fragment : -> dom-node?
;;   Create an empty document fragment.
(define (document-create-document-fragment)
  (dom-node-wrap (js-create-document-fragment)))

;; document-create-processing-instruction : (or/c string? symbol?) (or/c string? symbol?) -> dom-node?
;;   Create a processing instruction node.
(define (document-create-processing-instruction target data)
  (define target* (document-stringish->string 'document-create-processing-instruction target))
  (define data* (document-stringish->string 'document-create-processing-instruction data))
  (dom-node-wrap (js-create-processing-instruction target* data*)))

;; document-adopt-node : dom-node? -> dom-node?
;;   Adopt a node into the current document.
(define (document-adopt-node node)
  (dom-node-wrap (js-adopt-node (dom-node-unwrap node))))

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

;; document-query-selector-all : (or/c string? symbol?) -> node-list?
;;   Return all elements matching a selector as a wrapped NodeList.
(define (document-query-selector-all selector)
  (define selector* (document-stringish->string 'document-query-selector-all selector))
  (node-list-wrap (js-query-selector-all selector*)))

;; document-has-focus? : -> boolean?
;;   Report whether the document currently has focus.
(define (document-has-focus?)
  (not (zero? (js-has-focus))))

;; document-hidden? : -> boolean?
;;   Report whether the browser has hidden the current document.
(define (document-hidden?)
  (define value (js-ref (js-document) "hidden"))
  (unless (boolean? value)
    (raise-arguments-error 'document-hidden?
                           "Document.hidden did not return a boolean"))
  value)

;; document-get-selection : -> (or/c #f dom-selection?)
;;   Read the current selection.
(define (document-get-selection)
  (dom-selection-wrap (js-get-selection)))

;; document-close : -> void?
;;   Close a document stream.
(define (document-close)
  (js-close)
  (void))

;; document-open : -> dom-document?
;;   Open a document stream for writing.
(define (document-open)
  (dom-document (js-open)))

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
