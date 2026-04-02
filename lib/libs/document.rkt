#lang webracket

;;;
;;; Document wrappers
;;;

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

;; document-create-element : string? -> element?
;;   Create an element for a tag name.
(define (document-create-element tag)
  (unless (string? tag)
    (raise-argument-error 'document-create-element "string?" tag))
  (element-wrap (js-create-element tag)))

;; document-create-text-node : string? -> external/raw
;;   Create a text node.
(define (document-create-text-node text)
  (unless (string? text)
    (raise-argument-error 'document-create-text-node "string?" text))
  (js-create-text-node text))

;; document-get-element-by-id : string? -> (or/c #f element?)
;;   Look up a single element by id.
(define (document-get-element-by-id id)
  (unless (string? id)
    (raise-argument-error 'document-get-element-by-id "string?" id))
  (element-wrap (js-get-element-by-id id)))

;; document-query-selector : string? -> (or/c #f element?)
;;   Return the first element matching a selector.
(define (document-query-selector selector)
  (unless (string? selector)
    (raise-argument-error 'document-query-selector "string?" selector))
  (element-wrap (js-query-selector selector)))

;; document-query-selector-all : string? -> external/raw
;;   Return all elements matching a selector.
(define (document-query-selector-all selector)
  (unless (string? selector)
    (raise-argument-error 'document-query-selector-all "string?" selector))
  (js-query-selector-all selector))

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
