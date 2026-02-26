#lang webracket
;;;
;;; Standard library for the browser host
;;;

;; This file is included when the host is the browser
;; (that is, webracket is invoked with -b and stdlib is enabled).

;;;
;;; There are tests for this file in stdlib/test/test-browser.rkt
;;;


;;;
;;; SXML
;;;

(define (attribute-marker? x)
  (eq? x '@))

(define (add-children! elem children)
  (for ([child (in-list children)])
    (js-append-child! elem (sxml->dom child))))

(define (raise-sxml-shape-error exp)
  (raise-arguments-error
   'sxml->dom
   "malformed SXML expression; expected (or/c string? (list* symbol? ...))"
   "expression"
   exp))

(define (raise-sxml-tag-error tag exp)
  (raise-arguments-error
   'sxml->dom
   "tag name must be a symbol"
   "tag"
   tag
   "expression"
   exp))

(define (raise-sxml-stray-attribute-block-error exp)
  (raise-arguments-error
   'sxml->dom
   "attribute block (@ ...) must appear immediately after an element tag"
   "expression"
   exp))

(define (raise-sxml-attribute-block-error attrs exp)
  (raise-arguments-error
   'sxml->dom
   "attribute block must be a proper list"
   "attribute block"
   attrs
   "expression"
   exp))

(define (raise-sxml-attribute-name-error index name exp)
  (raise-arguments-error
   'sxml->dom
   "attribute name must be a symbol or string"
   "attribute index"
   index
   "attribute name"
   name
   "expression"
   exp))

(define (raise-sxml-attribute-entry-error index attr exp)
  (raise-arguments-error
   'sxml->dom
   "attribute entry must be a list of exactly two values"
   "attribute index"
   index
   "attribute entry"
   attr
   "expression"
   exp))

(define (attribute-value->string v)
  (cond
    [(string? v)  v]
    [(symbol? v)  (symbol->string v)]
    [(char? v)    (string v)]
    [(number? v)  (number->string v)]
    [(boolean? v) (if v "true" "false")]
    [else         (format "~a" v)]))

(define (attribute-entry->name+value attr index exp)
  (match attr
    [(list name value)
     (unless (or (symbol? name) (string? name))
       (raise-sxml-attribute-name-error index name exp))
     (values (if (symbol? name) (symbol->string name) name)
             (attribute-value->string value))]
    [_
     (raise-sxml-attribute-entry-error index attr exp)]))

(define (set-elem-attributes! elem attrs exp)
  (unless (list? attrs)
    (raise-sxml-attribute-block-error attrs exp))
  (for ([attr (in-list attrs)]
        [index (in-naturals 0)])
    (define-values (name value)
      (attribute-entry->name+value attr index exp))
    (js-set-attribute! elem name value)))

(define (create-element/with-context tag exp)
  (define tag-name (symbol->string tag))
  (define (handle-exn e)
    (error 'sxml->dom
           "create-element failed for tag ~s\n  sxml expression: ~a\n  underlying error: ~a"
           tag-name
           exp
           (exn-message e)))
  (with-handlers ([exn? handle-exn])
    (js-create-element tag-name)))

(define (sxml->dom exp)
  (match exp
    [(? string? s)
     (js-create-text-node s)]
    [(list '@ rest ...)
     (raise-sxml-stray-attribute-block-error exp)]
    [(list (? symbol? tag) rest ...)
     (define-values (attrs children)
       (match rest
         ['()
          (values '() '())]
         [(cons (list '@ attrs ...) children)
          (values attrs children)]
         [(cons (and maybe-attrs (cons '@ _)) _)
          (raise-sxml-attribute-block-error maybe-attrs exp)]
         [_
          (values '() rest)]))
     (define elem (create-element/with-context tag exp))
     (set-elem-attributes! elem attrs exp)
     (add-children! elem children)
     elem]
    [(list first _ ...)
     (raise-sxml-tag-error first exp)]
    [_
     (raise-sxml-shape-error exp)]))
