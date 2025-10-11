#lang webracket
;;;
;;; Standard library for the browser host
;;;

;; This file is only included with --stdlib when the host
;; is the browser (that is, webracket must be invoked with --stdlib -b).


;;;
;;; SXML
;;;

(define (add-children! elem children)
  (for ([child (in-list children)])
    (js-append-child! elem (sxml->dom child))))

(define (set-elem-attributes! elem attrs)
  (for ([attr (in-list attrs)])
    (match attr
      [(list name value)
       (js-set-attribute! elem (symbol->string name) value)]
      [_
       (js-log "set-elem-attributes: attrs malformed, got:")
       (js-log attrs)])))

(define (sxml->dom exp)
  (match exp
    [(? string? s)
     (js-create-text-node s)]
    [(list (? symbol? tag) (list '@ attrs ...) children ...)
     (define elem (js-create-element (symbol->string tag)))
     (set-elem-attributes! elem attrs)
     (add-children! elem children)
     elem]
    [(list (? symbol? tag) children ...)
     (define elem (js-create-element (symbol->string tag)))
     (add-children! elem children)
     elem]
    [_
     (error 'sxml->dom "unsupported expression: ~a" exp)]))

