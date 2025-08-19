(define (add-children elem children)
  (for ([child (in-list children)])
    (js-append-child! elem (sxml->dom child))))

(define (set-elem-attributes elem attrs)
  (for ([attr (in-list attrs)])
    (match attr
      [(list name value)
       (js-set-attribute! elem (symbol->string name) value)])))

(define (sxml->dom exp)
  (match exp
    [(? string? s)
     (js-create-text-node exp)]
    [(list tag (list '@ attrs ...) children ...)
     ;; Create a new element with the given tag.
     (define elem (js-create-element (symbol->string tag)))
     (set-elem-attributes elem attrs)
     (add-children elem children)
     elem]
    [(list tag children ...)
     ;; Create a new element with the given tag.
     (define elem (js-create-element (symbol->string tag)))
     (add-children elem children)
     elem]))

(define implemented-primitives
  '(equal? eqv? eq? eq-hash-code eqv-hash-code equal-hash-code
           not boolean?
           symbol? symbol-interned? symbol->string string->symbol
           string->uninterned-symbol symbol<? symbol=?
           symbol->immutable-string
           keyword? keyword->string keyword<?
           pair? null? cons car cdr list? list list* length
           list-ref list-tail append reverse map for-each memq))

(define sections
  '(("4.1 Equality"
      (equal? eqv? eq? eq-hash-code eqv-hash-code equal-hash-code))
    ("4.2 Booleans"
      (not boolean?))
    ("4.7 Symbols"
      (symbol? symbol-interned? symbol->string string->symbol
               string->uninterned-symbol symbol<? symbol=?))
    ("4.7.1 Additional Symbol Functions"
      (symbol->immutable-string))
    ("4.9 Keywords"
      (keyword? keyword->string keyword<?))
    ("4.10 Pairs and Lists"
      (pair? null? cons car cdr list? list list* length list-ref
             list-tail append reverse map for-each memq))))

(define (primitive-url sym)
  (string-append "https://docs.racket-lang.org/reference/data.html?q="
                 (symbol->string sym)))

(define (percent->color pct)
  (define hue (inexact->exact (round (* 120 pct))))
  (string-append "hsl(" (string-append (number->string hue) ",100%,50%)")))


(define (make-gauge pct)
  (define color       (percent->color pct))
  (define pct-str     (number->string (round (* 100 pct))))
  (define width       (string-append pct-str "%"))
  `(div (@ (style "display:flex;align-items:center;gap:8px;"))
        (div (@ (style "background:#ddd;width:100px;height:10px;"))
             (div (@ (style ,(string-append 
                              (string-append (string-append "height:100%;width:" width)
                                             (string-append ";background:" color))
                              "~;")))
                  (span ,(string-append pct-str "%"))))))

(define (primitive-li sym)
  (define checked? (memq sym implemented-primitives))
  ; (define checked? #t) ; TODO TODO TODO
  `(li
     (label
       (input (@ (type "checkbox") (disabled "")
                 ,@(if checked? '((checked "")) '())))
       (a (@ (href ,(primitive-url sym))) ,(symbol->string sym)))))

(define (section->sxml section)
  (match section
    [(list title primitives)
     #;(define implemented (filter (lambda (p) (memq p implemented-primitives))
                                   primitives))
     (define implemented
       (for/list ([p (in-list primitives)]
                  #:when (memq p implemented-primitives))
         p))
         
     (define pct (if (null? primitives)
                     0
                     (/ (length implemented) (length primitives))))
     `(section
        (h2 ,title)
        ,(make-gauge pct)
        (ul ,@(map primitive-li primitives)))]))


(define page
  `(div
     (h1 "Progress: Datatype Primitives")
     ,@(map section->sxml sections)))

(js-append-child! (js-document-body) (sxml->dom page))
