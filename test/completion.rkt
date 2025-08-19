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
  '(namespace-undefine-variable!
    namespace-set-variable-value!
    namespace-variable-value-simple
    make-empty-namespace
    namespace?
    unsafe-vector*-set!
    unsafe-vector*-length
    unsafe-cdr
    unsafe-car
    unsafe-fx<
    unsafe-fx=
    unsafe-fl/
    unsafe-fx+
    js-log
    variable-reference-constant?
    variable-reference-from-unsafe?
    primitive-result-arity
    primitive-closure?
    primitive?
    procedure-arity-includes?
    procedure-arity-mask
    procedure-arity
    procedure?
    procedure-rename
    apply
    keyword<?
    string->keyword
    keyword->string
    keyword?
    eq-hash-code
    hash-has-key?
    hash-clear!
    hash-remove!
    hash-set!
    hash-ref
    make-hasheq
    make-empty-hasheq
    fasl->s-exp
    s-exp->fasl
    port-next-location
    write-byte
    get-output-bytes
    open-output-bytes
    string-port?
    symbol->immutable-string
    symbol-interned?
    string->uninterned-symbol
    symbol->string
    string->symbol
    symbol<?
    symbol=?
    symbol?
    string-drop
    string-trim-right
    string-trim-left
    string->bytes/utf-8
    list->string
    string->list
    string-append
    string-fill!
    string-copy
    string-copy!
    substring
    string-length
    string-set!
    string-ref
    make-string
    string<?
    string=?
    string?
    bytes=?
    list->bytes
    bytes->list
    bytes-append
    bytes-fill!
    bytes-copy
    bytes-copy!
    subbytes
    bytes-length
    bytes-set!
    bytes-ref
    make-bytes
    bytes?
    vector->list
    vector-split-at
    vector-drop-right
    vector-drop
    vector-take
    vector-empty?
    vector-copy!
    vector-fill!
    vector-length
    vector-set!
    vector-ref
    make-vector
    vector?
    vector
    byte?
    flround
    fl>=
    fl<=
    fl>
    fl<
    fl=
    fl/
    fl*
    fl-
    fl+
    flonum?
    unsafe-fxquotient
    fxquotient
    fx>=
    fx<=
    fx<
    fx>
    fx=
    fx*
    fx-
    fx+
    fxzero?
    fixnum?
    round
    inexact->exact
    exact-positive-integer?
    exact-nonnegative-integer?
    exact-integer?
    exact?
    integer?
    sub1
    add1
    negative?
    positive?
    zero?
    >=
    <=
    >
    <
    =
    /
    *
    -
    +
    number->string
    equal?
    eqv?
    eq?
    char-whitespace?
    integer->char
    char->integer
    char=?
    char?
    not
    boolean?
    void
    make-void
    void?
    list*
    for-each
    map
    alt-reverse
    memq
    reverse
    append
    list-tail
    list-ref
    length
    list?
    list
    cdr
    car
    cons
    null?
    pair?
    string
    bytes
    vector-immutable
    set-box!
    unbox
    box
    set-boxed!
    unboxed
    boxed
    box-immutable
    values
    current-inspector
    struct-type?
    struct?
    struct-mutator-procedure?
    struct-accessor-procedure?
    struct-predicate-procedure?
    struct-constructor-procedure?
    make-struct-field-mutator
    make-struct-field-accessor
    make-struct-type
    raise-unbound-variable-reference)
  )

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
