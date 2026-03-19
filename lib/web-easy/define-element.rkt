#lang webracket

(require (for-syntax racket/base
                     racket/list
                     racket/path))

;;;
;;; web-easy define/element
;;;

;; define/element is web-easy internal and included with:
;;   (include/reader "define-element.rkt" read-syntax/skip-first-line)
;;
;; Phase 1 constructor shape:
;;   (define/element Name Base fixed-positional ...)
;;
;; Example:
;;   (define/element H1 html-element 'h1)
;; expands to a function constructor `H1` that:
;;   - expects one positional content argument
;;   - supports #:attrs as an escape hatch
;;   - treats any other keyword as a root HTML attribute

;; keyword->attr-key : keyword? -> symbol?
;;   Convert keyword value to attribute symbol key.
(define (keyword->attr-key kw)
  (string->symbol (keyword->string kw)))

;; string-prefix?/internal : string? string? -> boolean?
;;   Return #t when s starts with prefix.
(define (string-prefix?/internal s prefix)
  (define n  (string-length s))
  (define pn (string-length prefix))
  (and (<= pn n)
       (string=? (substring s 0 pn) prefix)))

;; wildcard-attr-symbol? : symbol? -> boolean?
;;   Return #t for wildcard symbols like 'data-*.
(define (wildcard-attr-symbol? sym)
  (define s (symbol->string sym))
  (and (>= (string-length s) 2)
       (char=? (string-ref s (- (string-length s) 1)) #\*)
       (char=? (string-ref s (- (string-length s) 2)) #\-)))

;; wildcard-attr-prefix : symbol? -> string?
;;   Return wildcard prefix for symbol like 'data-* -> "data-".
(define (wildcard-attr-prefix sym)
  (define s (symbol->string sym))
  (substring s 0 (- (string-length s) 1)))

;; attr-key-allowed? : symbol? (or/c #f list?) -> boolean?
;;   Check exact and wildcard matches for allowed attribute symbols.
(define (attr-key-allowed? attr-key allowed-attrs)
  (or (not allowed-attrs)
      (let ([key-s (symbol->string attr-key)])
        (let loop ([rest allowed-attrs])
          (cond
            [(null? rest) #f]
            [(eq? attr-key (car rest)) #t]
            [(and (symbol? (car rest))
                  (wildcard-attr-symbol? (car rest))
                  (string-prefix?/internal key-s
                                           (wildcard-attr-prefix (car rest))))
             #t]
            [else
             (loop (cdr rest))])))))

;; attrs-entry->pair : any/c -> (or/c pair? #f)
;;   Normalize attr entry as (cons symbol? any/c) when possible.
(define (attrs-entry->pair entry)
  (cond
    [(and (list? entry)
          (= (length entry) 2)
          (symbol? (car entry)))
     (cons (car entry) (cadr entry))]
    [(and (pair? entry)
          (symbol? (car entry)))
     (cons (car entry) (cdr entry))]
    [else
     #f]))

;; reject-procedure-attrs! : symbol? any/c -> void?
;;   Reject attribute values that are procedures (direct or observable current value).
(define (reject-procedure-attrs! who attrs)
  (when (list? attrs)
    (for-each
     (lambda (entry)
       (define normalized (attrs-entry->pair entry))
       (when normalized
         (define value (cdr normalized))
         (define current-value
           (if (obs? value)
               (obs-peek value)
               value))
         (when (procedure? current-value)
           (error who
                  "attribute value cannot be procedure: ~a"
                  (car normalized)))))
     attrs)))

;; parse-element-call-args : symbol? list? (or/c #f list?) -> values list? any/c any/c
;;   Parse raw args into positional-rev, raw-#:attrs value, and extra attrs (reversed).
(define (parse-element-call-args who all-args allowed-attrs)
  (define attrs '())
  (define positional-rev '())
  (define extra-attrs-rev '())
  (let loop ([rest all-args])
    (cond
      [(null? rest)
       (values positional-rev attrs extra-attrs-rev)]
      [(keyword? (car rest))
       (when (null? (cdr rest))
         (error who "missing value after keyword argument: ~a" (car rest)))
       (define kw (car rest))
       (define v  (cadr rest))
       (cond
         [(eq? kw '#:attrs)
          (set! attrs v)]
         [else
          (define attr-key (keyword->attr-key kw))
          (when (not (attr-key-allowed? attr-key allowed-attrs))
            (error who "unknown keyword argument: ~a" kw))
          (set! extra-attrs-rev
                (cons (cons attr-key v)
                      extra-attrs-rev))])
       (loop (cddr rest))]
      [else
       (set! positional-rev (cons (car rest) positional-rev))
       (loop (cdr rest))])))

;; validate-positional-count! : symbol? list? (or/c natural? 'any) -> void?
;;   Validate positional argument count against constructor arity unless arity is 'any.
(define (validate-positional-count! who positional expected-positional-count)
  (unless (eq? expected-positional-count 'any)
    (unless (= (length positional) expected-positional-count)
      (error who
             "wrong number of positional arguments (expected ~a, got ~a)"
             expected-positional-count
             (length positional)))))

;; merge-element-attrs : symbol? any/c list? -> any/c
;;   Merge raw #:attrs payload with keyword-derived attrs.
(define (merge-element-attrs who attrs extra-attrs)
  (cond
    [(eq? attrs #f)
     (if (null? extra-attrs)
         #f
         extra-attrs)]
    [(null? extra-attrs)
     attrs]
    [(list? attrs)
     (append attrs extra-attrs)]
    [else
     (error who "expected #:attrs as list? or #f, got ~a" attrs)]))

;; normalize-element-call : symbol? list? (or/c natural? 'any) (or/c #f list?) list? -> values list? any/c
;;   Parse constructor call payload into positional args and merged attrs.
(define (normalize-element-call who all-args expected-positional-count allowed-attrs required-keywords)
  (define (keyword-count+ counts kw)
    (cond
      [(null? counts)
       (list (cons kw 1))]
      [(eq? (caar counts) kw)
       (cons (cons kw (add1 (cdar counts)))
             (cdr counts))]
      [else
       (cons (car counts)
             (keyword-count+ (cdr counts) kw))]))
  (define (keyword-count-ref counts kw)
    (cond
      [(null? counts) 0]
      [(eq? (caar counts) kw) (cdar counts)]
      [else (keyword-count-ref (cdr counts) kw)]))
  (define-values (positional-rev attrs extra-attrs-rev)
    (parse-element-call-args who all-args allowed-attrs))
  (define required-counts
    (let loop ([rest all-args]
               [counts '()])
      (cond
        [(null? rest) counts]
        [(keyword? (car rest))
         (define kw (car rest))
         (if (memq kw required-keywords)
             (loop (cddr rest) (keyword-count+ counts kw))
             (loop (cddr rest) counts))]
        [else
         (loop (cdr rest) counts)])))
  (for-each
   (lambda (required-kw)
     (define count (keyword-count-ref required-counts required-kw))
     (unless (= count 1)
       (error who "expected exactly one ~a keyword argument" required-kw)))
   required-keywords)
  (define positional (reverse positional-rev))
  (validate-positional-count! who positional expected-positional-count)
  (define extra-attrs (reverse extra-attrs-rev))
  (define merged-attrs (merge-element-attrs who attrs extra-attrs))
  (reject-procedure-attrs! who merged-attrs)
  (values positional merged-attrs))

;; define/element : (define/element Name Base fixed-positional ...) -> definition
;;   Define generic uppercase element constructor.
(define-syntax (define/element stx)
  (define who 'define/element)
  ;; html-attr-table-cache : (or/c #f (hash/c string? list?))
  ;;   Cache table parsed from spec html-element-attributes.sexp.
  (define html-attr-table-cache #f)
  ;; load-html-attr-table : syntax? -> hash?
  ;;   Read HTML attribute table from `spec/html-element-attributes.sexp`.
  (define (load-html-attr-table use-stx)
    (if html-attr-table-cache
        html-attr-table-cache
        (let* ([src0 (syntax-source use-stx)]
               [src (cond
                      [(path? src0) src0]
                      [(string? src0) (string->path src0)]
                      [else #f])])
          (unless src
            (raise-syntax-error who
                                "cannot resolve source location for html attribute table lookup"
                                use-stx))
          (define table-path
            (build-path (path-only src)
                        "spec"
                        "html-element-attributes.sexp"))
          (unless (file-exists? table-path)
            (raise-syntax-error who
                                "missing spec/html-element-attributes.sexp (run tools/fetch-html-element-attributes.mjs)"
                                use-stx))
          (define datum
            (with-input-from-file table-path
              (lambda ()
                (read))))
          (unless (and (list? datum)
                       (pair? datum)
                       (eq? (car datum) 'html-element-attributes))
            (raise-syntax-error who
                                "malformed spec/html-element-attributes.sexp header"
                                use-stx))
          (define table-entry
            (let loop ([rest (cdr datum)])
              (cond
                [(null? rest) #f]
                [(and (pair? (car rest))
                      (eq? (caar rest) 'table))
                 (car rest)]
                [else
                 (loop (cdr rest))])))
          (unless (and table-entry (list? table-entry))
            (raise-syntax-error who
                                "malformed spec/html-element-attributes.sexp table"
                                use-stx))
          (define ht (make-hash))
          (for-each
           (lambda (row)
             (when (and (list? row)
                        (= (length row) 2)
                        (string? (car row))
                        (list? (cadr row)))
               (define attr-symbols
                 (filter symbol?
                         (map string->symbol
                              (filter string? (cadr row)))))
               (hash-set! ht (car row) attr-symbols)))
           (cdr table-entry))
          (set! html-attr-table-cache ht)
          ht)))
  ;; allowed-attrs-for-html-tag : syntax? symbol? -> list?
  ;;   Return deduplicated allowed attrs from globals + element-specific attrs.
  (define (allowed-attrs-for-html-tag use-stx tag-sym)
    (define table (load-html-attr-table use-stx))
    (define globals (hash-ref table "*" '()))
    (define specific (hash-ref table (symbol->string tag-sym) '()))
    (remove-duplicates (append globals
                               specific
                               '(data-* aria-*))))
  (syntax-case stx ()
    [(_ name base fixed ...
        #:required-keywords (required-kw ...)
        #:positional-count positional-count)
     (identifier? #'name)
     (let* ([base-sym (syntax-e #'base)]
            [fixed-list (syntax->list #'(fixed ...))]
            [required-kws (syntax->datum #'(required-kw ...))]
            [count-datum/raw (syntax-e #'positional-count)]
            [count-datum (if (eq? count-datum/raw 'any)
                             'any
                             count-datum/raw)]
            [_ (unless (or (eq? count-datum 'any)
                           (and (integer? count-datum) (>= count-datum 0)))
                 (raise-syntax-error who
                                     "expected non-negative integer or `any` for #:positional-count"
                                     stx
                                     #'positional-count))]
            [allowed-attrs
             (if (or (eq? base-sym 'html-element)
                     (eq? base-sym 'html-element-children))
                 (let ()
                   (unless (and fixed-list (= (length fixed-list) 1))
                     (raise-syntax-error who
                                         "html-element wrappers must provide exactly one fixed tag literal"
                                         stx
                                         #'(fixed ...)))
                   (define fixed0 (car fixed-list))
                   (syntax-case fixed0 (quote)
                     [(quote tag-sym)
                      (if (symbol? (syntax-e #'tag-sym))
                          (allowed-attrs-for-html-tag stx (syntax-e #'tag-sym))
                          (raise-syntax-error who
                                              "expected quoted symbol tag in html-element wrapper"
                                              stx
                                              fixed0))]
                     [_
                      (raise-syntax-error who
                                          "expected quoted symbol tag in html-element wrapper"
                                          stx
                                          fixed0)]))
                 #f)])
       (define count-expr-datum
         (if (eq? count-datum 'any)
             '(quote any)
             count-datum))
       (with-syntax ([name/sym (datum->syntax #'name (syntax-e #'name) #'name #'name)]
                     [allowed-attrs-stx (datum->syntax stx (list 'quote allowed-attrs) stx stx)]
                     [required-kws-stx (datum->syntax stx (list 'quote required-kws) stx stx)]
                     [expected-count-stx (datum->syntax stx count-expr-datum stx stx)])
         (cond
           [(and (eq? base-sym 'html-element) (equal? count-datum 0))
            #'(define (name . all-args)
                (define-values (positional attrs)
                  (normalize-element-call 'name/sym
                                          all-args
                                          expected-count-stx
                                          allowed-attrs-stx
                                          required-kws-stx))
                (base fixed ...
                      ""
                      #:attrs attrs))]
           [else
            #'(define (name . all-args)
                (define-values (positional attrs)
                  (normalize-element-call 'name/sym
                                          all-args
                                          expected-count-stx
                                          allowed-attrs-stx
                                          required-kws-stx))
                (apply base
                       (append (list fixed ...)
                               positional
                               (list #:attrs attrs))))])))]
    [(_ name base fixed ...)
     (identifier? #'name)
     (let* ([base-sym (syntax-e #'base)]
            [fixed-list (syntax->list #'(fixed ...))]
            [allowed-attrs
             (if (or (eq? base-sym 'html-element)
                     (eq? base-sym 'html-element-children))
                 (let ()
                   (unless (and fixed-list (= (length fixed-list) 1))
                     (raise-syntax-error who
                                         "html-element wrappers must provide exactly one fixed tag literal"
                                         stx
                                         #'(fixed ...)))
                   (define fixed0 (car fixed-list))
                   (syntax-case fixed0 (quote)
                     [(quote tag-sym)
                      (if (symbol? (syntax-e #'tag-sym))
                          (allowed-attrs-for-html-tag stx (syntax-e #'tag-sym))
                          (raise-syntax-error who
                                              "expected quoted symbol tag in html-element wrapper"
                                              stx
                                              fixed0))]
                     [_
                     (raise-syntax-error who
                                          "expected quoted symbol tag in html-element wrapper"
                                          stx
                                          fixed0)]))
                 #f)])
       (with-syntax ([name/sym (datum->syntax #'name (syntax-e #'name) #'name #'name)]
                     [allowed-attrs-stx (datum->syntax stx (list 'quote allowed-attrs) stx stx)])
         #'(define (name . all-args)
             (define-values (positional attrs)
               (normalize-element-call 'name/sym all-args 1 allowed-attrs-stx '()))
             (base fixed ...
                   (car positional)
                   #:attrs attrs))))]
    [_
     (raise-syntax-error who
                         "expected `(define/element Name Base fixed-positional ...)`"
                         stx)]))
