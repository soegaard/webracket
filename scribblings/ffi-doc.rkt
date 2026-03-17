#lang racket/base

(provide load-ffi-doc-index
         load-ffi-docs
         ffi-doc-description
         ffi-doc-description/required
         ffi-doc-default-description
         ffi-doc-mdn-path
         ffi-doc-mdn-path/required
         ffi-doc-mdn-path/default
         ffi-doc-assert-metadata-complete
         ffi-doc-argument-contracts
         ffi-doc-result-contract
         ffi-doc-result-types
         ffi-doc-return-note
         ffi-doc-signature-line)

(require racket/match
         racket/string
         "../define-foreign.rkt"
         "../structs.rkt")

(define (load-ffi-doc-index ffi-path)
  (for/hash ([fd (in-list (ffi-file->foreign-docs ffi-path))])
    (define f (foreign-doc-foreign fd))
    (values (foreign-racket-name f) fd)))

(define (load-ffi-docs ffi-path)
  (ffi-file->foreign-docs ffi-path))

(define (ffi-doc-description idx name)
  (define fd (hash-ref idx name #f))
  (and fd (foreign-doc-doc fd)))

(define (ffi-doc-description/required idx name)
  (define desc (ffi-doc-description idx name))
  (unless desc
    (error 'ffi-doc-description/required
           "missing #:doc for binding: ~a"
           name))
  desc)

(define (ffi-doc-mdn-path idx name)
  (define fd (hash-ref idx name #f))
  (and fd (foreign-doc-mdn fd)))

(define (ffi-doc-mdn-path/required idx name)
  (define path (ffi-doc-mdn-path idx name))
  (unless path
    (error 'ffi-doc-mdn-path/required
           "missing #:mdn for binding: ~a"
           name))
  path)

(define (name->string v)
  (cond
    [(symbol? v) (symbol->string v)]
    [(string? v) v]
    [else (format "~a" v)]))

(define (symbol->js-camel-case sym)
  (define parts (string-split (name->string sym) "-"))
  (cond
    [(null? parts) ""]
    [else
     (string-append
      (car parts)
      (apply string-append
             (for/list ([p (in-list (cdr parts))])
               (string-titlecase p))))]))

(define (module->mdn-class mod)
  (define m (string->symbol (string-downcase (name->string mod))))
  (case m
    [(window) "Window"]
    [(document) "Document"]
    [(element) "Element"]
    [(event) "Event"]
    [(performance) "Performance"]
    [else (string-titlecase (name->string mod))]))

(define (ffi-doc-default-description idx name)
  (define fd (hash-ref idx name #f))
  (unless fd
    (error 'ffi-doc-default-description "unknown binding: ~a" name))
  (define f (foreign-doc-foreign fd))
  (define mod (name->string (foreign-module-name f)))
  (define host (name->string (foreign-host-name f)))
  (format "Calls host binding ~a/~a." mod host))

(define (ffi-doc-mdn-path/default idx name)
  (or (ffi-doc-mdn-path idx name)
      (let* ([fd (hash-ref idx name #f)]
             [f  (and fd (foreign-doc-foreign fd))])
        (unless f
          (error 'ffi-doc-mdn-path/default "unknown binding: ~a" name))
        ;; Fallback to class-level page when an explicit member path is unavailable.
        (module->mdn-class (foreign-module-name f)))))

(define (ffi-type->contract t [result? #f])
  (case t
    [(extern/raw) 'external]
    [(extern) (if result? '(or/c #f external) 'external)]
    [(extern/undefined) (if result? '(or/c undefined external) 'external)]
    [(extern/nullish) (if result? '(or/c #f undefined external) 'external)]
    [(value) 'any/c]
    [(string) 'string?]
    [(boolean) 'boolean?]
    [(i32 u32) 'exact-integer?]
    [(f64) 'real?]
    [else 'any/c]))

(define (ffi-doc-argument-contracts idx name)
  (define fd (hash-ref idx name #f))
  (unless fd
    (error 'ffi-doc-argument-contracts "unknown binding: ~a" name))
  (define f (foreign-doc-foreign fd))
  (for/list ([t (in-list (foreign-argument-types f))])
    (ffi-type->contract t #f)))

(define (ffi-doc-result-contract idx name)
  (define fd (hash-ref idx name #f))
  (unless fd
    (error 'ffi-doc-result-contract "unknown binding: ~a" name))
  (define f (foreign-doc-foreign fd))
  (define rets (foreign-result-types f))
  (cond
    [(null? rets) 'void?]
    [(= (length rets) 1) (ffi-type->contract (car rets) #t)]
    [else 'any/c]))

(define (ffi-doc-assert-metadata-complete idx names [what "binding set"])
  (for ([name (in-list names)])
    (unless (hash-has-key? idx name)
      (error 'ffi-doc-assert-metadata-complete
             "unknown binding in ~a: ~a"
             what
             name))
    (unless (ffi-doc-description idx name)
      (error 'ffi-doc-assert-metadata-complete
             "missing #:doc for binding in ~a: ~a"
             what
             name))
    (unless (ffi-doc-mdn-path idx name)
      (error 'ffi-doc-assert-metadata-complete
             "missing #:mdn for binding in ~a: ~a"
             what
             name))))

(define (ffi-doc-signature-line idx name [source-tag "dom.ffi"])
  (define fd (hash-ref idx name #f))
  (unless fd
    (error 'ffi-doc-signature-line "unknown FFI binding: ~a" name))
  (define f (foreign-doc-foreign fd))
  (define args (foreign-argument-types f))
  (define rets (foreign-result-types f))
  (define args-str
    (if (null? args)
        "()"
        (string-join (map symbol->string args) " ")))
  (define ret-str
    (if (null? rets)
        "()"
        (string-join (map symbol->string rets) " ")))
  (format "~a : ~a -> ~a     [~a]"
          ;; Keep exact source tag in docs (for example: dom.ffi)
          ;; rather than the JS module-name field (for example: document).
          (symbol->string (foreign-racket-name f))
          args-str
          ret-str
          source-tag))

(define (ffi-doc-result-types idx name)
  (define fd (hash-ref idx name #f))
  (unless fd
    (error 'ffi-doc-result-types "unknown FFI binding: ~a" name))
  (foreign-result-types (foreign-doc-foreign fd)))

(define (ffi-doc-return-note idx name)
  (define rets (ffi-doc-result-types idx name))
  (cond
    [(null? rets) #f]
    [(equal? rets '(extern/raw)) "Returns an external value."]
    [(equal? rets '(extern)) "Returns #f or an external value."]
    [(equal? rets '(extern/undefined)) "Returns undefined or an external value."]
    [(equal? rets '(extern/nullish)) "Returns #f, undefined, or an external value."]
    [(equal? rets '(string)) "Returns a string value."]
    [(equal? rets '(boolean)) "Returns a boolean value."]
    [(equal? rets '(i32)) "Returns a 32-bit signed integer value."]
    [(equal? rets '(u32)) "Returns a 32-bit unsigned integer value."]
    [(equal? rets '(f64)) "Returns a 64-bit floating-point value."]
    [else #f]))
