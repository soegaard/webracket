#lang racket/base
(require               "fully.rkt" "primitives.rkt")
(provide (all-from-out "fully.rkt" "primitives.rkt"))

#;(require (for-syntax
          (only-in racket/base
                   define-syntax-rule
                   define-syntax
                   syntax-case                   
                   syntax/loc
                   with-syntax
                   generate-temporaries)))
#;(provide (for-syntax define-syntax-rule
                     define-syntax
                     syntax-case
                     syntax
                     syntax/loc
                     with-syntax
                     generate-temporaries))
(provide define-syntax define-syntax-rule)

; core
(provide (rename-out [#%plain-module-begin #%module-begin]
                     [web-#%app            #%app]
                     [lambda lambda] ; [#%plain-lambda       lambda]
                     [#%plain-lambda       λ]))

(provide _)


(require (for-syntax racket/base
                     racket/path
                     setup/dirs
                     syntax/location
                     "browser-options.rkt"))

(begin-for-syntax
  (define include-lib-seen (make-hash)))

(define-for-syntax (webracket-collection-dir)
  (define collection-main (collection-file-path "main.rkt" "webracket"))
  (or (path-only collection-main) (current-directory)))

(define-for-syntax (library-entrypoint-path lib-sym)
  (define collection-dir     (webracket-collection-dir))
  (define lib-dir            (build-path collection-dir "lib" (symbol->string lib-sym)))
  (define multi-main-path    (build-path lib-dir "main.rkt"))
  (define multi-browser-path (build-path lib-dir "main-browser.rkt"))
  (define single-file-path   (build-path collection-dir
                                         "lib" "libs"
                                         (string-append (symbol->string lib-sym) ".rkt")))
  (cond
    [(directory-exists? lib-dir)
     (cond
       [(browser-mode?)
        (unless (file-exists? multi-browser-path)
          (error 'include-lib
                 (string-append "browser entrypoint not found for library `"
                                (symbol->string lib-sym)
                                "`")))
        (values multi-browser-path #t)]
       [(file-exists? multi-main-path)
        (values multi-main-path #t)]
       [else
        (error 'include-lib
               (string-append "entrypoint not found for library `"
                              (symbol->string lib-sym)
                              "`"))])]
    [(file-exists? single-file-path)
     (values single-file-path #f)]
    [else (values #f #f)]))

;; syntax-source-directory : syntax? -> path?
;;   Return the directory containing the syntax source, or current dir.
(define-for-syntax (syntax-source-directory stx)
  (define src (syntax-source stx))
  (cond
    [(path-string? src)
     (define src-path (path->complete-path src))
     (or (path-only src-path) (current-directory))]
    [else
     (current-directory)]))

;; relative-path-string : path? path? -> string?
;;   Compute a relative pathname string from base-dir to target-path.
(define-for-syntax (relative-path-string base-dir target-path)
  (path->string
   (find-relative-path (path->complete-path base-dir)
                       (path->complete-path target-path))))

;; syntax-source->string : syntax? -> string?
;;   Convert the syntax source to a printable string, or "".
(define-for-syntax (syntax-source->string stx)
  (define src (syntax-source stx))
  (cond
    [(path? src)   (path->string src)]
    [(string? src) src]
    [else          ""]))

(define-syntax (web-lambda stx)
  (syntax-case stx ()
    [(_lambda formals body0 body ...)
     (syntax/loc stx
       (#%plain-lambda formals body0 body ...))]))

(define-syntax (web-λ stx)
  (syntax-case stx ()
    [(_lambda formals body0 body ...)
     (syntax/loc stx
       (#%plain-lambda formals body0 body ...))]))

(define-for-syntax (keyword-token? stx)
  (keyword? (syntax-e stx)))

(define-for-syntax (rewrite-keyword-token stx)
  (if (keyword-token? stx)
      (datum->syntax stx (list 'quote (syntax-e stx)) stx stx)
      stx))

;; web-#%app : like #%plain-app, but rewrite keyword tokens to keyword values.
;; This enables keyword surface syntax in non-macro call positions.
(define-syntax (web-#%app stx)
  (syntax-case stx ()
    [(_ f arg ...)
     (let* ([args (syntax->list #'(arg ...))]
            [has-keyword? (and args (ormap keyword-token? args))])
       (if has-keyword?
           (with-syntax ([(arg* ...) (map rewrite-keyword-token args)])
             (syntax/loc stx
               (#%plain-app f arg* ...)))
           (syntax/loc stx
             (#%plain-app f arg ...))))]
    [(_ . _)
     (raise-syntax-error 'web-#%app "bad application form" stx)]))

;; require-lib : (require-lib lib-id) -> require form
;;   Require `webracket/lib/libs/<lib-id>` at top level.
(define-syntax (require-lib stx)
  (define who 'require-lib)
  (define context (syntax-local-context))
  (unless (memq context '(module top-level))
    (raise-syntax-error who "may only be used at top level" stx))
  (syntax-case stx ()
    [(_ lib-id)
     (begin
       (unless (identifier? #'lib-id)
         (raise-syntax-error who "expected a library identifier" stx #'lib-id))
       (define lib-sym (syntax-e #'lib-id))
       (unless (symbol? lib-sym)
         (raise-syntax-error who "expected a library identifier" stx #'lib-id))
       (define collection-main (collection-file-path "main.rkt" "webracket"))
       (define collection-dir (or (path-only collection-main) (current-directory)))
       (define lib-file-name (string-append (symbol->string lib-sym) ".rkt"))
       (define lib-path (build-path collection-dir "lib" "libs" lib-file-name))
       (unless (file-exists? lib-path)
         (raise-syntax-error
          who
          (string-append "unknown library `" (symbol->string lib-sym) "`")
          stx
          #'lib-id))

       (define mod-id
         (datum->syntax stx
                        (string->symbol
                         (string-append "webracket/lib/libs/" (symbol->string lib-sym)))))
       (with-syntax ([module-id mod-id])
         (syntax/loc stx
           (require module-id))))]
    [_
     (raise-syntax-error who "expected `(require-lib lib-id)`" stx)]))

;; include-lib : (include-lib lib-id) -> include/reader form
;;   Include or require a WebRacket library depending on layout.
;;   Single-file libraries come from `lib/libs/<lib-id>.rkt`.
;;   Multi-file libraries come from `lib/<lib-id>/main.rkt` or
;;   `lib/<lib-id>/main-browser.rkt` when browser mode is active.
;;   Within one compilation run, the first include expands the library text
;;   and later includes of the same library expand to `(void)`.
(define-syntax (include-lib stx)
  (define who     'include-lib)
  (define context (syntax-local-context))
  (unless (memq context '(module top-level))
    (raise-syntax-error who "may only be used at top level" stx))
  (syntax-case stx ()
    [(_ lib-id)
     (let* ([lib-id-stx #'lib-id])
       (unless (identifier? lib-id-stx)
         (raise-syntax-error who "expected a library identifier" stx lib-id-stx))
       (define lib-sym (syntax-e lib-id-stx))
       (unless (symbol? lib-sym)
         (raise-syntax-error who "expected a library identifier" stx lib-id-stx))
       (define lib-key lib-sym)
       (when (hash-has-key? include-lib-seen lib-key)
         (syntax/loc stx (void)))
       (define-values (lib-path multi-file?)
         (library-entrypoint-path lib-sym))
       (unless lib-path
         (raise-syntax-error who
                             (string-append "unknown library `"
                                            (symbol->string lib-sym) "`")
                             stx
                             lib-id-stx))
       (hash-set! include-lib-seen lib-key #t)
       (define lib-file
         (relative-path-string
          (syntax-source-directory lib-id-stx)
          lib-path))
       (datum->syntax stx
                      `(include/reader ,lib-file read-syntax/skip-first-line)
                      stx
                      stx))]
    [_
     (raise-syntax-error who "expected `(include-lib lib-id)`" stx)]))

;; Constants


(provide null empty true false pi eof undefined unsafe-undefined)
(require (only-in racket/base              null eof)
         (only-in racket/bool              true false)
         (only-in racket/list              empty)
         (only-in racket/math              pi)
         (only-in racket/undefined         undefined)
         (only-in racket/unsafe/undefined  unsafe-undefined))


;; Note: When bootstrapping we need implementations
;;       for these.

;; 3.2 Importing and Exporting: require and provide
(require (only-in racket/base #%require #%provide))
(provide #%require #%provide)


;; 3.3 Literals: quote and #%datum
(require (only-in racket/base #%datum))
(provide #%datum)

;; 3.4 Expression Wrapper: #%expression
(require (only-in racket/base #%expression))
(provide #%expression)

;; 3.5 Variable References and #%top
(require (only-in racket/base #%top))
(provide #%top)

;; 3.6 Locations: #%variable-reference
(require (only-in racket/base #%variable-reference))
(provide #%variable-reference)

;; 3.7 Procedure Applications and #%app
;; Only #%plain-app for now.
;; (require (only-in racket/base #%app))
;; (provide #%app)

;; 3.8 Procedure Expressions: lambda, case-lambda
(require (only-in racket/base case-lambda))
(provide case-lambda)

;; 3.9 Local binding: let, let*, letrec, ...
(require (only-in racket/base let let* letrec
                  let-values let*-values letrec-values))
(provide let let* letrec
         let-values let*-values letrec-values)

;; 3.10 Local Definitions: local
(require (only-in racket/local local))
(provide local)

;; 3.11 Constructing Graphs: shared
;; (require (only-in racket/shared shared))
;; (provide shared)


;; 3.12 Conditionals: cond, or, and
(require (only-in racket/base cond else => or and))
(provide cond else => or and)

;; 3.13. Dispatch: case
(require (only-in racket/base case))
(provide case)

;; 3.14. Definitions: define
; (require (only-in racket/base define))
(provide define define-values)
; define-values provided from `fully`
(provide include-lib require-lib)

;; 3.15 Sequencing: begin, begin0
;;   Provided form `fully`.

;; 3.16 Guard Evaluation: when, unless
(require (only-in racket/base when unless))
(provide when unless)

;; 3.17 Assignment: set!, set!-values
(require (only-in racket/base set!-values))
(provide set!-values)
; Note: set!-values expands into let-values + set!


;; 3.18 Iterations and comprehensions
(require (only-in racket/base
                  for  for/list  for/vector  for/sum  for/fold  for/or  for/and  for/first
                  for* for*/list for*/vector for*/sum for*/fold for*/or for*/and for*/first
                  in-list in-vector in-range in-naturals in-string
                  ))
(provide          for  for/list  for/vector  for/sum  for/fold  for/or  for/and  for/first
                  for* for*/list for*/vector for*/sum for*/fold for*/or for*/and for*/first
                  in-list in-vector in-range in-naturals in-string
                  )

;; 3.19 Continuations marks
;; (require (only-in racket/base with-continuation-mark))
;; (provide with-continuation-mark)

;; 3.20 Quasiquoting
(require (only-in racket/base quasiquote unquote unquote-splicing))
(provide quasiquote unquote unquote-splicing)


;; 3.22 Interaction Wrapper
(require (only-in racket/base #%top-interaction))
(provide #%top-interaction)

;; 3.23 Blocks
(require (only-in racket/block block))
(provide block)

;; 4.7.1 Additional Symbol Functions
(require (only-in racket/symbol symbol->immutable-string))
(provide symbol->immutable-string)

;; 4.9.1 Additional Keyword Functions
(require (only-in racket/keyword keyword->immutable-string))
(provide keyword->immutable-string)

;; 4.10
(require (only-in racket/base
                  null))
(provide null)

;; 5.   Structures
;; 5.1  Defining Structure Types:  struct

(provide struct)


;; 10.2.3 Handling Exceptions
(provide with-handlers)

;; 13.10

(require (prefix-in rkt: racket/fasl))

; We simplify s-exp->fasl here in order to avoid keyword functions

(define (s-exp->fasl v [out #f]) (rkt:s-exp->fasl v out))

(define (fasl->s-exp bs)
  (unless (bytes? bs)
    (raise-argument-error 'fasl->s-exp "bytes?" bs))
  (define in (open-input-bytes bs))
  (define v (rkt:fasl->s-exp in))
  (close-input-port in)
  v)

(provide s-exp->fasl fasl->s-exp)

;; 14. 10

(provide namespace-variable-value-simple)

(define (namespace-variable-value-simple ns sym)
  (namespace-variable-value-simple sym #t #f ns))
