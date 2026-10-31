#lang racket/base

;;; Provider Fragment Splice
;;;
;;; Utilities for combining separately compiled module-bridge provider and
;;; program fragments into one split-runtime-reference program module. The
;;; provider fragment is intended to be cacheable by provider fingerprint; the
;;; program fragment remains user-specific.

(provide (struct-out provider-fragment-splice-info)
         splice-provider+program-modules)

(require racket/list
         racket/match
         racket/set)

(struct provider-fragment-splice-info
  (provider-form-count
   program-form-count
   dropped-provider-global-import-count
   colliding-program-names
   provider-init-form-count
   program-init-form-count
   spliced-form-count)
  #:transparent)

;; function-import-form? : any/c -> boolean?
;;   Check whether form is a top-level imported function declaration.
(define (function-import-form? form)
  (match form
    [`(func ,_ ,parts ...)
     (for/or ([part (in-list parts)])
       (match part
         [`(import ,(? string?) ,(? string?)) #t]
         [_ #f]))]
    [_ #f]))

;; global-import-form? : any/c -> boolean?
;;   Check whether form is a top-level imported global declaration.
(define (global-import-form? form)
  (match form
    [`(global ,_ ,parts ...)
     (for/or ([part (in-list parts)])
       (match part
         [`(import ,(? string?) ,(? string?)) #t]
         [_ #f]))]
    [_ #f]))

;; tag-import-form? : any/c -> boolean?
;;   Check whether form is a top-level imported tag declaration.
(define (tag-import-form? form)
  (match form
    [`(tag ,_ ,parts ...)
     (for/or ([part (in-list parts)])
       (match part
         [`(import ,(? string?) ,(? string?)) #t]
         [_ #f]))]
    [_ #f]))

;; top-level-import-form? : any/c -> boolean?
;;   Check whether form is an old-style top-level import declaration.
(define (top-level-import-form? form)
  (match form
    [`(import ,_ ...) #t]
    [_ #f]))

;; import-form? : any/c -> boolean?
;;   Check whether form introduces an imported Wasm item.
(define (import-form? form)
  (or (top-level-import-form? form)
      (function-import-form? form)
      (global-import-form? form)
      (tag-import-form? form)))

;; prefix-form? : any/c -> boolean?
;;   Recognize forms that should appear before definitions.
(define (prefix-form? form)
  (and (pair? form)
       (or (memq (car form) '(type rec import))
           (import-form? form))))

;; provider-global-import-form? : any/c -> boolean?
;;   Check whether a program form imports one provider bridge global.
(define (provider-global-import-form? form)
  (match form
    [`(global ,_ (import "provider" ,_) ,_ ...) #t]
    [_ #f]))

;; export-form? : any/c -> boolean?
;;   Check whether form is a standalone export.
(define (export-form? form)
  (match form
    [`(export ,_ ...) #t]
    [_ #f]))

;; named-form-name : any/c symbol? -> (or/c symbol? #f)
;;   Return the name for a named top-level form of the given kind.
(define (named-form-name form kind)
  (and (pair? form)
       (eq? (car form) kind)
       (pair? (cdr form))
       (symbol? (cadr form))
       (cadr form)))

;; definition-name : any/c -> (or/c symbol? #f)
;;   Return the top-level definition name, excluding imported declarations.
(define (definition-name form)
  (and (not (import-form? form))
       (for/or ([kind (in-list '(func global data tag))])
         (named-form-name form kind))))

;; definition-names : (listof any/c) -> (set/c symbol?)
;;   Return the set of names defined by a module-body form sequence.
(define (definition-names forms)
  (for/seteq ([form (in-list forms)]
              #:do [(define name (definition-name form))]
              #:when name)
    name))

;; rename-symbol : symbol? symbol? -> symbol?
;;   Prefix a WAT symbol while preserving its readable base name.
(define (rename-symbol prefix sym)
  (string->symbol
   (format "$~a:~a" prefix (regexp-replace #rx"^\\$" (symbol->string sym) ""))))

;; rewrite-symbols : any/c hash? -> any/c
;;   Rewrite every symbol that appears in renames.
(define (rewrite-symbols datum renames)
  (cond
    [(symbol? datum) (hash-ref renames datum datum)]
    [(pair? datum)
     (cons (rewrite-symbols (car datum) renames)
           (rewrite-symbols (cdr datum) renames))]
    [(vector? datum)
     (list->vector
      (for/list ([item (in-vector datum)])
        (rewrite-symbols item renames)))]
    [else datum]))

;; local-operation-form? : any/c -> boolean?
;;   Check whether a form references function-local state.
(define (local-operation-form? datum)
  (cond
    [(pair? datum)
     (or (memq (car datum) '(local.get local.set local.tee))
         (local-operation-form? (car datum))
         (local-operation-form? (cdr datum)))]
    [(vector? datum)
     (for/or ([item (in-vector datum)])
       (local-operation-form? item))]
    [else #f]))

;; remove-entry-export : any/c symbol? -> any/c
;;   Remove the inline `(export "entry")` marker from one entry function.
(define (remove-entry-export form entry-name)
  (match form
    [`(func ,(? (lambda (name) (eq? name entry-name)) name) ,parts ...)
     `(func ,name
            ,@(for/list ([part (in-list parts)]
                         #:unless (equal? part '(export "entry")))
                part))]
    [_ form]))

;; entry-run-body-start? : any/c -> boolean?
;;   Check whether a function body form starts the generated entry run block.
(define (entry-run-body-start? form)
  (match form
    [`(block $entry-done ,_ ...) #t]
    [_ #f]))

;; func-declaration-part? : any/c -> boolean?
;;   Check whether a function part belongs in the declaration prefix.
(define (func-declaration-part? part)
  (match part
    [`(param ,_ ...) #t]
    [`(result ,_ ...) #t]
    [`(local ,_ ...) #t]
    [`(type ,_) #t]
    [_ #f]))

;; extract-entry-function :
;;   any/c symbol? symbol? -> (values (or/c any/c #f) (listof any/c))
;;   Convert a full generated entry into a body function and return wrapper init forms.
(define (extract-entry-function form entry-name who)
  (match form
    [`(func ,(? (lambda (name) (eq? name entry-name)) name) ,parts ...)
     (define-values (declaration-parts body-parts)
       (splitf-at parts func-declaration-part?))
     (define-values (pre-run-forms run-forms)
       (splitf-at body-parts (lambda (part)
                               (not (entry-run-body-start? part)))))
     (unless (pair? run-forms)
       (raise-arguments-error who
                              "could not find $entry-done block"
                              "entry name" entry-name))
     (define-values (wrapper-init-forms local-pre-run-forms)
       (for/fold ([wrapper-init-forms '()]
                  [local-pre-run-forms '()])
                 ([part (in-list pre-run-forms)])
         (if (local-operation-form? part)
             (values wrapper-init-forms (cons part local-pre-run-forms))
             (values (cons part wrapper-init-forms) local-pre-run-forms))))
     (values `(func ,name
                    ,@declaration-parts
                    ,@(reverse local-pre-run-forms)
                    ,@run-forms)
             (reverse wrapper-init-forms))]
    [_ (values #f '())]))

;; replace-entry-function :
;;   (listof any/c) symbol? symbol? symbol? -> (values (listof any/c) (listof any/c))
;;   Collect the generated bootstrap and leave local/application work in the entry.
(define (replace-entry-function forms entry-name bootstrap-name who)
  (define found? #f)
  (define init-forms '())
  (define bootstrap-form
    (findf (lambda (form)
             (match form
               [`(func ,(== bootstrap-name eq?) ,_ ...) #t]
               [_ #f]))
           forms))
  (define replaced-forms
    (for/list ([form (in-list forms)]
               #:unless (eq? form bootstrap-form))
      (define-values (replacement replacement-init-forms)
        (extract-entry-function form entry-name who))
      (cond
        [replacement
         (set! found? #t)
         (set! init-forms replacement-init-forms)
         replacement]
        [else form])))
  (unless found?
    (raise-arguments-error who
                           "could not find generated entry function"
                           "entry name" entry-name))
  ;; Older inline entries are still accepted. New entries call a generated
  ;; zero-local helper: merge its instructions at that exact point, so shared
  ;; runtime initialization is deduplicated across provider and program.
  (define expanded-init-forms
    (if bootstrap-form
        (append-map (lambda (form)
                      (if (equal? form `(call ,bootstrap-name))
                          (cddr bootstrap-form)
                          (list form)))
                    init-forms)
        init-forms))
  (values replaced-forms expanded-init-forms))

;; append-unique-forms : (listof any/c) ... -> (listof any/c)
;;   Append form lists while preserving first occurrence.
(define (append-unique-forms . form-lists)
  (define seen (mutable-set))
  (define out-reversed '())
  (for* ([forms (in-list form-lists)]
         [form (in-list forms)])
    (unless (set-member? seen form)
      (set-add! seen form)
      (set! out-reversed (cons form out-reversed))))
  (reverse out-reversed))

;; split-body : (listof any/c) -> (values (listof any/c) (listof any/c))
;;   Split module forms into prefix declarations and body forms.
(define (split-body forms)
  (values (filter prefix-form? forms)
          (filter (lambda (form) (not (prefix-form? form))) forms)))

;; splice-provider+program-modules :
;;   list? list? [#:who symbol?] -> (values list? provider-fragment-splice-info?)
;;   Splice provider and program modules into one runnable program module.
(define (splice-provider+program-modules provider-module
                                         program-module
                                         #:who [who 'splice-provider+program-modules])
  (match provider-module
    [`(module ,_ ...) (void)]
    [_ (raise-argument-error who "WAT module datum" provider-module)])
  (match program-module
    [`(module ,_ ...) (void)]
    [_ (raise-argument-error who "WAT module datum" program-module)])
  (define raw-provider-forms (cdr provider-module))
  (define raw-program-forms (cdr program-module))
  (define provider-forms/no-exports
    (filter (lambda (form) (not (export-form? form))) raw-provider-forms))
  (define program-forms/no-provider-imports
    (filter (lambda (form) (not (provider-global-import-form? form)))
            raw-program-forms))
  (define provider-entry-name '$provider-fragment:entry)
  (define program-entry-name '$program-fragment:entry)
  (define provider-renames
    (hasheq '$entry provider-entry-name))
  (define provider-forms
    (for/list ([form (in-list provider-forms/no-exports)])
      (remove-entry-export (rewrite-symbols form provider-renames)
                           provider-entry-name)))
  (define provider-definitions
    (definition-names provider-forms))
  (define program-definitions
    (definition-names program-forms/no-provider-imports))
  (define colliding-program-names
    (sort (set->list (set-intersect provider-definitions program-definitions))
          symbol<?))
  (define program-renames
    (for/hasheq ([name (in-list (cons '$entry colliding-program-names))])
      (values name
              (if (eq? name '$entry)
                  program-entry-name
                  (rename-symbol "program-fragment" name)))))
  (define program-forms
    (for/list ([form (in-list program-forms/no-provider-imports)])
      (remove-entry-export (rewrite-symbols form program-renames)
                           program-entry-name)))
  (define-values (provider-body-entry-forms provider-init-forms)
    (replace-entry-function provider-forms provider-entry-name '$entry-bootstrap who))
  (define-values (program-body-entry-forms program-init-forms)
    (replace-entry-function program-forms program-entry-name
                            (hash-ref program-renames '$entry-bootstrap '$entry-bootstrap)
                            who))
  (define-values (provider-prefix provider-body)
    (split-body provider-body-entry-forms))
  (define-values (program-prefix program-body)
    (split-body program-body-entry-forms))
  (define wrapper-entry-form
    `(func $entry
           (export "entry")
           (result i32)
           (call $entry-bootstrap)
           (drop (call ,provider-entry-name))
           (call ,program-entry-name)))
  (define spliced-module
    `(module
      ,@(append-unique-forms provider-prefix program-prefix)
      ,@provider-body
      ,@program-body
      (func $entry-bootstrap
            ,@(append-unique-forms provider-init-forms program-init-forms))
      ,wrapper-entry-form))
  (values spliced-module
          (provider-fragment-splice-info
           (length raw-provider-forms)
           (length raw-program-forms)
           (- (length raw-program-forms)
              (length program-forms/no-provider-imports))
           colliding-program-names
           (length provider-init-forms)
           (length program-init-forms)
           (length (cdr spliced-module)))))

(module+ test
  (require rackunit)

  (define sample-provider-module
    '(module
      (type $T (func))
      (import "env" "memory" (memory $memory 1))
      (global $provider-value (mut i32) (i32.const 0))
      (export "$provider-value" (global $provider-value))
      (func $entry-uncaught-exception (param $e i32))
      (func $entry
            (export "entry")
            (result i32)
            (local $entry-exn i32)
            (global.set $provider-value (i32.const 1))
            (local.set $entry-exn (i32.const 0))
            (block $entry-done (br $entry-done))
            (i32.const 0))))

  (define sample-program-module
    '(module
      (type $T (func))
      (import "env" "memory" (memory $memory 1))
      (global $provider-value (import "provider" "$provider-value") (mut i32))
      (global $program-value (mut i32) (i32.const 0))
      (func $entry-uncaught-exception (param $e i32))
      (func $entry
            (export "entry")
            (result i32)
            (local $entry-exn i32)
            (global.set $program-value (i32.const 2))
            (local.set $entry-exn (i32.const 0))
            (block $entry-done (br $entry-done))
            (i32.const 0))))

  (define-values (sample-spliced sample-info)
    (splice-provider+program-modules sample-provider-module
                                     sample-program-module))

  (check-equal? (provider-fragment-splice-info-provider-form-count sample-info) 6)
  (check-equal? (provider-fragment-splice-info-program-form-count sample-info) 6)
  (check-equal? (provider-fragment-splice-info-dropped-provider-global-import-count
                 sample-info)
                1)
  (check-equal? (provider-fragment-splice-info-colliding-program-names sample-info)
                '($entry-uncaught-exception))
  (check-not-false
   (member '(func $entry (export "entry") (result i32)
                  (call $entry-bootstrap)
                  (drop (call $provider-fragment:entry))
                  (call $program-fragment:entry))
           (cdr sample-spliced)))
  (check-not-false
   (member '(func $entry-bootstrap
                  (global.set $provider-value (i32.const 1))
                  (global.set $program-value (i32.const 2)))
           (cdr sample-spliced)))
  (check-false
   (for/or ([form (in-list (cdr sample-spliced))])
     (equal? form
             '(global $provider-value
                      (import "provider" "$provider-value")
                      (mut i32)))))
  (check-not-false
   (member '(func $program-fragment:entry-uncaught-exception (param $e i32))
           (cdr sample-spliced)))

  (define helper-provider-module
    '(module
      (func $shared-init (import "core" "init"))
      (func $provider-init)
      (func $entry-bootstrap (call $shared-init) (call $provider-init))
      (func $entry (export "entry") (result i32) (local $x i32)
            (call $entry-bootstrap)
            (local.set $x (i32.const 1))
            (block $entry-done (br $entry-done))
            (local.get $x))))
  (define helper-program-module
    '(module
      (func $shared-init (import "core" "init"))
      (func $program-init)
      (func $entry-bootstrap (call $shared-init) (call $program-init))
      (func $entry (export "entry") (result i32) (local $x i32)
            (call $entry-bootstrap)
            (local.set $x (i32.const 2))
            (block $entry-done (br $entry-done))
            (local.get $x))))
  (define-values (helper-spliced helper-info)
    (splice-provider+program-modules helper-provider-module helper-program-module))
  ;; Both fragments generate the same helper name. Renaming must still allow
  ;; their shared initializer to run once, before either fragment's application.
  ;; Local initialization stays in each body, outside the merged bootstrap.
  (check-equal?
   helper-spliced
   '(module
     (func $shared-init (import "core" "init"))
     (func $provider-init)
     (func $provider-fragment:entry (result i32) (local $x i32)
           (local.set $x (i32.const 1))
           (block $entry-done (br $entry-done))
           (local.get $x))
     (func $program-init)
     (func $program-fragment:entry (result i32) (local $x i32)
           (local.set $x (i32.const 2))
           (block $entry-done (br $entry-done))
           (local.get $x))
     (func $entry-bootstrap
           (call $shared-init) (call $provider-init) (call $program-init))
     (func $entry (export "entry") (result i32)
           (call $entry-bootstrap)
           (drop (call $provider-fragment:entry))
           (call $program-fragment:entry))))
  (check-equal? (provider-fragment-splice-info-provider-init-form-count helper-info) 2)
  (check-equal? (provider-fragment-splice-info-program-init-form-count helper-info) 2))
