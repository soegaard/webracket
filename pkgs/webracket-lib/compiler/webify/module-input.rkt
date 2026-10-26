#lang racket/base

(provide webify-module-linklet-input
         webify-module-linklet-input?
         webify-module-linklet-input-name
         webify-module-linklet-input-linklet
         webify-module-linklet-input-imports
         webify-module-linklet-manifest
         webify-module-linklet-manifest?
         webify-module-linklet-manifest-inputs
         webify-module-linklet-manifest-roots
         datum->webify-module-linklet-manifest
         read-webify-module-linklet-manifest
         webify-module-linklet-manifest->registry
         webify-module-linklet-input-dependency-exports
         webify-module-linklet-inputs-dependency-exports
         webify-resolve-module-linklet-imports
         webify-module-resolution-outcome
         webify-module-resolution-outcome?
         webify-module-resolution-outcome-local
         webify-module-resolution-outcome-dependency
         webify-module-resolution-outcome-provider-export
         webify-module-resolution-outcome-status
         module-phase0-exported?
         class-internal-class-struct-import?
         compiled-primitive-alias-hint
         webify-resolve-module-linklet-imports/outcomes
         webify-module-linklet-input/resolve-imports
         webify-register-module-linklet-input!
         webify-module-linklet-inputs->registry)

(require racket/list
         racket/match
         racket/set
         racket/string
         racket/unsafe/undefined
         "../private/primitives-db.rkt"
         "../private/parameters.rkt"
         "primitive-modules.rkt"
         "module-registry.rkt"
         "read-provider.rkt"
         "regexp-provider.rkt")

(struct webify-module-linklet-input (name linklet imports) #:transparent)
(struct webify-module-linklet-manifest (inputs roots) #:transparent)
(struct webify-module-resolution-outcome
  (local dependency provider-export status)
  #:transparent)
(struct dependency-export-index
  (rows by-dependency dependency-rank by-export candidate-cache)
  #:transparent)

;; module-path-available? : module-path? -> boolean?
;;   Check whether the current host can resolve a module path to an existing source.
(define (module-path-available? module-path)
  (with-handlers ([exn:fail? (lambda (_exn) #f)])
    (define resolved
      (resolved-module-path-name
       (module-path-index-resolve
        (module-path-index-join module-path #f)
        #t)))
    (cond
      [(path-string? resolved) (file-exists? resolved)]
      [else resolved])))

;; validate-roots! : symbol? any/c -> void?
;;   Reject manifest roots that cannot identify module-linklet inputs.
(define (validate-roots! who roots)
  (unless (and (list? roots)
               (andmap symbol? roots))
    (raise-argument-error who "(listof symbol?)" roots)))

;; datum->webify-module-linklet-input : any/c -> webify-module-linklet-input?
;;   Parse one manifest input clause.
(define (cache-readable->datum v)
  (cond
    [(eq? v 'webracket-cache:void)
     (void)]
    [(eq? v 'webracket-cache:unsafe-undefined)
     unsafe-undefined]
    [(pair? v)
     (match v
       [`(webracket-cache:path ,s)
        (string->path s)]
       [`(webracket-cache:vector ,elems ...)
        (list->vector (map cache-readable->datum elems))]
       [`(webracket-cache:box ,elem)
        (box (cache-readable->datum elem))]
       [`(webracket-cache:prefab ,key ,fields ...)
        (apply make-prefab-struct key (map cache-readable->datum fields))]
       [`(webracket-cache:hash ,kind ,entries)
        (define decoded-entries
          (for/list ([entry (in-list entries)])
            (match entry
              [(list key value)
               (cons (cache-readable->datum key)
                     (cache-readable->datum value))]
              [_ (error 'cache-readable->datum
                        "invalid cached hash entry: ~e"
                        entry)])))
        (for/fold ([h (case kind
                        [(eq)  (hasheq)]
                        [(eqv) (hasheqv)]
                        [else  (hash)])])
                  ([entry (in-list decoded-entries)])
          (hash-set h (car entry) (cdr entry)))]
       [else
        (cons (cache-readable->datum (car v))
              (cache-readable->datum (cdr v)))])]
    [else v]))

(define (datum->webify-module-linklet-input datum)
  (match datum
    [`(input ,(? symbol? name) ,linklet (imports ,imports))
     (webify-module-linklet-input name
                                  (cache-readable->datum linklet)
                                  (cache-readable->datum imports))]
    [`(input ,(? symbol? name)
             ,linklet
             (resolve-imports (dependency-exports ,dependency-exports)))
     (webify-module-linklet-input/resolve-imports
      name
      (cache-readable->datum linklet)
      (cache-readable->datum dependency-exports))]
    [`(input ,(? symbol? name)
             ,linklet
             (resolve-imports (dependency-exports ,dependency-exports)
                              (dependencies ,dependencies)))
     (webify-module-linklet-input/resolve-imports
      name
      (cache-readable->datum linklet)
      (cache-readable->datum dependency-exports)
      #:dependencies (cache-readable->datum dependencies))]
    [_
     (raise-arguments-error 'datum->webify-module-linklet-manifest
                            "expected module-linklet input clause"
                            "clause" datum)]))

;; datum->webify-module-linklet-manifest : any/c -> webify-module-linklet-manifest?
;;   Parse a datum handoff format for extracted module-linklet inputs.
(define (datum->webify-module-linklet-manifest datum)
  (match datum
    [`(webify-module-linklet-manifest
       (roots ,roots)
       ,input-clauses ...)
     (validate-roots! 'datum->webify-module-linklet-manifest roots)
     (webify-module-linklet-manifest
      (map datum->webify-module-linklet-input input-clauses)
      roots)]
    [_
     (raise-arguments-error 'datum->webify-module-linklet-manifest
                            "expected webify module-linklet manifest datum"
                            "datum" datum)]))

;; read-webify-module-linklet-manifest : path-string? -> webify-module-linklet-manifest?
;;   Read one module-linklet manifest datum from a file.
(define (read-webify-module-linklet-manifest path)
  (call-with-input-file path
    (lambda (in)
      (datum->webify-module-linklet-manifest (read in)))))

;; linklet-import-bindings : symbol? any/c -> (listof (list/c symbol? symbol?))
;;   Extract flat local/external import bindings from a linklet datum.
(define (linklet-import-bindings who linklet)
  (match linklet
    [`(linklet ,importss ,_exports . ,_body)
     (unless (and (list? importss)
                  (for/and ([group (in-list importss)])
                    (and (list? group)
                         (for/and ([binding (in-list group)])
                           (match binding
                             [(list (? symbol?) (? symbol?)) #t]
                             [_                              #f])))))
       (raise-arguments-error who
                              "linklet imports must be grouped pairs of symbols"
                              "linklet" linklet))
     (define seen (make-hasheq))
     (define bindings
       (for/fold ([acc '()]) ([binding (in-list (apply append importss))])
         (match-define (list local external) binding)
         (define prior (hash-ref seen local #f))
         (cond
           [(not prior)
            (hash-set! seen local external)
            (cons binding acc)]
           [(symbolic-name=? prior external) acc]
           [else
            (raise-arguments-error who
                                   "conflicting duplicate linklet import names are not supported"
                                   "import" local
                                   "first external" prior
                                   "second external" external)])))
     (reverse bindings)]
    [_
     (raise-arguments-error who
                            "expected a linklet datum"
                            "linklet" linklet)]))

;; linklet-exports : symbol? any/c -> (listof symbol?)
;;   Extract flat external exports from a linklet datum.
(define (linklet-exports who linklet)
  (match linklet
    [`(linklet ,_importss ,exports . ,_body)
     (define normalized-exports
       (and (list? exports)
            (for/list ([export (in-list exports)])
              (match export
                [(list (? symbol?) (? symbol? external)) external]
                [(? symbol? external) external]
                [_ #f]))))
     (unless (and normalized-exports
                  (andmap symbol? normalized-exports))
       (raise-arguments-error who
                              "linklet exports must be symbols or internal/external symbol pairs"
                              "exports" exports))
     (define duplicate (check-duplicates normalized-exports))
     (when duplicate
       (raise-arguments-error who
                              "duplicate linklet exports are not supported"
                              "export" duplicate))
     normalized-exports]
    [_
     (raise-arguments-error who
                            "expected a linklet datum"
                            "linklet" linklet)]))

;; validate-dependency-exports! : symbol? any/c -> void?
;;   Reject dependency export tables that cannot resolve import sources.
(define (validate-dependency-exports! who dependency-exports)
  (unless (list? dependency-exports)
    (raise-argument-error who "(listof (list/c symbol? (listof symbol?)))" dependency-exports))
  (for ([entry (in-list dependency-exports)])
    (match entry
      [(list (? symbol?) exports)
       (unless (and (list? exports)
                    (andmap symbol? exports))
         (raise-argument-error who "(listof symbol?)" exports))]
      [_
       (raise-argument-error who "(list/c symbol? (listof symbol?))" entry)]))
  (define duplicate (check-duplicates (map car dependency-exports)))
  (when duplicate
    (raise-arguments-error who
                           "duplicate dependency export entries are not supported"
                           "dependency" duplicate)))

;; validate-import-dependencies! : symbol? any/c -> void?
;;   Reject explicit local-import to dependency-module mappings.
(define (validate-import-dependencies! who dependencies)
  (unless (list? dependencies)
    (raise-argument-error who "(listof (list/c symbol? symbol?))" dependencies))
  (for ([dependency (in-list dependencies)])
    (match dependency
      [(list (? symbol?) (? symbol?))
       (void)]
      [_
       (raise-argument-error who "(list/c symbol? symbol?)" dependency)]))
  (define duplicate (check-duplicates (map car dependencies)))
  (when duplicate
    (raise-arguments-error who
                           "duplicate import dependency entries are not supported"
                           "import" duplicate)))

;; validate-import-aliases! : symbol? any/c -> void?
;;   Reject explicit local-import to dependency export mappings.
(define (validate-import-aliases! who aliases)
  (unless (list? aliases)
    (raise-argument-error who "(listof (list/c symbol? symbol? symbol?))" aliases))
  (for ([alias (in-list aliases)])
    (match alias
      [(list (? symbol?) (? symbol?) (? symbol?))
       (void)]
      [_
       (raise-argument-error who "(list/c symbol? symbol? symbol?)" alias)]))
  (define seen (make-hasheq))
  (for ([alias (in-list aliases)])
    (match-define (list import module export) alias)
    (define prior (hash-ref seen import #f))
    (cond
      [(not prior)
       (hash-set! seen import (list module export))]
      [else
       (match-define (list prior-module prior-export) prior)
       (unless (and (eq? module prior-module)
                    (symbolic-name=? export prior-export))
         (raise-arguments-error who
                                "conflicting duplicate import alias entries are not supported"
                                "import" import
                                "first alias" prior
                                "second alias" (list module export)))])))

;; validate-reflection-hints! : symbol? any/c -> void?
;;   Reject metadata-derived local-import provider hints.
(define (validate-reflection-hints! who hints)
  (unless (list? hints)
    (raise-argument-error who "(listof reflection-hint?)" hints))
  (for ([hint (in-list hints)])
    (match hint
      [(list (? symbol?) (? symbol?) (? symbol?))
       (void)]
      [(list (? symbol?) (? symbol?) (? symbol?) (? symbol?))
       (void)]
      [_
       (raise-argument-error
        who
        "(or/c (list/c local dependency export) (list/c module local dependency export))"
        hint)])))

;; dependency-exports-ref : symbol? (listof dependency-exports?) symbol? -> (listof symbol?)
;;   Look up the exports declared for one dependency module.
(define (dependency-exports-ref who dependency-exports name)
  (define rows
    (if (dependency-export-index? dependency-exports)
        (dependency-export-index-rows dependency-exports)
        dependency-exports))
  (define found
    (if (dependency-export-index? dependency-exports)
        (let ([exports (hash-ref (dependency-export-index-by-dependency dependency-exports)
                                 name
                                 #f)])
          (and exports (list name exports)))
        (assoc name dependency-exports)))
  (cond
    [found (cadr found)]
    [(primitive-module-name? name)
     (for/list ([info (in-list primitive-db)])
       (primitive-info-name info))]
    [else
     (raise-arguments-error who
                            "import dependency has no export table entry"
                            "dependency" name
                            "available dependencies"
                            (map car rows))]))

;; make-dependency-export-index : dependency-exports -> dependency-export-index?
;;   Build per-resolution lookup state for dependency export rows.
(define (make-dependency-export-index dependency-exports)
  (define by-dependency
    (for/hasheq ([entry (in-list dependency-exports)])
      (match-define (list dependency exports) entry)
      (values dependency exports)))
  (define dependency-rank
    (for/hasheq ([entry (in-list dependency-exports)]
                 [rank (in-naturals)])
      (match-define (list dependency _exports) entry)
      (values dependency rank)))
  (dependency-export-index
   dependency-exports
   by-dependency
   dependency-rank
   (make-dependency-export-reverse-index dependency-exports)
   (make-hasheq)))

;; make-dependency-export-reverse-index : dependency-exports -> hasheq?
;;   Index export-name variants to dependency modules in original row order.
(define (make-dependency-export-reverse-index dependency-exports)
  (define by-export (make-hasheq))
  (for ([entry (in-list dependency-exports)])
    (match-define (list dependency exports) entry)
    (define seen-keys (make-hasheq))
    (for* ([export (in-list exports)]
           [key (in-list (dependency-export-key-symbols export))]
           #:unless (hash-has-key? seen-keys key))
      (hash-set! seen-keys key #t)
      (hash-update! by-export key (lambda (dependencies)
                                    (cons dependency dependencies))
                    '())))
  (for/hasheq ([(key dependencies) (in-hash by-export)])
    (values key (reverse dependencies))))

;; dependency-exports-has-dependency? : dependency-exports symbol? -> boolean?
;;   Check whether dependency has an explicit export row.
(define (dependency-exports-has-dependency? dependency-exports dependency)
  (if (dependency-export-index? dependency-exports)
      (hash-has-key? (dependency-export-index-by-dependency dependency-exports)
                     dependency)
      (and (assoc dependency dependency-exports) #t)))

;; dependency-export-candidates : dependency-exports symbol? -> (listof symbol?)
;;   Find provider rows with an export symbolically matching external.
(define (dependency-export-candidates dependency-exports external)
  (define (scan rows)
    (remove-duplicates
     (for/list ([entry (in-list rows)]
                #:do [(match-define (list dependency exports) entry)]
                #:when (for/or ([export (in-list exports)])
                         (symbolic-name=? export external)))
       dependency)
     eq?))
  (cond
    [(dependency-export-index? dependency-exports)
     (hash-ref! (dependency-export-index-candidate-cache dependency-exports)
                external
                (lambda ()
                  (dependency-export-index-candidates dependency-exports
                                                      external)))]
    [else
     (scan dependency-exports)]))

;; dependency-export-index-candidates : dependency-export-index? symbol? -> (listof symbol?)
;;   Find reverse-index candidates while preserving dependency row order.
(define (dependency-export-index-candidates dependency-exports external)
  (define seen (make-hasheq))
  (define rough-candidates
    (for*/fold ([candidates '()])
               ([key (in-list (dependency-export-key-symbols external))]
                [dependency (in-list
                             (hash-ref (dependency-export-index-by-export dependency-exports)
                                       key
                                       '()))]
                #:unless (hash-has-key? seen dependency))
      (hash-set! seen dependency #t)
      (cons dependency candidates)))
  (define by-dependency
    (dependency-export-index-by-dependency dependency-exports))
  (define dependency-rank
    (dependency-export-index-dependency-rank dependency-exports))
  (sort
   (filter (lambda (dependency)
             (for/or ([export (in-list (hash-ref by-dependency dependency '()))])
               (symbolic-name=? export external)))
           rough-candidates)
   <
   #:key (lambda (dependency)
           (hash-ref dependency-rank dependency +inf.0))))

;; kernel-dependency? : symbol? -> boolean?
;;   Recognize reflected origins for Racket's primitive kernel module.
(define (kernel-dependency? dependency)
  (primitive-module-name? dependency))

;; generated-base-name : symbol? -> string?
;;   Strip common decompiler-generated prefixes and numeric suffixes.
(define (generated-base-name name)
  (define text (symbol->string name))
  (define without-prefix (regexp-replace #rx"^new[:-]" text ""))
  (define without-index-prefix (regexp-replace #rx"^[0-9]+/" without-prefix ""))
  (regexp-replace #rx"[0-9]*[.][0-9]+$" without-index-prefix ""))

;; generated-name? : symbol? -> boolean?
;;   Detect common decompiler-generated imports such as `sort.1` or `1/reverse`.
(define (generated-name? name)
  (not (string=? (symbol->string name)
                 (generated-base-name name))))

;; dependency-export-key-symbols : symbol? -> (listof symbol?)
;;   Return exact and generated-base lookup keys for dependency export matching.
(define (dependency-export-key-symbols name)
  (remove-duplicates
   (list name
         (string->symbol (symbol->string name))
         (string->symbol (generated-base-name name)))
   eq?))

;; symbolic-name=? : symbol? symbol? -> boolean?
;;   Compare generated linklet names by printed identity when eq? is too strict.
(define (symbolic-name=? a b)
  (or (eq? a b)
      (string=? (symbol->string a) (symbol->string b))
      (string=? (generated-base-name a) (generated-base-name b))
      (string=? (generated-base-name a) (symbol->string b))
      (string=? (symbol->string a) (generated-base-name b))))

;; module-phase0-exported? : symbol? symbol? -> boolean?
;;   Check the host module's current phase-0 export table without loading it.
(define module-phase0-exports-cache (make-hasheq))

(define (module-phase0-exported? module-name external)
  (define exports
    (hash-ref!
     module-phase0-exports-cache
     module-name
     (lambda ()
       (with-handlers ([exn:fail? (lambda (_exn) '())])
         (define-values (value-exports syntax-exports)
           (module->exports module-name))
         (remove-duplicates
          (for*/list ([phase-exports (in-list (append value-exports syntax-exports))]
                      #:when (and (pair? phase-exports)
                                  (zero? (car phase-exports)))
                      [export (in-list (cdr phase-exports))]
                      #:when (and (pair? export) (symbol? (car export))))
            (car export))
          eq?)))))
  (and (memq external exports) #t))

;; class-internal-class-struct-import? : symbol? -> boolean?
;;   Recognize class-struct bindings imported directly by class-internal.
(define (class-internal-class-struct-import? external)
  (and (module-path-available? 'racket/private/class-struct)
       (for/or ([name (in-list '(_object?
                                 allowed-opaque-pred
                                 allowed-opaque?
                                 class-abstract-ids
                                 class-all-field-ids
                                 class-beta-methods
                                 class-check-undef?
                                 class-dynamic-idxs
                                 class-dynamic-projs
                                 class-field-ht
                                 class-field-ids
                                 class-field-pub-width
                                 class-field-ref
                                 class-field-set!
                                 class-field-width
                                 class-fixup
                                 class-ictc-classes
                                 class-init
                                 class-init-args
                                 class-init-mode
                                 class-inner-projs
                                 class-insp-mk
                                 class-int-methods
                                 class-make-object
                                 class-meth-flags
                                 class-method-ht
                                 class-method-ictcs
                                 class-method-ids
                                 class-method-width
                                 class-methods
                                 class-name
                                 class-no-super-init?
                                 class-obj-inspector
                                 class-object?
                                 class-orig-cls
                                 class-pos
                                 class-self-interface
                                 class-serializer
                                 class-struct:object
                                 class-super-methods
                                 class-supers
                                 class?
                                 exn:fail:object?
                                 find-method/who
                                 just-check-existence
                                 just-check-existence?
                                 make-class
                                 make-exn:fail:object
                                 object-ref
                                 object/c-wrapper-info
                                 object/c-wrapper-info-blame+neg-party
                                 object/c-wrapper-info-ctc
                                 object/c-wrapper-info-do-not-check-class-field-accessor-or-mutator-access?
                                 object/c-wrapper-info-methods-proc
                                 object/c-wrapper-info-neg-fields
                                 object/c-wrapper-info-opaque-fields
                                 object/c-wrapper-info-opaque-methods
                                 object/c-wrapper-info-pos-fields
                                 object/c-wrapper-info-val
                                 object/c-wrapper-info?
                                 prop:object
                                 set-class-field-ref!
                                 set-class-field-set!!
                                 set-class-fixup!
                                 set-class-ictc-classes!
                                 set-class-init!
                                 set-class-make-object!
                                 set-class-method-ht!
                                 set-class-object?!
                                 set-class-orig-cls!
                                 set-class-serializer!
                                 set-class-struct:object!
                                 struct:allowed-opaque
                                 struct:class
                                 struct:exn:fail:object
                                 struct:object/c-wrapper-info))])
         (symbolic-name=? external name))))

;; webify-primitive-name? : symbol? -> boolean?
;;   Check whether a kernel-origin binding is already a known Webify primitive.
(define (webify-primitive-name? name)
  (for/or ([info (in-list primitive-db)])
    (symbolic-name=? (primitive-info-name info) (kernel-primitive-base-name name))))

;; kernel-primitive-base-name : symbol? -> symbol?
;;   Strip the host expander's `kernel:` prefix from synthetic #%kernel names.
(define (kernel-primitive-base-name name)
  (define text (symbol->string name))
  (cond
    [(string-prefix? text "kernel:")
     (string->symbol (substring text (string-length "kernel:")))]
    [else name]))

;; kernel-primitive-export-names : primitive-info? -> (listof symbol?)
;;   List the public and host-expander alias names for one #%kernel primitive.
(define (kernel-primitive-export-names info)
  (define name (primitive-info-name info))
  (define base (kernel-primitive-base-name name))
  (if (eq? name base)
      (list base (string->symbol (format "kernel:~a" (symbol->string base))))
      (list name base)))

;; kernel-dependency-export-row : -> dependency-export-row
;;   Expose only supported Webify primitives as resolvable #%kernel exports.
(define (kernel-dependency-export-row)
  (list '#%kernel
        (remove-duplicates
         (append*
          (for/list ([info (in-list primitive-db)])
            (kernel-primitive-export-names info))))))

;; dependency-exports/add-kernel : dependency-exports -> dependency-exports
;;   Add a synthetic #%kernel row so unhinted kernel primitive imports can resolve.
(define io-format-provider-owned-export-symbols
  '(format fprintf printf eprintf))

(define io-print-provider-owned-export-symbols
  '(print println write writeln))

(define (provider-owned-primitive-alias-scope-blocked? module-name)
  (define local-report-names
    (current-module-bridge-local-report-names))
  (define support-provider-names
    (current-module-bridge-support-provider-names))
  (define (implementation-prefix? prefix)
    (and (symbol? module-name)
         (string-prefix? (symbol->string module-name) prefix)))
  (or (memq module-name
            '(expander/read
              expander/read-syntax
              racket-src/expander/read-provider-root.rkt
              racket-src/expander/read-syntax-provider-root.rkt))
      (and local-report-names
           (symbol? module-name)
           (not (set-member? local-report-names module-name)))
      (and support-provider-names
           (symbol? module-name)
           (set-member? support-provider-names module-name))
      (implementation-prefix? "racket-src/expander/read/")
      (implementation-prefix? "racket-src/expander/read-syntax/")
      (implementation-prefix? "racket-src/io/print/")))

(define (staged-read-private-context? module-name)
  (define (implementation-prefix? prefix)
    (and (symbol? module-name)
         (string-prefix? (symbol->string module-name) prefix)))
  (or (memq module-name
            '(expander/read
              expander/read-syntax
              racket-src/expander/read-provider-root.rkt
              racket-src/expander/read-syntax-provider-root.rkt))
      (implementation-prefix? "racket-src/expander/read/")
      (implementation-prefix? "racket-src/expander/read-syntax/")
      (implementation-prefix? "racket-src/expander/common/")
      (implementation-prefix? "racket-src/expander/host/")))

(define (blocked-provider-owned-dependencies module-name)
  (append
   (if (or (provider-owned-primitive-alias-scope-blocked? module-name)
           (staged-read-private-context? module-name))
       '(expander/read)
       '())
   (if (or (provider-owned-primitive-alias-scope-blocked? module-name)
           (staged-read-private-context? module-name))
       '(expander/read-syntax)
       '())))

(define (expander-read-private-provider-candidate? dependency)
  (and (symbol? dependency)
       (let ([s (symbol->string dependency)])
         (or (equal? s "racket-src/expander/read-provider-root.rkt")
             (string-prefix? s "racket-src/expander/read/")
             (string-prefix? s "racket-src/expander/common/")
             (string-prefix? s "racket-src/expander/host/")))))

(define (expander-read-syntax-private-provider-candidate? dependency)
  (and (symbol? dependency)
       (let ([s (symbol->string dependency)])
         (or (equal? s "racket-src/expander/read-syntax-provider-root.rkt")
             (string-prefix? s "racket-src/expander/read-syntax/")))))

(define (dependency-exports/filter-provider-owned-rows dependency-exports [module-name #f])
  (define blocked-dependencies
    (blocked-provider-owned-dependencies module-name))
  (define block-read-private?
    (not (staged-read-private-context? module-name)))
  (if (and (null? blocked-dependencies) (not block-read-private?))
      dependency-exports
      (filter (lambda (row)
                (and (not (memq (car row) blocked-dependencies))
                     (or (not block-read-private?)
                         (and (not (expander-read-private-provider-candidate? (car row)))
                              (not (expander-read-syntax-private-provider-candidate? (car row)))))))
              dependency-exports)))

(define (provider-owned-primitive-dependency-export-rows [module-name #f])
  (if (provider-owned-primitive-alias-scope-blocked? module-name)
      '()
      (let ()
        (define aliases (current-provider-owned-primitive-aliases))
        (define (active-exports exports)
          (for/list ([name (in-list exports)]
                     #:when (set-member? aliases name))
            name))
        (define read-exports
          (active-exports webify-read-public-alias-symbols))
        (define regexp-exports
          (active-exports webify-regexp-provider-owned-symbols))
        (define io-format-exports
          (active-exports io-format-provider-owned-export-symbols))
        (define io-print-exports
          (active-exports io-print-provider-owned-export-symbols))
        (filter values
                (list
                 (and (pair? read-exports)
                      (list 'expander/read read-exports))
                 (and (pair? regexp-exports)
                      (list 'regexp regexp-exports))
                 (and (pair? io-format-exports)
                      (list 'io/format io-format-exports))
                 (and (pair? io-print-exports)
                      (list 'io/print io-print-exports)))))))

;; dependency-exports/add-synthetic-rows : dependency-exports -> dependency-exports
;;   Add synthetic provider rows for kernel and staged provider-owned primitive aliases.
(define (dependency-exports/add-synthetic-rows dependency-exports [module-name #f])
  (for/fold ([rows dependency-exports])
            ([row (in-list
                   (cons (kernel-dependency-export-row)
                         (provider-owned-primitive-dependency-export-rows module-name)))])
    (if (assoc (car row) rows)
        rows
        (append rows (list row)))))

;; dependency-export-ref : symbol? dependency-name (listof symbol?) symbol? -> symbol?
;;   Find the provider export symbol that corresponds to an import's external name.
(define (dependency-export-ref who dependency exports external)
  (define exact-matches
    (for/list ([export (in-list exports)]
               #:when (eq? export external))
      export))
  (define matches
    (for/list ([export (in-list exports)]
               #:when (symbolic-name=? export external))
      export))
  (define generated-primary
    (string->symbol (format "~a.1" (generated-base-name external))))
  (define generated-primary-matches
    (for/list ([export (in-list matches)]
               #:when (equal? (symbol->string export)
                              (symbol->string generated-primary)))
      export))
  (define public-base
    (string->symbol (generated-base-name external)))
  (define public-base-matches
    (for/list ([export (in-list matches)]
               #:when (eq? export public-base))
      export))
  (define selected-matches
    (cond
      [(pair? exact-matches) exact-matches]
      [(pair? public-base-matches) public-base-matches]
      [(pair? generated-primary-matches) generated-primary-matches]
      [else matches]))
  (match selected-matches
    [(list export) export]
    ['()
     (raise-arguments-error who
                            "dependency does not export required name"
                            "dependency" dependency
                            "export" external)]
    [_
     (raise-arguments-error who
                            "dependency export name is ambiguous"
                            "dependency" dependency
                            "export" external
                            "matches" matches)]))

;; broad-base-reexport-module? : symbol? -> boolean?
;;   Identify base aggregators whose public exports should not outrank private providers.
(define (broad-base-reexport-module? dependency)
  (memq dependency '(racket/base racket/private/base)))

;; provider-candidate-reason : symbol? -> symbol?
;;   Classify broad export-scan candidates for ambiguity diagnostics.
(define (provider-candidate-reason dependency)
  (cond
    [(kernel-dependency? dependency)        'kernel-primitive-fallback]
    [(broad-base-reexport-module? dependency) 'broad-base-reexport]
    [else                                  'export-table-candidate]))

;; prefer-provider-candidates : (listof symbol?) -> (listof symbol?)
;;   Prefer concrete module providers over broad aggregators and kernel fallback.
(define (kernel-runtime-dependency? dependency)
  (eq? dependency '#%kernel))

;; symbol-name-prefix? : symbol? string? -> boolean?
;;   Check whether a module-name symbol starts with a collection prefix.
(define (symbol-name-prefix? sym prefix)
  (define s (symbol->string sym))
  (and (<= (string-length prefix) (string-length s))
       (equal? (substring s 0 (string-length prefix)) prefix)))

;; webracket-owned-provider? : symbol? -> boolean?
;;   Recognize WebRacket collection providers that intentionally replace Racket internals.
(define (webracket-owned-provider? dependency)
  (symbol-name-prefix? dependency "webracket/"))

;; racket-private-provider? : symbol? -> boolean?
;;   Recognize installed Racket private providers that may have WebRacket substitutes.
(define (racket-private-provider? dependency)
  (symbol-name-prefix? dependency "racket/private/"))

(define (prefer-webracket-owned-candidates candidates)
  (define webracket-candidates
    (filter webracket-owned-provider? candidates))
  (define has-racket-private?
    (for/or ([dependency (in-list candidates)])
      (racket-private-provider? dependency)))
  (if (and (pair? webracket-candidates) has-racket-private?)
      webracket-candidates
      candidates))

(define (prefer-provider-candidates candidates [external #f])
  (define kernel-candidates
    (filter kernel-runtime-dependency? candidates))
  (define concrete-candidates
    (filter (lambda (dependency)
              (and (not (broad-base-reexport-module? dependency))
                   (not (kernel-dependency? dependency))))
            candidates))
  (cond
    [(null? concrete-candidates) candidates]
    [else (prefer-webracket-owned-candidates concrete-candidates)]))

;; prefer-staged-read-public-candidates : (or/c symbol? #f) symbol? (listof symbol?) -> (listof symbol?)
;;   Prefer the public staged `expander/read` facade for user-module imports of
;;   reader-owned aliases, while keeping private staged-reader modules inside
;;   their implementation closure.
(define (prefer-staged-read-public-candidates module-name external candidates)
  (define public-read-alias?
    (for/or ([name (in-list webify-read-public-alias-symbols)])
      (symbolic-name=? external name)))
  (cond
    [(and public-read-alias?
          (not (provider-owned-primitive-alias-scope-blocked? module-name))
          (memq 'expander/read candidates)
          (for/or ([dependency (in-list candidates)])
            (expander-read-private-provider-candidate? dependency)))
     '(expander/read)]
    [else candidates]))

;; prefer-staged-io-print-public-candidates : (or/c symbol? #f) symbol? (listof symbol?) -> (listof symbol?)
;;   Prefer the staged io/print facade for high-level print aliases.
(define (prefer-staged-io-print-public-candidates module-name external candidates)
  (define public-io-print-alias?
    (for/or ([name (in-list io-print-provider-owned-export-symbols)])
      (symbolic-name=? external name)))
  (cond
    [(and public-io-print-alias?
          (not (provider-owned-primitive-alias-scope-blocked? module-name))
          (memq 'io/print candidates))
     '(io/print)]
    [else candidates]))

;; prefer-list-member-candidates : symbol? (listof symbol?) -> (listof symbol?)
;;   Prefer the list-layer `member` only when export scanning found it.
(define (prefer-list-member-candidates external candidates)
  (cond
    [(and (symbolic-name=? external 'member)
          (memq 'racket/private/list candidates))
     (filter (lambda (dependency)
               (eq? dependency 'racket/private/list))
             candidates)]
    [else candidates]))

;; public-provider-counterpart : symbol? -> (or/c symbol? #f)
;;   Map an implementation provider to the public facade that should win
;;   import-resolution ties when both export the same surface name.
(define (public-provider-counterpart dependency)
  (and (symbol? dependency)
       (or
        (match (regexp-match #rx"^racket/private/(.+)$"
                             (symbol->string dependency))
          [(list _ suffix)
           (string->symbol (string-append "racket/" suffix))]
          [_ #f])
        (match dependency
          ['racket-src/racket/format-web 'racket/format]
          ['racket-src/io/print/main-web 'io/print]
          [_ #f]))))

;; prefer-public-provider-candidates : (listof symbol?) -> (listof symbol?)
;;   Prefer public facades over their private or staged implementation
;;   providers when both survive export scanning.
(define (prefer-public-provider-candidates candidates)
  (define public-candidates
    (filter (lambda (dependency)
              (for/or ([other (in-list candidates)])
                (eq? dependency
                     (public-provider-counterpart other))))
            candidates))
  (if (null? public-candidates) candidates public-candidates))

;; prefer-exact-export-candidates : dependency-exports symbol? (listof symbol?) -> (listof symbol?)
;;   Prefer providers that export the exact generated import name over providers
;;   that only match after generated-suffix normalization.
(define (prefer-exact-export-candidates dependency-exports external candidates)
  (define exact-candidates
    (filter
     (lambda (dependency)
       (define exports (dependency-exports-ref 'prefer-exact-export-candidates
                                               dependency-exports
                                               dependency))
       (memq external exports))
     candidates))
  (define base-name
    (string->symbol (generated-base-name external)))
  (define base-exact-candidates
    (filter
     (lambda (dependency)
       (define exports (dependency-exports-ref 'prefer-exact-export-candidates
                                               dependency-exports
                                               dependency))
       (memq base-name exports))
     candidates))
  (cond
    [(and (generated-name? external)
          (pair? exact-candidates)
          (pair? base-exact-candidates)
          (for/or ([dependency (in-list base-exact-candidates)])
            (webracket-owned-provider? dependency))
          (andmap racket-private-provider? exact-candidates))
     base-exact-candidates]
    [(null? exact-candidates) candidates]
    [else exact-candidates]))

;; provider-candidate-reasons : (listof symbol?) -> (listof list?)
;;   Explain why export-scan candidates survived far enough to be ambiguous.
(define (provider-candidate-reasons candidates)
  (for/list ([dependency (in-list candidates)])
    (list dependency (provider-candidate-reason dependency))))

;; webify-module-linklet-input-dependency-exports : webify-module-linklet-input? -> dependency-export-row
;;   Derive a resolver export row from the provider linklet's compiled exports.
(define (webify-module-linklet-input-dependency-exports input)
  (define who 'webify-module-linklet-input-dependency-exports)
  (unless (webify-module-linklet-input? input)
    (raise-argument-error who "webify-module-linklet-input?" input))
  (list (webify-module-linklet-input-name input)
        (linklet-exports who (webify-module-linklet-input-linklet input))))

;; webify-module-linklet-inputs-dependency-exports : (listof webify-module-linklet-input?) -> dependency-exports
;;   Derive a resolver export table from provider linklet inputs.
(define (webify-module-linklet-inputs-dependency-exports inputs)
  (define who 'webify-module-linklet-inputs-dependency-exports)
  (unless (list? inputs)
    (raise-argument-error who "(listof webify-module-linklet-input?)" inputs))
  (define dependency-exports
    (for/list ([input (in-list inputs)])
      (unless (webify-module-linklet-input? input)
        (raise-argument-error who "webify-module-linklet-input?" input))
      (webify-module-linklet-input-dependency-exports input)))
  (validate-dependency-exports! who dependency-exports)
  dependency-exports)

;; reflection-hints->map : symbol? (or/c symbol? #f) list? -> hash?
;;   Build a local-import map from reflection hints relevant to one module.
(define (reflection-hints->map who module-name hints)
  (validate-reflection-hints! who hints)
  (define rows
    (filter
     values
     (for/list ([hint (in-list hints)])
       (match hint
         [(list (? symbol? local) (? symbol? dependency) (? symbol? export))
          (list local dependency export)]
         [(list (? symbol? importer) (? symbol? local) (? symbol? dependency) (? symbol? export))
          (and (or (not module-name) (eq? importer module-name))
               (list local dependency export))]))))
  (define duplicate (check-duplicates (map car rows)))
  (when duplicate
    (raise-arguments-error who
                           "duplicate reflection hint entries are not supported"
                           "import" duplicate))
  (for/hasheq ([row (in-list rows)])
    (match-define (list local dependency export) row)
    (values local (list dependency export))))

;; validate-provider-export! : symbol? dependency-exports symbol? symbol? -> symbol?
;;   Validate a selected provider/export pair and return the canonical export symbol.
(define (validate-provider-export! who dependency-exports dependency provider-export)
  (cond
    [(primitive-module-name? dependency)
     (define base-export (kernel-primitive-base-name provider-export))
     (cond
       [(webify-primitive-name? base-export) base-export]
       [else
        (raise-arguments-error who
                               "unsupported kernel-origin binding"
                               "dependency" dependency
                               "export" provider-export)])]
    [else
     (define exports (dependency-exports-ref who dependency-exports dependency))
     (unless (for/or ([export (in-list exports)])
               (symbolic-name=? export provider-export))
       (raise-arguments-error who
                              "dependency does not export hinted name"
                              "dependency" dependency
                              "export" provider-export))
     (dependency-export-ref who dependency exports provider-export)]))

;; resolve-import-source : symbol? dependency-exports hash? hash? hash? symbol? symbol? (or/c symbol? #f) -> values
;;   Resolve one import binding to its provider module and provider export symbol.
(define (resolve-import-source who dependency-exports dependencies aliases hints local external module-name)
  (define alias (hash-ref aliases local #f))
  (define explicit (hash-ref dependencies local #f))
  (define hint (hash-ref hints local #f))
  (cond
    [alias
     (match-define (list dependency provider-export) alias)
     (when (and explicit (not (eq? explicit dependency)))
       (raise-arguments-error who
                              "import alias conflicts with explicit dependency"
                              "import" local
                              "dependency" explicit
                              "alias dependency" dependency))
     (define exports (dependency-exports-ref who dependency-exports dependency))
     (unless (for/or ([export (in-list exports)])
               (symbolic-name=? export provider-export))
       (raise-arguments-error who
                              "dependency does not export aliased name"
                              "dependency" dependency
                              "export" provider-export))
     (list dependency
           (validate-provider-export! who
                                      dependency-exports
                                      dependency
                                      (dependency-export-ref who dependency exports provider-export))
           'resolved-by-import-alias)]
    [explicit
     (define exports (dependency-exports-ref who dependency-exports explicit))
     (define explicit-alias-hint
       (compiled-primitive-alias-hint module-name local external))
     (define provider-export
       (match explicit-alias-hint
         [(list dependency hinted-export)
          #:when (eq? dependency explicit)
          hinted-export]
         [_ external]))
     (cond
       [(and (not (for/or ([export (in-list exports)])
                    (symbolic-name=? export provider-export)))
             ;; Some Racket providers (such as racket/math) re-export a
             ;; runtime primitive without allocating a linklet export cell.
             ;; Preserve the source-level provider surface, but route that
             ;; imported binding to the supported runtime primitive.
             (module-phase0-exported? explicit provider-export)
             (webify-primitive-name? provider-export))
        (list '#%kernel
              (validate-provider-export! who
                                         dependency-exports
                                         '#%kernel
                                         provider-export)
              'resolved-by-explicit-reexported-runtime-primitive)]
       [else
        (list explicit
              (validate-provider-export! who
                                         dependency-exports
                                         explicit
                                         (dependency-export-ref who explicit exports provider-export))
              'resolved-by-explicit-dependency)])]
    [hint
     (match-define (list dependency provider-export) hint)
     (list dependency
           (validate-provider-export! who dependency-exports dependency provider-export)
           'resolved-by-reflection-hint)]
    [else
     (define candidates
       (dependency-export-candidates dependency-exports external))
     (define ranked-candidates
       (prefer-list-member-candidates
        external
        (prefer-public-provider-candidates
         (prefer-exact-export-candidates
          dependency-exports
          external
          (prefer-staged-io-print-public-candidates
           module-name
           external
           (prefer-staged-read-public-candidates
            module-name
            external
            (prefer-provider-candidates candidates external)))))))
     (cond
       [(null? candidates)
        (raise-arguments-error who
                               "missing-after-reflection"
                               "module" module-name
                               "import" local
                               "export" external)]
       [(null? (cdr ranked-candidates))
       (define dependency (car ranked-candidates))
       (define exports (dependency-exports-ref who dependency-exports dependency))
       (list dependency
              (validate-provider-export! who
                                         dependency-exports
                                         dependency
                                         (dependency-export-ref
                                          who
                                          dependency
                                          exports
                                          external))
              (if (equal? ranked-candidates candidates)
                  'resolved-by-export-scan
                  'resolved-by-export-scan/provider-precedence))]
       [else
        (raise-arguments-error who
                               "ambiguous-after-reflection"
                               "module" module-name
                               "import" local
                               "export" external
                               "candidates" ranked-candidates
                               "candidate reasons"
                               (provider-candidate-reasons candidates))])]))

;; compiled-primitive-alias-hints : (or/c symbol? #f) symbol? symbol? -> (or/c list? #f)
;;   Recover a few primitive-module aliases that compiled metadata currently
;;   exposes only as internalized imports. For example, setup/private/dirs
;;   imports #%utils:get-installation-name as 1/get-installation-name and then
;;   defines its own public get-installation-name, so export scanning alone
;;   incorrectly resolves the import to the module's own export cell.
(define (compiled-primitive-alias-hint module-name local external)
  (cond
    [(and (eq? module-name 'setup/private/dirs)
          (eq? local '1/get-installation-name)
          (eq? external 'get-installation-name))
     '(#%utils get-installation-name)]
    [(and (eq? module-name 'racket-src/expander/read-provider-root.rkt)
          (eq? local '1/string->number)
          (eq? external 'string->number))
     '(racket-src/expander/host/string-to-number string->number)]
    [(and (eq? module-name 'webracket/private/pre-base-apply)
          (eq? local '1/new-apply-proc)
          (eq? external 'new-apply-proc))
     '(#%kernel apply)]
    [(and (memq module-name '(racket-src/expander/read/vector
                              racket-src/expander/read/vector.rkt))
          (eq? local 'not-an-fX.1)
          (eq? external 'not-an-fX.1))
     '(racket/fixnum not-an-fX.1)]
    [(and (memq module-name '(racket-src/expander/read/vector
                              racket-src/expander/read/vector.rkt))
          (eq? local '1/not-an-fX.1)
          (eq? external 'not-an-fX.1))
     '(racket/flonum not-an-fX.1)]
    [(and (eq? local '-random)
          (eq? external '-random))
     '(#%kernel random)]
    [(and (eq? local '1/syntax-source)
          (eq? external 'syntax-source))
     '(webracket/primitives syntax-source)]
    [(and (memq module-name '(webracket/contract
                              racket/contract/base
                              racket/contract/combinator
                              racket/contract/region
                              racket/contract/private/base
                              racket/contract/private/blame
                              racket/contract/private/guts
                              racket/contract/private/object
                              racket/contract/private/object-c-wrapper))
          (eq? local '1/datum->syntax)
          (eq? external 'datum->syntax))
     '(webracket/primitives datum->syntax)]
    [(and (memq module-name '(webracket/contract
                              racket/contract/base
                              racket/contract/combinator
                              racket/contract/region
                              racket/contract/private/base
                              racket/contract/private/blame
                              racket/contract/private/guts
                              racket/contract/private/object
                              racket/contract/private/object-c-wrapper))
          (memq local '(1/syntax-span
                         1/syntax-position
                         1/syntax-column
                         1/syntax-line
                         1/syntax-source
                         1/syntax?))
          (memq external '(syntax-span
                           syntax-position
                           syntax-column
                           syntax-line
                           syntax-source
                           syntax?)))
     (list 'webracket/primitives external)]
    [(and (memq module-name '(racket/private/for webracket/private/for))
          (eq? local 'directory-list.1))
     '(#%kernel webracket-directory-list-build-first)]
    [(and (memq module-name '(racket/private/for
                              racket/private/list
                              racket/private/class-struct
                              webracket/list))
          (eq? local '1/reverse)
          (eq? external 'reverse))
     (list (if (module-path-available? 'racket/private/pico)
               'racket/private/pico
               'racket/private/reverse)
           'reverse)]
    [(and (memq module-name '(racket/list
                              racket/private/list))
          (symbolic-name=? external 'sort))
     '(webracket/private/sort-public sort)]
    [(and (memq module-name '(racket-src/io/format/main-web
                              racket-src/io/format/main-web.rkt))
          (symbolic-name=? external 'do-printf))
     '(racket-src/io/format/printf-web do-printf)]
    [(and (memq module-name '(racket-src/io/format/main-web
                              racket-src/io/format/main-web.rkt))
          (symbolic-name=? external 'print-boolean-long-form))
     '(racket-src/io/format/printf-adapter print-boolean-long-form)]
    [(symbolic-name=? external 'real->decimal-string)
     '(webracket/private/decimal-string real->decimal-string)]
    [(symbolic-name=? external 'format)
     '(racket-src/io/format/main-web format)]
    [(and (eq? module-name 'racket/private/kw)
          (memq external '(make-keyword-procedure
                           keyword-apply
                           new-keyword-apply
                           make-optional-keyword-procedure
                           make-optional-keyword-method
                           make-required-keyword-method/arity-error
                           keyword-procedure-extract
                           procedure-reduce-keyword-arity
                           procedure-reduce-keyword-arity-mask
                           procedure-keywords
                           struct:keyword-procedure
                           struct:keyword-procedure/arity-error
                           struct:keyword-method
                           struct:keyword-method/arity-error)))
     (list 'webracket/private/kw external)]
    [(and (memq module-name '(racket/list
                              racket/private/list
                              webracket/list))
          (symbolic-name=? external 'check-list))
     '(#%kernel check-list)]
    [(and (eq? module-name 'racket/private/class-internal)
          (symbolic-name=? external 'coerce-contract))
     '(racket/contract/private/guts coerce-contract)]
    [(and (eq? module-name 'racket/private/class-internal)
          (symbolic-name=? external 'apply-contract))
     '(racket/contract/private/guts apply-contract)]
    [(and (eq? module-name 'racket/private/class-internal)
          (symbolic-name=? external 'blame-swap))
     '(racket/contract/private/blame blame-swap)]
    [(and (eq? module-name 'racket/private/class-internal)
          (symbolic-name=? external 'raise-blame-error))
     '(racket/contract/private/blame raise-blame-error)]
    [(and (eq? module-name 'racket/private/class-internal)
          (symbolic-name=? external 'contract?))
     '(racket/contract/private/guts contract?)]
    [(and (eq? module-name 'racket/private/class-internal)
          (symbolic-name=? external 'contract-continuation-mark-key))
     '(racket/contract/private/guts contract-continuation-mark-key)]
    [(and (eq? module-name 'racket/private/class-internal)
          (class-internal-class-struct-import? external))
     ;; Racket 9.3 moved these bindings from the removed
     ;; `racket/private/class-wrapped` module into `class-struct`.
     (list 'racket/private/class-struct external)]
    [(and (eq? module-name 'racket/private/class-internal)
          (module-phase0-exported? 'racket/private/class-wrapped external))
     (list 'racket/private/class-wrapped external)]
    [(and (eq? module-name 'racket/private/class-internal)
          (for/or ([name (in-list '(wrapped-class-info-blame
                                    wrapped-class-info-neg-extra-arg-vec
                                    wrapped-class-info-neg-acceptors-ht
                                    wrapped-class-info-pos-field-projs
                                    wrapped-class-info-neg-field-projs
                                    wrapped-class-info-init-proj-pairs
                                    wrapped-object
                                    wrapped-object?
                                    wrapped-object-object
                                    wrapped-object-neg-extra-arg-vec
                                    wrapped-object-pos-field-projs
                                    wrapped-object-neg-field-projs
                                    wrapped-object-neg-party
                                    unwrap-object
                                    impersonator-prop:wrapped-class-info
                                    impersonator-prop:has-wrapped-class-info?
                                    impersonator-prop:get-wrapped-class-info
                                    impersonator-prop:wrapped-class-neg-party
                                    impersonator-prop:has-wrapped-class-neg-party?
                                    impersonator-prop:get-wrapped-class-neg-party))])
             (symbolic-name=? external name)))
     (list 'racket/private/class-wrapped external)]
    [(and (memq module-name '(contract racket/private/unit/contract))
          (symbolic-name=? external 'contract-continuation-mark-key))
     '(racket/contract/private/guts contract-continuation-mark-key)]
    [(and (memq module-name '(contract racket/private/unit/contract))
          (symbolic-name=? external 'member))
     ;; `racket/private/unit/contract` gets ordinary list membership from its
     ;; `racket/base` import. Prefer the list layer when full-language provider
     ;; composition also exposes the lower pico helper.
     '(racket/private/list member)]
    [(and (memq module-name '(runtime racket/private/unit/runtime))
          (symbolic-name=? external 'contract-continuation-mark-key))
     '(racket/contract/private/guts contract-continuation-mark-key)]
    [(and (memq module-name '(runtime racket/private/unit/runtime))
          (symbolic-name=? external '-make-blame))
     '(racket/contract/private/blame make-blame)]
    [(and (eq? module-name 'racket/private/unit/contract-syntax)
          (symbolic-name=? external 'unwind-to))
     '(syntax/parse/private/residual/state unwind-to)]
    [(and (eq? module-name 'racket/private/unit/contract-syntax)
          (memq external '(failure*
                           ps-add-cdr
                           ps-add-car
                           ps-add-stx
                           ps-add-post
                           ps-add
                           es-add-thing
                           es-add-literal
                           es-add-atom
                           es-add-proper-pair)))
     (list 'syntax/parse/private/residual/progress external)]
    [(and (eq? module-name 'racket/private/unit/contract-syntax)
          (memq external '(t-resyntax t-subst)))
     (list 'racket/private/template external)]
    [(and (eq? module-name 'racket/private/unit/contract-syntax)
          (symbolic-name=? external 'predicate-ellipsis-parser))
     '(syntax/parse/private/residual/parse-util predicate-ellipsis-parser)]
    [(and (eq? module-name 'syntax/parse/private/residual/parse-util)
          (symbolic-name=? external '2d-stx?.1))
     '(syntax/parse/private/residual/3d-stx 2d-stx?.1)]
    [(and (eq? module-name 'syntax/parse/private/residual/parse-util)
          (symbolic-name=? external 'resolve-paths))
     '(racket/runtime-path resolve-paths)]
    [(and (eq? module-name 'racket/runtime-path)
          (symbolic-name=? external 'table))
     '(racket/private/runtime-path-table table)]
    [(and (eq? module-name 'racket/runtime-path)
          (memq external '(decode-link-path encode-link-path encoded-link-path?)))
     (list 'racket/private/link-path external)]
    [(and (eq? module-name 'racket/runtime-path)
          (memq external '(so-find so-spec?)))
     (list 'webracket/private/so-search external)]
    [(and (eq? module-name 'racket/runtime-path)
          (memq external '(share-find share-spec?)))
     (list 'webracket/private/share-search external)]
    [(and (eq? module-name 'racket/runtime-path)
          (symbolic-name=? external 'new-apply-proc))
     '(webracket/private/pre-base-apply new-apply-proc)]
    [(and (eq? module-name 'syntax/parse/private/residual/parse-util)
          (symbolic-name=? external 'make-lazy-function))
     '(racket/lazy-require make-lazy-function)]
    [(and (eq? module-name 'racket/lazy-require)
          (symbolic-name=? external 'delay/sync))
     '(racket/promise delay/sync/proc)]
    [(and (eq? module-name 'racket/lazy-require)
          (symbolic-name=? external 'register-external-module))
     '(compiler/cm-accomplice register-external-module)]
    [(and (eq? module-name 'racket/private/unit/contract-syntax)
          (symbolic-name=? external 'expr-stx?))
     '(syntax/parse/private/lib expr-stx?)]
    [(and (memq module-name '(exptime/import-export
                               racket/private/unit/exptime/import-export))
          (symbolic-name=? external 's-any))
     '(syntax/parse/private/residual-interp s-any)]
    [(and (memq module-name '(exptime/import-export
                               racket/private/unit/exptime/import-export))
          (symbolic-name=? external 'check-list))
     ;; Source imports this through `racket/list`; prefer the private list
     ;; implementation when the compiled bundle exposes both names.
     '(racket/private/list check-list)]
    [(and (memq module-name '(exptime/import-export
                               racket/private/unit/exptime/import-export))
          (symbolic-name=? external 'empty?))
     ;; `racket/list` re-exports `empty?` from its predicate helper.
     '(racket/private/list-predicates empty?)]
    [(and (memq module-name '(exptime/import-export
                               racket/private/unit/exptime/import-export))
          (symbolic-name=? external 'append-map))
     ;; Source imports this public helper through `racket/list`.
     '(racket/list append-map)]
    [(and (eq? module-name 'syntax/parse/private/residual-interp)
          (symbol? external)
          (regexp-match? #rx"^sim:" (symbol->string external)))
     (list 'syntax/parse/private/rep-patterns/simple external)]
    [(and (eq? module-name 'racket/private/unit/exptime/syntax)
          (symbolic-name=? external 'interp-match))
     '(racket/private/stxcase interp-match)]
    [(and (memq module-name '(exptime/util
                               racket/private/unit/exptime/util))
          (symbolic-name=? external 'make-hygienic-metafunction))
     '(syntax/parse/experimental/template make-hygienic-metafunction)]
    [(and (memq module-name '(exptime/util
                               racket/private/unit/exptime/util))
          (symbolic-name=? external 'check-list))
     ;; This helper uses `for/list`; prefer the check emitted through the for
     ;; implementation when list internals are also visible.
     '(racket/private/for check-list)]
    [(and (eq? module-name 'racket-src/expander/read/string)
          (symbolic-name=? external 'check-list))
     ;; The staged reader string helper only needs the primitive list check.
     '(#%kernel check-list)]
    [(and (eq? module-name 'webracket/stream)
          (symbolic-name=? external 'check-list))
     ;; Staged stream checks are generated by `for`, not by an explicit
     ;; `racket/list` import.
     '(racket/private/for check-list)]
    [(and (eq? module-name 'racket/private/class-internal)
          (symbolic-name=? external 'variable-reference->module-source/submod))
     '(syntax/location variable-reference->module-source/submod)]
    [(and (eq? module-name 'racket/private/class-internal)
          (symbolic-name=? external 'current-contract-region))
     '(racket/contract/region current-contract-region)]
    [(and (eq? module-name 'racket/private/class-internal)
          (symbolic-name=? external 'contract))
     '(racket/contract/private/guts contract)]
    [(symbolic-name=? external 'struct:keyword-method/arity-error)
     '(webracket/private/kw struct:keyword-method/arity-error)]
    [(symbolic-name=? external 'subsets?)
     '(webracket/private/kw subsets?)]
    [(and (memq module-name '(racket/private/kw webracket/private/kw))
          (symbolic-name=? external 'new:procedure-reduce-arity))
     '(#%kernel procedure-reduce-arity)]
    [(and (memq module-name '(racket/private/kw webracket/private/kw))
          (symbolic-name=? external 'new:procedure-reduce-arity-mask))
     '(#%kernel procedure-reduce-arity-mask)]
    [(and (memq module-name '(racket/private/kw webracket/private/kw))
          (symbolic-name=? external 'new:procedure->method))
     '(#%kernel procedure->method)]
    [(and (memq module-name '(racket/private/kw webracket/private/kw))
          (symbolic-name=? external 'new:procedure-rename))
     '(#%kernel procedure-rename)]
    [(and (memq module-name '(racket/private/kw webracket/private/kw))
          (symbolic-name=? external 'new:procedure-realm))
     '(#%kernel procedure-realm)]
    [(and (memq module-name '(racket/private/kw webracket/private/kw))
          (symbolic-name=? external 'new-prop:procedure))
     '(#%kernel prop:procedure)]
    [(and (eq? module-name 'racket-src/expander/read-provider-root.rkt)
          (symbolic-name=? external 'string->number))
     '(racket-src/expander/host/string-to-number string->number)]
    [(symbolic-name=? external 'find-method/who)
     '(racket/private/class-struct find-method/who)]
    [(symbolic-name=? external 'do-make-object)
     '(racket/private/class-internal do-make-object)]
    [(symbolic-name=? external 'extract-arg)
     '(racket/private/class-internal extract-arg)]
    [(symbolic-name=? external 'compose-class)
     '(racket/private/class-internal compose-class)]
    [(symbolic-name=? external 'is-a?)
     '(racket/private/class-internal is-a?)]
    [(symbolic-name=? external 'object?)
     '(racket/private/class-internal object?)]
    [(symbolic-name=? external 'object%)
     '(racket/private/class-internal object%)]
    [(symbolic-name=? external 'check-inspector)
     '(racket/private/define-struct check-inspector)]
    [(symbolic-name=? external 'check-struct-type)
     '(racket/private/define-struct check-struct-type)]
    [(symbolic-name=? external 'make-serialize-info)
     '(racket/private/serialize-structs make-serialize-info)]
    [(symbolic-name=? external 'make-deserialize-info)
     '(racket/private/serialize-structs make-deserialize-info)]
    [(symbolic-name=? external 'remove-duplicates)
     '(webracket/list remove-duplicates)]
    ;; `racket/private/map` uses its private linklet names as actual exports
    ;; (`map2`, etc.) and provides the public bindings through renames. The
    ;; module bridge sees an explicit dependency on that provider but only the
    ;; public external name, so preserve the compiled provider's rename here.
    [(symbolic-name=? external 'map)
     '(racket/private/map map2)]
    [(symbolic-name=? external 'for-each)
     '(racket/private/map for-each2)]
    [(symbolic-name=? external 'andmap)
     '(racket/private/map andmap2)]
    [(symbolic-name=? external 'ormap)
     '(racket/private/map ormap2)]
    [(symbolic-name=? external 'andmap2)
     '(racket/private/map andmap2)]
    [(symbolic-name=? external 'ormap2)
     '(racket/private/map ormap2)]
    [(symbolic-name=? external 'normalise-inputs)
     '(racket/private/for normalise-inputs)]
    [(symbolic-name=? external 'unsafe-undefined)
     '(racket/unsafe/undefined unsafe-undefined)]
    [(symbolic-name=? external 'check-not-unsafe-undefined)
     '(racket/unsafe/undefined check-not-unsafe-undefined)]
    [(symbolic-name=? external 'check-not-unsafe-undefined/assign)
     '(racket/unsafe/undefined check-not-unsafe-undefined/assign)]
    [(symbolic-name=? external 'prop:chaperone-unsafe-undefined)
     '(racket/unsafe/undefined prop:chaperone-unsafe-undefined)]
    [(symbolic-name=? external 'chaperone-struct-unsafe-undefined)
     '(racket/unsafe/undefined chaperone-struct-unsafe-undefined)]
    [else #f]))

;; webify-resolve-module-linklet-imports/outcomes : linklet dependency-exports keyword-args ... -> values
;;   Derive flat registry import triples and classify each resolution decision.
(define (webify-resolve-module-linklet-imports/outcomes linklet
                                                        dependency-exports
                                                        #:module-name [module-name #f]
                                                        #:dependencies [dependencies '()]
                                                        #:import-aliases [import-aliases '()]
                                                        #:reflection-hints [reflection-hints '()])
  (define who 'webify-resolve-module-linklet-imports)
  (define bindings (linklet-import-bindings who linklet))
  (define dependency-exports*
    (dependency-exports/add-synthetic-rows
     (dependency-exports/filter-provider-owned-rows dependency-exports module-name)
     module-name))
  (validate-dependency-exports! who dependency-exports*)
  (validate-import-dependencies! who dependencies)
  (validate-import-aliases! who import-aliases)
  (unless (or (not module-name) (symbol? module-name))
    (raise-argument-error who "(or/c symbol? #f)" module-name))
  (define dependency-export-index
    (make-dependency-export-index dependency-exports*))
  (define imports (map car bindings))
  (define import-table
    (for/hasheq ([import (in-list imports)])
      (values import #t)))
  (define canonical-import-cache (make-hasheq))
  (define (canonical-import import kind)
    (cond
      [(hash-ref import-table import #f) import]
      [else
       (or (hash-ref canonical-import-cache import #f)
           (match (filter (lambda (candidate)
                            (symbolic-name=? candidate import))
                          imports)
             [(list candidate)
              (hash-set! canonical-import-cache import candidate)
              candidate]
             [matches
              (raise-arguments-error who
                                     (format "~a references a name not imported by the linklet" kind)
                                     "import" import
                                     "matches" matches
                                     "linklet imports" imports)]))]))
  (for ([dependency (in-list dependencies)])
    (match-define (list import _module) dependency)
    (canonical-import import "import dependency"))
  (for ([alias (in-list import-aliases)])
    (match-define (list import _module _export) alias)
    (canonical-import import "import alias"))
  (define dependency-map
    (for/hasheq ([dependency (in-list dependencies)])
      (match-define (list import module) dependency)
      (values (canonical-import import "import dependency") module)))
  (define alias-map
    (for/fold ([aliases (hasheq)]) ([alias (in-list import-aliases)])
      (match-define (list import module export) alias)
      (define canonical (canonical-import import "import alias"))
      (if (hash-has-key? aliases canonical)
          aliases
          (hash-set aliases canonical (list module export)))))
  (define hint-map
    (for/fold ([hints (hasheq)])
              ([(import value) (in-hash (reflection-hints->map who module-name reflection-hints))])
      (define canonical (canonical-import import "reflection hint"))
      (if (hash-has-key? hints canonical)
          hints
          (hash-set hints canonical value))))
  (define resolved
    (for/list ([binding (in-list bindings)])
      (match-define (list local external) binding)
      (define primitive-alias-override?
        (and (memq module-name '(racket/list
                                 racket/private/list
                                 webracket/list))
             (symbolic-name=? external 'check-list)))
      (define candidate-primitive-alias-hint
        (and (not (or (and (not primitive-alias-override?)
                            (hash-has-key? alias-map local))
                      (hash-has-key? hint-map local)))
             (compiled-primitive-alias-hint module-name local external)))
      (define primitive-alias-hint
        (match candidate-primitive-alias-hint
          [(list dependency _provider-export)
           #:when (dependency-exports-has-dependency? dependency-export-index dependency)
           candidate-primitive-alias-hint]
          [_ #f]))
      (match-define (list dependency provider-export status)
        (if primitive-alias-hint
            (match primitive-alias-hint
              [(list dependency provider-export)
               (list dependency
                     (validate-provider-export! who
                                                dependency-export-index
                                                dependency
                                                provider-export)
                     'resolved-by-compiled-primitive-alias)])
            (resolve-import-source who dependency-export-index dependency-map alias-map hint-map local external module-name)))
      (list (list local dependency provider-export)
            (webify-module-resolution-outcome local dependency provider-export status))))
  (values (map car resolved)
          (map cadr resolved)))

;; webify-resolve-module-linklet-imports : linklet dependency-exports [#:dependencies dependencies] -> imports
;;   Derive flat registry import triples from linklet imports and dependency exports.
(define (webify-resolve-module-linklet-imports linklet
                                               dependency-exports
                                               #:module-name [module-name #f]
                                               #:dependencies [dependencies '()]
                                               #:import-aliases [import-aliases '()]
                                               #:reflection-hints [reflection-hints '()])
  (define-values (imports outcomes)
    (webify-resolve-module-linklet-imports/outcomes
     linklet
     dependency-exports
     #:module-name module-name
     #:dependencies dependencies
     #:import-aliases import-aliases
     #:reflection-hints reflection-hints))
  imports)

;; webify-module-linklet-input/resolve-imports : symbol? linklet dependency-exports [#:dependencies dependencies] -> input
;;   Build one module-linklet input while deriving its flat import triples.
(define (webify-module-linklet-input/resolve-imports name
                                                     linklet
                                                     dependency-exports
                                                     #:dependencies [dependencies '()]
                                                     #:import-aliases [import-aliases '()]
                                                     #:reflection-hints [reflection-hints '()])
  (webify-module-linklet-input
   name
   linklet
   (webify-resolve-module-linklet-imports
    linklet
    dependency-exports
    #:module-name name
    #:dependencies dependencies
    #:import-aliases import-aliases
    #:reflection-hints reflection-hints)))

;; webify-register-module-linklet-input! : webify-module-registry? webify-module-linklet-input? -> void?
;;   Register one extracted module-linklet input in a module registry.
(define (webify-register-module-linklet-input! registry input)
  (define who 'webify-register-module-linklet-input!)
  (unless (webify-module-linklet-input? input)
    (raise-argument-error who "webify-module-linklet-input?" input))
  (webify-module-registry-add!
   registry
   (webify-module-linklet-input-name input)
   (webify-module-linklet-input-linklet input)
   #:imports (webify-module-linklet-input-imports input)))

;; webify-module-linklet-inputs->registry : (listof webify-module-linklet-input?) -> webify-module-registry?
;;   Build a registry from already-extracted module-linklet inputs.
(define (webify-module-linklet-inputs->registry inputs)
  (define who 'webify-module-linklet-inputs->registry)
  (unless (list? inputs)
    (raise-argument-error who "(listof webify-module-linklet-input?)" inputs))
  (define registry (make-webify-module-registry))
  (for ([input (in-list inputs)])
    (webify-register-module-linklet-input! registry input))
  registry)

;; webify-module-linklet-manifest->registry : webify-module-linklet-manifest? -> webify-module-registry?
;;   Build a registry from a parsed module-linklet manifest.
(define (webify-module-linklet-manifest->registry manifest)
  (unless (webify-module-linklet-manifest? manifest)
    (raise-argument-error 'webify-module-linklet-manifest->registry
                          "webify-module-linklet-manifest?"
                          manifest))
  (webify-module-linklet-inputs->registry
   (webify-module-linklet-manifest-inputs manifest)))
