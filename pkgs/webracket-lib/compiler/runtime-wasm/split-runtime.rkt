#lang racket

(provide (struct-out split-runtime-modules)
         (struct-out split-runtime-boundary-metadata)
         split-runtime-modules-core-module
         core-runtime-helper-import-form
         core-runtime-helper-import-forms
         classified-runtime-import/export-forms
         read-split-runtime-boundary-metadata/cache
         split-runtime-reference-compatible?
         split-runtime-reference-compatible+index
         runtime-tag-export-form
         runtime-tag-import-form
         runtime-function-export-form
         runtime-function-import-form
         runtime-global-export-form
         runtime-global-import-form
         strip-split-runtime-program-entry-init
         split-classified-runtime-forms
         split-classified-runtime-module
         split-core-runtime-module)

(require (only-in "../private/parameters.rkt"
                  current-runtime-cache-dir
                  current-runtime-metric-rows)
         (only-in "../private/wasm-data.rkt"
                  wasm-data)
         "core-helper.rkt"
         (only-in "constants.rkt"
                  constant-data-name?
                  constant-global-name?)
         "core-runtime.rkt"
         "module-forms.rkt"
         (only-in "runtime-analysis.rkt"
                  split-module-forms)
         (only-in "split-runtime-cache.rkt"
                  cacheable-datum
                  cached-datum->value
                  split-runtime-boundary-cache-version
                  [split-runtime-boundary-cache-path
                   split-runtime-boundary-cache-path/cache-root])
         (only-in "runtime-metrics.rkt"
                  runtime-metric-add!
                  time-runtime-step)
         "shared-types.rkt"
         racket/fasl
         racket/file)

;; A split runtime result contains the standalone runtime module and the
;; rewritten program module that imports selected helper functions from it.
;; core-cache-key is #f unless the producer can prove that the runtime side is
;; stable enough to cache without hashing the generated module tree.
;; ffi-cache-key is #f unless the optional FFI sidecar module has a stable
;; cache key.
;; When a cache key is present, `core` may be a promise so warm cache hits can
;; avoid constructing the runtime module tree.
(struct split-runtime-modules
  (core program imported-names import-forms core-cache-key ffi-module
        ffi-cache-key)
  #:transparent)

;; default-core-module-sentinel : symbol?
;;   Marks an omitted core module so stable-cache paths can keep it lazy.
(define default-core-module-sentinel
  (gensym 'default-core-module))

;; split-runtime-modules-core-module : split-runtime-modules? -> datum?
;;   Return the runtime core module, forcing a lazy core when needed.
(define (split-runtime-modules-core-module split)
  (define core (split-runtime-modules-core split))
  (if (promise? core) (force core) core))

;; split-runtime-entry-init-form? : any/c -> boolean?
;;   Check whether a top-level entry instruction reinitializes runtime-owned state.
(define (split-runtime-entry-init-form? form)
  (match form
    [`(call $initialize-split-runtime-before-constants!) #t]
    [`(call $initialize-split-runtime-constants!) #t]
    [`(call $initialize-split-runtime-after-constants!) #t]
    [`(call $initialize-unsafe-ffi-primitive-table) #t]
    [`(call $initialize-split-runtime-after-ffi!) #t]
    [`(drop (call $reset-current-input-port!)) #t]
    [`(drop (call $reset-current-output-port!)) #t]
    [`(drop (call $reset-current-error-port!)) #t]
    [_ #f]))

;; strip-split-runtime-entry-init-form : any/c -> any/c
;;   Remove loader-owned calls from the bootstrap helper or an older inline entry.
(define (strip-split-runtime-entry-init-form form)
  (match form
    [`(func ,(and name (or '$entry '$entry-bootstrap)) ,parts ...)
     `(func ,name ,@(filter-not split-runtime-entry-init-form? parts))]
    [_ form]))

;; strip-split-runtime-program-entry-init : datum? -> datum?
;;   Return a program module whose entry assumes runtime initialization is loader-owned.
(define (strip-split-runtime-program-entry-init module)
  (match module
    [`(module ,forms ...)
     `(module ,@(for/list ([form (in-list forms)])
                  (strip-split-runtime-entry-init-form form)))]
    [_ (raise-argument-error
        'strip-split-runtime-program-entry-init
        "WAT module datum"
        module)]))

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

;; function-definition-form? : any/c -> boolean?
;;   Check whether form is a named, non-imported function definition.
(define (function-definition-form? form)
  (and (module-func-name form)
       (not (function-import-form? form))))

;; global-definition-form? : any/c -> boolean?
;;   Check whether form is a named, non-imported global definition.
(define (global-definition-form? form)
  (and (module-global-name form)
       (not (global-import-form? form))))

;; module-tag-name : any/c -> (or/c symbol? #f)
;;   Return the name of a WAT tag form, if the form has one.
(define (module-tag-name form)
  (and (pair? form)
       (eq? (car form) 'tag)
       (pair? (cdr form))
       (symbol? (cadr form))
       (cadr form)))

;; tag-definition-form? : any/c -> boolean?
;;   Check whether form is a named, non-imported tag definition.
(define (tag-definition-form? form)
  (and (module-tag-name form)
       (not (tag-import-form? form))))

;; function-type-form? : any/c -> boolean?
;;   Check whether part is a named function type use.
(define (function-type-form? part)
  (match part
    [`(type ,(? symbol?)) #t]
    [_ #f]))

;; function-inline-signature-form? : any/c -> boolean?
;;   Check whether part belongs to an inline function signature.
(define (function-inline-signature-form? part)
  (match part
    [`(param . ,_) #t]
    [`(result . ,_) #t]
    [_ #f]))

;; function-import-signature-forms : datum? -> (listof datum?)
;;   Extract the type use or inline signature for an imported function.
(define (function-import-signature-forms function-form)
  (match function-form
    [`(func ,_ ,parts ...)
     (define type-forms
       (for/list ([part (in-list parts)]
                  #:when (function-type-form? part))
         part))
     (cond
       [(pair? type-forms) (list (car type-forms))]
       [else
        (for/list ([part (in-list parts)]
                   #:when (function-inline-signature-form? part))
          part)])]
    [_
     (error 'function-import-signature-forms
            "expected a named function form, got: ~s"
            function-form)]))

;; tag-signature-form? : any/c -> boolean?
;;   Check whether part belongs to a tag signature.
(define (tag-signature-form? part)
  (match part
    [`(param . ,_) #t]
    [_ #f]))

;; tag-import-signature-forms : datum? -> (listof datum?)
;;   Extract the parameter signature for an imported tag.
(define (tag-import-signature-forms tag-form)
  (match tag-form
    [`(tag ,_ ,parts ...)
     (for/list ([part (in-list parts)]
                #:when (tag-signature-form? part))
       part)]
    [_
     (error 'tag-import-signature-forms
            "expected a named tag form, got: ~s"
            tag-form)]))

;; core-runtime-helper-import-form :
;;   core-runtime-helper? [#:module string?] -> datum?
;;   Render an import declaration matching a registered core helper.
(define (core-runtime-helper-import-form helper #:module [module-name "core"])
  (define form (core-runtime-helper->form helper))
  (define name (core-runtime-helper-name helper))
  `(func ,name
         (import ,module-name ,(core-runtime-helper-export-name helper))
         ,@(function-import-signature-forms form)))

;; runtime-export-name : symbol? -> string?
;;   Derive a stable runtime export name from a Wasm identifier.
(define (runtime-export-name name)
  (symbol->string name))

;; runtime-tag-export-name : symbol? -> string?
;;   Derive a stable runtime export name for tags without colliding with functions.
(define (runtime-tag-export-name name)
  (format "tag:~a" (runtime-export-name name)))

;; runtime-global-export-name : symbol? -> string?
;;   Derive a stable runtime export name for globals without colliding with functions.
(define (runtime-global-export-name name)
  (format "global:~a" (runtime-export-name name)))

;; runtime-function-import-form : datum? [#:module string?] -> datum?
;;   Render an import declaration matching a runtime function definition.
(define (runtime-function-import-form function-form #:module [module-name "core"])
  (define name (module-func-name function-form))
  (unless name
    (error 'runtime-function-import-form
           "expected a named function form, got: ~s"
           function-form))
  `(func ,name
         (import ,module-name ,(runtime-export-name name))
         ,@(function-import-signature-forms function-form)))

;; runtime-function-export-form : symbol? -> datum?
;;   Render an export declaration for a runtime function.
(define (runtime-function-export-form name)
  `(export ,(runtime-export-name name) (func ,name)))

;; runtime-tag-import-form : datum? [#:module string?] -> datum?
;;   Render an import declaration matching a runtime tag definition.
(define (runtime-tag-import-form tag-form #:module [module-name "core"])
  (define name (module-tag-name tag-form))
  (unless name
    (error 'runtime-tag-import-form
           "expected a named tag form, got: ~s"
           tag-form))
  `(tag ,name
        (import ,module-name ,(runtime-tag-export-name name))
        ,@(tag-import-signature-forms tag-form)))

;; runtime-tag-export-form : symbol? -> datum?
;;   Render an export declaration for a runtime tag.
(define (runtime-tag-export-form name)
  `(export ,(runtime-tag-export-name name) (tag ,name)))

;; global-type-form : datum? -> datum?
;;   Extract the type declaration from a named global definition.
(define (global-type-form global-form)
  (match global-form
    [`(global ,_ ,type ,_ ...)
     type]
    [_
     (error 'global-type-form
            "expected a named global form, got: ~s"
            global-form)]))

;; runtime-global-import-form : datum? [#:module string?] -> datum?
;;   Render an import declaration matching a runtime global definition.
(define (runtime-global-import-form global-form #:module [module-name "core"])
  (define name (module-global-name global-form))
  (unless name
    (error 'runtime-global-import-form
           "expected a named global form, got: ~s"
           global-form))
  `(global ,name
           (import ,module-name ,(runtime-global-export-name name))
           ,(global-type-form global-form)))

;; runtime-global-export-form : symbol? -> datum?
;;   Render an export declaration for a runtime global.
(define (runtime-global-export-form name)
  `(export ,(runtime-global-export-name name) (global ,name)))

;; core-runtime-helper-import-forms :
;;   (listof core-runtime-helper?) [#:module string?] -> (listof datum?)
;;   Render import declarations for registered core helpers.
(define (core-runtime-helper-import-forms helpers #:module [module-name "core"])
  (for/list ([helper (in-list helpers)])
    (core-runtime-helper-import-form helper #:module module-name)))

;; import-insertion-prefix-form? : any/c -> boolean?
;;   Recognize forms that should stay before generated core imports.
(define (import-insertion-prefix-form? form)
  (and (pair? form)
       (or (memq (car form) '(type rec import tag))
           (function-import-form? form)
           (global-import-form? form)
           (tag-import-form? form))))

;; named-form-index : (listof datum?) (datum? -> (or/c symbol? #f)) -> hasheq?
;;   Build an index from Wasm identifier to top-level module form.
(define (named-form-index forms name-of)
  (for/hasheq ([form (in-list forms)]
               #:do [(define name (name-of form))]
               #:when name)
    (values name form)))

;; split-form-indexes :
;;   hasheq? hasheq? hasheq? hasheq? hasheq? hasheq? list? list?
;;   -> split-form-indexes?
;;   Precomputed indexes for repeated split-runtime module-form queries.
(struct split-form-indexes
  (function-defs
   global-defs
   tag-defs
   data-forms
   function-names
   data-names
   declare-elem-forms
   shared-prefix-forms)
  #:transparent)

;; build-split-form-indexes : (listof datum?) -> split-form-indexes?
;;   Build reusable indexes for split-runtime module body forms.
(define (build-split-form-indexes forms)
  (define function-defs (make-hasheq))
  (define global-defs (make-hasheq))
  (define tag-defs (make-hasheq))
  (define data-forms (make-hasheq))
  (define function-names (make-hasheq))
  (define data-names (make-hasheq))
  (define declare-elem-forms/reversed '())
  (define shared-prefix-forms/reversed '())
  (for ([form (in-list forms)])
    (define func-name (module-func-name form))
    (when func-name
      (hash-set! function-names func-name #t)
      (unless (function-import-form? form)
        (hash-set! function-defs func-name form)))
    (define global-name (module-global-name form))
    (when global-name
      (unless (global-import-form? form)
        (hash-set! global-defs global-name form)))
    (define tag-name (module-tag-name form))
    (when tag-name
      (unless (tag-import-form? form)
        (hash-set! tag-defs tag-name form)))
    (define data-name (module-data-name form))
    (when data-name
      (hash-set! data-forms data-name form)
      (hash-set! data-names data-name #t))
    (when (declare-elem-form? form)
      (set! declare-elem-forms/reversed
            (cons form declare-elem-forms/reversed)))
    (when (shared-program-prefix-form? form)
      (set! shared-prefix-forms/reversed
            (cons form shared-prefix-forms/reversed))))
  (split-form-indexes function-defs
                      global-defs
                      tag-defs
                      data-forms
                      function-names
                      data-names
                      (reverse declare-elem-forms/reversed)
                      (reverse shared-prefix-forms/reversed)))

;; record-split-program-metrics! :
;;   (listof datum?) (listof symbol?) (listof datum?) [#:program-index split-form-indexes?]
;;   -> void?
;;   Record low-cost split-program shape metrics for warm-cache benchmarks.
(define (record-split-program-metrics! program-forms
                                       imported-names
                                       import-forms
                                       #:program-index [program-index #f])
  (when (current-runtime-metric-rows)
    (define effective-program-index
      (or program-index
          (build-split-form-indexes program-forms)))
    (runtime-metric-add! "runtime:split-imported-name-count"
                         (length imported-names))
    (runtime-metric-add! "runtime:split-import-form-count"
                         (length import-forms))
    (runtime-metric-add! "runtime:split-program-form-count"
                         (length program-forms))
    (runtime-metric-add! "runtime:split-program-function-count"
                         (hash-count (split-form-indexes-function-names
                                      effective-program-index)))
    (runtime-metric-add! "runtime:split-program-global-count"
                         (hash-count (split-form-indexes-global-defs
                                      effective-program-index)))
    (runtime-metric-add! "runtime:split-program-data-count"
                         (hash-count (split-form-indexes-data-forms
                                      effective-program-index)))))

;; split-runtime-boundary-metadata :
;;   split-form-indexes? (listof symbol?) -> split-runtime-boundary-metadata?
;;   Cached runtime-side indexes and references needed at the split boundary.
(struct split-runtime-boundary-metadata
  (runtime-index
   runtime-symbol-refs)
  #:transparent)

;; sorted-hash-symbol-keys : hash? -> (listof symbol?)
;;   Return hash keys in stable symbol order.
(define (sorted-hash-symbol-keys ht)
  (sort (hash-keys ht) symbol<?))

;; function-boundary-form : datum? -> datum?
;;   Keep only the function signature needed for split-runtime imports.
(define (function-boundary-form form)
  (define name (module-func-name form))
  (unless name
    (error 'function-boundary-form
           "expected a named function form, got: ~s"
           form))
  `(func ,name ,@(function-import-signature-forms form)))

;; global-boundary-form : datum? -> datum?
;;   Keep only the global type needed for split-runtime imports.
(define (global-boundary-form form)
  (define name (module-global-name form))
  (unless name
    (error 'global-boundary-form
           "expected a named global form, got: ~s"
           form))
  `(global ,name ,(global-type-form form)))

;; tag-boundary-form : datum? -> datum?
;;   Keep only the tag signature needed for split-runtime imports.
(define (tag-boundary-form form)
  (define name (module-tag-name form))
  (unless name
    (error 'tag-boundary-form
           "expected a named tag form, got: ~s"
           form))
  `(tag ,name ,@(tag-import-signature-forms form)))

;; hash->sorted-assocs : hash? [(any/c -> any/c)] -> (listof list?)
;;   Serialize a symbol-keyed hash table as sorted association pairs.
(define (hash->sorted-assocs ht [value->datum values])
  (for/list ([key (in-list (sorted-hash-symbol-keys ht))])
    (list key (cacheable-datum (value->datum (hash-ref ht key))))))

;; sorted-assocs->hasheq : (listof list?) -> hasheq?
;;   Deserialize sorted association pairs to an eq-keyed hash table.
(define (sorted-assocs->hasheq assocs)
  (for/hasheq ([entry (in-list assocs)])
    (match entry
      [(list key value)
       (values key (cached-datum->value value))]
      [_
       (error 'split-runtime-boundary-cache
              "bad serialized hash entry: ~s"
              entry)])))

;; split-form-indexes->datum : split-form-indexes? -> list?
;;   Serialize split form indexes for the runtime boundary cache.
(define (split-form-indexes->datum indexes)
  `(split-form-indexes
    ,(hash->sorted-assocs (split-form-indexes-function-defs indexes)
                          function-boundary-form)
    ,(hash->sorted-assocs (split-form-indexes-global-defs indexes)
                          global-boundary-form)
    ,(hash->sorted-assocs (split-form-indexes-tag-defs indexes)
                          tag-boundary-form)
    ,(hash->sorted-assocs (split-form-indexes-data-forms indexes))
    ,(sorted-hash-symbol-keys (split-form-indexes-function-names indexes))
    ,(sorted-hash-symbol-keys (split-form-indexes-data-names indexes))
    ,(cacheable-datum (split-form-indexes-declare-elem-forms indexes))
    ,(cacheable-datum (split-form-indexes-shared-prefix-forms indexes))))

;; datum->split-form-indexes : any/c -> split-form-indexes?
;;   Deserialize split form indexes from the runtime boundary cache.
(define (datum->split-form-indexes v)
  (match v
    [`(split-form-indexes
       ,function-defs
       ,global-defs
       ,tag-defs
       ,data-forms
       ,function-names
       ,data-names
       ,declare-elem-forms
       ,shared-prefix-forms)
     (split-form-indexes
      (sorted-assocs->hasheq function-defs)
      (sorted-assocs->hasheq global-defs)
      (sorted-assocs->hasheq tag-defs)
      (sorted-assocs->hasheq data-forms)
      (for/hasheq ([name (in-list function-names)]) (values name #t))
      (for/hasheq ([name (in-list data-names)]) (values name #t))
      (cached-datum->value declare-elem-forms)
      (cached-datum->value shared-prefix-forms))]
    [_
     (error 'split-runtime-boundary-cache
            "bad serialized split form indexes: ~s"
            v)]))

;; split-runtime-boundary-metadata->datum : split-runtime-boundary-metadata? -> list?
;;   Serialize split-boundary metadata.
(define (split-runtime-boundary-metadata->datum metadata)
  `(split-runtime-boundary-metadata
    ,(split-form-indexes->datum
      (split-runtime-boundary-metadata-runtime-index metadata))
    ,(split-runtime-boundary-metadata-runtime-symbol-refs metadata)))

;; datum->split-runtime-boundary-metadata : any/c -> split-runtime-boundary-metadata?
;;   Deserialize split-boundary metadata.
(define (datum->split-runtime-boundary-metadata v)
  (match v
    [`(split-runtime-boundary-metadata ,runtime-index ,runtime-symbol-refs)
     (split-runtime-boundary-metadata
      (datum->split-form-indexes runtime-index)
      runtime-symbol-refs)]
    [_
     (error 'split-runtime-boundary-cache
            "bad serialized boundary metadata: ~s"
            v)]))

;; split-runtime-boundary-cache-path : any/c -> (or/c path? #f)
;;   Return the cache path for split-runtime boundary metadata.
(define (split-runtime-boundary-cache-path core-cache-key)
  (define cache-root (current-runtime-cache-dir))
  (and core-cache-key
       cache-root
       (split-runtime-boundary-cache-path/cache-root cache-root
                                                     core-cache-key)))

;; read-split-runtime-boundary-cache : (or/c path? #f) any/c
;;   -> (or/c split-runtime-boundary-metadata? #f)
;;   Read cached split-boundary metadata when present and current.
(define (read-split-runtime-boundary-cache cache-path core-cache-key)
  (define cacheable-core-cache-key (cacheable-datum core-cache-key))
  (and cache-path
       (file-exists? cache-path)
       (with-handlers ([exn:fail? (lambda (_exn) #f)])
         (match (call-with-input-file cache-path
                  (lambda (in)
                    (fasl->s-exp (port->bytes in))))
           [(list 'split-runtime-boundary-cache
                  version
                  cached-core-cache-key
                  payload)
            (and (= version split-runtime-boundary-cache-version)
                 (equal? cached-core-cache-key cacheable-core-cache-key)
                 (datum->split-runtime-boundary-metadata payload))]
           [_ #f]))))

;; read-split-runtime-boundary-metadata/cache :
;;   any/c -> (or/c split-runtime-boundary-metadata? #f)
;;   Read prewarmed split-boundary metadata by stable split-runtime cache key.
(define (read-split-runtime-boundary-metadata/cache core-cache-key)
  (read-split-runtime-boundary-cache
   (split-runtime-boundary-cache-path core-cache-key)
   core-cache-key))

;; write-split-runtime-boundary-cache! :
;;   (or/c path? #f) any/c split-runtime-boundary-metadata? -> void?
;;   Write split-boundary metadata to the runtime cache.
(define (write-split-runtime-boundary-cache! cache-path core-cache-key metadata)
  (when cache-path
    (with-handlers ([exn:fail?
                     (lambda (exn)
                       (runtime-metric-add!
                        "split-boundary-cache-write-failed"
                        (exn-message exn))
                       (with-handlers ([exn:fail? void])
                         (when (file-exists? cache-path)
                           (delete-file cache-path))))])
      (make-directory* (path-only cache-path))
      (call-with-output-file cache-path
        (lambda (out)
          (write-bytes
           (s-exp->fasl
            `(split-runtime-boundary-cache
              ,split-runtime-boundary-cache-version
              ,(cacheable-datum core-cache-key)
              ,(split-runtime-boundary-metadata->datum metadata)))
           out))
        #:exists 'truncate/replace))))

;; walk-symbol-reference-positions : any/c (symbol? -> any/c) -> void?
;;   Walk only WAT positions that refer to functions, globals, tags, or data.
(define (walk-symbol-reference-positions x record!)
  (define (walk-list xs)
    (for ([x (in-list xs)])
      (walk x)))
  (define (walk x)
    (cond
      [(pair? x)
       (match x
         [`(func ,(? symbol?) ,parts ...)
          (walk-list parts)]
         [`(global ,(? symbol?) ,parts ...)
          (walk-list parts)]
         [`(tag ,(? symbol?) ,parts ...)
          (walk-list parts)]
         [`(data ,(? symbol?) ,parts ...)
          (walk-list parts)]
         [`(type ,(? symbol?) ,parts ...)
          (walk-list parts)]
         [`(param ,(? symbol?) ,parts ...)
          (walk-list parts)]
         [`(local ,(? symbol?) ,parts ...)
          (walk-list parts)]
         [`(local.get ,_) (void)]
         [`(local.set ,_ ,parts ...)
          (walk-list parts)]
         [`(local.tee ,_ ,parts ...)
          (walk-list parts)]
         [`(block ,(? symbol?) ,parts ...)
          (walk-list parts)]
         [`(loop ,(? symbol?) ,parts ...)
          (walk-list parts)]
         [`(br ,_) (void)]
         [`(br_if ,_ ,parts ...)
          (walk-list parts)]
         [`(br_table ,_labels ... ,_default)
          (void)]
         [`(catch ,tag ,_)
          (record! tag)]
         [`(catch_ref ,tag ,_)
          (record! tag)]
         [`(catch_all ,_) (void)]
         [`(call ,name ,parts ...)
          (record! name)
          (walk-list parts)]
         [`(return_call ,name ,parts ...)
          (record! name)
          (walk-list parts)]
         [`(ref.func ,name)
          (record! name)]
         [`(global.get ,name)
          (record! name)]
         [`(global.set ,name ,parts ...)
          (record! name)
          (walk-list parts)]
         [`(throw ,tag ,parts ...)
          (record! tag)
          (walk-list parts)]
         [`(array.new_data ,_type ,data-name ,parts ...)
          (record! data-name)
          (walk-list parts)]
         [_ (walk-list x)])]
      [else (void)]))
  (walk x))

;; runtime-symbol-reference-names : (listof datum?) -> (listof symbol?)
;;   Return runtime-side function/global/tag/data references in stable order.
(define (runtime-symbol-reference-names runtime-forms)
  (define refs (make-hasheq))
  (for ([form (in-list runtime-forms)])
    (walk-symbol-reference-positions
     form
     (lambda (name)
       (hash-set! refs name #t))))
  (sort (hash-keys refs) symbol<?))

;; build-split-runtime-boundary-metadata :
;;   (listof datum?) -> split-runtime-boundary-metadata?
;;   Build runtime-side indexes and references used while splitting programs.
(define (build-split-runtime-boundary-metadata runtime-forms)
  (split-runtime-boundary-metadata
   (build-split-form-indexes runtime-forms)
   (runtime-symbol-reference-names runtime-forms)))

;; split-runtime-boundary-name-shape : split-runtime-boundary-metadata? -> any/c
;;   Return the runtime-side names that cached boundary metadata must match.
(define (split-runtime-boundary-name-shape metadata)
  (define indexes
    (split-runtime-boundary-metadata-runtime-index metadata))
  `(split-runtime-boundary-name-shape
    (function-defs
     ,(sorted-hash-symbol-keys
       (split-form-indexes-function-defs indexes)))
    (global-defs
     ,(sorted-hash-symbol-keys
       (split-form-indexes-global-defs indexes)))
    (tag-defs
     ,(sorted-hash-symbol-keys
       (split-form-indexes-tag-defs indexes)))
    (data-forms
     ,(sorted-hash-symbol-keys
       (split-form-indexes-data-forms indexes)))
    (function-names
     ,(sorted-hash-symbol-keys
       (split-form-indexes-function-names indexes)))
    (data-names
     ,(sorted-hash-symbol-keys
       (split-form-indexes-data-names indexes)))))

;; split-runtime-boundary-metadata/current? :
;;   split-runtime-boundary-metadata? (listof datum?) -> boolean?
;;   Check that cached boundary metadata still describes the runtime forms.
(define (split-runtime-boundary-metadata/current? metadata runtime-forms)
  (equal? (split-runtime-boundary-name-shape metadata)
          (split-runtime-boundary-name-shape
           (build-split-runtime-boundary-metadata runtime-forms))))

;; split-runtime-boundary-metadata/cached :
;;   (listof datum?) any/c -> split-runtime-boundary-metadata?
;;   Reuse stable runtime-side split metadata when a split-runtime cache key exists.
(define (split-runtime-boundary-metadata/cached runtime-forms core-cache-key)
  (define cache-path (split-runtime-boundary-cache-path core-cache-key))
  (runtime-metric-add! "split-boundary-cache-enabled?"
                       (if cache-path "yes" "no"))
  (define cached
    (and cache-path
         (time-runtime-step
          "runtime:split-boundary-cache-lookup"
          (lambda ()
            (read-split-runtime-boundary-cache cache-path core-cache-key)))))
  (if (and cached
           (split-runtime-boundary-metadata/current? cached runtime-forms))
      cached
      (let ([metadata
             (time-runtime-step
              "runtime:split-boundary-cache-build"
              (lambda ()
                (build-split-runtime-boundary-metadata runtime-forms)))])
        (time-runtime-step
         "runtime:split-boundary-cache-write"
         (lambda ()
           (write-split-runtime-boundary-cache! cache-path
                                                core-cache-key
                                                metadata)))
        metadata)))

;; walk-symbol-refs! : any/c hasheq? hasheq? -> void?
;;   Add symbolic references represented by keys in `available-names` to `refs`.
(define (walk-symbol-refs! x available-names refs)
  (define (record! name)
    (when (hash-ref available-names name #f)
      (hash-set! refs name #t)))
  (define (walk-list xs)
    (for ([x (in-list xs)])
      (walk x)))
  (define (walk x)
    (cond
      [(symbol? x)
       (record! x)]
      [(pair? x)
       (match x
         [`(func ,(? symbol?) ,parts ...)
          (walk-list parts)]
         [`(global ,(? symbol?) ,parts ...)
          (walk-list parts)]
         [`(tag ,(? symbol?) ,parts ...)
          (walk-list parts)]
         [`(data ,(? symbol?) ,parts ...)
          (walk-list parts)]
         [`(type ,(? symbol?) ,parts ...)
          (walk-list parts)]
         [`(param ,(? symbol?) ,parts ...)
          (walk-list parts)]
         [`(local ,(? symbol?) ,parts ...)
          (walk-list parts)]
         [`(local.get ,_) (void)]
         [`(local.set ,_ ,parts ...)
          (walk-list parts)]
         [`(local.tee ,_ ,parts ...)
          (walk-list parts)]
         [`(block ,(? symbol?) ,parts ...)
          (walk-list parts)]
         [`(loop ,(? symbol?) ,parts ...)
          (walk-list parts)]
         [`(br ,_) (void)]
         [`(br_if ,_ ,parts ...)
          (walk-list parts)]
         [`(br_table ,labels ... ,default)
          (void)]
         [`(catch ,tag ,_)
          (record! tag)]
         [`(catch_ref ,tag ,_)
          (record! tag)]
         [`(catch_all ,_) (void)]
         [`(call ,name ,parts ...)
          (record! name)
          (walk-list parts)]
         [`(return_call ,name ,parts ...)
          (record! name)
          (walk-list parts)]
         [`(ref.func ,name)
          (record! name)]
         [`(global.get ,name)
          (record! name)]
         [`(global.set ,name ,parts ...)
          (record! name)
          (walk-list parts)]
         [`(throw ,tag ,parts ...)
          (record! tag)
          (walk-list parts)]
         [`(array.new_data ,_type ,data-name ,parts ...)
          (record! data-name)
          (walk-list parts)]
         [_ (walk-list x)])]
      [else (void)]))
  (walk x))

;; walk-symbol-refs : any/c hasheq? -> (listof symbol?)
;;   Find symbolic references represented by keys in `available-names`.
(define (walk-symbol-refs x available-names)
  (define refs (make-hasheq))
  (walk-symbol-refs! x available-names refs)
  (hash-keys refs))

;; referenced-runtime-names : (listof datum?) hasheq? -> (listof symbol?)
;;   Return runtime names referenced by the program forms.
(define (referenced-runtime-names program-forms available-names)
  (define refs (make-hasheq))
  (for ([form (in-list program-forms)])
    (walk-symbol-refs! form available-names refs))
  (sort (hash-keys refs) symbol<?))

;; classified-runtime-import/export-forms :
;;   (listof datum?) (listof datum?) [#:module string?]
;;   [#:export-all? boolean?] [#:build-export-forms? boolean?]
;;   -> (values (listof symbol?) (listof datum?) (or/c (listof datum?) #f))
;;   Build import and export declarations for runtime definitions used by program forms.
(define (classified-runtime-import/export-forms runtime-forms
                                                program-forms
                                                #:module [module-name "core"]
                                                #:export-all? [export-all? #f]
                                                #:build-export-forms?
                                                [build-export-forms? #t]
                                                #:runtime-index
                                                [runtime-index #f])
  (define effective-runtime-index
    (or runtime-index
        (build-split-form-indexes runtime-forms)))
  (define function-form-by-name
    (split-form-indexes-function-defs effective-runtime-index))
  (define global-form-by-name
    (split-form-indexes-global-defs effective-runtime-index))
  (define tag-form-by-name
    (split-form-indexes-tag-defs effective-runtime-index))
  (define available-names
    (for/hasheq ([name (in-list (append (hash-keys function-form-by-name)
                                        (hash-keys global-form-by-name)
                                        (hash-keys tag-form-by-name)))])
      (values name #t)))
  (define referenced-names
    (referenced-runtime-names program-forms available-names))
  (define referenced-function-names
    (filter (lambda (name) (hash-ref function-form-by-name name #f))
            referenced-names))
  (define referenced-global-names
    (filter (lambda (name) (hash-ref global-form-by-name name #f))
            referenced-names))
  (define referenced-tag-names
    (filter (lambda (name) (hash-ref tag-form-by-name name #f))
            referenced-names))
  (define import-forms
    (append
     (for/list ([name (in-list referenced-function-names)])
       (runtime-function-import-form (hash-ref function-form-by-name name)
                                     #:module module-name))
     (for/list ([name (in-list referenced-global-names)])
       (runtime-global-import-form (hash-ref global-form-by-name name)
                                   #:module module-name))
     (for/list ([name (in-list referenced-tag-names)])
       (runtime-tag-import-form (hash-ref tag-form-by-name name)
                                #:module module-name))))
  (define export-forms
    (and build-export-forms?
         (runtime-index-export-forms
          effective-runtime-index
          #:function-names (if export-all? #f referenced-function-names)
          #:global-names   (if export-all? #f referenced-global-names)
          #:tag-names      (if export-all? #f referenced-tag-names))))
  (values (append referenced-function-names
                  referenced-global-names
                  referenced-tag-names)
          import-forms
          export-forms))

;; classified-runtime-import/export+data-forms :
;;   split-form-indexes? (listof datum?) split-form-indexes?
;;   [#:module string?] [#:export-all? boolean?] [#:build-export-forms? boolean?]
;;   -> (values (listof symbol?) (listof datum?) (or/c (listof datum?) #f) (listof datum?))
;;   Build runtime imports and referenced runtime data from one program reference walk.
(define (classified-runtime-import/export+data-forms runtime-index
                                                     program-forms
                                                     program-index
                                                     #:module [module-name "core"]
                                                     #:export-all? [export-all? #f]
                                                     #:build-export-forms?
                                                     [build-export-forms? #t])
  (define function-form-by-name
    (split-form-indexes-function-defs runtime-index))
  (define global-form-by-name
    (split-form-indexes-global-defs runtime-index))
  (define tag-form-by-name
    (split-form-indexes-tag-defs runtime-index))
  (define runtime-data-form-by-name
    (split-form-indexes-data-forms runtime-index))
  (define program-data-name-set
    (split-form-indexes-data-names program-index))
  (define available-names
    (for/hasheq ([name (in-list (append (hash-keys function-form-by-name)
                                        (hash-keys global-form-by-name)
                                        (hash-keys tag-form-by-name)
                                        (hash-keys runtime-data-form-by-name)))])
      (values name #t)))
  (define referenced-names
    (referenced-runtime-names program-forms available-names))
  (define referenced-function-names
    (filter (lambda (name) (hash-ref function-form-by-name name #f))
            referenced-names))
  (define referenced-global-names
    (filter (lambda (name) (hash-ref global-form-by-name name #f))
            referenced-names))
  (define referenced-tag-names
    (filter (lambda (name) (hash-ref tag-form-by-name name #f))
            referenced-names))
  (define referenced-data-names
    (filter (lambda (name)
              (and (hash-ref runtime-data-form-by-name name #f)
                   (not (hash-ref program-data-name-set name #f))))
            referenced-names))
  (define import-forms
    (append
     (for/list ([name (in-list referenced-function-names)])
       (runtime-function-import-form (hash-ref function-form-by-name name)
                                     #:module module-name))
     (for/list ([name (in-list referenced-global-names)])
       (runtime-global-import-form (hash-ref global-form-by-name name)
                                   #:module module-name))
     (for/list ([name (in-list referenced-tag-names)])
       (runtime-tag-import-form (hash-ref tag-form-by-name name)
                                #:module module-name))))
  (define export-forms
    (and build-export-forms?
         (runtime-index-export-forms
          runtime-index
          #:function-names (if export-all? #f referenced-function-names)
          #:global-names   (if export-all? #f referenced-global-names)
          #:tag-names      (if export-all? #f referenced-tag-names))))
  (define runtime-data-forms
    (for/list ([name (in-list (sort referenced-data-names symbol<?))])
      (hash-ref runtime-data-form-by-name name)))
  (values (append referenced-function-names
                  referenced-global-names
                  referenced-tag-names)
          import-forms
          export-forms
          runtime-data-forms))

;; runtime-index-export-forms :
;;   split-form-indexes? [#:function-names (or/c (listof symbol?) #f)]
;;                       [#:global-names (or/c (listof symbol?) #f)]
;;                       [#:tag-names (or/c (listof symbol?) #f)] -> (listof datum?)
;;   Build export declarations from a runtime form index.
(define (runtime-index-export-forms runtime-index
                                    #:function-names [function-names #f]
                                    #:global-names [global-names #f]
                                    #:tag-names [tag-names #f])
  (define function-form-by-name
    (split-form-indexes-function-defs runtime-index))
  (define global-form-by-name
    (split-form-indexes-global-defs runtime-index))
  (define tag-form-by-name
    (split-form-indexes-tag-defs runtime-index))
  (define effective-function-names
    (or function-names (sort (hash-keys function-form-by-name) symbol<?)))
  (define effective-global-names
    (or global-names (sort (hash-keys global-form-by-name) symbol<?)))
  (define effective-tag-names
    (or tag-names (sort (hash-keys tag-form-by-name) symbol<?)))
  (append
   (map runtime-function-export-form effective-function-names)
   (map runtime-global-export-form effective-global-names)
   (map runtime-tag-export-form effective-tag-names)))

;; type-prefix-form? : any/c -> boolean?
;;   Check whether form declares types that must be duplicated across modules.
(define (type-prefix-form? form)
  (and (pair? form)
       (memq (car form) '(type rec))))

;; top-level-import-form? : any/c -> boolean?
;;   Check whether form is an old-style top-level import declaration.
(define (top-level-import-form? form)
  (and (pair? form)
       (eq? (car form) 'import)))

;; shared-program-prefix-form? : any/c -> boolean?
;;   Check whether form must be duplicated before program-side runtime imports.
(define (shared-program-prefix-form? form)
  (or (type-prefix-form? form)
      (top-level-import-form? form)
      (function-import-form? form)
      (global-import-form? form)
      (tag-import-form? form)))

;; append-unique-forms : (listof datum?) ... -> (listof datum?)
;;   Append form lists while preserving the first occurrence of each form.
(define (append-unique-forms . form-lists)
  (define seen (make-hash))
  (for*/list ([forms (in-list form-lists)]
              [form (in-list forms)]
              #:unless (hash-ref seen form #f))
    (hash-set! seen form #t)
    form))

;; remove-sidecar-owned-forms : (listof datum?) (listof datum?) -> (listof datum?)
;;   Drop forms that are owned by a sidecar module.
(define (remove-sidecar-owned-forms forms sidecar-forms)
  (cond
    [(null? sidecar-forms) forms]
    [else
     (define sidecar-form-set
       (for/hash ([form (in-list sidecar-forms)])
         (values form #t)))
     (for/list ([form (in-list forms)]
                #:unless (hash-ref sidecar-form-set form #f))
       form)]))

;; sidecar-owned-import-form? : datum? split-form-indexes? -> boolean?
;;   Check whether an import duplicates a definition owned by the sidecar.
(define (sidecar-owned-import-form? form sidecar-index)
  (cond
    [(function-import-form? form)
     (define name (module-func-name form))
     (and name
          (hash-has-key? (split-form-indexes-function-defs sidecar-index)
                         name))]
    [(global-import-form? form)
     (define name (module-global-name form))
     (and name
          (hash-has-key? (split-form-indexes-global-defs sidecar-index)
                         name))]
    [(tag-import-form? form)
     (define name (module-tag-name form))
     (and name
          (hash-has-key? (split-form-indexes-tag-defs sidecar-index)
                         name))]
    [else #f]))

;; declare-elem-form? : any/c -> boolean?
;;   Check whether form is an `elem declare funcref` declaration.
(define (declare-elem-form? form)
  (match form
    [`(elem declare funcref . ,_) #t]
    [_ #f]))

;; ref-func-name : any/c -> (or/c symbol? #f)
;;   Return the function name referenced by a `ref.func` expression.
(define (ref-func-name form)
  (match form
    [`(ref.func ,(? symbol? name)) name]
    [_ #f]))

;; filter-declare-elem-form : datum? hasheq? -> (or/c datum? #f)
;;   Keep only declared function refs present in `names`.
(define (filter-declare-elem-form form names)
  (match form
    [`(elem declare funcref ,refs ...)
     (define kept-refs
       (for/list ([ref (in-list refs)]
                  #:do [(define name (ref-func-name ref))]
                  #:when (and name (hash-ref names name #f)))
         ref))
     (and (pair? kept-refs)
          `(elem declare funcref ,@kept-refs))]
    [_
     (error 'filter-declare-elem-form
            "expected an elem declare funcref form, got: ~s"
            form)]))

;; split-declare-elem-forms : (listof datum?) hasheq? hasheq?
;;   -> (values (listof datum?) (listof datum?))
;;   Split declared function refs according to runtime and program availability.
(define (split-declare-elem-forms forms runtime-function-names program-function-names)
  (for/fold ([runtime-forms '()]
             [program-forms '()])
            ([form (in-list forms)]
             #:when (declare-elem-form? form))
    (define runtime-form
      (filter-declare-elem-form form runtime-function-names))
    (define program-form
      (filter-declare-elem-form form program-function-names))
    (values (if runtime-form (cons runtime-form runtime-forms) runtime-forms)
            (if program-form (cons program-form program-forms) program-forms))))

;; declare-elem-forms-for-names : (listof datum?) hasheq? -> (listof datum?)
;;   Keep declared function refs that are present in names.
(define (declare-elem-forms-for-names forms names)
  (for/fold ([kept-forms '()])
            ([form (in-list forms)]
             #:when (declare-elem-form? form))
    (define kept-form
      (filter-declare-elem-form form names))
    (if kept-form
        (cons kept-form kept-forms)
        kept-forms)))

;; elem-form-ref-names : datum? -> (listof symbol?)
;;   Return function names referenced by an `elem declare funcref` form.
(define (elem-form-ref-names form)
  (match form
    [`(elem declare funcref ,refs ...)
     (for/list ([ref (in-list refs)]
                #:do [(define name (ref-func-name ref))]
                #:when name)
       name)]
    [_ '()]))

;; elem-forms-name-set : (listof datum?) -> hasheq?
;;   Build a set of names already present in declaration elem forms.
(define (elem-forms-name-set forms)
  (for*/hasheq ([form (in-list forms)]
                [name (in-list (elem-form-ref-names form))])
    (values name #t)))

;; direct-ref-func-names : (listof datum?) hasheq? -> (listof symbol?)
;;   Return available function names referenced by `ref.func` expressions.
(define (direct-ref-func-names forms available-function-names)
  (define seen (make-hasheq))
  (define names-reversed '())
  (define (record! name)
    (when (and (hash-ref available-function-names name #f)
               (not (hash-ref seen name #f)))
      (hash-set! seen name #t)
      (set! names-reversed (cons name names-reversed))))
  (define (walk x)
    (cond
      [(pair? x)
       (match x
         [`(ref.func ,name)
          (record! name)]
         [_ (for ([part (in-list x)])
              (walk part))])]
      [else (void)]))
  (for ([form (in-list forms)])
    (walk form))
  (reverse names-reversed))

;; supplemental-declare-elem-forms : (listof datum?) (listof symbol?) -> (listof datum?)
;;   Add elem declarations for direct ref.func uses missing from existing elem forms.
(define (supplemental-declare-elem-forms existing-forms direct-names)
  (define existing-name-set (elem-forms-name-set existing-forms))
  (define missing-names
    (for/list ([name (in-list direct-names)]
               #:unless (hash-ref existing-name-set name #f))
      name))
  (if (null? missing-names)
      '()
      (list `(elem declare funcref
                  ,@(for/list ([name (in-list missing-names)])
                      `(ref.func ,name))))))

;; form-function-name-set : (listof datum?) -> hasheq?
;;   Build a set of named function declarations in module body forms.
(define (form-function-name-set forms)
  (for/hasheq ([form (in-list forms)]
               #:do [(define name (module-func-name form))]
               #:when name)
    (values name #t)))

;; sorted-named-forms : (listof datum?) (datum? -> (or/c symbol? #f)) -> (listof datum?)
;;   Sort named forms into canonical order for stable split-runtime fingerprints.
(define (sorted-named-forms forms name-of)
  (sort forms
        (lambda (a b)
          (define a-name (name-of a))
          (define b-name (name-of b))
          (cond
            [(and a-name b-name) (symbol<? a-name b-name)]
            [a-name #t]
            [else #f]))))

;; split-runtime-top-name-summary : (listof symbol?) -> string?
;;   Render a short stable list of names for runtime boundary diagnostics.
(define (split-runtime-top-name-summary names)
  (if (null? names)
      "-"
      (string-join
       (for/list ([name (in-list (take (sort names symbol<?)
                                       (min 8 (length names))))])
         (symbol->string name))
       ", ")))

;; record-split-runtime-boundary-blocker-metrics! :
;;   (listof symbol?) (listof symbol?) boolean? -> void?
;;   Record program-owned names that force a non-cacheable runtime core.
(define (record-split-runtime-boundary-blocker-metrics! global-names
                                                        data-names
                                                        stable-core?)
  (runtime-metric-add! "runtime:split-stable-core-cache-key-eligible?"
                       (if stable-core? "yes" "no"))
  (runtime-metric-add! "runtime:split-runtime-pulled-program-global-count"
                       (length global-names))
  (runtime-metric-add! "runtime:split-runtime-pulled-program-data-count"
                       (length data-names))
  (runtime-metric-add! "runtime:split-runtime-pulled-program-global-top-names"
                       (split-runtime-top-name-summary global-names))
  (runtime-metric-add! "runtime:split-runtime-pulled-program-data-top-names"
                       (split-runtime-top-name-summary data-names)))

;; constant-declaration-form? : datum? -> boolean?
;;   Check whether a form declares a generated runtime/program constant.
(define (constant-declaration-form? form)
  (or (let ([name (module-data-name form)])
        (and name (constant-data-name? name)))
      (let ([name (module-global-name form)])
        (and name (constant-global-name? name)))))

;; constant-declaration-form-name : datum? -> (or/c symbol? #f)
;;   Return the generated name for a constant declaration form.
(define (constant-declaration-form-name form)
  (or (module-data-name form)
      (module-global-name form)))

;; canonicalize-runtime-constant-declarations : (listof datum?) -> (listof datum?)
;;   Merge pulled-back constants into one canonical runtime constant block.
(define (canonicalize-runtime-constant-declarations forms)
  (define-values (constant-forms non-constant-forms/reversed first-index _index)
    (for/fold ([constant-forms '()]
               [non-constant-forms/reversed '()]
               [first-index #f]
               [index 0])
              ([form (in-list forms)])
      (if (constant-declaration-form? form)
          (values (cons form constant-forms)
                  non-constant-forms/reversed
                  (or first-index index)
                  (add1 index))
          (values constant-forms
                  (cons form non-constant-forms/reversed)
                  first-index
                  (add1 index)))))
  (define sorted-constant-forms
    (sorted-named-forms constant-forms constant-declaration-form-name))
  (define non-constant-forms (reverse non-constant-forms/reversed))
  (cond
    [(null? sorted-constant-forms)
     non-constant-forms]
    [else
     (define insertion-index (or first-index (length non-constant-forms)))
     (append (take non-constant-forms insertion-index)
             sorted-constant-forms
             (drop non-constant-forms insertion-index))]))

;; constant-global-definition-index : split-form-indexes? -> hasheq?
;;   Return constant global definitions from a program-side form index.
(define (constant-global-definition-index program-index)
  (for/hasheq ([(name form) (in-hash (split-form-indexes-global-defs program-index))]
               #:when (constant-global-name? name))
    (values name form)))

;; constant-data-form-index : split-form-indexes? -> hasheq?
;;   Return constant passive data forms from a program-side form index.
(define (constant-data-form-index program-index)
  (for/hasheq ([(name form) (in-hash (split-form-indexes-data-forms program-index))]
               #:when (constant-data-name? name))
    (values name form)))

;; runtime-referenced-program-global-names :
;;   (listof datum?) (listof datum?) [#:program-index split-form-indexes?]
;;   -> (listof symbol?)
;;   Return program-owned globals that are referenced by runtime-side forms.
(define (runtime-referenced-program-global-names
         runtime-forms
         program-forms
         #:program-index [program-index #f])
  (define program-global-form-by-name
    (constant-global-definition-index
     (or program-index
         (build-split-form-indexes program-forms))))
  (define available-names
    (for/hasheq ([name (in-list (hash-keys program-global-form-by-name))])
      (values name #t)))
  (referenced-runtime-names runtime-forms available-names))

;; runtime-referenced-program-global-names/from-refs :
;;   (listof symbol?) split-form-indexes? [#:runtime-index split-form-indexes?]
;;   -> (listof symbol?)
;;   Return program-owned globals that cached runtime-side refs still need.
(define (runtime-referenced-program-global-names/from-refs runtime-symbol-refs
                                                           program-index
                                                           #:runtime-index
                                                           [runtime-index #f])
  (define program-global-form-by-name
    (constant-global-definition-index program-index))
  (define runtime-global-form-by-name
    (and runtime-index
         (constant-global-definition-index runtime-index)))
  (for/list ([name (in-list runtime-symbol-refs)]
             #:when (and (hash-ref program-global-form-by-name name #f)
                         (not (and runtime-global-form-by-name
                                   (hash-ref runtime-global-form-by-name
                                             name
                                             #f)))))
    name))

;; runtime-owned-program-constant-global-names :
;;   split-form-indexes? split-form-indexes? -> (listof symbol?)
;;   Return program constant globals that duplicate runtime-owned constants.
(define (runtime-owned-program-constant-global-names runtime-index program-index)
  (define runtime-global-form-by-name
    (constant-global-definition-index runtime-index))
  (define program-global-form-by-name
    (constant-global-definition-index program-index))
  (sort
   (for/list ([name (in-list (hash-keys program-global-form-by-name))]
              #:when (hash-ref runtime-global-form-by-name name #f))
     name)
   symbol<?))

;; split-runtime-referenced-program-globals : (listof datum?) (listof symbol?)
;;   -> (values (listof datum?) (listof datum?))
;;   Pull program-owned globals back to the runtime when runtime forms use them.
(define (split-runtime-referenced-program-globals program-forms names)
  (define name-set
    (for/hasheq ([name (in-list names)])
      (values name #t)))
  (for/fold ([runtime-global-forms '()]
             [remaining-program-forms '()])
            ([form (in-list program-forms)])
    (define name (module-global-name form))
    (if (and name
             (hash-ref name-set name #f))
        (values (cons form runtime-global-forms)
                remaining-program-forms)
        (values runtime-global-forms
                (cons form remaining-program-forms)))))

;; program-referenced-runtime-data-forms :
;;   (or/c (listof datum?) #f) (listof datum?) [#:runtime-index split-form-indexes?]
;;   [#:program-index split-form-indexes?]
;;   -> (listof datum?)
;;   Return runtime-owned passive data forms that program-side forms reference.
(define (program-referenced-runtime-data-forms
         runtime-forms
         program-forms
         #:runtime-index [runtime-index #f]
         #:program-index [program-index #f])
  (define effective-runtime-index
    (or runtime-index
        (build-split-form-indexes runtime-forms)))
  (define effective-program-index
    (or program-index
        (build-split-form-indexes program-forms)))
  (define runtime-data-form-by-name
    (split-form-indexes-data-forms effective-runtime-index))
  (define program-data-name-set
    (split-form-indexes-data-names effective-program-index))
  (define available-names
    (for/hasheq ([name (in-list (hash-keys runtime-data-form-by-name))])
      (values name #t)))
  (define referenced-name-set
    (for/hasheq ([name (in-list (referenced-runtime-names program-forms
                                                           available-names))])
      (values name #t)))
  (cond
    [runtime-forms
     (for/list ([form (in-list runtime-forms)]
                #:do [(define name (module-data-name form))]
                #:when (and name
                            (hash-ref referenced-name-set name #f)
                            (not (hash-ref program-data-name-set name #f))))
       form)]
    [runtime-index
     (for/list ([name (in-list
                       (sort (hash-keys runtime-data-form-by-name) symbol<?))]
                #:when (and (hash-ref referenced-name-set name #f)
                            (not (hash-ref program-data-name-set name #f))))
       (hash-ref runtime-data-form-by-name name))]
    [else
     (error 'program-referenced-runtime-data-forms
            "runtime forms or runtime index required")]))

;; runtime-referenced-program-data-forms :
;;   (listof datum?) (listof datum?) [#:program-index split-form-indexes?]
;;   -> (listof datum?)
;;   Return program-owned passive data forms that runtime-side forms reference.
(define (runtime-referenced-program-data-forms
         runtime-forms
         program-forms
         #:program-index [program-index #f])
  (define program-data-form-by-name
    (constant-data-form-index
     (or program-index
         (build-split-form-indexes program-forms))))
  (define available-names
    (for/hasheq ([name (in-list (hash-keys program-data-form-by-name))])
      (values name #t)))
  (define referenced-name-set
    (for/hasheq ([name (in-list (referenced-runtime-names runtime-forms
                                                           available-names))])
      (values name #t)))
  (for/list ([form (in-list program-forms)]
             #:do [(define name (module-data-name form))]
             #:when (and name (hash-ref referenced-name-set name #f)))
    form))

;; runtime-referenced-program-data-forms/from-refs :
;;   (listof symbol?) split-form-indexes? [#:runtime-index split-form-indexes?]
;;   -> (listof datum?)
;;   Return program-owned passive data forms that cached runtime refs still need.
(define (runtime-referenced-program-data-forms/from-refs runtime-symbol-refs
                                                        program-index
                                                        #:runtime-index
                                                        [runtime-index #f])
  (define program-data-form-by-name
    (constant-data-form-index program-index))
  (define runtime-data-form-by-name
    (and runtime-index
         (constant-data-form-index runtime-index)))
  (for/list ([name (in-list runtime-symbol-refs)]
             #:do [(define form (hash-ref program-data-form-by-name name #f))]
             #:when (and form
                         (not (and runtime-data-form-by-name
                                   (hash-ref runtime-data-form-by-name
                                             name
                                             #f)))))
    form))

;; split-runtime-reference-compatible?/index :
;;   split-runtime-boundary-metadata? split-form-indexes? -> boolean?
;;   Check compatibility using an already-built program form index.
(define (split-runtime-reference-compatible?/index boundary-metadata program-index)
  (define runtime-symbol-refs
    (split-runtime-boundary-metadata-runtime-symbol-refs boundary-metadata))
  (define runtime-index
    (split-runtime-boundary-metadata-runtime-index boundary-metadata))
  (and (null? (runtime-referenced-program-global-names/from-refs
               runtime-symbol-refs
               program-index
               #:runtime-index runtime-index))
       (null? (runtime-referenced-program-data-forms/from-refs
               runtime-symbol-refs
               program-index
               #:runtime-index runtime-index))))

;; split-runtime-reference-compatible+index :
;;   split-runtime-boundary-metadata? (listof datum?)
;;   -> (values boolean? split-form-indexes?)
;;   Check compatibility and return the program index built during the check.
(define (split-runtime-reference-compatible+index boundary-metadata program-forms)
  (define program-index
    (build-split-form-indexes program-forms))
  (values (split-runtime-reference-compatible?/index boundary-metadata
                                                     program-index)
          program-index))

;; split-runtime-reference-compatible? :
;;   split-runtime-boundary-metadata? (listof datum?) -> boolean?
;;   Check whether program forms can link against an unchanged cached core.
(define (split-runtime-reference-compatible? boundary-metadata program-forms)
  (define-values (compatible? _program-index)
    (split-runtime-reference-compatible+index boundary-metadata program-forms))
  compatible?)

;; module-forms : datum? -> (listof datum?)
;;   Return a WAT module's body forms.
(define (module-forms module)
  (match module
    [`(module ,forms ...) forms]
    [_ (error 'module-forms "expected a WAT module, got: ~s" module)]))

;; helper-name-set : (listof core-runtime-helper?) -> hasheq?
;;   Build an eq-keyed set of helper function names.
(define (helper-name-set helpers)
  (for/hasheq ([helper (in-list helpers)])
    (values (core-runtime-helper-name helper) #t)))

;; helper-definition-counts : (listof datum?) hasheq? -> hasheq?
;;   Count helper definitions present in module forms.
(define (helper-definition-counts forms names)
  (define counts (make-hasheq))
  (for ([form (in-list forms)]
        #:when (function-definition-form? form))
    (define name (module-func-name form))
    (when (hash-ref names name #f)
      (hash-update! counts name add1 0)))
  counts)

;; sorted-symbols : (listof symbol?) -> (listof symbol?)
;;   Sort symbols by printed name for stable diagnostics.
(define (sorted-symbols syms)
  (sort syms string<? #:key symbol->string))

;; check-helper-definitions! : (listof core-runtime-helper?) hasheq? -> void?
;;   Require exactly one definition for each helper selected for splitting.
(define (check-helper-definitions! helpers counts)
  (define missing
    (for/list ([helper (in-list helpers)]
               #:do [(define name (core-runtime-helper-name helper))]
               #:when (zero? (hash-ref counts name 0)))
      name))
  (define duplicates
    (for/list ([(name count) (in-hash counts)]
               #:when (> count 1))
      name))
  (unless (null? missing)
    (error 'split-core-runtime-module
           "missing core helper definitions: ~a"
           (sorted-symbols missing)))
  (unless (null? duplicates)
    (error 'split-core-runtime-module
           "duplicate core helper definitions: ~a"
           (sorted-symbols duplicates))))

;; remove-helper-definitions : (listof datum?) hasheq? -> (listof datum?)
;;   Remove selected helper function definitions from module body forms.
(define (remove-helper-definitions forms names)
  (for/list ([form (in-list forms)]
             #:unless (and (function-definition-form? form)
                           (hash-ref names (module-func-name form) #f)))
    form))

;; split-core-runtime-shared-global-imports :
;;   (listof datum?) (listof symbol?) string? -> (values (listof datum?) (listof datum?) (listof symbol?))
;;   Replace core-owned program global definitions with imports from the core runtime.
(define (split-core-runtime-shared-global-imports forms names import-module-name)
  (define name-set
    (for/hasheq ([name (in-list names)])
      (values name #t)))
  (for/fold ([import-forms '()]
             [remaining-forms '()]
             [imported-names '()])
            ([form (in-list forms)])
    (define name (and (global-definition-form? form)
                      (module-global-name form)))
    (if (and name
             (hash-ref name-set name #f))
        (values (cons (runtime-global-import-form form
                                                  #:module import-module-name)
                      import-forms)
                remaining-forms
                (cons name imported-names))
        (values import-forms
                (cons form remaining-forms)
                imported-names))))

;; split-core-runtime-shared-tag-imports :
;;   (listof datum?) (listof symbol?) string? -> (values (listof datum?) (listof datum?) (listof symbol?))
;;   Replace core-owned program tag definitions with imports from the core runtime.
(define (split-core-runtime-shared-tag-imports forms names import-module-name)
  (define name-set
    (for/hasheq ([name (in-list names)])
      (values name #t)))
  (for/fold ([import-forms '()]
             [remaining-forms '()]
             [imported-names '()])
            ([form (in-list forms)])
    (define name (and (tag-definition-form? form)
                      (module-tag-name form)))
    (if (and name
             (hash-ref name-set name #f))
        (values (cons (runtime-tag-import-form form
                                               #:module import-module-name)
                      import-forms)
                remaining-forms
                (cons name imported-names))
        (values import-forms
                (cons form remaining-forms)
                imported-names))))

;; remove-core-owned-constant-global-sets : datum? hasheq? -> (or/c datum? #f)
;;   Remove program-side initializer writes for constants imported from the core runtime.
(define (remove-core-owned-constant-global-sets form core-owned-global-names)
  (remove-dead-constant-global-sets form core-owned-global-names))

;; collect-constant-global-gets : datum? -> (listof symbol?)
;;   Collect generated constant globals whose values are read.
(define (collect-constant-global-gets form)
  (match form
    [`(global.get ,(? constant-global-name? name))
     (list name)]
    [(? list?)
     (append-map collect-constant-global-gets form)]
    [_ '()]))

;; collect-constant-data-refs : datum? -> (listof symbol?)
;;   Collect generated constant data segments referenced by instructions.
(define (collect-constant-data-refs form)
  (match form
    [`(array.new_data ,_ ,(? constant-data-name? name) ,body ...)
     (cons name (append-map collect-constant-data-refs body))]
    [(? list?)
     (append-map collect-constant-data-refs form)]
    [_ '()]))

;; constant-global-definition-name : datum? -> (or/c symbol? #f)
;;   Return a generated constant global definition name.
(define (constant-global-definition-name form)
  (define name (and (global-definition-form? form)
                    (module-global-name form)))
  (and name
       (constant-global-name? name)
       name))

;; constant-data-definition-name : datum? -> (or/c symbol? #f)
;;   Return a generated constant data definition name.
(define (constant-data-definition-name form)
  (define name (module-data-name form))
  (and name
       (constant-data-name? name)
       name))

;; remove-dead-constant-global-sets : datum? hasheq? -> (or/c datum? #f)
;;   Remove initializer writes to generated constants that no remaining code reads.
(define (remove-dead-constant-global-sets form dead-global-names)
  (match form
    [`(global.set ,(? (lambda (name)
                        (hash-ref dead-global-names name #f)))
                  ,_ ...)
     #f]
    [(? list?)
     (for/list ([part (in-list form)]
                #:do [(define rewritten
                        (remove-dead-constant-global-sets part
                                                          dead-global-names))]
                #:when rewritten)
       rewritten)]
    [_ form]))

;; prune-unreferenced-constant-forms : (listof datum?) -> (listof datum?)
;;   Drop generated constants left dead after split-core helper extraction.
(define (prune-unreferenced-constant-forms forms)
  (define used-global-names
    (for*/hasheq ([form (in-list forms)]
                  [name (in-list (collect-constant-global-gets form))])
      (values name #t)))
  (define dead-global-names
    (for/hasheq ([form (in-list forms)]
                 #:do [(define name (constant-global-definition-name form))]
                 #:when (and name
                             (not (hash-ref used-global-names name #f))))
      (values name #t)))
  (define forms-without-dead-globals
    (for/list ([form (in-list forms)]
               #:unless (let ([name (constant-global-definition-name form)])
                           (and name
                                (hash-ref dead-global-names name #f)))
               #:do [(define rewritten
                       (remove-dead-constant-global-sets form
                                                         dead-global-names))]
               #:when rewritten)
      rewritten))
  (define used-data-names
    (for*/hasheq ([form (in-list forms-without-dead-globals)]
                  [name (in-list (collect-constant-data-refs form))])
      (values name #t)))
  (for/list ([form (in-list forms-without-dead-globals)]
             #:unless (let ([name (constant-data-definition-name form)])
                         (and name
                              (not (hash-ref used-data-names name #f)))))
    form))

;; split-core-runtime-module :
;;   datum? [#:helpers (listof core-runtime-helper?)]
;;          [#:require-all? boolean?] [#:import-module string?]
;;          [#:core-module (or/c datum? promise?)] [#:core-cache-key any/c]
;;          -> split-runtime-modules?
;;   Rewrite a full module so registered core helper definitions become imports.
(define (split-core-runtime-module module
                                   #:helpers [helpers core-runtime-helpers]
                                   #:require-all? [require-all? #t]
                                   #:import-module [import-module-name "core"]
                                   #:core-module
                                   [core-module default-core-module-sentinel]
                                   #:core-cache-key [core-cache-key #f])
  (define effective-core-module
    (cond
      [(not (eq? core-module default-core-module-sentinel))
       core-module]
      [core-cache-key
       (delay (generate-core-runtime))]
      [else
       (generate-core-runtime)]))
  (define forms (module-forms module))
  (define names (helper-name-set helpers))
  (define counts (helper-definition-counts forms names))
  (define forms-without-helpers (remove-helper-definitions forms names))
  (define referenced-helper-name-set
    (for/hasheq ([name (in-list (referenced-runtime-names forms-without-helpers names))])
      (values name #t)))
  (define selected-helpers
    (if require-all?
        helpers
        (for/list ([helper (in-list helpers)]
                   #:do [(define name (core-runtime-helper-name helper))]
                   #:when (or (positive? (hash-ref counts name 0))
                              (hash-ref referenced-helper-name-set name #f)))
          helper)))
  (define helpers-selected-from-definitions
    (if require-all?
        selected-helpers
        (for/list ([helper (in-list selected-helpers)]
                   #:when (positive? (hash-ref counts
                                                (core-runtime-helper-name helper)
                                                0)))
          helper)))
  (check-helper-definitions! helpers-selected-from-definitions counts)
  (define core-owned-preinitialized-global-name-set
    (for/hasheq ([name (in-list
                        (append core-runtime-program-import-constant-global-names
                                core-runtime-program-preinitialized-global-names))])
      (values name #t)))
  (define forms-without-core-owned-constant-initializers
    (for/list ([form (in-list forms-without-helpers)]
               #:do [(define rewritten
                       (remove-core-owned-constant-global-sets
                        form
                        core-owned-preinitialized-global-name-set))]
               #:when rewritten)
      rewritten))
  (define pruned-program-forms
    (prune-unreferenced-constant-forms
     forms-without-core-owned-constant-initializers))
  (define-values (global-import-forms/reversed
                  forms-without-core-globals/reversed
                  imported-global-names/reversed)
    (split-core-runtime-shared-global-imports
     pruned-program-forms
     (append core-runtime-program-import-global-names
             core-runtime-program-import-constant-global-names)
     import-module-name))
  (define global-import-forms (reverse global-import-forms/reversed))
  (define forms-without-core-globals (reverse forms-without-core-globals/reversed))
  (define imported-global-names (reverse imported-global-names/reversed))
  (define-values (tag-import-forms/reversed
                  forms-without-core-tags/reversed
                  imported-tag-names/reversed)
    (split-core-runtime-shared-tag-imports
     forms-without-core-globals
     core-runtime-program-import-tag-names
     import-module-name))
  (define tag-import-forms (reverse tag-import-forms/reversed))
  (define forms-without-core-tags (reverse forms-without-core-tags/reversed))
  (define imported-tag-names (reverse imported-tag-names/reversed))
  (define helper-import-forms
    (core-runtime-helper-import-forms selected-helpers #:module import-module-name))
  (define import-forms
    (append helper-import-forms
            global-import-forms
            tag-import-forms))
  (define-values (prefix rest)
    (splitf-at forms-without-core-tags import-insertion-prefix-form?))
  (define program-forms
    (append prefix import-forms rest))
  (record-split-program-metrics! program-forms
                                 (append (map core-runtime-helper-name selected-helpers)
                                         imported-global-names
                                         imported-tag-names)
                                 import-forms)
  (split-runtime-modules
   effective-core-module
   `(module ,@program-forms)
   (append (map core-runtime-helper-name selected-helpers)
           imported-global-names
           imported-tag-names)
   import-forms
   core-cache-key
   #f
   #f))

;; split-classified-runtime-forms :
;;   (or/c (listof datum?) #f) (listof datum?) [#:import-module string?]
;;   -> split-runtime-modules?
;;   Build runtime/program modules from already-classified module body forms.
(define (split-classified-runtime-forms
         runtime-forms
         initial-program-forms
         #:import-module [import-module-name "core"]
         #:core-cache-key [core-cache-key #f]
         #:boundary-metadata [cached-boundary-metadata #f]
         #:initial-program-index [cached-initial-program-index #f]
         #:ffi-sidecar-forms [ffi-sidecar-forms '()]
         #:ffi-sidecar-extra-forms [ffi-sidecar-extra-forms '()]
         #:ffi-sidecar-function-names [ffi-sidecar-function-names '()]
         #:ffi-import-module [ffi-import-module-name "ffi"]
         #:ffi-cache-key [ffi-cache-key #f])
  (define boundary-metadata
    (cond
      [(and cached-boundary-metadata
            runtime-forms
            (split-runtime-boundary-metadata/current?
             cached-boundary-metadata
             runtime-forms))
       cached-boundary-metadata]
      [(and cached-boundary-metadata runtime-forms)
       (split-runtime-boundary-metadata/cached runtime-forms core-cache-key)]
      [cached-boundary-metadata
       cached-boundary-metadata]
      [runtime-forms
       (split-runtime-boundary-metadata/cached runtime-forms core-cache-key)]
      [else
       (error 'split-classified-runtime-forms
              "runtime forms required when boundary metadata is absent")]))
  (define runtime-source-index
    (split-runtime-boundary-metadata-runtime-index boundary-metadata))
  (define runtime-symbol-refs
    (split-runtime-boundary-metadata-runtime-symbol-refs boundary-metadata))
  (define initial-program-index
    (or cached-initial-program-index
        (build-split-form-indexes initial-program-forms)))
  (define pulled-runtime-global-names
    (runtime-referenced-program-global-names/from-refs runtime-symbol-refs
                                                       initial-program-index
                                                       #:runtime-index
                                                       runtime-source-index))
  (define-values (pulled-runtime-global-forms
                  program-forms-before-runtime-owned-constant-prune
                  program-index-before-runtime-owned-constant-prune)
    (cond
      [(null? pulled-runtime-global-names)
       (values '() initial-program-forms initial-program-index)]
      [else
       (define-values (pulled-runtime-global-forms/reversed
                       remaining-program-forms/reversed)
         (split-runtime-referenced-program-globals
          initial-program-forms
          pulled-runtime-global-names))
       (define program-forms-before-runtime-owned-constant-prune
         (reverse remaining-program-forms/reversed))
       (values
        (sorted-named-forms
         (reverse pulled-runtime-global-forms/reversed)
         module-global-name)
        program-forms-before-runtime-owned-constant-prune
        (build-split-form-indexes
         program-forms-before-runtime-owned-constant-prune))]))
  (define runtime-owned-program-constant-global-name-set
    (for/hasheq ([name (in-list
                        (runtime-owned-program-constant-global-names
                         runtime-source-index
                         program-index-before-runtime-owned-constant-prune))])
      (values name #t)))
  (define program-forms-without-runtime-owned-constant-globals
    (if (zero? (hash-count runtime-owned-program-constant-global-name-set))
        program-forms-before-runtime-owned-constant-prune
        (for/list ([form (in-list
                          program-forms-before-runtime-owned-constant-prune)]
                   #:unless (let ([name (constant-global-definition-name form)])
                               (and name
                                    (hash-ref
                                     runtime-owned-program-constant-global-name-set
                                     name
                                     #f)))
                   #:do [(define rewritten
                           (remove-core-owned-constant-global-sets
                            form
                            runtime-owned-program-constant-global-name-set))]
                   #:when rewritten)
          rewritten)))
  (define program-forms
    (prune-unreferenced-constant-forms
     program-forms-without-runtime-owned-constant-globals))
  (define sidecar-program-forms
    (remove-sidecar-owned-forms program-forms ffi-sidecar-forms))
  (define program-index
    (build-split-form-indexes sidecar-program-forms))
  (define program-owned-prefix-forms
    (split-form-indexes-shared-prefix-forms program-index))
  (define program-body-forms
    (for/list ([form (in-list sidecar-program-forms)]
               #:unless (shared-program-prefix-form? form))
      form))
  (define pulled-runtime-data-forms
    (sorted-named-forms
     (runtime-referenced-program-data-forms/from-refs runtime-symbol-refs
                                                      initial-program-index
                                                      #:runtime-index
                                                      runtime-source-index)
     module-data-name))
  (define pulled-runtime-data-names
    (for/list ([form (in-list pulled-runtime-data-forms)]
               #:do [(define name (module-data-name form))]
               #:when name)
      name))
  (define effective-core-cache-key
    (and core-cache-key
         (null? pulled-runtime-data-forms)
         (null? pulled-runtime-global-forms)
         core-cache-key))
  (record-split-runtime-boundary-blocker-metrics!
   pulled-runtime-global-names
   pulled-runtime-data-names
   (and effective-core-cache-key #t))
  (define runtime-body-forms-cache #f)
  (define (runtime-body-forms!)
    (unless runtime-forms
      (error 'split-classified-runtime-forms
             "runtime forms required to materialize split runtime module"))
    (or runtime-body-forms-cache
        (let ([body-forms
               (canonicalize-runtime-constant-declarations
                (append
                 (for/list ([form (in-list runtime-forms)]
                            #:unless (declare-elem-form? form))
                   form)
                 pulled-runtime-data-forms
                 pulled-runtime-global-forms))])
          (set! runtime-body-forms-cache body-forms)
          body-forms)))
  (define runtime-index
    (if effective-core-cache-key
        runtime-source-index
        (build-split-form-indexes (runtime-body-forms!))))
  (define ffi-sidecar-all-forms
    (append ffi-sidecar-forms ffi-sidecar-extra-forms))
  (define ffi-sidecar?
    (pair? ffi-sidecar-all-forms))
  (define ffi-sidecar-index
    (and ffi-sidecar?
         (build-split-form-indexes ffi-sidecar-all-forms)))
  (define ffi-sidecar-import-forms
    (if ffi-sidecar?
        (for/list ([name (in-list ffi-sidecar-function-names)])
          (define form
            (hash-ref (split-form-indexes-function-defs ffi-sidecar-index)
                      name
                      #f))
          (unless form
            (error 'split-classified-runtime-forms
                   "FFI sidecar function missing from sidecar forms: ~a"
                   name))
          (runtime-function-import-form form #:module ffi-import-module-name))
        '()))
  (define-values (imported-names import-forms export-forms program-data-forms)
    (if effective-core-cache-key
        (classified-runtime-import/export+data-forms
         runtime-index
         program-body-forms
         program-index
         #:module import-module-name
         #:export-all? #t
         #:build-export-forms? #f)
        (let-values ([(imported-names import-forms export-forms)
                      (classified-runtime-import/export-forms
                       runtime-forms
                       program-body-forms
                       #:module import-module-name
                       #:export-all? #t
                       #:build-export-forms? #t
                       #:runtime-index runtime-index)])
          (values
           imported-names
           import-forms
           export-forms
           (program-referenced-runtime-data-forms
            (runtime-body-forms!)
            program-body-forms
            #:runtime-index runtime-index
            #:program-index program-index)))))
  (define runtime-function-names
    (split-form-indexes-function-names runtime-index))
  (define program-available-function-names
    (for/fold ([names (hash-copy (split-form-indexes-function-names
                                  program-index))])
              ([form (in-list (append import-forms ffi-sidecar-import-forms))]
               #:do [(define name (module-func-name form))]
               #:when name)
      (hash-set! names name #t)
      names))
  (define declare-elem-forms
    (split-form-indexes-declare-elem-forms runtime-source-index))
  (define program-elem-forms
    (reverse
     (declare-elem-forms-for-names declare-elem-forms
                                   program-available-function-names)))
  (define program-supplemental-elem-forms
    (supplemental-declare-elem-forms
     program-elem-forms
     (direct-ref-func-names sidecar-program-forms
                            program-available-function-names)))
  (define program-prefix-forms
    (append-unique-forms
     (split-form-indexes-shared-prefix-forms runtime-index)
     program-owned-prefix-forms))
  (define final-program-forms
    (append program-prefix-forms
            import-forms
            ffi-sidecar-import-forms
            program-data-forms
            program-body-forms
            program-elem-forms
            program-supplemental-elem-forms))
  (record-split-program-metrics! final-program-forms
                                 imported-names
                                 import-forms)
  (define (runtime-core-module)
    (define runtime-body-forms
      (runtime-body-forms!))
    (define runtime-elem-forms
      (reverse
       (declare-elem-forms-for-names declare-elem-forms
                                     runtime-function-names)))
    (define runtime-supplemental-elem-forms
      (supplemental-declare-elem-forms
       runtime-elem-forms
       (direct-ref-func-names runtime-body-forms runtime-function-names)))
    `(module
       ,@runtime-body-forms
       ,@runtime-elem-forms
       ,@runtime-supplemental-elem-forms
       ,@(or export-forms
             (runtime-index-export-forms runtime-index))))
  (define (ffi-sidecar-module)
    (and ffi-sidecar?
         (let ()
           (define ffi-prefix-forms
             (append-unique-forms
              (split-form-indexes-shared-prefix-forms runtime-index)
              (split-form-indexes-shared-prefix-forms ffi-sidecar-index)))
           (define ffi-body-forms
             (for/list ([form (in-list ffi-sidecar-all-forms)]
                        #:unless (shared-program-prefix-form? form))
               form))
           (define-values (_ffi-imported-names
                           raw-ffi-core-import-forms
                           _ffi-export-forms
                           ffi-data-forms)
             (classified-runtime-import/export+data-forms
              runtime-index
              ffi-body-forms
              ffi-sidecar-index
              #:module import-module-name
              #:export-all? #t
              #:build-export-forms? #f))
           (define ffi-core-import-forms
             (for/list ([form (in-list raw-ffi-core-import-forms)]
                        #:unless (sidecar-owned-import-form?
                                  form
                                  ffi-sidecar-index))
               form))
           (define ffi-available-function-names
             (for/fold ([names (hash-copy
                                (split-form-indexes-function-names
                                 ffi-sidecar-index))])
                       ([form (in-list ffi-core-import-forms)]
                        #:do [(define name (module-func-name form))]
                        #:when name)
               (hash-set! names name #t)
               names))
           (define ffi-elem-forms
             (reverse
              (declare-elem-forms-for-names declare-elem-forms
                                            ffi-available-function-names)))
           (define ffi-supplemental-elem-forms
             (supplemental-declare-elem-forms
              ffi-elem-forms
              (direct-ref-func-names ffi-body-forms
                                     ffi-available-function-names)))
           `(module
              ,@ffi-prefix-forms
              ,@ffi-core-import-forms
              ,@ffi-data-forms
              ,@ffi-body-forms
              ,@ffi-elem-forms
              ,@ffi-supplemental-elem-forms
              ,@(map runtime-function-export-form
                     ffi-sidecar-function-names)))))
  (split-runtime-modules
   (if effective-core-cache-key
       (delay (runtime-core-module))
       (runtime-core-module))
   `(module ,@final-program-forms)
   imported-names
   import-forms
   effective-core-cache-key
   (ffi-sidecar-module)
   (and ffi-sidecar? ffi-cache-key)))

;; split-classified-runtime-module :
;;   datum? #:program-function-names (listof symbol?) #:result-global symbol?
;;          #:top-level-variable-declarations (listof datum?)
;;          #:string-constants list? #:bytes-constants list? #:symbol-constants list?
;;          [#:extra-program-function-names list?] [#:extra-program-global-names list?]
;;          [#:import-module string?] -> split-runtime-modules?
;;   Split a generated module into runtime and program modules using runtime metadata.
(define (split-classified-runtime-module
         module
         #:program-function-names program-function-names
         #:result-global result
         #:top-level-variable-declarations top-level-variable-declarations
         #:string-constants string-constants
         #:bytes-constants bytes-constants
         #:symbol-constants symbol-constants
         #:extra-program-function-names [extra-program-function-names '()]
         #:extra-program-global-names [extra-program-global-names '()]
         #:import-module [import-module-name "core"]
         #:core-cache-key [core-cache-key #f])
  (define-values (runtime-forms/reversed program-forms/reversed)
    (split-module-forms
     module
     #:program-function-names program-function-names
     #:result-global result
     #:top-level-variable-declarations top-level-variable-declarations
     #:string-constants string-constants
     #:bytes-constants bytes-constants
     #:symbol-constants symbol-constants
     #:extra-program-function-names extra-program-function-names
     #:extra-program-global-names extra-program-global-names))
  (split-classified-runtime-forms
   (reverse runtime-forms/reversed)
   (reverse program-forms/reversed)
   #:import-module import-module-name
   #:core-cache-key core-cache-key))

(module+ test
  (require rackunit
           racket/system
           (only-in "../assembler.rkt" wat->wasm))

  (define sample-helpers
    (list
     (core-runtime-helper
      '$sample-add
      "$sample-add"
      (lambda ()
        '(func $sample-add
               (param $x i32)
               (param $y i32)
               (result i32)
               (i32.add (local.get $x) (local.get $y)))))
     (core-runtime-helper
      '$sample-id
      "$sample-id"
      (lambda ()
        '(func $sample-id
               (type $SampleId)
               (param $x i32)
               (result i32)
               (local.get $x))))))

  (check-equal?
   (core-runtime-helper-import-form (first sample-helpers))
   '(func $sample-add
          (import "core" "$sample-add")
          (param $x i32)
          (param $y i32)
          (result i32)))
  (check-equal?
   (core-runtime-helper-import-form (second sample-helpers))
   '(func $sample-id
          (import "core" "$sample-id")
          (type $SampleId)))
  (check-equal?
   (append-unique-forms
    '((type $T (func))
      (func $host (import "env" "host")))
    '((func $host (import "env" "host"))
      (func $other (import "env" "other"))))
   '((type $T (func))
     (func $host (import "env" "host"))
     (func $other (import "env" "other"))))

  (check-equal?
   (strip-split-runtime-program-entry-init
    '(module
      (func $entry
            (export "entry")
            (result i32)
            (call $initialize-split-runtime-before-constants!)
            (call $initialize-split-runtime-constants!)
            (global.set $string:program (call $make-program-string))
            (call $initialize-split-runtime-after-constants!)
            (drop (call $reset-current-input-port!))
            (drop (call $reset-current-output-port!))
            (drop (call $reset-current-error-port!))
            (call $initialize-unsafe-ffi-primitive-table)
            (call $initialize-split-runtime-after-ffi!)
            (block $entry-done (br $entry-done))
            (i32.const 0))))
   '(module
     (func $entry
           (export "entry")
           (result i32)
           (global.set $string:program (call $make-program-string))
           (block $entry-done (br $entry-done))
           (i32.const 0))))

  ;; Loader-owned initialization now lives in the generated bootstrap helper.
  ;; Keep program constants and the entry-to-bootstrap call in their order.
  (check-equal?
   (strip-split-runtime-program-entry-init
    '(module
      (func $entry-bootstrap
            (call $initialize-split-runtime-before-constants!)
            (call $initialize-split-runtime-constants!)
            (global.set $string:program (call $make-program-string))
            (call $initialize-split-runtime-after-constants!)
            (drop (call $reset-current-input-port!))
            (drop (call $reset-current-output-port!))
            (drop (call $reset-current-error-port!))
            (call $initialize-unsafe-ffi-primitive-table)
            (call $initialize-split-runtime-after-ffi!)
            (call $initialize-program-namespace!))
      (func $entry (export "entry") (result i32)
            (call $entry-bootstrap)
            (block $entry-done (br $entry-done))
            (i32.const 0))))
   '(module
     (func $entry-bootstrap
           (global.set $string:program (call $make-program-string))
           (call $initialize-program-namespace!))
     (func $entry (export "entry") (result i32)
           (call $entry-bootstrap)
           (block $entry-done (br $entry-done))
           (i32.const 0))))

  (parameterize ([current-runtime-metric-rows '()])
    (record-split-runtime-boundary-blocker-metrics!
     '($program:global-b $program:global-a)
     '($program:data-a)
     #f)
    (define rows (current-runtime-metric-rows))
    (check-equal?
     (second (assoc "runtime:split-stable-core-cache-key-eligible?" rows))
     "no")
    (check-equal?
     (second (assoc "runtime:split-runtime-pulled-program-global-count" rows))
     "2")
    (check-equal?
     (second (assoc "runtime:split-runtime-pulled-program-global-top-names" rows))
     "$program:global-a, $program:global-b"))

  (define sample-full-module
    `(module
       (type $SampleId (func (param i32) (result i32)))
       ,@(core-runtime-helper-forms sample-helpers)
       (func $probe (export "probe") (result i32)
             (call $sample-add
                   (call $sample-id (i32.const 10))
                   (i32.const 32)))))

  (define sample-split
    (split-core-runtime-module
     sample-full-module
     #:helpers sample-helpers
     #:core-module '(module)))
  (define sample-program-forms
    (module-forms (split-runtime-modules-program sample-split)))
  (check-equal? (split-runtime-modules-imported-names sample-split)
                '($sample-add $sample-id))
  (check-false
   (for/or ([form (in-list sample-program-forms)])
     (and (function-definition-form? form)
          (member (module-func-name form) '($sample-add $sample-id)))))
  (check-equal?
   (for/list ([form (in-list sample-program-forms)]
              #:when (function-import-form? form))
     (module-func-name form))
   '($sample-add $sample-id))

  (define sample-reference-only-module
    `(module
       (type $SampleId (func (param i32) (result i32)))
       (func $probe (export "probe") (result i32)
             (call $sample-add
                   (i32.const 20)
                   (i32.const 22)))))
  (define sample-reference-only-split
    (split-core-runtime-module
     sample-reference-only-module
     #:helpers sample-helpers
     #:require-all? #f
     #:core-module '(module)))
  (define sample-reference-only-program-forms
    (module-forms (split-runtime-modules-program sample-reference-only-split)))
  (check-equal? (split-runtime-modules-imported-names sample-reference-only-split)
                '($sample-add))
  (check-equal?
   (for/list ([form (in-list sample-reference-only-program-forms)]
              #:when (function-import-form? form))
     (module-func-name form))
   '($sample-add))

  (define sample-lazy-core-split
    (split-core-runtime-module
     sample-full-module
     #:helpers sample-helpers
     #:core-cache-key '(stable-core-key)))
  (check-true (promise? (split-runtime-modules-core sample-lazy-core-split)))
  (check-equal? (split-runtime-modules-core-cache-key sample-lazy-core-split)
                '(stable-core-key))

  (define sample-pruned-module
    `(module
       (type $SampleId (func (param i32) (result i32)))
       ,(core-runtime-helper->form (first sample-helpers))
       (func $probe (export "probe") (result i32)
             (call $sample-add (i32.const 10) (i32.const 32)))))
  (define sample-pruned-split
    (split-core-runtime-module
     sample-pruned-module
     #:helpers sample-helpers
     #:require-all? #f
     #:core-module '(module)))
  (check-equal? (split-runtime-modules-imported-names sample-pruned-split)
                '($sample-add))

  (define boundary-runtime-forms
    '((tag $boundary-exn (param i32))
      (global $runtime-state (mut i32) (i32.const 40))
      (func $runtime-add-state
            (param $x i32)
            (result i32)
            (i32.add (local.get $x) (global.get $runtime-state)))
      (func $unused-runtime-helper
            (result i32)
            (i32.const 0))))

  (define boundary-program-forms
    '((func $probe
            (export "probe")
            (result i32)
            (if (i32.const 0)
                (then
                 (throw $boundary-exn (i32.const 5))))
            (global.set $runtime-state (i32.const 2))
            (call $runtime-add-state (i32.const 40)))))

  (define-values (boundary-imported-names
                  boundary-import-forms
                  boundary-export-forms)
    (classified-runtime-import/export-forms
     boundary-runtime-forms
     boundary-program-forms))
  (define boundary-runtime-index
    (build-split-form-indexes boundary-runtime-forms))
  (check-equal?
   (call-with-values
    (lambda ()
      (classified-runtime-import/export-forms
       boundary-runtime-forms
       boundary-program-forms
       #:runtime-index boundary-runtime-index))
    list)
   (list boundary-imported-names
         boundary-import-forms
         boundary-export-forms))

  (check-equal? boundary-imported-names
                '($runtime-add-state $runtime-state $boundary-exn))
  (check-equal?
   boundary-import-forms
   '((func $runtime-add-state
           (import "core" "$runtime-add-state")
           (param $x i32)
           (result i32))
     (global $runtime-state
             (import "core" "global:$runtime-state")
             (mut i32))
     (tag $boundary-exn
          (import "core" "tag:$boundary-exn")
          (param i32))))
  (check-equal?
   boundary-export-forms
   '((export "$runtime-add-state" (func $runtime-add-state))
     (export "global:$runtime-state" (global $runtime-state))
     (export "tag:$boundary-exn" (tag $boundary-exn))))

  (define classified-sample-module
    '(module
      (type $Probe (func (result i32)))
      (type $I8Array (array (mut i8)))
      (import "env" "memory" (memory $memory 1))
      (data $string-data:runtime "x")
      (data $symbol-data:check-list "check-list")
      (tag $boundary-exn (param i32))
      (global $runtime-state (mut i32) (i32.const 40))
      (global $symbol:check-list (mut (ref eq)) (ref.i31 (i32.const 0)))
      (func $runtime-add-state
            (param $x i32)
            (result i32)
            (i32.add (local.get $x) (global.get $runtime-state)))
      (func $runtime-symbol-value
            (result i32)
            (drop
             (array.new_data $I8Array
                             $symbol-data:check-list
                             (i32.const 0)
                             (i32.const 1)))
            (i31.get_s
             (ref.cast (ref i31)
                       (global.get $symbol:check-list))))
      (global $result (mut i32) (i32.const 0))
      (func $probe
            (export "probe")
            (result i32)
            (if (i32.const 0)
                (then
                 (throw $boundary-exn (i32.const 5))))
            (drop
             (array.new_data $I8Array
                             $string-data:runtime
                             (i32.const 0)
                             (i32.const 1)))
            (global.set $runtime-state (i32.const 2))
            (global.set $symbol:check-list (ref.i31 (i32.const 84)))
            (drop (ref.func $runtime-symbol-value))
            (i32.add
             (call $runtime-add-state (i32.const 40))
             (i32.sub
              (call $runtime-symbol-value)
              (i32.const 84))))
      (elem declare funcref
            (ref.func $runtime-add-state)
            (ref.func $probe))))

  (define classified-split
    (split-classified-runtime-module
     classified-sample-module
     #:program-function-names '($probe)
     #:result-global '$result
     #:top-level-variable-declarations
     '((global $result (mut i32) (i32.const 0)))
     #:string-constants '()
     #:bytes-constants '()
     #:symbol-constants '((check-list check-list))))
  (define classified-core-forms
    (module-forms (split-runtime-modules-core-module classified-split)))
  (define classified-program-forms
    (module-forms (split-runtime-modules-program classified-split)))
  (define-values (classified-runtime-forms/reversed
                  classified-program-forms/reversed)
    (split-module-forms
     classified-sample-module
     #:program-function-names '($probe)
     #:result-global '$result
     #:top-level-variable-declarations
     '((global $result (mut i32) (i32.const 0)))
     #:string-constants '()
     #:bytes-constants '()
     #:symbol-constants '((check-list check-list))))
  (check-equal?
   (split-classified-runtime-forms
    (reverse classified-runtime-forms/reversed)
    (reverse classified-program-forms/reversed))
   classified-split)

  (define ffi-sidecar-runtime-forms
    '((type $Prim0 (func (result (ref eq))))
      (func $runtime-helper
            (result i32)
            (i32.const 42))))
  (define ffi-sidecar-owned-forms
    '((func $js-undefined/imported
            (import "standard" "undefined")
            (result externref))
      (func $js-undefined
            (type $Prim0)
            (result (ref eq))
            (drop (call $js-undefined/imported))
            (drop (call $runtime-helper))
            (ref.i31 (i32.const 3)))))
  (define ffi-sidecar-extra-forms
    '((global $prim:js-undefined (mut i32) (i32.const 0))
      (func $initialize-split-ffi-sidecar-primitives!
            (global.set $prim:js-undefined (i32.const 1)))))
  (define ffi-sidecar-program-forms
    `((type $Prim0 (func (result (ref eq))))
      ,@ffi-sidecar-owned-forms
      (global $prim:js-undefined (mut i32) (i32.const 0))
      (func $entry
            (result (ref eq))
            (call $initialize-split-ffi-sidecar-primitives!)
            (call $js-undefined))))
	  (define ffi-sidecar-split
	    (split-classified-runtime-forms
	     ffi-sidecar-runtime-forms
	     ffi-sidecar-program-forms
	     #:ffi-sidecar-forms ffi-sidecar-owned-forms
	     #:ffi-sidecar-extra-forms ffi-sidecar-extra-forms
	     #:ffi-sidecar-function-names
             '($js-undefined $initialize-split-ffi-sidecar-primitives!)
	     #:ffi-cache-key '(ffi-cache-key)))
  (define ffi-sidecar-program-module-forms
    (module-forms (split-runtime-modules-program ffi-sidecar-split)))
	  (define ffi-sidecar-module-forms
	    (module-forms (split-runtime-modules-ffi-module ffi-sidecar-split)))
	  (check-not-false (split-runtime-modules-ffi-module ffi-sidecar-split))
	  (check-equal? (split-runtime-modules-ffi-cache-key ffi-sidecar-split)
	                '(ffi-cache-key))
  (check-not-false
   (member '(func $js-undefined
                  (import "ffi" "$js-undefined")
                  (type $Prim0))
           ffi-sidecar-program-module-forms))
  (check-not-false
   (member '(func $initialize-split-ffi-sidecar-primitives!
                  (import "ffi" "$initialize-split-ffi-sidecar-primitives!"))
           ffi-sidecar-program-module-forms))
  (check-not-false
   (member '(global $prim:js-undefined (mut i32) (i32.const 0))
           ffi-sidecar-program-module-forms))
  (check-false
   (member '(func $js-undefined/imported
                  (import "standard" "undefined")
                  (result externref))
           ffi-sidecar-program-module-forms))
  (check-false
   (for/or ([form (in-list ffi-sidecar-program-module-forms)])
     (and (equal? (module-func-name form) '$js-undefined)
          (not (function-import-form? form)))))
  (check-not-false
   (member '(func $runtime-helper
                  (import "core" "$runtime-helper")
                  (result i32))
           ffi-sidecar-module-forms))
  (check-not-false
   (member '(export "$js-undefined" (func $js-undefined))
           ffi-sidecar-module-forms))
  (check-not-false
   (member '(func $js-undefined/imported
                  (import "standard" "undefined")
                  (result externref))
           ffi-sidecar-module-forms))
  (check-not-false
   (member '(global $prim:js-undefined (mut i32) (i32.const 0))
           ffi-sidecar-module-forms))
  (check-false
   (member '(global $prim:js-undefined
                    (import "core" "global:$prim:js-undefined")
                    (mut i32))
           ffi-sidecar-module-forms))
  (check-not-false
   (member '(export "$initialize-split-ffi-sidecar-primitives!"
                   (func $initialize-split-ffi-sidecar-primitives!))
           ffi-sidecar-module-forms))

  (check-false (split-runtime-modules-core-cache-key classified-split))
  (check-equal?
   (split-runtime-modules-core-cache-key
    (split-classified-runtime-forms
     '((func $runtime-helper))
     '((func $program)
       (func $entry))
     #:core-cache-key '(stable-core-key)))
   '(stable-core-key))
  (define lazy-core-split
    (split-classified-runtime-forms
     '((func $runtime-helper))
     '((func $program)
       (func $entry))
     #:core-cache-key '(stable-core-key)))
  (check-true (promise? (split-runtime-modules-core lazy-core-split)))
  (check-equal? (car (split-runtime-modules-core-module lazy-core-split))
                'module)
  (check-equal?
   (runtime-symbol-reference-names
    '((func $runtime-helper
            (param $local-fn (ref func))
            (local $runtime-helper i32)
            (local.get $runtime-helper)
            (call $called-helper)
            (return_call $tail-helper)
            (ref.func $funcref-helper)
            (global.get $global-helper)
            (global.set $set-global-helper (i32.const 1))
            (throw $exn-tag (i32.const 2))
            (try (do (i32.const 3))
                 (catch $caught-tag $handler-block))
            (array.new_data $I8Array $data-helper (i32.const 0) (i32.const 1)))))
   '($called-helper
     $caught-tag
     $data-helper
     $exn-tag
     $funcref-helper
     $global-helper
     $set-global-helper
     $tail-helper))
  (define split-boundary-cache-test-root
    (let ([tmp-root (if (directory-exists? "/tmp")
                        "/tmp"
                        (find-system-path 'temp-dir))])
      (make-temporary-file "webracket-split-boundary-cache-~a"
                           'directory
                           tmp-root)))
  (define (split-boundary-cache-file-count root)
    (define cache-dir (build-path root "split-runtime-boundary-v2"))
    (if (directory-exists? cache-dir)
        (length (directory-list cache-dir))
        0))
  (parameterize ([current-runtime-cache-dir split-boundary-cache-test-root])
    (define cached-boundary-runtime-forms
      (list (list 'data '$runtime-data (wasm-data #"runtime"))
            (list 'func #'$runtime-helper
                  '(param $x i32)
                  '(result i32)
                  '(local.get $x))))
    (define cached-boundary-program-forms
      '((func $program
              (result i32)
              (call $runtime-helper (i32.const 42)))
        (func $entry)))
    (define cached-boundary-core-cache-key
      (list 'stable-core-key (string-copy "/app/")))
    (define cached-boundary-split
      (split-classified-runtime-forms
       cached-boundary-runtime-forms
       cached-boundary-program-forms
       #:core-cache-key cached-boundary-core-cache-key))
    (check-equal? (split-boundary-cache-file-count split-boundary-cache-test-root)
                  1)
    (define cached-boundary-metadata
      (read-split-runtime-boundary-metadata/cache
       cached-boundary-core-cache-key))
    (check-true (split-runtime-boundary-metadata? cached-boundary-metadata))
    (check-true
     (hash-has-key?
      (split-form-indexes-function-defs
       (split-runtime-boundary-metadata-runtime-index cached-boundary-metadata))
      '$runtime-helper))
    (define cached-boundary-split-again
      (split-classified-runtime-forms
       cached-boundary-runtime-forms
       cached-boundary-program-forms
       #:core-cache-key cached-boundary-core-cache-key
       #:boundary-metadata cached-boundary-metadata))
    (check-true (promise? (split-runtime-modules-core cached-boundary-split-again)))
    (check-equal? (split-runtime-modules-core-module cached-boundary-split-again)
                  (split-runtime-modules-core-module cached-boundary-split))
    (check-equal? (split-runtime-modules-program cached-boundary-split-again)
                  (split-runtime-modules-program cached-boundary-split))
    (check-equal? (split-runtime-modules-imported-names cached-boundary-split-again)
                  (split-runtime-modules-imported-names cached-boundary-split))
    (check-equal? (split-runtime-modules-core-cache-key cached-boundary-split-again)
                  cached-boundary-core-cache-key)
    (define-values (cached-boundary-compatible?
                    cached-boundary-program-index)
      (split-runtime-reference-compatible+index cached-boundary-metadata
                                                cached-boundary-program-forms))
    (check-true cached-boundary-compatible?)
    (define cached-boundary-program-only-split
      (split-classified-runtime-forms
       #f
       cached-boundary-program-forms
       #:core-cache-key cached-boundary-core-cache-key
       #:boundary-metadata cached-boundary-metadata
       #:initial-program-index cached-boundary-program-index))
    (check-true
     (split-runtime-reference-compatible? cached-boundary-metadata
                                          cached-boundary-program-forms))
    (check-true (promise? (split-runtime-modules-core cached-boundary-program-only-split)))
    (check-equal? (split-runtime-modules-program cached-boundary-program-only-split)
                  (split-runtime-modules-program cached-boundary-split))
    (check-equal? (split-runtime-modules-imported-names
                   cached-boundary-program-only-split)
                  (split-runtime-modules-imported-names cached-boundary-split))
    (check-equal? (split-runtime-modules-core-cache-key
                   cached-boundary-program-only-split)
                  cached-boundary-core-cache-key))
  (define stale-boundary-runtime-forms
    '((func $runtime-helper
            (result i32)
            (i32.const 42))))
  (define stale-boundary-metadata
    (build-split-runtime-boundary-metadata
     (append stale-boundary-runtime-forms
             '((func $stale-program-helper
                     (result i32)
                     (i32.const 0))))))
  (define stale-boundary-split
    (split-classified-runtime-forms
     stale-boundary-runtime-forms
     '((func $entry
             (result i32)
             (i32.const 0)))
     #:core-cache-key '(stale-boundary-core-key)
     #:boundary-metadata stale-boundary-metadata))
  (define stale-boundary-core-forms
    (module-forms (split-runtime-modules-core-module stale-boundary-split)))
  (check-false
   (member '(export "$stale-program-helper" (func $stale-program-helper))
           stale-boundary-core-forms))
  (check-false
   (for/or ([form (in-list stale-boundary-core-forms)])
     (equal? (module-func-name form) '$stale-program-helper)))
  (define pulled-constant-split
    (split-classified-runtime-forms
     '((type $I8Array (array (mut i8)))
       (func $runtime-symbol
             (drop
              (array.new_data $I8Array
                              $symbol-data:program-symbol
                              (i32.const 0)
                              (i32.const 1)))
             (drop (global.get $symbol:program-symbol))))
     '((data $symbol-data:program-symbol "program-symbol")
       (global $symbol:program-symbol (mut (ref eq)) (ref.i31 (i32.const 0)))
       (func $program)
       (func $entry))
     #:core-cache-key '(stable-core-key)))
  (define pulled-constant-boundary-metadata
    (build-split-runtime-boundary-metadata
     '((type $I8Array (array (mut i8)))
       (func $runtime-symbol
             (drop
              (array.new_data $I8Array
                              $symbol-data:program-symbol
                              (i32.const 0)
                              (i32.const 1)))
             (drop (global.get $symbol:program-symbol))))))
  (check-false
   (split-runtime-reference-compatible?
    pulled-constant-boundary-metadata
    '((data $symbol-data:program-symbol "program-symbol")
      (global $symbol:program-symbol (mut (ref eq)) (ref.i31 (i32.const 0)))
      (func $program)
      (func $entry))))
  (check-false (split-runtime-modules-core-cache-key pulled-constant-split))
  (check-false (promise? (split-runtime-modules-core pulled-constant-split)))
  (define duplicate-constant-runtime-forms
    '((type $I8Array (array (mut i8)))
      (data $symbol-data:shared-symbol "shared-symbol")
      (global $symbol:shared-symbol (mut (ref eq)) (ref.i31 (i32.const 0)))
      (func $runtime-symbol
            (drop
             (array.new_data $I8Array
                             $symbol-data:shared-symbol
                             (i32.const 0)
                             (i32.const 1)))
            (drop (global.get $symbol:shared-symbol)))))
  (define duplicate-constant-program-forms
    '((data $symbol-data:shared-symbol "shared-symbol")
      (global $symbol:shared-symbol (mut (ref eq)) (ref.i31 (i32.const 0)))
      (func $program
            (result i32)
            (global.get $symbol:shared-symbol)
            (drop)
            (i32.const 42))
      (func $entry)))
  (define duplicate-constant-boundary-metadata
    (build-split-runtime-boundary-metadata duplicate-constant-runtime-forms))
  (check-true
   (split-runtime-reference-compatible?
    duplicate-constant-boundary-metadata
    duplicate-constant-program-forms))
  (define duplicate-constant-program-only-split
    (split-classified-runtime-forms
     #f
     duplicate-constant-program-forms
     #:core-cache-key '(stable-core-key)
     #:boundary-metadata duplicate-constant-boundary-metadata))
  (define duplicate-constant-program-module
    (split-runtime-modules-program duplicate-constant-program-only-split))
  (check-true (promise? (split-runtime-modules-core
                         duplicate-constant-program-only-split)))
  (check-equal? (split-runtime-modules-core-cache-key
                 duplicate-constant-program-only-split)
                '(stable-core-key))
  (check-not-false
   (member '(global $symbol:shared-symbol
                    (import "core" "global:$symbol:shared-symbol")
                    (mut (ref eq)))
           (module-forms duplicate-constant-program-module)))
  (check-false
   (for/or ([form (in-list (module-forms duplicate-constant-program-module))])
     (and (global-definition-form? form)
          (eq? (module-global-name form) '$symbol:shared-symbol))))

  (check-equal?
   (split-runtime-modules-imported-names classified-split)
   '($runtime-add-state
     $runtime-symbol-value
     $runtime-state
     $symbol:check-list
     $boundary-exn))
  (check-not-false
   (member '(import "env" "memory" (memory $memory 1))
           classified-core-forms))
  (check-not-false
   (member '(import "env" "memory" (memory $memory 1))
           classified-program-forms))
  (check-not-false
   (member '(data $string-data:runtime "x")
           classified-core-forms))
  (check-not-false
   (member '(data $string-data:runtime "x")
           classified-program-forms))
  (check-not-false
   (member '(data $symbol-data:check-list "check-list")
           classified-core-forms))
  (check-false
   (member '(data $symbol-data:check-list "check-list")
           classified-program-forms))
  (check-not-false
   (member '(elem declare funcref (ref.func $runtime-add-state))
           classified-core-forms))
  (check-not-false
   (member '(global $symbol:check-list
                    (mut (ref eq))
                    (ref.i31 (i32.const 0)))
           classified-core-forms))
  (check-not-false
   (member '(global $symbol:check-list
                    (import "core" "global:$symbol:check-list")
                    (mut (ref eq)))
           classified-program-forms))
  (check-false
   (for/or ([form (in-list classified-program-forms)])
     (and (global-definition-form? form)
          (eq? (module-global-name form) '$symbol:check-list))))
  (check-not-false
   (member '(elem declare funcref
                  (ref.func $runtime-add-state)
                  (ref.func $probe))
           classified-program-forms))
  (check-not-false
   (member '(elem declare funcref (ref.func $runtime-symbol-value))
           classified-program-forms))
  (check-false
   (for/or ([form (in-list classified-program-forms)])
     (and (function-definition-form? form)
          (eq? (module-func-name form) '$runtime-add-state))))

  (define direct-ffi-classified-module
    '(module
      (type $T (func (result i32)))
      (func $runtime-helper (result i32) (i32.const 41))
      (func $js-eval/imported (import "host" "js-eval") (result i32))
      (func $js-eval
            (result i32)
            (i32.add
             (call $runtime-helper)
             (call $js-eval/imported)))
      (global $prim:js-eval (mut (ref eq)) (ref.null eq))
      (global $result (mut i32) (i32.const 0))
      (func $entry (global.set $result (call $js-eval)))))
  (define direct-ffi-classified-split
    (split-classified-runtime-module
     direct-ffi-classified-module
     #:program-function-names '($entry)
     #:result-global '$result
     #:top-level-variable-declarations
     '((global $result (mut i32) (i32.const 0)))
     #:string-constants '()
     #:bytes-constants '()
     #:symbol-constants '()
     #:extra-program-function-names '($js-eval/imported $js-eval)
     #:extra-program-global-names '($prim:js-eval)))
  (define direct-ffi-core-forms
    (module-forms (split-runtime-modules-core-module direct-ffi-classified-split)))
  (define direct-ffi-program-forms
    (module-forms (split-runtime-modules-program direct-ffi-classified-split)))

  (check-equal? (split-runtime-modules-imported-names
                 direct-ffi-classified-split)
                '($runtime-helper))
  (check-not-false
   (member '(func $runtime-helper (result i32) (i32.const 41))
           direct-ffi-core-forms))
  (check-not-false
   (member '(func $js-eval/imported
                  (import "host" "js-eval")
                  (result i32))
           direct-ffi-program-forms))
  (check-not-false
   (member '(global $prim:js-eval (mut (ref eq)) (ref.null eq))
           direct-ffi-program-forms))
  (check-false
   (for/or ([form (in-list direct-ffi-core-forms)])
     (memq (or (module-func-name form) (module-global-name form))
           '($js-eval/imported $js-eval $prim:js-eval))))

  (define pullback-order-module
    '(module
      (type $I8Array (array (mut i8)))
      (data $symbol-data:c "c")
      (global $symbol:c (mut (ref eq)) (ref.i31 (i32.const 0)))
      (func $runtime-symbols
            (drop
             (array.new_data $I8Array
                             $symbol-data:a
                             (i32.const 0)
                             (i32.const 1)))
            (drop
             (array.new_data $I8Array
                             $symbol-data:b
                             (i32.const 0)
                             (i32.const 1)))
            (drop (global.get $symbol:a))
            (drop (global.get $symbol:b)))
      (data $symbol-data:b "b")
      (global $symbol:b (mut (ref eq)) (ref.i31 (i32.const 0)))
      (data $symbol-data:a "a")
      (global $symbol:a (mut (ref eq)) (ref.i31 (i32.const 0)))
      (global $result (mut i32) (i32.const 0))
      (func $probe (export "probe") (call $runtime-symbols))))
  (define pullback-order-split
    (split-classified-runtime-module
     pullback-order-module
     #:program-function-names '($probe)
     #:result-global '$result
     #:top-level-variable-declarations
     '((global $result (mut i32) (i32.const 0)))
     #:string-constants '()
     #:bytes-constants '()
     #:symbol-constants '((b b) (a a))))
  (define pullback-order-core-forms
    (module-forms (split-runtime-modules-core-module pullback-order-split)))
  (check-equal?
   (for/list ([form (in-list pullback-order-core-forms)]
              #:do [(define name (or (module-data-name form)
                                     (module-global-name form)))]
              #:when (memq name '($symbol-data:a
                                  $symbol-data:b
                                  $symbol-data:c
                                  $symbol:a
                                  $symbol:b
                                  $symbol:c)))
     name)
   '($symbol-data:a
     $symbol-data:b
     $symbol-data:c
     $symbol:a
     $symbol:b
     $symbol:c))

  (define core-link-probe-module
    `(module
       ,@(generate-shared-types)
       ,@(core-runtime-helper-forms core-runtime-helpers)
       (func (export "fx_fixnum_true") (result i32)
             (call $fx?/i32 (ref.i31 (i32.const 84))))
       (func (export "array_ref_tagged") (result i32)
             (i31.get_s
              (ref.cast (ref i31)
                        (call $array-ref
                              (call $make-array
                                    (i32.const 2)
                                    (ref.i31 (i32.const 40)))
                              (i32.const 1)))))))

  (define core-link-split (split-core-runtime-module core-link-probe-module))
  (define core-link-program (split-runtime-modules-program core-link-split))
  (check-equal? (length (split-runtime-modules-import-forms core-link-split))
                (length core-runtime-helpers))
  (for ([name (in-list (split-runtime-modules-imported-names core-link-split))])
    (check-false
     (for/or ([form (in-list (module-forms core-link-program))])
       (and (function-definition-form? form)
            (eq? (module-func-name form) name)))))

  ;; form-contains-global-set? : symbol? datum? -> boolean?
  ;;   Check whether a form writes to the given global.
  (define (form-contains-global-set? name form)
    (match form
      [`(global.set ,target . ,body)
       (or (eq? target name)
           (for/or ([part (in-list body)])
             (form-contains-global-set? name part)))]
      [(? list?)
       (for/or ([part (in-list form)])
         (form-contains-global-set? name part))]
      [_ #f]))

  (define constant-prune-helper
    (core-runtime-helper
     '$dead-constant-helper
     "$dead-constant-helper"
     (lambda ()
       '(func $dead-constant-helper
              (drop (global.get $string:dead-helper-only))))))
  (define constant-prune-module
    `(module
       (type $I8Array (array (mut i8)))
       (data $string-data:dead-helper-only "dead")
       (global $string:dead-helper-only (mut (ref eq)) (ref.i31 (i32.const 0)))
       (data $string-data:init-only "init-only")
       (global $string:init-only (mut (ref eq)) (ref.i31 (i32.const 0)))
       (data $string-data:live "live")
       (global $string:live (mut (ref eq)) (ref.i31 (i32.const 0)))
       ,(core-runtime-helper->form constant-prune-helper)
       (func $init-dead-constant
             (global.set $string:init-only
                         (array.new_data $I8Array
                                         $string-data:init-only
                                         (i32.const 0)
                                         (i32.const 9))))
       (func $init-live-constant
             (global.set $string:live
                         (array.new_data $I8Array
                                         $string-data:live
                                         (i32.const 0)
                                         (i32.const 4))))
       (func $entry
             (export "entry")
             (result i32)
             (call $init-dead-constant)
             (call $init-live-constant)
             (drop (global.get $string:live))
             (i32.const 42))))
  (define constant-prune-program-forms
    (module-forms
     (split-runtime-modules-program
      (split-core-runtime-module constant-prune-module
                                 #:helpers (list constant-prune-helper)))))
  (check-false (member '(data $string-data:dead-helper-only "dead")
                       constant-prune-program-forms))
  (check-false (member '(global $string:dead-helper-only
                                (mut (ref eq))
                                (ref.i31 (i32.const 0)))
                       constant-prune-program-forms))
  (check-false (member '(data $string-data:init-only "init-only")
                       constant-prune-program-forms))
  (check-false (member '(global $string:init-only
                                (mut (ref eq))
                                (ref.i31 (i32.const 0)))
                       constant-prune-program-forms))
  (check-false
   (for/or ([form (in-list constant-prune-program-forms)])
     (form-contains-global-set? '$string:init-only form)))
  (check-not-false (member '(data $string-data:live "live")
                           constant-prune-program-forms))
  (check-not-false (member '(global $string:live
                                    (mut (ref eq))
                                    (ref.i31 (i32.const 0)))
                           constant-prune-program-forms))
  (check-not-false
   (for/or ([form (in-list constant-prune-program-forms)])
     (form-contains-global-set? '$string:live form)))

  (define core-string-constant-import-split
    (split-core-runtime-module
     `(module
        (type $I8Array (array (mut i8)))
        (data $string-data:empty "")
        (global $string:empty (mut (ref eq)) (ref.i31 (i32.const 0)))
        (func $initialize-string-empty
              (global.set $string:empty
                          (array.new_data $I8Array
                                          $string-data:empty
                                          (i32.const 0)
                                          (i32.const 0))))
        (func $entry
              (export "entry")
              (result i32)
              (call $initialize-string-empty)
              (drop (global.get $string:empty))
              (i32.const 42)))
     #:helpers '()))
  (define core-string-constant-import-program-forms
    (module-forms (split-runtime-modules-program
                   core-string-constant-import-split)))
  (check-not-false
   (member '$string:empty
           (split-runtime-modules-imported-names
            core-string-constant-import-split)))
  (check-not-false
   (member '(global $string:empty
                    (import "core" "global:$string:empty")
                    (mut (ref eq)))
           core-string-constant-import-program-forms))
  (check-false
   (member '(data $string-data:empty "")
           core-string-constant-import-program-forms))
  (check-false
   (for/or ([form (in-list core-string-constant-import-program-forms)])
     (and (global-definition-form? form)
          (eq? (module-global-name form) '$string:empty))))
  (check-false
   (for/or ([form (in-list core-string-constant-import-program-forms)])
     (form-contains-global-set? '$string:empty form)))

  (define core-symbol-constant-import-split
    (split-core-runtime-module
     `(module
        (type $I8Array (array (mut i8)))
        (data $symbol-data:racket "racket")
        (global $symbol:racket (mut (ref eq)) (ref.i31 (i32.const 0)))
        (func $initialize-symbol-racket
              (global.set $symbol:racket
                          (array.new_data $I8Array
                                          $symbol-data:racket
                                          (i32.const 0)
                                          (i32.const 6))))
        (func $entry
              (export "entry")
              (result i32)
              (call $initialize-symbol-racket)
              (drop (global.get $symbol:racket))
              (i32.const 42)))
     #:helpers '()))
  (define core-symbol-constant-import-program-forms
    (module-forms (split-runtime-modules-program
                   core-symbol-constant-import-split)))
  (check-not-false
   (member '$symbol:racket
           (split-runtime-modules-imported-names
            core-symbol-constant-import-split)))
  (check-not-false
   (member '(global $symbol:racket
                    (import "core" "global:$symbol:racket")
                    (mut (ref eq)))
           core-symbol-constant-import-program-forms))
  (check-false
   (member '(data $symbol-data:racket "racket")
           core-symbol-constant-import-program-forms))
  (check-false
   (for/or ([form (in-list core-symbol-constant-import-program-forms)])
     (and (global-definition-form? form)
          (eq? (module-global-name form) '$symbol:racket))))
  (check-false
   (for/or ([form (in-list core-symbol-constant-import-program-forms)])
     (form-contains-global-set? '$symbol:racket form)))

  (define core-primitive-symbol-constant-import-split
    (split-core-runtime-module
     `(module
        (type $I8Array (array (mut i8)))
        (data $symbol-data:append "append")
        (global $symbol:append (mut (ref eq)) (ref.i31 (i32.const 0)))
        (func $initialize-symbol-append
              (global.set $symbol:append
                          (array.new_data $I8Array
                                          $symbol-data:append
                                          (i32.const 0)
                                          (i32.const 6))))
        (func $entry
              (export "entry")
              (result i32)
              (call $initialize-symbol-append)
              (drop (global.get $symbol:append))
              (i32.const 42)))
     #:helpers '()))
  (define core-primitive-symbol-constant-import-program-forms
    (module-forms (split-runtime-modules-program
                   core-primitive-symbol-constant-import-split)))
  (check-not-false
   (member '$symbol:append
           (split-runtime-modules-imported-names
            core-primitive-symbol-constant-import-split)))
  (check-not-false
   (member '(global $symbol:append
                    (import "core" "global:$symbol:append")
                    (mut (ref eq)))
           core-primitive-symbol-constant-import-program-forms))
  (check-false
   (member '(data $symbol-data:append "append")
           core-primitive-symbol-constant-import-program-forms))
  (check-false
   (for/or ([form (in-list core-primitive-symbol-constant-import-program-forms)])
     (and (global-definition-form? form)
          (eq? (module-global-name form) '$symbol:append))))
  (check-false
   (for/or ([form (in-list core-primitive-symbol-constant-import-program-forms)])
     (form-contains-global-set? '$symbol:append form)))

  (define core-memory-map-import-split
    (split-core-runtime-module
     `(module
        (global $memory-map:callback-buffer-base
                (mut i32)
                (i32.const 1052672))
        (global $memory-map:callback-buffer-length
                (mut i32)
                (i32.const 16777216))
        (func $entry
              (export "entry")
              (result i32)
              (i32.add
               (global.get $memory-map:callback-buffer-base)
               (global.get $memory-map:callback-buffer-length))))
     #:helpers '()))
  (define core-memory-map-import-program-forms
    (module-forms (split-runtime-modules-program
                   core-memory-map-import-split)))
  (for ([name (in-list '($memory-map:callback-buffer-base
                         $memory-map:callback-buffer-length))])
    (check-not-false
     (member name
             (split-runtime-modules-imported-names
              core-memory-map-import-split)))
    (check-not-false
     (member `(global ,name
                      (import "core" ,(runtime-global-export-name name))
                      (mut i32))
             core-memory-map-import-program-forms))
    (check-false
     (for/or ([form (in-list core-memory-map-import-program-forms)])
       (and (global-definition-form? form)
            (eq? (module-global-name form) name)))))

  (define core-struct-property-import-split
    (split-core-runtime-module
     `(module
        (global $prop:procedure (mut (ref eq)) (ref.i31 (i32.const 0)))
        (global $equal+hash-recur/equal (mut (ref eq)) (ref.i31 (i32.const 0)))
        (func $initialize-struct-properties
              (global.set $prop:procedure (ref.i31 (i32.const 1)))
              (global.set $equal+hash-recur/equal (ref.i31 (i32.const 2))))
        (func $entry
              (export "entry")
              (result (ref eq))
              (call $initialize-struct-properties)
              (global.get $prop:procedure)))
     #:helpers '()))
  (define core-struct-property-import-program-forms
    (module-forms (split-runtime-modules-program
                   core-struct-property-import-split)))
  (check-not-false
   (member '$prop:procedure
           (split-runtime-modules-imported-names
            core-struct-property-import-split)))
  (check-not-false
   (member '(global $prop:procedure
                    (import "core" "global:$prop:procedure")
                    (mut (ref eq)))
           core-struct-property-import-program-forms))
  (check-false
   (for/or ([form (in-list core-struct-property-import-program-forms)])
     (and (global-definition-form? form)
          (eq? (module-global-name form) '$prop:procedure))))
  (check-false
   (for/or ([form (in-list core-struct-property-import-program-forms)])
     (form-contains-global-set? '$prop:procedure form)))
  (check-not-false
   (member '$equal+hash-recur/equal
           (split-runtime-modules-imported-names
            core-struct-property-import-split)))
  (check-not-false
   (member '(global $equal+hash-recur/equal
                    (import "core" "global:$equal+hash-recur/equal")
                    (mut (ref eq)))
           core-struct-property-import-program-forms))
  (check-false
   (for/or ([form (in-list core-struct-property-import-program-forms)])
     (and (global-definition-form? form)
          (eq? (module-global-name form) '$equal+hash-recur/equal))))
  (check-not-false
   (for/or ([form (in-list core-struct-property-import-program-forms)])
     (form-contains-global-set? '$equal+hash-recur/equal form)))

  (define core-char-category-import-split
    (split-core-runtime-module
     `(module
        (global $char-general-category-symbols
                (mut (ref null $Array))
                (ref.null $Array))
        (func $initialize-char-category
              (global.set $char-general-category-symbols
                          (array.new_fixed $Array 1
                                           (ref.i31 (i32.const 2)))))
        (func $entry
              (export "entry")
              (result i32)
              (call $initialize-char-category)
              (ref.is_null (global.get $char-general-category-symbols))))
     #:helpers '()))
  (define core-char-category-import-program-forms
    (module-forms (split-runtime-modules-program
                   core-char-category-import-split)))
  (check-not-false
   (member '$char-general-category-symbols
           (split-runtime-modules-imported-names
            core-char-category-import-split)))
  (check-not-false
   (member `(global $char-general-category-symbols
                    (import "core" ,(runtime-global-export-name '$char-general-category-symbols))
                    (mut (ref null $Array)))
           core-char-category-import-program-forms))
  (check-false
   (for/or ([form (in-list core-char-category-import-program-forms)])
     (and (global-definition-form? form)
          (eq? (module-global-name form) '$char-general-category-symbols))))
  (check-false
   (for/or ([form (in-list core-char-category-import-program-forms)])
     (form-contains-global-set? '$char-general-category-symbols form)))

  (define core-type-cache-import-split
    (split-core-runtime-module
     `(module
        (global $arity-at-least-type
                (mut (ref null $StructType))
                (ref.null $StructType))
        (global $syntax-type
                (mut (ref null $StructType))
                (ref.null $StructType))
        (func $initialize-type-caches
              (global.set $arity-at-least-type (ref.null $StructType))
              (global.set $syntax-type (ref.null $StructType)))
        (func $entry
              (export "entry")
              (result i32)
              (call $initialize-type-caches)
              (ref.is_null (global.get $arity-at-least-type))))
     #:helpers '()))
  (define core-type-cache-import-program-forms
    (module-forms (split-runtime-modules-program
                   core-type-cache-import-split)))
  (for ([name (in-list '($arity-at-least-type $syntax-type))])
    (check-not-false
     (member name
             (split-runtime-modules-imported-names
              core-type-cache-import-split)))
    (check-not-false
     (member `(global ,name
                      (import "core" ,(runtime-global-export-name name))
                      (mut (ref null $StructType)))
             core-type-cache-import-program-forms))
    (check-false
     (for/or ([form (in-list core-type-cache-import-program-forms)])
       (and (global-definition-form? form)
            (eq? (module-global-name form) name))))
    (check-not-false
     (for/or ([form (in-list core-type-cache-import-program-forms)])
       (form-contains-global-set? name form))))

  (define core-state-cell-import-split
    (split-core-runtime-module
     `(module
        (global $current-output-port-value
                (mut (ref eq))
                (ref.i31 (i32.const 0)))
        (global $system-path-convention
                (mut (ref eq))
                (ref.i31 (i32.const 0)))
        (global $next-hash-state
                (mut i32)
                (i32.const 0x9e3779b9))
        (func $initialize-state-cells
              (global.set $current-output-port-value (ref.i31 (i32.const 2)))
              (global.set $system-path-convention (ref.i31 (i32.const 4)))
              (global.set $next-hash-state (i32.const 1)))
        (func $entry
              (export "entry")
              (result i32)
              (call $initialize-state-cells)
              (global.get $next-hash-state)))
     #:helpers '()))
  (define core-state-cell-import-program-forms
    (module-forms (split-runtime-modules-program
                   core-state-cell-import-split)))
  (for ([name (in-list '($current-output-port-value
                         $system-path-convention))])
    (check-not-false
     (member name
             (split-runtime-modules-imported-names
              core-state-cell-import-split)))
    (check-not-false
     (member `(global ,name
                      (import "core" ,(runtime-global-export-name name))
                      (mut (ref eq)))
             core-state-cell-import-program-forms))
    (check-false
     (for/or ([form (in-list core-state-cell-import-program-forms)])
       (and (global-definition-form? form)
            (eq? (module-global-name form) name))))
    (check-not-false
     (for/or ([form (in-list core-state-cell-import-program-forms)])
       (form-contains-global-set? name form))))
  (check-not-false
   (member '$next-hash-state
           (split-runtime-modules-imported-names
            core-state-cell-import-split)))
  (check-not-false
   (member `(global $next-hash-state
                    (import "core" ,(runtime-global-export-name '$next-hash-state))
                    (mut i32))
           core-state-cell-import-program-forms))
  (check-false
   (for/or ([form (in-list core-state-cell-import-program-forms)])
     (and (global-definition-form? form)
          (eq? (module-global-name form) '$next-hash-state))))
  (check-not-false
   (for/or ([form (in-list core-state-cell-import-program-forms)])
     (form-contains-global-set? '$next-hash-state form)))

  (define core-parameter-cell-import-split
    (split-core-runtime-module
     `(module
        (global $current-environment-variables-value
                (mut (ref eq))
                (ref.i31 (i32.const 0)))
        (global $current-print-value
                (mut (ref eq))
                (ref.i31 (i32.const 0)))
        (global $current-read-interaction-value
                (mut (ref eq))
                (ref.i31 (i32.const 0)))
        (global $print-as-expression-value
                (mut (ref eq))
                (ref.i31 (i32.const 0)))
        (global $print-mpair-curly-braces-value
                (mut (ref eq))
                (ref.i31 (i32.const 0)))
        (global $prim:print-as-expression
                (mut (ref eq))
                (ref.i31 (i32.const 0)))
        (global $prim:print-mpair-curly-braces
                (mut (ref eq))
                (ref.i31 (i32.const 0)))
        (global $current-compile-realm-value
                (mut (ref eq))
                (ref.i31 (i32.const 0)))
        (global $error-value->string-handler-value
                (mut (ref eq))
                (ref.i31 (i32.const 0)))
        (func $initialize-parameter-cells
              (global.set $current-environment-variables-value (ref.i31 (i32.const 2)))
              (global.set $current-print-value (ref.i31 (i32.const 4)))
              (global.set $current-read-interaction-value (ref.i31 (i32.const 6)))
              (global.set $print-as-expression-value (ref.i31 (i32.const 8)))
              (global.set $print-mpair-curly-braces-value (ref.i31 (i32.const 10)))
              (global.set $prim:print-as-expression (ref.i31 (i32.const 12)))
              (global.set $prim:print-mpair-curly-braces (ref.i31 (i32.const 14)))
              (global.set $current-compile-realm-value (ref.i31 (i32.const 16)))
              (global.set $error-value->string-handler-value (ref.i31 (i32.const 18))))
        (func $entry
              (export "entry")
              (result (ref eq))
              (call $initialize-parameter-cells)
              (global.get $print-as-expression-value)))
     #:helpers '()))
  (define core-parameter-cell-import-program-forms
    (module-forms (split-runtime-modules-program
                   core-parameter-cell-import-split)))
  (for ([name (in-list '($current-environment-variables-value
                         $current-print-value
                         $current-read-interaction-value
                         $print-as-expression-value
                         $print-mpair-curly-braces-value
                         $prim:print-as-expression
                         $prim:print-mpair-curly-braces
                         $current-compile-realm-value
                         $error-value->string-handler-value))])
    (check-not-false
     (member name
             (split-runtime-modules-imported-names
              core-parameter-cell-import-split)))
    (check-not-false
     (member `(global ,name
                      (import "core" ,(runtime-global-export-name name))
                      (mut (ref eq)))
             core-parameter-cell-import-program-forms))
    (check-false
     (for/or ([form (in-list core-parameter-cell-import-program-forms)])
       (and (global-definition-form? form)
            (eq? (module-global-name form) name))))
    (check-not-false
     (for/or ([form (in-list core-parameter-cell-import-program-forms)])
       (form-contains-global-set? name form))))

  (define core-parameterization-state-import-split
    (split-core-runtime-module
     `(module
        (global $current-parameterization-value
                (mut (ref eq))
                (ref.i31 (i32.const 0)))
        (global $break-enabled-key
                (mut (ref eq))
                (ref.i31 (i32.const 0)))
        (global $exception-handler-key
                (mut (ref eq))
                (ref.i31 (i32.const 0)))
        (global $parameterization-key
                (mut (ref eq))
                (ref.i31 (i32.const 0)))
        (global $cache-configuration-values
                (mut (ref eq))
                (ref.i31 (i32.const 0)))
        (func $initialize-parameterization-state
              (global.set $current-parameterization-value (ref.i31 (i32.const 2)))
              (global.set $break-enabled-key (ref.i31 (i32.const 4)))
              (global.set $exception-handler-key (ref.i31 (i32.const 6)))
              (global.set $parameterization-key (ref.i31 (i32.const 8)))
              (global.set $cache-configuration-values (ref.i31 (i32.const 10))))
        (func $entry
              (export "entry")
              (result (ref eq))
              (call $initialize-parameterization-state)
              (global.get $current-parameterization-value)))
     #:helpers '()))
  (define core-parameterization-state-import-program-forms
    (module-forms (split-runtime-modules-program
                   core-parameterization-state-import-split)))
  (for ([name (in-list '($current-parameterization-value
                         $break-enabled-key
                         $exception-handler-key
                         $parameterization-key
                         $cache-configuration-values))])
    (check-not-false
     (member name
             (split-runtime-modules-imported-names
              core-parameterization-state-import-split)))
    (check-not-false
     (member `(global ,name
                      (import "core" ,(runtime-global-export-name name))
                      (mut (ref eq)))
             core-parameterization-state-import-program-forms))
    (check-false
     (for/or ([form (in-list core-parameterization-state-import-program-forms)])
       (and (global-definition-form? form)
            (eq? (module-global-name form) name))))
    (check-not-false
     (for/or ([form (in-list core-parameterization-state-import-program-forms)])
       (form-contains-global-set? name form))))

  (define intern-table-import-split
    (split-core-runtime-module
     `(module
        ,@(generate-shared-types)
        (global $the-symbol-table
                (mut (ref null $SymbolTable))
                (ref.null $SymbolTable))
        (global $the-keyword-table
                (mut (ref null $SymbolTable))
                (ref.null $SymbolTable))
        (func $entry
              (export "entry")
              (result i32)
              (if (result i32)
                  (i32.or
                   (ref.is_null (global.get $the-symbol-table))
                   (ref.is_null (global.get $the-keyword-table)))
                  (then (i32.const 0))
                  (else (i32.const 42)))))
     #:helpers '()))
  (define intern-table-import-program-forms
    (module-forms (split-runtime-modules-program intern-table-import-split)))
  (for ([name (in-list '($the-symbol-table $the-keyword-table))])
    (check-not-false
     (member name
             (split-runtime-modules-imported-names intern-table-import-split)))
    (check-not-false
     (member `(global ,name
                      (import "core" ,(runtime-global-export-name name))
                      (mut (ref null $SymbolTable)))
             intern-table-import-program-forms))
    (check-false
     (for/or ([form (in-list intern-table-import-program-forms)])
       (and (global-definition-form? form)
            (eq? (module-global-name form) name)))))

  (define exn-type-cache-import-split
    (split-core-runtime-module
     `(module
        ,@(generate-shared-types)
        (global $exn-type
                (mut (ref null $StructType))
                (ref.null $StructType))
        (func $entry
              (export "entry")
              (result i32)
              (if (result i32)
                  (ref.is_null (global.get $exn-type))
                  (then (i32.const 42))
                  (else (i32.const 0)))))
     #:helpers '()))
  (define exn-type-cache-import-program-forms
    (module-forms (split-runtime-modules-program exn-type-cache-import-split)))
  (check-not-false
   (member '$exn-type
           (split-runtime-modules-imported-names exn-type-cache-import-split)))
  (check-not-false
   (member '(global $exn-type
                    (import "core" "global:$exn-type")
                    (mut (ref null $StructType)))
           exn-type-cache-import-program-forms))
  (check-false
   (for/or ([form (in-list exn-type-cache-import-program-forms)])
     (and (global-definition-form? form)
          (eq? (module-global-name form) '$exn-type))))

  (define exn-tag-import-split
    (split-core-runtime-module
     `(module
        (tag $exn (param (ref eq)))
        (func $raise-it
              (param $v (ref eq))
              (throw $exn (local.get $v)))
        (func $entry
              (export "entry")
              (param $v (ref eq))
              (call $raise-it (local.get $v))))
     #:helpers '()))
  (define exn-tag-import-program-forms
    (module-forms (split-runtime-modules-program exn-tag-import-split)))
  (check-not-false
   (member '$exn
           (split-runtime-modules-imported-names exn-tag-import-split)))
  (check-not-false
   (member '(tag $exn
                 (import "core" "tag:$exn")
                 (param (ref eq)))
           exn-tag-import-program-forms))
  (check-false
   (for/or ([form (in-list exn-tag-import-program-forms)])
     (and (tag-definition-form? form)
          (eq? (module-tag-name form) '$exn))))

  (when (find-executable-path "wasm-tools")
    (define core-wat-path
      (make-temporary-file "webracket-split-core-runtime-~a.wat"))
    (define core-wasm-path
      (path-replace-extension core-wat-path ".wasm"))
    (define program-wat-path
      (make-temporary-file "webracket-split-program-~a.wat"))
    (define program-wasm-path
      (path-replace-extension program-wat-path ".wasm"))
    (define boundary-core-wat-path
      (make-temporary-file "webracket-boundary-core-~a.wat"))
    (define boundary-core-wasm-path
      (path-replace-extension boundary-core-wat-path ".wasm"))
    (define boundary-program-wat-path
      (make-temporary-file "webracket-boundary-program-~a.wat"))
    (define boundary-program-wasm-path
      (path-replace-extension boundary-program-wat-path ".wasm"))
    (define classified-core-wat-path
      (make-temporary-file "webracket-classified-core-~a.wat"))
    (define classified-core-wasm-path
      (path-replace-extension classified-core-wat-path ".wasm"))
    (define classified-program-wat-path
      (make-temporary-file "webracket-classified-program-~a.wat"))
    (define classified-program-wasm-path
      (path-replace-extension classified-program-wat-path ".wasm"))
    (check-true
     (wat->wasm (split-runtime-modules-core-module core-link-split)
                #:wat core-wat-path
                #:wasm core-wasm-path))
    (check-true
     (wat->wasm core-link-program
                #:wat program-wat-path
                #:wasm program-wasm-path))
    (check-true
     (wat->wasm `(module
                   ,@boundary-runtime-forms
                   ,@boundary-export-forms)
                #:wat boundary-core-wat-path
                #:wasm boundary-core-wasm-path))
    (check-true
     (wat->wasm `(module
                   ,@boundary-import-forms
                   ,@boundary-program-forms)
                #:wat boundary-program-wat-path
                #:wasm boundary-program-wasm-path))
    (check-true
     (wat->wasm (split-runtime-modules-core-module classified-split)
                #:wat classified-core-wat-path
                #:wasm classified-core-wasm-path))
    (check-true
     (wat->wasm (split-runtime-modules-program classified-split)
                #:wat classified-program-wat-path
                #:wasm classified-program-wasm-path))

    (define node-path (find-executable-path "node"))
    (when node-path
      (define node-script-path
        (make-temporary-file "webracket-split-runtime-link-~a.mjs"))
      (with-output-to-file node-script-path
        (lambda ()
          (display
           #<<JS
import fs from "node:fs/promises";

const [corePath, programPath] = process.argv.slice(2);
const coreBytes = await fs.readFile(corePath);
const programBytes = await fs.readFile(programPath);
async function instantiateCore(bytes) {
  const module = await WebAssembly.compile(bytes);
  const imports = {};
  for (const spec of WebAssembly.Module.imports(module)) {
    imports[spec.module] ??= {};
    if (spec.kind === "memory") {
      imports[spec.module][spec.name] = new WebAssembly.Memory({ initial: 1024 });
    } else if (spec.kind === "tag") {
      imports[spec.module][spec.name] = new WebAssembly.Tag({ parameters: ["externref"] });
    } else if (spec.kind === "function") {
      imports[spec.module][spec.name] = (..._args) => {
        if (spec.module === "math" && typeof Math[spec.name] === "function") {
          return Math[spec.name](..._args);
        }
        return 0;
      };
    }
  }
  return { instance: await WebAssembly.instantiate(module, imports), imports };
}
const core = await instantiateCore(coreBytes);
const program = await WebAssembly.instantiate(
  programBytes,
  { ...core.imports, core: core.instance.exports },
);

const expected = new Map([
  ["fx_fixnum_true", 1],
  ["array_ref_tagged", 40],
]);

for (const [name, value] of expected) {
  const actual = program.instance.exports[name]();
  if (actual !== value) {
    throw new Error(`${name}: expected ${value}, got ${actual}`);
  }
}
JS
           ))
        #:exists 'replace)
      (check-equal?
       (system*/exit-code node-path
                          (path->string node-script-path)
                          (path->string core-wasm-path)
                          (path->string program-wasm-path))
       0)
      (define boundary-node-script-path
        (make-temporary-file "webracket-boundary-link-~a.mjs"))
      (with-output-to-file boundary-node-script-path
        (lambda ()
          (display
           #<<JS
import fs from "node:fs/promises";

const [corePath, programPath] = process.argv.slice(2);
const coreBytes = await fs.readFile(corePath);
const programBytes = await fs.readFile(programPath);
async function instantiateCore(bytes) {
  const module = await WebAssembly.compile(bytes);
  const imports = {};
  for (const spec of WebAssembly.Module.imports(module)) {
    imports[spec.module] ??= {};
    if (spec.kind === "memory") {
      imports[spec.module][spec.name] = new WebAssembly.Memory({ initial: 1024 });
    } else if (spec.kind === "tag") {
      imports[spec.module][spec.name] = new WebAssembly.Tag({ parameters: ["externref"] });
    } else if (spec.kind === "function") {
      imports[spec.module][spec.name] = (..._args) => {
        if (spec.module === "math" && typeof Math[spec.name] === "function") {
          return Math[spec.name](..._args);
        }
        return 0;
      };
    }
  }
  return { instance: await WebAssembly.instantiate(module, imports), imports };
}
const core = await instantiateCore(coreBytes);
const program = await WebAssembly.instantiate(
  programBytes,
  { ...core.imports, core: core.instance.exports },
);

const actual = program.instance.exports.probe();
if (actual !== 42) {
  throw new Error(`probe: expected 42, got ${actual}`);
}
JS
           ))
        #:exists 'replace)
      (check-equal?
       (system*/exit-code node-path
                          (path->string boundary-node-script-path)
                          (path->string boundary-core-wasm-path)
                          (path->string boundary-program-wasm-path))
       0)
      (define classified-node-script-path
        (make-temporary-file "webracket-classified-link-~a.mjs"))
      (with-output-to-file classified-node-script-path
        (lambda ()
          (display
           #<<JS
import fs from "node:fs/promises";

const [corePath, programPath] = process.argv.slice(2);
const coreBytes = await fs.readFile(corePath);
const programBytes = await fs.readFile(programPath);
async function instantiateCore(bytes) {
  const module = await WebAssembly.compile(bytes);
  const imports = {};
  for (const spec of WebAssembly.Module.imports(module)) {
    imports[spec.module] ??= {};
    if (spec.kind === "memory") {
      imports[spec.module][spec.name] = new WebAssembly.Memory({ initial: 1024 });
    } else if (spec.kind === "tag") {
      imports[spec.module][spec.name] = new WebAssembly.Tag({ parameters: ["externref"] });
    } else if (spec.kind === "function") {
      imports[spec.module][spec.name] = (..._args) => {
        if (spec.module === "math" && typeof Math[spec.name] === "function") {
          return Math[spec.name](..._args);
        }
        return 0;
      };
    }
  }
  return { instance: await WebAssembly.instantiate(module, imports), imports };
}
const core = await instantiateCore(coreBytes);
const program = await WebAssembly.instantiate(
  programBytes,
  { ...core.imports, core: core.instance.exports },
);

const actual = program.instance.exports.probe();
if (actual !== 42) {
  throw new Error(`probe: expected 42, got ${actual}`);
}
JS
           ))
        #:exists 'replace)
      (check-equal?
       (system*/exit-code node-path
                          (path->string classified-node-script-path)
                          (path->string classified-core-wasm-path)
                          (path->string classified-program-wasm-path))
       0))))
