#lang racket
(provide generate-runtime
         generate-core-runtime
         generate-shared-types
         (struct-out split-runtime-modules)
         split-runtime-modules-core-module
         split-core-runtime-module)

(require (only-in "private/priminfo.rkt"
                  primitive->description)
         (only-in "private/parameters.rkt"
                  current-browser?
                  current-runtime-cache-dir
                  current-tree-shake?
                  current-split-runtime?
                  current-split-runtime-reference-only?
                  current-split-runtime-ffi-in-core?
                  current-split-runtime-ffi-module?
                  current-split-runtime-ffi-program-owned?
                  current-split-runtime-program-only-fast-path?)
         (only-in "private/structs.rkt"
                  foreign-argument-types
                  foreign-racket-name)
         (only-in "private/wasm-utils.rkt"
                  $
                  prim:)
         (only-in "runtime-wasm/entry-support.rkt"
                  entry-function-forms
                  entry-result-global-forms
                  entry-support-forms)
         (only-in "runtime-wasm/function-declarations.rkt"
                  runtime-function-declaration-forms)
         (only-in "runtime-wasm/js-imports.rkt"
                  js-host-import-forms)
         (only-in "runtime-wasm/module-forms.rkt"
                  module-func-name
                  module-func-names)
         (only-in "runtime-wasm/runtime-analysis.rkt"
                  analyze-and-prune-runtime-module
                  analyzed-runtime-module?
                  analyzed-runtime-module-module
                  analyzed-runtime-module-program-forms
                  analyzed-runtime-module-runtime-forms
                  program-driver-function-names
                  split-runtime-request-cache-fingerprint)
         (only-in "runtime-wasm/runtime-helper-index.rkt"
                  runtime-helper-index)
         (only-in "runtime-wasm/runtime-ffi.rkt"
                  external-value-core-runtime-helper-forms
                  external-value-runtime-helper-forms
                  ffi-import-name
                  ffi-core-runtime-helper-forms
                  ffi-callback-global-forms
                  ffi-entry-global-forms
                  ffi-console-bridge-helper-names
                  ffi-console-bridge-helper-forms
                  ffi-runtime-only-helper-forms
                  make-current-runtime-ffi-state
                  runtime-ffi-state-active-foreigns
                  runtime-ffi-state-active-funcs-wat
                  runtime-ffi-state-active-imports-wat
                  runtime-ffi-state-active-primitive-names
                  runtime-ffi-state-all-foreigns
                  runtime-ffi-state-filter-current-bindings!
                  runtime-ffi-state-import-wrapper-helper-forms
                  runtime-ffi-state-primitive-global-forms
                  runtime-ffi-state-primitive-initializer-forms
                  runtime-ffi-state-unsafe-primitive-helper-forms
                  runtime-ffi-state-record-metrics!
                  select-current-ffi-runtime-bindings
                  unsafe-ffi-primitive-table-global-forms)
         (only-in "runtime-wasm/runtime-metrics.rkt"
                  runtime-metric-add!
                  time-runtime-step)
         (only-in "runtime-wasm/runtime-primitives.rkt"
                  make-runtime-primitive-state
                  runtime-primitive-state-active-described-primitives-sorted
                  runtime-primitive-state-console-bridge-primitive-names
                  runtime-primitive-state-described-primitives
                  runtime-primitive-state-ffi-global->symbol
                  runtime-primitive-state-global-forms
                  runtime-primitive-state-initializer-forms
                  runtime-primitive-state-primitive-func->symbol
                  runtime-primitive-state-primitive-global->symbol
                  runtime-primitive-global-forms
                  runtime-primitive-initializer-forms)
         (only-in "runtime-wasm/runtime-source.rkt"
                  runtime-source-fingerprint)
         (only-in "runtime-wasm/split-runtime-cache.rkt"
                  make-split-runtime-core-cache-key
                  make-split-runtime-ffi-cache-key
                  split-runtime-core-cache-key-fingerprint
                  split-runtime-core-cache-wasm-path)
         (only-in "webify/provider-cache.rkt"
                  current-provider-cache-force?)
         (only-in "runtime-wasm/core/arrays.rkt"
                  array-core-runtime-helper-forms
                  array-runtime-helper-forms)
         (only-in "runtime-wasm/core/booleans.rkt"
                  boolean-runtime-helper-forms)
         (only-in "runtime-wasm/core/bignums.rkt"
                  bignum-core-runtime-helper-forms)
         (only-in "runtime-wasm/core/rationals.rkt"
                  rational-core-runtime-helper-forms)
         (only-in "runtime-wasm/core/boxes.rkt"
                  box-runtime-helper-forms
                  box-mutability-runtime-helper-forms)
         (only-in "runtime-wasm/core/bytes.rkt"
                  bytes-main-runtime-helper-forms
                  bytes-mutability-runtime-helper-forms)
         (only-in "runtime-wasm/core/characters.rkt"
                  character-general-category-global-forms
                  character-general-category-initializer-forms
                  character-runtime-helper-forms)
         (only-in "runtime-wasm/core/checkers.rkt"
                  checker-runtime-helper-forms)
         (only-in "runtime-wasm/core/closures.rkt"
                  closure-empty-free-global-forms
                  closure-runtime-global-forms)
         (only-in "runtime-wasm/core/control-flow.rkt"
                  continuation-mark-runtime-helper-forms
                  continuation-runtime-helper-forms
                  multiple-value-runtime-helper-forms
                  value-expectation-runtime-helper-forms)
         (only-in "runtime-wasm/core/equality.rkt"
                  equality-core-runtime-helper-forms
                  immutable-predicate-core-runtime-helper-forms
                  immutable-predicate-runtime-helper-forms
                  equality-runtime-helper-forms)
         (only-in "runtime-wasm/core/exceptions.rkt"
                  bootstrap-error-runtime-helper-forms
                  exception-tag-forms
                  exception-struct-type-bindings
                  exception-struct-type-cache-global-forms
                  exception-struct-type-global-forms
                  exception-struct-type-initializer-forms
                  exception-contract-runtime-helper-forms
                  exception-runtime-helper-forms)
         (only-in "runtime-wasm/core/events.rkt"
                  event-global-forms
                  event-initializer-forms
                  event-core-runtime-helper-forms
                  event-channel-runtime-helper-forms
                  event-sync-runtime-helper-forms)
         (only-in "runtime-wasm/core/fasl.rkt"
                  fasl-tag-runtime-helper-forms
                  fasl-encoding-runtime-helper-forms
                  fasl-decoding-runtime-helper-forms)
         (only-in "runtime-wasm/core/filesystem.rkt"
                  filesystem-temporary-global-forms
                  filesystem-runtime-helper-forms)
         (only-in "runtime-wasm/core/fixnums.rkt"
                  fixnum-global-forms
                  fixnum-runtime-helper-forms)
         (only-in "runtime-wasm/core/flonums.rkt"
                  flonum-global-forms
                  flonum-initializer-forms
                  flonum-runtime-helper-forms)
         (only-in "runtime-wasm/core/formatting.rkt"
                  printer-extension-runtime-helper-forms
                  unquoted-printing-string-core-runtime-helper-forms
                  unquoted-printing-string-runtime-helper-forms
                  display-formatting-core-runtime-helper-forms
                  formatting-runtime-helper-forms
                  numeric-display-formatting-runtime-helper-forms)
         (only-in "runtime-wasm/core/growable-arrays.rkt"
                  growable-array-core-runtime-helper-forms)
         (only-in "runtime-wasm/core/growable-bytes.rkt"
                  growable-bytes-core-runtime-helper-forms)
         (only-in "runtime-wasm/core/growable-i32-arrays.rkt"
                  growable-i32-array-core-runtime-helper-forms)
         (only-in "runtime-wasm/core/hashes.rkt"
                  hash-code-runtime-helper-forms
                  hash-seed-global-forms
                  hash-predicate-core-runtime-helper-forms
                  hash-predicate-runtime-helper-forms
                  hash-table-predicate-core-runtime-helper-forms
                  hash-table-predicate-runtime-helper-forms
                  hash-table-operation-runtime-helper-forms)
         (only-in "runtime-wasm/core/immutable-hashes.rkt"
                  immutable-hash-prelude-runtime-helper-forms
                  immutable-hash-public-set/remove-runtime-helper-forms
                  immutable-hash-public-update-runtime-helper-forms
                  immutable-hash-public-clear-runtime-helper-forms
                  immutable-hash-iteration-runtime-helper-forms)
         (only-in "runtime-wasm/core/i32-arrays.rkt"
                  i32-array-core-runtime-helper-forms
                  i32-array-runtime-helper-forms)
         (only-in "runtime-wasm/core/i8-arrays.rkt"
                  i8-array-byte-core-runtime-helper-forms
                  i8-array-core-runtime-helper-forms
                  i8-array-error-runtime-helper-forms
                  i8-array-conversion-runtime-helper-forms
                  i8-array-string-conversion-runtime-helper-forms)
         (only-in "runtime-wasm/core/immediates.rkt"
                  singleton-global-forms
                  void-runtime-helper-forms
                  eof-object-runtime-helper-forms)
         (only-in "runtime-wasm/core/impersonators.rkt"
                  chaperone-procedure-runtime-helper-forms
                  impersonator-core-runtime-helper-forms
                  impersonator-basic-runtime-helper-forms)
         (only-in "runtime-wasm/core/io-handlers.rkt"
                  io-handler-default-port-global-forms
                  io-handler-default-port-late-initializer-forms
                  io-handler-environment-predicate-runtime-helper-forms
                  io-handler-environment-ref-runtime-helper-forms
                  io-handler-error-runtime-helper-forms
                  io-handler-port-runtime-helper-forms
                  io-handler-reader-printer-runtime-helper-forms)
         (only-in "runtime-wasm/core/keywords.rkt"
                  keyword-core-runtime-helper-forms
                  keyword-table-global-forms
                  keyword-table-initializer-runtime-helper-forms
                  keyword-runtime-helper-forms)
         (only-in "runtime-wasm/core/lists.rkt"
                  list-pre-contract-runtime-helper-forms
                  list-post-contract-runtime-helper-forms
                  pair-global-forms)
         (only-in "runtime-wasm/core/linklet-instances.rkt"
                  linklet-instance-core-runtime-helper-forms
                  linklet-instance-runtime-helper-forms)
         (only-in "runtime-wasm/core/linklets.rkt"
                  linklet-bootstrap-core-runtime-helper-forms
                  linklet-bootstrap-runtime-helper-forms
                  linklet-interpreter-core-runtime-helper-forms
                  linklet-interpreter-runtime-helper-forms
                  primitive-table-module-symbols
                  linklet-primitive-table-global-forms
                  linklet-primitive-table-initializer-runtime-helper-forms
                  linklet-primitive-table-runtime-only-helper-forms
                  linklet-runtime-only-helper-forms)
         (only-in "runtime-wasm/core/locations.rkt"
                  location-global-forms
                  location-initial-runtime-helper-forms
                  source-location-core-runtime-helper-forms
                  source-location-runtime-helper-forms)
         (only-in "runtime-wasm/core/locks.rkt"
                  lock-runtime-helper-forms)
         (only-in "runtime-wasm/core/loggers.rkt"
                  logger-current-global-forms
                  logger-current-initializer-forms
                  logger-core-runtime-helper-forms
                  logger-runtime-helper-forms)
         (only-in "runtime-wasm/core/memory-map.rkt"
                  memory-import-forms
                  memory-map-global-forms)
         (only-in "runtime-wasm/core/mlists.rkt"
                  mlist-core-runtime-helper-forms
                  mlist-runtime-helper-forms)
         (only-in "runtime-wasm/core/mutable-hashes.rkt"
                  mutable-hash-constructor-runtime-helper-forms
                  mutable-hash-empty-iteration-runtime-helper-forms
                  mutable-hash-read-runtime-helper-forms
                  mutable-hash-remove-membership-clear-runtime-helper-forms
                  mutable-hash-update-runtime-helper-forms
                  mutable-hash-write-runtime-helper-forms)
         (only-in "runtime-wasm/core/namespaces.rkt"
                  namespace-current-global-forms
                  namespace-initializer-forms
                  namespace-module-registry-global-forms
                  namespace-runtime-helper-forms
                  namespace-top-level-global-forms)
         (only-in "runtime-wasm/core/numbers.rkt"
                  generic-arithmetic-runtime-helper-forms
                  generic-bitwise-runtime-helper-forms
                  generic-numeric-runtime-helper-forms
                  number-error-runtime-helper-forms
                  number-string-conversion-runtime-helper-forms
                  floating-point-byte-conversion-runtime-helper-forms
                  numeric-formatting-runtime-helper-forms
                  string-number-conversion-runtime-helper-forms
                  number-type-runtime-helper-forms)
         (only-in "runtime-wasm/core/parameters.rkt"
                  compile-parameter-global-forms
                  compile-parameter-core-runtime-helper-forms
                  compile-parameter-runtime-helper-forms
                  current-security-guard-core-runtime-helper-forms
                  current-security-guard-runtime-helper-forms
                  current-custodian-core-runtime-helper-forms
                  current-custodian-runtime-helper-forms
                  current-pseudo-random-generator-core-runtime-helper-forms
                  current-pseudo-random-generator-runtime-helper-forms
                  current-environment-variables-runtime-helper-forms
                  current-print-runtime-helper-forms
                  current-readtable-core-runtime-helper-forms
                  current-readtable-runtime-helper-forms
                  current-read-interaction-runtime-helper-forms
                  current-load-relative-directory-runtime-helper-forms
                  namespace-parameter-core-runtime-helper-forms
                  namespace-parameter-runtime-helper-forms
                  parameterization-binding-core-runtime-helper-forms
                  parameterization-binding-runtime-helper-forms
                  parameterization-call-runtime-helper-forms
                  parameter-directory-runtime-helper-forms
                  parameter-error-rendering-core-runtime-helper-forms
                  parameter-error-rendering-runtime-helper-forms
                  parameterization-global-forms
                  parameterization-initializer-forms
                  parameterization-runtime-helper-forms
                  parameter-width-core-runtime-helper-forms
                  parameter-width-runtime-helper-forms
                  error-rendering-handler-global-forms
                  error-value-handler-late-initializer-forms
                  error-syntax-handler-late-initializer-forms
                  print-read-parameter-global-forms
                  print-read-parameter-late-initializer-forms
                  environment-parameter-global-forms
                  environment-parameter-initializer-forms
                  reader-printer-policy-parameter-global-forms
                  reader-printer-policy-parameter-helper-forms)
         (only-in "runtime-wasm/core/paths.rkt"
                  path-current-global-forms
                  path-current-initializer-forms
                  path-core-runtime-helper-forms
                  path-runtime-helper-forms
                  path-manipulation-core-runtime-helper-forms
                  path-manipulation-runtime-helper-forms)
         (only-in "runtime-wasm/core/ports.rkt"
                  port-call-with-output-string-runtime-helper-forms
                  port-current-global-forms
                  port-custom-constructor-runtime-helper-forms
                  port-input-runtime-helper-forms
                  port-location-runtime-helper-forms
                  port-output-predicate-runtime-helper-forms
                  port-output-runtime-helper-forms
                  port-output-string-runtime-helper-forms
                  port-predicate-runtime-helper-forms
                  port-state-runtime-helper-forms
                  port-string-constructor-runtime-helper-forms)
         (only-in "runtime-wasm/core/procedures.rkt"
                  procedure-global-forms
                  primitive-reflection-runtime-helper-forms
                  procedure-arity-mask-runtime-helper-forms
                  procedure-bootstrap-runtime-helper-forms
                  procedure-case-lambda-core-runtime-helper-forms
                  procedure-invocation-error-core-runtime-helper-forms
                  procedure-invoker-core-runtime-helper-forms
                  procedure-main-runtime-helper-forms
                  procedure-post-primitive-invoker-runtime-helper-forms
                  procedure-primitive-dispatch-core-runtime-helper-forms
                  procedure-primitive-error-core-runtime-helper-forms
                  procedure-primitive-invoker-core-runtime-helper-forms
                  procedure-rest-argument-core-runtime-helper-forms
                  primitive-shapes)
         (only-in "runtime-wasm/core/random.rkt"
                  random-current-global-forms
                  random-current-initializer-forms
                  random-core-runtime-helper-forms
                  random-generator-runtime-helper-forms
                  random-public-runtime-helper-forms
                  random-state-global-forms)
         (only-in "runtime-wasm/core/realms.rkt"
                  realm-global-forms
                  realm-initializer-forms)
         (only-in "runtime-wasm/core/regexps.rkt"
                  regexp-runtime-helper-forms)
         (only-in "runtime-wasm/core/runtime-constants.rkt"
                  make-program-runtime-constant-registry
                  runtime-constant-declaration-forms
                  runtime-constant-initializer-forms
                  runtime-linklet-body-reserved-symbols)
         (only-in "runtime-wasm/constants.rkt"
                  make-runtime-constant-registry
                  runtime-constant-registry-add-symbols!)
         (only-in "runtime-wasm/core/reflection.rkt"
                  reflection-core-runtime-helper-forms
                  reflection-runtime-helper-forms)
         (only-in "runtime-wasm/core/security.rkt"
                  custodian-current-initializer-forms
                  custodian-runtime-helper-forms
                  inspector-runtime-helper-forms
                  security-core-runtime-helper-forms
                  security-current-global-forms
                  security-guard-current-initializer-forms
                  security-post-parameter-runtime-helper-forms
                  security-pre-parameter-runtime-helper-forms)
         (only-in "runtime-wasm/core/semaphores.rkt"
                  semaphore-core-runtime-helper-forms
                  semaphore-runtime-helper-forms)
         (only-in "runtime-wasm/core/stencil-vectors.rkt"
                  stencil-vector-core-runtime-helper-forms
                  stencil-vector-runtime-helper-forms)
         (only-in "runtime-wasm/core/strings.rkt"
                  string-buffer-runtime-helper-forms
                  string-hash-core-runtime-helper-forms
                  string-main-runtime-helper-forms
                  string-mutability-runtime-helper-forms)
         (only-in "runtime-wasm/core/structs.rkt"
                  struct-basic-runtime-helper-forms
                  struct-construction-runtime-helper-forms
                  struct-constructor-access-runtime-helper-forms
                  struct-core-predicate-runtime-helper-forms
                  struct-prefab-registry-runtime-helper-forms
                  struct-prefab-runtime-helper-forms
                  struct-procedure-invoker-runtime-helper-forms
                  struct-procedure-runtime-helper-forms
                  struct-type-property-runtime-helper-forms
                  struct-property-global-forms
                  struct-property-initializer-forms
                  struct-prefab-registry-global-forms)
         (only-in "runtime-wasm/core/symbols.rkt"
                  symbol-gensym-global-forms
                  symbol-gensym-core-runtime-helper-forms
                  symbol-gensym-runtime-helper-forms
                  symbol-public-runtime-helper-forms
                  symbol-table-global-forms
                  symbol-table-runtime-helper-forms
                  symbol-table-initializer-runtime-helper-forms)
         (only-in "runtime-wasm/core/system.rkt"
                  system-runtime-helper-forms)
         (only-in "runtime-wasm/core/syntax.rkt"
                  correlated-syntax-runtime-helper-forms
                  syntax-global-forms
                  syntax-runtime-helper-forms)
         (only-in "runtime-wasm/core/threads.rkt"
                  thread-current-global-forms
                  thread-current-initializer-forms
                  thread-group-current-global-forms
                  thread-core-runtime-helper-forms
                  thread-runtime-helper-forms)
         (only-in "runtime-wasm/core/time.rkt"
                  time-core-runtime-helper-forms
                  time-runtime-helper-forms)
         (only-in "runtime-wasm/core/treelists.rkt"
                  treelist-constant-global-forms
                  treelist-empty-global-forms
                  treelist-core-runtime-helper-forms
                  treelist-runtime-helper-forms)
         (only-in "runtime-wasm/core/unsafe.rkt"
                  unsafe-runtime-helper-forms)
         (only-in "runtime-wasm/core/vectors.rkt"
                  vector-dummy-array-global-forms
                  vector-dummy-global-forms
                  vector-error-runtime-helper-forms
                  vector-main-runtime-helper-forms
                  vector-mutability-runtime-helper-forms
                  vector-post-stencil-runtime-helper-forms)
         (only-in "runtime-wasm/core/wills.rkt"
                  will-core-runtime-helper-forms
                  will-runtime-helper-forms)
         "runtime-wasm/core-runtime.rkt"
         "runtime-wasm/shared-types.rkt"
         "runtime-wasm/split-runtime.rkt")

(module+ test
  (require (submod "runtime-wasm/core-runtime.rkt" test)))

; See the end of `generate-code` in `compiler.rkt` for an
; explanation of the arguments.
(define (generate-runtime
           dls tms entry-body result
           ; program specific
           top-vars
           console-bridge-bindings
           top-level-variable-declarations
           entry-locals
           ; general information
           primitives       ; list of symbols
           used-primitives  ; list of symbols referenced by generated program code
           string-constants ; (list (list name string) ...)
           bytes-constants  ; (list (list name bytes)  ...)
           symbol-constants ; (list (list name symbol) ....)
           )

  #;(pretty-print entry-body (current-error-port))

  (let ()

    ;; Variable definitions on the top-level ought to go in a namespace,
    ;; but for now we use a global WebAssembly variable.

    ;;;
    ;;; Primitives
    ;;;

    (define all-primitives primitives)
    (define ffi-state (make-current-runtime-ffi-state))
    (define all-ffi-foreigns (runtime-ffi-state-all-foreigns ffi-state))
    (define primitive-state
      (make-runtime-primitive-state all-primitives
                                    console-bridge-bindings
                                    all-ffi-foreigns))

    (define active-described-primitives-sorted
      (runtime-primitive-state-active-described-primitives-sorted
       primitive-state))

    (define described-primitives
      (runtime-primitive-state-described-primitives primitive-state))

    (define console-bridge-enabled? (pair? console-bridge-bindings))
    (define console-bridge-primitive-names
      (runtime-primitive-state-console-bridge-primitive-names primitive-state))

    (define primitive-func->symbol
      (runtime-primitive-state-primitive-func->symbol primitive-state))

    (define primitive-global->symbol
      (runtime-primitive-state-primitive-global->symbol primitive-state))

    (define ffi-global->symbol
      (runtime-primitive-state-ffi-global->symbol primitive-state))

    (define split-ffi-sidecar-candidate-foreigns
      (let-values ([(foreigns _imports _funcs)
                    (select-current-ffi-runtime-bindings
                     all-ffi-foreigns
                     used-primitives)])
        foreigns))
    (define split-ffi-sidecar-enabled?
      (and (current-split-runtime?)
           (current-split-runtime-ffi-module?)
           (not (current-split-runtime-ffi-in-core?))
           (pair? split-ffi-sidecar-candidate-foreigns)))
    (define (split-ffi-sidecar?)
      split-ffi-sidecar-enabled?)
    (define (split-ffi-sidecar-wide?)
      (and (split-ffi-sidecar?)
           (for/or ([f (in-list split-ffi-sidecar-candidate-foreigns)])
             (> (length (foreign-argument-types f)) 6))))
    (define (split-ffi-sidecar-unsafe?)
      (and (split-ffi-sidecar?)
           (memq 'unsafe-ffi-primitive used-primitives)))
    (define split-program-described-primitive-names '())
    (define split-program-described-primitive-name-set
      (for/hasheq ([name (in-list split-program-described-primitive-names)])
        (values name #t)))
    (define (split-direct-ffi-program-side?)
      (and (current-split-runtime?)
           (not (current-split-runtime-ffi-in-core?))
           (not (split-ffi-sidecar?))
           (not (memq 'unsafe-ffi-primitive used-primitives))))
    (define (split-ffi-program-owned?)
      (and (current-split-runtime-ffi-program-owned?)
           (or (split-direct-ffi-program-side?)
               (split-ffi-sidecar?))))

    (define program-function-names
      (module-func-names dls))

    (define linklet-body-reserved-symbols
      (runtime-linklet-body-reserved-symbols all-primitives))

    ;; Runtime constants include program constants passed by `generate-code`
    ;; plus constants required by runtime support.
    (define runtime-constant-registry
      (make-program-runtime-constant-registry
       string-constants
       bytes-constants
       symbol-constants
       active-described-primitives-sorted
       (runtime-ffi-state-active-primitive-names ffi-state)
       all-primitives))
    (define runtime-only-constant-registry
      (make-program-runtime-constant-registry
       '()
       '()
       '()
       active-described-primitives-sorted
       (runtime-ffi-state-active-primitive-names ffi-state)
       all-primitives))
    (define program-only-constant-registry
      (let ([registry (make-runtime-constant-registry string-constants
                                                       bytes-constants
                                                       symbol-constants)])
        ;; Split program modules initialize FFI primitive procedure globals on
        ;; the program side, so their procedure names must be declared as
        ;; program-owned symbol constants too.
        (when (split-ffi-program-owned?)
          (runtime-constant-registry-add-symbols!
           registry
           (runtime-ffi-state-active-primitive-names ffi-state)))
        (when (pair? split-program-described-primitive-names)
          (runtime-constant-registry-add-symbols!
           registry
           split-program-described-primitive-names))
        registry))

    (define split-runtime-described-primitives-sorted
      (for/list ([name (in-list active-described-primitives-sorted)]
                 #:unless (hash-ref split-program-described-primitive-name-set
                                     name
                                     #f))
        name))
    (define split-program-described-primitives-sorted
      (for/list ([name (in-list active-described-primitives-sorted)]
                 #:when (hash-ref split-program-described-primitive-name-set
                                   name
                                   #f))
        name))
    (define primitive-initializer-forms
      (if (current-split-runtime?)
          (runtime-primitive-initializer-forms
           split-runtime-described-primitives-sorted)
          (runtime-primitive-state-initializer-forms primitive-state)))
    (define split-program-primitive-global-forms
      (runtime-primitive-global-forms split-program-described-primitives-sorted))
    (define split-program-primitive-initializer-forms
      (runtime-primitive-initializer-forms split-program-described-primitives-sorted))
    (define unsafe-ffi-primitive-table-initializer-forms
      '((call $initialize-unsafe-ffi-primitive-table)))
    (define (split-ffi-sidecar-needs-initializer?)
      (or (split-ffi-sidecar-wide?)
          (split-ffi-sidecar-unsafe?)))
    (define (split-ffi-sidecar-registration-forms)
      (if (split-ffi-sidecar-unsafe?)
          (for/list ([f (in-list (runtime-ffi-state-active-foreigns ffi-state))])
            (define pr (foreign-racket-name f))
            `(call $register-unsafe-ffi-primitive!
                   (global.get ,($ (string->symbol (~a "symbol:" pr))))
                   (global.get ,($ (prim: pr)))))
          '()))
    (define (split-program-ffi-foreigns)
      (if (split-direct-ffi-program-side?)
          (runtime-ffi-state-active-foreigns ffi-state)
          '()))
    (define (split-program-ffi-function-names)
      (if (split-ffi-program-owned?)
          (let ([active-foreigns (runtime-ffi-state-active-foreigns ffi-state)]
                [active-names (runtime-ffi-state-active-primitive-names ffi-state)])
            (append (map ffi-import-name active-foreigns)
                    (map $ active-names)
                    (if (split-ffi-sidecar-needs-initializer?)
                        (list split-ffi-sidecar-initializer-name)
                        '())))
          '()))
    (define (split-program-console-bridge-function-names)
      (if console-bridge-enabled?
          ffi-console-bridge-helper-names
          '()))
    (define (split-program-ffi-global-names)
      (for/list ([name (in-list (runtime-ffi-state-active-primitive-names ffi-state))]
                 #:when (split-ffi-program-owned?))
        ($ (prim: name))))
    (define (split-ffi-sidecar-forms)
      (if (split-ffi-sidecar?)
          (append (runtime-ffi-state-active-imports-wat ffi-state)
                  (runtime-ffi-state-active-funcs-wat ffi-state))
          '()))
    (define split-ffi-sidecar-initializer-name
      '$initialize-split-ffi-sidecar-primitives!)
    (define (split-ffi-sidecar-extra-forms)
      (if (split-ffi-sidecar-needs-initializer?)
          (append (runtime-ffi-state-primitive-global-forms ffi-state)
                  (list
                   `(func ,split-ffi-sidecar-initializer-name
                          ,@(runtime-ffi-state-primitive-initializer-forms
                             ffi-state)
                          ,@(split-ffi-sidecar-registration-forms))))
          '()))
    (define (split-ffi-sidecar-function-names)
      (if (split-ffi-sidecar?)
          (append (map $ (runtime-ffi-state-active-primitive-names ffi-state))
                  (if (split-ffi-sidecar-needs-initializer?)
                      (list split-ffi-sidecar-initializer-name)
                      '()))
          '()))
    ;; Symbol and keyword tables are initialized here for interning.
    (define global-state-initializer-forms
      '((call $initialize-the-symbol-table)
        (call $initialize-the-keyword-table)
        (call $initialize-primitive-table-user-installed)
        (call $initialize-prefab-struct-type-registry)))
    (define split-runtime-primitive-initializer-name
      '$initialize-split-runtime-primitives!)
    (define split-runtime-constant-initializer-name
      '$initialize-split-runtime-constants!)
    (define split-runtime-pre-constant-initializer-name
      '$initialize-split-runtime-before-constants!)
    (define split-runtime-post-constant-initializer-name
      '$initialize-split-runtime-after-constants!)
    (define split-runtime-post-ffi-initializer-name
      '$initialize-split-runtime-after-ffi!)
    (define split-runtime-primitive-initializer-helper-forms
      (if (current-split-runtime?)
          (list
           `(func ,split-runtime-primitive-initializer-name
                  ,@primitive-initializer-forms))
          '()))
    (define split-runtime-constant-initializer-helper-forms
      (if (current-split-runtime?)
          (list
           `(func ,split-runtime-constant-initializer-name
                  ,@(runtime-constant-initializer-forms
                     runtime-only-constant-registry)))
          '()))
    (define split-runtime-entry-initializer-helper-forms
      (if (current-split-runtime?)
          `((func ,split-runtime-pre-constant-initializer-name
                  ,@flonum-initializer-forms
                  ,@global-state-initializer-forms)
            (func ,split-runtime-post-constant-initializer-name
                  ,@parameterization-initializer-forms
                  ,@random-current-initializer-forms
                  ,@thread-current-initializer-forms
                  ,@logger-current-initializer-forms
                  ,@custodian-current-initializer-forms
                  ,@event-initializer-forms
                  ,@struct-property-initializer-forms
                  ,@path-current-initializer-forms
                  ,@environment-parameter-initializer-forms
                  (drop (call $reset-current-input-port!))
                  (drop (call $reset-current-output-port!))
                  (drop (call $reset-current-error-port!))
                  ,@character-general-category-initializer-forms
                  ,@realm-initializer-forms
                  (call ,split-runtime-primitive-initializer-name))
            (func ,split-runtime-post-ffi-initializer-name
                  ,@security-guard-current-initializer-forms
                  ,@error-value-handler-late-initializer-forms
                  ,@print-read-parameter-late-initializer-forms
                  ,@io-handler-default-port-late-initializer-forms
                  ,@error-syntax-handler-late-initializer-forms))
          '()))
    (define entry-flonum-initializer-forms
      (if (current-split-runtime?)
          `((call ,split-runtime-pre-constant-initializer-name))
          flonum-initializer-forms))
    (define entry-global-state-initializer-forms
      (if (current-split-runtime?)
          '()
          global-state-initializer-forms))
    (define entry-runtime-constant-initializer-forms
      (if (current-split-runtime?)
          (append
           `((call ,split-runtime-constant-initializer-name))
           (runtime-constant-initializer-forms program-only-constant-registry))
          (runtime-constant-initializer-forms runtime-constant-registry)))
    (define entry-parameterization-initializer-forms
      (if (current-split-runtime?)
          `((call ,split-runtime-post-constant-initializer-name))
          parameterization-initializer-forms))
    (define entry-random-current-initializer-forms
      (if (current-split-runtime?) '() random-current-initializer-forms))
    (define entry-thread-current-initializer-forms
      (if (current-split-runtime?) '() thread-current-initializer-forms))
    (define entry-logger-current-initializer-forms
      (if (current-split-runtime?) '() logger-current-initializer-forms))
    (define entry-custodian-current-initializer-forms
      (if (current-split-runtime?) '() custodian-current-initializer-forms))
    (define entry-event-initializer-forms
      (if (current-split-runtime?) '() event-initializer-forms))
    (define entry-struct-property-initializer-forms
      (if (current-split-runtime?) '() struct-property-initializer-forms))
    (define entry-path-current-initializer-forms
      (if (current-split-runtime?) '() path-current-initializer-forms))
    (define entry-environment-parameter-initializer-forms
      (if (current-split-runtime?) '() environment-parameter-initializer-forms))
    (define entry-character-general-category-initializer-forms
      (if (current-split-runtime?) '() character-general-category-initializer-forms))
    (define entry-realm-initializer-forms
      (if (current-split-runtime?) '() realm-initializer-forms))
    (define entry-primitive-initializer-forms
      (if (current-split-runtime?)
          split-program-primitive-initializer-forms
          primitive-initializer-forms))
    (define entry-unsafe-ffi-primitive-table-initializer-forms
      ;; Split program modules may not own the unsafe FFI lookup helper. The
      ;; helper initializes the table lazily when it is actually called.
      (if (current-split-runtime?)
          '()
          unsafe-ffi-primitive-table-initializer-forms))
    (define entry-security-guard-current-initializer-forms
      (if (current-split-runtime?)
          `((call ,split-runtime-post-ffi-initializer-name))
          security-guard-current-initializer-forms))
    (define entry-error-value-handler-late-initializer-forms
      (if (current-split-runtime?) '() error-value-handler-late-initializer-forms))
    (define entry-print-read-parameter-late-initializer-forms
      (if (current-split-runtime?) '() print-read-parameter-late-initializer-forms))
    (define entry-io-handler-default-port-late-initializer-forms
      (if (current-split-runtime?) '() io-handler-default-port-late-initializer-forms))
    (define entry-error-syntax-handler-late-initializer-forms
      (if (current-split-runtime?) '() error-syntax-handler-late-initializer-forms))

    ;; build-entry-ffi-primitive-initializer-forms : boolean? -> (listof datum?)
    ;;   Return FFI primitive initializers for the entry currently being built.
    (define (build-entry-ffi-primitive-initializer-forms _program-only?)
      (append
       (if (and (split-ffi-program-owned?)
                (split-ffi-sidecar-needs-initializer?))
           `((call ,split-ffi-sidecar-initializer-name))
           '())
       (if (or (not (current-split-runtime?))
               (split-ffi-program-owned?))
           (runtime-ffi-state-primitive-initializer-forms ffi-state)
           '())))

    ;; build-entry-function-forms : [#:program-only? boolean?] -> (listof datum?)
    ;;   Build the bootstrap helper and entry using the current split/monolithic
    ;;   initializer choices.
    (define (build-entry-function-forms #:program-only? [program-only? #f])
      (entry-function-forms
       entry-locals
       entry-body
       result
       #:flonum-initializer-forms entry-flonum-initializer-forms
       #:global-state-initializer-forms
       entry-global-state-initializer-forms
       #:runtime-constant-initializer-forms
       entry-runtime-constant-initializer-forms
       #:parameterization-initializer-forms
       entry-parameterization-initializer-forms
       #:random-current-initializer-forms
       entry-random-current-initializer-forms
       #:thread-current-initializer-forms
       entry-thread-current-initializer-forms
       #:logger-current-initializer-forms
       entry-logger-current-initializer-forms
       #:custodian-current-initializer-forms
       entry-custodian-current-initializer-forms
       #:event-initializer-forms entry-event-initializer-forms
       #:struct-property-initializer-forms
       entry-struct-property-initializer-forms
       #:path-current-initializer-forms
       entry-path-current-initializer-forms
       #:environment-parameter-initializer-forms
       entry-environment-parameter-initializer-forms
       #:character-general-category-initializer-forms
       entry-character-general-category-initializer-forms
       #:realm-initializer-forms entry-realm-initializer-forms
       #:primitive-initializer-forms
       entry-primitive-initializer-forms
       #:ffi-primitive-initializer-forms
       (build-entry-ffi-primitive-initializer-forms program-only?)
       #:unsafe-ffi-primitive-table-initializer-forms
       entry-unsafe-ffi-primitive-table-initializer-forms
       #:security-guard-current-initializer-forms
       entry-security-guard-current-initializer-forms
       #:error-value-handler-late-initializer-forms
       entry-error-value-handler-late-initializer-forms
       #:print-read-parameter-late-initializer-forms
       entry-print-read-parameter-late-initializer-forms
       #:io-handler-default-port-late-initializer-forms
       entry-io-handler-default-port-late-initializer-forms
       #:error-syntax-handler-late-initializer-forms
       entry-error-syntax-handler-late-initializer-forms
       #:top-vars top-vars
       #:exception-struct-type-initializer-forms
       exception-struct-type-initializer-forms
       #:namespace-initializer-forms namespace-initializer-forms))

    ;; split-program-entry-support-form? : datum? -> boolean?
    ;;   Keep only entry support functions that belong in the program module.
    (define (split-program-entry-support-form? form)
      (define name (module-func-name form))
      (and name (memq name program-driver-function-names)))

    ;; build-split-program-forms : -> (listof datum?)
    ;;   Build only program-owned forms for a warm reference-only split compile.
    (define (build-split-program-forms)
      (append
       js-host-import-forms
       (if (split-ffi-program-owned?)
           (runtime-ffi-state-active-imports-wat ffi-state)
           '())
       (if (split-ffi-program-owned?)
           (runtime-ffi-state-active-funcs-wat ffi-state)
           '())
       (runtime-constant-declaration-forms program-only-constant-registry)
       top-level-variable-declarations
       (if (split-ffi-program-owned?)
           (runtime-ffi-state-primitive-global-forms ffi-state)
           '())
       split-program-primitive-global-forms
       dls
       (if console-bridge-enabled?
           (ffi-console-bridge-helper-forms console-bridge-bindings)
           '())
       (for/list ([form (in-list entry-support-forms)]
                  #:when (split-program-entry-support-form? form))
         form)
       (build-entry-function-forms #:program-only? #t)))

    ;;;
    ;;; WEB-ASSEMBLY
    ;;;
    (define (build-runtime-module)
      `(module
         ,@(generate-shared-types)

         ;;;
         ;;; MEMORY
         ;;;

         ,@memory-import-forms

         ;;;
         ;;; IMPORTS
         ;;;

         ;  Imported functions from the host (JavaScript).
         ;  Note all imports must appear before other function definitions.

         ,@js-host-import-forms

         ;; FFI imports, generated wrappers, and unsafe primitive lookup helpers.
         ,@(cond
             [(or (split-ffi-sidecar?)
                  (split-ffi-program-owned?))
              (runtime-ffi-state-unsafe-primitive-helper-forms
               ffi-state
               #:unsafe-ffi-foreigns '())]
             [else
              (runtime-ffi-state-import-wrapper-helper-forms ffi-state)])

         ;;;
         ;;; Exceptions
         ;;;

         ;;   Bootstrap error helpers are implemented in runtime-wasm/core/exceptions.rkt.
         ,@(bootstrap-error-runtime-helper-forms runtime-helper-index)

         ;;   Multiple-value expectation helpers are implemented in runtime-wasm/core/control-flow.rkt.
         ,@(value-expectation-runtime-helper-forms runtime-helper-index
                                                   core-runtime-helper-index)

         ;;   Unquoted printing string helpers are implemented in runtime-wasm/core/formatting.rkt.
         ,@(unquoted-printing-string-core-runtime-helper-forms core-runtime-helper-index)
         ,@(unquoted-printing-string-runtime-helper-forms runtime-helper-index)

         ,@singleton-global-forms

         ,@fixnum-global-forms

         ,@flonum-global-forms

         ;; Constants used by the program and runtime.
         ,@(runtime-constant-declaration-forms runtime-constant-registry)

         ,@character-general-category-global-forms

         ,@realm-global-forms

         ,@exception-struct-type-cache-global-forms

         ,@location-global-forms

         ,@procedure-global-forms

         ,@syntax-global-forms

         ,@namespace-module-registry-global-forms

         ;; Namespaces
         ,@namespace-top-level-global-forms

         ,@ffi-callback-global-forms

         ;; Primitives (as values)
         ,@(runtime-primitive-state-global-forms primitive-state)
         ,@(if (and (split-ffi-sidecar?)
                    (not (split-ffi-program-owned?)))
               '()
               (runtime-ffi-state-primitive-global-forms ffi-state))

         ,@closure-empty-free-global-forms

         ;; Procedure and primitive invocation helpers
         ;;   Implemented in runtime-wasm/core/procedures.rkt.
         ,@(procedure-primitive-error-core-runtime-helper-forms core-runtime-helper-index)
         ,@(procedure-invocation-error-core-runtime-helper-forms core-runtime-helper-index)
         ,@(procedure-primitive-dispatch-core-runtime-helper-forms core-runtime-helper-index)
         ,@(procedure-invoker-core-runtime-helper-forms core-runtime-helper-index)
         ,@(procedure-bootstrap-runtime-helper-forms runtime-helper-index)

         ,@(procedure-primitive-invoker-core-runtime-helper-forms core-runtime-helper-index)

         ;;   Procedure case-lambda helpers are implemented in runtime-wasm/core/procedures.rkt.
         ,@(procedure-case-lambda-core-runtime-helper-forms core-runtime-helper-index)
         ,@(procedure-post-primitive-invoker-runtime-helper-forms runtime-helper-index)

         ;;   Struct procedure invoker is implemented in runtime-wasm/core/structs.rkt.
         ,@(struct-procedure-invoker-runtime-helper-forms runtime-helper-index)

         ;;   Procedure rest-argument helpers are implemented in runtime-wasm/core/procedures.rkt.
         ,@(procedure-rest-argument-core-runtime-helper-forms core-runtime-helper-index)

         ;;   Source-linklet interpreter helpers are implemented in runtime-wasm/core/linklets.rkt.
         ,@(linklet-interpreter-core-runtime-helper-forms core-runtime-helper-index)
         ,@(linklet-interpreter-runtime-helper-forms runtime-helper-index)

         ,@closure-runtime-global-forms

         ;; Predefined exception struct type descriptors (as boxed top-level bindings)
         ;;   struct:exn, struct:exn:fail, etc.
         ,@exception-struct-type-global-forms

         ;; Return value (for a module)
         ,@(entry-result-global-forms result)

         ;; Variables defined at the top-level
         ,@top-level-variable-declarations

         ,@symbol-table-global-forms

         ,@keyword-table-global-forms

         ,@struct-prefab-registry-global-forms

         ,@linklet-primitive-table-global-forms

         ,@unsafe-ffi-primitive-table-global-forms

         ;;   Symbol table initializer is implemented in runtime-wasm/core/symbols.rkt.
         ,@(symbol-table-initializer-runtime-helper-forms runtime-helper-index)

         ;;   Keyword table initializer is implemented in runtime-wasm/core/keywords.rkt.
         ,@(keyword-table-initializer-runtime-helper-forms runtime-helper-index)

         ;;   Primitive table initializer is implemented in runtime-wasm/core/linklets.rkt.
         ,@(linklet-primitive-table-initializer-runtime-helper-forms runtime-helper-index)

         ;;;
         ;;; Arrays
         ;;;

         ;; Arrays are "vectors" of (ref eq) values.
         ;; They are used internally to implement various Racket data structures.

         ;; Define a mutable array type of (ref eq)
         ; (type $Array (array (mut (ref eq))))

         ,@(array-core-runtime-helper-forms core-runtime-helper-index)

         ;;   Generic array conversion helpers are implemented in runtime-wasm/core/arrays.rkt.
         ,@(array-runtime-helper-forms runtime-helper-index)

         ;;;
         ;;; Growable Arrays
         ;;;

         ,@(growable-array-core-runtime-helper-forms core-runtime-helper-index)

         ;;;
         ;;; Growable Arrays of Bytes
         ;;;

         ,@(growable-bytes-core-runtime-helper-forms core-runtime-helper-index)

         ;;;
         ;;; Growable Arrays of I32
         ;;;

         ,@(growable-i32-array-core-runtime-helper-forms core-runtime-helper-index)


         ;;;
         ;;; I32Array - Arrays of i32
         ;;;

         ; (type $I32Array (array (mut i32)))

         ,@(i32-array-core-runtime-helper-forms core-runtime-helper-index)

         ;;   I32 array conversion helpers are implemented in runtime-wasm/core/i32-arrays.rkt.
         ,@(i32-array-runtime-helper-forms runtime-helper-index)


         ;;;
         ;;; I8Array
         ;;;

         ; (type $I8Array (array (mut i8)))

         ;;   I8 array error helpers are implemented in runtime-wasm/core/i8-arrays.rkt.
         ,@(i8-array-error-runtime-helper-forms runtime-helper-index)

         ,@(i8-array-core-runtime-helper-forms core-runtime-helper-index)

         ;;   I8 array conversion helpers are implemented in runtime-wasm/core/i8-arrays.rkt.
         ,@(i8-array-conversion-runtime-helper-forms runtime-helper-index)

         ;;   I8 array byte-wrapper core helpers are implemented in runtime-wasm/core/i8-arrays.rkt.
         ,@(i8-array-byte-core-runtime-helper-forms core-runtime-helper-index)

         ;;;
         ;;; Exceptions
         ;;;

         ,@exception-tag-forms

         ;;   Exception helpers are implemented in runtime-wasm/core/exceptions.rkt.
         ,@(exception-runtime-helper-forms runtime-helper-index)

         ;;;
         ;;; RUNTIME SUPPORT FOR MATCH AND CHECKERS
         ;;;

         ;;   Implemented in runtime-wasm/core/checkers.rkt.
         ,@(checker-runtime-helper-forms runtime-helper-index)

         ;;;
         ;;; DATATYPES
         ;;;

         ;; https://docs.racket-lang.org/reference/data.html

         ;;;
         ;;;  4.1 Equality
         ;;;

         ;;   Bignum helpers are implemented in runtime-wasm/core/bignums.rkt.
         ,@(bignum-core-runtime-helper-forms core-runtime-helper-index)

         ;;   Rational helpers are implemented in runtime-wasm/core/rationals.rkt.
         ,@(rational-core-runtime-helper-forms core-runtime-helper-index)

         ;;   Implemented in runtime-wasm/core/equality.rkt.
         ,@(equality-core-runtime-helper-forms core-runtime-helper-index)
         ,@(equality-runtime-helper-forms runtime-helper-index)

         ;;;
         ;;; 4.2 Booleans
         ;;;

         ;; https://docs.racket-lang.org/reference/booleans.html

         ;;   String mutability predicates are implemented in runtime-wasm/core/strings.rkt.
         ,@(string-mutability-runtime-helper-forms runtime-helper-index)

         ;;   Bytes mutability predicates are implemented in runtime-wasm/core/bytes.rkt.
         ,@(bytes-mutability-runtime-helper-forms runtime-helper-index)

         ;;   Vector mutability predicates are implemented in runtime-wasm/core/vectors.rkt.
         ,@(vector-mutability-runtime-helper-forms runtime-helper-index)

         ;;   Box mutability predicates are implemented in runtime-wasm/core/boxes.rkt.
         ,@(box-mutability-runtime-helper-forms runtime-helper-index)

         ;;   Hash predicates are implemented in runtime-wasm/core/hashes.rkt.
         ,@(hash-predicate-core-runtime-helper-forms core-runtime-helper-index)
         ,@(hash-predicate-runtime-helper-forms runtime-helper-index)

         ;;   Boolean helpers are implemented in runtime-wasm/core/booleans.rkt.
         ,@(boolean-runtime-helper-forms runtime-helper-index)

         ;;   Generic immutability predicates are implemented in runtime-wasm/core/equality.rkt.
         ,@(immutable-predicate-core-runtime-helper-forms core-runtime-helper-index)
         ,@(immutable-predicate-runtime-helper-forms runtime-helper-index)

         ;;;
         ;;; 4.3 Numbers
         ;;;

         ;; https://docs.racket-lang.org/reference/numbers.html

         ;;; 4.3.1 Number Types

         ;; https://docs.racket-lang.org/reference/number-types.html

         ;; [x] number?
         ;; [ ] complex?
         ;; [x] real?
         ;; [ ] rational?
         ;; [x] integer?
         ;; [x] exact-integer?
         ;; [x] exact-nonnegative-integer?
         ;; [x] exact-positive-integer?
         ;; [x] positive-integer?
         ;; [x] negative-integer?
         ;; [x] nonpositive-integer?
         ;; [x] nonnegative-integer?
         ;; [x] natural?
         ;; [x] nan?
         ;; [x] infinite?
         ;; [x] inexact-real?
         ;; [x] fixnum?
         ;; [x] flonum?
         ;; [x] double-flonum?
         ;; [x] single-flonum?
         ;; [x] single-flonum-available?
         ;; [x] zero?
         ;; [x] positive?
         ;; [x] negative?
         ;; [x] even?
         ;; [x] odd?
         ;; [x] exact?
         ;; [x] inexact?
         ;; [x] inexact->exact
         ;; [x] exact->inexact
         ;; [ ] real->single-flonum
         ;; [x] real->double-flonum

         ;;   Number-type helpers are implemented in runtime-wasm/core/numbers.rkt.
         ,@(number-type-runtime-helper-forms runtime-helper-index)

         ;; 4.3.2 Generic Numerics
         ;;     https://docs.racket-lang.org/reference/generic-numbers.html

         ;; 4.3.2.1 Arithmetic
         ;;   https://docs.racket-lang.org/reference/generic-numbers.html#%28part._.Arithmetic%29

         ;;   Generic numeric helpers are implemented in runtime-wasm/core/numbers.rkt.
         ,@(generic-numeric-runtime-helper-forms runtime-helper-index)

         ;;   Numeric error helpers are implemented in runtime-wasm/core/numbers.rkt.
         ,@(number-error-runtime-helper-forms runtime-helper-index)

         ;;   Arithmetic and comparison helpers are implemented in runtime-wasm/core/numbers.rkt.
         ,@(generic-arithmetic-runtime-helper-forms runtime-helper-index)

         ;;   Bitwise numeric helpers are implemented in runtime-wasm/core/numbers.rkt.
         ,@(generic-bitwise-runtime-helper-forms runtime-helper-index)

        ,@random-state-global-forms

        ;;   Random generator helpers are implemented in runtime-wasm/core/random.rkt.
        ,@(random-core-runtime-helper-forms core-runtime-helper-index)
        ,@(random-generator-runtime-helper-forms runtime-helper-index)

        ;;   Implemented in runtime-wasm/core/parameters.rkt.
        ,@(current-pseudo-random-generator-core-runtime-helper-forms core-runtime-helper-index)
        ,@(current-pseudo-random-generator-runtime-helper-forms runtime-helper-index)

        ;;   Public random helpers are implemented in runtime-wasm/core/random.rkt.
        ,@(random-public-runtime-helper-forms runtime-helper-index)

         ;; System/runtime information helpers are implemented in runtime-wasm/core/system.rkt.
         ,@(system-runtime-helper-forms runtime-helper-index)

         ;;   Implemented in runtime-wasm/core/parameters.rkt.
         ,@(compile-parameter-core-runtime-helper-forms core-runtime-helper-index)
         ,@(compile-parameter-runtime-helper-forms runtime-helper-index)

         ;;;
         ;;;  4.3.4 Fixnums
         ;;;

         ;; https://docs.racket-lang.org/reference/fixnums.html

         ;; Fixnum helpers are implemented in runtime-wasm/core/fixnums.rkt.
         ,@(fixnum-runtime-helper-forms runtime-helper-index core-runtime-helper-index)

         ;;;
         ;;; 4.3.3 Floating Point Numbers
         ;;;

         ;; https://docs.racket-lang.org/reference/flonums.html

         ;; Flonum helpers are implemented in runtime-wasm/core/flonums.rkt.
         ,@(flonum-runtime-helper-forms runtime-helper-index core-runtime-helper-index)



         ;;;
         ;;; Number / String conversions
         ;;;

         ;; Number/string conversion helpers are implemented in runtime-wasm/core/numbers.rkt.
         ,@(number-string-conversion-runtime-helper-forms runtime-helper-index)

         ;; String/number conversion helpers are implemented in runtime-wasm/core/numbers.rkt.
         ,@(string-number-conversion-runtime-helper-forms runtime-helper-index)

         ;; Floating-point byte conversion helpers are implemented in runtime-wasm/core/numbers.rkt.
         ,@(floating-point-byte-conversion-runtime-helper-forms runtime-helper-index)


         ;; Numeric formatting helpers are implemented in runtime-wasm/core/numbers.rkt.
         ,@(numeric-formatting-runtime-helper-forms runtime-helper-index)

         ;; Numeric display helpers are implemented in runtime-wasm/core/formatting.rkt.
         ,@(numeric-display-formatting-runtime-helper-forms runtime-helper-index)



        ;;;
        ;;; - External host values
        ;;;

         ;;   Implemented in runtime-wasm/core/ffi.rkt.
         ,@(external-value-core-runtime-helper-forms core-runtime-helper-index)
         ,@(external-value-runtime-helper-forms runtime-helper-index)





        ;;;
        ;;; 4.3 Byte Strings
        ;;;

        ;; https://docs.racket-lang.org/reference/bytestrings.html

         ;;   Byte-string helpers are implemented in runtime-wasm/core/bytes.rkt.
         ,@(bytes-main-runtime-helper-forms runtime-helper-index)


        ;;;
        ;;; 4.4 Strings
        ;;;

        ;; https://docs.racket-lang.org/reference/strings.html

         ;;   String helpers are implemented in runtime-wasm/core/strings.rkt.
         ,@(string-main-runtime-helper-forms runtime-helper-index core-runtime-helper-index)


         ;;;
         ;;; 4.6 Characters
         ;;;

         ;;   Character helpers are implemented in runtime-wasm/core/characters.rkt.
         ,@(character-runtime-helper-forms runtime-helper-index)

         ;;;
         ;;; 4.7 SYMBOLS
         ;;;

         ;;   Symbol helpers are implemented in runtime-wasm/core/symbols.rkt.
         ,@(symbol-public-runtime-helper-forms runtime-helper-index)

         ,@symbol-gensym-global-forms

         ;;   Gensym helpers are implemented in runtime-wasm/core/symbols.rkt.
         ,@(symbol-gensym-core-runtime-helper-forms core-runtime-helper-index)
         ,@(symbol-gensym-runtime-helper-forms runtime-helper-index)

         ;;;
         ;;; 4.8 REGULAR EXPRESSIONS
         ;;;

         ;;   Regular expression helpers are implemented in runtime-wasm/core/regexps.rkt.
         ,@(regexp-runtime-helper-forms runtime-helper-index)

         ;;;
         ;;; 4.9 KEYWORDS
         ;;;

         ;;   Keyword helpers are implemented in runtime-wasm/core/keywords.rkt.
         ,@(keyword-core-runtime-helper-forms core-runtime-helper-index)
         ,@(keyword-runtime-helper-forms runtime-helper-index)

         ;;;
         ;;; 4.10 Pairs and lists
         ;;;

         ;; https://docs.racket-lang.org/reference/pairs.html

         ,@pair-global-forms

         ;; Pair/list helpers
         ;;   Implemented in runtime-wasm/core/lists.rkt.
         ,@(list-pre-contract-runtime-helper-forms runtime-helper-index
                                                   core-runtime-helper-index)

         ;; Generic contract-error helpers
         ;;   Implemented in runtime-wasm/core/exceptions.rkt.
         ,@(exception-contract-runtime-helper-forms runtime-helper-index)

         ,@(list-post-contract-runtime-helper-forms runtime-helper-index
                                                    core-runtime-helper-index)

         ;;;
         ;;; 4.11 Mutable Pairs and lists
         ;;;

         ;; https://docs.racket-lang.org/reference/mpairs.html

         ;; Mutable pairs are similar to pairs but allow their fields
         ;; to be updated. These operations parallel their immutable
         ;; counterparts above but use the `$MPair` type.

         ;; Mutable-pair helpers are implemented in runtime-wasm/core/mlists.rkt.
         ,@(mlist-core-runtime-helper-forms core-runtime-helper-index)
         ,@(mlist-runtime-helper-forms runtime-helper-index)

         ;;;
         ;;; 4.12 Vectors
         ;;;

         ;; https://docs.racket-lang.org/reference/vectors.html

         ;; (type $Vector (sub $Heap
         ;;                      (struct
         ;;                        (field $hash (mut i32))
         ;;                        (field (ref $Array))))))

         ,@treelist-constant-global-forms
         ,@vector-dummy-array-global-forms
         ,@treelist-empty-global-forms
         ,@vector-dummy-global-forms

         ;; Vector related exceptions
         ;;   Implemented in runtime-wasm/core/vectors.rkt.
         ,@(vector-error-runtime-helper-forms runtime-helper-index)

         ;; Treelist helpers are implemented in runtime-wasm/core/treelists.rkt.
         ,@(treelist-core-runtime-helper-forms core-runtime-helper-index)
         ,@(treelist-runtime-helper-forms runtime-helper-index)

         ;; Vector helpers
         ;;   Implemented in runtime-wasm/core/vectors.rkt.
         ,@(vector-main-runtime-helper-forms runtime-helper-index core-runtime-helper-index)

         ;;;
         ;;; Stencil Vectors
         ;;;

         ;; Stencil-vector helpers are implemented in runtime-wasm/core/stencil-vectors.rkt.
         ,@(stencil-vector-core-runtime-helper-forms core-runtime-helper-index)
         ,@(stencil-vector-runtime-helper-forms runtime-helper-index)

         ;; Vector copy, conversion, slicing, search, and sorting helpers
         ;;   Implemented in runtime-wasm/core/vectors.rkt.
         ,@(vector-post-stencil-runtime-helper-forms runtime-helper-index)

         ;; Box helpers are implemented in runtime-wasm/core/boxes.rkt.
         ,@(box-runtime-helper-forms runtime-helper-index)

         ;; Thread helpers are implemented in runtime-wasm/core/threads.rkt.
         ,@(thread-core-runtime-helper-forms core-runtime-helper-index)
         ,@(thread-runtime-helper-forms runtime-helper-index)

         ;; Logger helpers are implemented in runtime-wasm/core/loggers.rkt.
         ,@(logger-core-runtime-helper-forms core-runtime-helper-index)
         ,@(logger-runtime-helper-forms runtime-helper-index)

         ;; Event and channel helpers are implemented in runtime-wasm/core/events.rkt.
         ,@(event-core-runtime-helper-forms core-runtime-helper-index)
         ,@(event-channel-runtime-helper-forms runtime-helper-index)

         ;; Semaphore helpers are implemented in runtime-wasm/core/semaphores.rkt.
         ,@(semaphore-core-runtime-helper-forms core-runtime-helper-index)
         ,@(semaphore-runtime-helper-forms runtime-helper-index)

         ;; Will-executor helpers are implemented in runtime-wasm/core/wills.rkt.
         ,@(will-core-runtime-helper-forms core-runtime-helper-index)
         ,@(will-runtime-helper-forms runtime-helper-index)

         ;; Event synchronization helpers are implemented in runtime-wasm/core/events.rkt.
         ,@(event-sync-runtime-helper-forms runtime-helper-index)

         ;; Uninterruptible-lock helpers are implemented in runtime-wasm/core/locks.rkt.
         ,@(lock-runtime-helper-forms runtime-helper-index)

         ,@memory-map-global-forms


         ;;;
         ;;; String Buffer
         ;;;

         ;;   String-buffer error helpers are implemented in runtime-wasm/core/strings.rkt.
         ,@(string-buffer-runtime-helper-forms runtime-helper-index)

         ;;;
         ;;; HASH TABLES
         ;;;

         ;; Racket contains quite a few types of hash tables.
         ;;
         ;;   eq?/eqv?/equal?/equal-always?    mutable/immutable    strong/weak/ephemerons
         ;;
         ;; Currently `webracket` supports mutable, strong hash tables only.


         ;; Hash table predicates
         ;;   Implemented in runtime-wasm/core/hashes.rkt.
         ,@(hash-table-predicate-core-runtime-helper-forms core-runtime-helper-index)
         ,@(hash-table-predicate-runtime-helper-forms runtime-helper-index)


         ;;;
         ;;; MUTABLE HASHEQ
         ;;;

         ,@(mutable-hash-constructor-runtime-helper-forms runtime-helper-index)

         ;;;
         ;;; IMMUTABLE HASHEQV
         ;;;

         ,@(immutable-hash-prelude-runtime-helper-forms runtime-helper-index)

         ;; Source-linklet bootstrap helpers
         ;;   Implemented in runtime-wasm/core/linklets.rkt.
         ,@(linklet-bootstrap-core-runtime-helper-forms core-runtime-helper-index)
         ,@(linklet-bootstrap-runtime-helper-forms runtime-helper-index)

         ;; Primitive-table bridge helpers
         ;;   Implemented in runtime-wasm/core/linklets.rkt.
         ,@(linklet-primitive-table-runtime-only-helper-forms
            split-runtime-described-primitives-sorted
            primitive-table-module-symbols)

         ;; Prefab struct registry helpers
         ;;   Implemented in runtime-wasm/core/structs.rkt.
         ,@(struct-prefab-registry-runtime-helper-forms runtime-helper-index)

         ;; Mutable hash lookup helpers
         ;;   Implemented in runtime-wasm/core/mutable-hashes.rkt.
         ,@(mutable-hash-read-runtime-helper-forms runtime-helper-index)

         ;; Public immutable hash functional update wrappers
         ;;   Implemented in runtime-wasm/core/immutable-hashes.rkt.
         ,@(immutable-hash-public-set/remove-runtime-helper-forms runtime-helper-index)

         ;; Mutable hash write and ref! helpers
         ;;   Implemented in runtime-wasm/core/mutable-hashes.rkt.
         ,@(mutable-hash-write-runtime-helper-forms runtime-helper-index)

         ;; hash-update : immutable-hash? any/c (any/c -> any/c) [any/c or (-> any/c)] -> immutable-hash?
         ;;   Implemented in runtime-wasm/core/immutable-hashes.rkt.
         ,@(immutable-hash-public-update-runtime-helper-forms runtime-helper-index)

         ;; Mutable hash update! helpers
         ;;   Implemented in runtime-wasm/core/mutable-hashes.rkt.
         ,@(mutable-hash-update-runtime-helper-forms runtime-helper-index)

         ;; Mutable hash remove, resize, membership, and clear! helpers
         ;;   Implemented in runtime-wasm/core/mutable-hashes.rkt.
         ,@(mutable-hash-remove-membership-clear-runtime-helper-forms runtime-helper-index)

         ;; hash-clear : immutable-hash? -> immutable-hash?
         ;;   Implemented in runtime-wasm/core/immutable-hashes.rkt.
         ,@(immutable-hash-public-clear-runtime-helper-forms runtime-helper-index)

         ;; Mutable hash emptiness and iteration helpers
         ;;   Implemented in runtime-wasm/core/mutable-hashes.rkt.
         ,@(mutable-hash-empty-iteration-runtime-helper-forms runtime-helper-index)

         ;; unsafe-immutable-hash-iterate-* : immutable hash iteration
         ;;   Implemented in runtime-wasm/core/immutable-hashes.rkt.
         ,@(immutable-hash-iteration-runtime-helper-forms runtime-helper-index)

         ;; Generic hash table operations
         ;;   Implemented in runtime-wasm/core/hashes.rkt.
         ,@(hash-table-operation-runtime-helper-forms runtime-helper-index)

         ;;;
         ;;; HASH CODES
         ;;;

         ,@hash-seed-global-forms

         ;; Hash code helpers
         ;;   Implemented in runtime-wasm/core/hashes.rkt.
         ,@(hash-code-runtime-helper-forms runtime-helper-index)

         ;;;
         ;;; SYMBOL TABLE
         ;;;

         ;;   Implemented in runtime-wasm/core/symbols.rkt.
         ,@(symbol-table-runtime-helper-forms runtime-helper-index)

         ;; String hash helper is implemented in runtime-wasm/core/strings.rkt.
         ,@(string-hash-core-runtime-helper-forms core-runtime-helper-index)

         ;;;
         ;;; LOCATION
         ;;;

         ;; When counting lines, Racket treats linefeed, return, and
         ;; return-linefeed combinations as a line terminator and as a single
         ;; position (on all platforms).
         ;; Each tab advances the column count to one before the next multiple
         ;; of 8.
         ;; When a sequence of bytes in the range 128 to 253 forms a
         ;; UTF-8 encoding of a character, the position/column is incremented
         ;; once for each byte, and then decremented appropriately
         ;; when a complete encoding sequence is discovered.

         ;; (array line column pos)
         ;;   pos and line counts from 1
         ;;   column       counts from 0

         ;;   Implemented in runtime-wasm/core/locations.rkt.
         ,@(location-initial-runtime-helper-forms runtime-helper-index)

         ;;;
         ;;; 13.1.6  STRING PORT
         ;;;

         ;; https://docs.racket-lang.org/reference/ports.html

         ;; A string port reads or writes from a byte string. An input
         ;; string port can be created from either a byte string or a string;
         ;; in the latter case, the string is effectively converted to a byte
         ;; string using string->bytes/utf-8. An output string port collects
         ;; output into a byte string, but get-output-string conveniently converts
         ;; the accumulated bytes to a string.

         ;; Note: the index `idx` and the location position may be different,
         ;;       since the #\return#\newline combination counts as a
         ;;       single position.

         ;; [x] string-port?
         ;; [x] open-input-bytes
         ;; [x] open-input-string
         ;; [x] open-output-bytes
         ;; [x] open-output-string
         ;; [x] get-output-bytes
         ;; [x] get-output-string

         ;; [ ] write-byte

        ;; Note:
        ;;   Currently WebRacket only supports string ports. Distinguish
        ;;   between input and output ports based on the mutability of the
        ;;   underlying byte string: input ports store immutable bytes, while
        ;;   output ports keep a mutable byte buffer that grows as data is
        ;;   written.

        ;; Port predicate helpers are implemented in runtime-wasm/core/ports.rkt.
        ,@(port-predicate-runtime-helper-forms runtime-helper-index)

        ;;   Implemented in runtime-wasm/core/impersonators.rkt.
        ,@(impersonator-core-runtime-helper-forms core-runtime-helper-index)
        ,@(impersonator-basic-runtime-helper-forms runtime-helper-index)

        ;; Output-port predicate helper is implemented in runtime-wasm/core/ports.rkt.
        ,@(port-output-predicate-runtime-helper-forms runtime-helper-index)

        ;; Port state helpers are implemented in runtime-wasm/core/ports.rkt.
        ,@(port-state-runtime-helper-forms runtime-helper-index)

        ;; Port location helpers are implemented in runtime-wasm/core/ports.rkt.
        ,@(port-location-runtime-helper-forms runtime-helper-index)

         ;; String/byte string port constructors are implemented in runtime-wasm/core/ports.rkt.
         ,@(port-string-constructor-runtime-helper-forms runtime-helper-index)

         ;; Output string extraction helpers are implemented in runtime-wasm/core/ports.rkt.
         ,@(port-output-string-runtime-helper-forms runtime-helper-index)

         ;; Custom port constructors are implemented in runtime-wasm/core/ports.rkt.
         ,@(port-custom-constructor-runtime-helper-forms runtime-helper-index)

         ;;   EOF object helpers are implemented in runtime-wasm/core/immediates.rkt.
         ,@(eof-object-runtime-helper-forms runtime-helper-index)

         ;; call-with-output-string is implemented in runtime-wasm/core/ports.rkt.
         ,@(port-call-with-output-string-runtime-helper-forms runtime-helper-index)

         ;;;
         ;;;  13.2  Byte and String Input
         ;;;

         ;; Byte/string input helpers are implemented in runtime-wasm/core/ports.rkt.
         ,@(port-input-runtime-helper-forms runtime-helper-index)

         ;;;
         ;;;  13.3  Byte and String Output
         ;;;

         ;; (type $InputStringPort
         ;;     (sub $InputPort
         ;;       (struct
         ;;         (field $name  (ref eq))     ; the port name   (string)
         ;;         (field $bytes (ref eq))     ; the byte string (bytes)
         ;;         (field $len   i32)          ; the length of the string
         ;;         (field $idx   i32)          ; the current index into the string
         ;;         (field $loc   (ref $Location)))) ; the current location

         ;; Byte/string output helpers are implemented in runtime-wasm/core/ports.rkt.
         ,@(port-output-runtime-helper-forms runtime-helper-index)


         ;;;
         ;;; FFI Helpers, callbacks, and browser bridge
         ;;;

         ;;   Implemented in runtime-wasm/core/ffi.rkt.
         ,@(ffi-core-runtime-helper-forms core-runtime-helper-index)
         ,@(ffi-runtime-only-helper-forms console-bridge-bindings)
         ,@(if console-bridge-enabled?
               (ffi-console-bridge-helper-forms console-bridge-bindings)
               '())

         ;;;
         ;;; 5. STRUCTURES
         ;;;

         ;; TODO
         ;; [x] structure types and super structs
         ;; [x] auto fields
         ;; [/] prefab structures
         ;; [x] structure guards
         ;; [ ] applicable structures

         ;; 5.1 Defining Structure Types: struct
         ;; [/] struct                     [syntax]
         ;; [x] struct-field-index         [syntax]
         ;; [x] define-struct              [syntax]
         ;; [x] struct/derived             [syntax]
         ;; [x] define-struct/derived      [syntax]

         ;; 5.2 Creating Structure Types
         ;; [x] make-struct-type
         ;; [x] make-struct-field-accessor
         ;; [x] make-struct-field-mutator
         ;; [ ] prop:sealed                [value]
         ;;       - a structure type property
         ;;       - a sealed struct can not be a supertype of another structure type

         ;; 5.3 Structure Type Properties
         ;; [x] make-struct-type-property
         ;; [x] struct-type-property?
         ;; [x] struct-type-property-accessor-procedure?
         ;; [x] struct-type-property-predicate-procedure?

         ;; 5.4 Generic Interfaces (racket/generic)
         ;; ...

         ;; 5.5 Copying and Updating Structures
         ;; [ ] struct-copy      [syntax]

         ;; 5.6 Structure Utilities
         ;; [x] struct->vector
         ;; [x] struct?
         ;; [x] struct-type?
         ;; [x] struct-constructor-procedure?
         ;; [x] struct-predicate-procedure?
         ;; [x] struct-accessor-procedure?
         ;; [x] struct-mutator-procedure?
         ;; [x] prefab-struct-key
         ;; [x] make-prefab-struct
         ;; [x] prefab-struct-type-key+ field-count
         ;; [ ] prefab-key->struct-type
         ;; [x] prefab-key?

         ;; 5.6.1 Additional Structure Utilities
         ;; [ ] make-constructor-style-printer
         ;; [x] struct->list

         ,@(struct-core-predicate-runtime-helper-forms core-runtime-helper-index)

         ;; Basic struct predicates and checkers
         ;;   Implemented in runtime-wasm/core/structs.rkt.
         ,@(struct-basic-runtime-helper-forms runtime-helper-index)

         ;; > (topexpand  #`(let () (struct foo (bar)) (foo 11)))
         ;; #<syntax:racket-mode-repl::15
         ;; (let-values ()
         ;;   (let-values (((struct:foo foo1 foo? foo-bar)
         ;;                 (let-values (((struct: make- ? -ref -set!)
         ;;                               (let-values ()
         ;;                                 (let-values ()
         ;;                                   (#%app make-struct-type 'foo '#f '1 '0 '#f null (#%app current-inspector) '#f '(0) '#f 'foo)))))
         ;;                   (#%app values struct: make- ? (#%app make-struct-field-accessor -ref '0 'bar)))))
         ;;     (#%app foo1 '11)))

         ;; compiler.rkt>
         ;; (make-struct-type 'foo    ; name
         ;;                   '#f     ; no super
         ;;                   '1      ; init-field-count
         ;;                   '0      ; auto-field-cnt
         ;; optional:
         ;;                   '#f     ; auto-v
         ;;                   null    ; props
         ;;                   (current-inspector) ; inspector
         ;;                   '#f     ; proc-spec
         ;;                   '(0)    ; immutables
         ;;                   '#f     ; guard
         ;;                   'foo))) ; constructor-name


         ;; $make-struct-type
         ;;   returns 5 values:
         ;;     struct-type?
         ;;     struct-constructor-procedure?
         ;;     struct-predicate-procedure?
         ;;     struct-accessor-procedure?
         ;;     struct-mutator-procedure?

         ;; Initial prefab helpers
         ;;   Implemented in runtime-wasm/core/structs.rkt.
         ,@(struct-prefab-runtime-helper-forms runtime-helper-index)

         ;; Struct type construction helpers
         ;;   Implemented in runtime-wasm/core/structs.rkt.
         ,@(struct-construction-runtime-helper-forms runtime-helper-index)

         ,@(runtime-function-declaration-forms primitive-shapes
                                               split-runtime-described-primitives-sorted
                                               (runtime-ffi-state-active-primitive-names ffi-state))

         ;; Struct constructor implementation helpers
         ;;   Implemented in runtime-wasm/core/structs.rkt.
         ,@(struct-constructor-access-runtime-helper-forms runtime-helper-index)

         ;; -------------------------------------------------------------------
         ;; Structure Type Properties
         ;; -------------------------------------------------------------------

         ;; Struct type property lookup helpers
         ;;   Implemented in runtime-wasm/core/structs.rkt.
         ,@(struct-type-property-runtime-helper-forms runtime-helper-index)


         ;;;
         ;;; PROCEDURES
         ;;;

         ;; Struct procedure helpers
         ;;   Implemented in runtime-wasm/core/structs.rkt.
         ,@(struct-procedure-runtime-helper-forms runtime-helper-index)

         ;; Procedure helpers
         ;;   Implemented in runtime-wasm/core/procedures.rkt.
         ,@(procedure-main-runtime-helper-forms runtime-helper-index)

         ;;   Implemented in runtime-wasm/core/impersonators.rkt.
         ,@(chaperone-procedure-runtime-helper-forms runtime-helper-index)

         ;; Procedure arity mask helpers
         ;;   Implemented in runtime-wasm/core/procedures.rkt.
         ,@(procedure-arity-mask-runtime-helper-forms runtime-helper-index)


         ;;;
         ;;; PRIMITIVES
         ;;;

         ; https://docs.racket-lang.org/reference/procedures.html

         ; 4.20.2 Reflecting on Primitives

         ;; Primitive reflection helpers
         ;;   Implemented in runtime-wasm/core/procedures.rkt.
         ,@(primitive-reflection-runtime-helper-forms runtime-helper-index)

         ;;;
         ;;;  4.21 Void
         ;;;

         ;; https://docs.racket-lang.org/reference/void.html

         ;;   Implemented in runtime-wasm/core/immediates.rkt.
         ,@(void-runtime-helper-forms runtime-helper-index)


         ;;;
         ;;; 10. CONTROL FLOW
         ;;;

         ;; 10.1 Multiple Values

         ;;   Implemented in runtime-wasm/core/control-flow.rkt.
         ,@(multiple-value-runtime-helper-forms runtime-helper-index
                                                core-runtime-helper-index)



         ;; 10.2 Exceptions

         ;; 10.3 Delayed Evaluation
         ;; 10.4 Continuations

         ;;   Implemented in runtime-wasm/core/control-flow.rkt.
         ,@(continuation-runtime-helper-forms runtime-helper-index
                                              core-runtime-helper-index)

         ;; 10.5 Continuation Marks

         ;;   Implemented in runtime-wasm/core/control-flow.rkt.
         ,@(continuation-mark-runtime-helper-forms runtime-helper-index
                                                   core-runtime-helper-index)

         ;;   Implemented in runtime-wasm/core/parameters.rkt.
         ,@(parameterization-runtime-helper-forms runtime-helper-index)

         ;;   Implemented in runtime-wasm/core/parameters.rkt.
         ,@(namespace-parameter-core-runtime-helper-forms core-runtime-helper-index)

         ;;   Implemented in runtime-wasm/core/parameters.rkt.
         ,@(namespace-parameter-runtime-helper-forms runtime-helper-index)

         ;;   Implemented in runtime-wasm/core/parameters.rkt.
         ,@(parameterization-binding-core-runtime-helper-forms core-runtime-helper-index)

         ;;   Implemented in runtime-wasm/core/parameters.rkt.
         ,@(parameterization-binding-runtime-helper-forms runtime-helper-index)

         ;;   Implemented in runtime-wasm/core/parameters.rkt.
         ,@(parameterization-call-runtime-helper-forms runtime-helper-index)

         ;; 10.6 Breaks
         ;; 10.7 Exiting
         ;; 10.8 Unreachable Expressions


         ;;;
         ;;; 13. INPUT AND OUTPUT
         ;;;

         ;; 13.8 Printer Extension

         ;;   Implemented in runtime-wasm/core/formatting.rkt.
         ,@(printer-extension-runtime-helper-forms runtime-helper-index)


         ;; 13.10 Fast-Load Serialization

         ;;   FASL tag globals are implemented in runtime-wasm/core/fasl.rkt.
         ,@(fasl-tag-runtime-helper-forms runtime-helper-index)

         ;;   FASL encoding helpers are implemented in runtime-wasm/core/fasl.rkt.
         ,@(fasl-encoding-runtime-helper-forms runtime-helper-index)

         ;;; Fasl decoding (from byte string)

         ;;   FASL decoding helpers are implemented in runtime-wasm/core/fasl.rkt.
         ,@(fasl-decoding-runtime-helper-forms runtime-helper-index)

        ;;;
        ;;; 14. REFLECTION AND SECURITY
        ;;;

        ;; 14.1 Namespaces

        ;;   Reflection helpers are implemented in runtime-wasm/core/reflection.rkt.
        ,@(reflection-core-runtime-helper-forms core-runtime-helper-index)
        ,@(reflection-runtime-helper-forms runtime-helper-index)
         ;; 14.2 Evaluation and compilation
         ;; 14.3 The racket/load language
         ;; 14.4 Module names and loading
         ;; 14.5 Impersonators and chaperones
         ;; 14.6 Security Guards
         ;; 14.7 Custodians

         ;;   Security guard helpers are implemented in runtime-wasm/core/security.rkt.
         ,@(security-core-runtime-helper-forms core-runtime-helper-index)
         ,@(security-pre-parameter-runtime-helper-forms runtime-helper-index)

         ;;   Implemented in runtime-wasm/core/parameters.rkt.
         ,@(current-security-guard-core-runtime-helper-forms core-runtime-helper-index)
         ,@(current-security-guard-runtime-helper-forms runtime-helper-index)

         ;;   Security guard and custodian predicate helpers are implemented in runtime-wasm/core/security.rkt.
         ,@(security-post-parameter-runtime-helper-forms runtime-helper-index)

         ;;   Implemented in runtime-wasm/core/parameters.rkt.
         ,@(current-custodian-core-runtime-helper-forms core-runtime-helper-index)
         ,@(current-custodian-runtime-helper-forms runtime-helper-index)

         ;;   Custodian helpers are implemented in runtime-wasm/core/security.rkt.
         ,@(custodian-runtime-helper-forms runtime-helper-index)

         ;; 14.8 Thread Groups
         ;; 14.9 Structure Inspectors

         ;;   Implemented in runtime-wasm/core/time.rkt.
         ,@(time-core-runtime-helper-forms core-runtime-helper-index)
         ,@(time-runtime-helper-forms runtime-helper-index)

         ;;   Inspector and object-name helpers are implemented in runtime-wasm/core/security.rkt.
         ,@(inspector-runtime-helper-forms runtime-helper-index)

         ;; 14.10 Code Inspectors
         ;; 14.11 Plumbers
         ;; 14.12 Sandboxed Evaluation
         ;; 14.13 The racket/repl library

         ;;;
         ;;; 14.14 Linklets and the compiler
         ;;;


         ;;   Linklet instance helpers are implemented in runtime-wasm/core/linklet-instances.rkt.
         ,@(linklet-instance-core-runtime-helper-forms core-runtime-helper-index)
         ,@(linklet-instance-runtime-helper-forms runtime-helper-index)

         ;;   Linklet helpers are implemented in runtime-wasm/core/linklets.rkt.
         ,@(linklet-runtime-only-helper-forms linklet-body-reserved-symbols)

         ;;   Correlated syntax helpers are implemented in runtime-wasm/core/syntax.rkt.
         ,@(correlated-syntax-runtime-helper-forms runtime-helper-index)

         ;;;
         ;;; 17. UNSAFE OPERATIONS
         ;;;

         ;;   Implemented in runtime-wasm/core/unsafe.rkt.
         ,@(unsafe-runtime-helper-forms runtime-helper-index)

         ;;;
         ;;; FORMATTING - DISPLAY MODE
         ;;;

         ;;   I8 array/string conversion helper is implemented in runtime-wasm/core/i8-arrays.rkt.
         ,@(i8-array-string-conversion-runtime-helper-forms runtime-helper-index)

         ;;   Implemented in runtime-wasm/core/formatting.rkt.
         ,@(display-formatting-core-runtime-helper-forms core-runtime-helper-index)
         ,@(formatting-runtime-helper-forms runtime-helper-index)

               ;;;
               ;;; 14. REFLECTION AND SECURITY
               ;;;

               ;; 14.1 Namespaces

               ;;   Implemented in runtime-wasm/core/namespaces.rkt.
               ,@(namespace-runtime-helper-forms runtime-helper-index)

               ;;;
               ;;; 14.2 Source Locations
               ;;;

               ;;   Implemented in runtime-wasm/core/locations.rkt.
               ,@(source-location-core-runtime-helper-forms core-runtime-helper-index)
               ,@(source-location-runtime-helper-forms runtime-helper-index)

               ;;;
               ;;; 12. Macros
               ;;;

               ;;; 12.2 Syntax Object COntent

               ;;   Implemented in runtime-wasm/core/syntax.rkt.
               ,@(syntax-runtime-helper-forms runtime-helper-index)

               ;;;
               ;;; 15. OPERATING SYSTEM
               ;;;

               ;;; 15.1 Paths

               ;;   Basic path helpers are implemented in runtime-wasm/core/paths.rkt.
               ,@(path-core-runtime-helper-forms core-runtime-helper-index)
               ,@(path-runtime-helper-forms runtime-helper-index)

               ;;   Environment variable helpers are implemented in runtime-wasm/core/io-handlers.rkt.
               ,@(io-handler-environment-predicate-runtime-helper-forms runtime-helper-index)

               ;;   Implemented in runtime-wasm/core/parameters.rkt.
               ,@(current-environment-variables-runtime-helper-forms runtime-helper-index)

               ;;   Environment variable helpers are implemented in runtime-wasm/core/io-handlers.rkt.
               ,@(io-handler-environment-ref-runtime-helper-forms runtime-helper-index)

               ;;   Implemented in runtime-wasm/core/parameters.rkt.
               ,@(current-print-runtime-helper-forms runtime-helper-index)

               ;;   Bootstrap reader and printer helpers are implemented in runtime-wasm/core/io-handlers.rkt.
               ,@(io-handler-reader-printer-runtime-helper-forms runtime-helper-index)

               ;;   Implemented in runtime-wasm/core/parameters.rkt.
               ,@(current-readtable-core-runtime-helper-forms core-runtime-helper-index)
               ,@(current-readtable-runtime-helper-forms runtime-helper-index)

               ;;   Port handler helpers are implemented in runtime-wasm/core/io-handlers.rkt.
               ,@(io-handler-port-runtime-helper-forms runtime-helper-index)

               ;;   Implemented in runtime-wasm/core/parameters.rkt.
               ,@(current-read-interaction-runtime-helper-forms runtime-helper-index)

               ;; load-on-demand-enabled/use-user-specific-search-paths/read-accept-* : [boolean?] -> boolean? or void?
               ;;   Bootstrap reader policy parameters. The Reference gives
               ;;   load-on-demand-enabled a default of #t; WebRacket exposes
               ;;   the parameter now, while delayed source loading is deferred
               ;;   until the reader/load layer is richer.
               ,@(reader-printer-policy-parameter-helper-forms runtime-helper-index)

               ;;   Implemented in runtime-wasm/core/parameters.rkt.
               ,@(parameter-width-core-runtime-helper-forms core-runtime-helper-index)
               ,@(parameter-width-runtime-helper-forms runtime-helper-index)

               ;;   Error rendering defaults are implemented in runtime-wasm/core/io-handlers.rkt.
               ,@(io-handler-error-runtime-helper-forms runtime-helper-index)

               ;;   Implemented in runtime-wasm/core/parameters.rkt.
               ,@(parameter-error-rendering-core-runtime-helper-forms core-runtime-helper-index)
               ,@(parameter-error-rendering-runtime-helper-forms runtime-helper-index)

               ;;   Implemented in runtime-wasm/core/parameters.rkt.
               ,@(parameter-directory-runtime-helper-forms runtime-helper-index)

               ;;   Implemented in runtime-wasm/core/parameters.rkt.
               ,@(current-load-relative-directory-runtime-helper-forms runtime-helper-index)

               ;;   Path manipulation helpers are implemented in runtime-wasm/core/paths.rkt.
               ,@(path-manipulation-core-runtime-helper-forms core-runtime-helper-index)
               ,@(path-manipulation-runtime-helper-forms runtime-helper-index)

               ;;   VFS and file-operation helpers are implemented in runtime-wasm/core/filesystem.rkt.
               ,@(filesystem-runtime-helper-forms runtime-helper-index)

               ;;;
               ;;; FFI
               ;;;

               ;; Top level `(define-label ...)
               ,@dls

               ;; $entry
               ;;  - called by the host (Node or Browser) to start computation
               ;;  - the host expects `entry` to return an integer as result
               ;;  - we convert the result value into a byte string
               ;;  - store the byte string in $result-bytes
               ;;  - we return the length of $result-bytes
               ;;  - the host calls $get-bytes which copies the result-bytes
               ;;    into the linear memory, where the host can read it.

               ,@path-current-global-forms
               ,@environment-parameter-global-forms
               ,@filesystem-temporary-global-forms
               ,@port-current-global-forms
               ,@print-read-parameter-global-forms
               ,@logger-current-global-forms
               ,@security-current-global-forms
               ,@io-handler-default-port-global-forms
               ,@reader-printer-policy-parameter-global-forms
               ,@compile-parameter-global-forms
               ,@parameterization-global-forms
               ,@namespace-current-global-forms
               ,@random-current-global-forms
               ,@thread-group-current-global-forms
               ,@error-rendering-handler-global-forms
               ,@thread-current-global-forms
               ,@event-global-forms

               ,@struct-property-global-forms
               ,@ffi-entry-global-forms

               ,@entry-support-forms
               ,@split-runtime-primitive-initializer-helper-forms
               ,@split-runtime-constant-initializer-helper-forms
               ,@split-runtime-entry-initializer-helper-forms

               ,@(build-entry-function-forms)

               ))

    (time-runtime-step
     "runtime:ffi-wrapper-filter"
     (λ ()
       (runtime-ffi-state-filter-current-bindings! ffi-state used-primitives)))
    (runtime-metric-add! "runtime:target"
                         (if (current-browser?) "browser" "node"))
    (runtime-ffi-state-record-metrics! ffi-state)
    (runtime-metric-add! "runtime:program-primitive-count"
                         (length used-primitives))
    ;; Even when wrappers live in the program or FFI sidecar, the core
    ;; runtime still provides symbol constants imported by those wrappers.
    ;; Keep the wrapper bodies out of the core key, but include the names
    ;; that affect core constants/exports.
    (define stable-runtime-active-ffi-foreigns
      (runtime-ffi-state-active-foreigns ffi-state))
    (define stable-runtime-active-ffi-imports-wat
      (if (or (split-ffi-sidecar?)
              (split-ffi-program-owned?))
          '()
          (runtime-ffi-state-active-imports-wat ffi-state)))
    (define stable-runtime-active-ffi-funcs-wat
      (if (or (split-ffi-sidecar?)
              (split-ffi-program-owned?))
          '()
          (runtime-ffi-state-active-funcs-wat ffi-state)))
    (define split-runtime-request-fingerprint
      (and (current-split-runtime?)
           (time-runtime-step
            "runtime:split-runtime-request-cache-fingerprint"
            (lambda ()
              (split-runtime-request-cache-fingerprint
               #:browser? (current-browser?)
               #:runtime-source-fingerprint (runtime-source-fingerprint)
               #:active-ffi-foreigns stable-runtime-active-ffi-foreigns
               #:active-ffi-imports-wat stable-runtime-active-ffi-imports-wat
               #:active-ffi-funcs-wat stable-runtime-active-ffi-funcs-wat)))))
    (define split-runtime-core-cache-key
      (and (current-split-runtime?)
           (make-split-runtime-core-cache-key
            (current-tree-shake?)
            split-runtime-request-fingerprint)))
    (define split-runtime-ffi-cache-key
      (and (split-ffi-sidecar?)
           split-runtime-core-cache-key
           (time-runtime-step
            "runtime:split-ffi-cache-key"
            (lambda ()
              (make-split-runtime-ffi-cache-key
               split-runtime-core-cache-key
               (runtime-ffi-state-active-foreigns ffi-state)
               (runtime-ffi-state-active-imports-wat ffi-state)
               (runtime-ffi-state-active-funcs-wat ffi-state)
               (split-ffi-sidecar-function-names))))))
    (runtime-metric-add! "runtime:split-ffi-sidecar-cache-key?"
                         (if split-runtime-ffi-cache-key "yes" "no"))
    ;; split-runtime-core-wasm-cache-hit? : any/c -> boolean?
    ;;   Check whether the stable split core wasm is already materialized.
    (define (split-runtime-core-wasm-cache-hit? core-cache-key)
      (define cache-root (current-runtime-cache-dir))
      (and cache-root
           (not (current-provider-cache-force?))
           (file-exists?
            (split-runtime-core-cache-wasm-path
             cache-root
             (split-runtime-core-cache-key-fingerprint core-cache-key)))))
    (define split-runtime-core-wasm-cache-hit-now?
      (and split-runtime-core-cache-key
           (time-runtime-step
            "runtime:split-core-wasm-cache-preflight"
            (lambda ()
              (split-runtime-core-wasm-cache-hit?
               split-runtime-core-cache-key)))))
    (runtime-metric-add! "runtime:split-core-wasm-cache-preflight-hit?"
                         (if split-runtime-core-wasm-cache-hit-now? "yes" "no"))
    (define split-runtime-boundary-metadata
      (and split-runtime-core-cache-key
           (time-runtime-step
            "runtime:split-boundary-cache-preflight"
            (lambda ()
              (read-split-runtime-boundary-metadata/cache
               split-runtime-core-cache-key)))))
    (runtime-metric-add! "runtime:split-boundary-cache-preflight-hit?"
                         (if split-runtime-boundary-metadata "yes" "no"))
    (define split-runtime-program-forms-cache #f)
    (define (split-runtime-program-forms!)
      (or split-runtime-program-forms-cache
          (let ([forms (time-runtime-step
                        "runtime:build-split-program-forms"
                        build-split-program-forms)])
            (set! split-runtime-program-forms-cache forms)
            forms)))
    (define split-runtime-program-index-cache #f)
    (define split-runtime-program-only-candidate?
      (and (current-split-runtime?)
           (current-split-runtime-program-only-fast-path?)
           split-runtime-boundary-metadata
           (or (current-split-runtime-reference-only?)
               split-runtime-core-wasm-cache-hit-now?)))
    (define split-runtime-reference-compatible-now?
      (and split-runtime-program-only-candidate?
           (time-runtime-step
            "runtime:split-reference-compatibility"
            (lambda ()
              (define-values (compatible? program-index)
                (split-runtime-reference-compatible+index
                 split-runtime-boundary-metadata
                 (split-runtime-program-forms!)))
              (set! split-runtime-program-index-cache program-index)
              compatible?))))
    (runtime-metric-add! "runtime:split-reference-compatible?"
                         (if split-runtime-program-only-candidate?
                             (if split-runtime-reference-compatible-now? "yes" "no")
                             "n/a"))
    (define split-runtime-program-only-fast-path?
      (and (current-split-runtime?)
           split-runtime-program-only-candidate?
           split-runtime-reference-compatible-now?
           (or (split-ffi-sidecar?)
               (split-ffi-program-owned?)
               (current-split-runtime-ffi-in-core?)
               (and (null? (runtime-ffi-state-active-foreigns ffi-state))
                    (null? (runtime-ffi-state-active-imports-wat ffi-state))
                    (null? (runtime-ffi-state-active-funcs-wat ffi-state))))))
    (runtime-metric-add! "runtime:split-program-only-fast-path?"
                         (if split-runtime-program-only-fast-path? "yes" "no"))
    (cond
      [split-runtime-program-only-fast-path?
       (time-runtime-step
        "runtime:split-program-only-fast-path"
        (lambda ()
          (define program-forms (split-runtime-program-forms!))
          (time-runtime-step
           "runtime:split-classified-runtime-forms"
           (lambda ()
             (split-classified-runtime-forms
              #f
              program-forms
             #:core-cache-key split-runtime-core-cache-key
             #:boundary-metadata split-runtime-boundary-metadata
             #:initial-program-index split-runtime-program-index-cache
             #:ffi-sidecar-forms (split-ffi-sidecar-forms)
             #:ffi-sidecar-extra-forms (split-ffi-sidecar-extra-forms)
             #:ffi-sidecar-function-names
              (split-ffi-sidecar-function-names)
              #:ffi-cache-key split-runtime-ffi-cache-key)))))]
      [(and (current-split-runtime?)
            (current-split-runtime-reference-only?)
            (current-browser?)
            split-runtime-boundary-metadata
            (not split-runtime-reference-compatible-now?))
       (error 'generate-runtime
              "program requires a split runtime core different from the supplied core URL")]
      [else
       (let ()
          (define full-module
            (time-runtime-step "runtime:build-runtime-module" build-runtime-module))
          (define analyzed-module
            (analyze-and-prune-runtime-module
             full-module
             #:program-function-names program-function-names
             #:result-global result
             #:top-level-variable-declarations top-level-variable-declarations
             #:string-constants string-constants
             #:bytes-constants bytes-constants
             #:symbol-constants symbol-constants
             #:primitive-func->symbol primitive-func->symbol
             #:primitive-global->symbol primitive-global->symbol
             #:ffi-global->symbol ffi-global->symbol
             #:active-ffi-foreigns (runtime-ffi-state-active-foreigns ffi-state)
             #:active-ffi-imports-wat (runtime-ffi-state-active-imports-wat ffi-state)
             #:active-ffi-funcs-wat (runtime-ffi-state-active-funcs-wat ffi-state)
             #:stable-runtime-active-ffi-foreigns
             stable-runtime-active-ffi-foreigns
             #:stable-runtime-active-ffi-imports-wat
             stable-runtime-active-ffi-imports-wat
             #:stable-runtime-active-ffi-funcs-wat
             stable-runtime-active-ffi-funcs-wat
             #:runtime-owned-constant-global-names
             core-runtime-program-import-constant-global-names
             #:runtime-owned-constant-data-names
             core-runtime-program-import-constant-data-names
             #:extra-program-function-names
             (append (split-program-ffi-function-names)
                     (split-program-console-bridge-function-names))
             #:extra-program-global-names (split-program-ffi-global-names)
             #:described-primitives described-primitives
             #:used-primitives used-primitives
             #:console-bridge-primitive-names console-bridge-primitive-names
             #:primitive-description? primitive->description
             #:time-runtime-step time-runtime-step
             #:include-classified? (current-split-runtime?)))
          (define final-module
            (if (analyzed-runtime-module? analyzed-module)
                (analyzed-runtime-module-module analyzed-module)
                analyzed-module))
          (if (current-split-runtime?)
              (time-runtime-step
               "runtime:split-classified-runtime-forms"
               (lambda ()
                 (split-classified-runtime-forms
                  (analyzed-runtime-module-runtime-forms analyzed-module)
                  (analyzed-runtime-module-program-forms analyzed-module)
                  #:core-cache-key split-runtime-core-cache-key
                  #:boundary-metadata split-runtime-boundary-metadata
                  #:ffi-sidecar-forms (split-ffi-sidecar-forms)
                  #:ffi-sidecar-extra-forms (split-ffi-sidecar-extra-forms)
                  #:ffi-sidecar-function-names
                  (split-ffi-sidecar-function-names)
                  #:ffi-cache-key split-runtime-ffi-cache-key)))
              final-module))])
    ))
