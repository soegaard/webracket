#lang racket/base
;;;        
;;; DRIVER 
;;;        

;; This file is the host-side orchestration layer for compilation.
;; The compiler pipeline itself lives deeper in the compiler and webify
;; modules, while this file owns file-system checks, output naming, provider
;; cache setup, external tool invocation, and the shared execution path used by
;; both the ordinary CLI and the long-lived compile server.

;; The functions in this module "drive" the compiler so to speak.
;; The settings given by the user via the command line are
;; passed as arguments to `drive-compilation`.
;; 
;; Here we handle the necessary file operations and call the
;; appropriate compilation function from "compiler.rkt".
;;
;; Any external bookkeeping with respect to files are done here.


;;;
;;; PROVIDES
;;;

(provide drive-compilation
         (struct-out compilation-output-paths)
         (struct-out compilation-timings)
         resolve-compilation-output-paths
         (all-from-out "webify/provider-cache.rkt")
         (all-defined-out))


;;;
;;; DEPENDENCIES
;;;

(require (only-in syntax/modread    with-module-reading-parameterization)
         (only-in racket/path       file-name-from-path
                                    find-relative-path
                                    path-only
                                    path-get-extension)
         (only-in racket/file       delete-directory/files
                                    file->bytes
                                    find-files
                                    make-directory*
                                    make-temporary-file)
         (only-in racket/port       open-output-nowhere
                                    with-output-to-string)
         (only-in racket/pretty     pretty-write)
         (only-in racket/format     ~a)
         (only-in racket/fasl       fasl->s-exp s-exp->fasl)
         (only-in file/sha1         bytes->hex-string sha1-bytes)
         (only-in racket/promise    delay)
         (only-in racket/system     system*)
         (only-in racket/string     string-join
                                    string-prefix?
                                    string-suffix?)
         (only-in racket/set        set->list
                                    set-member?
                                    seteq)
         racket/unsafe/undefined
         #;(only-in "lang/reader.rkt" read-syntax)
         (only-in "assembler.rkt"
                  run
                  wat->wasm
                  wasm-tools-executable
                  node-executable
                  runtime)
         (only-in "runtime-wasm.rkt"
                  split-core-runtime-module
                  split-runtime-modules
                  split-runtime-modules?
                  split-runtime-modules-core
                  split-runtime-modules-core-module
                  split-runtime-modules-core-cache-key
                  split-runtime-modules-ffi-cache-key
                  split-runtime-modules-ffi-module
                  split-runtime-modules-import-forms
                  split-runtime-modules-imported-names
                  split-runtime-modules-program)
         (only-in "runtime-wasm/provider-fragment-splice.rkt"
                  provider-fragment-splice-info?
                  provider-fragment-splice-info-colliding-program-names
                  provider-fragment-splice-info-dropped-provider-global-import-count
                  provider-fragment-splice-info-program-form-count
                  provider-fragment-splice-info-provider-form-count
                  provider-fragment-splice-info-spliced-form-count
                  splice-provider+program-modules)
         (only-in "runtime-wasm/provider-fragment-runtime.rkt"
                  merge-provider-fragment-into-runtime
                  provider-runtime-merge-info?
                  provider-runtime-merge-info-core-form-count
                  provider-runtime-merge-info-provider-form-count
                  provider-runtime-merge-info-provider-import-count
                  provider-runtime-merge-info-provider-colliding-definition-names
                  provider-runtime-merge-info-merged-runtime-form-count)
         (only-in "runtime-wasm/split-runtime.rkt"
                  strip-split-runtime-program-entry-init)
         (only-in "runtime-wasm/split-runtime-cache.rkt"
                  cacheable-datum
                  cached-datum->value
                  split-runtime-core-fingerprint
                  split-runtime-core-cache-key-fingerprint
                  [split-runtime-core-cache-wasm-path
                   split-runtime-core-cache-wasm-path/cache-root]
                  [split-runtime-core-cache-wat-path
                   split-runtime-core-cache-wat-path/cache-root]
                  [split-runtime-ffi-cache-wasm-path
                   split-runtime-ffi-cache-wasm-path/cache-root]
                  [split-runtime-ffi-cache-wat-path
                   split-runtime-ffi-cache-wat-path/cache-root]
                  make-split-runtime-core-only-cache-key)
         (only-in "runtime-wasm/runtime-source.rkt"
                  runtime-source-fingerprint)
         (only-in "private/structs.rkt"
                  variable?
                  variable-id)
         (only-in "private/wasm-data.rkt"
                  wasm-data
                  wasm-data-ref
                  wasm-data?)
         (only-in racket/list       append* append-map remove-duplicates second take)
         (only-in racket/match      match)
         (only-in racket/set list->seteq)
         setup/dirs
         (only-in "private/parameters.rkt"  current-ffi-foreigns
                                    current-ffi-imports-wat
                                    current-ffi-funcs-wat
                                    current-browser?
                                    current-enable-cc-analysis?
                                    current-console-bridge?
                                    current-console-bridge-root-export-bindings
                                    current-console-bridge-root-source-path
                                    current-enable-simplify?
                                    current-module-bridge-elide-public-cells?
                                    current-module-bridge-elidable-module-names
                                    current-module-bridge-local-source-paths
                                    current-module-bridge-local-report-names
                                    current-module-bridge-support-provider-names
                                    current-module-bridge-module-kinds
                                    current-provider-owned-primitive-aliases
                                    current-imported-top-level-globals
                                    current-exported-top-level-globals
                                    current-webify-knowns-mode
                                    current-letrec-strategy
                                    current-tree-shake?
                                    current-runtime-cache-dir
                                    current-frontend-cache-enabled?
                                    current-runtime-primitive-report-path
                                    current-runtime-extra-used-primitives
                                    current-split-runtime?
                                    current-split-runtime-reference-only?
                                    current-split-runtime-ffi-in-core?
                                    current-split-runtime-ffi-module?
                                    current-split-runtime-ffi-program-owned?
                                    current-split-runtime-program-only-fast-path?)
         (only-in "private/timings.rkt"
                  now-ms
                  format-metric-table
                  format-timing-table)
         "private/wat-identifiers.rkt"
         racket/runtime-path
         racket/syntax
         "private/browser-options.rkt"
         (only-in "legacy-stdlib/module-exports.rkt"
                  stdlib-module-alias-symbols
                  stdlib-module-private-forms+bindings
                  stdlib-module-private-files-forms+bindings
                  stdlib-definition-form-symbols)
         "webify/provider-cache.rkt"
         (only-in "webify/io-format-bridge.rkt"
                  webify-io-format-root-name
                  webify-io-format-printf-name
                  webify-io-format-adapter-name
                  webify-io-format-map-shim-name
                  webify-io-format-module-protocol-name
                  webify-io-format-export-symbols
                  webify-io-format-reports
                  webify-io-format-registry
                  webify-io-format-private-alias-forms
                  webify-io-format-public-alias-forms)
         (only-in "webify/io-print-bridge.rkt"
                  webify-io-print-root-name
                  webify-io-print-adapter-name
                  webify-io-print-export-symbols
                  webify-io-print-reports
                  webify-io-print-registry
                  webify-io-print-private-alias-forms
                  webify-io-print-public-alias-forms)
         (only-in "webify/regexp-provider.rkt"
                  webify-regexp-default-cache-root
                  webify-regexp-bootstrap-helper-symbols
                  webify-regexp-export-symbols
                  webify-regexp-lowered-provider-cache-path
                  webify-regexp-linklet
                  webify-regexp-public-alias-forms
                  webify-regexp-source-fingerprint)
         (only-in "webify/read-provider.rkt"
                  webify-read-export-symbols
                  webify-read-linklet
                  webify-read-lowered-provider-cache-path
                  webify-read-provider-source
                  webify-read-public-alias-pairs
                  webify-read-public-alias-forms
                  webify-read-public-alias-symbols
                  webify-read-source-fingerprint)
         (only-in "webify/read-syntax-provider.rkt"
                  webify-read-syntax-alias-forms
                  webify-read-syntax-export-symbols
                  webify-read-syntax-linklet
                  webify-read-syntax-lowered-provider-cache-path
                  webify-read-syntax-source-fingerprint)
         (only-in "webify/module-adapter.rkt"
                  bridge-export-entry-binding
                  bridge-export-entry-kind
                  bridge-export-entry-metadata
                  first-slice-top-level-export-cell-ids
                  webify-linklet-first-slice-top-level)
         (only-in "webify/module-composer.rkt"
                  current-composer-primitive-import-cell-overrides
                  webify-module-fragment-datum
                  webify-module-fragment-instance)
         (only-in "webify/bridge-stats.rkt"
                  current-webify-bridge-stats)
         (only-in "webify/module-registry.rkt"
                  webify-compose-module-registry/roots/composition
                  webify-module-composition-datum
                  webify-module-composition-fragments
                  webify-module-composition-instances
                  webify-module-registry-names
                  webify-module-composition-with-tail)
         (only-in "webify/module-input.rkt"
                  datum->webify-module-linklet-manifest
                  webify-module-linklet-input
                  webify-module-linklet-input-imports
                  webify-module-linklet-input-linklet
                  webify-module-linklet-input-name
                  webify-module-linklet-inputs->registry
                  webify-module-linklet-manifest->registry)
         (only-in "webify/module-instance.rkt"
                  webify-module-instance-name
                  webify-module-instance-exports
                  webify-module-instance-export-entry)
         (only-in "webify/module-extractor.rkt"
                  make-webify-contract-erasing-namespace
                  make-webify-contract-erasing-module-name-resolver
                  make-webify-module-provider-cache
                  webify-compile-module-report
                  webify-module-import-map-required-module-candidates
                  webify-module-import-map-unresolved-import-groups
                  webify-module-path->report
                  webify-module-provider-cache-hit-count
                  webify-module-provider-cache-miss-count
                  webify-module-provider-cache-ref
                  webify-module-reports->discovered-registry
                  webify-module-reports->discovered-resolved-linklet-inputs
                  webify-module-bundle-entry
                  webify-module-bundle-entry?
                  webify-module-bundle-entry-datum
                  webify-module-bundle-entry-exports
                  webify-module-bundle-entry-imports
                  webify-module-bundle-entry-key
                  webify-module-bundle-entry-kind
                  webify-module-bundle-entry-value
                  webify-module-report
                  webify-module-report-bundle-entries
                  webify-module-report-bundle-keys
                  webify-module-report-directory-keys
                  webify-module-report-import-map
                  webify-module-report-manifest-ready?
                  webify-module-report-name
                  webify-module-report?
                  webify-module-report-notes
                  webify-module-report-phase0-exports
                  webify-module-report-phase0-imports
                  webify-module-report-phase0-linklet-datum
                  webify-module-report-required-provider-names
                  webify-module-reports->resolved-linklet-inputs)
         (only-in "webify/module-protocol.rkt"
                  webify-module-protocol-name
                  webify-module-protocol-input
                  webify-module-protocol-report
                  webify-module-protocol-dependency)
         (only-in "webify/primitive-modules.rkt"
                  kernel-provider-name?)
         (only-in "webify/provider-trials.rkt"
                  webify-provider-trial-default-bridge-module-allowed?
                  webify-provider-trial-module-allowed?
                  webify-provider-trial-module-allowlist)
         (only-in "webify/ffi-module-registry.rkt"
                  module-provider->ffi-filenames)
         "compiler.rkt"
         "private/define-foreign.rkt")

(define-runtime-path webracket-root "../../..")
(define-runtime-path webify-root "webify")

;; webracket/module-reader-path : (or/c 'base 'full) -> (or/c path? #f)
;;   Resolve an installed WebRacket module reader through Racket's collection search.
(define (webracket/module-reader-path language)
  (collection-file-path "reader.rkt"
                        "webracket"
                        (symbol->string language)
                        "lang"
                        #:fail (lambda (_msg) #f)))

;; webracket/base-reader-path : -> (or/c path? #f)
;;   Resolve the installed `webracket/base` reader through Racket's collection search.
(define (webracket/base-reader-path)
  (webracket/module-reader-path 'base))

;; ensure-webracket/module-reader! : (or/c 'base 'full) -> void?
;;   Fail early unless normal Racket collection resolution can find the reader.
(define (ensure-webracket/module-reader! language)
  (unless (webracket/module-reader-path language)
    (raise-arguments-error
     'read-top-level-from-file
     (format "cannot resolve #lang webracket/~a through Racket's collection search"
             language)
     "reader" `(lib ,(format "webracket/~a/lang/reader.rkt" language))
     "hint" "install/link the webracket collection or set PLTCOLLECTS")))

;; ensure-webracket/base-reader! : -> void?
;;   Compatibility wrapper for tests and older driver helpers.
(define (ensure-webracket/base-reader!)
  (ensure-webracket/module-reader! 'base))

;;;
;;; FFI Files
;;; 

;; resolve-ffi-files! : symbol? (listof path-string?) -> (listof path-string?)
;;   Resolve ffi-files to existing paths and fail if any are missing.
(define (resolve-ffi-files! who ffi-files)
  (for/list ([ffi-filename ffi-files])
    (define resolved (resolve-ffi-filename ffi-filename))
    (unless resolved
      (error who
             (~a "ffi file not found: " ffi-filename)))
    resolved))

;; ffi-filename-basename : path-string? -> string?
;;   Return the last path component for an ffi file specification.
(define (ffi-filename-basename ffi-filename)
  (define pathish (if (path? ffi-filename)
                      ffi-filename
                      (string->path ffi-filename)))
  (define basename (file-name-from-path pathish))
  (and basename (path->string basename)))

;; ffi-filename-stem : path-string? -> (or/c string? #f)
;;   Return the ffi filename without its .ffi extension.
(define (ffi-filename-stem ffi-filename)
  (define basename (ffi-filename-basename ffi-filename))
  (and basename
       (path->string
        (path-replace-extension (string->path basename) #""))))

;; source->inferred-ffi-files : syntax? (listof path-string?) -> (listof path-string?)
;;   Infer direct ffi bundles from top-level `(include-lib ...)` forms in source.
(define (source->inferred-ffi-files stx ffi-files)
  (define suppressed-include-libs
    (remove-duplicates
     (append-map bundle->suppressed-include-lib-names ffi-files)))
  (define (top-level-include-lib-names datum)
    (cond
      [(and (pair? datum)
            (eq? (car datum) 'module)
            (pair? (cdr datum))
            (pair? (cddr datum)))
       (append-map top-level-include-lib-names (cdddr datum))]
      [(and (pair? datum)
            (memq (car datum) '(begin begin0)))
       (append-map top-level-include-lib-names (cdr datum))]
      [(and (pair? datum)
            (eq? (car datum) 'include-lib)
            (pair? (cdr datum))
            (symbol? (cadr datum))
            (null? (cddr datum)))
       (if (memq (cadr datum) suppressed-include-libs)
           '()
           (list (cadr datum)))]
      [else
       '()]))
  (define inferred
    (for/list ([lib-name (in-list (top-level-include-lib-names (syntax->datum stx)))]
               #:when (include-lib->ffi-filename lib-name))
      (include-lib->ffi-filename lib-name)))
  (remove-duplicates inferred))

;; module-source->inferred-ffi-files : path-string? (listof path-string?) -> (listof path-string?)
;;   Infer ffi bundles from the actual module-provider graph.
(define (module-source->inferred-ffi-files filename ffi-files)
  (define suppressed-ffi-stems
    (remove-duplicates
     (filter values (map ffi-filename-stem ffi-files))))
  (define provider-names
    (or (current-source-module-provider-names)
        (let-values ([(_root reports _local-source-paths)
                      (webracket/base-file->local-module-reports filename)])
          (module-reports->provider-names reports))))
  (for*/list ([provider-name (in-list provider-names)]
              [ffi-filename (in-list (module-provider->ffi-filenames provider-name))]
              #:unless (member (ffi-filename-stem ffi-filename)
                               suppressed-ffi-stems
                               equal?))
    ffi-filename))

;; default-ffi-files : boolean? -> (listof path-string?)
;;   Return the bundled FFI files enabled by default for the target.
(define (default-ffi-files browser?)
  (if browser?
      (list "standard.ffi" "dom.ffi")
      (list "standard.ffi")))

;; merged-ffi-files : symbol? boolean? (listof path-string?) [syntax?] -> (listof path?)
;;   Resolve and deduplicate default, explicit, and inferred ffi files.
;;     explicit: from the command line
;;     inferred: from include-lib and module-provider requires
(define (merged-ffi-files who browser? ffi-files [stx #f] #:source-filename [source-filename #f])
  (define resolved-base-ffi-files
    (remove-duplicates
     (resolve-ffi-files! who
                         (append (default-ffi-files browser?)
                                 ffi-files))))
  (define inferred-ffi-files
    (if stx
        (source->inferred-ffi-files stx resolved-base-ffi-files)
        '()))
  (define module-inferred-ffi-files
    (if (and source-filename (current-source-module-language))
        (module-source->inferred-ffi-files source-filename
                                           (append resolved-base-ffi-files
                                                   inferred-ffi-files))
        '()))
  (remove-duplicates
   (resolve-ffi-files! who
                       (append resolved-base-ffi-files
                               inferred-ffi-files
                               module-inferred-ffi-files))))

;;;
;;; Include Files
;;;

;; include-lib->ffi-filename : symbol? -> (or/c path-string? #f)
;;   Return the ffi bundle corresponding to a public include-lib library name.
(define include-lib->ffi-filename-table
  '((array      . "array.ffi")
    (audio      . "audio.ffi")
    (canvas     . "canvas.ffi")
    (compression-streams . "dom.ffi")
    (console    . "console.ffi")
    (document   . "document.ffi")
    (dom        . "dom.ffi")
    (domrect    . "domrect.ffi")
    (element    . "element.ffi")
    (event      . "event.ffi")
    (audio      . "audio.ffi")
    (fetch      . "dom.ffi")          ; is window enough?
    (fullscreen . "dom.ffi")
    (gamepad . "dom.ffi")
    (geolocation . "dom.ffi")
    (history . "dom.ffi")
    (image      . "image.ffi")
    (intersection-observer . "dom.ffi")
    (iterator   . "iterator.ffi")
    (jsx-graph  . "jsxgraph.ffi")
    (media      . "media.ffi")
    (notifications . "dom.ffi")
    (rquery     . "dom.ffi")
    (performance . "performance.ffi")
    (resize-observer . "dom.ffi")
    (screen-orientation . "dom.ffi")
    (storage    . "dom.ffi")          ; smaller dep?
    (web-speech . "dom.ffi")
    (web-workers . "dom.ffi")
    (indexed-db . "dom.ffi")          ; smaller dep?
    (websocket  . "websocket.ffi")
    (window     . "window.ffi")
    (web-easy   . "dom.ffi")
    (web-pict   . "dom.ffi")))

(define (include-lib->ffi-filename lib-name)
  (define maybe (assq lib-name include-lib->ffi-filename-table))
  (and maybe (cdr maybe)))


;; bundle->suppressed-include-lib-names : string? -> (listof symbol?)
;;   Return include-lib names that are already covered by an explicit ffi bundle.
;;   This keeps umbrella bundles like `dom.ffi` from re-adding the sublibs
;;   they already include transitively.
(define bundle->suppressed-include-lib-names-table
  '(("dom"    . (array canvas document domrect element event image media performance window))
    ("canvas" . (array media))))

(define (bundle->suppressed-include-lib-names ffi-filename)
  (define basename (ffi-filename-stem ffi-filename))
  (define maybe (and basename (assoc basename bundle->suppressed-include-lib-names-table)))
  (if maybe
      (cdr maybe)
      '()))

;;;
;;; Printing of top-level results
;;;

;; --print-top-level-results
;;   Print each top-level expression result with print.

;; When this option is present, the compiler wraps top-level expressions.
;; The compiler needs to know where the standard library ends and where
;; the user program starts, so we insert a sentinel.

;; add-print-top-level-results-sentinel : syntax? -> syntax?
;;   Insert a compiler-private definition immediately before user forms.
(define (add-print-top-level-results-sentinel stx)
  (define sentinel-id (datum->syntax stx print-top-level-results-sentinel-symbol stx))
  #`(begin
      (define #,sentinel-id (void))
      #,stx))

;; stdlib-cell-symbol : exact-nonnegative-integer? -> symbol?
;;   Produce a private top-level cell name for a stdlib export alias.
(define (stdlib-cell-symbol i)
  (string->symbol (format "webify-stdlib-export-cell$~a" i)))

(define current-webracket/base-provider-substitutions?
  (make-parameter #t))

(define current-read-source-timing-rows
  (make-parameter '()))

(define current-module-bridge-detail-rows
  (make-parameter '()))

(define current-module-bridge-needs-regexp-provider?
  (make-parameter #t))

(define current-module-bridge-needs-expander-read-provider?
  (make-parameter #f))

;; module-bridge-datum-needs-regexp-provider? : any/c -> boolean?
;;   Detect whether the composed bridge datum refers to staged regexp bindings.
(define (module-bridge-datum-needs-regexp-provider? datum)
  (define target-symbols
    (for/hasheq ([sym (in-list (append webify-regexp-export-symbols
                                       webify-regexp-bootstrap-helper-symbols))])
      (values sym #t)))
  (define found? #f)
  (define visited-pairs (make-weak-hasheq))
  (define (walk v)
    (unless found?
      (cond
        [(symbol? v)
         (when (hash-ref target-symbols v #f)
           (set! found? #t))]
        [(or (regexp? v) (byte-regexp? v) (pregexp? v) (byte-pregexp? v))
         (set! found? #t)]
        [(pair? v)
         (unless (hash-ref visited-pairs v #f)
           (hash-set! visited-pairs v #t)
           (walk (car v))
           (walk (cdr v)))]
        [(vector? v)
         (for ([elt (in-vector v)])
           (walk elt))]
        [else (void)])))
  (walk datum)
  found?)

;; module-bridge-datum-contains-symbol? : any/c symbol? -> boolean?
;;   Detect whether a datum tree contains the target symbol.
(define (module-bridge-datum-contains-symbol? datum target)
  (define found? #f)
  (define visited-pairs (make-weak-hasheq))
  (define (walk v)
    (unless found?
      (cond
        [(symbol? v)
         (when (eq? v target)
           (set! found? #t))]
        [(pair? v)
         (unless (hash-ref visited-pairs v #f)
           (hash-set! visited-pairs v #t)
           (walk (car v))
           (walk (cdr v)))]
        [(vector? v)
         (for ([elt (in-vector v)])
           (walk elt))]
        [else (void)])))
  (walk datum)
  found?)

;; module-bridge-datum-needs-formatting-numbers? : any/c -> boolean?
;;   Detect whether script input needs the legacy ~r/precision helper slice.
(define (module-bridge-datum-needs-formatting-numbers? datum)
  (module-bridge-datum-contains-symbol? datum '~r/precision))

;; module-bridge-datum-operator-count : any/c symbol? -> exact-nonnegative-integer?
;;   Count bridge operator calls while skipping quoted/template data.
(define (module-bridge-datum-operator-counts datum targets)
  (define target-table
    (for/hasheq ([target (in-list targets)])
      (values target #t)))
  (define counts (make-hasheq))
  (for ([target (in-list targets)])
    (hash-set! counts target 0))
  (define (record! head)
    (when (and (symbol? head)
               (hash-ref target-table head #f))
      (hash-update! counts head add1 0)))
  (define (walk v)
    (cond
      [(pair? v)
       (match v
         [`(,(or 'quote 'quote-syntax 'syntax 'quasiquote) . ,_)
          (void)]
         [`(,head . ,rest)
          (record! head)
          (walk head)
          (walk rest)]
         [_ (void)])]
      [(vector? v)
       (for ([entry (in-vector v)])
         (walk entry))]
      [else
       (void)]))
  (walk datum)
  counts)

;; module-bridge-datum-operator-count : any/c symbol? -> exact-nonnegative-integer?
;;   Count one bridge operator call while skipping quoted/template data.
(define (module-bridge-datum-operator-count datum target)
  (hash-ref (module-bridge-datum-operator-counts datum (list target))
            target
            0))

;; module-bridge-datum-contains-unquoted-symbol? : any/c symbol? -> boolean?
;;   Detect a symbol in executable syntax while skipping quoted/template data.
(define (module-bridge-datum-contains-unquoted-symbol? datum target)
  (define found? #f)
  (define visited-pairs (make-weak-hasheq))
  (define (walk v)
    (unless found?
      (cond
        [(symbol? v)
         (when (eq? v target)
           (set! found? #t))]
        [(pair? v)
         (unless (hash-ref visited-pairs v #f)
           (hash-set! visited-pairs v #t)
           (match v
             [`(,(or 'quote 'quote-syntax 'syntax 'quasiquote) . ,_)
              (void)]
             [`(,head . ,rest)
              (walk head)
              (walk rest)]
             [_ (void)]))]
        [(vector? v)
         (for ([elt (in-vector v)])
           (walk elt))]
        [else (void)])))
  (walk datum)
  found?)

;; module-bridge-report-reexport-count : webify-module-report? -> exact-nonnegative-integer?
;;   Approximate phase-0 reexports as names that are both imported and exported.
(define (module-bridge-report-reexport-count report)
  (define imported
    (remove-duplicates
     (webify-module-report-phase0-imports report)
     eq?))
  (for/sum ([export (in-list (remove-duplicates
                              (webify-module-report-phase0-exports report)
                              eq?))])
    (if (memq export imported) 1 0)))

;; bridge-export-cell-meta-entries : hash? -> (listof (cons/c list? hash?))
;;   Extract per-cell metadata rows recorded during bridge generation.
(define (bridge-export-cell-meta-entries stats)
  (for/list ([(key value) (in-hash stats)]
             #:when (match key
                      [(list 'bridge-export-cell-meta _module-name _export) #t]
                      [_ #f]))
    (cons key value)))

;; bridge-export-cell-module-counts : hash? -> hasheq?
;;   Collect per-module export-cell counts from the active bridge stats.
(define (bridge-export-cell-module-counts stats)
  (for/hasheq ([(key value) (in-hash stats)]
               #:when (match key
                        [(list 'bridge-export-cell-module (? symbol? _name)) #t]
                        [_ #f]))
    (match key
      [(list 'bridge-export-cell-module name)
       (values name value)])))

;; bridge-export-cell-name-counts : hash? -> hasheq?
;;   Collect per-export-name bridge counts from the active bridge stats.
(define (bridge-export-cell-name-counts stats)
  (for/hasheq ([(key value) (in-hash stats)]
               #:when (match key
                        [(list 'bridge-export-cell-name (? symbol? _name)) #t]
                        [_ #f]))
    (match key
      [(list 'bridge-export-cell-name name)
       (values name value)])))

;; bridge-import-abi-meta-entries : hash? -> (listof hash?)
;;   Extract per-import ABI metadata recorded during linklet slice analysis.
(define (bridge-import-abi-meta-entries stats)
  (for/list ([(key value) (in-hash stats)]
             #:when (match key
                      [(list 'bridge-import-abi-meta _importer _provider _import _export) #t]
                      [_ #f]))
    value))

;; bridge-import-abi-false-module-counts : hash? -> hasheq?
;;   Count ABI-`#f` imports by provider module.
(define (bridge-import-abi-false-module-counts stats)
  (for/hasheq ([(key value) (in-hash stats)]
               #:when (match key
                        [(list 'bridge-import-abi-false-module (? symbol? _name)) #t]
                        [_ #f]))
    (match key
      [(list 'bridge-import-abi-false-module name)
       (values name value)])))

;; bridge-import-abi-false-name-counts : hash? -> hasheq?
;;   Count ABI-`#f` imports by provider export name.
(define (bridge-import-abi-false-name-counts stats)
  (for/hasheq ([(key value) (in-hash stats)]
               #:when (match key
                        [(list 'bridge-import-abi-false-name (? symbol? _name)) #t]
                        [_ #f]))
    (match key
      [(list 'bridge-import-abi-false-name name)
       (values name value)])))

;; bridge-import-abi-false-provider-kind-counts : hash? -> hasheq?
;;   Count ABI-`#f` imports by provider ownership kind.
(define (bridge-import-abi-false-provider-kind-counts stats)
  (for/hasheq ([(key value) (in-hash stats)]
               #:when (match key
                        [(list 'bridge-import-abi-false-provider-kind _kind) #t]
                        [_ #f]))
    (match key
      [(list 'bridge-import-abi-false-provider-kind kind)
       (values kind value)])))

;; bridge-import-abi-false-reason-counts : hash? -> hasheq?
;;   Count ABI-`#f` imports by the recorded analysis fallback reason.
(define (bridge-import-abi-false-reason-counts stats)
  (for/hasheq ([(key value) (in-hash stats)]
               #:when (match key
                        [(list 'bridge-import-abi-false-reason _reason) #t]
                        [_ #f]))
    (match key
      [(list 'bridge-import-abi-false-reason reason)
       (values reason value)])))

;; bridge-import-abi-false-filtered-counts : hash? symbol? (-> hash? any/c) -> hasheq?
;;   Count ABI-`#f` import metadata rows by extractor after filtering on reason.
(define (bridge-import-abi-false-filtered-counts stats false-reason extractor)
  (for/fold ([counts (hasheq)])
            ([entry (in-list (bridge-import-abi-meta-entries stats))])
    (if (and (not (hash-ref entry 'abi #f))
             (eq? (hash-ref entry 'false-reason #f) false-reason))
        (let ([key (extractor entry)])
          (if key
              (hash-update counts key add1 0)
              counts))
        counts)))

;; bridge-import-abi-false-kernel-entries : hash? -> (listof hash?)
;;   Collect ABI-`#f` import metadata rows for `#%kernel`-classified providers.
(define (bridge-import-abi-false-kernel-entries stats)
  (for/list ([entry (in-list (bridge-import-abi-meta-entries stats))]
             #:when (and (not (hash-ref entry 'abi #f))
                         (eq? (hash-ref entry 'provider-kind #f) 'kernel-provider)))
    entry))

;; bridge-import-abi-false-kernel-counts : hash? (-> hash? any/c) -> hasheq?
;;   Count kernel ABI holdouts using one metadata field extractor.
(define (bridge-import-abi-false-kernel-counts stats extractor)
  (for/fold ([counts (hasheq)])
            ([entry (in-list (bridge-import-abi-false-kernel-entries stats))])
    (define key (extractor entry))
    (if key
        (hash-update counts key add1 0)
        counts)))

;; bridge-import-abi-false-kernel-details : hash? -> string?
;;   Render a bounded detail list for residual kernel ABI-`#f` imports.
(define (bridge-import-abi-false-kernel-details stats)
  (define entries
    (sort (bridge-import-abi-false-kernel-entries stats)
          (lambda (a b)
            (define a-importer (~a (hash-ref a 'importer-module #f)))
            (define b-importer (~a (hash-ref b 'importer-module #f)))
            (or (string<? a-importer b-importer)
                (and (string=? a-importer b-importer)
                     (string<?
                      (~a (hash-ref a 'import-name #f))
                      (~a (hash-ref b 'import-name #f))))))))
  (define details
    (for/list ([entry (in-list (take entries (min 10 (length entries))))])
      (define importer-module (hash-ref entry 'importer-module #f))
      (define import-name (hash-ref entry 'import-name #f))
      (define export-name (hash-ref entry 'export-name #f))
      (define group-key (hash-ref entry 'group-key #f))
      (define false-reason (hash-ref entry 'false-reason #f))
      (define kernel-known? (hash-ref entry 'kernel-known? #f))
      (define provider-export-meta
        (and (symbol? (hash-ref entry 'provider-module #f))
             (symbol? export-name)
             (hash-ref stats
                       (list 'bridge-export-cell-meta
                             (hash-ref entry 'provider-module #f)
                             export-name)
                       #f)))
      (format "~a import=~a export=~a group=~a abi-false=~a candidate=~a forced-cell=~a kernel-known=~a"
              importer-module
              import-name
              export-name
              (or group-key '-)
              false-reason
              (cond
                [(not provider-export-meta) "-"]
                [(bridge-public-cell-base-candidate? provider-export-meta) "yes"]
                [else "no"])
              (if (and provider-export-meta
                       (bridge-public-cell-forced-cell? provider-export-meta))
                  "yes"
                  (if provider-export-meta "no" "-"))
              (if kernel-known? "yes" "no"))))
  (if (null? details)
      ""
      (string-join details " | ")))

;; bridge-top-count-summary : hasheq? -> string?
;;   Render the largest bucket counts in descending order for timing output.
(define (bridge-top-count-summary counts)
  (define entries
    (sort (hash->list counts)
          (lambda (a b)
            (define av (cdr a))
            (define bv (cdr b))
            (or (> av bv)
                (and (= av bv)
                     (symbol<? (car a) (car b)))))))
  (if (null? entries)
      "-"
      (string-join
       (for/list ([entry (in-list (take entries (min 5 (length entries))))])
         (format "~a(~a)" (car entry) (cdr entry)))
       ", ")))

;; bridge-public-cell-classification : hash? hash? -> symbol?
;;   Classify one public export cell by the first conservative reason it must stay cell-backed.
(define (bridge-public-cell-classification graph meta)
  (define module-name (hash-ref meta 'module-name #f))
  (define module-kinds
    (webracket/base-local-module-graph-module-kinds graph))
  (define (missing? key)
    (not (hash-has-key? meta key)))
  (define unknown?
    (or (not module-name)
        (not (hash-has-key? module-kinds module-name))
        (missing? 'public-boundary?)
        (missing? 'immutable?)
        (missing? 'reexport?)
        (missing? 'mutated?)
        (missing? 'undefined-sensitive?)
        (missing? 'reflection-visible?)
        (missing? 'dynamic-require-sensitive?)
        (missing? 'phase0-value-space-only?)))
  (cond
    [unknown? 'required-unknown]
    [(not (hash-ref meta 'public-boundary? #f)) 'required-unknown]
    [(hash-ref meta 'reflection-visible? #f) 'required-reflection]
    [(hash-ref meta 'dynamic-require-sensitive? #f) 'required-dynamic-require]
    [(or (not (hash-ref meta 'immutable? #f))
         (hash-ref meta 'mutated? #f))
     'required-mutation]
    [(hash-ref meta 'undefined-sensitive? #f) 'required-undefined]
    [(hash-ref meta 'reexport? #f) 'required-reexport]
    [(not (hash-ref meta 'phase0-value-space-only? #f)) 'required-phase-space]
    [else 'plain-value-candidate]))

;; bridge-public-cell-provider-kind : hash? hash? -> symbol?
;;   Classify one public export by the graph ownership of its defining module.
(define (bridge-public-cell-provider-kind graph meta)
  (define module-name (hash-ref meta 'module-name #f))
  (cond
    [(not (symbol? module-name)) 'unknown-provider]
    [else
     (hash-ref (webracket/base-local-module-graph-module-kinds graph)
               module-name
               'unknown-provider)]))

;; bridge-public-cell-base-candidate? : hash? -> boolean?
;;   Check whether one export is safety-eligible before policy and importer constraints.
(define (bridge-public-cell-base-candidate? meta)
  (hash-ref meta
            'plain-value-base-candidate?
            (eq? (hash-ref meta 'selected-export-kind 'cell)
                 'value)))

;; bridge-public-cell-policy-eligible? : hash? -> boolean?
;;   Check whether the current elision policy allows this export's defining module.
(define (bridge-public-cell-policy-eligible? meta)
  (hash-ref meta
            'plain-value-policy-eligible?
            (bridge-public-cell-base-candidate? meta)))

;; bridge-public-cell-forced-cell? : hash? -> boolean?
;;   Check whether some importer path still requires a cell-backed export entry.
(define (bridge-public-cell-forced-cell? meta)
  (hash-ref meta 'forced-cell? #f))

;; bridge-public-cell-selected-kind : hash? -> symbol?
;;   Read the final export-entry ABI choice recorded for one public export.
(define (bridge-public-cell-selected-kind meta)
  (hash-ref meta 'selected-export-kind 'cell))

;; bridge-public-cell-candidate-not-elided? : hash? -> boolean?
;;   Check whether a safety-eligible export stayed cell-backed.
(define (bridge-public-cell-candidate-not-elided? meta)
  (and (bridge-public-cell-base-candidate? meta)
       (not (eq? (bridge-public-cell-selected-kind meta) 'value))))

;; bridge-export-cell-role-counts : graph hash? -> hasheq?
;;   Partition export-cell counts by local/support/installed module ownership.
(define (bridge-export-cell-role-counts graph stats)
  (define module-counts (bridge-export-cell-module-counts stats))
  (for/fold ([counts (hasheq 'local-user 0
                             'support-provider 0
                             'installed-provider 0
                             'kernel-provider 0)])
            ([(name cell-count) (in-hash module-counts)])
    (define kind
      (hash-ref (webracket/base-local-module-graph-module-kinds graph)
                name
                'installed-provider))
    (hash-update counts kind (lambda (n) (+ n cell-count)) 0)))

;; bridge-public-cell-required-counts : graph hash? -> hasheq?
;;   Count public export cells by the conservative reason they must keep a cell.
(define (bridge-public-cell-required-counts graph stats)
  (for/fold ([counts (hasheq 'required-reflection 0
                             'required-dynamic-require 0
                             'required-mutation 0
                             'required-undefined 0
                             'required-reexport 0
                             'required-phase-space 0
                             'required-unknown 0
                             'plain-value-candidate 0)])
            ([entry (in-list (bridge-export-cell-meta-entries stats))])
    (define meta (cdr entry))
    (define reason
      (bridge-public-cell-classification graph meta))
    (hash-update counts reason add1 0)))

;; bridge-public-cell-candidate-module-counts : graph hash? -> hasheq?
;;   Count candidate plain-value public exports per provider module.
(define (bridge-public-cell-candidate-module-counts graph stats)
  (for/fold ([counts (hasheq)])
            ([entry (in-list (bridge-export-cell-meta-entries stats))])
    (define meta (cdr entry))
    (define module-name (hash-ref meta 'module-name #f))
    (if (and module-name
             (eq? (bridge-public-cell-classification graph meta)
                  'plain-value-candidate))
        (hash-update counts module-name add1 0)
        counts)))

;; bridge-public-cell-candidate-name-counts : graph hash? -> hasheq?
;;   Count candidate plain-value public exports per exported name.
(define (bridge-public-cell-candidate-name-counts graph stats)
  (for/fold ([counts (hasheq)])
            ([entry (in-list (bridge-export-cell-meta-entries stats))])
    (define meta (cdr entry))
    (define export-name (hash-ref meta 'export #f))
    (if (and export-name
             (eq? (bridge-public-cell-classification graph meta)
                  'plain-value-candidate))
        (hash-update counts export-name add1 0)
        counts)))

;; bridge-public-cell-candidate-not-elided-counts : hash? hash? -> hasheq?
;;   Count why safety-eligible exports stayed cell-backed under the current ABI.
(define (bridge-public-cell-candidate-not-elided-counts graph stats)
  (for/fold ([counts (hasheq 'candidate-not-elided 0
                             'candidate-not-elided-local-only-policy 0
                             'candidate-not-elided-provider-export 0
                             'candidate-not-elided-support-provider 0
                             'candidate-not-elided-installed-provider 0
                             'candidate-not-elided-kernel-provider 0
                             'candidate-not-elided-importer-needs-cell 0
                             'candidate-not-elided-registry-shape 0
                             'candidate-not-elided-unknown 0)])
            ([entry (in-list (bridge-export-cell-meta-entries stats))])
    (define meta (cdr entry))
    (if (not (bridge-public-cell-candidate-not-elided? meta))
        counts
        (let* ([provider-kind (bridge-public-cell-provider-kind graph meta)]
               [counts* (hash-update counts 'candidate-not-elided add1 0)]
               [counts** (case provider-kind
                           [(support-provider)
                            (hash-update
                             (hash-update counts* 'candidate-not-elided-provider-export add1 0)
                             'candidate-not-elided-support-provider
                             add1
                             0)]
                           [(installed-provider)
                           (hash-update
                             (hash-update counts* 'candidate-not-elided-provider-export add1 0)
                             'candidate-not-elided-installed-provider
                             add1
                             0)]
                           [(kernel-provider)
                            (hash-update
                             (hash-update counts* 'candidate-not-elided-provider-export add1 0)
                             'candidate-not-elided-kernel-provider
                             add1
                             0)]
                           [(unknown-provider)
                            (hash-update counts* 'candidate-not-elided-unknown add1 0)]
                           [else counts*])])
          (cond
            [(not (bridge-public-cell-policy-eligible? meta))
             (hash-update counts** 'candidate-not-elided-local-only-policy add1 0)]
            [(bridge-public-cell-forced-cell? meta)
             (hash-update counts** 'candidate-not-elided-importer-needs-cell add1 0)]
            [(eq? (bridge-public-cell-selected-kind meta) 'value)
             counts**]
            [else
             (hash-update counts** 'candidate-not-elided-registry-shape add1 0)])))))

;; bridge-public-cell-candidate-not-elided-module-counts : hash? hash? -> hasheq?
;;   Count non-elided safety candidates per defining module.
(define (bridge-public-cell-candidate-not-elided-module-counts graph stats)
  (for/fold ([counts (hasheq)])
            ([entry (in-list (bridge-export-cell-meta-entries stats))])
    (define meta (cdr entry))
    (define module-name (hash-ref meta 'module-name #f))
    (if (and module-name
             (bridge-public-cell-candidate-not-elided? meta))
        (hash-update counts module-name add1 0)
        counts)))

;; bridge-public-cell-candidate-not-elided-name-counts : hash? hash? -> hasheq?
;;   Count non-elided safety candidates per export name.
(define (bridge-public-cell-candidate-not-elided-name-counts graph stats)
  (for/fold ([counts (hasheq)])
            ([entry (in-list (bridge-export-cell-meta-entries stats))])
    (define meta (cdr entry))
    (define export-name (hash-ref meta 'export #f))
    (if (and export-name
             (bridge-public-cell-candidate-not-elided? meta))
        (hash-update counts export-name add1 0)
        counts)))

;; bridge-import-abi-false-elision-candidate-count : hash? hash? -> exact-nonnegative-integer?
;;   Count ABI-`#f` imports whose provider exports are otherwise plain-value candidates.
(define (bridge-import-abi-false-elision-candidate-count graph stats)
  (for/sum ([entry (in-list (bridge-import-abi-meta-entries stats))])
    (define provider (hash-ref entry 'provider-module #f))
    (define export-name (hash-ref entry 'export-name #f))
    (define export-meta
      (and provider
           export-name
           (hash-ref stats (list 'bridge-export-cell-meta provider export-name) #f)))
    (if (and export-meta
             (eq? (bridge-public-cell-classification graph export-meta)
                  'plain-value-candidate)
             (not (hash-ref entry 'abi #f)))
        1
        0)))

;; bridge-public-cell-candidate-not-elided-reason : hash? hash? -> symbol?
;;   Explain why one safety-eligible export stayed cell-backed.
(define (bridge-public-cell-candidate-not-elided-reason graph meta)
  (cond
    [(not (bridge-public-cell-policy-eligible? meta))
     'local-only-policy]
    [(bridge-public-cell-forced-cell? meta)
     'importer-needs-cell]
    [(eq? (bridge-public-cell-selected-kind meta) 'value)
     'elided]
    [else
     'registry-shape]))

;; bridge-import-abi-provider-export-entries : hash? symbol? symbol? -> (listof hash?)
;;   Collect per-import ABI metadata rows for one provider export.
(define (bridge-import-abi-provider-export-entries stats provider-name export-name)
  (for/list ([entry (in-list (bridge-import-abi-meta-entries stats))]
             #:when (and (eq? (hash-ref entry 'provider-module #f) provider-name)
                         (eq? (hash-ref entry 'export-name #f) export-name)))
    entry))

;; bridge-public-cell-candidate-not-elided-support-provider-entries : hash? hash? -> (listof (cons any/c hash?))
;;   Collect non-elided support-provider candidate export metadata entries.
(define (bridge-public-cell-candidate-not-elided-support-provider-entries graph stats)
  (for/list ([entry (in-list (bridge-export-cell-meta-entries stats))]
             #:do [(define meta (cdr entry))]
             #:when (and (bridge-public-cell-candidate-not-elided? meta)
                         (eq? (bridge-public-cell-provider-kind graph meta)
                              'support-provider)))
    entry))

;; bridge-public-cell-candidate-not-elided-support-provider-module-counts : hash? hash? -> hasheq?
;;   Count residual support-provider holdouts by provider module.
(define (bridge-public-cell-candidate-not-elided-support-provider-module-counts graph stats)
  (for/fold ([counts (hasheq)])
            ([entry (in-list (bridge-public-cell-candidate-not-elided-support-provider-entries graph stats))])
    (define module-name (hash-ref (cdr entry) 'module-name #f))
    (if module-name
        (hash-update counts module-name add1 0)
        counts)))

;; bridge-public-cell-candidate-not-elided-support-provider-name-counts : hash? hash? -> hasheq?
;;   Count residual support-provider holdouts by export name.
(define (bridge-public-cell-candidate-not-elided-support-provider-name-counts graph stats)
  (for/fold ([counts (hasheq)])
            ([entry (in-list (bridge-public-cell-candidate-not-elided-support-provider-entries graph stats))])
    (define export-name (hash-ref (cdr entry) 'export #f))
    (if export-name
        (hash-update counts export-name add1 0)
        counts)))

;; bridge-public-cell-candidate-not-elided-support-provider-details : hash? hash? -> string?
;;   Render a bounded detail list for residual support-provider holdouts.
(define (bridge-public-cell-candidate-not-elided-support-provider-details graph stats)
  (define support-provider-entries
    (bridge-public-cell-candidate-not-elided-support-provider-entries graph stats))
  (define details
    (for/list ([entry (in-list (take support-provider-entries
                                     (min 10 (length support-provider-entries))))])
      (define meta (cdr entry))
      (define provider-module (hash-ref meta 'module-name #f))
      (define export-name (hash-ref meta 'export #f))
      (define import-entries
        (bridge-import-abi-provider-export-entries stats provider-module export-name))
      (define importer-modules
        (remove-duplicates
         (for/list ([import-entry (in-list import-entries)])
           (hash-ref import-entry 'importer-module #f))
         eq?))
      (define import-abis
        (remove-duplicates
         (for/list ([import-entry (in-list import-entries)])
           (hash-ref import-entry 'abi #f))
         equal?))
      (define abi-false-reasons
        (remove-duplicates
         (for/list ([import-entry (in-list import-entries)]
                    #:when (not (hash-ref import-entry 'abi #f)))
           (hash-ref import-entry 'false-reason #f))
         eq?))
      (format "~a/~a importers=~a abi=~a provider-kind=~a not-elided=~a abi-false=~a cell-forced=~a"
              provider-module
              export-name
              (if (null? importer-modules) "-" (string-join (map ~a importer-modules) ","))
              (if (null? import-abis) "-" (string-join (map ~a import-abis) ","))
              (hash-ref meta 'provider-kind 'unknown-provider)
              (bridge-public-cell-candidate-not-elided-reason graph meta)
              (if (null? abi-false-reasons) "-" (string-join (map ~a abi-false-reasons) ","))
              (if (bridge-public-cell-forced-cell? meta) "yes" "no"))))
  (if (null? details)
      ""
      (string-join details " | ")))

;; installed-provider-policy-class : symbol? -> symbol?
;;   Split installed providers into the current public-cell elision policy buckets.
(define (installed-provider-policy-class module-name)
  (cond
    [(not (symbol? module-name))
     'unknown-installed-provider]
    [(webracket-owned-ffi-provider-name? module-name)
     'webracket-owned-installed-provider]
    [(string-prefix? (symbol->string module-name) "racket/")
     'racket-installed-provider]
    [else
     'third-party-installed-provider]))

;; racket-installed-provider-elision-allowed-module? : any/c -> boolean?
;;   Narrow allowlist for upstream Racket installed providers eligible for elision.
(define (racket-installed-provider-elision-allowed-module? module-name)
  (and (symbol? module-name)
       (memq module-name
             '(racket/private/list
               racket/list
               racket/private/more-scheme
               racket/private/string
               racket/string))))

;; bridge-installed-provider-candidate-not-elided-entries : hash? hash? -> (listof (cons any/c hash?))
;;   Collect non-elided installed-provider candidate export metadata entries.
(define (bridge-installed-provider-candidate-not-elided-entries graph stats)
  (for/list ([entry (in-list (bridge-export-cell-meta-entries stats))]
             #:do [(define meta (cdr entry))]
             #:when (and (bridge-public-cell-candidate-not-elided? meta)
                         (eq? (bridge-public-cell-provider-kind graph meta)
                              'installed-provider)))
    entry))

;; bridge-installed-provider-candidate-class-counts : hash? hash? -> hasheq?
;;   Count residual installed-provider candidates by policy class.
(define (bridge-installed-provider-candidate-class-counts graph stats)
  (for/fold ([counts (hasheq 'webracket-owned-installed-provider 0
                             'racket-installed-provider 0
                             'third-party-installed-provider 0
                             'unknown-installed-provider 0)])
            ([entry (in-list (bridge-installed-provider-candidate-not-elided-entries graph stats))])
    (define meta (cdr entry))
    (hash-update counts
                 (installed-provider-policy-class (hash-ref meta 'module-name #f))
                 add1
                 0)))

;; bridge-installed-provider-candidate-module-counts : hash? hash? -> hasheq?
;;   Count residual installed-provider candidates by provider module.
(define (bridge-installed-provider-candidate-module-counts graph stats)
  (for/fold ([counts (hasheq)])
            ([entry (in-list (bridge-installed-provider-candidate-not-elided-entries graph stats))])
    (define module-name (hash-ref (cdr entry) 'module-name #f))
    (if module-name
        (hash-update counts module-name add1 0)
        counts)))

;; bridge-installed-provider-candidate-name-counts : hash? hash? -> hasheq?
;;   Count residual installed-provider candidates by export name.
(define (bridge-installed-provider-candidate-name-counts graph stats)
  (for/fold ([counts (hasheq)])
            ([entry (in-list (bridge-installed-provider-candidate-not-elided-entries graph stats))])
    (define export-name (hash-ref (cdr entry) 'export #f))
    (if export-name
        (hash-update counts export-name add1 0)
        counts)))

;; bridge-installed-provider-candidate-class-details : hash? hash? symbol? -> string?
;;   Render a bounded detail list for one installed-provider policy class.
(define (bridge-installed-provider-candidate-class-details graph stats class)
  (define entries
    (filter (lambda (entry)
              (eq? (installed-provider-policy-class
                    (hash-ref (cdr entry) 'module-name #f))
                   class))
            (bridge-installed-provider-candidate-not-elided-entries graph stats)))
  (define details
    (for/list ([entry (in-list (take entries (min 50 (length entries))))])
      (define meta (cdr entry))
      (define provider-module (hash-ref meta 'module-name #f))
      (define export-name (hash-ref meta 'export #f))
      (define import-entries
        (bridge-import-abi-provider-export-entries stats provider-module export-name))
      (define importer-modules
        (remove-duplicates
         (for/list ([import-entry (in-list import-entries)])
           (hash-ref import-entry 'importer-module #f))
         eq?))
      (define import-abis
        (remove-duplicates
         (for/list ([import-entry (in-list import-entries)])
           (hash-ref import-entry 'abi #f))
         equal?))
      (format "~a/~a constance=~a immutable=~a phase0=~a dynamic=~a reflection=~a importers=~a importer-modules=~a import-abi=~a reason=~a"
              provider-module
              export-name
              (hash-ref meta 'constance #f)
              (hash-ref meta 'immutable? #f)
              (hash-ref meta 'phase0-value-space-only? #f)
              (hash-ref meta 'dynamic-require-sensitive? #f)
              (hash-ref meta 'reflection-visible? #f)
              (length importer-modules)
              (if (null? importer-modules) "-" (string-join (map ~a importer-modules) ","))
              (if (null? import-abis) "-" (string-join (map ~a import-abis) ","))
              (bridge-public-cell-candidate-not-elided-reason graph meta))))
  (if (null? details)
      ""
      (string-join details " | ")))

;; bridge-public-cell-elided-installed-provider-class-counts : hash? hash? -> hasheq?
;;   Count installed-provider exports that now elide by policy class.
(define (bridge-public-cell-elided-installed-provider-class-counts graph stats)
  (for/fold ([counts (hasheq 'webracket-owned-installed-provider 0
                             'racket-installed-provider 0
                             'third-party-installed-provider 0
                             'unknown-installed-provider 0)])
            ([entry (in-list (bridge-export-cell-meta-entries stats))])
    (define meta (cdr entry))
    (if (and (eq? (bridge-public-cell-provider-kind graph meta) 'installed-provider)
             (eq? (bridge-public-cell-selected-kind meta) 'value))
        (hash-update counts
                     (installed-provider-policy-class (hash-ref meta 'module-name #f))
                     add1
                     0)
        counts)))

;; bridge-racket-installed-provider-kept-reason-counts : hash? hash? -> hasheq?
;;   Count why Racket installed-provider exports remain cell-backed.
(define (bridge-racket-installed-provider-kept-reason-counts graph stats)
  (for/fold ([counts (hasheq 'metadata 0
                             'abi 0
                             'policy 0)])
            ([entry (in-list (bridge-export-cell-meta-entries stats))])
    (define meta (cdr entry))
    (define module-name (hash-ref meta 'module-name #f))
    (if (and (eq? (bridge-public-cell-provider-kind graph meta) 'installed-provider)
             (eq? (installed-provider-policy-class module-name)
                  'racket-installed-provider)
             (not (eq? (bridge-public-cell-selected-kind meta) 'value)))
        (cond
          [(not (bridge-public-cell-base-candidate? meta))
           (hash-update counts 'metadata add1 0)]
          [(bridge-public-cell-forced-cell? meta)
           (hash-update counts 'abi add1 0)]
          [else
           (hash-update counts 'policy add1 0)])
        counts)))

;; bridge-module-elidable-public-modules : graph -> (listof symbol?)
;;   List modules currently allowed to expose safe public exports as plain values.
(define (bridge-module-elidable-public-modules graph)
  (for/list ([(name kind)
              (in-hash (webracket/base-local-module-graph-module-kinds graph))]
             #:when (or (memq kind '(local-user support-provider))
                        (and (eq? kind 'installed-provider)
                             (or (webracket-owned-ffi-provider-name? name)
                                 (racket-installed-provider-elision-allowed-module? name)))))
    name))

;; module-bridge-fragment-body : any/c -> (listof any/c)
;;   Return the top-level forms generated for one composed module fragment.
(define (module-bridge-fragment-body datum)
  (match datum
    [`(begin ,body ...) body]
    [_ '()]))

(struct module-bridge-fragment-split
  (provider-datum program-datum provider-records program-records tail-forms)
  #:transparent)

;; module-bridge-fragment-provider-kind : hash? symbol? -> symbol?
;;   Classify one composed fragment using the bridge graph module-kind table.
(define (module-bridge-fragment-provider-kind module-kinds module-name)
  (cond
    [(kernel-provider-name? module-name) 'kernel-provider]
    [(hash-has-key? module-kinds module-name)
     (hash-ref module-kinds module-name)]
    [else 'unknown-provider]))

;; module-bridge-fragment-manifest :
;;   webify-module-composition? hash? -> (listof hash?)
;;   Record the module/kind/span ownership of each composed bridge fragment.
(define (module-bridge-fragment-manifest composition module-kinds)
  (define-values (reverse-records _next-start)
    (for/fold ([records '()]
               [start 0])
              ([fragment (in-list (webify-module-composition-fragments composition))])
      (define instance
        (webify-module-fragment-instance fragment))
      (define module-name
        (webify-module-instance-name instance))
      (define kind
        (module-bridge-fragment-provider-kind module-kinds module-name))
      (define body-count
        (length (module-bridge-fragment-body
                 (webify-module-fragment-datum fragment))))
      (values (cons (hasheq 'module-name module-name
                            'kind kind
                            'start start
                            'count body-count)
                    records)
              (+ start body-count))))
  (reverse reverse-records))

;; module-bridge-fragment-record-counts :
;;   (listof hash?) -> (values hasheq? hasheq? hasheq? exact-nonnegative-integer?)
;;   Count composed fragment manifest records by module kind and module name.
(define (module-bridge-fragment-record-counts fragment-manifest)
  (for/fold ([kind-form-counts (hasheq)]
             [kind-fragment-counts (hasheq)]
             [module-form-counts (hasheq)]
             [total-form-count 0])
            ([record (in-list fragment-manifest)])
    (define module-name (hash-ref record 'module-name))
    (define kind (hash-ref record 'kind))
    (define body-count
      (hash-ref record 'count))
    (values (hash-update kind-form-counts kind (lambda (n) (+ n body-count)) 0)
            (hash-update kind-fragment-counts kind add1 0)
            (hash-update module-form-counts module-name (lambda (n) (+ n body-count)) 0)
            (+ total-form-count body-count))))

;; module-bridge-fragment-counts :
;;   webify-module-composition? hash?
;;   -> (values hasheq? hasheq? hasheq? exact-nonnegative-integer?)
;;   Count composed fragment bodies by module kind and module name.
(define (module-bridge-fragment-counts composition module-kinds)
  (module-bridge-fragment-record-counts
   (module-bridge-fragment-manifest composition module-kinds)))

;; module-bridge-cacheable-provider-kind? : symbol? -> boolean?
;;   Check whether a fragment kind can be separated into a cacheable provider side.
(define (module-bridge-cacheable-provider-kind? kind)
  (memq kind '(support-provider installed-provider kernel-provider)))

;; module-bridge-lowered-datum-body : any/c -> (listof any/c)
;;   Return the top-level body of a lowered bridge datum.
(define (module-bridge-lowered-datum-body lowered-datum)
  (match lowered-datum
    [`(begin ,body ...) body]
    [_
     (raise-argument-error 'module-bridge-lowered-datum-body
                           "(list/c 'begin ...)"
                           lowered-datum)]))

;; module-bridge-fragment-record-end : hash? -> exact-nonnegative-integer?
;;   Return the exclusive form index at the end of one fragment record.
(define (module-bridge-fragment-record-end record)
  (+ (hash-ref record 'start)
     (hash-ref record 'count)))

;; module-bridge-fragment-manifest-covered-count :
;;   (listof hash?) -> exact-nonnegative-integer?
;;   Validate contiguous fragment spans and return the number of covered forms.
(define (module-bridge-fragment-manifest-covered-count fragment-manifest)
  (for/fold ([expected-start 0])
            ([record (in-list fragment-manifest)])
    (define start (hash-ref record 'start))
    (define end (module-bridge-fragment-record-end record))
    (unless (= start expected-start)
      (raise-arguments-error
       'module-bridge-fragment-manifest-covered-count
       "fragment manifest is not contiguous"
       "expected start" expected-start
       "record" record))
    end))

;; module-bridge-fragment-record-forms :
;;   vector? hash? -> (listof any/c)
;;   Extract the lowered forms covered by one fragment record.
(define (module-bridge-fragment-record-forms body-vector record)
  (define start (hash-ref record 'start))
  (define end (module-bridge-fragment-record-end record))
  (when (> end (vector-length body-vector))
    (raise-arguments-error
     'module-bridge-fragment-record-forms
     "fragment manifest extends past lowered datum body"
     "body length" (vector-length body-vector)
     "record" record))
  (for/list ([i (in-range start end)])
    (vector-ref body-vector i)))

;; module-bridge-split-lowered-datum :
;;   any/c (listof hash?) -> module-bridge-fragment-split?
;;   Split a lowered bridge datum into cacheable provider forms and program forms.
(define (module-bridge-split-lowered-datum lowered-datum fragment-manifest)
  (unless (module-bridge-fragment-manifest? fragment-manifest)
    (raise-argument-error 'module-bridge-split-lowered-datum
                          "module-bridge-fragment-manifest?"
                          fragment-manifest))
  (define body (module-bridge-lowered-datum-body lowered-datum))
  (define covered-count
    (module-bridge-fragment-manifest-covered-count fragment-manifest))
  (when (> covered-count (length body))
    (raise-arguments-error
     'module-bridge-split-lowered-datum
     "fragment manifest covers more forms than the lowered datum contains"
     "body length" (length body)
     "covered count" covered-count))
  (define body-vector (list->vector body))
  (define-values (provider-chunks-reverse
                  program-chunks-reverse
                  provider-records-reverse
                  program-records-reverse)
    (for/fold ([provider-chunks '()]
               [program-chunks '()]
               [provider-records '()]
               [program-records '()])
              ([record (in-list fragment-manifest)])
      (define forms
        (module-bridge-fragment-record-forms body-vector record))
      (if (module-bridge-cacheable-provider-kind? (hash-ref record 'kind))
          (values (cons forms provider-chunks)
                  program-chunks
                  (cons record provider-records)
                  program-records)
          (values provider-chunks
                  (cons forms program-chunks)
                  provider-records
                  (cons record program-records)))))
  (define tail-forms
    (for/list ([i (in-range covered-count (length body))])
      (vector-ref body-vector i)))
  (define provider-forms
    (apply append (reverse provider-chunks-reverse)))
  (define program-fragment-forms
    (apply append (reverse program-chunks-reverse)))
  (module-bridge-fragment-split
   `(begin ,@provider-forms)
   `(begin ,@program-fragment-forms ,@tail-forms)
   (reverse provider-records-reverse)
   (reverse program-records-reverse)
   tail-forms))

;; module-bridge-export-symbol? : any/c -> boolean?
;;   Check whether a symbol names a generated module-bridge export box/value.
(define (module-bridge-export-symbol? v)
  (and (symbol? v)
       (regexp-match? #rx"^webify-export-(value|cell)"
                      (symbol->string v))))

;; module-bridge-datum-symbols : any/c -> (set/c symbol?)
;;   Collect every symbol appearing in a lowered bridge datum.
(define (module-bridge-datum-symbols datum)
  (define symbols (make-hasheq))
  (define (walk v)
    (cond
      [(symbol? v)
       (hash-set! symbols v #t)]
      [(pair? v)
       (walk (car v))
       (walk (cdr v))]
      [(vector? v)
       (for ([item (in-vector v)])
         (walk item))]
      [else
       (void)]))
  (walk datum)
  (list->seteq (hash-keys symbols)))

;; module-bridge-top-define-symbols : any/c -> (set/c symbol?)
;;   Collect top-level definition names in a lowered bridge fragment.
(define (module-bridge-top-define-symbols datum)
  (define names
    (match datum
      [`(begin ,forms ...)
       (append*
        (for/list ([form (in-list forms)])
          (match form
            [`(define ,(? symbol? name) ,_) (list name)]
            [`(define-values (,names ...) ,_)
             (filter symbol? names)]
            [_ '()])))]
      [_ '()]))
  (list->seteq names))

;; module-bridge-provider-program-interface-symbols :
;;   any/c any/c (set/c symbol?) -> (set/c symbol?)
;;   Return provider top-level boxes/values demanded by the program fragment.
(define (module-bridge-provider-program-interface-symbols provider-datum
                                                          program-datum
                                                          provider-owned-aliases)
  (define provider-defs (module-bridge-top-define-symbols provider-datum))
  (define program-symbols (module-bridge-datum-symbols program-datum))
  (define (interface-symbol? name)
    (or (module-bridge-export-symbol? name)
        ;; Staged providers can also expose public primitive aliases, such as
        ;; `read`, as plain top-level definitions instead of generated export
        ;; cells. Those aliases still have to cross the provider/program split.
        (set-member? provider-owned-aliases name)))
  (list->seteq
   (sort
    (for/list ([name (in-list (set->list program-symbols))]
               #:when (and (interface-symbol? name)
                           (set-member? provider-defs name)))
      name)
    symbol<?)))

;; module-bridge-provider-ffi-primitive-symbols : any/c -> (listof symbol?)
;;   Return FFI primitive names referenced by the provider-side fragment.
(define (module-bridge-provider-ffi-primitive-symbols provider-datum)
  (define provider-symbols (module-bridge-datum-symbols provider-datum))
  (sort
   (remove-duplicates
    (append
     (if (set-member? provider-symbols 'unsafe-ffi-primitive)
         '(unsafe-ffi-primitive)
         '())
     (for/list ([name (in-list (foreigns->primitive-names
                                (current-ffi-foreigns)))]
                #:when (set-member? provider-symbols name))
       name)))
   symbol<?))

;; datum-sha1 : any/c -> string?
;;   Compute a stable digest for a serialized datum.
(define (datum-sha1 datum)
  (define out (open-output-bytes))
  (write datum out)
  (bytes->hex-string
   (sha1-bytes (get-output-bytes out))))

;; module-bridge-provider-fragment-record-shape :
;;   (listof hash?) -> (listof list?)
;;   Keep the provider-fragment cache key readable without including local spans.
(define (module-bridge-provider-fragment-record-shape records)
  (for/list ([record (in-list records)])
    (list (hash-ref record 'module-name)
          (hash-ref record 'kind)
          (hash-ref record 'count))))

;; module-bridge-provider-fragment-fingerprint :
;;   module-bridge-fragment-split? -> any/c
;;   Fingerprint the provider-side fragment payload independent of local-user forms.
(define (module-bridge-provider-fragment-fingerprint split)
  `(module-bridge-provider-fragment-cache
    1
    ,(module-bridge-provider-fragment-record-shape
      (module-bridge-fragment-split-provider-records split))
    ,(datum-sha1 (module-bridge-fragment-split-provider-datum split))))

;; module-bridge-provider-fragment-fingerprint-digest : any/c -> string?
;;   Return the compact digest component used for the shared provider-fragment path.
(define (module-bridge-provider-fragment-fingerprint-digest fingerprint)
  (match fingerprint
    [`(module-bridge-provider-fragment-cache 1 ,_shape ,(? string? digest))
     digest]
    [_
     (datum-sha1 fingerprint)]))

;; module-bridge-provider-fragment-cache-path : any/c -> path?
;;   Cache one provider-side lowered fragment payload by its stable fingerprint.
(define (module-bridge-provider-fragment-cache-path fingerprint)
  (provider-cache-path
   "module-bridge-provider-fragments"
   (format "~a.rktd"
           (provider-cache-string-segment
            (module-bridge-provider-fragment-fingerprint-digest fingerprint)))))

;; module-bridge-provider-fragment->cache-datum :
;;   module-bridge-fragment-split? -> datum?
;;   Serialize the cacheable provider-side bridge fragment.
(define (module-bridge-provider-fragment->cache-datum split)
  `(module-bridge-provider-fragment
    (provider-records
     ,(cache-readable-datum
       (module-bridge-fragment-split-provider-records split)))
    (provider-datum
     ,(cache-readable-datum
       (module-bridge-fragment-split-provider-datum split)))))

;; cache-datum->module-bridge-provider-fragment-payload : any/c -> (or/c vector? #f)
;;   Rehydrate one cached provider-fragment payload.
(define (cache-datum->module-bridge-provider-fragment-payload datum)
  (match datum
    [`(module-bridge-provider-fragment
       (provider-records ,provider-records-datum)
       (provider-datum ,provider-datum-datum))
     (define provider-records
       (cache-readable->datum provider-records-datum))
     (define provider-datum
       (cache-readable->datum provider-datum-datum))
     (and (list? provider-records)
          (andmap module-bridge-fragment-record? provider-records)
          (match provider-datum
            [`(begin ,_ ...) #t]
            [_ #f])
          (vector provider-records provider-datum))]
    [_ #f]))

;; read-module-bridge-provider-fragment-cache : any/c -> (or/c vector? #f)
;;   Read a shared provider-fragment cache payload when the fingerprint matches.
(define (read-module-bridge-provider-fragment-cache fingerprint)
  (define cache-path
    (module-bridge-provider-fragment-cache-path fingerprint))
  (define cached
    (read-provider-cache cache-path fingerprint))
  (and cached
       (cache-datum->module-bridge-provider-fragment-payload cached)))

;; write-module-bridge-provider-fragment-cache! :
;;   any/c module-bridge-fragment-split? -> void?
;;   Persist one shared provider-fragment payload.
(define (write-module-bridge-provider-fragment-cache! fingerprint split)
  (write-provider-cache!
   (module-bridge-provider-fragment-cache-path fingerprint)
   fingerprint
   (module-bridge-provider-fragment->cache-datum split)))

;; ensure-module-bridge-provider-fragment-cache! :
;;   any/c module-bridge-fragment-split? -> (or/c 'hit 'write)
;;   Materialize a shared provider-fragment payload if it is not already cached.
(define (ensure-module-bridge-provider-fragment-cache! fingerprint split)
  (define cache-path
    (module-bridge-provider-fragment-cache-path fingerprint))
  (cond
    [(read-module-bridge-provider-fragment-cache fingerprint)
     (provider-cache-log "#lang webracket/base provider fragment hit: ~a"
                         cache-path)
     'hit]
    [else
     (call-with-provider-cache-lock
      cache-path
      (lambda ()
        (cond
          [(read-module-bridge-provider-fragment-cache fingerprint)
           (provider-cache-log "#lang webracket/base provider fragment hit: ~a"
                               cache-path)
           'hit]
          [else
           (provider-cache-log "#lang webracket/base provider fragment miss; writing: ~a"
                               cache-path)
           (write-module-bridge-provider-fragment-cache! fingerprint split)
           'write])))]))

;; module-bridge-provider-compiled-fragment-cache-version : exact-positive-integer?
;;   Bump when compiled provider-fragment WAT cache semantics change.
(define module-bridge-provider-compiled-fragment-cache-version 8)

;; module-bridge-provider-compiled-fragment-cache-key :
;;   any/c #:browser? boolean? #:enable-cc-analysis? boolean?
;;   #:enable-simplify? boolean? #:split-runtime-reference? boolean?
;;   #:split-runtime-ffi-in-core? boolean? #:split-runtime-ffi-module? boolean?
;;   #:letrec-strategy symbol?
;;   #:provider-owned-primitive-aliases (listof symbol?) -> any/c
;;   Fingerprint the compiler/runtime options that affect provider-fragment WAT.
(define (module-bridge-provider-compiled-fragment-cache-key
         provider-fragment-fingerprint
         #:browser? [browser? #f]
         #:enable-cc-analysis? [enable-cc-analysis? #f]
         #:enable-simplify? [enable-simplify? #f]
         #:split-runtime-reference? [split-runtime-reference? #f]
         #:split-runtime-ffi-in-core? [split-runtime-ffi-in-core? #f]
         #:split-runtime-ffi-module? [split-runtime-ffi-module? #f]
         #:letrec-strategy [letrec-strategy 'scc]
         #:provider-owned-primitive-aliases [provider-owned-primitive-aliases '()])
  `(module-bridge-provider-compiled-fragment-wat-cache
    ,module-bridge-provider-compiled-fragment-cache-version
    (provider-fragment ,provider-fragment-fingerprint)
    (runtime-source ,(runtime-source-fingerprint))
    (browser? ,browser?)
    (enable-cc-analysis? ,enable-cc-analysis?)
    (enable-simplify? ,enable-simplify?)
    (split-runtime-reference? ,split-runtime-reference?)
    (split-runtime-ffi-in-core? ,split-runtime-ffi-in-core?)
    (split-runtime-ffi-module? ,split-runtime-ffi-module?)
    (letrec-strategy ,letrec-strategy)
    (provider-owned-primitive-aliases
     ,(sort provider-owned-primitive-aliases symbol<?))))

;; module-bridge-provider-runtime-core-cache-key : any/c any/c -> any/c
;;   Fingerprint a split runtime that embeds one compiled provider fragment.
(define (module-bridge-provider-runtime-core-cache-key core-cache-key
                                                       compiled-provider-cache-key)
  `(module-bridge-provider-runtime-core-cache
    1
    (core ,core-cache-key)
    (compiled-provider ,compiled-provider-cache-key)))

;; module-bridge-provider-compiled-fragment-cache-digest : any/c -> string?
;;   Return the compact digest used for compiled provider-fragment artifacts.
(define (module-bridge-provider-compiled-fragment-cache-digest cache-key)
  (datum-sha1 cache-key))

;; module-bridge-provider-compiled-fragment-wat-cache-path : any/c -> path?
;;   Cache compact WAT for one compiled provider-side bridge fragment.
(define (module-bridge-provider-compiled-fragment-wat-cache-path cache-key)
  (provider-cache-path
   "module-bridge-provider-compiled-fragments"
   (format "~a.wat"
           (provider-cache-string-segment
            (module-bridge-provider-compiled-fragment-cache-digest
             cache-key)))))

;; module-bridge-provider-compiled-fragment-datum-cache-path : any/c -> path?
;;   Cache one compiled provider-fragment datum in Racket-readable form.
(define (module-bridge-provider-compiled-fragment-datum-cache-path cache-key)
  (provider-cache-path
   "module-bridge-provider-compiled-fragments"
   (format "~a.rktd"
           (provider-cache-string-segment
            (module-bridge-provider-compiled-fragment-cache-digest
             cache-key)))))

;; module-bridge-provider-compiled-fragment-wat-cache-fingerprint-path : path? -> path?
;;   Return the sidecar fingerprint path for a compiled provider WAT artifact.
(define (module-bridge-provider-compiled-fragment-wat-cache-fingerprint-path
         wat-path)
  (string->path (string-append (path->string wat-path) ".fingerprint")))

;; module-bridge-provider-compiled-fragment-datum-cache-fingerprint-path : path? -> path?
;;   Return the sidecar fingerprint path for a compiled provider datum artifact.
(define (module-bridge-provider-compiled-fragment-datum-cache-fingerprint-path
         datum-path)
  (string->path (string-append (path->string datum-path) ".fingerprint")))

;; write-compiled-provider-fragment-datum-cache! : path-string? any/c -> void?
;;   Write the textual provider-fragment module cache used by warm driver runs.
(define (write-compiled-provider-fragment-datum-cache! datum-path wat)
  (make-directory* (or (path-only datum-path) (current-directory)))
  (define tmp-path
    (make-temporary-file "compiled-provider-fragment-~a.rktd"
                         #f
                         (or (path-only datum-path)
                             (current-directory))))
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (with-handlers ([exn:fail? void])
                       (when (file-exists? tmp-path)
                         (delete-file tmp-path)))
                     (raise e))])
    (call-with-output-file tmp-path
      (lambda (out)
        (write wat out)
        (newline out))
      #:exists 'replace)
    (when (file-exists? datum-path)
      (delete-file datum-path))
    (rename-file-or-directory tmp-path datum-path)))

;; compiled-provider-fragment-cache-datum : any/c -> any/c
;;   Normalize compiled provider WAT to a Racket-readable cache datum.
(define (compiled-provider-fragment-cache-datum wat)
  (cond
    [(syntax? wat)
     (compiled-provider-fragment-cache-datum (syntax-e wat))]
    [(datum? wat)
     (compiled-provider-fragment-cache-datum (datum-value wat))]
    [(variable? wat)
     (compiled-provider-fragment-cache-datum (syntax-e (variable-id wat)))]
    [(wasm-data? wat)
     (cacheable-datum wat)]
    [(symbol? wat)
     (string->symbol (symbol->string wat))]
    [(string? wat)
     (string->immutable-string wat)]
    [(bytes? wat)
     (bytes->immutable-bytes wat)]
    [(pair? wat)
     (cons (compiled-provider-fragment-cache-datum (car wat))
           (compiled-provider-fragment-cache-datum (cdr wat)))]
    [(vector? wat)
     (vector->immutable-vector
      (list->vector
       (for/list ([elem (in-vector wat)])
         (compiled-provider-fragment-cache-datum elem))))]
    [(box? wat)
     (box (compiled-provider-fragment-cache-datum (unbox wat)))]
    [(prefab-struct-key wat)
     (define data (struct->vector wat))
     (apply make-prefab-struct
            (prefab-struct-key wat)
            (for/list ([i (in-range 1 (vector-length data))])
              (compiled-provider-fragment-cache-datum (vector-ref data i))))]
    [(hash? wat)
     (for/fold ([h (cond
                     [(hash-eq? wat)  (hasheq)]
                     [(hash-eqv? wat) (hasheqv)]
                     [else            (hash)])])
               ([(key value) (in-hash wat)])
       (hash-set h
                 (compiled-provider-fragment-cache-datum key)
                 (compiled-provider-fragment-cache-datum value)))]
    [(or (boolean? wat)
         (number? wat)
         (char? wat)
         (keyword? wat)
         (path? wat)
         (null? wat)
         (void? wat)
         (eof-object? wat))
     (cacheable-datum wat)]
    [else
     (raise-arguments-error
      'compiled-provider-fragment-cache-datum
      "unsupported compiled provider WAT cache value"
      "value" wat)]))

;; module-bridge-provider-compiled-fragment-wat-cache-current? : any/c -> boolean?
;;   Check whether the compiled provider-fragment WAT cache is current.
(define (module-bridge-provider-compiled-fragment-wat-cache-current? cache-key)
  (define wat-path
    (module-bridge-provider-compiled-fragment-wat-cache-path cache-key))
  (define datum-path
    (module-bridge-provider-compiled-fragment-datum-cache-path cache-key))
  (and (file-exists? wat-path)
       (file-exists? datum-path)
       (cached-fingerprint-valid?
        (module-bridge-provider-compiled-fragment-datum-cache-fingerprint-path
         datum-path)
        cache-key)))

;; write-module-bridge-provider-compiled-fragment-wat-cache! :
;;   any/c any/c -> path?
;;   Write compact WAT and its fingerprint for one compiled provider fragment.
(define (write-module-bridge-provider-compiled-fragment-wat-cache!
         cache-key compiled-provider-wat)
  (define wat-path
    (module-bridge-provider-compiled-fragment-wat-cache-path cache-key))
  (define datum-path
    (module-bridge-provider-compiled-fragment-datum-cache-path cache-key))
  (define cache-datum
    (compiled-provider-fragment-cache-datum compiled-provider-wat))
  (write-wat-to-file wat-path (cached-datum->value cache-datum) #:pretty? #f)
  (write-compiled-provider-fragment-datum-cache! datum-path cache-datum)
  (write-cache-fingerprint!
   (module-bridge-provider-compiled-fragment-datum-cache-fingerprint-path
    datum-path)
   cache-key)
  wat-path)

;; read-module-bridge-provider-compiled-fragment-cache :
;;   any/c symbol? -> list?
;;   Read compiled provider-fragment WAT, preferring the readable sidecar cache.
(define (read-module-bridge-provider-compiled-fragment-cache cache-key who)
  (define datum-path
    (module-bridge-provider-compiled-fragment-datum-cache-path cache-key))
  (define wat-path
    (module-bridge-provider-compiled-fragment-wat-cache-path cache-key))
  (define cache-datum
    (cond
      [(file-exists? datum-path)
       (with-handlers ([exn:fail?
                        (lambda (_e)
                          (when (file-exists? datum-path)
                            (delete-file datum-path))
                          (read-wat-module-from-file wat-path who))])
         (call-with-input-file datum-path read))]
      [else
       (define wat (read-wat-module-from-file wat-path who))
       (with-handlers ([exn:fail? void])
         (write-compiled-provider-fragment-datum-cache!
          datum-path
          (compiled-provider-fragment-cache-datum wat)))
       wat]))
  (define datum (cached-datum->value cache-datum))
  (match datum
    [`(module ,_ ...) datum]
    [_ (raise-arguments-error who
                              "expected a compiled provider WAT module datum"
                              "cache key" cache-key
                              "datum" datum)]))

;; ensure-module-bridge-provider-compiled-fragment-wat-cache! :
;;   any/c (-> any/c) -> (values (or/c 'hit 'write) path?)
;;   Materialize compiled provider-fragment WAT, compiling only on a cache miss.
(define (ensure-module-bridge-provider-compiled-fragment-wat-cache!
         cache-key compile-provider-thunk)
  (define wat-path
    (module-bridge-provider-compiled-fragment-wat-cache-path cache-key))
  (cond
    [(module-bridge-provider-compiled-fragment-wat-cache-current? cache-key)
     (provider-cache-log "#lang webracket/base compiled provider fragment hit: ~a"
                         wat-path)
     (values 'hit wat-path)]
    [else
     (call-with-provider-cache-lock
      wat-path
      (lambda ()
        (cond
          [(module-bridge-provider-compiled-fragment-wat-cache-current?
            cache-key)
           (provider-cache-log "#lang webracket/base compiled provider fragment hit: ~a"
                               wat-path)
           (values 'hit wat-path)]
          [else
           (provider-cache-log "#lang webracket/base compiled provider fragment miss; writing: ~a"
                               wat-path)
           (define compiled-provider-wat
             (compile-provider-thunk))
           (define written-path
             (write-module-bridge-provider-compiled-fragment-wat-cache!
              cache-key
              compiled-provider-wat))
           (values 'write written-path)])))]))

;; read-wat-module-from-file : path-string? symbol? -> list?
;;   Read one generated WAT module datum from disk.
(define (read-wat-module-from-file wat-path who)
  (define datum (call-with-input-file wat-path read))
  (match datum
    [`(module ,_ ...) datum]
    [_ (raise-arguments-error who
                              "expected a WAT module datum"
                              "path" wat-path
                              "datum" datum)]))

;; compiled-fragment->program-wat : any/c -> any/c
;;   Extract the program-side WAT from a normal or split-runtime compile result.
(define (compiled-fragment->program-wat compiled-fragment)
  (if (split-runtime-modules? compiled-fragment)
      (split-runtime-modules-program compiled-fragment)
      compiled-fragment))

;; split-runtime-modules-with-program :
;;   split-runtime-modules? any/c -> split-runtime-modules?
;;   Preserve split-runtime core/cache metadata while replacing the program module.
(define (split-runtime-modules-with-program split-modules program-wat)
  (split-runtime-modules
   (split-runtime-modules-core split-modules)
   program-wat
   (split-runtime-modules-imported-names split-modules)
   (split-runtime-modules-import-forms split-modules)
   (split-runtime-modules-core-cache-key split-modules)
   (split-runtime-modules-ffi-module split-modules)
   (split-runtime-modules-ffi-cache-key split-modules)))

;; module-bridge-graph-metric-rows : graph composition -> (listof (list/c string? any/c))
;;   Summarize bridge scaffolding and graph shape before the main compiler pipeline.
(define (module-bridge-graph-metric-rows graph composition)
  (define reports (webracket/base-local-module-graph-reports graph))
  (define datum   (webify-module-composition-datum composition))
  (define operator-counts
    (module-bridge-datum-operator-counts
     datum
     '(module-variable-make
       module-variable-define!
       module-variable-ref
       module-variable-ref/no-check
       module-variable-set!
       module-variable-set!/check-undefined!
       unsafe-struct-ref
       unsafe-struct*-ref
       unsafe-struct?
       unsafe-struct*?)))
  (define (operator-count name)
    (hash-ref operator-counts name 0))
  (define stats   (or (current-webify-bridge-stats) (make-hash)))
  (define elision-enabled?
    (current-module-bridge-elide-public-cells?))
  (define local-module-count (length reports))
  (define direct-provider-names (webracket/base-external-provider-names reports))
  (define graph-module-count
    (hash-count (webify-module-composition-instances composition)))
  (define module-kinds
    (webracket/base-local-module-graph-module-kinds graph))
  (define fragment-manifest
    (module-bridge-fragment-manifest composition module-kinds))
  (hash-set! stats 'module-bridge-fragment-manifest fragment-manifest)
  (define-values (fragment-kind-form-counts
                  fragment-kind-counts
                  fragment-module-form-counts
                  fragment-form-count)
    (module-bridge-fragment-record-counts fragment-manifest))
  (define total-export-count
    (for/sum ([report (in-list reports)])
      (length (webify-module-report-phase0-exports report))))
  (define total-import-count
    (for/sum ([report (in-list reports)])
      (length (webify-module-report-phase0-imports report))))
  (define reexport-count
    (for/sum ([report (in-list reports)])
      (module-bridge-report-reexport-count report)))
  (define role-counts
    (bridge-export-cell-role-counts graph stats))
  (define public-cell-counts
    (bridge-public-cell-required-counts graph stats))
  (define public-cell-candidate-not-elided-counts
    (bridge-public-cell-candidate-not-elided-counts graph stats))
  (define installed-provider-candidate-class-counts
    (bridge-installed-provider-candidate-class-counts graph stats))
  (define installed-provider-elided-class-counts
    (bridge-public-cell-elided-installed-provider-class-counts graph stats))
  (define racket-installed-provider-kept-reason-counts
    (bridge-racket-installed-provider-kept-reason-counts graph stats))
  (define (stat key [default 0])
    (hash-ref stats key default))
  (list
   (list "bridge-module-variable-make-count"
         (operator-count 'module-variable-make))
   (list "bridge-module-variable-define-count"
         (operator-count 'module-variable-define!))
   (list "bridge-module-variable-ref-count"
         (operator-count 'module-variable-ref))
   (list "bridge-module-variable-ref-no-check-count"
         (operator-count 'module-variable-ref/no-check))
   (list "bridge-module-variable-set-count"
         (+ (operator-count 'module-variable-set!)
            (operator-count 'module-variable-set!/check-undefined!)))
   (list "bridge-export-cell-count" (stat 'bridge-export-cell-count))
   (list "bridge-export-cell-local-user-count" (hash-ref role-counts 'local-user 0))
   (list "bridge-export-cell-support-provider-count" (hash-ref role-counts 'support-provider 0))
   (list "bridge-export-cell-installed-provider-count" (hash-ref role-counts 'installed-provider 0))
   (list "bridge-export-cell-kernel-provider-count" (hash-ref role-counts 'kernel-provider 0))
   (list "bridge-export-cell-reexport-count" (stat 'bridge-export-cell-reexport-count))
   (list "bridge-export-cell-internal-count"
         (- (stat 'bridge-export-cell-count)
            (stat 'bridge-export-cell-public-boundary-count)))
   (list "bridge-export-cell-mutated-count" (stat 'bridge-export-cell-mutated-count))
   (list "bridge-export-cell-undefined-sensitive-count"
         (stat 'bridge-export-cell-undefined-sensitive-count))
   (list "bridge-export-cell-reflection-visible-count"
         (stat 'bridge-export-cell-reflection-visible-count))
   (list "bridge-export-cell-public-boundary-count"
         (stat 'bridge-export-cell-public-boundary-count))
   (list "bridge-export-cell-late-materialization-candidate-count"
         (stat 'bridge-export-cell-late-materialization-candidate-count))
   (list "bridge-public-cell-count"
         (stat 'bridge-export-cell-public-boundary-count))
   (list "module-bridge-public-cell-elision-mode"
         (if elision-enabled? "enabled" "disabled"))
   (list "bridge-public-cell-required-reflection-count"
         (hash-ref public-cell-counts 'required-reflection 0))
   (list "bridge-public-cell-required-dynamic-require-count"
         (hash-ref public-cell-counts 'required-dynamic-require 0))
   (list "bridge-public-cell-required-mutation-count"
         (hash-ref public-cell-counts 'required-mutation 0))
   (list "bridge-public-cell-required-undefined-count"
         (hash-ref public-cell-counts 'required-undefined 0))
   (list "bridge-public-cell-required-reexport-count"
         (hash-ref public-cell-counts 'required-reexport 0))
   (list "bridge-public-cell-required-phase-space-count"
         (hash-ref public-cell-counts 'required-phase-space 0))
   (list "bridge-public-cell-required-unknown-count"
         (hash-ref public-cell-counts 'required-unknown 0))
   (list "bridge-public-cell-plain-value-candidate-count"
         (hash-ref public-cell-counts 'plain-value-candidate 0))
   (list "bridge-public-cell-elided-count"
         (stat 'bridge-public-cell-elided-count))
   (list "bridge-public-cell-kept-count"
         (stat 'bridge-public-cell-kept-count))
   (list "bridge-public-cell-kept-required-count"
         (stat 'bridge-public-cell-kept-required-count))
   (list "bridge-public-cell-kept-mutation-count"
         (stat 'bridge-public-cell-kept-mutation-count))
   (list "bridge-public-cell-kept-undefined-count"
         (stat 'bridge-public-cell-kept-undefined-count))
   (list "bridge-public-cell-kept-reflection-count"
         (stat 'bridge-public-cell-kept-reflection-count))
   (list "bridge-public-cell-kept-dynamic-require-count"
         (stat 'bridge-public-cell-kept-dynamic-require-count))
   (list "bridge-public-cell-kept-reexport-count"
         (stat 'bridge-public-cell-kept-reexport-count))
   (list "bridge-public-cell-kept-unknown-count"
         (stat 'bridge-public-cell-kept-unknown-count))
   (list "bridge-public-cell-top-required-reasons"
         (bridge-top-count-summary
          (for/hasheq ([(key value) (in-hash public-cell-counts)]
                       #:unless (eq? key 'plain-value-candidate))
            (values key value))))
   (list "bridge-public-cell-top-plain-value-candidate-modules"
         (bridge-top-count-summary
          (bridge-public-cell-candidate-module-counts graph stats)))
   (list "bridge-public-cell-top-plain-value-candidate-names"
         (bridge-top-count-summary
          (bridge-public-cell-candidate-name-counts graph stats)))
   (list "bridge-public-cell-candidate-not-elided-count"
         (hash-ref public-cell-candidate-not-elided-counts
                   'candidate-not-elided
                   0))
   (list "bridge-public-cell-candidate-not-elided-local-only-policy-count"
         (hash-ref public-cell-candidate-not-elided-counts
                   'candidate-not-elided-local-only-policy
                   0))
   (list "bridge-public-cell-candidate-not-elided-provider-export-count"
         (hash-ref public-cell-candidate-not-elided-counts
                   'candidate-not-elided-provider-export
                   0))
   (list "bridge-public-cell-candidate-not-elided-support-provider-count"
         (hash-ref public-cell-candidate-not-elided-counts
                   'candidate-not-elided-support-provider
                   0))
   (list "bridge-public-cell-candidate-not-elided-support-provider-top-modules"
         (bridge-top-count-summary
          (bridge-public-cell-candidate-not-elided-support-provider-module-counts
           graph
           stats)))
   (list "bridge-public-cell-candidate-not-elided-support-provider-top-names"
         (bridge-top-count-summary
          (bridge-public-cell-candidate-not-elided-support-provider-name-counts
           graph
           stats)))
   (list "bridge-public-cell-candidate-not-elided-support-provider-details"
         (bridge-public-cell-candidate-not-elided-support-provider-details
          graph
          stats))
   (list "bridge-public-cell-candidate-not-elided-installed-provider-count"
         (hash-ref public-cell-candidate-not-elided-counts
                   'candidate-not-elided-installed-provider
                   0))
   (list "bridge-public-cell-candidate-not-elided-installed-provider-top-modules"
         (bridge-top-count-summary
          (bridge-installed-provider-candidate-module-counts graph stats)))
   (list "bridge-public-cell-candidate-not-elided-installed-provider-top-names"
         (bridge-top-count-summary
          (bridge-installed-provider-candidate-name-counts graph stats)))
   (list "bridge-public-cell-candidate-not-elided-installed-provider-details"
         (bridge-installed-provider-candidate-class-details
          graph
          stats
          'webracket-owned-installed-provider))
   (list "bridge-installed-provider-candidate-webracket-owned-count"
         (hash-ref installed-provider-candidate-class-counts
                   'webracket-owned-installed-provider
                   0))
   (list "bridge-installed-provider-candidate-racket-count"
         (hash-ref installed-provider-candidate-class-counts
                   'racket-installed-provider
                   0))
   (list "bridge-installed-provider-candidate-third-party-count"
         (hash-ref installed-provider-candidate-class-counts
                   'third-party-installed-provider
                   0))
   (list "bridge-installed-provider-candidate-unknown-count"
         (hash-ref installed-provider-candidate-class-counts
                   'unknown-installed-provider
                   0))
   (list "bridge-installed-provider-racket-candidate-details"
         (bridge-installed-provider-candidate-class-details
          graph
          stats
          'racket-installed-provider))
   (list "bridge-public-cell-elided-webracket-owned-installed-provider-count"
         (hash-ref installed-provider-elided-class-counts
                   'webracket-owned-installed-provider
                   0))
   (list "bridge-public-cell-kept-webracket-owned-installed-provider-count"
         (hash-ref installed-provider-candidate-class-counts
                   'webracket-owned-installed-provider
                   0))
   (list "bridge-public-cell-elided-racket-installed-provider-count"
         (hash-ref installed-provider-elided-class-counts
                   'racket-installed-provider
                   0))
   (list "bridge-public-cell-kept-racket-installed-provider-count"
         (hash-ref installed-provider-candidate-class-counts
                   'racket-installed-provider
                   0))
   (list "bridge-racket-installed-provider-elision-blocked-metadata-count"
         (hash-ref racket-installed-provider-kept-reason-counts
                   'metadata
                   0))
   (list "bridge-racket-installed-provider-elision-blocked-abi-count"
         (hash-ref racket-installed-provider-kept-reason-counts
                   'abi
                   0))
   (list "bridge-racket-installed-provider-elision-blocked-policy-count"
         (hash-ref racket-installed-provider-kept-reason-counts
                   'policy
                   0))
   (list "bridge-public-cell-candidate-not-elided-kernel-provider-count"
         (hash-ref public-cell-candidate-not-elided-counts
                   'candidate-not-elided-kernel-provider
                   0))
   (list "bridge-public-cell-candidate-not-elided-importer-needs-cell-count"
         (hash-ref public-cell-candidate-not-elided-counts
                   'candidate-not-elided-importer-needs-cell
                   0))
   (list "bridge-public-cell-candidate-not-elided-registry-shape-count"
         (hash-ref public-cell-candidate-not-elided-counts
                   'candidate-not-elided-registry-shape
                   0))
   (list "bridge-public-cell-candidate-not-elided-unknown-count"
         (hash-ref public-cell-candidate-not-elided-counts
                   'candidate-not-elided-unknown
                   0))
   (list "bridge-public-cell-candidate-not-elided-top-modules"
         (bridge-top-count-summary
          (bridge-public-cell-candidate-not-elided-module-counts graph stats)))
   (list "bridge-public-cell-candidate-not-elided-top-names"
         (bridge-top-count-summary
          (bridge-public-cell-candidate-not-elided-name-counts graph stats)))
   (list "bridge-export-cell-elision-candidate-count"
         (hash-ref public-cell-counts 'plain-value-candidate 0))
   (list "bridge-export-cell-elision-blocked-reexport-count"
         (hash-ref public-cell-counts 'required-reexport 0))
   (list "bridge-export-cell-elision-blocked-mutated-count"
         (hash-ref public-cell-counts 'required-mutation 0))
   (list "bridge-export-cell-elision-blocked-undefined-count"
         (hash-ref public-cell-counts 'required-undefined 0))
   (list "bridge-export-cell-elision-blocked-reflection-count"
         (hash-ref public-cell-counts 'required-reflection 0))
   (list "bridge-export-cell-elision-blocked-unknown-count"
         (hash-ref public-cell-counts 'required-unknown 0))
   (list "bridge-export-cell-top-modules"
         (bridge-top-count-summary (bridge-export-cell-module-counts stats)))
   (list "bridge-export-cell-top-names"
         (bridge-top-count-summary (bridge-export-cell-name-counts stats)))
   (list "bridge-import-cell-count" (stat 'bridge-import-cell-count))
   (list "bridge-direct-import-count" (stat 'bridge-direct-import-count))
   (list "bridge-direct-local-ref-count" (stat 'bridge-direct-local-ref-count))
   (list "bridge-cell-elision-count" (stat 'bridge-cell-elision-count))
   (list "bridge-import-from-plain-public-value-count"
         (stat 'bridge-import-from-plain-public-value-count))
   (list "bridge-import-from-cell-public-value-count"
         (stat 'bridge-import-from-cell-public-value-count))
   (list "bridge-import-abi-false-count"
         (stat 'bridge-import-abi-false-count))
   (list "bridge-import-abi-false-top-modules"
         (bridge-top-count-summary
          (bridge-import-abi-false-module-counts stats)))
   (list "bridge-import-abi-false-top-names"
         (bridge-top-count-summary
          (bridge-import-abi-false-name-counts stats)))
   (list "bridge-import-abi-false-top-provider-kinds"
         (bridge-top-count-summary
          (bridge-import-abi-false-provider-kind-counts stats)))
   (list "bridge-import-abi-false-top-reasons"
         (bridge-top-count-summary
          (bridge-import-abi-false-reason-counts stats)))
   (list "bridge-import-abi-false-kernel-provider-count"
         (stat 'bridge-import-abi-false-kernel-provider-count))
   (list "bridge-import-abi-false-kernel-top-names"
         (bridge-top-count-summary
          (bridge-import-abi-false-kernel-counts
           stats
           (lambda (entry) (hash-ref entry 'import-name #f)))))
   (list "bridge-import-abi-false-kernel-top-importers"
         (bridge-top-count-summary
          (bridge-import-abi-false-kernel-counts
           stats
           (lambda (entry) (hash-ref entry 'importer-module #f)))))
   (list "bridge-import-abi-false-kernel-details"
         (bridge-import-abi-false-kernel-details stats))
   (list "bridge-import-abi-false-unknown-top-modules"
         (bridge-top-count-summary
          (bridge-import-abi-false-filtered-counts
           stats
           'unknown
           (lambda (entry) (hash-ref entry 'provider-module #f)))))
   (list "bridge-import-abi-false-unknown-top-names"
         (bridge-top-count-summary
          (bridge-import-abi-false-filtered-counts
           stats
           'unknown
           (lambda (entry) (hash-ref entry 'export-name #f)))))
   (list "bridge-import-abi-false-unknown-provider-kind-top-modules"
         (bridge-top-count-summary
          (bridge-import-abi-false-filtered-counts
           stats
           'unknown-provider-kind
           (lambda (entry) (hash-ref entry 'provider-module #f)))))
   (list "bridge-import-abi-false-unknown-provider-kind-top-names"
         (bridge-top-count-summary
          (bridge-import-abi-false-filtered-counts
           stats
           'unknown-provider-kind
           (lambda (entry) (hash-ref entry 'export-name #f)))))
   (list "bridge-import-abi-false-elision-candidate-count"
         (bridge-import-abi-false-elision-candidate-count graph stats))
   (list "bridge-import-abi-false-cell-forcing-count"
         (stat 'bridge-import-abi-false-cell-forcing-count))
   (list "bridge-import-abi-resolved-from-prior-export-knowns-count"
         (stat 'bridge-import-abi-resolved-from-prior-export-knowns-count))
   (list "bridge-import-abi-resolved-from-kernel-knowns-count"
         (stat 'bridge-import-abi-resolved-from-kernel-knowns-count))
   (list "bridge-import-abi-still-false-after-prior-knowns-count"
         (stat 'bridge-import-abi-still-false-after-prior-knowns-count))
   (list "bridge-import-abi-false-kernel-after-knowns-count"
         (stat 'bridge-import-abi-false-kernel-after-knowns-count))
   (list "bridge-abi-only-import-group-knowns-forced-count"
         (stat 'bridge-abi-only-import-group-knowns-forced-count))
   (list "bridge-import-from-plain-local-value-count"
         (stat 'bridge-import-from-plain-local-value-count))
   (list "bridge-import-from-cell-local-value-count"
         (stat 'bridge-import-from-cell-local-value-count))
   (list "bridge-import-from-plain-provider-value-count"
         (stat 'bridge-import-from-plain-provider-value-count))
   (list "bridge-import-from-cell-provider-value-count"
         (stat 'bridge-import-from-cell-provider-value-count))
   (list "bridge-unsafe-struct-ref-count"
         (+ (operator-count 'unsafe-struct-ref)
            (operator-count 'unsafe-struct*-ref)))
   (list "bridge-unsafe-struct-predicate-count"
         (+ (operator-count 'unsafe-struct?)
            (operator-count 'unsafe-struct*?)))
   (list "provider-count" (length direct-provider-names))
   (list "local-module-count" local-module-count)
   (list "graph-module-count" graph-module-count)
   (list "bridge-fragment-count"
         (length (webify-module-composition-fragments composition)))
   (list "bridge-fragment-form-count" fragment-form-count)
   (list "bridge-fragment-local-user-count"
         (hash-ref fragment-kind-counts 'local-user 0))
   (list "bridge-fragment-support-provider-count"
         (hash-ref fragment-kind-counts 'support-provider 0))
   (list "bridge-fragment-installed-provider-count"
         (hash-ref fragment-kind-counts 'installed-provider 0))
   (list "bridge-fragment-kernel-provider-count"
         (hash-ref fragment-kind-counts 'kernel-provider 0))
   (list "bridge-fragment-unknown-provider-count"
         (hash-ref fragment-kind-counts 'unknown-provider 0))
   (list "bridge-fragment-local-user-form-count"
         (hash-ref fragment-kind-form-counts 'local-user 0))
   (list "bridge-fragment-support-provider-form-count"
         (hash-ref fragment-kind-form-counts 'support-provider 0))
   (list "bridge-fragment-installed-provider-form-count"
         (hash-ref fragment-kind-form-counts 'installed-provider 0))
   (list "bridge-fragment-kernel-provider-form-count"
         (hash-ref fragment-kind-form-counts 'kernel-provider 0))
   (list "bridge-fragment-unknown-provider-form-count"
         (hash-ref fragment-kind-form-counts 'unknown-provider 0))
   (list "bridge-fragment-top-modules"
         (bridge-top-count-summary fragment-module-form-counts))
   (list "export-count" total-export-count)
   (list "import-count" total-import-count)
   (list "reexport-count" reexport-count)
   (list "transitive-provider-count" (max 0 (- graph-module-count local-module-count)))))

(define current-source-module-language
  (make-parameter #f))

(define current-source-module-provider-names
  (make-parameter #f))

;; provider-report-cache-directory : -> path?
;;   Return the shared runtime-cache directory for installed provider reports.
(define (provider-report-cache-directory)
  (runtime-provider-cache-path "installed-provider-reports"))

;; provider-report-cache-entries : -> (listof (list/c path? integer? integer?))
;;   Report installed-provider cache files for diagnostics and tests.
(define (provider-report-cache-entries)
  (define dir (provider-report-cache-directory))
  (if (directory-exists? dir)
      (sort
       (for/list ([path (in-list (find-files file-exists? dir))]
                  #:when (file-exists? path))
         (list path
               (file-size path)
               (file-or-directory-modify-seconds path)))
       string<?
       #:key (lambda (entry) (path->string (car entry))))
      '()))

;; driver-timing-add! : symbol? exact-integer? -> void?
;;   Record fine-grained source-reading timing rows for `--timings`.
(define (driver-timing-add! name elapsed-ms)
  (current-read-source-timing-rows
   (cons (list (symbol->string name) elapsed-ms)
         (current-read-source-timing-rows))))

;; with-driver-timing : symbol? (-> any/c) -> any/c
;;   Time a small driver phase and append it to the source-reading rows.
(define (with-driver-timing name thunk)
  (define start-ms (now-ms))
  (begin0
    (thunk)
    (driver-timing-add! name (- (now-ms) start-ms))))

;; module-bridge-timing-add! : symbol? exact-integer? -> void?
;;   Record fine-grained module-bridge extraction rows for `--timings`.
(define (module-bridge-timing-add! name elapsed-ms)
  (current-module-bridge-detail-rows
   (cons (list (symbol->string name) elapsed-ms)
         (current-module-bridge-detail-rows))))

;; with-module-bridge-timing : symbol? (-> any/c) -> any/c
;;   Time one module-bridge extraction phase and append it to the detail rows.
(define (with-module-bridge-timing name thunk)
  (define start-ms (now-ms))
  (begin0
    (thunk)
    (module-bridge-timing-add! name (- (now-ms) start-ms))))

;; default-provider-cache-project-root : path?
;;   Keep project/extraction caches separate from stable staged-runtime caches.
(define default-provider-cache-project-root
  (build-path webify-regexp-default-cache-root "project"))

;; default-runtime-provider-cache-root : path?
;;   Reuse stable runtime-provider artifacts across private project caches.
(define default-runtime-provider-cache-root
  webify-regexp-default-cache-root)

;; runtime-provider-cache-path : path-string? ... -> path?
;;   Build a path below the stable runtime-provider cache root.
(define (runtime-provider-cache-path . elems)
  (apply build-path (current-runtime-cache-dir) elems))

;; source-file-direct-textual-include-cache : hash?
;;   Memoize direct include scans by source path, size, and mtime.
(define source-file-direct-textual-include-cache (make-hash))

;; source-file-cache-key : path? -> (or/c list? #f)
;;   Return a cache key for reading direct include forms from one source file.
(define (source-file-cache-key source-path)
  (and (file-exists? source-path)
       (list (path->string (simplify-path source-path))
             (file-size source-path)
             (file-or-directory-modify-seconds source-path))))

;; source-file-skip-leading-lang-line! : input-port? -> void?
;;   Skip a leading #lang line before reading body forms as ordinary data.
(define (source-file-skip-leading-lang-line! in)
  (define pos (file-position in))
  (define maybe-line (read-line in 'any))
  (cond
    [(and (string? maybe-line)
          (regexp-match? #rx"^#lang($|[ \t])" maybe-line))
     (void)]
    [else
     (file-position in pos)]))

;; source-file-forms : path? -> (listof any/c)
;;   Read ordinary data forms from a source file for lightweight include scans.
(define (source-file-forms source-path)
  (with-handlers ([exn:fail? (lambda (_exn) '())])
    (call-with-input-file source-path
      (lambda (in)
        (source-file-skip-leading-lang-line! in)
        (let loop ([forms '()])
          (define form (read in))
          (cond
            [(eof-object? form) (reverse forms)]
            [else              (loop (cons form forms))]))))))

;; literal-textual-include-paths : any/c -> (listof path-string?)
;;   Collect literal source paths from top-level textual include forms.
(define (literal-textual-include-paths form)
  (define (literal-path v)
    (and (path-string? v) v))
  (match form
    [`(include ,path . ,_)
     (cond
       [(literal-path path) => list]
       [else                  '()])]
    [`(include/reader ,path . ,_)
     (cond
       [(literal-path path) => list]
       [else                  '()])]
    [`(include-at/relative-to ,_ ,path . ,_)
     (cond
       [(literal-path path) => list]
       [else                  '()])]
    [`(include-at/relative-to/reader ,_ ,path . ,_)
     (cond
       [(literal-path path) => list]
       [else                  '()])]
    [`(begin ,body ...)
     (append-map literal-textual-include-paths body)]
    [`(begin-for-syntax ,body ...)
     (append-map literal-textual-include-paths body)]
    [`(#%module-begin ,body ...)
     (append-map literal-textual-include-paths body)]
    [`(module ,_ ,_ ,body ...)
     (append-map literal-textual-include-paths body)]
    [`(module* ,_ ,_ ,body ...)
     (append-map literal-textual-include-paths body)]
    [`(module+ ,_ ,body ...)
     (append-map literal-textual-include-paths body)]
    [_ '()]))

;; resolve-textual-include-path : path? path-string? -> (or/c path? #f)
;;   Resolve a literal include path relative to the including source file.
(define (resolve-textual-include-path source-path include-path)
  (with-handlers ([exn:fail? (lambda (_exn) #f)])
    (define base-dir (or (path-only source-path) (current-directory)))
    (define resolved
      (simplify-path
       (path->complete-path
        (if (path? include-path)
            include-path
            (string->path include-path))
        base-dir)))
    (and (file-exists? resolved) resolved)))

;; source-file-direct-textual-include-paths : path? -> (listof path?)
;;   Return direct literal textual includes from one source file.
(define (source-file-direct-textual-include-paths source-path)
  (define resolved-source (simplify-path (path->complete-path source-path)))
  (define key (source-file-cache-key resolved-source))
  (cond
    [key
     (hash-ref!
      source-file-direct-textual-include-cache
      key
      (lambda ()
        (remove-duplicates
         (filter values
                 (for/list ([include-path
                             (in-list
                              (append-map literal-textual-include-paths
                                          (source-file-forms resolved-source)))])
                   (resolve-textual-include-path resolved-source include-path)))
         equal?)))]
    [else '()]))

;; source-paths-with-textual-includes : (listof path-string?) -> (listof path?)
;;   Add transitive literal textual include dependencies to source paths.
(define (source-paths-with-textual-includes source-paths)
  (define visited (make-hash))
  (define (visit source-path)
    (define resolved (simplify-path (path->complete-path source-path)))
    (cond
      [(hash-ref visited resolved #f) '()]
      [else
       (hash-set! visited resolved #t)
       (cons resolved
             (append-map visit
                         (source-file-direct-textual-include-paths resolved)))]))
  (sort (append-map visit source-paths) string<? #:key path->string))

;; source-path-fingerprint : symbol? exact-integer? (listof path-string?) -> any/c
;;   Fingerprint source paths and transitive literal textual includes.
(define (source-path-fingerprint tag version source-paths)
  (path-fingerprint tag
                    version
                    (source-paths-with-textual-includes source-paths)))

;; collection-paths-fingerprint : -> (listof string?)
;;   Record the module-resolution roots that affect extracted provider reports.
(define (collection-paths-fingerprint)
  (for/list ([collection-path (in-list (current-library-collection-paths))])
    (path->string
     (simplify-path
      (path->complete-path collection-path)))))

;; provider-module-source-fingerprint : symbol? -> any/c
;;   Include provider source metadata in the disk cache key when available.
(define (provider-module-source-fingerprint name)
  (with-handlers ([exn:fail? (lambda (_exn) `(module-source unresolved ,name))])
    (define resolved
      (module-path-index-resolve
       (module-path-index-join name #f)))
    (define resolved-name (resolved-module-path-name resolved))
    (cond
      [(and (path? resolved-name) (file-exists? resolved-name))
       `(module-source
         path+textual-includes
         1
         ,(path->string (simplify-path resolved-name))
         ,(source-path-fingerprint 'module-source-files 1 (list resolved-name)))]
      [else
       `(module-source
         resolved
         ,(if (path-string? resolved-name)
              (path->string resolved-name)
              resolved-name))])))

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

;; provider-report-extractor-fingerprint : -> any/c
;;   Include bridge/extractor implementation metadata in installed-provider
;;   cache keys, since cached reports contain compiler-derived import maps.
(define (provider-report-extractor-fingerprint)
  (path-fingerprint
   'provider-report-extractor
   2
   (list (build-path webify-root "module-extractor.rkt")
         (build-path webify-root "module-extractor-spike.rkt")
         (build-path webify-root "module-input.rkt"))))

;; local-module-graph-extractor-fingerprint : -> any/c
;;   Include bridge-reader and extractor implementation metadata in cached
;;   local #lang webracket/base report graphs.
(define (local-module-graph-extractor-fingerprint)
  (path-fingerprint
   'local-module-graph-extractor
   1
   (filter values
           (list (build-path webracket-root "pkgs" "webracket-lib" "compiler" "driver.rkt")
                 (build-path webify-root "module-extractor.rkt")
                 (build-path webify-root "module-extractor-spike.rkt")
                 (build-path webify-root "module-input.rkt")
                 (webracket/base-reader-path)
                 (webracket/module-reader-path 'full)))))

;; provider-report->cache-datum : webify-module-report? -> datum?
;;   Serialize provider reports without relying on reader-visible struct types.
(define (provider-bundle-entry->cache-datum entry)
  `(bundle-entry
    ,(webify-module-bundle-entry-key entry)
    ,(webify-module-bundle-entry-kind entry)
    ,(cache-readable-datum (webify-module-bundle-entry-imports entry))
    ,(cache-readable-datum (webify-module-bundle-entry-exports entry))
    ,(cache-readable-datum (webify-module-bundle-entry-datum entry))
    ,(cache-readable-datum (webify-module-bundle-entry-value entry))))

(define (cache-datum->provider-bundle-entry datum)
  (match datum
    [`(bundle-entry ,key ,kind ,imports ,exports ,entry-datum ,value)
     (webify-module-bundle-entry key
                                 kind
                                 (cache-readable->datum imports)
                                 (cache-readable->datum exports)
                                 (cache-readable->datum entry-datum)
                                 (cache-readable->datum value))]
    [_ #f]))

(define (provider-report->cache-datum report)
  `(provider-report
    ,(webify-module-report-name report)
    ,(webify-module-report-directory-keys report)
    ,(webify-module-report-bundle-keys report)
    ,(map provider-bundle-entry->cache-datum
          (webify-module-report-bundle-entries report))
    ,(cache-readable-datum (webify-module-report-phase0-imports report))
    ,(cache-readable-datum (webify-module-report-phase0-exports report))
    ,(cache-readable-datum (webify-module-report-phase0-linklet-datum report))
    ,(webify-module-report-manifest-ready? report)
    ,(cache-readable-datum (webify-module-report-notes report))))

;; cache-datum->provider-report : datum? -> webify-module-report?
;;   Rehydrate the conservative provider-report cache representation.
(define (cache-datum->provider-report datum)
  (match datum
    [`(provider-report
       ,name
       ,directory-keys
       ,bundle-keys
       ,bundle-entries
       ,phase0-imports
       ,phase0-exports
       ,phase0-linklet-datum
       ,manifest-ready?
       ,notes)
     (define parsed-bundle-entries
       (and (list? bundle-entries)
            (for/list ([entry (in-list bundle-entries)])
              (cache-datum->provider-bundle-entry entry))))
     (and parsed-bundle-entries
          (andmap webify-module-bundle-entry? parsed-bundle-entries)
     (webify-module-report name
                           directory-keys
                           bundle-keys
                           parsed-bundle-entries
                           (cache-readable->datum phase0-imports)
                           (cache-readable->datum phase0-exports)
                           (cache-readable->datum phase0-linklet-datum)
                           manifest-ready?
                           (cache-readable->datum notes)))]
    [else #f]))

;; local-module-graph-cache-path : path-string? -> path?
;;   Cache one extracted local bridge graph per root source file.
(define (local-module-graph-cache-path filename)
  (define resolved (simplify-path (path->complete-path filename)))
  (provider-cache-path
   "local-module-graphs"
   (string-append
    (provider-cache-string-segment (path->string resolved))
    ".rktd")))

;; local-module-graph-fingerprint : (listof path?) -> any/c
;;   Fingerprint extracted local-module graphs using source files and bridge code.
(define (local-module-graph-fingerprint local-source-paths)
  `(local-module-graph-cache
    3
    ,(version)
    ,(collection-paths-fingerprint)
    ,(source-path-fingerprint 'local-module-graph-source 2 local-source-paths)
    ,(local-module-graph-extractor-fingerprint)))

;; local-module-graph->cache-datum : symbol? (listof report?) (listof path?) -> datum?
;;   Serialize extracted local-module reports plus their visited source files.
(define (local-module-graph->cache-datum root reports local-source-paths)
  `(local-module-graph
    ,root
    (reports
     ,@(for/list ([report (in-list reports)])
         (provider-report->cache-datum report)))
    (local-source-paths
     ,@(for/list ([path (in-list local-source-paths)])
         (cache-readable-datum path)))))

;; cache-datum->local-module-graph-payload : datum? -> (or/c vector? #f)
;;   Rehydrate cached local-module graph payload fields.
(define (cache-datum->local-module-graph-payload datum)
  (match datum
    [`(local-module-graph
       ,root
       (reports ,report-data ...)
       (local-source-paths ,path-data ...))
     (define reports
       (for/list ([report-datum (in-list report-data)])
         (cache-datum->provider-report report-datum)))
     (define local-source-paths
       (for/list ([path-datum (in-list path-data)])
         (cache-readable->datum path-datum)))
     (and (symbol? root)
          (andmap webify-module-report? reports)
          (andmap path? local-source-paths)
          (vector root
                  reports
                  (sort (map simplify-path local-source-paths)
                        string<?
                        #:key path->string)))]
    [_ #f]))

;; read-local-module-graph-cache : path? -> (or/c vector? #f)
;;   Read a cached local-module graph when its extracted source fingerprint still matches.
(define (read-local-module-graph-cache cache-path)
  (and (not (current-provider-cache-force?))
       (file-exists? cache-path)
       (with-handlers ([exn:fail? (lambda (_exn) #f)])
         (match (call-with-input-file cache-path read)
           [`(,cached-fingerprint ,payload)
            (define decoded (cache-datum->local-module-graph-payload payload))
            (and decoded
                 (equal? cached-fingerprint
                         (local-module-graph-fingerprint
                          (vector-ref decoded 2)))
                 decoded)]
           [_ #f]))))

;; write-local-module-graph-cache! : path? symbol? (listof report?) (listof path?) -> void?
;;   Persist one extracted local-module graph payload under its dynamic fingerprint.
(define (write-local-module-graph-cache! cache-path root reports local-source-paths)
  (write-provider-cache!
   cache-path
   (local-module-graph-fingerprint local-source-paths)
   (local-module-graph->cache-datum root reports local-source-paths)))

;; module-bridge-result-cache-path : path-string? -> path?
;;   Cache one fully composed module-bridge result per root source file.
(define (module-bridge-result-cache-path filename)
  (define resolved (simplify-path (path->complete-path filename)))
  (provider-cache-path
   "module-bridge-results"
   (string-append
    (provider-cache-string-segment (path->string resolved))
    ".rktd")))

;; current-provider-owned-primitive-alias-fingerprint : -> (listof symbol?)
;;   Return provider-owned primitive aliases in stable fingerprint order.
(define (current-provider-owned-primitive-alias-fingerprint)
  (sort (set->list (current-provider-owned-primitive-aliases)) symbol<?))

;; module-bridge-result-provider-fingerprints : hash? -> (listof list?)
;;   Fingerprint non-local provider modules that influence bridge composition.
(define (module-bridge-result-provider-fingerprints module-kinds)
  (for/list ([name (in-list (sort (hash-keys module-kinds) symbol<?))]
             #:do [(define kind (hash-ref module-kinds name))]
             #:unless (eq? kind 'local-user))
    (list name kind (provider-module-source-fingerprint name))))

;; module-bridge-result-fingerprint : (listof path?) hash? -> any/c
;;   Fingerprint a composed bridge result using source, provider, and policy state.
(define (module-bridge-result-fingerprint local-source-paths module-kinds)
  `(module-bridge-result-cache
    7
    ,(version)
    ,(current-webracket/base-provider-substitutions?)
    ,(current-module-bridge-elide-public-cells?)
    ,(collection-paths-fingerprint)
    ,(current-provider-owned-primitive-alias-fingerprint)
    ,(local-module-graph-extractor-fingerprint)
    ,(source-path-fingerprint
      'module-bridge-result-local-sources
      2
      local-source-paths)
    ,(module-bridge-result-provider-fingerprints module-kinds)))

;; module-bridge-result->cache-datum :
;;   (listof path?) (listof symbol?) hash? (or/c #f (listof symbol?))
;;   any/c any/c any/c any/c any/c boolean? boolean? any/c -> datum?
;;   Serialize the side metadata and lowered top-level datum produced by the bridge.
(define (module-bridge-result->cache-datum local-source-paths
                                           provider-names
                                           module-kinds
                                           elidable-module-names
                                           root-export-bindings
                                           root-source-path
                                           metric-rows
                                           fragment-manifest
                                           provider-fragment-fingerprint
                                           needs-expander-read-provider?
                                           needs-regexp-provider?
                                           lowered-datum)
  `(module-bridge-result
    (local-source-paths
     ,@(for/list ([path (in-list local-source-paths)])
         (cache-readable-datum path)))
    (provider-names ,provider-names)
    (module-kinds ,(cache-readable-datum module-kinds))
    (elidable-module-names ,(cache-readable-datum elidable-module-names))
    (console-root-export-bindings ,(cache-readable-datum root-export-bindings))
    (console-root-source-path ,(cache-readable-datum root-source-path))
    (metric-rows ,(cache-readable-datum metric-rows))
    (fragment-manifest ,(cache-readable-datum fragment-manifest))
    (provider-fragment-fingerprint ,(cache-readable-datum provider-fragment-fingerprint))
    (needs-expander-read-provider? ,needs-expander-read-provider?)
    (needs-regexp-provider? ,needs-regexp-provider?)
    (lowered-top-level-datum ,(cache-readable-datum lowered-datum))))

;; module-bridge-fragment-record? : any/c -> boolean?
;;   Check one cached bridge fragment ownership/span record.
(define (module-bridge-fragment-record? record)
  (and (hash? record)
       (symbol? (hash-ref record 'module-name #f))
       (symbol? (hash-ref record 'kind #f))
       (exact-nonnegative-integer? (hash-ref record 'start #f))
       (exact-nonnegative-integer? (hash-ref record 'count #f))))

;; module-bridge-fragment-manifest? : any/c -> boolean?
;;   Check a cached bridge fragment manifest.
(define (module-bridge-fragment-manifest? manifest)
  (and (list? manifest)
       (andmap module-bridge-fragment-record? manifest)))

;; cache-datum->module-bridge-result-payload : any/c -> (or/c vector? #f)
;;   Rehydrate one cached bridge result and its validation metadata.
(define (cache-datum->module-bridge-result-payload datum)
  (match datum
    [`(module-bridge-result
       (local-source-paths ,path-data ...)
       (provider-names ,provider-names)
       (module-kinds ,module-kinds-datum)
       (elidable-module-names ,elidable-module-names-datum)
       (console-root-export-bindings ,root-export-bindings-datum)
       (console-root-source-path ,root-source-path-datum)
       (metric-rows ,metric-rows-datum)
       (fragment-manifest ,fragment-manifest-datum)
       (provider-fragment-fingerprint ,provider-fragment-fingerprint-datum)
       (needs-expander-read-provider? ,needs-expander-read-provider?)
       (needs-regexp-provider? ,needs-regexp-provider?)
       (lowered-top-level-datum ,lowered-datum))
     (define local-source-paths
       (for/list ([path-datum (in-list path-data)])
         (cache-readable->datum path-datum)))
     (define module-kinds
       (cache-readable->datum module-kinds-datum))
     (define elidable-module-names
       (cache-readable->datum elidable-module-names-datum))
     (define root-export-bindings
       (cache-readable->datum root-export-bindings-datum))
     (define root-source-path
       (cache-readable->datum root-source-path-datum))
     (define metric-rows
       (cache-readable->datum metric-rows-datum))
     (define fragment-manifest
       (cache-readable->datum fragment-manifest-datum))
     (define provider-fragment-fingerprint
       (cache-readable->datum provider-fragment-fingerprint-datum))
     (and (andmap path? local-source-paths)
          (symbol-list? provider-names)
          (hash? module-kinds)
          (or (not elidable-module-names)
              (symbol-list? elidable-module-names))
          (module-bridge-fragment-manifest? fragment-manifest)
          (boolean? needs-expander-read-provider?)
          (boolean? needs-regexp-provider?)
          (vector
           (sort (map simplify-path local-source-paths)
                 string<?
                 #:key path->string)
           provider-names
           module-kinds
           elidable-module-names
           root-export-bindings
           root-source-path
           metric-rows
           fragment-manifest
           provider-fragment-fingerprint
           needs-expander-read-provider?
           needs-regexp-provider?
           (cache-readable->datum lowered-datum)))]
    [_ #f]))

(module+ test
  (require rackunit)

  ;; local-report-fingerprint/collections : (listof path?) -> any/c
  ;;   Evaluate a local-report cache key under one collection search path.
  (define (local-report-fingerprint/collections collection-paths)
    (parameterize ([current-library-collection-paths collection-paths])
      (local-module-report-fingerprint (build-path webify-root "module-input.rkt"))))
  (define collection-root-a
    (build-path (find-system-path 'temp-dir) "webracket-collections-a"))
  (define collection-root-b
    (build-path (find-system-path 'temp-dir) "webracket-collections-b"))
  (check-not-equal?
   (local-report-fingerprint/collections (list collection-root-a))
   (local-report-fingerprint/collections (list collection-root-b)))

  (define sample-fragment-manifest
    (list (hasheq 'module-name 'user/main
                  'kind 'local-user
                  'start 0
                  'count 2)
          (hasheq 'module-name 'racket-src/expander/read/main
                  'kind 'support-provider
                  'start 2
                  'count 57)))
  (define sample-provider-fragment-fingerprint
    '(module-bridge-provider-fragment-cache sample))
  (define sample-module-bridge-cache-datum
    (module-bridge-result->cache-datum
     (list (build-path (current-directory) "sample.rkt"))
     '(expander/read)
     (hasheq 'user/main 'local-user
             'racket-src/expander/read/main 'support-provider)
     '(user/main)
     '()
     "sample.rkt"
     '(("bridge-fragment-count" 2))
     sample-fragment-manifest
     sample-provider-fragment-fingerprint
     #t
     #f
     '(begin (define x 1) (void))))
  (define sample-module-bridge-cache-payload
    (cache-datum->module-bridge-result-payload
     sample-module-bridge-cache-datum))
  (check-true (vector? sample-module-bridge-cache-payload))
  (check-equal? (vector-ref sample-module-bridge-cache-payload 7)
                sample-fragment-manifest)
  (check-equal? (vector-ref sample-module-bridge-cache-payload 8)
                sample-provider-fragment-fingerprint)
  (define sample-split-fragment-manifest
    (list (hasheq 'module-name 'provider/a
                  'kind 'support-provider
                  'start 0
                  'count 2)
          (hasheq 'module-name 'user/main
                  'kind 'local-user
                  'start 2
                  'count 2)
          (hasheq 'module-name 'provider/b
                  'kind 'installed-provider
                  'start 4
                  'count 1)))
  (define sample-fragment-split
    (module-bridge-split-lowered-datum
     '(begin provider-a provider-b user-a user-b provider-c tail)
     sample-split-fragment-manifest))
  (check-equal? (module-bridge-fragment-split-provider-datum
                 sample-fragment-split)
                '(begin provider-a provider-b provider-c))
  (check-equal? (module-bridge-fragment-split-program-datum
                 sample-fragment-split)
                '(begin user-a user-b tail))
  (check-equal? (map (lambda (record) (hash-ref record 'module-name))
                     (module-bridge-fragment-split-provider-records
                      sample-fragment-split))
                '(provider/a provider/b))
  (check-equal? (map (lambda (record) (hash-ref record 'module-name))
                     (module-bridge-fragment-split-program-records
                      sample-fragment-split))
                '(user/main))
  (check-equal? (module-bridge-fragment-split-tail-forms
                 sample-fragment-split)
                '(tail))
  (define same-provider-fragment-split
    (module-bridge-split-lowered-datum
     '(begin provider-a provider-b changed-user-a changed-user-b provider-c changed-tail)
     sample-split-fragment-manifest))
  (check-equal? (module-bridge-provider-fragment-fingerprint sample-fragment-split)
                (module-bridge-provider-fragment-fingerprint same-provider-fragment-split))
  (define changed-provider-fragment-split
    (module-bridge-split-lowered-datum
     '(begin provider-a changed-provider-b user-a user-b provider-c tail)
     sample-split-fragment-manifest))
  (check-not-equal? (module-bridge-provider-fragment-fingerprint sample-fragment-split)
                    (module-bridge-provider-fragment-fingerprint
                     changed-provider-fragment-split))
  (define sample-provider-fragment-cache-root
    (let ([tmp-parent (if (directory-exists? "/tmp")
                          "/tmp"
                          (find-system-path 'temp-dir))])
      (make-temporary-file "webracket-driver-provider-fragment-cache-~a"
                           'directory
                           tmp-parent)))
  (parameterize ([current-provider-cache-dir sample-provider-fragment-cache-root]
                 [current-provider-cache-force? #f])
    (define fingerprint
      (module-bridge-provider-fragment-fingerprint sample-fragment-split))
    (check-equal? (ensure-module-bridge-provider-fragment-cache!
                   fingerprint
                   sample-fragment-split)
                  'write)
    (check-equal? (ensure-module-bridge-provider-fragment-cache!
                   fingerprint
                   sample-fragment-split)
                  'hit)
    (define cached-provider-fragment
      (read-module-bridge-provider-fragment-cache fingerprint))
    (check-true (vector? cached-provider-fragment))
    (check-equal? (vector-ref cached-provider-fragment 0)
                  (module-bridge-fragment-split-provider-records
                   sample-fragment-split))
    (check-equal? (vector-ref cached-provider-fragment 1)
                  '(begin provider-a provider-b provider-c))
    (define compiled-cache-key
      (module-bridge-provider-compiled-fragment-cache-key
       fingerprint
       #:browser? #f
       #:enable-cc-analysis? #f
       #:enable-simplify? #f
       #:letrec-strategy 'scc
       #:provider-owned-primitive-aliases '(regexp? read)))
    (define compile-count 0)
    (define compiled-provider-stub-wat
      `(module (data $payload ,(wasm-data (bytes 202 99 97 99 104 101)))))
    (define (compile-provider-stub)
      (set! compile-count (+ compile-count 1))
      compiled-provider-stub-wat)
    (define-values (compiled-cache-status compiled-cache-path)
      (ensure-module-bridge-provider-compiled-fragment-wat-cache!
       compiled-cache-key
       compile-provider-stub))
    (check-eq? compiled-cache-status 'write)
    (check-true (file-exists? compiled-cache-path))
    (check-true
     (module-bridge-provider-compiled-fragment-wat-cache-current?
      compiled-cache-key))
    (check-true
     (file-exists?
      (module-bridge-provider-compiled-fragment-datum-cache-path
       compiled-cache-key)))
    (match (read-module-bridge-provider-compiled-fragment-cache
            compiled-cache-key
            'driver-test)
      [`(module (data $payload ,(? wasm-data? payload)))
       (check-equal? (wasm-data-ref payload)
                     (bytes 202 99 97 99 104 101))]
      [other
       (fail-check (format "unexpected compiled provider cache value: ~s" other))])
    (check-equal? compile-count 1)
    (define-values (compiled-cache-status* compiled-cache-path*)
      (ensure-module-bridge-provider-compiled-fragment-wat-cache!
       compiled-cache-key
       compile-provider-stub))
    (check-eq? compiled-cache-status* 'hit)
    (check-equal? compiled-cache-path* compiled-cache-path)
    (check-equal? compile-count 1))
  (delete-directory/files sample-provider-fragment-cache-root)
  (check-exn exn:fail?
             (lambda ()
               (module-bridge-split-lowered-datum
                '(begin a b c)
                (list (hasheq 'module-name 'a
                              'kind 'local-user
                              'start 0
                              'count 1)
                      (hasheq 'module-name 'b
                              'kind 'support-provider
                              'start 2
                              'count 1)))))
  (check-false
   (cache-datum->module-bridge-result-payload
    `(module-bridge-result
      (local-source-paths)
      (provider-names ())
      (module-kinds ,(cache-readable-datum (hasheq)))
      (elidable-module-names #f)
      (console-root-export-bindings ())
      (console-root-source-path #f)
      (metric-rows ())
      (fragment-manifest ((not-a-valid-record)))
      (provider-fragment-fingerprint (invalid))
      (needs-expander-read-provider? #f)
      (needs-regexp-provider? #f)
      (lowered-top-level-datum (begin (void)))))))

;; read-module-bridge-result-cache : path? -> (or/c vector? #f)
;;   Read a cached bridge result when its source/provider fingerprint still matches.
(define (read-module-bridge-result-cache cache-path)
  (and (not (current-provider-cache-force?))
       (file-exists? cache-path)
       (with-handlers ([exn:fail? (lambda (_exn) #f)])
         (match (call-with-input-file cache-path read)
           [`(,cached-fingerprint ,payload)
            (define decoded
              (cache-datum->module-bridge-result-payload payload))
            (and decoded
                 (equal? cached-fingerprint
                         (module-bridge-result-fingerprint
                          (vector-ref decoded 0)
                          (vector-ref decoded 2)))
                 decoded)]
           [_ #f]))))

;; write-module-bridge-result-cache! :
;;   path? (listof path?) (listof symbol?) hash? (or/c #f (listof symbol?))
;;   any/c any/c any/c any/c any/c boolean? boolean? any/c -> void?
;;   Persist one fully composed bridge result under its dynamic fingerprint.
(define (write-module-bridge-result-cache! cache-path
                                           local-source-paths
                                           provider-names
                                           module-kinds
                                           elidable-module-names
                                           root-export-bindings
                                           root-source-path
                                           metric-rows
                                           fragment-manifest
                                           provider-fragment-fingerprint
                                           needs-expander-read-provider?
                                           needs-regexp-provider?
                                           lowered-datum)
  (write-provider-cache!
   cache-path
   (module-bridge-result-fingerprint local-source-paths module-kinds)
   (module-bridge-result->cache-datum local-source-paths
                                      provider-names
                                      module-kinds
                                      elidable-module-names
                                      root-export-bindings
                                      root-source-path
                                      metric-rows
                                      fragment-manifest
                                      provider-fragment-fingerprint
                                      needs-expander-read-provider?
                                      needs-regexp-provider?
                                      lowered-datum)))

;; module-bridge-result-cache-payload->syntax : vector? -> syntax?
;;   Restore bridge side channels and return the cached lowered top-level syntax.
(define (module-bridge-result-cache-payload->syntax payload)
  (current-module-bridge-local-source-paths (vector-ref payload 0))
  (current-source-module-provider-names (vector-ref payload 1))
  (current-module-bridge-module-kinds (vector-ref payload 2))
  (current-module-bridge-elidable-module-names (vector-ref payload 3))
  (current-console-bridge-root-export-bindings (vector-ref payload 4))
  (current-console-bridge-root-source-path (vector-ref payload 5))
  (hash-set! (current-webify-bridge-stats)
             'module-bridge-metric-rows
             (vector-ref payload 6))
  (hash-set! (current-webify-bridge-stats)
             'module-bridge-fragment-manifest
             (vector-ref payload 7))
  (hash-set! (current-webify-bridge-stats)
             'module-bridge-provider-fragment-fingerprint
             (vector-ref payload 8))
  (current-module-bridge-needs-expander-read-provider? (vector-ref payload 9))
  (current-module-bridge-needs-regexp-provider? (vector-ref payload 10))
  (datum->syntax #f (vector-ref payload 11)))

;; module-bridge-result-cache-payload->fragment-split :
;;   vector? -> module-bridge-fragment-split?
;;   Reconstruct the provider/program fragment split from a cached bridge result.
(define (module-bridge-result-cache-payload->fragment-split payload)
  (module-bridge-split-lowered-datum (vector-ref payload 11)
                                     (vector-ref payload 7)))

;; module-bridge-result-cache-payload->provider-fragment-fingerprint :
;;   vector? -> any/c
;;   Return the cached provider-side fragment fingerprint.
(define (module-bridge-result-cache-payload->provider-fragment-fingerprint payload)
  (vector-ref payload 8))

;; local-module-report-cache-path : path-string? -> path?
;;   Cache one extracted local module report per source file.
(define (local-module-report-cache-path filename)
  (define resolved (simplify-path (path->complete-path filename)))
  (provider-cache-path
   "local-module-reports"
   (string-append
    (provider-cache-string-segment (path->string resolved))
    ".rktd")))

;; local-module-report-fingerprint : path-string? -> any/c
;;   Fingerprint one local module report by source and bridge/extractor code.
(define (local-module-report-fingerprint filename)
  (define resolved (simplify-path (path->complete-path filename)))
  `(local-module-report-cache
    3
    ,(version)
    ,(current-webracket/base-provider-substitutions?)
    ,(collection-paths-fingerprint)
    ,(source-path-fingerprint 'local-module-report-source 2 (list resolved))
    ,(local-module-graph-extractor-fingerprint)))

;; local-module-report->cache-datum : webify-module-report? (listof path?) -> datum?
;;   Serialize one local report with the dependency paths needed for traversal.
(define (local-module-report->cache-datum report dependency-paths)
  `(local-module-report
    ,(provider-report->cache-datum report)
    (dependency-paths
     ,@(for/list ([path (in-list dependency-paths)])
         (cache-readable-datum path)))))

;; cache-datum->local-module-report-payload : any/c -> (or/c vector? #f)
;;   Rehydrate one cached local report and its dependency path list.
(define (cache-datum->local-module-report-payload datum)
  (match datum
    [`(local-module-report ,report-datum (dependency-paths ,path-data ...))
     (define report (cache-datum->provider-report report-datum))
     (define dependency-paths
       (for/list ([path-datum (in-list path-data)])
         (cache-readable->datum path-datum)))
     (and (webify-module-report? report)
          (andmap path? dependency-paths)
          (vector report
                  (map simplify-path dependency-paths)))]
    [_ #f]))

;; read-local-module-report-cache : path-string? -> (or/c vector? #f)
;;   Read one cached local module report when its source fingerprint matches.
(define (read-local-module-report-cache filename)
  (define cache-path (local-module-report-cache-path filename))
  (define fingerprint (local-module-report-fingerprint filename))
  (and (not (current-provider-cache-force?))
       (file-exists? cache-path)
       (with-handlers ([exn:fail? (lambda (_exn) #f)])
         (match (call-with-input-file cache-path read)
           [`(,cached-fingerprint ,payload)
            (define decoded (cache-datum->local-module-report-payload payload))
            (and decoded
                 (equal? cached-fingerprint fingerprint)
                 decoded)]
           [_ #f]))))

;; write-local-module-report-cache! : path-string? webify-module-report? (listof path?) -> void?
;;   Persist one local module report under its dynamic source fingerprint.
(define (write-local-module-report-cache! filename report dependency-paths)
  (define cache-path (local-module-report-cache-path filename))
  (write-provider-cache!
   cache-path
   (local-module-report-fingerprint filename)
   (local-module-report->cache-datum report dependency-paths)))

;; cached-installed-provider-report : symbol? (symbol? -> report?) -> report?
;;   Reuse extracted installed-provider reports across driver runs.
(define (cached-installed-provider-report name fetch-provider)
  (define cache-path
    (build-path
     (provider-report-cache-directory)
     (string-append (provider-cache-symbol-segment name) ".rktd")))
  (define fingerprint
    `(installed-provider-report-cache
      10
      ,name
      ,(version)
      ,(collection-paths-fingerprint)
      ,(provider-module-source-fingerprint name)
      ,(provider-report-extractor-fingerprint)))
  (define cached
    (with-module-bridge-timing
     'provider-cache-lookup
     (lambda ()
       (read-provider-cache cache-path fingerprint))))
  (cond
    [cached
     (provider-cache-log "installed provider report hit: ~a" name)
     (or (cache-datum->provider-report cached)
         (let ()
           (provider-cache-log "installed provider report cache unreadable; extracting: ~a" name)
           (define report
             (with-module-bridge-timing
              'provider-report-extraction-miss
              (lambda ()
                (fetch-provider name))))
           (write-provider-cache! cache-path
                                  fingerprint
                                  (provider-report->cache-datum report))
           report))]
    [else
     (provider-cache-log "installed provider report miss; extracting: ~a" name)
     (define report
       (with-module-bridge-timing
        'provider-cache-miss
        (lambda ()
          (with-module-bridge-timing
           'provider-report-extraction-miss
           (lambda ()
             (fetch-provider name))))))
     (write-provider-cache! cache-path fingerprint (provider-report->cache-datum report))
     report]))

;; log-discovered-provider-graph-cache-miss! : path? any/c -> void?
;;   Explain why the discovered provider-graph cache did not hit.
(define (log-discovered-provider-graph-cache-miss! cache-path fingerprint)
  (when (and (current-provider-cache-verbose?)
             (file-exists? cache-path))
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (provider-cache-log
                        "#lang webracket/base discovered provider graph cache unreadable: ~a"
                        (exn-message e)))])
      (match (call-with-input-file cache-path read)
        [`(,cached-fingerprint ,_payload)
         (unless (equal? cached-fingerprint fingerprint)
           (define labels
             '(tag
               cache-version
               root
               demand-slice?
               module-protocol-dependency
               racket-version
               collection-paths
               extractor-fingerprint
               local-source-fingerprint
               support-provider-fingerprints))
           (define cached-parts
             (if (list? cached-fingerprint) cached-fingerprint '()))
           (define current-parts
             (if (list? fingerprint) fingerprint '()))
           (define mismatch-index
             (for/or ([label (in-list labels)]
                      [cached-part (in-list cached-parts)]
                      [current-part (in-list current-parts)]
                      [i (in-naturals)])
               (and (not (equal? cached-part current-part))
                    (list i label cached-part current-part))))
           (match mismatch-index
             [(list _i label cached-part current-part)
              (provider-cache-log
               "#lang webracket/base discovered provider graph fingerprint mismatch at ~a"
               label)
              (provider-cache-log "  cached: ~s" cached-part)
              (provider-cache-log "  current: ~s" current-part)]
             [_ (provider-cache-log
                 "#lang webracket/base discovered provider graph fingerprint mismatch in unlabelled tail")]))]
        [_ (provider-cache-log
            "#lang webracket/base discovered provider graph cache has unexpected shape")]))))

;; module-linklet-input->cache-datum : webify-module-linklet-input? -> datum?
;;   Serialize one resolved linklet input using the manifest handoff shape.
(define (cache-readable-datum v)
  (cond
    [(void? v)
     'webracket-cache:void]
    [(eq? v unsafe-undefined)
     'webracket-cache:unsafe-undefined]
    [(path? v)
     `(webracket-cache:path ,(path->string v))]
    [(pair? v)
     (cons (cache-readable-datum (car v))
           (cache-readable-datum (cdr v)))]
    [(vector? v)
     `(webracket-cache:vector
       ,@(for/list ([elem (in-vector v)])
           (cache-readable-datum elem)))]
    [(box? v)
     `(webracket-cache:box ,(cache-readable-datum (unbox v)))]
    [(srcloc? v)
     `(webracket-cache:srcloc
       ,(cache-readable-datum (srcloc-source v))
       ,(srcloc-line v)
       ,(srcloc-column v)
       ,(srcloc-position v)
       ,(srcloc-span v))]
    [(prefab-struct-key v)
     (define data (struct->vector v))
     `(webracket-cache:prefab
       ,(prefab-struct-key v)
       ,@(for/list ([i (in-range 1 (vector-length data))])
           (cache-readable-datum (vector-ref data i))))]
    [(hash? v)
     `(webracket-cache:hash
       ,(cond
          [(hash-eq? v)  'eq]
          [(hash-eqv? v) 'eqv]
          [else          'equal])
       ,(for/list ([(key value) (in-hash v)])
          (list (cache-readable-datum key)
                (cache-readable-datum value))))]
    [else v]))

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
       [`(webracket-cache:srcloc ,source ,line ,column ,position ,span)
        (srcloc (cache-readable->datum source)
                line
                column
                position
                span)]
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

(define (module-linklet-input->cache-datum input)
  `(input
    ,(webify-module-linklet-input-name input)
    ,(cache-readable-datum (webify-module-linklet-input-linklet input))
    (imports ,(webify-module-linklet-input-imports input))))

;; module-linklet-manifest->cache-datum : (listof input?) (listof symbol?) -> datum?
;;   Serialize cached discovered inputs using the manifest handoff format.
(define (module-linklet-manifest->cache-datum inputs roots)
  `(webify-module-linklet-manifest
    (roots ,roots)
    ,@(map module-linklet-input->cache-datum inputs)))

;; discovered-provider-graph-fingerprint : symbol? (listof path?) (listof symbol?) boolean? any/c -> any/c
;;   Fingerprint discovered provider graphs using stable source file metadata.
(define (discovered-provider-graph-fingerprint root
                                               local-source-paths
                                               support-provider-names
                                               demand-slice?
                                               module-protocol-dependency)
  `(discovered-provider-graph-cache
    3
    ,root
    ,demand-slice?
    ,module-protocol-dependency
    ,(version)
    ,(collection-paths-fingerprint)
    ,(provider-report-extractor-fingerprint)
    ,(source-path-fingerprint
      'discovered-provider-graph-local-sources
      2
      local-source-paths)
    ,(for/list ([name (in-list support-provider-names)])
       (provider-module-source-fingerprint name))))

;; cached-discovered-provider-registry : symbol? (listof path?) (listof symbol?) (-> inputs) -> registry
;;   Cache the expensive discovered provider graph as resolved linklet inputs.
(define (cached-discovered-provider-registry root
                                             local-source-paths
                                             support-provider-names
                                             demand-slice?
                                             module-protocol-dependency
                                             discover-inputs)
  (define cache-path
    (provider-cache-path
     "discovered-provider-graphs"
     (string-append (provider-cache-symbol-segment root) ".rktd")))
  (define fingerprint
    (with-module-bridge-timing
     'discover-provider-graph-fingerprint
     (lambda ()
       (discovered-provider-graph-fingerprint root
                                              local-source-paths
                                              support-provider-names
                                              demand-slice?
                                              module-protocol-dependency))))
  (define cached
    (with-module-bridge-timing
     'discover-provider-graph-cache-lookup
     (lambda ()
       (read-provider-cache cache-path fingerprint))))
  (cond
    [cached
     (provider-cache-log "#lang webracket/base discovered provider graph hit: ~a" root)
     (define manifest
       (with-module-bridge-timing
        'discover-provider-graph-cache-hit-decode
        (lambda ()
          (datum->webify-module-linklet-manifest
           (fasl->s-exp cached)))))
     (with-module-bridge-timing
      'discover-provider-graph-cache-hit
      (lambda ()
        (webify-module-linklet-manifest->registry manifest)))]
    [else
     (log-discovered-provider-graph-cache-miss! cache-path fingerprint)
     (provider-cache-log "#lang webracket/base discovered provider graph miss: ~a" root)
     (define inputs
       (with-module-bridge-timing
        'discover-provider-graph-cache-miss
        discover-inputs))
     (write-provider-cache! cache-path
                            fingerprint
                            (s-exp->fasl
                             (module-linklet-manifest->cache-datum inputs (list root))))
     (webify-module-linklet-inputs->registry inputs)]))

;; staged-io-source-fingerprint : symbol? -> any/c
;;   Fingerprint staged io provider inputs.
(define (staged-io-source-fingerprint cache-tag)
  (path-fingerprint
   cache-tag
   1
   (cons (build-path webify-root "io-format-bridge.rkt")
         (cons (build-path webify-root "io-print-bridge.rkt")
               (find-files file-exists? (build-path webracket-root "racket-src/io"))))))

;; io-format-source-fingerprint : -> any/c
;;   Fingerprint the staged io/format provider inputs.
(define (io-format-source-fingerprint)
  (staged-io-source-fingerprint 'io-format-provider-cache))

;; io-print-source-fingerprint : -> any/c
;;   Fingerprint the staged io/print provider inputs.
(define (io-print-source-fingerprint)
  (staged-io-source-fingerprint 'io-print-provider-cache))

;; provider-slice-forms : symbol? #:public-aliases? boolean? -> (listof datum?)
;;   Compose a provider slice and expose export cells, optionally publishing
;;   public top-level aliases.
(define (provider-slice-forms name
                              #:public-aliases? [public-aliases? #f]
                              #:owner [owner 'top-level-stdlib])
  (case name
    [(io/format)
     (io-format-provider-forms #:public-aliases? public-aliases?)]
    [(io/print)
     (io-print-provider-forms #:public-aliases? public-aliases?)]
    [(regexp)
     (regexp-provider-forms #:public-aliases? public-aliases?)]
    [(expander/read)
     (read-provider-forms #:public-aliases? public-aliases?)]
    [(expander/read-syntax)
     (read-syntax-provider-forms)]
    [else
     (error 'provider-slice-forms "provider slice is not runtime-integrated yet: ~a" name)]))

;; io-format-provider-forms : #:public-aliases? boolean? -> (listof datum?)
;;   Compose the staged original-Racket io/format provider and expose export
;;   cells, optionally publishing public top-level aliases.
(define (io-format-provider-forms #:public-aliases? [public-aliases? #f])
  (define cache-path
    (runtime-provider-cache-path
     (if public-aliases?
         "io-format-provider-public.rktd"
         "io-format-provider-private.rktd")))
  (define fingerprint
    `(io-format-lowered-provider-cache 1 ,public-aliases? ,(io-format-source-fingerprint)))
  (define cached (read-provider-cache cache-path fingerprint))
  (cond
    [cached
     (provider-cache-log "io/format lowered forms hit: ~a" cache-path)
     cached]
    [else
     (provider-cache-log "io/format lowered forms miss; rebuilding: ~a" cache-path)
     (define composition
       (webify-compose-module-registry/roots/composition
        (webify-io-format-registry)
        (list webify-io-format-root-name)
        #:define-export-aliases? #f
        #:serializable?-box     #f
        #:datum-intern?         #f
        #:target                #f
        #:allow-set!-undefined? #f
        #:unsafe-mode?          #f
        #:enforce-constant?     #t
        #:allow-inline?         #t
        #:no-prompt?            #f
        #:prim-knowns           (hasheq)
        #:primitives            (hasheq)
        #:compiler-query        (lambda (v) #f)
        #:get-import-knowns     (lambda (key) (values #f #f #f))
        #:import-keys           #f))
     (define forms
       (cdr (webify-module-composition-with-tail
             composition
             ((if public-aliases?
                  webify-io-format-public-alias-forms
                  webify-io-format-private-alias-forms)
              composition))))
     (write-provider-cache! cache-path fingerprint forms)
     forms]))

;; io-print-provider-forms : #:public-aliases? boolean? -> (listof datum?)
;;   Compose the staged original-Racket io/print provider seed and expose export
;;   cells, optionally publishing public top-level aliases.
(define (io-print-provider-forms #:public-aliases? [public-aliases? #f])
  (define cache-path
    (runtime-provider-cache-path
     (if public-aliases?
         "io-print-provider-public.rktd"
         "io-print-provider-private.rktd")))
  (define fingerprint
    `(io-print-lowered-provider-cache 2 ,public-aliases? ,(io-print-source-fingerprint)))
  (define cached (read-provider-cache cache-path fingerprint))
  (cond
    [cached
     (provider-cache-log "io/print lowered forms hit: ~a" cache-path)
     cached]
    [else
     (provider-cache-log "io/print lowered forms miss; rebuilding: ~a" cache-path)
     (define composition
       (webify-compose-module-registry/roots/composition
        (webify-io-print-registry)
        (list webify-io-print-root-name)
        #:define-export-aliases? #f
        #:serializable?-box     #f
        #:datum-intern?         #f
        #:target                #f
        #:allow-set!-undefined? #f
        #:unsafe-mode?          #f
        #:enforce-constant?     #t
        #:allow-inline?         #t
        #:no-prompt?            #f
        #:prim-knowns           (hasheq)
        #:primitives            (hasheq)
        #:compiler-query        (lambda (v) #f)
        #:get-import-knowns     (lambda (key) (values #f #f #f))
        #:import-keys           #f))
     (define forms
       (cdr (webify-module-composition-with-tail
             composition
             ((if public-aliases?
                  webify-io-print-public-alias-forms
                  webify-io-print-private-alias-forms)
              composition))))
     (write-provider-cache! cache-path fingerprint forms)
     forms]))

;; io-format-provider-reports : -> (listof webify-module-report?)
;;   Expose staged io/format as the implementation report for `racket/format`.
(define (io-format-provider-reports)
  (define-values (adapter-report printf-report main-report map-report)
    (webify-io-format-reports))
  (define format-report
    (parameterize ([current-directory webracket-root])
      (webify-module-path->report (string->path "racket-src/racket/format-web.rkt"))))
  (list adapter-report
        (webify-module-report/rename adapter-report 'racket-src/io/format/printf-adapter)
        printf-report
        (webify-module-report/rename printf-report 'racket-src/io/format/printf-web)
        main-report
        (webify-module-report/rename main-report 'racket-src/io/format/main-web)
        map-report
        (webify-module-report/rename format-report 'racket-src/racket/format-web)
        (webify-module-report/rename format-report 'racket/format)))

;; io-format-provider-report/ref : symbol? -> (or/c webify-module-report? #f)
;;   Resolve staged io/format support aliases without falling back to generic
;;   module-path extraction. Some aliases, such as `racket-src/racket/format-web`,
;;   are report names but not real collection-path module names.
(define (io-format-provider-report/ref name)
  (for/or ([report (in-list (io-format-provider-reports))])
    (and (eq? name (webify-module-report-name report))
         report)))

;; io-print-provider-reports : -> (listof webify-module-report?)
;;   Expose staged io/print as the implementation report for high-level printing.
(define (io-print-provider-reports)
  (define-values (adapter-report main-report)
    (webify-io-print-reports))
  (list adapter-report
        (webify-module-report/rename adapter-report 'racket-src/io/format/printf-adapter)
        main-report
        (webify-module-report/rename main-report 'racket-src/io/print/main-web)
        (webify-module-report/rename main-report 'io/print)))

;; io-print-provider-report/ref : symbol? -> (or/c webify-module-report? #f)
;;   Resolve staged io/print support aliases without generic module extraction.
(define (io-print-provider-report/ref name)
  (for/or ([report (in-list (io-print-provider-reports))])
    (and (eq? name (webify-module-report-name report))
         report)))

;; read-provider-wrapper-module : datum?
;;   Public staged `expander/read` facade that re-exports reader aliases.
(define read-provider-wrapper-module
  `(module expander-read-public racket/base
     (require (file "racket-src/expander/read-provider-root.rkt"))
     (provide ,@(map car webify-read-public-alias-pairs))
     ,@(for/list ([alias (in-list webify-read-public-alias-pairs)])
         `(define ,(car alias) ,(cdr alias)))))

(define (expander-read-private-provider-name? name)
  (define s (and (symbol? name) (symbol->string name)))
  (and s
       (or (equal? s "racket-src/expander/read-provider-root.rkt")
           (string-prefix? s "racket-src/expander/read/")
           (string-prefix? s "racket-src/expander/common/")
           (string-prefix? s "racket-src/expander/host/"))))

(define (expander-read-private-provider-path name)
  (define s (symbol->string name))
  (string->path
   (if (regexp-match? #rx"[.]rkt$" s)
       s
       (string-append s ".rkt"))))

(define (expander-read-private-provider-file-backed? name)
  (file-exists? (build-path webracket-root
                            (expander-read-private-provider-path name))))

;; expander-read-private-provider-path->name : path-string? -> symbol?
;;   Convert one staged expander/read source path to its provider report name.
(define (expander-read-private-provider-path->name path)
  (define resolved (simplify-path (path->complete-path path)))
  (define relative
    (path->string (find-relative-path (simplify-path webracket-root) resolved)))
  (string->symbol
   (if (equal? resolved (simplify-path webify-read-provider-source))
       relative
       (regexp-replace #rx"[.]rkt$" relative ""))))

;; expander-read-private-provider-source-path? : path-string? -> boolean?
;;   Check whether a source path belongs to the staged expander/read closure.
(define (expander-read-private-provider-source-path? path)
  (and (file-exists? path)
       (expander-read-private-provider-name?
        (expander-read-private-provider-path->name path))))

;; expander-read-private-provider-dependency-paths : path-string? -> (listof path?)
;;   Read local string requires from one staged expander/read source file.
(define (expander-read-private-provider-dependency-paths path)
  (define resolved (simplify-path (path->complete-path path)))
  (define source-dir (or (path-only resolved) (current-directory)))
  (define module-datum
    (parameterize ([read-accept-reader #t])
      (call-with-input-file resolved
        (lambda (in)
          (syntax->datum (read-syntax resolved in))))))
  (define forms
    (match module-datum
      [`(module ,_name ,_language (#%module-begin ,body ...)) body]
      [`(module ,_name ,_language ,body ...) body]
      [_ '()]))
  (filter expander-read-private-provider-source-path?
          (append-map (lambda (form)
                        (local-require-form-paths form source-dir))
                      forms)))

;; read-provider-report-names : -> (listof symbol?)
;;   Collect staged expander/read report names without compiling the reports.
(define read-provider-report-names/cache (box #f))

(define (read-provider-report-names)
  (cond
    [(unbox read-provider-report-names/cache)
     => values]
    [else
     (define visited (make-hasheq))
     (define ordered-reverse '())
     (define (visit! path)
       (define resolved (simplify-path (path->complete-path path)))
       (define name (expander-read-private-provider-path->name resolved))
       (unless (hash-ref visited name #f)
         (hash-set! visited name #t)
         (set! ordered-reverse (cons name ordered-reverse))
         (for ([dependency (in-list
                            (expander-read-private-provider-dependency-paths
                             resolved))])
           (visit! dependency))))
     (visit! webify-read-provider-source)
     (define names
       (append (reverse ordered-reverse)
               (list 'expander/read)))
     (set-box! read-provider-report-names/cache names)
     names]))

;; read-provider-reports : -> (listof webify-module-report?)
;;   Expose the staged expander/read private implementation closure plus the
;;   public facade used for user-module alias ownership.
(define read-provider-reports/cache (box #f))

(define (read-provider-reports)
  (cond
    [(unbox read-provider-reports/cache)
     => values]
    [else
     (define built
       (parameterize ([current-directory webracket-root])
         (parameterize ([current-provider-owned-primitive-aliases (seteq)])
           (define visited (make-hasheq))
           (define ordered-reports '())
           (define (visit! name)
             (unless (hash-ref visited name #f)
               (hash-set! visited name #t)
               (define report
                 (let ([report
                        (webify-module-path->report
                         (expander-read-private-provider-path name))])
                   (if (eq? name (webify-module-report-name report))
                       report
                       (webify-module-report/rename report name))))
               (set! ordered-reports (append ordered-reports (list report)))
               (for ([required (in-list (webify-module-report-required-provider-names report))]
                     #:when (and (expander-read-private-provider-name? required)
                                 (expander-read-private-provider-file-backed? required)))
                 (visit! required))))
           (visit! 'racket-src/expander/read-provider-root.rkt)
           (append
            ordered-reports
            (list
             (webify-module-report/rename
              (webify-compile-module-report read-provider-wrapper-module)
              'expander/read))))))
     (set-box! read-provider-reports/cache built)
     built]))

;; read-provider-report/ref : symbol? -> (or/c webify-module-report? #f)
;;   Resolve staged expander/read support aliases without generic extraction.
(define (read-provider-report/ref name)
  (for/or ([report (in-list (read-provider-reports))])
    (and (eq? name (webify-module-report-name report))
         report)))

;; read-syntax-provider-forms : -> (listof datum?)
;;   Compose the private staged original-Racket read-syntax provider and publish
;;   only Webify-owned aliases. Public `read-syntax` ownership is still pending.
(define (read-syntax-provider-forms)
  (define runtime-cache-root (current-runtime-cache-dir))
  (define cache-path
    (webify-read-syntax-lowered-provider-cache-path runtime-cache-root))
  (define fingerprint
    `(expander-read-syntax-lowered-provider-cache 1
                                                  ,(webify-read-syntax-source-fingerprint)))
  (define cached (read-provider-cache cache-path fingerprint))
  (cond
    [cached
     (provider-cache-log "expander/read-syntax lowered forms hit: ~a" cache-path)
     cached]
    [else
     (provider-cache-log "expander/read-syntax lowered forms miss; rebuilding: ~a"
                         cache-path)
     (define lk
       (webify-read-syntax-linklet #:cache-root runtime-cache-root
                                   #:force? (current-provider-cache-force?)
                                   #:verbose? (current-provider-cache-verbose?)))
     (define-values (provider-stx exports _exports-info)
       (webify-linklet-first-slice-top-level
        lk
        #:define-export-aliases? #f
        #:serializable?-box     #f
        #:datum-intern?         #f
        #:target                #f
        #:allow-set!-undefined? #f
        #:unsafe-mode?          #f
        #:enforce-constant?     #t
        #:allow-inline?         #t
        #:no-prompt?            #f
        #:prim-knowns           (hasheq)
        #:primitives            (hasheq)
        #:compiler-query        (lambda (v) #f)
        #:get-import-knowns     (lambda (key) (values #f #f #f))
        #:import-keys           #f))
     (unless (equal? (sort exports symbol<?)
                     (sort webify-read-syntax-export-symbols symbol<?))
       (error 'read-syntax-provider-forms
              "unexpected expander/read-syntax exports: ~s"
              exports))
     (define export-cells
       (first-slice-top-level-export-cell-ids exports))
     (define forms
       (append
        (cdr (syntax->datum provider-stx))
        (webify-read-syntax-alias-forms export-cells)))
     (write-provider-cache! cache-path fingerprint forms)
     forms]))

;; read-provider-forms : #:public-aliases? boolean? -> (listof datum?)
;;   Compose the cached original-Racket expander/read provider and optionally
;;   publish its Webify-owned `webify-expander-*` names plus public reader
;;   aliases owned by the provider.
(define (read-provider-forms #:public-aliases? [public-aliases? #f]
                             #:owner [owner 'top-level-stdlib])
  (define runtime-cache-root (current-runtime-cache-dir))
  (define cache-path
    (webify-read-lowered-provider-cache-path runtime-cache-root))
  (define fingerprint
    `(expander-read-lowered-provider-cache 2
                                           ,public-aliases?
                                           ,webify-read-public-alias-symbols
                                           ,(webify-read-source-fingerprint)))
  (define cached (read-provider-cache cache-path fingerprint))
  (cond
    [cached
     (provider-cache-log "expander/read lowered forms hit: ~a" cache-path)
     cached]
    [else
     (provider-cache-log "expander/read lowered forms miss; rebuilding: ~a" cache-path)
     (define lk
       (webify-read-linklet #:cache-root runtime-cache-root
                            #:force? (current-provider-cache-force?)
                            #:verbose? (current-provider-cache-verbose?)))
     (define-values (provider-stx exports _exports-info)
       (webify-linklet-first-slice-top-level
        lk
        #:define-export-aliases? #f
        #:serializable?-box     #f
        #:datum-intern?         #f
        #:target                #f
        #:allow-set!-undefined? #f
        #:unsafe-mode?          #f
        #:enforce-constant?     #t
        #:allow-inline?         #t
        #:no-prompt?            #f
        #:prim-knowns           (hasheq)
        #:primitives            (hasheq)
        #:compiler-query        (lambda (v) #f)
        #:get-import-knowns     (lambda (key) (values #f #f #f))
        #:import-keys           #f))
     (unless (equal? (sort exports symbol<?)
                     (sort webify-read-export-symbols symbol<?))
       (error 'read-provider-forms
              "unexpected expander/read exports: ~s"
              exports))
     (define export-cells
       (first-slice-top-level-export-cell-ids exports))
     (define forms
       (append
        (cdr (syntax->datum provider-stx))
        (if public-aliases?
            (webify-read-public-alias-forms export-cells)
            '())))
     (write-provider-cache! cache-path fingerprint forms)
     forms]))

;; regexp-provider-forms : #:public-aliases? boolean? #:cache-context symbol? -> (listof datum?)
;;   Compose the cached original-Racket regexp provider and expose export cells,
;;   optionally publishing public top-level aliases.
(define (regexp-provider-forms #:public-aliases? [public-aliases? #f]
                               #:cache-context [cache-context 'default]
                               #:owner [owner 'top-level-stdlib])
  (define runtime-cache-root (current-runtime-cache-dir))
  (define cache-path
    (if (eq? cache-context 'default)
        (webify-regexp-lowered-provider-cache-path public-aliases?
                                                  runtime-cache-root)
        (runtime-provider-cache-path
         (format "regexp-provider-~a-~a.rktd"
                 (if public-aliases? "public" "private")
                 cache-context))))
  (define fingerprint
    `(regexp-lowered-provider-cache 2
                                    ,public-aliases?
                                    ,cache-context
                                    ,(webify-regexp-source-fingerprint)))
  (define cached (read-provider-cache cache-path fingerprint))
  (cond
    [cached
     (provider-cache-log "regexp lowered forms hit: ~a" cache-path)
     cached]
    [else
     (call-with-provider-cache-lock
      cache-path
      (lambda ()
        (define cached* (read-provider-cache cache-path fingerprint))
        (cond
          [cached*
           (provider-cache-log "regexp lowered forms hit: ~a" cache-path)
           cached*]
          [else
           (provider-cache-log "regexp lowered forms miss; rebuilding: ~a" cache-path)
           (define lk
             (webify-regexp-linklet #:cache-root runtime-cache-root
                                    #:force? (current-provider-cache-force?)
                                    #:verbose? (current-provider-cache-verbose?)))
           (define-values (provider-stx exports _exports-info)
             (webify-linklet-first-slice-top-level
              lk
              #:define-export-aliases? #f
              #:serializable?-box     #f
              #:datum-intern?         #f
              #:target                #f
              #:allow-set!-undefined? #f
              #:unsafe-mode?          #f
              #:enforce-constant?     #t
              #:allow-inline?         #t
              #:no-prompt?            #f
              #:prim-knowns           (hasheq)
              #:primitives            (hasheq)
              #:compiler-query        (lambda (v) #f)
              #:get-import-knowns     (lambda (key) (values #f #f #f))
              #:import-keys           #f))
           (unless (equal? (sort exports symbol<?)
                           (sort webify-regexp-export-symbols symbol<?))
             (error 'regexp-provider-forms
                    "unexpected regexp exports: ~s"
                    exports))
           (define export-cells
             (first-slice-top-level-export-cell-ids exports))
           (define forms
             (append
              (cdr (syntax->datum provider-stx))
              (if public-aliases?
                  (webify-regexp-public-alias-forms export-cells)
                  '())))
           (write-provider-cache! cache-path fingerprint forms)
           forms])))]))

;; add-io-format-provider : syntax? #:regexp-provider? boolean? #:expander-read-provider? boolean? #:expander-read-syntax-provider? boolean? -> syntax?
;;   Prepend only the staged io/format provider, without the old stdlib root.
(define (add-io-format-provider stx
                                #:regexp-provider? [regexp-provider? #t]
                                #:expander-read-provider? [expander-read-provider? #f]
                                #:expander-read-syntax-provider? [expander-read-syntax-provider? #f])
  (define provider-forms
    (append
     (if regexp-provider?
         (regexp-provider-forms #:public-aliases? #t
                                #:cache-context (if expander-read-syntax-provider?
                                                    'read-syntax
                                                    'default))
         '())
     (if expander-read-provider?
         (provider-slice-forms 'expander/read #:public-aliases? #t)
         '())
     (if expander-read-syntax-provider?
         (provider-slice-forms 'expander/read-syntax)
         '())
     (provider-slice-forms 'io/format #:public-aliases? #t)))
  (with-syntax ([(provider-form ...) (map (lambda (form) (datum->syntax stx form stx))
                                          provider-forms)])
    #`(begin
        provider-form ...
        #,stx)))

;; add-regexp-provider : syntax? -> syntax?
;;   Prepend only the always-available regexp provider slice.
(define (add-regexp-provider stx)
  (define regexp-forms
    (provider-slice-forms 'regexp #:public-aliases? #t))
  (with-syntax ([(regexp-form ...) (map (lambda (form) (datum->syntax stx form stx))
                                        regexp-forms)])
    #`(begin
        regexp-form ...
        #,stx)))

;; add-expander-read-provider : syntax? #:expander-read-syntax-provider? boolean? -> syntax?
;;   Prepend regexp plus staged expander providers for focused replacement trials.
(define (add-expander-read-provider stx
                                    #:expander-read-syntax-provider?
                                    [expander-read-syntax-provider? #f])
  (define provider-forms
    (append
     (regexp-provider-forms #:public-aliases? #t
                            #:cache-context (if expander-read-syntax-provider?
                                                'read-syntax
                                                'default))
     (provider-slice-forms 'expander/read #:public-aliases? #t)
     (if expander-read-syntax-provider?
         (provider-slice-forms 'expander/read-syntax)
         '())))
  (with-syntax ([(provider-form ...) (map (lambda (form) (datum->syntax stx form stx))
                                          provider-forms)])
    #`(begin
        provider-form ...
        #,stx)))

;; add-formatting-numbers-slice : syntax? -> syntax?
;;   Prepend the tiny legacy formatting-numbers slice for ~r/precision demand.
(define (add-formatting-numbers-slice stx)
  (define-values (slice-forms bindings)
    (stdlib-module-private-files-forms+bindings '("formatting-numbers.rkt")))
  (define public->private
    (for/hasheq ([binding (in-list bindings)])
      (values (car binding) (cdr binding))))
  (define public '~r/precision)
  (define private (hash-ref public->private public #f))
  (unless private
    (error 'add-formatting-numbers-slice
           "formatting-numbers slice did not define ~a"
           public))
  (define alias-forms
    (append*
     (for/list ([binding (in-list bindings)]
                [i (in-naturals)])
       (define public (car binding))
       (define private (cdr binding))
       (define cell (stdlib-cell-symbol i))
       `((define ,cell (module-variable-make ',public))
         (module-variable-define! ,cell ,private 'constant)
         (define ,public (module-variable-ref ,cell))))))
  (define forms (append slice-forms alias-forms))
  (with-syntax ([(slice-form ...) (map (lambda (form) (datum->syntax stx form stx))
                                       forms)])
    #`(begin
        slice-form ...
        #,stx)))

;; add-module-stdlib : syntax? boolean? #:io-format-provider? boolean? #:expander-read-provider? boolean? #:expander-read-syntax-provider? boolean? -> syntax?
;;   Evaluate the selected stdlib root with private top-level names, publish
;;   chosen exports through module-variable cells, then append the user program.
(define (add-module-stdlib stx browser?
                           #:io-format-provider? [io-format-provider? #f]
                           #:expander-read-provider? [expander-read-provider? #f]
                           #:expander-read-syntax-provider? [expander-read-syntax-provider? #f])
  (define-values (stdlib-forms bindings)
    (stdlib-module-private-forms+bindings browser?))
  (define alias-set
    (for/hasheq ([export (in-list (stdlib-module-alias-symbols browser?))])
      (values export #t)))
  (define alias-bindings
    (filter (λ (binding)
              (define public (car binding))
              (and (hash-ref alias-set public #f)
                   ;; Primitive-owned exports are already available at the
                   ;; script top level. Re-aliasing them would recreate the
                   ;; old textual-stdlib primitive collision under the module
                   ;; stdlib bridge.
                   (not (compiler-primitive? public))))
            bindings))
  (define private->binding
    (for/hasheq ([binding (in-list alias-bindings)])
      (values (cdr binding) binding)))
  (define public->cell
    (for/hasheq ([binding (in-list alias-bindings)]
                 [i (in-naturals)])
      (values (car binding) (stdlib-cell-symbol i))))
  (define (alias-forms-after form)
    (for*/list ([private (in-list (stdlib-definition-form-symbols form))]
                [binding (in-value (hash-ref private->binding private #f))]
                #:when binding
                [alias-form (in-list
                             (let* ([public (car binding)]
                                    [cell (hash-ref public->cell public)])
                               `((define ,cell (module-variable-make ',public))
                                 (module-variable-define! ,cell ,private 'constant)
                                 (define ,public (module-variable-ref ,cell)))))])
      alias-form))
  (define stdlib-program-forms
    (append (regexp-provider-forms #:public-aliases? #t
                                   #:cache-context (if expander-read-syntax-provider?
                                                       'read-syntax
                                                       'default))
            (if io-format-provider?
                (provider-slice-forms 'io/format #:public-aliases? #t)
                '())
            (if expander-read-provider?
                (provider-slice-forms 'expander/read #:public-aliases? #t)
                '())
            (if expander-read-syntax-provider?
                (provider-slice-forms 'expander/read-syntax)
                '())
            (append-map (λ (form) (cons form (alias-forms-after form)))
                        stdlib-forms)))
  (with-syntax ([(stdlib-form ...) (map (λ (form) (datum->syntax stx form stx))
                                        stdlib-program-forms)])
    #`(begin
        stdlib-form ...
        #,stx)))

;; io-print-provider-owned-alias-symbols : (listof symbol?)
;;   High-level print names that currently come only from the staged io/print seed.
(define io-print-provider-owned-alias-symbols
  '(print println write writeln))

;; provider-owned-primitive-alias-symbols : ... -> (listof symbol?)
;;   Compute public primitive names that must resolve through provider cells.
(define (provider-owned-primitive-alias-symbols
         #:expander-read-provider? [expander-read-provider? #f]
         #:io-format-provider? [io-format-provider? #f]
         #:io-print-provider? [io-print-provider? #f]
         #:formatting-numbers? [formatting-numbers? #f])
  (append webify-regexp-export-symbols
          (if expander-read-provider?
              webify-read-public-alias-symbols
              '())
          (if io-format-provider?
              webify-io-format-export-symbols
              '())
          (if io-print-provider?
              io-print-provider-owned-alias-symbols
              '())
          (if formatting-numbers?
              '(~r/precision)
              '())))

;;;
;;; Available Primitives
;;;

;; list-available-primitives : [#:ffi-files (listof path-string?)] -> (listof symbol?)
;;   Return a sorted list of known primitives, optionally extended with FFI primitives.
(define (list-available-primitives #:ffi-files [ffi-files '()])
  (define resolved-ffi-files
    (merged-ffi-files 'list-available-primitives #f ffi-files))
  (define requested-ffi-primitives
    (append-map (λ (ffi-filename)
                  (foreigns->primitive-names (ffi-file->foreigns ffi-filename)))
                resolved-ffi-files))
  ; `primitives` may already include previously registered ffi primitives.
  ; Keep core primitives stable, then add only explicitly requested ffi primitives.
  (define core-primitives
    (filter (λ (sym) (not (memq sym ffi-primitives)))
            primitives))
  
  (sort (remove-duplicates (append core-primitives requested-ffi-primitives))
        symbol<?))


;;;
;;; THE MAIN DRIVER
;;;

;;   --timings
;;      Print timing breakdown for compilation steps.

(struct compilation-timings
  (ffi-setup     ; time spent resolving and loading FFI declarations
   read-source   ; time spent reading source and module-language input
   add-stdlib    ; time spent composing stdlib and provider slices
   preflight     ; time spent validating outputs, tools, and runtime mode
   compile       ; core compiler pipeline time
   write-wat     ; time spent writing the .wat file
   assemble      ; time spent producing the .wasm file
   write-map     ; time spent writing the label-map sidecar
   write-host    ; time spent writing the JS or HTML host wrapper
   compile-total ; compile-pipeline subtotal excluding optional run-after
   run           ; optional Node run-after time
   driver-total  ; total driver time including orchestration and optional run
   )
  #:transparent)

(struct compilation-output-paths
  (wat   ; emitted WebAssembly text path
   wasm  ; emitted WebAssembly binary path
   map   ; emitted label-map sidecar path
   host  ; emitted JS or HTML host wrapper path
   )
  #:transparent)

;; stdlib-decision-note : string? string? -> (list/c string? string?)
;;   Build one stdlib/provider diagnostic row for timing output.
(define (stdlib-decision-note label value)
  (list label value))

;; resolve-compilation-output-paths : path-string? (or/c path-string? #f) (or/c path-string? #f) (or/c path-string? #f) (or/c path-string? #f) boolean? -> compilation-output-paths?
;;   Compute the effective output paths for one compilation request.
(define (resolve-compilation-output-paths filename
                                          wat-filename
                                          wasm-filename
                                          host-filename
                                          dest-dir
                                          node?)
  (define (default-output-path ext)
    (define source-base (or (file-name-from-path filename) filename))
    (define default-name (path-replace-extension source-base ext))
    (if dest-dir
        (build-path dest-dir default-name)
        (path-replace-extension filename ext)))
  (define out-wat  (or wat-filename  (default-output-path ".wat")))
  (define out-wasm (or wasm-filename (default-output-path ".wasm")))
  (define out-map  (path-replace-extension out-wasm ".wasm.map.sexp"))
  (define out-host (or host-filename
                       (if node?
                           (default-output-path ".js")
                           (default-output-path ".html"))))
  (compilation-output-paths out-wat out-wasm out-map out-host))

;; content-addressed-sidecar-path : path-string? string? string? string? -> path?
;;   Return a content-addressed sidecar path in the same directory as sibling.
(define (content-addressed-sidecar-path sibling prefix fingerprint ext)
  (define sidecar-name (format "~a-~a~a" prefix fingerprint ext))
  (define sibling-dir (path-only sibling))
  (if sibling-dir
      (build-path sibling-dir sidecar-name)
      (string->path sidecar-name)))

;; split-runtime-sidecar-fingerprint : any/c (-> list?) -> string?
;;   Return the stable fingerprint for a split-runtime sidecar.
(define (split-runtime-sidecar-fingerprint cache-key module-thunk)
  (if cache-key
      (split-runtime-core-cache-key-fingerprint cache-key)
      (split-runtime-core-fingerprint (module-thunk))))

;; split-runtime-wat-path : path-string? string? boolean? -> path?
;;   Return the content-addressed sidecar WAT path for the split runtime module.
(define (split-runtime-wat-path out-wat fingerprint pretty-wat?)
  (content-addressed-sidecar-path out-wat
                                  "webracket-runtime"
                                  fingerprint
                                  (format ".~a.wat"
                                          (if pretty-wat?
                                              "pretty"
                                              "compact"))))

;; split-runtime-wasm-path : path-string? string? -> path?
;;   Return the content-addressed sidecar Wasm path for the split runtime module.
(define (split-runtime-wasm-path out-wasm fingerprint)
  (content-addressed-sidecar-path out-wasm "webracket-runtime" fingerprint ".wasm"))

;; split-runtime-ffi-wat-path : path-string? string? boolean? -> path?
;;   Return the content-addressed sidecar WAT path for a split FFI module.
(define (split-runtime-ffi-wat-path out-wat fingerprint pretty-wat?)
  (content-addressed-sidecar-path out-wat
                                  "webracket-ffi"
                                  fingerprint
                                  (format ".~a.wat"
                                          (if pretty-wat?
                                              "pretty"
                                              "compact"))))

;; split-runtime-ffi-wasm-path : path-string? string? -> path?
;;   Return the content-addressed sidecar Wasm path for a split FFI module.
(define (split-runtime-ffi-wasm-path out-wasm fingerprint)
  (content-addressed-sidecar-path out-wasm "webracket-ffi" fingerprint ".wasm"))

;; split-runtime-core-cache-wasm-path : string? -> path?
;;   Return the stable cache path for an assembled split core runtime module.
(define (split-runtime-core-cache-wasm-path fingerprint)
  (split-runtime-core-cache-wasm-path/cache-root
   (current-runtime-cache-dir)
   fingerprint))

;; split-runtime-core-cache-wat-path : string? boolean? -> path?
;;   Return the stable cache path for a rendered split core runtime WAT module.
(define (split-runtime-core-cache-wat-path fingerprint pretty-wat?)
  (split-runtime-core-cache-wat-path/cache-root
   (current-runtime-cache-dir)
   fingerprint
   pretty-wat?))

;; split-runtime-ffi-cache-wasm-path : string? -> path?
;;   Return the stable cache path for an assembled split FFI sidecar module.
(define (split-runtime-ffi-cache-wasm-path fingerprint)
  (split-runtime-ffi-cache-wasm-path/cache-root
   (current-runtime-cache-dir)
   fingerprint))

;; split-runtime-ffi-cache-wat-path : string? boolean? -> path?
;;   Return the stable cache path for a rendered split FFI sidecar WAT module.
(define (split-runtime-ffi-cache-wat-path fingerprint pretty-wat?)
  (split-runtime-ffi-cache-wat-path/cache-root
   (current-runtime-cache-dir)
   fingerprint
   pretty-wat?))

;; copy-file/replace! : path-string? path-string? -> void?
;;   Copy src to dest, replacing an existing file.
(define (copy-file/replace! src dest)
  (copy-file src dest #t))

;; compilation-output-cache-version : exact-positive-integer?
;;   Bump when the cached compiler-output manifest format changes.
(define compilation-output-cache-version 1)

;; default-compilation-output-cache-max-bytes : exact-positive-integer?
;;   Keep at most this many bytes of complete compiler-output bundles by default.
(define default-compilation-output-cache-max-bytes (* 1024 1024 1024))

;; default-compilation-output-cache-max-entries : exact-positive-integer?
;;   Keep at most this many complete compiler-output bundles by default.
(define default-compilation-output-cache-max-entries 12)

;; compilation-output-cache-compiler-fingerprint : -> any/c
;;   Fingerprint compiler and generated-host sources so development edits
;;   invalidate output bundles.
(define (compilation-output-cache-compiler-fingerprint)
  (define (compiler-source-file? path)
    (and (file-exists? path)
         (member (path-get-extension path) '(#".rkt" #".rktl" #".js"))))
  (path-fingerprint
   'compilation-output-cache-compiler
   1
   (append
    (find-files compiler-source-file?
                (build-path webracket-root "pkgs" "webracket-lib"))
    (find-files compiler-source-file?
                (build-path webracket-root "pkgs" "webracket")))))

;; compilation-output-cache-key-fingerprint : any/c -> string?
;;   Encode an output-cache key into a directory-safe content address.
(define (compilation-output-cache-key-fingerprint key)
  (bytes->hex-string (sha1-bytes (s-exp->fasl key))))

;; compilation-output-cache-manifest-path : any/c -> path?
;;   Return the manifest path for one cached compiler-output bundle.
(define (compilation-output-cache-manifest-path key)
  (provider-cache-path
   "compiled-outputs"
   (string-append (compilation-output-cache-key-fingerprint key) ".rktd")))

;; compilation-output-cache-directory : any/c -> path?
;;   Return the directory that stores one cached compiler-output bundle.
(define (compilation-output-cache-directory key)
  (provider-cache-path
   "compiled-outputs"
   (compilation-output-cache-key-fingerprint key)))

;; compilation-output-cache-root : -> path?
;;   Return the dedicated directory containing compiler-output cache entries.
(define (compilation-output-cache-root)
  (provider-cache-path "compiled-outputs"))

;; cache-output-file-name : path-string? -> string?
;;   Return a single filename suitable for a compiler-output cache entry.
(define (cache-output-file-name path)
  (define name (file-name-from-path path))
  (unless name
    (raise-arguments-error
     'cache-output-file-name
     "expected a path with a filename"
     "path" path))
  (path->string name))

;; cache-output-file-name? : any/c -> boolean?
;;   Recognize safe, single-component cached output filenames.
(define (cache-output-file-name? name)
  (and (string? name)
       (not (member name '("" "." "..")))
       (not (regexp-match? #px"[/\\\\]" name))))

;; compilation-output-cache-manifest : (listof pair?) (listof path-string?) -> datum?
;;   Describe direct outputs and split sidecars in a cache-readable form.
(define (compilation-output-cache-manifest direct-output-files sidecar-output-paths)
  `(compilation-output-cache-files
    ,@(append
       (for/list ([entry (in-list direct-output-files)])
         (list (car entry) (cache-output-file-name (cdr entry))))
       (for/list ([path (in-list sidecar-output-paths)])
         (list 'sidecar (cache-output-file-name path))))))

;; decode-compilation-output-cache-manifest : any/c -> (or/c (listof list?) #f)
;;   Validate and decode cached compiler-output manifest entries.
(define (decode-compilation-output-cache-manifest payload)
  (match payload
    [`(compilation-output-cache-files ,entries ...)
     (and (andmap (lambda (entry)
                    (and (list? entry)
                         (= (length entry) 2)
                         (symbol? (car entry))
                         (cache-output-file-name? (cadr entry))))
                  entries)
          (let ([names (map cadr entries)])
            (and (= (length names) (length (remove-duplicates names string=?)))
                 entries)))]
    [_ #f]))

;; direct-output-cache-entries-valid? : (listof list?) (listof pair?) -> boolean?
;;   Check that a manifest has the direct files required by this invocation.
(define (direct-output-cache-entries-valid? entries direct-output-files)
  (and
   (for/and ([entry (in-list direct-output-files)])
     (define cached (assq (car entry) entries))
     (and cached
          (string=? (cadr cached) (cache-output-file-name (cdr entry)))))
   (for/and ([entry (in-list entries)]
             #:unless (eq? (car entry) 'sidecar))
     (assq (car entry) direct-output-files))))

;; cache-output-entry-path : path? list? -> path?
;;   Resolve one cached entry within cache-dir after manifest validation.
(define (cache-output-entry-path cache-dir entry)
  (build-path cache-dir (cadr entry)))

;; compilation-output-cache-entry-name? : any/c -> boolean?
;;   Recognize content-addressed compiler-output bundle directory names.
(define (compilation-output-cache-entry-name? name)
  (and (string? name)
       (regexp-match? #px"^[0-9a-f]{40}$" name)))

;; read-compilation-output-cache-manifest : path? string? -> (or/c (listof list?) #f)
;;   Read a manifest only when its fingerprint names the expected cache entry.
(define (read-compilation-output-cache-manifest manifest-path entry-name)
  (and (file-exists? manifest-path)
       (not (link-exists? manifest-path))
       (with-handlers ([exn:fail? (lambda (_exn) #f)])
         (match (call-with-input-file manifest-path read)
           [`(,fingerprint ,payload)
            (and (equal? entry-name
                         (compilation-output-cache-key-fingerprint fingerprint))
                 (decode-compilation-output-cache-manifest payload))]
           [_ #f]))))

;; compilation-output-cache-directory-complete? : path? (listof list?) -> boolean?
;;   Recognize only complete cache directories created by this cache writer.
;;
;; Requiring an exact filename set means pruning never recursively removes an
;; unfamiliar directory, even when it happens to be below `compiled-outputs`.
(define (compilation-output-cache-directory-complete? cache-dir entries)
  (and (directory-exists? cache-dir)
       (not (link-exists? cache-dir))
       (with-handlers ([exn:fail? (lambda (_exn) #f)])
         (define expected-names (sort (map cadr entries) string<?))
         (define actual-names
           (sort (map path->string (directory-list cache-dir)) string<?))
         (and (equal? actual-names expected-names)
              (for/and ([entry (in-list entries)])
                (define entry-path (cache-output-entry-path cache-dir entry))
                (and (file-exists? entry-path)
                     (not (link-exists? entry-path))
                     (eq? (file-or-directory-type entry-path) 'file)))))))

;; compilation-output-cache-entry-info : path? string? -> (or/c list? #f)
;;   Return (list mtime bytes manifest-path cache-dir) for one safe-to-prune entry.
(define (compilation-output-cache-entry-info cache-root entry-name)
  (and (compilation-output-cache-entry-name? entry-name)
       (let* ([cache-dir (build-path cache-root entry-name)]
              [manifest-path (build-path cache-root (string-append entry-name ".rktd"))]
              [entries (read-compilation-output-cache-manifest manifest-path entry-name)])
         (and entries
              (compilation-output-cache-directory-complete? cache-dir entries)
              (with-handlers ([exn:fail? (lambda (_exn) #f)])
                (list (file-or-directory-modify-seconds manifest-path)
                      (for/sum ([entry (in-list entries)])
                        (file-size (cache-output-entry-path cache-dir entry)))
                      manifest-path
                      cache-dir))))))

;; prune-compilation-output-cache! : [#:max-bytes exact-nonnegative-integer?]
;;                                    [#:max-entries exact-nonnegative-integer?]
;;                                    [#:protect-key any/c] -> void?
;;   Evict oldest complete, unlocked compiler-output entries after a cache write.
;;
;; Manifest mtimes approximate LRU: successful cache hits refresh the mtime,
;; and ties are broken by the content-addressed entry name. Entries whose lock
;; cannot be acquired immediately, malformed entries, and unfamiliar files are
;; retained. Consequently this is a conservative best-effort budget, not a
;; hard filesystem quota.
(define (prune-compilation-output-cache!
         #:max-bytes [max-bytes default-compilation-output-cache-max-bytes]
         #:max-entries [max-entries default-compilation-output-cache-max-entries]
         #:protect-key [protect-key #f])
  (unless (exact-nonnegative-integer? max-bytes)
    (raise-argument-error 'prune-compilation-output-cache!
                          "exact-nonnegative-integer?"
                          max-bytes))
  (unless (exact-nonnegative-integer? max-entries)
    (raise-argument-error 'prune-compilation-output-cache!
                          "exact-nonnegative-integer?"
                          max-entries))
  (define cache-root (compilation-output-cache-root))
  (define protected-name
    (and protect-key (compilation-output-cache-key-fingerprint protect-key)))
  (when (directory-exists? cache-root)
    (with-handlers ([exn:fail? (lambda (_exn) (void))])
      (define entries
        (for/list ([path (in-list (directory-list cache-root))]
                   #:do [(define name (file-name-from-path path))]
                   #:do [(define entry-name (and name (path->string name)))]
                   #:when entry-name
                   #:do [(define info
                           (compilation-output-cache-entry-info cache-root entry-name))]
                   #:when info)
          (cons entry-name info)))
      (define (entry-bytes entry) (cadr (cdr entry)))
      (define (over-budget? entries*)
        (or (> (length entries*) max-entries)
            (> (for/sum ([entry (in-list entries*)]) (entry-bytes entry)) max-bytes)))
      (let loop ([remaining entries])
        (when (over-budget? remaining)
          (define candidates
            (filter (lambda (entry)
                      (not (equal? (car entry) protected-name)))
                    remaining))
          (when (pair? candidates)
            (define oldest
              (car
               (sort candidates
                     (lambda (left right)
                       (define left-mtime (car (cdr left)))
                       (define right-mtime (car (cdr right)))
                       (or (< left-mtime right-mtime)
                           (and (= left-mtime right-mtime)
                                (string<? (car left) (car right))))))))
            (define entry-name (car oldest))
            (define expected-mtime (car (cdr oldest)))
            (define manifest-path (caddr (cdr oldest)))
            (define cache-dir (cadddr (cdr oldest)))
            (call-with-provider-cache-lock/try
             manifest-path
             (lambda ()
               ;; Revalidate while holding the writer lock. A concurrent build
               ;; can only turn this candidate into a newer complete entry.
               (define current
                 (compilation-output-cache-entry-info cache-root entry-name))
               (and current
                    (= expected-mtime (car current))
                    (begin
                      (delete-directory/files cache-dir)
                      (delete-file manifest-path)
                      #t))))
            ;; Whether this entry was deleted, locked, or changed, do not retry
            ;; it in this sweep; try the next oldest safe entry instead.
            (loop (remove oldest remaining))))))))

;; copy-cache-output-file! : path-string? path-string? -> void?
;;   Materialize one cached artifact, creating its destination directory.
(define (copy-cache-output-file! source destination)
  (make-directory* (or (path-only destination) (current-directory)))
  (copy-file/replace! source destination))

;; materialize-compilation-output-cache!/locked! : any/c (listof pair?) [#:sidecar-count exact-nonnegative-integer?] -> (or/c 'hit 'miss)
;;   Restore a complete compiler-output bundle when its manifest is valid.
(define (materialize-compilation-output-cache!/locked! key
                                                        direct-output-files
                                                        #:sidecar-count [expected-sidecar-count #f])
  (define manifest-path (compilation-output-cache-manifest-path key))
  (define cache-dir (compilation-output-cache-directory key))
  (define payload (read-provider-cache manifest-path key))
  (define entries (and payload (decode-compilation-output-cache-manifest payload)))
  (define wasm-output-path (cdr (assq 'program-wasm direct-output-files)))
  (define wasm-output-dir
    (or (path-only wasm-output-path) (current-directory)))
  (cond
    [(and entries
          (direct-output-cache-entries-valid? entries direct-output-files)
          (or (not expected-sidecar-count)
              (= expected-sidecar-count
                 (length (filter (lambda (entry)
                                   (eq? (car entry) 'sidecar))
                                 entries))))
          (for/and ([entry (in-list entries)])
            (file-exists? (cache-output-entry-path cache-dir entry))))
     (for ([entry (in-list direct-output-files)])
       (define cached (assq (car entry) entries))
       (copy-cache-output-file! (cache-output-entry-path cache-dir cached)
                                (cdr entry)))
     (for ([entry (in-list entries)]
           #:when (eq? (car entry) 'sidecar))
       (copy-cache-output-file!
        (cache-output-entry-path cache-dir entry)
        (build-path wasm-output-dir (cadr entry))))
     (with-handlers ([exn:fail? void])
       ;; The manifest mtime is the cheap LRU signal used by post-write pruning.
       (file-or-directory-modify-seconds manifest-path (current-seconds)))
     'hit]
    [else 'miss]))

;; materialize-compilation-output-cache! : any/c (listof pair?) [#:sidecar-count exact-nonnegative-integer?] -> (or/c 'hit 'miss)
;;   Restore a complete compiler-output bundle while preventing concurrent pruning.
(define (materialize-compilation-output-cache! key
                                                direct-output-files
                                                #:sidecar-count [expected-sidecar-count #f])
  (call-with-provider-cache-lock
   (compilation-output-cache-manifest-path key)
   (lambda ()
     (materialize-compilation-output-cache!/locked!
      key
      direct-output-files
      #:sidecar-count expected-sidecar-count))
   #:mode 'shared))

;; cache-compilation-output! : any/c (listof pair?) (listof path-string?)
;;                              [#:max-bytes exact-nonnegative-integer?]
;;                              [#:max-entries exact-nonnegative-integer?] -> (or/c 'write 'hit)
;;   Save one complete compiler-output bundle after a successful compilation.
(define (cache-compilation-output! key
                                   direct-output-files
                                   sidecar-output-paths
                                   #:max-bytes [max-bytes default-compilation-output-cache-max-bytes]
                                   #:max-entries [max-entries default-compilation-output-cache-max-entries])
  (define manifest-path (compilation-output-cache-manifest-path key))
  (define cache-dir (compilation-output-cache-directory key))
  (define manifest
    (compilation-output-cache-manifest direct-output-files sidecar-output-paths))
  (define entries (decode-compilation-output-cache-manifest manifest))
  (unless entries
    (error 'cache-compilation-output! "invalid compiler-output cache manifest"))
  (define status
    (call-with-provider-cache-lock
     manifest-path
     (lambda ()
       (cond
         [(eq? (materialize-compilation-output-cache!/locked!
                key
                direct-output-files
                #:sidecar-count (length sidecar-output-paths))
               'hit)
          'hit]
         [else
          (when (directory-exists? cache-dir)
            (delete-directory/files cache-dir))
          (make-directory* cache-dir)
          (for ([entry (in-list direct-output-files)])
            (copy-file/replace! (cdr entry)
                                (build-path cache-dir
                                            (cache-output-file-name (cdr entry)))))
          (for ([path (in-list sidecar-output-paths)])
            (copy-file/replace! path
                                (build-path cache-dir (cache-output-file-name path))))
          (write-provider-cache! manifest-path key manifest)
          'write]))))
  (when (eq? status 'write)
    (prune-compilation-output-cache!
     #:max-bytes max-bytes
     #:max-entries max-entries
     #:protect-key key))
  status)

(module+ test
  (require rackunit)

  (define compilation-output-cache-test-root
    (make-temporary-file "webracket-output-cache-test~a" 'directory))
  (define compilation-output-cache-test-output-dir
    (build-path compilation-output-cache-test-root "output"))
  (define compilation-output-cache-test-provider-dir
    (build-path compilation-output-cache-test-root "provider-cache"))
  (define (write-output-cache-test-file path text)
    (make-directory* (or (path-only path) (current-directory)))
    (call-with-output-file path
      (lambda (out) (display text out))
      #:exists 'truncate/replace))
  (define compilation-output-cache-test-direct-files
    (list (cons 'program-wat
                (build-path compilation-output-cache-test-output-dir "program.wat"))
          (cons 'program-wasm
                (build-path compilation-output-cache-test-output-dir "program.wasm"))
          (cons 'program-map
                (build-path compilation-output-cache-test-output-dir "program.wasm.map.sexp"))
          (cons 'host
                (build-path compilation-output-cache-test-output-dir "program.html"))))
  (define compilation-output-cache-test-sidecar
    (build-path compilation-output-cache-test-output-dir
                "webracket-runtime-test.wasm"))
  (for ([entry (in-list compilation-output-cache-test-direct-files)])
    (write-output-cache-test-file (cdr entry) (symbol->string (car entry))))
  (write-output-cache-test-file compilation-output-cache-test-sidecar "sidecar")
  (parameterize ([current-provider-cache-dir compilation-output-cache-test-provider-dir]
                 [current-provider-cache-force? #f])
    (define key '(compilation-output-cache-test 1))
    (check-eq? (cache-compilation-output!
                key
                compilation-output-cache-test-direct-files
                (list compilation-output-cache-test-sidecar))
               'write)
    (delete-directory/files compilation-output-cache-test-output-dir)
    (check-eq? (materialize-compilation-output-cache!
                key
                compilation-output-cache-test-direct-files
                #:sidecar-count 1)
               'hit)
    (for ([entry (in-list compilation-output-cache-test-direct-files)])
      (check-equal? (file->bytes (cdr entry))
                    (string->bytes/utf-8 (symbol->string (car entry)))))
    (check-equal? (file->bytes compilation-output-cache-test-sidecar)
                  #"sidecar"))
  (delete-directory/files compilation-output-cache-test-root))

(module+ test
  (define retention-test-root
    (make-temporary-file "webracket-output-cache-retention-test~a" 'directory))
  (define retention-test-output-dir (build-path retention-test-root "output"))
  (define retention-test-provider-dir (build-path retention-test-root "provider-cache"))
  (define retention-test-direct-files
    (list (cons 'program-wat (build-path retention-test-output-dir "program.wat"))
          (cons 'program-wasm (build-path retention-test-output-dir "program.wasm"))
          (cons 'program-map (build-path retention-test-output-dir "program.wasm.map.sexp"))
          (cons 'host (build-path retention-test-output-dir "program.html"))))
  (define (write-retention-test-output! text)
    (for ([entry (in-list retention-test-direct-files)])
      (write-output-cache-test-file (cdr entry) text)))
  (define (cache-retention-test-output! key text)
    (write-retention-test-output! text)
    (cache-compilation-output!
     key
     retention-test-direct-files
     '()
     #:max-bytes 1000000
     #:max-entries 2))
  (parameterize ([current-provider-cache-dir retention-test-provider-dir]
                 [current-provider-cache-force? #f])
    (define oldest-key '(compilation-output-cache-retention oldest))
    (define middle-key '(compilation-output-cache-retention middle))
    (define newest-key '(compilation-output-cache-retention newest))
    (define incomplete-key '(compilation-output-cache-retention incomplete))
    (define oldest-manifest (compilation-output-cache-manifest-path oldest-key))
    (define middle-manifest (compilation-output-cache-manifest-path middle-key))
    (define newest-manifest (compilation-output-cache-manifest-path newest-key))
    (check-eq? (cache-retention-test-output! oldest-key "oldest") 'write)
    (check-eq? (cache-retention-test-output! middle-key "middle") 'write)
    ;; Entries below the retention limit remain available.
    (check-true (file-exists? oldest-manifest))
    (check-true (file-exists? middle-manifest))
    (check-eq? (cache-retention-test-output! newest-key "newest") 'write)
    ;; The oldest complete entry is evicted, while the current write survives.
    (check-false (file-exists? oldest-manifest))
    (check-true (file-exists? middle-manifest))
    (check-true (file-exists? newest-manifest))
    (define incomplete-manifest (compilation-output-cache-manifest-path incomplete-key))
    (make-directory* (compilation-output-cache-directory incomplete-key))
    (write-provider-cache!
     incomplete-manifest
     incomplete-key
     (compilation-output-cache-manifest retention-test-direct-files '()))
    (define unrelated-path (provider-cache-path "unrelated-provider-data"))
    (write-output-cache-test-file unrelated-path "leave me alone")
    ;; Incomplete and unrelated data are ignored, not repaired or deleted.
    (prune-compilation-output-cache! #:max-bytes 1000000 #:max-entries 1
                                     #:protect-key newest-key)
    (check-true (directory-exists? (compilation-output-cache-directory incomplete-key)))
    (check-true (file-exists? incomplete-manifest))
    (check-true (file-exists? unrelated-path))
    ;; An evicted entry is a normal cache miss and can be rebuilt correctly.
    (check-eq? (cache-retention-test-output! oldest-key "rebuilt") 'write)
    (delete-directory/files retention-test-output-dir)
    (check-eq? (materialize-compilation-output-cache! oldest-key retention-test-direct-files)
               'hit)
    (for ([entry (in-list retention-test-direct-files)])
      (check-equal? (file->bytes (cdr entry)) #"rebuilt"))
    ;; The byte budget is independent of the entry-count limit.
    (define byte-old-key '(compilation-output-cache-retention byte-old))
    (define byte-new-key '(compilation-output-cache-retention byte-new))
    (define byte-old-manifest (compilation-output-cache-manifest-path byte-old-key))
    (define byte-new-manifest (compilation-output-cache-manifest-path byte-new-key))
    (write-retention-test-output! "12345678")
    (check-eq? (cache-compilation-output! byte-old-key retention-test-direct-files '()
                                          #:max-bytes 1000000 #:max-entries 100)
               'write)
    (file-or-directory-modify-seconds byte-old-manifest 1)
    (write-retention-test-output! "abcdefgh")
    (check-eq? (cache-compilation-output! byte-new-key retention-test-direct-files '()
                                          #:max-bytes 32 #:max-entries 100)
               'write)
    (check-false (file-exists? byte-old-manifest))
    (check-true (file-exists? byte-new-manifest)))
  (delete-directory/files retention-test-root))

;; assemble-split-runtime-core/cached! :
;;   (-> list?) path-string? path-string? boolean?
;;   (list? path-string? path-string? -> boolean?)
;;   [#:cache-key any/c]
;;   [#:materialize-cache-hit? boolean?]
;;   [#:assemble-cache-miss? boolean?]
;;   [#:record-timing! (string? real? -> any/c)]
;;   -> (values boolean? (or/c path-string? #f))
;;   Assemble or locate the core runtime, reusing a fingerprinted Wasm cache hit.
(define (assemble-split-runtime-core/cached! core-module-thunk
                                             runtime-wat
                                             runtime-wasm
                                             pretty-wat?
                                             assemble-module!
                                             #:cache-key [cache-key #f]
                                             #:materialize-cache-hit?
                                             [materialize-cache-hit? #t]
                                             #:assemble-cache-miss?
                                             [assemble-cache-miss? #t]
                                             #:record-timing!
                                             [record-timing! void])
  (define (time-core-cache-step label thunk)
    (define start-ms (now-ms))
    (begin0
      (thunk)
      (record-timing! label (- (now-ms) start-ms))))
  (define core-module #f)
  (define (force-core-module!)
    (or core-module
        (let ([module (time-core-cache-step
                       "split-runtime-core:force-module"
                       core-module-thunk)])
          (set! core-module module)
          module)))
  (define fingerprint
    (if cache-key
        (time-core-cache-step
         "split-runtime-core:stable-cache-key"
         (lambda ()
           (split-runtime-core-cache-key-fingerprint cache-key)))
        (time-core-cache-step
         "split-runtime-core:fingerprint"
         (lambda ()
           (split-runtime-core-fingerprint (force-core-module!))))))
  (define cache-wasm-path (split-runtime-core-cache-wasm-path fingerprint))
  (define cache-wat-path (split-runtime-core-cache-wat-path fingerprint
                                                            pretty-wat?))
  (define (cache-hit? path)
    (and (not (current-provider-cache-force?))
         (file-exists? path)))
  (define (cache-runtime-wat!)
    (make-directory* (path-only cache-wat-path))
    (time-core-cache-step
     "split-runtime-core:cache-wat-copy"
     (lambda ()
       (copy-file/replace! runtime-wat cache-wat-path))))
  (define (write-runtime-wat!)
    (time-core-cache-step
     "split-runtime-core:write-wat"
     (lambda ()
       (write-wat-to-file runtime-wat
                          (force-core-module!)
                          #:pretty? pretty-wat?))))
  (define (materialize-runtime-wat!)
    (cond
      [(cache-hit? cache-wat-path)
       (time-core-cache-step
        "split-runtime-core:materialize-wat-hit"
        (lambda ()
          (copy-file/replace! cache-wat-path runtime-wat)))]
      [else
       (provider-cache-log "split runtime core wat miss; writing: ~a"
                           cache-wat-path)
       (write-runtime-wat!)
       (cache-runtime-wat!)]))
  (define (copy-cached-runtime!)
    (time-core-cache-step
     "split-runtime-core:materialize-wasm-hit"
     (lambda ()
       (copy-file/replace! cache-wasm-path runtime-wasm)))
    (materialize-runtime-wat!)
    (values #t runtime-wasm))
  (define (reference-cached-runtime!)
    (time-core-cache-step
     "split-runtime-core:reference-wasm-hit"
     (lambda () (void)))
    (values #t cache-wasm-path))
  (define (cached-runtime-hit!)
    (if materialize-cache-hit?
        (copy-cached-runtime!)
        (reference-cached-runtime!)))
  (cond
    [(cache-hit? cache-wasm-path)
     (provider-cache-log "split runtime core wasm hit: ~a" cache-wasm-path)
     (cached-runtime-hit!)]
    [else
     (call-with-provider-cache-lock
      cache-wasm-path
      (lambda ()
        (cond
          [(cache-hit? cache-wasm-path)
           (provider-cache-log "split runtime core wasm hit: ~a"
                               cache-wasm-path)
           (cached-runtime-hit!)]
          [(not assemble-cache-miss?)
           (time-core-cache-step
            "split-runtime-core:cache-miss-not-assembled"
            (lambda ()
              (error 'assemble-split-runtime-core/cached!
                     (string-append
                      "split-runtime core wasm cache miss for reference-only output; "
                      "prewarm the runtime cache first"))))]
          [else
           (provider-cache-log "split runtime core wasm miss; assembling: ~a"
                               cache-wasm-path)
           (write-runtime-wat!)
           (define assembled?
             (time-core-cache-step
              "split-runtime-core:assemble-wasm"
              (lambda ()
                (assemble-module! (force-core-module!)
                                  runtime-wat
                                  runtime-wasm))))
           (when assembled?
             (make-directory* (path-only cache-wasm-path))
             (time-core-cache-step
              "split-runtime-core:cache-wasm-copy"
              (lambda ()
                (copy-file/replace! runtime-wasm cache-wasm-path)))
             (cache-runtime-wat!))
           (values assembled?
                   (and assembled?
                        (if materialize-cache-hit?
                            runtime-wasm
                            cache-wasm-path)))])))]))

;; assemble-split-runtime-ffi/cached! :
;;   list? path-string? path-string? boolean?
;;   (list? path-string? path-string? -> boolean?)
;;   [#:cache-key any/c]
;;   [#:record-timing! (string? real? -> any/c)]
;;   -> boolean?
;;   Assemble or materialize the split FFI sidecar using the runtime cache.
(define (assemble-split-runtime-ffi/cached! ffi-module
                                            ffi-wat
                                            ffi-wasm
                                            pretty-wat?
                                            assemble-module!
                                            #:cache-key [cache-key #f]
                                            #:record-timing!
                                            [record-timing! void])
  (define (time-ffi-cache-step label thunk)
    (define start-ms (now-ms))
    (begin0
      (thunk)
      (record-timing! label (- (now-ms) start-ms))))
  (define fingerprint
    (if cache-key
        (time-ffi-cache-step
         "split-runtime-ffi:stable-cache-key"
         (lambda ()
           (split-runtime-core-cache-key-fingerprint cache-key)))
        (time-ffi-cache-step
         "split-runtime-ffi:fingerprint"
         (lambda ()
           (split-runtime-core-fingerprint ffi-module)))))
  (define cache-wasm-path (split-runtime-ffi-cache-wasm-path fingerprint))
  (define cache-wat-path (split-runtime-ffi-cache-wat-path fingerprint
                                                           pretty-wat?))
  (define (cache-hit? path)
    (and (not (current-provider-cache-force?))
         (file-exists? path)))
  (define (cache-ffi-wat!)
    (make-directory* (path-only cache-wat-path))
    (time-ffi-cache-step
     "split-runtime-ffi:cache-wat-copy"
     (lambda ()
       (copy-file/replace! ffi-wat cache-wat-path))))
  (define (write-ffi-wat!)
    (time-ffi-cache-step
     "split-runtime-ffi:write-wat"
     (lambda ()
       (write-wat-to-file ffi-wat ffi-module #:pretty? pretty-wat?))))
  (define (materialize-ffi-wat!)
    (cond
      [(cache-hit? cache-wat-path)
       (time-ffi-cache-step
        "split-runtime-ffi:materialize-wat-hit"
        (lambda ()
          (copy-file/replace! cache-wat-path ffi-wat)))]
      [else
       (provider-cache-log "split runtime ffi wat miss; writing: ~a"
                           cache-wat-path)
       (write-ffi-wat!)
       (cache-ffi-wat!)]))
  (define (cached-ffi-hit!)
    (time-ffi-cache-step
     "split-runtime-ffi:materialize-wasm-hit"
     (lambda ()
       (copy-file/replace! cache-wasm-path ffi-wasm)))
    (materialize-ffi-wat!)
    #t)
  (cond
    [(cache-hit? cache-wasm-path)
     (provider-cache-log "split runtime ffi wasm hit: ~a" cache-wasm-path)
     (cached-ffi-hit!)]
    [else
     (call-with-provider-cache-lock
      cache-wasm-path
      (lambda ()
        (cond
          [(cache-hit? cache-wasm-path)
           (provider-cache-log "split runtime ffi wasm hit: ~a"
                               cache-wasm-path)
           (cached-ffi-hit!)]
          [else
           (provider-cache-log "split runtime ffi wasm miss; assembling: ~a"
                               cache-wasm-path)
           (write-ffi-wat!)
           (define assembled?
             (time-ffi-cache-step
              "split-runtime-ffi:assemble-wasm"
              (lambda ()
                (assemble-module! ffi-module ffi-wat ffi-wasm))))
           (when assembled?
             (make-directory* (path-only cache-wasm-path))
             (time-ffi-cache-step
              "split-runtime-ffi:cache-wasm-copy"
              (lambda ()
                (copy-file/replace! ffi-wasm cache-wasm-path)))
             (cache-ffi-wat!))
           assembled?])))]))

;; host-wasm-reference : boolean? path-string? path-string? -> path-string?
;;   Return the Wasm path as the generated host should reference it.
(define (host-wasm-reference browser? out-host out-wasm)
  (if browser?
      (path->string
       (find-relative-path
        (path->complete-path (or (path-only out-host) (current-directory))
                             (current-directory))
        (path->complete-path out-wasm (current-directory))))
      out-wasm))


(define (drive-compilation
         ;; Paths
         #:filename          filename
         #:wat-filename      wat-filename
         #:wasm-filename     wasm-filename
         #:host-filename     host-filename      ; default: "runtime.js"
         #:dest-dir          dest-dir
         ;; Debug Information
         #:label-map-forms?  label-map-forms?    
         #:dump-passes-dir   dump-passes-dir     
         #:dump-passes-limit dump-passes-limit  ; max number of passes to dump
         #:timings?          timings?
         #:alpha-rename-profile? alpha-rename-profile?
         #:simplify-profile? simplify-profile?
         #:pretty-wat?       pretty-wat?        ; pretty print (slow) the wat-file
         #:verbose?          verbose?
         ;; Targets
         #:browser?          browser?           
         #:node?             node?
         ;; Compiler strategy
         #:letrec-strategy   letrec-strategy
         ;; Optimization
         #:enable-cc-analysis? enable-cc-analysis?
         #:enable-simplify?  enable-simplify?
         #:frontend-cache?   [frontend-cache? #f]
         #:module-bridge-elide-public-cells? [module-bridge-elide-public-cells? #f]
         #:tree-shake?       tree-shake?        ; remove dead code from the wat-file
         #:tree-shake-report tree-shake-report
         #:split-runtime?    [split-runtime? #t]
         #:split-runtime-cache-reference? [split-runtime-cache-reference? #f]
         #:split-runtime-core-url [split-runtime-core-url #f]
         #:split-runtime-core-only? [split-runtime-core-only? #f]
         #:split-runtime-provider-runtime? [split-runtime-provider-runtime? #f]
         #:split-runtime-ffi-module? [split-runtime-ffi-module? #f]
         #:split-runtime-ffi-module-explicit? [split-runtime-ffi-module-explicit? #f]
         #:strip-wasm-names? [strip-wasm-names? #f]
         ;; Program Debug
         #:print-top-level-results? print-top-level-results?
         #:internal-bind-treelist-debug-shape? internal-bind-treelist-debug-shape?
         #:internal-bind-immutable-hash-debug-shape? internal-bind-immutable-hash-debug-shape?
         ;; Reflection
         #:console-bridge?   console-bridge?    ; adds WR on the JS side
         ;; Populate the Virtual File System
         #:vfs-preloads      vfs-preloads       
         #:vfs-mounts        vfs-mounts
         ;; Convenience
         #:run-after?        run-after?
         #:exec-path-from-shell? [exec-path-from-shell? #t]
         ;; Foreign Function Interface
         #:ffi-files         ffi-files    ; list of file paths for .ffi files
         ;; Standard Library
         #:stdlib?           stdlib?      ; can be turned off for testing the compiler
         #:stdlib-io-format-provider? [stdlib-io-format-provider? #t]
         #:expander-read-provider? [expander-read-provider? #f]
         #:expander-read-syntax-provider? [expander-read-syntax-provider? #f]
         #:webracket-module-bridge? [webracket-module-bridge? #f]
         #:provider-substitutions? [provider-substitutions? #t]
         ;; Provider Cache
         #:provider-cache-dir [provider-cache-dir default-provider-cache-project-root]
         #:runtime-provider-cache-dir [runtime-provider-cache-dir default-runtime-provider-cache-root]
         #:compiled-output-cache-max-bytes
         [compiled-output-cache-max-bytes default-compilation-output-cache-max-bytes]
         #:compiled-output-cache-max-entries
         [compiled-output-cache-max-entries default-compilation-output-cache-max-entries]
         #:clear-provider-cache? [clear-provider-cache? #f]
         #:clear-runtime-provider-cache? [clear-runtime-provider-cache? #f]
         #:force-provider-cache-refresh? [force-provider-cache-refresh? #f])

  (let ([result
         (let/ec return
  (define exit-code       0)
  (define compile-timings #f)
  (define split-runtime-core-timing-rows '())
  (define split-runtime-ffi-timing-rows '())
  (define t-driver-start (now-ms))
  (current-source-module-language #f)
  (current-source-module-provider-names #f)
  (current-module-bridge-elidable-module-names #f)
  (current-module-bridge-local-source-paths #f)
  (current-module-bridge-module-kinds #f)
  (current-module-bridge-needs-regexp-provider? #t)
  (current-console-bridge-root-export-bindings #f)
  (current-console-bridge-root-source-path #f)
  (current-webify-knowns-mode 'normal)
  (current-module-bridge-detail-rows '())
  (current-webify-bridge-stats (make-hash))

  (current-browser? browser?)
  (current-enable-cc-analysis? enable-cc-analysis?)
  (current-console-bridge? console-bridge?)
  (current-tree-shake? tree-shake?)
  (current-frontend-cache-enabled? frontend-cache?)
  (current-module-bridge-elide-public-cells? module-bridge-elide-public-cells?)
  (define (cache-dir->complete-path cache-dir)
    (let ([p (if (path? cache-dir)
                 cache-dir
                 (string->path cache-dir))])
      (path->complete-path p)))
  (define provider-cache-root-path
    (cache-dir->complete-path provider-cache-dir))
  (define runtime-provider-cache-root-path
    (cache-dir->complete-path runtime-provider-cache-dir))
  (current-runtime-cache-dir runtime-provider-cache-root-path)
  (current-runtime-primitive-report-path tree-shake-report)
  (set-browser-mode! browser?)
  (current-provider-cache-dir provider-cache-root-path)
  (current-provider-cache-force? force-provider-cache-refresh?)
  (current-provider-cache-verbose? verbose?)
  (current-webracket/base-provider-substitutions? provider-substitutions?)
  (unless (exact-nonnegative-integer? compiled-output-cache-max-bytes)
    (raise-argument-error 'drive-compilation
                          "exact-nonnegative-integer?"
                          compiled-output-cache-max-bytes))
  (unless (exact-nonnegative-integer? compiled-output-cache-max-entries)
    (raise-argument-error 'drive-compilation
                          "exact-nonnegative-integer?"
                          compiled-output-cache-max-entries))
  (when clear-provider-cache?
    (provider-cache-log "clearing provider cache directory: ~a" provider-cache-root-path)
    (with-handlers ([exn:fail:filesystem? void])
      (delete-directory/files provider-cache-root-path)))
  (when clear-runtime-provider-cache?
    (provider-cache-log "clearing runtime provider cache directory: ~a" runtime-provider-cache-root-path)
    (with-handlers ([exn:fail:filesystem? void])
      (delete-directory/files runtime-provider-cache-root-path)))
  (when verbose?
    (provider-cache-log "provider cache directory: ~a" provider-cache-root-path)
    (provider-cache-log "runtime provider cache directory: ~a" runtime-provider-cache-root-path)
    (when force-provider-cache-refresh?
      (provider-cache-log "force refresh enabled")))

  ; 0. Check that `filename` exists and is a source file.
  (ensure-source-file! 'drive-compilation filename)
  (define source-file-module-input?/pre-read
    (with-handlers ([exn:fail? (lambda (_e) #f)])
      (call-with-input-file filename
        (lambda (in)
          (webracket-module-lang-line? (read-line in 'any))))))
  (define provider-owned-primitive-aliases/pre-read
    (provider-owned-primitive-alias-symbols
     #:expander-read-provider? (or expander-read-provider?
                                   source-file-module-input?/pre-read)
     #:io-format-provider? #f
     #:io-print-provider? source-file-module-input?/pre-read))
  (current-module-bridge-needs-expander-read-provider? #f)

  ; 1. Read the top-level forms from `filename`.
  (define t-read-source-start (now-ms))
  (define stx
    (with-handlers ([exn:fail? (lambda (e)
                                 (error 'drive-compilation
                                        "~a"
                                        (~a "read failed: " (exn-message e))))])
      (parameterize ([current-provider-owned-primitive-aliases
                      provider-owned-primitive-aliases/pre-read])
        (read-top-level-from-file
         filename
         #:webracket-module-bridge? webracket-module-bridge?))))
  (define t-read-source-end (now-ms))
  (define stx-for-compile
    (if print-top-level-results?
        (add-print-top-level-results-sentinel stx)
        stx))
  (define module-input? (and (current-source-module-language) #t))
  (define effective-frontend-cache?
    (and frontend-cache?
         ;; Module-bridge input contains generated binding identities. The
         ;; current frontend syntax cache replays those identities from disk,
         ;; which is not safe for module compilations yet.
         (not module-input?)))
  (define effective-legacy-stdlib?
    (and (not module-input?) stdlib?))
  (define effective-io-format-provider?
    (and (not module-input?) stdlib-io-format-provider?))
  (define source-needs-expander-read-provider?
    (and module-input?
         (current-module-bridge-needs-expander-read-provider?)))
  (define effective-expander-read-provider?
    (or expander-read-provider?
        source-needs-expander-read-provider?))
  (define effective-expander-read-syntax-provider?
    expander-read-syntax-provider?)
  (define source-needs-regexp-provider?
    (if module-input?
        (current-module-bridge-needs-regexp-provider?)
        (module-bridge-datum-needs-regexp-provider? (syntax->datum stx-for-compile))))
  (define source-needs-io-print-provider?
    (and module-input?
         (let ([datum (syntax->datum stx-for-compile)])
           (for/or ([name (in-list io-print-provider-owned-alias-symbols)])
             (module-bridge-datum-contains-unquoted-symbol? datum name)))))
  (define effective-regexp-provider?
    (or source-needs-regexp-provider?
        effective-expander-read-provider?
        effective-expander-read-syntax-provider?))
  (define source-needs-formatting-numbers?
    (and (not module-input?)
         (module-bridge-datum-needs-formatting-numbers?
          (syntax->datum stx-for-compile))))
  (define effective-formatting-numbers-slice?
    (and source-needs-formatting-numbers?
         (not effective-legacy-stdlib?)))
  (current-module-bridge-needs-regexp-provider? source-needs-regexp-provider?)
  (define stdlib-decision-rows
    (list
     (stdlib-decision-note "input-mode"
                           (if module-input? "module" "script"))
     (stdlib-decision-note "frontend-cache-mode"
                           (cond
                             [effective-frontend-cache?
                              "enabled"]
                             [(and frontend-cache? module-input?)
                              "disabled-module-input"]
                             [else
                              "disabled"]))
     (stdlib-decision-note "legacy-stdlib-mode"
                           (cond
                             [module-input?
                              (if stdlib?
                                  "ignored-module-input"
                                  "skipped-module-input")]
                             [stdlib?
                              "enabled-script-input"]
                             [else
                              "disabled-script-input"]))
     (stdlib-decision-note "io-format-provider-mode"
                           (cond
                             [module-input?
                              (if stdlib-io-format-provider?
                                  "skipped-module-input"
                                  "disabled-module-input")]
                             [stdlib-io-format-provider?
                              "enabled-script-input"]
                             [else
                              "disabled-script-input"]))
     (stdlib-decision-note "expander-read-provider-mode"
                           (cond
                             [(and module-input?
                                   (or expander-read-provider?
                                       source-needs-expander-read-provider?))
                              "satisfied-module-bridge"]
                             [expander-read-provider?
                              "enabled-flag"]
                             [source-needs-expander-read-provider?
                              "enabled-module-demand"]
                             [else
                              "disabled"]))
     (stdlib-decision-note "expander-read-syntax-provider-mode"
                           (cond
                             [(and module-input?
                                   effective-expander-read-syntax-provider?)
                              "satisfied-module-bridge"]
                             [effective-expander-read-syntax-provider?
                              "enabled"]
                             [else
                              "disabled"]))
     (stdlib-decision-note "regexp-provider-top-level-mode"
                           (cond
                             [module-input?
                              (if (current-module-bridge-needs-regexp-provider?)
                                  "satisfied-module-bridge"
                                  "skipped-module-no-demand")]
                             [(current-module-bridge-needs-regexp-provider?)
                              "enabled-script-demand"]
                             [else
                              "skipped-script-no-demand"]))
     (stdlib-decision-note "regexp-provider-read-dependency-mode"
                           (cond
                             [(and module-input? effective-regexp-provider?)
                              "satisfied-module-bridge"]
                             [effective-expander-read-provider?
                              "enabled-via-expander-read"]
                             [else
                              "disabled"]))
     (stdlib-decision-note "formatting-numbers-slice-mode"
                           (cond
                             [module-input?
                              "skipped-module-input"]
                             [effective-legacy-stdlib?
                              "satisfied-legacy-stdlib"]
                             [effective-formatting-numbers-slice?
                              "enabled-script-demand"]
                             [else
                              "skipped-script-no-demand"]))
     (stdlib-decision-note "io-print-provider-mode"
                           (cond
                             [(and module-input?
                                   source-needs-io-print-provider?)
                              "satisfied-module-bridge"]
                             [module-input?
                              "skipped-module-no-demand"]
                             [else
                              "disabled-script-input"]))))

  ; 2. Handle ffi-files.
  (define t-ffi-setup-start  (now-ms))
  (define resolved-ffi-files
    (merged-ffi-files 'drive-compilation
                      browser?
                      ffi-files
                      stx
                      #:source-filename filename))

  (define ffi-foreigns  '()) ; list of `foreign` structures
  (define ffi-imports   '()) ; list of wat
  (define ffi-funcs     '()) ; list of wat

  (for ([ffi-filename resolved-ffi-files])
    (define fs (ffi-file->foreigns ffi-filename))
    (set! ffi-foreigns (cons fs ffi-foreigns)))
  (set! ffi-foreigns
        (foreigns-deduplicate
         'drive-compilation
         (append* (reverse ffi-foreigns))))
  (set! ffi-imports (map foreign->import ffi-foreigns))
  (set! ffi-funcs   (map foreign->primitive ffi-foreigns))

  (current-ffi-foreigns    ffi-foreigns)
  (current-ffi-imports-wat ffi-imports)
  (current-ffi-funcs-wat   ffi-funcs)
  (define t-ffi-setup-end (now-ms))

  (define provider-owned-primitive-aliases
    (provider-owned-primitive-alias-symbols
     #:expander-read-provider? effective-expander-read-provider?
     #:io-format-provider? effective-io-format-provider?
     #:io-print-provider? source-needs-io-print-provider?
     #:formatting-numbers? effective-formatting-numbers-slice?))
  (define split-runtime-ffi-module/effective?
    (if split-runtime-ffi-module-explicit?
        split-runtime-ffi-module?
        (and split-runtime?
             (not split-runtime-core-only?))))
  (define output-cache-expected-sidecar-count
    (+ (if (and split-runtime? (not split-runtime-core-url)) 2 0)
       (if split-runtime-ffi-module/effective? 2 0)))
  (define output-cache-output-paths
    (resolve-compilation-output-paths filename
                                      wat-filename
                                      wasm-filename
                                      host-filename
                                      dest-dir
                                      node?))
  (define output-cache-direct-output-files
    (list (cons 'program-wat  (compilation-output-paths-wat output-cache-output-paths))
          (cons 'program-wasm (compilation-output-paths-wasm output-cache-output-paths))
          (cons 'program-map  (compilation-output-paths-map output-cache-output-paths))
          (cons 'host         (compilation-output-paths-host output-cache-output-paths))))
  (define output-cache-local-source-paths
    (current-module-bridge-local-source-paths))
  (define output-cache-key
    (and module-input?
         output-cache-local-source-paths
         dest-dir
         (not wat-filename)
         (not wasm-filename)
         (not host-filename)
         (not dump-passes-dir)
         (not tree-shake-report)
         (not alpha-rename-profile?)
         (not simplify-profile?)
         (not timings?)
         (not run-after?)
         (not split-runtime-cache-reference?)
         (not force-provider-cache-refresh?)
         `(compilation-output-cache
           ,compilation-output-cache-version
           ,(version)
           ,(compilation-output-cache-compiler-fingerprint)
           (module-bridge-sources
            ,(source-path-fingerprint
              'compilation-output-cache-module-sources
              1
              output-cache-local-source-paths))
           (module-bridge-providers
            ,(module-bridge-result-provider-fingerprints
              (current-module-bridge-module-kinds)))
           (runtime ,(runtime-source-fingerprint))
           (ffi ,(path-fingerprint 'compilation-output-cache-ffi
                                    1
                                    resolved-ffi-files))
           (output-layout
            ,@(for/list ([entry (in-list output-cache-direct-output-files)])
                (list (car entry) (cache-output-file-name (cdr entry)))))
           (options
            (browser? ,browser?)
            (node? ,node?)
            (label-map-forms? ,label-map-forms?)
            (pretty-wat? ,pretty-wat?)
            (console-bridge? ,console-bridge?)
            (vfs-preloads ,vfs-preloads)
            (vfs-mounts ,vfs-mounts)
            (letrec-strategy ,letrec-strategy)
            (enable-cc-analysis? ,enable-cc-analysis?)
            (enable-simplify? ,enable-simplify?)
            (tree-shake? ,tree-shake?)
            (split-runtime? ,split-runtime?)
            (split-runtime-core-url ,split-runtime-core-url)
            (split-runtime-core-only? ,split-runtime-core-only?)
            (split-runtime-provider-runtime? ,split-runtime-provider-runtime?)
            (split-runtime-ffi-module? ,split-runtime-ffi-module/effective?)
            (strip-wasm-names? ,strip-wasm-names?)
            (print-top-level-results? ,print-top-level-results?)
            (internal-bind-treelist-debug-shape?
             ,internal-bind-treelist-debug-shape?)
            (internal-bind-immutable-hash-debug-shape?
             ,internal-bind-immutable-hash-debug-shape?)
            (module-bridge-elide-public-cells?
             ,module-bridge-elide-public-cells?)
            (provider-substitutions? ,provider-substitutions?)
            (provider-owned-primitive-aliases
             ,(sort (remove-duplicates provider-owned-primitive-aliases)
                    symbol<?))))))
  (when output-cache-key
    (when (eq? (materialize-compilation-output-cache!
                output-cache-key
                output-cache-direct-output-files
                #:sidecar-count output-cache-expected-sidecar-count)
               'hit)
      (provider-cache-log "compiled output cache hit: ~a"
                          (compilation-output-cache-manifest-path output-cache-key))
      (return (lambda () (values 0 #f)))))

  ; 3. Prepend the standard library (if enabled)
  ;     "legacy-stdlib/stdlib-for-browser.rkt" includes "stdlib.rkt" and adds `sxml->dom`
  (define t-add-stdlib-start (now-ms))
  (define stx-with-stdlib/base
    (cond
      [(current-source-module-language)
       (when (and verbose? stdlib?)
         (displayln
          "Ignoring legacy stdlib root for module-language input"
          (current-error-port)))
       (when (and verbose? stdlib-io-format-provider?)
         (displayln
          "Skipping staged io/format provider for module-language input; module providers resolve racket/format directly"
          (current-error-port)))
       (when (and verbose?
                  (or effective-regexp-provider?
                      effective-expander-read-provider?
                      effective-expander-read-syntax-provider?))
         (displayln
          "Using module-bridge providers for module-language input; skipping staged top-level provider composition"
          (current-error-port)))
       stx-for-compile]
      [(and effective-legacy-stdlib? browser?)
       (when verbose?
         (displayln "Composing module stdlib root `legacy-stdlib/stdlib-for-browser.rkt`"
                    (current-error-port)))
       (add-module-stdlib stx-for-compile
                           #t
                           #:io-format-provider? effective-io-format-provider?
                           #:expander-read-provider? effective-expander-read-provider?
                           #:expander-read-syntax-provider? effective-expander-read-syntax-provider?)]
      [effective-legacy-stdlib?
       (when verbose?
         (displayln "Composing module stdlib root `legacy-stdlib/stdlib.rkt`"
                    (current-error-port)))
       (add-module-stdlib stx-for-compile
                           #f
                           #:io-format-provider? effective-io-format-provider?
                           #:expander-read-provider? effective-expander-read-provider?
                           #:expander-read-syntax-provider? effective-expander-read-syntax-provider?)]
      [effective-io-format-provider?
       (when verbose?
         (displayln "Composing module stdlib-free `io/format` provider"
                    (current-error-port)))
       (add-io-format-provider stx-for-compile
                               #:regexp-provider? effective-regexp-provider?
                               #:expander-read-provider? effective-expander-read-provider?
                               #:expander-read-syntax-provider? effective-expander-read-syntax-provider?)]
      [effective-expander-read-provider?
       (when verbose?
         (displayln "Composing module stdlib-free `expander/read` provider"
                    (current-error-port)))
       (add-expander-read-provider stx-for-compile
                                   #:expander-read-syntax-provider?
                                   effective-expander-read-syntax-provider?)]
      [effective-expander-read-syntax-provider?
       (when verbose?
         (displayln "Composing module stdlib-free `expander/read-syntax` provider"
                    (current-error-port)))
       (with-syntax ([(provider-form ...)
                      (map (lambda (form) (datum->syntax stx-for-compile form stx-for-compile))
                           (append
                            (regexp-provider-forms #:public-aliases? #t
                                                   #:cache-context 'read-syntax)
                            (provider-slice-forms 'expander/read-syntax)))])
         #`(begin
             provider-form ...
             #,stx-for-compile))]
      [else
       (when verbose?
         (displayln "Composing module stdlib-free `regexp` provider"
                    (current-error-port)))
       (if (current-module-bridge-needs-regexp-provider?)
           (add-regexp-provider stx-for-compile)
           stx-for-compile)]))
  (define stx-with-stdlib
    (cond
      [effective-formatting-numbers-slice?
       (when verbose?
         (displayln "Composing module stdlib-free `formatting-numbers` slice"
                    (current-error-port)))
       (add-formatting-numbers-slice stx-with-stdlib/base)]
      [else
       stx-with-stdlib/base]))
  (define t-add-stdlib-end (now-ms))

  (define t-preflight-start (now-ms))

  ; 4. Preflight checks

  ; Preflight: `-r` requires Node host mode.
  (when (and run-after? browser?)
    (error 'drive-compilation
           "cannot use `-r` in browser mode; use Node mode or omit `-r`."))
  (when (and split-runtime-cache-reference? (not split-runtime?))
    (error 'drive-compilation
           "`--split-runtime-cache-reference` requires `--split-runtime`"))
  (when (and split-runtime-cache-reference? browser?)
    (error 'drive-compilation
           "`--split-runtime-cache-reference` is only supported for Node output"))
  (when (and split-runtime-core-url (not split-runtime?))
    (error 'drive-compilation
           "`--split-runtime-core-url` requires `--split-runtime`"))
  (when (and split-runtime-core-url (not browser?))
    (error 'drive-compilation
           "`--split-runtime-core-url` is only supported for browser output"))
  (when (and split-runtime-core-url split-runtime-cache-reference?)
    (error 'drive-compilation
           "`--split-runtime-core-url` cannot be combined with `--split-runtime-cache-reference`"))
  (when (and split-runtime-core-only? (not split-runtime?))
    (error 'drive-compilation
           "`--split-runtime-core-only` requires `--split-runtime`"))
  (when (and split-runtime-provider-runtime? (not split-runtime?))
    (error 'drive-compilation
           "`--split-runtime-provider-runtime` requires `--split-runtime`"))
  (when (and split-runtime-provider-runtime? split-runtime-core-only?)
    (error 'drive-compilation
           "`--split-runtime-provider-runtime` cannot be combined with `--split-runtime-core-only`"))
  (when (and split-runtime-provider-runtime? (not module-input?))
    (error 'drive-compilation
           "`--split-runtime-provider-runtime` requires #lang webracket/base or #lang webracket/full module input"))
  (when (and split-runtime-ffi-module/effective? (not split-runtime?))
    (error 'drive-compilation
           "`--split-runtime-ffi-module` requires `--split-runtime`"))
  (when (and split-runtime-ffi-module/effective? split-runtime-core-only?)
    (error 'drive-compilation
           "`--split-runtime-ffi-module` cannot be combined with `--split-runtime-core-only`"))
  ; Preflight: `-r` in Node mode requires a Node.js executable. Keep the
  ; resolved path for execution so a later shell fallback cannot displace it.
  (define node-path
    (and node?
         run-after?
         (check-node! 'drive-compilation
                      #:exec-path-from-shell? exec-path-from-shell?)))

  ; Precompute output paths for preflight checks.
  (define output-paths
    (resolve-compilation-output-paths filename
                                      wat-filename
                                      wasm-filename
                                      host-filename
                                      dest-dir
                                      node?))
  (define out-wat  (compilation-output-paths-wat output-paths))
  (define out-wasm (compilation-output-paths-wasm output-paths))
  (define out-map  (compilation-output-paths-map output-paths))
  (define out-host (compilation-output-paths-host output-paths))

  ; Preflight: ensure output files do not collide.
  (ensure-distinct-output-files!
   'drive-compilation
   (list (cons "program WAT output file"  out-wat)
         (cons "program Wasm output file" out-wasm)
         (cons "label-map output file"    out-map)
         (cons "host output file"         out-host)))

  ; Preflight: ensure output locations are writable.
  (ensure-output-path-writable! 'drive-compilation out-wat  "program WAT output file")
  (ensure-output-path-writable! 'drive-compilation out-wasm "program Wasm output file")
  (ensure-output-path-writable! 'drive-compilation out-map  "label-map output file")
  (ensure-output-path-writable! 'drive-compilation out-host "host output file")
  
  ; Preflight: ensure wasm-tools exists and can run. The shared locator
  ; preserves the inherited environment and imports the login shell only if
  ; wasm-tools is not already available.
  (check-wasm-tools! 'drive-compilation
                     #:exec-path-from-shell? exec-path-from-shell?)
  (define t-preflight-end (now-ms))

  ; 5. Compile the syntax object.
  (label-map-include-form? label-map-forms?)
  (current-pass-dump-dir   dump-passes-dir)
  (current-pass-dump-limit dump-passes-limit)
  (current-pass-timings?   timings?)
  (current-alpha-rename-profile? alpha-rename-profile?)
  (current-simplify-profile? simplify-profile?)
  (define split-runtime-provider-fragment-timing-rows '())
  (define split-runtime-provider-fragment-splice-info #f)
  (define split-runtime-provider-runtime-merge-info #f)
  (define t-compile-start (now-ms))
  (define split-runtime-provider-fragment-cache?
    (and split-runtime?
         module-input?
         (not split-runtime-core-only?)
         (or split-runtime-cache-reference?
             split-runtime-provider-runtime?)))
  (define (record-provider-fragment-timing! label start-ms)
    (set! split-runtime-provider-fragment-timing-rows
          (cons (list label (- (now-ms) start-ms))
                split-runtime-provider-fragment-timing-rows)))
  (define (compile-top-level stx-to-compile
                             #:imported-top-level-globals [imported-top-level-globals
                                                           (seteq)]
                             #:exported-top-level-globals [exported-top-level-globals
                                                           (seteq)]
                             #:extra-used-primitives [extra-used-primitives
                                                      '()]
                             #:split-runtime-ffi-in-core?
                             [split-runtime-ffi-in-core? #f]
                             #:split-runtime-ffi-module?
                             [split-runtime-ffi-module?
                              split-runtime-ffi-module/effective?]
                             #:split-runtime-ffi-program-owned?
                             [split-runtime-ffi-program-owned? #t]
                             #:allow-split-program-only-fast-path?
                             [allow-split-program-only-fast-path? #t])
    (parameterize ([current-browser? browser?]
                   [current-enable-cc-analysis? enable-cc-analysis?]
                   [current-letrec-strategy letrec-strategy]
                   [current-enable-simplify? enable-simplify?]
                   [current-frontend-cache-enabled? effective-frontend-cache?]
                   [current-module-bridge-elide-public-cells?
                    module-bridge-elide-public-cells?]
                   [current-print-top-level-results? print-top-level-results?]
                   [current-internal-treelist-debug-shape?
                    internal-bind-treelist-debug-shape?]
                   [current-internal-immutable-hash-debug-shape?
                    internal-bind-immutable-hash-debug-shape?]
                   [current-provider-owned-primitive-aliases
                    provider-owned-primitive-aliases]
                   [current-imported-top-level-globals
                    imported-top-level-globals]
                   [current-exported-top-level-globals
                    exported-top-level-globals]
                   [current-runtime-extra-used-primitives
                    extra-used-primitives]
                   [current-split-runtime?
                    (and split-runtime? (not split-runtime-core-only?))]
                   [current-split-runtime-reference-only?
                    (and split-runtime?
                         (not split-runtime-core-only?)
                         (or split-runtime-cache-reference?
                             split-runtime-core-url))]
                   [current-split-runtime-ffi-in-core?
                    split-runtime-ffi-in-core?]
                   [current-split-runtime-ffi-module?
                    split-runtime-ffi-module?]
                   [current-split-runtime-ffi-program-owned?
                    split-runtime-ffi-program-owned?]
                   [current-split-runtime-program-only-fast-path?
                    allow-split-program-only-fast-path?])
      (comp stx-to-compile)))
  (define (compile-with-provider-fragment-cache)
    (define fragment-manifest
      (hash-ref (current-webify-bridge-stats)
                'module-bridge-fragment-manifest
                #f))
    (unless fragment-manifest
      (raise-arguments-error
       'drive-compilation
       "module-bridge fragment manifest missing for split provider-fragment compile"
       "source" filename))
    (define fragment-split
      (module-bridge-split-lowered-datum (syntax->datum stx-with-stdlib)
                                         fragment-manifest))
    (define provider-datum
      (module-bridge-fragment-split-provider-datum fragment-split))
    (define program-datum
      (module-bridge-fragment-split-program-datum fragment-split))
    (define interface-symbols
      (module-bridge-provider-program-interface-symbols provider-datum
                                                        program-datum
                                                        provider-owned-primitive-aliases))
    (define provider-runtime-ffi-primitives
      (if split-runtime-provider-runtime?
          (module-bridge-provider-ffi-primitive-symbols provider-datum)
          '()))
    (define provider-fragment-extra-used-primitives
      (if (and split-runtime-provider-runtime?
               (memq 'unsafe-ffi-primitive provider-runtime-ffi-primitives))
          '(unsafe-ffi-primitive)
          provider-runtime-ffi-primitives))
    (define provider-runtime-ffi-in-core? #f)
    (define split-runtime-ffi-module/provider-effective?
      (and split-runtime-ffi-module/effective?
           (not provider-runtime-ffi-in-core?)))
    (define provider-fragment-split-runtime-ffi-module?
      (and split-runtime-ffi-module/provider-effective?
           (not split-runtime-provider-runtime?)))
    (define provider-fragment-fingerprint
      (module-bridge-provider-fragment-fingerprint fragment-split))
    (define compiled-cache-key
      (module-bridge-provider-compiled-fragment-cache-key
       provider-fragment-fingerprint
       #:browser? browser?
       #:enable-cc-analysis? enable-cc-analysis?
       #:enable-simplify? enable-simplify?
       #:split-runtime-reference? #t
       #:split-runtime-ffi-in-core? provider-runtime-ffi-in-core?
       #:split-runtime-ffi-module? provider-fragment-split-runtime-ffi-module?
       #:letrec-strategy letrec-strategy
       #:provider-owned-primitive-aliases provider-owned-primitive-aliases))
    (define provider-wat/cache #f)
    (define (compiled-provider-wat!)
      (or provider-wat/cache
          (let ()
            (define provider-cache-start (now-ms))
            (define-values (provider-cache-status _provider-wat-path)
              (ensure-module-bridge-provider-compiled-fragment-wat-cache!
               compiled-cache-key
               (lambda ()
                 (compiled-fragment->program-wat
                   (parameterize ([current-console-bridge? #f]
                                  [current-console-bridge-root-export-bindings #f]
                                  [current-console-bridge-root-source-path #f])
                     (compile-top-level
                      (datum->syntax #f provider-datum)
                      #:exported-top-level-globals interface-symbols
                      #:extra-used-primitives provider-fragment-extra-used-primitives
                      #:split-runtime-ffi-in-core?
                      provider-runtime-ffi-in-core?
                      #:split-runtime-ffi-module?
                      provider-fragment-split-runtime-ffi-module?))))))
            (record-provider-fragment-timing! "provider-fragment-cache"
                                              provider-cache-start)
            (hash-set! (current-webify-bridge-stats)
                       'module-bridge-compiled-provider-fragment-cache-status
                       provider-cache-status)
            (define provider-read-start (now-ms))
            (define provider-wat
              (read-module-bridge-provider-compiled-fragment-cache
               compiled-cache-key
               'drive-compilation))
            (record-provider-fragment-timing! "provider-fragment-read-wat"
                                              provider-read-start)
            (set! provider-wat/cache provider-wat)
            provider-wat)))
    (define (provider-runtime-core-cache-hit? cache-key)
      (and (not (current-provider-cache-force?))
           (file-exists?
            (split-runtime-core-cache-wasm-path
             (split-runtime-core-cache-key-fingerprint cache-key)))))
    (define program-compile-start (now-ms))
    (define program-compiled
      (compile-top-level
       (datum->syntax #f program-datum)
       #:imported-top-level-globals interface-symbols
       #:extra-used-primitives provider-runtime-ffi-primitives
       #:split-runtime-ffi-in-core?
       provider-runtime-ffi-in-core?
       #:split-runtime-ffi-module?
       split-runtime-ffi-module/provider-effective?
       #:split-runtime-ffi-program-owned?
       (not split-runtime-provider-runtime?)))
    (record-provider-fragment-timing! "program-fragment-compile"
                                      program-compile-start)
    (hash-set! (current-webify-bridge-stats)
               'module-bridge-provider-program-interface-symbol-count
               (length (set->list interface-symbols)))
    (cond
      [split-runtime-provider-runtime?
       (unless (split-runtime-modules? program-compiled)
         (raise-arguments-error
          'drive-compilation
          "split provider-runtime compilation did not produce split modules"
          "source" filename))
       (define initial-merged-core-cache-key
         (module-bridge-provider-runtime-core-cache-key
          (split-runtime-modules-core-cache-key program-compiled)
          compiled-cache-key))
       (define effective-program-compiled
         (if (provider-runtime-core-cache-hit? initial-merged-core-cache-key)
             program-compiled
             (let ()
               (define full-core-compile-start (now-ms))
               (define recompiled
                 (compile-top-level
                  (datum->syntax #f program-datum)
                  #:imported-top-level-globals interface-symbols
                  #:extra-used-primitives provider-runtime-ffi-primitives
                  #:split-runtime-ffi-in-core?
                  provider-runtime-ffi-in-core?
                  #:split-runtime-ffi-module?
                  split-runtime-ffi-module/provider-effective?
                  #:split-runtime-ffi-program-owned?
                  (not split-runtime-provider-runtime?)
                  #:allow-split-program-only-fast-path? #f))
               (record-provider-fragment-timing!
                "program-fragment-compile-materializable-core"
                full-core-compile-start)
               recompiled)))
       (unless (split-runtime-modules? effective-program-compiled)
         (raise-arguments-error
          'drive-compilation
          "split provider-runtime materializable compile did not produce split modules"
          "source" filename))
       (define merged-core-cache-key
         (module-bridge-provider-runtime-core-cache-key
          (split-runtime-modules-core-cache-key effective-program-compiled)
          compiled-cache-key))
       (define strip-start (now-ms))
       (define stripped-program-wat
         (strip-split-runtime-program-entry-init
          (compiled-fragment->program-wat effective-program-compiled)))
       (record-provider-fragment-timing! "program-fragment-strip-runtime-init"
                                         strip-start)
       (define merged-core-promise
         (delay
           (define merge-start (now-ms))
           (define-values (merged-runtime merge-info)
             (merge-provider-fragment-into-runtime
              (split-runtime-modules-core-module effective-program-compiled)
              (compiled-provider-wat!)
              #:who 'drive-compilation))
           (set! split-runtime-provider-runtime-merge-info merge-info)
           (record-provider-fragment-timing! "provider-runtime-merge"
                                             merge-start)
           merged-runtime))
       (hash-set! (current-webify-bridge-stats)
                  'module-bridge-provider-runtime-cache-key
                  merged-core-cache-key)
       (split-runtime-modules
        merged-core-promise
       stripped-program-wat
       (split-runtime-modules-imported-names effective-program-compiled)
       (split-runtime-modules-import-forms effective-program-compiled)
        merged-core-cache-key
        (split-runtime-modules-ffi-module effective-program-compiled)
        (split-runtime-modules-ffi-cache-key effective-program-compiled))]
      [else
       (define splice-start (now-ms))
       (define-values (spliced-program-wat splice-info)
         (splice-provider+program-modules
          (compiled-provider-wat!)
          (compiled-fragment->program-wat program-compiled)
          #:who 'drive-compilation))
       (set! split-runtime-provider-fragment-splice-info splice-info)
       (record-provider-fragment-timing! "provider-program-fragment-splice"
                                         splice-start)
       (if (split-runtime-modules? program-compiled)
           (split-runtime-modules-with-program program-compiled spliced-program-wat)
           spliced-program-wat)]))
  (define compiled-module
    (with-handlers (#;[exn:fail? (λ (e)
                                 (error 'drive-compilation
                                        (~a "compile failed: " (exn-message e))))])
      (if split-runtime-provider-fragment-cache?
          (compile-with-provider-fragment-cache)
          (compile-top-level stx-with-stdlib))))
  (define t-compile-end (now-ms))

  ; 6. Save the resulting WAT module.
  (define t-write-wat-start (now-ms))
  (define split-modules
    (and split-runtime?
         (if (and (not split-runtime-core-only?)
                  (split-runtime-modules? compiled-module))
             compiled-module
             (split-core-runtime-module
              compiled-module
              #:require-all? #f
              #:core-cache-key
              (make-split-runtime-core-only-cache-key
               (runtime-source-fingerprint))))))
  (define program-wat
    (if split-runtime?
        (split-runtime-modules-program split-modules)
        compiled-module))
  (define split-ffi-module
    (and split-runtime?
         (split-runtime-modules-ffi-module split-modules)))
  (define split-runtime-sidecar-path-needed?
    (and split-runtime?
         (not split-runtime-core-url)))
  (define materialize-split-runtime-sidecar?
    (and split-runtime-sidecar-path-needed?
         (not split-runtime-cache-reference?)))
  (define runtime-fingerprint
    (and split-runtime-sidecar-path-needed?
         (split-runtime-sidecar-fingerprint
          (split-runtime-modules-core-cache-key split-modules)
          (lambda ()
            (split-runtime-modules-core-module split-modules)))))
  (define runtime-wat
    (and split-runtime-sidecar-path-needed?
         (split-runtime-wat-path out-wat runtime-fingerprint pretty-wat?)))
  (define runtime-wasm
    (and split-runtime-sidecar-path-needed?
         (split-runtime-wasm-path out-wasm runtime-fingerprint)))
  (define ffi-fingerprint
    (and split-ffi-module
         (split-runtime-sidecar-fingerprint
          (split-runtime-modules-ffi-cache-key split-modules)
          (lambda () split-ffi-module))))
  (define ffi-wat
    (and split-ffi-module
         (split-runtime-ffi-wat-path out-wat ffi-fingerprint pretty-wat?)))
  (define ffi-wasm
    (and split-ffi-module
         (split-runtime-ffi-wasm-path out-wasm ffi-fingerprint)))
  (define sidecar-output-files
    (append
     (if split-runtime-sidecar-path-needed?
         (list (cons "split runtime WAT output file" runtime-wat)
               (cons "split runtime Wasm output file" runtime-wasm))
         '())
     (if split-ffi-module
         (list (cons "split FFI WAT output file" ffi-wat)
               (cons "split FFI Wasm output file" ffi-wasm))
         '())))
  (when (pair? sidecar-output-files)
    (ensure-distinct-output-files!
     'drive-compilation
     (append
      (list (cons "program WAT output file"  out-wat)
            (cons "program Wasm output file" out-wasm)
            (cons "label-map output file"    out-map)
            (cons "host output file"         out-host))
      sidecar-output-files))
    (for ([output-file (in-list sidecar-output-files)])
      (ensure-output-path-writable! 'drive-compilation
                                    (cdr output-file)
                                    (car output-file))))
  (when (and split-runtime? verbose?)
    (displayln
     (format "Split runtime imports: ~a runtime definitions"
             (length (split-runtime-modules-imported-names split-modules)))
     (current-error-port)))
  (write-wat-to-file out-wat program-wat #:pretty? pretty-wat?)
  (ensure-generated-file-exists! 'drive-compilation out-wat "program WAT output file")
  (define t-write-wat-end (now-ms))

  ; 7. Compile the wat-file to wasm using `wat->wasm` from `assembler.rkt`
  (define t-assemble-start (now-ms))
  (define (assemble-module! module wat-path wasm-path)
    (with-handlers ([exn:fail? (λ (e)
                                 (error 'drive-compilation
                                        (~a "wat->wasm failed: " (exn-message e))))])
      (wat->wasm module
                 #:wat wat-path
                 #:wasm wasm-path
                 #:write-wat? #f
                 #:exec-path-from-shell? exec-path-from-shell?)))
  (define-values (split-runtime-core-success?
                  split-runtime-core-reference-path)
    (if split-runtime?
        (assemble-split-runtime-core/cached!
         (lambda ()
           (split-runtime-modules-core-module split-modules))
         runtime-wat
         runtime-wasm
         pretty-wat?
         assemble-module!
         #:cache-key (split-runtime-modules-core-cache-key split-modules)
         #:materialize-cache-hit?
         (not (or split-runtime-cache-reference?
                  split-runtime-core-url))
         #:assemble-cache-miss?
         (not split-runtime-core-url)
         #:record-timing!
         (lambda (label elapsed-ms)
           (set! split-runtime-core-timing-rows
                 (cons (list label elapsed-ms)
                       split-runtime-core-timing-rows))))
        (values #t #f)))
  (define split-runtime-ffi-success?
    (if split-ffi-module
        (assemble-split-runtime-ffi/cached!
         split-ffi-module
         ffi-wat
         ffi-wasm
         pretty-wat?
         assemble-module!
         #:cache-key (split-runtime-modules-ffi-cache-key split-modules)
         #:record-timing!
         (lambda (label elapsed-ms)
           (set! split-runtime-ffi-timing-rows
                 (cons (list label elapsed-ms)
                       split-runtime-ffi-timing-rows))))
        #t))
  (define wat->wasm-success?
    (and split-runtime-core-success?
         split-runtime-ffi-success?
         (assemble-module! program-wat out-wat out-wasm)))
  (unless wat->wasm-success?
    (error 'drive-compilation
           (~a "wat->wasm failed to produce bytecode for " filename)))
  (when strip-wasm-names?
    (strip-wasm-name-section! 'drive-compilation
                              out-wasm
                              #:exec-path-from-shell? exec-path-from-shell?)
    (when (and split-runtime?
               (not split-runtime-cache-reference?)
               (not split-runtime-core-url))
      (strip-wasm-name-section! 'drive-compilation
                                runtime-wasm
                                #:exec-path-from-shell? exec-path-from-shell?))
    (when split-ffi-module
      (strip-wasm-name-section! 'drive-compilation
                                ffi-wasm
                                #:exec-path-from-shell? exec-path-from-shell?)))
  (ensure-generated-file-exists! 'drive-compilation out-wasm "Wasm bytecode file")
  (when (and split-runtime?
             (not split-runtime-cache-reference?)
             (not split-runtime-core-url))
    (ensure-generated-file-exists! 'drive-compilation runtime-wasm "split runtime Wasm bytecode file"))
  (when split-ffi-module
    (ensure-generated-file-exists! 'drive-compilation ffi-wat "split FFI WAT output file")
    (ensure-generated-file-exists! 'drive-compilation ffi-wasm "split FFI Wasm bytecode file"))
  (define t-assemble-end (now-ms))

  ; 7b. Write label map sidecar
  (define t-write-map-start (now-ms))
  (with-output-to-file out-map
    (λ () (pretty-write (label-map->sexp)))
    #:exists 'replace)
  (ensure-generated-file-exists! 'drive-compilation out-map "label-map output file")
  (define t-write-map-end (now-ms))

  ; 8. Write the host file (default: "runtime.js")
  (define t-write-host-start (now-ms))
  (define runtime-wasm-reference
    (host-wasm-reference browser? out-host out-wasm))
  (define split-runtime-wasm-reference
    (and split-runtime?
         (or split-runtime-core-url
             (host-wasm-reference
              browser?
              out-host
              (or split-runtime-core-reference-path runtime-wasm)))))
  (define split-runtime-ffi-wasm-reference
    (and split-ffi-module
         (host-wasm-reference browser? out-host ffi-wasm)))
  (with-output-to-file out-host
    (λ () (displayln
           (runtime #:out runtime-wasm-reference
                    #:core-out split-runtime-wasm-reference
                    #:ffi-out split-runtime-ffi-wasm-reference
                    #:core-provider-imports-from-core?
                    split-runtime-provider-runtime?
                    #:core-initialize? split-runtime-provider-runtime?
                    #:core-provider-entry
                    (and split-runtime-provider-runtime? "provider-entry")
                    #:host (if node? 'node 'browser)
                    #:console-bridge? console-bridge?
                    #:vfs-preloads vfs-preloads
                    #:vfs-mounts vfs-mounts)))
    #:exists 'replace)
  (ensure-generated-file-exists! 'drive-compilation out-host "host output file")
  (when output-cache-key
    (define cache-status
      (cache-compilation-output!
       output-cache-key
       output-cache-direct-output-files
       (map cdr sidecar-output-files)
       #:max-bytes compiled-output-cache-max-bytes
       #:max-entries compiled-output-cache-max-entries))
    (provider-cache-log "compiled output cache ~a: ~a"
                        cache-status
                        (compilation-output-cache-manifest-path output-cache-key)))
  (define t-write-host-end (now-ms))

  ; 9. Optionally run the program via Node.js.
  (define t-run-ms 0.0)
  (when (and node? run-after?)
    (define t-run-start (now-ms))
    (ensure-generated-file-exists! 'drive-compilation out-wasm "Wasm bytecode file")
    (define runtime-js out-host)
    (set! exit-code
          (run #f
               #:wat out-wat
               #:wasm out-wasm
               #:runtime.js runtime-js
               #:write-runtime? #f
               #:node-path node-path
               #:exec-path-from-shell? exec-path-from-shell?))
    (set! t-run-ms (- (now-ms) t-run-start)))

  ; 10. Output timings
  (define alpha-rename-profile-report
    (current-alpha-rename-profile-report))
  (define simplify-profile-report
    (current-simplify-profile-report))
  (when timings?
    (define ffi-setup-ms     (- t-ffi-setup-end   t-ffi-setup-start))
    (define read-source-ms   (- t-read-source-end t-read-source-start))
    (define add-stdlib-ms    (- t-add-stdlib-end  t-add-stdlib-start))
    (define preflight-ms     (- t-preflight-end   t-preflight-start))
    (define compile-ms       (- t-compile-end     t-compile-start))
    (define write-wat-ms     (- t-write-wat-end   t-write-wat-start))
    (define assemble-ms      (- t-assemble-end    t-assemble-start))
    (define write-map-ms     (- t-write-map-end   t-write-map-start))
    (define write-host-ms    (- t-write-host-end  t-write-host-start))
    (define compile-total-ms (- t-write-host-end  t-compile-start))
    (define driver-total-ms  (+ (- t-write-host-end t-driver-start)
                                t-run-ms))
    (set! compile-timings
          (compilation-timings ffi-setup-ms
                               read-source-ms
                               add-stdlib-ms
                               preflight-ms
                               compile-ms
                               write-wat-ms
                               assemble-ms
                               write-map-ms
                               write-host-ms
                               compile-total-ms
                               t-run-ms
                               driver-total-ms))
    (define read-detail-rows (reverse (current-read-source-timing-rows)))
    (define module-bridge-detail-rows (reverse (current-module-bridge-detail-rows)))
    (define split-runtime-core-detail-rows
      (reverse split-runtime-core-timing-rows))
    (define split-runtime-ffi-detail-rows
      (reverse split-runtime-ffi-timing-rows))
    (define driver-rows
      (append
       (list (list "ffi-setup"        ffi-setup-ms)
             (list "read-source"      read-source-ms)
             (list "add-stdlib"       add-stdlib-ms)
             (list "preflight"        preflight-ms)
             (list "compile-pipeline" compile-total-ms))
       (if (zero? t-run-ms)
           '()
           (list (list "run" t-run-ms)))
       (list (list "total" driver-total-ms))))
    (define overall-table
      (format-timing-table
       (list (list "compile"    compile-ms)
             (list "write-wat"  write-wat-ms)
             (list "assemble"   assemble-ms)
             (list "write-map"  write-map-ms)
             (list "write-host" write-host-ms)
             (list "total"      compile-total-ms))))
    (displayln "=== Driver Timings ===")
    (displayln (format-timing-table driver-rows))
    (displayln "=== Stdlib Decision ===")
    (displayln (format-metric-table stdlib-decision-rows))
    (when (pair? read-detail-rows)
      (displayln "=== Read Source Details ===")
      (displayln (format-timing-table read-detail-rows)))
    (when (pair? module-bridge-detail-rows)
      (displayln "=== Module Bridge Extract Details ===")
      (displayln
       (format-timing-table
        (append module-bridge-detail-rows
                (list (list "total"
                            (for/sum ([row (in-list module-bridge-detail-rows)])
                              (second row))))))))
      (define module-bridge-metric-rows
        (filter (lambda (row)
                  (let ([value (second row)])
                    (not (and (number? value)
                              (zero? value)))))
                (hash-ref (current-webify-bridge-stats)
                          'module-bridge-metric-rows
                          '())))
      (when (pair? module-bridge-metric-rows)
        (displayln
         (format-metric-table
          (for/list ([row (in-list module-bridge-metric-rows)])
            (list (car row) (~a (second row)))))))
    (when (pair? split-runtime-core-detail-rows)
      (displayln "=== Split Runtime Core Cache Details ===")
      (displayln
       (format-timing-table
        (append split-runtime-core-detail-rows
                (list (list "total"
                            (for/sum ([row (in-list split-runtime-core-detail-rows)])
                              (second row))))))))
    (when (pair? split-runtime-ffi-detail-rows)
      (displayln "=== Split Runtime FFI Cache Details ===")
      (displayln
       (format-timing-table
        (append split-runtime-ffi-detail-rows
                (list (list "total"
                            (for/sum ([row (in-list split-runtime-ffi-detail-rows)])
                              (second row))))))))
    (when (pair? split-runtime-provider-fragment-timing-rows)
      (displayln "=== Split Runtime Provider Fragment Details ===")
      (displayln
       (format-timing-table
        (append (reverse split-runtime-provider-fragment-timing-rows)
                (list (list "total"
                            (for/sum ([row (in-list split-runtime-provider-fragment-timing-rows)])
                              (second row)))))))
      (when (provider-fragment-splice-info?
             split-runtime-provider-fragment-splice-info)
        (displayln
         (format-metric-table
          (list
           (list "provider-fragment-forms"
                 (~a (provider-fragment-splice-info-provider-form-count
                      split-runtime-provider-fragment-splice-info)))
           (list "program-fragment-forms"
                 (~a (provider-fragment-splice-info-program-form-count
                      split-runtime-provider-fragment-splice-info)))
           (list "program-provider-global-imports-dropped"
                 (~a (provider-fragment-splice-info-dropped-provider-global-import-count
                      split-runtime-provider-fragment-splice-info)))
           (list "program-colliding-definitions"
                 (~a (length
                      (provider-fragment-splice-info-colliding-program-names
                       split-runtime-provider-fragment-splice-info))))
           (list "spliced-program-forms"
                 (~a (provider-fragment-splice-info-spliced-form-count
                      split-runtime-provider-fragment-splice-info))))))))
      (when (provider-runtime-merge-info?
             split-runtime-provider-runtime-merge-info)
        (displayln
         (format-metric-table
          (list
           (list "provider-runtime-core-forms"
                 (~a (provider-runtime-merge-info-core-form-count
                      split-runtime-provider-runtime-merge-info)))
           (list "provider-runtime-provider-forms"
                 (~a (provider-runtime-merge-info-provider-form-count
                      split-runtime-provider-runtime-merge-info)))
           (list "provider-runtime-kept-imports"
                 (~a (provider-runtime-merge-info-provider-import-count
                      split-runtime-provider-runtime-merge-info)))
           (list "provider-runtime-colliding-definitions"
                 (~a (length
                      (provider-runtime-merge-info-provider-colliding-definition-names
                       split-runtime-provider-runtime-merge-info))))
           (list "provider-runtime-merged-forms"
                 (~a (provider-runtime-merge-info-merged-runtime-form-count
                      split-runtime-provider-runtime-merge-info)))))))
    (displayln "=== Compile Pipeline ===")
    (displayln overall-table)
    (define pass-table (current-pass-timing-table))
    (when pass-table
      (displayln "=== Pass Timings ===")
      (displayln pass-table))
    (define gen-table (current-gen-timing-table))
    (when gen-table
      (displayln "=== Generate-Code Phases ===")
      (displayln gen-table)))
  (when alpha-rename-profile-report
    (displayln "=== Alpha-Rename Profile ===")
    (displayln alpha-rename-profile-report))
  (when simplify-profile-report
    (displayln "=== Simplify-LFE Profile ===")
    (displayln simplify-profile-report))
  (lambda () (values exit-code compile-timings)))])
    (result)))

;;;
;;; READ TOP-LEVEL FORMS FROM FILE 
;;;

(define (webracket-script-lang-line? line)
  (and (string? line)
       (regexp-match? #px"^#lang[ \t]+webracket([ \t].*)?$" line)))

(define (webracket-base-lang-line? line)
  (and (string? line)
       (regexp-match? #px"^#lang[ \t]+webracket/base([ \t].*)?$" line)))

(define (webracket-module-lang-line? line)
  (and (string? line)
       (regexp-match? #px"^#lang[ \t]+webracket/(base|full)([ \t].*)?$" line)))

(define (webracket-module-lang-line->language line)
  (define match
    (and (string? line)
         (regexp-match #px"^#lang[ \t]+webracket/(base|full)([ \t].*)?$" line)))
  (and match (string->symbol (cadr match))))

(define (webracket-module-source-language filename)
  (call-with-input-file filename
    (lambda (port)
      (or (webracket-module-lang-line->language (read-line port 'any))
          'base))))

(define (module-export-cell-symbol module-name export)
  (string->symbol (format "webify-module-export-cell$~a$~a" module-name export)))

(define (module-private-symbol module-name name)
  (string->symbol (format "webify-module-private$~a$~a" module-name name)))

(define (simple-provide-exports form)
  (match form
    [`(provide ,exports ...)
     (filter symbol? exports)]
    [_ '()]))

(define (simple-local-require-paths form)
  (define (spec->paths spec)
    (match spec
      [(? string?) (list spec)]
      [(? symbol?) '()]
      [`(combine-in ,inner ...)
       (append* (map spec->paths inner))]
      [`(,(or 'only-in 'except-in 'rename-in 'prefix-in
              'only-meta-in 'for-meta 'for-syntax 'for-template
              'for-label 'for-space)
         ,inner . ,_rest)
       (cond
         [(string? inner)
          (raise-arguments-error
           'read-top-level-from-file
           "unsupported #lang webracket/base local require spec"
           "spec" spec
           "supported" "plain local string requires, such as (require \"lib.rkt\")")]
         [else '()])]
      [_ '()]))
  (match form
    [`(require ,specs ...)
     (append* (map spec->paths specs))]
    [_ '()]))

(define (candidate-module-symbol->relative-rkt-path candidate)
  (and (symbol? candidate)
       (let ([name (symbol->string candidate)])
         (define path (string->path name))
         (if (path-get-extension path)
             path
             (string->path (string-append name ".rkt"))))))

;; path-within? : path-string? path-string? -> boolean?
;;   Check whether child is contained in parent after complete-path normalization.
(define (path-within? parent child)
  (define normalized-parent
    (simplify-path (path->complete-path parent)))
  (define normalized-child
    (simplify-path (path->complete-path child)))
  (define relative
    (find-relative-path normalized-parent normalized-child))
  (not (member 'up (explode-path relative))))

;; configured-local-collection-root : -> (or/c path? #f)
;;   Return the explicit highest-precedence collection root, when configured.
(define (configured-local-collection-root)
  (and (getenv "PLTCOLLECTS")
       (pair? (current-library-collection-paths))
       (car (current-library-collection-paths))))

;; collection-module->local-source-path : symbol? -> (or/c path? #f)
;;   Resolve a collection name only when it names source under the explicit local root.
(define (collection-module->local-source-path module-name)
  (define collection-root (configured-local-collection-root))
  (and collection-root
       (symbol? module-name)
       (with-handlers ([exn:fail? (lambda (_exn) #f)])
         (define resolved
           (module-path-index-resolve
            (module-path-index-join module-name #f)))
         (define source-path (resolved-module-path-name resolved))
         (and (path? source-path)
              (file-exists? source-path)
              (path-within? collection-root source-path)
              (simplify-path source-path)))))

;; local-require-spec-paths : any/c path? -> (listof path?)
;;   Discover source-visible local files before expanded metadata is available.
(define (local-require-spec-paths spec source-dir)
  (define (relative-path->complete path-string base-dir)
    (simplify-path (build-path base-dir path-string)))
  (define (module-path->local-complete-path module-path base-dir)
    (match module-path
      [(? string? path)
       (relative-path->complete path base-dir)]
      [`(file ,(? string? path))
       (relative-path->complete path base-dir)]
      [_ #f]))
  (match spec
    [(? symbol? module-name)
     (define source-path (collection-module->local-source-path module-name))
     (if source-path (list source-path) '())]
    [(? string?)
     (list (relative-path->complete spec source-dir))]
    [`(file ,(? string? path))
     (list (relative-path->complete path source-dir))]
    [`(relative-in ,base ,inner ...)
     (define base-path
       (module-path->local-complete-path base source-dir))
     (if base-path
         (let ([base-dir (or (path-only base-path) source-dir)])
           (append* (map (lambda (inner-spec)
                           (local-require-spec-paths inner-spec base-dir))
                         inner)))
         '())]
    [`(combine-in ,inner ...)
     (append* (map (lambda (inner-spec)
                     (local-require-spec-paths inner-spec source-dir))
                   inner))]
    [`(prefix-in ,_prefix ,inner)
     (local-require-spec-paths inner source-dir)]
    [`(,(or 'only-in 'except-in 'rename-in)
       ,inner . ,_rest)
     (local-require-spec-paths inner source-dir)]
    [`(only-meta-in 0 ,inner)
     (local-require-spec-paths inner source-dir)]
    [`(for-meta 0 ,inner ...)
     (append* (map (lambda (inner-spec)
                     (local-require-spec-paths inner-spec source-dir))
                   inner))]
    [_ '()]))

;; local-require-form-paths : any/c path? -> (listof path?)
;;   Discover local file dependencies from one source require form.
(define (local-require-form-paths form source-dir)
  (match form
    [`(require ,specs ...)
     (append* (map (lambda (spec)
                     (local-require-spec-paths spec source-dir))
                   specs))]
    [_ '()]))

;; webracket/base-report-local-dependencies : report path? -> (listof pair?)
;;   Find local source files and the exact provider names requested by one report.
(define (webracket/base-report-local-dependencies report source-dir)
  (define import-map (webify-module-report-import-map report))
  (define self-name (webify-module-report-name report))
  (remove-duplicates
   (for/list ([candidate (in-list (webify-module-import-map-required-module-candidates
                                    import-map))]
              #:when (= 0 (car candidate))
              #:do [(define module-name (cadr candidate))
                    (define local-collection-path
                      (collection-module->local-source-path module-name))
                    (define relative-path
                      (candidate-module-symbol->relative-rkt-path module-name))
                    (define path
                      (or local-collection-path
                          (and relative-path
                               (simplify-path
                                (if (absolute-path? relative-path)
                                    relative-path
                                    (build-path source-dir relative-path))))))]
              #:when (and (symbol? module-name)
                          path
                          (not (eq? module-name self-name))
                          (file-exists? path)))
     (cons module-name path))
   equal?))

;; webracket/base-report-local-dependency-name : report path? -> (or/c symbol? #f)
;;   Recover one unique report provider name for a source-visible local path.
(define (webracket/base-report-local-dependency-name report source-path)
  (define import-map (webify-module-report-import-map report))
  (define source-name (file-name-from-path source-path))
  (define matching-names
    (remove-duplicates
     (for/list
         ([candidate (in-list (webify-module-import-map-required-module-candidates
                               import-map))]
          #:when (= 0 (car candidate))
          #:do [(define module-name (cadr candidate))
                (define local-collection-path
                  (collection-module->local-source-path module-name))
                (define candidate-path
                  (candidate-module-symbol->relative-rkt-path module-name))]
          #:when (and (symbol? module-name)
                      (or (and local-collection-path
                               (equal? (simplify-path source-path)
                                       local-collection-path))
                          (and candidate-path
                               (equal? source-name
                                       (file-name-from-path candidate-path))))))
       module-name)
     eq?))
  (and (= (length matching-names) 1)
       (car matching-names)))

;; webracket/base-report-local-dependency-paths : report path? -> (listof path?)
;;   Find the local source files named by one extracted report.
(define (webracket/base-report-local-dependency-paths report source-dir)
  (map cdr (webracket/base-report-local-dependencies report source-dir)))

(define (simple-defined-symbols forms)
  (append-map stdlib-definition-form-symbols forms))

(define (module-rename-reference renames v)
  (cond
    [(symbol? v)
     (hash-ref renames v v)]
    [(pair? v)
     (match v
       [`(quote . ,_) v]
       [`(quote-syntax . ,_) v]
       [`(syntax . ,_) v]
       [`(quasiquote . ,_) v]
       [else
        (cons (module-rename-reference renames (car v))
              (module-rename-reference renames (cdr v)))])]
    [(vector? v)
     (list->vector
      (for/list ([elem (in-vector v)])
        (module-rename-reference renames elem)))]
    [else v]))

(define (module-rename-definition-name renames name)
  (hash-ref renames name name))

(define (module-rename-top-level-form renames form)
  (match form
    [`(define (,name . ,formals) . ,body)
     `(define (,(module-rename-definition-name renames name) . ,formals)
        ,@(map (lambda (expr) (module-rename-reference renames expr)) body))]
    [`(define ,name . ,rhs)
     `(define ,(module-rename-definition-name renames name)
        ,@(map (lambda (expr) (module-rename-reference renames expr)) rhs))]
    [`(define-values ,names . ,rhs)
     `(define-values ,(map (lambda (name) (module-rename-definition-name renames name))
                           names)
        ,@(map (lambda (expr) (module-rename-reference renames expr)) rhs))]
    [else
     (module-rename-reference renames form)]))

;; read-webracket/module : path-string? (or/c 'base 'full) -> syntax?
;;   Read a WebRacket module source through normal Racket module reading.
(define (read-webracket/module filename language)
  (with-driver-timing
   'module-read
   (lambda ()
     (define resolved (simplify-path (path->complete-path filename)))
     (call-with-input-file resolved
       (lambda (port)
         (port-count-lines! port)
         (define dir (or (path-only resolved) (current-directory)))
         (parameterize ([current-directory dir]
                        [current-load-relative-directory dir]
                        [current-write-relative-directory dir])
           (with-module-reading-parameterization
             (lambda ()
               ;; Module readers and expander-time relative requires must be
               ;; anchored to the source file, not the caller's cwd.
               (ensure-webracket/module-reader! language)
               (read-syntax resolved port)))))))))

;; read-webracket/base-module : path-string? -> syntax?
;;   Compatibility wrapper for WebRacket module readers.
(define (read-webracket/base-module filename)
  (webracket/base-substitute-module-requires
   (webracket/full-module->base-require-module
    (read-webracket/module filename (webracket-module-source-language filename)))))

;; webracket/full-module->base-require-module : syntax? -> syntax?
;;   Normalize #lang webracket/full to an explicit provider import. This keeps
;;   the full surface available while avoiding host-expander phase-1 lazy
;;   require resolution through the language-root module.
(define (webracket/full-module->base-require-module stx)
  (define datum (syntax->datum stx))
  (define rewritten
    (match datum
      [`(module ,name webracket/full (#%module-begin ,forms ...))
       `(module ,name webracket/base
          (#%module-begin
           (require webracket/full)
           ,@forms))]
      [`(module ,name webracket/full ,forms ...)
       `(module ,name webracket/base
          (require webracket/full)
          ,@forms)]
      [_ datum]))
  (if (equal? rewritten datum)
      stx
      (datum->syntax stx rewritten stx)))

;; webracket/base-provider-substitution : struct
;;   Describe one provider substitution so temporary shims stay visible.
(struct webracket/base-provider-substitution
  (requested routed classification pressure disposition reason)
  #:transparent)

;; substitution : symbol? symbol? symbol? symbol? symbol? string? -> webracket/base-provider-substitution?
;;   Keep the substitution table compact while retaining ownership metadata.
(define (substitution requested routed classification pressure disposition reason)
  (webracket/base-provider-substitution requested
                                        routed
                                        classification
                                        pressure
                                        disposition
                                        reason))

;; webracket/base-provider-substitution-records : (listof webracket/base-provider-substitution?)
;;   Single Goal 1 substitution registry for source-level requires and provider-graph fetching.
(define webracket/base-provider-substitution-records
  (list
   (substitution 'racket/contract 'webracket/contract 'temporary-shim 'high 'postpone
                 "erased contracts unblock provider composition until contract enforcement is available")
   (substitution 'racket/contract/base 'webracket/contract 'temporary-shim 'high 'postpone
                 "same erased-contract adapter as racket/contract")
   (substitution 'racket/contract/combinator 'webracket/contract 'temporary-shim 'high 'postpone
                 "same erased-contract adapter as racket/contract")
   (substitution 'racket/contract/region 'webracket/contract 'temporary-shim 'high 'postpone
                 "same erased-contract adapter as racket/contract")
   (substitution 'racket/contract/private/list 'webracket/contract 'temporary-shim 'high 'postpone
                 "same erased-contract adapter as racket/contract")
   (substitution 'racket/contract/private/misc 'webracket/contract 'temporary-shim 'high 'postpone
                 "same erased-contract adapter as racket/contract")
   (substitution 'racket/file 'webracket/file 'runtime-owned 'low 'keep-webracket-owned
                 "browser and VFS file behavior differs from host filesystem behavior")
   (substitution 'racket/format 'webracket/private/format 'staged-original 'medium 'keep-webracket-owned
                 "route source expansion to the staged io/format-backed adapter")
   (substitution 'racket/generator 'webracket/generator 'diagnostic-shim 'low 'postpone
                 "generators depend on continuation prompts, which are outside Goal 1")
   (substitution 'racket/hash 'webracket/hash 'temporary-shim 'medium 'keep-webracket-owned
                 "hash helpers should track WebRacket HAMT/runtime hash behavior")
   (substitution 'racket/list 'webracket/list 'runtime-owned 'low 'keep-webracket-owned
                 "list traversal helpers must resolve to WebRacket's iterative runtime primitives")
   (substitution 'racket/string 'webracket/string 'runtime-owned 'low 'keep-webracket-owned
                 "string assembly must resolve to WebRacket's iterative browser runtime helpers")
   (substitution 'racket/private/map 'webracket/private/map 'runtime-owned 'low 'keep-webracket-owned
                 "the base expander names its recursive map provider directly")
   (substitution 'racket/mpair 'webracket/mpair 'runtime-owned 'low 'keep-webracket-owned
                 "mutable-pair representation is WebRacket runtime work")
   (substitution 'racket/port 'webracket/port 'runtime-owned 'low 'keep-webracket-owned
                 "ports are tied to WebRacket string ports, VFS, and browser IO")
   (substitution 'racket/private/kw 'webracket/private/kw 'runtime-owned 'high 'keep-webracket-owned
                 "keyword-procedure structs must share one WebRacket runtime type across composed providers")
   (substitution 'racket/private/promise 'webracket/promise 'runtime-owned 'low 'keep-webracket-owned
                 "private promise helpers share the browser-local promise implementation")
   (substitution 'racket/private/sort 'webracket/private/sort 'runtime-owned 'high 'keep-webracket-owned
                 "keyword-capable sort bindings should use WebRacket's primitive sort bridge")
   (substitution 'racket/private/share-search 'webracket/private/share-search 'runtime-owned 'low 'keep-webracket-owned
                 "runtime-path shared-data search has no browser host directory equivalent")
   (substitution 'racket/private/so-search 'webracket/private/so-search 'runtime-owned 'low 'keep-webracket-owned
                 "runtime-path shared-object search has no browser native-library equivalent")
   (substitution 'racket/promise 'webracket/promise 'runtime-owned 'low 'keep-webracket-owned
                 "ordinary promises are small browser-local runtime behavior")
   (substitution 'racket/set 'webracket/set 'temporary-shim 'medium 'keep-webracket-owned
                 "set support follows WebRacket hash/HAMT behavior while contracts/generics are postponed")
   (substitution 'racket/stream 'webracket/stream 'temporary-shim 'medium 'postpone
                 "staged streams preserve lazy syntax without importing full stream internals")
   (substitution 'racket/unsafe/undefined 'webracket/unsafe/undefined 'runtime-owned 'low 'keep-webracket-owned
                 "undefined values need WebRacket representation support")
   (substitution 'syntax/contract 'webracket/syntax/contract 'temporary-shim 'high 'postpone
                 "supports contract erasure at expansion and provider time")))

;; webracket/base-erased-contract-provider-name? : symbol? -> boolean?
;;   Recognize contract implementation providers that real-provider diagnostics
;;   should satisfy with WebRacket's erased contract module during extraction.
(define (webracket/base-erased-contract-provider-name? name)
  (memq name
        '(racket/contract
          racket/contract/base
          racket/contract/combinator
          racket/contract/region
          racket/in-out
          racket/module-boundary-ctc
          racket/contract/private/arrow-collapsible
          racket/contract/private/arrow-common
          racket/contract/private/arrow-higher-order
          racket/contract/private/arrow-val-first
          racket/contract/private/base
          racket/contract/private/blame
          racket/contract/private/case-arrow
          racket/contract/private/guts
          racket/contract/private/list
          racket/contract/private/module-boundary-ctc
          racket/contract/private/misc
          racket/contract/private/object
          racket/contract/private/object-c-wrapper
          racket/contract/private/opt
          racket/contract/private/orc
          racket/contract/private/prop
          ;; Racket/contract's compiled metadata can expose relative contract
          ;; implementation paths. They are still contract-owned and should use
          ;; the erased WebRacket contract provider during Goal 1 diagnostics.
          private/arrow-common
          private/arrow-val-first
          private/base
          private/guts
          private/helpers
          private/misc)))

;; webracket/base-provider-substitution-pairs : (listof (cons/c symbol? symbol?))
;;   Pair view of the structured substitution registry.
(define webracket/base-provider-substitution-pairs
  (for/list ([record (in-list webracket/base-provider-substitution-records)])
    (cons (webracket/base-provider-substitution-requested record)
          (webracket/base-provider-substitution-routed record))))

;; webracket/base-provider-substitute : symbol? -> symbol?
;;   Route selected installed providers to smaller WebRacket-owned Goal 1 shims.
(define (webracket/base-provider-substitute name)
  (if (current-webracket/base-provider-substitutions?)
      (let ([maybe (assq name webracket/base-provider-substitution-pairs)])
        (if maybe
            (cdr maybe)
            name))
      name))

;; substitute-require-module-path : any/c -> any/c
;;   Rewrite selected source-level require paths to WebRacket-owned providers.
(define (substitute-require-module-path module-path)
  (if (symbol? module-path)
      (webracket/base-provider-substitute module-path)
      module-path))

;; substitute-require-spec : any/c -> any/c
;;   Rewrite provider paths inside common require combinators.
(define (substitute-require-spec spec)
  (match spec
    [(? symbol?) (substitute-require-module-path spec)]
    [`(,(and form
             (or 'only-in 'except-in 'rename-in 'prefix-in
                 'combine-in 'only-meta-in 'for-meta 'for-syntax
                 'for-template 'for-label 'for-space))
       ,body ...)
     (cons form (map substitute-require-spec body))]
    [_ spec]))

;; substitute-require-form : any/c -> any/c
;;   Rewrite module paths in one require form.
(define (substitute-require-form form)
  (match form
    [`(require ,specs ...)
     `(require ,@(map substitute-require-spec specs))]
    [_ form]))

;; webracket/base-substitute-module-requires : syntax? -> syntax?
;;   Apply Goal 1 provider substitutions before module extraction/expansion.
(define (webracket/base-substitute-module-requires stx)
  (define datum (syntax->datum stx))
  (define rewritten
    (match datum
      [`(module ,name ,language (#%module-begin ,forms ...))
       `(module ,name ,language (#%module-begin ,@(map substitute-require-form forms)))]
      [`(module ,name ,language ,forms ...)
       `(module ,name ,language ,@(map substitute-require-form forms))]
      [_ datum]))
  (if (equal? rewritten datum)
      stx
      (datum->syntax stx rewritten stx)))

;; module-stx->name+body : syntax? -> values
;;   Extract the module name and body from a #lang webracket/base or full datum.
(define (module-stx->name+body stx)
  (define datum (syntax->datum stx))
  (match datum
    [`(module ,(? symbol? name) ,(and language
                                      (or 'webracket/base 'webracket/full))
              (#%module-begin ,forms ...))
     (values name forms)]
    [`(module ,(? symbol? name) ,(and language
                                      (or 'webracket/base 'webracket/full))
              ,forms ...)
     (values name forms)]
    [_
     (raise-arguments-error
      'read-top-level-from-file
      "expected a #lang webracket/base or #lang webracket/full module"
      "datum" datum)]))

;; webracket/base-module->top-level-forms : path-string? [hash?] [boolean?] -> values
;;   Inline simple local string requires for the transitional top-level path.
(define (webracket/base-module->top-level-forms filename [seen (make-hash)] [root? #t])
  (define resolved (simplify-path (path->complete-path filename)))
  (cond
    [(hash-ref seen resolved #f)
     (values '() '())]
    [else
     (hash-set! seen resolved #t)
     (define stx (read-webracket/base-module resolved))
     (define-values (module-name body) (module-stx->name+body stx))
     (define source-dir (or (path-only resolved) (current-directory)))
     (define require-paths (append-map simple-local-require-paths body))
     (define-values (required-forms _required-exports)
       (for/fold ([forms '()]
                  [exports '()])
                 ([req (in-list require-paths)])
         (define req-path (simplify-path (build-path source-dir req)))
         (define-values (req-forms req-exports)
           (webracket/base-module->top-level-forms req-path seen #f))
         (values (append forms req-forms)
                 (append exports req-exports))))
     (define exports (append-map simple-provide-exports body))
     (define body-top-level-forms
       (filter (lambda (form)
                 (not (and (pair? form)
                           (memq (car form) '(provide module+ require)))))
               body))
     (define private-renames
       (if root?
           (hasheq)
           (for/hasheq ([name (in-list (simple-defined-symbols body-top-level-forms))]
                        #:unless (memq name exports))
             (values name (module-private-symbol module-name name)))))
     (define top-level-forms
       (if root?
           body-top-level-forms
           (map (lambda (form) (module-rename-top-level-form private-renames form))
                body-top-level-forms)))
     (define export-forms
       (for/list ([export (in-list exports)])
         (define cell (module-export-cell-symbol module-name export))
         `(begin
            (define ,cell (module-variable-make ',export))
            (module-variable-define! ,cell ,export 'constant))))
     (values (append required-forms top-level-forms export-forms)
             exports)]))

;; module-form->top-level : path-string? -> syntax?
;;   Transitional #lang webracket/base support for the top-level compiler path.
(define (module-file->top-level filename)
  (define-values (forms _exports)
    (webracket/base-module->top-level-forms filename))
  (datum->syntax #f `(begin ,@forms)))

;; webracket-sequence-prelude-forms : -> datum
;;   Provide the private generic sequence adapter used by expanded `for` forms.
(define (webracket-sequence-prelude-forms)
  '((define (make-sequence who v)
      (cond
        [(exact-nonnegative-integer? v)
         (values (lambda (pos) pos)
                 #f
                 add1
                 0
                 (lambda (pos) (< pos v))
                 #f
                 #f)]
        [(list? v)
         (values car
                 #f
                 cdr
                 v
                 pair?
                 #f
                 #f)]
        [(vector? v)
         (values (lambda (pos) (vector-ref v pos))
                 #f
                 add1
                 0
                 (lambda (pos) (< pos (vector-length v)))
                 #f
                 #f)]
        [(string? v)
         (values (lambda (pos) (string-ref v pos))
                 #f
                 add1
                 0
                 (lambda (pos) (< pos (string-length v)))
                 #f
                 #f)]
        [(bytes? v)
         (values (lambda (pos) (bytes-ref v pos))
                 #f
                 add1
                 0
                 (lambda (pos) (< pos (bytes-length v)))
                 #f
                 #f)]
        [(hash? v)
         (values (lambda (pos) (hash-iterate-key+value v pos #f))
                 #f
                 (lambda (pos) (hash-iterate-next v pos))
                 (hash-iterate-first v)
                 values
                 #f
                 #f)]
        [else
         (raise-argument-error 'for "sequence?" v)]))))

;; with-webracket-sequence-prelude : syntax? -> syntax?
;;   Add generic sequence support without exposing it as a language export.
(define (with-webracket-sequence-prelude stx)
  (datum->syntax
   stx
   `(begin ,@(webracket-sequence-prelude-forms)
           ,@(match (syntax->datum stx)
               [`(begin ,forms ...) forms]
               [datum (list datum)]))
   stx))

(struct webracket/base-local-module-graph (root reports registry module-kinds)
  #:transparent)

;; module-reports->provider-names : (listof webify-module-report?) -> (listof symbol?)
;;   Collect local and required provider names from extracted module reports.
(define (module-reports->provider-names reports)
  (remove-duplicates
   (append (map webify-module-report-name reports)
           (append-map webify-module-report-required-provider-names reports))
   eq?))

;; local-source-path->provider-name : path? path? -> symbol?
;;   Name a local source module by its root-directory-relative path.
(define (local-source-path->provider-name root-source-dir source-path)
  (define (path-element->string part)
    (cond
      [(eq? part 'same) "."]
      [(eq? part 'up) ".."]
      [else (path->string part)]))
  (define (strip-rkt-suffix text)
    (if (string-suffix? text ".rkt")
        (substring text 0 (- (string-length text) 4))
        text))
  (define relative-path
    (find-relative-path root-source-dir
                        (simplify-path (path->complete-path source-path))))
  (define raw-parts
    (filter (lambda (part) (not (eq? part 'same)))
            (explode-path relative-path)))
  (define parts
    (for/list ([part (in-list raw-parts)])
      (path-element->string part)))
  (define normalized-parts
    (cond
      [(null? parts) parts]
      [else
       (append (reverse (cdr (reverse parts)))
               (list (strip-rkt-suffix (car (reverse parts)))))]))
  (string->symbol (string-join normalized-parts "/")))

;; module-composer-primitive-overrides : -> hash?
;;   Override primitive cells whose compiled-module role differs from scripts.
(define (module-composer-primitive-overrides)
  (hasheq
   'print-values
   `(let ([cell (module-variable-make 'print-values)])
      ;; Module body expression values are discarded, unlike script top-level
      ;; results that may be printed by --print-top-level-results.
      (module-variable-define! cell (lambda values (void)) 'constant)
      cell)))

;; module-datum-direct-require-symbols : any/c -> (listof symbol?)
;;   Find plain direct external require symbols in a source module datum.
(define (module-datum-direct-require-symbols module-datum)
  (define (spec->symbols spec)
    (match spec
      [(? symbol?) (list spec)]
      [`(prefix-in ,_prefix ,inner)
       (spec->symbols inner)]
      [`(,(or 'only-in 'except-in 'rename-in) ,inner . ,_rest)
       (spec->symbols inner)]
      [`(,(or 'combine-in) ,inner ...)
       (append* (map spec->symbols inner))]
      [`(only-meta-in 0 ,inner)
       (spec->symbols inner)]
      [`(for-meta ,_phase ,inner ...)
       (append* (map spec->symbols inner))]
      [`(,(or 'for-syntax 'for-template 'for-label 'for-space)
         ,inner . ,_rest)
       (spec->symbols inner)]
      [_ '()]))
  (define (form->symbols form)
    (match form
      [`(require ,specs ...)
       (append* (map spec->symbols specs))]
      [`(#%module-begin ,inner ...)
       (append* (map form->symbols inner))]
      [_ '()]))
  (match module-datum
    [`(module ,_name ,_language ,body ...)
     (remove-duplicates (append* (map form->symbols body)) eq?)]
    [_ '()]))

;; provider-compile-stub-exports : struct
;;   Cache the export table needed to install one compile-time provider stub.
(struct provider-compile-stub-exports
  (safe? public-exports phase0-exports)
  #:transparent)

;; symbol-list? : any/c -> boolean?
;;   Check whether v is a list of symbols.
(define (symbol-list? v)
  (and (list? v) (andmap symbol? v)))

;; provider-stub-exports-cache-path : symbol? -> path?
;;   Cache one provider compile-stub export table by requested provider name.
(define (provider-stub-exports-cache-path name)
  (provider-cache-path
   "provider-stub-exports"
   (string-append (provider-cache-symbol-segment name) ".rktd")))

;; provider-stub-exports-fingerprint : symbol? symbol? -> any/c
;;   Fingerprint provider compile-stub exports using source and extractor code.
(define (provider-stub-exports-fingerprint name substitute-name)
  `(provider-stub-exports-cache
    2
    ,name
    ,substitute-name
    ,(version)
    ,(current-webracket/base-provider-substitutions?)
    ,(collection-paths-fingerprint)
    ,(provider-module-source-fingerprint substitute-name)
    ,(provider-report-extractor-fingerprint)))

;; provider-stub-exports->cache-datum : provider-compile-stub-exports? -> datum?
;;   Serialize the compile-stub export table.
(define (provider-stub-exports->cache-datum exports)
  `(provider-stub-exports
    ,(provider-compile-stub-exports-safe? exports)
    ,(provider-compile-stub-exports-public-exports exports)
    ,(provider-compile-stub-exports-phase0-exports exports)))

;; cache-datum->provider-stub-exports : any/c -> (or/c provider-compile-stub-exports? #f)
;;   Rehydrate a cached compile-stub export table.
(define (cache-datum->provider-stub-exports datum)
  (match datum
    [`(provider-stub-exports ,safe? ,public-exports ,phase0-exports)
     (and (boolean? safe?)
          (symbol-list? public-exports)
          (symbol-list? phase0-exports)
          (provider-compile-stub-exports safe? public-exports phase0-exports))]
    [_ #f]))

;; cached-provider-stub-exports : symbol? symbol? (-> provider-compile-stub-exports?) -> provider-compile-stub-exports?
;;   Reuse compile-stub export tables across local module extraction runs.
(define (cached-provider-stub-exports name substitute-name build-exports)
  (define cache-path (provider-stub-exports-cache-path name))
  (define fingerprint (provider-stub-exports-fingerprint name substitute-name))
  (define (read-cache)
    (define cached
      (with-module-bridge-timing
       'provider-stub-cache-lookup
       (lambda ()
         (read-provider-cache cache-path fingerprint))))
    (and cached
         (let ([decoded (cache-datum->provider-stub-exports cached)])
           (cond
             [decoded
              (provider-cache-log "#lang webracket/base provider stub hit: ~a via ~a"
                                  name
                                  substitute-name)
              (with-module-bridge-timing
               'provider-stub-cache-hit
               (lambda () decoded))]
             [else
              (provider-cache-log "#lang webracket/base provider stub cache unreadable: ~a via ~a"
                                  name
                                  substitute-name)
              #f]))))
  (or (read-cache)
      (call-with-provider-cache-lock
       cache-path
       (lambda ()
         (or (read-cache)
             (let ([exports
                    (with-module-bridge-timing
                     'provider-stub-cache-miss
                     build-exports)])
               (write-provider-cache! cache-path
                                      fingerprint
                                      (provider-stub-exports->cache-datum exports))
               exports))))))

;; declare-provider-compile-stub! : namespace? symbol? (listof symbol?) -> void?
;;   Declare a compile-time-only provider stub for local module extraction.
(define (declare-provider-compile-stub! namespace name exports)
  (define unique-exports (remove-duplicates exports eq?))
  (parameterize ([current-namespace namespace])
    (namespace-call-with-registry-lock
     namespace
     (lambda ()
       (eval
        `(module ,name racket/base
           ,@(for/list ([export (in-list unique-exports)])
               `(define-values (,export) #f))
           (provide ,@unique-exports)))))))

;; provider-public-export-symbols : symbol? namespace? procedure? -> (listof symbol?)
;;   Approximate source-level value exports added by requiring a provider.
(define (provider-public-export-symbols name namespace module-name-resolver)
  (define before (namespace-mapped-symbols namespace))
  (with-handlers ([exn:fail? (lambda (_exn) '())])
    (parameterize ([current-namespace namespace]
                   [current-module-name-resolver module-name-resolver])
      (namespace-require name)
      (define after (namespace-mapped-symbols namespace))
      (for/list ([sym (in-list after)]
                 #:unless (memq sym before))
        sym))))

;; shared-webify-contract-erasing-namespace/ref : -> namespace?
;;   Reuse one erased-contract extraction namespace per driver process.
(define shared-webify-contract-erasing-namespace/cache (box #f))
(define (shared-webify-contract-erasing-namespace/ref)
  (or (unbox shared-webify-contract-erasing-namespace/cache)
      (let ([ns (make-webify-contract-erasing-namespace)])
        (set-box! shared-webify-contract-erasing-namespace/cache ns)
        ns)))

;; shared-webify-contract-erasing-module-name-resolver/ref : -> procedure?
;;   Reuse the matching erased-contract resolver alongside the shared namespace.
(define shared-webify-contract-erasing-module-name-resolver/cache (box #f))
(define (shared-webify-contract-erasing-module-name-resolver/ref)
  (or (unbox shared-webify-contract-erasing-module-name-resolver/cache)
      (let ([resolver (make-webify-contract-erasing-module-name-resolver)])
        (set-box! shared-webify-contract-erasing-module-name-resolver/cache
                  resolver)
        resolver)))

;; lib-module-path->symbol : any/c -> (or/c symbol? #f)
;;   Convert simple `(lib "x.rkt" "collection" ...)` paths to shorthand names.
(define (lib-module-path->symbol module-path)
  (define (join-path-parts parts)
    (cond
      [(null? parts) ""]
      [(null? (cdr parts)) (car parts)]
      [else (string-append (car parts) "/" (join-path-parts (cdr parts)))]))
  (match module-path
    [`(lib ,file ,collections ...)
     (and (string? file)
          (andmap string? collections)
          (regexp-match? #rx"[.]rkt$" file)
          (string->symbol
           (join-path-parts
            (append collections
                    (list (regexp-replace #rx"[.]rkt$" file ""))))))]
    [_ #f]))

;; module-path-compile-stub-name : any/c -> (or/c symbol? #f)
;;   Recognize module-path spellings that should resolve to a local stub.
(define (module-path-compile-stub-name module-path)
  (cond
    [(symbol? module-path) module-path]
    [else (lib-module-path->symbol module-path)]))

;; webracket-owned-module-name? : symbol? -> boolean?
;;   Recognize modules that should be compiled directly from this collection.
(define (webracket-owned-module-name? name)
  (regexp-match? #rx"^webracket/" (symbol->string name)))

;; webracket-owned-provider-compile-stub-name? : symbol? -> boolean?
;;   Some WebRacket-owned staged adapters compile cleanly by module path but
;;   are still too awkward to inline into the local source-module extraction
;;   namespace. Let them use the same value-only compile-stub path as external
;;   providers, then fetch their real bridge reports later.
(define (webracket-owned-provider-compile-stub-name? name)
  (memq name
        '(webracket/private/format)))

(define provider-phase0-syntax-export-cache (make-hasheq))

;; provider-has-phase0-syntax-exports? : symbol? -> boolean?
;;   Check whether a provider exports phase-0 syntax that value stubs cannot fake.
(define (provider-has-phase0-syntax-exports? name)
  (hash-ref!
   provider-phase0-syntax-export-cache
   name
   (lambda ()
     (with-handlers ([exn:fail? (lambda (_exn) #t)])
       (define-values (_values syntaxes) (module->exports name))
       (define phase0 (assoc 0 syntaxes))
       (and phase0 (pair? (cdr phase0)))))))

;; provider-compile-stub-safe? : symbol? -> boolean?
;;   Value-only compile stubs are unsafe for providers with macro exports.
(define (provider-compile-stub-safe? name)
  (and (not (memq name '(racket/match racket/match/runtime racket/match/gen-match)))
       (not (provider-has-phase0-syntax-exports? name))))

;; compile-webracket/base-module-file-report : path-string? [#:namespace namespace?] [#:module-name-resolver procedure?] -> webify-module-report?
;;   Compile one module file from its absolute source path.
(define (compile-webracket/base-module-file-report filename
                                                  #:namespace [namespace (make-base-namespace)]
                                                  #:module-name-resolver [module-name-resolver (current-module-name-resolver)])
  (define resolved (simplify-path (path->complete-path filename)))
  (define module-stx (read-webracket/base-module resolved))
  (define source-dir (or (path-only resolved) (current-directory)))
  ;; Read with the normal resolver so #lang webracket/base itself is found,
  ;; then compile with the erased-contract resolver for provider imports. The
  ;; root source module itself must still compile relative to its own path so
  ;; sibling string requires resolve the same way they do during ordinary host
  ;; expansion.
  (provider-cache-log "#lang webracket/base local compile start: ~a" resolved)
  (with-module-bridge-timing
   'source-compile-decompile
   (lambda ()
     (parameterize ([current-directory source-dir]
                    [current-load-relative-directory source-dir]
                    [current-write-relative-directory source-dir])
       (webify-compile-module-report
        module-stx
        #:namespace namespace
        #:module-name-resolver module-name-resolver)))))

;; webracket/base-file->local-module-reports : path-string? -> values
;;   Extract local module reports plus the visited source files for the bridge path.
(define (webracket/base-file->local-module-reports filename)
  (define root-resolved (simplify-path (path->complete-path filename)))
  (define root-source-dir (or (path-only root-resolved) (current-directory)))
  (define extraction-namespace (make-base-namespace))
  (define base-module-name-resolver (current-module-name-resolver))
  (define (provider-namespace/ref)
    (shared-webify-contract-erasing-namespace/ref))
  (define (provider-module-name-resolver/ref)
    (shared-webify-contract-erasing-module-name-resolver/ref))
  (define compile-stub-names '())
  (define local-report-names (make-hash))
  (define (module-name-resolver module-path source syntax load?)
    (define stub-name (module-path-compile-stub-name module-path))
    (cond
      [(and stub-name (memq stub-name compile-stub-names))
       (make-resolved-module-path stub-name)]
      [else
       (base-module-name-resolver module-path source syntax load?)]))
  (define module-name-resolver*
    (case-lambda
      [(module-path source syntax load?)
       (module-name-resolver module-path source syntax load?)]
      [(resolved namespace)
       (base-module-name-resolver resolved namespace)]))
  (define (install-compile-stubs! module-datum)
    (with-module-bridge-timing
     'provider-substitution
     (lambda ()
       (for ([name (in-list (module-datum-direct-require-symbols module-datum))]
             #:unless (webracket/base-language-provider-name? name)
             #:unless (and (webracket-owned-module-name? name)
                           (not (webracket-owned-provider-compile-stub-name? name)))
             #:when (webify-provider-trial-module-allowed? name))
         (define substitute-name (webracket/base-provider-substitute name))
         (unless (memq name compile-stub-names)
           (provider-cache-log "#lang webracket/base compile stub install: ~a via ~a"
                               name
                               substitute-name)
           (define stub-exports
             (cached-provider-stub-exports
              name
              substitute-name
              (lambda ()
                (cond
                  [(provider-compile-stub-safe? name)
                   (define use-erased-provider-namespace?
                     (not (webracket-owned-ffi-provider-name? substitute-name)))
                   (define report
                     (if use-erased-provider-namespace?
                         (parameterize ([current-namespace (provider-namespace/ref)]
                                        [current-module-name-resolver
                                         (provider-module-name-resolver/ref)])
                           (webify-module-path->report substitute-name
                                                       #:namespace (provider-namespace/ref)))
                         (webify-module-path->report substitute-name)))
                   (define public-exports
                     (provider-public-export-symbols
                      substitute-name
                      (if use-erased-provider-namespace?
                          (make-webify-contract-erasing-namespace)
                          (make-base-namespace))
                      (if use-erased-provider-namespace?
                          (provider-module-name-resolver/ref)
                          base-module-name-resolver)))
                   (provider-compile-stub-exports
                    #t
                    public-exports
                    (webify-module-report-phase0-exports report))]
                  [else
                   (provider-compile-stub-exports #f '() '())]))))
           (when (provider-compile-stub-exports-safe? stub-exports)
             (declare-provider-compile-stub!
              extraction-namespace
              name
              (append (provider-compile-stub-exports-public-exports stub-exports)
                      (provider-compile-stub-exports-phase0-exports stub-exports)))
             (set! compile-stub-names (cons name compile-stub-names))))))))
  ;; local-dependency-pairs : report path? (listof path?) -> (listof pair?)
  ;;   Pair discovered local source files with their required provider names.
  (define (local-dependency-pairs report source-dir source-dependency-paths)
    (define report-dependencies
      (webracket/base-report-local-dependencies report source-dir))
    (define fallback-dependencies
      (for/list ([path (in-list source-dependency-paths)]
                 #:unless (member path (map cdr report-dependencies) equal?))
        (cons (or (webracket/base-report-local-dependency-name report path)
                  (local-source-path->provider-name root-source-dir path))
              path)))
    (remove-duplicates
     (append report-dependencies fallback-dependencies)
     equal?))
  ;; register-local-report! : path? report symbol? -> symbol?
  ;;   Register one requested alias for a previously extracted local source module.
  (define (register-local-report! path report module-name)
    (define names (hash-ref local-report-names path '()))
    (unless (memq module-name names)
      (set! reports-reverse
            (cons (webify-module-report/rename report module-name)
                  reports-reverse))
      (hash-set! local-report-names path (cons module-name names)))
    module-name)
  ;; source-path-matches-provider-name? : path? symbol? -> boolean?
  ;;   Recognize an unresolved local alias by its complete discovered source-file suffix.
  (define (source-path-matches-provider-name? source-path provider-name)
    (define candidate-path
      (candidate-module-symbol->relative-rkt-path provider-name))
    (and (local-looking-provider-name? provider-name)
         candidate-path
         (string-suffix? (path->string source-path)
                         (string-append "/" (path->string candidate-path)))))
  ;; register-external-local-aliases! : void? -> boolean?
  ;;   Add aliases only for unresolved names that identify one discovered local file.
  (define (register-external-local-aliases!)
    (define reports (reverse reports-reverse))
    (define changed? #f)
    (for ([provider-name (in-list (webracket/base-external-provider-names reports))])
      (define matching-paths
        (for/list ([source-path (in-list (hash-keys permanent))]
                   #:when (source-path-matches-provider-name? source-path provider-name))
          source-path))
      (when (= (length matching-paths) 1)
        (define source-path (car matching-paths))
        (unless (memq provider-name (hash-ref local-report-names source-path '()))
          (set! changed? #t)
          (register-local-report! source-path
                                  (hash-ref permanent source-path)
                                  provider-name))))
    changed?)
  (define visiting (make-hash))
  (define permanent (make-hash))
  (define reports-reverse '())
  (define (visit path [requested-name #f])
    (define resolved (simplify-path (path->complete-path path)))
    (cond
      [(hash-ref permanent resolved #f)
       => (lambda (report)
            (register-local-report!
             resolved
             report
             (or requested-name
                 (local-source-path->provider-name root-source-dir resolved))))]
      [(hash-ref visiting resolved #f)
       (raise-arguments-error
        'read-top-level-from-file
        "#lang webracket/base local module cycle is not supported"
        "path" resolved)]
      [else
       (hash-set! visiting resolved #t)
       (define cached
         (with-module-bridge-timing
          'local-module-report-cache-lookup
          (lambda ()
            (read-local-module-report-cache resolved))))
       (define-values (report dependencies)
         (cond
           [cached
            (provider-cache-log "#lang webracket/base local module report hit: ~a"
                                resolved)
            (with-module-bridge-timing
             'local-module-report-cache-hit
             (lambda ()
               (define report (vector-ref cached 0))
               (define source-dir (or (path-only resolved) (current-directory)))
               (values report
                       (local-dependency-pairs report
                                               source-dir
                                               (vector-ref cached 1)))))]
           [else
            (provider-cache-log "#lang webracket/base local module report miss: ~a"
                                resolved)
            (with-module-bridge-timing
             'local-module-report-cache-miss
             (lambda ()
               (define source-dir (or (path-only resolved) (current-directory)))
               (define module-stx (read-webracket/base-module resolved))
               (define module-datum (syntax->datum module-stx))
               (install-compile-stubs! module-datum)
               (define report
                 (compile-webracket/base-module-file-report
                  resolved
                  #:namespace extraction-namespace
                  #:module-name-resolver module-name-resolver*))
               (define source-dependency-paths
                 (remove-duplicates
                  (append-map (lambda (form)
                                (local-require-form-paths form source-dir))
                              (match module-datum
                                [`(module ,_name ,_language (#%module-begin ,forms ...)) forms]
                                [`(module ,_name ,_language ,forms ...) forms]
                                [_ '()]))
                  equal?))
               (define dependencies
                 (local-dependency-pairs report source-dir source-dependency-paths))
               (write-local-module-report-cache! resolved report (map cdr dependencies))
               (values report dependencies)))]))
       (define module-name
         (or requested-name
             (local-source-path->provider-name root-source-dir resolved)))
       (for ([dependency (in-list dependencies)])
         (visit (cdr dependency) (car dependency)))
       (hash-remove! visiting resolved)
       (hash-set! permanent resolved report)
       (register-local-report! resolved report module-name)]))
  (define root (visit filename))
  (let loop ([remaining (add1 (length (hash-keys permanent)))])
    (when (register-external-local-aliases!)
      (when (zero? remaining)
        (raise-arguments-error
         'webracket/base-file->local-module-reports
         "local provider aliases did not reach a bounded fixed point"
         "local source paths" (hash-keys permanent)))
      (loop (sub1 remaining))))
  (define local-source-paths
    (sort (hash-keys permanent)
          string<?
          #:key path->string))
  (values root (reverse reports-reverse) local-source-paths))

;; webracket/base-language-provider-name? : symbol? -> boolean?
;;   Recognize providers that are part of the #lang webracket/base language root.
(define (webracket/base-language-provider-name? name)
  (memq name
        '(webracket/base/main
          webracket/base
          webracket/js/main
          webracket/js
          webracket/core
          webracket/primitives
          ;; Some host-expanded source can still report legacy relative names.
          ;; Treat them as language-root providers while the bridge normalizes
          ;; installed and package-local module paths.
          base/main
          base
          js/main
          js
          core
          primitives
          #%kernel
          fully
          browser-options
          racket/base
          racket/private/base
          racket/private/misc
          racket/private/pre-base
          ;; Keyword-lambda expansion names this host implementation directly,
          ;; but runtime calls are supplied by `webracket/private/kw`. Keeping
          ;; the host module out of provider discovery prevents the shim from
          ;; being substituted back into itself.
          racket/private/kw
          ;; Require/provide transformer helpers run in the host expander while
          ;; extracting module metadata; they are not runtime provider demands.
          racket/private/modbeg
          racket/require
          racket/require-syntax
          racket/provide
          racket/provide-syntax)))

;; webracket/base-reports-demand-external-providers? : (listof webify-module-report?) -> boolean?
;;   Check whether a bridge graph needs installed-provider discovery.
(define (webracket/base-reports-demand-external-providers? reports)
  (pair? (webracket/base-external-provider-names reports)))

;; module-report-imports-expander-read-alias? : webify-module-report? -> boolean?
;;   Check whether one report imports a public name owned by staged expander/read.
(define (module-report-imports-expander-read-alias? report)
  (define read-aliases (list->seteq webify-read-public-alias-symbols))
  (define import-map (webify-module-report-import-map report))
  (for*/or ([group (in-list (webify-module-import-map-unresolved-import-groups
                              import-map))]
            [binding (in-list group)])
    (match binding
      [(list _local (? symbol? external))
       (set-member? read-aliases external)]
      [_ #f])))

;; module-reports-import-expander-read-alias? : (listof webify-module-report?) -> boolean?
;;   Check whether any report needs staged expander/read public aliases.
(define (module-reports-import-expander-read-alias? reports)
  (for/or ([report (in-list reports)])
    (module-report-imports-expander-read-alias? report)))

;; webracket/base-external-provider-names : (listof webify-module-report?) -> (listof symbol?)
;;   List required providers outside local reports and the language root.
(define (webracket/base-external-provider-names reports)
  (define local-provider-names (map webify-module-report-name reports))
  (define (local-bridge-provider-name? name)
    (and (symbol? name)
         (string-prefix? (symbol->string name) "webracket/local/")))
  (define (local-provider-name? name)
    (and (symbol? name)
         (or (memq name local-provider-names)
             (local-bridge-provider-name? name))))
  (remove-duplicates
   (append
    (for/list ([report (in-list reports)]
               #:do [(define name (webify-module-report-name report))]
               #:when (and (not (local-provider-name? name))
                           (webify-provider-trial-module-allowed? name)
                           (not (webracket/base-language-provider-name? name))))
      name)
    (for*/list ([report (in-list reports)]
                [name (in-list (webify-module-report-required-provider-names report))]
                #:unless (or (local-bridge-provider-name? name)
                             (and (local-provider-name? name)
                                   ;; A file module can have the same name as
                                   ;; an external collection provider, such as
                                   ;; `fetch.rkt` requiring collection `fetch`.
                                   ;; The local module's own name must not hide
                                   ;; that external provider from the bridge.
                                   (not (eq? name (webify-module-report-name report))))
                             (webracket/base-language-provider-name? name)))
      name))
   eq?))

;; webify-module-report/rename : report symbol? -> report
;;   Register a substituted provider under the originally requested name.
(define (webify-module-report/rename report name)
  (if (eq? name (webify-module-report-name report))
      report
      (webify-module-report
       name
       (webify-module-report-directory-keys report)
       (webify-module-report-bundle-keys report)
       (webify-module-report-bundle-entries report)
       (webify-module-report-phase0-imports report)
       (webify-module-report-phase0-exports report)
       (webify-module-report-phase0-linklet-datum report)
       (webify-module-report-manifest-ready? report)
       (cons (format "substituted provider registered as ~a" name)
             (webify-module-report-notes report)))))

;; webracket/base-fetch-provider/timed : (symbol? -> webify-module-report?) -> symbol? -> webify-module-report?
;;   Wrap provider extraction with verbose timing diagnostics.
(define ((webracket/base-fetch-provider/timed fetch-provider) name)
  (define substitute-name (webracket/base-provider-substitute name))
  (define start-ms (now-ms))
  (provider-cache-log "#lang webracket/base provider fetch start: ~a via ~a"
                      name
                      substitute-name)
  (define report
    (webify-module-report/rename
     (cached-installed-provider-report substitute-name fetch-provider)
     name))
  (provider-cache-log "#lang webracket/base provider fetch done: ~a via ~a (~ams)"
                      name
                      substitute-name
                      (- (now-ms) start-ms))
  report)

;; webracket/base-local-collision-name : symbol? -> symbol?
;;   Build a private bridge name for a local module/provider-name collision.
(define (webracket/base-local-collision-name name)
  (string->symbol (format "webracket/local/~a" name)))

;; webracket/base-rename-self-colliding-local-reports : symbol? reports -> values
;;   Rename local modules that import an external provider with the same name.
(define (webracket/base-rename-self-colliding-local-reports root reports)
  (define self-colliding-names
    (for/list ([report (in-list reports)]
               #:do [(define name (webify-module-report-name report))]
               #:when (memq name (webify-module-report-required-provider-names report)))
      name))
  (define (renamed-name name)
    (if (memq name self-colliding-names)
        (webracket/base-local-collision-name name)
        name))
  (values (renamed-name root)
          (for/list ([report (in-list reports)])
            (define name (webify-module-report-name report))
            (if (memq name self-colliding-names)
                (webify-module-report/rename report (renamed-name name))
                report))))

;; local-looking-provider-name? : symbol? -> boolean?
;;   Recognize unsupported provider names that may be missed local paths.
(define (local-looking-provider-name? name)
  (and (symbol? name)
       (let ([text (symbol->string name)])
         (or (string-suffix? text ".rkt")
             (string-prefix? text "./")
             (string-prefix? text "../")
             (regexp-match? #rx"/" text)))))

;; webracket/base-check-external-provider-allowlist! : reports -> void?
;;   Keep the temporary bridge from silently growing unsupported providers.
(define (webracket/base-check-external-provider-allowlist! reports
                                                          #:local-source-paths
                                                          [local-source-paths '()])
  (define external-names (webracket/base-external-provider-names reports))
  (define unsupported
    (filter (lambda (name)
              (not (webify-provider-trial-module-allowed? name)))
            external-names))
  (define local-looking-unsupported
    (filter local-looking-provider-name? unsupported))
  (unless (null? unsupported)
    (cond
      [(pair? local-looking-unsupported)
       (raise-arguments-error
        'webracket/base-reports->module-graph
        "#lang webracket/base external provider is not in the Goal 1 trial allowlist"
        "unsupported providers" unsupported
        "local-looking unsupported providers" local-looking-unsupported
        "local module hint"
        "one or more unsupported providers look like relative file/module paths; this may indicate that a local source module was discovered but not matched to the local module graph name"
        "local report names" (map webify-module-report-name reports)
        "local source paths" local-source-paths
        "allowed providers" webify-provider-trial-module-allowlist)]
      [else
       (raise-arguments-error
        'webracket/base-reports->module-graph
        "#lang webracket/base external provider is not in the Goal 1 trial allowlist"
        "unsupported providers" unsupported
        "allowed providers" webify-provider-trial-module-allowlist)])))

;; webracket-owned-ffi-provider-name? : symbol? -> boolean?
;;   Recognize WebRacket provider modules that do not need contract erasure.
(define webracket-owned-ffi-provider-exacts
  '(web-easy
    audio
    clipboard
    compression-streams
    fetch
    fullscreen
    gamepad
    geolocation
    history
    indexed-db
    intersection-observer
    jsxgraph
    notifications
    resize-observer
    screen-orientation
    storage
    web-speech
    web-workers
    websocket
    web-pict
    rquery
    webracket/full
    webracket/full/main
    webracket/private/decimal-string
    webracket/private/ffi
    webracket/private/kw
    webracket/private/sort-public
    webracket/private/sort))

(define webracket-owned-ffi-provider-prefixes
  '("js/"
    "browser/"
    "web-easy/"
    "web-pict/"
    "rquery/"
    "webracket/unsafe/"))

(define (webracket-owned-ffi-provider-name? name)
  (cond
    [(not (symbol? name)) #f]
    [(memq name webracket-owned-ffi-provider-exacts) #t]
    [else
     (define name-string (symbol->string name))
     (for/or ([prefix (in-list webracket-owned-ffi-provider-prefixes)])
       (string-prefix? name-string prefix))]))

;; webracket/base-reports->module-graph : symbol? reports keyword-args -> graph
;;   Build a bridge graph from already-extracted reports.
(define (webracket/base-reports->module-graph root
                                             reports
                                             #:local-source-paths [local-source-paths '()]
                                             #:fetch-provider
                                             [fetch-provider webify-module-path->report])
  (define-values (bridge-root bridge-reports)
    (with-module-bridge-timing
     'module-graph-rename-local-reports
     (lambda ()
       (webracket/base-rename-self-colliding-local-reports root reports))))
  (define local-report-names
    (with-module-bridge-timing
     'module-graph-local-report-names
     (lambda ()
       (map webify-module-report-name bridge-reports))))
  (define support-report-names '())
  (define needs-expander-read-support?
    (with-module-bridge-timing
     'module-graph-read-support-demand
     (lambda ()
       (module-reports-import-expander-read-alias? bridge-reports))))
  (current-module-bridge-needs-expander-read-provider?
   needs-expander-read-support?)
  (define needs-io-format-support?
    (with-module-bridge-timing
     'module-graph-io-format-support-demand
     (lambda ()
       (for/or ([name (in-list webify-io-format-export-symbols)])
         (set-member? (current-provider-owned-primitive-aliases) name)))))
  (define needs-io-print-support?
    (with-module-bridge-timing
     'module-graph-io-print-support-demand
     (lambda ()
       (and
        (for/or ([name (in-list io-print-provider-owned-alias-symbols)])
          (set-member? (current-provider-owned-primitive-aliases) name))
        (for*/or ([report (in-list bridge-reports)]
                  [name (in-list io-print-provider-owned-alias-symbols)])
          (module-bridge-datum-contains-unquoted-symbol?
           (webify-module-report-phase0-linklet-datum report)
           name))))))
  (define registry
    (cond
     [(or (webracket/base-reports-demand-external-providers? bridge-reports)
          needs-expander-read-support?
          needs-io-print-support?)
       (with-module-bridge-timing
        'module-graph-check-provider-allowlist
        (lambda ()
          (webracket/base-check-external-provider-allowlist!
           bridge-reports
           #:local-source-paths local-source-paths)))
       (define external-names
         (with-module-bridge-timing
          'module-graph-external-provider-names
          (lambda ()
            (webracket/base-external-provider-names bridge-reports))))
       (define provider-namespace-box (box #f))
       (define provider-module-name-resolver-box (box #f))
       (define (provider-namespace/ref)
         (or (unbox provider-namespace-box)
             (let ([namespace
                    (with-module-bridge-timing
                     'module-graph-provider-namespace
                     (lambda ()
                       (shared-webify-contract-erasing-namespace/ref)))])
               (set-box! provider-namespace-box namespace)
               namespace)))
       (define (provider-module-name-resolver/ref)
         (or (unbox provider-module-name-resolver-box)
             (let ([module-name-resolver
                    (with-module-bridge-timing
                     'module-graph-provider-resolver
                     (lambda ()
                       (shared-webify-contract-erasing-module-name-resolver/ref)))])
               (set-box! provider-module-name-resolver-box module-name-resolver)
               module-name-resolver)))
       (define (fetch-provider/erased-contracts name)
         (define staged-io-format-report
           (io-format-provider-report/ref name))
         (define staged-io-print-report
           (io-print-provider-report/ref name))
         (define staged-read-report
           (read-provider-report/ref name))
         (define substitute-name (webracket/base-provider-substitute name))
         (define extraction-name
           (cond
             [(eq? name 'racket/in-out)
              'webracket/private/in-out]
             [(memq name '(racket/module-boundary-ctc
                           racket/contract/private/module-boundary-ctc))
              'webracket/private/module-boundary-ctc]
             [(webracket/base-erased-contract-provider-name? name)
              'webracket/contract]
             [else
              substitute-name]))
         (define webracket-owned-provider?
           (and (symbol? extraction-name)
                (string-prefix? (symbol->string extraction-name) "webracket/")))
         (define normal-export-shape?
           ;; The root module is expanded by normal Racket, so contracted
           ;; public providers must keep their generated provide/contract
           ;; export names while their contract implementation dependencies
           ;; remain eligible for erased-provider routing.
           (or (memq name '(racket/dict
                            racket/math
                            racket/private/for
                            racket/private/map
                            racket/private/define-struct
                            racket/private/kernstruct
                            racket/private/class-internal
                            racket/private/class-struct
                            racket/private/class-wrapped
                            racket/private/stx
                            syntax/context
                            syntax/intdef
                            syntax/kerncase
                            syntax/name
                            syntax/parse/private/lib
                            syntax/parse/private/residual
                            syntax/parse/private/residual/parse-util
                            syntax/parse/private/residual/state
                            syntax/private/id-table
                            syntax/stx
                            racket/unit
                            racket/private/unit/contract
                            racket/private/unit/contract-syntax
                            racket/private/unit/keywords
                            racket/private/unit/runtime
                            racket/private/unit/signature
                            racket/private/unit/unit-core
                            racket/private/unit/unit-infer
                            racket/private/unit/util
                            racket/private/unit/exptime/import-export
                            racket/private/unit/exptime/signature
                            racket/private/unit/exptime/syntax
                            racket/private/unit/exptime/unit-infer
                            racket/private/unit/exptime/util
                            contract
                            contract-syntax
                            exptime/import-export
                            exptime/signature
                            exptime/syntax
                            exptime/unit-infer
                            exptime/util
                            keywords
                            runtime
                            signature
                            unit-core
                            unit-infer
                            util
                            racket/private/serialize-structs))
               (webracket-owned-ffi-provider-name? name)))
         (cond
           [staged-io-format-report
            staged-io-format-report]
           [staged-io-print-report
            staged-io-print-report]
           [staged-read-report
            staged-read-report]
           [else
            (define (extract)
              ;; Provider-owned primitive aliases are a user-module boundary
              ;; routing rule. Keep installed/support provider extraction in its
              ;; ordinary world so private support modules do not grow spurious
              ;; facade dependencies like `expander/read`.
              (parameterize ([current-provider-owned-primitive-aliases (seteq)])
                (webify-module-report/rename
                 (if (eq? fetch-provider webify-module-path->report)
                     (if (or normal-export-shape?
                             webracket-owned-provider?)
                         (webify-module-path->report extraction-name)
                         (webify-module-path->report extraction-name
                                                     #:namespace (provider-namespace/ref)))
                     (fetch-provider extraction-name))
                 name)))
            (if (or normal-export-shape?
                    webracket-owned-provider?)
                (extract)
                (parameterize ([current-namespace (provider-namespace/ref)]
                               [current-module-name-resolver (provider-module-name-resolver/ref)])
                  (extract)))]))
       (define provider-cache
         (make-webify-module-provider-cache
          (webracket/base-fetch-provider/timed fetch-provider/erased-contracts)))
       (define erased-contract-support-names
         '(racket/contract/base
           racket/contract/combinator
           racket/contract/region
           racket/in-out
           racket/module-boundary-ctc
           racket/contract/private/base
           racket/contract/private/blame
           racket/contract/private/guts
           racket/contract/private/object
           racket/contract/private/object-c-wrapper))
       (define (erased-contract-support-reports)
         (let ([report (webify-module-provider-cache-ref provider-cache
                                                         'webracket/contract)])
           (for/list ([name (in-list erased-contract-support-names)])
             (webify-module-report/rename report name))))
       (define needs-class-support?
         ;; `webracket/full` re-exports the installed `racket/class` surface, so
         ;; its graph needs the same seeded private-provider support as an
         ;; explicit `(require racket/class)` program. The same support bundle
         ;; also carries the private struct helpers that expanded local modules
         ;; can surface as phase-0 provider candidates.
         (with-module-bridge-timing
          'module-graph-class-support-demand
          (lambda ()
            (ormap (lambda (name)
                     (memq name '(racket/class
                                  racket/private/kernstruct
                                  webracket/full
                                  webracket/full/main)))
                   external-names))))
       (define have-racket/private/pico?
         (with-module-bridge-timing
          'module-graph-pico-support-probe
          (lambda ()
            (module-path-available? 'racket/private/pico))))
       (define have-racket/private/reverse?
         (with-module-bridge-timing
          'module-graph-reverse-support-probe
          (lambda ()
            (module-path-available? 'racket/private/reverse))))
       (define have-racket/private/class-struct?
         (with-module-bridge-timing
          'module-graph-class-struct-support-probe
          (lambda ()
            (module-path-available? 'racket/private/class-struct))))
       (define have-racket/private/class-wrapped?
         (with-module-bridge-timing
          'module-graph-class-wrapped-support-probe
          (lambda ()
            (module-path-available? 'racket/private/class-wrapped))))
       (define needs-unit-support?
         (with-module-bridge-timing
          'module-graph-unit-support-demand
          (lambda ()
            (ormap (lambda (name)
                     (memq name '(racket/unit
                                  racket/private/unit/contract
                                  racket/private/unit/runtime
                                  racket/private/unit/signature
                                  racket/private/unit/unit-core
                                  racket/private/unit/unit-infer)))
                   external-names))))
       (define unit-relative-provider-aliases
         '((contract . racket/private/unit/contract)
           (contract-syntax . racket/private/unit/contract-syntax)
           (exptime/import-export . racket/private/unit/exptime/import-export)
           (exptime/signature . racket/private/unit/exptime/signature)
           (exptime/syntax . racket/private/unit/exptime/syntax)
           (exptime/unit-infer . racket/private/unit/exptime/unit-infer)
           (exptime/util . racket/private/unit/exptime/util)
           (keywords . racket/private/unit/keywords)
           (runtime . racket/private/unit/runtime)
           (signature . racket/private/unit/signature)
           (unit-core . racket/private/unit/unit-core)
           (unit-infer . racket/private/unit/unit-infer)
           (util . racket/private/unit/util)))
       (define erased-contract-support-report-names
         (cons 'webracket/contract erased-contract-support-names))
       (define eager-base-support-report-names
         (list webify-module-protocol-name))
       (define lazy-base-support-report-names
         '(webracket/contract
           webracket/private/decimal-string
           webracket/private/sort-public
           webracket/private/kw
           racket/private/pre-base
           webracket/private/sort
           racket/private/sort
           webracket/unsafe/undefined
           racket/unsafe/undefined))
       (define class-support-report-names
         (append
          '(racket/private/for
            racket/private/map
            webracket/list
            syntax/location
            racket/private/define-struct
            racket/private/kernstruct
            racket/private/serialize-structs
            racket/private/class-internal
            racket/private/list)
          (if have-racket/private/class-struct?
              '(racket/private/class-struct)
              '())
          (if have-racket/private/class-wrapped?
              '(racket/private/class-wrapped)
              '())
          (if have-racket/private/pico?
              '(racket/private/pico)
              (if have-racket/private/reverse?
                  '(racket/private/reverse)
                  '()))))
       (define unit-support-report-names
         (append
          '(racket/private/unit/contract
            racket/private/unit/contract-syntax
            racket/private/unit/keywords
            racket/private/unit/runtime
            racket/private/unit/signature
            racket/private/unit/unit-core
            racket/private/unit/unit-infer
            racket/private/unit/util
            racket/promise
            racket/private/unit/exptime/import-export
            racket/private/unit/exptime/signature
            racket/private/unit/exptime/syntax
            racket/private/unit/exptime/unit-infer
            racket/private/unit/exptime/util)
          (map car unit-relative-provider-aliases)
          (map cdr unit-relative-provider-aliases)
          '(syntax/parse/private/residual/parse-util
            syntax/parse/private/residual/state
            syntax/parse/private/rep-patterns/simple)))
       (define ffi-support-report-names
         (for/list ([name (in-list external-names)]
                    #:when (webracket-owned-ffi-provider-name? name))
           name))
       (define needs-io-format-closure?
         (with-module-bridge-timing
          'module-graph-io-format-closure-demand
          (lambda ()
            (or needs-io-format-support?
                (member 'webracket/private/format external-names)
                (member 'racket/format external-names)
                (member webify-io-format-root-name external-names)
                (member 'racket-src/io/format/main-web external-names)
                (member webify-io-format-printf-name external-names)
                (member 'racket-src/io/format/printf-web external-names)
                (member webify-io-format-adapter-name external-names)
                (member 'racket-src/io/format/printf-adapter external-names)
                (member 'racket-src/racket/format-web external-names)))))
       (define needs-io-print-closure?
         (with-module-bridge-timing
          'module-graph-io-print-closure-demand
          (lambda ()
            (or needs-io-print-support?
                (member webify-io-print-root-name external-names)
                (member 'racket-src/io/print/main-web external-names)
                (member webify-io-print-adapter-name external-names)
                (member 'racket-src/io/format/printf-adapter external-names)
                (member 'io/print external-names)))))
       (define expander-read-support-report-names
         (with-module-bridge-timing
          'module-graph-expander-read-support-report-names
          (lambda ()
            (if needs-expander-read-support?
                (append
                 (read-provider-report-names)
                 ;; The private staged reader closure reaches racket/prefab through
                 ;; racket-src/expander/common/prefab.
                 '(racket/prefab))
                '()))))
       (define io-format-support-report-names
         (with-module-bridge-timing
          'module-graph-io-format-support-report-names
          (lambda ()
            (if needs-io-format-closure?
                (list webify-io-format-adapter-name
                      'racket-src/io/format/printf-adapter
                      webify-io-format-printf-name
                      'racket-src/io/format/printf-web
                      webify-io-format-root-name
                      'racket-src/io/format/main-web
                      webify-io-format-map-shim-name
                      'racket-src/racket/format-web
                      'racket/format)
                '()))))
       (define io-print-support-report-names
         (with-module-bridge-timing
          'module-graph-io-print-support-report-names
          (lambda ()
            (if needs-io-print-closure?
                (list webify-io-print-adapter-name
                      'racket-src/io/format/printf-adapter
                      webify-io-print-root-name
                      'racket-src/io/print/main-web
                      'io/print)
                '()))))
       (set! support-report-names
             (with-module-bridge-timing
              'module-graph-support-report-name-assembly
              (lambda ()
                (append
                 eager-base-support-report-names
                 lazy-base-support-report-names
                 (if needs-class-support?
                     class-support-report-names
                     '())
                 (if needs-unit-support?
                     unit-support-report-names
                     '())
                 ffi-support-report-names
                 expander-read-support-report-names
                 io-format-support-report-names
                 io-print-support-report-names))))
       (define (unit-support-reports)
         (append
          (for/list ([name (in-list '(racket/private/unit/contract
                                      racket/private/unit/contract-syntax
                                      racket/private/unit/keywords
                                      racket/private/unit/runtime
                                      racket/private/unit/signature
                                      racket/private/unit/unit-core
                                      racket/private/unit/unit-infer
                                      racket/private/unit/util
                                      racket/promise
                                      racket/private/unit/exptime/import-export
                                      racket/private/unit/exptime/signature
                                      racket/private/unit/exptime/syntax
                                      racket/private/unit/exptime/unit-infer
                                      racket/private/unit/exptime/util))])
            (webify-module-provider-cache-ref provider-cache name))
          (for/list ([alias (in-list unit-relative-provider-aliases)])
            (webify-module-report/rename
             (webify-module-provider-cache-ref provider-cache (cdr alias))
             (car alias)))
          (list
           (webify-module-report/rename
            (webify-module-path->report '(submod syntax/parse/private/residual parse-util))
            'syntax/parse/private/residual/parse-util)
           (webify-module-report/rename
           (webify-module-path->report '(submod syntax/parse/private/residual state))
           'syntax/parse/private/residual/state)
           (webify-module-report/rename
            (webify-module-path->report '(submod syntax/parse/private/rep-patterns simple))
            'syntax/parse/private/rep-patterns/simple))))
       (define registry
         (parameterize ([current-module-bridge-local-report-names
                         (list->seteq local-report-names)]
                        [current-module-bridge-support-provider-names
                         (list->seteq support-report-names)])
           (cached-discovered-provider-registry
            root
            local-source-paths
            support-report-names
            #t
            webify-module-protocol-dependency
            (lambda ()
              ;; Some resolver hints target WebRacket-owned support modules
              ;; directly; only materialize those reports when the discovered
              ;; provider graph cache itself misses.
              (define support-reports
                (append
                 (list webify-module-protocol-report
                       (webify-module-provider-cache-ref provider-cache
                                                         'webracket/private/sort-public)
                       (webify-module-provider-cache-ref provider-cache
                                                         'webracket/unsafe/undefined)
                       (webify-module-report/rename
                        (webify-module-provider-cache-ref provider-cache
                                                          'webracket/unsafe/undefined)
                        'racket/unsafe/undefined))
                 (if needs-class-support?
                     (append
                      (list (webify-module-provider-cache-ref provider-cache
                                                              'racket/private/for)
                            (webify-module-report/rename
                             (webify-module-provider-cache-ref provider-cache
                                                               'webracket/list)
                             'racket/private/list)
                            (webify-module-provider-cache-ref provider-cache
                                                              'racket/private/map))
                      (if have-racket/private/pico?
                          (list (webify-module-provider-cache-ref provider-cache
                                                                  'racket/private/pico))
                          (if have-racket/private/reverse?
                              (list (webify-module-provider-cache-ref provider-cache
                                                                      'racket/private/reverse))
                              '()))
                      (list (webify-module-provider-cache-ref provider-cache
                                                              'syntax/location)
                            (webify-module-provider-cache-ref provider-cache
                                                              'racket/private/define-struct)
                            (webify-module-provider-cache-ref provider-cache
                                                              'racket/private/kernstruct)
                            (webify-module-provider-cache-ref provider-cache
                                                              'racket/private/serialize-structs)
                            (webify-module-provider-cache-ref provider-cache
                                                              'racket/private/class-internal))
                      (if have-racket/private/class-struct?
                          (list (webify-module-provider-cache-ref provider-cache
                                                                  'racket/private/class-struct))
                          '())
                      (if have-racket/private/class-wrapped?
                          (list (webify-module-provider-cache-ref provider-cache
                                                                  'racket/private/class-wrapped))
                          '()))
                     '())
                 (if needs-unit-support?
                     (unit-support-reports)
                     '())
                 ;; WebRacket-owned browser/library providers are hand-authored
                 ;; modules with exported procedures that often close over private
                 ;; helpers. Until provider slicing can prove and retain those private
                 ;; closure dependencies, keep these providers unsliced.
                 (for/list ([name (in-list external-names)]
                            #:when (webracket-owned-ffi-provider-name? name))
                   (webify-module-provider-cache-ref provider-cache name))
                 (if needs-expander-read-support?
                     (append
                      (read-provider-reports)
                      ;; The private staged reader closure reaches racket/prefab
                      ;; through racket-src/expander/common/prefab.
                      (list (webify-module-provider-cache-ref provider-cache
                                                              'racket/prefab)))
                     '())
                 (if needs-io-format-closure?
                     (io-format-provider-reports)
                     '())
                 (if needs-io-print-closure?
                     (io-print-provider-reports)
                     '())))
              (with-module-bridge-timing
               'discover-provider-graph
               (lambda ()
                 (webify-module-reports->discovered-resolved-linklet-inputs
                  bridge-reports
                  #:extra-reports support-reports
                  #:fetch-provider (lambda (name)
                                     (unless (webify-provider-trial-module-allowed? name)
                                       (raise-arguments-error
                                        'webracket/base-reports->module-graph
                                        "#lang webracket/base transitive provider is not in the Goal 1 trial allowlist"
                                        "unsupported provider" name
                                        "allowed providers" webify-provider-trial-module-allowlist))
                                     (webify-module-provider-cache-ref provider-cache name))
                  #:skip-provider? webracket/base-language-provider-name?
                  #:module-protocol-dependency webify-module-protocol-dependency
                  #:demand-slice? #t)))))))
       (provider-cache-log "#lang webracket/base provider cache: ~a hit(s), ~a miss(es)"
                           (webify-module-provider-cache-hit-count provider-cache)
                           (webify-module-provider-cache-miss-count provider-cache))
       registry]
      [else
       (set! support-report-names (list webify-module-protocol-name))
       (with-module-bridge-timing
        'final-extracted-form-assembly
        (lambda ()
          (parameterize ([current-module-bridge-local-report-names
                          (list->seteq local-report-names)]
                         [current-module-bridge-support-provider-names
                          (list->seteq support-report-names)])
            (webify-module-linklet-inputs->registry
             (cons webify-module-protocol-input
                   (webify-module-reports->resolved-linklet-inputs
                    bridge-reports
                    #:module-protocol-dependency webify-module-protocol-dependency))))))]))
  (define module-kinds
    (with-module-bridge-timing
     'module-graph-module-kinds
     (lambda ()
       (for/hasheq ([name (in-list (webify-module-registry-names registry))])
         (values name
                 (cond
                   [(memq name local-report-names) 'local-user]
                   [(memq name support-report-names) 'support-provider]
                   [else 'installed-provider]))))))
  (webracket/base-local-module-graph bridge-root bridge-reports registry module-kinds))

;; webracket/base-file->local-module-graph+sources :
;;   path-string? -> (values webracket/base-local-module-graph? (listof path?))
;;   Build a local root graph plus the source paths that validate its caches.
(define (webracket/base-file->local-module-graph+sources filename)
  (define resolved (simplify-path (path->complete-path filename)))
  (define cache-path (local-module-graph-cache-path resolved))
  (define cached
    (with-module-bridge-timing
     'local-module-graph-cache-lookup
     (lambda ()
       (read-local-module-graph-cache cache-path))))
  (define-values (root reports local-source-paths)
    (cond
      [cached
       (provider-cache-log "#lang webracket/base local module graph hit: ~a" resolved)
       (with-module-bridge-timing
        'local-module-graph-cache-hit
        (lambda ()
          (values (vector-ref cached 0)
                  (vector-ref cached 1)
                  (vector-ref cached 2))))]
      [else
       (provider-cache-log "#lang webracket/base local module graph miss: ~a" resolved)
       (call-with-provider-cache-lock
        cache-path
        (lambda ()
          (define cached*
            (with-module-bridge-timing
             'local-module-graph-cache-lookup
             (lambda ()
               (read-local-module-graph-cache cache-path))))
          (cond
            [cached*
             (provider-cache-log "#lang webracket/base local module graph hit: ~a" resolved)
             (with-module-bridge-timing
              'local-module-graph-cache-hit
              (lambda ()
                (values (vector-ref cached* 0)
                        (vector-ref cached* 1)
                        (vector-ref cached* 2))))]
            [else
             (define-values (root reports local-source-paths)
               (with-module-bridge-timing
                'local-module-graph-cache-miss
                (lambda ()
                  (webracket/base-file->local-module-reports resolved))))
             (write-local-module-graph-cache! cache-path
                                              root
                                              reports
                                              local-source-paths)
             (values root reports local-source-paths)])))]))
  (define graph
    (with-module-bridge-timing
     'local-module-graph-reports->graph
     (lambda ()
       (webracket/base-reports->module-graph
        root
        reports
        #:local-source-paths local-source-paths))))
  (values graph local-source-paths))

;; webracket/base-file->local-module-graph : path-string? -> webracket/base-local-module-graph?
;;   Build a local root graph plus demanded provider reports for the module bridge.
(define (webracket/base-file->local-module-graph filename)
  (define-values (graph _local-source-paths)
    (webracket/base-file->local-module-graph+sources filename))
  graph)

;; module-bridge-root-export-bindings : webracket/base-local-module-graph? webify-module-composition? -> (listof hash?)
;;   Collect authoritative public export bindings for the root user module.
(define (module-bridge-root-export-bindings graph composition)
  (define who 'module-bridge-root-export-bindings)
  (define root-name
    (webracket/base-local-module-graph-root graph))
  (define root-instance
    (hash-ref (webify-module-composition-instances composition)
              root-name
              (lambda ()
                (raise-arguments-error who
                                       "composition is missing the root module instance"
                                       "root" root-name
                                       "available instances"
                                       (hash-keys
                                        (webify-module-composition-instances composition))))))
  (for/list ([export-name (in-list (webify-module-instance-exports root-instance))])
    (define entry
      (webify-module-instance-export-entry root-instance export-name))
    (define meta
      (bridge-export-entry-metadata entry))
    (hasheq 'name export-name
            'binding-kind (bridge-export-entry-kind entry)
            'binding-id (bridge-export-entry-binding entry)
            'mutable? (and (hash-ref meta 'mutated? #f) #t))))

;; module-file->module-bridge-top-level : path-string? -> syntax?
;;   Compose a local #lang webracket/base graph through module instances.
;;   `module-bridge-extract` covers this exact path:
;;   `module-file->module-bridge-top-level`
;;    -> `webracket/base-file->local-module-graph`
;;    -> `webracket/base-file->local-module-reports`
;;    -> `compile-webracket/base-module-file-report`
;;    -> `webracket/base-reports->module-graph`.
(define (module-file->module-bridge-top-level filename)
  (define resolved-filename
    (path->string (simplify-path (path->complete-path filename))))
  (define cache-path
    (module-bridge-result-cache-path resolved-filename))
  (define (read-cache)
    (with-module-bridge-timing
     'module-bridge-result-cache-lookup
     (lambda ()
       (read-module-bridge-result-cache cache-path))))
  (define (cache-hit->syntax cached)
    (provider-cache-log "#lang webracket/base module bridge result hit: ~a"
                        resolved-filename)
    (with-module-bridge-timing
     'module-bridge-result-cache-hit
     (lambda ()
       (module-bridge-result-cache-payload->syntax cached))))
  (define (compose-and-cache)
    (provider-cache-log "#lang webracket/base module bridge result miss: ~a"
                        resolved-filename)
    (define-values (graph local-source-paths)
      (with-driver-timing
       'module-bridge-extract
       (lambda ()
         (with-handlers ([exn:fail?
                          (lambda (e)
                            (raise-arguments-error
                             'read-top-level-from-file
                             "#lang webracket/base module bridge graph extraction failed"
                             "file" filename
                             "message" (exn-message e)))])
           (webracket/base-file->local-module-graph+sources filename)))))
    (define provider-names
      (module-reports->provider-names
       (webracket/base-local-module-graph-reports graph)))
    (define module-kinds
      (webracket/base-local-module-graph-module-kinds graph))
    (define elidable-module-names
      (bridge-module-elidable-public-modules graph))
    (current-source-module-provider-names provider-names)
    (current-module-bridge-module-kinds module-kinds)
    (current-module-bridge-elidable-module-names elidable-module-names)
    (current-module-bridge-local-source-paths local-source-paths)
    (define composition
      (with-driver-timing
       'module-bridge-compose
       (lambda ()
         (with-handlers ([exn:fail?
                          (lambda (e)
                            (raise-arguments-error
                             'read-top-level-from-file
                             "#lang webracket/base module bridge composition failed"
                             "root" (webracket/base-local-module-graph-root graph)
                             "message" (exn-message e)))])
           (parameterize ([current-composer-primitive-import-cell-overrides
                           (module-composer-primitive-overrides)])
             (webify-compose-module-registry/roots/composition
              (webracket/base-local-module-graph-registry graph)
              (list (webracket/base-local-module-graph-root graph))
              #:define-export-aliases? #f
              #:serializable?-box     #f
              #:datum-intern?         #f
              #:target                #f
              #:allow-set!-undefined? #f
              #:unsafe-mode?          #f
              #:enforce-constant?     #t
              #:allow-inline?         #t
              #:no-prompt?            #f
              #:prim-knowns           (hasheq)
              #:primitives            (hasheq)
              #:compiler-query        (lambda (v) #f)
              #:get-import-knowns     (lambda (key) (values #f #f #f))
              #:import-keys           #f))))))
    (define root-export-bindings
      (module-bridge-root-export-bindings graph composition))
    (current-console-bridge-root-export-bindings root-export-bindings)
    (current-console-bridge-root-source-path resolved-filename)
    (define metric-rows
      (module-bridge-graph-metric-rows graph composition))
    (define fragment-manifest
      (hash-ref (current-webify-bridge-stats)
                'module-bridge-fragment-manifest
                '()))
    (hash-set! (current-webify-bridge-stats)
               'module-bridge-metric-rows
               metric-rows)
    (define bridge-datum
      (webify-module-composition-datum composition))
    (define needs-expander-read-provider?
      (current-module-bridge-needs-expander-read-provider?))
    (define needs-regexp-provider?
      (module-bridge-datum-needs-regexp-provider? bridge-datum))
    (current-module-bridge-needs-regexp-provider? needs-regexp-provider?)
    (define lowered-datum
      (webify-module-composition-with-tail composition '((void))))
    (define provider-fragment-split
      (module-bridge-split-lowered-datum lowered-datum fragment-manifest))
    (define provider-fragment-fingerprint
      (module-bridge-provider-fragment-fingerprint provider-fragment-split))
    (define provider-fragment-cache-status
      (ensure-module-bridge-provider-fragment-cache! provider-fragment-fingerprint
                                                     provider-fragment-split))
    (hash-set! (current-webify-bridge-stats)
               'module-bridge-provider-fragment-cache-status
               provider-fragment-cache-status)
    (write-module-bridge-result-cache! cache-path
                                       local-source-paths
                                       provider-names
                                       module-kinds
                                       elidable-module-names
                                       root-export-bindings
                                       resolved-filename
                                       metric-rows
                                       fragment-manifest
                                       provider-fragment-fingerprint
                                       needs-expander-read-provider?
                                       needs-regexp-provider?
                                       lowered-datum)
    (datum->syntax #f lowered-datum))
  (define cached-before-lock
    (read-cache))
  (if cached-before-lock
      (cache-hit->syntax cached-before-lock)
      (call-with-provider-cache-lock
       cache-path
       (lambda ()
         (define cached-after-lock
           (read-cache))
         (if cached-after-lock
             (cache-hit->syntax cached-after-lock)
             (compose-and-cache))))))

;; module-file->module-bridge-top-level/local-only : path-string? -> (or/c syntax? #f)
;;   Use the default bridge only for local or explicitly staged provider graphs.
(define (module-file->module-bridge-top-level/local-only filename)
  (define resolved-filename
    (path->string (simplify-path (path->complete-path filename))))
  (define-values (root reports _local-source-paths)
    (with-driver-timing
     'module-bridge-extract
     (lambda ()
       (webracket/base-file->local-module-reports filename))))
  (define external-names (webracket/base-external-provider-names reports))
  (current-console-bridge-root-export-bindings #f)
  (current-console-bridge-root-source-path #f)
  (current-module-bridge-elidable-module-names #f)
  (define (compose)
    (let ([graph
           (with-driver-timing
            'module-bridge-register
            (lambda ()
              (webracket/base-reports->module-graph root reports)))])
      (current-module-bridge-module-kinds
       (webracket/base-local-module-graph-module-kinds graph))
      (current-module-bridge-elidable-module-names
       (bridge-module-elidable-public-modules graph))
      (define composition
        (with-driver-timing
         'module-bridge-compose
         (lambda ()
           (parameterize ([current-composer-primitive-import-cell-overrides
                           (module-composer-primitive-overrides)])
             (webify-compose-module-registry/roots/composition
              (webracket/base-local-module-graph-registry graph)
              (list (webracket/base-local-module-graph-root graph))
              #:define-export-aliases? #f
              #:serializable?-box     #f
              #:datum-intern?         #f
              #:target                #f
              #:allow-set!-undefined? #f
              #:unsafe-mode?          #f
              #:enforce-constant?     #t
              #:allow-inline?         #t
              #:no-prompt?            #f
              #:prim-knowns           (hasheq)
              #:primitives            (hasheq)
              #:compiler-query        (lambda (v) #f)
              #:get-import-knowns     (lambda (key) (values #f #f #f))
              #:import-keys           #f)))))
      (current-console-bridge-root-export-bindings
       (module-bridge-root-export-bindings graph composition))
      (current-console-bridge-root-source-path resolved-filename)
      (hash-set! (current-webify-bridge-stats)
                 'module-bridge-metric-rows
                 (module-bridge-graph-metric-rows graph composition))
      (datum->syntax
       #f
       (webify-module-composition-with-tail composition '((void))))))
  (and (andmap webify-provider-trial-default-bridge-module-allowed?
               external-names)
       (cond
         [(null? external-names)
          (with-handlers ([exn:fail? (lambda (_exn) #f)])
            (compose))]
         [else
          (compose)])))

;; module-form->top-level : syntax? -> syntax?
;;   Transitional helper for tests that already have a module syntax object.
(define (module-form->top-level stx)
  (define-values (module-name body) (module-stx->name+body stx))
  (define exports (append-map simple-provide-exports body))
  (define top-level-forms
    (filter (lambda (form)
              (not (and (pair? form)
                        (memq (car form) '(provide module+ require)))))
            body))
  (define export-forms
    (for/list ([export (in-list exports)])
      (define cell (module-export-cell-symbol module-name export))
      `(begin
         (define ,cell (module-variable-make ',export))
         (module-variable-define! ,cell ,export 'constant))))
  (datum->syntax stx `(begin ,@top-level-forms ,@export-forms) stx))

(define (read-top-level-from-file filename
                                  #:webracket-module-bridge?
                                  [webracket-module-bridge? #f])
  (current-read-source-timing-rows '())
  (current-module-bridge-detail-rows '())
  (current-webify-bridge-stats (make-hash))
  (current-source-module-language #f)
  (define first-line
    (call-with-input-file filename
      (λ (port)
        (read-line port 'any))))
  (define starts-with-webracket-lang?
    (webracket-script-lang-line? first-line))
  (define starts-with-webracket/base-lang?
    (webracket-module-lang-line? first-line))
  (define (read-forms port)
    (let loop ([forms '()])
      (define stx (read-syntax filename port))
      (if (eof-object? stx)
          (reverse forms)
          (loop (cons stx forms)))))
  (call-with-input-file filename
    (λ (port)
      (port-count-lines! port)
      (with-module-reading-parameterization
        (λ ()
          ;; `webracket.rkt` compiles script-style top-level programs. A source
          ;; that starts with `#lang webracket` uses the same body language, so
          ;; skip the language line here instead of asking Racket to resolve the
          ;; checkout as an installed `webracket/lang/reader` collection.
          (cond
            [starts-with-webracket-lang?
             (current-source-module-language #f)
             (void (read-line port 'any))
             (define forms (read-forms port))
             (datum->syntax #f `(begin ,@forms))]
            [starts-with-webracket/base-lang?
             (current-source-module-language
              (webracket-module-lang-line->language first-line))
             (with-webracket-sequence-prelude
              (module-file->module-bridge-top-level filename))]
            [else
             (current-source-module-language #f)
             (define forms (read-forms port))
             (datum->syntax #f `(begin ,@forms))]))))))

;;;
;;; WRITE WAT TO FILE
;;;

(define (write-wat-to-file out-filename wat #:pretty? [pretty? #t])
  (with-output-to-file out-filename
    (λ ()
      (displayln ";; This file was generated by `webracket`.")
      (if pretty?
          (let ()
            (define-values (in out)
              (make-pipe))
            (define result-channel
              (make-channel))
            (thread
             (lambda ()
               (define result
                 (with-handlers ([exn:fail? (lambda (exn) exn)])
                   (pretty-write wat out)
                   #t))
               (with-handlers ([exn:fail? void])
                 (close-output-port out))
               (channel-put result-channel result)))
            (rewrite-wat-identifiers-from-port in (current-output-port))
            (close-input-port in)
            (define result
              (channel-get result-channel))
            (when (exn:fail? result)
              (raise result)))
          (write-compact-wat wat (current-output-port))))
    #:exists 'replace))

;;;
;;; PREFLIGHT CHECK TOOLS
;;;

(define (ensure-source-file! who filename)
  (cond
    [(directory-exists? filename)
     (error who (~a "expected a source file, got directory: " filename))]
    [(not (file-exists? filename))
     (error who (~a "file not found: " filename))]))

(define (ensure-generated-file-exists! who path kind)
  (unless (file-exists? path)
    (error who (~a kind " was not created: " path))))

(define (ensure-output-path-writable! who output-path kind)
  (when (directory-exists? output-path)
    (error who
           (~a "cannot write " kind ": " output-path
               "\n" "target path is an existing directory")))
  (define dir (or (path-only output-path) (current-directory)))
  (with-handlers ([exn:fail:filesystem?
                   (λ (e)
                     (error who
                            (~a "cannot prepare directory for " kind ": " dir
                                "\n" (exn-message e))))])
    (make-directory* dir))
  (define probe #f)
  (with-handlers ([exn:fail:filesystem?
                   (λ (e)
                     (when (and probe (file-exists? probe))
                       (with-handlers ([exn:fail:filesystem? void])
                         (delete-file probe)))
                     (error who
                            (~a "cannot write " kind ": " output-path
                                "\n" (exn-message e))))])
    (set! probe (make-temporary-file "webracket-write-check~a.tmp" #f dir))
    (delete-file probe)))

;; check-node! : symbol? [#:exec-path-from-shell? boolean?] -> path?
;;   Find a compatible Node.js executable using the shared lookup policy.
(define (check-node! who #:exec-path-from-shell? [exec-path-from-shell? #t])
  (define node-path
    (node-executable #:who who
                     #:exec-path-from-shell? exec-path-from-shell?))
  (define node-ok?
    (parameterize ([current-output-port (open-output-nowhere)]
                   [current-error-port (open-output-nowhere)])
      (system* node-path "--version")))
  (unless node-ok?
    (error who
           (string-append
            "found `node` in PATH but could not run `node --version`.\n"
            "Check the installation and executable permissions.")))
  (define wasm-exnref-flag-ok?
    (parameterize ([current-output-port (open-output-nowhere)]
                   [current-error-port (open-output-nowhere)])
      (system* node-path "--experimental-wasm-exnref" "-e" "")))
  (unless wasm-exnref-flag-ok?
    (error who
           (string-append
            "found `node`, but it does not accept `--experimental-wasm-exnref`.\n"
            "Install a compatible Node.js version or run without `-r`.")))
  node-path)

(define (ensure-distinct-output-files! who entries)
  (define seen (make-hash))
  (for ([entry entries])
    (define kind          (car entry))
    (define path          (cdr entry))
    (define normalized    (path->complete-path path (current-directory)))
    (define previous-kind (hash-ref seen normalized #f))
    (when previous-kind
      (error who
             (~a "output file collision: " previous-kind " and "
                 kind " resolve to the same path: " path)))
    (hash-set! seen normalized kind)))

;; check-wasm-tools! : symbol? [#:exec-path-from-shell? boolean?] -> void?
;;   Find wasm-tools using the shared lookup policy and verify that it runs.
(define (check-wasm-tools! who #:exec-path-from-shell? [exec-path-from-shell? #t])
  (define wasm-tools-path
    (wasm-tools-executable #:who who
                           #:exec-path-from-shell? exec-path-from-shell?))
  (define parse-help-ok?
    (parameterize ([current-output-port (open-output-nowhere)]
                   [current-error-port (open-output-nowhere)])
      (system* wasm-tools-path "parse" "--help")))
  (unless parse-help-ok?
    (error who
           (string-append
            "found `wasm-tools` in PATH but could not run `wasm-tools parse --help`.\n"
            "Check the installation and executable permissions."))))

;; strip-wasm-name-section! : symbol? path-string? [#:exec-path-from-shell? boolean?] -> void?
;;   Remove the custom `name` section from a generated Wasm file.
(define (strip-wasm-name-section! who
                                  wasm-path
                                  #:exec-path-from-shell? [exec-path-from-shell? #t])
  (define (path-arg path)
    (path->string (path->complete-path path (current-directory))))
  (define wasm-tools-path
    (wasm-tools-executable #:who who
                           #:missing-purpose "strip Wasm custom name sections"
                           #:exec-path-from-shell? exec-path-from-shell?))
  (define tmp-path
    (make-temporary-file
     "webracket-strip-wasm-names-~a.wasm"
     #f
     (or (path-only wasm-path) (current-directory))))
  (define success? #t)
  (define strip-output
    (with-output-to-string
      (lambda ()
        (set! success?
              (system* wasm-tools-path
                       "strip"
                       "-d"
                       "^name$"
                       (path-arg wasm-path)
                       "-o"
                       (path-arg tmp-path))))))
  (cond
    [success?
     (rename-file-or-directory tmp-path wasm-path #t)]
    [else
     (with-handlers ([exn:fail:filesystem? void])
       (delete-file tmp-path))
     (error who
            (string-append
             "wasm-tools failed to strip the Wasm name section from "
             (path->string wasm-path)
             "\n"
             strip-output))]))

;;;
;;; READ MODULE
;;;

(define (read-lang-module port)
  (port-count-lines! port)
  ; Reset all reader parameters to default values.
  (with-module-reading-parameterization 
    (lambda ()
      (read-syntax (object-name port) port))))

; read-lang-file : path-string -> syntax
;   Read the program in `path-string`.
;   Return a syntax object representing a module.
(define (read-lang-file path-string)
  (call-with-input-file path-string
    (λ (port)
      (parameterize ([current-directory (or (path-only path-string)
                                            (current-directory))])
        (read-lang-module port)))))

;;;
;;; EXPAND MODULE
;;;

(require "expander.rkt") ; provides `topexpand`

;;;
;;; EXPAND FILE
;;;

; Note: The following procedure is modelled after:
;    racket/collects/compiler/compile-file.rkt
; It uses `dynamic-wind` to handle errors (removing temporary files etc.).
; With an eye towards bootstrapping, we postpone using `dynamic-wind`.

(define (expand-file src-path
                     [dest-path
                      (let-values ([(base name dir?) (split-path src-path)])
                        (build-path base "expanded"
                                    (path-add-suffix name #".erkt")))])
  ; create .../expanded/ if needed
  (make-directory* (path-only dest-path))

  ; We write to a temporary file and only move it to `dest-path`
  ; only if we encounter no errors.
  (define temp-filename (make-temporary-file "tmp~a" #f (path-only dest-path)))

  (define dir
    (let-values ([(base name dir?) (split-path src-path)])
      (if (eq? base 'relative)
          (current-directory)
          (path->complete-path base (current-directory)))))

  (define out (open-output-file temp-filename #:exists 'truncate/replace))

  (parameterize ([current-load-relative-directory dir]
                 [current-write-relative-directory dir])
    (define program (read-lang-file src-path))
    (pretty-write (syntax->datum
                   (topexpand program))
                   out))
  
  (close-output-port out)

  (rename-file-or-directory temp-filename dest-path #t) ; exists-ok? = #t

  (with-handlers ([exn:fail:filesystem? void])
    (delete-file temp-filename)))

;;;
;;; RESOLUTION OF FFI-PATHS
;;;


(define-runtime-path system-ffi-directory "ffi")

(define (resolve-ffi-filename ffi-filename)
  (define filename
    (if (path-get-extension ffi-filename)
        ffi-filename
        (path-add-extension ffi-filename ".ffi")))
  
  (define candidates
    (if (absolute-path? filename)
        (list filename)
        (append (list filename
                      (build-path "ffi" filename))
                (if system-ffi-directory
                    (list (build-path system-ffi-directory filename))
                    '()))))
  (for/or ([candidate (in-list candidates)])
    (and candidate
         (file-exists? candidate)
         (path->complete-path candidate (current-directory)))))
