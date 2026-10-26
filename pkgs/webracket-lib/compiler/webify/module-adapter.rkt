#lang racket/base

(provide strip-correlated
         (struct-out bridge-export-entry)
         (struct-out first-slice-export-plan)
         bridge-export-entry-cell-backed?
         bridge-export-entry-plain-value?
         bridge-export-entry-ref-expression
         bridge-export-entry-boundary-expression
         default-export-entry-map
         first-slice-import-args
         first-slice-import-cell-args
         build-first-slice-export-plan
         first-slice-top-level-helper-ids
         first-slice-top-level-export-cell-ids
         first-slice-top-level-export-value-ids
         first-slice->top-level-datum
         first-slice->top-level-datum/imports
         first-slice->top-level-syntax
         webify-linklet-first-slice-top-level/imports
         webify-linklet-first-slice-top-level)

(require racket/list
         racket/match
         racket/string
         "../private/parameters.rkt"
         "bridge-stats.rkt"
         "first-slice.rkt"
         "primitive-modules.rkt"
         "wrap.rkt")

(struct bridge-export-entry (name kind binding metadata) #:transparent)
(struct first-slice-export-plan (adapted-expr helper-ids cell-ids value-id-map value-ids export-args candidate-specs entry-map)
  #:transparent)

;; bridge-export-entry-cell-backed? : bridge-export-entry? -> boolean?
;;   Check whether entry exposes a module-variable cell at the public boundary.
(define (bridge-export-entry-cell-backed? entry)
  (eq? (bridge-export-entry-kind entry) 'cell))

;; bridge-export-entry-plain-value? : bridge-export-entry? -> boolean?
;;   Check whether entry exposes a plain lexical value at the public boundary.
(define (bridge-export-entry-plain-value? entry)
  (eq? (bridge-export-entry-kind entry) 'value))

;; bridge-export-entry-ref-expression : bridge-export-entry? -> any/c
;;   Read one export through the mixed boundary ABI.
(define (bridge-export-entry-ref-expression entry)
  (if (bridge-export-entry-cell-backed? entry)
      `(module-variable-ref/no-check ,(bridge-export-entry-binding entry))
      (bridge-export-entry-binding entry)))

;; bridge-export-entry-boundary-expression : bridge-export-entry? -> any/c
;;   Return the boundary binding passed to downstream importers.
(define (bridge-export-entry-boundary-expression entry)
  (bridge-export-entry-binding entry))

;; module-provider-kind : any/c -> symbol?
;;   Classify one module name using the active bridge graph, if available.
(define (module-provider-kind module-name)
  (define module-kinds
    (current-module-bridge-module-kinds))
  (cond
    [(kernel-provider-name? module-name) 'kernel-provider]
    [(not (symbol? module-name)) 'unknown-provider]
    [(and module-kinds
          (hash-has-key? module-kinds module-name))
     (hash-ref module-kinds module-name)]
    [else 'unknown-provider]))

;; quote-like-form? : any/c -> boolean?
;;   Recognize datum forms that should not count as executable bridge code.
(define (quote-like-form? v)
  (match v
    [`(,(or 'quote 'quote-syntax 'syntax 'quasiquote) . ,_) #t]
    [_ #f]))

;; strip-quote : any/c -> any/c
;;   Decode one quoted datum layer when bridge metadata stores literal symbols.
(define (strip-quote v)
  (match v
    [`(quote ,inner) inner]
    [_ v]))

;; export-arg-metadata-list : any/c (listof symbol?) (listof symbol?) (or/c symbol? #f) any/c -> (listof hash?)
;;   Build conservative metadata rows for each public export argument.
(define (export-arg-metadata-list expr exports export-args module-name importss)
  (define usage (lowered-export-usage-summary expr export-args))
  (define mutated (hash-ref usage 'mutated))
  (define undefined-sensitive (hash-ref usage 'undefined-sensitive))
  (define reflection-visible (hash-ref usage 'reflection-visible))
  (define define-constances (hash-ref usage 'define-constances))
  (define read-before-definition
    (lowered-export-read-before-definition-summary expr export-args))
  (define definition-materialization-before-definition
    (lowered-export-materialization-before-definition-summary expr
                                                              export-args
                                                              #:definitions-only? #t))
  (define imported-names
    (if importss
        (flatten-groups importss)
        '()))
  (for/list ([export (in-list exports)]
             [export-arg (in-list export-args)])
    (define reexport?
      (memq export imported-names))
    (define constance
      (hash-ref define-constances export-arg #f))
    (hasheq 'module-name module-name
            'export export
            'public-boundary? #t
            'internal? #f
            'immutable? (and constance #t)
            'constance constance
            'reexport? (and reexport? #t)
            'mutated? (and (hash-ref mutated export-arg #f) #t)
            'undefined-sensitive? (and (hash-ref undefined-sensitive export-arg #f) #t)
            'read-before-definition? (and (hash-ref read-before-definition export-arg #f) #t)
            'definition-materialization-before-definition?
            (and (hash-ref definition-materialization-before-definition export-arg #f) #t)
            'reflection-visible? (and (hash-ref reflection-visible export-arg #f) #t)
            'phase0-value-space-only? #t
            'dynamic-require-sensitive? #f)))

;; plain-value-export-base-candidate? : hash? -> boolean?
;;   Check the conservative safety preconditions for eliding one public export cell.
(define (plain-value-export-base-candidate? meta)
  (define module-name (hash-ref meta 'module-name #f))
  (and module-name
       (symbol? module-name)
       (hash-ref meta 'public-boundary? #f)
       (hash-ref meta 'immutable? #f)
       (not (hash-ref meta 'reexport? #f))
       (not (hash-ref meta 'mutated? #f))
       (not (hash-ref meta 'undefined-sensitive? #f))
       (not (hash-ref meta 'read-before-definition? #f))
       (not (hash-ref meta 'definition-materialization-before-definition? #f))
       (not (hash-ref meta 'reflection-visible? #f))
       (not (hash-ref meta 'dynamic-require-sensitive? #f))
       (hash-ref meta 'phase0-value-space-only? #f)))

;; plain-value-export-policy-eligible? : hash? -> boolean?
;;   Check whether the current public-cell elision policy allows this module.
(define (plain-value-export-policy-eligible? meta)
  (define module-name (hash-ref meta 'module-name #f))
  (define allowed-modules
    (current-module-bridge-elidable-module-names))
  (define provider-kind
    (hash-ref meta 'provider-kind (module-provider-kind module-name)))
  (and module-name
       (symbol? module-name)
       (or (memq provider-kind '(local-user support-provider))
           (and allowed-modules
                (memq module-name allowed-modules)))))

;; plain-value-export-candidate? : hash? -> boolean?
;;   Check whether one public export satisfies both safety and policy checks.
(define (plain-value-export-candidate? meta)
  (and (plain-value-export-base-candidate? meta)
       (plain-value-export-policy-eligible? meta)))

;; export-entry-kind : hash? hash? -> symbol?
;;   Decide whether one public export should stay cell-backed or become a plain value.
(define (export-entry-kind meta forced-cell-exports)
  (define export (hash-ref meta 'export #f))
  (if (and (current-module-bridge-elide-public-cells?)
           export
           (not (hash-ref forced-cell-exports export #f))
           (plain-value-export-candidate? meta))
      'value
      'cell))

;; default-export-entry-map : (listof symbol?) hash? hash? -> hasheq?
;;   Build the legacy all-cell export-entry map when no expression analysis is available.
(define (default-export-entry-map exports cell-id-map value-id-map)
  (for/hasheq ([export (in-list exports)])
    (define cell-id (hash-ref cell-id-map export))
    (define value-id (hash-ref value-id-map export))
    (values export
            (bridge-export-entry
             export
             'cell
             cell-id
             (hasheq 'cell-id cell-id
                     'value-id value-id)))))

;; lowered-export-usage-summary : any/c (listof symbol?) -> hasheq?
;;   Collect conservative per-export usage flags from one lowered first slice.
(define (lowered-export-usage-summary expr export-args)
  (define export-set
    (for/hasheq ([arg (in-list export-args)])
      (values arg #t)))
  (define mutated (make-hasheq))
  (define undefined-sensitive (make-hasheq))
  (define reflection-visible (make-hasheq))
  (define define-constances (make-hasheq))
  (define defined (make-hasheq))
  (define (record! table id)
    (when (hash-ref export-set id #f)
      (hash-set! table id #t)))
  (define (walk v)
    (cond
      [(pair? v)
       (unless (quote-like-form? v)
         (match v
           [`(,(or 'variable-set! 'variable-set!/check-undefined) ,cell ,rhs)
            (record! mutated cell)
            (when (eq? (car v) 'variable-set!/check-undefined)
              (record! undefined-sensitive cell))
            (walk rhs)]
           [`(module-variable-set! ,cell ,rhs)
            (record! mutated cell)
            (walk rhs)]
           [`(module-variable-set!/check-undefined! ,cell ,rhs)
            (record! mutated cell)
            (record! undefined-sensitive cell)
            (walk rhs)]
           [`(variable-set!/define ,cell ,rhs ,_constance)
            (walk rhs)
            (when (hash-ref export-set cell #f)
              (hash-set! define-constances cell (strip-quote _constance)))
            (hash-set! defined cell #t)]
           [`(module-variable-define! ,cell ,rhs ,_constance)
            (walk rhs)
            (when (hash-ref export-set cell #f)
              (hash-set! define-constances cell (strip-quote _constance)))
            (hash-set! defined cell #t)]
           [`(,(or 'variable-ref 'variable-ref/no-check) ,_cell)
            (void)]
           [`(,(or 'module-variable-ref 'module-variable-ref/no-check) ,_cell)
            (void)]
           [`(module-variable-reference ,instance ,cell)
            (walk instance)
            (record! reflection-visible cell)]
           [`(begin ,es ...)
            (for ([e (in-list es)])
              (walk e))]
           [`(,head . ,rest)
            (walk head)
            (walk rest)]
           [_ (void)]))]
      [(vector? v)
       (for ([entry (in-vector v)])
         (walk entry))]
      [else
       (void)]))
  (walk expr)
  (hasheq 'mutated mutated
          'undefined-sensitive undefined-sensitive
          'reflection-visible reflection-visible
          'define-constances define-constances))

;; lowered-export-read-before-definition-summary : any/c (listof symbol?) -> hasheq?
;;   Find export arguments that are captured or read before their materializing write.
(define (lowered-export-read-before-definition-summary expr export-args)
  (define export-set
    (for/hasheq ([arg (in-list export-args)])
      (values arg #t)))
  (define read-before-definition (make-hasheq))
  (define (record! id)
    (when (hash-ref export-set id #f)
      (hash-set! read-before-definition id #t)))
  (define (shadowed? id bound-set)
    (hash-ref bound-set id #f))
  (define (extend-bound bound-set ids)
    (for/fold ([set bound-set])
              ([id (in-list ids)])
      (hash-set set id #t)))
  (define (record-read! id defined bound-set)
    (when (and (symbol? id)
               (hash-ref export-set id #f)
               (not (shadowed? id bound-set))
               (not (hash-ref defined id #f)))
      (record! id)))
  (define (record-target! id defined bound-set)
    (when (and (symbol? id)
               (hash-ref export-set id #f)
               (not (shadowed? id bound-set)))
      (hash-set! defined id #t)))
  (define (collect-read-exports! v bound-set defined)
    (cond
      [(symbol? v)
       (record-read! v defined bound-set)]
      [(pair? v)
       (unless (quote-like-form? v)
         (match v
           [`(lambda ,args ,body ...)
            ;; Creating a closure does not read its free variables yet.
            ;; Only reads in the surrounding initialization path should
            ;; force an export to stay cell-backed.
            (void)]
           [`(case-lambda ,clauses ...)
            (void)]
           [`(,(or 'variable-ref
                   'variable-ref/no-check
                   'module-variable-ref
                   'module-variable-ref/no-check)
              ,cell)
            (record-read! cell defined bound-set)]
           [`(,(or 'module-variable-define! 'variable-set!/define) ,cell ,rhs ,_constance)
            (collect-read-exports! rhs bound-set defined)
            (record-target! cell defined bound-set)]
           [`(set-boxed! ,cell ,rhs)
            (collect-read-exports! rhs bound-set defined)
            (record-target! cell defined bound-set)]
           [`(define ,_id
               (begin
                 (module-variable-define! ,cell ,rhs ,_constance)
                 (void)))
            (collect-read-exports! rhs bound-set defined)
            (record-target! cell defined bound-set)]
           [`(let-values ,bindings ,body ...)
            (for ([binding (in-list bindings)])
              (match binding
                [(list _ids rhs) (collect-read-exports! rhs bound-set defined)]
                [_ (void)]))
            (define body-bound
              (extend-bound bound-set (apply append (map binding-ids bindings))))
            (for ([form (in-list body)])
              (collect-read-exports! form body-bound defined))]
           [`(letrec-values ,bindings ,body ...)
            (define body-bound
              (extend-bound bound-set (apply append (map binding-ids bindings))))
            (for ([binding (in-list bindings)])
              (match binding
                [(list _ids rhs) (collect-read-exports! rhs body-bound defined)]
                [_ (void)]))
            (for ([form (in-list body)])
              (collect-read-exports! form body-bound defined))]
           [`(letrec-syntaxes+values ,_syntax-bindings ,value-bindings ,body ...)
            (define body-bound
              (extend-bound bound-set (apply append (map binding-ids value-bindings))))
            (for ([binding (in-list value-bindings)])
              (match binding
                [(list _ids rhs) (collect-read-exports! rhs body-bound defined)]
                [_ (void)]))
            (for ([form (in-list body)])
              (collect-read-exports! form body-bound defined))]
           [`(define ,_id ,rhs)
            (collect-read-exports! rhs bound-set defined)]
           [`(define-values ,_ids ,rhs)
            (collect-read-exports! rhs bound-set defined)]
           [`(begin ,forms ...)
            (for ([form (in-list forms)])
              (collect-read-exports! form bound-set defined))]
           [`(begin0 ,head ,rest ...)
            (collect-read-exports! head bound-set defined)
            (for ([form (in-list rest)])
              (collect-read-exports! form bound-set defined))]
           [`(,head . ,rest)
            (collect-read-exports! head bound-set defined)
            (collect-read-exports! rest bound-set defined)]
           [_ (void)]))]
      [(vector? v)
       (for ([entry (in-vector v)])
         (collect-read-exports! entry bound-set defined))]
      [else (void)]))
  (match expr
    [`(lambda ,_args ,body ...)
     (define defined (make-hasheq))
     (for ([form (in-list body)])
       (collect-read-exports! form (hasheq) defined))]
    [_ (void)])
  read-before-definition)

;; lowered-export-closure-capture-summary : any/c (listof symbol?) -> hasheq?
;;   Find export arguments referenced from inside closure bodies.
(define (lowered-export-closure-capture-summary expr export-args)
  (define export-set
    (for/hasheq ([arg (in-list export-args)])
      (values arg #t)))
  (define captured (make-hasheq))
  (define defined (make-hasheq))
  (define (record! id)
    (when (and (hash-ref export-set id #f)
               (not (hash-ref defined id #f)))
      (hash-set! captured id #t)))
  (define (record-defined! id bound-set closure-depth)
    (when (and (zero? closure-depth)
               (symbol? id)
               (hash-ref export-set id #f)
               (not (shadowed? id bound-set)))
      (hash-set! defined id #t)))
  (define (shadowed? id bound-set)
    (hash-ref bound-set id #f))
  (define (extend-bound bound-set ids)
    (for/fold ([set bound-set])
              ([id (in-list ids)])
      (hash-set set id #t)))
  (define (resolve-export-id id alias-env bound-set)
    (cond
      [(shadowed? id bound-set) #f]
      [(hash-ref export-set id #f) id]
      [else (hash-ref alias-env id #f)]))
  (define (resolve-export-alias rhs alias-env bound-set)
    (cond
      [(symbol? rhs)
       (resolve-export-id rhs alias-env bound-set)]
      [(pair? rhs)
       (match rhs
         [`(,(or 'module-variable-ref 'module-variable-ref/no-check) ,cell)
          (and (symbol? cell)
               (resolve-export-id cell alias-env bound-set))]
         [_ #f])]
      [else #f]))
  (define (update-alias-env alias-env form bound-set)
    (match form
      [`(define ,id ,rhs)
       (if (symbol? id)
           (let ([src (resolve-export-alias rhs alias-env bound-set)])
             (if src
                 (hash-set alias-env id src)
                 (hash-remove alias-env id)))
           alias-env)]
      [`(define-values ,ids ,rhs)
       (define bound-ids (id-list ids))
       (if (= (length bound-ids) 1)
           (let* ([id (car bound-ids)]
                  [src (resolve-export-alias rhs alias-env bound-set)])
             (if src
                 (hash-set alias-env id src)
                 (hash-remove alias-env id)))
           alias-env)]
      [_ alias-env]))
  (define (walk v bound-set closure-depth alias-env)
    (cond
      [(symbol? v)
       (when (positive? closure-depth)
         (define src
           (resolve-export-id v alias-env bound-set))
         (when src
           (record! src)))]
      [(pair? v)
       (unless (quote-like-form? v)
         (match v
           [`(lambda ,args ,body ...)
            (define body-bound
              (extend-bound bound-set (id-list args)))
            (for ([form (in-list body)])
              (walk form body-bound (add1 closure-depth) alias-env))]
           [`(case-lambda ,clauses ...)
            (for ([clause (in-list clauses)])
              (match clause
                [`(,args ,body ...)
                 (define body-bound
                   (extend-bound bound-set (id-list args)))
                 (for ([form (in-list body)])
                   (walk form body-bound (add1 closure-depth) alias-env))]
                [_ (void)]))]
           [`(let-values ,bindings ,body ...)
            (for ([binding (in-list bindings)])
              (match binding
                [(list _ids rhs) (walk rhs bound-set closure-depth alias-env)]
                [_ (void)]))
            (define body-bound
              (extend-bound bound-set (apply append (map binding-ids bindings))))
            (for ([form (in-list body)])
              (walk form body-bound closure-depth alias-env))]
           [`(letrec-values ,bindings ,body ...)
            (define body-bound
              (extend-bound bound-set (apply append (map binding-ids bindings))))
            (for ([binding (in-list bindings)])
              (match binding
                [(list _ids rhs) (walk rhs body-bound closure-depth alias-env)]
                [_ (void)]))
            (for ([form (in-list body)])
              (walk form body-bound closure-depth alias-env))]
           [`(letrec-syntaxes+values ,_syntax-bindings ,value-bindings ,body ...)
            (define body-bound
              (extend-bound bound-set (apply append (map binding-ids value-bindings))))
            (for ([binding (in-list value-bindings)])
              (match binding
                [(list _ids rhs) (walk rhs body-bound closure-depth alias-env)]
                [_ (void)]))
            (for ([form (in-list body)])
              (walk form body-bound closure-depth alias-env))]
           [`(define ,_id ,rhs)
            (walk rhs bound-set closure-depth alias-env)]
           [`(define-values ,_ids ,rhs)
            (walk rhs bound-set closure-depth alias-env)]
           [`(,(or 'module-variable-define! 'variable-set!/define) ,cell ,rhs ,_constance)
            (walk rhs bound-set closure-depth alias-env)
            (record-defined! cell bound-set closure-depth)]
           [`(set-boxed! ,cell ,rhs)
            (walk rhs bound-set closure-depth alias-env)
            (record-defined! cell bound-set closure-depth)]
           [`(,head . ,rest)
            (walk head bound-set closure-depth alias-env)
            (walk rest bound-set closure-depth alias-env)]
           [_ (void)]))]
      [(vector? v)
       (for ([entry (in-vector v)])
         (walk entry bound-set closure-depth alias-env))]
      [else
       (void)]))
  (match expr
    [`(lambda ,_args ,body ...)
     (let loop ([forms body]
                [alias-env (hasheq)])
       (unless (null? forms)
         (define form (car forms))
         (walk form (hasheq) 0 alias-env)
         (loop (cdr forms)
               (update-alias-env alias-env form (hasheq)))))]
    [_ (walk expr (hasheq) 0 (hasheq))])
  captured)

;; lowered-export-placeholder-definition-summary : any/c (listof symbol?) -> hasheq?
;;   Find export cells that are initialized with a placeholder void value.
(define (lowered-export-placeholder-definition-summary expr export-args)
  (define export-set
    (for/hasheq ([arg (in-list export-args)])
      (values arg #t)))
  (define placeholder-definitions (make-hasheq))
  (define (placeholder-rhs? v)
    (or (equal? v '(void))
        (equal? v '(boxed (void)))))
  (define (record-placeholder! cell rhs)
    (when (and (hash-ref export-set cell #f)
               (placeholder-rhs? rhs))
      (hash-set! placeholder-definitions cell #t)))
  (define (walk v)
    (cond
      [(pair? v)
       (unless (quote-like-form? v)
         (match v
           [`(lambda ,_args ,body ...)
            (for ([form (in-list body)]) (walk form))]
           [`(case-lambda ,clauses ...)
            (for ([clause (in-list clauses)])
              (match clause
                [`(,_args ,body ...)
                 (for ([form (in-list body)]) (walk form))]
                [_ (void)]))]
           [`(let-values ,bindings ,body ...)
            (for ([binding (in-list bindings)])
              (match binding
                [(list _ids rhs) (walk rhs)]
                [_ (void)]))
            (for ([form (in-list body)]) (walk form))]
           [`(letrec-values ,bindings ,body ...)
            (for ([binding (in-list bindings)])
              (match binding
                [(list _ids rhs) (walk rhs)]
                [_ (void)]))
            (for ([form (in-list body)]) (walk form))]
           [`(letrec-syntaxes+values ,_syntax-bindings ,value-bindings ,body ...)
            (for ([binding (in-list value-bindings)])
              (match binding
                [(list _ids rhs) (walk rhs)]
                [_ (void)]))
            (for ([form (in-list body)]) (walk form))]
           [`(define ,_id
               (begin
                 (module-variable-define! ,cell ,rhs ,_constance)
                 (void)))
            (record-placeholder! cell rhs)
            (walk rhs)]
           [`(,(or 'module-variable-define! 'variable-set!/define) ,cell ,rhs ,_constance)
            (record-placeholder! cell rhs)
            (walk rhs)]
           [`(begin ,forms ...)
            (for ([form (in-list forms)]) (walk form))]
           [`(,head . ,rest)
            (walk head)
            (walk rest)]
           [_ (void)]))]
      [(vector? v)
       (for ([entry (in-vector v)]) (walk entry))]
      [else (void)]))
  (walk expr)
  placeholder-definitions)

;; lowered-export-materialization-before-definition-summary : any/c (listof symbol?) [#:definitions-only? boolean?] -> hasheq?
;;   Find export writes whose late materialization would be reordered after definitions.
(define (lowered-export-materialization-before-definition-summary expr
                                                                  export-args
                                                                  #:definitions-only? [definitions-only? #f])
  (define export-set
    (for/hasheq ([arg (in-list export-args)])
      (values arg #t)))
  (define unsafe (make-hasheq))
  (define (record! id)
    (when (hash-ref export-set id #f)
      (hash-set! unsafe id #t)))
  (define (materialization-targets form)
    (define targets '())
    (define (record-target! cell)
      (when (hash-ref export-set cell #f)
        (set! targets (cons cell targets))))
    (define (walk v)
      (cond
        [(pair? v)
         (unless (quote-like-form? v)
           (match v
             [`(lambda . ,_)
              (void)]
             [`(case-lambda . ,_)
              (void)]
             [`(,(or 'module-variable-define! 'variable-set!/define) ,cell ,rhs ,_constance)
              (record-target! cell)
              (walk rhs)]
             [`(set-boxed! ,cell ,rhs)
              (record-target! cell)
              (walk rhs)]
             [`(let-values ,bindings ,body ...)
              (for ([binding (in-list bindings)])
                (match binding
                  [(list _ids rhs) (walk rhs)]
                  [_ (void)]))
              (for ([body-form (in-list body)])
                (walk body-form))]
             [`(letrec-values ,bindings ,body ...)
              (for ([binding (in-list bindings)])
                (match binding
                  [(list _ids rhs) (walk rhs)]
                  [_ (void)]))
              (for ([body-form (in-list body)])
                (walk body-form))]
             [`(letrec-syntaxes+values ,_syntax-bindings ,value-bindings ,body ...)
              (for ([binding (in-list value-bindings)])
                (match binding
                  [(list _ids rhs) (walk rhs)]
                  [_ (void)]))
              (for ([body-form (in-list body)])
                (walk body-form))]
             [`(define ,_id ,rhs)
              (walk rhs)]
             [`(define-values ,_ids ,rhs)
              (walk rhs)]
             [`(begin ,forms ...)
              (for ([body-form (in-list forms)])
                (walk body-form))]
             [`(begin0 ,head ,rest ...)
              (walk head)
              (for ([body-form (in-list rest)])
                (walk body-form))]
             [`(,head . ,rest)
              (walk head)
              (walk rest)]
             [_ (void)]))]
        [(vector? v)
         (for ([entry (in-vector v)])
           (walk entry))]
        [else
         (void)]))
    (walk form)
    (remove-duplicates (reverse targets) eq?))
  (define (definition-form? form)
    (match form
      [`(define . ,_) #t]
      [`(define-values . ,_) #t]
      [_ #f]))
  (match expr
    [`(lambda ,_args ,body ...)
     (let loop ([forms (reverse body)]
                [definition-after? #f])
       (unless (null? forms)
         (define form (car forms))
         (define targets
           (if (or (not definitions-only?)
                   (definition-form? form))
               (materialization-targets form)
               '()))
         (when (and (not (null? targets))
                    definition-after?)
           (for ([target (in-list targets)])
             (record! target)))
         (loop (cdr forms)
               (or definition-after?
                   (definition-form? form)))))]
    [_ (void)])
  unsafe)

;; record-export-cell-stats! : any/c (listof symbol?) (listof symbol?) (or/c symbol? #f) -> void?
;;   Publish conservative export-cell metadata for dry-run bridge analysis.
(define (record-export-cell-stats! expr
                                   exports
                                   export-args
                                   module-name
                                   importss
                                   #:export-entries [export-entries #f])
  (define metas
    (if export-entries
        (for/list ([export (in-list exports)])
          (bridge-export-entry-metadata
           (hash-ref export-entries export)))
        (export-arg-metadata-list expr exports export-args module-name importss)))
  (for ([meta (in-list metas)])
    (define export (hash-ref meta 'export))
    (webify-bridge-stats-set! (list 'bridge-export-cell-meta module-name export) meta)
    (when module-name
      (webify-bridge-stats-inc! (list 'bridge-export-cell-module module-name)))
    (webify-bridge-stats-inc! (list 'bridge-export-cell-name export)))
  (webify-bridge-stats-inc! 'bridge-export-cell-public-boundary-count (length exports))
  (webify-bridge-stats-inc! 'bridge-export-cell-reexport-count
                            (for/sum ([meta (in-list metas)])
                              (if (hash-ref meta 'reexport? #f) 1 0)))
  (webify-bridge-stats-inc! 'bridge-export-cell-mutated-count
                            (for/sum ([meta (in-list metas)])
                              (if (hash-ref meta 'mutated? #f) 1 0)))
  (webify-bridge-stats-inc! 'bridge-export-cell-undefined-sensitive-count
                            (for/sum ([meta (in-list metas)])
                              (if (hash-ref meta 'undefined-sensitive? #f) 1 0)))
  (webify-bridge-stats-inc! 'bridge-export-cell-reflection-visible-count
                            (for/sum ([meta (in-list metas)])
                              (if (hash-ref meta 'reflection-visible? #f) 1 0))))

;; late-materializable-export-specs : any/c (listof symbol?) (listof symbol?) (listof symbol?) (listof symbol?) any/c -> (listof vector?)
;;   Identify exports that can use lexical values internally before cell materialization.
(define (late-materializable-export-specs expr
                                          exports
                                          export-args
                                          value-ids
                                          candidate-local-ids
                                          importss)
  (define usage (lowered-export-usage-summary expr export-args))
  (define mutated (hash-ref usage 'mutated))
  (define undefined-sensitive (hash-ref usage 'undefined-sensitive))
  (define reflection-visible (hash-ref usage 'reflection-visible))
  (define define-constances (hash-ref usage 'define-constances))
  (define read-before-definition
    (lowered-export-read-before-definition-summary expr export-args))
  (define closure-captured
    (lowered-export-closure-capture-summary expr export-args))
  (define placeholder-definition
    (lowered-export-placeholder-definition-summary expr export-args))
  (define materialization-before-definition
    (lowered-export-materialization-before-definition-summary expr export-args))
  (define definition-materialization-before-definition
    (lowered-export-materialization-before-definition-summary expr
                                                              export-args
                                                              #:definitions-only? #t))
  (define imported-names
    (if importss
        (flatten-groups importss)
        '()))
  (if (positive? (hash-count placeholder-definition))
      '()
      (for/list ([export (in-list exports)]
                 [export-arg (in-list export-args)]
                 [value-id (in-list value-ids)]
                 [candidate-local-id (in-list candidate-local-ids)]
                 #:unless (or (hash-ref mutated export-arg #f)
                              (hash-ref undefined-sensitive export-arg #f)
                              (hash-ref reflection-visible export-arg #f)
                              (hash-ref closure-captured export-arg #f)
                              (hash-ref read-before-definition export-arg #f)
                              (and (not (current-module-bridge-elide-public-cells?))
                                   (hash-ref materialization-before-definition export-arg #f))
                              (hash-ref definition-materialization-before-definition export-arg #f)
                              (not (hash-has-key? define-constances export-arg))))
        (vector export
                export-arg
                value-id
                candidate-local-id
                (hash-ref define-constances export-arg)
                (memq export imported-names)))))

;; id-list : any/c -> (listof symbol?)
;;   Collect symbol binders from lowered argument or binding forms.
(define (id-list ids)
  (cond
    [(symbol? ids) (list ids)]
    [(pair? ids)   (append (id-list (car ids))
                           (id-list (cdr ids)))]
    [(null? ids)   '()]
    [else          '()]))

;; canonical-candidate-value-id : symbol? hasheq? -> (or/c symbol? #f)
;;   Map alpha-renamed candidate value ids back to the canonical export value id.
(define (canonical-candidate-value-id id candidate-map)
  (and (symbol? id)
       (for/or ([candidate (in-list (hash-values candidate-map))])
         (define candidate-str (symbol->string candidate))
         (define id-str (symbol->string id))
         (and (string-prefix? id-str (string-append candidate-str "."))
              candidate))))

;; normalize-rewritten-id : any/c hasheq? -> any/c
;;   Canonicalize candidate value-id variants that passes may alpha-rename.
(define (normalize-rewritten-id id candidate-map)
  (or (canonical-candidate-value-id id candidate-map)
      id))

;; normalize-id-tree : any/c hasheq? -> any/c
;;   Canonicalize candidate value-id variants within binder shapes.
(define (normalize-id-tree ids candidate-map)
  (cond
    [(symbol? ids) (normalize-rewritten-id ids candidate-map)]
    [(pair? ids)   (cons (normalize-id-tree (car ids) candidate-map)
                         (normalize-id-tree (cdr ids) candidate-map))]
    [else          ids]))

;; binding-ids : any/c -> (listof symbol?)
;;   Collect binders from one lowered let-values binding clause.
(define (binding-ids binding)
  (match binding
    [(list ids _rhs) (id-list ids)]
    [_ '()]))

;; extend-bound-set : hasheq? (listof symbol?) hasheq? -> hasheq?
;;   Record identifiers that shadow candidate export-arg rewrites.
(define (extend-bound-set bound-set ids candidate-map)
  (for/fold ([set bound-set])
            ([id (in-list ids)])
    (hash-set set (normalize-rewritten-id id candidate-map) #t)))

;; extend-reference-bound-set : hasheq? (listof symbol?) -> hasheq?
;;   Record identifiers that shadow export-argument reference rewrites.
(define (extend-reference-bound-set bound-set ids)
  (for/fold ([set bound-set])
            ([id (in-list ids)])
    (hash-set set id #t)))

;; body-definition-ids : any/c -> (listof symbol?)
;;   Collect names defined by one body form for later shadowing.
(define (body-definition-ids form)
  (match form
    [`(define (,id . ,_args) . ,_)
     (if (symbol? id) (list id) '())]
    [`(define ,id . ,_)
     (if (symbol? id) (list id) '())]
    [`(define-values ,ids . ,_)
     (id-list ids)]
    [_ '()]))

;; rewrite-export-arg-references : any/c hasheq? [hasheq?] -> any/c
;;   Replace active export arguments with their generated wrapper bindings.
(define (rewrite-export-arg-references v replacement-map [bound-set (hasheq)])
  (define (replace-id id)
    (if (and (symbol? id)
             (not (hash-ref bound-set id #f)))
        (hash-ref replacement-map id id)
        id))
  (define (rewrite entry [bound bound-set])
    (rewrite-export-arg-references entry replacement-map bound))
  (define (rewrite-list xs [bound bound-set])
    (map (lambda (entry) (rewrite entry bound)) xs))
  (cond
    [(symbol? v) (replace-id v)]
    [(pair? v)
     (if (quote-like-form? v)
         v
         (match v
           [`(#%top . ,id)
            (replace-id id)]
           [`(lambda ,args ,body ...)
            (define body-bound
              (extend-reference-bound-set bound-set (id-list args)))
            `(lambda ,args
               ,@(rewrite-export-arg-body-forms body replacement-map body-bound))]
           [`(case-lambda ,clauses ...)
            `(case-lambda
               ,@(for/list ([clause (in-list clauses)])
                   (match clause
                     [`(,args ,body ...)
                      (define body-bound
                        (extend-reference-bound-set bound-set (id-list args)))
                      `(,args
                        ,@(rewrite-export-arg-body-forms body replacement-map body-bound))]
                     [_ clause])))]
           [`(let-values ,bindings ,body ...)
            (define body-bound
              (extend-reference-bound-set bound-set
                                          (apply append (map binding-ids bindings))))
            `(let-values
               ,(for/list ([binding (in-list bindings)])
                  (match binding
                    [(list ids rhs)
                     `(,ids ,(rewrite rhs bound-set))]
                    [_ binding]))
               ,@(rewrite-export-arg-body-forms body replacement-map body-bound))]
           [`(letrec-values ,bindings ,body ...)
            (define body-bound
              (extend-reference-bound-set bound-set
                                          (apply append (map binding-ids bindings))))
            `(letrec-values
               ,(for/list ([binding (in-list bindings)])
                  (match binding
                    [(list ids rhs)
                     `(,ids ,(rewrite rhs body-bound))]
                    [_ binding]))
               ,@(rewrite-export-arg-body-forms body replacement-map body-bound))]
           [`(letrec-syntaxes+values ,syntax-bindings ,value-bindings ,body ...)
            (define body-bound
              (extend-reference-bound-set bound-set
                                          (apply append (map binding-ids value-bindings))))
            `(letrec-syntaxes+values
               ,syntax-bindings
               ,(for/list ([binding (in-list value-bindings)])
                  (match binding
                    [(list ids rhs)
                     `(,ids ,(rewrite rhs body-bound))]
                    [_ binding]))
               ,@(rewrite-export-arg-body-forms body replacement-map body-bound))]
           [`(begin ,forms ...)
            `(begin
               ,@(rewrite-export-arg-body-forms forms replacement-map bound-set))]
           [`(begin0 ,head ,rest ...)
            `(begin0 ,(rewrite head bound-set)
                     ,@(rewrite-export-arg-body-forms rest replacement-map bound-set))]
           [`(if ,test ,then ,else)
            `(if ,(rewrite test bound-set)
                 ,(rewrite then bound-set)
                 ,(rewrite else bound-set))]
           [`(define (,id . ,args) ,body ...)
            (define body-bound
              (extend-reference-bound-set bound-set
                                          (append (if (symbol? id) (list id) '())
                                                  (id-list args))))
            `(define (,id . ,args)
               ,@(rewrite-export-arg-body-forms body replacement-map body-bound))]
           [`(define ,id ,rhs)
            (define define-bound
              (extend-reference-bound-set bound-set
                                          (if (symbol? id) (list id) '())))
            `(define ,id ,(rewrite rhs define-bound))]
           [`(define-values ,ids ,rhs)
            (define define-bound
              (extend-reference-bound-set bound-set (id-list ids)))
            `(define-values ,ids ,(rewrite rhs define-bound))]
           [`(,head . ,rest)
            (cons (rewrite head bound-set)
                  (rewrite rest bound-set))]
           [_ v]))]
    [(vector? v)
     (list->vector (rewrite-list (vector->list v) bound-set))]
    [else v]))

;; rewrite-export-arg-body-forms : (listof any/c) hasheq? [hasheq?] -> (listof any/c)
;;   Rewrite a body sequence while carrying definition shadowing forward.
(define (rewrite-export-arg-body-forms body replacement-map [bound-set (hasheq)])
  (let loop ([remaining body]
             [body-bound-set bound-set]
             [acc '()])
    (cond
      [(null? remaining) (reverse acc)]
      [else
       (define form (car remaining))
       (define rewritten
         (rewrite-export-arg-references form replacement-map body-bound-set))
       (define next-bound
         (extend-reference-bound-set body-bound-set
                                     (body-definition-ids form)))
       (loop (cdr remaining) next-bound (cons rewritten acc))])))

;; rewrite-first-slice-export-args : any/c hasheq? -> any/c
;;   Normalize cell-backed export formals to generated cell bindings.
(define (rewrite-first-slice-export-args expr replacement-map)
  (if (zero? (hash-count replacement-map))
      expr
      (match expr
        [`(lambda ,args ,body ...)
         `(lambda ,args
            ,@(rewrite-export-arg-body-forms body replacement-map))]
        [_ expr])))

;; rewrite-late-materialized-export : any/c hasheq? [hasheq?] keyword-args -> any/c
;;   Replace active candidate export-arg references with lexical value
;;   bindings while still recognizing the full candidate set for nested
;;   materialization writes.
(define (rewrite-late-materialized-export v
                                          candidate-map
                                          [bound-set (hasheq)]
                                          #:all-candidate-map [all-candidate-map candidate-map])
  (define (rewrite entry [bound bound-set])
    (rewrite-late-materialized-export entry
                                      candidate-map
                                      bound
                                      #:all-candidate-map all-candidate-map))
  (define (rewrite-list xs [bound bound-set])
    (map (lambda (entry) (rewrite entry bound)) xs))
  (cond
    [(symbol? v)
     (define normalized (normalize-rewritten-id v candidate-map))
     (if (hash-ref bound-set normalized #f)
         normalized
         (hash-ref candidate-map normalized normalized))]
    [(pair? v)
     (if (quote-like-form? v)
         v
         (match v
           [`(variable-ref ,cell)
            (define normalized (normalize-rewritten-id cell candidate-map))
            (or (and (not (hash-ref bound-set normalized #f))
                     (hash-ref candidate-map normalized #f))
                v)]
           [`(variable-ref/no-check ,cell)
            (define normalized (normalize-rewritten-id cell candidate-map))
            (or (and (not (hash-ref bound-set normalized #f))
                     (hash-ref candidate-map normalized #f))
                v)]
           [`(module-variable-ref ,cell)
            (define normalized (normalize-rewritten-id cell candidate-map))
            (or (and (not (hash-ref bound-set normalized #f))
                     (hash-ref candidate-map normalized #f))
                v)]
           [`(module-variable-ref/no-check ,cell)
            (define normalized (normalize-rewritten-id cell candidate-map))
            (or (and (not (hash-ref bound-set normalized #f))
                     (hash-ref candidate-map normalized #f))
                v)]
           [`(#%top . ,id)
            (define normalized (normalize-rewritten-id id candidate-map))
            (or (and (not (hash-ref bound-set normalized #f))
                     (hash-ref candidate-map normalized #f))
                v)]
           [`(lambda ,args ,body ...)
            (define body-bound
              (extend-bound-set bound-set (id-list args) all-candidate-map))
            `(lambda ,args
               ,@(late-materialized-body-forms body
                                               all-candidate-map
                                               body-bound
                                               #:active-candidate-map candidate-map))]
           [`(case-lambda ,clauses ...)
            `(case-lambda
               ,@(for/list ([clause (in-list clauses)])
                   (match clause
                     [`(,args ,body ...)
                      (define body-bound
                        (extend-bound-set bound-set (id-list args) all-candidate-map))
                      `(,args
                        ,@(late-materialized-body-forms body
                                                        all-candidate-map
                                                        body-bound
                                                        #:active-candidate-map candidate-map))]
                     [_ clause])))]
           [`(let-values ,bindings ,body ...)
            (define body-bound
              (extend-bound-set bound-set
                                (apply append (map binding-ids bindings))
                                all-candidate-map))
           `(let-values
               ,(for/list ([binding (in-list bindings)])
                  (match binding
                    [(list ids rhs)
                     `(,ids
                       ,(rewrite rhs bound-set))]
                    [_ binding]))
               ,@(late-materialized-body-forms body
                                               all-candidate-map
                                               body-bound
                                               #:active-candidate-map candidate-map))]
           [`(letrec-values ,bindings ,body ...)
            (define body-bound
              (extend-bound-set bound-set
                                (apply append (map binding-ids bindings))
                                all-candidate-map))
           `(letrec-values
               ,(for/list ([binding (in-list bindings)])
                  (match binding
                    [(list ids rhs)
                     `(,ids
                       ,(rewrite rhs body-bound))]
                    [_ binding]))
               ,@(late-materialized-body-forms body
                                               all-candidate-map
                                               body-bound
                                               #:active-candidate-map candidate-map))]
           [`(letrec-syntaxes+values ,syntax-bindings ,value-bindings ,body ...)
            (define body-bound
              (extend-bound-set bound-set
                                (apply append (map binding-ids value-bindings))
                                all-candidate-map))
           `(letrec-syntaxes+values
               ,syntax-bindings
               ,(for/list ([binding (in-list value-bindings)])
                  (match binding
                    [(list ids rhs)
                     `(,ids
                       ,(rewrite rhs body-bound))]
                    [_ binding]))
               ,@(late-materialized-body-forms body
                                               all-candidate-map
                                               body-bound
                                               #:active-candidate-map candidate-map))]
           [`(begin ,forms ...)
            `(begin
               ,@(late-materialized-body-forms forms
                                               all-candidate-map
                                               bound-set
                                               #:active-candidate-map candidate-map))]
           [`(begin0 ,head ,rest ...)
            `(begin0 ,(rewrite head bound-set)
                     ,@(late-materialized-body-forms rest
                                                     all-candidate-map
                                                     bound-set
                                                     #:active-candidate-map candidate-map))]
           [`(if ,test ,then ,else)
            `(if ,(rewrite test bound-set)
                 ,(rewrite then bound-set)
                 ,(rewrite else bound-set))]
           [`(set! ,id ,rhs)
            `(set! ,id ,(rewrite rhs bound-set))]
           [`(variable-set! ,cell ,rhs)
            `(variable-set! ,cell ,(rewrite rhs bound-set))]
           [`(variable-set!/check-undefined ,cell ,rhs)
            `(variable-set!/check-undefined ,cell ,(rewrite rhs bound-set))]
           [`(module-variable-set! ,cell ,rhs)
            `(module-variable-set! ,cell ,(rewrite rhs bound-set))]
           [`(module-variable-set!/check-undefined! ,cell ,rhs)
            `(module-variable-set!/check-undefined! ,cell ,(rewrite rhs bound-set))]
           [`(module-variable-define! ,cell ,rhs ,constance)
            (define normalized-cell
              (normalize-rewritten-id cell all-candidate-map))
            (define candidate
              (and (not (hash-ref bound-set normalized-cell #f))
                   (hash-ref all-candidate-map normalized-cell #f)))
            (if candidate
                `(begin
                   (set! ,candidate ,(rewrite rhs bound-set))
                   (void))
                `(module-variable-define! ,cell ,(rewrite rhs bound-set) ,constance))]
           [`(variable-set!/define ,cell ,rhs ,constance)
            (define normalized-cell
              (normalize-rewritten-id cell all-candidate-map))
            (define candidate
              (and (not (hash-ref bound-set normalized-cell #f))
                   (hash-ref all-candidate-map normalized-cell #f)))
            (if candidate
                `(begin
                   (set! ,candidate ,(rewrite rhs bound-set))
                   (void))
                `(variable-set!/define ,cell ,(rewrite rhs bound-set) ,constance))]
           [`(define ,id ,rhs)
            (define define-bound
              (extend-bound-set bound-set (if (symbol? id) (list id) '()) all-candidate-map))
            `(define ,id ,(rewrite rhs define-bound))]
           [`(define-values ,ids ,rhs)
            (define bound-ids (id-list ids))
            (define normalized-id
              (and (= (length bound-ids) 1)
                   (normalize-rewritten-id (car bound-ids) all-candidate-map)))
            (define candidate
              (and normalized-id
                   (not (hash-ref bound-set normalized-id #f))
                   (hash-ref candidate-map normalized-id #f)))
            (define define-bound
              (extend-bound-set bound-set bound-ids all-candidate-map))
            (if candidate
                `(begin
                   (set! ,candidate ,(rewrite rhs bound-set))
                   (void))
                `(define-values ,ids ,(rewrite rhs define-bound)))]
           [`(,head . ,rest)
            (cons (rewrite head bound-set)
                  (rewrite rest bound-set))]
           [_ v]))]
    [(vector? v)
     (list->vector (rewrite-list (vector->list v) bound-set))]
    [else
     v]))

;; late-materialized-body-forms : (listof any/c) hasheq? [hasheq?] [#:active-candidate-map hasheq?] -> (listof any/c)
;;   Rewrite one body context while hoisting candidate export definitions.
(define (late-materialized-body-forms body
                                      candidate-map
                                      [bound-set (hasheq)]
                                      #:active-candidate-map [active-candidate-map candidate-map])
  (let loop ([remaining body]
             [body-bound-set bound-set]
             [active-map active-candidate-map]
             [acc '()])
    (cond
      [(null? remaining) (reverse acc)]
      [else
       (define form (car remaining))
       (match form
         [`(define ,id
             (begin
               (module-variable-define! ,cell ,rhs ,_constance)
               (void)))
          (define normalized-cell
            (normalize-rewritten-id cell candidate-map))
         (define candidate
            (and (not (hash-ref body-bound-set normalized-cell #f))
                 (hash-ref candidate-map normalized-cell #f)))
         (loop (cdr remaining)
                (if candidate
                    body-bound-set
                    (extend-bound-set body-bound-set (if (symbol? id) (list id) '()) candidate-map))
                (if candidate
                    (hash-set active-map normalized-cell candidate)
                    active-map)
                (cons (if candidate
                          `(begin
                             (set! ,candidate
                                   ,(rewrite-late-materialized-export rhs
                                                                      active-map
                                                                      body-bound-set
                                                                      #:all-candidate-map candidate-map))
                             (void))
                          `(define ,id
                             (begin
                               (module-variable-define!
                                ,cell
                                ,(rewrite-late-materialized-export rhs
                                                                   active-map
                                                                   body-bound-set
                                                                   #:all-candidate-map candidate-map)
                                ,_constance)
                               (void))))
                      acc))]
         [`(module-variable-define! ,cell ,rhs ,_constance)
          (define normalized-cell
            (normalize-rewritten-id cell candidate-map))
          (define candidate
            (and (not (hash-ref body-bound-set normalized-cell #f))
                 (hash-ref candidate-map normalized-cell #f)))
         (loop (cdr remaining)
                body-bound-set
                (if candidate
                    (hash-set active-map normalized-cell candidate)
                    active-map)
                (cons (if candidate
                          `(begin
                             (set! ,candidate
                                   ,(rewrite-late-materialized-export rhs
                                                                      active-map
                                                                      body-bound-set
                                                                      #:all-candidate-map candidate-map))
                             (void))
                          `(module-variable-define!
                            ,cell
                            ,(rewrite-late-materialized-export rhs
                                                               active-map
                                                               body-bound-set
                                                               #:all-candidate-map candidate-map)
                            ,_constance))
                      acc))]
         [`(variable-set!/define ,cell ,rhs ,_constance)
          (define normalized-cell
            (normalize-rewritten-id cell candidate-map))
          (define candidate
            (and (not (hash-ref body-bound-set normalized-cell #f))
                 (hash-ref candidate-map normalized-cell #f)))
         (loop (cdr remaining)
                body-bound-set
                (if candidate
                    (hash-set active-map normalized-cell candidate)
                    active-map)
                (cons (if candidate
                          `(begin
                             (set! ,candidate
                                   ,(rewrite-late-materialized-export rhs
                                                                      active-map
                                                                      body-bound-set
                                                                      #:all-candidate-map candidate-map))
                             (void))
                          `(variable-set!/define
                            ,cell
                            ,(rewrite-late-materialized-export rhs
                                                               active-map
                                                               body-bound-set
                                                               #:all-candidate-map candidate-map)
                            ,_constance))
                      acc))]
         [`(begin ,forms ...)
          (loop (cdr remaining)
                body-bound-set
                active-map
                (cons `(begin
                         ,@(late-materialized-body-forms forms
                                                        candidate-map
                                                        body-bound-set
                                                        #:active-candidate-map active-map))
                      acc))]
         [`(define ,id (,(and op (or 'module-variable-ref 'module-variable-ref/no-check)) ,cell))
          (define normalized-cell
            (normalize-rewritten-id cell candidate-map))
          (define candidate
            (and (not (hash-ref body-bound-set normalized-cell #f))
                 (hash-ref candidate-map normalized-cell #f)))
         (loop (cdr remaining)
                (extend-bound-set body-bound-set (if (symbol? id) (list id) '()) candidate-map)
                active-map
                (cons (if candidate
                          `(define ,id ,candidate)
                          `(define ,id
                             ,(rewrite-late-materialized-export
                               `(,op ,cell)
                               active-map
                               body-bound-set
                               #:all-candidate-map candidate-map)))
                      acc))]
         [`(define ,id ,rhs)
          (define normalized-id
            (normalize-rewritten-id id candidate-map))
          (define candidate
            (and (not (hash-ref body-bound-set normalized-id #f))
                 (hash-ref candidate-map normalized-id #f)))
          (define define-bound
            (extend-bound-set body-bound-set (if (symbol? id) (list id) '()) candidate-map))
          (loop (cdr remaining)
                (if candidate body-bound-set define-bound)
                (if candidate
                    (hash-set active-map normalized-id candidate)
                    active-map)
                (cons (if candidate
                          `(begin
                             (set! ,candidate
                                   ,(rewrite-late-materialized-export rhs
                                                                      active-map
                                                                      body-bound-set
                                                                      #:all-candidate-map candidate-map))
                             (void))
                          `(define ,id
                             ,(rewrite-late-materialized-export rhs
                                                                active-map
                                                                define-bound
                                                                #:all-candidate-map candidate-map)))
                      acc))]
         [`(define-values ,ids ,rhs)
          (define bound-ids
            (id-list ids))
          (define normalized-id
            (and (= (length bound-ids) 1)
                 (normalize-rewritten-id (car bound-ids) candidate-map)))
          (define candidate
            (and normalized-id
                 (not (hash-ref body-bound-set normalized-id #f))
                 (hash-ref candidate-map normalized-id #f)))
          (define define-bound
            (extend-bound-set body-bound-set bound-ids candidate-map))
          (loop (cdr remaining)
                (if candidate body-bound-set define-bound)
                (if candidate
                    (hash-set active-map normalized-id candidate)
                    active-map)
                (cons (if candidate
                          `(begin
                             (set! ,candidate
                                   ,(rewrite-late-materialized-export rhs
                                                                      active-map
                                                                      body-bound-set
                                                                      #:all-candidate-map candidate-map))
                             (void))
                          `(define-values ,ids
                             ,(rewrite-late-materialized-export rhs
                                                                active-map
                                                                define-bound
                                                                #:all-candidate-map candidate-map)))
                      acc))]
         [_
          (loop (cdr remaining)
                body-bound-set
                active-map
                (cons (rewrite-late-materialized-export form
                                                       active-map
                                                       body-bound-set
                                                       #:all-candidate-map candidate-map)
                      acc))])])))

;; late-materialize-exports : any/c (listof vector?) -> any/c
;;   Make candidate exports lexical inside the slice and return their values.
(define (late-materialize-exports expr candidate-specs)
  (if (null? candidate-specs)
      expr
      (match expr
        [`(lambda ,args ,body ...)
         (define candidate-map
           (for/hasheq ([spec (in-list candidate-specs)])
             (values (vector-ref spec 1)
                     (vector-ref spec 3))))
         `(lambda ,args
             ,@(for/list ([spec (in-list candidate-specs)])
                 `(define ,(vector-ref spec 3) (void)))
            ,@(late-materialized-body-forms body
                                           candidate-map
                                           #:active-candidate-map (hasheq))
            (values ,@(for/list ([spec (in-list candidate-specs)])
                        (vector-ref spec 3))))]
        [_ expr])))

;; strip-correlated : any/c -> any/c
;;   Remove correlated wrappers so compiler terms can become source datums.
(define (strip-correlated v)
  (let ([v (unwrap v)])
    (cond
      [(pair? v)   (cons (strip-correlated (car v))
                         (strip-correlated (cdr v)))]
      [(vector? v) (list->vector (map strip-correlated (vector->list v)))]
      [else        v])))

;; validate-top-level-exports! : symbol? any/c -> void?
;;   Reject export lists the current top-level adapter cannot represent safely.
(define (validate-top-level-exports! who exports)
  (unless (and (list? exports)
               (andmap symbol? exports))
    (raise-argument-error who "(listof symbol?)" exports))
  (define duplicate (check-duplicates exports))
  (when duplicate
    (raise-arguments-error who
                           "duplicate exports are not supported by the top-level adapter"
                           "export" duplicate)))

;; validate-first-slice-lambda! : symbol? any/c exact-nonnegative-integer? (listof symbol?) -> void?
;;   Reject lowered values that cannot be invoked with instance plus export cells.
(define (validate-first-slice-lambda! who expr import-count exports)
  (match expr
    [`(lambda ,args ,_body ...)
     (unless (list? args)
       (raise-arguments-error who
                              "expected lowered first-slice lambda to have fixed arity"
                              "form" expr))
     (unless (andmap symbol? args)
       (raise-arguments-error who
                              "expected lowered first-slice lambda formals to be symbols"
                              "formals" args))
     (define duplicate-arg (check-duplicates args))
     (when duplicate-arg
       (raise-arguments-error who
                              "duplicate lowered first-slice lambda formals are not supported"
                              "formal" duplicate-arg))
     (define expected-arity (+ 1 import-count (length exports)))
     (define actual-arity   (length args))
     (unless (= actual-arity expected-arity)
       (raise-arguments-error who
                              "lowered first-slice lambda arity does not match imports and exports"
                              "expected arity" expected-arity
                              "actual arity" actual-arity
                              "import count" import-count
                              "exports" exports))]
    [_
     (raise-arguments-error who
                            "expected lowered first-slice value to be a lambda"
                            "form" expr)]))

;; validate-importss! : symbol? any/c -> void?
;;   Reject grouped import names that cannot become lexical bindings.
(define (validate-importss! who importss)
  (unless (and (list? importss)
               (for/and ([group (in-list importss)])
                 (and (list? group)
                      (andmap symbol? group))))
    (raise-argument-error who "(listof (listof symbol?))" importss))
  (define duplicate (check-duplicates (flatten-groups importss)))
  (when duplicate
    (raise-arguments-error who
                           "duplicate import names are not supported"
                           "import" duplicate)))

;; validate-imports-abis! : symbol? any/c (listof (listof symbol?)) -> void?
;;   Reject import ABI metadata that does not match grouped import names.
(define (validate-imports-abis! who imports-abis importss)
  (define valid?
    (and (list? imports-abis)
         (= (length imports-abis) (length importss))
         (for/and ([group      (in-list imports-abis)]
                   [name-group (in-list importss)])
           (and (list? group)
                (= (length group) (length name-group))))))
  (unless valid?
    (raise-arguments-error who
                           "import ABI metadata does not match grouped imports"
                           "imports-abis" imports-abis
                           "imports" importss)))

;; default-imports-abis : (listof (listof symbol?)) -> (listof (listof #f))
;;   Treat every import as a cell-backed binding when no ABI metadata is present.
(define (default-imports-abis importss)
  (for/list ([group (in-list importss)])
    (for/list ([name (in-list group)])
      #f)))

;; bind-imports : any/c (listof (listof symbol?)) any/c exact-nonnegative-integer? -> any/c
;;   Introduce lexical imports from either direct values or module-variable cells.
(define (rewrite-direct-import-ref-forms v direct-import-args)
  (cond
    [(pair? v)
     (if (quote-like-form? v)
         v
         (match v
           [`(,(or 'variable-ref
                   'variable-ref/no-check
                   'module-variable-ref
                   'module-variable-ref/no-check)
              ,cell)
            (if (hash-ref direct-import-args cell #f)
                cell
                v)]
           [`(,head . ,rest)
            (cons (rewrite-direct-import-ref-forms head direct-import-args)
                  (rewrite-direct-import-ref-forms rest direct-import-args))]
           [_ v]))]
    [(vector? v)
     (list->vector
      (map (lambda (entry)
             (rewrite-direct-import-ref-forms entry direct-import-args))
           (vector->list v)))]
    [else
     v]))

(define (bind-imports expr importss imports-abis import-count)
  (validate-importss! 'bind-imports importss)
  (validate-imports-abis! 'bind-imports imports-abis importss)
  (define import-names (flatten-groups importss))
  (define import-abis  (flatten-groups imports-abis))
  (define direct-import-count
    (for/sum ([abi (in-list import-abis)])
      (if abi 1 0)))
  (define cell-import-count
    (- (length import-abis) direct-import-count))
  (webify-bridge-stats-inc! 'bridge-direct-import-count direct-import-count)
  (webify-bridge-stats-inc! 'bridge-import-cell-count cell-import-count)
  (webify-bridge-stats-inc! 'bridge-cell-elision-count direct-import-count)
  (unless (= (length import-names) import-count)
    (raise-arguments-error 'bind-imports
                           "import metadata count does not match lowered lambda"
                           "metadata count" (length import-names)
                           "lambda import count" import-count
                           "imports" importss))
  (match expr
    [`(lambda ,args ,body ...)
     (define import-args (take (cdr args) import-count))
     (define direct-import-args
       (for/hasheq ([arg (in-list import-args)]
                    [abi (in-list import-abis)]
                    #:when abi)
         (values arg #t)))
     (define duplicate (check-duplicates (append import-names args)))
     (when duplicate
       (raise-arguments-error 'bind-imports
                              "import name collides with generated lambda argument"
                              "name" duplicate))
     `(lambda ,args
        ,@(for/list ([name (in-list import-names)]
                     [arg  (in-list import-args)]
                     [abi  (in-list import-abis)])
            `(define ,name ,(if abi
                                arg
                                `(module-variable-ref ,arg))))
        ,@(map (lambda (form)
                 (rewrite-direct-import-ref-forms form direct-import-args))
               body))]
    [_
     (raise-arguments-error 'bind-imports
                            "expected lowered first-slice value to be a lambda"
                            "form" expr)]))

;; fresh-internal-id : symbol? (listof symbol?) -> symbol?
;;   Build a deterministic helper name that does not collide with exports.
(define (fresh-internal-id base reserved)
  (let loop ([n 0])
    (define candidate
      (string->symbol (format "~a$~a" base n)))
    (if (memq candidate reserved)
        (loop (add1 n))
        candidate)))

;; datum-symbols : any/c -> (listof symbol?)
;;   Collect symbols already present in one lowered slice datum.
;;   Shared or cyclic pair/vector structure is traversed at most once.
(define (datum-symbols datum)
  (define seen-composites (make-hasheq))
  (define seen-symbols (make-hasheq))
  (define (seen-composite!? v)
    (cond
      [(or (pair? v) (vector? v))
       (cond
         [(hash-ref seen-composites v #f) #t]
         [else
          (hash-set! seen-composites v #t)
          #f])]
      [else #f]))
  (define (walk v)
    (cond
      [(symbol? v)
       (hash-set! seen-symbols v #t)]
      [(pair? v)
       (if (seen-composite!? v)
           (void)
           (begin
             (walk (car v))
             (walk (cdr v))))]
      [(vector? v)
       (if (seen-composite!? v)
           (void)
           (for ([entry (in-vector v)])
             (walk entry)))]
      [else
       (void)]))
  (walk datum)
  (hash-keys seen-symbols))

;; generated-linklet-proc-id : (listof symbol?) -> symbol?
;;   Build an internal binding name for the lowered linklet procedure.
(define (generated-linklet-proc-id reserved)
  (fresh-internal-id 'webify-linklet-proc reserved))

;; generated-cell-id : exact-nonnegative-integer? (listof symbol?) -> symbol?
;;   Build an internal cell binding name for an export argument.
(define (generated-cell-id index reserved)
  (fresh-internal-id (string->symbol (format "webify-export-cell~a" index))
                     reserved))

;; generated-value-id : exact-nonnegative-integer? (listof symbol?) -> symbol?
;;   Build an internal lexical binding name for one exported value.
(define (generated-value-id index reserved)
  (fresh-internal-id (string->symbol (format "webify-export-value~a" index))
                     reserved))

;; generated-candidate-local-id : exact-nonnegative-integer? (listof symbol?) -> symbol?
;;   Build a collision-resistant local cache id for late-materialized exports.
(define (generated-candidate-local-id index reserved)
  (fresh-internal-id (string->symbol (format "webify-late-export-local-value~a" index))
                     reserved))

;; validate-reserved-ids! : symbol? any/c -> void?
;;   Reject reserved-name lists that cannot be used for deterministic avoidance.
(define (validate-reserved-ids! who reserved)
  (unless (and (list? reserved)
               (andmap symbol? reserved))
    (raise-argument-error who "(listof symbol?)" reserved)))

;; flatten-groups : (listof list?) -> list?
;;   Flatten grouped import metadata while preserving group order.
(define (flatten-groups groups)
  (apply append groups))

;; first-slice-import-args : (listof (listof symbol?)) (listof hash?) -> (listof any/c)
;;   Resolve grouped import metadata to the flat explicit-import argument order.
(define (first-slice-import-args importss import-envs)
  (define who 'first-slice-import-args)
  (unless (and (list? importss)
               (for/and ([group (in-list importss)])
                 (and (list? group)
                      (andmap symbol? group))))
    (raise-argument-error who "(listof (listof symbol?))" importss))
  (unless (and (list? import-envs)
               (andmap hash? import-envs))
    (raise-argument-error who "(listof hash?)" import-envs))
  (unless (= (length importss) (length import-envs))
    (raise-arguments-error who
                           "import argument environment count does not match import groups"
                           "expected count" (length importss)
                           "actual count" (length import-envs)
                           "imports" importss))
  (apply
   append
   (for/list ([imports (in-list importss)]
              [env     (in-list import-envs)])
     (for/list ([import (in-list imports)])
       (hash-ref env
                 import
                 (lambda ()
                   (raise-arguments-error who
                                          "missing import argument"
                                          "import" import
                                          "available imports" (hash-keys env))))))))

;; first-slice-import-cell-args : (listof (listof symbol?)) (listof hash?) -> (listof any/c)
;;   Backward-compatible alias for explicit import argument resolution.
(define first-slice-import-cell-args
  first-slice-import-args)

;; first-slice-top-level-helper-ids : (listof symbol?) [#:reserved (listof symbol?)] -> (listof symbol?)
;;   Return the internal helper ids that the adapter would reserve for exports.
(define (first-slice-top-level-helper-ids exports #:reserved [reserved '()])
  (define who 'first-slice-top-level-helper-ids)
  (validate-top-level-exports! who exports)
  (validate-reserved-ids! who reserved)
  (define reserved* (append exports reserved))
  (define proc-id (generated-linklet-proc-id reserved*))
  (define cell-ids
    (for/list ([_export (in-list exports)]
               [index   (in-naturals)])
      (generated-cell-id index (append (list proc-id) reserved*))))
  (cons proc-id cell-ids))

;; first-slice-top-level-export-cell-ids : (listof symbol?) [#:reserved (listof symbol?)] -> hasheq?
;;   Map export names to the internal cell ids that the adapter would create.
(define (first-slice-top-level-export-cell-ids exports #:reserved [reserved '()])
  (define cell-ids (cdr (first-slice-top-level-helper-ids exports #:reserved reserved)))
  (for/hasheq ([export  (in-list exports)]
               [cell-id (in-list cell-ids)])
    (values export cell-id)))

;; first-slice-top-level-export-value-ids : (listof symbol?) [#:reserved (listof symbol?)] -> hasheq?
;;   Map export names to internal lexical bindings that cache exported values.
(define (first-slice-top-level-export-value-ids exports #:reserved [reserved '()])
  (define helper-ids (first-slice-top-level-helper-ids exports #:reserved reserved))
  (define reserved* (append exports reserved helper-ids))
  (define value-ids
    (for/list ([_export (in-list exports)]
               [index   (in-naturals)])
      (generated-value-id index reserved*)))
  (for/hasheq ([export   (in-list exports)]
               [value-id (in-list value-ids)])
    (values export value-id)))

;; build-first-slice-export-plan : any/c (listof symbol?) keyword-args -> first-slice-export-plan?
;;   Analyze one lowered first slice and compute the mixed public export plan.
(define (build-first-slice-export-plan expr
                                       exports
                                       #:import-count [import-count 0]
                                       #:reserved [reserved '()]
                                       #:module-name [module-name #f]
                                       #:importss [importss #f]
                                       #:imports-abis [imports-abis #f]
                                       #:forced-cell-exports [forced-cell-exports (hasheq)])
  (validate-top-level-exports! 'build-first-slice-export-plan exports)
  (validate-reserved-ids! 'build-first-slice-export-plan reserved)
  (unless (exact-nonnegative-integer? import-count)
    (raise-argument-error 'build-first-slice-export-plan "exact-nonnegative-integer?" import-count))
  (unless (hash? forced-cell-exports)
    (raise-argument-error 'build-first-slice-export-plan "hash?" forced-cell-exports))
  (define clean-expr (strip-correlated expr))
  (validate-first-slice-lambda! 'build-first-slice-export-plan clean-expr import-count exports)
  (define adapted-expr
    (if importss
        (bind-imports clean-expr
                      importss
                      (or imports-abis
                          (default-imports-abis importss))
                      import-count)
        clean-expr))
  (define helper-ids (first-slice-top-level-helper-ids exports #:reserved reserved))
  (define cell-id-map (first-slice-top-level-export-cell-ids exports #:reserved reserved))
  (define value-id-map (first-slice-top-level-export-value-ids exports #:reserved reserved))
  (define value-ids
    (for/list ([export (in-list exports)])
      (hash-ref value-id-map export)))
  (define adapted-expr-symbols
    (datum-symbols adapted-expr))
  (define candidate-local-ids
    (for/list ([export (in-list exports)]
               [index (in-naturals)])
      (generated-candidate-local-id
       index
       (append reserved
               exports
               helper-ids
               value-ids
               adapted-expr-symbols
               (list export)))))
  (define export-args
    (match adapted-expr
      [`(lambda ,args ,_body ...)
       (take-right args (length exports))]))
  (define candidate-specs
    (late-materializable-export-specs adapted-expr
                                      exports
                                      export-args
                                      value-ids
                                      candidate-local-ids
                                      importss))
  (define metas
    (export-arg-metadata-list adapted-expr exports export-args module-name importss))
  (define entry-map
    (for/hasheq ([export (in-list exports)]
                 [meta (in-list metas)])
      (define cell-id (hash-ref cell-id-map export))
      (define value-id (hash-ref value-id-map export))
      (define selected-kind
        (export-entry-kind meta forced-cell-exports))
      (define enriched-meta
        (hash-set* meta
                   'cell-id cell-id
                   'value-id value-id
                   'provider-kind (module-provider-kind module-name)
                   'plain-value-base-candidate? (plain-value-export-base-candidate? meta)
                   'plain-value-policy-eligible? (plain-value-export-policy-eligible? meta)
                   'forced-cell? (and export
                                      (hash-ref forced-cell-exports export #f)
                                      #t)
                   'selected-export-kind selected-kind))
      (values export
              (bridge-export-entry
               export
               selected-kind
               (if (eq? selected-kind 'value)
                   value-id
                   cell-id)
               enriched-meta))))
  (first-slice-export-plan adapted-expr
                           helper-ids
                           (cdr helper-ids)
                           value-id-map
                           value-ids
                           export-args
                           candidate-specs
                           entry-map))

;; export-alias-enabled? : any/c (or/c symbol? #f) symbol? -> boolean?
;;   Decide whether to define a top-level alias for one module export.
(define (export-alias-enabled? mode module-name export)
  (cond
    [(procedure? mode)
     (and (if (procedure-arity-includes? mode 2)
              (mode module-name export)
              (mode export))
          #t)]
    [else
     (and mode #t)]))

;; first-slice->top-level-datum : any/c (listof symbol?) -> any/c
;;   Wrap a lowered import-free first-slice lambda in top-level forms.
(define (first-slice->top-level-datum expr
                                      exports
                                      #:reserved [reserved '()]
                                      #:module-name [module-name #f]
                                      #:export-entries [export-entries #f]
                                      #:define-export-aliases? [define-export-aliases? #t])
  (first-slice->top-level-datum/imports
   expr
   '()
   exports
   #:reserved reserved
   #:module-name module-name
   #:export-entries export-entries
   #:define-export-aliases? define-export-aliases?))

;; first-slice->top-level-datum/imports : any/c (listof any/c) (listof symbol?) -> any/c
;;   Wrap a lowered first-slice lambda with explicit import argument expressions.
(define (first-slice->top-level-datum/imports expr
                                             import-args
                                             exports
                                             #:reserved [reserved '()]
                                             #:module-name [module-name #f]
                                             #:importss [importss #f]
                                             #:imports-abis [imports-abis #f]
                                             #:export-entries [export-entries #f]
                                             #:define-export-aliases? [define-export-aliases? #t])
  (unless (list? import-args)
    (raise-argument-error 'first-slice->top-level-datum/imports "list?" import-args))
  (define plan
    (build-first-slice-export-plan expr
                                   exports
                                   #:import-count (length import-args)
                                   #:reserved reserved
                                   #:module-name module-name
                                   #:importss importss
                                   #:imports-abis imports-abis))
  (define adapted-expr
    (first-slice-export-plan-adapted-expr plan))
  (define helper-ids
    (first-slice-export-plan-helper-ids plan))
  (define proc-id (car helper-ids))
  (define cell-ids
    (first-slice-export-plan-cell-ids plan))
  (define value-id-map
    (first-slice-export-plan-value-id-map plan))
  (define value-ids
    (first-slice-export-plan-value-ids plan))
  (define export-args
    (first-slice-export-plan-export-args plan))
  (define raw-candidate-specs
    (first-slice-export-plan-candidate-specs plan))
  (define chosen-export-entries
    (or export-entries
        (first-slice-export-plan-entry-map plan)))
  (define import-arg-symbols
    (for/hasheq ([sym (in-list (datum-symbols import-args))])
      (values sym #t)))
  (define (candidate-import-dependent? spec)
    (define export (vector-ref spec 0))
    (define entry (hash-ref chosen-export-entries export))
    (define meta (bridge-export-entry-metadata entry))
    (define cell-id (hash-ref meta 'cell-id #f))
    (define value-id (hash-ref meta 'value-id #f))
    (or (hash-ref import-arg-symbols (bridge-export-entry-binding entry) #f)
        (and cell-id
             (hash-ref import-arg-symbols cell-id #f))
        (and value-id
             (hash-ref import-arg-symbols value-id #f))))
  (define candidate-specs
    (filter (lambda (spec)
              (not (candidate-import-dependent? spec)))
            raw-candidate-specs))
  (define candidate-export-args
    (for/list ([spec (in-list candidate-specs)])
      (vector-ref spec 1)))
  (define candidate-value-ids
    (for/list ([spec (in-list candidate-specs)])
      (vector-ref spec 2)))
  (define candidate-local-ids
    (for/list ([spec (in-list candidate-specs)])
      (vector-ref spec 3)))
  (define candidate-constances
    (for/list ([spec (in-list candidate-specs)])
      (vector-ref spec 4)))
  (define candidate-arg-set
    (for/hasheq ([arg (in-list candidate-export-args)])
      (values arg #t)))
  (define cell-backed-export-arg-replacements
    (for/hasheq ([export     (in-list exports)]
                 [export-arg (in-list export-args)]
                 #:when (and (not (hash-ref candidate-arg-set export-arg #f))
                             (bridge-export-entry-cell-backed?
                              (hash-ref chosen-export-entries export))))
      (values export-arg
              (bridge-export-entry-binding
               (hash-ref chosen-export-entries export)))))
  (define late-adapted-expr
    (rewrite-first-slice-export-args
     (late-materialize-exports adapted-expr candidate-specs)
     cell-backed-export-arg-replacements))
  (define cell-backed-exports
    (for/list ([export (in-list exports)]
               #:when (bridge-export-entry-cell-backed?
                       (hash-ref chosen-export-entries export)))
      export))
  (define value-backed-exports
    (for/list ([export (in-list exports)]
               #:when (bridge-export-entry-plain-value?
                       (hash-ref chosen-export-entries export)))
      export))
  (webify-bridge-stats-inc! 'bridge-export-cell-count (length cell-backed-exports))
  (webify-bridge-stats-inc! 'bridge-module-variable-make-count (length cell-backed-exports))
  (webify-bridge-stats-inc! 'bridge-export-cell-late-materialization-candidate-count
                            (length candidate-specs))
  (webify-bridge-stats-inc! 'bridge-public-cell-elided-count (length value-backed-exports))
  (webify-bridge-stats-inc! 'bridge-public-cell-kept-count (length cell-backed-exports))
  (when (current-module-bridge-elide-public-cells?)
    (for ([export (in-list exports)])
      (define entry (hash-ref chosen-export-entries export))
      (when (bridge-export-entry-cell-backed? entry)
        (define meta (bridge-export-entry-metadata entry))
        (define required?
          (or (not (plain-value-export-candidate? meta))
              (bridge-export-entry-plain-value?
               (hash-ref (first-slice-export-plan-entry-map plan) export))))
        (when required?
          (webify-bridge-stats-inc! 'bridge-public-cell-kept-required-count))
        (cond
          [(hash-ref meta 'reflection-visible? #f)
           (webify-bridge-stats-inc! 'bridge-public-cell-kept-reflection-count)]
          [(hash-ref meta 'dynamic-require-sensitive? #f)
           (webify-bridge-stats-inc! 'bridge-public-cell-kept-dynamic-require-count)]
          [(or (not (hash-ref meta 'immutable? #f))
               (hash-ref meta 'mutated? #f))
           (webify-bridge-stats-inc! 'bridge-public-cell-kept-mutation-count)]
         [(hash-ref meta 'undefined-sensitive? #f)
           (webify-bridge-stats-inc! 'bridge-public-cell-kept-undefined-count)]
         [(hash-ref meta 'read-before-definition? #f)
          (webify-bridge-stats-inc! 'bridge-public-cell-kept-undefined-count)]
         [(hash-ref meta 'reexport? #f)
          (webify-bridge-stats-inc! 'bridge-public-cell-kept-reexport-count)]
          [(not (hash-ref meta 'phase0-value-space-only? #f))
           (webify-bridge-stats-inc! 'bridge-public-cell-kept-phase-space-count)]
          [(plain-value-export-candidate? meta)
           (webify-bridge-stats-inc! 'bridge-public-cell-kept-unknown-count)]
          [else
           (webify-bridge-stats-inc! 'bridge-public-cell-kept-unknown-count)]))))
  (record-export-cell-stats! adapted-expr
                            exports
                            export-args
                            module-name
                            importss
                            #:export-entries chosen-export-entries)
  `(begin
     (define ,proc-id ,late-adapted-expr)
     ,@(for/list ([export     (in-list exports)]
                  [export-arg (in-list export-args)]
                  #:when (and (bridge-export-entry-cell-backed?
                               (hash-ref chosen-export-entries export))
                              (not (hash-ref candidate-arg-set export-arg #f))))
         (define entry (hash-ref chosen-export-entries export))
         (define cell-id (bridge-export-entry-binding entry))
         `(define ,cell-id (module-variable-make ',export)))
     ,(if (null? candidate-value-ids)
          `(,proc-id (#%variable-reference)
                     ,@import-args
                     ,@(for/list ([export     (in-list exports)]
                                  [export-arg (in-list export-args)])
                         (define entry (hash-ref chosen-export-entries export))
                         (if (hash-ref candidate-arg-set export-arg #f)
                             '(void)
                             (bridge-export-entry-binding entry))))
          `(define-values ,candidate-value-ids
             (,proc-id (#%variable-reference)
                       ,@import-args
                       ,@(for/list ([export     (in-list exports)]
                                    [export-arg (in-list export-args)])
                           (define entry (hash-ref chosen-export-entries export))
                           (if (hash-ref candidate-arg-set export-arg #f)
                               '(void)
                               (bridge-export-entry-binding entry))))))
     ,@(for/list ([spec (in-list candidate-specs)])
         (define export (vector-ref spec 0))
         (define value-id (vector-ref spec 2))
         (define constance (vector-ref spec 4))
         (define entry (hash-ref chosen-export-entries export))
         (and (bridge-export-entry-cell-backed? entry)
              `(begin
                 (define ,(bridge-export-entry-binding entry) (module-variable-make ',export))
                 (module-variable-define! ,(bridge-export-entry-binding entry) ,value-id ',constance))))
     ,@(filter values
               (for/list ([export (in-list exports)]
                          [value-id  (in-list value-ids)]
                          [export-arg (in-list export-args)]
                          #:unless (hash-ref candidate-arg-set export-arg #f))
                 (define entry (hash-ref chosen-export-entries export))
                 (and (bridge-export-entry-cell-backed? entry)
                      `(define ,value-id (module-variable-ref ,(bridge-export-entry-binding entry))))))
     ,@(for/list ([export  (in-list exports)]
                  [value-id (in-list value-ids)]
                  #:when (export-alias-enabled? define-export-aliases?
                                                  module-name
                                                  export))
         `(define ,export ,value-id))
     (void)))

;; first-slice->top-level-syntax : any/c (listof symbol?) keyword-args -> syntax?
;;   Convert a lowered first-slice expression to top-level syntax for `comp`.
(define (first-slice->top-level-syntax expr
                                       exports
                                       #:reserved [reserved '()]
                                       #:define-export-aliases? [define-export-aliases? #t])
  (datum->syntax
   #f
   (first-slice->top-level-datum
    expr
    exports
    #:reserved reserved
    #:define-export-aliases? define-export-aliases?)))

;; webify-linklet-first-slice-top-level/imports : linklet (listof any/c) keyword-args ... -> syntax?
;;   Webify, lower, and wrap a first-slice linklet with explicit import cells.
(define (webify-linklet-first-slice-top-level/imports lk
                                                       import-args
                                                       #:reserved             [reserved '()]
                                                       #:define-export-aliases? [define-export-aliases? #t]
                                                       #:serializable?-box     serializable?-box
                                                       #:datum-intern?         datum-intern?
                                                       #:target                target
                                                       #:allow-set!-undefined? allow-set!-undefined?
                                                       #:unsafe-mode?          unsafe-mode?
                                                       #:enforce-constant?     enforce-constant?
                                                       #:allow-inline?         allow-inline?
                                                       #:no-prompt?            no-prompt?
                                                       #:prim-knowns           prim-knowns
                                                       #:primitives            primitives
                                                       #:compiler-query        compiler-query
                                                       #:get-import-knowns     get-import-knowns
                                                       #:import-keys           import-keys)
  (unless (list? import-args)
    (raise-argument-error 'webify-linklet-first-slice-top-level/imports
                          "list?"
                          import-args))
  (define-values (expr importss exports new-import-keys imports-abis exports-info)
    (webify-linklet-first-slice
     lk
     #:serializable?-box     serializable?-box
     #:datum-intern?         datum-intern?
     #:target                target
     #:allow-set!-undefined? allow-set!-undefined?
     #:unsafe-mode?          unsafe-mode?
     #:enforce-constant?     enforce-constant?
     #:allow-inline?         allow-inline?
     #:no-prompt?            no-prompt?
     #:prim-knowns           prim-knowns
     #:primitives            primitives
     #:compiler-query        compiler-query
     #:get-import-knowns     get-import-knowns
     #:import-keys           import-keys))
  (when new-import-keys
    (error 'webify-linklet-first-slice-top-level/imports
           "inlining-added import groups are not supported by the explicit-import adapter yet"))
  (define import-count (length (flatten-groups importss)))
  (unless (= import-count (length import-args))
    (raise-arguments-error 'webify-linklet-first-slice-top-level/imports
                           "explicit import cell count does not match linklet imports"
                           "expected count" import-count
                           "actual count" (length import-args)
                           "imports" importss))
  (values (datum->syntax
           #f
           (first-slice->top-level-datum/imports
            expr
            import-args
            exports
            #:reserved reserved
            #:importss importss
            #:imports-abis imports-abis
            #:define-export-aliases? define-export-aliases?))
          importss
          exports
          exports-info))

;; webify-linklet-first-slice-top-level : linklet keyword-args ... -> syntax?
;;   Webify, lower, and wrap an import-free first-slice linklet as top-level forms.
(define (webify-linklet-first-slice-top-level lk
                                              #:define-export-aliases? [define-export-aliases? #t]
                                              #:serializable?-box     serializable?-box
                                              #:datum-intern?         datum-intern?
                                              #:target                target
                                              #:allow-set!-undefined? allow-set!-undefined?
                                              #:unsafe-mode?          unsafe-mode?
                                              #:enforce-constant?     enforce-constant?
                                              #:allow-inline?         allow-inline?
                                              #:no-prompt?            no-prompt?
                                              #:prim-knowns           prim-knowns
                                              #:primitives            primitives
                                              #:compiler-query        compiler-query
                                              #:get-import-knowns     get-import-knowns
                                              #:import-keys           import-keys)
  (define-values (expr importss exports new-import-keys imports-abis exports-info)
    (webify-linklet-first-slice
     lk
     #:serializable?-box     serializable?-box
     #:datum-intern?         datum-intern?
     #:target                target
     #:allow-set!-undefined? allow-set!-undefined?
     #:unsafe-mode?          unsafe-mode?
     #:enforce-constant?     enforce-constant?
     #:allow-inline?         allow-inline?
     #:no-prompt?            no-prompt?
     #:prim-knowns           prim-knowns
     #:primitives            primitives
     #:compiler-query        compiler-query
     #:get-import-knowns     get-import-knowns
     #:import-keys           import-keys))
  (unless (and (null? importss)
               (not new-import-keys)
               (null? imports-abis))
    (error 'webify-linklet-first-slice-top-level
           "imported linklets are not supported by the first top-level adapter yet"))
  (values (first-slice->top-level-syntax
           expr
           exports
           #:define-export-aliases? define-export-aliases?)
          exports
          exports-info))
