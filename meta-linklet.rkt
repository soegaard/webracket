#lang racket
(require (only-in racket/unsafe/undefined
                  check-not-unsafe-undefined
                  unsafe-undefined))

(require (only-in racket/linklet
                  correlated?
                  correlated-e
                  correlated-source
                  correlated-line
                  correlated-column
                  correlated-position
                  correlated-span
                  correlated-property-symbol-keys
                  correlated-property
                  correlated->datum
                  datum->correlated))


(require (for-syntax racket/base))

;;;
;;; ... src/racket/expander/common/make-match.rkt
;;;

;; Yet another pattern matcher along the lines of `syntax-rules`, but
;; intended for relatively simple and small patterns.
;;
;; The `define-match` form generated here has the following syntax to
;; match the result of <s-expr> against <pattern>:
;;
;;  (define-match <m-id> <s-expr> <guard> <try> '<pattern>)
;;
;;   <guard> = <epsilon> | #:when <expr> | #:unless <expr>
;;   <try>   = <epsilon> | #:try
;;
;;   <pattern> = <id>      ; matches anything
;;             | id:<id>   ; matches only identifiers
;;             | (<pattern> ...)  ; zero or more
;;             | (<pattern> ...+) ; one or more
;;             | (<pattern> . <pattern>)
;;
;; Note that the ' before <pattern> doesn't produce a symbol or list;
;; it's just a literal to textually highlight the pattern.
;;
;; The <m-id> bound by `define-match` is used as either
;;   
;;   (<m-id>)
;;
;; to check whether the match succeeded (which makes sense only if a
;; guard or `#:try` is included) or
;;
;;    (<m-id> '<pattern-id>)
;;
;; to access the value for a match. Again, the ' here does not produce
;; a symbol, but serves only as visual highlighting.
;;
;; Unlike `syntax-rules`/`syntax-case`/`syntax-parse`, there's no
;; template system and no help in making sure your uses of variables
;; before `...` expect the right shape. For example, with
;;
;;   (define-match m s '(a ...))
;;
;; then `(m 'a)` will always produce a list of matches of `a`.
;;
;; If a pattern doesn't match and there's no `#:try`, then a syntax
;; error is reported.
;;
;; The `define-define-match` form is a macro-generating macro so that
;; it can be used with different underlying notions of syntax, as
;; specific by the `rt-syntax?`, etc., macro arguments.

(define-syntax-rule (define-define-match define-match
                      rt-syntax? rt-syntax-e rt-raise-syntax-error)
  (...
   (begin
     (define-for-syntax (extract-pattern-ids pattern)
       (cond
        [(identifier? pattern)
         (if (or (eq? '... (syntax-e pattern))
                 (eq? '...+ (syntax-e pattern)))
             null
             (list pattern))]
        [(symbol? pattern)
         (if (or (eq? '... pattern)
                 (eq? '...+ pattern))
             null
             (list pattern))]
        [(syntax? pattern) (extract-pattern-ids (syntax-e pattern))]
        [(pair?  pattern)
         (append (extract-pattern-ids (car pattern))
                 (extract-pattern-ids (cdr pattern)))]
        [else null]))
     
     ;; This pattern compiler has bad time complexity for complex
     ;; patterns, because it keeps recomputing the set of pattern
     ;; variables, but we're only going to use it on simple patterns
     
     (define-for-syntax (identifier-pattern? pattern)
       (regexp-match? #rx"^id(:|$)" (symbol->string pattern)))
     
     (define-for-syntax (compile-pattern pattern already-checked?)
       (cond
        [(symbol? pattern)
         (if (identifier-pattern? pattern)
             (if already-checked?
                 #'s
                 #`(if (or (and (rt-syntax? s)
                                (symbol? (rt-syntax-e s)))
                           (symbol? s))
                       s
                       (rt-raise-syntax-error #f "not an identifier" orig-s s)))
             #'s)]
        [else
         #`(let ([s (if (rt-syntax? s) (rt-syntax-e s) s)])
             #,(cond
                [(and (list? pattern)
                      (= (length pattern) 2)
                      (or (eq? '... (cadr pattern))
                          (eq? '...+ (cadr pattern))))
                 (with-syntax ([(pattern-id ...) (extract-pattern-ids (car pattern))])
                   #`(let ([flat-s (to-syntax-list s)])
                       (cond
                        [#,(if already-checked? #'#f #'(not flat-s))
                         (rt-raise-syntax-error #f "bad syntax" orig-s)]
                        [#,(if (and (eq? '...+ (cadr pattern)) (not already-checked?)) #'(null? flat-s) #'#f)
                         (rt-raise-syntax-error #f "bad syntax" orig-s)]
                        [else
                         #,(if (and (symbol? (car pattern))
                                    (or (not (identifier-pattern? (car pattern)))
                                        already-checked?))
                               #`flat-s
                               #`(for/lists (pattern-id ...) ([s (in-list flat-s)])
                                            #,(compile-pattern (car pattern) already-checked?)))])))]
                [(pair? pattern)
                 (with-syntax ([(a-pattern-id ...) (generate-temporaries (extract-pattern-ids (car pattern)))]
                               [(d-pattern-id ...) (generate-temporaries (extract-pattern-ids (cdr pattern)))])
                   #`(if #,(if already-checked? #'#t #'(pair? s))
                         (let-values ([(a-pattern-id ...) (let ([s (car s)]) #,(compile-pattern (car pattern)
                                                                                                 already-checked?))]
                                      [(d-pattern-id ...) (let ([s (cdr s)]) #,(compile-pattern (cdr pattern)
                                                                                                 already-checked?))])
                           (values a-pattern-id ... d-pattern-id ...))
                         (rt-raise-syntax-error #f "bad syntax" orig-s)))]
                [(null? pattern)
                 (if already-checked?
                     #'(values)
                     #'(if (null? s)
                           (values)
                           (rt-raise-syntax-error #f "bad syntax" orig-s)))]
                [(or (keyword? pattern)
                     (boolean? pattern))
                 (if already-checked?
                     #'(values)
                     #`(if (eq? '#,pattern s)
                           (values)
                           (rt-raise-syntax-error #f "bad syntax" orig-s)))]
                [else
                 (raise-syntax-error 'define-match "bad pattern" pattern)]))]))
     
     (define-for-syntax (compile-pattern-check pattern)
       (cond
        [(symbol? pattern)
         (if (identifier-pattern? pattern)
             #`(or (and (rt-syntax? s)
                        (symbol? (rt-syntax-e s)))
                   (symbol? s))
             #'#t)]
        [else
         #`(let ([s (if (rt-syntax? s) (rt-syntax-e s) s)])
             #,(cond
                [(and (list? pattern)
                      (= (length pattern) 2)
                      (or (eq? '... (cadr pattern))
                          (eq? '...+ (cadr pattern))))
                 (with-syntax ([(pattern-id ...) (extract-pattern-ids (car pattern))])
                   #`(let ([flat-s (to-syntax-list s)])
                       (cond
                        [(not flat-s) #f]
                        [#,(if (eq? '...+ (cadr pattern)) #'(null? flat-s) #'#f) #f]
                        [else #,(if (and (symbol? (car pattern))
                                         (not (identifier-pattern? (car pattern))))
                                    #`#t
                                    #`(for/and ([s (in-list flat-s)])
                                        #,(compile-pattern-check (car pattern))))])))]
                [(pair? pattern)
                 (with-syntax ([(a-pattern-id ...) (extract-pattern-ids (car pattern))]
                               [(d-pattern-id ...) (extract-pattern-ids (cdr pattern))])
                   #`(and (pair? s)
                          (let ([s (car s)]) #,(compile-pattern-check (car pattern)))
                          (let ([s (cdr s)]) #,(compile-pattern-check (cdr pattern)))))]
                [(null? pattern)
                 #'(null? s)]
                [(or (keyword? pattern)
                     (boolean? pattern))
                 #`(eq? '#,pattern s)]
                [else
                 (raise-syntax-error 'define-match "bad pattern" pattern)]))]))
     
     (define (to-syntax-list s)
       (cond
        [(list? s) s]
        [(pair? s)
         (define r (to-syntax-list (cdr s)))
         (and r (cons (car s) r))]
        [(rt-syntax? s) (to-syntax-list (rt-syntax-e s))]
        [else #f]))
     
     (define-syntax (define-match stx)
       (syntax-case stx (quote)
         [(_ id expr 'pattern)
          #'(do-define-match id expr 'pattern #:when #t #:try? #f)]
         [(_ id expr #:try 'pattern)
          #'(do-define-match id expr 'pattern #:when #t #:try? #t)]
         [(_ id expr #:when guard-expr 'pattern)
          #'(do-define-match id expr 'pattern #:when guard-expr #:try? #f)]
         [(_ id expr #:when guard-expr #:try 'pattern)
          #'(do-define-match id expr 'pattern #:when guard-expr #:try? #t)]
         [(_ id expr #:unless guard-expr 'pattern)
          #'(do-define-match id expr 'pattern #:when (not guard-expr) #:try? #f)]
         [(_ id expr #:unless guard-expr #:try 'pattern)
          #'(do-define-match id expr 'pattern #:when (not guard-expr) #:try? #t)]))

     (define-syntax (do-define-match stx)
       (syntax-case stx (quote)
         [(_ id expr 'pattern #:when guard-expr #:try? try?)
          (let ([pattern-ids (extract-pattern-ids #'pattern)]
                [try? (syntax-e #'try?)])
            (with-syntax ([(pattern-id ...) pattern-ids]
                          [(pattern-result-id ...) (generate-temporaries pattern-ids)]
                          [(false-result ...) (map (lambda (x) #'#f) pattern-ids)]
                          [matcher (compile-pattern (syntax->datum #'pattern) try?)])
              #`(begin
                  (define-values (ok? pattern-result-id ...)
                    (let ([s expr])
                      (if (and guard-expr
                               #,(if try?
                                     (compile-pattern-check (syntax->datum #'pattern))
                                     #'#t))
                          (let ([orig-s s]) 
                            (let-values ([(pattern-result-id ...) matcher])
                              (values #t pattern-result-id ...)))
                          (values #f false-result ...))))
                  (define-syntax id
                    (syntax-rules (quote pattern-id ...)
                      [(m) ok?]
                      [(m (quote pattern-id))
                       pattern-result-id]
                      ...)))))])))))
;;;
;;; .../racket/src/expander/host/correlate.rkt
;;;

(define (correlated->list e)
  (let loop ([e e])
    (cond
     [(list? e) e]
     [(pair? e) (cons (car e) (loop (cdr e)))]
     [(null? e) null]
     [(syntax? e) (loop (syntax-e e))]
     [else (error 'correlated->list "not a list")])))

(define (correlated-cadr e)
  (car (correlated-e (cdr (correlated-e e)))))

(define (correlate src-e s-exp)
  (define e       (datum->correlated s-exp src-e))
  (define maybe-n (syntax-property src-e 'inferred-name))
  (if maybe-n
      (syntax-property e 'inferred-name maybe-n)
      e))

(define-define-match define-correlated-match
  syntax? syntax-e (lambda (false str e) (error str)))


;;;
;;; src/expander/syntax/datum-map.rkt
;;;

;; `(datum-map v f)` walks over `v`, traversing objects that
;; `datum->syntax` traverses to convert content to syntax objects.
;; 
;; `(f tail? d)` is called on each datum `d`, where `tail?`
;; indicates that the value is a pair/null in a `cdr` --- so that it
;; doesn't need to be wrapped for `datum->syntax`, for example;
;; the `tail?` argument is actually #f or a fixnum for a lower bound
;; on `cdr`s that have been taken
;;
;; `gf` is like `f`, but `gf` is used when the argument might be
;; syntax; if `gf` is provided, `f` can assume that its argument
;; is not syntax
;;
;; If a `seen` argument is provided, then it should be an `eq?`-based
;; hash table, and cycle checking is enabled; when a cycle is
;; discovered, the procedure attached to 'cycle-fail in the initial
;; table is called
;;
;; If a `known-pairs` argument is provided, then it should be an
;; `eq?`-based hash table to map pairs that can be returned as-is
;; in a `tail?` position

;; The inline version uses `f` only in an application position to
;; help avoid allocating a closure. It also covers only the most common
;; cases, defering to the general (not inlined) function for other cases.

(define fx+ +)
(define fx> >)

(define (datum-map s f [gf f] [seen #f] [known-pairs #f])
  (let loop ([tail? #f] [s s] [prev-depth 0])
    (define depth (fx+ 1 prev-depth)) ; avoid cycle-checking overhead for shallow cases
    (cond
     [(and seen (depth . fx> . 32))
      (datum-map-slow tail? s  (lambda (tail? s) (gf tail? s)) seen known-pairs)]
     [(null? s) (f tail? s)]
     [(pair? s)
      (f tail? (cons (loop #f (car s) depth)
                     (loop 1 (cdr s) depth)))]
     [(symbol? s) (f #f s)]
     [(boolean? s) (f #f s)]
     [(number? s) (f #f s)]
     [(or (vector? s) (box? s) (prefab-struct-key s) (hash? s))
      (datum-map-slow tail? s (lambda (tail? s) (gf tail? s)) seen known-pairs)]
     [else (gf #f s)])))

(define (datum-map-slow tail? s f seen known-pairs)
  (let loop ([tail? tail?] [s s] [prev-seen seen])
    (define seen
      (cond
       [(and prev-seen (datum-has-elements? s))
        (cond
         [(hash-ref prev-seen s #f)
          ((hash-ref prev-seen 'cycle-fail) s)]
         [else (hash-set prev-seen s #t)])]
       [else prev-seen]))
    (cond
     [(null? s) (f tail? s)]
     [(pair? s)
      (cond
        [(and known-pairs
              tail?
              (hash-ref known-pairs s #f))
         s]
        [else
         (f tail? (cons (loop #f (car s) seen)
                        (loop (if tail? (fx+ 1 tail?) 1)  (cdr s) seen)))])]
     [(or (symbol? s) (boolean? s) (number? s))
      (f #f s)]
     [(vector? s)
      (f #f (vector->immutable-vector
             (for/vector #:length (vector-length s) ([e (in-vector s)])
                         (loop #f e seen))))]
     [(box? s)
      (f #f (box-immutable (loop #f (unbox s) seen)))]
     #;[(immutable-prefab-struct-key s)
      => (lambda (key)
           (f #f
              (apply make-prefab-struct
                     key
                     (for/list ([e (in-vector (struct->vector s) 1)])
                       (loop #f e seen)))))]
     [(and (hash? s) (immutable? s))
      (f #f
         (hash-map/copy s
                        (lambda (k v)
                          (values k (loop #f v seen)))
                        #:kind 'immutable))]
     [else (f #f s)])))

(define (datum-has-elements? d)
  (or (pair? d)
      (vector? d)
      (box? d)
      #;(immutable-prefab-struct-key d)
      (and (hash? d) (immutable? d) (positive? (hash-count d)))))


;;;
;;; src/expander/run/correlated-to-host-syntax.rkt
;;;

(define (correlated->host-syntax v)
  (datum-map v
             (lambda (tail? v)
               (cond
                [(correlated? v)
                 (define e (correlated->host-syntax (correlated-e v)))
                 (define s (datum->syntax #f
                                          e 
                                          (vector (correlated-source v)
                                                  (correlated-line v)
                                                  (correlated-column v)
                                                  (correlated-position v)
                                                  (correlated-span v))))
                 (define keys (correlated-property-symbol-keys v))
                 (for/fold ([s s]) ([key (in-list keys)])
                  (syntax-property s key (correlated-property v key)))]
                [else v]))))


;;; 
;;; src/expander/run/linklet.rkt
;;;

(struct linklet (name)                 #:transparent)

(struct source-linklet   linklet (src) #:transparent)

(struct compiled-linklet linklet (proc      ; takes self instance plus instance arguments to run the linklet body
                                  importss  ; list [length is 1 less than proc arity] of list of symbols
                                  exports)  ; list of symbols
  #:transparent)

(struct instance (name        ; for debugging, typically a module name + phase
                  data        ; any value (e.g., a namespace)
                  variables)  ; symbol -> value
  #:transparent)

(define (make-instance name [data #f] [mode #f] . content)
  (define i (instance name data (make-hasheq)))
  (let loop ([content content])
    (cond
     [(null? content) (void)]
     [else
      (unless (symbol? (car content))
        (raise-argument-error 'make-instance
                              "symbol?"
                              (car content)))
      (when (null? (cdr content))
        (raise-arguments-error 'make-instance
                               "missing variable value"
                               "variable" (car content)))
      (instance-set-variable-value! i (car content) (cadr content) mode)
      (loop (cddr content))]))
  i)

(define (instance-variable-names i)
  ; todo: filter unset variables
  (hash-keys (instance-variables i)))


; helper
(define (instance-variable-box i sym can-create?)
  (or (hash-ref (instance-variables i) sym #f)
      (if can-create?
          (let ([b (box undefined)])
            (hash-set! (instance-variables i) sym b)
            b)
          (error 'link "missing binding: ~s" sym))))

(define (instance-set-variable-value! i sym val [constant? #f])
  ; todo: this doesn't handle the mode
  (set-box! (instance-variable-box i sym #t) val))

(define (instance-unset-variable! i sym)
  (set-box! (instance-variable-box i sym #t) undefined))

(define (instance-variable-value i sym [fail-k (lambda () (error "instance variable not found:" sym))])
  (define b (hash-ref (instance-variables i) sym #f))
  (cond
   [(and b
         (not (eq? (unbox b) undefined)))
    (unbox b)]
   [(procedure? fail-k) (fail-k)]
   [else fail-k]))

(define (instance-describe-variable! i sym desc)
  (void))

;; ----------------------------------------

(define undefined (gensym 'undefined))

(define (check-not-undefined val sym)
  (if (eq? val undefined)
      (check-not-unsafe-undefined unsafe-undefined sym)
      val))

;; ----------------------------------------

(define (primitive-table name)
  (cond
    [(eq? name '#%bootstrap-linklet) #f]
    #;[(eq? name '#%linklet)           (linklet-operations=> reflect-hash)]
    [else
     (define mod-name           `(quote ,name))
     (define-values (vars trans) (module->exports mod-name))
     (for/hasheq ([sym (in-list (map car (cdr (assv 0 vars))))])
       (values sym
               (dynamic-require mod-name sym)))]))


;; Bootstrap implementation doesn't support bytecode:
(define (primitive->compiled-position v)      #f)
(define (compiled-position->primitive pos)    #f)
(define (primitive-in-category? name cat-sym) #f)
(define (primitive-lookup sym)                #f)

;; ----------------------------------------

(struct variable-reference (instance primitive-varref))

(define (variable-reference->instance vr [ref-site? #f])
  (and (or ref-site?
           ;; It would be better to have a `variable-reference-anonymous?` predicate:
           ; TODO
           #;(with-handlers ([exn:fail? (lambda (exn) #f)])
               (variable-reference->module-declaration-inspector
                (variable-reference-primitive-varref vr))))
       ;; Always returning ref-site instance; that's good enough to
       ;; bootstrap:
       (variable-reference-instance vr)))

(define variable-reference-constant?*
  (let ([variable-reference-constant?
         (lambda (vr)
           (variable-reference-constant? (variable-reference-primitive-varref vr)))])
    variable-reference-constant?))


(define variable-reference-from-unsafe?*
  (let ([variable-reference-from-unsafe?
         (lambda (vr)
           (variable-reference-from-unsafe? (variable-reference-primitive-varref vr)))])
    variable-reference-from-unsafe?))

;; ----------------------------------------

; https://github.com/racket/racket/ ... /racket/src/expander/boot/runtime-primitive.rkt#L11

;; Instances that are built into the runtime system, but
;; not including '#%linklet
(define runtime-instances
  '(#%kernel
    #%paramz
    #%foreign
    #%unsafe
    #%flfxnum
    #%extfl
    #%network
    #%place
    #%futures
    #%terminal))

(define cu-namespace (make-empty-namespace))
(namespace-attach-module (current-namespace) ''#%builtin cu-namespace)
(parameterize ([current-namespace cu-namespace])
  (for ([name (in-list runtime-instances)])
    (namespace-require `',name))
  (namespace-require ''#%linklet)
  (namespace-set-variable-value! 'check-not-undefined check-not-undefined)
  (namespace-set-variable-value! 'instance-variable-box instance-variable-box)
  (namespace-set-variable-value! 'variable-reference variable-reference)
  (namespace-set-variable-value! 'variable-reference? variable-reference? #t)
  (namespace-set-variable-value! 'variable-reference->instance variable-reference->instance #t)
  (namespace-set-variable-value! 'variable-reference-constant? variable-reference-constant?* #t)
  (namespace-set-variable-value! 'variable-reference-from-unsafe? variable-reference-from-unsafe?* #t)
  ;; Needed when the host is RacketCS:
  #;(namespace-set-variable-value! 'fasl->s-exp/intern (lambda (v)
                                                         (fasl->s-exp v #:datum-intern? #t))))

;; ----------------------------------------

;; (linklet [[imported-id/renamed ...] ...]
;;          [exported-id/renamed ...]
;;   defn-or-expr ...)
 
;; imported-id/renamed = imported-id
;;                     | (external-imported-id internal-imported-id)
 	 	 	 	 
;; exported-id/renamed = exported-id
;;                     | (internal-exported-id external-exported-id)



;; Compile a `linklet` to a plain `lambda`. Also, convert from the
;; notion of correlated that works for `compile-linklet` to the notion
;; of host syntax objects that works for `compile`.
(define (desugar-linklet c)
  (define imports    (list-ref c 1))
  (define exports    (list-ref c 2))
  (define bodys      (list-tail c 3))
  (define inst-names (for/list ([import (in-list imports)]
                                [i      (in-naturals)])
                       (string->symbol (format "in_~a" i))))
  (define import-box-bindings
    (for/list ([inst-imports (in-list imports)]
               [inst         (in-list inst-names)]
               #:when #t
               [name         (in-list inst-imports)])
      (define ext (if (symbol? name) name (car name)))   ; external name
      (define int (if (symbol? name) name (cadr name)))  ; internal name
      `[(,int) (instance-variable-box ,inst ',ext #f)]))
  (define export-box-bindings
    (for/list ([name (in-list exports)])
      (define int (if (symbol? name) name (car name)))
      (define ext (if (symbol? name) name (cadr name)))
      `[(,int) (instance-variable-box self-inst ',ext #t)]))
  (define box-bindings    (append import-box-bindings export-box-bindings))
  (define import-box-syms (apply seteq (map caar import-box-bindings)))
  (define box-syms        (set-union import-box-syms
                                     (apply seteq (map caar export-box-bindings))))
  (define (desugar e)
    (cond
     [(correlated? e)
      (correlate e (desugar (correlated-e e)))]
     [(symbol? e) (if (set-member? box-syms e)
                      (if (set-member? import-box-syms e)
                          `(unbox ,e)
                          `(check-not-undefined (unbox ,e) ',e))
                      e)]
     [(pair? e)
      (case (correlated-e (car e))
        [(quote) e]
        [(set!)
         (define-correlated-match m e '(set! var rhs))
         (if (set-member? box-syms (correlated-e (m 'var)))
             `(set-box! ,(m 'var) ,(desugar (m 'rhs)))
             `(set!     ,(m 'var) ,(desugar (m 'rhs))))]
        [(define-values)
         (define-correlated-match m e '(define-values (id ...) rhs))
         (define ids  (m 'id))
         (define tmps (map gensym (map correlated-e ids)))
         `(define-values ,(for/list ([id (in-list ids)]
                                     #:when (not (set-member? box-syms (correlated-e id))))
                            id)
           (let-values ([,tmps (let-values ([,ids ,(desugar (m 'rhs))])
                                 (values ,@ids))])
             (begin
               ,@(for/list ([id (in-list ids)]
                            [tmp (in-list tmps)]
                            #:when (set-member? box-syms (correlated-e id)))
                   `(set-box! ,id ,tmp))
               (values ,@(for/list ([id (in-list ids)]
                                    [tmp (in-list tmps)]
                                    #:when (not (set-member? box-syms (correlated-e id))))
                           tmp)))))]
        [(lambda)
         (define-correlated-match m e '(lambda formals body))
         `(lambda ,(m 'formals) ,(desugar (m 'body)))]
        [(case-lambda)
         (define-correlated-match m e '(case-lambda [formals body] ...))
         `(case-lambda ,@(for/list ([formals (in-list (m 'formals))]
                                    [body (in-list (m 'body))])
                           `[,formals ,(desugar body)]))]
        [(#%variable-reference)
         (if (and (pair? (correlated-e (cdr (correlated-e e))))
                  (set-member? box-syms (correlated-e (correlated-cadr e))))
             ;; Using a plain `#%variable-reference` (for now) means
             ;; that all imported and exported variables count as
             ;; mutable:
             '(variable-reference self-inst (#%variable-reference))
             ;; Preserve info about a local identifier:
             `(variable-reference self-inst ,e))]
        [else (map desugar (correlated->list e))])]
     [else e]))
  (define (last-is-definition? bodys)
    (define p (car (reverse bodys)))
    (and (pair? p) (eq? (correlated-e (car p)) 'define-values)))
  (correlated->host-syntax
   `(lambda (self-inst ,@inst-names)
     (let-values ,box-bindings
       ,(cond
         [(null? bodys) '(void)]
         [else
          `(begin
            ,@(for/list ([body (in-list bodys)])
                (desugar body))
            ,@(if (last-is-definition? bodys)
                  '((void))
                  null))])))))

;; #:pairs? #f -> list of list of symbols
;; #:pairs? #t -> list of list of (cons ext-symbol int-symbol)
(define (extract-import-variables-from-expression c #:pairs? pairs?)
  (for/list ([is (in-list (unmarshal (list-ref c 1)))])
    (for/list ([i (in-list is)])
      (cond 
       [pairs? (if (symbol? i)
                   (cons i i)
                   (cons (car i) (cadr i)))]
       [else (if (symbol? i)
                 i
                 (car i))]))))

;; #:pairs? #f -> list of symbols
;; #:pairs? #t -> list of (cons ext-symbol int-symbol)
(define (extract-export-variables-from-expression c #:pairs? pairs?)
  (for/list ([e (in-list (unmarshal (list-ref c 2)))])
    (cond
     [pairs? (if (symbol? e)
                 (cons e e)
                 (cons (cadr e) (car e)))]
     [else (if (symbol? e)
               e
               (cadr e))])))

;; ----------------------------------------

(define orig-eval    (current-eval))
(define orig-compile (current-compile))

(define linklet-compile-to-s-expr (make-parameter #f #f 'linklet-compile-to-s-expr))

;; Compile to a serializable form
(define (compile-linklet c
                         [info        #f]
                         [import-keys #f]
                         [get-import  (lambda (key) (values #f #f))]
                         [options     '(serializable)])
  (define l
    (cond
      [(linklet-compile-to-s-expr)
       (source-linklet (marshal (correlated->datum/lambda-name c)))]
      [else
       (define plain-c (desugar-linklet c))
       (parameterize ([current-namespace cu-namespace]
                      [current-eval      orig-eval]
                      [current-compile   orig-compile])
         ;; Use a vector to list the exported variables
         ;; with the compiled bytecode
         (compiled-linklet (compile plain-c)
                           (marshal (extract-import-variables-from-expression c #:pairs? #f))
                           (marshal (extract-export-variables-from-expression c #:pairs? #f))))]))
  (if import-keys
      (values l import-keys) ; no imports added or removed
      l))

;; ----------------------------------------

(struct path-bytes   (bstr)                             #:prefab)
(struct unreadable   (str)                              #:prefab)
(struct void-value   ()                                 #:prefab)
(struct srcloc-parts (source line column position span) #:prefab)

(define (marshal c)
  (datum-map c (lambda (tail? c)
                 (cond
                  [(path? c)                    (path-bytes (path->bytes c))]
                  [(and (symbol? c)
                        (symbol-unreadable? c)) (unreadable (symbol->string c))]
                  [(void? c)                    (void-value)]
                  [(srcloc? c)                  (srcloc-parts (marshal (srcloc-source c))
                                                              (marshal (srcloc-line c))
                                                              (marshal (srcloc-column c))
                                                              (marshal (srcloc-position c))
                                                              (marshal (srcloc-span c)))]
                  [else c]))))

(define (unmarshal c)
  (datum-map c
             (lambda (tail? c)
               (cond
                [(path-bytes? c)    (bytes->path (path-bytes-bstr c))]
                [(unreadable? c)    (string->unreadable-symbol (unreadable-str c))]
                [(void-value? c)    (void)]
                [(srcloc-parts? c)  (srcloc (marshal (srcloc-parts-source c))
                                            (marshal (srcloc-parts-line c))
                                            (marshal (srcloc-parts-column c))
                                            (marshal (srcloc-parts-position c))
                                            (marshal (srcloc-parts-span c)))]
                [else c]))))

;; Like `correlated->datum`, but preserves 'inferred-name information
;; by encoding it as a symbol in a `lambda` or `case-lambda` body.
;; Remove any existing symbol in the name position that might
;; otherwise be confused for the name. This conversion avoids parsing
;; expressions in general by relying on the fact that bindings are
;; renamed to avoid shadowing, `lambda`, `case-lambda`, or `quote`.
(define (correlated->datum/lambda-name c)
  (define (strip-potential-name-from-body body)
    (define-correlated-match m body #:try '(begin (quote _) body bodys ...))
    (cond
      [(and (m)
            (eq? 'begin (m 'begin))
            (eq? 'quote (m 'quote)))
       (strip-potential-name-from-body 
        (if (null? (m 'bodys))
            (m 'body)
            `(begin ,@(m 'bodys))))]
      [else body]))
  (let correlated->datum/lambda-name ([c c])
    (cond
      [(and (pair? c)
            (eq? (car c) 'lambda))
       (define-correlated-match m c '(lambda args body))
       `(lambda ,(correlated->datum (m 'args))
          ,(correlated->datum/lambda-name
            (strip-potential-name-from-body (m 'body))))]
      [(and (pair? c)
            (eq? (car c) 'case-lambda))
       (define-correlated-match m c '(case-lambda [argss bodys] ...))
       `(case-lambda
          ,@(for/list ([args (in-list (m 'argss))]
                       [body (in-list (m 'bodys))])
              `[,(correlated->datum args)
                ,(correlated->datum/lambda-name
                  (strip-potential-name-from-body body))]))]
      [(and (pair? c)
            (eq? (car c) 'quote))
       (correlated->datum c)]
      [(pair? c)
       (cons (correlated->datum/lambda-name (car c))
             (correlated->datum/lambda-name (cdr c)))]
      [(and (correlated? c)
            (let ([e (correlated-e c)])
              (and (pair? e)
                   (or (eq? 'lambda (car e))
                       (eq? 'case-lambda (car e)))))
            (correlated-property c 'inferred-name))
       => (lambda (name)
            (cond
              [(void? name)
               ;; Don't try to hide the name after all
               (correlated->datum/lambda-name (correlated-e c))]
              [else
               ;; Encode `name` as a symbol in the function body:
               (define lam (correlated->datum/lambda-name (correlated-e c)))
               (cond
                 [(eq? 'lambda (car lam))
                  (define-correlated-match m lam '(lambda args body))
                  `(lambda ,(m 'args) (begin (quote ,name) ,(m 'body)))]
                 [else
                  (define-correlated-match m lam '(case-lambda [argss bodys] ...))
                  (cond
                    [(null? (m 'argss))
                     ;; give up on naming an empty `case-lambda`
                     lam]
                    [else
                     `(case-lambda
                        [,(car (m 'argss)) (begin (quote ,name) ,(car (m 'bodys)))]
                        ,@(cddr lam))])])]))]
      [(correlated? c)
       (correlated->datum/lambda-name (correlated-e c))]
      [else
       (correlated->datum c)])))

(define (instantiate-linklet linklet               ; a linklet?
                             import-instances      ; a list of instances
                             [target-instance #f]  ; #f or an instance
                             [use-prompt?     #f]) ; ignored by webracket
  ; Type check linklet
  (unless (linklet? linklet)
    (raise-argument-error 'instantiate-linklet "linklet?" linklet))

  (unless (compiled-linklet? linklet)
    (raise-argument-error 'instantiate-linklet "compiled-linklet?" linklet))

  ; Type check import-instances
  (let loop ([l import-instances])
    (unless (null? l)
      (if (and (pair? l) (instance? (car l)))
          (loop (cdr l))
          (raise-argument-error 'instantiate-linklet "(listof instance?)" import-instances))))

  ; Type check the target instance
  (unless (and target-instance (instance? target-instance))
    (raise-argument-error 'instantiate-linklet "(or/c instance? #f)" target-instance))

  ;; From now on, we can assume the linklet is a compiled-linklet.
  
  ; Check that the length of import-instances match compiled-linklets-importss
  (unless (= (length (compiled-linklet-importss linklet))
             (length import-instances))
    (raise-argument-error 'instantiate-linklet
                          "the number of import instances does not match the expected number of imports"))
  ; Two cases:

  ;   1) If `target-instance` is an existing instance, then  
  ;      the instance is used and modified for the linklet definitions and expressions, and
  ;      the result is the value of the last expression in the linklet.
  
  ;   2) If target-instance is #f or not provided,
  ;      the result is a fresh instance for the linklet.

  (cond
    [target-instance
     ;; Instantiate into the given instance and return the result of the linklet body.     
     (apply (compiled-linklet-proc linklet) target-instance import-instances)]
    
    [else
     ;; Make a fresh instance, recur, and return the instance
     (let ([i (make-instance (linklet-name linklet))])
       (instantiate-linklet linklet import-instances i use-prompt?)
       i)]))
