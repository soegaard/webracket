#lang racket/base
(module+ test (require rackunit))
(require "expander.rkt"  ; provides topexpand
         "assembler.rkt"
         "priminfo.rkt"  ; information on Racket primitives
         nanopass/base
         racket/match
         racket/port 
         (only-in racket/format ~a)
         (only-in racket/list partition append* first second third last
                  index-where append-map make-list rest))

(require
  ; (prefix-in ur- urlang)
  (for-syntax nanopass/base syntax/parse racket/syntax racket/base)
  syntax/kerncase ; for kernel-form-identifier-list
  syntax/stx
  racket/syntax
  (except-in syntax/parse str) ; the identifier str is used in the runtime 
  ; (rename-in racket/match [match Match])
  ; (only-in srfi/1 list-index)cha
  '#%paramz) ; contains the identifier parameterization-key

;;;
;;; Expressions to work on
;;;

; If (values) is called in an <effect> context, an empty $Values is pushed to the stack.
; But the context expects 0 values on the stack. 
;     (comp+  #`(let () (let-values ([() (values)]) 3)))

; Invoking a primitive via a $PrimitiveProcedure is not done.


;;;
;;; TODO
;;;

; [ ] Use new machinery for runtime string and symbol constants.

; [x] Inline the `void` primitive.
; [ ] Implement a `void` function.

; [x] Implement `equal?`
; [ ] Handle cycles in `equal?`
; [ ] Handle long lists in `equal?` (no recursion).

; [ ] Implement functions needed by `for`
;     [x] variable-reference-from-unsafe?
;     [x] unsafe-car
;     [x] unsafe-cdr

; [x] Implement make-struct-field-mutator for mutable structs.

; [x] Super structs.

; [x]  Handle resize in the symbol table.
;      To test, use a low initial capacity.
;      Solution: insert was using $false instead of $missing as sentinel.

; [ ] Make a pass that rewrites primapps with primitives that takes an
;     optional number of arguments. Fill in the remaining slots with ... missing?
;     Or pass optional arguments in a global variable (caller/callee save)?

; [ ] A reference to an undefined, top-level variable must raise an error.
;     Currently the compiler just reports that classify-variable can't
;     can't find the identifier. Maybe there is a missing case in the
;     classifying pass?

; [x] Flonum as heap object (sigh)
; [x] Keywords.

; [x] Closures: rest arguments and arity check
; [x] Closures: $invoke-closure needs to do an arity check
; [x] Closures: $invoke-closure needs to handle rest arguments
; [x] Apply:    handle rest arguments
; [x] $invoke-closure  use repack-arguments
; [ ] References to primitives. Make a new heap type.
;     [x] New heap type: $PrimitiveProcedure
;     [x] Store a $PrimitiveProcedure in the globals prim:fx+, prim:fx-, ...
;         (each primitive `pr` get a global `$prim:pr`.
;     [x] A reference to `pr` evaluates to `(global.get $prim:pr)`.
;     [ ] Apply can call $invoke-primitive -- but how should $PrimitiveCode
;         look like? We could wrap e.g. `fx+` as (λ (x y) (fx+ x y))
;         and then use `$invoke-closure$ ?

; [ ] use an i32 to hold the arity in $Procedure

; [ ] case-lambda
; [ ] Calling conventions for primitives?

; [x] procedure?, procedure-rename, procedure-arity, etc.
; [ ] call-with-values
; [x] apply
; [ ] map

; [x] generating string and symbol constants for use in the runtime

; [ ] Implement guards for structs.

; [ ] String ports.

; [ ] Input / output port going to the host.

; [ ] Parameters.

; [ ] Synchronize primitives between compiler and the `webracket` language.

; [ ] Modules!

; [ ] Hash tables
; [x] - mutable hasheq tables

; [ ] Sets

; [ ] Numbers
;      - bignums, rationals, exact, inexact 
; [ ] Floating points.
;      - log, exp, expt, sin, cos, ...
; [ ] Implement a proper flonum printing algorithm.

;;;
;;; NOTES
;;;

; (eq? 1. 1.) is true in Racket, but false in WebRacket.
; According to RnRS eq? on numbers are implementation dependent,
; and one must use eqv? to work with numbers.


;;;
;;; HELPERS
;;;

(define (identity x) x)

(define-syntax (letv stx)
  ; syntax: (letv ((x ...) e) b ...)
  ;   bind the result of e to the variables x ... in the body b ...
  ;   That is: letv is let-values with only one clause.
  (syntax-parse stx
    [(_letv ((x:id ...) e:expr) b:expr ...)
     (syntax/loc stx
       (let-values ([(x ...) e]) b ...))]))

(define (map* f αs ρ)
  ; only first value is returned
  (for/fold ([ρ ρ]) ([α αs])
    (f α ρ)))

(define (map2* f xs ρ)
  ; f : α β -> (values α β)
  ; map f over xs while threading the seconding value  
  (define (f* xs ρ)
    (match xs
      ['()         (values '() ρ)]
      [(cons x xs) (letv ((x ρ) (f x ρ))
                     (letv ((xs ρ) (f* xs ρ))
                       (values (cons x xs) ρ)))]))
  (f* xs ρ))

;;;
;;; VARIABLES
;;;

; Representation of variables during compilation.

(struct variable (id) #:transparent)
; id is an identifier (i.e. a syntax-object)

(define (unparse-variable x)
  (syntax->datum (variable-id x)))

;;; Quick and dirty sets of variables represented as lists
(define (variable=? x y)         (free-identifier=? (variable-id x) (variable-id y)))
(define (ids->id-set xs)         (for/fold ([s '()]) ([x xs]) (set-add s x)))
(define (make-id-set . xs)       (ids->id-set xs))
(define empty-set                (make-id-set))
(define (set-in? x s)            (member x s variable=?))  
(define (set-add s x)            (if (set-in? x s) s (cons x s)))
(define (set-union s1 s2)        (for/fold ([s s1]) ([x s2]) (set-add s x)))
(define (set-union* ss)          (for/fold ([u empty-set]) ([s ss]) (set-union u s)))
(define (set-remove s x)         (remove x s variable=?))
(define (set-difference s1 s2)   (for/fold ([s s1]) ([x s2]) (set-remove s x)))
(define (set-intersection s1 s2) (for/list ([x s1] #:when (set-in? x s2)) x))
(define (set-empty? s)           (equal? s '()))
(define (set-disjoint? s1 s2)    (set-empty? (set-intersection s1 s2)))


;;;
;;; DATUMS AND CONSTANTS
;;;

; Representation of datums during compilation.
; Quotation using quote creates a datum.
; Constants are literals ("selfquoting").
; Parse will introduce explicit quotes for constants.

(struct datum (stx value) #:transparent)
(define (unparse-datum d) (datum-value d))

(define u31-min 0)
(define u31-max (- (expt 2 31) 1))

(define u30-min 0)                 ; unsigned, 30-bit
(define u30-max (- (expt 2 30) 1))

(define s30-min (- (expt 2 29)  )) ; signed, 30-bit
(define s30-max (- (expt 2 29) 1))

; (define most-negative-fixnum s30-min)
; (define most-positive-fixnum s30-max)


(define (wr-fixnum? x)
  (and (number? x)
       (integer? x)
       (<= u31-min x u31-max)))

(define (wr-number? v)
  (and (number? v)
       (or (wr-fixnum? v)
           #;(inexact? v))))

(define (constant? v)
  (or (wr-number? v)
      (boolean? v)
      (symbol? v)))

(define datum:undefined (datum #f #f)) ; TODO: make the undefined value a datum


;;;
;;; PRIMITIVES
;;;

;; For some primitives we have need an easy way to construct a
;; variable reference to the primitive.

(define primitives '())

;; Most primitives are either primitives or procedures in standard Racket.
;; We can therefore use reflection to lookup information about arities,
;; names, realms etc.


(define-syntax (define-primitive stx)
  (syntax-parse stx
    [(_define-primitive name:id)
     ; for each primitive `name` we define a function `var:name`
     ; that creates a new variable reference to the primitive.
     (define var:name (format-id #'name "var:~a" #'name))
     (with-syntax ([var:name var:name])
       (syntax/loc stx
         (begin
           ; add the primitive (as a symbol) to the primitives list
           (set! primitives (cons 'name primitives))
           (define (var:name) (variable #'name)))))]))

(define-syntax (define-primitives stx)
  (syntax-parse stx
    [(_define-primtives name ...)
     (syntax/loc stx
       (begin
         (define-primitive name)
         ...))]))

#;(type $PrimitiveProcedure
      (sub $Procedure
           (struct
             ; From $Procedure
             (field $hash   (mut i32))
             (field $name   (ref eq))
             (field $arity  (ref eq))
             (field $realm  (ref eq))
             (field $invoke (ref $ProcedureInvoker))
             ; own fields
             (field $result-arity (ref eq))))) ;; fixnum like 1 for most


(define-primitives
  ; call/cc ; todo remove - we use it for our test function

  raise-unbound-variable-reference
  
  ; structures
  make-struct-type
  make-struct-field-accessor 
  make-struct-field-mutator
  struct-constructor-procedure? 
  struct-predicate-procedure?
  struct-accessor-procedure?
  struct-mutator-procedure?
  struct?
  struct-type?

  ; null                     ; todo
  current-inspector          ; todo
  
  values                     
  
  box-immutable

  boxed      ; used by assignment elimination
  unboxed    ; used by assignment elimination
  set-boxed! ; used by assignment elimination

  box unbox set-box! ; normal primitives
  
  vector-immutable  ; used in datum construction
  bytes             ; used in datum construction
  string            ; used in datum construction

  pair? null? 
  cons car cdr
  list              ; not first order yet
  list? length list-ref list-tail
  append ; two arguments
  reverse memq
  alt-reverse ; used in expansion of for/list

  void?
  make-void  ; zero arguments
  void

  boolean? not

  char?
  char=? ; two arguments
  char->integer
  integer->char
  char-whitespace?
  
  eq?
  eqv?
  equal?

  number->string

  + - * /
  = < > <= >=
  zero? positive? negative? 
  add1 sub1

  integer?
  exact? exact-integer?
  exact-nonnegative-integer?
  exact-positive-integer?
  
  
  fixnum? fxzero?
  fx+ fx- fx* 
  fx= fx> fx< fx<= fx>=
  ; fxmin fxmax
  
  ; fxquotient fxremainder fxmodulo fxabs
  ; fxand fxior fxxor fxnot fxlshift fxrshift
  ; fxpopcount fxpopcount16 fxpopcount32
  ; fx+/wraparound fx-/wraparound fx*/wraparound fxlshift/wraparound
  ; fxrshift/logical

  ; fx->fl fl->fx
  ; fixnum-for-every-system?
  
  fl+ fl- fl* fl/
  fl= fl< fl> fl<= fl>=
  
  byte?

  vector  ; not first order yet, rewritten to (list->vector ...)
  vector? make-vector vector-ref vector-set! vector-length
  vector-fill! vector-copy! vector-empty? vector-take vector-drop
  vector-drop-right vector-split-at
  vector->list
  
  bytes?  make-bytes  bytes-ref  bytes-set!  bytes-length  subbytes bytes-copy!
  bytes-copy bytes-fill! bytes-append bytes->list list->bytes bytes=?

  
  string? string=? string<?
  make-string string-ref string-set! string-length substring string-copy!
  string-copy string-fill! string-append string->list list->string
  string->bytes/utf-8

  string-trim-left   ; not in Racket
  string-trim-right  ; not in Racket
  string-drop        ; not in Racket

  symbol? symbol=? symbol<?
  string->symbol symbol->string
  string->uninterned-symbol
  symbol-interned?
  
  string-port?
  ; open-input-bytes
  ; open-input-string  
  open-output-bytes
  ; open-output-string ; (same as open-output-bytes)
  get-output-bytes
  ; get-output-string
  write-byte
  port-next-location

  s-exp->fasl
  fasl->s-exp

  make-empty-hasheq ; not in Racket
  make-hasheq 
  hash-ref
  hash-set!
  hash-remove!
  hash-clear!
  hash-has-key?
  eq-hash-code

  keyword?
  keyword->string
  string->keyword

  apply
  procedure-rename
  procedure?
  procedure-arity
  procedure-arity-mask
  procedure-arity-includes?

  primitive?
  primitive-closure?
  primitive-result-arity

  variable-reference-from-unsafe?
  variable-reference-constant?


  ;; 17. Unsafe Operations
  unsafe-fx+
  unsafe-fl/

  unsafe-fx=
  unsafe-fx<

  unsafe-car
  unsafe-cdr
  unsafe-vector*-length
  unsafe-vector*-set!

  ;; support for `for`
  ; The functions below will be removed, when `webracket` implements `for`
  ; grow-vector
  ; shrink-vector  
  )


#; (require (for-syntax (only-in urlang urmodule-name->exports)))

#;(define-syntax (define-runtime-primitives stx)
  (syntax-parse stx
    [(_define-runtime-primitives)
     (define all-exports (for/list ([p (urmodule-name->exports 'runtime)])
                           (datum->syntax stx p)))
     (with-syntax ([(pr ...) all-exports])
       (syntax/loc stx
         (define-primitives pr ...)))]))

;; declare names as primitives - the compiler now treats these names as primitives
#;(define-runtime-primitives)

;; Primitives with names special names.
;; The following primitives require special attention.
;; The identifiers +, -, ... can't be used as names for functions in the target language.
;; The compiler knows how to open code the operators.
;; References to a primitive + will be compiled into PRIM+ which
;; is exported from the runtime.

#;(define-primitives
  + - * /
  = < > <= >=)

; This list of macro introduced identifiers goes away when module support arrives.
; set these in compiler3.rkt instead.
; (define macro-introduced '(call-handled-body check-struct-type))

(define (primitive? v)
  (and (or (and (syntax? v)   (primitive? (syntax-e v)))
           (and (variable? v) (primitive? (variable-id v)))
           (member v primitives))
       #t))

; A few primitives need access to _tc and are therefore wrapped in a closure.
; This means that applications of these use (app ...) and not (primapp ...).
; In particular all parameters are implemented as closures.
; Names that start with "current-" are associated with parameters -  the exception
; being  current-parameterization  which aren't associated with a parameter.
; Also, some parameters are named with a -handler postfix.

#;(require (only-in urlang urmodule-name->exports))
(define (special-primitive? x)
  #f
  ;; (define (handler-name?   id)      (regexp-match "-handler$" (~a id)))
  ;; (define (parameter-name? id) (and (regexp-match "^current-" (~a id))
  ;;                                   (not (equal? id "current-parameterization"))))
  ;; (define all-exports    (urmodule-name->exports 'runtime))
  ;; (define all-parameters (filter (λ (id) (or (parameter-name? id) (handler-name? id))) all-exports))

  ;; (cond [(syntax? x)   (special-primitive? (syntax-e x))]
  ;;       [(variable? x) (special-primitive? (variable-id x))]
  ;;       [else          (or (memq x (list* 'apply
  ;;                                         'call-handled-body
  ;;                                         all-parameters)))])
  )

(define (unparse-primitive pr)
  (unparse-variable pr))


;;;
;;; SOURCE LANGUAGE
;;;

(define (modname? x)
  (or (and (syntax? x) (symbol? (syntax-e x)))
      (symbol? x)))

; the expander has check that we have a module name
(define (modpath? x) #t)
 
(define (unparse-module-path mp) (if (syntax? mp) (syntax-e mp) mp))
(define (unparse-module-name mn) (if (syntax? mn) (syntax-e mn) mn))

(define (unparse-top x) x)

;; The following language is a large subset of the Fully Expanded Syntax
;; which is the output of the Racket expander. Since the output of
;; expand-syntax has been through the macro expander, the output
;; has already been verified (i.e. no need to check for duplicate
;; identifiers etc.).

(define-language LFE    ; FE = Fully Expanded
  (entry TopLevelForm)
  (terminals   
   ((variable    (x xd)) . => . unparse-variable)
   ((datum       (d))    . => . unparse-datum)
   ((modname     (mn))   . => . unparse-module-name)
   ((modpath     (mp))   . => . unparse-module-path)
   (syntax       (s)))
  (Formals (f)
    (formals (x ...))            => (x ...)
    (formals (x0 x1 ... . xd))   => (x0 x1 ... . xd)
    (formals x)                  => x)
  (TopLevelForm (t)
    ; turns out it is best keep unique tags (Expr also has a begin)
    (topbegin s t ...)           => (begin t ...)
    (topmodule s mn mp mf ...)   => (module mn mp (#%plain-module-begin mf ...))
    (#%expression s e)           => (#%expression e)
    g)
  (ModuleLevelForm (mf)
    ; (#%provide rps ...)                           => (#%provide rps ...)
    ; (modbegin-for-syntax ...) todo
    ; (#%declare ...)           todo
    g)
  ; (SubModuleLevelForm ...) todo
  (GeneralTopLevelForm (g)
    e
    (define-values   s (x ...) e)                 => (define-values   (x ...) e)
    (define-syntaxes s (x ...) e)                 => (define-syntaxes (x ...) e)
    (#%require s rrs ...)                         => (#%require s rrs ...))
  (RawRequireSpec     (rrs) rrmp) ; todo
  ;(RawRequireSpec    (rrs) ps)   ; todo  <- the correct one
  ;(PhaselessSpec     (ps)  rmp)  ; todo
  ;(RawModulePath     (rmp) rrmp) ; todo
  (RawRootModulePath (rrmp) (quote x))
  (Expr (e)
    x                                             
    (λ s f e ...)                                 => (λ f e ...)                      ; #%plain-lambda
    (case-lambda s (f e0 e ...) ...)              => (case-lambda (f e0 e ...) ...)
    (if s e0 e1 e2)                               => (if e0 e1 e2)
    (begin  s e0 e1 ...)                          => (begin  e0 e1 ...)
    (begin0 s e0 e1 ...)                          => (begin0 e0 e1 ...)
    (let-values    s ([(x ...) e] ...) e0 e1 ...) => (let-values    ([(x ...) e] ...) e0 e1 ...)
    (letrec-values s ([(x ...) e] ...) e0 e1 ...) => (letrec-values ([(x ...) e] ...) e0 e1 ...)
    (set! s x e)                                  => (set! x e)
    (quote s d)                                   => (quote d)
    (quote-syntax s d)                            => (quote-syntax d) 
    (wcm s e0 e1 e2)                              => (with-continuation-mark e0 e1 e2)
    (app s e0 e1 ...)                             => (e0 e1 ...)             ; (#%plain-app e0 e1 ...)
    (top s x)                                     => (#%top . x)
    (variable-reference s vrx)                    => (#%variable-reference vrx))
  (VariableReferenceId (vrx)
     x                                            
     (anonymous s)                                => ()
     (top s x)                                    => (#%top . x)))


;;; 
;;; Parse
;;;

;; The standard tools in Racket represents programs as syntax objects.
;; In particular the output of expand-syntax is a syntax object representing
;; a program in fully expanded form. Here parse takes such a syntax object
;; and return a nanopass representation of the fully expanded program.

(define-pass parse : * (stx) -> LFE ()
  (definitions
    (define (Datum E d)                (datum E (syntax->datum d)))
    (define (Expr* es)                 (map Expr              (stx->list es)))
    (define (Expr** ess)               (map Expr*             (stx->list ess)))
    (define (Formals* fs)              (map Formals           (stx->list fs)))
    (define (TopLevelForm* ts)         (map TopLevelForm      (stx->list ts)))
    (define (ModuleLevelForm* ms)      (map ModuleLevelForm   (stx->list ms)))
    (define (variable* xs)             (map variable          (stx->list xs)))
    (define (RawRequireSpec* rsss)     (map RawRequireSpec    (stx->list rsss)))
    (define (RawRootModulePath* rrmps) (map RawRootModulePath (stx->list rrmps))))
  
  (Formals : * (F) -> Formals ()
    (with-output-language (LFE Formals)
      (syntax-parse F
        [(x:id ...)               `(formals (,(variable* #'(x ...)) ...))]
        [(x0:id x:id ... . xd:id) `(formals (,(variable #'x0) ,(variable* #'(x ...)) ...
                                                              . ,(variable #'xd)))]
        [x:id                     `(formals ,(variable #'x))]
        [_ (raise-syntax-error 'parse "expected formals" F)])))
  
  (TopLevelForm : * (T) -> TopLevelForm ()
    (with-output-language (LFE TopLevelForm)
      (syntax-parse T #:literal-sets (kernel-literals) ; keywords in fully expanded programs
        [(begin t ...)                `(topbegin ,T ,(TopLevelForm* #'(t ...)) ...)]
        [(#%expression e)             `(#%expression ,T ,(Expr #'e))]
        [(module mn mp 
           (module-begin mlf ...))    `(topmodule ,T ,(syntax-e #'mn) ,(syntax-e #'mp) 
                                                  ,(ModuleLevelForm* #'(mlf ...)) ...)]
        [g                            `,(GeneralTopLevelForm #'g)])))

  (ModuleLevelForm : * (M) -> ModuleLevelForm ()
    (with-output-language (LFE ModuleLevelForm)
      (syntax-parse M #:literal-sets (kernel-literals)
        ; (#%provide rps ...)       todo
        ; (modbegin-for-syntax ...) todo
        ; (#%declare ...)           todo
        [g  `,(GeneralTopLevelForm #'g)])))

  (GeneralTopLevelForm : * (G) -> GeneralTopLevelForm ()
    (with-output-language (LFE GeneralTopLevelForm)
      (syntax-parse G #:literal-sets (kernel-literals) ; keywords in fully expanded programs
        [(define-values   (x ...) e)  `(define-values   ,G (,(variable* #'(x ...)) ...) ,(Expr #'e))]
        [(#%require rrs ...)          `(#%require       ,G ,(RawRequireSpec* #'(rrs ...)) ...)]
        [(define-syntaxes (x ...) e)  (Expr #''"define-syntaxes is ignored")]
        ; Note: The (Expr #'e) will fail. The syntax patterns will fail: the literals are
        ;       in phase 0 and the expression e is in phase 1.
        ; `(define-syntaxes ,G (,(variable* #'(x ...)) ...) ,(Expr #'e))]
        
        [e                            `,(Expr #'e)])))
  
  (RawRequireSpec : * (RRS) -> RawRequireSpec ()
    (with-output-language (LFE RawRequireSpec)
      ; (displayln (list 'in RRS))
      (RawRootModulePath RRS))) ; TODO - temporary solution

  (RawRootModulePath : * (RRMP) -> RawRootModulePath ()
    (with-output-language (LFE RawRootModulePath)
      (syntax-parse RRMP #:literal-sets (kernel-literals)
        [(quote x)      `(quote ,(variable #'x))]
        [x              `,(variable #'x)])))
  ; (PhaselessSpec     (ps)  rmp)
  ; (RawModulePath     (rmp) rrmp)
  
  (Expr : * (E) -> Expr ()
    (with-output-language (LFE Expr)
      (syntax-parse E #:literal-sets (kernel-literals)
        [x:id                                      `,(variable #'x)]
        [c #:when (constant? (syntax->datum #'c))  `(quote ,E ,(Datum E #'c))]
        [(if e0 e1 e2)                             `(if ,E ,(Expr #'e0) ,(Expr #'e1) ,(Expr #'e2))]
        [(begin  e0 e1 ...)                        `(begin  ,E ,(Expr #'e0) ,(Expr* #'(e1 ...)) ...)]
        [(begin0 e0 e1 ...)                        `(begin0 ,E ,(Expr #'e0) ,(Expr* #'(e1 ...)) ...)]
        [(#%plain-lambda f e ...+)                 `(λ ,E ,(Formals #'f)
                                                      ,(Expr*  #'(e ...)) ...)]
        [(case-lambda (f e ...+) ...)              (let* ([f (Formals* #'(f ...))]
                                                          [e (Expr**   #'((e ...) ...))])
                                                     (match e
                                                       [(list (list e0 e ...) ...)
                                                        `(case-lambda ,E (,f ,e0 ,e ...) ...)]))]
        [(quote d)                                 `(quote ,E ,(Datum E #'d))]
        [(quote-syntax d)                          `(quote-syntax ,E ,(Datum E #'d))]
        [(let-values ([(x ...) e] ...) e0 e1 ...)  (let ()
                                                     (define xss (map variable*
                                                                      (syntax->list #'((x ...) ...))))
                                                     (define es  (Expr* #'(e ...)))
                                                     `(let-values ,E ([(,xss ...) ,es] ...)
                                                        ,(Expr #'e0) ,(Expr* #'(e1 ...)) ...))]
        ;; Temporarily expand letrec such that letrec only binds lambdas
        [(letrec-values ([(x ...) e] ...) e0 e1 ...)
         ;    (letrec-values ([(x ...) ce] ...) e)
         ; => (let ([x undefined] ... ...])
         ;      (letrec ([xl le] ...)
         ;        (let-values ([(t ...) ce])
         ;           (set! x t) ...)
         ;        ...
         ;        e)
         ; where x is divided into xl.
         ; Note: Now the code generator doesn't have to deal with multiple values
         ;       returned from e.
         (define (lambda-clause? stx)
           (syntax-parse stx #:literal-sets (kernel-literals) [[(x) (#%plain-lambda . _)] #t] [_ #f]))
         (define clauses (syntax->list #'([(x ...) e] ...)))
         ; partition clauses into lambda clauses and complex clauses
         (define-values (lambda-clauses complex-clauses) (partition lambda-clause? clauses))
         (define/with-syntax ([(xl)     le] ...) lambda-clauses)  ; xl (an x bound to lambda)
         (define/with-syntax ([(xc ...) ce] ...) complex-clauses) ; xc (an x bound to a complex expression)
         (let* ([xc* (variable* (syntax->list #'(xc ... ...)))]
                [0s  (map (λ(_) `(quote ,E ,(datum E 0))) xc*)]
                [xl  (variable* (syntax->list #'(xl ...)))]
                [le  (Expr* #'(le ...))])
           (define (build-begin s . Es)
             (match (append* Es)
               [(list E0)        E0]
               [(list E0 E1 ...) `(begin ,s ,E0 ,E1 ...)]))
           `(let-values ,E ([(,xc*) ,0s] ...)      ; declare as undefined
              (letrec-values ,E ([(,xl) ,le] ...)  ; fix lambda expressions
                ,(build-begin
                  E (for/list ([complex complex-clauses])
                      (syntax-parse complex
                        [[(xc)     ce]  (Expr #'(set! xc ce))]
                        [[(xc ...) ce]  (with-syntax ([(tc ...) (generate-temporaries #'(xc ...))])
                                          (Expr #'(let-values ([(tc ...) ce])
                                                    (set! xc tc) ...)))]))
                  (Expr* #'(e0 e1 ...))))))]
        [(set! x:id e)                              `(set! ,E ,(variable #'x) ,(Expr #'e))]
        [(with-continuation-mark e0 e1 e2)          `(wcm ,E ,(Expr #'e0) ,(Expr #'e1) ,(Expr #'e2))]
        [(#%plain-app e0 e1 ...)                    `(app ,E ,(Expr #'e0) ,(Expr* #'(e1 ...)) ...)]
        [(#%top . x)                                `(top ,E ,(variable #'x))]
        [(#%variable-reference)                     `(variable-reference ,E (anonymous ,E))]
        [(#%variable-reference (#%top . x:id))      `(variable-reference ,E (top ,E ,(variable #'x)))]
        [(#%variable-reference x:id)                `(variable-reference ,E ,(variable #'x))]
        [_ (displayln E)
           (error 'parse "expected <expression> got: ~a" E)])))
  
  ; start parsing
  (TopLevelForm stx))


(define (unparse-all x)
    ; for some reason, Nanopass no longer unparse variables and datums.
    (cond
      [(variable? x) (unparse-variable x)]
      [(datum? x)    (unparse-datum x)]
      [(pair? x)     (cons (unparse-all (car x))
                           (unparse-all (cdr x)))]
      [else          x]))

(module+ test
  (let ([test (λ (stx) (unparse-all (unparse-LFE (parse (expand-syntax stx)))))])
    (check-equal? (test #'y) '(#%top . y))
    (check-equal? (test #''(1 x)) ''(1 x))
    (check-equal? (test #'(#%plain-lambda (x) '1)) '(λ (x) '1))
    (check-equal? (test #'(let-values ([(x y) 1]) 3)) '(let-values ([(x y) '1]) '3))
    (check-equal? (test #'(let-values ([(x y) 1] [(z) 2]) 3 4))
                  '(let-values ([(x y) '1] [(z) '2]) '3 '4))
    ; todo: insert test of letrec
    (check-equal? (test #'(set! x 3)) '(set! x '3))
    (check-equal? (test #'(if 1 2 3)) '(if '1 '2 '3))
    (check-equal? (test #'(begin  1 2 3)) '(begin  '1 '2 '3))
    (check-equal? (test #'(begin0 1 2 3)) '(begin0 '1 '2 '3))
    (check-equal? (test #'(with-continuation-mark 1 2 3)) '(with-continuation-mark '1 '2 '3))
    (check-equal? (test #'(#%plain-app + 1 2)) '(+ '1 '2))
    (check-equal? (test #'(#%top . foo)) '(#%top . foo))
    (check-equal? (test #'(λ (x . y) (+ x y))) '(#%expression (λ (x . y) (+ x y))))
    (check-equal? (test #'(λ x (+ x 1))) '(#%expression (λ x (+ x '1))))

    (check-equal? (test #'(module test webracket 11))
                  '(module test webracket (#%plain-module-begin '11)))
    (check-equal? (test #'(module test webracket (fx+ 11 22)))
                  '(module test webracket (#%plain-module-begin (fx+ '11 '22))))))
    

;;;
;;; QUOTATIONS
;;;

;;; Atomic values that are allowed as quotations in WebAssembly are unchanged.
;;; Other values are collected.
;;; A quotation  '(foo bar) becomes   _quote44
;;; and      (define _quote42 (string->symbol "foo"))
;;;          (define _quote43 (string->symbol "bar"))
;;;          (define _quote44 (list _quote42 _quote42))
;;; are lifted to the beginning of the top-level.
;;; Optimization:
;;;    Since symbols are interned, multiple quotations of the same symbol
;;;    will reference the same _quote variable.

(define-pass convert-quotations : LFE (T) -> LFE ()
  (definitions
    (define h #'cq)
    (define quotation-defines '())  ; in reverse order    
    (define symbols-ht (make-hasheq))
    (define (add-quotation! define-form)
      (set! quotation-defines (cons define-form quotation-defines)))
    (define (quoted-symbol! s sym)
      (match (hash-ref symbols-ht sym #f)
        [#f (let ([id (new-var #'_quote)] [str (symbol->string sym)])
              (with-output-language (LFE GeneralTopLevelForm)
                (hash-set! symbols-ht sym id)
                (add-quotation!
                 `(define-values ,h (,id)
                    (app ,h ,(var:string->symbol)
                         ,(datum->construction-expr s str)))))
              id)]
        [id id]))
    (define (datum->construction-expr s v)
      (define (loop v) (datum->construction-expr s v))
      (with-output-language (LFE Expr)
        (match v
          ; this should match the definition of wr-datum?
          [(and (or (? wr-fixnum?)
                    (? flonum?)
                    (? boolean?)
                    (? char?)
                    (? null?))
                v)
           `(quote ,s ,(datum s v))]
          [(? symbol? sym)  (quoted-symbol! s sym)]
          [(? pair? p)      `(app ,h ,(var:cons) ,(loop (car p)) ,(loop (cdr p)))]
          [(? vector? v)    (let ([vs (map loop (vector->list v))])
                              `(app ,h ,(var:vector-immutable) ,vs ...))]
          [(? box? b)        `(app ,h ,(var:box-immutable) ,(loop (unbox b)))]
          [(? bytes? bs)    (let ([bs (map loop (bytes->list bs))])
                              `(app ,h ,(var:bytes) ,bs ...))]
          [(? string? cs)   (let ([cs (loop (string->list cs))])
                              `(app ,h ,(var:list->string) ,cs))]
          [(? keyword? kw)  (let ([s (loop (keyword->string kw))])
                              `(app ,h ,(var:string->keyword) ,s))]
          [else             (error 'datum->construction-expr "got: ~a" v)]))))
  (Expr : Expr (e) -> Expr ()
    [(quote ,s ,d)
     (datum->construction-expr s (datum-value d))]
    [(quote-syntax ,s ,d)
     (let* ([src (syntax-source s)]
            [src (if (path? src)   (path->string src)   src)] ; no paths yet ...
            [src (if (symbol? src) (symbol->string src) src)] ; keep it simple for now ...
            [src   (Expr `(quote ,s ,(datum s src)))]
            [l     (Expr `(quote ,s ,(datum s (syntax-line s))))]
            [c     (Expr `(quote ,s ,(datum s (syntax-column s))))]
            [p     (Expr `(quote ,s ,(datum s (syntax-position s))))]
            [sp    (Expr `(quote ,s ,(datum s (syntax-span s))))]
            [false (Expr `(quote ,s ,(datum s #f)))]
            [v     (Expr `(quote ,s ,d))])
       ; (displayln (list 'generate-ur: 'quote-syntax s d (list src l c p sp)))
       ; srcloc(source,line,column,position,span)
       ; make_syntax_object(source_location,lexical_info,datum)       
       (Expr `(quote ,s ,(datum s #f))) ; todo: use the definition below instead

       #;`(app ,s ,(var:make-syntax-object)
             (app ,s ,(var:srcloc) ,src ,l ,c ,p ,sp) ; source location
             ,false                                   ; lexical info
             ,v))])
  (TopLevelForm    : TopLevelForm    (t)  -> TopLevelForm    ())

  (let ([T (TopLevelForm T)]
        [quotations (reverse quotation-defines)])
    `(topbegin ,h ,quotations ... ,T)))


;;;
;;; explicit-begin
;;;

;;  Rewrite the bodies of λ, let-values and letrec-values to a single expression.
;;  This simplifies the treatment of let-values and letrec-values later on.
;;  (Read: Fewer rules contains the pattern e ... )

(define-language LFE1 (extends LFE)
  (Expr (e)
    (- (λ s f e ...)
       (let-values    s ([(x ...) e] ...) e0 e1 ...)
       (letrec-values s ([(x ...) e] ...) e0 e1 ...)
       (case-lambda s (f e0 e ...) ...))
    (+ (λ s f e)                               => (λ f e)
       (let-values     s ([(x ...) e] ...) e0) => (let-values    ([(x ...) e] ...) e0)
       (letrec-values  s ([(x ...) e] ...) e0) => (letrec-values ([(x ...) e] ...) e0)
       (case-lambda    s (f e) ...)            => (case-lambda   (f e) ...))))

(define-language LFE2 (extends LFE1)
  (Abstraction (ab)
    (+ (λ s f e) => (λ f e)))
  (Expr (e)
    (- (λ s f e)
       (case-lambda s (f e) ...))
    (+ ab
       (case-lambda s ab ...) => (case-lambda ab ...))))


(define-pass explicit-begin : LFE (T) -> LFE1 ()
  (definitions
    (define h #'eb) ; h = here = source location info
    (define (Begin s e0 es)
      ;; don't wrap single expressions in (begin ...)
      (with-output-language (LFE1 Expr)
        (match (length es)
          [0 e0]
          [_ `(begin ,s ,e0 ,es ...)]))))
  (TopLevelForm        : TopLevelForm        (T) -> TopLevelForm        ())
  (ModuleLevelForm     : ModuleLevelForm     (M) -> ModuleLevelForm     ())
  (GeneralTopLevelForm : GeneralTopLevelForm (G) -> GeneralTopLevelForm ())
  (Formals             : Formals             (F) -> Formals             ())
  (Expr : Expr (E) -> Expr ()
    [(λ ,s ,[f] ,[e0] ,[e] ...)
     `(λ ,s ,f ,(Begin h e0 e))]
    [(let-values     ,s ([(,x ...) ,[e]] ...) ,[e0] ,[e1] ...)
     `(let-values    ,s ([(,x ...) ,e] ...) ,(Begin h e0 e1))]
    [(letrec-values  ,s ([(,x ...) ,[e]] ...) ,[e0] ,[e1] ...)
     `(letrec-values ,s ([(,x ...) ,e] ...) ,(Begin h e0 e1))]
    [(case-lambda ,s (,[f] ,[e0] ,[e] ...) ...)
     (let* ([hs (map (λ(_) h) f)]
            [e  (map Begin hs e0 e)])
       `(case-lambda ,s (,f ,e) ...))]))

(module+ test
    (let ([test (λ (stx)
                  (unparse-all
                   (unparse-LFE1
                    (explicit-begin
                     (parse                  
                      (expand-syntax stx))))))])
      (check-equal? (test #'(let-values ([(x) 1]) 2 3))
                    '(let-values ([(x) '1]) (begin '2 '3)))))


;;;
;;; explicit-case-lambda
;;;

;;  Rewrite case-lambda to have explicit lambda.
;;             (case-lambda [f e] ...)
;;  becomes    (case-lambda (λ f e) ...)


(define-pass explicit-case-lambda : LFE1 (T) -> LFE2 ()
  (definitions
    (define h #'ecl)) ; h = here = source location info
  (TopLevelForm        : TopLevelForm        (T) -> TopLevelForm        ())
  (ModuleLevelForm     : ModuleLevelForm     (M) -> ModuleLevelForm     ())
  (GeneralTopLevelForm : GeneralTopLevelForm (G) -> GeneralTopLevelForm ())
  (Formals             : Formals             (F) -> Formals             ())
  (Expr : Expr (E) -> Expr ()
    [(λ ,s ,[f] ,[e])
     (with-output-language (LFE2 Abstraction)
       `(λ ,s ,f ,e))]
    [(case-lambda ,s (,[f] ,[e]) ...)
     (let ([s* (map (λ(_) s) f)])
       `(case-lambda ,s (λ ,s* ,f ,e) ...))]))




;;;
;;; Remove begin0
;;;

; This pass illustrates how to write a simple pass that rewrites
; a single construct. Notice that a rule for each nonterminal is needed.
; The [_] in (begin0 ,s ,[e0] ,[e1] ...) ensures that e0 e1 .. are
; already rewritten into LFE3 Expression when the right hand side
; is evaluated. Note also that the variable t0 is not represented
; as a symbol, and thus  (let ([t0 (new-var 't0)]) ... ,t0) is
; needed to bind t0 to a variable (represented as a struct).

;(define-language LFE3 (extends LFE1)
;    (Expr (e)
;      (- (begin0 s e0 e1 ...))))

;(define-pass remove-begin0 : LFE1 (T) -> LFE3 ()
;    (definitions (define h #'rb0)) ; h = here = source location info    
;    (TopLevelForm        : TopLevelForm        (T) -> TopLevelForm        ())
;    (ModuleLevelForm     : ModuleLevelForm     (M) -> ModuleLevelForm     ())
;    (GeneralTopLevelForm : GeneralTopLevelForm (G) -> GeneralTopLevelForm ())
;    (Formals             : Formals             (F) -> Formals             ())
;    (Expr : Expr (E) -> Expr ()
;      [(begin0 ,s ,[e0] ,[e1] ...)
;       ; TODO: This rewrite rule breaks if e0 returns multiple values
;       (let ([t0 (new-var 't0)])
;         `(let-values ,s ([(,t0) ,e0]) ,`(begin ,h ,t0 ,e1 ... ,t0)))]))





;;;
;;; α-RENAMING
;;;

; To make the next pass easier to implement we do α-renaming.

; The α-renaming pass makes names unique.
;
; Example:
;                 (let ([x 1]) (let ([x   2]) x))
; == α-rename ==> (let ([x 1]) (let ([x.2 2]) x.2))
;
; The code below uses an environment ρ that maps original identifiers
; into ones used in the output. Identifiers refering to primitives are
; mapped to themselves in order not to rename primitives in the output.

; FACT: α-renaming will not rename identifiers bound to primitives.
;       After renaming an indentifier that "looks like a primitive is a
;       primitive".
;       Example:  (begin (define + 42) (+ 1 2))))))
;         becomes (begin (define-values (|+.8|) '42) (|+.8| '1 '2))
;         The + that didn't refer to a primitive was renamed.

; An unbound identifier is mapped to #f by the environment ρ.

; The expression (fresh x ρ) returns a new identifier not already mapped in ρ.
; The new name is based in the identifier x.

;; New identifiers are produced by new-var.

(define counter 0)
(define joiner ".")
(define (new-var [prefix "t"])
  (set! counter (+ counter 1))
  (define pre (cond
                [(syntax?   prefix) (syntax-e prefix)]
                [(variable? prefix) (syntax-e (variable-id prefix))]
                [else prefix]))
  (variable (datum->syntax #'here (string->symbol (~a pre joiner counter)))))
(define (reset-counter! [joiner ""])
  (set! counter 0))
(define-syntax (with-fresh* stx)
  (syntax-parse stx
    [(_with-fresh* (ts:id xs:expr) b ...)
     (syntax/loc stx
       (let ([ts (map new-var xs)])
         b ...))]))

(define (id=? v1 v2)
  ; After α-renaming all names are unique so we can check for symbolic equality.
  (eq? (syntax-e (variable-id v1)) (syntax-e (variable-id v2))))


(define-pass α-rename : LFE2 (T) -> LFE2 ()
  (definitions
    (define (reserved-target-language-keyword? id)
      ; Any reserved keywords in the target language needs renaming.
      ; In WebAssembly the "user" names are prefixed with $, so
      ; there is nothing to worry about.
      #f)
    (define (initial-ρ x)
      (cond [(reserved-target-language-keyword? x) => values]
            [(primitive? (variable-id x))             x]
            [else                                    #f]))
    (define (extend ρ original renamed)
      (λ (x) (if (id=? x original) renamed (ρ x))))
    (define (fresh x ρ [orig-x x])
      (if (ρ x) (fresh (new-var x) ρ x) x))
    (define (rename x ρ)
      (define x* (fresh x ρ))
      (values x* (extend ρ x x*)))
    (define (rename*          xs ρ) (map2* rename   xs ρ))
    (define (rename**        xss ρ) (map2* rename* xss ρ))
    (define (TopLevelForm*    xs ρ) (map2* TopLevelForm xs ρ))
    (define (ModuleLevelForm* xs ρ) (map2* ModuleLevelForm xs ρ))
    (define (Formals*         fs ρ) (map2* Formals fs ρ))
    (define (Abstraction*    abs ρ) (map2* Abstraction abs ρ))
    ; Expr* : no threading! all e use same env
    (define (Expr*         es ρ) (map (λ (e) (letv ((e _) (Expr e ρ)) e)) es))    
    (define (Binding* xss es ρ)
      (letv ((es) (Expr* es ρ))  ; xss are not bound in es
        (letv ((xss ρ) (rename** xss ρ))
          (values xss es ρ))))
    (define (RecBinding* xss es ρ)
      (letv ((xss ρ) (rename** xss ρ))
        (letv ((es) (Expr* es ρ)) ; xss are bound in es
              (values xss es ρ))))
    ; References to unbound variables within a module leads to an error.
    ; Outside modules, no errors will be signaled.
    ; Example: Try (if 1 2 x) in the repl.
    (define inside-module? (make-parameter #f)))
  
  (TopLevelForm : TopLevelForm (T ρ) -> TopLevelForm (ρ)
    [(topbegin ,s ,t ...)           (letv ((t ρ) (TopLevelForm* t ρ))
                                          (values `(topbegin ,s ,t ...) ρ))]
    [(#%expression ,s ,e)           (letv ((e ρ) (Expr e ρ))
                                          (values `(#%expression ,s ,e) ρ))]
    [(topmodule ,s ,mn ,mp ,mf ...) (letv ((mf ρ) (parameterize ([inside-module? #t])
                                                    (ModuleLevelForm* mf ρ)))
                                          (values `(topmodule ,s ,mn ,mp ,mf ...) ρ))]
    [,g                             (letv ((g ρ) (GeneralTopLevelForm g ρ))
                                          (values `,g ρ))])

  (ModuleLevelForm : ModuleLevelForm (M ρ) -> ModuleLevelForm (ρ)
    [,g                   (letv ((g ρ) (GeneralTopLevelForm g ρ))
                            (values `,g ρ))])

  (GeneralTopLevelForm : GeneralTopLevelForm (G ρ) -> GeneralTopLevelForm (ρ)
    [,e                                 (letv ((e ρ) (Expr e ρ))
                                          (values `,e ρ))]
    [(define-values   ,s (,x ...) ,e)   (letv ((x ρ) (rename* x ρ))
                                          (letv ((e _) (Expr e ρ))
                                            (values `(define-values ,s (,x ...) ,e) ρ)))]
    [(define-syntaxes ,s (,x ...) ,e)   (error)])
  
  (Formals : Formals (F ρ) -> Formals (ρ)
    [(formals (,x ...))               (letv ((x ρ) (rename* x ρ))
                                        (values `(formals (,x ...)) ρ))]
    [(formals (,x0 ,x1 ... . ,xd))    (letv ((x0 ρ) (rename x0 ρ))
                                        (letv ((x1 ρ) (rename* x1 ρ))
                                          (letv ((xd ρ) (rename xd ρ))
                                            (values `(formals (,x0 ,x1 ... . ,xd)) ρ))))]
    [(formals ,x)                     (letv ((x ρ) (rename x ρ))
                                        (values `(formals ,x) ρ))])
  
  (Abstraction : Abstraction (AB ρ) -> Abstraction (ρ)
    [(λ ,s ,f ,e)                               (let ([ρ-orig ρ])
                                                  (letv ((f ρ) (Formals f ρ))
                                                    (letv ((e ρ) (Expr e ρ))
                                                      (values `(λ ,s ,f ,e) ρ-orig))))])
  (Expr : Expr (E ρ) -> Expr (ρ)
    [,x                                         (let ([ρx (ρ x)])
                                                  (cond
                                                    [(and (not ρx) (inside-module?))
                                                     (raise-syntax-error
                                                      'α-rename "compiler.rkt: unbound variable"
                                                      (variable-id x))]
                                                    [(not ρx)
                                                     ; signal unbound variable at runtime
                                                     (values `(app ,#'here
                                                                   ,(variable #'raise-unbound-variable-reference)
                                                                   ; Note: `datum` has been eliminated at this point,
                                                                   ;       so a different approach is needed
                                                                   ; ,`(quote ,#'here  ,(datum #'unbound (variable-id x)))
                                                                   )
                                                             ρ)]
                                                    [else
                                                     (values `,ρx ρ)]))]
    [,ab                                        (Abstraction ab ρ)]
    [(case-lambda ,s ,ab ...)                   (let ([ρ-orig ρ])
                                                  (let-values ([(ab ρ) (Abstraction* ab ρ)])
                                                    (values `(case-lambda ,s ,ab ...) ρ-orig)))]
    [(if ,s ,e0 ,e1 ,e2)                        (letv ((e0 ρ) (Expr e0 ρ))
                                                  (letv ((e1 ρ) (Expr e1 ρ))
                                                    (letv ((e2 ρ) (Expr e2 ρ))
                                                      (values `(if ,s ,e0 ,e1 ,e2) ρ))))]
    [(begin  ,s ,e0 ,e1 ...)                    (letv ((e0 _) (Expr e0 ρ))
                                                  (letv ((e1) (Expr* e1 ρ))
                                                    (values `(begin ,s ,e0 ,e1 ...) ρ)))]
    [(begin0 ,s ,e0 ,e1 ...)                    (letv ((e0 _) (Expr e0 ρ))
                                                  (letv ((e1) (Expr* e1 ρ))
                                                    (values `(begin0 ,s ,e0 ,e1 ...) ρ)))]
    [(let-values ,s ([(,x ...) ,e] ...) ,e0)    (let ([ρ-orig ρ]) 
                                                  (letv ((x e ρ) (Binding* x e ρ))
                                                    (letv ((e0 ρ) (Expr e0 ρ))
                                                      (values `(let-values ,s ([(,x ...) ,e] ...)
                                                                 ,e0) ρ-orig))))]
    [(letrec-values ,s ([(,x ...) ,e] ...) ,e0) (let ([ρ-orig ρ])
                                                  (letv ((x e ρ) (RecBinding* x e ρ))
                                                    (letv ((e0 ρ) (Expr e0 ρ))
                                                      (values `(letrec-values
                                                                   ,s ([(,x ...) ,e] ...)
                                                                 ,e0) ρ-orig))))]
    [(set! ,s ,x ,e)                            (let ([x (or (ρ x) x)]) ; ignores unbound x
                                                  ; note: If x is unbound, then a module-level
                                                  ; assignment should give an error.
                                                  ; A top-level assignment is ok.
                                                  ; Compare: Racket compile
                                                  (letv ((e ρ) (Expr e ρ))
                                                    (values `(set! ,s ,x ,e) ρ)))]
    [(wcm ,s ,e0 ,e1 ,e2)                       (letv ((e0 ρ) (Expr e0  ρ))
                                                  (letv ((e1 ρ) (Expr e1  ρ))
                                                    (letv ((e2 ρ) (Expr e2  ρ))
                                                      (values
                                                       `(wcm ,s ,e0 ,e1 ,e2) ρ))))]
    [(app ,s ,e0 ,e1 ...)                     (letv ((e0 _) (Expr e0 ρ))
                                                (letv ((e1) (Expr* e1 ρ))
                                                  (values `(app ,s ,e0 ,e1 ...) ρ)))]
    ; Note: top-level-variables are looked up by name in the namespace,
    ;       so they can't be renamed.
    [(top ,s ,x)                              (values `(top ,s ,x) ρ)]
    [(variable-reference ,s ,vrx)             (values E ρ)])
  
  (letv ((T ρ) (TopLevelForm T initial-ρ))
    T))

  ;; (VariableReferenceId (vrx)
  ;;    x                                            
  ;;    (anonymous s)                                => ()
  ;;    (top s x)                                    => (#%top . x)))


(module+ test
  (let ([test (λ (stx)
                (reset-counter!)
                (unparse-all
                 (unparse-LFE2 (α-rename
                                (explicit-case-lambda
                                 (explicit-begin
                                  (parse (expand-syntax stx))))))))])
      (check-equal? (test #'(let ([x 10])
                              (let ([x 1]
                                    [y x])
                                y)))
                    '(let-values (((x) '10))
                       (let-values (((x.1) '1)
                                    ((y)   x))
                         y)))
      
      (check-equal? (test #'(let-values ([(x y z) 1] [(a b c) 2])
                              (let-values ([(x y) 1] [(a b) 2])
                                (begin x y z a b c))))
                    '(let-values (((x y z) '1) ((a b c) '2))
                       (let-values (((x.1 y.2) '1) ((a.3 b.4) '2))
                         (begin x.1 y.2 z a.3 b.4 c))))))


;;;
;;; ASSIGNMENT CONVERSION
;;;

;; Assignment conversion consists of two sub-passes:
;;     - collect-assignable-variables
;;     - box-mutables

;; The first pass returns an id-set of all variables that are (potentially) assigned to.
;; The second pass converts assignable variables into boxes and assignments into box mutations.

(define-pass collect-assignable-variables : LFE2 (T) -> * ()
  ;; Assumption: α-conversion has been done
  (definitions
    (define (TopLevelForm*    Ts  xs)          (map* TopLevelForm    Ts  xs))
    (define (ModuleLevelForm* Ms  xs)          (map* ModuleLevelForm Ms  xs))
    (define (Expr*            Es  xs)          (map* Expr            Es  xs))
    (define (Abstraction*     ABs xs) (append* (map* Abstraction     ABs xs))))
  (TopLevelForm : TopLevelForm (T xs) -> * (xs)
    [(topbegin ,s ,t ...)           (TopLevelForm* t xs)]
    [(#%expression ,s ,e)           (Expr e xs)]
    [(topmodule ,s ,mn ,mp ,mf ...) (ModuleLevelForm* mf xs)]
    [,g                             (GeneralTopLevelForm g xs)])
  (ModuleLevelForm : ModuleLevelForm (M xs) -> * (xs)
    [,g                             (GeneralTopLevelForm g xs)])
  (GeneralTopLevelForm : GeneralTopLevelForm (G xs) -> * (xs)
    [,e                                 (Expr e xs)]
    ; until we implement namespace, top-level variables are boxed
    [(define-values   ,s (,x ...) ,e)   (Expr e (append x xs))]
    [(define-syntaxes ,s (,x ...) ,e)   (Expr e xs)]
    [(#%require       ,s ,rrs ...)      empty-set])
  (Abstraction : Abstraction (AB xs) -> * (xs)
    [(λ ,s ,f ,e)                               (Expr e xs)])
  (Expr : Expr (E xs) -> * (xs)
    [,x                                         xs]
    [,ab                                        (Abstraction ab xs)]
    [(case-lambda ,s ,ab ...)                   (Abstraction* ab xs)]
    [(if ,s ,e0 ,e1 ,e2)                        (Expr* (list e0 e1 e2) xs)]
    [(begin  ,s ,e0 ,e1 ...)                    (Expr* (cons e0 e1) xs)]
    [(begin0 ,s ,e0 ,e1 ...)                    (Expr* (cons e0 e1) xs)]
    [(let-values    ,s ([(,x ...) ,e] ...) ,e0) (Expr* e (Expr e0 xs))]
    [(letrec-values ,s ([(,x ...) ,e] ...) ,e0) (Expr* e (Expr e0 xs))]
    [(set! ,s ,x ,e)                            (cons x (Expr e xs))]    
    [(top ,s ,x)                                xs]
    [(variable-reference ,s ,vrx)               xs]
    [(quote ,s ,d)                              xs]
    [(quote-syntax ,s ,d)                       xs]
    [(wcm ,s ,e0 ,e1 ,e2)                       (Expr* (list e0 e1 e2) xs)]
    [(app ,s ,e0 ,e1 ...)                       (Expr* (cons e0 e1) xs)])  
  (apply make-id-set (TopLevelForm T '())))

(define-pass box-mutables : LFE2 (T ms) -> LFE2 ()
  ;; Assumption: α-conversion has been done
  ;;  ms = assignable variables collected by collect-assignable-variables
  
  ;; The basic idea is to heap allocate the location for a mutable variable.
  ;; If (set! x e) is present in the program, then x is a mutable variable.
  ;; When a mutable variable is bound, it is bound to box keeping the value,
  ;; all assignments are then transformed into set-box.
  ;;    (let ([x 1]) (set! x 2))  ==>  (let ([x (boxed 1)]) (set-boxed! x 2))
  ;; In a (let-values ([(x ...) e] ...) b) expression the e expressions
  ;; can return multiple values, and only those corresponding to mutable
  ;; variables need to be boxed. Example:
  ;;     (let-values ([(x y) (values 1 2)]) (set! x 3))
  ;; ==> (let-values ([(x y) (let-values ([(tx ty) (values 1 2)])
  ;;                           (values (boxed tx) ty))
  ;;       (set-boxed! x 3))
  ;; Note that only clauses with mutable variables need this extra let-values expression.
  (definitions
    (define h #'ac)
    (define (mutable? x)   (set-in? x ms))
    (define (immutable? x) (not (mutable? x)))    
    (define (formal-variables F)
      ; list of variables occuring as formal arguments
      (nanopass-case (LFE2 Formals) F
        [(formals (,x ...))            x]
        [(formals (,x0 ,x1 ... . ,xd)) (cons x0 (append x1 (list xd)))]
        [(formals ,x)                  (list x)]))
    (define (Boxed e)   (with-output-language (LFE2 Expr) `(app ,h ,(var:boxed) ,e)))
    (define (Unboxed e) (with-output-language (LFE2 Expr) `(app ,h ,(var:unboxed) ,e)))
    (define (Undefined) (with-output-language (LFE2 Expr) `(quote ,h ,datum:undefined)))
    (define (LambdaBody s f fs e)
      ; fs are the variables to be bound in the body e
      (with-output-language (LFE2 Expr)
        (define (Begin es)  (match es [(list e0 e1 ...) `(begin ,h ,e0 ,e1 ...)]))
        (match (set-disjoint? fs ms) 
          [#t `,e]
          [_ (Begin (append (for/list ([a (set-intersection fs ms)])
                              `(set! ,h ,a ,(Boxed a)))
                            (list e)))])))
    (define (RHS xs e) ; rewrite right hand side of binding clause
      (define n (length xs))
      (if (andmap immutable? xs)
          e
          (match n
            [0 e]
            [1 (if (mutable? (first xs)) (Boxed e) e)]
            [_ (with-fresh* (ts xs)
                 (with-output-language (LFE2 Expr)
                   (let ([bt (for/list ([x xs] [t ts])
                               (if (mutable? x) (Boxed t) t))])
                     `(let-values ,h ([(,ts ...) ,e])
                        (app ,h ,(var:values) ,bt ...)))))]))))
  (TopLevelForm        : TopLevelForm        (T) -> TopLevelForm        ())
  (Formals             : Formals             (F) -> Formals             ())
  (ModuleLevelForm     : ModuleLevelForm     (M) -> ModuleLevelForm     ()
    #;[,g                   (let ([g (GeneralTopLevelForm g)])
                            `,g)])
  (GeneralTopLevelForm : GeneralTopLevelForm (G) -> GeneralTopLevelForm ()
    [(define-values   ,s (,x ...) ,[e]) `(define-values   ,s (,x ...) ,(RHS x e))]
    [(define-syntaxes ,s (,x ...) ,[e]) `(define-syntaxes ,s (,x ...) ,(RHS x e))])
  (Abstraction : Abstraction (AB) -> Abstraction ()
    [(λ ,s ,[f] ,[e])   (let ([lb (LambdaBody s f (formal-variables f) e)])
                          `(λ ,s ,f ,lb))])
  (Expr : Expr (E) -> Expr ()
    ; variable reference, set! and all binding forms must be rewritten
    [,x                       (if (set-in? x ms) (Unboxed x) x)]
    [(set! ,s ,x ,[e])        `(app ,h ,(var:set-boxed!) ,x ,e)]
    [,ab                      (Abstraction ab)]
    [(case-lambda ,s ,ab ...) (let ([ab (map Abstraction ab)])
                                `(case-lambda ,s ,ab ...))]
    [(let-values    ,s ([(,x ...) ,[e*]] ...) ,[e0])
     (let ([e* (for/list ([e e*] [xs x]) (RHS xs e))])
       `(let-values ,s ([(,x ...) ,e*] ...) ,e0))]
    [(letrec-values ,s ([(,x ...) ,[e*]] ...) ,[e0])
     (let ([e* (for/list ([e e*] [xs x]) (RHS xs e))])
       `(letrec-values ,s ([(,x ...) ,e*] ...) ,e0))])
  (TopLevelForm T))

(define (assignment-conversion T) ; LFE2 -> LFE2
  ; more convenient to use assignment-conversion than
  ; calling collect-assignable-variables and box-mutables in order
  ; (since  assignment-conversion has type T -> T)
  (box-mutables T (collect-assignable-variables T)))

(module+ test
  (let ()
    (define (test stx)
      (reset-counter!)
      (unparse-all
       (unparse-LFE2
        (assignment-conversion
         (α-rename
          (explicit-case-lambda
           (explicit-begin
            (parse
             (expand-syntax stx)))))))))
    (check-equal? (test #'(set! x 1)) '(set-boxed! x '1))
    (check-equal? (test #'(λ (x) (set! x 1) x))
                  '(#%expression
                    (λ (x)
                      (begin (set! x (boxed x))
                             (begin (set-boxed! x '1)
                                    (unboxed x))))))
    (check-equal? (test #'(let-values ([(x) 1]) (set! x 2) x))
                  '(let-values (((x) (boxed '1))) (begin (set-boxed! x '2) (unboxed x))))
    #;(check-equal? (test #'(letrec-values ([(x y) (begin (set! x 1) (+ x y))]) (- x y)))
                  '(let-values (((x) (box '0)) ((y) (box '0)))
                     (letrec-values ()
                       (begin
                         (let-values (((x17 y18)
                                       (begin (set-box! x '1)
                                              (+ (unbox x) (unbox y)))))
                           (begin (set-box! x x17)
                                  (set-box! y y18)))
                         (- (unbox x) (unbox y))))))
    #;(check-equal? (test #'(define-values (x y) (begin (set! x 2) (+ x y))))
                  '(define-values (x y)
                     (let-values (((x.1 y.2)
                                   (begin (set-box! x '2) (+ (#%top . x) (#%top . y)))))
                       (values (box x.1) y.2))))
    (check-equal? (test #'(λ (x) x)) '(#%expression (λ (x) x)))))


;;;
;;; CATEGORIZE APPLICATIONS
;;;

;; Application (app s e0 e1 ...) is rewritten into one of the following:
;;    (primapp     s pr  e1 ...)    application of a  primitive 
;;    (closedapp   s ab  e1 ...)    application of an abstraction
;;    (app         s e0  e1 ...)    application of a  general procedure

;; Closed applications of the type ((λ (x ...) e) e) are rewritten to let-values.
;; Closed application where the abstraction have formals of the form (x ... . y) and x
;; are kept (for now).

;; The terminal pr is a variable bound to a primitive.

(define-language LFE3 (extends LFE2)
  (terminals
   (+ ((primitive (pr)) . => . unparse-primitive)))
  (CaseAbstraction (cab)
    (+ (case-lambda s ab ...) => (case-lambda ab ...)))
  (Expr (e)
    (- (case-lambda s ab ...)
       (app s e0 e1 ...))
    (+ cab
       (primapp   s pr e1 ...)      => (primapp   pr e1 ...)
       (closedapp s ab e1 ...)      => (closedapp ab e1 ...)
       (app       s e0 e1 ...)      => (app       e0 e1 ...))))

(define-pass categorize-applications : LFE2 (T) -> LFE3 ()
  (definitions
    (define (Expr* Es) (map Expr Es))
    (define (Values s es)
      ; avoid wrapping a single expression in a values form
      (with-output-language (LFE3 Expr)
        (match (length es)
          [1 (first es)]
          [_ `(primapp ,s ,(var:values) ,es ...)]))))
  (Formals             : Formals             (F) -> Formals             ())
  (TopLevelForm        : TopLevelForm        (T) -> TopLevelForm        ())
  (ModuleLevelForm     : ModuleLevelForm     (M) -> ModuleLevelForm     ())
  (GeneralTopLevelForm : GeneralTopLevelForm (G) -> GeneralTopLevelForm ())
  (Expr : Expr (E) -> Expr ()
    ; the rule order is important here, since this rule applies in the other cases too    
    [(app ,s ,x           ,[e1] ...) (guard (primitive? x))  (if (special-primitive? x) ; like 
                                                                 `(app       ,s ,x              ,e1 ...)
                                                                 `(primapp   ,s ,x              ,e1 ...))]
    [(app ,s (top ,s0 ,x) ,[e1] ...) (guard (primitive? x))  (if (special-primitive? x)
                                                                 `(app       ,s ,x              ,e1 ...)
                                                                 `(primapp   ,s ,x              ,e1 ...))]
    [(app ,s (λ ,s1 ,[f] ,[e]) ,[e1] ...)             `(closedapp ,s ,`(λ ,s1 ,f ,e) ,e1 ...)]
    [(app ,s ,[e0] ,[e1] ...)                         `(app       ,s ,e0             ,e1 ...)])
  (TopLevelForm T))

(module+ helpers
  (provide primapp closedapp app)
  ;;;
  ;;; HELPERS
  ;;;
  
  ; At times it can be convenient to run an unparsed program
  ; in DrRacket. In order to do that we need to define the
  ; new language constructs used.
  
  (define-syntax (primapp stx)
    (syntax-parse stx
      [(_primapp pr e1 ...)
       (syntax/loc stx (#%app pr e1 ...))]))
  
  (define-syntax (closedapp stx)
    (syntax-parse stx
      [(_closedapp ab e1 ...)
       (syntax/loc stx (#%app ab e1 ...))]))
  
  (define-syntax (app stx)
    (syntax-parse stx
      [(_closedapp e0 e1 ...)
       (syntax/loc stx (#%app e0 e1 ...))])))


(module+ test
  (let ()
    (define (test stx)
      (reset-counter!)
      (unparse-all
       (unparse-LFE3
        (categorize-applications
         (assignment-conversion
          (α-rename
           (explicit-case-lambda
            (explicit-begin
             (parse
              (expand-syntax stx))))))))))
    (check-equal? (test #'(+ 1 2)) '(primapp + '1 '2))
    (check-equal? (test #'(begin (define foo 3)    (foo 1 2)))
                  ; At the top-level the expression is evaluted before
                  ; the binding is created.
                  ; Also (#%top . foo) looks up foo in the current
                  ; namespace by name, so it should not be renamed.
                  ; But ... until we implement namespaces, we store
                  ; top-level variables in a `boxed`.
                  '(begin (define-values (foo) (primapp boxed '3)) (app (#%top . foo) '1 '2)))
    (check-equal? (test #'((λ (x) 1) 2))
                  '(closedapp (λ (x) '1) '2))
    (check-equal? (test #'((λ (x y) 1) 2 3))
                  '(closedapp (λ (x y) '1) '2 '3))
    (check-equal? (test #'((λ (x . y) 1) 2 3))
                  '(closedapp (λ (x . y) '1) '2 '3))
    (check-equal? (test #'(module t webracket (fx+ 1 2)))
                  '(module t webracket
                     (#%plain-module-begin
                      (primapp fx+ '1 '2))))))

;;;
;;; ANORMALIZATION
;;;

; All subexpression are given a name unless
;  1) it is a RHS in a let-assignment (it already has a name)
;  2) it is a tail expression         (avoid building context)
;  3) it is an atomic expression
;  4) the value will be ignored       (non-last expressions in a begin)

; Introduce administrative normal form
(define-language LANF (extends LFE3)
  ;; Atomic Expressions
  ;;  - always terminates
  ;;  - cause no errors
  ;;  - have no side effects
  ;; Note: Application of pure primitives could be added here
  (AExpr (ae)
    (+ x
       ab
       cab
       (quote s d)                => (quote d)
       (quote-syntax s d)         => (quote-syntax d)
       (top s x)                  => (#%top . x)
       (variable-reference s vrx) => (variable-reference vrx)))
  ;; Complex Expressions
  ;;   - defer execution to at most one subexpression
  ;;   - may not terminate
  ;;   - may not error
  ;;   - may have side effects  
  (CExpr (ce)
    (+ ae 
       (if s ae0 e1 e2)                     => (if ae0 e1 e2)
       (set! s x ae)                        => (set! x ae)
       (wcm s ae0 ae1 e)                    => (with-continuation-mark ae0 ae1 e)
       (app       s ae ae1 ...)             => (app       ae ae1 ...)
       (primapp   s pr ae1 ...)             => (primapp   pr ae1 ...)
       (closedapp s ab ae1 ...)             => (closedapp ab ae1 ...)
       (begin  s e0 e1 ...)                 => (begin  e0 e1 ...)
       (begin0 s e0 e1 ...)                 => (begin0 e0 e1 ...)))
  (Expr (e)
    (-
     ; atomic
     x
     ab
     cab
     (quote s d)
     (quote-syntax s d)
     (top s x)
     (variable-reference s vrx)
     ; complex
     (if s e0 e1 e2)
     (set! s x e) 
     (wcm s e0 e1 e2)
     (primapp   s pr e1 ...)
     (closedapp s ab e1 ...)
     (app       s e0 e1 ...)
     (begin  s e0 e1 ...)
     (begin0 s e0 e1 ...)
     ; expr
     ; 
     (let-values    s ([(x ...) e] ...) e0)
     (letrec-values s ([(x ...) e] ...) e0))
    (+ ; ae ; not needed, an ae is also an ce
     ce
     (let-values    s ([(x ...) ce] ...) e) => (let-values    ([(x ...) ce] ...) e)
     (letrec-values s ([(x ...) ce] ...) e) => (letrec-values ([(x ...) ce] ...) e))))

; anormalize : LFE3 -> LANF
(define (anormalize T)
  ; (Formals             : Formals             (F) -> Formals             ())
  ; (TopLevelForm        : TopLevelForm        (T) -> TopLevelForm        ())
  ; (GeneralTopLevelForm : GeneralTopLevelForm (G) -> GeneralTopLevelForm ())
  (define (id x) x)
  (define h #'an)
  
  (define (TopLevelForm*    ts)  (map TopLevelForm    ts))
  (define (ModuleLevelForm* mfs) (map ModuleLevelForm mfs))
  
  (define (TopLevelForm T)
    (with-output-language (LANF TopLevelForm)
      (nanopass-case (LFE3 TopLevelForm) T
        [(topbegin ,s ,t ...)           `(topbegin ,s ,(TopLevelForm* t) ...)]
        [(#%expression ,s ,e)           `(#%expression ,s ,(Expr e id))]
        [(topmodule ,s ,mn ,mp ,mf ...) `(topmodule ,s ,mn ,mp ,(ModuleLevelForm* mf) ...)]
        [,g                             (GeneralTopLevelForm g)])))

  (define (ModuleLevelForm M)
    (with-output-language (LANF ModuleLevelForm)
      (nanopass-case (LFE3 ModuleLevelForm) M
        [,g (GeneralTopLevelForm g)])))

  (define (GeneralTopLevelForm G)
    (with-output-language (LANF GeneralTopLevelForm)
      (nanopass-case (LFE3 GeneralTopLevelForm) G
        [,e                                (Expr e id)]
        [(define-values   ,s (,x ...) ,e)  `(define-values   ,s (,x ...) ,(RHS e id))]
        [(define-syntaxes ,s (,x ...) ,e)  `(define-syntaxes ,s (,x ...) ,(RHS e id))]
        [(#%require       ,s ,rrs ...)   `(#%require ,s ,(map RawRequireSpec rrs) ...)]
        [else (error 'anormalize-GeneralTopLevelForm
                     "expected general top-level form, got ~a"
                     G)])))

  (define (RawRequireSpec RRS)
    (with-output-language (LANF RawRequireSpec)
      (nanopass-case (LFE3 RawRequireSpec) RRS
        [,rrmp (RawRootModulePath rrmp)])))

  (define (RawRootModulePath RRMP)
    (with-output-language (LANF RawRootModulePath)
      (nanopass-case (LFE3 RawRootModulePath) RRMP
        [(quote ,x)  `(quote ,x)])))
  
  (define (Abstraction ab k)
    (with-output-language (LANF AExpr) 
      (nanopass-case (LFE3 Abstraction) ab
        [(λ ,s ,f ,e) (k `(λ ,s ,(Formals f) ,(Expr e id)))]
        [else (error 'anormalize-Abstraction "expected abstraction")])))

  (define (CaseAbstraction cab k)
    (with-output-language (LANF AExpr) 
      (nanopass-case (LFE3 CaseAbstraction) cab
        [(case-lambda ,s ,ab ...)
         (k `(case-lambda ,s ,(map (λ (ab) (Abstraction ab id)) ab) ...))]
        [else (error 'anormalize-CaseAbstraction "expected case-abstraction")])))
  
  (define (Formals f)
    (with-output-language (LANF Formals) 
      (nanopass-case (LFE3 Formals) f
        [(formals (,x ...))            `(formals (,x ...))]
        [(formals (,x0 ,x1 ... . ,xd)) `(formals (,x0 ,x1 ... . ,xd))]
        [(formals ,x)                  `(formals ,x)])))
  
  (define (LFE3-Formals f)
    (with-output-language (LFE3 Formals) 
      (match f
        [(list x ...)         `(formals (,x ...))]
        [(list* x0 x1 ... xd) `(formals (,x0 ,x1 ... . ,xd))]
        [x                    `(formals ,x)])))
  
  (define (RHS E k)
    (nanopass-case (LFE3 Expr) E
      [,ab  (Abstraction ab k)]
      [else (Expr (with-output-language (LFE3 Expr)
                    `(closedapp ,h (λ ,h ,(LFE3-Formals '()) ,E))) k)]))
  
  (define (RHS* es k)
    (cond
      [(null? es)  (k '())]
      [else        (RHS (first es)
                     (λ (e0) (RHS* (cdr es)
                               (λ (es) (k (cons e0 es))))))]))    
  
  (define (Expr E k)
    ;(displayln (list 'anormalize-Expr E)) (newline)
    (with-output-language (LANF CExpr)
      (nanopass-case (LFE3 Expr) E
        [(if ,s ,e0 ,e1 ,e2)
         (Expr/name e0 (λ (ae0) (k `(if ,s ,ae0 ,(Expr e1 id) ,(Expr e2 id)))))]
        [(set! ,s ,x ,e)
         (Expr/name e (λ (ae) (k `(set! ,s ,x ,ae))))]
        [(let-values ,s ([(,x ...) ,e] ...) ,e0)
         ; ad 1) don't name e ... 
         (RHS* e (λ (ce) (with-output-language (LANF Expr)
                           `(let-values    ,s ([(,x ...) ,ce] ...) ,(Expr e0 k)))))]
        [(letrec-values ,s ([(,x ...) ,e] ...) ,e0)
         (RHS* e (λ (ce) (with-output-language (LANF Expr)
                           `(letrec-values ,s ([(,x ...) ,ce] ...) ,(Expr e0 k)))))]
        [(primapp   ,s ,pr ,e1 ...)
         (Expr*/names e1 (λ (ae1) (k `(primapp   ,s ,pr ,ae1 ...))))]
        [(closedapp ,s ,ab ,e1 ...)
         (Expr*/names e1 (λ (ae1) (k `(closedapp ,s ,(Abstraction ab id) ,ae1 ...))))]
        [(app       ,s ,e0 ,e1 ...)
         (Expr/name e0 (λ (ae0) (Expr*/names e1 (λ (ae1) (k `(app ,s ,ae0 ,ae1 ...))))))]
        ; Atomic expressions
        [,x                           (k x)]
        [,ab                          (Abstraction ab k)]
        [,cab                         (CaseAbstraction cab k)]
        [(quote ,s ,d)                (with-output-language (LANF AExpr) (k `(quote ,s ,d)))]
        [(quote-syntax ,s ,d)         (with-output-language (LANF AExpr) (k `(quote-syntax ,s ,d)))]
        [(top ,s ,x)                  (with-output-language (LANF AExpr) (k `(top ,s ,x)))]
        [(variable-reference ,s ,vrx) (with-output-language (LANF AExpr) (k `(variable-reference ,s ,vrx)))]
        [(begin ,s ,e0 ,e1 ...)
         (define (Expr/id e) (Expr e identity))
         (let ([e0 (Expr/id e0)] [e1 (map Expr/id e1)])
           (k (with-output-language (LANF Expr)
                `(begin ,s ,e0 ,e1 ...))))]
        [(begin0 ,s ,e0 ,e1 ...)
         (define (Expr/id e) (Expr e identity))
         (let ([e0 (Expr/id e0)] [e1 (map Expr/id e1)])
           (k (with-output-language (LANF Expr)
                `(begin0 ,s ,e0 ,e1 ...))))]
        [(wcm ,s ,e0 ,e1 ,e2)
         (Expr/name e0
           (λ (ae0) (Expr/name e1
                      (λ (ae1)
                        (k `(wcm ,s ,ae0 ,ae1 ,(Expr e2 id)))))))]
        [else
         (displayln (list 'anormalize-Expr "got" E))
         (error 'anormalize-Expr "internal error")])))

  ; Expr/name : Expr (AExpr -> Expr) -> Expr
  (define (Expr/name e k)
    ;(displayln (list 'anormalize-Expr/name e)) (newline)
    ; Transform e, then name it (unless it is an atomic expression),
    ; then call k with the name or the atomic expression
    (Expr e (λ (e)
              (nanopass-case (LANF Expr) e
                [,ae   (k ae)]               ; ad 3) don't name atomic expressions
                [else  (let ([t (new-var)])
                         (with-output-language (LANF Expr)
                           `(let-values ,h ([(,t) ,e])
                              ,(k t))))]))))

  (define (Expr*/names es k)
    (cond
      [(null? es)  (k '())]
      [else        (Expr/name (car es)
                     (λ (t) (Expr*/names (cdr es) 
                              (λ (ts) (k (cons t ts))))))]))
  (TopLevelForm T))


(module+ test
  (let ()
    (define (test stx)
      (reset-counter!)
      (unparse-all
       (unparse-LANF
        (anormalize
         (categorize-applications
          (assignment-conversion
           (α-rename
            (explicit-case-lambda
             (explicit-begin
              (parse
               (expand-syntax stx)))))))))))
    (check-equal? (test #'1) ''1)
    (check-equal? (test #'(+ 2 3)) '(primapp + '2 '3))
    (check-equal? (test #'(+ 2 (* 4 5)))
                  '(let-values (((t.1) (primapp * '4 '5))) (primapp + '2 t.1)))
    (check-equal? (test #'(begin (+ 2 (* 4 5)) (+ (* 6 7) (/ 8 9))))
                  '(begin
                     (let-values (((t.1) (primapp * '4 '5))) (primapp + '2 t.1))
                     (let-values (((t.2) (primapp * '6 '7)))
                       (let-values (((t.3) (primapp / '8 '9))) (primapp + t.2 t.3)))))
    (check-equal? (test #'(let-values ([(x) (+ 1 (* 2 3))])
                            (let-values ([(y) (+ (* 4 x) 1)])
                              ((λ (z) (+ x y)) 5))))
                  '(let-values (((x) (closedapp (λ () (let-values (((t.1) (primapp * '2 '3)))
                                                        (primapp + '1 t.1))))))
                     (let-values (((y) (closedapp (λ () (let-values (((t.2) (primapp * '4 x)))
                                                          (primapp + t.2 '1))))))
                       (closedapp (λ (z) (primapp + x y)) '5))))
    (check-equal? (test #'(letrec ([fact (λ (n) (if (= n 0) 1 (* n (fact (- n 1)))))]) (fact 5)))
                  '(let-values ()
                     (letrec-values (((fact)
                                      (λ (n)
                                        (let-values (((t.1) (primapp = n '0)))
                                          (if t.1 '1 (let-values (((t.2) (primapp - n '1)))
                                                       (let-values (((t.3) (app fact t.2)))
                                                         (primapp * n t.3))))))))
                       (app fact '5))))
    (check-equal? (test #'(module t webracket (fx+ 1 (fx+ 2 3))))
                  '(module t webracket
                     (#%plain-module-begin
                      (let-values (((t.1) (primapp fx+ '2 '3)))
                        (primapp fx+ '1 t.1)))))))

;;;
;;; FREE VARIABLE ANALYSIS
;;;

;; The goal of this pass is to determine the set of free variables
;; in each abstraction. The next pass, closure conversion, will use
;; this information to turn lambda abstractions into closures.

;; Rather than defining a new language with a variation of the
;; nonterminal λ with a field to store the free variables,
;; we use a hashtable to associate abstractions with sets of free variables.

;; Each nonterminal transformation produces two values: the form and the set of free variables
;; When a catamorphism is used as in (λ ,s ,f ,[e xs]) then
;; the Expr transformations is called and its output values are bound to e and xs.

;; There ought to be no free variables in top-level-forms, but we compute them
;; anyway to catch any internal errors.

(define-pass determine-free-variables : LANF (T) -> LANF (free-ht abstractions)
  (definitions
    ; list of all abstractions
    (define abs '())
    (define (seen-abstraction! ab) (set! abs (cons ab abs)))
    ; hash table from abstraction to its free variables
    (define ht (make-hasheq))
    (define (add! ab xs) (hash-set! ht ab xs))
    (define (get ab)     (hash-ref  ht ab))
    (define (formal-variables F)
      (nanopass-case (LANF Formals) F
        [(formals (,x ...))            (ids->id-set x)]
        [(formals (,x0 ,x1 ... . ,xd)) (set-union (make-id-set x0 xd) (ids->id-set x1))]
        [(formals ,x)                  (make-id-set x)]))
    (define (bound-at-top-level ts)
      ; return set of all xs occuring in a 
      ;     (define-values   ,s (,x ...) ,[e xs])
      (for/fold ([bound empty-set]) ([t ts])
        (define newly-bound
          (nanopass-case (LANF TopLevelForm) t
            [,g (nanopass-case (LANF GeneralTopLevelForm) g
                  [(define-values ,s (,x ...) ,e) (ids->id-set x)]
                  [else empty-set])]
            [else empty-set]))
        (set-union bound newly-bound)))
    (define (primitives->id-set)
      (ids->id-set (map (λ (sym) (variable (datum->syntax #'dfv sym)))
                        primitives))))
  
  (Formals : Formals (F) -> Formals ())
  (TopLevelForm : TopLevelForm (T xs) -> TopLevelForm (xs)
    [(topbegin ,s ,[t xs] ...)      (values T (set-difference
                                               (set-union* xs)
                                               (set-union (bound-at-top-level t)
                                                          ; todo: only import primitives that are declared
                                                          (primitives->id-set))))]
    [(topmodule ,s ,mn ,mp ,mf ...) (for ([m mf]) ; todo: find all identifiers defined at the module level
                                      (ModuleLevelForm m '()))   ; mark abstractions in the module
                                    (values T '())]               ; no free variables in module
    [(#%expression ,s ,[e xs])      (values T xs)]
    [,g                             (GeneralTopLevelForm g xs)])
  (ModuleLevelForm : ModuleLevelForm (M xs) -> ModuleLevelForm (xs)
    [,g                        (GeneralTopLevelForm g xs)])
  (GeneralTopLevelForm : GeneralTopLevelForm (G xs) -> GeneralTopLevelForm (xs)
    [(define-values   ,s (,x ...) ,[e xs]) (values G (set-difference xs (ids->id-set x)))]
    [(define-syntaxes ,s (,x ...) ,[e xs]) (values G (set-difference xs (ids->id-set x)))] ; todo
    [,e (Expr e xs)])
  (Abstraction : Abstraction (AB xs) -> Abstraction (xs)
    [(λ ,s ,f ,[e xs])        (let ([xs (set-difference xs (formal-variables f))])
                                (add! AB xs)
                                (seen-abstraction! AB)
                                (values AB xs))])
  (CaseAbstraction : CaseAbstraction (CAB xs) -> CaseAbstraction (xs)
    ; the free variables of a case-lambda is the union of the free variables of the clauses
    [(case-lambda ,s ,ab* ...) (define xs* (set-union*
                                            (for/list ([ab ab*])
                                              (letv ((ab xs) (Abstraction ab xs))
                                                xs))))
                               (add! CAB xs*)
                               (seen-abstraction! CAB)
                               (values CAB xs*)])
  (AExpr : AExpr (AE xs) -> AExpr (xs)
    [,x                   (if (primitive? (variable-id x))
                              (values AE empty-set)         ; primitive
                              (values AE (make-id-set x)))] ; non-primitive
    [,ab                  (Abstraction AE xs)]
    [,cab                 (CaseAbstraction AE xs)]
    [(quote ,s ,d)        (values AE empty-set)]
    [(quote-syntax ,s ,d) (values AE empty-set)]
    ; Note: Top-level variables are stored in a namespace.
    ;       In this pass we are only looking for variables that need to be stored in closures.
    [(top ,s ,x)                  (values AE empty-set)]
    [(variable-reference ,s ,vrx) (values AE empty-set)]) ; todo 
  (CExpr : CExpr (CE xs) -> CExpr (xs)
    [,ae                                      (AExpr ae xs)]
    [(if  ,s ,[ae0 xs0] ,[e1  xs1] ,[e2 xs2]) (values CE (set-union* (list xs0 xs1 xs2)))]
    [(wcm ,s ,[ae0 xs0] ,[ae1 xs1] ,[e  xs2]) (values CE (set-union* (list xs0 xs1 xs2)))]
    [(app ,s ,[ae  xs]  ,[ae1 xs1] ...)       (if (special-primitive? ae)
                                                  (values CE (set-union* xs1))
                                                  (values CE (set-union* (cons xs xs1))))]
    [(primapp   ,s ,pr      ,[ae1 xs1] ...)   (values CE (set-union* xs1))]
    [(closedapp ,s ,[ab xs] ,[ae1 xs1] ...)   (values CE (set-union* (cons xs xs1)))]
    [(begin     ,s ,[e0 xs0] ,[e1 xs] ...)    (values CE (set-union* (cons xs0 xs)))]
    [(begin0    ,s ,[e0 xs0] ,[e1 xs] ...)    (values CE (set-union* (cons xs0 xs)))])
  (Expr : Expr (E xs) -> Expr (xs)
    [,ce (CExpr E xs)]
    [(let-values    ,s (((,x ...) ,[ce rhs-xs]) ...) ,[e xs])
     (values E (set-union (set-union* rhs-xs) ; the ((x ...) ...) are not bound in ce ...
                          (set-difference xs  ; the free variables of e except those bound here
                                          (set-union* (map ids->id-set x)))))]
    [(letrec-values ,s (((,x ...) ,[ce rhs-xs]) ...) ,[e xs])
     (values E (set-difference (set-union* (cons xs rhs-xs)) (set-union* (map ids->id-set x))))])

  (letv ((T xs) (TopLevelForm T (make-id-set)))    
    (unless (set-empty? xs)
      (displayln "\n---\n")
      (displayln (unparse-LANF T))
      (displayln "\n---\n") (displayln xs) (newline)
      (error 'determine-free-variables "detected free variables (shouldn't be possible)"))
    (values T ht abs)))


;;;
;;; FINISH CLOSURE CONVERSION
;;;

;;; Step 1: Generate a label for each abstraction
;;;   label1:  (λ (x ...) e)
;;; Step 2:
;;;   Add a top-level-form for each abstraction:
;;;       (define-label label1  (λ (cl x ...) e))
;;;       where e has been closure converted
;;;   Rewrite each abstraction:
;;;       (λ (x ...) e)    =>  (make-closure label1 f ...)
;;;                            where f ... are the free variables of the abstraction
;;;   Rewrite references to free variables:
;;;       f                =>  (free-ref cl i)
;;;                            where i is the index of f in the list of free variables
;;; Runtime:
;;;   The top-level-form becomes
;;;      var label1 = function (cl, x, ...) { [[e]] }
;;;   An application of a closure cl to arguments a ... becomes:
;;;      cl.label(a,...)
;;;   Make closure becomes:
;;;      [the_unique_closure_tag, label1, v0, ...]
;;;      where v0, ... are the values stored in the closure
;;;   Local references (free-ref cl i) becomes:
;;;      cl[i+2]

(define (closure-conversion T)
  (define labels-ht (make-hasheq))
  (define (add-label! ab) (hash-set! labels-ht ab (new-var "label")))
  (letv ((T free-ht abs) (determine-free-variables T))
    (for ([ab abs])
      (add-label! ab))
    (finish-closure-conversion T free-ht labels-ht)))

(define (closure+primitive? v)
  (or (eq? v 'closure)
      (primitive? v)))

(define (natural? v) (and (integer? v) (not (negative? v))))

(define arity? integer?)

(define-language LANF+closure (extends LANF)
  (terminals
   (- (primitive (pr)))
   (+ (closure+primitive (pr)))
   (+ (arity (ar)))
   (- (variable (x xd)))
   (+ (variable (x xd l)) => unparse-variable)  ; l for label
   (+ (natural (i))))                           ; i for index 
  (Abstraction (ab)
    (- (λ s f e)))
  (CaseAbstraction (cab)
    (- (case-lambda s ab ...)))
  (ClosureAllocation (ca)
    (+ (closure s l ar ae1 ...)   => (closure l ae1 ...)))
  (CaseClosureAllocation (cca)
    (+ (case-closure s l [ar ca] ...) => (case-closure [ar ca] ...)))
  (AExpr (ae)
    (- ab
       cab)
    (+ ca
       cca
       (free-ref x i)))
  (CExpr (ce)
    (- (closedapp s ab ae1 ...))
    (+ (closedapp s ca ae1 ...) => (closedapp ca ae1 ...)))
  (ConvertedAbstraction (cab)
    (+ (λ s ar f e) => (λ f e)))
  #;(ConvertedCaseAbstraction (ccab)
      (+ (case-lambda s cab ...) => (case-lambda cab ...)))
  (TopLevelForm (t)
                (+ (define-label l cab)))
  (ModuleLevelForm (mf)
    (+ (define-label l cab))))

(define-pass finish-closure-conversion : LANF (T free-ht labels-ht) -> LANF+closure ()
  (definitions
    (define h #'fcc)
    (define (label-of ab) (hash-ref labels-ht ab))
    (define (free-of ab)  (hash-ref   free-ht ab))
    (define (index-of x free)
      (define (x? y) (id=? x y))
      (index-where free x?))
    ; the converted abstractions are lifted to the top-level,
    ; add the same time we store the arity for each abstraction.
    (define lifted-abstractions (make-hasheq))
    (define (formals->arity f)
      ; +n means precisely n
      ;  0 means precisely  0
      ; -1 means at least 0
      ; -2 means at least 1
      ; -n means at least n-1
      (nanopass-case (LANF+closure Formals) f
                     [(formals (,x ...))            (length x)]
                     [(formals (,x0 ,x1 ... . ,xd)) (- (+ 2 (length x1)))]
                     [(formals ,x)                  -1]))
    (define (lift! s label formals body)       
      (define cab
        (with-output-language (LANF+closure ConvertedAbstraction)
          `(λ ,s ,(formals->arity formals) ,formals ,body)))
      (hash-set! lifted-abstractions label cab))    
    (define (case-lift! label formals* body)
      (define ccab
        (with-output-language (LANF+closure ConvertedAbstraction)
          (let ([f (with-output-language  (LANF+closure Formals) `(formals ()))])
            `(λ ,h ,(formals->arity f) ,f ,body)))) ; TODO BUG TODO BUG
      (hash-set! lifted-abstractions label ccab))
    (define (abstraction-formals ab)
      (nanopass-case (LANF Abstraction) ab
        [(λ ,s ,f ,e) f]))
    (define current-free (make-parameter empty-set)))
  (TopLevelForm    : TopLevelForm    (T) -> TopLevelForm      ())
  (ModuleLevelForm : ModuleLevelForm (M) -> ModuleLevelForm   ())
  (Expr            : Expr            (E) -> Expr              ())
  (Abstraction     : Abstraction    (AB) -> ClosureAllocation ()
    [(λ ,s ,[f] ,e)
     (define l  (label-of AB))
     (let ([xs (map AExpr (free-of AB))]
           [e  (parameterize ([current-free (free-of AB)])
                 (Expr e))])
       (lift! s l f e)
       `(closure ,h ,l ,(formals->arity f) ,xs ...))])
  (CaseAbstraction : CaseAbstraction (CAB) -> CaseClosureAllocation ()
    [(case-lambda ,s ,ab ...)
     (define (formals->arity f)
       ; +n means precisely n
       ;  0 means precisely  0
       ; -1 means at least 0
       ; -2 means at least 1
       ; -n means at least n-1
       (nanopass-case (LANF Formals) f
         [(formals (,x ...))            (length x)]
         [(formals (,x0 ,x1 ... . ,xd)) (- (+ 2 (length x1)))]
         [(formals ,x)                  -1]))     
     (define (abstraction->formals ab)
       (nanopass-case (LANF Abstraction) ab [(λ ,s ,f ,e) f]))
     (define l  (label-of CAB))
     (let* ([fs*  (map abstraction-formals ab)]
            [ar   (map formals->arity fs*)]
            [ab   (map Abstraction ab)]           
            [E    (with-output-language (LANF+closure AExpr)
                    `(quote ,#'h ,(datum #'42 42)))])
       (case-lift! l fs* E)
       `(case-closure ,h ,l [,ar ,ab] ...))])
  (AExpr : AExpr (AE) -> AExpr ()
    [,x ; note: x is kept for debugging purposes
     (define xs (current-free))  ; set by the enclosing abstraction
     (match (index-of x xs)
       [#f x] ; refers to argument
       [i `(free-ref ,x ,(index-of x xs))])])
  (begin
    (let ([T (TopLevelForm T)])
    (with-output-language (LANF+closure TopLevelForm)
      ; the lifted (converted) abstractions are added to the top-level
      (define dl (for/list ([(l cab) lifted-abstractions])
                   `(define-label ,l ,cab)))
      `(topbegin ,h ,dl ... ,T)))))

(require racket/pretty)

#;(define (test stx)
  (reset-counter!)
  (pretty-print
   (unparse-all
    (unparse-LANF+closure
     (closure-conversion
      (anormalize
       (categorize-applications
        (assignment-conversion
         (α-rename
          (explicit-case-lambda
           (explicit-begin
            (convert-quotations
             (parse
              (topexpand stx))))))))))))))

(define-pass flatten-begin : LANF+closure (T) ->  LANF+closure ()
  (definitions)

  (ModuleLevelForm  : ModuleLevelForm     (M) -> ModuleLevelForm ())
  (GeneralLevelForm : GeneralTopLevelForm (G) -> GeneralTopLevelForm ())
  (AExpr : AExpr (AE) -> AExpr ())
  (Expr  :  Expr (E)  ->  Expr ())

  (TopLevelForm     : TopLevelForm        (T) -> TopLevelForm ()
    [(topbegin ,s ,t0 ...)
     (let ()
       (define (topbegin? t)
         (nanopass-case (LANF+closure TopLevelForm) t
           [(topbegin ,s ,t0 ...) #t]
           [else                  #f]))

       (let loop ([ts t0])
         ; we loop until there are no `topbegin`s left
         (cond
           [(memf topbegin? ts)
            ; for each t we return a list of `TopLevelForm`.
            (define tss
              (for/list ([t ts])
                (nanopass-case (LANF+closure TopLevelForm) t
                  [(topbegin ,s ,t0  ...) t0]
                  [else                   (list t)])))
            (loop (append* tss))]
           [else
            `(topbegin ,s ,(map TopLevelForm ts) ...)])))])


  (CExpr : CExpr (CE) -> CExpr ()
    [(begin ,s ,e0 ,e1 ...)
     (let ()
       (define (begin? ce)
         (nanopass-case (LANF+closure CExpr) ce
           [(begin ,s ,e0 ,e1 ...) #t]
           [else                   #f]))
     
       (let loop ([es (cons e0 e1)])
         ; we loop until there are no `begin`s left
         (cond
           [(memf begin? es)
            ; for each e we return a list of `CExpr`.
            (define ess
              (for/list ([e es])
                (nanopass-case (LANF+closure CExpr) e
                  [(begin ,s ,e0  ,e1 ...) (cons e0 e1)]
                  [else                    (list e)])))
            (loop (append* ess))]
           [else
            (if (null? (cdr es))
                (car es)
                `(begin ,s
                        ,(Expr (car es))
                        ,(map Expr (cdr es)) ...))])))]))
            
;;;
;;; SIMPLIFY FORMALS 
;;;

; expr     ::=  ... | (#%plain-lambda formals body ...+) | ...

; formals  ::=   (id ...)            ; fixed number of arguments
;              | (id ...+ . id)      ; fixed, followed by rest argument
;              | id                  ; arguments passed as list

; Note: Since the input to this compiler is "fully expanded",
;       we don't need to handle optional arguments (the expander
;       rewrites them to `case-lambda` or keyword arguments
;       (also rewritten by the expander).

; In WebAssembly there is only one type of function namely the first.
; Let's rewrite (id ...+ . id) to (id ...+ id).
; Example    f == (λ (x y . z) ...)
;    becomes      (λ (x y z)   ...).
;    A call like  (f 1 2 3 4)
;    becomes      (f 1 2 (cons 3 (cons 4 '()))).
; For known procedures we can do this rewrite.
; For a call to an unknow procedure, the invoker needs to do the rewrite.

; For the last case, where arguments are passed as list, we will do this
; rewrite:   f == (λ x ...)
;    becomes      (λ (x) ...).
;    A call like  (f 1 2 3)
;    becomes      (f (cons 1 (cons 2 (cons 3 '())))).

;; (Formals (f)
;;   (formals (x ...))            => (x ...)
;;   (formals (x0 x1 ... . xd))   => (x0 x1 ... . xd)
;;   (formals x)                  => x)




;;;
;;; CATEGORIZE VARIABLES 
;;;

; The goal of this pass is to categorize assignments according to the
; variable type: top-level, module-level, local.

; This pass is local to the pass `generate-code`.

(define-pass classify-variables : LANF+closure (T) -> * ()
  ;; Assumption: α-conversion has been done
  (definitions
    (define (TopLevelForm*         Ts)  (for-each TopLevelForm         Ts))
    (define (ModuleLevelForm*      Ms)  (for-each ModuleLevelForm      Ms))
    (define (Expr*                 Es)  (for-each Expr                 Es))
    (define (CExpr*                CEs) (for-each CExpr                CEs))
    (define (AExpr*                AEs) (for-each AExpr                AEs))
    (define (ConvertedAbstraction* ABs) (for-each ConvertedAbstraction ABs))
    (define (ClosureAllocation*    CAs) (for-each ClosureAllocation    CAs))

    (define top '()) ; top     (outside modules)
    (define mod '()) ; modules
    (define loc '()) ; locals  (argument or let/letrec bound
    (define lab '()) ; label
    (define (top! x)   (set! top (cons x top)))
    (define (mod! x)   (set! mod (cons x mod)))
    (define (loc! x)   (set! loc (cons x loc)))
    (define (lab! x)   (set! lab (cons x lab)))
    (define (top!* xs) (for-each top! xs))
    (define (mod!* xs) (for-each mod! xs))
    (define (loc!* xs) (for-each loc! xs))
    (define (lab!* xs) (for-each lab! xs))

    (define add! (make-parameter top!*)))
    
  (TopLevelForm : TopLevelForm (T) -> * ()
    [(define-label ,l ,cab)         (lab! l) (ConvertedAbstraction cab)]
    [(topbegin ,s ,t ...)           (TopLevelForm* t)]
    [(#%expression ,s ,e)           (Expr e)]
    [(topmodule ,s ,mn ,mp ,mf ...) (parameterize ([add! mod!*])
                                      (ModuleLevelForm* mf))]
    [,g                             (GeneralTopLevelForm g)])
  (ModuleLevelForm : ModuleLevelForm (M) -> * ()
    [(define-label ,l ,cab)         (lab! l) (ConvertedAbstraction cab)]
    [,g                             (GeneralTopLevelForm g)])
  (GeneralTopLevelForm : GeneralTopLevelForm (G) -> * ()
    [,e                                 (Expr e)]
    [(define-values   ,s (,x ...) ,e)   ((add!) x) (Expr e)]
    [(define-syntaxes ,s (,x ...) ,e)   ((add!) x) (Expr e)]
    [(#%require       ,s ,rrs ...)      empty-set])
  (ConvertedAbstraction : ConvertedAbstraction (CAB) -> * ()
    [(λ ,s ,ar ,f ,e)                   (parameterize ([add! loc!*])
                                          (Formals f)
                                          (Expr e))])
  (Formals : Formals (F) -> * ()
    [(formals (,x ...))            ((add!) x)]
    [(formals (,x0 ,x1 ... . ,xd)) ((add!) (cons x0 (cons xd x1)))]
    [(formals ,x)                  ((add!) (list x))])

  (AExpr : AExpr (AE) -> * ()
    [,x                      #f]
    [,cca                    (CaseClosureAllocation cca)]
    [,ca                     (ClosureAllocation ca)]
    [(quote ,s ,d)           #f]
    [(quote-syntax ,s ,d)    #f]
    ; Note: Top-level variables are stored in a namespace.
    ;       In this pass we are only looking for variables that
    ;       need to be stored in closures.
    [(top ,s ,x)                  #f]
    [(variable-reference ,s ,vrx) #f]
    [(free-ref ,x ,i)             #f])

  (CExpr : CExpr (CE) -> * ()
    [,ae                           (AExpr ae)]
    [(set! ,s ,x ,ae)              (AExpr ae)]
    [(if  ,s ,ae0 ,e1  ,e2)        (AExpr ae0) (Expr e1)   (Expr e2)]
    [(wcm ,s ,ae0 ,ae1 ,e)         (AExpr ae0) (AExpr ae1) (Expr e)]
    [(app ,s ,ae  ,ae1 ...)        (AExpr* (cons ae ae1))]
    [(primapp   ,s ,pr ,ae1 ...)   (AExpr* ae1)]
    [(closedapp ,s ,ca ,ae1 ...)   (ClosureAllocation ca) (AExpr* ae1)]
    [(begin     ,s ,e0 ,e1 ...)    (Expr* (cons e0 e1))]
    [(begin0    ,s ,e0 ,e1 ...)    (Expr* (cons e0 e1))])

  (Expr : Expr (E) -> * ()
     [(let-values    ,s ([(,x ...) ,ce] ...) ,e) (for-each loc!* x) (CExpr* ce) (Expr e)]
     [(letrec-values ,s ([(,x ...) ,ce] ...) ,e) (for-each loc!* x) (CExpr* ce) (Expr e)]
     [,ce (CExpr ce)])

  (ClosureAllocation : ClosureAllocation (CA) -> * ()
    [(closure ,s ,l ,ar ,ae ...) (AExpr* ae)])

  (CaseClosureAllocation : CaseClosureAllocation (CCA) -> * ()
    [(case-closure ,s ,l [,ar ,ca] ...) (ClosureAllocation* ca)])

  ; Fill in `top`, `mod` and `loc`
  (parameterize ([add! top!*])
    (TopLevelForm T))
  ; Convert to set
  (define sets (map (λ (xs) (apply make-id-set xs))
                    (list top mod loc)))
  (values (first  sets)
          (second sets)
          (third  sets)))

;;;
;;; Representation of immediates
;;;

;; Racket being a dynamic language, we will use a uniform representation for all
;; values. On platforms like x86 and arm64 most often tagged pointers are used 
;; for a uniform representation. On Web Assembly we do not have pointers.
;; Instead we have references. Heap allocated objects (arrays, structs) are
;; referred to by reference. In order to use references to represent other
;; immediates such as fixnums, booleans, null, etc. we will use `i31` which
;; denotes unboxed scalars.
;;
;; The specifications says:
;;   > The abstract type `i31` denotes unboxed scalars, that is, integers
;;   > injected into references. Their observable value range is limited to 31 bits.
;;
;;   > An `i31` is not actually allocated in the store, but represented in a way
;;   > that allows them to be mixed with actual references into the store
;;   > without ambiguity. Engines need to perform some form of pointer
;;   > tagging to achieve this, which is why 1 bit is reserved.
;;
;; That is, an `i31` is in effect a "tagged pointer" with a 1-bit tag.
;; Like `struct` and `array` we have that `i31` also is a heaptype.
;; We can therefore use `(ref eq)` for our uniform representation.
;;
;; We will use the lower bits of an `i31` as tags, so we can have other
;; immediates besides fixnums. We have 31 bits to work with.

;; Data Representation
;;   Fixnums:   Lower 1 bits are 0. Upper bits are an 30-bit integer.
;;   Immediate: Lower 1 bits are 1. Upper 30 bits contain data and a subtag.

;; Immediate Values (non-fixnum)
;;   Characters: Lower 8 bits are  cc...cc 0000 1111. The 21 bits marked c are a unicode scalar value.
;;   Booleans:   Lower 7 bits are          b001 1111. Bit b is 1 for true and 0 for false.
;;   Void:       Lower 8 bits are          0010 1111. Upper bits are zero.
;;   Empty:      Lower 8 bits are          0011 1111. Upper bits are zero.
;;   Undefined:  Lower 8 bits are          0100 1111. Upper bits are zero.
;;   Eof:        Lower 8 bits are          0101 1111. Upper bits are zero.
;;   Missing [*] All bits are 1.   1 ...   1111 1111. 

;; Note: We do not have tags for pairs, vectors, etc. since they need to heap allocated.
;; The missing value is not a Racket value. It is an immediate used to indicate
;; "not found" in hash tables. It needs to distinct from all valid Racket values.
;; [The missing immediates was introduced to avoid the null type in WebAssembly.]

(define fixnum-tag           #b0)
(define immediate-tag     #b1111)
(define char-tag       #b00001111)
(define boolean-tag    #b0011111)

(define void-value      #b00101111)   ; 0010 1111
(define empty-value     #b00111111)   ; 0011 1111
(define undefined-value #b01001111)   ; 0100 1111
(define eof-value       #b01011111)   ; 0101 1111
; The `missing` and `tombstone` value are used internally.
; In particular in the implementation of hash tables.
(define missing-value   #b1111111111111111111111111111111)
(define tombstone-value #b0111111111111111111111111111111)

(define immediate-mask      #b1111)
(define fixnum-mask            #b1)
(define boolean-mask     #b1111111)
(define char-mask        #b11111111) ; mask for tag only

(define fixnum-shift    1)
(define boolean-shift   7)
(define char-shift      8)

(define most-positive-fixnum (- (expt 2 29) 1)) ; 30-bit fixnum
(define most-negative-fixnum (- (expt 2 29)))   ; 30-bit fixnum

(define (fixnum? x)
  (and (number? x)
       (integer? x)
       (<= most-negative-fixnum x most-positive-fixnum)))

(define (immediate? x)
  (or (fixnum? x) (char? x) (boolean? x) (null? x) (void? x)))

(define the-undefined-value (gensym 'undefined))
(define (undefined)    the-undefined-value)
(define (undefined? x) (eq? x the-undefined-value))

(define (immediate-rep x)
  (when (flonum? x)
    (displayln (list 'immediate-rep x)))
  (define (shift x m) (arithmetic-shift x m))
  (cond
    [(fixnum?    x)              (shift x                 fixnum-shift)]  ; the tag is 0
    [(boolean?   x) (bitwise-ior (shift (if x 1 0)        boolean-shift)   boolean-tag)]
    [(char?      x) (bitwise-ior (shift (char->integer x) char-shift)      char-tag)]
    [(null?      x) empty-value]
    [(void?      x) void-value]
    [(undefined? x) undefined-value]
    [else          (error 'immediate-rep "expected immediate value, got: ~a" x)]))


;;; This code generator is inspired by "Destination-driven Code Generation"
;;; by Dybvig, Hieb and Butler. There are som differences however. The code
;;; generator in the paper generates "flat" code (assembler) where as we
;;; generate nested Web Assembly instructions.

;;; The code generator for an expression takes a data destination (dd) and a control destionation (cd)
;;; as arguments. The data destination determines where the value of the expression is to be placed.
;;;
;;;    dd in Data Destination  is  #f or a location (identifier),
;;;
;;; here #f means the expression is evaluated for its effect.

;;; If the data destination is #f (an effect) the value of the expression does not need to be stored.
;;; The control destination
;;;
;;;    cd in ControlDestination  is  label  or  label x label
;;;
;;; If the label is #f (return) it means that the value is to be returned from the function.
;;; If two labels are given, then there are two branches according to the expression value.

; Data destinations (also an identifier)
(define <effect> '<effect>)
(define <value>  '<value>)
; Control destinations
(define <return> '<return>)  
(define <expr>   '<expr>)    
(define <stat>   '<stat>)    ; a statement (i.e. no value result)
; identifier used to hold tail call flag
(define _tc      #'_tc) ; not used


(define-pass generate-code : LANF+closure (T) -> * ()
  (definitions
    ;; 1. Classify variables
    (define-values (top-vars module-vars local-vars)
      (classify-variables T))
    (define (top-variable? v)    (set-in? v top-vars))    ; boxed
    (define (module-variable? v) (set-in? v module-vars))
    (define (local-variable? v)  (set-in? v local-vars))
    (define (classify-variable v)
      (cond
        [(top-variable?    v) 'top]
        [(module-variable? v) 'module]
        [(local-variable?  v) 'local]
        [else (error 'classify-variable "got: ~a" v)]))
    ;; 2. References to variable according to their type
    (define (Reference v)
      ; reference to non-free variable
      ;   global refers to a Web Assembly global variable
      (case (classify-variable v)
        [(top)        `(global.get ,(TopVar v))]   ; unboxed
        [(local)      `(local.get  ,(LocalVar v))]
        [(module)     `(module.get ,(ModuleVar v))]
        [(global)     `(global.get ,(syntax-e v))]
        [else (error 'Reference "got: ~a" v)]))
    ;; 3. Variables assignments according to type.
    (define (Store! v e)
      (cond
        [(variable? v)
         (case (classify-variable v)
           [(top)    `(global.set ,(TopVar    v) ,e)]
           [(local)  `(local.set  ,(LocalVar  v) ,e)]
           [(module) `(module.set ,(ModuleVar v) ,e)]
           [else (error 'Store! "got: ~a" v)])]
        [(identifier? v)
         ; (displayln (list 'Store! v))
         ; variables like: $closedapp-clos and $result 
         ; global
         `(global.set ,(syntax-e v) ,e)]
        [else (displayln v)
              (error 'Store! "got: ~a" v)]))

    ; Convention:
    ;   1. WebAssembly requires all identifiers to begin with $.
    ;   2. We prefix all program identifiers with $$.
    ;   3. The compiler is free to control all identifiers that
    ;      begin with a single $.
    (define ($ x)                    (if (syntax? x)
                                         ($ (syntax-e x))
                                         (string->symbol (~a "$" x))))
    (define ($$ x)                   ($ ($ x))) 
    (define (prim: sym)              (if (syntax? sym)
                                         (prim: (syntax-e sym))
                                         (string->symbol (~a "prim:" sym))))
    
    (define (Label v)                ($  (variable-id v)))

    (define (Var v)                  ($$ (variable-id v)))
    (define (TopVar v)               ($$ (variable-id v)))
    (define (ModuleVar v)            ($$ (variable-id v)))
    (define (LocalVar v)             ($$ (variable-id v)))

    (define (Ref v)                  ($$ (variable-id v)))
    (define (Prim pr)                ($  (variable-id pr)))

    (define (PrimRef pr)             `(global.get ,($ (prim: (variable-id pr)))))

    (define (I31 n)                  `(ref.i31 (i32.const ,n)))
    (define (Imm x)                  (I31 (immediate-rep x)))
    (define (imm x)                  (immediate-rep x))
    (define (Undefined [_ #f])       (I31 undefined-value)) ; it is mapable
  
    (define (Expr e dd cd)           (Expr2 e dd cd))
    (define (AExpr2 ae [dd #f])      (AExpr3 ae dd))                    ; destination dd
    (define (AExpr ae)               (AExpr2 ae #f))                    ; for effect
    (define (AExpr* aes)             (map (λ (ae) (AExpr2 ae #f)) aes)) ; for effect 
    (define (Expr* es dd cd)         (map (λ (e)  (Expr e dd cd)) es))
    (define (CExpr ce dd cd)         (CExpr2 ce dd cd))      
    (define (TopLevelForm* ts dd)    (map (λ (t)  (TopLevelForm    t dd)) ts))
    (define (ModuleLevelForm* ms dd) (map (λ (m)  (ModuleLevelForm m dd)) ms))
    (define f-tmp                    (Var (new-var 'f))) ; used by app

    ;; Some expressions (e.g. let-values, letrec-values) need local
    ;; variables to hold values.
    ;; In Web Assembly the the only place to declare local variables
    ;; are in functions `(func ...)`.
    ;; When compiling the body of a function, we set the parameter
    ;; *locals* to the empty list. Any expressions that require
    ;; a local variable, will use `emit-local`. After the body is
    ;; compiled, the locals are added to the `func`.
    ;; The local variables need at the top-level are in `entry`. 
    (define *locals* (make-parameter '()))
    (define (emit-local x [type '(ref eq)] [init #f])
      (*locals* (cons (if init
                          (list x type init)
                          (list x type))
                      (*locals*))))
    (define (emit-fresh-local t [type '(ref eq)] [init #f])
      (define v (new-var t))
      (emit-local v type init)
      (set! local-vars (set-add local-vars v))
      v)
    )

  (ClosureAllocation : ClosureAllocation (ca dd) -> * ()
    [(closure ,s ,l ,ar ,ae1 ...)
     ; Note: We need to allocate the closure and then fill the free slots
     ;       in order to handle recursive references to the closure.
     ; If one of the ae1 ... are equal to dd,
     ; then the closure has a recursive reference.

     ;; (type $Args      (array (mut (ref eq))))
     ;; (type $Free      (array (mut (ref eq))))
     ;; (rec
     ;;  (type $Closure       (struct
     ;;                         (field $code (ref $ClosureCode))
     ;;                         (field $free (ref $Free))))
     ;;  (type $ClosureCode  (func (param $clos (ref $Closure))
     ;;                         (param $args (ref $Args))
     ;;                         (result (ref eq)))))
     
     ; If there is no self-reference then allocation is simple.
     ; If there is a self-reference we first need to allocate the closure,
     ; and then mutate fields with self-references.
     (define (maybe-store-in-dest e)
       ; (displayln (list '-------------- dd))
       (match dd
         [(or '<effect> '<value> #f)                      e]
         [dest                               (Store! dest e)]))

     ; (displayln (list dd))
     (define self-reference-indices
       (for/list ([ae ae1]
                  [i (in-naturals)]
                  #:when (and (variable? ae) (syntax? dd)
                              (eq? (Var ae) (syntax-e dd))))
         i))

     #; (type $Closure   
              (sub $Procedure
                   (struct
                     (field $hash   (mut i32))
                     (field $name   (ref eq))   ;; $false or a $String
                     (field $arity  (ref eq))   ;;fixnum (i31 with lsb=0) or (arity-at-least n)
                     (field $realm  (ref eq))   ;; $false or $Symbol
                     (field $invoke (ref $ProcedureInvoker))
                     (field $code   (ref $ClosureCode))
                     (field $free   (ref $Free)))))
          
     (maybe-store-in-dest
      (match self-reference-indices
        ; no self-references
        ['() `(struct.new $Closure
                (i32.const 0)                  ; hash
                (global.get $false)            ; name:  #f or $String
                ,(Imm ar)                      ; arity: fixnum
                (global.get $the-racket-realm) ; realm: #f or $Symbol
                (ref.func $invoke-closure)     ; invoke (used by apply, map, etc.)
                (ref.func ,(Label l)) 
                (array.new_fixed $Free ,(length ae1)
                                 ,@(for/list ([ae ae1])
                                     (AExpr3 ae <value>))))]
        [(list is)
         (define this (new-var #'$this))
         (emit-local this '(ref $Closure))
         `(block (ref eq)
                 ; 1. Allocate closure with dummy array of free variables.
                 (local.set ,this
                            `(struct.new $Closure    
                               (i32.const 0)               ; hash
                               (global.get $false)         ; name:  #f or $String
                               ,(Imm ar)                   ; arity: todo
                               (global.get $false)         ; realm: #f or $Symbol
                               (ref.func $invoke-closure)  ; invoke (used by apply, map, etc.)
                               (ref.func ,(Label l))
                               (global.get $empty-free)))
                 ; 2. Fill in the correct array of free variables.
                 (struct.set $Closure $free
                   (array.new_fixed $Free ,(length ae1)
                     ,@(for/list ([ae ae1] [i (in-naturals)])
                         (if (member i is)
                             `(local.get ,this)
                             (AExpr3 ae <value>)))))
                 ; 3. Return the closure.
                 (local.get ,this))]

        [_ (error 'internal-error "")]))])
  (CaseClosureAllocation : CaseClosureAllocation (cca dd) -> * ()
    ; dest = #f, means the cca is in value position
    [(case-closure ,s ,l [,ar ,ca] ...)
     (define t (or dd (Var (new-var)))) ; reuse dest if possible
     (let ([l  (Label l)]
           [ca (for/list ([c ca]) (ClosureAllocation c #f))])
       `(case-closure-allocation "todo" ,l ,t)
       #;`(app (lambda ()
                 (body
                  (var [binding ,t (array '"CLOS" ,#'dispatch-case-lambda
                                          (array (quote ,ar) ...)
                                          (array ,ca ...))])
                  ,t))))])
  
  (AExpr3 : AExpr (ae [dd #f]) -> * ()  ; only ca needs dest
    [,x               (if (memq (syntax-e (variable-id x)) primitives)
                          (PrimRef x)     ; reference to primitive
                          (Reference x))] ; reference to non-free variable
    [(free-ref ,x ,i) `(array.get $Free (local.get $free) (i32.const ,i))] ; x is the name
    [,ca              (ClosureAllocation ca dd)]
    [(quote ,s ,d)    (cond
                        [(eq? dd '<effect>)
                         `(nop)]
                        [else
                         (let ([v (datum-value d)])
                           (cond
                             ; We keep these for now, to get a more readable output.
                             ; In all likelyhood (Imm '()), (Imm (void)), etc. are better.
                             [(flonum? v)  `(struct.new $Flonum (i32.const 0) (f64.const ,v))]
                             [(null? v)    '(global.get $null)]
                             [(void? v)    '(global.get $void)]
                             [(eq? v #t)   '(global.get $true)]  
                             [(eq? v #f)   '(global.get $false)]
                             [(fixnum? v)  (Imm v)]
                             [(char? v)    (Imm v)]
                             #;[(symbol? v)  `(call $string->symbol  ',(~a v))]      ; todo
                             #;[(keyword? v) `(call $string->keyword ',(~a (keyword->string v)))] ; todo
                             [else         `',v]))])]
    [(top ,s ,x)
     ; Note: This is a quick hack until namespaces are implemented.
     ;       Note that if x is present in a top-level define-values
     ;       then (top x) will become x anyway.
     ; Note: What is missing here: is error handling for an undefined top-level-variable.
     `(struct.get $Boxed $v (ref.cast (ref $Boxed) ,(Reference x)))
     #;`(app ,#'namespace-variable-value (app ,#'string->symbol ',(symbol->string (syntax-e (variable-id x)))))
     ; ',#f ; use-mapping? TODO: this should be #t but that isn't implemented yet in runtime
     ]
    [(variable-reference ,s ,vrx)
     ; todo: dummy for now
     `(struct.new $VariableReference 
                  (i32.const 0))] ; hash
    [(quote-syntax ,s ,d)
     (raise-syntax-error 'generate-code "quote-syntax gone at this point")])

  (CExpr2 : CExpr (ce dd cd) -> * ()
    ;; All Complex Expressions are translated to statements
    [,ae                     (match dd
                               ['<effect>  `(nop)]
                               ['<value>   (match cd ; ClosureAllocation needs the dest
                                             ['<return> `(return ,(AExpr2 ae dd))]
                                             [_                   (AExpr2 ae dd)])]
                               [x          (Store! x (AExpr2 ae <value>))])]

    [(if ,s ,ae0 ,e1 ,e2)     `(if ,@(if (eq? dd <value>) '((result (ref eq))) '())
                                   (ref.eq ,(AExpr2 ae0 <expr>) (global.get $false))
                                   (then ,(Expr e2 dd cd))
                                   (else ,(Expr e1 dd cd)))]
    
    [(set! ,s ,x ,ae)         (match dd
                                ['<effect>  (Store! x (AExpr2 ae dd))]
                                ['<value>   `(block (ref eq)
                                                    ,(AExpr2 ae x)
                                                    (global.get $void))]
                                [y          (error 'todo-it-happened!)
                                            #;(CExpr `(begin (top:= ,(Var x) ,(AExpr ae))
                                                             (top:= ,y ,(Var x)))
                                                     dd cd)])]
    [(begin  ,s ,e0 ,e1 ... ) (match (cons e0 e1)
                                [(list e0 ... En)
                                 (let ([e0 (Expr* e0 <effect> <stat>)])
                                   (match dd
                                     ['<effect>  (define en (Expr  En dd cd))
                                                 `(block ,@e0 ,en)]
                                     ['<value>   (define en (Expr  En dd cd))
                                                 `(block (result (ref eq)) ,@e0 ,en)]
                                     [y          (define en (Expr En y '<stat>))
                                                 `(block ,@e0 ,en)]))])]
    [(begin0 ,s ,e0 ,e1 ...)  (match dd
                                ['<effect> `(block
                                             ,(Expr   e0 '<effect> '<stat>)
                                             ,@(Expr* e1 '<effect> '<stat>))]
                                ['<value>  (define t (emit-fresh-local 't))
                                           `(block (result (ref eq))
                                             ,(Expr   e0 t         '<stat>)
                                             ,@(Expr* e1 '<effect> '<stat>)
                                             ,(Reference t))]
                                ; is it okay (scope-wise) to assign to y?
                                [y         `(block 
                                             ,(Expr   e0 y         '<stat>)
                                             ,@(Expr* e1 '<effect> '<stat>))])]
    [(primapp ,s ,pr ,ae1 ...) (define sym (syntax->datum (variable-id pr)))
                               (define work
                                 (case sym
                                  [(s-exp->fasl)
                                   ; 1 to 2 arguments (in the keyword-less version in "core.rkt"
                                   (define aes (AExpr* ae1))
                                   (define n   (length aes))
                                   (when (> n 2) (error 'primapp "too many arguments: ~a" s))
                                   (when (< n 1) (error 'primapp "too few arguments: ~a"  s))
                                   (define m (- 2 n))
                                   (define optionals (make-list m `(global.get $missing)))
                                   `(call $s-exp->fasl ,@aes ,@optionals)]
                                  [(fasl->s-exp)
                                   ; exactly one argument
                                   (define aes (AExpr* ae1))
                                   (define n   (length aes))
                                   (when (> n 1) (error 'primapp "too many arguments: ~a" s))
                                   (when (< n 1) (error 'primapp "too few arguments: ~a" s))
                                   `(call $fasl->s-exp ,@aes)]
                                  [(void)
                                   (define (AE ae)   (AExpr3 ae <effect>))
                                   (define (AE* aes) (map AE aes))
                                    `(block (result (ref eq))
                                            ,@(AE* ae1)
                                            (global.get $void))]
                                   [(vector-copy!)
                                    ; 3 to 5 arguments
                                    (define aes (AExpr* ae1))
                                    (define n   (length aes))
                                    (when (> n 5) (error 'primapp "too many arguments: ~a" s))
                                    (when (< n 3) (error 'primapp "too few arguments: ~a"  s))
                                    (define m (- 5 n))
                                    (define optionals (make-list m `(global.get $missing)))
                                    `(call $vector-copy! ,@aes ,@optionals)]
                                   [(make-vector)
                                    ; 1 to 2 arguments
                                    (define aes (AExpr* ae1))
                                    (define n   (length aes))
                                    (when (> n 2) (error 'primapp "too many arguments: ~a" s))
                                    (when (< n 1) (error 'primapp "too few arguments: ~a"  s))
                                    (define m (- 2 n))
                                    (define optionals (make-list m `(global.get $missing)))
                                    `(call $make-vector ,@aes ,@optionals)]
                                   [(procedure-rename)
                                    ; 2 to 3 arguments
                                    (define aes (AExpr* ae1))
                                    (define n   (length aes))
                                    (when (> n 3) (error 'primapp "too many arguments: ~a" s))
                                    (when (< n 2) (error 'primapp "too few arguments: ~a"  s))
                                    (define m (- 3 n))
                                    (define optionals (make-list m `(global.get $missing)))
                                    `(call $procedure-rename ,@aes ,@optionals)]
                                   [(procedure-arity-includes?)
                                    ; Takes between 2 to 3 argument.
                                    ; The default value for the last argument is #f.
                                    (define aes (AExpr* ae1))
                                    (define n   (length aes))
                                    (when (> n 3)
                                      (error 'primapp "too many arguments: ~a" s))
                                    (define m (- 3 n))
                                    (define falses (make-list m (Imm #f)))
                                    `(call $procedure-arity-includes? ,@aes ,@falses)]
                                   [(make-hasheq) 
                                    ; Takes between 0 to 1 argument.
                                    ; The optional argument has no default, so use $missing.
                                    (define aes (AExpr* ae1))
                                    (define n   (length aes))
                                    (when (> n 1) (error 'primapp "too many arguments: ~a" s))
                                    (define m (- 1 n))
                                    (define falses (make-list m `(global.get $missing)))
                                    `(call $make-hasheq ,@aes ,@falses)]
                                   [(number->string)
                                    ; takes between 1 to 2 arguments.
                                    ; if arguments are missing, supply #f
                                    (define aes (AExpr* ae1))
                                    (define n   (length aes))
                                    (when (> n 2)
                                      (error 'primapp "too many arguments: ~a" s))
                                    (define m (- 2 n))
                                    (define falses (make-list m (Imm #f)))
                                    `(call $number->string ,@aes ,@falses)]
                                   [(make-struct-type)
                                    ; takes between 4 to 11 arguments.
                                    ; if arguments are missing, supply #f
                                    (define aes (AExpr* ae1))
                                    (define n   (length aes))
                                    (when (> n 11)
                                      (error 'primapp "too many arguments: ~a" s))
                                    (define m (- 11 n))
                                    (define falses (make-list m (Imm #f)))
                                    `(call $make-struct-type ,@aes ,@falses)]                                   
                                   [(make-struct-field-accessor)
                                    ; takes between 2 to 5 arguments.
                                    ; if arguments are missing, supply #f
                                    (define aes (AExpr* ae1))
                                    (define n   (length aes))
                                    (when (> n 5)
                                      (error 'primapp "too many arguments: ~a" s))
                                    (define m (- 5 n))
                                    (define falses (make-list m (Imm #f)))
                                    `(call $make-struct-field-accessor ,@aes ,@falses)]
                                   [(make-struct-field-mutator)
                                    ; takes between 2 to 5 arguments.
                                    ; if arguments are missing, supply #f
                                    (define aes (AExpr* ae1))
                                    (define n   (length aes))
                                    (when (> n 5)
                                      (error 'primapp "too many arguments: ~a" s))
                                    (define m (- 5 n))
                                    (define falses (make-list m (Imm #f)))
                                    `(call $make-struct-field-mutator ,@aes ,@falses)]
                                   [(list)   ; variadic
                                    (let loop ([aes ae1])
                                      (if (null? aes)
                                          `(global.get $null)
                                          `(struct.new $Pair
                                                       (i32.const 0)
                                                       ,(AExpr (first aes))
                                                       ,(loop (rest aes)))))]
                                   [(values) ; variadic
                                    (define n   (length ae1))
                                    (define aes (AExpr* ae1))
                                    (case n
                                      [(1)   (first aes)]
                                      [else  `(array.new_fixed $Values ,n ,@aes)])]
                                   [(bytes) ; variadic, needs inlining
                                    ; TODO: This assumes all the ae1 ... are integers.
                                    (define n    (length ae1))
                                    ; (func $make-bytes (param $k (ref eq)) (param $b (ref eq)) (result (ref eq))
                                    (define init `(ref.cast (ref $Bytes) (call $make-bytes ,(Imm n) ,(Imm 0))))
                                    ; (define init `(array.new $Bytes (i32.const 0) (i32.const ,n)))
                                    (define $bs  (emit-fresh-local 'quoted-bytes '(ref $Bytes) init))
                                    `(block (result (ref eq))
                                       ,@(for/list ([ae ae1] [i (in-naturals)])
                                           `(call $bytes-set!/checked
                                                  ,(Reference $bs) (i32.const ,i)
                                                  (i32.shr_s (i31.get_s ,(AExpr ae)) (i32.const 1))))
                                       ,(Reference $bs))]
                                   [(string) ; variadic, needs inlining for now
                                    ; TODO: This assumes all the ae1 ... are characters
                                    (define n    (length ae1))
                                    (define init `(array.new $String (i32.const 0) (i32.const ,n)))
                                    (define $qs  (emit-fresh-local 'quoted-string '(ref $String) init))
                                    `(block (result (ref eq))
                                      ,@(for/list ([ae ae1] [i (in-naturals)])
                                          `(array.set $String ,(Reference $qs) (i32.const ,i) 
                                                      (i32.shr_s (i31.get_s (ref.cast (ref i31) (call $char->integer ,(AExpr ae))))
                                                                 (i32.const 1))))
                                      ,(Reference $qs))]
                                   [(vector vector-immutable)
                                    (define n    (length ae1))
                                    ; (define init `(array.new $Vector (ref.i31 (i32.const 0)) (i32.const ,n)))
                                    (define init `(call $make-vector/checked (i32.const ,n) ,(Imm 0)))
                                    (define $vs  (emit-fresh-local 'quoted-vector '(ref $Vector) init))
                                    `(block (result (ref eq))
                                      ,@(for/list ([ae ae1] [i (in-naturals)])
                                          `(call $vector-set!/checked ,(Reference $vs) (i32.const ,i) ,(AExpr ae)))
                                          ; `(array.set $Vector ,(Reference $vs) (i32.const ,i) ,(AExpr ae)))
                                      ,(Reference $vs))]
                                   [else
                                    (match (length ae1)
                                      [0 (case sym
                                           [(+ -)      '(global.get $zero)]
                                           [(fx+ fx-)  '(global.get $zero)]
                                           [(fl+ fl-)  '(global.get $flzero)]
                                           [(* fx*)    '(global.get $one)]
                                           [(fl*)      '(global.get $flone)]
                                           [else       `(call ,(Prim pr))])]
                                      [1 (case sym
                                           [(  +   *)  (AExpr (first ae1))]
                                           [(fx+ fx*)  (AExpr (first ae1))]
                                           [(fl+ fl*)  (AExpr (first ae1))]
                                           [(fx-)      `(call ,(Prim pr) (global.get $zero)   ,(AExpr (first ae1)))]
                                           [(fl-)      `(call ,(Prim pr) (global.get $flzero) ,(AExpr (first ae1)))]
                                           [(fx/)      `(call ,(Prim pr) (global.get $one)    ,(AExpr (first ae1)))]
                                           [(fl/)      `(call ,(Prim pr) (global.get $flone)  ,(AExpr (first ae1)))]
                                           [else       `(call ,(Prim pr)                      ,(AExpr (first ae1)))])]
                                      [2 (case sym
                                           [(+ - *)       `(call ,(Prim pr)
                                                                 ,(AExpr (first ae1)) ,(AExpr (second ae1)))]
                                           [(fx+ fx- fx*) `(call ,(Prim pr)
                                                                 ,(AExpr (first ae1)) ,(AExpr (second ae1)))]                                           
                                           [(fl+ fl- fl*) `(call ,(Prim pr)
                                                                 ,(AExpr (first ae1)) ,(AExpr (second ae1)))]
                                           ; / needs to signal an Racket error if denominator is zero
                                           [else   `(call ,(Prim pr)
                                                          ,(AExpr (first ae1)) ,(AExpr (second ae1)))])]
                                      [_ (case sym
                                           [(+ fx+ fl+ 
                                             * fx* fl*
                                             - fx- fl-) ; (+ a b c ...) = (+ (+ a b) c ...)
                                            (let loop ([aes (AExpr* ae1)])
                                              (match aes
                                                [(list  ae0)         ae0]
                                                [(list* ae0 ae1 aes) `(call ,(Prim pr)
                                                                      (call ,(Prim pr) ,ae0 ,ae1)
                                                                      ,(loop aes))]))]
                                           [else
                                            `(call ,(Prim pr) ,@(AExpr* ae1))])])]))
                                   (match dd
                                     [(or '<value> '<effect>)
                                      (match cd
                                        ['<return>             `(return ,work)]
                                        ['<expr>                work]
                                        ['<stat>                `(drop ,work)]
                                        [_ (error)])]
                                     [x  (Store! x work)])]
    [(app ,s ,ae ,ae1 ...)
     ; Note: We do not know that type `ae` has.
     ;       We need to check that is an applicable value.
     
     ; TODO: Only one function application is active at a time, so we can
     ;       reuse the variable, $app-clos,  holding the function to be applied.

     ;;  (type $ClosureCode  (func (param $clos (ref $Closure))
     ;;                           (param $args (ref $Args))
     ;;                           (result (ref eq)))))
     (define tc (eq? cd '<return>))
     (define tc-flag (if tc (Imm #t) (Imm #f))) ; currently not used

     ; Package arguments in an $Args array.
     (define args `(array.new_fixed $Args ,(length ae1)
                                    ,@(for/list ([ae ae1])
                                        (AExpr3 ae <value>))))

     ; Variable to hold the closure to be applied
     (define clos-var (new-var '$app-clos))
     (define clos (syntax-e (variable-id clos-var)))
     (emit-local clos '(ref $Closure) '(global.get $dummy-closure)) ; todo: reuse

     ; The code of the closure
     (define code `(struct.get $Closure $code (local.get ,clos)))

     ; If `ae` is a procedure with rest arguments, we convert them
     ; to a list and pass them as the last argument.
     (define arity-var (emit-fresh-local 'arity 'i32))
     (define args-var  (emit-fresh-local 'arity '(ref $Args)))
     (define varargs
       (list (Store! arity-var ; convert fixnum arity to i32
                     `(i32.shr_s (i31.get_s (ref.cast (ref i31)
                                                      (struct.get $Closure $arity (local.get ,clos))))
                                 (i32.const 1)))
             (Store! args-var args)
             (Store! args-var `(call $repack-arguments
                                     ,(Reference args-var)
                                     ,(Reference arity-var)))))

     ; Call the closure. Note that the `code` argument goes last.
     (define work
       (if tc
           ; If `tc` is true, we generate `(return (return_call_ref ..))`,
           ; but that actually works in wasm.
           `(return_call_ref $ClosureCode (local.get ,clos) ,(Reference args-var) ,code)
           `(call_ref        $ClosureCode (local.get ,clos) ,(Reference args-var) ,code)))

     ; TODO: If the cast fails we get a runtime trap.
     ;       When exceptions are implemented, we need to insert type checks.
     (define (cast w)
       ; 1. check that w is applicable
       ; 2. is it a closure, struct ?
       `(ref.cast (ref $Closure) ,w))
     
     (match dd
       ['<effect> (match cd
                    ['<expr>   `(block (local.set ,clos ,(cast (AExpr ae))) ,@varargs       ,work)] ; should this add (ref eq)?
                    ['<stat>   `(block (local.set ,clos ,(cast (AExpr ae))) ,@varargs (drop ,work))]
                    [_         (error 'internal-app0 "combination impossible")])]
       ['<value>  (match cd
                    ['<return> `(block (result (ref eq))
                                       (local.set ,clos ,(cast (AExpr ae)))  ; <--
                                       ,@varargs
                                       (return ,work))]
                    ['<expr>   `(block (result (ref eq))
                                       (local.set ,clos ,(cast (AExpr ae)))
                                       ,@varargs
                                       ,work)]
                    [_         (display (list 'app ce dd cd))
                               (error 'internal-app1 "not impossible?!")])]
       [x         `(block 
                      (local.set ,clos ,(cast (AExpr ae)))
                      ,@varargs
                      ,(Store! x work))])]
    
    [(closedapp ,s ,ca ,ae2 ...)
     ; Note: In a `closedapp` we know that we are applying a closure,
     ;       so we do not need as much type checking as for `app`.
     
     ; ((λ (free ae1 ...) (args) l) 
     ;  ae2 ...)
     ; The ae2 ... must be packed up as arguments for the closure.
     ; The values of the free variables are ae1 ...
     ; The first argument of a closure is the closure itself,
     ; so we need a temporary value to hold the closure under construction.
     ; Since we can only construct one closure at a time, this variable
     ; can be a global: $closedapp-clos
     
     ;;  (type $ClosureCode  (func (param $clos (ref $Closure))
     ;;                           (param $args (ref $Args))
     ;;                           (result (ref eq)))))
     (let (#;[ae2 (AExpr* ae2)]) ; for effect
       (nanopass-case (LANF+closure ClosureAllocation) ca
         [(closure ,s ,l ,ar ,ae1 ...)
          (define tc (eq? cd '<return>))
          (define tc-flag (if tc (Imm #t) (Imm #f)))

          ; Package arguments in an $Args array.
          (define args
            `(array.new_fixed $Args ,(length ae2)
                              ,@(for/list ([ae ae2])
                                  (AExpr3 ae <value>))))
          
          (define work            
            `(block ,@(cond
                        [(eq? dd <value>)  (list `(result (ref eq)))]
                        [(eq? dd <effect>) (list)]
                        ; otherwise, the data destination is a variable
                        [else              (list `(result (ref eq)))])
              ; 1. Allocate closure and store it in $closedapp-clos
              ,(ClosureAllocation ca #'$closedapp-clos)
              ; 2. Call it. Some day we might need to pass `tc-flag` as well.
              ,(if tc
                   `(return_call ,(Label l) (global.get $closedapp-clos) ,args)
                   (cond
                     [(eq? dd <value>)        `(call ,(Label l) (global.get $closedapp-clos) ,args)]
                     [(eq? dd <effect>) `(drop (call ,(Label l) (global.get $closedapp-clos) ,args))]
                     ; otherwise, the data destination is a variable
                     [else                    `(call ,(Label l) (global.get $closedapp-clos) ,args)]))))

          #;(define work
            (match ae1
              [(list) ; no free variable => no need to actually allocate a closure, just jump to label
               (if tc
                   `(return_call ,(Label l) (global.get $undefined) ,tc-flag ,@ae2)   ; undefined due to no closure
                   `(call        ,(Label l) (global.get $undefined) ,tc-flag ,@ae2))]

              ; TODO: Only the no free variables case have been updated
              [_ (match dd
                   [(or '<effect> '<value>)
                    (let ([f (Var (new-var 'f))])
                      `(let ([,f (global.get $undefined)])
                         (body ,(ClosureAllocation ca f)
                               (app ,(Label l) ,f ,tc-flag ,ae2 ...))))]
                   [y
                    `(begin
                       ,(ClosureAllocation ca dd) ; use dest as temporary
                       (app ,(Label l) ,dd ,tc-flag ,ae2 ...))])]))
          (match dd
            [(or '<effect> '<value>)  (match cd
                                        ['<return> `(return ,work)]
                                        ['<expr>             work]
                                        ['<stat>             work])]
            [y                        (Store! y work)])]))]
    [(wcm ,s ,ae0 ,ae1 ,e)
     ;(displayln (list 'wcm 'dd dd 'cd cd s))
     (define-values (dest dest-declaration)
       (match dd
         [(or '<value> '<effect>) (let ([r (Var (new-var 'res))])
                                    (values r `(var [binding ,r '#f])))]
         [y                         (values y `(block))]))
     (let* ([key       (AExpr ae0)]
            [val       (AExpr ae1)]
            [result    (Expr e dest '<return>)] ; todo : is this correct?
            [enter     (match cd
                         ['<return>  `(sif ,_tc
                                           (app ,#'set-continuation-mark       ,key ,val)
                                           (app ,#'new-continuation-mark-frame ,key ,val))]
                         [_          `(app ,#'new-continuation-mark-frame ,key ,val)])]
            [leave     (match cd
                         ['<return> `(sif ,_tc
                                          (block) ; no frame to remove in tail position
                                          (app ,#'remove-continuation-mark-frame))]
                         [_         `(app ,#'remove-continuation-mark-frame)])]
            [return-it (match cd
                         ['<return> `(return ,dest)]
                         ['<effect> `(block)]
                         [_                   dest])])
       `(block
         (var [binding ,#'old_tc ,_tc])
         ; (app ,#'console.log ,_tc)
         ,dest-declaration                            ; maybe declare temporary variable
         (try {,enter                                 ; add new frame (in non-tail position)
               (:= ,_tc '#t)                          ; an inner wcm is in tail pos (relatively)
               ; wrap in lambda - since result might have a return
               (app (lambda () (body ,result '#f)))} ; calculate result
              (finally
               (:= ,_tc ,#'old_tc)
               ,leave))                      ; remove frame  (in non-tail position)
         ,return-it))])                               ; return the result                
  (Expr2 : Expr  (e  dd [cd #f]) -> * ()  ; todo the #f default is a temporary fix
    [,ce (CExpr ce dd cd)]

    [(let-values ,s (((,x** ...) ,ce*) ...) ,e)
     (let ()
       (define initialize
         (for/list ([x* x**] [ce ce*])
           ; Declare x* as local variables in the enclosing `func`.
           ; Also handles initialization.
           (for-each emit-local (reverse x*))
           ; Evaluate the ce* and store the results in the x*
           (match x*
             ; single value return is the common case, so this needs to be fast
             [(list x)         (CExpr ce x <stat>)]  ; single value
             ; multiple values are returned in an $Values array [v0,v1,...].
             ; to avoid allocating an extra variable, we receive the array in x0,
             ; then assign the individual variables (in reverse order so x0 is last)
             [(list x0 x1 ...)  (define mv (emit-fresh-local 'mv  '(ref null $Values)))
                                (define t  (emit-fresh-local 'mvt)) ; (ref eq)
                                (define n  (length (cons x0 x1)))
                                ; The result of (CExpr ce t <stat>) always produces an (ref eq).
                                ; To make casting easier, we make an extra local.
                                 `(block ,(CExpr ce t <stat>)
                                         ,(Store! mv `(ref.cast (ref $Values) ,(Reference t)))
                                         (if (i32.eqz (i32.eq (array.len ,(Reference mv))
                                                              (i32.const ,n)))
                                             (then (call $raise-wrong-number-of-values-received)))                                             
                                         ,@(for/list ([x (cons x0 x1)]
                                                      [i (in-naturals)])
                                             (Store! x `(array.get $Values ,(Reference mv) (i32.const ,i)))))]
             ; no values are expected
             ['() (CExpr ce <effect> '<stat>)]))) ; todo: signal error if values are produced
       (let* ([xs (map Var (append* x**))]            
              [e  (Expr e dd cd)])
         ; (displayln (list 'letv 'dd dd 'cd cd))
         `(block ,@(case cd
                     [(<stat>)          '()]
                     [(<expr> <return>) '((result (ref eq)))]
                     [else (error 'internal)])
           ,@initialize
           ,e)))]

    [(letrec-values ,s (((,x** ...) ,ce*) ...) ,e)
     #;(displayln (list 'letrec-values (map list x** ce*) e)
                (current-error-port))

     ; During the initial parsing we currently use this transform (ish):
     ;    (letrec-values ([(x ...) ce] ...) e)
     ; => (let ([x undefined] ... ...])
     ;      (letrec ([xl le] ...)              ; xl is an x bound to a λ-expresion
     ;        (let-values ([(t ...) ce])
     ;           (set! x t) ...)
     ;        ...
     ;        e)
     ; The idea is to make all right-hand sides of the `letrec` into lambda-abstractions.
     ; Closure conversion will turn these into closure allocations.
     ; The closures are built together and patched, so they can see each other.

     ; However, if `xl` is assigned to, then assignment conversion will turn the
     ; lambda-expression into (primapp boxed λ-expression). Also, references 
     ; to `xl` are turned into `(unboxed xl)`.

     


     ; At this point all right hand sides `ce` are closure allocations ... or `closedapp`s.
     ; Due to the scope of `letrec-values` the left hand
     ; variables `x` can occur as free variables at the
     ; `closure` expression at the right hand side.
     ; The strategy is thus to allocate closure shells,
     ; and fill in the free variables after.

     (unless (apply = (list* 1 1 (map length x**))) ; all clauses expect one value?
       (error 'generate-code "TODO support multiple values in letrec-values"))
     
     (define (AllocateClosure ca dest) ; called by letrec-values
       ; Allocates a closure in which the $free array contains zeros only.
       (nanopass-case (LANF+closure ClosureAllocation) ca
         [(closure ,s ,l ,ar ,ae1 ...)
          (let ([us (make-list (length ae1) (Undefined))])
            (Store! dest `(struct.new $Closure
                               (i32.const 0)               ; hash
                               (global.get $false)         ; name:  #f or $String
                               ,(Imm ar)                   ; arity: 
                               (global.get $false)         ; realm: #f or $Symbol
                               (ref.func $invoke-closure)  ; invoke (used by apply, map, etc.)
                               (ref.func ,(Label l))
                               (array.new_fixed $Free ,(length ae1) ,@us))))]
         [else (error 'AllocateClosure "internal error, got: ~a" ca)]))

     (define (FillClosure ca dd)
       ; Fill in the $free array
       (nanopass-case (LANF+closure ClosureAllocation) ca
         [(closure ,s ,l ,ar ,ae1 ...)
          ; This will be sliced into a `block` so we don't need another one.
          (append*
           (for/list ([ae (AExpr* ae1)]
                      [i  (in-naturals)])
             `(array.set $Free
                         (struct.get $Closure $free (ref.cast (ref $Closure) ,(Reference dd)))
                         (i32.const ,i)
                         ,ae)))]))

     ; declare the `x` as a local variables
     (for ([x* x**])
       (for ([x x*])
         (emit-local x)))
     
     (let* ([x* (map first x**)]                     ; assumes single value
            [u* (for/list ([x* x**]) (Undefined))]
            [e  (Expr e dd cd)])                     
       `(block ,@(for/list ([x x*] [u u*])
                   (Store! x u))
               ,@(map AllocateClosure ce* x*) 
               ,@(map FillClosure     ce* x*)
               ,e))])
     
  (ConvertedAbstraction : ConvertedAbstraction (cab) -> * ()
    [(λ ,s ,ar (formals (,x ...)) ,e)
     ; Note: The arity `ar`  
     ;          +n means precisely n     (nothing special to do)
     ;          0 means precisely  0
     ;         -1 means at least   0     (convert extra arguments to list)
     ;         -2 means at least   1     (convert extra arguments to list)
     ;         -n means at least   n-1   (convert extra arguments to list)
     ; Note: The other cases in the code generator pass the original arity along,
     ;       and rewrites the arguments to (,x ...) so `ar` and `(,x ...)`
     ;       might not match at this point.     
     (parameterize ([*locals* '()])
       (let* ([x (map Var x)] [σ (Expr e <value> <return>)])
         (define-values (src line col pos span)
           (values (~a (syntax-source s)) (syntax-line s) (syntax-column s)
                   (syntax-position s) (syntax-span s)))
         ; 1. For each argument, we make an WebAssembly parameter.
         (define (Formal  x) `(param ,x (ref eq)))
         (define (Formal* xs) (map Formal xs))
         ; 2. 
         (define (Local  x)  (match x
                               [(list v t)      `(local ,(if (symbol? v) v (Var v)) ,t)]   ; let bound 
                               [(list v t init) `(local ,(if (symbol? v) v (Var v)) ,t)]   ; emitted local
                               [x               `(local ,x (ref eq))]))                    ; argument
         (define (Local* xs) (map Local xs))
         (define (Init  x)  
           (match x
             [(list v t)      (if (equal? t '(ref eq))
                                  `(local.set ,(if (symbol? v) v (Var v)) ,(Undefined))
                                  '(nop))]
             [(list v t #f)   `(local.set ,(if (symbol? v) v (Var v)))] ; #f = no init
             [(list v t init) `(local.set ,(if (symbol? v) v (Var v)) ,init)]))
         (define (Init* xs) (map Init xs))
         (define (InitArgs* xs)
           (for/list ([x xs] [i (in-naturals)])
                `(local.set ,x (array.get $Args (local.get $args) (i32.const ,i)))))

         ; This list (whose head is 'converted-abstraction) will be turned into a
         ; a (func ...) by `define-label`.
         `(converted-abstraction ; ,(Formal* (list* '$free '$tc x)) ; arguments
                                 ; the parameter matches the type $ClosureCode
                                 ((param $clos (ref $Closure)) (param $args (ref $Args)))                                                     
                                 ,(cons
                                   '(local $free (ref $Free))                   ; array of free variables
                                   (Local* (append x                            ; argument
                                                   (reverse (*locals*)))))      ; declarations
                                 ,(append (list
                                           ; ARITY CHECK
                                           (cond
                                             ; precisely ar arguments expected
                                             [(>= ar 0) 
                                              `(if (i32.eqz (i32.eq (array.len (local.get $args))
                                                                    (i32.const ,ar)))
                                                   (then (call $raise-arity-error:exactly)
                                                         (unreachable)))]
                                             [else
                                              ; at least n expected
                                              (define n (- (- ar) 1))
                                              `(if (i32.lt_u (array.len (local.get $args))
                                                             (i32.const ,n))
                                                   (then (call $raise-arity-error:at-least)
                                                         (unreachable)))])
                                           
                                           `(local.set $free (struct.get $Closure $free (local.get $clos))))
                                          (Init* (*locals*))               ; initialization
                                          (InitArgs* x))
                                 ,σ)))
         #;`(func (param ,#'_free) (param ,#'_tc) (param ,@x))              
         #;`(lambda (,#'_free ,_tc ,x ...)
              (body (sif (app ,#'= ',(+ 2 ar) (ref ,#'arguments '"length")) ,#'VOID
                         (app ,#'raise-clos-arity-error* (app ,#'list ',src ',line ',col ',pos ',span)
                              ,#'_free ',ar ,#'arguments))
                    ,σ ,#'VOID))]
    [(λ ,s ,ar (formals ,x) ,e) ; variadic
     ; Note: the old arity is passed along, so ConvertedAbstraction
     ;       knows to insert an arguments-to-list conversion.
     (ConvertedAbstraction 
      (with-output-language (LANF+closure ConvertedAbstraction)
        `(λ ,s ,ar (formals (,x)) ,e)))]
     ;; ; todo
     ;; (let* ([x (Var x)] [σ (Expr e <value> <return>)])
     ;;   `(converted-abstraction ,ar (,@x) ,e)
     ;;   #;`(lambda (,#'_free ,_tc ,x)
     ;;      (body (:= ,x (app ,#'rest-args->list ,#'arguments '0))
     ;;            ,σ ,#'VOID)))]
    [(λ ,s ,ar (formals (,x0 ,x1 ... . ,xd)) ,e) ; rest arguments
     ; Note: the old arity is passed along, so ConvertedAbstraction
     ;       knows to insert an rest-arguments-to-list conversion for xd.
     (ConvertedAbstraction 
      (with-output-language (LANF+closure ConvertedAbstraction)
        `(λ ,s ,ar (formals (,x0 ,x1 ... ,xd)) ,e)))]
    ;; (let* ([x0 (Var x0)] [x1 (map Var x1)] [xd (Var xd)] [σ (Expr e <value> <return>)]
    ;;                       [ar (+ 1 (length x1))])
    ;;    (define-values (src line col pos span)
    ;;      (values (~a (syntax-source s)) (syntax-line s) (syntax-column s)
    ;;              (syntax-position s) (syntax-span s)))
    ;;    `(converted-abstraction ,ar (,x0 ,@x1 . ,xd) ,e)
    ;;    #;`(lambda (,#'_free ,_tc ,x0 ,x1 ... ,xd)
    ;;       (body (sif (app ,#'<= ',(+ 2 ar) (ref ,#'arguments '"length")) ,#'VOID
    ;;                  (app ,#'raise-clos-arity-error* (app ,#'list ',src ',line ',col ',pos ',span)
    ;;                       ,#'_free ',(- (+ ar 1)) ,#'arguments))
    ;;             (:= ,xd (app ,#'rest-args->list ,#'arguments ',(length (cons x0 x1))))
    ;;             ,σ ,#'VOID)))]
    [(λ ,s ,ar ,f ,e) (error 'fallthrough)])
  (GeneralTopLevelForm : GeneralTopLevelForm (g dd) -> * ()
    [,e                           (Expr e dd <stat>)]
    [(#%require     ,s ,rrs ...)  `'"ignored #%require"]
    [(define-values ,s (,x ...)   ,e)
     (match x
       ['()       (Expr e <effect> <stat>)]
       [(list x0) (cond
                    ; Until we implement namespaces, top-level variables are
                    ; stored as boxed global variables.
                    #;[(top-variable? x0) #;`(block ,(Expr e x0 <stat>)
                                                  ,(Store! x0 `(call $boxed ,(Reference x0))))
                                        (Store! x0 (Expr e x0 <stat>))]
                    [else               (Expr e x0 <stat>)])]
       [_
        (error 'generate-code "todo")])]

     ; Top Level     
     ;   Global variables outside modules.
     ;   Eventually, they need to stored in a namespace.
     ;   For now, we use a global WebAssembly variable.
     ; Elsewhere, we have declared the variable as global using:
     ;  `(global ,x0 (ref eq) ,(Fixnum 0))  

     #;(match (map Var x)
       [(list x0) ; single value is the common case
        `(block (var ,x0) ,(Expr e x0 <stat>))]
       [(list x0 x1 ...)
        (define x* (cons x0 x1))
        `(block (var ,x0 ,x1 ...)
                ,(Expr e x0)    ; reuse x0 to store values
                ,(for/list ([i (in-range (length x*) 0 -1)]
                            [x (reverse x*)])
                   `(:= ,x (ref ,x0 ',i)))
                ...)]
       ['() (error 'generate-code "TODO define-values of no values")])

    [(define-syntaxes ,s (,x ...) ,e)
     (error 'generate-code "TODO define-syntaxes")])

  (TopLevelForm : TopLevelForm (t dd) -> * ()
    [,g                             (GeneralTopLevelForm g dd)]
    [(#%expression ,s ,e)           (Expr e dd <stat>)]
    [(define-label ,l ,cab)         (match (ConvertedAbstraction cab)
                                      [(list 'converted-abstraction formals locals init-locals body-expr)
                                       `(func ,(Label l)
                                              (export ,(~a (syntax-e (variable-id l))))
                                              (type $ClosureCode)                                              
                                              ,@formals
                                              (result (ref eq))
                                              ,@locals
                                              ,@init-locals
                                              ,body-expr)]
                                      [x (pretty-print x)
                                         (error 'generate-code-Top "got: ~a" x)])]
    [(topmodule ,s ,mn ,mp ,mf ...) (let ([mf (ModuleLevelForm* mf dd)])
                                      `(topmodule ,mn ,mp ,mf ...))]
    [(topbegin ,s ,t ...)
     ; Note: We have flattened `topbegin` so no `t` is a `topbegin`.
     ; The data destination of `topbegin` is `$result`.
     ; The first forms are evaluated for effect.
     ; The last one stores the result in `$result`.
     (let ()
       (define (define-label? t)
         (nanopass-case (LANF+closure TopLevelForm) t
           [(define-label ,l ,cab) #t]
           [else                   #f]))
       (define (topmodule? t)
         (nanopass-case (LANF+closure TopLevelForm) t
           [(topmodule ,s ,mn ,mp ,mf ...) #t]
           [else                           #f]))

       (define-values (dls ts1) (partition define-label? t))
       (define-values (tms ts)  (partition topmodule?    ts1))

       ; (displayln (list 'topbegin 'dd dd))

       (values
        ; A `define-label` form becomes a function declaration in Web Assembly.
        (TopLevelForm* dls dd)
        ; A `topmodule` becomes ...
        (TopLevelForm* tms dd)
        ; The expressions and general top-level form becomes
        ; part of the body of the `entry` function called from the host language.
        (match ts
          [(list)
           ; (displayln "HERE2")
           ; No expressions at the top-level: simply return void
           `(global.set $result (global.get $void))]
          [(list t0 ... tn)
           ; (displayln "HERE")
           (define (non-nop? x) (not (equal? x '(nop))))
           (let ([t0s (filter non-nop? (TopLevelForm* t0 <effect>))]
                 [tn  (TopLevelForm  tn dd)])
             (if (null? t0s)
                 tn
                 `(block ,@t0s ,tn)))])))])

        #;(let ([ts (TopLevelForm* ts dd)])
          (match ts
            [(list t0)               t0]
            [(list t0 t1)            `(block #;(result (ref eq))
                                             ,t0
                                             ,t1)]
            [(list 'begin ts ... tn) `(block #;(result (ref eq))
                                             ,@ts
                                             ,tn)]
            [_ (error 'internal-here "got: ~a" ts)]))
  
  (ModuleLevelForm : ModuleLevelForm (mf dd) -> * ()
    [(define-label ,l ,cab)         
     `(define-label ,l ,(ConvertedAbstraction cab))
     #;`(var [binding ,(Label l) 
                      ,(ConvertedAbstraction cab)])]
    [,g
     (GeneralTopLevelForm g dd)])
  

  (let* ([result #'$result]) ; an wasm identifier
    (define-values (dls tms entry-body) (TopLevelForm T result))
    (let* (
           ; [prims    (ur-urmodule-name->exports 'runtime)]
           ; [prims    primitives]
           ; [pr       (map (λ (prim) (datum->syntax #'runtime prim)) prims)]
           ; [pr-str   (map (λ (prim) (~a (ur-mangle prim))) pr)]
           ; [pr-str   (map (λ (prim) (~a prim)) pr)]
           ; [RUNTIMES (map (λ (_) #'RUNTIME) prims)]
           ; [e        #'e]
           )
    
    ;; Variable definitions on the top-level ought to go in a namespace,
    ;; but for now we use a global WebAssembly variable.

    (define (TopVars)
      (for/list ([x top-vars])
        `(global ,(Var x) (mut (ref eq)) ,(Undefined))))

    ; R = Make reference
    (define (R x [type #f])
      (case type
        [(#f)  (match x
                 [(? integer?) `(ref.i31 (i32.const ,x))]
                 [_            `(ref.cast (ref eq) ,x)])]
        [else  `(ref.cast ,type ,x)]))

    ; D = Dereference
    (define (D x type)
      ; type is one of:
      ;   'i32           cast to signed i32
      ;   '(ref <type>)  cast to (ref <type>)
      
      ; x evaluates to an (ref eq), cast x to type
      (match type
        ['i32 `(i31.get_s (ref.cast i31ref ,x))]
        ['u32 `(i31.get_u (ref.cast i31ref ,x))]
        [type `(ref.cast (ref ,type) ,x)]))

    (define (I32 x) (D x 'i32))
    (define (U32 x) (D x 'u32))
    (define (Half x)   `(i32.shr_s ,x (i32.const 1)))
    (define (Double x) `(i32.shl   ,x (i32.const 1)))

    
    (define (declare-primitives-as-globals)
      (for/list ([pr (sort primitives symbol<?)]
                 #:do [(define desc (primitive->description pr))]
                 #:when desc)
        `(global ,($ (prim: pr)) (mut (ref eq)) ,(Imm (undefined)))))

    (define (arity->internal-representation a)
      (match a
        [(arity-at-least n) (- (- n) 1)]
        [(? number? a)      a]
        [#f                 #f]
        [_                  #f]
        [_                  (error 'arity->internal-representation "got: ~a" a)]))
    
    (define (initialize-primitives-as-globals)
      (for/list ([pr (sort primitives symbol<?)]
                 #:do [(define desc (primitive->description pr))]
                 #:when desc)
        `(global.set ,($ (prim: pr))
                     (struct.new $PrimitiveProcedure
                      ; for $Procedure
                      (i32.const 0)                               ; hash
                      ,(Imm #f)                                   ; name
                      ,(Imm (arity->internal-representation
                             (primitive-description-arity desc))) ; arity
                      (global.get $the-racket/primitive-realm)    ; realm
                      ; ,(primitive-description-realm desc)       ; todo
                      (ref.func $invoke-primitive)
                      ; for $PrimitiveProcedure
                      ; (ref.func ,($ pr))
                      ,(Imm #f
                            #;(arity->internal-representation
                               (primitive-description-result-arity desc)))))))

    ;; String constants used in the runtime
    (define runtime-string-constants '())
    (define (add-runtime-string-constant name string)
      (set! runtime-string-constants
            (cons (list name string) runtime-string-constants)))
    (define (declare-runtime-string-constants)
      (append*
       (for/list ([ns (reverse runtime-string-constants)])
         (define name         (first ns))
         (define string       (second ns))
         (define $bytes:name  (string->symbol (~a "$" "bytes:"  name)))
         (define $string:name (string->symbol (~a "$" "string:" name)))
         (list `(data   ,$bytes:name ,(~a string))
               `(global ,$string:name (mut (ref eq)) ,(Imm #f))))))
    (define (initialize-runtime-string-constants)
      (for/list ([ns (reverse runtime-string-constants)])
        (define name         (first ns))
        (define string       (second ns))
        (define $bytes:name  (string->symbol (~a "$" "bytes:"  name)))
        (define $string:name (string->symbol (~a "$" "string:" name)))
        (define n            (string-length string))
        `(global.set ,$string:name
                     (call $i8array->string
                           (array.new_data $I8Array ,$bytes:name
                                           (i32.const 0) (i32.const ,n))))))

    ;; Symbol constants used in the runtime
    (define runtime-symbol-constants '())
    (define (add-runtime-symbol-constant symbol)
      (define name symbol)
      (add-runtime-string-constant name (~a symbol))
      (set! runtime-symbol-constants
            (cons (list name symbol) runtime-symbol-constants)))
    (define (declare-runtime-symbol-constants)
      (for/list ([ns (reverse runtime-symbol-constants)])
        (define name         (first ns))
        (define symbol       (second ns))
        (define $symbol:name (string->symbol (~a "$" "symbol:" name)))
        (add-runtime-string-constant name (symbol->string symbol))
        `(global ,$symbol:name (mut (ref eq)) ,(Imm #f))))
    (define (initialize-runtime-symbol-constants)
      (for/list ([ns (reverse runtime-symbol-constants)])
        (define name         (first ns))
        (define symbol       (second ns))
        (define $string:name (string->symbol (~a "$" "string:" name)))
        (define $symbol:name (string->symbol (~a "$" "symbol:" name)))        
        `(global.set ,$symbol:name
                     (call $string->symbol
                           (global.get ,$string:name)))))

    ;; String and symbol constants used in the runtime
    (add-runtime-symbol-constant 'racket)
    (add-runtime-string-constant 'hash-variable-reference  "#<variable-reference>")
    (add-runtime-string-constant 'box-prefix               "#&")
        
    `(module
         ;;;
         ;;; Internal Types
         ;;;

         ; The following internal types are used to implement the various Racket data types.
         (type $Array    (array (mut (ref eq))))
         (type $I32Array (array (mut i32)))
         (type $I8Array  (array (mut i8)))

         (type $GrowableArray 
               (struct
                 (field $arr (mut (ref $Array)))  ;; underlying array
                 (field $cap (mut i32))           ;; capacity (the size of the array)
                 (field $i   (mut i32))))         ;; the index of the next free slot in $arr

         (type $GrowableBytes 
               (struct
                 (field $arr (mut (ref $I8Array))) ;; underlying byte array
                 (field $cap (mut i32))            ;; capacity of the array
                 (field $i   (mut i32))))          ;; next free index
         
         (type $I32GrowableArray
               (struct
                 (field $arr (mut (ref $I32Array))) ;; underlying array
                 (field $cap (mut i32))             ;; capacity
                 (field $i   (mut i32))))           ;; current size

         
         ; The type $Boxed is used for assignable variables.
         ; They are not exposed, so they do not need to carry a hash code.
         (type $Boxed (struct (field $v (mut (ref eq)))))

         ;;;
         ;;; Support 
         ;;;

         (type $Values    (array (mut (ref eq))))  ; for multiple values return
         (type $Args      (array (mut (ref eq))))  ; holds arguments passed to a closure
         (type $Free      (array (mut (ref eq))))  ; holds captured free variables
         
       ;;;
       ;;; Types: Heap allocated objects
       ;;;
       
       ; All heap allocated values carry a hash code.
       ; If the hash code is 0 it hash hasn't been computed yet.
         (rec
          (type $Heap (sub (struct (field $hash (mut i32)))))
          (type $Pair (sub $Heap
                           (struct
                             (field $hash (mut i32))
                             (field $a    (mut (ref eq)))
                             (field $d    (mut (ref eq))))))
          (type $Box  (sub $Heap
                           (struct
                             (field $hash (mut i32))
                             (field $v    (mut (ref eq))))))

          (type $Procedure
                (sub $Heap
                     (struct
                       (field $hash   (mut i32))
                       (field $name   (ref eq))   ;; $false or a $String    
                       (field $arity  (ref eq))   ;; fixnum (i31 with lsb=0)
                       ;                              +n means precisely n  
                       ;                               0 means precisely  0
                       ;                              -1 means at least 0   
                       ;                              -2 means at least 1   
                       ;                              -n means at least n-1 
                       (field $realm  (ref eq))   ;; $false or $Symbol
                       (field $invoke (ref $ProcedureInvoker)))))

          (type $ProcedureInvoker
                (func (param $proc (ref $Procedure))
                      (param $args (ref $Args))      ; an array of (ref eq)
                      (result (ref eq))))
          
          (type $ClosureCode  (func (param $clos (ref $Closure))
                                    (param $args (ref $Args))
                                    (result (ref eq))))
          (type $Closure   
                (sub $Procedure
                     (struct
                       ; from $Procedure
                       (field $hash   (mut i32))
                       (field $name   (ref eq))   ;; $false or a $String
                       (field $arity  (ref eq))   ;; fixnum (i31 with lsb=0) or (arity-at-least n)
                       (field $realm  (ref eq))   ;; $false or $Symbol
                       (field $invoke (ref $ProcedureInvoker))
                       ; from $Closure
                       (field $code   (ref $ClosureCode))
                       (field $free   (ref $Free)))))
          

          (type $PrimitiveProcedure
                (sub $Procedure
                     (struct
                       ; From $Procedure
                       (field $hash   (mut i32))
                       (field $name   (ref eq))
                       (field $arity  (ref eq))
                       (field $realm  (ref eq))
                       (field $invoke (ref $ProcedureInvoker))
                       ; own fields
                       ; (field $code   (ref $PrimitiveCode))
                       (field $result-arity (ref eq))))) ;; fixnum like 1 for most

          (type $PrimitiveClosure
                (sub $PrimitiveProcedure
                     (struct
                       ;; Inherits all fields from $PrimitiveProcedure:
                       (field $hash         (mut i32))
                       (field $name         (ref eq))
                       (field $arity        (ref eq))
                       (field $realm        (ref eq))
                       (field $invoke       (ref $ProcedureInvoker))
                       (field $result-arity (ref eq))  ; fixnum                     
                       ;; Own fields
                       ; ...
                       )))


          ; Some structs are applicable.
          ; In order to make function calls fast, we make structs a subtype of $Procedure.
          ; Non-applicable structs gets an $invoke function that signals an error.
          ; The down side is that we add 4 (four!) extra fields.
          (type $Struct
                (sub $Procedure
                     (struct
                       ; from the procedure super-type
                       (field $hash   (mut i32))  ;; Computed lazily, starts at 0
                       (field $name   (ref eq))   ;; $false or a $String
                       (field $arity  (ref eq))   ;; fixnum (i31 with lsb=0) or (arity-at-least n)
                       (field $realm  (ref eq))   ;; $false or $Symbol
                       (field $invoke (ref $ProcedureInvoker))
                       ; fields for structs
                       (field $type   (ref $StructType))      ;; Pointer to struct type descriptor
                       (field $fields (ref $Array)))))        ;; Array of (ref eq), holds field values        

          (type $StructConstructorProcedure
                ; current representation is a plain closure
                (sub $Closure
                     (struct
                       (field $hash   (mut i32))
                       (field $name   (ref eq))   ;; $false or a $String
                       (field $arity  (ref eq))   ;; fixnum (i31 with lsb=0) or (arity-at-least n)
                       (field $realm  (ref eq))   ;; $false or $Symbol
                       (field $invoke (ref $ProcedureInvoker))
                       (field $code   (ref $ClosureCode))
                       (field $free   (ref $Free)))))

          (type $StructPredicateProcedure
                ; current representation is a plain closure
                (sub $Closure
                     (struct
                       (field $hash   (mut i32))
                       (field $name   (ref eq))   ;; $false or a $String
                       (field $arity  (ref eq))   ;; fixnum (i31 with lsb=0) or (arity-at-least n)
                       (field $realm  (ref eq))   ;; $false or $Symbol
                       (field $invoke (ref $ProcedureInvoker))
                       (field $code   (ref $ClosureCode))
                       (field $free   (ref $Free)))))

          (type $StructAccessorProcedure
                ; current representation is a plain closure
                (sub $Closure
                     (struct
                       (field $hash   (mut i32))
                       (field $name   (ref eq))   ;; $false or a $String
                       (field $arity  (ref eq))   ;; fixnum (i31 with lsb=0) or (arity-at-least n)
                       (field $realm  (ref eq))   ;; $false or $Symbol
                       (field $invoke (ref $ProcedureInvoker))
                       (field $code   (ref $ClosureCode))
                       (field $free   (ref $Free)))))

          (type $StructMutatorProcedure
                ; current representation is a plain closure
                (sub $Closure
                     (struct
                       (field $hash   (mut i32))
                       (field $name   (ref eq))   ;; $false or a $String
                       (field $arity  (ref eq))   ;; fixnum (i31 with lsb=0) or (arity-at-least n)
                       (field $realm  (ref eq))   ;; $false or $Symbol
                       (field $invoke (ref $ProcedureInvoker))
                       (field $code   (ref $ClosureCode))
                       (field $free   (ref $Free)))))

          (type $Number (sub $Heap    ; abstract super type for boxed numbers
                             (struct
                               (field $hash (mut i32)))))
          (type $Flonum (sub $Number  ; double precision
                             (struct
                               (field $hash (mut i32))  ; sigh
                               (field $v f64))))
          
          (type $Vector    (sub $Heap
                                (struct
                                  (field $hash (mut i32))
                                  (field $arr  (ref $Array)))))
          (type $String    (sub $Heap
                                (struct
                                  (field $hash      (mut i32))
                                  (field $immutable i32)                        ;; 0 or 1
                                  (field $codepoints (mut (ref $I32Array))))))  ;; An array of Unicode code points
          (type $Bytes      (sub $Heap
                                 (struct
                                   (field $hash      (mut i32))
                                   (field $immutable i32)                      ;; 0 or 1
                                   (field $bs        (mut (ref $I8Array)))))) ;; An array of bytes
          (type $Symbol (sub $Heap
                             (struct
                               (field $hash          (mut i32))          ;; cached hash
                               (field $name          (ref $String))      ;; symbol name (string)
                               (field $property-list (mut (ref eq))))))  ;; user-defined properties
          (type $Keyword (sub $Heap
                              (struct
                                (field $hash  (mut i32))
                                (field $str   (ref $String)))))  ; string without #:
          
          (type $Location   (sub $Heap
                                 ; If line counting is not enabled, the first two are #f
                                 (struct
                                   (field $hash  (mut i32))
                                   (field $line  (mut (ref eq)))    ; #f or line number (fixnum)
                                   (field $col   (mut (ref eq)))    ; #f or column number
                                   (field $pos   (mut (ref eq)))))) ; #f or position
          (type $StringPort (sub $Heap
                                 (struct
                                   (field $hash  (mut i32))
                                   (field $bytes (mut (ref $Bytes)))     ; the byte string (bytes)
                                   (field $name  (mut (ref eq)))         ; the port name   (string)
                                   (field $len   (mut i32))              ; the length of the string
                                   (field $idx   (mut i32))              ; the current index into the string
                                   (field $loc   (mut (ref $Location)))  ; the current location
                                   ;; UTF-8 decoder state:
                                   (field $utf8-len    (mut i32))     ;; 0 = idle, 1-4 = number of bytes expected
                                   (field $utf8-left   (mut i32))     ;; number of continuation bytes still needed
                                   (field $utf8-bytes  (mut i32)))))  ;; current byte count seen (for column fix)

          (type $StructType
                (sub $Heap
                     (struct
                       (field $hash             (mut i32))            ;; Computed lazily, starts at 0
                       (field $name             (ref $Symbol))        ;; Struct name
                       (field $super            (ref eq))             ;; Supertype or #f (use global $false)
                       (field $field-count      i32)                  ;; Total number of fields
                       (field $init-indices     (ref eq))             ;; List of init field indices ($Pair or $null)
                       (field $auto-indices     (ref eq))             ;; List of auto field indices
                       (field $auto-values      (ref eq))             ;; List of values for auto fields
                       (field $properties       (ref eq))             ;; Property table: hash table
                       (field $inspector        (ref eq))             ;; Inspector object or #f
                       (field $immutables       (ref eq))             ;; Immutables descriptor or #f
                       (field $guard            (ref eq))             ;; Guard procedure or #f
                       (field $constructor-name (ref eq)))))          ;; For error reporting / printing ($Symbol or #f)
          
          (type $Hash   ; abstract super type for hashtables
                (sub $Heap
                     (struct
                       (field $hash     (mut i32))
                       (field $mutable? (ref eq)))))  ; boolean (immediate, i31 tagged)

          (type $HashEq
                (sub $Hash
                     (struct
                       (field $hash     (mut i32))
                       (field $mutable? (ref eq)))))

          (type $HashEqMutable
                ; Mutable hash tables are implemented as an open-addressing hash table
                ; with linear probing.
                (sub $Hash
                     (struct
                       (field $hash     (mut i32))
                       (field $mutable? (ref eq))
                       (field $entries  (mut (ref $Array)))  ;; flat array: key0, val0, key1, val1, ...
                       (field $count    (mut i32)))))        ;; number of key/value pairs currently stored

          (type $VariableReference ; opaque value returned by #%variable-reference
                ; TODO: This is a dummy implmentation in order to get code from the
                ;       expansion of `for`-loops running.
                (sub $Heap
                     (struct
                       (field $hash (mut i32))
                       ; todo: add fields
                       )))
          
          ) ; rec
       


         
         ;; Memory

         (import "env" "memory" (memory $memory 1024))

         
         ;; Imported functions from the host (JavaScript)
         (func $js_output
               (import "primitives" "js_output")
               (param i32))

         ;; Exceptions
         (func $raise-wrong-number-of-values-received (unreachable))
         
         ;; Singletons
         (global $null       (ref eq) ,(Imm '()))
         (global $undefined  (ref eq) ,(Imm (undefined)))
         (global $void       (ref eq) ,(Imm (void)))
         (global $false      (ref eq) ,(Imm #f))  ; (ref.i31 (i32.const ?))
         (global $true       (ref eq) ,(Imm #t))
         (global $error      (ref eq) ,(R 77))
         (global $missing    (ref eq) ,(R missing-value))   ; #x7fffffff
         (global $tombstone  (ref eq) ,(R tombstone-value)) ; #x3fffffff"

         ;; Commonly used fixnums
         (global $zero  (ref eq) ,(Imm 0))
         (global $one   (ref eq) ,(Imm 1))
         (global $two   (ref eq) ,(Imm 2))
         (global $three (ref eq) ,(Imm 3))

         ;; Commonly used flonums
         ;; - initialized in $entry
         (global $flzero  (mut (ref eq)) ,(Undefined))
         (global $flone   (mut (ref eq)) ,(Undefined))
         (global $fltwo   (mut (ref eq)) ,(Undefined))
         (global $flthree (mut (ref eq)) ,(Undefined))

         ;; String constants used in the runtime
         ,@(declare-runtime-string-constants)
         ;; Symbol constants used in the runtime
         ,@(declare-runtime-symbol-constants)
         
         ;; Commonly used realms
         (global $the-racket-realm           (mut (ref eq)) ,(Undefined)) ; the symbol 'racket
         (global $the-racket/primitive-realm (mut (ref eq)) ,(Undefined)) ; the symbol 'racket/primitive

         ;; Primitives (as values)
         ,@(declare-primitives-as-globals)
                                            
         ;; Closure invocation - invoke 
         #;(type $ProcedureInvoker
                 (func (param $proc (ref $Procedure))
                       (param $args (ref $Args))      ; an array of (ref eq)
                       (result (ref eq))))

         (func $raise-arity-mismatch (unreachable))

         (func $invoke-closure
               (type $ProcedureInvoker)
               (param $proc (ref $Procedure))
               (param $args (ref $Args))      ;; array of (ref eq)
               (result      (ref eq))

               (local $clos          (ref $Closure))
               (local $code          (ref $ClosureCode))
               (local $arity-i31     (ref i31))
               (local $arity-i32     i32)
               (local $arg-count     i32)
               (local $args-repacked (ref $Args))

               ;; Step 1: cast proc to closure and extract code
               (local.set $clos (ref.cast (ref $Closure) (local.get $proc)))
               (local.set $code (struct.get $Closure $code (local.get $clos)))
               ;; Step 2: get arity as signed i32
               (local.set $arity-i31
                          (ref.cast (ref i31) (struct.get $Procedure $arity (local.get $clos))))
               (local.set $arity-i32
                          (i32.shr_s (i31.get_s (local.get $arity-i31)) (i32.const 1)))
               ;; Step 3: get argument count
               (local.set $arg-count (array.len (local.get $args)))
               ;; Step 4: check arity match
               (if (i32.eqz
                    (call $procedure-arity-includes?/checked/i32
                          (local.get $clos)
                          (local.get $arg-count)))
                   (then (call $raise-arity-mismatch)))
               ;; Step 5: repack arguments (if variadic)
               (local.set $args-repacked
                          (call $repack-arguments
                                (local.get $args)
                                (local.get $arity-i32)))
               ;; Step 6: invoke
               (return_call_ref $ClosureCode
                                (local.get $clos)
                                (local.get $args-repacked)
                                (local.get $code)))

         (func $invoke-primitive
               (type $ProcedureInvoker)
               (param $proc (ref $Procedure))
               (param $args (ref $Args))      ;; array of (ref eq)
               (result      (ref eq))

               (local $prim          (ref $PrimitiveProcedure))
               (local $arity-i31     (ref i31))
               (local $arity-i32     i32)
               (local $arg-count     i32)
               (local $args-repacked (ref $Args))
               (local $code          (ref $ProcedureInvoker))  ;; function pointer

               ;; Step 1: Cast to $PrimitiveProcedure
               (local.set $prim
                          (ref.cast (ref $PrimitiveProcedure) (local.get $proc)))
               ;; Step 2: Get arity as signed i32 from fixnum
               (local.set $arity-i31
                          (ref.cast (ref i31)
                                    (struct.get $Procedure $arity (local.get $prim))))
               (local.set $arity-i32
                          (i32.shr_s (i31.get_s (local.get $arity-i31)) (i32.const 1)))
               ;; Step 3: Get argument count
               (local.set $arg-count (array.len (local.get $args)))
               ;; Step 4: Arity check
               (if (i32.eqz
                    (call $procedure-arity-includes?/checked/i32
                          (local.get $prim)
                          (local.get $arg-count)))
                   (then (call $raise-arity-mismatch)))
               ;; Step 5: Repack arguments if needed
               (local.set $args-repacked
                          (call $repack-arguments
                                (local.get $args)
                                (local.get $arity-i32)))
               ;; Step 6: Tail-call the primitive invoker
               (local.set $code
                          (struct.get $Procedure $invoke (local.get $prim)))
               (return_call_ref $ProcedureInvoker
                                (local.get $prim)
                                (local.get $args-repacked)
                                (local.get $code)))


         (func $repack-arguments
               ; Returns new $Args suitable for calling both fixed and variadic procedures.
               ; I.e. function converts the rest arguments to a list and stores them in the last slot.
               
               (param $args  (ref $Args))   ;; full argument list
               (param $arity i32)           ;; decoded arity (from fixnum)
               (result       (ref $Args))

               (local $arg-count  i32)
               (local $rest-start i32)
               (local $rest       (ref eq))
               (local $args+rest  (ref $Args))

               ;; Step 1: Compute number of arguments
               (local.set $arg-count (array.len (local.get $args)))
               ;; Step 2: Check if arity is negative (variadic)
               (if (i32.lt_s (local.get $arity) (i32.const 0))
                   (then
                    ;; Step 3: Compute number of fixed args = -1 - arity
                    (local.set $rest-start (i32.sub (i32.const -1) (local.get $arity)))
                    ;; Step 4: Extract rest arguments and turn into list
                    (local.set $rest (call $rest-arguments->list
                                           (local.get $args)
                                           (local.get $rest-start)))
                    ;; Step 5: Create new $Args array with fixed args + 1
                    (local.set $args+rest
                               (array.new $Args
                                          (global.get $false)
                                          (i32.add (local.get $rest-start) (i32.const 1))))
                    ;; Step 6: Copy fixed arguments
                    (array.copy $Args $Args
                                (local.get $args+rest)
                                (i32.const 0)
                                (local.get $args)
                                (i32.const 0)
                                (local.get $rest-start))
                    ;; Step 7: Place rest list at final position
                    (array.set $Args
                               (local.get $args+rest)
                               (local.get $rest-start)
                               (local.get $rest))
                    ;; Step 8: Return modified array
                    (return (local.get $args+rest)))
                   (else 
                    ;; Step 9: Arity is non-negative — return original
                    (return (local.get $args))))
               (unreachable))



         
         (func $invoke-struct
               (type $ProcedureInvoker)
               (param $proc (ref $Procedure)) ; type check: an $Struct is expected
               (param $args (ref $Args))      ; an array of (ref eq)
               (result (ref eq))
               ,(Imm #t))

         (func $raise-arity-error:exactly  (unreachable))
         (func $raise-arity-error:at-least (unreachable))
         
         (func $rest-arguments->list
               (param $args (ref $Args))
               (param $n    i32)
               (result      (ref eq))

               (local $len  i32)
               (local $i    i32)
               (local $x    (ref eq))
               (local $xs   (ref eq))
               ;; Compute length of args
               (local.set $len (array.len (local.get $args)))
               ;; Return empty list if n >= len
               (if (i32.ge_u (local.get $n) (local.get $len))
                   (then (return (global.get $null))))
               ;; Start with empty list
               (local.set $xs (global.get $null))
               ;; Start loop from len - 1 down to n (inclusive)
               (local.set $i (i32.sub (local.get $len) (i32.const 1)))
               (block $done
                      (loop $rev
                            ;; x = args[i]
                            (local.set $x (array.get $Args (local.get $args) (local.get $i)))
                            ;; xs = (cons x xs)
                            (local.set $xs
                                       (struct.new $Pair
                                                   (i32.const 0)        ;; dummy hash
                                                   (local.get $x)       ;; car
                                                   (local.get $xs)))    ;; cdr
                            ;; Stop when i == n
                            (br_if $done (i32.eq (local.get $i) (local.get $n)))
                            ;; i--
                            (local.set $i (i32.sub (local.get $i) (i32.const 1)))
                            (br $rev)))
               (local.get $xs))


         ;; Variable used by `closedapp` to hold the closure during construction.
         (func $dummy-code (type $ClosureCode) (param $clos (ref $Closure))
               (param $args (ref $Args)) (result (ref eq)) ,(Imm 0))
         (global $dummy-closure (ref $Closure)
                 (struct.new $Closure
                             (i32.const 0)               ; hash
                             (global.get $false)         ; name:  #f or $String
                             (global.get $zero)          ; arity: todo
                             (global.get $false)         ; realm: #f or $Symbol
                             (ref.func $invoke-closure) ; invoke (used by apply, map, etc.)
                             (ref.func $dummy-code)
                             (array.new_fixed $Free 0)))
         (global $closedapp-clos (mut (ref $Closure)) (global.get $dummy-closure))

         
         
         
         ;; Return value (for a module)
         (global ,result (mut (ref eq)) ,(Undefined))

         

         ;; Variables defined at the top-level
         ,@(TopVars)

         ;; The symbol table from strings to symbols.
         ;; Used to intern symbols.
         (global $the-symbol-table (mut (ref null $SymbolTable)) (ref.null $SymbolTable))
         ;; The keyword table from strings to keywords.
         ;; Used to intern keywords.
         (global $the-keyword-table (mut (ref null $SymbolTable)) (ref.null $SymbolTable))

         (func $initialize-the-symbol-table
               (if (ref.is_null (global.get $the-symbol-table))
                   (then (global.set $the-symbol-table (call $make-symbol-table)))))

         (func $initialize-the-keyword-table
               (if (ref.is_null (global.get $the-keyword-table))
                   (then (global.set $the-keyword-table (call $make-symbol-table)))))

         
         ;;;
         ;;; Arrays
         ;;;

         ;; Arrays are "vectors" of (ref eq) values.
         ;; They are used internally to implement various Racket data structures.

         ;; Define a mutable array type of (ref eq)
         ; (type $Array (array (mut (ref eq))))

         ;;  make-array : i32 (ref eq) -> $Array
         ;; (make-array size v) -> $Array         
         (func $make-array (param $size i32) (param $v (ref eq)) (result (ref $Array))
               (local $arr (ref $Array))
               (array.new $Array (local.get $v) (local.get $size)))

         ;;  array-length : $Array -> i32
         ;; (array-length arr) -> i32
         ;;   Note: Could just use `array.len` directly.
         (func $array-length (export "array-length")
               (param $arr (ref $Array))
               (result i32)
               (array.len (local.get $arr)))

         ;;  array-ref : $Array i32 -> (ref eq)
         ;; (array-ref arr pos) -> (ref eq)
         ;;   No bounds check.
         (func $array-ref (export "array-ref")
               (param $arr (ref $Array)) (param $pos i32)
               (result (ref eq))
               (array.get $Array (local.get $arr) (local.get $pos)))

         ;;  array-set! : $Array i32 (ref eq) -> 
         ;; (array-set! arr pos v) -> void
         (func $array-set! (export "array-set!")
               (param $arr (ref $Array)) (param $pos i32) (param $v (ref eq))
               (array.set $Array (local.get $arr) (local.get $pos) (local.get $v)))

         ;; array-fill! : $Array (ref eq) -> 
         ;; (array-fill! arr v) -> void
         (func $array-fill! (export "array-fill!")
               (param $arr (ref $Array)) (param $v (ref eq))
               (local $i i32)
               (local.set $i (i32.const 0))
               (block $exit
                      (loop $fill
                            (br_if $exit (i32.ge_u (local.get $i) (array.len (local.get $arr))))
                            (array.set $Array (local.get $arr) (local.get $i) (local.get $v))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $fill))))

         ;;  array-copy! : $Array i32        $Array i32       i32      -> 
         ;; (array-copy!   dest   dest-start src    src-start src-end) -> 
         ;;   Traps on error
         (func $array-copy! (export "array-copy!")
               (param $dest (ref $Array))
               (param $dest-start i32)
               (param $src (ref $Array))
               (param $src-start i32)
               (param $src-end i32)
               (local $i i32)
               (local $src-len i32)
               (local $dest-len i32)
               ;; Bounds checks
               (local.set $src-len (array.len (local.get $src)))
               (local.set $dest-len (array.len (local.get $dest)))
               (if (i32.or
                    (i32.or (i32.lt_u (local.get $src-start) (i32.const 0))
                            (i32.gt_u (local.get $src-end) (local.get $src-len)))
                    (i32.gt_u (i32.add (local.get $dest-start)
                                       (i32.sub (local.get $src-end) (local.get $src-start)))
                              (local.get $dest-len)))
                   (then (unreachable)))
               ;; Copy loop
               (local.set $i (i32.const 0))
               (block $done
                      (loop $copy
                            (br_if $done (i32.ge_u (local.get $i) (i32.sub (local.get $src-end) (local.get $src-start))))
                            (array.set $Array
                                       (local.get $dest)
                                       (i32.add (local.get $dest-start) (local.get $i))
                                       (array.get $Array (local.get $src) (i32.add (local.get $src-start) (local.get $i))))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $copy))))
         
         ;;  array-copy!/error : $Array i32        $Array i32       i32      -> i32
         ;; (array-copy!/error   dest   dest-start src    src-start src-end) -> i32
         ;;   Returns 1 on success, 0 on error.
         (func $array-copy!/error (export "array-copy!/error")
               (param $dest (ref $Array))
               (param $dest-start i32)
               (param $src (ref $Array))
               (param $src-start i32)
               (param $src-end i32)
               (result i32)
               (local $i i32)
               (local $src-len i32)
               (local $dest-len i32)
               ;; Bounds checks
               (local.set $src-len (array.len (local.get $src)))
               (local.set $dest-len (array.len (local.get $dest)))
               (if (i32.or
                    (i32.or (i32.lt_u (local.get $src-start) (i32.const 0))
                            (i32.gt_u (local.get $src-end) (local.get $src-len)))
                    (i32.gt_u (i32.add (local.get $dest-start)
                                       (i32.sub (local.get $src-end) (local.get $src-start)))
                              (local.get $dest-len)))
                   (then (return (i32.const 0))))
               ;; Copy loop
               (local.set $i (i32.const 0))
               (block $done
                      (loop $copy
                            (br_if $done (i32.ge_u (local.get $i) (i32.sub (local.get $src-end) (local.get $src-start))))
                            (array.set $Array
                                       (local.get $dest)
                                       (i32.add (local.get $dest-start) (local.get $i))
                                       (array.get $Array (local.get $src) (i32.add (local.get $src-start) (local.get $i))))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $copy)))
               (i32.const 1))

         ; array-append : $Array $Array -> $Array
         ;  Append the arrays.
         (func $array-append (param $a0 (ref $Array)) (param $a1 (ref $Array)) (result (ref $Array))
               (local $len0  i32)
               (local $len1  i32)
               (local $total i32)
               (local $arr   (ref $Array))
               (local.set $len0  (array.len (local.get $a0)))
               (local.set $len1  (array.len (local.get $a1)))
               (local.set $total (i32.add (local.get $len0) (local.get $len1)))
               (local.set $arr   (call $make-array (local.get $total) (global.get $false)))
               (call $array-copy! (local.get $arr) (i32.const 0)     (local.get $a0) (i32.const 0) (local.get $len0))
               (call $array-copy! (local.get $arr) (local.get $len0) (local.get $a1) (i32.const 0) (local.get $len1))
               (local.get $arr))

         ; array-append-all : (array-of $Array) -> $Array
         ;  Given an array of arrays. Make a new array.
         (func $array-append-all (param $arrs (ref $Array)) (result (ref $Array))
               (local $n     i32)
               (local $i     i32)
               (local $total i32)
               (local $len   i32)
               (local $dst   i32)
               (local $tmp (ref $Array))
               (local $result (ref $Array))

               (local.set $n (array.len (local.get $arrs)))
               (local.set $i (i32.const 0))
               (local.set $total (i32.const 0))
               (block $exit
                      (loop $count
                            (br_if $exit (i32.ge_u (local.get $i) (local.get $n)))
                            (local.set $tmp (ref.cast (ref $Array) (array.get $Array (local.get $arrs) (local.get $i))))
                            (local.set $len (array.len (local.get $tmp)))
                            (local.set $total (i32.add (local.get $total) (local.get $len)))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $count)))
               (local.set $result (call $make-array (local.get $total) (global.get $false)))
               (local.set $i (i32.const 0))
               (local.set $dst (i32.const 0))
               (block $done
                      (loop $copy
                            (br_if $done (i32.ge_u (local.get $i) (local.get $n)))
                            (local.set $tmp (ref.cast (ref $Array) (array.get $Array (local.get $arrs) (local.get $i))))
                            (local.set $len (array.len (local.get $tmp)))
                            (call $array-copy! (local.get $result) (local.get $dst) (local.get $tmp) (i32.const 0) (local.get $len))
                            (local.set $dst (i32.add (local.get $dst) (local.get $len)))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $copy)))
               (local.get $result))

         ; array-take : $Array i32 -> $Array
         (func $array-take (param $arr (ref $Array)) (param $pos i32) (result (ref $Array))
               (call $array-copy (local.get $arr) (i32.const 0) (local.get $pos)))

         ; array-take-right : $Array i32 -> $Array         
         (func $array-take-right (param $arr (ref $Array)) (param $pos i32) (result (ref $Array))
               (local $len i32)
               (local.set $len (array.len (local.get $arr)))
               (call $array-copy (local.get $arr) (i32.sub (local.get $len) (local.get $pos)) (local.get $len)))

         ; array-drop : $Array i32 -> $Array         
         (func $array-drop (param $arr (ref $Array)) (param $pos i32) (result (ref $Array))
               (local $len i32)
               (local.set $len (array.len (local.get $arr)))
               (call $array-copy (local.get $arr) (local.get $pos) (local.get $len)))

         ; array-drop-right : $Array i32 -> $Array         
         (func $array-drop-right (param $arr (ref $Array)) (param $pos i32) (result (ref $Array))
               (local $len i32)
               (local.set $len (array.len (local.get $arr)))
               (call $array-copy (local.get $arr) (i32.const 0) (i32.sub (local.get $len) (local.get $pos))))
         ; array-split-at : $Array i32 -> (array $Array $Array)
         (func $array-split-at (param $arr (ref $Array)) (param $pos i32) (result (ref $Array))
               (local $a   (ref $Array))
               (local $b   (ref $Array))
               (local $res (ref $Array))
               (local.set $a     (call $array-take (local.get $arr) (local.get $pos)))
               (local.set $b     (call $array-drop (local.get $arr) (local.get $pos)))               
               (local.set $res   (call $make-array (i32.const 2) (global.get $false)))
               (call $array-set! (local.get $res) (i32.const 0) (local.get $a))
               (call $array-set! (local.get $res) (i32.const 1) (local.get $b))
               (local.get $res))
         ; $array-copy : $Array i32 i32 -> $Array
         ; (array-copy arr start end)
         (func $array-copy (param $arr (ref $Array)) (param $start i32) (param $end i32) (result (ref $Array))
               (local $res (ref $Array))
               (local.set $res (call $make-array (i32.sub (local.get $end) (local.get $start))
                                                 (global.get $false)))
               (call $array-copy! (local.get $res) (i32.const 0) (local.get $arr) (local.get $start) (local.get $end))
               (local.get $res))

         (func $array-set/copy (param $arr (ref $Array)) (param $pos i32) (param $val (ref eq)) (result (ref $Array))
               (local $len i32)
               (local $res (ref $Array))
               (local.set $len (array.len (local.get $arr)))               
               (local.set $res (call $make-array (local.get $len) (global.get $false)))
               (call $array-copy! (local.get $res) (i32.const 0) (local.get $arr) (i32.const 0) (local.get $len))
               (call $array-set! (local.get $res) (local.get $pos) (local.get $val))
               (local.get $res))

         (func $array-extend (param $arr (ref $Array)) (param $new-size i32) (param $val (ref eq)) (result (ref $Array))
               (local $old-size i32)
               (local $res (ref $Array))
               (local.set $old-size (array.len (local.get $arr)))               
               (local.set $res (call $make-array (local.get $new-size) (local.get $val)))
               (call $array-copy! (local.get $res) (i32.const 0) (local.get $arr) (i32.const 0) (local.get $old-size))
               (local.get $res))

         (func $list->array
               (param $xs   (ref eq))
               (result      (ref $Array))

               (local $len  i32)
               (local $arr  (ref $Array))
               (local $idx  i32)
               (local $x    (ref eq))
               (local $node (ref eq))

               ;; Step 1: compute length of list
               (local.set $len (call $length/i32 (local.get $xs)))
               ;; Step 2: allocate array of given length
               (local.set $arr (call $make-array (local.get $len) (global.get $null)))
               ;; Step 3: initialize traversal variables
               (local.set $node (local.get $xs))
               (local.set $idx  (i32.const 0))
               ;; Step 4: fill array
               (block $done
                      (loop $fill
                            ;; Stop at null
                            (br_if $done (ref.eq (local.get $node) (global.get $null)))
                            ;; Check that it's a pair
                            (if (ref.test (ref $Pair) (local.get $node))
                                (then
                                 (local.set $x (struct.get $Pair $a (ref.cast (ref $Pair) (local.get $node))))
                                 (call $array-set! (local.get $arr) (local.get $idx) (local.get $x))
                                 (local.set $idx (i32.add (local.get $idx) (i32.const 1)))
                                 (local.set $node (struct.get $Pair $d (ref.cast (ref $Pair) (local.get $node))))
                                 (br $fill))
                                (else
                                 (call $raise-pair-expected (local.get $node))
                                 (unreachable)))))
               ;; Return the filled array
               (local.get $arr))

         ;;;
         ;;; Growable Arrays
         ;;;
         
         ;; A growable array is like an array, but the length can change over time.
         ;; They are modelled over "growable vectors" from `racket/data`.

         (func $make-growable-array (param $cap i32) (result (ref $GrowableArray))
               (local $initial-cap i32)
               (local.set $initial-cap
                          (if (result i32)
                              (i32.eqz (local.get $cap))
                              (then (i32.const 16))
                              (else (local.get $cap))))
               (struct.new $GrowableArray
                           (call $make-array (local.get $initial-cap) (global.get $false))
                           (local.get $initial-cap)
                           (i32.const 0)))

         (func $growable-array?? (param $x (ref eq)) (result i32)
               ; 0 = false, 1 = true
               (ref.test (ref $GrowableArray) (local.get $x)))

         (func $growable-array? (param $x (ref eq)) (result (ref eq))
               (if (result (ref eq)) (ref.test (ref $GrowableArray) (local.get $x))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $growable-array-ref (param $g (ref $GrowableArray)) (param $index i32) (result (ref eq))
               (local $i i32)
               (local.set $i (struct.get $GrowableArray $i (local.get $g)))
               (if (result (ref eq))
                   (i32.lt_u (local.get $index) (local.get $i))
                   (then (array.get $Array (struct.get $GrowableArray $arr (local.get $g)) (local.get $index)))
                   (else (global.get $false))))

         (func $growable-array-ref/default (param $g (ref $GrowableArray)) (param $index i32) (param $default (ref eq)) (result (ref eq))
               (local $i i32)
               (local.set $i (struct.get $GrowableArray $i (local.get $g)))
               (if (result (ref eq))
                   (i32.lt_u (local.get $index) (local.get $i))
                   (then (array.get $Array (struct.get $GrowableArray $arr (local.get $g)) (local.get $index)))
                   (else (local.get $default))))

         (func $growable-array-add! (param $g (ref $GrowableArray)) (param $v (ref eq))
               (local $i i32)
               (local $cap i32)
               (local $arr (ref $Array))
               (local $new-cap i32)
               (local $new-arr (ref $Array))
               (local.set $i   (struct.get $GrowableArray $i   (local.get $g)))
               (local.set $cap (struct.get $GrowableArray $cap (local.get $g)))
               (local.set $arr (struct.get $GrowableArray $arr (local.get $g)))
               (if (i32.eq (local.get $i) (local.get $cap))
                   (then                    
                    (local.set $new-cap (i32.shl (local.get $cap) (i32.const 1))) ;; new-cap = cap * 2
                    (local.set $new-arr (call $array-extend (local.get $arr) (local.get $new-cap) (global.get $false)))
                    (struct.set $GrowableArray $arr (local.get $g) (local.get $new-arr))
                    (struct.set $GrowableArray $cap (local.get $g) (local.get $new-cap))
                    (local.set $arr (local.get $new-arr))))
               (array.set $Array (local.get $arr) (local.get $i) (local.get $v))
               (struct.set $GrowableArray $i (local.get $g) (i32.add (local.get $i) (i32.const 1))))

         (func $growable-array-insert! (param $g (ref $GrowableArray)) (param $index i32) (param $value (ref eq))
               (local $i i32)
               (local $arr (ref $Array))
               (local.set $i (struct.get $GrowableArray $i (local.get $g)))
               (if (i32.eq (local.get $index) (local.get $i))
                   (then (call $growable-array-add! (local.get $g) (local.get $value)))
                   (else
                    (local.set $arr (struct.get $GrowableArray $arr (local.get $g)))
                    (call $growable-array-add! (local.get $g) (global.get $false))
                    (call $array-copy! ; dest dest-start src src-start src-end
                          (local.get $arr) (i32.add (local.get $index) (i32.const 1))
                          (local.get $arr) (local.get $index) (local.get $i))
                    (array.set $Array (local.get $arr) (local.get $index) (local.get $value)))))

         (func $growable-array-set! (param $g (ref $GrowableArray)) (param $index i32) (param $value (ref eq))
               (local $i i32)
               (local.set $i (struct.get $GrowableArray $i (local.get $g)))
               (if (i32.eq (local.get $index) (local.get $i))
                   (then (call $growable-array-add! (local.get $g) (local.get $value)))
                   (else
                    (array.set $Array (struct.get $GrowableArray $arr (local.get $g)) (local.get $index) (local.get $value)))))

         (func $growable-array-remove! (param $g (ref $GrowableArray)) (param $index i32)
               (local $i i32)
               (local.set $i (struct.get $GrowableArray $i (local.get $g)))
               (call $array-copy!
                     (struct.get $GrowableArray $arr (local.get $g)) (local.get $index)
                     (struct.get $GrowableArray $arr (local.get $g)) (i32.add (local.get $index) (i32.const 1)) (local.get $i))
               (struct.set $GrowableArray $i (local.get $g) (i32.sub (local.get $i) (i32.const 1))))

         (func $growable-array-remove-last! (param $g (ref $GrowableArray)) (result (ref eq))
               (local $i i32)
               (local.set $i (struct.get $GrowableArray $i (local.get $g)))
               (struct.set $GrowableArray $i (local.get $g) (i32.sub (local.get $i) (i32.const 1)))
               (array.get $Array (struct.get $GrowableArray $arr (local.get $g)) (i32.sub (local.get $i) (i32.const 1))))

         (func $growable-array-count (param $g (ref $GrowableArray)) (result i32)
               (struct.get $GrowableArray $i (local.get $g)))

         (func $growable-array->array (param $g (ref $GrowableArray)) (result (ref $Array))
               (local $count i32)
               (local.set $count (struct.get $GrowableArray $i (local.get $g)))
               (call $array-copy (struct.get $GrowableArray $arr (local.get $g)) (i32.const 0) (local.get $count)))

         (func $array->growable-array (param $a (ref $Array)) (result (ref $GrowableArray))
               ; Note: This wraps the array in a growable array. It does not make a copy.
               (local $n i32)
               (local.set $n (array.len (local.get $a)))
               (struct.new $GrowableArray (local.get $a) (local.get $n) (local.get $n)))

         


         ;;;
         ;;; Growable Arrays of Bytes
         ;;;

         (func $make-growable-bytes (param $capacity i32) (result (ref $GrowableBytes))
               (struct.new $GrowableBytes
                           (call $i8make-array (local.get $capacity) (i32.const 0))
                           (local.get $capacity)
                           (i32.const 0)))

         (func $growable-bytes-add! (param $g (ref $GrowableBytes)) (param $b i32)
               (local $i i32)
               (local $cap i32)
               (local $arr (ref $I8Array))

               (local.set $i   (struct.get $GrowableBytes $i   (local.get $g)))
               (local.set $cap (struct.get $GrowableBytes $cap (local.get $g)))
               (local.set $arr (struct.get $GrowableBytes $arr (local.get $g)))

               ;; Grow if necessary
               (if (i32.eq (local.get $i) (local.get $cap))
                   (then
                    (local.set $cap (i32.shl (local.get $cap) (i32.const 1))) ; double
                    (local.set $arr (call $i8array-extend (local.get $arr) (local.get $cap) (i32.const 0)))
                    (struct.set $GrowableBytes $arr (local.get $g) (local.get $arr))
                    (struct.set $GrowableBytes $cap (local.get $g) (local.get $cap))))

               ;; Set byte
               (array.set  $I8Array (local.get $arr) (local.get $i) (local.get $b))
               (struct.set $GrowableBytes $i (local.get $g) (i32.add (local.get $i) (i32.const 1))))

         (func $growable-bytes->bytes (param $g (ref $GrowableBytes)) (result (ref $Bytes))
               (local $src (ref $I8Array))
               (local $n   i32)
               (local $dst (ref $I8Array))

               ;; Extract fields
               (local.set $src (struct.get $GrowableBytes $arr (local.get $g)))
               (local.set $n   (struct.get $GrowableBytes $i   (local.get $g)))
               ;; Allocate new array of length $n
               (local.set $dst (array.new_default $I8Array (local.get $n)))
               ;; Copy from $src to $dst using array.copy
               (array.copy $I8Array $I8Array
                           (local.get $dst)  ;; dst array
                           (i32.const 0)     ;; dst offset
                           (local.get $src)  ;; src array
                           (i32.const 0)     ;; src offset
                           (local.get $n))   ;; length
               ;; Construct and return new Bytes struct
               (struct.new $Bytes
                           (i32.const 0)       ;; hash = 0
                           (local.get $n)      ;; length
                           (local.get $dst)))  ;; copied byte array

         ;;;
         ;;; Growable Arrays of I32
         ;;;
         
         #;(type $I32Array (array (mut i32)))

         #;(type $I32GrowableArray
               (struct
                 (field $arr (mut (ref $I32Array))) ;; underlying array
                 (field $cap (mut i32))             ;; capacity
                 (field $i   (mut i32))))           ;; current size

         (func $make-i32growable-array (param $cap i32) (result (ref $I32GrowableArray))
               (local $initial-cap i32)
               (local.set $initial-cap (if (result i32) (i32.eqz (local.get $cap))
                                           (then (i32.const 16))
                                           (else (local.get $cap))))
               (struct.new $I32GrowableArray
                           (array.new_default $I32Array (local.get $initial-cap))
                           (local.get $initial-cap)
                           (i32.const 0)))

        (func $i32growable-array-add! (param $g (ref $I32GrowableArray)) (param $v i32)
               (local $i       i32)
               (local $cap     i32)
               (local $arr     (ref $I32Array))               
               (local $new-cap i32)
               (local $new-arr (ref $I32Array))
               
               (local.set $i   (struct.get $I32GrowableArray $i   (local.get $g)))
               (local.set $cap (struct.get $I32GrowableArray $cap (local.get $g)))
               (local.set $arr (struct.get $I32GrowableArray $arr (local.get $g)))

               (if (i32.eq (local.get $i) (local.get $cap))
                   (then
                    (local.set $new-cap (i32.shl (local.get $cap) (i32.const 1)))
                    (local.set $new-arr (call $i32array-extend
                                              (local.get $arr) (local.get $new-cap) (i32.const 0)))
                    (struct.set $I32GrowableArray $arr (local.get $g) (local.get $new-arr))
                    (struct.set $I32GrowableArray $cap (local.get $g) (local.get $new-cap))
                    (local.set $arr (local.get $new-arr))))
               (array.set $I32Array (local.get $arr) (local.get $i) (local.get $v))
               (struct.set $I32GrowableArray $i (local.get $g) (i32.add (local.get $i) (i32.const 1))))

         (func $i32growable-array-ref (param $g (ref $I32GrowableArray)) (param $index i32) (result i32)
               (local $i i32)
               (local.set $i (struct.get $I32GrowableArray $i (local.get $g)))
               (if (result i32)
                   (i32.lt_u (local.get $index) (local.get $i))
                   (then (array.get $I32Array (struct.get $I32GrowableArray $arr (local.get $g)) (local.get $index)))
                   (else (i32.const 0)))) ;; You might want to raise instead

         (func $i32growable-array-count (param $g (ref $I32GrowableArray)) (result i32)
               (struct.get $I32GrowableArray $i (local.get $g)))

         (func $i32growable-array->array (param $g (ref $I32GrowableArray)) (result (ref $I32Array))
               (local $count i32)
               (local.set $count (struct.get $I32GrowableArray $i (local.get $g)))
               (call $i32array-copy (struct.get $I32GrowableArray $arr (local.get $g)) (i32.const 0) (local.get $count)))

         (func $i32array->growable-array (param $a (ref $I32Array)) (result (ref $I32GrowableArray))
               (local $n i32)
               (local.set $n (array.len (local.get $a)))
               (struct.new $I32GrowableArray (local.get $a) (local.get $n) (local.get $n)))

         
         ;;;
         ;;; I32Array - Arrays of i32
         ;;;

         ; (type $I32Array (array (mut i32)))

         (func $i32array-make (param $size i32) (param $v i32) (result (ref $I32Array))
               (array.new $I32Array (local.get $v) (local.get $size)))

         (func $i32array-length (param $arr (ref $I32Array)) (result i32)
               (array.len (local.get $arr)))

         (func $i32array-ref (param $arr (ref $I32Array)) (param $pos i32) (result i32)
               (array.get $I32Array (local.get $arr) (local.get $pos)))

         (func $i32array-set! (param $arr (ref $I32Array)) (param $pos i32) (param $v i32)
               (array.set $I32Array (local.get $arr) (local.get $pos) (local.get $v)))

         (func $i32array-equal?
               (param $a (ref $I32Array)) (param $b (ref $I32Array))
               (result i32)

               (local $len i32)
               (local $i   i32)
               ;; Compare lengths
               (if (i32.ne (array.len (local.get $a))
                           (array.len (local.get $b)))
                   (then (return (i32.const 0))))
               ;; Set up loop
               (local.set $len (array.len (local.get $a)))
               (local.set $i (i32.const 0))
               (block $exit
                      (loop $loop
                            (br_if $exit (i32.ge_u (local.get $i) (local.get $len)))
                            (if (i32.ne
                                 (array.get $I32Array (local.get $a) (local.get $i))
                                 (array.get $I32Array (local.get $b) (local.get $i)))
                                (then (return (i32.const 0))))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $loop)))
               (i32.const 1))

         (func $i32array-fill! (param $arr (ref $I32Array)) (param $v i32)
               (local $i i32)
               (local.set $i (i32.const 0))
               (block $exit
                      (loop $fill
                            (br_if $exit (i32.ge_u (local.get $i) (array.len (local.get $arr))))
                            (array.set $I32Array (local.get $arr) (local.get $i) (local.get $v))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $fill))))

         (func $i32array-copy!
               (param $dest       (ref $I32Array))
               (param $dest-start i32)
               (param $src        (ref $I32Array))
               (param $src-start  i32)
               (param $src-end    i32)
               (local $i          i32)
               
               (local.set $i (i32.const 0))
               (block $done
                      (loop $copy
                            (br_if $done (i32.ge_u (local.get $i) (i32.sub (local.get $src-end) (local.get $src-start))))
                            (array.set $I32Array
                                       (local.get $dest)
                                       (i32.add (local.get $dest-start) (local.get $i))
                                       (array.get $I32Array (local.get $src) (i32.add (local.get $src-start) (local.get $i))))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $copy))))

         (func $i32array-copy!/error (param $dest (ref $I32Array)) (param $dest-start i32)
               (param $src (ref $I32Array)) (param $src-start i32) (param $src-end i32)
               (result i32)
               (local $i i32)
               (local $src-len i32)
               (local $dest-len i32)
               (local.set $src-len (array.len (local.get $src)))
               (local.set $dest-len (array.len (local.get $dest)))
               (if (i32.or
                    (i32.or (i32.lt_u (local.get $src-start) (i32.const 0))
                            (i32.gt_u (local.get $src-end) (local.get $src-len)))
                    (i32.gt_u (i32.add (local.get $dest-start) (i32.sub (local.get $src-end) (local.get $src-start)))
                              (local.get $dest-len)))
                   (then (return (i32.const 0))))
               (local.set $i (i32.const 0))
               (block $done
                      (loop $copy
                            (br_if $done (i32.ge_u (local.get $i) (i32.sub (local.get $src-end) (local.get $src-start))))
                            (array.set $I32Array
                                       (local.get $dest)
                                       (i32.add (local.get $dest-start) (local.get $i))
                                       (array.get $I32Array (local.get $src) (i32.add (local.get $src-start) (local.get $i))))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $copy)))
               (i32.const 1))

         (func $i32array-copy (param $arr (ref $I32Array)) (param $start i32) (param $end i32) (result (ref $I32Array))
               (local $res (ref $I32Array))
               (local.set $res (call $i32array-make (i32.sub (local.get $end) (local.get $start)) (i32.const 0)))
               (call $i32array-copy! (local.get $res) (i32.const 0) (local.get $arr) (local.get $start) (local.get $end))
               (local.get $res))

         (func $i32array-append (param $a0 (ref $I32Array)) (param $a1 (ref $I32Array)) (result (ref $I32Array))
               (local $len0 i32)
               (local $len1 i32)
               (local $total i32)
               (local $arr (ref $I32Array))
               (local.set $len0 (array.len (local.get $a0)))
               (local.set $len1 (array.len (local.get $a1)))
               (local.set $total (i32.add (local.get $len0) (local.get $len1)))
               (local.set $arr (call $i32array-make (local.get $total) (i32.const 0)))
               (call $i32array-copy! (local.get $arr) (i32.const 0) (local.get $a0) (i32.const 0) (local.get $len0))
               (call $i32array-copy! (local.get $arr) (local.get $len0) (local.get $a1) (i32.const 0) (local.get $len1))
               (local.get $arr))

         (func $i32array-take (param $arr (ref $I32Array)) (param $pos i32) (result (ref $I32Array))
               (call $i32array-copy (local.get $arr) (i32.const 0) (local.get $pos)))

         (func $i32array-take-right (param $arr (ref $I32Array)) (param $pos i32) (result (ref $I32Array))
               (local $len i32)
               (local.set $len (array.len (local.get $arr)))
               (call $i32array-copy (local.get $arr) (i32.sub (local.get $len) (local.get $pos)) (local.get $len)))

         (func $i32array-drop (param $arr (ref $I32Array)) (param $pos i32) (result (ref $I32Array))
               (local $len i32)
               (local.set $len (array.len (local.get $arr)))
               (call $i32array-copy (local.get $arr) (local.get $pos) (local.get $len)))

         (func $i32array-drop-right (param $arr (ref $I32Array)) (param $pos i32) (result (ref $I32Array))
               (local $len i32)
               (local.set $len (array.len (local.get $arr)))
               (call $i32array-copy (local.get $arr) (i32.const 0) (i32.sub (local.get $len) (local.get $pos))))

         (func $i32array-set/copy (param $arr (ref $I32Array)) (param $pos i32) (param $val i32) (result (ref $I32Array))
               (local $len i32)
               (local $res (ref $I32Array))
               (local.set $len (array.len (local.get $arr)))
               (local.set $res (call $i32array-make (local.get $len) (i32.const 0)))
               (call $i32array-copy! (local.get $res) (i32.const 0) (local.get $arr) (i32.const 0) (local.get $len))
               (call $i32array-set! (local.get $res) (local.get $pos) (local.get $val))
               (local.get $res))

         (func $i32array-extend (param $arr (ref $I32Array)) (param $new-size i32) (param $val i32) (result (ref $I32Array))
               (local $old-size i32)
               (local $res (ref $I32Array))
               (local.set $old-size (array.len (local.get $arr)))
               (local.set $res (call $i32array-make (local.get $new-size) (local.get $val)))
               (call $i32array-copy! (local.get $res) (i32.const 0) (local.get $arr) (i32.const 0) (local.get $old-size))
               (local.get $res))

         (func $list->i32array (param $xs (ref eq)) (result (ref $I32Array))
               (local $len i32)
               (local $arr  (ref $I32Array))
               (local $idx  i32)
               (local $x    (ref eq))
               (local $v    i32)
               (local $node (ref eq))
               ;; Step 1: compute length of list
               (local.set $len (call $length/i32 (local.get $xs)))
               ;; Step 2: allocate array of given length
               (local.set $arr (call $i32array-make (local.get $len) (i32.const 0)))
               ;; Step 3: initialize traversal variables
               (local.set $node (local.get $xs))
               (local.set $idx  (i32.const 0))
               ;; Step 4: fill array
               (block $done
                      (loop $fill
                            ;; Stop at null
                            (br_if $done (ref.eq (local.get $node) (global.get $null)))
                            ;; Check that it's a pair
                            (if (ref.test (ref $Pair) (local.get $node))
                                (then
                                 ;; Get the car
                                 (local.set $x (struct.get $Pair $a (ref.cast (ref $Pair) (local.get $node))))
                                 ;; Check that car is a fixnum (i31 with LSB 0)
                                 (if (ref.test (ref i31) (local.get $x))
                                     (then
                                      (local.set $v (i31.get_u (ref.cast (ref i31) (local.get $x))))
                                      (if (i32.eqz (i32.and (local.get $v) (i32.const 1)))
                                          (then
                                           ;; Decode fixnum: shift right 1 and store
                                           (call $i32array-set! (local.get $arr)
                                                 (local.get $idx)
                                                 (i32.shr_u (local.get $v) (i32.const 1)))
                                           ;; Advance
                                           (local.set $idx  (i32.add (local.get $idx) (i32.const 1)))
                                           (local.set $node (struct.get $Pair $d (ref.cast (ref $Pair) (local.get $node))))
                                           (br $fill))
                                          (else (call $raise-check-fixnum (local.get $x))
                                                (unreachable))))
                                     (else (call $raise-check-fixnum (local.get $x))
                                           (unreachable))))
                                (else (call $raise-pair-expected (local.get $node))
                                      (unreachable)))))
               ;; Return the filled array
               (local.get $arr))


         ;;;
         ;;; I8Array
         ;;;

         ; (type $I8Array (array (mut i8)))

         (func $raise-bad-byte (param $x (ref eq)) (unreachable))
         
         (func $i8make-array (param $size i32) (param $v i32) (result (ref $I8Array))
               (array.new $I8Array (local.get $v) (local.get $size)))

         (func $i8array-length (export "i8array-length")
               (param $arr (ref $I8Array))
               (result i32)
               (array.len (local.get $arr)))

         (func $i8array-ref (export "i8array-ref")
               (param $arr (ref $I8Array)) (param $pos i32)
               (result i32)
               (array.get_u $I8Array (local.get $arr) (local.get $pos)))

         (func $i8array-set! (export "i8array-set!")
               (param $arr (ref $I8Array)) (param $pos i32) (param $v i32)
               (array.set $I8Array (local.get $arr) (local.get $pos) (local.get $v)))

         (func $i8array-fill! (export "i8array-fill!")
               (param $arr (ref $I8Array)) (param $v i32)
               (local $i i32)
               (local.set $i (i32.const 0))
               (block $exit
                      (loop $fill
                            (br_if $exit (i32.ge_u (local.get $i) (array.len (local.get $arr))))
                            (array.set $I8Array (local.get $arr) (local.get $i) (local.get $v))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $fill))))

         (func $i8array-copy! (export "i8array-copy!")
               (param $dest (ref $I8Array))
               (param $dest-start i32)
               (param $src (ref $I8Array))
               (param $src-start i32)
               (param $src-end i32)
               (local $i i32)
               (local $src-len i32)
               (local $dest-len i32)
               (local.set $src-len (array.len (local.get $src)))
               (local.set $dest-len (array.len (local.get $dest)))
               (if (i32.or
                    (i32.or (i32.lt_u (local.get $src-start) (i32.const 0))
                            (i32.gt_u (local.get $src-end) (local.get $src-len)))
                    (i32.gt_u (i32.add (local.get $dest-start)
                                       (i32.sub (local.get $src-end) (local.get $src-start)))
                              (local.get $dest-len)))
                   (then (unreachable)))
               (local.set $i (i32.const 0))
               (block $done
                      (loop $copy
                            (br_if $done (i32.ge_u (local.get $i) (i32.sub (local.get $src-end) (local.get $src-start))))
                            (array.set $I8Array
                                       (local.get $dest)
                                       (i32.add (local.get $dest-start) (local.get $i))
                                       (array.get_u $I8Array (local.get $src) (i32.add (local.get $src-start) (local.get $i))))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $copy))))

         (func $i8array-copy!/error (export "i8array-copy!/error")
               (param $dest (ref $I8Array))
               (param $dest-start i32)
               (param $src (ref $I8Array))
               (param $src-start i32)
               (param $src-end i32)
               (result i32)
               (local $i i32)
               (local $src-len i32)
               (local $dest-len i32)
               (local.set $src-len (array.len (local.get $src)))
               (local.set $dest-len (array.len (local.get $dest)))
               (if (i32.or
                    (i32.or (i32.lt_u (local.get $src-start) (i32.const 0))
                            (i32.gt_u (local.get $src-end) (local.get $src-len)))
                    (i32.gt_u (i32.add (local.get $dest-start)
                                       (i32.sub (local.get $src-end) (local.get $src-start)))
                              (local.get $dest-len)))
                   (then (return (i32.const 0))))
               (local.set $i (i32.const 0))
               (block $done
                      (loop $copy
                            (br_if $done (i32.ge_u (local.get $i) (i32.sub (local.get $src-end) (local.get $src-start))))
                            (array.set $I8Array
                                       (local.get $dest)
                                       (i32.add (local.get $dest-start) (local.get $i))
                                       (array.get_u $I8Array (local.get $src) (i32.add (local.get $src-start) (local.get $i))))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $copy)))
               (i32.const 1))

         (func $i8array-copy (param $arr (ref $I8Array)) (param $start i32) (param $end i32) (result (ref $I8Array))
               (local $res (ref $I8Array))
               (local.set $res (call $i8make-array (i32.sub (local.get $end) (local.get $start)) (i32.const 0)))
               (call $i8array-copy! (local.get $res) (i32.const 0) (local.get $arr) (local.get $start) (local.get $end))
               (local.get $res))

         (func $i8array-set/copy (param $arr (ref $I8Array)) (param $pos i32) (param $val i32) (result (ref $I8Array))
               (local $len i32)
               (local $res (ref $I8Array))
               (local.set $len (array.len (local.get $arr)))
               (local.set $res (call $i8make-array (local.get $len) (i32.const 0)))
               (call $i8array-copy! (local.get $res) (i32.const 0) (local.get $arr) (i32.const 0) (local.get $len))
               (call $i8array-set! (local.get $res) (local.get $pos) (local.get $val))
               (local.get $res))

         (func $i8array-extend (param $arr (ref $I8Array)) (param $new-size i32) (param $val i32) (result (ref $I8Array))
               (local $old-size i32)
               (local $res (ref $I8Array))
               (local.set $old-size (array.len (local.get $arr)))
               (local.set $res (call $i8make-array (local.get $new-size) (local.get $val)))
               (call $i8array-copy! (local.get $res) (i32.const 0) (local.get $arr) (i32.const 0) (local.get $old-size))
               (local.get $res))

         (func $i8array-append
               (param $a0  (ref $I8Array))
               (param $a1  (ref $I8Array))
               (result     (ref $I8Array))
               
               (local $n0  i32)
               (local $n1  i32)
               (local $res (ref $I8Array))

               (local.set $n0  (array.len (local.get $a0)))
               (local.set $n1  (array.len (local.get $a1)))
               (local.set $res (array.new $I8Array (i32.const 0) (i32.add (local.get $n0) (local.get $n1))))
               (call $i8array-copy! (local.get $res) (i32.const 0)   (local.get $a0) (i32.const 0) (local.get $n0))
               (call $i8array-copy! (local.get $res) (local.get $n0) (local.get $a1) (i32.const 0) (local.get $n1))
               (local.get $res))

         (func $list->i8array (param $xs (ref eq)) (result (ref $I8Array))
               (local $len  i32)
               (local $arr  (ref $I8Array))
               (local $idx  i32)
               (local $x    (ref eq))
               (local $v    i32)
               (local $node (ref eq))
               ;; Step 1: compute list length
               (local.set $len (call $length/i32 (local.get $xs)))
               ;; Step 2: allocate array of that length
               (local.set $arr (array.new $I8Array (i32.const 0) (local.get $len)))
               ;; Step 3: initialize index and node
               (local.set $idx  (i32.const 0))
               (local.set $node (local.get $xs))
               ;; Step 4: fill loop
               (block $done
                      (loop $fill
                            ;; If we hit null, stop
                            (br_if $done (ref.eq (local.get $node) (global.get $null)))
                            ;; Must be a pair
                            (if (ref.test (ref $Pair) (local.get $node))
                                (then
                                 ;; Extract the car
                                 (local.set $x (struct.get $Pair $a (ref.cast (ref $Pair) (local.get $node))))
                                 ;; Check that car is a fixnum
                                 (if (ref.test (ref i31) (local.get $x))
                                     (then
                                      (local.set $v (i31.get_u (ref.cast (ref i31) (local.get $x))))
                                      ;; Check fixnum has LSB = 0
                                      (if (i32.eqz (i32.and (local.get $v) (i32.const 1)))
                                          (then
                                           ;; Shift right to get raw i32
                                           (local.set $v (i32.shr_u (local.get $v) (i32.const 1)))
                                           ;; Check range 0–255
                                           (if (i32.le_u (local.get $v) (i32.const 255))
                                               (then
                                                ;; Store into array
                                                (array.set $I8Array (local.get $arr)
                                                           (local.get $idx)
                                                           (local.get $v))
                                                ;; Advance
                                                (local.set $idx  (i32.add (local.get $idx) (i32.const 1)))
                                                (local.set $node (struct.get $Pair $d (ref.cast (ref $Pair) (local.get $node))))
                                                (br $fill))
                                               (else (call $raise-bad-byte (local.get $x))
                                                     (unreachable))))
                                          (else (call $raise-check-fixnum (local.get $x))
                                                (unreachable))))
                                     (else (call $raise-check-fixnum (local.get $x))
                                           (unreachable))))
                                (else (call $raise-pair-expected (local.get $node))
                                      (unreachable)))))
               ;; Return array
               (local.get $arr))

         (func $i8array->bytes (param $arr (ref $I8Array)) (result (ref $Bytes))
               ;; Constructs a mutable $Bytes object (immutable = 0, hash = 0).
               (struct.new $Bytes
                           (i32.const 0)      ;; hash = 0
                           (i32.const 0)      ;; immutable = false
                           (local.get $arr))) ;; the backing I8Array

         (func $i8array->immutable-bytes (param $arr (ref $I8Array)) (result (ref $Bytes))
               ;; Constructs an immutable $Bytes object (immutable = 1, hash = 0).
               (struct.new $Bytes
                           (i32.const 0)      ;; hash = 0
                           (i32.const 1)      ;; immutable = true
                           (local.get $arr))) ;; the backing I8Array

         
         ;;; 
         ;;;  Equality
         ;;;

         (func $eq? (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.eq (local.get $v1) (local.get $v2))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $eqv?
               ; Except for numbers and characters, `eqv?` works like `eq?`.
               (param $v1 (ref eq))
               (param $v2 (ref eq))
               (result    (ref eq))
               
               (local $i1 i32)
               (local $i2 i32)
               (local $f1 (ref $Flonum))
               (local $f2 (ref $Flonum))
               ;; --- Fast path: ref.eq ---
               (if (ref.eq (local.get $v1) (local.get $v2))
                   (then (return (global.get $true))))
               ;; --- Case: both flonums ---
               (if (i32.and (ref.test (ref $Flonum) (local.get $v1))
                            (ref.test (ref $Flonum) (local.get $v2)))
                   (then
                    (local.set $f1 (ref.cast (ref $Flonum) (local.get $v1)))
                    (local.set $f2 (ref.cast (ref $Flonum) (local.get $v2)))
                    (if (f64.eq (struct.get $Flonum $v (local.get $f1))
                                (struct.get $Flonum $v (local.get $f2)))
                        (then (return (global.get $true)))
                        (else (return (global.get $false))))))
               ;; --- Characters ---
               ; With our implementation, we `eqv` is the same as `eq` on characters.
               ;; --- Fallback: ref.eq ---
               (if (ref.eq (local.get $v1) (local.get $v2))
                   (then (return (global.get $true)))
                   (else (return (global.get $false))))
               (unreachable))

         ;;; equal?

         ;; Top-level equal?
         (func $equal?
               (param $v1 (ref eq))
               (param $v2 (ref eq))
               (result    (ref eq))

               ;; Fast path: ref.eq
               (if (ref.eq (local.get $v1) (local.get $v2))
                   (then (return (global.get $true))))
               ;; Fallback to eqv?
               (if (ref.eq (call $eqv? (local.get $v1) (local.get $v2)) (global.get $true))
                   (then (return (global.get $true))))
               ;; Slower recursive structural equality
               (return (call $equal?/slow (local.get $v1) (local.get $v2))))

         ;; Slow structural equality (recursive)
         (func $equal?/slow
               (param $v1 (ref eq))
               (param $v2 (ref eq))
               (result    (ref eq))

               ;; --- Pair ---
               (if (i32.and (ref.test (ref $Pair) (local.get $v1))
                            (ref.test (ref $Pair) (local.get $v2)))
                   (then (return_call $equal?/pair
                                      (ref.cast (ref $Pair) (local.get $v1))
                                      (ref.cast (ref $Pair) (local.get $v2)))))
               ;; --- Box ---
               (if (i32.and (ref.test (ref $Box) (local.get $v1))
                            (ref.test (ref $Box) (local.get $v2)))
                   (then (return_call $equal?/box
                                      (ref.cast (ref $Box) (local.get $v1))
                                      (ref.cast (ref $Box) (local.get $v2)))))
               ;; --- Vector ---
               (if (i32.and (ref.test (ref $Vector) (local.get $v1))
                            (ref.test (ref $Vector) (local.get $v2)))
                   (then (return_call $equal?/vector
                                      (ref.cast (ref $Vector) (local.get $v1))
                                      (ref.cast (ref $Vector) (local.get $v2)))))
               ;; --- String ---
               (if (i32.and (ref.test (ref $String) (local.get $v1))
                            (ref.test (ref $String) (local.get $v2)))
                   (then (return_call $string=?
                                      (ref.cast (ref $String) (local.get $v1))
                                      (ref.cast (ref $String) (local.get $v2)))))
               ;; --- Bytes ---
               (if (i32.and (ref.test (ref $Bytes) (local.get $v1))
                            (ref.test (ref $Bytes) (local.get $v2)))
                   (then (return_call $bytes=?
                                      (ref.cast (ref $Bytes) (local.get $v1))
                                      (ref.cast (ref $Bytes) (local.get $v2)))))
               ;; --- Struct --- (fieldwise comparison)
               (if (i32.and (ref.test (ref $Struct) (local.get $v1))
                            (ref.test (ref $Struct) (local.get $v2)))
                   (then (return_call $equal?/struct
                                      (ref.cast (ref $Struct) (local.get $v1))
                                      (ref.cast (ref $Struct) (local.get $v2)))))
               ;; --- Fallback ---
               (return (global.get $false)))


         ;; Compare pairs
         (func $equal?/pair
               (param $p1 (ref $Pair))
               (param $p2 (ref $Pair))
               (result    (ref eq))
               
               (if (ref.eq (call $equal? (struct.get $Pair $a (local.get $p1))
                                         (struct.get $Pair $a (local.get $p2)))
                           (global.get $true))
                   (then (return_call $equal? (struct.get $Pair $d (local.get $p1))
                                              (struct.get $Pair $d (local.get $p2))))
                   (else (return (global.get $false))))
               (unreachable))


         ;; Compare boxes
         (func $equal?/box
               (param $b1 (ref $Box))
               (param $b2 (ref $Box))
               (result    (ref eq))
               (return_call $equal?
                            (struct.get $Box $v (local.get $b1))
                            (struct.get $Box $v (local.get $b2))))


         ;; Compare vectors
         (func $equal?/vector
               (param $v1 (ref $Vector))
               (param $v2 (ref $Vector))
               (result    (ref eq))
               
               (local $a1  (ref $Array))
               (local $a2  (ref $Array))
               (local $len i32)
               (local $i   i32)
               (local $x1  (ref eq))
               (local $x2  (ref eq))
               
               (local.set $a1  (struct.get $Vector $arr (local.get $v1)))
               (local.set $a2  (struct.get $Vector $arr (local.get $v2)))
               (local.set $len (array.len (local.get $a1)))
               (if (i32.ne (local.get $len) (array.len (local.get $a2)))
                   (then (return (global.get $false))))
               (local.set $i (i32.const 0))
               (block $done
                      (loop $loop
                            (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
                            (local.set $x1 (array.get $Array (local.get $a1) (local.get $i)))
                            (local.set $x2 (array.get $Array (local.get $a2) (local.get $i)))
                            (if (ref.eq (call $equal? (local.get $x1) (local.get $x2)) (global.get $false))
                                (then (return (global.get $false))))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $loop)))
               (return (global.get $true)))


         ;; Compare structs fieldwise
         (func $equal?/struct
               (param $s1 (ref $Struct))
               (param $s2 (ref $Struct))
               (result    (ref eq))

               (local $t1  (ref $StructType))
               (local $t2  (ref $StructType))
               (local $a1  (ref $Array))
               (local $a2  (ref $Array))
               (local $len i32)
               (local $i   i32)
               (local $x1  (ref eq))
               (local $x2  (ref eq))

               (local.set $t1 (struct.get $Struct $type (local.get $s1)))
               (local.set $t2 (struct.get $Struct $type (local.get $s2)))
               (if (ref.eq (local.get $t1) (local.get $t2))
                   (then
                    (local.set $a1  (struct.get $Struct $fields (local.get $s1)))
                    (local.set $a2  (struct.get $Struct $fields (local.get $s2)))
                    (local.set $len (array.len (local.get $a1)))
                    (local.set $i   (i32.const 0))
                    (block $done
                           (loop $loop
                                 (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
                                 (local.set $x1 (array.get $Array (local.get $a1) (local.get $i)))
                                 (local.set $x2 (array.get $Array (local.get $a2) (local.get $i)))
                                 (if (ref.eq (call $equal? (local.get $x1) (local.get $x2)) (global.get $false))
                                     (then (return (global.get $false))))
                                 (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                 (br $loop)))
                    (return (global.get $true))))
               (return (global.get $false)))


         ;;;
         ;;; Numbers
         ;;;

         (func $exact?
               ;; A number is exact if it's a fixnum: a ref i31 with LSB = 0
               (param $z (ref eq))
               (result   (ref eq))

               (local $bits i32)               
               (if (result (ref eq))
                   (ref.test (ref i31) (local.get $z))
                   (then (local.set $bits (i31.get_u (ref.cast (ref i31) (local.get $z))))
                         (if (result (ref eq))
                             (i32.eqz (i32.and (local.get $bits) (i32.const 1)))
                             (then (global.get $true))
                             (else (global.get $false))))
                   (else (global.get $false))))

         (func $exact-integer?
               (param $v (ref eq))
               (result   (ref eq))

               (local $bits i32)
               
               (if (result (ref eq))
                   (ref.test (ref i31) (local.get $v))
                   (then (local.set $bits (i31.get_u (ref.cast (ref i31) (local.get $v))))
                         (if (result (ref eq))
                             (i32.eqz (i32.and (local.get $bits) (i32.const 1)))
                             (then (global.get $true))
                             (else (global.get $false))))
                   (else (global.get $false))))

         (func $exact-nonnegative-integer?
               (param $v (ref eq))
               (result   (ref eq))

               (local $n i32)

               ;; Check: is v a fixnum with LSB = 0
               (if (ref.test (ref i31) (local.get $v))
                   (then
                    (local.set $n (i31.get_s (ref.cast (ref i31) (local.get $v))))
                    (if (i32.eqz (i32.and (local.get $n) (i32.const 1))) ;; LSB = 0?
                        (then
                         (if (i32.ge_s (i32.shr_s (local.get $n) (i32.const 1))
                                       (i32.const 0))
                             (then (return (global.get $true)))
                             (else (return (global.get $false))))))
                    (return (global.get $false))))

               ;; Not a fixnum
               (return (global.get $false)))

         (func $exact-positive-integer?
               (param $v (ref eq))
               (result   (ref eq))

               (local $n i32)

               ;; Check if it's a fixnum
               (if (ref.test (ref i31) (local.get $v))
                   (then
                    ;; Extract the signed value
                    (local.set $n (i31.get_s (ref.cast (ref i31) (local.get $v))))
                    ;; Check LSB = 0 (exact) and value > 0
                    (if (i32.eqz (i32.and (local.get $n) (i32.const 1)))
                        (then
                         (if (i32.gt_s (i32.shr_s (local.get $n) (i32.const 1)) (i32.const 0))
                             (then (return (global.get $true)))
                             (else (return (global.get $false))))))
                    ;; LSB ≠ 0 → not an exact integer
                    (return (global.get $false))))
               ;; Not a fixnum → not an exact integer
               (return (global.get $false)))



         
         (func $zero?
               (param $x (ref eq))
               (result   (ref eq))

               (local $x/fx i32)
               (local $x/fl (ref $Flonum))
               
               ;; If x is a fixnum, check if it's 0
               (if (ref.test (ref i31) (local.get $x))
                   (then
                    (local.set $x/fx (i31.get_u (ref.cast (ref i31) (local.get $x))))
                    (if (i32.eqz (i32.and (local.get $x/fx) (i32.const 1))) ;; lsb = 0?
                        (then
                         (if (i32.eqz (i32.shr_u (local.get $x/fx) (i32.const 1))) ;; value == 0?
                             (then (return (global.get $true)))
                             (else (return (global.get $false)))))
                        (else (call $raise-expected-number (local.get $x)) (unreachable)))))
               ;; If x is a flonum, check if it's 0.0
               (if (ref.test (ref $Flonum) (local.get $x))
                   (then
                    (local.set $x/fl (ref.cast (ref $Flonum) (local.get $x)))
                    (if (f64.eq (struct.get $Flonum $v (local.get $x/fl)) (f64.const 0.0))
                        (then (return (global.get $true)))
                        (else (return (global.get $false))))))
               ;; Not a number
               (call $raise-expected-number (local.get $x))
               (unreachable))

         (func $positive?
               (param $x (ref eq))
               (result   (ref eq))

               (local $x/fx i32)
               (local $x/fl (ref $Flonum))

               ;; If x is a fixnum, check if it's > 0
               (if (ref.test (ref i31) (local.get $x))
                   (then
                    (local.set $x/fx (i31.get_u (ref.cast (ref i31) (local.get $x))))
                    (if (i32.eqz (i32.and (local.get $x/fx) (i32.const 1))) ;; lsb = 0?
                        (then
                         (if (i32.gt_s (i32.shr_u (local.get $x/fx) (i32.const 1)) (i32.const 0))
                             (then (return (global.get $true)))
                             (else (return (global.get $false)))))
                        (else (call $raise-expected-number (local.get $x)) (unreachable)))))

               ;; If x is a flonum, check if it's > 0.0
               (if (ref.test (ref $Flonum) (local.get $x))
                   (then
                    (local.set $x/fl (ref.cast (ref $Flonum) (local.get $x)))
                    (if (f64.gt (struct.get $Flonum $v (local.get $x/fl)) (f64.const 0.0))
                        (then (return (global.get $true)))
                        (else (return (global.get $false))))))

               ;; Not a number
               (call $raise-expected-number (local.get $x))
               (unreachable))

         (func $negative?
               (param $x (ref eq))
               (result (ref eq))

               (local $x/fx i32)
               (local $x/fl (ref $Flonum))

               ;; If x is a fixnum, check if it's < 0
               (if (ref.test (ref i31) (local.get $x))
                   (then
                    (local.set $x/fx (i31.get_u (ref.cast (ref i31) (local.get $x))))
                    (if (i32.eqz (i32.and (local.get $x/fx) (i32.const 1))) ;; lsb = 0?
                        (then
                         (if (i32.lt_s (i32.shr_u (local.get $x/fx) (i32.const 1)) (i32.const 0))
                             (then (return (global.get $true)))
                             (else (return (global.get $false)))))
                        (else (call $raise-expected-number (local.get $x)) (unreachable)))))

               ;; If x is a flonum, check if it's < 0.0
               (if (ref.test (ref $Flonum) (local.get $x))
                   (then
                    (local.set $x/fl (ref.cast (ref $Flonum) (local.get $x)))
                    (if (f64.lt (struct.get $Flonum $v (local.get $x/fl)) (f64.const 0.0))
                        (then (return (global.get $true)))
                        (else (return (global.get $false))))))

               ;; Not a number
               (call $raise-expected-number (local.get $x))
               (unreachable))




         (func $integer?
               (param $v (ref eq))
               (result   (ref eq))

               (local $v/fx i32)
               (local $v/fl (ref $Flonum))
               (local $f64v f64)

               ;; Case 1: fixnum
               (if (ref.test (ref i31) (local.get $v))
                   (then
                    (local.set $v/fx (i31.get_u (ref.cast (ref i31) (local.get $v))))
                    (if (i32.eqz (i32.and (local.get $v/fx) (i32.const 1)))
                        (then (return (global.get $true)))
                        (else (return (global.get $false))))))
               ;; Case 2: flonum
               (if (ref.test (ref $Flonum) (local.get $v))
                   (then
                    (local.set $v/fl (ref.cast (ref $Flonum) (local.get $v)))
                    (local.set $f64v (struct.get $Flonum $v (local.get $v/fl)))
                    ;; Check: finite && round
                    (if (f64.ne (local.get $f64v) (local.get $f64v)) ;; NaN
                        (then (return (global.get $false))))
                    (if (f64.eq (local.get $f64v) (f64.const +inf)) ;; +inf.0
                        (then (return (global.get $false))))
                    (if (f64.eq (local.get $f64v) (f64.const -inf)) ;; -inf.0
                        (then (return (global.get $false))))
                    (if (f64.eq (f64.floor (local.get $f64v)) (local.get $f64v))
                        (then (return (global.get $true)))
                        (else (return (global.get $false))))))
               ;; Not a number
               (return (global.get $false)))

         (func $add1
               (param $v (ref eq))
               (result   (ref eq))

               (local $v/fx i32)
               (local $sum  i32)
               (local $v/fl (ref $Flonum))
               (local $f64v f64)

               ;; Case 1: Fixnum
               (if (ref.test (ref i31) (local.get $v))
                   (then (local.set $v/fx (i31.get_u (ref.cast (ref i31) (local.get $v))))
                         (if (i32.eqz (i32.and (local.get $v/fx) (i32.const 1)))
                             (then (local.set $sum (i32.add (local.get $v/fx) (i32.const 2))) ;; add1 on unshifted
                                   (return (ref.i31 (local.get $sum)))))))
               ;; Case 2: Flonum
               (if (ref.test (ref $Flonum) (local.get $v))
                   (then (local.set $v/fl (ref.cast (ref $Flonum) (local.get $v)))
                         (local.set $f64v (struct.get $Flonum $v (local.get $v/fl)))
                         (return (struct.new $Flonum
                                             (i32.const 0)
                                             (f64.add (local.get $f64v) (f64.const 1.0))))))
               ;; Not a number
               (call $raise-expected-number (local.get $v))
               (unreachable))

         (func $sub1
               (param $v (ref eq))
               (result   (ref eq))

               (local $v/fx i32)
               (local $diff i32)
               (local $v/fl (ref $Flonum))
               (local $f64v f64)
               ;; Case 1: Fixnum
               (if (ref.test (ref i31) (local.get $v))
                   (then (local.set $v/fx (i31.get_u (ref.cast (ref i31) (local.get $v))))
                         (if (i32.eqz (i32.and (local.get $v/fx) (i32.const 1)))
                             (then (local.set $diff (i32.sub (local.get $v/fx) (i32.const 2))) ;; sub1 = -1 << 1
                                   (return (ref.i31 (local.get $diff)))))))
               ;; Case 2: Flonum
               (if (ref.test (ref $Flonum) (local.get $v))
                   (then (local.set $v/fl (ref.cast (ref $Flonum) (local.get $v)))
                         (local.set $f64v (struct.get $Flonum $v (local.get $v/fl)))
                         (return
                          (struct.new $Flonum
                                      (i32.const 0)
                                      (f64.sub (local.get $f64v) (f64.const 1.0))))))
               ;; Not a number
               (call $raise-expected-number (local.get $v))
               (unreachable))


         (func $raise-expected-number (unreachable))

         ,@(let ()
             (define (binop $+ $fx+ $fl+)
               `(func ,$+ (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
                      (if (result (ref eq)) (call $fx?/i32 (local.get $x))
                          (then (if (result (ref eq)) (call $fx?/i32 (local.get $y))
                                    (then (call ,$fx+
                                                (local.get $x) (local.get $y)))
                                    (else (if (result (ref eq)) (call $fl?/i32 (local.get $y))
                                              (then (call ,$fl+
                                                          (call $fx->fl (local.get $x)) (local.get $y)))
                                              (else (call $raise-expected-number)
                                                    (unreachable))))))
                          (else (if (result (ref eq)) (call $fl?/i32 (local.get $x))
                                    (then (if (result (ref eq)) (call $fl?/i32 (local.get $y))
                                              (then (call ,$fl+
                                                          (local.get $x) (local.get $y)))
                                              (else (if (result (ref eq)) (call $fx?/i32 (local.get $y))
                                                        (then (call ,$fl+
                                                                    (local.get $x) (call $fx->fl (local.get $y))))
                                                        (else (call $raise-expected-number)
                                                              (unreachable))))))
                                    (else (call $raise-expected-number)
                                          (unreachable)))))))
             (list (binop '$+ '$fx+ '$fl+)
                   (binop '$- '$fx- '$fl-)
                   (binop '$* '$fx* '$fl*)))

         ;; Note: fx/ doesn't exist, but fxquotient do.

         (func $/
               (param $x (ref eq))
               (param $y (ref eq))
               (result   (ref eq))

               (local $x/fl (ref $Flonum))
               (local $y/fl (ref $Flonum))
               ;; --- Check that $x is a number ---
               ; Note: $x is not a number if: it is not a fixnum and not a flonum
               (if (i32.and
                    (i32.eqz (ref.test (ref i31) (local.get $x)))
                    (i32.eqz (ref.test (ref $Flonum) (local.get $x))))
                   (then (call $raise-expected-number (local.get $x)) (unreachable)))
               ;; --- Check that $y is a number ---
               (if (i32.and
                    (i32.eqz (ref.test (ref i31) (local.get $y)))
                    (i32.eqz (ref.test (ref $Flonum) (local.get $y))))
                   (then (call $raise-expected-number (local.get $y)) (unreachable)))
               ;; --- Convert $x to flonum if needed ---
               (local.set $x/fl
                          (if (result (ref $Flonum))
                              (ref.test (ref $Flonum) (local.get $x))
                              (then (ref.cast (ref $Flonum) (local.get $x)))
                              (else (call $fx->fl (local.get $x)))))
               ;; --- Convert $y to flonum if needed ---
               (local.set $y/fl
                          (if (result (ref $Flonum))
                              (ref.test (ref $Flonum) (local.get $y))
                              (then (ref.cast (ref $Flonum) (local.get $y)))
                              (else (call $fx->fl (local.get $y)))))
               ;; --- Divide using $fl/ ---
               (call $fl/ (local.get $x/fl) (local.get $y/fl)))

         ,@(let ()
             (define (gencmp cmp fxcmp flcmp)
               `(func ,cmp
                      (param $x (ref eq))
                      (param $y (ref eq))
                      (result   (ref eq))

                      (local $x/is-fx i32)
                      (local $y/is-fx i32)
                      (local $x/is-fl i32)
                      (local $y/is-fl i32)
                      (local $x-fx    i32)
                      (local $y-fx    i32)
                      (local $x-fl    (ref null $Flonum))
                      (local $y-fl    (ref null $Flonum))
                      ;; --- Check if x is a fixnum ---
                      (local.set $x/is-fx (ref.test (ref i31) (local.get $x)))
                      (if (local.get $x/is-fx)
                          (then (local.set $x-fx (i31.get_u (ref.cast (ref i31) (local.get $x))))
                                ;; Check low bit is 0 => valid fixnum
                                (if (i32.and (local.get $x-fx) (i32.const 1))
                                    (then (call $raise-check-fixnum (local.get $x)) (unreachable)))))
                      ;; --- Check if x is a flonum ---
                      (local.set $x/is-fl (ref.test (ref $Flonum) (local.get $x)))
                      ;; --- Raise if x is not a number ---
                      (if (i32.eqz (i32.or (local.get $x/is-fx) (local.get $x/is-fl)))
                          (then (call $raise-expected-number (local.get $x)) (unreachable)))
                      ;; --- Check if y is a fixnum ---
                      (local.set $y/is-fx (ref.test (ref i31) (local.get $y)))
                      (if (local.get $y/is-fx)
                          (then (local.set $y-fx (i31.get_u (ref.cast (ref i31) (local.get $y))))
                                (if (i32.and (local.get $y-fx) (i32.const 1))
                                    (then (call $raise-check-fixnum (local.get $y)) (unreachable)))))
                      ;; --- Check if y is a flonum ---
                      (local.set $y/is-fl (ref.test (ref $Flonum) (local.get $y)))
                      ;; --- Raise if y is not a number ---
                      (if (i32.eqz (i32.or (local.get $y/is-fx) (local.get $y/is-fl)))
                          (then (call $raise-expected-number (local.get $y)) (unreachable)))
                      ;; --- If both are fixnums, use fx< ---
                      (if (i32.and (local.get $x/is-fx) (local.get $y/is-fx))
                          (then (return (call ,fxcmp (local.get $x) (local.get $y)))))
                      ;; --- If both are flonums, use fl< ---
                      (if (i32.and (local.get $x/is-fl) (local.get $y/is-fl))
                          (then (return (call ,flcmp (local.get $x) (local.get $y)))))
                      ;; --- Mixed case: promote to flonum as needed ---
                      (if (local.get $x/is-fl)
                          (then (local.set $x-fl (ref.cast (ref $Flonum) (local.get $x))))
                          (else (local.set $x-fl (call $fx->fl (local.get $x)))))
                      (if (local.get $y/is-fl)
                          (then (local.set $y-fl (ref.cast (ref $Flonum) (local.get $y))))
                          (else (local.set $y-fl (call $fx->fl (local.get $y)))))

                      (call ,flcmp
                            (ref.as_non_null (local.get $x-fl))
                            (ref.as_non_null (local.get $y-fl)))))

             (list (gencmp '$=  '$fx=  '$fl=)   ; maybe specialize this one?
                   (gencmp '$<  '$fx<  '$fl<)
                   (gencmp '$>  '$fx>  '$fl>)
                   (gencmp '$<= '$fx<= '$fl<=)
                   (gencmp '$>= '$fx>= '$fl>=)))

         
         ;;;
         ;;;  Fixnums
         ;;;

         (func $raise-not-fixnum (param $x (ref eq)) (unreachable))

         (func $fixnum? (param $v (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.test i31ref (local.get $v))
                   (then (if (result (ref eq))
                             (i32.eqz (i32.and (i31.get_s (ref.cast i31ref (local.get $v)))
                                               (i32.const ,fixnum-mask)))
                             (then (global.get $true))
                             (else (global.get $false))))
                   (else (global.get $false))))

         (func $fx?/i32 (param $v (ref eq)) (result i32)
               (local $v/tag i32)
               (if (ref.test (ref i31) (local.get $v))
                   (then (local.set $v/tag (i31.get_u (ref.cast (ref i31) (local.get $v))))
                         (return (i32.eqz (i32.and (local.get $v/tag)
                                                   (i32.const 1)))))
                   (else (return (i32.const 0))))
               (unreachable))

         
         (func $fxzero? (param $x (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.test i31ref (local.get $x))
                   (then (if (result (ref eq))
                             (i32.eqz (i31.get_s (ref.cast i31ref (local.get $x))))
                             (then (global.get $true))
                             (else (global.get $false))))
                   (else (global.get $false))))
         (func $fx+ (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
               (ref.i31 (i32.add (i31.get_s (ref.cast i31ref (local.get $x)))
                                 (i31.get_s (ref.cast i31ref (local.get $y))))))
         (func $fx- (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
               (ref.i31 (i32.sub (i31.get_s (ref.cast i31ref (local.get $x)))
                                 (i31.get_s (ref.cast i31ref (local.get $y))))))
         ; Since an integer n is represented as 2n, we need to halve one argument. 
         (func $fx* (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
               (ref.i31 (i32.mul (i31.get_s (ref.cast i31ref (local.get $x)))
                                 ,(Half `(i31.get_s (ref.cast i31ref (local.get $y)))))))

         (func $fx/ (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
               (ref.i31 (i32.div_s (i31.get_s (ref.cast i31ref (local.get $x)))
                                   ,(Double `(i31.get_s (ref.cast i31ref (local.get $y)))))))

         (func $fx= (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.eq (call $fixnum? (local.get $v1))
                           (global.get $true))
                   (then (return_call $eq? (local.get $v1) (local.get $v2)))
                   (else (global.get $false))))

         ,@(for/list ([$fx-cmp '($fx<     $fx>     $fx<=    $fx>=)]
                      [inst    '(i32.lt_s i32.gt_s i32.le_s i32.ge_s)])
             `(func ,$fx-cmp
                    (param $x (ref eq))
                    (param $y (ref eq))
                    (result   (ref eq))               
                    ; type check
                    (if (i32.eqz (ref.test (ref i31) (local.get $x)))
                        (then (call $raise-check-fixnum (local.get $x)) (unreachable)))
                    (if (i32.eqz (ref.test (ref i31) (local.get $y)))
                        (then (call $raise-check-fixnum (local.get $y)) (unreachable)))
                    ; compare
                    (if (result (ref eq))
                        (,inst (i31.get_s (ref.cast (ref i31) (local.get $x)))
                               (i31.get_s (ref.cast (ref i31) (local.get $y))))
                        (then (global.get $true))
                        (else (global.get $false)))))

         (func $fixnum->i32 (param $x (ref eq)) (result i32)
               (local $val i32)
               (if (ref.test (ref i31) (local.get $x))
                   (then (local.set $val (i31.get_u (ref.cast (ref i31) (local.get $x))))
                         (if (i32.eqz (i32.and (local.get $val) (i32.const 1)))
                             (then (return (i32.shr_u (local.get $val) (i32.const 1)))) ;; return unboxed i32
                             (else (call $raise-not-fixnum (local.get $x)))))
                   (else (call $raise-not-fixnum (local.get $x))))
               (unreachable))

         ;;;
         ;;; Floating Point Numbers
         ;;;

         (func $i32->flonum (param $n i32) (result (ref $Flonum))
               (struct.new $Flonum
                           (i32.const 0)  ;; initial hash = 0
                           (f64.convert_i32_s (local.get $n))))
         
         ;; fl?/i32 : (ref eq) -> i32
         ;;   Returns 1 if the value is a flonum, 0 otherwise
         (func $fl?/i32 (param $a (ref eq)) (result i32)
               (ref.test (ref $Flonum) (local.get $a)))

         ;; flonum? : (ref eq) -> (ref eq)
         ;;   Returns #t if the value is a flonum, #f otherwise
         (func $flonum? (param $a (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.test (ref $Flonum) (local.get $a))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $fx->fl (param $v (ref eq)) (result (ref $Flonum))
               (local $v/i32 i32)
               ;; Check that v is a fixnum (ref i31) and has low bit 0
               (if (i32.eqz (ref.test (ref i31) (local.get $v)))
                   (then (call $raise-check-fixnum (local.get $v))
                         (unreachable)))
               (local.set $v/i32 (i31.get_u (ref.cast (ref i31) (local.get $v))))
               (if (i32.and (local.get $v/i32) (i32.const 1))
                   (then (call $raise-check-fixnum (local.get $v))
                         (unreachable)))
               ;; Convert fixnum to flonum
               (struct.new $Flonum
                           (i32.const 0)                             ;; hash = 0
                           (f64.convert_i32_s (i32.shr_u (local.get $v/i32) (i32.const 1)))))


         
         #;(func $fl+
               (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
               ;; Type checks
               (if (i32.eqz (ref.test (ref $Flonum) (local.get $x)))
                   (then (call $raise-argument-error:flonum-expected (local.get $x))))
               (if (i32.eqz (ref.test (ref $Flonum) (local.get $y)))
                   (then (call $raise-argument-error:flonum-expected (local.get $y))))
               ;; Compute and box result
               (struct.new $Flonum
                           (f64.add
                            (struct.get $Flonum $v
                                        (ref.cast (ref $Flonum) (local.get $x)))
                            (struct.get $Flonum $v
                                        (ref.cast (ref $Flonum) (local.get $y))))))

         (func $raise-argument-error:flonum-expected (unreachable))
         
         ,@(let ()
             (define (flbinop name flbinop)
               `(func ,name
                      (param $x (ref eq)) (param $y (ref eq))
                      (result (ref eq))
                      ;; Type checks
                      (if (i32.eqz (ref.test (ref $Flonum) (local.get $x)))
                          (then (call $raise-argument-error:flonum-expected (local.get $x))
                                (unreachable)))
                      (if (i32.eqz (ref.test (ref $Flonum) (local.get $y)))
                          (then (call $raise-argument-error:flonum-expected (local.get $y))
                                (unreachable)))
                      ;; Compute and box result
                      (struct.new $Flonum
                                  (i32.const 0)
                                  (,flbinop
                                   (struct.get $Flonum $v                                               
                                               (ref.cast (ref $Flonum) (local.get $x)))
                                   (struct.get $Flonum $v                                               
                                               (ref.cast (ref $Flonum) (local.get $y)))))))
             (map flbinop
                  '($fl+    $fl-    $fl*)
                  '(f64.add f64.sub f64.mul)))

         (func $unsafe-fl/
               (param $x (ref $Flonum))
               (param $y (ref $Flonum))
               (result   (ref $Flonum))
               
               (local $x/f64 f64)
               (local $y/f64 f64)
               ;; Extract f64 values
               (local.set $x/f64 (struct.get $Flonum $v (local.get $x)))
               (local.set $y/f64 (struct.get $Flonum $v (local.get $y)))
               ;; Compute and box result
               (struct.new $Flonum
                           (i32.const 0) ;; hash = 0
                           (f64.div (local.get $x/f64) (local.get $y/f64))))
         
         (func $fl//checked
               (param $x (ref $Flonum))
               (param $y (ref $Flonum))
               (result   (ref $Flonum))
               
               (struct.new $Flonum
                           (i32.const 0) ;; hash = 0
                           (f64.div
                            (struct.get $Flonum $v (local.get $x))
                            (struct.get $Flonum $v (local.get $y)))))


         (func $raise-check-flonum (unreachable))
         
         (func $fl/
               (param $x (ref eq))
               (param $y (ref eq))
               (result   (ref $Flonum))
               
               (local $x/fl  (ref $Flonum))
               (local $y/fl  (ref $Flonum))
               (local $x/f64 f64)
               (local $y/f64 f64)
               ;; Type checks
               (if (i32.eqz (ref.test (ref $Flonum) (local.get $x)))
                   (then (call $raise-check-flonum (local.get $x))
                         (unreachable)))
               (if (i32.eqz (ref.test (ref $Flonum) (local.get $y)))
                   (then (call $raise-check-flonum (local.get $y))
                         (unreachable)))
               ;; Cast and extract
               (local.set $x/fl  (ref.cast (ref $Flonum) (local.get $x)))
               (local.set $y/fl  (ref.cast (ref $Flonum) (local.get $y)))
               (local.set $x/f64 (struct.get $Flonum $v (local.get $x/fl)))
               (local.set $y/f64 (struct.get $Flonum $v (local.get $y/fl)))
               ;; Compute and box result
               (struct.new $Flonum
                           (i32.const 0)
                           (f64.div (local.get $x/f64) (local.get $y/f64))))


         ,@(let ()
             (define (fl-cmp flname flcmp)
               `(func ,flname
                      (param $x (ref eq)) (param $y (ref eq))
                      (result (ref eq))
                      ;; Type checks
                      (if (i32.eqz (ref.test (ref $Flonum) (local.get $x)))
                          (then (call $raise-argument-error:flonum-expected (local.get $x))
                                (unreachable)))
                      (if (i32.eqz (ref.test (ref $Flonum) (local.get $y)))
                          (then (call $raise-argument-error:flonum-expected (local.get $y))
                                (unreachable)))
                      ;; Compute and box result
                      (if (result (ref eq))
                          (,flcmp
                           (struct.get $Flonum $v                                               
                                       (ref.cast (ref $Flonum) (local.get $x)))
                           (struct.get $Flonum $v                                               
                                       (ref.cast (ref $Flonum) (local.get $y))))
                          (then (global.get $true))
                          (else (global.get $false)))))
             (map fl-cmp
                  '($fl=    $fl<    $fl>    $fl<=     $fl>=)
                  '(f64.eq  f64.lt  f64.gt  f64.le    f64.ge)))

         
         ;;;
         ;;; Number / String conversions
         ;;;

         ;;; Helpers for $number->string

         (func $raise-number->string-bad-input (unreachable))
         (func $raise-number->string-bad-radix (unreachable))
         
         (func $number->string:check-radix (param $radix i32) (result i32)
               ; number->string accepts 2, 8, 10, or 16 as radix
               (if (result i32) (i32.or (i32.eq (local.get $radix) (i32.const 2))
                    (i32.or (i32.eq (local.get $radix) (i32.const 8))
                     (i32.or (i32.eq (local.get $radix) (i32.const 10))
                             (i32.eq (local.get $radix) (i32.const 16)))))
                   (then (i32.const 1))
                   (else (i32.const 0))))

         (func $number->string:max-length (param $radix i32) (result i32)
               ;; Returns the maximum length of a fixnum when converted to a string
               ;; in the given radix, including the sign character.
               (if (result i32) (i32.eq (local.get $radix) (i32.const 2))
                   (then (i32.const 30)) ;; binary: 29 bits + sign
                   (else
                    (if (result i32) (i32.eq (local.get $radix) (i32.const 8))
                        (then (i32.const 12)) ;; octal: 11 digits + sign
                        (else
                         (if (result i32) (i32.eq (local.get $radix) (i32.const 10))
                             (then (i32.const 11)) ;; decimal: 10 digits + sign
                             (else
                              (if (result i32) (i32.eq (local.get $radix) (i32.const 16))
                                  (then (i32.const 9)) ;; hex: 8 digits + sign
                                  (else (i32.const 0)) ;; invalid radix, caller should check
                                  ))))))))
         
         (func $number->string:convert
               (param $n       i32)
               (param $radix   i32)
               (param $max-len i32)
               (result         (ref $String))

               (local $buf (ref $I32Array))
               (local $i   i32)
               (local $tmp i32)
               (local $neg i32)
               (local $abs i32)
               (local $out (ref $I32Array))
               (local $len i32)

               ;; Special case for 0
               (if (i32.eqz (local.get $n))
                   (then (return (call $i32->string (local.get $n)))))
               ;; Step 1: Determine sign and absolute value
               (local.set $neg (i32.lt_s (local.get $n) (i32.const 0)))
               (local.set $abs (if (result i32)
                                   (i32.ge_s (local.get $n) (i32.const 0))
                                   (then (local.get $n))
                                   (else (i32.sub (i32.const 0) (local.get $n)))))
               ;; Step 2: Create buffer of max-len, and fill from right to left
               (local.set $buf (array.new_default $I32Array (local.get $max-len)))
               (local.set $i   (local.get $max-len))
               (block $done
                      (loop $digit-loop
                            (br_if $done (i32.eqz (local.get $abs)))
                            (local.set $i   (i32.sub (local.get $i) (i32.const 1)))
                            (local.set $tmp (i32.rem_u (local.get $abs) (local.get $radix)))
                            (array.set $I32Array (local.get $buf) (local.get $i)
                                       (select
                                        (i32.add (local.get $tmp) (i32.const 48)) ;; '0'-'9'
                                        (i32.add (local.get $tmp) (i32.const 87)) ;; 'a'-'f'
                                        (i32.lt_u (local.get $tmp) (i32.const 10))))
                            (local.set $abs (i32.div_u (local.get $abs) (local.get $radix)))
                            (br $digit-loop)))
               ;; Step 3: Add minus sign if negative
               (if (local.get $neg)
                   (then
                    (local.set $i (i32.sub (local.get $i) (i32.const 1)))
                    (array.set $I32Array (local.get $buf) (local.get $i) (i32.const 45)))) ;; '-'
               ;; Step 4: Slice from $i to $max-len using array.copy
               (local.set $len (i32.sub (local.get $max-len) (local.get $i)))
               (local.set $out (array.new_default $I32Array (local.get $len)))
               (array.copy $I32Array $I32Array
                           (local.get $out) (i32.const 0)
                           (local.get $buf) (local.get $i)
                           (local.get $len))
               ;; Step 5: Wrap into a String object
               (struct.new $String
                           (i32.const 0)        ;; hash = 0
                           (i32.const 1)        ;; immutable = true
                           (local.get $out)))
                  
         (func $number->string
               (param $z         (ref eq))
               (param $radix-raw (ref eq))
               (result           (ref $String))

               (local $radix i32)
               (local $n     i32)
               (local $max   i32)
               (local $i31   (ref i31))
               ;; Step 1: Check that $z is a fixnum and extract signed i32
               (if (ref.test (ref i31) (local.get $z))
                   (then (local.set $n (i32.shr_s (i31.get_s (ref.cast (ref i31) (local.get $z)))
                                                  (i32.const 1))))
                   (else (call $raise-number->string-bad-input)))
               ;; Step 2: Handle radix
               (if (ref.eq (local.get $radix-raw) (global.get $false))
                   (then (local.set $radix (i32.const 10))) ;; default = 10
                   (else (if (ref.test (ref i31) (local.get $radix-raw))
                             (then (local.set $radix
                                              (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $radix-raw)))
                                                         (i32.const 1))))
                             (else (call $raise-number->string-bad-radix)))))
               ;; Step 3: Validate the radix
               (if (i32.eqz (call $number->string:check-radix (local.get $radix)))
                   (then (call $raise-number->string-bad-radix)))
               ;; Step 4: Get max length for this radix
               (local.set $max (call $number->string:max-length (local.get $radix)))
               ;; Step 5: Convert and return
               (call $number->string:convert (local.get $n) (local.get $radix) (local.get $max)))


         
         ;; This is a very naive implementation.
         ;; It is used as a place holder for now.
         (func $f64->string
               (param $x f64)
               (result (ref $String))

               (local $abs         f64)
               (local $int         i32)
               (local $frac-scaled f64)
               (local $frac        i32)
               (local $neg         i32)
               (local $s-int       (ref $String))
               (local $s-frac      (ref $String))
               (local $dot         (ref $String))
               (local $minus       (ref $String))
               (local $nan         (ref $String))
               (local $pinf        (ref $String))
               (local $ninf        (ref $String))
               ;; --- Construct needed string segments ---
               (local.set $dot   (call $codepoint->string (i32.const 46))) ;; "."
               (local.set $minus (call $codepoint->string (i32.const 45))) ;; "-"
               ;; "+nan.0"
               (local.set $nan (call $i32array->string
                                     (array.new_fixed $I32Array 6
                                                      (i32.const 43)  ;; '+'
                                                      (i32.const 110) ;; 'n'
                                                      (i32.const 97)  ;; 'a'
                                                      (i32.const 110) ;; 'n'
                                                      (i32.const 46)  ;; '.'
                                                      (i32.const 48)  ;; '0'
                                                      )))
               ;; "+inf.0"
               (local.set $pinf (call $i32array->string
                                      (array.new_fixed $I32Array 6
                                                       (i32.const 43)  ;; '+'
                                                       (i32.const 105) ;; 'i'
                                                       (i32.const 110) ;; 'n'
                                                       (i32.const 102) ;; 'f'
                                                       (i32.const 46)  ;; '.'
                                                       (i32.const 48)  ;; '0'
                                                       )))
               ;; "-inf.0"
               (local.set $ninf (call $i32array->string
                                      (array.new_fixed $I32Array 6
                                                       (i32.const 45)  ;; '-'
                                                       (i32.const 105) ;; 'i'
                                                       (i32.const 110) ;; 'n'
                                                       (i32.const 102) ;; 'f'
                                                       (i32.const 46)  ;; '.'
                                                       (i32.const 48)  ;; '0'
                                                       )))
               ;; --- Handle special cases ---
               ;; NaN: x != x
               (if (f64.ne (local.get $x) (local.get $x))
                   (then (return (local.get $nan))))
               ;; +inf.0
               (if (f64.eq (local.get $x) (f64.const inf))
                   (then (return (local.get $pinf))))
               ;; -inf.0
               (if (f64.eq (local.get $x) (f64.const -inf))
                   (then (return (local.get $ninf))))
               ;; --- Absolute value and sign ---
               (local.set $abs (f64.abs (local.get $x)))
               (local.set $neg (f64.lt (local.get $x) (f64.const 0)))
               ;; --- Integer part ---
               (local.set $int (i32.trunc_f64_s (local.get $abs)))
               ;; Scientific notation fallback if too large
               (if (i32.ge_u (local.get $int) (i32.const 1000000))
                   (then (return (call $f64->string/scientific (local.get $x)))))
               ;; --- Fractional part ---
               (local.set $frac-scaled
                          (f64.mul
                           (f64.sub (local.get $abs)
                                    (f64.convert_i32_s (local.get $int)))
                           (f64.const 1000000.0)))
               (local.set $frac (i32.trunc_f64_s (local.get $frac-scaled)))
               ;; --- Convert parts to strings ---
               (local.set $s-int  (call $i32->string       (local.get $int)))
               (local.set $s-frac (call $i32->string/pad6  (local.get $frac)))
               ;; --- Join int . frac ---
               (local.set $s-int (call $string-append
                                       (call $string-append
                                             (local.get $s-int)
                                             (local.get $dot))
                                       (local.get $s-frac)))
               ;; --- Add minus sign if needed ---
               (if (result (ref $String))
                   (local.get $neg)
                   (then (call $string-append (local.get $minus) (local.get $s-int)))
                   (else (local.get $s-int))))
         
         (func $i32->string/pad6
               ; Converts a non-negative integer to a decimal string padded with
               ; leading zeros to exactly 6 digits.
               (param $n i32)
               (result (ref $String))

               (local $g      (ref $I32GrowableArray))
               (local $digits (ref $I32GrowableArray))
               (local $digit  i32)
               (local $count  i32)
               
               ;; Make a growable array to collect digits
               (local.set $digits (call $make-i32growable-array (i32.const 6)))               
               ;; Extract digits in reverse order (least significant first)
               (block $done
                      (loop $loop
                            ;; Append digit
                            (local.set $digit (i32.rem_u (local.get $n) (i32.const 10)))
                            (call $i32growable-array-add!
                                  (local.get $digits) (i32.add (local.get $digit) (i32.const 48)))
                            ;; Prepare next
                            (local.set $n (i32.div_u (local.get $n) (i32.const 10)))
                            (br_if $loop (i32.ne (local.get $n) (i32.const 0)))))
               ;; Pad with '0's until we have at least 6 digits
               (local.set $count (call $i32growable-array-count (local.get $digits)))
               (block $pad
                      (loop $pad-loop
                            (br_if $pad (i32.ge_u (local.get $count) (i32.const 6)))
                            (call $i32growable-array-add! (local.get $digits) (i32.const 48)) ;; '0'
                            (local.set $count (i32.add (local.get $count) (i32.const 1)))
                            (br $pad-loop)))
               ;; Create final output array and reverse digits
               (local.set $g (call $make-i32growable-array (i32.const 6)))
               (local.set $count (call $i32growable-array-count (local.get $digits)))
               (block $done
                      (loop $rev
                            (br_if $done (i32.eqz (local.get $count)))
                            (local.set $count (i32.sub (local.get $count) (i32.const 1)))
                            (call $i32growable-array-add!
                                  (local.get $g)
                                  (call $i32growable-array-ref (local.get $digits) (local.get $count)))
                            (br $rev)))
               ;; Return as string
               (call $i32growable-array->string (local.get $g)))

         
         (func $f64->string/scientific ; TODO: This is a naive version.
               ; This function converts a 64-bit floating-point number (f64) into a
               ; human-readable scientific notation string like "1.234567e+02".               
               ; It handles special cases (NaN, +inf.0, -inf.0), computes the normalized
               ; mantissa and exponent, formats the number to 6  decimal places,
               ; and constructs the result as a Racket-style string.

               (param $x f64)
               (result (ref $String))

               (local $abs   f64)
               (local $neg   i32)
               (local $exp   i32)
               (local $digit i32)
               (local $frac  i32)
               (local $mant  f64)
               (local $g     (ref $I32GrowableArray))
               (local $exp-str (ref $String))
               (local $exp-cps (ref $I32Array))
               (local $count i32)
               (local $i i32)

               ;; --- Special cases ---
               (if (f64.ne (local.get $x) (local.get $x))  ;; NaN
                   (then
                    (return
                     (call $i32array->string
                           (array.new_fixed $I32Array 5  ;; "nan.0"
                                            (i32.const 110) (i32.const 97) (i32.const 110)
                                            (i32.const 46) (i32.const 48))))))
               
               (if (f64.eq (local.get $x) (f64.const inf))  ;; +inf.0
                   (then
                    (return
                     (call $i32array->string
                           (array.new_fixed $I32Array 6    ;; +inf.0
                                            (i32.const 43) (i32.const 105) (i32.const 110)
                                            (i32.const 102) (i32.const 46) (i32.const 48))))))

               (if (f64.eq (local.get $x) (f64.const -inf)) 
                   (then
                    (return
                     (call $i32array->string
                           (array.new_fixed $I32Array 6  ;; -inf.0
                                            (i32.const 45) (i32.const 105) (i32.const 110)
                                            (i32.const 102) (i32.const 46) (i32.const 48))))))

               ;; --- Normalize ---
               (local.set $neg (f64.lt (local.get $x) (f64.const 0)))
               (local.set $abs (f64.abs (local.get $x)))
               (local.set $exp (i32.const 0))
               (block $down
                      (loop $norm-down
                            (br_if $down (f64.ge (local.get $abs) (f64.const 1)))
                            (local.set $abs (f64.mul (local.get $abs) (f64.const 10)))
                            (local.set $exp (i32.sub (local.get $exp) (i32.const 1)))
                            (br $norm-down)))
               (block $up
                      (loop $norm-up
                            (br_if $up (f64.lt (local.get $abs) (f64.const 10)))
                            (local.set $abs (f64.div (local.get $abs) (f64.const 10)))
                            (local.set $exp (i32.add (local.get $exp) (i32.const 1)))
                            (br $norm-up)))
               ;; --- Format mantissa ---
               (local.set $mant (local.get $abs))
               (local.set $g    (call $make-i32growable-array (i32.const 16)))
               ;; Optional sign
               (if (local.get $neg)
                   (then (call $i32growable-array-add! (local.get $g) (i32.const 45)))) ;; '-'
               ;; Leading digit
               (local.set $digit (i32.trunc_f64_u (local.get $mant)))
               (call $i32growable-array-add! (local.get $g)
                     (i32.add (i32.const 48) (local.get $digit)))
               ;; Decimal point
               (call $i32growable-array-add! (local.get $g) (i32.const 46)) ;; '.'
               ;; Fractional digits
               (local.set $mant (f64.sub (local.get $mant)
                                         (f64.convert_i32_u (local.get $digit))))
               (local.set $frac (i32.const 0))
               (block $exit
                      (loop $frac-loop
                            (br_if $exit (i32.ge_u (local.get $frac) (i32.const 6)))
                            (local.set $mant (f64.mul (local.get $mant) (f64.const 10)))
                            (local.set $digit (i32.trunc_f64_u (local.get $mant)))
                            (call $i32growable-array-add!
                                  (local.get $g)
                                  (i32.add (i32.const 48) (local.get $digit)))
                            (local.set $mant
                                       (f64.sub (local.get $mant)
                                                (f64.convert_i32_u (local.get $digit))))
                            (local.set $frac (i32.add (local.get $frac) (i32.const 1)))
                            (br $frac-loop)))
               ;; 'e'
               (call $i32growable-array-add! (local.get $g) (i32.const 101)) ;; 'e'
               ;; Exponent
               (local.set $exp-str
                          (call $number->string
                                (ref.i31 (i32.shl (local.get $exp) (i32.const 1)))   ;; boxed fixnum
                                (ref.i31 (i32.const 20))))                          ;; fixnum 10 (radix)
               (local.set $exp-cps
                          (struct.get $String $codepoints (local.get $exp-str)))
               ;; Append minus sign if negative
               (if (i32.lt_s (local.get $exp) (i32.const 0))
                   (then (call $i32growable-array-add! (local.get $g) (i32.const 45)))) ;; '-'
               ;; Append digits
               (local.set $count (array.len (local.get $exp-cps)))
               (local.set $i (i32.const 0))
               (block $done
                      (loop $append-loop
                            (br_if $done (i32.ge_u (local.get $i) (local.get $count)))
                            (call $i32growable-array-add!
                                  (local.get $g)
                                  (array.get $I32Array (local.get $exp-cps) (local.get $i)))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $append-loop)))
               (call $i32growable-array->string (local.get $g)))

         (func $i32->string
               (param $n   i32)
               (result     (ref $String))
               (call $i32array->string
                     (call $i32->codepoints
                           (local.get $n))))
         
         (func $i32->codepoints
               (param $n   i32)
               (result     (ref $I32Array))

               (local $len i32)
               (local $tmp i32)
               (local $abs i32)
               (local $neg i32)
               (local $i   i32)
               (local $d   i32)
               (local $arr (ref $I32Array))

               ;; Special case: 0 → [48]
               (if (i32.eqz (local.get $n))
                   (then
                    (return (array.new_fixed $I32Array 1 (i32.const 48))))) ;; '0'
               ;; Check for negative
               (local.set $neg (i32.lt_s (local.get $n) (i32.const 0)))
               ;; Take absolute value safely (avoid overflow on min int)
               (local.set $abs (select
                                (i32.sub (i32.const 0) (local.get $n)) ;; -n
                                (local.get $n)
                                (local.get $neg)))
               ;; Count digits
               (local.set $tmp (local.get $abs))
               (local.set $len (i32.const 0))
               (block $done-count
                      (loop $count
                            (br_if $done-count (i32.eqz (local.get $tmp)))
                            (local.set $tmp (i32.div_u (local.get $tmp) (i32.const 10)))
                            (local.set $len (i32.add (local.get $len) (i32.const 1)))
                            (br $count)))
               ;; Add one more if negative (for '-')
               (if (local.get $neg)
                   (then (local.set $len (i32.add (local.get $len) (i32.const 1)))))
               ;; Allocate array of codepoints
               (local.set $arr (array.new_default $I32Array (local.get $len)))
               ;; Fill from right to left
               (local.set $tmp (local.get $abs))
               (local.set $i (i32.sub (local.get $len) (i32.const 1)))
               (block $fill-done
                      (loop $fill
                            (br_if $fill-done (i32.lt_s (local.get $i) (i32.const 0)))
                            (local.set $d (i32.rem_u (local.get $tmp) (i32.const 10)))
                            (array.set $I32Array
                                       (local.get $arr)
                                       (local.get $i)
                                       (i32.add (i32.const 48) (local.get $d))) ;; '0' + d
                            (local.set $tmp (i32.div_u (local.get $tmp) (i32.const 10)))
                            (local.set $i (i32.sub (local.get $i) (i32.const 1)))
                            (br $fill)))
               ;; Add '-' if needed
               (if (local.get $neg)
                   (then
                    (array.set $I32Array (local.get $arr) (i32.const 0) (i32.const 45)))) ;; '-'
               (local.get $arr))
         
         (func $format/display:flonum
               (param $val (ref $Flonum))
               (result     (ref $String))
               (call $string-trim-right
                     (call $f64->string
                           (struct.get $Flonum $v (local.get $val)))
                     ,(Imm #\0)))
         
         ;;;
         ;;;  - Void
         ;;;
         (func $void? (param $v (ref eq))  (result (ref eq))
               (if (result (ref eq))
                   (ref.eq (local.get $v) (global.get $void))
                   (then (global.get $true))
                   (else (global.get $false))))
         (func $make-void (result (ref eq)) ; no arguments
               (return (global.get $void)))

         ;;  - Boolean
         ; todo: Benchmark the two implementations of $boolean? below
         (func $boolean? (param $v (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.eq (local.get $v) (global.get $true))
                   (then (global.get $true))
                   (else (if (result (ref eq))
                             (ref.eq (local.get $v) (global.get $false))
                             (then (global.get $true))
                             (else (global.get $false))))))
         ; see comment above
         #;(func $boolean? (param $v (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.test i31ref (local.get $v))
                   (then (if (result (ref eq))
                             (i32.eq (i32.and (i31.get_s (ref.cast i31ref (local.get $v)))
                                              (i32.const ,boolean-mask))
                                     (i32.const ,boolean-tag))
                             (then (global.get $true))
                             (else (global.get $false))))
                   (else (global.get $false))))
         (func $not (param $v (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.eq (local.get $v) (global.get $false))
                   (then (global.get $true))
                   (else (global.get $false))))         
         ;;;
         ;;; - Characters
         ;;;
         
         (func $raise-check-char (param $x (ref eq)) (unreachable))
         
         (func $char? (param $v (ref eq)) (result (ref eq))
               (local $i31 (ref i31))
               ; Is $v an immediate?
               (if (i32.eqz (ref.test (ref i31) (local.get $v)))
                   (then (return (global.get $false))))               
               (local.set $i31 (ref.cast (ref i31) (local.get $v)))
               ; Is it a character?
               (if (result (ref eq))
                   (i32.eq (i32.and (i31.get_s (local.get $i31)) (i32.const ,char-mask))
                           (i32.const ,char-tag))
                   (then (global.get $true))
                   (else (global.get $false))))


         (func $char=? (param $c1 (ref eq)) (param $c2 (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.eq (call $char? (local.get $c1))
                           (global.get $true))
                   (then (return_call $eq? (local.get $c1) (local.get $c2)))
                   (else (global.get $false))))

         (func $char->integer (param $c (ref eq)) (result (ref eq))
               (local $i31   (ref i31))
               (local $c/tag i32)
               (local $cp    i32)
               ;; Check if $c is an i31
               (if (i32.eqz (ref.test (ref i31) (local.get $c)))
                   (then (call $raise-check-char (local.get $c))))
               (local.set $i31   (ref.cast (ref i31) (local.get $c)))
               (local.set $c/tag (i31.get_u (local.get $i31)))
               ;; Check character tag
               (if (i32.ne (i32.and (local.get $c/tag) (i32.const ,char-mask))
                           (i32.const ,char-tag))
                   (then (call $raise-check-char (local.get $c))))

               ;; Extract codepoint and return as fixnum
               (local.set $cp (i32.shr_u (local.get $c/tag) (i32.const ,char-shift)))
               (ref.i31 (i32.shl (local.get $cp) (i32.const 1))))

         (func $char->integer/i32 (param $c (ref eq)) (result i32)
               (local $raw i32)
               ;; Check that $c is an i31
               (if (ref.test (ref i31) (local.get $c))
                   (then
                    ;; Extract the raw bits
                    (local.set $raw (i31.get_u (ref.cast (ref i31) (local.get $c))))
                    ;; Verify the tag is 0x0F
                    (if (i32.eq (i32.and (local.get $raw) (i32.const ,char-mask)) (i32.const ,char-tag))
                        (then
                         ;; Return the codepoint: raw >> 8
                         (return (i32.shr_u (local.get $raw) (i32.const ,char-shift))))
                        (else
                         (call $raise-check-char (local.get $c)))))
                   (else
                    (call $raise-check-char (local.get $c))))
               (unreachable))

         (func $char-whitespace? (param $c (ref eq)) (result (ref eq))
               (local $i31   (ref i31))
               (local $c/tag i32)
               (local $cp    i32)
               ;; Type check
               (if (i32.eqz (ref.test (ref i31) (local.get $c)))
                   (then (call $raise-check-char (local.get $c))))
               (local.set $i31   (ref.cast (ref i31) (local.get $c)))
               (local.set $c/tag (i31.get_u (local.get $i31)))
               ;; Decode codepoint
               (if (i32.ne (i32.and (local.get $c/tag)
                                    (i32.const ,char-mask))
                           (i32.const ,char-tag))
                   (then (call $raise-check-char (local.get $c))))
               (local.set $cp (i32.shr_u (local.get $c/tag) (i32.const ,char-shift)))
               ;; Delegate
               (call $char-whitespace?/ucs (local.get $cp)))
         
         (func $char-whitespace?/ucs (param $cp i32) (result (ref eq))
               (if (i32.eq (local.get $cp) (i32.const ,(char->integer #\space)))
                   (then (return (global.get $true))))
               ;; U+0009..000D: tab, newline, vtab, formfeed, return
               (if (i32.le_u (local.get $cp) (i32.const ,(char->integer #\return)))
                   (then (if (i32.ge_u (local.get $cp) (i32.const ,(char->integer #\tab)))
                             (then (return (global.get $true))))))
               (if (i32.eq (local.get $cp) (i32.const ,(char->integer #\u0085))) ; NEXT LINE (NEL)
                   (then (return (global.get $true))))
               (if (i32.eq (local.get $cp) (i32.const ,(char->integer #\u00A0))) ; NO-BREAK SPACE (NBSP)
                   (then (return (global.get $true))))
               (if (i32.eq (local.get $cp) (i32.const ,(char->integer #\u1680))) ; OGHAM SPACE MARK
                   (then (return (global.get $true))))
               ;; U+2000–U+200A
               (if (i32.le_u (local.get $cp) (i32.const ,(char->integer #\u200A)))
                   (then (if (i32.ge_u (local.get $cp) (i32.const ,(char->integer #\u2000)))
                             (then (return (global.get $true))))))
               (if (i32.eq (local.get $cp) (i32.const ,(char->integer #\u2028))) ; LINE SEPARATOR
                   (then (return (global.get $true))))
               (if (i32.eq (local.get $cp) (i32.const ,(char->integer #\u2029))) ; PARAGRAPH SEPARATOR
                   (then (return (global.get $true))))
               (if (i32.eq (local.get $cp) (i32.const ,(char->integer #\u202F))) ; NARROW NO-BREAK SPACE
                   (then (return (global.get $true))))
               (if (i32.eq (local.get $cp) (i32.const ,(char->integer #\u205F))) ; MEDIUM MATHEMATICAL SPACE
                   (then (return (global.get $true))))
               (if (i32.eq (local.get $cp) (i32.const ,(char->integer #\u3000))) ; IDEOGRAPHIC SPACE
                   (then (return (global.get $true))))
               (global.get $false))

         (func $raise-invalid-codepoint (unreachable))
         
         (func $integer->char
               (param $k (ref eq))
               (result   (ref eq))
               
               (local $k/i32 i32)
               ;; Fail early if not a fixnum
               (if (i32.eqz (ref.test (ref i31) (local.get $k)))
                   (then (call $raise-expected-fixnum (local.get $k)) (unreachable)))
               ;; Unpack fixnum (must have LSB = 0)
               (local.set $k/i32 (i31.get_u (ref.cast (ref i31) (local.get $k))))
               (if (i32.and (local.get $k/i32) (i32.const 1))
                   (then (call $raise-expected-fixnum (local.get $k)) (unreachable)))
               (local.set $k/i32 (i32.shr_u (local.get $k/i32) (i32.const 1)))
               ;; Check allowed Unicode code point range:
               ;;   [0, 0xD7FF] or [0xE000, 0x10FFFF]
               (if (i32.or (i32.and (i32.ge_u (local.get $k/i32) (i32.const #xD800))
                                    (i32.le_u (local.get $k/i32) (i32.const #xDFFF)))
                           (i32.gt_u (local.get $k/i32) (i32.const #x10FFFF)))
                   (then (call $raise-invalid-codepoint (local.get $k)) (unreachable)))
               ;; TODO: Shared character object for 0 <= k < 256
               ;; (if needed, insert lookup here)
               ;; Pack as character: (k << (char-shift - 1)) | char-tag
               (ref.i31 (i32.or (i32.shl (local.get $k/i32) 
                                         (i32.const ,char-shift))
                                (i32.const ,char-tag))))
         
         
         ;;  - Boxed (for assignable variables)
         (func $boxed (param $v (ref eq))  (result (ref eq)) 
               (struct.new $Boxed (local.get $v)))
         (func $unboxed (param $b (ref eq))  (result (ref eq))
               (struct.get $Boxed $v
                           (block $ok (result (ref $Boxed))
                             (br_on_cast $ok (ref eq) (ref $Boxed) (local.get $b))
                             (unreachable))))
         (func $set-boxed!
               ; todo: make this return no values
               ;       problem: set-boxed! is currently wrapped by drop in
               ;                the code generator. Add an rule that avoids
               ;                the drop for set-boxed!.
               (param $b (ref eq)) (param $v (ref eq))
               (result (ref eq))
               ; 1. Cast $b into a (ref $Box)
               (local $B (ref $Boxed))                
               (local.set $B
                          (block $ok (result (ref $Boxed))
                                 (br_on_cast $ok (ref eq) (ref $Boxed) (local.get $b))
                                 (return (global.get $error))))
               ; 2. Set the contents
               (struct.set $Boxed $v (local.get $B) (local.get $v))
               ; 3. Return `void`
               (global.get $void))

         ;;  - Boxes (for the Racket data type `box`)
         (func $box (param $v (ref eq))  (result (ref eq)) 
               (struct.new $Box (i32.const 0) (local.get $v)))
         (func $unbox (param $b (ref eq))  (result (ref eq))
               (struct.get $Box $v
                           (block $ok (result (ref $Box))
                             (br_on_cast $ok (ref eq) (ref $Box) (local.get $b))
                             (return (global.get $error)))))
         (func $set-box! ; todo: should this invalidate the hash code?
               (param $b (ref eq)) (param $v (ref eq))
               (result (ref eq))
               ; 1. Cast $b into a (ref $Box)
               (local $B (ref $Box))                
               (local.set $B
                          (block $ok (result (ref $Box))
                                 (br_on_cast $ok (ref eq) (ref $Box) (local.get $b))
                                 (return (global.get $error))))
               ; 2. Set the contents
               (struct.set $Box $v (local.get $B) (local.get $v))
               ; 3. Return `void`
               (global.get $void))

         ;;;
         ;;; - Pairs and lists
         ;;;

         (global $dummy-pair (ref $Pair)
                 (struct.new $Pair
                             (i32.const 0)           ;; hash = 0
                             (global.get $false)      ;; a = null
                             (global.get $false)))    ;; d = null
         
         ;; Pair related exceptions         
         (func $raise-pair-expected (param $x (ref eq)) (unreachable))
         (func $raise-bad-list-ref-index
               (param $xs  (ref $Pair)) (param $i   i32) (param $len i32)
               (unreachable))
         (func $pair? (param $v (ref eq)) (result (ref eq))
               (if (result (ref eq)) (ref.test (ref $Pair) (local.get $v))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $null? (param $v (ref eq)) (result (ref eq))
               (if (result (ref eq)) (ref.eq (local.get $v) (global.get $null))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $cons (param $a (ref eq)) (param $d (ref eq)) (result (ref eq))
               (struct.new $Pair (i32.const 0) (local.get $a) (local.get $d)))

         (func $car (param $v (ref eq)) (result (ref eq))
               (if (result (ref eq)) (ref.test (ref $Pair) (local.get $v))
                   (then (struct.get $Pair $a (ref.cast (ref $Pair) (local.get $v))))
                   (else (call $raise-pair-expected (local.get $v))
                         (unreachable))))

         (func $cdr (param $v (ref eq)) (result (ref eq))
               (if (result (ref eq)) (ref.test (ref $Pair) (local.get $v))
                   (then (struct.get $Pair $d (ref.cast (ref $Pair) (local.get $v))))
                   (else (call $raise-pair-expected (local.get $v))
                         (unreachable))))


         (func $list? (param $v (ref eq)) (result (ref eq))
               (block $exit (result (ref eq))
                      (loop $loop
                            (if (ref.eq (local.get $v) (global.get $null))
                                (then (return (global.get $true))))
                            (if (ref.test (ref $Pair) (local.get $v))
                                (then
                                 (local.set $v
                                            (struct.get $Pair $d
                                                        (ref.cast (ref $Pair) (local.get $v))))
                                 (br $loop))
                                (else (return (global.get $false)))))
                      ;; fallthrough: not a proper list
                      (global.get $false)))

         (func $length/i32 (param $xs (ref eq)) (result i32)
               (local $i i32)
               (local.set $i (i32.const 0))
               (block $done
                      (loop $count
                            ;; if we've reached null, return the count so far
                            (if (ref.eq (local.get $xs) (global.get $null))
                                (then (return (local.get $i))))
                            ;; else, must be a pair: follow its cdr
                            (if (ref.test (ref $Pair) (local.get $xs))
                                (then (local.set $xs
                                                 (struct.get $Pair $d
                                                             (ref.cast (ref $Pair) (local.get $xs)))))
                                ;; neither null nor pair: error
                                (else (call $raise-pair-expected (local.get $xs))))
                            ;; increment and repeat
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $count)))
               ;; fall-through just returns the current count
               (local.get $i))

         (func $length (param $xs (ref eq)) (result (ref eq))
               (local $i i32)
               (local.set $i (i32.const 0))
               (block $done
                      (loop $count
                            (if (ref.eq (local.get $xs) (global.get $null))
                                (then (br $done)))
                            (if (ref.test (ref $Pair) (local.get $xs))
                                (then (local.set $xs
                                                 (struct.get $Pair $d
                                                             (ref.cast (ref $Pair) (local.get $xs)))))
                                (else (call $raise-pair-expected (local.get $xs))))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $count)))
               (ref.i31 (i32.shl (local.get $i) (i32.const 1))))

         
         ;; list-ref/checked: takes a proper Pair and unboxed i32 index, raises on out-of-bounds
         (func $list-ref/checked (param $xs (ref $Pair)) (param $i i32) (result (ref eq))
               (local $v   (ref $Pair))
               (local $k   i32)
               (local $len i32)
               ;; compute list length
               (local.set $len
                          (call $length/i32 (ref.cast (ref $Pair) (local.get $xs))))
               ;; bounds check: if i ≥ len, raise exception
               (if (i32.ge_u (local.get $i) (local.get $len))
                   (then (call $raise-bad-list-ref-index
                               (local.get $xs) (local.get $i) (local.get $len))))
               ;; traverse to the i-th element
               (local.set $v (local.get $xs))
               (local.set $k (local.get $i))
               (loop $loop
                     (if (i32.eqz (local.get $k))
                         (then (return (struct.get $Pair $a (local.get $v)))))
                     (local.set $v
                                (ref.cast (ref $Pair)
                                          (struct.get $Pair $d (local.get $v))))
                     (local.set $k (i32.sub (local.get $k) (i32.const 1)))
                     (br $loop))
               ;; should never fall through
               (unreachable))

         (func $list-ref (param $xs (ref eq)) (param $i (ref eq)) (result (ref eq))
               (local $idx i32)
               ;; Decode & check fixnum index
               (if (ref.test (ref i31) (local.get $i))
                   (then (local.set $idx
                                    (i31.get_u (ref.cast (ref i31) (local.get $i))))
                         (if (i32.ne (i32.and (local.get $idx) (i32.const 1)) (i32.const 0))
                             (then (call $raise-check-fixnum (local.get $i))))
                         (local.set $idx (i32.shr_u (local.get $idx) (i32.const 1))))
                   (else (call $raise-check-fixnum (local.get $i))))
               ;; Check & dispatch on Pair
               (if (result (ref eq))
                   (ref.test (ref $Pair) (local.get $xs))
                   (then (call $list-ref/checked
                               (ref.cast (ref $Pair) (local.get $xs))
                               (local.get $idx)))
                   (else (call $raise-pair-expected (local.get $xs))
                         (unreachable))))

         (func $list-tail/checked (param $xs (ref $Pair)) (param $i i32) (result (ref eq))
               (local $v    (ref $Pair))
               (local $k    i32)
               (local $len  i32)
               (local $next (ref eq))
               ;; compute length
               (local.set $len
                          (call $length/i32 (ref.cast (ref $Pair) (local.get $xs))))
               ;; bounds check (i > len ⇒ error)
               (if (i32.gt_u (local.get $i) (local.get $len))
                   (then (call $raise-bad-list-ref-index
                               (local.get $xs) (local.get $i) (local.get $len))))
               ;; traverse k steps
               (local.set $v (local.get $xs))
               (local.set $k (local.get $i))
               (loop $loop
                     (if (i32.eqz (local.get $k))
                         (then (return (local.get $v))))
                     ;; grab the raw cdr
                     (local.set $next
                                (struct.get $Pair $d (local.get $v)))
                     ;; ensure it's a Pair before casting
                     (if (ref.test (ref $Pair) (local.get $next))
                         (then (local.set $v (ref.cast (ref $Pair) (local.get $next))))
                         (else (call $raise-pair-expected (local.get $next))))
                     (local.set $k (i32.sub (local.get $k) (i32.const 1)))
                     (br $loop))
               (unreachable))


         (func $list-tail (param $xs (ref eq)) (param $i (ref eq)) (result (ref eq))
               (local $pair (ref $Pair))
               (local $idx  i32)
               (local.set $pair (global.get $dummy-pair))
               ;; decode and check fixnum index
               (if (ref.test (ref i31) (local.get $i))
                   (then
                    (local.set $idx (i31.get_u (ref.cast (ref i31) (local.get $i))))
                    (if (i32.ne (i32.and (local.get $idx) (i32.const 1)) (i32.const 0))
                        (then (call $raise-check-fixnum (local.get $i))))
                    (local.set $idx (i32.shr_u (local.get $idx) (i32.const 1))))
                   (else (call $raise-check-fixnum (local.get $i))))
               ;; check that xs is a Pair or null
               (if (ref.test (ref $Pair) (local.get $xs))
                   (then (local.set $pair (ref.cast (ref $Pair) (local.get $xs))))
                   (else (if (ref.eq (local.get $xs) (global.get $null))
                             (then (return (global.get $null)))
                             (else (call $raise-pair-expected (local.get $xs))))))
               ;; delegate to checked
               (call $list-tail/checked (local.get $pair) (local.get $idx)))

         (func $append (param $xs (ref eq)) (param $ys (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.eq (local.get $xs) (global.get $null))
                   (then (local.get $ys))  ; "the last list is used directly in the output"
                   (else (if (result (ref eq))
                             (ref.test (ref $Pair) (local.get $xs))
                             (then
                              (struct.new $Pair (i32.const 0)
                                          (struct.get $Pair $a (ref.cast (ref $Pair) (local.get $xs)))
                                          (call $append
                                                (struct.get $Pair $d (ref.cast (ref $Pair) (local.get $xs)))
                                                (local.get $ys))))
                             (else (call $raise-pair-expected (local.get $xs))
                                   (unreachable))))))

         (func $reverse (param $xs (ref eq)) (result (ref eq))
               (local $acc (ref eq))
               (local.set $acc (global.get $null))
               (block $done
                      (loop $rev
                            (if (ref.eq (local.get $xs) (global.get $null))
                                (then (return (local.get $acc))))
                            (if (ref.test (ref $Pair) (local.get $xs))
                                (then
                                 (local.set $acc
                                            (struct.new $Pair (i32.const 0)
                                                        (struct.get $Pair $a (ref.cast (ref $Pair) (local.get $xs)))
                                                        (local.get $acc)))
                                 (local.set $xs
                                            (struct.get $Pair $d (ref.cast (ref $Pair) (local.get $xs)))))
                                (else (call $raise-pair-expected (local.get $xs))))
                            (br $rev)))
               (unreachable))

         ; The original `alt-reverse` is defined `racket/private/reverse.rkt` and checks
         ; whether it is used in a module compiled in unsafe mode. If so, it skips
         ; the check that the input is a list.
         ; Here, for now, we simply have a copy of $reverse.
         ; Note: `alt-reverse` is used in the expansion of `for/list` loops.
         (func $alt-reverse (param $xs (ref eq)) (result (ref eq))
               (local $acc (ref eq))
               (local.set $acc (global.get $null))
               (block $done
                      (loop $rev
                            (if (ref.eq (local.get $xs) (global.get $null))
                                (then (return (local.get $acc))))
                            (if (ref.test (ref $Pair) (local.get $xs))
                                (then
                                 (local.set $acc
                                            (struct.new $Pair (i32.const 0)
                                                        (struct.get $Pair $a (ref.cast (ref $Pair) (local.get $xs)))
                                                        (local.get $acc)))
                                 (local.set $xs
                                            (struct.get $Pair $d (ref.cast (ref $Pair) (local.get $xs)))))
                                (else (call $raise-pair-expected (local.get $xs))))
                            (br $rev)))
               (unreachable))
         
         (func $memq (param $needle (ref eq)) (param $xs (ref eq)) (result (ref eq))
               (loop $search
                     ;; 1) end-of-list? => not found
                     (if (ref.eq (local.get $xs) (global.get $null))
                         (then (return (global.get $false))))
                     ;; 2) must be a Pair
                     (if (ref.test (ref $Pair) (local.get $xs))
                         (then
                          ;; compare needle to (car xs)
                          (if (ref.eq (local.get $needle)
                                      (struct.get $Pair $a
                                                  (ref.cast (ref $Pair) (local.get $xs))))
                              (then (return (local.get $xs)))    ;; found: return sublist
                              ;; else: fall through to step 3
                              ))
                         (else (call $raise-pair-expected (local.get $xs))))
                     ;; 3) advance to cdr
                     (local.set $xs
                                (struct.get $Pair $d (ref.cast (ref $Pair) (local.get $xs))))
                     (br $search))
               (unreachable))


         (func $make-list
               (param $n-raw (ref eq))    ;; fixnum
               (param $v     (ref eq))    ;; value to repeat
               (result       (ref eq))

               (local $n i32)

               ;; Check and unwrap fixnum
               (if (i32.or
                    (i32.eqz (ref.test (ref i31) (local.get $n-raw)))
                    (i32.ne (i32.and (i31.get_u (ref.cast (ref i31) (local.get $n-raw))) (i32.const 1))
                            (i32.const 0)))
                   (then (call $raise-argument-error (local.get $n-raw)))) ;; customize this if needed
               (local.set $n (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $n-raw))) (i32.const 1)))
               (call $make-list/checked (local.get $n) (local.get $v)))

         (func $make-list/checked
               (param $n i32)             ;; number of elements
               (param $v (ref eq))        ;; value to repeat
               (result (ref eq))          ;; proper list

               (local $i i32)
               (local $acc (ref eq))
               
               (local.set $i   (i32.const 0))
               (local.set $acc (global.get $null))
               (block $done
                      (loop $loop
                            (br_if $done (i32.ge_u (local.get $i) (local.get $n)))
                            (local.set $acc
                                       (struct.new $Pair
                                                   (i32.const 0)
                                                   (local.get $v)
                                                   (local.get $acc)))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $loop)))
               (local.get $acc))



         (func $raise-argument-error  (param $x (ref eq)) (unreachable))
         (func $raise-expected-fixnum (param $x (ref eq)) (unreachable))
         
         (func $list-from-range
               (param $start-raw (ref eq))   ;; inclusive, fixnum
               (param $end-raw   (ref eq))   ;; exclusive, fixnum
               (result (ref eq))

               (local $start-i31 (ref i31))
               (local $end-i31   (ref i31))
               (local $start     i32)
               (local $end       i32)

               ;; Check and unwrap start
               (if (i32.eqz (ref.test (ref i31) (local.get $start-raw)))
                   (then (call $raise-expected-fixnum (local.get $start-raw))))
               (local.set $start-i31 (ref.cast (ref i31) (local.get $start-raw)))
               (local.set $start (i32.shr_u (i31.get_u (local.get $start-i31)) (i32.const 1)))
               ;; Check and unwrap end
               (if (i32.eqz (ref.test (ref i31) (local.get $end-raw)))
                   (then (call $raise-expected-fixnum (local.get $end-raw))))
               (local.set $end-i31 (ref.cast (ref i31) (local.get $end-raw)))
               (local.set $end (i32.shr_u (i31.get_u (local.get $end-i31)) (i32.const 1)))
               ;; Delegate
               (call $list-from-range/checked (local.get $start) (local.get $end)))

         (func $list-from-range/checked
               (param $start i32)  ;; inclusive
               (param $end   i32)  ;; exclusive
               (result (ref eq))   ;; proper list of fixnums

               (local $i   i32)
               (local $lst (ref eq))  ;; initially null

               ;; Start from end and build backwards
               (local.set $i   (local.get $end))
               (local.set $lst (global.get $null))
               (block $done
                      (loop $loop
                            (br_if $done (i32.le_s (local.get $i) (local.get $start)))
                            ;; Decrement i
                            (local.set $i (i32.sub (local.get $i) (i32.const 1)))
                            ;; Prepend (ref.i31 (i32.shl $i 1)) as fixnum
                            (local.set $lst
                                       (struct.new $Pair
                                                   (i32.const 0)  ;; hash
                                                   (ref.i31 (i32.shl (local.get $i) (i32.const 1)))
                                                   (local.get $lst)))
                            (br $loop)))
               (local.get $lst))




         ;; - Vectors
         ;; (type $Vector (sub $Heap
         ;;                      (struct
         ;;                        (field $hash (mut i32))
         ;;                        (field (ref $Array))))))

         ; The global $dummy-vector is needed when a non-nullable local variable needs
         ; initialization to a default.
         (global $dummy-array (ref $Array) (array.new $Array (global.get $false) (i32.const 0)))         
         (global $dummy-vector (ref $Vector)
                 (struct.new $Vector
                             (i32.const 0)          ;; hash
                             (global.get $dummy-array)))

         ;; Vector related exceptions
         (func $raise-check-vector (param $x (ref eq)) (unreachable))
         (func $raise-check-fixnum (param $x (ref eq)) (unreachable))
         (func $raise-bad-vector-ref-index
               (param $v (ref $Vector)) (param $i i32) (param $len i32)
               (unreachable))
         (func $raise-bad-vector-copy-range
               (param (ref $Vector)) (param i32) (param (ref $Vector)) (param i32) (param i32)
               (unreachable))
         (func $raise-bad-vector-take-index
               (param (ref $Vector)) (param i32) (param i32)
               (unreachable))


         (func $raise-make-vector:bad-length (unreachable))
         

         (func $make-vector
               (param $k-fx   (ref eq))  ;; fixnum
               (param $val    (ref eq))  ;; optional, defaults to 0
               (result        (ref eq))

               (local $k i32)
               (local $v (ref eq))  ;; possibly rewritten value
               ;; --- Type check for fixnum ---
               (if (i32.eqz (ref.test (ref i31) (local.get $k-fx)))
                   (then (call $raise-make-vector:bad-length)))
               ;; --- Decode fixnum to i32 ---
               (local.set $k (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $k-fx)))
                                        (i32.const 1)))
               ;; --- Substitute default value if missing ---
               (local.set $v (if (result (ref eq))
                                 (ref.eq (local.get $val) (global.get $missing))
                                 (then (global.get $zero))
                                 (else (local.get $val))))
               ;; --- Delegate to checked version ---
               (call $make-vector/checked (local.get $k) (local.get $v)))

         (func $make-vector/checked
               (param $k   i32)        ;; number of elements
               (param $val (ref eq))   ;; initial value
               (result     (ref $Vector))

               (local $arr (ref $Array))
               ;; Create the array
               (local.set $arr (array.new $Array (local.get $val) (local.get $k)))
               ;; Construct and return the vector
               (struct.new $Vector
                           (i32.const 0)    ;; hash = 0
                           (local.get $arr)))

         
         (func $vector-length (param $v (ref eq)) (result (ref eq))
               (local $vec (ref $Vector))
               (if (result (ref eq))
                   (ref.test (ref $Vector) (local.get $v))
                   (then (local.set $vec (ref.cast (ref $Vector) (local.get $v)))
                         (ref.i31
                          (i32.shl
                           (array.len
                            (struct.get $Vector $arr (local.get $vec)))
                           (i32.const 1))))
                   (else (call $raise-check-vector (local.get $v))
                         (unreachable))))


         (func $vector-length/i32 (param $v (ref eq)) (result i32)
               (local $vec (ref $Vector))
               (if (result i32)
                   (ref.test (ref $Vector) (local.get $v))
                   (then (local.set $vec (ref.cast (ref $Vector) (local.get $v)))
                         (array.len (struct.get $Vector $arr (local.get $vec))))
                   (else (call $raise-check-vector (local.get $v))
                         (unreachable))))

         (func $vector-length/checked/i32 (param $v (ref $Vector)) (result i32)
               (array.len (struct.get $Vector $arr (local.get $v))))

         (func $vector?/i32 (param $a (ref eq)) (result i32)
               (ref.test (ref $Vector) (local.get $a)))

         (func $vector? (param $a (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.test (ref $Vector) (local.get $a))
                   (then (global.get $true))
                   (else (global.get $false))))

         
         (func $vector-ref/checked
               (param $a (ref $Vector)) (param $i i32)
               (result (ref eq))
               (local $len i32)
               ;; get length
               (local.set $len (array.len (struct.get $Vector $arr (local.get $a))))
               ;; bounds check
               (if (result (ref eq))
                   (i32.lt_u (local.get $i) (local.get $len))
                   (then (array.get $Array
                                    (struct.get $Vector $arr (local.get $a))
                                    (local.get $i)))
                   (else (call $raise-bad-vector-ref-index
                               (local.get $a) (local.get $i) (local.get $len))
                         (unreachable))))

         (func $vector-ref
               (param $v (ref eq)) (param $i (ref eq))
               (result (ref eq))
               (local $vec (ref $Vector))
               (local $idx i32)
               (local $len i32)
               ;; Initialize vec to dummy to satisfy non-nullable default requirement
               (local.set $vec (global.get $dummy-vector))
               ;; Check that $v is a vector
               (if (ref.test (ref $Vector) (local.get $v))
                   (then (local.set $vec (ref.cast (ref $Vector) (local.get $v))))
                   (else (call $raise-check-vector (local.get $v))))
               ;; Check that $i is an i31 and decode fixnum
               (if (ref.test (ref i31) (local.get $i))
                   (then
                    (local.set $idx
                               (i31.get_u (ref.cast (ref i31) (local.get $i))))
                    (if (i32.eqz (i32.and (local.get $idx) (i32.const 1)))
                        (then (local.set $idx (i32.shr_u (local.get $idx) (i32.const 1))))
                        (else (call $raise-check-fixnum (local.get $i)))))
                   (else (call $raise-check-fixnum (local.get $i))))
               ;; Get array length
               (local.set $len (array.len (struct.get $Vector $arr (local.get $vec))))
               ;; Bounds check
               (if (result (ref eq))
                   (i32.lt_u (local.get $idx) (local.get $len))
                   (then (return
                          (array.get $Array
                                     (struct.get $Vector $arr (local.get $vec))
                                     (local.get $idx))))
                   (else (call $raise-bad-vector-ref-index
                               (local.get $vec) (local.get $idx) (local.get $len))
                         (unreachable))))


         (func $vector-set!/checked
               (param $vec (ref $Vector)) (param $i i32) (param $val (ref eq))
               (local $len i32)

               (local.set $len (array.len (struct.get $Vector $arr (local.get $vec))))
               (if (i32.lt_u (local.get $i) (local.get $len))
                   (then (array.set $Array
                                    (struct.get $Vector $arr (local.get $vec))
                                    (local.get $i)
                                    (local.get $val)))
                   (else (call $raise-bad-vector-ref-index
                               (local.get $vec) (local.get $i) (local.get $len))
                         (unreachable))))

         (func $vector-set!
               (param $v (ref eq)) (param $i (ref eq)) (param $val (ref eq))
               (result (ref eq))
               (local $vec (ref $Vector))
               (local $idx i32)
               (local $len i32)
               ;; Initialize $vec with dummy to satisfy non-nullable restriction
               (local.set $vec (global.get $dummy-vector))
               ;; 1. Check $v is a vector
               (if (ref.test (ref $Vector) (local.get $v))
                   (then (local.set $vec (ref.cast (ref $Vector) (local.get $v))))
                   (else (call $raise-check-vector (local.get $v))))
               ;; 2. Check $i is a fixnum
               (if (ref.test (ref i31) (local.get $i))
                   (then (local.set $idx
                                    (i31.get_u (ref.cast (ref i31) (local.get $i))))
                         (if (i32.eqz (i32.and (local.get $idx) (i32.const 1)))
                             (then (local.set $idx (i32.shr_u (local.get $idx) (i32.const 1))))
                             (else (call $raise-check-fixnum (local.get $i)))))
                   (else (call $raise-check-fixnum (local.get $i))))
               ;; 3. Get length
               (local.set $len (array.len (struct.get $Vector $arr (local.get $vec))))
               ;; 4. Bounds check and set
               (if (result (ref eq))
                   (i32.lt_u (local.get $idx) (local.get $len))
                   (then (array.set $Array
                                    (struct.get $Vector $arr (local.get $vec))
                                    (local.get $idx)
                                    (local.get $val))
                         (global.get $void))
                   (else (call $raise-bad-vector-ref-index
                               (local.get $vec) (local.get $idx) (local.get $len))
                         (unreachable))))

         (func $vector-fill! (param $v (ref eq)) (param $x (ref eq)) (result (ref eq))
               (local $vec (ref $Vector))
               (local.set $vec (global.get $dummy-vector))
               (if (ref.test (ref $Vector) (local.get $v))
                   (then (local.set $vec (ref.cast (ref $Vector) (local.get $v))))
                   (else (call $raise-check-vector (local.get $v))))
               (call $array-fill! (struct.get $Vector $arr (local.get $vec)) (local.get $x))
               (global.get $void))

         (func $vector-copy!
               (param $dest       (ref eq))
               (param $dest-start (ref eq))   ;; fixnum
               (param $src        (ref eq))
               (param $src-start  (ref eq))   ;; fixnum or $missing, default: 0)
               (param $src-end    (ref eq))   ;; fixnum or $missing, default: (vector-length src)
               (result            (ref eq))
               
               (local $d       (ref $Vector))
               (local $s       (ref $Vector))
               (local $ds      i32)
               (local $ss      i32)
               (local $se      i32)
               (local $src-len i32)

               ;; --- Validate $dest ---
               (if (i32.eqz (ref.test (ref $Vector) (local.get $dest)))
                   (then (call $raise-check-vector (local.get $dest))))
               ;; --- Validate $src ---
               (if (i32.eqz (ref.test (ref $Vector) (local.get $src)))
                   (then (call $raise-check-vector (local.get $src))))
               ;; --- Validate $dest-start ---
               (if (i32.eqz (ref.test (ref i31) (local.get $dest-start)))
                   (then (call $raise-check-fixnum (local.get $dest-start))))
               (if (i32.and (i31.get_u (ref.cast (ref i31) (local.get $dest-start)))
                            (i32.const 1))
                   (then (call $raise-check-fixnum (local.get $dest-start))))
               ;; --- Validate $src-start ---
               (if (i32.eqz (ref.eq (local.get $src-start) (global.get $missing)))
                   (then (if (i32.eqz (ref.test (ref i31) (local.get $src-start)))
                             (then (call $raise-check-fixnum (local.get $src-start))))
                         (if (i32.and (i31.get_u (ref.cast (ref i31) (local.get $src-start)))
                                      (i32.const 1))
                        (then (call $raise-check-fixnum (local.get $src-start))))))
               ;; --- Validate $src-end ---
               (if (i32.eqz (ref.eq (local.get $src-end) (global.get $missing)))
                   (then (if (i32.eqz (ref.test (ref i31) (local.get $src-end)))
                             (then (call $raise-check-fixnum (local.get $src-end))))
                         (if (i32.and (i31.get_u (ref.cast (ref i31) (local.get $src-end)))
                                      (i32.const 1))
                             (then (call $raise-check-fixnum (local.get $src-end))))))
               ;; --- Cast and decode after validation ---
               (local.set $d  (ref.cast (ref $Vector) (local.get $dest)))
               (local.set $s  (ref.cast (ref $Vector) (local.get $src)))
               (local.set $ds (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $dest-start)))
                                         (i32.const 1)))
               
               (local.set $src-len (array.len (struct.get $Vector $arr (local.get $s))))

               (if (ref.eq (local.get $src-start) (global.get $missing))
                   (then (local.set $ss (i32.const 0)))
                   (else (local.set $ss (i32.shr_u (i31.get_u (ref.cast (ref i31)
                                                                        (local.get $src-start)))
                                                   (i32.const 1)))))
               
               (if (ref.eq (local.get $src-end) (global.get $missing))
                   (then (local.set $se (local.get $src-len)))
                   (else (local.set $se (i32.shr_u (i31.get_u (ref.cast (ref i31)
                                                                        (local.get $src-end)))
                                                   (i32.const 1)))))
               ;; --- Delegate to checked copy ---
               (call $vector-copy!/checked (local.get $d) (local.get $ds)
                     (local.get $s) (local.get $ss)
                     (local.get $se)))

         (func $vector-copy!/checked
               (param $dest (ref $Vector)) (param $ds i32)
               (param $src  (ref $Vector)) (param $ss i32) (param $se i32)               
               (result (ref eq))
               
               (local $src-len i32)
               (local $dest-len i32)
               (local.set $src-len  (array.len (struct.get $Vector $arr (local.get $src))))
               (local.set $dest-len (array.len (struct.get $Vector $arr (local.get $dest))))               
               (if (i32.or
                    (i32.or (i32.gt_u (local.get $ss) (local.get $src-len))
                            (i32.gt_u (local.get $se) (local.get $src-len)))
                    (i32.gt_u (i32.add (local.get $ds) (i32.sub (local.get $se) (local.get $ss))) (local.get $dest-len)))
                   (then (call $raise-bad-vector-copy-range
                               (local.get $dest) (local.get $ds)
                               (local.get $src) (local.get $ss) (local.get $se))
                         (unreachable)))

               (call $array-copy!
                     (struct.get $Vector $arr (local.get $dest)) (local.get $ds)
                     (struct.get $Vector $arr (local.get $src))  (local.get $ss) (local.get $se))
               (global.get $void))

         (func $vector-empty? (param $v (ref eq)) (result (ref eq))
               (local $vec (ref $Vector))
               (local $len i32)
               (if (result (ref eq))
                   (ref.test (ref $Vector) (local.get $v))
                   (then (local.set $vec (ref.cast (ref $Vector) (local.get $v)))
                         (local.set $len (array.len (struct.get $Vector $arr (local.get $vec))))
                         (if (result (ref eq))
                             (i32.eqz (local.get $len))
                             (then (global.get $true))
                             (else (global.get $false))))
                   (else (call $raise-check-vector (local.get $v))
                         (unreachable))))

         (func $vector-append (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
               (local $va (ref $Vector))
               (local $vb (ref $Vector))
               (local.set $va (global.get $dummy-vector))
               (local.set $vb (global.get $dummy-vector))
               (if (ref.test (ref $Vector) (local.get $a))
                   (then (local.set $va (ref.cast (ref $Vector) (local.get $a))))
                   (else (call $raise-check-vector (local.get $a))))
               (if (ref.test (ref $Vector) (local.get $b))
                   (then (local.set $vb (ref.cast (ref $Vector) (local.get $b))))
                   (else (call $raise-check-vector (local.get $b))))

               (struct.new $Vector (i32.const 0)
                           (call $array-append
                                 (struct.get $Vector $arr (local.get $va))
                                 (struct.get $Vector $arr (local.get $vb)))))

         (func $vector-take (param $v (ref eq)) (param $i (ref eq)) (result (ref eq))
               (local $vec (ref $Vector))
               (local $ix i32)
               (local $len i32)
               (local.set $vec (global.get $dummy-vector))
               (if (ref.test (ref $Vector) (local.get $v))
                   (then (local.set $vec (ref.cast (ref $Vector) (local.get $v))))
                   (else (call $raise-check-vector (local.get $v))))
               (if (ref.test (ref i31) (local.get $i))
                   (then (local.set $ix (i31.get_u (ref.cast (ref i31) (local.get $i))))
                         (if (i32.eqz (i32.and (local.get $ix) (i32.const 1)))
                             (then (local.set $ix (i32.shr_u (local.get $ix) (i32.const 1))))
                             (else (call $raise-check-fixnum (local.get $i)))))
                   (else (call $raise-check-fixnum (local.get $i))))
               (local.set $len (array.len (struct.get $Vector $arr (local.get $vec))))
               (if (i32.gt_u (local.get $ix) (local.get $len))
                   (then (call $raise-bad-vector-take-index (local.get $vec) (local.get $ix) (local.get $len))
                         (unreachable)))
               (struct.new $Vector (i32.const 0)
                           (call $array-take (struct.get $Vector $arr (local.get $vec)) (local.get $ix))))

         (func $vector-drop (param $v (ref eq)) (param $i (ref eq)) (result (ref eq))
               (local $vec (ref $Vector))
               (local $ix i32)
               (local $len i32)
               (local.set $vec (global.get $dummy-vector))
               (if (ref.test (ref $Vector) (local.get $v))
                   (then (local.set $vec (ref.cast (ref $Vector) (local.get $v))))
                   (else (call $raise-check-vector (local.get $v))))
               (if (ref.test (ref i31) (local.get $i))
                   (then (local.set $ix (i31.get_u (ref.cast (ref i31) (local.get $i))))
                         (if (i32.eqz (i32.and (local.get $ix) (i32.const 1)))
                             (then (local.set $ix (i32.shr_u (local.get $ix) (i32.const 1))))
                             (else (call $raise-check-fixnum (local.get $i)))))
                   (else (call $raise-check-fixnum (local.get $i))))
               (local.set $len (array.len (struct.get $Vector $arr (local.get $vec))))
               (if (i32.gt_u (local.get $ix) (local.get $len))
                   (then (call $raise-bad-vector-take-index (local.get $vec) (local.get $ix) (local.get $len))
                         (unreachable)))
               (struct.new $Vector (i32.const 0)
                           (call $array-drop (struct.get $Vector $arr (local.get $vec)) (local.get $ix))))

         (func $vector-drop-right (param $v (ref eq)) (param $i (ref eq)) (result (ref eq))
               (local $vec (ref $Vector))
               (local $ix i32)
               (local $len i32)
               (local.set $vec (global.get $dummy-vector))
               (if (ref.test (ref $Vector) (local.get $v))
                   (then (local.set $vec (ref.cast (ref $Vector) (local.get $v))))
                   (else (call $raise-check-vector (local.get $v))))
               (if (ref.test (ref i31) (local.get $i))
                   (then (local.set $ix (i31.get_u (ref.cast (ref i31) (local.get $i))))
                         (if (i32.eqz (i32.and (local.get $ix) (i32.const 1)))
                             (then (local.set $ix (i32.shr_u (local.get $ix) (i32.const 1))))
                             (else (call $raise-check-fixnum (local.get $i)))))
                   (else (call $raise-check-fixnum (local.get $i))))
               (local.set $len (array.len (struct.get $Vector $arr (local.get $vec))))
               (if (i32.gt_u (local.get $ix) (local.get $len))
                   (then (call $raise-bad-vector-take-index (local.get $vec) (local.get $ix) (local.get $len))
                         (unreachable)))
               (struct.new $Vector (i32.const 0)
                           (call $array-drop-right (struct.get $Vector $arr (local.get $vec)) (local.get $ix))))

         (func $vector-split-at (param $v (ref eq)) (param $i (ref eq)) (result (ref eq))
               (local $vec (ref $Vector))
               (local $ix  i32)
               (local $len i32)
               (local $res (ref $Array))

               ; 1. Check $v is a vector
               (local.set $vec (global.get $dummy-vector))
               (if (ref.test (ref $Vector) (local.get $v))
                   (then (local.set $vec (ref.cast (ref $Vector) (local.get $v))))
                   (else (call $raise-check-vector (local.get $v))))
               ; 2. Check $i is a fixnum 
               (if (ref.test (ref i31) (local.get $i))
                   (then (local.set $ix (i31.get_u (ref.cast (ref i31) (local.get $i))))
                         (if (i32.eqz (i32.and (local.get $ix) (i32.const 1)))
                             (then (local.set $ix (i32.shr_u (local.get $ix) (i32.const 1))))
                             (else (call $raise-check-fixnum (local.get $i)))))
                   (else (call $raise-check-fixnum (local.get $i))))
               ; 3. Range check
               (local.set $len (array.len (struct.get $Vector $arr (local.get $vec))))              
               (if (i32.gt_u (local.get $ix) (local.get $len))
                   (then (call $raise-bad-vector-take-index (local.get $vec) (local.get $ix) (local.get $len))
                         (unreachable)))
               ; 4. Split the vector
               (local.set $res (call $array-split-at (struct.get $Vector $arr (local.get $vec)) (local.get $ix)))
               (struct.new $Vector (i32.const 0) (local.get $res)))
         

         (func $raise-expected-vector (unreachable))
         
         (func $vector->list
               (param $v (ref eq))
               (result (ref eq))
               (if (i32.eqz (ref.test (ref $Vector) (local.get $v)))
                   (then (call $raise-expected-vector (local.get $v)) (unreachable)))
               (call $vector->list/checked (ref.cast (ref $Vector) (local.get $v))))

         (func $vector->list/checked
               (param $v (ref $Vector))
               (result   (ref eq))

               (local $arr (ref $Array))
               (local $i   i32)         ;; current index (starts from len - 1 and decrements)
               (local $x   (ref eq))
               (local $xs  (ref eq))
               ;; Extract backing array and initialize
               (local.set $arr (struct.get $Vector $arr (local.get $v)))
               (local.set $i   (i32.sub (array.len (local.get $arr)) (i32.const 1)))
               (local.set $xs  (global.get $null))
               ;; Loop backwards
               (block $done
                      (loop $loop
                            (br_if $done (i32.lt_s (local.get $i) (i32.const 0)))
                            (local.set $x (array.get $Array (local.get $arr) (local.get $i)))
                            (local.set $xs (struct.new $Pair
                                                       (i32.const 0)
                                                       (local.get $x)
                                                       (local.get $xs)))
                            (local.set $i (i32.sub (local.get $i) (i32.const 1)))
                            (br $loop)))
               (return (local.get $xs)))



         
         ;;;
         ;;; Strings
         ;;;

         (func $raise-check-string         (param $x (ref eq))                      (unreachable))
         (func $raise-bad-string-index     (param $x (ref eq)) (param $i (ref eq))  (unreachable))
         (func $raise-bad-string-index/i32 (param $x (ref eq)) (param $i i32)       (unreachable))
         
         (func $raise-string-index-out-of-bounds/i32 (param $x (ref eq)) (param $i i32) (param $n i32)
               (unreachable))
                  
         (func $string? (param $s (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.test (ref $String) (local.get $s))
                   (then (global.get $true))
                   (else (global.get $false))))

         ;; Constructors

         (func $raise-make-string:bad-length       (unreachable))
         (func $raise-make-string:bad-char         (unreachable))
         (func $raise-argument-error:char-expected (unreachable))
         
         ; for single character strings
         (func $codepoint->string
               (param  $ch i32)                    ;; Unicode scalar value
               (result     (ref $String))

               (local $cp  (ref $I32Array))
               ;; Validate that ch is a Unicode scalar (i.e. not in surrogate range)
               (if (i32.or (i32.lt_u (local.get $ch) (i32.const 0))
                           (i32.gt_u (local.get $ch) (i32.const 0x10FFFF)))
                   (then (call $raise-argument-error:char-expected (ref.i31 (local.get $ch)))
                         (unreachable)))
               (if (i32.and (i32.ge_u (local.get $ch) (i32.const 0xD800))
                            (i32.le_u (local.get $ch) (i32.const 0xDFFF)))
                   (then (call $raise-argument-error:char-expected (ref.i31 (local.get $ch)))
                         (unreachable)))
               ;; Allocate I32Array of length 1
               (local.set $cp (array.new_fixed $I32Array 1 (local.get $ch)))
               ;; Create string with immutable = 1, hash = 0
               (struct.new $String
                           (i32.const 0)       ;; hash (lazy)
                           (i32.const 1)       ;; immutable
                           (local.get $cp)))

         
         
         (func $make-string
               (param $n-raw  (ref eq))    ;; fixnum
               (param $ch-raw (ref eq))    ;; immediate character
               (result        (ref eq))

               (local $n          i32)
               (local $ch-tagged  i32)
               (local $ch         i32)
               ;; --- Type checks ---
               (if (i32.eqz (ref.test (ref i31) (local.get $n-raw)))
                   (then (call $raise-make-string:bad-length)))
               (if (i32.eqz (ref.test (ref i31) (local.get $ch-raw)))
                   (then (call $raise-make-string:bad-char)))
               ;; --- Decode ---
               (local.set $n         (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $n-raw))) (i32.const 1)))
               (local.set $ch-tagged (i31.get_u (ref.cast (ref i31) (local.get $ch-raw))))
               ;; --- Validate character tag ---
               (if (i32.ne (i32.and (local.get $ch-tagged) (i32.const ,char-mask)) (i32.const ,char-tag))
                   (then (call $raise-make-string:bad-char)))
               ;; --- Extract code point from character ---
               (local.set $ch (i32.shr_u (local.get $ch-tagged) (i32.const ,char-shift)))
               ;; --- Delegate ---
               (call $make-string/checked (local.get $n) (local.get $ch)))

         (func $make-string/checked
               (param $n  i32)      ;; number of characters
               (param $ch i32)      ;; codepoint to fill
               (result    (ref $String))

               (local $arr (ref $I32Array))
               ;; Create backing array and fill with `ch`
               (local.set $arr (array.new $I32Array (local.get $ch) (local.get $n)))
               ;; Construct mutable string
               (struct.new $String
                           (i32.const 0)     ;; hash = 0
                           (i32.const 0)     ;; mutable
                           (local.get $arr)))

         (func $make-immutable-string/checked
               (param $n  i32)      ;; number of characters
               (param $ch i32)      ;; codepoint to fill
               (result    (ref $String))

               (local $arr (ref $I32Array))
               ;; Create backing array and fill with `ch`
               (local.set $arr (array.new $I32Array (local.get $ch) (local.get $n)))
               ;; Construct mutable string
               (struct.new $String
                           (i32.const 0)     ;; hash = 0
                           (i32.const 1)     ;; immutable
                           (local.get $arr)))


         (func $i32array->string (param $arr (ref $I32Array)) (result (ref $String))
               ;; Constructs a $String from an I32Array of codepoints.
               ;; Assumes the string is mutable and un-hashed (hash = 0).
               (struct.new $String
                           (i32.const 0)            ;; hash = 0
                           (i32.const 0)            ;; mutable 
                           (local.get $arr)))       ;; codepoints

         (func $i32array->immutable-string (param $arr (ref $I32Array)) (result (ref $String))
               ;; Constructs a $String from an I32Array of codepoints.
               ;; Assumes the string is immutable and un-hashed (hash = 0).
               (struct.new $String
                           (i32.const 0)            ;; hash = 0
                           (i32.const 1)            ;; immutable
                           (local.get $arr)))       ;; codepoints

         (func $i32growable-array->string (param $g (ref $I32GrowableArray)) (result (ref $String))
               (call $i32array->string
                     (call $i32growable-array->array (local.get $g))))

         (func $i32growable-array->immutable-string (param $g (ref $I32GrowableArray)) (result (ref $String))
               (call $i32array->immutable-string
                     (call $i32growable-array->array (local.get $g))))

         (func $make-dummy-string (result (ref $String))
               (struct.new $String
                           (i32.const 0)  ;; hash = 0
                           (i32.const 0)  ;; mutable
                           (call $i32array-make (i32.const 0) (i32.const 0))))
         
         (func $string->immutable-string (param $s (ref eq)) (result (ref eq))
               (local $str (ref $String))
               (local.set $str (call $make-dummy-string))
               ;; 1. Check that s is a String
               (if (ref.test (ref $String) (local.get $s))
                   (then (call $raise-check-string (local.get $s))))
               (local.set $str (ref.cast (ref $String) (local.get $s)))
               ;; 2. If already immutable, return it directly
               (if (result (ref eq))
                   (i32.eq (struct.get $String $immutable (local.get $str)) (i32.const 1))
                   (then (local.get $str))
                   (else
                    ;; Otherwise, create a new immutable copy
                    (struct.new $String
                                ; Note: now the mutable and immutable string gets the same hash code.
                                ;       Okay?
                                (struct.get $String $hash (local.get $str))  ;; inherit hash
                                (i32.const 1)                                ;; immutable
                                (call $i32array-copy
                                      (struct.get $String $codepoints (local.get $str))
                                      (i32.const 0)
                                      (call $i32array-length (struct.get $String $codepoints (local.get $str))))))))

         (func $raise-string-length:bad-argument (unreachable))
         
         (func $string-length
               (param $s-raw (ref eq))
               (result (ref eq))
               
               (local $s   (ref $String))
               (local $len i32)
               ;; --- Type check ---
               (if (i32.eqz (ref.test (ref $String) (local.get $s-raw)))
                   (then (call $raise-string-length:bad-argument)))
               ;; --- Decode ---
               (local.set $s (ref.cast (ref $String) (local.get $s-raw)))
               ;; --- Compute ---
               (local.set $len (call $i32array-length
                                     (struct.get $String $codepoints (local.get $s))))
               ;; --- Return fixnum ---
               (ref.i31 (i32.shl (local.get $len) (i32.const 1))))


         (func $string-length/checked/i32
               (param $s (ref $String))
               (result i32)
               ;; Fetch the length of the codepoints array
               (call $i32array-length (struct.get $String $codepoints (local.get $s))))
         

         (func $string-ref
               (param $s (ref eq))
               (param $i (ref eq))
               (result (ref eq))

               (local $str     (ref $String))
               (local $idx-i31 (ref i31))
               (local $idx     i32)

               ;; --- All tests ---
               (if (i32.eqz (ref.test (ref $String) (local.get $s)))
                   (then (call $raise-check-string (local.get $s))))
               
               (if (i32.eqz (ref.test (ref i31) (local.get $i)))
                   (then (call $raise-check-fixnum (local.get $i))))

               (local.set $idx-i31 (ref.cast (ref i31) (local.get $i)))
               (local.set $idx (i31.get_u (local.get $idx-i31)))
               (if (i32.ne (i32.and (local.get $idx) (i32.const 1)) (i32.const 0))
                   (then (call $raise-check-fixnum (local.get $i))))

               ;; --- All decoding ---
               (local.set $str (ref.cast (ref $String) (local.get $s)))
               (local.set $idx (i32.shr_u (local.get $idx) (i32.const 1)))

               ;; --- Bounds check and delegate ---
               (if (result (ref eq))
                   (i32.lt_u (local.get $idx)
                             (call $string-length/checked/i32 (local.get $str)))
                   (then (call $string-ref/checked (local.get $str) (local.get $idx)))
                   (else (call $raise-bad-string-index/i32 (local.get $s) (local.get $idx))
                         (unreachable))))

         

         (func $string-ref/checked   ; unsafe: no bounds check
               (param $str (ref $String))
               (param $idx i32)
               (result     (ref eq))

               (local $arr (ref $I32Array))
               (local $cp  i32)

               (local.set $arr (struct.get $String $codepoints (local.get $str)))
               (local.set $cp  (call $i32array-ref (local.get $arr) (local.get $idx)))
               ;; Return (char): (cp << char-shift) | char-tag
               (ref.i31 (i32.or (i32.shl (local.get $cp) (i32.const ,char-shift))
                                (i32.const ,char-tag))))

         (func $string-ref/checked/i32   ; unsafe: no bounds check
               (param $str (ref $String))
               (param $idx i32)
               (result     i32)

               (local $arr (ref $I32Array))
               (local $cp  i32)

               (local.set $arr (struct.get $String $codepoints (local.get $str)))
               (local.set $cp  (call $i32array-ref (local.get $arr) (local.get $idx)))
               (local.get $cp))


         (func $raise-string-set!:string-expected  (param $x (ref eq)) (unreachable))
         (func $raise-string-set!:fixnum-expected  (param $x (ref eq)) (unreachable))
         (func $raise-string-set!:char-expected    (param $x (ref eq)) (unreachable))
         (func $raise-string-set!:string-immutable (param $x (ref eq)) (unreachable))
         
         (func $string-set!
               (param $s   (ref eq))
               (param $i   (ref eq))
               (param $ch  (ref eq))
               (result     (ref eq))

               (local $str     (ref $String))
               (local $arr     (ref $I32Array))
               (local $idx     i32)
               (local $cp      i32)
               (local $tagged  i32)
               ;; --- Type checks ---
               (if (i32.eqz (ref.test (ref $String) (local.get $s)))
                   (then (call $raise-string-set!:string-expected (local.get $s))))
               (if (i32.eqz (ref.test (ref i31) (local.get $i)))
                   (then (call $raise-string-set!:fixnum-expected (local.get $i))))
               (if (i32.eqz (ref.test (ref i31) (local.get $ch)))
                   (then (call $raise-string-set!:char-expected (local.get $ch))))
               ;; --- Decode ---
               (local.set $str (ref.cast (ref $String) (local.get $s)))
               ;; Check for immutability
               (if (i32.eq (struct.get $String $immutable (local.get $str)) (i32.const 1))
                   (then (call $raise-string-set!:string-immutable (local.get $s))))
               ;; Decode and validate fixnum index
               (local.set $idx (i31.get_u (ref.cast (ref i31) (local.get $i))))
               (if (i32.eqz (i32.and (local.get $idx) (i32.const 1)))
                   (then (local.set $idx (i32.shr_u (local.get $idx) (i32.const 1))))
                   (else (call $raise-string-set!:fixnum-expected (local.get $i))))
               ;; Decode and validate character
               (local.set $tagged (i31.get_u (ref.cast (ref i31) (local.get $ch))))
               (if (i32.ne (i32.and (local.get $tagged) (i32.const ,char-mask)) (i32.const ,char-tag))
                   (then (call $raise-string-set!:char-expected (local.get $ch))))
               (local.set $cp (i32.shr_u (local.get $tagged) (i32.const ,char-shift)))
               ;; --- Bounds-check and write ---
               (local.set $arr (struct.get $String $codepoints (local.get $str)))
               (if (i32.lt_u (local.get $idx) (call $i32array-length (local.get $arr)))
                   (then (call $i32array-set! (local.get $arr) (local.get $idx) (local.get $cp))
                         ;; Reset hash to 0
                         (struct.set $String $hash (local.get $str) (i32.const 0))
                         (return (global.get $void)))
                   (else (call $raise-bad-string-index/i32 (local.get $s) (local.get $idx))))
               (unreachable))

         (func $substring (param $s (ref eq)) (param $start (ref eq)) (param $end (ref eq)) (result (ref eq))
               (local $str      (ref null $String))
               (local $arr      (ref $I32Array))
               (local $i32start i32)
               (local $i32end   i32)
               (local $len      i32)
               ;; check string
               (if (ref.test (ref $String) (local.get $s))
                   (then (local.set $str (ref.cast (ref $String) (local.get $s))))
                   (else (call $raise-check-string (local.get $s))))
               ;; decode and check start index
               (if (ref.test (ref i31) (local.get $start))
                   (then (local.set $i32start (i31.get_u (ref.cast (ref i31) (local.get $start))))
                         (if (i32.ne (i32.and (local.get $i32start) (i32.const 1)) (i32.const 0))
                             (then (call $raise-check-fixnum (local.get $start))))
                         (local.set $i32start (i32.shr_u (local.get $i32start) (i32.const 1))))
                   (else (call $raise-check-fixnum (local.get $start))))
               ;; decode and check end index
               (if (ref.test (ref i31) (local.get $end))
                   (then (local.set $i32end (i31.get_u (ref.cast (ref i31) (local.get $end))))
                         (if (i32.ne (i32.and (local.get $i32end) (i32.const 1)) (i32.const 0))
                             (then (call $raise-check-fixnum (local.get $end))))
                         (local.set $i32end (i32.shr_u (local.get $i32end) (i32.const 1))))
                   (else (call $raise-check-fixnum (local.get $end))))
               ;; get array and length
               (local.set $arr (struct.get $String $codepoints (local.get $str)))
               (local.set $len (call $i32array-length (local.get $arr)))
               ;; bounds check: start <= end <= len
               (if (i32.or (i32.gt_u (local.get $i32start) (local.get $i32end))
                           (i32.gt_u (local.get $i32end) (local.get $len)))
                   (then (call $raise-string-index-out-of-bounds/i32 (local.get $s) (local.get $i32end) (local.get $len))))
               ;; create new string
               (struct.new $String
                           (i32.const 0) ; hash
                           (i32.const 0) ; mutable (also for immutable input)
                           (call $i32array-copy (local.get $arr) (local.get $i32start) (local.get $i32end))))

         (func $string-copy
               (param $s (ref eq))
               (result (ref eq))

               (local $str (ref $String))
               (local $arr (ref $I32Array))
               (local $len i32)
               (local $copy (ref $I32Array))
               ;; --- Type check ---
               (if (i32.eqz (ref.test (ref $String) (local.get $s)))
                   (then (call $raise-check-string (local.get $s))))
               ;; --- Cast and extract ---
               (local.set $str (ref.cast (ref $String) (local.get $s)))
               (local.set $arr (struct.get $String $codepoints (local.get $str)))
               (local.set $len (array.len (local.get $arr)))
               ;; --- Copy the codepoint array ---
               (local.set $copy (call $i32array-copy (local.get $arr) (i32.const 0) (local.get $len)))
               ;; --- Construct new mutable string with hash = 0 ---
               (struct.new $String
                           (i32.const 0)       ;; $hash
                           (i32.const 0)       ;; mutable
                           (local.get $copy))) ;; $codepoints

         (func $raise-immutable-string (param $x (ref eq)) (unreachable))
         
         (func $string-fill!
               (param $s   (ref eq))
               (param $ch  (ref eq))
               (result     (ref eq))

               (local $str (ref $String))
               (local $arr (ref $I32Array))
               (local $cp  i32)

               ;; --- 1. Check and cast string ---
               (if (i32.eqz (ref.test (ref $String) (local.get $s)))
                   (then (call $raise-check-string (local.get $s))))
               (local.set $str (ref.cast (ref $String) (local.get $s)))
               ;; --- 2. Raise if immutable ---
               (if (i32.ne (struct.get $String $immutable (local.get $str)) (i32.const 0))
                   (then (call $raise-immutable-string (local.get $s))))
               ;; --- 3. Decode and check char immediate ---
               (if (i32.eqz (ref.test (ref i31) (local.get $ch)))
                   (then (call $raise-check-char (local.get $ch))))
               (local.set $cp (i31.get_u (ref.cast (ref i31) (local.get $ch))))
               (if (i32.ne (i32.and (local.get $cp) (i32.const ,char-mask)) (i32.const ,char-tag))
                   (then (call $raise-check-char (local.get $ch))))
               (local.set $cp (i32.shr_u (local.get $cp) (i32.const ,char-shift)))
               ;; --- 4. Fill and reset hash ---
               (local.set $arr (struct.get $String $codepoints (local.get $str)))
               (call $i32array-fill! (local.get $arr) (local.get $cp))
               (struct.set $String $hash (local.get $str) (i32.const 0))
               ;; --- 5. Return void ---
               (global.get $void))


         (func $string-append (param $s1 (ref eq)) (param $s2 (ref eq)) (result (ref $String))
               (local $str1 (ref null $String))
               (local $str2 (ref null $String))
               (if (ref.test (ref $String) (local.get $s1))
                   (then (local.set $str1 (ref.cast (ref $String) (local.get $s1))))
                   (else (call $raise-check-string (local.get $s1))))
               (if (ref.test (ref $String) (local.get $s2))
                   (then (local.set $str2 (ref.cast (ref $String) (local.get $s2))))
                   (else (call $raise-check-string (local.get $s2))))
               (struct.new $String
                           (i32.const 0)
                           (i32.const 0)
                           (call $i32array-append
                                 (struct.get $String $codepoints (local.get $str1))
                                 (struct.get $String $codepoints (local.get $str2)))))

         (func $list->string (param $xs (ref eq)) (result (ref eq))
               (local $len   i32)
               (local $str   (ref $String))
               (local $arr   (ref $I32Array))
               (local $i     i32)
               (local $node  (ref eq))
               (local $chimm (ref eq))
               (local $cp    i32)
               ;; 1. Compute list length (will raise if not proper list)
               (local.set $len (call $length/i32 (local.get $xs)))
               ;; 2. Create new mutable string
               (local.set $str
                          (struct.new $String
                                      (i32.const 0)  ;; hash
                                      (i32.const 0)  ;; immutable = false
                                      (call $i32array-make (local.get $len) (i32.const 0))))
               ;; 3. Grab codepoint array once
               (local.set $arr (struct.get $String $codepoints (local.get $str)))
               ;; 4. Fill from list directly
               (local.set $i (i32.const 0))
               (local.set $node (local.get $xs))
               (block $done
                      (loop $fill
                            (br_if $done (ref.eq (local.get $node) (global.get $null)))
                            ;; Check node is a pair
                            (if (ref.test (ref $Pair) (local.get $node))
                                (then
                                 ;; extract car
                                 (local.set $chimm (struct.get $Pair $a (ref.cast (ref $Pair) (local.get $node))))
                                 ;; decode and validate character
                                 (local.set $cp (i31.get_u (ref.cast (ref i31) (local.get $chimm))))
                                 (if (i32.ne (i32.and (local.get $cp) (i32.const ,char-mask)) (i32.const ,char-tag))
                                     (then (call $raise-check-char (local.get $chimm))
                                           (unreachable)))
                                 ;; extract codepoint
                                 (local.set $cp (i32.shr_u (local.get $cp) (i32.const 8)))
                                 ;; write codepoint
                                 (call $i32array-set! (local.get $arr) (local.get $i) (local.get $cp))
                                 ;; advance
                                 (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                 (local.set $node (struct.get $Pair $d (ref.cast (ref $Pair) (local.get $node)))))
                                (else
                                 (call $raise-pair-expected (local.get $node))
                                 (unreachable)))
                            (br $fill)))
               ;; 5. Return string
               (local.get $str))
         
         (func $string-append-immutable (param $s1 (ref eq)) (param $s2 (ref eq)) (result (ref eq))
               (local $str1 (ref null $String))
               (local $str2 (ref null $String))
               ;; check strings
               (if (ref.test (ref $String) (local.get $s1))
                   (then (local.set $str1 (ref.cast (ref $String) (local.get $s1))))
                   (else (call $raise-check-string (local.get $s1))))
               (if (ref.test (ref $String) (local.get $s2))
                   (then (local.set $str2 (ref.cast (ref $String) (local.get $s2))))
                   (else (call $raise-check-string (local.get $s2))))
               ;; create immutable
               (struct.new $String
                           (i32.const 0)
                           (i32.const 1)
                           (call $i32array-copy
                                 (call $i32array-append
                                       (struct.get $String $codepoints (local.get $str1))
                                       (struct.get $String $codepoints (local.get $str2)))
                                 (i32.const 0)
                                 (call $i32array-length (call $i32array-append
                                                              (struct.get $String $codepoints (local.get $str1))
                                                              (struct.get $String $codepoints (local.get $str2)))))))

         (func $string->list (param $s (ref eq)) (result (ref eq))
               (local $str   (ref null $String))
               (local $arr   (ref $I32Array))
               (local $len   i32)
               (local $i     i32)
               (local $res   (ref $Pair))
               (local $cp    i32)
               ;; check string
               (if (ref.test (ref $String) (local.get $s))
                   (then (local.set $str (ref.cast (ref $String) (local.get $s))))
                   (else (call $raise-check-string (local.get $s))))
               (local.set $arr (struct.get $String $codepoints (local.get $str)))
               (local.set $len (call $i32array-length (local.get $arr)))
               ;; special case len=0
               (if (result (ref eq))
                   (i32.eqz (local.get $len))
                   (then (return (global.get $null)))
                   ; ;; special case len>=1
                   (else (local.set $i   (i32.sub (local.get $len) (i32.const 1))) ; from end
                         (local.set $cp  (call $i32array-ref (local.get $arr) (local.get $i)))
                         (local.set $res (struct.new $Pair (i32.const 0)
                                             (ref.i31 (i32.or (i32.shl (local.get $cp) (i32.const 8)) (i32.const ,char-tag)))
                                             (global.get $null)))
                         (local.set $i   (i32.sub (local.get $i) (i32.const 1)))
                         (block $done
                                (loop $build
                                      (br_if $done (i32.lt_s (local.get $i) (i32.const 0)))
                                      (local.set $cp (call $i32array-ref (local.get $arr) (local.get $i)))
                                      ;; tag cp as char
                                      (local.set $res 
                                                 (struct.new $Pair (i32.const 0)
                                                             (ref.i31 (i32.or (i32.shl (local.get $cp) (i32.const 8)) (i32.const ,char-tag)))
                                                             (local.get $res)))
                                      (local.set $i (i32.sub (local.get $i) (i32.const 1)))
                                      (br $build)))
                         (local.get $res))))

         (func $raise-invalid-utf8-input (param $bad (ref eq)) (result (ref eq)) (unreachable))

         (func $raise-invalid-utf8-start (param $bad (ref eq)) (result (ref eq)) (unreachable))

         (func $raise-invalid-utf8-end (param $bad (ref eq)) (result (ref eq)) (unreachable))

         (func $raise-invalid-utf8-range (param $start (ref eq)) (param $end (ref eq)) (result (ref eq))
               (unreachable))

         (func $raise-string->bytes/utf8                 (unreachable))
         (func $raise-string->bytes/utf8:expected-string (unreachable))
         (func $raise-string->bytes/utf8:range-error     (unreachable))
         
         (func $string->bytes/utf-8 (export "string->bytes/utf-8")
               (param $str       (ref eq))
               (param $err-byte  (ref eq)) ;; Ignored
               (param $start-raw (ref eq))
               (param $end-raw   (ref eq))
               (result (ref eq))

               (local $s         (ref null $String))
               (local $cp        (ref $I32Array))
               (local $start     i32)
               (local $end       i32)
               (local $len       i32)
               (local $out-bytes (ref $I8Array))
               (local $idx       i32)
               (local $i         i32)
               (local $char      i32)
               (local $bs        (ref $Bytes))

               (local $raw-start i32)
               (local $raw-end   i32)
               
               ;; Check and cast $str
               (if (ref.test (ref $String) (local.get $str))
                   (then (local.set $s (ref.cast (ref $String) (local.get $str))))
                   (else (call $raise-string->bytes/utf8:expected-string))) ;; code 1 = not a string
               ;; Get codepoints array and length
               (local.set $cp  (struct.get $String $codepoints (local.get $s)))
               (local.set $len (array.len (local.get $cp)))
               ;; Decode start
               (if (ref.test (ref i31) (local.get $start-raw))
                   (then (local.set $raw-start (i31.get_u (ref.cast (ref i31) (local.get $start-raw))))
                         (if (i32.eq (local.get $raw-start) (i32.const ,(immediate-rep #f)))
                             (then (local.set $start (i32.const 0)))  ;; #f => use 0
                             (else (local.set $start (i32.shr_u (local.get $raw-start) (i32.const 1))))))
                   (else (call $raise-string->bytes/utf8)))  ;; not a fixnum               
               ;; Decode end
               (if (ref.test (ref i31) (local.get $end-raw))
                   (then  (local.set $raw-end (i31.get_u (ref.cast (ref i31) (local.get $end-raw))))
                          (if (i32.eq (local.get $raw-end) (i32.const ,(immediate-rep #f))) ; #f
                              (then (local.set $end (local.get $len)))  ;; #f => use length
                              (else (local.set $end (i32.shr_u (local.get $raw-end) (i32.const 1))))))
                   (else (call $raise-string->bytes/utf8)))
               ;; Bounds check
               (if (i32.or (i32.gt_u (local.get $start) (local.get $end))
                           (i32.gt_u (local.get $end) (local.get $len)))
                   (then (call $raise-string->bytes/utf8:range-error)))
               ;; First pass: compute output size
               (local.set $i   (local.get $start))
               (local.set $idx (i32.const 0))
               (loop $size-loop
                     (if (i32.lt_u (local.get $i) (local.get $end))
                         (then
                          (local.set $char (array.get $I32Array (local.get $cp) (local.get $i)))
                          (local.set $idx (i32.add (local.get $idx) (call $utf8-size (local.get $char))))
                          (local.set $i (i32.add (local.get $i) (i32.const 1)))
                          (br $size-loop))))
               ;; Allocate byte array
               (local.set $out-bytes (array.new_default $I8Array (local.get $idx)))
               (local.set $bs
                          (struct.new $Bytes
                                      (i32.const 0) ;; hash
                                      (i32.const 1) ;; immutable
                                      (local.get $out-bytes)))
               ;; Second pass: encode into buffer
               (local.set $i   (local.get $start))
               (local.set $idx (i32.const 0))
               (loop $encode-loop
                     (if (i32.lt_u (local.get $i) (local.get $end))
                         (then
                          (local.set $char (array.get $I32Array (local.get $cp) (local.get $i)))
                          (local.set $idx  (call $write-utf8 (local.get $bs) (local.get $idx) (local.get $char)))
                          (local.set $i (i32.add (local.get $i) (i32.const 1)))
                          (br $encode-loop))))               
               (local.get $bs))

         (func $raise-string-copy!:bad-destination       (unreachable))
         (func $raise-string-copy!:bad-destination-start (unreachable))
         (func $raise-string-copy!:bad-source            (unreachable))
         (func $raise-string-copy!:bad-source-start      (unreachable))
         (func $raise-string-copy!:bad-source-end        (unreachable))
         (func $raise-string-copy!:bad-source-range      (unreachable))
         (func $raise-string-copy!:bad-destination-range (unreachable))

         (func $string-copy!
               (param $dst-raw       (ref eq))
               (param $dst-start-raw (ref eq))
               (param $src-raw       (ref eq))
               (param $src-start-raw (ref eq))
               (param $src-end-raw   (ref eq))
               (result (ref eq))

               (local $dst        (ref $String))
               (local $src        (ref $String))
               (local $dst-start  i32)
               (local $src-start  i32)
               (local $src-end    i32)
               (local $src-len    i32)
               (local $dst-len    i32)
               ;; --- Type + fixnum checks ---
               (if (i32.eqz (ref.test (ref $String) (local.get $dst-raw)))
                   (then (call $raise-string-copy!:bad-destination)))
               (if (i32.eqz (ref.test (ref i31) (local.get $dst-start-raw)))
                   (then (call $raise-string-copy!:bad-destination-start)))
               (if (i32.ne (i32.and (i31.get_u (ref.cast (ref i31) (local.get $dst-start-raw))) (i32.const 1)) (i32.const 0))
                   (then (call $raise-string-copy!:bad-destination-start)))

               (if (i32.eqz (ref.test (ref $String) (local.get $src-raw)))
                   (then (call $raise-string-copy!:bad-source)))
               (if (i32.eqz (ref.test (ref i31) (local.get $src-start-raw)))
                   (then (call $raise-string-copy!:bad-source-start)))
               (if (i32.ne (i32.and (i31.get_u (ref.cast (ref i31) (local.get $src-start-raw))) (i32.const 1)) (i32.const 0))
                   (then (call $raise-string-copy!:bad-source-start)))
               (if (i32.eqz (ref.test (ref i31) (local.get $src-end-raw)))
                   (then (call $raise-string-copy!:bad-source-end)))
               (if (i32.ne (i32.and (i31.get_u (ref.cast (ref i31) (local.get $src-end-raw))) (i32.const 1)) (i32.const 0))
                   (then (call $raise-string-copy!:bad-source-end)))
               ;; --- Decode ---
               (local.set $dst       (ref.cast (ref $String) (local.get $dst-raw)))
               (local.set $src       (ref.cast (ref $String) (local.get $src-raw)))
               (local.set $dst-start (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $dst-start-raw))) (i32.const 1)))
               (local.set $src-start (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $src-start-raw))) (i32.const 1)))
               (local.set $src-end   (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $src-end-raw)))   (i32.const 1)))
               ;; --- Mutability Check ---
               (if (i32.ne (struct.get $String $immutable (local.get $dst)) (i32.const 0))
                   (then (call $raise-immutable-string (local.get $dst))))
               ;; --- Range Validation ---
               (local.set $src-len (call $string-length/checked/i32 (local.get $src)))
               (local.set $dst-len (call $string-length/checked/i32 (local.get $dst)))
               (if (i32.gt_u (local.get $src-start) (local.get $src-end))
                   (then (call $raise-string-copy!:bad-source-range)))
               (if (i32.gt_u (local.get $src-end) (local.get $src-len))
                   (then (call $raise-string-copy!:bad-source-range)))
               (if (i32.gt_u
                    (i32.add (local.get $dst-start)
                             (i32.sub (local.get $src-end) (local.get $src-start)))
                    (local.get $dst-len))
                   (then (call $raise-string-copy!:bad-destination-range)))
               ;; --- Delegate ---
               (call $string-copy!/checked
                     (local.get $dst)
                     (local.get $dst-start)
                     (local.get $src)
                     (local.get $src-start)
                     (local.get $src-end))
               ;; --- Invalidate hash ---
               (struct.set $String $hash (local.get $dst) (i32.const 0))
               ;; --- Return ---
               (global.get $void))


         (func $raise-string-copy!/checked:out-of-bounds (unreachable))
         
         (func $string-copy!/checked
               (param $dst        (ref $String)) ;; Destination string
               (param $dst-start  i32)           ;; Start index in destination
               (param $src        (ref $String)) ;; Source string
               (param $src-start  i32)           ;; Start index in source
               (param $src-end    i32)           ;; End index in source (exclusive)

               (local $dst-len i32)
               (local $src-len i32)
               (local $len     i32)

               ;; --- Compute source slice length ---
               (local.set $len (i32.sub (local.get $src-end) (local.get $src-start)))
               ;; --- Range Checks ---
               (if (i32.lt_u (local.get $src-end) (local.get $src-start))
                   (then (call $raise-string-copy!/checked:out-of-bounds)))
               (local.set $dst-len (call $string-length/checked/i32 (local.get $dst)))
               (local.set $src-len (call $string-length/checked/i32 (local.get $src)))
               (if (i32.gt_u (i32.add (local.get $dst-start) (local.get $len)) (local.get $dst-len))
                   (then (call $raise-string-copy!/checked:out-of-bounds)))
               (if (i32.gt_u (local.get $src-end) (local.get $src-len))
                   (then (call $raise-string-copy!/checked:out-of-bounds)))
               ;; --- Copy elements ---
               (call $i32array-copy!
                     (struct.get $String $codepoints (local.get $dst))
                     (local.get $dst-start)
                     (struct.get $String $codepoints (local.get $src))
                     (local.get $src-start)
                     (local.get $len))
               ;; --- Invalidate destination hash ---
               (struct.set $String $hash (local.get $dst) (i32.const 0)))

         (func $string=?
               (param $a (ref eq)) (param $b (ref eq))
               (result (ref eq))
               (if (result (ref eq)) (call $string=?/i32 (local.get $a) (local.get $b))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $string=?/i32
               (param $a-raw (ref eq)) (param $b-raw (ref eq))
               (result i32)

               (local $a (ref $String))
               (local $b (ref $String))
               ;; Check types
               (if (i32.eqz (ref.test (ref $String) (local.get $a-raw)))
                   (then (return (i32.const 0))))
               (if (i32.eqz (ref.test (ref $String) (local.get $b-raw)))
                   (then (return (i32.const 0))))
               ;; Cast and delegate
               (local.set $a (ref.cast (ref $String) (local.get $a-raw)))
               (local.set $b (ref.cast (ref $String) (local.get $b-raw)))
               (return_call $string=?/i32/checked (local.get $a) (local.get $b)))
         
         (func $string=?/i32/checked
               (param $a (ref $String)) (param $b (ref $String))
               (result i32)

               (return_call $i32array-equal?
                            (struct.get $String $codepoints (local.get $a))
                            (struct.get $String $codepoints (local.get $b))))

         (func $string<?
               (param $a (ref eq)) (param $b (ref eq))
               (result (ref eq))
               (if (result (ref eq)) (call $string</i32 (local.get $a) (local.get $b))
                   (then (global.get $true))
                   (else (global.get $false))))
         
          (func $string</i32
               (param $a-raw (ref eq)) (param $b-raw (ref eq))
               (result i32)

               (local $a (ref $String))
               (local $b (ref $String))

               ;; Check types
               (if (i32.eqz (ref.test (ref $String) (local.get $a-raw)))
                   (then (return (i32.const 0))))
               (if (i32.eqz (ref.test (ref $String) (local.get $b-raw)))
                   (then (return (i32.const 0))))
               ;; Cast and delegate
               (local.set $a (ref.cast (ref $String) (local.get $a-raw)))
               (local.set $b (ref.cast (ref $String) (local.get $b-raw)))
               (return_call $string</i32/checked (local.get $a) (local.get $b)))

         (func $string</i32/checked
               (param $a (ref $String)) (param $b (ref $String))
               (result i32)

               (local $arr-a (ref $I32Array))
               (local $arr-b (ref $I32Array))
               (local $len-a i32)
               (local $len-b i32)
               (local $i i32)
               (local $cp-a i32)
               (local $cp-b i32)

               (local.set $arr-a (struct.get $String $codepoints (local.get $a)))
               (local.set $arr-b (struct.get $String $codepoints (local.get $b)))
               (local.set $len-a (array.len (local.get $arr-a)))
               (local.set $len-b (array.len (local.get $arr-b)))
               (local.set $i (i32.const 0))

               (block $exit
                      (loop $loop
                            (br_if $exit (i32.ge_u (local.get $i) (local.get $len-a)))
                            (br_if $exit (i32.ge_u (local.get $i) (local.get $len-b)))
                            (local.set $cp-a (array.get $I32Array (local.get $arr-a) (local.get $i)))
                            (local.set $cp-b (array.get $I32Array (local.get $arr-b) (local.get $i)))
                            (if (i32.lt_u (local.get $cp-a) (local.get $cp-b))
                                (then (return (i32.const 1))))
                            (if (i32.gt_u (local.get $cp-a) (local.get $cp-b))
                                (then (return (i32.const 0))))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $loop)))
               ;; If one is a prefix of the other
               (return (i32.lt_u (local.get $len-a) (local.get $len-b))))
         
         (func $array-of-strings->string
               (param $arr (ref $Array))
               (result (ref $String))

               (local $n     i32)
               (local $i     i32)
               (local $total i32)
               (local $s     (ref $String))
               (local $len   i32)
               (local $dst   (ref $String))
               (local $pos   i32)

               ;; Pass 1: compute total length
               (local.set $n     (array.len (local.get $arr)))
               (local.set $i     (i32.const 0))
               (local.set $total (i32.const 0))

               (block $count-done
                      (loop $count-loop
                            (br_if $count-done (i32.ge_u (local.get $i) (local.get $n)))
                            (local.set $s     (ref.cast (ref $String) (array.get $Array (local.get $arr) (local.get $i))))
                            (local.set $len   (call $string-length/checked/i32 (local.get $s)))
                            (local.set $total (i32.add (local.get $total) (local.get $len)))
                            (local.set $i     (i32.add (local.get $i) (i32.const 1)))
                            (br $count-loop)))
               ;; Allocate destination string
               (local.set $dst (call $make-string/checked (local.get $total) (i32.const 0)))
               ;; Pass 2: copy individual strings into destination
               (local.set $i   (i32.const 0))
               (local.set $pos (i32.const 0))
               (block $copy-done
                      (loop $copy-loop
                            (br_if $copy-done (i32.ge_u (local.get $i) (local.get $n)))
                            (local.set $s   (ref.cast (ref $String) (array.get $Array (local.get $arr) (local.get $i))))
                            (local.set $len (call $string-length/checked/i32 (local.get $s)))
                            (call $string-copy!/checked (local.get $dst) (local.get $pos) (local.get $s) (i32.const 0) (local.get $len))
                            (local.set $pos (i32.add (local.get $pos) (local.get $len)))
                            (local.set $i   (i32.add (local.get $i) (i32.const 1)))
                            (br $copy-loop)))
               (local.get $dst))

         (func $growable-array-of-strings->string
               (param $g (ref $GrowableArray))
               (result (ref $String))
               ;; Convert growable array to normal array and concatenate its strings
               (call $array-of-strings->string
                     (call $growable-array->array (local.get $g))))

         


         
         
         (func $string-take
               (param $s (ref eq))
               (param $n (ref eq))
               (result   (ref $String))

               (local $str   (ref $String))
               (local $n/tag i32)
               (local $n/i32 i32)
               (local $len   i32)
               
               (if (i32.eqz (ref.test (ref $String) (local.get $s)))
                   (then (call $raise-check-string (local.get $s))))
               (if (i32.eqz (ref.test (ref i31) (local.get $n)))
                   (then (call $raise-check-fixnum (local.get $n))))

               (local.set $str   (ref.cast (ref $String) (local.get $s)))
               (local.set $n/tag (i31.get_u (ref.cast (ref i31) (local.get $n))))

               (if (i32.and (local.get $n/tag) (i32.const 1))
                   (then (call $raise-check-fixnum (local.get $n))))
               (local.set $n/i32 (i32.shr_u (local.get $n/tag) (i32.const 1)))

               (local.set $len (call $string-length/checked/i32 (local.get $str)))
               (if (i32.gt_u (local.get $n/i32) (local.get $len))
                   (then (call $raise-bad-string-index/i32 (local.get $s) (local.get $n/i32))))

               (call $string-take/checked (local.get $str) (local.get $n/i32)))

         (func $string-take/checked
               (param $s (ref $String)) (param $n i32)
               (result (ref $String))
               (call $i32array->string
                     (call $i32array-take
                           (struct.get $String $codepoints (local.get $s))
                           (local.get $n))))



         (func $string-take-right
               (param $s (ref eq))
               (param $n (ref eq))
               (result   (ref $String))

               (local $str   (ref $String))
               (local $n/tag i32)
               (local $n/i32 i32)
               (local $len   i32)

               (if (i32.eqz (ref.test (ref $String) (local.get $s)))
                   (then (call $raise-check-string (local.get $s))))
               (if (i32.eqz (ref.test (ref i31) (local.get $n)))
                   (then (call $raise-check-fixnum (local.get $n))))

               (local.set $str   (ref.cast (ref $String) (local.get $s)))
               (local.set $n/tag (i31.get_u (ref.cast (ref i31) (local.get $n))))
               (if (i32.and (local.get $n/tag) (i32.const 1))
                   (then (call $raise-check-fixnum (local.get $n))))
               (local.set $n/i32 (i32.shr_u (local.get $n/tag) (i32.const 1)))

               (local.set $len (call $string-length/checked/i32 (local.get $str)))
               (if (i32.gt_u (local.get $n/i32) (local.get $len))
                   (then (call $raise-bad-string-index/i32 (local.get $s) (local.get $n/i32))
                         (unreachable)))

               (call $string-take-right/checked (local.get $str) (local.get $n/i32)))

         (func $string-take-right/checked
               (param $s (ref $String))
               (param $n i32)
               (result   (ref $String))

               (call $i32array->string
                     (call $i32array-take-right
                           (struct.get $String $codepoints (local.get $s))
                           (local.get $n))))

         (func $string-drop
               (param $s (ref eq))
               (param $n (ref eq))
               (result (ref $String))

               (local $str   (ref $String))
               (local $n/tag i32)
               (local $n/i32 i32)
               (local $len   i32)

               (if (i32.eqz (ref.test (ref $String) (local.get $s)))
                   (then (call $raise-check-string (local.get $s))))
               (if (i32.eqz (ref.test (ref i31) (local.get $n)))
                   (then (call $raise-check-fixnum (local.get $n))))

               (local.set $str   (ref.cast (ref $String) (local.get $s)))
               (local.set $n/tag (i31.get_u (ref.cast (ref i31) (local.get $n))))
               (if (i32.and (local.get $n/tag) (i32.const 1))
                   (then (call $raise-check-fixnum (local.get $n))))
               (local.set $n/i32 (i32.shr_u (local.get $n/tag) (i32.const 1)))

               (local.set $len (call $string-length/checked/i32 (local.get $str)))
               (if (i32.gt_u (local.get $n/i32) (local.get $len))
                   (then (call $raise-bad-string-index/i32 (local.get $s) (local.get $n/i32))))

               (call $string-drop/checked (local.get $str) (local.get $n/i32)))

         (func $string-drop/checked
               (param $s (ref $String)) (param $n i32)
               (result (ref $String))
               (call $i32array->string
                     (call $i32array-drop
                           (struct.get $String $codepoints (local.get $s))
                           (local.get $n))))

         (func $string-drop-right
               (param $s (ref eq))
               (param $n (ref eq))
               (result   (ref $String))

               (local $str   (ref $String))
               (local $n/tag i32)
               (local $n/i32 i32)
               (local $len   i32)

               (if (i32.eqz (ref.test (ref $String) (local.get $s)))
                   (then (call $raise-check-string (local.get $s))))
               (if (i32.eqz (ref.test (ref i31) (local.get $n)))
                   (then (call $raise-check-fixnum (local.get $n))))

               (local.set $str   (ref.cast (ref $String) (local.get $s)))
               (local.set $n/tag (i31.get_u (ref.cast (ref i31) (local.get $n))))
               (if (i32.and (local.get $n/tag) (i32.const 1))
                   (then (call $raise-check-fixnum (local.get $n))))
               (local.set $n/i32 (i32.shr_u (local.get $n/tag) (i32.const 1)))

               (local.set $len (call $string-length/checked/i32 (local.get $str)))
               (if (i32.gt_u (local.get $n/i32) (local.get $len))
                   (then (call $raise-bad-string-index/i32 (local.get $s) (local.get $n/i32))))

               (call $string-drop-right/checked (local.get $str) (local.get $n/i32)))

         (func $string-drop-right/checked
               (param $s (ref $String)) (param $n i32)
               (result (ref $String))
               (call $i32array->string
                     (call $i32array-drop-right
                           (struct.get $String $codepoints (local.get $s))
                           (local.get $n))))


         

         
         (func $raise-argument-error:string-expected (unreachable))

         (func $bomb (unreachable))
         
         (func $string-trim-right
               (param $s       (ref eq))   ;; any value, must be a string
               (param $sep     (ref eq))   ;; a character (tagged i31) or #f
               (result         (ref $String))

               (local $str     (ref $String))
               (local $len     i32)
               (local $new-len i32)
               (local $sep/tag i32) ;; still-tagged char
               (local $sep-ch  i32) ;; decoded code point
               (local $use-whitespace? i32)
               (local $ch      i32)

               ;; --- Type check inputs ---
               (if (i32.eqz (ref.test (ref $String) (local.get $s)))
                   (then (call $raise-argument-error:string-expected (local.get $s))
                         (unreachable)))

               (if (ref.eq (local.get $sep) (global.get $false))
                   (then (local.set $use-whitespace? (i32.const 1)))
                   (else
                    (if (i32.eqz (ref.test (ref i31) (local.get $sep)))
                        (then (call $raise-argument-error:char-expected (local.get $sep))
                              (unreachable)))
                    (local.set $sep/tag (i31.get_u (ref.cast (ref i31) (local.get $sep))))
                    (if (i32.ne (i32.and (local.get $sep/tag) (i32.const ,char-mask))
                                (i32.const ,char-tag))
                        (then (call $raise-argument-error:char-expected (local.get $sep))
                              (unreachable)))
                    (local.set $use-whitespace? (i32.const 0))))

               ;; --- Decode after checks ---
               (local.set $str (ref.cast (ref $String) (local.get $s)))
               (if (i32.eq (local.get $use-whitespace?) (i32.const 0))
                   (then (local.set $sep-ch (i32.shr_u (local.get $sep/tag) (i32.const ,char-shift)))))

               ;; --- Get length ---
               (local.set $len     (call $string-length/checked/i32 (local.get $str)))
               (local.set $new-len (local.get $len))

               ;; --- Scan backward ---
               (block $done
                      (loop $scan
                            (br_if $done (i32.eqz (local.get $new-len)))
                            (local.set $ch
                                       (call $string-ref/checked/i32
                                             (local.get $str)
                                             (i32.sub (local.get $new-len) (i32.const 1))))

                            (if (i32.eq (local.get $use-whitespace?) (i32.const 1))
                                ;; --- Trim if whitespace ---
                                (then
                                 (if (ref.eq (call $char-whitespace?/ucs (local.get $ch))
                                             (global.get $true))
                                     (then
                                      (local.set $new-len (i32.sub (local.get $new-len) (i32.const 1)))
                                      (br $scan))))
                                ;; --- Else trim if equals sep-ch ---
                                (else
                                 (if (i32.eq (local.get $ch) (local.get $sep-ch))
                                     (then
                                      (local.set $new-len (i32.sub (local.get $new-len) (i32.const 1)))
                                      (br $scan))))))
                      )

               ;; --- Return result ---
               (if (i32.eq (local.get $new-len) (local.get $len))
                   (then (return (local.get $str)))
                   (else (return (call $string-take/checked (local.get $str) (local.get $new-len)))))

               (unreachable))
         
         (func $string-trim-left
               (param $s   (ref eq))   ;; any value, must be a string
               (param $sep (ref eq))   ;; a character (i31) or #f
               (result     (ref $String))

               (local $str     (ref $String))
               (local $len     i32)
               (local $i       i32)
               (local $sep-ch  i32)
               (local $use-whitespace? i32) ;; boolean flag
               (local $sep/tag i32)
               (local $ch      i32)

               ;; --- Check inputs ---
               (if (i32.eqz (ref.test (ref $String) (local.get $s)))
                   (then (call $raise-argument-error:string-expected (local.get $s))
                         (unreachable)))

               (if (ref.eq (local.get $sep) (global.get $false))
                   (then (local.set $use-whitespace? (i32.const 1)))
                   (else
                    ;; Check: is it a (ref i31)?
                    (if (i32.eqz (ref.test (ref i31) (local.get $sep)))
                        (then (call $raise-argument-error:char-expected (local.get $sep))
                              (unreachable)))
                    ;; Extract raw tagged value
                    (local.set $sep/tag (i31.get_u (ref.cast (ref i31) (local.get $sep))))
                    ;; Check tag matches ,char-tag
                    (if (i32.ne (i32.and (local.get $sep/tag) (i32.const ,char-mask))
                                (i32.const ,char-tag))
                        (then (call $raise-argument-error:char-expected (local.get $sep))
                              (unreachable)))
                    ;; Passed: decode
                    (local.set $use-whitespace? (i32.const 0))
                    (local.set $sep-ch (i32.shr_u (local.get $sep/tag) (i32.const ,char-shift)))))
               ;; --- Decode after checks ---
               (local.set $str (ref.cast (ref $String) (local.get $s)))
               ;; --- Get string length (as i32) ---
               (local.set $len (call $string-length/checked/i32 (local.get $str)))
               (local.set $i   (i32.const 0))
               ;; --- Scan forward ---
               (block $done
                      (loop $scan
                            (br_if $done (i32.eq (local.get $i) (local.get $len)))

                            (local.set $ch (call $string-ref/checked/i32
                                                 (local.get $str)
                                                 (local.get $i)))

                            (if (i32.eq (local.get $use-whitespace?) (i32.const 1))
                                ;; --- Trim if character is whitespace ---
                                (then (if (ref.eq (call $char-whitespace?/ucs (local.get $ch))
                                                  (global.get $true))
                                          (then (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                                (br $scan))))
                                ;; --- Else trim if char equals $sep-ch ---
                                (else (if (i32.eq (local.get $ch) (local.get $sep-ch))
                                          (then (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                                (br $scan)))))))

               ;; --- Return result ---
               (if (i32.eqz (local.get $i))
                   (then (return (local.get $str)))
                   (else (return (call $string-drop
                                       (local.get $str)
                                       (ref.i31 (i32.shl (local.get $i) (i32.const 1)))))))
               (unreachable))
         
         ;;;
         ;;; Byte Strings
         ;;;

         (func $raise-byte-out-of-range   (param $x (ref eq)) (unreachable))
         (func $raise-check-bytes         (param $x (ref eq)) (unreachable))
         (func $raise-check-byte          (param $x (ref eq)) (unreachable))
         (func $raise-bad-bytes-ref-index (param $x (ref eq)) (param $idx (ref eq)) (unreachable))         
         (func $raise-bad-bytes-range     (param $x (ref eq)) (param i32) (param i32) (unreachable))         
         
         (func $make-bytes (param $k (ref eq)) (param $b (ref eq)) (result (ref eq))
               (local $len i32)
               (local $val i32)
               ;; Decode and check $k as fixnum
               (if (ref.test (ref i31) (local.get $k))
                   (then (local.set $len (i31.get_u (ref.cast (ref i31) (local.get $k))))
                         (if (i32.eqz (i32.and (local.get $len) (i32.const 1)))
                             (then (local.set $len (i32.shr_u (local.get $len) (i32.const 1))))
                             (else (call $raise-check-fixnum (local.get $k))
                                   (unreachable))))
                   (else (call $raise-check-fixnum (local.get $k))
                         (unreachable)))
               ;; Decode and check $b as fixnum in range [0, 255]
               (if (ref.test (ref i31) (local.get $b))
                   (then (local.set $val (i31.get_u (ref.cast (ref i31) (local.get $b))))
                         (if (i32.eqz (i32.and (local.get $val) (i32.const 1)))
                             (then (local.set $val (i32.shr_u (local.get $val) (i32.const 1)))
                                   (if (i32.gt_u (local.get $val) (i32.const 255))
                                       (then (call $raise-byte-out-of-range (local.get $b))
                                             (unreachable))))
                             (else (call $raise-check-fixnum (local.get $b))
                                   (unreachable))))
                   (else (call $raise-check-fixnum (local.get $b))
                         (unreachable)))
               ;; Construct mutable bytes object
               (struct.new $Bytes
                           (i32.const 0)  ;; hash
                           (i32.const 0)  ;; immutable = false
                           (call $i8make-array (local.get $len) (local.get $val))))
                  
         (func $bytes-length (param $a (ref eq)) (result (ref eq))
               (local $bs  (ref null $Bytes))
               (local $arr (ref $I8Array))
               (local $len i32)
               ;; Check that $a is a byte string
               (if (ref.test (ref $Bytes) (local.get $a))
                   (then (local.set $bs (ref.cast (ref $Bytes) (local.get $a))))
                   (else (call $raise-check-bytes (local.get $a))
                         (unreachable)))
               ;; Get the backing array and compute length
               (local.set $arr (struct.get $Bytes $bs (local.get $bs)))
               (local.set $len (call $i8array-length (local.get $arr)))
               ;; Convert to fixnum and return
               (ref.i31 (i32.shl (local.get $len) (i32.const 1))))

         (func $bytes? (param $a (ref eq)) (result (ref eq))
               (if (result (ref eq)) (ref.test (ref $Bytes) (local.get $a))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $raise-expected-bytes (unreachable))

         (func $bytes=?
               (param $v1 (ref eq))
               (param $v2 (ref eq))
               (result    (ref eq))

               (if (i32.eqz (ref.test (ref $Bytes) (local.get $v1)))
                   (then (call $raise-expected-bytes (local.get $v1)) (unreachable)))
               (if (i32.eqz (ref.test (ref $Bytes) (local.get $v2)))
                   (then (call $raise-expected-bytes (local.get $v2)) (unreachable)))

               (call $bytes=?/checked
                     (ref.cast (ref $Bytes) (local.get $v1))
                     (ref.cast (ref $Bytes) (local.get $v2))))

         (func $bytes=?/checked
               (param $b1 (ref $Bytes))
               (param $b2 (ref $Bytes))
               (result    (ref eq))

               (local $a1   (ref $I8Array))
               (local $a2   (ref $I8Array))
               (local $len1 i32)
               (local $len2 i32)
               (local $i    i32)
               (local $v1   i32)
               (local $v2   i32)

               ;; Fast path: same reference
               (if (ref.eq (local.get $b1) (local.get $b2))
                   (then (return (global.get $true))))
               ;; Extract arrays and lengths
               (local.set $a1   (struct.get $Bytes $bs (local.get $b1)))
               (local.set $a2   (struct.get $Bytes $bs (local.get $b2)))
               (local.set $len1 (array.len (local.get $a1)))
               (local.set $len2 (array.len (local.get $a2)))
               ;; Length mismatch → not equal
               (if (i32.ne (local.get $len1) (local.get $len2))
                   (then (return (global.get $false))))
               ;; Compare bytes one-by-one
               (local.set $i (i32.const 0))
               (block $done
                      (loop $loop
                            (br_if $done (i32.ge_u (local.get $i) (local.get $len1)))
                            ;; Load byte i (as sign-extended i32)
                            (local.set $v1 (i32.extend8_s (array.get_u $I8Array (local.get $a1) (local.get $i))))
                            (local.set $v2 (i32.extend8_s (array.get_u $I8Array (local.get $a2) (local.get $i))))
                            ;; Compare
                            (if (i32.ne (local.get $v1) (local.get $v2))
                                (then (return (global.get $false))))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $loop)))
               ;; All bytes match
               (global.get $true))

         
         (func $byte? (param $v (ref eq)) (result (ref eq))
               (local $i i32)
               (if (result (ref eq)) (ref.test (ref i31) (local.get $v))
                   (then (local.set $i (i31.get_u (ref.cast (ref i31) (local.get $v))))
                         (if (result (ref eq)) 
                             ;; Check: is even (fixnum) and in range 0–255
                             (i32.and 
                              (i32.eqz (i32.and (local.get $i) (i32.const 1)))       ;; even tag bit = 0
                              (i32.le_u (i32.shr_u (local.get $i) (i32.const 1))     ;; untagged value
                                        (i32.const 255)))
                             (then (global.get $true))
                             (else (global.get $false))))
                   (else (global.get $false))))
         
         (func $bytes-ref (param $a (ref eq)) (param $i (ref eq)) (result (ref eq))
               (local $b   (ref null $Bytes))
               (local $arr (ref $I8Array))
               (local $idx i32)
               (local $v   i32)
               ;; 1. Check that $a is a byte string
               (if (ref.test (ref $Bytes) (local.get $a))
                   (then (local.set $b (ref.cast (ref $Bytes) (local.get $a))))
                   (else (call $raise-check-bytes (local.get $a))))
               ;; 2. Decode and check that $i is a fixnum
               (if (ref.test (ref i31) (local.get $i))
                   (then (local.set $idx (i31.get_u (ref.cast (ref i31) (local.get $i))))
                         (if (i32.eqz (i32.and (local.get $idx) (i32.const 1)))
                             (then (local.set $idx (i32.shr_u (local.get $idx) (i32.const 1))))
                             (else (call $raise-check-fixnum (local.get $i)) )))
                   (else (call $raise-check-fixnum (local.get $i))))
               ;; 3. Get byte array and bounds-check
               (local.set $arr (struct.get $Bytes $bs (local.get $b)))
               (if (i32.lt_u (local.get $idx) (array.len (local.get $arr)))
                   (then
                    ;; 4. Read and box byte
                    (local.set $v (call $i8array-ref (local.get $arr) (local.get $idx)))
                    (return (ref.i31 (i32.shl (local.get $v) (i32.const 1)))))
                   (else
                    (call $raise-bad-bytes-ref-index (local.get $a) (local.get $i))))
               (unreachable))

         (func $bytes-set! (param $a (ref eq)) (param $i (ref eq)) (param $v (ref eq)) (result (ref eq))
               (local $b   (ref null $Bytes))
               (local $arr (ref $I8Array))
               (local $idx i32)
               (local $bv  i32)
               ;; 1. Check that $a is a byte string
               (if (ref.test (ref $Bytes) (local.get $a))
                   (then (local.set $b (ref.cast (ref $Bytes) (local.get $a))))
                   (else (call $raise-check-bytes (local.get $a))))
               ;; 2. Decode and check fixnum index $i
               (if (ref.test (ref i31) (local.get $i))
                   (then
                    (local.set $idx (i31.get_u (ref.cast (ref i31) (local.get $i))))
                    (if (i32.eqz (i32.and (local.get $idx) (i32.const 1)))
                        (then (local.set $idx (i32.shr_u (local.get $idx) (i32.const 1))))
                        (else (call $raise-check-fixnum (local.get $i)))))
                   (else (call $raise-check-fixnum (local.get $i))))
               ;; 3. Decode and check fixnum byte value $v
               (if (ref.test (ref i31) (local.get $v))
                   (then
                    (local.set $bv (i31.get_u (ref.cast (ref i31) (local.get $v))))
                    (if (i32.eqz (i32.and (local.get $bv) (i32.const 1)))
                        (then
                         (local.set $bv (i32.shr_u (local.get $bv) (i32.const 1)))
                         (if (i32.gt_u (local.get $bv) (i32.const 255))
                             (then (call $raise-check-byte (local.get $v)))))
                        (else (call $raise-check-byte (local.get $v)))))
                   (else (call $raise-check-byte (local.get $v))))
               ;; 4. Bounds check and set byte
               (local.set $arr (struct.get $Bytes $bs (local.get $b)))
               (if (i32.lt_u (local.get $idx) (call $i8array-length (local.get $arr)))
                   (then (call $i8array-set! (local.get $arr) (local.get $idx) (local.get $bv))
                         (return (global.get $void)))
                   (else (call $raise-bad-bytes-ref-index (local.get $a) (local.get $i))))
               (unreachable))

         (func $bytes-set!/checked (param $a (ref $Bytes)) (param $i i32) (param $b i32)
               ; unsafe 
               (local $arr (ref $I8Array))
               (local.set $arr (struct.get $Bytes $bs (local.get $a)))
               (call $i8array-set! (local.get $arr) (local.get $i) (local.get $b)))

         (func $subbytes
               (param $b     (ref eq))   ;; input byte string
               (param $start (ref eq))   ;; start index
               (param $end   (ref eq))   ;; end index
               (result (ref eq))
               
               (local $bs   (ref null $Bytes))
               (local $arr  (ref $I8Array))
               (local $from i32)
               (local $to   i32)
               (local $len  i32)
               ;; Check that $b is a byte string
               (if (ref.test (ref $Bytes) (local.get $b))
                   (then (local.set $bs (ref.cast (ref $Bytes) (local.get $b))))
                   (else (call $raise-check-bytes (local.get $b)) (unreachable)))

               (local.set $arr (struct.get $Bytes $bs (local.get $bs)))
               (local.set $len (call $i8array-length (local.get $arr)))
               ;; Decode and validate fixnum $start
               (if (ref.test (ref i31) (local.get $start))
                   (then
                    (local.set $from (i31.get_u (ref.cast (ref i31) (local.get $start))))
                    (if (i32.eqz (i32.and (local.get $from) (i32.const 1)))
                        (then (local.set $from (i32.shr_u (local.get $from) (i32.const 1))))
                        (else (call $raise-check-fixnum (local.get $start)) (unreachable))))
                   (else (call $raise-check-fixnum (local.get $start)) (unreachable)))
               ;; Decode and validate fixnum $end
               (if (ref.test (ref i31) (local.get $end))
                   (then
                    (local.set $to (i31.get_u (ref.cast (ref i31) (local.get $end))))
                    (if (i32.eqz (i32.and (local.get $to) (i32.const 1)))
                        (then (local.set $to (i32.shr_u (local.get $to) (i32.const 1))))
                        (else (call $raise-check-fixnum (local.get $end)) (unreachable))))
                   (else (call $raise-check-fixnum (local.get $end)) (unreachable)))
               ;; Bounds check: 0 <= from <= to <= len
               (if (i32.gt_u (local.get $from) (local.get $to))
                   (then (call $raise-bad-bytes-range (local.get $b) (local.get $from) (local.get $to)) (unreachable)))
               (if (i32.gt_u (local.get $to) (local.get $len))
                   (then (call $raise-bad-bytes-range (local.get $b) (local.get $from) (local.get $to)) (unreachable)))
               ;; Copy the subarray
               (struct.new $Bytes
                           (i32.const 0)
                           (i32.const 0)
                           (call $i8array-copy (local.get $arr) (local.get $from) (local.get $to))))
         
         (func $bytes-copy!
               (param $dest       (ref eq))
               (param $dest-start (ref eq))
               (param $src        (ref eq))
               (param $src-start  (ref eq))
               (param $src-end    (ref eq))
               (result (ref eq))

               (local $d    (ref null $Bytes))
               (local $s    (ref null $Bytes))
               (local $darr (ref $I8Array))
               (local $sarr (ref $I8Array))
               (local $di   i32)
               (local $si   i32)
               (local $ei   i32)
               ;; check $dest
               (if (ref.test (ref $Bytes) (local.get $dest))
                   (then (local.set $d (ref.cast (ref $Bytes) (local.get $dest))))
                   (else (call $raise-check-bytes (local.get $dest)) (unreachable)))
               ;; check $src
               (if (ref.test (ref $Bytes) (local.get $src))
                   (then (local.set $s (ref.cast (ref $Bytes) (local.get $src))))
                   (else (call $raise-check-bytes (local.get $src)) (unreachable)))
               ;; decode $dest-start
               (if (ref.test (ref i31) (local.get $dest-start))
                   (then (local.set $di (i31.get_u (ref.cast (ref i31) (local.get $dest-start))))
                         (if (i32.eqz (i32.and (local.get $di) (i32.const 1)))
                             (then (local.set $di (i32.shr_u (local.get $di) (i32.const 1))))
                             (else (call $raise-check-fixnum (local.get $dest-start)) (unreachable))))
                   (else (call $raise-check-fixnum (local.get $dest-start)) (unreachable)))
               ;; decode $src-start
               (if (ref.test (ref i31) (local.get $src-start))
                   (then (local.set $si (i31.get_u (ref.cast (ref i31) (local.get $src-start))))
                         (if (i32.eqz (i32.and (local.get $si) (i32.const 1)))
                             (then (local.set $si (i32.shr_u (local.get $si) (i32.const 1))))
                             (else (call $raise-check-fixnum (local.get $src-start)) (unreachable))))
                   (else (call $raise-check-fixnum (local.get $src-start)) (unreachable)))
               ;; decode $src-end
               (if (ref.test (ref i31) (local.get $src-end))
                   (then (local.set $ei (i31.get_u (ref.cast (ref i31) (local.get $src-end))))
                         (if (i32.eqz (i32.and (local.get $ei) (i32.const 1)))
                             (then (local.set $ei (i32.shr_u (local.get $ei) (i32.const 1))))
                             (else (call $raise-check-fixnum (local.get $src-end)) (unreachable))))
                   (else (call $raise-check-fixnum (local.get $src-end)) (unreachable)))
               ;; get byte arrays
               (local.set $darr (struct.get $Bytes $bs (local.get $d)))
               (local.set $sarr (struct.get $Bytes $bs (local.get $s)))
               ;; copy bytes
               (drop (call $i8array-copy!/error
                           (local.get $darr)
                           (local.get $di)
                           (local.get $sarr)
                           (local.get $si)
                           (local.get $ei)))
               (global.get $void))
         
         (func $bytes-copy
               (param $src (ref eq))
               (result (ref eq))
               (local $b  (ref null $Bytes))
               (local $a  (ref $I8Array))
               (local $a2 (ref $I8Array))
               ;; Check that $src is a byte string
               (if (ref.test (ref $Bytes) (local.get $src))
                   (then (local.set $b (ref.cast (ref $Bytes) (local.get $src))))
                   (else (call $raise-check-bytes (local.get $src)) (unreachable)))
               ;; Extract and copy the underlying I8Array
               (local.set $a  (struct.get $Bytes $bs (local.get $b)))
               (local.set $a2 (call $i8array-copy (local.get $a) (i32.const 0) (call $i8array-length (local.get $a))))
               ;; Return a new mutable Bytes struct
               (struct.new $Bytes
                           (i32.const 0)          ;; hash
                           (i32.const 0)          ;; immutable = false
                           (local.get $a2)))
         
         (func $bytes-fill!
               (param $dest (ref eq))
               (param $b (ref eq))
               (result (ref eq))
               
               (local $bs  (ref null $Bytes))
               (local $arr (ref $I8Array))
               (local $val i32)
               ;; Check that dest is a byte string
               (if (ref.test (ref $Bytes) (local.get $dest))
                   (then (local.set $bs (ref.cast (ref $Bytes) (local.get $dest))))
                   (else (call $raise-check-bytes (local.get $dest)) (unreachable)))
               ;; Check that b is a valid fixnum byte (0–255)
               (if (ref.test (ref i31) (local.get $b))
                   (then (local.set $val (i31.get_u (ref.cast (ref i31) (local.get $b))))
                         (if (i32.ge_u (local.get $val) (i32.const 256))
                             (then (call $raise-check-byte (local.get $b)) (unreachable))))
                   (else (call $raise-check-byte (local.get $b)) (unreachable)))
               ;; Fill the byte array
               (local.set $arr (struct.get $Bytes $bs (local.get $bs)))
               (call $i8array-fill! (local.get $arr) (local.get $val))
               ;; Return void
               (global.get $void))

         (func $bytes-append
               (param $b1 (ref eq))
               (param $b2 (ref eq))
               (result (ref eq))

               (local $bs1 (ref null $Bytes))
               (local $bs2 (ref null $Bytes))
               (local $a1  (ref $I8Array))
               (local $a2  (ref $I8Array))
               (local $new (ref $I8Array))
               ;; Check both arguments are byte strings
               (if (ref.test (ref $Bytes) (local.get $b1))
                   (then (local.set $bs1 (ref.cast (ref $Bytes) (local.get $b1))))
                   (else (call $raise-check-bytes (local.get $b1)) (unreachable)))
               (if (ref.test (ref $Bytes) (local.get $b2))
                   (then (local.set $bs2 (ref.cast (ref $Bytes) (local.get $b2))))
                   (else (call $raise-check-bytes (local.get $b2)) (unreachable)))
               ;; Extract the underlying arrays
               (local.set $a1 (struct.get $Bytes $bs (local.get $bs1)))
               (local.set $a2 (struct.get $Bytes $bs (local.get $bs2)))
               ;; Call append function on the I8Arrays
               (local.set $new (call $i8array-append (local.get $a1) (local.get $a2)))
               ;; Wrap in new mutable Bytes struct
               (struct.new $Bytes
                           (i32.const 0) ;; hash
                           (i32.const 0) ;; mutable
                           (local.get $new)))

         (func $bytes->list
               (param $bstr (ref eq))
               (result (ref eq))
               (local $bs   (ref null $Bytes))
               (local $arr  (ref $I8Array))
               (local $len  i32)
               (local $i    i32)
               (local $val  i32)
               (local $acc  (ref eq))
               ;; Check input is a byte string
               (if (ref.test (ref $Bytes) (local.get $bstr))
                   (then (local.set $bs (ref.cast (ref $Bytes) (local.get $bstr))))
                   (else (call $raise-check-bytes (local.get $bstr)) (unreachable)))
               ;; Extract underlying byte array
               (local.set $arr (struct.get $Bytes $bs (local.get $bs)))
               ;; Get its length
               (local.set $len (call $i8array-length (local.get $arr)))
               ;; Build list in reverse
               (local.set $i (i32.sub (local.get $len) (i32.const 1)))
               (local.set $acc (global.get $null))
               (block $done
                      (loop $loop
                            (br_if $done (i32.lt_s (local.get $i) (i32.const 0)))
                            (local.set $val (call $i8array-ref (local.get $arr) (local.get $i)))
                            (local.set $acc
                                       (struct.new $Pair
                                                   (i32.const 0)
                                                   (ref.i31 (i32.shl (local.get $val) (i32.const 1))) ;; encode fixnum
                                                   (local.get $acc)))
                            (local.set $i (i32.sub (local.get $i) (i32.const 1)))
                            (br $loop)))
               (local.get $acc))

         (func $list->bytes
               (param $xs (ref eq))
               (result (ref eq))

               (local $bs   (ref $Bytes))
               (local $arr  (ref $I8Array))
               (local $i    i32)
               (local $len  i32)
               (local $x    (ref eq))
               (local $v    i32)
               ;; Step 1: Compute length of list
               (local.set $len (call $length/i32 (local.get $xs)))
               ;; Step 2: Allocate mutable byte array of length $len
               (local.set $arr (call $i8make-array (local.get $len) (i32.const 0)))
               ;; Step 3: Allocate mutable Bytes struct
               (local.set $bs  (struct.new $Bytes
                                           (i32.const 0)
                                           (i32.const 0)
                                           (local.get $arr)))
               ;; Step 4: Iterate through list and populate byte array
               (local.set $i (i32.const 0))
               (block $done
                      (loop $loop
                            (br_if $done (ref.eq (local.get $xs) (global.get $null)))
                            (if (ref.test (ref $Pair) (local.get $xs))
                                (then
                                 (local.set $x (struct.get $Pair $a (ref.cast (ref $Pair) (local.get $xs))))
                                 (if (ref.test (ref i31) (local.get $x))
                                     (then
                                      (local.set $v (i31.get_u (ref.cast (ref i31) (local.get $x))))
                                      (if (i32.eqz (i32.and (local.get $v) (i32.const 1)))
                                          (then
                                           (local.set $v (i32.shr_u (local.get $v) (i32.const 1)))
                                           (if (i32.lt_u (local.get $v) (i32.const 256))
                                               (then
                                                (call $i8array-set! (local.get $arr) (local.get $i) (local.get $v))
                                                (local.set $i (i32.add (local.get $i) (i32.const 1))))
                                               (else
                                                (call $raise-byte-out-of-range (local.get $x))
                                                (unreachable))))
                                          (else
                                           (call $raise-check-fixnum (local.get $x))
                                           (unreachable))))
                                     (else
                                      (call $raise-check-fixnum (local.get $x))
                                      (unreachable)))
                                 (local.set $xs (struct.get $Pair $d (ref.cast (ref $Pair) (local.get $xs)))))
                                (else
                                 (call $raise-pair-expected (local.get $xs))
                                 (unreachable)))
                            (br $loop)))
               ;; Step 5: Return the byte string struct
               (local.get $bs))


         (func $raise-bytes->string/utf-8                  (unreachable))
         (func $raise-bytes->string/utf-8:invalid-err-char (unreachable))
         
         (func $bytes->string/utf-8
               (param $bstr      (ref eq))
               (param $err-char  (ref eq))
               (param $start-raw (ref eq))
               (param $end-raw   (ref eq))
               (result (ref $String))

               (local $bs               (ref null $Bytes))
               (local $start            i32)
               (local $end              i32)
               (local $use-err-char     i32)
               (local $decoded-err-char i32)

               ;; Cast input to $Bytes
               (if (ref.test (ref $Bytes) (local.get $bstr))
                   (then (local.set $bs (ref.cast (ref $Bytes) (local.get $bstr))))
                   (else (call $raise-bytes->string/utf-8)))
               ;; Decode start
               (if (ref.eq (local.get $start-raw) (global.get $false))
                   (then (local.set $start (i32.const 0)))
                   (else
                    (if (ref.test (ref i31) (local.get $start-raw))
                        (then (local.set $start (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $start-raw))) (i32.const 1))))
                        (else (call $raise-bytes->string/utf-8)))))
               ;; Decode end
               (if (ref.eq (local.get $end-raw) (global.get $false))
                   (then (local.set $end (array.len (struct.get $Bytes $bs (local.get $bs)))))
                   (else
                    (if (ref.test (ref i31) (local.get $end-raw))
                        (then (local.set $end (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $end-raw))) (i32.const 1))))
                        (else (call $raise-bytes->string/utf-8)))))
               ;; Decode err-char
               (if (ref.eq (local.get $err-char) (global.get $false))
                   (then (local.set $use-err-char     (i32.const 0))  ; don't use err-char
                         (local.set $decoded-err-char (i32.const 0))) 
                   (else (if (ref.test (ref i31) (local.get $err-char))
                             (then (local.set $use-err-char (i32.const 1))
                                   (local.set $decoded-err-char
                                              (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $err-char)))
                                                         (i32.const 1))))
                             (else (call $raise-bytes->string/utf-8:invalid-err-char)))))               
               ;; Delegate to implementation
               (call $bytes->string/utf-8:work
                     (ref.as_non_null (local.get $bs))
                     (local.get $use-err-char)
                     (local.get $decoded-err-char)
                     (local.get $start)
                     (local.get $end)))

         (func $bytes->string/utf-8/defaults
               (param $bs (ref $Bytes))
               (result (ref $String))
               (call $bytes->string/utf-8
                     (local.get $bs)
                     (global.get $false)   ;; err-char = #f
                     (global.get $false)   ;; start = #f → 0
                     (global.get $false))) ;; end = #f → full length

         (func $bytes->string/utf-8/checked
               (param $bs (ref $Bytes))
               (result (ref $String))
               (local $end i32)
               (local.set $end (array.len (struct.get $Bytes $bs (local.get $bs))))
               (call $bytes->string/utf-8:work
                     (local.get $bs)
                     (i32.const 0)   ;; use-err-char? = false
                     (i32.const 0)   ;; err-char = dummy
                     (i32.const 0)   ;; start
                     (local.get $end)))

         (func $bytes->string/utf-8:work
               (param $bs           (ref $Bytes))
               (param $use-err-char i32)
               (param $err-char     i32)
               (param $start        i32)
               (param $end          i32)
               (result              (ref $String))

               (local $buf  (ref $I32GrowableArray))
               (local $arr  (ref $I8Array))
               (local $i    i32)
               (local $byte i32)
               (local $need i32)
               (local $acc  i32)
               (local $b2   i32)
               (local $cp   i32)

               ;; Get underlying I8Array from Bytes
               (local.set $arr (struct.get $Bytes $bs (local.get $bs)))
               ;; Allocate buffer for codepoints
               (local.set $buf (call $make-i32growable-array (i32.const 16)))
               ;; Start decoding loop
               (local.set $i (local.get $start))
               (block $done
                      (loop $loop
                            (br_if $done (i32.ge_u (local.get $i) (local.get $end)))
                            (local.set $byte (array.get_u $I8Array (local.get $arr) (local.get $i)))
                            ;; ASCII fast path
                            (if (i32.lt_u (local.get $byte) (i32.const 128))
                                (then
                                 (call $i32growable-array-add! (local.get $buf) (local.get $byte))
                                 (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                 (br $loop)))
                            ;; Determine UTF-8 sequence size and initial accumulator
                            (call $bytes->string/utf-8:determine-utf-8-sequence (local.get $byte))
                            (local.set $acc) (local.set $need)
                            ;; Invalid lead byte
                            (if (i32.lt_s (local.get $need) (i32.const 0))
                                (then
                                 (if (i32.eqz (local.get $use-err-char))
                                     (then (call $raise-bytes->string/utf-8))
                                     (else
                                      (call $i32growable-array-add! (local.get $buf) (local.get $err-char))
                                      (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                      (br $loop)))))
                            ;; Not enough bytes left
                            (if (i32.gt_u (i32.add (local.get $i) (local.get $need)) (local.get $end))
                                (then
                                 (if (i32.eqz (local.get $use-err-char))
                                     (then (call $raise-bytes->string/utf-8))
                                     (else
                                      (call $i32growable-array-add! (local.get $buf) (local.get $err-char))
                                      (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                      (br $loop)))))
                            ;; Decode continuation bytes
                            (local.set $cp (local.get $acc))
                            (local.set $i (i32.add (local.get $i) (i32.const 1))) ;; skip lead byte
                            (block $cont-fail
                                   (loop $cont-loop
                                         (br_if $cont-fail (i32.eqz (local.get $need)))
                                         (local.set $b2 (array.get_u $I8Array (local.get $arr) (local.get $i)))
                                         (if (i32.and
                                              (i32.ge_u (local.get $b2) (i32.const 128))
                                              (i32.lt_u (local.get $b2) (i32.const 192)))
                                             (then
                                              (local.set $cp
                                                         (i32.or
                                                          (i32.shl (local.get $cp) (i32.const 6))
                                                          (i32.and (local.get $b2) (i32.const 0x3F))))
                                              (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                              (local.set $need (i32.sub (local.get $need) (i32.const 1)))
                                              (br $cont-loop)))))
                            ;; If we didn't finish the sequence, it's invalid
                            (if (i32.ne (local.get $need) (i32.const 0))
                                (then
                                 (if (i32.eqz (local.get $use-err-char))
                                     (then (call $raise-bytes->string/utf-8))
                                     (else
                                      (call $i32growable-array-add! (local.get $buf) (local.get $err-char))
                                      (br $loop)))))
                            ;; Valid sequence
                            (call $i32growable-array-add! (local.get $buf) (local.get $cp))
                            (br $loop)))
               ;; Convert buffer to immutable string
               (call $i32growable-array->immutable-string (local.get $buf)))


         (func $bytes->string/utf-8:determine-utf-8-sequence ; returns two i32s
               ; This function determines how many continuation bytes are needed for
               ; a given UTF-8 lead byte, and extracts the initial bits for the code point accumulator.
               (param $lead i32)
               (result i32  i32) ;; (need acc) or (-1 -1) if invalid

               (if (i32.and (i32.ge_u (local.get $lead) (i32.const 0xC0))
                            (i32.lt_u (local.get $lead) (i32.const 0xE0)))
                   (then (return (i32.const 1) (i32.and (local.get $lead) (i32.const 0x1F)))))
               
               (if (i32.and (i32.ge_u (local.get $lead) (i32.const 0xE0))
                            (i32.lt_u (local.get $lead) (i32.const 0xF0)))
                   (then (return (i32.const 2) (i32.and (local.get $lead) (i32.const 0x0F)))))
               
               (if (i32.and (i32.ge_u (local.get $lead) (i32.const 0xF0))
                            (i32.lt_u (local.get $lead) (i32.const 0xF8)))
                   (then (return (i32.const 3) (i32.and (local.get $lead) (i32.const 0x07)))))
               
               ;; Not a valid lead byte
               (return (i32.const -1) (i32.const -1)))

         ;;;
         ;;; Memory Map
         ;;;

         ;; Currently (June 2005) the proposal for multiple memories aren't supported
         ;; by Safari (WebKit). Therefore, we are forced to using a single memory only.

         ;; The following memory map is used to segment the linear memory.
         
         (global $memory-map:string-buffer-base   (mut i32) (i32.const 0))
         (global $memory-map:string-buffer-length (mut i32) (i32.const 4096))
         

         ;;;
         ;;; Data segment strings
         ;;;

         ;; The current (June 2005) support for strings in WebAssembly is *very* limited.
         ;; The data segment can be used to embed byte sequences in the source.
         ;; However in order to access the data, one must copy them to the linear memory first.

         
         (func $raise-string-buffer-overflow (unreachable))

         ; Turns out that the `memory.init` instruction requires a fixed
         ; segment number. So since we can't pass the segment number as an argument,
         ; we can't use these functions. Sigh.
         
         #;(func $data:codepoints->string
               (param $segment   i32)  ;; index of the passive data segment
               (param $count     i32)  ;; number of UTF-32 codepoints
               (result           (ref $String))
               
               (local $base      i32)
               (local $i         i32)
               (local $arr       (ref $I32Array))
               (local $str       (ref $String))
               (local $byte-size i32)

               ;; 1. Get base address for string buffer
               (local.set $base (global.get $memory-map:string-buffer-base))
               ;; 2. Compute byte size = count * 4
               (local.set $byte-size (i32.mul (local.get $count) (i32.const 4)))
               ;; 3. Bounds check (optional but recommended)
               (if (i32.gt_u (local.get $byte-size)
                             (global.get $memory-map:string-buffer-length))
                   (then (call $raise-string-buffer-overflow)))
               ;; 4. Copy data segment to string buffer
               (memory.init (local.get $segment)
                            (local.get $base)
                            (i32.const 0)
                            (local.get $byte-size))
               ;; 5. Allocate I32Array
               (local.set $arr (array.new_default $I32Array (local.get $count)))
               ;; 6. Copy from memory buffer into array
               (local.set $i (i32.const 0))
               (loop $copy (array.set $I32Array
                                      (local.get $arr)
                                      (local.get $i)
                                      (i32.load
                                       (i32.add
                                        (local.get $base)
                                        (i32.mul (local.get $i) (i32.const 4)))))
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (br_if $copy (i32.lt_u (local.get $i) (local.get $count))))

               ;; 7. Convert to string
               (local.set $str (call $i32array->string (local.get $arr)))
               (local.get $str))

         #;(func $data:bytes->string
               (param $segment    i32) ;; passive data segment index
               (param $byte-count i32) ;; number of UTF-8 bytes
               (result            (ref $String))

               (local $base  i32)
               (local $i     i32)
               (local $arr   (ref $I8Array))
               (local $bs    (ref $Bytes))
               (local $str   (ref $String))
               ;; 1. Get base address for string buffer
               (local.set $base (global.get $memory-map:string-buffer-base))
               ;; 2. Bounds check
               (if (i32.gt_u (local.get $byte-count)
                             (global.get $memory-map:string-buffer-length))
                   (then (call $raise-string-buffer-overflow)))
               ;; 3. Copy from data segment into memory buffer
               (memory.init (local.get $segment)
                            (local.get $base)
                            (i32.const 0)
                            (local.get $byte-count))
               ;; 4. Allocate and fill I8Array
               (local.set $arr (array.new_default $I8Array (local.get $byte-count)))
               (local.set $i (i32.const 0))
               (loop $copy
                     (array.set $I8Array (local.get $arr) (local.get $i)
                                (i32.load8_u
                                 (i32.add (local.get $base)
                                          (local.get $i))))
                     (local.set $i (i32.add (local.get $i) (i32.const 1)))
                     (br_if $copy (i32.lt_u (local.get $i) (local.get $byte-count))))
               ;; 5. Wrap I8Array in Bytes struct
               (local.set $bs (struct.new $Bytes
                                          (i32.const 0)         ;; hash = 0 (unset)
                                          (local.get $byte-count)
                                          (local.get $arr)))
               ;; 6. Decode UTF-8
               (local.set $str (call $bytes->string/utf-8/checked
                                     (local.get $bs)))
               (local.get $str))


         ;;;
         ;;; MUTABLE HASHEQ
         ;;;

         ; We'll use an open-addressing hash table with linear probing for simplicity.
         ; A load of 50% leads to fast lookup - but uses some more memory.

         ; Theory: https://thenumb.at/Hashtables/

         (func $make-empty-hasheq (result (ref $HashEqMutable))
               (local $entries (ref $Array))
               ;; Step 1: Allocate an array with 2 × capacity (key/value pairs)
               ;; Capacity = 16 entries → 32 elements
               (local.set $entries
                          (array.new $Array (global.get $missing) (i32.const 32)))
               ;; Step 2: Construct and return the hashtable struct
               (struct.new $HashEqMutable
                           (i32.const 0)                 ;; hash = 0 (placeholder or unused)
                           (global.get $true)            ;; mutable? = #t
                           (local.get $entries)          ;; entries array
                           (i32.const 0)))               ;; count = 0

         (func $raise-argument-error:pair-expected  (unreachable))
         (func $raise-argument-error:pair-expected1 (unreachable))
         (func $raise-argument-error:pair-expected2 (unreachable))
         
         (func $make-hasheq ; (make-hasheq [assocs])   - optional without default 
               (param $assocs (ref eq))       ;; Either $missing or an alist of key/value pairs
               (result (ref $HashEqMutable))

               (local $alist (ref eq))
               (local $pair  (ref $Pair))
               (local $key   (ref eq))
               (local $val   (ref eq))
               (local $first (ref eq)) ; of the alist
               (local $rest  (ref eq)) ; of the alist
               (local $ht    (ref $HashEqMutable))

               ;; Case 1: No argument => make empty table
               (if (ref.eq (local.get $assocs) (global.get $missing))
                   (then (return (call $make-empty-hasheq))))
               ;; Case 2: Provided association list
               (local.set $ht    (call $make-empty-hasheq))
               (local.set $alist (local.get $assocs))

               (block $done
                      (loop $walk
                            ;; Stop when list is null
                            (br_if $done (ref.eq (local.get $alist) (global.get $null)))
                            ;; Must be a pair
                            (if (i32.eqz (ref.test (ref $Pair) (local.get $alist)))
                                (then (call $raise-argument-error:pair-expected1 (local.get $alist)) (unreachable)))
                            (local.set $pair (ref.cast (ref $Pair) (local.get $alist)))
                            ;; Extract car and cdr of current pair
                            (local.set $first (struct.get $Pair $a (local.get $pair))) ;; first = key/value pair
                            (local.set $rest  (struct.get $Pair $d (local.get $pair))) ;; rest  = remaining pairings
                            ;; Validate first is a pair (key . value)
                            (if (i32.eqz (ref.test (ref $Pair) (local.get $first)))
                                (then (call $raise-argument-error:pair-expected2 (local.get $first)) (unreachable)))
                            ;; Extract key and value from nested pair
                            (local.set $pair (ref.cast (ref $Pair) (local.get $first)))
                            (local.set $key  (struct.get $Pair $a (local.get $pair))) ;; key
                            (local.set $val  (struct.get $Pair $d (local.get $pair))) ;; value
                            ;; Insert into table
                            (call $hasheq-set!/mutable/checked (local.get $ht) (local.get $key) (local.get $val))
                            ;; Move to next element in alist
                            (local.set $alist (local.get $rest))
                            (br $walk)))
               (local.get $ht))
         

         (func $raise-argument-error:hash-expected (unreachable))
         
         (func $hash-ref
               (param $ht      (ref eq))   ;; must be a mutable hasheq
               (param $key     (ref eq))
               (param $failure (ref eq))   ;; failure result
               (result         (ref eq))

               ;; Check type: must be (ref $Hash)
               (if (i32.eqz (ref.test (ref $Hash) (local.get $ht)))
                   (then (call $raise-argument-error:hash-expected)
                         (unreachable)))
               ;; Mutable or immutable?
               (if (ref.test (ref $HashEqMutable) (local.get $ht))
                   (then (return (call $hasheq-ref
                                       (local.get $ht) (local.get $key) (local.get $failure)))))
               (unreachable))
         
         (func $hasheq-ref
               (param $ht      (ref eq))     ;; hasheq
               (param $key     (ref eq))     ;; lookup key
               (param $failure (ref eq))     ;; value to return if not found
               (result         (ref eq))
               
               (if (ref.eq (local.get $failure) (global.get $missing))
                   (then (local.set $failure (global.get $false))))
               (return_call $hasheq-ref/plain
                            (local.get $ht) (local.get $key) (local.get $failure)))

         (func $raise-argument-error:hasheq-expected (unreachable))
         
         (func $hasheq-ref/plain
               (param $ht      (ref eq))     ;; hasheq
               (param $key     (ref eq))     ;; lookup key
               (param $failure (ref eq))     ;; value to return if not found
               (result         (ref eq))

               (local $table (ref $HashEqMutable))

               ;; Check that ht is a mutable hasheq
               (if (i32.eqz (ref.test (ref $HashEqMutable) (local.get $ht)))
                   (then (call $raise-argument-error:hasheq-expected (local.get $ht))
                         (unreachable)))

               ;; Decode
               (local.set $table (ref.cast (ref $HashEqMutable) (local.get $ht)))

               ;; Delegate to checked implementation
               (call $hasheq-ref/plain/checked
                     (local.get $table)
                     (local.get $key)
                     (local.get $failure)))

         (func $hasheq-ref/plain/checked
               (param $table (ref $HashEqMutable))
               (param $key   (ref eq))
               (param $fail  (ref eq))
               (result       (ref eq))

               (local $entries  (ref $Array))
               (local $capacity i32)
               (local $index    i32)
               (local $step     i32)
               (local $hash     i32)
               (local $k        (ref eq))
               (local $slot     i32)

               ;; Get entries and compute capacity
               (local.set $entries  (struct.get $HashEqMutable $entries (local.get $table)))
               (local.set $capacity (i32.div_u (array.len (local.get $entries)) (i32.const 2)))
               ;; Hash key (identity hash)
               (local.set $hash  (call $eq-hash/i32 (local.get $key)))
               (local.set $index (i32.rem_u (local.get $hash) (local.get $capacity)))
               (local.set $step  (i32.const 0))
               (block $not-found
                      (loop $probe
                            ;; Stop probing if we've checked all slots
                            (br_if $not-found (i32.ge_u (local.get $step) (local.get $capacity)))
                            ;; slot = 2 * ((index + step) % capacity)
                            (local.set $slot
                                       (i32.shl
                                        (i32.rem_u
                                         (i32.add (local.get $index) (local.get $step))
                                         (local.get $capacity))
                                        (i32.const 1)))
                            ;; Get key at slot
                            (local.set $k (array.get $Array (local.get $entries) (local.get $slot)))
                            ;; Empty slot means not found
                            (br_if $not-found (ref.eq (local.get $k) (global.get $missing)))
                            ;; Tombstone? — skip and continue probing
                            (if (ref.eq (local.get $k) (global.get $tombstone))
                                (then
                                 (local.set $step (i32.add (local.get $step) (i32.const 1)))
                                 (br $probe)))
                            ;; Match? — return value at slot + 1
                            (if (ref.eq (local.get $k) (local.get $key))
                                (then
                                 (return
                                  (array.get $Array
                                             (local.get $entries)
                                             (i32.add (local.get $slot) (i32.const 1))))))
                            ;; Continue probing
                            (local.set $step (i32.add (local.get $step) (i32.const 1)))
                            (br $probe)))
               ;; Not found — return failure result
               (local.get $fail))


         (func $raise-argument-error:mutable-hasheq-expected (unreachable))
         
         (func $hash-set!
               (param $ht  (ref eq))   ;; must be a mutable hasheq
               (param $key (ref eq))
               (param $val (ref eq))
               (result     (ref eq)) ;; return void (an immediate)

               (local $table (ref $HashEqMutable))

               ;; Check type: must be (ref $HashEqMutable)
               (if (i32.eqz (ref.test (ref $HashEqMutable) (local.get $ht)))
                   (then (call $raise-argument-error:mutable-hasheq-expected (local.get $ht))
                         (unreachable)))
               ;; Cast after check
               (local.set $table (ref.cast (ref $HashEqMutable) (local.get $ht)))
               ;; Delegate
               (call $hasheq-set!/mutable/checked
                     (local.get $table)
                     (local.get $key)
                     (local.get $val))
               ;; Return void
               (global.get $void))

         (func $raise-hasheq-insert:table-full (unreachable))
         
         (func $hasheq-set!/mutable/checked
               (param $table (ref $HashEqMutable))
               (param $key   (ref eq))
               (param $val   (ref eq))

               (local $entries        (ref $Array))
               (local $capacity       i32)
               (local $hash           i32)
               (local $index          i32)
               (local $step           i32)
               (local $slot           i32)
               (local $k              (ref eq))
               (local $first-tombstone i32)

               ;; Maybe resize
               (local.set $table    (call $maybe-resize-hasheq (local.get $table)))
               ;; Get entries and capacity
               (local.set $entries  (struct.get $HashEqMutable $entries (local.get $table)))
               (local.set $capacity (i32.div_u (array.len (local.get $entries)) (i32.const 2)))
               ;; Compute initial hash/index
               (local.set $hash  (call $eq-hash/i32 (local.get $key)))
               (local.set $index (i32.rem_u (local.get $hash) (local.get $capacity)))
               (local.set $step  (i32.const 0))
               ;; No tombstone seen yet
               (local.set $first-tombstone (i32.const -1))
               (block $done
                      (block $full
                             (loop $probe
                                   (br_if $full (i32.ge_u (local.get $step) (local.get $capacity)))
                                   ;; Compute probe slot = 2 * ((index + step) % capacity)
                                   (local.set $slot (i32.shl
                                               (i32.rem_u (i32.add (local.get $index) (local.get $step))
                                                          (local.get $capacity))
                                               (i32.const 1)))
                                   ;; Load key at slot
                                   (local.set $k (array.get $Array (local.get $entries) (local.get $slot)))
                                   ;; First tombstone — record slot
                                   (if (ref.eq (local.get $k) (global.get $tombstone))
                                       (then (if (i32.eq (local.get $first-tombstone) (i32.const -1))
                                                 (then (local.set $first-tombstone (local.get $slot))))))
                                   ;; Empty — insert into tombstone if available, else here
                                   (if (ref.eq (local.get $k) (global.get $missing))
                                       (then
                                        (local.set $slot
                                                   (select
                                                    (local.get $slot)
                                                    (local.get $first-tombstone)
                                                    (i32.eq (local.get $first-tombstone) (i32.const -1))))

                                        (array.set $Array (local.get $entries) (local.get $slot) (local.get $key))
                                        (array.set $Array
                                                   (local.get $entries)
                                                   (i32.add (local.get $slot) (i32.const 1))
                                                   (local.get $val))
                                        (struct.set $HashEqMutable $count
                                                    (local.get $table)
                                                    (i32.add (struct.get $HashEqMutable $count (local.get $table)) (i32.const 1)))
                                        (br $done)))
                                   ;; Key match — overwrite value
                                   (if (ref.eq (local.get $k) (local.get $key))
                                       (then
                                        (array.set $Array
                                                   (local.get $entries)
                                                   (i32.add (local.get $slot) (i32.const 1))
                                                   (local.get $val))
                                        (br $done)))
                                   ;; Next probe
                                   (local.set $step (i32.add (local.get $step) (i32.const 1)))
                                   (br $probe)))
                      ;; Table full (no missing or tombstone slots available)
                      (call $raise-hasheq-insert:table-full)))



         (func $raise-argument-error:hasheq-mutable-expected (unreachable))
         
         (func $hash-remove!
               (param $ht  (ref eq))
               (param $key (ref eq))
               (result     (ref eq))
               (call $hash-remove!/mutable
                     (local.get $ht) (local.get $key)))
         
         (func $hash-remove!/mutable
               (param $ht  (ref eq))
               (param $key (ref eq))
               (result     (ref eq))

               (local $table (ref $HashEqMutable))
               
               ;; --- Type checks ---
               (if (i32.eqz (ref.test (ref $HashEqMutable) (local.get $ht)))
                   (then
                    (call $raise-argument-error:hasheq-mutable-expected (local.get $ht))
                    (unreachable)))
               ;; --- Decode ---
               (local.set $table (ref.cast (ref $HashEqMutable) (local.get $ht)))
               ;; --- Delegate ---
               (call $hash-remove!/mutable/checked
                     (local.get $table)
                     (local.get $key))
               (global.get $void))

         (func $hash-remove!/mutable/checked
               ; Note: (global $tombstone) must be different from valid keys and $missing.
               (param $ht  (ref $HashEqMutable))
               (param $key (ref eq))

               (local $entries  (ref $Array))
               (local $capacity i32)
               (local $hash     i32)
               (local $index    i32)
               (local $step     i32)
               (local $k        (ref eq))
               (local $slot     i32)

               ;; Get entries and capacity
               (local.set $entries  (struct.get $HashEqMutable $entries (local.get $ht)))
               (local.set $capacity (i32.div_u (array.len (local.get $entries)) (i32.const 2)))
               ;; Compute hash and initial index
               (local.set $hash  (call $eq-hash/i32 (local.get $key)))
               (local.set $index (i32.rem_u (local.get $hash) (local.get $capacity)))
               (local.set $step  (i32.const 0))
               ;; Probe loop
               (block $done
                      (loop $probe
                            ;; Stop if probing exceeds capacity
                            (br_if $done (i32.ge_u (local.get $step) (local.get $capacity)))
                            ;; Compute probe slot (2 * index)
                            (local.set $slot (i32.shl
                                              (i32.rem_u (i32.add (local.get $index) (local.get $step))
                                                         (local.get $capacity))
                                              (i32.const 1)))
                            ;; Load key from slot
                            (local.set $k (array.get $Array (local.get $entries) (local.get $slot)))
                            ;; Stop if slot is missing
                            (br_if $done (ref.eq (local.get $k) (global.get $missing)))
                            ;; If key matches, remove
                            (if (ref.eq (local.get $k) (local.get $key))
                                (then
                                 ;; Replace key and value with tombstone
                                 (array.set $Array (local.get $entries) (local.get $slot) (global.get $tombstone))
                                 (array.set $Array (local.get $entries)
                                            (i32.add (local.get $slot) (i32.const 1))
                                            (global.get $tombstone))
                                 ;; Decrement count
                                 (struct.set $HashEqMutable $count
                                             (local.get $ht)
                                             (i32.sub (struct.get $HashEqMutable $count (local.get $ht)) (i32.const 1)))
                                 (br $done)))
                            ;; Step to next slot
                            (local.set $step (i32.add (local.get $step) (i32.const 1)))
                            (br $probe))))

         (func $hasheq-resize
               (param $old (ref $HashEqMutable))
               (result (ref $HashEqMutable))

               (local $old-entries (ref $Array))
               (local $old-cap     i32)
               (local $new-cap     i32)
               (local $new-array   (ref $Array))
               (local $new-table   (ref $HashEqMutable))
               (local $i           i32)
               (local $len         i32)
               (local $key         (ref eq))
               (local $val         (ref eq))

               ;; Get old table size and entries
               (local.set $old-entries (struct.get $HashEqMutable $entries (local.get $old)))
               (local.set $old-cap     (i32.div_u (array.len (local.get $old-entries)) (i32.const 2)))
               (local.set $new-cap     (i32.mul (local.get $old-cap) (i32.const 2)))               
               ;; Allocate new entries array (2 * new-capacity), all set to $missing
               (local.set $new-array (array.new $Array (global.get $missing)
                                                (i32.mul (local.get $new-cap) (i32.const 2))))
               ;; Create new empty table (copy mutability from old table)
               (local.set $new-table
                          (struct.new $HashEqMutable
                                      (i32.const 0)
                                      (struct.get $HashEqMutable $mutable? (local.get $old))
                                      (local.get $new-array)
                                      (i32.const 0)))
               ;; Reinsert valid key-value pairs
               (local.set $i   (i32.const 0))
               (local.set $len (array.len (local.get $old-entries)))
               (block $done
                      (loop $loop
                            (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
                            ;; Load key and value
                            (local.set $key (array.get $Array (local.get $old-entries) (local.get $i)))
                            (local.set $val (array.get $Array (local.get $old-entries)
                                                       (i32.add (local.get $i) (i32.const 1))))
                            ;; Reinsert only if key is not $missing or $tombstone
                            (if (i32.eqz (ref.eq (local.get $key) (global.get $missing)))
                                (then (if (i32.eqz (ref.eq (local.get $key) (global.get $tombstone)))
                                          (then
                                           (call $hasheq-set!/mutable/checked
                                                 (local.get $new-table)
                                                 (local.get $key)
                                                 (local.get $val))))))
                            ;; Next pair
                            (local.set $i (i32.add (local.get $i) (i32.const 2)))
                            (br $loop)))
               (local.get $new-table))

         (func $maybe-resize-hasheq
               (param $table (ref $HashEqMutable))
               (result (ref $HashEqMutable))

               (local $entries  (ref $Array))
               (local $capacity i32)
               (local $count    i32)
               ;; Get fields
               (local.set $entries  (struct.get $HashEqMutable $entries (local.get $table)))
               (local.set $capacity (i32.div_u (array.len (local.get $entries)) (i32.const 2)))
               (local.set $count    (struct.get $HashEqMutable $count (local.get $table)))
               ;; Resize if count ≥ capacity / 2
               (if (i32.ge_u (local.get $count)
                             (i32.shr_u (local.get $capacity) (i32.const 1)))
                   (then (return (call $hasheq-resize (local.get $table)))))
               (local.get $table))


         (func $hash-has-key?
               (param $ht  (ref eq))
               (param $key (ref eq))
               (result     (ref eq))

               (if (i32.eqz (ref.test (ref $HashEqMutable) (local.get $ht)))
                   (then (call $raise-argument-error:hasheq-mutable-expected (local.get $ht))
                         (unreachable)))
               (call $eqhash-has-key?/checked
                     (ref.cast (ref $HashEqMutable) (local.get $ht))
                     (local.get $key)))

         (func $eqhash-has-key?/checked
               (param $table (ref $HashEqMutable))
               (param $key   (ref eq))
               (result       (ref eq))

               (local $entries  (ref $Array))
               (local $capacity i32)
               (local $index    i32)
               (local $step     i32)
               (local $hash     i32)
               (local $k        (ref eq))
               (local $slot     i32)
               ;; Get entries and capacity
               (local.set $entries  (struct.get $HashEqMutable $entries (local.get $table)))
               (local.set $capacity (i32.div_u (array.len (local.get $entries)) (i32.const 2)))
               ;; Hash key and compute initial index
               (local.set $hash  (call $eq-hash/i32 (local.get $key)))
               (local.set $index (i32.rem_u (local.get $hash) (local.get $capacity)))
               (local.set $step  (i32.const 0))
               (block $not-found
                      (loop $probe
                            ;; Stop if we've probed the full table
                            (br_if $not-found (i32.ge_u (local.get $step) (local.get $capacity)))
                            ;; slot = 2 * ((index + step) % capacity)
                            (local.set $slot (i32.shl (i32.rem_u (i32.add (local.get $index) (local.get $step))
                                                                 (local.get $capacity))
                                                      (i32.const 1)))
                            ;; Load key from slot
                            (local.set $k (array.get $Array (local.get $entries) (local.get $slot)))
                            ;; Empty slot: key is not in the table
                            (br_if $not-found (ref.eq (local.get $k) (global.get $missing)))
                            ;; Tombstone: skip
                            (if (ref.eq (local.get $k) (global.get $tombstone))
                                (then (local.set $step (i32.add (local.get $step) (i32.const 1))) (br $probe)))
                            ;; Match: return #t
                            (if (ref.eq (local.get $k) (local.get $key))
                                (then (return (global.get $true))))
                            ;; Otherwise try next slot
                            (local.set $step (i32.add (local.get $step) (i32.const 1)))
                            (br $probe)))
               ;; Not found: return #f
               (global.get $false))

         (func $hash-clear!
               (param $ht (ref eq))
               (result    (ref eq))
               ;; Type check
               (if (i32.eqz (ref.test (ref $HashEqMutable) (local.get $ht)))
                   (then (call $raise-argument-error:hasheq-mutable-expected (local.get $ht))
                         (unreachable)))
               ;; Cast and delegate
               (call $eqhash-clear!/checked
                     (ref.cast (ref $HashEqMutable) (local.get $ht)))
               (global.get $void))

         (func $eqhash-clear!/checked
               (param $ht (ref $HashEqMutable))

               (local $new-entries (ref $Array))
               ;; Allocate fresh array of default size (16 entries = 32 slots)
               (local.set $new-entries (array.new $Array (global.get $missing) (i32.const 32)))
               ;; Replace entries array
               (struct.set $HashEqMutable $entries (local.get $ht) (local.get $new-entries))
               ;; Reset count to 0
               (struct.set $HashEqMutable $count (local.get $ht) (i32.const 0)))






         
         ;;;
         ;;; HASH CODES
         ;;;

         (func $eq-hash-code
               (param $v (ref eq))
               (result (ref eq))

               (ref.i31
                (i32.shl (call $eq-hash/i32
                               (local.get $v))
                 (i32.const 1))))

         
         (func $eq-hash/i32
               (param $v (ref eq))
               (result   i32)

               (local $v-i31 i32)
               (local $heap  (ref $Heap))
               (local $h     i32)
               (local $f     (ref $Flonum))
               (local $bits i64)
               (local $low  i32)
               (local $high i32)
               (local $x    i32)

               (if (result i32)
                   (ref.test (ref i31) (local.get $v))
                   (then
                    ;; --- Mix i31 immediate using Murmur3-style scramble ---
                    (local.set $v-i31 (i31.get_u (ref.cast (ref i31) (local.get $v))))
                    (i32.mul
                     (i32.rotl
                      (i32.mul (local.get $v-i31) (i32.const 0xcc9e2d51))
                      (i32.const 15))
                     (i32.const 0x1b873593)))
                   ;; Notes:
                   ;;   If the special case is commented out, then eq-hash-code
                   ;;   will always produce the same hash code for the same flonum value.
                   ;;   But that is not enough to make flonums work with $HashEQ since,
                   ;;   we are using (ref.eq ...) to test for equality.
                   ;; (else
                   ;;  ;; --- Special case: flonum ---
                   ;;  (if (result i32)
                   ;;      (ref.test (ref $Flonum) (local.get $v))
                   ;;      (then
                   ;;       (local.set $f    (ref.cast (ref $Flonum) (local.get $v)))
                   ;;       (local.set $bits (i64.reinterpret_f64
                   ;;                         (struct.get $Flonum $v (local.get $f))))
                   ;;       (local.set $low  (i32.wrap_i64 (local.get $bits)))
                   ;;       (local.set $high (i32.wrap_i64 (i64.shr_u (local.get $bits) (i64.const 32))))
                   ;;       (local.set $x    (i32.xor (local.get $low) (local.get $high)))
                   ;;       ;; Murmur3 mix again
                   ;;       (i32.mul
                   ;;        (i32.rotl
                   ;;         (i32.mul (local.get $x) (i32.const 0xcc9e2d51))
                   ;;         (i32.const 15))
                   ;;        (i32.const 0x1b873593)))
                        (else
                         ;; --- Heap object: return or assign hash without mixing for now ---
                         (local.set $heap (ref.cast (ref $Heap) (local.get $v)))
                         (local.set $h (struct.get $Heap $hash (local.get $heap)))
                         (if (result i32)
                             (i32.eqz (local.get $h))
                             (then (local.set $h (call $splitmix32))                         
                                   (struct.set $Heap $hash (local.get $heap) (local.get $h))
                                   (local.get $h))
                             (else (local.get $h))))))

         (global $next-hash-state (mut i32) (i32.const 0x9e3779b9)) ;; initial seed, can be randomized or fixed
         
         (func $splitmix32 (result i32)
               (local $z i32)
               ;; z = state += 0x9E3779B9
               (local.set $z (i32.add (global.get $next-hash-state) (i32.const 0x9E3779B9)))
               (global.set $next-hash-state (local.get $z))
               ;; z ^= (z >> 16)
               (local.set $z (i32.xor (local.get $z) (i32.shr_u (local.get $z) (i32.const 16))))
               ;; z *= 0x85EBCA6B
               (local.set $z (i32.mul (local.get $z) (i32.const 0x85EBCA6B)))
               ;; z ^= (z >> 13)
               (local.set $z (i32.xor (local.get $z) (i32.shr_u (local.get $z) (i32.const 13))))
               ;; z *= 0xC2B2AE35
               (local.set $z (i32.mul (local.get $z) (i32.const 0xC2B2AE35)))
               ;; z ^= (z >> 16)
               (local.set $z (i32.xor (local.get $z) (i32.shr_u (local.get $z) (i32.const 16))))
               (local.get $z))




         
         ;;;
         ;;; SYMBOL TABLE
         ;;;

         ; We'll use an open-addressing hash table with linear probing for simplicity.
         ; A load of 50% leads to fast lookup - but uses some more memory.

         ; Theory: https://thenumb.at/Hashtables/

         ; We do not support deleting symbols from the symbol table, so we do not
         ; need to handle tombstones.
         
         (type $SymbolTable
               (struct
                 (field $entries (mut (ref $Array))) ;; flat array: key0, val0, key1, val1, ...
                 (field $count   (mut i32))))        ;; number of symbols currently stored

         ; The $Array is a flat array: [key0 val0 key1 val1 ...].
         ; Capacity is half the array length (since entries are key-value pairs).
         ; Keys are (ref $String), values are (ref $Symbol).
         ; Count is number of active entries (not tombstones).

         (func $make-symbol-table (result (ref $SymbolTable))
               (local $entries  (ref $Array))
               ;; Initial capacity is 16, so total entries = 2 * 16 = 32
               (local.set $entries
                          (array.new $Array (global.get $missing) (i32.const 1024))) ; todo: was 32
               (struct.new $SymbolTable
                           (local.get $entries)  ;; $entries
                           (i32.const 0)))       ;; $count

         (func $symbol-table-find ; returns $missing for "not found"
               (param $table (ref $SymbolTable))
               (param $key   (ref $String))
               (result       (ref eq))

               (local $entries  (ref $Array))
               (local $capacity i32)
               (local $hash     i32)
               (local $index    i32)
               (local $step     i32)
               (local $k        (ref eq))
               ;; Get the entries array and capacity
               (local.set $entries  (struct.get $SymbolTable $entries (local.get $table)))
               (local.set $capacity (i32.div_u (array.len (local.get $entries)) (i32.const 2)))
               ;; Compute hash and initial index
               (local.set $hash  (call $string-hash/i32 (local.get $key)))
               (local.set $index (i32.rem_u (local.get $hash) (local.get $capacity)))
               (local.set $step  (i32.const 0))

               (block $not-found
                      (loop $probe
                            ;; Stop probing if we’ve gone through every slot
                            (br_if $not-found (i32.ge_u (local.get $step) (local.get $capacity)))
                            ;; Compute probe index: (index + step) % capacity
                            (local.set $index (i32.rem_u (i32.add (local.get $index) (local.get $step))
                                                         (local.get $capacity)))
                            ;; Load key at 2 * index
                            (local.set $k
                                       (array.get $Array
                                                  (local.get $entries)
                                                  (i32.shl (local.get $index) (i32.const 1)))) ;; 2 * index
                            ;; If slot is unused, stop
                            (br_if $not-found
                                   (ref.eq (local.get $k) (global.get $missing)))
                            ;; If match, return value at index + 1
                            (if #;(or (ref.eq (local.get $k) (local.get $key))
                                      (call $string=?/i32 (local.get $k) (local.get $key)))
                                (call $string=?/i32 (local.get $k) (local.get $key))
                                (then
                                 (return ; returns value at index 2*index+1.
                                  (array.get $Array
                                             (local.get $entries)
                                             (i32.add (i32.shl (local.get $index) (i32.const 1))
                                                      (i32.const 1))))))
                            ;; Try next slot
                            (local.set $step (i32.add (local.get $step) (i32.const 1)))
                            (br $probe)))
               ;; Not found
               (global.get $missing))

         (func $raise-symbol-table-insert:table-full (unreachable))
         
         (func $symbol-table-insert
               (param $table (ref $SymbolTable))
               (param $key   (ref $String))  ;; must be interned or immutable
               (param $val   (ref eq))

               (local $entries  (ref $Array))
               (local $capacity i32)
               (local $hash     i32)
               (local $index    i32)
               (local $step     i32)
               (local $k        (ref eq))
               (local $slot     i32)
               ;; Possibly resize before inserting
               (local.set $table (call $maybe-resize-symbol-table (local.get $table)))
               ;; Get updated entries array and capacity
               (local.set $entries  (struct.get $SymbolTable $entries (local.get $table)))
               (local.set $capacity (i32.div_u (array.len (local.get $entries)) (i32.const 2)))
               ;; Compute hash and initial index
               (local.set $hash  (call $string-hash/i32 (local.get $key)))
               (local.set $index (i32.rem_u (local.get $hash) (local.get $capacity)))
               (local.set $step  (i32.const 0))
               
               (block $done
                      (block $full
                             (loop $probe
                                   ;; Stop if probing exceeds capacity (should never happen if resize worked)
                                   (br_if $full (i32.ge_u (local.get $step) (local.get $capacity)))
                                   ;; Compute probe index and actual slot (2 * index)
                                   (local.set $slot (i32.shl (i32.rem_u (i32.add (local.get $index) (local.get $step))
                                                                        (local.get $capacity))
                                                             (i32.const 1))) ;; multiply by 2
                                   ;; Load key from slot
                                   (local.set $k (array.get $Array (local.get $entries) (local.get $slot)))
                                   ;; Empty slot — insert
                                   (if (ref.eq (local.get $k) (global.get $missing))
                                       (then
                                        (array.set $Array (local.get $entries) (local.get $slot) (local.get $key))
                                        (array.set $Array (local.get $entries)
                                                   (i32.add (local.get $slot) (i32.const 1))
                                                   (local.get $val))
                                        (struct.set $SymbolTable $count
                                                    (local.get $table)
                                                    (i32.add (struct.get $SymbolTable $count (local.get $table)) (i32.const 1)))
                                        (br $done)))
                                   ;; Key match — update value
                                   (if (call $string=?/i32 (local.get $k) (local.get $key))
                                       (then (array.set $Array (local.get $entries)
                                                        (i32.add (local.get $slot) (i32.const 1))
                                                        (local.get $val))
                                             (br $done)))
                                   ;; Otherwise, try next probe
                                   (local.set $step (i32.add (local.get $step) (i32.const 1)))
                                   (br $probe)))
                      ;; Failed to insert — table full (should not happen if resizing is correct)
                      (call $raise-symbol-table-insert:table-full)))

         


         (func $symbol-table-resize
               (param $old (ref $SymbolTable))
               (result (ref $SymbolTable))

               (local $old-entries (ref $Array))
               (local $old-cap     i32)
               (local $new-cap     i32)
               (local $new-array   (ref $Array))
               (local $new-table   (ref $SymbolTable))
               (local $i           i32)
               (local $len         i32)
               (local $key         (ref eq))
               (local $val         (ref eq))

               ;; Get old table size and entries
               (local.set $old-entries (struct.get $SymbolTable $entries (local.get $old)))
               (local.set $old-cap     (i32.div_u (array.len (local.get $old-entries)) (i32.const 2)))
               (local.set $new-cap     (i32.mul (local.get $old-cap) (i32.const 2)))
               ;; Allocate new entries array with (2 * new-capacity) slots filled with $missing
               (local.set $new-array (array.new $Array (global.get $missing)
                                                (i32.mul (local.get $new-cap) (i32.const 2))))
               ;; Create new empty table with count = 0
               (local.set $new-table (struct.new $SymbolTable (local.get $new-array) (i32.const 0)))
               ;; Reinsert old entries
               (local.set $i (i32.const 0))
               (local.set $len (array.len (local.get $old-entries)))
               (block $done
                      (loop $loop
                            (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
                            ;; Get key and value
                            (local.set $key (array.get $Array (local.get $old-entries) (local.get $i)))
                            (local.set $val (array.get $Array (local.get $old-entries) (i32.add (local.get $i) (i32.const 1))))
                            ;; Insert if key is not $missing (i.e. dummy)
                            (if (ref.test (ref $String) (local.get $key))
                                (then (call $symbol-table-insert
                                            (local.get $new-table)
                                            (ref.cast (ref $String) (local.get $key))
                                            (local.get $val))))
                            ;; Step to next pair
                            (local.set $i (i32.add (local.get $i) (i32.const 2)))
                            (br $loop)))
               (local.get $new-table))

         (func $maybe-resize-symbol-table
               (param $table    (ref $SymbolTable))
               (result          (ref $SymbolTable))
               
               (local $entries  (ref $Array))
               (local $capacity i32)
               (local $count    i32)
               ;; Get fields
               (local.set $entries  (struct.get $SymbolTable $entries (local.get $table)))
               (local.set $capacity (i32.div_u (array.len (local.get $entries)) (i32.const 2)))
               (local.set $count    (struct.get $SymbolTable $count (local.get $table)))
               ;; Resize if count >= capacity / 2
               (if (i32.ge_u (local.get $count)
                             (i32.shr_u (local.get $capacity) (i32.const 1)))
                   (then (return (call $symbol-table-resize (local.get $table)))))
               ;; Otherwise return unchanged
               (local.get $table))


         ;  Fowler–Noll–Vo hash function
         (func $string-hash/i32
               (param $s (ref $String))
               (result i32)

               (local $hash i32)
               (local $arr  (ref $I32Array))
               (local $len  i32)
               (local $i    i32)
               (local $cp   i32)

               ;; Check if already memoized
               (local.set $hash (struct.get $String $hash (local.get $s)))
               (if (result i32)
                   (i32.ne (local.get $hash) (i32.const 0))
                   (then (return (local.get $hash))) ;; Already cached
                   ;; Compute FNV-1a hash
                   (else
                    (local.set $hash (i32.const 2166136261)) ; offset
                    (local.set $arr  (struct.get $String $codepoints (local.get $s)))
                    (local.set $len  (array.len (local.get $arr)))
                    (local.set $i    (i32.const 0))
                    (block $done
                           (loop $loop
                                 (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
                                 (local.set $cp (array.get $I32Array (local.get $arr) (local.get $i)))
                                 (local.set $hash
                                            (i32.mul
                                             (i32.xor (local.get $hash) (local.get $cp))
                                             (i32.const 16777619))) ; prime
                                 (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                 (br $loop)))
                    ;; Memoize
                    (struct.set $String $hash (local.get $s) (local.get $hash))
                    (local.get $hash))))


         ;;;
         ;;; SYMBOLS
         ;;;

         ;; (type $Symbol
         ;;       (sub $Heap
         ;;            (struct
         ;;              (field $hash          (mut i32))         ;; cached hash                 
         ;;              (field $name          (ref $String))     ;; symbol name (string)        
         ;;              (field $property-list (mut (ref eq))))))  ;; user-defined properties    


         (func $symbol? (param $x (ref eq)) (result (ref eq))
               (if (result (ref eq)) (ref.test (ref $Symbol) (local.get $x))
                   (then (global.get $true))
                   (else (global.get $false))))
         
         (func $symbol=?
               (param $a (ref eq)) (param $b (ref eq))
               (result (ref eq))
               (if (result (ref eq))
                   (ref.eq (local.get $a) (local.get $b))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $symbol=?/i32 (param $a (ref eq)) (param $b (ref eq)) (result i32)
               (ref.eq (local.get $a) (local.get $b)))


         (func $raise-symbol->string:bad-argument (param $v (ref eq)) (unreachable))
         
         (func $symbol->string
               (param $v (ref eq))
               (result   (ref eq))

               (local $sym (ref $Symbol))
               (local $name (ref $String))

               ;; Check that input is a symbol
               (if (ref.test (ref $Symbol) (local.get $v))
                   (then
                    ;; Cast to $Symbol
                    (local.set $sym (ref.cast (ref $Symbol) (local.get $v)))
                    ;; Extract name field
                    (local.set $name (struct.get $Symbol $name (local.get $sym)))
                    ;; Return a fresh mutable copy
                    (return (call $string-copy (local.get $name))))
                   (else
                    ;; Not a symbol, raise error
                    (call $raise-symbol->string:bad-argument (local.get $v))))
               (unreachable))
         
         (func $raise-string->symbol:bad-argument (param $v (ref eq)) (unreachable))

         (func $string->symbol
               (param $v (ref eq))
               (result   (ref $Symbol))
               
               (if (ref.test (ref $String) (local.get $v))
                   (then (return
                          (call $string->symbol/checked
                                (ref.cast (ref $String) (local.get $v)))))
                   (else (call $raise-string->symbol:bad-argument (local.get $v))))
               (unreachable))

         (func $string->symbol/checked
               (param $str (ref $String))
               (result     (ref $Symbol))

               (local $existing (ref eq))
               (local $sym      (ref $Symbol))
               ;; Look up the string in the symbol table
               (local.set $existing (call $symbol-table-find
                                          (ref.as_non_null (global.get $the-symbol-table))
                                          (local.get $str)))
               ;; If found, return it (cast to (ref $Symbol))
               (if (ref.test (ref $Symbol) (local.get $existing))
                   (then (return (ref.cast (ref $Symbol) (local.get $existing)))))
               ;; Otherwise, construct a new interned symbol
               (local.set $sym (struct.new $Symbol
                                           (i32.const 0)        ;; hash = 0 (not computed)
                                           (local.get $str)     ;; name
                                           (global.get $null))) ;; empty property list
               ;; Insert it into the symbol table
               (call $symbol-table-insert
                     (ref.as_non_null (global.get $the-symbol-table))
                     (local.get $str)
                     (local.get $sym))
               ;; Return the new symbol
               (local.get $sym))

         (func $raise-string->uninterned-symbol:bad-argument (param $v (ref eq)) (unreachable))

         (func $string->uninterned-symbol
               (param $v (ref eq))
               (result (ref $Symbol))

               (if (ref.test (ref $String) (local.get $v))
                   (then (return
                          (call $string->uninterned-symbol/checked
                                (ref.cast (ref $String) (local.get $v)))))
                   (else (call $raise-string->uninterned-symbol:bad-argument (local.get $v))))
               (unreachable))

         (func $string->uninterned-symbol/checked
               (param $str (ref $String))
               (result     (ref $Symbol))

               (struct.new $Symbol
                           (i32.const 0)            ;; hash = 0 (deferred)
                           (local.get $str)         ;; name
                           (global.get $null)))     ;; empty property list

         (func $symbol-interned?
               (param $sym (ref eq))
               (result     (ref eq))

               (local $str   (ref $String))
               (local $found (ref eq))

               ;; Check that it's a symbol
               (if (i32.eqz (ref.test (ref $Symbol) (local.get $sym)))
                   (then (call $raise-check-symbol (local.get $sym))))
               (local.set $str   (struct.get $Symbol $name (ref.cast (ref $Symbol)   (local.get $sym))))
               (local.set $found (call $symbol-table-find
                                       (ref.as_non_null (global.get $the-symbol-table)) (local.get $str)))
               ;; If found symbol == input symbol => interned
               (if (result (ref eq)) (ref.eq (local.get $found) (local.get $sym))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $raise-check-symbol (param $x (ref eq)) (unreachable))
         
         (func $symbol<? (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
               (local $s1 (ref $String))
               (local $s2 (ref $String))

               ;; Type check: both must be symbols
               (if (i32.eqz (ref.test (ref $Symbol) (local.get $a)))
                   (then (call $raise-check-symbol (local.get $a))))
               (if (i32.eqz (ref.test (ref $Symbol) (local.get $b)))
                   (then (call $raise-check-symbol (local.get $b))))
               ;; Extract names
               (local.set $s1 (struct.get $Symbol $name (ref.cast (ref $Symbol) (local.get $a))))
               (local.set $s2 (struct.get $Symbol $name (ref.cast (ref $Symbol) (local.get $b))))
               ;; Compare using string<?
               (call $string<? (local.get $s1) (local.get $s2)))

         (global $gensym-counter (mut i32) (i32.const 0))

         (func $make-gensym-name
               (param $prefix (ref $String))
               (result        (ref $String))

               (local $n     i32)
               (local $n-str (ref $String))
               ;; Get current counter
               (local.set $n (global.get $gensym-counter))
               ;; Increment counter
               (global.set $gensym-counter (i32.add (local.get $n) (i32.const 1)))
               ;; Convert number to fixnum
               (local.set $n-str
                          (call $number->string (ref.i31 (local.get $n)) ,(Imm 10)))
               ;; Append prefix and number string
               (ref.cast (ref $String)
                         (call $string-append (local.get $prefix) (local.get $n-str))))

         (func $gensym:0 (result (ref $Symbol))
               ;; Use "g" as default prefix
               (call $gensym:1 (call $str-g)))

         (func $raise-gensym:bad-base (param $x (ref eq)) (unreachable))
         
         (func $gensym:1 (param $base (ref eq)) (result (ref $Symbol))
               (local $prefix (ref null $String))
               (local $name   (ref $String))

               ;; Convert symbol -> string if needed
               (if (ref.test (ref $Symbol) (local.get $base))
                   (then
                    (local.set $prefix
                               (struct.get $Symbol $name
                                           (ref.cast (ref $Symbol) (local.get $base)))))
                   (else
                    (if (ref.test (ref $String) (local.get $base))
                        (then (local.set $prefix
                                         (ref.cast (ref $String) (local.get $base))))
                        (else (call $raise-gensym:bad-base (local.get $base))
                              (unreachable)))))
               ;; Generate name string
               (local.set $name (call $make-gensym-name (ref.as_non_null (local.get $prefix))))
               ;; Return new uninterned symbol
               (struct.new $Symbol
                           (i32.const 0)        ;; hash = 0
                           (local.get $name)    ;; name
                           (global.get $null))) ;; empty property list

         ;;;
         ;;; KEYWORDS
         ;;;

         ;; Keywords are interned using `the-keywords-table` which maps strings (without #:)
         ;; to keywords.

         (func $keyword?/i32
               (param $v (ref eq))
               (result i32)
               (ref.test (ref $Keyword) (local.get $v)))

         (func $keyword?
               (param $v (ref eq))
               (result (ref eq))
               (if (result (ref eq))
                   (ref.test (ref $Keyword) (local.get $v))
                   (then (global.get $true))
                   (else (global.get $false))))
         
         (func $string->keyword
               (param $str (ref eq))
               (result (ref $Keyword))
               ;; Type check: must be a string
               (if (i32.eqz (ref.test (ref $String) (local.get $str)))
                   (then (call $raise-argument-error:string-expected (local.get $str))
                         (unreachable)))
               ;; Cast and delegate
               (call $string->keyword/checked (ref.cast (ref $String) (local.get $str))))
                       
               
         (func $string->keyword/checked
               (param $str (ref $String))
               (result     (ref $Keyword))

               (local $existing (ref eq))
               (local $kw       (ref $Keyword))
               ;; Look up in table
               (local.set $existing (call $symbol-table-find 
                                          (ref.as_non_null (global.get $the-keyword-table))
                                          (local.get $str)))
               (if (result (ref $Keyword))
                   (ref.eq (local.get $existing) (global.get $missing))
                   (then
                    ;; Not found – allocate and intern new keyword
                    (local.set $kw (struct.new $Keyword
                                               (i32.const 0) ;; hash will be assigned later
                                               (local.get $str)))
                    (call $symbol-table-insert
                          (ref.as_non_null (global.get $the-keyword-table))
                          (local.get $str)
                          (local.get $kw))
                    (local.get $kw))
                   (else
                    (ref.cast (ref $Keyword)
                              (local.get $existing)))))

         (func $raise-argument-error:keyword-expected (unreachable))
         
         (func $keyword->string
               (param $kw (ref eq))
               (result    (ref $String))
               ;; Type check: must be a keyword
               (if (i32.eqz (ref.test (ref $Keyword) (local.get $kw)))
                   (then (call $raise-argument-error:keyword-expected (local.get $kw))
                         (unreachable)))
               ;; Cast and delegate
               (call $keyword->string/checked (ref.cast (ref $Keyword) (local.get $kw))))
         
         (func $keyword->string/checked
               (param $kw     (ref $Keyword))
               (result        (ref $String))
               
               (local $name   (ref $String))
               (local $prefix (ref $String))

               (local.set $name   (struct.get $Keyword $str (local.get $kw)))
               
               (local.set $prefix (call $str-hash-colon)) ;; "#:"
               (call $string-append
                     (local.get $prefix) (local.get $name)))

         
         
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
    
         (func $make-initial-location (result (ref eq))
               ;; (location 1 0 1)
               (struct.new $Location
                           (i32.const 0)            ;; hash (placeholder)
                           (global.get $one)        ;; line
                           (global.get $zero)       ;; col
                           (global.get $one)))      ;; pos
         
         ;;; STRING PORT
         ;; Note: the index `idx` and the location position may be different,
         ;;       since the #\return#\newline combination counts as a
         ;;       single position.

        (func $raise-check-string-port (param $x (ref eq)) (unreachable))
        (func $raise-check-port-or-false (param $x (ref eq)) (unreachable))
         
         (func $string-port?
               (param $v (ref eq))
               (result (ref eq))
               (if (result (ref eq))
                   (ref.test (ref $StringPort) (local.get $v))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $port-next-location
               (param $p (ref eq))
               (result   (ref eq))

               (local $port (ref null $StringPort))
               (local $loc  (ref null $Location))
               ;; 1. Check and cast $p to (ref $StringPort)
               (if (ref.test (ref $StringPort) (local.get $p))
                   (then (local.set $port (ref.cast (ref $StringPort) (local.get $p))))
                   (else (return (global.get $false))))
               ;; 2. Get location struct from the port
               (local.set $loc (struct.get $StringPort $loc (local.get $port)))
               ;; 3. Extract and return as fixed array of 3 elements
               (array.new_fixed $Values 3
                                (struct.get $Location $line (local.get $loc))
                                (struct.get $Location $col  (local.get $loc))
                                (struct.get $Location $pos  (local.get $loc))))

         (func $open-output-bytes
               (result (ref eq))

               (local $bs  (ref $Bytes))
               (local $loc (ref $Location))
               ;; Step 1: Allocate the backing I8Array with capacity 32 and fill with 0
               (local.set $bs
                          (struct.new $Bytes
                                      (i32.const 0)                          ;; hash = 0
                                      (i32.const 0)                          ;; mutable = false
                                      (call $i8make-array (i32.const 32) (i32.const 0)))) ;; backing array
               ;; Step 2: Make initial location: (line 1, col 0, pos 1)
               (local.set $loc (ref.cast (ref $Location) (call $make-initial-location)))
               ;; Step 3: Construct and return the StringPort
               (struct.new $StringPort
                           (i32.const 0)                 ;; $hash
                           (local.get $bs)               ;; $bytes : (ref $Bytes)
                           (global.get $false)           ;; $name  : (ref eq)
                           (i32.const 32)                ;; $len   : i32
                           (i32.const 0)                 ;; $idx   : i32
                           (local.get $loc)              ;; $loc   : (ref $Location)
                           (i32.const 0)                 ;; $utf8-len
                           (i32.const 0)                 ;; $utf8-left
                           (i32.const 0)))               ;; $utf8-bytes

         (func $get-output-bytes
               (param $out (ref eq))
               (result (ref eq))
               
               (local $sp   (ref null $StringPort))
               (local $bs   (ref $Bytes))
               (local $idx  i32)
               (local $src  (ref $I8Array))
               (local $dest (ref $I8Array))
               (local $res  (ref $Bytes))
               ;; 1. Check that $out is a StringPort
               (if (ref.test (ref $StringPort) (local.get $out))
                   (then (local.set $sp (ref.cast (ref $StringPort) (local.get $out))))
                   (else (call $raise-check-string-port (local.get $out)) (unreachable)))
               ;; 2. Get internal byte string and index
               (local.set $bs  (struct.get $StringPort $bytes (local.get $sp)))
               (local.set $idx (struct.get $StringPort $idx   (local.get $sp)))
               ;; 3. Extract the I8Array from the Bytes object
               (local.set $src (struct.get $Bytes $bs (local.get $bs)))
               ;; 4. Allocate a new array of length $idx
               (local.set $dest (call $i8make-array (local.get $idx) (i32.const 0)))
               ;; 5. Copy contents into the new array
               (drop (call $i8array-copy!/error
                           (local.get $dest)            ;; dest
                           (i32.const 0)                ;; dest-start
                           (local.get $src)             ;; src
                           (i32.const 0)                ;; src-start
                           (local.get $idx)))           ;; count
               ;; 6. Construct new Bytes object
               (local.set $res
                          (struct.new $Bytes
                                      (i32.const 0)              ;; hash
                                      (i32.const 1)              ;; immutable
                                      (local.get $dest)))        ;; backing array
               ;; 7. Return the new Bytes
               (local.get $res))


         ;; (type $StringPort
         ;;       (struct
         ;;         (field $bytes (ref eq))     ; the byte string (bytes)
         ;;         (field $name  (ref eq))     ; the port name   (string)
         ;;         (field $len   i32)          ; the length of the string
         ;;         (field $idx   i32)          ; the current index into the string
         ;;         (field $loc   (ref $Location)))) ; the current location
         
         (func $write-byte
               (param $byte (ref eq))
               (param $out  (ref eq))
               (result      (ref eq))
               
               (local $b         i32)
               (local $sp        (ref null $StringPort))
               (local $bs        (ref eq))
               (local $idx       i32)
               (local $loc       (ref $Location))

               (local $pos       (ref eq))
               (local $line      (ref eq))
               (local $col       (ref eq))

               (local $int-pos   i32)
               (local $int-line  i32)
               (local $int-col   i32)

               (local $old-len   i32)
               (local $new-len   i32)
               (local $new-bytes (ref $I8Array))
               (local $len       i32)
               (local $left      i32)
               (local $seen      i32)
               ;; 1. Cast byte to i31 and extract i32
               (if (ref.test (ref i31) (local.get $byte))
                   (then (local.set $b (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $byte)))
                                                  (i32.const 1))))
                   (else (return (global.get $false))))
               ;; 2. Cast output to $StringPort
               (if (ref.test (ref $StringPort) (local.get $out))
                   (then (local.set $sp (ref.cast (ref $StringPort) (local.get $out))))
                   (else (return (global.get $false))))
               ;; 3. Get buffer and current index
               (local.set $bs  (struct.get $StringPort $bytes (local.get $sp)))
               (local.set $idx (struct.get $StringPort $idx   (local.get $sp)))
               ;; 4. Resize buffer if needed
               (local.set $old-len (struct.get $StringPort $len (local.get $sp)))
               (if (i32.eq (local.get $idx) (local.get $old-len))
                   (then (local.set $new-len (i32.shl (local.get $old-len) (i32.const 1)))
                         (local.set $new-bytes (call $i8array-extend
                                                     (struct.get $Bytes $bs (ref.cast (ref $Bytes) (local.get $bs)))
                                                     (local.get $new-len)
                                                     (i32.const 0)))
                         (struct.set $StringPort $bytes (local.get $sp)
                                     (struct.new $Bytes
                                                 (i32.const 0)
                                                 (i32.const 0)
                                                 (local.get $new-bytes)))
                         (struct.set $StringPort $len (local.get $sp) (local.get $new-len))
                         (local.set $bs (struct.get $StringPort $bytes (local.get $sp)))))
               ;; 5. Write byte into array
               (call $i8array-set!
                     (struct.get $Bytes $bs (ref.cast (ref $Bytes) (local.get $bs)))
                     (local.get $idx)
                     (local.get $b))
               (struct.set $StringPort $idx (local.get $sp)
                           (i32.add (local.get $idx) (i32.const 1)))
               ;; 6. Load old location fields
               (local.set $loc  (struct.get $StringPort $loc  (local.get $sp)))
               (local.set $pos  (struct.get $Location   $pos  (local.get $loc)))
               (local.set $line (struct.get $Location   $line (local.get $loc)))
               (local.set $col  (struct.get $Location   $col  (local.get $loc)))

               (local.set $int-pos  (if (result i32)
                                        (ref.test (ref i31) (local.get $pos))
                                        (then ,(Half `(i31.get_u (ref.cast (ref i31) (local.get $pos)))))
                                        (else (i32.const 0))))
               (local.set $int-line (if (result i32)
                                        (ref.test (ref i31) (local.get $line))
                                        (then ,(Half `(i31.get_u (ref.cast (ref i31) (local.get $line)))))
                                        (else (i32.const 0))))
               (local.set $int-col  (if (result i32)
                                        (ref.test (ref i31) (local.get $col))
                                        (then ,(Half `(i31.get_u (ref.cast (ref i31) (local.get $col)))))
                                        (else (i32.const 0))))
               ;; 7. Decode UTF-8 byte
               (local.set $len  (struct.get $StringPort $utf8-len   (local.get $sp)))
               (local.set $left (struct.get $StringPort $utf8-left  (local.get $sp)))
               (local.set $seen (struct.get $StringPort $utf8-bytes (local.get $sp)))
               ;; Start of UTF-8 sequence?
               (if (i32.eqz (local.get $len))
                   (then
                    ;; Determine length from lead byte
                    (if (i32.lt_u (local.get $b) (i32.const 128))  ;; ASCII
                        (then
                         (local.set $int-col (i32.add (local.get $int-col) (i32.const 1))))
                        (else
                         (if (i32.and (i32.ge_u (local.get $b) (i32.const 192))
                                      (i32.le_u (local.get $b) (i32.const 253)))
                             (then
                              (block
                              ;; Count leading 1s to determine length
                              (local.set $len
                                         (i32.clz (i32.xor (i32.shl (local.get $b) (i32.const 24))
                                                           (i32.const 0xFF000000))))
                              ;; Limit to 4
                              (local.set $len    ; minimum of $length and 5
                                         (select
                                          (local.get $len)
                                          (i32.const 4)
                                          (i32.lt_u (local.get $len) (i32.const 4))))

                              (struct.set $StringPort $utf8-len   (local.get $sp) (local.get $len))
                              (struct.set $StringPort $utf8-left  (local.get $sp)
                                          (i32.sub (local.get $len) (i32.const 1)))
                              (struct.set $StringPort $utf8-bytes (local.get $sp) (i32.const 1)))))))
                    ;; Inside a sequence
                    (else
                     (block
                      (local.set $seen (i32.add (local.get $seen) (i32.const 1)))
                      (local.set $left (i32.sub (local.get $left) (i32.const 1)))
                      (struct.set $StringPort $utf8-left  (local.get $sp) (local.get $left))
                      (struct.set $StringPort $utf8-bytes (local.get $sp) (local.get $seen))
                      (if (i32.eqz (local.get $left))
                          (then
                           ;; Sequence complete — count as 1 column
                           (local.set $int-col
                                      (i32.sub (local.get $int-col)
                                               (i32.sub (local.get $seen) (i32.const 1))))
                           (struct.set $StringPort $utf8-len   (local.get $sp) (i32.const 0))
                           (struct.set $StringPort $utf8-left  (local.get $sp) (i32.const 0))
                           (struct.set $StringPort $utf8-bytes (local.get $sp) (i32.const 0))))))))
               ;; 8. Handle line/column updates
               (if (i32.eq (local.get $b) (i32.const 10)) ;; '\n'
                   (then (local.set $int-line (i32.add (local.get $int-line) (i32.const 1)))
                         (local.set $int-col  (i32.const 0))))
               (if (i32.eq (local.get $b) (i32.const 13)) ;; '\r'
                   (then (local.set $int-line (i32.add (local.get $int-line) (i32.const 1)))
                         (local.set $int-col  (i32.const 0))))
               (if (i32.eq (local.get $b) (i32.const 9)) ;; '\t'
                   (then (local.set $int-col
                               (i32.add (local.get $int-col)
                                        (i32.sub
                                         (i32.const 8)
                                         (i32.rem_u (local.get $int-col) (i32.const 8)))))))
               ;; Always increment position
               (local.set $int-pos (i32.add (local.get $int-pos) (i32.const 1)))
               ;; 9. Store new location
               (struct.set $StringPort $loc (local.get $sp)
                           (struct.new $Location
                                       (i32.const 0)  ;; hash
                                       (ref.i31 (i32.shl (local.get $int-line) (i32.const 1)))
                                       (ref.i31 (i32.shl (local.get $int-col)  (i32.const 1)))
                                       (ref.i31 (i32.shl (local.get $int-pos)  (i32.const 1)))))
               ;; 10. Return void
               (global.get $void))
         (func $copy_bytes_to_memory
               (export "copy_bytes_to_memory")
               (param $ptr i32)   ;; destination address in linear memory
               (result i32)       ;; number of bytes copied

               (local $i   i32)
               (local $val i32)
               (local $len i32)
               (local $bs  (ref $Bytes))
               (local $arr (ref $I8Array))
               ;; 1. Cast global to (ref $Bytes)
               (local.set $bs (ref.cast (ref $Bytes) (global.get $result-bytes)))
               ;; 2. Get backing array
               (local.set $arr (struct.get $Bytes $bs (local.get $bs)))
               ;; 3. Get length of array
               (local.set $len (array.len (local.get $arr)))
               ;; 4. Loop to copy each byte
               (local.set $i (i32.const 0))
               (block $done
                      (loop $copy
                            ;; if i >= len, break
                            (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
                            ;; val = arr[i]
                            (local.set $val (array.get_u $I8Array (local.get $arr) (local.get $i)))
                            ;; memory[ptr + i] = val
                            (i32.store8 (i32.add (local.get $ptr) (local.get $i))
                                        (local.get $val))
                            ;; i++
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            ;; loop again
                            (br $copy)))
               ;; 5. Return total bytes copied
               (local.get $len))

         ;;;
         ;;; STRUCTURES
         ;;;

         ;; [x] structure types and super structs
         ;; [x] auto fields
         ;; [ ] prefab structures
         ;; [ ] structure guards
         ;; [ ] applicable structures
         
         (func $struct?/i32 (param $v (ref eq)) (result i32)
               (ref.test (ref $Struct) (local.get $v)))

         (func $struct? (param $v (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.test (ref $Struct) (local.get $v))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $struct-type?/i32 (param $v (ref eq)) (result i32)
               (ref.test (ref $StructType) (local.get $v)))

         (func $struct-type? (param $v (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.test (ref $StructType) (local.get $v))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $raise-check-struct-type (unreachable))
         
         (func $check-struct-type
               (param $name (ref $String))
               (param $v    (ref eq))
               (result      (ref eq))

               (if (result (ref eq))
                   (ref.eq (local.get $v) (global.get $false))
                   (then (local.get $v)) ;; allow #f
                   (else (if (result (ref eq))
                             (ref.test (ref $StructType) (local.get $v))
                             (then (local.get $v))
                             (else (call $raise-check-struct-type)
                                   #;(call $raise-argument-error
                                           (local.get $name)
                                           (call $str-struct-type-or-false)
                                           (local.get $v))
                                   (unreachable))))))

         (func $struct-type-is-a?/i32
                ; is $a a subtype of $b ?
               (param $a (ref eq)) (param $b (ref eq))
               (result i32)

               (local $cur (ref eq))
               (local.set $cur (local.get $a))

               (block $exit
                      (loop $walk
                            ;; if cur == b => success
                            (br_if $exit (ref.eq (local.get $cur) (local.get $b)))
                            ;; if cur == #f => fail
                            (br_if $exit (ref.eq (local.get $cur) (global.get $false)))
                            ;; if cur is not a struct type => fail
                            (br_if $exit (i32.eqz (ref.test (ref $StructType) (local.get $cur))))
                            ;; climb to supertype
                            (local.set $cur
                                       (struct.get $StructType $super
                                                   (ref.cast (ref $StructType) (local.get $cur))))
                            (br $walk))
                      ;; If we exited the loop early, we failed
                      (return (i32.const 0)))
               ;; If we fell through the loop, we succeeded
               (i32.const 1))


         (data $str-struct-type-descriptor-bytes "#<struct-type-descriptor>")
         (data $str-struct-open-bytes "#(struct ")

         (func $str-struct-type-descriptor (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-struct-type-descriptor-bytes
                                     (i32.const 0) (i32.const 27))))        
         (func $str-struct-open (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-struct-open-bytes
                                     (i32.const 0) (i32.const 9))))

         (func $raise-format/display:struct:expected-struct (unreachable))
         
         (func $format/display:struct
               (param $v (ref eq))
               (result (ref $String))

               (local $s (ref $Struct))
               (local $type (ref $StructType))
               (local $name (ref eq))
               (local $fields (ref $Array))
               (local $n i32)
               (local $i i32)
               (local $out (ref $GrowableArray))

               ;; Check struct type
               (if (i32.eqz (ref.test (ref $Struct) (local.get $v)))
                   (then (call $raise-format/display:struct:expected-struct)))

               ;; Cast and extract
               (local.set $s      (ref.cast (ref $Struct) (local.get $v)))
               (local.set $type   (struct.get $Struct     $type   (local.get $s)))
               (local.set $name   (struct.get $StructType $name   (local.get $type)))
               (local.set $fields (struct.get $Struct     $fields (local.get $s)))
               (local.set $n      (array.len (local.get $fields)))

               ;; Start output
               (local.set $out (call $make-growable-array (i32.const 8)))
               (call $growable-array-add! (local.get $out) (call $str-struct-open)) ;; "#(struct "

               ;; Add name
               (call $growable-array-add! (local.get $out)
                     (call $format/display (local.get $name)))

               ;; Add each field
               (local.set $i (i32.const 0))
               (block $done
                      (loop $loop
                            (br_if $done (i32.ge_u (local.get $i) (local.get $n)))
                            (call $growable-array-add! (local.get $out) (call $str-space))
                            (call $growable-array-add! (local.get $out)
                                  (call $format/display
                                        (array.get $Array (local.get $fields) (local.get $i))))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $loop)))

               ;; Close output
               (call $growable-array-add! (local.get $out) (call $str-close-paren))
               (call $growable-array-of-strings->string (local.get $out)))


         (func $make-struct-type-descriptor
               (param $name               (ref eq))  ;; (ref $Symbol)
               (param $super-type         (ref eq))  ;; (ref $StructType) or #f
               (param $init-field-count   (ref eq))  ;; fixnum
               (param $auto-field-count   (ref eq))  ;; fixnum
               ; optional:
               (param $auto-field-value   (ref eq))  ;; value to repeat
               (param $opt-props          (ref eq))  ;; or #f
               (param $inspector          (ref eq))  ;; or #f
               (param $proc-spec          (ref eq))  ;; or #f
               (param $immutables         (ref eq))  ;; or #f
               (param $opt-guard          (ref eq))  ;; or #f
               (param $constructor-name   (ref eq))  ;; or #f
               (result                    (ref $StructType))

               (local $ifc   i32)
               (local $afc   i32)
               (local $props (ref eq))
               (local $super (ref $StructType))

               ;; --- Argument checks ---
               (if (i32.eqz (ref.test (ref $Symbol) (local.get $name)))
                   (then (call $raise-argument-error (local.get $name))))
               (if (i32.or
                    (i32.eqz (ref.test (ref i31) (local.get $init-field-count)))
                    (i32.ne (i32.and (i31.get_u (ref.cast (ref i31) (local.get $init-field-count))) (i32.const 1)) (i32.const 0)))
                   (then (call $raise-argument-error (local.get $init-field-count))))
               (if (i32.or
                    (i32.eqz (ref.test (ref i31) (local.get $auto-field-count)))
                    (i32.ne (i32.and (i31.get_u (ref.cast (ref i31) (local.get $auto-field-count))) (i32.const 1)) (i32.const 0)))
                   (then (call $raise-argument-error (local.get $auto-field-count))))

               ;; --- Decode fixnums ---
               (local.set $ifc (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $init-field-count))) (i32.const 1)))
               (local.set $afc (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $auto-field-count))) (i32.const 1)))

               ;; --- Handle optional props ---
               (local.set $props (if (result (ref eq))
                                     (ref.eq (local.get $opt-props) (global.get $false))
                                     (then (array.new $Array (global.get $false) (i32.const 0)))
                                     (else (local.get $opt-props))))

               ;; --- Cast super if not #f ---
               #;(local.set $super (if (result (ref $StructType))
                                       (ref.eq (local.get $super-type) (global.get $false))
                                       (then (ref.null $StructType))
                                       (else (ref.cast (ref $StructType) (local.get $super-type)))))

               ;; --- Delegate to /checked ---
               (call $make-struct-type-descriptor/checked
                     (ref.cast (ref $Symbol) (local.get $name))
                     (local.get $super-type)
                     (local.get $ifc)
                     (local.get $afc)
                     (local.get $auto-field-value)
                     (local.get $props)
                     (local.get $inspector)
                     (local.get $proc-spec)
                     (local.get $immutables)
                     (local.get $opt-guard)
                     (local.get $constructor-name)))

         
         #;(func $make-struct-type-descriptor
               ;; Required
               (param $name               (ref eq))  ;; (ref $Symbol)
               (param $super-type         (ref eq))  ;; (ref $StructType) or #f
               (param $init-field-count   (ref eq))  ;; fixnum (even i31)
               (param $auto-field-count   (ref eq))  ;; fixnum (even i31)
               (param $auto-field-value   (ref eq))  ;; value to repeat for each auto field
               
               ;; Optional (pass #f if not used)
               (param $opt-props          (ref eq))  ;; hash table or #f
               (param $inspector          (ref eq))  ;; inspector object or #f
               (param $proc-spec          (ref eq))  ;; currently unused
               (param $immutables         (ref eq))  ;; immutables descriptor or #f
               (param $opt-guard          (ref eq))  ;; guard procedure or #f
               (param $constructor-name   (ref eq))  ;; (ref $Symbol) or #f

               (result (ref $StructType))

               (local $ifc i32)
               (local $afc i32)
               (local $props (ref eq))

               ;; --- Type checks ---
               (if (i32.eqz (ref.test (ref $Symbol) (local.get $name)))
                   (then (call $raise-argument-error (local.get $name))))

               (if (i32.and (i32.eqz (ref.eq (local.get $super-type) (global.get $false)))
                            (i32.eqz (ref.test (ref $StructType) (local.get $super-type))))
                   (then (call $raise-argument-error (local.get $super-type))))

               (if (i32.or (i32.eqz (ref.test (ref i31) (local.get $init-field-count)))
                           (i32.ne (i32.and (i31.get_u (ref.cast (ref i31)
                                                                 (local.get $init-field-count)))
                                            (i32.const 1))
                                   (i32.const 0)))
                   (then (call $raise-argument-error (local.get $init-field-count))))

               (if (i32.or
                    (i32.eqz (ref.test (ref i31) (local.get $auto-field-count)))
                    (i32.ne (i32.and (i31.get_u (ref.cast (ref i31)
                                                          (local.get $auto-field-count)))
                                     (i32.const 1))
                            (i32.const 0)))
                   (then (call $raise-argument-error (local.get $auto-field-count))))

               ;; --- Decode fixnums ---
               (local.set $ifc (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $init-field-count))) (i32.const 1)))
               (local.set $afc (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $auto-field-count))) (i32.const 1)))

               ;; --- Properties ---
               (local.set $props
                          (if (result (ref eq))
                              (ref.eq (local.get $opt-props) (global.get $false))
                              (then (array.new $Array (global.get $false) (i32.const 0)))
                              (else (local.get $opt-props))))

               ;; --- Delegate to checked version ---
               (call $make-struct-type-descriptor/checked
                     (ref.cast (ref $Symbol) (local.get $name))
                     (local.get $super-type)
                     (local.get $ifc)
                     (local.get $afc)
                     (local.get $auto-field-value)
                     (local.get $props)
                     (local.get $inspector)
                     (local.get $proc-spec)
                     (local.get $immutables)
                     (local.get $opt-guard)
                     (local.get $constructor-name)))

         (func $make-struct-type-descriptor/checked
               (param $name             (ref $Symbol))    ;; Struct name
               (param $super            (ref eq))         ;; (ref $StructType) or #f
               (param $ifc              i32)              ;; Init field count (decoded)
               (param $afc              i32)              ;; Auto field count (decoded)
               (param $auto-value       (ref eq))         ;; Value to fill auto fields
               (param $props            (ref eq))         ;; Property hash or #f
               (param $inspector        (ref eq))         ;; Inspector or #f
               (param $proc-spec        (ref eq))         ;; Unused
               (param $immutables       (ref eq))         ;; Immutables or #f
               (param $guard            (ref eq))         ;; Guard or #f
               (param $constructor-name (ref eq))         ;; Symbol or #f
               (result (ref $StructType))

               (local $has-super    i32)
               (local $super-typed  (ref null $StructType))
               (local $stfc         i32)
               (local $init-indices (ref eq))
               (local $auto-indices (ref eq))
               (local $auto-values  (ref eq))
               (local $total-fields i32)

               ;; Default all list fields
               (local.set $init-indices (global.get $false))
               (local.set $auto-indices (global.get $false))
               (local.set $auto-values  (global.get $false))

               ;; Determine presence of supertype
               (local.set $has-super (i32.eqz (ref.eq (local.get $super) (global.get $false))))

               (if (local.get $has-super)
                   (then
                    (local.set $super-typed (ref.cast (ref $StructType) (local.get $super)))
                    (local.set $stfc (struct.get $StructType $field-count (local.get $super-typed)))

                    (local.set $init-indices
                               (call $append
                                     (struct.get $StructType $init-indices (local.get $super-typed))
                                     (call $list-from-range/checked
                                           (local.get $stfc)
                                           (i32.add (local.get $stfc) (local.get $ifc)))))                    
                    (local.set $auto-indices
                               (call $append
                                     (struct.get $StructType $auto-indices (local.get $super-typed))
                                     (call $list-from-range/checked
                                           (i32.add (local.get $stfc) (local.get $ifc))
                                           (i32.add (local.get $stfc) (i32.add (local.get $ifc) (local.get $afc))))))

                    (local.set $auto-values
                               (call $append
                                     (struct.get $StructType $auto-values (local.get $super-typed))
                                     (call $make-list/checked (local.get $afc) (local.get $auto-value)))))
                   (else
                    (local.set $stfc (i32.const 0))
                    (local.set $init-indices
                               (call $list-from-range/checked (i32.const 0) (local.get $ifc)))
                    (local.set $auto-indices
                               (call $list-from-range/checked
                                     (local.get $ifc)
                                     (i32.add (local.get $ifc) (local.get $afc))))
                    (local.set $auto-values
                               (call $make-list/checked (local.get $afc) (local.get $auto-value)))))

               ;; Compute total field count
               (local.set $total-fields
                          (i32.add (local.get $stfc)
                                   (i32.add (local.get $ifc) (local.get $afc))))

               ;; Create struct type descriptor
               (struct.new $StructType
                           (i32.const 0)               ;; hash (lazily computed)
                           (local.get $name)
                           (local.get $super)
                           (local.get $total-fields)
                           (local.get $init-indices)
                           (local.get $auto-indices)
                           (local.get $auto-values)
                           (local.get $props)
                           (local.get $inspector)
                           (local.get $immutables)
                           (local.get $guard)
                           (local.get $constructor-name)))

         #;(func $make-struct-type-descriptor/checked
               ;; Required
               (param $name             (ref $Symbol))  ;; Symbol naming the struct type
               (param $super            (ref eq))       ;; (ref $StructType) or #f
               (param $ifc              i32)            ;; Number of init fields (unwrapped fixnum)
               (param $afc              i32)            ;; Number of auto fields (unwrapped fixnum)
               (param $auto-value       (ref eq))       ;; Value to fill for each auto field

               ;; Optional (already defaulted appropriately)
               (param $props            (ref eq))       ;; Property table (hash table) or #f
               (param $inspector        (ref eq))       ;; Inspector object or #f
               (param $proc-spec        (ref eq))       ;; Currently unused
               (param $immutables       (ref eq))       ;; Immutables descriptor or #f
               (param $guard            (ref eq))       ;; Guard procedure or #f
               (param $constructor-name (ref eq))       ;; Name symbol or #f

               (result (ref $StructType))
               
               (local $has-super    i32)
               (local $super-typed  (ref null $StructType))
               (local $stfc         i32)
               (local $init-indices (ref eq))
               (local $auto-indices (ref eq))
               (local $auto-values  (ref eq))
               (local $total-fields i32)

               ;; Initialize locals
               (local.set $init-indices (global.get $false))
               (local.set $auto-indices (global.get $false))
               (local.set $auto-values  (global.get $false))
               
               ;; Determine if super is present
               (local.set $has-super (i32.eqz (ref.eq (local.get $super) (global.get $false))))

               (if (local.get $has-super)
                   (then
                    (local.set $super-typed (ref.cast (ref $StructType) (local.get $super)))
                    (local.set $stfc (struct.get $StructType $field-count (local.get $super-typed)))
                    (local.set $init-indices
                               (call $append
                                     (struct.get $StructType $init-indices (local.get $super-typed))
                                     (call $list-from-range/checked (local.get $stfc) (local.get $ifc))))
                    (local.set $auto-indices
                               (call $append
                                     (struct.get $StructType $auto-indices (local.get $super-typed))
                                     (call $list-from-range/checked
                                           (i32.add (local.get $stfc) (local.get $ifc))
                                           (local.get $afc))))
                    (local.set $auto-values
                               (call $append
                                     (struct.get $StructType $auto-values (local.get $super-typed))
                                     (call $make-list/checked (local.get $afc) (local.get $auto-value)))))
                   (else
                    (local.set $stfc (i32.const 0))
                    (local.set $init-indices (call $list-from-range/checked (i32.const 0) (local.get $ifc)))
                    (local.set $auto-indices (call $list-from-range/checked (local.get $ifc) (local.get $afc)))
                    (local.set $auto-values  (call $make-list/checked (local.get $afc) (local.get $auto-value)))))

               (local.set $total-fields (i32.add          (local.get $stfc)
                                                 (i32.add (local.get $ifc)
                                                          (local.get $afc))))

               (struct.new $StructType
                           (i32.const 0)                  ;; $hash
                           (local.get $name)
                           (local.get $super)
                           (local.get $total-fields)
                           (local.get $init-indices)
                           (local.get $auto-indices)
                           (local.get $auto-values)
                           (local.get $props)
                           (local.get $inspector)
                           (local.get $immutables)
                           (local.get $guard)
                           (local.get $constructor-name)))

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
         
         (func $make-struct-type
               ;; Parameters
               (param $name             (ref eq))  ;; (ref $Symbol)
               (param $super            (ref eq))  ;; (ref $StructType) or #f
               (param $init-count       (ref eq))  ;; fixnum
               (param $auto-count       (ref eq))  ;; fixnum
               ; optional:
               (param $auto-val         (ref eq))  ;; default value for auto fields
               (param $props            (ref eq))  ;; hash table or #f
               (param $inspector        (ref eq))  ;; inspector object or #f
               (param $proc-spec        (ref eq))  ;; unused
               (param $immutables       (ref eq))  ;; immutable mask or #f
               (param $guard            (ref eq))  ;; closure or #f
               (param $constructor-name (ref eq))  ;; symbol or #f
               
               (result (ref eq))  ;; returns 5-values packed as a pair
               ;                  ;; values: (struct-type, constructor, predicate, accessor, mutator)

               ;; Locals
               (local $init           i32)
               (local $auto           i32)
               (local $super-count    i32)
               (local $field-count    i32)
               (local $std            (ref $StructType))
               (local $ctor           (ref eq))
               (local $pred           (ref eq))
               (local $acc            (ref eq))
               (local $mut            (ref eq))
               (local $super-count-fx (ref eq))

               ;; --- Type checks ---
               (if (i32.eqz (ref.test (ref $Symbol) (local.get $name)))
                   (then (call $raise-argument-error (local.get $name))))

               (if (i32.and
                    (i32.eqz (ref.eq (local.get $super) (global.get $false)))
                    (i32.eqz (ref.test (ref $StructType) (local.get $super))))
                   (then (call $raise-argument-error (local.get $super))))

               (if (i32.or
                    (i32.eqz (ref.test (ref i31) (local.get $init-count)))
                    (i32.ne (i32.and (i31.get_u (ref.cast (ref i31) (local.get $init-count))) (i32.const 1)) (i32.const 0)))
                   (then (call $raise-argument-error (local.get $init-count))))

               (if (i32.or
                    (i32.eqz (ref.test (ref i31) (local.get $auto-count)))
                    (i32.ne (i32.and (i31.get_u (ref.cast (ref i31) (local.get $auto-count))) (i32.const 1)) (i32.const 0)))
                   (then (call $raise-argument-error (local.get $auto-count))))

               ;; --- Decode fixnums ---
               (local.set $init (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $init-count))) (i32.const 1)))
               (local.set $auto (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $auto-count))) (i32.const 1)))

               ;; --- Super field count ---
               (local.set $super-count
                          (if (result i32)
                              (i32.eqz (ref.eq (local.get $super) (global.get $false)))
                              (then (struct.get $StructType $field-count (ref.cast (ref $StructType) (local.get $super))))
                              (else (i32.const 0))))
               (local.set $super-count-fx (ref.i31 (i32.shl (local.get $super-count) (i32.const 1))))

               (local.set $field-count (i32.add (local.get $super-count)
                                                (i32.add (local.get $init) (local.get $auto))))

               ;; --- Create struct type descriptor ---
               (local.set $std
                          (call $make-struct-type-descriptor/checked
                                (ref.cast (ref $Symbol) (local.get $name))
                                (local.get $super)
                                (local.get $init)
                                (local.get $auto)
                                (local.get $auto-val)
                                (local.get $props)
                                (local.get $inspector)
                                (local.get $proc-spec)
                                (local.get $immutables)
                                (local.get $guard)
                                (local.get $constructor-name)))

               ;; --- Create constructor ---
               (local.set $ctor (call $make-struct-constructor/checked (local.get $std)))
               (local.set $pred (call $make-struct-predicate/checked   (local.get $std)))
               (local.set $acc  (call $make-struct-accessor/checked    (local.get $std) (local.get $super-count-fx)))
               (local.set $mut  (call $make-struct-mutator/checked     (local.get $std) (local.get $super-count-fx)))

               ;; --- Return values as a compound value ---
               (array.new_fixed $Values 5
                     (local.get $std)    ; struct type descriptor
                     (local.get $ctor)   ; constructor procedure
                     (local.get $pred)   ; predicate procedure
                     (local.get $acc)    ; accessor  procedure
                     (local.get $mut)))  ; mutator   procedure


         (func $make-struct-accessor
               (param $std (ref eq))
               (param $field-index (ref eq))   ;; fixnum
               (param $super-count (ref eq))   ;; fixnum
               (result (ref eq))               ;; $StructAccessorProcedure

               (local $i i32)
               ; (local $s i32)

               ;; Type checks
               (if (i32.eqz (ref.test (ref $StructType) (local.get $std)))
                   (then (call $raise-argument-error (local.get $std))))
               (if (i32.eqz (ref.test (ref i31) (local.get $field-index)))
                   (then (call $raise-argument-error (local.get $field-index))))
               (if (i32.eqz (ref.test (ref i31) (local.get $super-count)))
                   (then (call $raise-argument-error (local.get $super-count))))
               ;; Decode
               (local.set $i (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $field-index))) (i32.const 1)))
               ; (local.set $s (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $super-count))) (i32.const 1)))
               ;; Delegate
               (call $make-struct-accessor/checked
                     (ref.cast (ref $StructType) (local.get $std))
                     (local.get $super-count)))
         
         (func $make-struct-accessor/checked
               (param $std         (ref $StructType))
               (param $super-count (ref eq))
               (result             (ref eq)) ;; StructAccessorProcedure

               (local $free (ref $Free))

               ;; Pack just std and super-count; index comes at runtime
               (local.set $free
                          (array.new_fixed $Free 2
                                           (local.get $std)
                                           (local.get $super-count)))

               (struct.new $StructAccessorProcedure
                           (i32.const 0)               ; hash
                           (global.get $false)         ; name:  #f or $String
                           (global.get $zero)          ; arity: todo
                           (global.get $false)         ; realm: #f or $Symbol
                           (ref.func $invoke-closure) ; invoke (used by apply, map, etc.)
                           (ref.func $struct-accessor)
                           (local.get $free)))


         (func $raise-argument-error1 (param $x (ref eq)) (unreachable))
         (func $raise-argument-error2 (param $x (ref eq)) (unreachable))
         (func $raise-argument-error3 (param $x (ref eq)) (unreachable))
         (func $raise-argument-error4 (param $x (ref eq)) (unreachable))
         (func $raise-argument-error5 (param $x (ref eq)) (unreachable))         
         
         (func $struct-accessor
               (type $ClosureCode)
               (param $clos (ref $Closure))
               (param $args (ref $Args))
               (result      (ref eq))
               
               (local $free           (ref $Free))
               (local $std            (ref $StructType))  ; free[0]
               (local $super-count-fx (ref eq))           ; free[1]
               (local $super-count    i32)
               (local $target         (ref eq))           ; args[0]
               (local $struct         (ref $Struct))                     
               (local $index-fx       (ref eq))           ; args[1]
               (local $index          i32)               
               (local $skip           i32)
               (local $fields         (ref $Array))
               
               ;; Unpack free vars
               (local.set $free            (struct.get $Closure $free (local.get $clos)))
               (local.set $std             (ref.cast (ref $StructType)
                                                     (array.get $Free (local.get $free) (i32.const 0))))               

               (local.set $super-count-fx  (array.get $Free (local.get $free) (i32.const 1)))
               (local.set $super-count     (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $super-count-fx)))
                                                      (i32.const 1)))               
               (local.set $skip            (local.get $super-count))
               
               ;; Get struct
               (local.set $target (array.get $Args (local.get $args) (i32.const 0)))
               (if (i32.eqz (ref.test (ref $Struct) (local.get $target)))
                   (then (call $raise-argument-error1 (local.get $target))))
               (local.set $struct (ref.cast (ref $Struct) (local.get $target)))

               ;; Get index and decode
               (local.set $index-fx (array.get $Args (local.get $args) (i32.const 1)))
               (if (i32.or (i32.eqz (ref.test (ref i31) (local.get $index-fx)))
                           (i32.ne (i32.and (i31.get_u (ref.cast (ref i31) (local.get $index-fx))) (i32.const 1)) (i32.const 0)))
                   (then (call $raise-argument-error2 (local.get $index-fx))))
               (local.set $index (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $index-fx))) (i32.const 1)))
               
               ;; Type match - is $struct a subtype of $std
               (if (i32.eqz (call $struct-type-is-a?/i32
                                  (struct.get $Struct $type (local.get $struct))
                                  (local.get $std)))
                   (then (call $raise-argument-error3 (local.get $target))))

               ;; Get and return field
               (local.set $fields (struct.get $Struct $fields (local.get $struct)))
               (array.get $Array (local.get $fields)
                          (i32.add (local.get $skip) (local.get $index))))
         
         (func $make-struct-constructor
               ;; Parameters
               (param $v (ref eq)) ;; Expected: (ref $StructType)
               (result   (ref eq)) ;; Returns: closure

               (local $std (ref $StructType))

               ;; Type check
               (if (i32.eqz (ref.test (ref $StructType) (local.get $v)))
                   (then (call $raise-argument-error (local.get $v))))
               ;; Decode
               (local.set $std (ref.cast (ref $StructType) (local.get $v)))
               ;; Delegate
               (call $make-struct-constructor/checked (local.get $std)))

         (elem declare funcref
               (ref.func $struct-constructor/no-guard)       ; closure body
               (ref.func $struct-accessor)                   ; closure body
               (ref.func $struct-mutator)                    ; closure body
               (ref.func $struct-predicate)                  ; closure body
               (ref.func $struct-field-accessor/specialized) ; closure body
               (ref.func $struct-mutator/specialized)
               (ref.func $invoke-struct)
               (ref.func $invoke-primitive)
               #;(ref.func $struct-constructor/with-guard))

         
         (func $struct-constructor/no-guard
               (type $ClosureCode)
               (param $clos (ref $Closure))
               (param $args (ref $Args))
               (result      (ref eq))

               (local $free         (ref $Free))
               (local $std          (ref $StructType))
               (local $init-indices (ref eq))
               (local $auto-indices (ref eq))
               (local $auto-values  (ref eq))
               (local $field-count  i32)
               (local $arr          (ref $Array))

               (local.set $free         (struct.get $Closure $free (local.get $clos)))
               (local.set $std          (ref.cast (ref $StructType) (array.get $Free (local.get $free) (i32.const 0))))
               (local.set $init-indices (array.get $Free (local.get $free) (i32.const 1)))
               (local.set $auto-indices (array.get $Free (local.get $free) (i32.const 2)))
               (local.set $auto-values  (array.get $Free (local.get $free) (i32.const 3)))
               (local.set $field-count  (struct.get $StructType $field-count (local.get $std)))

               (local.set $arr (array.new $Array (global.get $false) (local.get $field-count)))
               (call $fill-fields-from-args   (local.get $arr) (local.get $init-indices) (local.get $args))
               (call $fill-fields-from-values (local.get $arr) (local.get $auto-indices) (local.get $auto-values))

               (struct.new $Struct
                           ; $Heap
                           (i32.const 0)             ;; hash
                           ; $Procedure
                           ,(Imm #f)                 ;; $false or a $String
                           ,(Imm 0)                  ;; fixnum (i31 with lsb=0) or (arity-at-least n)
                           ,(Imm #f)                 ;; $false or $Symbol
                           (ref.func $invoke-struct)
                           ; $Struct
                           (local.get $std)
                           (local.get $arr)))

         (func $make-struct-constructor/checked
               (param $std (ref $StructType))
               (result (ref eq)) ;; closure

               (local $field-count  i32)
               (local $guard        (ref eq))
               (local $name         (ref eq))
               (local $init-indices (ref eq))
               (local $auto-indices (ref eq))
               (local $auto-values  (ref eq))
               (local $free         (ref $Free))
               (local $code         (ref $ClosureCode))

               ;; Extract descriptor data
               (local.set $field-count   (struct.get $StructType $field-count      (local.get $std)))
               (local.set $guard         (struct.get $StructType $guard            (local.get $std)))
               (local.set $name          (struct.get $StructType $name             (local.get $std)))
               (local.set $init-indices  (struct.get $StructType $init-indices     (local.get $std)))
               (local.set $auto-indices  (struct.get $StructType $auto-indices     (local.get $std)))
               (local.set $auto-values   (struct.get $StructType $auto-values      (local.get $std)))

               ;; Choose code based on guard
               ;;  TODO We are ignoring guards for now.
               #;(local.set $code
                          (if (result (ref $ClosureCode))
                              (ref.eq (local.get $guard) (global.get $false))
                              (then (ref.func $struct-constructor/no-guard))
                              (else (ref.func $struct-constructor/with-guard))))

               (local.set $code (ref.func $struct-constructor/no-guard))

               ;; Build free array
               (local.set $free
                          (array.new_fixed $Free 5
                                           (local.get $std)
                                           (local.get $init-indices)
                                           (local.get $auto-indices)
                                           (local.get $auto-values)
                                           (local.get $name)))

               ;; Construct closure
               (struct.new $Closure 
                           (i32.const 0)               ; hash
                           (global.get $false)         ; name:  #f or $String
                           (global.get $zero)          ; arity: todo
                           (global.get $false)         ; realm: #f or $Symbol
                           (ref.func $invoke-closure)  ; invoke (used by apply, map, etc.)
                           (local.get $code)
                           (local.get $free)))


         (func $make-struct-mutator/checked
               ; Makes a generic mutator that can mutate any field.
               (param $std            (ref $StructType))
               (param $super-count-fx (ref eq))              ;; fixnum: number of supertype fields to skip
               (result                (ref eq))              ;; returns a closure

               (local $free (ref $Free))
               ;; Store the struct type and super-field count in the closure's free array
               (local.set $free (array.new_fixed $Free 2
                                                 (local.get $std)
                                                 (local.get $super-count-fx)))
               (struct.new $Closure
                           (i32.const 0)               ; hash
                           (global.get $false)         ; name:  #f or $String
                           (global.get $zero)          ; arity: todo
                           (global.get $false)         ; realm: #f or $Symbol
                           (ref.func $invoke-closure)  ; invoke (used by apply, map, etc.)
                           (ref.func $struct-mutator)
                           (local.get $free)))

         (func $struct-mutator  ; closure body
               (type $ClosureCode)
               (param $clos (ref $Closure))
               (param $args (ref $Args))
               (result      (ref eq)) ;; returns #<void>

               (local $free           (ref $Free))
               (local $std            (ref $StructType)) ; free[0]
               (local $super-count-fx (ref eq))          ; free[1]
               (local $super-count    i32)           

               
               (local $target    (ref eq))      ; args[0]
               (local $struct    (ref $Struct)) ;  = (cast $Struct $target)
               (local $index-fx  (ref eq))      ; args[1]
               (local $index     i32)           ;  = decoded $index-fx
               (local $val       (ref eq))      ; args[2]
               
               (local $fields    (ref $Array))  ; fields of the received struct
               (local $skip      i32)

               ;; --- Unpack closure free vars ---
               (local.set $free           (struct.get $Closure $free (local.get $clos)))
               (local.set $std            (ref.cast (ref $StructType)
                                                    (array.get $Free (local.get $free) (i32.const 0))))
               (local.set $super-count-fx (array.get $Free (local.get $free) (i32.const 1)))
               (local.set $super-count    (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $super-count-fx)))
                                                     (i32.const 1)))
               (local.set $skip           (local.get $super-count))

               ;; --- Get arguments ---
               ;; struct
               (local.set $target (array.get $Args (local.get $args) (i32.const 0)))
               (if (i32.eqz (ref.test (ref $Struct) (local.get $target)))
                   (then (call $raise-argument-error (local.get $target))))
               (local.set $struct (ref.cast (ref $Struct) (local.get $target)))
               ;; index
               (local.set $index-fx (array.get $Args (local.get $args) (i32.const 1)))
               (if (i32.or (i32.eqz (ref.test (ref i31) (local.get $index-fx)))
                           (i32.ne (i32.and (i31.get_u (ref.cast (ref i31)
                                                                 (local.get $index-fx)))
                                            (i32.const 1)) (i32.const 0)))
                   (then (call $raise-argument-error (local.get $index-fx))))
               (local.set $index (i32.shr_u (i31.get_u (ref.cast (ref i31)
                                                                 (local.get $index-fx)))
                                            (i32.const 1)))
               ;; value to store
               (local.set $val (array.get $Args (local.get $args) (i32.const 2)))

               ;; --- Type match ---
               ;  Check that the structure received has the same type as the mutator.
               ;; Type match - is $struct a subtype of $std
               (if (i32.eqz (call $struct-type-is-a?/i32
                                  (struct.get $Struct $type (local.get $struct))
                                  (local.get $std)))
                   (then (call $raise-argument-error (local.get $target))))
               ;; --- Set field ---
               (local.set $fields (struct.get $Struct $fields (local.get $struct)))
               (array.set $Array (local.get $fields)
                          (i32.add (local.get $skip) (local.get $index))
                          (local.get $val))
               ;; --- Return #<void> ---
               (global.get $void))

         
         
         (func $make-struct-mutator/specialized/checked ; helper
               ; The specialized version saves a fixed index to mutate
               ; in the free variables.
               
               (param $std            (ref $StructType))
               (param $super-count-fx (ref eq)) ;; fixnum
               (param $index-fx       (ref eq)) ;; fixnum
               (result                (ref eq)) ;; returns closure

               (local $free (ref $Free))
               (local.set $free (array.new_fixed $Free 3
                                                 (local.get $std)
                                                 (local.get $super-count-fx)
                                                 (local.get $index-fx)))
               (struct.new $Closure
                           (i32.const 0)               ; hash
                           (global.get $false)         ; name:  #f or $String
                           (global.get $zero)          ; arity: todo
                           (global.get $false)         ; realm: #f or $Symbol
                           (ref.func $invoke-closure)  ; invoke (used by apply, map, etc.)
                           (ref.func $struct-mutator/specialized)
                           (local.get $free)))

         (func $struct-mutator/specialized ; closure code
               (type $ClosureCode)
               (param $clos (ref $Closure))
               (param $args (ref $Args))
               (result (ref eq))

               (local $free           (ref $Free))
               (local $std            (ref $StructType)) ; free[0]
               (local $super-count-fx (ref eq))          ; free[1]
               (local $index-fx       (ref eq))          ; free[2]
               (local $super-count    i32)
               (local $index          i32)

               (local $target   (ref eq))      ; args[0]
               (local $struct   (ref $Struct))
               (local $val      (ref eq))      ; args[1]
               (local $fields   (ref $Array))
               (local $skip     i32)

               ;; --- Unpack free vars ---
               (local.set $free           (struct.get $Closure $free (local.get $clos)))
               (local.set $std            (ref.cast (ref $StructType)
                                                    (array.get $Free (local.get $free) (i32.const 0))))
               (local.set $super-count-fx (array.get $Free (local.get $free) (i32.const 1)))
               (local.set $index-fx       (array.get $Free (local.get $free) (i32.const 2)))
               (local.set $super-count    (i32.shr_u
                                           (i31.get_u (ref.cast (ref i31) (local.get $super-count-fx)))
                                           (i32.const 1)))
               (local.set $index          (i32.shr_u
                                           (i31.get_u (ref.cast (ref i31) (local.get $index-fx)))
                                           (i32.const 1)))
               (local.set $skip           (local.get $super-count))

               ;; --- Get arguments ---
               (local.set $target (array.get $Args (local.get $args) (i32.const 0)))
               (if (i32.eqz (ref.test (ref $Struct) (local.get $target)))
                   (then (call $raise-argument-error (local.get $target))))
               (local.set $struct (ref.cast (ref $Struct) (local.get $target)))

               (local.set $val (array.get $Args (local.get $args) (i32.const 1)))

               ;; --- Type check ---
               ;; Is $struct a subtype of $std
               (if (i32.eqz (call $struct-type-is-a?/i32
                                  (struct.get $Struct $type (local.get $struct))
                                  (local.get $std)))
                   (then (call $raise-argument-error3 (local.get $target))))

               ;; --- Set field ---
               (local.set $fields (struct.get $Struct $fields (local.get $struct)))
               (array.set $Array (local.get $fields)
                          (i32.add (local.get $skip) (local.get $index))
                          (local.get $val))

               ;; --- Return #<void> ---
               (global.get $void))

         (func $make-struct-field-mutator  ; Racket primitive
               ; Note: This functions uses the internal representation of
               ;       a mutator procedure closure.
               (param $mutator-proc      (ref eq)) ;; closure
               (param $field-pos-fx      (ref eq)) ;; fixnum
               (param $field/proc-name   (ref eq)) ;; ignored
               (param $arg-contract-str  (ref eq)) ;; ignored
               (param $realm             (ref eq)) ;; ignored
               (result                   (ref eq)) ;; returns a specialized mutator

               (local $clos             (ref $Closure))
               (local $code             (ref $ClosureCode))
               (local $free             (ref $Free))
               (local $std              (ref $StructType))
               (local $super-count-fx   (ref eq))

               ;; --- Check mutator-proc is a closure ---
               (if (i32.eqz (ref.test (ref $Closure) (local.get $mutator-proc)))
                   (then (call $raise-argument-error (local.get $mutator-proc))))
               (local.set $clos (ref.cast (ref $Closure) (local.get $mutator-proc)))

               ;; --- Extract fields ---
               (local.set $free           (struct.get $Closure $free (local.get $clos)))
               (local.set $std            (ref.cast (ref $StructType)
                                                    (array.get $Free
                                                               (local.get $free) (i32.const 0))))
               (local.set $super-count-fx (array.get $Free
                                                     (local.get $free) (i32.const 1)))

               ;; --- Call specialized mutator constructor ---
               (call $make-struct-mutator/specialized/checked
                     (local.get $std)
                     (local.get $super-count-fx)
                     (local.get $field-pos-fx)))


         
         (func $make-struct-predicate
               (param $v (ref eq)) ;; expected: (ref $StructType)
               (result (ref eq))   ;; returns: closure

               (if (i32.eqz (ref.test (ref $StructType) (local.get $v)))
                   (then (call $raise-argument-error (local.get $v))))

               (call $make-struct-predicate/checked
                     (ref.cast (ref $StructType) (local.get $v))))

         (func $make-struct-predicate/checked
               (param $std (ref $StructType))
               (result (ref eq)) ;; returns closure

               (local $free (ref $Free))

               (local.set $free
                          (array.new_fixed $Free 1 (local.get $std)))

               (struct.new $Closure
                           (i32.const 0)               ; hash
                           (global.get $false)         ; name:  #f or $String
                           (global.get $zero)          ; arity: todo
                           (global.get $false)         ; realm: #f or $Symbol
                           (ref.func $invoke-closure)  ; invoke (used by apply, map, etc.)
                           (ref.func $struct-predicate)
                           (local.get $free)))


         (func $struct-predicate  ; closure code
               (type $ClosureCode)

               (param $clos (ref $Closure))
               (param $args (ref $Args))
               (result      (ref eq))

               (local $free (ref $Free))
               (local $std  (ref $StructType))
               (local $v    (ref eq))
               (local $ok   i32)

               (local.set $free (struct.get $Closure $free (local.get $clos)))
               (local.set $std  (ref.cast (ref $StructType)
                                          (array.get $Free (local.get $free) (i32.const 0))))
               (local.set $v    (array.get $Args (local.get $args) (i32.const 0)))
               (local.set $ok   (if (result i32)
                                    (ref.test (ref $Struct) (local.get $v))
                                    (then ;; Is $struct a subtype of $std
                                     (if (result i32)
                                         (i32.eqz (call $struct-type-is-a?/i32
                                                        (struct.get $Struct $type 
                                                                    (ref.cast (ref $Struct) (local.get $v)))
                                                        (local.get $std)))
                                         (then (call $raise-argument-error (local.get $v))
                                               (unreachable))
                                         (else (i32.const 1))))
                                    (else (i32.const 0))))
               (if (result (ref eq))
                   (local.get $ok)
                   (then (global.get $true))
                   (else (global.get $false))))


         (func $make-struct-field-accessor ; primitive, section 5.2, returns specialized accessor
               ;; Parameters
               (param $accessor-proc  (ref eq))   ;; closure produced by make-struct-accessor
               (param $field-index-fx (ref eq))   ;; fixnum
               (param $name           (ref eq))   ;; symbol or #f (ignored)
               (param $contract-str   (ref eq))   ;; string/symbol/#f (ignored)
               (param $realm          (ref eq))   ;; symbol (ignored)

               (result (ref eq)) ;; closure: (λ (struct) field-value)

               ;; Locals
               (local $free  (ref $Free))

               ;; --- Type checks ---
               ;; Check accessor-proc is a closure
               (if (i32.eqz (ref.test (ref $Closure) (local.get $accessor-proc)))
                   (then (call $raise-argument-error (local.get $accessor-proc))))
               ;; Check field-index is a fixnum (i31 with lsb = 0)
               (if (i32.or (i32.eqz (ref.test (ref i31) (local.get $field-index-fx)))
                           (i32.ne (i32.and (i31.get_u (ref.cast (ref i31) (local.get $field-index-fx)))
                                            (i32.const 1))
                                   (i32.const 0)))
                   (then (call $raise-argument-error (local.get $field-index-fx))))               
               ;; --- Build Free vector ---
               (local.set $free (array.new_fixed $Free 2
                                                 (local.get $accessor-proc)
                                                 (local.get $field-index-fx)))
               ;; --- Return closure ---
               (struct.new $Closure
                           (i32.const 0)               ; hash
                           (global.get $false)         ; name:  #f or $String
                           (global.get $zero)          ; arity: todo
                           (global.get $false)         ; realm: #f or $Symbol
                           (ref.func $invoke-closure)  ; invoke (used by apply, map, etc.)
                           (ref.func $struct-field-accessor/specialized)
                           (local.get $free)))

         (func $struct-field-accessor/specialized
               ;; TODO: This just calls the generic accessor.
               ;;       Making a specialized accessor ought to be more effecient.
               (type $ClosureCode)
               (param $clos (ref $Closure))
               (param $args (ref $Args))
               (result      (ref eq))

               (local $free      (ref $Free))    
               (local $accessor  (ref $Closure)) ;; free[0] generic accessor
               (local $field-idx (ref eq))       ;; free[1] field-pos as fixnum

               (local $struct    (ref eq))       ;; args[0]
               
               (local $args2     (ref $Args))    ;; new arguments used to call the generic accessor

               ;; Unpack free vars
               (local.set $free       (struct.get $Closure $free (local.get $clos)))
               (local.set $accessor   (ref.cast (ref $Closure)
                                                (array.get $Free (local.get $free) (i32.const 0))))
               (local.set $field-idx            (array.get $Free (local.get $free) (i32.const 1)))
               ;; Get struct argument
               (local.set $struct     (array.get $Args (local.get $args) (i32.const 0)))
               ;; Construct args array: [struct, field-idx]
               (local.set $args2      (array.new_fixed $Args 2
                                                       (local.get  $struct)
                                                       (local.get  $field-idx)))
               ;; Call the generic accessor directly
               ; (inlined $call-closure (local.get $accessor) (local.get $args2))
               (call_ref $ClosureCode
                         (local.get $accessor)
                         (local.get $args2)
                         (struct.get $Closure $code (local.get $accessor))))


         (func $struct-constructor-procedure? (param $v (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.test (ref $StructConstructorProcedure) (local.get $v))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $struct-predicate-procedure? (param $v (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.test (ref $StructPredicateProcedure) (local.get $v))
                   (then (global.get $true))
                   (else (global.get $false))))
         
         (func $struct-accessor-procedure? (param $v (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.test (ref $StructAccessorProcedure) (local.get $v))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $struct-mutator-procedure? (param $v (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.test (ref $StructMutatorProcedure) (local.get $v))
                   (then (global.get $true))
                   (else (global.get $false))))



         ;; TODO
         #;(func $struct-constructor/with-guard
               (param $clos (ref $Closure))
               (param $args (ref $Args))
               (result (ref eq))

               (local $free         (ref $Free))
               (local $std          (ref $StructType))
               (local $init-indices (ref eq))
               (local $auto-indices (ref eq))
               (local $auto-values  (ref eq))
               (local $name         (ref eq))
               (local $field-count  i32)
               (local $arr          (ref $Array))
               (local $g-args       (ref $Args))
               (local $guard        (ref $Closure))
               (local $result       (ref eq))

               (local.set $free         (struct.get $Closure 1 (local.get $clos)))
               (local.set $std          (ref.cast (ref $StructType) (array.get $Free (local.get $free) (i32.const 0))))
               (local.set $init-indices (array.get $Free (local.get $free) (i32.const 1)))
               (local.set $auto-indices (array.get $Free (local.get $free) (i32.const 2)))
               (local.set $auto-values  (array.get $Free (local.get $free) (i32.const 3)))
               (local.set $name         (array.get $Free (local.get $free) (i32.const 4)))
               (local.set $field-count  (struct.get $StructType $field-count (local.get $std)))
               (local.set $guard        (ref.cast (ref $Closure) (struct.get $StructType $guard (local.get $std))))

               ;; Make $arr and fill it
               (local.set $arr (array.new_default $Array (global.get $false) (local.get $field-count)))
               (call $fill-fields-from-args (local.get $arr) (local.get $init-indices) (local.get $args))
               (call $fill-fields-from-values (local.get $arr) (local.get $auto-indices) (local.get $auto-values))

               ;; Build closure call: [clos, #f, ...fields, name]
               (local.set $g-args (array.new_default $Args (global.get $false) (i32.add (local.get $field-count) (i32.const 2))))
               (array.set $Args (local.get $g-args) (i32.const 0) (local.get $guard)) ;; closure
               (array.set $Args (local.get $g-args) (i32.const 1) (global.get $false)) ;; tail-call = #f

               ;; Copy fields into args
               (local $i i32)
               (local.set $i (i32.const 0))
               (block $done
                      (loop $loop
                            (br_if $done (i32.ge_u (local.get $i) (local.get $field-count)))
                            (array.set $Args (local.get $g-args)
                                       (i32.add (local.get $i) (i32.const 2))
                                       (array.get $Array (local.get $arr) (local.get $i)))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $loop)))

               ;; Add struct name at the end
               (array.set $Args (local.get $g-args)
                          (i32.add (local.get $field-count) (i32.const 2))
                          (local.get $name))

               ;; Call guard
               (local.set $result (call_indirect (type $clos-call-type) (local.get $guard) (local.get $g-args)))

               ;; Use returned struct
               (local.get $result))

         ; (call $fill-fields-from-args   (local.get $arr) (local.get $init-indices) (local.get $args))

         (func $fill-fields-from-args
               (param $fields  (ref $Array)) ;; struct field array
               (param $indices (ref eq))     ;; list of i31 fixnums (init field indices)
               (param $args    (ref $Args))  ;; argument array

               (local $i       i32)
               (local $arg     (ref eq))
               (local $index   i32)

               (local.set $i (i32.const 0))
               (block $done
                      (loop $loop
                            ;; If $indices is null, we’re done
                            (br_if $done (ref.eq (local.get $indices) (global.get $null)))
                            ;; Type check: $indices must be $Pair
                            (if (i32.eqz (ref.test (ref $Pair) (local.get $indices)))
                                (then (call $raise-argument-error (local.get $indices))))
                            ;; Extract current index (fixnum) and decode
                            (local.set $index (i32.shr_u
                                               (i31.get_u
                                                (ref.cast (ref i31)
                                                          (struct.get $Pair $a
                                                                      (ref.cast (ref $Pair)
                                                                                (local.get $indices)))))
                                               (i32.const 1)))
                            ;; Get arg[i]
                            (local.set $arg (array.get $Args (local.get $args) (local.get $i)))
                            ;; fields[index] := arg[i]
                            (array.set $Array (local.get $fields) (local.get $index) (local.get $arg))
                            ;; Move to next
                            (local.set $i       (i32.add (local.get $i) (i32.const 1)))
                            (local.set $indices (struct.get $Pair $d
                                                            (ref.cast (ref $Pair)
                                                                      (local.get $indices))))
                            (br $loop))))

         (func $fill-fields-from-values
               (param $fields  (ref $Array)) ;; struct field array
               (param $indices (ref eq))     ;; list of i31 fixnums (auto field indices)
               (param $values  (ref eq))     ;; list of values for auto fields
               (local $val (ref eq))
               (local $index i32)

               (block $done
                      (loop $loop
                            ;; Done if either list is null
                            (br_if $done
                                   (i32.or
                                    (ref.eq (local.get $indices) (global.get $null))
                                    (ref.eq (local.get $values)  (global.get $null))))
                            ;; Type checks
                            (if (i32.or (i32.eqz (ref.test (ref $Pair) (local.get $indices)))
                                        (i32.eqz (ref.test (ref $Pair) (local.get $values))))
                                (then (call $raise-argument-error (local.get $indices))))
                            ;; Decode fixnum index
                            (local.set $index (i32.shr_u
                                               (i31.get_u
                                                (ref.cast (ref i31)
                                                          (struct.get $Pair $a
                                                                      (ref.cast (ref $Pair)
                                                                                (local.get $indices)))))
                                               (i32.const 1)))
                            ;; Get value
                            (local.set $val (struct.get $Pair $a (ref.cast (ref $Pair) (local.get $values))))
                            ;; fields[index] := val
                            (array.set $Array (local.get $fields) (local.get $index) (local.get $val))
                            ;; Move to next
                            (local.set $indices (struct.get $Pair $d (ref.cast (ref $Pair) (local.get $indices))))
                            (local.set $values  (struct.get $Pair $d (ref.cast (ref $Pair) (local.get $values))))
                            (br $loop))))

         
         ;;;
         ;;; PROCEDURES
         ;;;

         (func $procedure?
               (param $v (ref eq))
               (result (ref eq))
               (if (result (ref eq))
                   (ref.test (ref $Procedure) (local.get $v))
                   (then (global.get $true))
                   (else (global.get $false))))

         ; TODO: Revise `apply` to handle:
         ;           (apply proc v ... xs)
         ; We are not handling v ... at the moment.

         ; Notes: repacking of arguments are done in $invoke-closure,
         ;        so no repacking is needed in $apply.
         
         (func $apply
               (param $proc (ref eq))   ;; procedure
               (param $xs   (ref eq))   ;; list of arguments
               (result (ref eq))        ;; result of applying the procedure

               (local $p    (ref $Procedure))
               (local $args (ref $Array))  ;; array of arguments

               ;; Step 1: type check $proc
               (if (i32.eqz (ref.test (ref $Procedure) (local.get $proc)))
                   (then (call $raise-argument-error:procedure-expected)))
               (local.set $p    (ref.cast (ref $Procedure) (local.get $proc)))
               ;; Step 2: convert list to array
               (local.set $args (call $list->array (local.get $xs)))
               ;; Step 3: apply via procedure's invoke field
               (return_call_ref $ProcedureInvoker
                                (local.get $p)
                                (local.get $args)
                                (struct.get $Procedure $invoke (local.get $p))))

         

         (func $procedure-rename
               (param $proc  (ref eq))   ;; any procedure
               (param $name  (ref eq))   ;; symbol
               (param $realm (ref eq))   ;; symbol or $missing
               (result (ref eq))

               (local $arity  (ref eq))
               (local $invoke (ref $ProcedureInvoker))
               (local $realm* (ref eq))

               ;; Step 1: If realm is #f, replace with 'racket
               (local.set $realm*
                          (if (result (ref eq))
                              (ref.eq (local.get $realm) (global.get $missing))
                              (then (global.get $symbol:racket))
                              (else (local.get $realm))))
               ;; Step 2: If $proc is a Closure
               (if (ref.test (ref $Closure) (local.get $proc))
                   (then
                    (return
                    (struct.new $Closure
                                (i32.const 0)                                 ;; hash
                                (local.get $name)
                                (struct.get $Closure $arity  (ref.cast (ref $Closure) (local.get $proc)))
                                (local.get $realm*)
                                (struct.get $Closure $invoke (ref.cast (ref $Closure) (local.get $proc)))
                                (struct.get $Closure $code   (ref.cast (ref $Closure) (local.get $proc)))
                                (struct.get $Closure $free   (ref.cast (ref $Closure) (local.get $proc)))))))
               ;; Step 3: If $proc is a PrimitiveClosure
               (if (ref.test (ref $PrimitiveClosure) (local.get $proc))
                   (then
                    (return
                    (struct.new $PrimitiveClosure
                                (i32.const 0)
                                (local.get $name)
                                (struct.get $PrimitiveClosure $arity  (ref.cast (ref $PrimitiveClosure) (local.get $proc)))
                                (local.get $realm*)
                                (struct.get $PrimitiveClosure $invoke (ref.cast (ref $PrimitiveClosure) (local.get $proc)))
                                (struct.get $PrimitiveClosure $result-arity
                                            (ref.cast (ref $PrimitiveClosure) (local.get $proc)))))))
               ;; Step 4: If $proc is a PrimitiveProcedure
               (if (ref.test (ref $PrimitiveProcedure) (local.get $proc))
                   (then
                    (return
                    (struct.new $PrimitiveProcedure
                                (i32.const 0)
                                (local.get $name)
                                (struct.get $PrimitiveProcedure $arity  (ref.cast (ref $PrimitiveProcedure) (local.get $proc)))
                                (local.get $realm*)
                                (struct.get $PrimitiveProcedure $invoke (ref.cast (ref $PrimitiveProcedure) (local.get $proc)))
                                (struct.get $PrimitiveProcedure $result-arity
                                            (ref.cast (ref $PrimitiveProcedure) (local.get $proc)))))))
               ;; Step 5: Not a supported procedure type
               (call $raise-argument-error:procedure-expected)
               (unreachable))
         
         
         (func $raise-argument-error:procedure-expected (unreachable))
         
         (func $procedure-arity
               ; TODO: This currently returns the arity as a fixnum.
               ;       Negative arities should be converted to an arity-at-least struct.
               (param $proc (ref eq))
               (result (ref eq))

               (local $p (ref $Procedure))
               (local $arity (ref eq))
               (local $arity-fx (ref eq))
               ;; 1. Check that $proc is a procedure
               (if (i32.eqz (ref.test (ref $Procedure) (local.get $proc)))
                   (then (call $raise-argument-error:procedure-expected (local.get $proc))
                         (unreachable)))
               ;; 2. Store in $p
               (local.set $p (ref.cast (ref $Procedure) (local.get $proc)))
               ;; 3. Extract arity field (can be a fixnum or (arity-at-least n))
               (local.set $arity (struct.get $Procedure $arity (local.get $p)))
               ;; 4. Return the arity value as-is (already a fixnum or arity object)
               (local.get $arity))
         
         (func $procedure-arity-mask
               ; TODO: Only tested with closures.
               ;       Also test with primitives and case-lambda
               (param $proc (ref eq))
               (result (ref eq))

               (local $p     (ref null $Procedure))
               (local $arity i32)
               (local $mask  i32)

               ;; Step 1: type check and cast
               (if (ref.test (ref $Procedure) (local.get $proc))
                   (then (local.set $p (ref.cast (ref $Procedure) (local.get $proc))))
                   (else (call $raise-argument-error:procedure-expected) (unreachable)))
               ;; Step 2: get i32 arity (decode from fixnum)
               (local.set $arity
                          (i32.shr_s
                           (i31.get_s
                            (ref.cast (ref i31) (struct.get $Procedure $arity (local.get $p))))
                           (i32.const 1)))
               ;; Step 3: compute mask
               (local.set $mask
                          (if (result i32)
                              (i32.ge_s (local.get $arity) (i32.const 0))
                              ;; If arity ≥ 0, mask = 1 << arity
                              (then (i32.shl (i32.const 1) (local.get $arity)))
                              ;; If arity < 0, mask = -1 << (-1 - arity)
                              (else (i32.shl (i32.const -1)
                                             (i32.sub (i32.const -1) (local.get $arity))))))
               ;; Step 4: return as fixnum
               (ref.i31 (i32.shl (local.get $mask) (i32.const 1))))

         (func $procedure-arity-mask/checked/i32
               ; TODO: Only tested with closures.
               ;       Also test with primitives and case-lambda
               (param $proc (ref $Procedure))
               (result      i32)

               (local $arity i32)
               (local $mask  i32)

               ;; Step 1: get i32 arity (decode from fixnum)
               (local.set $arity (i32.shr_s
                                  (i31.get_s
                                   (ref.cast (ref i31)
                                             (struct.get $Procedure $arity (local.get $proc))))
                                  (i32.const 1)))
               ;; Step 3: compute mask
               (local.set $mask
                          (if (result i32)
                              (i32.ge_s (local.get $arity) (i32.const 0))
                              ;; If arity ≥ 0, mask = 1 << arity
                              (then (i32.shl (i32.const 1) (local.get $arity)))
                              ;; If arity < 0, mask = -1 << (-1 - arity)
                              (else (i32.shl (i32.const -1)
                                             (i32.sub (i32.const -1) (local.get $arity))))))
               ;; Step 4: return as i32
               (local.get $mask))


         (func $raise-argument-error:fixnum-expected (unreachable))

         (func $procedure-arity-includes?
               (param $proc    (ref eq))
               (param $k       (ref eq))    ;; fixnum
               (param $kws-ok? (ref eq))    ;; fixnum or #f
               (result         (ref eq))    ;; returns #true or #false

               (local $k-fx    (ref i31))
               (local $k-i32   i32)
               (local $mask-fx (ref i31))
               (local $mask    i32)

               ;; Step 1: type check and decode k
               (if (ref.test (ref i31) (local.get $k))
                   (then
                    (local.set $k-fx (ref.cast (ref i31) (local.get $k)))
                    (local.set $k-i32 (i32.shr_u (i31.get_u (local.get $k-fx)) (i32.const 1))))
                   (else
                    (call $raise-argument-error:fixnum-expected)
                    (unreachable)))
               ;; Step 2: call procedure-arity-mask
               (local.set $mask-fx (ref.cast (ref i31)
                                             (call $procedure-arity-mask (local.get $proc))))
               (local.set $mask    (i32.shr_u (i31.get_u (local.get $mask-fx)) (i32.const 1)))
               ;; Step 3: check if bit $k-i32 is set in $mask
               (if (i32.ne (i32.and (local.get $mask)
                                    (i32.shl (i32.const 1) (local.get $k-i32)))
                           (i32.const 0))
                   (then (return (global.get $true)))
                   (else (return (global.get $false))))
               (unreachable))


         (func $procedure-arity-includes?/checked/i32
               (param $proc    (ref $Procedure))
               (param $k-i32   i32)
               ; (param $kws-ok? (ref eq))          ;; we don't use this yet
               (result         i32)

               (local $mask i32)

               ;; Step 1: get the arity mask
               (local.set $mask (call $procedure-arity-mask/checked/i32
                                      (local.get $proc)))
               ;; Step 2: check if bit $k-i32 is set in $mask
               (i32.ne (i32.and (local.get $mask)
                                (i32.shl (i32.const 1) (local.get $k-i32)))
                       (i32.const 0)))


         ;;;
         ;;; PRIMITIVES 
         ;;;

         ; https://docs.racket-lang.org/reference/procedures.html

         ; 4.20.2 Reflecting on Primitives

         (func $raise-argument-error:primitive-procedure-expected (unreachable))
         
         (func $primitive?
               (param $v (ref eq))
               (result (ref eq))

               (if (result (ref eq))
                   (ref.test (ref $PrimitiveProcedure) (local.get $v))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $primitive-result-arity
               (param $v (ref eq))
               (result (ref eq))

               (local $p (ref $PrimitiveProcedure))
               ;; Step 1: Validate type
               (if (i32.eqz (ref.test (ref $PrimitiveProcedure) (local.get $v)))
                   (then (call $raise-argument-error:primitive-procedure-expected)))
               ;; Step 2: Cast after validation
               (local.set $p (ref.cast (ref $PrimitiveProcedure) (local.get $v)))
               ;; Step 3: Return result-arity field
               (struct.get $PrimitiveProcedure $result-arity (local.get $p)))


         (func $primitive-closure?
               (param $v (ref eq))
               (result (ref eq))

               (if (result (ref eq))
                   (ref.test (ref $PrimitiveClosure) (local.get $v))
                   (then (global.get $true))
                   (else (global.get $false))))

         ;;;
         ;;; 13. INPUT AND OUTPUT
         ;;;

         ;; 13.10 Fast-Load Serialization

         (global $fasl-fixnum     (ref i31) (ref.i31 (i32.const 0x00)))
         (global $fasl-character  (ref i31) (ref.i31 (i32.const 0x01)))
         (global $fasl-symbol     (ref i31) (ref.i31 (i32.const 0x02)))
         (global $fasl-string     (ref i31) (ref.i31 (i32.const 0x03)))
         (global $fasl-bytes      (ref i31) (ref.i31 (i32.const 0x04)))
         (global $fasl-boolean    (ref i31) (ref.i31 (i32.const 0x05)))
         (global $fasl-null       (ref i31) (ref.i31 (i32.const 0x06)))
         (global $fasl-pair       (ref i31) (ref.i31 (i32.const 0x07)))
         (global $fasl-vector     (ref i31) (ref.i31 (i32.const 0x08)))
         (global $fasl-flonum     (ref i31) (ref.i31 (i32.const 0x09)))
         (global $fasl-void       (ref i31) (ref.i31 (i32.const 0x0a)))
         (global $fasl-eof        (ref i31) (ref.i31 (i32.const 0x0b)))
         
         (func $s-exp->fasl
               (param $v   (ref eq))
               ; optionals:
               (param $out (ref eq)) ;; a StringPort or #f (or $missing)              
               (result     (ref eq))

               (local $port (ref eq))
               (local $res  (ref eq))

               ; Handle optional arguments
               (if (ref.eq (local.get $out) (global.get $missing))
                   (then (local.set $out (global.get $false))))
               
               (if (result (ref eq))
                   (ref.eq (local.get $out) (global.get $false))
                   (then
                    (local.set $port (call $open-output-bytes))
                    (call $fasl:s-exp->fasl (local.get $v) (local.get $port))
                    (local.set $res (call $get-output-bytes (local.get $port)))
                    (local.get $res))
                   (else
                    (if (result (ref eq))
                        (ref.test (ref $StringPort) (local.get $out))
                        (then
                         (call $fasl:s-exp->fasl (local.get $v) (local.get $out))
                         (global.get $void))
                        (else (call $raise-check-port-or-false (local.get $out))
                              (unreachable))))))
                              
         (func $fasl:s-exp->fasl
               (param $v   (ref eq))
               (param $out (ref eq)) ;; a StringPort

               (local $i   i32)
               (local $n   i32)
               (local $vec (ref $Vector))
               (local $arr (ref $Array))
               
               ;; Dispatch by type tag
               (if (ref.test (ref i31) (local.get $v))
                   ;; Fixnum or immediate
                   (then                    
                    (local.set $i (i31.get_u (ref.cast (ref i31) (local.get $v))))
                    (if (i32.eqz (i32.and (local.get $i) (i32.const 1)))
                        ;; Fixnum
                        (then
                         (drop (call $write-byte (global.get $fasl-fixnum) (local.get $out)))
                         (call $fasl:write-u32 (i32.shr_u (local.get $i) (i32.const 1)) (local.get $out)))
                        ;; Immediate — test tag
                        (else
                         (call $s-exp->fasl/immediate (local.get $i) (local.get $v) (local.get $out)))))
                   
                   ;; Otherwise check boxed types
                   (else
                    (if (ref.test (ref $String) (local.get $v))
                        (then
                         (drop (call $write-byte (global.get $fasl-string) (local.get $out)))
                         (call $fasl:write-string (ref.cast (ref $String) (local.get $v)) (local.get $out)))

                        (else (if (ref.test (ref $Bytes) (local.get $v))
                                  (then
                                   (drop (call $write-byte (global.get $fasl-bytes) (local.get $out)))
                                   (call $fasl:write-bytes (ref.cast (ref $Bytes) (local.get $v)) (local.get $out)))

                                  (else (if (ref.test (ref $Symbol) (local.get $v))
                                            (then
                                             (drop (call $write-byte (global.get $fasl-symbol) (local.get $out)))
                                             (call $fasl:write-symbol (ref.cast (ref $Symbol) (local.get $v)) (local.get $out)))

                                            (else (if (ref.test (ref $Flonum) (local.get $v))
                                                      (then
                                                       (drop (call $write-byte (global.get $fasl-flonum) (local.get $out)))
                                                       (call $fasl:write-f64 (struct.get $Flonum $v (ref.cast (ref $Flonum) (local.get $v))) (local.get $out)))

                                                      (else (if (ref.test (ref $Pair) (local.get $v))
                                                                (then
                                                                 (drop (call $write-byte (global.get $fasl-pair) (local.get $out)))
                                                                 (call $fasl:s-exp->fasl
                                                                       (struct.get $Pair $a (ref.cast (ref $Pair) (local.get $v)))
                                                                       (local.get $out))
                                                                 (call $fasl:s-exp->fasl
                                                                       (struct.get $Pair $d (ref.cast (ref $Pair) (local.get $v)))
                                                                       (local.get $out)))
                                                                (else
                                                                 (if (ref.test (ref $Vector) (local.get $v))
                                                                     (then
                                                                      (local.set $vec (ref.cast (ref $Vector) (local.get $v)))
                                                                      (drop (call $write-byte (global.get $fasl-vector) (local.get $out)))
                                                                      (local.set $arr (struct.get $Vector $arr (local.get $vec)))
                                                                      (local.set $n (array.len (local.get $arr)))
                                                                      (call $fasl:write-u32 (local.get $n) (local.get $out))
                                                                      (local.set $i (i32.const 0))
                                                                      (loop $loop
                                                                            (br_if $loop
                                                                                   (i32.lt_u (local.get $i) (local.get $n))
                                                                                   (block
                                                                                    (call $fasl:s-exp->fasl
                                                                                          (array.get $Array (local.get $arr) (local.get $i))
                                                                                          (local.get $out))
                                                                                    (local.set $i (i32.add (local.get $i) (i32.const 1)))))))
                                                                     (else
                                                                      (unreachable) ;; unsupported type
                                                                      )))))))))))))))

         (func $s-exp->fasl/immediate
               (param $i i32)
               (param $v (ref eq))
               (param $out (ref eq))

               (local $b i32)

               ;; Character immediate
               (if (i32.eq (i32.and (local.get $i) (i32.const ,char-mask)) (i32.const ,char-tag))
                   (then
                    (drop (call $write-byte (global.get $fasl-character) (local.get $out)))
                    (call $fasl:write-u32 (i32.shr_u (local.get $i) (i32.const ,char-shift)) (local.get $out)))
                   (else
                    ;; Boolean immediate
                    (if (i32.eq (i32.and (local.get $i) (i32.const ,boolean-mask)) (i32.const ,boolean-tag))
                        (then
                         (local.set $b (i32.shr_u (local.get $i) (i32.const ,boolean-shift)))
                         (drop (call $write-byte (global.get $fasl-boolean) (local.get $out)))
                         (drop (call $write-byte (ref.i31 (i32.shl (local.get $b) (i32.const 1))) (local.get $out))))
                        (else
                         ;; Null immediate
                         (if (i32.eq (local.get $i) (i32.const ,empty-value))
                             (then
                              (drop (call $write-byte (global.get $fasl-null) (local.get $out))))
                             (else
                              ;; Void immediate
                              (if (i32.eq (local.get $i) (i32.const ,void-value))
                                  (then (drop (call $write-byte (global.get $fasl-void) (local.get $out))))
                                  (else
                                   ;; EOF immediate
                                   (if (i32.eq (local.get $i) (i32.const ,eof-value))
                                       (then (drop (call $write-byte (global.get $fasl-eof) (local.get $out))))
                                       (else (unreachable))))))))))))
         
        (func $fasl:write-u32
              (param $v i32)
              (param $out (ref eq))

              ;; write the four bytes of $v in big-endian order
              (drop
               (call $write-byte
                     (ref.i31 (i32.shl (i32.and (i32.shr_u (local.get $v) (i32.const 24)) (i32.const 255)) (i32.const 1)))
                     (local.get $out)))
              (drop
               (call $write-byte
                     (ref.i31 (i32.shl (i32.and (i32.shr_u (local.get $v) (i32.const 16)) (i32.const 255)) (i32.const 1)))
                     (local.get $out)))
              (drop
               (call $write-byte
                     (ref.i31 (i32.shl (i32.and (i32.shr_u (local.get $v) (i32.const 8)) (i32.const 255)) (i32.const 1)))
                     (local.get $out)))
              (drop
               (call $write-byte
                     (ref.i31 (i32.shl (i32.and (local.get $v) (i32.const 255)) (i32.const 1)))
                     (local.get $out))))

        (func $fasl:write-bytes
               (param $b (ref $Bytes))
               (param $out (ref eq))

               (local $arr (ref $I8Array))
               (local $len i32)
               (local $i   i32)
               (local $val i32)

               ;; write length first
               (local.set $arr (struct.get $Bytes $bs (local.get $b)))
               (local.set $len (array.len (local.get $arr)))
               (call $fasl:write-u32 (local.get $len) (local.get $out))

               ;; output each byte
               (local.set $i (i32.const 0))
               (block $done
                      (loop $loop
                            (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
                            (local.set $val (call $i8array-ref (local.get $arr) (local.get $i)))
                            (drop (call $write-byte
                                        (ref.i31 (i32.shl (local.get $val) (i32.const 1)))
                                        (local.get $out)))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $loop))))

         (func $fasl:write-string
               (param $s (ref $String))
               (param $out (ref eq))

               (local $bs (ref eq))

               (local.set $bs (call $string->bytes/utf-8 (local.get $s) (global.get $false) (global.get $false) (global.get $false)))
               (call $fasl:write-bytes (ref.cast (ref $Bytes) (local.get $bs)) (local.get $out)))

         (func $fasl:write-symbol
               (param $sym (ref $Symbol))
               (param $out (ref eq))

               (call $fasl:write-string (struct.get $Symbol $name (local.get $sym)) (local.get $out)))

         (func $fasl:write-f64
               (param $v f64)
               (param $out (ref eq))

               (local $bits i64)
               (local $hi i32)
               (local $lo i32)

               (local.set $bits (i64.reinterpret_f64 (local.get $v)))
               (local.set $hi   (i32.wrap_i64 (i64.shr_u (local.get $bits) (i64.const 32))))
               (local.set $lo   (i32.wrap_i64 (local.get $bits)))
               (call $fasl:write-u32 (local.get $hi) (local.get $out))
               (call $fasl:write-u32 (local.get $lo) (local.get $out)))
         
         ;;; Fasl decoding
                  
         (func $fasl->s-exp
              ;; entry point: decode byte string
              (param $bs (ref eq))
              (result    (ref eq))

              (local $b   (ref $Bytes))
              (local $arr (ref $I8Array))
              (local $len i32)
              (local $val (ref eq))
              (local $i   i32)

              (if (i32.eqz (ref.test (ref $Bytes) (local.get $bs)))
                  (then (call $raise-argument-error (local.get $bs))
                        (unreachable)))
              (local.set $b   (ref.cast (ref $Bytes) (local.get $bs)))

              (local.set $arr (struct.get $Bytes $bs (local.get $b)))
              (local.set $len (array.len (local.get $arr)))
              (local.set $i   (i32.const 0))
              ;; ; the call returns two values: the decoded value and the index
              (call $fasl:read-s-exp (local.get $arr) (local.get $len) (local.get $i))
              (local.set $i) (local.set $val) ; note: reversed compared to $fasl:read-s-exp
              
              (local.get $val))


        (func $fasl:read-u32
              ;; read 4 bytes as big-endian u32
              ;; returns value and new index 
              (param $arr (ref $I8Array))
              (param $i   i32)
              (result i32 i32)

              (local $b0 i32)
              (local $b1 i32)
              (local $b2 i32)
              (local $b3 i32)

              (local.set $b0 (array.get_u $I8Array (local.get $arr)          (local.get $i)))
              (local.set $b1 (array.get_u $I8Array (local.get $arr) (i32.add (local.get $i) (i32.const 1))))
              (local.set $b2 (array.get_u $I8Array (local.get $arr) (i32.add (local.get $i) (i32.const 2))))
              (local.set $b3 (array.get_u $I8Array (local.get $arr) (i32.add (local.get $i) (i32.const 3))))
              
              (return (i32.or   (i32.shl (local.get $b0) (i32.const 24))
                                (i32.or  (i32.shl (local.get $b1) (i32.const 16))
                                         (i32.or (i32.shl (local.get $b2) (i32.const  8))
                                                 (local.get $b3))))
                      (i32.add (local.get $i) (i32.const 4))))

        (func $fasl:read-bytes
               ;; read length-prefixed byte array
               (param $arr (ref $I8Array))
               (param $i   i32)
               (result     (ref $Bytes) i32)

               (local $len  i32)
               (local $next i32)
               (local $data (ref $I8Array))

               (call $fasl:read-u32 (local.get $arr) (local.get $i))
               (local.set $next) (local.set $len) ; note: reversed

               (local.set $data (call $i8array-copy
                                      (local.get $arr)
                                      (local.get $next)
                                      (i32.add (local.get $next) (local.get $len))))
               (return
                (struct.new $Bytes
                            (i32.const 0)      ; hash 
                            (i32.const 1)      ; immutable
                            (local.get $data)) ; I8Array
                (i32.add (local.get $next) (local.get $len))))

        (func $fasl:read-string
              ;; read bytes and convert to string
              (param $arr (ref $I8Array))
              (param $i   i32)
              (result     (ref $String) i32)

              (local $bs   (ref $Bytes))
              (local $next i32)

              (call $fasl:read-bytes (local.get $arr) (local.get $i))
              (local.set $next) (local.set $bs) ; note: reversed
              
              (return (call $bytes->string/utf-8/checked (local.get $bs))
                      (local.get $next)))

        (func $fasl:read-symbol
              ;; read string and intern symbol
              (param $arr (ref $I8Array))
              (param $i   i32)
              (result     (ref $Symbol) i32)

              (local $str  (ref $String))
              (local $next i32)

              (call $fasl:read-string (local.get $arr) (local.get $i))
              (local.set $next) (local.set $str) ; note: reversed
              
              (return (call $string->symbol (local.get $str))
                      (local.get $next)))

        (func $fasl:read-f64
              ;; read IEEE double
              (param $arr (ref $I8Array))
              (param $i   i32)
              (result     (ref $Flonum) i32)

              (local $hi   i32)
              (local $idx  i32)
              (local $lo   i32)
              (local $next i32)
              (local $bits i64)

              (call $fasl:read-u32 (local.get $arr) (local.get $i))
              (local.set $idx) (local.set $hi) ; reversed
              
              (call $fasl:read-u32 (local.get $arr) (local.get $idx))
              (local.set $next) (local.set $lo) ; reversed
              
              (local.set $bits
                         (i64.or (i64.shl (i64.extend_i32_u (local.get $hi)) (i64.const 32))
                                 (i64.extend_i32_u (local.get $lo))))
              (return
               (struct.new $Flonum
                           (i32.const 0)
                           (f64.reinterpret_i64 (local.get $bits)))
               (local.get $next)))

        (func $fasl:read-s-exp
              ;; decode one FASL value by tag
              (param $arr (ref $I8Array))
              (param $len i32)
              (param $i   i32)
              (result (ref eq) i32)

              (local $tag  i32)
              (local $val  i32)
              (local $cp   i32)
              (local $sym  (ref $Symbol))
              (local $str  (ref $String))
              (local $bs   (ref $Bytes))
              (local $b    i32)
              (local $car  (ref eq))
              (local $cdr  (ref eq))
              (local $n    i32)
              (local $vec  (ref $Vector))
              (local $j    i32)
              (local $elem (ref eq))
              (local $fl   (ref $Flonum))
              
              (local.set $tag (array.get_u $I8Array (local.get $arr) (local.get $i)))
              (local.set $i   (i32.add (local.get $i) (i32.const 1)))

              ;; fixnum
              (if (result (ref eq) i32)
                  (i32.eq (local.get $tag) (i31.get_u (global.get $fasl-fixnum)))
                  (then (call $fasl:read-u32 (local.get $arr) (local.get $i))
                        (local.set $i) (local.set $val)                         
                        (return (ref.i31 (i32.shl (local.get $val) (i32.const 1)))
                                (local.get $i)))
               (else
                ;; character
                (if (result (ref eq) i32)
                    (i32.eq (local.get $tag) (i31.get_u (global.get $fasl-character)))
                    (then (call $fasl:read-u32 (local.get $arr) (local.get $i))
                          (local.set $i) (local.set $cp) 
                          (return (ref.i31 (i32.or (i32.shl (local.get $cp) (i32.const ,char-shift))
                                                   (i32.const ,char-tag)))
                                  (local.get $i)))
               (else
                ;; symbol
                (if (result (ref eq) i32)
                    (i32.eq (local.get $tag) (i31.get_u (global.get $fasl-symbol)))
                    (then (call $fasl:read-symbol (local.get $arr) (local.get $i))
                          (local.set $i) (local.set $sym) 
                          (return (local.get $sym) 
                                  (local.get $i)))
               (else
                ;; string
                (if (result (ref eq) i32)
                    (i32.eq (local.get $tag) (i31.get_u (global.get $fasl-string)))
                    (then (call $fasl:read-string (local.get $arr) (local.get $i))
                          (local.set $i) (local.set $str) 
                          (return (local.get $str)
                                  (local.get $i)))
               (else
                ;; bytes
                (if (result (ref eq) i32)
                    (i32.eq (local.get $tag) (i31.get_u (global.get $fasl-bytes)))
                    (then (call $fasl:read-bytes (local.get $arr) (local.get $i))
                          (local.set $i) (local.set $bs) 
                          (return (local.get $bs)
                                  (local.get $i)))
               (else
                ;; boolean
                (if (result (ref eq) i32)
                    (i32.eq (local.get $tag) (i31.get_u (global.get $fasl-boolean)))
                    (then (local.set $b (array.get_u $I8Array (local.get $arr) (local.get $i)))
                          (local.set $i (i32.add (local.get $i) (i32.const 1)))
                          (return (if (result (ref eq))
                                      (i32.ne (local.get $b) (i32.const 0))
                                      (then (global.get $true))
                                      (else (global.get $false)))
                                  (local.get $i)))
               (else
                ;; null
                (if (result (ref eq) i32)
                    (i32.eq (local.get $tag) (i31.get_u (global.get $fasl-null)))
                    (then (return (global.get $null)
                                  (local.get $i)))
               (else
                ;; pair
                (if (result (ref eq) i32)
                    (i32.eq (local.get $tag) (i31.get_u (global.get $fasl-pair)))
                    (then (call $fasl:read-s-exp (local.get $arr) (local.get $len) (local.get $i))
                          (local.set $i) (local.set $car) 
                          (call $fasl:read-s-exp (local.get $arr) (local.get $len) (local.get $i))
                          (local.set $i) (local.set $cdr) 
                          (return (call $cons (local.get $car) (local.get $cdr))
                                  (local.get $i)))
               (else
                ;; vector
                (if (result (ref eq) i32)
                    (i32.eq (local.get $tag) (i31.get_u (global.get $fasl-vector)))
                    (then (call $fasl:read-u32 (local.get $arr) (local.get $i))
                          (local.set $i) (local.set $n) 
                          (local.set $vec (call $make-vector/checked (local.get $n) (global.get $void)))
                          (local.set $j (i32.const 0))
                          (block $done
                                 (loop $loop
                                       (br_if $done (i32.ge_u (local.get $j) (local.get $n)))
                                       (call $fasl:read-s-exp (local.get $arr) (local.get $len) (local.get $i))
                                       (local.set $i) (local.set $elem) 
                                       (call $vector-set!/checked (local.get $vec) (local.get $j) (local.get $elem))
                                       (local.set $j (i32.add (local.get $j) (i32.const 1)))
                                       (br $loop)))
                          (return (local.get $vec)
                                  (local.get $i)))
               (else
                ;; flonum
                (if (result (ref eq) i32)
                    (i32.eq (local.get $tag) (i31.get_u (global.get $fasl-flonum)))
                    (then (call $fasl:read-f64 (local.get $arr) (local.get $i))
                          (local.set $i) (local.set $fl) 
                          (return (local.get $fl)
                                  (local.get $i)))
               (else
                ;; void
                (if (result (ref eq) i32)
                    (i32.eq (local.get $tag) (i31.get_u (global.get $fasl-void)))
                    (then (return (global.get $void)
                                  (local.get $i)))
               (else
                ;; eof
                (if (result (ref eq) i32)
                    (i32.eq (local.get $tag) (i31.get_u (global.get $fasl-eof)))
                    (then (return (ref.i31 (i32.const ,eof-value))
                                  (local.get $i)))
                    (else (unreachable))))))))))))))))))))))))))


         ;;;
         ;;; 14. REFLECTION AND SECURITY
         ;;;

         ;; 14.1 Namespaces

         ; We need dummy implementations of `#%variable-reference` and `variable-reference-from-unsafe?`
         ; in order to run code from an expand `for`.

         ; The form `#%variable-reference` can occur in a fully expanded syntax,
         ; so it is handled in the elsewhere (for now, we only handle the case `(#%variable-reference)`.

         ; This function determines if the variable stems from a module compiled in unsafe mode or not.
         (func $variable-reference-from-unsafe?
               (param  $varref (ref eq))
               (result (ref eq))
               (global.get $true))

         (func $variable-reference-constant?
               (param $varref (ref eq))
               (result (ref eq))
               (global.get $true))  ; todo: simple implementation for now.

         (func $raise-unbound-variable-reference
               (result (ref eq))
               (unreachable))
         
         ;; 14.2 Evaluation and compilation
         ;; 14.3 The racket/load language
         ;; 14.4 Module names and loading
         ;; 14.5 Impersonators and chaperones
         ;; 14.6 Security Guards
         ;; 14.7 Custodians
         ;; 14.8 Thread Groups
         ;; 14.9 Structure Inspectors

         (func $current-inspector   ; TODO: dummy 
               (result (ref eq))
               (global.get $false))
         
         ;; 14.10 Code Inspectors
         ;; 14.11 Plumbers
         ;; 14.12 Sandboxed Evaluation
         ;; 14.13 The racket/repl library
         ;; 14.14 Linklets and the compiler
         
         ;;;
         ;;; 17. UNSAFE OPERATIONS
         ;;;

         ;; 17.1 Unsafe Numeric Operations

         (func $unsafe-fx+
               (param $x (ref eq))   ;; x must be a fixnum (i31 with lsb = 0)
               (param $y (ref eq))   ;; y must be a fixnum (i31 with lsb = 0)
               (result   (ref eq))   ;; result is a fixnum (i31)

               ; the tag was chosen, so we could do this:
               (ref.i31 (i32.add (i31.get_s (ref.cast (ref i31) (local.get $x)))
                                 (i31.get_s (ref.cast (ref i31) (local.get $y))))))

         (func $unsafe-fx=
               (param $x (ref eq))
               (param $y (ref eq))
               (result   (ref eq))

               (if (result (ref eq))
                   (i32.eq (i31.get_s (ref.cast (ref i31) (local.get $x)))
                           (i31.get_s (ref.cast (ref i31) (local.get $y))))
                   (then (global.get $true))
                   (else (global.get $false))))
         
         (func $unsafe-fx<
               (param $x (ref eq)) ;; x must be a fixnum (i31 with lsb = 0)
               (param $y (ref eq)) ;; y must be a fixnum (i31 with lsb = 0)
               (result   (ref eq)) ;; result is a boolean

               (if (result (ref eq))
                   (i32.lt_s (i31.get_s (ref.cast (ref i31) (local.get $x)))
                             (i31.get_s (ref.cast (ref i31) (local.get $y))))
                   (then (global.get $true))
                   (else (global.get $false))))

         
         ;; 17.2 Unsafe Character Operations
         ;; 17.3 Unsafe Compound-Data Operations

         (func $unsafe-car (param $v (ref eq)) (result (ref eq))
               (struct.get $Pair $a (ref.cast (ref $Pair) (local.get $v))))

         (func $unsafe-cdr (param $v (ref eq)) (result (ref eq))
               (struct.get $Pair $d (ref.cast (ref $Pair) (local.get $v))))

         ;; Note the `unsafe-vector*-...` variants do not work on impersonators.
         ;; (the `unsafe-vector-...` variants do)
         (func $unsafe-vector*-length (param $v (ref eq)) (result (ref eq))
               (local $vec (ref $Vector))
               (local.set $vec (ref.cast (ref $Vector) (local.get $v)))
               (ref.i31 (i32.shl
                         (array.len
                          (struct.get $Vector $arr (local.get $vec)))
                         (i32.const 1))))

         (func $unsafe-vector*-set!
               (param $vec (ref eq))
               (param $idx (ref eq))
               (param $val (ref eq))
               (result     (ref eq))
               
               (array.set $Array
                          (struct.get $Vector $arr 
                                      (ref.cast (ref $Vector) (local.get $vec)))
                          (i32.shr_s (i31.get_s (ref.cast (ref i31)
                                                          (local.get $idx)))
                                     (i32.const 1))
                          (local.get $val))
               (global.get $void))

         ;; 17.4 Unsafe Extflonum Operations
         ;; 17.5 Unsafe Impersonators and Chaperones
         ;; 17.6 Unsafe Assertions
         ;; 17.7 Unsafe Undefined
         
         
         
         
         ;;;
         ;;; FORMATTING - DISPLAY MODE
         ;;;

         ;; Note: If you change the string constants, remember to change their
         ;;       lengths below.
         
         (data $str-true-bytes            "#t")
         (data $str-false-bytes           "#f")
         (data $str-null-bytes            "()")
         (data $str-void-bytes            "#<void>")
         (data $str-undefined-bytes       "#<undefined>")
         (data $str-unspecified-bytes     "#<unspecified>")
         (data $str-missing-bytes         "#<missing>")
         (data $str-closure-bytes         "#<closure>")
         (data $str-empty-bytes           "")
         (data $str-open-paren-bytes      "(")
         (data $str-close-paren-bytes     ")")
         (data $str-space-bytes           " ")
         (data $str-dot-space-bytes       ". ")
         (data $str-space-dot-space-bytes " . ")
         (data $str-vector-prefix-bytes   "#(")
         (data $str-values-prefix-bytes   "(values")
         (data $str-g-bytes               "g")

         (data $str-hash-colon-bytes       "#:")
         (data $str-hash-backslash-bytes   "#\\")
         (data $str-hash-backslash-u-bytes "#\\u")
         (data $str-hash-backslash-U-bytes "#\\U")
         (data $str-word-newline-bytes     "newline")
         (data $str-word-tab-bytes         "tab")
         (data $str-word-return-bytes      "return")
         (data $str-word-backspace-bytes   "backspace")
         (data $str-word-space-bytes       "space")
         (data $str-word-rubout-bytes      "rubout")
         (data $str-word-nul-bytes         "nul")
         ; realms
         (data $str-racket-bytes           "racket")
         (data $str-racket/primitive-bytes "racket/primitive")
         ; formatting
         ;; Byte literals
         (data $str-hash-less-procedure-colon-bytes "#<procedure:")
         (data $str-hash-less-primitive-colon-bytes "#<primitive:")
         (data $str-unknown-bytes      "unknown")
         (data $str-colon-bytes        ":")
         (data $str->-bytes            ">")

         (func $i8array->string (param $arr (ref $I8Array)) (result (ref $String))
               (call $bytes->string/utf-8/checked
                     (call $i8array->immutable-bytes
                           (local.get $arr))))
         
         (func $str-true (export "str-true") (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-true-bytes (i32.const 0) (i32.const 2))))
         (func $str-false (export "str-false") (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-false-bytes (i32.const 0) (i32.const 2))))
         (func $str-null (export "str-null") (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-null-bytes (i32.const 0) (i32.const 2))))
         (func $str-void (export "str-void") (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-void-bytes (i32.const 0) (i32.const 7))))
         (func $str-empty (export "str-empty") (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-empty-bytes (i32.const 0) (i32.const 0))))
         (func $str-undefined (export "str-undefined") (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-undefined-bytes (i32.const 0) (i32.const 13))))
         (func $str-unspecified (export "str-unspecified") (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-unspecified-bytes (i32.const 0) (i32.const 15))))
         (func $str-missing (export "str-missing") (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-missing-bytes (i32.const 0) (i32.const 10))))
         (func $str-closure (export "str-closure") (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-closure-bytes (i32.const 0) (i32.const 10))))
         (func $str-open-paren (export "str-open-paren") (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-open-paren-bytes (i32.const 0) (i32.const 1))))
         (func $str-close-paren (export "str-close-paren") (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-close-paren-bytes (i32.const 0) (i32.const 1))))
         (func $str-space (export "str-space") (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-space-bytes (i32.const 0) (i32.const 1))))
         (func $str-dot-space (export "str-dot-space") (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-dot-space-bytes (i32.const 0) (i32.const 2))))
         (func $str-space-dot-space (export "str-space-dot-space") (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-space-dot-space-bytes (i32.const 0) (i32.const 3))))
         (func $str-vector-prefix (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-vector-prefix-bytes (i32.const 0) (i32.const 2))))
         (func $str-values-prefix (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-values-prefix-bytes (i32.const 0) (i32.const 7))))
         (func $str-g (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-g-bytes (i32.const 0) (i32.const 2))))

         (func $str-hash-colon (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-hash-colon-bytes (i32.const 0) (i32.const 2))))
         (func $str-hash-backslash (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-hash-backslash-bytes (i32.const 0) (i32.const 2))))
         (func $str-hash-backslash-u (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-hash-backslash-u-bytes (i32.const 0) (i32.const 3))))
         (func $str-hash-backslash-U (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-hash-backslash-U-bytes (i32.const 0) (i32.const 3))))
         (func $str-word-newline (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-word-newline-bytes (i32.const 0) (i32.const 7))))
         (func $str-word-tab (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-word-tab-bytes (i32.const 0) (i32.const 3))))
         (func $str-word-return (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-word-return-bytes (i32.const 0) (i32.const 6))))
         (func $str-word-backspace (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-word-backspace-bytes (i32.const 0) (i32.const 9))))
         (func $str-word-space (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-word-space-bytes (i32.const 0) (i32.const 5))))
         (func $str-word-rubout (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-word-rubout-bytes (i32.const 0) (i32.const 6))))
         (func $str-word-nul (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-word-nul-bytes (i32.const 0) (i32.const 3))))
         ; Realms
         (func $str-racket (export "str-racket") (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-racket-bytes (i32.const 0) (i32.const 6))))
         (func $str-racket/primitive (export "str-racket/primitive") (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-racket/primitive-bytes (i32.const 0) (i32.const 16))))

         ;; Formatting: procedure
         (func $str-hash-less-procedure-colon
               (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-hash-less-procedure-colon-bytes (i32.const 0) (i32.const 12))))
         (func $str-hash-less-primitive-colon
               (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-hash-less-primitive-colon-bytes (i32.const 0) (i32.const 12))))
         (func $str-unknown (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-unknown-bytes (i32.const 0) (i32.const 7))))
         (func $str-colon (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str-colon-bytes (i32.const 0) (i32.const 1))))
         (func $str-> (result (ref $String))
               (call $i8array->string
                     (array.new_data $I8Array $str->-bytes (i32.const 0) (i32.const 1))))


         (func $raise-format/display:unknown-datatype (unreachable))

         (func $raise-format/display:got-boxed  (unreachable))
         (func $raise-format/display:got-box    (unreachable))
         (func $raise-format/display:got-values (unreachable))

         (func $format/display
               (param $v (ref eq))
               (result   (ref $String))

               (local $s   (ref $String))
               (local $i31 (ref i31))
               (local $n   i32)
               ;; --- Case: fixnum ---
               (if (ref.test (ref i31) (local.get $v))
                   (then (local.set $i31 (ref.cast (ref i31) (local.get $v)))
                         (local.set $n (i31.get_u (local.get $i31)))
                         (if (i32.eqz (i32.and (local.get $n) (i32.const 1))) ;; check lsb = 0
                             (then (return (call $number->string (local.get $v) ,(Imm 10)))))))
               ;; --- Case: null ---
               (if (ref.eq (local.get $v) (global.get $null))
                   (then (return (call $str-null))))
               ;; --- Case: true ---
               (if (ref.eq (local.get $v) (global.get $true))
                   (then (return (call $str-true))))
               ;; --- Case: false ---
               (if (ref.eq (local.get $v) (global.get $false))
                   (then (return (call $str-false))))
               ;; --- Case: void ---
               (if (ref.eq (local.get $v) (global.get $void))
                   (then (return (call $str-void))))
               ;; --- Case: missing ---
               (if (ref.eq (local.get $v) (global.get $missing))
                   (then (return (call $str-missing))))
               ;; --- Case: closure ---
               (if (ref.test (ref $Closure) (local.get $v))
                   (then (return (call $format/display:procedure
                                       (ref.cast (ref $Procedure) (local.get $v))))))
               ;; --- Case: primitive ---
               (if (ref.test (ref $PrimitiveProcedure) (local.get $v))
                   (then (return (call $format/display:primitive-procedure
                                       (ref.cast (ref $PrimitiveProcedure) (local.get $v))))))
               ;; --- Case: procedure ---
               (if (ref.test (ref $Procedure) (local.get $v))
                   (then (return (call $format/display:procedure
                                       (ref.cast (ref $Procedure) (local.get $v))))))
               ;; --- Case: string ---
               (if (ref.test (ref $String) (local.get $v))
                   (then (return (ref.cast (ref $String) (local.get $v)))))
               ;; --- Case: bytes ---
               (if (ref.test (ref $Bytes) (local.get $v))
                   (then (return (call $bytes->string/utf-8/defaults
                                       (ref.cast (ref $Bytes) (local.get $v))))))
               ;; --- Case: pair ---
               (if (ref.test (ref $Pair) (local.get $v))
                   (then (return (call $format/display:pair
                                       (ref.cast (ref $Pair) (local.get $v))))))
               ;; --- Case: vector ---
               (if (ref.test (ref $Vector) (local.get $v))
                   (then (return (call $format/display:vector
                                       (ref.cast (ref $Vector) (local.get $v))))))
               ;; --- Case: box ---
               (if (ref.test (ref $Box) (local.get $v))
                   (then (return (call $format/display:box
                                       (ref.cast (ref $Box) (local.get $v))))))
               ;; --- Case: symbol ---
               (if (ref.test (ref $Symbol) (local.get $v))
                   (then (return (call $format/display:symbol
                                       (ref.cast (ref $Symbol) (local.get $v))))))
               ;; --- Case: keyword ---
               (if (ref.test (ref $Keyword) (local.get $v))
                   (then (return (call $keyword->string
                                       (ref.cast (ref $Keyword) (local.get $v))))))
               ;; --- Case: flonum ---
               (if (ref.test (ref $Flonum) (local.get $v))
                   (then (return (call $format/display:flonum
                                       (ref.cast (ref $Flonum) (local.get $v))))))
               ;; --- Case: values ---
               (if (ref.test (ref $Values) (local.get $v))
                   (then (return (call $format/display:values
                                       (ref.cast (ref $Values) (local.get $v))))))
               ;; --- Case: struct ---
               (if (ref.test (ref $Struct) (local.get $v))
                   (then (return (call $format/display:struct
                                       (ref.cast (ref $Struct) (local.get $v))))))
               ;; --- Case: char ---
               (if (ref.test (ref i31) (local.get $v))
                   (then (local.set $i31 (ref.cast (ref i31) (local.get $v)))
                         (local.set $n   (i31.get_u (local.get $i31)))
                         (if (i32.eq (i32.and (local.get $n) (i32.const ,char-mask))
                                     (i32.const ,char-tag))
                             (then (return (call $format/display:char (local.get $v)))))))
               ;; --- Case: variable-reference ---
               (if (ref.test (ref $VariableReference) (local.get $v))
                   (then (return (call $format/display:variable-reference
                                       (ref.cast (ref $VariableReference) (local.get $v))))))
               ;; --- Internal data types ---
               ;; These shouldn't leak to the outside, but nice to know if it happens.
               ;; --- Case: boxed --- (shouldn't happen)
               (if (ref.test (ref $Boxed) (local.get $v))
                   (then (call $raise-format/display:got-boxed)))
               ;; --- Fallback ---
               (call $raise-format/display:unknown-datatype)
               (unreachable))

         (func $format/display:variable-reference
               (param $v (ref eq))
               (result   (ref $String))
               (ref.cast (ref $String)
                         (global.get $string:hash-variable-reference)))

         (func $format/display:procedure
               ; #<procedure:name:arity:mask>
               (param $v          (ref eq))
               (result            (ref $String))

               (local $p          (ref $Procedure))
               (local $name       (ref eq))         ;; $false or $String
               (local $arity-fx   (ref eq))         ;; fixnum (i31)
               (local $arity-str  (ref $String))
               (local $mask       i32)
               (local $mask-str   (ref $String))
               (local $ga         (ref $GrowableArray))

               ;; Step 1: type check and cast
               (if (i32.eqz (ref.test (ref $Procedure) (local.get $v)))
                   (then (call $raise-argument-error:procedure-expected)))
               (local.set $p (ref.cast (ref $Procedure) (local.get $v)))
               ;; Step 2: extract fields
               (local.set $name      (struct.get $Procedure $name (local.get $p)))
               (local.set $arity-fx  (struct.get $Procedure $arity (local.get $p)))
               ;; Step 3: convert arity to string
               (local.set $arity-str (call $number->string (local.get $arity-fx) (global.get $false)))
               ;; Step 4: get mask and convert to string
               (local.set $mask      (call $procedure-arity-mask/checked/i32 (local.get $p)))
               (local.set $mask-str  (call $i32->string                      (local.get $mask)))
               ;; Step 5: build output
               (local.set $ga (call $make-growable-array (i32.const 5)))
               (call $growable-array-add! (local.get $ga) (call $str-hash-less-procedure-colon))
               (call $growable-array-add!
                     (local.get $ga)
                     (if (result (ref eq))
                         (ref.eq (local.get $name) (global.get $false))
                         (then (call $str-unknown))
                         (else (local.get $name))))
               (call $growable-array-add! (local.get $ga) (call $str-colon))
               (call $growable-array-add! (local.get $ga) (local.get $arity-str))
               (call $growable-array-add! (local.get $ga) (call $str-colon))
               (call $growable-array-add! (local.get $ga) (local.get $mask-str))
               (call $growable-array-add! (local.get $ga) (call $str->))
               ;; Step 5: convert to string
               (call $array-of-strings->string
                     (call $growable-array->array
                           (local.get $ga))))


         (func $format/display:primitive-procedure
               (param $v (ref eq))
               (result (ref $String))

               (local $p          (ref $PrimitiveProcedure))
               (local $name       (ref eq))         ;; $false or $String
               (local $arity-fx   (ref eq))         ;; fixnum (i31)
               (local $arity-str  (ref $String))
               (local $mask       i32)
               (local $mask-str   (ref $String))
               (local $ga         (ref $GrowableArray))
               
               ;; Step 1: type check and cast
               (if (i32.eqz (ref.test (ref $PrimitiveProcedure) (local.get $v)))
                   (then (call $raise-argument-error:primitive-procedure-expected)))
               (local.set $p (ref.cast (ref $PrimitiveProcedure) (local.get $v)))
               ;; Step 2: extract fields
               (local.set $name      (struct.get $PrimitiveProcedure $name  (local.get $p)))
               (local.set $arity-fx  (struct.get $PrimitiveProcedure $arity (local.get $p)))
               ;; Step 3: convert arity to string
               (local.set $arity-str (call $number->string (local.get $arity-fx) (global.get $false)))
               ;; Step 4: get mask and convert to string
               (local.set $mask      (call $procedure-arity-mask/checked/i32 (local.get $p)))
               (local.set $mask-str  (call $i32->string                      (local.get $mask)))
               ;; Step 5: build output
               (local.set $ga (call $make-growable-array (i32.const 5)))
               (call $growable-array-add! (local.get $ga) (call $str-hash-less-primitive-colon))
               (call $growable-array-add!
                     (local.get $ga)
                     (if (result (ref eq))
                         (ref.eq (local.get $name) (global.get $false))
                         (then (call $str-unknown))
                         (else (call $symbol->string (local.get $name)))))
               (call $growable-array-add! (local.get $ga) (call $str-colon))
               (call $growable-array-add! (local.get $ga) (local.get $arity-str))
               (call $growable-array-add! (local.get $ga) (call $str-colon))
               (call $growable-array-add! (local.get $ga) (local.get $mask-str))
               (call $growable-array-add! (local.get $ga) (call $str->))
               ;; Step 6: convert to string
               (call $array-of-strings->string
                     (call $growable-array->array (local.get $ga))))


         (func $format/display:symbol
               (param $v (ref eq))
               (result   (ref $String))

               (call $format/display
                     (call $symbol->string
                           (local.get $v))))
         
         (func $raise-format/display:pair:expected-pair       (unreachable))

         (func $format/display:pair (param $v (ref eq)) (result (ref $String))
               (local $out   (ref $GrowableArray))
               (local $stack (ref $GrowableArray))
               (local $car   (ref eq))
               (local $cdr   (ref $Pair))
               (local $tail  (ref eq))
               (local $str   (ref $String))

               ;; Initialize buffers
               (local.set $out   (call $make-growable-array (i32.const 8)))
               (local.set $stack (call $make-growable-array (i32.const 8)))
               (call $growable-array-add! (local.get $out)   (call $str-open-paren))
               (call $growable-array-add! (local.get $stack) (local.get $v))

               (block $done
                      (loop $walk
                            (br_if $done (i32.eqz (call $growable-array-count (local.get $stack))))
                            (local.set $tail (call $growable-array-remove-last! (local.get $stack)))

                            (if (ref.test (ref $Pair) (local.get $tail))
                                (then
                                 (local.set $cdr (ref.cast (ref $Pair) (local.get $tail)))
                                 (local.set $car (struct.get $Pair $a (local.get $cdr)))
                                 (local.set $tail (struct.get $Pair $d (local.get $cdr)))
                                 ;; Format car
                                 (local.set $str (call $format/display (local.get $car)))
                                 (call $growable-array-add! (local.get $out) (local.get $str))
                                 ;; Handle cdr
                                 (if (ref.test (ref $Pair) (local.get $tail))
                                     (then (call $growable-array-add! (local.get $out) (call $str-space))
                                           (call $growable-array-add! (local.get $stack) (local.get $tail)))
                                     (else (if (ref.eq (local.get $tail) (global.get $null))
                                               (then) ;; proper list: done
                                               (else  ;; improper list: emit ". cdr"
                                                (call $growable-array-add! (local.get $out) (call $str-space-dot-space))
                                                (local.set $str (call $format/display (local.get $tail)))
                                                (call $growable-array-add! (local.get $out) (local.get $str)))))))
                                (else
                                 (call $raise-format/display:pair:expected-pair)
                                 (unreachable)))
                            (br $walk)))
               (call $growable-array-add! (local.get $out) (call $str-close-paren))
               (call $growable-array-of-strings->string (local.get $out)))


         (func $raise-format/display:vector:expected-vector (unreachable))

         (func $format/display:vector (param $v (ref $Vector)) (result (ref $String))
               (local $out   (ref $GrowableArray))
               (local $arr   (ref $Array))
               (local $len   i32)
               (local $i     i32)
               (local $elem  (ref eq))
               (local $str   (ref $String))

               ;; Extract internal array and its length
               (local.set $arr (struct.get $Vector $arr (local.get $v)))
               (local.set $len (array.len (local.get $arr)))
               ;; Allocate result buffer
               (local.set $out (call $make-growable-array (i32.const 8)))
               ;; Emit "#("
               (call $growable-array-add! (local.get $out) (call $str-vector-prefix))
               ;; Add formatted elements with spaces
               (local.set $i (i32.const 0))
               (block $done
                      (loop $loop
                            (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
                            (local.set $elem (array.get $Array (local.get $arr) (local.get $i)))
                            (local.set $str (call $format/display (local.get $elem)))
                            (call $growable-array-add! (local.get $out) (local.get $str))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (if (i32.lt_u (local.get $i) (local.get $len))
                                (then (call $growable-array-add! (local.get $out) (call $str-space))))
                            (br $loop)))
               ;; Emit ")"
               (call $growable-array-add! (local.get $out) (call $str-close-paren))
               ;; Combine and return
               (call $growable-array-of-strings->string (local.get $out)))

         (func $format/display:box
               (param $b (ref $Box))
               (result   (ref $String))

               (local $val     (ref eq))
               (local $val-str (ref $String))
               (local $out     (ref $GrowableArray))

               ;; Get the boxed value
               (local.set $val (struct.get $Box $v (local.get $b)))
               ;; Format the value
               (local.set $val-str (call $format/display (local.get $val)))
               ;; Allocate a growable array
               (local.set $out (call $make-growable-array (i32.const 3)))
               ;; Append "#&"
               (call $growable-array-add! (local.get $out) (global.get $string:box-prefix)) ;; "#&"
               ;; Append formatted value
               (call $growable-array-add! (local.get $out) (local.get $val-str))
               ;; Combine and return
               (call $growable-array-of-strings->string (local.get $out)))

         (func $raise-format/display:vector:expected-values (unreachable))

         (func $format/display:values (param $arr (ref $Values)) (result (ref $String))
               (local $out   (ref $GrowableArray))
               (local $len   i32)
               (local $i     i32)
               (local $elem  (ref eq))
               (local $str   (ref $String))

               ;; Extract length
               (local.set $len (array.len (local.get $arr)))
               ;; Allocate result buffer
               (local.set $out (call $make-growable-array (i32.const 8)))
               ;; Emit "(values"
               (call $growable-array-add! (local.get $out) (call $str-values-prefix))
               (call $growable-array-add! (local.get $out) (call $str-space))
               ;; Add formatted elements with spaces
               (local.set $i (i32.const 0))
               (block $done
                      (loop $loop
                            (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
                            (local.set $elem (array.get $Array (local.get $arr) (local.get $i)))
                            (local.set $str (call $format/display (local.get $elem)))
                            (call $growable-array-add! (local.get $out) (local.get $str))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (if (i32.lt_u (local.get $i) (local.get $len))
                                (then (call $growable-array-add! (local.get $out) (call $str-space))))
                            (br $loop)))
               ;; Emit ")"
               (call $growable-array-add! (local.get $out) (call $str-close-paren))
               ;; Combine and return
               (call $growable-array-of-strings->string (local.get $out)))

         ; Note: I think this uses the write conventions instead of display.
         (func $format/display:char
               (param $v (ref eq))
               (result (ref $String))

               (local $cp i32)                 ;; codepoint
               (local $s  (ref $String))       ;; temporary string

               ;; Check if input is a character immediate
               (if (i32.or (i32.eqz (ref.test (ref i31) (local.get $v)))
                           (i32.ne (i32.and (i31.get_u (ref.cast (ref i31) (local.get $v)))
                                            (i32.const ,char-mask))
                                   (i32.const ,char-tag)))
                   (then (call $raise-check-char (local.get $v))))
               ;; Decode character
               (local.set $cp (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $v)))
                                         (i32.const ,char-shift)))
               ;; Special character names
               (if (i32.eq (local.get $cp) (i32.const 10))  ;; newline
                   (then (return
                          (call $string-append
                                (call $str-hash-backslash)
                                (call $str-word-newline)))))
               (if (i32.eq (local.get $cp) (i32.const 13))  ;; return
                   (then (return
                          (call $string-append
                                (call $str-hash-backslash)
                                (call $str-word-return)))))
               (if (i32.eq (local.get $cp) (i32.const 9))   ;; tab
                   (then (return
                          (call $string-append
                                (call $str-hash-backslash)
                                (call $str-word-tab)))))
               (if (i32.eq (local.get $cp) (i32.const 8))   ;; backspace
                   (then (return
                          (call $string-append
                                (call $str-hash-backslash)
                                (call $str-word-backspace)))))
               (if (i32.eq (local.get $cp) (i32.const 127)) ;; rubout
                   (then (return
                          (call $string-append
                                (call $str-hash-backslash)
                                (call $str-word-rubout)))))
               (if (i32.eq (local.get $cp) (i32.const 32))  ;; space
                   (then (return
                          (call $string-append
                                (call $str-hash-backslash)
                                (call $str-word-space)))))
               (if (i32.eq (local.get $cp) (i32.const 0))  ;; nul
                   (then (return
                          (call $string-append
                                (call $str-hash-backslash)
                                (call $str-word-nul)))))
               ;; Printable graphic character
               (if (call $is-graphic (local.get $cp))
                   (then
                    (local.set $s (call $make-string/checked (i32.const 1) (local.get $cp)))
                    (return (call $string-append (call $str-hash-backslash) (local.get $s)))))

               ;; Fallback for non-printable or out-of-range characters
               ;; Fallback: #\uXXXX and #\UXXXXXX
               (if (i32.le_u (local.get $cp) (i32.const 65535))  ;; ≤ 0xFFFF
                   (then (return
                          (call $string-append
                                (call $str-hash-backslash-u)
                                (call $make-hex-string (local.get $cp) (i32.const 4)))))
                   ;; Else use #\UXXXXXX
                   (else (return
                          (call $string-append
                                (call $str-hash-backslash-U)
                                (call $make-hex-string (local.get $cp) (i32.const 6))))))
               (unreachable))

         (func $make-hex-string
               (param $n      i32)    ;; input number
               (param $digits i32)    ;; minimum number of hex digits
               (result        (ref $String))

               (local $raw (ref $String))
               (local $len i32)
               (local $pad i32)
               ;; Step 1: Convert number to hex string (unpadded)
               (local.set $raw
                          (call $number->string:convert
                                (local.get $n)
                                (i32.const 16)       ;; radix = 16
                                (i32.const 8)))      ;; allow up to 8 chars for safety
               ;; Step 2: Compute padding = digits - string-length(raw)
               (local.set $len (array.len
                                (struct.get $String $codepoints (local.get $raw))))
               (local.set $pad (select
                                (i32.sub (local.get $digits) (local.get $len))
                                (i32.const 0)
                                (i32.gt_s (local.get $digits) (local.get $len))))
               ;; Step 3: If padding is zero, return raw
               (if (i32.eqz (local.get $pad))
                   (then (return (local.get $raw))))
               ;; Step 4: Make padding string of '0's and append
               (return
                (call $string-append
                      (call $make-string/checked (local.get $pad) (i32.const 48)) ;; 48 = '0'
                      (local.get $raw))))

               ;;;
               ;;; Formatting 
               ;;;
                              
               (func $str-number (param $n (ref eq)) (result (ref $Bytes))
                     ;; Converts a fixnum (with LSB = 0) to a UTF-8 encoded byte string.

                     (local $i31 (ref i31))
                     (local $v   i32)
                     (local $abs i32)
                     (local $neg i32)
                     (local $tmp i32)
                     (local $i   i32)
                     (local $j   i32)
                     (local $len i32)
                     (local $buf (ref $I8Array))
                     (local $out (ref $I8Array))
                     ;; 1. Cast and unbox fixnum (assume it has LSB=0)
                     (local.set $i31 (ref.cast (ref i31) (local.get $n)))
                     (local.set $v (i32.shr_s (i31.get_s (local.get $i31)) (i32.const 1))) ;; shift to remove tag bit
                     ;; 2. Compute abs and sign
                     (local.set $neg (i32.lt_s (local.get $v) (i32.const 0)))
                     (local.set $abs (select
                                      (i32.sub (i32.const 0) (local.get $v))
                                      (local.get $v)
                                      (local.get $neg)))
                     ;; 3. Special case: 0
                     (if (i32.eqz (local.get $abs))
                         (then
                          (return
                           (struct.new $Bytes
                                       (i32.const 0)
                                       (i32.const 1)
                                       (array.new $I8Array (i32.const 48) (i32.const 1)))))) ;; ASCII '0'
                     ;; 4. Allocate temporary buffer (max 11 digits)
                     (local.set $buf (array.new_default $I8Array (i32.const 11)))
                     (local.set $i (i32.const 11))
                     ;; 5. Extract digits (in reverse)
                     (block $done
                            (loop $loop
                                  (br_if $done (i32.eqz (local.get $abs)))
                                  (local.set $i (i32.sub (local.get $i) (i32.const 1)))
                                  (local.set $tmp (i32.rem_u (local.get $abs) (i32.const 10)))
                                  (array.set $I8Array (local.get $buf) (local.get $i)
                                             (i32.add (local.get $tmp) (i32.const 48))) ;; ASCII '0' + digit
                                  (local.set $abs (i32.div_u (local.get $abs) (i32.const 10)))
                                  (br $loop)))
                     ;; 6. Add minus sign if needed
                     (if (local.get $neg)
                         (then
                          (local.set $i (i32.sub (local.get $i) (i32.const 1)))
                          (array.set $I8Array (local.get $buf) (local.get $i) (i32.const 45)))) ;; ASCII '-'
                     ;; 7. Allocate output and copy from temp
                     (local.set $len (i32.sub (i32.const 11) (local.get $i)))
                     (local.set $out (array.new_default $I8Array (local.get $len)))
                     (local.set $j (i32.const 0))
                     (block $done
                            (loop $copy
                                  (br_if $done (i32.ge_u (local.get $j) (local.get $len)))
                                  (array.set $I8Array (local.get $out) (local.get $j)
                                             (array.get_u $I8Array (local.get $buf)
                                                          (i32.add (local.get $i) (local.get $j))))
                                  (local.set $j (i32.add (local.get $j) (i32.const 1)))
                                  (br $copy)))
                     ;; 8. Wrap in $Bytes struct
                     (struct.new $Bytes
                                 (i32.const 0)          ;; hash
                                 (i32.const 1)          ;; immutable
                                 (local.get $out)))

               
               ;; ----------------------------------------------------------------------------
               ;; Racket-style (str-bytes v opt-mode) in Wasm GC
               ;;
               ;; This module implements a function `$str-bytes` that converts a string `v`
               ;; into a byte string representation based on a given mode, following Racket's
               ;; conventions for `write`, `print`, and `display`.
               ;;
               ;; Assumed existing definitions:
               ;;   (type $Bytes  (array (mut i8)))
               ;;   (type $String (array (mut i32))) ;; An array of Unicode code points
               ;;   (type (sub (ref i31)) (ref eq))  ;; For fixnums
               ;; ----------------------------------------------------------------------------

               ;;;; ------------------------------------------------------------------------
               ;;;; Helper Functions
               ;;;; ------------------------------------------------------------------------

               ;;; (func $is-blank char) -> bool
               ;;; A simplified check for blank characters: space (32) and tab (9).
               (func $is-blank (param $char i32) (result i32)
                     (i32.or
                      (i32.eq (local.get $char) (i32.const 32))   ;; #\space
                      (i32.eq (local.get $char) (i32.const 9))))  ;; #\tab

               ;;; (func $is-graphic char) -> bool
               ;;; A simplified check for graphic characters. This is an approximation
               ;;; of Racket's `char-graphic?`. It treats characters as graphic if they
               ;;; are not C0 control characters (U+00-U+1F), not DEL (U+7F).
               (func $is-graphic (param $char i32) (result i32)
                     (i32.and
                      (i32.ge_u (local.get $char) (i32.const 32))
                      (i32.ne   (local.get $char) (i32.const 127))))

               ;;; (func $get-special-escape char) -> escape_char_or_0
               ;;; Checks if a character has a simple one-letter escape sequence.
               ;;; Returns the escape character's ASCII value (e.g., 'a' for BEL) or 0.
               (func $get-special-escape (param $char i32) (result i32)
                     (block $result (result i32)
                            (if (i32.eq (local.get $char) (i32.const  7)) (then (return (i32.const  97)))) ;; \a BEL
                            (if (i32.eq (local.get $char) (i32.const  8)) (then (return (i32.const  98)))) ;; \b BS
                            (if (i32.eq (local.get $char) (i32.const  9)) (then (return (i32.const 116)))) ;; \t TAB
                            (if (i32.eq (local.get $char) (i32.const 10)) (then (return (i32.const 110)))) ;; \n LF
                            (if (i32.eq (local.get $char) (i32.const 11)) (then (return (i32.const 118)))) ;; \v VT
                            (if (i32.eq (local.get $char) (i32.const 12)) (then (return (i32.const 102)))) ;; \f FF
                            (if (i32.eq (local.get $char) (i32.const 13)) (then (return (i32.const 114)))) ;; \r CR
                            (if (i32.eq (local.get $char) (i32.const 27)) (then (return (i32.const 101)))) ;; \e ESC
                            (i32.const 0))) ;; No special escape

               ;;; (func $utf8-size char) -> byte_count
               ;;; Calculates the number of bytes required to represent a Unicode
               ;;; code point in UTF-8.
               (func $utf8-size (param $char i32) (result i32)
                     (if (result i32) (i32.le_u (local.get $char) (i32.const 0x7f))
                         (then (i32.const 1))
                         (else
                          (if (result i32) (i32.le_u (local.get $char) (i32.const 0x7ff))
                              (then (i32.const 2))
                              (else
                               (if (result i32) (i32.le_u (local.get $char) (i32.const 0xffff))
                                   (then (i32.const 3))
                                   (else (i32.const 4))))))))

               ;;; (func $write-utf8 buf idx char) -> new_idx
               ;;; Encodes a Unicode code point as UTF-8 and writes it into the buffer.
               ;;; Returns the updated buffer index.
               (func $write-utf8
                     (param $buf  (ref $Bytes))
                     (param $idx  i32)
                     (param $char i32)
                     (result      i32)

                     (local $arr (ref $I8Array))
                     ;; Extract raw array from $Bytes struct
                     (local.set $arr (struct.get $Bytes 2 (local.get $buf))) ;; field 2 = $bs
                     ;; Encode UTF-8 based on code point range
                     (if (i32.le_u (local.get $char) (i32.const 0x7f))
                         (then (array.set $I8Array (local.get $arr) (local.get $idx) (local.get $char))
                               (return (i32.add (local.get $idx) (i32.const 1)))))

                     (if (i32.le_u (local.get $char) (i32.const 0x7ff))
                         (then
                          (array.set $I8Array (local.get $arr) (local.get $idx)
                                     (i32.or (i32.shr_u (local.get $char) (i32.const 6)) (i32.const 0xc0)))
                          (array.set $I8Array (local.get $arr)
                                     (i32.add (local.get $idx) (i32.const 1))
                                     (i32.or (i32.and (local.get $char) (i32.const 0x3f)) (i32.const 0x80)))
                          (return (i32.add (local.get $idx) (i32.const 2)))))

                     (if (i32.le_u (local.get $char) (i32.const 0xffff))
                         (then
                          (array.set $I8Array (local.get $arr) (local.get $idx)
                                     (i32.or (i32.shr_u (local.get $char) (i32.const 12)) (i32.const 0xe0)))
                          (array.set $I8Array (local.get $arr)
                                     (i32.add (local.get $idx) (i32.const 1))
                                     (i32.or (i32.and (i32.shr_u (local.get $char) (i32.const 6)) (i32.const 0x3f)) (i32.const 0x80)))
                          (array.set $I8Array (local.get $arr)
                                     (i32.add (local.get $idx) (i32.const 2))
                                     (i32.or (i32.and (local.get $char) (i32.const 0x3f)) (i32.const 0x80)))
                          (return (i32.add (local.get $idx) (i32.const 3)))))

                     (if (i32.le_u (local.get $char) (i32.const 0x10ffff))
                         (then
                          (array.set $I8Array (local.get $arr) (local.get $idx)
                                     (i32.or (i32.shr_u (local.get $char) (i32.const 18)) (i32.const 0xf0)))
                          (array.set $I8Array (local.get $arr)
                                     (i32.add (local.get $idx) (i32.const 1))
                                     (i32.or (i32.and (i32.shr_u (local.get $char) (i32.const 12)) (i32.const 0x3f)) (i32.const 0x80)))
                          (array.set $I8Array (local.get $arr)
                                     (i32.add (local.get $idx) (i32.const 2))
                                     (i32.or (i32.and (i32.shr_u (local.get $char) (i32.const 6)) (i32.const 0x3f)) (i32.const 0x80)))
                          (array.set $I8Array (local.get $arr)
                                     (i32.add (local.get $idx) (i32.const 3))
                                     (i32.or (i32.and (local.get $char) (i32.const 0x3f)) (i32.const 0x80)))
                          (return (i32.add (local.get $idx) (i32.const 4)))))
                     ;; Invalid input — return unchanged index
                     (local.get $idx)) ;; Should not happen for valid Unicode
               

               ;;; (func $write-hex buf idx val digits)
               ;;; Writes the lower `digits` of `val` as hexadecimal characters into the buffer.
               (func $write-hex
                     (param $buf (ref $Bytes))
                     (param $idx i32)
                     (param $val i32)
                     (param $digits i32)

                     (local $i i32)
                     (local $shift i32)
                     (local $nibble i32)
                     (local $char i32)
                     (local $write-pos i32)
                     (local $arr (ref $I8Array))

                     ;; Extract the raw array from the $Bytes struct
                     (local.set $arr (struct.get $Bytes 2 (local.get $buf)))

                     (local.set $i (i32.sub (local.get $digits) (i32.const 1)))
                     (loop $hex-loop
                           (if (i32.ge_s (local.get $i) (i32.const 0))
                               (then
                                ;; Compute nibble from val
                                (local.set $shift (i32.shl (local.get $i) (i32.const 2))) ;; same as *4
                                (local.set $nibble
                                           (i32.and
                                            (i32.shr_u (local.get $val) (local.get $shift))
                                            (i32.const 0xF)))
                                ;; Convert nibble to ASCII
                                (local.set $char
                                           (if (result i32) (i32.lt_s (local.get $nibble) (i32.const 10))
                                               (then (i32.add (local.get $nibble) (i32.const 48)))    ;; '0'..'9'
                                               (else (i32.add (local.get $nibble) (i32.const 87)))))  ;; 'a'..'f'
                                ;; Compute write index and store
                                (local.set $write-pos
                                           (i32.add
                                            (local.get $idx)
                                            (i32.sub (i32.sub (local.get $digits) (local.get $i)) (i32.const 1))))
                                (array.set $I8Array (local.get $arr) (local.get $write-pos) (local.get $char))
                                ;; Continue loop
                                (local.set $i (i32.sub (local.get $i) (i32.const 1)))
                                (br $hex-loop)))))


               ;;;; ------------------------------------------------------------------------
               ;;;; Mode-Specific Implementations
               ;;;; ------------------------------------------------------------------------

               ;;; Converts a string to its 'display' representation (raw UTF-8 bytes).
               (func $string->bytes:display (param $str (ref $String)) (result (ref $Bytes))
                     (local $out-buf (ref $Bytes))
                     (local $arr     (ref $I8Array))
                     (local $str-len i32)
                     (local $i       i32)
                     (local $char    i32)
                     (local $total-size i32)
                     (local $idx     i32)

                     ;; Pass 1: calculate required UTF-8 buffer size
                     (local.set $str-len (struct.get $String 1 (local.get $str))) ;; string length
                     (local.set $i (i32.const 0))
                     (local.set $total-size (i32.const 0))
                     (loop $size-loop
                           (if (i32.lt_u (local.get $i) (local.get $str-len))
                               (then
                                (local.set $char
                                           (array.get $I32Array
                                                      (struct.get $String 2 (local.get $str))
                                                      (local.get $i)))
                                (local.set $total-size
                                           (i32.add (local.get $total-size)
                                                    (call $utf8-size (local.get $char))))
                                (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                (br $size-loop))))

                     ;; Allocate backing byte array and wrap in Bytes
                     (local.set $arr (array.new_default $I8Array (local.get $total-size)))
                     (local.set $out-buf
                                (struct.new $Bytes
                                            (i32.const 0)        ;; hash = 0
                                            (i32.const 1)        ;; immutable = true
                                            (local.get $arr)))   ;; backing array

                     ;; Pass 2: encode UTF-8 into buffer
                     (local.set $i   (i32.const 0))
                     (local.set $idx (i32.const 0))
                     (loop $fill-loop
                           (if (i32.lt_u (local.get $i) (local.get $str-len))
                               (then
                                (local.set $char
                                           (array.get $I32Array
                                                      (struct.get $String 2 (local.get $str))
                                                      (local.get $i)))
                                (local.set $idx
                                           (call $write-utf8
                                                 (local.get $out-buf)
                                                 (local.get $idx)
                                                 (local.get $char)))
                                (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                (br $fill-loop))))

                     (local.get $out-buf))

               
               ;;; Converts a string to its 'write'/'print' representation (quoted, escaped).
               #;(func $string->bytes:write (param $str (ref $String)) (result (ref $Bytes))
                       (local $out-buf    (ref $Bytes))
                       (local $arr        (ref $I8Array))
                       (local $str-len    i32)
                       (local $i          i32)
                       (local $char       i32)
                       (local $escape     i32)
                       (local $total-size i32)
                       (local $idx        i32)

                       ;; Pass 1: calculate output size
                       (local.set $str-len    (struct.get $String 1 (local.get $str))) ; todo: wrong
                       (local.set $i          (i32.const 0))
                       (local.set $total-size (i32.const 2)) ;; opening and closing quote
                       (loop $size-loop
                             (if (i32.lt_u (local.get $i) (local.get $str-len))
                                 (then
                                  (local.set $char
                                             (array.get $I32Array (struct.get $String 2 (local.get $str)) (local.get $i)))
                                  (block $char-size-done
                                         (if (i32.eq (local.get $char) (i32.const 34))
                                             (then
                                              (local.set $total-size (i32.add (local.get $total-size) (i32.const 2)))
                                              (br $char-size-done)))
                                         (if (i32.eq (local.get $char) (i32.const 92))
                                             (then
                                              (local.set $total-size (i32.add (local.get $total-size) (i32.const 2)))
                                              (br $char-size-done)))
                                         (if (call $get-special-escape (local.get $char))
                                             (then
                                              (local.set $total-size (i32.add (local.get $total-size) (i32.const 2)))
                                              (br $char-size-done)))
                                         (if (i32.or (call $is-graphic (local.get $char)) (call $is-blank (local.get $char)))
                                             (then
                                              (local.set $total-size
                                                         (i32.add (local.get $total-size) (call $utf8-size (local.get $char))))
                                              (br $char-size-done)))
                                         ;; Otherwise use hex escape
                                         (if (i32.le_u (local.get $char) (i32.const 0xffff))
                                             (then (local.set $total-size (i32.add (local.get $total-size) (i32.const 6))))
                                             (else (local.set $total-size (i32.add (local.get $total-size) (i32.const 10))))))
                                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                  (br $size-loop))))

                       ;; Allocate output array and wrap in Bytes
                       (local.set $arr (array.new_default $I8Array (local.get $total-size)))
                       (local.set $out-buf
                                  (struct.new $Bytes
                                              (i32.const 0)          ;; hash
                                              (i32.const 1)          ;; immutable
                                              (local.get $arr)))     ;; backing array

                       ;; Write opening quote
                       (array.set $I8Array (local.get $arr) (i32.const 0) (i32.const 34))
                       (local.set $idx (i32.const 1))
                       (local.set $i   (i32.const 0))

                       ;; Fill output array
                       (loop $fill-loop
                             (if (i32.lt_u (local.get $i) (local.get $str-len))
                                 (then
                                  (local.set $char
                                             (array.get $I32Array (struct.get $String 2 (local.get $str)) (local.get $i)))
                                  (block $char-done
                                         (if (i32.eq (local.get $char) (i32.const 34))
                                             (then
                                              (array.set $I8Array (local.get $arr) (local.get $idx) (i32.const 92))
                                              (array.set $I8Array (local.get $arr) (i32.add (local.get $idx) (i32.const 1)) (i32.const 34))
                                              (local.set $idx (i32.add (local.get $idx) (i32.const 2)))
                                              (br $char-done)))
                                         (if (i32.eq (local.get $char) (i32.const 92))
                                             (then
                                              (array.set $I8Array (local.get $arr) (local.get $idx) (i32.const 92))
                                              (array.set $I8Array (local.get $arr) (i32.add (local.get $idx) (i32.const 1)) (i32.const 92))
                                              (local.set $idx (i32.add (local.get $idx) (i32.const 2)))
                                              (br $char-done)))
                                         (local.set $escape (call $get-special-escape (local.get $char)))
                                         (if (local.get $escape)
                                             (then
                                              (array.set $I8Array (local.get $arr) (local.get $idx) (i32.const 92))
                                              (array.set $I8Array (local.get $arr) (i32.add (local.get $idx) (i32.const 1)) (local.get $escape))
                                              (local.set $idx (i32.add (local.get $idx) (i32.const 2)))
                                              (br $char-done)))
                                         (if (i32.or (call $is-graphic (local.get $char)) (call $is-blank (local.get $char)))
                                             (then
                                              (local.set $idx (call $write-utf8 (local.get $out-buf) (local.get $idx) (local.get $char)))
                                              (br $char-done)))
                                         (if (i32.le_u (local.get $char) (i32.const 0xffff))
                                             (then
                                              (array.set $I8Array (local.get $arr) (local.get $idx) (i32.const 92))
                                              (array.set $I8Array (local.get $arr) (i32.add (local.get $idx) (i32.const 1)) (i32.const 117)) ;; 'u'
                                              (call $write-hex (local.get $out-buf) (i32.add (local.get $idx) (i32.const 2)) (local.get $char) (i32.const 4))
                                              (local.set $idx (i32.add (local.get $idx) (i32.const 6))))
                                             (else
                                              (array.set $I8Array (local.get $arr) (local.get $idx) (i32.const 92))
                                              (array.set $I8Array (local.get $arr) (i32.add (local.get $idx) (i32.const 1)) (i32.const 85)) ;; 'U'
                                              (call $write-hex (local.get $out-buf) (i32.add (local.get $idx) (i32.const 2)) (local.get $char) (i32.const 8))
                                              (local.set $idx (i32.add (local.get $idx) (i32.const 10))))))
                                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                  (br $fill-loop)))

                             ;; Final quote
                             (array.set $I8Array (local.get $arr) (local.get $idx) (i32.const 34))
                             (local.get $out-buf)))


               (func $raise-not-a-string (param $val (ref eq)) (unreachable))

               #;(func $string->bytes (export "string->bytes")
                       ; todo: check mode
                       (param $v        (ref eq))
                       (param $mode-raw (ref eq))
                       (result (ref eq))

                       (local $mode     (ref i31))
                       (local $int-mode i32)
                       (local $str      (ref null $String))

                       ;; 1. Extract integer mode from fixnum
                       (local.set $mode (ref.cast (ref i31) (local.get $mode-raw)))
                       (local.set $int-mode (i31.get_u (local.get $mode)))

                       ;; 2. Check if $v is a string
                       (if (ref.test (ref $String) (local.get $v))
                           (then
                            (local.set $str (ref.cast (ref $String) (local.get $v))))
                           (else
                            ;; Raise an exception: not a string
                            (return (call $raise-not-a-string (local.get $v)))))

                       ;; 3. Dispatch by mode: 1 = display, else write
                       (if (result (ref eq))
                           (i32.eq (local.get $int-mode) (i32.const 1))
                           (then (call $string->bytes:display (local.get $str)))
                           (else (call $string->bytes:write   (local.get $str)))))


               #;(func $str-list
                       (param $v        (ref eq))
                       (param $opt-mode (ref eq))
                       
                       (result (ref $Bytes))

                       (local $result (ref $SegmentVec))
                       (local $mode   i32)
                       (local $bs     (ref $Bytes))
                       ;; Determine mode
                       (local.set $mode
                                  (select
                                   ,(Half `(i31.get_s (ref.cast (ref i31) (local.get $opt-mode))))
                                   (i32.const 0)
                                   (ref.eq (local.get $opt-mode) (global.get $false))))
                       ;; Initialize accumulator with "("
                       (local.set $result
                                  (array.new_fixed $SegmentVec
                                                   (array.new_data $Bytes "(" (i32.const 1))))

                       ;; Traverse list and accumulate
                       (block $done
                              (loop $loop
                                    (br_if $done (ref.eq (local.get $v) (global.get $null)))

                                    ;; car
                                    (local.set $bs     (call $str (call $car (local.get $v))
                                                             (ref.i31 (local.get $mode))))
                                    (local.set $result (call $str-segment-vec-append
                                                             (local.get $result)
                                                             (local.get $bs)))
                                    ;; cdr
                                    (local.set $v (call $cdr (local.get $v)))

                                    (br_if $loop (ref.eq (local.get $v) (global.get $null)))

                                    ;; space between elements
                                    (local.set $result (call $str-segment-vec-append (local.get $result)
                                                             (array.new_data $Bytes " " (i32.const 1))))
                                    (br $loop)))

                       ;; Add closing ")"
                       (local.set $result (call $str-segment-vec-append (local.get $result)
                                                (array.new_data $Bytes ")" (i32.const 1))))

                       ;; Join all segments with no delimiter
                       (call $str-join (local.get $result)))

               ;;;
               ;;; Support for the for-family: for, for/list, for/vector
               ;;;
               
               #;(func $grow-vector
                     (param $vec (ref $Vector))
                     (result     (ref $Vector))

                     (local $arr     (ref $Array))
                     (local $len     i32)
                     (local $new-len i32)
                     (local $new-vec (ref $Vector))
                     (local $new-arr (ref $Array))
                     ;; Extract underlying array and its length
                     (local.set $arr     (struct.get $Vector $arr (local.get $vec)))
                     (local.set $len     (array.len (local.get $arr)))
                     (local.set $new-len (i32.mul (local.get $len) (i32.const 2)))
                     ;; Allocate new array and vector wrapper
                     (local.set $new-arr (array.new_default $Array (local.get $new-len) (global.get $null)))
                     (local.set $new-vec (struct.new $Vector (i32.const 0) (local.get $new-arr)))
                     ;; Copy elements
                     (call $vector-copy!
                           (local.get $new-vec)
                           (ref.i31 (i32.const 0))
                           (local.get $vec)
                           (ref.i31 (i32.const 0))
                           (ref.i31 (local.get $len)))
                     (return (local.get $new-vec)))

               
               #;(func $shrink-vector
                     (param $vec (ref $Vector))
                     (param $i   (ref eq))   ;; fixnum
                     (result (ref $Vector))

                     (local $i/u i32)
                     (local $new-vec (ref $Vector))
                     ;; Validate fixnum index
                     (if (i32.eqz (ref.test (ref i31) (local.get $i)))
                         (then (call $raise-check-fixnum (local.get $i)) (unreachable)))
                     (local.set $i/u
                                (i32.shr_u
                                 (i31.get_u (ref.cast (ref i31) (local.get $i)))
                                 (i32.const 1)))
                     ;; Allocate new vector and copy
                     (local.set $new-vec (struct.new $Vector
                                                     (i32.const 0)
                                                     (array.new_default $Array (local.get $i/u) (global.get $null))))
                     (call $vector-copy!
                           (local.get $new-vec)
                           (ref.i31 (i32.const 0))
                           (local.get $vec)
                           (ref.i31 (i32.const 0))
                           (ref.i31 (i32.shl (local.get $i/u) (i32.const 1)))) ;; convert i32 to fixnum
                     (return (local.get $new-vec)))

               
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

               (global $result-bytes (mut (ref eq)) (ref.i31 (i32.const 0)))
               
               (func $entry (export "entry") (result i32)
                     ; Declare local variables (bound by let-values and letrec-values)
                     ,@(let ()
                         (define (Local  x)
                           (match x
                             [(list v t)      `(local ,(if (symbol? v) v (Var v)) ,t)]
                             [(list v t init) `(local ,(if (symbol? v) v (Var v)) ,t)]))
                         (define (Local* xs) (map Local xs))
                         (Local* (reverse (*locals*))))

                     ;; Initialize global constants
                     (global.set $flzero  (call $i32->flonum (i32.const 0)))
                     (global.set $flone   (call $i32->flonum (i32.const 1)))
                     (global.set $fltwo   (call $i32->flonum (i32.const 2)))
                     (global.set $flthree (call $i32->flonum (i32.const 3)))

                     ;; Initialize global state
                     (call $initialize-the-symbol-table)  ; for interning
                     (call $initialize-the-keyword-table) ; for interning

                     ;; Initialize string constants used in the runtime
                     ,@(initialize-runtime-string-constants)

                     ;; Initialize symbol constants used in the runtime
                     ,@(initialize-runtime-symbol-constants)
                     
                     ;; Initialize realm symbols
                     (global.set $the-racket-realm
                                 (call $string->symbol (call $str-racket)))
                     (global.set $the-racket/primitive-realm
                                 (call $string->symbol (call $str-racket/primitive)))

                     ;; Initialize variables holding primitives
                     ,@(initialize-primitives-as-globals)
                     
                     ;; Initialize top-level variables.
                     ;; These are all "boxed".
                     ,@(let ()
                         (for/list ([v top-vars])
                           `(global.set ,(TopVar v) (struct.new $Boxed (global.get ,(TopVar v))))))
                     
                     
                     ; Initialize local variables
                     ,@(let ()
                         (define (Init  x)
                           (match x
                             [(list v t)
                              (if (equal? t '(ref eq))
                                  `(local.set ,(if (symbol? v) v (Var v)) ,(Undefined))
                                  `(nop))]
                             [(list v t init)
                              `(local.set ,(if (symbol? v) v (Var v)) ,init)]))
                         (define (Init* xs) (map Init xs))
                         (Init* (*locals*)))

                     
                     ; Body
                     ,entry-body
                     
                     ; Return the result
                     (global.set $result-bytes
                                 (call $string->bytes/utf-8
                                       (call $format/display (global.get ,result))
                                       (global.get $false)   ; ignored
                                       (global.get $zero)    ; start = 0
                                       (global.get $false))) ; end                        
                     (drop (call $copy_bytes_to_memory (i32.const 0)))
                     (call $fixnum->i32 (call $bytes-length (global.get $result-bytes))))

               (func $get-bytes (export "get_bytes")
                     (result (ref $Bytes))
                     (ref.cast (ref $Bytes) (global.get $result-bytes)))

               
               ))))

;; > (pretty-print (language->s-expression LANF+closure))
;; '(define-language LANF+closure
;;   (entry TopLevelForm)
;;   (terminals
;;     (natural (i))        ; index
;;     (variable (x xd l))  ; variable and label
;;     (arity (ar))         
;;     (closure+primitive (pr))
;;     (syntax s)
;;     (modpath (mp))
;;     (modname (mn))
;;     (datum (d)))
;;   (Formals (f)
;;     (formals (x ...))
;;     (formals (x0 x1 ... . xd))
;;     (formals x))
;;   (TopLevelForm (t)
;;     (define-label l cab)
;;     (topbegin s t ...)
;;     (topmodule s mn mp mf ...)
;;     (#%expression s e)
;;     g)
;;   (ModuleLevelForm (mf)
;;     (define-label l cab)
;;     g)
;;   (GeneralTopLevelForm (g)
;;     e
;;     (define-values s (x ...) e)
;;     (define-syntaxes s (x ...) e)
;;     (#%require s rrs ...))
;;   (RawRequireSpec (rrs)
;;      rrmp)
;;   (RawRootModulePath (rrmp)
;;     'x)
;;   (Expr
;;     (e)
;;     ce
;;     (let-values s (((x ...) ce) ...) e)
;;     (letrec-values s (((x ...) ce) ...) e))
;;   (CExpr (ce)
;;     (closedapp s ca ae1 ...)
;;     ae
;;     (if s ae0 e1 e2)
;;     (set! s x ae)
;;     (wcm s ae0 ae1 e)
;;     (app s ae ae1 ...)
;;     (primapp s pr ae1 ...)
;;     (begin s e0 e1 ...)
;;     (begin0 s e0 e1 ...))
;;   (AExpr (ae)
;;     (free-ref x i)
;;     cca
;;     ca
;;     x
;;     (quote s d)
;;     (quote-syntax s d)
;;     (top s x))
;;   (CaseClosureAllocation (cca)
;;     (case-closure s l (ar ca) ...))
;;   (ClosureAllocation (ca)
;;     (closure s l ae1 ...))
;;   (ConvertedAbstraction (cab)
;;     (λ s f e)))

(pretty-print-columns 100)

(define (wat-pretty-print-table)
  (pretty-print-extend-style-table
   #f
   '(func   module block)
   '(lambda when   begin)))

(define (wat-pretty-write x)
  (parameterize ([current-print write]
                 [pretty-print-current-style-table (wat-pretty-print-table)])
    (pretty-write x)))

(define (print-wat x)
  (wat-pretty-write x))


(define (strip x)
  (cond
    [(variable? x) (variable-id x)]
    [(pair? x)     (cons (strip (car x))
                         (strip (cdr x)))]
    [(null? x)     '()]
    [(syntax? x)   (syntax-e x)]
    [(datum? x)    (datum-value x)]
    
    [else          x]))

(define (run-expr stx)
  (define out 
    (with-output-to-string
      (λ ()
        (run (comp stx)))))
  ; (write out) (newline)
  (cond
    [(equal? out "#<void>\n\n") (void)]
    [else
     (with-handlers ([exn? (λ (e)
                             ; (displayln out) 
                             "<read-error>")])
       (with-input-from-string out
         (λ ()
           (read))))]))
  
(define (comp+ stx)
  (run (comp stx)))

(define (comp stx)
  (reset-counter!)
  (strip
    (generate-code
     (flatten-begin
      (closure-conversion
       (anormalize
        (categorize-applications
         (assignment-conversion
          (α-rename
           (explicit-case-lambda
            (explicit-begin
             (convert-quotations
              (parse
               (topexpand stx))))))))))))))

(define (comp- stx)
  (reset-counter!)
  (pretty-print
   (strip    
    (flatten-begin
     (closure-conversion
      (anormalize
       (categorize-applications
        (assignment-conversion
         (α-rename
          (explicit-case-lambda
           (explicit-begin
            (convert-quotations
             (parse
              (topexpand stx))))))))))))))

(define (comp-- stx)
  (reset-counter!)
  (pretty-print
   (strip    
      (anormalize
       (categorize-applications
        (assignment-conversion
         (α-rename
          (explicit-case-lambda
           (explicit-begin
            (convert-quotations
             (parse
              (topexpand stx))))))))))))

(define (comp--- stx)
  (reset-counter!)
  (pretty-print
   (strip    
    (categorize-applications
     (assignment-conversion
      (α-rename
       (explicit-case-lambda
        (explicit-begin
         (convert-quotations
          (parse
           (topexpand stx)))))))))))

(define (test stx)
  (reset-counter!)
  (classify-variables
   (flatten-begin
    (closure-conversion
     (anormalize
      (categorize-applications
       (assignment-conversion
        (α-rename
         (explicit-case-lambda
          (explicit-begin
           (convert-quotations
            (parse
             (topexpand stx)))))))))))))



;;;
;;; Tests
;;;

;; Use (test-suite) to run the test suite.
;;
;; The output looks like this:
;;
;;  '(("Immediate Values"     #t)
;;    ("Call unary primitive" #t)
;;    ("Some characters "     #t)
;;    ("All characters"       #f))
;;
;; We see that the test "All characters" failed.
;; To see precisely what expression failed, find the test below  and change `and` to `list`.
;; After fixing the problem, change `list` back to `and`.

(define (test-suite)
  (define (run x)
    (displayln x)
    (run-expr x))
  (define (test-immediates)
    (define immediates
      (list 42 -42 0 most-negative-fixnum most-positive-fixnum #t #f '() #\a))
    (equal? (map run immediates) immediates))
  (define (test-some-characters)
    (define cs '(#\a #\A #\z #\Z #\newline #\space))
    (equal? (map run cs) cs))
  (define (test-all-characters) ; *very* slow, passes
    (for/list ([i 256])
      (define c (integer->char i))
      (equal? (run c) c)))
  (define (test-call-unary-primitive)
    ; (define equal? list)
    (and  #;(equal? (run '(add1 42)) 43)
          #;(equal? (run '(sub1 42)) 41)
          (equal? (run `(integer->char ,(char->integer #\a))) #\a)
          (equal? (run `(char->integer ,(integer->char 32)))  32)
          (equal? (run `(zero? 0))       #t)
          (equal? (run `(zero? 1))       #f)
          (equal? (run `(boolean? #t))   #t)
          (equal? (run `(boolean? #f))   #t)
          (equal? (run `(boolean? 42))   #f)
          (equal? (run `(integer? 42))   #t)
          (equal? (run `(integer? #t))   #f)
          (equal? (run `(void? ,(void))) #t)
          (equal? (run `(void? 1))       #f)         
          (equal? (run `(null? ()))      #t)
          (equal? (run `(null? 42))      #f)
          (equal? (run `(char? #\a))     #t)
          (equal? (run `(char? 42))      #f)
          (equal? (run `(not #f))        #t)
          (equal? (run `(not #t))        #f)
          (equal? (run `(not 0))         #f)))
  (define (test-call-binary-primitive)
    (and  (equal? (run '(+  1 2))  3)
          (equal? (run '(- 42 1)) 41)
          (equal? (run '(*  2 3))  6)
          (equal? (run '(=  2 3)) #f)
          (equal? (run '(=  2 2)) #t)
          (equal? (run '(<  2 3)) #t)
          (equal? (run '(<  4 3)) #f)
          (equal? (run '(>  2 3)) #f)
          (equal? (run '(>  4 3)) #t)
          (equal? (run '(<= 2 3)) #t)
          (equal? (run '(<= 4 3)) #f)
          (equal? (run '(<= 2 2)) #t)
          (equal? (run '(>= 2 3)) #f)
          (equal? (run '(>= 4 3)) #t)
          (equal? (run '(>= 3 3)) #t)
          (equal? (run '(char=? #\a #\a)) #t)
          (equal? (run '(char=? #\a #\b)) #f)
          ;; TODO: implement these
          ;; (equal? (run '(char<? #\a #\b)) #t)
          ;; (equal? (run '(char<? #\b #\a)) #f)
          ;; (equal? (run '(char<? #\a #\a)) #f)
          ;; (equal? (run '(char>? #\a #\b)) #f)
          ;; (equal? (run '(char>? #\b #\a)) #t)
          ;; (equal? (run '(char>? #\a #\a)) #f)
          ;; (equal? (run '(char<=? #\a #\b)) #t)
          ;; (equal? (run '(char<=? #\b #\a)) #f)
          ;; (equal? (run '(char<=? #\a #\a)) #t)
          ;; (equal? (run '(char>=? #\a #\b)) #f)
          ;; (equal? (run '(char>=? #\b #\a)) #t)
          ;; (equal? (run '(char>=? #\a #\a)) #t)
    ))
  (define (test-let)
    ; (define equal? list)
    (and  (equal? (run '(let ([x 1]) x))                1)
          (equal? (run '(let ([x 1] [y 42]) y))        42)
          (equal? (run '(let ([x 1] [y 42]) (+ x y)))  43)
          (equal? (run '(let ([x 1]) (let ([x 2]) x)))  2)
          (equal? (run '(let ([x 1] [y 42]) (+ (+ (+ 1 (+ 2 3)) x) y)))  49)))
  (define (test-if)
    (and  (equal? (run '(if #t 41 42)) 41)
          (equal? (run '(if #f 41 42)) 42)
          (equal? (run '(if (= 1 1) 41 42)) 41)
          (equal? (run '(if (= 1 2) 41 42)) 42)
          (equal? (run '(if (= 1 1) (+ 40 1) (+ 40 2))) 41)
          (equal? (run '(if (= 1 2) (+ 40 1) (+ 40 2))) 42)))
  (define (test-pairs)
    (and  (equal? (run '(car (cons 1 2))) 1)                ; e0 and e1 are both immediate
          (equal? (run '(cdr (cons 1 2))) 2)           
          (equal? (run '(car (cons 1 (+ 2 20)))) 1)         ; e0 is immediate
          (equal? (run '(cdr (cons 1 (+ 2 20)))) 22)
          (equal? (run '(car (cons (+ 1 10) 2))) 11)        ; e1 is immediate
          (equal? (run '(cdr (cons (+ 1 10) 2)))  2)
          (equal? (run '(car (cons (+ 1 10) (+ 2 20)))) 11) ; neither e0 nor e1 are immediate
          (equal? (run '(cdr (cons (+ 1 10) (+ 2 20)))) 22)
          (equal? (run '(cons 1 (cons 2 (cons 3 (cons 4 ()))))) (list 1 2 3 4))))
  (define (test-begin)
    ; (define equal? list)
    (and   (equal? (run '(begin))       (void))
           (equal? (run '(begin 1))     1)
           (equal? (run '(begin 1 2))   2)
           (equal? (run '(begin 1 2 3)) 3)))
  (define (test-vectors)
    (and  (equal? (run '(make-vector 2 3)) (make-vector 2 3))
          (equal? (run '(make-vector 3 4)) (make-vector 3 4))
          ; (equal? (run '(make-vector 3))   (make-vector 3))
          (equal? (run '(let ([v (make-vector 10 1)])
                          (begin
                            (vector-set! v 1 10)
                            (cons (vector-ref v 0) (vector-ref v 1)))))
                  (cons 1 10))))
  (define (test-function-declarations)
    (and  (equal? (run '(let () (define (two) 2) (two)))
                  2)
          (equal? (run '(let () (define (two x) 2) (two 3)))
                  2)
          (equal? (run '(let () (define (identity x) x) (identity 11)))
                  11)
          (equal? (run '(let () (define (add1 x) (+ x 1)) (add1 11)))
                  12)
          (equal? (run '(let () (define (add x y) (+ x y)) (add 11 22)))
                  33)
          (equal? (run '(let () (define (foo x y) (+ x (+ y 1))) (foo 11 22)))
                  34)
          (equal? (run '(let ()
                            (define (fact x)
                              (if (= x 0) 1 (* x (fact (- x 1)))))
                          (fact 5)))
                  120)))
  (define (test-lambda/no-free)
    (and  (equal? (run '(let ([f (lambda () 10)]) (f))) 10)
          (equal? (run '((lambda () 10))) 10)
          (equal? (run '(let ([f (lambda (x) x)]) (f 10))) 10)
          (equal? (run '((lambda (x) x) 10)) 10)
          (equal? (run '(let ([f (lambda (x y) (+ 1 (+ x y)))]) (f 20 300))) 321)
          (equal? (run '((lambda (x y) (+ 1 (+ x y))) 20 300)) 321)))
  (define (test-thunks)
    ; From Aziz' test suite
    (and  (equal? (run '(let ([f (lambda () 12)]) (f))) 12)
          (equal? (run '(let ([f (lambda () (+ 12 13))]) (f))) 25)
          (equal? (run '(let ([f (lambda () 13)]) (+ (f) (f)))) 26)
          (equal? (run '(let ([f (lambda () 
                                   (let ([g (lambda () (+ 2 3))])
                                     (* (g) (g))))])
                          (+ (f) (f))))
                  50)
          (equal? (run '(let ([f (lambda () 
                                   (let ([f (lambda () (+ 2 3))])
                                     (* (f) (f))))])
                          (+ (f) (f))))
                  50)
          (equal? (run '(let ([f (if (boolean? (lambda () 12))
                                     (lambda () 13)
                                     (lambda () 14))])
                          (f)))
                  14)))
  (define (test-parameter-passing)
    ; From Aziz' test suite
    (and  (equal? (run '(let ([f (lambda (x) x)]) (f 12))) 12)
          (equal? (run '(let ([f (lambda (x y) (+ x y))]) (f 12 13))) 25)
          (equal? (run '(let ([f (lambda (x)
                                   (let ([g (lambda (x y) (+ x y))])
                                     (g x 100)))])
                          (f 1000)))
                  1100)
          (equal? (run '(let ([f (lambda (g) (g 2 13))])
                          (f (lambda (n m) (* n m))))) 26)
          (equal? (run '(let ([f (lambda (g) (+ (g 10) (g 100)))])
                          (f (lambda (x) (* x x))))) 10100)
          (equal? (run '(let ([f (lambda (f n m)
                                   (if (zero? n)
                                       m
                                       (f f (sub1 n) (* n m))))])
                          (f f 5 1)))
                  120)
          (equal? (run '(let ([f (lambda (f n)
                                   (if (zero? n)
                                       1
                                       (* n (f f (sub1 n)))))])
                          (f f 5)))
                  120)))
  (define (test-closures)
    ; From Aziz' test suite
    (and  (equal? (run '(let ([n 12]) (let ([f (lambda () n)]) (f)))) 12)
          (equal? (run '(let ([n 12]) (let ([f (lambda (m) (+ n m))]) (f 100)))) 112)
          (equal? (run '(let ([f (lambda (f n m)
                                   (if (zero? n)
                                       m
                                       (f (sub1 n) (* n m))))])
                          (let ([g (lambda (g n m)
                                     (f (lambda (n m) (g g n m)) n m))])
                            (g g 5 1)))) 120)
          (equal? (run '(let ([f (lambda (f n)
                                   (if (zero? n)
                                       1
                                       (* n (f (sub1 n)))))])
                          (let ([g (lambda (g n) (f (lambda (n) (g g n)) n))])
                            (g g 5)))) 120)))  
  (define (test-lambda)
    (and  (equal? (run '(let ()
                          (let ([f (lambda (x y) (+ x y))])
                            (f 1 20))))
                  21)
          (equal? (run '(let ([z 300])
                          (let ([f (lambda (x y) (+ x (+ y z)))])
                            (f 1 20))))
                  321)
          (equal? (run '(let ([z 300] [w 4000])
                          (let ([f (lambda (x y) (+ x (+ y (+ z w))))])
                            (f 1 20))))
                  4321)))
  (define (test-tail-calls)
    (and  (equal? (run '(let ()
                            (define (sum n a) (if (= n 0) a (sum (- n 1) (+ n a))))
                          (sum 100 0)))
                  5050)
          (equal? (run '(let ([f (lambda (y) y)])
                          (let ([x 20] [z 30])
                            (f z))))
                  30)
          (equal? (run '(let ([n 11])
                          (let ([f (lambda (f i s)
                                     (if (= i n)
                                         s
                                         (f f (+ i 1) (+ s i))))])
                            (f f 0 0))))
                  55)
          (equal? (run '(let ([n 10001])
                          (let ([f (lambda (f i s)
                                     (if (= i n)
                                         s
                                         (f f (+ i 1) (+ s i))))])
                            (f f 0 0))))
                  50005000)))
  (define (test-quotations)
    (and  (equal? (run '(let () '7)) 7)
          (equal? (run '(let () '(1 2))) '(1 2))
          (equal? (run '(let () '(11 . 22))) '(11 . 22))))
  (define (test-boxes)
    (and  (equal? (run '(box 11))          (box 11))
          (equal? (run '(unbox (box 12)))  12)
          (equal? (run '(let ([b (box 13)]) (begin (set-box! b 14) (unbox b))))  14)
          (equal? (run '(let ([b0 (box 10)] [b1 (box 11)]) (unbox b1))) 11)))
  (define (test-bytes)
    (and  (equal? (run '(bytes? (make-bytes 10 #xff)))          #t)
          (equal? (run '(bytes? (box #xff)))                    #f)
          (equal? (run '(bytes-ref (make-bytes 10 7)  0))  7)
          (equal? (run '(bytes-ref (make-bytes 10 7)  9))  7)
          (equal? (run '(let ([bs (make-bytes 10 #xff)])
                          (bytes-set! bs 5 8)
                          (bytes-ref bs 5)))               8)
          (equal? (run '(let ([bs (make-bytes 10 #xff)])
                          (bytes-set! bs 5 8)
                          (bytes-ref bs 6)))               #xff)
          (equal? (run '(let ([bs (make-bytes 10 #xff)])
                          (bytes-set! bs 5 8)
                          (bytes-ref bs 4)))               #xff)))
  (define (test-strings)
    (and  (equal? (run '(string? (make-string 10 #\a)))          #t)
          (equal? (run '(string? (box #xff)))                    #f)
          (equal? (run '(string-ref (make-string 10 #\7)  0))  #\7)
          (equal? (run '(string-ref (make-string 10 #\7)  9))  #\7)
          (equal? (run '(let ([s (make-string 10 #\a)])
                          (string-set! s 5 #\b)
                          (string-ref s 5)))               #\b)
          (equal? (run '(let ([s (make-string 10 #\a)])
                          (string-set! s 5 #\b)
                          (string-ref s 6)))               #\a)
          (equal? (run '(let ([s (make-string 10 #\a)])
                          (string-set! s 5 #\b)
                          (string-ref s 4)))               #\a)
          (equal? (run '(string=? (make-string 0 #\a) (make-string 0 #\a))) #t)
          (equal? (run '(string=? (make-string 1 #\a) (make-string 1 #\a))) #t)
          (equal? (run '(string=? (make-string 2 #\a) (make-string 2 #\a))) #t)
          (equal? (run '(string=? (make-string 3 #\a) (make-string 3 #\a))) #t)
          
          (equal? (run '(string=? (make-string 0 #\a) (make-string 1 #\a))) #f)
          (equal? (run '(string=? (make-string 1 #\a) (make-string 2 #\a))) #f)
          (equal? (run '(string=? (make-string 1 #\a) (make-string 3 #\a))) #f)
          (equal? (run '(string=? (make-string 3 #\a) (make-string 4 #\a))) #f)
          
          (equal? (run '(string=? (make-string 1 #\a) (make-string 1 #\b))) #f)
          (equal? (run '(string=? (make-string 2 #\a) (make-string 2 #\b))) #f)))
  (define (test-assignments)
    (and  (equal? (run '(let ([x 11]) (begin (set! x 12) x)))        12)
          (equal? (run '(let ([x 12]) (begin (set! x (+ x 1)) x)))   13)
          (equal? (run '(let ([x 13]) (let ([x #f]) (begin (set! x 14) x)))) 14)
          (equal? (run '(let ([x 14]) (let ([y (let ([x #f]) (set! x 15))]) x))) 14)
          (equal? (run '(let ([f #f]) (let ([g (lambda () f)]) (begin (set! f 15) (g))))) 15)
          (equal? (run '(let ([x 16]) (begin (set! x x) x))) 16)
          (equal? (run '(let ([x (box 17)]) (unbox x))) 17)
          (equal? (run '(let ([x 17]) (begin (set! x (box x)) (unbox x)))) 17) 
          (equal? (run '(let ([f (lambda (x) (begin (set! x 18) x))]) (f 17))) 18)
          (equal? (run '(let ([f (lambda (x) (begin (set! x x) x))]) (f 19))) 19)
          (equal? (run '(let ([f (lambda (x) (begin (set! x (add1 x)) x))]) (f 20))) 21)
          (equal? (run '(let ([x 21])
                          (let ([f (lambda (x) (begin (set! x (add1 x)) x))])
                            (cons x (f x)))))
                  '(21 . 22))
          (equal? (run '(let ([t #f])
                          (let ([locative (cons (lambda () t)
                                                (lambda (n) (set! t n)))])
                            (begin
                              ((cdr locative) 23)
                              ((car locative))))))
                  23)
          (equal? (run '(let ([locative
                               (let ([t #f])
                                 (cons
                                  (lambda () t)
                                  (lambda (n) (set! t n))))])
                          (begin
                            ((cdr locative) 24)
                            ((car locative)))))
                  24)
          (equal? (run '(let ([make-counter
                               (lambda ()
                                 (let ([counter -1])
                                   (lambda ()
                                     (begin
                                       (set! counter (add1 counter))
                                       counter))))])
                          (let ([c0 (make-counter)]
                                [c1 (make-counter)])
                            (begin
                              (c0)
                              (cons (c0) (c1))))))
                  '(1 . 0))
          (equal? (run '(let ([fact #f])
                          (begin
                            (set! fact (lambda (n)
                                         (if (zero? n)
                                             1
                                             (* n (fact (sub1 n))))))
                            (fact 5))))
                  120)
          (equal? (run '(let ([fact #f])
                          ((begin 
                             (set! fact (lambda (n)
                                          (if (zero? n)
                                              1
                                              (* n (fact (sub1 n))))))
                             fact)
                           5)))
                  120)))
  ;; The tests below require the expander.
  (define (test-letrec)
    (and  (equal? (run '(letrec () 12)) 12)
          (equal? (run '(letrec ([f 12]) f))12)
          (equal? (run '(letrec ([f 12] [g 13]) (+ f g))) 25)
          (equal? (run '(letrec ([fact
                                  (lambda (n)
                                    (if (zero? n)
                                        1
                                        (* n (fact (sub1 n)))))])
                          (fact 5)))
                  120)
          (equal? (run '(letrec ([f 12] [g (lambda () f)])
                          (g)))
                  12)
          (equal? (run '(letrec ([f 12] [g (lambda (n) (set! f n))])
                          (g 130)
                          f))
                  130)
          (equal? (run '(letrec ([f (lambda (g) (set! f g) (f))])
                          (f (lambda () 12)))) 12)
          (equal? (run '(letrec ([f (cons (lambda () f)
                                          (lambda (x) (set! f x)))])
                          (let ([g (car f)])
                            ((cdr f) 100)
                            (g)))) 100)
          (equal? (run '(letrec ([f (letrec ([g (lambda (x) (* x 2))])
                                      (lambda (n) (g (* n 2))))])
                          (f 12))) 48)
          (equal? (run '(letrec ([f (lambda (f n)
                                      (if (zero? n)
                                          1
                                          (* n (f f (sub1 n)))))])
                          (f f 5))) 120)
          (equal? (run '(let ([f (lambda (f)
                                   (lambda (n)
                                     (if (zero? n)
                                         1
                                         (* n (f (sub1 n))))))])
                          (letrec ([fix
                                    (lambda (f)
                                      (f (lambda (n) ((fix f) n))))])
                            ((fix f) 5))))
                  120)))
  (define (test-named-let)
    (and  (equal? (run '(let loop ([n 5] [sum 0]) (if (zero? n) sum (loop (- n 1) (+ sum n))))) 15)))
  (define (test-cond)
    (and  (equal? (run '(cond [3])) 3)
          (equal? (run '(let ([else #f]) (cond [else 10] [#t 11]))) 11)
          ; test cases from Aziz
          (equal? (run '(cond [1 2] [else 3])) 2)
          (equal? (run '(cond [1] [else 13]))                                  1)
          (equal? (run '(cond [#f #t] [#t #f]))                                #f)
          (equal? (run '(cond [else 17]))                                      17)
          (equal? (run '(cond [#f] [#f 12] [12 13]))                           13)
          (equal? (run '(cond [(cons 1 2) => (lambda (x) (cdr x))]))           2)
          (equal? (run '(let ([else #t]) (cond [else 1287])))                  1287)
          (equal? (run '(let ([else 17]) (cond [else])))                       17)
          (equal? (run '(let ([else 17]) (cond [else => (lambda (x) x)])))     17)
          (equal? (run '(let ([else #f]) (cond [else ((lambda (x) (x x)) (lambda (x) (x x)))]) else)) #f)
          (equal? (run '(let ([=> 12])   (cond [12 => 14] [else 17]))) 14)
          (equal? (run '(let ([=> 12])   (cond [=>]))) 12)
          (equal? (run '(let ([=> 12])   (cond [=> =>]))) 12)
          (equal? (run '(let ([=> 12])   (cond [=> => =>]))) 12)
          (equal? (run '(let ([let 12])  (cond [let => (lambda (x) (+ let x))] [else 14]))) 24)))
  (define (test-when/unless)
    ; From Aziz.
    ; But we don't have set-car! and set-cdr! for immutable pairs.
    (and #;(equal? (run '(let ([x (cons 1 2)])
                         (when (pair? x) 
                           (set-car! x (+ (car x) (cdr x))))
                         x))
                 '(3 . 2))
         #;(equal? (run '(let ([x (cons 1 2)])
                         (when (pair? x) 
                           (set-car! x (+ (car x) (cdr x)))
                           (set-car! x (+ (car x) (cdr x))))
                         x))
                 '(5 . 2))
         #;(equal? (run '(let ([x (cons 1 2)])
                         (unless (fixnum? x) 
                           (set-car! x (+ (car x) (cdr x))))
                         x))
                 '(3 . 2))
         #;(equal? (run '(let ([x (cons 1 2)])
                         (unless (fixnum? x) 
                           (set-car! x (+ (car x) (cdr x)))
                           (set-car! x (+ (car x) (cdr x))))
                         x))
                 '(5 . 2))
         (equal? (run '(let ([let 12])
                         (when let let let let let)))
                 12)
         (equal? (run '(let ([let #f])
                         (unless let let let let let)))
                 #f)))
  (define (test-and/or)
    ; From Aziz
    (and  (equal? (run '(and)) #t)
          (equal? (run '(and 5))5)
          (equal? (run '(and #f)) #f)
          (equal? (run '(and 5 6)) 6)
          (equal? (run '(and #f ((lambda (x) (x x)) (lambda (x) (x x))))) #f)
          (equal? (run '(or)) #f)
          (equal? (run '(or #t)) #t)
          (equal? (run '(or 5)) 5)
          (equal? (run '(or 1 2 3)) 1)
          (equal? (run '(or (cons 1 2) ((lambda (x) (x x)) (lambda (x) (x x))))) '(1 . 2))
          (equal? (run '(let ([if 12]) (or if 17))) 12)
          (equal? (run '(let ([if 12]) (and if 17))) 17)
          (equal? (run '(let ([let 8]) (or let 18))) 8)
          (equal? (run '(let ([let 8]) (and let 18))) 18)
          (equal? (run '(let ([t 1])
                          (and (begin (set! t (add1 t)) t) t))) 2)
          (equal? (run '(let ([t 1])
                          (or (begin (set! t (add1 t)) t) t))) 2)))
  (define (test-begin0)
    (and  (equal? (run '(begin0 10))                10)
          (equal? (run '(begin0 10 11))             10)
          (equal? (run '(begin0 10 11 12))          10)
          (equal? (run '(begin0 (begin0 10) 11 12)) 10)
          (equal? (run '(begin0 (begin0 10 11) 12)) 10)
          (equal? (run '(begin0 (begin0 10 11)))    10)
          (equal? (run '(let ([t 20]) (begin0 10))) 10)))
  (define (test-symbols)
    (list (equal? (run '(let ([x (string->symbol            (make-string 5 #\x))]
                              [y (string->symbol            (make-string 3 #\y))]
                              [z (string->uninterned-symbol (make-string 3 #\z))])
                          (cons (symbol-interned? x)
                                (cons (symbol-interned? y)
                                      (cons (symbol-interned? z)
                                            '())))))
                  '(#t #t #f))))
  (list "-- Core Constructs --"
        ;; (list "Immediate Values"              (test-immediates))
        ;; (list "Call unary primitive"          (test-call-unary-primitive))
        ;; #;(list "Some characters "            (test-some-characters)) ; slow
        ;; #;(list "All characters"              (test-all-characters))  ; very slow
        ;; (list "Call binary primitive"         (test-call-binary-primitive))
        ;; (list "Local variables (let)"         (test-let))
        ;; (list "Conditional (if)"              (test-if))
        ;; (list "Sequencing (begin)"            (test-begin))
        ;; (list "Vectors"                       (test-vectors))
        ;; (list "Functions"                     (test-function-declarations))
        ;; (list "Lambda without free variables" (test-lambda/no-free))
        ;; (list "Lambda - Thunks"               (test-thunks))
        ;; (list "Lambda - Parameter passing"    (test-parameter-passing))
        ;; (list "Lambda - Closures"             (test-closures))
        ;; (list "Lambda"                        (test-lambda))
        ;; (list "Tail calls"                    (test-tail-calls))
        ;; (list "Quotations"                    (test-quotations))
        ;; (list "Boxes"                         (test-boxes))
        ;; (list "Assignments"                   (test-assignments))
        ;; (list "Byte strings"                  (test-bytes))
        ;; (list "Strings"                       (test-strings))
        ;; Tests below require the expander to be present.
        "-- Derived Constructs --"
        (list "Letrec"                        (test-letrec))  ;; TODO!
        ;; (list "Named let"                     (test-named-let))
        ;; (list "And/Or"                        (test-and/or))
        ;; (list "Cond"                          (test-cond))
        ;; (list "When/unless"                   (test-when/unless))
        ;; (list "Begin0"                        (test-begin0))
        ))
