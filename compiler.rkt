#lang racket/base
(module+ test (require rackunit))
(provide (all-defined-out))

(require "expander.rkt"       ; provides topexpand
         "assembler.rkt"
         "priminfo.rkt"       ; information on Racket primitives
         "runtime-wasm.rkt"   ;
         "define-foreign.rkt"
         "parameters.rkt"
         ; "wasm-data.rkt"
         nanopass/base
         racket/match
         racket/port 
         (only-in racket/format ~a)
         (only-in racket/list partition append* first second third last
                              index-where append-map make-list rest take drop)
         (only-in racket/set  list->set))
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

;;; todo - just for testing FFI

  ;; (define ffi-files '("dom.ffi"))
  ;; (for ([ffi-filename ffi-files])
  ;;   (unless (file-exists? ffi-filename)
  ;;     (error 'drive-compilation (~a "ffi file not found: " ffi-filename))))

  ;; (define ffi-foreigns  '()) ; list of `foreign` structures
  ;; (define ffi-imports   '()) ; list of wat
  ;; (define ffi-funcs     '()) ; list of wat
  ;; (for ([ffi-filename ffi-files])
  ;;   (define fs (ffi-file->foreigns ffi-filename))
  ;;   (define ims   (map foreign->import fs))
  ;;   (define prims (map foreign->primitive fs))
  ;;   (set! ffi-foreigns (cons fs    ffi-foreigns))
  ;;   (set! ffi-imports  (cons ims   ffi-imports))
  ;;   (set! ffi-funcs    (cons prims ffi-funcs)))
  ;; (set! ffi-foreigns (append* (reverse ffi-foreigns)))
  ;; (set! ffi-imports  (append* (reverse ffi-imports)))
  ;; (set! ffi-funcs    (append* (reverse ffi-funcs)))

  ;; (current-ffi-foreigns    ffi-foreigns)
  ;; (current-ffi-imports-wat ffi-imports)
  ;; (current-ffi-funcs-wat   ffi-funcs)

;; --- done for testing FFI section


;;;
;;; Expressions to work on
;;;

; [ ] (list (procedure-arity (case-lambda [() 10] [x 1])) -1)))
;     result is (0 -1) instead of the expected -1


;;;
;;; TODO
;;;

; [ ] Consider adding extra shapes to `primitive-invoke`.
;     The possible shapes:
;        (list->set (map (λ (x) (if x (primitive-description-arity x) #f))
;                          (map primitive->description primitives)))
;     - The shape could be precomputed.
; [ ] For primitives not in original Racket, provide arity information.

; [ ] Extend +, fx+ and friends to be variadic.

; [ ] Modules!

; [ ] Keyword arguments
;     [ ] struct:keyword-procedure/arity-error
;     [ ] prop:named-keyword-procedure
;     [ ] missing-kw
;     [ ] null

; [x] $vector-immutable - Make the returned vector immutable


; [ ] Input / output port going to the host.

; [x] case-lambda
; [/] Calling conventions for primitives?
;       - optional and variadic primitives are now possible

; [x] procedure?, procedure-rename, procedure-arity, etc.
; [x] call-with-values
; [x] apply
; [x] map
; [x] for-each

; [x] Use new machinery for runtime string and symbol constants.

; [x] Inline the `void` primitive.
; [x] Implement a `void` function.

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

; [/] A reference to an undefined, top-level variable must lookup the variable
;     in the current namespace.
;     Currently the compiler just reports that classify-variable can't
;     can't find the identifier.
;     For now, top-level variables are defined as global variables that contain a $Boxed.


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


; [x] generating string and symbol constants for use in the runtime

; [ ] Implement guards for structs.

; [ ] String ports.

; [ ] Parameters.

; [ ] Synchronize primitives between compiler and the `webracket` language.


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

(require "structs.rkt")

; Representation of variables during compilation.
;     (struct variable (id) #:transparent)
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
       (or (wr-signed-fixnum? x)
           (wr-unsigned-fixnum? x))))

(define (wr-signed-fixnum? x)
  (and (number? x)
       (integer? x)
       (<= s30-min x s30-max)))

(define (wr-unsigned-fixnum? x)
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
;;; CONSTANTS
;;;

; See also this list:
;    https://gist.github.com/jesboat/1859af07d6d7b8521bf40b59a551fd79

(require (only-in racket/math pi))

(define constants '(null       ; '() racket/base
                    undefined  ;     racket/undefined
                    empty      ; '() racket/list
                    true       ;     racket/bool (not racket/base)
                    false      ;     racket/bool (not racket/base)
                    pi))       ;     racket/math

(define (constant-value c)
  (case c
    [(null empty) '()]
    [(true)       #t]
    [(false)      #f]
    [(undefined)  datum:undefined]
    [(pi)         pi]
    [else         (error 'constant-value "got: ~a" c)]))
  

;;;
;;; PRIMITIVES
;;;

;; For some primitives we have need an easy way to construct a
;; variable reference to the primitive.

(define primitives '())  ; includes the ffi-primitives


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

  current-inspector          ; todo
  
  values                     
  
  ; box-immutable ; todo 

  boxed      ; used by assignment elimination
  unboxed    ; used by assignment elimination
  set-boxed! ; used by assignment elimination

  box unbox set-box! ; normal primitives
  
  vector-immutable  ; used in datum construction
  bytes             ; used in datum construction
  string            ; used in datum construction

  pair? cons? null? empty?
  cons car cdr
  list              ; not first order yet
  list? length list-ref list-tail
  first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth thirteenth fourteenth fifteenth last last-pair
  append ; variadic list primitive
  reverse memq
  alt-reverse ; used in expansion of for/list
  map andmap ormap for-each
  list*
  filter partition remove
  make-list
   build-list
   argmax argmin

   void?
  make-void  ; zero arguments
  void

  ;; BOOLEANS
  boolean? not immutable?

  ;; CHARACTERS
  char?
  char->integer
  integer->char
  ; comparisons
  char=?           ; variadic
  char<?           ; variadic
  char<=?          ; variadic
  char>?           ; variadic
  char>=?          ; variadic
  ; char conversion
  char-downcase
  char-foldcase
  char-titlecase
  char-upcase
  ; char comparisons using conversions
  char-ci=?           ; variadic
  char-ci<?           ; variadic
  char-ci<=?          ; variadic
  char-ci>?           ; variadic
  char-ci>=?          ; variadic
  ; char predicates
  char-whitespace?
  
  eq?
  eqv?
  equal?

  number->string
  string->number

  + - * / quotient remainder modulo quotient/remainder
  = < > <= >=
  zero? positive? negative? even? odd?
  add1 sub1 gcd lcm

  number?
  integer?
  exact? exact-integer?
  exact-nonnegative-integer?
  exact-positive-integer?
  nan? infinite?
  positive-integer? negative-integer?
  nonpositive-integer? nonnegative-integer?
  natural?
  inexact?
  inexact->exact
  exact->inexact
  exact-round exact-floor exact-ceiling exact-truncate
  round floor ceiling truncate
  sin  cos  tan  asin  acos  atan
  sinh cosh tanh asinh acosh atanh
  degrees->radians radians->degrees
  order-of-magnitude
  abs sgn max min sqr sqrt integer-sqrt integer-sqrt/remainder expt exp log

  bitwise-ior bitwise-and bitwise-xor bitwise-not bitwise-bit-set?
  bitwise-first-bit-set  ; note : added in 8.16
  integer-length
  random

  fixnum? fxzero?
  fx+ fx- fx*
  fx= fx> fx< fx<= fx>=
  fxmin fxmax

  fxquotient unsafe-fxquotient
  fxremainder fxmodulo fxabs
  fxand fxior fxxor fxnot fxlshift fxrshift
  fxpopcount fxpopcount16 fxpopcount32
  fx+/wraparound fx-/wraparound fx*/wraparound fxlshift/wraparound
  fxrshift/logical
  most-positive-fixnum most-negative-fixnum

  fx->fl
  fl->fx
  ; fixnum-for-every-system?

  flonum?
  fl+ fl- fl* fl/
  fl= fl< fl> fl<= fl>=
  flabs flround flfloor flceiling fltruncate flsingle
  fllog flexp flsqrt 
  flsin flcos fltan flasin flacos flatan
  flmin flmax flexpt ->fl fl->exact-integer

  byte?

  vector 
  vector? make-vector vector-ref vector-set! vector-length
  vector-fill! vector-copy! vector-empty? vector-take vector-drop
  vector-drop-right vector-split-at
  vector->list list->vector vector-copy vector-map vector-map!
  
  bytes?  make-bytes  bytes-ref  bytes-set!  bytes-length  subbytes bytes-copy!
  bytes-copy bytes-fill! bytes-append bytes->immutable-bytes
  bytes->list list->bytes bytes=?
  bytes->string/utf-8

  string? string=? string<? string<=? string>? string>=?
  make-string build-string string-ref string-set! string-length substring
  string-copy!
  string-copy string-fill! string-append string-append-immutable
  string->list list->string
  string->bytes/utf-8 string->immutable-string
  non-empty-string?

  string-take        ; not in Racket
  string-take-right  ; not in Racket
  string-drop        ; not in Racket
  string-drop-right  ; not in Racket
  string-trim-left   ; not in Racket
  string-trim-right  ; not in Racket
  string-suffix?     ; from racket/string
  string-prefix?     ; from racket/string
  string-contains?   ; from racket/string
  ; string-find        ; from racket/string  (added in 8.15)

  symbol? symbol=? symbol<? 
  string->symbol symbol->string
  string->uninterned-symbol symbol-interned?
  symbol->immutable-string
  
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
  hash?
  hash-ref
  hash-set!
  hash-remove!
  hash-clear!
  hash-has-key?
  eq-hash-code

  keyword?
  keyword->string
  keyword->immutable-string
  string->keyword
  keyword<?

  apply
  procedure-rename
  procedure?
  procedure->external
  procedure-arity
  procedure-arity-mask
  procedure-arity-includes?

  primitive?
  primitive-closure?
  primitive-result-arity

  variable-reference-from-unsafe?
  variable-reference-constant?

  js-log

  ;; 10. Control Flow
  call-with-values
  
  ;; 17. Unsafe Operations
  unsafe-fx+
  unsafe-fl/
  unsafe-flabs unsafe-flround unsafe-flfloor unsafe-flceiling unsafe-fltruncate
  unsafe-flsingle unsafe-flsin unsafe-flcos unsafe-fltan unsafe-flasin
  unsafe-flacos unsafe-flatan unsafe-fllog unsafe-flexp unsafe-flsqrt
  unsafe-flmin unsafe-flmax unsafe-flexpt

  unsafe-fx=
  unsafe-fx<

  unsafe-car
  unsafe-cdr
  unsafe-struct-ref
  unsafe-vector-length
  unsafe-vector-ref
  unsafe-vector*-length
  unsafe-vector*-set!
  unsafe-struct-set!

  namespace?
  make-empty-namespace
  namespace-variable-value-simple ; ns sym -> value
  namespace-set-variable-value!
  namespace-undefine-variable!
  ; namespace-has-key?
  
  ;; support for `for`
  ; The functions below will be removed, when `webracket` implements `for`
  ; grow-vector
  ; shrink-vector

  external-number->flonum
  external-string->string
  external?
  )

;;;
;;; FFI Primitives
;;;

;; An FFI Primitive is a primitive that is defined in an .ffi file.

(define ffi-primitives '()) ; list of symbols

(define (define-ffi-primitive name)
  ; 1. There is no `var:name` since `var:name` is only used
  ;    in "compiler.rkt" to generate code.
  (set!     primitives (cons name     primitives))
  (set! ffi-primitives (cons name ffi-primitives)))

;; Primitives declared using ffi-files.

(define (define-ffi-primitives names)
  (for-each define-ffi-primitive names))

(define (reset-ffi-primitives)
  ; called by `parse` before parsing begins
  (define-ffi-primitives (foreigns->primitive-names (current-ffi-foreigns))))

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
     (non-top s x)                                => (non-top x)
     (anonymous s)                                => (anonymous)
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
    (reset-ffi-primitives)
    (reset-runtime-constants)
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
        [(#%variable-reference (#%top . x:id))      `(variable-reference ,E (top       ,E ,(variable #'x)))]
        [(#%variable-reference x:id)                `(variable-reference ,E (non-top   ,E ,(variable #'x)))]
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
    #;(define (quoted-symbol! s sym)
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
                    (? null?)
                    (? void?)                    
                    (? number?))) ; a bignum or rational number
           (if (number? v)
               (if (not (or (wr-fixnum? v) (flonum? v)))
                   `(quote ,s ,(datum s (* 1. v))) ; convert to a flonum
                   `(quote ,s ,(datum s v)))       ; fixnum or flonum
               `(quote ,s ,(datum s v)))]          ; other literals
          [(? string? cs)  `(quote ,s ,(datum s v))]
          [(? bytes?  bs)  `(quote ,s ,(datum s v))]
          [(? symbol? sym) `(quote ,s ,(datum s v))]
          [(? pair? p)     `(app ,h ,(var:cons) ,(loop (car p)) ,(loop (cdr p)))]
          [(? vector? v)   (let ([vs (map loop (vector->list v))])
                             `(app ,h ,(var:vector-immutable) ,vs ...))]
          [(? box? b)       `(app ,h ,(var:box) ,(loop (unbox b)))]
          [(? keyword? kw) (let ([s (loop (keyword->string kw))])
                             `(app ,h ,(var:string->keyword) ,s))]
          [else            (error 'datum->construction-expr "got: ~a" v)]))))
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

(define α-rename-mode (make-parameter 'full))  ; 'full or 'simple

; 'simple is only used for the existig test cases
; 'full   is used otherwise
; In the simple mode, the  first occurence of a variable x in a scope is not renamed.
; In the full mode, all variables are renamed.
; In the code generator the variables of a `let-values` all end up as local variables
; in a `func`. Therefore, all variable names must be unique.
; In the simple mode, the program (begin (let ([x 1]) x)  (let ([x 2]) x)) fails.
; Someday, when the test cases are rewritten, we can remove the simple mode.

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
    (define (extend-map-to-self* ρ xs)
      (if (null? xs)
          ρ
          (extend-map-to-self* (extend ρ (car xs) (car xs)) (cdr xs))))
    (define (fresh/simple x ρ [orig-x x])
      (if (ρ x) (fresh (new-var x) ρ x) x))
    (define (fresh/full x ρ [orig-x x])
      (if (ρ x) (fresh (new-var x) ρ x) (new-var x)))
    (define fresh (case (α-rename-mode)
                    [(simple) fresh/simple]
                    [(full)   fresh/full]))
    (define (rename x ρ)
      (define x* (fresh x ρ))
      (values x* (extend ρ x x*)))
    (define (rename*          xs ρ) (map2* rename   xs ρ))
    (define (rename**        xss ρ) (map2* rename* xss ρ))
    (define (TopLevelForm*    xs ρ) (map2* TopLevelForm xs ρ))
    (define (ModuleLevelForm* xs ρ) (map2* ModuleLevelForm xs ρ))
    (define (Formals*         fs ρ) (map2* Formals fs ρ))
    (define (Abstraction*    abs ρ) (map2* Abstraction abs ρ))
    ; (define (Expr*         es ρ) (map (λ (e) (letv ((e _) (Expr e ρ)) e)) es))  ; <--
    (define (Expr*         es ρ)    (if (null? es)
                                        (values '() ρ)
                                        (letv ((e ρ) (Expr (car es) ρ))
                                          (letv ((es ρ) (Expr* (cdr es) ρ))
                                            (values (cons e es) ρ)))))
    (define (Binding* xss es ρ)
      (letv ((es ρ) (Expr* es ρ))  ; xss are not bound in es
        (letv ((xss ρ) (rename** xss ρ))
          (values xss es ρ))))
    (define (RecBinding* xss es ρ)
      (letv ((xss ρ) (rename** xss ρ))
        (letv ((es ρ) (Expr* es ρ)) ; xss are bound in es
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
    [(define-values   ,s (,x ...) ,e)   (cond
                                          [(inside-module?)
                                           (letv ((x ρ) (rename* x ρ))
                                                 (letv ((e _) (Expr e ρ))
                                                       (values `(define-values ,s (,x ...) ,e) ρ)))]
                                          [else
                                           ; top-level variables aren't renamed
                                           ; but in order to see that they are defined,
                                           ; they must map to themselves. 
                                           (letv ((e _) (Expr e ρ))
                                                 (let ((ρ (extend-map-to-self* ρ x)))
                                                   (values `(define-values ,s (,x ...) ,e) ρ)))])]
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
                                                     (unless (variable? x)
                                                       (error 'here "got ~a" x))
                                                     (define s (syntax-e (variable-id x)))
                                                     (cond
                                                       [(memq s constants)
                                                        (values `(quote ,(variable-id x)
                                                                        ,(datum (variable-id x) (constant-value s)))
                                                                ρ)]
                                                       [else
                                                        ; signal unbound variable at runtime
                                                        (displayln (list 'WARNING "unbound?" x))
                                                        (values `(app ,#'here
                                                                      ,(variable #'raise-unbound-variable-reference)
                                                                      ,`(quote ,(variable-id x)
                                                                               ,(datum (variable-id x) s))
                                                                      ; Note: `datum` has been eliminated at this point,
                                                                      ;       so a different approach is needed
                                                                      ; ,`(quote ,#'here  ,(datum #'unbound (variable-id x)))
                                                                      )
                                                                ρ)])]
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
    [(begin  ,s ,e0 ,e1 ...)                    (letv ((e0 ρ) (Expr e0 ρ))
                                                  (letv ((e1 ρ) (Expr* e1 ρ))
                                                    (values `(begin ,s ,e0 ,e1 ...) ρ)))]
    [(begin0 ,s ,e0 ,e1 ...)                    (letv ((e0 ρ) (Expr e0 ρ))
                                                  (letv ((e1 ρ) (Expr* e1 ρ))
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
    [(app ,s ,e0 ,e1 ...)                     (letv ((e0 ρ) (Expr e0 ρ))
                                                (letv ((e1 ρ) (Expr* e1 ρ))
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
                (parameterize ([α-rename-mode 'simple])
                  (unparse-all
                   (unparse-LFE2 (α-rename
                                  (explicit-case-lambda
                                   (explicit-begin
                                    (parse (expand-syntax stx)))))))))])
      (check-equal? (test #'(let ([x 10])
                              (let ([x 1]
                                    [y x])
                                y)))
                    '(let-values (((x) '10))
                       (let-values (((x.1) '1)
                                    ((y)    x))
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
    (define (TopLevelForm*    Ts  xs)  (map* TopLevelForm    Ts  xs))
    (define (ModuleLevelForm* Ms  xs)  (map* ModuleLevelForm Ms  xs))
    (define (Expr*            Es  xs)  (map* Expr            Es  xs))
    (define (Abstraction*     ABs xs)  (map* Abstraction     ABs xs)))
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
      (parameterize ([α-rename-mode 'simple])
        (unparse-all
         (unparse-LFE2
          (assignment-conversion
           (α-rename
            (explicit-case-lambda
             (explicit-begin
              (parse
               (expand-syntax stx))))))))))
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
      (parameterize ([α-rename-mode 'simple])
        (unparse-all
         (unparse-LFE3
          (categorize-applications
           (assignment-conversion
            (α-rename
             (explicit-case-lambda
              (explicit-begin
               (parse
                (expand-syntax stx)))))))))))
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

        [(variable-reference ,s (anonymous ,s0))   (with-output-language (LANF AExpr) 
                                                      (k `(variable-reference ,s (anonymous ,s0))))]
        [(variable-reference ,s (non-top ,s0 ,x))  (with-output-language (LANF AExpr) 
                                                      (k `(variable-reference ,s (non-top ,s0 ,x))))]
        [(variable-reference ,s (top ,s0 ,x))      (with-output-language (LANF AExpr) 
                                                     (k `(variable-reference ,s (top ,s0 ,x))))]
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
      (parameterize ([α-rename-mode 'simple])
        (unparse-all
         (unparse-LANF
          (anormalize
           (categorize-applications
            (assignment-conversion
             (α-rename
              (explicit-case-lambda
               (explicit-begin
                (parse
                 (expand-syntax stx))))))))))))
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
      (pretty-print (unparse-LANF T)) (newline)
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
  ; (displayln sets)
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

; this information was also needed by "runtime-wasm.rkt" and "wasm-utils.rkt"
(require "immediates.rkt") 

;;;
;;; CODE GENERATOR
;;;

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


;; A few helpers for generating WebAssembly is placed in:
(require "wasm-utils.rkt")    ; helpers like $, $$, prim:, Imm, Var, etc.

; The helpers allow us to follow these conventions:

; Convention:
;   1. WebAssembly requires all identifiers to begin with $.
;   2. We prefix all program identifiers with $$.
;   3. The compiler is free to control all identifiers that begin with a single $.

; a symbol `name` turns into "$string:name"
(define string-constants '())  ; (list (list name string) ...)
(define (add-string-constant name string)
  (set! string-constants (cons (list name string) string-constants)))
(define (reset-string-constants)  ; called in parse
  (set! string-constants '()))

; a symbol `name` turns into "$bytes:name"
(define bytes-constants '())  ; (list (list name bytes) ...)
(define (add-bytes-constant name bytes)
  (set! bytes-constants (cons (list name bytes) bytes-constants)))
(define (reset-bytes-constants)  ; called in parse
  (set! bytes-constants '()))

(define symbol-constants '())  ; (list (list name symbol) ...)
(define (add-symbol-constant name symbol)
  (set! symbol-constants (cons (list name symbol) symbol-constants)))
(define (reset-symbol-constants)  ; called in parse
  (set! symbol-constants '()))

(define (reset-runtime-constants)
  (reset-string-constants)
  (reset-bytes-constants)
  (reset-symbol-constants))
  

(define-pass generate-code : LANF+closure (T) -> * ()
  (definitions
    ;; 1. Classify variables
    (define-values (top-vars module-vars local-vars)
      (classify-variables T))
    (define (top-variable? v)    (set-in? v top-vars))    ; boxed
    (define (module-variable? v) (set-in? v module-vars))
    (define (local-variable? v)  (set-in? v local-vars))
    (define (ffi-variable? v)    (set-in? (syntax-e (variable-id v))
                                          ffi-primitives))
    (define (classify v)
      (cond
        [(top-variable?    v) 'top]
        [(module-variable? v) 'module]
        [(local-variable?  v) 'local]
        [(ffi-variable? v)    'ffi]
        [else
         ;; (displayln (list 'top (map unparse-variable top-vars)))
         ;; (displayln (list 'mod (map unparse-variable module-vars)))
         ;; (displayln (list 'loc (map unparse-variable local-vars)))
         (error 'classify "got: ~a" v)]))
    ;; 2. References to variable according to their type
    (define (Reference v)
      ; reference to non-free variable
      ;   global refers to a Web Assembly global variable
      (case (classify v)
        [(top)        `(global.get ,(TopVar v))]   ; unboxed
        [(local)      `(local.get  ,(LocalVar v))]
        [(module)     `(module.get ,(ModuleVar v))]
        [(global)     `(global.get ,(syntax-e v))]
        [(ffi)        'TODO]
        [else (error 'Reference "got: ~a" v)]))
    ;; 3. Variables assignments according to type.
    (define (Store! v e)
      (cond
        [(variable? v)
         (case (classify v)
           [(top)    `(global.set ,(TopVar    v) ,e)]
           [(local)  `(local.set  ,(LocalVar  v) ,e)]
           [(module) `(module.set ,(ModuleVar v) ,e)]
           [(ffi)        'TODO]
           [else (error 'Store! "got: ~a" v)])]
        [(identifier? v)
         ; (displayln (list 'Store! v))
         ; variables like: $closedapp-clos and $result 
         ; global
         `(global.set ,(syntax-e v) ,e)]
        [else (displayln v)
              (error 'Store! "got: ~a" v)]))
    
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
    ;; In Web Assembly the only place to declare local variables
    ;; are in functions `(func ...)`.

    ;; When compiling the body of a function, we set the parameter
    ;; *locals* to the empty list. Any expressions that require
    ;; a local variable, will use `emit-local`. After the body is
    ;; compiled, the locals are added to the `func`.

    ;; The local variables needed at the top-level end up in `$entry`.
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
    ;; quoted strings
    (define quoted-string-counter 0)
    (define (add-quoted-string string)
      (define name (string->symbol (~a "quoted-string" quoted-string-counter)))
      (set! quoted-string-counter (+ quoted-string-counter 1))
      (add-string-constant name string)
      name)
    ;; quoted bytes
    (define quoted-bytes-counter 0)
    (define (add-quoted-bytes bytes)
      (define name (string->symbol (~a "quoted-bytes" quoted-bytes-counter)))
      (set! quoted-bytes-counter (+ quoted-bytes-counter 1))
      (add-bytes-constant name bytes)
      name)
    ;; quoted symbols
    (define quoted-symbol-counter 0)
    (define quoted-symbols-ht (make-hasheq))
    (define (add-quoted-symbol symbol)
      (cond
        [(hash-ref quoted-symbols-ht symbol #f)
         => values]
        [else
         (define name (string->symbol (~a "quoted-symbol" quoted-symbol-counter)))
         (hash-set! quoted-symbols-ht symbol name)
         (set! quoted-symbol-counter (+ quoted-symbol-counter 1))         
         (add-symbol-constant name symbol)
         name]))
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
         `(block (result (ref eq))
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
     (let* ([n   (length ca)]
            ;; arrays
            [as-init  `(array.new $Array    (global.get $null) (i32.const ,n))]
            [ars-init `(array.new $I32Array (i32.const 0)      (i32.const ,n))]
            [$as      (emit-fresh-local 'case-arms    '(ref $Array)    as-init)]
            [$ars     (emit-fresh-local 'case-arities '(ref $I32Array) ars-init)]
            ;; fill both arrays in lockstep
            [fills    (for/list ([m ar] [c ca] [i (in-naturals)])
                        (define arm (ClosureAllocation c #f)) ; => (ref $Closure)
                        `(block
                           (array.set $Array    ,(Reference $as)  (i32.const ,i) ,arm)
                           (array.set $I32Array ,(Reference $ars) (i32.const ,i) (i32.const ,m))))]
            ;; name (use #f unless you carry names)
            [name-expr '(global.get $false)])
       `(block (result (ref $CaseClosure))
               ,@fills
               (struct.new $CaseClosure
                           (i32.const 0)                                 ;; $hash
                           ,name-expr                                    ;; $name
                           ,(if (= (length ar) 1)
                                (Imm (car ar))
                                `(ref.cast (ref eq) ,(Reference $ars)))  ;; $arity  = precise set
                           (global.get $false)                           ;; $realm
                           (ref.func $invoke-case-closure)               ;; $invoke
                           (ref.func $code:case-lambda-dispatch)         ;; $code (dispatcher)
                           (global.get $empty-free)                      ;; $free (unused here)
                           ,(Reference $ars)                             ;; $arities (typed field)
                           ,(Reference $as))))                           ;; $arms    (typed field)
     ]                                                                 
    ) ; CaseClosureAllocation
  
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
                             [(flonum? v)  (define l (case v
                                                       [(+nan.0) 'nan] [(-nan.0) '-nan]
                                                       [(+inf.0) 'inf] [(-inf.0) '-inf]
                                                       [else v]))
                                           `(struct.new $Flonum (i32.const 0) (f64.const ,l))]
                             [(null? v)    '(global.get $null)]
                             [(void? v)    '(global.get $void)]
                             [(eq? v #t)   '(global.get $true)]  
                             [(eq? v #f)   '(global.get $false)]
                             [(fixnum? v)  (Imm v)]
                             [(char? v)    (Imm v)]
                             [(string? v)  (define name         (add-quoted-string v))
                                           (define $string:name (string->symbol (~a "$string:" name)))
                                           `(global.get ,$string:name)]
                             [(bytes? v)   (define name         (add-quoted-bytes v))
                                           (define $bytes:name  (string->symbol (~a "$bytes:" name)))
                                           `(global.get ,$bytes:name)]
                             [(symbol? v)  (define name         (add-quoted-symbol v))
                                           (define $symbol:name (string->symbol (~a "$symbol:" name)))
                                           `(global.get ,$symbol:name)]
                             [else         `',v]))])]
    [(top ,s ,x)
     ; Note: Until namespaces are implemented we represented top-level variables as using `$Boxed`.
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

    ;; Inline Primitives. Inlining.
    [(primapp ,s ,pr ,ae1 ...)

     ;; Inlines a call to a primitive with
     ;;   a fixed number of arguments.
     (define (inline-prim/fixed sym ae1 arg-count)
       (define aes (AExpr* ae1))
       (define n   (length aes))
       (when (> n arg-count) (error 'primapp "too many arguments: ~a" sym))
       (when (< n arg-count) (error 'primapp "too few arguments: ~a"  sym))
       `(call ,($ sym) ,@aes))

     ;; Inlines a call to a primitive with
     ;;   at least `min` arguments,
     ;;   at most  `max` arguments.
     ;; That is, arguments beyound `min` are optional.
     ;; Passes `(global.get $missing)` to indicate a missing argument.
     (define (inline-prim/optional sym ae1 min max)
       (define filler `(global.get $missing))
       (define aes (AExpr* ae1))
       (define n   (length aes))
       (when (> n max) (error 'primapp "too many arguments: ~a" sym))
       (when (< n min) (error 'primapp "too few arguments: ~a"  sym))
       (define optionals (make-list (- max n) filler))
       `(call ,($ sym) ,@aes ,@optionals))

     ;; Inlines a call to a primitive with
     ;;   at least `min` arguments,
     ;;   at most  `max` arguments.
     ;; That is, arguments beyound `min` are optional.
     ;; Passes `default` for missing arguments.
     ;; Note: The same default values is used for all arguments.
     (define (inline-prim/optional/default sym ae1 min max default)
       (define filler `(global.get $missing))
       (define aes (AExpr* ae1))
       (define n   (length aes))
       (when (> n max) (error 'primapp "too many arguments: ~a" sym))
       (when (< n min) (error 'primapp "too few arguments: ~a"  sym))
       (define optionals (make-list (- max n) default))
       `(call ,($ sym) ,@aes ,@optionals))

     (define (build-rest-args aes)
       (let loop ([aes aes])
         (if (null? aes)
             `(global.get $null)
             `(struct.new $Pair
                          (i32.const 0)
                          ,(first aes)
                          ,(loop (rest aes))))))
     
     ;; Inlines a call to a primitive with
     ;;   at least `min` arguments.
     ;; Arguments from `rest-start` are rest arguments.
     ;; They are passed in newly allocated list.
     ;; Note: Having `rest-start` as well as `min` allows us to
     ;; share code for (f x ...) and (f x ...+).
     (define (inline-prim/variadic sym ae1 min [rest-start min])
       (define n (length ae1))
       (when (< n min) (error 'primapp "too few arguments: ~a" sym))
       (define aes       (AExpr* ae1))
       (define mandatory (take aes rest-start))
       (define rest-args (build-rest-args (drop aes rest-start)))
       `(call ,($ sym) ,@mandatory ,rest-args))

     ;; Allocate a sequence container, fill it from `aes`, and return the container.
     (define (build-seq aes local-name local-type alloc set finish)
       (define n    (length aes))
       (define init (alloc n))
       (define tmp  (emit-fresh-local local-name local-type init))
       `(block (result (ref eq))
               ,@(for/list ([ae aes] [i (in-naturals)])
                   (set (Reference tmp) i (AExpr ae)))
               ,(finish (Reference tmp))))


     (define sym (syntax->datum (variable-id pr)))
     (define work
       (case sym
         ;;; Special Inlining
         [(void)
          (define (AE ae)   (AExpr3 ae <effect>))
          (define (AE* aes) (map AE aes))
          `(block (result (ref eq))
                  ,@(AE* ae1)
                  (global.get $void))]
         [(list) 
          (build-rest-args (AExpr* ae1))]
         [(list*)
          (let loop ([aes (AExpr* ae1)])
            (match aes
              [(list v)            v]
              [(list v1 v2)       `(call $list* ,v1 ,v2)]
              [(list* v0 v1 vs)   `(call $list* ,v0 ,(loop (cons v1 vs))) ]))]
         [(values) ; variadic
          (define n   (length ae1))
          (define aes (AExpr* ae1))
          (case n
            [(1)   (first aes)]
            [else  `(array.new_fixed $Values ,n ,@aes)])]

        ;;; Standard Inlining
        [(+)                         (inline-prim/variadic sym ae1 0)]
        [(*)                         (inline-prim/variadic sym ae1 0)]
        [(-)                         (inline-prim/variadic sym ae1 1)]
        [(/)                         (inline-prim/variadic sym ae1 1)]
        [(s-exp->fasl) ; 1 to 2 arguments (in the keyword-less version in "core.rkt"
          (inline-prim/optional sym ae1 1 2)]
         [(fasl->s-exp)                (inline-prim/fixed sym ae1 1)]

         [(vector-copy!)               (inline-prim/optional sym ae1 3 5)]
         [(string-copy!)               (inline-prim/optional sym ae1 3 5)]
         [(bytes-copy!)                (inline-prim/optional sym ae1 3 5)]

         [(make-vector)                (inline-prim/optional sym ae1 1 2)]
         [(make-string)                (inline-prim/optional sym ae1 1 2)]
         [(make-bytes)                 (inline-prim/optional sym ae1 1 2)]
         
         [(substring)                  (inline-prim/optional sym ae1 2 3)]
         [(subbytes)                   (inline-prim/optional sym ae1 2 3)]
         [(vector-copy)                (inline-prim/optional sym ae1 1 3)] ; "subvector"

         [(procedure-rename)           (inline-prim/optional sym ae1 2 3)]
         [(procedure-arity-includes?)  (inline-prim/optional/default sym ae1 2  3 (Imm #f))]
         [(make-hasheq)                (inline-prim/optional sym ae1 0 1)]
         [(number->string)             (inline-prim/optional/default sym ae1 1  2 (Imm #f))]
         [(string->number)             (inline-prim/optional sym ae1 1 5)]
         [(make-struct-type)           (inline-prim/optional/default sym ae1 4 11 (Imm #f))]
         [(make-struct-field-accessor) (inline-prim/optional/default sym ae1 2  5 (Imm #f))]
         [(make-struct-field-mutator)  (inline-prim/optional/default sym ae1 2  5 (Imm #f))]
         [(log)                        (inline-prim/optional sym ae1 1 2)]
         ; Todo: map and for-each needs to check that the first argument is a procedure
         [(map)                        (inline-prim/variadic sym ae1 2 1)]
         [(andmap)                     (inline-prim/variadic sym ae1 2 1)]
         [(ormap)                      (inline-prim/variadic sym ae1 2 1)]
         [(for-each)                   (inline-prim/variadic sym ae1 2 1)]
         [(vector-map)                 (inline-prim/variadic sym ae1 2 2)]
         [(vector-map!)                (inline-prim/variadic sym ae1 2 2)]

          [(remove)                     (inline-prim/optional sym ae1 2 3)]
          [(argmax argmin)              (inline-prim/fixed sym ae1 2)]

          [(hash-ref)                   (inline-prim/optional sym ae1 2 3)]
        [(random)                      (inline-prim/optional sym ae1 0 2)]
         [(fx-/wraparound)             (inline-prim/variadic sym ae1 1)]            ; actual arity: 1,2

         [(min max)                      (inline-prim/variadic sym ae1 2)]
         [(flmin flmax unsafe-flmin unsafe-flmax) (inline-prim/variadic sym ae1 1)] ; at least 1
         [(fxmin fxmax unsafe-fxmin unsafe-fxmax) (inline-prim/variadic sym ae1 1)] ; at least 1
         [(gcd lcm)                               (inline-prim/variadic sym ae1 0)] ; at least 0
         
        [(fx= fx< fx> fx<= fx>=)
          ; variadic, at least one argument
          (define n   (length ae1))
          (when (< n 1) (error 'primapp "too few arguments: ~a" sym))
          (define aes  (AExpr* ae1))
          (define $cmp   ($ sym))
          (define $cmp/2 ($ (string->symbol (~a sym "/2"))))
          (case n
            [(1) `(call ,$cmp ,(first aes) (global.get $null))]
            [(2) `(call ,$cmp/2 ,@aes)]
            [else `(call ,$cmp ,(first aes) ,(build-rest-args (rest aes)))])]

        [(char=? char<? char<=? char>? char>=?
                 char-ci=? char-ci<? char-ci<=? char-ci>? char-ci>=?)
          ; variadic, at least one argument
          (define who    sym)
          (define n      (length ae1))
          (when (< n 1)  (error 'primapp "too few arguments: ~a" s))
          (define aes    (AExpr* ae1))
          (define $cmp   ($ sym))
          (define $cmp/2 ($ (string->symbol (~a sym "/2"))))
          (case n
            [(1) `(call ,$cmp   ,(first aes) (global.get $null))] ; type checks 1st argument
            [(2) `(call ,$cmp/2 ,@aes)]
            [else
             (define c0 (first aes))
             (define xs
               (let loop ([aes (rest aes)])
                 (if (null? aes)
                     `(global.get $null)
                     `(struct.new $Pair
                                  (i32.const 0)
                                  ,(first aes)
                                  ,(loop (rest aes))))))
            `(call ,$cmp ,c0 ,xs)])]

        [(bitwise-and bitwise-ior bitwise-xor)
         (define aes (AExpr* ae1))
         (when (null? aes) (error 'primapp "too few arguments: ~a" sym))
         (define $op (case sym
                        [(bitwise-and) '$bitwise-and/2]
                        [(bitwise-ior) '$bitwise-ior/2]
                        [(bitwise-xor) '$bitwise-xor/2]))
         (let loop ([aes aes])
           (match aes
             [(list)
              (case sym
                [(bitwise-and) (Imm -1)]
                [else           '(global.get $zero)])]
             [(list ae0) ae0]
             [(list* ae0 ae1 aes*)
              `(call ,$op (call ,$op ,ae0 ,ae1) ,(loop aes*))]))]

        [(fxand fxior fxxor)
         (define aes (AExpr* ae1))
         (define $op (case sym
                        [(fxand) '$fxand/2]
                        [(fxior) '$fxior/2]
                        [(fxxor) '$fxxor/2]))
         (let loop ([aes aes])
           (match aes
             [(list)
              (case sym
                [(fxand) (Imm -1)]
                [else      '(global.get $zero)])]
             [(list ae0) ae0]
             [(list* ae0 ae1 aes*)
             `(call ,$op (call ,$op ,ae0 ,ae1) ,(loop aes*))]))]

        [(append)
         (let loop ([aes (AExpr* ae1)])
           (match aes
             [(list)        '(global.get $null)]
             [(list v)      v]
             [(list v1 v2)  `(call $append/2 ,v1 ,v2)]
             [(list* vs)    `(call $append ,(build-rest-args aes))]))]

        [(string-append)
         (let loop ([aes (AExpr* ae1)])
           (match aes
              [(list)        '(global.get $string:empty)]
              [(list v)      `(call $string-copy ,v)]
              [(list v1 v2)  `(call $string-append/2 ,v1 ,v2)]
              [(list* vs)    `(call $string-append ,(build-rest-args aes))]) )]
         [(string-append-immutable) ; variadic, at least zero arguments
          (inline-prim/variadic sym ae1 0)]

         [(bytes-append)
          (let loop ([aes (AExpr* ae1)])
            (match aes
              [(list)        `(global.get $bytes:empty)]
              [(list v)      `(call $bytes-copy ,v)]
              [(list v1 v2)  `(call $bytes-append/2 ,v1 ,v2)]
              [(list* vs)    `(call $bytes-append ,(build-rest-args aes))]) )]

         ;;; Arrays : bytes, string, vector, vector-immutable
         [(bytes)
          #;(build-seq aes local-name local-type alloc set finish)
          (build-seq ae1 'quoted-bytes '(ref $Bytes)
                     ; Allocate 
                     (λ (n) `(ref.cast (ref $Bytes)
                                       (call $make-bytes ,(Imm n) ,(Imm 0))))
                     ; Initialize
                     (λ ($bs i v)
                        `(call $bytes-set!/checked ,$bs (i32.const ,i)
                               (i32.shr_s (i31.get_s ,v) (i32.const 1))))
                     ; Finish
                     (λ ($bs) $bs))]
         [(string)
          (build-seq ae1 'cp-array '(ref eq)
                     (λ (n) `(array.new $I32Array (i32.const 0) (i32.const ,n)))
                     (λ ($a i v)
                        `(array.set $I32Array
                                    (ref.cast (ref $I32Array) ,$a)
                                    (i32.const ,i)
                                    (i32.shr_s
                                     (i31.get_s
                                      (ref.cast (ref i31)
                                                (call $char->integer ,v)))
                                     (i32.const 1))))
                     (λ ($a)
                        `(struct.new $String
                                     (i32.const 0) ; hash
                                     (i32.const 0) ; mutable
                                     (ref.cast (ref $I32Array) ,$a))))]          
         [(vector vector-immutable)
          (build-seq ae1 'quoted-vector-array '(ref $Array)
                     (λ (n)       `(call $make-array (i32.const ,n) ,(Imm 0)))
                     (λ ($a i v)  `(array.set $Array ,$a (i32.const ,i) ,v))
                     (λ ($a)      `(struct.new $Vector
                                               (i32.const 0) ; hash
                                               (i32.const
                                                ,(if (eq? sym 'vector-immutable)
                                                     1 0))
                                               ,$a)))]
         
         [else
          (match (length ae1)
            [0 (case sym
                 [(-)        '(global.get $zero)]
                 [(fx+ fx-)  '(global.get $zero)]
                 [(fl+ fl-)  '(global.get $flzero)]
                 [(fx*)      '(global.get $one)]
                 [(fl*)      '(global.get $flone)]
                 [else       `(call ,(Prim pr))])]
            [1 (case sym
                 [(fx+ fx*)  (AExpr (first ae1))]
                 [(fl+ fl*)  (AExpr (first ae1))]
                 [(fx-)      `(call ,(Prim pr) (global.get $zero)   ,(AExpr (first ae1)))]
                 [(fl-)      `(call ,(Prim pr) (global.get $flzero) ,(AExpr (first ae1)))]
                 [(fx/)      `(call ,(Prim pr) (global.get $one)    ,(AExpr (first ae1)))]
                 [(fl/)      `(call ,(Prim pr) (global.get $flone)  ,(AExpr (first ae1)))]
                 [else       `(call ,(Prim pr)                      ,(AExpr (first ae1)))])]
            [2 (case sym
                 [(-)           `(call ,(Prim pr)
                                       ,(AExpr (first ae1)) ,(AExpr (second ae1)))]
                 [(fx+ fx- fx*) `(call ,(Prim pr)
                                      ,(AExpr (first ae1)) ,(AExpr (second ae1)))]
                 [(fl+ fl- fl*) `(call ,(Prim pr)
                                      ,(AExpr (first ae1)) ,(AExpr (second ae1)))]
                 ; / needs to signal an Racket error if denominator is zero
                 [else   `(call ,(Prim pr)
                                ,(AExpr (first ae1)) ,(AExpr (second ae1)))])]
            [_ (case sym
                 [(fx+ fl+
                     fx* fl*
                     - fx- fl-) ; (+ a b c ...) = (+ (+ a b) c ...)
                  (let loop ([aes (AExpr* ae1)])
                    (match aes
                      [(list)              `(global.get $zero)]
                      [(list  ae0)         ae0]
                      [(list* ae0 ae1 aes) `(call ,(Prim pr)
                                                  (call ,(Prim pr) ,ae0 ,ae1)
                                                  ,(loop aes))]))]
                 [else
                  ; TODO - INSERT ARITY CHECK
                  `(call ,(Prim pr) ,@(AExpr* ae1))])])]))
     (match dd
       [(or '<value> '<effect>)
        (match cd
          ['<return>             `(return ,work)]
          ['<expr>                work]
          ['<stat>                `(drop ,work)]
          [_ (error)])]
       [x  (Store! x work)])]

    ; This version doesn't repack when it invokes a variadic function.
    [(app ,s ,ae ,ae1 ...)
     ; Note: We do not know that type `ae` has.
     ;       We need to check that is an applicable value.
     
     ; TODO: Only one function application is active at a time, so we can
     ;       reuse the variable, $app-clos,  holding the function to be applied.

     (define tc (eq? cd '<return>))

     ;; Build user-argument vector (no header, no repack)
     (define args `(array.new_fixed $Args ,(length ae1)
                                    ,@(for/list ([ae ae1]) (AExpr3 ae <value>))))

     ;; Hold the callee as a (ref $Procedure) 
     (define proc-var (new-var '$app-proc))
     (define proc (syntax-e (variable-id proc-var)))
     (emit-local proc '(ref $Procedure) '(global.get $dummy-closure)) ; really $dummy-procedure

     ;; The dynamic invoker for the procedure
     (define inv `(struct.get $Procedure $invoke (local.get ,proc)))

     ;; Tail-call the invoker with raw args
     (define work
       (if tc
           `(return_call_ref $ProcedureInvoker (local.get ,proc) ,args ,inv)
           `(call_ref        $ProcedureInvoker (local.get ,proc) ,args ,inv)))

     ;; Cast helper (add checks later if you want nice errors)
     (define (cast w) `(ref.cast (ref $Procedure) ,w))

     (match dd
       ['<effect>
        (match cd
          ['<expr> `(block (local.set ,proc ,(cast (AExpr ae))) ,work)]
          ['<stat> `(block (local.set ,proc ,(cast (AExpr ae))) (drop ,work))]
          [_ (error 'internal-app0 "combination impossible")])]
       ['<value>
        (match cd
          ['<return> `(block (result (ref eq))
                             (local.set ,proc ,(cast (AExpr ae)))
                             ,(if tc work `(return ,work)))]
          ['<expr>   `(block (result (ref eq))
                             (local.set ,proc ,(cast (AExpr ae)))
                             ,work)]
          [_ (error 'internal-app1 "not impossible?!")])]
       [x `(block (local.set ,proc ,(cast (AExpr ae)))
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
                        [else              (list `(result (ref eq)))])  ; <--
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
     ;; (displayln (list 'letrec-values (map list x** ce*) e) (current-error-port))
     ;; (displayln (list 'dd dd)                              (current-error-port))

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
          (for/list ([ae (AExpr* ae1)]
                     [i  (in-naturals)])
            `(array.set $Free
                        (struct.get $Closure $free (ref.cast (ref $Closure) ,(Reference dd)))
                        (i32.const ,i)
                        ,ae))]))

     ; declare the `x` as a local variables
     (for ([x* x**])
       (for ([x x*])
         (emit-local x)))
     
     (let* ([x* (map first x**)]                     ; assumes single value
            [u* (for/list ([x* x**]) (Undefined))]
            [e  (Expr e dd cd)])                     
       `(block ,@(match dd                               ; <--
                   ['<effect> (list)]
                   ['<value>  (list '(result (ref eq)))]
                   [x         (list)])
               ,@(for/list ([x x*] [u u*])
                   (Store! x u))
               ,@(map AllocateClosure ce* x*) 
               ,@(append* (map FillClosure ce* x*))
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
     #;(displayln `(define-values ,s (,x ...)   ,e))
     (match x
       ['()       (Expr e <effect> <stat>)]
       ; Until we implement namespaces, top-level variables are
       ; stored as boxed global variables.
       [(list x0) (Expr e x0 <stat>)]                  
       [(list x ...)  (define vals (emit-fresh-local 'vals '(ref eq) '(global.get $false)))
                      `(block
                        ,(Expr e vals <stat>)
                        ,@(for/list ([x0 x] [i (in-naturals)])
                            `(global.set ,($$ (variable-id x0))
                                         (array.get $Values
                                                    (ref.cast (ref $Values)
                                                              (local.get ,($$ (variable-id vals))))
                                                    (i32.const ,i)))))])]
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

  ;;;
  ;;; Integrating Host Runtime and Program
  ;;;

  ; The idea is to generate everything program specific here,
  ; and pass it along to `generate-host-runtime` which inserts
  ; the program specific parts into the runtime.

  
  (define (generate-global-declarations-for-top-level-variables)
    (for/list ([x top-vars])
      `(global ,(Var x) (mut (ref eq)) ,(Undefined))))
  
  (define result #'$result) ; holds result in $entry

  (define-values (dls tms entry-body) (TopLevelForm T result))
  (define top-level-variable-declarations
    (generate-global-declarations-for-top-level-variables))

  ; variables bound by let-values and others at the top-level
  ; are represented in the runtime as local variables of a function `entry`.
  (define entry-locals (*locals*))
  
  (generate-runtime
   dls                              ; define-labels
   tms                              ; top modules
   entry-body                       ; expressions and general top-level forms
   result                           ; variable that holds the result (in $entry)
   ; program specific
   top-vars                         ; top level variables (list of variables)
   top-level-variable-declarations  ; wasm code for declaring top-level variables
   entry-locals                     ; variables that are local to $entry
   ; general
   primitives                       ; primitives (list of symbols)
   string-constants                 ; (list (list name string) ...)
   bytes-constants                  ; (list (list name bytes) ...)
   symbol-constants                 ; (list (list name symbol) ...)
   )
  )

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

(define (strip* x)
  ; (displayln x)
  (cond
    [(variable? x)   (syntax-e (variable-id x))]
    [(pair? x)       (cons (strip* (car x))
                           (strip* (cdr x)))]
    [(null? x)      '()]
    [(syntax? x)    (syntax-e x)]
    [(datum? x)     (datum-value x)]
    
    [else           x]))

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
               (let ([t (topexpand stx)])
                 ; (displayln (pretty-print (syntax->datum t)))
                 t))))))))))))))

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
          (equal? (run '(char=? #\a)) #t)
          (equal? (run '(char=? #\a #\a)) #t)
          (equal? (run '(char=? #\a #\b)) #f)
          (equal? (run '(char=? #\a #\a #\a)) #t)
          (equal? (run '(char=? #\a #\b #\a)) #f)
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
    (list  (equal? (run '(let ([f (lambda (x) x)]) (f 12))) 12)
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
  (define (test-multiple-values)
    (and (equal? (run '(let-values ([() (values)]) 3)) 3)
         (equal? (run '(let-values ([(a) 11]) a)) 11)
         (equal? (run '(let-values ([(a b) (values 11 22)]) (+ a b))) 33)
         (equal? (run '(let-values ([(a b) (values 11 22)]
                                    [(c d) (values 100 200)]) (+ a b c d))) 333)
         (equal? (run '(let ()
                         (define (f) (values 11 22))
                         (let-values ([(a b) (f)]) (+ a b))))  33)
         (equal? (run '(let ()                         
                         (let-values ([(a b)
                                       (if 1 (values 11 22) (values 11 22 33))])
                           (+ a b))))
                 33)))
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
    (and  (equal? (run '(let loop () 11)) 11)
          (equal? (run '(let loop ([x 5]) (if (= x 0) 42 (loop (- x 1))))) 42)
          (equal? (run '(let loop ([n 5] [sum 0]) (if (zero? n) sum (loop (- n 1) (+ sum n))))) 15)))
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
  (define (test-fasl)
    (list
     ;; fixnums
     (equal? (run '(fasl->s-exp (s-exp->fasl 42))) 42)
     (equal? (run `(fasl->s-exp (s-exp->fasl ,most-positive-fixnum))) most-positive-fixnum)
     (equal? (run `(fasl->s-exp (s-exp->fasl ,most-negative-fixnum))) most-negative-fixnum)
     ;; characters
     (equal? (run '(fasl->s-exp (s-exp->fasl #\a))) #\a)
     (equal? (run '(fasl->s-exp (s-exp->fasl #\λ))) #\λ)
     ;; booleans
     (equal? (run '(fasl->s-exp (s-exp->fasl #t))) #t)
     (equal? (run '(fasl->s-exp (s-exp->fasl #f))) #f)
     ;; symbol
     (equal? (run '(fasl->s-exp (s-exp->fasl 'hello))) 'hello)
     ;; strings
     (equal? (run '(fasl->s-exp (s-exp->fasl "hi"))) "hi")
     (equal? (run '(fasl->s-exp (s-exp->fasl "")))   "")
     (equal? (run '(fasl->s-exp (s-exp->fasl "hé"))) "hé")
     ;; byte strings
     (equal? (run '(fasl->s-exp (s-exp->fasl (bytes 1 2 3)))) (bytes 1 2 3))
     (equal? (run '(fasl->s-exp (s-exp->fasl (bytes))))       (bytes))
     (equal? (run '(fasl->s-exp (s-exp->fasl (bytes 0 255)))) (bytes 0 255))
     ;; null
     (equal? (run '(fasl->s-exp (s-exp->fasl '()))) '())
     ;; pairs
     (equal? (run '(fasl->s-exp (s-exp->fasl '(1 . 2)))) '(1 . 2))
     (equal? (run '(fasl->s-exp (s-exp->fasl '(1 2 3)))) '(1 2 3))
     ;; vectors
     (equal? (run '(fasl->s-exp (s-exp->fasl #(1 2 3)))) #(1 2 3))
     (equal? (run '(fasl->s-exp (s-exp->fasl #())))      #())
     ;; flonums
     (equal? (run '(fasl->s-exp (s-exp->fasl 3.5)))   3.5)
     (equal? (run '(fasl->s-exp (s-exp->fasl -2.0))) -2.0)
     ;; void
     (equal? (run '(fasl->s-exp (s-exp->fasl (void)))) (void))
     ;; eof object
     #;(equal? (run '(fasl->s-exp (s-exp->fasl (eof)))) (eof))
     (equal? (run '(fasl->s-exp (s-exp->fasl (list 11 (vector 22 #\x) 'foo "bar" (list 55)))))
             (list 11 (vector 22 #\x) 'foo "bar" (list 55)))     
     ;; explicit output port
     ; todo: implement `call-with-output-bytes`
     #;(equal? (run '(let ([bs (call-with-output-bytes
                              (lambda (out)
                                (s-exp->fasl '(1 2) out)))])
                     (fasl->s-exp bs)))
             '(1 2))))

  (list "-- Core Constructs --"
        ;; (list "Immediate Values"              (test-immediates))
        ;; (list "Call unary primitive"          (test-call-unary-primitive))
        ;; (list "Some characters "            (test-some-characters)) ; slow
        ;; #;(list "All characters"              (test-all-characters))  ; very slow
        (list "Call binary primitive"         (test-call-binary-primitive))
        (list "Local variables (let)"         (test-let))
        (list "Conditional (if)"              (test-if))
        (list "Sequencing (begin)"            (test-begin))
        (list "Vectors"                       (test-vectors))
        (list "Functions"                     (test-function-declarations))  
        (list "Lambda without free variables" (test-lambda/no-free))
        (list "Lambda - Thunks"               (test-thunks))
        (list "Lambda - Parameter passing"    (test-parameter-passing)) 
        (list "Lambda - Closures"             (test-closures))          
        (list "Lambda"                        (test-lambda))
        (list "Tail calls"                    (test-tail-calls))        
        (list "Quotations"                    (test-quotations))
        (list "Boxes"                         (test-boxes))
        (list "Assignments"                   (test-assignments))      
        (list "Byte strings"                  (test-bytes))
        (list "Strings"                       (test-strings))
        (list "Multiple Values"               (test-multiple-values))
        ;; Tests below require the expander to be present.
        "-- Derived Constructs --"
        #; (list "Letrec"                        (test-letrec))  ;; TODO!
        #; (letrec ((f (lambda (g) (set! f g) (f)))) (f (lambda () 12))) ; assignment to letrec bound variable
        (list "Named let"                     (test-named-let))  ; <-
        (list "And/Or"                        (test-and/or))
        (list "Cond"                          (test-cond))
        (list "When/unless"                   (test-when/unless))
        (list "Begin0"                        (test-begin0))
        (list "Fasl"                          (test-fasl))
        ))
