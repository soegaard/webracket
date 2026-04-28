#lang racket/base
(module+ test (require rackunit))
(provide (all-defined-out))
(require racket/set
         racket/list
         syntax/id-set
         syntax/id-table)

;;; The following primitives are needed for regular expressions.
;;; When they are implemented, reneable "regexp.rkt" in "stdlib.rkt".

;; Done
;   [x] bytes-utf-8-length
;   [x] bytes->string/latin-1
;   [x] char-grapheme-step
;   [x] unquoted-printing-string
;   [x] hash-iterate-first        (stdlib/hash.rkt)
;   [x] hash-iterate-key          (stdlib/hash.rkt)
;   [x] hash-iterate-next         (stdlib/hash.rkt)
;   [x] hash-set                  (stdlib/hash.rkt)
;   [x] raise-arguments-error     (stdlib/expections.rkt)
;   [x] raise-arguments-error*    (stdlib/expections.rkt)
;   [x] raise-range-error*        (stdlib/expections.rkt)
;   [x] raise-result-error        (stdlib/expections.rkt)
;   [x] peek-bytes-avail!
;   [x] peek-bytes-avail!*
;   [x] prop:authentic
;   [x] prop:custom-write
;   [x] prop:equal+hash
;   [x] arity-at-least-value
;   [x] arity-at-least?
;   [x] make-weak-hash
;   [x] make-input-port      ; note: The calls in "regexp.rkt" all have 4 arguments.
;   [x] progress-evt?

;   [x] format/display needs to have formating for hash tables

;; Todo
#;(abort-current-continuation
   call-with-continuation-prompt
   make-continuation-prompt-tag   
   )



(require "expander.rkt"       ; provides topexpand
         "assembler.rkt"
         "priminfo.rkt"       ; information on Racket primitives
         "runtime-wasm.rkt"   ;
         "define-foreign.rkt"
         "parameters.rkt"
         "timings.rkt"
         "wat-identifiers.rkt"
         ; "wasm-data.rkt"
         nanopass/base
         racket/match
         racket/file
         racket/port
         racket/pretty
         (only-in racket/string string-prefix? string-replace)
         (only-in racket/format ~a)
         (only-in racket/list partition append* first second third last
                  index-where append-map make-list rest take drop
                  takef dropf drop-right
                  group-by)         
         (only-in racket/set  list->set set->list set-intersect))
(require
  ; (prefix-in ur- urlang)
 (for-syntax nanopass/base
             syntax/parse
             racket/syntax
             racket/base)
 
  syntax/kerncase ; for kernel-form-identifier-list
  syntax/stx
  racket/syntax
  (except-in syntax/parse str) ; the identifier str is used in the runtime 
  ; (rename-in racket/match [match Match])
  ; (only-in srfi/1 list-index)
  '#%paramz) ; contains the identifier parameterization-key

(define (count-sexp x)
  (cond
    [(pair? x)
     (+ 1 (count-sexp (car x)) (count-sexp (cdr x)))]
    [(vector? x)
     (+ 1 (for/sum ([e (in-vector x)]) (count-sexp e)))]
    [(box? x)
     (+ 1 (count-sexp (unbox x)))]
    [else 1]))

(define (count-unparsed unparse-fn x)
  (count-sexp (unparse-all (unparse-fn x))))


;;; todo - just for testing FFI

  ;; (define ffi-files '("dom.ffi" "standard.ffi"))
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

; [ ] Support $Linket, $CompiledLinklet and $Instance in format/display

; [/] Redefining a primitive on top-level, like `(define (cadr x) (car (cdr x)))`
;     leads to problems. The error is strange (runtime error). Find the underlying problem.
;     For now we simply report an error, that this is not allowed in webracket.

; [x] Find solution for two types of rest arguments.
;     For inlining direct calls, the $Args representation is fine (1 allocation)
;     For `map`, `apply` and others, lists are the natural representation.
;     Support both?
;     All variadic functions must accept the rest arguments as a list.
;     Some also accept them as an $Args - this is used in inlining.

; [/] Consider adding extra shapes to `primitive-invoke`.
;     The possible shapes:
;        (list->set (map (λ (x) (if x (primitive-description-arity x) #f))
;                          (map primitive->description primitives)))
;     - The shape could be precomputed.
; [ ] For primitives not in original Racket, provide arity information.

; [ ] Modules!

; [ ] Keyword arguments
;     [ ] struct:keyword-procedure/arity-error
;     [ ] prop:named-keyword-procedure
;     [ ] missing-kw
;     [x] null

; [x] $vector-immutable - Make the returned vector immutable


; [ ] Input / output port going to the host.

; [x] case-lambda
; [x] Calling conventions for primitives?
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

; [ ] Make a pass that rewrites primapps with primitives that take an
;     optional number of arguments. Fill in the remaining slots with ... missing?
;     Or pass optional arguments in a global variable (caller/callee save)?

; [/] A reference to an undefined, top-level variable must look up the variable
;     in the current namespace.
;     Currently the compiler just reports that classify-variable
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
;         (each primitive `pr` gets a global `$prim:pr`.
;     [x] A reference to `pr` evaluates to `(global.get $prim:pr)`.
;     [ ] Apply can call $invoke-primitive -- but how should $PrimitiveCode
;         look like? We could wrap e.g. `fx+` as (λ (x y) (fx+ x y))
;         and then use `$invoke-closure$ ?

; [ ] use an i32 to hold the arity in $Procedure


; [x] generating string and symbol constants for use in the runtime

; [ ] Implement guards for structs.

; [x] String ports.

; [ ] Parameters.

; [x] Synchronize primitives between compiler and the `webracket` language.


; [ ] Hash tables
; [x] - mutable hasheq  (eq?)    tables
; [x] - mutable hasheqv (eqv?)   tables
; [x] - mutable hash    (equal?) tables
; [ ] - immutable hash tables


; [ ] Sets

; [ ] Numbers
;      - bignums, rationals, exact, inexact 
; [x] Floating points.
;      - log, exp, expt, sin, cos, ...
; [ ] Implement a proper flonum printing algorithm.


;; [ ] Compiler warnings for call with wrong arity:
;;     Example of a call with wrong arity:
;;       (define text
;;         (case-lambda
;;           [(string)               (text string '() 12)]
;;           [(string style)         (text string style 12)]
;;           [(string style size)    (text string style size 0)]


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
  ; map f over xs while threading the second value  
  (define (f* xs ρ)
    (match xs
      ['()         (values '() ρ)]
      [(cons x xs) (letv ((x ρ) (f x ρ))
                     (letv ((xs ρ) (f* xs ρ))
                       (values (cons x xs) ρ)))]))
  (f* xs ρ))


;; Added in new versions of Racket. See docs.
(define (slice-by proc lst)
  ;; Build slices by scanning adjacent pairs. If (proc x y) holds, keep extending
  ;; the current slice; otherwise, close the slice and start a new one at y.
  (let loop ([xs lst] [curr '()] [acc '()])
    (match xs
      ['()
       (reverse (if (null? curr) acc (cons (reverse curr) acc)))]
      [(list x)
       (reverse (cons (reverse (cons x curr)) acc))]
      [(cons x (cons y rest))
       (define new-curr (cons x curr))
       (if (proc x y)
           (loop (cons y rest) new-curr acc)
           (loop (cons y rest) '()
                 (cons (reverse new-curr) acc)))])))

;;;
;;; VARIABLES
;;;

(require "structs.rkt")

; Representation of variables during compilation.
;     (struct variable (id) #:transparent)
; id is an identifier (i.e. a syntax-object)

(define (unparse-variable x)
  (syntax->datum (variable-id x)))

(define current-top-symbols-needing-defined-check (make-parameter (make-hasheq)))
(define current-print-top-level-results? (make-parameter #f))
(define print-top-level-results-sentinel-symbol
  'webracket-print-top-level-results-sentinel)

(define letrec-initialization-set-key 'webracket:letrec-initialization-set?)

(define (record-top-symbol-needing-defined-check! x)
  (define sym (syntax-e (variable-id x)))
  (hash-set! (current-top-symbols-needing-defined-check) sym #t))

(define (top-symbol-needs-defined-check? x)
  (hash-has-key? (current-top-symbols-needing-defined-check)
                 (syntax-e (variable-id x))))

;;; Quick and dirty sets of variables represented as free-id sets
(define (variable=? x y) (free-identifier=? (variable-id x) (variable-id y)))

(struct id-set (ids entries) #:transparent)

(define empty-set (id-set '() (immutable-free-id-set)))
(define (id-set->list s) (id-set-ids s))

(define (set-in? x s)
  (free-id-set-member? (id-set-entries s) (variable-id x)))

(define (set-add s x)
  (if (set-in? x s)
      s
      (id-set (cons x (id-set-ids s))
              (free-id-set-add (id-set-entries s) (variable-id x)))))

(define (ids->id-set xs)
  (for/fold ([s empty-set]) ([x xs]) (set-add s x)))

(define (make-id-set . xs)
  (ids->id-set xs))

(define (set-union s1 s2)
  (for/fold ([s s1]) ([x (in-list (id-set-ids s2))]) (set-add s x)))

(define (set-union* ss)
  (for/fold ([u empty-set]) ([s ss]) (set-union u s)))

(define (set-remove s x)
  (if (set-in? x s)
      (id-set (remove x (id-set-ids s) variable=?)
              (free-id-set-remove (id-set-entries s) (variable-id x)))
      s))

(define (set-difference s1 s2)
  (for/fold ([s s1]) ([x (in-list (id-set-ids s2))]) (set-remove s x)))

(define (set-intersection s1 s2)
  (for/fold ([s empty-set]) ([x (in-list (id-set-ids s1))] #:when (set-in? x s2))
    (set-add s x)))

(define (set-empty? s)
  (null? (id-set-ids s)))

(define (set-disjoint? s1 s2)
  (set-empty? (set-intersection s1 s2)))

;;;
;;; Environment for the α-renaming pass
;;;

(struct rename-env (lexical by-name fallback) #:transparent)

;; rename-env-variable : (or/c variable? identifier?) -> variable?
;;   Normalize alpha-rename lookup inputs to variables.
(define (rename-env-variable x)
  (cond
    [(variable? x)   x]
    [(identifier? x) (variable x)]
    [else            (error 'rename-env-variable "expected identifier or variable, got ~a" x)]))

;; rename-env-name-key : (or/c variable? identifier?) -> (or/c symbol? #f)
;;   Compute the printed-name key used for alpha-renaming name occupancy checks.
(define (rename-env-name-key x)
  (define datum (syntax-e (variable-id (rename-env-variable x))))
  (and (symbol? datum) datum))

;; rename-env-fallback-key : variable? -> (or/c symbol? #f)
;;   Compute the printed-name fallback key for non-lexical identifiers.
(define (rename-env-fallback-key x)
  (define id (variable-id (rename-env-variable x)))
  (define b  (identifier-binding id))
  (and (not (eq? b 'lexical))
       (syntax-e id)))

;; ρ-empty : -> rename-env?
;;   Construct an empty alpha-renaming environment.
(define (ρ-empty)
  (rename-env (make-immutable-free-id-table) #hasheq() #hasheq()))

;; ρ-ref : rename-env? variable? -> (or/c variable? #f)
;;   Look up x in rho using lexical identity first and fallback name matching second.
(define (ρ-ref ρ x)
  (define v       (rename-env-variable x))
  (define lexical (free-id-table-ref (rename-env-lexical ρ) (variable-id v) #f))
  (cond
    [lexical lexical]
    [else
     (define key    (rename-env-name-key v))
     (define bucket (if key
                        (hash-ref (rename-env-by-name ρ) key '())
                        '()))
     (define by-name (for/or ([entry (in-list bucket)])
                       (and (id=? v (car entry))
                            (cdr entry))))
     (or by-name
         (and key
              (hash-ref (rename-env-fallback ρ) key #f)))]))

;; ρ-name-used? : rename-env? (or/c variable? identifier?) -> boolean?
;;   Check whether rho already contains some binding with x's printed name.
(define (ρ-name-used? ρ x)
  (define key (rename-env-name-key x))
  (and key
       (pair? (hash-ref (rename-env-by-name ρ) key '()))))

;; ρ-set : rename-env? variable? variable? -> rename-env?
;;   Extend rho with a mapping from original to renamed.
(define (ρ-set ρ original renamed)
  (define original*    (rename-env-variable original))
  (define lexical      (free-id-table-set (rename-env-lexical ρ)
                                          (variable-id original*) renamed))
  (define key          (rename-env-name-key original*))
  (define by-name      (if key
                           (hash-set (rename-env-by-name ρ)
                                     key
                                     (cons (cons original* renamed)
                                           (hash-ref (rename-env-by-name ρ) key '())))
                           (rename-env-by-name ρ)))
  (define fallback-key (rename-env-fallback-key original*))
  (rename-env lexical
              by-name
              (if fallback-key
                  (hash-set (rename-env-fallback ρ) fallback-key renamed)
                  (rename-env-fallback ρ))))

;; ρ-set* : rename-env? (listof variable?) -> rename-env?
;;   Extend rho by mapping each variable in xs to itself.
(define (ρ-set* ρ xs)
  (for/fold ([ρ ρ]) ([x (in-list xs)])
    (ρ-set ρ x x)))

(module+ test
  (let ()
    ;; mkvar : symbol? -> variable?
    ;;   Construct a fresh variable wrapper for symbol s.
    (define (mkvar s)
      (variable (datum->syntax #f s)))

    ;; set-size : id-set? -> exact-nonnegative-integer?
    ;;   Number of variables stored in id-set s.
    (define (set-size s)
      (length (id-set->list s)))
    ;; set-has-name-string? : id-set? string? -> boolean?
    ;;   Check whether some variable in s has printed symbol name name.
    (define (set-has-name-string? s name)
      (for/or ([v (in-list (id-set->list s))])
        (define x (syntax-e (variable-id v)))
        (and (symbol? x)
             (string=? (symbol->string x) name))))
    ;; set-has-name-symbol/eq? : id-set? symbol? -> boolean?
    ;;   Name membership check using eq? on symbols (intentionally strict).
    (define (set-has-name-symbol/eq? s sym)
      (for/or ([v (in-list (id-set->list s))])
        (define x (syntax-e (variable-id v)))
        (and (symbol? x) (eq? x sym))))

    (define x  (mkvar 'x))
    (define x2 (mkvar 'x))
    (define y  (mkvar 'y))
    (define z  (mkvar 'z))

    (check-true (set-empty? empty-set))
    (check-equal? (set-size empty-set) 0)

    ;; Deduplicate same free identifier.
    (define sx (set-add empty-set x))
    (define sxx (set-add sx x2))
    (check-true (set-in? x sx))
    (check-true (set-in? x2 sx))
    (check-equal? (set-size sx) 1)
    (check-equal? (set-size sxx) 1)

    ;; Basic add/remove operations.
    (define sxy (set-add sx y))
    (check-true (set-in? x sxy))
    (check-true (set-in? y sxy))
    (check-equal? (set-size sxy) 2)
    (define sy (set-remove sxy x))
    (check-false (set-in? x sy))
    (check-true (set-in? y sy))
    (check-equal? (set-size sy) 1)
    (check-equal? (set-size (set-remove sy z)) 1)

    ;; make-id-set / ids->id-set dedupe.
    (define s/make (make-id-set x x2 y))
    (define s/ids (ids->id-set (list x x2 y)))
    (check-equal? (set-size s/make) 2)
    (check-equal? (set-size s/ids) 2)
    (check-true (set-in? x s/make))
    (check-true (set-in? y s/make))

    ;; Union, intersection, difference, disjoint.
    (define sxz (make-id-set x z))
    (define syz (make-id-set y z))
    (define su (set-union sxz syz))
    (check-equal? (set-size su) 3)
    (check-true (set-in? x su))
    (check-true (set-in? y su))
    (check-true (set-in? z su))
    (check-equal? (set-size (set-union sxz sxz)) 2)

    (define si (set-intersection sxz syz))
    (check-equal? (set-size si) 1)
    (check-true (set-in? z si))
    (check-false (set-in? x si))

    (define sd (set-difference su syz))
    (check-equal? (set-size sd) 1)
    (check-true (set-in? x sd))
    (check-false (set-in? y sd))
    (check-false (set-in? z sd))

    (check-true (set-disjoint? (make-id-set x) (make-id-set y)))
    (check-false (set-disjoint? (make-id-set x z) (make-id-set y z)))

    ;; Union over many sets.
    (define su* (set-union* (list (make-id-set x) (make-id-set y) (make-id-set z x2))))
    (check-equal? (set-size su*) 3)
    (check-true (set-in? x su*))
    (check-true (set-in? y su*))
    (check-true (set-in? z su*))

    ;; Regression guard: printed-name lookup is robust for uninterned symbols.
    (define error.59/uninterned (mkvar (string->uninterned-symbol "error.59")))
    (define se (set-add empty-set error.59/uninterned))
    (check-true (set-has-name-string? se "error.59"))
    (check-false (set-has-name-symbol/eq? se 'error.59))))


;;;
;;; DATUMS AND CONSTANTS
;;;

; Representation of datums during compilation.
; Quotation using quote creates a datum.
; Constants are literals ("selfquoting").
; Parse will introduce explicit quotes for constants.

(struct datum (stx value)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc d port mode)
     ;; delegate to Racket's printer for the value field
     (write (datum-value d) port))])
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

(define datum:undefined        the-undefined-value) 
(define datum:unsafe-undefined the-unsafe-undefined-value)

;;;
;;; CONSTANTS
;;;

; See also this list:
;    https://gist.github.com/jesboat/1859af07d6d7b8521bf40b59a551fd79

(require (only-in racket/math pi))

(define constants '(null             ; '() racket/base
                    undefined        ;     racket/undefined
                    unsafe-undefined ;     racket/unsafe/undefined
                    empty            ; '() racket/list
                    true             ;     racket/bool (not racket/base)
                    false            ;     racket/bool (not racket/base)
                    pi               ;     racket/math
                    eof              ;     racket/base
                    ))
(define (constant-value c)
  (case c
    [(null empty)       '()]
    [(true)             #t]
    [(false)            #f]
    [(undefined)        datum:undefined]
    [(unsafe-undefined) datum:unsafe-undefined]
    [(pi)               pi]
    [(eof)              eof]
    [else
     (error 'constant-value "got: ~a" c)]))

(define non-literal-constants
  '(prop:arity-string
    prop:checked-procedure  
    prop:impersonator-of
    prop:incomplete-arity
    prop:method-arity-error
    prop:object-name
    prop:procedure
    prop:authentic
    prop:custom-write
    prop:equal+hash

    struct:exn
    struct:exn:fail
    struct:exn:fail:contract
    struct:exn:fail:contract:arity
    struct:exn:fail:contract:divide-by-zero
    struct:exn:fail:contract:non-fixnum-result
    struct:exn:fail:contract:variable
    struct:exn:fail:read
    struct:exn:fail:read:eof
    struct:exn:fail:read:non-char
    struct:exn:fail:filesystem
    struct:exn:fail:syntax
    struct:exn:fail:syntax:missing-module
    struct:exn:fail:syntax:unbound
    ))
(define non-literal-constants-set (list->seteq non-literal-constants))

(define (non-literal-constant? x)
  (set-member? non-literal-constants-set x))



;;;
;;; PRIMITIVES
;;;

;; For some primitives we need an easy way to construct a
;; variable reference to the primitive.

(define primitives '())  ; includes the ffi-primitives
(define primitives-set (mutable-seteq))

(define primitive-arity-cache (make-hasheq))

(define (ffi-primitive-arity sym)
  (and (ffi-primitive? sym)
       (let ([f (ffi-foreign-by-name sym)])
         (and f (length (foreign-argument-types f))))))

(define (primitive-arity sym)
  (hash-ref! primitive-arity-cache sym
             (λ ()
               (or (ffi-primitive-arity sym)
                   (let ([desc (primitive->description sym)])
                     (and desc (primitive-description-arity desc)))))))

(define (arity-accepts? arity n)
  (cond
    [(arity-at-least? arity) (>= n (arity-at-least-value arity))]
    [(integer? arity)        (= n arity)]
    [(list? arity)           (for/or ([sub arity]) (arity-accepts? sub n))]
    [else                    #t]))

(define (primitive-arity-accepts? sym n)
  (define arity (primitive-arity sym))
  (or (not arity) (arity-accepts? arity n)))


;;;
;;; Inlining
;;;

(struct primitive-inline-spec
  (name kind min max rest-start default)
  #:transparent)

;; primitive-inline-spec-accepted-counts : primitive-inline-spec? -> (listof exact-nonnegative-integer?)
;;   Concrete arities covered by spec when the range is finite; otherwise #f.
(define (primitive-inline-spec-accepted-counts spec)
  (define min (primitive-inline-spec-min spec))
  (define max (primitive-inline-spec-max spec))
  (and max
       (for/list ([n (in-range min (+ max 1))]) n)))

;; primitive-inline-spec-covers? : primitive-inline-spec? exact-nonnegative-integer? -> boolean?
;;   Check whether n is accepted by spec's declarative arity shape.
(define (primitive-inline-spec-covers? spec n)
  (define min (primitive-inline-spec-min spec))
  (define max (primitive-inline-spec-max spec))
  (and (<= min n)
       (or (not max) (<= n max))))

;; primitive-inline-spec-covers-arity? : primitive-inline-spec? arity? -> boolean?
;;   Check whether spec covers the full primitive arity shape.
;;   That is, the inline spec must include the entire set of argument counts
;;   that the primitive may legally receive.
(define (primitive-inline-spec-covers-arity? spec arity)
  (cond
    [(arity-at-least? arity)
     (and (not (primitive-inline-spec-max spec))
          (<= (primitive-inline-spec-min spec)
              (arity-at-least-value arity)))]
    [(integer? arity)
     (primitive-inline-spec-covers? spec arity)]
    [(list? arity)
     (for/and ([sub (in-list arity)])
       (primitive-inline-spec-covers-arity? spec sub))]
    [else
     #t]))

(define primitive-inline-priminfo-skip-set
  (seteq 'fx-/wraparound
         'unsafe-fx-/wraparound
         'get-output-bytes
         'unsafe-fx=
         'unsafe-fx<
         'unsafe-fx>
         'unsafe-fx<=
         'unsafe-fx>=
         'unsafe-fxand
         'unsafe-fxior
         'unsafe-fxxor
         'apply
         'peek-bytes
         'peek-string))

;; primitive-inline-spec-valid? : primitive-inline-spec? -> void?
;;   Validate one declarative inlining spec for internal consistency.
(define (primitive-inline-spec-valid? spec)
  (define who 'primitive-inline-spec)
  (define kind       (primitive-inline-spec-kind spec))
  (define name       (primitive-inline-spec-name spec))
  (define min        (primitive-inline-spec-min spec))
  (define max        (primitive-inline-spec-max spec))
  (define rest-start (primitive-inline-spec-rest-start spec))
  (define default    (primitive-inline-spec-default spec))
  (unless (symbol? name)
    (error who "expected primitive name symbol, got ~a" name))
  (unless (exact-nonnegative-integer? min)
    (error who "invalid minimum arity for ~a: ~a" name min))
  (when (and max (not (exact-nonnegative-integer? max)))
    (error who "invalid maximum arity for ~a: ~a" name max))
  (when (and max (> min max))
    (error who "invalid arity range for ~a: ~a..~a" name min max))
  (case kind
    [(fixed)
     (unless (and max (= min max))
       (error who "fixed spec for ~a must have min=max, got ~a..~a" name min max))
     (when rest-start
       (error who "fixed spec for ~a cannot have rest-start" name))]
    [(optional variadic variadic-args)
     (when default
       (error who "~a spec for ~a cannot have a default" kind name))
     (when (and (memq kind '(variadic variadic-args)) max)
       (error who "~a spec for ~a cannot have a finite max" kind name))]
    [(optional/default)
     (unless default
       (error who "optional/default spec for ~a requires a default" name))]
    [(optional-rest)
     (unless max
       (error who "optional-rest spec for ~a requires a finite max" name))
     (unless (exact-nonnegative-integer? rest-start)
       (error who "optional-rest spec for ~a requires rest-start" name))]
    [else
     (error who "unknown spec kind for ~a: ~a" name kind)])
  (when (and rest-start max (> rest-start max))
    (error who "rest-start after maximum arity for ~a: ~a > ~a" name rest-start max))
  (define desc-arity (and (not (set-member? primitive-inline-priminfo-skip-set name))
                          (primitive-arity name)))
  (when desc-arity
    (for ([n (in-list (or (primitive-inline-spec-accepted-counts spec) (list min)))])
      (unless (arity-accepts? desc-arity n)
        (error who
               "spec arity for ~a is not accepted by priminfo: ~a"
               name
               n)))
    (unless (primitive-inline-spec-covers-arity? spec desc-arity)
      (error who
             "spec arity for ~a does not cover priminfo arity: ~a"
             name
             desc-arity)))
  (void))

;; build-primitive-inline-spec-table : (listof primitive-inline-spec?) -> hasheq?
;;   Validate specs and build a lookup table keyed by primitive symbol.
(define (build-primitive-inline-spec-table specs)
  (define ht (make-hasheq))
  (for ([spec (in-list specs)])
    (primitive-inline-spec-valid? spec)
    (define name (primitive-inline-spec-name spec))
    (when (hash-has-key? ht name)
      (error 'build-primitive-inline-spec-table
             "duplicate primitive inline spec for ~a"
             name))
    (hash-set! ht name spec))
  ht)

(define (make-inline-specs names kind min max [rest-start #f] [default #f])
  (for/list ([name (in-list names)])
    (primitive-inline-spec name kind min max rest-start default)))

(define (make-inline-specs/by-name names kind min-proc max-proc
                                   [rest-start-proc (λ (_name) #f)]
                                   [default-proc (λ (_name) #f)])
  (for/list ([name (in-list names)])
    (primitive-inline-spec name
                           kind
                           (min-proc name)
                           (max-proc name)
                           (rest-start-proc name)
                           (default-proc name))))

(define primitive-inline-specs
  (append
   (make-inline-specs/by-name
    ;; Variadic
    ;; - names
    '(list*
      symbol<? keyword<? path<?
      string=? string<?  string<=?  string>?  string>=?  string-ci=?
      string-ci<?  string-ci<=?  string-ci>?  string-ci>=?
      + * - /
      fx-/wraparound     
      unsafe-fx+ unsafe-fx* unsafe-fx- unsafe-fx-/wraparound
      unsafe-fxand unsafe-fxior unsafe-fxxor       
      unsafe-flmin unsafe-flmax unsafe-fxmin unsafe-fxmax            
      append* apply compose compose1 cartesian-product vector-count map andmap ormap
      append-map count for-each foldl foldr vector-map vector-map! filter-map      
      min max flmin flmax fxmin fxmax gcd lcm
      make-instance instance-set-variable-value! instance-variable-value
      string-append-immutable string-append* bytes-append*)
    ; - kind
    'variadic
    ; - min-proc
    (λ (name)
      (case name
        [(+ * unsafe-fx+ unsafe-fx* compose compose1 cartesian-product string-append-immutable gcd lcm) 0]
        [(- / unsafe-fx- fx-/wraparound unsafe-fx-/wraparound min max
             flmin flmax unsafe-flmin unsafe-flmax
             fxmin fxmax unsafe-fxmin unsafe-fxmax
             append* string-append* bytes-append* list*) 1]
        [(unsafe-fxand unsafe-fxior unsafe-fxxor) 1]
        [(symbol<? keyword<? path<?
                   string=? string<? string<=? string>? string>=?
                   string-ci=? string-ci<? string-ci<=? string-ci>? string-ci>=?) 1]
        [(apply) 2]
        [(foldl foldr instance-set-variable-value!) 3]
        [(make-instance) 1]
        [else 2]))
    ; - max-proc
    (λ (_name) #f)
    ; - rest-start-proc
    (λ (name)
      (case name
        [(apply list*) 1]
        [(vector-count map andmap ormap append-map count for-each vector-map vector-map! filter-map
                       instance-variable-value) 2]
        [(foldl foldr instance-set-variable-value!) 3]
        [(+ * unsafe-fx+ unsafe-fx* compose compose1 cartesian-product string-append-immutable gcd lcm) 0]
        [else 1])))
   (make-inline-specs '(= < > <= >=) 'variadic-args 1 #f 1)
   (make-inline-specs '(build-path/convention-type) 'variadic 2 #f 2)
   (append
    (make-inline-specs
     '(unsafe-fxnot
       unsafe-fxpopcount
       unsafe-fxpopcount16
       unsafe-fxpopcount32
       fasl->s-exp
       get-output-bytes
       get-output-string
       call-with-output-string
       port-closed?
       close-input-port
       close-output-port
       struct-type-property?
       struct-type-property-accessor-procedure?
       struct-type-authentic?
       instance-name
       instance-data
       instance-variable-names)
     'fixed 1 1)
    (make-inline-specs
     '(symbol=?
       unsafe-fx=
       unsafe-fx<
       unsafe-fx>
       unsafe-fx<=
       unsafe-fx>=
       call-with-exception-handler
       vector-filter
       vector-filter-not
       vector-argmax
       vector-argmin
       assw
       assv
       assq
       assf
       memf
       findf
       index-where
       indexes-where
       take
       take-right
       takef
       takef-right
       drop
       drop-right
       dropf
       dropf-right
       argmax
       argmin
       hash-filter
       hash-filter-keys
       hash-filter-values
       unsafe-fx+/wraparound
       unsafe-fx*/wraparound
       unsafe-fxlshift/wraparound
       unsafe-fxlshift
       unsafe-fxrshift
       unsafe-fxrshift/logical
       unsafe-fxquotient
       unsafe-fxremainder
       unsafe-fxmodulo
       instance-unset-variable!
       bytes-join)
     'fixed 2 2))
   (append
    (make-inline-specs/by-name
     '(vector-copy!
       string-copy!
       bytes-copy!
       substring
       subbytes
       string-utf-8-length
       bytes->string/utf-8
       bytes->string/latin-1
       bytes-utf-8-length
       vector-copy
       vector->values
       vector-extend
       procedure-rename
       string->number
       floating-point-bytes->real
       log
       atan
       assoc
       group-by
       member
       remove
       remove*
       index-of
       indexes-of
       list-prefix?
       take-common-prefix
       drop-common-prefix
       split-common-prefix
       hash-ref
       hash-update!
       random
       flrandom
       unsafe-flrandom
       datum->syntax
       read-bytes!
       read-bytes-avail!
       read-bytes-avail!*
       read-string!
       peek-bytes!
       peek-string!
       peek-bytes
       peek-string
       progress-evt?
       write-byte
       write-char
       write-bytes
       write-string
       make-vector
       make-string
       make-bytes
       vector-sort!
       vector-sort
       struct->vector
       real->floating-point-bytes
       string-trim
       string-split)
     'optional
     (λ (name)
       (case name
         [(vector-copy! string-copy! bytes-copy!) 3]
         [(substring subbytes vector-extend procedure-rename datum->syntax) 2]
         [(peek-bytes! peek-string!) 2]
         [(string-trim string-split) 1]
         [(assoc group-by member remove remove* index-of indexes-of
                 list-prefix? take-common-prefix drop-common-prefix split-common-prefix
                 hash-ref real->floating-point-bytes vector-sort! vector-sort
                 ) 2]
         [(hash-update!) 3]
         [(random flrandom unsafe-flrandom) 0]
        [(progress-evt? write-byte write-char) 1]
         [(write-bytes write-string) 1]
         [else 1]))
     (λ (name)
       (case name
         [(vector-copy! string-copy! bytes-copy!) 5]
         [(substring subbytes vector-copy vector->values vector-extend
                       procedure-rename hash-ref) 3]
         [(string-utf-8-length) 3]
         [(bytes->string/utf-8 bytes->string/latin-1 bytes-utf-8-length) 4]
         [(string->number datum->syntax) 5]
         [(floating-point-bytes->real) 4]
         [(log atan struct->vector) 2]
         [(assoc group-by member remove remove* index-of indexes-of
                 list-prefix? take-common-prefix drop-common-prefix split-common-prefix) 3]
         [(read-bytes! read-bytes-avail! read-bytes-avail!* read-string!
                       write-bytes write-string) 4]
         [(peek-bytes! peek-string!) 5]
         [(peek-bytes peek-string) 3]
         [(progress-evt? make-vector make-string make-bytes) 2]
         [(random) 2]
         [(flrandom unsafe-flrandom) 1]
         [(vector-sort! vector-sort) 4]
         [(real->floating-point-bytes) 5]
         [(hash-update!) 4]
         [(string-trim) 5]
         [(string-split) 4]
         [else 2])))
    (make-inline-specs
     '(s-exp->fasl
       open-input-string
       open-input-bytes
       read-bytes
       read-string
       normalize-path
       simplify-path
       make-directory)
     'optional 1 2)
    (make-inline-specs
     '(file->bytes
       file->string)
     'optional 1 2)
    (make-inline-specs
     '(file->lines)
     'optional 1 3)
    (make-inline-specs
     '(file->bytes-lines)
     'optional 1 3)
    (make-inline-specs
     '(open-input-file)
     'optional 1 3)
    (make-inline-specs
     '(call-with-input-file
       call-with-input-file*
       with-input-from-file)
     'optional 2 3)
    (make-inline-specs
     '(open-output-file)
     'optional 1 3)
    (make-inline-specs
     '(make-temporary-file)
     'optional 0 3)
    (make-inline-specs
     '(make-temporary-directory)
     'optional 0 2)
    (make-inline-specs
     '(make-temporary-file*)
     'optional 2 4)
    (make-inline-specs
     '(make-temporary-directory*)
     'optional 2 3)
    (make-inline-specs
     '(display-to-file)
     'optional 2 4)
    (make-inline-specs
     '(display-lines-to-file)
     'optional 2 5)
    (make-inline-specs
     '(call-with-output-file
       call-with-output-file*
       with-output-to-file)
     'optional 2 4)
    (make-inline-specs
     '(open-output-bytes
       open-output-string
       current-continuation-marks
       gensym
       make-hasheq
       make-hasheqv
       make-hash
       make-hashalw
       make-weak-hasheq
       make-weak-hasheqv
       make-weak-hash
       make-weak-hashalw
       byte-ready?
       char-ready?
       read-byte
       read-char
       newline
       flush-output
       current-input-port
       current-output-port
       current-error-port
       current-directory
       current-directory-for-user)
     'optional 0 1)
    (make-inline-specs
     '(raise-read-error
       raise-read-eof-error)
     'optional 6 7)
    (make-inline-specs
     '(read-line peek-byte peek-char)
     'optional 0 2)
    (make-inline-specs
     '(directory-list)
     'optional 0 2)
    (make-inline-specs
     '(file-or-directory-modify-seconds)
     'optional 1 2)
    (make-inline-specs
     '(file-or-directory-permissions)
     'optional 1 2)
    (make-inline-specs
     '(file-or-directory-stat)
     'optional 1 2)
    (make-inline-specs
     '(file-or-directory-identity)
     'optional 1 2)
    (make-inline-specs
     '(file-or-directory-type)
     'optional 1 2)
    (make-inline-specs
     '(delete-directory/files)
     'optional 1 2)
    (make-inline-specs
     '(path-add-extension)
     'optional 2 3)
    (make-inline-specs
     '(find-relative-path)
     'optional 2 5)
    (make-inline-specs
     '(copy-file rename-file-or-directory)
     'optional 2 3)
    (make-inline-specs
     '(copy-directory/files)
     'optional 2 4))
   (make-inline-specs
    '(build-path)
    'variadic 1 #f 1)
   (append
    (make-inline-specs/by-name
     '(raise
       procedure-arity-includes?
       procedure-reduce-arity
       number->string
       struct-type-property-predicate-procedure?
       hash->list
       hash-keys
       hash-values
       hash-for-each
       hash-map
       hash-map/copy
       instantiate-linklet
       string-join
       string->bytes/utf-8
       path->complete-path
       bytes->path
       string->path-element
       bytes->path-element
         datum->correlated
         correlated-property
         inclusive-range
         inclusive-range-proc
         struct->list
       make-struct-type
       make-struct-field-accessor
       make-struct-field-mutator
       make-struct-type-property
       string-replace)
     'optional/default
     (λ (name)
       (case name
         [(raise hash->list hash-keys hash-values
                 string-join string->bytes/utf-8 bytes->path
                 string->path-element bytes->path-element datum->correlated
                 struct-type-property-predicate-procedure?) 1]
         [(procedure-arity-includes? procedure-reduce-arity) 2]
         [(number->string) 1]
         [(hash-for-each hash-map hash-map/copy correlated-property) 2]
         [(instantiate-linklet inclusive-range inclusive-range-proc) 2]
         [(struct->list make-struct-type-property) 1]
         [(make-struct-type) 4]
         [(make-struct-field-accessor make-struct-field-mutator) 2]
         [(string-replace) 3]
         [else 1]))
     (λ (name)
       (case name
         [(raise number->string
                 struct-type-property-predicate-procedure? hash->list hash-keys
                 hash-values string-join bytes->path path->complete-path) 2]
         [(string->path-element) 2]
         [(bytes->path-element) 3]
         [(procedure-arity-includes?) 3]
         [(procedure-reduce-arity) 4]
         [(string->bytes/utf-8) 4]
         [(hash-for-each hash-map hash-map/copy) 3]
         [(instantiate-linklet) 4]
         [(datum->correlated correlated-property inclusive-range inclusive-range-proc) 3]
         [(struct->list) 2]
         [(make-struct-type) 11]
         [(make-struct-field-accessor make-struct-field-mutator) 5]
         [(make-struct-type-property) 7]
         [(string-replace) 4]
         [else 3]))
     (λ (_name) #f)
     (λ (name)
       (case name
         [(string-replace) 'true]
         [(struct->list) 'symbol:error]
         [(string-join) 'string:space]
         [(procedure-reduce-arity bytes->path path->complete-path string->path-element
                       bytes->path-element datum->correlated correlated-property
                       inclusive-range inclusive-range-proc) 'missing]
         [else 'false])))
   (make-inline-specs/by-name
    '(make-input-port
      peek-bytes-avail!
      peek-bytes-avail!*)
    'optional-rest
    (λ (name)
      (case name
        [(make-input-port) 4]
        [else 2]))
    (λ (name)
      (case name
        [(make-input-port) 10]
        [else 6]))
    (λ (name)
      (case name
        [(make-input-port) 4]
        [else 1])))))
  )

(define primitive-inline-spec-table #f)

(define (ensure-primitive-inline-spec-table!)
  (or primitive-inline-spec-table
      (let ([ht (build-primitive-inline-spec-table primitive-inline-specs)])
        (set! primitive-inline-spec-table ht)
        ht)))

(define (primitive-inline-spec-ref sym)
  (hash-ref (ensure-primitive-inline-spec-table!) sym #f))

(module+ test
  (check-equal? (primitive-inline-spec-kind (primitive-inline-spec-ref 'substring))
                'optional)
  (check-equal? (primitive-inline-spec-default (primitive-inline-spec-ref 'string-join))
                'string:space)
  (check-exn exn:fail?
             (λ ()
               (build-primitive-inline-spec-table
                (list (primitive-inline-spec 'demo 'fixed 1 1 #f #f)
                      (primitive-inline-spec 'demo 'fixed 1 1 #f #f)))))
  (check-exn exn:fail?
             (λ ()
               (primitive-inline-spec-valid?
                (primitive-inline-spec 'demo 'optional/default 1 2 #f #f))))
  (check-exn exn:fail?
             (λ ()
               (primitive-inline-spec-valid?
                (primitive-inline-spec 'procedure-arity-includes? 'fixed 2 2 #f #f))))
  (check-false
   (primitive-inline-spec-covers-arity?
    (primitive-inline-spec 'demo 'fixed 2 2 #f #f)
    (arity-at-least 1)))
  (check-true
   (primitive-inline-spec-covers-arity?
    (primitive-inline-spec 'demo 'variadic 1 #f 1 #f)
    (arity-at-least 1))))

;;;
;;; Primitives
;;;


;; Most primitives are either primitives or procedures in standard Racket.
;; We can therefore use reflection to look up information about arities,
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
           (set-add! primitives-set 'name)
           (define (var:name) (variable #'name)))))]))

(define-syntax (define-primitives stx)
  (syntax-parse stx
    [(_define-primitives name ...)
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

  ; in-list
  
  catch   ; single predicate and handler
  catch*  ; multiple predicates and handlers
  call-with-exception-handler
  
  raise
  raise-unbound-variable-reference
  ; raise-argument-error
  raise-read-error
  raise-read-eof-error

  exn
  exn?
  exn-message
  exn-continuation-marks
  make-exn
  
  exn:fail  ; no new fields (compared to `exn`)
  exn:fail?
  make-exn:fail

  exn:fail:contract
  exn:fail:contract?
  make-exn:fail:contract
  
  exn:fail:contract:arity
  exn:fail:contract:arity?
  make-exn:fail:contract:arity
  
  exn:fail:contract:divide-by-zero
  exn:fail:contract:divide-by-zero?
  make-exn:fail:contract:divide-by-zero

  exn:fail:contract:non-fixnum-result
  exn:fail:contract:non-fixnum-result?
  make-exn:fail:contract:non-fixnum-result
  
  exn:fail:contract:variable
  exn:fail:contract:variable?
  make-exn:fail:contract:variable  
  exn:fail:contract:variable-id

  exn:fail:read
  exn:fail:read?
  make-exn:fail:read
  exn:fail:read-srclocs

  exn:fail:read:eof
  exn:fail:read:eof?
  make-exn:fail:read:eof

  exn:fail:read:non-char
  exn:fail:read:non-char?
  make-exn:fail:read:non-char

  exn:fail:filesystem
  exn:fail:filesystem?
  make-exn:fail:filesystem

  exn:fail:syntax
  exn:fail:syntax?
  make-exn:fail:syntax
  exn:fail:syntax-exprs

  exn:fail:syntax:missing-module
  exn:fail:syntax:missing-module?
  make-exn:fail:syntax:missing-module
  exn:fail:syntax:missing-module-path

  exn:fail:syntax:unbound
  exn:fail:syntax:unbound?
  make-exn:fail:syntax:unbound
  
  

  ; checkers
  check-list
  check-mlist
  check-range
  check-range-generic
  check-naturals
  check-string
  check-vector
  
  ; structures
  make-struct-type
  make-struct-field-accessor
  make-struct-field-mutator
  make-struct-type-property

  struct-constructor-procedure?
  struct-predicate-procedure?
  struct-accessor-procedure?
  struct-mutator-procedure?
  struct?
  struct-type?
  struct-type-property?
  struct-type-property-accessor-procedure?
  struct-type-property-predicate-procedure?
  struct-type-authentic?
  
  struct->list
  struct->vector

  current-inspector          ; todo
  
  values                     
  

  boxed      ; used by assignment elimination
  unboxed    ; used by assignment elimination
  set-boxed! ; used by assignment elimination
  initialize-boxed! ; used by letrec-values initialization after assignment elimination
  boxed?

  box? box box-immutable unbox set-box! 

  mpair? mcons mcar mcdr set-mcar! set-mcdr!
  
  vector-immutable  ; used in datum construction
  bytes             ; used in datum construction
  string            ; used in datum construction

  
  ; 4.10.1 Pair constructors and selectors
  pair?
  null?
  cons car cdr  
  list?
  list
  list*
  build-list
  ; 4.10.2 List Operations
  length
  list-ref
  list-tail
  append ; variadic list primitive
  
  reverse alt-reverse ; used in expansion of for/list
  ; 4.10.3 List Iteration
  map
  andmap
  ormap
  for-each
  foldl
  foldr
  
  ; 4.10.4 More List Iteration
  ; running-foldl
  ; running-foldr
  ; 4.10.5 List Filtering
  filter
  remove  remq  remv  remw  remf
  remove* remq* remv* remw* remf*
  sort
  ; 4.10.6 List Searching
  member  memq  memv memw   memf
  findf  
  assoc assw assv assq assf

  ; 4.10.7 Pair Accessor Shorthands
  caar  cadr  cdar  cddr
  caaar caadr cadar caddr
  cdaar cdadr cddar cdddr
  caaaar caaadr caadar caaddr
  cadaar cadadr caddar cadddr
  cdaaar cdaadr cdadar cdaddr
  cddaar cddadr cdddar cddddr

  ; 4.10.8 Additional List Functions and Synonyms
  cons?
  empty?
  first rest
  second third fourth fifth sixth seventh eighth ninth tenth
  eleventh twelfth thirteenth fourteenth fifteenth
  last last-pair
  make-list
  list-update
  list-set
  index-of index-where indexes-of indexes-where
  take  take-right
  takef takef-right
  
  drop 
  dropf
  drop-right
  dropf-right

  split-at  split-at-right
  splitf-at splitf-at-right

  list-prefix?
  take-common-prefix
  drop-common-prefix
  split-common-prefix
  add-between ; simplified
  append*
  flatten
  ; check-duplicates
  ; remove-duplicates
  filter-map
  append-map
  count
  partition
  range           range-proc
  inclusive-range inclusive-range-proc  
  filter-not shuffle
  ; combinations
  ; in-combinations
  permutations
  ; in-permutations
  argmin argmax
  group-by
  cartesian-product
  
  ; 4.10.9 More List Grouping
  ; windows
  ; slice-by

  ; 4.10.10 Immutable Cyclic Data

  mutable-hash-iterate-first
  mutable-hash-iterate-next
  mutable-hash-iterate-key
  mutable-hash-iterate-value
  mutable-hash-iterate-pair
  mutable-hash-iterate-key+value
  
  
  ; 4.21 Void
  void?
  make-void  ; zero arguments
  void

  ;; BOOLEANS
  boolean? boolean=? false? not xor immutable?
  mutable-string? immutable-string?
  mutable-bytes?  immutable-bytes?
  mutable-vector? immutable-vector?
  mutable-box?    immutable-box?
  mutable-hash?   immutable-hash?

  ;; CHARACTERS
  char?
  char->integer
  integer->char
  char-utf-8-length
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
  char-alphabetic?
  char-lower-case?
  char-upper-case?
  char-title-case?
  char-numeric?
  char-symbolic?
  char-punctuation?
  char-graphic?
  char-whitespace?
  char-grapheme-break-property
  char-general-category
  char-blank?
  char-iso-control?
  char-extended-pictographic?

  char-grapheme-step

  eq?
  eqv?
  equal?
  equal-always?

  number->string
  string->number
  floating-point-bytes->real

  + - * / quotient remainder modulo quotient/remainder
  = < > <= >=
  zero? positive? negative? even? odd?
  add1 sub1 gcd lcm

  number?
  real?
  integer?
  exact? exact-integer?
  exact-nonnegative-integer?
  exact-positive-integer?
  nan? infinite?
  positive-integer? negative-integer?
  nonpositive-integer? nonnegative-integer?
  natural?
  inexact-real?
  inexact?
  inexact->exact
  exact->inexact
  real->double-flonum
  exact-round exact-floor exact-ceiling exact-truncate
  round floor ceiling truncate
  sin  cos  tan  asin  acos  atan
  sinh cosh tanh asinh acosh atanh
  degrees->radians radians->degrees
  order-of-magnitude
  abs sgn max min sqr sqrt integer-sqrt integer-sqrt/remainder expt exp log
  real->floating-point-bytes

  bitwise-ior bitwise-and bitwise-xor bitwise-not bitwise-bit-set?
  bitwise-first-bit-set  ; note : added in 8.16
  bitwise-bit-field
  arithmetic-shift
  integer-length
  random
  system-big-endian?

  fixnum? fxzero?
  fx+ fx- fx*
  fx= fx> fx< fx<= fx>=
  fxmin fxmax

  fxquotient 
  fxremainder fxmodulo fxabs
  fxand fxior fxxor fxnot fxlshift fxrshift
  fxpopcount fxpopcount16 fxpopcount32
  fx+/wraparound fx-/wraparound fx*/wraparound fxlshift/wraparound
  fxrshift/logical
  most-positive-fixnum most-negative-fixnum

  fx->fl
  fl->fx
  fixnum-for-every-system?

  flonum?
  double-flonum?
  single-flonum?
  single-flonum-available?
  fl+ fl- fl* fl/
  fl= fl< fl> fl<= fl>=
  flabs flround flfloor flceiling fltruncate flsingle
  fllog flexp flsqrt 
  flsin flcos fltan flasin flacos flatan
  flsinh flcosh fltanh flasinh flacosh flatanh
  flmin flmax flexpt ->fl fl->exact-integer
  flrandom
  flbit-field

  byte?

  vector 
  vector? make-vector vector-ref vector-set! vector-length  
  vector-fill! vector-copy! vector-empty?
  vector->list list->vector vector->values vector->immutable-vector
  vector-copy vector-map vector-map! vector-argmax vector-argmin
  vector-append vector-extend vector-count
  vector-filter vector-filter-not
  vector-member vector-memq vector-memv
  vector-sort! vector-sort
  vector-drop vector-drop-right
  vector-take vector-take-right
  vector-split-at vector-split-at-right
  build-vector
  vector-set/copy 
  
  bytes?  make-bytes  bytes-ref  bytes-set!  bytes-length  subbytes bytes-copy!
  bytes-copy bytes-fill!
  bytes-append bytes-append* bytes-join
  bytes->immutable-bytes
  bytes->string/utf-8
  bytes->string/latin-1
  bytes-utf-8-length
  bytes->list list->bytes 
  bytes=? bytes<? bytes>?

  string?
  string=?    string<?    string<=?    string>?    string>=?
  string-ci=? string-ci<? string-ci<=? string-ci>? string-ci>=?
  make-string build-string string-ref string-set! string-length substring
  string-copy!
  string-copy string-fill!
  string-upcase string-downcase string-titlecase string-foldcase
  string-append string-append-immutable string-append* string-join
  string->list list->string
  string->bytes/utf-8 string-utf-8-length string->immutable-string
  non-empty-string?
  string-split
  string-trim

  string-take        ; not in Racket
  string-take-right  ; not in Racket
  string-drop        ; not in Racket
  string-drop-right  ; not in Racket
  string-trim-left   ; not in Racket
  string-trim-right  ; not in Racket
  string-suffix?     ; from racket/string
  string-prefix?     ; from racket/string
  string-contains?   ; from racket/string
  string-find        ; from racket/string  (added in 8.15)
  string-replace     ; from racket/string 

  symbol? symbol=? symbol<? 
  string->symbol symbol->string
  string->uninterned-symbol symbol-interned?
  symbol->immutable-string
  gensym
  
  eof-object?

  port?
  input-port?
  output-port?  
  port-closed?
  current-input-port
  current-output-port
  current-error-port
  reset-current-input-port!
  reset-current-output-port!
  reset-current-error-port!
  close-input-port
  close-output-port
  flush-output
  string-port?
  
  open-input-bytes
  open-input-string  
  open-output-bytes
  open-output-string
  get-output-bytes
  get-output-string

  call-with-output-string
  make-input-port

  read-byte
  read-char
  read-bytes!
  read-string!
  read-bytes
  read-string
  read-bytes-avail!
  read-bytes-avail!*
  peek-bytes-avail!
  peek-bytes-avail!*
  progress-evt?

  byte-ready?
  char-ready?
  read-line
  peek-bytes!
  peek-string!
  peek-bytes
  peek-string
  peek-byte
  peek-char
  
  write-byte
  write-char
  newline
  write-bytes
  write-string

  
  port-next-location
  port-count-lines!
  port-counts-lines?

  s-exp->fasl
  fasl->s-exp

  make-empty-hasheq  ; not in Racket
  make-empty-hasheqv ; not in Racket
  make-empty-hash    ; not in Racket
  make-empty-hashalw ; not in Racket
  
  make-hasheq
  make-hasheqv
  make-hash
  make-hashalw

  make-weak-hasheq  ; for now same as make-hasheq
  make-weak-hasheqv ; for now same as make-hasheqv
  make-weak-hash    ; for now same as make-hash
  make-weak-hashalw ; for now same as make-hashalw

  hash?
  hash-eq?
  hash-eqv?
  hash-equal?
  hash-equal-always?
  
  hash-ref
  hash-ref!
  hash-set!
  hash-update!
  hash-remove!
  hash-clear!
  hash-has-key?
  hash-empty?
  hash-count
  hash->list
  hash-for-each
  hash-map
  hash-map/copy
  hash-keys
  hash-values
  hash-filter
  hash-filter-keys
  hash-filter-values
  
  eq-hash-code
  eqv-hash-code
  equal-hash-code
  

  keyword?
  keyword->string
  keyword->immutable-string
  string->keyword
  keyword<?

  apply
  compose
  compose1
  procedure-rename
  procedure?
  procedure->external
  procedure-arity
  procedure-arity-mask
  procedure-arity-includes?
  procedure-reduce-arity
  arity-at-least
  make-arity-at-least
  arity-at-least?
  arity-at-least-value

  primitive?
  primitive-closure?
  primitive-result-arity

  variable-reference-from-unsafe?
  variable-reference-constant?

  js-log

  ;; 10. Control Flow
  call-with-values

  ; 10.2 Exceptions
  unquoted-printing-string?
  unquoted-printing-string
  unquoted-printing-string-value
  
  make-srcloc
  srcloc
  srcloc?
  srcloc-source
  srcloc-line
  srcloc-column
  srcloc-position
  srcloc-span

  srcloc->string

  ;; 10.5 Continuation Marks
  current-continuation-marks  ; dummy, always returns #f

  ;; 12.2 Syntax Object Content
  syntax?
  syntax-e
  
  syntax-source
  syntax-line
  syntax-column
  syntax-position
  syntax-span

  identifier?

  datum->syntax
  syntax->datum
  syntax->list
  
  ; make-syntax
  ; syntax
  ; syntax-scopes
  ; syntax-shifted-multi-scopes
  ; syntax-props
  ; empty-props

  syntax-srcloc  ; in racket/syntax-srcloc

  ;; 13.8 Printer Extension
  custom-write?
  custom-write-accessor
  
  ;; 14.9 Structure Inspectors
  object-name
  ; prop:object-name  (see non-literal-constants)

  ;; 14.14 Linklets and the Core Compiler
  correlated?
  correlated-source
  correlated-line
  correlated-column
  correlated-position
  correlated-span
  correlated-e
  correlated->datum
  datum->correlated
  correlated-property
  correlated-property-symbol-keys

  make-instance
  instance?
  instance-name
  instance-data
  instance-variable-names
  instance-set-variable-value!
  instance-unset-variable!
  instance-variable-value
  instance-variable-box      ; internal in full Racket

  linklet?
  linklet-body-reserved-symbol?
  linklet-bundle?
  hash->linklet-bundle
  linklet-bundle->hash
  linklet-directory?
  hash->linklet-directory
  linklet-directory->hash
  make-compiled-linklet
  linklet-name
  linklet-import-variables
  linklet-export-variables
  instantiate-linklet
  
  ;; 15.1 Paths
  path?
  path-for-some-system?
  path-string?
  string->path
  path->bytes
  path->string
  some-system-path->string
  path<?
  path-convention-type
  system-path-convention-type
  bytes->path
  string->path-element
  bytes->path-element
  string->some-system-path
  absolute-path?
  relative-path?
  complete-path?
  build-path
  build-path/convention-type
  current-directory
  current-directory-for-user
  path->complete-path
  path->directory-path
  cleanse-path
  simplify-path
  resolve-path
  normal-case-path
  path-replace-extension
  path-add-extension
  path-replace-suffix
  path-add-suffix
  reroot-path
  path-get-extension
  filename-extension
  path-has-extension?
  file-name-from-path
  split-path
  explode-path
  path-element?
  path-element->bytes
  path-element->string
  simple-form-path
  normalize-path
  find-relative-path
  shrink-path-wrt
  path-only

  ;; 15.2 Filesystem
  file-exists?
  directory-exists?
  link-exists?
  file-or-directory-type
  directory-list
  filesystem-root-list
  find-system-path
  current-drive
  file-size
  delete-file
  make-directory
  make-directory*
  make-parent-directory*
  make-temporary-file
  make-temporary-directory
  make-temporary-file*
  make-temporary-directory*
  delete-directory
  delete-directory/files
  rename-file-or-directory
  copy-file
  copy-directory/files
  file-or-directory-modify-seconds
  file-or-directory-permissions
  file-or-directory-stat
  file-or-directory-identity
  file->bytes
  file->string
  file->lines
  file->bytes-lines
  open-input-file
  open-output-file
  display-to-file
  display-lines-to-file
  call-with-input-file
  call-with-output-file
  call-with-input-file*
  call-with-output-file*
  with-input-from-file
  with-output-to-file
  webracket-vfs-write-file
  
  ;; 17. Unsafe Operations
  unsafe-fx+ unsafe-fx- unsafe-fx* unsafe-fl/
  unsafe-fxquotient unsafe-fxremainder unsafe-fxmodulo
  unsafe-fxabs
  unsafe-fxand unsafe-fxior unsafe-fxxor unsafe-fxnot
  unsafe-fxpopcount unsafe-fxpopcount16 unsafe-fxpopcount32
  unsafe-fxlshift unsafe-fxrshift unsafe-fxrshift/logical

  unsafe-fx+/wraparound unsafe-fx-/wraparound unsafe-fx*/wraparound
  unsafe-fxlshift/wraparound

  unsafe-fx= unsafe-fx< unsafe-fx> unsafe-fx<= unsafe-fx>=
  unsafe-fxmin unsafe-fxmax
  
  unsafe-flabs unsafe-flround unsafe-flfloor unsafe-flceiling unsafe-fltruncate
  unsafe-flsingle unsafe-flsin unsafe-flcos unsafe-fltan unsafe-flasin
  unsafe-flacos unsafe-flatan
  unsafe-flsinh unsafe-flcosh unsafe-fltanh unsafe-flasinh unsafe-flacosh unsafe-flatanh
  unsafe-fllog unsafe-flexp unsafe-flsqrt
  unsafe-flmin unsafe-flmax unsafe-flexpt
  unsafe-flrandom

  unsafe-car
  unsafe-cdr
  unsafe-struct-ref
  unsafe-vector-length
  unsafe-vector*-length
  unsafe-vector-ref
  unsafe-vector*-ref
  unsafe-vector-set!
  unsafe-vector*-set!
  unsafe-struct-set!

  unsafe-string-length

  unsafe-bytes-length
  unsafe-bytes-ref
  unsafe-bytes-set!

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

  ;;; match runtime support
  syntax-srclocs ; in racket/match/runtime.rkt
  match:error
  )

;;;
;;; FFI Primitives
;;;

;; An FFI Primitive is a primitive that is defined in an .ffi file.

(define ffi-primitives '()) ; list of symbols
(define ffi-primitives-set (mutable-seteq))

(define (ffi-primitive? sym)
  (set-member? ffi-primitives-set sym))

(define (ffi-foreign-by-name sym)
  (for/first ([ff (in-list (current-ffi-foreigns))]
              #:when (eq? (foreign-racket-name ff) sym))
    ff))

(define (define-ffi-primitive name)
  ; 1. There is no `var:name` since `var:name` is only used
  ;    in "compiler.rkt" to generate code.
  (set!     primitives (cons name     primitives))
  (set-add! primitives-set name)
  (set! ffi-primitives (cons name ffi-primitives))
  (set-add! ffi-primitives-set name))

;; Primitives declared using ffi-files.

(define (define-ffi-primitives names)
  (for-each define-ffi-primitive names))

(define (reset-ffi-primitives)
  ; called by `parse` before parsing begins
  (hash-clear! primitive-arity-cache)
  (set-clear! ffi-primitives-set)
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
           (set-member? primitives-set v))
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
   ((variable      (x xd))    . => . unparse-variable)
   ((datum         (d))       . => . unparse-datum)
   ((modname       (mn))      . => . unparse-module-name)
   ((modpath       (mp))      . => . unparse-module-path)
   (integer        (ei))
   (syntax         (s)))
  (Formals (f)
    (formals (x ...))            => (x ...)
    (formals (x0 x1 ... . xd))   => (x0 x1 ... . xd)
    (formals x)                  => x)
  (TopLevelForm (t)
    ; turns out it is best keep unique tags (Expr also has a begin)
    (topbegin s t ...)           => (topbegin t ...)
    (topmodule s mn mp mf ...)   => (module mn mp (#%plain-module-begin mf ...))
    (#%expression s e)           => (#%expression e)
    (begin-for-syntax s t ...)   => (topbegin-for-syntax t ...)
    g)
  (ModuleLevelForm (mf)
    (#%provide rps ...)           => (#%provide rps ...)
    ; (modbegin-for-syntax ...) todo
    ; (#%declare ...)           todo
    g)  
  ; (SubModuleLevelForm ...) todo
  (GeneralTopLevelForm (g)
    e
    (define-values   s (x ...) e)  => (define-values   (x ...) e)
    (define-syntaxes s (x ...) e)  => (define-syntaxes (x ...) e)
    (#%require s rrs ...)          => (#%require s rrs ...))

  ; (RawRequireSpec     (rrs) rrmp) ; todo
  (RawRequireSpec (rrs) ; todo: there are more types of require specs
    (for-meta pl rrs ...) => (for-meta pl rrs ...)
    ps) ; note: was rrmp 
  (PhaseLevel (pl)
    (no-phase)   ; #f in concrete syntax
    ei)
  (PhaselessSpec     (ps)  
    #;(for-space  space sls ...)
    #;(just-space space sls ...)
    sls) ; spaceless-spec
  (Space (space)
    x   ; identifier
    (no-space))  ; #f in concrete syntax
  (RawProvideSpec (rps) 
    #;(for-meta   phase-level ps ...)
    #;(for-syntax             ps ...)
    #;(for-label              ps ...)
    #;(protect                rps ...)
    ps)  
  (SpacelessSpec (sls)
    #;(rename local-id export-id)
    #;(struct ...)
    #;(all-from ...)
    #;(all-from-except ...)
    #;(all-defined)
    #;(all-defined-except ...)
    #;(prefix-all-defined prefix-id)
    #;(prefix-all-defined-except ...)
    #;(protect ...)
    #;(expand (id . datum))
    #;(expand (id . datum) orig-form)
    x) ; id

  
  ;(RawModulePath     (rmp) rrmp) ; todo
  (RawRootModulePath (rrmp) 
    (quote x))

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
;;; Unexpand
;;;

;; The `unexpand` pass is from fully expanded syntax to fully expanded syntax.

;; Tasks like Task 1 are in fragile. If full Racket changes the expansion
;; of the `with-handlers` form, then `unexpand` needs to be rewritten.
;; Therefore, if possible avoid to extend `unexpand` if possible.


;; Task 1
;; ------
;; For now it rewrites the expansion of `with-handlers` into a
;; a use of `catch*` instead.

;; Real Racket uses continuation marks and more to implement exception
;; handlers. We do not have continuation marks available, so we
;; rewrite to an alternative form.

;     (topexpand #'(with-handlers ([pred1 handler1] 
;                                  [pred2 handler2])
;                     body))                        
; =>                                                
;     (catch* (list pred1    pred2)                 
;             (list handler1 handler2)              
;             (lambda () body))                     

(define (unexpand stx)
  (define (with-handlers-prefix? id prefix)
    (and (identifier? id)
         (let ([name (symbol->string (syntax-e id))])
           (string-prefix? name prefix))))

  (define (with-handlers-predicate? id)
    (string-prefix? (symbol->string (syntax-e id))
                    "with-handlers-predicate"))
  (define (with-handlers-handler? id)
    (string-prefix? (symbol->string (syntax-e id))
                    "with-handlers-handler"))
  
  
  (define (rewrite-with-handlers stx)
    (syntax-parse stx
      #:literal-sets (kernel-literals) ; #%plain-app quote let-values
      #:literals (continuation-mark-set-first break-enabled-key)
      ;; Task 2 
      #;[x:id (rewrite-kernel-identifier #'x)]
      ;; Task 1
      [(let-values ([(pred/handler ...) pred/handler-expr] ...)
         (let-values (((bpz)
                       (#%plain-app continuation-mark-set-first
                                    '#f break-enabled-key)))
           (#%plain-app call-handled-body:id
                        bpz1
                        lambda0
                        lambda1)))
       ; Compare the pattern above to:
       ;   (topexpand #'(with-handlers ([pred1 handler1]
       ;                                [pred2 handler2])
       ;                   body))
       ; We are rewriting to:
       ;    (catch* (list pred1    pred2)
       ;            (list handler1 handler2)
       ;            (lambda () body))
       #:when (let ()
                (define names (syntax->list #'(pred/handler ... ...)))
                (and (null?
                      (dropf (dropf names with-handlers-predicate?)
                             with-handlers-handler?))
                     (eq? (syntax-e #'call-handled-body) 'call-handled-body)))
       (define pred/handler*      (syntax->list #'(pred/handler ... ...)))
       (define pred-count         (length (takef pred/handler*
                                                 with-handlers-predicate?)))
       (define handlers-count     (- (length pred/handler*) pred-count))
       (define pred/handler-expr* (syntax->list #'(pred/handler-expr ...)))
       (define pred-exprs         (take pred/handler-expr* pred-count))
       (define handler-exprs      (drop pred/handler-expr* pred-count))
       
       (define preds    (map walk pred-exprs))
       (define handlers (map walk handler-exprs))
       #;(define bodies   (map walk (syntax->list #'(body ...))))

       (with-syntax ([(preds ...)    preds]
                     [(handlers ...) handlers]
                     #;[(body ...)     bodies]
                     [catch*         (datum->syntax stx 'catch*)]
                     [list-id        (datum->syntax stx 'list)]
                     [lambda-id      (datum->syntax stx '#%plain-lambda)]
                     [lambda1        (walk #'lambda1)])
         ; The expansion is:
         ;    (catch* (list pred1    pred2)
         ;            (list handler1 handler2)
         ;            (lambda () body))
         ; But we need to use the fully expanded form.
         (syntax/loc stx
           (#%plain-app
            catch*
            (#%plain-app list preds ...)
            (#%plain-app list handlers ...)
            lambda1)))]
      [_ #f]))

  (define (walk stx)
    (define rewritten (rewrite-with-handlers stx))
    (cond
      [rewritten rewritten]
      [else
       (syntax-case stx ()
         [() stx]
        [(a . d)
          (with-syntax ([a* (walk #'a)]
                        [d* (walk #'d)])
            (syntax/loc stx (a* . d*)))]
        [#(ele ...)
          (with-syntax ([(ele* ...) (map walk (syntax->list #'(ele ...)))])
            (syntax/loc stx #(ele* ...)))]
        [_ stx])]))

  (walk stx))

;;;
;;; Renaming of exception structures
;;;

;; The fully expanded syntax contains the names `kernel:exn`, `kernel:exn:fail`, etc.
;; instead of `exn`, `exn:fail`, etc.
;; We prefer to use the shorter names in the runtime, so when parsing the
;; fully expanded syntax into the nanopass representation, we substitute these
;; identifiers.

; Note: The `exn` visible in `compiler.rkt` is different
;       from the binding in `racket/kernel` so we
;       need to import from `racket/kernel` here.
(require (prefix-in kern- racket/kernel))

(define (rewrite-kernel-identifier id)
  (syntax-parse id
    #:literal-sets (kernel-literals) ; #%plain-app quote let-values
    #:literals (kern-exn kern-exn?
                         kern-exn-message kern-exn-continuation-marks
                         kern-exn:fail kern-exn:fail?
                         kern-exn:fail:contract kern-exn:fail:contract?
                         kern-exn:fail:contract:arity kern-exn:fail:contract:arity?
                         kern-exn:fail:contract:divide-by-zero kern-exn:fail:contract:divide-by-zero?
                         kern-exn:fail:contract:non-fixnum-result kern-exn:fail:contract:non-fixnum-result?
                         kern-exn:fail:contract:variable kern-exn:fail:contract:variable?
                         kern-exn:fail:contract:variable-id
                         kern-exn:fail:read kern-exn:fail:read?
                         kern-exn:fail:read-srclocs
                         kern-exn:fail:read:eof kern-exn:fail:read:eof?
                         kern-exn:fail:read:non-char kern-exn:fail:read:non-char?
                         kern-exn:fail:filesystem kern-exn:fail:filesystem?
                         kern-exn:fail:syntax kern-exn:fail:syntax?
                         kern-exn:fail:syntax-exprs
                         kern-exn:fail:syntax:missing-module kern-exn:fail:syntax:missing-module?
                         kern-exn:fail:syntax:missing-module-path
                         kern-exn:fail:syntax:unbound kern-exn:fail:syntax:unbound?
                         kern-arity-at-least
                          )
    [kern-exn                               #'exn]
    [kern-exn?                              #'exn?]
    [kern-exn-message                       #'exn-message]
    [kern-exn-continuation-marks            #'exn-continuation-marks]
    [kern-exn:fail                          #'exn:fail]  
    [kern-exn:fail?                         #'exn:fail?] 
    [kern-exn:fail:contract                 #'exn:fail:contract]
    [kern-exn:fail:contract?                #'exn:fail:contract?]
    [kern-exn:fail:contract:arity           #'exn:fail:contract:arity]
    [kern-exn:fail:contract:arity?          #'exn:fail:contract:arity?]
    [kern-exn:fail:contract:divide-by-zero  #'exn:fail:contract:divide-by-zero]
    [kern-exn:fail:contract:divide-by-zero? #'exn:fail:contract:divide-by-zero?]
    [kern-exn:fail:contract:non-fixnum-result #'exn:fail:contract:non-fixnum-result]
    [kern-exn:fail:contract:non-fixnum-result? #'exn:fail:contract:non-fixnum-result?]
    [kern-exn:fail:contract:variable        #'exn:fail:contract:variable]
    [kern-exn:fail:contract:variable?       #'exn:fail:contract:variable?]
    [kern-exn:fail:contract:variable-id     #'exn:fail:contract:variable-id]
    [kern-exn:fail:read                     #'exn:fail:read]
    [kern-exn:fail:read?                    #'exn:fail:read?]
    [kern-exn:fail:read-srclocs             #'exn:fail:read-srclocs]
    [kern-exn:fail:read:eof                 #'exn:fail:read:eof]
    [kern-exn:fail:read:eof?                #'exn:fail:read:eof?]
    [kern-exn:fail:read:non-char            #'exn:fail:read:non-char]
    [kern-exn:fail:read:non-char?           #'exn:fail:read:non-char?]
    [kern-exn:fail:filesystem               #'exn:fail:filesystem]
    [kern-exn:fail:filesystem?              #'exn:fail:filesystem?]
    [kern-exn:fail:syntax                   #'exn:fail:syntax]
    [kern-exn:fail:syntax?                  #'exn:fail:syntax?]
    [kern-exn:fail:syntax-exprs             #'exn:fail:syntax-exprs]
    [kern-exn:fail:syntax:missing-module    #'exn:fail:syntax:missing-module]
    [kern-exn:fail:syntax:missing-module?   #'exn:fail:syntax:missing-module?]
    [kern-exn:fail:syntax:missing-module-path #'exn:fail:syntax:missing-module-path]
    [kern-exn:fail:syntax:unbound           #'exn:fail:syntax:unbound]
    [kern-exn:fail:syntax:unbound?          #'exn:fail:syntax:unbound?]
    [kern-arity-at-least                    #'arity-at-least]
    
    [_ id]))


;;; 
;;; Parse
;;;

;; The standard tools in Racket represents programs as syntax objects.
;; In particular the output of `expand-syntax` is a syntax object representing
;; a program in fully expanded form. Here `parse` takes such a syntax object
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
    (define (RawProvideSpec* rpss)     (map RawProvideSpec    (stx->list rpss)))
    (define (RawRootModulePath* rrmps) (map RawRootModulePath (stx->list rrmps))))
  
  
  (TopLevelForm : * (T) -> TopLevelForm ()
    (with-output-language (LFE TopLevelForm)
      (syntax-parse T #:literal-sets (kernel-literals) ; keywords in fully expanded programs
        [(begin t ...)                `(topbegin ,T ,(TopLevelForm* #'(t ...)) ...)]
        [(#%expression e)             `(#%expression ,T ,(Expr #'e))]
        [(module mn mp 
           (module-begin mlf ...))    `(topmodule ,T ,(syntax-e #'mn) ,(syntax-e #'mp) 
                                                  ,(ModuleLevelForm* #'(mlf ...)) ...)]
        [(begin-for-syntax s t ...)   `(#%expression ,T ,(Expr #'#f)) ]

        [g                            `,(GeneralTopLevelForm #'g)])))

  (ModuleLevelForm : * (M) -> ModuleLevelForm ()
    (with-output-language (LFE ModuleLevelForm)
      (syntax-parse M #:literal-sets (kernel-literals)
        [(#%provide rps ...)          `(#%provide ,(RawProvideSpec* #'(rps ...)) ...)]
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

  ;; (RawRequireSpec (rrs) ; todo: there are more types of require specs
  ;;   (for-meta pl rrs ...) => (for-meta pl rrs ...)
  ;;   ps) ; note: was rrmp 
  
  (RawRequireSpec : * (RRS) -> RawRequireSpec ()
    (with-output-language (LFE RawRequireSpec)
      (syntax-parse RRS #:literal-sets (kernel-literals)
        [(for-meta pl rrs ...)
         `(for-meta ,(PhaseLevel #'pl)
                    ,(RawRequireSpec* #'(rrs ...)) ...)]
        [_
         `,(PhaselessSpec RRS)])))

  ;; (PhaseLevel (pl)
  ;;   (no-phase)   ; #f in concrete syntax
  ;;   ei)

  (PhaseLevel : * (PL) -> PhaseLevel ()
    (with-output-language (LFE PhaseLevel)
      (syntax-parse PL #:literal-sets (kernel-literals)
        [pl:exact-integer
         `,(syntax-e #'pl)]
        [#f
         `(no-phase)])))


  ;; (PhaselessSpec     (ps)  
  ;;   #;(for-space  space sls ...)
  ;;   #;(just-space space sls ...)
  ;;   sls) ; spaceless-spec

  (PhaselessSpec : * (PS) -> PhaselessSpec ()
    (with-output-language (LFE PhaselessSpec)
      (syntax-parse PS #:literal-sets (kernel-literals)
        #;[(for-space space sls ...) ...]
        #;[(protect         ps ...)  ...]
        [sls `,(SpacelessSpec #'sls)])))
  
  ;; (Space (space)
  ;;   x   ; identifier
  ;;   (no-space))  ; #f in concrete syntax

  (Space : * (S) -> Space ()
    (with-output-language (LFE Space)
      (syntax-parse S #:literal-sets (kernel-literals)
        [x:identifier
         `,#'x]
        [#f
         `(no-space)])))
  
  ;; (RawProvideSpec (rps) 
  ;;   #;(for-meta   phase-level ps ...)
  ;;   #;(for-syntax             ps ...)
  ;;   #;(for-label              ps ...)
  ;;   #;(protect                rps ...)
  ;;   ps)

  (RawProvideSpec : * (RPS) -> RawProvideSpec ()
    (with-output-language (LFE RawProvideSpec)
      (syntax-parse RPS #:literal-sets (kernel-literals)
      #;(for-meta   phase-level ps ...)
      #;(for-syntax             ps ...)
      #;(for-label              ps ...)
      #;(protect                rps ...)
      [ps  `,(PhaselessSpec #'ps)])))
  
  ;; (SpacelessSpec (sls)
  ;;   #;(rename local-id export-id)
  ;;   #;(struct ...)
  ;;   #;(all-from ...)
  ;;   #;(all-from-except ...)
  ;;   #;(all-defined)
  ;;   #;(all-defined-except ...)
  ;;   #;(prefix-all-defined prefix-id)
  ;;   #;(prefix-all-defined-except ...)
  ;;   #;(protect ...)
  ;;   #;(expand (id . datum))
  ;;   #;(expand (id . datum) orig-form)
  ;;   x) ; id

  (SpacelessSpec : * (SLS) -> SpacelessSpec ()
    (with-output-language (LFE SpacelessSpec)
      (syntax-parse SLS #:literal-sets (kernel-literals)
         #;(rename local-id export-id)
         #;(struct ...)
         #;(all-from ...)
         #;(all-from-except ...)
         #;(all-defined)
         #;(all-defined-except ...)
         #;(prefix-all-defined prefix-id)
         #;(prefix-all-defined-except ...)
         #;(protect ...)
         #;(expand (id . datum))
         #;(expand (id . datum) orig-form)
         [x   `,(variable #'x)])))

  
  ;; (RawModulePath     (rmp) rrmp)
  
  (RawRootModulePath : * (RRMP) -> RawRootModulePath ()
    (with-output-language (LFE RawRootModulePath)
      (displayln (list 'RawRootModulePath-in RRMP))
      (syntax-parse RRMP #:literal-sets (kernel-literals)
        [(quote x)      `(quote ,(variable #'x))]
        [x              `,(variable #'x)])))





  (Formals : * (F) -> Formals ()
           (with-output-language (LFE Formals)
             (syntax-parse F
               [(x:id ...)               `(formals (,(variable* #'(x ...)) ...))]
               [(x0:id x:id ... . xd:id) `(formals (,(variable #'x0) ,(variable* #'(x ...)) ...
                                                                     . ,(variable #'xd)))]
               [x:id                     `(formals ,(variable #'x))]
               [_ (raise-syntax-error 'parse "expected formals" F)])))
  
  (Expr : * (E) -> Expr ()
    (with-output-language (LFE Expr)
      (syntax-parse E #:literal-sets (kernel-literals)
        [x:id                                      `,(variable (rewrite-kernel-identifier #'x))]
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
        [(letrec-values ([(x ...) e] ...) e0 e1 ...)
         (let ()
           (define xss (map variable*
                            (syntax->list #'((x ...) ...))))
           (define es  (Expr* #'(e ...)))
           `(letrec-values ,E ([(,xss ...) ,es] ...)
              ,(Expr #'e0) ,(Expr* #'(e1 ...)) ...))]
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

;; print-top-level-results : LFE -> LFE
;;   Wrap parsed user top-level expressions so `-r` prints results like Racket.
;;   The driver inserts a sentinel before user code, and this pass wraps only
;;   top-level expressions after that sentinel. It skips definitions, requires,
;;   and void results.
(define-pass print-top-level-results : LFE (T) -> LFE ()
  (definitions
    (define wr-top-level-results (variable #'wr-top-level-results))
    (define wr-top-level-result  (variable #'wr-top-level-result))
    (define wr-print             (variable #'print))
    (define wr-current-output-port (variable #'current-output-port))
    (define wr-result-syntax     #'print-top-level-results)

    (define (wrap-top-level-result s e)
      (with-output-language (LFE Expr)
        `(app ,s
              ,(var:call-with-values)
              (λ ,s (formals ())
                ,e)
              (λ ,s (formals ,wr-top-level-results)
                (app ,s
                     ,(var:for-each)
                     (λ ,s (formals (,wr-top-level-result))
                       (if ,s
                           (app ,s ,(var:void?) ,wr-top-level-result)
                           (app ,s ,(var:void))
                           (begin ,s
                             (app ,s
                                  ,wr-print
                                  ,wr-top-level-result
                                  (app ,s ,wr-current-output-port)
                                  (quote ,s ,(datum s 0)))
                             (app ,s
                                  ,(var:write-char)
                                  (quote ,s ,(datum s #\newline))
                                  (app ,s ,wr-current-output-port))
                             (app ,s ,(var:void)))))
                     ,wr-top-level-results)
                (app ,s ,(var:void))))))

    (define (ignored-define-syntaxes-expression? e)
      (nanopass-case (LFE Expr) e
        [(quote ,s ,d)
         (equal? (datum-value d) "define-syntaxes is ignored")]
        [else #f]))

    (define (wrap-user-general-top-level-form g)
      (nanopass-case (LFE GeneralTopLevelForm) g
        [(define-values ,s (,x ...) ,e)
         (GeneralTopLevelForm g)]
        [(define-syntaxes ,s (,x ...) ,e)
         (GeneralTopLevelForm g)]
        [(#%require ,s ,rrs ...)
         (GeneralTopLevelForm g)]
        [,e
         (if (ignored-define-syntaxes-expression? e)
             (GeneralTopLevelForm g)
             (with-output-language (LFE GeneralTopLevelForm)
               `,(wrap-top-level-result wr-result-syntax e)))]))

    (define (sentinel-definition? g)
      (nanopass-case (LFE GeneralTopLevelForm) g
        [(define-values ,s (,x) ,e)
         (eq? (syntax-e (variable-id x))
              print-top-level-results-sentinel-symbol)]
        [else #f]))

    (define (wrap-user-top-level-form t)
      (nanopass-case (LFE TopLevelForm) t
        [(topbegin ,s ,t ...)
         (with-output-language (LFE TopLevelForm)
           `(topbegin ,s ,(map wrap-user-top-level-form t) ...))]
        [(#%expression ,s ,e)
         (with-output-language (LFE TopLevelForm)
           `(#%expression ,s ,(wrap-top-level-result s e)))]
        [,g (wrap-user-general-top-level-form g)]
        [else (TopLevelForm t)]))

    (define (process-top-level-forms ts)
      (let loop ([ts ts] [wrap? #f])
        (cond
          [(null? ts) '()]
          [else
           (define t (car ts))
           (nanopass-case (LFE TopLevelForm) t
             [,g
              (if (sentinel-definition? g)
                  (loop (cdr ts) #t)
                  (cons (if wrap?
                            (wrap-user-top-level-form t)
                            (TopLevelForm t))
                        (loop (cdr ts) wrap?)))]
             [else
              (cons (if wrap?
                        (wrap-user-top-level-form t)
                        (TopLevelForm t))
                    (loop (cdr ts) wrap?))])]))))

  (TopLevelForm : TopLevelForm (T) -> TopLevelForm ()
    [(topbegin ,s ,t ...)
     `(topbegin ,s ,(process-top-level-forms t) ...)]
    [(#%expression ,s ,e)
     `(#%expression ,s ,(Expr e))]
    [(topmodule ,s ,mn ,mp ,mf ...)
     `(topmodule ,s ,mn ,mp ,(map ModuleLevelForm mf) ...)]
    [(begin-for-syntax ,s ,t ...)
     `(begin-for-syntax ,s ,(map TopLevelForm t) ...)]
    [,g
     (GeneralTopLevelForm g)])

  (ModuleLevelForm : ModuleLevelForm (M) -> ModuleLevelForm ()
    [(#%provide ,rps ...)
     `(#%provide ,rps ...)]
    [,g
     (GeneralTopLevelForm g)])

  (GeneralTopLevelForm : GeneralTopLevelForm (G) -> GeneralTopLevelForm ()
    [(define-values ,s (,x ...) ,e)
     `(define-values ,s (,x ...) ,(Expr e))]
    [(define-syntaxes ,s (,x ...) ,e)
     `(define-syntaxes ,s (,x ...) ,(Expr e))]
    [(#%require ,s ,rrs ...)
     `(#%require ,s ,rrs ...)]
    [,e
     (Expr e)])

  (Expr : Expr (e) -> Expr ()))


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
    (check-equal? (test #'(let-values ([(x) 0]) (set! x 3) x))
                  '(let-values ([(x) '0]) (set! x '3) x))
    (check-equal? (test #'(set! x 3)) '(set! x '3))
    (check-equal? (test #'(if 1 2 3)) '(if '1 '2 '3))
    (check-equal? (test #'(begin  1 2 3)) '(topbegin  '1 '2 '3))
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
;;; FLATTEN TOP-LEVEL BEGIN
;;;

(define-pass flatten-topbegin : LFE (T) ->  LFE ()
  (definitions)

  (TopLevelForm     : TopLevelForm        (T) -> TopLevelForm ()
    [(topbegin ,s ,t0 ...)
     (let ()
       (define (topbegin? t)
         (nanopass-case (LFE TopLevelForm) t
           [(topbegin ,s ,t0 ...) #t]
           [else                  #f]))

       (let loop ([ts t0])
         ; we loop until there are no `topbegin`s left
         (cond
           [(memf topbegin? ts)
            ; for each t we return a list of `TopLevelForm`.
            (define tss
              (for/list ([t ts])
                (nanopass-case (LFE TopLevelForm) t
                  [(topbegin ,s ,t0  ...) t0]
                  [else                   (list t)])))
            (loop (append* tss))]
           [else
            `(topbegin ,s ,ts ...)])))]))

;;;
;;; Infer names for `#%plain-lambda` and `case-lambda` 
;;;

;; This pass looks for lambda and case-lambda expressions and
;; annotate their syntax-object `s` with an inferred name.

;; The inferred name is taken from surrounding bindings constructs:
;;    - define-values
;;    - let-values
;;    - letrec-values
;;

;; Special care is needed to support optional arguments.
;; A definition like 
;;    (define (foo [a 10]) a)
;; expands into
;;    (define-values (foo) (let-values (((foo)
;;                                       (lambda (a1)
;;                                          (let-values (((a) (if '#f '10 a1))) (let-values () a)))))
;;                           (case-lambda (() (#%app foo '10)) ((a1) (#%app foo a1)))))

;; Inferring that `(lambda (a1) ...)` is named `foo` is obvious from the `let-values`.
;; That the `foo` from `define-values` is the name for `case-lambda` requires a bit
;; effort to detect.

#;(define (infer-names x) x)

(define-pass infer-names : LFE (T) -> LFE ()
  (definitions
    ; Environments that map a binding position to a name
    (define ε (λ (i) #f))      ; empty environment
    (define (make-ρ names)
      (define ns (list->vector names))
      (define n  (vector-length ns))
      (λ (i) (and i (< i n) (vector-ref ns i))))
    
    (define (enumerate xs) (for/list ([i (length xs)]) i))
    
    (define (var->symbol-datum s x)
      (datum s (syntax-e (variable-id x))))
    
    (define (Expr* es ρ i/is)
      (cond
        [(list? i/is)
         (define is i/is)
         (map (λ (e i) (Expr e ρ i)) es is)]
        [else
         (define i i/is)
         (map (λ (e) (Expr e ρ i)) es)]))

    (define (with-source-stx stx src)
      (if (syntax? src)
          (syntax-property stx 'source-stx src)
          stx))

    (define (wrap e s ρ i)
      ; Rather than wrap `e` in a `procedure-rename`, we
      ; annotate `s` to hold the inferred name.
      ; The closure conversion pass will use the name,
      ; when explicit closure allocation in the form
      ; of `(closure ...)` is introduced.
      (define x (and i (ρ i)))
      (cond
        ; Put the inferred name in the syntax slot of the abstraction
        [x    (with-output-language (LFE Expr)
                (nanopass-case (LFE Expr) e
                  [(λ ,s ,f ,e ...) 
                   (define named-s
                     (with-source-stx #`(inferred-name #,x s) s))
                   `(λ ,named-s
                      ,f ,e ...)]
                  [(case-lambda ,s (,f ,e0 ,e ...) ...)
                   (define named-s
                     (with-source-stx #`(inferred-name #,x s) s))
                   `(case-lambda ,named-s
                                 (,f ,e0 ,e ...) ...)]
                  [else (error 'wrap "expected a λ or case-lambda, got: ~a" e)]))]
        [else (with-output-language (LFE Expr)
                `,e)]))

    (define (Expr** ess ρ i)
      (map (λ (es) (Expr* es ρ i)) ess))

    (define (is-values? e)
      (and (variable? e)
           (free-identifier=? (variable-id e) #'values)
           #;(eq? (syntax-e (variable-id e)) 'values)))
    )

  (TopLevelForm : TopLevelForm (T) -> TopLevelForm ()
    [(#%expression ,s ,e)  `(#%expression ,s ,(Expr e ε 0))]
    [,g                    (GeneralTopLevelForm g ε)])

  (ModuleLevelForm : ModuleLevelForm (M) -> ModuleLevelForm ()
    [,g                    (GeneralTopLevelForm g ε)])
  
  (Formals             : Formals             (F) -> Formals             ())

  (GeneralTopLevelForm : GeneralTopLevelForm (G ρ) -> GeneralTopLevelForm ()
    [,e                              (Expr e ε 0)]
    [(define-values ,s (,x ...) ,e)  (let ([e (Expr e (make-ρ x) 0)])
                                       `(define-values ,s (,x ...) ,e))])

  
  (Expr : Expr (E ρ i) -> Expr ()
    ; ρ : maps a bindings position to a name or #f
    ; i : the binding position the expression will be bound in
    [,x                                            `,x]
    [(λ ,s ,f ,e ...)                              (let ([e (Expr* e ε #f)])
                                                     (wrap `(λ ,s ,f ,e ...) 
                                                           s ρ i))]
    [(case-lambda ,s (,f ,e0 ,e ...) ...)          (let ([e0 (Expr*  e0 ε #f)]
                                                         [e  (Expr** e  ε #f)])
                                                     (wrap `(case-lambda ,s (,f ,e0 ,e ...) ...)
                                                           s ρ i))]
    [(if ,s ,e0 ,e1 ,e2)                           (let ([e0 (Expr e0 ε #f)]
                                                         [e1 (Expr e1 ρ i)]
                                                         [e2 (Expr e2 ρ i)])
                                                     `(if ,s ,e0 ,e1 ,e2))]
    [(begin  ,s ,e0 ,e ...)                        (let* ([es (cons e0 e)]
                                                          [e  (Expr* (drop-right es 1) ε #f)]
                                                          [en (Expr  (last es)         ρ i)])
                                                     (let* ([es (append e (list en))]
                                                            [e0 (car es)]
                                                            [e  (cdr es)])
                                                     `(begin ,s ,e0 ,e ...)))]
    [(begin0 ,s ,e0 ,e1 ...)                       (let ([e0 (Expr  e0 ρ i)]
                                                         [e1 (Expr* e1 ε #f)])
                                                     `(begin0 ,s ,e0 ,e1 ...))]
    [(set! ,s ,x ,e)                               (let ([e (Expr e ε #f)])
                                                     `(set! ,s ,x ,e))]
    [(wcm ,s ,e0 ,e1 ,e2)                          (let ([e0 (Expr e0 ε #f)]
                                                         [e1 (Expr e1 ε #f)]
                                                         [e2 (Expr e2 ρ i)])
                                                     `(wcm ,s ,e0 ,e1 ,e2))]
    [(quote ,s ,d)                                `(quote ,s ,d)]
    [(quote-syntax ,s ,d)                         `(quote-syntax ,s ,d)]
    [(app ,s ,e0 ,e ...)
     (guard (is-values? e0))                      (let ([e (Expr* e ρ (enumerate e))])
                                                    `(app ,s ,e0 ,e ...))]    
    [(app ,s ,e0 ,e1 ...)                          (let ([e0 (Expr  e0 ε #f)]
                                                         [e1 (Expr* e1 ε #f)])
                                                     `(app ,s ,e0 ,e1 ...))]
    [(top ,s ,x)                                   `(top ,s ,x)]
    [(variable-reference ,s ,vrx)                  `(variable-reference ,s ,vrx)]

    [(let-values ,s ([(,x** ...) ,e*] ...) ,e0 ,e1 ...)
     (let* ([e* (map (λ (x* e) (Expr e (make-ρ x*) 0))
                     x** e*)]
            ; e0 e1 ... = e ... en
            [es (cons e0 e1)]
            [e  (drop-right es 1)]
            [en (last es)]
            ; transform e ... en
            [e  (Expr* e  ε #f)]
            [en (Expr  en ρ i)]
            ; e0 e1 ... = e ... en
            [es (append e (list en))]
            [e0 (car es)]
            [e1 (cdr es)])
       `(let-values ,s ([(,x** ...) ,e*] ...) ,e0 ,e1 ...))]

    [(letrec-values ,s ([(,x** ...) ,e*] ...) ,e0 ,e1 ...)  
     (let* ([e* (map (λ (x* e)
                       (Expr e (make-ρ x*) 0))
                     x** e*)]
            ; e0 e1 ... = e ... en
            [es (cons e0 e1)]
            [e  (drop-right es 1)]
            [en (last es)]
            ; transform e ... en
            [e  (Expr* e ε #f)]
            [en (Expr  en ρ i)]
            ; e0 e1 ... = e ... en
            [es (append e (list en))]
            [e0 (car es)]
            [e1 (cdr es)])
       `(letrec-values ,s ([(,x** ...) ,e*] ...) ,e0 ,e1 ...))]
    ))


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
          [(? box? b)       `(app ,h ,(var:box-immutable) ,(loop (unbox b)))]
          [(? keyword? kw) (let ([s (loop (keyword->string kw))])
                             `(app ,h ,(var:string->keyword) ,s))]
          [(? hash? ht)    (cond
                             [(hash-eq? ht)
                              `(app ,h ,(var:make-hasheq)
                                    ,(loop (hash->list ht)))]
                             [(hash-eqv? ht)
                              `(app ,h ,(var:make-hasheqv)
                                    ,(loop (hash->list ht)))]
                             [(hash-equal? ht)
                              `(app ,h ,(var:make-hash)
                                    ,(loop (hash->list ht)))]
                             ; todo: immutable hashes
                             ; todo: always equal hashes
                             [else
                              (error 'datum->construction-expr
                                     "unsupported hash table: ~a" ht)])]
          [else            (error 'datum->construction-expr "got: ~a" v)]))))
  (Expr : Expr (e) -> Expr ()
    [(quote ,s ,d)
     (datum->construction-expr s (datum-value d))]
    [(quote-syntax ,s ,d)
     (let* ([src   (syntax-source s)]
            [src   (if (path? src)   (path->string src)   src)] ; no paths yet ...
            [src   (if (symbol? src) (symbol->string src) src)] ; keep it simple for now ...
            [src   (Expr `(quote ,s ,(datum s src)))]
            
            [l     (Expr `(quote ,s ,(datum s (syntax-line s))))]
            [c     (Expr `(quote ,s ,(datum s (syntax-column s))))]
            [p     (Expr `(quote ,s ,(datum s (syntax-position s))))]
            [sp    (Expr `(quote ,s ,(datum s (syntax-span s))))]
            [false (Expr `(quote ,s ,(datum s #f)))]
            [v     (Expr `(quote ,s ,d))])
       `(app ,s
             ,(var:datum->syntax)
             ,false ; context
             ,v     ; datum
             ,`(app ,s ,(var:make-srcloc) ,src ,l ,c ,p ,sp) 
             ,false ; prop
             ,false ; ignored
             ))])
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
  (TopLevelForm (t)
    (- (begin-for-syntax s t ...)))
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
                     (flatten-topbegin
                      (parse                  
                       (expand-syntax stx)))))))])
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
;;; collect-top-level-bindings
;;;

;; Collect top-level bindings (for seeding α-rename).
;; This allows forward references at the top-level.

(define-pass collect-top-level-bindings : LFE2 (T) -> * ()
  (definitions
    (define top                '())
    (define (top! x)           (set! top (cons x top)))
    (define (top!* xs)         (for-each top! xs))
    (define (TopLevelForm* Ts) (for-each TopLevelForm Ts))
    (define (ModuleLevelForm* Ms) (for-each ModuleLevelForm Ms)))

  (TopLevelForm : TopLevelForm (T) -> * ()
    [(topbegin ,s ,t ...)           (TopLevelForm* t)]
    [(#%expression ,s ,e)           (void)]
    [(topmodule ,s ,mn ,mp ,mf ...) (ModuleLevelForm* mf)]
    [,g                             (GeneralTopLevelForm g)])

  (ModuleLevelForm : ModuleLevelForm (M) -> * ()
    [(#%provide ,rps ...)           (void)]
    [,g                             (GeneralTopLevelForm g)])

  (GeneralTopLevelForm : GeneralTopLevelForm (G) -> * ()
    [(define-values   ,s (,x ...) ,e) (top!* x)]
    [(define-syntaxes ,s (,x ...) ,e) (top!* x)]
    [(#%require       ,s ,rrs ...)    (void)]
    [,e                               (void)])

  (TopLevelForm T)
  top)


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
; into ones used in the output. Identifiers referring to primitives are
; mapped to themselves in order not to rename primitives in the output.

; FACT: α-renaming will not rename identifiers bound to primitives.
;       After renaming an identifier that "looks like a primitive is a
;       primitive".
;       Example:  (begin (define + 42) (+ 1 2))))))
;         becomes (begin (define-values (|+.8|) '42) (|+.8| '1 '2))
;         The + that didn't refer to a primitive was renamed.

; An unbound identifier is mapped to #f by the environment ρ.

; The expression (fresh x ρ) returns a new identifier not already mapped in ρ.
; The new name is based in the identifier x.

(define-language LFE2+ (extends LFE2)
  (SpacelessSpec (sls)
    (- x)
    (+ [x0 x1])))  ; [x_before x_after] names before and after renaming


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
  ;; During α-renaming we must preserve lexical binding identity.
  ;; Comparing only `(syntax-e ...)` causes accidental capture when a
  ;; user binding has the same printed name as a core binding
  ;; (for example local `values` vs primitive `values`).
  ;;
  ;; However, some compiler-generated top-level/unbound identifiers are
  ;; compared by printed name in existing code paths, so we keep a narrow
  ;; fallback for cases where lexical binding data is missing.
  (define id1 (variable-id v1))
  (define id2 (variable-id v2))
  (define b1  (identifier-binding id1))
  (define b2  (identifier-binding id2))
  (define s1  (syntax-e id1))
  (define s2  (syntax-e id2))
  (or (free-identifier=?  id1 id2)
      (bound-identifier=? id1 id2)
      ;; Name fallback is only allowed when neither side is lexical.
      ;; This preserves module/unbound matching (needed for some top refs)
      ;; while avoiding local lexical capture (for example local `values`).
      (and (not (eq? b1 'lexical))
           (not (eq? b2 'lexical))
           (or (not b1) (not b2))
           (eq? s1 s2))))

(define α-rename-mode (make-parameter 'full))  ; 'full or 'simple

; 'simple is only used for the existing test cases
; 'full   is used otherwise
; In the simple mode, the  first occurence of a variable x in a scope is not renamed.
; In the full mode, all variables are renamed.
; In the code generator the variables of a `let-values` all end up as local variables
; in a `func`. Therefore, all variable names must be unique.
; In the simple mode, the program (begin (let ([x 1]) x)  (let ([x 2]) x)) fails.
; Someday, when the test cases are rewritten, we can remove the simple mode.

; α-rename : LFE2 -> LFE2+
(define (α-rename T)
  (define top-bindings (collect-top-level-bindings T))
  (current-top-symbols-needing-defined-check (make-hasheq))
  (current-console-bridge-top-binding-names
   (for/list ([x (in-list top-bindings)]
              #:do [(define sym (syntax-e (variable-id x)))]
              #:when (symbol? sym))
     sym))
  (letv ((T ρ) (α-rename/pass1 T top-bindings))
    ; pass 2 patches the `provide` form to hold both variables
    ; before and after renaming.
    (α-rename/pass2 T ρ)))

(define-pass α-rename/pass2 : LFE2 (T ρ) -> LFE2+ ()
  (definitions
    (define (ModuleLevelForm* xs) (map ModuleLevelForm xs))
    (define (TopLevelForm*    xs) (map TopLevelForm    xs))
    (define (RawProvideSpec*  xs) (map RawProvideSpec  xs)))
  
  (TopLevelForm : TopLevelForm (T) -> TopLevelForm ()
    [(topbegin ,s ,t ...)           (let ([t (TopLevelForm* t)])
                                      `(topbegin ,s ,t ...))]     
    [(topmodule ,s ,mn ,mp ,mf ...) (let ([mf (ModuleLevelForm* mf)])
                                      `(topmodule ,s ,mn ,mp ,mf ...))])
  
  (ModuleLevelForm : ModuleLevelForm (M) -> ModuleLevelForm ()
    [(#%provide ,rps ...) `(#%provide ,(RawProvideSpec* rps) ...)])
  
  (RawProvideSpec : RawProvideSpec (RPS) -> RawProvideSpec ()
    [,ps `,(PhaselessSpec ps)])

  (PhaselessSpec : PhaselessSpec (PS) -> PhaselessSpec ()
    [,sls `,(SpacelessSpec sls)])

  (SpacelessSpec : SpacelessSpec (SS) -> SpacelessSpec ()
    [,x       `[,x ,(or (ρ-ref ρ x) x)]]))  ; <-- before and after renaming


(define-pass α-rename/pass1 : LFE2 (T top-bindings) -> LFE2 (ρ)
  (definitions
    (define (reserved-target-language-keyword? id)
      ; Any reserved keywords in the target language that needs renaming.
      ; In WebAssembly the "user" names are prefixed with $, so
      ; there is nothing to worry about.
      #f)
    (define (initial-binding x)
      (cond [(reserved-target-language-keyword? x) => values]
            [(primitive? (variable-id (rename-env-variable x))) x]
            [else                                             #f]))
    (define (lookup ρ x)
      (or (ρ-ref ρ x)
          (initial-binding x)))
    (define (initial-ρ)
      (ρ-set* (ρ-empty) top-bindings))
    (define (fresh/simple x ρ [orig-x x])
      (if (ρ-name-used? ρ x) (fresh (new-var x) ρ x) x))
    (define (fresh/full x ρ [orig-x x])
      (if (ρ-name-used? ρ x) (fresh (new-var x) ρ x) (new-var x)))
    (define fresh (case (α-rename-mode)
                    [(simple) fresh/simple]
                    [(full)   fresh/full]))
    (define (rename x ρ)
      (define x* (fresh x ρ))
      (values x* (ρ-set ρ x x*)))
    (define (rename*          xs ρ) (map2* rename   xs ρ))
    (define (rename**        xss ρ) (map2* rename* xss ρ))
    (define (top-binding? x)
      (ormap (λ (top-binding) (id=? x top-binding)) top-bindings))
    (define seen-top-bindings empty-set)
    (define (seen-top-binding? x)
      (set-in? x seen-top-bindings))
    (define (remember-top-level! xs)
      (set! seen-top-bindings (set-union seen-top-bindings (ids->id-set xs))))
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
    (define inside-module? (make-parameter #f))
    (define delayed-expression? (make-parameter #f)))
  
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
                                           ; TODO - lift this restriction?
                                           (define x-prim (ormap (λ (x) (and (primitive? (variable-id x))
                                                                             (variable-id x)))
                                                                 x))
                                           (when x-prim
                                             (raise-syntax-error 'α-rename
                                                    "In webracket it is not allowed to redefine a primitive at the top-level"
                                                    s x-prim))
                                           ; top-level variables aren't renamed
                                           ; but in order to see that they are defined,
                                           ; they must map to themselves. 
                                           (let ((ρ (ρ-set* ρ x)))
                                             (letv ((e _) (Expr e ρ))
                                                   (remember-top-level! x)
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
                                                    (letv ((e ρ) (parameterize ([delayed-expression? #t])
                                                                  (Expr e ρ)))
                                                      (values `(λ ,s ,f ,e) ρ-orig))))])
  (Expr : Expr (E ρ) -> Expr (ρ)
    [,x                                         (let ([ρx (lookup ρ x)])
                                                  (cond
                                                    [ρx
                                                     ;; A top-level read before the binding is seen must check
                                                     ;; for $undefined at runtime, but later reads stay fast.
                                                     (when (and (top-binding? ρx)
                                                                (not (seen-top-binding? ρx)))
                                                       (record-top-symbol-needing-defined-check! ρx))
                                                     (values `,ρx ρ)]
                                                    [else
                                                     (unless (variable? x)
                                                       (error 'here "got ~a" x))
                                                     (define s (syntax-e (variable-id x)))
                                                     (cond
                                                       [(memq s constants)
                                                        (values `(quote ,(variable-id x)
                                                                        ,(datum (variable-id x) (constant-value s)))
                                                                ρ)]
                                                       [(non-literal-constant? s) ; like prop:object-name
                                                        (values `,x ρ)]
                                                       [(inside-module?)
                                                        (raise-syntax-error
                                                         'α-rename "compiler.rkt: unbound variable"
                                                         (variable-id x))]
                                                       [else
                                                        ; In full Racket variable references outside modules
                                                        ; go through a current namespace - think #%top .
                                                        
                                                        ; signal unbound variable at runtime
                                                        (displayln (list 'WARNING "unbound?" x) (current-error-port))
                                                        (values `(app ,#'here
                                                                      ,(variable #'raise-unbound-variable-reference)
                                                                      ,`(quote ,(variable-id x)
                                                                               ,(datum (variable-id x) s))
                                                                      ; Note: `datum` has been eliminated at this point,
                                                                      ;       so a different approach is needed
                                                                      ; ,`(quote ,#'here  ,(datum #'unbound (variable-id x)))
                                                                      )
                                                                ρ)])]
                                                    ))]
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
    [(set! ,s ,x ,e)                            (define x* (lookup ρ x))
                                                  (unless x*
                                                    (raise-syntax-error
                                                     'α-rename
                                                     "set!: assignment to unbound identifier"
                                                     (variable-id x)))
                                                  (when (and (not (inside-module?))
                                                             (not (delayed-expression?))
                                                             (top-binding? x*)
                                                             (not (seen-top-binding? x*)))
                                                    (raise-syntax-error
                                                     'α-rename
                                                     "set!: assignment disallowed; cannot set variable before its definition"
                                                     (variable-id x)))
                                                  (letv ((e ρ) (Expr e ρ))
                                                    (values `(set! ,s ,x* ,e) ρ))]
    [(wcm ,s ,e0 ,e1 ,e2)                       (letv ((e0 ρ) (Expr e0  ρ))
                                                  (letv ((e1 ρ) (Expr e1  ρ))
                                                    (letv ((e2 ρ) (Expr e2  ρ))
                                                      (values
                                                       `(wcm ,s ,e0 ,e1 ,e2) ρ))))]
    [(app ,s ,e0 ,e1 ...)                     (letv ((e0 ρ) (Expr e0 ρ))
                                                (letv ((e1 ρ) (Expr* e1 ρ))
                                                  (values `(app ,s ,e0 ,e1 ...) ρ)))]
    ; Until full namespace lookup is implemented, normalize known top refs
    ; to the collected top-level binding so forward and non-forward refs
    ; share one variable path.
    [(top ,s ,x)                              (cond
                                                [(and (variable? x) (ρ-ref ρ x))
                                                 => (λ (x*)
                                                      (when (and (top-binding? x*)
                                                                 (not (seen-top-binding? x*)))
                                                        (record-top-symbol-needing-defined-check! x*))
                                                      (values x* ρ))]
                                                [else
                                                 (values `(top ,s ,x) ρ)])]
    [(variable-reference ,s ,vrx)             (values `(variable-reference ,s ,(VariableReferenceId vrx ρ)) ρ)])

  (VariableReferenceId : VariableReferenceId (VRX ρ) -> VariableReferenceId ()
    [(anonymous ,s)       `(anonymous ,s)]
    [(top ,s ,x)          `(top ,s ,(or (and (variable? x) (ρ-ref ρ x)) x))]
    [(non-top ,s ,x)      (cond
                            [(and (variable? x) (lookup ρ x))
                             => (λ (x*)
                                  (if (top-binding? x*)
                                      `(top ,s ,x*)
                                      `(non-top ,s ,x*)))]
                            [else `(non-top ,s ,x)])])
  
  (let ([ρ0 (initial-ρ)])
    (letv ((T ρ) (TopLevelForm T ρ0))
      (values T ρ))))

  ;; (VariableReferenceId (vrx)
  ;;    x                                            
  ;;    (anonymous s)                                => ()
  ;;    (top s x)                                    => (#%top . x)))


(module+ test
  (let ([test (λ (stx)
                (reset-counter!)
                (parameterize ([α-rename-mode 'simple])
                  (unparse-all
                   (unparse-LFE2+ (α-rename
                                  (explicit-case-lambda
                                   (explicit-begin
                                    (flatten-topbegin
                                     (parse
                                      (expand-syntax stx))))))))))])
        
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
                         (begin x.1 y.2 z a.3 b.4 c))))

      (check-equal? (test #'(let ([x 0])
                              (let ([x 1])
                                (let ([x 2]
                                      [y x])
                                  (begin x y)))))
                    '(let-values (((x) '0))
                       (let-values (((x.1) '1))
                         (let-values (((x.2) '2)
                                      ((y)   x.1))
                           (begin x.2 y)))))

      (define (mkvar s)
        (variable (datum->syntax #f s)))
      (let* ([x  (mkvar 'x)]
             [x* (mkvar 'x.1)]
             [ρ  (ρ-set (ρ-empty) x x*)])
        (check-equal? (unparse-variable (ρ-ref ρ x)) 'x.1)
        (check-equal? (unparse-variable (ρ-ref ρ (datum->syntax #f 'x))) 'x.1)
        (check-true  (ρ-name-used? ρ (datum->syntax #f 'x))))))

;;;
;;; Uncover Assigned Variables
;;;

;; The pass `collect-assignable-variables` returns id-set of all variables
;; that are (potentially) assigned to.

;; This information is needed in tho following two passes:
;;   `lower-letrec-values` and `box-mutables`.

;; Note: When `lower-letrec-values` needs to insert `set!` it needs to
;;       update the table, so `box-mutable` gets uptodate information.

(struct var-info        (assigned?) #:transparent)
(struct assigned-analysis (assigned)  #:transparent)
; where
;  assigned is a hashtable from identifier to var-info

(define current-assigned-analysis (make-parameter #f))

(define assigned-var-info (var-info #t))

(define (assigned-analysis-mark-assigned! analysis x)
  (when (assigned-analysis? analysis)
    (define ht (assigned-analysis-assigned analysis))
    (unless (match (hash-ref ht x #f)
              [(var-info #t) #t]
              [_             #f])
      (hash-set! ht x assigned-var-info))))

(define (assigned-analysis->id-set analysis)
  (for/fold ([xs empty-set]) ([(x vi) (in-hash (assigned-analysis-assigned analysis))])
    (match vi
      [(var-info #t) (set-add xs x)]
      [_ xs])))

(define (uncover-assigned! T)
  (define assigned (make-hasheq))
  (for ([x (in-list (id-set->list (collect-assignable-variables T)))])
    (hash-set! assigned x assigned-var-info))
  (assigned-analysis assigned))

(define-pass collect-assignable-variables : LFE2+ (T) -> * ()
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
    [(#%provide ,rps ...)
     ;; Preserve accumulator: this pass threads one running set across forms.
     ;; Returning empty-set here would erase mutable vars seen earlier.
     xs]
    [,g                             (GeneralTopLevelForm g xs)])
  (GeneralTopLevelForm : GeneralTopLevelForm (G xs) -> * (xs)
    [,e                                 (Expr e xs)]
    ; until we implement namespace, top-level variables are boxed
    [(define-values   ,s (,x ...) ,e)
     (if assignment-convertsion-for-top-level-vars?
         (Expr e (set-union (ids->id-set x) xs))
         (Expr e xs))]
    [(define-syntaxes ,s (,x ...) ,e)   (Expr e xs)]
    [(#%require       ,s ,rrs ...)
     ;; Preserve accumulator for the same reason as #%provide above.
     xs])

  (RawProvideSpec : RawProvideSpec (RPS rps) -> * (rps)
    #;(for-meta   phase-level ps ...)
    #;(for-syntax             ps ...)
    #;(for-label              ps ...)
    #;(protect                rps ...)
    [,ps (PhaselessSpec ps)])

  (PhaselessSpec : PhaselessSpec (PS ps) -> * (ps)
    #;(for-space space sls ...)
    #;(protect         ps ...)
    [,sls (SpacelessSpec sls)]) ; spaceless-spec

  (SpacelessSpec : SpacelessSpec (SLS sls) -> * (sls)
    #;(rename local-id export-id)
    #;(struct ...)
    #;(all-from ...)
    #;(all-from-except ...)
    #;(all-defined)
    #;(all-defined-except ...)
    #;(prefix-all-defined prefix-id)
    #;(prefix-all-defined-except ...)
    #;(protect ...)
    #;(expand (id . datum))
    #;(expand (id . datum) orig-form)
    [[,x0 ,x1] empty-set]) ; [id_before id_after]

  #;(Space (space)
      x ; identifier
      #f)
  

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
    [(set! ,s ,x ,e)                            (set-add (Expr e xs) x)]    
    [(top ,s ,x)                                xs]
    [(variable-reference ,s ,vrx)               xs]
    [(quote ,s ,d)                              xs]
    [(quote-syntax ,s ,d)                       xs]
    [(wcm ,s ,e0 ,e1 ,e2)                       (Expr* (list e0 e1 e2) xs)]
    [(app ,s ,e0 ,e1 ...)                       (Expr* (cons e0 e1) xs)])  
  (TopLevelForm T empty-set))


;;;
;;; LOWER LETREC VALUES
;;;

;; Lower complex letrec-values only after infer-names has seen the original
;; binding shape. Otherwise generated temporaries become the inferred names.
;; This stage-1 version keeps the old rewrite strategy and only ports it to
;; the post-α-rename language, so it can run after α-rename.

(define-pass lower-letrec-values : LFE2+ (T) -> LFE2+ ()
  (definitions
    (define h #'lower-letrec-values)
    (define (referenced-vars e)
      (define (Expr* es)
        (for/fold ([xs empty-set]) ([e (in-list es)])
          (set-union xs (Expr e))))
      (define (Abstraction ab)
        (nanopass-case (LFE2+ Abstraction) ab
          [(λ ,s ,f ,e0) (Expr e0)]))
      (define (Expr e)
        (nanopass-case (LFE2+ Expr) e
          [,x                       (set-add empty-set x)]
          [,ab                      (Abstraction ab)]
          [(case-lambda ,s ,ab ...) (for/fold ([xs empty-set]) ([ab (in-list ab)])
                                     (set-union xs (Abstraction ab)))]
          [(if ,s ,e0 ,e1 ,e2)      (Expr* (list e0 e1 e2))]
          [(begin ,s ,e0 ,e1 ...)   (Expr* (cons e0 e1))]
          [(begin0 ,s ,e0 ,e1 ...)  (Expr* (cons e0 e1))]
          [(let-values ,s ([(,x ...) ,e] ...) ,e0)
           (Expr* (cons e0 e))]
          [(letrec-values ,s ([(,x ...) ,e] ...) ,e0)
           (Expr* (cons e0 e))]
          [(set! ,s ,x ,e0)         (Expr e0)]
          [(top ,s ,x)              empty-set]
          [(variable-reference ,s ,vrx) empty-set]
          [(quote ,s ,d)            empty-set]
          [(quote-syntax ,s ,d)     empty-set]
          [(wcm ,s ,e0 ,e1 ,e2)     (Expr* (list e0 e1 e2))]
          [(app ,s ,e0 ,e1 ...)     (Expr* (cons e0 e1))]))
      (Expr e))
    (define (assigned-vars e)
      (define (Expr* es)
        (for/fold ([xs empty-set]) ([e (in-list es)])
          (set-union xs (Expr e))))
      (define (Abstraction ab)
        (nanopass-case (LFE2+ Abstraction) ab
          [(λ ,s ,f ,e0) (Expr e0)]))
      (define (Expr e)
        (nanopass-case (LFE2+ Expr) e
          [,x                       empty-set]
          [,ab                      (Abstraction ab)]
          [(case-lambda ,s ,ab ...) (for/fold ([xs empty-set]) ([ab (in-list ab)])
                                     (set-union xs (Abstraction ab)))]
          [(if ,s ,e0 ,e1 ,e2)      (Expr* (list e0 e1 e2))]
          [(begin ,s ,e0 ,e1 ...)   (Expr* (cons e0 e1))]
          [(begin0 ,s ,e0 ,e1 ...)  (Expr* (cons e0 e1))]
          [(let-values ,s ([(,x ...) ,e] ...) ,e0)
           (Expr* (cons e0 e))]
          [(letrec-values ,s ([(,x ...) ,e] ...) ,e0)
           (Expr* (cons e0 e))]
          [(set! ,s ,x ,e0)         (set-add (Expr e0) x)]
          [(top ,s ,x)              empty-set]
          [(variable-reference ,s ,vrx) empty-set]
          [(quote ,s ,d)            empty-set]
          [(quote-syntax ,s ,d)     empty-set]
          [(wcm ,s ,e0 ,e1 ,e2)     (Expr* (list e0 e1 e2))]
          [(app ,s ,e0 ,e1 ...)     (Expr* (cons e0 e1))]))
      (Expr e))
    (define (lhs-free? e lhs-set)
      (set-empty? (set-intersection lhs-set (referenced-vars e))))
    (define (effect-free-simple-kind e lhs-set)
      (define (join-simple-kinds k1 k2)
        (cond
          [(or (not k1) (not k2)) #f]
          [(or (eq? k1 'allocates)
               (eq? k2 'allocates))
           'allocates]
          [else
           'pure]))
      (define (Expr* es)
        (for/fold ([kind 'pure]) ([e (in-list es)])
          (join-simple-kinds kind (Expr e))))
      (define letrec-primitive-order-classes
        (let ([ht (make-hasheq)])
          (for ([x (in-list
                    '(+ - * / = < > <= >=
                      zero? add1 sub1 abs max min
                      not boolean? boolean=? false? xor immutable?
                      number? number->string string->number real? inexact-real?
                      inexact? inexact->exact exact->inexact real->double-flonum
                      nan? infinite? positive-integer? negative-integer?
                      nonpositive-integer? nonnegative-integer? natural?
                      sqr sqrt integer-sqrt integer-sqrt/remainder expt exp log
                      sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh
                      bitwise-ior bitwise-and bitwise-xor bitwise-not
                      bitwise-bit-set? bitwise-first-bit-set bitwise-bit-field
                      arithmetic-shift integer-length
                      flonum? double-flonum? single-flonum? single-flonum-available?
                      fl+ fl- fl* fl/ fl= fl< fl> fl<= fl>= flabs flround flfloor
                      flceiling fltruncate flsingle flbit-field flsin flcos fltan
                      flasin flacos flatan flsinh flcosh fltanh flasinh flacosh
                      flatanh fllog flexp flsqrt flmin flmax flexpt
                      fixnum? fxzero? fx+ fx- fx* fx= fx> fx< fx<= fx>= fxquotient
                      unsafe-fxquotient fxremainder fxmodulo fxabs fxand fxior fxxor
                      fxnot fxlshift fxrshift fxpopcount fxpopcount32 fxpopcount16
                      fx+/wraparound fx-/wraparound fx*/wraparound
                      fxlshift/wraparound fxrshift/logical fxmin fxmax
                      fx->fl ->fl fl->fx fl->exact-integer
                      string? string-length string-ref string=?
                      string<? string>? string<=? string>=?
                      string-ci=? string-ci<? string-ci<=? string-ci>? string-ci>=?
                      string->immutable-string string->list list->string
                      bytes? bytes-length bytes-ref bytes=? bytes<? bytes>?
                      bytes->immutable-bytes byte?
                      char? char->integer integer->char char-utf-8-length
                      char=? char<? char<=? char>? char>=? char-alphabetic?
                      char-lower-case? char-upper-case? char-title-case?
                      char-numeric? char-symbolic? char-punctuation? char-graphic?
                      char-whitespace? char-grapheme-break-property char-grapheme-step
                      char-general-category char-blank? char-iso-control?
                      char-extended-pictographic? char-upcase char-downcase
                      char-titlecase char-foldcase char-ci=? char-ci<?
                      char-ci<=? char-ci>? char-ci>=?
                      symbol? symbol=? symbol-interned? symbol<?
                      keyword? keyword<? cons? empty? first rest second third fourth
                      fifth sixth seventh eighth ninth tenth eleventh twelfth
                      thirteenth fourteenth fifteenth last last-pair list-ref
                      list-tail member memq memv memw memf findf assq assv assw
                      assoc assf argmax argmin vector? vector-length
                      unsafe-vector-length unsafe-vector-ref vector-empty?
                      hash-eq? hash-eqv? hash-equal? hash-equal-always? hash-empty?
                      hash-count procedure? procedure-arity arity-at-least?
                      arity-at-least-value eof-object? port? input-port? output-port?
                      port-closed? string-port? srcloc? srcloc-source srcloc-line
                      srcloc-column srcloc-position srcloc-span object-name
                      path? path-for-some-system? path-string? path<?
                      file-exists? directory-exists? link-exists?
                      file-or-directory-type file-size
                      values))])
            (hash-set! ht x 'pure))
          (for ([x (in-list '(box box-immutable cons hasheq hasheqv list vector))])
            (hash-set! ht x 'allocates))
          (for ([x (in-list '(unsafe-car unsafe-cdr))])
            (hash-set! ht x 'ordered))
          ht))
      (define (letrec-primitive-order-class x)
        (hash-ref letrec-primitive-order-classes
                  (syntax-e (variable-id x))
                  #f))
      (define (letrec-simple-primitive-application? rator rands)
        (and (variable? rator)
             (primitive? (variable-id rator))
             (not (special-primitive? rator))
             (let ([rator-kind (letrec-primitive-order-class rator)])
               (and (memq rator-kind '(pure allocates))
                    (let ([args-kind (Expr* rands)])
                      (and args-kind
                           (join-simple-kinds rator-kind args-kind)))))))
      (define (Expr e)
        (nanopass-case (LFE2+ Expr) e
          [(quote ,s ,d)            'pure]
          [(quote-syntax ,s ,d)     'pure]
          [,x                       (and (not (set-in? x lhs-set)) 'pure)]
          [(top ,s ,x)              'pure]
          [(if ,s ,e0 ,e1 ,e2)      (Expr* (list e0 e1 e2))]
          [(begin ,s ,e0 ,e1 ...)   (Expr* (cons e0 e1))]
          [(begin0 ,s ,e0 ,e1 ...)  (Expr* (cons e0 e1))]
          [(app ,s ,e0 ,e1 ...)     (letrec-simple-primitive-application? e0 e1)]
          [else                     #f]))
      (and (lhs-free? e lhs-set)
           (Expr e)))
    (define (lambda-clause? xs e)
      (and (= (length xs) 1)
           (nanopass-case (LFE2+ Expr) e
             [(λ ,s ,f ,e0) #t]
             [(case-lambda ,s ,ab ...) #t]
             [else #f])))
    (define (split-lambda-values-clause xs rhs assigned)
      (and (all-unassigned-bindings? xs assigned)
           (nanopass-case (LFE2+ Expr) rhs
             [(app ,s ,e0 ,e1 ...)
              (and (variable? e0)
                   (free-identifier=? (variable-id e0) #'values)
                   (= (length xs) (length e1))
                   (for/and ([e (in-list e1)])
                     (nanopass-case (LFE2+ Expr) e
                       [(λ ,s ,f ,e0) #t]
                       [(case-lambda ,s ,ab ...) #t]
                       [else #f]))
                   (for/list ([x (in-list xs)]
                              [e (in-list e1)])
                     (list 'lambda (list x) e)))]
             [else
              #f])))
    (define (fresh-variable-like x)
      (new-var x))
    (define (Unsafe-Undefined)
      (with-output-language (LFE2+ Expr)
        `(quote ,h ,(datum h datum:unsafe-undefined))))
    (define (Zero)
      (with-output-language (LFE2+ Expr)
        `(quote ,h ,(datum h 0))))
    (define (Begin s es)
      (with-output-language (LFE2+ Expr)
        (match es
          [(list e)          `,e]
          [(list e0 e1 ...)  `(begin ,s ,e0 ,e1 ...)])))
    (define (init-set-syntax s)
      (if (syntax? s)
          (syntax-property s letrec-initialization-set-key #t)
          s))
    (define (InitSet s x t)
      (assigned-analysis-mark-assigned! (current-assigned-analysis) x)
      (with-output-language (LFE2+ Expr)
        `(set! ,(init-set-syntax s) ,x ,t)))
    (define (InitializeClause s xs e)
      (with-output-language (LFE2+ Expr)
        (match xs
          ['()
           `(let-values ,s ([() ,e]) ,(Zero))]
          [(list x)
           (define t (fresh-variable-like x))
           `(let-values ,s ([(,t) ,e])
              (begin ,s ,(InitSet s x t) ,(Zero)))]
          [_
           (define ts (map fresh-variable-like xs))
           (define init-sets
             (for/list ([x (in-list xs)]
                        [t (in-list ts)])
               (InitSet s x t)))
           `(let-values ,s ([(,ts ...) ,e])
              ,(Begin s (append init-sets (list (Zero)))))])))
    (define (EvaluateClause s xs e)
      (with-output-language (LFE2+ Expr)
        `(let-values ,s ([(,xs ...) ,e]) ,(Zero))))
    (define (single-unassigned-binding? xs assigned)
      (and (= (length xs) 1)
           (not (var-assigned? assigned (first xs)))))
    (define (all-unassigned-bindings? xs assigned)
      (for/and ([x (in-list xs)])
        (not (var-assigned? assigned x))))
    (define (simple-values-kind xs rhs lhs-set assigned)
      (define (join-mv-kind kind mv-kind)
        (cond
          [(or (not kind) (not mv-kind)) #f]
          [(or (eq? kind 'allocates)
               (eq? mv-kind 'allocates))
           'allocates]
          [else
           'pure]))
      (define (MV rhs)
        (nanopass-case (LFE2+ Expr) rhs
          [(app ,s ,e0 ,e1 ...)
           (and (variable? e0)
                (free-identifier=? (variable-id e0) #'values)
                (= (length xs) (length e1))
                (for/fold ([kind 'pure]) ([e (in-list e1)])
                  (join-mv-kind kind (effect-free-simple-kind e lhs-set))))]
          [(begin ,s ,e0 ,e1 ...)
           (match (cons e0 e1)
             [(list prefix ... last)
              (join-mv-kind (for/fold ([kind 'pure]) ([e (in-list prefix)])
                              (join-mv-kind kind (effect-free-simple-kind e lhs-set)))
                            (MV last))])]
          [(begin0 ,s ,e0 ,e1 ...)
           (match (cons e0 e1)
             [(list first rest ...)
              (join-mv-kind (MV first)
                            (for/fold ([kind 'pure]) ([e (in-list rest)])
                              (join-mv-kind kind (effect-free-simple-kind e lhs-set))))])]
          [(if ,s ,e0 ,e1 ,e2)
           (join-mv-kind (effect-free-simple-kind e0 lhs-set)
                         (join-mv-kind (MV e1)
                                       (MV e2)))]
          [else
           #f]))
      (and (all-unassigned-bindings? xs assigned)
           (MV rhs)))
    (define (classify-clause xs rhs lhs-set referenced assigned)
      (cond
        [(and (for/and ([x (in-list xs)])
                (and (not (set-in? x referenced))
                     (not (var-assigned? assigned x)))))
         'unreferenced]
        [(and (single-unassigned-binding? xs assigned)
              (lambda-clause? xs rhs))
         'lambda]
        [(simple-values-kind xs rhs lhs-set assigned)
         => (lambda (kind)
              (case kind
                [(pure)      'simple-pure]
                [(allocates) 'simple-allocates]
                [else        'complex]))]
        [(and (single-unassigned-binding? xs assigned)
              (effect-free-simple-kind rhs lhs-set))
         => (lambda (kind)
              (case kind
                [(pure)      'simple-pure]
                [(allocates) 'simple-allocates]
                [else        'complex]))]
        [else
         'complex]))
    (define (clause-vars-set clauses)
      (for/fold ([xs empty-set]) ([clause (in-list clauses)])
        (for/fold ([xs xs]) ([x (in-list (second clause))])
          (set-add xs x))))
    (define (degrade-simple-pure-body-kind kind rhs ordered-lhs-set)
      (if (and (eq? kind 'simple-pure)
               (not (lhs-free? rhs ordered-lhs-set)))
          'complex
          kind))
    (define (assimilate-let-values-clause xs rhs lhs-set referenced assigned)
      (and (all-unassigned-bindings? xs assigned)
           (nanopass-case (LFE2+ Expr) rhs
             [(let-values ,s ([(,x* ...) ,e*] ...) ,e0)
              (define nested-classified
                (for/list ([xs (in-list x*)]
                           [rhs (in-list e*)])
                  (list (classify-clause xs rhs lhs-set referenced assigned)
                        xs
                        rhs)))
              (define ordered-nested-lhs-set
                (clause-vars-set
                 (filter (lambda (clause)
                           (eq? (first clause) 'simple-allocates))
                         nested-classified)))
              (define body-kind
                (degrade-simple-pure-body-kind
                 (classify-clause xs e0 lhs-set referenced assigned)
                 e0
                 ordered-nested-lhs-set))
              (and (for/and ([clause (in-list nested-classified)])
                     (memq (first clause) '(simple-pure simple-allocates lambda)))
                   (memq body-kind '(simple-pure simple-allocates lambda complex))
                   (append nested-classified
                           (list (list body-kind xs e0))))]
             [else
              #f])))
    (define (assimilate-letrec-values-clause xs rhs lhs-set referenced assigned)
      (and (all-unassigned-bindings? xs assigned)
           (nanopass-case (LFE2+ Expr) rhs
             [(letrec-values ,s ([(,x* ...) ,e*] ...) ,e0)
              (define nested-lambda-clauses
               (for/list ([xs (in-list x*)]
                          [rhs (in-list e*)])
                  (or (and (single-unassigned-binding? xs assigned)
                           (lambda-clause? xs rhs)
                           (list (list 'lambda xs rhs)))
                      (split-lambda-values-clause xs rhs assigned)
                      #f)))
              (define body-kind
                (classify-clause xs e0 lhs-set referenced assigned))
              (and (andmap values nested-lambda-clauses)
                   (memq body-kind '(simple-pure simple-allocates lambda complex))
                   (append (append* nested-lambda-clauses)
                           (list (list body-kind xs e0))))]
             [else
              #f])))
    (define (classify-clauses x e lhs-set referenced assigned)
      (define clauses (map list x e))
      (define (Loop pending classified)
        (cond
          [(null? pending)
           (reverse classified)]
          [else
           (match (car pending)
             [(list (? symbol? kind) xs rhs)
              #:when (memq kind '(unreferenced simple-pure simple-allocates lambda complex))
              (Loop (cdr pending)
                    (cons (list kind xs rhs) classified))]
             [(list xs rhs)
              (define split-lambdas
                (split-lambda-values-clause xs rhs assigned))
              (define assimilated
                (or split-lambdas
                    (assimilate-let-values-clause xs rhs lhs-set referenced assigned)
                    (assimilate-letrec-values-clause xs rhs lhs-set referenced assigned)))
              (cond
                [assimilated
                 (Loop (append assimilated (cdr pending)) classified)]
                [else
                 (define kind
                   (classify-clause xs rhs lhs-set referenced assigned))
                 (Loop (cdr pending)
                       (cons (list kind xs rhs) classified))])])]))
      (Loop clauses '()))
    (define (binding-vars-set xss)
      (for/fold ([xs empty-set]) ([x* (in-list xss)])
        (for/fold ([xs xs]) ([x (in-list x*)])
          (set-add xs x))))
    (define (rhs-vars-set rhs* var-proc)
      (for/fold ([xs empty-set]) ([rhs (in-list rhs*)])
        (set-union xs (var-proc rhs))))
    (define (var-assigned? assigned x)
      (cond
        [(assigned-analysis? assigned)
         (match (hash-ref (assigned-analysis-assigned assigned) x #f)
           [(var-info assigned?) assigned?]
           [_ #f])]
        [(hash? assigned)
         (match (hash-ref assigned x #f)
           [(var-info assigned?) assigned?]
           [_ #f])]
        [else
         (set-in? assigned x)]))
    (define (all-assigned-vars body rhs*)
      (set-union (assigned-vars body)
                 (rhs-vars-set rhs* assigned-vars)))
    (define (all-referenced-vars body rhs*)
      (set-union (referenced-vars body)
                 (rhs-vars-set rhs* referenced-vars)))
    (define (split-basic-clauses x e assigned)
      (append*
       (for/list ([xs  (in-list x)]
                  [rhs (in-list e)])
         (let ([split (split-lambda-values-clause xs rhs assigned)])
           (if split
               (for/list ([clause (in-list split)])
                 (rest clause))
               (list (list xs rhs)))))))
    (define (placeholder-bindings clauses)
      (for*/list ([clause (in-list clauses)]
                  [x      (in-list (first clause))])
        (list x (Unsafe-Undefined))))
    (define (single-clause-lambda-bindings clauses)
      (for/list ([clause (in-list clauses)])
        (list (first (first clause)) (second clause))))
    (define (Lower-basic s x e e0)
      (define assigned (or (current-assigned-analysis)
                           (all-assigned-vars e0 e)))
      (define clauses (split-basic-clauses x e assigned))
      (define-values (raw-lambda-clauses raw-complex-clauses)
        (partition (match-lambda
                     [(list xs rhs)
                      (and (lambda-clause? xs rhs)
                           (not (var-assigned? assigned (first xs))))])
                   clauses))
      (define-values (lambda-clauses complex-clauses)
        (if (null? raw-complex-clauses)
            (values raw-lambda-clauses '())
            (values '() clauses)))
      (with-output-language (LFE2+ Expr)
        (if (null? complex-clauses)
            (let ([lambda-xss  (map first  lambda-clauses)]
                  [lambda-rhss (map second lambda-clauses)])
              `(letrec-values ,s ([(,lambda-xss ...) ,lambda-rhss] ...)
                 ,e0))
            (let ()
              (define placeholder-clauses (placeholder-bindings complex-clauses))
              (define lambda-bindings (single-clause-lambda-bindings lambda-clauses))
              (define init-exprs
                (for/list ([clause (in-list complex-clauses)])
                  (InitializeClause s (first clause) (second clause))))
              (define body (Begin s (append init-exprs (list e0))))
              `(let-values ,s ([(,(map first placeholder-clauses))
                                ,(map second placeholder-clauses)] ...)
                 ,(if (null? lambda-bindings)
                      body
                      `(letrec-values ,s ([(,(map first lambda-bindings))
                                           ,(map second lambda-bindings)] ...)
                         ,body)))))))
    (define (Lower-waddell s x e e0)
      (define lhs-set (binding-vars-set x))
      (define referenced (all-referenced-vars e0 e))
      (define assigned (or (current-assigned-analysis)
                           (all-assigned-vars e0 e)))
      (define classified
        (classify-clauses x e lhs-set referenced assigned))
      (define (bindings-of kind)
        (for/list ([clause (in-list classified)]
                   #:when (eq? (first clause) kind))
          (rest clause)))
      (define allocating-clauses   (bindings-of 'simple-allocates))
      (define lambda-clauses       (bindings-of 'lambda))
      (define complex-clauses      (bindings-of 'complex))
      (define unreferenced-clauses (bindings-of 'unreferenced))
      (with-output-language (LFE2+ Expr)
      (define placeholder-clauses
        (placeholder-bindings (append allocating-clauses complex-clauses)))
      (define lambda-bindings
        (for/list ([clause (in-list lambda-clauses)])
          (list (first clause) (second clause))))
      (define lambda-lhs-set
        (binding-vars-set (map first lambda-clauses)))
      (define (simple-pure-after-lambda? clause)
        (match clause
          [(list 'simple-pure _xs rhs)
           (not (lhs-free? rhs lambda-lhs-set))]
          [_
           #f]))
      (define seq-exprs
        (for/list ([clause (in-list classified)]
                   #:unless (eq? (first clause) 'simple-pure)
                   #:unless (eq? (first clause) 'lambda))
            (match clause
              [(list 'simple-allocates xs rhs) (InitializeClause s xs rhs)]
              [(list 'complex xs rhs)      (InitializeClause s xs rhs)]
              [(list 'unreferenced xs rhs) (EvaluateClause s xs rhs)])))
      (define body0
          (if (null? seq-exprs)
              e0
              (Begin s (append seq-exprs (list e0)))))
      (define body0*
        (for/fold ([body body0])
                  ([clause (in-list (reverse classified))]
                   #:when (simple-pure-after-lambda? clause))
          (match clause
            [(list _kind xs rhs)
             `(let-values ,s ([(,xs ...) ,rhs]) ,body)])))
      (define body1
          (if (null? lambda-bindings)
              body0*
              (let ([lambda-xss  (map first  lambda-bindings)]
                    [lambda-rhss (map second lambda-bindings)])
                `(letrec-values ,s ([(,lambda-xss ...) ,lambda-rhss] ...)
                   ,body0*))))
      (define body2
          (if (null? placeholder-clauses)
              body1
              `(let-values ,s ([(,(map first placeholder-clauses))
                                ,(map second placeholder-clauses)] ...)
                 ,body1)))
      (for/fold ([body body2])
                  ([clause (in-list (reverse classified))]
                   #:when (memq (first clause) '(simple-pure))
                   #:unless (simple-pure-after-lambda? clause))
          (match clause
            [(list _kind xs rhs)
             `(let-values ,s ([(,xs ...) ,rhs]) ,body)])))))
  (TopLevelForm        : TopLevelForm        (T) -> TopLevelForm        ())
  (ModuleLevelForm     : ModuleLevelForm     (M) -> ModuleLevelForm     ())
  (GeneralTopLevelForm : GeneralTopLevelForm (G) -> GeneralTopLevelForm ())
  (Formals             : Formals             (F) -> Formals             ())
  (Expr : Expr (E) -> Expr ()
    [(letrec-values ,s ([(,x ...) ,[e]] ...) ,[e0])
     (case (current-letrec-strategy)
       [(basic)   (Lower-basic s x e e0)]
       [(waddell) (Lower-waddell s x e e0)]
       [(scc)     (Lower-waddell s x e e0)]
       [else
        (error 'lower-letrec-values
               "unknown letrec strategy: ~a"
               (current-letrec-strategy))])])
  (TopLevelForm T))


;;;
;;; ASSIGNMENT CONVERSION
;;;

;; Assignment conversion consists of two sub-passes:
;;     - collect-assignable-variables
;;     - box-mutables

;; The first pass returns an id-set of all variables that are (potentially) assigned to.
;; The second pass converts assignable variables into boxes and assignments into box mutations.

(define assignment-convertsion-for-top-level-vars? #f)

(define (assignment-conversion T) ; LFE2+ -> LFE2+
  ; more convenient to use assignment-conversion than
  ; calling collect-assignable-variables and box-mutables in order
  ; (since  assignment-conversion has type T -> T)
  (define ms
    (cond
      [(current-assigned-analysis)
       => assigned-analysis->id-set]
      [else
       (collect-assignable-variables T)]))
  (current-console-bridge-mutable-top-binding-names
   (for/list ([x (in-list (id-set->list ms))]
              #:do [(define sym (syntax-e (variable-id x)))]
              #:when (and (symbol? sym)
                          (member sym (current-console-bridge-top-binding-names))))
     sym))
  (box-mutables T ms))


(define-pass box-mutables : LFE2+ (T ms) -> LFE2+ ()
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
    (define (mutable?   x) (set-in? x ms))
    (define (immutable? x) (not (mutable? x)))    
    (define (formal-variables F)
      ; list of variables occuring as formal arguments
      (nanopass-case (LFE2+ Formals) F
        [(formals (,x ...))            x]
        [(formals (,x0 ,x1 ... . ,xd)) (cons x0 (append x1 (list xd)))]
        [(formals ,x)                  (list x)]))
    (define (Boxed e)   (with-output-language (LFE2+ Expr) `(app ,h ,(var:boxed)   ,e)))
    (define (Unboxed e) (with-output-language (LFE2+ Expr) `(app ,h ,(var:unboxed) ,e)))
    (define (Undefined) (with-output-language (LFE2+ Expr) `(quote ,h ,datum:undefined)))
    (define (letrec-initialization-set? s)
      (and (syntax? s)
           (syntax-property s letrec-initialization-set-key)))
    (define (LambdaBody s f fs e)
      ; fs are the variables to be bound in the body e
      (with-output-language (LFE2+ Expr)
        (define (Begin es)  (match es [(list e0 e1 ...) `(begin ,h ,e0 ,e1 ...)]))
        (match (set-disjoint? fs ms) 
          [#t `,e]
          [_ (Begin (append (for/list ([a (in-list (id-set->list (set-intersection fs ms)))])
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
                 (with-output-language (LFE2+ Expr)
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
    [(λ ,s ,[f] ,[e])   (let ([lb (LambdaBody s f (ids->id-set (formal-variables f)) e)])
                          `(λ ,s ,f ,lb))])
  (Expr : Expr (E) -> Expr ()
    ; variable reference, set! and all binding forms must be rewritten
    [,x                       (if (set-in? x ms) (Unboxed x) x)]
    [(set! ,s ,x ,[e])        `(app ,h ,(if (letrec-initialization-set? s)
                                            (var:initialize-boxed!)
                                            (var:set-boxed!))
                                      ,x ,e)]
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


(module+ test
  (let ()
    (define (test stx)
      (reset-counter!)
      (parameterize ([α-rename-mode 'simple])
        (unparse-all
         (unparse-LFE2+
          (assignment-conversion
           (α-rename
            (explicit-case-lambda
             (explicit-begin
              (flatten-topbegin
               (parse
                (expand-syntax stx)))))))))))
    
    (check-equal? (test #'(let-values ([(x) 0]) (set! x 1) x))
                  '(let-values (((x) (boxed '0))) (begin (set-boxed! x '1) (unboxed x))))
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
;; are kept (for now) (todo).

;; The terminal pr is a variable bound to a primitive.

(define-language LFE3 (extends LFE2+)
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

(define-pass categorize-applications : LFE2+ (T) -> LFE3 ()
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
               (flatten-topbegin
                (parse
                 (expand-syntax stx))))))))))))
    
    (check-equal? (test #'(+ 1 2)) '(primapp + '1 '2))
    (check-equal? (test #'(begin (define foo 3) (foo 1 2)))
                  ; At the top-level the expression is evaluated before
                  ; the binding is created.
                  ; The current compiler pipeline preserves the plain
                  ; top-level variable name here. Namespace-based #%top
                  ; lookup is not implemented yet.
                  '(topbegin (define-values (foo) '3)
                             (app foo '1 '2)))
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

; An atomic expression (AExpr) is:
;   - guaranteed to terminate
;   - causes no side effects
;   - causes no control effects
;   - never produces an error

; Non-atomic expressions are called complex expressions (CExpr).

; All subexpression are given a name unless
;   1) it is a RHS in a let-assignment (it already has a name)
;   2) it is a tail expression         (avoid building context)
;   3) it is an atomic expression
;   4) the value will be ignored       (non-last expressions in a begin)

; Introduce administrative normal form
(define-language LANF (extends LFE3)
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
  (define (with-source src stx)
    (syntax-property stx 'source-stx src))
  (define (make-h s)
    (if (syntax? s)
        (with-source s #'an)
        #'an))
  
  (define (TopLevelForm*    ts)   (map TopLevelForm    ts))
  (define (ModuleLevelForm* mfs)  (map ModuleLevelForm mfs))
  (define (RawProvideSpec*  rpss) (map RawProvideSpec  rpss))
  
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
        [(#%provide ,rps ...)  `(#%provide ,(RawProvideSpec* rps) ...)]
        [,g                     (GeneralTopLevelForm g)])))

  (define (GeneralTopLevelForm G)
    (with-output-language (LANF GeneralTopLevelForm)
      (nanopass-case (LFE3 GeneralTopLevelForm) G
        [,e                                (Expr e id)]
        [(define-values   ,s (,x ...) ,e)  `(define-values   ,s (,x ...) ,(RHS s e id))]
        [(define-syntaxes ,s (,x ...) ,e)  `(define-syntaxes ,s (,x ...) ,(RHS s e id))]
        [(#%require       ,s ,rrs ...)     `(#%require ,s ,(map RawRequireSpec rrs) ...)]
        [else (error 'anormalize-GeneralTopLevelForm
                     "expected general top-level form, got ~a"
                     G)])))

  (define (RawRequireSpec RRS)
    (with-output-language (LANF RawRequireSpec)
      (nanopass-case (LFE3 RawRequireSpec) RRS
        [(for-meta ,pl ,rrs ...)  `(for-meta ,pl ,(map RawRequireSpec rrs) ...)] 
        [,ps                      (PhaselessSpec ps)])))

        ; [,rrmp                    (RawRootModulePath rrmp)]

  ;; (RawRequireSpec (rrs) ; todo: there are more types of require specs
  ;;   (for-meta pl rrs ...) => (for-meta pl rrs ...)
  ;;   ps) ; note: was rrmp 

  
  (define (RawRootModulePath RRMP)
    (with-output-language (LANF RawRootModulePath)
      (nanopass-case (LFE3 RawRootModulePath) RRMP
        [(quote ,x)  `(quote ,x)])))

  (define (RawProvideSpec RPS)
    (with-output-language (LANF RawProvideSpec)
      (nanopass-case (LFE3 RawProvideSpec) RPS
        [,ps (PhaselessSpec ps)])))

  (define (PhaselessSpec PS)
    (with-output-language (LANF PhaselessSpec)
      (nanopass-case (LFE3 PhaselessSpec) PS
        [,sls (SpacelessSpec sls)])))
                     
  (define (SpacelessSpec SS)
    (with-output-language (LANF SpacelessSpec)
      (nanopass-case (LFE3 SpacelessSpec) SS
        [[,x0 ,x1] `[,x0 ,x1]])))

  
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
  
  (define (RHS s E k)
    (nanopass-case (LFE3 Expr) E
      [,ab  (Abstraction ab k)]
      [,cab (CaseAbstraction cab k)]
      [else (Expr (with-output-language (LFE3 Expr)
                    (let ([h (make-h s)])
                      `(closedapp ,h (λ ,h ,(LFE3-Formals '()) ,E)))) k)]))
  
  (define (RHS* s es k)
    (cond
      [(null? es)  (k '())]
      [else        (RHS s (first es)
                     (λ (e0) (RHS* s (cdr es)
                               (λ (es) (k (cons e0 es))))))]))
  
  (define (Expr E k)
    ;(displayln (list 'anormalize-Expr E)) (newline)
    (with-output-language (LANF CExpr)
      (nanopass-case (LFE3 Expr) E
        [(if ,s ,e0 ,e1 ,e2)
         (Expr/name s e0 (λ (ae0) (k `(if ,s ,ae0 ,(Expr e1 id) ,(Expr e2 id)))))]
        [(set! ,s ,x ,e)
         (Expr/name s e (λ (ae) (k `(set! ,s ,x ,ae))))]
        [(let-values ,s ([(,x ...) ,e] ...) ,e0)
         ; ad 1) don't name e ... 
         (RHS* s e (λ (ce) (with-output-language (LANF Expr)
                           `(let-values    ,s ([(,x ...) ,ce] ...) ,(Expr e0 k)))))]
        [(letrec-values ,s ([(,x ...) ,e] ...) ,e0)
         (RHS* s e (λ (ce) (with-output-language (LANF Expr)
                           `(letrec-values ,s ([(,x ...) ,ce] ...) ,(Expr e0 k)))))]
        [(primapp   ,s ,pr ,e1 ...)
         (Expr*/names e1 (λ (ae1) (k `(primapp   ,s ,pr ,ae1 ...))))]
        [(closedapp ,s ,ab ,e1 ...)
         (Expr*/names e1 (λ (ae1) (k `(closedapp ,s ,(Abstraction ab id) ,ae1 ...))))]
        [(app       ,s ,e0 ,e1 ...)
         (Expr/name s e0 (λ (ae0) (Expr*/names e1 (λ (ae1) (k `(app ,s ,ae0 ,ae1 ...))))))]
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
        [(begin ,s ,e0)          (Expr e0 k)]
        [(begin ,s ,e0 ,e1 ...)  (define (Expr/id e) (Expr e identity))
                                 (let ([e0 (Expr/id e0)] [e1 (map Expr/id e1)])                                  
                                  (k (with-output-language (LANF Expr)
                                       `(begin ,s ,e0 ,e1 ...))))]        
        [(begin0 ,s ,e0)         (Expr e0 k)]
        [(begin0 ,s ,e0 ,e1 ...) (define (Expr/id e) (Expr e identity))
                                 (let ([e0 (Expr/id e0)] [e1 (map Expr/id e1)])
                                   (k (with-output-language (LANF Expr)
                                        `(begin0 ,s ,e0 ,e1 ...))))]
        [(wcm ,s ,e0 ,e1 ,e2)
         (Expr/name s e0
           (λ (ae0) (Expr/name s e1
                      (λ (ae1)
                        (k `(wcm ,s ,ae0 ,ae1 ,(Expr e2 id)))))))]
        [else
         (displayln (list 'anormalize-Expr "got" E))
         (error 'anormalize-Expr "internal error")])))

  ; Expr/name : syntax Expr (AExpr -> Expr) -> Expr
  (define (Expr/name s e k)
    ;(displayln (list 'anormalize-Expr/name e)) (newline)
    ; Transform e, then name it (unless it is an atomic expression),
    ; then call k with the name or the atomic expression
    (Expr e (λ (e)
              (nanopass-case (LANF Expr) e
                [,ae   (k ae)]               ; ad 3) don't name atomic expressions
                [else  (let ([t (new-var)])
                         (with-output-language (LANF Expr)
                           `(let-values ,(make-h s) ([(,t) ,e])
                              ,(k t))))]))))

  (define (Expr*/names es k)
    (cond
      [(null? es)  (k '())]
      [else        (Expr/name #f (car es)
                     (λ (t) (Expr*/names (cdr es)
                              (λ (ts) (k (cons t ts))))))]))
  (TopLevelForm T))


(module+ test
  (let ()
    (define (lower-test stx [strategy 'basic])
      (reset-counter!)
      (parameterize ([α-rename-mode 'simple]
                     [current-letrec-strategy strategy])
        (define ar
          (α-rename
           (explicit-case-lambda
            (explicit-begin
             (infer-names
              (flatten-topbegin
               (parse
                (expand-syntax stx))))))))
        (parameterize ([current-assigned-analysis (uncover-assigned! ar)])
          (unparse-all
           (unparse-LFE2+
            (lower-letrec-values ar))))))
    (define (check-complex-waddell stx rhs-rx)
      (define lowered
        (format "~s"
                (lower-test stx 'waddell)))
      (check-true
       (regexp-match? #rx"\\(quote unsafe-undefined[0-9]+\\)"
                      lowered))
      (check-true
       (regexp-match? (regexp rhs-rx)
                      lowered))
      (check-true
       (regexp-match? #rx"\\(set! x x[.0-9]+\\)"
                      lowered)))
    (define (test stx)
      (reset-counter!)
      (parameterize ([α-rename-mode 'simple])
        (define ar
          (α-rename
           (explicit-case-lambda
            (explicit-begin
             (infer-names
              (flatten-topbegin
               (parse
                (expand-syntax stx))))))))
        (define ua (uncover-assigned! ar))
        (define lr
          (parameterize ([current-assigned-analysis ua])
            (lower-letrec-values ar)))
        (unparse-all
         (unparse-LANF
          (anormalize
           (categorize-applications
            (parameterize ([current-assigned-analysis ua])
              (assignment-conversion lr))))))))
    (check-equal? (test #'1) ''1)
    (check-equal? (test #'(+ 2 3)) '(primapp + '2 '3))
    (check-equal? (test #'(+ 2 (* 4 5)))
                  '(let-values (((t.1) (primapp * '4 '5))) (primapp + '2 t.1)))
    (check-equal? (test #'(begin (+ 2 (* 4 5)) (+ (* 6 7) (/ 8 9))))
                  '(topbegin
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
                  '(letrec-values (((fact)
                                     (λ (n)
                                       (let-values (((t.1) (primapp = n '0)))
                                         (if t.1 '1 (let-values (((t.2) (primapp - n '1)))
                                                      (let-values (((t.3) (app fact t.2)))
                                                        (primapp * n t.3))))))))
                     (app fact '5)))
    (check-true
     (regexp-match?
      #rx"^\\(let-values \\(\\(\\(x\\) \\(quote unsafe-undefined[0-9]+\\)\\) \\(\\(f\\) \\(quote unsafe-undefined[0-9]+\\)\\)\\) \\(begin \\(let-values \\(\\(\\(x[.0-9]+\\) \\(quote 1\\)\\)\\) \\(begin \\(set! x x[.0-9]+\\) \\(quote 0\\)\\)\\) \\(let-values \\(\\(\\(f[.0-9]+\\) \\(λ \\(\\) x\\)\\)\\) \\(begin \\(set! f f[.0-9]+\\) \\(quote 0\\)\\)\\) \\(f\\)\\)\\)$"
      (format "~s"
              (lower-test #'(letrec ([x 1]
                                     [f (λ () x)])
                              (f))
                          'basic))))
    (check-equal? (lower-test #'(letrec ([x 1]
                                         [f (λ () x)])
                                  (f))
                              'waddell)
                  '(let-values (((x) '1))
                     (letrec-values (((f) (λ () x)))
                       (f))))
    (check-true
     (regexp-match?
      #rx"^\\(let-values \\(\\(\\(f\\) \\(quote unsafe-undefined[0-9]+\\)\\)\\) \\(begin \\(let-values \\(\\(\\(f[.0-9]+\\) \\(λ \\(g\\) \\(begin \\(set! f g\\) \\(f\\)\\)\\)\\)\\) \\(begin \\(set! f f[.0-9]+\\) \\(quote 0\\)\\)\\) \\(f \\(λ \\(\\) \\(quote 12\\)\\)\\)\\)\\)$"
      (format "~s"
              (lower-test #'(letrec ([f (λ (g) (set! f g) (f))])
                              (f (λ () 12)))
                          'basic))))
    (check-equal? (lower-test #'(letrec ([x (begin 1 2)]
                                         [f (λ () 3)]
                                         [u (begin 4 5)])
                                  (f))
                              'waddell)
                  '(letrec-values (((f) (λ () '3)))
                     (begin
                       (let-values (((x) (begin '1 '2))) '0)
                       (let-values (((u) (begin '4 '5))) '0)
                       (f))))
    (check-equal? (lower-test #'(letrec ([x (begin0 1 2)]) x)
                              'waddell)
                  '(let-values (((x) (begin0 '1 '2)))
                     x))
    (check-equal? (lower-test #'(letrec-values ([(x y) (values 1 2)])
                                 (+ x y))
                              'waddell)
                  '(let-values (((x y) (values '1 '2)))
                     (+ x y)))
    (check-equal? (lower-test #'(letrec-values ([(x y) (begin 0 (values 1 2))])
                                 (+ x y))
                              'waddell)
                  '(let-values (((x y) (begin '0 (values '1 '2))))
                     (+ x y)))
    (check-equal? (lower-test #'(letrec-values ([(x y) (if #t
                                                           (values 1 2)
                                                           (values 3 4))])
                                 (+ x y))
                              'waddell)
                  '(let-values (((x y) (if '#t
                                           (values '1 '2)
                                           (values '3 '4))))
                     (+ x y)))
    (check-equal? (lower-test #'(letrec-values ([(x y) (begin0 (values 1 2) 9)])
                                 (+ x y))
                              'waddell)
                  '(let-values (((x y) (begin0 (values '1 '2) '9)))
                     (+ x y)))
    (check-equal? (lower-test #'(letrec-values ([(f g) (values (λ () 1)
                                                               (λ () 2))])
                                 (list (f) (g)))
                              'waddell)
                  '(letrec-values (((f) (λ () '1))
                                   ((g) (λ () '2)))
                     (list (f) (g))))
    (check-equal? (lower-test #'(letrec-values ([(f g) (values (case-lambda
                                                                 [() 1])
                                                                (case-lambda
                                                                  [() 2]))])
                                 (list (f) (g)))
                              'waddell)
                  '(letrec-values (((f) (case-lambda
                                          (λ () '1)))
                                   ((g) (case-lambda
                                          (λ () '2))))
                     (list (f) (g))))
    (check-equal? (lower-test #'(letrec ([f (case-lambda [() 1])])
                                  (f))
                              'basic)
                  '(letrec-values (((f) (case-lambda
                                          (λ () '1))))
                     (f)))
    (check-equal? (lower-test #'(letrec ([f (case-lambda [() 1])])
                                  (f))
                              'waddell)
                  '(letrec-values (((f) (case-lambda
                                          (λ () '1))))
                     (f)))
    (check-equal? (lower-test #'(letrec-values ([(x y) (let-values ([(a b) (values 1 2)])
                                                  (values a b))])
                                 (+ x y))
                              'waddell)
                  '(let-values (((a b) (values '1 '2)))
                     (let-values (((x y) (values a b)))
                       (+ x y))))
    (check-true
     (regexp-match?
      #rx"^\\(let-values \\(\\(\\(x\\) \\(quote unsafe-undefined[0-9]+\\)\\) \\(\\(y\\) \\(quote unsafe-undefined[0-9]+\\)\\)\\) \\(begin \\(let-values \\(\\(\\(x[.0-9]+ y[.0-9]+\\) \\(values \\(cons \\(quote 1\\) \\(quote 2\\)\\) \\(quote 3\\)\\)\\)\\) \\(begin \\(set! x x[.0-9]+\\) \\(set! y y[.0-9]+\\) \\(quote 0\\)\\)\\) \\(cons x y\\)\\)\\)$"
      (format "~s"
              (lower-test #'(letrec-values ([(x y) (values (cons 1 2) 3)])
                              (cons x y))
                          'waddell))))
    (check-true
     (regexp-match?
      #rx"^\\(let-values \\(\\(\\(a\\) \\(quote unsafe-undefined[0-9]+\\)\\) \\(\\(b\\) \\(quote unsafe-undefined[0-9]+\\)\\) \\(\\(x\\) \\(quote unsafe-undefined[0-9]+\\)\\) \\(\\(y\\) \\(quote unsafe-undefined[0-9]+\\)\\)\\) \\(begin \\(let-values \\(\\(\\(a[.0-9]+ b[.0-9]+\\) \\(values \\(cons \\(quote 1\\) \\(quote 2\\)\\) \\(quote 3\\)\\)\\)\\) \\(begin \\(set! a a[.0-9]+\\) \\(set! b b[.0-9]+\\) \\(quote 0\\)\\)\\) \\(let-values \\(\\(\\(x[.0-9]+ y[.0-9]+\\) \\(values a b\\)\\)\\) \\(begin \\(set! x x[.0-9]+\\) \\(set! y y[.0-9]+\\) \\(quote 0\\)\\)\\) \\(cons x y\\)\\)\\)$"
      (format "~s"
              (lower-test #'(letrec-values ([(x y) (let-values ([(a b) (values (cons 1 2) 3)])
                                                   (values a b))])
                              (cons x y))
                          'waddell))))
    (check-equal? (lower-test #'(letrec-values ([(f g) (letrec-values ([(a b) (values (λ () 1)
                                                                                         (λ () 2))])
                                                  (values a b))])
                                 (list (f) (g)))
                              'waddell)
                  '(letrec-values (((a) (λ () '1))
                                   ((b) (λ () '2)))
                     (let-values (((f g) (values a b)))
                       (list (f) (g)))))
    (check-equal? (lower-test #'(letrec-values ([(f g) (letrec-values ([(a b) (values (case-lambda
                                                                                          [() 1])
                                                                                         (case-lambda
                                                                                           [() 2]))])
                                                   (values a b))])
                                 (list (f) (g)))
                              'waddell)
                  '(letrec-values (((a) (case-lambda
                                          (λ () '1)))
                                   ((b) (case-lambda
                                          (λ () '2))))
                     (let-values (((f g) (values a b)))
                       (list (f) (g)))))
    (check-equal? (lower-test #'(letrec ([x (let-values ([(a) 1]) a)])
                                  x)
                              'waddell)
                  '(let-values (((a) '1))
                     (let-values (((x) a))
                       x)))
    (check-equal? (lower-test #'(letrec ([f (let-values ([(a) 1]) (λ () a))])
                                  (f))
                              'waddell)
                  '(let-values (((a) '1))
                     (letrec-values (((f) (λ () a)))
                       (f))))
    (check-true
     (regexp-match?
      #rx"^\\(let-values \\(\\(\\(a\\) \\(quote unsafe-undefined[0-9]+\\)\\) \\(\\(x\\) \\(quote unsafe-undefined[0-9]+\\)\\)\\) \\(begin \\(let-values \\(\\(\\(a[.0-9]+\\) \\(cons \\(quote 1\\) \\(quote 2\\)\\)\\)\\) \\(begin \\(set! a a[.0-9]+\\) \\(quote 0\\)\\)\\) \\(let-values \\(\\(\\(x[.0-9]+\\) a\\)\\) \\(begin \\(set! x x[.0-9]+\\) \\(quote 0\\)\\)\\) x\\)\\)$"
      (format "~s"
              (lower-test #'(letrec ([x (let-values ([(a) (cons 1 2)]) a)])
                              x)
                          'waddell))))
    (check-equal? (lower-test #'(letrec ([f (letrec ([a (λ () 1)])
                                               (λ () a))])
                                  (f))
                              'waddell)
                  '(letrec-values (((a) (λ () '1))
                                   ((f) (λ () a)))
                     (f)))
    (check-true
     (regexp-match?
      #rx"^\\(let-values \\(\\(\\(x\\) \\(quote unsafe-undefined[0-9]+\\)\\)\\) \\(letrec-values \\(\\(\\(a\\) \\(λ \\(\\) \\(quote 1\\)\\)\\)\\) \\(begin \\(let-values \\(\\(\\(x[.0-9]+\\) \\(cons \\(a\\) \\(quote 2\\)\\)\\)\\) \\(begin \\(set! x x[.0-9]+\\) \\(quote 0\\)\\)\\) x\\)\\)\\)$"
      (format "~s"
              (lower-test #'(letrec ([x (letrec ([a (λ () 1)])
                                          (cons (a) 2))])
                              x)
                          'waddell))))
    (check-complex-waddell #'(letrec ([x (random)]) x)
                           "\\(random\\)")
    (check-complex-waddell #'(letrec ([x (gensym)]) x)
                           "\\(gensym\\)")
    (check-complex-waddell #'(letrec ([x (current-directory)]) x)
                           "\\(current-directory\\)")
    (check-complex-waddell #'(letrec ([x (current-input-port)]) x)
                           "\\(current-input-port\\)")
    (check-complex-waddell #'(letrec ([x (cons 1 2)]) x)
                           "\\(cons \\(quote 1\\) \\(quote 2\\)\\)")
    (check-complex-waddell #'(letrec ([x (list 1 2)]) x)
                           "\\(list \\(quote 1\\) \\(quote 2\\)\\)")
    (check-complex-waddell #'(letrec ([x (vector 1 2)]) x)
                           "\\(vector \\(quote 1\\) \\(quote 2\\)\\)")
    (check-complex-waddell #'(letrec ([x (car '(1 . 2))]) x)
                           (regexp-quote "(car (quote (1 . 2)))"))
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
    ; hash table from an abstraction to its free variables
    (define ht (make-hasheq))
    (define (add! ab xs) (hash-set! ht ab xs))
    (define (get ab)     (hash-ref  ht ab))
    (define (formal-variables F)
      (nanopass-case (LANF Formals) F
        [(formals (,x ...))            (ids->id-set x)]
        [(formals (,x0 ,x1 ... . ,xd)) (set-union (make-id-set x0 xd) (ids->id-set x1))]
        [(formals ,x)                  (make-id-set x)]))
    (define bound-at-top #f)
    (define (bound-at-top-level ts)
      ; return the set of all xs occuring in a 
      ;     (define-values   ,s (,x ...) ,[e xs])
      ; Note: The method of collecting below assumes that `topbegin`
      ;       has been flattened.
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
                        primitives)))
    (define (non-literal-constants->id-set)
      (ids->id-set (map (λ (sym) (variable (datum->syntax #'dfv sym)))
                        non-literal-constants))))
  
  (Formals : Formals (F) -> Formals ())
  (TopLevelForm : TopLevelForm (T xs) -> TopLevelForm (xs)
    [(topbegin ,s ,[t xs] ...)      (values T (set-difference
                                               (set-union* xs)
                                               (set-union bound-at-top #;(bound-at-top-level t)
                                                          ; todo: only import primitives that are declared
                                                          (set-union (primitives->id-set)
                                                                     (non-literal-constants->id-set)))))]
    [(topmodule ,s ,mn ,mp ,mf ...) (for ([m mf]) ; todo: find all identifiers defined at the module level
                                      (ModuleLevelForm m empty-set)) ; mark abstractions in the module
                                    (values T empty-set)]            ; no free variables in module
    [(#%expression ,s ,[e xs])      (values T xs)]
    [,g                             (GeneralTopLevelForm g xs)])
  (ModuleLevelForm : ModuleLevelForm (M xs) -> ModuleLevelForm (xs)
    [,g                        (GeneralTopLevelForm g xs)])
  (GeneralTopLevelForm : GeneralTopLevelForm (G xs) -> GeneralTopLevelForm (xs)
    [(define-values   ,s (,x ...) ,[e xs]) (values G (set-difference xs (ids->id-set x)))]
    [(define-syntaxes ,s (,x ...) ,[e xs]) (values G (set-difference xs (ids->id-set x)))] ; todo
    [,e (Expr e xs)])
  (Abstraction : Abstraction (AB xs) -> Abstraction (xs)
    [(λ ,s ,f ,[e xs])        (let ([xs (set-difference xs 
                                                        (set-union (formal-variables f)
                                                                   bound-at-top))])
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
    [,x       (define sym (variable-id x))
              (cond
                [(primitive? sym)            (values AE empty-set)]         ; immediate constants
                #;[(non-literal-constant? sym) (values AE empty-set)]         ; value stored in a global
                [else                        (values AE (make-id-set x))])] ; non-primitive
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

  ;; 1. Find all top-level variables  
  (set! bound-at-top
        (let loop ([T T])
          (nanopass-case (LANF TopLevelForm) T
            [(topbegin ,s ,t ...)
             (set-union* (map loop t))]
            [,g
             (bound-at-top-level (list g))]
            [else
             ; #%expresion and topmodule
             empty-set])))
  
  ;; 2. Compute free variables
  (letv ((T xs) (TopLevelForm T (make-id-set)))    
    (unless (set-empty? xs)
      (displayln "\n---\n")
      (displayln bound-at-top)
      ; (displayln "\n---\n")
      ; (pretty-print (unparse-LANF T)) (newline)
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
;;;       (λ (x ...) e)    =>  (closure in label1 f ...)
;;;                            where f ... are the free variables of the abstraction
;;;                            and `in` is an inferred name (if present)
;;;   Rewrite references to free variables:
;;;       f                =>  (free-ref cl i)
;;;                            where i is the index of f in the list of free variables

(define (closure-conversion T)
  (define labels-ht (make-hasheq))
  (define (add-label! ab) (hash-set! labels-ht ab (new-var "label")))
  (letv ((T free-ht abs) (determine-free-variables T))
    #;(pretty-print free-ht)
    (for ([ab abs])
      (add-label! ab))
    (finish-closure-conversion T free-ht labels-ht)))

(define (closure+primitive? v)
  (or (eq? v 'closure)
      (primitive? v)))

(define (natural? v) (and (integer? v) (not (negative? v))))

(define (inferred-name? v)
  ; either #f or a variable
  (or (not v) (variable? v)))

(define arity? integer?)

(define-language LANF+closure (extends LANF)
  (terminals
   (- (primitive (pr)))
   (+ (closure+primitive (pr)))
   (+ (arity (ar)))
   (- (variable (x xd)))
   (+ (variable (x xd l)) => unparse-variable)  ; l for label
   (+ (natural (i)))                            ; i for index  
   (+ (inferred-name (in))))
  (Abstraction (ab)
    (- (λ s f e)))
  (CaseAbstraction (cab)
    (- (case-lambda s ab ...)))
  (ClosureAllocation (ca)
    (+ (closure s in l ar ae1 ...)    => (closure in l ae1 ...))) ; x is the inferred name
  (CaseClosureAllocation (cca)
    (+ (case-closure s in l [ar ca] ...) => (case-closure in [ar ca] ...)))
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
    (define (free-of ab)  (id-set->list (hash-ref free-ht ab)))
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
      (record-label-map! label s)
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
    (define (inferred-name s)
      ; The inferred name (if present) was added to the
      ; syntax object in the pass `infer-names`.
      (syntax-case s ()
        [(inferred-name? x s)
         (eq? (syntax-e #'inferred-name?) 'inferred-name)
         (syntax-e #'x)]
        [_ #f]))
    (define current-free (make-parameter '())))
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
       (define name (inferred-name s))
       `(closure ,h ,name ,l ,(formals->arity f) ,xs ...))])
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
                    `(quote ,#'h ,(datum #'42 42)))]) ; TODO TODO 
       #;(case-lift! l fs* E)  ; TODO <---- is uncommenting this correct?
       (define name (inferred-name s))
       `(case-closure ,h ,name ,l [,ar ,ab] ...))])
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

; The goal of this pass is to categorize variables according to the
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
    (define (RawProvideSpec*      RPSs) (for-each RawProvideSpec      RPSs))

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

    ; variable x is provided from module with name mn
    (define provides '())
    (define (provide! mn x-from x-to)
      ; x-from : original name
      ; x-to   : name after alpha-renaming
      (set! provides (cons (list mn x-from x-to) provides)))
    
    (define prov! (make-parameter #f))    ; begin at top-level
    (define add!  (make-parameter top!*)))
    
  (TopLevelForm : TopLevelForm (T) -> * ()
    [(define-label ,l ,cab)         (lab! l) (ConvertedAbstraction cab)]
    [(topbegin ,s ,t ...)           (TopLevelForm* t)]
    [(#%expression ,s ,e)           (Expr e)]
    [(topmodule ,s ,mn ,mp ,mf ...) (parameterize ([add!  mod!*]
                                                   [prov! (λ (x0 x1) (provide! mn x0 x1))])
                                      (ModuleLevelForm* mf))]
    [,g                             (GeneralTopLevelForm g)])
  (ModuleLevelForm : ModuleLevelForm (M) -> * ()
    [(#%provide ,rps ...)           (RawProvideSpec* rps)] ; todo : collect provided vars
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
    [(closure ,s ,in ,l ,ar ,ae ...) (AExpr* ae)])

  (CaseClosureAllocation : CaseClosureAllocation (CCA) -> * ()
    [(case-closure ,s ,in ,l [,ar ,ca] ...) (ClosureAllocation* ca)])

  (RawProvideSpec : RawProvideSpec (RPS) -> * ()
    [,ps (PhaselessSpec ps)])

  (PhaselessSpec : PhaselessSpec (PS) -> * ()
    [,sls (SpacelessSpec sls)])

  (SpacelessSpec : SpacelessSpec (SS) -> * ()
    [[,x0 ,x1]  ((prov!) x0 x1)])


  ; Fill in `top`, `mod` and `loc`
  (parameterize ([add! top!*])
    (TopLevelForm T))
  ; Convert to set
  (define sets (map (λ (xs) (apply make-id-set xs))
                    (list top mod loc)))
  
  (define (group-provides)
    (define grouped (group-by first (reverse provides)))
    (define (same-module-name? x y) (eq? (car x) (car y)))
    (define slices (slice-by same-module-name? grouped))
    (for/list ([slice slices])
       (define mod-name (first (first (car slice))))
       (cons mod-name (map rest (car slice)))))

  ; Output:
  ;   (list
  ;      (cons module-name0  (list from-name alpha-renamed-name) ...)
  ;      (cons module-name1  (list from-name alpha-renamed-name) ...)
  ;      ...)
  
  ; (displayln sets)
  (values (first  sets)
          (second sets)
          (third  sets)
          (group-provides)))

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
;;   Unsafe und: Lower 8 bits are        1 0100 1111. Upper bits are zero.
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
;;; generator in the paper generates "flat" code (assembler) whereas we
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

; a bytestring with the name `name` turns into "$bytes:name"
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

(define (console-bridge-symbol-constant-name sym)
  (string->symbol (~a "wr-top-level/" (symbol->string sym))))

(define current-label-map (make-parameter '()))
(define current-label-form-ht (make-parameter (make-hash)))
(define current-label-map-include-form? (make-parameter #t))
(define (reset-label-map!)
  (current-label-map '())
  (current-label-form-ht (make-hash))
  (current-label-map-include-form? #t))

(define (label-map-include-form? v)
  (current-label-map-include-form? (and v #t)))

(define (source-stx s)
  (if (and (syntax? s) (syntax-property s 'source-stx))
      (syntax-property s 'source-stx)
      s))

(define (record-label-map! l stx)
  (define id (unparse-variable l))
  (define s (source-stx stx))
  (define src (and (syntax? s) (syntax-source s)))
  (define line (and (syntax? s) (syntax-line s)))
  (define col (and (syntax? s) (syntax-column s)))
  (define span (and (syntax? s) (syntax-span s)))
  (define form-str
    (and (current-label-map-include-form?)
         (syntax? s)
         (format "~s" (syntax->datum s))))
  (define entry
    (let ([base `(label ,(format "~a" id)
                        (src ,(or src 'unknown)
                             ,(or line 0)
                             ,(or col 0)
                             ,(or span 0)))])
      (cond
        [(not form-str) base]
        [else
         (let* ([key (list src line col span form-str)]
                [seen (hash-ref (current-label-form-ht) key #f)])
           (if seen
               `(label ,(format "~a" id)
                       (src ,(or src 'unknown)
                            ,(or line 0)
                            ,(or col 0)
                            ,(or span 0))
                       (same-as ,seen))
               (begin
                 (hash-set! (current-label-form-ht) key (format "~a" id))
                 `(label ,(format "~a" id)
                         (src ,(or src 'unknown)
                              ,(or line 0)
                              ,(or col 0)
                              ,(or span 0))
                         (form ,form-str)))))])))
  (current-label-map (cons entry (current-label-map))))

(define (label-map->sexp)
  `(wasm-label-map ,@(reverse (current-label-map))))
  

(define-pass generate-code : LANF+closure (T) -> * ()
  (definitions
    (define gen-times '())
    (define (time-gen label thunk)
      (if (current-pass-timings?)
          (let-values ([(vals ms)
                        (with-timing
                          (λ ()
                            (call-with-values thunk list)))])
            (set! gen-times (cons (list label ms) gen-times))
            (apply values vals))
          (thunk)))
    ;; 1. Classify variables
    (define-values (top-vars module-vars local-vars provides)
      (time-gen "classify-variables"
        (λ ()
          (classify-variables T))))
    ;; (displayln "-- Provides --"           (current-error-port))
    ;; (displayln provides                   (current-error-port))
    ;; (displayln "-- Top-vars --"           (current-error-port))
    ;; (displayln (sort (map syntax->datum (map variable-id (id-set->list top-vars)))
    ;;                  symbol<?)
    ;;            (current-error-port))

    (define top-names
      (for/seteq ([x (in-list (id-set->list top-vars))])
        (syntax-e (variable-id x))))
    (define (top-variable? v)
      (set-member? top-names (syntax-e (variable-id v))))   ; top-level: match by symbol name
    (define (module-variable? v) (set-in? v module-vars)) ; free-identifier=?
    (define (local-variable? v)  (set-in? v local-vars))  ; free-identifier=?
    (define (ffi-variable? v)    (set-member? ffi-primitives-set
                                              (if (symbol? v) v (syntax-e (variable-id v)))))
    #;(displayln (list 'top? (top-variable? (variable #'sxml->dom))) (current-error-port))
    
    (define (global-variable? v)
      ; a global (wasm) varible is unboxed
      (non-literal-constant? (syntax-e (variable-id v))))
    (define (top-reference-symbol-expr v)
      (define sym (syntax-e (variable-id v)))
      (define name (console-bridge-symbol-constant-name sym))
      (add-symbol-constant name sym)
      `(global.get ,(string->symbol (~a "$symbol:" name))))
    (define (CheckedTopReference v)
      (define val (emit-fresh-local 'top-val '(ref eq)))
      `(block (result (ref eq))
        (local.set ,(LocalVar val)
                   (struct.get $Boxed $v
                               (ref.cast (ref $Boxed) (global.get ,(TopVar v)))))
        (if (ref.eq (local.get ,(LocalVar val)) (global.get $undefined))
            (then
             (drop (call $raise-unbound-variable-reference ,(top-reference-symbol-expr v)))))
        (local.get ,(LocalVar val))))
    (define (classify v)
      #;(displayln (list 'clas v))
      #;(when (symbol? v) (error 'classify "got: ~a" v))
      ;; (displayln (list 'top (top-variable?    v)))
      ;; (displayln (list 'mod (module-variable? v)))
      ;; (displayln (list 'loc (and (local-variable?  v) #t)))
      ;; (displayln (list 'ffi (ffi-variable?    v)))
      ;; (displayln (list 'glo (global-variable? v)))
      (cond
        [(top-variable?    v) 'top]
        [(module-variable? v) 'module]
        [(local-variable?  v) 'local]
        [(ffi-variable?    v) 'ffi]
        [(global-variable? v) 'global]
        [else
         ;; (displayln (list 'top (map unparse-variable (id-set->list top-vars))))
         ;; (displayln (list 'mod (map unparse-variable module-vars)))
         ;; (displayln (list 'loc (map unparse-variable local-vars)))

         ;; Note: In full Racket a reference to an unbound variable at the top-level
         ;;       will go through the current namespace.
         (when (variable? v)
           (define id (variable-id v))
           (raise-syntax-error (syntax-e id)
                               "unbound identifier"
                               id))
         (error 'classify "got: ~a" v)]))
    ;; 2. References to variable according to their type
    (define (Reference v)
      #;(displayln (list 'ref (list v (classify v))) (current-error-port))
      #;(when (symbol? v)
          (error 'Reference "got: ~a" v))
      ; reference to non-free variable
      ;   global refers to a Web Assembly global variable
      (case (classify v)
        ; [(top)        `(global.get ,(TopVar v))]   ; unboxed
        [(top)        (if (top-symbol-needs-defined-check? v)
                          (CheckedTopReference v)
                          `(struct.get $Boxed $v
                                       (ref.cast (ref $Boxed) (global.get ,(TopVar v)))))]
        [(global)     `(global.get ,($ (syntax-e (variable-id v))))]
        [(local)      `(local.get  ,(LocalVar v))]
        [(module)     `(module.get ,(ModuleVar v))]
        [(ffi)        'TODO]
        [else (error 'Reference "got: ~a" v)]))
    ;; 3. Variables assignments according to type.
    (define (Store! v e)
      (cond
        [(variable? v)
         (case (classify v)
           ; [(top)    `(global.set ,(TopVar    v) ,e)]
           [(top)    `(struct.set $Boxed $v (ref.cast (ref $Boxed) (global.get ,(TopVar v))) ,e)]
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
    (define f-tmp                    (Var (new-var 'f))) ; used by app
    (define used-primitives-ht       (make-hasheq))

    (define (record-used-primitive! pr)
      (hash-set! used-primitives-ht pr #t))

    (define primitive-func->symbol
      (for/hasheq ([pr (in-list primitives)])
        (values ($ pr) pr)))

    (define primitive-global->symbol
      (for/hasheq ([pr (in-list primitives)])
        (values ($ (prim: pr)) pr)))

    ;; Record primitive roots after inlining so the runtime shaker sees the
    ;; primitive references that survive code generation.
    (define (record-used-primitives-in-wat! wat)
      (define (walk x)
        (cond
          [(symbol? x)
           (cond
             [(hash-ref primitive-func->symbol x #f)
              => record-used-primitive!]
             [(hash-ref primitive-global->symbol x #f)
              => record-used-primitive!]
             [else (void)])]
          [(pair? x)
           (walk (car x))
           (walk (cdr x))]
          [else (void)]))
      (walk wat))

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
    (define quoted-strings-ht (make-hash))
    (define (add-quoted-string string)
      (cond
        [(hash-ref quoted-strings-ht string #f)
         => values]
        [else
         (define name (string->symbol (~a "quoted-string" quoted-string-counter)))
         (hash-set! quoted-strings-ht string name)
         (set! quoted-string-counter (+ quoted-string-counter 1))
         (add-string-constant name string)
         name]))
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
    (define (canonicalize-arity-markers markers)
      (define (dedup xs)
        (reverse
         (for/fold ([acc '()]) ([x (in-list xs)])
           (if (member x acc) acc (cons x acc)))))
      (define exacts
        (for/list ([m (in-list markers)]
                   #:when (>= m 0))
          m))
      (define at-least-starts
        (for/list ([m (in-list markers)]
                   #:when (< m 0))
          (- (- m) 1)))
      (define min-at-least
        (and (pair? at-least-starts)
             (apply min at-least-starts)))
      (define exacts-dedup (dedup exacts))
      (define exacts-usable
        (if min-at-least
            (filter (λ (n) (< n min-at-least)) exacts-dedup)
            exacts-dedup))
      (define exacts-sorted (sort exacts-usable <))
      (cond
        [min-at-least
         (define m (- (- min-at-least) 1))
         (if (null? exacts-sorted)
             (list m)
             (append exacts-sorted (list m)))]
        [else exacts-sorted]))

    (define (arity-markers->eq-expr markers)
      (match markers
        ['()       `(ref.cast (ref eq) (array.new_fixed $I32Array 0))]
        [(list m)  (Imm m)]
        [_
         `(ref.cast (ref eq)
                    (array.new_fixed $I32Array
                                     ,(length markers)
                                     ,@(for/list ([m (in-list markers)])
                                         `(i32.const ,m))))]))
    )

  (ClosureAllocation : ClosureAllocation (ca dd) -> * ()
    [(closure ,s ,in ,l ,ar ,ae1 ...)
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

     ;; Inferred name for the closure
     (define get-name
       (cond
         [in   (define name  (syntax-e (variable-id in)))
               (define $name (string->symbol (~a "$symbol:" name)))
               (add-symbol-constant name name) ; on purpose name twice
               `(global.get ,$name)]
         [else `(global.get $false)]))
     (define debug-id-expr
       (let ()
         (define debug-id (syntax-e (variable-id l)))
         (define $debug-id (string->symbol (~a "$symbol:" debug-id)))
         (add-symbol-constant debug-id debug-id)
         `(global.get ,$debug-id)))
     
     ; If there is no self-reference then allocation is simple.
     ; If there is a  self-reference we first need to allocate the closure,
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
                  ; #:do   [(displayln (list 'ae ae)) (displayln (list 'dd dd))]
                  #:when (and (variable? ae) (syntax? dd)
                              (eq? (Var ae) (syntax-e dd))))
         i))

     #;(displayln 'self-reference-indices)
     #;(displayln self-reference-indices)
     
     (maybe-store-in-dest
      (match self-reference-indices
        ; no self-references
        ['() `(struct.new $Closure
                (i32.const 0)                  ; hash
                ,get-name                      ; name:  #f or $Symbol
                ,(Imm ar)                      ; arity: fixnum
                (global.get $the-racket-realm) ; realm: #f or $Symbol
                (ref.func $invoke-closure)     ; invoke (used by apply, map, etc.)
                ,debug-id-expr                 ; debug-id
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
                               ,get-name                   ; name:  #f or $Symbol
                               ,(Imm ar)                   ; arity: todo
                               (global.get $false)         ; realm: #f or $Symbol
                               (ref.func $invoke-closure)  ; invoke (used by apply, map, etc.)
                               ,debug-id-expr              ; debug-id
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
    [(case-closure ,s ,in ,l [,ar ,ca] ...)
     (let* ([n   (length ca)]
            ;; arrays
            [as-init  `(array.new $Array    (global.get $null) (i32.const ,n))]
            [ars-init `(array.new $I32Array (i32.const 0)      (i32.const ,n))]
            [$as      (emit-fresh-local 'case-arms    '(ref $Array)    as-init)]
            [$ars     (emit-fresh-local 'case-arities '(ref $I32Array) ars-init)]
            [ar*      (canonicalize-arity-markers ar)]
            [arity-expr (arity-markers->eq-expr ar*)]
            ;; fill both arrays in lockstep
            [fills    (for/list ([m ar] [c ca] [i (in-naturals)])
                        (define arm (ClosureAllocation c #f)) ; => (ref $Closure)
                        `(block
                           (array.set $Array    ,(Reference $as)  (i32.const ,i) ,arm)
                           (array.set $I32Array ,(Reference $ars) (i32.const ,i) (i32.const ,m))))]
            ;; name (use #f unless you carry names)
            [name-expr (cond
                         ; `in` holds the inferred name
                         [in (define name  (syntax-e (variable-id in)))
                             (define $name (string->symbol (~a "$symbol:" name)))
                             (add-symbol-constant name name) ; on purpose name twice
                             `(global.get ,$name)]
                         [else
                          `(global.get $false)])]
            [debug-id-expr
             (let ()
               (define debug-id (syntax-e (variable-id l)))
               (define $debug-id (string->symbol (~a "$symbol:" debug-id)))
               (add-symbol-constant debug-id debug-id)
               `(global.get ,$debug-id))])
       `(block (result (ref $CaseClosure))
               ,@fills
               (struct.new $CaseClosure
                           (i32.const 0)                                 ;; $hash
                           ,name-expr                                    ;; $name
                           ,arity-expr                                    ;; $arity  = normalized set
                           (global.get $false)                           ;; $realm
                           (ref.func $invoke-case-closure)               ;; $invoke
                           ,debug-id-expr                               ;; $debug-id
                           (ref.func $code:case-lambda-dispatch)         ;; $code (dispatcher)
                           (global.get $empty-free)                      ;; $free (unused here)
                           ,(Reference $ars)                             ;; $arities (typed field)
                           ,(Reference $as))))                           ;; $arms    (typed field)
     ]                                                                 
    ) ; CaseClosureAllocation
  
  (AExpr3 : AExpr (ae [dd #f]) -> * ()  ; only ca needs dest
    [,x               (if (memq (syntax-e (variable-id x)) primitives)
                          (begin
                            (record-used-primitive! (syntax-e (variable-id x)))
                            (PrimRef x)) ; reference to primitive
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
                             [(flonum? v)     (define l (case v
                                                          [(+nan.0) 'nan] [(-nan.0) '-nan]
                                                          [(+inf.0) 'inf] [(-inf.0) '-inf]
                                                          [else v]))
                                              `(struct.new $Flonum (i32.const 0) (f64.const ,l))]
                             [(null? v)       '(global.get $null)]
                             [(void? v)       '(global.get $void)]
                             [(eq? v #t)      '(global.get $true)]  
                             [(eq? v #f)      '(global.get $false)]
                             [(eof-object? v) '(global.get $eof)]
                             [(fixnum? v)     (Imm v)]
                             [(char? v)       (Imm v)]
                             [(undefined? v)  (Imm v)]
                             [(unsafe-undefined? v) (Imm v)]
                             [(string? v)     (define name         (add-quoted-string v))
                                              (define $string:name (string->symbol (~a "$string:" name)))
                                              `(global.get ,$string:name)]
                             [(bytes? v)      (define name         (add-quoted-bytes v))
                                              (define $bytes:name  (string->symbol (~a "$bytes:" name)))
                                              `(global.get ,$bytes:name)]
                             [(symbol? v)     (define name         (add-quoted-symbol v))
                                              (define $symbol:name (string->symbol (~a "$symbol:" name)))
                                              `(global.get ,$symbol:name)]
                             [else            (displayln (list 'sigh: v) (current-error-port)) `',v]))])]
    [(top ,s ,x)
     ; Note: Until namespaces are implemented we represented top-level variables as using `$Boxed`.
     ;       Note that if x is present in a top-level define-values
     ;       then (top x) will become x anyway.
     ; Note: What is missing here: is error handling for an undefined top-level-variable.
     #;(displayln (list 'top (variable-id x)) (current-error-port))
     (Reference x)
     #;`(app ,#'namespace-variable-value (app ,#'string->symbol ',(symbol->string (syntax-e (variable-id x)))))
     ; ',#f ; use-mapping? TODO: this should be #t but that isn't implemented yet in runtime
     ]
    [(variable-reference ,s ,vrx)
     (define-values (constant? from-unsafe?)
       (nanopass-case (LANF+closure VariableReferenceId) vrx
         [(anonymous ,s0)        (values #f #f)]
         [(non-top ,s0 ,x0)      (values #t #f)]
         [(top ,s0 ,x0)          (values #f #f)]))
     `(struct.new $VariableReference
                  (i32.const 0)
                  ,(Imm constant?)
                  ,(Imm from-unsafe?))] ; hash, constant?, from-unsafe?
    [(quote-syntax ,s ,d)
     (raise-syntax-error 'generate-code "quote-syntax gone at this point")])

  (CExpr2 : CExpr (ce dd cd) -> * ()
    ;; All Complex Expressions are translated to statements
    [,ae                     (match dd
                               ;; Effect-position atomics are usually dropped, but
                               ;; risky top-level reads must still run so they can raise.
                               ['<effect>  (if (and (variable? ae)
                                                     (top-variable? ae)
                                                     (top-symbol-needs-defined-check? ae))
                                                `(drop ,(AExpr2 ae <value>))
                                                `(nop))]
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
     (define sym (syntax->datum (variable-id pr)))
     (define (primapp-loc s)
       (and (syntax? s)
            (let* ([src (syntax-source s)]
                   [src (cond [(path? src)   (path->string src)]
                              [(symbol? src) (symbol->string src)]
                              [else          src])]
                   [line (syntax-line s)]
                   [col  (syntax-column s)])
              (and src line col (format "~a:~a:~a" src line col)))))

     (define (primapp-arity-msg base s)
       (define loc (primapp-loc s))
       (if loc (format "~a at ~a" base loc) base))

     ;; Inlines a call to a primitive with
     ;;   a fixed number of arguments.
     (define (inline-prim/fixed sym ae1 arg-count)
       (define aes (AExpr* ae1))
       (define n   (length aes))
       (when (> n arg-count) (raise-syntax-error 'primapp (primapp-arity-msg "too many arguments" s) s))
       (when (< n arg-count) (raise-syntax-error 'primapp (primapp-arity-msg (format "too few arguments: ~a" sym) s) s))
       `(call ,($ sym) ,@aes))

     ;; Inlines a call to a primitive with
     ;;   at least `min` arguments, and
     ;;   at most  `max` arguments.
     ;; That is, arguments beyound `min` are optional.
     ;; Passes `(global.get $missing)` to indicate a missing argument.
     (define (inline-prim/optional sym ae1 min max)
       (define filler `(global.get $missing))
       (define aes (AExpr* ae1))
       (define n   (length aes))
       (when (> n max) (raise-syntax-error 'primapp "too many arguments" s))
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
       (when (> n max) (raise-syntax-error 'primapp "too many arguments" s))
       (when (< n min) (error 'primapp "too few arguments: ~a"  sym))
       (define optionals (make-list (- max n) default))
       `(call ,($ sym) ,@aes ,@optionals))

     ;; Inlines a call to a primitive that accepts optional arguments
     ;; bundled into a rest list after `rest-start` mandatory arguments.
     ;; This is used for $peek-bytes-avail! which takes at least 1
     ;; argument and at most 6. Instead of introducing a new shape
     ;; (would be $Prim16) we are using $Prim>=1 .
     (define (inline-prim/optional-rest sym ae1 min max [rest-start min])
       (define aes (AExpr* ae1))
       (define n   (length aes))
       (when (> n max) (raise-syntax-error 'primapp "too many arguments" s))
       (when (< n min) (error 'primapp "too few arguments: ~a"  sym))
       (define mandatory (take aes rest-start))
       (define rest       (build-rest-args (drop aes rest-start)))
       `(call ,($ sym) ,@mandatory ,rest))

     (define (inline-range sym ae1)
       (define aes (AExpr* ae1))
       (match aes
         [(list end)
          `(call ,($ sym) (global.get $missing) ,end (global.get $missing))]
         [(list start end)
          `(call ,($ sym) ,start ,end (global.get $missing))]
         [(list start end step)
          `(call ,($ sym) ,start ,end ,step)]
         [_ (error 'primapp "wrong number of arguments: ~a" sym)]))

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

     (define (build-rest-args/args aes)
      (define n (length aes))
      (if (zero? n)
          '(array.new $Args (global.get $null) (i32.const 0))
          `(array.new_fixed $Args ,n ,@aes)))

     ;; Inlines a call to a primitive with
     ;;   at least `min` arguments.
     ;; Arguments from `rest-start` are rest arguments.
     ;; They are passed in newly allocated $Args
     ;; Note: Having `rest-start` as well as `min` allows us to
     ;; share code for (f x ...) and (f x ...+).     
     (define (inline-prim/variadic-args sym ae1 min [rest-start min])
       (define n (length ae1))
       (when (< n min) (error 'primapp "too few arguments: ~a" sym))
       (define aes       (AExpr* ae1))
       (define mandatory (take aes rest-start))
       (define rest-args (build-rest-args/args (drop aes rest-start)))
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


     (define (inline-prim-default default)
       (case default
         [(false)        (Imm #f)]
         [(true)         (Imm #t)]
         [(missing)      '(global.get $missing)]
         [(string:space) '(global.get $string:space)]
         [(symbol:error) '(global.get $symbol:error)]
         [else
          (error 'inline-prim-default "unknown inline default: ~a" default)]))

     (define (inline-prim/spec spec sym ae1)
       (define kind       (primitive-inline-spec-kind spec))
       (define min        (primitive-inline-spec-min spec))
       (define max        (primitive-inline-spec-max spec))
       (define rest-start (primitive-inline-spec-rest-start spec))
       (define default    (primitive-inline-spec-default spec))
       (case kind
         [(fixed)            (inline-prim/fixed sym ae1 min)]
         [(optional)         (inline-prim/optional sym ae1 min max)]
         [(optional/default) (inline-prim/optional/default sym ae1 min max
                                                           (inline-prim-default default))]
         [(optional-rest)    (inline-prim/optional-rest sym ae1 min max rest-start)]
         [(variadic)         (inline-prim/variadic sym ae1 min rest-start)]
         [(variadic-args)    (inline-prim/variadic-args sym ae1 min rest-start)]
         [else
          (error 'inline-prim/spec "unknown primitive spec kind: ~a" kind)]))

     (define spec (primitive-inline-spec-ref sym))
     (define work
       (cond
         [(eq? sym 'void)
          (define (AE ae)   `(drop ,(AExpr3 ae <effect>)))
          (define (AE* aes) (map AE aes))
          `(block (result (ref eq))
                  ,@(AE* ae1)
                  (global.get $void))]
         [(eq? sym 'list)
          (build-rest-args (AExpr* ae1))]
         [(eq? sym 'values)
          (define n   (length ae1))
          (define aes (AExpr* ae1))
          (case n
            [(1)   (first aes)]
            [else  `(array.new_fixed $Values ,n ,@aes)])]
         [(memq sym '(range range-proc))
          (inline-range sym ae1)]
         [spec
          (inline-prim/spec spec sym ae1)]
         [else
          (case sym
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

        [(bytes=?)
         ; variadic, at least one argument
         (define n (length ae1))
         (when (< n 1) (error 'primapp "too few arguments: ~a" sym))
         (define aes    (AExpr* ae1))
         (define $cmp   ($ sym))
         (define $cmp/2 ($ (string->symbol (~a sym "/2"))))
         (case n
           [(1)  `(global.get $true)]
           [(2)  `(call ,$cmp/2 ,@aes)]
           [else `(call ,$cmp ,(first aes) ,(build-rest-args (rest aes)))])]

        [(bytes<? bytes>?)
         ; variadic, at least one argument
         (define n (length ae1))
         (when (< n 1) (error 'primapp "too few arguments: ~a" sym))
         (define aes    (AExpr* ae1))
         (define $cmp   ($ sym))
         (define $cmp/2 ($ (string->symbol (~a sym "/2"))))
         (case n
           [(1)  `(global.get $true)]
           [(2)  `(call ,$cmp/2 ,@aes)]
           [else `(call ,$cmp ,(first aes) ,(build-rest-args (rest aes)))])]
        

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
        
        [(vector-append)
         (let loop ([aes (AExpr* ae1)])
           (match aes
             [(list v)      `(call $vector-copy ,v (global.get $missing) (global.get $missing))]
             [(list v1 v2)  `(call $vector-append/2 ,v1 ,v2)]
             [(list* vs)    `(call $vector-append ,(build-rest-args aes))]) )]

        [(vector-member)
         (let* ([aes (AExpr* ae1)]
                [n   (length aes)])
           (cond
             [(= n 2) `(call $vector-member/2 ,(list-ref aes 0) ,(list-ref aes 1))]
             [(= n 3) `(call $vector-member ,(list-ref aes 0) ,(list-ref aes 1) ,(list-ref aes 2))]
             [(< n 2) (error 'primapp "too few arguments: ~a" 'vector-member)]
             [else    (error 'primapp "too many arguments: ~a" 'vector-member)]))]
        
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
                       `(call $bytes-set!/checked                         
                              ,$bs                                        
                              (i32.const ,i)                              
                              (i32.shr_s
                               (i31.get_s (ref.cast i31ref ,v)) 
                               (i32.const 1))))
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
                                      (ref.cast (ref i31) (call $char->integer ,v)))
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
          (define (primapp-arity-error sym aes)
            `(block (result (ref eq))
                    ,@(for/list ([ae aes]) `(drop ,ae))
                    (call $primitive-invoke:raise-arity-error
                          (ref.cast (ref $PrimitiveProcedure)
                                    (global.get ,($ (prim: sym))))
                          (i32.const ,(length aes)))))

          (match (length ae1)
            [0 (case sym
                 [(-)        '(global.get $zero)]
                 [(fx+ fx-)  '(global.get $zero)]
                 [(fl+ fl-)  '(global.get $flzero)]
                 [(fx*)      '(global.get $one)]
                 [(fl*)      '(global.get $flone)]
                 [else       (if (primitive-arity-accepts? sym 0)
                                 `(call ,(Prim pr))
                                 (primapp-arity-error sym '()))])]
            [1 (case sym
                 [(fx+ fx*)  (AExpr (first ae1))]
                 [(fl+ fl*)  (AExpr (first ae1))]
                 [(fx-)      `(call ,(Prim pr) (global.get $zero)   ,(AExpr (first ae1)))]
                 [(fl-)      `(call ,(Prim pr) (global.get $flzero) ,(AExpr (first ae1)))]
                 [(fx/)      `(call ,(Prim pr) (global.get $one)    ,(AExpr (first ae1)))]
                 [(fl/)      `(call ,(Prim pr) (global.get $flone)  ,(AExpr (first ae1)))]
                 [else       (let ([aes (AExpr* ae1)])
                               (if (primitive-arity-accepts? sym 1)
                                   `(call ,(Prim pr) ,@aes)
                                   (primapp-arity-error sym aes)))])]
            [2 (case sym
                 [(-)           `(call ,(Prim pr)
                                       ,(AExpr (first ae1)) ,(AExpr (second ae1)))]
                 [(fx+ fx- fx*) `(call ,(Prim pr)
                                       ,(AExpr (first ae1)) ,(AExpr (second ae1)))]
                 [(fl+ fl- fl*) `(call ,(Prim pr)
                                       ,(AExpr (first ae1)) ,(AExpr (second ae1)))]
                 ; / needs to signal an Racket error if denominator is zero
                 [else   (let ([aes (AExpr* ae1)])
                           (if (primitive-arity-accepts? sym 2)
                               `(call ,(Prim pr) ,@aes)
                               (primapp-arity-error sym aes)))])]
            [_ (case sym
                 [(fx+ fl+
                     fx* fl*
                     - fx- fl-) ; (+ a b c ...) = (+ (+ a b) c ...)
                  (define identity
                    (case sym
                      [(fx+ fx- -) '(global.get $zero)]
                      [(fl+ fl-)    '(global.get $flzero)]
                      [(fx*)        '(global.get $one)]
                      [(fl*)        '(global.get $flone)]
                      [else         '(global.get $zero)]))
                  (let loop ([aes (AExpr* ae1)])
                    (match aes
                      [(list)              identity]
                      [(list  ae0)         ae0]
                      [(list* ae0 ae1 aes) `(call ,(Prim pr)
                                                  (call ,(Prim pr) ,ae0 ,ae1)
                                                  ,(loop aes))]))]
                 [(fx/ fl/)
                  (define aes (AExpr* ae1))
                  (define first-two
                    `(call ,(Prim pr)
                           ,(car aes)
                           ,(cadr aes)))
                  (for/fold ([acc first-two])
                            ([arg (in-list (cddr aes))])
                    `(call ,(Prim pr) ,acc ,arg))]
                 [else
                  ;; Without arity checks, this is simply:
                  ;;     `(call ,(Prim pr) ,@(AExpr* ae1))
                  ;; But this gives a compile time error from the web assembly compiler,
                  ;; due to too many or too few arguments on the stack.
                  (let* ([aes (AExpr* ae1)]
                         [n   (length aes)])
                    (if (primitive-arity-accepts? sym n)
                        `(call ,(Prim pr) ,@aes)
                        (primapp-arity-error sym aes)))])])])]))
     (record-used-primitives-in-wat! work)

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
     ;; Hold the callee as a raw value for validation.
     (define proc-raw-var (new-var '$app-proc-raw))
     (define proc-raw (syntax-e (variable-id proc-raw-var)))
     (emit-local proc-raw '(ref eq) '(global.get $false))

     ;; The dynamic invoker for the procedure
     (define inv `(struct.get $Procedure $invoke (local.get ,proc)))

     ;; Tail-call the invoker with raw args
     (define work
       (if tc
           `(return_call_ref $ProcedureInvoker (local.get ,proc) ,args ,inv)
           `(call_ref        $ProcedureInvoker (local.get ,proc) ,args ,inv)))

     ;; Validate callee before casting to procedure.
     (define (set-proc w)
       `((local.set ,proc-raw ,w)
         (if (i32.eqz (ref.test (ref $Procedure) (local.get ,proc-raw)))
             (then (call $raise-application-not-procedure (local.get ,proc-raw))
                   (unreachable)))
         (local.set ,proc (ref.cast (ref $Procedure) (local.get ,proc-raw)))))

     (match dd
       ['<effect>
       (match cd
          ['<expr> `(block ,@(set-proc (AExpr ae)) ,work)]
          ['<stat> `(block ,@(set-proc (AExpr ae)) (drop ,work))]
          [_ (error 'internal-app0 "combination impossible")])]
      ['<value>
        (match cd
          ['<return> `(block (result (ref eq))
                             ,@(set-proc (AExpr ae))
                             ,(if tc work `(return ,work)))]
          ['<expr>   `(block (result (ref eq))
                             ,@(set-proc (AExpr ae))
                             ,work)]
          [_ (error 'internal-app1 "not impossible?!")])]
       [x `(block ,@(set-proc (AExpr ae))
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
         [(closure ,s ,in ,l ,ar ,ae1 ...)
         (define tc (eq? cd '<return>))
         (define tc-flag (if tc (Imm #t) (Imm #f)))

          ; Package arguments in an $Args array.
          (define args
            `(array.new_fixed $Args ,(length ae2)
                              ,@(for/list ([ae ae2])
                                  (AExpr3 ae <value>))))

          ;; For variadic closures, argument repacking is handled by
          ;; `$invoke-closure` via `$repack-arguments`. A direct label call
          ;; bypasses repacking and breaks `(lambda x ...)` / rest bindings.
          (define proc-closedapp
            `(ref.cast (ref $Procedure) (global.get $closedapp-clos)))

          (define invoke-via-procedure
            (if tc
                `(return_call_ref $ProcedureInvoker
                                  ,proc-closedapp
                                  ,args
                                  (struct.get $Procedure $invoke ,proc-closedapp))
                (cond
                  [(eq? dd <effect>)
                   `(drop (call_ref $ProcedureInvoker
                                    ,proc-closedapp
                                    ,args
                                    (struct.get $Procedure $invoke ,proc-closedapp)))]
                  [else
                   `(call_ref $ProcedureInvoker
                              ,proc-closedapp
                              ,args
                              (struct.get $Procedure $invoke ,proc-closedapp))])))

          (define invoke-direct
            (if tc
                `(return_call ,(Label l) (global.get $closedapp-clos) ,args)
                (cond
                  [(eq? dd <value>)        `(call ,(Label l) (global.get $closedapp-clos) ,args)]
                  [(eq? dd <effect>) `(drop (call ,(Label l) (global.get $closedapp-clos) ,args))]
                  ; otherwise, the data destination is a variable
                  [else                    `(call ,(Label l) (global.get $closedapp-clos) ,args)])))

          (define invoke
            (if (and (integer? ar) (< ar 0))
                invoke-via-procedure
                invoke-direct))

          ;; TODO:
          ;;  - There is a potential for optimization here.
          ;;    If there is no free variables, then we
          ;;    can jump directly to the label, without allocating
          ;;    a closure. [See the commented out definition below.]
         
          (define work            
            `(block ,@(cond
                        [(eq? dd <value>)  (list `(result (ref eq)))]
                        [(eq? dd <effect>) (list)]
                        ; otherwise, the data destination is a variable
                        [else              (list `(result (ref eq)))])  ; <--
              ; 1. Allocate closure and store it in $closedapp-clos
              ,(ClosureAllocation ca #'$closedapp-clos)
              ; 2. Call it.
              ,invoke))

          #;(define work
            (match ae1
              [(list) ; no free variable => no need to actually allocate a closure, just jump to label
               (if tc
                   `(return_call ,(Label l) (global.get $undefined) ,tc-flag ,@ae2)   ; undefined due to no closure
                   `(call        ,(Label l) (global.get $undefined) ,tc-flag ,@ae2))]

              ; TODO: Only the no-free-variables case has been updated
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
       (error 'wcm "todo")
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
             ; A single target accepts either one ordinary value or a $Values bundle
             ; of length 1. Anything else must raise a value-count error.
             [(list x)         `(block
                                  ,(CExpr ce x <stat>)
                                  ,(Store! x `(call $expect-one-value ,(Reference x))))]
             ; multiple values are returned in an $Values array [v0,v1,...].
             ; Reuse x0 for the raw result, then unpack the validated $Values.
             [(list x0 x1 ...)  (define mv (emit-fresh-local 'mv  '(ref null $Values)))
                                (define n  (length (cons x0 x1)))
                                 `(block ,(CExpr ce x0 <stat>)
                                         ,(Store! mv `(call $expect-n-values ,(Reference x0) (i32.const ,n)))
                                         ,@(for/list ([x (cons x0 x1)]
                                                      [i (in-naturals)])
                                             (Store! x `(array.get $Values ,(Reference mv) (i32.const ,i)))))]
             ; no values are expected
             ['()              (define t (emit-fresh-local 'letv0))
                                `(block
                                  ,(CExpr ce t <stat>)
                                  (drop (call $expect-zero-values ,(Reference t))))])))
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
     
     (define (closure-debug-id-expr l)
       (define debug-id (syntax-e (variable-id l)))
       (define $debug-id (string->symbol (~a "$symbol:" debug-id)))
       (add-symbol-constant debug-id debug-id)
       `(global.get ,$debug-id))
     (define (closure-name-expr in)
       (cond
         [in (define name  (syntax-e (variable-id in)))
             (define $name (string->symbol (~a "$symbol:" name)))
             (add-symbol-constant name name) ; on purpose name twice
             `(global.get ,$name)]
         [else
          `(global.get $false)]))
     (define (AllocateClosure ca dest) ; called by letrec-values
       ; Allocates a closure in which the $free array contains zeros only.
       (nanopass-case (LANF+closure AExpr) ca
         [(closure ,s ,in ,l ,ar ,ae1 ...)
          (let ([us (make-list (length ae1) (Undefined))])
            (Store! dest `(struct.new $Closure
                               (i32.const 0)               ; hash
                               ,(closure-name-expr in)     ; name:  #f or $Symbol
                               ,(Imm ar)                   ; arity:
                               (global.get $false)         ; realm: #f or $Symbol
                               (ref.func $invoke-closure)  ; invoke (used by apply, map, etc.)
                               ,(closure-debug-id-expr l)  ; debug-id
                               (ref.func ,(Label l))
                               (array.new_fixed $Free ,(length ae1) ,@us))))]
         [(case-closure ,s ,in ,l [,ar ,ca*] ...)
          (define n (length ca*))
          (define ar* (canonicalize-arity-markers ar))
          (define arity-expr (arity-markers->eq-expr ar*))
          (Store! dest
                  `(struct.new $CaseClosure
                               (i32.const 0)
                               ,(closure-name-expr in)
                               ,arity-expr
                               (global.get $false)
                               (ref.func $invoke-case-closure)
                               ,(closure-debug-id-expr l)
                               (ref.func $code:case-lambda-dispatch)
                               (global.get $empty-free)
                               (array.new $I32Array (i32.const 0) (i32.const ,n))
                               (array.new $Array (global.get $null) (i32.const ,n))))]
         [else (error 'AllocateClosure "internal error, got: ~a" ca)]))

     (define (FillClosure ca dd)
       ; Fill in the $free array
       (nanopass-case (LANF+closure AExpr) ca
         [(closure ,s ,in ,l ,ar ,ae1 ...)
          ; This will be sliced into a `block` so we don't need another one.
          (for/list ([ae (AExpr* ae1)]
                     [i  (in-naturals)])
            `(array.set $Free
                        (struct.get $Closure $free (ref.cast (ref $Closure) ,(Reference dd)))
                        (i32.const ,i)
                        ,ae))]
         [(case-closure ,s ,in ,l [,ar ,ca*] ...)
         (append*
          (for/list ([m  ar]
                     [arm-ca ca*]
                     [i  (in-naturals)])
             (define arm (emit-fresh-local 'case-arm '(ref $Closure)))
             (append
              (list (AllocateClosure arm-ca arm))
              (FillClosure arm-ca arm)
              (list `(array.set $I32Array
                                (struct.get $CaseClosure $arities
                                            (ref.cast (ref $CaseClosure) ,(Reference dd)))
                                (i32.const ,i)
                                (i32.const ,m))
                    `(array.set $Array
                                (struct.get $CaseClosure $arms
                                            (ref.cast (ref $CaseClosure) ,(Reference dd)))
                                (i32.const ,i)
                                ,(Reference arm))))))]))

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
             [(list v t init) #;(when (eq? init #f) (error 'here "here!"))
                              `(local.set ,(if (symbol? v) v (Var v)) ,init)]))
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
                                                   (then (call $raise-arity-mismatch/proc
                                                               (local.get $clos)
                                                               (array.len (local.get $args)))
                                                         (unreachable)))]
                                             [else
                                              ; at least n expected
                                              (define n (- (- ar) 1))
                                              `(if (i32.lt_u (array.len (local.get $args))
                                                             (i32.const ,n))
                                                   (then (call $raise-arity-mismatch/proc
                                                               (local.get $clos)
                                                               (array.len (local.get $args)))
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
    ; [(#%require     ,s ,rrs ...)  `(block) ] ; #;`'"ignored #%require"
    [(#%require     ,s ,rrs ...)  `(global.set $result (global.get $void)) ] ; #;`'"ignored #%require"    
    [(define-values ,s (,x ...)   ,e)
     ; Note: Initializing the top-level variables x ... with ($Boxed undefined)
     ;       here is too late. If the variables appear as free variables
     ;       they can be referenced during closure creation for earlier
     ;       `define-label` declarations.

     ; The variables x ... are initialized to (Boxed undefined) in $entry.
     ; That is, at this point the variables x ... are bound to a $Boxed instance.
     
     (match x
       ['()           (define vals (emit-fresh-local 'vals '(ref eq) '(global.get $undefined)))
                      `(block
                        ,(Expr e vals <stat>)
                        (drop (call $expect-zero-values ,(Reference vals))))]
       ; Until we implement namespaces, top-level variables are
       ; stored as boxed global variables.
       ;              (Expr e dd cd)
       ; A single target accepts either one ordinary value or a $Values bundle
       ; of length 1. Anything else must raise a value-count error.
       [(list x0)     (define vals (emit-fresh-local 'vals '(ref eq) '(global.get $undefined)))
                      `(block
                        ,(Expr e vals <stat>)
                        ,(Store! x0 `(call $expect-one-value ,(Reference vals))))]
       #;[(list x0)     `(block
                          (global.set ,($$ (variable-id x0)) 
                                    (ref.cast (ref eq) 
                                              (struct.new $Boxed
                                                          (global.get $undefined))))
                        ,(Expr e x0 <stat>))]
       [(list x ...)  (define vals (emit-fresh-local 'vals '(ref eq) '(global.get $undefined)))
                      (define mv   (emit-fresh-local 'mv '(ref $Values)))
                      `(block
                        #;,@(for/list ([x0 x])
                              `(global.set ,($$ (variable-id x0)) 
                                           (ref.cast (ref eq) 
                                                     (struct.new $Boxed
                                                                 (global.get $false)))))
                        ,(Expr e vals <stat>)
                        (local.set ,(LocalVar mv) (call $expect-n-values ,(Reference vals) (i32.const ,(length x))))
                        ,@(for/list ([x0 x] [i (in-naturals)])
                            (Store! x0 `(array.get $Values
                                                   ,(Reference mv)
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
    [(topmodule ,s ,mn ,mp ,mf ...) (let ([mf (map (λ (m) (ModuleLevelForm m dd)) mf)])
                                      `(topmodule ,mn ,mp ,mf ...))]
    [(topbegin ,s ,t ...)
     ; Note: We have flattened `topbegin` so no `t` is a `topbegin`.
     ; The data destination of `topbegin` is `$result`.
     ; The first forms are evaluated for effect.
     ; The last one stores the result in `$result`.
     (let ()
       (define (TopLevelForm* ts dd)
         (map (λ (t) (TopLevelForm t dd)) ts))
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
           (let ([t0s (filter non-nop?
                              (TopLevelForm* t0 <effect>))]
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
    [(#%provide ,rps ...)  
     (void)]                   ; todo : generate provide vars here
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
    (for/list ([x (in-list (id-set->list top-vars))])
      `(global ,(Var x) (mut (ref eq)) ,(Undefined))))
  
  (define result #'$result) ; holds result in $entry

  (define-values (dls tms entry-body)
    (time-gen "top-level-forms"
      (λ ()
        (TopLevelForm T result))))
  (define top-level-variable-declarations
    (time-gen "top-level-globals"
      (λ ()
        (generate-global-declarations-for-top-level-variables))))

  (define console-bridge-bindings
    (if (not (current-console-bridge?))
        '()
        (let* ([top-binding-names
                (list->seteq (current-console-bridge-top-binding-names))]
               [mutable-top-binding-names
                (list->seteq (current-console-bridge-mutable-top-binding-names))]
               [top-vars-by-name
                (for/hasheq ([x (in-list (id-set->list top-vars))]
                             #:do [(define sym (syntax-e (variable-id x)))]
                             #:when (symbol? sym))
                  (values sym x))]
               [console-top-binding-names
                (for/list ([sym (in-list (set->list top-binding-names))]
                           #:when (hash-ref top-vars-by-name sym #f))
                  sym)]
               [exposed-names
                (sort (remove-duplicates console-top-binding-names) symbol<?)])
          (add-string-constant 'wr-console-bridge-kind "top-level")
          (for/list ([sym (in-list exposed-names)])
            (define const-name (console-bridge-symbol-constant-name sym))
            (add-symbol-constant const-name sym)
            (cond
              [(hash-ref top-vars-by-name sym #f)
               => (λ (x)
                    (define source-path
                      (console-bridge-source->string
                       (and (variable? x)
                            (syntax-source (variable-id x)))))
                    (define origin-kind
                      (console-bridge-binding-origin-kind source-path))
                    (define origin-kind-const-name
                      (console-bridge-origin-kind-constant-name sym))
                    (define source-path-const-name
                      (console-bridge-source-path-constant-name sym))
                    (add-string-constant origin-kind-const-name origin-kind)
                    (when source-path
                      (add-string-constant source-path-const-name source-path))
                    (list 'top sym const-name x
                          (set-member? mutable-top-binding-names sym)
                          origin-kind-const-name
                          (and source-path source-path-const-name)))]
              [else
               (error 'console-bridge-bindings
                      "internal error: console bridge symbol without runtime binding: ~s"
                      sym)])))))

  ; variables bound by let-values and others at the top-level
  ; are represented in the runtime as local variables of a function `entry`.
  (define entry-locals (*locals*))

  (define primitives-also-declared-as-variables-at-top-level
    (time-gen "top-level-prim-vars"
      (λ ()
        (sort (set->list
               (set-intersect (list->set (map syntax-e (map variable-id (id-set->list top-vars))))
                              (list->set primitives)))
              symbol<?))))
  
  ;; (displayln "-- variables declared at top-level --")
  ;; (displayln (sort (map syntax-e (map variable-id (id-set->list top-vars))) symbol<?))
  ;; (displayln "-- primitives also declared as variables at top-level --")
  ;; (displayln primitives-also-declared-as-variables-at-top-level)
  
  (define-values (gen runtime-gen-rows)
    (time-gen "generate-runtime"
      (λ ()
        (parameterize ([current-runtime-primitive-report-path
                        (or (current-runtime-primitive-report-path)
                            (and (current-pass-dump-dir)
                                 (build-path (current-pass-dump-dir)
                                             "runtime-primitives.sexp")))]
                       [current-runtime-timing-rows
                        (and (current-pass-timings?) '())])
          (define mod
            (generate-runtime
             dls                              ; define-labels
             tms                              ; top modules
             entry-body                       ; expressions and general top-level forms
             result                           ; variable that holds the result (in $entry)
             ; program specific
             (id-set->list top-vars)          ; top level variables (list of variables)
             console-bridge-bindings          ; top-level bindings exposed through globalThis.WR
             top-level-variable-declarations  ; wasm code for declaring top-level variables
             entry-locals                     ; variables that are local to $entry
             ; general
             primitives                       ; primitives (list of symbols)
             (sort (hash-keys used-primitives-ht) symbol<?)
             string-constants                 ; (list (list name string) ...)
             bytes-constants                  ; (list (list name bytes) ...)
             symbol-constants                 ; (list (list name symbol) ...)
             ))
          (values mod (current-runtime-timing-rows))))))
  (when (current-pass-timings?)
    (when runtime-gen-rows
      (set! gen-times (append runtime-gen-rows gen-times))))
  (when (current-pass-timings?)
    (current-gen-timing-table
     (format-timing-table (reverse gen-times))))
  gen
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
    (display (rewrite-wat-identifiers
              (with-output-to-string
                (λ ()
                  (pretty-write x)))))))

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

(define debug:print-passes? #f)

;; Get command line options
(define current-pass-timings?     (make-parameter #f))
(define current-pass-timing-table (make-parameter #f))
(define current-gen-timing-table  (make-parameter #f))
(define current-pass-dump-dir     (make-parameter #f))
(define current-pass-dump-limit   (make-parameter #f))
(define current-console-bridge-top-binding-names (make-parameter '()))
(define current-console-bridge-mutable-top-binding-names (make-parameter '()))
(define current-console-bridge-program-source-path (make-parameter #f))

(define (console-bridge-source->string src)
  (cond
    [(path? src)
     (path->string
      (simplify-path (path->complete-path src)))]
    [(string? src)
     (path->string
      (simplify-path (path->complete-path (string->path src))))]
    [(symbol? src) (symbol->string src)]
    [else          #f]))

(define (console-bridge-binding-origin-kind source-path)
  (cond
    [(not source-path) "generated"]
    [(regexp-match? #rx"/stdlib/" source-path) "stdlib"]
    [(regexp-match? #rx"/libs/" source-path) "library"]
    [else "program"]))

(define (console-bridge-origin-kind-constant-name sym)
  (string->symbol (~a "wr-origin-kind/" sym)))

(define (console-bridge-source-path-constant-name sym)
  (string->symbol (~a "wr-source-path/" sym)))

(define (comp+ stx)
  (run (comp stx)))

(define (comp stx)
  (reset-counter!)
  (reset-label-map!)
  (current-pass-timing-table #f)
  (current-gen-timing-table #f)
  (current-console-bridge-top-binding-names '())
  (current-console-bridge-mutable-top-binding-names '())
  (current-console-bridge-program-source-path
   (and (syntax? stx)
        (console-bridge-source->string (syntax-source stx))))
  (define dump-dir (current-pass-dump-dir))
  (define dump-limit (current-pass-dump-limit))
  (when dump-dir
    (make-directory* dump-dir))
  (define pass-times '())
  (define pass-counter 0)

  (define (sanitize-pass-label s)
    (list->string
     (for/list ([ch (in-string s)])
       (if (or (char-alphabetic? ch)
               (char-numeric? ch))
           ch
           #\-))))

  (define (dump-pass! label dump-fn v)
    (when (and dump-dir
               (or (not dump-limit) (< pass-counter dump-limit)))
      (define base-label (sanitize-pass-label label))
      (define dump-file  (build-path dump-dir
                           (format "~a-~a.sexp" pass-counter base-label)))
      (with-output-to-file dump-file
        (lambda ()
          (pretty-write (dump-fn v)))
        #:exists 'replace))
    (set! pass-counter (+ pass-counter 1)))

  ; The time-pass function also handles dumping if ncessary
  (define (time-pass label thunk [size-fn #f] [dump-fn #f])
    (define (finish-pass v ms)
      (when (current-pass-timings?)
        (define size (and size-fn (size-fn v)))
        (set! pass-times (cons (list label ms size) pass-times)))
      (when dump-fn
        (dump-pass! label dump-fn v))
      v)
    (if (current-pass-timings?)
        (let-values ([(v ms) (with-timing thunk)])
          (finish-pass v ms))
        (finish-pass (thunk) #f)))

  (define t
    (time-pass "topexpand"
      (λ ()
        (define t (topexpand stx))
        (when debug:print-passes?
          (displayln "--- topexpand ---")
          (displayln (pretty-print (syntax->datum t)) (current-error-port)))
        t)
      #f
      (λ (v) (if (syntax? v) (syntax->datum v) v))))

  (define u   (time-pass "unexpand"             (λ () (unexpand t))
                         #f
                         (λ (v) (if (syntax? v) (syntax->datum v) v))))
  (define p0  (time-pass "parse"                (λ () (parse u))
                         (λ (v) (count-unparsed unparse-LFE v))
                         (λ (v) (unparse-all (unparse-LFE v)))))
  (define p   (time-pass "print-top-level-results"
                         (λ () (if (current-print-top-level-results?)
                                   (print-top-level-results p0)
                                   p0))
                         (λ (v) (count-unparsed unparse-LFE v))
                         (λ (v) (unparse-all (unparse-LFE v)))))
  (define ft  (time-pass "flatten-topbegin"     (λ () (flatten-topbegin p))
                         (λ (v) (count-unparsed unparse-LFE v))
                         (λ (v) (unparse-all (unparse-LFE v)))))
  (define in  (time-pass "infer-names"          (λ () (infer-names ft))
                         (λ (v) (count-unparsed unparse-LFE v))
                         (λ (v) (unparse-all (unparse-LFE v)))))
  (define cq  (time-pass "convert-quotations"   (λ () (convert-quotations in))
                         (λ (v) (count-unparsed unparse-LFE v))
                         (λ (v) (unparse-all (unparse-LFE v)))))
  (define eb  (time-pass "explicit-begin"       (λ () (explicit-begin cq))
                         (λ (v) (count-unparsed unparse-LFE1 v))
                         (λ (v) (unparse-all (unparse-LFE1 v)))))
  (define ecl (time-pass "explicit-case-lambda" (λ () (explicit-case-lambda eb))
                         (λ (v) (count-unparsed unparse-LFE2 v))
                         (λ (v) (unparse-all (unparse-LFE2 v)))))
  (define ar  (time-pass "α-rename"             (λ () (α-rename ecl))
                         (λ (v) (count-unparsed unparse-LFE2+ v))
                         (λ (v) (unparse-all (unparse-LFE2+ v)))))
  (define ua  (time-pass "uncover-assigned!"    (λ () (uncover-assigned! ar))))
  (define lr  (time-pass "lower-letrec-values"  (λ () (parameterize ([current-assigned-analysis ua])
                                                            (lower-letrec-values ar)))
                         (λ (v) (count-unparsed unparse-LFE2+ v))
                         (λ (v) (unparse-all (unparse-LFE2+ v)))))

  (define ac
    (time-pass "assignment-conversion"
      (λ ()
        (define a
          (parameterize ([current-assigned-analysis ua])
            (assignment-conversion lr)))
        (when debug:print-passes?
          (displayln "--- assignment conversion ---")
          (displayln (pretty-print a) (current-error-port)))
        a)
      (λ (v) (count-unparsed unparse-LFE2+ v))
      (λ (v) (unparse-all (unparse-LFE2+ v)))))

  (define ca  (time-pass "categorize-applications" (λ () (categorize-applications ac))
                         (λ (v) (count-unparsed unparse-LFE3 v))
                         (λ (v) (unparse-all (unparse-LFE3 v)))))
  (define an  (time-pass "anormalize"            (λ () (anormalize ca))
                         (λ (v) (count-unparsed unparse-LANF v))
                         (λ (v) (unparse-all (unparse-LANF v)))))
  (define cc  (time-pass "closure-conversion"    (λ () (closure-conversion an))
                         (λ (v) (count-unparsed unparse-LANF+closure v))
                         (λ (v) (unparse-all (unparse-LANF+closure v)))))
  (define fb  (time-pass "flatten-begin"         (λ () (flatten-begin cc))
                         (λ (v) (count-unparsed unparse-LANF+closure v))
                         (λ (v) (unparse-all (unparse-LANF+closure v)))))
  (define gen (time-pass "generate-code"         (λ () (generate-code fb))
                         (λ (_) (count-unparsed unparse-LANF+closure fb))
                         (λ (v) (strip v))))

  (define result (strip gen))
  (when debug:print-passes?
    (displayln "--- compiled ---")
    (displayln (pretty-print result) (current-error-port)))
  (when (current-pass-timings?)
    (current-pass-timing-table
     (format-timing-table/size (reverse pass-times))))
  result)


(define (comp- stx)
  (reset-counter!)
  (define ar
    (α-rename
     (explicit-case-lambda
      (explicit-begin
       (convert-quotations
        (infer-names
         (flatten-topbegin
          (parse
           (unexpand
            (topexpand stx))))))))))
  (define ua (uncover-assigned! ar))
  (define lr
    (parameterize ([current-assigned-analysis ua])
      (lower-letrec-values ar)))
  (define ac
    (parameterize ([current-assigned-analysis ua])
      (assignment-conversion lr)))
  (pretty-print
   (strip    
    (flatten-begin
     (closure-conversion
      (anormalize
       (categorize-applications
        ac)))))))

(module+ test
  (define (module-has-top-level-form? mod head name)
    (for/or ([form (in-list (cdr mod))])
      (and (pair? form)
           (eq? (car form) head)
           (pair? (cdr form))
           (equal? (cadr form) name))))

  (define (module-func-exports mod name)
    (for/or ([form (in-list (cdr mod))])
      (and (pair? form)
           (eq? (car form) 'func)
           (pair? (cdr form))
           (equal? (cadr form) name)
           (for/list ([part (in-list (cddr form))]
                      #:when (and (pair? part)
                                  (eq? (car part) 'export)
                                  (pair? (cdr part))
                                  (string? (cadr part))))
             (cadr part)))))

  (define (report-ref report key)
    (cdr (assq key report)))

  (define (compile/primitive-report stx)
    (define dump-dir (make-temporary-file "webracket-tree-shaker~a" 'directory))
    (define mod
      (parameterize ([current-pass-dump-dir dump-dir])
        (comp stx)))
    (define report
      (with-input-from-file (build-path dump-dir "runtime-primitives.sexp")
        read))
    (delete-directory/files dump-dir)
    (values mod report))

  (define (compile/primitive-report/ffi stx ffi-path)
    (define fs
      (foreigns-deduplicate
       'compile/primitive-report/ffi
       (ffi-file->foreigns ffi-path)))
    (define ims   (map foreign->import fs))
    (define prims (map foreign->primitive fs))
    (define dump-dir (make-temporary-file "webracket-tree-shaker-ffi~a" 'directory))
    (define mod
      (parameterize ([current-pass-dump-dir dump-dir]
                     [current-ffi-foreigns fs]
                     [current-ffi-imports-wat ims]
                     [current-ffi-funcs-wat prims])
        (comp stx)))
    (define report
      (with-input-from-file (build-path dump-dir "runtime-primitives.sexp")
        read))
    (delete-directory/files dump-dir)
    (parameterize ([current-ffi-foreigns '()]
                   [current-ffi-imports-wat '()]
                   [current-ffi-funcs-wat '()])
      (reset-ffi-primitives))
    (values mod report))

  (let-values ([(mod report)
                (compile/primitive-report #'(module tree-shake-acos webracket
                                                   (acos 0.0)))])
    (check-true  (module-has-top-level-form? mod 'func   '$acos))
    (check-false (module-has-top-level-form? mod 'func   '$asin))
    (check-true  (module-has-top-level-form? mod 'global '$prim:acos))
    (check-false (module-has-top-level-form? mod 'global '$prim:asin))
    (check-true  (pair? (memq 'acos   (report-ref report 'isolated-primitives))))
    (check-false (pair? (memq 'append (report-ref report 'isolated-primitives)))))

  (let-values ([(mod report)
                (compile/primitive-report #'(module tree-shake-append webracket
                                                   (append '(1) '(2 3) '(4))))])
    (check-true (module-has-top-level-form? mod 'func '$append))
    (check-true (module-has-top-level-form? mod 'func '$reverse))
    (check-true (pair? (memq 'append  (report-ref report 'retained-primitives))))
    (check-true (pair? (memq 'reverse (report-ref report 'retained-primitives)))))

  (let-values ([(mod report)
                (compile/primitive-report #'(module tree-shake-callback-exports webracket
                                                   "hello world"))])
    (check-not-false (member "callback" (module-func-exports mod '$callback)))
    (check-not-false (member "callback-register" (module-func-exports mod '$callback-register)))
    (check-not-false (member "callback-accepts-argc" (module-func-exports mod '$callback-accepts-argc)))
    (check-true (pair? (assq 'summary report))))

  (let ([mod (parameterize ([current-console-bridge? #t])
               (comp #'(module console-bridge-exports webracket
                         (define x 41)
                         (define y 99)
                         (define (f n) (+ n 1))
                         (set! x 42)
                         x)))])
    (check-not-false (member "wr-ref"    (module-func-exports mod '$wr-ref)))
    (check-not-false (member "wr-call"   (module-func-exports mod '$wr-call)))
    (check-not-false (member "wr-names"  (module-func-exports mod '$wr-names)))
    (check-not-false (member "wr-names-detailed" (module-func-exports mod '$wr-names-detailed)))
    (check-not-false (member "wr-format" (module-func-exports mod '$wr-format))))

  (check-equal? (run-expr #'(append '(1) '(2 3) '(4)))
                '(1 2 3 4))

  (check-true
   (regexp-match?
    #rx"#<procedure:f>"
    (with-output-to-string
      (λ ()
        (run (comp #'(letrec-values ([(f) (begin 0 (lambda () 1))])
                       f)))))))

  (let-values ([(mod report)
                (compile/primitive-report/ffi
                 #'(module tree-shake-ffi-console-unused webracket
                     "hello")
                 (build-path (current-directory) "ffi" "console.ffi"))])
    (check-false (module-has-top-level-form? mod 'func '$js-console-info))
    (check-false (module-has-top-level-form? mod 'func '$js-console-log))
    (check-false (module-has-top-level-form? mod 'global '$prim:js-console-log))
    (check-false (module-has-top-level-form? mod 'global '$prim:js-console-info))
    (check-true (pair? (assq 'summary report))))
  )
  
(define (comp-- stx)
  (reset-counter!)
  (define p   (parse (unexpand (topexpand stx))))
  (define ft  (flatten-topbegin p))
  (define in  (infer-names ft))
  (define cq  (convert-quotations in))
  (define eb  (explicit-begin cq))
  (define ecl (explicit-case-lambda eb))
  (define ar  (α-rename ecl))
  (define ua  (uncover-assigned! ar))
  (define lr  (parameterize ([current-assigned-analysis ua])
                (lower-letrec-values ar)))
  (define ac  (parameterize ([current-assigned-analysis ua])
                (assignment-conversion lr)))
  (define ca  (categorize-applications ac))
  (define an  (anormalize ca))
  (pretty-print (strip an)))

(define (comp--- stx)
  (reset-counter!)
  (define p   (parse (unexpand (topexpand stx))))
  (define ft  (flatten-topbegin p))
  (define in  (infer-names ft))
  (define cq  (convert-quotations in))
  (define eb  (explicit-begin cq))
  (define ecl (explicit-case-lambda eb))
  (define ar  (α-rename ecl))
  (define ua  (uncover-assigned! ar))
  (define lr  (parameterize ([current-assigned-analysis ua])
                (lower-letrec-values ar)))
  (define ac  (parameterize ([current-assigned-analysis ua])
                (assignment-conversion lr)))
  (define ca  (categorize-applications ac))
  (pretty-print (strip ca)))

(define (test stx)
  (reset-counter!)
  (define p   (parse (unexpand (topexpand stx))))
  (define ft  (flatten-topbegin p))
  (define in  (infer-names ft))
  (define cq  (convert-quotations in))
  (define eb  (explicit-begin cq))
  (define ecl (explicit-case-lambda eb))
  (define ar  (α-rename ecl))
  (define ua  (uncover-assigned! ar))
  (define lr  (parameterize ([current-assigned-analysis ua])
                (lower-letrec-values ar)))
  (define ac  (parameterize ([current-assigned-analysis ua])
                (assignment-conversion lr)))
  (define ca  (categorize-applications ac))
  (define an  (anormalize ca))
  (define cc  (closure-conversion an))
  (define fb  (flatten-begin cc))
  (pretty-print
   (values ; strip
    (values ; classify-variables
     fb))))

(define (test- stx)
  (reset-counter!)
  (define p   (parse (unexpand (topexpand stx))))
  (define ft  (flatten-topbegin p))
  (define in  (infer-names ft))
  (define cq  (convert-quotations in))
  (define eb  (explicit-begin cq))
  (define ecl (explicit-case-lambda eb))
  (define ar  (α-rename ecl))
  (define ua  (uncover-assigned! ar))
  (define lr  (parameterize ([current-assigned-analysis ua])
                (lower-letrec-values ar)))
  (define ac  (parameterize ([current-assigned-analysis ua])
                (assignment-conversion lr)))
  (pretty-print
   (values ; strip
    (values ; classify-variables
     (values ; flatten-begin
      (values ; closure-conversion
       (values ; anormalize
        (values ;categorize-applications
         ac))))))))



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
          (equal? (run '(char<? #\a #\b)) #t)
          (equal? (run '(char<? #\b #\a)) #f)
          (equal? (run '(char<? #\a #\a)) #f)
          (equal? (run '(char>? #\a #\b)) #f)
          (equal? (run '(char>? #\b #\a)) #t)
          (equal? (run '(char>? #\a #\a)) #f)
          (equal? (run '(char<=? #\a #\b)) #t)
          (equal? (run '(char<=? #\b #\a)) #f)
          (equal? (run '(char<=? #\a #\a)) #t)
          (equal? (run '(char>=? #\a #\b)) #f)
          (equal? (run '(char>=? #\b #\a)) #t)
          (equal? (run '(char>=? #\a #\a)) #t)
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
  (define (test-procedure-arity)
    (and (equal? (run '(procedure-arity-includes? (lambda (x [y 0]) x) 1)) #t)
         (equal? (run '(procedure-arity-includes? (lambda (x) x) 1 #f)) #t)
         (equal? (run '(procedure-arity-includes? (lambda (x) x) 2)) #f)))
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
             (list 11 (vector 22 #\x) 'foo "bar" (list 55)))))

  (list "-- Core Constructs --"
        ;; (list "Immediate Values"           (test-immediates))
        ;; (list "Call unary primitive"       (test-call-unary-primitive))
        ;; (list "Some characters "           (test-some-characters)) ; slow
        ;; #;(list "All characters"           (test-all-characters))  ; very slow
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
        (list "Procedure arity"               (test-procedure-arity))
        (list "Multiple Values"               (test-multiple-values))
        ;; Tests below require the expander to be present.
        "-- Derived Constructs --"
        #; (list "Letrec"                        (test-letrec))  ;; TODO!
        ; assignment to letrec bound variable
        #; (letrec ((f (lambda (g) (set! f g) (f)))) (f (lambda () 12))) 
        (list "Named let"                     (test-named-let))  ; <-
        (list "And/Or"                        (test-and/or))
        (list "Cond"                          (test-cond))
        (list "When/unless"                   (test-when/unless))
        (list "Begin0"                        (test-begin0))
        (list "Fasl"                          (test-fasl))
        ))
