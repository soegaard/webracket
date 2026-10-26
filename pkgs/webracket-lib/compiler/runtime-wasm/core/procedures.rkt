#lang racket

(provide procedure-core-runtime-helpers
         procedure-runtime-string-constants
         procedure-runtime-symbol-constants
         procedure-global-forms
         procedure-global-names
         procedure-basic-runtime-only-helper-names
         procedure-arity-at-least-runtime-only-helper-names
         procedure-arity-field-runtime-only-helper-names
         procedure-composition-runtime-only-helper-names
         procedure-reduction-runtime-only-helper-names
         procedure-arity-runtime-only-helper-names
         procedure-result-arity-runtime-only-helper-names
         procedure-arity-mask-runtime-only-helper-names
         primitive-reflection-runtime-only-helper-names
         procedure-application-error-runtime-only-helper-names
         procedure-invocation-error-runtime-only-helper-names
         procedure-invoker-runtime-only-helper-names
         procedure-case-lambda-runtime-only-helper-names
         primitive-dispatch-runtime-only-helper-names
         primitive-invocation-error-runtime-only-helper-names
         arity->procedure-field-expr
         arity->shape
         primitive-shapes
         primitive-arity->shape
         shape->invoker
         procedure-primitive-global-forms
         procedure-ffi-primitive-global-forms
         procedure-primitive-initializer-forms
         procedure-ffi-primitive-initializer-forms
         procedure-primitive-invoker-forms
         procedure-invocation-error-core-runtime-helper-forms
         procedure-invoker-core-runtime-helper-forms
         procedure-case-lambda-core-runtime-helper-forms
         procedure-primitive-error-core-runtime-helper-forms
         procedure-primitive-dispatch-core-runtime-helper-forms
         procedure-primitive-invoker-core-runtime-helper-forms
         procedure-rest-argument-core-runtime-helper-forms
         procedure-bootstrap-runtime-helper-forms
         procedure-post-primitive-invoker-runtime-helper-forms
         procedure-main-runtime-helper-forms
         procedure-arity-mask-runtime-helper-forms
         primitive-reflection-runtime-helper-forms
         procedure-runtime-only-helpers)

(require "../core-helper.rkt"
         "../../private/immediates.rkt"
         "../../private/wasm-utils.rkt")

;; procedure-runtime-string-constants : (listof (list/c symbol? string?))
;;   Declare string constants used by procedure helpers and procedure errors.
(define procedure-runtime-string-constants
  '((procedure-realm     "procedure-realm")
    (procedure-or-false? "(or/c procedure? #f)")
    (procedure-arity?    "procedure-arity?")
    (arity-at-least?     "arity-at-least?")
    (procedure?          "procedure?")
    (primitive?          "primitive?")
    (arity-error:start
     ": arity mismatch;\n the expected number of arguments does not match the given number\n  expected: ")
    (arity-error:start/no-expected
     ": arity mismatch;\n the expected number of arguments does not match the given number\n  given: ")
    (arity-error:given   "\n  given: ")
    (arity-error:at-least "at least ")
    (arity-error:or      " or ")
    (result-arity-error:start
     ": result arity mismatch;\n expected number of values not received\n  expected: ")
    (result-arity-error:received "\n  received: ")
    (contract-violation:prefix
     ": contract violation\n  expected: ")
    (primitive-invoke:no-code
     "primitive procedure has no code pointer")
    (primitive-invoke:code-type-mismatch
     "primitive procedure code does not match its arity shape")
    (primitive-invoke:unhandled-shape
     "primitive procedure has unsupported arity shape")
    (arity-error:generic
     "arity mismatch;\n the expected number of arguments does not match the given number")
    (application:not-procedure
     "application: not a procedure;\n expected a procedure that can be applied to arguments\n  given: ")
    (question            "?")))

;; procedure-runtime-symbol-constants : (listof symbol?)
;;   Declare symbols used by procedure helpers and procedure errors.
(define procedure-runtime-symbol-constants
  '(procedure-realm
    name
    debug-id
    arity-at-least
    make-arity-at-least
    arity-at-least?
    arity-at-least-value
    application-not-procedure-frontier
    binding-result-arity-frontier
    procedure-arity-frontier
    procedure-arity
    procedure-arity-mask
    procedure-arity-includes?
    procedure-extract-target
    procedure-result-arity
    procedure-specialize
    procedure-reduce-arity
    procedure-reduce-arity-mask
    primitive?
    primitive-result-arity
    expected-count
    received-count
    got-struct-type
    got-procedure?
    caller-name
    caller-debug-id))

;; procedure-global-forms : (listof datum?)
;;   Declare cached procedure-related struct type descriptors.
(define procedure-global-forms
  '(
         ;; Cached arity-at-least struct type descriptor
         (global $arity-at-least-type (mut (ref null $StructType)) (ref.null $StructType))))

;; procedure-global-names : (listof symbol?)
;;   Enumerate cached procedure-related global names.
(define procedure-global-names
  (for/list ([form (in-list procedure-global-forms)])
    (match form
      [`(global ,name . ,_) name]
      [_ (error 'procedure-global-names
                "expected global form, got: ~s"
                form)])))

;; arity-range? : any/c exact-nonnegative-integer? exact-nonnegative-integer? -> boolean?
;;   Check whether a list arity contains exactly the closed integer range.
(define (arity-range? a min-arity max-arity)
  (and (list? a)
       (for/and ([n (in-list a)])
         (and (integer? n)
              (<= min-arity n)
              (<= n max-arity)))
       (let ([uniq (remove-duplicates a)])
         (and (= (length uniq) (+ 1 (- max-arity min-arity)))
              (for/and ([n (in-range min-arity (add1 max-arity))])
                (memq n uniq))))))

;; arity-2-3? : any/c -> boolean?
;;   Check whether a list arity accepts exactly two or three arguments.
(define (arity-2-3? a)
  (and (list? a)
       (let ([uniq (remove-duplicates a)])
         (and (= (length uniq) 2)
              (memq 2 uniq)
              (memq 3 uniq)))))

;; arity-3-4? : any/c -> boolean?
;;   Check whether a list arity accepts exactly three or four arguments.
(define (arity-3-4? a)
  (and (list? a)
       (let ([uniq (remove-duplicates a)])
         (and (= (length uniq) 2)
              (memq 3 uniq)
              (memq 4 uniq)))))

;; arity->internal-representation : any/c -> (or/c exact-integer? #f)
;;   Encode simple arities for the legacy primitive result-arity field.
(define (arity->internal-representation a)
  (match a
    [(arity-at-least n) (- (- n) 1)]
    [(? number? a)      a]
    [(? arity-2-3?)     2]
    [(? arity-3-4?)     3]
    [#f                 #f]
    [_                  #f]
    [_                  (error
                         'arity->internal-representation "got: ~a" a)]))

;; arity-entry->marker : any/c -> (or/c exact-integer? #f)
;;   Convert a single arity entry to the runtime arity marker encoding.
(define (arity-entry->marker a)
  (match a
    [(? exact-nonnegative-integer? n) n]
    [(arity-at-least n)               (- (- n) 1)]
    [_                                #f]))

;; canonicalize-arity-markers : (listof exact-integer?) -> (listof exact-integer?)
;;   Normalize exact and at-least arity markers for runtime procedure records.
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
        (filter (lambda (n) (< n min-at-least)) exacts-dedup)
        exacts-dedup))
  (define exacts-sorted (sort exacts-usable <))
  (cond
    [min-at-least
     (define m (- (- min-at-least) 1))
     (if (null? exacts-sorted)
         (list m)
         (append exacts-sorted (list m)))]
    [else exacts-sorted]))

;; arity->markers : any/c -> (or/c (listof exact-integer?) #f)
;;   Encode a Racket procedure arity into runtime arity markers.
(define (arity->markers a)
  (match a
    [(? exact-nonnegative-integer? n)
     (list n)]
    [(arity-at-least n)
     (list (- (- n) 1))]
    [(? list? as)
     (define ms (for/list ([x (in-list as)]) (arity-entry->marker x)))
     (and (andmap integer? ms)
          (canonicalize-arity-markers ms))]
    [#f #f]
    [_ #f]))

;; arity->procedure-field-expr : any/c -> datum?
;;   Build the runtime $Procedure arity field expression for a reflected arity.
(define (arity->procedure-field-expr a)
  (define ms (arity->markers a))
  (cond
    [(not ms)         (Imm #f)]
    [(null? ms)       `(ref.cast (ref eq) (array.new_fixed $I32Array 0))]
    [(null? (cdr ms)) (Imm (car ms))]
    [else
     `(ref.cast (ref eq)
                (array.new_fixed $I32Array
                                 ,(length ms)
                                 ,@(for/list ([m (in-list ms)])
                                     `(i32.const ,m))))]))

;; See $primitive-invoke for an explanation of shapes.

;; arity->shape : any/c -> (or/c exact-nonnegative-integer? #f)
;;   Select the primitive invoker shape for a reflected procedure arity.
(define (arity->shape a)
  (match a
    [(arity-at-least n) (if (>= n 4) 30 (+ 6 n))]
    [(? number? n)      (if (= n 6) 26 (min n 5))]
    [(? (lambda (a) (arity-range? a 0 1)))  16]
    [(? (lambda (a) (arity-range? a 0 2)))  17]
    [(? (lambda (a) (arity-range? a 0 3)))  27]
    [(? (lambda (a) (arity-range? a 0 4)))  28]
    [(? (lambda (a) (arity-range? a 0 5)))  29]
    [(? (lambda (a) (arity-range? a 1 2)))  18]
    [(? (lambda (a) (arity-range? a 1 3)))  19]
    [(? (lambda (a) (arity-range? a 1 4)))  20]
    [(? (lambda (a) (arity-range? a 1 5)))  21]
    [(? (lambda (a) (arity-range? a 2 3)))  14]
    [(? (lambda (a) (arity-range? a 2 4)))  22]
    [(? (lambda (a) (arity-range? a 2 5)))  23]
    [(? (lambda (a) (arity-range? a 3 4)))  15]
    [(? (lambda (a) (arity-range? a 3 5)))  24]
    [(? (lambda (a) (arity-range? a 6 7)))  25]
    [_ #f]))

; These functions are variadic functions that can handle the
; rest arguments both as an $Args array and as a list.
; The $Args convention is used by inlining.
; The list  convention by `apply`, `map` and others.
(define primitive-variadic-args
  '(bytes
    date
    date*
    make-date
    make-date*
    string
    vector
    treelist
    treelist-append
    module-call-with-prompt
    chaperone-procedure
    chaperone-struct
    chaperone-vector
    chaperone-box
    chaperone-hash
    unsafe-chaperone-procedure
    unsafe-impersonate-procedure
    raise-argument-error
    raise-argument-error*
    raise-arguments-error
    vector-immutable
    values
    void))

;; primitive-finite-rest-list-wrappers : (listof (list/c symbol? exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer?))
;;   Primitives whose reflected arity is a finite optional range, but whose
;;   runtime entry point uses mandatory arguments plus one rest-list argument.
(define primitive-finite-rest-list-wrappers
  '((bytes-convert 2 7 32)
    (peek-bytes-avail! 2 6 33)
    (peek-bytes-avail!* 2 6 33)))

;; primitive-finite-rest-list-shape : symbol? any/c -> (or/c exact-nonnegative-integer? #f)
;;   Return the specialized finite-arity rest-list invoker shape, if any.
(define (primitive-finite-rest-list-shape pr arity)
  (for/first ([entry (in-list primitive-finite-rest-list-wrappers)]
              #:when (and (eq? pr (first entry))
                          (arity-range? arity (second entry) (third entry))))
    (fourth entry)))

;; primitive-shapes : (listof exact-nonnegative-integer?)
;;   Enumerate primitive invoker shapes emitted by the runtime.
(define primitive-shapes
  '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33))

;; primitive-arity->shape : symbol? any/c -> (or/c exact-nonnegative-integer? #f)
;;   Select the primitive invoker shape for a primitive name and arity.
(define (primitive-arity->shape pr arity)
  (or (primitive-finite-rest-list-shape pr arity)
      (let ([base (arity->shape arity)])
        (and base
             (cond
               [(= base 30)
                (if (memq pr primitive-variadic-args) 31 base)]
               [(>= base 6)
                (if (memq pr primitive-variadic-args) (+ base 4) base)]
               [else base])))))

;; shape->invoker : exact-nonnegative-integer? -> symbol?
;;   Return the primitive invoker function name for shape.
(define (shape->invoker shape)
  (match shape
    [0  'primitive-invoke:shape-0]
    [1  'primitive-invoke:shape-1]
    [2  'primitive-invoke:shape-2]
    [3  'primitive-invoke:shape-3]
    [4  'primitive-invoke:shape-4]
    [5  'primitive-invoke:shape-5]
    [6  'primitive-invoke:shape-6]
    [7  'primitive-invoke:shape-7]
    [8  'primitive-invoke:shape-8]
    [9  'primitive-invoke:shape-9]
    [10 'primitive-invoke:shape-10]
    [11 'primitive-invoke:shape-11]
    [12 'primitive-invoke:shape-12]
    [13 'primitive-invoke:shape-13]
    [14 'primitive-invoke:shape-14]
    [15 'primitive-invoke:shape-15]
    [16 'primitive-invoke:shape-16]
    [17 'primitive-invoke:shape-17]
    [18 'primitive-invoke:shape-18]
    [19 'primitive-invoke:shape-19]
    [20 'primitive-invoke:shape-20]
    [21 'primitive-invoke:shape-21]
    [22 'primitive-invoke:shape-22]
    [23 'primitive-invoke:shape-23]
    [24 'primitive-invoke:shape-24]
    [25 'primitive-invoke:shape-25]
    [26 'primitive-invoke:shape-26]
    [27 'primitive-invoke:shape-27]
    [28 'primitive-invoke:shape-28]
    [29 'primitive-invoke:shape-29]
    [30 'primitive-invoke:shape-30]
    [31 'primitive-invoke:shape-31]
    [32 'primitive-invoke:shape-32]
    [33 'primitive-invoke:shape-33]
    [_  'primitive-invoke]))

;; procedure-primitive-global-forms : (listof symbol?) -> (listof datum?)
;;   Declare globals that hold primitive procedure values.
(define (procedure-primitive-global-forms primitive-names)
  (for/list ([pr (in-list primitive-names)])
    `(global ,($ (prim: pr)) (mut (ref eq)) ,(Imm (undefined)))))

;; procedure-ffi-primitive-global-forms : (listof symbol?) -> (listof datum?)
;;   Declare globals that hold FFI primitive procedure values.
(define (procedure-ffi-primitive-global-forms primitive-names)
  (for/list ([pr (in-list primitive-names)])
    `(global ,($ (prim: pr)) (mut (ref eq)) ,(Imm (undefined)))))

;; procedure-primitive-initializer-forms : (listof (list/c symbol? any/c any/c)) -> (listof datum?)
;;   Initialize primitive globals as $PrimitiveProcedure values.
(define (procedure-primitive-initializer-forms primitive-arities)
  (for/list ([entry (in-list primitive-arities)])
    (define pr           (first entry))
    (define ar           (second entry))
    (define result-arity (third entry))
    (define shape        (primitive-arity->shape pr ar))
    (define $name ($ (string->symbol (~a "symbol:" pr))))
    `(global.set ,($ (prim: pr))
                 (struct.new $PrimitiveProcedure
                  ; for $Procedure
                  (i32.const 0)                               ; hash
                  (global.get ,$name)                         ; name  (used by object-name)
                  ,(arity->procedure-field-expr ar)           ; arity
                  (global.get $the-racket/primitive-realm)    ; realm
                  #;(ref.func $primitive-invoke)
                  (ref.func ,($ (shape->invoker shape)))
                  (ref.func ,($ pr))
                  ,(Imm #f
                        #;(arity->internal-representation result-arity))))))

;; procedure-ffi-primitive-initializer-forms : (listof (list/c symbol? exact-nonnegative-integer?)) -> (listof datum?)
;;   Initialize FFI primitive globals as $PrimitiveProcedure values.
(define (procedure-ffi-primitive-initializer-forms primitive-arities)
  (for/list ([entry (in-list primitive-arities)])
    (define pr    (first entry))
    (define ar    (second entry))
    ;; FFI wrappers with more than six arguments use the normal variadic
    ;; rest-list function type. The procedure object still records the
    ;; exact source arity, and the generated wrapper checks it after
    ;; receiving the rest list.
    (define shape (if (> ar 6) 6 (arity->shape ar)))
    (define $name ($ (string->symbol (~a "symbol:" pr))))
    `(global.set ,($ (prim: pr))
                 (struct.new $PrimitiveProcedure
                  ; for $Procedure
                  (i32.const 0)                               ; hash
                  (global.get ,$name)                         ; name  (used by object-name)
                  ,(arity->procedure-field-expr ar)           ; arity
                  (global.get $the-racket/primitive-realm)    ; realm
                  (ref.func ,($ (shape->invoker shape)))
                  (ref.func ,($ pr))
                  ,(Imm #f)))))

;; primitive-invoker-tail : exact-nonnegative-integer? -> (listof datum?)
;;   Generate the shape-specific primitive invocation tail.
(define (primitive-invoker-tail shape)
  (match shape
    [0 ; exact 0
     `((if (i32.eqz (local.get $argc))
           (then (if (ref.test (ref $Prim0) (local.get $code))
                     (then (return_call_ref $Prim0
                                            (ref.cast (ref $Prim0) (local.get $code))))
                     (else (return (call $raise-code-type-mismatch (local.get $pproc))))))
           (else (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (unreachable))]
    [1 ; exact 1
     `((if (i32.eq (local.get $argc) (i32.const 1))
           (then (if (ref.test (ref $Prim1) (local.get $code))
                     (then (return_call_ref $Prim1
                                            (local.get $a0)
                                            (ref.cast (ref $Prim1) (local.get $code))))
                     (else (return (call $raise-code-type-mismatch (local.get $pproc))))))
           (else (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (unreachable))]
    [2 ; exact 2
     `((if (i32.eq (local.get $argc) (i32.const 2))
           (then (if (ref.test (ref $Prim2) (local.get $code))
                     (then (return_call_ref $Prim2
                                            (local.get $a0)
                                            (local.get $a1)
                                            (ref.cast (ref $Prim2) (local.get $code))))
                     (else (return (call $raise-code-type-mismatch (local.get $pproc))))))
           (else (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (unreachable))]
    [3 ; exact 3
     `((if (i32.eq (local.get $argc) (i32.const 3))
           (then (if (ref.test (ref $Prim3) (local.get $code))
                     (then (return_call_ref $Prim3
                                            (local.get $a0)
                                            (local.get $a1)
                                            (local.get $a2)
                                            (ref.cast (ref $Prim3) (local.get $code))))
                     (else (return (call $raise-code-type-mismatch (local.get $pproc))))))
           (else (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (unreachable))]
    [4 ; exact 4
     `((if (i32.eq (local.get $argc) (i32.const 4))
           (then (if (ref.test (ref $Prim4) (local.get $code))
                     (then (return_call_ref $Prim4
                                            (local.get $a0)
                                            (local.get $a1)
                                            (local.get $a2)
                                            (array.get $Args (local.get $args) (i32.const 3))
                                            (ref.cast (ref $Prim4) (local.get $code))))
                     (else (return (call $raise-code-type-mismatch (local.get $pproc))))))
           (else (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (unreachable))]
    [5 ; exact 5
     `((if (i32.eq (local.get $argc) (i32.const 5))
           (then (if (ref.test (ref $Prim5) (local.get $code))
                     (then (return_call_ref $Prim5
                                            (local.get $a0)
                                            (local.get $a1)
                                            (local.get $a2)
                                            (array.get $Args (local.get $args) (i32.const 3))
                                            (array.get $Args (local.get $args) (i32.const 4))
                                            (ref.cast (ref $Prim5) (local.get $code))))
                     (else (return (call $raise-code-type-mismatch (local.get $pproc))))))
           (else (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (unreachable))]
    [6 ; at least 0, rest arguments as list
     `((if (ref.test (ref $Prim>=0) (local.get $code))
           (then (local.set $rest
                            (call $rest-arguments->list
                                  (local.get $args)
                                  (i32.const 0)))
                 (return_call_ref $Prim>=0
                                  (local.get $rest)
                                  (ref.cast (ref $Prim>=0) (local.get $code))))
           (else (return (call $raise-code-type-mismatch (local.get $pproc)))))
       (unreachable))]
    [7 ; at least 1, rest arguments as list
     `((if (i32.ge_u (local.get $argc) (i32.const 1))
           (then (if (ref.test (ref $Prim>=1) (local.get $code))
                     (then (local.set $rest
                                      (call $rest-arguments->list
                                            (local.get $args)
                                            (i32.const 1)))
                           (return_call_ref $Prim>=1
                                            (local.get $a0)
                                            (local.get $rest)
                                            (ref.cast (ref $Prim>=1) (local.get $code))))
                     (else (return (call $raise-code-type-mismatch (local.get $pproc))))))
           (else (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (unreachable))]
    [8 ; at least 2, rest arguments as list
     `((if (i32.ge_u (local.get $argc) (i32.const 2))
           (then (if (ref.test (ref $Prim>=2) (local.get $code))
                     (then (local.set $rest
                                      (call $rest-arguments->list
                                            (local.get $args)
                                            (i32.const 2)))
                           (return_call_ref $Prim>=2
                                            (local.get $a0)
                                            (local.get $a1)
                                            (local.get $rest)
                                            (ref.cast (ref $Prim>=2) (local.get $code))))
                     (else (return (call $raise-code-type-mismatch (local.get $pproc))))))
           (else (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (unreachable))]
    [9 ; at least 3, rest arguments as list
     `((if (i32.ge_u (local.get $argc) (i32.const 3))
           (then
            (if (ref.test (ref $Prim>=3) (local.get $code))
                (then (local.set $rest
                                 (call $rest-arguments->list
                                       (local.get $args)
                                       (i32.const 3)))
                      (return_call_ref $Prim>=3
                                       (local.get $a0)
                                       (local.get $a1)
                                       (local.get $a2)
                                       (local.get $rest)
                                       (ref.cast (ref $Prim>=3) (local.get $code))))
                (else (return (call $raise-code-type-mismatch (local.get $pproc))))))
           (else (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (unreachable))]
    [10 ; at least 0, rest arguments as $Args array
     `((if (ref.test (ref $Prim>=0) (local.get $code))
           (then (return_call_ref $Prim>=0
                                  (local.get $args)
                                  (ref.cast (ref $Prim>=0) (local.get $code))))
           (else (return (call $raise-code-type-mismatch (local.get $pproc)))))
       (unreachable))]
    [11 ; at least 1, rest arguments as $Args array
     `((if (i32.ge_u (local.get $argc) (i32.const 1))
           (then (if (ref.test (ref $Prim>=1) (local.get $code))
                     (then (return_call_ref $Prim>=1
                                            (local.get $a0)
                                            (call $rest-arguments->args
                                                  (local.get $args)
                                                  (i32.const 1))
                                            (ref.cast (ref $Prim>=1) (local.get $code))))
                     (else (return (call $raise-code-type-mismatch (local.get $pproc))))))
           (else (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (unreachable))]
    [12 ; at least 2, rest arguments as $Args array
     `((if (i32.ge_u (local.get $argc) (i32.const 2))
           (then (if (ref.test (ref $Prim>=2) (local.get $code))
                     (then (return_call_ref $Prim>=2
                                            (local.get $a0)
                                            (local.get $a1)
                                            (call $rest-arguments->args
                                                  (local.get $args)
                                                  (i32.const 2))
                                            (ref.cast (ref $Prim>=2) (local.get $code))))
                     (else (return (call $raise-code-type-mismatch (local.get $pproc))))))
           (else (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (unreachable))]
    [13 ; at least 3, rest arguments as $Args array
     `((if (i32.ge_u (local.get $argc) (i32.const 3))
           (then
            (if (ref.test (ref $Prim>=3) (local.get $code))
                (then (return_call_ref $Prim>=3
                                       (local.get $a0)
                                       (local.get $a1)
                                       (local.get $a2)
                                       (call $rest-arguments->args
                                             (local.get $args)
                                             (i32.const 3))
                                       (ref.cast (ref $Prim>=3) (local.get $code))))
                (else (return (call $raise-code-type-mismatch (local.get $pproc))))))
           (else (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (unreachable))]
    [14 ; between 2 and 3 arguments
     `((if (i32.lt_u (local.get $argc) (i32.const 2))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (i32.gt_u (local.get $argc) (i32.const 3))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (ref.test (ref $Prim23) (local.get $code))
           (then (if (i32.eq (local.get $argc) (i32.const 2))
                     (then (local.set $a2 (global.get $missing))))
                 (return_call_ref $Prim23
                                  (local.get $a0)
                                  (local.get $a1)
                                  (local.get $a2)
                                  (ref.cast (ref $Prim23) (local.get $code))))
           (else (return (call $raise-code-type-mismatch (local.get $pproc)))))
       (unreachable))]
    [15 ; between 3 and 4 arguments
     `((if (i32.lt_u (local.get $argc) (i32.const 3))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (i32.gt_u (local.get $argc) (i32.const 4))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (ref.test (ref $Prim34) (local.get $code))
           (then (if (i32.eq (local.get $argc) (i32.const 3))
                     (then (local.set $a3 (global.get $missing)))
                     (else (local.set $a3
                                      (array.get $Args (local.get $args) (i32.const 3)))))
                 (return_call_ref $Prim34
                                  (local.get $a0)
                                  (local.get $a1)
                                  (local.get $a2)
                                  (local.get $a3)
                                  (ref.cast (ref $Prim34) (local.get $code))))
           (else (return (call $raise-code-type-mismatch (local.get $pproc)))))
       (unreachable))]
    [16 ; between 0 and 1 arguments
     `((if (i32.gt_u (local.get $argc) (i32.const 1))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (ref.test (ref $Prim01) (local.get $code))
           (then (if (i32.eqz (local.get $argc))
                     (then (local.set $a0 (global.get $missing))))
                 (return_call_ref $Prim01
                                  (local.get $a0)
                                  (ref.cast (ref $Prim01) (local.get $code))))
           (else (return (call $raise-code-type-mismatch (local.get $pproc)))))
       (unreachable))]
    [17 ; between 0 and 2 arguments
     `((if (i32.gt_u (local.get $argc) (i32.const 2))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (ref.test (ref $Prim02) (local.get $code))
           (then (if (i32.eqz (local.get $argc))
                     (then (local.set $a0 (global.get $missing))
                           (local.set $a1 (global.get $missing)))
                     (else (if (i32.eq (local.get $argc) (i32.const 1))
                               (then (local.set $a1 (global.get $missing))))))
                 (return_call_ref $Prim02
                                  (local.get $a0)
                                  (local.get $a1)
                                  (ref.cast (ref $Prim02) (local.get $code))))
           (else (return (call $raise-code-type-mismatch (local.get $pproc)))))
       (unreachable))]
    [27 ; between 0 and 3 arguments
     `((if (i32.gt_u (local.get $argc) (i32.const 3))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (ref.test (ref $Prim03) (local.get $code))
           (then (if (i32.eqz (local.get $argc))
                     (then (local.set $a0 (global.get $missing))
                           (local.set $a1 (global.get $missing))
                           (local.set $a2 (global.get $missing)))
                     (else (if (i32.eq (local.get $argc) (i32.const 1))
                               (then (local.set $a1 (global.get $missing))
                                     (local.set $a2 (global.get $missing)))
                               (else (if (i32.eq (local.get $argc) (i32.const 2))
                                         (then (local.set $a2 (global.get $missing))))))))
                 (return_call_ref $Prim03
                                  (local.get $a0)
                                  (local.get $a1)
                                  (local.get $a2)
                                  (ref.cast (ref $Prim03) (local.get $code))))
           (else (return (call $raise-code-type-mismatch (local.get $pproc)))))
       (unreachable))]
    [28 ; between 0 and 4 arguments
     `((if (i32.gt_u (local.get $argc) (i32.const 4))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (ref.test (ref $Prim4) (local.get $code))
           (then (if (i32.eqz (local.get $argc))
                     (then (local.set $a0 (global.get $missing))
                           (local.set $a1 (global.get $missing))
                           (local.set $a2 (global.get $missing))
                           (local.set $a3 (global.get $missing)))
                     (else (if (i32.eq (local.get $argc) (i32.const 1))
                               (then (local.set $a1 (global.get $missing))
                                     (local.set $a2 (global.get $missing))
                                     (local.set $a3 (global.get $missing)))
                               (else (if (i32.eq (local.get $argc) (i32.const 2))
                                         (then (local.set $a2 (global.get $missing))
                                               (local.set $a3 (global.get $missing)))
                                         (else (if (i32.eq (local.get $argc) (i32.const 3))
                                                   (then (local.set $a3 (global.get $missing)))
                                                   (else (local.set $a3
                                                                    (array.get $Args (local.get $args) (i32.const 3)))))))))))
                 (return_call_ref $Prim4
                                  (local.get $a0)
                                  (local.get $a1)
                                  (local.get $a2)
                                  (local.get $a3)
                                  (ref.cast (ref $Prim4) (local.get $code))))
           (else (return (call $raise-code-type-mismatch (local.get $pproc)))))
       (unreachable))]
    [29 ; between 0 and 5 arguments
     `((if (i32.gt_u (local.get $argc) (i32.const 5))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (ref.test (ref $Prim5) (local.get $code))
           (then (if (i32.eqz (local.get $argc))
                     (then (local.set $a0 (global.get $missing))
                           (local.set $a1 (global.get $missing))
                           (local.set $a2 (global.get $missing))
                           (local.set $a3 (global.get $missing))
                           (local.set $a4 (global.get $missing)))
                     (else (if (i32.eq (local.get $argc) (i32.const 1))
                               (then (local.set $a1 (global.get $missing))
                                     (local.set $a2 (global.get $missing))
                                     (local.set $a3 (global.get $missing))
                                     (local.set $a4 (global.get $missing)))
                               (else (if (i32.eq (local.get $argc) (i32.const 2))
                                         (then (local.set $a2 (global.get $missing))
                                               (local.set $a3 (global.get $missing))
                                               (local.set $a4 (global.get $missing)))
                                         (else (if (i32.eq (local.get $argc) (i32.const 3))
                                                   (then (local.set $a3 (global.get $missing))
                                                         (local.set $a4 (global.get $missing)))
                                                   (else (if (i32.eq (local.get $argc) (i32.const 4))
                                                             (then (local.set $a3
                                                                              (array.get $Args (local.get $args) (i32.const 3)))
                                                                   (local.set $a4 (global.get $missing)))
                                                             (else (local.set $a3
                                                                              (array.get $Args (local.get $args) (i32.const 3)))
                                                                   (local.set $a4
                                                                              (array.get $Args (local.get $args) (i32.const 4)))))))))))))
                 (return_call_ref $Prim5
                                  (local.get $a0)
                                  (local.get $a1)
                                  (local.get $a2)
                                  (local.get $a3)
                                  (local.get $a4)
                                  (ref.cast (ref $Prim5) (local.get $code))))
           (else (return (call $raise-code-type-mismatch (local.get $pproc)))))
       (unreachable))]
    [18 ; between 1 and 2 arguments
     `((if (i32.lt_u (local.get $argc) (i32.const 1))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (i32.gt_u (local.get $argc) (i32.const 2))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (ref.test (ref $Prim12) (local.get $code))
           (then (if (i32.eq (local.get $argc) (i32.const 1))
                     (then (local.set $a1 (global.get $missing))))
                 (return_call_ref $Prim12
                                  (local.get $a0)
                                  (local.get $a1)
                                  (ref.cast (ref $Prim12) (local.get $code))))
           (else (return (call $raise-code-type-mismatch (local.get $pproc)))))
       (unreachable))]
    [19 ; between 1 and 3 arguments
     `((if (i32.lt_u (local.get $argc) (i32.const 1))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (i32.gt_u (local.get $argc) (i32.const 3))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (ref.test (ref $Prim13) (local.get $code))
           (then (if (i32.eq (local.get $argc) (i32.const 1))
                     (then (local.set $a1 (global.get $missing))
                           (local.set $a2 (global.get $missing)))
                     (else (if (i32.eq (local.get $argc) (i32.const 2))
                               (then (local.set $a2 (global.get $missing))))))
                 (return_call_ref $Prim13
                                  (local.get $a0)
                                  (local.get $a1)
                                  (local.get $a2)
                                  (ref.cast (ref $Prim13) (local.get $code))))
           (else (return (call $raise-code-type-mismatch (local.get $pproc)))))
       (unreachable))]
    [20 ; between 1 and 4 arguments
     `((if (i32.lt_u (local.get $argc) (i32.const 1))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (i32.gt_u (local.get $argc) (i32.const 4))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (ref.test (ref $Prim14) (local.get $code))
           (then (if (i32.eq (local.get $argc) (i32.const 1))
                     (then (local.set $a1 (global.get $missing))
                           (local.set $a2 (global.get $missing))
                           (local.set $a3 (global.get $missing))))
                 (if (i32.eq (local.get $argc) (i32.const 2))
                     (then (local.set $a2 (global.get $missing))
                           (local.set $a3 (global.get $missing))))
                 (if (i32.eq (local.get $argc) (i32.const 3))
                     (then (local.set $a3 (global.get $missing)))
                     (else (if (i32.eq (local.get $argc) (i32.const 4))
                               (then (local.set $a3
                                                (array.get $Args (local.get $args) (i32.const 3)))))))
                 (return_call_ref $Prim14
                                  (local.get $a0)
                                  (local.get $a1)
                                  (local.get $a2)
                                  (local.get $a3)
                                  (ref.cast (ref $Prim14) (local.get $code))))
           (else (return (call $raise-code-type-mismatch (local.get $pproc)))))
       (unreachable))]
    [21 ; between 1 and 5 arguments
     `((if (i32.lt_u (local.get $argc) (i32.const 1))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (i32.gt_u (local.get $argc) (i32.const 5))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (ref.test (ref $Prim15) (local.get $code))
           (then (if (i32.eq (local.get $argc) (i32.const 1))
                     (then (local.set $a1 (global.get $missing))
                           (local.set $a2 (global.get $missing))
                           (local.set $a3 (global.get $missing))
                           (local.set $a4 (global.get $missing))))
                 (if (i32.eq (local.get $argc) (i32.const 2))
                     (then (local.set $a2 (global.get $missing))
                           (local.set $a3 (global.get $missing))
                           (local.set $a4 (global.get $missing))))
                 (if (i32.eq (local.get $argc) (i32.const 3))
                     (then (local.set $a3 (global.get $missing))
                           (local.set $a4 (global.get $missing)))
                     (else (if (i32.eq (local.get $argc) (i32.const 4))
                               (then (local.set $a3
                                                (array.get $Args (local.get $args) (i32.const 3)))
                                     (local.set $a4 (global.get $missing)))
                               (else (if (i32.eq (local.get $argc) (i32.const 5))
                                         (then (local.set $a3
                                                          (array.get $Args (local.get $args) (i32.const 3)))
                                               (local.set $a4
                                                          (array.get $Args (local.get $args) (i32.const 4)))))))))
                 (return_call_ref $Prim15
                                  (local.get $a0)
                                  (local.get $a1)
                                  (local.get $a2)
                                  (local.get $a3)
                                  (local.get $a4)
                                  (ref.cast (ref $Prim15) (local.get $code))))
           (else (return (call $raise-code-type-mismatch (local.get $pproc)))))
       (unreachable))]
    [22 ; between 2 and 4 arguments
     `((if (i32.lt_u (local.get $argc) (i32.const 2))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (i32.gt_u (local.get $argc) (i32.const 4))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (ref.test (ref $Prim24) (local.get $code))
           (then (if (i32.eq (local.get $argc) (i32.const 2))
                     (then (local.set $a2 (global.get $missing))
                           (local.set $a3 (global.get $missing))))
                 (if (i32.eq (local.get $argc) (i32.const 3))
                     (then (local.set $a3 (global.get $missing)))
                     (else (if (i32.eq (local.get $argc) (i32.const 4))
                               (then (local.set $a3
                                                (array.get $Args (local.get $args) (i32.const 3)))))))
                 (return_call_ref $Prim24
                                  (local.get $a0)
                                  (local.get $a1)
                                  (local.get $a2)
                                  (local.get $a3)
                                  (ref.cast (ref $Prim24) (local.get $code))))
           (else (return (call $raise-code-type-mismatch (local.get $pproc)))))
       (unreachable))]
    [23 ; between 2 and 5 arguments
     `((if (i32.lt_u (local.get $argc) (i32.const 2))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (i32.gt_u (local.get $argc) (i32.const 5))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (ref.test (ref $Prim25) (local.get $code))
           (then (if (i32.eq (local.get $argc) (i32.const 2))
                     (then (local.set $a2 (global.get $missing))
                           (local.set $a3 (global.get $missing))
                           (local.set $a4 (global.get $missing))))
                 (if (i32.eq (local.get $argc) (i32.const 3))
                     (then (local.set $a3 (global.get $missing))
                           (local.set $a4 (global.get $missing)))
                     (else (if (i32.eq (local.get $argc) (i32.const 4))
                               (then (local.set $a3
                                                (array.get $Args (local.get $args) (i32.const 3)))
                                     (local.set $a4 (global.get $missing)))
                               (else (if (i32.eq (local.get $argc) (i32.const 5))
                                         (then (local.set $a3
                                                          (array.get $Args (local.get $args) (i32.const 3)))
                                               (local.set $a4
                                                          (array.get $Args (local.get $args) (i32.const 4)))))))))
                 (return_call_ref $Prim25
                                  (local.get $a0)
                                  (local.get $a1)
                                  (local.get $a2)
                                  (local.get $a3)
                                  (local.get $a4)
                                  (ref.cast (ref $Prim25) (local.get $code))))
           (else (return (call $raise-code-type-mismatch (local.get $pproc)))))
       (unreachable))]
    [24 ; between 3 and 5 arguments
     `((if (i32.lt_u (local.get $argc) (i32.const 3))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (i32.gt_u (local.get $argc) (i32.const 5))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (ref.test (ref $Prim35) (local.get $code))
           (then (if (i32.eq (local.get $argc) (i32.const 3))
                     (then (local.set $a3 (global.get $missing))
                           (local.set $a4 (global.get $missing)))
                     (else (if (i32.eq (local.get $argc) (i32.const 4))
                               (then (local.set $a3
                                                (array.get $Args (local.get $args) (i32.const 3)))
                                     (local.set $a4 (global.get $missing)))
                               (else (if (i32.eq (local.get $argc) (i32.const 5))
                                         (then (local.set $a3
                                                          (array.get $Args (local.get $args) (i32.const 3)))
                                               (local.set $a4
                                                          (array.get $Args (local.get $args) (i32.const 4)))))))))
                 (return_call_ref $Prim35
                                  (local.get $a0)
                                  (local.get $a1)
                                  (local.get $a2)
                                  (local.get $a3)
                                  (local.get $a4)
                                  (ref.cast (ref $Prim35) (local.get $code))))
           (else (return (call $raise-code-type-mismatch (local.get $pproc)))))
       (unreachable))]
    [25 ; between 6 and 7 arguments
     `((if (i32.lt_u (local.get $argc) (i32.const 6))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (i32.gt_u (local.get $argc) (i32.const 7))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (ref.test (ref $Prim67) (local.get $code))
           (then (local.set $a0 (array.get $Args (local.get $args) (i32.const 0)))
                 (local.set $a1 (array.get $Args (local.get $args) (i32.const 1)))
                 (local.set $a2 (array.get $Args (local.get $args) (i32.const 2)))
                 (local.set $a3 (array.get $Args (local.get $args) (i32.const 3)))
                 (local.set $a4 (array.get $Args (local.get $args) (i32.const 4)))
                 (local.set $a5 (array.get $Args (local.get $args) (i32.const 5)))
                 (if (i32.eq (local.get $argc) (i32.const 6))
                     (then (local.set $a6 (global.get $missing)))
                     (else (local.set $a6
                                      (array.get $Args (local.get $args) (i32.const 6)))))
                 (return_call_ref $Prim67
                                  (local.get $a0)
                                  (local.get $a1)
                                  (local.get $a2)
                                  (local.get $a3)
                                  (local.get $a4)
                                  (local.get $a5)
                                  (local.get $a6)
                                  (ref.cast (ref $Prim67) (local.get $code))))
           (else (return (call $raise-code-type-mismatch (local.get $pproc)))))
       (unreachable))]
    [26 ; exact 6
     `((if (i32.eq (local.get $argc) (i32.const 6))
           (then (if (ref.test (ref $Prim6) (local.get $code))
                     (then (return_call_ref $Prim6
                                            (local.get $a0)
                                            (local.get $a1)
                                            (local.get $a2)
                                            (array.get $Args (local.get $args) (i32.const 3))
                                            (array.get $Args (local.get $args) (i32.const 4))
                                            (array.get $Args (local.get $args) (i32.const 5))
                                            (ref.cast (ref $Prim6) (local.get $code))))
                     (else (return (call $raise-code-type-mismatch (local.get $pproc))))))
           (else (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (unreachable))]
    [30 ; at least 4, rest arguments as list
     `((if (i32.ge_u (local.get $argc) (i32.const 4))
           (then
            (if (ref.test (ref $Prim>=4) (local.get $code))
                (then (local.set $a3
                                 (array.get $Args (local.get $args) (i32.const 3)))
                      (local.set $rest
                                 (call $rest-arguments->list
                                       (local.get $args)
                                       (i32.const 4)))
                      (return_call_ref $Prim>=4
                                       (local.get $a0)
                                       (local.get $a1)
                                       (local.get $a2)
                                       (local.get $a3)
                                       (local.get $rest)
                                       (ref.cast (ref $Prim>=4) (local.get $code))))
                (else (return (call $raise-code-type-mismatch (local.get $pproc))))))
           (else (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (unreachable))]
    [31 ; at least 4, rest arguments as $Args array
     `((if (i32.ge_u (local.get $argc) (i32.const 4))
           (then
            (if (ref.test (ref $Prim>=4) (local.get $code))
                (then (return_call_ref $Prim>=4
                                       (local.get $a0)
                                       (local.get $a1)
                                       (local.get $a2)
                                       (array.get $Args (local.get $args) (i32.const 3))
                                       (call $rest-arguments->args
                                             (local.get $args)
                                             (i32.const 4))
                                       (ref.cast (ref $Prim>=4) (local.get $code))))
                (else (return (call $raise-code-type-mismatch (local.get $pproc))))))
           (else (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (unreachable))]
    [32 ; between 2 and 7 arguments, rest arguments as list
     `((if (i32.lt_u (local.get $argc) (i32.const 2))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (i32.gt_u (local.get $argc) (i32.const 7))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (ref.test (ref $Prim>=2) (local.get $code))
           (then (local.set $rest
                            (call $rest-arguments->list
                                  (local.get $args)
                                  (i32.const 2)))
                 (return_call_ref $Prim>=2
                                  (local.get $a0)
                                  (local.get $a1)
                                  (local.get $rest)
                                  (ref.cast (ref $Prim>=2) (local.get $code))))
           (else (return (call $raise-code-type-mismatch (local.get $pproc)))))
       (unreachable))]
    [33 ; between 2 and 6 arguments, rest arguments as list after the first
     `((if (i32.lt_u (local.get $argc) (i32.const 2))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (i32.gt_u (local.get $argc) (i32.const 6))
           (then (return (call $primitive-invoke:raise-arity-error
                               (local.get $pproc) (local.get $argc)))))
       (if (ref.test (ref $Prim>=1) (local.get $code))
           (then (local.set $rest
                            (call $rest-arguments->list
                                  (local.get $args)
                                  (i32.const 1)))
                 (return_call_ref $Prim>=1
                                  (local.get $a0)
                                  (local.get $rest)
                                  (ref.cast (ref $Prim>=1) (local.get $code))))
           (else (return (call $raise-code-type-mismatch (local.get $pproc)))))
       (unreachable))]
    [_ (error 'primitive-invoker-tail "unknown shape: ~a" shape)]))

;; primitive-invoker : exact-nonnegative-integer? -> datum?
;;   Generate the full primitive invoker function for shape.
(define (primitive-invoker shape)
  `(func ,($ (shape->invoker shape)) (type $ProcedureInvoker)
         (param $proc (ref $Procedure))
         (param $args (ref $Args))
         (result      (ref eq))

         (local $pproc (ref $PrimitiveProcedure))
         (local $code  (ref null func))

         (local $argc      i32)

         (local $a0 (ref eq))
         (local $a1 (ref eq))
         (local $a2 (ref eq))
         (local $a3 (ref eq)) ; ? needed - see primitive-invoker-tail above
         (local $a4 (ref eq)) ; ? needed
         (local $a5 (ref eq)) ; ? needed
         (local $a6 (ref eq)) ; ? needed
         (local $rest (ref eq)) ; todo: we should limit this to fewer locals

         (local.set $a0   (global.get $null))
         (local.set $a1   (global.get $null))
         (local.set $a2   (global.get $null))
         (local.set $a3   (global.get $null))
         (local.set $a4   (global.get $null)) ; todo remove these
         (local.set $a5   (global.get $null)) ; todo remove these
         (local.set $a6   (global.get $null)) ; todo remove these
         (local.set $rest (global.get $null))

         (local.set $pproc
                    (ref.cast (ref $PrimitiveProcedure) (local.get $proc)))

         (local.set $code
                    (struct.get $PrimitiveProcedure $code (local.get $pproc)))

         (if (ref.is_null (local.get $code))
             (then (return (call $raise-no-code (local.get $pproc)))))

         (local.set $argc (array.len (local.get $args)))

         (if (i32.gt_u (local.get $argc) (i32.const 0))
             (then
              (local.set $a0
                         (array.get $Args (local.get $args) (i32.const 0)))))

         (if (i32.gt_u (local.get $argc) (i32.const 1))
             (then
              (local.set $a1
                         (array.get $Args (local.get $args) (i32.const 1)))))

         (if (i32.gt_u (local.get $argc) (i32.const 2))
             (then
              (local.set $a2
                         (array.get $Args (local.get $args) (i32.const 2)))))

         ,@(primitive-invoker-tail shape)))

;; procedure-primitive-invoker-forms : (listof exact-nonnegative-integer?) -> (listof datum?)
;;   Generate shape-specific primitive invoker functions.
(define (procedure-primitive-invoker-forms shapes)
  (for/list ([shape (in-list shapes)])
    (primitive-invoker shape)))

;; procedure-base-core-runtime-helpers : (listof core-runtime-helper?)
;;   Enumerate hand-written procedure helpers that can be emitted in core runtime.
(define procedure-base-core-runtime-helpers
  (list
   ;; rest-arguments->list : (ref $Args) i32 -> (ref eq)
   ;;   Copy arguments from index n onward into a freshly allocated list.
   (core-runtime-helper
    '$rest-arguments->list
    "$rest-arguments->list"
    (lambda ()
      `(func $rest-arguments->list
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
                 (then (return ,(Imm '()))))
             ;; Start with empty list
             (local.set $xs ,(Imm '()))
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
             (local.get $xs))))

   ;; rest-arguments->args : (ref $Args) i32 -> (ref $Args)
   ;;   Copy arguments from index n onward into a fresh argument array.
   (core-runtime-helper
    '$rest-arguments->args
    "$rest-arguments->args"
    (lambda ()
      `(func $rest-arguments->args
             (param $args (ref $Args))
             (param $n    i32)
             (result      (ref $Args))

             (local $len   i32)
             (local $count i32)
             (local $res   (ref $Args))

             (local.set $len (array.len (local.get $args)))
             (if (i32.ge_u (local.get $n) (local.get $len))
                 (then (return (array.new $Args ,(Imm '()) (i32.const 0)))))
             (local.set $count (i32.sub (local.get $len) (local.get $n)))
             (local.set $res (array.new $Args ,(Imm '()) (local.get $count)))
             (array.copy $Args $Args
                         (local.get $res)
                         (i32.const 0)
                         (local.get $args)
                         (local.get $n)
                         (local.get $count))
             (local.get $res))))

   ;; procedure-name->error-string : any/c -> string?
   ;;   Convert the stored procedure name to a compact error-message string.
   (core-runtime-helper
    '$procedure-name->error-string
    "$procedure-name->error-string"
    (lambda ()
      `(func $procedure-name->error-string
             (param $name (ref eq))
             (result      (ref $String))

             (if (ref.test (ref $String) (local.get $name))
                 (then (return (ref.cast (ref $String) (local.get $name)))))
             (if (ref.test (ref $Symbol) (local.get $name))
                 (then (return (struct.get $Symbol $name
                                (ref.cast (ref $Symbol) (local.get $name))))))
             (ref.cast (ref $String) (global.get $string:procedure?)))))

   ;; arity-marker->expected-string : i32 -> string?
   ;;   Convert one arity marker to an expected-argument-count string.
   (core-runtime-helper
    '$arity-marker->expected-string
    "$arity-marker->expected-string"
    (lambda ()
      `(func $arity-marker->expected-string
             (param $marker i32)
             (result        (ref $String))

             (local $n   i32)
             (local $out (ref $I32GrowableArray))

             (if (i32.ge_s (local.get $marker) (i32.const 0))
                 (then (return (call $i32->string (local.get $marker)))))
             (local.set $n (i32.sub (i32.const -1) (local.get $marker)))
             (local.set $out (call $make-i32growable-array (i32.const 16)))
             (call $i32growable-array-add-string!
                   (local.get $out)
                   (ref.cast (ref $String)
                             (global.get $string:arity-error:at-least)))
             (call $i32growable-array-add-string!
                   (local.get $out)
                   (call $i32->string (local.get $n)))
             (call $i32growable-array->string (local.get $out)))))

   ;; procedure-arity-field->expected-string : any/c -> string?
   ;;   Convert a compact procedure arity field into an error-message fragment.
   (core-runtime-helper
    '$procedure-arity-field->expected-string
    "$procedure-arity-field->expected-string"
    (lambda ()
      `(func $procedure-arity-field->expected-string
             (param $arity-field (ref eq))
             (result             (ref $String))

             (local $markers (ref $I32Array))
             (local $out     (ref $I32GrowableArray))
             (local $len     i32)
             (local $i       i32)

             (if (ref.test (ref i31) (local.get $arity-field))
                 (then
                  (return
                   (call $arity-marker->expected-string
                         (i32.shr_s
                          (i31.get_s (ref.cast (ref i31) (local.get $arity-field)))
                          (i32.const 1))))))
             (local.set $markers (ref.cast (ref $I32Array) (local.get $arity-field)))
             (local.set $len (array.len (local.get $markers)))
             (local.set $out (call $make-i32growable-array (i32.const 32)))
             (block $done
                    (loop $loop
                          (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
                          (if (i32.ne (local.get $i) (i32.const 0))
                              (then (call $i32growable-array-add-string!
                                          (local.get $out)
                                          (ref.cast (ref $String)
                                                    (global.get $string:arity-error:or)))))
                          (call $i32growable-array-add-string!
                                (local.get $out)
                                (call $arity-marker->expected-string
                                      (array.get $I32Array
                                                 (local.get $markers)
                                                 (local.get $i))))
                          (local.set $i (i32.add (local.get $i) (i32.const 1)))
                          (br $loop)))
             (call $i32growable-array->string (local.get $out)))))

   ;; primitive-invoke:raise-arity-error : primitive-procedure? i32 -> none
   ;;   Raise a catchable primitive application arity mismatch.
   (core-runtime-helper
    '$primitive-invoke:raise-arity-error
    "$primitive-invoke:raise-arity-error"
    (lambda ()
      `(func $primitive-invoke:raise-arity-error
             (param $pproc (ref $PrimitiveProcedure))
             (param $argc  i32)
             (result       (ref eq))

             (local $name-str     (ref $String))
             (local $expected-str (ref $String))
             (local $received-str (ref $String))
             (local $out          (ref $I32GrowableArray))
             (local $message      (ref $String))

             (local.set $name-str
                        (call $procedure-name->error-string
                              (struct.get $Procedure $name (local.get $pproc))))
             (local.set $expected-str
                        (call $procedure-arity-field->expected-string
                              (struct.get $Procedure $arity (local.get $pproc))))
             (local.set $received-str
                        (call $i32->string (local.get $argc)))

             (local.set $out (call $make-i32growable-array (i32.const 80)))
             (call $i32growable-array-add-string! (local.get $out) (local.get $name-str))
             (call $i32growable-array-add-string!
                   (local.get $out)
                   (ref.cast (ref $String)
                             (global.get $string:arity-error:start)))
             (call $i32growable-array-add-string! (local.get $out) (local.get $expected-str))
             (call $i32growable-array-add-string!
                   (local.get $out)
                   (ref.cast (ref $String)
                             (global.get $string:arity-error:given)))
             (call $i32growable-array-add-string! (local.get $out) (local.get $received-str))
             (local.set $message (call $i32growable-array->string (local.get $out)))

             (call $raise
                   (call $exn:fail:contract:arity/make
                         (local.get $message)
                         (call $current-continuation-marks (global.get $missing)))
                   (global.get $true))
             (unreachable))))

   ;; raise-no-code : primitive-procedure? -> none
   ;;   Raise a catchable internal error for a primitive with no code pointer.
   (core-runtime-helper
    '$raise-no-code
    "$raise-no-code"
    (lambda ()
      `(func $raise-no-code
             (param $pproc (ref $PrimitiveProcedure))
             (result       (ref eq))

             (call $raise
                   (call $exn:fail:contract/make
                         (ref.cast (ref $String)
                                   (global.get $string:primitive-invoke:no-code))
                         (call $current-continuation-marks (global.get $missing)))
                   (global.get $true))
             (unreachable))))

   ;; raise-code-type-mismatch : primitive-procedure? -> none
   ;;   Raise a catchable internal error for a primitive code-shape mismatch.
   (core-runtime-helper
    '$raise-code-type-mismatch
    "$raise-code-type-mismatch"
    (lambda ()
      `(func $raise-code-type-mismatch
             (param $pproc (ref $PrimitiveProcedure))
             (result       (ref eq))

             (call $raise
                   (call $exn:fail:contract/make
                         (ref.cast (ref $String)
                                   (global.get $string:primitive-invoke:code-type-mismatch))
                         (call $current-continuation-marks (global.get $missing)))
                   (global.get $true))
             (unreachable))))

   ;; raise-primitive-invoke:unhandled-shape : -> none
   ;;   Raise a catchable internal error for a primitive shape unsupported by the generic dispatcher.
   (core-runtime-helper
    '$raise-primitive-invoke:unhandled-shape
    "$raise-primitive-invoke:unhandled-shape"
    (lambda ()
      `(func $raise-primitive-invoke:unhandled-shape
             (call $raise
                   (call $exn:fail:contract/make
                         (global.get $string:primitive-invoke:unhandled-shape)
                         (call $current-continuation-marks (global.get $missing)))
                   (global.get $true))
             (unreachable))))))

;; procedure-invocation-debug-form? : datum? -> boolean?
;;   Recognize probe-only instructions that would pull JS logging into core runtime.
(define (procedure-invocation-debug-form? form)
  (or (equal? form '(global.set $debug-current-procedure (local.get $proc)))
      (equal? form '(call $debug-invoke-tick))))

;; primitive-invoke-core-form : datum? -> datum?
;;   Return the generic primitive dispatcher without program-side debug probes.
(define (primitive-invoke-core-form form)
  (match form
    [`(func $primitive-invoke ,parts ...)
     `(func $primitive-invoke
            ,@(for/list ([part (in-list parts)]
                         #:unless (procedure-invocation-debug-form? part))
                part))]
    [_
     (error 'primitive-invoke-core-form
            "expected $primitive-invoke function form, got: ~s"
            form)]))

;; invoke-closure-core-form : datum? -> datum?
;;   Return the closure invoker without program-side debug probes.
(define (invoke-closure-core-form form)
  (match form
    [`(func $invoke-closure ,parts ...)
     `(func $invoke-closure
            ,@(for/list ([part (in-list parts)]
                         #:unless (procedure-invocation-debug-form? part))
                part))]
    [_
     (error 'invoke-closure-core-form
            "expected $invoke-closure function form, got: ~s"
            form)]))

;; procedure-primitive-invoker-core-runtime-helpers : (listof core-runtime-helper?)
;;   Enumerate stable primitive shape invokers available in core runtime.
(define procedure-primitive-invoker-core-runtime-helpers
  (function-forms->runtime-helpers
   'procedure-primitive-invoker-core-runtime-helpers
   (procedure-primitive-invoker-forms primitive-shapes)))

;; procedure-rest-argument-core-runtime-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for rest-argument core helpers.
(define procedure-rest-argument-core-runtime-helper-names
  '($rest-arguments->list
    $rest-arguments->args))

;; procedure-primitive-error-core-runtime-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for core primitive invocation errors.
(define procedure-primitive-error-core-runtime-helper-names
  '($procedure-name->error-string
    $arity-marker->expected-string
    $procedure-arity-field->expected-string
    $primitive-invoke:raise-arity-error
    $raise-no-code
    $raise-code-type-mismatch
    $raise-primitive-invoke:unhandled-shape))

;; procedure-basic-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for basic procedure helpers.
(define procedure-basic-runtime-only-helper-names
  '($identity
    $procedure?
    $procedure-specialize
    $apply
    $procedure->method
    $procedure-rename
    $procedure-realm
    $raise-argument-error:procedure-expected/who
    $raise-argument-error:procedure-expected))

;; procedure-arity-at-least-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for arity-at-least helpers.
(define procedure-arity-at-least-runtime-only-helper-names
  '($ensure-arity-at-least-type
    $arity-at-least/make
    $arity-at-least/make/value
    $make-arity-at-least
    $arity-at-least
    $raise-argument-error:arity-at-least-expected
    $arity-at-least-unwrap
    $arity-at-least?
    $arity-at-least-value))

;; procedure-arity-field-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for procedure arity field helpers.
(define procedure-arity-field-runtime-only-helper-names
  '($procedure-arity-entry->marker/i32
    $procedure-arity?
    $procedure-arity-value->field
    $procedure-arity-field-includes?/i32
    $procedure-arity-field-subset-of-mask?/i32
    $procedure-arity-field->mask/i32
    $procedure-arity-mask-value->field))

;; procedure-composition-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for procedure composition helpers.
(define procedure-composition-runtime-only-helper-names
  '($make-composed-procedure
    $compose
    $compose1
    $procedure-extract-target))

;; procedure-reduction-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for procedure arity reduction helpers.
(define procedure-reduction-runtime-only-helper-names
  '($procedure-reduce-arity
    $procedure-reduce-arity-mask))

;; procedure-arity-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for procedure arity accessors.
(define procedure-arity-runtime-only-helper-names
  '($procedure-arity
    $procedure-arity/checked))

;; procedure-result-arity-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for procedure result arity helpers.
(define procedure-result-arity-runtime-only-helper-names
  '($procedure-result-arity))

;; procedure-arity-mask-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for procedure arity mask helpers.
(define procedure-arity-mask-runtime-only-helper-names
  '($procedure-arity-mask
    $procedure-arity-mask/checked/i32
    $raise-argument-error:fixnum-expected
    $procedure-arity-mask-includes?/i32
    $procedure-arity-includes?
    $procedure-arity-includes?/checked/i32))

;; primitive-reflection-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for primitive reflection helpers.
(define primitive-reflection-runtime-only-helper-names
  '($raise-argument-error:primitive-procedure-expected/who
    $raise-argument-error:primitive-procedure-expected
    $primitive?
    $primitive-result-arity
    $primitive-closure?))

;; procedure-invocation-error-all-runtime-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for procedure invocation error helpers.
(define procedure-invocation-error-all-runtime-helper-names
  '($raise-arity-mismatch
    $procedure-arity->expected-string
    $raise-arity-mismatch/proc
    $raise-arity-mismatch/name+given
    $result-arity-count
    $raise-result-arity-error/internal
    $raise-binding-result-arity-error
    $raise-definition-result-arity-error))

;; procedure-invocation-error-core-runtime-helper-names : (listof symbol?)
;;   Name invocation error helpers available in the standalone core runtime.
(define procedure-invocation-error-core-runtime-helper-names
  '($raise-arity-mismatch/proc))

;; procedure-invocation-error-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime-only invocation error helper emission order.
(define procedure-invocation-error-runtime-only-helper-names
  (remove* procedure-invocation-error-core-runtime-helper-names
           procedure-invocation-error-all-runtime-helper-names))

;; procedure-application-error-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for procedure application error helpers.
(define procedure-application-error-runtime-only-helper-names
  '($raise-application-not-procedure
    $raise-apply-final-arg-not-list
    $raise-struct-type-property-accessor-contract))

;; procedure-invoker-all-runtime-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for procedure invoker helpers.
(define procedure-invoker-all-runtime-helper-names
  '($invoke-closure
    $invoke-reduced-procedure
    $invoke-composed-procedure
    $invoke-case-closure))

;; procedure-invoker-core-runtime-helper-names : (listof symbol?)
;;   Name procedure invokers available in the standalone core runtime.
(define procedure-invoker-core-runtime-helper-names
  '($invoke-closure))

;; procedure-invoker-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime-only procedure invoker emission order.
(define procedure-invoker-runtime-only-helper-names
  (remove* procedure-invoker-core-runtime-helper-names
           procedure-invoker-all-runtime-helper-names))

;; primitive-invocation-error-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for primitive invocation error helpers.
(define primitive-invocation-error-runtime-only-helper-names
  '())

;; procedure-case-lambda-all-runtime-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for procedure case-lambda helpers.
(define procedure-case-lambda-all-runtime-helper-names
  '($repack-arguments
    $raise-arity-error/case-lambda/arities
    $code:case-lambda-dispatch))

;; procedure-case-lambda-core-runtime-helper-names : (listof symbol?)
;;   Name case-lambda helpers available in the standalone core runtime.
(define procedure-case-lambda-core-runtime-helper-names
  '($repack-arguments))

;; procedure-case-lambda-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime-only case-lambda helper emission order.
(define procedure-case-lambda-runtime-only-helper-names
  (remove* procedure-case-lambda-core-runtime-helper-names
           procedure-case-lambda-all-runtime-helper-names))

;; procedure-invocation-error-all-runtime-helpers : (listof core-runtime-helper?)
;;   Enumerate all procedure invocation error helpers.
(define procedure-invocation-error-all-runtime-helpers
  (function-forms->runtime-helpers
   'procedure-invocation-error-all-runtime-helpers
   '(
        (func $raise-arity-mismatch
              (call $raise
                    (call $exn:fail:contract:arity/make
                          (global.get $string:arity-error:generic)
                          (call $current-continuation-marks (global.get $missing)))
                    (global.get $true))
              (unreachable))

        (func $procedure-arity->expected-string
              (param $proc (ref $Procedure))
              (result (ref $String))
              (local $a         (ref eq))
              (local $arity/tag i32)
              (local $arity     i32)

              (local.set $a (struct.get $Procedure $arity (local.get $proc)))

              (if (ref.test (ref i31) (local.get $a))
                  (then
                   (local.set $arity/tag (i31.get_s (ref.cast (ref i31) (local.get $a))))
                   (local.set $arity (i32.shr_s (local.get $arity/tag) (i32.const 1)))
                   (return (call $arity-i32->string (local.get $arity)))))

              (ref.cast (ref $String)
                        (call $format/display (call $procedure-arity (local.get $proc)))))

        (func $raise-arity-mismatch/proc
              (param $proc (ref $Procedure))
              (param $argc i32)
              (local $name         (ref eq))
              (local $debug-id     (ref eq))
              (local $expected-str (ref $String))
              (local $received-str (ref $String))
              (local $out          (ref $GrowableArray))
              (local $message      (ref $String))

              (local.set $name
                         (struct.get $Procedure $name (local.get $proc)))
              (local.set $debug-id (global.get $false))
              (if (ref.test (ref $Closure) (local.get $proc))
                  (then
                   (local.set $debug-id
                              (struct.get $Closure $debug-id
                                          (ref.cast (ref $Closure) (local.get $proc))))))

              (local.set $expected-str
                         (call $procedure-arity->expected-string (local.get $proc)))
              (local.set $received-str
                         (call $i32->string (local.get $argc)))

              (if (i32.gt_u (global.get $debug-invoke-budget) (i32.const 0))
                  (then
                   (drop (call $js-log (global.get $symbol:procedure-arity-frontier)))
                   (drop (call $js-log
                               (call $cons
                                     (call $cons (global.get $symbol:name)
                                           (call $cons (local.get $name)
                                                 (global.get $null)))
                                     (global.get $null))))
                   (drop (call $js-log
                               (call $cons
                                     (call $cons (global.get $symbol:debug-id)
                                           (call $cons (local.get $debug-id)
                                                 (global.get $null)))
                                     (global.get $null))))
                   (drop (call $js-log
                               (call $cons
                                     (call $cons (global.get $symbol:expected-count)
                                           (call $cons (local.get $expected-str)
                                                 (global.get $null)))
                                     (global.get $null))))
                   (drop (call $js-log
                               (call $cons
                                     (call $cons (global.get $symbol:received-count)
                                           (call $cons (local.get $received-str)
                                                 (global.get $null)))
                                     (global.get $null))))))

              (local.set $out
                         (call $make-growable-array (i32.const 5)))
              (call $growable-array-add! (local.get $out) (call $format/display (local.get $name)))
              (call $growable-array-add! (local.get $out) (global.get $string:arity-error:start))
              (call $growable-array-add! (local.get $out) (local.get $expected-str))
              (call $growable-array-add! (local.get $out) (global.get $string:arity-error:given))
              (call $growable-array-add! (local.get $out) (local.get $received-str))
              (local.set $message
                         (call $growable-array-of-strings->string (local.get $out)))

              (call $raise
                    (call $exn:fail:contract:arity/make
                          (local.get $message)
                          (call $current-continuation-marks (global.get $missing)))
                    (global.get $true))
              (unreachable))

        (func $raise-arity-mismatch/name+given
              (param $name (ref eq))
              (param $argc i32)
              (local $received-str (ref $String))
              (local $out          (ref $GrowableArray))
              (local $message      (ref $String))

              ;; TODO: include "arguments..." block like Racket.
              (local.set $received-str
                         (call $i32->string (local.get $argc)))
              (local.set $out
                         (call $make-growable-array (i32.const 3)))
              (call $growable-array-add! (local.get $out) (call $format/display (local.get $name)))
              (call $growable-array-add! (local.get $out) (global.get $string:arity-error:start/no-expected))
              (call $growable-array-add! (local.get $out) (local.get $received-str))
              (local.set $message
                         (call $growable-array-of-strings->string (local.get $out)))

              (call $raise
                    (call $exn:fail:contract:arity/make
                          (local.get $message)
                          (call $current-continuation-marks (global.get $missing)))
                    (global.get $true))
              (unreachable))

        ;; result-arity-count : (or/c exact-integer? list? values?) -> i32
        ;;   Compute counts for Schemify's result-arity helper primitives.
        (func $result-arity-count
              (param $v (ref eq))
              (result i32)

              (if (ref.test (ref i31) (local.get $v))
                  (then (return (i32.shr_s
                                 (i31.get_s (ref.cast (ref i31) (local.get $v)))
                                 (i32.const 1)))))
              (if (ref.test (ref $Values) (local.get $v))
                  (then (return (array.len (ref.cast (ref $Values) (local.get $v))))))
              (call $length/i32 (local.get $v)))

        ;; raise-result-arity-error/internal : symbol? any/c any/c -> none
        ;;   Raise a catchable result-arity error for internal Schemify checks.
        (func $raise-result-arity-error/internal
              (param $who      (ref eq))      ;; symbol
              (param $expected (ref eq))      ;; expected count or identifier list
              (param $args     (ref eq))      ;; received values as list or $Values
              (result          (ref eq))

              (local $out          (ref $GrowableArray))
              (local $message      (ref $String))

              (local.set $out
                         (call $make-growable-array (i32.const 5)))
              (call $growable-array-add! (local.get $out) (call $format/display (local.get $who)))
              (call $growable-array-add! (local.get $out) (global.get $string:result-arity-error:start))
              (call $growable-array-add! (local.get $out)
                    (call $i32->string (call $result-arity-count (local.get $expected))))
              (call $growable-array-add! (local.get $out) (global.get $string:result-arity-error:received))
              (call $growable-array-add! (local.get $out)
                    (call $i32->string (call $result-arity-count (local.get $args))))
              (local.set $message
                         (call $growable-array-of-strings->string (local.get $out)))

              (call $raise
                    (call $exn:fail:contract:arity/make
                          (local.get $message)
                          (call $current-continuation-marks (global.get $missing)))
                    (global.get $true))
              (unreachable))

        ;; raise-binding-result-arity-error : any/c any/c -> none
        ;;   Report a local-binding result-arity mismatch from Schemify output.
        (func $raise-binding-result-arity-error
              (type $Prim2)
              (param $expected (ref eq))
              (param $args     (ref eq))
              (result          (ref eq))

              (local $caller-proc     (ref eq))
              (local $caller-name     (ref eq))
              (local $caller-debug-id (ref eq))

              (if (i32.gt_u (global.get $debug-invoke-budget) (i32.const 0))
                  (then
                   (local.set $caller-proc (global.get $debug-current-procedure))
                   (local.set $caller-name (global.get $debug-current-procedure-name))
                   (local.set $caller-debug-id (global.get $false))
                   (if (ref.test (ref $Procedure) (local.get $caller-proc))
                       (then
                        (local.set $caller-name
                                   (struct.get $Procedure $name
                                               (ref.cast (ref $Procedure)
                                                         (local.get $caller-proc))))))
                   (if (ref.test (ref $Closure) (local.get $caller-proc))
                       (then
                        (local.set $caller-debug-id
                                   (struct.get $Closure $debug-id
                                               (ref.cast (ref $Closure)
                                                         (local.get $caller-proc))))))
                   (drop (call $js-log
                               (global.get $symbol:binding-result-arity-frontier)))
                   (drop (call $js-log
                               (call $cons
                                     (call $cons (global.get $symbol:expected-count)
                                           (call $cons
                                                 (call $i32->string
                                                       (call $result-arity-count
                                                             (local.get $expected)))
                                                 (global.get $null)))
                                     (global.get $null))))
                   (drop (call $js-log
                               (call $cons
                                     (call $cons (global.get $symbol:received-count)
                                           (call $cons
                                                 (call $i32->string
                                                       (call $result-arity-count
                                                             (local.get $args)))
                                                 (global.get $null)))
                                     (global.get $null))))
                   (drop (call $js-log
                               (call $cons
                                     (call $cons (global.get $symbol:caller-name)
                                           (call $cons (local.get $caller-name)
                                                 (global.get $null)))
                                     (global.get $null))))
                   (drop (call $js-log
                               (call $cons
                                     (call $cons (global.get $symbol:caller-debug-id)
                                           (call $cons (local.get $caller-debug-id)
                                                 (global.get $null)))
                                     (global.get $null))))))
              (call $raise-result-arity-error/internal
                    (global.get $symbol:raise-binding-result-arity-error)
                    (local.get $expected)
                    (local.get $args)))

        ;; raise-definition-result-arity-error : any/c any/c -> none
        ;;   Report a define-values result-arity mismatch from Schemify output.
        (func $raise-definition-result-arity-error
              (type $Prim2)
              (param $expected (ref eq))
              (param $args     (ref eq))
              (result          (ref eq))
              (call $raise-result-arity-error/internal
                    (global.get $symbol:raise-definition-result-arity-error)
                    (local.get $expected)
                    (local.get $args)))
     )))

;; procedure-invocation-error-core-runtime-helpers : (listof core-runtime-helper?)
;;   Register invocation error helpers available in the standalone core runtime.
(define procedure-invocation-error-core-runtime-helpers
  (select-runtime-helpers procedure-invocation-error-all-runtime-helpers
                          procedure-invocation-error-core-runtime-helper-names))

;; procedure-invocation-error-runtime-only-helpers : (listof core-runtime-helper?)
;;   Register invocation error helpers that still depend on monolithic runtime definitions.
(define procedure-invocation-error-runtime-only-helpers
  (remove-runtime-helpers procedure-invocation-error-all-runtime-helpers
                          procedure-invocation-error-core-runtime-helper-names))

;; procedure-application-error-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate procedure application error helpers used by the monolithic runtime.
(define procedure-application-error-runtime-only-helpers
  (function-forms->runtime-helpers
   'procedure-application-error-runtime-only-helpers
   '(
        (func $raise-application-not-procedure
              (param $v (ref eq))
              (local $out     (ref $GrowableArray))
              (local $message (ref $String))
              (local $caller-proc (ref eq))
              (local $caller-name (ref eq))
              (local $caller-debug-id (ref eq))
              (local $got-struct-type (ref eq))

              (if (i32.gt_u (global.get $debug-invoke-budget) (i32.const 0))
                  (then
                   (local.set $caller-proc (global.get $debug-current-procedure))
                   (local.set $caller-name (global.get $debug-current-procedure-name))
                   (local.set $caller-debug-id (global.get $false))
                   (local.set $got-struct-type (global.get $false))
                   (if (ref.test (ref $Procedure) (local.get $caller-proc))
                       (then
                        (local.set $caller-name
                                   (struct.get $Procedure $name
                                               (ref.cast (ref $Procedure)
                                                         (local.get $caller-proc))))))
                   (if (ref.test (ref $Closure) (local.get $caller-proc))
                       (then
                        (local.set $caller-debug-id
                                   (struct.get $Closure $debug-id
                                               (ref.cast (ref $Closure)
                                                         (local.get $caller-proc))))))
                   (if (ref.test (ref $Struct) (local.get $v))
                       (then
                        (local.set $got-struct-type
                                   (struct.get $StructType $name
                                               (struct.get $Struct $type
                                                           (ref.cast (ref $Struct)
                                                                     (local.get $v)))))))
                   (drop (call $js-log
                               (global.get $symbol:application-not-procedure-frontier)))
                   (drop (call $js-log
                               (call $cons
                                     (call $cons (global.get $symbol:got-struct-type)
                                           (call $cons (local.get $got-struct-type)
                                                 (global.get $null)))
                                     (global.get $null))))
                   (drop (call $js-log
                               (call $cons
                                     (call $cons (global.get $symbol:got-procedure?)
                                           (call $cons (call $procedure? (local.get $v))
                                                 (global.get $null)))
                                     (global.get $null))))
                   (drop (call $js-log
                               (call $cons
                                     (call $cons (global.get $symbol:caller-name)
                                           (call $cons (local.get $caller-name)
                                                 (global.get $null)))
                                     (global.get $null))))
                   (drop (call $js-log
                               (call $cons
                                     (call $cons (global.get $symbol:caller-debug-id)
                                           (call $cons (local.get $caller-debug-id)
                                                 (global.get $null)))
                                     (global.get $null))))))

              (local.set $out
                         (call $make-growable-array (i32.const 2)))
              (call $growable-array-add! (local.get $out)
                    (global.get $string:application:not-procedure))
              (call $growable-array-add! (local.get $out)
                    (call $format/display (local.get $v)))
              (local.set $message
                         (call $growable-array-of-strings->string (local.get $out)))

              (call $raise
                    (call $exn:fail:contract/make
                          (local.get $message)
                          (call $current-continuation-marks (global.get $missing)))
                    (global.get $true))
              (unreachable))

        (func $raise-apply-final-arg-not-list
              (param $v (ref eq))
              (local $out     (ref $GrowableArray))
              (local $message (ref $String))

              ;; TODO: include "argument position: last" and "other arguments..." block like Racket.
              (local.set $out
                         (call $make-growable-array (i32.const 5)))
              (call $growable-array-add! (local.get $out)
                    (call $format/display (global.get $symbol:apply)))
              (call $growable-array-add! (local.get $out)
                    (global.get $string:contract-violation:prefix))
              (call $growable-array-add! (local.get $out)
                    (global.get $string:list?))
              (call $growable-array-add! (local.get $out)
                    (global.get $string:arity-error:given))
              (call $growable-array-add! (local.get $out)
                    (call $format/print
                          (local.get $v)
                          (global.get $missing)))
              (local.set $message
                         (call $growable-array-of-strings->string (local.get $out)))

              (call $raise
                    (call $exn:fail:contract/make
                          (local.get $message)
                          (call $current-continuation-marks (global.get $missing)))
                    (global.get $true))
              (unreachable))

        (func $raise-struct-type-property-accessor-contract
              (param $prop (ref $StructTypeProperty))
              (param $v    (ref eq))
              (local $accessor-name     (ref $String))
              (local $prop-name-str     (ref $String))
              (local $expected-str      (ref $String))
              (local $out               (ref $GrowableArray))
              (local $message           (ref $String))

              (local.set $prop-name-str
                         (ref.cast (ref $String)
                                   (call $symbol->string
                                         (struct.get $StructTypeProperty $name (local.get $prop)))))
              (local.set $accessor-name
                         (call $string-append/2
                               (local.get $prop-name-str)
                               (global.get $string:accessor-suffix)))
              (local.set $expected-str
                         (call $string-append/2
                               (local.get $prop-name-str)
                               (global.get $string:question)))

              (local.set $out
                         (call $make-growable-array (i32.const 5)))
              (call $growable-array-add! (local.get $out) (local.get $accessor-name))
              (call $growable-array-add! (local.get $out) (global.get $string:contract-violation:prefix))
              (call $growable-array-add! (local.get $out) (local.get $expected-str))
              (call $growable-array-add! (local.get $out) (global.get $string:arity-error:given))
              (call $growable-array-add! (local.get $out) (call $format/display (local.get $v)))
              (local.set $message
                         (call $growable-array-of-strings->string (local.get $out)))

              (call $raise
                    (call $exn:fail:contract/make
                          (local.get $message)
                          (call $current-continuation-marks (global.get $missing)))
                    (global.get $true))
              (unreachable))
     )))

;; procedure-invoker-all-runtime-helpers : (listof core-runtime-helper?)
;;   Enumerate all procedure invoker helpers.
(define procedure-invoker-all-runtime-helpers
  (function-forms->runtime-helpers
   'procedure-invoker-all-runtime-helpers
   '(
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

               (global.set $debug-current-procedure (local.get $proc))
               (call $debug-invoke-tick)
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
               ;; Debug: log argument count and expected arity
               ; (drop (call $js-log (local.get $proc)))
               ;; Step 4: check arity match
               (if (i32.eqz
                    (call $procedure-arity-includes?/checked/i32
                          (local.get $clos)
                          (local.get $arg-count)))
                   (then
                    (call $raise-arity-mismatch/proc (local.get $clos) (local.get $arg-count))))
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

         (func $invoke-reduced-procedure
               (type $ProcedureInvoker)
               (param $proc (ref $Procedure))
               (param $args (ref $Args))
               (result      (ref eq))

               (local $wrapper  (ref $Closure))
               (local $original (ref $Procedure))
               (local $arg-count i32)

               (local.set $wrapper (ref.cast (ref $Closure) (local.get $proc)))
               (local.set $original
                          (ref.cast (ref $Procedure)
                                    (array.get $Free
                                               (struct.get $Closure $free (local.get $wrapper))
                                               (i32.const 0))))
               (local.set $arg-count (array.len (local.get $args)))
               (if (i32.eqz
                    (call $procedure-arity-includes?/checked/i32
                          (local.get $wrapper)
                          (local.get $arg-count)))
                   (then
                    (call $raise-arity-mismatch/proc (local.get $wrapper) (local.get $arg-count))))
               (return_call_ref $ProcedureInvoker
                                (local.get $original)
                                (local.get $args)
                                (struct.get $Procedure $invoke (local.get $original))))

         ;; invoke-composed-procedure : procedure? args-array? -> any
         ;;   Invoke a compose/compose1 wrapper from right to left.
         (func $invoke-composed-procedure
               (type $ProcedureInvoker)
               (param $proc (ref $Procedure))
               (param $args (ref $Args))
               (result      (ref eq))

               (local $wrapper (ref $Closure))
               (local $free    (ref $Free))
               (local $procs   (ref $Array))
               (local $single? (ref eq))
               (local $p       (ref $Procedure))
               (local $inv     (ref $ProcedureInvoker))
               (local $result  (ref eq))
               (local $vals    (ref $Values))
               (local $next    (ref $Args))
               (local $i       i32)
               (local $n       i32)
               (local $m       i32)

               (local.set $p      (global.get $dummy-closure))
               (local.set $inv    (ref.func $invoke-closure))
               (local.set $result (global.get $false))
               (local.set $vals   (array.new $Values (global.get $null) (i32.const 0)))
               (local.set $next   (array.new $Args (global.get $null) (i32.const 0)))
               (local.set $wrapper (ref.cast (ref $Closure) (local.get $proc)))
               (local.set $free    (struct.get $Closure $free (local.get $wrapper)))
               (local.set $procs   (ref.cast (ref $Array)
                                              (array.get $Free (local.get $free) (i32.const 0))))
               (local.set $single? (array.get $Free (local.get $free) (i32.const 1)))
               (local.set $n       (array.len (local.get $procs)))
               (if (i32.eqz (local.get $n))
                   (then (return_call $values (local.get $args))))

               (local.set $i (i32.sub (local.get $n) (i32.const 1)))
               (local.set $p (ref.cast (ref $Procedure)
                                        (array.get $Array (local.get $procs) (local.get $i))))
               (local.set $inv (struct.get $Procedure $invoke (local.get $p)))
               (local.set $result
                          (call_ref $ProcedureInvoker
                                    (local.get $p)
                                    (local.get $args)
                                    (local.get $inv)))

               (block $done
                      (loop $loop
                            (br_if $done (i32.eqz (local.get $i)))
                            (local.set $i (i32.sub (local.get $i) (i32.const 1)))
                            (if (ref.eq (local.get $single?) (global.get $true))
                                (then
                                 (local.set $result (call $expect-one-value (local.get $result)))
                                 (local.set $next (array.new_fixed $Args 1 (local.get $result))))
                                (else
                                 (if (ref.test (ref $Values) (local.get $result))
                                     (then
                                      (local.set $vals (ref.cast (ref $Values) (local.get $result)))
                                      (local.set $m    (array.len (local.get $vals)))
                                      (local.set $next (array.new $Args (global.get $null) (local.get $m)))
                                      (array.copy $Args $Values
                                                  (local.get $next) (i32.const 0)
                                                  (local.get $vals) (i32.const 0)
                                                  (local.get $m)))
                                     (else
                                      (local.set $next
                                                 (array.new_fixed $Args 1 (local.get $result)))))))
                            (local.set $p (ref.cast (ref $Procedure)
                                                    (array.get $Array
                                                               (local.get $procs)
                                                               (local.get $i))))
                            (local.set $inv (struct.get $Procedure $invoke (local.get $p)))
                            (local.set $result
                                       (call_ref $ProcedureInvoker
                                                 (local.get $p)
                                                 (local.get $next)
                                                 (local.get $inv)))
                            (br $loop)))
               (local.get $result))

         ;; Invoker for case-lambda closures.
         ;; - $args is the vector of *user* arguments (no [closure, tail?] header).
         ;; - Arity checking + repacking are handled by the closure's code
         ;;   ($code:case-lambda-dispatch), so we just tail-call it.
         ; Note: In principle we could use $invoke-closure,
         ;       but that leads to checking the arity multiple times.
         (func $invoke-case-closure (type $ProcedureInvoker)
               (param $proc (ref $Procedure))
               (param $args (ref $Args))
               (result      (ref eq))

               (local $clos (ref $Closure))
               (local $code (ref $ClosureCode))
               (global.set $debug-current-procedure (local.get $proc))
               (call $debug-invoke-tick)
               ;; Cast and fetch code pointer
               (local.set $clos (ref.cast (ref $Closure) (local.get $proc)))
               (local.set $code (struct.get $Closure $code (local.get $clos)))
               ;; Tail-call the dispatcher
               (return_call_ref $ClosureCode
                                (local.get $clos)
                                (local.get $args)
                                (local.get $code)))
     )))

;; procedure-invoker-core-runtime-helpers : (listof core-runtime-helper?)
;;   Register procedure invokers available in the standalone core runtime.
(define procedure-invoker-core-runtime-helpers
  (for/list ([helper (in-list
                      (select-runtime-helpers procedure-invoker-all-runtime-helpers
                                              procedure-invoker-core-runtime-helper-names))])
    (core-runtime-helper
     (core-runtime-helper-name helper)
     (core-runtime-helper-export-name helper)
     (lambda ()
       (invoke-closure-core-form (core-runtime-helper->form helper))))))

;; procedure-invoker-runtime-only-helpers : (listof core-runtime-helper?)
;;   Register procedure invokers that still depend on monolithic runtime definitions.
(define procedure-invoker-runtime-only-helpers
  (remove-runtime-helpers procedure-invoker-all-runtime-helpers
                          procedure-invoker-core-runtime-helper-names))

;; primitive-invocation-error-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate primitive invocation error helpers used by the monolithic runtime.
(define primitive-invocation-error-runtime-only-helpers
  (remove-runtime-helpers
   (function-forms->runtime-helpers
    'primitive-invocation-error-runtime-only-helpers
    '(
        (func $raise-no-code
              (param $pproc (ref $PrimitiveProcedure))
              (result (ref eq))
              (call $raise-argument-error1
                    (global.get $symbol:error)
                    (global.get $string:primitive-invoke:no-code)
                    (local.get $pproc))
              (unreachable))

        (func $raise-code-type-mismatch
              (param $pproc (ref $PrimitiveProcedure))
              (result (ref eq))
              (call $raise-argument-error1
                    (global.get $symbol:error)
                    (global.get $string:primitive-invoke:code-type-mismatch)
                    (local.get $pproc))
              (unreachable))

        (func $primitive-invoke:raise-arity-error
              (param $pproc (ref $PrimitiveProcedure))
              (param $argc  i32)
              (result       (ref eq))
              (local $name         (ref eq))
              (local $expected-str (ref $String))
              (local $received-str (ref $String))
              (local $out          (ref $GrowableArray))
              (local $message      (ref $String))

              (local.set $name
                         (struct.get $Procedure $name (local.get $pproc)))

              (local.set $expected-str
                         (call $procedure-arity->expected-string (local.get $pproc)))
              (local.set $received-str
                         (call $i32->string (local.get $argc)))

              (local.set $out
                         (call $make-growable-array (i32.const 5)))
              (call $growable-array-add! (local.get $out) (call $format/display (local.get $name)))
              (call $growable-array-add! (local.get $out) (global.get $string:arity-error:start))
              (call $growable-array-add! (local.get $out) (local.get $expected-str))
              (call $growable-array-add! (local.get $out) (global.get $string:arity-error:given))
              (call $growable-array-add! (local.get $out) (local.get $received-str))
              (local.set $message
                         (call $growable-array-of-strings->string (local.get $out)))

              (call $raise
                    (call $exn:fail:contract:arity/make
                          (local.get $message)
                          (call $current-continuation-marks (global.get $missing)))
                    (global.get $true))
              (unreachable))

        (func $raise-primitive-invoke:unhandled-shape
              (call $raise
                    (call $exn:fail:contract/make
                          (global.get $string:primitive-invoke:unhandled-shape)
                          (call $current-continuation-marks (global.get $missing)))
                    (global.get $true))
              (unreachable))
     ))
   procedure-primitive-error-core-runtime-helper-names))

;; primitive-dispatch-all-runtime-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for primitive dispatch helpers.
(define primitive-dispatch-all-runtime-helper-names
  '($primitive-invoke
    $raise-arity-error:exactly
    $raise-arity-error:at-least))

;; primitive-dispatch-core-runtime-helper-names : (listof symbol?)
;;   Name primitive dispatch helpers available in the standalone core runtime.
(define primitive-dispatch-core-runtime-helper-names
  '($primitive-invoke))

;; primitive-dispatch-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime-only primitive dispatch helper emission order.
(define primitive-dispatch-runtime-only-helper-names
  (remove* primitive-dispatch-core-runtime-helper-names
           primitive-dispatch-all-runtime-helper-names))

;; primitive-dispatch-all-runtime-helpers : (listof core-runtime-helper?)
;;   Enumerate all primitive dispatch helpers.
(define primitive-dispatch-all-runtime-helpers
  (function-forms->runtime-helpers
   'primitive-dispatch-all-runtime-helpers
   '(
        ;; Shape codes:
        ;;   0: exact 0
        ;;   1: exact 1
        ;;   2: exact 2
        ;;   3: exact 3
        ;;   4: exact 4
        ;;   5: exact 5
        ;;   6: at least 0
        ;;   7: at least 1
        ;;   8: at least 2
        ;;   9: at least >=3
        ;;  10-13: like 6-9 but pass rest arguments as $Args arrays
        ;;  14: between 2 and 3 arguments (3rd optional)
        ;;  15: between 3 and 4 arguments (4th optional)
        ;;  16: between 0 and 1 arguments
        ;;  17: between 0 and 2 arguments
        ;;  18: between 1 and 2 arguments
        ;;  19: between 1 and 3 arguments
        ;;  20: between 1 and 4 arguments
        ;;  21: between 1 and 5 arguments
        ;;  22: between 2 and 4 arguments
        ;;  23: between 2 and 5 arguments
        ;;  24: between 3 and 5 arguments
        ;;  25: between 6 and 7 arguments

        (func $primitive-invoke (type $ProcedureInvoker)
              (param $proc (ref $Procedure))
              (param $args (ref $Args))
              (result      (ref eq))

              (local $pproc (ref $PrimitiveProcedure))
              (local $code  (ref null func))

              (local $arity/tag i32)
              (local $arity     i32)
              (local $shape     i32)
              (local $k         i32)
              (local $argc      i32)

              (local $a0   (ref eq))
              (local $a1   (ref eq))
              (local $a2   (ref eq))
              (local $a3   (ref eq))
              (local $a4   (ref eq))
              (local $rest (ref eq))

              (global.set $debug-current-procedure (local.get $proc))
              (call $debug-invoke-tick)
              (local.set $a0 (global.get $null))
              (local.set $a1 (global.get $null))
              (local.set $a2 (global.get $null))
              ; (local.set $a3 (global.get $null))
              ; (local.set $a4 (global.get $null))
              (local.set $rest (global.get $null))

              ;; Proc -> PrimitiveProcedure
              (local.set $pproc
                         (ref.cast (ref $PrimitiveProcedure) (local.get $proc)))

              ;; Fetch code
              (local.set $code
                         (struct.get $PrimitiveProcedure $code (local.get $pproc)))

              ;; Ensure non-null
              (if (ref.is_null (local.get $code))
                  (then (return (call $raise-no-code (local.get $pproc)))))

              ;; argc
              (local.set $argc (array.len (local.get $args)))

              ;; Decode arity fixnum: i31 -> i32, then >> 1
              (local.set $arity/tag
                         (i31.get_s  ; signed due to negative arities
                          (ref.cast (ref i31)
                                    (struct.get $Procedure $arity (local.get $pproc)))))
              (local.set $arity
                         (i32.shr_s (local.get $arity/tag) (i32.const 1)))

              ;; Compute shape (and k for "at least k")
              (if (i32.ge_s (local.get $arity) (i32.const 0))
                  (then
                   ;; exact: shape = min(arity, 5), except for 6 -> 26
                   (local.set $shape (local.get $arity))
                   (if (i32.gt_u (local.get $shape) (i32.const 5))
                       (then (if (i32.eq (local.get $shape) (i32.const 6))
                                 (then (local.set $shape (i32.const 26)))
                                 (else (local.set $shape (i32.const 5)))))))
                  (else
                   ;; at least: k = -arity - 1; shape = 6 + min(k, 3)
                   (local.set $k
                              (i32.sub (i32.const -1) (local.get $arity)))
                   (local.set $shape (local.get $k))
                   (if (i32.gt_u (local.get $shape) (i32.const 3))
                       (then (local.set $shape (i32.const 3))))
                   (local.set $shape (i32.add (i32.const 6) (local.get $shape)))))

              ;; Debug: log argc, arity, shape
              ;; (drop (call $js-log (call $i32->string (local.get $argc))))
              ;; (drop (call $js-log (call $i32->string (local.get $arity))))
              ;; (drop (call $js-log (call $i32->string (local.get $shape))))

              ;; Preload first five args when available
              (if (i32.gt_u (local.get $argc) (i32.const 0))
                  (then
                   (local.set $a0
                              (array.get $Args (local.get $args) (i32.const 0)))))
              (if (i32.gt_u (local.get $argc) (i32.const 1))
                  (then
                   (local.set $a1
                              (array.get $Args (local.get $args) (i32.const 1)))))
              (if (i32.gt_u (local.get $argc) (i32.const 2))
                  (then
                   (local.set $a2
                              (array.get $Args (local.get $args) (i32.const 2)))))

              #;(if (i32.gt_u (local.get $argc) (i32.const 3))
                  (then
                   (local.set $a3
                              (array.get $Args (local.get $args) (i32.const 3)))))
              #;(if (i32.gt_u (local.get $argc) (i32.const 4))
                  (then
                   (local.set $a4
                              (array.get $Args (local.get $args) (i32.const 4)))))

              ;; br_table dispatch by shape
              (block $default
                (block $L13
                  (block $L12
                    (block $L11
                      (block $L10
                        (block $L9
                          (block $L8
                            (block $L7
                              (block $L6
                                (block $L5
                                  (block $L4
                                    (block $L3
                                      (block $L2
                                        (block $L1
                                          (block $L0
                                            (br_table $L0 $L1 $L2 $L3 $L4 $L5 $L6 $L7 $L8 $L9 $L10 $L11 $L12 $L13 $default (local.get $shape))
                                          ) ;; end $L0
                                          ;; shape 0: exact 0
                                          #;(drop (call $js-log (call $i32->string (i32.const 0))))
                                          (if (i32.eqz (local.get $argc))
                                              (then
                                               (if (ref.test (ref $Prim0) (local.get $code))
                                                   (then
                                                    (return_call_ref $Prim0
                                                                     (ref.cast (ref $Prim0) (local.get $code))))
                                                   (else
                                                    (return (call $raise-code-type-mismatch (local.get $pproc)))))
                                              (else
                                               (return (call $primitive-invoke:raise-arity-error
                                                                 (local.get $pproc) (local.get $argc)))))
                                        )) ;; end $L1
                                        ;; shape 1: exact 1
                                        #;(drop (call $js-log (call $i32->string (i32.const 1))))
                                        (if (i32.eq (local.get $argc) (i32.const 1))
                                            (then
                                             (if (ref.test (ref $Prim1) (local.get $code))
                                                 (then
                                                  (return_call_ref $Prim1
                                                                   (local.get $a0)
                                                                   (ref.cast (ref $Prim1) (local.get $code))))
                                                 (else
                                                  (return (call $raise-code-type-mismatch (local.get $pproc)))))
                                            (else
                                             (return (call $primitive-invoke:raise-arity-error
                                                           (local.get $pproc) (local.get $argc)))))
                                      )) ;; end $L2
                                      ;; shape 2: exact 2
                                      #;(drop (call $js-log (call $i32->string (i32.const 2))))
                                      (if (i32.eq (local.get $argc) (i32.const 2))
                                          (then
                                           (if (ref.test (ref $Prim2) (local.get $code))
                                               (then
                                                (return_call_ref $Prim2
                                                                 (local.get $a0)
                                                                 (local.get $a1)
                                                                 (ref.cast (ref $Prim2) (local.get $code))))
                                               (else
                                                (return (call $raise-code-type-mismatch (local.get $pproc)))))
                                          (else
                                           (return (call $primitive-invoke:raise-arity-error
                                                         (local.get $pproc) (local.get $argc)))))
                                    )) ;; end $L3
                                    ;; shape 3: exact 3
                                    #;(drop (call $js-log (call $i32->string (i32.const 3))))
                                    (if (i32.eq (local.get $argc) (i32.const 3))
                                        (then
                                         (if (ref.test (ref $Prim3) (local.get $code))
                                             (then
                                              (return_call_ref $Prim3
                                                               (local.get $a0)
                                                               (local.get $a1)
                                                               (local.get $a2)
                                                               (ref.cast (ref $Prim3) (local.get $code))))
                                             (else
                                              (return (call $raise-code-type-mismatch (local.get $pproc)))))
                                        (else
                                         (return (call $primitive-invoke:raise-arity-error
                                                       (local.get $pproc) (local.get $argc)))))
                                  )) ;; end $L4
                                  ;; shape 4: exact 4
                                  #;(drop (call $js-log (call $i32->string (i32.const 4))))
                                  (if (i32.eq (local.get $argc) (i32.const 4))
                                      (then (if (ref.test (ref $Prim4) (local.get $code))
                                                (then (return_call_ref $Prim4
                                                                       (local.get $a0)
                                                                       (local.get $a1)
                                                                       (local.get $a2)
                                                                       (array.get $Args (local.get $args) (i32.const 3))
                                                                       (ref.cast (ref $Prim4) (local.get $code))))
                                                (else
                                                 (return (call $raise-code-type-mismatch (local.get $pproc))))))
                                      (else (return (call $primitive-invoke:raise-arity-error
                                                          (local.get $pproc) (local.get $argc))))
                                )) ;; end $L5
                                ;; shape 5: exact 5
                                #;(drop (call $js-log (call $i32->string (i32.const 5))))
                                (if (i32.eq (local.get $argc) (i32.const 5))
                                    (then (if (ref.test (ref $Prim5) (local.get $code))
                                              (then (return_call_ref $Prim5
                                                                     (local.get $a0)
                                                                     (local.get $a1)
                                                                     (local.get $a2)
                                                                     (array.get $Args (local.get $args) (i32.const 3))
                                                                     (array.get $Args (local.get $args) (i32.const 4))
                                                                     (ref.cast (ref $Prim5) (local.get $code))))
                                              (else
                                               (return (call $raise-code-type-mismatch (local.get $pproc))))))
                                    (else (return (call $primitive-invoke:raise-arity-error
                                                        (local.get $pproc) (local.get $argc))))
                              )) ;; end $L6
                              ;; shape 6: at least 0 (rest list)
                              (if (ref.test (ref $Prim>=0) (local.get $code))
                                  (then (local.set $rest
                                                   (call $rest-arguments->list
                                                         (local.get $args)
                                                         (i32.const 0)))
                                        (return_call_ref $Prim>=0
                                                         (local.get $rest)
                                                         (ref.cast (ref $Prim>=0) (local.get $code))))
                                  (else
                                   (return (call $raise-code-type-mismatch (local.get $pproc))))
                            )) ;; end $L7
                            ;; shape 7: at least 1 (rest list)
                            (if (i32.ge_u (local.get $argc) (i32.const 1))
                                (then (if (ref.test (ref $Prim>=1) (local.get $code))
                                          (then (local.set $rest
                                                           (call $rest-arguments->list
                                                                 (local.get $args)
                                                                 (i32.const 1)))
                                                (return_call_ref $Prim>=1
                                                                 (local.get $a0)
                                                                 (local.get $rest)
                                                                 (ref.cast (ref $Prim>=1) (local.get $code))))
                                          (else (return (call $raise-code-type-mismatch (local.get $pproc))))))
                                (else (return (call $primitive-invoke:raise-arity-error
                                                    (local.get $pproc) (local.get $argc))))
                          )) ;; end $L8
                          ;; shape 8: at least 2 (rest list)
                          (if (i32.ge_u (local.get $argc) (i32.const 2))
                              (then (if (ref.test (ref $Prim>=2) (local.get $code))
                                        (then (local.set $rest
                                                         (call $rest-arguments->list
                                                               (local.get $args)
                                                               (i32.const 2)))
                                              (return_call_ref $Prim>=2
                                                               (local.get $a0)
                                                               (local.get $a1)
                                                               (local.get $rest)
                                                               (ref.cast (ref $Prim>=2) (local.get $code))))
                                        (else (return (call $raise-code-type-mismatch (local.get $pproc))))))
                              (else (return (call $primitive-invoke:raise-arity-error
                                                  (local.get $pproc) (local.get $argc))))
                        )) ;; end $L9
                        ;; shape 9: at least 3 (rest list)
                        (if (i32.ge_u (local.get $argc) (i32.const 3))
                            (then (if (ref.test (ref $Prim>=3) (local.get $code))
                                      (then (local.set $rest
                                                       (call $rest-arguments->list
                                                             (local.get $args)
                                                             (i32.const 3)))
                                            (return_call_ref $Prim>=3
                                                             (local.get $a0)
                                                             (local.get $a1)
                                                             (local.get $a2)
                                                             (local.get $rest)
                                                             (ref.cast (ref $Prim>=3) (local.get $code))))
                                      (else (return (call $raise-code-type-mismatch (local.get $pproc))))))
                            (else (return (call $primitive-invoke:raise-arity-error
                                                    (local.get $pproc) (local.get $argc))))
                      )) ;; end $L10
                      ;; shape 10: at least 0 (rest $Args)
                       (if (ref.test (ref $Prim>=0) (local.get $code))
                           (then (return_call_ref $Prim>=0
                                                  (local.get $args)
                                                  (ref.cast (ref $Prim>=0) (local.get $code))))
                           (else (return (call $raise-code-type-mismatch (local.get $pproc))))
                    )) ;; end $L11
                    ;; shape 11: at least 1 (rest $Args)
                    (if (i32.ge_u (local.get $argc) (i32.const 1))
                        (then
                         (if (ref.test (ref $Prim>=1) (local.get $code))
                             (then (return_call_ref $Prim>=1
                                                    (local.get $a0)
                                                    (call $rest-arguments->args
                                                          (local.get $args)
                                                          (i32.const 1))
                                                    (ref.cast (ref $Prim>=1) (local.get $code))))
                             (else (return (call $raise-code-type-mismatch (local.get $pproc))))))
                        (else
                         (return (call $primitive-invoke:raise-arity-error
                                       (local.get $pproc) (local.get $argc))))
                  )) ;; end $L12
                  ;; shape 12: at least 2 (rest $Args)
                  (if (i32.ge_u (local.get $argc) (i32.const 2))
                      (then
                       (if (ref.test (ref $Prim>=2) (local.get $code))
                           (then (return_call_ref $Prim>=2
                                                  (local.get $a0)
                                                  (local.get $a1)
                                                  (call $rest-arguments->args
                                                        (local.get $args)
                                                        (i32.const 2))
                                                  (ref.cast (ref $Prim>=2) (local.get $code))))
                           (else (return (call $raise-code-type-mismatch (local.get $pproc)))))
                       (else
                       (return (call $primitive-invoke:raise-arity-error
                                     (local.get $pproc) (local.get $argc)))))
                )) ;; end $L13
                ;; shape 13: at least 3 (rest $Args)
                (if (i32.ge_u (local.get $argc) (i32.const 3))
                    (then
                     (if (ref.test (ref $Prim>=3) (local.get $code))
                         (then (return_call_ref $Prim>=3
                                                (local.get $a0)
                                                (local.get $a1)
                                                (local.get $a2)
                                                (call $rest-arguments->args
                                                      (local.get $args)
                                                      (i32.const 3))
                                                (ref.cast (ref $Prim>=3) (local.get $code))))
                         (else (return (call $raise-code-type-mismatch (local.get $pproc)))))
                     (else
                      (return (call $primitive-invoke:raise-arity-error
                                    (local.get $pproc) (local.get $argc)))))
              )) ;; end $default
              (if (i32.eq (local.get $shape) (i32.const 26))
                  (then (if (i32.eq (local.get $argc) (i32.const 6))
                            (then (if (ref.test (ref $Prim6) (local.get $code))
                                      (then (return_call_ref $Prim6
                                                             (local.get $a0)
                                                             (local.get $a1)
                                                             (local.get $a2)
                                                             (array.get $Args (local.get $args) (i32.const 3))
                                                             (array.get $Args (local.get $args) (i32.const 4))
                                                             (array.get $Args (local.get $args) (i32.const 5))
                                                             (ref.cast (ref $Prim6) (local.get $code))))
                                      (else (return (call $raise-code-type-mismatch (local.get $pproc))))))
                            (else (return (call $primitive-invoke:raise-arity-error
                                                (local.get $pproc) (local.get $argc)))))
                        (unreachable))
                  (else (nop)))
              #;(drop (call $js-log (call $i32->string (i32.const 10))))
              (call $raise-primitive-invoke:unhandled-shape)
              (unreachable))

        ;; Unused stubs kept for now; safe to remove later.
        (func $raise-arity-error:exactly
              (call $raise
                    (call $exn:fail:contract:arity/make
                          (global.get $string:arity-error:generic)
                          (call $current-continuation-marks (global.get $missing)))
                    (global.get $true))
              (unreachable))

        (func $raise-arity-error:at-least
              (call $raise
                    (call $exn:fail:contract:arity/make
                          (global.get $string:arity-error:generic)
                          (call $current-continuation-marks (global.get $missing)))
                    (global.get $true))
              (unreachable))
     )))

;; primitive-dispatch-core-runtime-helpers : (listof core-runtime-helper?)
;;   Register primitive dispatch helpers available in the standalone core runtime.
(define primitive-dispatch-core-runtime-helpers
  (for/list ([helper (in-list
                      (select-runtime-helpers primitive-dispatch-all-runtime-helpers
                                              primitive-dispatch-core-runtime-helper-names))])
    (core-runtime-helper
     (core-runtime-helper-name helper)
     (core-runtime-helper-export-name helper)
     (lambda ()
       (primitive-invoke-core-form (core-runtime-helper->form helper))))))

;; primitive-dispatch-runtime-only-helpers : (listof core-runtime-helper?)
;;   Register primitive dispatch helpers that still depend on monolithic runtime definitions.
(define primitive-dispatch-runtime-only-helpers
  (remove-runtime-helpers primitive-dispatch-all-runtime-helpers
                          primitive-dispatch-core-runtime-helper-names))

;; procedure-case-lambda-all-runtime-helpers : (listof core-runtime-helper?)
;;   Enumerate all procedure case-lambda helpers.
(define procedure-case-lambda-all-runtime-helpers
  (function-forms->runtime-helpers
   'procedure-case-lambda-all-runtime-helpers
   '(
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

         ;; Dispatcher for (case-lambda ...) using arities:
         ;;   m >= 0  ⇒ exactly m args
         ;;   m <  0  ⇒ at least (-m - 1) args
         ;;
         ;; $Free payload captured by the dispatcher closure:
         ;;   index 0 : (ref $I32Array)  ; arities per arm
         ;;   index 1 : (ref $Array)     ; arm closures (source order)
         (func $raise-arity-error/case-lambda/arities
               (param $proc    (ref $Procedure))
               (param $argc    i32)
               (param $arities (ref $I32Array))
               (result (ref eq))

               (local $name         (ref eq))
               (local $expected-str (ref $String))
               (local $received-str (ref $String))
               (local $i            i32)
               (local $n            i32)
               (local $m            i32)
               (local $ga           (ref $GrowableArray))
               (local $out          (ref $GrowableArray))
               (local $message      (ref $String))

               (local.set $name
                          (struct.get $Procedure $name (local.get $proc)))
               (local.set $received-str
                          (call $i32->string (local.get $argc)))

               (local.set $n (array.len (local.get $arities)))
               (local.set $ga (call $make-growable-array (local.get $n)))
               (local.set $i (i32.const 0))
               (block $done
                      (loop $loop
                            (br_if $done (i32.ge_u (local.get $i) (local.get $n)))
                            (local.set $m (array.get $I32Array (local.get $arities) (local.get $i)))
                            (call $growable-array-add! (local.get $ga)
                                  (call $arity-i32->string (local.get $m)))
                            (if (i32.lt_u (i32.add (local.get $i) (i32.const 1)) (local.get $n))
                                (then (call $growable-array-add! (local.get $ga)
                                              (global.get $string:arity-error:or))))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $loop)))

               (local.set $expected-str
                          (call $growable-array-of-strings->string (local.get $ga)))

               (local.set $out
                          (call $make-growable-array (i32.const 5)))
               (call $growable-array-add! (local.get $out) (call $format/display (local.get $name)))
               (call $growable-array-add! (local.get $out) (global.get $string:arity-error:start))
               (call $growable-array-add! (local.get $out) (local.get $expected-str))
               (call $growable-array-add! (local.get $out) (global.get $string:arity-error:given))
               (call $growable-array-add! (local.get $out) (local.get $received-str))
               (local.set $message
                          (call $growable-array-of-strings->string (local.get $out)))

               (call $raise
                     (call $exn:fail:contract:arity/make
                           (local.get $message)
                           (call $current-continuation-marks (global.get $missing)))
                     (global.get $true))
               (unreachable))
         ;; Dispatcher uses typed $CaseClosure fields: $arities and $arms.
         (func $code:case-lambda-dispatch (type $ClosureCode)
               (param $clos (ref $Closure))
               (param $args (ref $Args))
               (result      (ref eq))

               (local $cclos    (ref $CaseClosure))
               (local $arities  (ref $I32Array))
               (local $arms     (ref $Array))
               (local $argc     i32)
               (local $i        i32)
               (local $m        i32)
               (local $arm      (ref $Closure))
               (local $out      (ref $Args))
               (local $code     (ref $ClosureCode))

               ; Get arities and arms (closures)
               (local.set $cclos   (ref.cast (ref $CaseClosure) (local.get $clos)))
               (local.set $arities (struct.get $CaseClosure $arities (local.get $cclos)))
               (local.set $arms    (struct.get $CaseClosure $arms    (local.get $cclos)))
               ; Argument count
               (local.set $argc (array.len (local.get $args)))
               (local.set $i (i32.const 0))

               (loop $scan
                     (if (i32.ge_u (local.get $i) (array.len (local.get $arms)))
                         (then (drop (call $js-log (call $format/display (local.get $clos))))
                               (call $raise-arity-error/case-lambda/arities
                                     (local.get $clos) (local.get $argc) (local.get $arities))
                               (unreachable)))
                     (local.set $m
                                (array.get $I32Array (local.get $arities) (local.get $i)))
                     (if
                      (i32.or
                       ;; fixed: argc == m
                       (i32.and (i32.ge_s (local.get $m) (i32.const 0))
                                (i32.eq   (local.get $argc) (local.get $m)))
                       ;; rest:  argc >= -m - 1
                       (i32.and (i32.lt_s (local.get $m) (i32.const 0))
                                (i32.ge_u (local.get $argc)
                                          (i32.sub (i32.sub (i32.const 0) (local.get $m))
                                                   (i32.const 1)))))
                      (then
                       ;; Match → repack once and tail-call arm's *code*
                       (local.set $arm  (ref.cast (ref $Closure)
                                                  (array.get $Array (local.get $arms) (local.get $i))))
                       (local.set $out  (call $repack-arguments (local.get $args) (local.get $m)))
                       (local.set $code (struct.get $Closure $code (local.get $arm)))
                       (return_call_ref $ClosureCode (local.get $arm) (local.get $out) (local.get $code)))
                      (else
                       (local.set $i (i32.add (local.get $i) (i32.const 1)))
                       (br $scan))))
               (unreachable))
     )))

;; procedure-case-lambda-core-runtime-helpers : (listof core-runtime-helper?)
;;   Register case-lambda helpers available in the standalone core runtime.
(define procedure-case-lambda-core-runtime-helpers
  (select-runtime-helpers procedure-case-lambda-all-runtime-helpers
                          procedure-case-lambda-core-runtime-helper-names))

;; procedure-case-lambda-runtime-only-helpers : (listof core-runtime-helper?)
;;   Register case-lambda helpers that still depend on monolithic runtime definitions.
(define procedure-case-lambda-runtime-only-helpers
  (remove-runtime-helpers procedure-case-lambda-all-runtime-helpers
                          procedure-case-lambda-core-runtime-helper-names))

;; procedure-basic-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate basic procedure helpers used by the monolithic runtime.
(define procedure-basic-runtime-only-helpers
  (function-forms->runtime-helpers
   'procedure-basic-runtime-only-helpers
   '(
     ;; `identity` is useful as a first-class function in module-language
     ;; programs, so it needs a runtime procedure in addition to inlining.
     (func $identity (type $Prim1)
           (param $v (ref eq))
           (result   (ref eq))
           (local.get $v))

     ;; procedure? : any/c -> boolean?
     ;;   Recognize procedures semantically; non-applicable structs are not procedures.
     (func $procedure? (type $Prim1)
           (param $v (ref eq)) ;; candidate value
           (result   (ref eq))
           (if (ref.test (ref $Struct) (local.get $v))
               (then (return (if (result (ref eq))
                                 (call $applicable-struct?/i32 (local.get $v))
                                 (then (global.get $true))
                                 (else (global.get $false))))))
           (if (result (ref eq))
               (ref.test (ref $Procedure) (local.get $v))
               (then (global.get $true))
               (else (global.get $false))))

     ;; procedure-specialize : procedure? -> procedure?
     ;;   Validate and return the procedure. Racket uses this as an
     ;;   optimization hint; WebRacket currently has no procedure
     ;;   specializer to invoke.
     (func $procedure-specialize (type $Prim1)
           (param $proc (ref eq)) ;; procedure?
           (result      (ref eq))

           (if (ref.eq (call $procedure? (local.get $proc)) (global.get $false))
               (then (call $raise-argument-error:procedure-expected/who
                           (global.get $symbol:procedure-specialize)
                           (local.get $proc))
                     (unreachable)))
           (local.get $proc))

     ; Notes: repacking of arguments are done in $invoke-closure,
     ;        so no repacking is needed in $apply.

     (func $apply (type $Prim>=1)
           (param $proc      (ref eq)) ;; procedure to apply
           (param $args-list (ref eq)) ;; list of arguments (direct args + final list)
           (result           (ref eq))

           (local $p             (ref $Procedure))
           (local $args-array    (ref $Array))
           (local $final-array   (ref $Array))
           (local $result-array  (ref $Array))
           (local $final-list    (ref eq))
           (local $total-count   i32)
           (local $direct-count  i32)
           (local $final-count   i32)
           (local $i             i32)
           (local $val           (ref eq))

           ;; Initialize non-defaultable locals.
           (local.set $args-array   (call $make-array (i32.const 0) (global.get $null)))
           (local.set $final-array  (call $make-array (i32.const 0) (global.get $null)))
           (local.set $result-array (call $make-array (i32.const 0) (global.get $null)))
           (local.set $final-list   (global.get $null))
           (local.set $total-count  (i32.const 0))
           (local.set $direct-count (i32.const 0))
           (local.set $final-count  (i32.const 0))
           (local.set $i            (i32.const 0))
           (local.set $val          (global.get $null))

           ;; Step 1: ensure $proc is a semantic procedure. Structs are
           ;; represented under $Procedure for fast application, but only
           ;; structs with prop:procedure are Racket procedures.
           (if (ref.eq (call $procedure? (local.get $proc)) (global.get $false))
               (then (call $raise-application-not-procedure (local.get $proc))
                     (unreachable)))
           (local.set $p (ref.cast (ref $Procedure) (local.get $proc)))

           ;; Step 2: convert arguments list into arrays for indexed access
           (if (ref.eq (call $list? (local.get $args-list)) (global.get $false))
               (then (call $raise-apply-final-arg-not-list (local.get $args-list))
                     (unreachable)))
           (local.set $args-array  (call $list->array (local.get $args-list)))
           (local.set $total-count (array.len (local.get $args-array)))
           ;; The final element of $args-array is the list of trailing arguments.
           (local.set $final-list
                      (array.get $Array
                                 (local.get $args-array)
                                 (i32.sub (local.get $total-count) (i32.const 1))))
           (if (ref.eq (call $list? (local.get $final-list)) (global.get $false))
               (then (call $raise-apply-final-arg-not-list (local.get $final-list))
                     (unreachable)))
           (local.set $final-array (call $list->array (local.get $final-list)))
           (local.set $final-count (array.len (local.get $final-array)))
           (local.set $direct-count (i32.sub (local.get $total-count) (i32.const 1)))

           ;; Step 3: allocate the combined argument array
           (local.set $result-array
                      (call $make-array
                            (i32.add (local.get $direct-count) (local.get $final-count))
                            (global.get $null)))


           ;; Step 4: copy direct arguments
           (local.set $i (i32.const 0))
           (block $direct-done
                  (loop $direct
                        (if (i32.ge_u (local.get $i) (local.get $direct-count))
                            (then (br $direct-done)))
                        (local.set $val
                                   (array.get $Array (local.get $args-array)
                                              (local.get $i)))
                        (call $array-set!
                              (local.get $result-array)
                              (local.get $i)
                              (local.get $val))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br $direct)))

           ;; Step 5: append values from the final list
           (local.set $i (i32.const 0))
           (block $append-done
                  (loop $append
                        (if (i32.ge_u (local.get $i) (local.get $final-count))
                            (then (br $append-done)))
                        (local.set $val
                                   (array.get $Array (local.get $final-array)
                                              (local.get $i)))
                        (call $array-set!
                              (local.get $result-array)
                              (i32.add (local.get $i) (local.get $direct-count))
                              (local.get $val))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br $append)))

           ;; Step 6: apply via the procedure's invoke field
           (return_call_ref $ProcedureInvoker
                            (local.get $p)
                            (local.get $result-array)
                            (struct.get $Procedure $invoke (local.get $p))))

     ;; procedure->method : procedure? -> procedure?
     ;;   Racket uses method procedures to adjust arity errors by hiding
     ;;   the implicit receiver argument. WebRacket does not model that
     ;;   method-wrapper distinction yet, so preserve the callable while
     ;;   still enforcing the documented procedure? contract.
     (func $procedure->method (type $Prim1)
           (param $proc (ref eq)) ; procedure?
           (result      (ref eq))

           (if (ref.eq (call $procedure? (local.get $proc)) (global.get $false))
               (then (call $raise-argument-error1
                           (global.get $symbol:procedure->method)
                           (global.get $string:procedure?)
                           (local.get $proc))
                     (unreachable)))
           (local.get $proc))

     (func $procedure-rename (type $Prim23)
           (param $proc  (ref eq))   ;; any procedure
           (param $name  (ref eq))   ;; symbol
           (param $realm (ref eq))   ;; symbol or $missing
           (result (ref eq))

           (local $arity      (ref eq))
           (local $invoke     (ref $ProcedureInvoker))
           (local $realm*     (ref eq))
           (local $case-proc  (ref $CaseClosure))

           ;; Step 1: If realm is #f, replace with 'racket
           (local.set $realm*
                      (if (result (ref eq))
                          (ref.eq (local.get $realm) (global.get $missing))
                          (then (global.get $symbol:racket))
                          (else (local.get $realm))))
           ;; Step 2: If $proc is a CaseClosure, preserve its arms and arity table
           (if (ref.test (ref $CaseClosure) (local.get $proc))
               (then
                (local.set $case-proc (ref.cast (ref $CaseClosure) (local.get $proc)))
                 (return
                 (struct.new $CaseClosure
                             (i32.const 0)                                 ;; hash
                             (local.get $name)
                             (struct.get $CaseClosure $arity   (local.get $case-proc))
                             (local.get $realm*)
                             (struct.get $CaseClosure $invoke  (local.get $case-proc))
                             (struct.get $CaseClosure $debug-id (local.get $case-proc))
                             (struct.get $CaseClosure $code    (local.get $case-proc))
                             (struct.get $CaseClosure $free    (local.get $case-proc))
                             (struct.get $CaseClosure $arities (local.get $case-proc))
                             (struct.get $CaseClosure $arms    (local.get $case-proc))))))
           ;; Step 3: If $proc is a Closure
           (if (ref.test (ref $Closure) (local.get $proc))
               (then
                (return
                (struct.new $Closure
                            (i32.const 0)                                 ;; hash
                            (local.get $name)
                            (struct.get $Closure $arity  (ref.cast (ref $Closure) (local.get $proc)))
                            (local.get $realm*)
                            (struct.get $Closure $invoke (ref.cast (ref $Closure) (local.get $proc)))
                            (struct.get $Closure $debug-id (ref.cast (ref $Closure) (local.get $proc)))
                            (struct.get $Closure $code   (ref.cast (ref $Closure) (local.get $proc)))
                            (struct.get $Closure $free   (ref.cast (ref $Closure) (local.get $proc)))))))
           ;; Step 4: If $proc is a PrimitiveClosure
           (if (ref.test (ref $PrimitiveClosure) (local.get $proc))
               (then
                (return
                (struct.new $PrimitiveClosure
                            (i32.const 0)
                            (local.get $name)
                            (struct.get $PrimitiveClosure $arity  (ref.cast (ref $PrimitiveClosure) (local.get $proc)))
                            (local.get $realm*)
                            (struct.get $PrimitiveClosure $invoke (ref.cast (ref $PrimitiveClosure) (local.get $proc)))
                            (struct.get $PrimitiveClosure $code (ref.cast (ref $PrimitiveClosure) (local.get $proc)))
                            (struct.get $PrimitiveClosure $result-arity
                                        (ref.cast (ref $PrimitiveClosure) (local.get $proc)))))))
           ;; Step 5: If $proc is a PrimitiveProcedure
           (if (ref.test (ref $PrimitiveProcedure) (local.get $proc))
               (then
                (return
                (struct.new $PrimitiveProcedure
                            (i32.const 0)
                            (local.get $name)
                            (struct.get $PrimitiveProcedure $arity  (ref.cast (ref $PrimitiveProcedure) (local.get $proc)))
                            (local.get $realm*)
                            (struct.get $PrimitiveProcedure $invoke (ref.cast (ref $PrimitiveProcedure) (local.get $proc)))
                            (struct.get $PrimitiveProcedure $code   (ref.cast (ref $PrimitiveProcedure) (local.get $proc)))
                            (struct.get $PrimitiveProcedure $result-arity
                                        (ref.cast (ref $PrimitiveProcedure) (local.get $proc)))))))
           ;; Step 6: Not a supported procedure type
           (call $raise-argument-error:procedure-expected (local.get $proc))
           (unreachable))

     ;; procedure-realm : procedure? -> symbol?
     ;;   Return a procedure's realm, defaulting to 'racket when WebRacket
     ;;   represents an applicable value without richer realm metadata.
     (func $procedure-realm (type $Prim1)
           (param $proc (ref eq)) ; procedure?
           (result      (ref eq))

           (local $p     (ref $Procedure))
           (local $realm (ref eq))

           (if (ref.eq (call $procedure? (local.get $proc)) (global.get $false))
               (then (call $raise-argument-error1
                           (global.get $symbol:procedure-realm)
                           (global.get $string:procedure?)
                           (local.get $proc))
                     (unreachable)))

           (local.set $p (ref.cast (ref $Procedure) (local.get $proc)))
           (local.set $realm (struct.get $Procedure $realm (local.get $p)))
           (if (result (ref eq))
               (ref.eq (local.get $realm) (global.get $false))
               (then (global.get $symbol:racket))
               (else (local.get $realm))))


     (func $raise-argument-error:procedure-expected/who
           (param $who (ref eq))
           (param $got (ref eq))

           (call $raise-argument-error1
                 (local.get $who)
                 (global.get $string:procedure?)
                 (local.get $got)))

     (func $raise-argument-error:procedure-expected
           (param $got (ref eq))

           (call $raise-argument-error:procedure-expected/who
                 (global.get $symbol:procedure)
                 (local.get $got)))
     )))

;; procedure-arity-and-reflection-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate procedure arity and reflection helpers used by the monolithic runtime.
(define procedure-arity-and-reflection-runtime-only-helpers
  (function-forms->runtime-helpers
   'procedure-arity-and-reflection-runtime-only-helpers
   '(
     ;; Support for the arity-at-least structure used by procedure-arity.
     (func $ensure-arity-at-least-type
           (result (ref $StructType))

           (local $existing (ref null $StructType))
           (local $std      (ref $StructType))
           (local $indices  (ref eq))

           (local.set $existing (global.get $arity-at-least-type))
           (if (ref.is_null (local.get $existing))
               (then
                (local.set $indices (call $list-from-range/checked (i32.const 0) (i32.const 1)))
                (local.set $std
                           (struct.new $StructType
                                       (i32.const 0)
                                       (ref.cast (ref $Symbol) (global.get $symbol:arity-at-least))
                                       (global.get $false)
                                       (i32.const 1)
                                       (local.get $indices)
                                       (global.get $null)
                                       (global.get $null)
                                       (ref.cast (ref eq) (call $struct-type-property-table-empty))
                                       (global.get $false)
                                       (local.get $indices)
                                       (global.get $false)
                                       (ref.cast (ref $Symbol) (global.get $symbol:arity-at-least))))
                (global.set $arity-at-least-type (local.get $std))
                (local.set $existing (local.get $std))))
           (ref.as_non_null (local.get $existing)))

     (func $arity-at-least/make
           (param $value i32)   ;; exact non-negative integer
           (result (ref eq))

           (call $arity-at-least/make/value
                 (ref.i31 (i32.shl (local.get $value) (i32.const 1)))))

     (func $arity-at-least/make/value
           (param $value (ref eq)) ;; exact-nonnegative-integer?
           (result (ref eq))

           (local $std    (ref $StructType))
           (local $fields (ref $Array))

           (local.set $std (call $ensure-arity-at-least-type))
           (local.set $fields
                      (array.new_fixed $Array 1
                                       (local.get $value)))
           (ref.cast (ref eq)
                     (struct.new $Struct
                                 (i32.const 0)
                                 (global.get $false)
                                 (ref.i31 (i32.const 0))
                                 (global.get $false)
                                 (ref.func $invoke-struct)
                                 (local.get $std)
                                 (local.get $fields))))

     (func $make-arity-at-least (type $Prim1)
           (param $value (ref eq)) ; exact-nonnegative-integer?
           (result (ref eq))

           (if (ref.eq (call $exact-nonnegative-integer? (local.get $value)) (global.get $false))
               (then (call $raise-argument-error1
                           (global.get $symbol:make-arity-at-least)
                           (global.get $string:exact-nonnegative-integer?)
                           (local.get $value))
                     (unreachable)))
           (call $arity-at-least/make/value
                 (local.get $value)))

     (func $arity-at-least (type $Prim1)
           (param $value (ref eq)) ; exact-nonnegative-integer?
           (result (ref eq))

           (return_call $make-arity-at-least (local.get $value)))

     (func $raise-argument-error:arity-at-least-expected
           (param $who (ref eq))
           (param $got (ref eq))

           (call $raise-argument-error1
                 (local.get $who)
                 (global.get $string:arity-at-least?)
                 (local.get $got)))

     (func $arity-at-least-unwrap
           (param $who (ref eq))
           (param $v   (ref eq))
           (result (ref $Struct))

           (local $struct (ref $Struct))
           (local $type   (ref eq))
           (local $std    (ref $StructType))
           (local $ok     i32)

           (local.set $std (call $ensure-arity-at-least-type))
           (if (i32.eqz (ref.test (ref $Struct) (local.get $v)))
               (then (call $raise-argument-error:arity-at-least-expected (local.get $who) (local.get $v))
                     (unreachable)))
           (local.set $struct (ref.cast (ref $Struct) (local.get $v)))
           (local.set $type   (struct.get $Struct $type (local.get $struct)))
           (local.set $ok     (call $struct-type-is-a?/i32 (local.get $type) (local.get $std)))
           (if (i32.eqz (local.get $ok))
               (then (call $raise-argument-error:arity-at-least-expected (local.get $who) (local.get $v))
                     (unreachable)))
           (local.get $struct))

     (func $arity-at-least? (type $Prim1)
           (param $v (ref eq))
           (result   (ref eq))

           (local $struct (ref $Struct))
           (local $type   (ref eq))
           (local $std    (ref $StructType))
           (local $ok     i32)

           (local.set $std (call $ensure-arity-at-least-type))
           (if (result (ref eq))
               (ref.test (ref $Struct) (local.get $v))
               (then
                (local.set $struct (ref.cast (ref $Struct) (local.get $v)))
                (local.set $type   (struct.get $Struct $type (local.get $struct)))
                (local.set $ok     (call $struct-type-is-a?/i32 (local.get $type) (local.get $std)))
                (if (result (ref eq))
                    (local.get $ok)
                    (then (global.get $true))
                    (else (global.get $false))))
               (else (global.get $false))))

     (func $arity-at-least-value (type $Prim1)
           (param $v (ref eq))
           (result (ref eq))

           (local $struct (ref $Struct))
           (local $fields (ref $Array))

           (local.set $struct (call $arity-at-least-unwrap
                                     (global.get $symbol:arity-at-least-value)
                                     (local.get $v)))
           (local.set $fields (struct.get $Struct $fields (local.get $struct)))
           (array.get $Array (local.get $fields) (i32.const 0)))

     (func $procedure-arity-entry->marker/i32
           (param $who (ref eq))
           (param $v   (ref eq))
           (result i32)

           (local $struct (ref $Struct))
           (local $fields (ref $Array))
           (local $value  (ref eq))
           (local $value-i32 i32)

           (if (ref.eq (call $exact-nonnegative-integer? (local.get $v)) (global.get $true))
               (then
                (return
                 (call $exact-nonnegative-integer->i32/saturating
                       (local.get $v)))))
           (if (ref.eq (call $arity-at-least? (local.get $v)) (global.get $true))
               (then
                (local.set $struct (call $arity-at-least-unwrap (local.get $who) (local.get $v)))
                (local.set $fields (struct.get $Struct $fields (local.get $struct)))
                (local.set $value  (array.get $Array (local.get $fields) (i32.const 0)))
                (local.set $value-i32
                           (call $exact-nonnegative-integer->i32/saturating
                                 (local.get $value)))
                (return
                 (i32.sub
                  (i32.const -1)
                  (local.get $value-i32)))))
           (call $raise-argument-error1
                 (local.get $who)
                 (global.get $string:procedure-arity?)
                 (local.get $v))
           (unreachable))

     ;; procedure-arity? : any/c -> boolean?
     ;;   A procedure arity is an exact nonnegative integer, an
     ;;   arity-at-least value, or a proper list of those values.
     (func $procedure-arity? (type $Prim1)
           (param $v (ref eq))
           (result   (ref eq))

           (local $node  (ref eq))
           (local $entry (ref eq))

           (if (ref.eq (call $exact-nonnegative-integer? (local.get $v))
                       (global.get $true))
               (then (return (global.get $true))))
           (if (ref.eq (call $arity-at-least? (local.get $v))
                       (global.get $true))
               (then (return (global.get $true))))
           (if (ref.eq (call $list? (local.get $v)) (global.get $false))
               (then (return (global.get $false))))
           (local.set $node (local.get $v))
           (block $done
                  (loop $loop
                        (br_if $done (ref.eq (local.get $node)
                                             (global.get $null)))
                        (local.set $entry
                                   (struct.get $Pair $a
                                               (ref.cast (ref $Pair)
                                                         (local.get $node))))
                        (if (ref.eq (call $exact-nonnegative-integer?
                                          (local.get $entry))
                                    (global.get $false))
                            (then
                             (if (ref.eq (call $arity-at-least?
                                               (local.get $entry))
                                         (global.get $false))
                                 (then (return (global.get $false))))))
                        (local.set $node
                                   (struct.get $Pair $d
                                               (ref.cast (ref $Pair)
                                                         (local.get $node))))
                        (br $loop)))
           (global.get $true))

     (func $procedure-arity-value->field
           (param $who   (ref eq))
           (param $arity (ref eq))
           (result (ref eq))

           (local $len    i32)
           (local $arr    (ref $I32Array))
           (local $i      i32)
           (local $node   (ref eq))
           (local $entry  (ref eq))
           (local $marker i32)
           (local $struct (ref $Struct))
           (local $fields (ref $Array))
           (local $value  (ref eq))
           (local $value-i32 i32)

           (if (ref.eq (call $exact-nonnegative-integer? (local.get $arity)) (global.get $true))
               (then (return (local.get $arity))))
           (if (ref.eq (call $arity-at-least? (local.get $arity)) (global.get $true))
               (then
                (local.set $struct (call $arity-at-least-unwrap
                                         (local.get $who)
                                         (local.get $arity)))
                (local.set $fields (struct.get $Struct $fields (local.get $struct)))
                (local.set $value (array.get $Array (local.get $fields) (i32.const 0)))
                (local.set $value-i32
                           (call $exact-nonnegative-integer->i32/saturating
                                 (local.get $value)))
                (if (i32.gt_u (local.get $value-i32) (i32.const 536870911))
                    (then (return (local.get $arity))))
                (local.set $marker (i32.sub (i32.const -1) (local.get $value-i32)))
                (return (ref.i31 (i32.shl (local.get $marker) (i32.const 1))))))
           (if (ref.eq (call $list? (local.get $arity)) (global.get $false))
               (then
                (call $raise-argument-error1
                      (local.get $who)
                      (global.get $string:procedure-arity?)
                      (local.get $arity))
                (unreachable)))
           (local.set $len (call $length/i32 (local.get $arity)))
           (if (i32.eqz (local.get $len))
               (then
                (return (ref.cast (ref eq)
                                  (array.new_fixed $I32Array 0)))))
           (if (i32.eq (local.get $len) (i32.const 1))
               (then
                (local.set $entry (call $car (local.get $arity)))
                (if (i32.or
                     (ref.eq (call $exact-nonnegative-integer? (local.get $entry))
                             (global.get $true))
                     (ref.eq (call $arity-at-least? (local.get $entry))
                             (global.get $true)))
                    (then (return (local.get $entry))))
                (return
                 (ref.i31
                  (i32.shl
                   (call $procedure-arity-entry->marker/i32
                         (local.get $who)
                         (local.get $entry))
                   (i32.const 1))))))
           (local.set $node (local.get $arity))
           (block $wide-scan-done
                  (loop $wide-scan
                        (br_if $wide-scan-done
                               (ref.eq (local.get $node)
                                       (global.get $null)))
                        (local.set $entry
                                   (struct.get $Pair $a
                                               (ref.cast (ref $Pair)
                                                         (local.get $node))))
                        (if (ref.eq (call $exact-nonnegative-integer?
                                          (local.get $entry))
                                    (global.get $true))
                            (then
                             (local.set $value-i32
                                        (call $exact-nonnegative-integer->i32/saturating
                                              (local.get $entry)))
                             (if (i32.gt_u (local.get $value-i32)
                                           (i32.const 536870911))
                                 (then (return (local.get $arity))))))
                        (if (ref.eq (call $arity-at-least? (local.get $entry))
                                    (global.get $true))
                            (then
                             (local.set $struct
                                        (call $arity-at-least-unwrap
                                              (local.get $who)
                                              (local.get $entry)))
                             (local.set $fields
                                        (struct.get $Struct $fields
                                                    (local.get $struct)))
                             (local.set $value
                                        (array.get $Array
                                                   (local.get $fields)
                                                   (i32.const 0)))
                             (local.set $value-i32
                                        (call $exact-nonnegative-integer->i32/saturating
                                              (local.get $value)))
                             (if (i32.gt_u (local.get $value-i32)
                                           (i32.const 536870911))
                                 (then (return (local.get $arity))))))
                        (local.set $node
                                   (struct.get $Pair $d
                                               (ref.cast (ref $Pair)
                                                         (local.get $node))))
                        (br $wide-scan)))
           (local.set $arr (call $i32array-make (local.get $len) (i32.const 0)))
           (local.set $node (local.get $arity))
           (local.set $i    (i32.const 0))
           (block $done
                  (loop $fill
                        (br_if $done (ref.eq (local.get $node) (global.get $null)))
                        (local.set $entry (struct.get $Pair $a
                                                      (ref.cast (ref $Pair) (local.get $node))))
                        (local.set $marker (call $procedure-arity-entry->marker/i32
                                                 (local.get $who)
                                                 (local.get $entry)))
                        (call $i32array-set! (local.get $arr) (local.get $i) (local.get $marker))
                        (local.set $node (struct.get $Pair $d
                                                     (ref.cast (ref $Pair) (local.get $node))))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br $fill)))
           (ref.cast (ref eq) (local.get $arr)))

     (func $procedure-arity-field-includes?/i32
           (param $a     (ref eq))
           (param $k-i32 i32)
           (result       i32)

           (local $arr    (ref $I32Array))
           (local $i      i32)
           (local $n      i32)
           (local $m      i32)
           (local $arity  i32)
           (local $start  i32)
           (local $struct (ref $Struct))
           (local $fields (ref $Array))
           (local $value  (ref eq))
           (local $k      (ref eq))
           (local $node   (ref eq))
           (local $entry  (ref eq))

           (if (ref.test (ref i31) (local.get $a))
               (then
                (local.set $arity (i32.shr_s
                                   (i31.get_s
                                    (ref.cast (ref i31) (local.get $a)))
                                   (i32.const 1)))
                (if (i32.ge_s (local.get $arity) (i32.const 0))
                    (then
                     (return (i32.eq (local.get $arity) (local.get $k-i32)))))
                (local.set $start (i32.sub (i32.const -1) (local.get $arity)))
                (return (i32.ge_u (local.get $k-i32) (local.get $start)))))

           (local.set $k
                      (call $integer-from-i64
                            (i64.extend_i32_u (local.get $k-i32))))
           (if (ref.eq (call $exact-nonnegative-integer? (local.get $a))
                       (global.get $true))
               (then
                (return
                 (i32.eqz (call $integer-compare/i32
                                (local.get $a)
                                (local.get $k))))))

           (if (ref.eq (call $arity-at-least? (local.get $a)) (global.get $true))
               (then
                (local.set $struct (call $arity-at-least-unwrap
                                         (global.get $symbol:procedure-arity-includes?)
                                         (local.get $a)))
                (local.set $fields (struct.get $Struct $fields (local.get $struct)))
                (local.set $value  (array.get $Array (local.get $fields) (i32.const 0)))
                (return
                 (i32.le_s (call $integer-compare/i32
                                 (local.get $value)
                                 (local.get $k))
                           (i32.const 0)))))

           (if (ref.test (ref $Pair) (local.get $a))
               (then
                (local.set $node (local.get $a))
                (block $list-done
                       (loop $list-loop
                             (br_if $list-done
                                    (ref.eq (local.get $node)
                                            (global.get $null)))
                             (local.set $entry
                                        (struct.get $Pair $a
                                                    (ref.cast (ref $Pair)
                                                              (local.get $node))))
                             (if (call $procedure-arity-field-includes?/i32
                                       (local.get $entry)
                                       (local.get $k-i32))
                                 (then (return (i32.const 1))))
                             (local.set $node
                                        (struct.get $Pair $d
                                                    (ref.cast (ref $Pair)
                                                              (local.get $node))))
                             (br $list-loop)))))

           (local.set $arr (ref.cast (ref $I32Array) (local.get $a)))
           (local.set $n   (array.len (local.get $arr)))
           (block $done
                  (loop $loop
                        (br_if $done (i32.ge_u (local.get $i) (local.get $n)))
                        (local.set $m
                                   (array.get $I32Array (local.get $arr) (local.get $i)))
                        (if (i32.ge_s (local.get $m) (i32.const 0))
                            (then
                             (if (i32.eq (local.get $m) (local.get $k-i32))
                                 (then (return (i32.const 1)))))
                            (else
                             (local.set $start
                                        (i32.sub (i32.const -1) (local.get $m)))
                             (if (i32.ge_u (local.get $k-i32) (local.get $start))
                                 (then (return (i32.const 1))))))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br $loop)))
           (i32.const 0))

     (func $procedure-arity-field-subset-of-mask?/i32
           (param $a        (ref eq))
           (param $old-mask i32)
           (result          i32)

           (local $arr       (ref $I32Array))
           (local $i         i32)
           (local $n         i32)
           (local $m         i32)
           (local $entry     (ref eq))
           (local $node      (ref eq))
           (local $start     i32)
           (local $value     (ref eq))
           (local $struct    (ref $Struct))
           (local $fields    (ref $Array))
           (local $value-i32 i32)
           (local $new-mask  i32)

           (if (ref.test (ref i31) (local.get $a))
               (then
                (local.set $new-mask
                           (call $procedure-arity-field->mask/i32
                                 (local.get $a)))
                (return
                 (i32.eqz
                  (i32.and (local.get $new-mask)
                           (i32.xor (local.get $old-mask)
                                    (i32.const -1)))))))

           (if (ref.eq (call $exact-nonnegative-integer? (local.get $a))
                       (global.get $true))
               (then
                (local.set $value-i32
                           (call $exact-nonnegative-integer->i32/saturating
                                 (local.get $a)))
                (return
                 (call $procedure-arity-mask-includes?/i32
                       (local.get $old-mask)
                       (local.get $value-i32)))))

           (if (ref.eq (call $arity-at-least? (local.get $a)) (global.get $true))
               (then
                (local.set $struct (call $arity-at-least-unwrap
                                         (global.get $symbol:procedure-reduce-arity)
                                         (local.get $a)))
                (local.set $fields (struct.get $Struct $fields (local.get $struct)))
                (local.set $value  (array.get $Array (local.get $fields) (i32.const 0)))
                (local.set $value-i32
                           (call $exact-nonnegative-integer->i32/saturating
                                 (local.get $value)))
                (return
                 (call $procedure-arity-mask-includes?/i32
                       (local.get $old-mask)
                       (local.get $value-i32)))))

           (if (ref.test (ref $Pair) (local.get $a))
               (then
                (local.set $node (local.get $a))
                (block $list-done
                       (loop $list-loop
                             (br_if $list-done
                                    (ref.eq (local.get $node)
                                            (global.get $null)))
                             (local.set $entry
                                        (struct.get $Pair $a
                                                    (ref.cast (ref $Pair)
                                                              (local.get $node))))
                             (if (i32.eqz
                                  (call $procedure-arity-field-subset-of-mask?/i32
                                        (local.get $entry)
                                        (local.get $old-mask)))
                                 (then (return (i32.const 0))))
                             (local.set $node
                                        (struct.get $Pair $d
                                                    (ref.cast (ref $Pair)
                                                              (local.get $node))))
                             (br $list-loop)))
                (return (i32.const 1))))

           (local.set $arr (ref.cast (ref $I32Array) (local.get $a)))
           (local.set $n   (array.len (local.get $arr)))
           (block $done
                  (loop $loop
                        (br_if $done (i32.ge_u (local.get $i) (local.get $n)))
                        (local.set $m
                                   (array.get $I32Array (local.get $arr) (local.get $i)))
                        (if (i32.ge_s (local.get $m) (i32.const 0))
                            (then
                             (if (i32.eqz
                                  (call $procedure-arity-mask-includes?/i32
                                        (local.get $old-mask)
                                        (local.get $m)))
                                 (then (return (i32.const 0)))))
                            (else
                             (local.set $start
                                        (i32.sub (i32.const -1) (local.get $m)))
                             (if (i32.eqz
                                  (call $procedure-arity-mask-includes?/i32
                                        (local.get $old-mask)
                                        (local.get $start)))
                                 (then (return (i32.const 0))))))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br $loop)))
           (i32.const 1))

     (func $procedure-arity-field->mask/i32
           (param $a (ref eq))
           (result i32)

           (local $arr   (ref $I32Array))
           (local $i     i32)
           (local $n     i32)
           (local $m     i32)
           (local $arity i32)
           (local $mask  i32)
           (local $start i32)

           (if (ref.test (ref i31) (local.get $a))
               (then
                (local.set $arity (i32.shr_s
                                   (i31.get_s
                                    (ref.cast (ref i31) (local.get $a)))
                                   (i32.const 1)))
                (return
                 (if (result i32)
                     (i32.ge_s (local.get $arity) (i32.const 0))
                     (then (i32.shl (i32.const 1) (local.get $arity)))
                     (else (i32.shl (i32.const -1)
                                    (i32.sub (i32.const -1) (local.get $arity))))))))
           (local.set $arr (ref.cast (ref $I32Array) (local.get $a)))
           (local.set $n   (array.len (local.get $arr)))
           (local.set $i   (i32.const 0))
           (local.set $mask (i32.const 0))
           (block $done
                  (loop $loop
                        (br_if $done (i32.ge_u (local.get $i) (local.get $n)))
                        (local.set $m
                                   (array.get $I32Array (local.get $arr) (local.get $i)))
                        (if (i32.ge_s (local.get $m) (i32.const 0))
                            (then
                             (local.set $mask
                                        (i32.or
                                         (local.get $mask)
                                         (i32.shl (i32.const 1) (local.get $m)))))
                            (else
                             (local.set $start
                                        (i32.sub (i32.const -1) (local.get $m)))
                             (local.set $mask
                                        (i32.or
                                         (local.get $mask)
                                         (i32.shl (i32.const -1) (local.get $start))))))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br $loop)))
           (local.get $mask))

     (func $procedure-arity-mask-value->field
           (param $who      (ref eq))
           (param $mask-raw (ref eq)) ; exact-integer?
           (result (ref eq))

           (local $mask          i32)
           (local $negative?     i32)
           (local $at-least-from i32)
           (local $limit         i32)
           (local $i             i32)
           (local $len           i32)
           (local $out           i32)
           (local $marker        i32)
           (local $arr           (ref $I32Array))

           (if (ref.eq (call $exact-integer? (local.get $mask-raw)) (global.get $false))
               (then
                (call $raise-argument-error1
                      (local.get $who)
                      (global.get $string:exact-integer?)
                      (local.get $mask-raw))
                (unreachable)))
           (local.set $mask
                      (i32.shr_s
                       (i31.get_s (ref.cast (ref i31) (local.get $mask-raw)))
                       (i32.const 1)))
           (local.set $negative? (i32.lt_s (local.get $mask) (i32.const 0)))
           (local.set $at-least-from (i32.const 31))
           (local.set $limit (i32.const 31))
           (if (local.get $negative?)
               (then
                ;; Negative masks represent exact arities below the first
                ;; all-ones suffix plus an arity-at-least marker.
                (local.set $at-least-from (i32.const 0))
                (block $found
                       (loop $find
                             (if (i32.eq (i32.shr_s
                                          (local.get $mask)
                                          (local.get $at-least-from))
                                         (i32.const -1))
                                 (then (br $found)))
                             (local.set $at-least-from
                                        (i32.add (local.get $at-least-from)
                                                 (i32.const 1)))
                             (br $find)))
                (local.set $limit (local.get $at-least-from))))

           (local.set $i (i32.const 0))
           (block $count-done
                  (loop $count
                        (br_if $count-done (i32.ge_u (local.get $i) (local.get $limit)))
                        (if (i32.ne (i32.and (local.get $mask)
                                             (i32.shl (i32.const 1) (local.get $i)))
                                    (i32.const 0))
                            (then
                             (local.set $marker (local.get $i))
                             (local.set $len (i32.add (local.get $len) (i32.const 1)))))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br $count)))
           (if (local.get $negative?)
               (then
                (local.set $marker
                           (i32.sub (i32.const -1) (local.get $at-least-from)))
                (local.set $len (i32.add (local.get $len) (i32.const 1)))))
           (if (i32.eqz (local.get $len))
               (then
                (return (ref.cast (ref eq)
                                  (array.new_fixed $I32Array 0)))))
           (if (i32.eq (local.get $len) (i32.const 1))
               (then
                (return (ref.i31 (i32.shl (local.get $marker) (i32.const 1))))))

           (local.set $arr (call $i32array-make (local.get $len) (i32.const 0)))
           (local.set $i (i32.const 0))
           (local.set $out (i32.const 0))
           (block $fill-done
                  (loop $fill
                        (br_if $fill-done (i32.ge_u (local.get $i) (local.get $limit)))
                        (if (i32.ne (i32.and (local.get $mask)
                                             (i32.shl (i32.const 1) (local.get $i)))
                                    (i32.const 0))
                            (then
                             (call $i32array-set!
                                   (local.get $arr)
                                   (local.get $out)
                                   (local.get $i))
                             (local.set $out (i32.add (local.get $out) (i32.const 1)))))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br $fill)))
           (if (local.get $negative?)
               (then
                (call $i32array-set!
                      (local.get $arr)
                      (local.get $out)
                      (i32.sub (i32.const -1) (local.get $at-least-from)))))
           (ref.cast (ref eq) (local.get $arr)))

     ;; make-composed-procedure : list? boolean? symbol? -> procedure?
     ;;   Build the procedure returned by compose or compose1.
     (func $make-composed-procedure
           (param $args    (ref eq))
           (param $single? (ref eq))
           (param $who     (ref eq))
           (result         (ref eq))

           (local $procs (ref $Array))
           (local $n     i32)
           (local $i     i32)
           (local $p     (ref $Procedure))
           (local $free  (ref $Free))

           (local.set $procs (call $list->array (local.get $args)))
           (local.set $n     (array.len (local.get $procs)))
           (block $done
                  (loop $loop
                        (br_if $done (i32.ge_u (local.get $i) (local.get $n)))
                        (if (ref.eq
                             (call $procedure?
                                   (array.get $Array
                                              (local.get $procs)
                                              (local.get $i)))
                             (global.get $false))
                            (then
                             (call $raise-argument-error:procedure-expected/who
                                   (local.get $who)
                                   (array.get $Array
                                              (local.get $procs)
                                              (local.get $i)))
                             (unreachable)))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br $loop)))
           (if (i32.eq (local.get $n) (i32.const 1))
               (then (return (array.get $Array (local.get $procs) (i32.const 0)))))
           (if (i32.eqz (local.get $n))
               (then
                (local.set $free
                           (array.new_fixed $Free
                                            2
                                            (ref.cast (ref eq) (local.get $procs))
                                            (local.get $single?)))
                (return
                 (struct.new $Closure
                             (i32.const 0)
                             (global.get $symbol:compose)
                             (ref.i31 (i32.const -2))
                             (global.get $the-racket-realm)
                             (ref.func $invoke-composed-procedure)
                             (global.get $symbol:compose)
                             (ref.func $dummy-code)
                             (local.get $free)))))
           (local.set $p (ref.cast (ref $Procedure)
                                    (array.get $Array
                                               (local.get $procs)
                                               (i32.sub (local.get $n) (i32.const 1)))))
           (local.set $free
                      (array.new_fixed $Free
                                       2
                                       (ref.cast (ref eq) (local.get $procs))
                                       (local.get $single?)))
           (struct.new $Closure
                       (i32.const 0)
                       (global.get $symbol:compose)
                       (struct.get $Procedure $arity (local.get $p))
                       (global.get $the-racket-realm)
                       (ref.func $invoke-composed-procedure)
                       (global.get $symbol:compose)
                       (ref.func $dummy-code)
                       (local.get $free)))

     (func $compose (type $Prim>=0)
           (param $args (ref eq))
           (result      (ref eq))
           (call $make-composed-procedure
                 (local.get $args)
                 (global.get $false)
                 (global.get $symbol:compose)))

     (func $compose1 (type $Prim>=0)
           (param $args (ref eq))
           (result      (ref eq))
           (call $make-composed-procedure
                 (local.get $args)
                 (global.get $true)
                 (global.get $symbol:compose1)))

     (func $procedure-extract-target (type $Prim1)
           (param $proc (ref eq)) ; procedure?
           (result      (ref eq))
           ;; Racket extracts a target procedure from a struct procedure
           ;; whose prop:procedure value names a field. WebRacket does
           ;; not yet expose that internal target, so ordinary procedures
           ;; conservatively return #f for now.
           (if (ref.eq (call $procedure? (local.get $proc)) (global.get $false))
               (then (call $raise-argument-error:procedure-expected/who
                           (global.get $symbol:procedure-extract-target)
                           (local.get $proc))
                     (unreachable)))
           (global.get $false))

     (func $procedure-reduce-arity (type $Prim24)
           (param $proc  (ref eq)) ; procedure?
           (param $arity (ref eq)) ; procedure-arity?
           (param $name  (ref eq)) ; (or/c symbol? #f), default #f
           (param $realm (ref eq)) ; symbol?, default 'racket
           (result (ref eq))

           (local $p          (ref $Procedure))
           (local $new-arity  (ref eq))
           (local $old-mask   i32)
           (local $new-name   (ref eq))
           (local $new-realm  (ref eq))
           (local $free       (ref $Free))

           (local.set $new-arity (global.get $false))
           (local.set $new-name  (global.get $false))
           (local.set $new-realm (global.get $false))
           (if (ref.eq (call $procedure? (local.get $proc)) (global.get $false))
               (then (call $raise-argument-error:procedure-expected/who
                           (global.get $symbol:procedure-reduce-arity)
                           (local.get $proc))
                     (unreachable)))
           (local.set $p (ref.cast (ref $Procedure) (local.get $proc)))
           (local.set $new-arity (call $procedure-arity-value->field
                                       (global.get $symbol:procedure-reduce-arity)
                                       (local.get $arity)))
           (local.set $old-mask (call $procedure-arity-mask/checked/i32 (local.get $p)))
           (if (i32.eqz
                (call $procedure-arity-field-subset-of-mask?/i32
                      (local.get $new-arity)
                      (local.get $old-mask)))
               (then
                (call $raise-argument-error1
                      (global.get $symbol:procedure-reduce-arity)
                      (global.get $string:procedure-arity?)
                      (local.get $arity))
                (unreachable)))

           (if (ref.eq (local.get $name) (global.get $missing))
               (then (local.set $name (global.get $false))))
           (if (ref.eq (local.get $name) (global.get $false))
               (then
                (local.set $new-name  (struct.get $Procedure $name (local.get $p)))
                (local.set $new-realm (struct.get $Procedure $realm (local.get $p))))
               (else
                (if (i32.eqz (ref.test (ref $Symbol) (local.get $name)))
                    (then
                     (call $raise-argument-error1
                           (global.get $symbol:procedure-reduce-arity)
                           (global.get $string:symbol?)
                           (local.get $name))
                     (unreachable)))
                (local.set $new-name (local.get $name))
                (if (ref.eq (local.get $realm) (global.get $missing))
                    (then (local.set $new-realm (global.get $symbol:racket)))
                    (else
                     (if (i32.eqz (ref.test (ref $Symbol) (local.get $realm)))
                         (then
                          (call $raise-argument-error1
                                (global.get $symbol:procedure-reduce-arity)
                                (global.get $string:symbol?)
                                (local.get $realm))
                          (unreachable)))
                     (local.set $new-realm (local.get $realm))))))

           ;; Keep the original procedure as payload. The reduced wrapper
           ;; checks its new arity, then forwards the original user args.
           (local.set $free (array.new_fixed $Free 1 (local.get $p)))
           (struct.new $Closure
                       (i32.const 0)
                       (local.get $new-name)
                       (local.get $new-arity)
                       (local.get $new-realm)
                       (ref.func $invoke-reduced-procedure)
                       (local.get $new-name)
                       (ref.func $dummy-code)
                       (local.get $free)))

     (func $procedure-reduce-arity-mask (type $Prim24)
           (param $proc  (ref eq)) ; procedure?
           (param $mask  (ref eq)) ; exact-integer?
           (param $name  (ref eq)) ; (or/c symbol? #f), default #f
           (param $realm (ref eq)) ; symbol?, default 'racket
           (result (ref eq))

           (local $p          (ref $Procedure))
           (local $new-arity  (ref eq))
           (local $old-mask   i32)
           (local $new-name   (ref eq))
           (local $new-realm  (ref eq))
           (local $free       (ref $Free))

           (local.set $new-arity (global.get $false))
           (local.set $new-name  (global.get $false))
           (local.set $new-realm (global.get $false))
           (if (ref.eq (call $procedure? (local.get $proc)) (global.get $false))
               (then (call $raise-argument-error:procedure-expected/who
                           (global.get $symbol:procedure-reduce-arity-mask)
                           (local.get $proc))
                     (unreachable)))
           (local.set $p (ref.cast (ref $Procedure) (local.get $proc)))
           (local.set $new-arity
                      (call $procedure-arity-mask-value->field
                            (global.get $symbol:procedure-reduce-arity-mask)
                            (local.get $mask)))
           (local.set $old-mask (call $procedure-arity-mask/checked/i32 (local.get $p)))
           (if (i32.eqz
                (call $procedure-arity-field-subset-of-mask?/i32
                      (local.get $new-arity)
                      (local.get $old-mask)))
               (then
                (call $raise-argument-error1
                      (global.get $symbol:procedure-reduce-arity-mask)
                      (global.get $string:exact-integer?)
                      (local.get $mask))
                (unreachable)))

           (if (ref.eq (local.get $name) (global.get $missing))
               (then (local.set $name (global.get $false))))
           (if (ref.eq (local.get $name) (global.get $false))
               (then
                (local.set $new-name  (struct.get $Procedure $name (local.get $p)))
                (local.set $new-realm (struct.get $Procedure $realm (local.get $p))))
               (else
                (if (i32.eqz (ref.test (ref $Symbol) (local.get $name)))
                    (then
                     (call $raise-argument-error1
                           (global.get $symbol:procedure-reduce-arity-mask)
                           (global.get $string:symbol?)
                           (local.get $name))
                     (unreachable)))
                (local.set $new-name (local.get $name))
                (if (ref.eq (local.get $realm) (global.get $missing))
                    (then (local.set $new-realm (global.get $symbol:racket)))
                    (else
                     (if (i32.eqz (ref.test (ref $Symbol) (local.get $realm)))
                         (then
                          (call $raise-argument-error1
                                (global.get $symbol:procedure-reduce-arity-mask)
                                (global.get $string:symbol?)
                                (local.get $realm))
                          (unreachable)))
                     (local.set $new-realm (local.get $realm))))))

           (local.set $free (array.new_fixed $Free 1 (local.get $p)))
           (struct.new $Closure
                       (i32.const 0)
                       (local.get $new-name)
                       (local.get $new-arity)
                       (local.get $new-realm)
                       (ref.func $invoke-reduced-procedure)
                       (local.get $new-name)
                       (ref.func $dummy-code)
                       (local.get $free)))

     (func $procedure-arity (type $Prim1)
           ; Wrapper: accepts procedures and non-applicable structs, then delegates
           ; to the checked version that expects (ref $Procedure).
           (param $proc (ref eq))
           (result      (ref eq))

           ;; Racket reports no accepted arities for a non-applicable struct
           ;; instead of treating it like an ordinary non-procedure value.
           (if (ref.test (ref $Struct) (local.get $proc))
               (then
                (if (ref.eq (call $procedure? (local.get $proc)) (global.get $false))
                    (then (return (global.get $null))))))
           (if (i32.eqz (ref.test (ref $Procedure) (local.get $proc)))
               (then (call $raise-argument-error:procedure-expected/who
                           (global.get $symbol:procedure-arity)
                           (local.get $proc))
                     (unreachable)))
           ;; 2. Delegate to the checked implementation
           (return_call $procedure-arity/checked
                        (ref.cast (ref $Procedure) (local.get $proc))))

     (func $procedure-arity/checked
           ; Produces normalized arity information: exact integers or arity-at-least structs.
           (param $p (ref $Procedure))
           (result (ref eq))

           (local $a         (ref eq))
           (local $arr       (ref $I32Array))
           (local $n         i32)
           (local $i         i32)
           (local $m         i32)
           (local $list      (ref eq))
           (local $fx        (ref i31))
           (local $elem      (ref eq))
           (local $arity-i32 i32)
           (local $value     i32)
           (local $delegate  (ref eq))

           ;; Initialize non-defaultable locals
           (local.set $elem (global.get $false))
           (local.set $delegate (global.get $false))

           ;; Applicable structs should reflect the arity of their
           ;; field-based prop:procedure delegate, not their wrapper
           ;; struct's internal zero-arity application shell.
           (if (ref.test (ref $Struct) (local.get $p))
               (then
                (local.set $delegate (call $struct-procedure-delegate
                                           (local.get $p)))
                (if (ref.test (ref $Procedure) (local.get $delegate))
                    (then (return_call $procedure-arity/checked
                                       (ref.cast (ref $Procedure)
                                                 (local.get $delegate)))))))

           ;; 1. Extract arity field (either a fixnum (ref i31) or an $I32Array of markers)
           (local.set $a (struct.get $Procedure $arity (local.get $p)))
           ;; 2. If it’s a single-arity fixnum, normalize negatives to arity-at-least
           (if (ref.test (ref i31) (local.get $a))
               (then (local.set $fx       (ref.cast (ref i31) (local.get $a)))
                     (local.set $arity-i32 (i32.shr_s (i31.get_s (local.get $fx)) (i32.const 1)))
                     (if (i32.ge_s (local.get $arity-i32) (i32.const 0))
                         (then (return (local.get $a)))
                         (else (local.set $value (i32.sub (i32.const -1) (local.get $arity-i32)))
                               (return (call $arity-at-least/make (local.get $value)))))))
           (if (ref.eq (call $exact-nonnegative-integer? (local.get $a))
                       (global.get $true))
               (then (return (local.get $a))))
           (if (ref.eq (call $arity-at-least? (local.get $a))
                       (global.get $true))
               (then (return (local.get $a))))
           (if (ref.test (ref $Pair) (local.get $a))
               (then (return (local.get $a))))
           ;; 3. Otherwise, cast to $I32Array and build a list of ALL markers (incl. negatives)
           (local.set $arr (ref.cast (ref $I32Array) (local.get $a)))
           (local.set $n   (array.len (local.get $arr)))
           (local.set $list (global.get $null))
           ;;    Iterate right-to-left so that consing preserves original order
           (local.set $i (i32.sub (local.get $n) (i32.const 1)))
           (loop $rev
                 ;; 4. If done, return the accumulated list
                 (if (i32.lt_s (local.get $i) (i32.const 0))
                     (then (return (local.get $list))))
                 ;; 5. Read marker m at index i and normalize it
                 (local.set $m (array.get $I32Array (local.get $arr) (local.get $i)))
                 (if (i32.ge_s (local.get $m) (i32.const 0))
                     (then (local.set $elem (ref.i31 (i32.shl (local.get $m) (i32.const 1)))))
                     (else (local.set $value (i32.sub (i32.const -1) (local.get $m)))
                           (local.set $elem (call $arity-at-least/make (local.get $value)))))
                 (local.set $list (call $cons (local.get $elem) (local.get $list)))
                 ;; 6. Decrement i and continue
                 (local.set $i (i32.sub (local.get $i) (i32.const 1)))
                 (br $rev))
           (unreachable))

     ;; procedure-result-arity : procedure? -> (or/c #f procedure-arity?)
     ;;   Return known result arity. WebRacket currently does not track
     ;;   result arity for closures, so report #f conservatively except
     ;;   for primitive procedures, which store explicit metadata.
     (func $procedure-result-arity (type $Prim1)
           (param $proc (ref eq)) ;; procedure?
           (result      (ref eq))

           (if (ref.eq (call $procedure? (local.get $proc)) (global.get $false))
               (then (call $raise-argument-error:procedure-expected/who
                           (global.get $symbol:procedure-result-arity)
                           (local.get $proc))
                     (unreachable)))
           (if (ref.test (ref $PrimitiveProcedure) (local.get $proc))
               (then
                (return
                 (struct.get $PrimitiveProcedure $result-arity
                             (ref.cast (ref $PrimitiveProcedure)
                                       (local.get $proc))))))
           (global.get $false))

     (func $procedure-arity-mask (type $Prim1)
           ; TODO: Only tested with closures.
           ;       Also test with primitives and case-lambda
           (param $proc (ref eq))
           (result (ref eq))

           (local $p     (ref null $Procedure))
           (local $mask  i32)

           ;; Plain structs have an empty arity mask in Racket.
           (if (ref.test (ref $Struct) (local.get $proc))
               (then
                (if (ref.eq (call $procedure? (local.get $proc)) (global.get $false))
                    (then (return (ref.i31 (i32.const 0)))))))
           (if (i32.eqz (ref.test (ref $Procedure) (local.get $proc)))
               (then (call $raise-argument-error:procedure-expected/who
                           (global.get $symbol:procedure-arity-mask)
                           (local.get $proc))
                     (unreachable)))
           (local.set $p (ref.cast (ref $Procedure) (local.get $proc)))
           ;; Step 2: compute mask in checked helper
           (local.set $mask
                      (call $procedure-arity-mask/checked/i32
                            (ref.cast (ref $Procedure) (local.get $p))))
           ;; Step 4: return as fixnum
           (ref.i31 (i32.shl (local.get $mask) (i32.const 1))))

     (func $procedure-arity-mask/checked/i32
           ; TODO: Only tested with closures.
           ;       Also test with primitives and case-lambda
           (param $proc (ref $Procedure))
           (result      i32)

           (local $a     (ref eq))
           (local $arr   (ref $I32Array))
           (local $i     i32)
           (local $n     i32)
           (local $m     i32)
           (local $arity i32)
           (local $mask  i32)
           (local $start i32)
           (local $delegate (ref eq))

           (local.set $delegate (global.get $false))
           (if (ref.test (ref $Struct) (local.get $proc))
               (then
                (local.set $delegate (call $struct-procedure-delegate
                                           (local.get $proc)))
                (if (ref.test (ref $Procedure) (local.get $delegate))
                    (then (return_call $procedure-arity-mask/checked/i32
                                       (ref.cast (ref $Procedure)
                                                 (local.get $delegate)))))))

           ;; Step 1: inspect the procedure arity representation
           (local.set $a (struct.get $Procedure $arity (local.get $proc)))

           ;; Step 2: single arity marker stored as fixnum
           (if (ref.test (ref i31) (local.get $a))
               (then
                (local.set $arity (i32.shr_s
                                   (i31.get_s
                                    (ref.cast (ref i31) (local.get $a)))
                                   (i32.const 1)))
                (local.set $mask
                           (if (result i32)
                               (i32.ge_s (local.get $arity) (i32.const 0))
                               ;; If arity ≥ 0, mask = 1 << arity
                               (then (i32.shl (i32.const 1) (local.get $arity)))
                               ;; If arity < 0, mask = -1 << (-1 - arity)
                               (else (i32.shl (i32.const -1)
                                              (i32.sub (i32.const -1) (local.get $arity))))))
                (return (local.get $mask))))

           ;; Step 3: arity set stored as $I32Array of markers
           (local.set $arr (ref.cast (ref $I32Array) (local.get $a)))
           (local.set $n   (array.len (local.get $arr)))
           (local.set $i   (i32.const 0))
           (local.set $mask (i32.const 0))

           (block $done
                  (loop $loop
                        (br_if $done (i32.ge_u (local.get $i) (local.get $n)))
                        (local.set $m
                                   (array.get $I32Array (local.get $arr) (local.get $i)))
                        (if (i32.ge_s (local.get $m) (i32.const 0))
                            (then
                             (local.set $mask
                                        (i32.or
                                         (local.get $mask)
                                         (i32.shl (i32.const 1) (local.get $m)))))
                            (else
                             (local.set $start
                                        (i32.sub (i32.const -1) (local.get $m)))
                             (local.set $mask
                                        (i32.or
                                         (local.get $mask)
                                         (i32.shl (i32.const -1) (local.get $start))))))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br $loop)))
           (local.get $mask))

     (func $raise-argument-error:fixnum-expected
           (param $got (ref eq))

           (call $raise-argument-error1
                 (global.get $symbol:procedure-arity-includes?)
                 (global.get $string:exact-nonnegative-integer?)
                 (local.get $got)))

     (func $procedure-arity-mask-includes?/i32
           (param $mask  i32)
           (param $k-i32 i32)
           (result       i32)

           ;; For k >= 31, the bit shift below would wrap. A negative mask
           ;; still indicates an arity-at-least suffix that includes large k.
           (if (i32.ge_u (local.get $k-i32) (i32.const 31))
               (then (return (i32.lt_s (local.get $mask) (i32.const 0)))))
           (i32.ne (i32.and (local.get $mask)
                            (i32.shl (i32.const 1) (local.get $k-i32)))
                   (i32.const 0)))

     (func $procedure-arity-includes?
           (param $proc    (ref eq))    ;; procedure?
           (param $k       (ref eq))    ;; exact-nonnegative-integer?
           (param $kws-ok? (ref eq))    ;; optional any/c, default = #f
           (result         (ref eq))    ;; returns #true or #false

           (local $k-i32 i32)

           ;; Step 1: validate proc with the public function's `who`. Plain
           ;; structs are procedure-shaped internally and have no accepted
           ;; arities, so they are handled after the k check like Racket.
           (if (i32.eqz (ref.test (ref $Procedure) (local.get $proc)))
               (then (call $raise-argument-error:procedure-expected/who
                           (global.get $symbol:procedure-arity-includes?)
                           (local.get $proc))
                     (unreachable)))
           ;; Step 2: type check and decode k.
           (local.set $k-i32
                      (call $exact-nonnegative-integer->i32/saturating
                            (local.get $k)))
           (if (i32.lt_s (local.get $k-i32) (i32.const 0))
               (then (call $raise-argument-error:fixnum-expected (local.get $k))
                     (unreachable)))
           (if (ref.eq (call $procedure? (local.get $proc)) (global.get $false))
               (then (return (global.get $false))))
           ;; Step 3: check whether k is included in the effective arity.
           (if (call $procedure-arity-includes?/checked/i32
                     (ref.cast (ref $Procedure) (local.get $proc))
                     (local.get $k-i32))
               (then (return (global.get $true)))
               (else (return (global.get $false))))
           (unreachable))

     (func $procedure-arity-includes?/checked/i32
           (param $proc    (ref $Procedure))
           (param $k-i32   i32)
           ; (param $kws-ok? (ref eq))          ;; we don't use this yet
           (result         i32)

           (local $delegate (ref eq))

           (local.set $delegate (global.get $false))
           (if (ref.test (ref $Struct) (local.get $proc))
               (then
                (local.set $delegate (call $struct-procedure-delegate
                                           (local.get $proc)))
                (if (ref.test (ref $Procedure) (local.get $delegate))
                    (then (return_call $procedure-arity-includes?/checked/i32
                                       (ref.cast (ref $Procedure)
                                                 (local.get $delegate))
                                       (local.get $k-i32))))))

           ;; Step 1: inspect the stored arity representation directly.
           (call $procedure-arity-field-includes?/i32
                 (struct.get $Procedure $arity (local.get $proc))
                 (local.get $k-i32)))

     (func $raise-argument-error:primitive-procedure-expected/who
           (param $who (ref eq))
           (param $got (ref eq))

           (call $raise-argument-error1
                 (local.get $who)
                 (global.get $string:primitive?)
                 (local.get $got)))

     (func $raise-argument-error:primitive-procedure-expected
           (param $got (ref eq))

           (call $raise-argument-error:primitive-procedure-expected/who
                 (global.get $symbol:primitive?)
                 (local.get $got)))

     (func $primitive? (type $Prim1)
           (param $v (ref eq))
           (result   (ref eq))
           (if (result (ref eq))
               (ref.test (ref $PrimitiveProcedure) (local.get $v))
               (then (global.get $true))
               (else (global.get $false))))

     (func $primitive-result-arity (type $Prim1)
           (param $v (ref eq))
           (result (ref eq))

           (local $p (ref $PrimitiveProcedure))
           ;; Step 1: Validate type
           (if (i32.eqz (ref.test (ref $PrimitiveProcedure) (local.get $v)))
               (then (call $raise-argument-error:primitive-procedure-expected/who
                           (global.get $symbol:primitive-result-arity)
                           (local.get $v))))
           ;; Step 2: Cast after validation
           (local.set $p (ref.cast (ref $PrimitiveProcedure) (local.get $v)))
           ;; Step 3: Return result-arity field
           (struct.get $PrimitiveProcedure $result-arity (local.get $p)))


     (func $primitive-closure? (type $Prim1)
           (param $v (ref eq))
           (result   (ref eq))
           (if (result (ref eq))
               (ref.test (ref $PrimitiveClosure) (local.get $v))
               (then (global.get $true))
               (else (global.get $false)))))))

;; procedure-promoted-runtime-helper-names : (listof symbol?)
;;   Name procedure helpers available in the standalone core runtime.
(define procedure-promoted-runtime-helper-names
  '($ensure-arity-at-least-type
    $arity-at-least/make
    $arity-at-least/make/value
    $make-arity-at-least
    $arity-at-least
    $raise-argument-error:arity-at-least-expected
    $arity-at-least-unwrap
    $arity-at-least?
    $arity-at-least-value
    $procedure-arity-entry->marker/i32
    $procedure-arity?
    $procedure-arity-value->field
    $procedure-arity-mask-includes?/i32
    $procedure-arity-field-includes?/i32
    $procedure-arity-field-subset-of-mask?/i32
    $procedure-arity-field->mask/i32
    $procedure-arity-mask-value->field
    $procedure-arity-includes?/checked/i32))

;; procedure-promoted-runtime-helpers : (listof core-runtime-helper?)
;;   Enumerate promoted procedure helpers selected from monolithic runtime order.
(define procedure-promoted-runtime-helpers
  (select-runtime-helpers procedure-arity-and-reflection-runtime-only-helpers
                          procedure-promoted-runtime-helper-names))

;; procedure-core-runtime-helpers : (listof core-runtime-helper?)
;;   Enumerate procedure-call helpers that can be emitted in core runtime.
(define procedure-core-runtime-helpers
  (append procedure-base-core-runtime-helpers
          primitive-dispatch-core-runtime-helpers
          procedure-primitive-invoker-core-runtime-helpers
          procedure-invocation-error-core-runtime-helpers
          procedure-invoker-core-runtime-helpers
          procedure-case-lambda-core-runtime-helpers
          procedure-promoted-runtime-helpers))

;; procedure-helper-forms : hasheq? (listof symbol?) -> (listof datum?)
;;   Render the given procedure helper names from the runtime helper index.
(define (procedure-helper-forms helper-index names)
  (for/list ([name (in-list names)])
    (core-runtime-helper-form helper-index name)))

;; procedure-rest-argument-core-runtime-helper-forms : hasheq? -> (listof datum?)
;;   Render procedure rest-argument core helpers in runtime assembly order.
(define (procedure-rest-argument-core-runtime-helper-forms helper-index)
  (procedure-helper-forms helper-index procedure-rest-argument-core-runtime-helper-names))

;; procedure-primitive-error-core-runtime-helper-forms : hasheq? -> (listof datum?)
;;   Render primitive invocation error helpers that are shared with core runtime.
(define (procedure-primitive-error-core-runtime-helper-forms helper-index)
  (procedure-helper-forms helper-index procedure-primitive-error-core-runtime-helper-names))

;; procedure-invocation-error-core-runtime-helper-forms : hasheq? -> (listof datum?)
;;   Render procedure invocation error helpers that are shared with core runtime.
(define (procedure-invocation-error-core-runtime-helper-forms helper-index)
  (procedure-helper-forms helper-index procedure-invocation-error-core-runtime-helper-names))

;; procedure-invoker-core-runtime-helper-forms : hasheq? -> (listof datum?)
;;   Render procedure invokers that are shared with core runtime.
(define (procedure-invoker-core-runtime-helper-forms helper-index)
  (procedure-helper-forms helper-index procedure-invoker-core-runtime-helper-names))

;; procedure-case-lambda-core-runtime-helper-forms : hasheq? -> (listof datum?)
;;   Render case-lambda helpers that are shared with core runtime.
(define (procedure-case-lambda-core-runtime-helper-forms helper-index)
  (procedure-helper-forms helper-index procedure-case-lambda-core-runtime-helper-names))

;; procedure-primitive-dispatch-core-runtime-helper-forms : hasheq? -> (listof datum?)
;;   Render primitive dispatch helpers from the shared core helper registry.
(define (procedure-primitive-dispatch-core-runtime-helper-forms helper-index)
  (procedure-helper-forms helper-index primitive-dispatch-core-runtime-helper-names))

;; procedure-primitive-invoker-core-runtime-helper-forms : hasheq? -> (listof datum?)
;;   Render primitive shape invokers from the shared core helper registry.
(define (procedure-primitive-invoker-core-runtime-helper-forms helper-index)
  (procedure-helper-forms
   helper-index
   (map core-runtime-helper-name procedure-primitive-invoker-core-runtime-helpers)))

;; procedure-bootstrap-runtime-helper-forms : hasheq? -> (listof datum?)
;;   Render procedure bootstrap helpers before generated primitive invokers.
(define (procedure-bootstrap-runtime-helper-forms helper-index)
  (append
   (procedure-helper-forms helper-index procedure-invocation-error-runtime-only-helper-names)
   (procedure-helper-forms helper-index procedure-application-error-runtime-only-helper-names)
   (procedure-helper-forms helper-index procedure-invoker-runtime-only-helper-names)
   (procedure-helper-forms helper-index primitive-invocation-error-runtime-only-helper-names)
   (procedure-helper-forms helper-index primitive-dispatch-runtime-only-helper-names)))

;; procedure-post-primitive-invoker-runtime-helper-forms : hasheq? -> (listof datum?)
;;   Render procedure bootstrap helpers after generated primitive invokers.
(define (procedure-post-primitive-invoker-runtime-helper-forms helper-index)
  (procedure-helper-forms helper-index procedure-case-lambda-runtime-only-helper-names))

;; procedure-main-runtime-helper-forms : hasheq? -> (listof datum?)
;;   Render the main procedure helper block before chaperone-procedure helpers.
(define (procedure-main-runtime-helper-forms helper-index)
  (append
   (procedure-helper-forms helper-index procedure-basic-runtime-only-helper-names)
   (procedure-helper-forms helper-index procedure-arity-at-least-runtime-only-helper-names)
   (procedure-helper-forms helper-index procedure-arity-field-runtime-only-helper-names)
   (procedure-helper-forms helper-index procedure-composition-runtime-only-helper-names)
   (procedure-helper-forms helper-index procedure-reduction-runtime-only-helper-names)
   (procedure-helper-forms helper-index procedure-arity-runtime-only-helper-names)
   (procedure-helper-forms helper-index procedure-result-arity-runtime-only-helper-names)))

;; procedure-arity-mask-runtime-helper-forms : hasheq? -> (listof datum?)
;;   Render procedure arity-mask helpers in runtime assembly order.
(define (procedure-arity-mask-runtime-helper-forms helper-index)
  (procedure-helper-forms helper-index procedure-arity-mask-runtime-only-helper-names))

;; primitive-reflection-runtime-helper-forms : hasheq? -> (listof datum?)
;;   Render primitive reflection helpers in runtime assembly order.
(define (primitive-reflection-runtime-helper-forms helper-index)
  (procedure-helper-forms helper-index primitive-reflection-runtime-only-helper-names))

;; procedure-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate all procedure helpers used by the monolithic runtime.
(define procedure-runtime-only-helpers
  (append procedure-invocation-error-runtime-only-helpers
          procedure-application-error-runtime-only-helpers
          procedure-invoker-runtime-only-helpers
          primitive-invocation-error-runtime-only-helpers
          primitive-dispatch-runtime-only-helpers
          procedure-case-lambda-runtime-only-helpers
          procedure-basic-runtime-only-helpers
          (remove-runtime-helpers procedure-arity-and-reflection-runtime-only-helpers
                                  procedure-promoted-runtime-helper-names)))

(module+ test
  (require rackunit)

  ;; form-name : datum? -> symbol?
  ;;   Return the function name from a rendered helper form.
  (define (form-name form)
    (second form))

  (define procedure-helper-index
    (make-core-runtime-helper-index
     (append procedure-core-runtime-helpers
             procedure-runtime-only-helpers)))

  (define procedure-core-helper-index
    (make-core-runtime-helper-index procedure-core-runtime-helpers))

  (check-equal? procedure-global-names
                '($arity-at-least-type))

  (define (tree-member? needle tree)
    (cond
      [(equal? needle tree) #t]
      [(pair? tree)
       (or (tree-member? needle (car tree))
           (tree-member? needle (cdr tree)))]
      [else #f]))

  (check-equal? (arity->shape 0) 0)
  (check-equal? (arity->shape 6) 26)
  (check-equal? (arity->shape '(2 3)) 14)
  (check-equal? (arity->shape '(0 1 2 3 4 5)) 29)
  (check-equal? (arity->shape (arity-at-least 4)) 30)
  (check-equal? (primitive-arity->shape 'values (arity-at-least 0)) 10)
  (check-equal? (primitive-arity->shape 'apply (arity-at-least 0)) 6)
  (check-equal? (primitive-arity->shape 'void (arity-at-least 4)) 31)
  (check-equal? (primitive-arity->shape 'bytes-convert '(2 3 4 5 6 7)) 32)
  (check-equal? (primitive-arity->shape 'peek-bytes-avail! '(2 3 4 5 6)) 33)
  (check-equal? (primitive-arity->shape 'peek-bytes-avail!* '(2 3 4 5 6)) 33)
  (check-equal? (shape->invoker 31) 'primitive-invoke:shape-31)
  (check-equal? (shape->invoker 32) 'primitive-invoke:shape-32)
  (check-equal? (shape->invoker 33) 'primitive-invoke:shape-33)
  (check-equal? (arity->procedure-field-expr 2) (Imm 2))
  (check-equal? (arity->procedure-field-expr '(0 2))
                '(ref.cast (ref eq)
                           (array.new_fixed $I32Array
                                            2
                                            (i32.const 0)
                                            (i32.const 2))))
  (check-equal? (procedure-primitive-global-forms '(foo))
                `((global $prim:foo (mut (ref eq)) ,(Imm (undefined)))))
  (check-equal? (procedure-ffi-primitive-global-forms '(js-alert))
                `((global $prim:js-alert (mut (ref eq)) ,(Imm (undefined)))))

  (define primitive-init
    (first (procedure-primitive-initializer-forms
            (list (list 'foo 2 1)))))
  (check-equal? (first primitive-init) 'global.set)
  (check-equal? (second primitive-init) '$prim:foo)
  (check-true (tree-member? '(ref.func $primitive-invoke:shape-2) primitive-init))
  (check-true (tree-member? '(ref.func $foo) primitive-init))

  (define peek-bytes-avail-init
    (first (procedure-primitive-initializer-forms
            (list (list 'peek-bytes-avail! '(2 3 4 5 6) 1)))))
  (check-true (tree-member? '(ref.func $primitive-invoke:shape-33)
                            peek-bytes-avail-init))

  (define ffi-init
    (first (procedure-ffi-primitive-initializer-forms
            (list (list 'js-many 7)))))
  (check-equal? (first ffi-init) 'global.set)
  (check-equal? (second ffi-init) '$prim:js-many)
  (check-true (tree-member? '(ref.func $primitive-invoke:shape-6) ffi-init))
  (check-true (tree-member? '(ref.func $js-many) ffi-init))

  (define invoker-forms
    (procedure-primitive-invoker-forms '(0 31)))
  (check-equal? (map second invoker-forms)
                '($primitive-invoke:shape-0
                  $primitive-invoke:shape-31))
  (check-true
   (tree-member?
    '(return_call_ref $Prim0
                      (ref.cast (ref $Prim0) (local.get $code)))
    (first invoker-forms)))
  (check-true
   (tree-member?
    '(call $rest-arguments->args
           (local.get $args)
           (i32.const 4))
    (second invoker-forms)))
  (check-true
   (tree-member?
    '(call $rest-arguments->list
           (local.get $args)
           (i32.const 2))
    (primitive-invoker 32)))
  (check-exn exn:fail?
             (lambda ()
               (primitive-invoker-tail 99)))

  (check-equal?
   (map form-name
        (procedure-rest-argument-core-runtime-helper-forms procedure-core-helper-index))
   procedure-rest-argument-core-runtime-helper-names)

  (check-equal?
   (map form-name
        (procedure-primitive-error-core-runtime-helper-forms procedure-core-helper-index))
   procedure-primitive-error-core-runtime-helper-names)

  (check-equal?
   (map form-name
        (procedure-invocation-error-core-runtime-helper-forms procedure-core-helper-index))
   procedure-invocation-error-core-runtime-helper-names)

  (check-equal?
   (map form-name
        (procedure-invoker-core-runtime-helper-forms procedure-core-helper-index))
   procedure-invoker-core-runtime-helper-names)

  (check-equal?
   (map form-name
        (procedure-case-lambda-core-runtime-helper-forms procedure-core-helper-index))
   procedure-case-lambda-core-runtime-helper-names)

  (check-equal?
   (map form-name
        (procedure-primitive-dispatch-core-runtime-helper-forms procedure-core-helper-index))
   primitive-dispatch-core-runtime-helper-names)

  (check-false
   (tree-member? '(call $debug-invoke-tick)
                 (first (procedure-primitive-dispatch-core-runtime-helper-forms
                         procedure-core-helper-index))))
  (check-false
   (tree-member? '(global.set $debug-current-procedure (local.get $proc))
                 (first (procedure-primitive-dispatch-core-runtime-helper-forms
                         procedure-core-helper-index))))

  (define invoke-closure-form
    (core-runtime-helper-form procedure-core-helper-index '$invoke-closure))
  (check-false
   (tree-member? '(call $debug-invoke-tick)
                 invoke-closure-form))
  (check-false
   (tree-member? '(global.set $debug-current-procedure (local.get $proc))
                 invoke-closure-form))
  (check-true
   (tree-member? '(call $raise-arity-mismatch/proc
                        (local.get $clos)
                        (local.get $arg-count))
                 invoke-closure-form))

  (check-equal?
   (map form-name
        (procedure-primitive-invoker-core-runtime-helper-forms procedure-core-helper-index))
   (for/list ([shape (in-list primitive-shapes)])
     (string->symbol (format "$~a" (shape->invoker shape)))))

  (check-equal?
   (map form-name
        (procedure-bootstrap-runtime-helper-forms procedure-helper-index))
   (append procedure-invocation-error-runtime-only-helper-names
           procedure-application-error-runtime-only-helper-names
           procedure-invoker-runtime-only-helper-names
           primitive-invocation-error-runtime-only-helper-names
           primitive-dispatch-runtime-only-helper-names))

  (check-equal?
   (map form-name
        (procedure-post-primitive-invoker-runtime-helper-forms procedure-helper-index))
   procedure-case-lambda-runtime-only-helper-names)

  (check-equal?
   (map form-name
        (procedure-main-runtime-helper-forms procedure-helper-index))
   (append procedure-basic-runtime-only-helper-names
           procedure-arity-at-least-runtime-only-helper-names
           procedure-arity-field-runtime-only-helper-names
           procedure-composition-runtime-only-helper-names
           procedure-reduction-runtime-only-helper-names
           procedure-arity-runtime-only-helper-names
           procedure-result-arity-runtime-only-helper-names))

  (check-equal?
   (map form-name
        (procedure-arity-mask-runtime-helper-forms procedure-helper-index))
   procedure-arity-mask-runtime-only-helper-names)

  (check-equal?
   (map form-name
        (primitive-reflection-runtime-helper-forms procedure-helper-index))
   primitive-reflection-runtime-only-helper-names))
