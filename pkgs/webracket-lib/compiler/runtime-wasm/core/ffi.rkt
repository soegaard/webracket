#lang racket

(provide external-value-core-runtime-helpers
         external-value-core-runtime-helper-forms
         external-value-runtime-only-helper-names
         external-value-runtime-helper-forms
         ffi-callback-global-forms
         ffi-callback-global-names
         ffi-core-runtime-helper-forms
         ffi-core-runtime-helpers
         ffi-debug-global-forms
         ffi-debug-global-names
         ffi-entry-global-forms
         ffi-result-global-forms
         ffi-result-global-names
         ffi-runtime-string-constants
         ffi-runtime-symbol-constants
         ffi-console-bridge-helper-names
         ffi-console-bridge-helper-forms
         ffi-runtime-only-helpers
         ffi-runtime-only-helper-names
         ffi-runtime-only-helper-forms
         unsafe-ffi-primitive-table-global-forms
         unsafe-ffi-runtime-only-helper-forms)

(require "../core-helper.rkt"
         (only-in "../../private/wasm-utils.rkt"
                  $
                  Var
                  prim:)
         (only-in "../../private/structs.rkt"
                  foreign-racket-name))

;; current-ffi-console-bridge-bindings : (parameter/c list?)
;;   Holds the program-specific console bridge binding metadata while rendering FFI helpers.
(define current-ffi-console-bridge-bindings
  (make-parameter '()))

;; console-bridge-symbol-global : symbol? -> symbol?
;;   Return the Wasm global name for a console bridge binding symbol.
(define (console-bridge-symbol-global const-name)
  ($ (string->symbol (~a "symbol:" const-name))))

;; ffi-runtime-symbol-constants : (listof symbol?)
;;   Declare symbols used by FFI debug probes and callback/bridge diagnostics.
(define ffi-runtime-symbol-constants
  '(webracket-debug-allocation-budget!
    unsafe-ffi-primitive
    procedure->external
    wr-ref
    webracket-debug-allocation-counters
    webracket-debug-invoke-budget!
    allocation-budget-exhausted
    invoke-budget-exhausted
    pairs
    vectors
    structs
    hash-sets
    closures
    arg-arrays
    strings
    bytes
    value-arrays
    free-arrays
    invocations
    current-procedure
    call-with-parameterization-wrapper-entry
    argc
    arg0-parameterization?
    arg1-procedure?
    external-number->flonum
    external-string->string))

;; ffi-runtime-string-constants : (listof (list/c symbol? string?))
;;   Declare strings used by callback return diagnostics and the console bridge.
(define ffi-runtime-string-constants
  '((callback:no-js-equivalent
     "The callback attempted to return a WebRacket value with no JavaScript equivalent (i.e. without a FASL encoding): ")
    (unsafe-ffi-primitive:expected "registered unsafe FFI primitive symbol?")
    (external?                    "external?")
    (string-or-symbol?         "(or/c string? symbol?)")
    (wr-kind:value              "value")
    (wr-kind:missing-binding    "missing-binding")
    (wr-kind:not-procedure      "not-procedure")
    (wr-kind:procedure          "procedure")
    (wr-kind:exception          "exception")
    (wr-kind:non-fasl-result    "non-fasl-result")
    (wr-kind:bad-request        "bad-request")
    (wr:bad-request             "invalid WebRacket console bridge request")
    (wr:not-procedure           "top-level binding is not a procedure: ")
    (wr:undefined-exception     "bridge call raised #<undefined>")
    (wr:unsafe-undefined-exception "bridge call raised #<unsafe-undefined>")))

;; ffi-callback-global-forms : (listof datum?)
;;   Declare callback bridge globals used by FFI helpers.
(define ffi-callback-global-forms
  '(
         ;; Callback registry
         (global $callback-registry (ref $GrowableArray)
                 (struct.new $GrowableArray
                             (array.new $Array (global.get $false) (i32.const 4))
                             (i32.const 4)
                             (i32.const 0)))))

;; ffi-callback-global-names : (listof symbol?)
;;   Enumerate callback bridge globals shared by split runtime modules.
(define ffi-callback-global-names
  (for/list ([form (in-list ffi-callback-global-forms)])
    (match form
      [`(global ,name . ,_) name]
      [_ (error 'ffi-callback-global-names
                "expected global form, got: ~s"
                form)])))

;; ffi-result-global-forms : (listof datum?)
;;   Declare the program entry result bytes global.
(define ffi-result-global-forms
  '(
               (global $result-bytes             (mut (ref eq)) (ref.i31 (i32.const 0)))))

;; ffi-result-global-names : (listof symbol?)
;;   Enumerate FFI result globals shared by split runtime modules.
(define ffi-result-global-names
  (for/list ([form (in-list ffi-result-global-forms)])
    (match form
      [`(global ,name . ,_) name]
      [_ (error 'ffi-result-global-names
                "expected global form, got: ~s"
                form)])))

;; ffi-debug-global-forms : (listof datum?)
;;   Declare probe-only FFI debug globals shared by split runtime and program modules.
(define ffi-debug-global-forms
  '(
               (global $debug-allocation-budget        (mut i32)      (i32.const 0))
               (global $debug-allocation-pairs         (mut i32)      (i32.const 0))
               (global $debug-allocation-vectors       (mut i32)      (i32.const 0))
               (global $debug-allocation-structs       (mut i32)      (i32.const 0))
               (global $debug-allocation-hash-sets     (mut i32)      (i32.const 0))
               (global $debug-allocation-closures      (mut i32)      (i32.const 0))
               (global $debug-allocation-arg-arrays    (mut i32)      (i32.const 0))
               (global $debug-allocation-strings       (mut i32)      (i32.const 0))
               (global $debug-allocation-bytes         (mut i32)      (i32.const 0))
               (global $debug-allocation-value-arrays  (mut i32)      (i32.const 0))
               (global $debug-allocation-free-arrays   (mut i32)      (i32.const 0))
               (global $debug-invoke-budget            (mut i32)      (i32.const 0))
               (global $debug-invocations              (mut i32)      (i32.const 0))
               (global $debug-label102683-count        (mut i32)      (i32.const 0))
               (global $debug-current-procedure        (mut (ref eq)) (global.get $false))
               (global $debug-current-procedure-name   (mut (ref eq)) (global.get $false))))

;; ffi-debug-global-names : (listof symbol?)
;;   Enumerate FFI debug globals that split-core program modules import from core.
(define ffi-debug-global-names
  (for/list ([form (in-list ffi-debug-global-forms)])
    (match form
      [`(global ,name . ,_) name]
      [_ (error 'ffi-debug-global-names
                "expected global form, got: ~s"
                form)])))

;; ffi-entry-global-forms : (listof datum?)
;;   Declare entry result and probe-only FFI debug globals for monolithic output.
(define ffi-entry-global-forms
  (append ffi-result-global-forms
          ffi-debug-global-forms))

;; unsafe-ffi-primitive-table-global-forms : (listof datum?)
;;   Declare the generated unsafe FFI primitive table.
(define unsafe-ffi-primitive-table-global-forms
  '(
         ;; Symbol -> primitive procedure table for generated unsafe FFI modules.
         (global $unsafe-ffi-primitive-table (mut (ref null $HashEqMutable)) (ref.null $HashEqMutable))))

;; ffi-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for FFI, callback, and browser bridge helpers.
(define ffi-helper-names
  '($js-log
    $webracket-debug-allocation-counters
    $webracket-debug-allocation-budget!
    $webracket-debug-invoke-budget!
    $debug-watch-label102683
    $debug-allocation-tick
    $debug-invoke-tick
    $copy-bytes-to-memory
    $memory-range->immutable-bytes
    $linear-memory-range-available?
    $copy-bytes-to-callback-buffer
    $callback-result->bridge-payload
    $callback-result->bridge-vector
    $callback-non-fasl-return-message
    $wr-result->bridge-vector
    $wr-copy-result
    $wr-bridge-error-message
    $wr-success
    $wr-success/no-display
    $wr-procedure-info
    $wr-procedure-success
    $wr-procedure-success/no-display
    $wr-failure
    $wr-binding-value
    $wr-top-level-names
    $wr-top-level-names-detailed
    $wr-name->symbol
    $wr-vector->args
    $wr-ref/internal
    $wr-ref
    $wr-value-ref
    $wr-call/internal
    $wr-call
    $wr-value-call
    $wr-names
    $wr-names-detailed
    $wr-format
    $callback-register
    $callback
    $callback-accepts-argc
    $callback-expected-arity
    $callback-name
    $callback-debug-id
    $procedure->external))

;; ffi-core-runtime-helper-names : (listof symbol?)
;;   Name FFI helpers available in the standalone core runtime.
(define ffi-core-runtime-helper-names
  '($js-log
    $webracket-debug-allocation-counters
    $debug-allocation-tick
    $memory-range->immutable-bytes
    $copy-bytes-to-callback-buffer
    $callback-result->bridge-payload
    $callback-result->bridge-vector
    $callback-non-fasl-return-message
    $wr-result->bridge-vector
    $wr-copy-result
    $wr-failure
    $callback-register
    $callback
    $callback-accepts-argc
    $callback-expected-arity
    $callback-name
    $callback-debug-id))

;; ffi-console-bridge-helper-names : (listof symbol?)
;;   Name the program-specific console bridge helpers.
(define ffi-console-bridge-helper-names
  '($wr-bridge-error-message
    $wr-success
    $wr-success/no-display
    $wr-procedure-info
    $wr-procedure-success
    $wr-procedure-success/no-display
    $wr-binding-value
    $wr-top-level-names
    $wr-top-level-names-detailed
    $wr-name->symbol
    $wr-vector->args
    $wr-ref/internal
    $wr-ref
    $wr-value-ref
    $wr-call/internal
    $wr-call
    $wr-value-call
    $wr-names
    $wr-names-detailed
    $wr-format))

;; ffi-runtime-only-helper-names : (listof symbol?)
;;   Preserve the program/runtime-local FFI helper emission order.
(define ffi-runtime-only-helper-names
  (let ([core-name-set
         (for/hasheq ([name (in-list ffi-core-runtime-helper-names)])
           (values name #t))]
        [console-bridge-name-set
         (for/hasheq ([name (in-list ffi-console-bridge-helper-names)])
           (values name #t))])
    (for/list ([name (in-list ffi-helper-names)]
               #:unless (or (hash-ref core-name-set name #f)
                            (hash-ref console-bridge-name-set name #f)))
      name)))

;; external-value-all-runtime-helper-names : (listof symbol?)
;;   Preserve the full runtime emission order for external host value helpers.
(define external-value-all-runtime-helper-names
  '($external?
    $external-null?
    $external-number->flonum
    $external-string->string))

;; external-value-core-runtime-helper-names : (listof symbol?)
;;   Name external host value helpers available in the standalone core runtime.
(define external-value-core-runtime-helper-names
  '($external?
    $external-null?))

;; external-value-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime-only external host value helper emission order.
(define external-value-runtime-only-helper-names
  (remove* external-value-core-runtime-helper-names
           external-value-all-runtime-helper-names))

;; external-value-all-runtime-helpers : (listof core-runtime-helper?)
;;   Register all external host value helpers.
(define external-value-all-runtime-helpers
  (function-forms->runtime-helpers
   'external-value-all-runtime-helpers
   '(
     (func $external? (type $Prim1)
           (param $v (ref eq))
           (result   (ref eq))
           (if (result (ref eq))
               (ref.test (ref $External) (local.get $v))
               (then (global.get $true))
               (else (global.get $false))))

     (func $external-null? (param $v (ref eq)) (result (ref eq))
           (if (result (ref eq))
               (ref.test (ref $External) (local.get $v))
               (then (if (result (ref eq))
                         (ref.is_null (struct.get $External $v (ref.cast (ref $External) (local.get $v))))
                         (then (global.get $true))
                         (else (global.get $false))))
               (else (global.get $false))))

     (func $external-number->flonum (type $Prim1)
           (param $v (ref eq))
           (result   (ref eq))

           (local $e   (ref $External))
           (local $raw externref)
           (local $f   f64)
           ;; Check that $v is an $External
           (if (i32.eqz (ref.test (ref $External) (local.get $v)))
               (then (call $raise-argument-error1
                           (global.get $symbol:external-number->flonum)
                           (global.get $string:external?)
                           (local.get $v))
                     (unreachable)))
           ;; Cast to $External
           (local.set $e (ref.cast (ref $External) (local.get $v)))
           ;; Extract underlying JS value
           (local.set $raw (struct.get $External $v (local.get $e)))
           ;; If null, return #f
           (if (ref.is_null (local.get $raw))
               (then (return (global.get $false))))
           ;; Non-null now: pass as (ref extern) directly
           (local.set $f (call $js-external-number->f64
                               (ref.as_non_null (local.get $raw))))
           (struct.new $Flonum (i32.const 0) (local.get $f)))

     (func $external-string->string (type $Prim1)
           (param $v (ref eq))
           (result   (ref eq))

           (local $e   (ref $External))
           (local $raw externref)
           (local $ptr i32)
           ;; Check that $v is an $External
           (if (i32.eqz (ref.test (ref $External) (local.get $v)))
               (then (call $raise-argument-error1
                           (global.get $symbol:external-string->string)
                           (global.get $string:external?)
                           (local.get $v))
                     (unreachable)))
           ;; Cast to $External
           (local.set $e (ref.cast (ref $External) (local.get $v)))
           ;; Extract underlying JS value
           (local.set $raw (struct.get $External $v (local.get $e)))
           ;; If null, return #f
           (if (ref.is_null (local.get $raw))
               (then (return (global.get $false))))
           ;; Non-null now: pass as (ref extern) directly
           (local.set $ptr (call $js-external-string->string
                                  (ref.as_non_null (local.get $raw))))
           (call $linear-memory->string (local.get $ptr)))
     )))

;; external-value-core-runtime-helpers : (listof core-runtime-helper?)
;;   Register external host value helpers available in the standalone core runtime.
(define external-value-core-runtime-helpers
  (select-runtime-helpers external-value-all-runtime-helpers
                          external-value-core-runtime-helper-names))

;; external-value-runtime-only-helpers : (listof core-runtime-helper?)
;;   Register external host value helpers that still depend on monolithic runtime definitions.
(define external-value-runtime-only-helpers
  (remove-runtime-helpers external-value-all-runtime-helpers
                          external-value-core-runtime-helper-names))

;;;
;;; Browser external-value sketch
;;;

#;(type $External
        (sub $Heap
             (struct
               (field $hash (mut i32))
               (field $v    (ref null extern)))))

;; Moved up to import section
#;(func $js_document_body
        (import "ffi" "js_document_body")
        (result externref))

#;(func $js-document-body
        (result (ref eq))
        (struct.new $External
                    (i32.const 0)
                    (call $js-document-body/imported)))

#;(func $js-make-text-node
        (result (ref eq))
        (struct.new $External
                    (i32.const 0)
                    (call $js_make_text_node)))

;; ffi-runtime-only-forms : -> (listof datum?)
;;   Enumerate FFI, callback, and browser bridge helpers used by the monolithic runtime.
(define (ffi-runtime-only-forms)
  (define console-bridge-bindings (current-ffi-console-bridge-bindings))
  `(
         ;;;
         ;;; FFI Helpers
         ;;;

         (func $js-log (type $Prim1)
               (param $v (ref eq))
               (result   (ref eq))

               (local $len i32)

               (global.set $result-bytes
                           (call $s-exp->fasl
                                 ;; Preserve native JavaScript logging for
                                 ;; atomic values, but turn pairs into Racket
                                 ;; write notation. FASL otherwise reconstructs
                                 ;; pairs as host implementation objects.
                                 (if (result (ref eq))
                                     (i32.or
                                      (ref.test (ref $Pair) (local.get $v))
                                      (ref.test (ref $MPair) (local.get $v)))
                                     (then (call $format/write (local.get $v)))
                                     (else (local.get $v)))
                                 (global.get $false)))
               #;(local.set $len (call $copy_bytes_to_memory (global.get $memory-map:callback-buffer-base)))
               (local.set $len
                          (call $copy-bytes-to-callback-buffer
                                (global.get $result-bytes)))
               (call $js_print_fasl
                     (global.get $memory-map:callback-buffer-base)
                     (local.get $len))
               (global.get $void))

         ;; webracket-debug-allocation-counters : -> list?
         ;;   Return probe-only allocation counters for the currently selected hot paths.
         (func $webracket-debug-allocation-counters (type $Prim0)
               (result (ref eq))
               (call $cons
                     (call $cons (global.get $symbol:pairs)
                           (call $cons (ref.i31 (i32.shl (global.get $debug-allocation-pairs) (i32.const 1)))
                                 (global.get $null)))
                     (call $cons
                           (call $cons (global.get $symbol:vectors)
                                 (call $cons (ref.i31 (i32.shl (global.get $debug-allocation-vectors) (i32.const 1)))
                                       (global.get $null)))
                           (call $cons
                                 (call $cons (global.get $symbol:structs)
                                       (call $cons (ref.i31 (i32.shl (global.get $debug-allocation-structs) (i32.const 1)))
                                             (global.get $null)))
                                 (call $cons
                                       (call $cons (global.get $symbol:hash-sets)
                                             (call $cons (ref.i31 (i32.shl (global.get $debug-allocation-hash-sets) (i32.const 1)))
                                                   (global.get $null)))
                                       (call $cons
                                             (call $cons (global.get $symbol:closures)
                                                   (call $cons (ref.i31 (i32.shl (global.get $debug-allocation-closures) (i32.const 1)))
                                                         (global.get $null)))
                                             (call $cons
                                                   (call $cons (global.get $symbol:arg-arrays)
                                                         (call $cons (ref.i31 (i32.shl (global.get $debug-allocation-arg-arrays) (i32.const 1)))
                                                               (global.get $null)))
                                                   (call $cons
                                                         (call $cons (global.get $symbol:strings)
                                                               (call $cons (ref.i31 (i32.shl (global.get $debug-allocation-strings) (i32.const 1)))
                                                                     (global.get $null)))
                                                         (call $cons
                                                               (call $cons (global.get $symbol:bytes)
                                                                 (call $cons (ref.i31 (i32.shl (global.get $debug-allocation-bytes) (i32.const 1)))
                                                                       (global.get $null)))
                                                               (call $cons
                                                                     (call $cons (global.get $symbol:value-arrays)
                                                                           (call $cons (ref.i31 (i32.shl (global.get $debug-allocation-value-arrays) (i32.const 1)))
                                                                                 (global.get $null)))
                                                                     (call $cons
                                                                           (call $cons (global.get $symbol:free-arrays)
                                                                                 (call $cons (ref.i31 (i32.shl (global.get $debug-allocation-free-arrays) (i32.const 1)))
                                                                                       (global.get $null)))
                                                                           (call $cons
                                                                                 (call $cons (global.get $symbol:invocations)
                                                                                       (call $cons (ref.i31 (i32.shl (global.get $debug-invocations) (i32.const 1)))
                                                                                             (global.get $null)))
                                                                                 (global.get $null)))))))))))))

         ;; webracket-debug-allocation-budget! : exact-nonnegative-integer? -> void?
         ;;   Enable a probe-only allocation budget and reset hot-path counters.
         (func $webracket-debug-allocation-budget! (type $Prim1)
               (param $budget-fx (ref eq)) ;; exact nonnegative fixnum budget
               (result (ref eq))
               (if (i32.eqz (ref.test (ref i31) (local.get $budget-fx)))
                   (then (call $raise-check-fixnum (local.get $budget-fx))
                         (unreachable)))
               (global.set $debug-allocation-budget
                           (i32.shr_u
                            (i31.get_u (ref.cast (ref i31) (local.get $budget-fx)))
                            (i32.const 1)))
               (global.set $debug-allocation-pairs (i32.const 0))
               (global.set $debug-allocation-vectors (i32.const 0))
               (global.set $debug-allocation-structs (i32.const 0))
               (global.set $debug-allocation-hash-sets (i32.const 0))
               (global.set $debug-allocation-closures (i32.const 0))
               (global.set $debug-allocation-arg-arrays (i32.const 0))
               (global.set $debug-allocation-strings (i32.const 0))
               (global.set $debug-allocation-bytes (i32.const 0))
               (global.set $debug-allocation-value-arrays (i32.const 0))
               (global.set $debug-allocation-free-arrays (i32.const 0))
               (global.get $void))

         ;; webracket-debug-invoke-budget! : exact-nonnegative-integer? -> void?
         ;;   Enable a probe-only invocation watchdog and reset its counter.
         (func $webracket-debug-invoke-budget! (type $Prim1)
               (param $budget-fx (ref eq)) ;; exact nonnegative fixnum budget
               (result (ref eq))
               (if (i32.eqz (ref.test (ref i31) (local.get $budget-fx)))
                   (then (call $raise-check-fixnum (local.get $budget-fx))
                         (unreachable)))
               (global.set $debug-invoke-budget
                           (i32.shr_u
                            (i31.get_u (ref.cast (ref i31) (local.get $budget-fx)))
                            (i32.const 1)))
               (global.set $debug-invocations (i32.const 0))
               (global.set $debug-label102683-count (i32.const 0))
               (global.get $void))

         ;; debug-watch-label102683 : args-array? -> void?
         ;;   Probe the lowered call-with-parameterization wrapper entry.
         (func $debug-watch-label102683
               (param $args (ref $Args))
               (local $argc i32)
               (local $arg0 (ref eq))
               (local $arg1 (ref eq))
               (local $arg1-name (ref eq))
               (local $arg1-debug-id (ref eq))
               (local $arg0-paramz? (ref eq))
               (local $arg1-proc? (ref eq))
               (if (i32.gt_u (global.get $debug-invoke-budget) (i32.const 0))
                   (then
                    (global.set $debug-label102683-count
                                (i32.add (global.get $debug-label102683-count) (i32.const 1)))
                    (if (i32.le_u (global.get $debug-label102683-count) (i32.const 12))
                        (then
                         (local.set $argc (array.len (local.get $args)))
                         (local.set $arg0 (global.get $false))
                         (local.set $arg1 (global.get $false))
                         (local.set $arg1-name (global.get $false))
                         (local.set $arg1-debug-id (global.get $false))
                         (if (i32.gt_u (local.get $argc) (i32.const 0))
                             (then (local.set $arg0 (array.get $Args (local.get $args) (i32.const 0)))))
                         (if (i32.gt_u (local.get $argc) (i32.const 1))
                             (then (local.set $arg1 (array.get $Args (local.get $args) (i32.const 1)))))
                         (local.set $arg0-paramz?
                                    (if (result (ref eq))
                                        (ref.test (ref $Parameterization) (local.get $arg0))
                                        (then (global.get $true))
                                        (else (global.get $false))))
                         (local.set $arg1-proc? (call $procedure? (local.get $arg1)))
                         (if (ref.test (ref $Procedure) (local.get $arg1))
                             (then
                              (local.set $arg1-name
                                         (struct.get $Procedure $name
                                                     (ref.cast (ref $Procedure) (local.get $arg1))))))
                         (if (ref.test (ref $Closure) (local.get $arg1))
                             (then
                              (local.set $arg1-debug-id
                                         (struct.get $Closure $debug-id
                                                     (ref.cast (ref $Closure) (local.get $arg1))))))
                         (drop (call $js-log (global.get $symbol:call-with-parameterization-wrapper-entry)))
                         (drop (call $js-log
                                     (call $cons
                                           (call $cons (global.get $symbol:count)
                                                 (call $cons
                                                       (ref.i31 (i32.shl (global.get $debug-label102683-count)
                                                                         (i32.const 1)))
                                                       (global.get $null)))
                                           (global.get $null))))
                         (drop (call $js-log
                                     (call $cons
                                           (call $cons (global.get $symbol:argc)
                                                 (call $cons
                                                       (ref.i31 (i32.shl (local.get $argc) (i32.const 1)))
                                                       (global.get $null)))
                                           (global.get $null))))
                         (drop (call $js-log
                                     (call $cons
                                           (call $cons (global.get $symbol:arg0-parameterization?)
                                                 (call $cons (local.get $arg0-paramz?)
                                                       (global.get $null)))
                                           (global.get $null))))
                         (drop (call $js-log
                                     (call $cons
                                           (call $cons (global.get $symbol:arg1-procedure?)
                                                 (call $cons (local.get $arg1-proc?)
                                                       (global.get $null)))
                                           (global.get $null))))
                         (drop (call $js-log
                                     (call $cons
                                           (call $cons (global.get $symbol:arg1-name)
                                                 (call $cons (local.get $arg1-name)
                                                       (global.get $null)))
                                           (global.get $null))))
                         (drop (call $js-log
                                     (call $cons
                                           (call $cons (global.get $symbol:arg1-debug-id)
                                                 (call $cons (local.get $arg1-debug-id)
                                                       (global.get $null)))
                                           (global.get $null)))))))))

         ;; debug-allocation-tick : i32 -> void
         ;;   Count selected allocations while an explicit probe budget is active.
         ;;   This core-safe helper traps when the budget is exhausted. Runtime-only
         ;;   debug wrappers can format/log counters before enabling the budget, but
         ;;   the tick itself cannot depend on JS/FASL logging and remain movable.
         (func $debug-allocation-tick
               (param $kind i32)
               (if (i32.gt_u (global.get $debug-allocation-budget) (i32.const 0))
                   (then
                    (if (i32.eq (local.get $kind) (i32.const 0))
                        (then (global.set $debug-allocation-pairs
                                          (i32.add (global.get $debug-allocation-pairs) (i32.const 1)))))
                    (if (i32.eq (local.get $kind) (i32.const 1))
                        (then (global.set $debug-allocation-vectors
                                          (i32.add (global.get $debug-allocation-vectors) (i32.const 1)))))
                    (if (i32.eq (local.get $kind) (i32.const 2))
                        (then (global.set $debug-allocation-structs
                                          (i32.add (global.get $debug-allocation-structs) (i32.const 1)))))
                    (if (i32.eq (local.get $kind) (i32.const 3))
                        (then (global.set $debug-allocation-hash-sets
                                          (i32.add (global.get $debug-allocation-hash-sets) (i32.const 1)))))
                    (if (i32.eq (local.get $kind) (i32.const 4))
                        (then (global.set $debug-allocation-closures
                                          (i32.add (global.get $debug-allocation-closures) (i32.const 1)))))
                    (if (i32.eq (local.get $kind) (i32.const 5))
                        (then (global.set $debug-allocation-arg-arrays
                                          (i32.add (global.get $debug-allocation-arg-arrays) (i32.const 1)))))
                    (if (i32.eq (local.get $kind) (i32.const 6))
                        (then (global.set $debug-allocation-strings
                                          (i32.add (global.get $debug-allocation-strings) (i32.const 1)))))
                    (if (i32.eq (local.get $kind) (i32.const 7))
                        (then (global.set $debug-allocation-bytes
                                          (i32.add (global.get $debug-allocation-bytes) (i32.const 1)))))
                    (if (i32.eq (local.get $kind) (i32.const 8))
                        (then (global.set $debug-allocation-value-arrays
                                          (i32.add (global.get $debug-allocation-value-arrays) (i32.const 1)))))
                    (if (i32.eq (local.get $kind) (i32.const 9))
                        (then (global.set $debug-allocation-free-arrays
                                          (i32.add (global.get $debug-allocation-free-arrays) (i32.const 1)))))
                    (global.set $debug-allocation-budget
                                (i32.sub (global.get $debug-allocation-budget) (i32.const 1)))
                    (if (i32.eqz (global.get $debug-allocation-budget))
                        (then (unreachable))))))

         ;; debug-invoke-tick : -> void
         ;;   Count procedure invocations while an explicit probe budget is active.
         (func $debug-invoke-tick
               (local $proc (ref eq))
               (local $name (ref eq))
               (local $debug-id (ref eq))
               (if (i32.gt_u (global.get $debug-invoke-budget) (i32.const 0))
                   (then
                    (global.set $debug-invocations
                                (i32.add (global.get $debug-invocations) (i32.const 1)))
                    (global.set $debug-invoke-budget
                                (i32.sub (global.get $debug-invoke-budget) (i32.const 1)))
                    (if (i32.eqz (global.get $debug-invoke-budget))
                        (then
                         (local.set $proc (global.get $debug-current-procedure))
                         (local.set $name (global.get $debug-current-procedure-name))
                         (local.set $debug-id (global.get $false))
                         (if (ref.test (ref $Procedure) (local.get $proc))
                             (then
                              (local.set $name
                                         (struct.get $Procedure $name
                                                     (ref.cast (ref $Procedure) (local.get $proc))))))
                         (if (ref.test (ref $Closure) (local.get $proc))
                             (then
                              (local.set $debug-id
                                         (struct.get $Closure $debug-id
                                                     (ref.cast (ref $Closure) (local.get $proc))))))
                         (drop (call $js-log (global.get $symbol:invoke-budget-exhausted)))
                         (drop (call $js-log
                                     (call $cons
                                           (call $cons (global.get $symbol:current-procedure)
                                                 (call $cons (local.get $proc)
                                                       (global.get $null)))
                                           (global.get $null))))
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
                         (drop (call $js-log (call $webracket-debug-allocation-counters)))
                         (unreachable))))))

         (func $copy-bytes-to-memory
               ;; Copy a Racket $Bytes object into linear memory at $ptr.
               (param $bs-any (ref eq))  ;; source: expected (ref $Bytes)
               (param $ptr    i32)       ;; destination address in linear memory
               (result        i32)       ;; number of bytes copied

               (local $bs  (ref $Bytes))
               (local $arr (ref $I8Array))
               (local $len i32)
               (local $i   i32)
               (local $val i32)

               ;; 1) Type-check: ensure $bs-any is a $Bytes
               (if (i32.eqz (ref.test (ref $Bytes) (local.get $bs-any)))
                   (then (call $raise-expected-bytes (local.get $bs-any))
                         (unreachable)))
               (local.set $bs (ref.cast (ref $Bytes) (local.get $bs-any)))
               ;; 2) Get backing array and its length
               (local.set $arr (struct.get $Bytes $bs (local.get $bs)))
               (local.set $len (array.len (local.get $arr)))
               ;; 3) Copy loop
               (local.set $i (i32.const 0))
               (block $done
                      (loop $copy
                            (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
                            (local.set $val (array.get_u $I8Array (local.get $arr) (local.get $i)))
                            (i32.store8
                             (i32.add (local.get $ptr) (local.get $i))
                             (local.get $val))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $copy)))
               ;; 4) Return number of bytes copied
               (local.get $len))

         (func $memory-range->immutable-bytes
               ;; Copy linear memory into an immutable byte string.
               (param $ptr i32) ;; source address in linear memory
               (param $len i32) ;; byte count
               (result (ref $Bytes))

               (local $arr (ref $I8Array))
               (local $i   i32)

               (local.set $arr (array.new_default $I8Array (local.get $len)))
               (local.set $i (i32.const 0))
               (block $done
                      (loop $copy
                            (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
                            (array.set $I8Array
                                       (local.get $arr)
                                       (local.get $i)
                                       (i32.load8_u
                                        (i32.add (local.get $ptr) (local.get $i))))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $copy)))
               (struct.new $Bytes
                           (i32.const 0)  ;; hash
                           (i32.const 1)  ;; immutable
                           (local.get $arr)))

         (func $linear-memory-range-available?
               ;; Check whether ptr..ptr+len is inside the current memory.
               (param $ptr i32)
               (param $len i32)
               (result i32)

               (local $mem-bytes i32)
               (local $end       i32)

               (local.set $mem-bytes (i32.mul (memory.size) (i32.const 65536)))
               (local.set $end (i32.add (local.get $ptr) (local.get $len)))
               (if (i32.lt_u (local.get $end) (local.get $ptr))
                   (then (return (i32.const 0))))
               (i32.le_u (local.get $end) (local.get $mem-bytes)))

         (func $copy-bytes-to-callback-buffer
               ;; Copy encoded callback/FASL bytes into their static region.
               (param $bs-any (ref eq))
               (result i32)

               (local $bs  (ref $Bytes))
               (local $len i32)

               (if (i32.eqz (ref.test (ref $Bytes) (local.get $bs-any)))
                   (then (call $raise-expected-bytes (local.get $bs-any))
                         (unreachable)))
               (local.set $bs (ref.cast (ref $Bytes) (local.get $bs-any)))
               (local.set $len (array.len (struct.get $Bytes $bs (local.get $bs))))
               (if (i32.gt_u (local.get $len)
                             (global.get $memory-map:callback-buffer-length))
                   (then (call $raise-string-buffer-overflow)
                         (unreachable)))
               (if (i32.eqz
                    (call $linear-memory-range-available?
                          (global.get $memory-map:callback-buffer-base)
                          (global.get $memory-map:callback-buffer-length)))
                   (then (call $raise-string-buffer-overflow)
                         (unreachable)))
               (call $copy-bytes-to-memory
                     (local.get $bs)
                     (global.get $memory-map:callback-buffer-base)))

         #;(func $copy_bytes_to_memory
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
                            (br_if $done
                                   (i32.ge_u (local.get $i) (local.get $len)))
                            ;; val = arr[i]
                            (local.set $val
                                       (array.get_u $I8Array
                                                    (local.get $arr)
                                                    (local.get $i)))
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
         ;;; CALLBACKS (Calling webracket from js)
         ;;;

         ;; callback-result->bridge-payload : any/c boolean? -> any/c
         ;;   Convert a callback result into a host-serializable bridge payload.
         (func $callback-result->bridge-payload
               (param $v        (ref eq))
               (param $success? i32)
               (result          (ref eq))

               (if (result (ref eq))
                   (local.get $success?)
                   (then (local.get $v))
                   (else
                    (if (result (ref eq))
                        (ref.eq (call $exn? (local.get $v)) (global.get $false))
                        (then (call $format/display (local.get $v)))
                        (else (call $exn-message (local.get $v)))))))

         ;; callback-result->bridge-vector : boolean? any/c -> vector?
         ;;   Build the tagged callback bridge result returned to JS.
         (func $callback-result->bridge-vector
               (param $success? i32)
               (param $payload  (ref eq))
               (result          (ref $Vector))

               (struct.new $Vector
                           (i32.const 0)
                           (i32.const 1)
                           (array.new_fixed $Array 2
                                            (if (result (ref eq))
                                                (local.get $success?)
                                                (then (global.get $true))
                                                (else (global.get $false)))
                                            (local.get $payload))))

         ;; callback-non-fasl-return-message : any/c -> string?
         ;;   Explain that a callback returned a value with no JS/FASL encoding.
         (func $callback-non-fasl-return-message
               (param $v (ref eq))
               (result   (ref $String))

               (local $out (ref $GrowableArray))

               (local.set $out
                          (call $make-growable-array (i32.const 2)))
               (call $growable-array-add!
                     (local.get $out)
                     (global.get $string:callback:no-js-equivalent))
               (call $growable-array-add!
                     (local.get $out)
                     (call $format/display (local.get $v)))
               (call $array-of-strings->string
                     (call $growable-array->array (local.get $out))))

         ;;;
         ;;; Browser Console Bridge (globalThis.WR)
         ;;;

         (func $wr-result->bridge-vector
               (param $success? (ref eq))
               (param $kind     (ref eq))
               (param $value    (ref eq))
               (param $printed  (ref eq))
               (param $error    (ref eq))
               (result          (ref $Vector))

               (struct.new $Vector
                           (i32.const 0)
                           (i32.const 1)
                           (array.new_fixed $Array 5
                                            (local.get $success?)
                                            (local.get $kind)
                                            (local.get $value)
                                            (local.get $printed)
                                            (local.get $error))))

         (func $wr-copy-result
               (param $v (ref eq))
               (result i32)

               (global.set $result-bytes
                           (call $s-exp->fasl (local.get $v) (global.get $false)))
               (call $copy-bytes-to-callback-buffer
                     (ref.cast (ref $Bytes) (global.get $result-bytes))))

         (func $wr-bridge-error-message
               (param $v (ref eq))
               (result (ref eq))

               (if (result (ref eq))
                   (ref.eq (local.get $v) (global.get $undefined))
                   (then (global.get $string:wr:undefined-exception))
                   (else
                    (if (result (ref eq))
                        (ref.eq (local.get $v) (global.get $unsafe-undefined))
                        (then (global.get $string:wr:unsafe-undefined-exception))
                        (else
                         (if (result (ref eq))
                             (ref.eq (call $exn? (local.get $v)) (global.get $false))
                             (then (call $format/display (local.get $v)))
                             (else (call $exn-message (local.get $v)))))))))

         (func $wr-success
               (param $v (ref eq))
               (result (ref $Vector))

               (call $wr-result->bridge-vector
                     (global.get $true)
                     (global.get $string:wr-kind:value)
                     (local.get $v)
                     (call $format/display (local.get $v))
                     (global.get $false)))

         ;; $wr-success/no-display : (ref eq) -> (ref $Vector)
         ;;   Build a successful bridge result without a display representation.
         (func $wr-success/no-display
               (param $v (ref eq))
               (result (ref $Vector))

               ;; `WR.value` does not observe the bridge's `printed` field.
               ;; Avoid allocating and serializing a display representation.
               (call $wr-result->bridge-vector
                     (global.get $true)
                     (global.get $string:wr-kind:value)
                     (local.get $v)
                     (global.get $false)
                     (global.get $false)))

         (func $wr-procedure-info
               (param $proc (ref $Procedure))
               (result (ref $Vector))

               (struct.new $Vector
                           (i32.const 0)
                           (i32.const 1)
                           (array.new_fixed $Array 3
                                            (struct.get $Procedure $name
                                                        (local.get $proc))
                                            (call $procedure-arity->expected-string
                                                  (local.get $proc))
                                            (call $format/display:procedure
                                                  (ref.cast (ref eq)
                                                            (local.get $proc))))))

         (func $wr-procedure-success
               (param $proc (ref $Procedure))
               (result (ref $Vector))

               (call $wr-result->bridge-vector
                     (global.get $true)
                     (global.get $string:wr-kind:procedure)
                     (ref.cast (ref eq)
                               (call $wr-procedure-info (local.get $proc)))
                     (call $format/display:procedure
                           (ref.cast (ref eq) (local.get $proc)))
                     (global.get $false)))

         ;; $wr-procedure-success/no-display : (ref $Procedure) -> (ref $Vector)
         ;;   Build procedure metadata without a duplicate printed representation.
         (func $wr-procedure-success/no-display
               (param $proc (ref $Procedure))
               (result (ref $Vector))

               (call $wr-result->bridge-vector
                     (global.get $true)
                     (global.get $string:wr-kind:procedure)
                     (ref.cast (ref eq)
                               (call $wr-procedure-info (local.get $proc)))
                     (global.get $false)
                     (global.get $false)))

         (func $wr-failure
               (param $kind    (ref eq))
               (param $printed (ref eq))
               (param $error   (ref eq))
               (result (ref $Vector))

               (call $wr-result->bridge-vector
                     (global.get $false)
                     (local.get $kind)
                     (global.get $false)
                     (local.get $printed)
                     (local.get $error)))

         (func $wr-binding-value
               (param $sym (ref $Symbol))
               (result (ref eq) i32)

               ,(let loop ([bindings console-bridge-bindings])
                  (cond
                    [(null? bindings)
                     `(return (global.get $false) (i32.const 0))]
                    [else
                     (match-define (cons binding rest) bindings)
                     (match binding
                       [(list 'top _ const-name x mutable? _ _)
                        `(if (ref.eq (local.get $sym)
                                     (global.get ,(console-bridge-symbol-global const-name)))
                             (then
                              (return
                               ,(if mutable?
                                    `(struct.get $Boxed $v
                                                 (ref.cast (ref $Boxed)
                                                           (struct.get $Boxed $v
                                                                       (ref.cast (ref $Boxed)
                                                                                 (global.get ,(Var x))))))
                                    `(struct.get $Boxed $v
                                                (ref.cast (ref $Boxed)
                                                          (global.get ,(Var x)))))
                               (i32.const 1)))
                             (else ,(loop rest)))]
                       [(list 'module-cell _ const-name x _ _ _)
                        `(if (ref.eq (local.get $sym)
                                     (global.get ,(console-bridge-symbol-global const-name)))
                             (then
                              (return
                               (call $module-variable-ref/no-check
                                     (struct.get $Boxed $v
                                                 (ref.cast (ref $Boxed)
                                                           (global.get ,(Var x)))))
                               (i32.const 1)))
                             (else ,(loop rest)))]
                       [(list 'primitive pr const-name _)
                        `(if (ref.eq (local.get $sym)
                                     (global.get ,(console-bridge-symbol-global const-name)))
                             (then
                              (return (global.get ,($ (prim: pr)))
                                      (i32.const 1)))
                             (else ,(loop rest)))])]))
               (unreachable))

         (func $wr-top-level-names
               (result (ref $Vector))

               (struct.new $Vector
                           (i32.const 0)
                           (i32.const 1)
                           (array.new_fixed $Array
                                            ,(length console-bridge-bindings)
                                            ,@(for/list ([binding (in-list console-bridge-bindings)])
                                                (match binding
                                                  [(list 'top _ const-name _ _ _ _)
                                                   `(call $symbol->string
                                                          (global.get ,(console-bridge-symbol-global const-name)))]
                                                  [(list 'module-cell _ const-name _ _ _ _)
                                                   `(call $symbol->string
                                                          (global.get ,(console-bridge-symbol-global const-name)))]
                                                  [(list 'primitive _ const-name _)
                                                   `(call $symbol->string
                                                          (global.get ,(console-bridge-symbol-global const-name)))])))))

         (func $wr-top-level-names-detailed
               (result (ref $Vector))

               (struct.new $Vector
                           (i32.const 0)
                           (i32.const 1)
                           (array.new_fixed $Array
                                            ,(length console-bridge-bindings)
                                            ,@(for/list ([binding (in-list console-bridge-bindings)])
                                                (match binding
                                                  [(list 'top _ const-name _ mutable? origin-kind-const-name source-path-const-name)
                                                   `(struct.new $Vector
                                                                (i32.const 0)
                                                                (i32.const 1)
                                                                (array.new_fixed $Array 5
                                                                                 (call $symbol->string
                                                                                       (global.get ,(console-bridge-symbol-global const-name)))
                                                                                 (global.get ,($ (string->symbol (~a "string:" origin-kind-const-name))))
                                                                                 ,(if mutable?
                                                                                      `(global.get $true)
                                                                                      `(global.get $false))
                                                                                 (global.get $string:wr-console-bridge-kind)
                                                                                 ,(if source-path-const-name
                                                                                      `(global.get ,($ (string->symbol (~a "string:" source-path-const-name))))
                                                                                      `(global.get $false))))]
                                                  [(list 'module-cell _ const-name _ mutable? origin-kind-const-name source-path-const-name)
                                                   `(struct.new $Vector
                                                                (i32.const 0)
                                                                (i32.const 1)
                                                                (array.new_fixed $Array 5
                                                                                 (call $symbol->string
                                                                                       (global.get ,(console-bridge-symbol-global const-name)))
                                                                                 (global.get ,($ (string->symbol (~a "string:" origin-kind-const-name))))
                                                                                 ,(if mutable?
                                                                                      `(global.get $true)
                                                                                      `(global.get $false))
                                                                                 (global.get $string:wr-console-bridge-kind)
                                                                                 ,(if source-path-const-name
                                                                                      `(global.get ,($ (string->symbol (~a "string:" source-path-const-name))))
                                                                                      `(global.get $false))))])))))

         (func $wr-name->symbol
               (param $name (ref eq))
               (result (ref $Symbol))

               (if (ref.test (ref $Symbol) (local.get $name))
                   (then
                    (return (ref.cast (ref $Symbol) (local.get $name)))))
               (if (ref.test (ref $String) (local.get $name))
                   (then
                    (return (ref.cast (ref $Symbol)
                                      (call $string->symbol
                                            (ref.cast (ref $String) (local.get $name)))))))
               (call $raise-argument-error1
                     (global.get $symbol:wr-ref)
                     (global.get $string:string-or-symbol?)
                     (local.get $name))
               (unreachable))

         (func $wr-vector->args
               (param $vec (ref $Vector))
               (result (ref $Args))

               (local $src (ref $Array))
               (local $dst (ref $Args))
               (local $len i32)
               (local $i   i32)

               (local.set $src (struct.get $Vector $arr (local.get $vec)))
               (local.set $len (array.len (local.get $src)))
               (local.set $dst (array.new $Args (global.get $null) (local.get $len)))
               (local.set $i (i32.const 0))
               (block $done
                      (loop $loop
                            (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
                            (array.set $Args
                                       (local.get $dst)
                                       (local.get $i)
                                       (array.get $Array
                                                  (local.get $src)
                                                  (local.get $i)))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $loop)))
               (local.get $dst))

         ;; $wr-ref/internal : i32 i32 -> i32
         ;;   Look up a bridge binding, optionally retaining its printed form.
         (func $wr-ref/internal
               (param $fasl i32)
               (param $include-printed? i32)
               (result i32)

               (local $name            (ref eq))
               (local $sym             (ref $Symbol))
               (local $val             (ref eq))
               (local $found?          i32)
               (local $bridge          (ref $Vector))
               (local $bridge-exn      (ref eq))
               (local $bridge-sentinel (ref eq))
               (local $len             i32)

               (local.set $name (call $fasl-memory->s-exp (local.get $fasl)))
               (local.set $sym  (call $wr-name->symbol (local.get $name)))
               (call $wr-binding-value (local.get $sym))
               (local.set $found?)
               (local.set $val)

               (local.set $bridge
                          (if (result (ref $Vector))
                              (i32.eqz (local.get $found?))
                              (then
                               (call $wr-failure
                                     (global.get $string:wr-kind:missing-binding)
                                     (global.get $false)
                                     (call $string-append/2
                                           (global.get $string:missing-binding)
                                           (call $symbol->string (local.get $sym)))))
                              (else
                               (if (result (ref $Vector))
                                   (ref.eq (call $procedure? (local.get $val))
                                           (global.get $true))
                                   (then
                                    (if (result (ref $Vector))
                                        (i32.eqz (local.get $include-printed?))
                                        (then (call $wr-procedure-success/no-display
                                                    (ref.cast (ref $Procedure)
                                                              (local.get $val))))
                                        (else (call $wr-procedure-success
                                                   (ref.cast (ref $Procedure)
                                                             (local.get $val))))))
                                   (else
                                    (if (result (ref $Vector))
                                        (i32.eqz (local.get $include-printed?))
                                        (then (call $wr-success/no-display
                                                    (local.get $val)))
                                        (else (call $wr-success
                                                    (local.get $val)))))))))

               (local.set $bridge-sentinel
                          (call $cons (global.get $false) (global.get $false)))
               (local.set $bridge-exn
                          (block $wr-ref-encode-failed (result (ref eq))
                            (try_table (result (ref eq))
                                       (catch $exn $wr-ref-encode-failed)
                                       (global.set $result-bytes
                                                   (call $s-exp->fasl
                                                         (local.get $bridge)
                                                         (global.get $false)))
                                       (local.get $bridge-sentinel))))
               (if (i32.eqz (ref.eq (local.get $bridge-exn) (local.get $bridge-sentinel)))
                   (then
                    (global.set $result-bytes
                                      (call $s-exp->fasl
                                            (call $wr-failure
                                            (global.get $string:wr-kind:non-fasl-result)
                                            (global.get $false)
                                            (call $callback-non-fasl-return-message
                                                  (if (result (ref eq))
                                                      (i32.eqz (local.get $found?))
                                                      (then (local.get $sym))
                                                      (else (local.get $val)))))
                                      (global.get $false)))))
               (local.set $len
                          (call $copy-bytes-to-callback-buffer
                                (ref.cast (ref $Bytes) (global.get $result-bytes))))
               (local.get $len))

         ;; $wr-ref : i32 -> i32
         ;;   Look up a bridge binding with its formatted console representation.
         (func $wr-ref (export "wr-ref")
               (param $fasl i32)
               (result i32)

               (call $wr-ref/internal
                     (local.get $fasl)
                     (i32.const 1)))

         ;; $wr-value-ref : i32 -> i32
         ;;   Look up a bridge binding without producing a discarded display form.
         (func $wr-value-ref (export "wr-value-ref")
               (param $fasl i32)
               (result i32)

               (call $wr-ref/internal
                     (local.get $fasl)
                     (i32.const 0)))

         ;; $wr-call/internal : i32 i32 -> i32
         ;;   Call a bridge binding, optionally retaining its printed form.
         (func $wr-call/internal
               (param $fasl i32)
               (param $include-printed? i32)
               (result i32)

               (local $req             (ref $Vector))
               (local $arr             (ref $Array))
               (local $name            (ref eq))
               (local $args-val        (ref eq))
               (local $sym             (ref $Symbol))
               (local $val             (ref eq))
               (local $found?          i32)
               (local $proc            (ref $Procedure))
               (local $args            (ref $Args))
               (local $res             (ref eq))
               (local $bridge          (ref $Vector))
               (local $bridge-exn      (ref eq))
               (local $bridge-sentinel (ref eq))
               (local $len             i32)

               (local.set $req
                          (ref.cast (ref $Vector)
                                    (call $fasl-memory->s-exp (local.get $fasl))))
               (local.set $arr      (struct.get $Vector $arr (local.get $req)))
               (local.set $name     (array.get $Array (local.get $arr) (i32.const 0)))
               (local.set $args-val (array.get $Array (local.get $arr) (i32.const 1)))
               (local.set $sym      (call $wr-name->symbol (local.get $name)))
               (call $wr-binding-value (local.get $sym))
               (local.set $found?)
               (local.set $val)
               (local.set $res      (global.get $false))

               (local.set $bridge
                          (if (result (ref $Vector))
                              (i32.eqz (local.get $found?))
                              (then
                               (call $wr-failure
                                     (global.get $string:wr-kind:missing-binding)
                                     (global.get $false)
                                     (call $string-append/2
                                           (global.get $string:missing-binding)
                                           (call $symbol->string (local.get $sym)))))
                              (else
                               (if (result (ref $Vector))
                                   (ref.eq (call $procedure? (local.get $val))
                                           (global.get $false))
                                   (then
                                    (call $wr-failure
                                          (global.get $string:wr-kind:not-procedure)
                                          (call $format/display (local.get $val))
                                          (call $string-append/2
                                                (global.get $string:wr:not-procedure)
                                                (call $symbol->string (local.get $sym)))))
                                    (else
                                     (local.set $proc (ref.cast (ref $Procedure) (local.get $val)))
                                     (local.set $args
                                               (call $wr-vector->args
                                                     (ref.cast (ref $Vector)
                                                               (local.get $args-val))))
                                    (local.set $res
                                               (block $wr-call-raised (result (ref eq))
                                                 (try_table (result (ref eq))
                                                            (catch $exn $wr-call-raised)
                                                            (call_ref $ProcedureInvoker
                                                                      (local.get $proc)
                                                                      (local.get $args)
                                                                      (struct.get $Procedure $invoke
                                                                                  (local.get $proc))))))
                                    (if (result (ref $Vector))
                                        (ref.eq (call $exn? (local.get $res)) (global.get $false))
                                        (then
                                         (if (result (ref $Vector))
                                             (i32.eqz (local.get $include-printed?))
                                             (then (call $wr-success/no-display
                                                         (local.get $res)))
                                             (else (call $wr-success
                                                         (local.get $res)))))
                                        (else
                                         (call $wr-failure
                                               (global.get $string:wr-kind:exception)
                                               (call $format/display (local.get $res))
                                               (call $wr-bridge-error-message (local.get $res))))))))))

               (local.set $bridge-sentinel
                          (call $cons (global.get $false) (global.get $false)))
               (local.set $bridge-exn
                          (block $wr-call-encode-failed (result (ref eq))
                            (try_table (result (ref eq))
                                       (catch $exn $wr-call-encode-failed)
                                       (global.set $result-bytes
                                                   (call $s-exp->fasl
                                                         (local.get $bridge)
                                                         (global.get $false)))
                                       (local.get $bridge-sentinel))))
               (if (i32.eqz (ref.eq (local.get $bridge-exn) (local.get $bridge-sentinel)))
                   (then
                    (global.set $result-bytes
                                      (call $s-exp->fasl
                                            (call $wr-failure
                                            (global.get $string:wr-kind:non-fasl-result)
                                            (global.get $false)
                                            (call $callback-non-fasl-return-message
                                                  (local.get $res)))
                                      (global.get $false)))))
               (local.set $len
                          (call $copy-bytes-to-callback-buffer
                                (ref.cast (ref $Bytes) (global.get $result-bytes))))
               (local.get $len))

         ;; $wr-call : i32 -> i32
         ;;   Call a bridge binding with its formatted console representation.
         (func $wr-call (export "wr-call")
               (param $fasl i32)
               (result i32)

               (call $wr-call/internal
                     (local.get $fasl)
                     (i32.const 1)))

         ;; $wr-value-call : i32 -> i32
         ;;   Call a bridge binding without producing a discarded display form.
         (func $wr-value-call (export "wr-value-call")
               (param $fasl i32)
               (result i32)

               (call $wr-call/internal
                     (local.get $fasl)
                     (i32.const 0)))

         (func $wr-names (export "wr-names")
               (param $ignored i32)
               (result i32)

               (call $wr-copy-result (call $wr-top-level-names)))

         (func $wr-names-detailed (export "wr-names-detailed")
               (param $ignored i32)
               (result i32)

               (call $wr-copy-result (call $wr-top-level-names-detailed)))

         (func $wr-format (export "wr-format")
               (param $fasl i32)
               (result i32)

               (call $wr-copy-result
                     (call $format/display
                           (call $fasl-memory->s-exp (local.get $fasl)))))

         (func $callback-register (export "callback-register")
               (param $p (ref $Procedure))
               (result   i32)

               (local $g (ref $GrowableArray))
               (local $i i32)

               (local.set $g (global.get $callback-registry))
               (local.set $i (call $growable-array-count (local.get $g)))
               (call $growable-array-add! (local.get $g) (local.get $p))
               (local.get $i))

         (func $callback (export "callback")
               (param $id   i32)
               (param $fasl i32) ; index into linear memory
               (result      i32)

               (local $proc  (ref $Procedure))
               (local $vec   (ref $Vector))
               (local $args  (ref $Args))
               (local $res   (ref eq))
               (local $payload (ref eq))
               (local $bridge  (ref $Vector))
               (local $bridge-exn (ref eq))
               (local $bridge-sentinel (ref eq))
               (local $success? i32)
               (local $len   i32)

               ;; Look up procedure by id
               (local.set $proc
                          (ref.cast (ref $Procedure)
                                    (call $growable-array-ref
                                          (global.get $callback-registry)
                                          (local.get $id))))
               ;; Decode FASL-encoded arguments from linear memory
               (local.set $vec
                          (ref.cast (ref $Vector)
                                    (call $linear-memory->value
                                          (local.get $fasl))))
               (local.set $args
                          (ref.cast (ref $Args)
                                    (struct.get $Vector $arr (local.get $vec))))
               ;; Invoke procedure and tag the callback outcome before
               ;; serializing anything across the Wasm<->JS callback bridge.
               (local.set $success? (i32.const 0))
               (local.set $res
                          (block $callback-raised (result (ref eq))
                                 (try_table (result (ref eq))
                                            (catch $exn $callback-raised)
                                            (local.set $res
                                                       (call_ref $ProcedureInvoker
                                                                 (local.get $proc)
                                                                 (local.get $args)
                                                                 (struct.get $Procedure $invoke
                                                                             (local.get $proc))))
                                            (local.set $success? (i32.const 1))
                                            (local.get $res))))
               (local.set $payload
                          (call $callback-result->bridge-payload
                                (local.get $res)
                                (local.get $success?)))
               (local.set $bridge
                          (call $callback-result->bridge-vector
                                (local.get $success?)
                                (local.get $payload)))
               ;; Encode tagged result and copy to memory for host. If a
               ;; successful callback returned a non-FASL value, convert that
               ;; serialization failure into an ordinary callback failure.
               (local.set $bridge-sentinel
                          (call $cons (global.get $false) (global.get $false)))
               (local.set $bridge-exn
                          (block $callback-encode-failed (result (ref eq))
                                 (try_table (result (ref eq))
                                            (catch $exn $callback-encode-failed)
                                            (global.set $result-bytes
                                                        (call $s-exp->fasl
                                                              (local.get $bridge)
                                                              (global.get $false)))
                                            (local.get $bridge-sentinel))))
               (if (i32.eqz (ref.eq (local.get $bridge-exn) (local.get $bridge-sentinel)))
                   (then
                    (local.set $payload
                               (call $callback-non-fasl-return-message
                                     (local.get $res)))
                    (local.set $bridge
                               (call $callback-result->bridge-vector
                                     (i32.const 0)
                                     (local.get $payload)))
                    (global.set $result-bytes
                                (call $s-exp->fasl
                                      (local.get $bridge)
                                      (global.get $false)))))
               (local.set $len
                          (call $copy-bytes-to-callback-buffer
                                (ref.cast (ref $Bytes) (global.get $result-bytes))))
               (local.get $len))

         ;; callback-accepts-argc : callback-id argc -> boolean
         ;; Returns 1 when the callback procedure accepts argc arguments, else 0.
         (func $callback-accepts-argc (export "callback-accepts-argc")
               (param $id   i32)
               (param $argc i32)
               (result      i32)

               (local $proc (ref $Procedure))

               (local.set $proc
                          (ref.cast (ref $Procedure)
                                    (call $growable-array-ref
                                          (global.get $callback-registry)
                                          (local.get $id))))
               (call $procedure-arity-includes?/checked/i32
                     (local.get $proc)
                     (local.get $argc)))

         ;; callback-expected-arity : callback-id -> fasl-string-byte-length
         ;; Encodes the callback's expected-arity string in the callback buffer.
         (func $callback-expected-arity (export "callback-expected-arity")
               (param $id i32)
               (result i32)

               (local $proc (ref $Procedure))
               (local $len  i32)

               (local.set $proc
                          (ref.cast (ref $Procedure)
                                    (call $growable-array-ref
                                          (global.get $callback-registry)
                                          (local.get $id))))
               (global.set $result-bytes
                           (call $s-exp->fasl
                                 (call $procedure-arity->expected-string (local.get $proc))
                                 (global.get $false)))
               (local.set $len
                          (call $copy-bytes-to-callback-buffer
                                (ref.cast (ref $Bytes) (global.get $result-bytes))))
               (local.get $len))

         ;; callback-name : callback-id -> fasl-name-byte-length
         ;; Encodes the callback's object-name result in the callback buffer.
         (func $callback-name (export "callback-name")
               (param $id i32)
               (result i32)

               (local $proc (ref $Procedure))
               (local $len  i32)

               (local.set $proc
                          (ref.cast (ref $Procedure)
                                    (call $growable-array-ref
                                          (global.get $callback-registry)
                                          (local.get $id))))
               (global.set $result-bytes
                           (call $s-exp->fasl
                                 (call $object-name (local.get $proc))
                                 (global.get $false)))
               (local.set $len
                          (call $copy-bytes-to-callback-buffer
                                (ref.cast (ref $Bytes) (global.get $result-bytes))))
               (local.get $len))

         ;; callback-debug-id : callback-id -> fasl-debug-id-byte-length
         ;; Encodes the callback's closure debug-id in the callback buffer, or #f.
         (func $callback-debug-id (export "callback-debug-id")
               (param $id i32)
               (result i32)

               (local $proc (ref $Procedure))
               (local $len  i32)
               (local $debug-id (ref eq))

               (local.set $proc
                          (ref.cast (ref $Procedure)
                                    (call $growable-array-ref
                                          (global.get $callback-registry)
                                          (local.get $id))))
               (local.set $debug-id
                          (if (result (ref eq))
                              (ref.test (ref $Closure) (local.get $proc))
                              (then (struct.get $Closure $debug-id
                                                (ref.cast (ref $Closure) (local.get $proc))))
                              (else (global.get $false))))
               (global.set $result-bytes
                           (call $s-exp->fasl
                                 (local.get $debug-id)
                                 (global.get $false)))
               (local.set $len
                          (call $copy-bytes-to-callback-buffer
                                (ref.cast (ref $Bytes) (global.get $result-bytes))))
               (local.get $len))

         (func $procedure->external (export "procedure->external")
               (type $Prim1)
               (param $proc (ref eq))
               (result (ref eq))

               (local $p  (ref $Procedure))
               (local $id i32)
               (local $cb (ref extern))

               ;; Fail-early semantic type check. Structs are represented under
               ;; $Procedure, but only applicable structs are procedures.
               (if (ref.eq (call $procedure? (local.get $proc))
                           (global.get $false))
                   (then (call $raise-argument-error:procedure-expected/who
                               (global.get $symbol:procedure->external)
                               (local.get $proc))
                         (unreachable)))

               (local.set $p (ref.cast (ref $Procedure) (local.get $proc)))
               (local.set $id (call $callback-register (local.get $p)))
               (local.set $cb (call $js-make-callback (local.get $id)))

               ;; Wrap extern callback as a Racket external value
               (struct.new $External
                           (i32.const 0)
                           (local.get $cb)))


    ))

;; select-ffi-helper-forms : list? list? -> (listof datum?)
;;   Render selected FFI helper forms for the given console bridge bindings.
(define (select-ffi-helper-forms names console-bridge-bindings)
  (parameterize ([current-ffi-console-bridge-bindings console-bridge-bindings])
    (define name-set
      (for/hasheq ([name (in-list names)])
        (values name #t)))
    (for/list ([form (in-list (ffi-runtime-only-forms))]
               #:when (and (pair? form)
                            (eq? 'func (car form))
                            (hash-ref name-set (cadr form) #f)))
      form)))

;; ffi-console-bridge-helper-forms : list? -> (listof datum?)
;;   Render program-specific console bridge helpers.
(define (ffi-console-bridge-helper-forms console-bridge-bindings)
  (select-ffi-helper-forms ffi-console-bridge-helper-names
                           console-bridge-bindings))

;; ffi-runtime-only-helper-forms : list? -> (listof datum?)
;;   Render program/runtime-local FFI helpers for the given console bridge bindings.
(define (ffi-runtime-only-helper-forms console-bridge-bindings)
  (select-ffi-helper-forms ffi-runtime-only-helper-names
                           console-bridge-bindings))

;; external-value-runtime-helper-forms : hasheq? -> (listof datum?)
;;   Render external host value helpers in runtime assembly order.
(define (external-value-runtime-helper-forms helper-index)
  (for/list ([name (in-list external-value-runtime-only-helper-names)])
    (core-runtime-helper-form helper-index name)))

;; external-value-core-runtime-helper-forms : hasheq? -> (listof datum?)
;;   Render promoted external host value helpers in runtime assembly order.
(define (external-value-core-runtime-helper-forms helper-index)
  (for/list ([name (in-list external-value-core-runtime-helper-names)])
    (core-runtime-helper-form helper-index name)))

;; ffi-core-runtime-helper-forms : hasheq? -> (listof datum?)
;;   Render promoted FFI helpers in runtime assembly order.
(define (ffi-core-runtime-helper-forms helper-index)
  (for/list ([name (in-list ffi-core-runtime-helper-names)])
    (core-runtime-helper-form helper-index name)))

;; unsafe-ffi-runtime-only-helper-forms : (listof foreign?) -> (listof datum?)
;;   Render unsafe FFI primitive lookup helpers for the active `.ffi` primitives.
(define (unsafe-ffi-runtime-only-helper-forms active-ffi-foreigns)
  `(
         ;; unsafe-ffi-primitive : symbol? -> procedure?
         ;;   Resolve generated unsafe FFI module aliases to active `.ffi` primitive closures.
         (func $unsafe-ffi-primitive (type $Prim1)
               (param $name (ref eq)) ;; quoted FFI primitive symbol
               (result (ref eq))
               (local $proc (ref eq))
               (if (ref.is_null (global.get $unsafe-ffi-primitive-table))
                   (then (call $initialize-unsafe-ffi-primitive-table)))
               (local.set $proc
                          (call $hasheq-ref/plain
                                (ref.cast (ref $HashEqMutable)
                                          (global.get $unsafe-ffi-primitive-table))
                                (local.get $name)
                                (global.get $false)))
               (if (ref.eq (local.get $proc) (global.get $false))
                   (then
                    (call $raise-unexpected-argument (local.get $name))
                    (unreachable)))
               (local.get $proc))

         ;; ensure-unsafe-ffi-primitive-table : -> void
         ;;   Lazily allocate the generated unsafe FFI primitive table.
         (func $ensure-unsafe-ffi-primitive-table
               (if (ref.is_null (global.get $unsafe-ffi-primitive-table))
                   (then
                    (global.set $unsafe-ffi-primitive-table
                                (ref.cast (ref $HashEqMutable)
                                          (call $make-empty-hasheq))))))

         ;; register-unsafe-ffi-primitive! : symbol? procedure? -> void
         ;;   Register a generated unsafe FFI primitive procedure in the core-owned table.
         (func $register-unsafe-ffi-primitive!
               (param $name (ref eq)) ;; quoted FFI primitive symbol
               (param $proc (ref eq)) ;; primitive procedure value
               (call $ensure-unsafe-ffi-primitive-table)
               (call $hasheq-set!/mutable/checked
                     (ref.cast (ref $HashEqMutable)
                               (global.get $unsafe-ffi-primitive-table))
                     (local.get $name)
                     (local.get $proc)))

         (func $initialize-unsafe-ffi-primitive-table
               (call $ensure-unsafe-ffi-primitive-table)
               ,@(for/list ([f (in-list active-ffi-foreigns)])
                   (define pr (foreign-racket-name f))
                   `(call $register-unsafe-ffi-primitive!
                          (global.get ,($ (string->symbol (~a "symbol:" pr))))
                          (global.get ,($ (prim: pr))))))))

;; ffi-runtime-only-form-ref : symbol? -> datum?
;;   Render the registered helper with the given Wasm function name.
(define (ffi-runtime-only-form-ref name)
  (or (for/first ([form (in-list (ffi-runtime-only-forms))]
                  #:when (and (pair? form)
                              (eq? 'func (car form))
                              (eq? name (cadr form))))
        form)
      (error 'ffi-runtime-only-form-ref
             "unknown FFI runtime helper: ~a"
             name)))

(define ffi-helpers
  (for/list ([name (in-list ffi-helper-names)])
    (core-runtime-helper name
                         (symbol->string name)
                         (lambda ()
                           (ffi-runtime-only-form-ref name)))))

;; ffi-core-runtime-helpers : (listof core-runtime-helper?)
;;   Enumerate promoted FFI helpers available in the standalone core runtime.
(define ffi-core-runtime-helpers
  (select-runtime-helpers ffi-helpers ffi-core-runtime-helper-names))

;; ffi-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate FFI helpers that remain program/runtime-local.
(define ffi-runtime-only-helpers
  (append external-value-runtime-only-helpers
          (select-runtime-helpers ffi-helpers ffi-runtime-only-helper-names)))

(module+ test
  (require rackunit)

  ;; form-name : datum? -> symbol?
  ;;   Return the function name from a rendered helper form.
  (define (form-name form)
    (second form))

  (define ffi-helper-index
    (make-core-runtime-helper-index
     (append external-value-core-runtime-helpers
             ffi-core-runtime-helpers
             ffi-runtime-only-helpers)))

  (define core-ffi-helper-index
    (make-core-runtime-helper-index
     (append external-value-core-runtime-helpers
             ffi-core-runtime-helpers)))

  (check-equal?
   (map core-runtime-helper-name external-value-core-runtime-helpers)
   external-value-core-runtime-helper-names)

  (check-equal?
   (map form-name
        (external-value-core-runtime-helper-forms core-ffi-helper-index))
   external-value-core-runtime-helper-names)

  (check-equal?
   (map core-runtime-helper-name ffi-core-runtime-helpers)
   ffi-core-runtime-helper-names)

  (check-equal?
   (map form-name (ffi-core-runtime-helper-forms core-ffi-helper-index))
   ffi-core-runtime-helper-names)

  (check-false
   (member '$webracket-debug-allocation-counters
           (map form-name (ffi-runtime-only-helper-forms '()))))

  ;; Console bridge helpers are program-owned in split output.  Keeping them
  ;; out of the general runtime-only group prevents the monolithic source
  ;; module from contributing a second definition before split classification.
  (check-false
   (member '$wr-bridge-error-message ffi-runtime-only-helper-names))

  (check-false
   (member '$wr-bridge-error-message
           (map form-name (ffi-runtime-only-helper-forms '()))))

  (check-equal?
   (map form-name (ffi-console-bridge-helper-forms '()))
   ffi-console-bridge-helper-names)

  (check-false
   (member '$wr-bridge-error-message
           (map core-runtime-helper-name ffi-runtime-only-helpers)))

  (check-equal?
   (map form-name (external-value-runtime-helper-forms ffi-helper-index))
   external-value-runtime-only-helper-names))
