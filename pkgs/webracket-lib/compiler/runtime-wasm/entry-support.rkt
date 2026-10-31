#lang racket

(provide entry-local-declaration-forms
         entry-local-initializer-forms
         entry-result-copy-forms
         entry-function-forms
         entry-result-global-forms
         entry-run-body-form
         entry-support-forms
         entry-top-level-variable-initializer-forms)

(require "../private/wasm-utils.rkt"
         "../private/parameters.rkt")

;; entry-support-forms : (listof datum?)
;;   Declare static exported entry support helpers.
(define entry-support-forms
  '(
               (func $get-bytes (export "get_bytes")
                     (result (ref $Bytes))
                     (ref.cast (ref $Bytes) (global.get $result-bytes)))

               (func $entry-uncaught-exception2
                     (param $e (ref eq))
                     (local $message (ref eq))

                     ;; Keep this intentionally verbose for bootstrap probes:
                     ;; a malformed exception may have an undefined message,
                     ;; so log both the full exception shape and its message.
                     (drop (call $js-log (call $format/display (local.get $e))))
                     (local.set $message (call $exn-message (local.get $e)))
                     (drop (call $js-log (local.get $message)))
                     (drop (call $js-log (call $format/display (local.get $message)))))

               (func $entry-uncaught-exception1
                     (param $e (ref eq))
                     (drop (call $js-log (local.get $e))))

               (func $entry-uncaught-exception
                     (param $e (ref eq))
                     (if (ref.eq (call $exn? (local.get $e)) (global.get $false))
                         (then
                         (call $entry-uncaught-exception1 (local.get $e)))
                         (else
                          (call $entry-uncaught-exception2 (local.get $e))))
                     (throw $exn (local.get $e)))))

;; entry-local-name : (or/c symbol? variable?) -> symbol?
;;   Return the Wasm local name for an entry-local binding.
(define (entry-local-name v)
  (if (symbol? v) v (Var v)))

;; entry-local-declaration-forms : (listof list?) -> (listof datum?)
;;   Declare entry-local variables for let-values and letrec-values bindings.
(define (entry-local-declaration-forms entry-locals)
  (for/list ([x (in-list (reverse entry-locals))])
    (match x
      [(list v t)      `(local ,(entry-local-name v) ,t)]
      [(list v t init) `(local ,(entry-local-name v) ,t)])))

;; entry-top-level-variable-initializer-forms : (listof variable?) -> (listof datum?)
;;   Initialize boxed top-level variables to undefined.
(define (entry-top-level-variable-initializer-forms top-vars)
  (define imported-names (current-imported-top-level-globals))
  (define (top-var-source-symbol v)
    (define name (symbol->string (TopVar v)))
    (and (string-prefix? name "$$")
         (string->symbol (substring name 2))))
  (for/list ([v (in-list top-vars)]
             #:unless (let ([sym (top-var-source-symbol v)])
                         (and sym (set-member? imported-names sym))))
    `(global.set ,(TopVar v)
                 (struct.new $Boxed
                             (global.get $undefined)))
    #;`(global.set ,(TopVar v)
                   (struct.new $Boxed
                               (global.get ,(TopVar v))))))

;; entry-result-global-forms : symbol? -> (listof datum?)
;;   Declare the program result global initialized to void.
(define (entry-result-global-forms result)
  `((global ,result (mut (ref eq)) (global.get $void))))

;; entry-local-initializer-forms : (listof list?) -> (listof datum?)
;;   Initialize entry locals before running the compiled program body.
(define (entry-local-initializer-forms entry-locals)
  (for/list ([x (in-list entry-locals)])
    (match x
      [(list v t)
       (if (equal? t '(ref eq))
           `(local.set ,(entry-local-name v)
                       ,(Undefined))
           `(nop))]
      [(list v t init)
       `(local.set ,(entry-local-name v) ,init)])))

;; entry-run-body-form : datum? -> datum?
;;   Run the compiled entry body under the top-level exception handler.
(define (entry-run-body-form entry-body)
  `(block $entry-done
          (local.set $entry-exn
                     (block $entry-handler (result (ref eq))
                            (try_table (result (ref eq))
                                       (catch $exn $entry-handler)
                                       ,entry-body
                                       (br $entry-done))))
          (call $entry-uncaught-exception (local.get $entry-exn))
          (unreachable)))

;; entry-result-copy-forms : symbol? -> (listof datum?)
;;   Copy current standard output followed by the final result to linear memory.
(define (entry-result-copy-forms result)
  `((global.set $result-bytes
                (call $string->bytes/utf-8
                      (call $string-append/2
                            (call $get-output-string
                                  (call $current-output-port
                                        (global.get $missing)))
                            (call $format/display (global.get ,result)))
                      (global.get $false)   ; ignored
                      (global.get $zero)    ; start = 0
                      (global.get $false))) ; end
    (call $copy-bytes-to-memory ; copy and return length as i32
          (global.get $result-bytes) (i32.const 0))))

;; entry-function-forms :
;;   (listof list?) datum? symbol? #:flonum-initializer-forms list?
;;   #:runtime-constant-initializer-forms list? #:parameterization-initializer-forms list?
;;   #:random-current-initializer-forms list? #:thread-current-initializer-forms list?
;;   #:logger-current-initializer-forms list? #:custodian-current-initializer-forms list?
;;   #:event-initializer-forms list? #:struct-property-initializer-forms list?
;;   #:global-state-initializer-forms list?
;;   #:path-current-initializer-forms list? #:environment-parameter-initializer-forms list?
;;   #:character-general-category-initializer-forms list? #:realm-initializer-forms list?
;;   #:primitive-initializer-forms list? #:ffi-primitive-initializer-forms list?
;;   #:unsafe-ffi-primitive-table-initializer-forms list?
;;   #:security-guard-current-initializer-forms list? #:error-value-handler-late-initializer-forms list?
;;   #:print-read-parameter-late-initializer-forms list?
;;   #:io-handler-default-port-late-initializer-forms list?
;;   #:error-syntax-handler-late-initializer-forms list? #:top-vars (listof variable?)
;;   #:exception-struct-type-initializer-forms list? #:namespace-initializer-forms list? -> (listof datum?)
;;   Build a zero-local bootstrap helper followed by the exported program entry.
;; The many constant-initialization calls must not share the application's large
;; local frame: that combination makes browser baseline compilation expensive.
;; Keep the bootstrap phases in execution order, before local initialization and
;; outside the application's exception handler.
(define (entry-function-forms
         entry-locals
         entry-body
         result
         #:flonum-initializer-forms flonum-initializer-forms
         #:runtime-constant-initializer-forms runtime-constant-initializer-forms
         #:parameterization-initializer-forms parameterization-initializer-forms
         #:random-current-initializer-forms random-current-initializer-forms
         #:thread-current-initializer-forms thread-current-initializer-forms
         #:logger-current-initializer-forms logger-current-initializer-forms
         #:custodian-current-initializer-forms custodian-current-initializer-forms
         #:event-initializer-forms event-initializer-forms
         #:struct-property-initializer-forms struct-property-initializer-forms
         #:global-state-initializer-forms
         ;; Symbol and keyword tables are initialized here for interning.
         [global-state-initializer-forms
          '((call $initialize-the-symbol-table)
            (call $initialize-the-keyword-table)
            (call $initialize-primitive-table-user-installed)
            (call $initialize-prefab-struct-type-registry))]
         #:path-current-initializer-forms path-current-initializer-forms
         #:environment-parameter-initializer-forms environment-parameter-initializer-forms
         #:character-general-category-initializer-forms character-general-category-initializer-forms
         #:realm-initializer-forms realm-initializer-forms
         #:primitive-initializer-forms primitive-initializer-forms
         #:ffi-primitive-initializer-forms ffi-primitive-initializer-forms
         #:unsafe-ffi-primitive-table-initializer-forms
         [unsafe-ffi-primitive-table-initializer-forms
          '((call $initialize-unsafe-ffi-primitive-table))]
         #:security-guard-current-initializer-forms security-guard-current-initializer-forms
         #:error-value-handler-late-initializer-forms error-value-handler-late-initializer-forms
         #:print-read-parameter-late-initializer-forms print-read-parameter-late-initializer-forms
         #:io-handler-default-port-late-initializer-forms io-handler-default-port-late-initializer-forms
         #:error-syntax-handler-late-initializer-forms error-syntax-handler-late-initializer-forms
         #:top-vars top-vars
         #:exception-struct-type-initializer-forms exception-struct-type-initializer-forms
         #:namespace-initializer-forms namespace-initializer-forms)
  `((func $entry-bootstrap
         ;; Initialize global constants
         ,@flonum-initializer-forms

         ;; Initialize global state
         ,@global-state-initializer-forms

         ;; Initialize constants used by the program and runtime.
         ,@runtime-constant-initializer-forms
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

         ;; Initialize variables holding primitives
         ,@primitive-initializer-forms
         ,@ffi-primitive-initializer-forms
         ,@unsafe-ffi-primitive-table-initializer-forms
         ,@security-guard-current-initializer-forms
         ,@error-value-handler-late-initializer-forms
         ,@print-read-parameter-late-initializer-forms
         ,@io-handler-default-port-late-initializer-forms
         ,@error-syntax-handler-late-initializer-forms

         ;; Initialize top-level variables.
         ;; These are all "boxed".
         ,@(entry-top-level-variable-initializer-forms top-vars)

         ;; Populate exception struct type descriptor bindings.
         ;;   struct:exn, struct:exn:fail, etc
         ,@exception-struct-type-initializer-forms

         ;; Initialize the top-level namespace
         ,@namespace-initializer-forms)

    (func $entry (export "entry") (result i32)
         ; Declare local variables (bound by let-values and letrec-values)
         (local $entry-exn (ref eq))
         ,@(entry-local-declaration-forms entry-locals)

         (call $entry-bootstrap)

         ; Initialize local variables
         ,@(entry-local-initializer-forms entry-locals)

         ; This runs `entry-body`. An exception handler is
         ; installed to catch exceptions not handled by the
         ; user program.
         ,(entry-run-body-form entry-body)

         ; Return the result
         ,@(entry-result-copy-forms result))))

(module+ test
  (require rackunit
           racket/list
           "../private/structs.rkt")

  (define top-var (variable #'top))

  (check-equal? (entry-local-declaration-forms '((x (ref eq))
                                                 (y i32 (i32.const 0))))
                '((local y i32)
                  (local x (ref eq))))

  (check-equal? (entry-local-initializer-forms '((x (ref eq))
                                                 (y i32)
                                                 (z i32 (i32.const 7))))
                `((local.set x ,(Undefined))
                  (nop)
                  (local.set z (i32.const 7))))

  (check-equal? (entry-top-level-variable-initializer-forms (list top-var))
                '((global.set $$top
                              (struct.new $Boxed
                                          (global.get $undefined)))))

  (check-equal? (entry-result-global-forms '$result)
                '((global $result (mut (ref eq)) (global.get $void))))

  (check-equal? (entry-run-body-form '(call $program))
                '(block $entry-done
                        (local.set $entry-exn
                                   (block $entry-handler (result (ref eq))
                                          (try_table (result (ref eq))
                                                     (catch $exn $entry-handler)
                                                     (call $program)
                                                     (br $entry-done))))
                        (call $entry-uncaught-exception (local.get $entry-exn))
                        (unreachable)))

  (check-equal? (entry-result-copy-forms '$result)
                '((global.set $result-bytes
                              (call $string->bytes/utf-8
                                    (call $string-append/2
                                          (call $get-output-string
                                                (call $current-output-port
                                                      (global.get $missing)))
                                          (call $format/display (global.get $result)))
                                    (global.get $false)
                                    (global.get $zero)
                                    (global.get $false)))
                  (call $copy-bytes-to-memory
                        (global.get $result-bytes) (i32.const 0))))

  (define sample-entry-forms
    (entry-function-forms
     '((x (ref eq)))
     '(call $program)
     '$result
     #:flonum-initializer-forms '((call $init-flonum))
     #:runtime-constant-initializer-forms '((call $init-runtime-constants))
     #:parameterization-initializer-forms '((call $init-parameterization))
     #:random-current-initializer-forms '((call $init-random))
     #:thread-current-initializer-forms '((call $init-thread))
     #:logger-current-initializer-forms '((call $init-logger))
     #:custodian-current-initializer-forms '((call $init-custodian))
     #:event-initializer-forms '((call $init-event))
     #:struct-property-initializer-forms '((call $init-struct-property))
     #:path-current-initializer-forms '((call $init-path))
     #:environment-parameter-initializer-forms '((call $init-environment))
     #:character-general-category-initializer-forms '((call $init-char-category))
     #:realm-initializer-forms '((call $init-realm))
     #:primitive-initializer-forms '((call $init-primitives))
     #:ffi-primitive-initializer-forms '((call $init-ffi-primitives))
     #:security-guard-current-initializer-forms '((call $init-security-guard))
     #:error-value-handler-late-initializer-forms '((call $init-error-value-handler))
     #:print-read-parameter-late-initializer-forms '((call $init-print-read))
     #:io-handler-default-port-late-initializer-forms '((call $init-default-ports))
     #:error-syntax-handler-late-initializer-forms '((call $init-error-syntax-handler))
     #:top-vars (list top-var)
     #:exception-struct-type-initializer-forms '((call $init-exception-structs))
     #:namespace-initializer-forms '((call $init-namespace))))

  ;; All bootstrap phases are emitted directly in order, with no application
  ;; locals in the helper. In particular, interning precedes constants, primitive
  ;; setup precedes unsafe FFI lookup, and top-level boxes precede their values.
  (check-equal? (length sample-entry-forms) 2)
  (check-equal?
   (first sample-entry-forms)
   '(func $entry-bootstrap
          (call $init-flonum)
          (call $initialize-the-symbol-table)
          (call $initialize-the-keyword-table)
          (call $initialize-primitive-table-user-installed)
          (call $initialize-prefab-struct-type-registry)
          (call $init-runtime-constants)
          (call $init-parameterization)
          (call $init-random)
          (call $init-thread)
          (call $init-logger)
          (call $init-custodian)
          (call $init-event)
          (call $init-struct-property)
          (call $init-path)
          (call $init-environment)
          (drop (call $reset-current-input-port!))
          (drop (call $reset-current-output-port!))
          (drop (call $reset-current-error-port!))
          (call $init-char-category)
          (call $init-realm)
          (call $init-primitives)
          (call $init-ffi-primitives)
          (call $initialize-unsafe-ffi-primitive-table)
          (call $init-security-guard)
          (call $init-error-value-handler)
          (call $init-print-read)
          (call $init-default-ports)
          (call $init-error-syntax-handler)
          (global.set $$top (struct.new $Boxed (global.get $undefined)))
          (call $init-exception-structs)
          (call $init-namespace)))
  ;; Bootstrap completes before locals are initialized and the application's
  ;; exception handler is installed; result copying stays after the run block.
  (check-equal?
   (second sample-entry-forms)
   `(func $entry (export "entry") (result i32)
          (local $entry-exn (ref eq))
          (local x (ref eq))
          (call $entry-bootstrap)
          (local.set x ,(Undefined))
          ,(entry-run-body-form '(call $program))
          ,@(entry-result-copy-forms '$result))))
