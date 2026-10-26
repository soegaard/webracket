#lang racket

(provide port-predicate-runtime-helper-forms
         port-predicate-runtime-string-constants
         port-runtime-symbol-constants
         port-event-runtime-string-constants
         port-event-runtime-symbol-constants
         port-output-runtime-string-constants
         port-closed-error-runtime-string-constants
         port-output-predicate-runtime-helper-forms
         port-state-runtime-helper-forms
         port-location-runtime-helper-forms
         port-string-constructor-runtime-helper-forms
         port-output-string-runtime-helper-forms
         port-custom-constructor-runtime-helper-forms
         port-call-with-output-string-runtime-helper-forms
         port-input-runtime-helper-forms
         port-current-global-forms
         port-current-global-names
         port-primitive-global-forms
         port-primitive-global-names
         port-output-runtime-helper-forms
         port-core-runtime-helpers
         port-runtime-only-helpers)

(require "../core-helper.rkt"
         "../../private/wasm-utils.rkt"
         (only-in "../../private/immediates.rkt"
                  char-mask
                  char-shift
                  char-tag
                  eof-value))

;; port-predicate-runtime-string-constants : (listof (list/c symbol? string?))
;;   Declare string constants used by port predicate and contract checks.
(define port-predicate-runtime-string-constants
  '((port?                      "port?")
    (input-port?                "input-port?")
    (output-port?               "output-port?")
    (call-with-output-string-procedure "(output-port? . -> . any)")
    (input-port-read-contract   "(or/c (procedure-arity-includes/c 1) input-port?)")
    (input-port-peek-contract   "(or/c (procedure-arity-includes/c 3) input-port? #f)")
    (output-port-write-contract "(or/c output-port? (procedure-arity-includes/c 5))")
    (port-procedure-arity-0     "(procedure-arity-includes/c 0)")
    (port-procedure-arity-0-or-false "(or/c (procedure-arity-includes/c 0) #f)")
    (port-procedure-arity-3-or-false "(or/c (procedure-arity-includes/c 3) #f)")
    (port-procedure-arity-1-or-false "(or/c #f (procedure-arity-includes/c 1))")
    (port-init-position-contract "(or/c exact-positive-integer? port? #f (procedure-arity-includes/c 0))")
    (port-buffer-mode-contract "(or/c #f (and/c (procedure-arity-includes/c 0) (procedure-arity-includes/c 1)))")
    (output-port-write-special-contract "(or/c #f output-port? (procedure-arity-includes/c 3))")
    (port-or-false-contract     "(or/c port? #f)")
    (progress-or-false-contract "(or/c #f progress-evt?)")
    (evt-or-false-contract      "(or/c #f evt?)")
    (read-special-wrap-contract "(or/c #f (procedure-arity-includes/c 1))")
    (peek-special-wrap-contract "(or/c (any/c -> any/c) #f 'special)")
    (string-port-backend-unsupported
     ": only string-backed ports are supported by WebRacket for this operation")
    (file-stream-port?          "file-stream-port?")
    (terminal-port?             "terminal-port?")
    (read-line-mode-contract    "(or/c 'linefeed 'return 'return-linefeed 'any 'any-one)")))

;; port-runtime-symbol-constants : (listof symbol?)
;;   Declare symbols used by string-port modes and line-ending modes.
(define port-runtime-symbol-constants
  '(string
    call-with-output-string
    make-input-port
    make-output-port
    read-bytes
    read-bytes!
    read-bytes-avail!
    read-bytes-avail!*
    peek-bytes!
    peek-bytes-avail!
    peek-bytes-avail!*
    peek-bytes
    peek-byte
    peek-char
    write-bytes
    read-string
    read-string!
    peek-string!
    peek-string
    write-byte
    write-char
    write-string
    newline
    display
    displayln
    get-output-bytes
    read-line
    read-bytes-line
    file-position*
    port-count-lines!
    port-counts-lines?
    port-next-location
    read-byte-or-special
    read-char-or-special
    peek-byte-or-special
    peek-char-or-special
    special
    linefeed
    return
    return-linefeed
    any
    any-one))

;; port-event-runtime-string-constants : (listof (list/c symbol? string?))
;;   Declare strings used by port event capability predicates.
(define port-event-runtime-string-constants
  '((port-writes-atomic?         "port-writes-atomic?")
    (port-writes-special?        "port-writes-special?")
    (port-provides-progress-evts? "port-provides-progress-evts?")))

;; port-event-runtime-symbol-constants : (listof symbol?)
;;   Declare symbols used by port event capability predicates.
(define port-event-runtime-symbol-constants
  '(port-writes-atomic?
    port-writes-special?
    port-provides-progress-evts?))

;; port-output-runtime-string-constants : (listof (list/c symbol? string?))
;;   Declare strings used by byte/string output port error messages.
(define port-output-runtime-string-constants
  '((get-output-bytes:output-string-port-contract "(and/c output-port? string-port?)")
    (get-output-bytes:end-index-contract         "(or/c exact-nonnegative-integer? #f)")
    (get-output-bytes:valid-range-open           "\n  valid range: [")
    (get-output-bytes:valid-range-separator      ", ")
    (get-output-bytes:port-content               "\n  port content: ")
    (get-output-bytes:output-port-prefix         "#<output-port:")
    (get-output-bytes:output-port-suffix         ">")))

;; port-closed-error-runtime-string-constants : (listof (list/c symbol? string?))
;;   Declare string constants used by closed-port error messages.
(define port-closed-error-runtime-string-constants
  '((read-byte:input-port-closed "read-byte: input port is closed")
    (peek-byte:input-port-closed "peek-byte: input port is closed")
    (write-byte:output-port-closed "write-byte: output port is closed")
    (flush-output:output-port-closed "flush-output: output port is closed")
    (file-position*:input-port-closed "file-position*: input port is closed")
    (file-position*:output-port-closed "file-position*: output port is closed")
    (port-count-lines!:input-port-closed "port-count-lines!: input port is closed")
    (port-count-lines!:output-port-closed "port-count-lines!: output port is closed")
    (port-next-location:input-port-closed "port-next-location: input port is closed")
    (port-next-location:output-port-closed "port-next-location: output port is closed")))

;; port-current-global-forms : (listof datum?)
;;   Declare globals for current input, output, and error ports.
(define port-current-global-forms
  '(
               (global $current-input-port-value  (mut (ref eq)) (ref.i31 (i32.const 0)))
               (global $current-output-port-value (mut (ref eq)) (ref.i31 (i32.const 0)))
               (global $current-error-port-value  (mut (ref eq)) (ref.i31 (i32.const 0)))))

;; port-current-global-names : (listof symbol?)
;;   Enumerate current input, output, and error port globals.
(define port-current-global-names
  (for/list ([form (in-list port-current-global-forms)])
    (match form
      [`(global ,name . ,_) name]
      [_ (error 'port-current-global-names
                "expected global form, got: ~s"
                form)])))

;; port-primitive-global-forms : (listof datum?)
;;   Declare primitive globals used by port helpers for arity/error reporting.
(define port-primitive-global-forms
  '(
               (global $prim:make-input-port       (mut (ref eq)) (global.get $undefined))
               (global $prim:make-output-port      (mut (ref eq)) (global.get $undefined))
               (global $prim:peek-bytes-avail!     (mut (ref eq)) (global.get $undefined))
               (global $prim:peek-bytes-avail!*    (mut (ref eq)) (global.get $undefined))))

;; port-primitive-global-names : (listof symbol?)
;;   Enumerate port primitive globals shared by split modules.
(define port-primitive-global-names
  (for/list ([form (in-list port-primitive-global-forms)])
    (match form
      [`(global ,name . ,_) name]
      [_ (error 'port-primitive-global-names
                "expected global form, got: ~s"
                form)])))

;; port-predicate-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for port predicate and wrapper helpers.
(define port-predicate-runtime-only-helper-names
  '($raise-check-string-port
    $raise-check-port-or-false
    $string-port?
    $struct-has-property?/i32
    $input-port-like?/i32
    $output-port-like?/i32
    $prop-output-port-target
    $port-like?/i32
    $port?
    $input-port?
    $file-stream-port?
    $terminal-port?))

;; port-state-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for current, close, and flush port helpers.
(define port-state-runtime-only-helper-names
  '($port-closed?
    $close-input-port
    $current-input-port
    $current-output-port
    $current-error-port
    $reset-current-input-port!
    $reset-current-output-port!
    $reset-current-error-port!
    $flush-output
    $close-output-port))

;; port-core-runtime-helper-names : (listof symbol?)
;;   Name port helpers available in the standalone core runtime.
(define port-core-runtime-helper-names
  '($raise-check-string-port
    $input-port-like?/i32
    $output-port?
    $close-input-port
    $current-input-port
    $flush-output
    $close-output-port
    $open-input-bytes
    $read-byte/custom
    $peek-byte/custom
    $read-byte
    $read-char
    $check-read-special-wrap/who
    $raise-port-bytes-index-expected
    $raise-bad-port-bytes-range
    $read-bytes!/who
    $raise-port-range-index-expected
    $raise-read-line:bad-mode
    $read-line-mode-code
    $peek-byte
    $check-peek-special-wrap/who
    $write-bytes/custom
    $write-bytes
    $raise-bad-string-range
    $write-string))

;; port-location-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for port location helpers.
(define port-location-runtime-only-helper-names
  '($port-count-lines!
    $port-counts-lines?
    $file-position*
    $port-next-location))

;; port-string-constructor-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for string and byte string port constructors.
(define port-string-constructor-runtime-only-helper-names
  '($open-input-bytes
    $open-input-string
    $open-output-bytes
    $open-output-string
    $format-get-output-bytes:port-content
    $raise-get-output-bytes:port-expected
    $raise-get-output-bytes:start-expected
    $raise-get-output-bytes:end-expected
    $raise-get-output-bytes:range
    $get-output-bytes))

;; port-custom-constructor-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for custom input/output port constructors.
(define port-custom-constructor-runtime-only-helper-names
  '($check-port-procedure-or-false/who
    $check-port-init-position/who
    $check-port-buffer-mode/who
    $check-output-write-special/who
    $make-port-initial-location
    $make-input-port
    $make-output-port))

;; port-event-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for port event capability predicates.
(define port-event-runtime-only-helper-names
  '($progress-evt?
    $port-provides-progress-evts?
    $port-writes-atomic?
    $port-writes-special?))

;; port-custom-byte-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for custom byte peek/read helpers.
(define port-custom-byte-runtime-only-helper-names
  '($read-byte/custom
    $peek-byte/custom))

;; port-byte-char-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for byte and character input helpers.
(define port-byte-char-runtime-only-helper-names
  '($read-byte
    $read-char))

;; port-byte-char-special-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for byte and character special input helpers.
(define port-byte-char-special-runtime-only-helper-names
  '($check-read-special-wrap/who
    $read-byte-or-special
    $read-char-or-special))

;; port-read-bytes-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for byte-vector read helpers.
(define port-read-bytes-runtime-only-helper-names
  '($raise-port-bytes-index-expected
    $raise-bad-port-bytes-range
    $read-bytes!/who
    $read-bytes!
    $read-bytes-avail!
    $read-bytes-avail!*))

;; port-read-string-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for string and bytes read helpers.
(define port-read-string-runtime-only-helper-names
  '($raise-port-range-index-expected
    $read-string!
    $read-bytes
    $read-string))

;; port-ready-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for ready predicates.
(define port-ready-runtime-only-helper-names
  '($byte-ready?
    $char-ready?))

;; port-read-line-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for line-reading helpers.
(define port-read-line-runtime-only-helper-names
  '($raise-read-line:bad-mode
    $read-line-mode-code
    $read-line/custom
    $read-line
    $read-bytes-line))

;; port-peek-bytes-bang-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for mutating byte peek helpers.
(define port-peek-bytes-bang-runtime-only-helper-names
  '($peek-bytes!/custom
    $peek-bytes!/who
    $peek-bytes!
    $peek-bytes-avail!:progress-not-supported
    $peek-bytes-avail!/who
    $peek-bytes-avail!
    $peek-bytes-avail!*))

;; port-peek-byte-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for byte and character peek helpers.
(define port-peek-byte-runtime-only-helper-names
  '($peek-byte
    $peek-char
    $check-peek-progress-evt/who
    $check-peek-special-wrap/who
    $peek-byte-or-special
    $peek-char-or-special))

;; port-peek-bytes-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for byte and string peek helpers.
(define port-peek-bytes-runtime-only-helper-names
  '($peek-bytes
    $peek-string))

;; port-write-basic-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for basic byte/string output helpers.
(define port-write-basic-runtime-only-helper-names
  '($write-byte
    $write-char
    $newline
    $display
    $displayln))

;; port-write-bytes-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for byte/string output range helpers.
(define port-write-bytes-runtime-only-helper-names
  '($write-bytes/custom
    $write-bytes
    $raise-bad-string-range
    $write-string))

;; port-output-predicate-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for the output-port? predicate.
(define port-output-predicate-runtime-only-helper-names
  '($output-port?))

;; port-output-string-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for output string extraction helpers.
(define port-output-string-runtime-only-helper-names
  '($get-output-string))

;; port-call-with-output-string-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for call-with-output-string.
(define port-call-with-output-string-runtime-only-helper-names
  '($call-with-output-string))

;; port-input-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for byte/string input helpers.
(define port-input-runtime-only-helper-names
  (append port-event-runtime-only-helper-names
          port-custom-byte-runtime-only-helper-names
          port-byte-char-runtime-only-helper-names
          port-byte-char-special-runtime-only-helper-names
          port-read-bytes-runtime-only-helper-names
          port-read-string-runtime-only-helper-names
          port-ready-runtime-only-helper-names
          port-read-line-runtime-only-helper-names
          port-peek-bytes-bang-runtime-only-helper-names
          '($peek-string!)
          port-peek-byte-runtime-only-helper-names
          port-peek-bytes-runtime-only-helper-names))

;; port-output-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for byte/string output helpers.
(define port-output-runtime-only-helper-names
  (append port-write-basic-runtime-only-helper-names
          port-write-bytes-runtime-only-helper-names))

;; port-helper-forms : hasheq? (listof symbol?) -> (listof datum?)
;;   Render the given port helper names from the runtime helper index.
(define (port-helper-forms helper-index names)
  (for/list ([name (in-list names)])
    (core-runtime-helper-form helper-index name)))

;; port-predicate-runtime-helper-forms : hasheq? -> (listof datum?)
;;   Render port predicate and wrapper-recognition helpers in runtime assembly order.
(define (port-predicate-runtime-helper-forms helper-index)
  (port-helper-forms helper-index port-predicate-runtime-only-helper-names))

;; port-output-predicate-runtime-helper-forms : hasheq? -> (listof datum?)
;;   Render output-port? in runtime assembly order.
(define (port-output-predicate-runtime-helper-forms helper-index)
  (port-helper-forms helper-index port-output-predicate-runtime-only-helper-names))

;; port-state-runtime-helper-forms : hasheq? -> (listof datum?)
;;   Render current, close, and flush port helpers in runtime assembly order.
(define (port-state-runtime-helper-forms helper-index)
  (port-helper-forms helper-index port-state-runtime-only-helper-names))

;; port-location-runtime-helper-forms : hasheq? -> (listof datum?)
;;   Render port location helpers in runtime assembly order.
(define (port-location-runtime-helper-forms helper-index)
  (port-helper-forms helper-index port-location-runtime-only-helper-names))

;; port-string-constructor-runtime-helper-forms : hasheq? -> (listof datum?)
;;   Render string and byte string port constructors in runtime assembly order.
(define (port-string-constructor-runtime-helper-forms helper-index)
  (port-helper-forms helper-index port-string-constructor-runtime-only-helper-names))

;; port-output-string-runtime-helper-forms : hasheq? -> (listof datum?)
;;   Render output string extraction helpers in runtime assembly order.
(define (port-output-string-runtime-helper-forms helper-index)
  (port-helper-forms helper-index port-output-string-runtime-only-helper-names))

;; port-custom-constructor-runtime-helper-forms : hasheq? -> (listof datum?)
;;   Render custom input/output port constructors in runtime assembly order.
(define (port-custom-constructor-runtime-helper-forms helper-index)
  (port-helper-forms helper-index port-custom-constructor-runtime-only-helper-names))

;; port-call-with-output-string-runtime-helper-forms : hasheq? -> (listof datum?)
;;   Render call-with-output-string in runtime assembly order.
(define (port-call-with-output-string-runtime-helper-forms helper-index)
  (port-helper-forms helper-index port-call-with-output-string-runtime-only-helper-names))

;; port-input-runtime-helper-forms : hasheq? -> (listof datum?)
;;   Render byte/string input helpers in runtime assembly order.
(define (port-input-runtime-helper-forms helper-index)
  (port-helper-forms helper-index port-input-runtime-only-helper-names))

;; port-output-runtime-helper-forms : hasheq? -> (listof datum?)
;;   Render byte/string output helpers in runtime assembly order.
(define (port-output-runtime-helper-forms helper-index)
  (port-helper-forms helper-index port-output-runtime-only-helper-names))

;; port-predicate-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate port predicate and wrapper-recognition helpers.
(define port-predicate-runtime-only-helpers
  (function-forms->runtime-helpers
   'port-predicate-runtime-only-helpers
   '(
     (func $raise-check-string-port
           (param $who (ref eq))
           (param $x   (ref eq))

           (local $message (ref $String))

           (local.set $message
                      (call $string-append/2
                            (call $format/display (local.get $who))
                            (global.get $string:string-port-backend-unsupported)))
           (call $raise
                 (call $exn:fail:unsupported/make
                       (local.get $message)
                       (call $current-continuation-marks (global.get $missing)))
                 (global.get $true))
           (unreachable))

     (func $raise-check-port-or-false
           (param $who (ref eq))
           (param $x   (ref eq))

           (call $raise-argument-error1
                 (local.get $who)
                 (global.get $string:port-or-false-contract)
                 (local.get $x))
           (unreachable))

     (func $string-port? (type $Prim1)
           (param $v (ref eq))
           (result (ref eq))
           (if (result (ref eq))
               (i32.or (ref.test (ref $InputStringPort)  (local.get $v))
                       (ref.test (ref $OutputStringPort) (local.get $v)))
               (then (global.get $true))
               (else (global.get $false))))

     ;; struct-has-property?/i32 : any/c struct-type-property? -> i32
     ;;   Check whether a struct instance's type has a property.
     (func $struct-has-property?/i32
           (param $v    (ref eq))
           (param $prop (ref $StructTypeProperty))
           (result      i32)

           (local $type     (ref $StructType))
           (local $sentinel (ref eq))
           (local $prop-val (ref eq))

           (if (i32.eqz (ref.test (ref $Struct) (local.get $v)))
               (then (return (i32.const 0))))
           (local.set $type
                      (struct.get $Struct $type
                                  (ref.cast (ref $Struct) (local.get $v))))
           (local.set $sentinel (call $cons (global.get $false)
                                      (global.get $false)))
           (local.set $prop-val
                      (call $struct-type-property-lookup
                            (local.get $type)
                            (local.get $prop)
                            (local.get $sentinel)))
           (i32.eqz (ref.eq (local.get $prop-val) (local.get $sentinel))))

     ;; input-port-like?/i32 : any/c -> i32
     ;;   Recognize native input ports and prop:input-port wrappers.
     (func $input-port-like?/i32
           (param $v (ref eq))
           (result   i32)
           (if (ref.test (ref $InputPort) (local.get $v))
               (then (return (i32.const 1))))
           (call $struct-has-property?/i32
                 (local.get $v)
                 (ref.cast (ref $StructTypeProperty)
                           (global.get $prop:input-port))))

     ;; output-port-like?/i32 : any/c -> i32
     ;;   Recognize native output ports and prop:output-port wrappers.
     (func $output-port-like?/i32
           (param $v (ref eq))
           (result   i32)
           (if (ref.test (ref $OutputPort) (local.get $v))
               (then (return (i32.const 1))))
           (call $struct-has-property?/i32
                 (local.get $v)
                 (ref.cast (ref $StructTypeProperty)
                           (global.get $prop:output-port))))

     ;; prop-output-port-target : any/c -> (or/c output-port? #f)
     ;;   Resolve a prop:output-port wrapper to its underlying output port.
     ;;   The Reference allows either a direct output port or a field index
     ;;   whose immutable field contains an output port. If the property is
     ;;   present but unusable, Racket uses a discard port; WebRacket has no
     ;;   discard-port subtype yet, so return #f and let callers follow their
     ;;   existing unsupported-output-port path.
     (func $prop-output-port-target
           (param $v (ref eq))
           (result   (ref eq))

           (local $struct   (ref $Struct))
           (local $type     (ref $StructType))
           (local $sentinel (ref eq))
           (local $prop-val (ref eq))
           (local $raw      i32)
           (local $idx      i32)
           (local $fields   (ref $Array))
           (local $field-v  (ref eq))

           (if (ref.test (ref $OutputPort) (local.get $v))
               (then (return (local.get $v))))
           (if (i32.eqz (ref.test (ref $Struct) (local.get $v)))
               (then (return (global.get $false))))
           (local.set $struct (ref.cast (ref $Struct) (local.get $v)))
           (local.set $type   (struct.get $Struct $type (local.get $struct)))
           (local.set $sentinel (call $cons (global.get $false)
                                      (global.get $false)))
           (local.set $prop-val
                      (call $struct-type-property-lookup
                            (local.get $type)
                            (ref.cast (ref $StructTypeProperty)
                                      (global.get $prop:output-port))
                            (local.get $sentinel)))
           (if (ref.eq (local.get $prop-val) (local.get $sentinel))
               (then (return (global.get $false))))
           (if (ref.test (ref $OutputPort) (local.get $prop-val))
               (then (return (local.get $prop-val))))
           (if (i32.eqz (ref.test (ref i31) (local.get $prop-val)))
               (then (return (global.get $false))))
           (local.set $raw (i31.get_u (ref.cast (ref i31) (local.get $prop-val))))
           (if (i32.and (local.get $raw) (i32.const 1))
               (then (return (global.get $false))))
           (local.set $idx (i32.shr_u (local.get $raw) (i32.const 1)))
           (local.set $fields (struct.get $Struct $fields (local.get $struct)))
           (if (i32.ge_u (local.get $idx) (array.len (local.get $fields)))
               (then (return (global.get $false))))
           (local.set $field-v (array.get $Array (local.get $fields) (local.get $idx)))
           (if (result (ref eq))
               (ref.test (ref $OutputPort) (local.get $field-v))
               (then (local.get $field-v))
               (else (global.get $false))))

     ;; port-like?/i32 : any/c -> i32
     ;;   Recognize native ports and prop:input-port/prop:output-port wrappers.
     (func $port-like?/i32
           (param $v (ref eq))
           (result   i32)
           (if (ref.test (ref $Port) (local.get $v))
               (then (return (i32.const 1))))
           (i32.or (call $input-port-like?/i32 (local.get $v))
                   (call $output-port-like?/i32 (local.get $v))))

     (func $port? (type $Prim1)
           (param $v (ref eq)) ;; any/c
           (result   (ref eq))
           (if (result (ref eq))
               (call $port-like?/i32 (local.get $v))
               (then (global.get $true))
               (else (global.get $false))))

     (func $input-port? (type $Prim1)
           (param $v (ref eq)) ;; any/c
           (result   (ref eq))
           (if (result (ref eq))
               (call $input-port-like?/i32 (local.get $v))
               (then (global.get $true))
               (else (global.get $false))))

     ;; file-stream-port? : any/c -> boolean?
     ;;   Recognize VFS-backed file ports. WebRacket currently represents
     ;;   input files as input-string ports with a path source name, while
     ;;   output files have a dedicated OutputFilePort subtype.
     (func $file-stream-port? (type $Prim1)
           (param $v (ref eq)) ;; any value
           (result   (ref eq))

           (local $in (ref $InputStringPort))

           (if (ref.test (ref $OutputFilePort) (local.get $v))
               (then (return (global.get $true))))
           (if (ref.test (ref $InputStringPort) (local.get $v))
               (then
                (local.set $in (ref.cast (ref $InputStringPort) (local.get $v)))
                (if (ref.test (ref $Path)
                              (struct.get $InputStringPort $name (local.get $in)))
                    (then (return (global.get $true))))))
           (global.get $false))

     ;; terminal-port? : any/c -> boolean?
     ;;   WebRacket currently has string/VFS/browser ports, but no
     ;;   interactive terminal port. The predicate is intentionally false
     ;;   until a terminal-backed port subtype exists.
     (func $terminal-port? (type $Prim1)
           (param $v (ref eq)) ;; any value
           (result   (ref eq))
           (global.get $false))

     (func $output-port? (type $Prim1)
           (param $v (ref eq)) ;; any/c
           (result   (ref eq))
           (if (result (ref eq))
               (call $output-port-like?/i32 (local.get $v))
               (then (global.get $true))
               (else (global.get $false)))))))

;; port-state-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate port status, current-port, flush, and close helpers.
(define port-state-runtime-only-helpers
  (function-forms->runtime-helpers
   'port-state-runtime-only-helpers
   '(
     (func $port-closed? (type $Prim1)
           (param $p (ref eq)) ;; port?
           (result   (ref eq))

           (local $port (ref $Port))

           (if (i32.eqz (ref.test (ref $Port) (local.get $p)))
               (then (call $raise-argument-error1
                           (global.get $symbol:port-closed?)
                           (global.get $string:port?)
                           (local.get $p))
                     (unreachable)))
           (local.set $port (ref.cast (ref $Port) (local.get $p)))
           (if (result (ref eq))
               (struct.get $Port $closed (local.get $port))
               (then (global.get $true))
               (else (global.get $false))))

     ;; close-input-port : input-port? -> void?
     ;;   Call a custom input port's close thunk once, then mark the port closed.
     (func $close-input-port (type $Prim1)
           (param $p (ref eq)) ;; input-port?
           (result   (ref eq))

           (local $port       (ref $InputPort))
           (local $custom     (ref $CustomInputPort))
           (local $close-proc (ref eq))
           (local $f          (ref $Procedure))
           (local $finv       (ref $ProcedureInvoker))
           (local $args       (ref $Args))

           (if (i32.eqz (ref.test (ref $InputPort) (local.get $p)))
               (then (call $raise-argument-error1
                           (global.get $symbol:close-input-port)
                           (global.get $string:input-port?)
                           (local.get $p))
                     (unreachable)))
           (local.set $port (ref.cast (ref $InputPort) (local.get $p)))
           (if (struct.get $InputPort $closed (local.get $port))
               (then (return (global.get $void))))
           (if (ref.test (ref $CustomInputPort) (local.get $p))
               (then
                (local.set $custom (ref.cast (ref $CustomInputPort) (local.get $p)))
                (local.set $close-proc
                           (struct.get $CustomInputPort $close-proc
                                       (local.get $custom)))
                (if (ref.test (ref $Procedure) (local.get $close-proc))
                    (then
                     (local.set $f (ref.cast (ref $Procedure)
                                             (local.get $close-proc)))
                     (local.set $finv
                                (struct.get $Procedure $invoke (local.get $f)))
                     (local.set $args (array.new_fixed $Args 0))
                     (drop (call_ref $ProcedureInvoker
                                     (local.get $f)
                                     (local.get $args)
                                     (local.get $finv)))))))
           (struct.set $InputPort $closed (local.get $port) (i32.const 1))
           (global.get $void))

     ;; current-input-port : [input-port?] -> input-port? or void?
     ;;   Get or set the runtime current input port.
     (func $current-input-port (type $Prim01)
           (param $p (ref eq)) ;; optional input-port?
           (result   (ref eq))

           (if (ref.eq (local.get $p) (global.get $missing))
               (then (return (global.get $current-input-port-value))))
           (if (i32.eqz (ref.test (ref $InputPort) (local.get $p)))
               (then (call $raise-argument-error1
                           (global.get $symbol:current-input-port)
                           (global.get $string:input-port?)
                           (local.get $p))
                     (unreachable)))
           (global.set $current-input-port-value (local.get $p))
           (global.get $void))

     ;; current-output-port : [output-port?] -> output-port? or void?
     ;;   Get or set the runtime current output port.
     (func $current-output-port (type $Prim01)
           (param $p (ref eq)) ;; optional output-port?
           (result   (ref eq))

           (if (ref.eq (local.get $p) (global.get $missing))
               (then (return (global.get $current-output-port-value))))
           (if (i32.eqz (ref.test (ref $OutputPort) (local.get $p)))
               (then (call $raise-argument-error1
                           (global.get $symbol:current-output-port)
                           (global.get $string:output-port?)
                           (local.get $p))
                     (unreachable)))
           (global.set $current-output-port-value (local.get $p))
           (global.get $void))

     ;; current-error-port : [output-port?] -> output-port? or void?
     ;;   Get or set the runtime current error port.
     (func $current-error-port (type $Prim01)
           (param $p (ref eq)) ;; optional output-port?
           (result   (ref eq))

           (if (ref.eq (local.get $p) (global.get $missing))
               (then (return (global.get $current-error-port-value))))
           (if (i32.eqz (ref.test (ref $OutputPort) (local.get $p)))
               (then (call $raise-argument-error1
                           (global.get $symbol:current-error-port)
                           (global.get $string:output-port?)
                           (local.get $p))
                     (unreachable)))
           (global.set $current-error-port-value (local.get $p))
           (global.get $void))

     ;; reset-current-input-port! : -> void?
     ;;   Reset the runtime current input port to an empty string port.
     (func $reset-current-input-port! (type $Prim0)
           (result (ref eq))

           (global.set $current-input-port-value
                       (call $open-input-string
                             (global.get $string:empty)
                             (global.get $missing)))
           (global.get $void))

     ;; reset-current-output-port! : -> void?
     ;;   Reset the runtime current output port to a fresh output string port.
     (func $reset-current-output-port! (type $Prim0)
           (result (ref eq))

           (global.set $current-output-port-value
                       (call $open-output-string (global.get $missing)))
           (global.get $void))

     ;; reset-current-error-port! : -> void?
     ;;   Reset the runtime current error port to a fresh output string port.
     (func $reset-current-error-port! (type $Prim0)
           (result (ref eq))

           (global.set $current-error-port-value
                       (call $open-output-string (global.get $missing)))
           (global.get $void))

     ;; flush-output : [output-port?] -> void?
     ;;   Flush a VFS output file port; string and bytes output ports are no-ops.
     (func $flush-output (type $Prim01)
           (param $p (ref eq)) ;; optional output-port?, default = current output port
           (result   (ref eq))

           (local $port      (ref $OutputPort))
           (local $file-port (ref $OutputFilePort))

           (if (ref.eq (local.get $p) (global.get $missing))
               (then (local.set $p (call $current-output-port
                                         (global.get $missing)))))
           (if (i32.eqz (ref.test (ref $OutputPort) (local.get $p)))
               (then (call $raise-argument-error1
                           (global.get $symbol:flush-output)
                           (global.get $string:output-port?)
                           (local.get $p))
                     (unreachable)))
           (local.set $port (ref.cast (ref $OutputPort) (local.get $p)))
           (if (struct.get $OutputPort $closed (local.get $port))
               (then (call $raise-output-port-closed
                           (global.get $string:flush-output:output-port-closed))
                     (unreachable)))
           (if (ref.test (ref $OutputFilePort) (local.get $p))
               (then
                (local.set $file-port (ref.cast (ref $OutputFilePort) (local.get $p)))
                (drop (call $webracket-vfs-write-file
                            (struct.get $OutputFilePort $path (local.get $file-port))
                            (call $get-output-bytes
                                  (local.get $p)
                                  (global.get $missing)
                                  (global.get $missing)
                                  (global.get $missing))))))
           (global.get $void))

     ;; close-output-port : output-port? -> void?
     ;;   Flush a VFS file output port if needed, then mark the port closed.
     (func $close-output-port (type $Prim1)
           (param $p (ref eq)) ;; output-port?
           (result   (ref eq))

           (local $port       (ref $OutputPort))
           (local $custom     (ref $CustomOutputPort))
           (local $close-proc (ref eq))
           (local $f          (ref $Procedure))
           (local $finv       (ref $ProcedureInvoker))

           (if (i32.eqz (ref.test (ref $OutputPort) (local.get $p)))
               (then (call $raise-argument-error1
                           (global.get $symbol:close-output-port)
                           (global.get $string:output-port?)
                           (local.get $p))
                     (unreachable)))
           (local.set $port (ref.cast (ref $OutputPort) (local.get $p)))
           (if (struct.get $OutputPort $closed (local.get $port))
               (then (return (global.get $void))))
           (drop (call $flush-output (local.get $p)))
           (if (ref.test (ref $CustomOutputPort) (local.get $p))
               (then
                (local.set $custom (ref.cast (ref $CustomOutputPort) (local.get $p)))
                (local.set $close-proc
                           (struct.get $CustomOutputPort $close-proc (local.get $custom)))
                (local.set $f (ref.cast (ref $Procedure) (local.get $close-proc)))
                (local.set $finv (struct.get $Procedure $invoke (local.get $f)))
                (drop (call_ref $ProcedureInvoker
                                (local.get $f)
                                (array.new_fixed $Args 0)
                                (local.get $finv)))))
           (struct.set $OutputPort $closed (local.get $port) (i32.const 1))
           (global.get $void)))))

;; port-location-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate port line-count, position, and location helpers.
(define port-location-runtime-only-helpers
  (function-forms->runtime-helpers
   'port-location-runtime-only-helpers
   '(
     ;; Note:
     ;;   WebRacket's current string ports always track line and column
     ;;   information, so enabling line counting is a no-op.
     ;;
     ;;   For now the primitives port-count-lines! and port-counts-lines?
     ;;   exists to preserve compatibility with Racket
     ;;   programs that explicitly request line counting.
     ;;
     ;;   When support for file ports is added, these functions needs
     ;;   to be revised.

     (func $port-count-lines! (type $Prim1)
           (param $p (ref eq))
           (result   (ref eq))

           (local $port (ref $Port))

           (if (i32.eqz (call $port-like?/i32 (local.get $p)))
               (then (call $raise-argument-error1
                           (global.get $symbol:port-count-lines!)
                           (global.get $string:port?)
                           (local.get $p))
                     (unreachable)))
           (if (ref.test (ref $Port) (local.get $p))
               (then
                (local.set $port (ref.cast (ref $Port) (local.get $p)))
                (if (struct.get $Port $closed (local.get $port))
                    (then
                     (if (ref.test (ref $OutputPort) (local.get $p))
                         (then (call $raise-output-port-closed
                                     (global.get $string:port-count-lines!:output-port-closed)))
                         (else (call $raise-input-port-closed
                                     (global.get $string:port-count-lines!:input-port-closed))))
                     (unreachable)))))
           (global.get $void))

     ;; Note:
     ;;   See previous note.
     (func $port-counts-lines? (type $Prim1)
           (param $p (ref eq))
           (result   (ref eq))

           (local $port (ref $Port))

           (if (i32.eqz (call $port-like?/i32 (local.get $p)))
               (then (call $raise-argument-error1
                           (global.get $symbol:port-counts-lines?)
                           (global.get $string:port?)
                           (local.get $p))
                     (unreachable)))
           (if (ref.test (ref $Port) (local.get $p))
               (then
                (local.set $port (ref.cast (ref $Port) (local.get $p)))
                (if (struct.get $Port $closed (local.get $port))
                    (then (return (global.get $false))))))
           (global.get $true))

     ;; file-position* : port? -> (or/c exact-nonnegative-integer? #f)
     ;;   Return the known byte position for WebRacket string/VFS ports.
     ;;   Other future port kinds should return #f when the position is
     ;;   unknown instead of raising.
     (func $file-position* (type $Prim1)
           (param $p (ref eq)) ;; port?
           (result   (ref eq))

           (local $port (ref $Port))

           (if (i32.eqz (ref.test (ref $Port) (local.get $p)))
               (then (call $raise-argument-error1
                           (global.get $symbol:file-position*)
                           (global.get $string:port?)
                           (local.get $p))
                     (unreachable)))
           (local.set $port (ref.cast (ref $Port) (local.get $p)))
           (if (struct.get $Port $closed (local.get $port))
               (then (if (ref.test (ref $OutputPort) (local.get $p))
                         (then (call $raise-output-port-closed
                                     (global.get $string:file-position*:output-port-closed)))
                         (else (call $raise-input-port-closed
                                     (global.get $string:file-position*:input-port-closed))))
                     (unreachable)))
           (if (ref.test (ref $InputPort) (local.get $p))
               (then (return (ref.i31
                              (i32.shl
                               (struct.get $InputPort $idx
                                           (ref.cast (ref $InputPort) (local.get $p)))
                               (i32.const 1))))))
           (if (ref.test (ref $OutputPort) (local.get $p))
               (then (return (ref.i31
                              (i32.shl
                               (struct.get $OutputPort $idx
                                           (ref.cast (ref $OutputPort) (local.get $p)))
                               (i32.const 1))))))
           (global.get $false))

     (func $port-next-location (type $Prim1)
           (param $p (ref eq))
           (result   (ref eq))

           (local $port (ref null $Port))
           (local $loc  (ref null $Location))
           (local $original (ref eq))
           ;; 1. Check and cast $p to (ref $Port). Some Goal 1 formatter
           ;; paths can pass higher-level port wrappers before WebRacket
           ;; fully models prop:input-port/prop:output-port. Preserve the
           ;; Reference-mandated three-result shape; when possible, resolve
           ;; prop:output-port to the underlying native port first.
           (local.set $original (local.get $p))
           (if (ref.test (ref $Port) (local.get $p))
               (then (local.set $port (ref.cast (ref $Port) (local.get $p))))
               (else
                (local.set $p (call $prop-output-port-target (local.get $p)))
                (if (ref.test (ref $Port) (local.get $p))
                    (then (local.set $port (ref.cast (ref $Port) (local.get $p))))
                    (else (call $raise-argument-error1
                                (global.get $symbol:port-next-location)
                                (global.get $string:port?)
                                (local.get $original))
                          (unreachable)))))
           (if (struct.get $Port $closed (local.get $port))
               (then (if (ref.test (ref $OutputPort) (local.get $p))
                         (then (call $raise-output-port-closed
                                     (global.get $string:port-next-location:output-port-closed)))
                         (else (call $raise-input-port-closed
                                     (global.get $string:port-next-location:input-port-closed))))
                     (unreachable)))
           ;; 2. Get location struct from the port
           (local.set $loc (struct.get $Port $loc (local.get $port)))
           ;; 3. Extract and return as fixed array of 3 elements
           (array.new_fixed $Values 3
                            (struct.get $Location $line (local.get $loc))
                            (struct.get $Location $col  (local.get $loc))
                            (struct.get $Location $pos  (local.get $loc)))))))

;; port-byte-string-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate byte/string port constructors and byte extraction helpers.
(define port-byte-string-runtime-only-helpers
  (function-forms->runtime-helpers
   'port-byte-string-runtime-only-helpers
   '(
     (func $open-input-bytes (type $Prim12)
           (param $bstr-raw (ref eq)) ;; bytes?
           (param $name     (ref eq)) ;; optional any/c, default = 'string
           (result          (ref eq))

           (local $bstr      (ref $Bytes))
           (local $port-bs   (ref $Bytes))
           (local $name-val  (ref eq))
           (local $src-arr   (ref $I8Array))
           (local $arr       (ref $I8Array))
           (local $len       i32)
           (local $loc       (ref $Location))
           (local $immutable i32)

           ;; --- Initialize non-defaultable locals ---
           (local.set $bstr    (ref.cast (ref $Bytes) (global.get $bytes:empty)))
           (local.set $port-bs (ref.cast (ref $Bytes) (global.get $bytes:empty)))
           ;; --- Validate byte string argument ---
           (if (i32.eqz (ref.test (ref $Bytes) (local.get $bstr-raw)))
               (then (call $raise-argument-error1
                           (global.get $symbol:open-input-bytes)
                           (global.get $string:bytes?)
                           (local.get $bstr-raw))
                     (unreachable)))
           (local.set $bstr (ref.cast (ref $Bytes) (local.get $bstr-raw)))
           ;; --- Determine port name, honoring optional argument ---
           (local.set $name-val
                      (if (result (ref eq))
                          (ref.eq (local.get $name) (global.get $missing))
                          (then (global.get $symbol:string))
                          (else (local.get $name))))
           ;; --- Ensure the port holds an immutable copy of the bytes ---
           (local.set $immutable (struct.get $Bytes $immutable (local.get $bstr)))
           (local.set $src-arr (struct.get $Bytes $bs (local.get $bstr)))
           (if (i32.eq (local.get $immutable) (i32.const 1))
               (then (local.set $port-bs (local.get $bstr)))
               (else
                (local.set $port-bs
                           (struct.new $Bytes
                                       (struct.get $Bytes $hash (local.get $bstr))
                                       (i32.const 1)
                                       (call $i8array-copy
                                             (local.get $src-arr)
                                             (i32.const 0)
                                             (call $i8array-length (local.get $src-arr)))))))
           ;; --- Cache backing array and byte length ---
           (local.set $arr (struct.get $Bytes $bs (local.get $port-bs)))
           (local.set $len (array.len (local.get $arr)))
           ;; --- Initialize location and construct the port ---
           (local.set $loc (ref.cast (ref $Location) (call $make-initial-location)))
           (struct.new $InputStringPort
                       (i32.const 0)         ;; $hash
                       (local.get $name-val) ;; $name
                       (i32.const 0)         ;; $closed
                       (local.get $port-bs)  ;; $bytes
                       (local.get $len)      ;; $len
                       (i32.const 0)         ;; $idx
                       (local.get $loc)      ;; $loc
                       (global.get $false)   ;; $read-handler
                       (i32.const 0)         ;; $utf8-len
                       (i32.const 0)         ;; $utf8-left
                       (i32.const 0)))       ;; $utf8-bytes

     (func $open-input-string (type $Prim12)
           (param $str-raw (ref eq)) ;; string?
           (param $name    (ref eq)) ;; optional any/c, default = 'string
           (result         (ref eq))

           (local $str      (ref $String))
           (local $name-val (ref eq))
           (local $bytes    (ref $Bytes))
           (local $arr      (ref $I8Array))
           (local $len      i32)
           (local $loc      (ref $Location))

           ;; --- Validate string argument ---
           (if (i32.eqz (ref.test (ref $String) (local.get $str-raw)))
               (then (call $raise-argument-error1
                           (global.get $symbol:open-input-string)
                           (global.get $string:string?)
                           (local.get $str-raw))
                     (unreachable)))
           (local.set $str (ref.cast (ref $String) (local.get $str-raw)))
           ;; --- Determine port name, honoring optional argument ---
           (local.set $name-val
                      (if (result (ref eq))
                          (ref.eq (local.get $name) (global.get $missing))
                          (then (global.get $symbol:string))
                          (else (local.get $name))))
           ;; --- Convert string contents to immutable UTF-8 bytes ---
           (local.set $bytes
                      (ref.cast (ref $Bytes)
                                (call $string->bytes/utf-8
                                      (local.get $str)
                                      (global.get $false)
                                      (global.get $false)
                                      (global.get $false))))
           (local.set $arr (struct.get $Bytes $bs (local.get $bytes)))
           (local.set $len (array.len (local.get $arr)))
           ;; --- Initialize location and construct the port ---
           (local.set $loc (ref.cast (ref $Location) (call $make-initial-location)))
           (struct.new $InputStringPort
                       (i32.const 0)          ;; $hash
                       (local.get $name-val)  ;; $name
                       (i32.const 0)          ;; $closed
                       (local.get $bytes)     ;; $bytes
                       (local.get $len)       ;; $len (byte length)
                       (i32.const 0)          ;; $idx
                       (local.get $loc)       ;; $loc
                       (global.get $false)    ;; $read-handler
                       (i32.const 0)          ;; $utf8-len
                       (i32.const 0)          ;; $utf8-left
                       (i32.const 0)))        ;; $utf8-bytes


     (func $open-output-bytes (type $Prim01)
           (param $name (ref eq)) ;; optional any/c, default = 'string
           (result (ref eq))

           (local $bs       (ref $Bytes))
           (local $loc      (ref $Location))
           (local $name-val (ref eq))
           (local $port     (ref $OutputStringPort))
           ;; Step 1: Allocate the backing I8Array with capacity 32 and fill with 0
           (local.set $bs
                      (struct.new $Bytes
                        (i32.const 0)                                       ;; hash = 0
                        (i32.const 0)                                       ;; mutable = false
                        (call $i8make-array (i32.const 32) (i32.const 0)))) ;; backing array
           ;; Step 2: Make initial location: (line 1, col 0, pos 1)
           (local.set $loc (ref.cast (ref $Location) (call $make-initial-location)))
           ;; Step 3: Determine the port name, defaulting to 'string
           (local.set $name-val
                      (if (result (ref eq))
                          (ref.eq (local.get $name) (global.get $missing))
                          (then (global.get $symbol:string))
                          (else (local.get $name))))
           ;; Step 4: Construct and return the StringPort
           (local.set $port
                      (struct.new $OutputStringPort
                                  (i32.const 0)                 ;; $hash
                                  (local.get $name-val)         ;; $name  : (ref eq)
                                  (i32.const 0)                 ;; $closed
                                  (local.get $bs)               ;; $bytes : (ref $Bytes)
                                  (i32.const 32)                ;; $len   : i32
                                  (i32.const 0)                 ;; $idx   : i32
                                  (local.get $loc)              ;; $loc   : (ref $Location)
                                  (i32.const 0)                 ;; $utf8-len
                                  (i32.const 0)                 ;; $utf8-left
                                  (i32.const 0)))               ;; $utf8-bytes
           (local.get $port))

     (func $open-output-string (type $Prim01)
           (param $name (ref eq)) ;; optional any/c, default = 'string
           (result (ref eq))

           (local $port     (ref $OutputStringPort))
           (local $name-val (ref eq))
           ;; Determine the port name, defaulting to 'string when omitted
           (local.set $name-val
                      (if (result (ref eq))
                          (ref.eq (local.get $name) (global.get $missing))
                          (then (global.get $symbol:string))
                          (else (local.get $name))))
           ;; Reuse the byte-backed string port and update its name field
           (local.set $port (ref.cast (ref $OutputStringPort)
                                      (call $open-output-bytes (global.get $missing))))
           (struct.set $OutputStringPort $name (local.get $port) (local.get $name-val))
           (local.get $port))

     (func $format-get-output-bytes:port-content
           (param $port (ref null $OutputStringPort))
           (result      (ref $String))

           (call $string-append/2
                 (call $string-append/2
                       (global.get $string:get-output-bytes:output-port-prefix)
                       (call $format/display
                             (struct.get $OutputStringPort $name (local.get $port))))
                 (global.get $string:get-output-bytes:output-port-suffix)))

     (func $raise-get-output-bytes:port-expected
           (param $x (ref eq))

           (call $raise-argument-error1
                 (global.get $symbol:get-output-bytes)
                 (global.get $string:get-output-bytes:output-string-port-contract)
                 (local.get $x))
           (unreachable))

     (func $raise-get-output-bytes:start-expected
           (param $x (ref eq))

           (call $raise-argument-error1
                 (global.get $symbol:get-output-bytes)
                 (global.get $string:exact-nonnegative-integer?)
                 (local.get $x))
           (unreachable))

     (func $raise-get-output-bytes:end-expected
           (param $x (ref eq))

           (call $raise-argument-error1
                 (global.get $symbol:get-output-bytes)
                 (global.get $string:get-output-bytes:end-index-contract)
                 (local.get $x))
           (unreachable))

     (func $raise-get-output-bytes:range
           (param $reason      (ref eq))
           (param $index-label (ref eq))
           (param $port        (ref null $OutputStringPort))
           (param $idx         i32)
           (param $range-start i32)
           (param $range-end   i32)

           (local $out     (ref $GrowableArray))
           (local $message (ref $String))

           (local.set $out
                      (call $make-growable-array (i32.const 11)))
           (call $growable-array-add! (local.get $out)
                 (call $format/display (global.get $symbol:get-output-bytes)))
           (call $growable-array-add! (local.get $out)
                 (local.get $reason))
           (call $growable-array-add! (local.get $out)
                 (local.get $index-label))
           (call $growable-array-add! (local.get $out)
                 (call $format/display
                       (call $integer-from-i64
                             (i64.extend_i32_u (local.get $idx)))))
           (call $growable-array-add! (local.get $out)
                 (global.get $string:get-output-bytes:valid-range-open))
           (call $growable-array-add! (local.get $out)
                 (call $format/display
                       (call $integer-from-i64
                             (i64.extend_i32_u (local.get $range-start)))))
           (call $growable-array-add! (local.get $out)
                 (global.get $string:get-output-bytes:valid-range-separator))
           (call $growable-array-add! (local.get $out)
                 (call $format/display
                       (call $integer-from-i64
                             (i64.extend_i32_u (local.get $range-end)))))
           (call $growable-array-add! (local.get $out)
                 (global.get $string:string-ref:valid-range-close))
           (call $growable-array-add! (local.get $out)
                 (global.get $string:get-output-bytes:port-content))
           (call $growable-array-add! (local.get $out)
                 (call $format-get-output-bytes:port-content (local.get $port)))
           (local.set $message
                      (call $growable-array-of-strings->string (local.get $out)))
           (call $raise
                 (call $exn:fail:contract/make
                       (local.get $message)
                       (call $current-continuation-marks (global.get $missing)))
                 (global.get $true))
           (unreachable))

     (func $get-output-bytes (type $Prim14)
           (param $out    (ref eq))
           (param $reset? (ref eq)) ;; optional any/c, default = #f
           (param $start  (ref eq)) ;; optional exact-nonnegative-integer?, default = 0
           (param $end    (ref eq)) ;; optional exact-nonnegative-integer?, default = current output length
           (result        (ref eq))

           (local $sp   (ref null $OutputStringPort))
           (local $bs   (ref $Bytes))
           (local $idx  i32)
           (local $from i32)
           (local $to   i32)
           (local $src  (ref $I8Array))
           (local $dest (ref $I8Array))
           (local $res  (ref $Bytes))
           ;; 1. Check that $out is a StringPort
           (if (ref.test (ref $OutputStringPort) (local.get $out))
               (then (local.set $sp (ref.cast (ref $OutputStringPort) (local.get $out))))
               (else (call $raise-get-output-bytes:port-expected (local.get $out))))
           ;; 2. Get internal byte string and index
           (local.set $bs  (struct.get $OutputStringPort $bytes (local.get $sp)))
           (local.set $idx (struct.get $OutputStringPort $idx   (local.get $sp)))
           ;; 3. Extract the I8Array from the Bytes object
           (local.set $src (struct.get $Bytes $bs (local.get $bs)))
           ;; 4. Decode the optional slice bounds. The visible content is
           ;;    the written prefix [0, idx), not the backing capacity.
           (if (ref.eq (local.get $start) (global.get $missing))
               (then (local.set $from (i32.const 0)))
               (else
                (local.set $from
                           (call $exact-nonnegative-integer->i32/saturating
                                 (local.get $start)))
                (if (i32.lt_s (local.get $from) (i32.const 0))
                    (then (call $raise-get-output-bytes:start-expected
                                (local.get $start))))))
           (if (i32.or (ref.eq (local.get $end) (global.get $missing))
                       (ref.eq (local.get $end) (global.get $false)))
               (then (local.set $to (local.get $idx)))
               (else
                (local.set $to
                           (call $exact-nonnegative-integer->i32/saturating
                                 (local.get $end)))
                (if (i32.lt_s (local.get $to) (i32.const 0))
                    (then (call $raise-get-output-bytes:end-expected
                                (local.get $end))))))
           (if (i32.gt_u (local.get $from) (local.get $idx))
               (then (call $raise-get-output-bytes:range
                           (global.get $string:substring:starting-index-out-of-range)
                           (global.get $string:substring:starting-index)
                           (local.get $sp)
                           (local.get $from)
                           (i32.const 0)
                           (local.get $idx))))
           (if (i32.or (i32.gt_u (local.get $to) (local.get $idx))
                       (i32.lt_u (local.get $to) (local.get $from)))
               (then (call $raise-get-output-bytes:range
                           (global.get $string:substring:ending-index-out-of-range)
                           (global.get $string:substring:ending-index)
                           (local.get $sp)
                           (local.get $to)
                           (local.get $from)
                           (local.get $idx))))
           ;; 5. Allocate a new array for the requested slice.
           (local.set $dest (call $i8make-array
                                  (i32.sub (local.get $to) (local.get $from))
                                  (i32.const 0)))
           ;; 6. Copy contents into the new array
           (drop (call $i8array-copy!/error
                       (local.get $dest)            ;; dest
                       (i32.const 0)                ;; dest-start
                       (local.get $src)             ;; src
                       (local.get $from)             ;; src-start
                       (local.get $to)))             ;; src-end
           ;; 7. Construct new Bytes object
           (local.set $res
                      (struct.new $Bytes
                                  (i32.const 0)              ;; hash
                                  (i32.const 1)              ;; immutable
                                  (local.get $dest)))        ;; backing array
           ;; 8. A true reset? clears the visible output but keeps capacity.
           (if (i32.and
                (i32.eqz (ref.eq (local.get $reset?) (global.get $false)))
                (i32.eqz (ref.eq (local.get $reset?) (global.get $missing))))
               (then (struct.set $OutputStringPort $idx (local.get $sp) (i32.const 0))
                     (struct.set $OutputStringPort $utf8-len (local.get $sp) (i32.const 0))
                     (struct.set $OutputStringPort $utf8-left (local.get $sp) (i32.const 0))
                     (struct.set $OutputStringPort $utf8-bytes (local.get $sp) (i32.const 0))))
           ;; 9. Return the new Bytes
           (local.get $res)))))

;; port-output-string-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate output string extraction and convenience helpers.
(define port-output-string-runtime-only-helpers
  (function-forms->runtime-helpers
   'port-output-string-runtime-only-helpers
   (list
    `(func $get-output-string (type $Prim1)
           (param $out (ref eq))
           (result     (ref eq))

           (local $bs (ref $Bytes))

           (local.set $bs
                      (ref.cast (ref $Bytes)
                                (call $get-output-bytes
                                      (local.get $out)
                                      (global.get $missing)
                                      (global.get $missing)
                                      (global.get $missing))))
           (call $bytes->string/utf-8
                 (local.get $bs)
                 ;; Replacement character #\uFFFD
                 (ref.i31 (i32.or (i32.shl (i32.const 65533) (i32.const ,char-shift))
                                  (i32.const ,char-tag)))
                 (global.get $missing)
                 (global.get $missing)))

    '(func $call-with-output-string (type $Prim1)
           (param $proc (ref eq)) ;; procedure?
           (result      (ref eq))

           (local $f    (ref $Procedure))
           (local $finv (ref $ProcedureInvoker))
           (local $port (ref $OutputStringPort))
           (local $args (ref $Args))

           ;; 1. Ensure the argument is a one-argument procedure
           (local.set $f
                      (call $check-procedure-arity/who
                            (global.get $symbol:call-with-output-string)
                            (global.get $string:call-with-output-string-procedure)
                            (local.get $proc)
                            (i32.const 1)))

           ;; 2. Open a fresh output string port
           (local.set $port
                      (ref.cast (ref $OutputStringPort)
                                (call $open-output-string (global.get $missing))))

           ;; 3. Prepare the procedure invocation with the port argument
           (local.set $finv (struct.get $Procedure $invoke (local.get $f)))
           (local.set $args (array.new_fixed $Args 1 (local.get $port)))

           ;; 4. Invoke the procedure and ignore its result
           (drop (call_ref $ProcedureInvoker
                           (local.get $f)
                           (local.get $args)
                           (local.get $finv)))

           ;; 5. Return the accumulated output as a string
           (call $get-output-string (local.get $port))))))

;; port-custom-constructor-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate custom input and output port constructors.
(define port-custom-constructor-runtime-only-helpers
  (function-forms->runtime-helpers
   'port-custom-constructor-runtime-only-helpers
   '(
     (func $check-port-procedure-or-false/who
           (param $who      (ref eq))
           (param $expected (ref eq))
           (param $v        (ref eq))
           (param $arity    i32)

           (if (ref.eq (local.get $v) (global.get $false))
               (then)
               (else
                (drop (call $check-procedure-arity/who
                            (local.get $who)
                            (local.get $expected)
                            (local.get $v)
                            (local.get $arity))))))

     (func $check-port-init-position/who
           (param $who (ref eq))
           (param $v   (ref eq))

           (if (ref.eq (local.get $v) (global.get $false))
               (then (return)))
           (if (call $port-like?/i32 (local.get $v))
               (then (return)))
           (if (ref.eq (call $exact-positive-integer? (local.get $v))
                       (global.get $true))
               (then (return)))
           (if (ref.eq (call $procedure? (local.get $v))
                       (global.get $true))
               (then (drop (call $check-procedure-arity/who
                                  (local.get $who)
                                  (global.get $string:port-init-position-contract)
                                  (local.get $v)
                                  (i32.const 0)))
                     (return)))
           (call $raise-argument-error1
                 (local.get $who)
                 (global.get $string:port-init-position-contract)
                 (local.get $v))
           (unreachable))

     (func $check-port-buffer-mode/who
           (param $who (ref eq))
           (param $v   (ref eq))

           (if (ref.eq (local.get $v) (global.get $false))
               (then)
               (else
                (drop (call $check-procedure-arity/who
                            (local.get $who)
                            (global.get $string:port-buffer-mode-contract)
                            (local.get $v)
                            (i32.const 0)))
                (drop (call $check-procedure-arity/who
                            (local.get $who)
                            (global.get $string:port-buffer-mode-contract)
                            (local.get $v)
                            (i32.const 1))))))

     (func $check-output-write-special/who
           (param $who (ref eq))
           (param $v   (ref eq))

           (if (ref.eq (local.get $v) (global.get $false))
               (then (return)))
           (if (call $output-port-like?/i32 (local.get $v))
               (then (return)))
           (if (ref.eq (call $procedure? (local.get $v))
                       (global.get $true))
               (then (drop (call $check-procedure-arity/who
                                  (local.get $who)
                                  (global.get $string:output-port-write-special-contract)
                                  (local.get $v)
                                  (i32.const 3)))
                     (return)))
           (call $raise-argument-error1
                 (local.get $who)
                 (global.get $string:output-port-write-special-contract)
                 (local.get $v))
           (unreachable))

     ;; make-port-initial-location : init-position -> location
     ;;   Initialize custom-port locations from exact-positive integers and #f.
     (func $make-port-initial-location
           (param $init-position (ref eq))
           (result (ref $Location))

           (if (ref.eq (local.get $init-position) (global.get $false))
               (then (return
                      (struct.new $Location
                                  (i32.const 0)
                                  (global.get $false)
                                  (global.get $false)
                                  (global.get $false)))))
           (if (ref.eq (call $exact-positive-integer? (local.get $init-position))
                       (global.get $true))
               (then (return
                      (struct.new $Location
                                  (i32.const 0)
                                  (global.get $false)
                                  (global.get $false)
                                  (local.get $init-position)))))
           ;; Port and procedure init-position cases are accepted by the
           ;; constructor contract, but dynamic position tracking is not
           ;; modeled yet.
           (ref.cast (ref $Location) (call $make-initial-location)))

     ;; NOTE: Procedure-based custom ports currently support only immediate
     ;;       byte operations without events or special results.
     (func $make-input-port (type $Prim>=4)
           (param $name   (ref eq)) ;; any/c
           (param $read   (ref eq)) ;; input-port? or procedure?
           (param $peek   (ref eq)) ;; input-port? or #f
           (param $close  (ref eq)) ;; procedure?
           (param $rest   (ref eq)) ;; list of optional arguments (get-progress-evt commit get-location count-lines! init-position buffer-mode)
           (result        (ref eq))

           (local $args          (ref eq))
           (local $node          (ref $Pair))
           (local $arg           (ref eq))
           (local $count         i32)
           (local $progress-evt  (ref eq))
           (local $commit-proc   (ref eq))
           (local $get-location  (ref eq))
           (local $count-lines!  (ref eq))
           (local $init-position (ref eq))
           (local $buffer-mode   (ref eq))
           (local $loc           (ref $Location))
           (local $bytes         (ref $Bytes))

           ;; --- Initialize optional arguments with their defaults ---
           (local.set $progress-evt  (global.get $false))
           (local.set $commit-proc   (global.get $false))
           (local.set $get-location  (global.get $false))
           (local.set $count-lines!  (global.get $void))
           (local.set $init-position (ref.i31 (i32.shl (i32.const 1) (i32.const 1))))
           (local.set $buffer-mode   (global.get $false))

           ;; --- Decode optional rest arguments ---
           (local.set $args  (local.get $rest))
           (local.set $count (i32.const 0))
           (block $done
                  (loop $loop
                        (if (ref.eq (local.get $args) (global.get $null))
                            (then (br $done)))
                        (local.set $node  (ref.cast (ref $Pair) (local.get $args)))
                        (local.set $arg   (struct.get $Pair $a (local.get $node)))
                        (local.set $args  (struct.get $Pair $d (local.get $node)))
                        (local.set $count (i32.add (local.get $count) (i32.const 1)))
                        (if (i32.gt_u (local.get $count) (i32.const 6))
                            (then (call $primitive-invoke:raise-arity-error
                                        (ref.cast (ref $PrimitiveProcedure)
                                                  (global.get $prim:make-input-port))
                                        (local.get $count))
                                  (unreachable)))
                        (if (i32.eq (local.get $count) (i32.const 1))
                            (then (local.set $progress-evt (local.get $arg)))
                            (else
                             (if (i32.eq (local.get $count) (i32.const 2))
                                 (then (local.set $commit-proc (local.get $arg)))
                                 (else
                                  (if (i32.eq (local.get $count) (i32.const 3))
                                      (then (local.set $get-location (local.get $arg)))
                                      (else
                                       (if (i32.eq (local.get $count) (i32.const 4))
                                           (then (local.set $count-lines! (local.get $arg)))
                                           (else
                                            (if (i32.eq (local.get $count) (i32.const 5))
                                                (then (local.set $init-position (local.get $arg)))
                                                (else (local.set $buffer-mode (local.get $arg))))))))))))
                        (br $loop)))

           ;; --- Ensure read-in is either an input port or a one-argument procedure ---
           (if (i32.eqz (i32.or (ref.test (ref $InputPort) (local.get $read))
                                (ref.eq (call $procedure? (local.get $read))
                                        (global.get $true))))
               (then (call $raise-argument-error1
                           (global.get $symbol:make-input-port)
                           (global.get $string:input-port-read-contract)
                           (local.get $read))
                     (unreachable)))
           (if (ref.eq (call $procedure? (local.get $read))
                       (global.get $true))
               (then (drop (call $check-procedure-arity/who
                                  (global.get $symbol:make-input-port)
                                  (global.get $string:input-port-read-contract)
                                  (local.get $read)
                                  (i32.const 1)))))

           ;; --- Ensure peek-in is #f, an input port, or a three-argument procedure ---
           (if (i32.eqz (ref.eq (local.get $peek) (global.get $false)))
               (then
                (if (i32.eqz (i32.or (ref.test (ref $InputPort) (local.get $peek))
                                     (ref.eq (call $procedure? (local.get $peek))
                                             (global.get $true))))
                    (then (call $raise-argument-error1
                                (global.get $symbol:make-input-port)
                                (global.get $string:input-port-peek-contract)
                                (local.get $peek))
                          (unreachable)))
                (if (ref.eq (call $procedure? (local.get $peek))
                            (global.get $true))
                    (then (drop (call $check-procedure-arity/who
                                       (global.get $symbol:make-input-port)
                                       (global.get $string:input-port-peek-contract)
                                       (local.get $peek)
                                       (i32.const 3)))))))

           (drop (call $check-procedure-arity/who
                       (global.get $symbol:make-input-port)
                       (global.get $string:port-procedure-arity-0)
                       (local.get $close)
                       (i32.const 0)))
           (if (i32.ge_u (local.get $count) (i32.const 1))
               (then (call $check-port-procedure-or-false/who
                           (global.get $symbol:make-input-port)
                           (global.get $string:port-procedure-arity-0-or-false)
                           (local.get $progress-evt)
                           (i32.const 0))))
           (if (i32.ge_u (local.get $count) (i32.const 2))
               (then (call $check-port-procedure-or-false/who
                           (global.get $symbol:make-input-port)
                           (global.get $string:port-procedure-arity-3-or-false)
                           (local.get $commit-proc)
                           (i32.const 3))))
           (if (i32.ge_u (local.get $count) (i32.const 3))
               (then (call $check-port-procedure-or-false/who
                           (global.get $symbol:make-input-port)
                           (global.get $string:port-procedure-arity-0-or-false)
                           (local.get $get-location)
                           (i32.const 0))))
           (if (i32.ge_u (local.get $count) (i32.const 4))
               (then (call $check-port-procedure-or-false/who
                           (global.get $symbol:make-input-port)
                           (global.get $string:port-procedure-arity-0-or-false)
                           (local.get $count-lines!)
                           (i32.const 0))))
           (if (i32.ge_u (local.get $count) (i32.const 5))
               (then (call $check-port-init-position/who
                           (global.get $symbol:make-input-port)
                           (local.get $init-position))))
           (if (i32.ge_u (local.get $count) (i32.const 6))
               (then (call $check-port-buffer-mode/who
                           (global.get $symbol:make-input-port)
                           (local.get $buffer-mode))))

           (local.set $bytes (ref.cast (ref $Bytes) (global.get $bytes:empty)))
           (local.set $loc   (call $make-port-initial-location (local.get $init-position)))

           (struct.new $CustomInputPort
                       (i32.const 0)              ;; $hash
                       (local.get $name)          ;; $name
                       (i32.const 0)              ;; $closed
                       (local.get $bytes)         ;; $bytes (scratch buffer)
                       (i32.const 0)              ;; $len placeholder
                       (i32.const 0)              ;; $idx placeholder
                       (local.get $loc)           ;; $loc
                       (global.get $false)        ;; $read-handler
                       (local.get $read)          ;; $read-proc
                       (local.get $peek)          ;; $peek-proc
                       (local.get $close)         ;; $close-proc
                       (local.get $progress-evt)  ;; $get-progress-evt
                       (local.get $commit-proc)   ;; $commit-proc
                       (local.get $get-location)  ;; $get-location-proc
                       (local.get $count-lines!)  ;; $count-lines-proc
                       (local.get $init-position) ;; $init-position-arg
                       (local.get $buffer-mode))) ;; $buffer-mode-arg

     ;; make-output-port : any/c evt? (or/c output-port? procedure?) procedure? ... -> output-port?
     ;;   First useful custom-output-port slice: support byte/string writes
     ;;   via either an output-port delegate or a five-argument write
     ;;   procedure. Events and special writes are stored but not modeled
     ;;   by the current single-threaded runtime.
     (func $make-output-port (type $Prim>=4)
           (param $name  (ref eq)) ;; any/c
           (param $evt   (ref eq)) ;; evt?; accepted but not used yet
           (param $write (ref eq)) ;; output-port? or procedure?
           (param $close (ref eq)) ;; procedure?
           (param $rest  (ref eq)) ;; optional arguments
           (result       (ref eq))

           (local $args              (ref eq))
           (local $node              (ref $Pair))
           (local $arg               (ref eq))
           (local $count             i32)
           (local $write-special     (ref eq))
           (local $get-write-evt     (ref eq))
           (local $get-write-special (ref eq))
           (local $get-location      (ref eq))
           (local $count-lines!      (ref eq))
           (local $init-position     (ref eq))
           (local $buffer-mode       (ref eq))
           (local $loc               (ref $Location))
           (local $bytes             (ref $Bytes))

           (local.set $write-special     (global.get $false))
           (local.set $get-write-evt     (global.get $false))
           (local.set $get-write-special (global.get $false))
           (local.set $get-location      (global.get $false))
           (local.set $count-lines!      (global.get $void))
           (local.set $init-position     (ref.i31 (i32.shl (i32.const 1) (i32.const 1))))
           (local.set $buffer-mode       (global.get $false))

           (local.set $args  (local.get $rest))
           (local.set $count (i32.const 0))
           (block $done
                  (loop $loop
                        (if (ref.eq (local.get $args) (global.get $null))
                            (then (br $done)))
                        (local.set $node  (ref.cast (ref $Pair) (local.get $args)))
                        (local.set $arg   (struct.get $Pair $a (local.get $node)))
                        (local.set $args  (struct.get $Pair $d (local.get $node)))
                        (local.set $count (i32.add (local.get $count) (i32.const 1)))
                        (if (i32.gt_u (local.get $count) (i32.const 7))
                            (then (call $primitive-invoke:raise-arity-error
                                        (ref.cast (ref $PrimitiveProcedure)
                                                  (global.get $prim:make-output-port))
                                        (local.get $count))
                                  (unreachable)))
                        (if (i32.eq (local.get $count) (i32.const 1))
                            (then (local.set $write-special (local.get $arg)))
                            (else
                             (if (i32.eq (local.get $count) (i32.const 2))
                                 (then (local.set $get-write-evt (local.get $arg)))
                                 (else
                                  (if (i32.eq (local.get $count) (i32.const 3))
                                      (then (local.set $get-write-special (local.get $arg)))
                                      (else
                                       (if (i32.eq (local.get $count) (i32.const 4))
                                           (then (local.set $get-location (local.get $arg)))
                                           (else
                                            (if (i32.eq (local.get $count) (i32.const 5))
                                                (then (local.set $count-lines! (local.get $arg)))
                                                (else
                                                 (if (i32.eq (local.get $count) (i32.const 6))
                                                     (then (local.set $init-position (local.get $arg)))
                                                     (else (local.set $buffer-mode (local.get $arg))))))))))))))
                        (br $loop)))

           (if (ref.eq (call $evt? (local.get $evt))
                       (global.get $false))
               (then (call $raise-argument-error1
                           (global.get $symbol:make-output-port)
                           (global.get $string:evt?)
                           (local.get $evt))
                     (unreachable)))
           (if (i32.eqz (i32.or (ref.test (ref $OutputPort) (local.get $write))
                                (ref.eq (call $procedure? (local.get $write))
                                        (global.get $true))))
               (then (call $raise-argument-error1
                           (global.get $symbol:make-output-port)
                           (global.get $string:output-port-write-contract)
                           (local.get $write))
                     (unreachable)))
           (if (ref.eq (call $procedure? (local.get $write))
                       (global.get $true))
               (then (drop (call $check-procedure-arity/who
                                  (global.get $symbol:make-output-port)
                                  (global.get $string:output-port-write-contract)
                                  (local.get $write)
                                  (i32.const 5)))))
           (drop (call $check-procedure-arity/who
                       (global.get $symbol:make-output-port)
                       (global.get $string:port-procedure-arity-0)
                       (local.get $close)
                       (i32.const 0)))
           (if (i32.ge_u (local.get $count) (i32.const 1))
               (then (call $check-output-write-special/who
                           (global.get $symbol:make-output-port)
                           (local.get $write-special))))
           (if (i32.ge_u (local.get $count) (i32.const 2))
               (then (call $check-port-procedure-or-false/who
                           (global.get $symbol:make-output-port)
                           (global.get $string:port-procedure-arity-3-or-false)
                           (local.get $get-write-evt)
                           (i32.const 3))))
           (if (i32.ge_u (local.get $count) (i32.const 3))
               (then (call $check-port-procedure-or-false/who
                           (global.get $symbol:make-output-port)
                           (global.get $string:port-procedure-arity-1-or-false)
                           (local.get $get-write-special)
                           (i32.const 1))))
           (if (i32.ge_u (local.get $count) (i32.const 4))
               (then (call $check-port-procedure-or-false/who
                           (global.get $symbol:make-output-port)
                           (global.get $string:port-procedure-arity-0-or-false)
                           (local.get $get-location)
                           (i32.const 0))))
           (if (i32.ge_u (local.get $count) (i32.const 5))
               (then (drop (call $check-procedure-arity/who
                                  (global.get $symbol:make-output-port)
                                  (global.get $string:port-procedure-arity-0)
                                  (local.get $count-lines!)
                                  (i32.const 0)))))
           (if (i32.ge_u (local.get $count) (i32.const 6))
               (then (call $check-port-init-position/who
                           (global.get $symbol:make-output-port)
                           (local.get $init-position))))
           (if (i32.ge_u (local.get $count) (i32.const 7))
               (then (call $check-port-buffer-mode/who
                           (global.get $symbol:make-output-port)
                           (local.get $buffer-mode))))

           (local.set $bytes (ref.cast (ref $Bytes) (global.get $bytes:empty)))
           (local.set $loc   (call $make-port-initial-location (local.get $init-position)))
           (struct.new $CustomOutputPort
                       (i32.const 0)                  ;; $hash
                       (local.get $name)              ;; $name
                       (i32.const 0)                  ;; $closed
                       (local.get $bytes)             ;; $bytes placeholder
                       (i32.const 0)                  ;; $len placeholder
                       (i32.const 0)                  ;; $idx known byte position
                       (local.get $loc)               ;; $loc
                       (local.get $write)             ;; $write-proc
                       (local.get $close)             ;; $close-proc
                       (local.get $write-special)     ;; $write-special-proc
                       (local.get $get-write-evt)     ;; $get-write-evt-proc
                       (local.get $get-write-special) ;; $get-write-special-proc
                       (local.get $get-location)      ;; $get-location-proc
                       (local.get $count-lines!)      ;; $count-lines-proc
                       (local.get $init-position)     ;; $init-position-arg
                       (local.get $buffer-mode))))))  ;; $buffer-mode-arg

;; port-capability-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate port progress, atomic-write, and special-write capability predicates.
(define port-capability-runtime-only-helpers
  (function-forms->runtime-helpers
   'port-capability-runtime-only-helpers
   '(
     ;; NOTE: WebRacket currently lacks progress events, so report #f.
     (func $progress-evt? (type $Prim12)
           (param $v  (ref eq)) ;; any/c
           (param $in (ref eq)) ;; input-port? (optional, default = (current-input-port))
           (result    (ref eq))

           (global.get $false))

     ;; port-provides-progress-evts? : input-port? -> boolean?
     ;;   Racket's built-in ports normally support progress events, but
     ;;   WebRacket has not implemented port-progress-evt yet. Report #f
     ;;   so libraries take their no-progress-event fallback paths.
     (func $port-provides-progress-evts? (type $Prim1)
           (param $in (ref eq)) ;; input-port?
           (result    (ref eq))

           (if (i32.eqz (ref.test (ref $InputPort) (local.get $in)))
               (then (call $raise-argument-error1
                           (global.get $symbol:port-provides-progress-evts?)
                           (global.get $string:input-port?)
                           (local.get $in))
                     (unreachable)))
           (global.get $false))

     ;; port-writes-atomic? : output-port? -> boolean?
     ;;   WebRacket output writes are sequential in the current runtime,
     ;;   but the event-based atomic write protocol is not implemented.
     ;;   Report #f so libraries avoid atomic-write event paths.
     (func $port-writes-atomic? (type $Prim1)
           (param $out (ref eq)) ;; output-port?
           (result     (ref eq))

           (if (i32.eqz (call $output-port-like?/i32 (local.get $out)))
               (then (call $raise-argument-error1
                           (global.get $symbol:port-writes-atomic?)
                           (global.get $string:output-port?)
                           (local.get $out))
                     (unreachable)))
           (global.get $false))

     ;; port-writes-special? : output-port? -> boolean?
     ;;   Racket's ordinary output ports reject special values. WebRacket
     ;;   does not yet model custom output ports with write-special hooks.
     (func $port-writes-special? (type $Prim1)
           (param $out (ref eq)) ;; output-port?
           (result     (ref eq))

           (if (i32.eqz (call $output-port-like?/i32 (local.get $out)))
               (then (call $raise-argument-error1
                           (global.get $symbol:port-writes-special?)
                           (global.get $string:output-port?)
                           (local.get $out))
                     (unreachable)))
           (global.get $false)))))

;; port-custom-input-byte-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate custom input port byte read and peek helpers.
(define port-custom-input-byte-runtime-only-helpers
  (function-forms->runtime-helpers
   'port-custom-input-byte-runtime-only-helpers
   '(
     (func $read-byte/custom  ; read byte from custom input port
           (param $port (ref $CustomInputPort))
           (result      (ref eq))

           (local $delegate  (ref eq))
           (local $buffer    (ref $Bytes))
           (local $arr       (ref $I8Array))
           (local $buf-len   i32)
           (local $idx       i32)
           (local $len       i32)
           (local $proc      (ref $Procedure))
           (local $inv       (ref $ProcedureInvoker))
           (local $args      (ref $Args))
           (local $result    (ref eq))
           (local $count-raw i32)
           (local $count     i32)
           (local $byte      i32)

           (if (struct.get $CustomInputPort $closed (local.get $port))
               (then (call $raise-input-port-closed
                           (global.get $string:read-byte:input-port-closed))
                     (unreachable)))
           (local.set $delegate (struct.get $CustomInputPort $read-proc (local.get $port)))
           ;; Delegate to another input port when provided.
           (if (ref.test (ref $InputPort) (local.get $delegate))
               (then (return (call $read-byte (local.get $delegate)))))

           ;; Procedure-based handler: maintain scratch buffer and cached bytes.
           ;;   - the delegate is passed a mutable byte string to return the read bytes
           (local.set $buffer  (struct.get $CustomInputPort $bytes (local.get $port)))
           (local.set $arr     (struct.get $Bytes $bs              (local.get $buffer)))
           (local.set $buf-len (call $i8array-length               (local.get $arr)))
           (local.set $idx     (struct.get $CustomInputPort $idx   (local.get $port)))
           (local.set $len     (struct.get $CustomInputPort $len   (local.get $port)))

           (if (i32.ge_u (local.get $idx) (local.get $len))
               (then
                ;; Ensure scratch buffer has capacity.
                (if (i32.eqz (local.get $buf-len))
                    (then (local.set $buffer
                                     (ref.cast (ref $Bytes)
                                               (call $make-bytes
                                                     (ref.i31 (i32.shl (i32.const 4096)
                                                                       (i32.const 1)))
                                                     (global.get $missing))))
                          (struct.set $CustomInputPort $bytes (local.get $port) (local.get $buffer))
                          (local.set $arr    (struct.get $Bytes $bs (local.get $buffer)))
                          (local.set $buf-len (call $i8array-length (local.get $arr)))))

                ;; Require a procedure to produce fresh bytes.
                (if (i32.eqz (ref.test (ref $Procedure) (local.get $delegate)))
                    (then (return (global.get $false))))

                (local.set $proc (ref.cast (ref $Procedure) (local.get $delegate)))
                (local.set $inv  (struct.get $Procedure $invoke (local.get $proc)))
                (local.set $args (array.new_fixed $Args 1 (local.get $buffer)))
                (local.set $result
                           (call_ref $ProcedureInvoker
                                     (local.get $proc)
                                     (local.get $args)
                                     (local.get $inv)))

                ;; Interpret procedure result.
                (if (ref.eq (local.get $result) (global.get $eof))
                    (then (struct.set $CustomInputPort $len (local.get $port) (i32.const 0))
                          (struct.set $CustomInputPort $idx (local.get $port) (i32.const 0))
                          (return (global.get $eof))))

                (if (i32.eqz (ref.test (ref i31) (local.get $result)))
                    (then (struct.set $CustomInputPort $len (local.get $port) (i32.const 0))
                          (struct.set $CustomInputPort $idx (local.get $port) (i32.const 0))
                          (return (global.get $false))))

                (local.set $count-raw (i31.get_s (ref.cast (ref i31) (local.get $result))))
                (if (i32.and (local.get $count-raw) (i32.const 1))
                    (then (struct.set $CustomInputPort $len (local.get $port) (i32.const 0))
                          (struct.set $CustomInputPort $idx (local.get $port) (i32.const 0))
                          (return (global.get $false))))

                (local.set $count (i32.shr_s (local.get $count-raw) (i32.const 1)))
                (if (i32.lt_s (local.get $count) (i32.const 0))
                    (then (struct.set $CustomInputPort $len (local.get $port) (i32.const 0))
                          (struct.set $CustomInputPort $idx (local.get $port) (i32.const 0))
                          (return (global.get $false))))
                (if (i32.gt_u (local.get $count) (local.get $buf-len))
                    (then (struct.set $CustomInputPort $len (local.get $port) (i32.const 0))
                          (struct.set $CustomInputPort $idx (local.get $port) (i32.const 0))
                          (return (global.get $false))))
                (struct.set $CustomInputPort $len (local.get $port) (local.get $count))
                (struct.set $CustomInputPort $idx (local.get $port) (i32.const 0))
                (local.set $idx (i32.const 0))
                (local.set $len (local.get $count))
                (if (i32.eqz (local.get $count))
                    (then (return (global.get $false))))))

           ;; Deliver next buffered byte.
           (if (i32.ge_u (local.get $idx) (local.get $len))
               (then (return (global.get $false))))

           (local.set $buffer (struct.get $CustomInputPort $bytes (local.get $port)))
           (local.set $arr    (struct.get $Bytes $bs              (local.get $buffer)))
           (local.set $byte   (array.get_u $I8Array (local.get $arr) (local.get $idx)))
           (struct.set $CustomInputPort $idx
                       (local.get $port)
                       (i32.add (local.get $idx) (i32.const 1)))

           (ref.i31 (i32.shl (local.get $byte) (i32.const 1))))

     (func $peek-byte/custom  ;; peek byte from custom input port
           (param $port (ref $CustomInputPort))
           (param $skip (ref eq))        ;; exact-nonnegative-integer?, default = 0
           (result      (ref eq))

           (local $delegate  (ref eq))
           (local $skip-arg  (ref eq))
           (local $skip-count i32)
           (local $buffer    (ref $Bytes))
           (local $arr       (ref $I8Array))
           (local $buf-len   i32)
           (local $idx       i32)
           (local $len       i32)
           (local $proc      (ref $Procedure))
           (local $inv       (ref $ProcedureInvoker))
           (local $args      (ref $Args))
           (local $result    (ref eq))
           (local $count-raw i32)
           (local $count     i32)
           (local $byte      i32)

           (if (struct.get $CustomInputPort $closed (local.get $port))
               (then (call $raise-input-port-closed
                           (global.get $string:peek-byte:input-port-closed))
                     (unreachable)))
           ;; Normalize skip argument for procedure-based handlers.
           (local.set $skip-count (i32.const 0))
           (local.set $skip-arg (local.get $skip))
           (if (ref.eq (local.get $skip-arg) (global.get $missing))
               (then (local.set $skip-arg (ref.i31 (i32.const 0))))
               (else
                (local.set $skip-count
                           (call $exact-nonnegative-integer->i32/saturating
                                 (local.get $skip-arg)))
                (if (i32.lt_s (local.get $skip-count) (i32.const 0))
                    (then (return (global.get $false))))))

           ;; Look up delegate
           (local.set $delegate (struct.get $CustomInputPort $peek-proc (local.get $port)))

           ;; TODO: If delegate is #f, then peeking should be done via read-byte.
           (if (ref.eq (local.get $delegate) (global.get $false))
               (then (return (global.get $false))))

           ;; If delegate is an input port, just forward to peek-byte
           (if (ref.test (ref $InputPort) (local.get $delegate))
               (then (return (call $peek-byte
                                   (local.get $delegate)
                                   (local.get $skip)))))

           ;; Procedure-based handler. Only skip = 0 is currently supported.
           (if (i32.ne (local.get $skip-count) (i32.const 0))
               (then (return (global.get $false))))
           (if (i32.eqz (ref.test (ref $Procedure) (local.get $delegate)))
               (then (return (global.get $false))))

           (local.set $buffer (struct.get $CustomInputPort $bytes (local.get $port)))
           (local.set $arr    (struct.get $Bytes $bs (local.get $buffer)))
           (local.set $buf-len (call $i8array-length (local.get $arr)))
           (local.set $idx (struct.get $CustomInputPort $idx (local.get $port)))
           (local.set $len (struct.get $CustomInputPort $len (local.get $port)))

           ;; Use buffered data when available.
           (if (i32.lt_u (local.get $idx) (local.get $len))
               (then (local.set $byte (array.get_u $I8Array (local.get $arr) (local.get $idx)))
                     (return (ref.i31 (i32.shl (local.get $byte) (i32.const 1))))))

           ;; Ensure scratch buffer exists before invoking procedure.
           (if (i32.eqz (local.get $buf-len))
               (then (local.set $buffer
                                (ref.cast (ref $Bytes)
                                          (call $make-bytes
                                                (ref.i31 (i32.shl (i32.const 4096)
                                                                  (i32.const 1)))
                                                (global.get $missing))))
                     (struct.set $CustomInputPort $bytes (local.get $port) (local.get $buffer))
                     (local.set $arr    (struct.get $Bytes $bs (local.get $buffer)))
                     (local.set $buf-len (call $i8array-length (local.get $arr)))))

           (local.set $proc (ref.cast (ref $Procedure) (local.get $delegate)))
           (local.set $inv  (struct.get $Procedure $invoke (local.get $proc)))
           (local.set $args (array.new_fixed $Args 3
                                            (local.get $buffer)
                                            (local.get $skip-arg)
                                            (global.get $false)))
           (local.set $result
                      (call_ref $ProcedureInvoker
                                (local.get $proc)
                                (local.get $args)
                                (local.get $inv)))

           (if (ref.eq (local.get $result) (global.get $false))
               (then (struct.set $CustomInputPort $len (local.get $port) (i32.const 0))
                     (struct.set $CustomInputPort $idx (local.get $port) (i32.const 0))
                     (return (global.get $false))))
           (if (ref.eq (local.get $result) (global.get $eof))
               (then (struct.set $CustomInputPort $len (local.get $port) (i32.const 0))
                     (struct.set $CustomInputPort $idx (local.get $port) (i32.const 0))
                     (return (global.get $eof))))
           (if (i32.eqz (ref.test (ref i31) (local.get $result)))
               (then (struct.set $CustomInputPort $len (local.get $port) (i32.const 0))
                     (struct.set $CustomInputPort $idx (local.get $port) (i32.const 0))
                     (return (global.get $false))))

           (local.set $count-raw (i31.get_s (ref.cast (ref i31) (local.get $result))))
           (if (i32.and (local.get $count-raw) (i32.const 1))
               (then (struct.set $CustomInputPort $len (local.get $port) (i32.const 0))
                     (struct.set $CustomInputPort $idx (local.get $port) (i32.const 0))
                     (return (global.get $false))))

           (local.set $count (i32.shr_s (local.get $count-raw) (i32.const 1)))
           (if (i32.lt_s (local.get $count) (i32.const 0))
               (then (struct.set $CustomInputPort $len (local.get $port) (i32.const 0))
                     (struct.set $CustomInputPort $idx (local.get $port) (i32.const 0))
                     (return (global.get $false))))
           (if (i32.gt_u (local.get $count) (local.get $buf-len))
               (then (struct.set $CustomInputPort $len (local.get $port) (i32.const 0))
                     (struct.set $CustomInputPort $idx (local.get $port) (i32.const 0))
                     (return (global.get $false))))

           (if (i32.eqz (local.get $count))
               (then (return (global.get $false))))

           (local.set $buffer (struct.get $CustomInputPort $bytes (local.get $port)))
           (local.set $arr    (struct.get $Bytes $bs (local.get $buffer)))
           (local.set $byte   (array.get_u $I8Array (local.get $arr) (i32.const 0)))
           (ref.i31 (i32.shl (local.get $byte) (i32.const 1)))))))

;; port-byte-char-input-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate primitive byte and character input helpers.
(define port-byte-char-input-runtime-only-helpers
  (function-forms->runtime-helpers
   'port-byte-char-input-runtime-only-helpers
   (list
    `(func $read-byte (type $Prim01)
           (param $in (ref eq)) ;; optional input-port?, default = (current-input-port)
           (result    (ref eq))

           (local $sp        (ref null $InputStringPort))
           (local $bs        (ref eq))
           (local $arr       (ref $I8Array))
           (local $idx       i32)
           (local $limit     i32)
           (local $byte      i32)
           (local $loc       (ref $Location))

           (local $pos       (ref eq))
           (local $line      (ref eq))
           (local $col       (ref eq))

           (local $int-pos   i32)
           (local $int-line  i32)
           (local $int-col   i32)

           (local $len       i32)
           (local $left      i32)
           (local $seen      i32)

           (if (ref.eq (local.get $in) (global.get $missing))
               (then (local.set $in (call $current-input-port
                                          (global.get $missing)))))
           ;; Ensure the input is an input port.
           (if (i32.eqz (call $input-port-like?/i32 (local.get $in)))
               (then (call $raise-argument-error1
                           (global.get $symbol:read-byte)
                           (global.get $string:input-port?)
                           (local.get $in))
                     (unreachable)))
           ;; Dispatch supported input-port implementations.
           (if (ref.test (ref $InputStringPort) (local.get $in))
               (then (local.set $sp (ref.cast (ref $InputStringPort) (local.get $in))))
               (else
                (if (ref.test (ref $CustomInputPort) (local.get $in))
                    (then (return (call $read-byte/custom
                                         (ref.cast (ref $CustomInputPort) (local.get $in)))))
                    (else (return (global.get $false))))))
           (if (struct.get $InputStringPort $closed (local.get $sp))
               (then (call $raise-input-port-closed
                           (global.get $string:read-byte:input-port-closed))
                     (unreachable)))

           (local.set $idx   (struct.get $InputStringPort $idx (local.get $sp)))
           (local.set $limit (struct.get $InputStringPort $len (local.get $sp)))
           ;; Return the EOF object when no more bytes remain.
           (if (i32.ge_u (local.get $idx) (local.get $limit))
               (then (return (ref.i31 (i32.const ,eof-value)))))

           (local.set $bs  (struct.get $InputStringPort $bytes (local.get $sp)))
           (local.set $arr (struct.get $Bytes $bs (ref.cast (ref $Bytes) (local.get $bs))))
           (local.set $byte (array.get_u $I8Array (local.get $arr) (local.get $idx)))

           (struct.set $InputStringPort $idx (local.get $sp)
                       (i32.add (local.get $idx) (i32.const 1)))
           ;; Load the existing location information.
           (local.set $loc  (struct.get $InputStringPort $loc  (local.get $sp)))
           (local.set $pos  (struct.get $Location        $pos  (local.get $loc)))
           (local.set $line (struct.get $Location        $line (local.get $loc)))
           (local.set $col  (struct.get $Location        $col  (local.get $loc)))

           (local.set $int-pos  (if (result i32)
                                    (ref.test (ref i31) (local.get $pos))
                                    (then (i32.shr_s (i31.get_u (ref.cast (ref i31) (local.get $pos)))
                                                     (i32.const 1)))
                                    (else (i32.const 0))))
           (local.set $int-line (if (result i32)
                                    (ref.test (ref i31) (local.get $line))
                                    (then (i32.shr_s (i31.get_u (ref.cast (ref i31) (local.get $line)))
                                                     (i32.const 1)))
                                    (else (i32.const 0))))
           (local.set $int-col  (if (result i32)
                                    (ref.test (ref i31) (local.get $col))
                                    (then (i32.shr_s (i31.get_u (ref.cast (ref i31) (local.get $col)))
                                                     (i32.const 1)))
                                    (else (i32.const 0))))
           ;; Update UTF-8 state and column tracking using the read byte.
           (local.set $len  (struct.get $InputStringPort $utf8-len   (local.get $sp)))
           (local.set $left (struct.get $InputStringPort $utf8-left  (local.get $sp)))
           (local.set $seen (struct.get $InputStringPort $utf8-bytes (local.get $sp)))
           (if (i32.eqz (local.get $len))
               (then
                ;; Start of UTF-8 sequence
                (if (i32.lt_u (local.get $byte) (i32.const 128))
                    (then (local.set $int-col (i32.add (local.get $int-col) (i32.const 1))))
                    (else
                     (if (i32.and (i32.ge_u (local.get $byte) (i32.const 192))
                                  (i32.le_u (local.get $byte) (i32.const 253)))
                         (then
                          (block
                           (local.set $len
                                      (i32.clz (i32.xor (i32.shl (local.get $byte) (i32.const 24))
                                                        (i32.const 0xFF000000))))
                           (local.set $len
                                      (select
                                       (local.get $len)
                                       (i32.const 4)
                                       (i32.lt_u (local.get $len) (i32.const 4))))
                           (struct.set $InputStringPort $utf8-len   (local.get $sp) (local.get $len))
                           (struct.set $InputStringPort $utf8-left  (local.get $sp)
                                       (i32.sub (local.get $len) (i32.const 1)))
                           (struct.set $InputStringPort $utf8-bytes (local.get $sp) (i32.const 1))))))))
               (else
                (block
                 (local.set $seen (i32.add (local.get $seen) (i32.const 1)))
                 (local.set $left (i32.sub (local.get $left) (i32.const 1)))
                 (struct.set $InputStringPort $utf8-left  (local.get $sp) (local.get $left))
                 (struct.set $InputStringPort $utf8-bytes (local.get $sp) (local.get $seen))
                 (if (i32.eqz (local.get $left))
                     (then
                      ;; UTF-8 sequence complete: count one character column.
                      (local.set $int-col
                                 (i32.add (local.get $int-col) (i32.const 1)))
                      (struct.set $InputStringPort $utf8-len   (local.get $sp) (i32.const 0))
                      (struct.set $InputStringPort $utf8-left  (local.get $sp) (i32.const 0))
                      (struct.set $InputStringPort $utf8-bytes (local.get $sp) (i32.const 0)))))))
           ;; Handle newline, return, and tab characters.
           (if (i32.eq (local.get $byte) (i32.const 10))
               (then (local.set $int-line (i32.add (local.get $int-line) (i32.const 1)))
                     (local.set $int-col  (i32.const 0))))
           (if (i32.eq (local.get $byte) (i32.const 13))
               (then (local.set $int-line (i32.add (local.get $int-line) (i32.const 1)))
                     (local.set $int-col  (i32.const 0))))
           (if (i32.eq (local.get $byte) (i32.const 9))
               (then (local.set $int-col
                           (i32.add (local.get $int-col)
                                    (i32.sub
                                     (i32.const 8)
                                     (i32.rem_u (local.get $int-col) (i32.const 8)))))))
           ;; Always advance the absolute position.
           (local.set $int-pos (i32.add (local.get $int-pos) (i32.const 1)))
           ;; Store updated location back into the port.
           (struct.set $InputStringPort $loc (local.get $sp)
                       (struct.new $Location
                                   (i32.const 0)
                                   (ref.i31 (i32.shl (local.get $int-line) (i32.const 1)))
                                   (ref.i31 (i32.shl (local.get $int-col)  (i32.const 1)))
                                   (ref.i31 (i32.shl (local.get $int-pos)  (i32.const 1)))))
           ;; Return the read byte as a fixnum.
           (ref.i31 (i32.shl (local.get $byte) (i32.const 1))))

    `(func $read-char (type $Prim01)
           (param $in (ref eq)) ;; optional input-port?, default = (current-input-port)
           (result    (ref eq))

           (local $first        (ref eq))
           (local $next         (ref eq))
           (local $byte         i32)
           (local $need         i32)
           (local $initial-need i32)
           (local $acc          i32)
           (local $cp           i32)
           (local $cont         i32)

           (if (ref.eq (local.get $in) (global.get $missing))
               (then (local.set $in (call $current-input-port
                                          (global.get $missing)))))
           ;; Ensure the input is an input port.
           (if (i32.eqz (call $input-port-like?/i32 (local.get $in)))
               (then (call $raise-argument-error1
                           (global.get $symbol:read-char)
                           (global.get $string:input-port?)
                           (local.get $in))
                     (unreachable)))

           (local.set $first (call $read-byte (local.get $in)))
           (if (ref.eq (local.get $first) (global.get $eof))
               (then (return (global.get $eof))))

           (if (i32.eqz (ref.test (ref i31) (local.get $first)))
               (then (return (global.get $false))))

           (local.set $byte
                      (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $first)))
                                 (i32.const 1)))

           ;; ASCII fast path.
           (if (i32.lt_u (local.get $byte) (i32.const 128))
               (then (return (ref.i31 (i32.or (i32.shl (local.get $byte) (i32.const ,char-shift))
                                             (i32.const ,char-tag))))))

           ;; Determine continuation byte count and initial accumulator bits.
           (call $bytes->string/utf-8:determine-utf-8-sequence (local.get $byte))
           (local.set $acc)
           (local.set $need)
           (local.set $initial-need (local.get $need))

           ;; Reject invalid lead bytes.
           (if (i32.lt_s (local.get $need) (i32.const 0))
               (then (return (global.get $false))))

           (local.set $cp (local.get $acc))

           ;; Consume required continuation bytes.
           (block $done
                  (loop $loop
                        (br_if $done (i32.eqz (local.get $need)))
                        (local.set $next (call $read-byte (local.get $in)))
                        (if (ref.eq (local.get $next) (global.get $eof))
                            (then (return (global.get $eof))))
                        (if (i32.eqz (ref.test (ref i31) (local.get $next)))
                            (then (return (global.get $false))))
                        (local.set $cont
                                   (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $next)))
                                              (i32.const 1)))
                        ;; Continuation bytes must have the form #b10xxxxxx.
                        (if (i32.or (i32.lt_u (local.get $cont) (i32.const 128))
                                    (i32.ge_u (local.get $cont) (i32.const 192)))
                            (then (return (global.get $false))))
                        (local.set $cp
                                   (i32.or (i32.shl (local.get $cp) (i32.const 6))
                                           (i32.and (local.get $cont) (i32.const 63))))
                        (local.set $need (i32.sub (local.get $need) (i32.const 1)))
                        (br $loop)))

           ;; Validate resulting code point: reject surrogates, overlong encodings, and out-of-range values.
           (if (i32.gt_u (local.get $cp) (i32.const #x10FFFF))
               (then (return (global.get $false))))
           (if (i32.and (i32.ge_u (local.get $cp) (i32.const #xD800))
                        (i32.le_u (local.get $cp) (i32.const #xDFFF)))
               (then (return (global.get $false))))
           (if (i32.eq (local.get $initial-need) (i32.const 1))
               (then (if (i32.lt_u (local.get $cp) (i32.const #x80))
                         (then (return (global.get $false))))))
           (if (i32.eq (local.get $initial-need) (i32.const 2))
               (then (if (i32.lt_u (local.get $cp) (i32.const #x800))
                         (then (return (global.get $false))))))
           (if (i32.eq (local.get $initial-need) (i32.const 3))
               (then (if (i32.lt_u (local.get $cp) (i32.const #x10000))
                         (then (return (global.get $false))))))

           ;; Construct and return the character immediate.
           (ref.i31 (i32.or (i32.shl (local.get $cp) (i32.const ,char-shift))
                            (i32.const ,char-tag)))))))

;; port-read-special-wrapper-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate read-byte-or-special and read-char-or-special wrappers.
(define port-read-special-wrapper-runtime-only-helpers
  (function-forms->runtime-helpers
   'port-read-special-wrapper-runtime-only-helpers
   '(
     (func $check-read-special-wrap/who
           (param $who          (ref eq))
           (param $special-wrap (ref eq))

           (if (ref.eq (local.get $special-wrap) (global.get $missing))
               (then)
               (else
                (if (ref.eq (local.get $special-wrap) (global.get $false))
                    (then)
                    (else
                     (drop (call $check-procedure-arity/who
                                 (local.get $who)
                                 (global.get $string:read-special-wrap-contract)
                                 (local.get $special-wrap)
                                 (i32.const 1))))))))

     ;; read-byte-or-special : [input-port?] [special-wrap/c] [any/c] -> (or/c byte? eof-object? any/c)
     ;;   Bootstrap implementation: WebRacket ports do not yet produce special values.
     (func $read-byte-or-special (type $Prim03)
           (param $in           (ref eq)) ;; optional input-port?, default = (current-input-port)
           (param $special-wrap (ref eq)) ;; optional procedure-or-false?, validated but ignored until special values exist
           (param $source-name  (ref eq)) ;; optional any/c, passed to special producers in full Racket
           (result              (ref eq))

           (if (i32.and (i32.eqz (ref.eq (local.get $in) (global.get $missing)))
                        (i32.eqz (call $input-port-like?/i32 (local.get $in))))
               (then (call $raise-argument-error1
                           (global.get $symbol:read-byte-or-special)
                           (global.get $string:input-port?)
                           (local.get $in))
                     (unreachable)))
           (call $check-read-special-wrap/who
                 (global.get $symbol:read-byte-or-special)
                 (local.get $special-wrap))
           (call $read-byte (local.get $in)))

     ;; read-char-or-special : [input-port?] [special-wrap/c] [any/c] -> (or/c char? eof-object? any/c)
     ;;   Bootstrap implementation: WebRacket ports do not yet produce special values.
     (func $read-char-or-special (type $Prim03)
           (param $in           (ref eq)) ;; optional input-port?, default = (current-input-port)
           (param $special-wrap (ref eq)) ;; optional procedure-or-false?, validated but ignored until special values exist
           (param $source-name  (ref eq)) ;; optional any/c, passed to special producers in full Racket
           (result              (ref eq))

           (if (i32.and (i32.eqz (ref.eq (local.get $in) (global.get $missing)))
                        (i32.eqz (call $input-port-like?/i32 (local.get $in))))
               (then (call $raise-argument-error1
                           (global.get $symbol:read-char-or-special)
                           (global.get $string:input-port?)
                           (local.get $in))
                     (unreachable)))
           (call $check-read-special-wrap/who
                 (global.get $symbol:read-char-or-special)
                 (local.get $special-wrap))
           (call $read-char (local.get $in))))))

;; port-read-bytes-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate byte-vector input helpers.
(define port-read-bytes-runtime-only-helpers
  (function-forms->runtime-helpers
   'port-read-bytes-runtime-only-helpers
   '(
     (func $raise-port-bytes-index-expected
           (param $who (ref eq))
           (param $x   (ref eq))
           (call $raise-argument-error1
                 (local.get $who)
                 (global.get $string:exact-nonnegative-integer?)
                 (local.get $x))
           (unreachable))

     (func $raise-bad-port-bytes-range
           (param $who  (ref eq))
           (param $x    (ref eq))
           (param $from i32)
           (param $to   i32)

           (local $bs      (ref $Bytes))
           (local $len     i32)
           (local $out     (ref $GrowableArray))
           (local $message (ref $String))

           (local.set $bs  (ref.cast (ref $Bytes) (local.get $x)))
           (local.set $len (call $i8array-length
                                 (struct.get $Bytes $bs (local.get $bs))))

           (if (i32.gt_u (local.get $from) (local.get $len))
               (then
                (local.set $out
                           (call $make-growable-array (i32.const 9)))
                (call $growable-array-add! (local.get $out)
                      (call $format/display (local.get $who)))
                (call $growable-array-add! (local.get $out)
                      (global.get $string:substring:starting-index-out-of-range))
                (call $growable-array-add! (local.get $out)
                      (global.get $string:substring:starting-index))
                (call $growable-array-add! (local.get $out)
                      (call $format/display
                            (call $integer-from-i64
                                  (i64.extend_i32_u (local.get $from)))))
                (call $growable-array-add! (local.get $out)
                      (global.get $string:string-ref:valid-range))
                (call $growable-array-add! (local.get $out)
                      (call $format/display
                            (call $integer-from-i64
                                  (i64.extend_i32_u (local.get $len)))))
                (call $growable-array-add! (local.get $out)
                      (global.get $string:string-ref:valid-range-close))
                (call $growable-array-add! (local.get $out)
                      (global.get $string:string-utf8:byte-string))
                (call $growable-array-add! (local.get $out)
                      (call $format/error-value->string (local.get $x)))
                (local.set $message
                           (call $growable-array-of-strings->string (local.get $out)))
                (call $raise
                      (call $exn:fail:contract/make
                            (local.get $message)
                            (call $current-continuation-marks (global.get $missing)))
                      (global.get $true))
                (unreachable)))

           (if (i32.gt_u (local.get $to) (local.get $len))
               (then
                (local.set $out
                           (call $make-growable-array (i32.const 9)))
                (call $growable-array-add! (local.get $out)
                      (call $format/display (local.get $who)))
                (call $growable-array-add! (local.get $out)
                      (global.get $string:substring:ending-index-out-of-range))
                (call $growable-array-add! (local.get $out)
                      (global.get $string:substring:ending-index))
                (call $growable-array-add! (local.get $out)
                      (call $format/display
                            (call $integer-from-i64
                                  (i64.extend_i32_u (local.get $to)))))
                (call $growable-array-add! (local.get $out)
                      (global.get $string:string-ref:valid-range))
                (call $growable-array-add! (local.get $out)
                      (call $format/display
                            (call $integer-from-i64
                                  (i64.extend_i32_u (local.get $len)))))
                (call $growable-array-add! (local.get $out)
                      (global.get $string:string-ref:valid-range-close))
                (call $growable-array-add! (local.get $out)
                      (global.get $string:string-utf8:byte-string))
                (call $growable-array-add! (local.get $out)
                      (call $format/error-value->string (local.get $x)))
                (local.set $message
                           (call $growable-array-of-strings->string (local.get $out)))
                (call $raise
                      (call $exn:fail:contract/make
                            (local.get $message)
                            (call $current-continuation-marks (global.get $missing)))
                      (global.get $true))
                (unreachable)))

           (local.set $out
                      (call $make-growable-array (i32.const 11)))
           (call $growable-array-add! (local.get $out)
                 (call $format/display (local.get $who)))
           (call $growable-array-add! (local.get $out)
                 (global.get $string:substring:ending-index-before-start))
           (call $growable-array-add! (local.get $out)
                 (global.get $string:substring:ending-index))
           (call $growable-array-add! (local.get $out)
                 (call $format/display
                       (call $integer-from-i64
                             (i64.extend_i32_u (local.get $to)))))
           (call $growable-array-add! (local.get $out)
                 (global.get $string:substring:starting-index))
           (call $growable-array-add! (local.get $out)
                 (call $format/display
                       (call $integer-from-i64
                             (i64.extend_i32_u (local.get $from)))))
           (call $growable-array-add! (local.get $out)
                 (global.get $string:string-ref:valid-range))
           (call $growable-array-add! (local.get $out)
                 (call $format/display
                       (call $integer-from-i64
                             (i64.extend_i32_u (local.get $len)))))
           (call $growable-array-add! (local.get $out)
                 (global.get $string:string-ref:valid-range-close))
           (call $growable-array-add! (local.get $out)
                 (global.get $string:string-utf8:byte-string))
           (call $growable-array-add! (local.get $out)
                 (call $format/error-value->string (local.get $x)))
           (local.set $message
                      (call $growable-array-of-strings->string (local.get $out)))
           (call $raise
                 (call $exn:fail:contract/make
                       (local.get $message)
                       (call $current-continuation-marks (global.get $missing)))
                 (global.get $true))
           (unreachable))

     (func $read-bytes!/who
           (param $who   (ref eq))
           (param $bstr  (ref eq)) ;; bytes?
           (param $in    (ref eq)) ;; input-port?               (optional, default = (current-input-port))
           (param $start (ref eq)) ;; exact-nonnegative-integer? (optional, default = 0)
           (param $end   (ref eq)) ;; exact-nonnegative-integer? (optional, default = (bytes-length bstr))
           (result       (ref eq))

           (local $bs        (ref $Bytes))
           (local $arr       (ref $I8Array))
           (local $len       i32)
           (local $from      i32)
           (local $to        i32)
           (local $count     i32)
           (local $i         i32)
           (local $res       (ref eq))
           (local $byte      i32)
           (local $dest-idx  i32)

           ;; --- Validate byte string argument ---
           (if (i32.eqz (ref.test (ref $Bytes) (local.get $bstr)))
               (then (call $raise-argument-error1
                           (local.get $who)
                           (global.get $string:bytes?)
                           (local.get $bstr))
                     (unreachable)))
           (local.set $bs (ref.cast (ref $Bytes) (local.get $bstr)))
           (local.set $arr (struct.get $Bytes $bs (local.get $bs)))
           (local.set $len (call $i8array-length (local.get $arr)))
           ;; --- Determine input port ---
           (if (ref.eq (local.get $in) (global.get $missing))
               (then (local.set $in (call $current-input-port
                                          (global.get $missing)))))
           (if (i32.eqz (call $input-port-like?/i32 (local.get $in)))
               (then (call $raise-argument-error1
                           (local.get $who)
                           (global.get $string:input-port?)
                           (local.get $in))
                     (unreachable)))
           ;; --- Decode optional start index ---
           (if (ref.eq (local.get $start) (global.get $missing))
               (then (local.set $from (i32.const 0)))
               (else
                (local.set $from
                           (call $exact-nonnegative-integer->i32/saturating
                                 (local.get $start)))
                (if (i32.lt_s (local.get $from) (i32.const 0))
                    (then (call $raise-port-bytes-index-expected
                                (local.get $who)
                                (local.get $start))))))
           ;; --- Decode optional end index ---
           (if (ref.eq (local.get $end) (global.get $missing))
               (then (local.set $to (local.get $len)))
               (else
                (local.set $to
                           (call $exact-nonnegative-integer->i32/saturating
                                 (local.get $end)))
                (if (i32.lt_s (local.get $to) (i32.const 0))
                    (then (call $raise-port-bytes-index-expected
                                (local.get $who)
                                (local.get $end))))))
           ;; --- Bounds checks ---
           (if (i32.gt_u (local.get $from) (local.get $to))
               (then (call $raise-bad-port-bytes-range
                           (local.get $who)
                           (local.get $bstr)
                           (local.get $from)
                           (local.get $to))
                     (unreachable)))
           (if (i32.gt_u (local.get $to) (local.get $len))
               (then (call $raise-bad-port-bytes-range
                           (local.get $who)
                           (local.get $bstr)
                           (local.get $from)
                           (local.get $to))
                     (unreachable)))

           (local.set $count (i32.sub (local.get $to) (local.get $from)))
           (local.set $i (i32.const 0))
           ;; --- Read bytes into destination ---
           (block $done
                  (loop $loop
                        (br_if $done (i32.ge_u (local.get $i) (local.get $count)))
                        (local.set $res (call $read-byte (local.get $in)))
                        (if (ref.eq (local.get $res) (global.get $false))
                            (then (return (global.get $false))))
                        (if (ref.eq (local.get $res) (global.get $eof))
                            (then (if (i32.eqz (local.get $i))
                                      (then (return (global.get $eof))))
                                  (br $done)))
                        (if (i32.eqz (ref.test (ref i31) (local.get $res)))
                            (then (return (global.get $false))))
                        (local.set $byte (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $res)))
                                                    (i32.const 1)))
                        (local.set $dest-idx (i32.add (local.get $from) (local.get $i)))
                        (call $i8array-set! (local.get $arr) (local.get $dest-idx) (local.get $byte))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br $loop)))
           ;; --- Report number of bytes read ---
           (ref.i31 (i32.shl (local.get $i) (i32.const 1))))

     (func $read-bytes! (type $Prim14)
           (param $bstr  (ref eq)) ;; bytes?
           (param $in    (ref eq)) ;; input-port?               (optional, default = (current-input-port))
           (param $start (ref eq)) ;; exact-nonnegative-integer? (optional, default = 0)
           (param $end   (ref eq)) ;; exact-nonnegative-integer? (optional, default = (bytes-length bstr))
           (result       (ref eq))
           (call $read-bytes!/who
                 (global.get $symbol:read-bytes!)
                 (local.get $bstr)
                 (local.get $in)
                 (local.get $start)
                 (local.get $end)))

     ;; NOTE: Like Racket's read-bytes-avail!, but currently only string ports are supported
     ;;       and the runtime does not yet produce special results.
     (func $read-bytes-avail! (type $Prim14)
           (param $bstr  (ref eq)) ;; bytes?
           (param $in    (ref eq)) ;; input-port?                (optional, default = (current-input-port))
           (param $start (ref eq)) ;; exact-nonnegative-integer? (optional, default = 0)
           (param $end   (ref eq)) ;; exact-nonnegative-integer? (optional, default = (bytes-length bstr))
           (result       (ref eq))

           (call $read-bytes!/who
                 (global.get $symbol:read-bytes-avail!)
                 (local.get $bstr)
                 (local.get $in)
                 (local.get $start)
                 (local.get $end)))


     ;; NOTE: Like Racket's read-bytes-avail!*, but currently only string ports are supported
     ;;       and the runtime does not yet produce special results.
     (func $read-bytes-avail!* (type $Prim14)
           (param $bstr  (ref eq)) ;; bytes?
           (param $in    (ref eq)) ;; input-port?                (optional, default = (current-input-port))
           (param $start (ref eq)) ;; exact-nonnegative-integer? (optional, default = 0)
           (param $end   (ref eq)) ;; exact-nonnegative-integer? (optional, default = (bytes-length bstr))
           (result       (ref eq))

           (call $read-bytes!/who
                 (global.get $symbol:read-bytes-avail!*)
                 (local.get $bstr)
                 (local.get $in)
                 (local.get $start)
                 (local.get $end))))))

;; port-read-string-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate string input helpers.
(define port-read-string-runtime-only-helpers
  (function-forms->runtime-helpers
   'port-read-string-runtime-only-helpers
   (list
    '(func $raise-port-range-index-expected
           (param $who (ref eq))
           (param $x   (ref eq))
           (call $raise-argument-error1
                 (local.get $who)
                 (global.get $string:exact-nonnegative-integer?)
                 (local.get $x))
           (unreachable))
    `(func $read-string! (type $Prim14)
           (param $str   (ref eq)) ;; string?
           (param $in    (ref eq)) ;; input-port?                (optional, default = (current-input-port))
           (param $start (ref eq)) ;; exact-nonnegative-integer? (optional, default = 0)
           (param $end   (ref eq)) ;; exact-nonnegative-integer? (optional, default = (string-length str))
           (result       (ref eq))

           (local $s        (ref $String))
           (local $arr      (ref $I32Array))
           (local $len      i32)
           (local $from     i32)
           (local $to       i32)
           (local $count    i32)
           (local $i        i32)
           (local $res      (ref eq))
           (local $tagged   i32)
           (local $cp       i32)
           (local $dest-idx i32)

           ;; --- Validate string argument ---
           (if (i32.eqz (ref.test (ref $String) (local.get $str)))
               (then (call $raise-argument-error1
                           (global.get $symbol:read-string!)
                           (global.get $string:string?)
                           (local.get $str))
                     (unreachable)))
           (local.set $s (ref.cast (ref $String) (local.get $str)))
           (local.set $arr (struct.get $String $codepoints (local.get $s)))
           (local.set $len (call $i32array-length (local.get $arr)))
           ;; --- Determine input port ---
           (if (ref.eq (local.get $in) (global.get $missing))
               (then (local.set $in (call $current-input-port
                                          (global.get $missing)))))
           (if (i32.eqz (call $input-port-like?/i32 (local.get $in)))
               (then (call $raise-argument-error1
                           (global.get $symbol:read-string!)
                           (global.get $string:input-port?)
                           (local.get $in))
                     (unreachable)))
           ;; --- Decode optional start index ---
           (if (ref.eq (local.get $start) (global.get $missing))
               (then (local.set $from (i32.const 0)))
               (else
                (local.set $from
                           (call $exact-nonnegative-integer->i32/saturating
                                 (local.get $start)))
                (if (i32.lt_s (local.get $from) (i32.const 0))
                    (then (call $raise-port-range-index-expected
                                (global.get $symbol:read-string!)
                                (local.get $start))))))
           ;; --- Decode optional end index ---
           (if (ref.eq (local.get $end) (global.get $missing))
               (then (local.set $to (local.get $len)))
               (else
                (local.set $to
                           (call $exact-nonnegative-integer->i32/saturating
                                 (local.get $end)))
                (if (i32.lt_s (local.get $to) (i32.const 0))
                    (then (call $raise-port-range-index-expected
                                (global.get $symbol:read-string!)
                                (local.get $end))))))
           ;; --- Bounds checks ---
           (if (i32.gt_u (local.get $from) (local.get $to))
               (then (call $raise-bad-string-range
                           (global.get $symbol:read-string!)
                           (local.get $str)
                           (local.get $from)
                           (local.get $to))
                     (unreachable)))
           (if (i32.gt_u (local.get $to) (local.get $len))
               (then (call $raise-bad-string-range
                           (global.get $symbol:read-string!)
                           (local.get $str)
                           (local.get $from)
                           (local.get $to))
                     (unreachable)))

           (local.set $count (i32.sub (local.get $to) (local.get $from)))
           (local.set $i (i32.const 0))
           ;; --- Read characters into destination ---
           (block $done
                  (loop $loop
                        (br_if $done (i32.ge_u (local.get $i) (local.get $count)))
                        (local.set $res (call $read-char (local.get $in)))
                        (if (ref.eq (local.get $res) (global.get $false))
                            (then (return (global.get $false))))
                        (if (ref.eq (local.get $res) (global.get $eof))
                            (then (if (i32.eqz (local.get $i))
                                      (then (return (global.get $eof))))
                                  (br $done)))
                        (if (i32.eqz (ref.test (ref i31) (local.get $res)))
                            (then (return (global.get $false))))
                        (local.set $tagged (i31.get_u (ref.cast (ref i31) (local.get $res))))
                        (if (i32.ne (i32.and (local.get $tagged) (i32.const ,char-mask)) (i32.const ,char-tag))
                            (then (return (global.get $false))))
                        (local.set $cp (i32.shr_u (local.get $tagged) (i32.const ,char-shift)))
                        (local.set $dest-idx (i32.add (local.get $from) (local.get $i)))
                        (call $i32array-set! (local.get $arr) (local.get $dest-idx) (local.get $cp))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br $loop)))
           ;; --- Report number of characters read ---
           (ref.i31 (i32.shl (local.get $i) (i32.const 1))))

    ;; Like Racket's read-bytes, but currently only string ports are supported
    '(func $read-bytes (type $Prim12)
           (param $amt (ref eq)) ;; exact-nonnegative-integer?
           (param $in  (ref eq)) ;; input-port? (optional, default = (current-input-port))
           (result     (ref eq))

           (local $count     i32)
           (local $buf       (ref $Bytes))
           (local $res       (ref eq))
           (local $read      i32)
           (local $arr       (ref $I8Array))
           (local $new-arr   (ref $I8Array))

           ;; --- Decode amount ---
           (local.set $count
                      (call $bytes-length/i32/who
                            (global.get $symbol:read-bytes)
                            (local.get $amt)))

           ;; --- Handle zero-length read ---
           (if (i32.eqz (local.get $count))
               (then (return (global.get $bytes:empty))))

           ;; --- Determine input port ---
           (if (ref.eq (local.get $in) (global.get $missing))
               (then (local.set $in (call $current-input-port
                                          (global.get $missing)))))
           (if (i32.eqz (ref.test (ref $InputPort) (local.get $in)))
               (then (call $raise-argument-error1
                           (global.get $symbol:read-bytes)
                           (global.get $string:input-port?)
                           (local.get $in))))

           ;; --- Allocate destination buffer ---
           (local.set $buf
                      (ref.cast (ref $Bytes)
                                (call $make-bytes
                                      (ref.i31 (i32.shl (local.get $count) (i32.const 1)))
                                      (global.get $missing))))

           ;; --- Fill buffer using read-bytes! ---
           (local.set $res
                      (call $read-bytes!
                            (local.get $buf)
                            (local.get $in)
                            (global.get $missing)
                            (global.get $missing)))

           ;; Propagate failure conditions
           (if (ref.eq (local.get $res) (global.get $false))
               (then (return (global.get $false))))
           (if (ref.eq (local.get $res) (global.get $eof))
               (then (return (global.get $eof))))

           (if (i32.eqz (ref.test (ref i31) (local.get $res)))
               (then (return (global.get $false))))
           (local.set $read (i31.get_u (ref.cast (ref i31) (local.get $res))))
           (if (i32.ne (i32.and (local.get $read) (i32.const 1)) (i32.const 0))
               (then (return (global.get $false))))
           (local.set $read (i32.shr_u (local.get $read) (i32.const 1)))

           ;; Shrink buffer on partial read
           (if (i32.lt_u (local.get $read) (local.get $count))
               (then (local.set $arr (struct.get $Bytes $bs (local.get $buf)))
                     (local.set $new-arr
                                (call $i8array-copy
                                      (local.get $arr)
                                      (i32.const 0)
                                      (local.get $read)))
                     (struct.set $Bytes $bs (local.get $buf) (local.get $new-arr))))

           ;; Freeze buffer to make result immutable
           #;(struct.set $Bytes $immutable (local.get $buf) (i32.const 1))

           (local.get $buf))

    ;; Like Racket's read-string, but currently only string ports are supported
    '(func $read-string (type $Prim12)
           (param $amt (ref eq)) ;; exact-nonnegative-integer?
           (param $in  (ref eq)) ;; input-port?               (optional, default = (current-input-port))
           (result     (ref eq))

           (local $count     i32)
           (local $buf       (ref $String))
           (local $res       (ref eq))
           (local $read      i32)
           (local $arr       (ref $I32Array))
           (local $new-arr   (ref $I32Array))

           ;; --- Decode amount ---
           (local.set $count
                      (call $string-length/i32/who
                            (global.get $symbol:read-string)
                            (local.get $amt)
                            (global.get $string:exact-nonnegative-integer?)))

           ;; --- Handle zero-length read ---
           (if (i32.eqz (local.get $count))
               (then (return (global.get $string:empty))))

           ;; --- Determine input port ---
           (if (ref.eq (local.get $in) (global.get $missing))
               (then (local.set $in (call $current-input-port
                                          (global.get $missing)))))
           (if (i32.eqz (ref.test (ref $InputPort) (local.get $in)))
               (then (call $raise-argument-error1
                           (global.get $symbol:read-string)
                           (global.get $string:input-port?)
                           (local.get $in))))

           ;; --- Allocate destination string ---
           (local.set $buf (call $make-string/checked (local.get $count) (i32.const 0)))

           ;; --- Fill buffer using read-string! ---
           (local.set $res
                      (call $read-string!
                            (local.get $buf)
                            (local.get $in)
                            (global.get $missing)
                            (global.get $missing)))

           ;; Propagate failure conditions
           (if (ref.eq (local.get $res) (global.get $false))
               (then (return (global.get $false))))
           (if (ref.eq (local.get $res) (global.get $eof))
               (then (return (global.get $eof))))

           (if (i32.eqz (ref.test (ref i31) (local.get $res)))
               (then (return (global.get $false))))
           (local.set $read (i31.get_u (ref.cast (ref i31) (local.get $res))))
           (if (i32.ne (i32.and (local.get $read) (i32.const 1)) (i32.const 0))
               (then (return (global.get $false))))
           (local.set $read (i32.shr_u (local.get $read) (i32.const 1)))

           (local.set $arr (struct.get $String $codepoints (local.get $buf)))

           ;; Shrink buffer on partial read
           (if (i32.lt_u (local.get $read) (local.get $count))
               (then (local.set $new-arr
                                (call $i32array-copy
                                      (local.get $arr)
                                      (i32.const 0)
                                      (local.get $read)))
                     (struct.set $String $codepoints (local.get $buf) (local.get $new-arr))))

           (struct.set $String $hash (local.get $buf) (i32.const 0))

           (local.get $buf)))))

;; port-readiness-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate input readiness predicates.
(define port-readiness-runtime-only-helpers
  (function-forms->runtime-helpers
   'port-readiness-runtime-only-helpers
   '(
     (func $byte-ready? (type $Prim01)
           (param $in (ref eq)) ;; input-port? (optional, default = (current-input-port))
           (result    (ref eq))

           (local $sp  (ref null $InputStringPort))
           (local $idx i32)
           (local $len i32)

           (if (ref.eq (local.get $in) (global.get $missing))
               (then (local.set $in (call $current-input-port
                                          (global.get $missing)))))
           ;; Ensure the input is a string port.
           (if (ref.test (ref $InputStringPort) (local.get $in))
               (then (local.set $sp (ref.cast (ref $InputStringPort) (local.get $in))))
               (else (return (global.get $false))))

           (local.set $idx (struct.get $InputStringPort $idx (local.get $sp)))
           (local.set $len (struct.get $InputStringPort $len (local.get $sp)))

           (if (result (ref eq))
               (i32.lt_u (local.get $idx) (local.get $len))
               (then (global.get $true))
               (else (global.get $false))))

     (func $char-ready? (type $Prim01)
           (param $in (ref eq)) ;; input-port? (optional, default = (current-input-port))
           (result    (ref eq))

           (local $sp        (ref null $InputStringPort))
           (local $bs        (ref eq))
           (local $arr       (ref $I8Array))
           (local $idx       i32)
           (local $len       i32)
           (local $byte      i32)
           (local $need      i32)
           (local $acc       i32)
           (local $initial   i32)
           (local $available i32)
           (local $cp        i32)
           (local $i         i32)
           (local $scan      i32)
           (local $cont      i32)

           (if (ref.eq (local.get $in) (global.get $missing))
               (then (local.set $in (call $current-input-port
                                          (global.get $missing)))))
           ;; Ensure the input is a string port.
           (if (ref.test (ref $InputStringPort) (local.get $in))
               (then (local.set $sp (ref.cast (ref $InputStringPort) (local.get $in))))
               (else (return (global.get $false))))

           (local.set $idx (struct.get $InputStringPort $idx (local.get $sp)))
           (local.set $len (struct.get $InputStringPort $len (local.get $sp)))

           ;; No characters remain when at EOF.
           (if (i32.ge_u (local.get $idx) (local.get $len))
               (then (return (global.get $false))))

           (local.set $bs   (struct.get $InputStringPort $bytes (local.get $sp)))
           (local.set $arr  (struct.get $Bytes $bs (ref.cast (ref $Bytes) (local.get $bs))))
           (local.set $byte (array.get_u $I8Array (local.get $arr) (local.get $idx)))

           ;; ASCII characters are always ready when a byte is available.
           (if (i32.lt_u (local.get $byte) (i32.const 128))
               (then (return (global.get $true))))

           ;; Determine continuation requirements for the UTF-8 sequence.
           (call $bytes->string/utf-8:determine-utf-8-sequence (local.get $byte))
           (local.set $acc)
           (local.set $need)
           (local.set $initial (local.get $need))

           ;; Reject invalid lead bytes.
           (if (i32.lt_s (local.get $need) (i32.const 0))
               (then (return (global.get $false))))

           ;; Ensure enough bytes remain to finish the character.
           (local.set $available (i32.sub (local.get $len) (local.get $idx)))
           (if (i32.lt_u (local.get $available)
                         (i32.add (local.get $need) (i32.const 1)))
               (then (return (global.get $false))))

           (local.set $cp (local.get $acc))
           (local.set $i (i32.const 0))
           (local.set $scan (local.get $idx))

           ;; Validate continuation bytes and build the code point.
           (block $done
                  (loop $loop
                        (br_if $done (i32.ge_u (local.get $i) (local.get $need)))
                        (local.set $scan (i32.add (local.get $scan) (i32.const 1)))
                        (local.set $cont (array.get_u $I8Array (local.get $arr)
                                                          (local.get $scan)))
                        (if (i32.or (i32.lt_u (local.get $cont) (i32.const 128))
                                    (i32.ge_u (local.get $cont) (i32.const 192)))
                            (then (return (global.get $false))))
                        (local.set $cp
                                   (i32.or (i32.shl (local.get $cp) (i32.const 6))
                                           (i32.and (local.get $cont) (i32.const 63))))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br $loop)))

           ;; Reject invalid Unicode scalars and overlong encodings.
           (if (i32.gt_u (local.get $cp) (i32.const #x10FFFF))
               (then (return (global.get $false))))
           (if (i32.and (i32.ge_u (local.get $cp) (i32.const #xD800))
                        (i32.le_u (local.get $cp) (i32.const #xDFFF)))
               (then (return (global.get $false))))
           (if (i32.eq (local.get $initial) (i32.const 1))
               (then (if (i32.lt_u (local.get $cp) (i32.const #x80))
                         (then (return (global.get $false))))))
           (if (i32.eq (local.get $initial) (i32.const 2))
               (then (if (i32.lt_u (local.get $cp) (i32.const #x800))
                         (then (return (global.get $false))))))
           (if (i32.eq (local.get $initial) (i32.const 3))
               (then (if (i32.lt_u (local.get $cp) (i32.const #x10000))
                         (then (return (global.get $false))))))

           (global.get $true)))))

;; port-read-line-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate line-reading helpers.
(define port-read-line-runtime-only-helpers
  (function-forms->runtime-helpers
   'port-read-line-runtime-only-helpers
   (list
    '(func $raise-read-line:bad-mode
           (param $who  (ref eq))
           (param $mode (ref eq))
           (call $raise-argument-error1
                 (local.get $who)
                 (global.get $string:read-line-mode-contract)
                 (local.get $mode))
           (unreachable))

    '(func $read-line-mode-code
           (param $who      (ref eq))
           (param $mode-val (ref eq))
           (result          i32)

           (block
            (if (ref.eq (local.get $mode-val) (global.get $symbol:linefeed))
                (then (return (i32.const 0))))
            (if (ref.eq (local.get $mode-val) (global.get $symbol:return))
                (then (return (i32.const 1))))
            (if (ref.eq (local.get $mode-val) (global.get $symbol:return-linefeed))
                (then (return (i32.const 2))))
            (if (ref.eq (local.get $mode-val) (global.get $symbol:any))
                (then (return (i32.const 3))))
            (if (ref.eq (local.get $mode-val) (global.get $symbol:any-one))
                (then (return (i32.const 4)))))
           (call $raise-read-line:bad-mode (local.get $who) (local.get $mode-val))
           (unreachable))

    `(func $read-line/custom
           (param $port        (ref $CustomInputPort))
           (param $mode-code   i32)
           (result             (ref eq))

           (local $buf         (ref $I32GrowableArray))
           (local $count       i32)
           (local $res         (ref eq))
           (local $tagged      i32)
           (local $cp          i32)
           (local $append      i32)
           (local $stop        i32)
           (local $consume     i32)
           (local $skip        (ref eq))
           (local $peek        (ref eq))
           (local $peek-tagged i32)
           (local $peek-byte   i32)

           (local.set $buf   (call $make-i32growable-array (i32.const 32)))
           (local.set $count (i32.const 0))

           (block $done
                  (loop $loop
                        (local.set $res (call $read-char (local.get $port)))
                        (if (ref.eq (local.get $res) (global.get $false))
                            (then (return (global.get $false))))
                        (if (ref.eq (local.get $res) (global.get $eof))
                            (then (if (i32.eqz (local.get $count))
                                      (then (return (global.get $eof)))
                                      (else (br $done)))))
                        (if (i32.eqz (ref.test (ref i31) (local.get $res)))
                            (then (return (global.get $false))))
                        (local.set $tagged (i31.get_u (ref.cast (ref i31) (local.get $res))))
                        (if (i32.ne (i32.and (local.get $tagged) (i32.const ,char-mask))
                                    (i32.const ,char-tag))
                            (then (return (global.get $false))))

                        (local.set $cp      (i32.shr_u (local.get $tagged) (i32.const ,char-shift)))
                        (local.set $append  (i32.const 1))
                        (local.set $stop    (i32.const 0))
                        (local.set $consume (i32.const 0))

                        (if (i32.eq (local.get $mode-code) (i32.const 0))
                            (then (if (i32.eq (local.get $cp) (i32.const 10))
                                      (then (local.set $append (i32.const 0))
                                            (local.set $stop   (i32.const 1))))))

                        (if (i32.eq (local.get $mode-code) (i32.const 1))
                            (then (if (i32.eq (local.get $cp) (i32.const 13))
                                      (then (local.set $append (i32.const 0))
                                            (local.set $stop   (i32.const 1))))))

                        (if (i32.eq (local.get $mode-code) (i32.const 3))
                            (then (if (i32.eq (local.get $cp) (i32.const 10))
                                      (then (local.set $append (i32.const 0))
                                            (local.set $stop   (i32.const 1))))))

                        (if (i32.eq (local.get $mode-code) (i32.const 4))
                            (then (if (i32.or (i32.eq (local.get $cp) (i32.const 10))
                                              (i32.eq (local.get $cp) (i32.const 13)))
                                      (then (local.set $append (i32.const 0))
                                            (local.set $stop   (i32.const 1))))))

                        (if (i32.or (i32.eq (local.get $mode-code) (i32.const 2))
                                    (i32.eq (local.get $mode-code) (i32.const 3)))
                            (then (if (i32.eq (local.get $cp) (i32.const 13))
                                      (then (local.set $append (i32.const 0))
                                            (local.set $stop   (i32.const 1))
                                            (local.set $peek (call $peek-byte (local.get $port)
                                                                    (global.get $missing)))
                                            (if (ref.eq (local.get $peek) (global.get $false))
                                                (then (return (global.get $false))))
                                            (if (ref.eq (local.get $peek) (global.get $eof))
                                                (then)
                                                (else (if (i32.eqz (ref.test (ref i31) (local.get $peek)))
                                                          (then (return (global.get $false))))
                                                      (local.set $peek-tagged
                                                                 (i31.get_u (ref.cast (ref i31) (local.get $peek))))
                                                      (if (i32.ne (i32.and (local.get $peek-tagged) (i32.const 1))
                                                                 (i32.const 0))
                                                          (then (return (global.get $false))))
                                                      (local.set $peek-byte
                                                                 (i32.shr_u (local.get $peek-tagged)
                                                                            (i32.const 1)))
                                                      (if (i32.eq (local.get $peek-byte) (i32.const 10))
                                                          (then (local.set $consume (i32.const 1))))))))))

                        (if (i32.eq (local.get $append) (i32.const 1))
                            (then (call $i32growable-array-add! (local.get $buf) (local.get $cp))
                                  (local.set $count (i32.add (local.get $count) (i32.const 1)))))

                        (if (i32.eq (local.get $stop) (i32.const 1))
                            (then (if (i32.eq (local.get $consume) (i32.const 1))
                                      (then (local.set $skip (call $read-char (local.get $port)))
                                            (if (ref.eq (local.get $skip) (global.get $false))
                                                (then (return (global.get $false))))
                                            (if (ref.eq (local.get $skip) (global.get $eof))
                                                (then (return (global.get $false))))))
                                  (br $done)))

                        (br $loop)))

           (local.set $res (call $i32growable-array->immutable-string (local.get $buf)))
           (local.get $res))

    `(func $read-line (type $Prim02)
           (param $in   (ref eq)) ;; optional input-port?, default = (current-input-port)
           (param $mode (ref eq)) ;; optional read-line-mode?, default = 'linefeed
           (result      (ref eq))

           (local $sp        (ref null $InputStringPort))
           (local $bytes     (ref null $Bytes))
           (local $arr       (ref null $I8Array))
           (local $mode-val  (ref eq))
           (local $mode-code i32)
           (local $buf       (ref $I32GrowableArray))
           (local $res       (ref eq))
           (local $skip      (ref eq))
           (local $tagged    i32)
           (local $cp        i32)
           (local $count     i32)
           (local $append    i32)
           (local $break     i32)
           (local $consume   i32)
           (local $idx       i32)
           (local $len       i32)
           (local $next-byte i32)

           (if (ref.eq (local.get $in) (global.get $missing))
               (then (local.set $in (call $current-input-port
                                          (global.get $missing)))))

           ;; Determine the mode symbol, defaulting to 'linefeed.
           (local.set $mode-val
                      (if (result (ref eq))
                          (ref.eq (local.get $mode) (global.get $missing))
                          (then (global.get $symbol:linefeed))
                          (else (local.get $mode))))
           ;; Decode the mode into an internal tag.
           (local.set $mode-code
                      (call $read-line-mode-code
                            (global.get $symbol:read-line)
                            (local.get $mode-val)))

           ;; Dispatch based on the input port type.
           (if (ref.test (ref $InputStringPort) (local.get $in))
               (then (local.set $sp (ref.cast (ref $InputStringPort) (local.get $in)))
                     (local.set $bytes
                                (ref.cast (ref $Bytes)
                                          (struct.get $InputStringPort $bytes (local.get $sp))))
                     (local.set $arr (struct.get $Bytes $bs (local.get $bytes))))
               (else (if (ref.test (ref $CustomInputPort) (local.get $in))
                         (then (return (call $read-line/custom
                                             (ref.cast (ref $CustomInputPort) (local.get $in))
                                             (local.get $mode-code))))
                         (else (if (i32.eqz (call $input-port-like?/i32 (local.get $in)))
                                   (then (call $raise-argument-error1
                                               (global.get $symbol:read-line)
                                               (global.get $string:input-port?)
                                               (local.get $in))
                                         (unreachable))
                                   (else (call $raise-check-string-port
                                               (global.get $symbol:read-line)
                                               (local.get $in))
                                         (unreachable)))))))

           (local.set $buf   (call $make-i32growable-array (i32.const 32)))
           (local.set $count (i32.const 0))

           (block $done
                  (loop $loop
                        (local.set $res (call $read-char (local.get $in)))
                        (if (ref.eq (local.get $res) (global.get $false))
                            (then (return (global.get $false))))
                        (if (ref.eq (local.get $res) (global.get $eof))
                            (then (if (i32.eqz (local.get $count))
                                      (then (return (global.get $eof)))
                                      (else (br $done)))))
                        (if (i32.eqz (ref.test (ref i31) (local.get $res)))
                            (then (return (global.get $false))))
                        (local.set $tagged (i31.get_u (ref.cast (ref i31) (local.get $res))))
                        (if (i32.ne (i32.and (local.get $tagged) (i32.const ,char-mask))
                                    (i32.const ,char-tag))
                            (then (return (global.get $false))))
                        (local.set $cp (i32.shr_u (local.get $tagged) (i32.const ,char-shift)))

                        (local.set $append (i32.const 1))
                        (local.set $break  (i32.const 0))
                        (local.set $consume (i32.const 0))

                        (if (i32.eq (local.get $mode-code) (i32.const 0))
                            (then (if (i32.eq (local.get $cp) (i32.const 10))
                                      (then (local.set $append (i32.const 0)))
                                      (else (local.set $break (i32.const 1))))))

                        (if (i32.eq (local.get $mode-code) (i32.const 1))
                            (then (if (i32.eq (local.get $cp) (i32.const 13))
                                      (then (local.set $append (i32.const 0)))
                                      (else (local.set $break (i32.const 1))))))

                        (if (i32.eq (local.get $mode-code) (i32.const 2))
                            (then (if (i32.eq (local.get $cp) (i32.const 13))
                                      (then (local.set $idx (struct.get $InputStringPort $idx (local.get $sp)))
                                            (local.set $len (struct.get $InputStringPort $len (local.get $sp)))
                                            (if (i32.lt_u (local.get $idx) (local.get $len))
                                                (then (local.set $next-byte (array.get_u $I8Array (local.get $arr) (local.get $idx)))
                                                      (if (i32.eq (local.get $next-byte) (i32.const 10))
                                                          (then (local.set $append (i32.const 0))
                                                                (local.set $break (i32.const 1))
                                                                (local.set $consume (i32.const 1))))))))))

                        (if (i32.eq (local.get $mode-code) (i32.const 3))
                            (then (if (i32.eq (local.get $cp) (i32.const 10))
                                      (then (local.set $append (i32.const 0))
                                            (local.set $break (i32.const 1)))
                                      (else (if (i32.eq (local.get $cp) (i32.const 13))
                                                (then (local.set $append (i32.const 0))
                                                      (local.set $break (i32.const 1))
                                                      (local.set $idx (struct.get $InputStringPort $idx (local.get $sp)))
                                                      (local.set $len (struct.get $InputStringPort $len (local.get $sp)))
                                                      (if (i32.lt_u (local.get $idx) (local.get $len))
                                                          (then (local.set $next-byte (array.get_u $I8Array (local.get $arr) (local.get $idx)))
                                                                (if (i32.eq (local.get $next-byte) (i32.const 10))
                                                                    (then (local.set $consume (i32.const 1))))))))))))

                        (if (i32.eq (local.get $mode-code) (i32.const 4))
                            (then (if (i32.or (i32.eq (local.get $cp) (i32.const 10))
                                              (i32.eq (local.get $cp) (i32.const 13)))
                                      (then (local.set $append (i32.const 0)))
                                      (else (local.set $break (i32.const 1))))))

                        (if (i32.eq (local.get $append) (i32.const 1))
                            (then (call $i32growable-array-add! (local.get $buf) (local.get $cp))
                                  (local.set $count (i32.add (local.get $count) (i32.const 1)))))

                        (if (i32.eq (local.get $break) (i32.const 1))
                            (then (if (i32.eq (local.get $consume) (i32.const 1))
                                      (then (local.set $skip (call $read-char (local.get $in)))
                                            (if (ref.eq (local.get $skip) (global.get $false))
                                                (then (return (global.get $false))))
                                            (if (ref.eq (local.get $skip) (global.get $eof))
                                                (then (return (global.get $false)))))))
                            (else (br $done)))

                        (br $loop)))

           (local.set $res (call $i32growable-array->immutable-string (local.get $buf)))
           (local.get $res))

    ;; read-bytes-line : [input-port?] [read-line-mode?] -> (or/c bytes? eof-object?)
    ;;   Read a line as a string, then encode it as UTF-8 bytes.
    ;;   Optional in defaults to current-input-port; mode defaults to 'linefeed.
    '(func $read-bytes-line (type $Prim02)
           (param $in   (ref eq)) ;; optional input-port?, default = (current-input-port)
           (param $mode (ref eq)) ;; optional read-line-mode?, default = 'linefeed
           (result      (ref eq))

           (local $line      (ref eq))
           (local $mode-val  (ref eq))
           (local $mode-code i32)

           (local.set $mode-val
                      (if (result (ref eq))
                          (ref.eq (local.get $mode) (global.get $missing))
                          (then (global.get $symbol:linefeed))
                          (else (local.get $mode))))
           (local.set $mode-code
                      (call $read-line-mode-code
                            (global.get $symbol:read-bytes-line)
                            (local.get $mode-val)))
           (local.set $line (call $read-line (local.get $in) (local.get $mode-val)))
           (if (ref.eq (local.get $line) (global.get $eof))
               (then (return (global.get $eof))))
           (call $string->bytes/utf-8
                 (local.get $line)
                 (global.get $false)
                 (global.get $false)
                 (global.get $false))))))

;; port-peek-bytes-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate byte-vector peek helpers.
(define port-peek-bytes-runtime-only-helpers
  (function-forms->runtime-helpers
   'port-peek-bytes-runtime-only-helpers
   '(
     ;; $peek-bytes!/custom : symbol? bytes? exact-nonnegative-integer? custom-input-port? exact-nonnegative-integer? exact-nonnegative-integer? -> exact-nonnegative-integer? or eof?
     ;;   Peek through a custom input port's synchronous peek procedure.
     (func $peek-bytes!/custom
           (param $who   (ref eq))
           (param $bstr  (ref eq)) ;; bytes?
           (param $skip  (ref eq)) ;; exact-nonnegative-integer?
           (param $port  (ref $CustomInputPort))
           (param $from  i32)
           (param $to    i32)
           (result       (ref eq))

           (local $delegate  (ref eq))
           (local $proc      (ref $Procedure))
           (local $inv       (ref $ProcedureInvoker))
           (local $args      (ref $Args))
           (local $scratch   (ref eq))
           (local $result    (ref eq))
           (local $raw-count i32)
           (local $count     i32)
           (local $capacity  i32)

           (if (struct.get $CustomInputPort $closed (local.get $port))
               (then (call $raise-input-port-closed
                           (global.get $string:peek-byte:input-port-closed))
                     (unreachable)))

           (local.set $delegate
                      (struct.get $CustomInputPort $peek-proc (local.get $port)))
           ;; A port delegate preserves the destination range directly.
           (if (ref.test (ref $InputPort) (local.get $delegate))
               (then (return
                      (call $peek-bytes!/who
                            (local.get $who)
                            (local.get $bstr)
                            (local.get $skip)
                            (local.get $delegate)
                            (ref.i31 (i32.shl (local.get $from) (i32.const 1)))
                            (ref.i31 (i32.shl (local.get $to) (i32.const 1)))))))
           ;; A custom port without a peek procedure is not ready for peeking.
           (if (ref.eq (local.get $delegate) (global.get $false))
               (then (return (global.get $false))))
           (if (i32.eqz (ref.test (ref $Procedure) (local.get $delegate)))
               (then (return (global.get $false))))

           ;; The callback receives a buffer for exactly the selected range.
           ;; Copy a successful result back into the caller's byte string.
           (local.set $capacity (i32.sub (local.get $to) (local.get $from)))
           (local.set $scratch
                      (call $make-bytes
                            (ref.i31 (i32.shl (local.get $capacity) (i32.const 1)))
                            (global.get $missing)))
           (local.set $proc (ref.cast (ref $Procedure) (local.get $delegate)))
           (local.set $inv  (struct.get $Procedure $invoke (local.get $proc)))
           (local.set $args (array.new_fixed $Args 3
                                              (local.get $scratch)
                                              (local.get $skip)
                                              (global.get $false)))
           (local.set $result
                      (call_ref $ProcedureInvoker
                                (local.get $proc)
                                (local.get $args)
                                (local.get $inv)))
           (if (ref.eq (local.get $result) (global.get $false))
               (then (return (global.get $false))))
           (if (ref.eq (local.get $result) (global.get $eof))
               (then (return (global.get $eof))))
           (if (i32.eqz (ref.test (ref i31) (local.get $result)))
               (then (return (global.get $false))))
           (local.set $raw-count (i31.get_s (ref.cast (ref i31) (local.get $result))))
           (if (i32.and (local.get $raw-count) (i32.const 1))
               (then (return (global.get $false))))
           (local.set $count (i32.shr_s (local.get $raw-count) (i32.const 1)))
           (if (i32.or (i32.lt_s (local.get $count) (i32.const 0))
                       (i32.gt_u (local.get $count) (local.get $capacity)))
               (then (return (global.get $false))))
           (drop (call $bytes-copy!
                       (local.get $bstr)
                       (ref.i31 (i32.shl (local.get $from) (i32.const 1)))
                       (local.get $scratch)
                       (ref.i31 (i32.const 0))
                       (ref.i31 (i32.shl (local.get $count) (i32.const 1)))))
           (local.get $result))

     ;; $peek-bytes!/who : symbol? bytes? exact-nonnegative-integer? input-port? exact-nonnegative-integer? exact-nonnegative-integer? -> exact-nonnegative-integer? or eof?
     ;;   Peek bytes without consuming a supported input port.
     (func $peek-bytes!/who
           (param $who   (ref eq))
           (param $bstr  (ref eq)) ;; bytes?
           (param $skip  (ref eq)) ;; exact-nonnegative-integer?
           (param $in    (ref eq)) ;; input-port?               (optional, default = (current-input-port))
           (param $start (ref eq)) ;; exact-nonnegative-integer? (optional, default = 0)
           (param $end   (ref eq)) ;; exact-nonnegative-integer? (optional, default = (bytes-length bstr))
           (result       (ref eq))

           (local $bs          (ref $Bytes))
           (local $arr         (ref $I8Array))
           (local $len         i32)
           (local $from        i32)
           (local $to          i32)
           (local $count       i32)
           (local $skip-count  i32)
           (local $sp          (ref null $InputStringPort))
           (local $port-bytes  (ref $Bytes))
           (local $port-arr    (ref $I8Array))
           (local $idx         i32)
           (local $limit       i32)
           (local $remaining   i32)
           (local $peek-start  i32)
           (local $copy-count  i32)
           (local $i           i32)
           (local $dest-idx    i32)
           (local $byte        i32)

           ;; --- Validate byte string argument ---
           (if (i32.eqz (ref.test (ref $Bytes) (local.get $bstr)))
               (then (call $raise-argument-error1
                           (local.get $who)
                           (global.get $string:bytes?)
                           (local.get $bstr))
                     (unreachable)))
           (local.set $bs (ref.cast (ref $Bytes) (local.get $bstr)))
           (local.set $arr (struct.get $Bytes $bs (local.get $bs)))
           (local.set $len (call $i8array-length (local.get $arr)))

           ;; --- Decode skip amount ---
           (local.set $skip-count
                      (call $exact-nonnegative-integer->i32/saturating
                            (local.get $skip)))
           (if (i32.lt_s (local.get $skip-count) (i32.const 0))
               (then (call $raise-port-bytes-index-expected
                           (local.get $who)
                           (local.get $skip))))

           ;; --- Determine input port ---
           (if (ref.eq (local.get $in) (global.get $missing))
               (then (local.set $in (call $current-input-port
                                          (global.get $missing)))))
           (if (i32.eqz (call $input-port-like?/i32 (local.get $in)))
               (then (call $raise-argument-error1
                           (local.get $who)
                           (global.get $string:input-port?)
                           (local.get $in))
                     (unreachable)))
           ;; --- Decode optional start index ---
           (if (ref.eq (local.get $start) (global.get $missing))
               (then (local.set $from (i32.const 0)))
               (else
                (local.set $from
                           (call $exact-nonnegative-integer->i32/saturating
                                 (local.get $start)))
                (if (i32.lt_s (local.get $from) (i32.const 0))
                    (then (call $raise-port-bytes-index-expected
                                (local.get $who)
                                (local.get $start))))))
           ;; --- Decode optional end index ---
           (if (ref.eq (local.get $end) (global.get $missing))
               (then (local.set $to (local.get $len)))
               (else
                (local.set $to
                           (call $exact-nonnegative-integer->i32/saturating
                                 (local.get $end)))
                (if (i32.lt_s (local.get $to) (i32.const 0))
                    (then (call $raise-port-bytes-index-expected
                                (local.get $who)
                                (local.get $end))))))
           ;; --- Bounds checks ---
           (if (i32.gt_u (local.get $from) (local.get $to))
               (then (call $raise-bad-port-bytes-range
                           (local.get $who)
                           (local.get $bstr)
                           (local.get $from)
                           (local.get $to))
                     (unreachable)))
           (if (i32.gt_u (local.get $to) (local.get $len))
               (then (call $raise-bad-port-bytes-range
                           (local.get $who)
                           (local.get $bstr)
                           (local.get $from)
                           (local.get $to))
                     (unreachable)))

           (local.set $count (i32.sub (local.get $to) (local.get $from)))
           (if (i32.eqz (local.get $count))
               (then (return (ref.i31 (i32.const 0)))))

           (if (ref.test (ref $CustomInputPort) (local.get $in))
               (then (return
                      (call $peek-bytes!/custom
                            (local.get $who)
                            (local.get $bstr)
                            (local.get $skip)
                            (ref.cast (ref $CustomInputPort) (local.get $in))
                            (local.get $from)
                            (local.get $to)))))
           (if (i32.eqz (ref.test (ref $InputStringPort) (local.get $in)))
               (then (call $raise-check-string-port
                           (local.get $who)
                           (local.get $in))
                     (unreachable)))
           (local.set $sp (ref.cast (ref $InputStringPort) (local.get $in)))

           ;; --- Determine peek window ---
           (local.set $idx       (struct.get $InputStringPort $idx (local.get $sp)))
           (local.set $limit     (struct.get $InputStringPort $len (local.get $sp)))
           (local.set $remaining (i32.sub (local.get $limit) (local.get $idx)))
           (if (i32.le_u (local.get $remaining) (local.get $skip-count))
               (then (return (global.get $eof))))
           (local.set $peek-start (i32.add (local.get $idx)   (local.get $skip-count)))
           (local.set $remaining  (i32.sub (local.get $limit) (local.get $peek-start)))

           ;; --- Determine number of bytes to copy ---
           (local.set $copy-count (local.get $count))
           (if (i32.lt_u (local.get $remaining) (local.get $copy-count))
               (then (local.set $copy-count (local.get $remaining))))

           (local.set $port-bytes (struct.get $InputStringPort $bytes (local.get $sp)))
           (local.set $port-arr (struct.get $Bytes $bs (local.get $port-bytes)))
           (local.set $i (i32.const 0))
           ;; --- Copy bytes without consuming the port ---
           (block $done
                  (loop $loop
                        (br_if $done (i32.ge_u (local.get $i) (local.get $copy-count)))
                        (local.set $dest-idx (i32.add (local.get $from) (local.get $i)))
                        (local.set $byte
                                   (array.get_u $I8Array
                                                (local.get $port-arr)
                                                (i32.add (local.get $peek-start) (local.get $i))))
                        (call $i8array-set! (local.get $arr) (local.get $dest-idx) (local.get $byte))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br $loop)))

           ;; --- Report number of bytes peeked ---
           (ref.i31 (i32.shl (local.get $copy-count) (i32.const 1))))

     (func $peek-bytes! (type $Prim25)
           (param $bstr  (ref eq)) ;; bytes?
           (param $skip  (ref eq)) ;; exact-nonnegative-integer?
           (param $in    (ref eq)) ;; input-port?               (optional, default = (current-input-port))
           (param $start (ref eq)) ;; exact-nonnegative-integer? (optional, default = 0)
           (param $end   (ref eq)) ;; exact-nonnegative-integer? (optional, default = (bytes-length bstr))
           (result       (ref eq))
           (call $peek-bytes!/who
                 (global.get $symbol:peek-bytes!)
                 (local.get $bstr)
                 (local.get $skip)
                 (local.get $in)
                 (local.get $start)
                 (local.get $end)))


     (func $peek-bytes-avail!:progress-not-supported
           (param $who      (ref eq))
           (param $progress (ref eq))

           (call $raise-argument-error1
                 (local.get $who)
                 (global.get $string:progress-or-false-contract)
                 (local.get $progress))
           (unreachable))

     ;; NOTE: Like Racket's peek-bytes-avail!, but progress events are not supported
     ;;       and the progress argument must be #f.
     ;;       Takes at least 1 argument and at most 6.
     ;;       Instead of introducing a new shaped ($Prim16) we reuse $Prim>=1
     (func $peek-bytes-avail!/who
           (param $who  (ref eq))
           (param $prim (ref $PrimitiveProcedure))
           (param $bstr (ref eq)) ;; bytes?
           (param $rest (ref eq)) ;; remaining arguments as a list
           (result      (ref eq))

           (local $args     (ref eq))
           (local $node     (ref $Pair))
           (local $arg      (ref eq))
           (local $count    i32)
           (local $skip     (ref eq))
           (local $progress (ref eq))
           (local $in       (ref eq))
           (local $start    (ref eq))
           (local $end      (ref eq))

           ;; Initialize optional arguments to "missing"
           (local.set $skip     (global.get $missing))
           (local.set $progress (global.get $missing))
           (local.set $in       (global.get $missing))
           (local.set $start    (global.get $missing))
           (local.set $end      (global.get $missing))

           ;; Decode rest arguments (skip, progress, in, start, end)
           (local.set $args  (local.get $rest))
           (local.set $count (i32.const 0))
           (block $done
                  (loop $loop
                        (if (ref.eq (local.get $args) (global.get $null))
                            (then (br $done)))
                        (local.set $node (ref.cast (ref $Pair) (local.get $args)))
                        (local.set $arg  (struct.get $Pair $a (local.get $node)))
                        (local.set $args (struct.get $Pair $d (local.get $node)))
                        (local.set $count (i32.add (local.get $count) (i32.const 1)))
                        (if (i32.gt_u (local.get $count) (i32.const 5))
                            (then (call $primitive-invoke:raise-arity-error
                                        (local.get $prim)
                                        (local.get $count))
                                  (unreachable)))
                        (if (i32.eq (local.get $count) (i32.const 1))
                            (then (local.set $skip (local.get $arg)))
                            (else
                             (if (i32.eq (local.get $count) (i32.const 2))
                                 (then (local.set $progress (local.get $arg)))
                                 (else
                                  (if (i32.eq (local.get $count) (i32.const 3))
                                      (then (local.set $in (local.get $arg)))
                                      (else
                                       (if (i32.eq (local.get $count) (i32.const 4))
                                           (then (local.set $start (local.get $arg)))
                                           (else (local.set $end (local.get $arg))))))))))
                        (br $loop)))

           ;; Require the skip argument
           (if (ref.eq (local.get $skip) (global.get $missing))
               (then (call $primitive-invoke:raise-arity-error
                           (local.get $prim)
                           (local.get $count))
                     (unreachable)))

           ;; Progress defaults to #f and must be #f
           (if (ref.eq (local.get $progress) (global.get $missing))
               (then (local.set $progress (global.get $false))))
           (if (i32.eqz (ref.eq (local.get $progress) (global.get $false)))
               (then (call $peek-bytes-avail!:progress-not-supported
                           (local.get $who)
                           (local.get $progress))
                     (unreachable)))

           (call $peek-bytes!/who
                 (local.get $who)
                 (local.get $bstr)
                 (local.get $skip)
                 (local.get $in)
                 (local.get $start)
                 (local.get $end)))

     (func $peek-bytes-avail! (type $Prim>=1)
           (param $bstr (ref eq))
           (param $rest (ref eq))
           (result      (ref eq))

           (call $peek-bytes-avail!/who
                 (global.get $symbol:peek-bytes-avail!)
                 (ref.cast (ref $PrimitiveProcedure)
                           (global.get $prim:peek-bytes-avail!))
                 (local.get $bstr)
                 (local.get $rest)))

     ;; NOTE: Like Racket's peek-bytes-avail!*, but progress events are not supported
     ;;       and the progress argument must be #f.
     ;;       Takes at least 1 argument and at most 6.
     (func $peek-bytes-avail!* (type $Prim>=1)
           (param $bstr (ref eq))
           (param $rest (ref eq))
           (result      (ref eq))

           (call $peek-bytes-avail!/who
                 (global.get $symbol:peek-bytes-avail!*)
                 (ref.cast (ref $PrimitiveProcedure)
                           (global.get $prim:peek-bytes-avail!*))
                 (local.get $bstr)
                 (local.get $rest))))))

;; port-peek-string-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate string peek helpers.
(define port-peek-string-runtime-only-helpers
  (function-forms->runtime-helpers
   'port-peek-string-runtime-only-helpers
   (list
    `(func $peek-string! (type $Prim25)
           (param $str   (ref eq)) ;; string?
           (param $skip  (ref eq)) ;; exact-nonnegative-integer?
           (param $in    (ref eq)) ;; input-port?                (optional, default = (current-input-port))
           (param $start (ref eq)) ;; exact-nonnegative-integer? (optional, default = 0)
           (param $end   (ref eq)) ;; exact-nonnegative-integer? (optional, default = (string-length str))
           (result       (ref eq))

           (local $s            (ref $String))
           (local $arr          (ref $I32Array))
           (local $len          i32)
           (local $from         i32)
           (local $to           i32)
           (local $count        i32)
           (local $skip-count   i32)
           (local $sp           (ref null $InputStringPort))
           (local $port-bytes   (ref $Bytes))
           (local $src          (ref $I8Array))
           (local $idx          i32)
           (local $limit        i32)
           (local $peek-start   i32)
           (local $skipped      i32)
           (local $written      i32)
           (local $byte         i32)
           (local $need         i32)
           (local $initial-need i32)
           (local $acc          i32)
           (local $cp           i32)
           (local $cont         i32)
           (local $dest-idx     i32)

           ;; --- Validate string argument ---
           (if (i32.eqz (ref.test (ref $String) (local.get $str)))
               (then (call $raise-argument-error1
                           (global.get $symbol:peek-string!)
                           (global.get $string:string?)
                           (local.get $str))
                     (unreachable)))
           (local.set $s (ref.cast (ref $String) (local.get $str)))
           (local.set $arr (struct.get $String $codepoints (local.get $s)))
           (local.set $len (call $i32array-length (local.get $arr)))

           ;; --- Decode skip amount ---
           (local.set $skip-count
                      (call $exact-nonnegative-integer->i32/saturating
                            (local.get $skip)))
           (if (i32.lt_s (local.get $skip-count) (i32.const 0))
               (then (call $raise-port-range-index-expected
                           (global.get $symbol:peek-string!)
                           (local.get $skip))))

           ;; --- Determine input port ---
           (if (ref.eq (local.get $in) (global.get $missing))
               (then (local.set $in (call $current-input-port
                                          (global.get $missing)))))
           (if (i32.eqz (call $input-port-like?/i32 (local.get $in)))
               (then (call $raise-argument-error1
                           (global.get $symbol:peek-string!)
                           (global.get $string:input-port?)
                           (local.get $in))
                     (unreachable)))
           (if (i32.eqz (ref.test (ref $InputStringPort) (local.get $in)))
               (then (call $raise-check-string-port
                           (global.get $symbol:peek-string!)
                           (local.get $in))
                     (unreachable)))
           (local.set $sp (ref.cast (ref $InputStringPort) (local.get $in)))

           ;; --- Decode optional start index ---
           (if (ref.eq (local.get $start) (global.get $missing))
               (then (local.set $from (i32.const 0)))
               (else
                (local.set $from
                           (call $exact-nonnegative-integer->i32/saturating
                                 (local.get $start)))
                (if (i32.lt_s (local.get $from) (i32.const 0))
                    (then (call $raise-port-range-index-expected
                                (global.get $symbol:peek-string!)
                                (local.get $start))))))
           ;; --- Decode optional end index ---
           (if (ref.eq (local.get $end) (global.get $missing))
               (then (local.set $to (local.get $len)))
               (else
                (local.set $to
                           (call $exact-nonnegative-integer->i32/saturating
                                 (local.get $end)))
                (if (i32.lt_s (local.get $to) (i32.const 0))
                    (then (call $raise-port-range-index-expected
                                (global.get $symbol:peek-string!)
                                (local.get $end))))))
           ;; --- Bounds checks ---
           (if (i32.gt_u (local.get $from) (local.get $to))
               (then (call $raise-bad-string-range
                           (global.get $symbol:peek-string!)
                           (local.get $str)
                           (local.get $from)
                           (local.get $to))
                     (unreachable)))
           (if (i32.gt_u (local.get $to) (local.get $len))
               (then (call $raise-bad-string-range
                           (global.get $symbol:peek-string!)
                           (local.get $str)
                           (local.get $from)
                           (local.get $to))
                     (unreachable)))

           (local.set $count (i32.sub (local.get $to) (local.get $from)))
           (if (i32.eqz (local.get $count))
               (then (return (ref.i31 (i32.const 0)))))

           ;; --- Determine peek window and skip characters ---
           (local.set $idx        (struct.get $InputStringPort $idx   (local.get $sp)))
           (local.set $limit      (struct.get $InputStringPort $len   (local.get $sp)))
           (local.set $port-bytes (struct.get $InputStringPort $bytes (local.get $sp)))
           (local.set $src        (struct.get $Bytes $bs (local.get $port-bytes)))
           (local.set $peek-start (local.get $idx))
           (local.set $skipped    (i32.const 0))
           (block $skip-done
                  (loop $skip
                        (br_if $skip-done (i32.ge_u (local.get $skipped) (local.get $skip-count)))
                        (if (i32.ge_u (local.get $peek-start) (local.get $limit))
                            (then (return (global.get $eof))))
                        (local.set $byte (array.get_u $I8Array (local.get $src) (local.get $peek-start)))
                        (local.set $peek-start (i32.add (local.get $peek-start) (i32.const 1)))
                        (if (i32.lt_u (local.get $byte) (i32.const 128))
                            (then
                             (local.set $skipped (i32.add (local.get $skipped) (i32.const 1))))
                            (else
                             (call $bytes->string/utf-8:determine-utf-8-sequence (local.get $byte))
                             (local.set $acc)
                             (local.set $need)
                             (local.set $initial-need (local.get $need))
                             (if (i32.lt_s (local.get $need) (i32.const 0))
                                 (then (return (global.get $false))))
                             (local.set $cp (local.get $acc))
                             (block $skip-cont-done
                                    (loop $skip-cont
                                          (br_if $skip-cont-done (i32.eqz (local.get $need)))
                                          (if (i32.ge_u (local.get $peek-start) (local.get $limit))
                                              (then (return (global.get $eof))))
                                          (local.set $cont (array.get_u $I8Array (local.get $src) (local.get $peek-start)))
                                          (local.set $peek-start (i32.add (local.get $peek-start) (i32.const 1)))
                                          (if (i32.or (i32.lt_u (local.get $cont) (i32.const 128))
                                                      (i32.ge_u (local.get $cont) (i32.const 192)))
                                              (then (return (global.get $false))))
                                          (local.set $cp
                                                     (i32.or (i32.shl (local.get $cp) (i32.const 6))
                                                             (i32.and (local.get $cont) (i32.const 63))))
                                          (local.set $need (i32.sub (local.get $need) (i32.const 1)))
                                          (br $skip-cont)))
                             (if (i32.gt_u (local.get $cp) (i32.const #x10FFFF))
                                 (then (return (global.get $false))))
                             (if (i32.and (i32.ge_u (local.get $cp) (i32.const #xD800))
                                          (i32.le_u (local.get $cp) (i32.const #xDFFF)))
                                 (then (return (global.get $false))))
                             (if (i32.eq (local.get $initial-need) (i32.const 1))
                                 (then (if (i32.lt_u (local.get $cp) (i32.const #x80))
                                           (then (return (global.get $false))))))
                             (if (i32.eq (local.get $initial-need) (i32.const 2))
                                 (then (if (i32.lt_u (local.get $cp) (i32.const #x800))
                                           (then (return (global.get $false))))))
                             (if (i32.eq (local.get $initial-need) (i32.const 3))
                                 (then (if (i32.lt_u (local.get $cp) (i32.const #x10000))
                                           (then (return (global.get $false))))))
                             (local.set $skipped (i32.add (local.get $skipped) (i32.const 1)))))
                        (br $skip)))

           (local.set $written (i32.const 0))

           ;; --- Decode characters without consuming the port ---
           (block $done
                  (loop $loop
                        (br_if $done (i32.ge_u (local.get $written) (local.get $count)))
                        (if (i32.ge_u (local.get $peek-start) (local.get $limit))
                            (then (if (i32.eqz (local.get $written))
                                      (then (return (global.get $eof)))
                                      (else (br $done)))))
                        (local.set $byte (array.get_u $I8Array (local.get $src) (local.get $peek-start)))
                        (local.set $peek-start (i32.add (local.get $peek-start) (i32.const 1)))
                        (if (i32.lt_u (local.get $byte) (i32.const 128))
                            (then (local.set $cp (local.get $byte))
                                  (local.set $initial-need (i32.const 0)))
                            (else
                             (call $bytes->string/utf-8:determine-utf-8-sequence (local.get $byte))
                             (local.set $acc)
                             (local.set $need)
                             (local.set $initial-need (local.get $need))
                             (if (i32.lt_s (local.get $need) (i32.const 0))
                                 (then (return (global.get $false))))
                             (local.set $cp (local.get $acc))
                             (block $cont-done
                                    (loop $cont
                                          (br_if $cont-done (i32.eqz (local.get $need)))
                                          (if (i32.ge_u (local.get $peek-start) (local.get $limit))
                                              (then (if (i32.eqz (local.get $written))
                                                        (then (return (global.get $eof)))
                                                        (else (br $done)))))
                                          (local.set $cont (array.get_u $I8Array (local.get $src) (local.get $peek-start)))
                                          (local.set $peek-start (i32.add (local.get $peek-start) (i32.const 1)))
                                          (if (i32.or (i32.lt_u (local.get $cont) (i32.const 128))
                                                      (i32.ge_u (local.get $cont) (i32.const 192)))
                                              (then (return (global.get $false))))
                                          (local.set $cp
                                                     (i32.or (i32.shl (local.get $cp) (i32.const 6))
                                                             (i32.and (local.get $cont) (i32.const 63))))
                                          (local.set $need (i32.sub (local.get $need) (i32.const 1)))
                                          (br $cont)))
                             (if (i32.gt_u (local.get $cp) (i32.const #x10FFFF))
                                 (then (return (global.get $false))))
                             (if (i32.and (i32.ge_u (local.get $cp) (i32.const #xD800))
                                          (i32.le_u (local.get $cp) (i32.const #xDFFF)))
                                 (then (return (global.get $false))))
                             (if (i32.eq (local.get $initial-need) (i32.const 1))
                                 (then (if (i32.lt_u (local.get $cp) (i32.const #x80))
                                           (then (return (global.get $false))))))
                             (if (i32.eq (local.get $initial-need) (i32.const 2))
                                 (then (if (i32.lt_u (local.get $cp) (i32.const #x800))
                                           (then (return (global.get $false))))))
                             (if (i32.eq (local.get $initial-need) (i32.const 3))
                                 (then (if (i32.lt_u (local.get $cp) (i32.const #x10000))
                                           (then (return (global.get $false))))))))
                        (local.set $dest-idx (i32.add (local.get $from) (local.get $written)))
                        (call $i32array-set! (local.get $arr) (local.get $dest-idx) (local.get $cp))
                        (local.set $written (i32.add (local.get $written) (i32.const 1)))
                        (br $loop)))

           ;; --- Report number of characters peeked ---
           (ref.i31 (i32.shl (local.get $written) (i32.const 1)))))))

;; port-peek-single-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate single byte/character peek helpers.
(define port-peek-single-runtime-only-helpers
  (function-forms->runtime-helpers
   'port-peek-single-runtime-only-helpers
   (list
    '(func $peek-byte (type $Prim02)
           (param $in   (ref eq)) ;; input-port?                (optional, default = (current-input-port))
           (param $skip (ref eq)) ;; exact-nonnegative-integer? (optional, default = 0)
           (result      (ref eq))

           (local $sp         (ref null $InputStringPort))
           (local $bs         (ref eq))
           (local $arr        (ref $I8Array))
           (local $idx        i32)
           (local $limit      i32)
           (local $remaining  i32)
           (local $skip-count i32)
           (local $peek-idx   i32)
           (local $byte       i32)
           (local $skip-arg   (ref eq))

           (if (ref.eq (local.get $in) (global.get $missing))
               (then (local.set $in (call $current-input-port
                                          (global.get $missing)))))

           ;; Decode optional skip amount, defaulting to 0.
           (local.set $skip-count (i32.const 0))
           (local.set $skip-arg   (ref.i31 (i32.const 0)))
           (if (ref.eq (local.get $skip) (global.get $missing))
               (then)
               (else
                (local.set $skip-count
                           (call $exact-nonnegative-integer->i32/saturating
                                 (local.get $skip)))
                (if (i32.lt_s (local.get $skip-count) (i32.const 0))
                    (then (call $raise-port-bytes-index-expected
                                (global.get $symbol:peek-byte)
                                (local.get $skip))))
                (local.set $skip-arg (local.get $skip))))

           (if (i32.eqz (call $input-port-like?/i32 (local.get $in)))
               (then (call $raise-argument-error1
                           (global.get $symbol:peek-byte)
                           (global.get $string:input-port?)
                           (local.get $in))))

           ;; Ensure the input is a supported port and dispatch accordingly.
           (if (ref.test (ref $InputStringPort) (local.get $in))
               (then (local.set $sp (ref.cast (ref $InputStringPort) (local.get $in))))
               (else
                (if (ref.test (ref $CustomInputPort) (local.get $in))
                    (then (return (call $peek-byte/custom
                                         (ref.cast (ref $CustomInputPort) (local.get $in))
                                         (local.get $skip-arg))))
                    (else (return (global.get $false))))))
           (if (struct.get $InputStringPort $closed (local.get $sp))
               (then (call $raise-input-port-closed
                           (global.get $string:peek-byte:input-port-closed))
                     (unreachable)))

           (local.set $idx       (struct.get $InputStringPort $idx (local.get $sp)))
           (local.set $limit     (struct.get $InputStringPort $len (local.get $sp)))
           (local.set $remaining (i32.sub (local.get $limit) (local.get $idx)))
           (if (i32.ge_u (local.get $skip-count) (local.get $remaining))
               (then (return (global.get $eof))))
           (local.set $peek-idx (i32.add (local.get $idx) (local.get $skip-count)))

           (local.set $bs       (struct.get $InputStringPort $bytes (local.get $sp)))
           (local.set $arr      (struct.get $Bytes $bs (ref.cast (ref $Bytes) (local.get $bs))))
           (local.set $byte     (array.get_u $I8Array (local.get $arr) (local.get $peek-idx)))

           (ref.i31 (i32.shl (local.get $byte) (i32.const 1))))

    `(func $peek-char (type $Prim02)
           (param $in   (ref eq)) ;; input-port?                (optional, default = (current-input-port))
           (param $skip (ref eq)) ;; exact-nonnegative-integer? (optional, default = 0)
           (result      (ref eq))

           (local $sp           (ref null $InputStringPort))
           (local $port-bytes   (ref $Bytes))
           (local $src          (ref $I8Array))
           (local $idx          i32)
           (local $limit        i32)
           (local $skip-count   i32)
           (local $peek-start   i32)
           (local $skipped      i32)
           (local $byte         i32)
           (local $need         i32)
           (local $initial-need i32)
           (local $acc          i32)
           (local $cp           i32)
           (local $cont         i32)

           (if (ref.eq (local.get $in) (global.get $missing))
               (then (local.set $in (call $current-input-port
                                          (global.get $missing)))))
           (if (i32.eqz (call $input-port-like?/i32 (local.get $in)))
               (then (call $raise-argument-error1
                           (global.get $symbol:peek-char)
                           (global.get $string:input-port?)
                           (local.get $in))))
           ;; Ensure the input is a string port.
           (if (i32.eqz (ref.test (ref $InputStringPort) (local.get $in)))
               (then (return (global.get $false))))
           (local.set $sp (ref.cast (ref $InputStringPort) (local.get $in)))

           ;; Decode optional skip amount, defaulting to 0.
           (local.set $skip-count (i32.const 0))
           (if (ref.eq (local.get $skip) (global.get $missing))
               (then)
               (else
                (local.set $skip-count
                           (call $exact-nonnegative-integer->i32/saturating
                                 (local.get $skip)))
                (if (i32.lt_s (local.get $skip-count) (i32.const 0))
                    (then (call $raise-port-range-index-expected
                                (global.get $symbol:peek-char)
                                (local.get $skip))))))

           (local.set $idx        (struct.get $InputStringPort $idx   (local.get $sp)))
           (local.set $limit      (struct.get $InputStringPort $len   (local.get $sp)))
           (local.set $port-bytes (struct.get $InputStringPort $bytes (local.get $sp)))
           (local.set $src        (struct.get $Bytes $bs (local.get $port-bytes)))
           (local.set $peek-start (local.get $idx))
           (local.set $skipped    (i32.const 0))

           ;; Skip the requested number of characters without consuming the port.
           (block $skip-done
                  (loop $skip-loop
                        (br_if $skip-done (i32.ge_u (local.get $skipped) (local.get $skip-count)))
                        (if (i32.ge_u (local.get $peek-start) (local.get $limit))
                            (then (return (global.get $eof))))
                        (local.set $byte (array.get_u $I8Array (local.get $src) (local.get $peek-start)))
                        (local.set $peek-start (i32.add (local.get $peek-start) (i32.const 1)))
                        (if (i32.lt_u (local.get $byte) (i32.const 128))
                            (then (local.set $skipped (i32.add (local.get $skipped) (i32.const 1))))
                            (else
                             (call $bytes->string/utf-8:determine-utf-8-sequence (local.get $byte))
                             (local.set $acc)
                             (local.set $need)
                             (local.set $initial-need (local.get $need))
                             (if (i32.lt_s (local.get $need) (i32.const 0))
                                 (then (return (global.get $false))))
                             (local.set $cp (local.get $acc))
                             (block $skip-cont-done
                                    (loop $skip-cont
                                          (br_if $skip-cont-done (i32.eqz (local.get $need)))
                                          (if (i32.ge_u (local.get $peek-start) (local.get $limit))
                                              (then (return (global.get $eof))))
                                          (local.set $cont (array.get_u $I8Array (local.get $src) (local.get $peek-start)))
                                          (local.set $peek-start (i32.add (local.get $peek-start) (i32.const 1)))
                                          (if (i32.or (i32.lt_u (local.get $cont) (i32.const 128))
                                                      (i32.ge_u (local.get $cont) (i32.const 192)))
                                              (then (return (global.get $false))))
                                          (local.set $cp
                                                     (i32.or (i32.shl (local.get $cp) (i32.const 6))
                                                             (i32.and (local.get $cont) (i32.const 63))))
                                          (local.set $need (i32.sub (local.get $need) (i32.const 1)))
                                          (br $skip-cont)))
                             (if (i32.gt_u (local.get $cp) (i32.const #x10FFFF))
                                 (then (return (global.get $false))))
                             (if (i32.and (i32.ge_u (local.get $cp) (i32.const #xD800))
                                          (i32.le_u (local.get $cp) (i32.const #xDFFF)))
                                 (then (return (global.get $false))))
                             (if (i32.eq (local.get $initial-need) (i32.const 1))
                                 (then (if (i32.lt_u (local.get $cp) (i32.const #x80))
                                           (then (return (global.get $false))))))
                             (if (i32.eq (local.get $initial-need) (i32.const 2))
                                 (then (if (i32.lt_u (local.get $cp) (i32.const #x800))
                                           (then (return (global.get $false))))))
                             (if (i32.eq (local.get $initial-need) (i32.const 3))
                                 (then (if (i32.lt_u (local.get $cp) (i32.const #x10000))
                                           (then (return (global.get $false))))))
                             (local.set $skipped (i32.add (local.get $skipped) (i32.const 1)))))
                        (br $skip-loop)))

           ;; Peek the next character without consuming the port.
           (if (i32.ge_u (local.get $peek-start) (local.get $limit))
               (then (return (global.get $eof))))
           (local.set $byte (array.get_u $I8Array (local.get $src) (local.get $peek-start)))
           (local.set $peek-start (i32.add (local.get $peek-start) (i32.const 1)))

           (if (i32.lt_u (local.get $byte) (i32.const 128))
               (then (local.set $cp (local.get $byte)))
               (else
                (call $bytes->string/utf-8:determine-utf-8-sequence (local.get $byte))
                (local.set $acc)
                (local.set $need)
                (local.set $initial-need (local.get $need))
                (if (i32.lt_s (local.get $need) (i32.const 0))
                    (then (return (global.get $false))))
                (local.set $cp (local.get $acc))
                (block $cont-done
                       (loop $cont-loop
                             (br_if $cont-done (i32.eqz (local.get $need)))
                             (if (i32.ge_u (local.get $peek-start) (local.get $limit))
                                 (then (return (global.get $eof))))
                             (local.set $cont (array.get_u $I8Array (local.get $src) (local.get $peek-start)))
                             (local.set $peek-start (i32.add (local.get $peek-start) (i32.const 1)))
                             (if (i32.or (i32.lt_u (local.get $cont) (i32.const 128))
                                         (i32.ge_u (local.get $cont) (i32.const 192)))
                                 (then (return (global.get $false))))
                             (local.set $cp
                                        (i32.or (i32.shl (local.get $cp) (i32.const 6))
                                                (i32.and (local.get $cont) (i32.const 63))))
                             (local.set $need (i32.sub (local.get $need) (i32.const 1)))
                             (br $cont-loop)))
                (if (i32.gt_u (local.get $cp) (i32.const #x10FFFF))
                    (then (return (global.get $false))))
                (if (i32.and (i32.ge_u (local.get $cp) (i32.const #xD800))
                             (i32.le_u (local.get $cp) (i32.const #xDFFF)))
                    (then (return (global.get $false))))
                (if (i32.eq (local.get $initial-need) (i32.const 1))
                    (then (if (i32.lt_u (local.get $cp) (i32.const #x80))
                              (then (return (global.get $false))))))
                (if (i32.eq (local.get $initial-need) (i32.const 2))
                    (then (if (i32.lt_u (local.get $cp) (i32.const #x800))
                              (then (return (global.get $false))))))
                (if (i32.eq (local.get $initial-need) (i32.const 3))
                    (then (if (i32.lt_u (local.get $cp) (i32.const #x10000))
                              (then (return (global.get $false))))))))

           (ref.i31 (i32.or (i32.shl (local.get $cp) (i32.const ,char-shift))
                            (i32.const ,char-tag))))

    '(func $check-peek-progress-evt/who
           (param $who          (ref eq))
           (param $progress-evt (ref eq))

           (if (ref.eq (local.get $progress-evt) (global.get $missing))
               (then)
               (else
                (if (i32.and (i32.eqz (ref.eq (local.get $progress-evt) (global.get $false)))
                             (ref.eq (call $evt? (local.get $progress-evt)) (global.get $false)))
                    (then (call $raise-argument-error1
                                (local.get $who)
                                (global.get $string:evt-or-false-contract)
                                (local.get $progress-evt))
                          (unreachable))))))

    '(func $check-peek-special-wrap/who
           (param $who          (ref eq))
           (param $special-wrap (ref eq))

           (if (ref.eq (local.get $special-wrap) (global.get $missing))
               (then)
               (else
                (if (ref.eq (local.get $special-wrap) (global.get $false))
                    (then)
                    (else
                     (if (ref.eq (local.get $special-wrap) (global.get $symbol:special))
                         (then)
                         (else
                          (drop (call $check-procedure-arity/who
                                      (local.get $who)
                                      (global.get $string:peek-special-wrap-contract)
                                      (local.get $special-wrap)
                                      (i32.const 1))))))))))

    ;; peek-byte-or-special : [input-port?] [exact-nonnegative-integer?] [evt-or-false?] [special-wrap/c] [any/c] -> (or/c byte? eof-object? any/c)
    ;;   Bootstrap implementation: special values and progress events are not modeled yet.
    '(func $peek-byte-or-special (type $Prim5)
           (param $in           (ref eq)) ;; optional input-port?, default = (current-input-port)
           (param $skip         (ref eq)) ;; optional exact-nonnegative-integer?, default = 0
           (param $progress-evt (ref eq)) ;; optional evt-or-false?, validated but ignored until progress events exist
           (param $special-wrap (ref eq)) ;; optional procedure-or-false-or-special?, validated but ignored until special values exist
           (param $source-name  (ref eq)) ;; optional any/c, passed to special producers in full Racket
           (result              (ref eq))

           (if (i32.and (i32.eqz (ref.eq (local.get $in) (global.get $missing)))
                        (i32.eqz (call $input-port-like?/i32 (local.get $in))))
               (then (call $raise-argument-error1
                           (global.get $symbol:peek-byte-or-special)
                           (global.get $string:input-port?)
                           (local.get $in))
                     (unreachable)))
           (if (i32.and (i32.eqz (ref.eq (local.get $skip) (global.get $missing)))
                        (ref.eq (call $exact-nonnegative-integer? (local.get $skip))
                                (global.get $false)))
               (then (call $raise-port-bytes-index-expected
                           (global.get $symbol:peek-byte-or-special)
                           (local.get $skip))))
           (call $check-peek-progress-evt/who
                 (global.get $symbol:peek-byte-or-special)
                 (local.get $progress-evt))
           (call $check-peek-special-wrap/who
                 (global.get $symbol:peek-byte-or-special)
                 (local.get $special-wrap))
           (call $peek-byte (local.get $in) (local.get $skip)))

    ;; peek-char-or-special : [input-port?] [exact-nonnegative-integer?] [special-wrap/c] [any/c] -> (or/c char? eof-object? any/c)
    ;;   Bootstrap implementation: special values are not modeled yet.
    '(func $peek-char-or-special (type $Prim4)
           (param $in           (ref eq)) ;; optional input-port?, default = (current-input-port)
           (param $skip         (ref eq)) ;; optional exact-nonnegative-integer?, default = 0
           (param $special-wrap (ref eq)) ;; optional procedure-or-false-or-special?, validated but ignored until special values exist
           (param $source-name  (ref eq)) ;; optional any/c, passed to special producers in full Racket
           (result              (ref eq))

           (if (i32.and (i32.eqz (ref.eq (local.get $in) (global.get $missing)))
                        (i32.eqz (call $input-port-like?/i32 (local.get $in))))
               (then (call $raise-argument-error1
                           (global.get $symbol:peek-char-or-special)
                           (global.get $string:input-port?)
                           (local.get $in))
                     (unreachable)))
           (if (i32.and (i32.eqz (ref.eq (local.get $skip) (global.get $missing)))
                        (ref.eq (call $exact-nonnegative-integer? (local.get $skip))
                                (global.get $false)))
               (then (call $raise-port-range-index-expected
                           (global.get $symbol:peek-char-or-special)
                           (local.get $skip))))
           (call $check-peek-special-wrap/who
                 (global.get $symbol:peek-char-or-special)
                 (local.get $special-wrap))
           (call $peek-char (local.get $in) (local.get $skip))))))

;; port-peek-wrapper-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate high-level byte/string peek allocation wrappers.
(define port-peek-wrapper-runtime-only-helpers
  (function-forms->runtime-helpers
   'port-peek-wrapper-runtime-only-helpers
   '(
     ;; Like Racket's peek-bytes, but currently only string ports are supported.
     ;; The optional skip argument defaults to 0.
     (func $peek-bytes (type $Prim23)
           (param $amt  (ref eq)) ;; exact-nonnegative-integer?
           (param $skip (ref eq)) ;; exact-nonnegative-integer? (optional, default = 0)
           (param $in   (ref eq)) ;; input-port?                (optional, default = (current-input-port))
           (result      (ref eq))

           (local $count     i32)
           (local $skip-arg  (ref eq))
           (local $skip-val  i32)
           (local $buf       (ref $Bytes))
           (local $res       (ref eq))
           (local $peeked    i32)
           (local $arr       (ref $I8Array))
           (local $new-arr   (ref $I8Array))

           ;; --- Decode amount ---
           (local.set $count
                      (call $bytes-length/i32/who
                            (global.get $symbol:peek-bytes)
                            (local.get $amt)))

           ;; --- Decode optional skip amount ---
           (local.set $skip-arg (ref.i31 (i32.const 0)))
           (local.set $skip-val (i32.const 0))
           (if (ref.eq (local.get $skip) (global.get $missing))
               (then)
               (else
                (local.set $skip-val
                           (call $exact-nonnegative-integer->i32/saturating
                                 (local.get $skip)))
                (if (i32.lt_s (local.get $skip-val) (i32.const 0))
                    (then (call $raise-port-bytes-index-expected
                                (global.get $symbol:peek-bytes)
                                (local.get $skip))))
                (local.set $skip-arg (local.get $skip))))

           ;; --- Determine input port ---
           (if (ref.eq (local.get $in) (global.get $missing))
               (then (local.set $in (call $current-input-port
                                          (global.get $missing)))))
           (if (i32.eqz (call $input-port-like?/i32 (local.get $in)))
               (then (call $raise-argument-error1
                           (global.get $symbol:peek-bytes)
                           (global.get $string:input-port?)
                           (local.get $in))))
           (if (i32.eqz (ref.test (ref $InputStringPort) (local.get $in)))
               (then (call $raise-check-string-port
                           (global.get $symbol:peek-bytes)
                           (local.get $in))
                     (unreachable)))

           ;; --- Handle zero-length peek ---
           (if (i32.eqz (local.get $count))
               (then (return (global.get $bytes:empty))))

           ;; --- Allocate destination buffer ---
           (local.set $buf
                      (ref.cast (ref $Bytes)
                                (call $make-bytes
                                      (ref.i31 (i32.shl (local.get $count) (i32.const 1)))
                                      (global.get $missing))))

           ;; --- Fill buffer using peek-bytes! ---
           (local.set $res
                      (call $peek-bytes!
                            (local.get $buf)
                            (local.get $skip-arg)
                            (local.get $in)
                            (global.get $missing)
                            (global.get $missing)))

           ;; Propagate failure conditions
           (if (ref.eq (local.get $res) (global.get $false))
               (then (return (global.get $false))))
           (if (ref.eq (local.get $res) (global.get $eof))
               (then (return (global.get $eof))))

           (if (i32.eqz (ref.test (ref i31) (local.get $res)))
               (then (return (global.get $false))))
           (local.set $peeked (i31.get_u (ref.cast (ref i31) (local.get $res))))
           (if (i32.ne (i32.and (local.get $peeked) (i32.const 1)) (i32.const 0))
               (then (return (global.get $false))))
           (local.set $peeked (i32.shr_u (local.get $peeked) (i32.const 1)))
           ;; When no characters were peeked, signal EOF
           (if (i32.eqz (local.get $peeked))
               (then (return (global.get $eof))))

           ;; Shrink buffer on partial peek
           (if (i32.lt_u (local.get $peeked) (local.get $count))
               (then (local.set $arr (struct.get $Bytes $bs (local.get $buf)))
                     (local.set $new-arr
                                (call $i8array-copy
                                      (local.get $arr)
                                      (i32.const 0)
                                      (local.get $peeked)))
                     (struct.set $Bytes $bs (local.get $buf) (local.get $new-arr))))

           (local.get $buf))

     ;; Like Racket's peek-string, but currently only string ports are supported.
     ;; The optional skip argument defaults to 0.
     (func $peek-string (type $Prim23)
           (param $amt  (ref eq)) ;; exact-nonnegative-integer?
           (param $skip (ref eq)) ;; exact-nonnegative-integer? (optional, default = 0)
           (param $in   (ref eq)) ;; input-port?                (optional, default = (current-input-port))
           (result      (ref eq))

           (local $count     i32)
           (local $skip-arg  (ref eq))
           (local $skip-val  i32)
           (local $buf       (ref $String))
           (local $res       (ref eq))
           (local $peeked    i32)
           (local $arr       (ref $I32Array))
           (local $new-arr   (ref $I32Array))

           ;; --- Decode amount ---
           (local.set $count
                      (call $string-length/i32/who
                            (global.get $symbol:peek-string)
                            (local.get $amt)
                            (global.get $string:exact-nonnegative-integer?)))

           ;; --- Decode optional skip amount ---
           (local.set $skip-arg (ref.i31 (i32.const 0)))
           (local.set $skip-val (i32.const 0))
           (if (ref.eq (local.get $skip) (global.get $missing))
               (then)
               (else
                (local.set $skip-val
                           (call $exact-nonnegative-integer->i32/saturating
                                 (local.get $skip)))
                (if (i32.lt_s (local.get $skip-val) (i32.const 0))
                    (then (call $raise-port-range-index-expected
                                (global.get $symbol:peek-string)
                                (local.get $skip))))
                (local.set $skip-arg (local.get $skip))))

           ;; --- Determine input port ---
           (if (ref.eq (local.get $in) (global.get $missing))
               (then (local.set $in (call $current-input-port
                                          (global.get $missing)))))
           (if (i32.eqz (call $input-port-like?/i32 (local.get $in)))
               (then (call $raise-argument-error1
                           (global.get $symbol:peek-string)
                           (global.get $string:input-port?)
                           (local.get $in))))
           (if (i32.eqz (ref.test (ref $InputStringPort) (local.get $in)))
               (then (call $raise-check-string-port
                           (global.get $symbol:peek-string)
                           (local.get $in))
                     (unreachable)))

           ;; --- Handle zero-length peek ---
           (if (i32.eqz (local.get $count))
               (then (return (global.get $string:empty))))

           ;; --- Allocate destination string ---
           (local.set $buf (call $make-string/checked (local.get $count) (i32.const 0)))

           ;; --- Fill buffer using peek-string! ---
           (local.set $res
                      (call $peek-string!
                            (local.get $buf)
                            (local.get $skip-arg)
                            (local.get $in)
                            (global.get $missing)
                            (global.get $missing)))

           ;; Propagate failure conditions
           (if (ref.eq (local.get $res) (global.get $false))
               (then (return (global.get $false))))
           (if (ref.eq (local.get $res) (global.get $eof))
               (then (return (global.get $eof))))

           (if (i32.eqz (ref.test (ref i31) (local.get $res)))
               (then (return (global.get $false))))
           (local.set $peeked (i31.get_u (ref.cast (ref i31) (local.get $res))))
           (if (i32.ne (i32.and (local.get $peeked) (i32.const 1)) (i32.const 0))
               (then (return (global.get $false))))
           (local.set $peeked (i32.shr_u (local.get $peeked) (i32.const 1)))

           ;; Return EOF when no characters were peeked.
           (if (i32.eqz (local.get $peeked))
               (then (return (global.get $eof))))

           (local.set $arr (struct.get $String $codepoints (local.get $buf)))


           ;; Shrink buffer on partial peek
           (if (i32.lt_u (local.get $peeked) (local.get $count))
               (then (local.set $new-arr
                                (call $i32array-copy
                                      (local.get $arr)
                                      (i32.const 0)
                                      (local.get $peeked)))
                     (struct.set $String $codepoints (local.get $buf) (local.get $new-arr))))

           (struct.set $String $hash (local.get $buf) (i32.const 0))

           (local.get $buf)))))

;; port-byte-char-output-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate byte/character display output helpers.
(define port-byte-char-output-runtime-only-helpers
  (function-forms->runtime-helpers
   'port-byte-char-output-runtime-only-helpers
   (list
    '(func $write-byte (type $Prim12)
           (param $byte (ref eq))
           (param $out  (ref eq)) ;; optional output-port?, default = current-output-port
           (result      (ref eq))

           (local $b         i32)
           (local $sp        (ref null $OutputStringPort))
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
           (if (ref.eq (call $byte? (local.get $byte)) (global.get $false))
               (then (call $raise-argument-error1
                           (global.get $symbol:write-byte)
                           (global.get $string:byte?)
                           (local.get $byte))))
           (local.set $b (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $byte)))
                                     (i32.const 1)))
           ;; 2. Cast output to $OutputStringPort
           (if (ref.eq (local.get $out) (global.get $missing))
               (then (local.set $out (call $current-output-port
                                           (global.get $missing)))))
           (if (i32.eqz (call $output-port-like?/i32 (local.get $out)))
               (then (call $raise-argument-error1
                           (global.get $symbol:write-byte)
                           (global.get $string:output-port?)
                           (local.get $out))))
           (if (i32.eqz (ref.test (ref $OutputPort) (local.get $out)))
               (then (local.set $out (call $prop-output-port-target
                                           (local.get $out)))))
           (if (ref.test (ref $OutputStringPort) (local.get $out))
               (then (local.set $sp (ref.cast (ref $OutputStringPort) (local.get $out))))
               (else (return (global.get $false))))
           (if (struct.get $OutputStringPort $closed (local.get $sp))
               (then (call $raise-output-port-closed
                           (global.get $string:write-byte:output-port-closed))
                     (unreachable)))
           ;; 3. Get buffer and current index
           (local.set $bs  (struct.get $OutputStringPort $bytes (local.get $sp)))
           (local.set $idx (struct.get $OutputStringPort $idx   (local.get $sp)))
           ;; 4. Resize buffer if needed
           (local.set $old-len (struct.get $OutputStringPort $len (local.get $sp)))
           (if (i32.eq (local.get $idx) (local.get $old-len))
               (then (local.set $new-len (i32.shl (local.get $old-len) (i32.const 1)))
                     (local.set $new-bytes (call $i8array-extend
                                                 (struct.get $Bytes $bs (ref.cast (ref $Bytes) (local.get $bs)))
                                                 (local.get $new-len)
                                                 (i32.const 0)))
                     (struct.set $OutputStringPort $bytes (local.get $sp)
                                 (struct.new $Bytes
                                             (i32.const 0)
                                             (i32.const 0)
                                             (local.get $new-bytes)))
                     (struct.set $OutputStringPort $len (local.get $sp) (local.get $new-len))
                     (local.set $bs (struct.get $OutputStringPort $bytes (local.get $sp)))))
           ;; 5. Write byte into array
           (call $i8array-set!
                 (struct.get $Bytes $bs (ref.cast (ref $Bytes) (local.get $bs)))
                 (local.get $idx)
                 (local.get $b))
           (struct.set $OutputStringPort $idx (local.get $sp)
                       (i32.add (local.get $idx) (i32.const 1)))
           ;; 6. Load old location fields
           (local.set $loc  (struct.get $OutputStringPort $loc  (local.get $sp)))
           (local.set $pos  (struct.get $Location         $pos  (local.get $loc)))
           (local.set $line (struct.get $Location         $line (local.get $loc)))
           (local.set $col  (struct.get $Location         $col  (local.get $loc)))

           (local.set $int-pos  (if (result i32)
                                    (ref.test (ref i31) (local.get $pos))
                                    (then (i32.shr_s (i31.get_u (ref.cast (ref i31) (local.get $pos)))
                                                     (i32.const 1)))
                                    (else (i32.const 0))))
           (local.set $int-line (if (result i32)
                                    (ref.test (ref i31) (local.get $line))
                                    (then (i32.shr_s (i31.get_u (ref.cast (ref i31) (local.get $line)))
                                                     (i32.const 1)))
                                    (else (i32.const 0))))
           (local.set $int-col  (if (result i32)
                                    (ref.test (ref i31) (local.get $col))
                                    (then (i32.shr_s (i31.get_u (ref.cast (ref i31) (local.get $col)))
                                                     (i32.const 1)))
                                    (else (i32.const 0))))
           ;; 7. Decode UTF-8 byte
           (local.set $len  (struct.get $OutputStringPort $utf8-len   (local.get $sp)))
           (local.set $left (struct.get $OutputStringPort $utf8-left  (local.get $sp)))
           (local.set $seen (struct.get $OutputStringPort $utf8-bytes (local.get $sp)))
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

                          (struct.set $OutputStringPort $utf8-len   (local.get $sp) (local.get $len))
                          (struct.set $OutputStringPort $utf8-left  (local.get $sp)
                                      (i32.sub (local.get $len) (i32.const 1)))
                          (struct.set $OutputStringPort $utf8-bytes (local.get $sp) (i32.const 1)))))))
                ;; Inside a sequence
                (else
                 (block
                  (local.set $seen (i32.add (local.get $seen) (i32.const 1)))
                  (local.set $left (i32.sub (local.get $left) (i32.const 1)))
                  (struct.set $OutputStringPort $utf8-left  (local.get $sp) (local.get $left))
                  (struct.set $OutputStringPort $utf8-bytes (local.get $sp) (local.get $seen))
                  (if (i32.eqz (local.get $left))
                      (then
                       ;; UTF-8 sequence complete: count one character column.
                       (local.set $int-col
                                  (i32.add (local.get $int-col) (i32.const 1)))
                       (struct.set $OutputStringPort $utf8-len   (local.get $sp) (i32.const 0))
                       (struct.set $OutputStringPort $utf8-left  (local.get $sp) (i32.const 0))
                       (struct.set $OutputStringPort $utf8-bytes (local.get $sp) (i32.const 0))))))))
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
           (struct.set $OutputStringPort $loc (local.get $sp)
                       (struct.new $Location
                                   (i32.const 0)  ;; hash
                                   (ref.i31 (i32.shl (local.get $int-line) (i32.const 1)))
                                   (ref.i31 (i32.shl (local.get $int-col)  (i32.const 1)))
                                   (ref.i31 (i32.shl (local.get $int-pos)  (i32.const 1)))))
           ;; 10. Return void
           (global.get $void))

    '(func $write-char (type $Prim12)
           (param $char (ref eq)) ;; character?
           (param $out  (ref eq)) ;; optional output-port?, default = (current-output-port)
           (result      (ref eq))

           (local $cp   i32)
           (local $byte i32)
           (local $res  (ref eq))

           (if (ref.eq (local.get $out) (global.get $missing))
               (then (local.set $out (call $current-output-port
                                           (global.get $missing)))))
           ;; Decode the character argument to a Unicode scalar value.
           (if (ref.eq (call $char? (local.get $char)) (global.get $false))
               (then (call $raise-argument-error1
                           (global.get $symbol:write-char)
                           (global.get $string:char?)
                           (local.get $char))))
           (if (i32.eqz (call $output-port-like?/i32 (local.get $out)))
               (then (call $raise-argument-error1
                           (global.get $symbol:write-char)
                           (global.get $string:output-port?)
                           (local.get $out))))
           (local.set $cp (call $char->integer/i32 (local.get $char)))

           ;; Fast path for ASCII characters (1-byte UTF-8 sequence).
           (if (i32.le_u (local.get $cp) (i32.const 0x7f))
               (then
                (local.set $byte (local.get $cp))
                (local.set $res
                           (call $write-byte
                                 (ref.i31 (i32.shl (local.get $byte) (i32.const 1)))
                                 (local.get $out)))
                (if (ref.eq (local.get $res) (global.get $false))
                    (then (return (global.get $false))))
                (return (global.get $void))))

           ;; Two-byte UTF-8 sequence for U+0080 .. U+07FF.
           (if (i32.le_u (local.get $cp) (i32.const 0x7ff))
               (then
                (local.set $byte
                           (i32.or (i32.shr_u (local.get $cp) (i32.const 6))
                                   (i32.const 0xc0)))
                (local.set $res
                           (call $write-byte
                                 (ref.i31 (i32.shl (local.get $byte) (i32.const 1)))
                                 (local.get $out)))
                (if (ref.eq (local.get $res) (global.get $false))
                    (then (return (global.get $false))))
                (local.set $byte
                           (i32.or (i32.and (local.get $cp) (i32.const 0x3f))
                                   (i32.const 0x80)))
                (local.set $res
                           (call $write-byte
                                 (ref.i31 (i32.shl (local.get $byte) (i32.const 1)))
                                 (local.get $out)))
                (if (ref.eq (local.get $res) (global.get $false))
                    (then (return (global.get $false))))
                (return (global.get $void))))

           ;; Three-byte UTF-8 sequence for U+0800 .. U+FFFF.
           (if (i32.le_u (local.get $cp) (i32.const 0xffff))
               (then
                (local.set $byte
                           (i32.or (i32.shr_u (local.get $cp) (i32.const 12))
                                   (i32.const 0xe0)))
                (local.set $res
                           (call $write-byte
                                 (ref.i31 (i32.shl (local.get $byte) (i32.const 1)))
                                 (local.get $out)))
                (if (ref.eq (local.get $res) (global.get $false))
                    (then (return (global.get $false))))
                (local.set $byte
                           (i32.or (i32.and (i32.shr_u (local.get $cp) (i32.const 6))
                                            (i32.const 0x3f))
                                   (i32.const 0x80)))
                (local.set $res
                           (call $write-byte
                                 (ref.i31 (i32.shl (local.get $byte) (i32.const 1)))
                                 (local.get $out)))
                (if (ref.eq (local.get $res) (global.get $false))
                    (then (return (global.get $false))))
                (local.set $byte
                           (i32.or (i32.and (local.get $cp) (i32.const 0x3f))
                                   (i32.const 0x80)))
                (local.set $res
                           (call $write-byte
                                 (ref.i31 (i32.shl (local.get $byte) (i32.const 1)))
                                 (local.get $out)))
                (if (ref.eq (local.get $res) (global.get $false))
                    (then (return (global.get $false))))
                (return (global.get $void))))

           ;; Four-byte UTF-8 sequence for U+10000 .. U+10FFFF.
           (local.set $byte
                      (i32.or (i32.shr_u (local.get $cp) (i32.const 18))
                              (i32.const 0xf0)))
           (local.set $res
                      (call $write-byte
                            (ref.i31 (i32.shl (local.get $byte) (i32.const 1)))
                            (local.get $out)))
           (if (ref.eq (local.get $res) (global.get $false))
               (then (return (global.get $false))))
           (local.set $byte
                      (i32.or (i32.and (i32.shr_u (local.get $cp) (i32.const 12))
                                       (i32.const 0x3f))
                              (i32.const 0x80)))
           (local.set $res
                      (call $write-byte
                            (ref.i31 (i32.shl (local.get $byte) (i32.const 1)))
                            (local.get $out)))
           (if (ref.eq (local.get $res) (global.get $false))
               (then (return (global.get $false))))
           (local.set $byte
                      (i32.or (i32.and (i32.shr_u (local.get $cp) (i32.const 6))
                                       (i32.const 0x3f))
                              (i32.const 0x80)))
           (local.set $res
                      (call $write-byte
                            (ref.i31 (i32.shl (local.get $byte) (i32.const 1)))
                            (local.get $out)))
           (if (ref.eq (local.get $res) (global.get $false))
               (then (return (global.get $false))))
           (local.set $byte
                      (i32.or (i32.and (local.get $cp) (i32.const 0x3f))
                              (i32.const 0x80)))
           (local.set $res
                      (call $write-byte
                            (ref.i31 (i32.shl (local.get $byte) (i32.const 1)))
                            (local.get $out)))
           (if (ref.eq (local.get $res) (global.get $false))
               (then (return (global.get $false))))
           (return (global.get $void))

           ;; Should be unreachable because all cases return above.
           (global.get $void))

    `(func $newline (type $Prim01)
           (param $out (ref eq)) ;; optional output-port?, default = (current-output-port)
           (result     (ref eq))

           (local $res (ref eq))

           (if (ref.eq (local.get $out) (global.get $missing))
               (then (local.set $out (call $current-output-port
                                           (global.get $missing)))))
           (if (i32.eqz (call $output-port-like?/i32 (local.get $out)))
               (then (call $raise-argument-error1
                           (global.get $symbol:newline)
                           (global.get $string:output-port?)
                           (local.get $out))))

           ;; Delegate to write-char with the newline character.
           (call $write-char ,(Imm #\newline) (local.get $out)))

    ;; display : any/c [output-port?] -> void?
    ;;   Display a value using WebRacket's current display formatter.
    ;;   Optional output port defaults to (current-output-port).
    '(func $display (type $Prim12)
           (param $v   (ref eq)) ;; any/c
           (param $out (ref eq)) ;; output-port? (optional, default = (current-output-port))
           (result     (ref eq))

           (if (ref.eq (local.get $out) (global.get $missing))
               (then (local.set $out (call $current-output-port
                                           (global.get $missing)))))
           (if (i32.eqz (call $output-port-like?/i32 (local.get $out)))
               (then (call $raise-argument-error1
                           (global.get $symbol:display)
                           (global.get $string:output-port?)
                           (local.get $out))))
           (drop (call $write-string
                       (call $format/display (local.get $v))
                       (local.get $out)
                       (global.get $missing)
                       (global.get $missing)))
           (global.get $void))

    ;; displayln : any/c [output-port?] -> void?
    ;;   Display a value followed by a newline.
    ;;   Optional output port defaults to (current-output-port).
    '(func $displayln (type $Prim12)
           (param $v   (ref eq)) ;; any/c
           (param $out (ref eq)) ;; output-port? (optional, default = (current-output-port))
           (result     (ref eq))

           (if (ref.eq (local.get $out) (global.get $missing))
               (then (local.set $out (call $current-output-port
                                           (global.get $missing)))))
           (if (i32.eqz (call $output-port-like?/i32 (local.get $out)))
               (then (call $raise-argument-error1
                           (global.get $symbol:displayln)
                           (global.get $string:output-port?)
                           (local.get $out))))
           (drop (call $display (local.get $v) (local.get $out)))
           (drop (call $newline (local.get $out)))
           (global.get $void)))))

;; port-byte-string-output-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate byte/string output delegation helpers.
(define port-byte-string-output-runtime-only-helpers
  (function-forms->runtime-helpers
   'port-byte-string-output-runtime-only-helpers
   '(
     ;; write-bytes/custom : custom-output-port? bytes? i32 i32 -> exact-nonnegative-integer? or #f
     ;;   Dispatch a byte slice to a custom output port's delegate.
     (func $write-bytes/custom
           (param $port (ref $CustomOutputPort)) ;; custom-output-port?
           (param $bstr (ref $Bytes))            ;; bytes?
           (param $from i32)                     ;; start byte
           (param $to   i32)                     ;; end byte
           (result      (ref eq))

           (local $delegate (ref eq))
           (local $proc     (ref $Procedure))
           (local $inv      (ref $ProcedureInvoker))
           (local $args     (ref $Args))
           (local $result   (ref eq))
           (local $n        i32)

           (if (struct.get $CustomOutputPort $closed (local.get $port))
               (then (call $raise-output-port-closed
                           (global.get $string:write-byte:output-port-closed))
                     (unreachable)))
           (local.set $delegate
                      (struct.get $CustomOutputPort $write-proc (local.get $port)))
           (if (ref.test (ref $OutputPort) (local.get $delegate))
               (then
                (local.set $result
                           (call $write-bytes
                                 (ref.cast (ref eq) (local.get $bstr))
                                 (local.get $delegate)
                                 (ref.i31 (i32.shl (local.get $from) (i32.const 1)))
                                 (ref.i31 (i32.shl (local.get $to)   (i32.const 1)))))
                (if (ref.test (ref i31) (local.get $result))
                    (then
                     (local.set $n (i32.shr_u
                                    (i31.get_u (ref.cast (ref i31) (local.get $result)))
                                    (i32.const 1)))
                     (struct.set $CustomOutputPort $idx
                                 (local.get $port)
                                 (i32.add
                                  (struct.get $CustomOutputPort $idx (local.get $port))
                                  (local.get $n)))))
                (return (local.get $result))))

           (local.set $proc (ref.cast (ref $Procedure) (local.get $delegate)))
           (local.set $inv  (struct.get $Procedure $invoke (local.get $proc)))
           (local.set $args
                      (array.new_fixed $Args 5
                                       (ref.cast (ref eq) (local.get $bstr))
                                       (ref.i31 (i32.shl (local.get $from) (i32.const 1)))
                                       (ref.i31 (i32.shl (local.get $to)   (i32.const 1)))
                                       (global.get $false)
                                       (global.get $true)))
           (local.set $result
                      (call $expect-one-value
                            (call_ref $ProcedureInvoker
                                      (local.get $proc)
                                      (local.get $args)
                                      (local.get $inv))))
           (if (ref.test (ref i31) (local.get $result))
               (then
                (local.set $n (i32.shr_u
                               (i31.get_u (ref.cast (ref i31) (local.get $result)))
                               (i32.const 1)))
                (struct.set $CustomOutputPort $idx
                            (local.get $port)
                            (i32.add
                             (struct.get $CustomOutputPort $idx (local.get $port))
                             (local.get $n)))))
           (local.get $result))

     ;; Like Racket's write-bytes.
     (func $write-bytes (type $Prim14)
           (param $bstr  (ref eq)) ;; bytes?
           (param $out   (ref eq)) ;; output-port?               (optional, default = (current-output-port))
           (param $start (ref eq)) ;; exact-nonnegative-integer? (optional, default = 0)
           (param $end   (ref eq)) ;; exact-nonnegative-integer? (optional, default = (bytes-length bstr))
           (result       (ref eq))

           (local $bs    (ref $Bytes))
           (local $arr   (ref $I8Array))
           (local $len   i32)
           (local $from  i32)
           (local $to    i32)
           (local $count i32)
           (local $idx   i32)
           (local $byte  i32)
           (local $i     i32)
           (local $res   (ref eq))
           (local $discard?  i32)

           ;; --- Validate byte string argument ---
           (if (i32.eqz (ref.test (ref $Bytes) (local.get $bstr)))
               (then (call $raise-argument-error1
                           (global.get $symbol:write-bytes)
                           (global.get $string:bytes?)
                           (local.get $bstr))
                     (unreachable)))
           (local.set $bs  (ref.cast (ref $Bytes) (local.get $bstr)))
           (local.set $arr (struct.get $Bytes $bs (local.get $bs)))
           (local.set $len (call $i8array-length (local.get $arr)))
           ;; --- Determine output port ---
           (if (ref.eq (local.get $out) (global.get $missing))
               (then (local.set $out (call $current-output-port
                                           (global.get $missing)))))
           (if (i32.eqz (call $output-port-like?/i32 (local.get $out)))
               (then (call $raise-argument-error1
                           (global.get $symbol:write-bytes)
                           (global.get $string:output-port?)
                           (local.get $out))
                     (unreachable)))
           (if (i32.eqz (ref.test (ref $OutputPort) (local.get $out)))
               (then (local.set $out (call $prop-output-port-target
                                           (local.get $out)))))
           (if (i32.eqz (ref.test (ref $OutputPort) (local.get $out)))
               (then (local.set $discard? (i32.const 1))))
           ;; --- Decode optional start index ---
           (if (ref.eq (local.get $start) (global.get $missing))
               (then (local.set $from (i32.const 0)))
               (else
                (local.set $from
                           (call $exact-nonnegative-integer->i32/saturating
                                 (local.get $start)))
                (if (i32.lt_s (local.get $from) (i32.const 0))
                    (then (call $raise-port-bytes-index-expected
                                (global.get $symbol:write-bytes)
                                (local.get $start))))))
           ;; --- Decode optional end index ---
           (if (ref.eq (local.get $end) (global.get $missing))
               (then (local.set $to (local.get $len)))
               (else
                (local.set $to
                           (call $exact-nonnegative-integer->i32/saturating
                                 (local.get $end)))
                (if (i32.lt_s (local.get $to) (i32.const 0))
                    (then (call $raise-port-bytes-index-expected
                                (global.get $symbol:write-bytes)
                                (local.get $end))))))
           ;; --- Bounds checks ---
           (if (i32.gt_u (local.get $from) (local.get $to))
               (then (call $raise-bad-port-bytes-range
                           (global.get $symbol:write-bytes)
                           (local.get $bstr)
                           (local.get $from)
                           (local.get $to))
                     (unreachable)))
           (if (i32.gt_u (local.get $to) (local.get $len))
               (then (call $raise-bad-port-bytes-range
                           (global.get $symbol:write-bytes)
                           (local.get $bstr)
                           (local.get $from)
                           (local.get $to))
                     (unreachable)))

           (local.set $count (i32.sub (local.get $to) (local.get $from)))
           (if (local.get $discard?)
               (then (return (ref.i31 (i32.shl (local.get $count) (i32.const 1))))))
           (if (ref.test (ref $CustomOutputPort) (local.get $out))
               (then
                (return_call $write-bytes/custom
                             (ref.cast (ref $CustomOutputPort) (local.get $out))
                             (local.get $bs)
                             (local.get $from)
                             (local.get $to))))
           (if (i32.eqz (ref.test (ref $OutputPort) (local.get $out)))
               (then (call $raise-check-string-port
                           (global.get $symbol:write-bytes)
                           (local.get $out))
                     (unreachable)))
           ;; --- Write the requested slice byte by byte ---
           (local.set $i (i32.const 0))
           (block $done
                  (loop $loop
                        (br_if $done (i32.ge_u (local.get $i) (local.get $count)))
                        (local.set $idx (i32.add (local.get $from) (local.get $i)))
                        (local.set $byte (call $i8array-ref (local.get $arr) (local.get $idx)))
                        (local.set $res
                                   (call $write-byte
                                         (ref.i31 (i32.shl (local.get $byte) (i32.const 1)))
                                         (local.get $out)))
                        (if (ref.eq (local.get $res) (global.get $false))
                            (then (return (global.get $false))))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br $loop)))
           ;; --- Report number of bytes written ---
           (ref.i31 (i32.shl (local.get $count) (i32.const 1))))


     (func $raise-bad-string-range
           (param $who  (ref eq))
           (param $x    (ref eq))
           (param $from i32)
           (param $to   i32)

           (local $s       (ref $String))
           (local $len     i32)
           (local $out     (ref $GrowableArray))
           (local $message (ref $String))

           (local.set $s   (ref.cast (ref $String) (local.get $x)))
           (local.set $len
                      (call $i32array-length
                            (struct.get $String $codepoints (local.get $s))))

           (if (i32.gt_u (local.get $from) (local.get $len))
               (then
                (local.set $out
                           (call $make-growable-array (i32.const 9)))
                (call $growable-array-add! (local.get $out)
                      (call $format/display (local.get $who)))
                (call $growable-array-add! (local.get $out)
                      (global.get $string:substring:starting-index-out-of-range))
                (call $growable-array-add! (local.get $out)
                      (global.get $string:substring:starting-index))
                (call $growable-array-add! (local.get $out)
                      (call $format/display
                            (call $integer-from-i64
                                  (i64.extend_i32_u (local.get $from)))))
                (call $growable-array-add! (local.get $out)
                      (global.get $string:string-ref:valid-range))
                (call $growable-array-add! (local.get $out)
                      (call $format/display
                            (call $integer-from-i64
                                  (i64.extend_i32_u (local.get $len)))))
                (call $growable-array-add! (local.get $out)
                      (global.get $string:string-ref:valid-range-close))
                (call $growable-array-add! (local.get $out)
                      (global.get $string:string-utf8:byte-string))
                (call $growable-array-add! (local.get $out)
                      (call $format/error-value->string (local.get $x)))
                (local.set $message
                           (call $growable-array-of-strings->string (local.get $out)))
                (call $raise
                      (call $exn:fail:contract/make
                            (local.get $message)
                            (call $current-continuation-marks (global.get $missing)))
                      (global.get $true))
                (unreachable)))

           (if (i32.gt_u (local.get $to) (local.get $len))
               (then
                (local.set $out
                           (call $make-growable-array (i32.const 9)))
                (call $growable-array-add! (local.get $out)
                      (call $format/display (local.get $who)))
                (call $growable-array-add! (local.get $out)
                      (global.get $string:substring:ending-index-out-of-range))
                (call $growable-array-add! (local.get $out)
                      (global.get $string:substring:ending-index))
                (call $growable-array-add! (local.get $out)
                      (call $format/display
                            (call $integer-from-i64
                                  (i64.extend_i32_u (local.get $to)))))
                (call $growable-array-add! (local.get $out)
                      (global.get $string:string-ref:valid-range))
                (call $growable-array-add! (local.get $out)
                      (call $format/display
                            (call $integer-from-i64
                                  (i64.extend_i32_u (local.get $len)))))
                (call $growable-array-add! (local.get $out)
                      (global.get $string:string-ref:valid-range-close))
                (call $growable-array-add! (local.get $out)
                      (global.get $string:string-utf8:byte-string))
                (call $growable-array-add! (local.get $out)
                      (call $format/error-value->string (local.get $x)))
                (local.set $message
                           (call $growable-array-of-strings->string (local.get $out)))
                (call $raise
                      (call $exn:fail:contract/make
                            (local.get $message)
                            (call $current-continuation-marks (global.get $missing)))
                      (global.get $true))
                (unreachable)))

           (local.set $out
                      (call $make-growable-array (i32.const 11)))
           (call $growable-array-add! (local.get $out)
                 (call $format/display (local.get $who)))
           (call $growable-array-add! (local.get $out)
                 (global.get $string:substring:ending-index-before-start))
           (call $growable-array-add! (local.get $out)
                 (global.get $string:substring:ending-index))
           (call $growable-array-add! (local.get $out)
                 (call $format/display
                       (call $integer-from-i64
                             (i64.extend_i32_u (local.get $to)))))
           (call $growable-array-add! (local.get $out)
                 (global.get $string:substring:starting-index))
           (call $growable-array-add! (local.get $out)
                 (call $format/display
                       (call $integer-from-i64
                             (i64.extend_i32_u (local.get $from)))))
           (call $growable-array-add! (local.get $out)
                 (global.get $string:string-ref:valid-range))
           (call $growable-array-add! (local.get $out)
                 (call $format/display
                       (call $integer-from-i64
                             (i64.extend_i32_u (local.get $len)))))
           (call $growable-array-add! (local.get $out)
                 (global.get $string:string-ref:valid-range-close))
           (call $growable-array-add! (local.get $out)
                 (global.get $string:string-utf8:byte-string))
           (call $growable-array-add! (local.get $out)
                 (call $format/error-value->string (local.get $x)))
           (local.set $message
                      (call $growable-array-of-strings->string (local.get $out)))
           (call $raise
                 (call $exn:fail:contract/make
                       (local.get $message)
                       (call $current-continuation-marks (global.get $missing)))
                 (global.get $true))
           (unreachable))

     ;; Like Racket's write-string, but currently only string ports are supported
     ;; as output destinations. The port argument must be provided explicitly.
     (func $write-string (type $Prim14)
           (param $str   (ref eq)) ;; string?
           (param $out   (ref eq)) ;; output-port?               (optional, default = (current-output-port))
           (param $start (ref eq)) ;; exact-nonnegative-integer? (optional, default = 0)
           (param $end   (ref eq)) ;; exact-nonnegative-integer? (optional, default = (string-length str))
           (result       (ref eq))

           (local $s     (ref $String))
           (local $arr   (ref $I32Array))
           (local $len   i32)
           (local $from  i32)
           (local $to    i32)
           (local $count i32)
           (local $bytes (ref $Bytes))
           (local $res   (ref eq))

           ;; --- Validate string argument ---
           (if (i32.eqz (ref.test (ref $String) (local.get $str)))
               (then (call $raise-argument-error1
                           (global.get $symbol:write-string)
                           (global.get $string:string?)
                           (local.get $str))
                     (unreachable)))
           (local.set $s   (ref.cast (ref $String) (local.get $str)))
           (local.set $arr (struct.get $String $codepoints (local.get $s)))
           (local.set $len (call $i32array-length (local.get $arr)))
           ;; --- Determine output port ---
           (if (ref.eq (local.get $out) (global.get $missing))
               (then (local.set $out (call $current-output-port
                                           (global.get $missing)))))
           (if (i32.eqz (call $output-port-like?/i32 (local.get $out)))
               (then (call $raise-argument-error1
                           (global.get $symbol:write-string)
                           (global.get $string:output-port?)
                           (local.get $out))
                     (unreachable)))
           ;; --- Decode optional start index ---
           (if (ref.eq (local.get $start) (global.get $missing))
               (then (local.set $from (i32.const 0)))
               (else
                (local.set $from
                           (call $exact-nonnegative-integer->i32/saturating
                                 (local.get $start)))
                (if (i32.lt_s (local.get $from) (i32.const 0))
                    (then (call $raise-port-range-index-expected
                                (global.get $symbol:write-string)
                                (local.get $start))))))
           ;; --- Decode optional end index ---
           (if (ref.eq (local.get $end) (global.get $missing))
               (then (local.set $to (local.get $len)))
               (else
                (local.set $to
                           (call $exact-nonnegative-integer->i32/saturating
                                 (local.get $end)))
                (if (i32.lt_s (local.get $to) (i32.const 0))
                    (then (call $raise-port-range-index-expected
                                (global.get $symbol:write-string)
                                (local.get $end))))))
           ;; --- Bounds checks ---
           (if (i32.gt_u (local.get $from) (local.get $to))
               (then (call $raise-bad-string-range
                           (global.get $symbol:write-string)
                           (local.get $str)
                           (local.get $from)
                           (local.get $to))
                     (unreachable)))
           (if (i32.gt_u (local.get $to) (local.get $len))
               (then (call $raise-bad-string-range
                           (global.get $symbol:write-string)
                           (local.get $str)
                           (local.get $from)
                           (local.get $to))
                     (unreachable)))

           (local.set $count (i32.sub (local.get $to) (local.get $from)))
           ;; --- Convert requested slice to bytes ---
           (local.set $bytes
                      (ref.cast (ref $Bytes)
                                (call $string->bytes/utf-8
                                      (local.get $str)
                                      (global.get $false)
                                      (ref.i31 (i32.shl (local.get $from) (i32.const 1)))
                                      (ref.i31 (i32.shl (local.get $to)   (i32.const 1))))))
           ;; --- Delegate to write-bytes ---
           (local.set $res
                      (call $write-bytes
                            (local.get $bytes)
                            (local.get $out)
                            (global.get $missing)
                            (global.get $missing)))
           (if (ref.eq (local.get $res) (global.get $false))
               (then (return (global.get $false))))
           ;; --- Report number of characters written ---
           (ref.i31 (i32.shl (local.get $count) (i32.const 1)))))))

;; port-all-runtime-helpers : (listof core-runtime-helper?)
;;   Enumerate port helpers used by the monolithic runtime.
(define port-all-runtime-helpers
  (append port-predicate-runtime-only-helpers
          port-state-runtime-only-helpers
          port-location-runtime-only-helpers
          port-byte-string-runtime-only-helpers
          port-output-string-runtime-only-helpers
          port-custom-constructor-runtime-only-helpers
          port-capability-runtime-only-helpers
          port-custom-input-byte-runtime-only-helpers
          port-byte-char-input-runtime-only-helpers
          port-read-special-wrapper-runtime-only-helpers
          port-read-bytes-runtime-only-helpers
          port-read-string-runtime-only-helpers
          port-readiness-runtime-only-helpers
          port-read-line-runtime-only-helpers
          port-peek-bytes-runtime-only-helpers
          port-peek-string-runtime-only-helpers
          port-peek-single-runtime-only-helpers
          port-peek-wrapper-runtime-only-helpers
          port-byte-char-output-runtime-only-helpers
          port-byte-string-output-runtime-only-helpers))

;; port-core-runtime-helpers : (listof core-runtime-helper?)
;;   Enumerate port helpers available in the standalone core runtime.
(define port-core-runtime-helpers
  (select-runtime-helpers port-all-runtime-helpers
                          port-core-runtime-helper-names))

;; port-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate port helpers that still depend on monolithic runtime definitions.
(define port-runtime-only-helpers
  (remove-runtime-helpers port-all-runtime-helpers
                          port-core-runtime-helper-names))

(module+ test
  (require rackunit)

  ;; form-name : datum? -> symbol?
  ;;   Return the function name from a rendered helper form.
  (define (form-name form)
    (second form))

  (define port-helper-index
    (make-core-runtime-helper-index
     (append port-core-runtime-helpers
             port-runtime-only-helpers)))

  (check-equal? port-current-global-names
                '($current-input-port-value
                  $current-output-port-value
                  $current-error-port-value))

  (check-equal? port-primitive-global-names
                '($prim:make-input-port
                  $prim:make-output-port
                  $prim:peek-bytes-avail!
                  $prim:peek-bytes-avail!*))

  (check-equal?
   (map core-runtime-helper-name port-core-runtime-helpers)
   port-core-runtime-helper-names)

  (check-equal?
   (map form-name (port-predicate-runtime-helper-forms port-helper-index))
   port-predicate-runtime-only-helper-names)

  (check-equal?
   (map form-name (port-output-predicate-runtime-helper-forms port-helper-index))
   port-output-predicate-runtime-only-helper-names)

  (check-equal?
   (map form-name (port-state-runtime-helper-forms port-helper-index))
   port-state-runtime-only-helper-names)

  (check-equal?
   (map form-name (port-location-runtime-helper-forms port-helper-index))
   port-location-runtime-only-helper-names)

  (check-equal?
   (map form-name (port-string-constructor-runtime-helper-forms port-helper-index))
   port-string-constructor-runtime-only-helper-names)

  (check-equal?
   (map form-name (port-output-string-runtime-helper-forms port-helper-index))
   port-output-string-runtime-only-helper-names)

  (check-equal?
   (map form-name (port-custom-constructor-runtime-helper-forms port-helper-index))
   port-custom-constructor-runtime-only-helper-names)

  (check-equal?
   (map form-name (port-call-with-output-string-runtime-helper-forms port-helper-index))
   port-call-with-output-string-runtime-only-helper-names)

  (check-equal?
   (map form-name (port-input-runtime-helper-forms port-helper-index))
   port-input-runtime-only-helper-names)

  (check-equal?
   (map form-name (port-output-runtime-helper-forms port-helper-index))
   port-output-runtime-only-helper-names))
