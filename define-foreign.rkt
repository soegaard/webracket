#lang racket/base
(provide ffi-file->foreigns
         foreign->import
         foreign->primitive
         foreigns->primitive-names)
;;;
;;; Foreign Function Interface
;;;

;; The actual declarations of foreign functions are placed
;; in external files, such as "dom.ffi" (for the functions
;; that manipulate the dom).
;;
;; An .ffi file contains declarations of the form:

;;   (define-foreign racket-name
;;     #:module "document"
;;     #:name   "create-text-node"
;;     (-> (string) (extern)))           ; params → result
;;
;;   (define-foreign set-attribute!
;;     #:module "document"
;;     #:name   "set-attribute!"
;;     (-> (extern string string) void))

;; Calling 
;;     (ffi-file->foreigns "dom.ffi")
;; will validate all declarations, and return a list
;; of `foreign` structures.

#;(struct foreign (racket-name    ; the Racket primitive, we are defining
                   module-name    ; name of imported module
                   host-name      ; name in the imported module
                   argument-types ; list of argument types
                   result-types   ; list of result types
                   ) #:transparent)

;; The main interface for the compiler is:

(define (ffi-file->foreigns file-path)
  (define forms (read-forms-from-file file-path))
  (for-each validate-form forms)
  (for/list ([form forms])
    (syntax-case* form (define-foreign) literal=?
      [(define-foreign racket-name #:module mod #:name name type)
       (foreign (syntax-e #'racket-name)
                (syntax-e #'mod)
                (syntax-e #'name)
                (argument-types #'type)
                (result-types #'type))])))

;; The types are as follows:

;; Type language (initial)
;;   string        — Racket string, marshaled as FASL → linear memory.
;;   string/symbol - accepts string or symbol as argument - a string is sent via fasl
;;   value         — arbitrary Racket value, marshaled as FASL → linear memory.
;;   extern        — $External wrapper around externref.
;;   i32           — fixnum → i32 ;   signed
;;   u32           — fixnum → u32 ; unsigned
;;   f64           — flonum → f64 (checked)

;; Using `void` instead of (<result-type> ...) is equivalent to ().



;; The following are the parts needed for `ffi-file->foreigns`.

(require (for-syntax racket/base)
         "structs.rkt")


; read-forms-from-file : file-path -> list-of-syntax
;   read all forms 
(define (read-forms-from-file file-path)
  (define source-name file-path)
  (with-input-from-file file-path
    (λ ()
      (for/list ([form (in-port (λ (in) (read-syntax source-name in)))])
        form))))


;; We use `syntax-case*` with the comparison function `literal=?`
;; to avoid problems with bindings for `string`, `void` etc.

(define (literal=? x y)
  (eq? (syntax-e x) (syntax-e y)))

(define (validate-base-argument-type who form type)
  (syntax-case* type (string string/symbol value extern i32 u32 f64) literal=?
    [string        #t]
    [string/symbol #t]
    [value         #t]
    [extern        #t]
    [i32           #t]
    [u32           #t]
    [f64           #t]
    [_
     (raise-syntax-error who "expected an argument type, got: "
                         form type)]))

(define (validate-base-result-type who form type)
  (syntax-case* type (string value extern i32 u32 f64) literal=?
    [string #t]
    [value  #t]
    [extern #t]
    [i32    #t]
    [u32    #t]
    [f64    #t]
    [void   #t]  ; synonym for ()
    [_
     (raise-syntax-error who "expected an result type, got: "
                         form type)]))

(define (validate-argument-types who form types)
  (syntax-case types ()
    [(arg-type ...) (for ([arg-type (syntax->list #'(arg-type ...))])
                      (validate-base-argument-type who form arg-type))]
    [_ (raise-syntax-error who "expected a list of argument types, got: "
                           form types)]))

(define (validate-result-types who form types)
  (syntax-case types ()
    [(arg-type ...) (for ([arg-type (syntax->list #'(arg-type ...))])
                      (validate-base-result-type who form arg-type))]
    [_ (raise-syntax-error who "expected a list of result types, got: "
                           form types)]))
                                                

(define (validate-form form)
  (define who 'define-foreign)
  (syntax-case* form (define-foreign -> void) literal=?
    [(define-foreign racket-name #:module mod #:name name type)
     (begin
       (unless (identifier? #'racket-name)
         (raise-syntax-error who
                             "expected a symbol for the racket name"
                             form #'racket-name))
       
       (unless (string? (syntax-e #'mod))
         (raise-syntax-error who
                             "expected a string for the module name"
                             form #'mod))
       (unless (string? (syntax-e #'name))
         (raise-syntax-error who
                             "expected a string for the host name"
                             form #'name))
       (syntax-case #'type (->)
         [(-> arg-types void) ; void is the same as ()
          (let ()
            (validate-argument-types who form #'arg-types))]
         [(-> arg-types result-types)
          (let ()
            (validate-argument-types who form #'arg-types)
            (validate-result-types   who form #'result-types))]
         [_
          (raise-syntax-error who
                              (string-append
                               "expected a type specification of the form "
                               "(-> (<argument-type> ...) (<result-type> ...))")
                              form #'type)])       
       #t)]
    [(define-foreign . _)
     (raise-syntax-error who
                         (string-append
                          "malformed define-foreign, expected:\n"
                          "(define-foreign #:module <module> #:name <name>)\n"
                          "where <module> and <name> are literal strings")
                         form)]
    [_
     (raise-syntax-error who
                         "expected an `define-foreign` form\n")]))

;; Note: After validation we can assume the form has the correct form.

(define (argument-types type)
  (syntax-case* type (define-foreign -> void) literal=?
    [(-> arg-types result-types)
     (syntax->datum #'arg-types)]))

(define (result-types type)
  (syntax-case* type (define-foreign -> void) literal=?
    [(-> arg-types result-types)
     (let ()
       (define out (syntax->datum #'result-types))
       (if (eq? out 'void)
           '()
           out))]))

;;;
;;; Generate Code for the Web Assembly runtime
;;;

(require racket/match
         racket/format)

;; Import Declarations

(define (foreign->import f)
  (match f
    [(foreign racket-name module-name host-name argument-types result-types)
     (define $racket-name          (string->symbol (~a "$" racket-name)))
     (define $racket-name/imported (string->symbol (~a $racket-name "/imported")))
     `(func ,$racket-name/imported
            (import ,module-name ,host-name)
            ,@(argument-types->wasm-import-parameters argument-types)
            ,@(result-types->wasm-import-results      result-types))]))

(define (argument-types->wasm-import-parameters argument-types)
  (for/list ([t argument-types])
    `(param ,(argument-type->wasm-import-parameter t))))

(define (argument-type->wasm-import-parameter type)
  (case type
    [(string)        'i32]         ; index into linear memory
    [(string/symbol) 'i32]         ; index into linear memory
    [(value)         'i32]         ; index into linear memory
    [(extern)        'externref]
    [(i32)           'i32]
    [(u32)           'i32]
    [(f64)           'f64]
    [else
     (error 'argument-type->wasm-import-parameter
            "expected type, got: ~a"
            type)]))


(define (result-types->wasm-import-results result-types)
  (map result-type->wasm-import-result result-types))

(define (result-type->wasm-import-result type)
  (define t (case type
              [(string) 'i32]         ; index into linear memory
              [(value)  'i32]         ; index into linear memory
              [(extern) 'externref]
              [(i32)    'i32]
              [(u32)    'i32]
              [(f64)    'f64]
              [else
               (error 'result-type->wasm-import-result
                      "expected type, got: ~a"
                      type)]))
  `(result ,t))


;; Primitive wrapper for the imported function

(define (argument-type->wasm-primtive-expected type)
  (case type
    [(string)        '(ref $String)]    ; index into linear memory
    [(string/symbol) '(ref eq)]         ; index into linear memory
    [(value)         '(ref eq)]         ; index into linear memory
    [(extern)        '(ref $External)]
    [(i32)           '(ref i31)]
    [(u32)           '(ref i31)]
    [(f64)           '(ref $Flonum)]
    [else
     (error argument-type->wasm-primtive-expected
            "expected type, got: ~a"
            type)]))


(define (foreign->primitive f)
  (match f
    [(foreign racket-name module-name host-name argument-types result-types)
     (define $racket-name          (string->symbol (~a "$" racket-name)))
     (define $racket-name/imported (string->symbol (~a $racket-name "/imported")))

     (define (fasl-needed? t) (member t '(string string/symbol value)))
     (define fasl-needed-for-arguments? (ormap fasl-needed? argument-types))
     (define fasl-needed-for-results?   (ormap fasl-needed? result-types))

     (define local-start-index (length argument-types))
     (define (param-index  argument-index) argument-index)
     (define (local-index  argument-index) (+      local-start-index  argument-index))
     (define (import-index argument-index) (+ (* 2 local-start-index) argument-index))
     
     `(func ,$racket-name
            ;; Arguments
            ,@(for/list ([t argument-types])  `(param (ref eq)))

            ;; Result
            (result (ref eq))              ;; todo - just one return value for now

            ;; Locals
            
            ;;   - each argument will be type checked and converted
            ;;     they are stoed in (local-index i)
            ,@(for/list ([t argument-types])
                (define expected (argument-type->wasm-primtive-expected t))
                `(local ,expected))
            ;;   - the types needed for the /imported function
            ;;     they are stored in import-index
            ,@(for/list ([t argument-types])
                (define converted (argument-type->wasm-import-parameter t))
                `(local ,converted))

            ;; The indices of the above locals are fixed, so named locals
            ;; need to come below.
            
            ;;  - if fasl is needed for arguments, produce helper locals here
            ,@(if fasl-needed-for-arguments?
                  `((local $fasl-index i32)           ;; current write ptr in linear memory
                    (local $bs         (ref eq))      ;; temp for returned bytes-as-(ref eq)
                    (local $b          (ref $Bytes))  ;; casted bytes
                    (local $len        i32))          ;; length just copied
                  '())
            ;; - a local to hold the results (only if one is expected)
            ,@(match result-types
                [(list t)
                 `((local $results ,(argument-type->wasm-import-parameter t)))]
                [_ '()])
            
            ;; 0. Type check - fail early
            ,@(for/list ([t argument-types] [i (in-naturals)])
                (match t
                  ['string/symbol
                   `(if (i32.eqz (i32.or (ref.test (ref $String) (local.get ,(param-index i)))
                                         (ref.test (ref $Symbol) (local.get ,(param-index i)))))
                        ; TODO  Call a more specific error function that includes the type.
                        (then (call $raise-unexpected-argument (local.get ,(param-index i)))
                              (unreachable)))]
                  [_
                   (define expected (argument-type->wasm-primtive-expected t))
                   `(if (i32.eqz (ref.test ,expected (local.get ,(param-index i))))
                        ; TODO  Call a more specific error function that includes the type.
                        (then (call $raise-unexpected-argument (local.get ,(param-index i)))
                              (unreachable)))]))

            ;; 1. Extract the values from the parameters and store them in the locals.
            ;;    I.e. extract (param-index i) and store into (local-index i)            
            ,@(for/list ([t argument-types] [i (in-naturals)])
                (match t
                  ['i32 ; signed
                   `(local.set ,(local-index i)
                               (ref.cast (ref i31) (local.get ,(param-index i))))]
                  ['u32 ; unsigned
                   `(local.set ,(local-index i)
                               (ref.cast (ref i31) (local.get ,(param-index i))))]
                  ['f64
                   `(local.set ,(local-index i)
                               (ref.cast (ref $Flonum)
                                                     (local.get ,(param-index i))))]
                  ['string
                   `(local.set ,(local-index i)
                               (ref.cast (ref $String)
                                         (local.get ,(param-index i))))]
                  ['string/symbol
                   ; todo - if the argument is a symbol, convert it to a string first
                   `(local.set ,(local-index i)
                               (if (ref.test (ref $Symbol) (local.get ,(param-index i)))
                                   (ref.cast (ref $String) (call $symbol->string
                                                                 (local.get ,(param-index i))))
                                   (ref.cast (ref $String) (local.get ,(param-index i)))))]
                  ['value
                   `(local.set ,(local-index i) (local.get ,(param-index i)))]
                  ['extern
                   `(local.set ,(local-index i) (ref.cast (ref $External) (local.get ,(param-index i))))]
                  [_
                   `(local.set ,(local-index i) (local.get ,(param-index i)))]))
            
            ;; 2. Convert the expected values into types for the /imported function
            ;;    I.e. convert (local-index i) and store into (import-index i)
            ;;    The types `value` and `string` converts into indiced returned by the fasl.
            ,@(if fasl-needed-for-arguments?
                  `((local.set $fasl-index (i32.const 0))) ;; start of FASL area
                  '())
            
            ,@(for/list ([t argument-types] [i (in-naturals)])
                (match t
                  ['i32
                   ;; pass-through primitives
                   `(local.set ,(import-index i)
                               (i32.shr_s (i31.get_s (local.get ,(local-index i)))
                                          (i32.const 1)))]
                  ['u32
                   ;; pass-through primitives
                   `(local.set ,(import-index i)
                               (i32.shr_u (i31.get_u (local.get ,(local-index i)))
                                          (i32.const 1)))]
                  ['f64
                   `(local.set ,(import-index i)
                               (struct.get $Flonum $v (local.get ,(local-index i))))]
                  [(or 'string 'string/symbol)
                   ;; 1) FASL-encode directly to a bytes object (port = #f)
                   ;; 2) Copy bytes to linear memory at $fasl-index
                   ;; 3) Store old $fasl-index as the import argument
                   ;; 4) Advance $fasl-index by the length
                   `(block
                     (local.set ,(import-index i) (local.get $fasl-index))
                     (local.set $bs  (call $s-exp->fasl (local.get ,(local-index i))
                                           (global.get $false)))
                     (local.set $b   (ref.cast (ref $Bytes) (local.get $bs)))
                     (local.set $len (call $copy-bytes-to-memory (local.get $b)
                                           (local.get $fasl-index)))
                     (local.set $fasl-index
                                (i32.add (local.get $fasl-index) (local.get $len))))]
                  ['value
                   `(block
                     (local.set ,(import-index i) (local.get $fasl-index))
                     (local.set $bs  (call $s-exp->fasl (local.get ,(local-index i))
                                           (global.get $false)))
                     (local.set $b   (ref.cast (ref $Bytes) (local.get $bs)))
                     (local.set $len (call $copy-bytes-to-memory (local.get $b)
                                           (local.get $fasl-index)))
                     (local.set $fasl-index
                                (i32.add (local.get $fasl-index) (local.get $len))))]
                  ['extern
                   `(local.set ,(import-index i)
                               (struct.get $External $v (local.get ,(local-index i))))]                   
                  [_
                   `(local.set ,(import-index i) (local.get ,(local-index i)))]))
            
            ;; 3) call the host function
            ,(case (length result-types)
               [(0)   `(call ,$racket-name/imported
                             ,@(for/list ([i (in-range (length argument-types))])
                                 `(local.get ,(import-index i))))]
               [else `(local.set $results
                                 (call ,$racket-name/imported
                                       ,@(for/list ([i (in-range (length argument-types))])
                                           `(local.get ,(import-index i)))))])
                     
            ;; 4) Convert the result
            ,(match result-types
               [(list 'i32) ; to fixnum
                `(return
                  (ref.i31 (i32.shl (local.get $results) (i32.const 1))))]
               [(list 'u32) ; to fixnum
                `(return
                  (ref.i31 (i32.shl (local.get $results) (i32.const 1))))]
               [(list 'f64) ; to flonum
                `(return
                  (struct.new $Flonum (i32.const 0) (local.get $results)))]
               [(list 'string) ; a string is returned as an index into linear memory
                `(return
                  (call $linear-memory->string (local.get $results)))]
               [(list 'value) ; a value is returned as an index into linear memory
                `(return
                  (call $linear-memory->value (local.get $results)))]
               [(list 'extern) ; wrap externref in our External box
                `(return
                  (struct.new $External
                              (i32.const 0)              ;; $hash = 0 (lazy)
                              (local.get $results)))]    ;; externref payload
               [_
                `(return (global.get $void))]))]))

;;;
;;;
;;;

; foreigns->primitive-names : list of `foreign` -> list of symbols
(define (foreigns->primitive-names fs)
  (map foreign-racket-name fs))

;;;
;;; TEST
;;;

;; (require racket/pretty)

;; (define fs (ffi-file->foreigns "dom.ffi"))
;; 'IMPORTS
;; (pretty-print (map foreign->import fs))
;; 'PRIMITIVES
;; (pretty-print (map foreign->primitive fs))
