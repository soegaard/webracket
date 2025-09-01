#lang racket
(provide generate-runtime)

(require "wasm-data.rkt"
         "wasm-utils.rkt"
         "immediates.rkt"
         "priminfo.rkt"
         "parameters.rkt")

; See the end of `generate-code` in `compiler.rkt` for an
; explanation of the arguments.
(define (generate-runtime
           dls tms entry-body result
           ; program specific
           top-vars
           top-level-variable-declarations
           entry-locals           
           ; general information
           primitives       ; list of symbols
           string-constants ; (list (list name string) ...)
           bytes-constants  ; (list (list name bytes)  ...)
           symbol-constants ; (list (list name symbol) ....)
           )
  (let* () ; an wasm identifier
    (let* ()
    
    ;; Variable definitions on the top-level ought to go in a namespace,
    ;; but for now we use a global WebAssembly variable.


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
        [_                  (error
                             'arity->internal-representation "got: ~a" a)]))

    (define (min-arity a)
      (match a
        [(arity-at-least n) n]
        [(? number? n)      n]
        [(? list? l)        (apply min (map min-arity l))]
        [_                  (error 'min-arity "got: ~a" a)]))

    ; If a primitive is handled in the inliner, but hasn't been
    ; implemented her in `runtime-wasm.rkt` then put the symbol here.
    (define todo-handle-later '())

    (define (initialize-primitives-as-globals)
      (for/list ([pr (sort (remove* todo-handle-later primitives) symbol<?)]
                 #:do [(define desc (primitive->description pr))]
                 #:when desc)
        (define ar (primitive-description-arity desc))
        `(global.set ,($ (prim: pr))
                     (struct.new $PrimitiveProcedure
                      ; for $Procedure
                      (i32.const 0)                               ; hash
                      ,(Imm #f)                                   ; name
                      ,(Imm (arity->internal-representation ar))  ; arity
                      (global.get $the-racket/primitive-realm)    ; realm
                      (ref.func $primitive-invoke)
                      (ref.func ,($ pr))
                      ,(Imm #f
                            #;(arity->internal-representation
                               (primitive-description-result-arity desc)))))))

    ;; String constants used in the runtime
    ;;  `string-constants holds the constants passed by `generate-code` in `compiler.rkt`
    (define runtime-string-constants string-constants)
    (define (add-runtime-string-constant name string)
      (set! runtime-string-constants
            (cons (list name string) runtime-string-constants)))
    (define (declare-runtime-string-constants)
      (append*
       (for/list ([ns (reverse runtime-string-constants)])
         (define name              (first ns))
         (define string            (second ns))
         (define $string-data:name (string->symbol (~a "$" "string-data:"  name)))
         (define $string:name      (string->symbol (~a "$" "string:"       name)))
         (list `(data   ,$string-data:name ,(wasm-data (string->bytes/utf-8 string)))
               `(global ,$string:name (mut (ref eq)) ,(Imm #f))))))
    (define (initialize-runtime-string-constants)
      (for/list ([ns (reverse runtime-string-constants)])
        (define name              (first ns))
        (define string            (second ns))
        (define $string-data:name (string->symbol (~a "$" "string-data:"  name)))
        (define $string:name      (string->symbol (~a "$" "string:"       name)))
        (define n                 (bytes-length (string->bytes/utf-8 string)))
        `(global.set ,$string:name
                     (call $i8array->string
                           (array.new_data $I8Array ,$string-data:name
                                           (i32.const 0) (i32.const ,n))))))

    ;; Bytes constants used in the runtime
    ;;  `bytes-constants holds the constants passed by `generate-code` in `compiler.rkt`
    (define runtime-bytes-constants bytes-constants)
    (define (add-runtime-bytes-constant name bytes)
      (set! runtime-bytes-constants
            (cons (list name bytes) runtime-bytes-constants)))
    (define (declare-runtime-bytes-constants)
      (append*
       (for/list ([ns (reverse runtime-bytes-constants)])
         (define name             (first ns))
         (define bytes            (second ns))
         (define $bytes-data:name (string->symbol (~a "$" "bytes-data:" name)))
         (define $bytes:name      (string->symbol (~a "$" "bytes:"  name)))
         (list `(data ,$bytes-data:name ,(wasm-data bytes))
               `(global ,$bytes:name (mut (ref eq)) ,(Imm #f))))))
    (define (initialize-runtime-bytes-constants)
      (for/list ([ns (reverse runtime-bytes-constants)])
        (define name             (first ns))
        (define bytes            (second ns))
        (define $bytes-data:name (string->symbol (~a "$" "bytes-data:" name)))
        (define $bytes:name      (string->symbol (~a "$" "bytes:"      name)))
        (define n                (bytes-length bytes))
        `(global.set ,$bytes:name
                     (call $i8array->bytes
                           (array.new_data $I8Array ,$bytes-data:name
                                           (i32.const 0) (i32.const ,n))))))
    
    ;; Symbol constants used in the runtime
    ;;  `symbol-constants holds the constants passed by `generate-code`
    ;; in `compiler.rkt`
    (define runtime-symbol-constants symbol-constants)
    (define runtime-symbols-ht (make-hasheq))
    (define (add-runtime-symbol-constant symbol)
      (cond
        [(hash-ref runtime-symbols-ht symbol #f)
         => values]
        [else
         (define name symbol)
         (set! runtime-symbol-constants
               (cons (list name symbol) runtime-symbol-constants))]))
    (define (declare-runtime-symbol-constants)
      (append*
       (for/list ([ns (reverse runtime-symbol-constants)])
         (define name                (first ns))
         (define symbol              (second ns))
         (define string              (symbol->string symbol))
         (define $symbol-data:name   (string->symbol (~a "$" "symbol-data:"   name)))
         (define $symbol:name        (string->symbol (~a "$" "symbol:"        name)))
         (list `(data   ,$symbol-data:name                  ,string)
               `(global ,$symbol:name (mut (ref eq))        ,(Imm #f))))))
    
    (define (initialize-runtime-symbol-constants)
      (for/list ([ns (reverse runtime-symbol-constants)])
        (define name         (first ns))
        (define symbol       (second ns))
        (define $symbol-data:name   (string->symbol (~a "$" "symbol-data:"   name)))
        (define $symbol:name        (string->symbol (~a "$" "symbol:"        name)))
        (define n                   (string-length (~a symbol)))
        `(global.set ,$symbol:name
                     (call $string->symbol
                           (call $i8array->string
                                 (array.new_data $I8Array ,$symbol-data:name
                                                 (i32.const 0) (i32.const ,n)))))))

    ;; String and symbol constants used in the runtime
    (add-runtime-symbol-constant 'racket)
    (add-runtime-symbol-constant 'racket/primitive)
    
    (add-runtime-string-constant 'hash-variable-reference   "#<variable-reference>")
    (add-runtime-string-constant 'box-prefix                "#&")
    (add-runtime-string-constant 'bytes-prefix              "#\"")
    (add-runtime-string-constant 'backslash                 "\\")
    (add-runtime-string-constant 'backslash-x               "\\x")
    (add-runtime-string-constant 'double-quote              "\"")
    (add-runtime-string-constant 'hash-t                    "#t")
    (add-runtime-string-constant 'hash-f                    "#f")
    (add-runtime-string-constant 'null                      "()")
    (add-runtime-string-constant 'void                      "#<void>")
    (add-runtime-string-constant 'undefined                 "#<undefined>")
    (add-runtime-string-constant 'unspecified               "#<unspecified>")
    (add-runtime-string-constant 'missing                   "#<missing>")
    (add-runtime-string-constant 'closure                   "#<closure>")
    (add-runtime-string-constant 'external                  "#<external>")
    (add-runtime-string-constant 'external-null             "#<external-null>")
    (add-runtime-string-constant 'namespace                 "#<namespace>")
    (add-runtime-string-constant 'hash-less-namespace-colon "#<namespace:")
    (add-runtime-string-constant 'empty                     "")
    (add-runtime-string-constant 'open-paren                "(")
    (add-runtime-string-constant 'close-paren               ")")
    (add-runtime-string-constant 'space                     " ")
    (add-runtime-string-constant 'dot-space                 ". ")
    (add-runtime-string-constant 'space-dot-space           " . ")
    (add-runtime-string-constant 'vector-prefix             "#(")
    (add-runtime-string-constant 'values-prefix             "(values")
    (add-runtime-string-constant 'g                         "g")
    (add-runtime-string-constant 'struct-type-descriptor    "#<struct-type-descriptor>")
    (add-runtime-string-constant 'struct-open               "#(struct ")
    (add-runtime-string-constant 'hash-colon                "#:")
    (add-runtime-string-constant 'hash-backslash            "#\\")
    (add-runtime-string-constant 'hash-backslash-u          "#\\u")
    (add-runtime-string-constant 'hash-backslash-U          "#\\U")
    (add-runtime-string-constant 'word-newline              "newline")
    (add-runtime-string-constant 'word-tab                  "tab")
    (add-runtime-string-constant 'word-return               "return")
    (add-runtime-string-constant 'word-backspace            "backspace")
    (add-runtime-string-constant 'word-space                "space")
    (add-runtime-string-constant 'word-rubout               "rubout")
    (add-runtime-string-constant 'word-nul                  "nul")
    (add-runtime-string-constant 'hash-less-procedure-colon "#<procedure:")
    (add-runtime-string-constant 'hash-less-primitive-colon "#<primitive:")
    (add-runtime-string-constant 'unknown                   "unknown")
    (add-runtime-string-constant 'colon                     ":")
    (add-runtime-string-constant '->                        ">")
    (add-runtime-string-constant 'hash-less-boxed-colon     "#<boxed:")

    (add-runtime-bytes-constant  'empty                     #"")
    
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

          ;; Raw primitive function types
          (type $Prim0    (func                                   (result (ref eq))))
          (type $Prim1    (func (param (ref eq))                  (result (ref eq))))
          (type $Prim2    (func (param (ref eq)) (param (ref eq)) (result (ref eq))))
          (type $Prim3    (func (param (ref eq)) (param (ref eq)) (param (ref eq))
                                (result (ref eq))))
          (type $Prim4    (func (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
                                (result (ref eq))))
          (type $Prim5    (func (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
                                (result (ref eq))))

          (type $Prim>=0  (func (param (ref eq))      ;; list of args
                                (result (ref eq))))
          (type $Prim>=1  (func (param (ref eq))      ;; first arg
                                (param (ref eq))      ;; rest list
                                (result (ref eq))))
          (type $Prim>=2  (func (param (ref eq)) (param (ref eq))
                                (param (ref eq))      ;; rest list
                                (result (ref eq))))
          (type $Prim>=3  (func (param (ref eq)) (param (ref eq)) (param (ref eq))
                                (param (ref eq))      ;; rest list
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

          (type $CaseClosure  ; for case-lambda
                (sub $Closure
                     (struct
                       ;; inherited $Procedure fields
                       (field $hash   (mut i32))
                       (field $name   (ref eq))
                       (field $arity  (ref eq))        ;; store (ref $I32Array) here for Racket-style arity
                       (field $realm  (ref eq))
                       (field $invoke (ref $ProcedureInvoker))
                       ;; inherited $Closure fields
                       (field $code   (ref $ClosureCode))  ;; dispatcher
                       (field $free   (ref $Free))         ;; can be an empty array
                       ;; new, typed payload
                       (field $arities (ref $I32Array))    ;; markers: m>=0 exact m; m<0 at least (-m-1)
                       (field $arms    (ref $Array)))))    ;; (ref eq) array of arm closures
          

          (type $PrimitiveProcedure
                (sub $Procedure
                     (struct
                       ; From $Procedure
                       (field $hash   (mut i32))
                       (field $name   (ref eq))
                       (field $arity  (ref eq))
                       (field $realm  (ref eq))
                       (field $invoke (ref $ProcedureInvoker))
                       ; Function pointer for primitive implementation
                       (field $code (ref null func))
                       ; other fields
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
                       (field $code         (ref null func))
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
                                  (field $hash      (mut i32))
                                  (field $immutable i32)                        ;; 0 or 1
                                  (field $arr       (ref $Array)))))
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
          (type $External 
            (sub $Heap
              (struct
                (field $hash (mut i32))
                (field $v    (ref null extern)))))

          ;; (Placeholder) module registry
          (type $ModuleRegistry
                (sub $Heap
                     (struct
                       (field $hash  (mut i32))
                       (field $table (mut (ref $Array))))))

          ;; Namespace now maps Symbol → Boxed via a single hasheq/mutable table
          (type $Namespace
                (sub $Heap
                     (struct
                       (field $hash       (mut i32))
                       (field $name       (ref eq))                   ;; #f or $String
                       (field $base-phase i32)
                       (field $table      (mut (ref $HashEqMutable))) ;; hasheq: Symbol → Boxed
                       (field $modules    (mut (ref $ModuleRegistry)))
                       (field $protect    (mut i32)))))

          ;; A builder that accumulates arguments to be serialized for the host.
          (type $FaslBuilder
                (sub $Heap
                     (struct
                       (field $hash     (mut i32))               ;; lazy, start at 0
                       (field $strings  (ref $GrowableArray))    ;; growable of (ref $String)
                       (field $values   (ref $GrowableArray))))) ;; growable of (ref eq)
          
          
          
          ) ; rec
       


         ;;;
         ;;; MEMORY
         ;;;

         (import "env" "memory" (memory $memory 1024))

         ;;;
         ;;; IMPORTS
         ;;; 

         ;  Imported functions from the host (JavaScript).
         ;  Note all imports must appear before other function definitions.
         
         (func $js_output
               (import "primitives" "js_output")
               (param i32))
         (func $js_print_fasl
               (import "primitives" "js_print_fasl")
               (param i32) (param i32))

         (func $char-upcase/ucs
               (import "primitives" "char_upcase")
               (param i32) (result i32))

         (func $char-downcase/ucs
               (import "primitives" "char_downcase")
               (param i32) (result i32))

         (func $char-titlecase/ucs
               (import "primitives" "char_titlecase")
               (param i32) (result i32))

         (func $char-foldcase/ucs
               (import "primitives" "char_foldcase")
               (param i32) (result i32))

        ;; Math functions
        (func $js-math-abs
              (import "math" "abs")
              (param f64) (result f64))
        (func $js-math-acos
              (import "math" "acos")
              (param f64) (result f64))
        (func $js-math-acosh
              (import "math" "acosh")
              (param f64) (result f64))
        (func $js-math-asin
              (import "math" "asin")
              (param f64) (result f64))
        (func $js-math-asinh
              (import "math" "asinh")
              (param f64) (result f64))
        (func $js-math-atan
              (import "math" "atan")
              (param f64) (result f64))
        (func $js-math-atan2
              (import "math" "atan2")
              (param f64) (param f64) (result f64))
        (func $js-math-atanh
              (import "math" "atanh")
              (param f64) (result f64))
        (func $js-math-cbrt
              (import "math" "cbrt")
              (param f64) (result f64))
        (func $js-math-ceil
              (import "math" "ceil")
              (param f64) (result f64))
        (func $js-math-clz32
              (import "math" "clz32")
              (param i32) (result i32))
        (func $js-math-cos
              (import "math" "cos")
              (param f64) (result f64))
        (func $js-math-cosh
              (import "math" "cosh")
              (param f64) (result f64))
        (func $js-math-exp
              (import "math" "exp")
              (param f64) (result f64))
        (func $js-math-expm1
              (import "math" "expm1")
              (param f64) (result f64))
        (func $js-math-floor
              (import "math" "floor")
              (param f64) (result f64))
        (func $js-math-fround
              (import "math" "fround")
              (param f64) (result f64))
        (func $js-math-hypot
              (import "math" "hypot")
              (param f64) (param f64) (result f64))
        (func $js-math-imul
              (import "math" "imul")
              (param i32) (param i32) (result i32))
        (func $js-math-log
              (import "math" "log")
              (param f64) (result f64))
        (func $js-math-log10
              (import "math" "log10")
              (param f64) (result f64))
        (func $js-math-log1p
              (import "math" "log1p")
              (param f64) (result f64))
        (func $js-math-log2
              (import "math" "log2")
              (param f64) (result f64))
        (func $js-math-max
              (import "math" "max")
              (param f64) (param f64) (result f64))
        (func $js-math-min
              (import "math" "min")
              (param f64) (param f64) (result f64))
        (func $js-math-pow
              (import "math" "pow")
              (param f64) (param f64) (result f64))
        (func $js-math-random
              (import "math" "random")
              (result f64))
        (func $js-math-round
              (import "math" "round")
              (param f64) (result f64))
        (func $js-math-sign
              (import "math" "sign")
              (param f64) (result f64))
        (func $js-math-sin
              (import "math" "sin")
              (param f64) (result f64))
        (func $js-math-sinh
              (import "math" "sinh")
              (param f64) (result f64))
        (func $js-math-sqrt
              (import "math" "sqrt")
              (param f64) (result f64))
        (func $js-math-tan
              (import "math" "tan")
              (param f64) (result f64))
        (func $js-math-tanh
              (import "math" "tanh")
              (param f64) (result f64))
        (func $js-math-trunc
              (import "math" "trunc")
              (param f64) (result f64))

         (func $js-make-callback
               (import "primitives" "make_callback")
               (param i32)
               (result (ref extern)))

         (func $js-register-external
               (import "primitives" "register_external")
               (param (ref extern))
               (result i32))

         (func $js-lookup-external
               (import "primitives" "lookup_external")
               (param i32)
               (result (ref extern)))

         ;; FFI related imports
         ,@(current-ffi-imports-wat) ; generated from "driver.rkt" in "define-foreign.rkt"
         
         (func $raise-expected-string     (unreachable))
         (func $raise-unexpected-argument (unreachable))

         ,@(current-ffi-funcs-wat)   ; generated from "driver.rkt" in "define-foreign.rkt"
         
         
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
         ;; Bytes constants used in the runtime
         ,@(declare-runtime-bytes-constants)
         ;; Symbol constants used in the runtime
         ,@(declare-runtime-symbol-constants)
         
         ;; Commonly used realms
         (global $the-racket-realm           (mut (ref eq)) ,(Undefined)) ; the symbol 'racket
         (global $the-racket/primitive-realm (mut (ref eq)) ,(Undefined)) ; the symbol 'racket/primitive

         ;; Module Registry
         (global $empty-module-registry (ref $ModuleRegistry)
                 (struct.new $ModuleRegistry
                             (i32.const 0)
                             (array.new $Array (global.get $null) (i32.const 0))))

         ;; Namespaces
         (global $top-level-namespace (mut (ref null $Namespace)) (ref.null $Namespace))

         ;; Callback registry
         (global $callback-registry (ref $GrowableArray)
                 (struct.new $GrowableArray
                             (array.new $Array (global.get $false) (i32.const 4))
                             (i32.const 4)
                             (i32.const 0)))


         ;; Primitives (as values)
         ,@(declare-primitives-as-globals)

         ;; Closures

         ; Closures with no free variables can share an empty array
         (global $empty-free (ref $Free)
                 (array.new $Free (global.get $null) (i32.const 0)))
                                            
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
               ;; Debug: log argument count and expected arity
               #;(drop (call $js-log (call $i32->string (local.get $arg-count))))
               #;(drop (call $js-log (call $i32->string (local.get $arity-i32))))
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
               ;; Cast and fetch code pointer
               (local.set $clos (ref.cast (ref $Closure) (local.get $proc)))
               (local.set $code (struct.get $Closure $code (local.get $clos)))
               ;; Tail-call the dispatcher
               (return_call_ref $ClosureCode
                                (local.get $clos)
                                (local.get $args)
                                (local.get $code)))

        ;; Primitive procedure invocation helpers
        (func $raise-no-code
              (param $pproc (ref $PrimitiveProcedure))
              (result (ref eq))
              (unreachable))

        (func $raise-code-type-mismatch
              (param $pproc (ref $PrimitiveProcedure))
              (result (ref eq))
              (unreachable))

        (func $primitive-invoke:raise-arity-error
              (param $pproc (ref $PrimitiveProcedure))
              (param $argc  i32)
              (result       (ref eq))
              (unreachable))
        
        ;; Shape codes:
        ;;   0: exact 0     1: exact 1     2: exact 2     3: exact 3     4: exact 4
        ;;   5: exact 5     6: at least 0  7: at least 1  8: at least 2  9: at least >=3
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

              (local $a0 (ref eq))
              (local $a1 (ref eq))
              (local $a2 (ref eq))
              (local $a3 (ref eq))
              (local $a4 (ref eq))

              (local.set $a0 (global.get $null))
              (local.set $a1 (global.get $null))
              (local.set $a2 (global.get $null))
              ; (local.set $a3 (global.get $null))
              ; (local.set $a4 (global.get $null))
              
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
                   ;; exact: shape = min(arity, 5)
                   (local.set $shape (local.get $arity))
                   (if (i32.gt_u (local.get $shape) (i32.const 5))
                       (then (local.set $shape (i32.const 5)))))
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
                                    (br_table $L0 $L1 $L2 $L3 $L4 $L5 $L6 $L7 $L8 $L9 $default (local.get $shape))
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
                              (then
                               (if (ref.test (ref $Prim4) (local.get $code))
                                   (then
                                    (return_call_ref $Prim4
                                                     (local.get $a0)
                                                     (local.get $a1)
                                                     (local.get $a2)
                                                     (array.get $Args (local.get $args) (i32.const 3))
                                                     (ref.cast (ref $Prim4) (local.get $code))))
                                   (else
                                    (return (call $raise-code-type-mismatch (local.get $pproc)))))
                              (else
                               (return (call $primitive-invoke:raise-arity-error
                                             (local.get $pproc) (local.get $argc)))))
                        )) ;; end $L5
                        ;; shape 5: exact 5
                        #;(drop (call $js-log (call $i32->string (i32.const 5))))
                        (if (i32.eq (local.get $argc) (i32.const 5))
                            (then
                             (if (ref.test (ref $Prim5) (local.get $code))
                                 (then
                                  (return_call_ref $Prim5
                                                   (local.get $a0)
                                                   (local.get $a1)
                                                   (local.get $a2)
                                                   (array.get $Args (local.get $args) (i32.const 3))
                                                   (array.get $Args (local.get $args) (i32.const 4))
                                                   (ref.cast (ref $Prim5) (local.get $code))))
                                 (else
                                  (return (call $raise-code-type-mismatch (local.get $pproc)))))
                            (else
                             (return (call $primitive-invoke:raise-arity-error
                                           (local.get $pproc) (local.get $argc)))))
                      )) ;; end $L6
                      ;; shape 6: at least 0
                      #;(drop (call $js-log (call $i32->string (i32.const 6))))
                      (if (ref.test (ref $Prim>=0) (local.get $code))
                          (then
                           (return_call_ref $Prim>=0
                                            (local.get $args)
                                            (ref.cast (ref $Prim>=0) (local.get $code))))
                          (else
                           (return (call $raise-code-type-mismatch (local.get $pproc))))
                    )) ;; end $L7
                    ;; shape 7: at least 1
                    #;(drop (call $js-log (call $i32->string (i32.const 7))))
                    (if (i32.ge_u (local.get $argc) (i32.const 1))
                        (then
                         (if (ref.test (ref $Prim>=1) (local.get $code))
                             (then
                              (return_call_ref $Prim>=1
                                               (local.get $a0)
                                               (local.get $args)
                                               (ref.cast (ref $Prim>=1) (local.get $code))))
                             (else
                              (return (call $raise-code-type-mismatch (local.get $pproc)))))
                        (else
                         (return (call $primitive-invoke:raise-arity-error
                                       (local.get $pproc) (local.get $argc)))))
                  )) ;; end $L8
                  ;; shape 8: at least 2
                  #;(drop (call $js-log (call $i32->string (i32.const 8))))
                  (if (i32.ge_u (local.get $argc) (i32.const 2))
                      (then
                       (if (ref.test (ref $Prim>=2) (local.get $code))
                           (then
                            (return_call_ref $Prim>=2
                                             (local.get $a0)
                                             (local.get $a1)
                                             (local.get $args)
                                             (ref.cast (ref $Prim>=2) (local.get $code))))
                           (else
                            (return (call $raise-code-type-mismatch (local.get $pproc)))))
                      (else
                       (return (call $primitive-invoke:raise-arity-error
                                     (local.get $pproc) (local.get $argc)))))
                )) ;; end $L9
                ;; shape 9: at least >=3
                #;(drop (call $js-log (call $i32->string (i32.const 9))))
                (if (i32.ge_u (local.get $argc) (i32.const 3))
                    (then
                     (if (ref.test (ref $Prim>=3) (local.get $code))
                         (then
                          (return_call_ref $Prim>=3
                                           (local.get $a0)
                                           (local.get $a1)
                                           (local.get $a2)
                                           (local.get $args)
                                           (ref.cast (ref $Prim>=3) (local.get $code))))
                         (else
                          (return (call $raise-code-type-mismatch (local.get $pproc)))))
                    (else
                     (return (call $primitive-invoke:raise-arity-error
                                   (local.get $pproc) (local.get $argc)))))
              )) ;; end $default
              #;(drop (call $js-log (call $i32->string (i32.const 10))))
              (unreachable))

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
         (func $raise-arity-error/case-lambda/arities (unreachable))
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
                         (then (call $raise-arity-error/case-lambda/arities
                                     (local.get $argc) (local.get $arities))
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
         (global ,result (mut (ref eq)) (global.get $void))
        
         ;; Variables defined at the top-level
         ,@top-level-variable-declarations

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
              (local $src-len i32)
              (local $dest-len i32)
              (local $len i32)
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
              (local.set $len (i32.sub (local.get $src-end) (local.get $src-start)))
              (array.copy $Array $Array
                          (local.get $dest) (local.get $dest-start)
                          (local.get $src)  (local.get $src-start)
                          (local.get $len)))
         
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
              (local $src-len i32)
              (local $dest-len i32)
              (local $len i32)
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
              (local.set $len (i32.sub (local.get $src-end) (local.get $src-start)))
              (array.copy $Array $Array
                          (local.get $dest) (local.get $dest-start)
                          (local.get $src)  (local.get $src-start)
                          (local.get $len))
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
              (local $len        i32)

              (local.set $len (i32.sub (local.get $src-end) (local.get $src-start)))
              (array.copy $I32Array $I32Array
                          (local.get $dest) (local.get $dest-start)
                          (local.get $src)  (local.get $src-start)
                          (local.get $len)))

        (func $i32array-copy!/error (param $dest (ref $I32Array)) (param $dest-start i32)
              (param $src (ref $I32Array)) (param $src-start i32) (param $src-end i32)
              (result i32)
              (local $src-len i32)
              (local $dest-len i32)
              (local $len i32)
              (local.set $src-len (array.len (local.get $src)))
              (local.set $dest-len (array.len (local.get $dest)))
              (if (i32.or
                   (i32.or (i32.lt_u (local.get $src-start) (i32.const 0))
                           (i32.gt_u (local.get $src-end) (local.get $src-len)))
                   (i32.gt_u (i32.add (local.get $dest-start) (i32.sub (local.get $src-end) (local.get $src-start)))
                             (local.get $dest-len)))
                  (then (return (i32.const 0))))
              (local.set $len (i32.sub (local.get $src-end) (local.get $src-start)))
              (array.copy $I32Array $I32Array
                          (local.get $dest) (local.get $dest-start)
                          (local.get $src)  (local.get $src-start)
                          (local.get $len))
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
              (local $src-len i32)
              (local $dest-len i32)
              (local $len i32)
              (local.set $src-len (array.len (local.get $src)))
              (local.set $dest-len (array.len (local.get $dest)))
              (if (i32.or
                   (i32.or (i32.lt_u (local.get $src-start) (i32.const 0))
                           (i32.gt_u (local.get $src-end) (local.get $src-len)))
                   (i32.gt_u (i32.add (local.get $dest-start)
                                      (i32.sub (local.get $src-end) (local.get $src-start)))
                             (local.get $dest-len)))
                  (then (unreachable)))
              (local.set $len (i32.sub (local.get $src-end) (local.get $src-start)))
              (array.copy $I8Array $I8Array
                          (local.get $dest) (local.get $dest-start)
                          (local.get $src)  (local.get $src-start)
                          (local.get $len)))

        (func $i8array-copy!/error (export "i8array-copy!/error")
              (param $dest (ref $I8Array))
              (param $dest-start i32)
              (param $src (ref $I8Array))
              (param $src-start i32)
              (param $src-end i32)
              (result i32)
              (local $src-len i32)
              (local $dest-len i32)
              (local $len i32)
              (local.set $src-len (array.len (local.get $src)))
              (local.set $dest-len (array.len (local.get $dest)))
              (if (i32.or
                   (i32.or (i32.lt_u (local.get $src-start) (i32.const 0))
                           (i32.gt_u (local.get $src-end) (local.get $src-len)))
                   (i32.gt_u (i32.add (local.get $dest-start)
                                      (i32.sub (local.get $src-end) (local.get $src-start)))
                             (local.get $dest-len)))
                  (then (return (i32.const 0))))
              (local.set $len (i32.sub (local.get $src-end) (local.get $src-start)))
              (array.copy $I8Array $I8Array
                          (local.get $dest) (local.get $dest-start)
                          (local.get $src)  (local.get $src-start)
                          (local.get $len))
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
         ;;; DATATYPES
         ;;;

         ;; https://docs.racket-lang.org/reference/data.html

         ;;; 
         ;;;  4.1 Equality
         ;;;

         ;; https://docs.racket-lang.org/reference/Equality.html

         (func $eq? (type $Prim2) (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.eq (local.get $v1) (local.get $v2))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $eqv? (type $Prim2)
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
         (func $equal? (type $Prim2)
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
         ;;; 4.2 Booleans
         ;;;

         ;; https://docs.racket-lang.org/reference/booleans.html

         ;; [x] boolean?
         ;; [x] not
         ;; [x] immutable?
         
         ;;; Boolean Aliases
         ;; [ ] true
         ;; [ ] false
         ;; [ ] symbol=?
         ;; [ ] boolean=?
         ;; [ ] false?
         ;; [ ] nand
         ;; [ ] nor
         ;; [ ] implies
         ;; [ ] xor

         ;;; Mutability Predicates
         ;; [ ] mutable-string?
         ;; [ ] immutable-string?
         ;; [ ] mutable-bytes?
         ;; [ ] immutable-bytes?
         ;; [ ] mutable-vector?
         ;; [ ] immutable-vector?
         ;; [ ] mutable-box?
         ;; [ ] immutable-box?
         ;; [ ] mutable-hash?
         ;; [ ] immutable-hash?
         
         ; todo: Benchmark the two implementations of $boolean? below

         (func $boolean? (type $Prim1) (param $v (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.eq (local.get $v) (global.get $true))
                   (then (global.get $true))
                   (else (if (result (ref eq))
                             (ref.eq (local.get $v) (global.get $false))
                             (then (global.get $true))
                             (else (global.get $false))))))
         ; see comment above
         #;(func $boolean? (type $Prim1) (param $v (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.test i31ref (local.get $v))
                   (then (if (result (ref eq))
                             (i32.eq (i32.and (i31.get_s (ref.cast i31ref (local.get $v)))
                                              (i32.const ,boolean-mask))
                                     (i32.const ,boolean-tag))
                             (then (global.get $true))
                             (else (global.get $false))))
                   (else (global.get $false))))

         (func $not (type $Prim1) (param $v (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.eq (local.get $v) (global.get $false))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $immutable? (type $Prim1)
               (param $v (ref eq))
               (result (ref eq))

               (local $s    (ref $String))
               (local $bs   (ref $Bytes))
               (local $vec  (ref $Vector))
               (local $hash (ref $Hash))

               (if (ref.test (ref $String) (local.get $v))
                   (then
                    (local.set $s (ref.cast (ref $String) (local.get $v)))
                    (if (i32.eq (struct.get $String $immutable (local.get $s)) (i32.const 1))
                        (then (return (global.get $true)))
                        (else (return (global.get $false))))))
               (if (ref.test (ref $Bytes) (local.get $v))
                   (then
                    (local.set $bs (ref.cast (ref $Bytes) (local.get $v)))
                    (if (i32.eq (struct.get $Bytes $immutable (local.get $bs)) (i32.const 1))
                        (then (return (global.get $true)))
                        (else (return (global.get $false))))))
               (if (ref.test (ref $Vector) (local.get $v))
                   (then
                    (local.set $vec (ref.cast (ref $Vector) (local.get $v)))
                    (if (i32.eq (struct.get $Vector $immutable (local.get $vec)) (i32.const 1))
                        (then (return (global.get $true)))
                        (else (return (global.get $false))))))
               (if (ref.test (ref $Hash) (local.get $v))
                   (then
                    (local.set $hash (ref.cast (ref $Hash) (local.get $v)))
                    (if (ref.eq (struct.get $Hash $mutable? (local.get $hash)) (global.get $false))
                        (then (return (global.get $true)))
                        (else (return (global.get $false))))))
               (if (ref.test (ref $Box) (local.get $v))
                   (then (return (global.get $false))))
               (global.get $false))
         
         ;;;
         ;;; 4.3 Numbers
         ;;;

         ;; https://docs.racket-lang.org/reference/numbers.html

         ;;; 4.3.1 Number Types

         ;; https://docs.racket-lang.org/reference/number-types.html

         ;; [x] number?
         ;; [ ] complex?
         ;; [ ] real?
         ;; [ ] rational?
         ;; [x] integer?
         ;; [x] exact-integer?
        ;; [x] exact-nonnegative-integer?
        ;; [x] exact-positive-integer?
        ;; [x] positive-integer?
        ;; [x] negative-integer?
        ;; [x] nonpositive-integer?
        ;; [x] nonnegative-integer?
        ;; [x] natural?
        ;; [x] nan?
        ;; [x] infinite?
        ;; [ ] inexact-real?
        ;; [ ] fixnum?
        ;; [ ] flonum?
        ;; [ ] double-flonum?
        ;; [ ] single-flonum?
        ;; [ ] single-flonum-available?
        ;; [x] zero?
        ;; [x] positive?
        ;; [x] negative?
        ;; [x] even?
        ;; [x] odd?
        ;; [x] exact?
        ;; [x] inexact?
        ;; [x] inexact->exact
        ;; [x] exact->inexact
        ;; [ ] real->single-flonum
        ;; [ ] real->double-flonum

         (func $number? (type $Prim1)
               (param $v (ref eq))
               (result (ref eq))
               (if (result (ref eq))
                   (ref.test i31ref (local.get $v))
                   (then (if (result (ref eq))
                             (i32.eqz (i32.and (i31.get_s (ref.cast i31ref (local.get $v)))
                                               (i32.const ,fixnum-mask)))
                             (then (global.get $true))
                             (else (global.get $false))))
                   (else (if (result (ref eq))
                             (ref.test (ref $Flonum) (local.get $v))
                             (then (global.get $true))
                             (else (global.get $false))))))

         (func $exact? (type $Prim1)
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

         (func $inexact? (type $Prim1)
               ;; A number is inexact if it's a flonum
               (param $z (ref eq))
               (result   (ref eq))

               (if (result (ref eq))
                   (ref.test (ref $Flonum) (local.get $z))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $exact-integer? (type $Prim1)
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

         (func $exact-nonnegative-integer? (type $Prim1)
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

         (func $exact-positive-integer? (type $Prim1)
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




        (func $inexact->exact (type $Prim1)
              (param $z (ref eq))
              (result   (ref eq))

              (local $bits i32)
              (local $fl (ref $Flonum))
              (local $f64 f64)
              (local $i32 i32)

              ;; If z is a fixnum, ensure LSB = 0 and return it
              (if (ref.test (ref i31) (local.get $z))
                  (then
                   (local.set $bits (i31.get_u (ref.cast (ref i31) (local.get $z))))
                   (if (i32.eqz (i32.and (local.get $bits) (i32.const 1)))
                       (then (return (local.get $z))))))

              ;; If z is a flonum, convert to fixnum if finite and integral
              (if (ref.test (ref $Flonum) (local.get $z))
                  (then
                   (local.set $fl (ref.cast (ref $Flonum) (local.get $z)))
                   (local.set $f64 (struct.get $Flonum $v (local.get $fl)))
                   (if (f64.ne (local.get $f64) (local.get $f64))
                       (then (call $raise-expected-number (local.get $z)) (unreachable)))
                   (if (f64.eq (local.get $f64) (f64.const +inf))
                       (then (call $raise-expected-number (local.get $z)) (unreachable)))
                   (if (f64.eq (local.get $f64) (f64.const -inf))
                       (then (call $raise-expected-number (local.get $z)) (unreachable)))
                   (if (f64.eq (f64.floor (local.get $f64)) (local.get $f64))
                       (then
                        (local.set $i32 (i32.trunc_f64_s (local.get $f64)))
                        (return (ref.i31 (i32.shl (local.get $i32) (i32.const 1)))))
                       (else (call $raise-expected-number (local.get $z)) (unreachable)))))

              ;; Not a number
              (call $raise-expected-number (local.get $z))
              (unreachable))


        (func $exact->inexact (type $Prim1)
              (param $z (ref eq))
              (result   (ref eq))

              (local $bits i32)

              ;; If z is already a flonum, return it
              (if (ref.test (ref $Flonum) (local.get $z))
                  (then (return (local.get $z))))

              ;; If z is a fixnum, ensure LSB = 0 and convert
              (if (ref.test (ref i31) (local.get $z))
                  (then
                   (local.set $bits (i31.get_u (ref.cast (ref i31) (local.get $z))))
                   (if (i32.eqz (i32.and (local.get $bits) (i32.const 1)))
                       (then (return (struct.new $Flonum (i32.const 0)
                                                 (f64.convert_i32_s
                                                  (i32.shr_u (local.get $bits)
                                                             (i32.const 1)))))))))

              ;; Not a number
              (call $raise-expected-number (local.get $z))
              (unreachable))


        (func $zero? (type $Prim1)
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

        (func $integer-sqrt (type $Prim1)
              (param $n (ref eq))
              (result (ref eq))

              (local $bits i32)
              (local $fl (ref $Flonum))
              (local $f64 f64)
              (local $i32 i32)

              ;; Fixnum case
              (if (ref.test (ref i31) (local.get $n))
                  (then
                   (local.set $bits (i31.get_s (ref.cast (ref i31) (local.get $n))))
                   (if (i32.eqz (i32.and (local.get $bits) (i32.const 1)))
                       (then
                        (local.set $i32 (i32.shr_s (local.get $bits) (i32.const 1)))
                        (local.set $f64 (f64.floor (call $js-math-sqrt (f64.convert_i32_s (local.get $i32)))))
                        (if (i32.ge_s (local.get $i32) (i32.const 0))
                            (then
                             (local.set $i32 (i32.trunc_f64_s (local.get $f64)))
                             (return (ref.i31 (i32.shl (local.get $i32) (i32.const 1)))))
                            (else
                             (return (struct.new $Flonum (i32.const 0) (local.get $f64))))))
                       (else (call $raise-expected-number (local.get $n))
                             (unreachable)))))

              ;; Flonum case
              (if (ref.test (ref $Flonum) (local.get $n))
                  (then
                   (local.set $fl (ref.cast (ref $Flonum) (local.get $n)))
                   (local.set $f64 (struct.get $Flonum $v (local.get $fl)))
                   (if (f64.eq (f64.floor (local.get $f64)) (local.get $f64))
                       (then
                        (return (struct.new $Flonum
                                            (i32.const 0)
                                            (f64.floor (call $js-math-sqrt (local.get $f64))))))
                       (else (call $raise-expected-number (local.get $n))
                             (unreachable)))))

              ;; Not a number
              (call $raise-expected-number (local.get $n))
              (unreachable))

        (func $integer-sqrt/remainder (type $Prim1)
              (param $n (ref eq))
              (result (ref eq))

              (local $bits i32)
              (local $fl (ref $Flonum))
              (local $f64 f64)
              (local $i32 i32)
              (local $sqrtf f64)
              (local $remf f64)
              (local $sq i32)
              (local $rem i32)

              ;; Fixnum case
              (if (ref.test (ref i31) (local.get $n))
                  (then
                   (local.set $bits (i31.get_s (ref.cast (ref i31) (local.get $n))))
                   (if (i32.eqz (i32.and (local.get $bits) (i32.const 1)))
                       (then
                        (local.set $i32 (i32.shr_s (local.get $bits) (i32.const 1)))
                        (local.set $sqrtf (f64.floor (call $js-math-sqrt (f64.convert_i32_s (local.get $i32)))))
                        (if (i32.ge_s (local.get $i32) (i32.const 0))
                            (then
                             (local.set $sq   (i32.trunc_f64_s (local.get $sqrtf)))
                             (local.set $rem  (i32.sub (local.get $i32) (i32.mul (local.get $sq) (local.get $sq))))
                             (return (array.new_fixed $Values 2
                                                       (ref.i31 (i32.shl (local.get $sq) (i32.const 1)))
                                                       (ref.i31 (i32.shl (local.get $rem) (i32.const 1))))))
                            (else
                             (local.set $remf (f64.sub (f64.convert_i32_s (local.get $i32))
                                                       (f64.mul (local.get $sqrtf) (local.get $sqrtf))))
                             (return (array.new_fixed $Values 2
                                                       (struct.new $Flonum (i32.const 0) (local.get $sqrtf))
                                                       (struct.new $Flonum (i32.const 0) (local.get $remf))))))
                       (else (call $raise-expected-number (local.get $n))
                             (unreachable))))))

              ;; Flonum case
              (if (ref.test (ref $Flonum) (local.get $n))
                  (then
                   (local.set $fl (ref.cast (ref $Flonum) (local.get $n)))
                   (local.set $f64 (struct.get $Flonum $v (local.get $fl)))
                   (if (f64.eq (f64.floor (local.get $f64)) (local.get $f64))
                       (then
                        (local.set $sqrtf (f64.floor (call $js-math-sqrt (local.get $f64))))
                        (local.set $remf (f64.sub (local.get $f64) (f64.mul (local.get $sqrtf) (local.get $sqrtf))))
                        (return (array.new_fixed $Values 2
                                                  (struct.new $Flonum (i32.const 0) (local.get $sqrtf))
                                                  (struct.new $Flonum (i32.const 0) (local.get $remf)))))
                       (else (call $raise-expected-number (local.get $n))
                             (unreachable)))))

              ;; Not a number
              (call $raise-expected-number (local.get $n))
              (unreachable))

        (func $expt (type $Prim2)
              (param $z (ref eq))
              (param $w (ref eq))
              (result (ref eq))

              (local $zbits i32)
              (local $wbits i32)
              (local $zf64 f64)
              (local $wf64 f64)
              (local $res f64)
              (local $exactz i32)
              (local $exactw i32)

              ;; Decode z
              (if (ref.test (ref i31) (local.get $z))
                  (then
                   (local.set $zbits (i31.get_s (ref.cast (ref i31) (local.get $z))))
                   (if (i32.eqz (i32.and (local.get $zbits) (i32.const 1)))
                       (then
                        (local.set $zf64 (f64.convert_i32_s (i32.shr_s (local.get $zbits) (i32.const 1))))
                        (local.set $exactz (i32.const 1)))
                       (else (call $raise-expected-number (local.get $z)) (unreachable))))
                  (else
                   (if (ref.test (ref $Flonum) (local.get $z))
                       (then
                        (local.set $zf64 (struct.get $Flonum $v (ref.cast (ref $Flonum) (local.get $z))))
                        (local.set $exactz (i32.const 0)))
                       (else (call $raise-expected-number (local.get $z)) (unreachable)))))

              ;; Decode w
              (if (ref.test (ref i31) (local.get $w))
                  (then
                   (local.set $wbits (i31.get_s (ref.cast (ref i31) (local.get $w))))
                   (if (i32.eqz (i32.and (local.get $wbits) (i32.const 1)))
                       (then
                        (local.set $wf64 (f64.convert_i32_s (i32.shr_s (local.get $wbits) (i32.const 1))))
                        (local.set $exactw (i32.const 1)))
                       (else (call $raise-expected-number (local.get $w)) (unreachable))))
                  (else
                   (if (ref.test (ref $Flonum) (local.get $w))
                       (then
                        (local.set $wf64 (struct.get $Flonum $v (ref.cast (ref $Flonum) (local.get $w))))
                        (local.set $exactw (i32.const 0)))
                       (else (call $raise-expected-number (local.get $w)) (unreachable)))))
              
              ;; Special cases for exact results
              (if (i32.and (local.get $exactw)
                           (i32.eqz (i32.shr_s (local.get $wbits) (i32.const 1))))
                  (then (return (ref.i31 (i32.const 2)))))
              (if (i32.and (local.get $exactz)
                           (i32.eq (i32.shr_s (local.get $zbits) (i32.const 1)) (i32.const 1)))
                  (then (return (ref.i31 (i32.const 2)))))

              ;; Compute using JS pow
              (local.set $res (call $js-math-pow (local.get $zf64) (local.get $wf64)))

              ;; If both operands exact and result integral within range, return fixnum
              (if (i32.and (local.get $exactz) (local.get $exactw))
                  (then
                       (if (f64.eq (f64.floor (local.get $res)) (local.get $res))
                       (then
                        ;; 1073741823.0 = 2^30-1 and -1073741824.0 = -2^30
                        ;; ensure result fits in fixnum range before converting
                        (if (f64.le (local.get $res) (f64.const 1073741823.0))
                            (then
                             (if (f64.ge (local.get $res) (f64.const -1073741824.0))
                                 (then
                                  (return (ref.i31 (i32.shl (i32.trunc_f64_s (local.get $res)) (i32.const 1))))))))))))

              (struct.new $Flonum (i32.const 0) (local.get $res)))

        (func $exp (type $Prim1)
              (param $x (ref eq))
              (result (ref eq))

              (local $bits i32)
              (local $fl (ref $Flonum))
              (local $f64 f64)

              ;; Fixnum case
              (if (ref.test (ref i31) (local.get $x))
                  (then
                   (local.set $bits (i31.get_s (ref.cast (ref i31) (local.get $x))))
                   (if (i32.eqz (i32.and (local.get $bits) (i32.const 1)))
                       (then
                        (local.set $bits (i32.shr_s (local.get $bits) (i32.const 1)))
                        (if (i32.eqz (local.get $bits))
                            (then (return (ref.i31 (i32.const 2))))
                            (else
                             (local.set $f64 (call $js-math-exp (f64.convert_i32_s (local.get $bits))))
                             (return (struct.new $Flonum (i32.const 0) (local.get $f64))))))
                       (else (call $raise-expected-number (local.get $x))
                             (unreachable)))))

              ;; Flonum case
              (if (ref.test (ref $Flonum) (local.get $x))
                  (then
                   (local.set $fl (ref.cast (ref $Flonum) (local.get $x)))
                   (local.set $f64 (struct.get $Flonum $v (local.get $fl)))
                   (return (struct.new $Flonum (i32.const 0) (call $js-math-exp (local.get $f64))))))

              ;; Not a number
              (call $raise-expected-number (local.get $x))
              (unreachable))

        (func $log (type $Prim2)
              (param $z (ref eq))
              (param $b (ref eq))
              (result (ref eq))

              (local $bits i32)
              (local $fl (ref $Flonum))
              (local $zf64 f64)
              (local $bf64 f64)
              (local $res f64)

              ;; Natural log when base is missing
              (if (ref.eq (local.get $b) (global.get $missing))
                  (then
                   (if (ref.test (ref i31) (local.get $z))
                       (then
                        (local.set $bits (i31.get_s (ref.cast (ref i31) (local.get $z))))
                        (if (i32.eqz (i32.and (local.get $bits) (i32.const 1)))
                            (then
                             (local.set $bits (i32.shr_s (local.get $bits) (i32.const 1)))
                             (if (i32.eqz (local.get $bits))
                                 (then (call $raise-division-by-zero) (unreachable)))
                             (if (i32.eq (local.get $bits) (i32.const 1))
                                 (then (return (ref.i31 (i32.const 0)))))
                             (local.set $res
                                        (call $js-math-log (f64.convert_i32_s (local.get $bits))))
                             (return (struct.new $Flonum (i32.const 0) (local.get $res))))
                            (else (call $raise-expected-number (local.get $z)) (unreachable)))))
                   (if (ref.test (ref $Flonum) (local.get $z))
                       (then
                        (local.set $fl (ref.cast (ref $Flonum) (local.get $z)))
                        (local.set $zf64 (struct.get $Flonum $v (local.get $fl)))
                        (return (struct.new $Flonum (i32.const 0)
                                            (call $js-math-log (local.get $zf64))))))
                   (call $raise-expected-number (local.get $z))
                   (unreachable))
                  (else
                   ;; Log with base
                   (if (ref.test (ref i31) (local.get $z))
                       (then
                        (local.set $bits (i31.get_s (ref.cast (ref i31) (local.get $z))))
                        (if (i32.eqz (i32.and (local.get $bits) (i32.const 1)))
                            (then
                             (local.set $bits (i32.shr_s (local.get $bits) (i32.const 1)))
                             (if (i32.eqz (local.get $bits))
                                 (then (call $raise-division-by-zero) (unreachable)))
                             (local.set $zf64 (f64.convert_i32_s (local.get $bits))))
                            (else (call $raise-expected-number (local.get $z)) (unreachable))))
                       (else
                        (if (ref.test (ref $Flonum) (local.get $z))
                            (then
                             (local.set $fl (ref.cast (ref $Flonum) (local.get $z)))
                             (local.set $zf64 (struct.get $Flonum $v (local.get $fl))))
                            (else (call $raise-expected-number (local.get $z)) (unreachable)))))
                   (if (ref.test (ref i31) (local.get $b))
                       (then
                        (local.set $bits (i31.get_s (ref.cast (ref i31) (local.get $b))))
                        (if (i32.eqz (i32.and (local.get $bits) (i32.const 1)))
                            (then
                             (local.set $bits (i32.shr_s (local.get $bits) (i32.const 1)))
                             (if (i32.eq (local.get $bits) (i32.const 1))
                                 (then (call $raise-division-by-zero) (unreachable)))
                             (local.set $bf64 (f64.convert_i32_s (local.get $bits))))
                            (else (call $raise-expected-number (local.get $b)) (unreachable))))
                       (else
                        (if (ref.test (ref $Flonum) (local.get $b))
                            (then
                             (local.set $fl (ref.cast (ref $Flonum) (local.get $b)))
                             (local.set $bf64 (struct.get $Flonum $v (local.get $fl))))
                            (else (call $raise-expected-number (local.get $b)) (unreachable)))))
                   (local.set $res
                              (f64.div (call $js-math-log (local.get $zf64))
                                       (call $js-math-log (local.get $bf64))))
                   (return (struct.new $Flonum (i32.const 0) (local.get $res)))))
              ;; Ensure static fallthrough is unreachable for the validator.
              (unreachable))


        ;; Generic numeric unary functions
         (func $positive? (type $Prim1)
               (param $x (ref eq))
               (result   (ref eq))

               (local $x/fx i32)
               (local $x/fl (ref $Flonum))

               ;; If x is a fixnum, check if it's > 0
               (if (ref.test (ref i31) (local.get $x))
                   (then
                    (local.set $x/fx (i31.get_s (ref.cast (ref i31) (local.get $x))))
                    (if (i32.eqz (i32.and (local.get $x/fx) (i32.const 1))) ;; lsb = 0?
                        (then
                         (if (i32.gt_s (i32.shr_s (local.get $x/fx) (i32.const 1))
                                       (i32.const 0))
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

         (func $negative? (type $Prim1)
               (param $x (ref eq))
               (result (ref eq))

               (local $x/fx i32)
               (local $x/fl (ref $Flonum))

               ;; If x is a fixnum, check if it's < 0
               (if (ref.test (ref i31) (local.get $x))
                   (then
                    (local.set $x/fx (i31.get_s (ref.cast (ref i31) (local.get $x))))
                    (if (i32.eqz (i32.and (local.get $x/fx) (i32.const 1))) ;; lsb = 0?
                        (then
                         (if (i32.lt_s (i32.shr_s (local.get $x/fx) (i32.const 1)) (i32.const 0))
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

         (func $even? (type $Prim1)
               (param $n (ref eq))
               (result (ref eq))

               (local $bits i32)
               (local $fl (ref $Flonum))
               (local $f64 f64)
               (local $i32 i32)

               ;; Fixnum case
               (if (ref.test (ref i31) (local.get $n))
                   (then
                    (local.set $bits (i31.get_s (ref.cast (ref i31) (local.get $n))))
                    (if (i32.eqz (i32.and (local.get $bits) (i32.const 1)))
                        (then
                         (local.set $i32 (i32.shr_s (local.get $bits) (i32.const 1)))
                         (if (i32.eqz (i32.and (local.get $i32) (i32.const 1)))
                             (then (return (global.get $true)))
                             (else (return (global.get $false)))))
                        (else (call $raise-expected-number (local.get $n)) (unreachable)))))

               ;; Flonum case
               (if (ref.test (ref $Flonum) (local.get $n))
                   (then
                    (local.set $fl (ref.cast (ref $Flonum) (local.get $n)))
                    (local.set $f64 (struct.get $Flonum $v (local.get $fl)))
                    (if (f64.ne (local.get $f64) (local.get $f64))
                        (then (call $raise-expected-number (local.get $n)) (unreachable)))
                    (if (f64.eq (local.get $f64) (f64.const +inf))
                        (then (call $raise-expected-number (local.get $n)) (unreachable)))
                    (if (f64.eq (local.get $f64) (f64.const -inf))
                        (then (call $raise-expected-number (local.get $n)) (unreachable)))
                    (if (f64.eq (f64.floor (local.get $f64)) (local.get $f64))
                        (then
                         (local.set $i32 (i32.trunc_f64_s (local.get $f64)))
                         (if (i32.eqz (i32.and (local.get $i32) (i32.const 1)))
                             (then (return (global.get $true)))
                             (else (return (global.get $false)))))
                        (else (call $raise-expected-number (local.get $n)) (unreachable)))))

               ;; Not an integer
               (call $raise-expected-number (local.get $n))
               (unreachable))

         (func $odd? (type $Prim1)
               (param $n (ref eq))
               (result (ref eq))

               (if (result (ref eq))
                   (ref.eq (call $even? (local.get $n)) (global.get $true))
                   (then (global.get $false))
                   (else (global.get $true))))


         (func $integer? (type $Prim1)
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

         (func $nan? (type $Prim1)
               (param $x (ref eq))
               (result (ref eq))

               (local $x/fl (ref $Flonum))
               (local $f64 f64)

               ;; Fixnum -> #f
               (if (ref.test (ref i31) (local.get $x))
                   (then (return (global.get $false))))

               ;; Flonum -> check NaN
               (if (ref.test (ref $Flonum) (local.get $x))
                   (then
                    (local.set $x/fl (ref.cast (ref $Flonum) (local.get $x)))
                    (local.set $f64 (struct.get $Flonum $v (local.get $x/fl)))
                    (if (f64.ne (local.get $f64) (local.get $f64))
                        (then (return (global.get $true)))
                        (else (return (global.get $false))))))

               ;; Not a number
               (call $raise-expected-number (local.get $x))
               (unreachable))

         (func $infinite? (type $Prim1)
               (param $x (ref eq))
               (result (ref eq))

               (local $x/fl (ref $Flonum))
               (local $f64 f64)

               ;; Fixnum -> #f
               (if (ref.test (ref i31) (local.get $x))
                   (then (return (global.get $false))))

               ;; Flonum -> check ±inf
               (if (ref.test (ref $Flonum) (local.get $x))
                   (then
                    (local.set $x/fl (ref.cast (ref $Flonum) (local.get $x)))
                    (local.set $f64 (struct.get $Flonum $v (local.get $x/fl)))
                    (if (f64.eq (local.get $f64) (f64.const +inf))
                        (then (return (global.get $true))))
                    (if (f64.eq (local.get $f64) (f64.const -inf))
                        (then (return (global.get $true))))
                    (return (global.get $false))))

               ;; Not a number
               (call $raise-expected-number (local.get $x))
               (unreachable))

         (func $positive-integer? (type $Prim1)
               (param $x (ref eq))
               (result (ref eq))

               (if (result (ref eq))
                   (ref.eq (call $integer? (local.get $x)) (global.get $true))
                   (then (call $positive? (local.get $x)))
                   (else (global.get $false))))

         (func $negative-integer? (type $Prim1)
               (param $x (ref eq))
               (result (ref eq))

               (if (result (ref eq))
                   (ref.eq (call $integer? (local.get $x)) (global.get $true))
                   (then (call $negative? (local.get $x)))
                   (else (global.get $false))))

         (func $nonpositive-integer? (type $Prim1)
               (param $x (ref eq))
               (result (ref eq))

               (if (result (ref eq))
                   (ref.eq (call $integer? (local.get $x)) (global.get $true))
                   (then (if (result (ref eq))
                             (ref.eq (call $positive? (local.get $x)) (global.get $true))
                             (then (global.get $false))
                             (else (global.get $true))))
                   (else (global.get $false))))

         (func $nonnegative-integer? (type $Prim1)
               (param $x (ref eq))
               (result (ref eq))

               (if (result (ref eq))
                   (ref.eq (call $integer? (local.get $x)) (global.get $true))
                   (then (if (result (ref eq))
                             (ref.eq (call $negative? (local.get $x)) (global.get $true))
                             (then (global.get $false))
                             (else (global.get $true))))
                   (else (global.get $false))))

         (func $natural? (type $Prim1)
               (param $x (ref eq))
               (result (ref eq))

               (call $exact-nonnegative-integer? (local.get $x)))

         ;; 4.3.2 Generic Numerics
         ;;     https://docs.racket-lang.org/reference/generic-numbers.html

         ;; 4.3.2.1 Arithmetic
         ;;   https://docs.racket-lang.org/reference/generic-numbers.html#%28part._.Arithmetic%29

         ;; [/] +
         ;; [/] -
         ;; [/] *
         ;; [/] /
         ;; TODO  The functions + - * / currently uses 2 arguments. Make them variadic.
                  

         (func $quotient (type $Prim2)
               (param $n (ref eq))
               (param $m (ref eq))
               (result   (ref eq))

               (local $nu i32)
               (local $mu i32)
               (local $fl (ref $Flonum))
               (local $nf f64)
               (local $mf f64)
               (local $qf f64)

               ;; Case 1: both fixnums
               (if (ref.test (ref i31) (local.get $n))
                   (then (local.set $nu (i31.get_s (ref.cast i31ref (local.get $n))))
                         (if (i32.eqz (i32.and (local.get $nu) (i32.const 1)))
                             (then (if (ref.test (ref i31) (local.get $m))
                                       (then (local.set $mu (i31.get_s (ref.cast i31ref (local.get $m))))
                                             (if (i32.eqz (i32.and (local.get $mu) (i32.const 1)))
                                                 (then (return (call $fxquotient (local.get $n) (local.get $m)))))))))))

               ;; Case 2: flonum/inexact
               ;; convert n to f64
               (if (ref.test (ref $Flonum) (local.get $n))
                   (then (local.set $fl (ref.cast (ref $Flonum) (local.get $n)))
                         (local.set $nf (struct.get $Flonum $v (local.get $fl))))
                   (else (if (ref.test (ref i31) (local.get $n))
                             (then (local.set $nu (i31.get_s (ref.cast i31ref (local.get $n))))
                                   (if (i32.eqz (i32.and (local.get $nu) (i32.const 1)))
                                       (then (local.set $nf (f64.convert_i32_s (i32.shr_s (local.get $nu) (i32.const 1)))))
                                       (else (call $raise-expected-number (local.get $n)) (unreachable))))
                             (else (call $raise-expected-number (local.get $n)) (unreachable)))))

               ;; ensure n is a finite integer
               (if (f64.eq (local.get $nf) (f64.const inf))
                   (then (call $raise-expected-number (local.get $n)) (unreachable)))
               (if (f64.eq (local.get $nf) (f64.const -inf))
                   (then (call $raise-expected-number (local.get $n)) (unreachable)))
               (if (f64.eq (f64.floor (local.get $nf)) (local.get $nf))
                   (then)
                   (else (call $raise-expected-number (local.get $n)) (unreachable)))

               ;; convert m to f64
               (if (ref.test (ref $Flonum) (local.get $m))
                   (then (local.set $fl (ref.cast (ref $Flonum) (local.get $m)))
                         (local.set $mf (struct.get $Flonum $v (local.get $fl))))
                   (else (if (ref.test (ref i31) (local.get $m))
                             (then (local.set $mu (i31.get_s (ref.cast i31ref (local.get $m))))
                                   (if (i32.eqz (i32.and (local.get $mu) (i32.const 1)))
                                       (then (local.set $mf (f64.convert_i32_s (i32.shr_s (local.get $mu) (i32.const 1)))))
                                       (else (call $raise-expected-number (local.get $m)) (unreachable))))
                             (else (call $raise-expected-number (local.get $m)) (unreachable)))))

               ;; ensure m is a finite integer
               (if (f64.eq (local.get $mf) (f64.const inf))
                   (then (call $raise-expected-number (local.get $m)) (unreachable)))
               (if (f64.eq (local.get $mf) (f64.const -inf))
                   (then (call $raise-expected-number (local.get $m)) (unreachable)))
               (if (f64.eq (f64.floor (local.get $mf)) (local.get $mf))
                   (then)
                   (else (call $raise-expected-number (local.get $m)) (unreachable)))

               ;; divide by zero?
               (if (f64.eq (local.get $mf) (f64.const 0.0))
                   (then (call $raise-division-by-zero) (unreachable)))

               ;; compute quotient in flonum
               (local.set $qf (f64.trunc (f64.div (local.get $nf) (local.get $mf))))
               (return (struct.new $Flonum (i32.const 0) (local.get $qf))))


         (func $remainder (type $Prim2)
               (param $n (ref eq))
               (param $m (ref eq))
               (result   (ref eq))

               (local $nu i32)
               (local $mu i32)
               (local $fl (ref $Flonum))
               (local $nf f64)
               (local $mf f64)
               (local $rf f64)

               ;; Case 1: both fixnums
               (if (ref.test (ref i31) (local.get $n))
                   (then (local.set $nu (i31.get_s (ref.cast i31ref (local.get $n))))
                         (if (i32.eqz (i32.and (local.get $nu) (i32.const 1)))
                             (then (if (ref.test (ref i31) (local.get $m))
                                       (then (local.set $mu (i31.get_s (ref.cast i31ref (local.get $m))))
                                             (if (i32.eqz (i32.and (local.get $mu) (i32.const 1)))
                                                 (then (if (i32.eqz (local.get $mu))
                                                           (then (call $raise-division-by-zero) (unreachable)))
                                                       (return (ref.i31 (i32.rem_s (local.get $nu) (local.get $mu))))))))))))

               ;; Case 2: flonum/inexact
               ;; convert n to f64
               (if (ref.test (ref $Flonum) (local.get $n))
                   (then (local.set $fl (ref.cast (ref $Flonum) (local.get $n)))
                         (local.set $nf (struct.get $Flonum $v (local.get $fl))))
                   (else (if (ref.test (ref i31) (local.get $n))
                             (then (local.set $nu (i31.get_s (ref.cast i31ref (local.get $n))))
                                   (if (i32.eqz (i32.and (local.get $nu) (i32.const 1)))
                                       (then (local.set $nf (f64.convert_i32_s (i32.shr_s (local.get $nu) (i32.const 1)))))
                                       (else (call $raise-expected-number (local.get $n)) (unreachable))))
                             (else (call $raise-expected-number (local.get $n)) (unreachable)))))

               ;; ensure n is a finite integer
               (if (f64.eq (local.get $nf) (f64.const inf))
                   (then (call $raise-expected-number (local.get $n)) (unreachable)))
               (if (f64.eq (local.get $nf) (f64.const -inf))
                   (then (call $raise-expected-number (local.get $n)) (unreachable)))
               (if (f64.eq (f64.floor (local.get $nf)) (local.get $nf))
                   (then)
                   (else (call $raise-expected-number (local.get $n)) (unreachable)))

               ;; convert m to f64
               (if (ref.test (ref $Flonum) (local.get $m))
                   (then (local.set $fl (ref.cast (ref $Flonum) (local.get $m)))
                         (local.set $mf (struct.get $Flonum $v (local.get $fl))))
                   (else (if (ref.test (ref i31) (local.get $m))
                             (then (local.set $mu (i31.get_s (ref.cast i31ref (local.get $m))))
                                   (if (i32.eqz (i32.and (local.get $mu) (i32.const 1)))
                                       (then (local.set $mf (f64.convert_i32_s (i32.shr_s (local.get $mu) (i32.const 1)))))
                                       (else (call $raise-expected-number (local.get $m)) (unreachable))))
                             (else (call $raise-expected-number (local.get $m)) (unreachable)))))

               ;; ensure m is a finite integer
               (if (f64.eq (local.get $mf) (f64.const inf))
                   (then (call $raise-expected-number (local.get $m)) (unreachable)))
               (if (f64.eq (local.get $mf) (f64.const -inf))
                   (then (call $raise-expected-number (local.get $m)) (unreachable)))
               (if (f64.eq (f64.floor (local.get $mf)) (local.get $mf))
                   (then)
                   (else (call $raise-expected-number (local.get $m)) (unreachable)))

               ;; divide by zero?
               (if (f64.eq (local.get $mf) (f64.const 0.0))
                   (then (call $raise-division-by-zero) (unreachable)))

               ;; compute remainder in flonum
               (local.set $rf (f64.sub
                               (local.get $nf)
                               (f64.mul
                                (f64.trunc (f64.div (local.get $nf) (local.get $mf)))
                                (local.get $mf))))
                (return (struct.new $Flonum (i32.const 0) (local.get $rf))))

         (func $modulo (type $Prim2)
               (param $n (ref eq))
               (param $m (ref eq))
               (result (ref eq))

               (local $r  (ref eq))
               (local $ru i32)
               (local $mu i32)
               (local $fl (ref $Flonum))
               (local $mf f64)
               (local $rf f64)

               ;; Compute remainder; it validates numeric args and non-zero divisor.
               (local.set $r (call $remainder (local.get $n) (local.get $m)))
               ;; Convert m to f64 for sign checks; capture integer payload when fixnum.
               (if (ref.test (ref i31) (local.get $m))
                   (then
                    (local.set $mu (i31.get_s (ref.cast (ref i31) (local.get $m))))
                    ;; Ensure it's a fixnum (lsb=0), else raise (should be unreachable if
                    ;; $remainder already validated).
                    (if (i32.eqz (i32.and (local.get $mu) (i32.const 1)))
                        (then
                         (local.set $mf
                                    (f64.convert_i32_s
                                     (i32.shr_s (local.get $mu) (i32.const 1)))))
                        (else
                         (call $raise-expected-number (local.get $m))
                         (unreachable))))
                   (else
                    (local.set $fl (ref.cast (ref $Flonum) (local.get $m)))
                    (local.set $mf (struct.get $Flonum $v (local.get $fl)))))
               ;; Result expression: handle integer and flonum remainders.
               (if (result (ref eq))
                   (ref.test (ref i31) (local.get $r))
                   ;; --- Integer remainder case ------------------------------------------
                   (then
                    (block (result (ref eq))
                           (local.set $ru (i31.get_s (ref.cast (ref i31) (local.get $r))))
                           (if (result (ref eq))
                               (i32.eqz (local.get $ru))             ;; r == 0  → exact 0
                               (then (local.get $r))
                               ;; If r and m have same sign → r, else r + m (still a fixnum).
                               (else (if (result (ref eq))
                                         (i32.eq (i32.lt_s (local.get $ru) (i32.const 0))
                                                 (i32.lt_s (local.get $mu) (i32.const 0)))
                                         (then (local.get $r))
                                         (else (ref.i31
                                                (i32.add (local.get $ru) (local.get $mu)))))))))
                   ;; --- Flonum remainder case -------------------------------------------
                   (else
                    (block (result (ref eq))
                           (local.set $fl (ref.cast (ref $Flonum) (local.get $r)))
                           (local.set $rf (struct.get $Flonum $v (local.get $fl)))
                           (if (result (ref eq))
                               (f64.eq (local.get $rf) (f64.const 0.0))
                               ;; Preserve signed zero: 0.0 * mf carries mf's sign.
                               (then (struct.new $Flonum (i32.const 0)
                                                 (f64.mul (f64.const 0.0) (local.get $mf))))
                               ;; Non-zero: if signs match → r, else r + m.
                               (else (if (result (ref eq))
                                         (i32.eq (f64.lt (local.get $rf) (f64.const 0.0))
                                                 (f64.lt (local.get $mf) (f64.const 0.0)))
                                         (then (local.get $r))
                                         (else (struct.new $Flonum (i32.const 0)
                                                           (f64.add (local.get $rf) (local.get $mf)))))))))))


         (func $quotient/remainder (type $Prim2)
               (param $n (ref eq))
               (param $m (ref eq))
               (result   (ref eq))

               (local $nu i32)
               (local $mu i32)
               (local $q  i32)
               (local $r  i32)
               (local $fl (ref $Flonum))
               (local $nf f64)
               (local $mf f64)
               (local $qf f64)
               (local $rf f64)

               ;; Case 1: both fixnums
               (if (ref.test (ref i31) (local.get $n))
                   (then (local.set $nu (i31.get_s (ref.cast i31ref (local.get $n))))
                         (if (i32.eqz (i32.and (local.get $nu) (i32.const 1)))
                             (then (if (ref.test (ref i31) (local.get $m))
                                       (then (local.set $mu (i31.get_s (ref.cast i31ref (local.get $m))))
                                             (if (i32.eqz (i32.and (local.get $mu) (i32.const 1)))
                                                 (then (if (i32.eqz (local.get $mu))
                                                           (then (call $raise-division-by-zero) (unreachable)))
                                                      (local.set $q (i32.shl (i32.div_s (local.get $nu) (local.get $mu))
                                                                            (i32.const 1)))
                                                      (local.set $r (i32.rem_s (local.get $nu) (local.get $mu)))
                                                      (return (array.new_fixed $Values 2
                                                                              (ref.i31 (local.get $q))
                                                                              (ref.i31 (local.get $r))))))))))))

               ;; Case 2: flonum/inexact
               ;; convert n to f64
               (if (ref.test (ref $Flonum) (local.get $n))
                   (then (local.set $fl (ref.cast (ref $Flonum) (local.get $n)))
                         (local.set $nf (struct.get $Flonum $v (local.get $fl))))
                   (else (if (ref.test (ref i31) (local.get $n))
                             (then (local.set $nu (i31.get_s (ref.cast i31ref (local.get $n))))
                                   (if (i32.eqz (i32.and (local.get $nu) (i32.const 1)))
                                       (then (local.set $nf (f64.convert_i32_s (i32.shr_s (local.get $nu) (i32.const 1)))))
                                       (else (call $raise-expected-number (local.get $n)) (unreachable))))
                             (else (call $raise-expected-number (local.get $n)) (unreachable)))))

               ;; ensure n is a finite integer
               (if (f64.eq (local.get $nf) (f64.const inf))
                   (then (call $raise-expected-number (local.get $n)) (unreachable)))
               (if (f64.eq (local.get $nf) (f64.const -inf))
                   (then (call $raise-expected-number (local.get $n)) (unreachable)))
               (if (f64.eq (f64.floor (local.get $nf)) (local.get $nf))
                   (then)
                   (else (call $raise-expected-number (local.get $n)) (unreachable)))

               ;; convert m to f64
               (if (ref.test (ref $Flonum) (local.get $m))
                   (then (local.set $fl (ref.cast (ref $Flonum) (local.get $m)))
                         (local.set $mf (struct.get $Flonum $v (local.get $fl))))
                   (else (if (ref.test (ref i31) (local.get $m))
                             (then (local.set $mu (i31.get_s (ref.cast i31ref (local.get $m))))
                                   (if (i32.eqz (i32.and (local.get $mu) (i32.const 1)))
                                       (then (local.set $mf (f64.convert_i32_s (i32.shr_s (local.get $mu) (i32.const 1)))))
                                       (else (call $raise-expected-number (local.get $m)) (unreachable))))
                             (else (call $raise-expected-number (local.get $m)) (unreachable)))))

               ;; ensure m is a finite integer
               (if (f64.eq (local.get $mf) (f64.const inf))
                   (then (call $raise-expected-number (local.get $m)) (unreachable)))
               (if (f64.eq (local.get $mf) (f64.const -inf))
                   (then (call $raise-expected-number (local.get $m)) (unreachable)))
               (if (f64.eq (f64.floor (local.get $mf)) (local.get $mf))
                   (then)
                   (else (call $raise-expected-number (local.get $m)) (unreachable)))

               ;; divide by zero?
               (if (f64.eq (local.get $mf) (f64.const 0.0))
                   (then (call $raise-division-by-zero) (unreachable)))

               ;; compute quotient and remainder in flonum
               (local.set $qf (f64.trunc (f64.div (local.get $nf) (local.get $mf))))
               (local.set $rf (f64.sub (local.get $nf) (f64.mul (local.get $qf) (local.get $mf))))
               (return (array.new_fixed $Values 2
                                        (struct.new $Flonum (i32.const 0) (local.get $qf))
                                        (struct.new $Flonum (i32.const 0) (local.get $rf)))))
         

         (func $add1 (type $Prim1)
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

         (func $sub1 (type $Prim1)
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

         (func $sqr (type $Prim1)
               (param $x (ref eq))
               (result (ref eq))

               (local $bits i32)

               ;; Case 1: Fixnum
               (if (ref.test (ref i31) (local.get $x))
                   (then (local.set $bits (i31.get_u (ref.cast (ref i31) (local.get $x))))
                         (if (i32.eqz (i32.and (local.get $bits) (i32.const 1)))
                             (then (return (call $fx* (local.get $x) (local.get $x)))))))
               ;; Case 2: Flonum
               (if (ref.test (ref $Flonum) (local.get $x))
                   (then (return (call $fl* (local.get $x) (local.get $x)))))
               ;; Not a number
               (call $raise-expected-number (local.get $x))
               (unreachable))


         (func $sqrt (type $Prim1)
               (param $x (ref eq))
               (result (ref eq))

               (local $bits i32)
               (local $fl (ref $Flonum))
               (local $f64 f64)

               ;; If x is a fixnum, compute sqrt and return fixnum if integral
               (if (ref.test (ref i31) (local.get $x))
                   (then
                    (local.set $bits (i31.get_s (ref.cast (ref i31) (local.get $x))))
                    (if (i32.eqz (i32.and (local.get $bits) (i32.const 1)))
                        (then
                         (local.set $f64
                                    (call $js-math-sqrt
                                          (f64.convert_i32_s
                                           (i32.shr_s (local.get $bits) (i32.const 1)))))
                         (if (f64.eq (f64.floor (local.get $f64)) (local.get $f64))
                             (then
                              (local.set $bits (i32.trunc_f64_s (local.get $f64)))
                              (return (ref.i31 (i32.shl (local.get $bits) (i32.const 1)))))
                             (else
                              (return (struct.new $Flonum (i32.const 0) (local.get $f64))))))
                        (else (call $raise-expected-number (local.get $x))
                              (unreachable)))))

               ;; If x is a flonum, compute sqrt and box
               (if (ref.test (ref $Flonum) (local.get $x))
                   (then
                    (local.set $fl (ref.cast (ref $Flonum) (local.get $x)))
                    (local.set $f64 (struct.get $Flonum $v (local.get $fl)))
                    (return (struct.new $Flonum
                                        (i32.const 0)
                                        (call $js-math-sqrt (local.get $f64))))))

               ;; Not a number
               (call $raise-expected-number (local.get $x))
               (unreachable))


        ;; Generic numeric unary functions
        ;               name      kind    flonum-expr
        ; Implements: $abs $round $floor $ceiling $truncate
        ,@(let ([ops '((abs     fx-abs   (f64.abs     (local.get $f64)))
                       (round   fx-id    (f64.nearest (local.get $f64)))
                       (floor   fx-id    (f64.floor   (local.get $f64)))
                       (ceiling fx-id    (f64.ceil    (local.get $f64)))
                       (truncate fx-id   (f64.trunc   (local.get $f64))))])
            (for/list ([p ops])
              (define name (car p))
              (define kind (cadr p))
              (define expr (caddr p))
              (define fixstmts
                (case kind
                  [(fx-id)  `((return (local.get $x)))]
                  [(fx-abs) `((local.set $bits (i32.shr_s (local.get $bits) (i32.const 1)))
                              (if (i32.lt_s (local.get $bits) (i32.const 0))
                                  (then (local.set $bits (i32.sub (i32.const 0) (local.get $bits)))))
                              (if (i32.lt_u (local.get $bits) (i32.const 1073741824))
                                  (then (return (ref.i31 (i32.shl (local.get $bits) (i32.const 1)))))
                                  (else (return (struct.new $Flonum
                                                            (i32.const 0)
                                                            (f64.convert_i32_s (local.get $bits)))))))]))
              `(func ,(string->symbol (format "$~a" name))
                     (type $Prim1)
                     (param $x (ref eq))
                     (result (ref eq))

                     (local $bits i32)
                     (local $fl (ref $Flonum))
                     (local $f64 f64)

                     ;; Fixnum case
                     (if (ref.test (ref i31) (local.get $x))
                         (then
                          (local.set $bits (i31.get_s (ref.cast (ref i31) (local.get $x))))
                          (if (i32.eqz (i32.and (local.get $bits) (i32.const 1)))
                              (then ,@fixstmts)
                              (else (call $raise-expected-number (local.get $x))
                                    (unreachable)))))

                     ;; Flonum case
                     (if (ref.test (ref $Flonum) (local.get $x))
                         (then
                          (local.set $fl (ref.cast (ref $Flonum) (local.get $x)))
                          (local.set $f64 (struct.get $Flonum $v (local.get $fl)))
                          (return (struct.new $Flonum
                                              (i32.const 0)
                                              ,expr))))

                     ;; Not a number
                     (call $raise-expected-number (local.get $x))
                     (unreachable))))

        (func $sgn (type $Prim1)
              (param $x (ref eq))
              (result (ref eq))

              (local $bits i32)
              (local $fl   (ref $Flonum))
              (local $f64  f64)

              ;; Fixnum case
              (if (ref.test (ref i31) (local.get $x))
                  (then
                   (local.set $bits (i31.get_s (ref.cast (ref i31) (local.get $x))))
                   (if (i32.eqz (i32.and (local.get $bits) (i32.const 1)))
                       (then
                        (local.set $bits (i32.shr_s (local.get $bits) (i32.const 1)))
                        (if (i32.eqz (local.get $bits))
                            (then (return (local.get $x)))
                            (else (if (i32.gt_s (local.get $bits) (i32.const 0))
                                      (then (return ,(Imm 1)))
                                      (else (return ,(Imm -1)))))))
                       (else (call $raise-expected-number (local.get $x))
                             (unreachable)))))

              ;; Flonum case
              (if (ref.test (ref $Flonum) (local.get $x))
                  (then
                   (local.set $fl (ref.cast (ref $Flonum) (local.get $x)))
                   (local.set $f64 (struct.get $Flonum $v (local.get $fl)))
                   (if (f64.eq (local.get $f64) (f64.const 0.0))
                       (then (return (local.get $x)))
                       (else (if (f64.gt (local.get $f64) (f64.const 0.0))
                                 (then (return (struct.new $Flonum (i32.const 0) (f64.const 1.0))))
                                 (else (if (f64.lt (local.get $f64) (f64.const 0.0))
                                           (then (return (struct.new $Flonum (i32.const 0) (f64.const -1.0))))
                                           (else (return (struct.new $Flonum (i32.const 0)
                                                                     (f64.div (f64.const 0.0)
                                                                              (f64.const 0.0))))))))))))
                  
              ;; Not a number
              (call $raise-expected-number (local.get $x))
              (unreachable))

        ;; Exact integer rounding functions
        ;; Implements: $exact-round $exact-floor $exact-ceiling $exact-truncate
        ,@(let ([ops '((exact-round   f64.nearest)
                       (exact-floor   f64.floor)
                       (exact-ceiling f64.ceil)
                       (exact-truncate f64.trunc))])
            (for/list ([p ops])
              (define name (car p))
              (define expr (cadr p))
              `(func ,(string->symbol (format "$~a" name))
                     (type $Prim1)
                     (param $x (ref eq))
                     (result (ref eq))

                     (local $bits i32)
                     (local $fl (ref $Flonum))
                     (local $f64 f64)

                     ;; Fixnum case
                     (if (ref.test (ref i31) (local.get $x))
                         (then
                          (local.set $bits (i31.get_s (ref.cast (ref i31) (local.get $x))))
                          (if (i32.eqz (i32.and (local.get $bits) (i32.const 1)))
                              (then (return (local.get $x)))
                              (else (call $raise-expected-number (local.get $x))
                                    (unreachable)))))

                     ;; Flonum case
                     (if (ref.test (ref $Flonum) (local.get $x))
                         (then
                          (local.set $fl (ref.cast (ref $Flonum) (local.get $x)))
                          (local.set $f64 (struct.get $Flonum $v (local.get $fl)))
                          (if (f64.ne (local.get $f64) (local.get $f64))
                              (then (call $raise-expected-number (local.get $x)) (unreachable)))
                          (if (f64.eq (local.get $f64) (f64.const +inf))
                              (then (call $raise-expected-number (local.get $x)) (unreachable)))
                          (if (f64.eq (local.get $f64) (f64.const -inf))
                              (then (call $raise-expected-number (local.get $x)) (unreachable)))
                          (local.set $f64 (,expr (local.get $f64)))
                          (if (f64.gt (local.get $f64) (f64.const 1073741823.0))
                              (then (call $raise-expected-number (local.get $x)) (unreachable)))
                          (if (f64.lt (local.get $f64) (f64.const -1073741824.0))
                              (then (call $raise-expected-number (local.get $x)) (unreachable)))
                          (local.set $bits (i32.trunc_f64_s (local.get $f64)))
                          (return (ref.i31 (i32.shl (local.get $bits) (i32.const 1))))))

                     ;; Not a number
                     (call $raise-expected-number (local.get $x))
                     (unreachable))))

        ;; Trigonometric functions
        ;               name js            inbits outbits
        ,@(let ([ops '((sin  $js-math-sin  0      0)    ;  sin(0) = 0
                       (cos  $js-math-cos  0      2)    ;  cos(0) = 1
                       (tan  $js-math-tan  0      0)    ;  tan(0) = 0
                       (asin $js-math-asin 0      0)    ; asin(0) = 0
                       (acos $js-math-acos 2      0)    ; acos(1) = 0
                       (atan $js-math-atan 0      0)    ; atan(0) = 0
                       (sinh  $js-math-sinh  0      0) ;  sinh(0) = 0
                       (cosh  $js-math-cosh  0      2) ;  cosh(0) = 1
                       (tanh  $js-math-tanh  0      0) ;  tanh(0) = 0
                       (asinh $js-math-asinh 0      0) ; asinh(0) = 0
                       (acosh $js-math-acosh 2      0) ; acosh(1) = 0
                       (atanh $js-math-atanh 0      0))]) ; atanh(0) = 0
            ;; inbits and outbits are raw i31 fixnum encodings.
            ;; They mark trivial exact identities of trig functions
            ;; (e.g. sin 0 = 0, cos 0 = 1, acos 1 = 0).  
            ;; This avoids JS calls and flonum allocation in those cases.  
            (for/list ([p ops])
              (define name    (car p))
              (define js      (cadr p))
              (define inbits  (caddr p))
              (define outbits (cadddr p))
              `(func ,(string->symbol (format "$~a" name))
                     (type $Prim1)
                     (param $x (ref eq))
                     (result (ref eq))

                     (local $bits i32)
                     (local $fl (ref $Flonum))
                     (local $f64 f64)

                     (if (ref.test (ref i31) (local.get $x))
                         (then
                          (local.set $bits (i31.get_s (ref.cast (ref i31) (local.get $x))))
                          (if (i32.eqz (i32.and (local.get $bits) (i32.const 1)))
                              (then
                               (if (i32.eq (local.get $bits) (i32.const ,inbits))
                                   (then (return (ref.i31 (i32.const ,outbits)))))
                               (return (struct.new $Flonum
                                                   (i32.const 0)
                                                   (call ,js
                                                         (f64.convert_i32_s
                                                          (i32.shr_s (local.get $bits) (i32.const 1)))))))
                              (else (call $raise-expected-number (local.get $x))
                                    (unreachable)))))

                     (if (ref.test (ref $Flonum) (local.get $x))
                         (then
                          (local.set $fl (ref.cast (ref $Flonum) (local.get $x)))
                          (local.set $f64 (struct.get $Flonum $v (local.get $fl)))
                          (return (struct.new $Flonum
                                               (i32.const 0)
                                               (call ,js (local.get $f64))))))

                     (call $raise-expected-number (local.get $x))
                     (unreachable))))

        ;; Angle conversion functions
        ,@(let ([ops '((degrees->radians 0.017453292519943295)
                       (radians->degrees 57.29577951308232))])
            (for/list ([p ops])
              (define name (car p))
              (define factor (cadr p))
              `(func ,(string->symbol (format "$~a" name))
                     (type $Prim1)
                     (param $x (ref eq))
                     (result (ref eq))

                     (local $bits i32)
                     (local $fl (ref $Flonum))
                     (local $f64 f64)

                     (if (ref.test (ref i31) (local.get $x))
                         (then
                          (local.set $bits (i31.get_s (ref.cast (ref i31) (local.get $x))))
                          (if (i32.eqz (i32.and (local.get $bits) (i32.const 1)))
                              (then
                               (local.set $f64
                                          (f64.mul
                                           (f64.convert_i32_s
                                            (i32.shr_s (local.get $bits) (i32.const 1)))
                                           (f64.const ,factor)))
                               (return (struct.new $Flonum
                                                   (i32.const 0)
                                                   (local.get $f64))))
                              (else (call $raise-expected-number (local.get $x))
                                    (unreachable)))))

                     (if (ref.test (ref $Flonum) (local.get $x))
                         (then
                          (local.set $fl (ref.cast (ref $Flonum) (local.get $x)))
                          (local.set $f64 (struct.get $Flonum $v (local.get $fl)))
                          (return (struct.new $Flonum
                                               (i32.const 0)
                                               (f64.mul (local.get $f64) (f64.const ,factor))))))

                    (call $raise-expected-number (local.get $x))
                    (unreachable))))

        (func $order-of-magnitude (type $Prim1)
              (param $r (ref eq))
              (result (ref eq))

              (local $bits i32)
              (local $i i32)
              (local $fl (ref $Flonum))
              (local $f64 f64)
              (local $m i32)
              (local $p f64)
              (local $u f64)
              (local $ok i32)

              (local.set $ok (i32.const 0))

              (if (ref.test (ref i31) (local.get $r))
                  (then
                   (local.set $bits (i31.get_s (ref.cast (ref i31) (local.get $r))))
                   (if (i32.eqz (i32.and (local.get $bits) (i32.const 1)))
                       (then
                        (local.set $i (i32.shr_s (local.get $bits) (i32.const 1)))
                        (if (i32.gt_s (local.get $i) (i32.const 0))
                            (then
                             (local.set $f64 (f64.convert_i32_s (local.get $i)))
                             (local.set $ok (i32.const 1)))
                            (else (call $raise-argument-error (local.get $r))
                                  (unreachable))))
                       (else (call $raise-argument-error (local.get $r))
                             (unreachable)))))

              (if (i32.eqz (local.get $ok))
                  (then
                   (if (ref.test (ref $Flonum) (local.get $r))
                       (then
                        (local.set $fl (ref.cast (ref $Flonum) (local.get $r)))
                        (local.set $f64 (struct.get $Flonum $v (local.get $fl)))
                        (if (f64.le (local.get $f64) (f64.const 0))
                            (then (call $raise-argument-error (local.get $r))
                                  (unreachable)))
                        (if (f64.eq (local.get $f64) (f64.const inf))
                            (then (call $raise-argument-error (local.get $r))
                                  (unreachable)))
                        (if (f64.ne (local.get $f64) (local.get $f64))
                            (then (call $raise-argument-error (local.get $r))
                                  (unreachable)))
                        (local.set $ok (i32.const 1))))))

              (if (i32.eqz (local.get $ok))
                  (then (call $raise-argument-error (local.get $r))
                        (unreachable)))

              (local.set $m
                         (i32.trunc_f64_s
                          (f64.floor
                           (f64.mul
                            (call $js-math-log (local.get $f64))
                            (f64.const 0.4342944819032518)))))
              (local.set $p
                         (call $js-math-pow
                               (f64.const 10)
                               (f64.convert_i32_s (local.get $m))))

              (block $down
                     (loop $loop-down
                           (br_if $down (f64.le (local.get $p) (local.get $f64)))
                           (local.set $m (i32.sub (local.get $m) (i32.const 1)))
                           (local.set $p (f64.mul (local.get $p) (f64.const 0.1)))
                           (br $loop-down)))

              (local.set $u (f64.mul (local.get $p) (f64.const 10)))

              (block $up
                     (loop $loop-up
                           (br_if $up (f64.lt (local.get $f64) (local.get $u)))
                           (local.set $m (i32.add (local.get $m) (i32.const 1)))
                           (local.set $p (local.get $u))
                           (local.set $u (f64.mul (local.get $u) (f64.const 10)))
                           (br $loop-up)))

              (ref.i31 (i32.shl (local.get $m) (i32.const 1))))

        (func $raise-expected-number (unreachable))

         ,@(let ()
             (define (binop $+ $fx+ $fl+)
               `(func ,$+ (type $Prim2)
                      (param $x (ref eq))
                      (param $y (ref eq))
                      (result   (ref eq))
                      
                      (if (result (ref eq)) (call $fx?/i32 (local.get $x))
                          (then (if (result (ref eq)) (call $fx?/i32 (local.get $y))
                                    (then (call ,$fx+
                                                (local.get $x) (local.get $y)))
                                    (else (if (result (ref eq)) (call $fl?/i32 (local.get $y))
                                              (then (call ,$fl+
                                                          (call $fx->fl/precise (local.get $x)) (local.get $y)))
                                              (else (call $raise-expected-number)
                                                    (unreachable))))))
                          (else (if (result (ref eq)) (call $fl?/i32 (local.get $x))
                                    (then (if (result (ref eq)) (call $fl?/i32 (local.get $y))
                                              (then (call ,$fl+
                                                          (local.get $x) (local.get $y)))
                                              (else (if (result (ref eq)) (call $fx?/i32 (local.get $y))
                                                        (then (call ,$fl+
                                                                    (local.get $x) (call $fx->fl/precise (local.get $y))))
                                                        (else (call $raise-expected-number)
                                                              (unreachable))))))
                                    (else (call $raise-expected-number)
                                          (unreachable)))))))
             (list (binop '$+ '$fx+ '$fl+)
                   (binop '$- '$fx- '$fl-)
                   (binop '$* '$fx* '$fl*)))

         ;; Note: fx/ doesn't exist, but fxquotient do.

         (func $/ (type $Prim2)
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
                              (else (call $fx->fl/precise (local.get $x)))))
               ;; --- Convert $y to flonum if needed ---
               (local.set $y/fl
                          (if (result (ref $Flonum))
                              (ref.test (ref $Flonum) (local.get $y))
                              (then (ref.cast (ref $Flonum) (local.get $y)))
                              (else (call $fx->fl/precise (local.get $y)))))
               ;; --- Divide using $fl/ ---
               (call $fl/ (local.get $x/fl) (local.get $y/fl)))

         (func $gcd/2 (type $Prim2)
               (param $n (ref eq))
               (param $m (ref eq))
               (result (ref eq))

               (local $tmp (ref eq))

               (local.set $n (call $abs (local.get $n)))
               (local.set $m (call $abs (local.get $m)))
               (block $done
                      (loop $loop
                            (br_if $done (ref.eq (call $zero? (local.get $m)) (global.get $true)))
                            (local.set $tmp (local.get $m))
                            (local.set $m (call $remainder (local.get $n) (local.get $m)))
                            (local.set $n (local.get $tmp))
                            (br $loop)))
               (local.get $n))

         (func $gcd (type $Prim>=0)
               (param $xs0 (ref eq))
               (result (ref eq))

               (local $xs   (ref eq))
               (local $node (ref $Pair))
               (local $v    (ref eq))
               (local $r    (ref eq))

               (local.set $xs
                          (if (result (ref eq))
                              (ref.test (ref $Args) (local.get $xs0))
                              (then (call $rest-arguments->list
                                          (ref.cast (ref $Args) (local.get $xs0))
                                          (i32.const 0)))
                              (else (local.get $xs0))))
               (local.set $r ,(Imm 0))
               (block $done
                      (loop $loop
                            (br_if $done (ref.eq (local.get $xs) (global.get $null)))
                            (local.set $node (ref.cast (ref $Pair) (local.get $xs)))
                            (local.set $v    (struct.get $Pair $a (local.get $node)))
                            (local.set $r    (call $gcd/2 (local.get $r) (local.get $v)))
                            (local.set $xs   (struct.get $Pair $d (local.get $node)))
                            (br $loop)))
               (local.get $r))
         
         (func $lcm/2 (type $Prim2)
               (param $n (ref eq))
               (param $m (ref eq))
               (result (ref eq))

               (local $g (ref eq))

               ;; Work with absolute values.
               (local.set $n (call $abs (local.get $n)))
               (local.set $m (call $abs (local.get $m)))

               ;; Expression style: the top-level IF yields the final (ref eq).
               (if (result (ref eq))
                   (ref.eq (call $zero? (local.get $n)) (global.get $true))
                   ;; THEN: n = 0
                   (then (if (result (ref eq))
                             (ref.eq (call $zero? (local.get $m)) (global.get $true))
                             ;; both zero: prefer inexact if any is inexact
                             (then (if (result (ref eq))
                                       (ref.eq (call $exact? (local.get $n)) (global.get $true))
                                       (then (if (result (ref eq))
                                                 (ref.eq (call $exact? (local.get $m)) (global.get $true))
                                                 (then (local.get $n))                    ;; both exact zeros → n (exact 0)
                                                 (else (local.get $m))))                  ;; m is inexact zero → m
                                       (else (local.get $n))))                            ;; n is inexact zero → n
                             ;; n = 0, m ≠ 0 → 0 (preserve n’s zero)
                             (else (local.get $n))))
                   ;; ELSE: n ≠ 0
                   (else (if (result (ref eq))
                             (ref.eq (call $zero? (local.get $m)) (global.get $true))
                             ;; m = 0, n ≠ 0 → 0 (preserve m’s zero)
                             (then (local.get $m))
                             ;; general case: lcm(n,m) = (n / gcd(n,m)) * m
                             (else (block (result (ref eq))
                                          (local.set $g (call $gcd/2 (local.get $n) (local.get $m)))
                                          (call $*
                                                (if (result (ref eq))
                                                    (i32.and (call $fx?/i32 (local.get $n))
                                                             (call $fx?/i32 (local.get $g)))
                                                    (then (call $quotient (local.get $n)
                                                                          (local.get $g)))
                                                    (else (call $/ (local.get $n)
                                                                  (local.get $g))))
                                                (local.get $m))))))))



         (func $lcm (type $Prim>=0)
               (param $xs0 (ref eq))
               (result (ref eq))

               (local $xs   (ref eq))
               (local $node (ref $Pair))
               (local $v    (ref eq))
               (local $r    (ref eq))

               (local.set $xs
                          (if (result (ref eq))
                              (ref.test (ref $Args) (local.get $xs0))
                              (then (call $rest-arguments->list
                                          (ref.cast (ref $Args) (local.get $xs0))
                                          (i32.const 0)))
                              (else (local.get $xs0))))
               (local.set $r ,(Imm 1))
               (block $done
                      (loop $loop
                            (br_if $done (ref.eq (local.get $xs) (global.get $null)))
                            (local.set $node (ref.cast (ref $Pair) (local.get $xs)))
                            (local.set $v    (struct.get $Pair $a (local.get $node)))
                            (local.set $r    (call $lcm/2 (local.get $r) (local.get $v)))
                            (local.set $xs   (struct.get $Pair $d (local.get $node)))
                            (br $loop)))
               (local.get $r))

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
                          (else (local.set $x-fl (call $fx->fl/precise (local.get $x)))))
                      (if (local.get $y/is-fl)
                          (then (local.set $y-fl (ref.cast (ref $Flonum) (local.get $y))))
                          (else (local.set $y-fl (call $fx->fl/precise (local.get $y)))))

                      (call ,flcmp
                            (ref.as_non_null (local.get $x-fl))
                            (ref.as_non_null (local.get $y-fl)))))

             (list (gencmp '$=  '$fx=/2  '$fl=)   ; maybe specialize this one?
                   (gencmp '$<  '$fx</2  '$fl<)
                   (gencmp '$>  '$fx>/2  '$fl>)
                   (gencmp '$<= '$fx<=/2 '$fl<=)
                     (gencmp '$>= '$fx>=/2 '$fl>=)))

        ,@(let ()
            (define (gen-bitop name fxop)
              (define $name/2 (string->symbol (~a "$" name "/2")))
              (define $name   (string->symbol (~a "$" name)))
              `((func ,$name/2 (type $Prim2)
                      (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
                      (call ,fxop (local.get $x) (local.get $y)))
                (func ,$name (type $Prim>=1)
                      (param $x0 (ref eq)) (param $xs0 (ref eq)) (result (ref eq))
                      (local $xs   (ref eq))
                      (local $node (ref $Pair))
                      (local $v    (ref eq))
                      (local $r    (ref eq))
                      (local.set $xs
                                 (if (result (ref eq))
                                     (ref.test (ref $Args) (local.get $xs0))
                                     (then (call $rest-arguments->list
                                                 (ref.cast (ref $Args) (local.get $xs0))
                                                 (i32.const 0)))
                                     (else (local.get $xs0))))
                      (local.set $r (local.get $x0))
                      (block $done
                             (loop $loop
                                   (br_if $done (ref.eq (local.get $xs) (global.get $null)))
                                   (local.set $node (ref.cast (ref $Pair) (local.get $xs)))
                                   (local.set $v    (struct.get $Pair $a (local.get $node)))
                                   (local.set $r    (call ,$name/2 (local.get $r) (local.get $v)))
                                   (local.set $xs   (struct.get $Pair $d (local.get $node)))
                                   (br $loop)))
                      (local.get $r))))
            (append (gen-bitop 'bitwise-and '$fxand/2)
                    (gen-bitop 'bitwise-ior '$fxior/2)
                    (gen-bitop 'bitwise-xor '$fxxor/2)))


        (func $integer-length (type $Prim1)
              (param $n (ref eq))
              (result (ref eq))

              (local $bits i32)
              (local $n/fx i32)
              (local $len i32)

              ;; Validate: must be a fixnum (ref i31 with lsb = 0); otherwise raise.
              (if (result (ref eq))
                  (ref.test (ref i31) (local.get $n))
                  (then
                   (local.set $bits (i31.get_u (ref.cast (ref i31) (local.get $n))))
                   (if (result (ref eq))
                       (i32.eqz (i32.and (local.get $bits) (i32.const 1)))
                       (then
                        ;; Extract unboxed signed i32 from fixnum: (bits << 1) >> 2.
                        (local.set $n/fx
                                   (i32.shr_s
                                    (i32.shl (local.get $bits) (i32.const 1))
                                    (i32.const 2)))
                        ;; Compute integer-length per Racket’s definition.
                        (if (result (ref eq))
                            (i32.ge_s (local.get $n/fx) (i32.const 0))
                            (then
                             (if (result (ref eq))
                                 (i32.eqz (local.get $n/fx))
                                 (then
                                  (ref.i31 (i32.const 0)))
                                 (else
                                  (local.set $len
                                             (i32.sub
                                              (i32.const 32)
                                              (i32.clz (local.get $n/fx))))
                                  (ref.i31 (i32.shl (local.get $len) (i32.const 1))))))
                            (else
                             ;; For negatives, use bit-length of (~n).
                             (local.set $len
                                        (i32.sub
                                         (i32.const 32)
                                         (i32.clz
                                          (i32.xor (local.get $n/fx) (i32.const -1)))))
                             (ref.i31 (i32.shl (local.get $len) (i32.const 1))))))
                       (else
                        (call $raise-expected-number (local.get $n))
                        (unreachable))))
                  (else
                   (call $raise-expected-number (local.get $n))
                   (unreachable))))



         ;;;
         ;;;  4.3.4 Fixnums
         ;;;

         ;; https://docs.racket-lang.org/reference/fixnums.html

         (func $raise-not-fixnum (param $x (ref eq)) (unreachable))

         (func $fixnum? (type $Prim1) (param $v (ref eq)) (result (ref eq))
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

         
         (func $fxzero? (type $Prim1) (param $x (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.test i31ref (local.get $x))
                   (then (if (result (ref eq))
                             (i32.eqz (i31.get_s (ref.cast i31ref (local.get $x))))
                             (then (global.get $true))
                             (else (global.get $false))))
                   (else (global.get $false))))

         (func $fx+ (type $Prim2)
               (param $x (ref eq))
               (param $y (ref eq))
               (result   (ref eq))
               (ref.i31 (i32.add (i31.get_s (ref.cast i31ref (local.get $x)))
                                 (i31.get_s (ref.cast i31ref (local.get $y))))))
         
         (func $fx- (type $Prim2) (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
               (ref.i31 (i32.sub (i31.get_s (ref.cast i31ref (local.get $x)))
                                 (i31.get_s (ref.cast i31ref (local.get $y))))))
         ; Since an integer n is represented as 2n, we need to halve one argument. 
         (func $fx* (type $Prim2) (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
               (ref.i31 (i32.mul (i31.get_s (ref.cast i31ref (local.get $x)))
                                 ,(Half `(i31.get_s (ref.cast i31ref (local.get $y)))))))

         (func $fx/ (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
               (ref.i31 (i32.div_s (i31.get_s (ref.cast i31ref (local.get $x)))
                                   ,(Double `(i31.get_s (ref.cast i31ref (local.get $y)))))))

         (func $unsafe-fxquotient (type $Prim2)
               (param $x (ref eq)) (param $y (ref eq)) (result   (ref eq))               
               (ref.i31 ,(Double `(i32.div_s ,(Half `(i31.get_s (ref.cast i31ref (local.get $x))))
                                             ,(Half `(i31.get_s (ref.cast i31ref (local.get $y))))))))

         (func $raise-division-by-zero (unreachable))
         
         (func $fxquotient (type $Prim2)
               (param $x (ref eq))
               (param $y (ref eq))
               (result   (ref eq))

               (local $xu i32) (local $yu i32)     ;; raw tagged bits
               (local $xi i32) (local $yi i32)     ;; untagged i32s
               (local $q  i32)                     ;; quotient

               ;; --- check $x is a fixnum ---
               (if (i32.eqz (ref.test (ref i31) (local.get $x)))
                   (then (call $raise-check-fixnum (local.get $x)) (unreachable)))
               (local.set $xu (i31.get_s (ref.cast (ref i31) (local.get $x))))
               (if (i32.and (local.get $xu) (i32.const 1))
                   (then (call $raise-check-fixnum (local.get $x)) (unreachable)))
               (local.set $xi (i32.shr_s (local.get $xu) (i32.const 1)))
               ;; --- check $y is a fixnum ---
               (if (i32.eqz (ref.test (ref i31) (local.get $y)))
                   (then (call $raise-check-fixnum (local.get $y)) (unreachable)))
               (local.set $yu (i31.get_s (ref.cast (ref i31) (local.get $y))))
               (if (i32.and (local.get $yu) (i32.const 1))
                   (then (call $raise-check-fixnum (local.get $y)) (unreachable)))
               (local.set $yi (i32.shr_s (local.get $yu) (i32.const 1)))
               ;; --- divide by zero? ---
               (if (i32.eqz (local.get $yi))
                   (then (call $raise-division-by-zero) (unreachable)))
               ;; --- compute truncating quotient ---
               (local.set $q (i32.div_s (local.get $xi) (local.get $yi)))
               ;; --- (optional) fixnum-range check: [-2^29, 2^29-1] ---
               ;; uncomment if you want to signal overflow instead of wrapping
               ;; (if (i32.or
               ;;       (i32.lt_s (local.get $q) (i32.const -536870912)) ;; -2^29
               ;;       (i32.gt_s (local.get $q) (i32.const  536870911))) ;;  2^29-1
               ;;     (then (call $raise-fixnum-overflow) (unreachable)))
               ;; --- re-tag as fixnum ---
               (ref.i31 (i32.shl (local.get $q) (i32.const 1))))

         (func $fxremainder (type $Prim2)
               (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
               (ref.i31 (i32.rem_s (i31.get_s (ref.cast i31ref (local.get $x)))
                                    (i31.get_s (ref.cast i31ref (local.get $y))))))

         (func $fxmodulo (type $Prim2)
               (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
               (local $xv i32) (local $yv i32) (local $r i32)
               (local.set $xv (i31.get_s (ref.cast i31ref (local.get $x))))
               (local.set $yv (i31.get_s (ref.cast i31ref (local.get $y))))
               (local.set $r  (i32.rem_s (local.get $xv) (local.get $yv)))
               (if (result (ref eq))
                   (i32.eqz (local.get $r))
                   (then (ref.i31 (local.get $r)))
                   (else (if (result (ref eq))
                              (i32.eq (i32.lt_s (local.get $r) (i32.const 0))
                                      (i32.lt_s (local.get $yv) (i32.const 0)))
                              (then (ref.i31 (local.get $r)))
                              (else (ref.i31 (i32.add (local.get $r) (local.get $yv))))))))

         (func $fxabs (type $Prim1)
               (param $x (ref eq)) (result (ref eq))
               (local $xi i32)
               (local.set $xi (i31.get_s (ref.cast i31ref (local.get $x))))
               (ref.i31 (if (result i32)
                            (i32.lt_s (local.get $xi) (i32.const 0))
                            (then (i32.sub (i32.const 0) (local.get $xi)))
                            (else (local.get $xi)))))

         (func $fxand/2 (type $Prim2)
               (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
               (ref.i31 (i32.and (i31.get_s (ref.cast i31ref (local.get $x)))
                                 (i31.get_s (ref.cast i31ref (local.get $y))))))

         (func $fxand (type $Prim>=0)
               (param $xs0 (ref eq)) (result (ref eq))
               (local $xs   (ref eq))
               (local $node (ref $Pair))
               (local $v    (ref eq))
               (local $r    (ref eq))
               (local.set $xs
                          (if (result (ref eq))
                              (ref.test (ref $Args) (local.get $xs0))
                              (then (call $rest-arguments->list
                                          (ref.cast (ref $Args) (local.get $xs0))
                                          (i32.const 0)))
                              (else (local.get $xs0))))
               (local.set $r ,(Imm -1))
               (block $done
                      (loop $loop
                            (br_if $done (ref.eq (local.get $xs) (global.get $null)))
                            (local.set $node (ref.cast (ref $Pair) (local.get $xs)))
                            (local.set $v    (struct.get $Pair $a (local.get $node)))
                            (local.set $r    (call $fxand/2 (local.get $r) (local.get $v)))
                            (local.set $xs   (struct.get $Pair $d (local.get $node)))
                            (br $loop)))
               (local.get $r))

         (func $fxior/2 (type $Prim2)
               (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
               (ref.i31 (i32.or (i31.get_s (ref.cast i31ref (local.get $x)))
                                (i31.get_s (ref.cast i31ref (local.get $y))))))

         (func $fxior (type $Prim>=0)
               (param $xs0 (ref eq)) (result (ref eq))
               (local $xs   (ref eq))
               (local $node (ref $Pair))
               (local $v    (ref eq))
               (local $r    (ref eq))
               (local.set $xs
                          (if (result (ref eq))
                              (ref.test (ref $Args) (local.get $xs0))
                              (then (call $rest-arguments->list
                                          (ref.cast (ref $Args) (local.get $xs0))
                                          (i32.const 0)))
                              (else (local.get $xs0))))
               (local.set $r (global.get $zero))
               (block $done
                      (loop $loop
                            (br_if $done (ref.eq (local.get $xs) (global.get $null)))
                            (local.set $node (ref.cast (ref $Pair) (local.get $xs)))
                            (local.set $v    (struct.get $Pair $a (local.get $node)))
                            (local.set $r    (call $fxior/2 (local.get $r) (local.get $v)))
                            (local.set $xs   (struct.get $Pair $d (local.get $node)))
                            (br $loop)))
               (local.get $r))

         (func $fxxor/2 (type $Prim2)
               (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
               (ref.i31 (i32.xor (i31.get_s (ref.cast i31ref (local.get $x)))
                                 (i31.get_s (ref.cast i31ref (local.get $y))))))

         (func $fxxor (type $Prim>=0)
               (param $xs0 (ref eq)) (result (ref eq))
               (local $xs   (ref eq))
               (local $node (ref $Pair))
               (local $v    (ref eq))
               (local $r    (ref eq))
               (local.set $xs
                          (if (result (ref eq))
                              (ref.test (ref $Args) (local.get $xs0))
                              (then (call $rest-arguments->list
                                          (ref.cast (ref $Args) (local.get $xs0))
                                          (i32.const 0)))
                              (else (local.get $xs0))))
               (local.set $r (global.get $zero))
               (block $done
                      (loop $loop
                            (br_if $done (ref.eq (local.get $xs) (global.get $null)))
                            (local.set $node (ref.cast (ref $Pair) (local.get $xs)))
                            (local.set $v    (struct.get $Pair $a (local.get $node)))
                            (local.set $r    (call $fxxor/2 (local.get $r) (local.get $v)))
                            (local.set $xs   (struct.get $Pair $d (local.get $node)))
                            (br $loop)))
               (local.get $r))

         (func $fxnot (type $Prim1)
               (param $x (ref eq)) (result (ref eq))
               (ref.i31 (i32.shl (i32.xor ,(Half `(i31.get_s (ref.cast i31ref (local.get $x))))
                                          (i32.const -1))
                                 (i32.const 1))))

         (func $fxlshift (type $Prim2)
               (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
               (ref.i31 (i32.shl (i31.get_s (ref.cast i31ref (local.get $x)))
                                 ,(Half `(i31.get_s (ref.cast i31ref (local.get $y)))))))

         (func $fxrshift (type $Prim2)
               (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
               (ref.i31 (i32.shr_s (i31.get_s (ref.cast i31ref (local.get $x)))
                                   ,(Half `(i31.get_s (ref.cast i31ref (local.get $y)))))))

         (func $fxpopcount (type $Prim1)
               (param $x (ref eq)) (result (ref eq))
               (ref.i31 (i32.shl (i32.popcnt ,(Half `(i31.get_u (ref.cast i31ref (local.get $x)))))
                                 (i32.const 1))))

         (func $fxpopcount32 (type $Prim1)
               (param $x (ref eq)) (result (ref eq))
               (ref.i31 (i32.shl (i32.popcnt ,(Half `(i31.get_u (ref.cast i31ref (local.get $x)))))
                                 (i32.const 1))))

         (func $fxpopcount16 (type $Prim1)
               (param $x (ref eq)) (result (ref eq))
               (ref.i31 (i32.shl (i32.popcnt (i32.and ,(Half `(i31.get_u (ref.cast i31ref (local.get $x))))
                                                     (i32.const 65535)))
                                 (i32.const 1))))

         (func $fx+/wraparound (type $Prim2)
               (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
               (ref.i31 (i32.add (i31.get_s (ref.cast i31ref (local.get $x)))
                                 (i31.get_s (ref.cast i31ref (local.get $y))))))

         (func $fx-/wraparound (type $Prim>=1)
               (param $a1   (ref eq))
               (param $rest (ref eq))
               (result      (ref eq))

               (local $a    (ref eq))
               (local $b    (ref eq))
               (local $node (ref $Pair))

               ;; Eager init so locals are definitely assigned before any possible get.
               (local.set $a (ref.i31 (i32.const 0)))
               (local.set $b (local.get $a1))
               ;; Determine arguments based on rest list
               (if (ref.eq (local.get $rest) (global.get $null))
                   (then
                    ;; (0 - a1)
                    (local.set $a (ref.i31 (i32.const 0)))
                    (local.set $b (local.get $a1)))
                   (else
                    (if (ref.test (ref $Pair) (local.get $rest))
                        (then
                         (local.set $node (ref.cast (ref $Pair) (local.get $rest)))
                         (local.set $a    (local.get $a1))
                         (local.set $b    (struct.get $Pair $a (local.get $node)))
                         ;; Ensure no extra arguments
                         (if (ref.eq (struct.get $Pair $d (local.get $node))
                                     (global.get $null))
                             (then (nop))
                             (else (call $raise-arity-error:exactly) (unreachable))))
                        (else (call $raise-arity-error:exactly) (unreachable)))))
               (ref.i31
                (i32.sub
                 (i31.get_s (ref.cast (ref i31) (local.get $a)))
                 (i31.get_s (ref.cast (ref i31) (local.get $b))))))



         (func $fx*/wraparound (type $Prim2)
               (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
               (ref.i31 (i32.mul (i31.get_s (ref.cast i31ref (local.get $x)))
                                 ,(Half `(i31.get_s (ref.cast i31ref (local.get $y)))))))

         (func $fxlshift/wraparound (type $Prim2)
               (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
               (ref.i31 (i32.shl (i31.get_s (ref.cast i31ref (local.get $x)))
                                 ,(Half `(i31.get_s (ref.cast i31ref (local.get $y)))))))

         (func $fxrshift/logical (type $Prim2)
               (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
               (ref.i31
                ,(Double
                  `(i32.shr_u 
                    ,(Half `(i31.get_u (ref.cast (ref i31) (local.get $x))))
                    ,(Half `(i31.get_u (ref.cast (ref i31) (local.get $y))))))))

         (func $most-positive-fixnum (type $Prim0) (result (ref eq))
               ,(Imm most-positive-fixnum))

         (func $most-negative-fixnum (type $Prim0) (result (ref eq))
               ,(Imm most-negative-fixnum))

         (func $fx=/2 (type $Prim2) (param $v1 (ref eq)) (param $v2 (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.eq (call $fixnum? (local.get $v1))
                           (global.get $true))
                   (then (return_call $eq? (local.get $v1) (local.get $v2)))
                   (else (global.get $false))))

         ,@(for/list ([$fx-cmp '($fx</2   $fx>/2   $fx<=/2  $fx>=/2)]
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

         ,@(for/list ([$cmp   (in-list '($fx=   $fx<   $fx>   $fx<=   $fx>=))]
                      [$cmp/2 (in-list '($fx=/2 $fx</2 $fx>/2 $fx<=/2 $fx>=/2))])
             `(func ,$cmp (param $x0 (ref eq)) (param $xs (ref eq)) (result (ref eq))
                    (local $node (ref $Pair))
                    (local $fx   (ref eq))
                    ;; Validate the first argument
                    (if (i32.eqz (ref.test (ref i31) (local.get $x0)))
                        (then (call $raise-check-fixnum (local.get $x0)) (unreachable)))
                    (block $done
                           (loop $loop
                                 (br_if $done (ref.eq (local.get $xs) (global.get $null)))
                                 (local.set $node (ref.cast (ref $Pair) (local.get $xs)))
                                 (local.set $fx   (struct.get $Pair $a (local.get $node)))
                                 (if (i32.eqz (ref.test (ref i31) (local.get $fx)))
                                     (then (call $raise-check-fixnum (local.get $fx)) (unreachable)))
                                 (if (ref.eq (call ,$cmp/2 (local.get $x0) (local.get $fx))
                                             (global.get $false))
                                     (then (return (global.get $false))))
                                 (local.set $x0 (local.get $fx))
                                 (local.set $xs (struct.get $Pair $d (local.get $node)))
                                 (br $loop)))
                    (global.get $true)))

        ,@(for/list ([name '( $fxmin/2 $fxmax/2 )]
                     [inst '( i32.lt_s i32.gt_s )])
            `(func ,name (type $Prim2)
                   (param $x (ref eq))
                   (param $y (ref eq))
                   (result (ref eq))
                   (if (i32.eqz (ref.test (ref i31) (local.get $x)))
                       (then (call $raise-check-fixnum (local.get $x)) (unreachable)))
                   (if (i32.eqz (ref.test (ref i31) (local.get $y)))
                       (then (call $raise-check-fixnum (local.get $y)) (unreachable)))
                   (if (result (ref eq))
                       (,inst (i31.get_s (ref.cast (ref i31) (local.get $x)))
                              (i31.get_s (ref.cast (ref i31) (local.get $y))))
                       (then (local.get $x))
                       (else (local.get $y)))))

        ,@(for/list ([name '( $fxmin $fxmax )]
                     [cmp  '( $fxmin/2 $fxmax/2 )])
            `(func ,name (param $x0 (ref eq)) (param $xs (ref eq)) (result (ref eq))
                   (local $node (ref $Pair))
                   (local $fx   (ref eq))
                   (local $best (ref eq))
                   (if (i32.eqz (ref.test (ref i31) (local.get $x0)))
                       (then (call $raise-check-fixnum (local.get $x0)) (unreachable)))
                   (local.set $best (local.get $x0))
                   (block $done
                          (loop $loop
                                (br_if $done (ref.eq (local.get $xs) (global.get $null)))
                                (local.set $node (ref.cast (ref $Pair) (local.get $xs)))
                                (local.set $fx   (struct.get $Pair $a (local.get $node)))
                                (if (i32.eqz (ref.test (ref i31) (local.get $fx)))
                                    (then (call $raise-check-fixnum (local.get $fx)) (unreachable)))
                                (local.set $best (call ,cmp (local.get $best) (local.get $fx)))
                                (local.set $xs (struct.get $Pair $d (local.get $node)))
                                (br $loop)))
                   (local.get $best)))

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
         ;;; 4.3.3 Floating Point Numbers
         ;;;

         ;; https://docs.racket-lang.org/reference/flonums.html

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
         (func $flonum? (type $Prim1) (param $a (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.test (ref $Flonum) (local.get $a))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $fx->fl/precise (param $v (ref eq)) (result (ref $Flonum))
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

         (func $fx->fl (type $Prim1)
               (param $v (ref eq))
               (result   (ref eq))  ; a $Flonum

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
        (func $->fl (type $Prim1)
              (param $v (ref eq))
              (result (ref eq))
              (call $fx->fl (local.get $v)))
        (func $raise-fl->fx (param $x (ref eq)) (unreachable))

        (func $fl->fx (type $Prim1)
              (param $v (ref eq))
              (result   (ref eq)) ; a fixnum

              (local $v/fl  (ref $Flonum))
              (local $x/f64 f64)
              (local $t/f64 f64)
              (local $i32   i32)

              ;; Check that v is a flonum
              (if (i32.eqz (ref.test (ref $Flonum) (local.get $v)))
                  (then (call $raise-check-flonum (local.get $v))
                        (unreachable)))
              (local.set $v/fl  (ref.cast (ref $Flonum) (local.get $v)))
              (local.set $x/f64 (struct.get $Flonum $v (local.get $v/fl)))
              ;; Truncate toward zero
              (local.set $t/f64 (f64.trunc (local.get $x/f64)))
              ;; NaN?
              (if (f64.ne (local.get $t/f64) (local.get $t/f64))
                  (then (call $raise-fl->fx (local.get $v))
                        (unreachable)))
              ;; Check fixnum range
              (if (i32.or (f64.gt (local.get $t/f64) (f64.const 536870911.0))
                          (f64.lt (local.get $t/f64) (f64.const -536870912.0)))
                  (then (call $raise-fl->fx (local.get $v))
                        (unreachable)))
              ;; Convert to i32 and box
              (local.set $i32 (i32.trunc_f64_s (local.get $t/f64)))
              (ref.i31 (i32.shl (local.get $i32) (i32.const 1))))

        (func $raise-fl->exact-integer (param $x (ref eq)) (unreachable))

        (func $fl->exact-integer (type $Prim1)
              (param $v (ref eq))
              (result (ref eq))

              (local $v/fl  (ref $Flonum))
              (local $x/f64 f64)
              (local $t/f64 f64)
              (local $i32   i32)

              ;; Check that v is a flonum
              (if (i32.eqz (ref.test (ref $Flonum) (local.get $v)))
                  (then (call $raise-check-flonum (local.get $v))
                        (unreachable)))
              (local.set $v/fl  (ref.cast (ref $Flonum) (local.get $v)))
              (local.set $x/f64 (struct.get $Flonum $v (local.get $v/fl)))
              (local.set $t/f64 (f64.trunc (local.get $x/f64)))
              ;; Must be integer
              (if (f64.ne (local.get $x/f64) (local.get $t/f64))
                  (then (call $raise-fl->exact-integer (local.get $v))
                        (unreachable)))
              ;; NaN?
              (if (f64.ne (local.get $t/f64) (local.get $t/f64))
                  (then (call $raise-fl->exact-integer (local.get $v))
                        (unreachable)))
              ;; Check fixnum range
              (if (i32.or (f64.gt (local.get $t/f64) (f64.const 536870911.0))
                          (f64.lt (local.get $t/f64) (f64.const -536870912.0)))
                  (then (call $raise-fl->exact-integer (local.get $v))
                        (unreachable)))
              (local.set $i32 (i32.trunc_f64_s (local.get $t/f64)))
              (ref.i31 (i32.shl (local.get $i32) (i32.const 1))))

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

         (func $unsafe-fl/ (type $Prim2)
               (param $x (ref eq))
               (param $y (ref eq))
               (result   (ref eq))

               (local $x/fl (ref $Flonum))
               (local $y/fl (ref $Flonum))
               (local $x/f64 f64)
               (local $y/f64 f64)
               ;; Validate via cast (will trap if not a Flonum - hence unsafe)
               (local.set $x/fl (ref.cast (ref $Flonum) (local.get $x)))
               (local.set $y/fl (ref.cast (ref $Flonum) (local.get $y)))
               ;; Extract f64 values
               (local.set $x/f64 (struct.get $Flonum $v (local.get $x/fl)))
               (local.set $y/f64 (struct.get $Flonum $v (local.get $y/fl)))
               ;; Compute and box result
               (struct.new $Flonum
                           (i32.const 0)  ;; hash = 0
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
         
         (func $fl/ (type $Prim2)
               (param $x (ref eq))
               (param $y (ref eq))
               (result   (ref eq)) ; An (ref $Flonum)
               
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


         ;; flonum -> flonum
        ,@(let ([ops '((flabs      (f64.abs (local.get $a/f64)))
                       (flround    (f64.nearest (local.get $a/f64)))
                       (flfloor    (f64.floor (local.get $a/f64)))
                       (flceiling  (f64.ceil (local.get $a/f64)))
                       (fltruncate (f64.trunc (local.get $a/f64)))
                       (flsingle   (f64.promote_f32 (f32.demote_f64 (local.get $a/f64))))
                       (flsin      (call $js-math-sin  (local.get $a/f64)))
                       (flcos      (call $js-math-cos  (local.get $a/f64)))
                       (fltan      (call $js-math-tan  (local.get $a/f64)))
                       (flasin     (call $js-math-asin (local.get $a/f64)))
                       (flacos     (call $js-math-acos (local.get $a/f64)))
                       (flatan     (call $js-math-atan (local.get $a/f64)))
                       (fllog      (call $js-math-log  (local.get $a/f64)))
                       (flexp      (call $js-math-exp  (local.get $a/f64)))
                       (flsqrt     (call $js-math-sqrt (local.get $a/f64))))])
             (append
              (for/list ([p ops])
                (define name (car p))
                (define expr (cadr p))
                `(func ,(string->symbol (format "$~a" name))
                       (type $Prim1)
                       (param $a (ref eq))
                       (result (ref eq))
                       (local $a/fl (ref $Flonum))
                       (local $a/f64 f64)
                       (if (i32.eqz (ref.test (ref $Flonum) (local.get $a)))
                           (then (call $raise-argument-error:flonum-expected (local.get $a))
                                 (unreachable)))
                       (local.set $a/fl (ref.cast (ref $Flonum) (local.get $a)))
                       (local.set $a/f64 (struct.get $Flonum $v (local.get $a/fl)))
                       (struct.new $Flonum
                                   (i32.const 0)
                                   ,expr)))
              (for/list ([p ops])
                (define name (car p))
                (define expr (cadr p))
                `(func ,(string->symbol (format "$unsafe-~a" name))
                       (type $Prim1)
                       (param $a (ref eq))
                       (result (ref eq))
                       (local $a/fl (ref $Flonum))
                       (local $a/f64 f64)
                       (local.set $a/fl (ref.cast (ref $Flonum) (local.get $a)))
                       (local.set $a/f64 (struct.get $Flonum $v (local.get $a/fl)))
                       (struct.new $Flonum
                                   (i32.const 0)
                                   ,expr)))))

        ;; flonum flonum -> flonum
        ,@(let ([ops '((flmin/2  (f64.min (local.get $a/f64) (local.get $b/f64)))
                       (flmax/2  (f64.max (local.get $a/f64) (local.get $b/f64)))
                       (flexpt   (call $js-math-pow (local.get $a/f64) (local.get $b/f64))))])
             (append
              (for/list ([p ops])
                (define name (car p))
                (define expr (cadr p))
                `(func ,(string->symbol (format "$~a" name))
                       (type $Prim2)
                       (param $a (ref eq))
                       (param $b (ref eq))
                       (result (ref eq))
                       (local $a/fl (ref $Flonum))
                       (local $b/fl (ref $Flonum))
                       (local $a/f64 f64)
                       (local $b/f64 f64)
                       (if (i32.eqz (ref.test (ref $Flonum) (local.get $a)))
                           (then (call $raise-argument-error:flonum-expected (local.get $a))
                                 (unreachable)))
                       (if (i32.eqz (ref.test (ref $Flonum) (local.get $b)))
                           (then (call $raise-argument-error:flonum-expected (local.get $b))
                                 (unreachable)))
                       (local.set $a/fl (ref.cast (ref $Flonum) (local.get $a)))
                       (local.set $b/fl (ref.cast (ref $Flonum) (local.get $b)))
                       (local.set $a/f64 (struct.get $Flonum $v (local.get $a/fl)))
                       (local.set $b/f64 (struct.get $Flonum $v (local.get $b/fl)))
                       (struct.new $Flonum
                                   (i32.const 0)
                                   ,expr)))
              (for/list ([p ops])
                (define name (car p))
                (define expr (cadr p))
               `(func ,(string->symbol (format "$unsafe-~a" name))
                      (type $Prim2)
                      (param $a (ref eq))
                      (param $b (ref eq))
                      (result (ref eq))
                       (local $a/fl (ref $Flonum))
                       (local $b/fl (ref $Flonum))
                       (local $a/f64 f64)
                       (local $b/f64 f64)
                       (local.set $a/fl (ref.cast (ref $Flonum) (local.get $a)))
                      (local.set $b/fl (ref.cast (ref $Flonum) (local.get $b)))
                      (local.set $a/f64 (struct.get $Flonum $v (local.get $a/fl)))
                      (local.set $b/f64 (struct.get $Flonum $v (local.get $b/fl)))
                      (struct.new $Flonum
                                  (i32.const 0)
                                   ,expr)))))

        ;; variadic flonum min/max built on binary helpers
        ,@(let ()
             (define (fl-min/max name cmp unsafe?)
               `(func ,name (param $x0 (ref eq)) (param $xs (ref eq)) (result (ref eq))
                      (local $node (ref $Pair))
                      (local $fx   (ref eq))
                      (local $best (ref eq))
                      ,@(if unsafe?
                            '()
                            `((if (i32.eqz (ref.test (ref $Flonum) (local.get $x0)))
                                  (then (call $raise-argument-error:flonum-expected (local.get $x0))
                                        (unreachable)))))
                      (local.set $best (local.get $x0))
                      (block $done
                             (loop $loop
                                   (br_if $done (ref.eq (local.get $xs) (global.get $null)))
                                   (local.set $node (ref.cast (ref $Pair) (local.get $xs)))
                                   (local.set $fx   (struct.get $Pair $a (local.get $node)))
                                   ,@(if unsafe?
                                         '()
                                         `((if (i32.eqz (ref.test (ref $Flonum) (local.get $fx)))
                                               (then (call $raise-argument-error:flonum-expected (local.get $fx))
                                                     (unreachable)))))
                                   (local.set $best (call ,cmp (local.get $best) (local.get $fx)))
                                   (local.set $xs (struct.get $Pair $d (local.get $node)))
                                   (br $loop)))
                      (local.get $best)))
             (append (for/list ([name '( $flmin $flmax )]
                                [cmp  '( $flmin/2 $flmax/2)])
                       (fl-min/max name cmp #f))
                     (for/list ([name '( $unsafe-flmin $unsafe-flmax )]
                                [cmp  '( $unsafe-flmin/2 $unsafe-flmax/2)])
                       (fl-min/max name cmp #t))))

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
                  
         (func $number->string (type $Prim2)
               (param $z         (ref eq))
               (param $radix-raw (ref eq))
               (result           (ref eq)) ;; An (ref $String)

               (local $radix i32)
               (local $n     i32)
               (local $max   i32)
               (local $i31   (ref i31))
               ;; Step 1: Check if $z is a fixnum or flonum
               (if (ref.test (ref i31) (local.get $z))
                   (then (local.set $n (i32.shr_s (i31.get_s (ref.cast (ref i31) (local.get $z)))
                                                  (i32.const 1))))
                   (else
                    (if (ref.test (ref $Flonum) (local.get $z))
                        (then
                         ;; Handle optional radix for flonum (must be 10 or #f)
                         (if (ref.eq (local.get $radix-raw) (global.get $false))
                             (then (return (call $flonum->string
                                                  (ref.cast (ref $Flonum) (local.get $z)))))
                             (else
                              (if (ref.test (ref i31) (local.get $radix-raw))
                                  (then
                                   (local.set $radix
                                              (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $radix-raw)))
                                                         (i32.const 1)))
                                   (if (i32.eq (local.get $radix) (i32.const 10))
                                       (then (return (call $flonum->string
                                                            (ref.cast (ref $Flonum) (local.get $z)))))
                                       (else (call $raise-number->string-bad-radix))))
                                  (else (call $raise-number->string-bad-radix))))))
                        (else (call $raise-number->string-bad-input)))))
               ;; Step 2: Handle radix for fixnums
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

        (func $raise-string->number:bad-argument (unreachable))
        (func $raise-string->number:bad-radix (unreachable))

        (func $string->number (type $Prim5)
              (param $s-raw (ref eq))
              (param $radix-raw (ref eq))
              (param $convert-mode (ref eq))
              (param $decimal-mode (ref eq))
              (param $single-mode (ref eq))
              (result (ref eq))

              (local $s     (ref $String))
              (local $radix i32)
              (local $arr   (ref $I32Array))
              (local $len   i32)
              (local $i     i32)
              (local $neg   i32)
              (local $cp    i32)
              (local $digit i32)
              (local $acc   i32)

              ;; Validate string argument
              (if (ref.test (ref $String) (local.get $s-raw))
                  (then (nop))
                  (else (call $raise-string->number:bad-argument)
                        (unreachable)))
              (local.set $s (ref.cast (ref $String) (local.get $s-raw)))

              ;; Decode optional radix
              (if (ref.eq (local.get $radix-raw) (global.get $missing))
                  (then (local.set $radix (i32.const 10)))
                  (else
                   (if (ref.test (ref i31) (local.get $radix-raw))
                       (then
                        (local.set $radix
                                   (i32.shr_u
                                    (i31.get_u (ref.cast (ref i31) (local.get $radix-raw)))
                                    (i32.const 1))))
                       (else
                        (call $raise-string->number:bad-radix)
                        (unreachable)))))

              ;; Ensure radix within [2,16]
              (if (i32.or
                   (i32.lt_u (local.get $radix) (i32.const 2))
                   (i32.gt_u (local.get $radix) (i32.const 16)))
                  (then
                   (call $raise-string->number:bad-radix)
                   (unreachable))
                  (else (nop)))

              ;; Extract codepoints array and length
              (local.set $arr (struct.get $String $codepoints (local.get $s)))
              (local.set $len (call $i32array-length (local.get $arr)))

              ;; Empty string -> #f
              (if (i32.eqz (local.get $len))
                  (then (return (global.get $false)))
                  (else (nop)))

              ;; Handle optional sign
              (local.set $i   (i32.const 0))
              (local.set $neg (i32.const 0))
              (local.set $acc (i32.const 0))
              (local.set $cp (call $i32array-ref (local.get $arr) (i32.const 0)))
              (if (i32.eq (local.get $cp) (i32.const 45))  ;; '-'
                  (then (local.set $neg (i32.const 1))
                        (local.set $i (i32.const 1)))
                  (else
                   (if (i32.eq (local.get $cp) (i32.const 43)) ;; '+'
                       (then (local.set $i (i32.const 1)))
                       (else (nop)))))

              ;; Parse digits
              (block $done
                     (loop $loop
                           (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
                           (local.set $cp (call $i32array-ref (local.get $arr) (local.get $i)))

                           ;; Try '0'..'9'
                           (local.set $digit (i32.sub (local.get $cp) (i32.const 48)))
                           (if (i32.lt_u (local.get $digit) (i32.const 10))
                               (then (nop))
                               (else
                                ;; Try 'a'..'z'
                                (local.set $digit (i32.sub (local.get $cp) (i32.const 87)))
                                (if (i32.lt_u (local.get $digit) (i32.const 26))
                                    (then (local.set $digit
                                                     (i32.add (local.get $digit) (i32.const 10))))
                                    (else
                                     ;; Try 'A'..'Z'
                                     (local.set $digit (i32.sub (local.get $cp) (i32.const 55)))
                                     (if (i32.lt_u (local.get $digit) (i32.const 26))
                                         (then (local.set $digit
                                                          (i32.add (local.get $digit) (i32.const 10))))
                                         (else (return (global.get $false))))))))

                           (if (i32.ge_u (local.get $digit) (local.get $radix))
                               (then (return (global.get $false)))
                               (else (nop)))

                           (local.set $acc
                                      (i32.add
                                       (i32.mul (local.get $acc) (local.get $radix))
                                       (local.get $digit)))

                           (local.set $i (i32.add (local.get $i) (i32.const 1)))
                           (br $loop)))

              ;; Apply sign
              (if (local.get $neg)
                  (then (local.set $acc (i32.sub (i32.const 0) (local.get $acc))))
                  (else (nop)))

              ;; Return fixnum result
              (ref.i31 (i32.shl (local.get $acc) (i32.const 1))))




         
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
               (local.set $s-int (call $string-append/2
                                       (call $string-append/2
                                             (local.get $s-int)
                                             (local.get $dot))
                                      (local.get $s-frac)))
               ;; --- Add minus sign if needed ---
               (if (result (ref $String))
                   (local.get $neg)
                   (then (call $string-append/2 (local.get $minus) (local.get $s-int)))
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
                          (ref.cast (ref $String)
                                    (call $number->string
                                          (ref.i31 (i32.shl (local.get $exp) (i32.const 1)))   ;; boxed fixnum
                                          (ref.i31 (i32.const 20)))))                          ;; fixnum 10 (radix)
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

         (func $flonum->string
               (param $f (ref $Flonum))
               (result   (ref $String))

               (local $s   (ref $String))
               (local $len i32)
               (local $ch  i32)

               ;; Convert to decimal string and trim trailing zeros
               (local.set $s
                          (ref.cast (ref $String)
                            (call $string-trim-right
                                  (call $f64->string
                                        (struct.get $Flonum $v (local.get $f)))
                                  ,(Imm #\0))))
               (local.set $len (call $string-length/checked/i32 (local.get $s)))
               (local.set $ch
                          (call $string-ref/checked/i32
                                (local.get $s)
                                (i32.sub (local.get $len) (i32.const 1))))
               (if (result (ref $String))
                   (i32.eq (local.get $ch) (i32.const 46)) ;; '.'
                   (then (call $string-append/2
                                (local.get $s)
                                (call $codepoint->string (i32.const 48)))) ;; '0'
                   (else (local.get $s))))

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
               (ref.cast (ref $String)
                 (call $string-trim-right
                       (call $f64->string
                             (struct.get $Flonum $v (local.get $val)))
                       ,(Imm #\0))))
         


        ;;;
        ;;; - External host values
        ;;;

        (func $external? (param $v (ref eq)) (result (ref eq))
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

        ;;;
        ;;; 4.3 Byte Strings
        ;;;

        ;; https://docs.racket-lang.org/reference/bytestrings.html

         (func $raise-byte-out-of-range   (param $x (ref eq)) (unreachable))
         (func $raise-check-bytes         (param $x (ref eq)) (unreachable))
         (func $raise-check-byte          (param $x (ref eq)) (unreachable))
         (func $raise-bad-bytes-ref-index (param $x (ref eq)) (param $idx (ref eq)) (unreachable))         
         (func $raise-bad-bytes-range     (param $x (ref eq)) (param i32) (param i32) (unreachable))         
         
         (func $make-bytes (type $Prim2)
               (param $k-raw (ref eq))     ;; fixnum
               (param $b-raw (ref eq))     ;; optional byte
               (result       (ref eq))

               (local $len i32)
               (local $val i32)
               ;; Decode and check $k as fixnum
               (if (ref.test (ref i31) (local.get $k-raw))
                   (then (local.set $len (i31.get_u (ref.cast (ref i31) (local.get $k-raw))))
                         (if (i32.eqz (i32.and (local.get $len) (i32.const 1)))
                             (then (local.set $len (i32.shr_u (local.get $len) (i32.const 1))))
                             (else (call $raise-check-fixnum (local.get $k-raw))
                                   (unreachable))))
                   (else (call $raise-check-fixnum (local.get $k-raw))
                         (unreachable)))
               ;; Handle optional byte argument
               (if (ref.eq (local.get $b-raw) (global.get $missing))
                   (then (local.set $val (i32.const 0)))
                   (else (if (ref.test (ref i31) (local.get $b-raw))
                             (then (local.set $val (i31.get_u (ref.cast (ref i31) (local.get $b-raw))))
                                   (if (i32.eqz (i32.and (local.get $val) (i32.const 1)))
                                       (then (local.set $val (i32.shr_u (local.get $val) (i32.const 1)))
                                             (if (i32.gt_u (local.get $val) (i32.const 255))
                                                 (then (call $raise-byte-out-of-range (local.get $b-raw))
                                                       (unreachable))))
                                       (else (call $raise-check-fixnum (local.get $b-raw))
                                             (unreachable))))
                             (else (call $raise-check-fixnum (local.get $b-raw))
                                   (unreachable)))))
               ;; Construct mutable bytes object
               (struct.new $Bytes
                           (i32.const 0)  ;; hash
                           (i32.const 0)  ;; immutable = false
                           (call $i8make-array (local.get $len) (local.get $val))))
         

         (func $bytes (type $Prim>=0)
               (param $args (ref eq))
               (result (ref eq))

               (local $as  (ref $Args))
               (local $len i32)
               (local $arr (ref $I8Array))
               (local $i   i32)
               (local $x   (ref eq))
               (local $v   i32)

               ;; Cast argument array and determine length
               (local.set $as  (ref.cast (ref $Args) (local.get $args)))
               (local.set $len (array.len (local.get $as)))

               ;; Allocate mutable byte array
               (local.set $arr (call $i8make-array (local.get $len) (i32.const 0)))

               ;; Populate array from arguments
               (local.set $i (i32.const 0))
               (block $done
                      (loop $loop
                            (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
                            (local.set $x (array.get $Args (local.get $as) (local.get $i)))
                            (if (ref.test (ref i31) (local.get $x))
                                (then
                                 (local.set $v (i31.get_u (ref.cast (ref i31) (local.get $x))))
                                 (if (i32.eqz (i32.and (local.get $v) (i32.const 1)))
                                     (then
                                      (local.set $v (i32.shr_u (local.get $v) (i32.const 1)))
                                      (if (i32.lt_u (local.get $v) (i32.const 256))
                                          (then
                                           (call $i8array-set! (local.get $arr) (local.get $i) (local.get $v)))
                                          (else
                                           (call $raise-byte-out-of-range (local.get $x))
                                           (unreachable))))
                                     (else
                                      (call $raise-check-fixnum (local.get $x))
                                      (unreachable))))
                                (else
                                 (call $raise-check-fixnum (local.get $x))
                                 (unreachable)))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $loop)))

               ;; Wrap in Bytes struct and return
               (struct.new $Bytes
                           (i32.const 0)
                           (i32.const 0)
                           (local.get $arr)))

         (func $bytes->immutable-bytes (type $Prim1) (param $b (ref eq)) (result (ref eq))
               (local $bs (ref $Bytes))
               (local.set $bs (ref.cast (ref $Bytes) (global.get $bytes:empty)))
               ;; 1. Check that b is a byte string
               (if (ref.test (ref $Bytes) (local.get $b))
                   (then (local.set $bs (ref.cast (ref $Bytes) (local.get $b))))
                   (else (call $raise-check-bytes (local.get $b))
                         (unreachable)))
               ;; 2. If already immutable, return it directly
               (if (result (ref eq))
                   (i32.eq (struct.get $Bytes $immutable (local.get $bs)) (i32.const 1))
                   (then (local.get $bs))
                   (else
                    (struct.new $Bytes
                                (struct.get $Bytes $hash (local.get $bs))  ;; inherit hash
                                (i32.const 1)                                ;; immutable
                                (call $i8array-copy
                                      (struct.get $Bytes $bs (local.get $bs))
                                      (i32.const 0)
                                      (call $i8array-length (struct.get $Bytes $bs (local.get $bs))))))))

         (func $bytes-length (type $Prim1) (param $a (ref eq)) (result (ref eq))
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

         (func $bytes? (type $Prim1) (param $a (ref eq)) (result (ref eq))
               (if (result (ref eq)) (ref.test (ref $Bytes) (local.get $a))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $raise-expected-bytes (unreachable))

         (func $bytes=? (type $Prim2)
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

         
         (func $byte? (type $Prim1) (param $v (ref eq)) (result (ref eq))
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
         
         (func $bytes-ref (type $Prim2) (param $a (ref eq)) (param $i (ref eq)) (result (ref eq))
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
               (param $end   (ref eq))   ;; optional end index, default = (bytes-length $b)
               (result       (ref eq))
               
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
               ;; Decode and validate fixnum $end (optional)
               (if (ref.eq (local.get $end) (global.get $missing))
                   (then (local.set $to (local.get $len)))
                   (else (if (ref.test (ref i31) (local.get $end))
                             (then
                              (local.set $to (i31.get_u (ref.cast (ref i31) (local.get $end))))
                              (if (i32.eqz (i32.and (local.get $to) (i32.const 1)))
                                  (then (local.set $to (i32.shr_u (local.get $to) (i32.const 1))))
                                  (else (call $raise-check-fixnum (local.get $end)) (unreachable))))
                             (else (call $raise-check-fixnum (local.get $end)) (unreachable)))))
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
               (param $src-start  (ref eq))  ;; optional fixnum, default = 0
               (param $src-end    (ref eq))  ;; optional fixnum, default = (bytes-length src)
               (result            (ref eq))

               (local $d          (ref $Bytes))
               (local $s          (ref $Bytes))
               (local $darr       (ref $I8Array))
               (local $sarr       (ref $I8Array))
               
               (local $di         i32)
               (local $si         i32)
               (local $ei         i32)
               (local $src-len    i32)
               (local $dest-len   i32)

               ;; --- Validate $dest ---
               (if (i32.eqz (ref.test (ref $Bytes) (local.get $dest)))
                   (then (call $raise-check-bytes (local.get $dest))))
               ;; --- Validate $src ---
               (if (i32.eqz (ref.test (ref $Bytes) (local.get $src)))
                   (then (call $raise-check-bytes (local.get $src))))
               ;; --- Cast after validation ---
               (local.set $d (ref.cast (ref $Bytes) (local.get $dest)))
               (local.set $s (ref.cast (ref $Bytes) (local.get $src)))
               ;; --- Reject immutable destination ---
               (if (i32.eq (struct.get $Bytes $immutable (local.get $d)) (i32.const 1))
                   (then (call $raise-expected-mutable-bytes (local.get $dest)) (unreachable)))
               ;; --- Decode $dest-start ---
               (if (i32.eqz (ref.test (ref i31) (local.get $dest-start)))
                   (then (call $raise-check-fixnum (local.get $dest-start))))
               (if (i32.ne (i32.and (i31.get_u (ref.cast (ref i31) (local.get $dest-start)))
                                    (i32.const 1))
                           (i32.const 0))
                   (then (call $raise-check-fixnum (local.get $dest-start))))
               (local.set $di
                          (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $dest-start)))
                                     (i32.const 1)))
               ;; --- Extract arrays and lengths  ---
               (local.set $darr     (struct.get $Bytes $bs (local.get $d)))
               (local.set $sarr     (struct.get $Bytes $bs (local.get $s)))
               (local.set $src-len  (call $i8array-length (local.get $sarr)))
               (local.set $dest-len (call $i8array-length (local.get $darr)))
               ;; --- Decode optional $src-start ---
               (if (ref.eq (local.get $src-start) (global.get $missing))
                   (then (local.set $si (i32.const 0)))
                   (else
                    (if (i32.eqz (ref.test (ref i31) (local.get $src-start)))
                        (then (call $raise-check-fixnum (local.get $src-start))))
                    (if (i32.ne (i32.and (i31.get_u (ref.cast (ref i31) (local.get $src-start)))
                                         (i32.const 1))
                                (i32.const 0))
                        (then (call $raise-check-fixnum (local.get $src-start))))
                    (local.set $si
                               (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $src-start)))
                                          (i32.const 1)))))
               ;; --- Decode optional $src-end ---
               (if (ref.eq (local.get $src-end) (global.get $missing))
                   (then (local.set $ei (local.get $src-len)))
                   (else
                    (if (i32.eqz (ref.test (ref i31) (local.get $src-end)))
                        (then (call $raise-check-fixnum (local.get $src-end))))
                    (if (i32.ne (i32.and (i31.get_u (ref.cast (ref i31) (local.get $src-end)))
                                         (i32.const 1))
                                (i32.const 0))
                        (then (call $raise-check-fixnum (local.get $src-end))))
                    (local.set $ei
                               (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $src-end)))
                                          (i32.const 1)))))
               ;; --- Range validation ---
               (if (i32.gt_u (local.get $si) (local.get $ei))
                   (then (call $raise-bad-bytes-range (local.get $src)
                               (local.get $si) (local.get $ei))
                         (unreachable)))
               (if (i32.gt_u (local.get $ei) (local.get $src-len))
                   (then (call $raise-bad-bytes-range (local.get $src)
                               (local.get $si) (local.get $ei))
                         (unreachable)))
               (if (i32.gt_u (i32.add (local.get $di)
                                      (i32.sub (local.get $ei) (local.get $si)))
                             (local.get $dest-len))
                   (then (call $raise-bad-bytes-range
                               (local.get $dest) (local.get $di)
                               (i32.add (local.get $di)
                                        (i32.sub (local.get $ei) (local.get $si))))
                         (unreachable)))
               ;; --- Copy bytes ---
               (call $i8array-copy! (local.get $darr) (local.get $di)
                     (local.get $sarr) (local.get $si) (local.get $ei))
               (global.get $void))

         
         (func $bytes-copy (type $Prim1)
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

         (func $raise-expected-mutable-bytes (unreachable))
         
         (func $bytes-fill! (type $Prim2)
               (param $dest (ref eq))
               (param $b    (ref eq))
               (result      (ref eq))

               (local $bs     (ref null $Bytes))
               (local $arr    (ref $I8Array))
               (local $b/tag  i32)     ;; raw i31 payload (tagged)
               (local $val    i32)     ;; untagged byte value 0..255

               ;; dest must be a (mutable) byte string
               (if (ref.test (ref $Bytes) (local.get $dest))
                   (then (local.set $bs (ref.cast (ref $Bytes) (local.get $dest))))
                   (else (call $raise-check-bytes (local.get $dest)) (unreachable)))

               ;; reject immutable byte strings
               (if (i32.eq (struct.get $Bytes $immutable (local.get $bs)) (i32.const 1))
                   (then (call $raise-expected-mutable-bytes (local.get $dest)) (unreachable)))
               
               ;; b must be a fixnum byte (i31 with lsb=0), then 0..255
               (if (ref.test (ref i31) (local.get $b))
                   (then
                    (local.set $b/tag (i31.get_u (ref.cast (ref i31) (local.get $b))))
                    ;; ensure lsb=0 => fixnum (not a char etc.)
                    (if (i32.ne (i32.and (local.get $b/tag) (i32.const 1)) (i32.const 0))
                        (then (call $raise-check-byte (local.get $b)) (unreachable)))
                    ;; untag: shift right by 1 (your fixnum convention)
                    (local.set $val (i32.shr_u (local.get $b/tag) (i32.const 1)))
                    ;; range check 0..255
                    (if (i32.ge_u (local.get $val) (i32.const 256))
                        (then (call $raise-check-byte (local.get $b)) (unreachable))))
                   (else (call $raise-check-byte (local.get $b)) (unreachable)))

               ;; Fill underlying byte array
               (local.set $arr (struct.get $Bytes $bs (local.get $bs)))
               (call $i8array-fill! (local.get $arr) (local.get $val))

               ;; return void
               (global.get $void))

         (func $bytes-append (type $Prim>=0)
               (param  $xs   (ref eq))        ;; list of byte strings
               (result       (ref eq))

               (local $n     i32)
               (local $node  (ref $Pair))
               (local $b     (ref $Bytes))
               (local $v     (ref eq))
               (local $orig  (ref eq))
               (local $total i32)
               (local $len   i32)
               (local $arr   (ref $I8Array))
               (local $pos   i32)

               ;; === initialize non-defaultable refs ===
               (local.set $b (ref.cast (ref $Bytes) (global.get $bytes:empty)))

               ;; Preserve original list
               (local.set $orig (local.get $xs))
               ;; Determine number of arguments
               (local.set $n (call $length/i32 (local.get $xs)))
               ;; Zero arguments -> empty byte string
               (if (i32.eqz (local.get $n))
                   (then (return (global.get $bytes:empty))))
               ;; Extract and check first argument
               (local.set $node (ref.cast (ref $Pair) (local.get $xs)))
               (local.set $v    (struct.get $Pair $a (local.get $node)))
               (if (ref.test (ref $Bytes) (local.get $v))
                   (then (local.set $b (ref.cast (ref $Bytes) (local.get $v))))
                   (else (call $raise-check-bytes (local.get $v))))
               ;; Single argument -> copy to ensure fresh mutable bytes
               (if (i32.eq (local.get $n) (i32.const 1))
                   (then (return (call $bytes-copy (local.get $b)))))
               ;; Compute total length
               (local.set $total
                          (call $i8array-length
                                (struct.get $Bytes $bs (local.get $b))))
               (local.set $xs (struct.get $Pair $d (local.get $node)))
               (block $done1
                      (loop $loop1
                            (br_if $done1 (ref.eq (local.get $xs) (global.get $null)))
                            (local.set $node (ref.cast (ref $Pair) (local.get $xs)))
                            (local.set $v    (struct.get $Pair $a (local.get $node)))
                            (if (ref.test (ref $Bytes) (local.get $v))
                                (then
                                 (local.set $b (ref.cast (ref $Bytes) (local.get $v)))
                                 (local.set $len
                                            (call $i8array-length
                                                  (struct.get $Bytes $bs (local.get $b))))
                                 (local.set $total (i32.add (local.get $total) (local.get $len))))
                                (else (call $raise-check-bytes (local.get $v))))
                            (local.set $xs (struct.get $Pair $d (local.get $node)))
                            (br $loop1)))
               ;; All byte strings empty -> return empty byte string
               (if (i32.eqz (local.get $total))
                   (then (return (global.get $bytes:empty))))
               ;; Allocate result array
               (local.set $arr (call $i8make-array (local.get $total) (i32.const 0)))
               ;; Copy byte strings into result array
               (local.set $xs (local.get $orig))
               (local.set $pos (i32.const 0))
               (block $done2
                      (loop $loop2
                            (br_if $done2 (ref.eq (local.get $xs) (global.get $null)))
                            (local.set $node (ref.cast (ref $Pair) (local.get $xs)))
                            (local.set $b
                                       (ref.cast (ref $Bytes)
                                                 (struct.get $Pair $a (local.get $node))))
                            (local.set $len
                                       (call $i8array-length
                                             (struct.get $Bytes $bs (local.get $b))))
                            (call $i8array-copy!
                                  (local.get $arr) (local.get $pos)
                                  (struct.get $Bytes $bs (local.get $b))
                                  (i32.const 0) (local.get $len))
                            (local.set $pos (i32.add (local.get $pos) (local.get $len)))
                            (local.set $xs (struct.get $Pair $d (local.get $node)))
                            (br $loop2)))

               (struct.new $Bytes
                           (i32.const 0)
                           (i32.const 0)
                           (local.get $arr)))
         

         (func $bytes-append/2 
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
         

         (func $bytes->list (type $Prim1)
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

         (func $list->bytes (type $Prim1)
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
        ;;; 4.4 Strings
        ;;;

        ;; https://docs.racket-lang.org/reference/strings.html

         (func $raise-check-string         (param $x (ref eq))                      (unreachable))
         (func $raise-bad-string-index     (param $x (ref eq)) (param $i (ref eq))  (unreachable))
         (func $raise-bad-string-index/i32 (param $x (ref eq)) (param $i i32)       (unreachable))
         (func $raise-string-length:bad-argument (unreachable))
         
         (func $raise-string-index-out-of-bounds/i32 (param $x (ref eq)) (param $i i32) (param $n i32)
               (unreachable))

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

         
         ;; 4.4.1 String Constructors, Selectors, and Mutators
         
         (func $string? (type $Prim1) (param $s (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.test (ref $String) (local.get $s))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $non-empty-string? (type $Prim1)
               (param $s (ref eq))
               (result   (ref eq))
               
               (local $str (ref $String))
               (local $len i32)
               (if (result (ref eq))
                   (ref.test (ref $String) (local.get $s))
                   (then (local.set $str (ref.cast (ref $String) (local.get $s)))
                         (local.set $len (call $i32array-length
                                                  (struct.get $String $codepoints (local.get $str))))
                         (if (result (ref eq))
                             (i32.eqz (local.get $len))
                             (then (global.get $false))
                             (else (global.get $true))))
                   (else (global.get $false))))

         ;; Constructors

         (func $string (type $Prim>=0)
               (param $args (ref eq))
               (result      (ref eq))

               (local $argv (ref $Args))
               (local $len  i32)
               (local $arr  (ref $I32Array))
               (local $i    i32)
               (local $ch   (ref eq))

               ;; Cast argument list and determine length
               (local.set $argv (ref.cast (ref $Args) (local.get $args)))
               (local.set $len  (array.len (local.get $argv)))
               ;; Allocate array for codepoints
               (local.set $arr (array.new $I32Array (i32.const 0) (local.get $len)))
               ;; Fill array with characters
               (local.set $i (i32.const 0))
               (block $done
                      (loop $loop
                            (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
                            (local.set $ch (array.get $Args (local.get $argv) (local.get $i)))
                            (array.set $I32Array
                                       (local.get $arr)
                                       (local.get $i)
                                       (call $char->integer/i32 (local.get $ch)))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $loop)))
               ;; Construct mutable string
               (struct.new $String
                           (i32.const 0)  ;; hash
                           (i32.const 0)  ;; mutable
                           (local.get $arr)))

         (func $raise-make-string:bad-length       (unreachable))
         (func $raise-make-string:bad-char         (unreachable))
         (func $raise-argument-error:char-expected (unreachable))
                  
         (func $make-string (type $Prim2)
               (param $n-raw  (ref eq))    ;; fixnum
               (param $ch-raw (ref eq))    ;; optional immediate character
               (result        (ref eq))

               (local $n          i32)
               (local $ch-tagged  i32)
               (local $ch         i32)
               ;; --- Type check for length ---
               (if (i32.eqz (ref.test (ref i31) (local.get $n-raw)))
                   (then (call $raise-make-string:bad-length)))
               ;; --- Decode length ---
               (local.set $n (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $n-raw))) (i32.const 1)))
               ;; --- Handle optional character ---
               (if (ref.eq (local.get $ch-raw) (global.get $missing))
                   (then (local.set $ch (i32.const 0)))
                   (else
                    ;; --- Validate character ---
                    (if (i32.eqz (ref.test (ref i31) (local.get $ch-raw)))
                        (then (call $raise-make-string:bad-char)))
                    (local.set $ch-tagged (i31.get_u (ref.cast (ref i31) (local.get $ch-raw))))
                    (if (i32.ne (i32.and (local.get $ch-tagged) (i32.const ,char-mask)) (i32.const ,char-tag))
                        (then (call $raise-make-string:bad-char)))
                    ;; --- Extract code point from character ---
                    (local.set $ch (i32.shr_u (local.get $ch-tagged) (i32.const ,char-shift)))))
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
         
         (func $string->immutable-string (type $Prim1)
               (param $s (ref eq))
               (result   (ref eq))
               
               (local $str (ref $String))

               (local.set $str (call $make-dummy-string))
               ;; 1. Check that s is a String
               (if (i32.eqz (ref.test (ref $String) (local.get $s)))
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
         
         (func $raise-build-string:bad-length    (unreachable))
         (func $raise-build-string:char-expected
               (param $v (ref eq)) (call $js-log (local.get $v)) (unreachable))

         (func $build-string (type $Prim2)
               (param $n-raw (ref eq))
               (param $proc  (ref eq))
               (result       (ref eq))

               (local $n    i32)
               (local $f    (ref $Procedure))
               (local $finv (ref $ProcedureInvoker))
               (local $args (ref $Args))
               (local $arr  (ref $I32Array))
               (local $i    i32)
               (local $res  (ref eq))
               (local $cp   i32)
               (local $str  (ref $String))

               ;; --- Check arguments ---
               (if (i32.eqz (ref.test (ref i31) (local.get $n-raw)))
                   (then (call $raise-build-string:bad-length)))
               (if (i32.eqz (ref.test (ref $Procedure) (local.get $proc)))
                   (then (call $raise-argument-error:procedure-expected (local.get $proc))
                         (unreachable)))

               ;; --- Decode and prepare ---
               (local.set $n    (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $n-raw)))
                                           (i32.const 1)))
               (local.set $f    (ref.cast (ref $Procedure) (local.get $proc)))
               (local.set $finv (struct.get $Procedure $invoke (local.get $f)))

               (local.set $arr  (call $i32array-make (local.get $n) (i32.const 0)))
               (local.set $args (array.new $Args (global.get $null) (i32.const 1)))

               ;; --- Loop ---
               (local.set $i (i32.const 0))
               (block $done
                      (loop $loop
                            (br_if $done (i32.ge_u (local.get $i) (local.get $n)))
                            ;; Set argument to current index as fixnum
                            (array.set $Args (local.get $args) (i32.const 0)
                                       (ref.i31 (i32.shl (local.get $i) (i32.const 1))))
                            ;; Call procedure
                            (local.set $res
                                       (call_ref $ProcedureInvoker
                                                 (local.get $f)
                                                 (local.get $args)
                                                 (local.get $finv)))
                            ;; Validate character result
                            (if (i32.eqz (ref.test (ref i31) (local.get $res)))
                                (then (call $raise-build-string:char-expected (local.get $res))
                                      (unreachable)))
                            (local.set $cp (i31.get_u (ref.cast (ref i31) (local.get $res))))
                            (if (i32.ne (i32.and (local.get $cp) (i32.const ,char-mask))
                                        (i32.const ,char-tag))
                                (then (call $raise-build-string:char-expected (local.get $res))
                                      (unreachable)))
                            (local.set $cp (i32.shr_u (local.get $cp) (i32.const ,char-shift)))
                            (call $i32array-set! (local.get $arr) (local.get $i) (local.get $cp))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $loop)))
               ;; --- Construct string ---
               (local.set $str
                          (struct.new $String
                                      (i32.const 0)
                                      (i32.const 0)
                                      (local.get $arr)))
               (local.get $str))
         

         (func $string-length (type $Prim1)
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
         

         (func $string-ref (type $Prim2)
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

         (func $substring (type $Prim3)
              (param $s     (ref eq))
              (param $start (ref eq))
              (param $end   (ref eq))
              (result       (ref eq))

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
              ;; get array and length
              (local.set $arr (struct.get $String $codepoints (local.get $str)))
              (local.set $len (call $i32array-length (local.get $arr)))
              ;; decode and check end index (optional)
              (if (ref.eq (local.get $end) (global.get $missing))
                  (then (local.set $i32end (local.get $len)))
                  (else (if (ref.test (ref i31) (local.get $end))
                            (then (local.set $i32end (i31.get_u (ref.cast (ref i31) (local.get $end))))
                                  (if (i32.ne (i32.and (local.get $i32end) (i32.const 1)) (i32.const 0))
                                      (then (call $raise-check-fixnum (local.get $end))))
                                  (local.set $i32end (i32.shr_u (local.get $i32end) (i32.const 1))))
                            (else (call $raise-check-fixnum (local.get $end))))))
              ;; bounds check: start <= end <= len
              (if (i32.or (i32.gt_u (local.get $i32start) (local.get $i32end))
                          (i32.gt_u (local.get $i32end) (local.get $len)))
                  (then (call $raise-string-index-out-of-bounds/i32 (local.get $s) (local.get $i32end) (local.get $len))))
              ;; create new string
              (struct.new $String
                          (i32.const 0) ; hash
                          (i32.const 0) ; mutable (also for immutable input)
                          (call $i32array-copy (local.get $arr) (local.get $i32start) (local.get $i32end))))

         (func $string-copy (type $Prim1)
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
         
         (func $string-fill! (type $Prim2)
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


         (func $string-append/2 (param $s1 (ref eq)) (param $s2 (ref eq)) (result (ref $String))
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


         (func $string-append (type $Prim>=0)
               (param  $xs   (ref eq))        ;; expects a list of strings
               (result       (ref eq))

               (local $n     i32)
               (local $node  (ref $Pair))
               (local $s     (ref $String))
               (local $v     (ref eq))
               (local $orig  (ref eq))
               (local $total i32)
               (local $len   i32)
               (local $arr   (ref $I32Array))
               (local $pos   i32)

               ;; === initialize non-defaultable refs ===
               (local.set $s (ref.cast (ref $String) (global.get $string:empty)))
               
               ;; Preserve original list
               (local.set $orig (local.get $xs))
               ;; Determine number of arguments
               (local.set $n (call $length/i32 (local.get $xs)))
               ;; Zero arguments -> existing empty string
               (if (i32.eqz (local.get $n))
                   (then (return (global.get $string:empty))))
               ;; Extract and check first argument
               (local.set $node (ref.cast (ref $Pair) (local.get $xs)))
               (local.set $v    (struct.get $Pair $a (local.get $node)))
               (if (ref.test (ref $String) (local.get $v))
                   (then (local.set $s (ref.cast (ref $String) (local.get $v))))
                   (else (call $raise-check-string (local.get $v))))
               ;; Single argument -> copy to ensure fresh mutable string
               (if (i32.eq (local.get $n) (i32.const 1))
                   (then (if (ref.eq (local.get $s) (global.get $string:empty))
                             (then (return (global.get $string:empty)))
                             (else (return (call $string-copy (local.get $s)))))))
               ;; Compute total length
               (local.set $total
                          (call $i32array-length
                                (struct.get $String $codepoints (local.get $s))))
               (local.set $xs (struct.get $Pair $d (local.get $node)))
               (block $done1
                      (loop $loop1
                            (br_if $done1 (ref.eq (local.get $xs) (global.get $null)))
                            (local.set $node (ref.cast (ref $Pair) (local.get $xs)))
                            (local.set $v    (struct.get $Pair $a (local.get $node)))
                            (if (ref.test (ref $String) (local.get $v))
                                (then
                                 (local.set $s (ref.cast (ref $String) (local.get $v)))
                                 (local.set $len
                                            (call $i32array-length
                                                  (struct.get $String $codepoints (local.get $s))))
                                 (local.set $total (i32.add (local.get $total) (local.get $len))))
                                (else (call $raise-check-string (local.get $v))))
                            (local.set $xs (struct.get $Pair $d (local.get $node)))
                            (br $loop1)))
               ;; All strings empty -> return empty string
               (if (i32.eqz (local.get $total))
                   (then (return (global.get $string:empty))))
               ;; Allocate result array
               (local.set $arr (call $i32array-make (local.get $total) (i32.const 0)))
               ;; Copy strings into result array
               (local.set $xs (local.get $orig))
               (local.set $pos (i32.const 0))
               (block $done2
                      (loop $loop2
                            (br_if $done2 (ref.eq (local.get $xs) (global.get $null)))
                            (local.set $node (ref.cast (ref $Pair) (local.get $xs)))
                            (local.set $s
                                       (ref.cast (ref $String) (struct.get $Pair $a (local.get $node))))
                            (local.set $len
                                       (call $i32array-length
                                             (struct.get $String $codepoints (local.get $s))))
                            (call $i32array-copy!
                                  (local.get $arr) (local.get $pos)
                                  (struct.get $String $codepoints (local.get $s))
                                  (i32.const 0) (local.get $len))
                            (local.set $pos (i32.add (local.get $pos) (local.get $len)))
                            (local.set $xs (struct.get $Pair $d (local.get $node)))
                            (br $loop2)))

               (struct.new $String
                           (i32.const 0)
                           (i32.const 0)
                           (local.get $arr)))


         (func $string-append-immutable (type $Prim>=0)
               (param  $xs (ref eq))         ;; list of strings
               (result     (ref eq))

               (local $s   (ref $String))
               (local $arr (ref $I32Array))
               ;; Do the append (returns a fresh $String)
               (local.set $s (ref.cast (ref $String)
                                       (call $string-append (local.get $xs))))
               ;; Reuse its codepoint array
               (local.set $arr (struct.get $String $codepoints (local.get $s)))
               ;; Build a new $String that is immutable (=1). Hash = 0 (lazy).
               (struct.new $String
                           (i32.const 0)      ;; $hash
                           (i32.const 1)      ;; $immutable
                           (local.get $arr))) ;; $codepoints

         
         (func $string->list (type $Prim1) (param $s (ref eq)) (result (ref eq))
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

         (func $list->string (type $Prim1) (param $xs (ref eq)) (result (ref eq))
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

         #;(func $string-replace
               (param $s    (ref eq))
               (param $from (ref eq))
               (param $to   (ref eq))
               (param $all  (ref eq))
               (result (ref eq))

               (local $str        (ref $String))
               (local $from-str   (ref $String))
               (local $to-str     (ref $String))
               (local $src        (ref $I32Array))
               (local $pat        (ref $I32Array))
               (local $rep        (ref $I32Array))
               (local $len-src    i32)
               (local $len-pat    i32)
               (local $len-rep    i32)
               (local $i          i32)
               (local $j          i32)
               (local $match      i32)
               (local $out        (ref $I32GrowableArray))
               (local $all?       i32)

               ;; Check string arguments
               (if (i32.eqz (ref.test (ref $String) (local.get $s)))
                   (then (call $raise-check-string (local.get $s))))
               (local.set $str (ref.cast (ref $String) (local.get $s)))
               (if (i32.eqz (ref.test (ref $String) (local.get $from)))
                   (then (call $raise-check-string (local.get $from))))
               (local.set $from-str (ref.cast (ref $String) (local.get $from)))
               (if (i32.eqz (ref.test (ref $String) (local.get $to)))
                   (then (call $raise-check-string (local.get $to))))
               (local.set $to-str (ref.cast (ref $String) (local.get $to)))

               ;; Determine all? flag
               (local.set $all? (i32.eqz (ref.eq (local.get $all) (global.get $false))))

               ;; Extract arrays and lengths
               (local.set $src (struct.get $String $codepoints (local.get $str)))
               (local.set $pat (struct.get $String $codepoints (local.get $from-str)))
               (local.set $rep (struct.get $String $codepoints (local.get $to-str)))
               (local.set $len-src (call $i32array-length (local.get $src)))
               (local.set $len-pat (call $i32array-length (local.get $pat)))
               (local.set $len-rep (call $i32array-length (local.get $rep)))

               ;; pattern length zero -> copy string
               (if (i32.eq (local.get $len-pat) (i32.const 0))
                   (then (return (call $string-copy (local.get $str)))))

               ;; prepare builder
               (local.set $out (call $make-i32growable-array (local.get $len-src)))
               (local.set $i (i32.const 0))
               (block $done
                      (loop $loop
                            (br_if $done (i32.ge_u (local.get $i) (local.get $len-src)))
                            (if (i32.gt_u (local.get $len-pat)
                                          (i32.sub (local.get $len-src) (local.get $i)))
                                (then
                                 (block $copy-rest
                                        (loop $copy-loop
                                              (br_if $copy-rest (i32.ge_u (local.get $i) (local.get $len-src)))
                                              (call $i32growable-array-add! (local.get $out)
                                                    (call $i32array-ref (local.get $src) (local.get $i)))
                                              (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                              (br $copy-loop)))
                                 (br $done))
                                (else
                                 (local.set $j (i32.const 0))
                                 (local.set $match (i32.const 1))
                                 (block $cmp
                                        (loop $cmp-loop
                                              (br_if $cmp (i32.ge_u (local.get $j) (local.get $len-pat)))
                                              (if (i32.ne
                                                    (call $i32array-ref (local.get $src)
                                                                        (i32.add (local.get $i) (local.get $j)))
                                                    (call $i32array-ref (local.get $pat) (local.get $j)))
                                                  (then (local.set $match (i32.const 0))
                                                        (br $cmp))
                                                  (else (local.set $j (i32.add (local.get $j) (i32.const 1)))
                                                        (br $cmp-loop))))
                                 (if (i32.eqz (local.get $match))
                                     (then
                                      (call $i32growable-array-add! (local.get $out)
                                            (call $i32array-ref (local.get $src) (local.get $i)))
                                      (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                      (br $loop))
                                     (else
                                      (local.set $j (i32.const 0))
                                      (block $append
                                             (loop $append-loop
                                                   (br_if $append (i32.ge_u (local.get $j) (local.get $len-rep)))
                                                   (call $i32growable-array-add! (local.get $out)
                                                         (call $i32array-ref (local.get $rep) (local.get $j)))
                                                   (local.set $j (i32.add (local.get $j) (i32.const 1)))
                                                   (br $append-loop)))
                                      (local.set $i (i32.add (local.get $i) (local.get $len-pat)))
                                      (if (i32.eqz (local.get $all?))
                                          (then
                                           (block $copy-rest2
                                                  (loop $copy-loop2
                                                        (br_if $copy-rest2 (i32.ge_u (local.get $i) (local.get $len-src)))
                                                        (call $i32growable-array-add! (local.get $out)
                                                              (call $i32array-ref (local.get $src) (local.get $i)))
                                                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                                        (br $copy-loop2)))
                                           (br $done))
                                          (else (br $loop))))))))))

               (call $i32growable-array->string (local.get $out)))
                  
         
         ;; 4.4.2 String Comparisons
         
         (func $string=? (type $Prim2)
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

         (func $string<? (type $Prim2)
               (param $a (ref eq)) (param $b (ref eq))
               (result (ref eq))
               (if (result (ref eq)) (call $string</i32 (local.get $a) (local.get $b))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $string<=? (type $Prim2)
               (param $a (ref eq)) (param $b (ref eq))
               (result (ref eq))
               (if (result (ref eq))
                   (ref.eq (call $string<? (local.get $a) (local.get $b))
                           (global.get $true))
                   (then (global.get $true))
                   (else (call $string=? (local.get $a) (local.get $b)))))

         (func $string>? (type $Prim2)
               (param $a (ref eq)) (param $b (ref eq))
               (result (ref eq))
               (call $string<? (local.get $b) (local.get $a)))

         (func $string>=? (type $Prim2)
               (param $a (ref eq)) (param $b (ref eq))
               (result (ref eq))
               (if (result (ref eq))
                   (ref.eq (call $string<? (local.get $b) (local.get $a))
                           (global.get $true))
                   (then (global.get $true))
                   (else (call $string=? (local.get $a) (local.get $b)))))

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

         
         ;;; 

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

         (func $string-copy! (type $Prim5)
               (param $dst-raw       (ref eq))
               (param $dst-start-raw (ref eq))
               (param $src-raw       (ref eq))
               (param $src-start-raw (ref eq))   ;; fixnum or $missing
               (param $src-end-raw   (ref eq))   ;; fixnum or $missing
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
               ;; src-start optional
               (if (i32.eqz (ref.eq (local.get $src-start-raw) (global.get $missing)))
                   (then
                    (if (i32.eqz (ref.test (ref i31) (local.get $src-start-raw)))
                        (then (call $raise-string-copy!:bad-source-start)))
                    (if (i32.ne (i32.and (i31.get_u (ref.cast (ref i31) (local.get $src-start-raw))) (i32.const 1)) (i32.const 0))
                        (then (call $raise-string-copy!:bad-source-start)))))
               ;; src-end optional
               (if (i32.eqz (ref.eq (local.get $src-end-raw) (global.get $missing)))
                   (then
                    (if (i32.eqz (ref.test (ref i31) (local.get $src-end-raw)))
                        (then (call $raise-string-copy!:bad-source-end)))
                    (if (i32.ne (i32.and (i31.get_u (ref.cast (ref i31) (local.get $src-end-raw))) (i32.const 1)) (i32.const 0))
                        (then (call $raise-string-copy!:bad-source-end)))))
               ;; --- Decode ---
               (local.set $dst       (ref.cast (ref $String) (local.get $dst-raw)))
               (local.set $src       (ref.cast (ref $String) (local.get $src-raw)))
               (local.set $dst-start (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $dst-start-raw))) (i32.const 1)))
               (local.set $src-len   (call $string-length/checked/i32 (local.get $src)))
               (if (ref.eq (local.get $src-start-raw) (global.get $missing))
                   (then (local.set $src-start (i32.const 0)))
                   (else (local.set $src-start (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $src-start-raw))) (i32.const 1)))))
               (if (ref.eq (local.get $src-end-raw) (global.get $missing))
                   (then (local.set $src-end (local.get $src-len)))
                   (else (local.set $src-end (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $src-end-raw))) (i32.const 1)))))
               ;; --- Mutability Check ---
               (if (i32.ne (struct.get $String $immutable (local.get $dst)) (i32.const 0))
                   (then (call $raise-immutable-string (local.get $dst))))
               ;; --- Range Validation ---
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

         
         (func $string-take (type $Prim2)
               (param $s (ref eq))
               (param $n (ref eq))
               (result   (ref eq))  ; an (ref $String)

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

               (local.set $len
                          (call $string-length/checked/i32 (local.get $str)))
               (if (i32.gt_u (local.get $n/i32) (local.get $len))
                   (then (call $raise-bad-string-index/i32
                               (local.get $s) (local.get $n/i32))))

               (call $string-take/checked (local.get $str) (local.get $n/i32)))

         (func $string-take/checked
               (param $s (ref $String)) (param $n i32)
               (result (ref $String))
               (call $i32array->string
                     (call $i32array-take
                           (struct.get $String $codepoints (local.get $s))
                           (local.get $n))))



         (func $string-take-right (type $Prim2)
               (param $s (ref eq))
               (param $n (ref eq))
               (result   (ref eq))  ; an (ref $String)

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

         (func $string-drop (type $Prim2)
               (param $s (ref eq))
               (param $n (ref eq))
               (result   (ref eq))  ; an (ref $String)

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

         (func $string-drop-right (type $Prim2)
               (param $s (ref eq))
               (param $n (ref eq))
               (result   (ref eq)) ; an (ref $String)

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
         
         (func $string-trim-right (type $Prim2)
               (param $s       (ref eq))   ;; any value, must be a string
               (param $sep     (ref eq))   ;; a character (tagged i31) or #f
               (result         (ref eq))   ;; an (ref $String)

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
         
         (func $string-trim-left (type $Prim2)
               (param $s   (ref eq))   ;; any value, must be a string
               (param $sep (ref eq))   ;; a character (i31) or #f
               (result     (ref eq))   ;; an (ref $String)

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
                        (then (call $raise-argument-error:char-expected
                                    (local.get $sep))
                              (unreachable)))
                    ;; Extract raw tagged value
                    (local.set $sep/tag
                               (i31.get_u (ref.cast (ref i31) (local.get $sep))))
                    ;; Check tag matches ,char-tag
                    (if (i32.ne (i32.and (local.get $sep/tag) (i32.const ,char-mask))
                                (i32.const ,char-tag))
                        (then (call $raise-argument-error:char-expected
                                    (local.get $sep))
                              (unreachable)))
                    ;; Passed: decode
                    (local.set $use-whitespace? (i32.const 0))
                    (local.set $sep-ch (i32.shr_u (local.get $sep/tag)
                                                  (i32.const ,char-shift)))))
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

         ;; 4.4.6 Additional String Functions  (racket/string)
         
         (func $string-suffix? (type $Prim2)
               (param $s (ref eq)) (param $suffix (ref eq))
               (result (ref eq))
               (if (result (ref eq))
                   (call $string-suffix?/i32 (local.get $s) (local.get $suffix))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $string-suffix?/i32
               (param $s-raw (ref eq)) (param $suffix-raw (ref eq))
               (result i32)

               (local $s (ref $String))
               (local $suf (ref $String))

               ;; Type checks
               (if (i32.eqz (ref.test (ref $String) (local.get $s-raw)))
                   (then (return (i32.const 0))))
               (if (i32.eqz (ref.test (ref $String) (local.get $suffix-raw)))
                   (then (return (i32.const 0))))

               ;; Cast and delegate
               (local.set $s   (ref.cast (ref $String) (local.get $s-raw)))
               (local.set $suf (ref.cast (ref $String) (local.get $suffix-raw)))
               (return_call $string-suffix?/i32/checked (local.get $s) (local.get $suf)))

         (func $string-suffix?/i32/checked
               (param $s (ref $String)) (param $suf (ref $String))
               (result i32)

               (local $arr-s   (ref $I32Array))
               (local $arr-suf (ref $I32Array))
               (local $len-s   i32)
               (local $len-suf i32)
               (local $offset  i32)
               (local $i       i32)

               (local.set $arr-s   (struct.get $String $codepoints (local.get $s)))
               (local.set $arr-suf (struct.get $String $codepoints (local.get $suf)))
               (local.set $len-s   (array.len (local.get $arr-s)))
               (local.set $len-suf (array.len (local.get $arr-suf)))

               ;; If suffix longer than string, fail
               (if (i32.gt_u (local.get $len-suf) (local.get $len-s))
                   (then (return (i32.const 0))))

               (local.set $offset (i32.sub (local.get $len-s) (local.get $len-suf)))
               (local.set $i (i32.const 0))
               (block $done
                      (loop $loop
                            (br_if $done (i32.ge_u (local.get $i) (local.get $len-suf)))
                            (if (i32.ne
                                 (array.get $I32Array (local.get $arr-s)
                                            (i32.add (local.get $offset) (local.get $i)))
                                 (array.get $I32Array (local.get $arr-suf) (local.get $i)))
                                (then (return (i32.const 0))))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $loop)))
               (i32.const 1))

         (func $string-prefix? (type $Prim2)
               (param $s (ref eq)) (param $prefix (ref eq))
               (result (ref eq))
               (if (result (ref eq)) (call $string-prefix?/i32 (local.get $s) (local.get $prefix))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $string-prefix?/i32
               (param $s-raw (ref eq)) (param $prefix-raw (ref eq))
               (result i32)

               (local $s (ref $String))
               (local $p (ref $String))

               (if (i32.eqz (ref.test (ref $String) (local.get $s-raw)))
                   (then (call $raise-check-string (local.get $s-raw))))
               (if (i32.eqz (ref.test (ref $String) (local.get $prefix-raw)))
                   (then (call $raise-check-string (local.get $prefix-raw))))

               (local.set $s (ref.cast (ref $String) (local.get $s-raw)))
               (local.set $p (ref.cast (ref $String) (local.get $prefix-raw)))

               (return_call $string-prefix?/i32/checked (local.get $s) (local.get $p)))

         (func $string-prefix?/i32/checked
               (param $s (ref $String)) (param $p (ref $String))
               (result i32)

               (local $arr-s (ref $I32Array))
               (local $arr-p (ref $I32Array))
               (local $len-s i32)
               (local $len-p i32)
               (local $i i32)
               (local $cp-s i32)
               (local $cp-p i32)

               (local.set $arr-s (struct.get $String $codepoints (local.get $s)))
               (local.set $arr-p (struct.get $String $codepoints (local.get $p)))
               (local.set $len-s (array.len (local.get $arr-s)))
               (local.set $len-p (array.len (local.get $arr-p)))
               (if (i32.lt_u (local.get $len-s) (local.get $len-p))
                   (then (return (i32.const 0))))
               (local.set $i (i32.const 0))
               (block $exit
                      (loop $loop
                            (br_if $exit (i32.ge_u (local.get $i) (local.get $len-p)))
                            (local.set $cp-s (array.get $I32Array (local.get $arr-s) (local.get $i)))
                            (local.set $cp-p (array.get $I32Array (local.get $arr-p) (local.get $i)))
                            (if (i32.ne (local.get $cp-s) (local.get $cp-p))
                                (then (return (i32.const 0))))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $loop)))
               (i32.const 1))

         (func $string-contains? (type $Prim2)
               (param $s (ref eq))
               (param $contained (ref eq))
               (result (ref eq))

               (local $str    (ref $String))
               (local $sub    (ref $String))
               (local $arr-s  (ref $I32Array))
               (local $arr-c  (ref $I32Array))
               (local $len-s  i32)
               (local $len-c  i32)
               (local $limit  i32)
               (local $i      i32)
               (local $j      i32)
               (local $ch-s   i32)
               (local $ch-c   i32)

               ;; --- Check inputs ---
               (if (i32.eqz (ref.test (ref $String) (local.get $s)))
                   (then (call $raise-argument-error:string-expected (local.get $s))
                         (unreachable)))
               (if (i32.eqz (ref.test (ref $String) (local.get $contained)))
                   (then (call $raise-argument-error:string-expected (local.get $contained))
                         (unreachable)))

               ;; --- Decode after checks ---
               (local.set $str   (ref.cast (ref $String) (local.get $s)))
               (local.set $sub   (ref.cast (ref $String) (local.get $contained)))
               (local.set $arr-s (struct.get $String $codepoints (local.get $str)))
               (local.set $arr-c (struct.get $String $codepoints (local.get $sub)))
               (local.set $len-s (array.len (local.get $arr-s)))
               (local.set $len-c (array.len (local.get $arr-c)))

               ;; --- Edge cases ---
               (if (i32.eqz (local.get $len-c))
                   (then (return (global.get $true))))
               (if (i32.lt_u (local.get $len-s) (local.get $len-c))
                   (then (return (global.get $false))))

               (local.set $limit (i32.sub (local.get $len-s) (local.get $len-c)))
               (local.set $i     (i32.const 0))
               (block $not-found
                      (loop $outer
                            (br_if $not-found
                                   (i32.gt_u (local.get $i) (local.get $limit)))
                            (local.set $j (i32.const 0))
                            (block $mismatch
                                   (loop $inner
                                         (br_if $mismatch
                                                (i32.ge_u (local.get $j) (local.get $len-c)))
                                         (local.set $ch-s
                                                    (call $i32array-ref
                                                          (local.get $arr-s)
                                                          (i32.add (local.get $i) (local.get $j))))
                                         (local.set $ch-c
                                                    (call $i32array-ref
                                                          (local.get $arr-c)
                                                          (local.get $j)))
                                         (if (i32.ne (local.get $ch-s) (local.get $ch-c))
                                             (then (br $mismatch)))
                                         (local.set $j (i32.add (local.get $j) (i32.const 1)))
                                         (br $inner)))
                            (if (i32.eq (local.get $j) (local.get $len-c))
                                (then (return (global.get $true))))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $outer)))
               (global.get $false))
         
         (func $string-find (type $Prim2)
               (param $s (ref eq))
               (param $contained (ref eq))
               (result (ref eq))

               (local $str (ref $String))
               (local $sub (ref $String))
               (local $len i32)
               (local $clen i32)
               (local $limit i32)
               (local $i i32)

               ;; Type checks
               (if (i32.eqz (ref.test (ref $String) (local.get $s)))
                   (then (call $raise-check-string (local.get $s))))
               (if (i32.eqz (ref.test (ref $String) (local.get $contained)))
                   (then (call $raise-check-string (local.get $contained))))
               ;; Cast and lengths
               (local.set $str (ref.cast (ref $String) (local.get $s)))
               (local.set $sub (ref.cast (ref $String) (local.get $contained)))
               (local.set $len  (call $string-length/checked/i32 (local.get $str)))
               (local.set $clen (call $string-length/checked/i32 (local.get $sub)))
               ;; Empty substring => 0
               (if (i32.eqz (local.get $clen))
                   (then (return (ref.i31 (i32.const 0)))))
               ;; Substring longer than string => #f
               (if (i32.lt_u (local.get $len) (local.get $clen))
                   (then (return (global.get $false))))
               ;; Search
               (local.set $limit (i32.sub (local.get $len) (local.get $clen)))
               (local.set $i (i32.const 0))
               (block $done
                      (loop $loop
                            (br_if $done (i32.gt_u (local.get $i) (local.get $limit)))
                            (if (ref.eq
                                 (call $string=?
                                       (call $substring
                                             (local.get $s)
                                             (ref.i31 (i32.shl (local.get $i) (i32.const 1)))
                                             (ref.i31 (i32.shl (i32.add (local.get $i) (local.get $clen)) (i32.const 1))))
                                       (local.get $contained))
                                 (global.get $true))
                                (then (return (ref.i31 (i32.shl (local.get $i) (i32.const 1)))))
                                (else (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                      (br $loop)))))
               (global.get $false))

         ;;;
         ;;; 4.6 Characters
         ;;;

         ;; https://docs.racket-lang.org/reference/characters.html
         
         (func $raise-check-char (param $x (ref eq)) (unreachable))
         (func $raise-invalid-codepoint (unreachable))

         ;; 4.6.1 Characters and Scalar Values
         
         (func $char? (type $Prim1) (param $v (ref eq)) (result (ref eq))
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

         (func $char->integer (type $Prim1) (param $c (ref eq)) (result (ref eq))
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

         (func $integer->char (type $Prim1)
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

         ;; 4.6.2 Character Comparisons

        ,@(for/list ([$cmp   (in-list '($char=?   $char<?   $char<=?   $char>?   $char>=?))]
                     [$cmp/2 (in-list '($char=?/2 $char<?/2 $char<=?/2 $char>?/2 $char>=?/2))]
                     [inst   (in-list '(#f i32.lt_u i32.le_u i32.gt_u i32.ge_u))])
            ; binary version
            `(func ,$cmp/2 (param $c1 (ref eq)) (param $c2 (ref eq)) (result (ref eq))
                   ;; Ensure both arguments are characters
                   (if (ref.eq (call $char? (local.get $c1)) (global.get $false))
                       (then (call $raise-check-char (local.get $c1))))
                   (if (ref.eq (call $char? (local.get $c2)) (global.get $false))
                       (then (call $raise-check-char (local.get $c2))))
                   ,(if inst
                        `(if (result (ref eq))
                             (,inst (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $c1)))
                                            (i32.const ,char-shift))
                                   (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $c2)))
                                            (i32.const ,char-shift)))
                             (then (global.get $true))
                             (else (global.get $false)))
                        `(return_call $eq? (local.get $c1) (local.get $c2)))))

        ,@(for/list ([$cmp   (in-list '($char=?   $char<?   $char<=?   $char>?   $char>=?))]
                     [$cmp/2 (in-list '($char=?/2 $char<?/2 $char<=?/2 $char>?/2 $char>=?/2))])
            ; variadic version
            `(func ,$cmp (param $c0 (ref eq)) (param $cs (ref eq)) (result (ref eq))
                   (local $node  (ref $Pair))
                   (local $ch    (ref eq))
                   ;; Validate the first argument
                   (if (ref.eq (call $char? (local.get $c0)) (global.get $false))
                       (then (call $raise-check-char (local.get $c0))))
                    (block $done
                           (loop $loop
                                 (br_if $done (ref.eq (local.get $cs) (global.get $null)))
                                 (local.set $node (ref.cast (ref $Pair) (local.get $cs)))
                                 (local.set $ch   (struct.get $Pair $a (local.get $node)))
                                 ;; Validate each subsequent argument
                                 (if (ref.eq (call $char? (local.get $ch)) (global.get $false))
                                     (then (call $raise-check-char (local.get $ch))))
                                 (if (ref.eq (call ,$cmp/2 (local.get $c0) (local.get $ch))
                                             (global.get $false))
                                     (then (return (global.get $false))))
                                 (local.set $cs (struct.get $Pair $d (local.get $node)))
                                (br $loop)))
                   (global.get $true)))

        ,@(for/list ([$cmp   (in-list '($char-ci=?   $char-ci<?   $char-ci<=?   $char-ci>?   $char-ci>=?))]
                     [$cmp/2 (in-list '($char-ci=?/2 $char-ci<?/2 $char-ci<=?/2 $char-ci>?/2 $char-ci>=?/2))]
                     [inst   (in-list '(#f i32.lt_u i32.le_u i32.gt_u i32.ge_u))])
            ; binary case-insensitive version
            `(func ,$cmp/2 (param $c1 (ref eq)) (param $c2 (ref eq)) (result (ref eq))
                   (local $cp1 i32)
                   (local $cp2 i32)
                   (local $fc1 i32)
                   (local $fc2 i32)
                   ;; Ensure both arguments are characters
                   (if (ref.eq (call $char? (local.get $c1)) (global.get $false))
                       (then (call $raise-check-char (local.get $c1))))
                   (if (ref.eq (call $char? (local.get $c2)) (global.get $false))
                       (then (call $raise-check-char (local.get $c2))))
                   ;; Extract codepoints
                   (local.set $cp1 (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $c1)))
                                             (i32.const ,char-shift)))
                   (local.set $cp2 (i32.shr_u (i31.get_u (ref.cast (ref i31) (local.get $c2)))
                                             (i32.const ,char-shift)))
                   ;; Foldcase codepoints
                   (local.set $fc1 (call $char-foldcase/ucs (local.get $cp1)))
                   (local.set $fc2 (call $char-foldcase/ucs (local.get $cp2)))
                   ,(if inst
                        `(if (result (ref eq))
                             (,inst (local.get $fc1) (local.get $fc2))
                             (then (global.get $true))
                             (else (global.get $false)))
                        `(if (result (ref eq))
                             (i32.eq (local.get $fc1) (local.get $fc2))
                             (then (global.get $true))
                             (else (global.get $false))))))

        ,@(for/list ([$cmp   (in-list '($char-ci=?   $char-ci<?   $char-ci<=?   $char-ci>?   $char-ci>=?))]
                     [$cmp/2 (in-list '($char-ci=?/2 $char-ci<?/2 $char-ci<=?/2 $char-ci>?/2 $char-ci>=?/2))])
            ; variadic case-insensitive version
            `(func ,$cmp (param $c0 (ref eq)) (param $cs (ref eq)) (result (ref eq))
                   (local $node  (ref $Pair))
                   (local $ch    (ref eq))
                   ;; Validate the first argument
                   (if (ref.eq (call $char? (local.get $c0)) (global.get $false))
                       (then (call $raise-check-char (local.get $c0))))
                   (block $done
                          (loop $loop
                                (br_if $done (ref.eq (local.get $cs) (global.get $null)))
                                (local.set $node (ref.cast (ref $Pair) (local.get $cs)))
                                (local.set $ch   (struct.get $Pair $a (local.get $node)))
                                ;; Validate each subsequent argument
                                (if (ref.eq (call $char? (local.get $ch)) (global.get $false))
                                    (then (call $raise-check-char (local.get $ch))))
                                (if (ref.eq (call ,$cmp/2 (local.get $c0) (local.get $ch))
                                            (global.get $false))
                                    (then (return (global.get $false))))
                                (local.set $cs (struct.get $Pair $d (local.get $node)))
                                (br $loop)))
                   (global.get $true)))

        ;; 4.6.3 Classifications

        (func $char-whitespace? (type $Prim1) (param $c (ref eq)) (result (ref eq))
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

        ;; 4.6.4 Character Conversions

        (func $char-upcase (type $Prim1) (param $c (ref eq)) (result (ref eq))
              (local $i31   (ref i31))
              (local $c/tag i32)
              (local $cp    i32)
              (local $cp2   i32)
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
              ;; Call host to compute upcase mapping
              (local.set $cp2 (call $char-upcase/ucs (local.get $cp)))
              ;; Return tagged character
              (ref.i31 (i32.or (i32.shl (local.get $cp2) (i32.const ,char-shift))
                               (i32.const ,char-tag))))

        (func $char-downcase (type $Prim1) (param $c (ref eq)) (result (ref eq))
              (local $i31   (ref i31))
              (local $c/tag i32)
              (local $cp    i32)
              (local $cp2   i32)
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
              ;; Call host to compute downcase mapping
              (local.set $cp2 (call $char-downcase/ucs (local.get $cp)))
              ;; Return tagged character
              (ref.i31 (i32.or (i32.shl (local.get $cp2) (i32.const ,char-shift))
                               (i32.const ,char-tag))))

        (func $char-titlecase (type $Prim1) (param $c (ref eq)) (result (ref eq))
              (local $i31   (ref i31))
              (local $c/tag i32)
              (local $cp    i32)
              (local $cp2   i32)
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
              ;; Call host to compute titlecase mapping
              (local.set $cp2 (call $char-titlecase/ucs (local.get $cp)))
              ;; Return tagged character
              (ref.i31 (i32.or (i32.shl (local.get $cp2) (i32.const ,char-shift))
                               (i32.const ,char-tag))))

        ;; Note: JavaScript doesn't have a unicode aware `casefold` so instead
        ;        toLower is used. This is not 100% correct.
        (func $char-foldcase (type $Prim1) (param $c (ref eq)) (result (ref eq))
              (local $i31   (ref i31))
              (local $c/tag i32)
              (local $cp    i32)
              (local $cp2   i32)
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
              ;; Call host to compute foldcase mapping
              (local.set $cp2 (call $char-foldcase/ucs (local.get $cp)))
              ;; Return tagged character
              (ref.i31 (i32.or (i32.shl (local.get $cp2) (i32.const ,char-shift))
                               (i32.const ,char-tag))))

        ;; 4.6.5 Character Grapheme-Cluster Streaming

        ;; todo  char-grapheme-step
        

         ;;;
         ;;; 4.7 SYMBOLS
         ;;;

         ;; https://docs.racket-lang.org/reference/symbols.html

         ;; (type $Symbol
         ;;       (sub $Heap
         ;;            (struct
         ;;              (field $hash          (mut i32))         ;; cached hash                 
         ;;              (field $name          (ref $String))     ;; symbol name (string)        
         ;;              (field $property-list (mut (ref eq))))))  ;; user-defined properties    


         (func $symbol? (type $Prim1) (param $x (ref eq)) (result (ref eq))
               (if (result (ref eq)) (ref.test (ref $Symbol) (local.get $x))
                   (then (global.get $true))
                   (else (global.get $false))))
         
         (func $symbol=? (type $Prim2)
               (param $a (ref eq)) (param $b (ref eq))
               (result (ref eq))
               (if (result (ref eq))
                   (ref.eq (local.get $a) (local.get $b))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $symbol=?/i32 (param $a (ref eq)) (param $b (ref eq)) (result i32)
               (ref.eq (local.get $a) (local.get $b)))


         (func $raise-symbol->string:bad-argument (param $v (ref eq))
               (call $js-log (local.get $v))
               (unreachable))
         
         (func $symbol->string (type $Prim1)
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
         

         (func $symbol->immutable-string (type $Prim1)
               (param  $v (ref eq))
               (result (ref eq))

               (local $sym  (ref $Symbol))
               (local $name (ref $String))
               (local $src  (ref $I32Array))
               (local $dst  (ref $I32Array))
               (local $len  i32)

               (if (ref.test (ref $Symbol) (local.get $v))
                   (then
                    (local.set $sym  (ref.cast (ref $Symbol) (local.get $v)))
                    (local.set $name (struct.get $Symbol $name (local.get $sym)))
                    ;; Already immutable? Return as-is.
                    (if (i32.eq (struct.get $String $immutable (local.get $name)) (i32.const 1))
                        (then (return (local.get $name)))
                        (else
                         ;; Copy codepoints with i32array-copy [0, len)
                         (local.set $src (struct.get $String $codepoints (local.get $name)))
                         (local.set $len (array.len (local.get $src)))
                         (local.set $dst (call $i32array-copy
                                               (local.get $src) (i32.const 0) (local.get $len)))
                         ;; Build fresh immutable string (hash=0, immutable=1)
                         (return
                          (struct.new $String (i32.const 0) (i32.const 1) (local.get $dst))))))
                   (else
                    (call $raise-symbol->string:bad-argument (local.get $v))
                    (unreachable)))
               (unreachable))


         (func $raise-string->symbol:bad-argument (param $v (ref eq)) (unreachable))                  

         (func $string->symbol (type $Prim1)
               (param $v (ref eq))
               (result   (ref eq))  ; An (ref $Symbol)
               
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

         (func $string->uninterned-symbol (type $Prim1)
               (param $v (ref eq))
               (result   (ref eq)) ; An (ref $Symbol)

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

         (func $symbol-interned? (type $Prim1)
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
         
         (func $symbol<? (type $Prim2) (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
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
                          (ref.cast (ref $String)
                            (call $number->string (ref.i31 (local.get $n)) ,(Imm 10))))
               ;; Append prefix and number string
               (ref.cast (ref $String)
                         (call $string-append/2 (local.get $prefix) (local.get $n-str))))

         (func $gensym:0 (result (ref $Symbol))
               ;; Use "g" as default prefix
               (call $gensym:1 (ref.cast (ref $String) (global.get $string:g))))

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
         ;;; 4.8 REGULAR EXPRESSIONS
         ;;;

         ;; https://docs.racket-lang.org/reference/regexp.html

         ;; TODO - Implement regular expressions.

         ;;;
         ;;; 4.9 KEYWORDS
         ;;;

         ;; https://docs.racket-lang.org/reference/keywords.html
         
         ;; Keywords are interned using `the-keywords-table` which maps strings (without #:)
         ;; to keywords.

         (func $keyword?/i32
               (param $v (ref eq))
               (result i32)
               (ref.test (ref $Keyword) (local.get $v)))

         (func $keyword? (type $Prim1)
               (param $v (ref eq))
               (result (ref eq))
               (if (result (ref eq))
                   (ref.test (ref $Keyword) (local.get $v))
                   (then (global.get $true))
                   (else (global.get $false))))
         
         (func $string->keyword (type $Prim1)
               (param $str (ref eq))
               (result (ref eq)) ; an (ref $Keyword)
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
         
         (func $keyword->string (type $Prim1) ; the result does not contain #:
               (param $kw (ref eq))
               (result    (ref eq)) ; an (ref $String)
               ;; Type check: must be a keyword
               (if (i32.eqz (ref.test (ref $Keyword) (local.get $kw)))
                   (then (call $raise-argument-error:keyword-expected (local.get $kw))
                         (unreachable)))
               ;; Cast and delegate
               (call $keyword->string/checked (ref.cast (ref $Keyword) (local.get $kw))))
         
         (func $keyword->string/checked
               (param $kw       (ref $Keyword))
               (result          (ref $String))

               (local $name     (ref $String))
               (local.set $name (struct.get $Keyword $str (local.get $kw)))

               (ref.cast (ref $String) (call $string-copy (local.get $name))))

         (func $keyword->immutable-string (type $Prim1)
               (param $kw (ref eq))
               (result    (ref eq)) ; an (ref $String)
               ;; Type check: must be a keyword
               (if (i32.eqz (ref.test (ref $Keyword) (local.get $kw)))
                   (then (call $raise-argument-error:keyword-expected (local.get $kw))
                         (unreachable)))
               ;; Cast and delegate
               (call $keyword->immutable-string/checked (ref.cast (ref $Keyword) (local.get $kw))))

         (func $keyword->immutable-string/checked
               (param $kw (ref $Keyword))
               (result    (ref $String))

               (local $name (ref $String))
               (local $src  (ref $I32Array))
               (local $dst  (ref $I32Array))
               (local $len  i32)

               (local.set $name (struct.get $Keyword $str (local.get $kw)))

               (if (result (ref $String))
                   (i32.eq (struct.get $String $immutable (local.get $name)) (i32.const 1))
                   (then (local.get $name))
                   (else
                    (local.set $src (struct.get $String $codepoints (local.get $name)))
                    (local.set $len (array.len (local.get $src)))
                    (local.set $dst (call $i32array-copy
                                              (local.get $src) (i32.const 0) (local.get $len)))
                    (struct.new $String (i32.const 0) (i32.const 1) (local.get $dst)))))

         (func $raise-keyword-expected (unreachable))
         
         ;; keyword<? : (ref eq) (ref eq) -> (ref eq)  ;; returns #t/#f
         (func $keyword<? (type $Prim2) (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
               (local $s1 (ref $String))
               (local $s2 (ref $String))

               ;; Type check: both must be keywords (fail early), then convert.
               (if (i32.eqz (ref.test (ref $Keyword) (local.get $a)))
                   (then (call $raise-keyword-expected (local.get $a))
                         (unreachable)))
               (if (i32.eqz (ref.test (ref $Keyword) (local.get $b)))
                   (then (call $raise-keyword-expected (local.get $b))
                         (unreachable)))

               ;; Extract underlying strings (without "#:").
               (local.set $s1
                          (struct.get $Keyword $str (ref.cast (ref $Keyword) (local.get $a))))
               (local.set $s2
                          (struct.get $Keyword $str (ref.cast (ref $Keyword) (local.get $b))))

               ;; Compare using string<? (code-point lexicographic; UTF-8 preserves order)
               (call $string<? (local.get $s1) (local.get $s2)))

         
         ;;;
         ;;; 4.10 Pairs and lists
         ;;;

         ;; https://docs.racket-lang.org/reference/pairs.html
         
         (global $dummy-pair (ref $Pair)
                 (struct.new $Pair
                             (i32.const 0)            ;; hash = 0
                             (global.get $false)      ;; a = null
                             (global.get $false)))    ;; d = null
         
         ;; Pair related exceptions         
         (func $raise-pair-expected (param $x (ref eq)) (unreachable))
         (func $raise-bad-list-ref-index
               (param $xs  (ref $Pair)) (param $i   i32) (param $len i32)
               (unreachable))
         (func $pair? (type $Prim1) (param $v (ref eq)) (result (ref eq))
               (if (result (ref eq)) (ref.test (ref $Pair) (local.get $v))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $null? (type $Prim1) (param $v (ref eq)) (result (ref eq))
               (if (result (ref eq)) (ref.eq (local.get $v) (global.get $null))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $cons (type $Prim2) (param $a (ref eq)) (param $d (ref eq)) (result (ref eq))
               (struct.new $Pair (i32.const 0) (local.get $a) (local.get $d)))

         (func $list* (type $Prim>=1) (param $a (ref eq)) (param $tail (ref eq)) (result (ref eq))
               (struct.new $Pair (i32.const 0) (local.get $a) (local.get $tail)))

         (func $car (type $Prim1) (param $v (ref eq)) (result (ref eq))
               (if (result (ref eq)) (ref.test (ref $Pair) (local.get $v))
                   (then (struct.get $Pair $a (ref.cast (ref $Pair) (local.get $v))))
                   (else (call $raise-pair-expected (local.get $v))
                         (unreachable))))

         (func $cdr (type $Prim1) (param $v (ref eq)) (result (ref eq))
               (if (result (ref eq)) (ref.test (ref $Pair) (local.get $v))
                   (then (struct.get $Pair $d (ref.cast (ref $Pair) (local.get $v))))
                   (else (call $raise-pair-expected (local.get $v))
                         (unreachable))))


         (func $list? (type $Prim1) (param $v (ref eq)) (result (ref eq))
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

         (func $list (type $Prim>=0)
               (param $args (ref eq))
               (result      (ref eq))

               (call $rest-arguments->list
                     (ref.cast (ref $Args) (local.get $args))
                     (i32.const 0)))

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

         (func $length (type $Prim1) (param $xs (ref eq)) (result (ref eq))
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

         
         ;; list-ref/checked: takes a Pair and an unboxed i32 index.
         ;; Works for improper lists as long as the index doesn't step past the last Pair.
         (func $list-ref/checked (param $xs (ref $Pair)) (param $i i32) (result (ref eq))
               (local $v    (ref $Pair))
               (local $k    i32)
               (local $next (ref eq))
               (local $len  i32)

               (local.set $v (local.get $xs))
               (local.set $k (local.get $i))

               (loop $loop
                     ;; If we've reached the desired pair, return its car.
                     (if (i32.eqz (local.get $k))
                         (then (return (struct.get $Pair $a (local.get $v)))))
                     ;; Otherwise, try to step to the next pair.
                     (local.set $next (struct.get $Pair $d (local.get $v)))
                     (if (ref.test (ref $Pair) (local.get $next))
                         (then
                          (local.set $v (ref.cast (ref $Pair) (local.get $next)))
                          (local.set $k (i32.sub (local.get $k) (i32.const 1)))
                          (br $loop))
                         (else
                          ;; Ran out of pairs before reaching index: compute length of the
                          ;; pair-chain we've actually got and raise.
                          ;; len = steps_so_far + 1 = i - k + 1
                          (local.set $len
                                     (i32.add (i32.sub (local.get $i) (local.get $k)) (i32.const 1)))
                          (call $raise-bad-list-ref-index (local.get $xs) (local.get $i) (local.get $len))
                          (unreachable))))
               ;; Should not fall through.
               (unreachable))


         (func $list-ref (type $Prim2) (param $xs (ref eq)) (param $i (ref eq)) (result (ref eq))
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

         ;; list-tail/checked: xs is known to be a Pair and i > 0.
         ;; Returns the result of cdr^i(xs). Works with improper lists:
         ;; if the i-th cdr is a non-pair, it is returned. If we need to
         ;; cdr again past a non-pair, raise pair-expected.
         (func $list-tail/checked (param $xs (ref $Pair)) (param $i i32) (result (ref eq))
               (local $v    (ref $Pair))
               (local $k    i32)
               (local $next (ref eq))

               (local.set $v (local.get $xs))
               (local.set $k (local.get $i))

               (loop $loop
                     ;; If no steps remain, return current tail (a Pair value is fine as (ref eq)).
                     (if (i32.eqz (local.get $k))
                         (then (return (local.get $v))))
                     ;; Step once.
                     (local.set $next (struct.get $Pair $d (local.get $v)))
                     (local.set $k (i32.sub (local.get $k) (i32.const 1)))
                     ;; If that single step completed all steps, return whatever we landed on
                     ;; (pair or not).
                     (if (i32.eqz (local.get $k))
                         (then (return (local.get $next))))
                     ;; Otherwise, we must continue stepping. Ensure next is a Pair.
                     (if (ref.test (ref $Pair) (local.get $next))
                         (then
                          (local.set $v (ref.cast (ref $Pair) (local.get $next)))
                          (br $loop))
                         (else
                          (call $raise-pair-expected (local.get $next)))))
               (unreachable))
         
         (func $list-tail (type $Prim2) (param $xs (ref eq)) (param $i (ref eq)) (result (ref eq))
               (local $pair (ref $Pair))
               (local $idx  i32)
               ;; Initialize non-defaultable local to a safe value.
               (local.set $pair (global.get $dummy-pair))
               ;; Decode and check fixnum index (i31 with lsb=0).
               (if (ref.test (ref i31) (local.get $i))
                   (then
                    (local.set $idx (i31.get_u (ref.cast (ref i31) (local.get $i))))
                    (if (i32.ne (i32.and (local.get $idx) (i32.const 1)) (i32.const 0))
                        (then (call $raise-check-fixnum (local.get $i))))
                    (local.set $idx (i32.shr_u (local.get $idx) (i32.const 1))))
                   (else (call $raise-check-fixnum (local.get $i))))
               ;; (list-tail xs 0) => xs
               (if (i32.eqz (local.get $idx))
                   (then (return (local.get $xs))))
               ;; For idx > 0, xs must be a Pair. Make dominance explicit with `unreachable`.
               (if (ref.test (ref $Pair) (local.get $xs))
                   (then (local.set $pair (ref.cast (ref $Pair) (local.get $xs))))
                   (else
                    (call $raise-pair-expected (local.get $xs))
                    (unreachable)))

               (call $list-tail/checked (local.get $pair) (local.get $idx)))


         (func $append (type $Prim>=0)
               (param $xs (ref eq))        ;; list of arguments
               (result    (ref eq))

               (local $rev (ref eq))
               (local $node (ref $Pair))
               (local $arg (ref eq))
               (local $acc (ref eq))

               ;; Zero arguments -> null
               (if (ref.eq (local.get $xs) (global.get $null))
                   (then (return (global.get $null))))

               ;; Reverse argument list to process from last to first
               (local.set $rev (call $reverse (local.get $xs)))

               ;; Initialize accumulator with last argument
               (local.set $node (ref.cast (ref $Pair) (local.get $rev)))
               (local.set $acc  (struct.get $Pair $a (local.get $node)))
               (local.set $rev  (struct.get $Pair $d (local.get $node)))

               ;; Fold over remaining arguments with $append/2
               (block $done
                      (loop $loop
                            (br_if $done (ref.eq (local.get $rev) (global.get $null)))
                            (local.set $node (ref.cast (ref $Pair) (local.get $rev)))
                            (local.set $arg  (struct.get $Pair $a (local.get $node)))
                            (local.set $acc  (call $append/2 (local.get $arg) (local.get $acc)))
                            (local.set $rev  (struct.get $Pair $d (local.get $node)))
                            (br $loop)))

               (local.get $acc))

         (func $append/2
               (param $xs (ref eq))
               (param $ys (ref eq))
               (result    (ref eq))

               (if (result (ref eq))
                   (ref.eq (local.get $xs) (global.get $null))
                   (then (local.get $ys))  ; "the last list is used directly in the output"
                   (else (if (result (ref eq))
                             (ref.test (ref $Pair) (local.get $xs))
                             (then
                              (struct.new $Pair (i32.const 0)
                                          (struct.get $Pair $a (ref.cast (ref $Pair) (local.get $xs)))
                                          (call $append/2
                                                (struct.get $Pair $d (ref.cast (ref $Pair) (local.get $xs)))
                                                (local.get $ys))))
                             (else (call $raise-pair-expected (local.get $xs))
                                   (unreachable))))))

         (func $reverse (type $Prim1) (param $xs (ref eq)) (result (ref eq))
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
         (func $alt-reverse (type $Prim1) (param $xs (ref eq)) (result (ref eq))
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
         
         (func $memq (type $Prim2) (param $needle (ref eq)) (param $xs (ref eq)) (result (ref eq))
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


         (func $make-list (type $Prim2)
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

         (func $map (type $Prim>=1)
               (param $proc (ref eq))   ;; procedure
               (param $xss  (ref eq))   ;; list of lists
               (result      (ref eq))

               (local $f      (ref $Procedure))
               (local $finv   (ref $ProcedureInvoker))
               (local $outer  (ref eq))
               (local $pair   (ref $Pair))
               (local $elem   (ref eq))
               (local $nlists i32)

               (local $lists  (ref $Args))  ;; cursors for each list
               (local $call   (ref $Args))  ;; args for f (length = nlists)
               (local $i      i32)
               (local $cur    (ref eq))
               (local $stop   i32)

               (local $acc    (ref eq))     ;; reversed accumulator
               (local $res    (ref eq))     ;; final result
               (local $r      (ref eq))

               ;; 1) Check that $proc is a procedure and fetch its invoker
               (if (i32.eqz (ref.test (ref $Procedure) (local.get $proc)))
                   (then (call $raise-argument-error:procedure-expected (local.get $proc))
                         (unreachable)))
               (local.set $f    (ref.cast (ref $Procedure) (local.get $proc)))
               (local.set $finv (struct.get $Procedure $invoke (local.get $f)))

               ;; 2) Walk outer list xss to count #lists; ensure xss is proper and each element is a list head
               (local.set $nlists (i32.const 0))
               (local.set $outer  (local.get $xss))
               (block $count_done
                      (loop $count
                            (if (ref.eq (local.get $outer) (global.get $null))
                                (then (br $count_done)))
                            (if (i32.eqz (ref.test (ref $Pair) (local.get $outer)))
                                (then (call $raise-pair-expected (local.get $outer)) (unreachable)))
                            (local.set $pair (ref.cast (ref $Pair) (local.get $outer)))
                            (local.set $elem (struct.get $Pair $a (local.get $pair)))
                            (if (i32.eqz
                                 (i32.or
                                  (ref.eq (local.get $elem) (global.get $null))
                                  (ref.test (ref $Pair) (local.get $elem))))
                                (then (call $raise-pair-expected (local.get $elem)) (unreachable)))
                            (local.set $nlists (i32.add (local.get $nlists) (i32.const 1)))
                            (local.set $outer (struct.get $Pair $d (local.get $pair)))
                            (br $count)))

               ;; Racket's map requires at least one list argument
               (if (i32.eq (local.get $nlists) (i32.const 0))
                   (then (call $raise-arity-mismatch) (unreachable)))

               ;; 3) Allocate arrays for list cursors and call arguments; seed list cursors from xss
               (local.set $lists (array.new $Args (global.get $null) (local.get $nlists)))
               (local.set $call  (array.new $Args (global.get $null) (local.get $nlists)))

               (local.set $outer (local.get $xss))
               (local.set $i (i32.const 0))
               (block $seed_done
                      (loop $seed
                            (if (i32.ge_u (local.get $i) (local.get $nlists))
                                (then (br $seed_done)))
                            (local.set $pair (ref.cast (ref $Pair) (local.get $outer)))
                            (local.set $elem (struct.get $Pair $a (local.get $pair)))
                            (array.set $Args (local.get $lists) (local.get $i) (local.get $elem))
                            (local.set $outer (struct.get $Pair $d (local.get $pair)))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $seed)))

               ;; 4) Main loop: stop at the shortest list
               (local.set $acc (global.get $null))

               (loop $loop
                     ;; (a) Check state of all lists; determine if we stop
                     (local.set $stop (i32.const 0))
                     (local.set $i (i32.const 0))
                     (block $check_done
                            (loop $check
                                  (if (i32.ge_u (local.get $i) (local.get $nlists))
                                      (then (br $check_done)))
                                  (local.set $cur (array.get $Args (local.get $lists) (local.get $i)))
                                  (if (ref.eq (local.get $cur) (global.get $null))
                                      (then (local.set $stop (i32.const 1)))
                                      (else
                                       (if (i32.eqz (ref.test (ref $Pair) (local.get $cur)))
                                           (then (call $raise-pair-expected (local.get $cur)) (unreachable)))))
                                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                  (br $check)))

                     ;; If any list is empty → finish by reversing $acc
                     (if (i32.ne (local.get $stop) (i32.const 0))
                         (then
                          (local.set $res  (global.get $null))
                          (local.set $cur  (local.get $acc))
                          (loop $rev
                                (if (ref.eq (local.get $cur) (global.get $null))
                                    (then (return (local.get $res))))
                                (local.set $pair (ref.cast (ref $Pair) (local.get $cur)))
                                (local.set $res
                                           (call $cons
                                                 (struct.get $Pair $a (local.get $pair))
                                                 (local.get $res)))
                                (local.set $cur (struct.get $Pair $d (local.get $pair)))
                                (br $rev))))

                     ;; (b) Build call args for f: cars of each list
                     (local.set $i (i32.const 0))
                     (block $cars_done
                            (loop $cars
                                  (if (i32.ge_u (local.get $i) (local.get $nlists))
                                      (then (br $cars_done)))
                                  (local.set $pair
                                             (ref.cast (ref $Pair)
                                                       (array.get $Args (local.get $lists) (local.get $i))))
                                  (array.set $Args
                                             (local.get $call) (local.get $i)
                                             (struct.get $Pair $a (local.get $pair)))
                                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                  (br $cars)))

                     ;; (c) Apply f to those cars
                     (local.set $r
                                (call_ref $ProcedureInvoker
                                          (local.get $f)
                                          (local.get $call)
                                          (local.get $finv)))

                     ;; (d) cons the result onto the accumulator
                     (local.set $acc (call $cons (local.get $r) (local.get $acc)))

                     ;; (e) Advance each list (cdr)
                     (local.set $i (i32.const 0))
                     (block $cdrs_done
                            (loop $cdrs
                                  (if (i32.ge_u (local.get $i) (local.get $nlists))
                                      (then (br $cdrs_done)))
                                  (local.set $pair
                                             (ref.cast (ref $Pair)
                                                       (array.get $Args (local.get $lists) (local.get $i))))
                                  (array.set $Args
                                             (local.get $lists) (local.get $i)
                                             (struct.get $Pair $d (local.get $pair)))
                                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                  (br $cdrs)))

                     (br $loop))
               (unreachable))


         (func $for-each (type $Prim>=1)
               (param $proc (ref eq))   ;; procedure
               (param $xss  (ref eq))   ;; list of lists
               (result      (ref eq))

               (local $f      (ref $Procedure))
               (local $finv   (ref $ProcedureInvoker))
               (local $outer  (ref eq))
               (local $pair   (ref $Pair))
               (local $elem   (ref eq))
               (local $nlists i32)

               (local $lists  (ref $Args))  ;; cursors for each list
               (local $call   (ref $Args))  ;; args for f (length = nlists)
               (local $i      i32)
               (local $cur    (ref eq))
               (local $stop   i32)

               ;; 1) Check that $proc is a procedure and fetch its invoker
               (if (i32.eqz (ref.test (ref $Procedure) (local.get $proc)))
                   (then (call $raise-argument-error:procedure-expected (local.get $proc))
                         (unreachable)))
               (local.set $f    (ref.cast (ref $Procedure) (local.get $proc)))
               (local.set $finv (struct.get $Procedure $invoke (local.get $f)))

               ;; 2) Walk outer list xss to count #lists; ensure xss is proper and each element is a list head
               (local.set $nlists (i32.const 0))
               (local.set $outer  (local.get $xss))
               (block $count_done
                      (loop $count
                            (if (ref.eq (local.get $outer) (global.get $null))
                                (then (br $count_done)))
                            (if (i32.eqz (ref.test (ref $Pair) (local.get $outer)))
                                (then (call $raise-pair-expected (local.get $outer)) (unreachable)))
                            (local.set $pair (ref.cast (ref $Pair) (local.get $outer)))
                            (local.set $elem (struct.get $Pair $a (local.get $pair)))
                            (if (i32.eqz
                                 (i32.or
                                  (ref.eq (local.get $elem) (global.get $null))
                                  (ref.test (ref $Pair) (local.get $elem))))
                                (then (call $raise-pair-expected (local.get $elem)) (unreachable)))
                            (local.set $nlists (i32.add (local.get $nlists) (i32.const 1)))
                            (local.set $outer (struct.get $Pair $d (local.get $pair)))
                            (br $count)))

               ;; Racket's for-each requires at least one list argument
               (if (i32.eq (local.get $nlists) (i32.const 0))
                   (then (call $raise-arity-mismatch) (unreachable)))

               ;; 3) Allocate arrays for list cursors and call arguments; seed list cursors from xss
               (local.set $lists (array.new $Args (global.get $null) (local.get $nlists)))
               (local.set $call  (array.new $Args (global.get $null) (local.get $nlists)))

               (local.set $outer (local.get $xss))
               (local.set $i (i32.const 0))
               (block $seed_done
                      (loop $seed
                            (if (i32.ge_u (local.get $i) (local.get $nlists))
                                (then (br $seed_done)))
                            (local.set $pair (ref.cast (ref $Pair) (local.get $outer)))
                            (local.set $elem (struct.get $Pair $a (local.get $pair)))
                            (array.set $Args (local.get $lists) (local.get $i) (local.get $elem))
                            (local.set $outer (struct.get $Pair $d (local.get $pair)))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $seed)))

               ;; 4) Main loop: stop at the shortest list
               (loop $loop
                     ;; (a) Check state of all lists; determine if we stop
                     (local.set $stop (i32.const 0))
                     (local.set $i (i32.const 0))
                     (block $check_done
                            (loop $check
                                  (if (i32.ge_u (local.get $i) (local.get $nlists))
                                      (then (br $check_done)))
                                  (local.set $cur (array.get $Args (local.get $lists) (local.get $i)))
                                  (if (ref.eq (local.get $cur) (global.get $null))
                                      (then (local.set $stop (i32.const 1)))
                                      (else
                                       (if (i32.eqz (ref.test (ref $Pair) (local.get $cur)))
                                           (then (call $raise-pair-expected (local.get $cur)) (unreachable)))))
                                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                  (br $check)))

                     ;; If any list is empty → return void
                     (if (i32.ne (local.get $stop) (i32.const 0))
                         (then (return (global.get $void))))

                     ;; (b) Build call args for f: cars of each list
                     (local.set $i (i32.const 0))
                     (block $cars_done
                            (loop $cars
                                  (if (i32.ge_u (local.get $i) (local.get $nlists))
                                      (then (br $cars_done)))
                                  (local.set $pair
                                             (ref.cast (ref $Pair)
                                                       (array.get $Args (local.get $lists) (local.get $i))))
                                  (array.set $Args
                                             (local.get $call) (local.get $i)
                                             (struct.get $Pair $a (local.get $pair)))
                                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                  (br $cars)))

                     ;; (c) Apply f to those cars and drop result
                     (drop (call_ref $ProcedureInvoker
                                     (local.get $f)
                                     (local.get $call)
                                     (local.get $finv)))

                     ;; (d) Advance each list (cdr)
                     (local.set $i (i32.const 0))
                     (block $cdrs_done
                            (loop $cdrs
                                  (if (i32.ge_u (local.get $i) (local.get $nlists))
                                      (then (br $cdrs_done)))
                                  (local.set $pair
                                             (ref.cast (ref $Pair)
                                                       (array.get $Args (local.get $lists) (local.get $i))))
                                  (array.set $Args
                                             (local.get $lists) (local.get $i)
                                             (struct.get $Pair $d (local.get $pair)))
                                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                  (br $cdrs)))

                     (br $loop))
               (unreachable))

         (func $filter (type $Prim>=1)
               (param $proc (ref eq))  ;; predicate
               (param $xs   (ref eq))  ;; list
               (result      (ref eq))

               (local $f    (ref $Procedure))
               (local $finv (ref $ProcedureInvoker))
               (local $cur  (ref eq))
               (local $pair (ref $Pair))
               (local $elem (ref eq))
               (local $call (ref $Args))
               (local $r    (ref eq))
               (local $acc  (ref eq))
               (local $res  (ref eq))

               ;; 1) Check that $proc is a procedure and fetch its invoker
               (if (i32.eqz (ref.test (ref $Procedure) (local.get $proc)))
                   (then (call $raise-argument-error:procedure-expected (local.get $proc))
                         (unreachable)))
               (local.set $f    (ref.cast (ref $Procedure) (local.get $proc)))
               (local.set $finv (struct.get $Procedure $invoke (local.get $f)))

               ;; 2) Prepare argument array for predicate
               (local.set $call (array.new $Args (global.get $null) (i32.const 1)))

               ;; 3) Iterate through list, building reversed accumulator
               (local.set $cur (local.get $xs))
               (local.set $acc (global.get $null))
               (loop $loop
                     (if (ref.eq (local.get $cur) (global.get $null))
                         (then
                          ;; Reverse accumulator and return
                          (local.set $res (global.get $null))
                          (local.set $cur (local.get $acc))
                          (loop $rev
                                (if (ref.eq (local.get $cur) (global.get $null))
                                    (then (return (local.get $res))))
                                (local.set $pair (ref.cast (ref $Pair) (local.get $cur)))
                                (local.set $res (call $cons
                                                      (struct.get $Pair $a (local.get $pair))
                                                      (local.get $res)))
                                (local.set $cur (struct.get $Pair $d (local.get $pair)))
                                (br $rev))))

                     (if (i32.eqz (ref.test (ref $Pair) (local.get $cur)))
                         (then (call $raise-pair-expected (local.get $cur))
                               (unreachable)))
                     (local.set $pair (ref.cast (ref $Pair) (local.get $cur)))
                     (local.set $elem (struct.get $Pair $a (local.get $pair)))
                     (array.set $Args (local.get $call) (i32.const 0) (local.get $elem))
                     (local.set $r
                                (call_ref $ProcedureInvoker
                                          (local.get $f)
                                          (local.get $call)
                                          (local.get $finv)))
                     (if (ref.eq (local.get $r) (global.get $false))
                         (then (nop))
                         (else (local.set $acc (call $cons (local.get $elem) (local.get $acc)))))
                     (local.set $cur (struct.get $Pair $d (local.get $pair)))
                     (br $loop))
               (unreachable))



         ;;;
         ;;; 4.11 Mutable Pairs and lists
         ;;;

         ;; https://docs.racket-lang.org/reference/mpairs.html

         ;; TODO - Implement mutable pairs and lists.
         
         ;;;
         ;;; 4.12 Vectors
         ;;;

         ;; https://docs.racket-lang.org/reference/vectors.html
         
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
                            (i32.const 0)          ;; mutable
                            (global.get $dummy-array)))

        ;; Vector related exceptions
        (func $raise-check-vector (param $x (ref eq)) (unreachable))
        (func $raise-check-fixnum (param $x (ref eq)) (unreachable))
        (func $raise-immutable-vector (param $x (ref eq)) (unreachable))
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
         

         (func $make-vector (type $Prim2)
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
                           (i32.const 0)    ;; mutable
                           (local.get $arr)))

         (func $vector (type $Prim>=0)
               (param $args (ref eq))
               (result (ref eq))

               (local $as  (ref $Args))
               (local $len i32)
               (local $arr (ref $Array))

               (local.set $as  (ref.cast (ref $Args) (local.get $args)))
               (local.set $len (array.len (local.get $as)))
               (local.set $arr (call $make-array
                                     (local.get $len) (global.get $false)))
               (array.copy $Array $Args
                           (local.get $arr)
                           (i32.const 0)
                           (local.get $as)
                           (i32.const 0)
                           (local.get $len))
               (struct.new $Vector
                           (i32.const 0)          ;; hash
                           (i32.const 0)          ;; mutable
                           (local.get $arr)))

        (func $vector-immutable (type $Prim>=0)
              (param $args (ref eq))
              (result (ref eq))

              (local $as  (ref $Args))
              (local $len i32)
              (local $arr (ref $Array))

              (local.set $as  (ref.cast (ref $Args) (local.get $args)))
              (local.set $len (array.len (local.get $as)))
              (local.set $arr (call $make-array
                                    (local.get $len) (global.get $false)))
              (array.copy $Array $Args
                          (local.get $arr)
                          (i32.const 0)
                          (local.get $as)
                          (i32.const 0)
                          (local.get $len))
              (struct.new $Vector
                          (i32.const 0)          ;; hash
                          (i32.const 1)          ;; immutable
                          (local.get $arr)))


        (func $vector-length (type $Prim1) (param $v (ref eq)) (result (ref eq))
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

         (func $vector? (type $Prim1) (param $a (ref eq)) (result (ref eq))
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

         (func $vector-ref (type $Prim2)
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

               (if (i32.ne (struct.get $Vector $immutable (local.get $vec)) (i32.const 0))
                   (then (call $raise-immutable-vector (local.get $vec)) (unreachable)))
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
              ;; 3. Check mutability and get length
              (if (i32.ne (struct.get $Vector $immutable (local.get $vec)) (i32.const 0))
                  (then (call $raise-immutable-vector (local.get $v))))
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

         (func $vector-fill! (type $Prim2) (param $v (ref eq)) (param $x (ref eq)) (result (ref eq))
               (local $vec (ref $Vector))
               (local.set $vec (global.get $dummy-vector))
               (if (ref.test (ref $Vector) (local.get $v))
                   (then (local.set $vec (ref.cast (ref $Vector) (local.get $v))))
                   (else (call $raise-check-vector (local.get $v))))
               (if (i32.ne (struct.get $Vector $immutable (local.get $vec)) (i32.const 0))
                   (then (call $raise-immutable-vector (local.get $v))))
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
               (if (i32.ne (struct.get $Vector $immutable (local.get $d)) (i32.const 0))
                   (then (call $raise-immutable-vector (local.get $dest))))
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
               (if (i32.ne (struct.get $Vector $immutable (local.get $dest)) (i32.const 0))
                   (then (call $raise-immutable-vector (local.get $dest)) (unreachable)))
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

         (func $vector-copy (type $Prim3)
               (param $v     (ref eq))
               (param $start (ref eq))   ;; fixnum or $missing, default: 0
               (param $end   (ref eq))   ;; fixnum or $missing, default: (vector-length v)
               (result       (ref eq))

               (local $vec (ref $Vector))
               (local $ss  i32)
               (local $se  i32)
               (local $len i32)

               ;; --- Validate vector ---
               (local.set $vec (global.get $dummy-vector))
               (if (ref.test (ref $Vector) (local.get $v))
                   (then (local.set $vec (ref.cast (ref $Vector) (local.get $v))))
                   (else (call $raise-check-vector (local.get $v))))
               (local.set $len (array.len (struct.get $Vector $arr (local.get $vec))))
               ;; --- Decode $start ---
               (if (ref.eq (local.get $start) (global.get $missing))
                   (then (local.set $ss (i32.const 0)))
                   (else (if (ref.test (ref i31) (local.get $start))
                             (then (local.set $ss (i31.get_u (ref.cast (ref i31) (local.get $start))))
                                   (if (i32.and (local.get $ss) (i32.const 1))
                                       (then (call $raise-check-fixnum (local.get $start))))
                                   (local.set $ss (i32.shr_u (local.get $ss) (i32.const 1))))
                             (else (call $raise-check-fixnum (local.get $start))))))
               ;; --- Decode $end ---
               (if (ref.eq (local.get $end) (global.get $missing))
                   (then (local.set $se (local.get $len)))
                   (else (if (ref.test (ref i31) (local.get $end))
                             (then (local.set $se (i31.get_u (ref.cast (ref i31) (local.get $end))))
                                   (if (i32.and (local.get $se) (i32.const 1))
                                       (then (call $raise-check-fixnum (local.get $end))))
                                   (local.set $se (i32.shr_u (local.get $se) (i32.const 1))))
                             (else (call $raise-check-fixnum (local.get $end))))))
               ;; --- Bounds check: start <= end <= len ---
               (if (i32.or (i32.gt_u (local.get $ss) (local.get $se))
                           (i32.gt_u (local.get $se) (local.get $len)))
                   (then (call $raise-bad-vector-copy-range
                               (local.get $vec) (i32.const 0)
                               (local.get $vec) (local.get $ss) (local.get $se))
                         (unreachable)))
               ;; --- Allocate and return new mutable vector ---
               (struct.new $Vector (i32.const 0)
                           (i32.const 0)
                           (call $array-copy
                                 (struct.get $Vector $arr (local.get $vec))
                                 (local.get $ss) (local.get $se))))

         (func $vector-empty? (type $Prim1) (param $v (ref eq)) (result (ref eq))
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
                           (i32.const 0)
                           (call $array-append
                                 (struct.get $Vector $arr (local.get $va))
                                 (struct.get $Vector $arr (local.get $vb)))))

         (func $vector-take (type $Prim2) (param $v (ref eq)) (param $i (ref eq)) (result (ref eq))
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
                           (i32.const 0)
                           (call $array-take (struct.get $Vector $arr (local.get $vec)) (local.get $ix))))

         (func $vector-drop (type $Prim2) (param $v (ref eq)) (param $i (ref eq)) (result (ref eq))
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
                           (i32.const 0)
                           (call $array-drop (struct.get $Vector $arr (local.get $vec)) (local.get $ix))))

         (func $vector-drop-right (type $Prim2) (param $v (ref eq)) (param $i (ref eq)) (result (ref eq))
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
                           (i32.const 0)
                           (call $array-drop-right (struct.get $Vector $arr (local.get $vec)) (local.get $ix))))

         (func $vector-split-at (type $Prim2)
               (param $v (ref eq))
               (param $i (ref eq))
               (result   (ref eq))  ; returns two values
               
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
               (array.new_fixed $Values 2
                                (struct.new $Vector
                                            (i32.const 0)  ; hash
                                            (i32.const 0)  ; mutable
                                            (ref.cast (ref $Array)
                                                      (array.get $Array (local.get $res) (i32.const 0))))
                                (struct.new $Vector
                                            (i32.const 0) ; hash
                                            (i32.const 0) ; mutable
                                            (ref.cast (ref $Array)
                                                      (array.get $Array (local.get $res) (i32.const 1))))))
         

         (func $raise-expected-vector (unreachable))

         (func $list->vector (type $Prim1)
               (param $xs (ref eq))
               (result    (ref eq))

               (local $arr (ref $Array))

               (local.set $arr (call $list->array (local.get $xs)))
               (struct.new $Vector
                           (i32.const 0)     ; hash
                           (i32.const 0)     ; immutable
                           (local.get $arr)))
         
         (func $vector->list (type $Prim1)
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
         ;;; Boxed (for assignable variables)
         ;;;

         ;; We used `boxed`, `set-boxed!` and `unboxed` for assignable variables.
         ;; These "boxes" are not the same as the Racket datatype `box`.
         ;; See next section.
         
         (func $boxed (type $Prim1) (param $v (ref eq))  (result (ref eq)) 
               (struct.new $Boxed (local.get $v)))

         (func $unboxed (type $Prim1) (param $b (ref eq))  (result (ref eq))
               (struct.get $Boxed $v
                           (block $ok (result (ref $Boxed))
                             (br_on_cast $ok (ref eq) (ref $Boxed) (local.get $b))
                             (unreachable))))

         (func $set-boxed! (type $Prim2)
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

         ;;;
         ;;; 4.14 Boxes
         ;;;

         ;; https://docs.racket-lang.org/reference/boxes.html

         ;; This section implements the Racket data type `box`.
         (func $box (type $Prim1) (param $v (ref eq))  (result (ref eq)) 
               (struct.new $Box (i32.const 0) (local.get $v)))
         (func $unbox (type $Prim1) (param $b (ref eq))  (result (ref eq))
               (struct.get $Box $v
                           (block $ok (result (ref $Box))
                             (br_on_cast $ok (ref eq) (ref $Box) (local.get $b))
                             (return (global.get $error)))))
         (func $set-box! (type $Prim2) ; todo: should this invalidate the hash code?
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
         ;;; HASH TABLES
         ;;;

         (func $hash? (type $Prim1)
               (param $v (ref eq))
               (result   (ref eq))
               
               (if (result (ref eq))
                   (ref.test (ref $Hash) (local.get $v))
                   (then (global.get $true))
                   (else (global.get $false))))
         
         ;;;
         ;;; MUTABLE HASHEQ
         ;;;

         ; We'll use an open-addressing hash table with linear probing for simplicity.
         ; A load of 50% leads to fast lookup - but uses some more memory.

         ; Theory: https://thenumb.at/Hashtables/

         (func $make-empty-hasheq (type $Prim0)
               (result (ref eq))  ; an (ref $HashEqMutable)
               
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
         
         (func $make-hasheq (type $Prim1) ; (make-hasheq [assocs])   - optional without default 
               (param $assocs (ref eq))   ;; Either $missing or an alist of key/value pairs
               (result        (ref eq))   ;; an (ref $HashEqMutable)

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
               (local.set $ht    (ref.cast (ref $HashEqMutable)
                                           (call $make-empty-hasheq)))
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

         (func $raise-hash-ref-key-not-found (param $key (ref eq)) (unreachable))

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
               (local $proc     (ref $Procedure))
               (local $inv      (ref $ProcedureInvoker))
               (local $noargs   (ref $Args))

               ;; Get entries and compute capacity
               (local.set $entries  (struct.get $HashEqMutable $entries (local.get $table)))
               (local.set $capacity (i32.div_u (array.len (local.get $entries)) (i32.const 2)))
               ;; Hash key (identity hash)
               (local.set $hash     (call $eq-hash/i32 (local.get $key)))
               (local.set $index    (i32.rem_u (local.get $hash) (local.get $capacity)))
               (local.set $step     (i32.const 0))
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
               ;; Not found — handle failure result
               (if (result (ref eq))
                   (ref.eq (local.get $fail) (global.get $missing))
                   (then (call $raise-hash-ref-key-not-found (local.get $key))
                         (unreachable))
                   (else
                    (if (result (ref eq))
                        (ref.test (ref $Procedure) (local.get $fail))
                        (then
                         (local.set $proc   (ref.cast (ref $Procedure) (local.get $fail)))
                         (local.set $inv    (struct.get $Procedure $invoke (local.get $proc)))
                         (local.set $noargs (array.new $Args (global.get $null) (i32.const 0)))
                         (return_call_ref $ProcedureInvoker
                                          (local.get $proc)
                                          (local.get $noargs)
                                          (local.get $inv)))
                        (else
                         (local.get $fail))))))


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
         
         (func $hash-remove! (type $Prim2)
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


         (func $hash-has-key? (type $Prim2)
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

         (func $hash-clear! (type $Prim1)
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

         (func $eq-hash-code (type $Prim1)
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
         
         (func $string-port? (type $Prim1)
               (param $v (ref eq))
               (result (ref eq))
               (if (result (ref eq))
                   (ref.test (ref $StringPort) (local.get $v))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $port-next-location (type $Prim1)
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

         (func $open-output-bytes (type $Prim0)
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

         (func $get-output-bytes (type $Prim1)
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
         
         (func $write-byte (type $Prim2)
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

         ;;;
         ;;; FFI Helpers
         ;;;

         (func $js-log (type $Prim1)
               (param $v (ref eq))
               (result   (ref eq))

               (local $len i32)

               (global.set $result-bytes
                           (call $s-exp->fasl (local.get $v) (global.get $false)))
               #;(local.set $len (call $copy_bytes_to_memory (i32.const 0)))
               (local.set $len (call $copy-bytes-to-memory
                                     (global.get $result-bytes) (i32.const 0)))
               (call $js_print_fasl (i32.const 0) (local.get $len))
               (global.get $void))
         
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
               (param $id i32)
               (param $fasl i32)
               (result i32)

               (local $proc  (ref $Procedure))
               (local $vec   (ref $Vector))
               (local $args  (ref $Args))
               (local $res   (ref eq))
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
               ;; Invoke procedure with arguments
               (local.set $res
                          (call_ref $ProcedureInvoker
                                    (local.get $proc)
                                    (local.get $args)
                                    (struct.get $Procedure $invoke (local.get $proc))))
               ;; Encode result and copy to memory for host
               (global.set $result-bytes
                           (call $s-exp->fasl (local.get $res) (global.get $false)))
               (local.set $len
                          (call $copy-bytes-to-memory
                                (ref.cast (ref $Bytes) (global.get $result-bytes))
                                (i32.const 0)))
               (local.get $len))

         (func $procedure->external (export "procedure->external")
               (param $proc (ref eq))
               (result (ref eq))

               (local $p  (ref $Procedure))
               (local $id i32)
               (local $cb (ref extern))

               ;; Fail-early type check
               (if (i32.eqz (ref.test (ref $Procedure) (local.get $proc)))
                   (then (call $raise-argument-error:procedure-expected (local.get $proc))
                         (unreachable)))

               (local.set $p (ref.cast (ref $Procedure) (local.get $proc)))
               (local.set $id (call $callback-register (local.get $p)))
               (local.set $cb (call $js-make-callback (local.get $id)))

               ;; Wrap extern callback as a Racket external value
               (struct.new $External
                           (i32.const 0)
                           (local.get $cb)))

         ;;;
         ;;; 5. STRUCTURES
         ;;;

         ;; TODO
         ;; [x] structure types and super structs
         ;; [x] auto fields
         ;; [ ] prefab structures
         ;; [ ] structure guards
         ;; [ ] applicable structures

         ;; 5.1 Defining Structure Types: struct
         ;; [/] struct                     [syntax]
         ;; [x] struct-field-index         [syntax]
         ;; [x] define-struct              [syntax]
         ;; [x] struct/derived             [syntax]
         ;; [x] define-struct/derived      [syntax]

         ;; 5.2 Creating Structure Types
         ;; [x] make-struct-type
         ;; [x] make-struct-field-accessor
         ;; [x] make-struct-field-mutator
         ;; [ ] prop:sealed                [value]
         ;;       - a structure type property
         ;;       - a sealed struct can not be a supertype of another structure type

         ;; 5.3 Structure Type Properties
         ;; [ ] make-struct-type-property
         ;; [ ] struct-type-property?
         ;; [ ] struct-type-property-accessor-procedure?
         ;; [ ] struct-type-property-predicate-procedure?

         ;; 5.4 Generic Interfaces (racket/generic)
         ;; ...

         ;; 5.5 Copying and Updating Structures
         ;; [ ] struct-copy      [syntax]

         ;; 5.6 Structure Utilities
         ;; [ ] struct->vector
         ;; [x] struct?
         ;; [x] struct-type?
         ;; [x] struct-constructor-procedure?
         ;; [x] struct-predicate-procedure?
         ;; [x] struct-accessor-procedure?
         ;; [x] struct-mutator-procedure?
         ;; [ ] prefab-struct-key
         ;; [ ] make-prefab-struct
         ;; [ ] prefab-struct-type-key+ field-count
         ;; [ ] prefab-key->struct-type
         ;; [ ] prefab-key?

         ;; 5.6.1 Additional Structure Utilities
         ;; [ ] make-constructor-style-printer
         ;; [ ] struct->list
         
         (func $struct?/i32 (param $v (ref eq)) (result i32)
               (ref.test (ref $Struct) (local.get $v)))

         (func $struct? (type $Prim1) (param $v (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.test (ref $Struct) (local.get $v))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $struct-type?/i32 (param $v (ref eq)) (result i32)
               (ref.test (ref $StructType) (local.get $v)))

         (func $struct-type? (type $Prim1) (param $v (ref eq)) (result (ref eq))
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
               (call $growable-array-add! (local.get $out)
                                        (ref.cast (ref $String) (global.get $string:struct-open))) ;; "#(struct "

               ;; Add name
               (call $growable-array-add! (local.get $out)
                     (call $format/display (local.get $name)))

               ;; Add each field
               (local.set $i (i32.const 0))
               (block $done
                      (loop $loop
                            (br_if $done (i32.ge_u (local.get $i) (local.get $n)))
                            (call $growable-array-add! (local.get $out)
                                                       (ref.cast (ref $String)
                                                                (global.get $string:space)))
                            (call $growable-array-add! (local.get $out)
                                  (call $format/display
                                        (array.get $Array (local.get $fields) (local.get $i))))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $loop)))

               ;; Close output
               (call $growable-array-add! (local.get $out)
                                        (ref.cast (ref $String)
                                                 (global.get $string:close-paren)))
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
                               (call $append/2
                                     (struct.get $StructType $init-indices (local.get $super-typed))
                                     (call $list-from-range/checked
                                           (local.get $stfc)
                                           (i32.add (local.get $stfc) (local.get $ifc)))))
                    (local.set $auto-indices
                               (call $append/2
                                     (struct.get $StructType $auto-indices (local.get $super-typed))
                                     (call $list-from-range/checked
                                           (i32.add (local.get $stfc) (local.get $ifc))
                                           (i32.add (local.get $stfc) (i32.add (local.get $ifc) (local.get $afc))))))

                    (local.set $auto-values
                               (call $append/2
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
                               (call $append/2
                                     (struct.get $StructType $init-indices (local.get $super-typed))
                                     (call $list-from-range/checked (local.get $stfc) (local.get $ifc))))
                    (local.set $auto-indices
                               (call $append/2
                                     (struct.get $StructType $auto-indices (local.get $super-typed))
                                     (call $list-from-range/checked
                                           (i32.add (local.get $stfc) (local.get $ifc))
                                           (local.get $afc))))
                    (local.set $auto-values
                               (call $append/2
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
                           (ref.i31 (i32.const 4))     ; arity: 2
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
              (ref.func $primitive-invoke)
              (ref.func $code:case-lambda-dispatch)
              (ref.func $invoke-case-closure)
              #;(ref.func $struct-constructor/with-guard)
              
              ;;; Declare all primitives
              ,@(for/list ([pr (in-list (sort (remove* todo-handle-later primitives) symbol<?))])
                  `(ref.func ,($ pr))))

         
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
               (local $arity        i32)

               ;; Extract descriptor data
               (local.set $field-count   (struct.get $StructType $field-count      (local.get $std)))
               (local.set $guard         (struct.get $StructType $guard            (local.get $std)))
               (local.set $name          (struct.get $StructType $name             (local.get $std)))
               (local.set $init-indices  (struct.get $StructType $init-indices     (local.get $std)))
               (local.set $auto-indices  (struct.get $StructType $auto-indices     (local.get $std)))
               (local.set $auto-values   (struct.get $StructType $auto-values      (local.get $std)))
               (local.set $arity        (call $length/i32 (local.get $init-indices)))

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
                           (ref.i31 (i32.shl (local.get $arity) (i32.const 1))) ; arity
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
                           (ref.i31 (i32.const 6))     ; arity: 3
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
                           (ref.i31 (i32.const 4))     ; arity: 2
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
                           (ref.i31 (i32.const 2))     ; arity: 1
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
                           (ref.i31 (i32.const 2))     ; arity: 1
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


         (func $struct-constructor-procedure? (type $Prim1) (param $v (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.test (ref $StructConstructorProcedure) (local.get $v))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $struct-predicate-procedure? (type $Prim1) (param $v (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.test (ref $StructPredicateProcedure) (local.get $v))
                   (then (global.get $true))
                   (else (global.get $false))))
         
         (func $struct-accessor-procedure? (type $Prim1) (param $v (ref eq)) (result (ref eq))
               (if (result (ref eq))
                   (ref.test (ref $StructAccessorProcedure) (local.get $v))
                   (then (global.get $true))
                   (else (global.get $false))))

         (func $struct-mutator-procedure? (type $Prim1) (param $v (ref eq)) (result (ref eq))
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

         (func $procedure? (type $Prim1)
               (param $v (ref eq))
               (result   (ref eq))

               (if (result (ref eq))
                   (ref.test (ref $Procedure) (local.get $v))
                   (then (global.get $true))
                   (else (global.get $false))))

         ; TODO: Revise `apply` to handle:
         ;           (apply proc v ... xs)
         ; We are not handling v ... at the moment.

         ; Notes: repacking of arguments are done in $invoke-closure,
         ;        so no repacking is needed in $apply.
         
         (func $apply #;(type $Prim>=2)  ;; TODO - Implement full `append` and add type
               (param $proc (ref eq))    ;; procedure
               (param $xs   (ref eq))    ;; list of arguments
               (result      (ref eq))    ;; result of applying the procedure

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
                                (struct.get $PrimitiveClosure $code (ref.cast (ref $PrimitiveClosure) (local.get $proc)))
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
                                (struct.get $PrimitiveProcedure $code   (ref.cast (ref $PrimitiveProcedure) (local.get $proc)))
                                (struct.get $PrimitiveProcedure $result-arity
                                            (ref.cast (ref $PrimitiveProcedure) (local.get $proc)))))))
               ;; Step 5: Not a supported procedure type
               (call $raise-argument-error:procedure-expected)
               (unreachable))
         
         
         (func $raise-argument-error:procedure-expected (unreachable))
         
         (func $procedure-arity (type $Prim1)
               ; Wrapper: accepts any value, checks that it’s a procedure, then delegates
               ; to the checked version that expects (ref $Procedure).
               (param $proc (ref eq))
               (result      (ref eq))
               ;; 1. Check that $proc is a procedure
               (if (i32.eqz (ref.test (ref $Procedure) (local.get $proc)))
                   (then (call $raise-argument-error:procedure-expected (local.get $proc))
                         (unreachable)))
               ;; 2. Delegate to the checked implementation
               (return_call $procedure-arity/checked
                            (ref.cast (ref $Procedure) (local.get $proc))))

         (func $procedure-arity/checked
               ; TODO: If you want Racket-style results, convert any negative marker m to
               ;       a single (arity-at-least (-m - 1)) object (possibly combined with
               ;       exact integers). For now we return ALL markers as fixnums, including
               ;       negatives like -2.
               (param $p (ref $Procedure))
               (result (ref eq))

               (local $a    (ref eq))
               (local $arr  (ref $I32Array))
               (local $n    i32) (local $i i32) (local $m i32)
               (local $list (ref eq))
               (local $fx (ref i31))

               ;; 1. Extract arity field (either a fixnum (ref i31) or an $I32Array of markers)
               (local.set $a (struct.get $Procedure $arity (local.get $p)))
               ;; 2. If it’s a single-arity fixnum, return it as-is
               (if (ref.test (ref i31) (local.get $a))
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
                     ;; 5. Read marker m at index i and cons it as a fixnum (handles m >= 0 and m < 0)
                     (local.set $m (array.get $I32Array (local.get $arr) (local.get $i)))
                     (local.set $fx (ref.i31 (i32.shl (local.get $m) (i32.const 1))))
                     (local.set $list (call $cons (local.get $fx) (local.get $list)))
                     ;; 6. Decrement i and continue
                     (local.set $i (i32.sub (local.get $i) (i32.const 1)))
                     (br $rev))
               (unreachable))




         
         (func $procedure-arity-mask (type $Prim1)
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
         
         (func $primitive? (type $Prim1)
               (param $v (ref eq))
               (result (ref eq))

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
                   (then (call $raise-argument-error:primitive-procedure-expected)))
               ;; Step 2: Cast after validation
               (local.set $p (ref.cast (ref $PrimitiveProcedure) (local.get $v)))
               ;; Step 3: Return result-arity field
               (struct.get $PrimitiveProcedure $result-arity (local.get $p)))


         (func $primitive-closure? (type $Prim1)
               (param $v (ref eq))
               (result (ref eq))

               (if (result (ref eq))
                   (ref.test (ref $PrimitiveClosure) (local.get $v))
                   (then (global.get $true))
                   (else (global.get $false))))

         ;;;
         ;;;  4.21 Void
         ;;;

         ;; https://docs.racket-lang.org/reference/void.html
         
         (func $void? (type $Prim1)
               (param $v (ref eq))
               (result   (ref eq))

               (if (result (ref eq))
                   (ref.eq (local.get $v) (global.get $void))
                   (then (global.get $true))
                   (else (global.get $false))))
         
         (func $void (type $Prim>=0)
               (param $xs (ref eq))
               (result    (ref eq))

               (global.get $void))

         (func $make-void (type $Prim0) (result (ref eq)) ; no arguments
               (return (global.get $void)))


         ;;;
         ;;; 10. CONTROL FLOW
         ;;;

         ;; 10.1 Multiple Values

         (func $values (type $Prim>=0)
               (param $args (ref eq))
               (result      (ref eq))

               (local $as   (ref $Args))
               (local $n    i32)
               (local $vals (ref $Values))
               (local $i    i32)

               ;; Cast argument array
               (local.set $as (ref.cast (ref $Args) (local.get $args)))
               (local.set $n  (array.len (local.get $as)))
               ;; Single argument -> return directly
               (if (i32.eq (local.get $n) (i32.const 1))
                   (then (return (array.get $Args (local.get $as) (i32.const 0)))))
               ;; Allocate $Values array

              ;; Allocate array and copy arguments
              (local.set $vals (array.new $Values (global.get $null) (local.get $n)))
              (array.copy $Values $Args
                          (local.get $vals) (i32.const 0)
                          (local.get $as)   (i32.const 0)
                          (local.get $n))
              (local.get $vals)
               
              ;; (local.set $vals
              ;;            (array.new_default $Values (global.get $false) (local.get $len)))
              ;; (local.set $i (i32.const 0))
              ;; (block $done
              ;;        (loop $loop
              ;;              (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
              ;;              (array.set $Values (local.get $vals) (local.get $i)
              ;;                         (array.get $Args (local.get $as) (local.get $i)))
              ;;              (local.set $i (i32.add (local.get $i) (i32.const 1)))
              ;;              (br $loop)))
              ;; (local.get $vals)
              )

         (func $call-with-values (type $Prim2)
               (param $gen (ref eq))
               (param $rec (ref eq))
               (result     (ref eq))

               (local $g     (ref $Procedure))
               (local $r     (ref $Procedure))
               (local $ginv  (ref $ProcedureInvoker))
               (local $rinv  (ref $ProcedureInvoker))
               (local $vals  (ref eq))
               (local $vals* (ref $Values))
               (local $args  (ref $Args))
               (local $n     i32)

               ;; initialize $args to satisfy the validator; overwritten later
               (local.set $args (array.new $Args (global.get $null) (i32.const 0)))

               ;; Step 1: type check generator
               (if (i32.eqz (ref.test (ref $Procedure) (local.get $gen)))
                   (then (call $raise-argument-error:procedure-expected (local.get $gen))
                         (unreachable)))
               (local.set $g    (ref.cast (ref $Procedure) (local.get $gen)))

               ;; Step 2: type check receiver
               (if (i32.eqz (ref.test (ref $Procedure) (local.get $rec)))
                   (then (call $raise-argument-error:procedure-expected (local.get $rec))
                         (unreachable)))
               (local.set $r    (ref.cast (ref $Procedure) (local.get $rec)))

               ;; Step 3: call generator with zero arguments
               (local.set $ginv (struct.get $Procedure $invoke (local.get $g)))
               (local.set $vals
                          (call_ref $ProcedureInvoker
                                    (local.get $g)
                                    (array.new $Args (global.get $null) (i32.const 0))
                                    (local.get $ginv)))

               ;; Step 4: unpack returned values into argument array
               (if (ref.test (ref $Values) (local.get $vals))
                   (then (local.set $vals* (ref.cast (ref $Values) (local.get $vals)))
                         (local.set $n     (array.len (local.get $vals*)))
                         (local.set $args  (array.new $Args (global.get $null) (local.get $n)))
                         (array.copy $Args $Values
                                     (local.get $args) (i32.const 0)
                                     (local.get $vals*) (i32.const 0)
                                     (local.get $n)))
                   (else (local.set $args (array.new_fixed $Args 1 (local.get $vals)))))

               ;; Step 5: call receiver in tail position
               (local.set $rinv (struct.get $Procedure $invoke (local.get $r)))
               (return_call_ref $ProcedureInvoker
                                (local.get $r)
                                (local.get $args)
                                (local.get $rinv)))
         

         
         ;; 10.2 Exceptions
         ;; 10.3 Delayed Evaluation
         ;; 10.4 Continuations
         ;; 10.5 Continuation Marks
         ;; 10.6 Breaks
         ;; 10.7 Exiting
         ;; 10.8 Unreachable Expressions
         

         ;;;
         ;;; 13. INPUT AND OUTPUT
         ;;;

         ;; 13.10 Fast-Load Serialization

         (global $fasl-fixnum     (ref i31) ,(Imm 0))
         (global $fasl-character  (ref i31) ,(Imm 1))
         (global $fasl-symbol     (ref i31) ,(Imm 2))
         (global $fasl-string     (ref i31) ,(Imm 3))
         (global $fasl-bytes      (ref i31) ,(Imm 4))
         (global $fasl-boolean    (ref i31) ,(Imm 5))
         (global $fasl-null       (ref i31) ,(Imm 6))
         (global $fasl-pair       (ref i31) ,(Imm 7))
         (global $fasl-vector     (ref i31) ,(Imm 8))
         (global $fasl-flonum     (ref i31) ,(Imm 9))
         (global $fasl-void       (ref i31) ,(Imm 10))
         (global $fasl-eof        (ref i31) ,(Imm 11))
         (global $fasl-external   (ref i31) ,(Imm 12))
         
         (func $s-exp->fasl (type $Prim2)
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
                                                                      (local.set $n   (array.len (local.get $arr)))
                                                                      (call $fasl:write-u32 (local.get $n) (local.get $out))

                                                                      (local.set $i (i32.const 0))
                                                                      (block $break
                                                                             (loop $loop
                                                                                   (br_if $break (i32.ge_u (local.get $i) (local.get $n)))
                                                                                   (call $fasl:s-exp->fasl
                                                                                         (array.get $Array (local.get $arr) (local.get $i))
                                                                                         (local.get $out))
                                                                                   (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                                                                   (br $loop))))
                                                                     (else
                                                                      (if (ref.test (ref $External) (local.get $v))
                                                                          (then
                                                                           (drop (call $write-byte (global.get $fasl-external) (local.get $out)))
                                                                           (call $fasl:write-u32
                                                                                 (call $js-register-external
                                                                                       (ref.as_non_null
                                                                                         (struct.get $External $v
                                     (ref.cast (ref $External) (local.get $v)))))
                                                                                 (local.get $out)))
                                                                          (else (unreachable)))))))))))))))))) ;; unsupported type
         
         (func $s-exp->fasl/immediate
               (param $i   i32)
               (param $v   (ref eq))
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
              (param $b   (ref $Bytes))
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
                  
         (func $fasl->s-exp (type $Prim1)
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
              
              (return (ref.cast (ref $Symbol)
                                (call $string->symbol (local.get $str)))
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
              (local $idx  i32)
              
              (local.set $tag (array.get_u $I8Array (local.get $arr) (local.get $i)))
              (local.set $tag (i32.shl (local.get $tag) (i32.const 1))) ; as fixnum
              (local.set $i   (i32.add (local.get $i)   (i32.const 1)))

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
               (else
                ;; external
                (if (result (ref eq) i32)
                    (i32.eq (local.get $tag) (i31.get_u (global.get $fasl-external)))
                    (then (call $fasl:read-u32 (local.get $arr) (local.get $i))
                          (local.set $i) (local.set $idx)
                          (return (struct.new $External
                                              (i32.const 0)
                                              (call $js-lookup-external (local.get $idx)))
                                  (local.get $i)))
                    (else (unreachable))))))))))))))))))))))))))))


        (func $copy-memory-to-i8array (export "copy-memory-to-i8array")
              (param $start i32)
              (result (ref $I8Array) i32)

              (local $mem-bytes i32)
              (local $len       i32)
              (local $arr       (ref $I8Array))
              (local $i         i32)
              (local $end       i32)
              (local $res       (ref $I8Array))

              (local.set $mem-bytes (i32.mul (memory.size) (i32.const 65536)))
              (local.set $len (i32.sub (local.get $mem-bytes) (local.get $start)))
              (local.set $arr (array.new_default $I8Array (local.get $len)))
              (local.set $i (i32.const 0))
              (block $done
                     (loop $copy
                           (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
                           (array.set $I8Array (local.get $arr) (local.get $i)
                                      (i32.load8_u
                                       (i32.add (local.get $start) (local.get $i))))
                           (local.set $i (i32.add (local.get $i) (i32.const 1)))
                           (br $copy)))

              (call $fasl:read-s-exp (local.get $arr) (local.get $len) (i32.const 0))
              (local.set $end) (drop)

              (local.set $res (call $i8array-copy (local.get $arr) (i32.const 0) (local.get $end)))
              (return (local.get $res) (i32.add (local.get $start) (local.get $end))))

        (func $linear-memory->value (export "linear-memory->value")
              (param $start i32)
              (result (ref eq))

              (local $arr (ref $I8Array))
              (local $len i32)
              (local $val (ref eq))

              (call $copy-memory-to-i8array (local.get $start))
              (local.set $len) (local.set $arr)
              (call $fasl:read-s-exp (local.get $arr) (local.get $len) (i32.const 0))
              (local.set $len) (local.set $val)
              (local.get $val))

        (func $linear-memory->string (export "linear-memory->string")
              (param $start i32)
              (result (ref eq))

              (local $v (ref eq))

              (local.set $v (call $linear-memory->value (local.get $start)))
              (if (i32.eqz (ref.test (ref $String) (local.get $v)))
                  (then (call $raise-expected-string)
                        (unreachable)))
              (return (ref.cast (ref $String) (local.get $v))))


        ;;;
        ;;; 14. REFLECTION AND SECURITY
        ;;;

        ;; 14.1 Namespaces
        
        (func $namespace? (type $Prim1)
              (param $v (ref eq))
              (result   (ref eq))

              (if (result (ref eq))
                  (ref.test (ref $Namespace) (local.get $v))
                  (then (global.get $true))
                  (else (global.get $false))))
        
        ; We need dummy implementations of `#%variable-reference` and `variable-reference-from-unsafe?`
        ; in order to run code from an expand `for`.

        ; The form `#%variable-reference` can occur in a fully expanded syntax,
        ; so it is handled in the elsewhere (for now, we only handle the case `(#%variable-reference)`.

        ; This function determines if the variable stems from a module compiled in unsafe mode or not.
        (func $variable-reference-from-unsafe? (type $Prim1)
              (param  $varref (ref eq))
              (result (ref eq))
              (global.get $true))

        (func $variable-reference-constant? (type $Prim1)
              (param $varref (ref eq))
              (result (ref eq))
              (global.get $true))  ; todo: simple implementation for now.

        (func $raise-unbound-variable-reference (type $Prim1)
              (param $name (ref eq))
              (result (ref eq))
              (drop (call $js-log (local.get $name)))
              (unreachable))
         
         ;; 14.2 Evaluation and compilation
         ;; 14.3 The racket/load language
         ;; 14.4 Module names and loading
         ;; 14.5 Impersonators and chaperones
         ;; 14.6 Security Guards
         ;; 14.7 Custodians
         ;; 14.8 Thread Groups
         ;; 14.9 Structure Inspectors

         (func $current-inspector (type $Prim0)   ; TODO: dummy 
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

         (func $unsafe-fx+ (type $Prim2)
               (param $x (ref eq))   ;; x must be a fixnum (i31 with lsb = 0)
               (param $y (ref eq))   ;; y must be a fixnum (i31 with lsb = 0)
               (result   (ref eq))   ;; result is a fixnum (i31)

               ; the tag was chosen, so we could do this:
               (ref.i31 (i32.add (i31.get_s (ref.cast (ref i31) (local.get $x)))
                                 (i31.get_s (ref.cast (ref i31) (local.get $y))))))

         (func $unsafe-fx= (type $Prim2)
               (param $x (ref eq))
               (param $y (ref eq))
               (result   (ref eq))

               (if (result (ref eq))
                   (i32.eq (i31.get_s (ref.cast (ref i31) (local.get $x)))
                           (i31.get_s (ref.cast (ref i31) (local.get $y))))
                   (then (global.get $true))
                   (else (global.get $false))))
         
         (func $unsafe-fx< (type $Prim2)
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

         (func $unsafe-car (type $Prim1) (param $v (ref eq)) (result (ref eq))
               (struct.get $Pair $a (ref.cast (ref $Pair) (local.get $v))))

         (func $unsafe-cdr (type $Prim1) (param $v (ref eq)) (result (ref eq))
               (struct.get $Pair $d (ref.cast (ref $Pair) (local.get $v))))

         (func $unsafe-struct-ref (type $Prim2)
               (param $v (ref eq))
               (param $k (ref eq))
               (result (ref eq))
               (array.get $Array
                          (struct.get $Struct $fields
                                      (ref.cast (ref $Struct) (local.get $v)))
                          (i32.shr_s (i31.get_s (ref.cast (ref i31)
                                                          (local.get $k)))
                                     (i32.const 1))))

         ;; Note the `unsafe-vector*-...` variants do not work on impersonators.
         ;; (the `unsafe-vector-...` variants do)
         (func $unsafe-vector*-length (type $Prim1) (param $v (ref eq)) (result (ref eq))
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

        (func $unsafe-struct-set!
              (param $struct (ref eq))
              (param $idx    (ref eq))
              (param $val    (ref eq))
              (result        (ref eq))

              (array.set $Array
                         (struct.get $Struct $fields
                                     (ref.cast (ref $Struct) (local.get $struct)))
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
         (func $i8array->string (param $arr (ref $I8Array)) (result (ref $String))
               (call $bytes->string/utf-8/checked
                     (call $i8array->immutable-bytes
                            (local.get $arr))))

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
                             (then (return (ref.cast (ref $String)
                                                     (call $number->string (local.get $v) ,(Imm 10))))))))
               ;; --- Case: null ---
               (if (ref.eq (local.get $v) (global.get $null))
                   (then (return (ref.cast (ref $String) (global.get $string:null)))))
               ;; --- Case: true ---
               (if (ref.eq (local.get $v) (global.get $true))
                   (then (return (ref.cast (ref $String) (global.get $string:hash-t)))))
               ;; --- Case: false ---
               (if (ref.eq (local.get $v) (global.get $false))
                   (then (return (ref.cast (ref $String) (global.get $string:hash-f)))))
               ;; --- Case: void ---
               (if (ref.eq (local.get $v) (global.get $void))
                   (then (return (ref.cast (ref $String) (global.get $string:void)))))
               ;; --- Case: missing ---
               (if (ref.eq (local.get $v) (global.get $missing))
                   (then (return (ref.cast (ref $String) (global.get $string:missing)))))
               ;; --- Case: undefined ---
               (if (ref.eq (local.get $v) (global.get $undefined))
                   (then (return (ref.cast (ref $String) (global.get $string:undefined)))))
               ;; --- Case: closure ---
               (if (ref.test (ref $Closure) (local.get $v))
                   (then (return (call $format/display:procedure
                                       (ref.cast (ref $Procedure) (local.get $v))))))
               ;; --- Case: case-lambda ---
               (if (ref.test (ref $CaseClosure) (local.get $v))
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
                    (then (return (call $format/display:bytes
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
                   (then (return (call $format/display:keyword
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
               ;; --- Case: external ---
               (if (ref.test (ref $External) (local.get $v))
                   (then (return (call $format/display:external
                                       (ref.cast (ref $External) (local.get $v))))))
               ;; --- Case: char ---
               (if (ref.test (ref i31) (local.get $v))
                   (then (local.set $i31 (ref.cast (ref i31) (local.get $v)))
                         (local.set $n   (i31.get_u (local.get $i31)))
                         (if (i32.eq (i32.and (local.get $n) (i32.const ,char-mask))
                                     (i32.const ,char-tag))
                             (then (return (call $format/display:char (local.get $v)))))))
               ;; --- Case: namespace ---
               (if (ref.test (ref $Namespace) (local.get $v))
                   (then (return (call $format/display:namespace
                                       (ref.cast (ref $Namespace) (local.get $v))))))
               ;; --- Case: variable-reference ---
               (if (ref.test (ref $VariableReference) (local.get $v))
                   (then (return (call $format/display:variable-reference
                                       (ref.cast (ref $VariableReference) (local.get $v))))))
               ;; --- Internal data types ---
               ;; These shouldn't leak to the outside, but nice to know if it happens.
               ;; --- Case: boxed --- (shouldn't happen)
               (if (ref.test (ref $Boxed) (local.get $v))
                   #;(then (call $raise-format/display:got-boxed))
                   (then (return (call $format/display:boxed
                                       (ref.cast (ref $Boxed) (local.get $v))))))
               ;; --- Fallback ---
               (call $raise-format/display:unknown-datatype)
               (unreachable))

         (func $format/display:variable-reference
               (param $v (ref eq))
               (result   (ref $String))
               (ref.cast (ref $String)
                         (global.get $string:hash-variable-reference)))

        (func $format/display:namespace
              (param $ns (ref $Namespace))
              (result (ref $String))

              (local $name (ref eq))
              (local $out  (ref $GrowableArray))

              ;; Extract namespace name
              (local.set $name (struct.get $Namespace $name (local.get $ns)))
              ;; If no name, return constant
              (if (result (ref $String))
                  (ref.eq (local.get $name) (global.get $false))
                  (then (ref.cast (ref $String)
                                  (global.get $string:namespace)))
                  (else ;; Build "#<namespace:NAME>"
                        (local.set $out (call $make-growable-array (i32.const 3)))
                        (call $growable-array-add! (local.get $out)
                              (ref.cast (ref $String)
                                        (global.get $string:hash-less-namespace-colon)))
                        (call $growable-array-add! (local.get $out)
                              (ref.cast (ref $String) (local.get $name)))
                        (call $growable-array-add! (local.get $out)
                              (ref.cast (ref $String)
                                        (global.get $string:->)))
                        (call $growable-array-of-strings->string (local.get $out)))))

        (func $format/display:external
              (param $v (ref $External))
              (result (ref $String))
              (if (result (ref $String))
                  (ref.is_null (struct.get $External $v (local.get $v)))
                  (then (ref.cast (ref $String)
                                   (global.get $string:external-null)))
                  (else (ref.cast (ref $String)
                                   (global.get $string:external)))))

         (func $format/display:procedure
               ; #<procedure:name:arity:mask>
               (param $v          (ref eq))
               (result            (ref $String))

               (local $p          (ref $Procedure))
               (local $name       (ref eq))         ;; $false or $String
               #;(local $arity-fx   (ref eq))         ;; fixnum (i31)
               #;(local $arity-str  (ref $String))
               (local $mask       i32)
               (local $mask-str   (ref $String))
               (local $ga         (ref $GrowableArray))

               ;; Step 1: type check and cast
               (if (i32.eqz (ref.test (ref $Procedure) (local.get $v)))
                   (then (call $raise-argument-error:procedure-expected)))
               (local.set $p (ref.cast (ref $Procedure) (local.get $v)))
               ;; Step 2: extract fields
               (local.set $name      (struct.get $Procedure $name (local.get $p)))
               #;(local.set $arity-fx  (struct.get $Procedure $arity (local.get $p)))
               ;; Step 3: convert arity to string
               #;(local.set $arity-str (call $number->string (local.get $arity-fx) (global.get $false)))
               ;; Step 4: get mask and convert to string
               ; (local.set $mask      (call $procedure-arity-mask/checked/i32 (local.get $p)))
               ; (local.set $mask-str  (call $i32->string                      (local.get $mask)))
               ;; Step 5: build output
               (local.set $ga (call $make-growable-array (i32.const 5)))
               (call $growable-array-add! (local.get $ga)
                     (ref.cast (ref $String)
                               (global.get $string:hash-less-procedure-colon)))
               (call $growable-array-add! (local.get $ga)
                     (if (result (ref eq))
                         (ref.eq (local.get $name) (global.get $false))
                         (then (ref.cast (ref $String)
                                         (global.get $string:unknown)))
                         (else (local.get $name))))
               #;(call $growable-array-add! (local.get $ga)
                     (ref.cast (ref $String)
                               (global.get $string:colon)))
               #;(call $growable-array-add! (local.get $ga) (local.get $arity-str))
               #;(call $growable-array-add! (local.get $ga)
                     (ref.cast (ref $String)
                               (global.get $string:colon)))
               #;(call $growable-array-add! (local.get $ga) (local.get $mask-str))
               (call $growable-array-add! (local.get $ga)
                     (ref.cast (ref $String)
                               (global.get $string:->)))
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
               (local.set $arity-str (ref.cast (ref $String)
                                               (call $number->string
                                                     (local.get $arity-fx) (global.get $false))))
               ;; Step 4: get mask and convert to string
               (local.set $mask      (call $procedure-arity-mask/checked/i32 (local.get $p)))
               (local.set $mask-str  (call $i32->string                      (local.get $mask)))
               ;; Step 5: build output
               (local.set $ga (call $make-growable-array (i32.const 5)))
               (call $growable-array-add! (local.get $ga)
                                        (ref.cast (ref $String)
                                                 (global.get $string:hash-less-primitive-colon)))
               (call $growable-array-add!
                     (local.get $ga)
                     (if (result (ref eq))
                         (ref.eq (local.get $name) (global.get $false))
                         (then (ref.cast (ref $String)
                                          (global.get $string:unknown)))
                         (else (call $symbol->string (local.get $name)))))
               (call $growable-array-add! (local.get $ga)
                                        (ref.cast (ref $String)
                                                 (global.get $string:colon)))
               (call $growable-array-add! (local.get $ga) (local.get $arity-str))
               (call $growable-array-add! (local.get $ga)
                                        (ref.cast (ref $String)
                                                 (global.get $string:colon)))
               (call $growable-array-add! (local.get $ga) (local.get $mask-str))
               (call $growable-array-add! (local.get $ga)
                                        (ref.cast (ref $String)
                                                 (global.get $string:->)))
               ;; Step 6: convert to string
               (call $array-of-strings->string
                     (call $growable-array->array (local.get $ga))))


         (func $format/display:symbol
               (param $v (ref eq))
               (result   (ref $String))

               (call $format/display
                     (call $symbol->string
                           (local.get $v))))

         (func $format/display:keyword
               (param $v (ref eq))
               (result (ref $String))
               ;; Fail early if not a keyword (and make the path unreachable).
               (if (i32.eqz (ref.test (ref $Keyword) (local.get $v)))
                   (then
                    (call $raise-keyword-expected (local.get $v))
                    (unreachable)))
               ;; "#:" ++ underlying name string
               (call $string-append/2
                     (global.get $string:hash-colon)
                     (struct.get $Keyword $str
                                 (ref.cast (ref $Keyword) (local.get $v)))))


         (func $format/display:bytes
               (param $bs (ref $Bytes))
               (result    (ref $String))

               (local $arr  (ref $I8Array))
               (local $len  i32)
               (local $i    i32)
               (local $byte i32)
               (local $esc  i32)
               (local $s    (ref $String))
               (local $out  (ref $GrowableArray))

               ;; Extract raw byte array and its length
               (local.set $arr (struct.get $Bytes $bs (local.get $bs)))
               (local.set $len (array.len (local.get $arr)))

               ;; Allocate result buffer.
               ;;   Format is #"<content>", so we need 2 chars for the quotes, plus worst-case
               ;;   room to escape *every* byte (e.g., "\ooo" or "\n"), hence len * 4 slack.
               (local.set $out
                          (call $make-growable-array
                                (i32.add (i32.const 2)
                                         (i32.mul (local.get $len) (i32.const 4)))))

               ;; Emit the bytes display prefix:  #"
               (call $growable-array-add! (local.get $out)
                     (global.get $string:bytes-prefix))

               ;; Iterate bytes and append escaped fragments
               (local.set $i (i32.const 0))
               (block $done
                      (loop $loop
                            (br_if $done (i32.ge_u (local.get $i) (local.get $len)))

                            ;; Read next byte as unsigned 0..255 (kept in i32)
                            (local.set $byte (call $i8array-ref (local.get $arr) (local.get $i)))

                            ;; If ASCII (<= 127) and graphic or blank, emit directly (with escapings for \" and \\).
                            ;; Otherwise, try a named special escape (e.g., \n, \t); if none, fall back to octal.
                            (if (i32.and
                                 (i32.le_u (local.get $byte) (i32.const 127))
                                 (i32.or (call $is-graphic (local.get $byte))
                                         (call $is-blank   (local.get $byte))))
                                (then
                                 ;; Handle the two ASCII characters that must be escaped inside quotes:
                                 ;;   92 = '\' backslash, 34 = '"' double quote
                                 (if (i32.eq (local.get $byte) (i32.const 92))       ;; '\'
                                     (then
                                      ;; Emit "\\" to represent a single backslash
                                      (call $growable-array-add! (local.get $out) (global.get $string:backslash))
                                      (call $growable-array-add! (local.get $out) (global.get $string:backslash)))
                                     (else
                                      (if (i32.eq (local.get $byte) (i32.const 34)) ;; '"'
                                          (then
                                           ;; Emit \" to include a literal quote inside the string
                                           (call $growable-array-add! (local.get $out) (global.get $string:backslash))
                                           (call $growable-array-add! (local.get $out) (global.get $string:double-quote)))
                                          (else
                                           ;; Safe printable ASCII → emit 1-char string directly
                                           (local.set $s (call $make-string/checked (i32.const 1) (local.get $byte)))
                                           (call $growable-array-add! (local.get $out) (local.get $s)))))))
                                (else
                                  ;; Non-printable or non-ASCII:
                                  ;; Try special escapes first (returns 0 if none, else codepoint of escaped letter).
                                  (local.set $esc (call $get-special-escape (local.get $byte)))
                                  (if (i32.ne (local.get $esc) (i32.const 0))
                                      (then
                                       ;; Emit backslash + special escape letter (e.g., "\n")
                                       (call $growable-array-add! (local.get $out) (global.get $string:backslash))
                                       (local.set $s (call $make-string/checked (i32.const 1) (local.get $esc)))
                                       (call $growable-array-add! (local.get $out) (local.get $s)))
                                      (else
                                       ;; Fallback: octal escape like "\ooo"
                                       (call $growable-array-add! (local.get $out) (global.get $string:backslash))
                                       (local.set $s (call $make-oct-string (local.get $byte)))
                                       (call $growable-array-add! (local.get $out) (local.get $s))))))

                            ;; Advance to next byte
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $loop)))

               ;; Close the display with a trailing quote and materialize the final string
               (call $growable-array-add! (local.get $out)
                     (global.get $string:double-quote))
               (call $growable-array-of-strings->string (local.get $out)))

         
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
               (call $growable-array-add! (local.get $out)
                                        (ref.cast (ref $String)
                                                 (global.get $string:open-paren)))
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
                                     (then (call $growable-array-add! (local.get $out)
                                                                       (ref.cast (ref $String)
                                                                                (global.get $string:space)))
                                           (call $growable-array-add! (local.get $stack) (local.get $tail)))
                                     (else (if (ref.eq (local.get $tail) (global.get $null))
                                               (then) ;; proper list: done
                                               (else  ;; improper list: emit ". cdr"
                                                (call $growable-array-add! (local.get $out)
                                                                         (ref.cast (ref $String)
                                                                                  (global.get $string:space-dot-space)))
                                                (local.set $str (call $format/display (local.get $tail)))
                                                (call $growable-array-add! (local.get $out) (local.get $str)))))))
                                (else
                                 (call $raise-format/display:pair:expected-pair)
                                 (unreachable)))
                            (br $walk)))
               (call $growable-array-add! (local.get $out)
                                        (ref.cast (ref $String)
                                                 (global.get $string:close-paren)))
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
               (call $growable-array-add! (local.get $out)
                                        (ref.cast (ref $String)
                                                 (global.get $string:vector-prefix)))
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
                                (then (call $growable-array-add! (local.get $out)
                                                                   (ref.cast (ref $String)
                                                                            (global.get $string:space)))))
                            (br $loop)))
               ;; Emit ")"
               (call $growable-array-add! (local.get $out)
                                        (ref.cast (ref $String)
                                                 (global.get $string:close-paren)))
               ;; Combine and return
               (call $growable-array-of-strings->string (local.get $out)))

         ; Note: the `boxed` is internal to the compiler, so the following
         ;       is strictly for debug purposes
         (func $format/display:boxed
               (param $b (ref $Boxed))
               (result   (ref $String))

               (local $val     (ref eq))
               (local $val-str (ref $String))
               (local $out     (ref $GrowableArray))

               ;; Extract value
               (local.set $val (struct.get $Boxed $v (local.get $b)))
               ;; Format the value
               (local.set $val-str (call $format/display (local.get $val)))
               ;; Allocate a growable array
               (local.set $out (call $make-growable-array (i32.const 3)))
               ;; Append "#<boxed:"
               (call $growable-array-add! (local.get $out)
                     (ref.cast (ref $String)
                               (global.get $string:hash-less-boxed-colon)))
               ;; Append formatted value
               (call $growable-array-add! (local.get $out) (local.get $val-str))
               ;; Append closing ">"
               (call $growable-array-add! (local.get $out)
                     (ref.cast (ref $String)
                               (global.get $string:->)))
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
               (call $growable-array-add! (local.get $out)
                                        (ref.cast (ref $String)
                                                 (global.get $string:values-prefix)))
               (call $growable-array-add! (local.get $out)
                                        (ref.cast (ref $String)
                                                 (global.get $string:space)))
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
                                (then (call $growable-array-add! (local.get $out)
                                                                   (ref.cast (ref $String)
                                                                            (global.get $string:space)))))
                            (br $loop)))
               ;; Emit ")"
               (call $growable-array-add! (local.get $out)
                                        (ref.cast (ref $String)
                                                 (global.get $string:close-paren)))
               ;; Combine and return
               (call $growable-array-of-strings->string (local.get $out)))

         ; Note: This uses the write conventions instead of display.
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
                          (call $string-append/2
                                (ref.cast (ref $String) (global.get $string:hash-backslash))
                                (ref.cast (ref $String) (global.get $string:word-newline))))))
               (if (i32.eq (local.get $cp) (i32.const 13))  ;; return
                   (then (return
                          (call $string-append/2
                                (ref.cast (ref $String) (global.get $string:hash-backslash))
                                (ref.cast (ref $String) (global.get $string:word-return))))))
               (if (i32.eq (local.get $cp) (i32.const 9))   ;; tab
                   (then (return
                          (call $string-append/2
                                (ref.cast (ref $String) (global.get $string:hash-backslash))
                                (ref.cast (ref $String) (global.get $string:word-tab))))))
               (if (i32.eq (local.get $cp) (i32.const 8))   ;; backspace
                   (then (return
                          (call $string-append/2
                                (ref.cast (ref $String) (global.get $string:hash-backslash))
                                (ref.cast (ref $String) (global.get $string:word-backspace))))))
               (if (i32.eq (local.get $cp) (i32.const 127)) ;; rubout
                   (then (return
                          (call $string-append/2
                                (ref.cast (ref $String) (global.get $string:hash-backslash))
                                (ref.cast (ref $String) (global.get $string:word-rubout))))))
               (if (i32.eq (local.get $cp) (i32.const 32))  ;; space
                   (then (return
                          (call $string-append/2
                                (ref.cast (ref $String) (global.get $string:hash-backslash))
                                (ref.cast (ref $String) (global.get $string:word-space))))))
               (if (i32.eq (local.get $cp) (i32.const 0))  ;; nul
                   (then (return
                          (call $string-append/2
                                (ref.cast (ref $String) (global.get $string:hash-backslash))
                                (ref.cast (ref $String) (global.get $string:word-nul))))))
               ;; Printable graphic character
               (if (call $is-graphic (local.get $cp))
                   (then
                    (local.set $s (call $make-string/checked (i32.const 1) (local.get $cp)))
                    (return (call $string-append/2
                                  (ref.cast (ref $String) (global.get $string:hash-backslash))
                                  (local.get $s)))))

               ;; Fallback for non-printable or out-of-range characters
               ;; Fallback: #\uXXXX and #\UXXXXXX
               (if (i32.le_u (local.get $cp) (i32.const 65535))  ;; ≤ 0xFFFF
                   (then (return
                          (call $string-append/2
                                (ref.cast (ref $String) (global.get $string:hash-backslash-u))
                                (call $make-hex-string (local.get $cp) (i32.const 4)))))
                   ;; Else use #\UXXXXXX
                   (else (return
                          (call $string-append/2
                                (ref.cast (ref $String) (global.get $string:hash-backslash-U))
                                (call $make-hex-string (local.get $cp) (i32.const 6))))))
               (unreachable))

        (func $make-oct-string
              (param $n i32)
              (result (ref $String))
              (call $number->string:convert
                    (local.get $n)
                    (i32.const 8)
                    (i32.const 3)))

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
                (call $string-append/2
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
                     (local.set $new-vec (struct.new $Vector (i32.const 0) (i32.const 0) (local.get $new-arr)))
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
                                                     (i32.const 0)
                                                     (array.new_default $Array (local.get $i/u) (global.get $null))))
                     (call $vector-copy!
                           (local.get $new-vec)
                           (ref.i31 (i32.const 0))
                           (local.get $vec)
                           (ref.i31 (i32.const 0))
                           (ref.i31 (i32.shl (local.get $i/u) (i32.const 1)))) ;; convert i32 to fixnum
                     (return (local.get $new-vec)))

               ;;;
               ;;; 14. REFLECTION AND SECURITY
               ;;;

               ;; 14.1 Namespaces

               (func $make-empty-namespace (type $Prim0)
                     (result (ref eq)) ;; and (ref $Namespace)
                     
                     (struct.new $Namespace
                                 (i32.const 0)                         ;; $hash
                                 (global.get $false)                   ;; $name
                                 (i32.const 0)                         ;; $base-phase
                                 (ref.cast (ref $HashEqMutable)        ;; $table
                                           (call $make-empty-hasheq))             
                                 (global.get $empty-module-registry)   ;; $modules
                                 (i32.const 0)))                       ;; $protect

               (func $raise-undefined-top (unreachable))
               (func $raise-argument-error:namespace-expected (unreachable))

               (func $namespace-variable-value-simple (type $Prim2)
                     (param $ns  (ref eq))
                     (param $sym (ref eq))
                     (result     (ref eq))
                     ;; 1) Check that $ns is a namespace
                     (if (i32.eqz (ref.test (ref $Namespace) (local.get $ns)))
                         (then (call $raise-argument-error:namespace-expected (local.get $ns))
                               (unreachable)))
                     ;; 2) Check that $sym is a symbol
                     (if (i32.eqz (ref.test (ref $Symbol) (local.get $sym)))
                         (then (call $raise-check-symbol (local.get $sym))
                               (unreachable)))
                     ;; 3) Delegate to the checked implementation
                     (call $namespace-variable-value/checked
                           (ref.cast (ref $Namespace) (local.get $ns))
                           (ref.cast (ref $Symbol)    (local.get $sym))))
               
               (func $namespace-variable-value/checked
                     (param $ns  (ref $Namespace))
                     (param $sym (ref $Symbol))
                     (result     (ref eq))

                     (local $tab (ref $HashEqMutable))
                     (local $got (ref eq))
                     (local $box (ref $Boxed))

                     (local.set $tab (struct.get $Namespace $table (local.get $ns)))
                     (local.set $got
                                (call $hasheq-ref
                                      (ref.cast (ref eq) (local.get $tab))
                                      (local.get $sym)
                                      (global.get $false)))
                     ;; Fail early if the binding is missing or not a $Boxed.
                     (if (i32.eqz (ref.test (ref $Boxed) (local.get $got)))
                         (then (call $raise-undefined-top (local.get $sym))
                               (unreachable)))
                     ;; Cast and return the boxed value.
                     (local.set $box (ref.cast (ref $Boxed) (local.get $got)))
                     (struct.get $Boxed $v (local.get $box)))


               (func $namespace-set-variable-value!
                     (param $ns  (ref eq))
                     (param $sym (ref eq))
                     (param $val (ref eq))
                     (result     (ref eq))
                     ;; 1) Check that $ns is a namespace
                     (if (i32.eqz (ref.test (ref $Namespace) (local.get $ns)))
                         (then (call $raise-argument-error:namespace-expected (local.get $ns))
                               (unreachable)))
                     ;; 2) Check that $sym is a symbol
                     (if (i32.eqz (ref.test (ref $Symbol) (local.get $sym)))
                         (then (call $raise-check-symbol (local.get $sym))
                               (unreachable)))
                     ;; 3) Delegate to the checked implementation
                     (call $namespace-set-variable-value!/checked
                           (ref.cast (ref $Namespace) (local.get $ns))
                           (ref.cast (ref $Symbol)    (local.get $sym))
                           (local.get $val)))
               
               (func $namespace-set-variable-value!/checked
                     ;; Racket semantics: set OR define
                     ;; If `sym` is bound   => mutate the existing $Boxed.
                     ;; If `sym` is unbound => install a fresh $Boxed with `val`.
                     (param $ns  (ref $Namespace))
                     (param $sym (ref $Symbol))
                     (param $val (ref eq))
                     (result     (ref eq))

                     (local $tab (ref $HashEqMutable))
                     (local $got (ref eq))
                     (local $box (ref $Boxed))

                     (local.set $tab (struct.get $Namespace $table (local.get $ns)))
                     (local.set $got
                                (call $hasheq-ref
                                      (ref.cast (ref eq) (local.get $tab))
                                      (local.get $sym)
                                      (global.get $false)))

                     (if (ref.test (ref $Boxed) (local.get $got))
                         (then
                          ;; Binding exists => mutate in place
                          (local.set $box (ref.cast (ref $Boxed) (local.get $got)))
                          (struct.set $Boxed $v (local.get $box) (local.get $val))
                          (return (global.get $void)))
                         (else
                          ;; Binding missing => create and insert new box
                          (local.set $box (struct.new $Boxed (local.get $val)))
                          (call $hasheq-set!/mutable/checked
                                (local.get $tab)
                                (local.get $sym)
                                (ref.cast (ref eq) (local.get $box)))
                          (return (global.get $void))))
                     (unreachable))

               (func $namespace-undefine-variable! (type $Prim2)
                     (param $ns  (ref eq))
                     (param $sym (ref eq))
                     (result     (ref eq))
                     ;; 1) Check that $ns is a namespace
                     (if (i32.eqz (ref.test (ref $Namespace) (local.get $ns)))
                         (then (call $raise-argument-error:namespace-expected (local.get $ns))
                               (unreachable)))
                     ;; 2) Check that $sym is a symbol
                     (if (i32.eqz (ref.test (ref $Symbol) (local.get $sym)))
                         (then (call $raise-check-symbol (local.get $sym))
                               (unreachable)))
                     ;; 3) Delegate to the checked implementation
                     (call $namespace-undefine-variable!/checked
                           (ref.cast (ref $Namespace) (local.get $ns))
                           (ref.cast (ref $Symbol)    (local.get $sym))))

               (func $namespace-undefine-variable!/checked
                     (param $ns  (ref $Namespace))
                     (param $sym (ref $Symbol))
                     (result     (ref eq))

                     (local $tab (ref $HashEqMutable))
                     
                     (local.set $tab (struct.get $Namespace $table (local.get $ns)))
                      ; returns void
                     (call $hash-remove!/mutable (local.get $tab) (local.get $sym)))


               (func $namespace-has-key?
                     (param $ns  (ref eq))
                     (param $sym (ref eq))
                     (result     (ref eq))
                     ;; 1) Check that $ns is a namespace
                     (if (i32.eqz (ref.test (ref $Namespace) (local.get $ns)))
                         (then (call $raise-argument-error:namespace-expected (local.get $ns))
                               (unreachable)))
                     ;; 2) Check that $sym is a symbol
                     (if (i32.eqz (ref.test (ref $Symbol) (local.get $sym)))
                         (then (call $raise-check-symbol (local.get $sym))
                               (unreachable)))
                     ;; 3) Delegate to the checked implementation
                     (call $namespace-has-key?/checked
                           (ref.cast (ref $Namespace) (local.get $ns))
                           (ref.cast (ref $Symbol)    (local.get $sym))))
               
               (func $namespace-has-key?/checked
                     (param $ns  (ref $Namespace))
                     (param $sym (ref $Symbol))
                     (result     (ref eq))

                     (local $tab (ref $HashEqMutable))

                     (local.set $tab (struct.get $Namespace $table (local.get $ns)))
                     (call $hash-has-key? (ref.cast (ref eq) (local.get $tab)) (local.get $sym)))

               
               ;;;
               ;;; FFI
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
                         (Local* (reverse entry-locals)))

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
                     ;; Initialize bytes constants used in the runtime
                     ,@(initialize-runtime-bytes-constants)
                     ;; Initialize symbol constants used in the runtime
                     ,@(initialize-runtime-symbol-constants)
                     
                     ;; ;; Initialize realm symbols
                     ;; (global.set $the-racket-realm
                     ;;             (call $string->symbol
                     ;;                   (ref.cast (ref $String)
                     ;;                             (global.get $string:racket))))
                     ;; (global.set $the-racket/primitive-realm
                     ;;             (call $string->symbol
                     ;;                   (ref.cast
                     ;;                    (ref $String)
                     ;;                    (global.get $string:racket/primitive))))
                     (global.set $the-racket-realm           (global.get $symbol:racket))
                     (global.set $the-racket/primitive-realm (global.get $symbol:racket/primitive))
                     
                     ;; Initialize variables holding primitives
                     ,@(initialize-primitives-as-globals)
                     
                     ;; Initialize top-level variables.
                     ;; These are all "boxed".
                     ,@(let ()
                         (for/list ([v top-vars])
                           `(global.set ,(TopVar v)
                                        (struct.new $Boxed
                                                    (global.get ,(TopVar v))))))

                     ;; Initialize the top-level namespace
                     (global.set $top-level-namespace 
                                 (ref.cast (ref $Namespace) (call $make-empty-namespace)))
                     
                     
                     ; Initialize local variables
                     ,@(let ()
                         (define (Init  x)
                           (match x
                             [(list v t)
                              (if (equal? t '(ref eq))
                                  `(local.set ,(if (symbol? v) v (Var v))
                                              ,(Undefined))
                                  `(nop))]
                             [(list v t init)
                              `(local.set ,(if (symbol? v) v (Var v)) ,init)]))
                         (define (Init* xs) (map Init xs))
                         (Init* entry-locals))

                     
                     ; Body
                     ,entry-body
                     
                     ; Return the result
                     (global.set $result-bytes
                                 (call $string->bytes/utf-8
                                       (call $format/display (global.get ,result))
                                       (global.get $false)   ; ignored
                                       (global.get $zero)    ; start = 0
                                       (global.get $false))) ; end                        

                     (call $copy-bytes-to-memory ; copy and return length as i32
                           (global.get $result-bytes) (i32.const 0)))

               (func $get-bytes (export "get_bytes")
                     (result (ref $Bytes))
                     (ref.cast (ref $Bytes) (global.get $result-bytes)))

               ))))
