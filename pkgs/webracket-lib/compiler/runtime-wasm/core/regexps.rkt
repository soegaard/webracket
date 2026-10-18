#lang racket

(provide regexp-runtime-string-constants
         regexp-runtime-bytes-constants
         regexp-runtime-symbol-constants
         regexp-core-runtime-helpers
         regexp-runtime-helper-forms
         regexp-runtime-only-helper-names
         regexp-runtime-only-helpers)

(require "../core-helper.rkt")

;; regexp-runtime-string-constants : (listof (list/c symbol? string?))
;;   Declare string constants used by regexp helpers and regexp bootstrap patterns.
(define regexp-runtime-string-constants
  '((rx-dot-class       "[.]")
    (rx-dot-zo-end      "[.]zo$")
    (rx-id-colon-or-end "^id(:|$)")
    (rx-bounded-pair    "^([1-8]),([1-8])$")
    (rx-matrix-environment "\\\\begin\\{(array|matrix|pmatrix|bmatrix|Bmatrix|vmatrix|Vmatrix|smallmatrix|cases)\\}")
    (matrix-environment-prefix "\\begin{")
    (matrix-environment:array "array}")
    (matrix-environment:matrix "matrix}")
    (matrix-environment:pmatrix "pmatrix}")
    (matrix-environment:bmatrix "bmatrix}")
    (matrix-environment:Bmatrix "Bmatrix}")
    (matrix-environment:vmatrix "vmatrix}")
    (matrix-environment:Vmatrix "Vmatrix}")
    (matrix-environment:smallmatrix "smallmatrix}")
    (matrix-environment:cases "cases}")
    (dot                ".")
    (dot-zo             ".zo")
    (id                 "id")
    (regexp?            "regexp?")
    (regexp-replace:pattern-contract "(or/c regexp? byte-regexp? string? bytes?)")
    (regexp-match:input-contract "(or/c string? bytes? path? input-port?)")
    (regexp-replace:input-contract "(or/c string? bytes?)")
    (regexp-replace:insert-contract "(or/c string? bytes? procedure?)")
    (regexp-replace     "regexp-replace")
    (regexp-replace*    "regexp-replace*")))

;; regexp-runtime-bytes-constants : (listof (list/c symbol? bytes?))
;;   Declare byte-string constants used by byte-regexp bootstrap patterns.
(define regexp-runtime-bytes-constants
  '((dot-zo             #".zo")
    (id                 #"id")
    (rx-dot-class       #"[.]")
    (rx-dot-zo-end      #"[.]zo$")
    (rx-id-colon-or-end #"^id(:|$)")
    (rx-envvar-name     #"^[^\0=]*$")
    (rx-unix-path-list  #"([^:]*):(.*)")
    (rx-windows-path-list #"([^;]*);(.*)")))

;; regexp-runtime-symbol-constants : (listof symbol?)
;;   Declare symbols used by regexp constructor contract checks.
(define regexp-runtime-symbol-constants
  '(regexp
    pregexp
    byte-regexp
    byte-pregexp
    regexp-match
    regexp-match-positions
    regexp-match?
    regexp-replace
    regexp-replace*))

;; regexp-runtime-only-forms : (listof datum?)
;;   Enumerate regular expression helpers used by the monolithic runtime.
(define regexp-runtime-only-forms
  '(
         ;;; 4.8 REGULAR EXPRESSIONS
         ;;;

         ;; https://docs.racket-lang.org/reference/regexp.html

         ;; Matching support is implemented separately by the demodded regexp
         ;; layer. These primitives provide self-contained regexp values so
         ;; reader literals can compile without that library being present.

         ;; make-regexp-value : i32 any/c -> regexp?
         ;;   Create a compact regexp value with a recoverable source name.
         (func $make-regexp-value
               (param $kind   i32)      ;; 0 rx, 1 px, 2 byte-rx, 3 byte-px
               (param $source (ref eq)) ;; string or bytes object-name
               (result        (ref eq))
               (struct.new $Regexp
                           (i32.const 0)
                           (local.get $kind)
                           (local.get $source)))

         ;; webracket-literal-regexp : string? -> regexp?
         ;;   Construct a private regexp literal value; matching is later.
         (func $webracket-literal-regexp (type $Prim1)
               (param $source (ref eq)) ;; pattern string
               (result        (ref eq))
               (if (i32.eqz (ref.test (ref $String) (local.get $source)))
                   (then (call $raise-argument-error1
                               (global.get $symbol:regexp)
                               (global.get $string:string?)
                               (local.get $source))
                         (unreachable)))
               (call $make-regexp-value
                     (i32.const 0)
                     (ref.cast (ref $String) (local.get $source))))

         ;; webracket-literal-pregexp : string? -> pregexp?
         ;;   Construct a private pregexp literal value; matching is later.
         (func $webracket-literal-pregexp (type $Prim1)
               (param $source (ref eq)) ;; pattern string
               (result        (ref eq))
               (if (i32.eqz (ref.test (ref $String) (local.get $source)))
                   (then (call $raise-argument-error1
                               (global.get $symbol:pregexp)
                               (global.get $string:string?)
                               (local.get $source))
                         (unreachable)))
               (call $make-regexp-value
                     (i32.const 1)
                     (ref.cast (ref $String) (local.get $source))))

         ;; webracket-literal-byte-regexp : bytes? -> byte-regexp?
         ;;   Construct a private byte regexp literal value; matching is later.
         (func $webracket-literal-byte-regexp (type $Prim1)
               (param $source (ref eq)) ;; pattern bytes
               (result        (ref eq))
               (if (i32.eqz (ref.test (ref $Bytes) (local.get $source)))
                   (then (call $raise-argument-error1
                               (global.get $symbol:byte-regexp)
                               (global.get $string:bytes?)
                               (local.get $source))
                         (unreachable)))
               (call $make-regexp-value
                     (i32.const 2)
                     (ref.cast (ref $Bytes) (local.get $source))))

         ;; webracket-literal-byte-pregexp : bytes? -> byte-pregexp?
         ;;   Construct a private byte pregexp literal value; matching is later.
         (func $webracket-literal-byte-pregexp (type $Prim1)
               (param $source (ref eq)) ;; pattern bytes
               (result        (ref eq))
               (if (i32.eqz (ref.test (ref $Bytes) (local.get $source)))
                   (then (call $raise-argument-error1
                               (global.get $symbol:byte-pregexp)
                               (global.get $string:bytes?)
                               (local.get $source))
                         (unreachable)))
               (call $make-regexp-value
                     (i32.const 3)
                     (ref.cast (ref $Bytes) (local.get $source))))

         ;; regexp : string? -> regexp?
         ;;   Construct a bootstrap regexp from a string pattern.
         (func $regexp (type $Prim1)
               (param $source (ref eq)) ;; pattern string
               (result        (ref eq))
               (if (i32.eqz (ref.test (ref $String) (local.get $source)))
                   (then (call $raise-argument-error1
                               (global.get $symbol:regexp)
                               (global.get $string:string?)
                               (local.get $source))
                         (unreachable)))
               (return_call $webracket-literal-regexp (local.get $source)))

         ;; pregexp : string? -> pregexp?
         ;;   Construct a bootstrap pregexp from a string pattern.
         (func $pregexp (type $Prim1)
               (param $source (ref eq)) ;; pattern string
               (result        (ref eq))
               (if (i32.eqz (ref.test (ref $String) (local.get $source)))
                   (then (call $raise-argument-error1
                               (global.get $symbol:pregexp)
                               (global.get $string:string?)
                               (local.get $source))
                         (unreachable)))
               (return_call $webracket-literal-pregexp (local.get $source)))

         ;; byte-regexp : bytes? -> byte-regexp?
         ;;   Construct a bootstrap byte regexp from a byte pattern.
         (func $byte-regexp (type $Prim1)
               (param $source (ref eq)) ;; pattern bytes
               (result        (ref eq))
               (if (i32.eqz (ref.test (ref $Bytes) (local.get $source)))
                   (then (call $raise-argument-error1
                               (global.get $symbol:byte-regexp)
                               (global.get $string:bytes?)
                               (local.get $source))
                         (unreachable)))
               (return_call $webracket-literal-byte-regexp (local.get $source)))

         ;; byte-pregexp : bytes? -> byte-pregexp?
         ;;   Construct a bootstrap byte pregexp from a byte pattern.
         (func $byte-pregexp (type $Prim1)
               (param $source (ref eq)) ;; pattern bytes
               (result        (ref eq))
               (if (i32.eqz (ref.test (ref $Bytes) (local.get $source)))
                   (then (call $raise-argument-error1
                               (global.get $symbol:byte-pregexp)
                               (global.get $string:bytes?)
                               (local.get $source))
                         (unreachable)))
               (return_call $webracket-literal-byte-pregexp (local.get $source)))

         ;; regexp? : any/c -> boolean?
         ;;   Check for character regexp and pregexp values.
         (func $regexp? (type $Prim1)
               (param $v (ref eq)) ;; candidate value
               (result   (ref eq))
               (if (i32.eqz (ref.test (ref $Regexp) (local.get $v)))
                   (then (return (global.get $false))))
               (if (result (ref eq))
                   (i32.lt_u (struct.get $Regexp $kind
                                         (ref.cast (ref $Regexp) (local.get $v)))
                             (i32.const 2))
                   (then (global.get $true))
                   (else (global.get $false))))

         ;; pregexp? : any/c -> boolean?
         ;;   Check for character pregexp values.
         (func $pregexp? (type $Prim1)
               (param $v (ref eq)) ;; candidate value
               (result   (ref eq))
               (if (i32.eqz (ref.test (ref $Regexp) (local.get $v)))
                   (then (return (global.get $false))))
               (if (result (ref eq))
                   (i32.eq (struct.get $Regexp $kind
                                       (ref.cast (ref $Regexp) (local.get $v)))
                           (i32.const 1))
                   (then (global.get $true))
                   (else (global.get $false))))

         ;; byte-regexp? : any/c -> boolean?
         ;;   Check for byte regexp and byte pregexp values.
         (func $byte-regexp? (type $Prim1)
               (param $v (ref eq)) ;; candidate value
               (result   (ref eq))
               (if (i32.eqz (ref.test (ref $Regexp) (local.get $v)))
                   (then (return (global.get $false))))
               (if (result (ref eq))
                   (i32.ge_u (struct.get $Regexp $kind
                                         (ref.cast (ref $Regexp) (local.get $v)))
                             (i32.const 2))
                   (then (global.get $true))
                   (else (global.get $false))))

         ;; byte-pregexp? : any/c -> boolean?
         ;;   Check for byte pregexp values.
         (func $byte-pregexp? (type $Prim1)
               (param $v (ref eq)) ;; candidate value
               (result   (ref eq))
               (if (i32.eqz (ref.test (ref $Regexp) (local.get $v)))
                   (then (return (global.get $false))))
               (if (result (ref eq))
                   (i32.eq (struct.get $Regexp $kind
                                       (ref.cast (ref $Regexp) (local.get $v)))
                           (i32.const 3))
                   (then (global.get $true))
                   (else (global.get $false))))

         ;; regexp-match?/string-subset : regexp? string? -> boolean?
         ;;   Small bootstrap matcher for expander startup. It handles the
         ;;   simple literal regexp shapes currently reached before the
         ;;   demodded regexp library is available.
         (func $regexp-match?/string-subset
               (param $rx (ref $Regexp)) ;; regexp value
               (param $s  (ref $String)) ;; input string
               (result    (ref eq))
               (local $source (ref eq))
               (local $pattern (ref $String))
               (local $arr (ref $I32Array))
               (local $len i32)

               (local.set $source (struct.get $Regexp $source (local.get $rx)))
               (if (i32.eqz (ref.test (ref $String) (local.get $source)))
                   (then (return (global.get $false))))
               (local.set $pattern (ref.cast (ref $String) (local.get $source)))

               (if (call $string=?/i32/checked
                         (local.get $pattern)
                         (ref.cast (ref $String) (global.get $string:rx-dot-class)))
                   (then (return (call $string-contains?
                                       (ref.cast (ref eq) (local.get $s))
                                       (global.get $string:dot)))))
               (if (call $string=?/i32/checked
                         (local.get $pattern)
                         (ref.cast (ref $String) (global.get $string:rx-dot-zo-end)))
                   (then (return (if (result (ref eq))
                                     (call $string-suffix?/i32/checked
                                           (local.get $s)
                                           (ref.cast (ref $String)
                                                     (global.get $string:dot-zo)))
                                     (then (global.get $true))
                                     (else (global.get $false))))))
               (if (call $string=?/i32/checked
                         (local.get $pattern)
                         (ref.cast (ref $String) (global.get $string:rx-id-colon-or-end)))
                   (then
                    (if (i32.eqz (call $string-prefix?/i32/checked
                                       (local.get $s)
                                       (ref.cast (ref $String)
                                                 (global.get $string:id))))
                        (then (return (global.get $false))))
                    (local.set $arr (struct.get $String $codepoints (local.get $s)))
                    (local.set $len (array.len (local.get $arr)))
                    (return
                     (if (result (ref eq))
                         (i32.eq (local.get $len) (i32.const 2))
                         (then (global.get $true))
                         (else
                          (if (result (ref eq))
                              (i32.eq (array.get $I32Array
                                                 (local.get $arr)
                                                 (i32.const 2))
                                      (i32.const 58))
                              (then (global.get $true))
                              (else (global.get $false))))))))

               (return_call $string-contains?
                            (ref.cast (ref eq) (local.get $s))
                            (ref.cast (ref eq) (local.get $pattern))))

         ;; bytes-prefix?/i32/checked : bytes? bytes? -> i32
         ;;   Check whether b starts with prefix. Arguments are prechecked.
         (func $bytes-prefix?/i32/checked
               (param $b      (ref $Bytes)) ;; bytes
               (param $prefix (ref $Bytes)) ;; bytes prefix
               (result        i32)
               (local $arr-b (ref $I8Array))
               (local $arr-p (ref $I8Array))
               (local $len-b i32)
               (local $len-p i32)
               (local $i     i32)

               (local.set $arr-b (struct.get $Bytes $bs (local.get $b)))
               (local.set $arr-p (struct.get $Bytes $bs (local.get $prefix)))
               (local.set $len-b (array.len (local.get $arr-b)))
               (local.set $len-p (array.len (local.get $arr-p)))
               (if (i32.lt_u (local.get $len-b) (local.get $len-p))
                   (then (return (i32.const 0))))
               (local.set $i (i32.const 0))
               (block $done
                      (loop $loop
                            (br_if $done (i32.ge_u (local.get $i) (local.get $len-p)))
                            (if (i32.ne (array.get_u $I8Array (local.get $arr-b) (local.get $i))
                                        (array.get_u $I8Array (local.get $arr-p) (local.get $i)))
                                (then (return (i32.const 0))))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $loop)))
               (i32.const 1))

         ;; bytes-suffix?/i32/checked : bytes? bytes? -> i32
         ;;   Check whether b ends with suffix. Arguments are prechecked.
         (func $bytes-suffix?/i32/checked
               (param $b      (ref $Bytes)) ;; bytes
               (param $suffix (ref $Bytes)) ;; bytes suffix
               (result        i32)
               (local $arr-b  (ref $I8Array))
               (local $arr-s  (ref $I8Array))
               (local $len-b  i32)
               (local $len-s  i32)
               (local $offset i32)
               (local $i      i32)

               (local.set $arr-b (struct.get $Bytes $bs (local.get $b)))
               (local.set $arr-s (struct.get $Bytes $bs (local.get $suffix)))
               (local.set $len-b (array.len (local.get $arr-b)))
               (local.set $len-s (array.len (local.get $arr-s)))
               (if (i32.gt_u (local.get $len-s) (local.get $len-b))
                   (then (return (i32.const 0))))
               (local.set $offset (i32.sub (local.get $len-b) (local.get $len-s)))
               (local.set $i (i32.const 0))
               (block $done
                      (loop $loop
                            (br_if $done (i32.ge_u (local.get $i) (local.get $len-s)))
                            (if (i32.ne
                                 (array.get_u $I8Array
                                              (local.get $arr-b)
                                              (i32.add (local.get $offset) (local.get $i)))
                                 (array.get_u $I8Array (local.get $arr-s) (local.get $i)))
                                (then (return (i32.const 0))))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $loop)))
               (i32.const 1))

         ;; bytes-contains?/checked : bytes? bytes? -> boolean?
         ;;   Check whether b contains needle. Arguments are prechecked.
         (func $bytes-contains?/checked
               (param $b      (ref $Bytes)) ;; bytes
               (param $needle (ref $Bytes)) ;; bytes needle
               (result        (ref eq))
               (local $arr-b (ref $I8Array))
               (local $arr-n (ref $I8Array))
               (local $len-b i32)
               (local $len-n i32)
               (local $limit i32)
               (local $i     i32)
               (local $j     i32)

               (local.set $arr-b (struct.get $Bytes $bs (local.get $b)))
               (local.set $arr-n (struct.get $Bytes $bs (local.get $needle)))
               (local.set $len-b (array.len (local.get $arr-b)))
               (local.set $len-n (array.len (local.get $arr-n)))
               (if (i32.eqz (local.get $len-n))
                   (then (return (global.get $true))))
               (if (i32.lt_u (local.get $len-b) (local.get $len-n))
                   (then (return (global.get $false))))
               (local.set $limit (i32.sub (local.get $len-b) (local.get $len-n)))
               (local.set $i (i32.const 0))
               (block $not-found
                      (loop $outer
                            (br_if $not-found
                                   (i32.gt_u (local.get $i) (local.get $limit)))
                            (local.set $j (i32.const 0))
                            (block $mismatch
                                   (loop $inner
                                         (br_if $mismatch
                                                (i32.ge_u (local.get $j) (local.get $len-n)))
                                         (if (i32.ne
                                              (array.get_u $I8Array
                                                           (local.get $arr-b)
                                                           (i32.add (local.get $i) (local.get $j)))
                                              (array.get_u $I8Array
                                                           (local.get $arr-n)
                                                           (local.get $j)))
                                             (then (br $mismatch)))
                                         (local.set $j (i32.add (local.get $j) (i32.const 1)))
                                         (br $inner)))
                            (if (i32.eq (local.get $j) (local.get $len-n))
                                (then (return (global.get $true))))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $outer)))
               (global.get $false))

         ;; regexp-match?/bytes-subset : byte-regexp? bytes? -> boolean?
         ;;   Byte analogue of regexp-match?/string-subset for startup probes.
         (func $regexp-match?/bytes-subset
               (param $rx (ref $Regexp)) ;; byte regexp value
               (param $b  (ref $Bytes))  ;; input bytes
               (result    (ref eq))
               (local $source  (ref eq))
               (local $pattern (ref $Bytes))
               (local $arr     (ref $I8Array))
               (local $len     i32)

               (local.set $source (struct.get $Regexp $source (local.get $rx)))
               (if (i32.eqz (ref.test (ref $Bytes) (local.get $source)))
                   (then (return (global.get $false))))
               (local.set $pattern (ref.cast (ref $Bytes) (local.get $source)))

               (if (ref.eq (call $bytes=?/2/checked
                                 (local.get $pattern)
                                 (ref.cast (ref $Bytes) (global.get $bytes:rx-dot-class)))
                           (global.get $true))
                   (then (return (call $bytes-contains?/checked
                                       (local.get $b)
                                       (ref.cast (ref $Bytes) (global.get $bytes:dot))))))
               (if (ref.eq (call $bytes=?/2/checked
                                 (local.get $pattern)
                                 (ref.cast (ref $Bytes) (global.get $bytes:rx-dot-zo-end)))
                           (global.get $true))
                   (then (return (if (result (ref eq))
                                     (call $bytes-suffix?/i32/checked
                                           (local.get $b)
                                           (ref.cast (ref $Bytes) (global.get $bytes:dot-zo)))
                                     (then (global.get $true))
                                     (else (global.get $false))))))
               (if (ref.eq (call $bytes=?/2/checked
                                 (local.get $pattern)
                                 (ref.cast (ref $Bytes) (global.get $bytes:rx-id-colon-or-end)))
                           (global.get $true))
                   (then
                    (if (i32.eqz (call $bytes-prefix?/i32/checked
                                       (local.get $b)
                                       (ref.cast (ref $Bytes) (global.get $bytes:id))))
                        (then (return (global.get $false))))
                    (local.set $arr (struct.get $Bytes $bs (local.get $b)))
                    (local.set $len (array.len (local.get $arr)))
                    (return
                     (if (result (ref eq))
                         (i32.eq (local.get $len) (i32.const 2))
                         (then (global.get $true))
                         (else
                          (if (result (ref eq))
                              (i32.eq (array.get_u $I8Array
                                                   (local.get $arr)
                                                   (i32.const 2))
                                      (i32.const 58))
                              (then (global.get $true))
                              (else (global.get $false))))))))
               (if (ref.eq (call $bytes=?/2/checked
                                 (local.get $pattern)
                                 (ref.cast (ref $Bytes) (global.get $bytes:rx-envvar-name)))
                           (global.get $true))
                   (then (return
                          (if (result (ref eq))
                              (i32.and
                               (i32.lt_s
                                (call $bytes-index-of-byte/checked
                                      (local.get $b)
                                      (i32.const 0))
                                (i32.const 0))
                               (i32.lt_s
                                (call $bytes-index-of-byte/checked
                                      (local.get $b)
                                      (i32.const 61))
                                (i32.const 0)))
                              (then (global.get $true))
                              (else (global.get $false))))))
               (return_call $bytes-contains?/checked
                            (local.get $b)
                            (local.get $pattern)))

         ;; bytes-index-of-byte/checked : bytes? byte -> i32
         ;;   Return the first index of byte, or -1 if absent.
         (func $bytes-index-of-byte/checked
               (param $b    (ref $Bytes)) ;; bytes
               (param $byte i32)          ;; byte
               (result      i32)
               (local $arr (ref $I8Array))
               (local $len i32)
               (local $i   i32)

               (local.set $arr (struct.get $Bytes $bs (local.get $b)))
               (local.set $len (array.len (local.get $arr)))
               (local.set $i (i32.const 0))
               (block $not-found
                      (loop $loop
                            (br_if $not-found (i32.ge_u (local.get $i) (local.get $len)))
                            (if (i32.eq (array.get_u $I8Array (local.get $arr) (local.get $i))
                                        (local.get $byte))
                                (then (return (local.get $i))))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $loop)))
               (i32.const -1))

         ;; regexp-match/path-list-bytes : byte-regexp? bytes? byte -> (or/c list? #f)
         ;;   Bootstrap path-list splitter for patterns like #"([^:]*):(.*)".
         (func $regexp-match/path-list-bytes
               (param $b   (ref $Bytes)) ;; input bytes
               (param $sep i32)          ;; separator byte
               (result     (ref eq))
               (local $idx        i32)
               (local $left       (ref eq))
               (local $right      (ref eq))
               (local $idx-fx     (ref eq))
               (local $after-fx   (ref eq))

               (local.set $idx (call $bytes-index-of-byte/checked
                                     (local.get $b)
                                     (local.get $sep)))
               (if (i32.lt_s (local.get $idx) (i32.const 0))
                   (then (return (global.get $false))))
               (local.set $idx-fx (ref.i31 (i32.shl (local.get $idx) (i32.const 1))))
               (local.set $after-fx
                          (ref.i31 (i32.shl (i32.add (local.get $idx) (i32.const 1))
                                            (i32.const 1))))
               (local.set $left
                          (call $subbytes
                                (ref.cast (ref eq) (local.get $b))
                                (ref.i31 (i32.const 0))
                                (local.get $idx-fx)))
               (local.set $right
                          (call $subbytes
                                (ref.cast (ref eq) (local.get $b))
                                (local.get $after-fx)
                                (global.get $missing)))
               (call $cons
                     (ref.cast (ref eq) (local.get $b))
                     (call $cons
                           (local.get $left)
                           (call $cons
                                 (local.get $right)
                                 (global.get $null)))))

         ;; regexp-match/bytes-subset : byte-regexp? bytes? -> (or/c list? #f)
         ;;   Minimal capture support needed by racket/private/path-list.
         (func $regexp-match/bytes-subset
               (param $rx (ref $Regexp)) ;; byte regexp value
               (param $b  (ref $Bytes))  ;; input bytes
               (result    (ref eq))
               (local $source  (ref eq))
               (local $pattern (ref $Bytes))

               (local.set $source (struct.get $Regexp $source (local.get $rx)))
               (if (i32.eqz (ref.test (ref $Bytes) (local.get $source)))
                   (then (return (global.get $false))))
               (local.set $pattern (ref.cast (ref $Bytes) (local.get $source)))
               (if (ref.eq (call $bytes=?/2/checked
                                 (local.get $pattern)
                                 (ref.cast (ref $Bytes) (global.get $bytes:rx-unix-path-list)))
                           (global.get $true))
                   (then (return_call $regexp-match/path-list-bytes
                                      (local.get $b)
                                      (i32.const 58))))
               (if (ref.eq (call $bytes=?/2/checked
                                 (local.get $pattern)
                                 (ref.cast (ref $Bytes) (global.get $bytes:rx-windows-path-list)))
                           (global.get $true))
                   (then (return_call $regexp-match/path-list-bytes
                                      (local.get $b)
                                      (i32.const 59))))
               (global.get $false))

         ;; regexp-match/bounded-pair-string : regexp? string? -> (or/c list? #f)
         ;;   Return captures for the bootstrap anchored bounded-pair pattern.
         (func $regexp-match/bounded-pair-string
               (param $rx (ref $Regexp)) ;; character regexp value
               (param $s  (ref $String)) ;; input string
               (result    (ref eq))
               (local $source  (ref eq))
               (local $pattern (ref $String))
               (local $len     i32)
               (local $left-cp i32)
               (local $right-cp i32)
               (local $left    (ref eq))
               (local $right   (ref eq))

               (local.set $source (struct.get $Regexp $source (local.get $rx)))
               (if (i32.eqz (ref.test (ref $String) (local.get $source)))
                   (then (return (global.get $false))))
               (local.set $pattern (ref.cast (ref $String) (local.get $source)))
               (if (i32.eqz
                    (call $string=?/i32/checked
                          (local.get $pattern)
                          (ref.cast (ref $String)
                                    (global.get $string:rx-bounded-pair))))
                   (then (return (global.get $false))))
               (local.set $len (call $string-length/checked/i32 (local.get $s)))
               (if (i32.ne (local.get $len) (i32.const 3))
                   (then (return (global.get $false))))
               (if (i32.ne (call $string-ref/checked/i32 (local.get $s) (i32.const 1))
                           (i32.const 44))
                   (then (return (global.get $false))))
               (local.set $left-cp
                          (call $string-ref/checked/i32 (local.get $s) (i32.const 0)))
               (local.set $right-cp
                          (call $string-ref/checked/i32 (local.get $s) (i32.const 2)))
               (if (i32.or (i32.lt_u (local.get $left-cp) (i32.const 49))
                           (i32.gt_u (local.get $left-cp) (i32.const 56)))
                   (then (return (global.get $false))))
               (if (i32.or (i32.lt_u (local.get $right-cp) (i32.const 49))
                           (i32.gt_u (local.get $right-cp) (i32.const 56)))
                   (then (return (global.get $false))))
               (local.set $left
                          (call $substring
                                (ref.cast (ref eq) (local.get $s))
                                (ref.i31 (i32.const 0))
                                (ref.i31 (i32.const 2))))
               (local.set $right
                          (call $substring
                                (ref.cast (ref eq) (local.get $s))
                                (ref.i31 (i32.const 4))
                                (ref.i31 (i32.const 6))))
               (call $cons
                     (ref.cast (ref eq) (local.get $s))
                     (call $cons
                           (local.get $left)
                           (call $cons
                                 (local.get $right)
                                 (global.get $null)))))

         ;; string-prefix-at?/checked : string? string? exact-nonnegative-integer? -> boolean?
         ;;   Check whether prefix occurs in s at the validated character index.
         (func $string-prefix-at?/checked
               (param $s      (ref $String)) ;; input string
               (param $prefix (ref $String)) ;; candidate prefix
               (param $start  i32)           ;; character index
               (result        i32)
               (local $s-length      i32)
               (local $prefix-length i32)
               (local $i             i32)

               (local.set $s-length (call $string-length/checked/i32 (local.get $s)))
               (local.set $prefix-length
                          (call $string-length/checked/i32 (local.get $prefix)))
               (if (i32.gt_u (local.get $start) (local.get $s-length))
                   (then (return (i32.const 0))))
               (if (i32.gt_u (local.get $prefix-length)
                             (i32.sub (local.get $s-length) (local.get $start)))
                   (then (return (i32.const 0))))
               (local.set $i (i32.const 0))
               (block $done
                      (loop $loop
                            (br_if $done
                                   (i32.ge_u (local.get $i)
                                             (local.get $prefix-length)))
                            (if (i32.ne
                                 (call $string-ref/checked/i32
                                       (local.get $s)
                                       (i32.add (local.get $start) (local.get $i)))
                                 (call $string-ref/checked/i32
                                       (local.get $prefix)
                                       (local.get $i)))
                                (then (return (i32.const 0))))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $loop)))
               (i32.const 1))

         ;; regexp-match-positions/matrix-environment-string : regexp? string? -> (or/c list? #f)
         ;;   Return positions for the bootstrap Matrix-environment detector pattern.
         (func $regexp-match-positions/matrix-environment-string
               (param $rx (ref $Regexp)) ;; character regexp value
               (param $s  (ref $String)) ;; input string
               (result    (ref eq))
               (local $source         (ref eq))
               (local $pattern        (ref $String))
               (local $input-length   i32)
               (local $i              i32)
               (local $capture-start  i32)
               (local $capture-length i32)
               (local $full-end       i32)

               (local.set $source (struct.get $Regexp $source (local.get $rx)))
               (if (i32.eqz (ref.test (ref $String) (local.get $source)))
                   (then (return (global.get $false))))
               (local.set $pattern (ref.cast (ref $String) (local.get $source)))
               (if (i32.eqz
                    (call $string=?/i32/checked
                          (local.get $pattern)
                          (ref.cast (ref $String)
                                    (global.get $string:rx-matrix-environment))))
                   (then (return (global.get $false))))
               (local.set $input-length
                          (call $string-length/checked/i32 (local.get $s)))
               (local.set $i (i32.const 0))
               (local.set $capture-start (i32.const 0))
               (local.set $capture-length (i32.const -1))
               (block $search-done
                      (loop $search
                            (br_if $search-done
                                   (i32.ge_u (local.get $i) (local.get $input-length)))
                            (local.set $capture-length (i32.const -1))
                            (if (call $string-prefix-at?/checked
                                      (local.get $s)
                                      (ref.cast (ref $String)
                                                (global.get $string:matrix-environment-prefix))
                                      (local.get $i))
                                (then
                                 (local.set $capture-start
                                            (i32.add (local.get $i) (i32.const 7)))
                                 (if (call $string-prefix-at?/checked
                                           (local.get $s)
                                           (ref.cast (ref $String)
                                                     (global.get $string:matrix-environment:array))
                                           (local.get $capture-start))
                                     (then (local.set $capture-length (i32.const 5))))
                                 (if (call $string-prefix-at?/checked
                                           (local.get $s)
                                           (ref.cast (ref $String)
                                                     (global.get $string:matrix-environment:matrix))
                                           (local.get $capture-start))
                                     (then (local.set $capture-length (i32.const 6))))
                                 (if (call $string-prefix-at?/checked
                                           (local.get $s)
                                           (ref.cast (ref $String)
                                                     (global.get $string:matrix-environment:pmatrix))
                                           (local.get $capture-start))
                                     (then (local.set $capture-length (i32.const 7))))
                                 (if (call $string-prefix-at?/checked
                                           (local.get $s)
                                           (ref.cast (ref $String)
                                                     (global.get $string:matrix-environment:bmatrix))
                                           (local.get $capture-start))
                                     (then (local.set $capture-length (i32.const 7))))
                                 (if (call $string-prefix-at?/checked
                                           (local.get $s)
                                           (ref.cast (ref $String)
                                                     (global.get $string:matrix-environment:Bmatrix))
                                           (local.get $capture-start))
                                     (then (local.set $capture-length (i32.const 7))))
                                 (if (call $string-prefix-at?/checked
                                           (local.get $s)
                                           (ref.cast (ref $String)
                                                     (global.get $string:matrix-environment:vmatrix))
                                           (local.get $capture-start))
                                     (then (local.set $capture-length (i32.const 7))))
                                 (if (call $string-prefix-at?/checked
                                           (local.get $s)
                                           (ref.cast (ref $String)
                                                     (global.get $string:matrix-environment:Vmatrix))
                                           (local.get $capture-start))
                                     (then (local.set $capture-length (i32.const 7))))
                                 (if (call $string-prefix-at?/checked
                                           (local.get $s)
                                           (ref.cast (ref $String)
                                                     (global.get $string:matrix-environment:smallmatrix))
                                           (local.get $capture-start))
                                     (then (local.set $capture-length (i32.const 11))))
                                 (if (call $string-prefix-at?/checked
                                           (local.get $s)
                                           (ref.cast (ref $String)
                                                     (global.get $string:matrix-environment:cases))
                                           (local.get $capture-start))
                                     (then (local.set $capture-length (i32.const 5))))
                                 (if (i32.ge_s (local.get $capture-length) (i32.const 0))
                                     (then
                                      (local.set $full-end
                                                 (i32.add
                                                  (i32.add (local.get $capture-start)
                                                           (local.get $capture-length))
                                                  (i32.const 1)))
                                      (return
                                       (call $cons
                                             (call $cons
                                                   (ref.i31
                                                    (i32.shl (local.get $i) (i32.const 1)))
                                                   (ref.i31
                                                    (i32.shl (local.get $full-end) (i32.const 1))))
                                             (call $cons
                                                   (call $cons
                                                         (ref.i31
                                                          (i32.shl (local.get $capture-start)
                                                                   (i32.const 1)))
                                                         (ref.i31
                                                          (i32.shl
                                                           (i32.add (local.get $capture-start)
                                                                    (local.get $capture-length))
                                                           (i32.const 1))))
                                                   (global.get $null))))))))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $search)))
               (global.get $false))

         ;; regexp-match : regexp? (or/c string? bytes?) -> (or/c list? #f)
         ;;   Return bootstrap capture lists for supported string and byte patterns.
         (func $regexp-match (type $Prim2)
               (param $rx-raw (ref eq)) ;; regexp?
               (param $s-raw  (ref eq)) ;; string? or bytes? for the bootstrap subset
               (result        (ref eq))
               (if (i32.eqz (ref.test (ref $Regexp) (local.get $rx-raw)))
                   (then (call $raise-argument-error1
                               (global.get $symbol:regexp-match)
                               (global.get $string:regexp-replace:pattern-contract)
                               (local.get $rx-raw))
                         (unreachable)))
               (if (ref.test (ref $Bytes) (local.get $s-raw))
                   (then (return_call $regexp-match/bytes-subset
                                      (ref.cast (ref $Regexp) (local.get $rx-raw))
                                      (ref.cast (ref $Bytes) (local.get $s-raw)))))
               (if (i32.eqz (ref.test (ref $String) (local.get $s-raw)))
                   (then (call $raise-argument-error1
                               (global.get $symbol:regexp-match)
                               (global.get $string:regexp-match:input-contract)
                               (local.get $s-raw))
                         (unreachable)))
               (return_call $regexp-match/bounded-pair-string
                            (ref.cast (ref $Regexp) (local.get $rx-raw))
                            (ref.cast (ref $String) (local.get $s-raw))))

         ;; regexp-match-positions : regexp? string? -> (or/c list? #f)
         ;;   Bootstrap subset for the Matrix-environment detector; optional
         ;;   start-pos, end-pos, output-port, and input-prefix are unsupported.
         (func $regexp-match-positions (type $Prim2)
               (param $rx-raw (ref eq)) ;; regexp?
               (param $s-raw  (ref eq)) ;; string? for the bootstrap subset
               (result        (ref eq))
               (if (i32.eqz (ref.test (ref $Regexp) (local.get $rx-raw)))
                   (then (call $raise-argument-error1
                               (global.get $symbol:regexp-match-positions)
                               (global.get $string:regexp-replace:pattern-contract)
                               (local.get $rx-raw))
                         (unreachable)))
               (if (i32.eqz (ref.test (ref $String) (local.get $s-raw)))
                   (then (call $raise-argument-error1
                               (global.get $symbol:regexp-match-positions)
                               (global.get $string:regexp-match:input-contract)
                               (local.get $s-raw))
                         (unreachable)))
               (return_call $regexp-match-positions/matrix-environment-string
                            (ref.cast (ref $Regexp) (local.get $rx-raw))
                            (ref.cast (ref $String) (local.get $s-raw))))

         ;; regexp-match? : regexp? (or/c string? bytes?) -> boolean?
         ;;   Bootstrap subset. Full Racket regexp semantics should come from
         ;;   the demodded regexp library; this primitive only unblocks the
         ;;   expander's early literal/predicate-style matches.
         (func $regexp-match? (type $Prim2)
               (param $rx-raw (ref eq)) ;; regexp?
               (param $s-raw  (ref eq)) ;; string? or bytes? for the bootstrap subset
               (result        (ref eq))
               (if (i32.eqz (ref.test (ref $Regexp) (local.get $rx-raw)))
                   (then (call $raise-argument-error1
                               (global.get $symbol:regexp-match?)
                               (global.get $string:regexp-replace:pattern-contract)
                               (local.get $rx-raw))
                         (unreachable)))
               (if (ref.test (ref $Bytes) (local.get $s-raw))
                   (then (return_call $regexp-match?/bytes-subset
                                      (ref.cast (ref $Regexp) (local.get $rx-raw))
                                      (ref.cast (ref $Bytes) (local.get $s-raw)))))
               (if (i32.eqz (ref.test (ref $String) (local.get $s-raw)))
                   (then (call $raise-argument-error1
                               (global.get $symbol:regexp-match?)
                               (global.get $string:regexp-match:input-contract)
                               (local.get $s-raw))
                         (unreachable)))
               (return_call $regexp-match?/string-subset
                            (ref.cast (ref $Regexp) (local.get $rx-raw))
                            (ref.cast (ref $String) (local.get $s-raw))))

         ;; regexp-replace/subset : regexp? string? string? boolean? -> string?
         ;;   Bootstrap replacement support for simple string-backed regexp
         ;;   values. It intentionally treats the stored regexp source as a
         ;;   literal string; full regexp replacement belongs to the demodded
         ;;   regexp library.
         (func $regexp-replace/subset
               (param $rx-raw  (ref eq)) ;; regexp? or string?
               (param $input   (ref eq)) ;; string? or bytes?
               (param $insert  (ref eq)) ;; string? for the subset
               (param $all?    (ref eq)) ;; boolean?
               (result         (ref eq))

               (local $source (ref eq))

               (local.set $source (global.get $false))
               (if (i32.eqz (ref.test (ref $String) (local.get $input)))
                   (then (return (local.get $input))))
               (if (i32.eqz (ref.test (ref $String) (local.get $insert)))
                   (then (return (local.get $input))))
               (if (ref.test (ref $String) (local.get $rx-raw))
                   (then (local.set $source (local.get $rx-raw)))
                   (else
                    (if (i32.eqz (ref.test (ref $Regexp) (local.get $rx-raw)))
                        (then (return (local.get $input))))
                    (local.set $source
                               (struct.get $Regexp $source
                                           (ref.cast (ref $Regexp)
                                                     (local.get $rx-raw))))))
               (if (i32.eqz (ref.test (ref $String) (local.get $source)))
                   (then (return (local.get $input))))
               (return_call $string-replace
                            (local.get $input)
                            (local.get $source)
                            (local.get $insert)
                            (local.get $all?)))

         ;; regexp-replace : regexp? string? string? [bytes?] -> string?
         ;;   Bootstrap subset: literal string replacement only.
         (func $regexp-replace (type $Prim34)
               (param $rx-raw (ref eq)) ;; regexp?, byte-regexp?, string?, or bytes?
               (param $input  (ref eq)) ;; string? or bytes?
               (param $insert (ref eq)) ;; string?, bytes?, or procedure?
               (param $prefix (ref eq)) ;; optional bytes?, default = #"", ignored by subset
               (result        (ref eq))
               (if (i32.eqz
                    (i32.or
                     (i32.or (ref.test (ref $Regexp) (local.get $rx-raw))
                             (ref.test (ref $String) (local.get $rx-raw)))
                     (ref.test (ref $Bytes) (local.get $rx-raw))))
                   (then (call $raise-argument-error1
                               (global.get $symbol:regexp-replace)
                               (global.get $string:regexp-replace:pattern-contract)
                               (local.get $rx-raw))
                         (unreachable)))
               (if (i32.eqz
                    (i32.or (ref.test (ref $String) (local.get $input))
                            (ref.test (ref $Bytes) (local.get $input))))
                   (then (call $raise-argument-error1
                               (global.get $symbol:regexp-replace)
                               (global.get $string:regexp-replace:input-contract)
                               (local.get $input))
                         (unreachable)))
               (if (i32.eqz
                    (i32.or
                     (i32.or (ref.test (ref $String) (local.get $insert))
                             (ref.test (ref $Bytes) (local.get $insert)))
                     (ref.eq (call $procedure? (local.get $insert))
                             (global.get $true))))
                   (then (call $raise-argument-error1
                               (global.get $symbol:regexp-replace)
                               (global.get $string:regexp-replace:insert-contract)
                               (local.get $insert))
                         (unreachable)))
               (if (i32.and
                    (i32.eqz (ref.eq (local.get $prefix) (global.get $missing)))
                    (i32.eqz (ref.test (ref $Bytes) (local.get $prefix))))
                   (then (call $raise-argument-error1
                               (global.get $symbol:regexp-replace)
                               (global.get $string:bytes?)
                               (local.get $prefix))
                         (unreachable)))
               (return_call $regexp-replace/subset
                            (local.get $rx-raw)
                            (local.get $input)
                            (local.get $insert)
                            (global.get $false)))

         ;; regexp-replace* : regexp? string? string? [bytes?] -> string?
         ;;   Bootstrap subset: literal string replacement only.
         (func $regexp-replace* (type $Prim>=3)
               (param $rx-raw (ref eq)) ;; regexp?, byte-regexp?, string?, or bytes?
               (param $input  (ref eq)) ;; string? or bytes?
               (param $insert (ref eq)) ;; string?, bytes?, or procedure?
               (param $rest   (ref eq)) ;; optional start/end/prefix args, ignored by subset
               (result        (ref eq))
               (if (i32.eqz
                    (i32.or
                     (i32.or (ref.test (ref $Regexp) (local.get $rx-raw))
                             (ref.test (ref $String) (local.get $rx-raw)))
                     (ref.test (ref $Bytes) (local.get $rx-raw))))
                   (then (call $raise-argument-error1
                               (global.get $symbol:regexp-replace*)
                               (global.get $string:regexp-replace:pattern-contract)
                               (local.get $rx-raw))
                         (unreachable)))
               (if (i32.eqz
                    (i32.or (ref.test (ref $String) (local.get $input))
                            (ref.test (ref $Bytes) (local.get $input))))
                   (then (call $raise-argument-error1
                               (global.get $symbol:regexp-replace*)
                               (global.get $string:regexp-replace:input-contract)
                               (local.get $input))
                         (unreachable)))
               (if (i32.eqz
                    (i32.or
                     (i32.or (ref.test (ref $String) (local.get $insert))
                             (ref.test (ref $Bytes) (local.get $insert)))
                     (ref.eq (call $procedure? (local.get $insert))
                             (global.get $true))))
                   (then (call $raise-argument-error1
                               (global.get $symbol:regexp-replace*)
                               (global.get $string:regexp-replace:insert-contract)
                               (local.get $insert))
                         (unreachable)))
               (return_call $regexp-replace/subset
                            (local.get $rx-raw)
                            (local.get $input)
                            (local.get $insert)
                            (global.get $true)))
     ))

;; regexp-core-runtime-helper-names : (listof symbol?)
;;   Name regular expression helpers available in the standalone core runtime.
(define regexp-core-runtime-helper-names
  '($make-regexp-value
    $regexp?
    $pregexp?
    $byte-regexp?
    $byte-pregexp?
    $bytes-prefix?/i32/checked
    $bytes-suffix?/i32/checked
    $bytes-contains?/checked
    $bytes-index-of-byte/checked))

;; regexp-all-runtime-helpers : (listof core-runtime-helper?)
;;   Enumerate regular expression helpers used by the monolithic runtime.
(define regexp-all-runtime-helpers
  (function-forms->runtime-helpers
   'regexp-all-runtime-helpers
   regexp-runtime-only-forms))

;; regexp-runtime-only-helper-names : (listof symbol?)
;;   Preserve the runtime emission order for regular expression helpers.
(define regexp-runtime-only-helper-names
  (map core-runtime-helper-name regexp-all-runtime-helpers))

;; select-regexp-helpers : (listof core-runtime-helper?) (listof symbol?) -> (listof core-runtime-helper?)
;;   Select helpers by name while preserving the order of the source list.
(define (select-regexp-helpers helpers names)
  (define name-set
    (for/hasheq ([name (in-list names)])
      (values name #t)))
  (for/list ([helper (in-list helpers)]
             #:when (hash-ref name-set (core-runtime-helper-name helper) #f))
    helper))

;; regexp-core-runtime-helpers : (listof core-runtime-helper?)
;;   Enumerate regexp helpers available in the standalone core runtime.
(define regexp-core-runtime-helpers
  (select-regexp-helpers regexp-all-runtime-helpers
                         regexp-core-runtime-helper-names))

;; regexp-runtime-only-helpers : (listof core-runtime-helper?)
;;   Enumerate regexp helpers that still depend on monolithic runtime globals.
(define regexp-runtime-only-helpers
  (let ([core-name-set
         (for/hasheq ([name (in-list regexp-core-runtime-helper-names)])
           (values name #t))])
    (for/list ([helper (in-list regexp-all-runtime-helpers)]
               #:unless (hash-ref core-name-set
                                  (core-runtime-helper-name helper)
                                  #f))
      helper)))

;; regexp-helper-forms : hasheq? (listof symbol?) -> (listof datum?)
;;   Render the given regexp helper names from a helper index.
(define (regexp-helper-forms helper-index names)
  (for/list ([name (in-list names)])
    (core-runtime-helper-form helper-index name)))

;; regexp-runtime-helper-forms : hasheq? -> (listof datum?)
;;   Render regexp helpers in runtime assembly order.
(define (regexp-runtime-helper-forms helper-index)
  (regexp-helper-forms helper-index regexp-runtime-only-helper-names))

(module+ test
  (require rackunit)

  ;; form-name : datum? -> symbol?
  ;;   Return the function name from a rendered helper form.
  (define (form-name form)
    (second form))

  (define regexp-helper-index
    (make-core-runtime-helper-index
     (append regexp-core-runtime-helpers
             regexp-runtime-only-helpers)))

  (check-equal?
   (map core-runtime-helper-name regexp-core-runtime-helpers)
   regexp-core-runtime-helper-names)

  (check-equal?
   (map form-name (regexp-runtime-helper-forms regexp-helper-index))
   regexp-runtime-only-helper-names))
