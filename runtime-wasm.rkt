        (func $bytes (type $Prim>=0)
              (param $args (ref eq))
              (result (ref eq))
              (local $as        (ref $Args))
              (local $len       i32)
              (local $arr       (ref $I8Array))
              (local $i         i32)
              (local $x         (ref eq))
              (local $v         i32)
              (local $use-args? i32)
              (local $list      (ref eq))
              (local $node      (ref eq))
              (local $pair      (ref $Pair))

              ;; Determine whether we received an $Args array or a list of rest arguments.
              (local.set $use-args? (ref.test (ref $Args) (local.get $args)))
              (local.set $list      (global.get $null))
              (local.set $node      (global.get $null))
              (local.set $len
                         (if (result i32) (local.get $use-args?)
                             (then
                              (local.set $as (ref.cast (ref $Args) (local.get $args)))
                              (array.len (local.get $as)))
                             (else
                              (local.set $list (local.get $args))
                              (local.set $node (local.get $list))
                              (call $length/i32 (local.get $list)))))
              ;; Allocate mutable byte array
              (local.set $arr (call $i8make-array (local.get $len) (i32.const 0)))
              ;; Populate array from arguments or list elements
              (local.set $i (i32.const 0))
              (block $done
                     (loop $loop
                           (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
                           (if (i32.eqz (local.get $use-args?))
                               (then
                                (if (ref.test (ref $Pair) (local.get $node))
                                    (then
                                     (local.set $pair (ref.cast (ref $Pair) (local.get $node)))
                                     (local.set $x    (struct.get $Pair $a (local.get $pair)))
                                     (local.set $node (struct.get $Pair $d (local.get $pair))))
                                    (else
                                     (call $raise-pair-expected (local.get $node))
                                     (unreachable))))
                               (else
                                (local.set $x (array.get $Args (local.get $as) (local.get $i)))))
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
        (func $string (type $Prim>=0)
              (param $args (ref eq))
              (result      (ref eq))
              (local $argv      (ref $Args))
              (local $len       i32)
              (local $arr       (ref $I32Array))
              (local $i         i32)
              (local $ch        (ref eq))
              (local $use-args? i32)
              (local $list      (ref eq))
              (local $node      (ref eq))
              (local $pair      (ref $Pair))

              ;; Support both $Args arrays and ordinary rest argument lists.
              (local.set $use-args? (ref.test (ref $Args) (local.get $args)))
              (local.set $list      (global.get $null))
              (local.set $node      (global.get $null))
              (local.set $len
                         (if (result i32) (local.get $use-args?)
                             (then
                              (local.set $argv (ref.cast (ref $Args) (local.get $args)))
                              (array.len (local.get $argv)))
                             (else
                              (local.set $list (local.get $args))
                              (local.set $node (local.get $list))
                              (call $length/i32 (local.get $list)))))

              ;; Allocate array for codepoints
              (local.set $arr (array.new $I32Array (i32.const 0) (local.get $len)))

              ;; Fill array with characters from either representation
              (local.set $i (i32.const 0))
              (block $done
                     (loop $loop
                           (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
                           (if (i32.eqz (local.get $use-args?))
                               (then
                                (if (ref.test (ref $Pair) (local.get $node))
                                    (then
                                     (local.set $pair (ref.cast (ref $Pair) (local.get $node)))
                                     (local.set $ch   (struct.get $Pair $a (local.get $pair)))
                                     (local.set $node (struct.get $Pair $d (local.get $pair))))
                                    (else
                                     (call $raise-pair-expected (local.get $node))
                                     (unreachable))))
                               (else
                                (local.set $ch (array.get $Args (local.get $argv) (local.get $i)))))
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
               (local $as        (ref $Args))
               (local $len       i32)
               (local $arr       (ref $Array))
               (local $use-args? i32)
               (local $list      (ref eq))
               (local $node      (ref eq))
               (local $pair      (ref $Pair))
               (local $i         i32)
               (local $x         (ref eq))

               (local.set $use-args? (ref.test (ref $Args) (local.get $args)))
               (local.set $list      (global.get $null))
               (local.set $node      (global.get $null))
               (local.set $len
                          (if (result i32) (local.get $use-args?)
                              (then
                               (local.set $as (ref.cast (ref $Args) (local.get $args)))
                               (array.len (local.get $as)))
                              (else
                               (local.set $list (local.get $args))
                               (local.set $node (local.get $list))
                               (call $length/i32 (local.get $list)))))

               (if (local.get $use-args?)
                   (then
                    (array.copy $Array $Args
                                (local.get $arr)
                                (i32.const 0)
                                (local.get $as)
                                (i32.const 0)
                                (local.get $len)))
                   (else
                    (local.set $i (i32.const 0))
                    (block $done
                           (loop $loop
                                 (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
                                 (if (ref.test (ref $Pair) (local.get $node))
                                     (then
                                      (local.set $pair (ref.cast (ref $Pair) (local.get $node)))
                                      (local.set $x    (struct.get $Pair $a (local.get $pair)))
                                      (local.set $node (struct.get $Pair $d (local.get $pair))))
                                     (else
                                      (call $raise-pair-expected (local.get $node))
                                      (unreachable)))
                                 (array.set $Array (local.get $arr) (local.get $i) (local.get $x))
                                 (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                 (br $loop)))))

              (local $as        (ref $Args))
              (local $len       i32)
              (local $arr       (ref $Array))
              (local $use-args? i32)
              (local $list      (ref eq))
              (local $node      (ref eq))
              (local $pair      (ref $Pair))
              (local $i         i32)
              (local $x         (ref eq))

              (local.set $use-args? (ref.test (ref $Args) (local.get $args)))
              (local.set $list      (global.get $null))
              (local.set $node      (global.get $null))
              (local.set $len
                         (if (result i32) (local.get $use-args?)
                             (then
                              (local.set $as (ref.cast (ref $Args) (local.get $args)))
                              (array.len (local.get $as)))
                             (else
                              (local.set $list (local.get $args))
                              (local.set $node (local.get $list))
                              (call $length/i32 (local.get $list)))))

              (if (local.get $use-args?)
                  (then
                   (array.copy $Array $Args
                               (local.get $arr)
                               (i32.const 0)
                               (local.get $as)
                               (i32.const 0)
                               (local.get $len)))
                  (else
                   (local.set $i (i32.const 0))
                   (block $done
                          (loop $loop
                                (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
                                (if (ref.test (ref $Pair) (local.get $node))
                                    (then
                                     (local.set $pair (ref.cast (ref $Pair) (local.get $node)))
                                     (local.set $x    (struct.get $Pair $a (local.get $pair)))
                                     (local.set $node (struct.get $Pair $d (local.get $pair))))
                                    (else
                                     (call $raise-pair-expected (local.get $node))
                                     (unreachable)))
                                (array.set $Array (local.get $arr) (local.get $i) (local.get $x))
                                (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                (br $loop)))))

        (func $values (type $Prim>=0)
              (param $args (ref eq))
              (result      (ref eq))
              (local $as        (ref $Args))
              (local $n         i32)
              (local $vals      (ref $Values))
              (local $i         i32)
              (local $use-args? i32)
              (local $list      (ref eq))
              (local $node      (ref eq))
              (local $pair      (ref $Pair))

              ;; Determine argument representation.
              (local.set $use-args? (ref.test (ref $Args) (local.get $args)))
              (local.set $list      (global.get $null))
              (local.set $node      (global.get $null))
              (local.set $n
                         (if (result i32) (local.get $use-args?)
                             (then
                              (local.set $as (ref.cast (ref $Args) (local.get $args)))
                              (array.len (local.get $as)))
                             (else
                              (local.set $list (local.get $args))
                              (local.set $node (local.get $list))
                              (call $length/i32 (local.get $list)))))
              ;; Fast path when only a single value is returned.
              (if (i32.eq (local.get $n) (i32.const 1))
                  (then
                   (if (local.get $use-args?)
                       (then (return (array.get $Args (local.get $as) (i32.const 0))))
                       (else
                        (if (ref.test (ref $Pair) (local.get $list))
                            (then (return (struct.get $Pair $a (ref.cast (ref $Pair) (local.get $list)))))
                            (else (call $raise-pair-expected (local.get $list))
                                  (unreachable))))))
              ;; Allocate $Values array and populate from either representation.
              (if (result (ref eq)) (local.get $use-args?)
                  (then
                   (array.copy $Values $Args
                               (local.get $vals) (i32.const 0)
                               (local.get $as)   (i32.const 0)
                               (local.get $n))
                   (local.get $vals))
                  (else
                   (local.set $i (i32.const 0))
                   (block $done
                          (loop $loop
                                (br_if $done (i32.ge_u (local.get $i) (local.get $n)))
                                (if (ref.test (ref $Pair) (local.get $node))
                                    (then
                                     (local.set $pair (ref.cast (ref $Pair) (local.get $node)))
                                     (array.set $Values (local.get $vals)
                                                 (local.get $i)
                                                 (struct.get $Pair $a (local.get $pair)))
                                     (local.set $node (struct.get $Pair $d (local.get $pair))))
                                    (else
                                     (call $raise-pair-expected (local.get $node))
                                     (unreachable)))
                                (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                (br $loop)))
                   (local.get $vals)))

