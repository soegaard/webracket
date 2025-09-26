                      (if (i32.eqz (local.get $use-args?))
                          (then
                           (loop $loop-list
                                 (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
                                      (unreachable)))
                                 (if (ref.test (ref i31) (local.get $x))
                                      (local.set $v (i31.get_u (ref.cast (ref i31) (local.get $x))))
                                      (if (i32.eqz (i32.and (local.get $v) (i32.const 1)))
                                           (local.set $v (i32.shr_u (local.get $v) (i32.const 1)))
                                           (if (i32.lt_u (local.get $v) (i32.const 256))
                                               (then
                                                (call $i8array-set! (local.get $arr) (local.get $i) (local.get $v)))
                                               (else
                                                (call $raise-byte-out-of-range (local.get $x))
                                                (unreachable))))
                                           (call $raise-check-fixnum (local.get $x))
                                      (unreachable)))
                                 (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                 (br $loop-list)))
                          (else
                           (loop $loop-args
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
                                 (br $loop-args))))
                      (if (i32.eqz (local.get $use-args?))
                          (then
                           (loop $loop-list
                                 (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
                                      (unreachable)))
                                 (array.set $I32Array
                                            (local.get $arr)
                                            (local.get $i)
                                            (call $char->integer/i32 (local.get $ch)))
                                 (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                 (br $loop-list)))
                          (else
                           (loop $loop-args
                                 (br_if $done (i32.ge_u (local.get $i) (local.get $len)))
                                 (local.set $ch (array.get $Args (local.get $argv) (local.get $i)))
                                 (array.set $I32Array
                                            (local.get $arr)
                                            (local.get $i)
                                            (call $char->integer/i32 (local.get $ch)))
                                 (local.set $i (i32.add (local.get $i) (i32.const 1)))
                                 (br $loop-args))))
