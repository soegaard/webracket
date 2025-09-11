        (func $group-by:update-groups/equal?
              (param $k (ref eq))      ;; key
              (param $x (ref eq))      ;; element
              (param $groups (ref eq)) ;; groups list
              (result (ref eq))
              (local $gs    (ref eq))
              (local $pair  (ref $Pair))
              (local $bucket (ref $Pair))
              (local $bk    (ref eq))
              (local $box   (ref eq))
              (local.set $gs (local.get $groups))
              (loop $loop
                    (if (ref.eq (local.get $gs) (global.get $null))
                        (then (return (call $cons
                                            (call $cons (local.get $k)
                                                        (call $box
                                                              (call $cons (local.get $x)
                                                                          (global.get $null))))
                                            (local.get $groups)))))
                    (local.set $pair (ref.cast (ref $Pair) (local.get $gs)))
                    (local.set $bucket (ref.cast (ref $Pair) (struct.get $Pair $a (local.get $pair))))
                    (local.set $bk  (struct.get $Pair $a (local.get $bucket)))
                    (local.set $box (struct.get $Pair $d (local.get $bucket)))
                    (if (ref.eq (call $equal? (local.get $k) (local.get $bk)) (global.get $false))
                        (then (local.set $gs (struct.get $Pair $d (local.get $pair)))
                              (br $loop))
                        (else (call $set-box!
                                    (local.get $box)
                                    (call $cons (local.get $x)
                                                (call $unbox (local.get $box))))
                              (return (local.get $groups)))))
              (unreachable))

        (func $group-by:update-groups/general
              (param $k (ref eq))      ;; key
              (param $x (ref eq))      ;; element
              (param $groups (ref eq)) ;; groups list
              (param $same (ref $Procedure)) ;; comparator
              (param $same-inv (ref $ProcedureInvoker))
              (result (ref eq))
              (local $gs    (ref eq))
              (local $pair  (ref $Pair))
              (local $bucket (ref $Pair))
              (local $bk    (ref eq))
              (local $box   (ref eq))
              (local $args  (ref $Args))
              (local $res   (ref eq))
              (local.set $gs (local.get $groups))
              (local.set $args (array.new $Args (global.get $null) (i32.const 2)))
              (loop $loop
                    (if (ref.eq (local.get $gs) (global.get $null))
                        (then (return (call $cons
                                            (call $cons (local.get $k)
                                                        (call $box
                                                              (call $cons (local.get $x)
                                                                          (global.get $null))))
                                            (local.get $groups)))))
                    (local.set $pair (ref.cast (ref $Pair) (local.get $gs)))
                    (local.set $bucket (ref.cast (ref $Pair) (struct.get $Pair $a (local.get $pair))))
                    (local.set $bk  (struct.get $Pair $a (local.get $bucket)))
                    (local.set $box (struct.get $Pair $d (local.get $bucket)))
                    (array.set $Args (local.get $args) (i32.const 0) (local.get $k))
                    (array.set $Args (local.get $args) (i32.const 1) (local.get $bk))
                    (local.set $res (call_ref $ProcedureInvoker
                                              (local.get $same)
                                              (local.get $args)
                                              (local.get $same-inv)))
                    (if (ref.eq (local.get $res) (global.get $false))
                        (then (local.set $gs (struct.get $Pair $d (local.get $pair)))
                              (br $loop))
                        (else (call $set-box!
                                    (local.get $box)
                                    (call $cons (local.get $x)
                                                (call $unbox (local.get $box))))
                              (return (local.get $groups)))))
              (unreachable))

        (func $group-by:extract-groups
              (param $groups (ref eq))
              (result (ref eq))
              (local $cur  (ref eq))
              (local $pair (ref $Pair))
              (local $bucket (ref $Pair))
              (local $box  (ref eq))
              (local $acc  (ref eq))
              (local.set $cur (local.get $groups))
              (local.set $acc (global.get $null))
              (loop $loop
                    (if (ref.eq (local.get $cur) (global.get $null))
                        (then (return (call $reverse (local.get $acc)))))
                    (local.set $pair (ref.cast (ref $Pair) (local.get $cur)))
                    (local.set $bucket (ref.cast (ref $Pair) (struct.get $Pair $a (local.get $pair))))
                    (local.set $box (struct.get $Pair $d (local.get $bucket)))
                    (local.set $acc (call $cons (call $unbox (local.get $box)) (local.get $acc)))
                    (local.set $cur (struct.get $Pair $d (local.get $pair)))
                    (br $loop))
              (unreachable))

        ;; Groups list elements by a key function. The third parameter is
        ;; optional and defaults to equal?. Group and element order are
        ;; not guaranteed.
              (local $f       (ref $Procedure))
              (local $finv    (ref $ProcedureInvoker))
              (local $cur     (ref eq))
              (local $pair    (ref $Pair))
              (local $x       (ref eq))
              (local $call    (ref $Args))
              (local $k       (ref eq))
              (local $groups  (ref eq))
              (local $use-proc i32)
              (local $same-f  (ref $Procedure))
              (local $same-inv (ref $ProcedureInvoker))

              ;; Ensure proc is a procedure and fetch invoker
              (if (i32.eqz (ref.test (ref $Procedure) (local.get $proc)))
                  (then (call $raise-argument-error:procedure-expected (local.get $proc))
                        (unreachable)))
              (local.set $f    (ref.cast (ref $Procedure) (local.get $proc)))
              (local.set $finv (struct.get $Procedure $invoke (local.get $f)))

              ;; Handle optional comparator
              (if (ref.eq (local.get $same?) (global.get $missing))
                  (then (local.set $use-proc (i32.const 0)))
                  (else
                   (if (i32.eqz (ref.test (ref $Procedure) (local.get $same?)))
                       (then (call $raise-argument-error:procedure-expected (local.get $same?))
                             (unreachable)))
                   (local.set $use-proc (i32.const 1))
                   (local.set $same-f   (ref.cast (ref $Procedure) (local.get $same?)))
                   (local.set $same-inv (struct.get $Procedure $invoke (local.get $same-f)))))

              ;; Prepare argument buffer for key function
              (local.set $call (array.new $Args (global.get $null) (i32.const 1)))

              ;; Iterate over list building groups
              (local.set $groups (global.get $null))
              (local.set $cur (local.get $xs))
              (loop $loop
                    (if (ref.eq (local.get $cur) (global.get $null))
                        (then (return (call $group-by:extract-groups (local.get $groups)))))
                    (if (i32.eqz (ref.test (ref $Pair) (local.get $cur)))
                        (then (call $raise-pair-expected (local.get $cur))
                              (unreachable)))
                    (local.set $pair (ref.cast (ref $Pair) (local.get $cur)))
                    (local.set $x (struct.get $Pair $a (local.get $pair)))
                    (array.set $Args (local.get $call) (i32.const 0) (local.get $x))
                    (local.set $k (call_ref $ProcedureInvoker
                                            (local.get $f)
                                            (local.get $call)
                                            (local.get $finv)))
                    (local.set $groups
                               (if (result (ref eq)) (i32.eqz (local.get $use-proc))
                                   (then (call $group-by:update-groups/equal?
                                               (local.get $k)
                                               (local.get $x)
                                               (local.get $groups)))
                                   (else (call $group-by:update-groups/general
                                               (local.get $k)
                                               (local.get $x)
                                               (local.get $groups)
                                               (local.get $same-f)
                                               (local.get $same-inv)))))
                    (local.set $cur (struct.get $Pair $d (local.get $pair)))
                    (br $loop))
              (unreachable))
