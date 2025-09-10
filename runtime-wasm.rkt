

        ;; Remove list elements using predicate
        (func $remf/impl
              (param $proc (ref eq)) ;; predicate
              (param $lst  (ref eq)) ;; list
              (param $all  i32)      ;; non-zero => remove all matches
              (result      (ref eq))

              (local $f     (ref $Procedure))
              (local $finv  (ref $ProcedureInvoker))
              (local $cur   (ref eq))
              (local $pair  (ref $Pair))
              (local $elem  (ref eq))
              (local $tail  (ref eq))
              (local $acc   (ref eq))
              (local $res   (ref eq))
              (local $args  (ref $Args))
              (local $r     (ref eq))
              (local $found i32)

              ;; Ensure proc is a procedure
              (if (i32.eqz (ref.test (ref $Procedure) (local.get $proc)))
                  (then (call $raise-argument-error:procedure-expected (local.get $proc))
                        (unreachable)))
              (local.set $f    (ref.cast (ref $Procedure) (local.get $proc)))
              (local.set $finv (struct.get $Procedure $invoke (local.get $f)))
              (local.set $args (array.new $Args (global.get $null) (i32.const 1)))

              ;; Iterate through list
              (local.set $cur (local.get $lst))
              (local.set $acc (global.get $null))
              (local.set $found (i32.const 0))
              (block $done
                     (loop $loop
                           (if (ref.eq (local.get $cur) (global.get $null))
                               (then (br $done)))
                           (if (i32.eqz (ref.test (ref $Pair) (local.get $cur)))
                               (then (call $raise-pair-expected (local.get $cur))
                                     (unreachable)))
                           (local.set $pair (ref.cast (ref $Pair) (local.get $cur)))
                           (local.set $elem (struct.get $Pair $a (local.get $pair)))
                           (local.set $tail (struct.get $Pair $d (local.get $pair)))
                           (array.set $Args (local.get $args) (i32.const 0) (local.get $elem))
                           (local.set $r
                                      (call_ref $ProcedureInvoker
                                                (local.get $f)
                                                (local.get $args)
                                                (local.get $finv)))
                           (if (ref.eq (local.get $r) (global.get $false))
                               (then
                                (local.set $acc (call $cons (local.get $elem) (local.get $acc)))
                                (local.set $cur (local.get $tail))
                                (br $loop))
                               (else
                                (local.set $found (i32.const 1))
                                (local.set $cur (local.get $tail))
                                (if (i32.eqz (local.get $all))
                                    (then (br $done))
                                    (else (br $loop)))))))
              (if (i32.eqz (local.get $found))
                  (then (return (local.get $lst))))
              (local.set $res (local.get $cur))
              (local.set $cur (local.get $acc))
              (loop $rev
                    (if (ref.eq (local.get $cur) (global.get $null))
                        (then (return (local.get $res))))
                    (local.set $pair (ref.cast (ref $Pair) (local.get $cur)))
                    (local.set $res (call $cons (struct.get $Pair $a (local.get $pair))
                                          (local.get $res)))
                    (local.set $cur (struct.get $Pair $d (local.get $pair)))
                    (br $rev))
              (unreachable))

        (func $remf (type $Prim2)
              (param $proc (ref eq)) ;; predicate
              (param $lst  (ref eq)) ;; list
              (result      (ref eq))

              (return_call $remf/impl (local.get $proc) (local.get $lst) (i32.const 0)))

        (func $remf* (type $Prim2)
              (param $proc (ref eq)) ;; predicate
              (param $lst  (ref eq)) ;; list
              (result      (ref eq))

              (return_call $remf/impl (local.get $proc) (local.get $lst) (i32.const 1)))
