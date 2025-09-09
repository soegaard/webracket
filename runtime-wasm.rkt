        (func $remf (type $Prim2)
              (param $pred (ref eq))  ;; predicate
              (param $lst  (ref eq))  ;; list
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

              ;; Validate predicate
              (if (i32.eqz (ref.test (ref $Procedure) (local.get $pred)))
                  (then (call $raise-argument-error:procedure-expected (local.get $pred))
                        (unreachable)))
              (local.set $f    (ref.cast (ref $Procedure) (local.get $pred)))
              (local.set $finv (struct.get $Procedure $invoke (local.get $f)))

              ;; Prepare argument array
              (local.set $call (array.new $Args (global.get $null) (i32.const 1)))

              ;; Iterate through list until match found
              (local.set $cur (local.get $lst))
              (local.set $acc (global.get $null))
              (loop $loop
                    (if (ref.eq (local.get $cur) (global.get $null))
                        (then (return (local.get $lst))))
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
                        (then
                         (local.set $acc (call $cons (local.get $elem) (local.get $acc)))
                         (local.set $cur (struct.get $Pair $d (local.get $pair)))
                         (br $loop)))
                    ;; Found match: rebuild with accumulator
                    (local.set $cur (struct.get $Pair $d (local.get $pair)))
                    (local.set $res (local.get $cur))
                    (local.set $cur (local.get $acc))
                    (loop $rev
                          (if (ref.eq (local.get $cur) (global.get $null))
                              (then (return (local.get $res))))
                          (local.set $pair (ref.cast (ref $Pair) (local.get $cur)))
                          (local.set $res
                                     (call $cons
                                           (struct.get $Pair $a (local.get $pair))
                                           (local.get $res)))
                          (local.set $cur (struct.get $Pair $d (local.get $pair)))
                          (br $rev)))
              (unreachable))

        (func $remf* (type $Prim2)
              (param $pred (ref eq)) (param $lst (ref eq))
              (result (ref eq))
              (return_call $filter-not (local.get $pred) (local.get $lst)))

        (func $filter (type $Prim>=1)
        (func $remove (type $Prim3)
                     (local.set $cur (struct.get $Pair $d (local.get $pair)))
                      (br $rev)))
              (unreachable))
        (func $remove* (param $vlist (ref eq)) (param $lst (ref eq)) (param $proc (ref eq)) (result (ref eq))
              (local $cur  (ref eq))
              (local $pair (ref $Pair))
              (local $v    (ref eq))
              (local $res  (ref eq))

              (local.set $res (local.get $lst))
              (local.set $cur (local.get $vlist))
              (loop $loop
                    (if (ref.eq (local.get $cur) (global.get $null))
                        (then (return (local.get $res))))
                    (if (i32.eqz (ref.test (ref $Pair) (local.get $cur)))
                        (then (call $raise-pair-expected (local.get $cur))
                              (unreachable)))
                    (local.set $pair (ref.cast (ref $Pair) (local.get $cur)))
                    (local.set $v    (struct.get $Pair $a (local.get $pair)))
                    (local.set $res  (call $remove (local.get $v) (local.get $res) (local.get $proc)))
                    (local.set $cur  (struct.get $Pair $d (local.get $pair)))
                    (br $loop))
              (unreachable))

        (func $remq (type $Prim2)
              (param $v (ref eq)) (param $lst (ref eq)) (result (ref eq))
              (return_call $remove (local.get $v) (local.get $lst) (global.get $eq?)))

        (func $remv (type $Prim2)
              (param $v (ref eq)) (param $lst (ref eq)) (result (ref eq))
              (return_call $remove (local.get $v) (local.get $lst) (global.get $eqv?)))

        ;; equal-always? not yet implemented; use equal?
        (func $remw (type $Prim2)
              (param $v (ref eq)) (param $lst (ref eq)) (result (ref eq))
              (return_call $remove (local.get $v) (local.get $lst) (global.get $equal?)))

        (func $remq* (type $Prim2)
              (param $vlist (ref eq)) (param $lst (ref eq)) (result (ref eq))
              (return_call $remove* (local.get $vlist) (local.get $lst) (global.get $eq?)))

        (func $remv* (type $Prim2)
              (param $vlist (ref eq)) (param $lst (ref eq)) (result (ref eq))
              (return_call $remove* (local.get $vlist) (local.get $lst) (global.get $eqv?)))
        ;; equal-always? not yet implemented; use equal?
        (func $remw* (type $Prim2)
              (param $vlist (ref eq)) (param $lst (ref eq)) (result (ref eq))
              (return_call $remove* (local.get $vlist) (local.get $lst) (global.get $equal?)))
