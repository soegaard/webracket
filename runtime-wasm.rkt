         ,@(for/list ([name   '($memq $memv $member $memw)]
                      [type   '($Prim2 $Prim2 $Prim3 $Prim2)]
                      [needs-proc '(0 0 1 0)]
                      [cmp    '((ref.eq (local.get $needle) (local.get $elem))
                                (ref.eq (call $eqv? (local.get $needle) (local.get $elem))
                                        (global.get $true))
                                (ref.eq (global.get $false) (global.get $false)) ; unused for member
                                (ref.eq (call $equal-always? (local.get $needle) (local.get $elem))
                                        (global.get $true)))]
             `(func ,name
                    (type ,type)
                    (param $needle (ref eq))    ;; value to find
                    (param $xs     (ref eq))    ;; list to search
                    ,@(if (zero? needs-proc) '() '((param $same? (ref eq)) ;; optional comparator, defaults to equal?
                                                  ))
                    (result (ref eq))
                    (local $pair (ref $Pair))
                    (local $elem (ref eq))
                    ,@(if (zero? needs-proc) '() '((local $args     (ref $Args))
                                                  (local $res      (ref eq))
                                                  (local $use-proc i32)))
                    ,@(if (zero? needs-proc)
                          '()
                          `((if (ref.eq (local.get $same?) (global.get $missing))
                                (then (local.set $use-proc (i32.const 0)))
                                (else
                                 (if (i32.eqz (ref.test (ref $Procedure) (local.get $same?)))
                                     (then (call $raise-argument-error:procedure-expected (local.get $same?))
                                           (unreachable)))
                                 (local.set $use-proc (i32.const 1))))
                            (local.set $args (array.new $Args (global.get $null) (i32.const 2)))))
                    (loop $search
                          ;; 1) end-of-list? => not found
                          (if (ref.eq (local.get $xs) (global.get $null))
                              (then (return (global.get $false))))
                          ;; 2) must be a Pair
                          (if (i32.eqz (ref.test (ref $Pair) (local.get $xs)))
                              (then (call $raise-pair-expected (local.get $xs))
                                    (unreachable)))
                          (local.set $pair (ref.cast (ref $Pair) (local.get $xs)))
                          (local.set $elem (struct.get $Pair $a (local.get $pair)))
                          ,(if (zero? needs-proc)
                               `(if ,cmp (then (return (local.get $xs))))
                               `(if (i32.eqz (local.get $use-proc))
                                    (then
                                     (if (ref.eq (call $equal? (local.get $needle) (local.get $elem))
                                                 (global.get $true))
                                         (then (return (local.get $xs)))
                                         (else (nop))))
                                    (else
                                     (array.set $Args (local.get $args) (i32.const 0) (local.get $needle))
                                     (array.set $Args (local.get $args) (i32.const 1) (local.get $elem))
                                     (local.set $res
                                                (call_ref $ProcedureInvoker
                                                          (ref.cast (ref $Procedure) (local.get $same?))
                                                          (local.get $args)
                                                          (struct.get $Procedure $invoke
                                                                      (ref.cast (ref $Procedure) (local.get $same?)))))
                                     (if (ref.eq (local.get $res) (global.get $true))
                                         (then (return (local.get $xs)))
                                         (else (nop))))))
                          ;; 3) advance to cdr
                          (local.set $xs (struct.get $Pair $d (local.get $pair)))
                          (br $search))
                    (unreachable)))
