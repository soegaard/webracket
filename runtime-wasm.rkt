                      ($remv $prim:eqv?)
                      ($remw $prim:equal-always?))])


        (func $remove* (type $Prim3)
              (param $v-lst (ref eq))   ;; list of values
              (param $lst   (ref eq))   ;; list
              (param $proc  (ref eq))   ;; optional comparator, defaults to equal?
              (result       (ref eq))

              (local $cur       (ref eq))
              (local $pair      (ref $Pair))
              (local $elem      (ref eq))
              (local $acc       (ref eq))
              (local $res       (ref eq))
              (local $tail      (ref eq))
              (local $vlcur     (ref eq))
              (local $vlpair    (ref $Pair))
              (local $v         (ref eq))
              (local $args      (ref $Args))
              (local $r         (ref eq))
              (local $found     i32)
              (local $modified  i32)

              ;; Handle optional comparator
              ;; Defaults to equal? when not supplied
              (if (ref.eq (local.get $proc) (global.get $missing))
                  (then (local.set $proc (global.get $prim:equal?)))
                  (else
                   (if (i32.eqz (ref.test (ref $Procedure) (local.get $proc)))
                       (then (call $raise-argument-error:procedure-expected (local.get $proc))
                             (unreachable))
                       (else))))

              ;; If v-lst is empty, return lst directly
              (if (ref.eq (local.get $v-lst) (global.get $null))
                  (then (return (local.get $lst))))

              (local.set $cur (local.get $lst))
              (local.set $acc (global.get $null))
              (local.set $modified (i32.const 0))

              (block $done
                     (loop $loop
                           (if (ref.eq (local.get $cur) (global.get $null))
                               (then (br $done)))

                           (if (i32.eqz (ref.test (ref $Pair) (local.get $cur)))
                               (then (call $raise-pair-expected (local.get $cur))
                                     (unreachable))
                               (else))

                           (local.set $pair (ref.cast (ref $Pair) (local.get $cur)))
                           (local.set $elem (struct.get $Pair $a (local.get $pair)))
                           (local.set $tail (struct.get $Pair $d (local.get $pair)))

                           ;; search v-lst for elem
                           (local.set $vlcur (local.get $v-lst))
                           (local.set $found (i32.const 0))
                           (block $search-done
                                  (loop $search
                                        (if (ref.eq (local.get $vlcur) (global.get $null))
                                            (then (br $search-done)))
                                        (if (i32.eqz (ref.test (ref $Pair) (local.get $vlcur)))
                                            (then (call $raise-pair-expected (local.get $vlcur))
                                                  (unreachable))
                                            (else))
                                        (local.set $vlpair (ref.cast (ref $Pair) (local.get $vlcur)))
                                        (local.set $v (struct.get $Pair $a (local.get $vlpair)))
                                        (local.set $vlcur (struct.get $Pair $d (local.get $vlpair)))
                                          (block $cont
                                                 (local.set $r
                                                            (call_ref $ProcedureInvoker
                                                                      (ref.cast (ref $Procedure) (local.get $proc))
                                                                      (block (result (ref $Args))
                                                                             (local.set $args (array.new $Args (global.get $null) (i32.const 2)))
                                                                             (array.set $Args (local.get $args) (i32.const 0) (local.get $v))
                                                                             (array.set $Args (local.get $args) (i32.const 1) (local.get $elem))
                                                                             (local.get $args))
                                                                      (struct.get $Procedure $invoke
                                                                                  (ref.cast (ref $Procedure) (local.get $proc))))
                                                 (if (ref.eq (local.get $r) (global.get $false))
                                                     (then (br $cont))
                                                     (else (local.set $found (i32.const 1)) (br $search-done)))))

                           (if (i32.eqz (local.get $found))
                               (then (local.set $acc (call $cons (local.get $elem) (local.get $acc))))
                               (else (local.set $modified (i32.const 1))))

                           (local.set $cur (local.get $tail))
                           (br $loop))

                     ;; done loop
                     )

              (if (i32.eqz (local.get $modified))
                  (then (return (local.get $lst))))

              ;; rebuild reversed accumulator
              (local.set $cur (local.get $acc))
              (local.set $res (global.get $null))
              (loop $rev
                    (if (ref.eq (local.get $cur) (global.get $null))
                        (then (return (local.get $res)))
                        (else))
                    (local.set $pair (ref.cast (ref $Pair) (local.get $cur)))
                    (local.set $res (call $cons (struct.get $Pair $a (local.get $pair))
                                              (local.get $res)))
                    (local.set $cur (struct.get $Pair $d (local.get $pair)))
                    (br $rev))
              (unreachable))


        ;; $remq*, $remv*, and $remw* implemented via remove*
        ,@(let ([ops '(($remq* $prim:eq?)
                       ($remv* $prim:eqv?)
                       ($remw* $prim:equal-always?))])
            (for/list ([p ops])
              (define name (car p))
              (define cmp  (cadr p))

              `(func ,name (type $Prim2)
                     (param $v-lst (ref eq))
                     (param $lst   (ref eq))
                     (result       (ref eq))

                     (return_call $remove*
                                  (local.get $v-lst)
                                  (local.get $lst)
                                  (global.get ,cmp)))))
