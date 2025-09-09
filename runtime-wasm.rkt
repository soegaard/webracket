                            ;; search v-lst for elem
                            (local.set $vlcur (local.get $v-lst))
                            (local.set $found (i32.const 0))
                            (block $search-done
                                   (loop $search
                                         (if (ref.eq (local.get $vlcur) (global.get $null))
                                             (then (br $search-done)))
                                         (if (i32.eqz (ref.test (ref $Pair) (local.get $vlcur)))
                                             (then (call $raise-pair-expected (local.get $vlcur))
                                                   (unreachable)))
                                         (local.set $vlpair (ref.cast (ref $Pair) (local.get $vlcur)))
                                         (local.set $v      (struct.get $Pair $a (local.get $vlpair)))
                                         (local.set $vlcur  (struct.get $Pair $d (local.get $vlpair)))
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
