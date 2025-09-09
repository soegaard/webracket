         (func $raise-argument-error
               (param $who (ref eq))       ;; symbol
               (param $expected (ref eq))  ;; expected description
               (param $got (ref eq))       ;; received value
               (call $js-log (local.get $who))
               (call $js-log (local.get $expected))
               (call $js-log (local.get $got))
               (unreachable))
        ;; Checkers
        (func $check-list (type $Prim1)
              (param $l (ref eq))                 ;; list
              (result (ref eq))
              (if (ref.eq (call $list? (local.get $l)) (global.get $true))
                  (then (global.get $void))
                  (else (call $raise-argument-error (R 'in-list) (R "list?") (local.get $l))
                        (unreachable))))

        ;; Racket's check-mlist accepts only mutable pairs; mpairs are
        ;; currently unsupported, so this version accepts any pair.
        (func $check-mlist (type $Prim1)
              (param $l (ref eq))                 ;; mutable list
              (result (ref eq))
              (if (ref.eq (local.get $l) (global.get $null))
                  (then (global.get $void))
                  (else (if (ref.eq (call $pair? (local.get $l)) (global.get $true))
                            (then (global.get $void))
                            (else (call $raise-argument-error
                                        (R 'in-mlist)
                                        (R "(or/c mpair? null?)")
                                        (local.get $l))
                                  (unreachable))))))

        (func $check-range (type $Prim3)
              (param $a (ref eq))     ;; start
              (param $b (ref eq))     ;; end
              (param $step (ref eq))  ;; step
              (result (ref eq))
              (call $check-range-generic (R 'in-range)
                                         (local.get $a)
                                         (local.get $b)
                                         (local.get $step)))

        (func $check-range-generic
              (param $who  (ref eq))  ;; symbol
              (param $a    (ref eq))  ;; start
              (param $b    (ref eq))  ;; end
              (param $step (ref eq))  ;; step
              (result (ref eq))
              (if (ref.eq (call $real? (local.get $a)) (global.get $true))
                  (then (if (ref.eq (call $real? (local.get $b)) (global.get $true))
                            (then (if (ref.eq (call $real? (local.get $step)) (global.get $true))
                                      (then (global.get $void))
                                      (else (call $raise-argument-error (local.get $who) (R "real?") (local.get $step))
                                            (unreachable))))
                            (else (call $raise-argument-error (local.get $who) (R "real?") (local.get $b))
                                  (unreachable))))
                  (else (call $raise-argument-error (local.get $who) (R "real?") (local.get $a))
                        (unreachable))))

        (func $check-naturals (type $Prim1)
              (param $n (ref eq))   ;; n
              (result (ref eq))
              (if (ref.eq (call $integer? (local.get $n)) (global.get $true))
                  (then (if (ref.eq (call $exact? (local.get $n)) (global.get $true))
                            (then (if (ref.test (ref i31) (local.get $n))
                                      (then (if (i32.ge_s (i31.get_s (ref.cast (ref i31) (local.get $n))) (i32.const 0))
                                                (then (global.get $void))
                                                (else (call $raise-argument-error
                                                            (R 'in-naturals)
                                                            (R "exact-nonnegative-integer?")
                                                            (local.get $n))
                                                      (unreachable))))
                                      (else (call $raise-argument-error
                                                  (R 'in-naturals)
                                                  (R "exact-nonnegative-integer?")
                                                  (local.get $n))
                                            (unreachable))))
                            (else (call $raise-argument-error
                                        (R 'in-naturals)
                                        (R "exact-nonnegative-integer?")
                                        (local.get $n))
                                  (unreachable))))
                  (else (call $raise-argument-error
                              (R 'in-naturals)
                              (R "exact-nonnegative-integer?")
                              (local.get $n))
                        (unreachable))))

        (func $raise-expected-fixnum (param $x (ref eq)) (unreachable))
