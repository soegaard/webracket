
    ;; Symbols for sequence checkers
    (add-runtime-symbol-constant 'in-list)
    (add-runtime-symbol-constant 'in-mlist)
    (add-runtime-symbol-constant 'in-range)
    (add-runtime-symbol-constant 'in-naturals)

    ;; Strings for sequence checkers
    (add-runtime-string-constant 'list-pred "list?")
    (add-runtime-string-constant 'mpair-or-null "(or/c mpair? null?)")
    (add-runtime-string-constant 'real-pred "real?")
    (add-runtime-string-constant 'exact-nonnegative-integer-pred
                                 "exact-nonnegative-integer?")

        (func $always-throw (type $Prim0)
              (throw $exn ,(Imm 42)))

        (func $raise-argument-error (type $Prim3)
              (param $who (ref eq)) (param $expected (ref eq)) (param $val (ref eq))
              (drop (call $js-log (local.get $who)))
              (drop (call $js-log (local.get $expected)))
              (drop (call $js-log (local.get $val)))
              (unreachable))


        ;;;
        ;;; Checkers
        ;;;

        (func $check-list (type $Prim1) (param $l (ref eq)) (result (ref eq))
              (if (result (ref eq))
                  (ref.eq (call $list? (local.get $l)) (global.get $false))
                  (then (call $raise-argument-error
                              (global.get $symbol:in-list)
                              (global.get $string:list-pred)
                              (local.get $l)))
                  (else (global.get $void))))

        (func $check-mlist (type $Prim1) (param $l (ref eq)) (result (ref eq))
              (if (result (ref eq))
                  (ref.eq (local.get $l) (global.get $null))
                  (then (global.get $void))
                  (else (call $raise-argument-error
                              (global.get $symbol:in-mlist)
                              (global.get $string:mpair-or-null)
                              (local.get $l)))))

        (func $check-range (type $Prim3)
              (param $a (ref eq)) (param $b (ref eq)) (param $step (ref eq))
              (result (ref eq))
              (call $check-range-generic
                    (global.get $symbol:in-range)
                    (local.get $a)
                    (local.get $b)
                    (local.get $step)))

        (func $check-range-generic (type $Prim4)
              (param $who (ref eq)) (param $a (ref eq)) (param $b (ref eq)) (param $step (ref eq))
              (result (ref eq))
              (if (result (ref eq))
                  (ref.eq (call $real? (local.get $a)) (global.get $false))
                  (then (call $raise-argument-error
                              (local.get $who)
                              (global.get $string:real-pred)
                              (local.get $a)))
                  (else
                   (if (result (ref eq))
                       (ref.eq (call $real? (local.get $b)) (global.get $false))
                       (then (call $raise-argument-error
                                   (local.get $who)
                                   (global.get $string:real-pred)
                                   (local.get $b)))
                       (else
                        (if (result (ref eq))
                            (ref.eq (call $real? (local.get $step)) (global.get $false))
                            (then (call $raise-argument-error
                                        (local.get $who)
                                        (global.get $string:real-pred)
                                        (local.get $step)))
                            (else (global.get $void)))))))

        (func $check-naturals (type $Prim1) (param $n (ref eq)) (result (ref eq))
              (if (result (ref eq))
                  (ref.eq (call $integer? (local.get $n)) (global.get $false))
                  (then (call $raise-argument-error
                              (global.get $symbol:in-naturals)
                              (global.get $string:exact-nonnegative-integer-pred)
                              (local.get $n)))
                  (else
                   (if (result (ref eq))
                       (ref.eq (call $exact? (local.get $n)) (global.get $false))
                       (then (call $raise-argument-error
                                   (global.get $symbol:in-naturals)
                                   (global.get $string:exact-nonnegative-integer-pred)
                                   (local.get $n)))
                       (else
                        (if (result (ref eq))
                            (ref.eq (call $>= (local.get $n) (global.get $zero)) (global.get $false))
                            (then (call $raise-argument-error
                                        (global.get $symbol:in-naturals)
                                        (global.get $string:exact-nonnegative-integer-pred)
                                        (local.get $n)))
                            (else (global.get $void)))))))


        ;;;
        ;;;
        ;;; DATATYPES
