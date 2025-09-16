    (define primitive-variadic-args
      '(bytes
        string
        vector
        vector-immutable
        values
        void))

    (define primitive-shapes '(0 1 2 3 4 5 6 7 8 9 10 11 12 13))
      (and desc
           (let ([base (arity->shape (primitive-description-arity desc))])
             (and base
                  (if (>= base 6)
                      (if (memq pr primitive-variadic-args)
                          (+ base 4)
                          base)
                      base)))))
        [10 'primitive-invoke:shape-10]
        [11 'primitive-invoke:shape-11]
        [12 'primitive-invoke:shape-12]
        [13 'primitive-invoke:shape-13]
        [6 ; at least 0, rest arguments as list
         `((if (ref.test (ref $Prim>=0) (local.get $code))
               (then (local.set $rest
                                (call $rest-arguments->list
                                      (local.get $args)
                                      (i32.const 0)))
                     (return_call_ref $Prim>=0
                                      (local.get $rest)
                                      (ref.cast (ref $Prim>=0) (local.get $code))))
               (else (return (call $raise-code-type-mismatch (local.get $pproc)))))
           (unreachable))]
        [7 ; at least 1, rest arguments as list
         `((if (i32.ge_u (local.get $argc) (i32.const 1))
               (then (if (ref.test (ref $Prim>=1) (local.get $code))
                         (then (local.set $rest
                                          (call $rest-arguments->list
                                                (local.get $args)
                                                (i32.const 1)))
                               (return_call_ref $Prim>=1
                                                (local.get $a0)
                                                (local.get $rest)
                                                (ref.cast (ref $Prim>=1) (local.get $code))))
                         (else (return (call $raise-code-type-mismatch (local.get $pproc))))))
               (else (return (call $primitive-invoke:raise-arity-error
                                   (local.get $pproc) (local.get $argc)))))
           (unreachable))]
        [8 ; at least 2, rest arguments as list
         `((if (i32.ge_u (local.get $argc) (i32.const 2))
               (then (if (ref.test (ref $Prim>=2) (local.get $code))
                         (then (local.set $rest
                                          (call $rest-arguments->list
                                                (local.get $args)
                                                (i32.const 2)))
                               (return_call_ref $Prim>=2
                                                (local.get $a0)
                                                (local.get $a1)
                                                (local.get $rest)
                                                (ref.cast (ref $Prim>=2) (local.get $code))))
                         (else (return (call $raise-code-type-mismatch (local.get $pproc))))))
               (else (return (call $primitive-invoke:raise-arity-error
                                   (local.get $pproc) (local.get $argc)))))
           (unreachable))]
        [9 ; at least 3, rest arguments as list
         `((if (i32.ge_u (local.get $argc) (i32.const 3))
               (then
                (if (ref.test (ref $Prim>=3) (local.get $code))
                    (then (local.set $rest
                                     (call $rest-arguments->list
                                           (local.get $args)
                                           (i32.const 3)))
                          (return_call_ref $Prim>=3
                                           (local.get $a0)
                                           (local.get $a1)
                                           (local.get $a2)
                                           (local.get $rest)
                                           (ref.cast (ref $Prim>=3) (local.get $code))))
                    (else (return (call $raise-code-type-mismatch (local.get $pproc))))))
               (else (return (call $primitive-invoke:raise-arity-error
                                   (local.get $pproc) (local.get $argc)))))
           (unreachable))]
        [10 ; at least 0, rest arguments as $Args array
        [11 ; at least 1, rest arguments as $Args array
                                                (call $rest-arguments->args
                                                      (local.get $args)
                                                      (i32.const 1))
        [12 ; at least 2, rest arguments as $Args array
                                                (call $rest-arguments->args
                                                      (local.get $args)
                                                      (i32.const 2))
        [13 ; at least 3, rest arguments as $Args array
                                           (call $rest-arguments->args
                                                 (local.get $args)
                                                 (i32.const 3))
             (local $rest (ref eq))
             (local.set $rest (global.get $null))
                (block $L13
                  (block $L12
                    (block $L11
                      (block $L10
                        (block $L9
                          (block $L8
                            (block $L7
                              (block $L6
                                (block $L5
                                  (block $L4
                                    (block $L3
                                      (block $L2
                                        (block $L1
                                          (block $L0
                                            (br_table $L0 $L1 $L2 $L3 $L4 $L5 $L6 $L7 $L8 $L9 $L10 $L11 $L12 $L13 $default (local.get $shape))
                                          ) ;; end $L0
                                          ;; shape 0: exact 0
                                          #;(drop (call $js-log (call $i32->string (i32.const 0))))
                                          (if (i32.eqz (local.get $argc))
                                              (then
                                               (if (ref.test (ref $Prim0) (local.get $code))
                                                   (then
                                                    (return_call_ref $Prim0
                                                                     (ref.cast (ref $Prim0) (local.get $code))))
                                                   (else
                                                    (return (call $raise-code-type-mismatch (local.get $pproc)))))
                                              (else
                                               (return (call $primitive-invoke:raise-arity-error
                                                                 (local.get $pproc) (local.get $argc)))))
                                        )) ;; end $L1
                                        ;; shape 1: exact 1
                                        #;(drop (call $js-log (call $i32->string (i32.const 1))))
                                        (if (i32.eq (local.get $argc) (i32.const 1))
                                            (then
                                             (if (ref.test (ref $Prim1) (local.get $code))
                                                 (then
                                                  (return_call_ref $Prim1
                                                                   (local.get $a0)
                                                                   (ref.cast (ref $Prim1) (local.get $code))))
                                                 (else
                                                  (return (call $raise-code-type-mismatch (local.get $pproc)))))
                                            (else
                                             (return (call $primitive-invoke:raise-arity-error
                                                           (local.get $pproc) (local.get $argc)))))
                                      )) ;; end $L2
                                      ;; shape 2: exact 2
                                      #;(drop (call $js-log (call $i32->string (i32.const 2))))
                                      (if (i32.eq (local.get $argc) (i32.const 2))
                                          (then
                                           (if (ref.test (ref $Prim2) (local.get $code))
                                               (then
                                                (return_call_ref $Prim2
                                                                 (local.get $a0)
                                                                 (local.get $a1)
                                                                 (ref.cast (ref $Prim2) (local.get $code))))
                                               (else
                                                (return (call $raise-code-type-mismatch (local.get $pproc)))))
                                          (else
                                           (return (call $primitive-invoke:raise-arity-error
                                                         (local.get $pproc) (local.get $argc)))))
                                    )) ;; end $L3
                                    ;; shape 3: exact 3
                                    #;(drop (call $js-log (call $i32->string (i32.const 3))))
                                    (if (i32.eq (local.get $argc) (i32.const 3))
                                        (then
                                         (if (ref.test (ref $Prim3) (local.get $code))
                                             (then
                                              (return_call_ref $Prim3
                                                               (local.get $a0)
                                                               (local.get $a1)
                                                               (local.get $a2)
                                                               (ref.cast (ref $Prim3) (local.get $code))))
                                             (else
                                              (return (call $raise-code-type-mismatch (local.get $pproc)))))
                                        (else
                                         (return (call $primitive-invoke:raise-arity-error
                                                       (local.get $pproc) (local.get $argc)))))
                                  )) ;; end $L4
                                  ;; shape 4: exact 4
                                  #;(drop (call $js-log (call $i32->string (i32.const 4))))
                                  (if (i32.eq (local.get $argc) (i32.const 4))
                                       (if (ref.test (ref $Prim4) (local.get $code))
                                            (return_call_ref $Prim4
                                                             (local.get $a0)
                                                             (local.get $a1)
                                                             (local.get $a2)
                                                             (array.get $Args (local.get $args) (i32.const 3))
                                                             (ref.cast (ref $Prim4) (local.get $code))))
                                )) ;; end $L5
                                ;; shape 5: exact 5
                                #;(drop (call $js-log (call $i32->string (i32.const 5))))
                                (if (i32.eq (local.get $argc) (i32.const 5))
                                     (if (ref.test (ref $Prim5) (local.get $code))
                                          (return_call_ref $Prim5
                                                           (local.get $a1)
                                                           (local.get $a2)
                                                           (array.get $Args (local.get $args) (i32.const 3))
                                                           (array.get $Args (local.get $args) (i32.const 4))
                                                           (ref.cast (ref $Prim5) (local.get $code))))
                              )) ;; end $L6
                              ;; shape 6: at least 0 (rest list)
                              (if (ref.test (ref $Prim>=0) (local.get $code))
                                  (then (local.set $rest
                                                   (call $rest-arguments->list
                                                         (local.get $args)
                                                         (i32.const 0)))
                                        (return_call_ref $Prim>=0
                                                         (local.get $rest)
                                                         (ref.cast (ref $Prim>=0) (local.get $code))))
                                   (return (call $raise-code-type-mismatch (local.get $pproc)))))
                            )) ;; end $L7
                            ;; shape 7: at least 1 (rest list)
                            (if (i32.ge_u (local.get $argc) (i32.const 1))
                                 (if (ref.test (ref $Prim>=1) (local.get $code))
                                     (then (local.set $rest
                                                      (call $rest-arguments->list
                                                            (local.get $args)
                                                            (i32.const 1)))
                                           (return_call_ref $Prim>=1
                                                            (local.get $a0)
                                                            (local.get $rest)
                                                            (ref.cast (ref $Prim>=1) (local.get $code))))
                          )) ;; end $L8
                          ;; shape 8: at least 2 (rest list)
                          (if (i32.ge_u (local.get $argc) (i32.const 2))
                               (if (ref.test (ref $Prim>=2) (local.get $code))
                                   (then (local.set $rest
                                                    (call $rest-arguments->list
                                                          (local.get $args)
                                                          (i32.const 2)))
                                         (return_call_ref $Prim>=2
                                                          (local.get $a0)
                                                          (local.get $a1)
                                                          (local.get $rest)
                                                          (ref.cast (ref $Prim>=2) (local.get $code))))
                        )) ;; end $L9
                        ;; shape 9: at least 3 (rest list)
                        (if (i32.ge_u (local.get $argc) (i32.const 3))
                             (if (ref.test (ref $Prim>=3) (local.get $code))
                                 (then (local.set $rest
                                                  (call $rest-arguments->list
                                                        (local.get $args)
                                                        (i32.const 3)))
                                       (return_call_ref $Prim>=3
                                                        (local.get $a0)
                                                        (local.get $a1)
                                                        (local.get $a2)
                                                        (local.get $rest)
                                                        (ref.cast (ref $Prim>=3) (local.get $code))))
                      )) ;; end $L10
                      ;; shape 10: at least 0 (rest $Args)
                          (then (return_call_ref $Prim>=0
                          (else (return (call $raise-code-type-mismatch (local.get $pproc)))))
                    )) ;; end $L11
                    ;; shape 11: at least 1 (rest $Args)
                             (then (return_call_ref $Prim>=1
                                                    (local.get $a0)
                                                    (call $rest-arguments->args
                                                          (local.get $args)
                                                          (i32.const 1))
                                                    (ref.cast (ref $Prim>=1) (local.get $code))))
                             (else (return (call $raise-code-type-mismatch (local.get $pproc)))))
                  )) ;; end $L12
                  ;; shape 12: at least 2 (rest $Args)
                           (then (return_call_ref $Prim>=2
                                                  (local.get $a0)
                                                  (local.get $a1)
                                                  (call $rest-arguments->args
                                                        (local.get $args)
                                                        (i32.const 2))
                                                  (ref.cast (ref $Prim>=2) (local.get $code))))
                           (else (return (call $raise-code-type-mismatch (local.get $pproc)))))
                )) ;; end $L13
                ;; shape 13: at least 3 (rest $Args)
                         (then (return_call_ref $Prim>=3
                                                (local.get $a0)
                                                (local.get $a1)
                                                (local.get $a2)
                                                (call $rest-arguments->args
                                                      (local.get $args)
                                                      (i32.const 3))
                                                (ref.cast (ref $Prim>=3) (local.get $code))))
                         (else (return (call $raise-code-type-mismatch (local.get $pproc)))))
         (func $rest-arguments->args
               (param $args (ref $Args))
               (param $n    i32)
               (result      (ref $Args))

               (local $len   i32)
               (local $count i32)
               (local $res   (ref $Args))

               (local.set $len (array.len (local.get $args)))
               (if (i32.ge_u (local.get $n) (local.get $len))
                   (then (return (array.new $Args (global.get $null) (i32.const 0)))))
               (local.set $count (i32.sub (local.get $len) (local.get $n)))
               (local.set $res (array.new $Args (global.get $null) (local.get $count)))
               (array.copy $Args $Args
                           (local.get $res)
                           (i32.const 0)
                           (local.get $args)
                           (local.get $n)
                           (local.get $count))
               (local.get $res))

