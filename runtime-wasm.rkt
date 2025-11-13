          prop:equal+hash
          equal+hash
         (func $equal+hash-recur/equal (type $ClosureCode)
               (param $clos (ref $Closure))
               (param $args (ref $Args))
               (result (ref eq))

               (local $argc i32)
               (local $a    (ref eq))
               (local $b    (ref eq))

               (local.set $argc (array.len (local.get $args)))
               (if (i32.ne (local.get $argc) (i32.const 2))
                   (then (call $raise-arity-mismatch)
                         (unreachable)))

               (local.set $a (array.get $Args (local.get $args) (i32.const 0)))
               (local.set $b (array.get $Args (local.get $args) (i32.const 1)))
               (call $equal? (local.get $a) (local.get $b)))

         (func $equal+hash-recur/equal-always (type $ClosureCode)
               (param $clos (ref $Closure))
               (param $args (ref $Args))
               (result (ref eq))

               (local $argc i32)
               (local $a    (ref eq))
               (local $b    (ref eq))

               (local.set $argc (array.len (local.get $args)))
               (if (i32.ne (local.get $argc) (i32.const 2))
                   (then (call $raise-arity-mismatch)
                         (unreachable)))

               (local.set $a (array.get $Args (local.get $args) (i32.const 0)))
               (local.set $b (array.get $Args (local.get $args) (i32.const 1)))
               (call $equal-always? (local.get $a) (local.get $b)))

         (func $equal+hash-recur/hash (type $ClosureCode)
               (param $clos (ref $Closure))
               (param $args (ref $Args))
               (result (ref eq))

               (local $argc i32)
               (local $v    (ref eq))

               (local.set $argc (array.len (local.get $args)))
               (if (i32.ne (local.get $argc) (i32.const 1))
                   (then (call $raise-arity-mismatch)
                         (unreachable)))

               (local.set $v (array.get $Args (local.get $args) (i32.const 0)))
               (call $equal-hash-code (local.get $v)))

         (func $struct-equal+hash-apply
               (param $info  (ref $Array))
               (param $self  (ref eq))
               (param $other (ref eq))
               (param $mode  i32)
               (result (ref eq))

               (local $tag       (ref i31))
               (local $variant   i32)
               (local $proc      (ref $Procedure))
               (local $inv       (ref $ProcedureInvoker))
               (local $args      (ref $Args))
               (local $recur     (ref eq))
               (local $mode-val  (ref eq))

               (local.set $tag (ref.cast (ref i31)
                                         (array.get $Array (local.get $info) (i32.const 0))))
               (local.set $variant (i32.shr_u (i31.get_u (local.get $tag)) (i32.const 1)))

               (if (i32.eq (local.get $mode) (i32.const 0))
                   (then (local.set $recur    (global.get $equal+hash-recur/equal))
                         (local.set $mode-val (global.get $false)))
                   (else (local.set $recur    (global.get $equal+hash-recur/equal-always))
                         (local.set $mode-val (global.get $true))))

               (if (i32.eq (local.get $variant) (i32.const 3))
                   (then
                    (local.set $proc
                               (ref.cast (ref $Procedure)
                                         (array.get $Array (local.get $info) (i32.const 1))))
                    (local.set $inv (struct.get $Procedure $invoke (local.get $proc)))
                    (local.set $args (array.new $Args (global.get $null) (i32.const 3)))
                    (array.set $Args (local.get $args) (i32.const 0) (local.get $self))
                    (array.set $Args (local.get $args) (i32.const 1) (local.get $other))
                    (array.set $Args (local.get $args) (i32.const 2) (local.get $recur))
                    (return_call_ref $ProcedureInvoker
                                     (local.get $proc)
                                     (local.get $args)
                                     (local.get $inv))))

               (local.set $proc
                          (ref.cast (ref $Procedure)
                                    (array.get $Array (local.get $info) (i32.const 1))))
               (local.set $inv (struct.get $Procedure $invoke (local.get $proc)))
               (local.set $args (array.new $Args (global.get $null) (i32.const 4)))
               (array.set $Args (local.get $args) (i32.const 0) (local.get $self))
               (array.set $Args (local.get $args) (i32.const 1) (local.get $other))
               (array.set $Args (local.get $args) (i32.const 2) (local.get $recur))
               (array.set $Args (local.get $args) (i32.const 3) (local.get $mode-val))
               (return_call_ref $ProcedureInvoker
                                (local.get $proc)
                                (local.get $args)
                                (local.get $inv)))

         (func $struct-equal+hash-hash
               (param $info (ref $Array))
               (param $self (ref $Struct))
               (param $mode i32)
               (result i32)

               (local $tag      (ref i31))
               (local $variant  i32)
               (local $proc     (ref $Procedure))
               (local $inv      (ref $ProcedureInvoker))
               (local $args     (ref $Args))
               (local $mode-val (ref eq))
               (local $result   (ref eq))
               (local $hash     (ref i31))
               (local $value    i32)

               (local.set $tag (ref.cast (ref i31)
                                         (array.get $Array (local.get $info) (i32.const 0))))
               (local.set $variant (i32.shr_u (i31.get_u (local.get $tag)) (i32.const 1)))

               (if (i32.eq (local.get $mode) (i32.const 0))
                   (then (local.set $mode-val (global.get $false)))
                   (else (local.set $mode-val (global.get $true))))

               (if (i32.eq (local.get $variant) (i32.const 3))
                   (then
                    (local.set $proc
                               (ref.cast (ref $Procedure)
                                         (array.get $Array (local.get $info) (i32.const 2))))
                    (local.set $inv (struct.get $Procedure $invoke (local.get $proc)))
                    (local.set $args (array.new $Args (global.get $null) (i32.const 2)))
                    (array.set $Args (local.get $args) (i32.const 0)
                               (ref.cast (ref eq) (local.get $self)))
                    (array.set $Args (local.get $args) (i32.const 1)
                               (global.get $equal+hash-recur/hash))
                    (local.set $result
                               (call_ref $ProcedureInvoker
                                         (local.get $proc)
                                         (local.get $args)
                                         (local.get $inv))))
                   (else
                    (local.set $proc
                               (ref.cast (ref $Procedure)
                                         (array.get $Array (local.get $info) (i32.const 2))))
                    (local.set $inv (struct.get $Procedure $invoke (local.get $proc)))
                    (local.set $args (array.new $Args (global.get $null) (i32.const 3)))
                    (array.set $Args (local.get $args) (i32.const 0)
                               (ref.cast (ref eq) (local.get $self)))
                    (array.set $Args (local.get $args) (i32.const 1)
                               (global.get $equal+hash-recur/hash))
                    (array.set $Args (local.get $args) (i32.const 2) (local.get $mode-val))
                    (local.set $result
                               (call_ref $ProcedureInvoker
                                         (local.get $proc)
                                         (local.get $args)
                                         (local.get $inv)))))

               (local.set $hash (ref.cast (ref i31) (local.get $result)))
               (local.set $value (i32.shr_s (i31.get_s (local.get $hash)) (i32.const 1)))
               (local.get $value))


         ;; Compare structs using prop:equal+hash when available
               (local $t1        (ref $StructType))
               (local $t2        (ref $StructType))
               (local $a1        (ref $Array))
               (local $a2        (ref $Array))
               (local $len       i32)
               (local $i         i32)
               (local $x1        (ref eq))
               (local $x2        (ref eq))
               (local $prop-name (ref $Symbol))
               (local $sentinel  (ref eq))
               (local $prop-val  (ref eq))
               (local $prop-info (ref $Array))

               (local.set $prop-name (ref.cast (ref $Symbol)
                                                (global.get $symbol:prop:equal+hash)))
               (local.set $sentinel (call $cons (global.get $false) (global.get $false)))

               (local.set $prop-val
                          (call $struct-type-property-lookup-by-name
                                (local.get $t1)
                                (local.get $prop-name)
                                (local.get $sentinel)))
               (if (i32.eqz (ref.eq (local.get $prop-val) (local.get $sentinel)))
                   (then
                    (local.set $prop-info (ref.cast (ref $Array) (local.get $prop-val)))
                    (return (call $struct-equal+hash-apply
                                   (local.get $prop-info)
                                   (ref.cast (ref eq) (local.get $s1))
                                   (ref.cast (ref eq) (local.get $s2))
                                   (i32.const 0)))))

               (local.set $prop-val
                          (call $struct-type-property-lookup-by-name
                                (local.get $t2)
                                (local.get $prop-name)
                                (local.get $sentinel)))
               (if (i32.eqz (ref.eq (local.get $prop-val) (local.get $sentinel)))
                   (then
                    (local.set $prop-info (ref.cast (ref $Array) (local.get $prop-val)))
                    (return (call $struct-equal+hash-apply
                                   (local.get $prop-info)
                                   (ref.cast (ref eq) (local.get $s2))
                                   (ref.cast (ref eq) (local.get $s1))
                                   (i32.const 0)))))

                                 (if (ref.eq (call $equal? (local.get $x1) (local.get $x2))
                                             (global.get $false))

               (global.get $false))

         (func $equal-always?/struct
               (param $s1 (ref $Struct))
               (param $s2 (ref $Struct))
               (result    (ref eq))

               (local $t1        (ref $StructType))
               (local $t2        (ref $StructType))
               (local $prop-name (ref $Symbol))
               (local $sentinel  (ref eq))
               (local $prop-val  (ref eq))
               (local $prop-info (ref $Array))

               (local.set $t1 (struct.get $Struct $type (local.get $s1)))
               (local.set $t2 (struct.get $Struct $type (local.get $s2)))

               (local.set $prop-name (ref.cast (ref $Symbol)
                                                (global.get $symbol:prop:equal+hash)))
               (local.set $sentinel (call $cons (global.get $false) (global.get $false)))

               (local.set $prop-val
                          (call $struct-type-property-lookup-by-name
                                (local.get $t1)
                                (local.get $prop-name)
                                (local.get $sentinel)))
               (if (i32.eqz (ref.eq (local.get $prop-val) (local.get $sentinel)))
                   (then
                    (local.set $prop-info (ref.cast (ref $Array) (local.get $prop-val)))
                    (return (call $struct-equal+hash-apply
                                   (local.get $prop-info)
                                   (ref.cast (ref eq) (local.get $s1))
                                   (ref.cast (ref eq) (local.get $s2))
                                   (i32.const 1)))))

               (local.set $prop-val
                          (call $struct-type-property-lookup-by-name
                                (local.get $t2)
                                (local.get $prop-name)
                                (local.get $sentinel)))
               (if (i32.eqz (ref.eq (local.get $prop-val) (local.get $sentinel)))
                   (then
                    (local.set $prop-info (ref.cast (ref $Array) (local.get $prop-val)))
                    (return (call $struct-equal+hash-apply
                                   (local.get $prop-info)
                                   (ref.cast (ref eq) (local.get $s2))
                                   (ref.cast (ref eq) (local.get $s1))
                                   (i32.const 1)))))

               (global.get $false))
                (if (i32.and (ref.test (ref $Vector) (local.get $v1))
                             (ref.test (ref $Vector) (local.get $v2)))
                    (then (return_call $equal-always?/vector
                                       (ref.cast (ref $Vector) (local.get $v1))
                                       (ref.cast (ref $Vector) (local.get $v2)))))
                ;; --- Struct ---
                (if (i32.and (ref.test (ref $Struct) (local.get $v1))
                             (ref.test (ref $Struct) (local.get $v2)))
                    (then (return_call $equal-always?/struct
                                       (ref.cast (ref $Struct) (local.get $v1))
                                       (ref.cast (ref $Struct) (local.get $v2)))))
                ;; --- String ---
                (if (i32.and (ref.test (ref $String) (local.get $v1))
                             (ref.test (ref $String) (local.get $v2)))
        ;; equal-hash-code -- computes a hash code consistent with equal?
        ;;   Supports struct customization via prop:equal+hash.
        ;;   Note: generic interfaces gen:equal+hash and gen:equal-mode+hash
        ;;         remain unsupported.
               (local $prop-name (ref $Symbol))
               (local $prop-sentinel (ref eq))
               (local $prop-val (ref eq))
               (local $prop-info (ref $Array))

               (local.set $prop-name (ref.cast (ref $Symbol)
                                                (global.get $symbol:prop:equal+hash)))
               (local.set $prop-sentinel (call $cons (global.get $false) (global.get $false)))
               (local.set $prop-val
                          (call $struct-type-property-lookup-by-name
                                (local.get $type)
                                (local.get $prop-name)
                                (local.get $prop-sentinel)))
               (if (i32.eqz (ref.eq (local.get $prop-val) (local.get $prop-sentinel)))
                   (then
                    (local.set $prop-info (ref.cast (ref $Array) (local.get $prop-val)))
                    (local.set $h (call $struct-equal+hash-hash
                                        (local.get $prop-info)
                                        (local.get $s)
                                        (i32.const 0)))
                    (struct.set $Heap $hash (local.get $heap) (i32.const 0))
                    (return (local.get $h))))

              (ref.func $equal+hash-recur/equal)            ; closure body
              (ref.func $equal+hash-recur/equal-always)     ; closure body
              (ref.func $equal+hash-recur/hash)             ; closure body
               (local $equal+hash-array (ref $Array))
               (local $equal+hash-info (ref $Array))
               (local $equal+hash-count i32)
               (local $equal+hash-index i32)
               (local $equal+hash-proc (ref eq))
               (if (i32.eq (call $symbol=?/i32
                                       (struct.get $StructTypeProperty $name (local.get $prop))
                                       (global.get $symbol:prop:equal+hash))
                                (i32.const 1))
                   (then
                    (if (i32.eqz (ref.test (ref $Array) (local.get $processed)))
                        (then
                         (if (i32.eqz (ref.test (ref $Pair) (local.get $processed)))
                             (then (call $raise-argument-error (local.get $value))
                                   (unreachable)))
                         (local.set $equal+hash-array
                                    (ref.cast (ref $Array)
                                              (call $list->array (local.get $processed))))
                         (local.set $equal+hash-count (array.len (local.get $equal+hash-array)))
                         (if (i32.or (i32.lt_s (local.get $equal+hash-count) (i32.const 2))
                                     (i32.gt_s (local.get $equal+hash-count) (i32.const 3)))
                             (then (call $raise-argument-error (local.get $value))
                                   (unreachable)))
                         (local.set $equal+hash-index (i32.const 0))
                         (block $done
                                (loop $loop
                                      (br_if $done (i32.ge_u (local.get $equal+hash-index)
                                                             (local.get $equal+hash-count)))
                                      (local.set $equal+hash-proc
                                                 (array.get $Array
                                                            (local.get $equal+hash-array)
                                                            (local.get $equal+hash-index)))
                                      (if (i32.eqz (ref.test (ref $Procedure)
                                                             (local.get $equal+hash-proc)))
                                          (then (call $raise-argument-error (local.get $value))
                                                (unreachable)))
                                      (local.set $equal+hash-index
                                                 (i32.add (local.get $equal+hash-index)
                                                          (i32.const 1)))
                                      (br $loop)))
                         (local.set $equal+hash-info
                                    (array.new_fixed $Array 4
                                                     (ref.i31 (i32.shl (local.get $equal+hash-count)
                                                                        (i32.const 1)))
                                                     (array.get $Array (local.get $equal+hash-array) (i32.const 0))
                                                     (array.get $Array (local.get $equal+hash-array) (i32.const 1))
                                                     (if (i32.eq (local.get $equal+hash-count) (i32.const 3))
                                                         (array.get $Array (local.get $equal+hash-array) (i32.const 2))
                                                         (global.get $false))))
                         (local.set $processed
                                    (ref.cast (ref eq) (local.get $equal+hash-info))))
                        (else
                         (local.set $equal+hash-info
                                    (ref.cast (ref $Array) (local.get $processed)))
                         (local.set $equal+hash-count
                                    (i32.shr_u
                                     (i31.get_u (ref.cast (ref i31)
                                                          (array.get $Array (local.get $equal+hash-info)
                                                                      (i32.const 0))))
                                     (i32.const 1)))
                         (if (i32.or (i32.lt_s (local.get $equal+hash-count) (i32.const 2))
                                     (i32.gt_s (local.get $equal+hash-count) (i32.const 3)))
                             (then (call $raise-argument-error (local.get $value))
                                   (unreachable))))))
               (global $prop:equal+hash          (mut (ref eq)) (global.get $void))
               (global $equal+hash-recur/equal        (mut (ref eq)) (global.get $void))
               (global $equal+hash-recur/equal-always (mut (ref eq)) (global.get $void))
               (global $equal+hash-recur/hash         (mut (ref eq)) (global.get $void))
                     (global.set $prop:equal+hash
                                 (ref.cast (ref eq)
                                           (call $make-struct-type-property-descriptor/checked
                                                 (ref.cast (ref $Symbol)
                                                           (global.get $symbol:prop:equal+hash))
                                                 (global.get $false)
                                                 (global.get $null)
                                                 (global.get $false)
                                                 (global.get $false))))

                     (global.set $equal+hash-recur/equal
                                 (struct.new $Closure
                                             (i32.const 0)
                                             (global.get $false)
                                             (ref.i31 (i32.const 4))
                                             (global.get $false)
                                             (ref.func $invoke-closure)
                                             (ref.func $equal+hash-recur/equal)
                                             (array.new_fixed $Free 0)))
                     (global.set $equal+hash-recur/equal-always
                                 (struct.new $Closure
                                             (i32.const 0)
                                             (global.get $false)
                                             (ref.i31 (i32.const 4))
                                             (global.get $false)
                                             (ref.func $invoke-closure)
                                             (ref.func $equal+hash-recur/equal-always)
                                             (array.new_fixed $Free 0)))
                     (global.set $equal+hash-recur/hash
                                 (struct.new $Closure
                                             (i32.const 0)
                                             (global.get $false)
                                             (ref.i31 (i32.const 2))
                                             (global.get $false)
                                             (ref.func $invoke-closure)
                                             (ref.func $equal+hash-recur/hash)
                                             (array.new_fixed $Free 0)))
