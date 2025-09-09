         ;; Placeholder for equal-always? until a dedicated implementation
         (func $equal-always? (type $Prim2)
               (param $v1 (ref eq))
               (param $v2 (ref eq))
               (result    (ref eq))
               (return_call $equal? (local.get $v1) (local.get $v2)))

                          (br $rev)))
              (unreachable))
       
        ;; remq, remv, and remw implemented via remove
        ,@(let ([ops '((remq eq?)
                       (remv eqv?)
                       (remw equal-always?))])
            (for/list ([p ops])
              (define name (car p))
              (define cmp  (cadr p))
              `(func ,(string->symbol (format "$~a" name)) (type $Prim2)
                     (param $v   (ref eq))
                     (param $lst (ref eq))
                     (result     (ref eq))
                     (return_call $remove
                                  (local.get $v)
                                  (local.get $lst)
                                  (global.get ,(string->symbol (format "$~a" cmp)))))))
