(list
 (list "linear-memory->string"
       (let* ([bs (s-exp->fasl "hello" #f)]
              [_  (copy-bytes-to-memory bs 0)])
         (equal? (linear-memory->string 0) "hello")))
 (list "linear-memory->value"
       (let* ([v  '(1 . 2)]
              [bs (s-exp->fasl v #f)]
              [_  (copy-bytes-to-memory bs 0)])
         (equal? (linear-memory->value 0) v)))
 (list "linear-memory->string-negative"
       (with-handlers ([exn:fail? (lambda (_) #t)])
         (let* ([bs (s-exp->fasl 42 #f)]
                [_  (copy-bytes-to-memory bs 0)])
           (linear-memory->string 0)
           #f))))
