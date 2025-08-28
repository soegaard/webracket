(list
 (list "callback"
       (let* ([p   (lambda (a b) (+ a b))]
              [id  (callback-register p)]
              [bs  (s-exp->fasl (vector 1 2) #f)]
              [_   (copy-bytes-to-memory bs 0)]
              [_len (callback id 0)]
              [res (linear-memory->value 0)])
         (equal? res 3))))

