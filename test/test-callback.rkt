(list
 (list "callback"
       (let* ([callback-buffer-base 1052672]
              [p   (lambda (a b) (+ a b))]
              [id  (callback-register p)]
              [bs  (s-exp->fasl (vector 1 2) #f)]
              [_   (copy-bytes-to-memory bs callback-buffer-base)]
              [_len (callback id callback-buffer-base)]
              [res (linear-memory->value callback-buffer-base)])
         (equal? res 3))))
