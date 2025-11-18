(list
 (list "hash-iterate on empty hash"
       (eq? (hash-iterate-first (hash)) #f))

 (list "hash-iterate traversal"
       (let* ([h          (hash 'a 1 'b 2)]
              [first-iter (hash-iterate-first h)])
         (and first-iter
              (let loop ([iter first-iter]
                         [acc '()])
                (if iter
                    (loop (hash-iterate-next h iter)
                          (cons (cons (hash-iterate-key h iter)
                                      (hash-iterate-value h iter))
                                acc))
                    (let* ([pa (assq 'a acc)]
                           [pb (assq 'b acc)]
                           [pair (hash-iterate-pair h first-iter)]
                           [next-iter (hash-iterate-next h first-iter)]
                           [kv (call-with-values (lambda () (hash-iterate-key+value h first-iter))
                                                 list)])
                      (and pa pb
                           (= (cdr pa) 1)
                           (= (cdr pb) 2)
                           (pair? pair)
                           (eq? (car pair) (hash-iterate-key h first-iter))
                           (= (cdr pair) (hash-iterate-value h first-iter))
                           (= (length kv) 2)
                           (eq? (car kv) (hash-iterate-key h first-iter))
                           (= (cadr kv) (hash-iterate-value h first-iter))
                           (or (eq? next-iter #f)
                               (eq? (hash-iterate-next h next-iter) #f))))))))))
