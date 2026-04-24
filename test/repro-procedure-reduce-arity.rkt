(list
 (procedure? (procedure-reduce-arity values 1))
 (equal? (procedure-arity (procedure-reduce-arity values 1)) 1)
 (equal? ((procedure-reduce-arity (lambda xs xs) 2) 'a 'b) '(a b))
 (with-handlers ([exn? (lambda (ex) #t)])
   ((procedure-reduce-arity (lambda xs xs) 2) 'a)))
