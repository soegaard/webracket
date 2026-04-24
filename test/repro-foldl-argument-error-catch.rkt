(with-handlers ([(lambda (x) #t)
                 (lambda (ex) #t)])
  (foldl 'list 0 10)
  #f)
