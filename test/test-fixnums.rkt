(list
 (list "fx*"
       (equal? (fx* 2 3) 6)
       (equal? (fx* -2 3 4) -24)
       (equal? (fx* 1) 1)
       (equal? (fx* 1 2 3 4) 24))
 
 (list "arguments are checked"
       (with-handlers ([(λ (_) #t) (lambda (e)
                                     (string=? (exn-message e)
                                               "fx*expected fixnum, got: 1.073741e9"))])
         (fx* (expt 2 30) 1))
       
       (with-handlers ([(λ (_) #t) (lambda (e)
                                     (string=? (exn-message e)
                                               "fx*expected fixnum, got: 1.073741e9"))])
         (fx* 1 (expt 2 30))))
 
 (list "fx* overflow"
       (with-handlers ([exn:fail? (lambda (e)
                                    (string=? (exn-message e)
                                              "fx*: fixnum overflow with arguments -536870912 and -536870912"))])
         (fx* (expt 2 29) (expt 2 29))
         #f))
 
 (list "fx* boundaries"
       (let* ([a (sub1 (expt 2 14))]
              [b (sub1 (expt 2 15))]
              [prod (fx* a b)])
         (and (fixnum? prod)
              (equal? prod (* a b))))
       (let* ([a (- (sub1 (expt 2 14)))]
              [b (sub1 (expt 2 15))]
              [prod (fx* a b)])
         (and (fixnum? prod)
              (equal? prod (* a b)))))
 (list "fx+ fx-"
       (and (equal? (fx+ 10 20) 30)
            (equal? (fx- 10 3) 7))))
