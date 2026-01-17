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
            (equal? (fx- 10 3) 7)))

 (list "fx+ arguments are checked"
       (with-handlers ([(λ (_) #t) (lambda (e)
                                     (string=? (exn-message e)
                                               "fx+expected fixnum, got: 1.073741e9"))])
         (fx+ (expt 2 30) 1))
       (with-handlers ([(λ (_) #t) (lambda (e)
                                     (string=? (exn-message e)
                                               "fx+expected fixnum, got: 1.073741e9"))])
         (fx+ 1 (expt 2 30))))

 (list "fx- arguments are checked"
       (with-handlers ([(λ (_) #t) (lambda (e)
                                     (string=? (exn-message e)
                                               "fx-expected fixnum, got: 1.073741e9"))])
         (fx- (expt 2 30) 1))
       (with-handlers ([(λ (_) #t) (lambda (e)
                                     (string=? (exn-message e)
                                               "fx-expected fixnum, got: 1.073741e9"))])
         (fx- 1 (expt 2 30))))

 (list "fx/ arguments are checked"
       (with-handlers ([(λ (_) #t) (lambda (e)
                                     (string=? (exn-message e)
                                               "fx/expected fixnum, got: 1.073741e9"))])
         (fx/ (expt 2 30) 1))
       (with-handlers ([(λ (_) #t) (lambda (e)
                                     (string=? (exn-message e)
                                               "fx/expected fixnum, got: 1.073741e9"))])
         (fx/ 1 (expt 2 30))))

 (list "fx+ overflow"
       (with-handlers ([exn:fail? (lambda (e)
                                    (string=? (exn-message e)
                                              "fx+: fixnum overflow with arguments 536870911 and 1"))])
         (fx+ (sub1 (expt 2 29)) 1)
         #f))

 (list "fx- overflow"
       (with-handlers ([exn:fail? (lambda (e)
                                    (string=? (exn-message e)
                                              "fx-: fixnum overflow with arguments -536870912 and 1"))])
         (fx- (- (expt 2 29)) 1)
         #f))

 (list "fx/ overflow"
       (with-handlers ([exn:fail? (lambda (e)
                                    (string=? (exn-message e)
                                              "fx/: fixnum overflow with arguments -536870912 and -1"))])
         (fx/ (- (expt 2 29)) -1)
         #f))

 (list "fx/ division by zero"
       (with-handlers ([exn:fail? (lambda (_) #t)])
         (fx/ 1 0)
         #f))

 (list "fx+ boundaries"
       (let* ([a (sub1 (expt 2 29))]
              [sum (fx+ a 0)])
         (and (fixnum? sum)
              (equal? sum a)))
       (let* ([a (- (expt 2 29))]
              [sum (fx+ a 0)])
         (and (fixnum? sum)
              (equal? sum a))))

 (list "fx- boundaries"
       (let* ([a (sub1 (expt 2 29))]
              [diff (fx- a 0)])
         (and (fixnum? diff)
              (equal? diff a)))
       (let* ([a (- (expt 2 29))]
              [diff (fx- a 0)])
         (and (fixnum? diff)
              (equal? diff a))))

 (list "fx/ boundaries"
       (let* ([a (sub1 (expt 2 29))]
              [q (fx/ a 1)])
         (and (fixnum? q)
              (equal? q a)))
       (let* ([a (- (expt 2 29))]
              [q (fx/ a 1)])
         (and (fixnum? q)
              (equal? q a)))))
