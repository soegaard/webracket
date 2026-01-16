(list
 (list "fx*"
       (equal? (fx* 2 3) 6)
       (equal? (fx* -2 3 4) -24)
       (equal? (fx* 1) 1)
       (equal? (fx* 1 2 3 4) 24))
 (list "fx* overflow"
       (with-handlers ([exn:fail? (lambda (e)
                                    (string=? (exn-message e)
                                              "fx*: fixnum overflow with arguments 1073741824 and 1073741824"))])
         (fx* (expt 2 30) (expt 2 30))
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
