(list
 (list "error"
       (with-handlers ([exn:fail? (λ (e)
                                   (string=? (exn-message e)
                                             "error: problem"))])
         (error 'problem)
         #f)
       (with-handlers ([exn:fail? (λ (e)
                                   (string-prefix? (exn-message e)
                                                   "Numbers"))])
         (error "Numbers" 1 2)
         #f))
 (list "raise-argument-error"
       (with-handlers ([exn:fail:contract? (λ (e)
                                             (string=?
                                              (exn-message e)
                                              "add1: contract violation\n  expected: exact-integer?\n  given: #f"))])
         (raise-argument-error 'add1 "exact-integer?" #f)
         #f)
       (with-handlers ([exn:fail:contract? (λ (e)
                                             (string=?
                                              (exn-message e)
                                              "map: contract violation\n  expected: list?\n  given: 42\n  argument position: 1st"))])
         (raise-argument-error 'map "list?" 0 42)
         #f))
 (list "raise-argument-error*"
       (with-handlers ([exn:fail:contract? (λ (e)
                                             (string=?
                                              (exn-message e)
                                              "map: contract violation\n  expected: list?\n  given: 42\n  argument position: 1st"))])
         (raise-argument-error* 'map "list?" 0 42)
         #f)
       (with-handlers ([exn:fail:contract? (λ (e)
                                             (string=?
                                              (exn-message e)
                                              "map: contract violation\n  expected: list?\n  given: 42\n  argument position: 1st"))])
         (raise-argument-error* 'map "list?" 0 1 42)
         #f))
 (list "error-message->adjusted-string"
       (string=? (error-message->adjusted-string "add1" 'realm "problem" 'other)
                 "add1: problem")
       (string=? (error-message->adjusted-string #f 'realm "problem" 'other)
                 "problem"))

 (list "raise-arguments-error"
       (with-handlers ([exn:fail:contract? (λ (e)
                                             (and (string-contains? (exn-message e)
                                                                    "expected at least one argument")
                                                  (string-contains? (exn-message e)
                                                                    "first argument")))])
         (raise-arguments-error 'build-list "expected at least one argument"
                                "first argument" 42)
         #f)
       
       (with-handlers ([exn:fail:contract? (λ (e)
                                             (and (string-contains? (exn-message e)
                                                                    "expected at least one argument")
                                                  (string-contains? (exn-message e)
                                                                    "second argument")))])
         (raise-arguments-error 'build-list "expected at least one argument"
                                "first argument" 42
                                "second argument" '(1 2 3))
         #f))

 (list "raise-arguments-error*"
       (with-handlers ([exn:fail:contract? (λ (e)
                                             (and (string-contains? (exn-message e)
                                                                    "must provide a list")
                                                  (string-contains? (exn-message e)
                                                                    "data")))])
         (raise-arguments-error* 'map 'custom-realm "must provide a list"
                                 "data" 'bad)
         #f)
       (with-handlers ([exn:fail:contract? (λ (e)
                                             (and (string-contains? (exn-message e)
                                                                    "must provide a list")
                                                  (string-contains? (exn-message e)
                                                                    "index")))])
         (raise-arguments-error* 'map 'custom-realm "must provide a list"
                                 "index" 0
                                 "data" 'bad)
         #f))

 (list
  (list "unquoted-printing-string basics"
        (let* ([s "hello"]
               [ups (unquoted-printing-string s)])
          (and (equal? (unquoted-printing-string? ups)      #t)
               (equal? (unquoted-printing-string? s)        #f)
               (equal? (unquoted-printing-string-value ups) s))))

  ; Note: display and write are in stdlib, so use --stdlib
  (list "unquoted-printing-string display/write"
        (let* ([ups       (unquoted-printing-string "x\ny")]
               [displayed (call-with-output-string
                           (lambda (p)
                             (display ups p)))]
               [written (call-with-output-string
                         (lambda (p)
                           (write ups p)))])
          (and (equal? displayed "x\ny")
               (equal? written   "x\ny"))))
  )
 )
