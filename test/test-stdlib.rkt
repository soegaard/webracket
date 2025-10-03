
(list "Writing"

      (list "write"
            (equal? (call-with-output-string  (λ (out) (write 42 out)))
                    "42")
            (equal? (call-with-output-string  (λ (out) (write "abc" out)))
                    "\"abc\""))

      (list "fprintf"
            (equal? (call-with-output-string
                     (λ (out)
                       (fprintf out
                                "~a as a string is ~s.\n"
                                '(3 4)
                                "(3 4)")))
                    "(3 4) as a string is \"(3 4)\".")))

      

