;; The numbering follows the sections in "The Reference".
(list "map"
      (list
       ; shape 1 — exact 1 argument
       (equal? (map add1 '(1 2 3))
               '(2 3 4))
       ; shape 2 — exact 2 arguments
       (equal? (map cons '(1 2) '(3 4))
               '((1 . 3) (2 . 4)))
       ; shape 3 — exact 3 arguments
       (equal? (map bitwise-bit-field
                    '(15 8)
                    '(1 0)
                    '(3 4))
               '(3 8))
       ; shape 4 — exact 4 arguments
       (equal? (map string-split
                    '("a,b" "hello world")
                    '("," " ")
                    '(#f #f)
                    '(#f #f))
               '(("a" "b") ("hello" "world")))
       ; shape 5 — optional defaults filled to 5 arguments
       (equal? (map string-trim
                    '("  hi  " "--wow--")
                    '(" " "-")
                    '(#t #f)
                    '(#t #t)
                    '(#t #t))
               '("hi" "--wow"))
       ; shape 6 — at least 0 arguments, rest packed as a list
       (equal? (map +
                    '(1 2 3)
                    '(4 5 6))
               '(5 7 9))
       ; shape 7 — at least 1 argument, rest packed as a list
       (equal? (map -
                    '(10 20 30)
                    '(1 2 3))
               '(9 18 27))
       ; shape 8 — at least 2 arguments, rest packed as a list
       (equal? (map filter-map
                    (list (lambda (x) (and (positive? x) x))
                          (lambda (x) (and (negative? x) x)))
                    (list '(1 -2 3)
                          '(-1 -2 5)))
               '((1 3) (-1 -2)))
       ; shape 9 — at least 3 arguments, rest packed as a list
       (equal? (map foldl
                    (list + *)
                    (list 0 1)
                    (list '(1 2 3)
                          '(2 3 4)))
               '(6 24))
       ; shape 10 — at least 0 arguments, rest provided as $Args array
       (equal? (map string '(#\a #\b #\c))
               '("a" "b" "c"))
       ; shape 11 — at least 1 argument, rest provided as $Args array
       (equal? (map vector '(1 2 3) '(4 5 6))
               '(#(1 4) #(2 5) #(3 6)))
       ; shape 12 — at least 2 arguments, rest provided as $Args array
       (equal? (map vector-immutable
                    '(1 2)
                    '(3 4)
                    '(5 6))
               '(#(1 3 5) #(2 4 6)))
       ; shape 13 — at least 3 arguments, rest provided as $Args array
       (equal? (map bytes
                    '(65 68)
                    '(66 69)
                    '(67 70))
               '(#"ABC" #"DEF"))
       ; shape 14 — between 2 and 3 arguments with default handling
       (equal? (map substring
                    '("hello" "world")
                    '(1 2))
               '("ello" "rld"))
       ; shape 15 — between 3 and 4 arguments with default handling
       (equal? (map string-replace
                    '("aba" "hello")
                    '("a" "l")
                    '("x" "L"))
               '("xbx" "heLLo"))))
