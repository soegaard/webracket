;; The numbering follows the sections in "The Reference".
(list "map"
      (list
       ; fixed 1
       (equal? (map add1 '(1 2 3))
               '(2 3 4))
       ; fixed 2
       (equal? (map cons '(1 2) '(3 4))
               '((1 . 3) (2 . 4)))
       ; 2 or 3, has default
       (equal? (map substring
                    '("hello" "world")
                    '(0 1)
                    '(5 3))
                 '("hello" "or"))
       ; 3 or 4, has default
       (equal? (map string-replace
                      '("aba" "hello")
                      '("a" "l")
                      '("x" "L")
                      '(#t #f))
                 '("xbx" "heLlo"))
       ; 1 to 5 arguments
       (equal? (map string-trim
                      '("  hi  " "--wow--")
                      '(" " "-")
                      '(#t #f)
                      '(#t #t)
                      '(#t #t))
                 '("hi" "--wow"))
       ; 0 or more
       (equal? (map list
                    '(1 2 3)
                    '(4 5 6))
               '((1 4) (2 5) (3 6)))
       (equal? (map -
                    '(1 2 3)
                    '(4 5 6))
               '(-3 -3 -3))
       ; 1 or more
       (equal? (map - '(1 2 3))
                 '(-1 -2 -3))
       ; 2 or more
       (equal? (map filter-map
                      (list (lambda (x) (and (positive? x) x))
                            (lambda (x) (and (negative? x) x)))
                      (list '(1 -2 3)
                            '(-1 -2 5)))
                 '((1 3) (-1 -2)))
       ; 3 or more
       (equal? (map foldl
                      (list + *)
                      (list 0 1)
                      (list '(1 2 3)
                            '(2 3 4)))
                 '(6 24))))
