;; The numbering follows the sections in "The Reference".
(list "map"
      (list
       ; shape 0 — exact 0 arguments
       (void? (make-void))
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
               '("xbx" "heLLo"))
       ; shape 16 — between 0 and 1 arguments with default handling
       (let ([no-arg (flrandom)]
             [with-arg (flrandom #f)])
         (and (flonum? no-arg)
              (flonum? with-arg)))
       ; shape 17 — between 0 and 2 arguments with default handling
       (let ([r0 (random)]
             [r1 (random 7)]
             [r2 (random 3 7)])
         (and (real? r0)
              (<= 0 r0) (< r0 1)
              (exact-nonnegative-integer? r1) (< r1 7)
              (integer? r2) (<= 3 r2) (< r2 7)))
       ; shape 18 — between 1 and 2 arguments with default handling
       (and (string=? (number->string 255)
                      "255")
            (string=? (number->string 255 16)
                      "ff"))
       ; shape 19 — between 1 and 3 arguments with default handling
       (let* ([s "h\u00E9"]
              [full (string-utf-8-length s)]
              [suffix (string-utf-8-length s 1)]
              [prefix (string-utf-8-length s 0 1)])
         (and (= full 3)
              (= suffix 2)
              (= prefix 1)))
       ; shape 20 — between 1 and 4 arguments with default handling
       (let ([words (string-split "a,b,c" ",")]
             [trimmed (string-split "x--y--z" "--" #t #t)])
         (and (equal? words '("a" "b" "c"))
              (equal? trimmed '("x" "y" "z"))))
       ; shape 21 — between 1 and 5 arguments with default handling
       (let ([basic (string-trim "  hi  ")]
             [custom (string-trim "--wow--" "-" #t #t #t)])
         (and (string=? basic "hi")
              (string=? custom "wow")))
       ; shape 22 — between 2 and 4 arguments with default handling
       #;(let* ([all (vector-sort '#(5 3 4 1 2) <)]
                [partial (vector-sort '#(4 3 2 1 0) < 1 4)])
           (and (equal? all '#(1 2 3 4 5))
                (equal? partial '#(4 1 2 3 0))))
       ; shape 23 — between 2 and 5 arguments with default handling
       (let* ([bs #"h\xC3\xA9!"]
              [full (bytes->string/utf-8 bs #f #f #f)]
              [slice (bytes->string/utf-8 bs #f 1 3)])
         (and (string=? full "hé!")
              (string=? slice "é")))
       ; shape 24 — between 3 and 5 arguments with default handling
       (let ([dst (string-copy "hello")]
             [src "XYZW"])
         (string-copy! dst 1 src 1 3)
         (string=? dst "hYZlo"))))

