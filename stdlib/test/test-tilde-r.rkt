(list
  ;; Basic decimal rounding
  (equal? (~r/precision 10.22  1) "10.2")
  (equal? (~r/precision 10.25  1) "10.3")  ; round half up
  (equal? (~r/precision 10.0   1) "10")    ; trailing zero dropped
  (equal? (~r/precision 10.0   2) "10")    ; trailing zeros dropped
  (equal? (~r/precision 3.14159 2) "3.14")
  (equal? (~r/precision 3.14159 4) "3.1416")
  (equal? (~r/precision 3.14159 0) "3")

  ;; Negative numbers
  (equal? (~r/precision -10.22 1) "-10.2")
  (equal? (~r/precision -3.14159 3) "-3.142")

  ;; Zero
  (equal? (~r/precision 0.0   2) "0")
  (equal? (~r/precision 0     2) "0")

  ;; Integers
  (equal? (~r/precision 42    3) "42")
  (equal? (~r/precision 100   0) "100")

  ;; Large integers (precision loss risk)
  ; (equal? (~r/precision 21231234567890 0) "21231234567890")

  ;; Values that round up to next integer
  (equal? (~r/precision 9.99  1) "10")
  (equal? (~r/precision 9.99  0) "10")
  (equal? (~r/precision -9.99 1) "-10")

  ;; Small fractions
  (equal? (~r/precision 0.001  2) "0")
  (equal? (~r/precision 0.005  2) "0.01")
  (equal? (~r/precision 0.1234 3) "0.123")
  (equal? (~r/precision 0.1235 3) "0.124")

  ;; Precision 6 (default-like)
  (equal? (~r/precision 78.80000000000001 6) "78.8")
  (equal? (~r/precision 1.5 6) "1.5")
  (equal? (~r/precision 50.0 6) "50")

  ;; Precision 1
  (equal? (~r/precision 78.80000000000001 1) "78.8")
  (equal? (~r/precision 1.100000000  1)
          (~r/precision 1.1000000000 1))
  (equal? (~r/precision 1.1000000000  1)  ; more of a reader test ;-)
          (~r/precision 1.10000000000 1))

  
  )
