;;;
;;; console bridge smoke test
;;;

;; Focused browser smoke fixture for the `globalThis.WR` console bridge.
;;
;; Build:
;;   racket -l errortrace -t ../webracket.rkt -- --browser --console-bridge test-console-bridge-smoke.rkt

(define x 41)

(define (add1* n)
  (+ n 1))

(define (current-x)
  x)

(define (set-x! n)
  (set! x n)
  x)

(define (explode! _)
  (error 'explode! "boom"))

(display "console bridge ready")
