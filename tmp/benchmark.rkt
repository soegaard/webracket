;;;
;;; Benchmark: generic `js-ref` versus a dedicated binding
;;;

;; Run with:
;;   racket webracket.rkt -r --ffi ffi/standard.ffi --ffi ffi/console.ffi --ffi ffi/temp.ffi tmp/benchmark.rkt
;; or:
;;   racket webracket.rkt -r --browser --ffi ffi/standard.ffi --ffi ffi/console.ffi --ffi ffi/temp.ffi tmp/benchmark.rkt

(include-lib console)
(include-lib temp)

;; bench : string? (-> any/c) exact-nonnegative-integer? -> void?
;;   Run THUNK N times and report elapsed milliseconds and a checksum.
(define (bench label thunk n)
  (void (thunk)) ; warm up
  (define start (js-send/value (js-var "Date") 'now (vector)))
  (define checksum
    (let loop ([i 0] [acc 0])
      (if (= i n)
          acc
          (loop (add1 i) (+ acc (thunk))))))
  (define elapsed (- (js-send/value (js-var "Date") 'now (vector)) start))
  (console-log label elapsed checksum))

(define fixture
  (js-eval "({x: 1})"))

(define setter-fixture
  (js-eval "({x: 0})"))

;; generic-read : -> number?
;;   Read the benchmark property through `js-ref`.
(define (generic-read)
  (js-ref fixture "x"))

;; direct-read : -> number?
;;   Read the benchmark property through the dedicated binding.
(define (direct-read)
  (temp-x fixture))

;; generic-set! : exact-nonnegative-integer? -> void?
;;   Set the benchmark property through `js-set!`.
(define (generic-set! v)
  (js-set! setter-fixture "x" v))

;; direct-set! : exact-nonnegative-integer? -> void?
;;   Set the benchmark property through the dedicated binding.
(define (direct-set! v)
  (temp-set-x! setter-fixture v))

;; bench-set : string? (exact-nonnegative-integer? -> void?) exact-nonnegative-integer? -> void?
;;   Run a setter benchmark and report elapsed milliseconds.
(define (bench-set label setter n)
  (define start (js-send/value (js-var "Date") 'now (vector)))
  (let loop ([i 0])
    (when (< i n)
      (setter i)
      (loop (add1 i))))
  (define elapsed (- (js-send/value (js-var "Date") 'now (vector)) start))
  (console-log label elapsed (js-ref setter-fixture "x")))

(console-log "benchmark starting")
(bench "generic js-ref" generic-read 5000000)
(bench "dedicated binding" direct-read 5000000)
(bench-set "generic js-set!" generic-set! 5000000)
(bench-set "dedicated set!" direct-set! 5000000)
(console-log "benchmark done")
