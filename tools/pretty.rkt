#!/usr/bin/env racket
#lang racket

(require racket/pretty
         racket/cmdline
         racket/string)

;; Reads s-expressions from an input port and pretty-prints to an output port.
(define (maybe-output-hash-lang in out)
  ;; If the input starts with "#lang", echo that line to the output and
  ;; leave the rest to be read as s-expressions.
  (define s (with-handlers ([exn:fail? (lambda (_) #f)]) (peek-string 5 in)))
  (cond
    [(and (string? s) (string=? s "#lang"))
     (define line (read-line in 'any))
     (when (string? line) (displayln line out))
     #t]
    [else #f]))

(define (pretty-from-port in [out (current-output-port)])
  (port-count-lines! in)
  (define emitted-hash-lang? (maybe-output-hash-lang in out))
  (define (read-one)
    (with-handlers ([exn:fail:read?
                     (lambda (e)
                       (define msg (exn-message e))
                       (if (and (not emitted-hash-lang?) (string-contains? msg "#lang"))
                           (begin
                             (set! emitted-hash-lang? #t)
                             (let ([rest (read-line in 'any)])
                               (when (string? rest)
                                 (displayln (string-append "#lang " rest) out)))
                             (read-one))
                           (let* ([err (current-error-port)]
                                  [locs (exn:fail:read-srclocs e)]
                                  [loc (and (pair? locs) (car locs))]
                                  [src (and loc (srcloc-source loc))]
                                  [line (and loc (srcloc-line loc))]
                                  [col (and loc (srcloc-column loc))])
                             (cond
                               [(and line col src)
                                (fprintf err "Malformed s-expression at ~a:~a:~a: ~a\n" src line col msg)]
                               [(and line col)
                                (fprintf err "Malformed s-expression at line ~a, column ~a: ~a\n" line col msg)]
                               [else
                                (fprintf err "Malformed s-expression: ~a\n" msg)])
                             (exit 1))))])
      (read in)))
  (let loop ()
    (define v (read-one))
    (unless (eof-object? v)
      (with-handlers ([exn:fail?
                       (lambda (e)
                         (fprintf (current-error-port)
                                  "Error writing s-expression: ~a\n" (exn-message e))
                         (exit 1))])
        (pretty-write v out))
      (loop))))

(module+ main
  (define columns-arg #f)
  (define files '())
  (command-line
   #:once-each
   [("-w" "--columns") cols
    "Set pretty-print width (columns)."
    (define n (string->number cols))
    (unless (and n (integer? n) (> n 0))
      (eprintf "--columns requires a positive integer, got: ~a\n" cols)
      (exit 2))
    (set! columns-arg n)]
   #:args rest
   (set! files rest))
  (when columns-arg (pretty-print-columns columns-arg))
  (cond
    [(null? files)
     (pretty-from-port (current-input-port))]
    [else
     (for ([f files])
       (with-handlers ([exn:fail:filesystem?
                        (lambda (e)
                          (eprintf "Could not open ~a: ~a\n" f (exn-message e))
                          (exit 2))])
         (define in (open-input-file f))
         (dynamic-wind
           void
            (lambda ()
              (pretty-from-port in))
            (lambda () (close-input-port in)))))]))
