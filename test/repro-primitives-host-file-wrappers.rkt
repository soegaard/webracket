#lang webracket

(define path "tmp/repro-primitives-host-file-wrappers.txt")

(display-to-file "alpha\nbeta\n" path 'text 'replace)

(define read-with-call
  (call-with-input-file path
    (lambda (in)
      (read-line in))
    'text))

(call-with-output-file path
  (lambda (out)
    (display "gamma\n" out))
  'text
  'replace)

(with-output-to-file path
  (lambda ()
    (display "delta\n"))
  'text
  'replace)

(unless (and (equal? read-with-call "alpha")
             (equal? (file->string path 'text) "delta\n")
             (equal? (file->lines path 'text 'any) '("delta")))
  (error 'primitives-host-file-wrappers "failed"))
