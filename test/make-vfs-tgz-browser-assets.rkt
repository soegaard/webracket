#lang racket/base

(require file/tar
         file/gzip
         racket/file
         racket/path
         racket/runtime-path)

;; file-entry : path-string? bytes? -> tar-entry?
;;   Create one stable tar member for the browser VFS fixture.
(define (file-entry path content)
  (tar-entry 'file
             path
             (open-input-bytes content)
             (bytes-length content)
             (hash 'permissions #o644
                   'modify-seconds 123)))

;; gzip-bytes : bytes? -> bytes?
;;   Compress bytes as gzip data for the browser VFS fixture.
(define (gzip-bytes bs)
  (define in (open-input-bytes bs))
  (define out (open-output-bytes))
  (gzip-through-ports in out #f 0)
  (get-output-bytes out))

(define-runtime-path here ".")

(define tar-out (open-output-bytes))
(void
 (tar->output (list (file-entry (build-path "hello.txt")
                                #"hello from browser tgz\n"))
              tar-out
              #:format 'ustar))

(call-with-output-file (build-path here "browser-assets.tgz")
  (lambda (out)
    (void (write-bytes (gzip-bytes (get-output-bytes tar-out)) out)))
  #:exists 'truncate/replace)
