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

(define dir (make-temporary-directory "wr-host-dir~a" "tmp"))
(define nested (build-path dir "nested"))
(define data-path (build-path nested "data.txt"))
(define copy-path (build-path nested "copy.txt"))
(define renamed-path (build-path nested "renamed.txt"))
(define temp-path (make-temporary-file "wr-host-file~a.txt" #f dir))
(define temp-star-path (make-temporary-file* #"wr-host-prefix-" #".txt" #f dir))
(define temp-star-dir (make-temporary-directory* #"wr-host-dir-prefix-" #"" dir))

(make-directory* nested)
(display-to-file "payload" data-path 'text 'replace)
(copy-file data-path copy-path)
(rename-file-or-directory copy-path renamed-path)

(unless (and (file-exists? temp-path)
             (file-exists? temp-star-path)
             (directory-exists? temp-star-dir)
             (member (build-path nested "data.txt") (directory-list nested #t))
             (equal? (file->bytes renamed-path 'binary) #"payload")
             (integer? (file-or-directory-modify-seconds renamed-path))
             (file-or-directory-permissions renamed-path)
             (file-or-directory-stat renamed-path)
             (file-or-directory-identity renamed-path)
             (path? (find-system-path 'temp-dir))
             (list? (filesystem-root-list))
             (relative-path? (find-relative-path dir renamed-path))
             (path? (shrink-path-wrt renamed-path (list dir)))
             (or (not (current-drive)) (path? (current-drive))))
  (error 'primitives-host-filesystem-wrappers "failed"))

(delete-file data-path)
(delete-file renamed-path)
(delete-file temp-path)
(delete-file temp-star-path)
(delete-directory temp-star-dir)
(delete-directory/files dir)
(delete-file path)
