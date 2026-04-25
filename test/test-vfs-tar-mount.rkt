#lang racket/base

(require net/base64
         file/tar
         racket/file
         racket/path
         racket/system)

;; file-entry : path-string? bytes? -> tar-entry?
;;   Create one in-memory tar file entry with stable metadata.
(define (file-entry path content)
  (tar-entry 'file
             path
             (open-input-bytes content)
             (bytes-length content)
             (hash 'permissions #o644
                   'modify-seconds 123)))

;; make-test-tar : -> bytes?
;;   Build a tiny tar archive with implicit directories.
(define (make-test-tar)
  (define out (open-output-bytes))
  (tar->output (list (file-entry (build-path "hello.txt") #"hello from tar\n")
                     (file-entry (build-path "nested" "leaf.txt") #"leaf\n"))
               out
               #:format 'ustar)
  (get-output-bytes out))

;; run-tar-program : string? -> (values exact-integer? string?)
;;   Compile and run a program against an inline tar-mounted VFS backend.
(define (run-tar-program program)
  (define repo-root (simplify-path (build-path (current-directory) "..")))
  (define webracket-rkt (build-path repo-root "webracket.rkt"))
  (define source-dir (make-temporary-file "webracket-vfs-tar-src-~a" 'directory))
  (define dest-dir (make-temporary-file "webracket-vfs-tar-out-~a" 'directory))
  (define source-path (build-path source-dir "tar-smoke.rkt"))
  (dynamic-wind
    void
    (lambda ()
      (call-with-output-file source-path
        (lambda (out) (display program out))
        #:exists 'truncate/replace)
      (define tar-base64
        (bytes->string/utf-8 (base64-encode (make-test-tar) #"")))
      (define stdout (open-output-string))
      (define stderr (open-output-string))
      (define status
        (parameterize ([current-output-port stdout]
                       [current-error-port stderr])
          (system*/exit-code
           (find-executable-path "racket")
           (path->string webracket-rkt)
           "--dest" (path->string dest-dir)
           "--vfs-tar-base64" (string-append "/assets=" tar-base64)
           "-r"
           (path->string source-path))))
      (values status
              (string-append (get-output-string stdout)
                             (get-output-string stderr))))
    (lambda ()
      (with-handlers ([exn:fail:filesystem? void])
        (delete-directory/files source-dir))
      (with-handlers ([exn:fail:filesystem? void])
        (delete-directory/files dest-dir)))))

;; test-vfs-tar-mount : -> void
;;   Check read/list/stat behavior for an inline tar-mounted VFS backend.
(define (test-vfs-tar-mount)
  (define program
    #<<PROGRAM
(unless
 (and (equal? (file->string "/assets/hello.txt") "hello from tar\n")
      (equal? (file->string "/assets/nested/leaf.txt") "leaf\n")
      (equal? (map path->string (directory-list "/assets"))
              '("hello.txt" "nested"))
      (equal? (map path->string (directory-list "/"))
              '("app" "assets" "tmp"))
      (equal? (map path->string (filesystem-root-list))
              '("/app/" "/assets/" "/tmp/"))
      (directory-exists? "/assets/nested")
      (equal? (file-or-directory-modify-seconds "/assets/hello.txt") 123)
      (equal? (file-or-directory-permissions "/assets/hello.txt" 'bits) #o644))
 (error 'vfs-tar-mount "mounted tar checks failed"))
PROGRAM
)
  (define-values (status output) (run-tar-program program))
  (unless (zero? status)
    (error 'test-vfs-tar-mount
           (format "compile/run failed (~a): ~a" status output))))

;; test-vfs-tar-mount-read-only : -> void
;;   Check that writes to tar-mounted files fail in the generated runtime.
(define (test-vfs-tar-mount-read-only)
  (define program
    "(webracket-vfs-write-file \"/assets/hello.txt\" #\"overwrite\")\n")
  (define-values (status output) (run-tar-program program))
  (when (zero? status)
    (error 'test-vfs-tar-mount-read-only
           "expected write into tar mount to fail"))
  (unless (regexp-match? #rx"VFS file write failed" output)
    (error 'test-vfs-tar-mount-read-only
           (format "expected VFS file write failure, got: ~a" output))))

(module+ test
  (test-vfs-tar-mount)
  (test-vfs-tar-mount-read-only))

(module+ main
  (test-vfs-tar-mount)
  (test-vfs-tar-mount-read-only)
  (displayln "ok"))
