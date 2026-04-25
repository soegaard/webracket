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

;; make-link-tar : -> bytes?
;;   Build a tar archive with an unsupported symbolic-link entry.
(define (make-link-tar)
  (define out (open-output-bytes))
  (tar->output (list (tar-entry 'link
                                (build-path "hello-link.txt")
                                (build-path "hello.txt")
                                0
                                (hash 'permissions #o777
                                      'modify-seconds 123)))
               out
               #:format 'ustar)
  (get-output-bytes out))

;; make-truncated-tar : -> bytes?
;;   Build a tar archive whose second file entry is cut short.
(define (make-truncated-tar)
  (subbytes (make-test-tar) 0 1600))

;; retag-header-checksum! : bytes? -> void
;;   Recompute the first tar header checksum after a test mutation.
(define (retag-header-checksum! bs)
  (for ([i (in-range 148 156)])
    (bytes-set! bs i 32))
  (define checksum
    (for/sum ([i (in-range 512)])
      (bytes-ref bs i)))
  (define text (number->string checksum 8))
  (define padded
    (string-append (make-string (max 0 (- 6 (string-length text))) #\0)
                   text))
  (for ([b (in-bytes (string->bytes/utf-8 padded))]
        [i (in-naturals 148)])
    (bytes-set! bs i b))
  (bytes-set! bs 154 0)
  (bytes-set! bs 155 32))

;; write-ascii! : bytes? exact-nonnegative-integer? string? -> void
;;   Write an ASCII field into a byte string.
(define (write-ascii! bs start s)
  (for ([b (in-bytes (string->bytes/utf-8 s))]
        [i (in-naturals start)])
    (bytes-set! bs i b)))

;; write-octal-field! : bytes? exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer? -> void
;;   Write a NUL-padded tar octal field.
(define (write-octal-field! bs start len n)
  (define text (number->string n 8))
  (define padded
    (string-append (make-string (max 0 (- len 1 (string-length text))) #\0)
                   text))
  (write-ascii! bs start padded))

;; global-pax-header : string? -> bytes?
;;   Build a single global pax header member.
(define (global-pax-header record)
  (define header (make-bytes 512 0))
  (define record-bs (string->bytes/utf-8 record))
  (write-ascii! header 0 "GlobalHead")
  (write-octal-field! header 100 8 #o644)
  (write-octal-field! header 108 8 0)
  (write-octal-field! header 116 8 0)
  (write-octal-field! header 124 12 (bytes-length record-bs))
  (write-octal-field! header 136 12 0)
  (bytes-set! header 156 (char->integer #\g))
  (write-ascii! header 257 "ustar")
  (write-ascii! header 263 "00")
  (retag-header-checksum! header)
  (bytes-append header
                record-bs
                (make-bytes (modulo (- 512 (modulo (bytes-length record-bs) 512)) 512) 0)))

;; make-invalid-size-tar : -> bytes?
;;   Build a tar archive with an invalid octal size field.
(define (make-invalid-size-tar)
  (define bs (bytes-copy (make-test-tar)))
  (bytes-set! bs 124 (char->integer #\x))
  (retag-header-checksum! bs)
  bs)

;; make-global-pax-mtime-tar : -> bytes?
;;   Build a tar archive with a global pax mtime default.
(define (make-global-pax-mtime-tar)
  (bytes-append (global-pax-header "13 mtime=321\n")
                (make-test-tar)))

;; make-pax-tar : -> bytes?
;;   Build a tar archive that uses a pax path record.
(define (make-pax-tar)
  (define long-name
    (string-append (make-string 120 #\a) ".txt"))
  (define out (open-output-bytes))
  (tar->output (list (file-entry (string->path long-name) #"long\n"))
               out
               #:format 'pax)
  (get-output-bytes out))

;; make-pax-unicode-path-tar : -> bytes?
;;   Build a tar archive whose pax path record needs UTF-8 byte counting.
(define (make-pax-unicode-path-tar)
  (define out (open-output-bytes))
  (tar->output (list (file-entry (string->path "caf\u00E9.txt") #"coffee\n"))
               out
               #:format 'pax)
  (get-output-bytes out))

;; make-invalid-pax-tar : -> bytes?
;;   Build a tar archive with a malformed pax record body.
(define (make-invalid-pax-tar)
  (define bs (bytes-copy (make-pax-tar)))
  (bytes-set! bs 512 (char->integer #\x))
  bs)

;; run-tar-program : string? [#:tar-bytes bytes?] [#:mount-path string?] [#:source-mode symbol?] -> (values exact-integer? string?)
;;   Compile and run a program against an inline tar-mounted VFS backend.
(define (run-tar-program program
                         #:tar-bytes [tar-bytes (make-test-tar)]
                         #:mount-path [mount-path "/assets"]
                         #:source-mode [source-mode 'base64])
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
      (define-values (mount-flag mount-spec)
        (case source-mode
          [(base64)
           (values "--vfs-tar-base64"
                   (string-append mount-path
                                  "="
                                  (bytes->string/utf-8 (base64-encode tar-bytes #""))))]
          [(relative-file)
           (define tar-path (build-path dest-dir "assets.tar"))
           (call-with-output-file tar-path
             (lambda (out) (write-bytes tar-bytes out))
             #:exists 'truncate/replace)
           (values "--vfs-tar-file" (string-append mount-path "=assets.tar"))]
          [else
           (error 'run-tar-program
                  (format "unknown tar source mode: ~a" source-mode))]))
      (define stdout (open-output-string))
      (define stderr (open-output-string))
      (define status
        (parameterize ([current-output-port stdout]
                       [current-error-port stderr])
          (system*/exit-code
           (find-executable-path "racket")
           (path->string webracket-rkt)
           "--dest" (path->string dest-dir)
           mount-flag mount-spec
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

;; test-vfs-tar-file-mount : -> void
;;   Check relative tar file sources are resolved against the generated runtime.
(define (test-vfs-tar-file-mount)
  (define-values (status output)
    (run-tar-program "(file->string \"/assets/hello.txt\")\n"
                     #:source-mode 'relative-file))
  (unless (zero? status)
    (error 'test-vfs-tar-file-mount
           (format "compile/run failed (~a): ~a" status output))))

;; test-vfs-tar-nested-mount : -> void
;;   Check that a nested tar mount is visible through its parent directory.
(define (test-vfs-tar-nested-mount)
  (define program
    #<<PROGRAM
(unless
 (and (equal? (file->string "/app/assets/hello.txt") "hello from tar\n")
      (equal? (map path->string (directory-list "/app"))
              '("assets"))
      (equal? (map path->string (directory-list "/app/assets"))
              '("hello.txt" "nested"))
      (directory-exists? "/app/assets/nested"))
 (error 'vfs-tar-mount "nested mount checks failed"))
PROGRAM
)
  (define-values (status output)
    (run-tar-program program #:mount-path "/app/assets"))
  (unless (zero? status)
    (error 'test-vfs-tar-nested-mount
           (format "compile/run failed (~a): ~a" status output))))

;; test-vfs-tar-synthetic-parent-mount : -> void
;;   Check that unmounted parents of nested tar mounts behave as directories.
(define (test-vfs-tar-synthetic-parent-mount)
  (define program
    #<<PROGRAM
(unless
 (and (equal? (file->string "/pkg/assets/hello.txt") "hello from tar\n")
      (directory-exists? "/pkg")
      (equal? (map path->string (directory-list "/"))
              '("app" "pkg" "tmp"))
      (equal? (map path->string (directory-list "/pkg"))
              '("assets"))
      (equal? (map path->string (directory-list "/pkg/assets"))
              '("hello.txt" "nested")))
 (error 'vfs-tar-mount "synthetic parent mount checks failed"))
PROGRAM
)
  (define-values (status output)
    (run-tar-program program #:mount-path "/pkg/assets"))
  (unless (zero? status)
    (error 'test-vfs-tar-synthetic-parent-mount
           (format "compile/run failed (~a): ~a" status output))))

;; test-vfs-tar-pax-unicode-path : -> void
;;   Check that pax path lengths are interpreted as UTF-8 byte counts.
(define (test-vfs-tar-pax-unicode-path)
  (define program
    #<<PROGRAM
(unless
 (and (equal? (file->string "/assets/caf\u00E9.txt") "coffee\n")
      (equal? (map path->string (directory-list "/assets"))
              '("caf\u00E9.txt")))
 (error 'vfs-tar-mount "unicode pax path checks failed"))
PROGRAM
)
  (define-values (status output)
    (run-tar-program program #:tar-bytes (make-pax-unicode-path-tar)))
  (unless (zero? status)
    (error 'test-vfs-tar-pax-unicode-path
           (format "compile/run failed (~a): ~a" status output))))

;; test-vfs-tar-mount-global-pax-mtime : -> void
;;   Check that global pax mtime applies to following entries.
(define (test-vfs-tar-mount-global-pax-mtime)
  (define program
    #<<PROGRAM
(unless
 (and (equal? (file-or-directory-modify-seconds "/assets/hello.txt") 321)
      (equal? (file-or-directory-modify-seconds "/assets/nested/leaf.txt") 321))
 (error 'vfs-tar-mount "global pax mtime checks failed"))
PROGRAM
)
  (define-values (status output)
    (run-tar-program program #:tar-bytes (make-global-pax-mtime-tar)))
  (unless (zero? status)
    (error 'test-vfs-tar-mount-global-pax-mtime
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

;; test-vfs-tar-mount-rejects-links : -> void
;;   Check that unsupported tar link entries fail while mounting.
(define (test-vfs-tar-mount-rejects-links)
  (define-values (status output)
    (run-tar-program "(void)\n" #:tar-bytes (make-link-tar)))
  (when (zero? status)
    (error 'test-vfs-tar-mount-rejects-links
           "expected tar link mount to fail"))
  (unless (regexp-match? #rx"VFS tar link entries are unsupported" output)
    (error 'test-vfs-tar-mount-rejects-links
           (format "expected unsupported tar link failure, got: ~a" output))))

;; test-vfs-tar-mount-rejects-truncated-entry : -> void
;;   Check that tar entries whose payload extends past the archive fail.
(define (test-vfs-tar-mount-rejects-truncated-entry)
  (define-values (status output)
    (run-tar-program "(void)\n" #:tar-bytes (make-truncated-tar)))
  (when (zero? status)
    (error 'test-vfs-tar-mount-rejects-truncated-entry
           "expected truncated tar mount to fail"))
  (unless (regexp-match? #rx"VFS tar entry data is truncated" output)
    (error 'test-vfs-tar-mount-rejects-truncated-entry
           (format "expected truncated tar failure, got: ~a" output))))

;; test-vfs-tar-mount-rejects-invalid-size : -> void
;;   Check that malformed octal size fields fail while mounting.
(define (test-vfs-tar-mount-rejects-invalid-size)
  (define-values (status output)
    (run-tar-program "(void)\n" #:tar-bytes (make-invalid-size-tar)))
  (when (zero? status)
    (error 'test-vfs-tar-mount-rejects-invalid-size
           "expected invalid-size tar mount to fail"))
  (unless (regexp-match? #rx"VFS tar size is invalid" output)
    (error 'test-vfs-tar-mount-rejects-invalid-size
           (format "expected invalid tar size failure, got: ~a" output))))

;; test-vfs-tar-mount-rejects-invalid-pax : -> void
;;   Check that malformed pax records fail while mounting.
(define (test-vfs-tar-mount-rejects-invalid-pax)
  (define-values (status output)
    (run-tar-program "(void)\n" #:tar-bytes (make-invalid-pax-tar)))
  (when (zero? status)
    (error 'test-vfs-tar-mount-rejects-invalid-pax
           "expected invalid-pax tar mount to fail"))
  (unless (regexp-match? #rx"VFS tar pax header is invalid" output)
    (error 'test-vfs-tar-mount-rejects-invalid-pax
           (format "expected invalid tar pax failure, got: ~a" output))))

(module+ test
  (test-vfs-tar-mount)
  (test-vfs-tar-file-mount)
  (test-vfs-tar-nested-mount)
  (test-vfs-tar-synthetic-parent-mount)
  (test-vfs-tar-pax-unicode-path)
  (test-vfs-tar-mount-global-pax-mtime)
  (test-vfs-tar-mount-read-only)
  (test-vfs-tar-mount-rejects-links)
  (test-vfs-tar-mount-rejects-truncated-entry)
  (test-vfs-tar-mount-rejects-invalid-size)
  (test-vfs-tar-mount-rejects-invalid-pax))

(module+ main
  (test-vfs-tar-mount)
  (test-vfs-tar-file-mount)
  (test-vfs-tar-nested-mount)
  (test-vfs-tar-synthetic-parent-mount)
  (test-vfs-tar-pax-unicode-path)
  (test-vfs-tar-mount-global-pax-mtime)
  (test-vfs-tar-mount-read-only)
  (test-vfs-tar-mount-rejects-links)
  (test-vfs-tar-mount-rejects-truncated-entry)
  (test-vfs-tar-mount-rejects-invalid-size)
  (test-vfs-tar-mount-rejects-invalid-pax)
  (displayln "ok"))
