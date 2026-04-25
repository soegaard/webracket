#lang racket/base

(require net/base64
         file/tar
         racket/file
         racket/path
         racket/string
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

;; make-unsupported-type-tar : -> bytes?
;;   Build a tar archive with an unsupported tar entry typeflag.
(define (make-unsupported-type-tar)
  (bytes-append (tar-header "device" 0 #\3)
                (make-bytes 1024 0)))

;; make-truncated-tar : -> bytes?
;;   Build a tar archive whose second file entry is cut short.
(define (make-truncated-tar)
  (subbytes (make-test-tar) 0 1600))

;; make-trailing-data-tar : -> bytes?
;;   Build a tar archive with non-zero trailing partial-block data.
(define (make-trailing-data-tar)
  (bytes-append (make-test-tar) #"junk"))

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

;; tar-header : string? exact-nonnegative-integer? char? [#:mode exact-nonnegative-integer?] -> bytes?
;;   Build a minimal ustar header for hand-written tar fixtures.
(define (tar-header name size typeflag #:mode [mode #o644])
  (define header (make-bytes 512 0))
  (write-ascii! header 0 name)
  (write-octal-field! header 100 8 mode)
  (write-octal-field! header 108 8 0)
  (write-octal-field! header 116 8 0)
  (write-octal-field! header 124 12 size)
  (write-octal-field! header 136 12 123)
  (bytes-set! header 156 (char->integer typeflag))
  (write-ascii! header 257 "ustar")
  (write-ascii! header 263 "00")
  (retag-header-checksum! header)
  header)

;; pad-tar-data : bytes? -> bytes?
;;   Pad a tar entry payload to a 512-byte boundary.
(define (pad-tar-data bs)
  (bytes-append bs
                (make-bytes (modulo (- 512 (modulo (bytes-length bs) 512)) 512) 0)))

;; tar-file-member : string? bytes? -> bytes?
;;   Build one hand-written regular-file member for tar fixtures.
(define (tar-file-member name content)
  (bytes-append (tar-header name (bytes-length content) #\0)
                (pad-tar-data content)))

;; tar-directory-member : string? -> bytes?
;;   Build one hand-written directory member for tar fixtures.
(define (tar-directory-member name)
  (tar-header name 0 #\5 #:mode #o755))

;; make-duplicate-file-tar : -> bytes?
;;   Build a tar archive where an appended file entry replaces an earlier one.
(define (make-duplicate-file-tar)
  (bytes-append (tar-file-member "hello.txt" #"old\n")
                (tar-file-member "hello.txt" #"new\n")
                (make-bytes 1024 0)))

;; make-file-directory-conflict-tar : symbol? -> bytes?
;;   Build a tar archive with conflicting file and directory entries.
(define (make-file-directory-conflict-tar order)
  (case order
    [(file-then-directory)
     (bytes-append (tar-file-member "conflict" #"file\n")
                   (tar-directory-member "conflict/")
                   (make-bytes 1024 0))]
    [(directory-then-file)
     (bytes-append (tar-directory-member "conflict/")
                   (tar-file-member "conflict" #"file\n")
                   (make-bytes 1024 0))]
    [else
     (error 'make-file-directory-conflict-tar
            (format "unknown conflict order: ~a" order))]))

;; make-explicit-directory-tar : -> bytes?
;;   Build a tar archive with explicit directory entries and one nested file.
(define (make-explicit-directory-tar)
  (bytes-append (tar-directory-member "empty/")
                (tar-directory-member "dir/")
                (tar-file-member "dir/file.txt" #"inside\n")
                (make-bytes 1024 0)))

;; make-duplicate-directory-tar : -> bytes?
;;   Build a tar archive with duplicate directory entries.
(define (make-duplicate-directory-tar)
  (bytes-append (tar-directory-member "dup/")
                (tar-directory-member "dup/")
                (tar-file-member "dup/file.txt" #"inside\n")
                (make-bytes 1024 0)))

;; pax-record : string? string? -> bytes?
;;   Build one pax record with the correct byte-count prefix.
(define (pax-record key value)
  (define body (string-append key "=" value "\n"))
  (let loop ([digits 1])
    (define len (+ digits 1 (bytes-length (string->bytes/utf-8 body))))
    (define next-digits (string-length (number->string len)))
    (if (= digits next-digits)
        (string->bytes/utf-8 (string-append (number->string len) " " body))
        (loop next-digits))))

;; make-invalid-size-tar : -> bytes?
;;   Build a tar archive with an invalid octal size field.
(define (make-invalid-size-tar)
  (define bs (bytes-copy (make-test-tar)))
  (bytes-set! bs 124 (char->integer #\x))
  (retag-header-checksum! bs)
  bs)

;; make-invalid-checksum-tar : -> bytes?
;;   Build a tar archive with a corrupted first header checksum.
(define (make-invalid-checksum-tar)
  (define bs (bytes-copy (make-test-tar)))
  (bytes-set! bs 0 (char->integer #\x))
  bs)

;; make-invalid-path-tar : string? -> bytes?
;;   Build a tar archive with an unsafe entry path.
(define (make-invalid-path-tar name)
  (bytes-append (tar-file-member name #"bad\n")
                (make-bytes 1024 0)))

;; make-invalid-pax-path-tar : string? -> bytes?
;;   Build a tar archive with an unsafe pax path override.
(define (make-invalid-pax-path-tar name)
  (define pax-bs (pax-record "path" name))
  (define content #"bad\n")
  (bytes-append (tar-header "PaxHeader" (bytes-length pax-bs) #\x)
                (pad-tar-data pax-bs)
                (tar-header "safe.txt" (bytes-length content) #\0)
                (pad-tar-data content)
                (make-bytes 1024 0)))

;; make-global-pax-mtime-tar : -> bytes?
;;   Build a tar archive with a global pax mtime default.
(define (make-global-pax-mtime-tar)
  (bytes-append (global-pax-header (bytes->string/utf-8 (pax-record "mtime" "321")))
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

;; make-pax-size-tar : -> bytes?
;;   Build a tar archive whose file size comes from a local pax header.
(define (make-pax-size-tar)
  (define pax-bs (pax-record "size" "7"))
  (define content #"content")
  (bytes-append (tar-header "PaxHeader" (bytes-length pax-bs) #\x)
                (pad-tar-data pax-bs)
                (tar-header "sized.txt" 0 #\0)
                (pad-tar-data content)
                (make-bytes 1024 0)))

;; make-invalid-pax-size-tar : -> bytes?
;;   Build a tar archive with an invalid pax size record.
(define (make-invalid-pax-size-tar)
  (define pax-bs (pax-record "size" "bad"))
  (define content #"content")
  (bytes-append (tar-header "PaxHeader" (bytes-length pax-bs) #\x)
                (pad-tar-data pax-bs)
                (tar-header "sized.txt" 0 #\0)
                (pad-tar-data content)
                (make-bytes 1024 0)))

;; make-pax-fractional-mtime-tar : -> bytes?
;;   Build a tar archive with a local fractional pax mtime.
(define (make-pax-fractional-mtime-tar)
  (define pax-bs (pax-record "mtime" "321.9"))
  (define content #"time\n")
  (bytes-append (tar-header "PaxHeader" (bytes-length pax-bs) #\x)
                (pad-tar-data pax-bs)
                (tar-header "time.txt" (bytes-length content) #\0)
                (pad-tar-data content)
                (make-bytes 1024 0)))

;; make-invalid-pax-mtime-tar : -> bytes?
;;   Build a tar archive with an invalid pax mtime record.
(define (make-invalid-pax-mtime-tar)
  (define pax-bs (pax-record "mtime" "bad"))
  (define content #"time\n")
  (bytes-append (tar-header "PaxHeader" (bytes-length pax-bs) #\x)
                (pad-tar-data pax-bs)
                (tar-header "time.txt" (bytes-length content) #\0)
                (pad-tar-data content)
                (make-bytes 1024 0)))

;; make-invalid-pax-tar : -> bytes?
;;   Build a tar archive with a malformed pax record body.
(define (make-invalid-pax-tar)
  (define bs (bytes-copy (make-pax-tar)))
  (bytes-set! bs 512 (char->integer #\x))
  bs)

;; make-dangling-pax-tar : -> bytes?
;;   Build a tar archive with a local pax header but no target entry.
(define (make-dangling-pax-tar)
  (define pax-bs (pax-record "path" "x.txt"))
  (bytes-append (tar-header "PaxHeader" (bytes-length pax-bs) #\x)
                (pad-tar-data pax-bs)
                (make-bytes 1024 0)))

;; make-dangling-long-name-tar : -> bytes?
;;   Build a tar archive with a GNU long-name header but no target entry.
(define (make-dangling-long-name-tar)
  (define name #"long-name.txt\0")
  (bytes-append (tar-header "././@LongLink" (bytes-length name) #\L)
                (pad-tar-data name)
                (make-bytes 1024 0)))

;; make-dangling-long-link-tar : -> bytes?
;;   Build a tar archive with a GNU long-link header but no target entry.
(define (make-dangling-long-link-tar)
  (define name #"long-link-target.txt\0")
  (bytes-append (tar-header "././@LongLink" (bytes-length name) #\K)
                (pad-tar-data name)
                (make-bytes 1024 0)))

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

;; test-vfs-tar-duplicate-file-last-wins : -> void
;;   Check that appended regular-file entries update earlier tar contents.
(define (test-vfs-tar-duplicate-file-last-wins)
  (define program
    #<<PROGRAM
(unless (equal? (file->string "/assets/hello.txt") "new\n")
  (error 'vfs-tar-mount "duplicate file checks failed"))
PROGRAM
)
  (define-values (status output)
    (run-tar-program program #:tar-bytes (make-duplicate-file-tar)))
  (unless (zero? status)
    (error 'test-vfs-tar-duplicate-file-last-wins
           (format "compile/run failed (~a): ~a" status output))))

;; test-vfs-tar-explicit-directories : -> void
;;   Check explicit tar directory entries, listings, and metadata.
(define (test-vfs-tar-explicit-directories)
  (define program
    #<<PROGRAM
(unless
 (and (directory-exists? "/assets/empty")
      (directory-exists? "/assets/dir")
      (equal? (map path->string (directory-list "/assets"))
              '("dir" "empty"))
      (equal? (map path->string (directory-list "/assets/dir"))
              '("file.txt"))
      (equal? (file-or-directory-modify-seconds "/assets/empty") 123)
      (equal? (file-or-directory-permissions "/assets/empty" 'bits) #o755)
      (equal? (file->string "/assets/dir/file.txt") "inside\n"))
 (error 'vfs-tar-mount "explicit directory checks failed"))
PROGRAM
)
  (define-values (status output)
    (run-tar-program program #:tar-bytes (make-explicit-directory-tar)))
  (unless (zero? status)
    (error 'test-vfs-tar-explicit-directories
           (format "compile/run failed (~a): ~a" status output))))

;; test-vfs-tar-duplicate-directories : -> void
;;   Check that duplicate tar directory entries are harmless.
(define (test-vfs-tar-duplicate-directories)
  (define program
    #<<PROGRAM
(unless
 (and (directory-exists? "/assets/dup")
      (equal? (map path->string (directory-list "/assets"))
              '("dup"))
      (equal? (map path->string (directory-list "/assets/dup"))
              '("file.txt"))
      (equal? (file->string "/assets/dup/file.txt") "inside\n"))
 (error 'vfs-tar-mount "duplicate directory checks failed"))
PROGRAM
)
  (define-values (status output)
    (run-tar-program program #:tar-bytes (make-duplicate-directory-tar)))
  (unless (zero? status)
    (error 'test-vfs-tar-duplicate-directories
           (format "compile/run failed (~a): ~a" status output))))

;; test-vfs-tar-mount-rejects-file-then-directory : -> void
;;   Check that a later directory cannot replace an earlier file entry.
(define (test-vfs-tar-mount-rejects-file-then-directory)
  (define-values (status output)
    (run-tar-program "(void)\n"
                     #:tar-bytes (make-file-directory-conflict-tar 'file-then-directory)))
  (when (zero? status)
    (error 'test-vfs-tar-mount-rejects-file-then-directory
           "expected file-then-directory tar mount to fail"))
  (unless (regexp-match? #rx"VFS tar file/directory conflict" output)
    (error 'test-vfs-tar-mount-rejects-file-then-directory
           (format "expected file/directory conflict failure, got: ~a" output))))

;; test-vfs-tar-mount-rejects-directory-then-file : -> void
;;   Check that a later file cannot replace an earlier directory entry.
(define (test-vfs-tar-mount-rejects-directory-then-file)
  (define-values (status output)
    (run-tar-program "(void)\n"
                     #:tar-bytes (make-file-directory-conflict-tar 'directory-then-file)))
  (when (zero? status)
    (error 'test-vfs-tar-mount-rejects-directory-then-file
           "expected directory-then-file tar mount to fail"))
  (unless (regexp-match? #rx"VFS tar file/directory conflict" output)
    (error 'test-vfs-tar-mount-rejects-directory-then-file
           (format "expected file/directory conflict failure, got: ~a" output))))

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
      (equal? (map path->string (filesystem-root-list))
              '("/app/" "/pkg/" "/tmp/"))
      (equal? (hash-ref (file-or-directory-stat "/") 'inode)
              (file-or-directory-identity "/"))
      (equal? (hash-ref (file-or-directory-stat "/") 'mode) #o777)
      (equal? (map path->string (directory-list "/pkg"))
              '("assets"))
      (equal? (file-or-directory-modify-seconds "/pkg") 0)
      (equal? (file-or-directory-permissions "/pkg" 'bits) #o777)
      (equal? (hash-ref (file-or-directory-stat "/pkg") 'mode) #o777)
      (equal? (hash-ref (file-or-directory-stat "/pkg") 'modify-time-seconds) 0)
      (equal? (hash-ref (file-or-directory-stat "/pkg") 'inode)
              (file-or-directory-identity "/pkg"))
      (with-handlers ([(lambda (ex) (exn:fail:filesystem? ex))
                       (lambda (_ex) #t)])
        (delete-directory "/pkg")
        #f)
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

;; test-vfs-tar-root-mount-list : -> void
;;   Check that an explicit root mount reports `/` as the filesystem root.
(define (test-vfs-tar-root-mount-list)
  (define program
    #<<PROGRAM
(unless
 (and (equal? (file->string "/hello.txt") "hello from tar\n")
      (equal? (map path->string (filesystem-root-list))
              '("/"))
      (equal? (map path->string (directory-list "/"))
              '("app" "hello.txt" "nested" "tmp")))
 (error 'vfs-tar-mount "root mount checks failed"))
PROGRAM
)
  (define-values (status output)
    (run-tar-program program #:mount-path "/"))
  (unless (zero? status)
    (error 'test-vfs-tar-root-mount-list
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

;; test-vfs-tar-pax-size : -> void
;;   Check that local pax size records determine regular file payload length.
(define (test-vfs-tar-pax-size)
  (define program
    #<<PROGRAM
(unless
 (and (equal? (file-size "/assets/sized.txt") 7)
      (equal? (file->string "/assets/sized.txt") "content"))
 (error 'vfs-tar-mount "pax size checks failed"))
PROGRAM
)
  (define-values (status output)
    (run-tar-program program #:tar-bytes (make-pax-size-tar)))
  (unless (zero? status)
    (error 'test-vfs-tar-pax-size
           (format "compile/run failed (~a): ~a" status output))))

;; test-vfs-tar-pax-fractional-mtime : -> void
;;   Check that local fractional pax mtimes are truncated to seconds.
(define (test-vfs-tar-pax-fractional-mtime)
  (define program
    #<<PROGRAM
(unless
 (and (equal? (file->string "/assets/time.txt") "time\n")
      (equal? (file-or-directory-modify-seconds "/assets/time.txt") 321))
 (error 'vfs-tar-mount "fractional pax mtime checks failed"))
PROGRAM
)
  (define-values (status output)
    (run-tar-program program #:tar-bytes (make-pax-fractional-mtime-tar)))
  (unless (zero? status)
    (error 'test-vfs-tar-pax-fractional-mtime
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

;; test-vfs-tar-mount-rejects-unsupported-type : -> void
;;   Check that unsupported non-link tar entry types fail while mounting.
(define (test-vfs-tar-mount-rejects-unsupported-type)
  (define-values (status output)
    (run-tar-program "(void)\n" #:tar-bytes (make-unsupported-type-tar)))
  (when (zero? status)
    (error 'test-vfs-tar-mount-rejects-unsupported-type
           "expected unsupported-type tar mount to fail"))
  (unless (regexp-match? #rx"VFS tar entry type is unsupported" output)
    (error 'test-vfs-tar-mount-rejects-unsupported-type
           (format "expected unsupported tar type failure, got: ~a" output))))

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

;; test-vfs-tar-mount-rejects-trailing-data : -> void
;;   Check that non-zero partial-block trailing data fails while mounting.
(define (test-vfs-tar-mount-rejects-trailing-data)
  (define-values (status output)
    (run-tar-program "(void)\n" #:tar-bytes (make-trailing-data-tar)))
  (when (zero? status)
    (error 'test-vfs-tar-mount-rejects-trailing-data
           "expected trailing-data tar mount to fail"))
  (unless (regexp-match? #rx"VFS tar trailing data is invalid" output)
    (error 'test-vfs-tar-mount-rejects-trailing-data
           (format "expected trailing tar data failure, got: ~a" output))))

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

;; test-vfs-tar-mount-rejects-invalid-checksum : -> void
;;   Check that corrupted tar header checksums fail while mounting.
(define (test-vfs-tar-mount-rejects-invalid-checksum)
  (define-values (status output)
    (run-tar-program "(void)\n" #:tar-bytes (make-invalid-checksum-tar)))
  (when (zero? status)
    (error 'test-vfs-tar-mount-rejects-invalid-checksum
           "expected invalid-checksum tar mount to fail"))
  (unless (regexp-match? #rx"VFS tar header checksum is invalid" output)
    (error 'test-vfs-tar-mount-rejects-invalid-checksum
           (format "expected invalid tar checksum failure, got: ~a" output))))

;; test-vfs-tar-mount-rejects-absolute-path : -> void
;;   Check that absolute archive entry names fail while mounting.
(define (test-vfs-tar-mount-rejects-absolute-path)
  (define-values (status output)
    (run-tar-program "(void)\n" #:tar-bytes (make-invalid-path-tar "/escape.txt")))
  (when (zero? status)
    (error 'test-vfs-tar-mount-rejects-absolute-path
           "expected absolute-path tar mount to fail"))
  (unless (regexp-match? #rx"VFS tar entry path is invalid" output)
    (error 'test-vfs-tar-mount-rejects-absolute-path
           (format "expected invalid tar path failure, got: ~a" output))))

;; test-vfs-tar-mount-rejects-parent-path : -> void
;;   Check that parent-directory archive entry names fail while mounting.
(define (test-vfs-tar-mount-rejects-parent-path)
  (define-values (status output)
    (run-tar-program "(void)\n" #:tar-bytes (make-invalid-path-tar "../escape.txt")))
  (when (zero? status)
    (error 'test-vfs-tar-mount-rejects-parent-path
           "expected parent-path tar mount to fail"))
  (unless (regexp-match? #rx"VFS tar entry path is invalid" output)
    (error 'test-vfs-tar-mount-rejects-parent-path
           (format "expected invalid tar path failure, got: ~a" output))))

;; test-vfs-tar-mount-rejects-absolute-pax-path : -> void
;;   Check that absolute pax path overrides fail while mounting.
(define (test-vfs-tar-mount-rejects-absolute-pax-path)
  (define-values (status output)
    (run-tar-program "(void)\n" #:tar-bytes (make-invalid-pax-path-tar "/escape.txt")))
  (when (zero? status)
    (error 'test-vfs-tar-mount-rejects-absolute-pax-path
           "expected absolute-pax-path tar mount to fail"))
  (unless (regexp-match? #rx"VFS tar entry path is invalid" output)
    (error 'test-vfs-tar-mount-rejects-absolute-pax-path
           (format "expected invalid tar path failure, got: ~a" output))))

;; test-vfs-tar-mount-rejects-parent-pax-path : -> void
;;   Check that parent-directory pax path overrides fail while mounting.
(define (test-vfs-tar-mount-rejects-parent-pax-path)
  (define-values (status output)
    (run-tar-program "(void)\n" #:tar-bytes (make-invalid-pax-path-tar "../escape.txt")))
  (when (zero? status)
    (error 'test-vfs-tar-mount-rejects-parent-pax-path
           "expected parent-pax-path tar mount to fail"))
  (unless (regexp-match? #rx"VFS tar entry path is invalid" output)
    (error 'test-vfs-tar-mount-rejects-parent-pax-path
           (format "expected invalid tar path failure, got: ~a" output))))

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

;; test-vfs-tar-mount-rejects-invalid-pax-size : -> void
;;   Check that malformed pax size records fail while mounting.
(define (test-vfs-tar-mount-rejects-invalid-pax-size)
  (define-values (status output)
    (run-tar-program "(void)\n" #:tar-bytes (make-invalid-pax-size-tar)))
  (when (zero? status)
    (error 'test-vfs-tar-mount-rejects-invalid-pax-size
           "expected invalid-pax-size tar mount to fail"))
  (unless (regexp-match? #rx"VFS tar pax size is invalid" output)
    (error 'test-vfs-tar-mount-rejects-invalid-pax-size
           (format "expected invalid tar pax size failure, got: ~a" output))))

;; test-vfs-tar-mount-rejects-invalid-pax-mtime : -> void
;;   Check that malformed pax mtime records fail while mounting.
(define (test-vfs-tar-mount-rejects-invalid-pax-mtime)
  (define-values (status output)
    (run-tar-program "(void)\n" #:tar-bytes (make-invalid-pax-mtime-tar)))
  (when (zero? status)
    (error 'test-vfs-tar-mount-rejects-invalid-pax-mtime
           "expected invalid-pax-mtime tar mount to fail"))
  (unless (regexp-match? #rx"VFS tar pax mtime is invalid" output)
    (error 'test-vfs-tar-mount-rejects-invalid-pax-mtime
           (format "expected invalid tar pax mtime failure, got: ~a" output))))

;; test-vfs-tar-mount-rejects-dangling-pax : -> void
;;   Check that local pax headers must have a following target entry.
(define (test-vfs-tar-mount-rejects-dangling-pax)
  (define-values (status output)
    (run-tar-program "(void)\n" #:tar-bytes (make-dangling-pax-tar)))
  (when (zero? status)
    (error 'test-vfs-tar-mount-rejects-dangling-pax
           "expected dangling-pax tar mount to fail"))
  (unless (regexp-match? #rx"VFS tar extension entry has no target" output)
    (error 'test-vfs-tar-mount-rejects-dangling-pax
           (format "expected dangling tar extension failure, got: ~a" output))))

;; test-vfs-tar-mount-rejects-dangling-long-name : -> void
;;   Check that GNU long-name records must have a following target entry.
(define (test-vfs-tar-mount-rejects-dangling-long-name)
  (define-values (status output)
    (run-tar-program "(void)\n" #:tar-bytes (make-dangling-long-name-tar)))
  (when (zero? status)
    (error 'test-vfs-tar-mount-rejects-dangling-long-name
           "expected dangling-long-name tar mount to fail"))
  (unless (regexp-match? #rx"VFS tar extension entry has no target" output)
    (error 'test-vfs-tar-mount-rejects-dangling-long-name
           (format "expected dangling tar extension failure, got: ~a" output))))

;; test-vfs-tar-mount-rejects-dangling-long-link : -> void
;;   Check that GNU long-link records must have a following target entry.
(define (test-vfs-tar-mount-rejects-dangling-long-link)
  (define-values (status output)
    (run-tar-program "(void)\n" #:tar-bytes (make-dangling-long-link-tar)))
  (when (zero? status)
    (error 'test-vfs-tar-mount-rejects-dangling-long-link
           "expected dangling-long-link tar mount to fail"))
  (unless (regexp-match? #rx"VFS tar extension entry has no target" output)
    (error 'test-vfs-tar-mount-rejects-dangling-long-link
           (format "expected dangling tar extension failure, got: ~a" output))))

;; vfs-tar-tests : (listof (cons/c symbol? (-> void?)))
;;   Generated-runtime tar tests; each case uses its own temporary directories.
(define vfs-tar-tests
  `((test-vfs-tar-mount . ,test-vfs-tar-mount)
    (test-vfs-tar-duplicate-file-last-wins . ,test-vfs-tar-duplicate-file-last-wins)
    (test-vfs-tar-explicit-directories . ,test-vfs-tar-explicit-directories)
    (test-vfs-tar-duplicate-directories . ,test-vfs-tar-duplicate-directories)
    (test-vfs-tar-mount-rejects-file-then-directory . ,test-vfs-tar-mount-rejects-file-then-directory)
    (test-vfs-tar-mount-rejects-directory-then-file . ,test-vfs-tar-mount-rejects-directory-then-file)
    (test-vfs-tar-file-mount . ,test-vfs-tar-file-mount)
    (test-vfs-tar-nested-mount . ,test-vfs-tar-nested-mount)
    (test-vfs-tar-synthetic-parent-mount . ,test-vfs-tar-synthetic-parent-mount)
    (test-vfs-tar-root-mount-list . ,test-vfs-tar-root-mount-list)
    (test-vfs-tar-pax-unicode-path . ,test-vfs-tar-pax-unicode-path)
    (test-vfs-tar-pax-size . ,test-vfs-tar-pax-size)
    (test-vfs-tar-pax-fractional-mtime . ,test-vfs-tar-pax-fractional-mtime)
    (test-vfs-tar-mount-global-pax-mtime . ,test-vfs-tar-mount-global-pax-mtime)
    (test-vfs-tar-mount-read-only . ,test-vfs-tar-mount-read-only)
    (test-vfs-tar-mount-rejects-links . ,test-vfs-tar-mount-rejects-links)
    (test-vfs-tar-mount-rejects-unsupported-type . ,test-vfs-tar-mount-rejects-unsupported-type)
    (test-vfs-tar-mount-rejects-truncated-entry . ,test-vfs-tar-mount-rejects-truncated-entry)
    (test-vfs-tar-mount-rejects-trailing-data . ,test-vfs-tar-mount-rejects-trailing-data)
    (test-vfs-tar-mount-rejects-invalid-size . ,test-vfs-tar-mount-rejects-invalid-size)
    (test-vfs-tar-mount-rejects-invalid-checksum . ,test-vfs-tar-mount-rejects-invalid-checksum)
    (test-vfs-tar-mount-rejects-absolute-path . ,test-vfs-tar-mount-rejects-absolute-path)
    (test-vfs-tar-mount-rejects-parent-path . ,test-vfs-tar-mount-rejects-parent-path)
    (test-vfs-tar-mount-rejects-absolute-pax-path . ,test-vfs-tar-mount-rejects-absolute-pax-path)
    (test-vfs-tar-mount-rejects-parent-pax-path . ,test-vfs-tar-mount-rejects-parent-pax-path)
    (test-vfs-tar-mount-rejects-invalid-pax . ,test-vfs-tar-mount-rejects-invalid-pax)
    (test-vfs-tar-mount-rejects-invalid-pax-size . ,test-vfs-tar-mount-rejects-invalid-pax-size)
    (test-vfs-tar-mount-rejects-invalid-pax-mtime . ,test-vfs-tar-mount-rejects-invalid-pax-mtime)
    (test-vfs-tar-mount-rejects-dangling-pax . ,test-vfs-tar-mount-rejects-dangling-pax)
    (test-vfs-tar-mount-rejects-dangling-long-name . ,test-vfs-tar-mount-rejects-dangling-long-name)
    (test-vfs-tar-mount-rejects-dangling-long-link . ,test-vfs-tar-mount-rejects-dangling-long-link)))

;; vfs-tar-test-worker-count : -> exact-positive-integer?
;;   Read the generated-runtime tar test worker count from the environment.
(define (vfs-tar-test-worker-count)
  (define raw (getenv "WEBRACKET_VFS_TAR_TEST_JOBS"))
  (cond
    [(not raw) 4]
    [(let ([n (string->number raw)])
       (and (exact-positive-integer? n) n))]
    [else
     (error 'vfs-tar-test-worker-count
            "expected WEBRACKET_VFS_TAR_TEST_JOBS to be a positive integer, got ~v"
            raw)]))

;; run-vfs-tar-tests : -> void?
;;   Run generated-runtime tar tests with bounded parallelism.
(define (run-vfs-tar-tests)
  (define worker-count (vfs-tar-test-worker-count))
  (define slots (make-semaphore worker-count))
  (define results (make-channel))
  (for ([test (in-list vfs-tar-tests)])
    (thread
     (lambda ()
       (define name (car test))
       (define thunk (cdr test))
       (semaphore-wait slots)
       (define failure
         (dynamic-wind
           void
           (lambda ()
             (with-handlers ([exn:fail? (lambda (ex) ex)])
               (thunk)
               #f))
           (lambda () (semaphore-post slots))))
       (channel-put results (cons name failure)))))
  (define failures
    (for/list ([_ (in-list vfs-tar-tests)]
               #:do [(define result (channel-get results))]
               #:when (cdr result))
      result))
  (unless (null? failures)
    (error 'run-vfs-tar-tests
           (string-join
            (for/list ([failure (in-list failures)])
              (format "~a: ~a" (car failure) (exn-message (cdr failure))))
            "\n"))))

(module+ test
  (run-vfs-tar-tests))

(module+ main
  (run-vfs-tar-tests)
  (displayln "ok"))
