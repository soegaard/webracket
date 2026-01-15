#lang racket/base
;;;
;;; exec-path-from-shell.rkt
;;;

;; Functions such as `system` and `system*` search for commands in the
;; search path of the current process. If a program is invoked from a
;; terminal the process inherits the value of the search path from
;; the shell. This allows calls such as `(system "node -v")` to work.

;; Alas, if we are in a GUI program such as DrRacket or Emacs on macOS,
;; then the current process does not inherit the standard shell
;; search path. Instead, a minimal search path is used.

;; Racket programs calling out to external program using `system` or
;; `system*` therefore behave differently, when used in the editor.

;; To fix this, the function `exec-path-from-shell-initialize` below
;; looks up the shell search path and installs in the current process,
;; such that `system` and `system*` can be used from the editor
;; without hard coding paths.

;; The search path for commands is called `exec-path` in the commands below.

;; I have tested `exec-path-from-shell-initialize` on both macOS and Ubuntu.
;; Report back if you encounter problems on other systems.


;;;
;;; Imports
;;;

(require racket/string
         racket/list
         racket/system
         racket/port
         racket/path
         racket/format
         file/sha1     ; for bytes->hex-string
         json)

;;;
;;; Exports
;;;

(provide
 ;; Initialize Racket’s process environment from the user’s login shell.
 ;; Intended for GUI-launched Racket so (system ...) sees the same PATH,
 ;; MANPATH, etc. as a terminal session.
 exec-path-from-shell-initialize

 ;; Import a single environment variable from the user’s login shell and
 ;; install it into Racket’s environment.  Returns the variable’s value
 ;; or #f if it is unset in the shell.
 exec-path-from-shell-copy-env

 ;; Import multiple environment variables from the user’s login shell and
 ;; install them into Racket’s environment.  Returns an alist mapping
 ;; variable names to their values (or #f if unset).
 exec-path-from-shell-copy-envs

 ;; Query a single environment variable from the user’s login shell without
 ;; modifying Racket’s current environment.  Returns the value or #f.
 exec-path-from-shell-getenv

 ;; Query multiple environment variables from the user’s login shell without
 ;; modifying Racket’s current environment.  Returns an alist mapping names
 ;; to values (or #f if unset).
 exec-path-from-shell-getenvs)


;; ----------------------------
;; Configuration knobs
;; ----------------------------

(define exec-path-from-shell-variables
  '("PATH" "MANPATH"))

(define exec-path-from-shell-warn-ms 500)

(define exec-path-from-shell-shell-name
  #f) ; if #f, use (getenv "SHELL") or error

(define exec-path-from-shell-arguments
  (let ([sh (or exec-path-from-shell-shell-name (getenv "SHELL"))])
    (cond [(and sh (regexp-match? #rx"t?csh$" sh)) '("-d")]
          [(and sh (regexp-match? #rx"fish" sh))   '("-l")]
          [else                                    '("-l")])))



;; ----------------------------
;; Byte String Utility
;; ----------------------------

;; bytes-split : bytes bytes -> (listof bytes)
;; Split BS on occurrences of SEP (a non-empty bytes), returning the pieces.
;; Includes empty pieces when SEP occurs at the beginning/end or repeatedly.
;;
;; Examples:
;;   (bytes-split #"a\0b\0c" (bytes 0))  => '(#"a" #"b" #"c")
;;   (bytes-split #"a\0\0b"  (bytes 0))  => '(#"a" #"" #"b")
;;   (bytes-split #"\0a\0"   (bytes 0))  => '(#"" #"a" #"")
;;   (bytes-split #"foo--bar--" #"--")   => '(#"foo" #"bar" #"")

(define (bytes-split bs sep)
  (unless (bytes? bs)
    (raise-argument-error 'bytes-split "bytes?" bs))
  (unless (bytes? sep)
    (raise-argument-error 'bytes-split "bytes?" sep))
  (when (zero? (bytes-length sep))
    (raise-argument-error 'bytes-split "non-empty bytes as separator" sep))

  ;; Make a byte-regexp that matches SEP literally.
  ;; - regexp-quote escapes regex metacharacters.
  ;; - byte->regexp: convert the (escaped) bytes to a byte regexp.
  (regexp-split (byte-regexp (regexp-quote sep)) bs))


(define (bytes-index-of haystack needle [start 0])
  (define hlen (bytes-length haystack))
  (define nlen (bytes-length needle))

  (cond
    [(< start 0)
     (raise-argument-error 'bytes-index-of "exact-nonnegative-integer?" start)]
    [(zero? nlen)
     start]
    [(= nlen 1)
     (define b (bytes-ref needle 0))
     (for/first ([i (in-range start hlen)]
                 #:when (= (bytes-ref haystack i) b))
       i)]
    [else
     (define limit (- hlen nlen))
     (for/first ([i (in-range start (add1 limit))]
                 #:when
                 (for/and ([j (in-range nlen)])
                   (= (bytes-ref haystack (+ i j))
                      (bytes-ref needle j))))
       i)]))

;; ----------------------------
;; Helpers
;; ----------------------------

(define (shell-to-use)
  (or exec-path-from-shell-shell-name
      (getenv "SHELL")
      (error 'exec-path-from-shell "SHELL environment variable is unset")))

(define (nushell? sh)
  (and sh (regexp-match? #rx"nu$" sh)))

(define (standard-shell? sh)
  ;; supports ${VAR-default}
  (not (regexp-match? #rx"(fish|nu|t?csh)$" sh)))

(define (now-ms)
  (current-inexact-monotonic-milliseconds))

(define (warn-duration start-ms)
  (define dur (- (now-ms) start-ms))
  (when (> dur exec-path-from-shell-warn-ms)
    (eprintf
     "Warning: exec-path-from-shell took ~ams; consider slimming shell init.\n"
     dur)))


(define (extract-between-markers out)
  ;; Expect: __RESULT\0 PAYLOAD \0__RESULT
  (define pre-null  (bytes-append #"__RESULT" (bytes 0)))
  (define post-null (bytes-append (bytes 0) #"__RESULT"))
  (define pre-text  #"__RESULT\\000")
  (define post-text #"\\000__RESULT")

  (define (extract pre post)
    (define i (bytes-index-of out pre 0))
    (and i
         (let ([j (bytes-index-of out post (+ i (bytes-length pre)))])
           (and j (subbytes out (+ i (bytes-length pre)) j)))))

  (define payload (or (extract pre-null post-null)
                      (extract pre-text post-text)))
  (unless payload
    (error 'exec-path-from-shell "Expected marker in output, got: ~s" out))
  payload)


(define (run-shell-c shell args)
  (define start (now-ms))
  (define argv  (append exec-path-from-shell-arguments args))

  ;; subprocess returns: proc, stdout-in, stdin-out, stderr-in
  (define-values (proc p-stdout p-stdin p-stderr)
    (apply subprocess #f #f #f shell argv))

  (when p-stdin (close-output-port p-stdin))

  (define out (if p-stdout (port->bytes p-stdout) #""))
  (define err (if p-stderr (port->bytes p-stderr) #""))

  (when p-stdout (close-input-port p-stdout))
  (when p-stderr (close-input-port p-stderr))

  (subprocess-wait proc)               
  (define ec (subprocess-status proc)) 

  (warn-duration start)

  (unless (and (integer? ec) (zero? ec))
    (error 'exec-path-from-shell
           "Non-zero exit code ~a from ~a with args ~s.\nstdout: ~s\nstderr: ~s"
           ec shell argv out err))
  out)


;; -------------------------------------
;; The "printf with NUL separators" path
;; -------------------------------------

(define (shell-printf fmt args)
  ;; fmt should contain sequences like "\\000" (one backslash in the string)
  ;; so that printf receives \000 and emits a NUL byte.
  (define printf-bin (or (find-executable-path "printf") "printf"))
  (define format-arg (string-append "__RESULT\\000" fmt "\\000__RESULT"))
  (define cmd
    (string-append
     (format "~a " (if (path? printf-bin) (path->string printf-bin) printf-bin))
     (format "~s " format-arg) ; quote as a single argv word for the shell
     (string-join (map (λ (s) (format "~s" s)) args) " ")))

  (define sh       (shell-to-use))
  (define full-cmd (if (standard-shell? sh)
                       cmd
                       ;; for fish/csh/nu: run a POSIX sh to execute the printf reliably,
                       ;; but still inherit env from the login shell.
                       (format "sh -c ~s" cmd)))

  (extract-between-markers
   (run-shell-c sh (list "-c" full-cmd))))

(define (getenvs-standard names)
  ;; Returns list of (name . value/#f) pairs.
  (define random-default (bytes->hex-string
                          (sha1-bytes
                           (string->bytes/utf-8
                            (format "~a~a~a"
                                    (current-process-milliseconds)
                                    (random)
                                    (current-seconds))))))
  (define dollar-names  (for/list ([n names])
                          (format "${~a-~a}" n random-default)))
  (define fmt           (string-join (make-list (length names) "%s") "\\000"))
  (define payload-bytes (shell-printf fmt dollar-names))
  (define values        (map bytes->string/utf-8
                             (bytes-split payload-bytes (bytes 0))))

  (unless (= (length values) (length names))
    (error 'exec-path-from-shell
           "Shell returned ~a values for ~a names.\nNames: ~s\nValues: ~s\nPayload bytes: ~s"
           (length values) (length names) names values payload-bytes))

  (for/list ([n names] [v values])
    (cons n (and (not (string=? v random-default)) v))))



;; ----------------------------
;; Nushell path (JSON)
;; ----------------------------

(define (getenvs-nushell names)
  ;; nu expression: [ $env."PATH"? , ... ] | to json
  (define sh   (shell-to-use))
  (define expr (format "[ ~a ] | to json"
                       (string-join
                        (for/list ([n names])
                          (format "$env.~s?" n))
                        ", ")))
  (define out  (run-shell-c sh (list "-c" expr)))
  (define js   (bytes->jsexpr out))

  ;; Nu may return list values for paths; join with ":".
  (define (normalize v)
    (cond [(eq? v 'null) #f]
          [(list? v) (string-join (map ~a v) ":")]
          [else (~a v)]))
  (reverse
   (for/fold ([acc '()]) ([n names] [v js])
     (cons (cons n (normalize v)) acc))))

;; ----------------------------
;; Public API
;; ----------------------------

(define (exec-path-from-shell-getenvs names)
  (define sh (shell-to-use))
  (if (nushell? sh)
      (getenvs-nushell names)
      (getenvs-standard names)))

(define (exec-path-from-shell-getenv name)
  (define p (assoc name (exec-path-from-shell-getenvs (list name))))
  (and p (cdr p)))

(define (exec-path-from-shell-setenv! name value)
  ;; Install into Racket's environment so (system ...) inherits it.
  (if value
      (putenv name value)
      (void)))

(define (exec-path-from-shell-copy-envs names)
  (define pairs (exec-path-from-shell-getenvs names))
  (for ([p pairs])
    (exec-path-from-shell-setenv! (car p) (cdr p)))
  pairs)

(define (exec-path-from-shell-copy-env name)
  (define pairs (exec-path-from-shell-copy-envs (list name)))
  (define p (assoc name pairs))
  (and p (cdr p)))

(define (exec-path-from-shell-initialize
         #:vars [vars exec-path-from-shell-variables])
  (exec-path-from-shell-copy-envs vars))
