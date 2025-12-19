#lang racket/base

;; exec-path-from-shell.rkt
;;
;; Import env vars from your login shell, so (system ...) sees the same
;; PATH etc. as your terminal.

(require racket/string
         racket/list
         racket/system
         racket/port
         racket/path
         racket/format
         file/sha1     ; for bytes->hex-string
         json)

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
  (define pre  (bytes-append #"__RESULT" (bytes 0)))
  (define post (bytes-append (bytes 0) #"__RESULT"))
  (define i    (bytes-index-of out pre 0))
  (unless i
    (error 'exec-path-from-shell "Expected marker in output, got: ~s" out))
  (define j (bytes-index-of out post (+ i (bytes-length pre))))
  (unless j
    (error 'exec-path-from-shell "Expected end marker in output, got: ~s" out))
  (subbytes out (+ i (bytes-length pre)) j))


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
  ;; If value is #f, remove the var (mirrors “unset”).
  (if value
      (putenv name value)
      (putenv name #f)))

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
