#lang racket/base
(provide
 (rename-out [my-read        read]
             [my-read-syntax read-syntax]
             [my-get-info    get-info]))


(define (source-name->module-name sn)
  ; Example:
  ;   /Users/soegaard/Dropbox/GitHub/webracket/testing.rkt
  (cond
    [(path? sn)   (define-values (base name must-be-dir?) (split-path sn))  
                  (define mod (path->string (path-replace-extension name #"")))
                  (string->symbol mod)]
    [(symbol? sn) sn]
    [else         'anon]))


; Since read-syntax is supposed to return a syntax object without
; lexcial context, we use `strip-context`.
(define (my-read-syntax [source-name (object-name (current-input-port))]
                        [in          (current-input-port)])
  ;; 1. Read all forms after the #lang
  (define (read-one input-port) (read-syntax source-name in))
  (define forms    (for/list ([form (in-port read-one)]) form))
  (define mod-name (source-name->module-name source-name))
  (define mod      `(module ,mod-name webracket ,@forms))
  (define out      (strip-context (datum->syntax #f mod)))
  ; (displayln out (current-error-port))
  out)


; The procedure `read` returns the same as `read-syntax`, except as a datum.
(define (my-read [in (current-input-port)])
  (syntax->datum (my-read-syntax #f in)))


; The procedure `get-info` is in general used by external
; tools to retrieve information about a program.
; Here we use `get-info` to tell editors which lexer it should use to
; syntax color programs.
(define (my-get-info in mod line col pos)
  (lambda (key default)
    (case key
      [(color-lexer)
       (dynamic-require 'syntax-color/racket-lexer 'racket-lexer)]
      [else default])))


(require syntax/strip-context
         racket/string
         racket/list)
