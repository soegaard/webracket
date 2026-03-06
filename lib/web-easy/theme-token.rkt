#lang webracket

;;;
;;; web-easy Theme Tokens
;;;

;; Runtime token helpers for external CSS themes.
;;
;; Exports:
;;   theme-token-ref       Read token value from runtime token store.
;;   theme-token-set!      Set one token value and apply CSS variable in browser when available.
;;   theme-token-set-many! Set many token values from an alist.
;;   theme-token-install-applier!  Install optional token-application hook.

(define-values
  (theme-token-ref
   theme-token-set!
   theme-token-set-many!
   theme-token-install-applier!)
  (let ()
    ;; Constants for theme token handling.
    (define token-state '()) ; Association list (symbol . value) with latest token values.
    (define token-applier #f) ; Optional callback applied after token updates.

    ;; token-name->symbol : (or/c symbol? string?) -> symbol?
    ;;   Normalize token name to symbol form.
    (define (token-name->symbol name)
      (cond
        [(symbol? name) name]
        [(string? name) (string->symbol name)]
        [else (string->symbol "token")]))

    ;; token-value->css-string : any/c -> string?
    ;;   Convert token value to string form.
    (define (token-value->css-string v)
      (cond
        [(string? v) v]
        [(symbol? v) (symbol->string v)]
        [(number? v) (number->string v)]
        [(boolean? v) (if v "true" "false")]
        [else "#<value>"]))

    ;; token-state-set! : symbol? any/c -> void?
    ;;   Update token-state with token => value.
    (define (token-state-set! token value)
      (set! token-state
            (cons (cons token value)
                  (let loop ([pairs token-state])
                    (cond
                      [(null? pairs) '()]
                      [(eq? (caar pairs) token) (cdr pairs)]
                      [else (cons (car pairs) (loop (cdr pairs)))])))))

    ;; maybe-apply-token! : symbol? any/c -> void?
    ;;   Call optional token-applier callback when installed.
    (define (maybe-apply-token! token value)
      (when token-applier
        (token-applier token value)))

    ;; theme-token-install-applier! : (or/c (-> symbol? any/c void?) false/c) -> void?
    ;;   Install or clear the token application callback used by theme-token-set!.
    (define (theme-token-install-applier! applier)
      (set! token-applier applier)
      (void))

    ;; theme-token-ref : (or/c symbol? string?) [any/c] -> any/c
    ;;   Return token value from runtime store; default when not present.
    ;;   Optional parameter default-value defaults to #f.
    (define (theme-token-ref token-name [default-value #f])
      (define token (token-name->symbol token-name))
      (define pair (assq token token-state))
      (if pair
          (cdr pair)
          default-value))

    ;; theme-token-set! : (or/c symbol? string?) any/c -> void?
    ;;   Set token value and apply CSS variable in browser mode.
    (define (theme-token-set! token-name value)
      (define token (token-name->symbol token-name))
      (token-state-set! token value)
      (maybe-apply-token! token (token-value->css-string value))
      (void))

    ;; theme-token-set-many! : list? -> void?
    ;;   Set many token values from (token . value) entries.
    (define (theme-token-set-many! entries)
      (let loop ([rest entries])
        (unless (null? rest)
          (define entry (car rest))
          (when (pair? entry)
            (theme-token-set! (car entry) (cdr entry)))
          (loop (cdr rest))))
      (void))

    (values theme-token-ref
            theme-token-set!
            theme-token-set-many!
            theme-token-install-applier!)))
