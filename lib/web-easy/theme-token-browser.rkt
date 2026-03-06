#lang webracket

;;;
;;; web-easy Theme Tokens (Browser)
;;;

;; Browser-only token applier for theme-token API.

(define-values
  ()
  (let ()
    ;; extern-nullish? : any/c -> boolean?
    ;;   Check whether external value encodes null or undefined.
    (define (extern-nullish? v)
      (define s (js-value->string v))
      (or (string=? s "null")
          (string=? s "undefined")))

    ;; token-symbol->css-name : symbol? -> string?
    ;;   Convert token symbol to CSS custom-property name.
    (define (token-symbol->css-name token)
      (define s (symbol->string token))
      (if (and (> (string-length s) 2)
               (char=? (string-ref s 0) #\-)
               (char=? (string-ref s 1) #\-))
          s
          (string-append "--" s)))

    ;; apply-token! : symbol? string? -> void?
    ;;   Apply one token to html style as CSS custom-property.
    (define (apply-token! token value)
      (define body* (js-document-body))
      (unless (or (not body*)
                  (extern-nullish? body*))
        (define html* (js-ref/extern body* "parentElement"))
        (unless (or (not html*)
                    (extern-nullish? html*))
          (define style* (js-ref/extern html* "style"))
          (unless (or (not style*)
                      (extern-nullish? style*))
            (js-send/extern/nullish
             style*
             "setProperty"
             (vector (token-symbol->css-name token)
                     value))))))

    (theme-token-install-applier! apply-token!)
    (values)))
