#lang webracket
(define sxml->html
  (let ()
    (define (sxml-element? v)
      (and (pair? v)
           (symbol? (car v))))

    (define (escape-common s attribute-context?)
      (define n (string-length s))
      (let loop ([i 0] [acc ""])
        (if (= i n)
            acc
            (let* ([ch (string-ref s i)]
                   [piece
                    (cond
                      [(char=? ch #\&) "&amp;"]
                      [(char=? ch #\<) "&lt;"]
                      [(char=? ch #\>) "&gt;"]
                      [(and attribute-context? (char=? ch #\")) "&quot;"]
                      [(and attribute-context? (char=? ch #\')) "&#39;"]
                      [else (string ch)])])
              (loop (+ i 1) (string-append acc piece))))))

    (define (escape-text s)
      (escape-common s #f))

    (define (escape-attr s)
      (escape-common s #t))

    (define (render-attrs attrs)
      (if (null? attrs)
          ""
          (let ([a (car attrs)])
            (unless (and (pair? a)
                         (symbol? (car a))
                         (pair? (cdr a))
                         (null? (cddr a)))
              (error 'sxml->html "bad attribute form: ~a" a))
            (define key (symbol->string (car a)))
            (define raw (cadr a))
            (define value
              (cond
                [(string? raw) raw]
                [(number? raw) (number->string raw)]
                [(symbol? raw) (symbol->string raw)]
                [else
                 (error 'sxml->html "bad attribute value: ~a" raw)]))
            (string-append
             " "
             key
             "=\""
             (escape-attr value)
             "\""
             (render-attrs (cdr attrs))))))

    (define (render-children children)
      (string-append* (map render-node children)))

    (define (render-node v)
      (cond
        [(string? v) (escape-text v)]
        [(number? v) (number->string v)]
        [(symbol? v) (escape-text (symbol->string v))]
        [(pair? v)
         (define tag (car v))
         (unless (symbol? tag)
           (error 'sxml->html "expected tag symbol, got: ~a" tag))
         (define rest (cdr v))
         (define attrs '())
         (define children rest)
         (when (and (pair? rest)
                    (pair? (car rest))
                    (eq? '@ (caar rest)))
           (set! attrs (cdar rest))
           (set! children (cdr rest)))
         (string-append
          "<"
          (symbol->string tag)
          (render-attrs attrs)
          ">"
          (render-children children)
          "</"
          (symbol->string tag)
          ">")]
        [else
         (error 'sxml->html "unsupported SXML value: ~a" v)]))

    (define (sxml->html sxml)
      (cond
        [(sxml-element? sxml) (render-node sxml)]
        [(list? sxml)         (render-children sxml)]
        [else                 (render-node sxml)]))

    sxml->html))
