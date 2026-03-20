#lang webracket
(define sxml->html
  (let ()
    (define (sxml-element? v)
      (and (pair?        v)
           (symbol? (car v))))

    (define (tree->string t)
      (string-append* (flatten t)))
    
    ;; Convention: Function that end in * produces a tree of strings
    
    (define (escape-common s attribute-context?)
      (define ac attribute-context?)
      (for/list ([ch (in-string s)])
        (define s
          (case ch
            [(#\&) "&amp;"]
            [(#\<) "&lt;"]
            [(#\>) "&gt;"]
            [(#\") (and ac "&quot;")]
            [(#\') (and ac "&#39;")]
            [else  #f]))
        (or s (string ch))))

    (define (escape-text s)
      (escape-common s #f))

    (define (escape-attr s)
      (escape-common s #t))

    (define (render-attrs* attrs)
      (map render-attr* attrs))
    
    (define (render-attr* attr)
      (define a attr)
      (unless (and (pair? a)
                   (symbol? (car a))
                   (pair?   (cdr a))
                   (null?   (cddr a)))
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
      
      (list " " key "=\"" (escape-attr value) "\""))

    (define (render-children* children)
      (map render-node* children))

    (define (render-node* v)
      (cond
        [(string? v) (escape-text v)]
        [(number? v) (number->string v)]
        [(symbol? v) (escape-text (symbol->string v))]
        [(pair? v)   (define tag (car v))
                     (unless (symbol? tag)
                       (error 'sxml->html "expected tag symbol, got: ~a" tag))
                     (define rest     (cdr v))
                     (define attrs    '())
                     (define children rest)                     
                     (when (and (pair?       rest)
                                (pair?  (car rest))
                                (eq? '@ (caar rest)))
                       (set! attrs    (cdar rest))
                       (set! children (cdr  rest)))
                     (list "<" (symbol->string tag) (render-attrs* attrs) ">"
                           (render-children* children)
                           "</" (symbol->string tag) ">")]
        [else
         (error 'sxml->html "unsupported SXML value: ~a" v)]))
    
    (define (sxml->html sxml)
      (tree->string
       (cond
         [(sxml-element? sxml) (render-node* sxml)]
         [(list? sxml)         (render-children* sxml)]
         [else                 (render-node* sxml)])))

    sxml->html))

