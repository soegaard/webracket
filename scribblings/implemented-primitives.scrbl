#lang scribble/manual

@(require scribble/manual
          net/uri-codec
          racket/list
          (for-label (lib "scribblings/primitives-labels.rkt" "webracket"))
          (for-label (prefix-in racket: racket))
          (only-in (lib "scribblings/primitives-labels.rkt" "webracket")
                   implemented-primitives))

@title{Implemented Primitives}
@declare-exporting[(lib "scribblings/primitives-labels.rkt" "webracket")]

This chapter lists the primitives currently implemented in WebRacket.

No extra per-primitive documentation is included here. Primitive behavior is
intended to match Racket; use the Racket Reference for semantics.

@section{Compact Index}

@(define scribble-ns (variable-reference->namespace (#%variable-reference)))
@(define (racket-doc-id name)
   (string->symbol (symbol->string name)))
@(define (racket-doc-url name)
   (string-append "https://docs.racket-lang.org/search/index.html?q="
                  (uri-encode (symbol->string name))))
@(define (render-primitive name)
   (eval `(hyperlink ,(racket-doc-url name)
                     (racket ,(racket-doc-id name)))
         scribble-ns))
@(define (render-primitive-defthing name)
   (eval `(defthing ,(racket-doc-id name) any/c
            (list "Racket docs: "
                  (hyperlink ,(racket-doc-url name)
                             (tt ,(symbol->string name)))))
         scribble-ns))
@(define (alpha-initial? s)
   (and (positive? (string-length s))
        (let ([c (string-ref s 0)])
          (char-alphabetic? c))))
@(define (bucket-key sym)
   (define s (symbol->string sym))
   (if (alpha-initial? s)
       (string-upcase (string (string-ref s 0)))
       "*"))
@(define grouped
   (for/fold ([h (hash)]) ([name (in-list implemented-primitives)])
     (define k (bucket-key name))
     (hash-update h k (lambda (xs) (append xs (list name))) '())))
@(define present-alpha
   (filter (lambda (k) (not (string=? k "*")))
           (sort (hash-keys grouped) string<?)))
@(define ordered-keys
   (if (hash-has-key? grouped "*")
       (cons "*" present-alpha)
       present-alpha))
@(define (header-text k)
   (if (string=? k "*") "Special Names" k))
@(define (render-group k)
   (define names (hash-ref grouped k))
   (list
    (para (bold (header-text k)))
    (para (add-between (for/list ([name (in-list names)])
                         (render-primitive name))
                       " "))
    (para "")))

@(append* (for/list ([k (in-list ordered-keys)])
            (render-group k)))

@section{Complete Binding List}

Each WebRacket primitive is listed below with a documentation entry.

@(append*
  (for/list ([name (in-list implemented-primitives)])
    (list (render-primitive-defthing name))))
