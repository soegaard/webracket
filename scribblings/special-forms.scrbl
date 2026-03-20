#lang scribble/manual

@(require scribble/manual
          net/uri-codec
          racket/list
          (for-label (prefix-in racket: racket))
          "special-forms-index.rkt")

@title{Special Forms}
@declare-exporting[(lib "core.rkt" "webracket")]

This chapter documents special forms available in @tt{#lang webracket}.
Unless noted, behavior is intended to match the corresponding Racket form.

@(define scribble-ns (variable-reference->namespace (#%variable-reference)))
@(define webracket-specific
   '(require-lib include-lib #%app #%module-begin))
@(define (racket-doc-url name)
   (string-append "https://docs.racket-lang.org/search/index.html?q="
                  (uri-encode (symbol->string name))))
@(define (signature-datum name)
   (case name
     [(if) '(if test-expr then-expr else-expr)]
     [(cond) '(cond [test-expr body ...+] ... [else body ...+])]
     [(case) '(case val-expr [(datum ...) body ...+] ... [else body ...+])]
     [(and) '(and expr ...)]
     [(or) '(or expr ...)]
     [(when) '(when test-expr body ...+)]
     [(unless) '(unless test-expr body ...+)]
     [(lambda) '(lambda formals body ...+)]
     [(λ) '(λ formals body ...+)]
     [(case-lambda) '(case-lambda [formals body ...+] ...)]
     [(let) '(let ([id expr] ...) body ...+)]
     [(let*) '(let* ([id expr] ...) body ...+)]
     [(letrec) '(letrec ([id expr] ...) body ...+)]
     [(let-values) '(let-values ([(id ...) expr] ...) body ...+)]
     [(let*-values) '(let*-values ([(id ...) expr] ...) body ...+)]
     [(letrec-values) '(letrec-values ([(id ...) expr] ...) body ...+)]
     [(local) '(local [definition ...+] body ...+)]
     [(define) '(define head body ...+)]
     [(define-values) '(define-values (id ...) expr)]
     [(begin) '(begin expr ...+)]
     [(begin0) '(begin0 expr expr ...)]
     [(set!) '(set! id expr)]
     [(set!-values) '(set!-values (id ...) expr)]
     [(for) '(for ([id seq-expr] ...) body ...+)]
     [(for*) '(for* ([id seq-expr] ...) body ...+)]
     [(for/list) '(for/list ([id seq-expr] ...) body ...+)]
     [(for*/list) '(for*/list ([id seq-expr] ...) body ...+)]
     [(for/vector) '(for/vector ([id seq-expr] ...) body ...+)]
     [(for*/vector) '(for*/vector ([id seq-expr] ...) body ...+)]
     [(for/sum) '(for/sum ([id seq-expr] ...) body ...+)]
     [(for*/sum) '(for*/sum ([id seq-expr] ...) body ...+)]
     [(for/fold) '(for/fold ([acc init-expr] ...) ([id seq-expr] ...) body ...+)]
     [(for*/fold) '(for*/fold ([acc init-expr] ...) ([id seq-expr] ...) body ...+)]
     [(for/or) '(for/or ([id seq-expr] ...) body ...+)]
     [(for*/or) '(for*/or ([id seq-expr] ...) body ...+)]
     [(for/and) '(for/and ([id seq-expr] ...) body ...+)]
     [(for*/and) '(for*/and ([id seq-expr] ...) body ...+)]
     [(for/first) '(for/first ([id seq-expr] ...) body ...+)]
     [(for*/first) '(for*/first ([id seq-expr] ...) body ...+)]
     [(quasiquote) '(quasiquote datum)]
     [(unquote) '(unquote expr)]
     [(unquote-splicing) '(unquote-splicing expr)]
     [(with-handlers) '(with-handlers ([pred handler] ...) body ...+)]
     [(block) '(block expr ...+)]
     [(require-lib) '(require-lib lib-id)]
     [(include-lib) '(include-lib lib-id)]
     [(#%app) '(#%app f arg ...)]
     [(#%module-begin) '(#%module-begin form ...)]
     [else `(,name form ...)]))
@(define (webracket-note name)
   (case name
     [(require-lib)
      "Requires a WebRacket library by identifier. Valid only at module top level."]
     [(include-lib)
      "Includes a WebRacket library source file by identifier. Valid only at module top level."]
     [(#%app)
      "Application rewrites keyword tokens to keyword values so keyword-call surface syntax works in ordinary call positions."]
     [(#%module-begin)
      "Language-specific module-begin for #lang webracket."]
     [else #f]))
@(define (render-form-entry name)
   (define note (webracket-note name))
   (eval
    `(defform ,(signature-datum name)
       (para (bold "Racket docs:") " "
             (hyperlink ,(racket-doc-url name) (tt ,(symbol->string name))))
       ,@(if note
             `((para (bold "WebRacket note:") " " ,note))
             '()))
    scribble-ns))

@section{Core Forms}

@(append*
  (for/list ([name (in-list documented-special-forms)]
             #:unless (memq name webracket-specific))
    (list (render-form-entry name))))

@section{WebRacket-specific Forms}

@(append*
  (for/list ([name (in-list documented-special-forms)]
             #:when (memq name webracket-specific))
    (list (render-form-entry name))))
