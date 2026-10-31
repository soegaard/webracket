#lang racket

(provide (struct-out top-level-initializer)
         batch-top-level-initializers
         initializer-batch-function-forms)

(require "entry-support.rkt")

;; A complete generated top-level form and its private local bindings.
(struct top-level-initializer (body locals) #:transparent)

;; initializer-expression-count : datum? -> exact-nonnegative-integer?
;;   Count folded expressions as a cheap estimate of generated code size.
(define (initializer-expression-count form)
  (if (pair? form)
      (add1 (for/sum ([part (in-list form)])
              (initializer-expression-count part)))
      0))

;; batch-top-level-initializers : (listof top-level-initializer?)
;;   [#:max-forms positive-integer?] [#:max-locals positive-integer?]
;;   [#:max-expressions positive-integer?] -> (listof (listof top-level-initializer?))
;;   Group adjacent initializers without splitting or reordering any initializer.
;; Optional limits default to 128 forms, 256 locals and 8000 folded expressions.
;; A single initializer above a limit gets a batch of its own.
(define (batch-top-level-initializers initializers
                                      #:max-forms [max-forms 128]
                                      #:max-locals [max-locals 256]
                                      #:max-expressions [max-expressions 8000])
  (define batches '())
  (define batch '())
  (define form-count 0)
  (define local-count 0)
  (define expression-count 0)
  ;; flush! : -> void?
  ;;   Finish the current batch and reset its size counters.
  (define (flush!)
    (when (pair? batch)
      (set! batches (cons (reverse batch) batches)))
    (set! batch '())
    (set! form-count 0)
    (set! local-count 0)
    (set! expression-count 0))
  (for ([initializer (in-list initializers)])
    (define locals (length (top-level-initializer-locals initializer)))
    (define expressions
      (initializer-expression-count (top-level-initializer-body initializer)))
    (when (and (pair? batch)
               (or (>= form-count max-forms)
                   (> (+ local-count locals) max-locals)
                   (> (+ expression-count expressions) max-expressions)))
      (flush!))
    (set! batch (cons initializer batch))
    (set! form-count (add1 form-count))
    (set! local-count (+ local-count locals))
    (set! expression-count (+ expression-count expressions)))
  (flush!)
  (reverse batches))

;; initializer-batch-function-forms : (listof (listof top-level-initializer?))
;;   -> (values (listof datum?) (listof datum?))
;;   Emit private, argument-free helpers and their ordered call sequence.
(define (initializer-batch-function-forms batches)
  (define names
    (for/list ([_ (in-list batches)] [i (in-naturals)])
      (string->symbol (format "$entry-initialize~a" i))))
  (values
   (for/list ([batch (in-list batches)] [name (in-list names)])
     `(func ,name
            ,@(append-map
               (lambda (initializer)
                 (entry-local-declaration-forms (top-level-initializer-locals initializer)))
               batch)
            ,@(append-map
               (lambda (initializer)
                 (entry-local-initializer-forms (top-level-initializer-locals initializer)))
               batch)
            ,@(map top-level-initializer-body batch)))
   (for/list ([name (in-list names)]) `(call ,name))))

(module+ test
  (require rackunit)

  ;; sample : natural? natural? natural? -> top-level-initializer?
  ;;   Build distinct initializers with independently controlled size and locals.
  (define (sample id locals expressions)
    (top-level-initializer
     `(block ,@(make-list expressions `(call ,(string->symbol (format "$step~a" id)))))
     (for/list ([i (in-range locals)])
       (list (string->symbol (format "$local~a-~a" id i)) 'i32 '(i32.const 0)))))

  (define initializers (list (sample 0 1 1) (sample 1 2 2) (sample 2 1 1)))
  (check-equal? (batch-top-level-initializers '()) '())
  (check-equal? (batch-top-level-initializers initializers) (list initializers))
  (for ([batches (in-list
                  (list (batch-top-level-initializers initializers #:max-forms 2)
                        (batch-top-level-initializers initializers #:max-locals 3)
                        (batch-top-level-initializers initializers #:max-expressions 5)))])
    (check-equal? (map length batches) '(2 1))
    (check-equal? (append* batches) initializers))

  (define oversized (sample 1 300 9000))
  (define with-oversized (list (first initializers) oversized (last initializers)))
  (check-equal? (batch-top-level-initializers with-oversized)
                (map list with-oversized))

  (define-values (functions calls)
    (initializer-batch-function-forms
     (batch-top-level-initializers initializers #:max-forms 2)))
  (check-equal? calls '((call $entry-initialize0) (call $entry-initialize1)))
  (check-equal?
   (first functions)
   '(func $entry-initialize0
          (local $local0-0 i32) (local $local1-1 i32) (local $local1-0 i32)
          (local.set $local0-0 (i32.const 0))
          (local.set $local1-0 (i32.const 0))
          (local.set $local1-1 (i32.const 0))
          (block (call $step0))
          (block (call $step1) (call $step1))))
  (check-equal?
   (second functions)
   '(func $entry-initialize1
          (local $local2-0 i32)
          (local.set $local2-0 (i32.const 0))
          (block (call $step2)))))
