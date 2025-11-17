#lang webracket

;; Just enough to run "regexp.rkt".

;; The implementation of regular expressions uses immutable hash tables.
;; We use immutable hash tables in form of Patricia trees (see "stdlib/intmap.rkt").

(define string?
  (let ()
    (λ (x)
      (js-log "works!")
      (string? x))))


(define-values (immutable-hash-ref ; to avoid name clash
                hash-iterate-first
                hash-iterate-key
                hash-iterate-value
                hash-iterate-pair
                hash-iterate-key+value
                hash-iterate-next
                hash-set
                hash
                hashalw
                hasheq
                hasheqv)
  (let ()
    (define (immutable-hash-ref ht key [failure-result #f])
      (cond
        [(intmap? ht) (intmap-ref ht key failure-result)]
        [else         (error 'immutable-hash-ref
                             "expected immutable hash table")]))
    
    (define (hash-iterate-first ht)
      (js-log "hash-iterate-first - A")
      (cond
        [(intmap? ht) (js-log "hash-iterate-first - B")
                      (intmap-iterate-first ht)]
        [(hash? ht)   (js-log "hash-iterate-first - D")
                      (error 'hash-iterate-first
                             "expected immutable hash table")]        
        [else         (js-log "hash-iterate-first - C")
                      (error 'hash-iterate-first
                             "expected immutable hash table")]))

    
    (define fail (list 'fail))
    
    (define hash-iterate-key
      (case-lambda
        [(ht pos)
         (hash-iterate-key ht pos fail)]
        [(ht pos bad-index-v)
         (unless (intmap? ht)
           (error 'hash-iterate-first "expected immutable hash table"))

         (define key  (intmap-iterate-key ht pos bad-index-v))

         (when (eq? key fail)
           (raise-arguments-error hash-iterate-key "no element at index"
                                  "index" pos))
         key]))

    (define hash-iterate-value
      (case-lambda
        [(ht pos)
         (hash-iterate-value ht pos fail)]
        [(ht pos bad-index-v)
         (unless (intmap? ht)
           (error 'hash-iterate-value "expected immutable hash table"))

         (define value (intmap-iterate-value ht pos bad-index-v))

         (when (eq? value fail)
           (raise-arguments-error hash-iterate-value "no element at index"
                                  "index" pos))
         value]))

    (define hash-iterate-pair
      (case-lambda
        [(ht pos)
         (hash-iterate-pair ht pos fail)]
        [(ht pos bad-index-v)
         (unless (intmap? ht)
           (error 'hash-iterate-pair "expected immutable hash table"))

         (define pair (intmap-iterate-pair ht pos fail))

         (cond
           [(eq? pair fail)
            (if (eq? bad-index-v fail)
                (raise-arguments-error hash-iterate-pair "no element at index"
                                       "index" pos)
                (cons bad-index-v bad-index-v))]
           [else pair])]))

    (define hash-iterate-key+value
      (case-lambda
        [(ht pos)
         (hash-iterate-key+value ht pos fail)]
        [(ht pos bad-index-v)
         (unless (intmap? ht)
           (error 'hash-iterate-key+value "expected immutable hash table"))

         (define pair (intmap-iterate-pair ht pos fail))

         (cond
           [(eq? pair fail)
            (if (eq? bad-index-v fail)
                (raise-arguments-error hash-iterate-key+value "no element at index"
                                       "index" pos)
                (values bad-index-v bad-index-v))]
           [else (values (car pair) (cdr pair))])]))

    (define (hash-iterate-next ht pos)
      (cond
        [(intmap? ht) (intmap-iterate-next ht pos)]
        [else         (error 'hash-iterate-next
                             "expected immutable hash table")]))

    (define (hash-set ht key v)
      (cond
        [(intmap? ht) (intmap-set ht key v)]
        [else         (error 'hash-set "expected immutable hash table")]))

    (define (build-immutable-hash who empty-hash args)
      (define (loop ht args)
        (cond
          [(null? args) ht]
          [(null? (cdr args))
           (raise-arguments-error who "expected an even number of arguments" "arguments" args)]
          [else
           (loop (hash-set ht (car args) (cadr args))
                 (cddr args))]))

      (loop empty-hash args))


    (define (hash    . args) (build-immutable-hash 'hash    empty-hash    args))
    (define (hashalw . args) (build-immutable-hash 'hashalw empty-hash    args))
    (define (hasheq  . args) (build-immutable-hash 'hasheq  empty-hasheq  args))
    (define (hasheqv . args) (build-immutable-hash 'hasheqv empty-hasheqv args))

    (values immutable-hash-ref
            hash-iterate-first
            hash-iterate-key
            hash-iterate-value
            hash-iterate-pair
            hash-iterate-key+value
            hash-iterate-next
            hash-set
            hash
            hashalw
            hasheq
            hasheqv
            )))
