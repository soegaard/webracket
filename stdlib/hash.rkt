#lang webracket

;; Just enough to run "regexp.rkt".

;; The implementation of regular expressions uses immutable hash tables.
;; We use immutable hash tables in form of Patricia trees (see "stdlib/intmap.rkt").

(define-values (immutable-hash-ref ; to avoid name clash
                hash-iterate-first
                hash-iterate-key
                hash-iterate-next
                hash-set)
  (let ()
    (define (immutable-hash-ref ht key [failure-result #f])
      (cond
        [(intmap? ht) (intmap-ref ht key failure-result)]
        [else         (error 'immutable-hash-ref
                             "expected immutable hash table")]))
    
    (define (hash-iterate-first ht)
      (cond
        [(intmap? ht) (intmap-iterate-first ht)]
        [else         (error 'hash-iterate-first
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

    (define (hash-iterate-next ht pos)
      (cond
        [(intmap? ht) (intmap-iterate-next ht pos)]
        [else         (error 'hash-iterate-next
                             "expected immutable hash table")]))

    (define (hash-set ht key v)
      (cond
        [(intmap? ht) (intmap-set ht key v)]
        [else         (error 'hash-set "expected immutable hash table")]))

    (values immutable-hash-ref
            hash-iterate-first
            hash-iterate-key
            hash-iterate-next
            hash-set)))
