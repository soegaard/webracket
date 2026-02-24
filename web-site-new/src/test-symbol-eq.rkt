;; Test whether string->symbol returns eq? symbols in WebRacket.

(define a (string->symbol "toc-key"))
(define b (string->symbol "toc-key"))
(define c (string->symbol "other-key"))

(define h (make-hasheq))
(hash-set! h a "ok")

(js-log (format "~a"
                (list
                 (list 'eq? (eq? a b))
                 (list 'eq?-diff (eq? a c))
                 (list 'has-key-a (hash-has-key? h a))
                 (list 'has-key-b (hash-has-key? h b)) 
                 (list 'has-key-c (hash-has-key? h c)))))

        


