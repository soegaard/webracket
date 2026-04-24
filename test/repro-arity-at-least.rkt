(list
 (arity-at-least? (arity-at-least 2))
 (arity-at-least? (make-arity-at-least 2))
 (equal? (arity-at-least-value (arity-at-least 2)) 2))
