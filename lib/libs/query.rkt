#lang webracket

;;;
;;; Query wrappers
;;;

(include-lib document)
(include-lib element)
(require-lib query-chain)

;; safe-list-ref : (listof any/c) exact-nonnegative-integer? -> (or/c #f any/c)
;;   Return the list element at i when it is in range, otherwise #f.
(define (safe-list-ref xs i)
  (define n (length xs))
  (if (and (number? i) (exact? i) (integer? i)
           (<= 0 i) (< i n))
      (list-ref xs i)
      #f))

;; safe-vector-ref : vector? exact-nonnegative-integer? -> (or/c #f any/c)
;;   Return the vector element at i when it is in range, otherwise #f.
(define (safe-vector-ref xs i)
  (define n (vector-length xs))
  (if (and (number? i) (exact? i) (integer? i)
           (<= 0 i) (< i n))
      (vector-ref xs i)
      #f))

;; $selection : (struct (elements)) -> selection?
;;   Represent a collection of DOM elements.
(struct $selection (elements) #:transparent)

;; check-$selection : symbol? any/c -> void?
;;   Ensure x is a selection wrapper.
(define (check-$selection who x)
  (unless ($selection? x)
    (raise-argument-error who "$selection?" x)))

;; list->$selection : (listof any/c) -> $selection?
;;   Pack a list into a selection wrapper.
(define (list->$selection xs)
  ($selection (list->vector xs)))

;; vector->$selection : vector? -> $selection?
;;   Copy a vector into a selection wrapper.
(define (vector->$selection xs)
  ($selection (vector-copy xs)))

;; make-$selection : (or/c list? vector? dom-node-list?) -> $selection?
;;   Normalize supported selection sources into a wrapper.
(define (make-$selection xs)
  (cond
    [(dom-node-list? xs) ($selection xs)]
    [(vector? xs)    (vector->$selection xs)]
    [(list? xs)      (list->$selection xs)]
    [else
     (raise-argument-error
      'make-$selection
      "(or/c dom-node-list? vector? list?)"
      xs)]))

;; $length : $selection? -> exact-nonnegative-integer?
;;   Return the number of wrapped elements.
(define ($length sel)
  (check-$selection '$length sel)
  (define xs ($selection-elements sel))
  (cond
    [(vector? xs)    (vector-length xs)]
    [(dom-node-list? xs) (node-list-length xs)]
    [else
     (error '$length
            "internal error: unexpected elements payload: ~a"
            xs)]))

;; $ref : $selection? exact-nonnegative-integer? -> (or/c #f any/c)
;;   Return the selected element at i, or #f when i is out of range.
(define ($ref sel i)
  (check-$selection '$ref sel)
  (define xs ($selection-elements sel))
  (cond
    [(vector? xs)    (safe-vector-ref xs i)]
    [(dom-node-list? xs)
     (define node (node-list-item xs i))
     (and node (element-wrap (dom-node-raw node)))]
    [else
     (error '$ref
            "internal error: unexpected elements payload: ~a"
            xs)]))

;; $first : $selection? -> (or/c #f any/c)
;;   Return the first selected element, if any.
(define ($first sel)
  (check-$selection '$first sel)
  ($ref sel 0))

;; .first : $selection? -> (or/c #f any/c)
;;   Chainable alias for $first.
(define (.first sel)
  ($first sel))

;; $selection->vector : $selection? -> vector?
;;   Convert a selection wrapper into a vector of raw elements.
(define ($selection->vector sel)
  (check-$selection '$selection->vector sel)
  (define xs ($selection-elements sel))
  (cond
    [(vector? xs)    xs]
    [(dom-node-list? xs)
     (define n (node-list-length xs))
     (for/vector #:length n ([i (in-range n)])
       ($ref sel i))]
    [else
     (error '$selection->vector
            "internal error: unexpected elements payload: ~a"
            xs)]))

;; $vector : $selection? -> vector?
;;   Convert a selection wrapper into a vector of wrapped elements.
(define ($vector sel)
  (check-$selection '$vector sel)
  ($selection->vector sel))

;; .vector : $selection? -> vector?
;;   Chainable alias for $vector.
(define (.vector sel)
  ($vector sel))

;; $list : $selection? -> list?
;;   Convert a selection wrapper into a list of wrapped elements.
(define ($list sel)
  (check-$selection '$list sel)
  (vector->list ($selection->vector sel)))

;; .list : $selection? -> list?
;;   Chainable alias for $list.
(define (.list sel)
  ($list sel))

;; $for-each : (any/c -> any/c) $selection? -> $selection?
;;   Apply f to each wrapped element and return the original selection.
(define ($for-each f sel)
  (check-$selection '$for-each sel)
  (for ([x (in-vector ($selection->vector sel))])
    (f x))
  sel)

;; $map : (any/c -> any/c) $selection? -> $selection?
;;   Map f over the wrapped elements and return a new selection.
(define ($map f sel)
  (check-$selection '$map sel)
  (define n ($length sel))
  (vector->$selection
   (for/vector #:length n
               ([x (in-vector ($selection->vector sel))])
     (f x))))

;; .map : $selection? (any/c -> any/c) -> $selection?
;;   Chainable alias for $map.
(define (.map sel f)
  ($map f sel))

;; .for-each : $selection? (any/c -> any/c) -> $selection?
;;   Chainable alias for $for-each.
(define (.for-each sel f)
  ($for-each f sel))

;; $filter : (any/c -> any/c) $selection? -> $selection?
;;   Keep only the selected elements for which f returns true.
(define ($filter f sel)
  (check-$selection '$filter sel)
  (vector->$selection
   (for/list ([x (in-vector ($selection->vector sel))]
              #:when (f x))
     x)))

;; .filter : $selection? (any/c -> any/c) -> $selection?
;;   Chainable alias for $filter.
(define (.filter sel f)
  ($filter f sel))

;; $where : (any/c -> any/c) $selection? -> $selection?
;;   Alias for $filter with a selector-style name.
(define ($where f sel)
  ($filter f sel))

;; .where : $selection? (any/c -> any/c) -> $selection?
;;   Chainable alias for $where.
(define (.where sel f)
  ($where f sel))

;; $not : (any/c -> any/c) $selection? -> $selection?
;;   Keep only the selected elements for which f returns false.
(define ($not f sel)
  (check-$selection '$not sel)
  (vector->$selection
   (for/list ([x (in-vector ($selection->vector sel))]
              #:when (not (f x)))
     x)))

;; .not : $selection? (any/c -> any/c) -> $selection?
;;   Chainable alias for $not.
(define (.not sel f)
  ($not f sel))

;; $select : string? -> $selection?
;;   Query the document for matching elements.
(define ($select s)
  (unless (string? s)
    (raise-argument-error '$select "string?" s))
  (define nl (document-query-selector-all s))
  (make-$selection (or nl '())))

;; $ : string? -> $selection?
;;   Public jQuery-style selector alias.
(define ($ s)
  ($select s))

;; .text : $selection? -> string?
;;   Read the text content of the first selected element.
(define (.text sel)
  (check-$selection '.text sel)
  (define x ($first sel))
  (or (and x (element-text-content x))
      ""))

;; $attr : $selection? (or/c string? symbol?) -> (or/c #f string?)
;;   Read the named attribute from the first selected element.
(define ($attr sel name)
  (check-$selection '$attr sel)
  (define x ($first sel))
  (and x (element-get-attribute x name)))

;; .attr : $selection? (or/c string? symbol?) -> (or/c #f string?)
;;   Chainable alias for $attr.
(define (.attr sel name)
  ($attr sel name))

;; $has-attr? : $selection? (or/c string? symbol?) -> boolean?
;;   Report whether the first selected element has the named attribute.
(define ($has-attr? sel name)
  (check-$selection '$has-attr? sel)
  (define x ($first sel))
  (and x (element-has-attribute? x name)))

;; .has-attr? : $selection? (or/c string? symbol?) -> boolean?
;;   Chainable alias for $has-attr?.
(define (.has-attr? sel name)
  ($has-attr? sel name))

;; $class-list : $selection? -> (or/c #f dom-token-list?)
;;   Return the class list for the first selected element.
(define ($class-list sel)
  (check-$selection '$class-list sel)
  (define x ($first sel))
  (and x (element-class-list x)))

;; .class-list : $selection? -> (or/c #f dom-token-list?)
;;   Chainable alias for $class-list.
(define (.class-list sel)
  ($class-list sel))

;; $has-class? : $selection? (or/c string? symbol?) -> boolean?
;;   Report whether the first selected element has the named class.
(define ($has-class? sel class-name)
  (check-$selection '$has-class? sel)
  (define class-list ($class-list sel))
  (and class-list (dom-token-list-contains? class-list class-name)))

;; .has-class? : $selection? (or/c string? symbol?) -> boolean?
;;   Chainable alias for $has-class?.
(define (.has-class? sel class-name)
  ($has-class? sel class-name))

;; $add-class! : $selection? (or/c string? symbol?) ... -> $selection?
;;   Add one or more class tokens to the first selected element.
(define ($add-class! sel class-name . more-class-names)
  (check-$selection '$add-class! sel)
  (define x ($first sel))
  (when x
    (define class-list (element-class-list x))
    (when class-list
      (apply dom-token-list-add! class-list class-name more-class-names)))
  sel)

;; .add-class! : $selection? (or/c string? symbol?) ... -> $selection?
;;   Chainable alias for $add-class!.
(define (.add-class! sel class-name . more-class-names)
  (apply $add-class! sel class-name more-class-names))

;; $remove-class! : $selection? (or/c string? symbol?) ... -> $selection?
;;   Remove one or more class tokens from the first selected element.
(define ($remove-class! sel class-name . more-class-names)
  (check-$selection '$remove-class! sel)
  (define x ($first sel))
  (when x
    (define class-list (element-class-list x))
    (when class-list
      (apply dom-token-list-remove! class-list class-name more-class-names)))
  sel)

;; .remove-class! : $selection? (or/c string? symbol?) ... -> $selection?
;;   Chainable alias for $remove-class!.
(define (.remove-class! sel class-name . more-class-names)
  (apply $remove-class! sel class-name more-class-names))

;; $find : $selection? (or/c string? symbol?) -> $selection?
;;   Find descendant matches for each selected element and return a new selection.
(define ($find sel selector)
  (check-$selection '$find sel)
  (define found
    (for/fold ([acc '()])
              ([node (in-vector ($selection->vector sel))])
      (define matches (element-query-selector-all node selector))
      (append acc
              (for/list ([i (in-range (node-list-length matches))])
                (element-wrap (dom-node-raw (node-list-item matches i)))))))
  (list->$selection found))

;; .find : $selection? (or/c string? symbol?) -> $selection?
;;   Chainable alias for $find.
(define (.find sel selector)
  ($find sel selector))

;; $children : $selection? -> $selection?
;;   Return a selection of the child elements of each selected element.
(define ($children sel)
  (check-$selection '$children sel)
  (define kids
    (for/fold ([acc '()])
              ([node (in-vector ($selection->vector sel))])
      (define children (element-children node))
      (append acc
              (for/list ([i (in-range (html-collection-length children))])
                (html-collection-item children i)))))
  (list->$selection kids))

;; .children : $selection? -> $selection?
;;   Chainable alias for $children.
(define (.children sel)
  ($children sel))

;; $parent : $selection? -> $selection?
;;   Return a selection of the parent elements of each selected element.
(define ($parent sel)
  (check-$selection '$parent sel)
  (define parents
    (for/list ([node (in-vector ($selection->vector sel))]
               #:when (element-parent-element node))
      (element-parent-element node)))
  (list->$selection parents))

;; .parent : $selection? -> $selection?
;;   Chainable alias for $parent.
(define (.parent sel)
  ($parent sel))

;; $closest : $selection? (or/c string? symbol?) -> $selection?
;;   Return the closest matching ancestor for each selected element.
(define ($closest sel selector)
  (check-$selection '$closest sel)
  (define matches
    (for/list ([node (in-vector ($selection->vector sel))]
               #:when (element-closest node selector))
      (element-closest node selector)))
  (list->$selection matches))

;; .closest : $selection? (or/c string? symbol?) -> $selection?
;;   Chainable alias for $closest.
(define (.closest sel selector)
  ($closest sel selector))

;; $next : $selection? -> $selection?
;;   Return the next element sibling of each selected element.
(define ($next sel)
  (check-$selection '$next sel)
  (define nexts
    (for/list ([node (in-vector ($selection->vector sel))]
               #:when (element-next-element-sibling node))
      (element-next-element-sibling node)))
  (list->$selection nexts))

;; .next : $selection? -> $selection?
;;   Chainable alias for $next.
(define (.next sel)
  ($next sel))

;; $prev : $selection? -> $selection?
;;   Return the previous element sibling of each selected element.
(define ($prev sel)
  (check-$selection '$prev sel)
  (define prevs
    (for/list ([node (in-vector ($selection->vector sel))]
               #:when (element-previous-element-sibling node))
      (element-previous-element-sibling node)))
  (list->$selection prevs))

;; .prev : $selection? -> $selection?
;;   Chainable alias for $prev.
(define (.prev sel)
  ($prev sel))

;; $siblings : $selection? -> $selection?
;;   Return the element siblings surrounding each selected element.
(define ($siblings sel)
  (check-$selection '$siblings sel)
  (define siblings
    (for/fold ([acc '()])
              ([node (in-vector ($selection->vector sel))])
      (define prevs
        (let loop ([current (element-previous-element-sibling node)] [acc '()])
          (if current
              (loop (element-previous-element-sibling current)
                    (cons current acc))
              acc)))
      (define nexts
        (let loop ([current (element-next-element-sibling node)] [acc '()])
          (if current
              (loop (element-next-element-sibling current)
                    (append acc (list current)))
              acc)))
      (append acc prevs nexts)))
  (list->$selection siblings))

;; .siblings : $selection? -> $selection?
;;   Chainable alias for $siblings.
(define (.siblings sel)
  ($siblings sel))

;; $last : $selection? -> (or/c #f any/c)
;;   Return the last selected element, if any.
(define ($last sel)
  (check-$selection '$last sel)
  (define n ($length sel))
  (if (zero? n)
      #f
      ($ref sel (sub1 n))))

;; .last : $selection? -> (or/c #f any/c)
;;   Chainable alias for $last.
(define (.last sel)
  ($last sel))

;; $empty? : $selection? -> boolean?
;;   Report whether the selection contains no elements.
(define ($empty? sel)
  (check-$selection '$empty? sel)
  (zero? ($length sel)))

;; .empty? : $selection? -> boolean?
;;   Chainable alias for $empty?.
(define (.empty? sel)
  ($empty? sel))
