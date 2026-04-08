#lang webracket

;;;
;;; Array wrappers
;;;

(struct array (raw) #:transparent)

;; array-stringish->string : symbol? (or/c string? symbol?) -> string?
;;   Normalize a string-like array argument.
(define (array-stringish->string who v)
  (cond
    [(string? v) v]
    [(symbol? v) (symbol->string v)]
    [else
     (raise-argument-error who "(or/c string? symbol?)" v)]))

;; array-sequence->vector : symbol? (or/c list? vector?) -> vector?
;;   Normalize a list or vector to a vector for array construction.
(define (array-sequence->vector who value)
  (cond
    [(vector? value) (list->vector (map array-unwrap (vector->list value)))]
    [(list? value) (list->vector (map array-unwrap value))]
    [else
     (raise-argument-error who "(or/c list? vector?)" value)]))

;; array-wrap : any/c -> array?
;;   Wrap a browser JavaScript Array value.
(define (array-wrap value)
  (array value))

;; array-unwrap : any/c -> any/c
;;   Unwrap a browser JavaScript Array value when available.
(define (array-unwrap value)
  (if (array? value)
      (array-raw value)
      value))

;; array-items->vector : (listof any/c) -> vector?
;;   Pack rest arguments into the vector shape expected by Array methods.
(define (array-items->vector items)
  (list->vector (map array-unwrap items)))

;; array-from : any/c [(or/c procedure? external?) #f] [any/c #f] -> array?
;;   Build an array from an iterable or array-like value.
;;   When map-function is supplied, it is called with each element and its
;;   index; this-arg becomes the JavaScript this value during those calls.
(define (array-from source [map-function #f] [this-arg #f])
  (define source* (array-unwrap source))
  (cond
    [(eq? map-function #f)
     (array-wrap (js-send/extern (js-var "Array") "from" (vector source*)))]
    [(eq? this-arg #f)
     (array-wrap (js-send/extern (js-var "Array") "from" (vector source* map-function)))]
    [else
     (array-wrap (js-send/extern (js-var "Array") "from"
                                 (vector source* map-function (array-unwrap this-arg))))]))

;; array-from-async : any/c [(or/c procedure? external?) #f] [any/c #f] -> array?
;;   Build an array from an async iterable or array-like value.
;;   When map-function is supplied, it is called with each element and its
;;   index; this-arg becomes the JavaScript this value during those calls.
(define (array-from-async source [map-function #f] [this-arg #f])
  (define source* (array-unwrap source))
  (cond
    [(eq? map-function #f)
     (array-wrap (js-send/extern (js-var "Array") "from-async" (vector source*)))]
    [(eq? this-arg #f)
     (array-wrap (js-send/extern (js-var "Array") "from-async" (vector source* map-function)))]
    [else
     (array-wrap (js-send/extern (js-var "Array") "from-async"
                                 (vector source* map-function (array-unwrap this-arg))))]))

;; array-of : any/c ... -> array?
;;   Build a JavaScript array from the given items.
(define (array-of . items)
  (array-wrap (js-array-of (array-items->vector items))))

;; array-length : any/c -> exact-nonnegative-integer?
;;   Read the length of a JavaScript array.
(define (array-length arr)
  (js-array-length (array-unwrap arr)))

;; array-ref : any/c exact-integer? -> any/c
;;   Read an array element using Array.prototype.at semantics.
(define (array-ref arr index)
  (js-index (array-unwrap arr) index))

;; array-at : any/c exact-integer? -> any/c
;;   Read an array element using Array.prototype.at semantics.
(define (array-at arr index)
  (js-index (array-unwrap arr) index))

;; array->vector : any/c -> vector?
;;   Convert a JavaScript array into a Racket vector.
(define (array->vector arr)
  (define arr* (array-unwrap arr))
  (cond
    [(vector? arr*) arr*]
    [(list? arr*) (list->vector arr*)]
    [else
     (js-array->vector arr*)]))

;; array->list : any/c -> list?
;;   Convert a JavaScript array into a Racket list.
(define (array->list arr)
  (vector->list (array->vector arr)))

;; vector->array : vector? -> array?
;;   Convert a Racket vector into a JavaScript array.
(define (vector->array vec)
  (array-wrap (js-array-of (array-sequence->vector 'vector->array vec))))

;; list->array : list? -> array?
;;   Convert a Racket list into a JavaScript array.
(define (list->array xs)
  (array-wrap (js-array-of (array-sequence->vector 'list->array xs))))

;; sequence->array : (or/c list? vector?) -> array?
;;   Convert a Racket list or vector into a JavaScript array.
(define (sequence->array items)
  (array-wrap (js-array-of (array-sequence->vector 'sequence->array items))))

;; array-join : any/c [any/c #f] -> string?
;;   Join the array elements into a string.
(define (array-join arr [separator #f])
  (if (eq? separator #f)
      (js-send/value (array-unwrap arr) "join" (vector))
      (js-send/value (array-unwrap arr) "join" (vector (array-stringish->string 'array-join separator)))))

;; array-slice : any/c [any/c #f] [any/c #f] -> array?
;;   Return a sliced copy of the array.
;;   start and end are forwarded to JavaScript's slice method unchanged.
(define (array-slice arr [start #f] [end #f])
  (define arr* (array-unwrap arr))
  (cond
    [(and (eq? start #f) (eq? end #f))
     (array-wrap (js-send/extern arr* "slice" (vector)))]
    [(eq? end #f)
     (array-wrap (js-send/extern arr* "slice" (vector start)))]
    [else
     (array-wrap (js-send/extern arr* "slice" (vector start end)))]))

;; array-concat : any/c any/c ... -> array?
;;   Concatenate arrays and values into a new array.
(define (array-concat arr . items)
  (array-wrap (js-array-concat (array-unwrap arr) (array-items->vector items))))

;; array-copy-within! : any/c exact-integer? exact-integer? [any/c #f] -> array?
;;   Copy a slice of the array within the array itself.
;;   The optional end index is forwarded to JavaScript unchanged.
(define (array-copy-within! arr target start [end #f])
  (define arr* (array-unwrap arr))
  (if (eq? end #f)
      (array-wrap (js-send/extern arr* "copyWithin" (vector target start)))
      (array-wrap (js-send/extern arr* "copyWithin" (vector target start end)))))

;; array-fill! : any/c any/c [any/c #f] [any/c #f] -> array?
;;   Fill a range of the array in place.
;;   start and end are forwarded to JavaScript's fill method unchanged.
(define (array-fill! arr value [start #f] [end #f])
  (define arr* (array-unwrap arr))
  (cond
    [(and (eq? start #f) (eq? end #f))
     (array-wrap (js-send/extern arr* "fill" (vector (array-unwrap value))))]
    [(eq? end #f)
     (array-wrap (js-send/extern arr* "fill" (vector (array-unwrap value) start)))]
    [else
     (array-wrap (js-send/extern arr* "fill" (vector (array-unwrap value) start end)))]))

;; array-push! : any/c any/c ... -> exact-nonnegative-integer?
;;   Append one or more values to the end of the array.
(define (array-push! arr . items)
  (js-array-push (array-unwrap arr) (array-items->vector items)))

;; array-pop : any/c -> any/c
;;   Remove and return the last array element.
(define (array-pop arr)
  (js-send/value (array-unwrap arr) "pop" (vector)))

;; array-reverse! : any/c -> array?
;;   Reverse the array in place.
(define (array-reverse! arr)
  (array-wrap (js-array-reverse (array-unwrap arr))))

;; array-shift : any/c -> any/c
;;   Remove and return the first array element.
(define (array-shift arr)
  (js-send/value (array-unwrap arr) "shift" (vector)))

;; array-sort! : any/c [(or/c procedure? external?) #f] -> array?
;;   Sort the array in place.
;;   When compare-function is supplied, it is called with two elements and
;;   should return a negative, zero, or positive value.
(define (array-sort! arr [compare-function #f])
  (define arr* (array-unwrap arr))
  (if (eq? compare-function #f)
      (array-wrap (js-send/extern arr* "sort" (vector)))
      (array-wrap (js-send/extern arr* "sort" (vector compare-function)))))

;; array-splice! : any/c exact-integer? exact-integer? any/c ... -> array?
;;   Replace array elements in place and return the removed items.
(define (array-splice! arr start delete-count . items)
  (array-wrap
   (js-array-splice (array-unwrap arr)
                    (array-items->vector (cons start (cons delete-count items))))))

;; array-to-locale-string : any/c [any/c #f] -> string?
;;   Convert the array to a localized string.
(define (array-to-locale-string arr [locales-options #f])
  (if (eq? locales-options #f)
      (js-send/value (array-unwrap arr) "toLocaleString" (vector))
      (js-send/value (array-unwrap arr) "toLocaleString" (vector locales-options))))

;; array-to-string : any/c -> string?
;;   Convert the array to a string.
(define (array-to-string arr)
  (js-array-to-string (array-unwrap arr)))

;; array-unshift! : any/c any/c ... -> exact-nonnegative-integer?
;;   Prepend one or more values to the array.
(define (array-unshift! arr . items)
  (js-array-unshift (array-unwrap arr) (array-items->vector items)))

;; array-flat : any/c [any/c #f] -> array?
;;   Flatten one level or an explicit depth.
;;   depth is forwarded to JavaScript's flat method unchanged.
(define (array-flat arr [depth #f])
  (define arr* (array-unwrap arr))
  (if (eq? depth #f)
      (array-wrap (js-send/extern arr* "flat" (vector)))
      (array-wrap (js-send/extern arr* "flat" (vector depth)))))

;; array-to-reversed : any/c -> array?
;;   Return a reversed copy of the array.
(define (array-to-reversed arr)
  (array-wrap (js-array-to-reversed (array-unwrap arr))))

;; array-to-sorted : any/c [(or/c procedure? external?) #f] -> array?
;;   Return a sorted copy of the array.
;;   When compare-function is supplied, it is called with two elements and
;;   should return a negative, zero, or positive value.
(define (array-to-sorted arr [compare-function #f])
  (define arr* (array-unwrap arr))
  (if (eq? compare-function #f)
      (array-wrap (js-send/extern arr* "toSorted" (vector)))
      (array-wrap (js-send/extern arr* "toSorted" (vector compare-function)))))

;; array-to-spliced : any/c exact-integer? exact-integer? any/c ... -> array?
;;   Return the result of splicing without mutating the original array.
;;   start and delete-count are array positions.
(define (array-to-spliced arr start delete-count . items)
  (array-wrap
   (js-array-to-spliced (array-unwrap arr)
                        (array-items->vector (cons start (cons delete-count items))))))

;; array-with : any/c exact-integer? any/c -> array?
;;   Return a copy of the array with one value replaced.
;;   index is an array position.
(define (array-with arr index value)
  (array-wrap (js-array-with (array-unwrap arr) index (array-unwrap value))))
