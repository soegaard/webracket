#lang webracket

;;;
;;; Array wrappers
;;;

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
    [(vector? value) value]
    [(list? value) (list->vector value)]
    [else
     (raise-argument-error who "(or/c list? vector?)" value)]))

;; array-items->vector : (listof any/c) -> vector?
;;   Pack rest arguments into the vector shape expected by Array methods.
(define (array-items->vector items)
  (list->vector items))

;; array-is-array? : any/c -> boolean?
;;   Report whether a value is a JavaScript Array.
(define (array-is-array? value)
  (not (zero? (js-array-is-array value))))

;; array-from : any/c [any/c #f] [any/c #f] -> external/raw
;;   Build an array from an iterable or array-like value.
(define (array-from source [map-function #f] [this-arg #f])
  (cond
    [(eq? map-function #f)
     (js-send/extern (js-var "Array") "from" (vector source))]
    [(eq? this-arg #f)
     (js-send/extern (js-var "Array") "from" (vector source map-function))]
    [else
     (js-send/extern (js-var "Array") "from" (vector source map-function this-arg))]))

;; array-from-async : any/c [any/c #f] [any/c #f] -> external/raw
;;   Build an array from an async iterable or array-like value.
(define (array-from-async source [map-function #f] [this-arg #f])
  (cond
    [(eq? map-function #f)
     (js-send/extern (js-var "Array") "from-async" (vector source))]
    [(eq? this-arg #f)
     (js-send/extern (js-var "Array") "from-async" (vector source map-function))]
    [else
     (js-send/extern (js-var "Array") "from-async" (vector source map-function this-arg))]))

;; array-of : any/c ... -> external/raw
;;   Build a JavaScript array from the given items.
(define (array-of . items)
  (js-array-of (array-items->vector items)))

;; array-length : external/raw -> exact-nonnegative-integer?
;;   Read the length of a JavaScript array.
(define (array-length arr)
  (js-array-length arr))

;; array-ref : external/raw exact-integer? -> any/c
;;   Read an array element using Array.prototype.at semantics.
(define (array-ref arr index)
  (js-index arr index))

;; array-at : external/raw exact-integer? -> any/c
;;   Read an array element using Array.prototype.at semantics.
(define (array-at arr index)
  (js-index arr index))

;; array->vector : any/c -> vector?
;;   Convert a JavaScript array into a Racket vector.
(define (array->vector arr)
  (cond
    [(vector? arr) arr]
    [(list? arr) (list->vector arr)]
    [else
     (define len (array-length arr))
     (let loop ([i 0] [acc '()])
       (if (= i len)
           (list->vector (reverse acc))
           (loop (add1 i) (cons (js-index arr i) acc))))]))

;; array->list : any/c -> list?
;;   Convert a JavaScript array into a Racket list.
(define (array->list arr)
  (vector->list (array->vector arr)))

;; vector->array : vector? -> external/raw
;;   Convert a Racket vector into a JavaScript array.
(define (vector->array vec)
  (js-array-of vec))

;; list->array : list? -> external/raw
;;   Convert a Racket list into a JavaScript array.
(define (list->array xs)
  (js-array-of (list->vector xs)))

;; sequence->array : (or/c list? vector?) -> external/raw
;;   Convert a Racket list or vector into a JavaScript array.
(define (sequence->array items)
  (js-array-of (array-sequence->vector 'sequence->array items)))

;; array-join : external/raw [any/c #f] -> string?
;;   Join the array elements into a string.
(define (array-join arr [separator #f])
  (if (eq? separator #f)
      (js-send/value arr "join" (vector))
      (js-send/value arr "join" (vector (array-stringish->string 'array-join separator)))))

;; array-slice : external/raw [any/c #f] [any/c #f] -> external/raw
;;   Return a sliced copy of the array.
(define (array-slice arr [start #f] [end #f])
  (cond
    [(and (eq? start #f) (eq? end #f))
     (js-send/extern arr "slice" (vector))]
    [(eq? end #f)
     (js-send/extern arr "slice" (vector start))]
    [else
     (js-send/extern arr "slice" (vector start end))]))

;; array-concat : external/raw any/c ... -> external/raw
;;   Concatenate arrays and values into a new array.
(define (array-concat arr . items)
  (js-array-concat arr (array-items->vector items)))

;; array-copy-within! : external/raw exact-integer? exact-integer? [any/c #f] -> external/raw
;;   Copy a slice of the array within the array itself.
(define (array-copy-within! arr target start [end #f])
  (if (eq? end #f)
      (js-send/extern arr "copyWithin" (vector target start))
      (js-send/extern arr "copyWithin" (vector target start end))))

;; array-fill! : external/raw any/c [any/c #f] [any/c #f] -> external/raw
;;   Fill a range of the array in place.
(define (array-fill! arr value [start #f] [end #f])
  (cond
    [(and (eq? start #f) (eq? end #f))
     (js-send/extern arr "fill" (vector value))]
    [(eq? end #f)
     (js-send/extern arr "fill" (vector value start))]
    [else
     (js-send/extern arr "fill" (vector value start end))]))

;; array-push! : external/raw any/c ... -> exact-nonnegative-integer?
;;   Append one or more values to the end of the array.
(define (array-push! arr . items)
  (js-array-push arr (array-items->vector items)))

;; array-pop : external/raw -> any/c
;;   Remove and return the last array element.
(define (array-pop arr)
  (js-send/value arr "pop" (vector)))

;; array-reverse! : external/raw -> external/raw
;;   Reverse the array in place.
(define (array-reverse! arr)
  (js-array-reverse arr))

;; array-shift : external/raw -> any/c
;;   Remove and return the first array element.
(define (array-shift arr)
  (js-send/value arr "shift" (vector)))

;; array-sort! : external/raw [any/c #f] -> external/raw
;;   Sort the array in place.
(define (array-sort! arr [compare-function #f])
  (if (eq? compare-function #f)
      (js-send/extern arr "sort" (vector))
      (js-send/extern arr "sort" (vector compare-function))))

;; array-splice! : external/raw exact-integer? exact-integer? any/c ... -> external/raw
;;   Replace array elements in place and return the removed items.
(define (array-splice! arr start delete-count . items)
  (js-array-splice arr
                   (array-items->vector (cons start (cons delete-count items)))))

;; array-to-locale-string : external/raw [any/c #f] -> string?
;;   Convert the array to a localized string.
(define (array-to-locale-string arr [locales-options #f])
  (if (eq? locales-options #f)
      (js-send/value arr "toLocaleString" (vector))
      (js-send/value arr "toLocaleString" (vector locales-options))))

;; array-to-string : external/raw -> string?
;;   Convert the array to a string.
(define (array-to-string arr)
  (js-array-to-string arr))

;; array-unshift! : external/raw any/c ... -> exact-nonnegative-integer?
;;   Prepend one or more values to the array.
(define (array-unshift! arr . items)
  (js-array-unshift arr (array-items->vector items)))

;; array-flat : external/raw [any/c #f] -> external/raw
;;   Flatten one level or an explicit depth.
(define (array-flat arr [depth #f])
  (if (eq? depth #f)
      (js-send/extern arr "flat" (vector))
      (js-send/extern arr "flat" (vector depth))))

;; array-to-reversed : external/raw -> external/raw
;;   Return a reversed copy of the array.
(define (array-to-reversed arr)
  (js-array-to-reversed arr))

;; array-to-sorted : external/raw [any/c #f] -> external/raw
;;   Return a sorted copy of the array.
(define (array-to-sorted arr [compare-function #f])
  (if (eq? compare-function #f)
      (js-send/extern arr "toSorted" (vector))
      (js-send/extern arr "toSorted" (vector compare-function))))

;; array-to-spliced : external/raw exact-integer? exact-integer? any/c ... -> external/raw
;;   Return the result of splicing without mutating the original array.
(define (array-to-spliced arr start delete-count . items)
  (js-array-to-spliced arr
                       (array-items->vector (cons start (cons delete-count items)))))

;; array-with : external/raw exact-integer? any/c -> external/raw
;;   Return a copy of the array with one value replaced.
(define (array-with arr index value)
  (js-array-with arr index value))
