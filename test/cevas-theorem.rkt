;;;
;;; Geometry Constructions
;;;

;; JSXGraph is an open-source JavaScript library for interactive 
;; geometry, plotting, and data visualization in the browser.

;; Here we use it to make a simple geometric construction.
;; The primary goals is to demonstrate how the FFI works in WebRacket.

;; In JSXGraph there is a board (think "black board") on which
;; one can draw points, lines, circles, graphs etc.
;; It is displayed in a container (in this example the id is "box").

;;;
;;; Style Notes
;;;

;; When constructing new points follow the convention:

;; - Free/independent points (those you can drag freely) are often shown in blue.
;; - Dependent/constructed points (intersection points, midpoints, etc.)
;;   are often shown in red.
;; - Sometimes, auxiliary or hidden construction points are shown in
;;   gray or lighter colors. 

;;;
;;; Build Notes
;;;

;; racket -l errortrace -t ../webracket.rkt --
;;  --ffi ../jsxgraph.ffi
;;  --ffi ../standard.ffi
;;  --ffi ../dom.ffi
;;  -b jsxgraph-example.rkt


;;;
;;; Construct the DOM
;;; 

(define head (js-document-head))

;; Container element for the board
(define container (js-create-element "div"))
(js-set-attribute! container "id"    "box")
(js-set-attribute! container "class" "jxgbox")
(js-set-attribute! container "style" "width: 500px; height: 400px;")
(js-append-child! (js-document-body) container)

;; Use the JSXGraph stylesheet
(define link (js-create-element "link"))
(js-set-attribute! link "rel" "stylesheet")
(js-set-attribute! link "href"
                   "https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraph.css")
(js-append-child! head link)

;;;
;;; Syntax
;;;

#;(define-syntax-rule (defv (x ...) expr)
    (define-values (x ...) expr))
 

;;;
;;; General Helpers
;;;

; obj.prop0.prop1 
(define (dot obj . props)
  (let loop ([val obj]
             [ps  props])
    (cond
      [(null? ps) val]
      [else       (loop (js-ref val (car props))
                        (cdr props))])))

(define (error msg . values)
  (js-log msg)
  ; todo - fix js-log - $prim:js-log is missing?!
  (for-each (λ (x) (js-log x)) values)
  (/ 0 0))


;;;
;;; Pairings, Keys, Attributes and Parents
;;;

(define (key? x)
  (or (symbol? x) (string? x)))

(define (key->string x)
  (or (and (string? x) x)
      (and (symbol? x) (symbol->string x))
      (error 'key->string "expected a key (string or symbol), got: " x)))


(define (pairing? x)
  (match x
    [(list   key value) #t]
    [(vector key value) #t]
    [_                  #f]))

(define (pairing->vector x)
  (match x
    [(list   key value) (vector (key->string key) value)]
    [(vector key value) (vector (key->string key) value)]
    [_                  #f]))



(define (pairings->vector xs)
  (cond
    [(vector? xs) (vector-map pairing->vector xs)]
    [(list? xs)   (list->vector (map pairing->vector xs))]
    [else         #f]))

(define (attributes . key/values)
  (define n (length key/values))
  (unless (even? n)
    (error 'attributes
           "expected an equal number of keys and values, got:"
           key/values))

  (js-object
   (list->vector
    (let loop ([kvs key/values])
      (match kvs
        ['()                    '()]
        [(list* key value more) (cons (vector (key->string key) value)
                                      (loop (cdr (cdr kvs))))])))))

(define (parents . xs)
  (list->vector xs))

(define (set-attribute! elem key value)
  (js-send elem "setAttribute" (vector (vector (key->string key) value))))

;;;
;;; Invokers
;;;

; js-send/flonum* returns a flonum
(define (js-send/flonum* obj method-name . args)
  (js-send/flonum obj method-name (apply vector args)))


;;;
;;; Constructors
;;;

(define JSXGraph #f) ; initialized in `init-board`

;; Board Constructor
(define (create-board container-id [maybe-attributes #f])
  (define attrs (or maybe-attributes
                    ; default attributes
                    (attributes 'boundingbox     #[-5 5 5 -5]
                                'axis            #t
                                'keepaspectratio #t)))
  (js-send JSXGraph "initBoard" (vector container-id attrs)))


;; Element Constructors
;;   - creates elements on `board`

(define (board-create board element-type parents [attributes #f])
  (define attrs (or attributes '#[])) ; optional
  (js-send board "create" (vector element-type parents attrs)))

(define (create-point board parents [attributes #f])
  (board-create board "point" parents attributes))

(define (create-line board parents [attributes #f])
  ; parents can be:
  ;   - two points     (JXG.Point, array, function)
  ;   - three numbers: (number, function)
  ;       a,b,c   =>   az + bx + cy = 0
  ;   - a function:    (a function returning an array of 3 numbers
  ;                     in homogenous coordinates)
  (board-create board "line" parents attributes))

(define (create-segment board parents [attributes #f])
  (board-create board "segment" parents attributes))

(define (create-circle board parents [attributes #f])
  ; parents:
  ;   center: a point
  ;   radius: number, point, line, circle
  (board-create board "circle" parents attributes))

(define (create-perpendicular board parents [attributes #f])
  (board-create board "perpendicular" parents attributes))

(define (create-intersection board parents [attributes #f])
  (board-create board "intersection" parents attributes))

(define (create-text board parents [attributes #f])
  (board-create board "text" parents attributes))

;; Point

(define (point? x)
  (and (external? x)
       (js-instanceof x (dot (js-var "JXG") "Point"))))

(define (point-x p)
  (js-send/flonum* p "X"))

(define (point-y p)
  (js-send/flonum* p "Y"))

(define (coordinates p)
  (values (point-x p) (point-y p)))

;; Event handlers

(define (on element event handler)
  (js-send element "on" (vector event handler)))

;;;
;;; Vector Operations
;;;

(define (as-vector obj)
  (define msg "expected a vector-like value, got: ")
  (match obj
    [(vector x y) obj]
    [(list   x y) (vector x y)]
    [(? point?)   (vector (point-x obj) (point-y obj))]
    [_ (error 'as-vector msg obj)]))

(define (vector-plus v w)
  (define msg "expected a vector-like value, got: ")
  (match* ((as-vector v) (as-vector w))
    [((vector vx vy) (vector wx wy))
     (vector (+ wx vx) (+ wy vy))]))

(define (vector-minus v w)
  (define msg "expected a vector-like value, got: ")
  (match* ((as-vector v) (as-vector w))
    [((vector vx vy) (vector wx wy))
     (vector (- wx vx) (- wy vy))]))

(define (displacement P Q)
  (define OP (as-vector P))
  (define OQ (as-vector Q))
  (vector-minus OQ OP))


(define (dot-product v w)
  (match* ((as-vector v) (as-vector w))
    [((vector vx vy) (vector wx wy))
     (+ (* vx wx) (* vy wy))]))


;;; The Construction
;;;

(define (init-board _evt)
  ; The JXG.JSXGraph singleton stores all properties required to load, save,
  ; create and free a board.
  (set! JSXGraph (dot (js-var "JXG") "JSXGraph"))

  (define board
    (create-board "box"
                  (attributes 'boundingbox    #[-5 5 5 -5]
                              'axis           #f
                              'showNavigation #f)))

  (define a  (create-point board (parents -4  0) (attributes 'name "a")))
  (define b  (create-point board (parents  4 -3) (attributes 'name "b")))
  (define c  (create-point board (parents  1  4) (attributes 'name "c")))

  (define s1 (create-segment board (vector a b) (attributes 'color "black")))
  (define s2 (create-segment board (vector a c) (attributes 'color "black")))
  (define s3 (create-segment board (vector c b) (attributes 'color "black")))

  (define as (board-create board "glider" (vector 3  0 s3) (attributes 'name "a'")))
  (define bs (board-create board "glider" (vector 4 -3 s2) (attributes 'name "b'")))
  (define cs (board-create board "glider" (vector 1  4 s1) (attributes 'name "c'")))

  (define s4 (create-segment board (vector a as) (attributes 'color "black")))
  (define s5 (create-segment board (vector b bs) (attributes 'color "black")))
  (define s6 (create-segment board (vector c cs) (attributes 'color "black")))

  (define (TV p q t)
    (define v (/ (js-send/flonum* p "Dist" t)
                 (js-send/flonum* p "Dist" q)))
    (define dp (dot-product (displacement p q) (displacement p t)))
    (if (>= dp 0) v (- v)))

  (define (to-fixed2 x)
    "x"
    #;(define scaled (round (* x 100)))
    #;(define value (/ scaled 100.0))
    #;(js-log value)
    #;(number->string value))

  #;(create-text board (parents -4.5 -4 
                              (procedure->external
                               (λ ()
                                 (string-append
                                  "TV(a',c,b) * TV(b',a,c) * TV(c',b,a) = "
                                  (to-fixed2 (* (TV as c b)
                                                (TV bs a c)
                                                (TV cs b a))))))))

  (void))

;;; Load JSXGraph.
;;;

(define script (js-create-element "script"))
(js-set-attribute! script "src"
                   "https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraphcore.js")
(js-add-event-listener! script "load" (procedure->external init-board))
(js-append-child! head  script)
