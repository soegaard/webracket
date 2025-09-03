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

;; Point

(define (point? x)
  (and (external? x)
       ...???...))

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
    #;[(? point?)   (vector (point-x obj) (point-y obj))]
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


;;;
;;; The Construction
;;;

(define (init-board _evt)
  ; The JXG.JSXGraph singleton stores all properties required to load, save,
  ; create and free a board.
  (set! JSXGraph (dot (js-var "JXG") "JSXGraph"))

  
  (define board (create-board "box"))

  (define A  (create-point board
                           (parents -2 1) 
                           (attributes 'name "A" 'color "blue")))
  
  (define B  (create-point board
                           (parents 4 -2) 
                           (attributes 'name "B" 'color "blue")))
  
  (define C  (create-point board
                           (parents 1 -2) 
                           (attributes 'name "C" 'color "blue")))

  (define BC (create-line board (vector B C) (attributes 'visible #f 'dash 2)))

  ; Triangle ABC
  (create-segment board (vector A B))
  (create-segment board (vector B C))
  (create-segment board (vector C A))

  (define l (create-perpendicular board
                                  (parents BC A)
                                  (attributes 'name "l")))
  
  (define P (create-intersection board
                                 (parents l BC)
                                 (attributes 'name  "P" 'color "red")))

  (create-circle board (parents P A))

  (define (update-BC-line . _)
    (define-values (bx by) (coordinates B))
    (define-values (cx cy) (coordinates C))
    (define-values (px py) (coordinates P))

    (define dot1 (+ (* (- px bx) (- cx bx)) (* (- py by) (- cy by))))
    (define dot2 (+ (* (- px cx) (- bx cx)) (* (- py cy) (- by cy))))

    (define outside? (not (and (>= dot1 0) (>= dot2 0))))

    (if outside?
        (set-attribute! BC 'visible #t)
        (set-attribute! BC 'visible #f))

    (void))
 
  ;; Updates  
  (update-BC-line)

  ;; Install event handlers
  (define on-drag-handler (procedure->external update-BC-line))
  (define on-down-handler (procedure->external
                           (λ _ (set-attribute! BC 'visible #t))))
  (define on-up-handler   (procedure->external update-BC-line))

  (on board "down" on-down-handler)
  (on board "up"   on-up-handler)
  
  ;; (on A "down" on-down-handler)
  ;; (on B "down" on-down-handler)
  ;; (on C "down" on-down-handler)
  ;; (on A "up"   on-up-handler)
  ;; (on B "up"   on-up-handler)
  ;; (on C "up"   on-up-handler)
  (void))

;;;
;;; Load JSXGraph. 
;;;

;; The JSXGraph is loaded from a CDN.
;; When loaded, the board is created.

(define script (js-create-element "script"))
(js-set-attribute! script "src"
                  "https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraphcore.js")
(js-add-event-listener! script "load" (procedure->external init-board))
(js-append-child! head  script)
