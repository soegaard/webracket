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
;; - Dependent/constructed points (intersection points, midpoints, etc.) are often shown in red.
;; - Sometimes, auxiliary or hidden construction points are shown in gray or lighter colors. 

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
(js-set-attribute! link "href" "https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraph.css")
(js-append-child! head link)

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

; invoke the `method-name` method of the object `obj` with arguments `args`.
; js-send* returns an external object
(define (js-send* obj method-name . args)
  (js-send obj method-name (apply vector args)))

; js-send/flonum* returns a flonum
(define (js-send/flonum* obj method-name . args)
  (js-send/flonum obj method-name (apply vector args)))

;;;
;;; Constructors
;;;

(define (board-create board element-type parents [attributes #f])
  (define attrs (or attributes '#[])) ; optional
  (js-send board "create" (vector element-type parents attrs)))

(define (create-point board parents [attributes #f])
  (board-create board "point" parents attributes))

;;;
;;; Pairings, Keys and Attributes
;;;

(define (key? x)
  (or (symbol? x) (string? x)))

(define (key->string x)
  (or (and (string? x) x)
      (and (symbol? x) (symbol->string x))
      #f))  ; todo : signal error


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

(define (vector-map f xs)
  (list->vector
   (map f (vector->list xs))))

(define (error msg . values)
  ; (js-log msg)
  ; (for-each js-log values)
  (/ 0 0))


(define (pairings->vector xs)
  (cond
    [(vector? xs) (vector-map pairing->vector xs)]
    [(list? xs)   (list->vector (map pairing->vector xs))]
    [else         #f]))

(define (attributes . key/values)
  (define n (length key/values))
  (unless (even? n)
    (error 'attributes "expected an equal number of keys and values, got:" key/values))

  (js-object
   (list->vector
    (let loop ([kvs key/values])
      (match kvs
        ['()                    '()]
        [(list* key value more) (cons (vector (key->string key) value)
                                      (loop (cdr (cdr kvs))))])))))

;;;
;;; The Construction
;;;

(define (init-board _evt)
  ; The JXG.JSXGraph singleton stores all properties required to load, save, create and free a board.
  (define JSXGraph (dot (js-var "JXG") "JSXGraph"))
  
  (define board
    (js-send JSXGraph "initBoard" (vector "box" (js-object '#[#["boundingbox" #[-5 5 5 -5]]
                                                              #["axis"        #t]]))))

  (define A (create-point board
                          (vector -2 1) 
                          (attributes 'name "A" 'color "blue")))
  
  (define B
    (board-create board "point"  ; element type
              (vector 3 4)                      ; array of parents
              (js-object '#[#["name"  "B"]
                            #["color" "blue"]]))) ; attributes
  (define C
    (js-send* board "create"
              "point"
              (vector 1 -2)
              (js-object '#[#["name"  "C"]
                            #["color" "blue"]]))) ; attributes
  
  (js-send* board "create" "line"    (vector A B) (js-object '#[]))
  (js-send* board "create" "segment" (vector A B) (js-object '#[]))

  (define BC
    (js-send* board "create" "line" (vector B C) (js-object '#[#["visible" #f]
                                                               #["dash"    2]])))

  (js-send* board "create"
            "segment"
            (vector B C)
            (js-object '#[]))

  (js-send* board "create"
            "segment"
            (vector C A)
            (js-object '#[]))

  (define l
    (js-send* board "create"
              "perpendicular"
              (vector BC A)
              (js-object '#[#["name" "l"]])))

  (define P
    (js-send* board "create"
              "intersection"
              (vector l BC)
              (js-object '#[#["name"  "P"]
                                 #["color" "red"]])))

  (define (update-BC-line . _)
    (define bx (js-send/flonum* B "X"))
    (define by (js-send/flonum* B "Y"))
    (define cx (js-send/flonum* C "X"))
    (define cy (js-send/flonum* C "Y"))
    (define px (js-send/flonum* P "X"))
    (define py (js-send/flonum* P "Y"))

    (define dot1 (+ (* (- px bx) (- cx bx)) (* (- py by) (- cy by))))
    (define dot2 (+ (* (- px cx) (- bx cx)) (* (- py cy) (- by cy))))

    (define outside? (not (and (>= dot1 0) (>= dot2 0))))
    
    (js-send* BC "setAttribute"
              (if outside?
                  '#["visible" #t]
                  '#["visible" #f]))
    )

  

  ;; 
  
  (update-BC-line)
  (define on-drag-handler (procedure->external update-BC-line))  
  (define on-down-handler (procedure->external
                           (λ _ (js-send* BC "setAttribute" '#["visible" #t]))))
  (define on-up-handler   (procedure->external update-BC-line))

  (js-send* board "on" "down" on-down-handler)
  (js-send* board "on" "up"   on-up-handler)
  
  ;; (js-send* A "on" "down" on-down-handler)
  ;; (js-send* B "on" "down" on-down-handler)
  ;; (js-send* C "on" "down" on-down-handler)
  ;; (js-send* A "on" "up"   on-up-handler)
  ;; (js-send* B "on" "up"   on-up-handler)
  ;; (js-send* C "on" "up"   on-up-handler)
  (void))

;;;
;;; Load JSXGraph. 
;;;

;; The JSXGraph is loaded from a CDN.
;; When loaded, the board is created.

(define script (js-create-element "script"))
(js-set-attribute!      script "src"  "https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraphcore.js")
(js-add-event-listener! script "load" (procedure->external init-board))
(js-append-child! head  script)
