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
(define (js-send* obj method-name . args)
  (js-send obj method-name (apply vector args)))

;;;
;;; The Construction
;;;

(define (init-board _evt)
  ; The JXG.JSXGraph singleton stores all properties required to load, save, create and free a board.
  (define JSXGraph (dot (js-var "JXG") "JSXGraph"))

  (define board
    (js-send JSXGraph "initBoard" (vector "box" (js-object '#[#["boundingbox" #[-5 5 5 -5]]
                                                              #["axis"        #t]]))))
  (define A
    (js-send* board "create"
              "point"                           ; element type
              (vector -2 1)                     ; array of parents
              (js-object '#[#["name" "A"]])))   ; attributes

  (define B
    (js-send* board "create"
              "point"                           ; element type
              (vector 3 4)                      ; array of parents
              (js-object '#[#["name" "B"]])))   ; attributes

  (define C
    (js-send* board "create"
              "point"
              (vector 1 -2)
              (js-object '#[#["name" "C"]])))   ; attributes

  (js-send* board "create"
            "line"
            (vector A B)
            (js-object '#[]))

  (js-send* board "create"
            "segment"
            (vector A B)
            (js-object '#[]))

  (define BC
    (js-send* board "create"
              "line"
              (vector B C)
              (js-object '#[#["visible" #f]])))

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
              (js-object '#[#["name" "P"]])))

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
