(define head (js-document-head))

(define container (js-create-element "div"))
(js-set-attribute! container "id"    "box")
(js-set-attribute! container "class" "jxgbox")
(js-set-attribute! container "style" "width: 500px; height: 400px;")
(js-append-child! (js-document-body) container)

(define link (js-create-element "link"))
(js-set-attribute! link "rel" "stylesheet")
(js-set-attribute! link "href" "https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraph.css")
(js-append-child! head link)

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
  )

(define script (js-create-element "script"))
(js-set-attribute!      script "src"  "https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraphcore.js")
(js-add-event-listener! script "load" (procedure->external init-board))
(js-append-child! head  script)
