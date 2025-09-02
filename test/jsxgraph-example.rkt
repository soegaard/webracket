(define head (js-document-head))

(define container (js-create-element "div"))
(js-set-attribute! container "id" "box")
(js-set-attribute! container "class" "jxgbox")
(js-set-attribute! container "style" "width: 500px; height: 400px;")
(js-append-child! (js-document-body) container)

(define link (js-create-element "link"))
(js-set-attribute! link "rel" "stylesheet")
(js-set-attribute! link "href" "https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraph.css")
(js-append-child! head link)

(define options
  (js-object
   (vector
    (vector "boundingbox" (vector -5 5 5 -5))
    (vector "axis" #t))))


(define (init-board _evt)
  (define jxg (js-var "JXG"))
  (define jsx (js-ref jxg "JSXGraph"))
  (js-send jsx "initBoard" (vector "box" options)))

(define script (js-create-element "script"))
(js-set-attribute! script "src" "https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraphcore.js")
(js-add-event-listener! script "load" (procedure->external init-board))
(js-append-child! head script)
