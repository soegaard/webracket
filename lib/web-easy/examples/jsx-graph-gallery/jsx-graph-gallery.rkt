;;;
;;; JSXGraph Gallery
;;;

(include-lib web-easy)
(include-lib event)
(include-lib document)
(include-lib jsx-graph)
(include-lib console)

(define geometry-board-id "jsx-graph-gallery-geometry")
(define group-board-id "jsx-graph-gallery-group")
(define chart-board-id "jsx-graph-gallery-chart")
(define primitives-board-id "jsx-graph-gallery-primitives")
(define arrowparallel-board-id "jsx-graph-gallery-arrowparallel")
(define constructions-board-id "jsx-graph-gallery-constructions")
(define widgets-board-id "jsx-graph-gallery-widgets")
(define annotation-board-id "jsx-graph-gallery-annotation")

(define @status (@ "Loading JSXGraph assets..."))
(define @summary (@ "Waiting for the boards to initialize."))

(define geometry-board #f)
(define geometry-p #f)
(define geometry-q #f)
(define geometry-arrow #f)
(define geometry-slider #f)
(define geometry-label #f)

(define group-board #f)
(define group-p #f)
(define group-q #f)
(define group-group #f)

(define chart-board #f)
(define chart #f)
(define chart-legend #f)

(define geometry-board-ready? #f)
(define group-board-ready? #f)
(define chart-board-ready? #f)
(define primitives-board-ready? #f)
(define arrowparallel-board-ready? #f)
(define constructions-board-ready? #f)
(define widgets-board-ready? #f)
(define annotation-board-ready? #f)

(define primitives-board #f)
(define arrowparallel-board #f)
(define arrowparallel-p1 #f)
(define arrowparallel-p2 #f)
(define arrowparallel-p3 #f)
(define arrowparallel-segment #f)
(define arrowparallel-object #f)
(define constructions-board #f)
(define widgets-board #f)
(define annotation-board #f)
(define annotation-slider #f)
(define annotation-text #f)

;; extern-present? : any/c -> boolean?
;;   Check whether a JS value is a live external value.
(define (extern-present? v)
  (external? v))

;; set-status! : string? -> void?
;;   Update the status line in the gallery.
(define (set-status! s)
  (obs-set! @status s))

;; set-summary! : string? -> void?
;;   Update the summary line in the gallery.
(define (set-summary! s)
  (obs-set! @summary s))

;; ensure-jsxgraph-assets! : -> void?
;;   Load the JSXGraph CSS and core script with a local fallback.
(define (ensure-jsxgraph-assets!)
  (define target
    (or (document-head)
        (document-body)))

  (define cdn-css "https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraph.css")
  (define cdn-js  "https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraphcore.js")
  (define local-css "../../web-site-new/local/assets/vendor/jsxgraph/jsxgraph.css")
  (define local-js  "../../web-site-new/local/assets/vendor/jsxgraph/jsxgraphcore.js")

  (unless (document-query-selector "link[data-jsxgraph-css='1']")
    (define link (document-create-element "link"))
    (element-set-attribute! link "rel" "stylesheet")
    (element-set-attribute! link "href" cdn-css)
    (element-set-attribute! link "data-jsxgraph-css" "1")
    (element-add-event-listener!
     link
     "error"
     (lambda (_evt)
       (element-set-attribute! link "href" local-css)))
    (element-append! target link))

  (unless (document-query-selector "script[data-jsxgraph-core='1']")
    (define script (document-create-element "script"))
    (element-set-attribute! script "src" cdn-js)
    (element-set-attribute! script "data-jsxgraph-core" "1")
    (element-add-event-listener!
     script
     "error"
     (lambda (_evt)
       (element-set-attribute! script "src" local-js)))
    (element-append! target script)))

;; init-geometry-board! : -> boolean?
;;   Build the JSXGraph geometry board once the browser assets have loaded.
(define (init-geometry-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (if (not (extern-present? jxg))
      #f
      (let ()
        (define jsxgraph (js-ref jxg "JSXGraph"))
        (if (not (extern-present? jsxgraph))
            #f
            (let ()
              (unless geometry-board-ready?
                (set! geometry-board
                      (jsx-create-board
                       geometry-board-id
                       (js-object
                        (vector (vector "boundingbox" #[-7 7 7 -7])
                                (vector "axis" #t)
                                (vector "keepaspectratio" #t)))))
                (set! geometry-p
                      (jsx-create-point geometry-board
                                        (jsx-parents -4 1)
                                        (js-object (vector (vector "name" "P")
                                                           (vector "size" 4)))))
                (set! geometry-q
                      (jsx-create-point geometry-board
                                        (jsx-parents 3 -1)
                                        (js-object (vector (vector "name" "Q")
                                                           (vector "size" 4)))))
                (set! geometry-arrow
                      (jsx-create-arrow geometry-board
                                        (jsx-parents geometry-p geometry-q)))
                (set! geometry-label
                      (jsx-create-smartlabel
                       geometry-board
                       (jsx-parents geometry-p)
                       (js-object
                        (vector (vector "digits" 1)
                                (vector "unit" "m")
                                (vector "dir" "col")
                                (vector "useMathJax" #f)))))
                (set! geometry-slider
                      (jsx-create-slider geometry-board
                                         (jsx-parents (jsx-parents -5 -5)
                                                       (jsx-parents 2 -5)
                                                       (jsx-parents 0 1 3))
                                         (js-object
                                          (vector (vector "name" "s")
                                                  (vector "strokeColor" "black")
                                                  (vector "fillColor" "white")))))
                (set! geometry-board-ready? #t))
              (void (jsx-board-full-update! geometry-board))
              #t)))))

;; init-group-board! : -> boolean?
;;   Build the JSXGraph group board once the browser assets have loaded.
(define (init-group-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless group-board-ready?
          (set! group-board
                (jsx-create-board
                 group-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! group-p
                (jsx-create-point group-board
                                  (jsx-parents -3 2)
                                  (js-object (vector (vector "name" "P")
                                                     (vector "size" 5)
                                                     (vector "fillColor" "#c53030")))))
          (set! group-q
                (jsx-create-point group-board
                                  (jsx-parents 2 -1)
                                  (js-object (vector (vector "name" "Q")
                                                     (vector "size" 5)
                                                     (vector "fillColor" "#2b6cb0")))))
                (set! group-group
                      (jsx-create-group group-board
                                        (jsx-parents group-q)
                                        (js-object
                                         (vector (vector "name" "G")
                                                 (vector "strokeColor" "#4a5568")
                                                 (vector "fillColor" "#4a5568")))))
          (jsx-group-add-point! group-group group-q)
          (jsx-group-set-rotation-center! group-group group-p)
          (jsx-group-set-rotation-points! group-group (jsx-parents group-q))
          (jsx-group-update! group-group)
          (set! group-board-ready? #t))
        (void (jsx-board-full-update! group-board))
        #t)))

;; init-chart-board! : -> boolean?
;;   Build the JSXGraph chart board once the browser assets have loaded.
(define (init-chart-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (define x-values (jsx-parents -3 -2 -1 0 1 2 3 4 5 6 7 8))
        (define data-values (jsx-parents 4 7 7 27 33 37 46 22 11 4 1 0))
        (define colors (jsx-parents "green" "yellow" "red" "blue"))
        (unless chart-board-ready?
          (set! chart-board
                (jsx-create-board
                 chart-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-4 50 13 -4])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! chart
                (jsx-create-chart
                 chart-board
                 (jsx-parents x-values data-values)
                 (js-object
                  (vector (vector "chartStyle" "bar")
                          (vector "width" 1.0)
                          (vector "labels" data-values)
                          (vector "colors" colors)))))
          (set! chart-legend
                (jsx-create-legend
                 chart-board
                 (jsx-parents 8 45)
                 (js-object
                  (vector (vector "labels" data-values)
                          (vector "colors" colors)
                          (vector "strokeWidth" 5)))))
          (set! chart-board-ready? #t))
        (void (jsx-board-full-update! chart-board))
        #t)))

;; init-primitives-board! : -> boolean?
;;   Build the JSXGraph primitives board once the browser assets have loaded.
(define (init-primitives-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless primitives-board-ready?
          (set! primitives-board
                (jsx-create-board
                 primitives-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-8 8 8 -8])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (let ()
            (define line-a (jsx-create-point primitives-board
                                             (jsx-parents -6 2)
                                             (js-object (vector (vector "name" "A")
                                                                (vector "size" 4)))))
            (define line-b (jsx-create-point primitives-board
                                             (jsx-parents -2 4)
                                             (js-object (vector (vector "name" "B")
                                                                (vector "size" 4)))))
            (define segment-c (jsx-create-point primitives-board
                                                (jsx-parents -6 -3)
                                                (js-object (vector (vector "name" "C")
                                                                   (vector "size" 4)))))
            (define segment-d (jsx-create-point primitives-board
                                                (jsx-parents -2 -2)
                                                (js-object (vector (vector "name" "D")
                                                                   (vector "size" 4)))))
            (define arc-e (jsx-create-point primitives-board
                                            (jsx-parents 1 3)
                                            (js-object (vector (vector "name" "E")
                                                               (vector "size" 4)))))
            (define arc-f (jsx-create-point primitives-board
                                            (jsx-parents 2 4)
                                            (js-object (vector (vector "name" "F")
                                                               (vector "size" 4)))))
            (define arc-g (jsx-create-point primitives-board
                                            (jsx-parents 3 3)
                                            (js-object (vector (vector "name" "G")
                                                               (vector "size" 4)))))
            (define angle-h (jsx-create-point primitives-board
                                              (jsx-parents 1 -1)
                                              (js-object (vector (vector "name" "H")
                                                                 (vector "size" 4)))))
            (define angle-i (jsx-create-point primitives-board
                                              (jsx-parents 2 -2)
                                              (js-object (vector (vector "name" "I")
                                                                 (vector "size" 4)))))
            (define angle-j (jsx-create-point primitives-board
                                              (jsx-parents 3 -1)
                                              (js-object (vector (vector "name" "J")
                                                                 (vector "size" 4)))))
            (define sector-k (jsx-create-point primitives-board
                                               (jsx-parents 4 3)
                                               (js-object (vector (vector "name" "K")
                                                                  (vector "size" 4)))))
            (define sector-l (jsx-create-point primitives-board
                                               (jsx-parents 5 4)
                                               (js-object (vector (vector "name" "L")
                                                                  (vector "size" 4)))))
            (define sector-m (jsx-create-point primitives-board
                                               (jsx-parents 6 3)
                                               (js-object (vector (vector "name" "M")
                                                                  (vector "size" 4)))))
            (define circle-center (jsx-create-point primitives-board
                                                    (jsx-parents -4 0)
                                                    (js-object (vector (vector "name" "O")
                                                                       (vector "size" 4)))))
            (define circle-through (jsx-create-point primitives-board
                                                     (jsx-parents -4 1)
                                                     (js-object (vector (vector "name" "P")
                                                                        (vector "size" 4)))))
            (define primitives-line (jsx-create-line primitives-board
                                                     (jsx-parents line-a line-b)))
            (define primitives-segment (jsx-create-segment primitives-board
                                                           (jsx-parents segment-c segment-d)))
            (define primitives-arc (jsx-create-arc primitives-board
                                                   (jsx-parents arc-e arc-f arc-g)))
            (define primitives-angle (jsx-create-angle primitives-board
                                                       (jsx-parents angle-h angle-i angle-j)))
            (define primitives-sector (jsx-create-sector primitives-board
                                                         (jsx-parents sector-k sector-l sector-m)))
            (define primitives-circle (jsx-create-circle primitives-board
                                                         (jsx-parents circle-center circle-through)))
            (define primitives-glider (jsx-create-glider primitives-board
                                                         (jsx-parents -4 1 primitives-circle)))
            (void primitives-line)
            (void primitives-segment)
            (void primitives-arc)
            (void primitives-angle)
            (void primitives-sector)
            (void primitives-circle)
            (void primitives-glider))
          (set! primitives-board-ready? #t))
        (void (jsx-board-full-update! primitives-board))
        #t)))

;; init-arrowparallel-board! : -> boolean?
;;   Build the JSXGraph arrowparallel board once the browser assets have loaded.
(define (init-arrowparallel-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless arrowparallel-board-ready?
          (set! arrowparallel-board
                (jsx-create-board
                 arrowparallel-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-1 9 9 -1])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! arrowparallel-p1
                (jsx-create-point arrowparallel-board
                                  (jsx-parents 0 2)
                                  (js-object (vector (vector "name" "P1")
                                                     (vector "size" 4)))))
          (set! arrowparallel-p2
                (jsx-create-point arrowparallel-board
                                  (jsx-parents 2 1)
                                  (js-object (vector (vector "name" "P2")
                                                     (vector "size" 4)))))
          (set! arrowparallel-segment
                (jsx-create-segment arrowparallel-board
                                    (jsx-parents arrowparallel-p1 arrowparallel-p2)))
          (set! arrowparallel-p3
                (jsx-create-point arrowparallel-board
                                  (jsx-parents 3 3)
                                  (js-object (vector (vector "name" "P3")
                                                     (vector "size" 4)))))
          (set! arrowparallel-object
                (jsx-create-arrowparallel arrowparallel-board
                                          (jsx-parents arrowparallel-p1
                                                       arrowparallel-p2
                                                       arrowparallel-p3)))
          (set! arrowparallel-board-ready? #t))
        (void (jsx-board-full-update! arrowparallel-board))
        #t)))

;; init-constructions-board! : -> boolean?
;;   Build the JSXGraph construction board once the browser assets have loaded.
(define (init-constructions-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless constructions-board-ready?
          (set! constructions-board
                (jsx-create-board
                 constructions-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-8 8 8 -8])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (let ()
            (define conic-board-object
              (jsx-create-conic constructions-board
                                (jsx-parents 1 2 -4 0 0 0)))
            (define ellipse-a (jsx-create-point constructions-board
                                                (jsx-parents -4 4)
                                                (js-object (vector (vector "name" "A")
                                                                   (vector "size" 4)))))
            (define ellipse-b (jsx-create-point constructions-board
                                                (jsx-parents -1 5)
                                                (js-object (vector (vector "name" "B")
                                                                   (vector "size" 4)))))
            (define ellipse-c (jsx-create-point constructions-board
                                                (jsx-parents 2 4)
                                                (js-object (vector (vector "name" "C")
                                                                   (vector "size" 4)))))
            (define ellipse-board-object
              (jsx-create-ellipse constructions-board
                                  (jsx-parents ellipse-a ellipse-b ellipse-c)))
            (define functiongraph-board-object
              (jsx-create-functiongraph
               constructions-board
               (jsx-parents (lambda (x) (- (* 0.4 x x) (* 1.5 x)))
                            -3
                            4)))
            (define polygon-a (jsx-create-point constructions-board
                                               (jsx-parents -6 -4)
                                               (js-object (vector (vector "name" "D")
                                                                  (vector "size" 4)))))
            (define polygon-b (jsx-create-point constructions-board
                                               (jsx-parents -4 -1)
                                               (js-object (vector (vector "name" "E")
                                                                  (vector "size" 4)))))
            (define polygon-c (jsx-create-point constructions-board
                                               (jsx-parents -2 -4)
                                               (js-object (vector (vector "name" "F")
                                                                  (vector "size" 4)))))
            (define polygon-d (jsx-create-point constructions-board
                                               (jsx-parents -3 -6)
                                               (js-object (vector (vector "name" "G")
                                                                  (vector "size" 4)))))
            (define polygon-board-object
              (jsx-create-polygon constructions-board
                                  (jsx-parents polygon-a polygon-b polygon-c polygon-d)))
            (define midpoint-a (jsx-create-point constructions-board
                                                 (jsx-parents 3 5)
                                                 (js-object (vector (vector "name" "H")
                                                                    (vector "size" 4)))))
            (define midpoint-b (jsx-create-point constructions-board
                                                 (jsx-parents 5 2)
                                                 (js-object (vector (vector "name" "I")
                                                                    (vector "size" 4)))))
            (define midpoint-board-object
              (jsx-create-midpoint constructions-board
                                   (jsx-parents midpoint-a midpoint-b)))
            (define relation-a (jsx-create-point constructions-board
                                                 (jsx-parents 1 0)
                                                 (js-object (vector (vector "name" "J")
                                                                    (vector "size" 4)))))
            (define relation-b (jsx-create-point constructions-board
                                                 (jsx-parents 4 1)
                                                 (js-object (vector (vector "name" "K")
                                                                    (vector "size" 4)))))
            (define relation-c (jsx-create-point constructions-board
                                                 (jsx-parents 2 4)
                                                 (js-object (vector (vector "name" "L")
                                                                    (vector "size" 4)))))
            (define relation-d (jsx-create-point constructions-board
                                                 (jsx-parents 5 4)
                                                 (js-object (vector (vector "name" "M")
                                                                    (vector "size" 4)))))
            (define relation-e (jsx-create-point constructions-board
                                                 (jsx-parents 1 -3)
                                                 (js-object (vector (vector "name" "N")
                                                                    (vector "size" 4)))))
            (define relation-f (jsx-create-point constructions-board
                                                 (jsx-parents 4 -1)
                                                 (js-object (vector (vector "name" "O")
                                                                    (vector "size" 4)))))
            (define relation-line (jsx-create-line constructions-board
                                                   (jsx-parents relation-a relation-b)))
            (define relation-line-2 (jsx-create-line constructions-board
                                                     (jsx-parents relation-c relation-d)))
            (define relation-circle (jsx-create-circle constructions-board
                                                       (jsx-parents relation-e relation-f)))
            (define relation-parallel
              (jsx-create-parallel constructions-board
                                   (jsx-parents relation-line relation-c)))
            (define relation-perpendicular
              (jsx-create-perpendicular constructions-board
                                        (jsx-parents relation-line relation-d)))
            (define relation-reflection
              (jsx-create-reflection constructions-board
                                     (jsx-parents relation-c relation-line)))
            (define relation-bisector
              (jsx-create-bisector constructions-board
                                   (jsx-parents relation-a relation-c relation-e)))
            (define relation-normal
              (jsx-create-normal constructions-board
                                 (jsx-parents relation-circle relation-d)))
            (define relation-intersection
              (jsx-create-intersection constructions-board
                                       (jsx-parents relation-line relation-line-2)))
            (void conic-board-object)
            (void ellipse-board-object)
            (void functiongraph-board-object)
            (void polygon-board-object)
            (void midpoint-board-object)
            (void relation-parallel)
            (void relation-perpendicular)
            (void relation-reflection)
            (void relation-bisector)
            (void relation-normal)
            (void relation-intersection)
          )
          (set! constructions-board-ready? #t))
        (void (jsx-board-full-update! constructions-board))
        #t)))

;; init-widgets-board! : -> boolean?
;;   Build the JSXGraph widgets board once the browser assets have loaded.
(define (init-widgets-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless widgets-board-ready?
          (set! widgets-board
                (jsx-create-board
                 widgets-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (let ()
            (define widget-button
              (jsx-create-button
               widgets-board
               (jsx-parents -4 4 "Click me"
                            (procedure->external
                             (lambda ()
                               (set-summary! "The JSXGraph button on the widget board was clicked."))))
               (js-object
                (vector (vector "name" "button")))))
            (define widget-checkbox
              (jsx-create-checkbox widgets-board
                                   (jsx-parents -4 2 "Show me")
                                   (js-object
                                    (vector (vector "name" "checkbox")))))
            (define widget-input
              (jsx-create-input widgets-board
                                (jsx-parents -4 0 "x^2" "f(x)=")
                                (js-object
                                 (vector (vector "cssStyle" "width: 5em")))))
            (define widget-slider
              (jsx-create-slider widgets-board
                                 (jsx-parents (jsx-parents -5 -3)
                                              (jsx-parents 1 -3)
                                              (jsx-parents -2 0 2))
                                 (js-object
                                  (vector (vector "name" "t")
                                          (vector "strokeColor" "black")
                                          (vector "fillColor" "white")))))
            (define widget-text
              (jsx-create-text widgets-board
                               (jsx-parents -4 -5 "Widget board: use the input, checkbox, slider, and button.")
                               (js-object
                                (vector (vector "fontSize" 14)
                                        (vector "anchorX" "left")))))
            (void widget-button)
            (void widget-checkbox)
            (void widget-input)
            (void widget-slider)
            (void widget-text))
          (set! widgets-board-ready? #t))
        (void (jsx-board-full-update! widgets-board))
        #t)))

;; init-annotation-board! : -> boolean?
;;   Build the JSXGraph annotation board once the browser assets have loaded.
(define (init-annotation-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless annotation-board-ready?
          (set! annotation-board
                (jsx-create-board
                 annotation-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (let ()
            (set! annotation-slider
                  (jsx-create-slider
                   annotation-board
                   (jsx-parents (jsx-parents -5 -5)
                                (jsx-parents 2 -5)
                                (jsx-parents 0 0 5))
                   (js-object
                    (vector (vector "name" "v")
                            (vector "strokeColor" "black")
                            (vector "fillColor" "white")))))
            (set! annotation-text
                  (jsx-create-text
                   annotation-board
                   (jsx-parents
                    -5
                    4
                    (procedure->external
                     (lambda ()
                       (format "Annotation board: slider value = ~a. Move the slider below to update this text."
                               (jsx-slider-value annotation-slider)))))
                   (js-object
                    (vector (vector "fontSize" 14)
                            (vector "anchorX" "left")))))
            (define annotation-image
              (jsx-create-image
               annotation-board
               (jsx-parents
                "https://jsxgraph.org/jsxgraph/distrib/images/uccellino.jpg"
                (jsx-parents -4 -3)
                (jsx-parents 2 2))
               (js-object
                (vector (vector "name" "image")))))
            (void annotation-text)
            (void annotation-image))
          (set! annotation-board-ready? #t))
        (void (jsx-board-full-update! annotation-board))
        #t)))

;; init-boards! : -> boolean?
;;   Build both gallery boards once the browser assets have loaded.
(define (init-boards!)
  (define geometry-ready? (init-geometry-board!))
  (define group-ready? (init-group-board!))
  (define chart-ready? (init-chart-board!))
  (define primitives-ready? (init-primitives-board!))
  (define arrowparallel-ready? (init-arrowparallel-board!))
  (define constructions-ready? (init-constructions-board!))
  (define widgets-ready? (init-widgets-board!))
  (define annotation-ready? (init-annotation-board!))
  (when (and geometry-ready? group-ready? chart-ready?
             primitives-ready? arrowparallel-ready? constructions-ready? widgets-ready? annotation-ready?
             geometry-board-ready? group-board-ready? chart-board-ready?
             primitives-board-ready? arrowparallel-board-ready? constructions-board-ready? widgets-board-ready? annotation-board-ready?)
    (set-status! "Boards ready.")
    (set-summary!
     (format "Created geometry, group, chart, primitive, arrowparallel, construction, widget, and annotation boards. Geometry objects: ~a. Group objects: ~a. Chart objects: ~a. Primitive objects: ~a. Arrowparallel objects: ~a. Construction objects: ~a. Widget objects: ~a. Annotation objects: ~a."
             (jsx-board-num-objects geometry-board)
             (jsx-board-num-objects group-board)
             (jsx-board-num-objects chart-board)
             (jsx-board-num-objects primitives-board)
             (jsx-board-num-objects arrowparallel-board)
             (jsx-board-num-objects constructions-board)
             (jsx-board-num-objects widgets-board)
             (jsx-board-num-objects annotation-board))))
  (and geometry-ready? group-ready? chart-ready?
       primitives-ready? arrowparallel-ready? constructions-ready? widgets-ready? annotation-ready?
       geometry-board-ready? group-board-ready? chart-board-ready?
       primitives-board-ready? arrowparallel-board-ready? constructions-board-ready? widgets-board-ready? annotation-board-ready?))

;; refresh-gallery! : -> void?
;;   Force a redraw of the live gallery boards.
(define (refresh-gallery!)
  (when geometry-board
    (jsx-board-full-update! geometry-board))
  (when group-board
    (jsx-board-full-update! group-board))
  (when chart-board
    (jsx-board-full-update! chart-board))
  (when primitives-board
    (jsx-board-full-update! primitives-board))
  (when arrowparallel-board
    (jsx-board-full-update! arrowparallel-board))
  (when constructions-board
    (jsx-board-full-update! constructions-board))
  (when widgets-board
    (jsx-board-full-update! widgets-board))
  (when annotation-board
    (jsx-board-full-update! annotation-board))
  (set-status! "Boards refreshed.")
  (set-summary! "The live gallery boards were redrawn, including the new class examples."))

(define jsx-graph-gallery-app
  (window
   (container #:style "max-width: 1100px;"
    (vpanel
     (h1 "JSXGraph Gallery")
     (text "A small browser gallery for the jsx-graph wrapper.")
     (text "It shows the jsx-graph classes in several separate boards.")
     (text "Geometry board: drag the free points and the arrow, slider, and smart label should update together.")
     (container #:id geometry-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Group board: drag P freely while Q follows it as the grouped point.")
     (container #:id group-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Chart board: the bars and legend should match the same data set.")
     (container #:id chart-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Primitive board: line, segment, arc, angle, sector, circle, and glider each show a separate primitive.")
     (container #:id primitives-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Arrowparallel board: the three points and segment define a single arrowparallel example.")
     (container #:id arrowparallel-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Construction board: conic, ellipse, function graph, polygon, midpoint, parallel, perpendicular, reflection, bisector, normal, and intersection demonstrate the higher-level constructors.")
     (container #:id constructions-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Widget board: the button, checkbox, input, and slider show the interactive widget wrappers.")
     (container #:id widgets-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Annotation board: text and image show the annotation-style wrappers.")
     (container #:id annotation-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (hpanel
      (button "Refresh gallery" refresh-gallery!))
     (P @status)
     (P @summary)))))

(define app-renderer
  (render jsx-graph-gallery-app))

(mount-renderer! app-renderer)

(define (pump-init!)
  (cond
    [(init-boards!) (void)]
    [else
     (void
      (js-window-set-timeout/delay
       (procedure->external pump-init!)
       100.))]))

(define (main)
  (ensure-jsxgraph-assets!)
  (void (pump-init!)))

(main)
