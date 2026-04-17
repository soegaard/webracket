;;;
;;; JSXGraph Gallery
;;;

(include-lib web-easy)
(include-lib event)
(include-lib document)
(include-lib jsx-graph)

(define geometry-board-id "jsx-graph-gallery-geometry")
(define chart-board-id "jsx-graph-gallery-chart")

(define @status (@ "Loading JSXGraph assets..."))
(define @summary (@ "Waiting for the boards to initialize."))

(define geometry-board #f)
(define geometry-p #f)
(define geometry-q #f)
(define geometry-arrow #f)
(define geometry-slider #f)
(define geometry-label #f)

(define chart-board #f)
(define chart #f)
(define chart-legend #f)

(define geometry-board-ready? #f)
(define chart-board-ready? #f)

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

;; init-boards! : -> boolean?
;;   Build both gallery boards once the browser assets have loaded.
(define (init-boards!)
  (define geometry-ready? (init-geometry-board!))
  (define chart-ready? (init-chart-board!))
  (when (and geometry-ready? chart-ready? geometry-board-ready? chart-board-ready?)
    (set-status! "Boards ready.")
    (set-summary!
     (format "Created an arrow, a slider, a smart label, a chart, and a legend. Geometry objects: ~a. Chart objects: ~a."
             (jsx-board-num-objects geometry-board)
             (jsx-board-num-objects chart-board))))
  (and geometry-ready? chart-ready? geometry-board-ready? chart-board-ready?))

;; refresh-gallery! : -> void?
;;   Force a redraw of the live gallery boards.
(define (refresh-gallery!)
  (when geometry-board
    (jsx-board-full-update! geometry-board))
  (when chart-board
    (jsx-board-full-update! chart-board))
  (set-status! "Boards refreshed.")
  (set-summary! "The live gallery boards were redrawn."))

(define jsx-graph-gallery-app
  (window
   (container #:style "max-width: 1100px;"
    (vpanel
     (h1 "JSXGraph Gallery")
     (text "A small browser gallery for the jsx-graph wrapper.")
     (text "It shows the newer constructor classes in two separate boards.")
     (container #:id geometry-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (container #:id chart-board-id
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
