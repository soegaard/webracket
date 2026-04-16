;;;
;;; JSXGraph Demo
;;;

(include-lib web-easy)
(include-lib jsx-graph)

(define board-id "jsx-graph-board")
(define @status (@ "Loading JSXGraph assets..."))
(define @summary (@ "Waiting for the board to initialize."))

(define current-board #f)
(define current-p #f)
(define current-q #f)
(define board-init-ext #f)

;; extern-present? : any/c -> boolean?
;;   Check whether a JS value is a live external value.
(define (extern-present? v)
  (external? v))

;; set-status! : string? -> void?
;;   Update the status line in the demo.
(define (set-status! s)
  (obs-set! @status s))

;; set-summary! : string? -> void?
;;   Update the summary line in the demo.
(define (set-summary! s)
  (obs-set! @summary s))

;; ensure-jsxgraph-assets! : -> void?
;;   Load the JSXGraph CSS and core script with a local fallback.
(define (ensure-jsxgraph-assets!)
  (define head (js-document-head))
  (define body (js-document-body))
  (define target (if (extern-present? head) head body))

  ;; CDN first, local fallback so the example still works when served offline.
  (define cdn-css "https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraph.css")
  (define cdn-js  "https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraphcore.js")
  (define local-css "../../web-site-new/local/assets/vendor/jsxgraph/jsxgraph.css")
  (define local-js  "../../web-site-new/local/assets/vendor/jsxgraph/jsxgraphcore.js")

  (unless (extern-present? (js-query-selector "link[data-jsxgraph-css='1']"))
    (define link (js-create-element "link"))
    (js-set-attribute! link "rel" "stylesheet")
    (js-set-attribute! link "href" cdn-css)
    (js-set-attribute! link "data-jsxgraph-css" "1")
    (define on-css-error
      (procedure->external
       (lambda (_evt)
         (js-set-attribute! link "href" local-css))))
    (js-add-event-listener! link "error" on-css-error)
    (js-append-child! target link))

  (unless (extern-present? (js-query-selector "script[data-jsxgraph-core='1']"))
    (define script (js-create-element "script"))
    (js-set-attribute! script "src" cdn-js)
    (js-set-attribute! script "data-jsxgraph-core" "1")
    (define on-js-error
      (procedure->external
       (lambda (_evt)
         (js-set-attribute! script "src" local-js))))
    (js-add-event-listener! script "error" on-js-error)
    (js-append-child! target script)))

;; init-board! : -> void?
;;   Build the JSXGraph board once the browser assets have loaded.
(define (init-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (if (not (extern-present? jxg))
      #f
      (let ()
        (define jsxgraph (js-ref jxg "JSXGraph"))
        (if (not (extern-present? jsxgraph))
            #f
            (let ()
              (define line
                (jsx-create-line current-board (jsx-parents current-p current-q)))
              (define segment
                (jsx-create current-board 'segment (jsx-parents current-p current-q)))
              (define circle
                (jsx-create-circle current-board (jsx-parents current-p current-q)))
              (define label
                (jsx-create current-board
                            'text
                            (jsx-parents -6 6 "P-Q constructors")
                            (js-object (vector))))
              (void
               (set! current-board
                     (jsx-create-board
                      board-id
                      (js-object
                       (vector (vector "boundingbox" #[-7 7 7 -7])
                               (vector "axis" #t)
                               (vector "keepaspectratio" #t))))))
              (void
               (set! current-p
                     (jsx-create-point current-board
                                       (jsx-parents -4 1)
                                       (js-object (vector (vector "name" "P")
                                                          (vector "size" 4))))))
              (void
               (set! current-q
                     (jsx-create-point current-board
                                       (jsx-parents 2 3)
                                       (js-object (vector (vector "name" "Q")
                                                          (vector "size" 4))))))
              (void (jsx-board-suspend-update! current-board))
              (void (jsx-set-point-size! current-p 5.0))
              (void (jsx-set-point-size! current-q 5.0))
              (void (jsx-board-unsuspend-update! current-board))
              (define child-count (jsx-board-count-children current-board))
              (void (jsx-board-full-update! current-board))
              (void (set-status! "Board ready."))
              (void
               (set-summary!
                (format "Created a line, a segment, a circle, and a text label. children=~a. P = (~a, ~a), Q = (~a, ~a)."
                        child-count
                        (jsx-point-x current-p)
                        (jsx-point-y current-p)
                        (jsx-point-x current-q)
                        (jsx-point-y current-q))))
              (void line segment circle label))))))

;; refresh-board! : -> void?
;;   Force a full redraw of the live board.
(define (refresh-board!)
  (when current-board
    (jsx-board-full-update! current-board)
    (set-status! "Board refreshed.")
    (set-summary! "The live board was redrawn."))
  (void))

(define jsx-graph-app
  (window
   (container #:style "max-width: 960px;"
    (vpanel
     (h1 "JSXGraph Demo")
     (text "A small browser example for the jsx-graph wrapper.")
     (text "It creates a board, two draggable points, a line, a segment, a circle, and a text label.")
     (container #:id board-id
                #:class "jxgbox"
                #:attrs '((style "width: 640px; height: 420px;")))
     (hpanel
      (button "Refresh board" refresh-board!))
     (P @status)
     (P @summary)))))

(define app-renderer
  (render jsx-graph-app))

(mount-renderer! app-renderer)

(define (pump-init!)
  (cond
    [(init-board!) (void)]
    [else
     (void (set! board-init-ext (procedure->external pump-init!)))
     (void (js-window-set-timeout/delay board-init-ext 100.))]))

(define (main)
  (ensure-jsxgraph-assets!)
  (void (pump-init!)))

(main)
