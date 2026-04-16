;;;
;;; Minimal JSXGraph Demo
;;;

(include-lib web-easy)
(include-lib document)
(include-lib jsx-graph)

(define board-id "jsx-graph-minimal-board")
(define @status (@ "Loading JSXGraph assets..."))

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

;; ensure-jsxgraph-assets! : -> void?
;;   Load the JSXGraph CSS and core script with a local fallback.
(define (ensure-jsxgraph-assets!)
  (define target
    (or (document-head)
        (document-body)))

  ;; CDN first, local fallback so the example still works when served offline.
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

;; init-board! : -> void?
;;   Build the JSXGraph board once the browser assets have loaded.
(define (init-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (cond
    [(not (extern-present? jxg))
     #f]
    [else
     (let ([jsxgraph (js-ref jxg "JSXGraph")])
       (if (not (extern-present? jsxgraph))
           #f
           (let ()
             (void
              (set! current-board
                    (jsx-create-board
                     board-id
                     (js-object
                      (vector (vector "boundingbox" #[-5 5 5 -5])
                              (vector "axis" #t)
                              (vector "keepaspectratio" #t))))))
             (void
              (set! current-p
                    (jsx-create-point current-board
                                      (jsx-parents -2 1)
                                      (js-object (vector (vector "name" "P")
                                                         (vector "size" 4))))))
             (void
              (set! current-q
                    (jsx-create-point current-board
                                      (jsx-parents 2 2)
                                      (js-object (vector (vector "name" "Q")
                                                         (vector "size" 4))))))
             (void
              (jsx-create-line current-board (jsx-parents current-p current-q)))
             (void (jsx-board-full-update! current-board))
             (void (set-status! "Board ready. points=P,Q."))
             (void))))]))

;; refresh-board! : -> void?
;;   Force a full redraw of the live board.
(define (refresh-board!)
  (when current-board
    (jsx-board-full-update! current-board)
    (set-status! "Board refreshed. points=P,Q."))
  (void))

(define jsx-graph-app
  (window
   (container #:style "max-width: 720px;"
    (vpanel
     (h1 "Minimal JSXGraph Demo")
     (text "A tiny browser example for the jsx-graph wrapper.")
     (text "It creates a board, two draggable points, and a line between them.")
     (container #:id board-id
                #:class "jxgbox"
                #:attrs '((style "width: 500px; height: 320px;")))
     (button "Refresh board" refresh-board!)
     (P @status)))))

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
