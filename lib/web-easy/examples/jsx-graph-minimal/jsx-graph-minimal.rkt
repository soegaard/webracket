;;;
;;; Minimal JSXGraph Demo
;;;

(include-lib web-easy)
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
  (cond
    [(not (extern-present? jxg))
     #f]
    [else
     (let ([jsxgraph (js-ref jxg "JSXGraph")])
       (if (not (extern-present? jsxgraph))
           #f
           (begin
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
             (void (set-status! "Board ready."))
             (void))))]))

;; refresh-board! : -> void?
;;   Force a full redraw of the live board.
(define (refresh-board!)
  (when current-board
    (jsx-board-full-update! current-board)
    (set-status! "Board refreshed."))
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
