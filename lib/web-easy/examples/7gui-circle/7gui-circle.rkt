;;;
;;; 7 GUI - Circle
;;;

;; https://eugenkiss.github.io/7guis/tasks#circle

(include/reader "../../main-browser.rkt" read-syntax/skip-first-line)

;; ----------------------------------------
;; Utilities                          
;; ----------------------------------------

(define (indexed xs)
  (for/list ([x (in-list xs)]
             [i (in-naturals)])
    (list i x)))

(define (safe-list-ref xs i)
  (define n (length xs))
  (if (and (integer? i) (<= 0 i) (< i n))
      (list-ref xs i)
      #f))

(define (inexact x)
  (if (exact? x) (exact->inexact x) x))


;; ----------------------------------------
;; Constants
;;-----------------------------------------

(define canvas-width  400.) ; we need floats later
(define canvas-height 300.)

(define canvas-name   "circle-canvas")

;; ----------------------------------------
;; Canvas Utilities
;;-----------------------------------------

(define (get-canvas)
  (define doc (js-var "document"))
  (js-send/extern/nullish doc "getElementById" (vector canvas-name)))

(define (get-drawing-context)
  (define canvas (get-canvas))
  (and canvas 
       (js-canvas-get-context canvas "2d" (js-var "undefined"))))

(define (get-cursor-position event)
  (define canvas   (get-canvas))
  (define rect     (js-get-bounding-client-rect canvas))

  (define client-x (mouse-event-client-x event))
  (define client-y (mouse-event-client-y event))

  (define left     (js-dom-rect-left   rect))
  (define top      (js-dom-rect-top    rect))
  (define width    (js-dom-rect-width  rect))
  (define height   (js-dom-rect-height rect))

  (define canvas-width  (js-canvas-width  canvas))
  (define canvas-height (js-canvas-height canvas))

  (define x (* (/ (- client-x left) width)  canvas-width))
  (define y (* (/ (- client-y top)  height) canvas-height))

  (values x y))


(define (clear-canvas! ctx)
  (js-canvas2d-clear-rect ctx 0. 0. canvas-width canvas-height))

(define (draw-circle! ctx c selected?)
  (define radius (/ (circle-diameter c) 2))
  (js-canvas2d-begin-path ctx)
  (js-canvas2d-arc ctx
                   (inexact (circle-x c))
                   (inexact (circle-y c))
                   (inexact radius)
                   0. (* 2 pi)
                   #f)  ; counter clockwise ?
  (cond
    [selected?
     (js-set-canvas2d-fill-style!   ctx "#e8cfff")
     (js-set-canvas2d-stroke-style! ctx "#5f2bab")]
    [else
     (js-set-canvas2d-fill-style!   ctx "#cfe8ff")
     (js-set-canvas2d-stroke-style! ctx "#2b5fab")])
  (js-canvas2d-fill   ctx (void) (void))
  (js-canvas2d-stroke ctx (void))
  (void))


;; ----------------------------------------
;; Data Representation and Formatting      
;; ----------------------------------------

;; Circles

(struct circle (x y diameter) #:transparent)

(define (make-circle x y)
  (circle x y 40))

(define (make-random-circle)
  (make-circle (inexact (random (exact-floor canvas-width)))
               (inexact (random (exact-floor canvas-height)))))

(define (check-circle who c)
  (unless (circle? c)
    (error who "expected a circle, got: ~a" c)))

(define (~circle c)
  (check-circle '~circle c)
  (match c
    [(circle x y diameter)
     (format "circle(~a,~a,~a)" x y diameter)]))

(define (~circles cs)
  (map ~circle cs))

(define (circle-radius c)
  (/ (circle-diameter c) 2.0))

(define (circle-distance c x y)
  (sqrt (+ (sqr (- x (circle-x c)))
           (sqr (- y (circle-y c))))))

(define (point-in-circle? x y c)
  (<= (circle-distance c x y)
      (circle-radius c)))

(define (find-closest-circle circles x y)
  (define-values (closest closest-dist)
    (for/fold ([closest      #f]
               [closest-dist +inf.0])
              ([c (in-list circles)])      
      (define dist (circle-distance c x y))
      (if (< dist closest-dist)
          (values c dist)
          (values closest closest-dist))))
  (cond
    [(not closest) #f]
    [else
     (define r (circle-radius closest))
     (and (<= closest-dist r)
          closest)]))

(define (add-circle circles c)
  (append circles (list c)))


;; History

(struct history (prev current next) #:transparent)

(define (add-history h circles)
  (match-define (history prev current _) h)
  (cond
    [(equal? circles current)
     h]
    [else
     (struct-copy history h
                  [prev    (cons current prev)]
                  [current circles]
                  [next    '()])]))

(define (history-undo h)
  (match-define (history prev current next) h)
  (if (pair? prev)
      (history (cdr prev) (car prev) (cons current next))
      h))

(define (history-redo h)
  (match-define (history prev current next) h)
  (if (pair? next)
      (history (cons current prev) (car next) (cdr next))
      h))

;; ----------------------------------------
;; Model                                   
;; ----------------------------------------

(define initial-circles '())

; Application Model
(define @history (obs (history '() initial-circles '())))

; Domain Model
(define @circles (@history . ~> . history-current))

; Making @circles depend on @history gives us a single source of truth.


; commit-circles! : (circles -> circles) -> (void)
;   Apply `f` to @circles and get the next state.
;   Update the history.
(define (commit-circles! f)
  (@history . <~ . (λ (h)
                     (define cs      (history-current h))
                     (define new-cs  (f cs))
                     (if (equal? cs new-cs)
                         h
                         (add-history h new-cs))))
  (void))

(define (undo!)
  (define h (obs-peek @history))
  (:= @history (history-undo h))
  (void))

(define (redo!)
  (define h (obs-peek @history))
  (:= @history (history-redo h))
  (void))

(define (add-circle! c)
  (commit-circles!
   (λ (circles) (add-circle circles c))))


;; ----------------------------------------
;; App state
;; ----------------------------------------

; If the mouse pointer enters a circle, it is selected.
; A selected circle will be rendered in a different color.
(define @selected-circle (@ #f))

; Right clicking a selected circle reveals a popup menu
; with two options: "Adjust Diameter" and "Delete".

;; The popup menu has offsets relative to the upper, left of the canvas.
(struct menu-state (offset-x offset-y selected-circle) #:transparent)
(define @menu      (@ #f))


(define (redraw-canvas! circles)
  (define ctx      (get-drawing-context))
  (define selected (obs-peek @selected-circle))
  (when ctx
    (clear-canvas! ctx)    
    (for ([c (in-list circles)])
      (draw-circle! ctx c (eq? c selected)))
    (void)))

(obs-observe! @circles
              (λ (circles)
                (redraw-canvas! circles)))

(obs-observe! @selected-circle
              (λ (c)
                (redraw-canvas! (obs-peek @circles))))



;; ----------------------------------------
;; Views                                   
;; ----------------------------------------

(define (all-events _) #t)
(define (log-event e) (js-log (format "~a" e)))

(define 7gui-circle-drawer-app
  (window ; #:title "Circle Drawer"
   (container #:style (format "max-width: ~apx;" canvas-width)
     (h1 "Circle Drawer")
     (vpanel
      ;; Undo and Redo
      (hpanel
       (spacer)
       (button "Undo"
               (λ _
                 (with-handlers ([all-events log-event])
                   (js-log "undo")
                   (undo!)
                   (void))))
       (button "Redo"
               (λ _
                 (js-log "redo")
                 (redo!)
                 (void)))
       (spacer))

      ;; The canvas and menu are placed in a shared <div> with `position: relative;`.
      ;; This way, the menus are placed relative to the top, left of the canvas.
      (Div #:style (format "position: relative; overflow: visible;")
           ;; Canvas
           (Canvas #:id             "circle-canvas"
                   #:width          canvas-width
                   #:height         canvas-height
                   #:style          "border: 1px solid #ccc;"


                   #:on-contextmenu (lambda (evt)
                                      (with-handlers ([all-events log-event])
                                        (js-log "on-contextmenu")
                                        (prevent-default! evt)

                                        ; The menu position uses offsets.
                                        ; The offsets are from the top, left corner of the canvas.
                                        (define mx (mouse-event-offset-x evt))
                                        (define my (mouse-event-offset-y evt))
                                        
                                        ; Circle centers use canvas coordinates.
                                        (define-values (x y) (get-cursor-position evt))
                                        (define circles (obs-peek @circles))
                                        (define c (find-closest-circle circles x y))
                                        (js-log (format "circle: ~a" c))
                                        (if c
                                            (:= @menu (menu-state mx my c))
                                            (:= @menu #f))
                                        (void)))

                   #:on-mouseup     (λ (evt) ; external - raw mouse event
                                      (prevent-default! evt)
                                      (js-log "on-mouseup")
                                      (with-handlers ([all-events log-event])
                                        (define-values (x y) (get-cursor-position evt))
                                        (js-log (format "mouseup at (~a, ~a)" x y))
                                        (when (and (number? x) (number? y))
                                          (define circles (obs-peek @circles))
                                          (define closest (find-closest-circle circles x y))
                                          (cond
                                            [closest ; click is inside an existing circle
                                             (js-log (format "inside: ~a" closest))
                                             (:= @selected-circle closest)]
                                            [else    ; click is outside all existing circles
                                             (add-circle! (make-circle x y))]))
                                        (void)))
                   ) ; Canvas ends here
           (observable-view
            @menu
            (lambda (m)
              (cond
                [m
                 (Div #:style
                      (format (string-append "position:   absolute;"
                                             "left:       ~apx;"
                                             "top:        ~apx;"
                                             "white-space: nowrap;"
                                             "background: white;"
                                             "border:     1px solid #999;"
                                             "padding:    6px;"
                                             "z-index:    1000;")
                              (menu-state-offset-x m)
                              (menu-state-offset-y m))
                      (Button "Adjust diameter"
                              #:on-click
                              (lambda (_evt)
                                (define c (obs-peek @selected-circle))
                                (js-log (format "adjust circle ~a" c))
                                (:= @menu #f)))
                      (Button "Delete"
                              #:on-click
                              (lambda (_evt)
                                (define c (obs-peek @selected-circle))
                                (js-log (format "delete circle ~a" c))
                                (:= @menu #f))))]
                [else
                 (Span "")]))))

      (hpanel
       (spacer)
       (button "New"
               (λ ()
                 (with-handlers ([all-events log-event])
                   (add-circle! (make-random-circle)))))
       (spacer))

      ))))
  
(define app-renderer
  (render 7gui-circle-drawer-app))


;;;
;;; Themes
;;;

;; Note: The paths to the theme css files assume the
;;       web-server was started in lib/web-easy/

;; install-theme-link! : string? -> any/c
;;   Create and attach a stylesheet link element in <head>.
(define (install-theme-link! link-id)
  (define doc  (js-var "document"))
  (define head (js-ref/extern doc "head"))
  (define link (js-create-element "link"))
  (js-set-attribute! link "id"  link-id)
  (js-set-attribute! link "rel" "stylesheet")
  (js-append-child! head link)
  link)

;; apply-light-theme! : any/c any/c -> void?
;;   Set root class and stylesheet hrefs for Light theme.
(define (apply-light-theme! core-link light-link)
  (define html-node (js-ref/extern (js-document-body) "parentElement"))
  (js-set-attribute! html-node  "class" "we-theme-light")
  (js-set-attribute! core-link  "href"  "web-easy-core.css")
  (js-set-attribute! light-link "href"  "theme-external-light.css")
  (void))

(define theme-core-link-node  (install-theme-link! "we-theme-core-css"))
(define theme-light-link-node (install-theme-link! "we-theme-external-css"))
(apply-light-theme! theme-core-link-node theme-light-link-node)

;;;
;;; Mount the renderer
;;;

(mount-renderer! app-renderer)
