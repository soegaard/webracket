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

(define (replace-first xs x y [equiv? equal?])
  (let loop ([xs xs])
    (cond
      [(null? xs)          '()]
      [(equiv? (car xs) x) (cons y       (rest xs))]
      [else                (cons x (loop (rest xs)))])))


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

(define (clear-canvas! ctx)
  (js-canvas2d-clear-rect ctx 0. 0. canvas-width canvas-height))

(define (draw-circle! ctx c selected? [fill-color #f])
  (define radius (/ (circle-diameter c) 2))
  (js-canvas2d-begin-path ctx)
  (js-canvas2d-arc        ctx
                          (inexact (circle-x c))
                          (inexact (circle-y c))
                          (inexact radius)
                          0. (* 2 pi)
                          #f)  ; counter clockwise
  (cond
    [selected?
     (js-set-canvas2d-fill-style!   ctx (or fill-color "#e8cfff"))
     (js-set-canvas2d-stroke-style! ctx "#5f2bab")]
    [else
     (js-set-canvas2d-fill-style!   ctx (or fill-color (circle-fill-color c)))
     (js-set-canvas2d-stroke-style! ctx "#2b5fab")])
  (js-canvas2d-fill   ctx (void) (void))
  (js-canvas2d-stroke ctx (void))
  (void))


(define (client-position->canvas-position client-x client-y)
  (define canvas (get-canvas))
  (define rect   (js-get-bounding-client-rect canvas))

  (define left     (js-dom-rect-left rect))
  (define top      (js-dom-rect-top rect))
  (define width    (js-dom-rect-width rect))
  (define height   (js-dom-rect-height rect))

  (define canvas-width  (js-canvas-width canvas))
  (define canvas-height (js-canvas-height canvas))

  (define x (* (/ (- client-x left) width)  canvas-width))
  (define y (* (/ (- client-y top)  height) canvas-height))

  (values x y))

(define (get-mouse-position evt)
  (client-position->canvas-position
   (mouse-event-client-x evt)
   (mouse-event-client-y evt)))

(define (get-touch-position t)
  (client-position->canvas-position
   (touch-client-x t)
   (touch-client-y t)))

(define (get-mouse-offsets evt)
  (define mx (mouse-event-offset-x evt))
  (define my (mouse-event-offset-y evt))
  (values mx my))


;; ----------------------------------------
;; Data Representation and Formatting      
;; ----------------------------------------

;; Circle

(struct circle (id x y diameter red green blue) #:transparent)

(define default-circle-red   207)
(define default-circle-green 232)
(define default-circle-blue  255)

(define next-id 0)
(define (new-id)
  (begin0 next-id
    (set! next-id (add1 next-id))))

(define (same-circle? c0 c1)
  (equal? (circle-id c0) (circle-id c1)))

(define (make-circle x y [d 40])
  (circle (new-id) x y d
          default-circle-red
          default-circle-green
          default-circle-blue))

(define (make-random-circle)
  (make-circle (inexact (random (exact-floor canvas-width)))
               (inexact (random (exact-floor canvas-height)))))

(define (check-circle who c)
  (unless (circle? c)
    (error who "expected a circle, got: ~a" c)))

(define (set-circle-diameter c d)
  (check-circle 'set-circle-diameter c)
  (struct-copy circle c
    [diameter d]))

(define (set-circle-color c red green blue)
  (check-circle 'set-circle-color c)
  (struct-copy circle c
    [red   red]
    [green green]
    [blue  blue]))

(define (circle-fill-color c)
  (check-circle 'circle-fill-color c)
  (format "rgb(~a, ~a, ~a)"
          (circle-red c)
          (circle-green c)
          (circle-blue c)))

(define (~circle c)
  (check-circle '~circle c)
  (match c
    [(circle id x y diameter red green blue)
     (format "circle(~a, ~a,~a,~a,~a,~a,~a)"
             id x y diameter red green blue)]))

(define (~circles cs)
  (map ~circle cs))

(define (circle-radius c)
  (check-circle 'circle-radius c)
  (/ (circle-diameter c) 2.0))

(define (circle-distance c x y)
  (check-circle 'circle-distance c)
  (sqrt (+ (sqr (- x (circle-x c)))
           (sqr (- y (circle-y c))))))

(define (point-in-circle? x y c)
  (<= (circle-distance c x y)
      (circle-radius c)))

;; Circles

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

(define (remove-circle circles c)
  ; remove circles that has the same id as `c`
  (remove c circles same-circle?))

(define (adjust-circle circles the-circle new-diameter)
  (define adjusted
    (struct-copy circle the-circle
      [diameter new-diameter]))
  (replace-first circles the-circle adjusted same-circle?))

(define (adjust-circle/id circles c-id new-diameter)
  (for/list ([c (in-list circles)])
    (if (equal? (circle-id c) c-id)
        (struct-copy circle c
          [diameter new-diameter])
        c)))

(define (adjust-circle-color/id circles c-id red green blue)
  (for/list ([c (in-list circles)])
    (if (equal? (circle-id c) c-id)
        (set-circle-color c red green blue)
        c)))

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
   (λ (circles)
     (add-circle circles c))))

(define (delete-circle! c)
  (commit-circles!
   (λ (circles)
     (remove-circle circles c))))

(define (adjust-circle/id! c-id new-diameter)
  (commit-circles!
   (λ (circles)
     (adjust-circle/id circles c-id new-diameter))))

(define (adjust-circle-color/id! c-id red green blue)
  (commit-circles!
   (λ (circles)
     (adjust-circle-color/id circles c-id red green blue))))


;; ----------------------------------------
;; App state
;; ----------------------------------------

;; The model state consists of a list of circles.
;; In the app the user can select a circle and choose a task.
;; The user can only work on one selected circle at a time.

;; In the app, there are additional, temporary state
;; associated with each task. 

;; In this app the tasks are "adjusting diameter"
;; and "adjusting color".

;; The state for the tasks are under "App State".


;; Normally a circle will we selected when the mouse cursor
;; enters a circle - or the circle is tapped (touch).
;; The exception is if a task is active.
;; The selected circle never change during an active task.
; A selected circle is rendered in a different color.
(define @selected-circle (@ #f))

;; The selected circle has an `id`.
;; The task can change the selected circle without
;; changing the model in @circles.
;; When the user is done with a task, the final
;; circle in @selected-circle is "commited".
;; I.e. the model is updated with the new circle.


;; Right clicking a selected circle reveals a popup menu
;; with options for adjusting diameter/color or deleting the circle.

;; The popup menu has offsets relative to the upper, left of the canvas.
(struct menu-state (offset-x offset-y) #:transparent)
(define @menu (@ #f))

;; Dialog for adjusting the diameter of a circle
(struct adjust-diameter-state (original-diameter) #:transparent)
(define @adjusting-diameter (@ #f)) ; temporary state while adjusting diameter
(define @diam               (@ 40)) ; diameter for the "Adjust Diameter" slider.

;; Dialog for adjusting the fill color of a circle
(struct adjust-color-state (original-red original-green original-blue) #:transparent)
(define @adjusting-color (@ #f)) ; temporary state while adjusting color
(define @red             (@ default-circle-red))
(define @green           (@ default-circle-green))
(define @blue            (@ default-circle-blue))


;; Task active?
(define @task-active? (@ #f))
(obs-watch! @adjusting-diameter @adjusting-color
  (λ (adjusting-diameter adjusting-color)
    (:= @task-active? (or adjusting-diameter adjusting-color))))

; Update selected circle, unless a task is active.
(define (update-selected-circle-from-point! x y)
  (unless (obs-peek @task-active?)
    (define circles (obs-peek @circles))
    (define closest (find-closest-circle circles x y))
    (:= @selected-circle closest))
  (void))


; Called when a task is done.
(define (clear-app-state!)
  (:= @selected-circle #f)
  (:= @menu            #f)
  (:= @adjusting-diameter #f)
  (:= @adjusting-color    #f))


;; Rendering draws the currrent circles to the canvas.
;; If a there is a selected circle, it gets special treatment.
;; The selected circle is drawn using the app state.
(define (redraw-canvas! circles)
  (define ctx       (get-drawing-context))
  (define selected  (obs-peek @selected-circle))

  (define adjusting-diameter (obs-peek @adjusting-diameter))
  (define diam               (obs-peek @diam))

  (define adjusting-color (obs-peek @adjusting-color))
  (define red             (obs-peek @red))
  (define green           (obs-peek @green))
  (define blue            (obs-peek @blue))

  (when ctx
    (clear-canvas! ctx)    
    (for ([c (in-list circles)])
      (define selected?           ; is the model circle c the same 
        (and (circle? selected)   ; as the currently selected circle?
             (same-circle? c selected))) 
      (cond
        [(and selected? adjusting-diameter)
         (draw-circle! ctx (set-circle-diameter c diam) selected?)]
        [(and selected? adjusting-color)
         (draw-circle! ctx c selected?
                       (format "rgb(~a, ~a, ~a)" red green blue))]
        [else
         (draw-circle! ctx c selected?)]))))


(define (commit-adjustment!)
  ; adjustment is done - commit the adjustment to history
  (js-log "commit-adjustment!")
  (define sc (obs-peek @selected-circle))
  (define as (obs-peek @adjusting-diameter))
  (define d  (obs-peek @diam))
  (when (and sc as d)
    (adjust-circle/id! (circle-id sc) d) ; changes the model
    (:= @adjusting-diameter #f)
    (:= @selected-circle    #f)))

(define (commit-color-adjustment!)
  ; adjustment is done - commit the color adjustment to history
  (js-log "commit-color-adjustment!")
  (define sc    (obs-peek @selected-circle))
  (define as    (obs-peek @adjusting-color))
  (define red   (obs-peek @red))
  (define green (obs-peek @green))
  (define blue  (obs-peek @blue))
  (when (and sc as red green blue)
    (adjust-circle-color/id! (circle-id sc) red green blue)
    (:= @adjusting-color #f)
    (:= @selected-circle #f)))


(obs-watch! @circles @selected-circle @diam @red @green @blue
  (λ (circles selected-circle diam red green blue)
    (redraw-canvas! circles)))






;; ----------------------------------------
;; Views                                   
;; ----------------------------------------

(define (on-click/tap x y)
  (when (not (obs-peek @task-active?))
    ; close any open menus on click/tap
    (:= @menu #f)
    ; find out if the click/tap was inside or outside a circle
    (when (and (number? x) (number? y))
      (define circles (obs-peek @circles))
      (define closest (find-closest-circle circles x y))
      (cond
        [closest ; click is inside an existing circle
         (:= @selected-circle closest)]
        [else    ; click is outside all existing circles
         (add-circle! (make-circle x y))]))
    (update-selected-circle-from-point! x y))
  (void))

(define 7gui-circle-drawer-app
  (window ; #:title "Circle Drawer"
   (container #:style (format "max-width: ~apx;" canvas-width)
     (h1 "Circle Drawer" #:style "text-align: center;")
     (text "Click to create a new circle.")
     (text "Right click on a circle to get a context menu.")
     (vpanel
      ;; Undo and Redo
      (hpanel
       (spacer)
       (button "Undo"
               (λ _
                 (clear-app-state!)
                 (undo!)))
       (button "Redo"
               (λ _
                 (clear-app-state!)
                 (redo!)))
       (spacer))

      ;; The canvas and menu are placed in a shared <div> with `position: relative;`.
      ;; This way, the menus are placed relative to the top, left of the canvas.
      (Div #:style (format "position: relative; overflow: visible;")
           ;; Canvas
           (Canvas #:id             "circle-canvas"
                   #:width          canvas-width
                   #:height         canvas-height
                   #:style          "border: 1px solid #ccc;"

                   #:on-mousemove   (λ (evt)
                                      (define-values (x y) (get-mouse-position evt))
                                      (update-selected-circle-from-point! x y))

                   #:on-contextmenu (λ (evt) ; right click / context menu gesture
                                      (js-log "on-contextmenu")
                                      (prevent-default! evt)
                                      (when (and      (obs-peek @selected-circle)
                                                 (not (obs-peek @task-active?)))
                                        ; The menu position uses offsets. 
                                        ; Measured from the top, left corner of the canvas.
                                        (define-values (mx my) (get-mouse-offsets evt))
                                        (:= @menu (menu-state mx my)))
                                      (void))

                   #:on-click       (λ (evt) ; external - raw mouse event
                                      (js-log "on-click")
                                      (prevent-default! evt)
                                      (define-values (x y) (get-mouse-position evt))
                                      (on-click/tap x y))
                   
                   #:on-touchend    (λ (evt)
                                      (js-log "on-touchend")
                                      (prevent-default! evt)
                                      (define touches (touch-event-changed-touches evt))
                                      (define t       (touch-list-ref touches 0))
                                      (when t
                                        (define-values (x y) (get-touch-position t))                                          
                                        (on-click/tap x y)))                   
                   ) ; Canvas ends here

           ;; Menu: only visible when @selected-circle is a circle
           (observable-view
            @menu
            (lambda (m)
              (cond
                [m
                 (define c (obs-peek @selected-circle))
                 (define d (and (circle? c) (circle-diameter c)))
                 (define red   (and (circle? c) (circle-red c)))
                 (define green (and (circle? c) (circle-green c)))
                 (define blue  (and (circle? c) (circle-blue c)))
                 (Div #:style
                      (format (string-append "position:    absolute;"
                                             "left:        ~apx;"
                                             "top:         ~apx;"
                                             "white-space: nowrap;"
                                             "background:  white;"
                                             "border:      1px solid #999;"
                                             "padding:     6px;"
                                             "z-index:     1000;")
                              (menu-state-offset-x m)
                              (menu-state-offset-y m))           
                      (Button "Adjust diameter"
                              #:on-click
                              (lambda (_evt)
                                (when c
                                  (:= @adjusting-diameter
                                      (adjust-diameter-state d))
                                  (:= @diam d)
                                  (:= @menu #f))))
                      (Button "Adjust color"
                              #:on-click
                              (lambda (_evt)
                                (when c
                                  (:= @adjusting-color
                                      (adjust-color-state red green blue))
                                  (:= @red   red)
                                  (:= @green green)
                                  (:= @blue  blue)
                                  (:= @menu  #f))))
                      (Button "Delete"
                              #:on-click
                              (lambda (_evt)
                                (delete-circle! c)
                                (:= @menu            #f)                                
                                (:= @selected-circle #f))))]
                [else
                 (Span "")]))))
      
      ;; New Button
      #;(hpanel
         (spacer)
         (button "New"
                 (λ ()
                   (add-circle! (make-random-circle))))
         (spacer))

      ;; Adjustable slider - only visible after choosing the menu
      ;;                    item "Adjust diameter"
      (observable-view
            @adjusting-diameter
            (procedure-rename 
             (lambda (a)
               (cond
                 [a (vpanel
                     (heading 3 "Adjust Diameter")
                     (slider @diam
                             (λ (diam) (:= @diam diam))
                             0 100)
                     (hpanel
                      (spacer)
                      (Button "Cancel"
                              #:on-click
                              (lambda (_evt)
                                (clear-app-state!)))
                      (Button "Save"
                              #:on-click
                              (lambda (_evt)
                                (commit-adjustment!)))
                      (spacer)))]
                 [else (text "")]))
             "adjusting-slider"))

      ;; RGB color picker - only visible after choosing the menu
      ;;                    item "Adjust color"
      (observable-view
       @adjusting-color
       (procedure-rename
        (lambda (a)
          (cond
            [a (vpanel
                (heading 3 "Adjust Color")

                (text (@red . ~> . (λ (red) (format "Red: ~a" red))))
                (slider @red
                        (λ (red) (:= @red red))
                        0 255)

                (text (@green . ~> . (λ (green) (format "Green: ~a" green))))
                (slider @green
                        (λ (green) (:= @green green))
                        0 255)

                (text (@blue . ~> . (λ (blue) (format "Blue: ~a" blue))))
                (slider @blue
                        (λ (blue) (:= @blue blue))
                        0 255)

                (hpanel
                 (spacer)
                 (Button "Cancel"
                         #:on-click
                         (lambda (_evt)
                           (clear-app-state!)))
                 (Button "Save"
                         #:on-click
                         (lambda (_evt)
                           (commit-color-adjustment!)))
                 (spacer)))]
            [else (text "")]))
        "adjusting-color-slider"))
      ))))
  
(define app-renderer
  (render 7gui-circle-drawer-app))


;;;
;;; Themes
;;;

;; Note: compile.sh copies the required theme CSS files next to the
;;       generated HTML, so these stylesheet paths are relative to
;;       the generated/ output directory.

;; light-theme : theme?
;;   Shared light theme used by this example.
(define light-theme
  (theme 'light
         "we-theme-light"
         "web-easy-core.css"
         "theme-light.css"
         #f))

(define theme-manager
  (install-theme-manager! light-theme))

;;;
;;; Mount the renderer
;;;

(mount-renderer! app-renderer)
