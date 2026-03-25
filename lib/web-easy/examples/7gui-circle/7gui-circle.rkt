;;;
;;; 7 GUI - Circle
;;;

;; https://eugenkiss.github.io/7guis/tasks#circle

(include/reader "../../main-browser.rkt" read-syntax/skip-first-line)

;; ----------------------------------------
;; List Utilities                          
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


;; ----------------------------------------
;; Data Representation and Formatting      
;; ----------------------------------------

;; Circles

(struct circle (x y diameter) #:transparent)

(define (make-circle x y)
  (circle x y 40))

(define (make-random-circle)
  (make-circle (random 10) (random 10)))

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


(define (circle-distance c x y)
  (sqrt (+ (sqr (- x (circle-x c)))
           (sqr (- y (circle-y c))))))

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
     (define d (circle-diameter closest))
     (and (<= closest-dist (/ d 2))
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

(define initial-circles
  (list (make-circle 0 0)
        (make-circle 1 0)
        (make-circle 0 2)
        (make-circle 3 7)))

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
  (:= @selected-circle-index #f)
  (void))

(define (redo!)
  (define h (obs-peek @history))
  (:= @history (history-redo h))
  (:= @selected-circle-index #f)
  (void))

(define (add-circle! c)
  (commit-circles!
   (λ (circles) (add-circle circles c))))


;; ----------------------------------------
;; App state
;; ----------------------------------------

;; List Group 
(define @circles-formatted     (@circles           . ~> . ~circles))
(define @circles-indexed       (@circles-formatted . ~> . indexed))
(define @selected-circle-index (@ #f))
(define @selected-circle-desc  (obs-combine
                                (λ (indexed-circles circle-index)
                                  (define ic (safe-list-ref indexed-circles circle-index))
                                  (and ic (second ic)))
                                @circles-indexed @selected-circle-index))

;; Debug: Log selected 
(obs-observe! @selected-circle-desc
              (λ (c) (js-log (format "~a" c))))


;; ----------------------------------------
;; Views                                   
;; ----------------------------------------


(define 7gui-circle-app
  (window ; #:title "Circle"
   (container #:style "max-width: 500px;"
     (h1 "Circle")
     (vpanel
      #; (Canvas #:height 200 #:width 200)

      (hpanel
       (spacer)
       (button "Undo"
               (λ _
                 (with-handlers ([(λ _ #t)
                                  (λ (e)
                                    (js-log (format "~a" e)))])
                   (js-log "undo")
                   (undo!)
                   (void))))
       (button "Redo"
               (λ _
                 (js-log "redo")
                 (redo!)
                 (void)))
       (spacer))

      (list-group @circles-indexed   
                  #f ; @selected-circle-index    ; selected-index
                  (λ (new-index)     ; on-selected
                    (:= @selected-circle-index new-index)
                    (void)))

      (button "New"
              (λ ()
                (add-circle! (make-random-circle))))
      

      ))))
  
(define app-renderer
  (render 7gui-circle-app))


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
