;;;
;;; 7 GUI - CRUD
;;;

;; https://eugenkiss.github.io/7guis/tasks#crud

(include/reader "../../main-browser.rkt" read-syntax/skip-first-line)

;; ----------------------------------------
;; List Utilites                           
;; ----------------------------------------

(define (replace-first xs x y [== equal?])
  (let loop ([rs '()] [xs xs])
    (cond
      [(null? xs) (reverse rs)]
      [else       (define x0 (car xs))
                  (cond
                    [(== x x0)
                     (append (reverse (cons y rs)) (cdr xs))]
                    [else
                     (loop (cons x0 rs) (cdr xs))])])))

(define (indices xs)  (range (length xs)))
(define (zip xs ys)   (map list xs ys))

(define (safe-list-ref xs i)
  (define n (length xs))
  (if (and (integer? i) (<= 0 i) (< i n))
      (list-ref xs i)
      #f))

;; ----------------------------------------
;; Data Representation and Formatting      
;; ----------------------------------------

(struct person (name surname) #:transparent)

(define (~person p)
  (format "~a, ~a" (person-surname p) (person-name p)))



;; ----------------------------------------
;; Model                                   
;; ----------------------------------------

(define @people
  (@ (list (person "Emil"  "Hans")
           (person "Max"   "Mustermann")
           (person "Roman" "Tisch"))))

(define (update p name surname) 
  (define updated-p (person name surname))
  (@people . <~ . (λ (people)
                    (replace-first people p updated-p eq?)))
  updated-p)

(define (create name surname)
  (define new-p (person name surname))
  (@people . <~ . (λ (people)
                    (append people (list new-p))))
  new-p)

(define (delete! p)
  (@people . <~ . (λ (people)
                    (remq p people)))
  (void))


;; ----------------------------------------
;; App state
;; ----------------------------------------

;; Directly Present in the UI

(define @prefix                  ; The search prefix used to filter the 
  (@ ""))                        ; shown person list.

(define @selected-person  (@ #f)) ; chosen person in the list

(define @current-name    (@ "")) ; input fields for new person / update person
(define @current-surname (@ "")) ;

;; Derived from the Model

(define @filtered-people
  (obs-combine
   (λ (people prefix)
     (for/list ([p (in-list people)]
                #:when (string-prefix? (string-downcase (person-surname p))
                                       (string-downcase prefix)))
       p))
   @people @prefix))


; A user can select a person and then enter a prefix,
; that doesn't match the selected person.
; We want to disable the Update and Delete buttons when
; the selected is not visible.

(define @selected-visible?
  (obs-combine
   (λ (filtered selected)
     (and selected
          (memq selected filtered)))
   @filtered-people
   @selected-person))


;; Utils

(define (set-name-fields name surname)
  (:= @current-name    name)
  (:= @current-surname surname))

(define (clear-name-fields)
  (:= @current-name    "")
  (:= @current-surname ""))


;; Deselect, if the selected person becomes invisible.
;; At the same time, clear the names fields.
(obs-observe!
 (obs-combine list @filtered-people @selected-person)
 (λ (state)
   (define filtered (first state))
   (define selected (second state))
   (when (and selected (not (memq selected filtered)))
     (:= @selected-person #f)
     (clear-name-fields))))

;; ----------------------------------------
;; Views                                   
;; ----------------------------------------


(define (rich-better-list-group @datas @selected-data render-data on-selected)
  ;; render-data : data -> (or/c string? view?)
  (define (render-datas ds)
    (map render-data ds))

  (define @indices
    (@datas . ~> . indices))
  (define @rendered-datas
    (@datas . ~> . render-datas))
  (define @data-list
    (obs-combine zip @indices @rendered-datas))

  (define @selected-index
    (obs-combine
     (λ (datas selected)
       (and selected
            (index-of datas selected eq?)))
     @datas @selected-data))

  (rich-list-group
   @data-list
   @selected-index
   (λ (new-index)
     (js-log "rich-better-list-group")
     (define data (safe-list-ref (obs-peek @datas) new-index))
     (on-selected data)
     (void))))


(define (field label v)
  (hpanel 
   (text label)
   (spacer)
   v))


(define (better-list-group @datas @selected-data ~data on-selected)
  ; ~data : data -> string  ; formats `data` as a string
  (define (~datas ds)   (map ~data ds))

  (define @indices            (@datas . ~> . indices))
  (define @formatted-datas    (@datas . ~> . ~datas))
  (define @data-list          (obs-combine zip @indices @formatted-datas))

  (define @selected-index     (obs-combine
                               (λ (datas selected)
                                 (and selected
                                      (index-of datas selected eq?)))
                               @datas @selected-data))

  (list-group @data-list
              @selected-index
              (λ (new-index) ; on-selected
                (define data (safe-list-ref (obs-peek @datas) new-index))
                (on-selected data)
                (void))))


(define (no-matches-list-group)
  (define italic/muted "font-style: italic; color: #888;")
  (rich-better-list-group
   (@ (list 'no-matches)) ; datas
   (@ #f)                 ; selected data (none)
   (λ (_)                 ; format the single data as "No Matches"
     (js-log "here!")
     (Div #:style italic/muted
       (text "No matches")))
   (λ (_) (void))))       ; do nothing

(define (people-list-group)
  (rich-better-list-group
   @filtered-people     ; datas
   @selected-person     ; the selected person
   ~person              ; formatter
   on-person-selected)) ; called with a person

(define (on-person-selected person)
  ;; A person was (de)selected in the filtered people list-group
  (define selected  (obs-peek @selected-person))
  (define deselect? (or (not person) (eq? selected person)))
  (cond
    [deselect?
     (:= @selected-person #f)
     (clear-name-fields)]
    [else
     ; Selected person
     (:= @selected-person person)
     ; Populate edit fields
     (set-name-fields (person-name person) (person-surname person))])
  (void))


(define 7gui-crud-app
  (window ; #:title "CRUD"
   (container #:style "max-width: 500px;"
     (h1 "CRUD")
     (vpanel
     ;; Filter Prefix
     (hpanel ; #:stretch '(#t #f)
      (text "Filter prefix:")
      (input @prefix
             (λ (new-text)
               (:= @prefix new-text))))

     ;; People List and name fields
     (hpanel
      (spacer)
      ;; Left Column: list of people      
      (vpanel
       ;; List of persons matching the prefix       
       (observable-view
        (@filtered-people . ~> . null?)
        (λ (empty?)
          (if empty?
              (no-matches-list-group)
              (people-list-group)))))

      (spacer)
      ;; right Column: Name fields
      (vpanel
       (field "Name:"
              (input @current-name
                     (λ (text) (:= @current-name text))))
       (spacer)
       (field "Surname:"
              (input @current-surname
                     (λ (text) (:= @current-surname text)))))
      (spacer))
     
     ;; Create, Update and Delete Buttons
     (hpanel
      (button "Create"
        (λ ()
          (define name    (obs-peek @current-name))
          (define surname (obs-peek @current-surname))
          (unless (or (string=? name    "")
                      (string=? surname ""))
            (create name surname)
            (:= @selected-person #f)
            ; Clear fields to avoid accidental duplicates
            (clear-name-fields))
          (void)))
       
      (button  "Update" 
               #:disabled (@selected-visible? . ~> . not)
        (λ ()
          (define name    (obs-peek @current-name))
          (define surname (obs-peek @current-surname))
          (define p       (obs-peek @selected-person))
          (unless (or (string=? name    "")
                      (string=? surname ""))            
            (:= @selected-person (update p name surname)))
          (void)))
       
      (button "Delete" 
        #:disabled (@selected-visible? . ~> . not)
        (λ ()
          (define p (obs-peek @selected-person))
          (delete! p)
          (:= @selected-person #f)
          (clear-name-fields)))))
          
       )))
  
(define app-renderer
  (render 7gui-crud-app))


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
