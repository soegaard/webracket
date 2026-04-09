(include-lib web-easy)

(define (~a . xs)
  (string-append* (map (λ (x) (format "~a" x))
                       xs)))

;;;
;;; TODO Lists
;;;

;; Simple todo lists 

;; A single todo item is represented as a structure.

;;;
;;; Data Representation
;;;

(struct todo (id done? text) #:transparent)

; id    is a number
; done? is a boolean
; text  is a string

;; Functional Updaters

;; set-todo-done : todo? boolean? -> todo?
;;   Return a copy of entry with the done flag replaced.
(define (set-todo-done entry done?)
  (todo (todo-id entry) done? (todo-text entry)))

;; set-todo-text : todo? string? -> todo?
;;   Return a copy of entry with the label text replaced.
(define (set-todo-text entry text)
  (todo (todo-id entry) (todo-done? entry) text))

;; Predicates

(define (todo-not-done? entry)
  (not (todo-done? entry)))

;; Sample tasks

(define tasks
  '("laundry"          "get groceries"         "wash dishes"       "vacuum apartment" 
    "take out trash"   "water plants"          "pay bills"         "check email" 
    "read a book"      "exercise"              "go for a walk"     "call a friend"    
    "cook dinner"      "make breakfast"        "organize desk"     "backup files"
    "update software"  "review notes"          "study programming" "practice guitar"
    "learn a new word" "tidy bedroom"          "sort laundry"      "fold clothes"
    "iron shirt"       "clean kitchen counter" "wipe table"        "empty dishwasher"
    "load dishwasher"  "clean fridge"          "buy vegetables"    "buy fruit"
    "prepare lunch"    "stretch"               "meditate"          "check calendar"    
    "review goals"     "practice typing"       "delete old files"  "clean keyboard"
    "charge devices"   "refill water bottle"   "take vitamins"     "review budget"
    "plan weekend"     "sleep early"           "clean bathroom"    "write journal"
    "plan meals"       "write to-do list"))

(define (random-task)
  (list-ref tasks (random (length tasks))))


;;;
;;; Model
;;;

;; Constants for example-local observable state.
(define @draft        (@ (random-task)))
(define @next-id      (@ 6))
(define @todos        (@ (list (todo 1 #f (random-task))
                               (todo 2 #f (random-task))
                               (todo 3 #f (random-task))
                               (todo 4 #f (random-task))
                               (todo 5 #f (random-task)))))
(define @editing-id   (@ #f))
(define @editing-text (@ ""))


;; toggle-todo! : number? boolean? -> void?
;;   Set done state for the todo entry matching id.
(define (toggle-todo! id done?)
  (obs-update! @todos
               (lambda (todos)
                 (map (lambda (entry)
                        (if (= (todo-id entry) id)
                            (set-todo-done entry done?)
                            entry))
                      todos)))
  (void))

;; mark-all-done! : -> void?
;;   Mark every todo as done.
(define (mark-all-done!)
  (obs-update! @todos
               (lambda (todos)
                 (map (lambda (entry)
                        (set-todo-done entry #t))
                      todos)))
  (void))

;; add-todo! : -> void?
;;   Append a new unchecked todo from draft text when non-empty.
(define (add-todo!)
  (define label (obs-peek @draft))
  (unless (string=? label "")
    (define id (obs-peek @next-id))
    (obs-update! @todos
                 (lambda (todos)
                   (append todos (list (todo id #f label)))))
    (obs-update! @next-id add1)
    (obs-set! @draft (random-task))))

;; start-edit! : todo? -> void?
;;   Enter edit mode for entry and initialize edit draft.
(define (start-edit! entry)
  (obs-set! @editing-id (todo-id entry))
  (obs-set! @editing-text (todo-text entry)))

;; cancel-edit! : -> void?
;;   Leave edit mode without applying changes.
(define (cancel-edit!)
  (obs-set! @editing-id #f)
  (obs-set! @editing-text ""))

;; save-edit! : number? -> void?
;;   Save current edit draft to the todo matching id and leave edit mode.
(define (save-edit! id)
  (define new-text (obs-peek @editing-text))
  (obs-update! @todos
               (lambda (todos)
                 (map (lambda (entry)
                        (if (= (todo-id entry) id)
                            (set-todo-text entry new-text)
                            entry))
                      todos)))
  (cancel-edit!))

;; clear-done! : -> void?
;;   Remove todos that are currently marked done.
(define (clear-done!)
  (obs-update! @todos
               (lambda (todos)
                 (filter (lambda (entry)
                           (not (todo-done? entry)))
                         todos)))
  (cancel-edit!))

;;;
;;; VIEW
;;; 

(define app-renderer
  (render
   (window ; the full browser window
    (container ; restricts the width
     (vpanel
      (h1 "The TODO List")
      (text "Enter an item. ")
      (text "Add the item to the list using either the Add button, or press <enter>.")

      ;; TODO Experiment: how to add an on-click handler to a text element.
      #;(text "Click me!"
            #:attrs `((onclick
                       ,(procedure->external
                         (lambda (_evt)
                           (with-handlers ([(lambda (_e) #t) (lambda (_e) (void))])
                             (js-log "hello")))))))

      
      (h2 "The List")
      (list-view @todos
                 (lambda (_key entry)
                   (define id    (todo-id entry))
                   (define done? (todo-done? entry))
                   (define label (todo-text entry))
                   (hpanel
                    (checkbox done? (lambda (new-value) (toggle-todo! id new-value)))
                    (if-view (~> @editing-id (lambda (editing-id) (and editing-id (= editing-id id))))
                             (hpanel
                              (input @editing-text (lambda (new-value) (:= @editing-text new-value)))
                              (button "save"   (lambda () (save-edit! id)))
                              (button "cancel" cancel-edit!))
                             (hpanel
                              (text label)
                              (button "edit"   (lambda () (start-edit! entry)))))))
                 todo-id)
      (spacer)
      (hpanel (button "All done!"          mark-all-done!)
              (button "Remove done items"  clear-done!))

      (h2 "New Item")
      (grid '(50 50)
            (vpanel
             (input @draft
                    (lambda (new-value) (:= @draft new-value))
                    add-todo!)
             (hpanel (button "Add"      add-todo!)))
            (spacer))

      (h2 "Summary")

      (text (~> @todos
                (lambda (todos)
                  (~a "Total:" (length todos)))))

      (text (~> @todos
                (lambda (todos)
                  (~a "Done:" (length (filter todo-done? todos))))))

      (text (~> @todos
                (lambda (todos)
                  (~a "To Do:" (length (filter todo-not-done? todos))))))
      
      )))))


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
