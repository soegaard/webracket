;;;
;;; web-easy Browser Parity Todo Example
;;;

;; Parity example: simple todo list with add/toggle/clear-done.
;; Extended with remaining count, mark-all-done, and inline edit/save/cancel.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)

(struct todo (id done? text) #:transparent)

;; Constants for example-local observable state.
(define @draft        (@ "task-1"))
(define @next-id      (@ 2))
(define @todos        (@ (list (todo 1 #f "task-0"))))
(define @editing-id   (@ #f))
(define @editing-text (@ ""))

;; set-todo-done : todo? boolean? -> todo?
;;   Return a copy of entry with the done flag replaced.
(define (set-todo-done entry done?)
  (todo (todo-id entry) done? (todo-text entry)))

;; set-todo-text : todo? string? -> todo?
;;   Return a copy of entry with the label text replaced.
(define (set-todo-text entry text)
  (todo (todo-id entry) (todo-done? entry) text))

;; toggle-todo! : number? boolean? -> void?
;;   Set done state for the todo entry matching id.
(define (toggle-todo! id done?)
  (obs-update! @todos
               (lambda (todos)
                 (map (lambda (entry)
                        (if (= (todo-id entry) id)
                            (set-todo-done entry done?)
                            entry))
                      todos))))

;; mark-all-done! : -> void?
;;   Mark every todo as done.
(define (mark-all-done!)
  (obs-update! @todos
               (lambda (todos)
                 (map (lambda (entry)
                        (set-todo-done entry #t))
                      todos))))

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
    (obs-set! @draft "")))

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

(define app-renderer
  (render
   (window
    (vpanel
     (hpanel
      (input @draft (lambda (new-value)
                      (:= @draft new-value))
             add-todo!)
      (button "add-todo" add-todo!)
      (button "mark-all-done" mark-all-done!)
      (button "clear-done" clear-done!))
     (text (~> @todos
               (lambda (todos)
                 (string-append "remaining:"
                                (number->string
                                 (length
                                  (filter (lambda (entry)
                                            (not (todo-done? entry)))
                                          todos)))))))
     (list-view @todos
                (lambda (_key entry)
                  (define id    (todo-id entry))
                  (define done? (todo-done? entry))
                  (define label (todo-text entry))
                  (hpanel
                   (text (string-append (number->string id) ":"))
                   (checkbox done? (lambda (new-value)
                                     (toggle-todo! id new-value)))
                   (if-view (~> @editing-id (lambda (editing-id)
                                              (and editing-id (= editing-id id))))
                            (hpanel
                             (input @editing-text (lambda (new-value)
                                                    (:= @editing-text new-value)))
                             (button "save" (lambda ()
                                              (save-edit! id)))
                             (button "cancel" cancel-edit!))
                            (hpanel
                             (text label)
                             (button "edit" (lambda ()
                                              (start-edit! entry)))))))
                todo-id)))))

(mount-renderer! app-renderer)
