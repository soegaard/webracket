;;;
;;; Add Two Numbers (Light Theme)
;;;

(include/reader "../../main-browser.rkt" read-syntax/skip-first-line)

;; Minimal web-easy browser example:
;;   enter two numbers and show their sum using the Light theme.

;;;
;;; Model
;;;

; The model consists of two numbers a and b and their sum s.

(define @a (@ 10))
(define @b (@ 20))
(define @s (obs-combine + @a @b))


;;;
;;; View
;;;

;; Constants for initial input state.
(define @a-text (@ (number->string (obs-peek @a)))) ; input text for a 
(define @b-text (@ (number->string (obs-peek @b)))) ; input text for b 

;; The result is always computed from the model 
(define @s-text (obs-map @s number->string))        ; Result/status line.

(define app-renderer
  (render
   (window
    (container ; used to limit the width
     (stack ; #:attrs '((style "width: 100%; max-width: 34rem;"))
            (heading 2 "Add Two Numbers")
            (text "Type two numbers to see their sum.")
            
            (group "Inputs"
                   (stack              
                    (text "First number")
                    (input @a-text
                           (lambda (v)
                             (:= @a-text v)
                             (update-model!))
                           #:attrs '((placeholder "e.g. 12.5")))
                    
                    (text "Second number")
                    (input @b-text
                           (lambda (v)
                             (:= @b-text v)
                             (update-model!))
                           #:attrs '((placeholder "e.g. 3.5")))))
            
            (group "Result"
                   (inline (text "The sum is: ")
                           (text @s-text))))))))

;;;
;;; Control
;;; 


;; parse-number : string? -> (or/c number? #f)
;;   Parse s as a number, or #f when parsing fails.
(define (parse-number s)
  (string->number s))

;; update-model! : -> void?
;;   If the text fields contain numbers, we update
;;   the numbers a and b in the model.
(define (update-model!)
  (define a (parse-number (obs-peek @a-text)))
  (when a (:= @a a))
    
  (define b (parse-number (obs-peek @b-text)))
  (when b (:= @b b)))



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
