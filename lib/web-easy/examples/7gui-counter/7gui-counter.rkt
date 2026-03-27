;;;
;;;7 GUI - Counter 
;;;

;; https://eugenkiss.github.io/7guis/tasks#counter

(include/reader "../../main-browser.rkt" read-syntax/skip-first-line)

(define @count (@ 0))

(define 7gui-counter-app
  (window
   (container
    (h1 "Counter")
    (hpanel
     (input (@count . ~> . number->string)
            (λ _ (void)) ; on-change
            #:input-attrs '((readonly "readonly"))) 
     (button "count"
             (λ () (obs-update! @count add1)))))))

(define app-renderer
  (render 7gui-counter-app))



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
