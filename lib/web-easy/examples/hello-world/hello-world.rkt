;;;
;;; Hello World (Light Theme)
;;;

(include/reader "../../main-browser.rkt" read-syntax/skip-first-line)

; Full screen
(define hello-world-app-1
  (window
   (h1 "Hello World")
   (text "Have a nice day.")))


; Body width constrained to 1200px
(define hello-world-app-2
  (window
   (container
    (h1 "Hello World")
    (text "Have a nice day."))))

; Inline style to change the color.
(define hello-world-app-3
  (window
   (container
    (h1 "Hello World")
    (text "Have a nice day."
          #:attrs '((style "color: red;"))))))

; Body width constrained to 600px
(define my-container-style
  "width: min(600px, calc(100vw - 28px));")

(define hello-world-app-4
  (window
   (container 
     (h1 "Hello World")
     (text "Have a nice day.")
     #:attrs `((style ,my-container-style)))))


(define app-renderer
  (render hello-world-app-1)) ; <-- change app variant here


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
