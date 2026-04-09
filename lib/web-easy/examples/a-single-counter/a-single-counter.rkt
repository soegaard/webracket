;;;
;;; Quickstart: A Single Counter (Light Theme)
;;;

(include-lib web-easy)

(define @count (@ 0))

(define a-single-counter-app
  (window
   (container
    (h1 "A Single Counter")
    (hpanel
     (button "-" (λ () (@count . <~ . sub1)))
     (text (@count . ~> . number->string))
     (button "+" (λ () (@count . <~ . add1)))))))

(define a-single-counter-app-2
  (window
   (container
    (h1 "A Single Counter - 2")
    (hpanel
     (button "-" (λ () (obs-update! @count sub1)))
     (text (obs-map @count number->string))
     (button "+" (λ () (obs-update! @count add1)))))))

(define app-renderer
  (render a-single-counter-app))  ; <-- change app variant here


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
