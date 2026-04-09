;;;
;;; Quickstart: Multiple Counters (Light Theme)
;;;

(include-lib web-easy)

(define (counter @count action)
  (hpanel
   (button "-" (λ () (action sub1)))
   (text (@count . ~> . number->string))
   (button "+" (λ () (action add1)))))
 
(define @counter-1 (@ 0))
(define @counter-2 (@ 0))

(define multiple-counters-app
  (window
   (container
    (h1 "Two Counters")
    (vpanel
     (counter @counter-1 (λ (proc) (@counter-1 . <~ . proc)))
     (counter @counter-2 (λ (proc) (@counter-2 . <~ . proc)))))))



(define app-renderer
  (render multiple-counters-app))


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
