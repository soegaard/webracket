;;;
;;; Quickstart: Dynamic Counters (Light Theme)
;;;

(include-lib web-easy)

(define @counters (@ '((0 . 0))))
 
(define (append-counter counts)
  (define next-id (add1 (apply max -1 (map car counts))))
  (append counts `((,next-id . 0))))
 
(define (update-count counts k proc)
  (for/list ([entry (in-list counts)])
    (if (equal? (car entry) k)
        (cons k (proc (cdr entry)))
        entry)))
 
(define (counter count action)
  (hpanel
   ; #:stretch '(#t #f)
   (button "-" (λ () (action sub1)))
   (text (number->string count))
   (button "+" (λ () (action add1)))))


(define dynamic-counters-app
  (window ; #:size '(#f 200)
   (container
    (h1 "Dynamic Counters")

    (vpanel
     (hpanel
      ; #:alignment '(center top)
      ; #:stretch '(#t #f)
      (button "Add counter" (λ () (@counters . <~ . append-counter))))
     
     (list-view @counters
                (λ (k entry)
                  (counter (cdr entry)
                           (λ (proc)
                             (@counters . <~ .
                                        (λ (counts)
                                          (update-count counts k proc))))))
                car))))) ; key-of



(define app-renderer
  (render dynamic-counters-app))


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
