;;;
;;; Quickstart: Dynamic Counters (Light Theme)
;;;

(include/reader "../../main-browser.rkt" read-syntax/skip-first-line)

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
  (js-set-attribute! light-link "href"  "theme-light.css")
  (void))

(define theme-core-link-node  (install-theme-link! "we-theme-core-css"))
(define theme-light-link-node (install-theme-link! "we-theme-external-css"))
(apply-light-theme! theme-core-link-node theme-light-link-node)

;;;
;;; Mount the renderer
;;;

(mount-renderer! app-renderer)
