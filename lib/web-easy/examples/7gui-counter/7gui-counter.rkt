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
