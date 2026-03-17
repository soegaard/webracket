;;;
;;;7 GUI - Temperature Converter
;;;

;; https://eugenkiss.github.io/7guis/tasks#counter

(include/reader "../../main-browser.rkt" read-syntax/skip-first-line)

(define (F->C f) (* (- f 32) 5/9))
(define (C->F c) (+ (* c 9/5) 32))

(define (one-digit x) (/ (flround (* 10. x)) 10.))
(define (~one x)      (number->string (one-digit x)))
; (define (~one x)      (number->string x))

; TODO: Make this work. There is a problem with ~r/precision
;       when called from this file.
;       the test suite in stdlib/test/ passes.
#;(define (~one x)
  ; (js-log "~one")
  ; (js-log x)
  (with-handlers ([(lambda _ #t) (lambda _ "exn: ~one")])
    (cond
      [(number? x)
       ; (js-log "A")
       (define out (~r/precision x 1))
       ; (js-log "B")
       ; (js-log out)
       out]
      [else
       "bad"])))

(define @background (@ "lightblue"))
(define @tempC      (@ 26))
(define @tempF      (@tempC . ~> . C->F))

(define @input      (@ "26"))      ; text in last edited field 
(define @source     (@ "Celsius")) ; label for last edited field

(define (~background color)
  (if color color ""))

(define (temp label @value convert-to-C)
  (hpanel          
   (input (obs-combine (λ (value input source bgcolor)
                         (if (equal? source label)
                             input
                             (~one value)))
                       @value @input @source @background)
          (λ (new-text)
            (:= @input  new-text)
            (:= @source label)
            (define new-temp (string->number new-text))
            (cond
              [new-temp (:= @tempC (convert-to-C new-temp))
                        (:= @background #f)]
              [else     (js-log "red")
                        (:= @background "red")]))
          #:attrs `((style ,(format "background-color: ~a;"
                                    (~background (obs-peek @background))))))
   (text label)))

(define 7gui-temperature-converter-app
  (window
   (container
    (h1 "Temperature Converter")
    (hpanel
     (temp "Celsius"    @tempC values)
     (text " = ")
     (temp "Fahrenheit" @tempF F->C)))))

(define app-renderer
  (render 7gui-temperature-converter-app))



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
