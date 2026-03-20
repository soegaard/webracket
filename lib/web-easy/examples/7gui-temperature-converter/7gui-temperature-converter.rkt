;;;
;;; 7 GUI - Temperature Converter
;;;

;; https://eugenkiss.github.io/7guis/tasks#counter

(include/reader "../../main-browser.rkt" read-syntax/skip-first-line)

;;;
;;; Model
;;;

(define (F->C f) (* (- f 32) 5/9))
(define (C->F c) (+ (* c 9/5) 32))

(define @tempC      (@ 26))
(define @tempF      (@tempC . ~> . C->F))

;;;
;;; Formatters
;;;

(define (~background color) (or color ""))
(define (~a x)              (format "~a" x))
(define (~one x) (~r/precision x 1))

;;;
;;; View and Control
;;;

;; We will never change the field value,
;; so we track where the user is actively editing.
(define @source     (@ "Celsius"))                 ; label for last edited field

;; temp : string obs (number -> number) -> view
;;   Makes an temperature input component.
;;   Concretely, it makes an <input> view.
;;   The model temperature is @temp and convert-to-C
;;   is used to convert the entered value into Celsius.
(define (temp label @temp convert-to-C)
  ;; State for the <input> element
  (define @input       (@ (~a (obs-peek @temp))))  ; text in last edited field, initial value from @temp
  (define @background  (@ #f))                     ; current background color, #f means none
  ;; Derived state
  (define @input-style                             ; the style (red when the input value is invalid)
    (@background . ~> .
                 (lambda (bg)
                   (format "background-color: ~a;" (~background bg)))))
  (define @input-value                       ; the current input text
    (obs-combine (λ (temp input source)
                   (cond
                     [(equal? source label)  ; if the user is editing,
                      input]                 ;   keep the current value
                     [else                   ; else,
                      (:= @background #f)    ;   clear the background,
                      (~one temp)]))         ;   and use the model value.
                 @temp @input @source))
  ;; The view
  (hpanel          
   (input @input-value
          (λ (new-text)
            (:= @input  new-text)
            (:= @source label)
            (js-log (format "new text: ~a" new-text))
            (define new-temp (string->number new-text))
            (js-log (format "new temp: ~a" new-temp))
            (cond
              [new-temp (:= @tempC (convert-to-C new-temp))
                        (:= @background #f)]
              [else     (js-log "red")
                        (:= @background "red")]))
          #:style @input-style)
   (text label)))

;;;
;;; App
;;;

;; The app consists of two temperature input fields.
;; One for Celsius, one for Fahrenheit.

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
