;;;
;;;7 GUI - Timer 
;;;

;; https://eugenkiss.github.io/7guis/tasks#timer

(include/reader "../../main-browser.rkt" read-syntax/skip-first-line)

;; ----------------------------------------
;; Timers
;; ----------------------------------------

(define interval-id #f)

; start-timer : callback number -> void
;   Calls `on-tick` every `delay` milliseconds.
(define (start-timer on-tick delay)
  ; delay is in milliseconds
  (when interval-id
    (js-window-clear-interval interval-id))
  (define callback (procedure->external on-tick))
  (set! interval-id (js-window-set-interval callback delay)))


;; ----------------------------------------
;; App state
;; ----------------------------------------

(define @elapsed  (@  0))  ; Time elapsed since last reset, capped at duration
(define @duration (@ 10))  ; Max duration for timer

(define (on-tick)
  (@elapsed . <~ . (λ (e)
                     (define next-e (+ e 0.1))
                     (define d      (obs-peek @duration))                     
                     (if (< next-e d) next-e d))))

(define (~seconds s) (~r/precision s 2))

(define 7gui-timer-app
  (window ; #:title "Timer"
   (container #:style "max-width: 400px;"
     (h1 "Timer")
     (Form
      (vpanel
       ;; Elapsed time and progress bar
       (Label "Elapsed time")
       (progress (@elapsed  . ~> . (λ (e) (exact-round (* e 10))))
                 #:max (@duration . ~> . (λ (d) (* d 10))) 
                 #:min 0)
       (text (@elapsed . ~> . ~seconds))

       (text " ")
       ;; Duration and slider
       (vpanel
        (Label "Duration")
        (slider @duration                ; value
                (λ (d) (:= @duration d)) ; on-change, was (λ:= @duration)
                1                        ; min
                100)                     ; max
        (text (@duration . ~> . ~seconds)))

       ;; Reset Button
       (hpanel ; prevents wide button
        (button #:type "button"
                ; In a form the default type is "submit",
                ; but on submit, the form is rebuilt.
                "Reset"
                (λ () (:= @elapsed 0)))))))))

  
(define app-renderer
  (render 7gui-timer-app))

;; Start ticking
(start-timer on-tick 100.) ; 100ms


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
