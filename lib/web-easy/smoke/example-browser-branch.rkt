;;;
;;; web-easy Browser Branch Smoke Example
;;;

;; Minimal browser app for structural branch switching smoke tests.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)

(define @show-on (@ #t))

(define app-renderer
  (render
   (window
    (vpanel
     (button "toggle"
             (lambda ()
               (:= @show-on (not (obs-peek @show-on)))))
     (if-view @show-on
              (text "ON")
              (text "OFF"))))))

(mount-renderer! app-renderer)
