;;;
;;; web-easy Browser Smoke Example
;;;

;; Minimal browser smoke app that renders a counter and mounts it to the document body.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)

(define @count (@ 0))

(define app-renderer
  (render
   (window
    (vpanel
     (text (~> @count number->string))
     (button "inc"
             (lambda ()
               (<~ @count add1)))))))

(mount-renderer! app-renderer)
