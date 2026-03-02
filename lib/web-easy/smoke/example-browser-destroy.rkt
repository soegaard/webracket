;;;
;;; web-easy Browser Destroy Smoke Example
;;;

;; Minimal browser app for renderer-destroy lifecycle smoke tests.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)

(define @count (@ 0))

(define app-renderer #f)

(set! app-renderer
  (render
   (window
    (vpanel
     (text (~> @count number->string))
     (button "inc"
             (lambda ()
               (<~ @count add1)))
     (button "destroy"
             (lambda ()
               (renderer-destroy app-renderer)))))))

(mount-renderer! app-renderer)
