;;;
;;; web-easy Browser Smoke Example
;;;

;; Minimal browser smoke app that renders a counter and mounts it to the document body.

(include/reader "smoke-format.rkt" read-syntax/skip-first-line)
(include-lib web-easy)

(define @count (@ 0))

(define app-renderer
  (render
   (window
    (vpanel
     (text (~> @count (lambda (n) (~a n))))
     (button "inc"
             (lambda ()
               (<~ @count add1)))))))

(mount-renderer! app-renderer)
