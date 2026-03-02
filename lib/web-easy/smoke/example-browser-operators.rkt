;;;
;;; web-easy Browser Operators Smoke Example
;;;

;; Minimal browser app for ~#> and λ<~ smoke tests.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)

(define @count (@ 0))
(define inc!   (λ<~ @count add1))
(define @even  (~#> @count even?))

(define app-renderer
  (render
   (window
    (vpanel
     (text (~> @count (lambda (n)
                        (string-append "count:" (number->string n)))))
     (text (~> @even (lambda (n)
                       (string-append "even:" (number->string n)))))
     (button "inc" inc!)))))

(mount-renderer! app-renderer)
