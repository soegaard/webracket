;;;
;;; web-easy Browser Operators Smoke Example
;;;

;; Minimal browser app for ~#> and λ<~ smoke tests.

(include-lib web-easy)

(define @count (@ 0))
(define inc!   (λ<~ @count add1))
(define @even  (~#> @count even?))

(define app-renderer
  (render
   (window
    (vpanel
     (text (~> @count (lambda (n)
                        (~a "count:" n))))
     (text (~> @even (lambda (n)
                       (~a "even:" n))))
     (button "inc" inc!)))))

(mount-renderer! app-renderer)
