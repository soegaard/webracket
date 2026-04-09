;;;
;;; web-easy Browser Controls Smoke Example
;;;

;; Minimal browser app for choice/slider/progress smoke tests.

(include-lib web-easy)

(define @color (@ 'red))
(define @level (@ 10))

(define app-renderer
  (render
   (window
    (vpanel
     (choice '(red green blue)
             @color
             (lambda (new-value)
               (:= @color (string->symbol new-value))))
     (text (~> @color (lambda (s) (~a "color:" s))))
     (slider
               @level
               (lambda (new-value)
                 (:= @level new-value))
               #:min 0
               #:max 100)
     (text (~> @level (lambda (n) (~a "level:" n))))
     (progress
               @level
               #:min 0
               #:max 100)))))

(mount-renderer! app-renderer)
