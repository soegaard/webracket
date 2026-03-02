;;;
;;; web-easy Browser Controls Smoke Example
;;;

;; Minimal browser app for choice/slider/progress smoke tests.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)

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
     (text (~> @color (lambda (s) (string-append "color:" (symbol->string s)))))
     (slider @level
             (lambda (new-value)
               (:= @level new-value))
             0
             100)
     (text (~> @level (lambda (n) (string-append "level:" (number->string n)))))
     (progress @level 0 100)))))

(mount-renderer! app-renderer)
