;;;
;;; web-easy Browser Checkbox Smoke Example
;;;

;; Minimal browser app for checkbox change and boolean propagation smoke tests.

(include-lib web-easy)

(define @enabled (@ #f))

(define app-renderer
  (render
   (window
    (vpanel
     (checkbox @enabled
               (lambda (new-value)
                 (:= @enabled new-value)))
     (text (~> @enabled (lambda (v) (if v "on" "off"))))))))

(mount-renderer! app-renderer)
