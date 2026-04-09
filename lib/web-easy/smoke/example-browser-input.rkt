;;;
;;; web-easy Browser Input Smoke Example
;;;

;; Minimal browser app for two-way input binding smoke tests.

(include-lib web-easy)

(define @name (@ "alice"))

(define app-renderer
  (render
   (window
    (vpanel
     (input @name
            (lambda (new-value)
              (:= @name new-value)))
     (text @name)))))

(mount-renderer! app-renderer)
