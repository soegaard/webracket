;;;
;;; web-easy Browser Input Smoke Example
;;;

;; Minimal browser app for two-way input binding smoke tests.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)

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
