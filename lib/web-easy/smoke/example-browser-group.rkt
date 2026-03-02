;;;
;;; web-easy Browser Group Legend Example
;;;

;; Smoke example focused on group/fieldset legend rendering.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)

(define @title (@ "Visual Check"))
(define @body  (@ "inside"))

(define app-renderer
  (render
   (window
    (vpanel
     (group @title
            (text @body))))))

(mount-renderer! app-renderer)
