;;;
;;; web-easy Browser Menu Keyboard Example
;;;

;; Smoke example focused on keyboard focus/activation for menu-item widgets.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)

(define @count (@ 0))

(define (inc!)
  (<~ @count add1))

(define app-renderer
  (render
   (window
    (vpanel
     (text (~> @count (lambda (n) (string-append "count:" (number->string n)))))
     (menu-bar
      (menu "Actions"
            (menu-item "inc" inc!)))))))

(mount-renderer! app-renderer)
