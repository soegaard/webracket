;;;
;;; web-easy Browser Parity Menu Keyboard Example
;;;

;; Parity smoke example for menu-item keyboard focus and activation semantics.

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
