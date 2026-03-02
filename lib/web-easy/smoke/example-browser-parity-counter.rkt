;;;
;;; web-easy Browser Parity Counter Example
;;;

;; Parity example: gui-easy quickstart 1.2-style counter.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)

(define @count (@ 0))

(define app-renderer
  (render
   (window
    (vpanel
     (text (~> @count number->string))
     (button "+" (lambda ()
                    (<~ @count add1)))))))

(mount-renderer! app-renderer)
