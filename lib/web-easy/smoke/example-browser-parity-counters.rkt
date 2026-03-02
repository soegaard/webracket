;;;
;;; web-easy Browser Parity Counters Example
;;;

;; Parity example: gui-easy quickstart 1.3-style independent counters.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)

;; Constants for example-local observable state.
(define @counter-1 (@ 0))
(define @counter-2 (@ 0))

;; counter-view : string? observable? -> view?
;;   Build one labeled counter row with increment/decrement buttons.
(define (counter-view label @count)
  (hpanel
   (text (~> @count
             (lambda (n)
               (string-append label ":" (number->string n)))))
   (button "+" (lambda ()
                 (<~ @count add1)))
   (button "-" (lambda ()
                 (<~ @count sub1)))))

(define app-renderer
  (render
   (window
    (vpanel
     (counter-view "c1" @counter-1)
     (counter-view "c2" @counter-2)))))

(mount-renderer! app-renderer)
