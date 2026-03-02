;;;
;;; web-easy Browser List Smoke Example
;;;

;; Minimal browser app for list-view reorder and reactive update smoke tests.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)

(define @items (@ '((1 . "a") (2 . "b") (3 . "c"))))

(define app-renderer
  (render
   (window
    (vpanel
     (button "reorder"
             (lambda ()
               (:= @items '((3 . "c") (1 . "a") (2 . "b")))))
     (list-view @items
                (lambda (_key entry)
                  (text (cdr entry)))
                car)))))

(mount-renderer! app-renderer)
