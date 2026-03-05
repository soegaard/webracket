;;;
;;; web-easy Browser List Smoke Example
;;;

;; Minimal browser app for list-view reorder and reactive update smoke tests.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)

;; The representation in @items is chosen by the web-easy user.
;; list-view only requires a key function and an entry renderer.
(define @items    (@ '((1 "a") (2 "b") (3 "c"))))
(define id-of     first)
(define label-of  second)

;; render-entry : any/c list? -> view?
;;   Render one list entry.
(define (render-entry _key entry)
  (text (label-of entry)))

(define app-renderer
  (render
   (window
    (vpanel
     (button "reorder"
             (lambda ()
               (:= @items '((3 "c") (1 "a") (2 "b")))))
     (list-view @items render-entry id-of)))))

(mount-renderer! app-renderer)
