;;;
;;; web-easy Browser Parity List Example
;;;

;; Parity example: list ordering and keyed updates.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)

;; Constants for example-local observable state.
(define @entries (@ '((a . "alpha") (b . "beta"))))

;; reverse-list! : -> void?
;;   Reverse the current entry order.
(define (reverse-list!)
  (obs-update! @entries reverse))

;; add-gamma! : -> void?
;;   Append key g with label gamma when missing.
(define (add-gamma!)
  (obs-update! @entries
               (lambda (entries)
                 (if (assq 'g entries)
                     entries
                     (append entries (list (cons 'g "gamma")))))))

;; remove-beta! : -> void?
;;   Remove the entry keyed by b.
(define (remove-beta!)
  (obs-update! @entries
               (lambda (entries)
                 (filter (lambda (entry)
                           (not (eq? (car entry) 'b)))
                         entries))))

(define app-renderer
  (render
   (window
    (vpanel
     (hpanel
      (button "reverse" reverse-list!)
      (button "add-gamma" add-gamma!)
      (button "remove-beta" remove-beta!))
     (list-view @entries
                (lambda (_key entry)
                  (text (cdr entry)))
                car)))))

(mount-renderer! app-renderer)
