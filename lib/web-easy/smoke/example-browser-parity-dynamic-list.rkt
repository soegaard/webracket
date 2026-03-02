;;;
;;; web-easy Browser Parity Dynamic List Example
;;;

;; Parity example: gui-easy quickstart 1.4-style dynamic counters/list behavior.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)

;; Constants for example-local observable state.
(define @items (@ '((a . 0) (b . 0))))

;; increment-item! : symbol? -> void?
;;   Increment the numeric value for the entry keyed by id.
(define (increment-item! id)
  (obs-update! @items
               (lambda (items)
                 (map (lambda (entry)
                        (if (eq? (car entry) id)
                            (cons id (add1 (cdr entry)))
                            entry))
                      items))))

;; reorder-items! : -> void?
;;   Swap the first two entries when at least two entries are present.
(define (reorder-items!)
  (obs-update! @items
               (lambda (items)
                 (cond
                   [(>= (length items) 2)
                    (define first  (car items))
                    (define second (cadr items))
                    (cons second (cons first (cddr items)))]
                   [else
                    items]))))

;; add-item-c! : -> void?
;;   Append key c with value 0 when it is not already present.
(define (add-item-c!)
  (obs-update! @items
               (lambda (items)
                 (if (assq 'c items)
                     items
                     (append items (list (cons 'c 0)))))))

;; drop-item-b! : -> void?
;;   Remove the entry keyed by b from the list.
(define (drop-item-b!)
  (obs-update! @items
               (lambda (items)
                 (filter (lambda (entry)
                           (not (eq? (car entry) 'b)))
                         items))))

(define app-renderer
  (render
   (window
    (vpanel
     (hpanel
      (button "inc-a" (lambda () (increment-item! 'a)))
      (button "reorder" reorder-items!)
      (button "add-c" add-item-c!)
      (button "drop-b" drop-item-b!))
     (list-view @items
                (lambda (_key entry)
                  (define id (car entry))
                  (define n  (cdr entry))
                  (text (string-append (symbol->string id)
                                       ":"
                                       (number->string n))))
                car)))))

(mount-renderer! app-renderer)
