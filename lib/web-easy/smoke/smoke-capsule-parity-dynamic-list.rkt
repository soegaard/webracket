#lang webracket

;;;
;;; Smoke Capsule: Parity dynamic list
;;;

;; Isolated parity capsule for parity-dynamic-list.
;;
;; Exports:
;;   parity-dynamic-list-make-page      Build and mount the parity page under root.
;;   parity-dynamic-list-run-test       Execute capsule-local setup checks.
;;   parity-dynamic-list-cleanup        Destroy mounted renderer state for this capsule.

(define-values (parity-dynamic-list-make-page parity-dynamic-list-run-test parity-dynamic-list-cleanup)
  (let ()
    ;; Constants for parity-dynamic-list capsule state.
    (define parity-dynamic-list-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-dynamic-list-make-page : any/c -> void?
    ;;   Build and mount the parity page under root.
    (define (parity-dynamic-list-make-page root)
      ;;;
      ;;; web-easy Browser Parity Dynamic List Example
      ;;;
      
      ;; Parity example: gui-easy quickstart 1.4-style dynamic counters/list behavior.
      
      
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
                        (text (~a id ":" n)))
                      car)))))
      
      (set! parity-dynamic-list-renderer app-renderer)
      (mount-renderer! app-renderer root)
      (void))

    ;; parity-dynamic-list-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-dynamic-list-run-test _root)
      (and parity-dynamic-list-renderer #t))

    ;; parity-dynamic-list-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-dynamic-list-cleanup _root)
      (when parity-dynamic-list-renderer
        (renderer-destroy parity-dynamic-list-renderer)
        (set! parity-dynamic-list-renderer #f))
      (void))

    (values parity-dynamic-list-make-page parity-dynamic-list-run-test parity-dynamic-list-cleanup)))
