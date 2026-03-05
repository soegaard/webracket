#lang webracket

;;;
;;; Smoke Capsule: Parity list
;;;

;; Isolated parity capsule for parity-list.
;;
;; Exports:
;;   parity-list-make-page      Build and mount the parity page under root.
;;   parity-list-run-test       Execute capsule-local setup checks.
;;   parity-list-cleanup        Destroy mounted renderer state for this capsule.

(define-values (parity-list-make-page parity-list-run-test parity-list-cleanup)
  (let ()
    ;; Constants for parity-list capsule state.
    (define parity-list-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-list-make-page : any/c -> void?
    ;;   Build and mount the parity page under root.
    (define (parity-list-make-page root)
      ;;;
      ;;; web-easy Browser Parity List Example
      ;;;

      ;; Parity example: list ordering and keyed updates.

      ;; Constants for example-local observable state.
      ;; The representation in @entries is chosen by the web-easy user.
      ;; list-view only requires a key function and an entry renderer.
      (define @entries  (@ '((a "alpha") (b "beta"))))
      (define id-of     first)
      (define label-of  second)

      ;; reverse-list! : -> void?
      ;;   Reverse the current entry order.
      (define (reverse-list!)
        (obs-update! @entries reverse))

      ;; add-gamma! : -> void?
      ;;   Append key g with label gamma when missing.
      (define (add-gamma!)
        (obs-update! @entries
                     (lambda (entries)
                       (if (memf (lambda (entry)
                                   (eq? (id-of entry) 'g))
                                 entries)
                           entries
                           (append entries (list (list 'g "gamma")))))))

      ;; remove-beta! : -> void?
      ;;   Remove the entry keyed by b.
      (define (remove-beta!)
        (obs-update! @entries
                     (lambda (entries)
                       (filter (lambda (entry)
                                 (not (eq? (id-of entry) 'b)))
                               entries))))

      ;; render-entry : any/c list? -> view?
      ;;   Render one parity list entry.
      (define (render-entry _key entry)
        (text (label-of entry)))

      (define app-renderer
        (render
         (window
          (vpanel
           (hpanel
            (button "reverse"     reverse-list!)
            (button "add-gamma"   add-gamma!)
            (button "remove-beta" remove-beta!))
           (list-view @entries render-entry id-of)))))

      (set! parity-list-renderer app-renderer)
      (mount-renderer! app-renderer root)
      (void))

    ;; parity-list-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-list-run-test _root)
      (and parity-list-renderer #t))

    ;; parity-list-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-list-cleanup _root)
      (when parity-list-renderer
        (renderer-destroy parity-list-renderer)
        (set! parity-list-renderer #f))
      (void))

    (values parity-list-make-page parity-list-run-test parity-list-cleanup)))
