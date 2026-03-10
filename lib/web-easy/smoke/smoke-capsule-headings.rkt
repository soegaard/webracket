#lang webracket

;;;
;;; Smoke Capsule: Headings
;;;

;; Isolated smoke capsule for heading, display-heading, and lead rendering in smoke-all.
;;
;; Exports:
;;   headings-make-page   Build and mount the headings page under root.
;;   headings-run-test    Execute capsule-local setup checks.
;;   headings-cleanup     Destroy mounted renderer state for this capsule.

(define-values (headings-make-page headings-run-test headings-cleanup)
  (let ()
    ;; Constants for headings capsule state.
    (define headings-renderer #f) ; Mounted renderer for this capsule.

    ;; headings-make-page : any/c -> void?
    ;;   Build and mount the headings page under root.
    (define (headings-make-page root)
      (define @level   (@ 2))
      (define @title   (@ "Smoke Heading"))
      (define @display (@ "Display Heading"))
      (define @lead    (@ "Lead text"))
      (define @subtitle (@ "Updated today"))
      (define @align    (@ 'left))
      (define @spacing  (@ 'normal))
      (set! headings-renderer
            (render
             (window
              (vpanel
               (hpanel
                (button "set-level-1" (lambda () (:= @level 1)))
                (button "set-level-3" (lambda () (:= @level 3)))
                (button "set-title"   (lambda () (:= @title "Updated Heading")))
                (button "center"      (lambda () (:= @align 'center)))
                (button "right"       (lambda () (:= @align 'right)))
                (button "space-loose" (lambda () (:= @spacing 'loose))))
               (h1 "Section")
               (heading @level @title @align @spacing)
               (heading-with-subtitle @level @title @subtitle @align @spacing)
               (display-heading @level @display @align @spacing)
               (display-heading-with-subtitle @level @display @subtitle @align @spacing)
               (display-2 "Display Wrapper")
               (lead @lead)))))
      (mount-renderer! headings-renderer root)
      (void))

    ;; headings-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (headings-run-test _root)
      (and headings-renderer #t))

    ;; headings-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (headings-cleanup _root)
      (when headings-renderer
        (renderer-destroy headings-renderer)
        (set! headings-renderer #f))
      (void))

    (values headings-make-page headings-run-test headings-cleanup)))
