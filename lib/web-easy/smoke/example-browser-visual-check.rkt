;;;
;;; web-easy Browser Visual Check Example
;;;

;; Manual visual sanity page for styled widgets in one place.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)

;; Constants for choice values.
(define mode/basic   "basic") ; Basic mode value.
(define mode/advanced "advanced") ; Advanced mode value.

;; Constants for table rows.
(define base-rows (list (list "alpha" "ok")
                        (list "beta" "hold")
                        (list "gamma" "new")))

;; Constants for local state.
(define @name    (@ "Alice"))
(define @enabled (@ #t))
(define @mode    (@ mode/basic))
(define @level   (@ 35))
(define @rows    (@ base-rows))
(define @tab     (@ 'overview))
(define @status  (@ "ready"))

;; bump-level! : -> void?
;;   Increase level up to max 100.
(define (bump-level!)
  (:= @level (min 100 (+ (obs-peek @level) 5)))
  (:= @status "bumped"))

;; reset-all! : -> void?
;;   Restore initial values for visual check state.
(define (reset-all!)
  (:= @name "Alice")
  (:= @enabled #t)
  (:= @mode mode/basic)
  (:= @level 35)
  (:= @rows base-rows)
  (:= @tab 'overview)
  (:= @status "reset"))

;; reverse-rows! : -> void?
;;   Reverse the table row order.
(define (reverse-rows!)
  (:= @rows (reverse (obs-peek @rows)))
  (:= @status "rows-reversed"))

(define app-renderer
  (render
   (window
    (vpanel
     (group "Visual Check"
            (input @name (lambda (v) (:= @name v)))
            (checkbox @enabled (lambda (v) (:= @enabled v)))
            (choice (list mode/basic mode/advanced)
                    @mode
                    (lambda (v) (:= @mode v)))
            (slider @level (lambda (v) (:= @level v)) 0 100)
            (progress @level 0 100)
            (table '(item state) @rows 'compact)
            (tab-panel @tab
                       (list (list 'overview
                                   (text (~> @name (lambda (n) (~a "overview:" n)))))
                             (list 'details
                                   (text (~> @mode (lambda (m) (~a "details:" m)))))
                             (list 'status
                                   (text (~> @status (lambda (s) (~a "status:" s))))))))
     (menu-bar
      (menu "Actions"
            (menu-item "bump level" bump-level!)
            (menu-item "reverse rows" reverse-rows!)
            (menu-item "reset" reset-all!)))))))

(mount-renderer! app-renderer)
