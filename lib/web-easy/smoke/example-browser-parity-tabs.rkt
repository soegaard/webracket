;;;
;;; web-easy Browser Parity Tabs Example
;;;

;; Parity example: tabs selection/content switching.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)

(define @selected (@ 'overview))

(define app-renderer
  (render
   (window
    (vpanel
     (tab-panel
      @selected
      (list (cons 'overview (text "Overview panel"))
            (cons 'details  (text "Details panel"))
            (cons 'help     (text "Help panel"))))))))

(mount-renderer! app-renderer)
