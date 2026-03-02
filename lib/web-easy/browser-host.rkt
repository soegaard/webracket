#lang webracket

;;;
;;; web-easy Browser Host
;;;

;; Host integration helpers for mounting rendered trees into the browser DOM.
;;
;; Exports:
;;   mount-renderer!  Mount renderer root into browser container.

(define-values
  (mount-renderer!)
  (let ()
    ;; mount-renderer! : renderer? [any/c] -> void?
    ;;   Mount renderer root node into browser container via backend hook.
    ;;   Optional parameter container defaults to backend default container.
    (define (mount-renderer! r [container #f])
      (define root (renderer-root r))
      (if container
          (backend-mount-root! root container)
          (backend-mount-root! root))
      (void))

    (values mount-renderer!)))
