#lang webracket

;;;
;;; web-easy Operators
;;;

;; Convenience operators that wrap core observable operations.
;;
;; Exports:
;;   @    Convert a value to an observable when needed.
;;   :=   Set observable to value and return that value.
;;   <~   Update observable by applying a function.
;;   λ<~  Build a thunk that applies <~ when invoked.
;;   ~>   Map observable through a function to a derived observable.
;;   ~#>  Filter observable updates through a predicate.

(define-values
  (@ := <~ λ<~ ~> ~#>)
  (let ()
    ;; @ : any/c -> observable?
    ;;   Convert v to an observable if needed.
    (define (|@| v)
      (if (obs? v) v (obs v)))

    ;; := : observable? any/c -> any/c
    ;;   Set the observable value to v and return v.
    (define (:= o v)
      (obs-set! o v)
      v)

    ;; <~ : observable? (-> any/c any/c) -> any/c
    ;;   Alias for observable update.
    (define (<~ o f)
      (obs-update! o f))

    ;; λ<~ : observable? (-> any/c any/c) -> (-> any/c)
    ;;   Build a thunk that applies <~ to o using f.
    (define (λ<~ o f)
      (lambda ()
        (<~ o f)))

    ;; ~> : observable? (-> any/c any/c) -> observable?
    ;;   Alias for observable mapping.
    (define (~> o f)
      (obs-map o f))

    ;; ~#> : observable? (-> any/c any/c) -> observable?
    ;;   Alias for observable filtering.
    (define (~#> o f)
      (obs-filter o f))

    (values |@| := <~ λ<~ ~> ~#>)))
