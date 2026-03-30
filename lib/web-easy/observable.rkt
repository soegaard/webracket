#lang webracket

;;;
;;; web-easy Observables
;;;

;; Observable data model and operations: creation, update, subscription, and mapping.
;;
;; Exports:
;;   obs?            Predicate: true when value is an observable.
;;   obs             Construct an observable value.
;;   obs-name        Return the observable name symbol.
;;   obs-observe!    Register an observer callback.
;;   obs-unobserve!  Remove an observer callback.
;;   obs-update!     Update observable value via function.
;;   obs-set!        Set observable value directly.
;;   obs-peek        Read current observable value.
;;   obs-map         Create a derived mapped observable.
;;   obs-combine     Create a derived observable from many observables.
;;   obs-filter      Create a derived filtered observable.

(define-values
  (obs?
   obs
   obs-name
   obs-observe!
   obs-unobserve!
   obs-update!
   obs-set!
   obs-peek
   obs-map
   obs-combine
   obs-filter)
  (let ()
    (struct observable (id name value observers updatable?) #:mutable #:transparent)

    ;; Constants used by the observable runtime.
    (define default-observable-name 'anon) ; Default name for unnamed observables.

    ;; next-observable-id : -> exact-positive-integer?
    ;;   Produce a fresh monotonically increasing id for each observable.
    (define next-observable-id
      (let ([n 0])
        (lambda ()
          (set! n (add1 n))
          n)))

    ;; obs? : any/c -> boolean?
    ;;   Check whether v is an observable value.
    (define (obs? v)
      (observable? v))

    ;; obs : any/c [symbol?] [boolean?] -> observable?
    ;;   Construct an observable with optional name and derived? flag.
    (define (obs v [name default-observable-name] [derived? #f])
      (observable (next-observable-id) name v '() (not derived?)))

    ;; obs-name : observable? -> symbol?
    ;;   Return the debug/display name attached to o.
    (define (obs-name o)
      (observable-name o))

    ;; obs-peek : observable? -> any/c
    ;;   Return the current value held by o.
    (define (obs-peek o)
      (observable-value o))

    ;; obs-observe! : observable? (-> any/c any/c) -> void?
    ;;   Register f to be called on each update of o.
    (define (obs-observe! o f)
      (set-observable-observers! o (cons f (observable-observers o)))
      (void))

    ;; obs-unobserve! : observable? (-> any/c any/c) -> void?
    ;;   Remove f from o's observer set.
    (define (obs-unobserve! o f)
      (set-observable-observers!
       o
       (filter (lambda (g) (not (eq? g f)))
               (observable-observers o)))
      (void))

    ;; notify-observers : observable? any/c -> void?
    ;;   Notify all currently registered observers with value v.
    (define (notify-observers o v)
      (for-each (lambda (f) (f v))
                (observable-observers o)))

    ;; obs-update! : observable? (-> any/c any/c) -> any/c
    ;;   Update o by applying f to the current value and store the result.
    (define (obs-update! o f)
      (unless (observable-updatable? o)
        (raise-arguments-error 'obs-update!
                               "cannot update derived observable"
                               "observable"
                               o))
      (define v (f (observable-value o)))
      (set-observable-value! o v)
      (notify-observers o v)
      v)

    ;; obs-set! : observable? any/c -> void?
    ;;   Replace o's current value with v.
    (define (obs-set! o v)
      (void (obs-update! o (lambda (_) v))))

    ;; obs-map : observable? (-> any/c any/c) -> observable?
    ;;   Create a derived observable that maps values of o through f.
    (define (obs-map o f)
      (define d (obs (f (obs-peek o)) default-observable-name #t))
      (define (update-derived v)
        (define mapped (f v))
        (set-observable-value! d mapped)
        (notify-observers d mapped))
      (obs-observe! o update-derived)
      d)

    ;; obs-combine : (-> any/c ... any/c) observable? ... -> observable?
    ;;   Create a derived observable by applying f to values from one or more observables.
    (define (obs-combine f . os)
      (when (null? os)
        (raise-arguments-error 'obs-combine
                               "expected at least one observable"
                               "observables"
                               os))
      (for-each (lambda (o)
                  (unless (obs? o)
                    (raise-arguments-error 'obs-combine
                                           "expected observable argument"
                                           "value"
                                           o)))
                os)
      (define (combined-value)
        (apply f (map obs-peek os)))
      (define d (obs (combined-value) default-observable-name #t))
      (define (update-derived _v)
        (define combined (combined-value))
        (set-observable-value! d combined)
        (notify-observers d combined))
      (for-each (lambda (o)
                  (obs-observe! o update-derived))
                os)
      d)

    ;; obs-filter : observable? (-> any/c any/c) [any/c] -> observable?
    ;;   Create a derived observable that updates only when predicate f is truthy.
    (define (obs-filter o f [default #f])
      (define initial (obs-peek o))
      (define d (obs (if (f initial) initial default)
                     default-observable-name
                     #t))
      (define (update-derived v)
        (when (f v)
          (set-observable-value! d v)
          (notify-observers d v)))
      (obs-observe! o update-derived)
      d)

    (values obs?
            obs
            obs-name
            obs-observe!
            obs-unobserve!
            obs-update!
            obs-set!
            obs-peek
            obs-map
            obs-combine
            obs-filter)))

(define-syntax (obs-watch! stx)
  (syntax-case stx ()
    [(_ o ... f)
     #'(obs-observe! (obs-combine list o ...)
         (λ (as) (apply f as)))]))
