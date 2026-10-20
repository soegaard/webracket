#lang webracket/base

(require browser/element
         (only-in webracket/unsafe/dom
                  js-constructible?
                  js-index
                  js-instanceof
                  js-new
                  js-object
                  js-nullish?
                  js-ref
                  js-ref/extern
                  js-send/extern/nullish
                  js-typeof
                  js-var)
         (only-in webracket/primitives
                  external?
                  procedure->external))

(provide (except-out (all-defined-out)
                     resize-observer-entry-info?
                     resize-observer-entry-info-raw))

;;;
;;; ResizeObserver wrappers
;;;

(struct resize-observer-info (raw) #:transparent)
(struct resize-observer-entry-info (raw)
  #:transparent
  #:constructor-name resize-observer-entry)

(struct resize-observer-options (box)
  #:transparent
  #:constructor-name make-resize-observer-options
  #:guard
  (lambda (box name)
    (unless (or (eq? box #f) (string? box) (symbol? box))
      (raise-argument-error name "(or/c #f string? symbol?)" box))
    (values box)))

(define (resize-observer-supported?)
  (js-constructible? (js-var "ResizeObserver")))

(define (resize-observer raw)
  (resize-observer-info raw))

(define (resize-observer-raw value)
  (resize-observer-info-raw value))

;; resize-observer-entry-raw : resize-observer-entry? -> external/raw
;;   Extract the raw browser ResizeObserverEntry object.
(define (resize-observer-entry-raw value)
  (resize-observer-entry-info-raw value))

;; observer-element-raw/checked : symbol? dom-element? -> external/raw
;;   Validate and unwrap a DOM element before passing it to observer host calls.
(define (observer-element-raw/checked who element)
  (define raw (element-unwrap element))
  (unless (shared-dom-raw-host-value? raw)
    (raise-argument-error who
                          "dom-element with non-nullish raw value"
                          element))
  raw)

;; observer-element-wrapper? : any/c -> boolean?
;;   Report whether value is already a DOM wrapper accepted by element-unwrap.
(define (observer-element-wrapper? value)
  (not (eq? (element-unwrap value) value)))

(define (resize-observer? v)
  (and (resize-observer-host-value? v)
       (resize-observer-core-methods-present? (resize-observer-raw v))))

;; resize-observer-host-value? : any/c -> boolean?
;;   Report whether v wraps an external ResizeObserver-like host value.
(define (resize-observer-host-value? v)
  (and (resize-observer-info? v)
       (resize-observer-supported?)
       (resize-observer-raw-host-value? (resize-observer-raw v))
       (js-instanceof (resize-observer-raw v) (js-var "ResizeObserver"))))

;; resize-observer-host-method-present? : external/raw string? -> boolean?
;;   Report whether raw exposes a callable ResizeObserver method.
(define (resize-observer-host-method-present? raw method-name)
  (and (resize-observer-raw-host-value? raw)
       (let ([method (js-ref/extern raw method-name)])
         (and (not (js-nullish? method))
              (string=? (js-typeof method) "function")))))

;; resize-observer-core-methods-present? : external/raw -> boolean?
;;   Report whether raw has the required ResizeObserver methods.
(define (resize-observer-core-methods-present? raw)
  (and (resize-observer-host-method-present? raw "observe")
       (resize-observer-host-method-present? raw "unobserve")
       (resize-observer-host-method-present? raw "disconnect")))

;; resize-observer-raw-host-value? : any/c -> boolean?
;;   Report whether raw is a non-nullish external host value.
(define (resize-observer-raw-host-value? raw)
  (and (external? raw)
       (not (js-nullish? raw))))

;; resize-observer-entry-host-value? : any/c -> boolean?
;;   Report whether value wraps a non-null ResizeObserverEntry-like host value.
(define (resize-observer-entry-host-value? value)
  (and (resize-observer-entry-info? value)
       (resize-observer-raw-host-value?
        (resize-observer-entry-raw value))))

;; resize-observer-entry-property-present? : external/raw string? -> boolean?
;;   Report whether raw exposes a non-nullish entry property.
(define (resize-observer-entry-property-present? raw property-name)
  (and (resize-observer-raw-host-value? raw)
       (not (js-nullish? (js-ref/extern raw property-name)))))

;; resize-observer-entry-core-properties-present? : external/raw -> boolean?
;;   Report whether raw has the required ResizeObserverEntry properties.
(define (resize-observer-entry-core-properties-present? raw)
  (and (resize-observer-entry-property-present? raw "target")
       (resize-observer-entry-property-present? raw "contentRect")))

;; resize-observer-entry? : any/c -> boolean?
;;   Report whether value is a usable wrapped ResizeObserverEntry value.
(define (resize-observer-entry? value)
  (and (resize-observer-entry-host-value? value)
       (resize-observer-entry-core-properties-present?
        (resize-observer-entry-raw value))))

;; check-resize-observer-host-method : symbol? resize-observer-info? string? -> void?
;;   Ensure a required ResizeObserver host method is available.
(define (check-resize-observer-host-method who observer method-name)
  (unless (resize-observer-host-method-present? (resize-observer-raw observer)
                                                method-name)
    (raise-arguments-error who
                           (string-append "ResizeObserver."
                                          method-name
                                          " is not available"))))

;; check-resize-observer-entry-property : symbol? resize-observer-entry? string? -> void?
;;   Ensure a required ResizeObserverEntry property is available.
(define (check-resize-observer-entry-property who entry property-name)
  (when (js-nullish? (js-ref/extern (resize-observer-entry-raw entry) property-name))
    (raise-arguments-error who
                           (string-append "ResizeObserverEntry."
                                          property-name
                                          " is not available"))))

(define (resize-observer-options->raw options)
  (define box (resize-observer-options-box options))
  (js-object
   (if (eq? box #f)
       #()
       (vector (vector "box" (if (symbol? box) (symbol->string box) box))))))

(define (resize-entries->vector entries)
  (cond
    ;; JS callback arguments are FASL-decoded by the host bridge; a plain
    ;; browser Array such as ResizeObserver entries arrives here as a Racket
    ;; vector of raw external entries.
    [(vector? entries)
     (let ([len (vector-length entries)])
       (list->vector
        (let loop ([i 0])
          (if (= i len)
              '()
              (cons (resize-observer-entry (vector-ref entries i))
                    (loop (add1 i)))))))]
    [else
     (define len (js-ref entries "length"))
     (list->vector
      (let loop ([i 0])
        (if (= i len)
            '()
            (cons (resize-observer-entry (js-index entries i))
                  (loop (add1 i))))))]))

(define (resize-observer-callback->external who callback)
  (cond
    [(external? callback) callback]
    [(procedure? callback)
     ;; The native observer retains this bridge while it is live. A global
     ;; cache would retain callbacks after an observer is disconnected.
     (procedure->external
      (lambda (entries raw-observer)
        (callback (resize-entries->vector entries)
                  (resize-observer raw-observer))
        (void)))]
    [else
     (raise-argument-error who "(or/c procedure? external?)" callback)]))

(define (observer-target->raw who target)
  (cond
    [(observer-element-wrapper? target) (observer-element-raw/checked who target)]
    [(resize-observer-raw-host-value? target) target]
    [else
     (raise-argument-error who "(or/c dom-element? external?)" target)]))

(define (make-resize-observer callback)
  (unless (resize-observer-supported?)
    (raise-arguments-error 'make-resize-observer
                           "ResizeObserver is not available"))
  (resize-observer
   (js-new (js-var "ResizeObserver")
           (vector (resize-observer-callback->external
                    'make-resize-observer
                    callback)))))

(define (resize-observer-observe! observer target [options #f])
  (unless (resize-observer-host-value? observer)
    (raise-argument-error 'resize-observer-observe! "resize-observer-info?" observer))
  (check-resize-observer-host-method 'resize-observer-observe! observer "observe")
  (unless (or (eq? options #f) (resize-observer-options? options))
    (raise-argument-error 'resize-observer-observe! "(or/c #f resize-observer-options?)" options))
  (define target* (observer-target->raw 'resize-observer-observe! target))
  (if (eq? options #f)
      (js-send/extern/nullish (resize-observer-raw observer) "observe" (vector target*))
      (js-send/extern/nullish (resize-observer-raw observer)
                              "observe"
                              (vector target*
                                      (resize-observer-options->raw options))))
  (void))

(define (resize-observer-unobserve! observer target)
  (unless (resize-observer-host-value? observer)
    (raise-argument-error 'resize-observer-unobserve! "resize-observer-info?" observer))
  (check-resize-observer-host-method 'resize-observer-unobserve! observer "unobserve")
  (js-send/extern/nullish (resize-observer-raw observer)
                          "unobserve"
                          (vector (observer-target->raw 'resize-observer-unobserve! target)))
  (void))

(define (resize-observer-disconnect! observer)
  (unless (resize-observer-host-value? observer)
    (raise-argument-error 'resize-observer-disconnect! "resize-observer-info?" observer))
  (check-resize-observer-host-method 'resize-observer-disconnect! observer "disconnect")
  (js-send/extern/nullish (resize-observer-raw observer) "disconnect" (vector))
  (void))

(define (resize-observer-entry-target entry)
  (unless (resize-observer-entry-host-value? entry)
    (raise-argument-error 'resize-observer-entry-target "resize-observer-entry?" entry))
  (check-resize-observer-entry-property 'resize-observer-entry-target entry "target")
  (element-wrap (js-ref/extern (resize-observer-entry-raw entry) "target")))

(define (resize-observer-entry-content-rect entry)
  (unless (resize-observer-entry-host-value? entry)
    (raise-argument-error 'resize-observer-entry-content-rect "resize-observer-entry?" entry))
  (check-resize-observer-entry-property 'resize-observer-entry-content-rect entry "contentRect")
  (dom-rect-wrap (js-ref/extern (resize-observer-entry-raw entry) "contentRect")))
