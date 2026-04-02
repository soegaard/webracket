#lang webracket

;;;
;;; Performance wrappers
;;;

(include-lib iterator)

;; procedure->external-cache : hash?
;;   Cache JS callback wrappers so the same procedure maps to the same external.
(define procedure->external-cache (make-hasheq))

;; performance-event-count-map : external/raw -> performance-event-count-map?
;;   Wrap a browser EventCounts object in a checked structure.
(struct performance-event-count-map (raw) #:transparent)

;; performance-event-count-map-size : performance-event-count-map? -> exact-nonnegative-integer?
;;   Read the number of recorded event types.
(define (performance-event-count-map-size counts)
  (unless (performance-event-count-map? counts)
    (raise-argument-error 'performance-event-count-map-size
                          "performance-event-count-map?"
                          counts))
  (js-ref (performance-event-count-map-raw counts) "size"))

;; performance-event-count-map-entries : performance-event-count-map? -> iterator?
;;   Read the iterator of event-count entries.
(define (performance-event-count-map-entries counts)
  (unless (performance-event-count-map? counts)
    (raise-argument-error 'performance-event-count-map-entries
                          "performance-event-count-map?"
                          counts))
  (make-iterator (js-send/value (performance-event-count-map-raw counts) "entries" (vector))))

;; performance-event-count-map-keys : performance-event-count-map? -> iterator?
;;   Read the iterator of event-count keys.
(define (performance-event-count-map-keys counts)
  (unless (performance-event-count-map? counts)
    (raise-argument-error 'performance-event-count-map-keys
                          "performance-event-count-map?"
                          counts))
  (make-iterator (js-send/value (performance-event-count-map-raw counts) "keys" (vector))))

;; performance-event-count-map-values : performance-event-count-map? -> iterator?
;;   Read the iterator of event-count values.
(define (performance-event-count-map-values counts)
  (unless (performance-event-count-map? counts)
    (raise-argument-error 'performance-event-count-map-values
                          "performance-event-count-map?"
                          counts))
  (make-iterator (js-send/value (performance-event-count-map-raw counts) "values" (vector))))

;; performance-event-count-map-get : performance-event-count-map? string? -> (or/c #f exact-nonnegative-integer?)
;;   Read the count for a specific event type.
(define (performance-event-count-map-get counts event-type)
  (unless (performance-event-count-map? counts)
    (raise-argument-error 'performance-event-count-map-get
                          "performance-event-count-map?"
                          counts))
  (unless (string? event-type)
    (raise-argument-error 'performance-event-count-map-get "string?" event-type))
  (js-send/value (performance-event-count-map-raw counts) "get" (vector event-type)))

;; performance-event-count-map-has? : performance-event-count-map? string? -> boolean?
;;   Check whether a specific event type is exposed.
(define (performance-event-count-map-has? counts event-type)
  (unless (performance-event-count-map? counts)
    (raise-argument-error 'performance-event-count-map-has?
                          "performance-event-count-map?"
                          counts))
  (unless (string? event-type)
    (raise-argument-error 'performance-event-count-map-has? "string?" event-type))
  (js-send/boolean (performance-event-count-map-raw counts) "has" (vector event-type)))

;; performance-event-count-map-for-each : performance-event-count-map? (procedure? external?) -> void?
;;   Iterate over event-count entries with a Map-style callback.
(define (performance-event-count-map-for-each counts proc)
  (unless (performance-event-count-map? counts)
    (raise-argument-error 'performance-event-count-map-for-each
                          "performance-event-count-map?"
                          counts))
  (unless (or (procedure? proc)
              (external? proc))
    (raise-argument-error 'performance-event-count-map-for-each
                          "(or/c procedure? external?)"
                          proc))
  (define proc*
    (cond
      [(external? proc) proc]
      [else
       (define cached (hash-ref procedure->external-cache proc #f))
       (cond
         [cached cached]
         [else
          (define external (procedure->external proc))
          (hash-set! procedure->external-cache proc external)
          external])]))
  (js-send/value (performance-event-count-map-raw counts) "forEach" (vector proc*))
  (void))

;; performance-event-counts : -> (or/c #f performance-event-count-map?)
;;   Read the event counts map for the current page.
(define (performance-event-counts)
  (define counts (js-performance-event-counts))
  (if counts
      (performance-event-count-map counts)
      #f))

;; performance-interaction-count : -> exact-nonnegative-integer?
;;   Read the interaction count for the current page.
(define (performance-interaction-count)
  (js-performance-interaction-count))

;; performance-memory-info : external/raw -> performance-memory-info?
;;   Wrap the browser-specific memory information object.
(struct performance-memory-info (raw) #:transparent)

;; performance-memory-info-js-heap-size-limit : performance-memory-info? -> exact-nonnegative-integer?
;;   Read the browser's JavaScript heap size limit.
(define (performance-memory-info-js-heap-size-limit memory-info)
  (unless (performance-memory-info? memory-info)
    (raise-argument-error 'performance-memory-info-js-heap-size-limit
                          "performance-memory-info?"
                          memory-info))
  (js-ref (performance-memory-info-raw memory-info) "jsHeapSizeLimit"))

;; performance-memory-info-total-js-heap-size : performance-memory-info? -> exact-nonnegative-integer?
;;   Read the browser's total JavaScript heap size.
(define (performance-memory-info-total-js-heap-size memory-info)
  (unless (performance-memory-info? memory-info)
    (raise-argument-error 'performance-memory-info-total-js-heap-size
                          "performance-memory-info?"
                          memory-info))
  (js-ref (performance-memory-info-raw memory-info) "totalJSHeapSize"))

;; performance-memory-info-used-js-heap-size : performance-memory-info? -> exact-nonnegative-integer?
;;   Read the browser's used JavaScript heap size.
(define (performance-memory-info-used-js-heap-size memory-info)
  (unless (performance-memory-info? memory-info)
    (raise-argument-error 'performance-memory-info-used-js-heap-size
                          "performance-memory-info?"
                          memory-info))
  (js-ref (performance-memory-info-raw memory-info) "usedJSHeapSize"))

;; performance-memory : -> (or/c #f performance-memory-info?)
;;   Read the browser-specific memory information object.
(define (performance-memory)
  (define memory (js-performance-memory))
  (and memory (performance-memory-info memory)))

;; performance-resolve-optional : any/c -> any/c
;;   Treat #f as omitted, and force thunks when a literal value is needed.
(define (performance-resolve-optional value)
  (cond
    [(eq? value #f) (void)]
    [(procedure? value) (value)]
    [else value]))

;; performance-time-origin : -> real?
;;   Read the high-resolution performance origin.
(define (performance-time-origin)
  (js-performance-time-origin))

;; performance-now : -> f64
;;   Read a high-resolution monotonic timestamp.
(define (performance-now)
  (js-performance-now))

;; performance-clear-marks : [string?] -> void?
;;   Clear performance marks, optionally filtered by name.
(define (performance-clear-marks [mark-name #f])
  (when (and (not (eq? mark-name #f))
             (not (string? mark-name))
             (not (procedure? mark-name)))
    (raise-argument-error 'performance-clear-marks "(or/c #f string? procedure?)" mark-name))
  (js-performance-clear-marks (performance-resolve-optional mark-name))
  (void))

;; performance-clear-measures : [string?] -> void?
;;   Clear performance measures, optionally filtered by name.
(define (performance-clear-measures [measure-name #f])
  (when (and (not (eq? measure-name #f))
             (not (string? measure-name))
             (not (procedure? measure-name)))
    (raise-argument-error 'performance-clear-measures "(or/c #f string? procedure?)" measure-name))
  (js-performance-clear-measures (performance-resolve-optional measure-name))
  (void))

;; performance-clear-resource-timings : -> void?
;;   Clear the resource timing buffer.
(define (performance-clear-resource-timings)
  (js-performance-clear-resource-timings)
  (void))

;; performance-get-entries : -> external/raw
;;   Read the current performance entry list.
(define (performance-get-entries)
  (js-performance-get-entries))

;; performance-get-entries-by-name : string? [string?] -> external/raw
;;   Read performance entries filtered by name and optional type.
(define (performance-get-entries-by-name name [entry-type #f])
  (unless (string? name)
    (raise-argument-error 'performance-get-entries-by-name "string?" name))
  (when (and (not (eq? entry-type #f))
             (not (string? entry-type))
             (not (procedure? entry-type)))
    (raise-argument-error 'performance-get-entries-by-name "(or/c #f string? procedure?)" entry-type))
  (js-performance-get-entries-by-name name (performance-resolve-optional entry-type)))

;; performance-get-entries-by-type : string? -> external/raw
;;   Read performance entries filtered by type.
(define (performance-get-entries-by-type entry-type)
  (unless (string? entry-type)
    (raise-argument-error 'performance-get-entries-by-type "string?" entry-type))
  (js-performance-get-entries-by-type entry-type))

;; performance-mark : string? [any/c] -> external/raw
;;   Create a performance mark and return the entry.
(define (performance-mark name [options #f])
  (unless (string? name)
    (raise-argument-error 'performance-mark "string?" name))
  (js-performance-mark name (performance-resolve-optional options)))

;; performance-measure : string? [any/c] [any/c] -> external/raw
;;   Create a performance measure and return the entry.
(define (performance-measure name [start-or-options #f] [end-mark #f])
  (unless (string? name)
    (raise-argument-error 'performance-measure "string?" name))
  (js-performance-measure name
                          (performance-resolve-optional start-or-options)
                          (performance-resolve-optional end-mark)))

;; performance-measure-user-agent-specific-memory : -> external/raw
;;   Estimate user-agent-specific memory usage.
(define (performance-measure-user-agent-specific-memory)
  (js-performance-measure-user-agent-specific-memory))

;; performance-set-resource-timing-buffer-size : exact-nonnegative-integer? -> void?
;;   Set the resource timing buffer size.
(define (performance-set-resource-timing-buffer-size size)
  (unless (exact-nonnegative-integer? size)
    (raise-argument-error 'performance-set-resource-timing-buffer-size
                          "exact-nonnegative-integer?"
                          size))
  (js-performance-set-resource-timing-buffer-size size)
  (void))

;; performance-to-json : -> external/raw
;;   Return a JSON-like representation of the performance object.
(define (performance-to-json)
  (js-performance-to-json))
