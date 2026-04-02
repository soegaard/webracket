#lang webracket

;;;
;;; Window wrappers
;;;

;; window-i32->boolean : integer? -> boolean?
;;   Convert a browser i32 flag to a boolean.
(define (window-i32->boolean v)
  (not (zero? v)))

;; window-resolve-optional : any/c -> any/c
;;   Treat #f as omitted, and force thunks when a literal value is needed.
(define (window-resolve-optional value)
  (cond
    [(eq? value #f) (void)]
    [(procedure? value) (value)]
    [else value]))

;; window-scroll-options : any/c any/c any/c -> void?
;;   Validate a browser ScrollToOptions dictionary wrapper.
(struct window-scroll-options (top left behavior)
  #:transparent
  #:constructor-name make-window-scroll-options
  #:guard (lambda (top left behavior name)
            (unless (or (eq? top #f) (real? top))
              (raise-argument-error name "(or/c #f real?)" top))
            (unless (or (eq? left #f) (real? left))
              (raise-argument-error name "(or/c #f real?)" left))
            (unless (or (eq? behavior #f) (string? behavior) (symbol? behavior))
              (raise-argument-error name "(or/c #f string? symbol?)" behavior))
            (values top left behavior)))

;; window-resolve-scroll-options : any/c -> (or/c #f window-scroll-options?)
;;   Treat #f as omitted and force thunks for scroll option values.
(define (window-resolve-scroll-options value)
  (define resolved (window-resolve-optional value))
  (cond
    [(eq? resolved (void)) #f]
    [(eq? resolved #f) #f]
    [else resolved]))

;; window-scroll-options->raw : window-scroll-options? real? real? -> external/raw
;;   Convert a Rackety scroll-options struct to a browser dictionary.
(define (window-scroll-options->raw options fallback-x fallback-y)
  (define top (window-scroll-options-top options))
  (define left (window-scroll-options-left options))
  (define behavior (window-scroll-options-behavior options))
  (define fields
    (append (if (eq? top #f)
                (list (vector "top" fallback-y))
                (list (vector "top" top)))
            (if (eq? left #f)
                (list (vector "left" fallback-x))
                (list (vector "left" left)))
            (if (eq? behavior #f)
                '()
                (list (vector "behavior"
                              (if (symbol? behavior)
                                  (symbol->string behavior)
                                  behavior))))))
  (js-object (list->vector fields)))

;; window : external/raw -> window?
;;   Wrap a browser Window object.
(struct window (raw) #:transparent)

;; Window : -> window?
;;   Read the current window object.
(define (Window)
  (window (js-window-window)))

;; window-wrap-optional : any/c -> (or/c #f window?)
;;   Wrap a browser Window-like value when one is present.
(define (window-wrap-optional value)
  (and (not (eq? value #f))
       (window value)))

;; window-document-info : external/raw -> window-document-info?
;;   Wrap the current document object.
(struct window-document-info (raw) #:transparent)

;; window-self : -> window?
;;   Read the current window via self.
(define (window-self)
  (window (js-window-self)))

;; window-document : -> window-document-info?
;;   Read the document for the current window.
(define (window-document)
  (window-document-info (js-window-document)))

;; window-name : -> string?
;;   Read the window name.
(define (window-name)
  (js-window-name))

;; window-set-name! : string? -> void?
;;   Set the window name.
(define (window-set-name! name)
  (unless (string? name)
    (raise-argument-error 'window-set-name! "string?" name))
  (js-set-window-name! name)
  (void))

;; window-location-info : external/raw -> window-location-info?
;;   Wrap the current window location object.
(struct window-location-info (raw) #:transparent)

;; window-location : -> window-location-info?
;;   Read the window location object.
(define (window-location)
  (window-location-info (js-window-location)))

;; window-set-location! : any/c -> void?
;;   Navigate the window to a new URL or Location.
(define (window-set-location! loc)
  (js-set-window-location! loc)
  (void))

;; window-open : string? [target (or/c string? procedure?) #f] [features (or/c string? procedure?) #f] [replace (or/c boolean? procedure?) #f] -> (or/c #f window?)
;;   Open a new browsing context.
(define (window-open url [target #f] [features #f] [replace #f])
  (unless (string? url)
    (raise-argument-error 'window-open "string?" url))
  (define target* (window-resolve-optional target))
  (define features* (window-resolve-optional features))
  (define replace* (window-resolve-optional replace))
  (when (and (not (eq? target* (void)))
             (not (string? target*)))
    (raise-argument-error 'window-open "(or/c #f string? procedure?)" target))
  (when (and (not (eq? features* (void)))
             (not (string? features*)))
    (raise-argument-error 'window-open "(or/c #f string? procedure?)" features))
  (when (and (not (eq? replace* (void)))
             (not (boolean? replace*)))
    (raise-argument-error 'window-open "(or/c #f boolean? procedure?)" replace))
  (window-wrap-optional (js-window-open url target* features* replace*)))

;; window-fetch : any/c [any/c #f] -> external/raw
;;   Start a fetch request.
(define (window-fetch request [init #f])
  (js-window-fetch request (window-resolve-optional init)))

;; window-confirm : string? -> boolean?
;;   Show a confirmation dialog.
(define (window-confirm message)
  (unless (string? message)
    (raise-argument-error 'window-confirm "string?" message))
  (window-i32->boolean (js-window-confirm message)))

;; window-prompt : string? [any/c #f] -> any/c
;;   Show a prompt dialog.
(define (window-prompt message [default-value #f])
  (unless (string? message)
    (raise-argument-error 'window-prompt "string?" message))
  (js-window-prompt message (window-resolve-optional default-value)))

;; window-alert : string? -> void?
;;   Show an alert dialog.
(define (window-alert message)
  (unless (string? message)
    (raise-argument-error 'window-alert "string?" message))
  (js-window-alert message)
  (void))

;; window-print : -> void?
;;   Show the print dialog.
(define (window-print)
  (js-window-print)
  (void))

;; window-focus : -> void?
;;   Focus the current browsing context.
(define (window-focus)
  (js-window-focus)
  (void))

;; window-stop : -> void?
;;   Stop loading the current browsing context.
(define (window-stop)
  (js-window-stop)
  (void))

;; window-scroll-to : real? real? [or/c #f window-scroll-options? procedure?] -> void?
;;   Scroll the window to an absolute position.
(define (window-scroll-to x y [options #f])
  (define options* (window-resolve-scroll-options options))
  (cond
    [(eq? options* #f)
     (js-window-scroll-to x y (void))]
    [(window-scroll-options? options*)
     (js-window-scroll-to x y (window-scroll-options->raw options* x y))]
    [else
     (raise-argument-error 'window-scroll-to
                           "(or/c #f window-scroll-options? procedure?)"
                           options)])
  (void))

;; window-scroll-by : real? real? [or/c #f window-scroll-options? procedure?] -> void?
;;   Scroll the window by a relative offset.
(define (window-scroll-by x y [options #f])
  (define options* (window-resolve-scroll-options options))
  (cond
    [(eq? options* #f)
     (js-window-scroll-by x y (void))]
    [(window-scroll-options? options*)
     (js-window-scroll-by x y (window-scroll-options->raw options* x y))]
    [else
     (raise-argument-error 'window-scroll-by
                           "(or/c #f window-scroll-options? procedure?)"
                           options)])
  (void))

;; window-scroll : real? real? [or/c #f window-scroll-options? procedure?] -> void?
;;   Scroll the document to an absolute position.
(define (window-scroll x y [options #f])
  (define options* (window-resolve-scroll-options options))
  (cond
    [(eq? options* #f)
     (js-window-scroll x y (void))]
    [(window-scroll-options? options*)
     (js-window-scroll x y (window-scroll-options->raw options* x y))]
    [else
     (raise-argument-error 'window-scroll
                           "(or/c #f window-scroll-options? procedure?)"
                           options)])
  (void))

;; window-resize-to : real? real? -> void?
;;   Resize the window to a given size.
(define (window-resize-to w h)
  (js-window-resize-to w h)
  (void))

;; window-resize-by : real? real? -> void?
;;   Resize the window by a delta.
(define (window-resize-by w h)
  (js-window-resize-by w h)
  (void))

;; window-move-to : real? real? -> void?
;;   Move the window to a given position.
(define (window-move-to x y)
  (js-window-move-to x y)
  (void))

;; window-move-by : real? real? -> void?
;;   Move the window by a given delta.
(define (window-move-by x y)
  (js-window-move-by x y)
  (void))

;; window-set-timeout : external? -> u32?
;;   Schedule a one-shot timer callback.
(define (window-set-timeout callback)
  (js-window-set-timeout callback))

;; window-set-timeout/delay : external? real? -> u32?
;;   Schedule a one-shot timer callback with an explicit delay.
(define (window-set-timeout/delay callback delay)
  (js-window-set-timeout/delay callback delay))

;; window-clear-timeout : exact-integer? -> void?
;;   Cancel a timeout handle.
(define (window-clear-timeout id)
  (js-window-clear-timeout id)
  (void))

;; window-set-interval : external? real? -> u32?
;;   Schedule a repeated timer callback.
(define (window-set-interval callback delay)
  (js-window-set-interval callback delay))

;; window-clear-interval : exact-integer? -> void?
;;   Cancel an interval handle.
(define (window-clear-interval id)
  (js-window-clear-interval id)
  (void))

;; window-request-animation-frame : external? -> u32?
;;   Schedule a repaint callback.
(define (window-request-animation-frame callback)
  (js-window-request-animation-frame callback))

;; window-cancel-animation-frame : exact-integer? -> void?
;;   Cancel a repaint callback.
(define (window-cancel-animation-frame id)
  (js-window-cancel-animation-frame id)
  (void))

;; window-request-idle-callback : external? [any/c #f] -> u32?
;;   Schedule an idle callback.
(define (window-request-idle-callback callback [options #f])
  (js-window-request-idle-callback callback (window-resolve-optional options)))

;; window-cancel-idle-callback : exact-integer? -> void?
;;   Cancel an idle callback.
(define (window-cancel-idle-callback id)
  (js-window-cancel-idle-callback id)
  (void))

;; window-get-selection : -> (or/c #f external?)
;;   Read the current selection object.
(define (window-get-selection)
  (js-window-get-selection))

;; window-match-media : string? -> external/raw
;;   Evaluate a media query.
(define (window-match-media query)
  (unless (string? query)
    (raise-argument-error 'window-match-media "string?" query))
  (js-window-match-media query))

;; window-get-computed-style : external? [any/c #f] -> external/raw
;;   Read computed style for an element.
(define (window-get-computed-style element [pseudo-element #f])
  (js-window-get-computed-style element (window-resolve-optional pseudo-element)))

;; window-structured-clone : any/c [any/c #f] -> any/c
;;   Clone a value using the structured-clone algorithm.
(define (window-structured-clone value [options #f])
  (js-window-structured-clone value (window-resolve-optional options)))
