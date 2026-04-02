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

;; window : -> external/raw
;;   Read the current window object.
(define (window)
  (js-window-window))

;; window-document-info : external/raw -> window-document-info?
;;   Wrap the current document object.
(struct window-document-info (raw) #:transparent)

;; window-self : -> external/raw
;;   Read the current window via self.
(define (window-self)
  (js-window-self))

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

;; window-open : string? [target any/c #f] [features any/c #f] [replace any/c #f] -> (or/c #f external?)
;;   Open a new browsing context.
(define (window-open url [target #f] [features #f] [replace #f])
  (unless (string? url)
    (raise-argument-error 'window-open "string?" url))
  (js-window-open url
                  (window-resolve-optional target)
                  (window-resolve-optional features)
                  (window-resolve-optional replace)))

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

;; window-scroll-to : real? real? [any/c #f] -> void?
;;   Scroll the window to an absolute position.
(define (window-scroll-to x y [options #f])
  (js-window-scroll-to x y (window-resolve-optional options))
  (void))

;; window-scroll-by : real? real? [any/c #f] -> void?
;;   Scroll the window by a relative offset.
(define (window-scroll-by x y [options #f])
  (js-window-scroll-by x y (window-resolve-optional options))
  (void))

;; window-scroll : real? real? [any/c #f] -> void?
;;   Scroll the document to an absolute position.
(define (window-scroll x y [options #f])
  (js-window-scroll x y (window-resolve-optional options))
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
