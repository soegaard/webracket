#lang webracket

;;;
;;; Window wrappers
;;;

;; i32->boolean : integer? -> boolean?
;;   Convert a browser i32 flag to a boolean.
(define (i32->boolean v)
  (not (zero? v)))

;; window : -> external/raw
;;   Read the current window object.
(define (window)
  (js-window-window))

;; window-self : -> external/raw
;;   Read the current window via self.
(define (window-self)
  (js-window-self))

;; window-document : -> external/raw
;;   Read the document for the current window.
(define (window-document)
  (js-window-document))

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

;; window-location : -> external/raw
;;   Read the window location object.
(define (window-location)
  (js-window-location))

;; window-set-location! : any/c -> void?
;;   Navigate the window to a new URL or Location.
(define (window-set-location! loc)
  (js-set-window-location! loc)
  (void))

;; window-open : string? [target any/c] [features any/c] [replace any/c] -> (or/c #f external?)
;;   Open a new browsing context.
(define (window-open url [target (void)] [features (void)] [replace (void)])
  (unless (string? url)
    (raise-argument-error 'window-open "string?" url))
  (js-window-open url target features replace))

;; window-fetch : any/c [any/c] -> external/raw
;;   Start a fetch request.
(define (window-fetch request [init (void)])
  (js-window-fetch request init))

;; window-confirm : string? -> boolean?
;;   Show a confirmation dialog.
(define (window-confirm message)
  (unless (string? message)
    (raise-argument-error 'window-confirm "string?" message))
  (i32->boolean (js-window-confirm message)))

;; window-prompt : string? [any/c] -> any/c
;;   Show a prompt dialog.
(define (window-prompt message [default-value (void)])
  (unless (string? message)
    (raise-argument-error 'window-prompt "string?" message))
  (js-window-prompt message default-value))

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

;; window-scroll-to : real? real? [any/c] -> void?
;;   Scroll the window to an absolute position.
(define (window-scroll-to x y [options (void)])
  (js-window-scroll-to x y options)
  (void))

;; window-scroll-by : real? real? [any/c] -> void?
;;   Scroll the window by a relative offset.
(define (window-scroll-by x y [options (void)])
  (js-window-scroll-by x y options)
  (void))

;; window-scroll : real? real? [any/c] -> void?
;;   Scroll the document to an absolute position.
(define (window-scroll x y [options (void)])
  (js-window-scroll x y options)
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

;; window-request-idle-callback : external? [any/c] -> u32?
;;   Schedule an idle callback.
(define (window-request-idle-callback callback [options (void)])
  (js-window-request-idle-callback callback options))

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

;; window-get-computed-style : external? [any/c] -> external/raw
;;   Read computed style for an element.
(define (window-get-computed-style element [pseudo-element (void)])
  (js-window-get-computed-style element pseudo-element))

;; window-structured-clone : any/c [any/c] -> any/c
;;   Clone a value using the structured-clone algorithm.
(define (window-structured-clone value [options (void)])
  (js-window-structured-clone value options))
