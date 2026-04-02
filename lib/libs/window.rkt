#lang webracket

;;;
;;; Window wrappers
;;;

;; window-i32->boolean : integer? -> boolean?
;;   Convert a browser i32 flag to a boolean.
(define (window-i32->boolean v)
  (not (zero? v)))

;; window-stringish->string : symbol? any/c -> string?
;;   Normalize a string-like wrapper argument to a browser string.
(define (window-stringish->string who v)
  (cond
    [(string? v) v]
    [(symbol? v) (symbol->string v)]
    [else (raise-argument-error who "(or/c string? symbol?)" v)]))

;; window-resolve-optional : any/c -> any/c
;;   Treat #f as omitted, and force thunks when a literal value is needed.
(define (window-resolve-optional value)
  (cond
    [(eq? value #f) (void)]
    [(procedure? value) (value)]
    [else value]))

;; window-resolve-stringish-optional : symbol? any/c -> any/c
;;   Resolve an optional string-like argument and normalize strings and symbols.
(define (window-resolve-stringish-optional who value)
  (define resolved (window-resolve-optional value))
  (cond
    [(string? resolved) resolved]
    [(symbol? resolved) (symbol->string resolved)]
    [else resolved]))

;; window-resolve-scroll-options : any/c -> (or/c #f window-scroll-options?)
;;   Treat #f as omitted and force thunks for scroll option values.
(define (window-resolve-scroll-options value)
  (define resolved (window-resolve-optional value))
  (cond
    [(eq? resolved (void)) #f]
    [(eq? resolved #f) #f]
    [else resolved]))

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

;; window-scroll-options->raw : window-scroll-options? real? real? -> external/raw
;;   Convert a Rackety scroll-options struct to a browser dictionary.
(define (window-scroll-options->raw options fallback-x fallback-y)
  (define top (window-scroll-options-top options))
  (define left (window-scroll-options-left options))
  (define behavior (window-scroll-options-behavior options))
  (define fields
    (list (list "top" (if (eq? top #f) fallback-y top))
          (list "left" (if (eq? left #f) fallback-x left))))
  (define fields*
    (if (eq? behavior #f)
        fields
        (append fields
                (list (list "behavior"
                            (if (symbol? behavior)
                                (symbol->string behavior)
                                behavior))))))
  (js-object (list->vector (map list->vector fields*))))

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

;; window-set-name! : (or/c string? symbol?) -> void?
;;   Set the window name.
(define (window-set-name! name)
  (define name* (window-stringish->string 'window-set-name! name))
  (js-set-window-name! name*)
  (void))

;; window-location-info : external/raw -> window-location-info?
;;   Wrap the current window location object.
(struct window-location-info (raw) #:transparent)

;; window-custom-elements-info : external/raw -> window-custom-elements-info?
;;   Wrap the browser CustomElementRegistry object.
(struct window-custom-elements-info (raw) #:transparent)

;; window-history-info : external/raw -> window-history-info?
;;   Wrap the browser History object.
(struct window-history-info (raw) #:transparent)

;; window-visual-viewport-info : external/raw -> window-visual-viewport-info?
;;   Wrap the browser VisualViewport object.
(struct window-visual-viewport-info (raw) #:transparent)

;; window-navigator-info : external/raw -> window-navigator-info?
;;   Wrap the browser Navigator object.
(struct window-navigator-info (raw) #:transparent)

;; window-screen-info : external/raw -> window-screen-info?
;;   Wrap the browser Screen object.
(struct window-screen-info (raw) #:transparent)

;; window-performance-info : external/raw -> window-performance-info?
;;   Wrap the browser Performance object.
(struct window-performance-info (raw) #:transparent)

;; window-local-storage-info : external/raw -> window-local-storage-info?
;;   Wrap the browser Storage object.
(struct window-local-storage-info (raw) #:transparent)

;; window-session-storage-info : external/raw -> window-session-storage-info?
;;   Wrap the browser Storage object.
(struct window-session-storage-info (raw) #:transparent)

;; window-indexed-db-info : external/raw -> window-indexed-db-info?
;;   Wrap the browser IndexedDB factory object.
(struct window-indexed-db-info (raw) #:transparent)

;; window-caches-info : external/raw -> window-caches-info?
;;   Wrap the browser CacheStorage object.
(struct window-caches-info (raw) #:transparent)

;; window-speech-synthesis-info : external/raw -> window-speech-synthesis-info?
;;   Wrap the browser SpeechSynthesis object.
(struct window-speech-synthesis-info (raw) #:transparent)

;; window-style-media-info : external/raw -> window-style-media-info?
;;   Wrap the browser StyleMedia object.
(struct window-style-media-info (raw) #:transparent)

;; window-crypto-info : external/raw -> window-crypto-info?
;;   Wrap the browser Crypto object.
(struct window-crypto-info (raw) #:transparent)

;; window-location : -> window-location-info?
;;   Read the window location object.
(define (window-location)
  (window-location-info (js-window-location)))

;; window-custom-elements : -> window-custom-elements-info?
;;   Read the browser CustomElementRegistry object.
(define (window-custom-elements)
  (window-custom-elements-info (js-window-custom-elements)))

;; window-history : -> window-history-info?
;;   Read the browser History object.
(define (window-history)
  (window-history-info (js-window-history)))

;; window-visual-viewport : -> (or/c #f window-visual-viewport-info?)
;;   Read the browser VisualViewport object when present.
(define (window-visual-viewport)
  (define value (js-window-visual-viewport))
  (and (not (eq? value #f))
       (window-visual-viewport-info value)))

;; window-navigator : -> window-navigator-info?
;;   Read the browser Navigator object.
(define (window-navigator)
  (window-navigator-info (js-window-navigator)))

;; window-screen : -> window-screen-info?
;;   Read the browser Screen object.
(define (window-screen)
  (window-screen-info (js-window-screen)))

;; window-performance : -> window-performance-info?
;;   Read the browser Performance object.
(define (window-performance)
  (window-performance-info (js-window-performance)))

;; window-local-storage : -> window-local-storage-info?
;;   Read the browser localStorage object.
(define (window-local-storage)
  (window-local-storage-info (js-window-local-storage)))

;; window-session-storage : -> window-session-storage-info?
;;   Read the browser sessionStorage object.
(define (window-session-storage)
  (window-session-storage-info (js-window-session-storage)))

;; window-indexed-db : -> window-indexed-db-info?
;;   Read the browser IndexedDB factory object.
(define (window-indexed-db)
  (window-indexed-db-info (js-window-indexed-db)))

;; window-caches : -> window-caches-info?
;;   Read the browser CacheStorage object.
(define (window-caches)
  (window-caches-info (js-window-caches)))

;; window-speech-synthesis : -> window-speech-synthesis-info?
;;   Read the browser SpeechSynthesis object.
(define (window-speech-synthesis)
  (window-speech-synthesis-info (js-window-speech-synthesis)))

;; window-style-media : -> window-style-media-info?
;;   Read the browser StyleMedia object.
(define (window-style-media)
  (window-style-media-info (js-window-style-media)))

;; window-crypto : -> window-crypto-info?
;;   Read the browser Crypto object.
(define (window-crypto)
  (window-crypto-info (js-window-crypto)))

;; window-set-location! : any/c -> void?
;;   Navigate the window to a new URL or Location.
(define (window-set-location! loc)
  (js-set-window-location! loc)
  (void))

;; window-closed? : -> boolean?
;;   Report whether the browsing context is closed.
(define (window-closed?)
  (window-i32->boolean (js-window-closed)))

;; window-length : -> exact-nonnegative-integer?
;;   Read the number of child browsing contexts.
(define (window-length)
  (js-window-length))

;; window-origin : -> string?
;;   Read the origin string for the browsing context.
(define (window-origin)
  (js-window-origin))

;; window-device-pixel-ratio : -> real?
;;   Read the current device pixel ratio.
(define (window-device-pixel-ratio)
  (js-window-device-pixel-ratio))

;; window-inner-height : -> real?
;;   Read the viewport's inner height.
(define (window-inner-height)
  (js-window-inner-height))

;; window-inner-width : -> real?
;;   Read the viewport's inner width.
(define (window-inner-width)
  (js-window-inner-width))

;; window-outer-height : -> real?
;;   Read the window's outer height.
(define (window-outer-height)
  (js-window-outer-height))

;; window-outer-width : -> real?
;;   Read the window's outer width.
(define (window-outer-width)
  (js-window-outer-width))

;; window-screen-x : -> real?
;;   Read the screen X coordinate.
(define (window-screen-x)
  (js-window-screen-x))

;; window-screen-y : -> real?
;;   Read the screen Y coordinate.
(define (window-screen-y)
  (js-window-screen-y))

;; window-screen-left : -> real?
;;   Read the screen's left edge.
(define (window-screen-left)
  (js-window-screen-left))

;; window-screen-top : -> real?
;;   Read the screen's top edge.
(define (window-screen-top)
  (js-window-screen-top))

;; window-page-x-offset : -> real?
;;   Read the horizontal page offset.
(define (window-page-x-offset)
  (js-window-page-x-offset))

;; window-page-y-offset : -> real?
;;   Read the vertical page offset.
(define (window-page-y-offset)
  (js-window-page-y-offset))

;; window-scroll-x : -> real?
;;   Read the horizontal scroll offset.
(define (window-scroll-x)
  (js-window-scroll-x))

;; window-scroll-y : -> real?
;;   Read the vertical scroll offset.
(define (window-scroll-y)
  (js-window-scroll-y))

;; window-is-secure-context? : -> boolean?
;;   Report whether the browsing context is secure.
(define (window-is-secure-context?)
  (window-i32->boolean (js-window-is-secure-context)))

;; window-cross-origin-isolated? : -> boolean?
;;   Report whether the browsing context is cross-origin isolated.
(define (window-cross-origin-isolated?)
  (window-i32->boolean (js-window-cross-origin-isolated)))

;; window-open : (or/c string? symbol?) [target (or/c string? symbol? procedure?) #f] [features (or/c string? symbol? procedure?) #f] [replace (or/c boolean? procedure?) #f] -> (or/c #f window?)
;;   Open a new browsing context.
(define (window-open url [target #f] [features #f] [replace #f])
  (define url* (window-stringish->string 'window-open url))
  (define target* (window-resolve-stringish-optional 'window-open target))
  (define features* (window-resolve-stringish-optional 'window-open features))
  (define replace* (window-resolve-optional replace))
  (when (and (not (eq? target* (void)))
             (not (or (string? target*) (eq? target* #f))))
    (raise-argument-error 'window-open "(or/c #f string? symbol? procedure?)" target))
  (when (and (not (eq? features* (void)))
             (not (or (string? features*) (eq? features* #f))))
    (raise-argument-error 'window-open "(or/c #f string? symbol? procedure?)" features))
  (when (and (not (eq? replace* (void)))
             (not (boolean? replace*)))
    (raise-argument-error 'window-open "(or/c #f boolean? procedure?)" replace))
  (window-wrap-optional (js-window-open url* target* features* replace*)))

;; window-fetch : any/c [any/c #f] -> external/raw
;;   Start a fetch request.
(define (window-fetch request [init #f])
  (js-window-fetch request (window-resolve-optional init)))

;; window-confirm : (or/c string? symbol?) -> boolean?
;;   Show a confirmation dialog.
(define (window-confirm message)
  (define message* (window-stringish->string 'window-confirm message))
  (window-i32->boolean (js-window-confirm message*)))

;; window-prompt : (or/c string? symbol?) [any/c #f] -> any/c
;;   Show a prompt dialog.
(define (window-prompt message [default-value #f])
  (define message* (window-stringish->string 'window-prompt message))
  (js-window-prompt message* (window-resolve-optional default-value)))

;; window-alert : (or/c string? symbol?) -> void?
;;   Show an alert dialog.
(define (window-alert message)
  (define message* (window-stringish->string 'window-alert message))
  (js-window-alert message*)
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

;; window-get-selection : -> (or/c #f selection?)
;;   Read the current selection object.
(define (window-get-selection)
  (selection-wrap (js-window-get-selection)))

;; window-match-media : (or/c string? symbol?) -> media-query-list?
;;   Evaluate a media query.
(define (window-match-media query)
  (define query* (window-stringish->string 'window-match-media query))
  (media-query-list-wrap (js-window-match-media query*)))

;; window-get-computed-style : external? [any/c #f] -> css-style-declaration?
;;   Read computed style for an element.
(define (window-get-computed-style element [pseudo-element #f])
  (define pseudo-element* (window-resolve-stringish-optional 'window-get-computed-style
                                                             pseudo-element))
  (css-style-declaration-wrap
   (js-window-get-computed-style element pseudo-element*)))

;; window-structured-clone : any/c [any/c #f] -> any/c
;;   Clone a value using the structured-clone algorithm.
(define (window-structured-clone value [options #f])
  (js-window-structured-clone value (window-resolve-optional options)))
