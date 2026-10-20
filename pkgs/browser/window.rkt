#lang racket/base

(require browser/element
         webracket/unsafe/window
         (only-in webracket/unsafe/standard
                  js-object
                  js-ref
                  js-ref/extern
                  js-send/value
                  js-typeof)
         (only-in webracket/primitives
                  external?
                  procedure->external))

(provide (except-out (struct-out dom-window) dom-window?)
         (except-out (struct-out window-document-info) window-document-info?)
         (except-out (struct-out window-location-info) window-location-info?)
         (except-out (struct-out window-custom-elements-info) window-custom-elements-info?)
         (except-out (struct-out window-history-info) window-history-info?)
         (except-out (struct-out window-visual-viewport-info) window-visual-viewport-info?)
         (except-out (struct-out window-navigator-info) window-navigator-info?)
         (except-out (struct-out window-screen-info) window-screen-info?)
         (except-out (struct-out window-performance-info) window-performance-info?)
         (except-out (struct-out window-local-storage-info) window-local-storage-info?)
         (except-out (struct-out window-session-storage-info) window-session-storage-info?)
         (except-out (struct-out window-indexed-db-info) window-indexed-db-info?)
         (except-out (struct-out window-caches-info) window-caches-info?)
         (except-out (struct-out window-speech-synthesis-info) window-speech-synthesis-info?)
         (except-out (struct-out window-style-media-info) window-style-media-info?)
         (except-out (struct-out window-crypto-info) window-crypto-info?)
         (rename-out [dom-window/surface? dom-window?]
                     [window-document-info/surface? window-document-info?]
                     [window-location-info/surface? window-location-info?]
                     [window-custom-elements-info/surface? window-custom-elements-info?]
                     [window-history-info/surface? window-history-info?]
                     [window-visual-viewport-info/surface? window-visual-viewport-info?]
                     [window-navigator-info/surface? window-navigator-info?]
                     [window-screen-info/surface? window-screen-info?]
                     [window-performance-info/surface? window-performance-info?]
                     [window-local-storage-info/surface? window-local-storage-info?]
                     [window-session-storage-info/surface? window-session-storage-info?]
                     [window-indexed-db-info/surface? window-indexed-db-info?]
                     [window-caches-info/surface? window-caches-info?]
                     [window-speech-synthesis-info/surface? window-speech-synthesis-info?]
                     [window-style-media-info/surface? window-style-media-info?]
                     [window-crypto-info/surface? window-crypto-info?])
         (struct-out window-scroll-options)
         Window
         window-self
         window-document
         window-name
         window-set-name!
         window-location
         window-custom-elements
         window-history
         window-visual-viewport
         window-visual-viewport-width
         window-visual-viewport-height
         window-visual-viewport-offset-left
         window-visual-viewport-offset-top
         window-visual-viewport-scale
         window-navigator
         window-screen
         window-performance
         window-local-storage
         window-session-storage
         window-indexed-db
         window-caches
         window-speech-synthesis
         window-style-media
         window-crypto
         window-random-uuid
         window-set-location!
         window-closed?
         window-length
         window-origin
         window-device-pixel-ratio
         window-inner-height
         window-inner-width
         window-outer-height
         window-outer-width
         window-screen-x
         window-screen-y
         window-screen-left
         window-screen-top
         window-page-x-offset
         window-page-y-offset
         window-scroll-x
         window-scroll-y
         window-is-secure-context?
         window-cross-origin-isolated?
         window-open
         window-fetch
         window-confirm
         window-prompt
         window-alert
         window-print
         window-focus
         window-stop
         window-scroll-to
         window-scroll-by
         window-scroll
         window-resize-to
         window-resize-by
         window-move-to
         window-move-by
         window-set-timeout
         window-set-timeout/delay
         window-clear-timeout
         window-set-interval
         window-clear-interval
         window-request-animation-frame
         window-cancel-animation-frame
         window-queue-microtask
         window-request-idle-callback
         window-cancel-idle-callback
         window-get-selection
         window-match-media
         window-get-computed-style
         window-structured-clone)

;; This is the canonical module version of the old `include-lib window`
;; safe wrapper. Raw bindings remain in `webracket/unsafe/window`.
;; Require `webracket/unsafe/window` directly for raw FFI access.

;;;
;;; Window wrappers
;;;

;; window-i32->boolean : integer? -> boolean?
;;   Convert a browser i32 flag to a boolean.
(define (window-i32->boolean v)
  (not (zero? v)))

;; window-real->flonum : symbol? any/c -> real?
;;   Validate a real coordinate and coerce it to an inexact value for FFI calls.
(define (window-real->flonum who v)
  (unless (real? v)
    (raise-argument-error who "real?" v))
  (exact->inexact v))

;; window-callback->external : symbol? (or/c procedure? external?) -> external?
;;   Convert a callback without retaining it after its native owner releases it.
(define (window-callback->external who callback)
  (cond
    [(external? callback) callback]
    [(procedure? callback)
     (procedure->external
      (lambda args
        (apply callback args)
        (void)))]
    [else
     (raise-argument-error who "(or/c procedure? external?)" callback)]))

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
  (cond
    [(window-scroll-options? value) value]
    [else
     (define resolved (window-resolve-optional value))
     (cond
       [(eq? resolved (void)) #f]
       [(eq? resolved #f) #f]
       [(window-scroll-options? resolved) resolved]
       [else resolved])]))

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

;; dom-window : external/raw -> dom-window?
;;   Wrap a browser Window object.
(struct dom-window (raw) #:transparent)

;; Window : -> dom-window?
;;   Read the current window object.
(define (Window)
  (dom-window (js-window-window)))

;; window-wrap-optional : any/c -> (or/c #f dom-window?)
;;   Wrap a browser Window-like value when one is present.
(define (window-wrap-optional value)
  (and (not (eq? value #f))
       (dom-window value)))

;; window-document-info : external/raw -> window-document-info?
;;   Wrap the current document object.
(struct window-document-info (raw) #:transparent)

;; window-self : -> dom-window?
;;   Read the current window via self.
(define (window-self)
  (dom-window (js-window-self)))

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

;; window-host-surface? : external/raw (listof string?) (listof string?) -> boolean?
;;   Check whether a window property result exposes a stable host surface.
(define (window-host-surface? raw property-names method-names)
  (shared-dom-host-surface-present? raw property-names method-names))

;; window-result-wrap/checked : symbol? string? string? any/c procedure? procedure? -> any/c
;;   Wrap a window property result and reject malformed host objects early.
(define (window-result-wrap/checked who display-name expected raw make-wrapper surface?)
  (define wrapped (make-wrapper raw))
  (unless (surface? wrapped)
    (raise-arguments-error who
                           (string-append display-name " result must be " expected)))
  wrapped)

(define window-custom-elements-methods
  '("define" "get" "whenDefined" "upgrade"))

(define window-history-methods
  '("back" "forward" "go" "pushState" "replaceState"))

(define window-screen-properties
  '("width" "height" "availWidth" "availHeight" "colorDepth" "pixelDepth"))

(define window-performance-properties
  '("timeOrigin"))

(define window-performance-methods
  '("now" "mark" "measure" "clearMarks" "clearMeasures" "getEntries" "getEntriesByName" "getEntriesByType"))

(define window-storage-methods
  '("key" "getItem" "setItem" "removeItem" "clear"))

(define window-indexed-db-methods
  '("open" "deleteDatabase" "cmp"))

(define window-caches-methods
  '("open" "has" "delete" "match" "keys"))

(define window-speech-synthesis-properties
  '("speaking" "paused" "pending"))

(define window-speech-synthesis-methods
  '("speak" "cancel" "pause" "resume" "getVoices"))

(define window-crypto-methods
  '("getRandomValues"))

;; dom-window/surface? : any/c -> boolean?
;;   Check whether value is a Window wrapper with a usable host value.
(define (dom-window/surface? value)
  (and (dom-window? value)
       (shared-dom-raw-host-value? (dom-window-raw value))))

;; window-document-info/surface? : any/c -> boolean?
;;   Check whether value is a Document wrapper with a usable host value.
(define (window-document-info/surface? value)
  (and (window-document-info? value)
       (shared-dom-raw-host-value? (window-document-info-raw value))))

;; window-location-info/surface? : any/c -> boolean?
;;   Check whether value is a Location wrapper with a usable host value.
(define (window-location-info/surface? value)
  (and (window-location-info? value)
       (shared-dom-raw-host-value? (window-location-info-raw value))))

;; window-custom-elements-info/surface? : any/c -> boolean?
;;   Check whether value is a CustomElementRegistry wrapper with a usable host value.
(define (window-custom-elements-info/surface? value)
  (and (window-custom-elements-info? value)
       (window-host-surface? (window-custom-elements-info-raw value)
                             '()
                             window-custom-elements-methods)))

;; window-history-info/surface? : any/c -> boolean?
;;   Check whether value is a History wrapper with a usable host value.
(define (window-history-info/surface? value)
  (and (window-history-info? value)
       (window-host-surface? (window-history-info-raw value)
                             '()
                             window-history-methods)))

;; window-visual-viewport-info/surface? : any/c -> boolean?
;;   Check whether value is a VisualViewport wrapper with a usable host value.
(define (window-visual-viewport-info/surface? value)
  (and (window-visual-viewport-info? value)
       (shared-dom-raw-host-value? (window-visual-viewport-info-raw value))))

;; window-navigator-info/surface? : any/c -> boolean?
;;   Check whether value is a Navigator wrapper with a usable host value.
(define (window-navigator-info/surface? value)
  (and (window-navigator-info? value)
       (shared-dom-raw-host-value? (window-navigator-info-raw value))))

;; window-screen-info/surface? : any/c -> boolean?
;;   Check whether value is a Screen wrapper with a usable host value.
(define (window-screen-info/surface? value)
  (and (window-screen-info? value)
       (window-host-surface? (window-screen-info-raw value)
                             window-screen-properties
                             '())))

;; window-performance-info/surface? : any/c -> boolean?
;;   Check whether value is a Performance wrapper with a usable host value.
(define (window-performance-info/surface? value)
  (and (window-performance-info? value)
       (window-host-surface? (window-performance-info-raw value)
                             window-performance-properties
                             window-performance-methods)))

;; window-local-storage-info/surface? : any/c -> boolean?
;;   Check whether value is a localStorage wrapper with a usable host value.
(define (window-local-storage-info/surface? value)
  (and (window-local-storage-info? value)
       (window-host-surface? (window-local-storage-info-raw value)
                             '()
                             window-storage-methods)))

;; window-session-storage-info/surface? : any/c -> boolean?
;;   Check whether value is a sessionStorage wrapper with a usable host value.
(define (window-session-storage-info/surface? value)
  (and (window-session-storage-info? value)
       (window-host-surface? (window-session-storage-info-raw value)
                             '()
                             window-storage-methods)))

;; window-indexed-db-info/surface? : any/c -> boolean?
;;   Check whether value is an IndexedDB wrapper with a usable host value.
(define (window-indexed-db-info/surface? value)
  (and (window-indexed-db-info? value)
       (window-host-surface? (window-indexed-db-info-raw value)
                             '()
                             window-indexed-db-methods)))

;; window-caches-info/surface? : any/c -> boolean?
;;   Check whether value is a CacheStorage wrapper with a usable host value.
(define (window-caches-info/surface? value)
  (and (window-caches-info? value)
       (window-host-surface? (window-caches-info-raw value)
                             '()
                             window-caches-methods)))

;; window-speech-synthesis-info/surface? : any/c -> boolean?
;;   Check whether value is a SpeechSynthesis wrapper with a usable host value.
(define (window-speech-synthesis-info/surface? value)
  (and (window-speech-synthesis-info? value)
       (window-host-surface? (window-speech-synthesis-info-raw value)
                             window-speech-synthesis-properties
                             window-speech-synthesis-methods)))

;; window-style-media-info/surface? : any/c -> boolean?
;;   Check whether value is a StyleMedia wrapper with a usable host value.
(define (window-style-media-info/surface? value)
  (and (window-style-media-info? value)
       (shared-dom-raw-host-value? (window-style-media-info-raw value))))

;; window-crypto-info/surface? : any/c -> boolean?
;;   Check whether value is a Crypto wrapper with a usable host value.
(define (window-crypto-info/surface? value)
  (and (window-crypto-info? value)
       (window-host-surface? (window-crypto-info-raw value)
                             '()
                             window-crypto-methods)))

;; window-location : -> window-location-info?
;;   Read the window location object.
(define (window-location)
  (window-location-info (js-window-location)))

;; window-custom-elements : -> window-custom-elements-info?
;;   Read the browser CustomElementRegistry object.
(define (window-custom-elements)
  (window-result-wrap/checked 'window-custom-elements
                              "window.customElements"
                              "CustomElementRegistry"
                              (js-window-custom-elements)
                              window-custom-elements-info
                              window-custom-elements-info/surface?))

;; window-history : -> window-history-info?
;;   Read the browser History object.
(define (window-history)
  (window-result-wrap/checked 'window-history
                              "window.history"
                              "History"
                              (js-window-history)
                              window-history-info
                              window-history-info/surface?))

;; window-visual-viewport : -> (or/c #f window-visual-viewport-info?)
;;   Read the browser VisualViewport object when present.
(define (window-visual-viewport)
  (define value (js-window-visual-viewport))
  (and (not (eq? value #f))
       (window-visual-viewport-info value)))

;; window-visual-viewport-number : symbol? window-visual-viewport-info? string? -> real?
;;   Read one finite numeric VisualViewport property.
(define (window-visual-viewport-number who viewport property-name)
  (unless (window-visual-viewport-info/surface? viewport)
    (raise-argument-error who "window-visual-viewport-info?" viewport))
  (define value (js-ref (window-visual-viewport-info-raw viewport) property-name))
  (unless (real? value)
    (raise-arguments-error who
                           (string-append "VisualViewport." property-name
                                          " did not return a number")))
  value)

;; window-visual-viewport-width : window-visual-viewport-info? -> real?
;;   Read the visual viewport width in CSS pixels.
(define (window-visual-viewport-width viewport)
  (window-visual-viewport-number 'window-visual-viewport-width viewport "width"))

;; window-visual-viewport-height : window-visual-viewport-info? -> real?
;;   Read the visual viewport height in CSS pixels.
(define (window-visual-viewport-height viewport)
  (window-visual-viewport-number 'window-visual-viewport-height viewport "height"))

;; window-visual-viewport-offset-left : window-visual-viewport-info? -> real?
;;   Read the visual viewport horizontal layout offset in CSS pixels.
(define (window-visual-viewport-offset-left viewport)
  (window-visual-viewport-number 'window-visual-viewport-offset-left viewport "offsetLeft"))

;; window-visual-viewport-offset-top : window-visual-viewport-info? -> real?
;;   Read the visual viewport vertical layout offset in CSS pixels.
(define (window-visual-viewport-offset-top viewport)
  (window-visual-viewport-number 'window-visual-viewport-offset-top viewport "offsetTop"))

;; window-visual-viewport-scale : window-visual-viewport-info? -> real?
;;   Read the current visual viewport scale factor.
(define (window-visual-viewport-scale viewport)
  (window-visual-viewport-number 'window-visual-viewport-scale viewport "scale"))

;; window-navigator : -> window-navigator-info?
;;   Read the browser Navigator object.
(define (window-navigator)
  (window-navigator-info (js-window-navigator)))

;; window-screen : -> window-screen-info?
;;   Read the browser Screen object.
(define (window-screen)
  (window-result-wrap/checked 'window-screen
                              "window.screen"
                              "Screen"
                              (js-window-screen)
                              window-screen-info
                              window-screen-info/surface?))

;; window-performance : -> window-performance-info?
;;   Read the browser Performance object.
(define (window-performance)
  (window-result-wrap/checked 'window-performance
                              "window.performance"
                              "Performance"
                              (js-window-performance)
                              window-performance-info
                              window-performance-info/surface?))

;; window-local-storage : -> window-local-storage-info?
;;   Read the browser localStorage object.
(define (window-local-storage)
  (window-result-wrap/checked 'window-local-storage
                              "window.localStorage"
                              "Storage"
                              (js-window-local-storage)
                              window-local-storage-info
                              window-local-storage-info/surface?))

;; window-session-storage : -> window-session-storage-info?
;;   Read the browser sessionStorage object.
(define (window-session-storage)
  (window-result-wrap/checked 'window-session-storage
                              "window.sessionStorage"
                              "Storage"
                              (js-window-session-storage)
                              window-session-storage-info
                              window-session-storage-info/surface?))

;; window-indexed-db : -> window-indexed-db-info?
;;   Read the browser IndexedDB factory object.
(define (window-indexed-db)
  (window-result-wrap/checked 'window-indexed-db
                              "window.indexedDB"
                              "IDBFactory"
                              (js-window-indexed-db)
                              window-indexed-db-info
                              window-indexed-db-info/surface?))

;; window-caches : -> window-caches-info?
;;   Read the browser CacheStorage object.
(define (window-caches)
  (window-result-wrap/checked 'window-caches
                              "window.caches"
                              "CacheStorage"
                              (js-window-caches)
                              window-caches-info
                              window-caches-info/surface?))

;; window-speech-synthesis : -> window-speech-synthesis-info?
;;   Read the browser SpeechSynthesis object.
(define (window-speech-synthesis)
  (window-result-wrap/checked 'window-speech-synthesis
                              "window.speechSynthesis"
                              "SpeechSynthesis"
                              (js-window-speech-synthesis)
                              window-speech-synthesis-info
                              window-speech-synthesis-info/surface?))

;; window-style-media : -> window-style-media-info?
;;   Read the browser StyleMedia object.
(define (window-style-media)
  (window-style-media-info (js-window-style-media)))

;; window-crypto : -> window-crypto-info?
;;   Read the browser Crypto object.
(define (window-crypto)
  (window-result-wrap/checked 'window-crypto
                              "window.crypto"
                              "Crypto"
                              (js-window-crypto)
                              window-crypto-info
                              window-crypto-info/surface?))

;; window-random-uuid : -> string?
;;   Generate a native, cryptographically strong UUID when supported.
(define (window-random-uuid)
  (define crypto (window-crypto))
  (define raw (window-crypto-info-raw crypto))
  (define method (js-ref/extern raw "randomUUID"))
  (unless (and (external? method)
               (string=? (js-typeof method) "function"))
    (raise-arguments-error 'window-random-uuid
                           "Crypto.randomUUID is not available"))
  (define value (js-send/value raw "randomUUID" (vector)))
  (unless (string? value)
    (raise-arguments-error 'window-random-uuid
                           "Crypto.randomUUID did not return a string"))
  value)

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
  (define x* (window-real->flonum 'window-scroll-to x))
  (define y* (window-real->flonum 'window-scroll-to y))
  (define options* (window-resolve-scroll-options options))
  (cond
    [(eq? options* #f)
     (js-window-scroll-to x* y* (void))]
    [(window-scroll-options? options*)
     (js-window-scroll-to x* y* (window-scroll-options->raw options* x* y*))]
    [else
     (raise-argument-error 'window-scroll-to
                           "(or/c #f window-scroll-options? procedure?)"
                           options)])
  (void))

;; window-scroll-by : real? real? [or/c #f window-scroll-options? procedure?] -> void?
;;   Scroll the window by a relative offset.
(define (window-scroll-by x y [options #f])
  (define x* (window-real->flonum 'window-scroll-by x))
  (define y* (window-real->flonum 'window-scroll-by y))
  (define options* (window-resolve-scroll-options options))
  (cond
    [(eq? options* #f)
     (js-window-scroll-by x* y* (void))]
    [(window-scroll-options? options*)
     (js-window-scroll-by x* y* (window-scroll-options->raw options* x* y*))]
    [else
     (raise-argument-error 'window-scroll-by
                           "(or/c #f window-scroll-options? procedure?)"
                           options)])
  (void))

;; window-scroll : real? real? [or/c #f window-scroll-options? procedure?] -> void?
;;   Scroll the document to an absolute position.
(define (window-scroll x y [options #f])
  (define x* (window-real->flonum 'window-scroll x))
  (define y* (window-real->flonum 'window-scroll y))
  (define options* (window-resolve-scroll-options options))
  (cond
    [(eq? options* #f)
     (js-window-scroll x* y* (void))]
    [(window-scroll-options? options*)
     (js-window-scroll x* y* (window-scroll-options->raw options* x* y*))]
    [else
     (raise-argument-error 'window-scroll
                           "(or/c #f window-scroll-options? procedure?)"
                           options)])
  (void))

;; window-resize-to : real? real? -> void?
;;   Resize the window to a given size.
(define (window-resize-to w h)
  (js-window-resize-to (window-real->flonum 'window-resize-to w)
                       (window-real->flonum 'window-resize-to h))
  (void))

;; window-resize-by : real? real? -> void?
;;   Resize the window by a delta.
(define (window-resize-by w h)
  (js-window-resize-by (window-real->flonum 'window-resize-by w)
                       (window-real->flonum 'window-resize-by h))
  (void))

;; window-move-to : real? real? -> void?
;;   Move the window to a given position.
(define (window-move-to x y)
  (js-window-move-to (window-real->flonum 'window-move-to x)
                     (window-real->flonum 'window-move-to y))
  (void))

;; window-move-by : real? real? -> void?
;;   Move the window by a given delta.
(define (window-move-by x y)
  (js-window-move-by (window-real->flonum 'window-move-by x)
                     (window-real->flonum 'window-move-by y))
  (void))

;; window-set-timeout : (or/c procedure? external?) -> u32?
;;   Schedule a one-shot timer callback.
(define (window-set-timeout callback)
  (js-window-set-timeout
   (window-callback->external 'window-set-timeout callback)))

;; window-set-timeout/delay : (or/c procedure? external?) real? -> u32?
;;   Schedule a one-shot timer callback with an explicit delay.
(define (window-set-timeout/delay callback delay)
  (js-window-set-timeout/delay
   (window-callback->external 'window-set-timeout/delay callback)
   (window-real->flonum 'window-set-timeout/delay delay)))

;; window-clear-timeout : exact-integer? -> void?
;;   Cancel a timeout handle.
(define (window-clear-timeout id)
  (js-window-clear-timeout id)
  (void))

;; window-set-interval : (or/c procedure? external?) real? -> u32?
;;   Schedule a repeated timer callback.
(define (window-set-interval callback delay)
  (js-window-set-interval
   (window-callback->external 'window-set-interval callback)
   (window-real->flonum 'window-set-interval delay)))

;; window-clear-interval : exact-integer? -> void?
;;   Cancel an interval handle.
(define (window-clear-interval id)
  (js-window-clear-interval id)
  (void))

;; window-request-animation-frame : (or/c procedure? external?) -> u32?
;;   Schedule a repaint callback.
(define (window-request-animation-frame callback)
  (js-window-request-animation-frame
   (window-callback->external 'window-request-animation-frame callback)))

;; window-cancel-animation-frame : exact-integer? -> void?
;;   Cancel a repaint callback.
(define (window-cancel-animation-frame id)
  (js-window-cancel-animation-frame id)
  (void))

;; window-queue-microtask : (or/c procedure? external?) -> void?
;;   Schedule callback at the browser microtask checkpoint.
(define (window-queue-microtask callback)
  (js-window-queue-microtask
   (window-callback->external 'window-queue-microtask callback))
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

;; window-get-selection : -> (or/c #f dom-selection?)
;;   Read the current selection object.
(define (window-get-selection)
  (dom-selection-wrap (js-window-get-selection)))

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
   (js-window-get-computed-style
    (element-raw/checked 'window-get-computed-style element)
    pseudo-element*)))

;; window-structured-clone : any/c [any/c #f] -> any/c
;;   Clone a value using the structured-clone algorithm.
(define (window-structured-clone value [options #f])
  (js-window-structured-clone value (window-resolve-optional options)))
