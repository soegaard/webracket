#lang scribble/manual

@(require scribble/manual
          (for-label (only-in racket/base struct))
          (for-label (lib "scribblings/lib-window-labels.rkt" "webracket"))
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/lib-document-labels.rkt" "webracket"))
          (for-label (lib "scribblings/lib-iterator-labels.rkt" "webracket")))

@title{Library: @racketid[window]}
@declare-exporting[(lib "scribblings/lib-window-labels.rkt" "webracket")]

@(how-to-require include-lib window (lib "libs/window.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @racket[window] library is the easiest way to talk to the browser
from WebRacket when you need the page itself, not just the current DOM
tree.

If you are new to browser programming, think of the Window as the
browser tab or page container. It gives you access to the current
document, browser dialogs, timers, scrolling, resizing, and a few other
page-level controls.

Use @racket[window] when you want to:

@itemlist[
  @item{find the current document}
  @item{show an alert, confirm, or prompt dialog}
  @item{schedule code to run later}
  @item{scroll or resize the page}
]

The @racket[window] library provides checked wrappers for the browser
Window object, timers, dialogs, scrolling, and navigation helpers.
@margin-note{Checked wrappers are small Racket values that stand in for browser objects and only accept the right kind of raw value at the boundary.}
The current Window object, document, and location are wrapped in small
checked structs so the API stays Rackety instead of exposing raw browser
objects at the top level.

@section{Window Quick Start}

Start by including the library, getting the current document, and
calling a simple page-level helper.

@racketblock[
(code:comment "Include the checked Window wrapper library.")
(include-lib window)

(code:comment "Capture the current Window object as a checked wrapper.")
(define win (Window))

(code:comment "Get the current document from the browser window.")
(define doc (window-document))

(code:comment "Show a simple confirmation dialog.")
(define ok? (window-confirm "Continue with this action?"))

(code:comment "If the user agrees, scroll the page a little.")
(when ok?
  (window-scroll-to 0 240))
]

The quick start shows the three most common ideas:
include the wrapper, capture the current Window object, and call a
browser helper that affects the page. After this block, @racket[win] is
the wrapped current Window object and @racket[doc] is the current
document.

@section{Window Example}

This example shows how to use the current document, schedule work to
happen a little later, and log a message to the browser console. The
timer is useful when you want to defer a page update without blocking
the current event handler.

@racketblock[
(code:comment "Include the wrapper library at the top level.")
(include-lib window)
(include-lib console)

(code:comment "Capture the current browser Window object.")
(define win (Window))

(code:comment "Get the current document so we can inspect the page.")
(define doc (window-document))

(code:comment "Keep the wrapped document value around for later DOM work.")

(code:comment "Run a small update after a short delay.")
(window-set-timeout
  (lambda ()
    (console-log "Hello from the browser window")))
]

If you are only reading the page and not changing it, the most useful
window entry points are usually @racket[Window], @racket[window-document],
@racket[window-confirm], @racket[window-set-timeout], and
@racket[window-scroll-to].

For optional arguments, @racket[#f] means omitted. If you need a
literal @racket[#f] value, pass a thunk such as @racket[(lambda () #f)].

@section{Window API}

@defproc[(window-document) window-document-info?]{
@(mdn-bar "Window: document property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/document")
Returns the current document object wrapped in a checked struct.
}

@defproc[(window-location) window-location-info?]{
@(mdn-bar "Window: location property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/location")
Returns the current location object wrapped in a checked struct.
}

@defproc[(window-get-selection) (or/c #f dom-selection?)]{
@(mdn-bar "Window: getSelection() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/getSelection")
Returns the current selection as a wrapped @racket[dom-selection] value.
}

@section{Window Values}

@defstruct[dom-window ([raw external/raw])]{
Wraps a browser Window object.
}

@defproc[(Window) dom-window?]{
@(mdn-bar "Window"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window")
Returns the current browser Window object wrapped in a checked struct.
}

@defproc[(window-self) dom-window?]{
@(mdn-bar "Window: self property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/self")
Returns the current browser Window object via @racket[self], wrapped in
the checked struct.
}

@defstruct[window-document-info ([raw external/raw])]{
Wraps the current document object returned by @racket[window-document].
}

@defstruct[window-location-info ([raw external/raw])]{
Wraps the current location object returned by @racket[window-location].
}

@defstruct[window-scroll-options ([top (or/c #f real?)]
                                  [left (or/c #f real?)]
                                  [behavior (or/c #f string? symbol?)])]{
Describes the optional @tt{ScrollToOptions} dictionary used by
@racket[window-scroll-to], @racket[window-scroll-by], and
@racket[window-scroll]. Leave a field as @racket[#f] to omit it from the
browser dictionary. If @racket[behavior] is present, use one of
@racket['auto], @racket['instant], or @racket['smooth], or the matching
string values. Construct values with @racket[make-window-scroll-options].
}

@section{Window Browser Objects}

@defstruct[window-custom-elements-info ([raw external/raw])]{
Wraps the browser @tt{CustomElementRegistry} object.
}

@defproc[(window-custom-elements) window-custom-elements-info?]{
Returns the browser custom-elements registry wrapped in a checked struct.
}

@defstruct[window-history-info ([raw external/raw])]{
Wraps the browser @tt{History} object.
}

@defproc[(window-history) window-history-info?]{
Returns the browser history object wrapped in a checked struct.
}

@defstruct[window-visual-viewport-info ([raw external/raw])]{
Wraps the browser @tt{VisualViewport} object.
}

@defproc[(window-visual-viewport) (or/c #f window-visual-viewport-info?)]{
Returns the browser visual viewport when present.
}

@defstruct[window-navigator-info ([raw external/raw])]{
Wraps the browser @tt{Navigator} object.
}

@defproc[(window-navigator) window-navigator-info?]{
Returns the browser navigator wrapped in a checked struct.
}

@defstruct[window-screen-info ([raw external/raw])]{
Wraps the browser @tt{Screen} object.
}

@defproc[(window-screen) window-screen-info?]{
Returns the browser screen wrapped in a checked struct.
}

@defstruct[window-performance-info ([raw external/raw])]{
Wraps the browser @tt{Performance} object.
}

@defproc[(window-performance) window-performance-info?]{
Returns the browser performance object wrapped in a checked struct.
}

@defstruct[window-local-storage-info ([raw external/raw])]{
Wraps the browser @tt{Storage} object from @racket[localStorage].
}

@defproc[(window-local-storage) window-local-storage-info?]{
Returns the browser @tt{localStorage} object wrapped in a checked struct.
}

@defstruct[window-session-storage-info ([raw external/raw])]{
Wraps the browser @tt{Storage} object from @racket[sessionStorage].
}

@defproc[(window-session-storage) window-session-storage-info?]{
Returns the browser @tt{sessionStorage} object wrapped in a checked struct.
}

@defstruct[window-indexed-db-info ([raw external/raw])]{
Wraps the browser @tt{indexedDB} factory object.
}

@defproc[(window-indexed-db) window-indexed-db-info?]{
Returns the browser indexedDB factory wrapped in a checked struct.
}

@defstruct[window-caches-info ([raw external/raw])]{
Wraps the browser @tt{CacheStorage} object.
}

@defproc[(window-caches) window-caches-info?]{
Returns the browser cache storage object wrapped in a checked struct.
}

@defstruct[window-speech-synthesis-info ([raw external/raw])]{
Wraps the browser @tt{SpeechSynthesis} object.
}

@defproc[(window-speech-synthesis) window-speech-synthesis-info?]{
Returns the browser speech-synthesis object wrapped in a checked struct.
}

@defstruct[window-style-media-info ([raw external/raw])]{
Wraps the browser @tt{StyleMedia} object.
}

@defproc[(window-style-media) window-style-media-info?]{
Returns the browser style-media object wrapped in a checked struct.
}

@defstruct[window-crypto-info ([raw external/raw])]{
Wraps the browser @tt{Crypto} object.
}

@defproc[(window-crypto) window-crypto-info?]{
Returns the browser crypto object wrapped in a checked struct.
}

@section{Window Properties}

@defproc[(window-closed?) boolean?]{
Returns @racket[#t] when the browsing context is closed.
}

@defproc[(window-length) exact-nonnegative-integer?]{
Returns the number of child browsing contexts.
}

@defproc[(window-origin) string?]{
Returns the browsing-context origin string.
}

@defproc[(window-device-pixel-ratio) real?]{
Returns the current device pixel ratio.
}

@defproc[(window-inner-height) real?]{
Returns the inner viewport height.
}

@defproc[(window-inner-width) real?]{
Returns the inner viewport width.
}

@defproc[(window-outer-height) real?]{
Returns the outer window height.
}

@defproc[(window-outer-width) real?]{
Returns the outer window width.
}

@defproc[(window-screen-x) real?]{
Returns the screen x coordinate.
}

@defproc[(window-screen-y) real?]{
Returns the screen y coordinate.
}

@defproc[(window-screen-left) real?]{
Returns the screen's left edge.
}

@defproc[(window-screen-top) real?]{
Returns the screen's top edge.
}

@defproc[(window-page-x-offset) real?]{
Returns the horizontal page offset.
}

@defproc[(window-page-y-offset) real?]{
Returns the vertical page offset.
}

@defproc[(window-scroll-x) real?]{
Returns the horizontal scroll offset.
}

@defproc[(window-scroll-y) real?]{
Returns the vertical scroll offset.
}

@defproc[(window-is-secure-context?) boolean?]{
Returns @racket[#t] when the browsing context is secure.
}

@defproc[(window-cross-origin-isolated?) boolean?]{
Returns @racket[#t] when the browsing context is cross-origin isolated.
}

@section{Window Style Values}

@defstruct[media-query-list ([raw external/raw])]{
Wraps a browser @tt{MediaQueryList} object.
}

@defproc[(window-match-media [query (or/c string? symbol?)]) media-query-list?]{
@(mdn-bar "Window: matchMedia() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/matchMedia")
Evaluates a media query and returns a wrapped @racket[media-query-list]
value.
}

@defproc[(media-query-list-media [query media-query-list?]) string?]{
Returns the media query string that @racket[query] wraps.
}

@defproc[(media-query-list-matches? [query media-query-list?]) boolean?]{
Returns @racket[#t] when the wrapped query currently matches.
}

@defstruct[css-style-declaration ([raw external/raw])]{
Wraps a browser @tt{CSSStyleDeclaration} object.
}

@defproc[(window-get-computed-style [element element?]
                                    [pseudo-element (or/c #f string? symbol?) #f])
         css-style-declaration?]{
@(mdn-bar "Window: getComputedStyle() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/getComputedStyle")
Returns the computed style for @racket[element] as a wrapped
@racket[css-style-declaration] value. The pseudo-element argument
accepts a string or symbol and is omitted when @racket[#f].
}

@defproc[(css-style-declaration-css-text [style css-style-declaration?]) string?]{
Returns the serialized CSS text for the wrapped style declaration.
}

@defproc[(css-style-declaration-set-css-text! [style css-style-declaration?]
                                              [value (or/c string? symbol?)]) void?]{
Sets the serialized CSS text for the wrapped style declaration.
}

@defproc[(css-style-declaration-length [style css-style-declaration?])
         exact-nonnegative-integer?]{
Returns the number of declared properties.
}

@defproc[(css-style-declaration-item [style css-style-declaration?]
                                     [index exact-nonnegative-integer?])
         (or/c #f string?)]{
Returns the property name at @racket[index] or @racket[#f] when the
index is out of range.
}

@defproc[(css-style-declaration-get-property-value [style css-style-declaration?]
                                                   [property (or/c string? symbol?)])
         string?]{
Returns the value for @racket[property].
}

@defproc[(css-style-declaration-set-property! [style css-style-declaration?]
                                              [property (or/c string? symbol?)]
                                              [value (or/c string? symbol?)])
         void?]{
Sets the value for @racket[property].
}

@defproc[(css-style-declaration-remove-property! [style css-style-declaration?]
                                                 [property (or/c string? symbol?)])
         void?]{
Removes @racket[property] from the wrapped style declaration.
}

@defproc[(window-set-name! [name (or/c string? symbol?)]) void?]{
@(mdn-bar "Window: name property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/name")
Sets the browsing context name used by the browser for targeting,
reusing windows, and script access. This does not visibly change the
page itself. Symbols are accepted and converted to strings.
}

@defproc[(window-open [url (or/c string? symbol?)]
                      [target (or/c string? symbol? procedure?) #f]
                      [features (or/c string? symbol? procedure?) #f]
                      [replace (or/c boolean? procedure?) #f])
         (or/c #f dom-window?)]{
@(mdn-bar "Window: open() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/open")
Opens a new browsing context. The @racket[url], @racket[target], and
@racket[features] arguments accept strings or symbols, and
@racket[replace] accepts a boolean value.
Each optional argument may also be a thunk that takes no arguments and
returns the matching value: @racket[target] and @racket[features] must
produce a string or symbol, while @racket[replace] must produce a
boolean.
The returned value is wrapped as @racket[dom-window] when the browser opens
the new context, or @racket[#f] if the popup is blocked. Use @racket[#f]
to omit an optional argument. If you need a literal @racket[#f] value,
pass a thunk such as @racket[(lambda () #f)].
}

@defproc[(window-confirm [message (or/c string? symbol?)]) boolean?]{
@(mdn-bar "Window: confirm() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/confirm")
Shows a confirmation dialog and converts the browser result to a boolean.
The message accepts a string or symbol and is normalized to a string.
}

@defproc[(window-scroll-to [x real?] [y real?]
                           [options (or/c #f window-scroll-options? procedure?) #f])
         void?]{
@(mdn-bar "Window: scrollTo() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/scrollTo")
Scrolls the window to an absolute position. The optional @racket[options]
argument can supply a @racket[window-scroll-options] struct to use the browser's
dictionary form. It may also be a thunk that takes no arguments and
returns either @racket[#f] or a @racket[window-scroll-options] value.
Use @racket[#f] to omit the options argument.
}

@defproc[(window-scroll-by [x real?] [y real?]
                           [options (or/c #f window-scroll-options? procedure?) #f])
         void?]{
@(mdn-bar "Window: scrollBy() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/scrollBy")
Scrolls the window by a relative offset. The optional @racket[options]
argument can supply a @racket[window-scroll-options] struct to use the
browser's dictionary form. It may also be a thunk that takes no
arguments and returns either @racket[#f] or a @racket[window-scroll-options]
value. Use @racket[#f] to omit the options argument.
}

@defproc[(window-scroll [x real?] [y real?]
                        [options (or/c #f window-scroll-options? procedure?) #f])
         void?]{
@(mdn-bar "Window: scroll() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/scroll")
Scrolls the window to an absolute position. The optional @racket[options]
argument can supply a @racket[window-scroll-options] struct to use the
browser's dictionary form. It may also be a thunk that takes no
arguments and returns either @racket[#f] or a @racket[window-scroll-options]
value. Use @racket[#f] to omit the options argument.
}

@defproc[(window-set-timeout [callback external?]) exact-nonnegative-integer?]{
@(mdn-bar "Window: setTimeout() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/setTimeout")
The raw @racket[callback] argument should be a browser JavaScript
function value.

Schedules a one-shot timer callback.
}
