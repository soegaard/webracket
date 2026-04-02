#lang scribble/manual

@(require scribble/manual
          (for-label (only-in racket/base struct))
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/lib-document-labels.rkt" "webracket"))
          (for-label (lib "scribblings/lib-iterator-labels.rkt" "webracket"))
          (for-label (lib "scribblings/lib-window-labels.rkt" "webracket")))

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
The current Window object, document, and location are wrapped in small
checked structs so the API stays Rackety instead of exposing raw browser
objects at the top level. The raw accessor for @racket[window] lives in
the @seclink["raw-accessors"]{Raw Accessors} appendix.

@section{Window Values}

@defstruct[window ([raw external/raw])]{
Wraps a browser Window object.
}

@defproc[(Window) window?]{
@(mdn-bar "Window"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window")
Returns the current browser Window object wrapped in a checked struct.
}

@defproc[(window-self) window?]{
@(mdn-bar "Window: self property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/self")
Returns the current browser Window object via @racket[self], wrapped in
the checked struct.
}

@defstruct[window-document-info ([raw external/raw])]{
Wraps the current document object returned by @racket[window-document].
}

@defproc[(window-document-info-raw [doc window-document-info?])
         external/raw]{
Returns the wrapped document object.
}

@defstruct[window-location-info ([raw external/raw])]{
Wraps the current location object returned by @racket[window-location].
}

@defproc[(window-location-info-raw [loc window-location-info?])
         external/raw]{
Returns the wrapped location object.
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

@section{Window Quick Start}

Start by including the library, getting the current document, and
calling a simple page-level helper.

@racketblock[
(code:comment "Include the checked Window wrapper library.")
(include-lib window)

(code:comment "Capture the current Window object as a checked wrapper.")
(define win
  (Window))

(code:comment "Get the current document from the browser window.")
(define doc
  (window-document))

(code:comment "Show a simple confirmation dialog.")
(define ok?
  (window-confirm "Continue with this action?"))

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

This example shows how to use the current document and schedule work
to happen a little later. The timer is useful when you want to defer a
page update without blocking the current event handler.

@racketblock[
(code:comment "Include the wrapper library at the top level.")
(include-lib window)

(code:comment "Capture the current browser Window object.")
(define win
  (Window))

(code:comment "Get the current document so we can inspect the page.")
(define doc
  (window-document))

(code:comment "Keep the wrapped document value around for later DOM work.")

(code:comment "Run a small update after a short delay.")
(window-set-timeout
  (lambda ()
    (displayln "Hello from the browser window")))
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

@defproc[(window-get-selection) (or/c #f selection?)]{
@(mdn-bar "Window: getSelection() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/getSelection")
Returns the current selection as a wrapped @racket[selection] value.
}

@section{Window Style Values}

@defstruct[media-query-list ([raw external/raw])]{
Wraps a browser @tt{MediaQueryList} object.
}

@defproc[(media-query-list-raw [query media-query-list?]) external/raw]{
Returns the wrapped browser MediaQueryList object.
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

@defproc[(css-style-declaration-raw [style css-style-declaration?]) external/raw]{
Returns the wrapped browser CSSStyleDeclaration object.
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
         (or/c #f window?)]{
@(mdn-bar "Window: open() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/open")
Opens a new browsing context. The @racket[url], @racket[target], and
@racket[features] arguments accept strings or symbols, and
@racket[replace] accepts a boolean value.
The returned value is wrapped as @racket[window] when the browser opens
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
dictionary form. Use @racket[#f] to omit the options argument.
}

@defproc[(window-scroll-by [x real?] [y real?]
                           [options (or/c #f window-scroll-options? procedure?) #f])
         void?]{
@(mdn-bar "Window: scrollBy() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/scrollBy")
Scrolls the window by a relative offset. The optional @racket[options]
argument can supply a @racket[window-scroll-options] struct to use the
browser's dictionary form. Use @racket[#f] to omit the options argument.
}

@defproc[(window-scroll [x real?] [y real?]
                        [options (or/c #f window-scroll-options? procedure?) #f])
         void?]{
@(mdn-bar "Window: scroll() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/scroll")
Scrolls the window to an absolute position. The optional @racket[options]
argument can supply a @racket[window-scroll-options] struct to use the
browser's dictionary form. Use @racket[#f] to omit the options argument.
}

@defproc[(window-set-timeout [callback external?]) exact-nonnegative-integer?]{
@(mdn-bar "Window: setTimeout() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/setTimeout")
The raw @racket[callback] argument should be a browser JavaScript
function value.

Schedules a one-shot timer callback.
}
