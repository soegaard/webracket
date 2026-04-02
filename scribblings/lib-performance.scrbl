#lang scribble/manual

@(require scribble/manual
          (for-label (only-in racket/base struct))
          (for-label (lib "scribblings/lib-performance-labels.rkt" "webracket"))
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/lib-iterator-labels.rkt" "webracket")))

@title{Library: @racketid[performance]}
@declare-exporting[(lib "scribblings/lib-performance-labels.rkt" "webracket")]

@(how-to-require include-lib performance (lib "libs/performance.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @racket[performance] library is a browser stopwatch and logbook for
the current page.

Use it when you want to measure how long something takes, record named
marks, or inspect timing data from the current page. The API stays
close to the MDN names, but the wrapper adds a little validation and
keeps the common calls easy to read from WebRacket.

The Performance API is mostly about:

@itemlist[
  @item{reading timestamps and page timing metadata}
  @item{creating marks and measures for your own code}
  @item{querying the browser's recorded performance entries}
]

The @racket[performance-event-counts] helper returns either @racket[#f]
or a @racket[performance-event-count-map] value that wraps the browser's
EventCounts object.

The @racket[performance-memory] helper returns either @racket[#f] or a
@racket[performance-memory-info] value that wraps the browser's memory
information object.

@section{Performance Quick Start}

Start by including the library, taking a timestamp, and measuring a
small piece of work.

@racketblock[
(code:comment "Include the checked performance wrapper library.")
(include-lib performance)

(code:comment "Capture the starting timestamp.")
(define start (performance-now))

(code:comment "Do the work you want to measure.")
(define total (for/sum ([n (in-range 1000)]) n))

(code:comment "Capture the ending timestamp and compute the duration.")
(define elapsed (- (performance-now) start))
]

The quick start shows the simplest path: call
@racket[performance-now] before and after the work you care about, and
subtract the results.
After this block, @racket[start] holds the starting timestamp,
@racket[elapsed] holds the duration, and @racket[total] holds the
computed sum.

@section{Performance Example}

This example uses named marks and a measure entry. Named marks are
useful when you want to keep a browser-friendly breadcrumb trail for a
piece of work, not just a raw duration.

@racketblock[
(code:comment "Include the performance wrapper library.")
(include-lib performance)

(code:comment "Clear any old marks or measures with the same names.")
(performance-clear-marks "compile-start")
(performance-clear-measures "compile-span")

(code:comment "Record the start of the work.")
(performance-mark "compile-start")

(code:comment "Pretend to do some work.")
(define total (for/sum ([n (in-range 1000)]) n))

(code:comment "Record the end of the work and create a measure entry.")
(performance-mark "compile-end")
(performance-measure "compile-span" "compile-start" "compile-end")

(code:comment "Look up the entries that were just created.")
(define marks (performance-get-entries-by-name "compile-start"))
(define measures (performance-get-entries-by-name "compile-span" "measure"))
]

The example shows the usual workflow:
clear any old entries, create a start mark, do the work, create an end
mark, and then ask the browser for the recorded measure.
After this block, @racket[marks] and @racket[measures] hold the browser
entry lists for the mark and measure names, and @racket[total] holds the
computed sum.

@section{Performance Properties}

@defproc[(performance-event-counts) (or/c #f performance-event-count-map?)]{
@(mdn-bar "Performance: eventCounts property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Performance/eventCounts")
Returns the browser's event-count map for the current page.
}

@defproc[(performance-interaction-count) exact-nonnegative-integer?]{
@(mdn-bar "Performance: interactionCount property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Performance/interactionCount")
Returns the current interaction count for the page.
}

@defproc[(performance-memory) (or/c #f performance-memory-info?)]{
@(mdn-bar "Performance: memory property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Performance/memory")
Returns the browser-specific memory information object when the browser
exposes it.
}

@defproc[(performance-time-origin) real?]{
@(mdn-bar "Performance: timeOrigin property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Performance/timeOrigin")
Returns the high-resolution start timestamp for the page.
}

@section{Performance EventCounts Map}

The browser's EventCounts map is a little table that records how many
times certain event types, such as @racket["click"], have been
dispatched on the current page. It is useful when you want to inspect
interaction activity without dealing with the browser's raw JavaScript
object directly.

The map-like helpers let you inspect the counts for a particular event
type without dropping back to the raw browser object.

The @racket[performance-event-count-map] struct keeps that browser
object tucked away in a single field. That gives the wrapper room to
grow with additional helpers without exposing the raw external value as
the main user-facing result.

The iterator-returning helpers on the map, such as
@racket[performance-event-count-map-entries], return wrapped
@racket[iterator] values rather than raw browser iterator objects, so
you can pass them directly to helpers such as @racket[iterator->vector]
or inspect the next step directly with the iterator helpers.

@defstruct[performance-event-count-map ([raw external/raw])]{
Wraps a browser EventCounts object in the checked struct used by
@racket[performance-event-counts].
}

@defproc[(performance-event-count-map-size [counts performance-event-count-map?])
         exact-nonnegative-integer?]{
Returns the number of recorded event types.
}

@defproc[(performance-event-count-map-entries [counts performance-event-count-map?])
         iterator?]{
Returns the wrapped browser iterator produced by
@racket[EventCounts.entries()] for the current page.
}

@defproc[(performance-event-count-map-keys [counts performance-event-count-map?])
         iterator?]{
@(mdn-bar "EventCounts: keys() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/EventCounts/keys")
Returns the wrapped browser iterator produced by @racket[EventCounts.keys()]
for the current page.
}

@defproc[(performance-event-count-map-values [counts performance-event-count-map?])
         iterator?]{
@(mdn-bar "EventCounts: values() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/EventCounts/values")
Returns the wrapped browser iterator produced by
@racket[EventCounts.values()] for the current page.
}

@defproc[(performance-event-count-map-get [counts performance-event-count-map?]
                                          [event-type (or/c string? symbol?)])
         (or/c #f exact-nonnegative-integer?)]{
@(mdn-bar "EventCounts: get() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/EventCounts/get")
Returns the count for the named event type, or @racket[#f] when the
browser does not expose a value for that type. The event type accepts a
string or symbol and is normalized to a string.
}

@defproc[(performance-event-count-map-has? [counts performance-event-count-map?]
                                           [event-type (or/c string? symbol?)])
         boolean?]{
@(mdn-bar "EventCounts: has() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/EventCounts/has")
Returns @racket[#t] when the browser exposes a count for the named
event type. The event type accepts a string or symbol and is normalized
to a string.
}

@defproc[(performance-event-count-map-for-each [counts performance-event-count-map?]
                                               [proc (or/c procedure? external?)])
         void?]{
@(mdn-bar "EventCounts: forEach() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/EventCounts/forEach")
The raw @racket[proc] argument, when supplied as an external, should be a
browser JavaScript function value. Calls @racket[proc] for each
event-count entry in the browser map.
The callback receives the count first, the event type second, and the
EventCounts map itself third, just like the underlying JavaScript
map-style API.
}

@section{Performance Memory Info}

The browser's memory information object reports the JavaScript heap
limits and usage for the current page. It is useful when you want to see
how much browser-managed memory your page is using, but it is a
browser-specific feature and may be unavailable.

@defstruct[performance-memory-info ([raw external/raw])]{
Wraps the browser memory information object used by
@racket[performance-memory].
}

@defproc[(performance-memory-info-js-heap-size-limit [memory-info performance-memory-info?])
         exact-nonnegative-integer?]{
Returns the JavaScript heap size limit reported by the browser.
}

@defproc[(performance-memory-info-total-js-heap-size [memory-info performance-memory-info?])
         exact-nonnegative-integer?]{
Returns the browser's total JavaScript heap size.
}

@defproc[(performance-memory-info-used-js-heap-size [memory-info performance-memory-info?])
         exact-nonnegative-integer?]{
Returns the browser's currently used JavaScript heap size.
}

@section{Performance Methods}

@defproc[(performance-clear-marks [mark-name (or/c #f string? symbol? procedure?) #f]) void?]{
@(mdn-bar "Performance: clearMarks() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Performance/clearMarks")
Clears performance marks, optionally filtered by name. Use @racket[#f] to omit
@racket[mark-name]. If you need a literal @racket[#f] value, pass a thunk such
as @racket[(lambda () #f)].
The mark name accepts a string or symbol and is normalized to a string.
}

@defproc[(performance-clear-measures [measure-name (or/c #f string? symbol? procedure?) #f]) void?]{
@(mdn-bar "Performance: clearMeasures() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Performance/clearMeasures")
Clears performance measures, optionally filtered by name. Use @racket[#f] to
omit @racket[measure-name]. If you need a literal @racket[#f] value, pass a
thunk such as @racket[(lambda () #f)].
The measure name accepts a string or symbol and is normalized to a string.
}

@defproc[(performance-clear-resource-timings) void?]{
@(mdn-bar "Performance: clearResourceTimings() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Performance/clearResourceTimings")
Clears resource timing entries from the browser buffer.
}

@defproc[(performance-get-entries) external/raw]{
@(mdn-bar "Performance: getEntries() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Performance/getEntries")
Returns the current list of performance entries.
}

@defproc[(performance-get-entries-by-name [name (or/c string? symbol?)]
                                          [entry-type (or/c #f string? symbol? procedure?) #f])
         external/raw]{
@(mdn-bar "Performance: getEntriesByName() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Performance/getEntriesByName")
Returns performance entries filtered by name and optional type. Use @racket[#f]
to omit @racket[entry-type]. If you need a literal @racket[#f] value, pass a
thunk such as @racket[(lambda () #f)]. The name and type arguments accept
strings or symbols and are normalized to strings.
}

@defproc[(performance-get-entries-by-type [entry-type (or/c string? symbol?)]) external/raw]{
@(mdn-bar "Performance: getEntriesByType() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Performance/getEntriesByType")
Returns performance entries filtered by type. The type accepts a string or
symbol and is normalized to a string.
}

@defproc[(performance-mark [name (or/c string? symbol?)]
                          [options any/c #f])
         external/raw]{
@(mdn-bar "Performance: mark() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Performance/mark")
Creates a named performance mark and returns the resulting entry. Use
@racket[#f] to omit @racket[options]. If you need a literal @racket[#f]
value, pass a thunk such as @racket[(lambda () #f)]. The mark name accepts a
string or symbol and is normalized to a string.
}

@defproc[(performance-measure [name (or/c string? symbol?)]
                              [start-or-options any/c #f]
                              [end-mark any/c #f])
         external/raw]{
@(mdn-bar "Performance: measure() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Performance/measure")
Creates a named performance measure and returns the resulting entry.
The optional arguments accept the browser's standard overload forms. Use
@racket[#f] to omit either optional argument. If you need a literal
@racket[#f] value, pass a thunk such as @racket[(lambda () #f)].
The measure name, start mark, end mark, and string-style options all accept
strings or symbols and are normalized to strings.
}

@defproc[(performance-measure-user-agent-specific-memory) external/raw]{
@(mdn-bar "Performance: measureUserAgentSpecificMemory() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Performance/measureUserAgentSpecificMemory")
Estimates user-agent-specific memory usage.
}

@defproc[(performance-now) real?]{
@(mdn-bar "Performance: now() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Performance/now")
Returns a monotonic browser timestamp suitable for elapsed-time
measurements.
}

@defproc[(performance-set-resource-timing-buffer-size [size exact-nonnegative-integer?]) void?]{
@(mdn-bar "Performance: setResourceTimingBufferSize() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Performance/setResourceTimingBufferSize")
Sets the resource timing buffer size.
}

@defproc[(performance-to-json) external/raw]{
@(mdn-bar "Performance: toJSON() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Performance/toJSON")
Returns a JSON-style representation of the Performance object.
}
