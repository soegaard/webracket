#lang scribble/manual

@(require scribble/manual
          (for-label (lib "scribblings/lib-browser-binding-foundation-labels.rkt" "webracket-doc")
                     (lib "scribblings/lib-js-promise-labels.rkt" "webracket-doc"))
          "webracket-scribble-utils.rkt")

@title{Browser Binding Foundation}

The modules below are the focused DOM, event, lifecycle, geometry, and
segmentation bindings used by browser consumers. They preserve native object
identity and browser coordinate domains; they do not implement editor policy.

@section{Abort and listener lifecycle}

@defmodule[browser/abort]

@defproc[(abort-controller-supported?) boolean?]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/AbortController"]{AbortController}.

@tt{abort-controller-supported? : -> boolean?     [standard.ffi]}

Reports whether the browser exposes a constructible @tt{AbortController}.}

@defproc[(make-abort-controller) any/c]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/AbortController/AbortController"]{AbortController()}.

@tt{make-abort-controller : -> abort-controller?     [standard.ffi]}

Creates a native controller or raises when it is unavailable.}

@defproc[(abort-controller-signal [controller any/c]) any/c]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/AbortController/signal"]{AbortController.signal}.

@tt{abort-controller-signal : abort-controller? -> abort-signal?     [standard.ffi]}

Returns the controller's native signal.}

@defproc[(abort-controller-abort! [controller any/c]) void?]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/AbortController/abort"]{AbortController.abort()}.

@tt{abort-controller-abort! : abort-controller? -> void?     [standard.ffi]}

Synchronously aborts the signal with the browser default reason.}

@defproc[(abort-controller-abort/with-reason! [controller any/c] [reason any/c]) void?]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/AbortController/abort"]{AbortController.abort(reason)}.

@tt{abort-controller-abort/with-reason! : abort-controller? value -> void?     [standard.ffi]}

Synchronously aborts the signal with @racket[reason].}

@defproc[(abort-signal-aborted? [signal any/c]) boolean?]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/AbortSignal/aborted"]{AbortSignal.aborted}.

@tt{abort-signal-aborted? : abort-signal? -> boolean?     [standard.ffi]}

Reports the native signal state synchronously.}

@defproc[(abort-signal-reason [signal any/c]) (or/c #f any/c)]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/AbortSignal/reason"]{AbortSignal.reason}.

@tt{abort-signal-reason : abort-signal? -> (or/c #f any/c)     [standard.ffi]}

Returns the browser reason, mapping only native nullish values to @racket[#f].}

@defmodule[browser/event-target]

@defproc[(make-event-listener-options [capture? boolean? #f]
                                      [passive? boolean? #f]
                                      [once? boolean? #f]
                                      [signal (or/c #f any/c) #f])
         any/c]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener#options"]{EventTarget.addEventListener options}.

@tt{make-event-listener-options : [boolean? boolean? boolean? (or/c #f abort-signal?)] -> event-listener-options?     [standard.ffi]}

Creates listener options; every omitted boolean defaults to @racket[#f].}

@defproc[(event-target-add-listener! [target any/c] [type (or/c string? symbol?)]
                                     [listener procedure?] [options any/c])
         any/c]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener"]{EventTarget.addEventListener()}.

@tt{event-target-add-listener! : event-target? string/symbol procedure? event-listener-options? -> event-listener-token?     [standard.ffi]}

Registers one native listener and returns the token that owns that registration. A listener runs during native dispatch and its result is ignored.}

@defproc[(event-target-remove-listener! [token any/c]) void?]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/removeEventListener"]{EventTarget.removeEventListener()}.

@tt{event-target-remove-listener! : event-listener-token? -> void?     [standard.ffi]}

Removes the token's registration and releases the binding's callback reference.}

@defproc[(event-target-dispatch! [target any/c] [event any/c]) boolean?]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/dispatchEvent"]{EventTarget.dispatchEvent()}.

@tt{event-target-dispatch! : event-target? external? -> boolean?     [standard.ffi]}

Dispatches synchronously and returns @racket[#f] exactly when a cancelable event was canceled.}

@defproc[(event-target-listener-active? [token any/c]) boolean?]{
@tt{event-target-listener-active? : event-listener-token? -> boolean?     [standard.ffi]}

Reports whether a registration token still owns an installed listener.}

@section{Native DOM geometry}

@defmodule[browser/range]

@defproc[(document-create-range) any/c]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/Document/createRange"]{Document.createRange()}.

@tt{document-create-range : -> dom-range?     [document.ffi]}

Creates a native DOM Range.}

@defproc[(document-create-tree-walker [root any/c] [what-to-show exact-nonnegative-integer?]) any/c]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/Document/createTreeWalker"]{Document.createTreeWalker()}.

@tt{document-create-tree-walker : dom-node? exact-nonnegative-integer? -> dom-tree-walker?     [document.ffi]}

Creates a TreeWalker with a native null filter; @racket[node-filter-show-text] selects text nodes.}

@defproc[(range-set-start! [range any/c] [node any/c] [offset exact-nonnegative-integer?]) void?]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/Range/setStart"]{Range.setStart()}.

@tt{range-set-start! : dom-range? dom-node? exact-nonnegative-integer? -> void?     [standard.ffi]}

Sets the start in the browser's native child or UTF-16 text-offset domain.}

@defproc[(range-set-end! [range any/c] [node any/c] [offset exact-nonnegative-integer?]) void?]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/Range/setEnd"]{Range.setEnd()}.

@tt{range-set-end! : dom-range? dom-node? exact-nonnegative-integer? -> void?     [standard.ffi]}

Sets the end in the browser's native child or UTF-16 text-offset domain.}

@defproc[(range-start-offset [range any/c]) exact-nonnegative-integer?]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/Range/startOffset"]{Range.startOffset}.

@tt{range-start-offset : dom-range? -> exact-nonnegative-integer?     [standard.ffi]}

Returns the unconverted native offset.}

@defproc[(range-end-offset [range any/c]) exact-nonnegative-integer?]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/Range/endOffset"]{Range.endOffset}.

@tt{range-end-offset : dom-range? -> exact-nonnegative-integer?     [standard.ffi]}

Returns the unconverted native offset.}

@defproc[(range-get-client-rects [range any/c]) vector?]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/Range/getClientRects"]{Range.getClientRects()}.

@tt{range-get-client-rects : dom-range? -> vector?     [standard.ffi]}

Returns client rectangles in browser CSS-pixel coordinates.}

@defproc[(range-get-bounding-client-rect [range any/c]) any/c]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/Range/getBoundingClientRect"]{Range.getBoundingClientRect()}.

@tt{range-get-bounding-client-rect : dom-range? -> dom-rect?     [standard.ffi]}

Returns the native range bounding rectangle.}

@defproc[(tree-walker-next-node [walker any/c]) any/c]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/TreeWalker/nextNode"]{TreeWalker.nextNode()}.

@tt{tree-walker-next-node : dom-tree-walker? -> (or/c #f dom-node?)     [standard.ffi]}

Returns the next selected node or @racket[#f] when traversal is exhausted.}

@section{Font and segmentation services}

@defmodule[browser/font]

@defproc[(document-font-face-set) any/c]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/Document/fonts"]{Document.fonts}.

@tt{document-font-face-set : -> (or/c #f font-face-set?)     [document.ffi + standard.ffi]}

Returns @racket[#f] when the browser has no FontFaceSet.}

@defproc[(font-face-set-ready [fonts any/c]) js-promise?]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/FontFaceSet/ready"]{FontFaceSet.ready}.

@tt{font-face-set-ready : font-face-set? -> js-promise?     [standard.ffi]}

Returns the native readiness Promise; it does not by itself prove that a face was used for layout.}

@defproc[(font-face-set-check? [fonts any/c] [font (or/c string? symbol?)]
                          [text (or/c string? symbol?) ""]) boolean?]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/FontFaceSet/check"]{FontFaceSet.check()}.

@tt{font-face-set-check? : font-face-set? string/symbol [string/symbol] -> boolean?     [standard.ffi]}

Checks whether the CSS font shorthand can render text; omitted text defaults to the empty string.}

@defmodule[browser/segmenter]

@defproc[(make-grapheme-segmenter [locale (or/c #f string? symbol?) #f]) any/c]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Segmenter"]{Intl.Segmenter}.

@tt{make-grapheme-segmenter : [(or/c #f string? symbol?)] -> intl-segmenter?     [standard.ffi]}

Creates a grapheme segmenter; @racket[#f] uses the browser default locale.}

@defproc[(intl-segmenter-segment [segmenter any/c] [text string?]) any/c]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Segmenter/segment"]{Intl.Segmenter.prototype.segment()}.

@tt{intl-segmenter-segment : intl-segmenter? string? -> intl-segments?     [standard.ffi]}

Segments text and preserves native UTF-16 indexes.}

@defproc[(intl-segments-containing [segments any/c] [index exact-nonnegative-integer?]) any/c]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Segmenter/segment/Segments/containing"]{Segments.containing()}.

@tt{intl-segments-containing : intl-segments? exact-nonnegative-integer? -> (or/c #f intl-segment-data?)     [standard.ffi]}

Returns the native segment at a UTF-16 input index, or @racket[#f].}

@defproc[(intl-segment-data-index [data any/c]) exact-nonnegative-integer?]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Segmenter/segment/Segments#examples"]{Intl.Segmenter segment records}.

@tt{intl-segment-data-index : intl-segment-data? -> exact-nonnegative-integer?     [standard.ffi]}

Returns the record's native UTF-16 index.}

@section{Extended existing modules}

@defproc[(document-create-element-ns [namespace string?] [qualified-name (or/c string? symbol?)]) any/c]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/Document/createElementNS"]{Document.createElementNS()}.

@tt{document-create-element-ns : string? string/symbol -> element?     [document.ffi]}

Creates an element in @racket[namespace], such as the MathML namespace.}

@defproc[(document-active-element) (or/c #f element?)]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/Document/activeElement"]{Document.activeElement}.

@tt{document-active-element : -> (or/c #f element?)     [document.ffi + standard.ffi]}

Returns the focused element or @racket[#f] for a nullish native value.}

@defproc[(dom-node-text-content [node any/c]) (or/c #f string?)]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/Node/textContent"]{Node.textContent}.

@tt{dom-node-text-content : dom-node? -> (or/c #f string?)     [standard.ffi]}

Returns text content without parsing it, mapping native null to @racket[#f].}

@defproc[(dom-node-set-text-content! [node any/c] [text (or/c #f string? symbol?)]) void?]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/Node/textContent"]{Node.textContent}.

@tt{dom-node-set-text-content! : dom-node? (or/c #f string? symbol?) -> void?     [standard.ffi]}

Writes text content without parsing markup; @racket[#f] writes native null.}

@defproc[(element-set-selection-range! [element any/c] [start exact-nonnegative-integer?]
                                        [end exact-nonnegative-integer?]
                                        [direction (or/c #f string? symbol?) #f])
         void?]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement/setSelectionRange"]{HTMLTextAreaElement.setSelectionRange()}.

@tt{element-set-selection-range! : element? exact-nonnegative-integer? exact-nonnegative-integer? [(or/c #f string? symbol?)] -> void?     [standard.ffi]}

Sets native textarea selection; the omitted direction defaults to the browser default.}

@defproc[(input-event-data [event any/c]) (or/c #f string?)]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/InputEvent/data"]{InputEvent.data}.

@tt{input-event-data : input-event? -> (or/c #f string?)     [event.ffi]}

Returns native input data, preserving an empty string and mapping native null to @racket[#f].}

@defproc[(input-event-input-type [event any/c]) string?]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/InputEvent/inputType"]{InputEvent.inputType}.

@tt{input-event-input-type : input-event? -> string?     [event.ffi]}

Returns the browser input-type string without editor policy.}

@defproc[(input-event-is-composing? [event any/c]) boolean?]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/InputEvent/isComposing"]{InputEvent.isComposing}.

@tt{input-event-is-composing? : input-event? -> boolean?     [event.ffi]}

Reports whether the input event belongs to an IME composition.}

@defproc[(window-queue-microtask [callback procedure?]) void?]{
MDN: @hyperlink["https://developer.mozilla.org/en-US/docs/Web/API/Window/queueMicrotask"]{Window.queueMicrotask()}.

@tt{window-queue-microtask : procedure? -> void?     [window.ffi]}

Queues @racket[callback] at the browser microtask checkpoint; the binding retains it only while the native queue owns it.}
