# Browser-binding foundation for math-editor consumers

This ledger records browser-facing WebRacket primitives needed by the
math-editor integration plan. It is deliberately a binding layer: it does
not copy the editor's parsing, rendering, selection-policy, or layout
algorithms.

## Import and operation ledger

| Need | Public WebRacket surface | Semantics and limits | Evidence |
| --- | --- | --- | --- |
| DOM node construction and traversal | `browser/document`: `document-create-element-ns`, `document-active-element`; `browser/element`: `append-child!`, `dom-node-*`, `element-focus!`, `element-blur!`, `element-selection-*` | Namespace input is a string; DOM null becomes `#f` where noted; text and selection offsets stay in the browser's native UTF-16 domain. | B1, B2 |
| Synthetic native events and event data | `browser/event`: `event-new`, cancellation and default-prevented queries, keyboard composition/modifier queries, InputEvent, CompositionEvent, ClipboardEvent, FocusEvent, and PointerEvent accessors | Event type strings/symbols are normalized; nullable native event fields map to `#f`; these accessors apply no editor policy. | B1 |
| Listener lifecycle | `browser/event-target`: `make-event-listener-options`, `event-target-add-listener!`, `event-target-remove-listener!`, `event-target-listener-active?`, `event-target-dispatch!`; `browser/abort` | A returned registration token owns exactly one callback bridge. `once?` marks its token inactive before nested dispatch; explicit removal releases it. `AbortController.abort` is synchronous. | B2 |
| Browser scheduling | `browser/window`: timers, interval, animation frame, `window-queue-microtask`, `window-random-uuid`, visual-viewport helpers; `js/promise`; `resize-observer` | A procedure is converted to one native bridge for each attachment/schedule. The native Promise, observer, or scheduler owns it while active; no library-global callback cache is retained. Handlers intended only for effects should return `(void)`. | B2, B3 |
| Native range and selection geometry | `browser/range`: Range endpoints, collapse/select/compare, client rectangles, TreeWalker, Selection range helpers | Node offsets and segmentation indexes remain native UTF-16 code-unit or child-boundary offsets. `TreeWalker.nextNode` returns `#f` at exhaustion. | B2 |
| Font readiness and graphemes | `browser/font`: FontFaceSet access/status/check/ready; `browser/segmenter`: native grapheme Segmenter records | `document-font-face-set` can return `#f`; `font-face-set-ready` is not a layout guarantee. Intl segment records retain native indexes. | B2 |
| Clipboard payload representations | `clipboard`: DataTransfer, Blob, ClipboardItem, `clipboard-read`, `clipboard-write!`, plus text helpers | DataTransfer calls are event-lifetime scoped and synchronous. Blob and ClipboardItem methods return promises where the browser does. OS clipboard read/write remains permission- and user-activation-gated. | B3 uses synthetic DataTransfer and Blob/ClipboardItem only |
| Cross-module callback exports | `webracket/primitives`: `procedure->external`, plus `webracket/unsafe/standard` for the narrowly scoped interop probe | The probe publishes four exported callback values to `globalThis` and invokes them from ordinary browser JavaScript. It does not expose a console bridge or depend on root-module exports. | WB-07 interop probe |

## Required imports

Consumer programs import only the public modules listed above. The reusable
B3 consumer also proves that nested Racket modules can import
`browser/abort`, `browser/document`, `browser/element`, `browser/event`,
`browser/event-target`, `browser/font`, `browser/range`,
`browser/segmenter`, `browser/window`, `clipboard`, `js/promise`, and
`resize-observer` together.

## Browser API sources

The public Scribble documentation links each new browser binding to its MDN
API signature. The WebRacket adapter sources are `document.ffi`,
`element.ffi`, `event.ffi`, and the generic checked standard interop helpers;
the source comments record the Racket-level contract and purpose.

## Compatibility

The changes are additive. Existing listener helpers continue to work, while
the new token API provides deterministic removal. Native coordinate domains,
browser permission failures, and browser-specific feature availability are
preserved instead of emulated.
