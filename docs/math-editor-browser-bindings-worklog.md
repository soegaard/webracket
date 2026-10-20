# Browser-binding foundation worklog

## 2026-10-20 — WB-00 through WB-02

- Read the math-editor plan as a read-only requirements source and recorded
  the WebRacket revision before implementation.
- Added DOM namespace/node, native event-data, input selection/focus, and
  explicit EventTarget token bindings.
- Added the B1 browser consumer for namespaced MathML construction, listener
  dispatch/cancellation, input data, and textarea-selection behavior.

## 2026-10-20 — WB-03 through WB-05

- Added abort, Range/TreeWalker/Selection, FontFaceSet, native grapheme
  segmentation, visual viewport, UUID, and microtask adapters.
- Removed module-global callback caches from Promise, ResizeObserver, and
  newly added scheduling/listener helpers. Native owners now determine the
  active lifetime.
- A minimal TreeWalker failure showed that the raw third
  `Document.createTreeWalker` argument cannot be Racket `#f`; the binding now
  supplies native `null`. The complete B2 browser probe then passed.

## 2026-10-20 — WB-06 through WB-07

- Extended `clipboard` with synchronous DataTransfer, Blob, ClipboardItem,
  and MIME read/write bindings.
- Corrected array-like MIME type handling because generic property access can
  produce a Racket vector; both vectors and native array-like values are now
  checked in order.
- Added the B3 services probe. It creates synthetic DataTransfer and native
  Blob/ClipboardItem values and deliberately avoids changing the user's OS
  clipboard.
- Added the WB-07 interop probe, which invokes compiled callback values from
  ordinary browser JavaScript and checks synchronous values, cancellation,
  nested callbacks, asynchronous completion, and intentional failure
  propagation.

## 2026-10-20 — WB-08

- Added the public Scribble section, callback-lifetime notes, this ledger,
  and the handoff record.
- Final validation reruns B1, B2, B3, and WB-07 after documentation and
  source checks are complete.
