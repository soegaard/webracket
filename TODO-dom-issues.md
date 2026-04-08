# DOM Library Issues to Fix Later

This file collects the issues we ran into while getting the DOM libraries working, with an eye toward fixing the underlying compiler, runtime, and tooling problems instead of working around them in examples.

For interop conventions and helper selection, see the `JS interop conventions` section in [`DESIGN-new-web-api.md`]( /Users/soegaard/Dropbox/GitHub/webracket/DESIGN-new-web-api.md ).

## Compiler / Runtime Limitations

- `for/vector` issue
  - Category: compiler/runtime semantics
  - What happened: browser-side code hit a limitation or mismatch around `for/vector`.
  - Why it matters: this should be fixed in the compiler/runtime, not by changing examples.
  - Future fix area: compiler lowering and loop semantics.

- Browser build vs browser runner timing
  - Category: runtime/test harness interaction
  - What happened: tests could pass under direct Playwright probing but still fail in the headless wrapper because the wrapper had stricter expectations.
  - Why it matters: the page can be correct while the runner still reports failure.
  - Future fix area: test harness behavior and diagnostics.

## JS Interop / Runtime Bridge Mismatches

- `js-ref` on array-like JS objects
  - Category: JS interop
  - What happened: using `js-ref` with numeric indices did not behave correctly for some JS arrays or array-likes; some code needed string property keys instead.
  - Why it matters: this is a core boundary issue between Racket values and JS property access.
  - Future fix area: `js-ref` / property lookup semantics or clearer API docs. For recurring wrapper properties, prefer direct bindings instead of the generic property bridge.

- `media-current-time` bridge mismatch
  - Category: JS interop
  - What happened: the `currentTime` media wrapper still trips a bridge/runtime error in the browser probe even though the raw JS property reads correctly.
  - Why it matters: this is a good example of a browser field that looks simple but still needs the right bridge shape.
  - Future fix area: media property bridging and the direct FFI wrapper path.

- One-off wrapper-shape fixes for DOM properties
  - Category: DOM bridge / interop
  - What happened: a handful of properties needed the wrapper shape adjusted to match the actual browser object.
  - Examples:
    - `media-cross-origin` needed direct property access through `crossOrigin`
    - `media-src-object` needed to distinguish `MediaSource`-style values from `MediaStream`
    - `media-set-src-object!` needed direct property assignment to `srcObject`
    - `dom-token-list-value` exposed the need to use the indexed-access helper instead of stringifying numeric indices
  - Why it matters: these were mostly one-off fixes, but they show how easy it is to pick the wrong JS bridge shape on the first pass.
  - Future fix area: wrapper generation and JS bridge conventions. The long-term direction is to use direct bindings for repeated public properties and reserve the generic bridge for rare or dynamic cases.

- Canvas image-data bridge methods
  - Category: JS interop
  - What happened: `createImageData` and `getImageData` needed browser calling conventions fixed up so the runtime used the JS methods correctly.
  - Why it matters: this is an FFI shape mismatch rather than a library logic error.
  - Future fix area: JS method invocation helpers and optional-argument handling.

- `performance.event-counts` missing as a callable import
  - Category: runtime/assembler mismatch
  - What happened: the import table did not match the actual `Performance` API shape, causing a `LinkError` at instantiation.
  - Why it matters: this is a hard failure before the program can run.
  - Future fix area: `assembler.rkt` and runtime import object parity checks.

## DOM Wrapper Modeling Issues

- `text` wrapper name collision
  - Category: API naming / wrapper model
  - What happened: the DOM text wrapper collided with `web-easy`'s `text`.
  - Fix made: renamed the wrapper to `text-node`.
  - Why it matters: this is an API-shape issue, not just a docs issue.
  - Future fix area: wrapper naming policy for generic DOM types.

- `media-src-object` classification
  - Category: wrapper modeling
  - What happened: the source object could be either a media source or a stream, and the wrapper initially classified it incorrectly.
  - Why it matters: wrapper predicates need to match the actual runtime object kind.
  - Future fix area: object-type discrimination helpers.

- Checked wrappers and checked structs were not clearly explained
  - Category: docs plus API contract clarity
  - What happened: the docs needed clearer explanations of what a checked wrapper is and what a checked struct means.
  - Why it matters: users need to understand the boundary between raw browser objects and wrapped values.
  - Future fix area: design-guide language and consistent API docs.

## Scribble / Documentation Tooling Issues

- Missing docs anchors and label-module mismatches
  - Category: docs tooling
  - What happened: several `@defproc` and `for-label` targets were missing or not lined up, producing red underlines and undefined-tag warnings.
  - Why it matters: broken cross references make the manual look broken even when the code works.
  - Future fix area: docs generation conventions and automated checks.

- Standalone vs full-manual rendering assumptions
  - Category: docs tooling / workflow
  - What happened: rendering a chapter by itself produced misleading cross-reference behavior; the full manual is the correct unit.
  - Why it matters: Scribble docs in this repo are meant to be rendered as one whole document.
  - Future fix area: docs workflow guidance and render checks.

- `dom` chapter was documented like a value, not a library
  - Category: docs modeling
  - What happened: `dom` was initially boxed as if it were a value, which was semantically wrong.
  - Why it matters: `dom` is an import point, not a runtime value.
  - Future fix area: Scribble form choice and library-documentation conventions.

## Example / Test Harness Problems

- `displayln` in browser examples
  - Category: example environment mismatch
  - What happened: `displayln` was used where the example really needed `console-log`.
  - Why it matters: browser examples should speak to the browser console, not rely on host I/O.
  - Future fix area: example conventions and browser-console helpers.

- Headless runner required console output
  - Category: test harness assumption
  - What happened: `run-dom-headless.sh` failed when a page produced no console output, even if the assertions passed in Playwright.
  - Why it matters: the page can be correct while the runner still flags it as failing.
  - Future fix area: runner policy, or a minimal expected console marker.

- Temporary debug logging in tests
  - Category: test hygiene
  - What happened: we had to insert and later remove lots of checkpoint logs while isolating issues.
  - Why it matters: helpful during debugging, but should not stay in the final test file.
  - Future fix area: better diagnostics or more targeted test probes.

## Compiler / Runtime Follow-Ups Worth Tracking Separately

- `for/vector` issue
  - This is the clearest compiler/runtime semantics gap from the DOM work.
  - Fixing it at the compiler level should reduce the need for special-case workarounds in browser-facing code.

- `js-ref` on array-like JS objects
  - This is a core JS interop issue.
  - We should decide whether `js-ref` should support numeric indexing consistently for array-like objects or whether the DOM layer should keep avoiding that shape.

- `media-current-time` bridge mismatch
  - The raw JS `currentTime` property reads as expected in the browser fixture.
  - The WebRacket wrapper still trips a bridge/runtime error, so this needs a closer look at the media accessor path.

- One-off wrapper-shape fixes for DOM properties
  - These are mostly individual cases, not one recurring bug.
  - They show up when a browser object wants direct property access, a different return classification, or an indexed-access helper instead of a stringified property lookup.

- Canvas image-data bridge methods
  - `createImageData` and `getImageData` needed browser calling conventions fixed up.
  - This is another good candidate for a runtime bridge helper or compiler-level call convention check.

- `performance.event-counts` missing as a callable import
  - This was a hard runtime/assembler mismatch.
  - It is worth keeping a dedicated check or audit for browser import objects so these fail earlier.
