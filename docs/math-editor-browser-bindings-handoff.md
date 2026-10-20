# Math-editor browser-binding handoff

## Handoff ID and revision

- Handoff ID/date: `webracket-browser-bindings-foundation-2026-10-20`.
- WebRacket baseline: `d3c79f72fe7e1116c12d4481e618c3b68ff6eb6e`.
- Final revision: `d3c79f72fe7e1116c12d4481e618c3b68ff6eb6e`.
- Validation-time WebRacket dirty-worktree fingerprint:
  `ad7e9af37acafa13bcf284dd4ebf77733e314f3a`; it includes unrelated
  pre-existing worktree changes.

## Imports, exports, and contracts

The full public import map and exported operation groups are in
[`math-editor-browser-bindings.md`](math-editor-browser-bindings.md). In
short: `browser/abort`, `browser/event-target`, `browser/range`,
`browser/font`, and `browser/segmenter` are new public modules; DOM/event,
window, Promise, ResizeObserver, and clipboard additions are exported by
their existing public modules.

- Optional arguments use `#f` only where the API documents omission: listener
  options default to false values, grapheme locale defaults to the browser
  locale, text-blob MIME type defaults to the empty native type, and textarea
  selection direction defaults to browser behavior.
- Nullable native object fields become `#f` at the documented wrapper
  boundaries. Browser errors, permission rejections, invalid DOM hierarchy,
  and unavailable APIs are not hidden.
- Range, textarea selection, and `Intl.Segmenter` indexes stay in native
  UTF-16 code units; browser CSS geometry stays in CSS pixels.

## Callback lifetime and scheduling

- Each event-registration token holds one callback bridge until `once`,
  explicit removal, or native abort removes it.
- Promise handlers live with the chained native Promise until settlement.
- ResizeObserver callbacks live with the native observer until disconnect or
  native collection.
- Timer, interval, animation-frame, and microtask bridges live with their
  native scheduler registrations. Cancel intervals and requests when they are
  no longer needed.
- Effect-only callbacks should return `(void)`; the browser probe exercises
  this rule for nested Promise work.

## Compiled browser probes

All commands run from the WebRacket repository root. The generic runner
builds into `local/math-editor-browser-bindings/<probe-id>/` only.

```sh
MATH_EDITOR_BINDINGS_PROBE_ID=b1 \
  bash test/run-math-editor-binding-probes.sh
MATH_EDITOR_BINDINGS_PROBE_ID=b2 \
  bash test/run-math-editor-binding-probes.sh
MATH_EDITOR_BINDINGS_PROBE_ID=b3 \
  bash test/run-math-editor-binding-probes.sh
MATH_EDITOR_BINDINGS_PROBE_ID=interop \
  MATH_EDITOR_BINDINGS_B1_SOURCE=test/test-math-editor-bindings-interop.rkt \
  MATH_EDITOR_BINDINGS_PROBE_SCRIPT="$PWD/test/run-math-editor-binding-interop-probe.mjs" \
  bash test/run-math-editor-binding-probes.sh
```

- B1 source: `test/test-math-editor-bindings-b1.rkt`; expected receipt:
  `pass` with 20 assertions.
- B2 source: `test/test-math-editor-bindings-b2.rkt`; expected receipt:
  `pass` with 28 assertions after the abort-reason assertion.
- B3 source: `test/test-math-editor-bindings-b3.rkt`; expected receipt:
  `pass` with 15 assertions.
- WB-07 source: `test/test-math-editor-bindings-interop.rkt`; expected receipt
  includes `sum: 42`, `veto: false`, `nested: 42`, asynchronous completion,
  an intentional propagated failure, and `consoleBridgePresent: false`.

## Environment and provenance

- Browser: Playwright Chromium headless shell `Google Chrome for Testing
  149.0.7827.55` on macOS arm64.
- Math-editor input revision: `7f3d0bcdcdca2f5de8f42633e93c90c5035d829c`.
  Its validation-time dirty-worktree fingerprint was
  `076707001e995a018ae04b6c6855560a84ecce03`, reflecting files already
  present there; this work made no writes in that repository.
- Source plan (read only):
  `/Users/soegaard/Dropbox/GitHub/math-editor/plans/webracket-browser-bindings-implementation-prompt.md`, SHA-256
  `4076997bcd96a8f31711028b1c39d6dd16e6e8e2629e42c550937e687f55f1f1`.
- Consumer-source SHA-256: B1
  `4896db1dd5f9a5d239425cbfd30571aaa0866470fdf5283b61105aa53102bd84`;
  B2 `86ee8787000c3f255a043616f9358236bc90688685359cb7efff5bdfa9f35a47`;
  B3 `e918ca0748065a0e9c5780faa53bcc9f7bdecf003b537d8941fc3e504f0b29d7`;
  WB-07 `4b83566926ffcaaaf397940270dfc9a5a268087645538111234b20dc1d16e156`.
- Compiled-Wasm SHA-256: B1
  `6fc04a2ab39f9ad978ff9ebf91f2a466362c8d5161e99380f226c9c4d607333c`;
  B2 `5596138d0e2846e6beaa2f6b551424257d3390ce8b28c6a41773ac60b220dfd1`;
  B3 `f56293e0d7ebb57a2f7e3bc9f3387dad4a302a7bec3be5e37eec87c6179afbc8`;
  WB-07 `86446fa99c820e5e20c23c4f7cf722e62164656cd8dfbbe3dafaa6979dcd3b9f`.
- Shared B2 runtime asset SHA-256:
  `webracket-runtime-fd0b5606d7c2140f7a989fec3efd5e9b02a1a191.wasm`
  `4eba1cc946afaa8600f0ff620a57f99cead09dd54c2b81ff4dca85fe70d1379a`;
  FFI asset `webracket-ffi-9d31aa1e7fa24ea00e9cfc8d5b4ca02d36cfcb6a.wasm`
  `44a442ee84e4f90036a79eb9df6346b56d832c14b90f4f4f155ae46c5f96ad2c`.
- Builds write solely under `local/math-editor-browser-bindings/`; no files
  were written to `public/`.

## Gaps and unrun conditions

- The browser probes do not execute OS clipboard read/write because those
  operations can require a trusted activation and user permission; B3 instead
  validates synthetic DataTransfer plus Blob/ClipboardItem behavior.
- Real IME hardware/composition, clipboard permission prompts, and
  cross-browser Safari/Firefox behavior remain for downstream manual testing.
- Font readiness is native Promise readiness, not a claim that a particular
  face was used in rendered layout.

## Downstream impact

The foundation unblocks math-editor packages that need native input events,
selection geometry, namespaces, scheduling, font readiness, segmentation,
and transfer payloads. The surface is additive; no editor algorithm was
ported or changed.
