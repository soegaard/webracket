# MiniScheme vs R5RS Gap List

Last updated: 2026-02-20

Scope:
- Interpreter source: `web-site/src/examples/minischeme/minischeme.rkt`
- Primitive list from `minischeme-primitives`
- Keyword/forms list from `minischeme-keywords`

Notes:
- MiniScheme already includes some non-R5RS extensions (`when`, `unless`, `displayln`, `writeln`, `add1`, `sub1`, promise state predicates).
- This file tracks what is still missing compared to R5RS.

## Missing R5RS Syntax Forms

- `define-syntax`
- `syntax-rules`
- `let-syntax`
- `letrec-syntax`

## Missing R5RS Procedures

- `angle`
- `call-with-input-file`
- `call-with-output-file`
- `char-ready?`
- `close-input-port`
- `close-output-port`
- `complex?`
- `current-input-port`
- `current-output-port`
- `eof-object?`
- `imag-part`
- `input-port?`
- `load`
- `magnitude`
- `make-polar`
- `make-rectangular`
- `open-input-file`
- `open-output-file`
- `output-port?`
- `peek-char`
- `read`
- `read-char`
- `real-part`
- `set-car!`
- `set-cdr!`
- `transcript-off`
- `transcript-on`
- `with-input-from-file`
- `with-output-to-file`

## Current Behavioral Gaps (not strict R5RS compatibility)

- `display`, `write`, `write-char`, and `newline` currently do not accept optional port arguments.
- `letrec` has a regression-locked behavior for the multi-binding continuation probe test.

Implemented since initial draft:
- `eval`
- `interaction-environment`
- `scheme-report-environment` (version `5`)
- `null-environment` (version `5`)

## Lowest Hanging Fruit

The best first target is the complex-number family:
- `complex?`
- `make-rectangular`
- `make-polar`
- `real-part`
- `imag-part`
- `magnitude`
- `angle`

Why this is the easiest:
- These are pure wrappers over existing host numeric operations.
- They require no evaluator/control-flow changes.
- They do not require file/port infrastructure.
- They can be added with small primitive table edits plus straightforward tests.
