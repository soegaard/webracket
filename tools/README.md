Tools
=====

This folder holds various useful tools.

For now, there are a few tools here.


pretty.rkt
----------
Reads s-expressions from an input port and pretty-prints to an output port.


check-docs-standard.py
----------------------
Checks consistency for:
- `DOCS-standard.md` against `ffi/standard.ffi`
- `DOCS-dom.md` against `ffi/dom.ffi`
- `DOCS-js.md` against `ffi/js.ffi`
- `DOCS-math.md` against `ffi/math.ffi`
- `DOCS-jsxgraph.md` against `ffi/jsxgraph.ffi`
- `DOCS-xtermjs.md` against `ffi/xtermjs.ffi`

Per document, it validates that:
- documented table functions exist in the corresponding `.ffi`
- table function names link to an allowed docs source for that file
- there are no duplicate table function rows
- Table of Contents links cover all `##`/`###` sections
- coverage count matches documented table rows
- documented table rows match all non-legacy `define-foreign` entries

Run:

```
python3 tools/check-docs-standard.py
```

check-wrapper-arity.rkt
-----------------------
Scans `.rkt` files and reports wrapper calls with wrong arity.
Current checks:
- `with-class` must have 2 arguments
- `with-id` must have 2 arguments
- `with-attrs` must have 2 arguments

Useful for catching parenthesis drift where a wrapper accidentally swallows
many following forms (runtime symptom: arity mismatch with large `given` count).

Run:

```sh
racket tools/check-wrapper-arity.rkt lib/web-easy/smoke
```

fetch-html-element-attributes.mjs
---------------------------------
One-off generator for a machine-readable HTML element/attribute snapshot.
It fetches the `html-element-attributes` package source and writes:
- `lib/web-easy/generated/html-element-attributes.json`
- `lib/web-easy/spec/html-element-attributes.sexp`

Run:

```sh
node tools/fetch-html-element-attributes.mjs
```

Optional:

```sh
node tools/fetch-html-element-attributes.mjs --version 3.1.0
```
