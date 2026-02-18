Tools
=====

This folder holds various useful tools.

For now, there is one tool here.


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
