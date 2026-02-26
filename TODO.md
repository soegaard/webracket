# TODO

- Validate SXML input in helper/render functions and raise clear compile-time errors on malformed structures (for example, wrong `card-grid` shape) instead of surfacing opaque runtime `WebAssembly.Exception` failures in the browser.
- Add `js-set-text-content!` to the DOM FFI so examples can set text content directly without manual text-node creation.
- Change `js-ref/extern` to behave like `js-ref/extern/null` (map JS `null` to `#f`) and add `js-ref/extern/raw` for the current raw return behavior.
- Investigate suspected compiler/runtime bug with local mutation in browser stdlib code: in `sxml->dom`, a `set!`-based attrs/children update path produced inconsistent state (`attribute-marker?` matched `(@ ...)`, but runtime values behaved as if attrs stayed `null` and children still contained the `(@ ...)` block). Refactoring to pure `define-values`/`let` computation fixed the site; after cleanup, isolate and minimize this into a compiler bug repro and fix.
