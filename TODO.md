# TODO

- Validate SXML input in helper/render functions and raise clear compile-time errors on malformed structures (for example, wrong `card-grid` shape) instead of surfacing opaque runtime `WebAssembly.Exception` failures in the browser.
- Add `js-set-text-content!` to the DOM FFI so examples can set text content directly without manual text-node creation.
- Change `js-ref/extern` to behave like `js-ref/extern/null` (map JS `null` to `#f`) and add `js-ref/extern/raw` for the current raw return behavior.
