# AGENTS.md

Coding guidelines for Racket/WebRacket code in `lib/web-easy`.

1. Prefer `cond` over nested `if`/`let` patterns when branching logic is non-trivial.
2. Prefer internal `define` inside `cond` branches instead of wrapping branch bodies in `let` or `let*`.
3. Align right-hand-side expressions when it improves readability.
4. Avoid hanging parentheses; keep closing `))` on the same line as the final expression.
5. Prefer named struct fields over numeric indices.
6. Add function comments for helpers and exported functions:
   - `;; name : contract -> result`
   - `;;   Brief purpose sentence.`
7. Add inline comments for parameter types and optional/default parameters when relevant.
8. Use WebRacket file inclusion style with `include/reader ... read-syntax/skip-first-line` instead of module `require/provide`.
9. Add a comment header before blocks of constant definitions.
10. If a constant/definition name is not self-explanatory, add an end-of-line comment explaining its meaning/purpose.
11. When symbols are used for enumeration, use `case` instead of `cond` for branching.
12. When given rules, always ask whether they should be added to `AGENTS.md`.
13. At the top of each file, add a header comment in this form:
    - `;;;`
    - `;;; Title`
    - `;;;`
    - ``
    - `;; An explanation of the contents of the source file.`
14. For each file export, add a comment near the top (after the header and file explanation) with the export identifier and a one-line explanation; align the start of the explanations.
15. When something looks like a WebRacket bug, do not code around it. First make a minimal reproduction of the error, then ask whether to fix the compiler bug.
16. Assume primitives implemented in `runtime-wasm.rkt` are always available (unless shadowed by user code). Do not add workarounds for missing primitives in `web-easy`; investigate and fix the root cause instead.
17. `test/test-basics.rkt` outputs a large S-expression where `#t` means success and `#f` means failure; when reviewing results, pipe output through `tools/pretty.rkt` before analyzing failing entries.
18. Smoke tests in `smoke/test-browser-*.html` and parity pages must expose `#status` beginning with `PASS`/`FAIL`. Dashboard guard checks must fail on forbidden leaked attribute tokens (`#<value>`, `#<void>`, `#<procedure>`, `#<eof>`, `[object Object]`, `undefined`, `NaN`, `Infinity`), and guard self-test scripts are expected to exit `0` when that intentional failure is detected.
19. Prefer stylesheet-first CSS: emit semantic classes and attributes for theming; avoid renderer-owned inline visual styles unless a dynamic runtime value cannot be represented through classes/tokens.
20. Use `data-we-widget` as a stable semantic marker for each rendered widget root (for example `data-we-widget=\"dialog\"`) so user CSS/tests/tooling can target component type without relying on DOM shape.
21. User-facing example CSS files (especially external theme examples) must be commented and novice-readable: include section headers, explain token purpose, and provide brief customization guidance.
22. In beginner-facing CSS examples, add comments for non-obvious layout/stacking or visual-state rules (for example `z-index`, transparent border reservation, border-color seam fixes, and focus tint techniques) so readers understand why those rules exist.
23. For consecutive calls with the same callee and simple arguments, align argument columns to improve scanability.
24. CSS comments are required when adding non-obvious visual behavior (for example no-layout-shift active states, focus rendering, layering, or seam fixes). In beginner-facing stylesheets, always explain the rationale in plain language.
25. Use `navigation-bar` as the component/API name; avoid `navbar` naming to prevent confusion with HTML naming.
26. Prefer keyword-first constructor calls for DOM decoration:
    - use `#:class`, `#:id`, and `#:attrs` on constructors instead of decorator composition.
    - for direct constructor identifiers, use direct keyword calls.
    - for aliased/higher-order constructor values, use `call/key`.
27. For component calls where the first argument is a short title/label string (for example `card`, `group`, `menu`, `button`), keep that first string argument on the same line as the callee.
28. Theme-level typography and component styling rules must be defined in the theme stylesheet (general scope), not only in showcase/page-specific selectors; showcase CSS should only contain page-layout scaffolding.
