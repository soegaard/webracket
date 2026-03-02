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
16. `test/test-basics.rkt` outputs a large S-expression where `#t` means success and `#f` means failure; when reviewing results, pipe output through `tools/pretty.rkt` before analyzing failing entries.
