<INSTRUCTIONS>
# AGENTS.md (web-site/)

These conventions apply when working in `web-site/`.

## Racket Formatting & Syntax
- Always count delimiters after edits: `()` `[]` `{}` must match separately.
- Use `[]` for `cond` clauses.
- Use `[]` in `let*` binding clauses.
- Avoid `letrec` unless explicitly requested; prefer internal `define` when appropriate.

## WebRacket Constraints
- Do not introduce JavaScript implementations for page behavior. Use WebRacket/DOM bindings instead.
- If using debug logging, keep it minimal and remove it when done.

## Debugging
- Prefer small, focused instrumentation that can be removed later.
- Confirm hypotheses with a minimal test program before changing core logic.

## DOM Id Normalization (Prevention)
- Normalize any DOM-provided id or `data-*` value at the boundary: if `external?` then convert with `external-string->string`.
- Convert normalized ids to symbols and use `make-hasheq` for lookup maps; do not mix string and symbol keys.
- Never compare or hash raw `external?` values; normalize first.
- Add/keep a small test (e.g. `test-toc.rkt`) that verifies the normalization and lookup path.

## Docs-Only Build Gotchas
- `documentation.rkt` is shared by `web-site.rkt` and `website-docs-only.rkt`; changes affect both builds.
- `website-docs-only.rkt` intentionally omits example includes/data to keep builds fast; avoid reintroducing them.
- Use the correct build script: `build.sh` for full site, `build-docs-only.sh` for docs-only.
- Hard-reload after rebuilds to avoid stale `web-site.wasm` in the browser cache.
</INSTRUCTIONS>
