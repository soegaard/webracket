# AGENTS.md (scribblings)

This file defines documentation-writing rules for files under `scribblings/`.

## Scope

- This file applies only to documentation work in `scribblings/`.
- Rules marked **[Bindings chapter only]** apply only to the `Browser API` chapter and its binding entries.

## General Documentation Rules

- Treat documentation files as concurrently edited by other threads.
- Re-read the current file state before each edit.
- Keep wording concise and concrete.
- When editing text, run a grammar/spell pass and re-render docs.
- In reference sections, use Racket Reference-style entries with `@defproc`.

## Grammar and Render Checks

- Grammar/spell check command:
  - `aspell --lang=en_US --mode=tex list < scribblings/webracket.scrbl`
- Render command:
  - `raco scribble --htmls --dest html/ scribblings/webracket.scrbl`

## Bindings Documentation Rules **[Bindings chapter only]**

- Use Racket Reference-style entries with `@defproc`.
- Keep a one-line description of what each binding does.
- Include an explicit return sentence when return behavior is not obvious from the documented return type.
- Omit the return sentence when the return type already makes it obvious (for example `void?`).
- Include an `MDN:` line for bindings that map to MDN-documented APIs.
- Place the `MDN:` line above the source-signature line.
- Include a compact source-signature line in this format:
  - `name : args -> return     [dom.ffi]`
- Ensure displayed signatures align with the corresponding `.ffi` declaration.

## Return Type Presentation **[Bindings chapter only]**

Use user-facing return descriptions in entries as follows:

- `extern/raw`       -> `external`
- `extern`           -> `#f or external`              (for example `(or/c #f external)`)
- `extern/undefined` -> `undefined or external`       (for example `(or/c undefined external)`)
- `extern/nullish`   -> `#f or undefined or external` (for example `(or/c #f undefined external)`)

## MDN Link Helper **[Bindings chapter only]**

Use the shared helper in `webracket.scrbl` for MDN URLs:

- `@(define (mdn path [label #f]) ...)`

Prefer stable API pages such as:

- `Document`
- `Document/createElement`
- `Document/getElementById`
- `Node/appendChild`
