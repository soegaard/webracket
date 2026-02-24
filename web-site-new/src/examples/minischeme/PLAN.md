Temporary limitations

- peek-bytes, peek-bytes!, peek-string, peek-string! currently do not accept the 2-argument form.
  Reason: the default argument is (current-input-port), which is not implemented in the runtime yet.

MiniScheme Roadmap

1. Stabilize semantics (quick, high value)
- Add tests for lexical scoping/shadowing, closure capture, and `set!` behavior across nested scopes.
- Add regression tests for define-then-use and recursive self-reference.
- Tighten error assertions (message patterns) for malformed forms.

2. Procedure model upgrades
- Implement variadic lambdas:
  - `(lambda x body ...)`
  - `(lambda (a b . rest) body ...)`
- Update application arity checks to support fixed + rest.
- Add tests for variadic calls and arity errors.

3. Quoting improvements
- Implement `quasiquote`, `unquote`, `unquote-splicing`.
- Restrict `unquote-splicing` to list contexts.
- Add nested quasiquote tests.

4. Control/binding forms (largest usability gain)
- Add `and`, `or` with short-circuit semantics.
- Add `cond` and `case`.
- Add `when`/`unless`.
- Add `let*` and `letrec` (or at least `let*` first).
- Add tests for evaluation order and recursive binding rules.

5. Primitive/library expansion
- Add commonly expected list and number utilities (`append`, `map`, `length`, `zero?`, `add1`, etc.).
- Add `apply` to improve functional expressiveness.
- Keep primitive set explicitly listed and tested.

6. Reader/evaluator integration hardening (done)
- Keep MiniScheme using `read`.
- Add tests with multiline programs, comments, quote forms, and dotted pairs through full `process-input`.

7. Error model and diagnostics (done)
- Standardize error prefixes (`=> read error:` vs eval errors).
- Include offending form/operator in messages where possible.
- Add malformed-form test matrix for each special form.

8. Optional advanced features (later, in progress)
- Multiple values (`values`, `call-with-values`) support. (done)
- Lightweight module-like loading for scripts.
- Better REPL UX features (history helpers, pretty printing).
- Integration/stability validation after multiple-values changes: tests + website rebuild. (done)

Suggested execution order
1. Variadic lambdas
2. `and`/`or` + `let*`
3. `cond` + `when`/`unless`
4. `quasiquote` suite
5. Primitive expansion
