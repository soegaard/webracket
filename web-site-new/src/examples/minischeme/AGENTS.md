# MiniScheme Notes

- Source of truth:
  - Interpreter: `web-site/src/examples/minischeme/minischeme.rkt`
  - Page wiring: `web-site/src/examples/minischeme-page.rkt`
  - Do not hand-edit generated files: `web-site/src/minischeme.html`, `web-site/public/minischeme.html`

- Project intent:
  - The MiniScheme example is part of testing WebRacket itself.
  - Do not code around WebRacket bugs or limitations in MiniScheme.
  - If you find a WebRacket bug/limitation, alert the user and isolate/reproduce it so it can be fixed in WebRacket.

- Tests:
  - When adding or updating MiniScheme tests, add them in both:
    - `web-site/src/examples/minischeme/test-minischeme.rkt` (host Racket run)
    - `web-site/src/examples/minischeme/test-minischeme-with-webracket.rkt` (compiled WebRacket `-r` run)
  - Conventions for `web-site/src/examples/minischeme/test-minischeme.rkt`:
    - Prefer one assertion per `test-case` (do not group many `check-equal?` assertions in one `test-case`).
    - Keep test names aligned with `test-minischeme-with-webracket.rkt` where practical.
    - Keep host and WebRacket test counts aligned unless there is an explicit reason not to.

- Canonical test commands:
  - `racket web-site/src/examples/minischeme/test-minischeme.rkt`
  - `(cd web-site/src/examples/minischeme && racket ../../../../webracket.rkt -r test-minischeme-with-webracket.rkt)`

- Known expected WebRacket test failures:
  - None currently. Treat any WebRacket-run failure as a regression until triaged.

- Rebuild after MiniScheme/page changes:
  - `(cd web-site/src && ./build.sh)`
  - Note: a final audio error from `afplay` can be ignored.

- Behavior contract:
  - Top-level multiple values are rendered as multiple lines (`=> v1`, `=> v2`, ...), not as an eval error.

- Reader-related changes:
  - If changing reader behavior, add paired tests in both MiniScheme test files above.
