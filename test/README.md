Testing
-------

This folder contains the test suite.

The most important test file is `test-basics.rkt`.
These basic tests need to work without the standard library,
so compile this file with `--no-stdlib`.

If the current directory is `test/` then this command will
make the `webracket` command, run the tests, and format them.

raco make ../webracket.rkt && racket -l errortrace -t ../webracket.rkt -- --no-stdlib -r test-basics.rkt | racket ../tools/pretty.rkt

The tool `pretty.rkt` simply pretty prints an s-expression read from standard input.

DOM wrapper tests
-----------------

The DOM split now has a small raw smoke test and a few family-focused wrapper tests:

- `test-dom.rkt` for the raw `ffi/dom.ffi` aggregate
- `test-dom-window-document.rkt` for `window`, `document`, and `element`
- `test-dom-canvas-media-image.rkt` for `canvas`, `domrect`, `media`, and `image`
- `test-dom-event.rkt` for `event`
