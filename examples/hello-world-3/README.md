# Hello World 3 Example

This browser example uses SXML via `sxml->dom` to build the page without direct DOM construction calls.

## Build

From this folder:

```sh
racket ../../webracket.rkt --ffi dom --ffi standard --stdlib -b hello-world-3.rkt
```

## Serve

From this folder:

```sh
raco static-web
```

## Open

Open:

- `http://localhost:8000/hello-world-3.html`

This example also calls `(js-log "Hello!")`, which prints `Hello!` in the browser's JavaScript console.
