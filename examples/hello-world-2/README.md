# Hello World 2 Example

## Build

From this folder:

```sh
racket ../../webracket.rkt --ffi dom --ffi standard -b hello-world.rkt
```

## Serve

From this folder:

```sh
raco static-web
```

## Open

Open:

- `http://localhost:8000/hello-world.html`

This example also calls `(js-log "Hello!")`, which prints `Hello!` in the browser's JavaScript console.
