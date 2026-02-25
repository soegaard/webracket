# Snake Examples

This folder contains multiple variants (`snake`, `snake2`, and an experimental `snake3`).

## Build

From this folder:

```sh
racket ../../webracket.rkt --ffi dom --ffi standard -b snake.rkt
racket ../../webracket.rkt --ffi dom --ffi standard -b snake2.rkt
```

## Serve

From this folder:

```sh
raco static-web
```

## Open

Open:

- `http://localhost:8000/snake.html`
- `http://localhost:8000/snake2.html`
