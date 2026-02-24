# Xterm.js Demo

## Build

From this folder:

```sh
racket ../../webracket.rkt --ffi xtermjs --ffi dom --ffi standard --stdlib -b xtermjs-demo.rkt
```

Or from `examples/`:

```sh
./build.sh
```

## Serve

From this folder:

```sh
raco static-web
```

## Open

Open:

- `http://localhost:8000/xtermjs-demo.html`
