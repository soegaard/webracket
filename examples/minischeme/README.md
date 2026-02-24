# MiniScheme Example

## Build

From this folder:

```sh
racket ../../webracket.rkt --ffi standard --ffi xtermjs --ffi js --ffi dom --stdlib -b minischeme.rkt
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

- `http://localhost:8000/minischeme.html`
