# Hello World 2 Example

## Build

From this folder:

```sh
racket ../../webracket.rkt --ffi dom --ffi standard --stdlib -b hello-world.rkt
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

- `http://localhost:8000/hello-world.html`
