# MathJax 4 Example

## Build

From this folder:

```sh
racket ../../webracket.rkt --ffi dom --ffi standard -b mathjax.rkt
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

- `http://localhost:8000/mathjax.html`
