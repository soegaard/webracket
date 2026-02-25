# Hello World 1 (Node)

This example targets Node.js and runs in the terminal.

## Quick start (build + run)

From this folder:

```sh
racket ../../webracket.rkt -r hello-world-1.rkt
```

The switch -r causes webracket to compile and run the program in one go.
Node is used to run the program.

## Build then run

From this folder:

```sh
racket ../../webracket.rkt --node hello-world-1.rkt
```

The switch --node causes webracket to compile the program for Node.
That is, it generates WebAssembly bytecode and a small JavaScript
program for Node, that loads and runs the bytecode.

## Run

From this folder:

```sh
node --experimental-wasm-exnref --expose-gc hello-world-1.js
```

Build writes these files:

- `hello-world-1.wat`             (WebAssembly in text format)
- `hello-world-1.wasm`            (WebAssembly bytecode)
- `hello-world-1.wasm.map.sexp`   (Source map)
- `hello-world-1.js`              (JS used to invoke the WebAssembly program)

Only wasm and js files are needed by Node to run the program.

## Serve/Open

Not applicable (terminal example; no browser page).

Notes:

- Stdlib is enabled by default, so this example works as-is with `displayln`.
- For programs that do not use stdlib functionality, add `--no-stdlib`.
