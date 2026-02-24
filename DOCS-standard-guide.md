# `js-send` Variants in `standard.ffi`

## What `js-send` Is Used For

`js-send` calls a JavaScript method on an external JavaScript object from WebRacket.

Conceptually:

```text
obj[methodName](...args)
```

In WebRacket, `args` are passed as a Racket vector.

## Concrete Example

```racket
(define arr (js-array/extern (vector 1 2 3)))
(js-send arr "join" (vector "-"))
;; => "1-2-3"
```

This calls JS:

```js
arr.join("-")
```

## Why There Are Several Variants

Different JS calls return different kinds of values, and callers need different conversion behavior:

- Sometimes you want a normal Racket value.
- Sometimes you want the raw JS object for further JS interop.
- Sometimes you want strict boolean validation.
- Sometimes you want JS truthiness collapsed to `#t/#f`.

The variants make that explicit, so behavior is predictable and type intent is visible in code.

## Variants

### `js-send`

Default method call. Returns a converted Racket value (same behavior as `js-send/value`).

```racket
(js-send arr "join" (vector "-"))
;; => "1-2-3"
```

### `js-send/value`

Method call with explicit "convert JS result to Racket value" behavior.

```racket
(js-send/value arr "slice" (vector 1))
;; => #(2 3)
```

### `js-send/extern`

Method call returning raw external JS value (no value conversion).

```racket
(define s (js-send/extern arr "join" (vector "-")))
(external? s)
;; => #t
```

Use this when you need to keep chaining JS operations on the same JS object/result.

### `js-send/extern/null`

Method call returning extern except when the JS result is `null`, which becomes `#f`.

```racket
(define obj (js-eval "({ f: () => null, g: () => 'ok' })"))
(js-send/extern/null obj "f" (vector))
;; => #f
(external-string->string (js-send/extern/null obj "g" (vector)))
;; => "ok"
```

### `js-send/extern/undefined`

Method call returning extern except when the JS result is `undefined`, which becomes `#f`.

```racket
(define obj (js-eval "({ f: () => undefined, g: () => 'ok' })"))
(js-send/extern/undefined obj "f" (vector))
;; => #f
(external-string->string (js-send/extern/undefined obj "g" (vector)))
;; => "ok"
```

### `js-send/extern/nullish`

Method call returning extern except when the JS result is `null` or `undefined`, both mapped to `#f`.

```racket
(define obj (js-eval "({ n: () => null, u: () => undefined, g: () => 'ok' })"))
(js-send/extern/nullish obj "n" (vector))
;; => #f
(js-send/extern/nullish obj "u" (vector))
;; => #f
(external-string->string (js-send/extern/nullish obj "g" (vector)))
;; => "ok"
```

### `js-send/boolean`

Method call expecting a boolean result. Accepts JS `true/false` and `Boolean` objects; raises an error for other result types.

```racket
(js-send/boolean arr "includes" (vector 2))
;; => #t

(js-send/boolean (js-Math) "abs" (vector -1.0))
;; => exn: js-send/boolean: expected boolean result, got number
```

### `js-send/truthy`

Method call using JS truthiness (`0`, `""`, `null`, `undefined`, etc. are falsey; most others truthy), returned as `#t/#f`.

```racket
(define obj (js-eval "({ z: () => 0, n: () => 42 })"))
(js-send/truthy obj "z" (vector))
;; => #f
(js-send/truthy obj "n" (vector))
;; => #t
```

## How to Choose

- Use `js-send` for normal interop where converted Racket values are what you want.
- Use `js-send/value` when you want to be explicit about conversion in code/readability.
- Use `js-send/extern` when the result must stay a JS external object.
- Use `js-send/extern/null` when `null` means "missing".
- Use `js-send/extern/undefined` when `undefined` means "missing".
- Use `js-send/extern/nullish` when either `null` or `undefined` should become `#f`.
- Use `js-send/boolean` when non-boolean results are a bug and should fail fast.
- Use `js-send/truthy` when JS truthiness semantics are intended.

## Common Uses

- Array/string helpers returning plain data:
  - `js-send` / `js-send/value`
- DOM/JS object chaining:
  - `js-send/extern`
- Optional-handle APIs:
  - `js-send/extern/null`
  - `js-send/extern/undefined`
  - `js-send/extern/nullish`
- Predicate-style JS APIs (e.g., `includes`, `has`, `matches`) where strictness matters:
  - `js-send/boolean`
- APIs where "is this value truthy?" is enough:
  - `js-send/truthy`
