# Reference: `standard.ffi` 

## Chapter 1 — Introduction

This document describes the JavaScript FFI exported by `ffi/standard.ffi` in WebRacket.

What this API gives you:
- Access to JavaScript globals (`Math`, `Date`, `Intl`, typed arrays, etc.).
- Property reads/writes and method calls on external JS objects.
- Controlled conversion between JS values and WebRacket values.
- Number helpers that mirror JavaScript `Number` static/instance behavior.

All functions below include:
- input/output types
- a concrete example call
- a short “use when” note
- a link on the function name to the corresponding MDN documentation page

Assumption in examples: the program is compiled with `--ffi standard`.

### Table of Contents

- [Chapter 1 — Introduction](#chapter-1--introduction)
- [Type Legend](#type-legend)
- [Conversion Rules](#conversion-rules)
- [Chapter 2 — Core Interop Functions](#chapter-2--core-interop-functions)
- [2.1 Value Properties](#21-value-properties)
- [2.2 Value Predicates](#22-value-predicates)
- [2.3 Global Function Properties](#23-global-function-properties)
- [2.4 Variables, Properties, and Indexing](#24-variables-properties-and-indexing)
- [2.5 Construction and Object Helpers](#25-construction-and-object-helpers)
- [2.6 Method Calls](#26-method-calls)
- [2.7 Type and Operator Helpers](#27-type-and-operator-helpers)
- [2.8 Legacy Wrappers](#28-legacy-wrappers)
- [2.9 Mini Workflows](#29-mini-workflows)
- [Chapter 3 — Number Module](#chapter-3--number-module)
- [3.1 Constants](#31-constants)
- [3.2 Predicates](#32-predicates)
- [3.3 Parsing and Formatting](#33-parsing-and-formatting)
- [Chapter 4 — Global Constructors and Namespaces](#chapter-4--global-constructors-and-namespaces)
- [4.0 Environment Availability](#40-environment-availability)
- [4.1 Fundamental](#41-fundamental)
- [4.2 Error Types](#42-error-types)
- [4.3 Numbers, Dates, and Text](#43-numbers-dates-and-text)
- [4.4 Indexed Collections and Typed Arrays](#44-indexed-collections-and-typed-arrays)
- [4.5 Keyed Collections](#45-keyed-collections)
- [4.6 Structured Data](#46-structured-data)
- [4.7 Memory Management](#47-memory-management)
- [4.8 Control Abstractions](#48-control-abstractions)
- [4.9 Reflection](#49-reflection)
- [4.10 Internationalization](#410-internationalization)
- [Chapter 5 — Choosing the Right Function Quickly](#chapter-5--choosing-the-right-function-quickly)
- [Chapter 6 — Coverage Checklist](#chapter-6--coverage-checklist)

### Type Legend

| Type | Meaning |
|---|---|
| `(extern)` | External JavaScript value/object reference. In return position, JS `null` maps to `#f`. |
| `(extern/raw)` | Raw JavaScript return value/object reference (no `null`/`undefined` mapping). |
| `(extern/undefined)` | External JavaScript return where JS `undefined` maps to `#f`. |
| `(extern/nullish)` | External JavaScript return where JS `null` or `undefined` maps to `#f`. |
| `(value)` | WebRacket value; JS results/inputs are converted through the FFI value bridge. |
| `(boolean)` | WebRacket boolean (`#t` / `#f`). |
| `(i32)` | 32-bit integer (often legacy 0/1 booleans in older wrappers). |
| `(f64)` | Double-precision floating-point number. |
| `(string/symbol)` | Either a Racket string or symbol (typically property/method name). |
| `()` | No arguments (in input position) or no return value / void (in output position). |

### Conversion Rules

#### JS result converted to WebRacket (`js-send/value`)

```racket
(define arr (js-array/extern (vector 1 2 3)))
(js-send/value arr "slice" (vector 1))
;; => #(2 3)
```

#### Keep raw JS external value (`js-send/extern`)

```racket
(define arr (js-array/extern (vector 1 2 3)))
(define x (js-send/extern arr "slice" (vector 1)))
(external? x)
;; => #t
```

#### Strict boolean vs truthiness (`js-send/boolean` vs `js-send/truthy`)

```racket
(define obj (js-eval "({ a: () => true, b: () => 1 })"))
(js-send/boolean obj "a" (vector))
;; => #t
(js-send/truthy obj "b" (vector))
;; => #t
;; (js-send/boolean obj "b" (vector)) raises, because result is number, not boolean
```

## Chapter 2 — Core Interop Functions

These are the functions you will use most in application code.

### 2.1 Value Properties

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-global-this`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/globalThis) | `()` | `(extern/raw)` | `(js-global-this)` | you need the JS global object as an external value. |
| [`js-infinity`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Infinity) | `()` | `(f64)` | `(js-infinity)` | you want JS `Infinity` as a flonum. |
| [`js-nan`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NaN) | `()` | `(f64)` | `(js-nan)` | you want JS `NaN` as a flonum. |
| [`js-null`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/null) | `()` | `(extern/raw)` | `(js-null)` | an API needs literal JS `null`. |
| [`js-undefined`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/undefined) | `()` | `(extern/raw)` | `(js-undefined)` | an API needs literal JS `undefined`. |

### 2.2 Value Predicates

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-null?`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/null) | `(extern)` | `(boolean)` | `(js-null? (js-null))` | you need a strict null test (`null` only). |
| [`js-undefined?`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/undefined) | `(extern)` | `(boolean)` | `(js-undefined? (js-undefined))` | you need a strict undefined test (`undefined` only). |
| [`js-nullish?`](https://developer.mozilla.org/en-US/docs/Glossary/Nullish) | `(extern)` | `(boolean)` | `(js-nullish? (js-undefined))` | you need a combined null-or-undefined test. |
| [`js-truthy?`](https://developer.mozilla.org/en-US/docs/Glossary/Truthy) | `(extern)` | `(boolean)` | `(js-truthy? (js-eval "0"))` | you need JS truthiness (`Boolean(x)`) as `#t/#f`. |
| [`js-finite?`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/isFinite) | `(f64)` | `(boolean)` | `(js-finite? 42.0)` | you need `isFinite` with a boolean result. |
| [`js-nan?`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/isNaN) | `(f64)` | `(boolean)` | `(js-nan? +nan.0)` | you need `isNaN` with a boolean result. |

In JavaScript, a truthy value is a value that is considered true in a Boolean context.
All values are truthy unless they are falsy.
Falsy values are `false`, `0`, `-0`, `0n`, `""`, `null`, `undefined`, `NaN`, and `document.all`.

### 2.3 Global Function Properties

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-eval`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/eval) | `(string)` | `(extern/raw)` | `(js-eval "({x: 10})")` | you must evaluate JS source dynamically. |
| [`js-parse-float`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/parseFloat) | `(string)` | `(f64)` | `(js-parse-float "3.14abc")` | you want JS global `parseFloat` behavior. |
| [`js-parse-int`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/parseInt) | `(string)` | `(f64)` | `(js-parse-int "42")` | you want JS global `parseInt` behavior. |
| [`js-decode-uri`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/decodeURI) | `(string)` | `(string)` | `(js-decode-uri "https://a.test/?q=a%20b")` | you decode a full URI string. |
| [`js-decode-uri-component`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/decodeURIComponent) | `(string)` | `(string)` | `(js-decode-uri-component "a%20b%2Bc")` | you decode one URI component. |
| [`js-encode-uri`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURI) | `(string)` | `(string)` | `(js-encode-uri "https://a.test/?q=a b")` | you encode a full URI string. |
| [`js-encode-uri-component`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent) | `(string)` | `(string)` | `(js-encode-uri-component "a b+c")` | you encode one URI component. |

### 2.4 Variables, Properties, and Indexing

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-var`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/globalThis) | `(string)` | `(extern/raw)` | `(js-var "Math")` | you need `globalThis[name]`. |
| [`js-ref/extern`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Property_accessors) | `(extern string/symbol)` | `(extern/raw)` | `(js-ref/extern (js-var "window") "document")` | you need a raw JS property value as extern. |
| [`js-ref`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Property_accessors) | `(extern string/symbol)` | `(value)` | `(js-ref (js-eval "({a:1})") "a")` | you want a property converted to a WebRacket value. |
| [`js-set!`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Assignment) | `(extern value value)` | `()` | `(js-set! (js-eval "({})") "x" 10)` | you need `obj[key] = value`. |
| [`js-index`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Property_accessors) | `(extern value)` | `(value)` | `(js-index (js-array/extern (vector 10 20)) 1)` | you read an indexed value and want conversion. |
| [`js-assign!`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/globalThis) | `(string/symbol value)` | `()` | `(js-assign! "tmpValue" 123)` | you assign to `globalThis[name]`. |

### 2.5 Construction and Object Helpers

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-new`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/new) | `(extern value)` | `(extern/raw)` | `(js-new (js-Date) (vector))` | you need `new Ctor(...args)`. |
| [`js-throw`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/throw) | `(value)` | `()` | `(js-throw "boom")` | you need to throw a JS exception. |
| [`js-this`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/this) | `()` | `(extern/raw)` | `(js-this)` | you need the current JS `this` (callback contexts). |
| [`js-object`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Object_initializer) | `(value)` | `(extern/raw)` | `(js-object (vector (vector "x" 1) (vector "y" 2)))` | you need a JS object literal from key/value pairs. |
| [`js-array`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array) | `(value)` | `(value)` | `(js-array (vector 1 2 3))` | you need a JS array result converted to a WebRacket value. |
| [`js-array/extern`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array) | `(value)` | `(extern/raw)` | `(js-array/extern (vector 1 2 3))` | you need a raw external JS array. |
| [`js-Array`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array) | `(value)` | `(extern/raw)` | `(js-Array)` | you need the JS `Array` constructor reference. |


### 2.6 Method Calls

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-send`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Property_accessors) | `(extern string/symbol value)` | `(value)` | `(js-send (js-array/extern (vector 1 2 3)) "join" (vector "-"))` | default method call with value conversion. |
| [`js-send/extern`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Property_accessors) | `(extern string/symbol value)` | `(extern/raw)` | `(js-send/extern (js-array/extern (vector 1 2 3)) "slice" (vector 1))` | you need raw external method result. |
| [`js-send/extern/null`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Property_accessors) | `(extern string/symbol value)` | `(extern)` | `(js-send/extern/null (js-eval "({f:()=>null})") "f" (vector))` | `null` should be treated as missing (`#f`). |
| [`js-send/extern/undefined`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Property_accessors) | `(extern string/symbol value)` | `(extern/undefined)` | `(js-send/extern/undefined (js-eval "({f:()=>undefined})") "f" (vector))` | `undefined` should be treated as missing (`#f`). |
| [`js-send/extern/nullish`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Property_accessors) | `(extern string/symbol value)` | `(extern/nullish)` | `(js-send/extern/nullish (js-eval "({f:()=>null})") "f" (vector))` | both `null` and `undefined` should map to `#f`. |
| [`js-send/value`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Property_accessors) | `(extern string/symbol value)` | `(value)` | `(js-send/value (js-array/extern (vector 1 2 3)) "slice" (vector 1))` | you want explicit value-converting method call. |
| [`js-send/boolean`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Property_accessors) | `(extern string/symbol value)` | `(boolean)` | `(js-send/boolean (js-array/extern (vector 1 2 3)) "includes" (vector 2))` | the method must return a boolean; fail otherwise. |
| [`js-send/truthy`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Property_accessors) | `(extern string/symbol value)` | `(boolean)` | `(js-send/truthy (js-eval "({f:()=>0})") "f" (vector))` | you want JS truthiness mapped to `#t/#f`. |
| [`js-send/flonum`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Property_accessors) | `(extern string/symbol value)` | `(f64)` | `(js-send/flonum (js-Math) "abs" (vector -1.0))` | you expect a numeric result as flonum. |

Notes:
- `send/extern` => raw JS result
- `send/extern/null` => `null` becomes `#f`; other results stay extern
- `send/extern/undefined` => `undefined` becomes `#f`; other results stay extern
- `send/extern/nullish` => `null`/`undefined` become `#f`; other results stay extern
- `send/value` => JS result converted via FASL
- `send/boolean` => strict boolean result; throws on non-boolean
- `send/truthy` => JS truthiness to `#t`/`#f`
- default `send` mirrors `send/value`

Error behavior:
- `js-send/boolean` raises if the JS method result is not boolean.
- JS exceptions thrown by a method call cross the FFI boundary and can be handled with `with-handlers`.

```racket
(with-handlers ([exn? (lambda (e) 'caught)])
  (js-send/extern (js-array/extern (vector 1 2 3)) "notAMethod" (vector))
  'unreachable)
;; => 'caught
```


### 2.7 Type and Operator Helpers

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-typeof`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/typeof) | `(extern)` | `(string)` | `(js-typeof (js-var "Math"))` | you need JS `typeof` classification. |
| [`js-value->string`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/String) | `(extern)` | `(string)` | `(js-value->string (js-send/extern (js-array/extern (vector 1 2)) "join" (vector ",")))` | you need JS string coercion for an extern value. |
| [`js-instanceof`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/instanceof) | `(extern extern)` | `(boolean)` | `(js-instanceof (js-array/extern (vector)) (js-ref (js-global-this) "Array"))` | you need JS `instanceof` check. |
| [`js-operator`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators) | `(string/symbol value)` | `(extern/raw)` | `(js-operator "+" (vector 1 2))` | you need dynamic operator application. |

### 2.8 Legacy Wrappers

The following wrappers are still available in `standard.ffi`, but intentionally omitted from the primary tables:

- `js-is-finite` (returns `(i32)` 0/1)
- `js-is-nan` (returns `(i32)` 0/1)

Prefer `js-finite?` and `js-nan?` in new code.

### 2.9 Mini Workflows

#### Get global + call method + read property

```racket
(define Date (js-var "Date"))
(define d (js-new Date (vector)))
(define _year (js-send d "getUTCFullYear" (vector)))
(define ctor-name (js-ref (js-ref/extern d "constructor") "name"))
;; ctor-name => "Date"
```

#### Construct object and pass options

```racket
(define opts (js-object (vector (vector "mode" "cors")
                                (vector "cache" "no-store"))))
(define keys (js-send (js-Object) "keys" (vector opts)))
;; keys => #("mode" "cache")
```

#### Typed array interop

```racket
(define u8 (js-new (js-Uint8Array) (vector 3)))
(js-send u8 "set" (vector (js-array/extern (vector 7 8 9)) 0))
(list (js-index u8 0) (js-index u8 2))
;; => '(7 9)
```

## Chapter 3 — Number Module

These helpers mirror JavaScript `Number` constants and methods.

### 3.1 Constants

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-number-epsilon`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/EPSILON) | `()` | `(f64)` | `(js-number-epsilon)` | you need machine epsilon for JS numbers. |
| [`js-number-max-safe-integer`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MAX_SAFE_INTEGER) | `()` | `(f64)` | `(js-number-max-safe-integer)` | you need max exact integer in JS. |
| [`js-number-max-value`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MAX_VALUE) | `()` | `(f64)` | `(js-number-max-value)` | you need max finite f64. |
| [`js-number-min-safe-integer`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MIN_SAFE_INTEGER) | `()` | `(f64)` | `(js-number-min-safe-integer)` | you need min exact integer in JS. |
| [`js-number-min-value`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MIN_VALUE) | `()` | `(f64)` | `(js-number-min-value)` | you need smallest positive non-zero f64. |
| [`js-number-nan`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/NaN) | `()` | `(f64)` | `(js-number-nan)` | you need the Number NaN constant. |
| [`js-number-negative-infinity`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/NEGATIVE_INFINITY) | `()` | `(f64)` | `(js-number-negative-infinity)` | you need negative infinity constant. |
| [`js-number-positive-infinity`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/POSITIVE_INFINITY) | `()` | `(f64)` | `(js-number-positive-infinity)` | you need positive infinity constant. |

### 3.2 Predicates

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-number-finite?`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/isFinite) | `(f64)` | `(boolean)` | `(js-number-finite? 10.0)` | you need strict `Number.isFinite` behavior. |
| [`js-number-integer?`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/isInteger) | `(f64)` | `(boolean)` | `(js-number-integer? 10.0)` | you need strict `Number.isInteger` behavior. |
| [`js-number-nan?`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/isNaN) | `(f64)` | `(boolean)` | `(js-number-nan? +nan.0)` | you need strict `Number.isNaN` behavior. |
| [`js-number-safe-integer?`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/isSafeInteger) | `(f64)` | `(boolean)` | `(js-number-safe-integer? 9007199254740991.0)` | you need strict `Number.isSafeInteger` behavior. |

### 3.3 Parsing and Formatting

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-number-parse-float`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/parseFloat) | `(string)` | `(f64)` | `(js-number-parse-float "3.14x")` | you want Number.parseFloat behavior. |
| [`js-number-parse-int`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/parseInt) | `(string)` | `(f64)` | `(js-number-parse-int "42")` | you want Number.parseInt behavior. |
| [`js-number-to-exponential`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toExponential) | `(f64 value)` | `(string)` | `(js-number-to-exponential 1234.5 2)` | you need exponential string formatting. |
| [`js-number-to-fixed`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed) | `(f64 value)` | `(string)` | `(js-number-to-fixed 3.14159 2)` | you need fixed-point string formatting. |
| [`js-number-to-locale-string`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toLocaleString) | `(f64 value value)` | `(string)` | `(js-number-to-locale-string 12345.67 "en-US" (void))` | you need locale-aware number formatting. |
| [`js-number-to-precision`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toPrecision) | `(f64 value)` | `(string)` | `(js-number-to-precision 3.14159 4)` | you need significant-digits formatting. |
| [`js-number-to-string`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toString) | `(f64 value)` | `(string)` | `(js-number-to-string 255.0 16)` | you need radix-aware number-to-string conversion. |
| [`js-number-value-of`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/valueOf) | `(f64)` | `(f64)` | `(js-number-value-of 42.0)` | you need primitive numeric value extraction. |

Tip: where the wrapper accepts optional JS args, pass `(void)` for omitted ones.

## Chapter 4 — Global Constructors and Namespaces

These functions return references to JS global constructors/objects. Typical next step: `js-new`, `js-send`, or `js-ref`.

### 4.0 Environment Availability

Some globals depend on the JavaScript runtime (browser vs Node.js vs embedded host).
In those cases, WebRacket exposes missing globals as `undefined`.

Use feature detection before calling methods on runtime-dependent globals:

```racket
(define temporal (js-Temporal))
(unless (js-undefined? temporal)
  (displayln (js-typeof temporal)))
```

For constructor-like globals, a safe check is:

```racket
(define gf (js-GeneratorFunction))
(when (and (not (js-undefined? gf))
           (equal? (js-typeof gf) "function"))
  (displayln "GeneratorFunction is available"))
```

### 4.1 Fundamental

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-Object`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object) | `()` | `(extern/raw)` | `(js-typeof (js-Object))` | you need the JS global reference (`Object`). |
| [`js-Function`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function) | `()` | `(extern/raw)` | `(js-typeof (js-Function))` | you need the JS global reference (`Function`). |
| [`js-Boolean`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Boolean) | `()` | `(extern/raw)` | `(js-typeof (js-Boolean))` | you need the JS global reference (`Boolean`). |
| [`js-Symbol`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol) | `()` | `(extern/raw)` | `(js-typeof (js-Symbol))` | you need the JS global reference (`Symbol`). |

### 4.2 Error Types

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-Error`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error) | `()` | `(extern/raw)` | `(js-typeof (js-Error))` | you need the constructor for a specific JS error type (`Error`). |
| [`js-AggregateError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/AggregateError) | `()` | `(extern/raw)` | `(js-typeof (js-AggregateError))` | you need the constructor for a specific JS error type (`AggregateError`). |
| [`js-EvalError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/EvalError) | `()` | `(extern/raw)` | `(js-typeof (js-EvalError))` | you need the constructor for a specific JS error type (`EvalError`). |
| [`js-RangeError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RangeError) | `()` | `(extern/raw)` | `(js-typeof (js-RangeError))` | you need the constructor for a specific JS error type (`RangeError`). |
| [`js-ReferenceError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ReferenceError) | `()` | `(extern/raw)` | `(js-typeof (js-ReferenceError))` | you need the constructor for a specific JS error type (`ReferenceError`). |
| [`js-SuppressedError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SuppressedError) | `()` | `(extern/raw)` | `(js-typeof (js-SuppressedError))` | you need the constructor for a specific JS error type (`SuppressedError`). |
| [`js-SyntaxError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SyntaxError) | `()` | `(extern/raw)` | `(js-typeof (js-SyntaxError))` | you need the constructor for a specific JS error type (`SyntaxError`). |
| [`js-TypeError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypeError) | `()` | `(extern/raw)` | `(js-typeof (js-TypeError))` | you need the constructor for a specific JS error type (`TypeError`). |
| [`js-URIError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/URIError) | `()` | `(extern/raw)` | `(js-typeof (js-URIError))` | you need the constructor for a specific JS error type (`URIError`). |
| [`js-InternalError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/InternalError) | `()` | `(extern/raw)` | `(js-typeof (js-InternalError))` | you need the constructor for a specific JS error type (`InternalError`). |

### 4.3 Numbers, Dates, and Text

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-Number`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number) | `()` | `(extern/raw)` | `(js-typeof (js-Number))` | you need the constructor/namespace (`Number`). |
| [`js-BigInt`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt) | `()` | `(extern/raw)` | `(js-typeof (js-BigInt))` | you need the constructor/namespace (`BigInt`). |
| [`js-Math`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math) | `()` | `(extern/raw)` | `(js-typeof (js-Math))` | you need the constructor/namespace (`Math`). |
| [`js-Date`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date) | `()` | `(extern/raw)` | `(js-typeof (js-Date))` | you need the constructor/namespace (`Date`). |
| [`js-Temporal`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal) | `()` | `(extern/raw)` | `(js-typeof (js-Temporal))` | you need the constructor/namespace (`Temporal`). |
| [`js-String`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String) | `()` | `(extern/raw)` | `(js-typeof (js-String))` | you need the constructor/namespace (`String`). |
| [`js-RegExp`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp) | `()` | `(extern/raw)` | `(js-typeof (js-RegExp))` | you need the constructor/namespace (`RegExp`). |

### 4.4 Indexed Collections and Typed Arrays

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-TypedArray`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray) | `()` | `(extern/raw)` | `(js-typeof (js-TypedArray))` | you need the typed-array global (`TypedArray`). |
| [`js-Int8Array`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int8Array) | `()` | `(extern/raw)` | `(js-typeof (js-Int8Array))` | you need the typed-array global (`Int8Array`). |
| [`js-Uint8Array`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8Array) | `()` | `(extern/raw)` | `(js-typeof (js-Uint8Array))` | you need the typed-array global (`Uint8Array`). |
| [`js-Uint8ClampedArray`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8ClampedArray) | `()` | `(extern/raw)` | `(js-typeof (js-Uint8ClampedArray))` | you need the typed-array global (`Uint8ClampedArray`). |
| [`js-Int16Array`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int16Array) | `()` | `(extern/raw)` | `(js-typeof (js-Int16Array))` | you need the typed-array global (`Int16Array`). |
| [`js-Uint16Array`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint16Array) | `()` | `(extern/raw)` | `(js-typeof (js-Uint16Array))` | you need the typed-array global (`Uint16Array`). |
| [`js-Int32Array`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array) | `()` | `(extern/raw)` | `(js-typeof (js-Int32Array))` | you need the typed-array global (`Int32Array`). |
| [`js-Uint32Array`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint32Array) | `()` | `(extern/raw)` | `(js-typeof (js-Uint32Array))` | you need the typed-array global (`Uint32Array`). |
| [`js-BigInt64Array`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt64Array) | `()` | `(extern/raw)` | `(js-typeof (js-BigInt64Array))` | you need the typed-array global (`BigInt64Array`). |
| [`js-BigUint64Array`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigUint64Array) | `()` | `(extern/raw)` | `(js-typeof (js-BigUint64Array))` | you need the typed-array global (`BigUint64Array`). |
| [`js-Float16Array`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float16Array) | `()` | `(extern/raw)` | `(js-typeof (js-Float16Array))` | you need the typed-array global (`Float16Array`). |
| [`js-Float32Array`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array) | `()` | `(extern/raw)` | `(js-typeof (js-Float32Array))` | you need the typed-array global (`Float32Array`). |
| [`js-Float64Array`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array) | `()` | `(extern/raw)` | `(js-typeof (js-Float64Array))` | you need the typed-array global (`Float64Array`). |

### 4.5 Keyed Collections

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-Map`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map) | `()` | `(extern/raw)` | `(js-typeof (js-Map))` | you need a keyed-collection constructor (`Map`). |
| [`js-Set`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set) | `()` | `(extern/raw)` | `(js-typeof (js-Set))` | you need a keyed-collection constructor (`Set`). |
| [`js-WeakMap`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WeakMap) | `()` | `(extern/raw)` | `(js-typeof (js-WeakMap))` | you need a keyed-collection constructor (`WeakMap`). |
| [`js-WeakSet`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WeakSet) | `()` | `(extern/raw)` | `(js-typeof (js-WeakSet))` | you need a keyed-collection constructor (`WeakSet`). |

### 4.6 Structured Data

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-ArrayBuffer`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer) | `()` | `(extern/raw)` | `(js-typeof (js-ArrayBuffer))` | you need the structured-data API object/constructor (`ArrayBuffer`). |
| [`js-SharedArrayBuffer`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer) | `()` | `(extern/raw)` | `(js-typeof (js-SharedArrayBuffer))` | you need the structured-data API object/constructor (`SharedArrayBuffer`). |
| [`js-DataView`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView) | `()` | `(extern/raw)` | `(js-typeof (js-DataView))` | you need the structured-data API object/constructor (`DataView`). |
| [`js-Atomics`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Atomics) | `()` | `(extern/raw)` | `(js-typeof (js-Atomics))` | you need the structured-data API object/constructor (`Atomics`). |
| [`js-JSON`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON) | `()` | `(extern/raw)` | `(js-typeof (js-JSON))` | you need the structured-data API object/constructor (`JSON`). |

### 4.7 Memory Management

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-WeakRef`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WeakRef) | `()` | `(extern/raw)` | `(js-typeof (js-WeakRef))` | you need the memory-management constructor (`WeakRef`). |
| [`js-FinalizationRegistry`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/FinalizationRegistry) | `()` | `(extern/raw)` | `(js-typeof (js-FinalizationRegistry))` | you need the memory-management constructor (`FinalizationRegistry`). |

### 4.8 Control Abstractions

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-Iterator`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator) | `()` | `(extern/raw)` | `(js-typeof (js-Iterator))` | you need the control-abstraction global (`Iterator`). |
| [`js-AsyncIterator`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/AsyncIterator) | `()` | `(extern/raw)` | `(js-typeof (js-AsyncIterator))` | you need the control-abstraction global (`AsyncIterator`). |
| [`js-Promise`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise) | `()` | `(extern/raw)` | `(js-typeof (js-Promise))` | you need the control-abstraction global (`Promise`). |
| [`js-GeneratorFunction`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/GeneratorFunction) | `()` | `(extern/raw)` | `(js-typeof (js-GeneratorFunction))` | you need the control-abstraction global (`GeneratorFunction`). |
| [`js-AsyncGeneratorFunction`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/AsyncGeneratorFunction) | `()` | `(extern/raw)` | `(js-typeof (js-AsyncGeneratorFunction))` | you need the control-abstraction global (`AsyncGeneratorFunction`). |
| [`js-Generator`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Generator) | `()` | `(extern/raw)` | `(js-typeof (js-Generator))` | you need the control-abstraction global (`Generator`). |
| [`js-AsyncGenerator`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/AsyncGenerator) | `()` | `(extern/raw)` | `(js-typeof (js-AsyncGenerator))` | you need the control-abstraction global (`AsyncGenerator`). |
| [`js-AsyncFunction`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/AsyncFunction) | `()` | `(extern/raw)` | `(js-typeof (js-AsyncFunction))` | you need the control-abstraction global (`AsyncFunction`). |
| [`js-DisposableStack`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DisposableStack) | `()` | `(extern/raw)` | `(js-typeof (js-DisposableStack))` | you need the control-abstraction global (`DisposableStack`). |
| [`js-AsyncDisposableStack`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/AsyncDisposableStack) | `()` | `(extern/raw)` | `(js-typeof (js-AsyncDisposableStack))` | you need the control-abstraction global (`AsyncDisposableStack`). |

### 4.9 Reflection

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-Reflect`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Reflect) | `()` | `(extern/raw)` | `(js-typeof (js-Reflect))` | you need the reflection API (`Reflect`). |
| [`js-Proxy`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Proxy) | `()` | `(extern/raw)` | `(js-typeof (js-Proxy))` | you need the reflection API (`Proxy`). |

### 4.10 Internationalization

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-Intl`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl) | `()` | `(extern/raw)` | `(js-typeof (js-Intl))` | you need the i18n constructor/namespace (`Intl`). |
| [`js-IntlCollator`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Collator) | `()` | `(extern/raw)` | `(js-typeof (js-IntlCollator))` | you need the i18n constructor/namespace (`IntlCollator`). |
| [`js-IntlDateTimeFormat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat) | `()` | `(extern/raw)` | `(js-typeof (js-IntlDateTimeFormat))` | you need the i18n constructor/namespace (`IntlDateTimeFormat`). |
| [`js-IntlDisplayNames`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DisplayNames) | `()` | `(extern/raw)` | `(js-typeof (js-IntlDisplayNames))` | you need the i18n constructor/namespace (`IntlDisplayNames`). |
| [`js-IntlDurationFormat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DurationFormat) | `()` | `(extern/raw)` | `(js-typeof (js-IntlDurationFormat))` | you need the i18n constructor/namespace (`IntlDurationFormat`). |
| [`js-IntlListFormat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/ListFormat) | `()` | `(extern/raw)` | `(js-typeof (js-IntlListFormat))` | you need the i18n constructor/namespace (`IntlListFormat`). |
| [`js-IntlLocale`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Locale) | `()` | `(extern/raw)` | `(js-typeof (js-IntlLocale))` | you need the i18n constructor/namespace (`IntlLocale`). |
| [`js-IntlNumberFormat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/NumberFormat) | `()` | `(extern/raw)` | `(js-typeof (js-IntlNumberFormat))` | you need the i18n constructor/namespace (`IntlNumberFormat`). |
| [`js-IntlPluralRules`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/PluralRules) | `()` | `(extern/raw)` | `(js-typeof (js-IntlPluralRules))` | you need the i18n constructor/namespace (`IntlPluralRules`). |
| [`js-IntlRelativeTimeFormat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/RelativeTimeFormat) | `()` | `(extern/raw)` | `(js-typeof (js-IntlRelativeTimeFormat))` | you need the i18n constructor/namespace (`IntlRelativeTimeFormat`). |
| [`js-IntlSegmenter`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Segmenter) | `()` | `(extern/raw)` | `(js-typeof (js-IntlSegmenter))` | you need the i18n constructor/namespace (`IntlSegmenter`). |

## Chapter 5 — Choosing the Right Function Quickly

- Need a global object/constructor: use the matching `js-*` accessor in Chapter 4.
- Need `new`: use `js-new`.
- Need method call: see Section 2.6 (`js-send` variants).
- Need property read: `js-ref` (converted) or `js-ref/extern` (raw).
- Need strict null/undefined checks: `js-null?`, `js-undefined?`, or `js-nullish?`.
- Need JS numeric formatting/predicate semantics: use the Chapter 3 `js-number-*` functions.

## Chapter 6 — Coverage Checklist

This document covers **129** functions from `ffi/standard.ffi`.
