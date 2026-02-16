# `math.ffi` Reference

## Chapter 1 — Introduction

This document describes the JavaScript Math bindings exported by `ffi/math.ffi` in WebRacket.

What this API gives you:
- Numeric functions from JavaScript `Math`
- Trigonometric and hyperbolic functions
- Exponential/logarithmic functions
- Integer-oriented helpers (`clz32`, `imul`)

Assumption in examples: the program is compiled with `--ffi math`.

All function names are linked to MDN pages for the corresponding `Math` member.

### Table of Contents

- [Chapter 1 — Introduction](#chapter-1--introduction)
- [Chapter 2 — Conventions](#chapter-2--conventions)
- [2.1 Type Legend](#21-type-legend)
- [2.2 Numeric Behavior Notes](#22-numeric-behavior-notes)
- [2.3 Arity and Variadic Notes](#23-arity-and-variadic-notes)
- [Chapter 3 — Quick Navigation by Goal](#chapter-3--quick-navigation-by-goal)
- [Chapter 4 — Core Numeric and Rounding Functions](#chapter-4--core-numeric-and-rounding-functions)
- [Chapter 5 — Exponential and Logarithmic Functions](#chapter-5--exponential-and-logarithmic-functions)
- [Chapter 6 — Trigonometric and Hyperbolic Functions](#chapter-6--trigonometric-and-hyperbolic-functions)
- [Chapter 7 — Random Number Function](#chapter-7--random-number-function)
- [Chapter 8 — Mini Workflows](#chapter-8--mini-workflows)
- [Normalize and Clamp-Style Distance Computation](#normalize-and-clamp-style-distance-computation)
- [Numerically Stable Small-Value Calculations](#numerically-stable-small-value-calculations)
- [Angle Conversion and Trig Pipeline](#angle-conversion-and-trig-pipeline)
- [Chapter 9 — Coverage Checklist](#chapter-9--coverage-checklist)
- [Chapter 10 — Alphabetized Index](#chapter-10--alphabetized-index)

## Chapter 2 — Conventions

### 2.1 Type Legend

| Type | Meaning |
|---|---|
| `(f64)` | Double-precision floating-point number. |
| `(i32)` | Signed 32-bit integer. |
| `(u32)` | Unsigned 32-bit integer. |
| `()` | No arguments (input) or no value / void (output). |

### 2.2 Numeric Behavior Notes

- These bindings mirror JavaScript `Math` behavior.
- Results may be `NaN`, `+Infinity`, or `-Infinity` where JavaScript specifies it.
- Trigonometric functions use radians.
- `js-math-random` returns an `(f64)` in the interval `[0, 1[`.

### 2.3 Arity and Variadic Notes

- JavaScript `Math.max`, `Math.min`, and `Math.hypot` are variadic.
- In `ffi/math.ffi`, wrappers are fixed-arity:
  - `js-math-max`: exactly 2 arguments
  - `js-math-min`: exactly 2 arguments
  - `js-math-hypot`: exactly 2 arguments

## Chapter 3 — Quick Navigation by Goal

| Goal | Start with | Typical next calls |
|---|---|---|
| Absolute value and sign handling | [`js-math-abs`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/abs), [`js-math-sign`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/sign) | `js-math-max`, `js-math-min` |
| Rounding and integer-like operations | [`js-math-floor`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/floor), [`js-math-round`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/round) | `js-math-trunc`, `js-math-fround`, `js-math-clz32`, `js-math-imul` |
| Exponential and logarithmic work | [`js-math-exp`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/exp), [`js-math-log`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/log) | `js-math-expm1`, `js-math-log1p`, `js-math-pow`, `js-math-sqrt` |
| Trigonometric calculations | [`js-math-sin`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/sin), [`js-math-cos`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/cos), [`js-math-atan2`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/atan2) | `js-math-tan`, `js-math-asin`, `js-math-acos` |
| Hyperbolic calculations | [`js-math-sinh`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/sinh), [`js-math-cosh`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/cosh) | `js-math-tanh`, `js-math-asinh`, `js-math-acosh`, `js-math-atanh` |

## Chapter 4 — Core Numeric and Rounding Functions

MDN root: [Math](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-math-abs`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/abs) | `(f64)` | `(f64)` | `(js-math-abs -3.5)` | absolute magnitude is needed. |
| [`js-math-ceil`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/ceil) | `(f64)` | `(f64)` | `(js-math-ceil 2.1)` | round upward to the next integer value. |
| [`js-math-floor`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/floor) | `(f64)` | `(f64)` | `(js-math-floor 2.9)` | round downward to the previous integer value. |
| [`js-math-round`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/round) | `(f64)` | `(f64)` | `(js-math-round 2.5)` | nearest-integer rounding is needed. |
| [`js-math-trunc`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/trunc) | `(f64)` | `(f64)` | `(js-math-trunc -2.9)` | fractional part should be removed toward zero. |
| [`js-math-sign`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/sign) | `(f64)` | `(f64)` | `(js-math-sign -8.0)` | sign classification (`-1`, `0`, `1`, `NaN`) is needed. |
| [`js-math-fround`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/fround) | `(f64)` | `(f64)` | `(js-math-fround 1.337)` | emulate 32-bit float rounding in JS semantics. |
| [`js-math-clz32`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/clz32) | `(u32)` | `(u32)` | `(js-math-clz32 1)` | count leading zero bits in 32-bit unsigned value. |
| [`js-math-imul`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/imul) | `(i32 i32)` | `(i32)` | `(js-math-imul 12345 6789)` | perform 32-bit integer multiplication with JS wrapping behavior. |
| [`js-math-max`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/max) | `(f64 f64)` | `(f64)` | `(js-math-max 4.0 9.0)` | choose larger of two values. |
| [`js-math-min`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/min) | `(f64 f64)` | `(f64)` | `(js-math-min 4.0 9.0)` | choose smaller of two values. |

## Chapter 5 — Exponential and Logarithmic Functions

MDN root: [Math](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-math-exp`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/exp) | `(f64)` | `(f64)` | `(js-math-exp 1.0)` | compute `e^x`. |
| [`js-math-expm1`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/expm1) | `(f64)` | `(f64)` | `(js-math-expm1 1e-6)` | compute `e^x - 1` with better precision for small `x`. |
| [`js-math-log`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/log) | `(f64)` | `(f64)` | `(js-math-log 10.0)` | natural logarithm is needed. |
| [`js-math-log10`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/log10) | `(f64)` | `(f64)` | `(js-math-log10 1000.0)` | base-10 logarithm is needed. |
| [`js-math-log1p`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/log1p) | `(f64)` | `(f64)` | `(js-math-log1p 1e-6)` | compute `ln(1+x)` accurately for small `x`. |
| [`js-math-log2`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/log2) | `(f64)` | `(f64)` | `(js-math-log2 8.0)` | base-2 logarithm is needed. |
| [`js-math-pow`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/pow) | `(f64 f64)` | `(f64)` | `(js-math-pow 2.0 10.0)` | exponentiation with explicit base/exponent. |
| [`js-math-sqrt`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/sqrt) | `(f64)` | `(f64)` | `(js-math-sqrt 81.0)` | square root is needed. |
| [`js-math-cbrt`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/cbrt) | `(f64)` | `(f64)` | `(js-math-cbrt 27.0)` | cube root is needed. |
| [`js-math-hypot`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/hypot) | `(f64 f64)` | `(f64)` | `(js-math-hypot 3.0 4.0)` | Euclidean norm from two components. |

## Chapter 6 — Trigonometric and Hyperbolic Functions

MDN root: [Math](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-math-sin`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/sin) | `(f64)` | `(f64)` | `(js-math-sin 0.0)` | sine of angle in radians. |
| [`js-math-cos`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/cos) | `(f64)` | `(f64)` | `(js-math-cos 0.0)` | cosine of angle in radians. |
| [`js-math-tan`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/tan) | `(f64)` | `(f64)` | `(js-math-tan 0.0)` | tangent of angle in radians. |
| [`js-math-asin`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/asin) | `(f64)` | `(f64)` | `(js-math-asin 0.5)` | inverse sine is needed. |
| [`js-math-acos`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/acos) | `(f64)` | `(f64)` | `(js-math-acos 0.5)` | inverse cosine is needed. |
| [`js-math-atan`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/atan) | `(f64)` | `(f64)` | `(js-math-atan 1.0)` | inverse tangent from one ratio. |
| [`js-math-atan2`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/atan2) | `(f64 f64)` | `(f64)` | `(js-math-atan2 1.0 1.0)` | inverse tangent with quadrant awareness. |
| [`js-math-sinh`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/sinh) | `(f64)` | `(f64)` | `(js-math-sinh 1.0)` | hyperbolic sine is needed. |
| [`js-math-cosh`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/cosh) | `(f64)` | `(f64)` | `(js-math-cosh 1.0)` | hyperbolic cosine is needed. |
| [`js-math-tanh`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/tanh) | `(f64)` | `(f64)` | `(js-math-tanh 1.0)` | hyperbolic tangent is needed. |
| [`js-math-asinh`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/asinh) | `(f64)` | `(f64)` | `(js-math-asinh 1.0)` | inverse hyperbolic sine is needed. |
| [`js-math-acosh`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/acosh) | `(f64)` | `(f64)` | `(js-math-acosh 2.0)` | inverse hyperbolic cosine is needed. |
| [`js-math-atanh`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/atanh) | `(f64)` | `(f64)` | `(js-math-atanh 0.5)` | inverse hyperbolic tangent is needed. |

## Chapter 7 — Random Number Function

MDN root: [Math.random](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-math-random`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random) | `()` | `(f64)` | `(js-math-random)` | pseudo-random value in the interval `[0, 1[` is needed. |

## Chapter 8 — Mini Workflows

### Normalize and Clamp-Style Distance Computation

```racket
(define dx 3.0)
(define dy 4.0)
(define dist (js-math-hypot dx dy))
(define capped (js-math-min dist 5.0))
capped
```

### Numerically Stable Small-Value Calculations

```racket
(define x 1e-8)
(define a (js-math-expm1 x))
(define b (js-math-log1p x))
(list a b)
```

### Angle Conversion and Trig Pipeline

```racket
(define degrees 60.0)
(define radians (/ (* degrees 3.141592653589793) 180.0))
(list (js-math-sin radians)
      (js-math-cos radians)
      (js-math-tan radians))
```

## Chapter 9 — Coverage Checklist

- This document covers **35** functions from `ffi/math.ffi`.
- Total documented functions: **35**
- `core numeric and rounding`: 11 functions
- `exponential and logarithmic`: 10 functions
- `trigonometric and hyperbolic`: 13 functions
- `random`: 1 function

## Chapter 10 — Alphabetized Index

- [`js-math-abs`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/abs)
- [`js-math-acos`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/acos)
- [`js-math-acosh`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/acosh)
- [`js-math-asin`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/asin)
- [`js-math-asinh`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/asinh)
- [`js-math-atan`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/atan)
- [`js-math-atan2`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/atan2)
- [`js-math-atanh`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/atanh)
- [`js-math-cbrt`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/cbrt)
- [`js-math-ceil`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/ceil)
- [`js-math-clz32`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/clz32)
- [`js-math-cos`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/cos)
- [`js-math-cosh`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/cosh)
- [`js-math-exp`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/exp)
- [`js-math-expm1`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/expm1)
- [`js-math-floor`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/floor)
- [`js-math-fround`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/fround)
- [`js-math-hypot`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/hypot)
- [`js-math-imul`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/imul)
- [`js-math-log`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/log)
- [`js-math-log1p`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/log1p)
- [`js-math-log10`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/log10)
- [`js-math-log2`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/log2)
- [`js-math-max`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/max)
- [`js-math-min`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/min)
- [`js-math-pow`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/pow)
- [`js-math-random`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [`js-math-round`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/round)
- [`js-math-sign`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/sign)
- [`js-math-sin`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/sin)
- [`js-math-sinh`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/sinh)
- [`js-math-sqrt`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/sqrt)
- [`js-math-tan`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/tan)
- [`js-math-tanh`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/tanh)
- [`js-math-trunc`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/trunc)
