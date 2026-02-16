# `js.ffi` Reference

## Chapter 1 — Introduction

This document describes the JavaScript Array bindings exported by `ffi/js.ffi` in WebRacket.

What this API gives you:
- `Array` static functions (`from`, `fromAsync`, `isArray`, `of`)
- Array property access (`length`)
- Array instance methods, including immutable variants (`toReversed`, `toSorted`, `toSpliced`, `with`)

Assumption in examples: the program is compiled with `--ffi js`.

All function names are linked to MDN pages for the corresponding `Array` member.

### Table of Contents

- [Chapter 1 — Introduction](#chapter-1--introduction)
- [Chapter 2 — Conventions](#chapter-2--conventions)
- [2.1 Type Legend](#21-type-legend)
- [2.2 Return and Conversion Conventions](#22-return-and-conversion-conventions)
- [2.3 Common Setup Helpers](#23-common-setup-helpers)
- [Chapter 3 — Quick Navigation by Goal](#chapter-3--quick-navigation-by-goal)
- [Chapter 4 — Static Methods](#chapter-4--static-methods)
- [Chapter 5 — Properties](#chapter-5--properties)
- [Chapter 6 — Instance Methods](#chapter-6--instance-methods)
- [Chapter 7 — Callback Signatures](#chapter-7--callback-signatures)
- [7.1 Predicate, Mapper, and Visitor Callbacks](#71-predicate-mapper-and-visitor-callbacks)
- [7.2 Reducer Callbacks](#72-reducer-callbacks)
- [7.3 Comparator Callbacks](#73-comparator-callbacks)
- [Chapter 8 — Optional Argument Matrix](#chapter-8--optional-argument-matrix)
- [Chapter 9 — Gotchas](#chapter-9--gotchas)
- [Chapter 10 — Mini Workflows](#chapter-10--mini-workflows)
- [Build an Array and Map Values](#build-an-array-and-map-values)
- [Search with Fallback](#search-with-fallback)
- [Non-Mutating Updates](#non-mutating-updates)
- [Mutating Pipeline](#mutating-pipeline)
- [Filter Then Aggregate](#filter-then-aggregate)
- [Chapter 11 — Coverage Checklist](#chapter-11--coverage-checklist)
- [Chapter 12 — Alphabetized Index](#chapter-12--alphabetized-index)

## Chapter 2 — Conventions

### 2.1 Type Legend

| Type | Meaning |
|---|---|
| `(extern)` | Raw JavaScript value/object reference (no WebRacket value conversion). |
| `(value)` | WebRacket value converted through the FFI value bridge. |
| `(string)` | JavaScript string mapped to WebRacket string. |
| `(i32)` | 32-bit integer. In this module it is used both for booleans (`1`/`0`) and indexes (`-1` when not found). |
| `(u32)` | Unsigned 32-bit integer. |
| `()` | No arguments (input) or no value / void (output). |

### 2.2 Return and Conversion Conventions

- Predicate-style methods (`is-array`, `every`, `includes`, `some`) return `(i32)` where `1` means true and `0` means false.
- Index methods (`find-index`, `find-last-index`, `index-of`, `last-index-of`) return `(i32)` indexes and return `-1` when no match is found.
- Methods returning `(extern)` keep JavaScript identity; continue with FFI calls on that value.
- Callback parameters are `(extern)` and should be JavaScript-callable values.

### 2.3 Common Setup Helpers

```racket
(define arr (js-array-of (vector 1 2 3)))
(define callback (lambda args #t))
(define compare-fn (lambda (a b) 0))
```

## Chapter 3 — Quick Navigation by Goal

| Goal | Start with | Typical next calls |
|---|---|---|
| Create arrays from data | [`js-array-from`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/from), [`js-array-of`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/of) | `js-array-map`, `js-array-filter`, `js-array-length` |
| Read/search elements | [`js-array-at`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/at), [`js-array-includes`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/includes) | `js-array-find`, `js-array-find-index`, `js-array-index-of` |
| Transform without mutation | [`js-array-map`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/map), [`js-array-filter`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/filter) | `js-array-to-reversed`, `js-array-to-sorted`, `js-array-to-spliced`, `js-array-with` |
| Mutate in place | [`js-array-push`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/push), [`js-array-splice`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/splice) | `js-array-pop`, `js-array-shift`, `js-array-unshift`, `js-set-array-length!` |
| Aggregate/stringify | [`js-array-reduce`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/reduce), [`js-array-join`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/join) | `js-array-reduce-right`, `js-array-to-string`, `js-array-to-locale-string` |
| Iterate keys/entries/values | [`js-array-keys`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/keys), [`js-array-entries`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/entries) | `js-array-values`, custom iterator handling |

## Chapter 4 — Static Methods

MDN root: [Array](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array)

| Function | Input types | Output type | Mutates receiver? | Callback? | Example | Use when |
|---|---|---|---|---|---|---|
| [`js-array-from`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/from) | `(value extern value)` | `(extern)` | n/a | yes | `(js-array-from (vector 1 2 3) (void) (void))` | convert iterable/array-like input to JS Array. |
| [`js-array-from-async`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/fromAsync) | `(value extern value)` | `(extern)` | n/a | yes | `(js-array-from-async (vector 1 2 3) (void) (void))` | build Array asynchronously from async input source. |
| [`js-array-is-array`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/isArray) | `(value)` | `(i32)` | n/a | no | `(js-array-is-array arr)` | check whether a value is an Array (`1`/`0`). |
| [`js-array-of`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/of) | `(value)` | `(extern)` | n/a | no | `(js-array-of (vector 4 5))` | construct an Array from explicit items. |

## Chapter 5 — Properties

MDN root: [Array](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array)

| Function | Input types | Output type | Mutates receiver? | Callback? | Example | Use when |
|---|---|---|---|---|---|---|
| [`js-array-length`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/length) | `(extern)` | `(u32)` | no | no | `(js-array-length arr)` | read array length quickly. |
| [`js-set-array-length!`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/length) | `(extern u32)` | `()` | yes | no | `(js-set-array-length! arr 2)` | truncate/extend an array by setting `length`. |

## Chapter 6 — Instance Methods

MDN root: [Array.prototype](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array)

| Function | Input types | Output type | Mutates receiver? | Callback? | Example | Use when |
|---|---|---|---|---|---|---|
| [`js-array-ref`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/at) | `(extern i32)` | `(value)` | no | no | `(js-array-ref arr -1)` | read one element by index (supports negative indexes). |
| [`js-array-at`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/at) | `(extern i32)` | `(value)` | no | no | `(js-array-at arr -1)` | read one element by index (supports negative indexes). |
| [`js-array-concat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/concat) | `(extern value)` | `(extern)` | no | no | `(js-array-concat arr (vector 4 5))` | concatenate arrays/items into a new array. |
| [`js-array-copy-within`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/copyWithin) | `(extern i32 i32 value)` | `(extern)` | yes | no | `(js-array-copy-within arr 0 1 (void))` | copy a range in-place within same array. |
| [`js-array-entries`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/entries) | `(extern)` | `(extern)` | no | no | `(js-array-entries arr)` | iterate index/value pairs. |
| [`js-array-every`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/every) | `(extern extern value)` | `(i32)` | no | yes | `(js-array-every arr callback (void))` | check whether all elements satisfy a predicate. |
| [`js-array-fill`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/fill) | `(extern value value value)` | `(extern)` | yes | no | `(js-array-fill arr 0 (void) (void))` | overwrite a range with a single value. |
| [`js-array-filter`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/filter) | `(extern extern value)` | `(extern)` | no | yes | `(js-array-filter arr callback (void))` | create a new array with matching elements. |
| [`js-array-find`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/find) | `(extern extern value)` | `(value)` | no | yes | `(js-array-find arr callback (void))` | return first matching element (converted value). |
| [`js-array-find-index`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/findIndex) | `(extern extern value)` | `(i32)` | no | yes | `(js-array-find-index arr callback (void))` | return first matching index, or `-1`. |
| [`js-array-find-last`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/findLast) | `(extern extern value)` | `(value)` | no | yes | `(js-array-find-last arr callback (void))` | return last matching element (converted value). |
| [`js-array-find-last-index`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/findLastIndex) | `(extern extern value)` | `(i32)` | no | yes | `(js-array-find-last-index arr callback (void))` | return last matching index, or `-1`. |
| [`js-array-flat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/flat) | `(extern value)` | `(extern)` | no | no | `(js-array-flat arr (void))` | flatten nested arrays by depth. |
| [`js-array-flat-map`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/flatMap) | `(extern extern value)` | `(extern)` | no | yes | `(js-array-flat-map arr callback (void))` | map and flatten by one level in one pass. |
| [`js-array-for-each`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/forEach) | `(extern extern value)` | `()` | no | yes | `(js-array-for-each arr callback (void))` | run side-effect callback for each element. |
| [`js-array-includes`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/includes) | `(extern value value)` | `(i32)` | no | no | `(js-array-includes arr 2 (void))` | membership test with `1`/`0` result. |
| [`js-array-index-of`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/indexOf) | `(extern value value)` | `(i32)` | no | no | `(js-array-index-of arr 2 (void))` | first index of exact value, or `-1`. |
| [`js-array-join`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/join) | `(extern value)` | `(string)` | no | no | `(js-array-join arr (void))` | convert array to delimiter-separated string. |
| [`js-array-keys`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/keys) | `(extern)` | `(extern)` | no | no | `(js-array-keys arr)` | iterate indexes only. |
| [`js-array-last-index-of`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/lastIndexOf) | `(extern value value)` | `(i32)` | no | no | `(js-array-last-index-of arr 2 (void))` | last index of exact value, or `-1`. |
| [`js-array-map`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/map) | `(extern extern value)` | `(extern)` | no | yes | `(js-array-map arr callback (void))` | create transformed array. |
| [`js-array-pop`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/pop) | `(extern)` | `(value)` | yes | no | `(js-array-pop arr)` | remove and return the last element. |
| [`js-array-push`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/push) | `(extern value)` | `(u32)` | yes | no | `(js-array-push arr (vector 4 5))` | append items and get new length. |
| [`js-array-reduce`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/reduce) | `(extern extern value)` | `(value)` | no | yes | `(js-array-reduce arr callback 0)` | aggregate from left to right. |
| [`js-array-reduce-right`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/reduceRight) | `(extern extern value)` | `(value)` | no | yes | `(js-array-reduce-right arr callback 0)` | aggregate from right to left. |
| [`js-array-reverse`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/reverse) | `(extern)` | `(extern)` | yes | no | `(js-array-reverse arr)` | reverse elements in-place. |
| [`js-array-shift`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/shift) | `(extern)` | `(value)` | yes | no | `(js-array-shift arr)` | remove and return the first element. |
| [`js-array-slice`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/slice) | `(extern value value)` | `(extern)` | no | no | `(js-array-slice arr 1 (void))` | copy a subrange into a new array. |
| [`js-array-some`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/some) | `(extern extern value)` | `(i32)` | no | yes | `(js-array-some arr callback (void))` | check whether any element satisfies predicate. |
| [`js-array-sort`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/sort) | `(extern extern)` | `(extern)` | yes | yes | `(js-array-sort arr compare-fn)` | sort array in-place. |
| [`js-array-splice`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/splice) | `(extern value)` | `(extern)` | yes | no | `(js-array-splice arr (vector 1 2 9))` | remove/insert items in-place. |
| [`js-array-to-locale-string`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/toLocaleString) | `(extern value)` | `(string)` | no | no | `(js-array-to-locale-string arr (void))` | locale-aware string conversion. |
| [`js-array-to-string`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/toString) | `(extern)` | `(string)` | no | no | `(js-array-to-string arr)` | default string conversion. |
| [`js-array-unshift`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/unshift) | `(extern value)` | `(u32)` | yes | no | `(js-array-unshift arr (vector 4 5))` | prepend items and get new length. |
| [`js-array-values`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/values) | `(extern)` | `(extern)` | no | no | `(js-array-values arr)` | iterate values only. |
| [`js-array-to-reversed`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/toReversed) | `(extern)` | `(extern)` | no | no | `(js-array-to-reversed arr)` | get reversed copy without mutating source. |
| [`js-array-to-sorted`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/toSorted) | `(extern extern)` | `(extern)` | no | yes | `(js-array-to-sorted arr compare-fn)` | get sorted copy without mutating source. |
| [`js-array-to-spliced`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/toSpliced) | `(extern value)` | `(extern)` | no | no | `(js-array-to-spliced arr (vector 1 0 9))` | get spliced copy without mutating source. |
| [`js-array-with`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/with) | `(extern i32 value)` | `(extern)` | no | no | `(js-array-with arr 0 99)` | get copy with one index replaced. |

## Chapter 7 — Callback Signatures

### 7.1 Predicate, Mapper, and Visitor Callbacks

Used by:
- `js-array-every`, `js-array-filter`, `js-array-find`, `js-array-find-index`
- `js-array-find-last`, `js-array-find-last-index`, `js-array-flat-map`
- `js-array-for-each`, `js-array-map`, `js-array-some`

Callback arguments follow JavaScript `Array` conventions:

```text
(element, index, array)
```

- `element`: current value
- `index`: current index
- `array`: receiver array

`thisArg` is optional where supported; pass `(void)` to use JS default binding behavior.

### 7.2 Reducer Callbacks

Used by:
- `js-array-reduce`
- `js-array-reduce-right`

Reducer signature:

```text
(accumulator, element, index, array)
```

If no initial accumulator is supplied, pass `(void)` and JavaScript uses the first (or last) element as the initial accumulator, following JS rules.

### 7.3 Comparator Callbacks

Used by:
- `js-array-sort`
- `js-array-to-sorted`

Comparator signature:

```text
(a, b) -> negative | 0 | positive
```

Return value meaning:
- negative: `a` before `b`
- zero: unchanged order
- positive: `a` after `b`

## Chapter 8 — Optional Argument Matrix

| Binding | Optional arguments | Pass `(void)` for | JavaScript default behavior |
|---|---|---|---|
| `js-array-from` | `map-fn`, `this-arg` | missing mapper and/or `thisArg` | no mapper; default `this` binding |
| `js-array-from-async` | `map-fn`, `this-arg` | missing mapper and/or `thisArg` | no mapper; default `this` binding |
| `js-array-copy-within` | `end` | missing end index | copy through end of array |
| `js-array-every` | `this-arg` | missing `thisArg` | callback uses default `this` binding |
| `js-array-fill` | `start`, `end` | missing start/end index | fill whole array |
| `js-array-filter` | `this-arg` | missing `thisArg` | callback uses default `this` binding |
| `js-array-find` | `this-arg` | missing `thisArg` | callback uses default `this` binding |
| `js-array-find-index` | `this-arg` | missing `thisArg` | callback uses default `this` binding |
| `js-array-find-last` | `this-arg` | missing `thisArg` | callback uses default `this` binding |
| `js-array-find-last-index` | `this-arg` | missing `thisArg` | callback uses default `this` binding |
| `js-array-flat` | `depth` | missing depth | flatten by depth `1` |
| `js-array-flat-map` | `this-arg` | missing `thisArg` | callback uses default `this` binding |
| `js-array-for-each` | `this-arg` | missing `thisArg` | callback uses default `this` binding |
| `js-array-includes` | `from-index` | missing start index | start at index `0` |
| `js-array-index-of` | `from-index` | missing start index | start at index `0` |
| `js-array-join` | `separator` | missing separator | comma separator |
| `js-array-last-index-of` | `from-index` | missing start index | search from end |
| `js-array-map` | `this-arg` | missing `thisArg` | callback uses default `this` binding |
| `js-array-reduce` | `initial-value` | missing initial accumulator | infer from first element |
| `js-array-reduce-right` | `initial-value` | missing initial accumulator | infer from last element |
| `js-array-slice` | `start`, `end` | missing start/end index | full range |
| `js-array-some` | `this-arg` | missing `thisArg` | callback uses default `this` binding |
| `js-array-sort` | `compare-function` | missing comparator | lexicographic sort semantics |
| `js-array-to-locale-string` | `locales/options` | missing locale/options list | environment default locale/options |
| `js-array-to-sorted` | `compare-function` | missing comparator | lexicographic sort semantics |

## Chapter 9 — Gotchas

- `js-array-ref` and `js-array-at` both call the same JS method (`Array.prototype.at`). `js-array-ref` is an alias.
- Not every `(i32)` result is boolean. Search methods like `js-array-index-of` return indexes and can be `-1`.
- `(extern)` results are JS objects/iterators, not Racket lists or vectors. Use more FFI calls to inspect them.
- `js-array-sort` mutates in-place; `js-array-to-sorted` does not.
- `js-array-splice` mutates in-place; `js-array-to-spliced` does not.

## Chapter 10 — Mini Workflows

### Build an Array and Map Values

```racket
(define arr (js-array-from (vector 1 2 3) (void) (void)))
(define doubled (js-array-map arr (lambda (x i a) (* x 2)) (void)))
doubled
```

### Search with Fallback

```racket
(define arr (js-array-of (vector 5 10 15)))
(define idx (js-array-find-index arr (lambda (x i a) (> x 20)) (void)))
(if (= idx -1)
    'missing
    (js-array-at arr idx))
```

### Non-Mutating Updates

```racket
(define arr (js-array-of (vector 3 1 2)))
(define sorted (js-array-to-sorted arr (lambda (a b) (- a b))))
(define replaced (js-array-with sorted 0 99))
replaced
```

### Mutating Pipeline

```racket
(define arr (js-array-of (vector 1 2 3)))
(js-array-push arr (vector 4 5))
(js-array-splice arr (vector 1 2 9 9))
(js-array-sort arr (lambda (a b) (- a b)))
(js-array-length arr)
```

### Filter Then Aggregate

```racket
(define arr (js-array-of (vector 1 2 3 4)))
(define evens (js-array-filter arr (lambda (x i a) (= (modulo x 2) 0)) (void)))
(js-array-reduce evens (lambda (acc x i a) (+ acc x)) 0)
```

## Chapter 11 — Coverage Checklist

- This document covers **45** functions from `ffi/js.ffi`.
- Total documented functions: **45**
- `static methods`: 4 functions
- `properties`: 2 functions
- `instance methods`: 39 functions

## Chapter 12 — Alphabetized Index

- [`js-array-at`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/at) — instance method
- [`js-array-concat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/concat) — instance method
- [`js-array-copy-within`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/copyWithin) — instance method
- [`js-array-entries`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/entries) — instance method
- [`js-array-every`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/every) — instance method
- [`js-array-fill`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/fill) — instance method
- [`js-array-filter`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/filter) — instance method
- [`js-array-find`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/find) — instance method
- [`js-array-find-index`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/findIndex) — instance method
- [`js-array-find-last`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/findLast) — instance method
- [`js-array-find-last-index`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/findLastIndex) — instance method
- [`js-array-flat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/flat) — instance method
- [`js-array-flat-map`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/flatMap) — instance method
- [`js-array-for-each`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/forEach) — instance method
- [`js-array-from`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/from) — static method
- [`js-array-from-async`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/fromAsync) — static method
- [`js-array-includes`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/includes) — instance method
- [`js-array-index-of`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/indexOf) — instance method
- [`js-array-is-array`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/isArray) — static method
- [`js-array-join`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/join) — instance method
- [`js-array-keys`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/keys) — instance method
- [`js-array-last-index-of`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/lastIndexOf) — instance method
- [`js-array-length`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/length) — property
- [`js-array-map`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/map) — instance method
- [`js-array-of`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/of) — static method
- [`js-array-pop`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/pop) — instance method
- [`js-array-push`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/push) — instance method
- [`js-array-reduce`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/reduce) — instance method
- [`js-array-reduce-right`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/reduceRight) — instance method
- [`js-array-ref`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/at) — instance method
- [`js-array-reverse`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/reverse) — instance method
- [`js-array-shift`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/shift) — instance method
- [`js-array-slice`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/slice) — instance method
- [`js-array-some`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/some) — instance method
- [`js-array-sort`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/sort) — instance method
- [`js-array-splice`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/splice) — instance method
- [`js-array-to-locale-string`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/toLocaleString) — instance method
- [`js-array-to-reversed`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/toReversed) — instance method
- [`js-array-to-sorted`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/toSorted) — instance method
- [`js-array-to-spliced`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/toSpliced) — instance method
- [`js-array-to-string`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/toString) — instance method
- [`js-array-unshift`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/unshift) — instance method
- [`js-array-values`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/values) — instance method
- [`js-array-with`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/with) — instance method
- [`js-set-array-length!`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/length) — property
