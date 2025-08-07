# AGENTS.md

**Project**: WebRacket — A Racket-to-WebAssembly Compiler and Runtime  
**Purpose**: Implement the core Racket runtime and compiler targeting Wasm-GC using accurate value representation and Racket-compatible semantics.

---

## 🧠 Agent Overview

Codex agents working on this project must understand the **data
model**, **value tagging**, **type system**, and **semantic
goals**. Most work involves **emitting validated WebAssembly GC
code**, **writing runtime support functions**, and **implementing core
Racket primitives** faithfully.

---

## 🔧 Core Responsibilities

| Agent Name         | Responsibility Description |
|--------------------|----------------------------|
| **type-checker**   | Validates types and enforces that functions only call `_checked` versions after verifying fixnum, character, string, etc. tags. Uses `ref.test` + `ref.cast`. |
| **formatter**      | Builds `display` and `write` routines for Racket values. Uses growable arrays/strings to avoid O(n²) behavior. |
| **utf8-agent**     | Maintains line/column/position tracking during UTF-8 output via `StringPort`. Handles CR, LF, CRLF, tab, and multibyte sequences. |
| **fasl-encoder**   | Encodes Racket values to FASL format. No graph sharing. Writes to a `GrowableBytes` buffer. Uses Racket’s FASL tag format. |
| **printer**        | Implements `format/display`, `format/display:symbol`, etc., dispatching based on tagged types or heap tags. |
| **structure-agent**| Builds and supports `make-struct-type-descriptor`, guards, accessors, mutators. Uses `$Struct`, `$StructType`, `$Array`. |
| **value-agent**    | Encodes immediate values: fixnums, characters, booleans. Knows tagging layout and validates properly. |
| **hash-agent**     | Supports mutable hash tables with open addressing. Uses `(ref $Array)` of alternating keys/values. |
| **closure-agent**  | Implements closures as `(ref $Closure)` with `$ClosCode` and `$Free` array. Follows Racket's argument vector model. |
| **symbol-agent**   | Manages symbol interning and gensyms. Symbols store interned names in `$String` form. |
| **growable-agent** | Constructs growable string/byte/int builders using `Growable*` types and converts to final immutable arrays. |

---

## 🧱 Representation

### Tagged Values

- **Fixnum**: `(ref i31)` where `lsb = 0` (checked via `ref.test (ref i31)`)
- **Character**: `(ref i31)` with lower 8 bits = `0x0F`, upper bits contain UTF-21 codepoint
- **Booleans**: distinct immediate tags for `#t`, `#f`
- **Void / Null**: special constants using unique immediate tags
- **Heap Values**: all boxed types are subtypes of `$Heap`

### Core Structs

```wasm
(type $Pair     (sub $Heap (struct (field $a (mut (ref eq))) (field $d (mut (ref eq))))))
(type $Box      (sub $Heap (struct (field $v (mut (ref eq))))))
(type $Flonum   (sub $Heap (struct (field $v f64))))
(type $String   (sub $Heap (struct (field $immutable i32) (field $len i32) (field (mut (ref $I32Array))))))
(type $Bytes    (sub $Heap (struct (field $immutable i32) (field (mut (ref $I8Array))))))
(type $Symbol   (sub $Heap (struct (field $str (mut (ref $String))) ...)))
(type $Closure  (struct (field $code (ref $ClosCode)) (field $free (ref $Free))))
etc.
```

---

## 🚧 Known Limitations

- ❌ No support for graph sharing in FASL
- ❌ `equal?`, `read`, `write` still unimplemented
- ❌ No I/O ports beyond `StringPort`
- ❌ Struct printing and comparison not finalized

---

## 🧭 Style Rules

- Always validate `(ref eq)` types before using.
- Do not mix `ref.test` and `ref.cast` without conditional control.
- Use named fields, not struct field indices.
- Use `i31.get_u` + right-shift for fixnums.
- Avoid recursion in formatting; use explicit stack/array.
- No hanging parens. Write `(if (...) (then ...) (else ...))` style.
- Add suffixes like `/checked`, `/fx`, `/i32`, `/unchecked` where helpful.

---

## 📝 Pull Request Guidelines

- Do not mention testing in PR messages.

---

## 🔗 References

- [FASL Format](https://docs.racket-lang.org/reference/fasl.html)
- [Line/Column Tracking](https://docs.racket-lang.org/reference/linecol.html)
- [`racket/fasl.rkt` Source](https://github.com/racket/racket/blob/master/racket/collects/racket/fasl.rkt)
