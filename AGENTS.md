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

## Implementing new WebRacket primitives

The following steps are needed to implement a WebRacket primitive.

- Implement the primitive in `runtime-wasm.rkt`. 
- Add it to the list of primitives in `compiler.rkt`.
- If needed add a clause to the inlining in `compiler.rkt`
  (needed when the function doesn't take a fixed number of arguments)
- Export the primitive in `primitives.rkt`.
- If the WebRacket primitive isn't a primitive in Racket,
  then add a "dummy" implementation at the botton of `primitives.rkt`.
  (this way a program written in `#lang webracket` works the same
   as if the program were compiled with webracket.
- Add test cases to `test/test-basics.rkt`. 
  Make sure to put the test cases in the correct section.
  (consult `docs/` to se the correct section)
- When asked to implement a WebRacket primitives, which is also
  in Racket, consult the documentation in `docs/` to get a 
  documentation for the function.
- If the WebRacket function has restrictions or behave differently
  from the Racket one, make a comment about it in `runtime-wasm.rkt`
- If a parameter of a function is optional, mention it in an inline comment.
  Also, mention the default value.
- Use inline comments for the type of each parameter.

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

### Heap and Array

(type $Array    (array (mut (ref eq))))
(type $I32Array (array (mut i32)))
(type $I8Array  (array (mut i8)))

### Boxed Types

(type $Bytes (sub $Heap
  (struct (field $hash (mut i32))
          (field $immutable i32)
          (field $bs (mut (ref $I8Array))))))

(type $String (sub $Heap
  (struct (field $hash (mut i32))
          (field $immutable i32)
          (field $codepoints (mut (ref $I32Array))))))

(type $Pair (sub $Heap
  (struct (field $hash (mut i32))
          (field $a (mut (ref eq)))
          (field $d (mut (ref eq))))))

(type $Vector (sub $Heap
  (struct (field $hash (mut i32))
          (field $arr (ref $Array)))))

(type $Flonum (sub $Heap
  (struct (field $hash (mut i32))
          (field $v f64))))


### Immediates

Characters: (codepoint << char-shift)    | char-tag
Booleans:   (bit       << boolean-shift) | boolean-tag
Null:       fixed encoded value.
Void:       fixed encoded value.
EOF:        fixed encoded value.


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

## Coding Style & Conventions
- Validate first → fail if not valid → proceed.
- Ref types are always type-checked with ref.test + ref.cast before use.
- Functions with /checked expect already-validated arguments.
-  Fixnums: (ref i31) with LSB=0; store/unbox via i31.get_u + shift.
- Immediates: (ref i31) with LSB=1 + subtag bits (characters, booleans, null, void, eof).
- Separate locals for:
  - Tagged fixnum/immediate → x/tag
  - Unboxed i32 version → plain name (x)

- No hanging parentheses — closing )) goes on same line as final expression.
- Prefer named struct fields over numeric indices.
- Drop results from calls when unused with (drop (call ...)).
- Stack discipline: all branches/blocks must leave same number of values.
- In order to avoid the error "failed: uninitialized non-defaultable local"
  use the "Validate first → fail if not valid → proceed" pattern.
  That is, do not put initializations inside if-expressions.
- Remember the result type for if-expressions that put a value of the stack.
- Remember `then` and `else` in if-expressions.
- In the folded text format for WebAssembly, an if-expression does not need an `else`-clause.
  Drop the `else` in this situation `(else (nop))`.

--- 

## 📝 Pull Request Guidelines

- Do not mention testing in PR messages.

---

## 🔗 References

- [FASL Format](https://docs.racket-lang.org/reference/fasl.html)
- [Line/Column Tracking](https://docs.racket-lang.org/reference/linecol.html)
- [`racket/fasl.rkt` Source](https://github.com/racket/racket/blob/master/racket/collects/racket/fasl.rkt)
