# Benchmark Design Notes

This document captures the checklist for small microbenchmarks so we can
interpret results consistently and avoid over-reading one-off numbers.

## What A Benchmark Must Hold Constant

- Measure the same work in both cases.
  - Same fixture shape.
  - Same property or method name.
  - Same conversion semantics.
  - Same write semantics when comparing setters.
- Run both cases in the same execution mode.
  - Same node/browser mode.
  - Same compiler flags.
  - Same FFI set.
  - Same build state.
- Keep the benchmark dominated by the operation under test.
  - Avoid printing inside the timed loop.
  - Avoid allocation unrelated to the operation.
  - Avoid extra bridge calls in the hot path.
- Use the same timer source for both cases.
  - Prefer one clock for the whole benchmark.
  - Measure elapsed time, not absolute timestamps.
- Warm up the path before timing.
  - Run once before the timed loop.
  - This reduces first-run noise.

## How To Read The Result

- Treat the number as a relative comparison, not a universal truth.
- A dedicated binding being faster than a generic bridge is useful only if
  the two paths are semantically equivalent.
- A result on a plain JS object does not automatically generalize to:
  - DOM objects
  - typed arrays
  - accessors with side effects
  - wrapped browser objects
  - values that need extra conversion

## Checklist For Future Microbenchmarks

1. Same semantics on both sides.
2. Same runtime mode.
3. Same warmup behavior.
4. Tight loop, minimal noise.
5. One timer source.
6. Check the result with a checksum or final value.
7. Interpret ratios, not just raw milliseconds.

## Current Temp Benchmark

The temporary benchmark in [`tmp/benchmark.rkt`]( /Users/soegaard/Dropbox/GitHub/webracket/tmp/benchmark.rkt ) compares a generic
`js-ref`/`js-set!` property path with a dedicated `temp-x` / `temp-set-x!`
binding.

It uses a plain JS object with an `x` property, warms up once, then runs
5,000,000 read iterations and 5,000,000 write iterations. The benchmark
prints elapsed milliseconds and a checksum/final value through
`console-log`.

Observed results from the run:

- `generic js-ref`: about `2885 ms`
- `dedicated binding`: about `1220 ms`
- `generic js-set!`: about `2146 ms`
- `dedicated set!`: about `790 ms`

Interpretation:

- The dedicated binding was faster for this specific plain-object property
  case.
- The read path was roughly 2.3x faster.
- The write path was roughly 2.7x faster.
- These numbers are only meaningful for the exact benchmark shape above;
  they do not automatically generalize to DOM objects or more complex bridge
  cases.
