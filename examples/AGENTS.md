# AGENTS.md (examples/)

This folder contains runnable WebRacket examples intended for local use after cloning the repo.

## Purpose

- Keep small, practical examples that users can build and run locally.
- Treat `examples/` as user-facing material, not temporary scratch output.

## When Adding a New Example

1. Create a new subfolder under `examples/` (for example `examples/hello-world-3/`).
2. Add the source program (`.rkt`).
3. Add a `README.md` in that subfolder with:
   - build command(s),
   - run/serve command(s),
   - URL to open (for browser examples), or terminal run command (for node examples).
4. Add the example to `examples/build.sh` if it should be part of the standard examples build.
5. Build once and verify it runs.

## Artifact Policy

- Keep runnable build artifacts needed by users in `examples/`:
  - browser examples: `.html`, `.js` (if generated), `.wasm`
  - node examples: `.js`, `.wasm`
- Do not rely on website build scripts for these artifacts.
- `examples/` is independent of `web-site/` and `web-site-new/`.

## Commit Policy

- Use `../commit-examples.sh` from repo root to stage/commit example changes.
- The commit helper:
  - includes relevant example sources/docs/assets and runnable artifacts,
  - excludes temporary/editor junk,
  - excludes `examples/raco/`,
  - excludes `.wat` and `.wasm.map.sexp`.

## Scope Boundaries

- Do not "fix" compiler/runtime bugs by coding around them in examples.
- If an example reveals a WebRacket bug, create a minimal repro and fix WebRacket itself.
