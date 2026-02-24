# Web-site Build Scripts

This folder contains scripts for building the old and new WebRacket web sites.

## Scripts

- `web-site/src/build.sh`
  - Builds the **old** site into `web-site/local/`.
  - Preserves `web-site/local/new/` when run directly.
  - Generates FFI docs unless `SKIP_FFI_DOC_GEN=1`.
  - Plays a sound on macOS unless `SKIP_SOUND=1`.

- `web-site-new/src/build.sh`
  - Builds the **new** site into `web-site-new/local/`.
  - Syncs `web-site-new/local/` into `web-site/local/new/`.
  - Always generates FFI docs.
  - Plays a sound on macOS unless `SKIP_SOUND=1`.

- `web-site/build-both.sh`
  - Builds old and new sites in parallel.
  - Runs FFI doc generation once in `web-site/src/`, then invokes:
    - `SKIP_FFI_DOC_GEN=1 SKIP_SOUND=1 web-site/src/build.sh`
    - `SKIP_SOUND=1 web-site-new/src/build.sh`
  - Copies `web-site-new/local/` into `web-site/local/new/`.

- `web-site/build-examples.sh`
  - Builds repository examples by calling `examples/build.sh`.
  - This is separate from website builds.
  - Use this when you want prebuilt artifacts in `examples/` for local demo serving.

- `web-site/src/build-docs-only.sh`
  - Builds only documentation-focused output for the old site workflow.

- `web-site/src/build-toc-test.sh`
  - Utility script for table-of-contents/test build scenarios.

## Environment Variables

- `SKIP_FFI_DOC_GEN=1`
  - Used by `web-site/src/build.sh` to skip FFI doc generation.
  - Used by `web-site/build-both.sh` when invoking old-site build.

- `SKIP_SOUND=1`
  - Disables macOS `afplay` notification sound in:
    - `web-site/src/build.sh`
    - `web-site-new/src/build.sh`
    - and therefore `web-site/build-both.sh` child builds.
