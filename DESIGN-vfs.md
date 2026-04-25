# WebRacket VFS Design

This document sketches a virtual filesystem for WebRacket. The goal is to
support Racket-shaped file, path, and input-port behavior in the browser without
pretending that WebRacket has an operating-system filesystem.

The initial target is intentionally small:

- syntactic Unix-style path manipulation
- read-only mounted files from JavaScript-owned data
- mutable in-memory files for temporary data
- file input ports with accurate line, column, and position tracking

The standard library is compiled into WebRacket and is not modeled as a
filesystem mount.

## Design Summary

WebRacket should have a small JS-side VFS with mountable backends. WebRacket
runtime primitives call this VFS through host imports for filesystem queries and
file contents. File input ports in the Wasm runtime should reuse the existing
port and location machinery.

Example mount layout:

```text
/app      read-only mounted app source/resources
/assets   read-only tar/blob-backed assets or packages
/tmp      mutable in-memory files
/user     later: persistent IndexedDB-backed files
```

The core separation is:

```text
Path operations are syntactic.
Filesystem operations go through JS VFS backends.
File input ports reuse WebRacket's existing location-tracking port machinery.
```

## References

Relevant local Racket reference sections:

- `docs/reference/paths.scrbl`
- `docs/reference/filesystem.scrbl`
- `docs/reference/ports.scrbl`
- `docs/reference/file-ports.scrbl`
- `docs/reference/port-procs.scrbl`
- `docs/reference/port-buffers.scrbl`
- `docs/reference/string-input.scrbl`
- `docs/reference/port-line-counting.scrbl`

Relevant WebRacket runtime structures already exist in `runtime-wasm.rkt`:

- `$Location`
- `$Port`
- `$InputPort`
- `$InputStringPort`
- `$CustomInputPort`
- `port-next-location`
- `port-count-lines!`
- `read-byte`
- `read-char`
- `read-line`

The local Racket checkout also provides a useful reference point:

- `/Users/soegaard/Dropbox/GitHub/racket/racket/src/zuo/zuo.c`

Zuo is a particularly good model for the first WebRacket VFS path layer:
it favors syntactic path normalization, keeps path handling simple, and avoids
making ordinary path construction depend on filesystem symlink resolution.

## Non-Goals

The initial VFS is not a POSIX compatibility layer.

Defer:

- symbolic links
- permissions
- filesystem change events
- platform-specific Windows path behavior
- subprocess/device/fifo behavior
- current-user/home/preference directory discovery
- full errno-specific exception compatibility
- full output file `#:exists` mode compatibility
- async file APIs
- direct reuse of Emscripten's JS `FS`

Emscripten's VFS is a useful design reference, especially its mount table and
blob-backed backends, but WebRacket should not depend on Emscripten's runtime
unless a concrete Emscripten-compiled library requires that bridge.

## Path Model

Racket has a distinct path datatype, and many filesystem functions accept
`path-string?`, meaning a path value or a non-empty string without nul
characters. WebRacket should eventually represent paths with the existing
`$Path` type:

```wasm
(type $Path
  (sub $Heap
    (struct
      (field $hash       (mut i32))
      (field $bytes      (ref $Bytes))
      (field $convention (ref eq)))))
```

For the browser target, the first implementation should use Unix path
conventions only. The convention field can still be set to `'unix` so the
representation does not paint us into a corner.

Path operations should not consult the VFS unless the Racket reference requires
filesystem access. This follows both Racket's path/function split and Zuo's
syntactic normalization style.

Initial path procedures:

- `path?`
- `path-string?`
- `string->path`
- `path->string`
- `path->bytes`
- `bytes->path`
- `relative-path?`
- `absolute-path?`
- `complete-path?`
- `path->complete-path`
- `build-path`
- `split-path`
- `path->directory-path`
- `file-name-from-path`
- `path-only`
- `simplify-path` with filesystem access disabled

For the browser target:

- A complete path starts with `/`.
- A relative path does not start with `/`.
- Empty strings and strings containing nul are rejected for path-string
  positions, except where the Racket reference explicitly allows shape
  predicates to return `#f`.
- `.` and `..` are normalized syntactically for `build-path` and
  non-filesystem `simplify-path`.
- Directory paths are represented syntactically with a trailing `/`.

The current directory can initially be a parameter whose default value is:

```text
/app/
```

Setting `current-directory` should cleanse/simplify syntactically, convert to a
directory path, and require a complete path. It should not require that the
directory exists, matching Racket's documented behavior.

## JS VFS

The JS-side VFS owns mount points and backend implementations.

Suggested interface:

```js
class WebRacketVFS {
  mount(mountPoint, backend) {}
  stat(path) {}
  readFile(path) {}
  listDir(path) {}
  writeFile(path, bytes) {}
  mkdir(path) {}
  mkdirp(path) {}
  makeParentDirectory(path) {}
  deleteFile(path) {}
  deleteDirectory(path) {}
  deleteTree(path) {}
  copyFile(srcPath, destPath, existsOk) {}
  copyTree(srcPath, destPath) {}
  rename(oldPath, newPath, existsOk) {}
  modifySeconds(path, secs) {}
  permissions(path, mode) {}
  identity(path) {}
}
```

The mount resolver should use the longest matching mount point.

Backends should operate on paths relative to their mount point, while the VFS
layer handles absolute path validation and mount dispatch.

### Status Objects

Use a small structured status value internally in JS:

```js
{
  type: "file" | "directory" | "missing",
  size: 123,
  mtime: 0,
  mode: 0o666,
  identity: 17
}
```

The host import boundary can encode this in whatever form is easiest for
WebRacket:

- integer tags plus size
- handles to JS-owned results
- serialized byte data
- direct construction of WebRacket values, if convenient

## Backends

### Memory Backend

The memory backend supports `/tmp` and tests.

It should support:

- file creation
- file replacement
- directory creation
- directory listing
- file reads
- file writes

It can ignore permissions initially.

The memory backend should preserve bytes exactly. Text-mode behavior belongs in
ports, not in the backend.

### Tar Blob Backend

The tar/blob backend supports read-only mounted files such as `/assets`.

The archive data stays on the JavaScript side as a `Blob` or `Uint8Array`.
Metadata maps paths to byte ranges in the decompressed tar data:

```json
{
  "files": [
    { "filename": "images/logo.png", "start": 512, "end": 1401 }
  ]
}
```

The backend should build:

- a file map: relative path -> byte range
- a directory map: relative directory -> child path elements

Tar entry names are interpreted as relative Unix paths. Absolute names,
empty components, `.`, and `..` are rejected when the backend indexes the
archive. Header checksums are validated while indexing. Duplicate regular-file
entries use tar extraction semantics: the last member wins. Duplicate directory
entries are harmless, but file/directory conflicts for the same path are
rejected. Ustar `prefix` fields, GNU long-name records, and POSIX pax `path`
and `mtime` records are supported.

For a read:

```js
const bytes = await blob.slice(start, end).arrayBuffer();
```

For the first implementation, it is acceptable for the backend to return a
whole file as bytes. Streaming reads can be added later with handles.

### Future IndexedDB Backend

The `/user` backend can persist user-created files through IndexedDB. It should
follow the same backend interface as memory files.

## Host Import Boundary

Keep the initial Wasm imports small. Avoid exposing a POSIX descriptor API too
early.

Initial imports can be conceptually:

```text
vfs_stat(path)       -> kind and size
vfs_read_file(path)  -> immutable bytes
vfs_write_file(path, bytes) -> status
vfs_list_dir(path)   -> list/vector of path-element bytes or strings
```

The exact Wasm signatures should follow existing WebRacket JS FFI conventions.
If direct return of compound values is awkward, use a JS handle protocol:

```text
vfs_stat_kind(path)       -> fixnum tag
vfs_stat_size(path)       -> fixnum
vfs_read_file_handle(path)-> fixnum handle
vfs_handle_to_bytes(h)    -> bytes
vfs_release_handle(h)     -> void
```

The first version should be synchronous from WebRacket's point of view. If a
backend requires async fetching, it should preload before running WebRacket or
before the mount is made visible.

## Filesystem Procedures

Initial Racket-compatible filesystem primitives:

- `file-exists?`
- `directory-exists?`
- `file-or-directory-type`
- `file-size`
- `directory-list`
- `open-input-file`
- `open-output-file`
- `call-with-input-file`
- `call-with-output-file`
- `flush-output`
- `file->bytes`
- `file->string`

`file-exists?` returns `#t` only for files, not directories.

`directory-exists?` returns `#t` only for directories.

`file-or-directory-type` returns one of:

- `'file`
- `'directory`
- `#f`

Links are out of scope initially.

`directory-list` should return path elements sorted with `path<?`. When
`#:build?` is true, combine each element with the directory path using
`build-path`.

`file-size` returns the logical byte size of a file.

## File Input Ports

Racket ports produce and consume bytes. Character operations decode bytes as
UTF-8. File input ports should behave like byte-backed input ports whose bytes
come from the VFS.

For the first implementation:

1. `open-input-file` validates and completes the path.
2. The runtime asks JS VFS for the file's bytes.
3. The runtime constructs an input port over immutable bytes.
4. The port name is the cleansed path.
5. Existing byte, char, line, peek, and location operations handle the stream.

This can reuse the same representation as `open-input-bytes`, or a new
`$InputFilePort` subtype can be added if `file-stream-port?` needs to
distinguish file-backed ports from byte/string ports.

The conservative choice is:

- add `$InputFilePort` as a subtype of `$InputPort`
- use the same fields as `$InputStringPort`
- share read logic through helper functions where possible

That keeps `file-stream-port?` meaningful without duplicating the location
rules.

## File Output Ports

The first output-file implementation uses a buffered byte-backed output port.
Writes go into the same growable byte buffer used by output string/byte ports.
`flush-output` copies the written prefix and sends it to the JS VFS through
`vfs_write_file` without closing the port. `close-output-port` flushes the
same buffer, then marks the port closed.

For the first implementation:

1. `open-output-file` validates and completes the path.
2. The runtime constructs an `$OutputFilePort` whose object name is the cleansed
   path.
3. `write-byte`, `write-bytes`, `write-char`, and `write-string` reuse the
   existing output-port machinery.
4. `flush-output` writes the current buffered bytes to the JS VFS.
5. `close-output-port` flushes the buffered bytes and then closes the port.
6. `call-with-output-file` closes the port after the procedure returns.

Keyword options are not implemented yet. Until WebRacket supports keywords,
`open-output-file` accepts one positional path argument, and
`call-with-output-file` accepts `(path proc)`. The memory backend currently
creates or replaces files on each flush.

## Location Tracking

Location tracking is required for file ports from the start.

Racket's line-counting model:

- line numbers start at `1`
- columns start at `0`
- positions start at `1`
- `port-next-location` reports the next read location
- `port-count-lines!` enables line counting for ports that do not already count

WebRacket's current string ports already track locations using `$Location`:

```wasm
(type $Location
  (sub $Heap
    (struct
      (field $hash  (mut i32))
      (field $line  (mut (ref eq)))
      (field $col   (mut (ref eq)))
      (field $pos   (mut (ref eq))))))
```

Initial file ports should count lines by default, like the current string-port
implementation.

The update rules should match the existing runtime comment near
`$make-initial-location`:

- `\n` increments line and resets column to `0`
- `\r` increments line and resets column to `0`
- `\r\n` counts as a single position for source-location purposes
- tab advances the column to one before the next multiple of `8`
- UTF-8 multibyte sequences advance byte position per byte, but column per
  decoded character

The existing `read-byte` logic already handles much of this for
`$InputStringPort`. File ports should reuse that implementation or move the
common byte-read-and-advance-location code into a helper that works for any
byte-backed input port.

`port-count-lines!` can remain a no-op for byte-backed WebRacket ports that
always count. `port-counts-lines?` should return `#t` for those ports.

## Text and Binary Mode

`open-input-file` defaults to `#:mode 'binary`.

Since the browser target follows Unix path/file conventions, `#:mode 'text` can
initially behave like `#:mode 'binary`: no newline translation. This matches
Racket's Unix behavior.

The mode argument should still be accepted and validated so programs using the
keyword do not fail unnecessarily.

## Error Behavior

The first implementation should distinguish at least:

- contract errors for invalid path arguments
- filesystem errors for missing files or wrong kind
- unsupported errors for deferred features

Exact Racket exception subtype parity can come later, but error messages should
include the primitive name and the path.

Examples:

- `open-input-file` on a missing path raises a filesystem error.
- `open-input-file` on a directory raises a filesystem error.
- `file-size` on a directory raises a filesystem error.
- `directory-list` on a file raises a filesystem error.

## Implementation Milestones

### Milestone 1: Paths

Implement Unix path values and syntactic operations:

- `$Path` constructors and predicates
- string/bytes conversion
- path-string validation
- `build-path`
- `split-path`
- `path->complete-path`
- `path->directory-path`
- `simplify-path` without filesystem access

Tests should compare against Racket behavior for Unix-style examples.

### Milestone 2: JS VFS Skeleton

Implement JS mount table and memory backend.

Add a preload/init path for mounting and seeding memory-backed files:

```js
webracketVFS.mount("/app", new WebRacketMemoryBackend(...));
webracketVFS.mount("/tmp", new WebRacketMemoryBackend());
```

Generated runtimes expose the singleton as `globalThis.webracketVFS` and
export `globalThis.WebRacketVFS` and `globalThis.WebRacketMemoryBackend` for
manual mounting. They also export `globalThis.WebRacketTarBackend` and support
pre-entry mount and preload manifests through `globalThis.WebRacketVFSMounts`
and `globalThis.WebRacketVFSPreload`. The host sets manifests before
importing/running the generated module:

```js
globalThis.WebRacketVFSMounts = {
  "/assets": { tar: { url: "./assets.tar" } }
};

globalThis.WebRacketVFSPreload = {
  "/app/data/message.txt": { text: "hello\n" },
  "/app/data/blob.bin":    { base64: "QUJD" },
  "/app/data/from-host":    { file: "./host-input.dat" },
  "/app/data/from-url":     { url: "./asset.dat" },
  "/app/assets":            { directory: "./assets" },
  "/app/images/":          { directory: true }
};
await import("./program.js");
```

The same loader is available after startup as:

```js
globalThis.preloadWebRacketVFS({
  "/tmp/input.txt": "later\n"
});

await globalThis.preloadWebRacketVFSAsync({
  "/tmp/from-url.txt": { url: "./later.txt" }
});

await globalThis.mountWebRacketVFSAsync({
  "/pkg": { tar: { url: "./pkg.tar" } }
});
```

Use `preloadWebRacketVFS` only for in-memory data such as strings, byte
arrays, `Uint8Array`, `ArrayBuffer`, `{ text: ... }`, `{ bytes: ... }`, or
`{ base64: ... }`. Use `preloadWebRacketVFSAsync` for `{ file: ... }`,
`{ url: ... }`, or `{ directory: "./host-dir" }` records, since those require
host I/O before the VFS entry can be created. Use `mountWebRacketVFSAsync` for
preparing mounted backends such as `{ tar: { file: "./assets.tar" } }`.

For generated hosts, the command line can emit this manifest directly:

```sh
racket webracket.rkt --vfs-file /app/data/message.txt=./message.txt \
                     --vfs-url /app/data/browser-message.txt=./message.txt \
                     --vfs-text /app/config.txt=mode=test \
                     --vfs-base64 /app/blob.dat=aGVsbG8= \
                     --vfs-mkdir /app/cache \
                     --vfs-dir /app/assets=./assets \
                     --vfs-tar-file /assets=./assets.tar \
                     main.rkt
```

`--vfs-url` resolves relative URLs against the generated host module URL
and works in browser hosts. `--vfs-file` and `--vfs-dir` resolve relative
paths against the generated host module URL too, but require the Node host.
`--vfs-text` and `--vfs-base64` are embedded directly in the generated host.
`--vfs-mkdir` creates an empty directory in the memory backend.
`--vfs-tar-file`, `--vfs-tar-url`, and `--vfs-tar-base64` mount read-only tar
archives at the target VFS path.
CLI preload and mount targets must be absolute VFS paths, such as
`/app/config.txt`. A preload target may not be equal to or inside an explicit
mounted backend target.

Manifest entries may be:

- an object mapping VFS paths to file data
- an array of `[path, data]` pairs
- an array of records with a `path` field

File data may be a string, `Uint8Array`, `ArrayBuffer`, typed-array view, byte
array, or an object containing `text`, `bytes`, `base64`, `file`, or `url`.
`base64` data uses the standard `A-Z`, `a-z`, `0-9`, `+`, `/`, and `=`
alphabet with padding in the final group only; an empty string represents a
zero-byte file.
`file` entries are loaded with the Node runtime before WebRacket starts. `url`
entries are fetched before WebRacket starts. Relative `file`, `url`, and
directory source paths are resolved against the generated JavaScript module URL.
Directory records use `{ directory: true }` or a path ending in `/` for an empty
VFS directory. In the Node runtime, `{ directory: "./assets" }` recursively
copies a host directory into the target VFS directory before WebRacket starts.
Manifest target paths must be absolute VFS paths. Duplicate detection treats
trailing slashes as insignificant, so `/app/cache` and `/app/cache/` are the
same preload target.

Add small host imports for stat, read-file, and list-dir.

### Milestone 3: Filesystem Predicates

Implement:

- `file-exists?`
- `directory-exists?`
- `file-or-directory-type`
- `file-size`
- `directory-list`

Use `/app` and `/tmp` test fixtures, not `/stdlib`.

### Milestone 4: File Input Ports

Implement `open-input-file` by reading file bytes from JS VFS and constructing
a byte-backed file input port.

Add:

- `file-stream-port?` for file ports
- port name as path
- close behavior
- location tracking from the first byte

Reuse existing read-byte/read-char/read-line machinery.

### Milestone 5: Tar Blob Backend

Implement read-only tar metadata backend for `/assets` or `/pkg`.

Support:

- file map
- derived directory map
- stat
- read-file
- list-dir

The tar payload remains JS-owned. Only requested file bytes cross into
WebRacket.

### Milestone 6: Convenience Procedures

Implement file convenience wrappers:

- `file->bytes`
- `file->string`
- `file->lines`
- `file->bytes-lines`

Prefer implementing these in terms of ports when practical.

### Milestone 7: Writes

Add output-file support on the memory backend:

- `open-output-file` with buffered close-time writes
- `call-with-output-file`
- `flush-output`
- basic `#:exists` modes
- `delete-file`
- `make-directory`
- `delete-directory`

Do not implement writes for tar/blob backends.

## Testing Strategy

Add tests in `test/test-basics.rkt` in the appropriate filesystem, path, and
port sections.

Use minimal WebRacket programs that exercise core behavior. Do not code around
runtime/compiler bugs in examples. If a bug appears, reduce it to a minimal
WebRacket repro and fix the core.

Important test groups:

- syntactic path normalization
- relative vs complete paths
- path-string rejection for empty/nul strings
- file/directory predicates
- sorted `directory-list`
- `open-input-file` error cases
- byte reads from file ports
- char reads from UTF-8 file data
- `read-line` modes
- `port-next-location` after LF, CR, CRLF, tabs, and multibyte UTF-8

For tar/blob backend tests, use a tiny archive mounted at `/assets` or `/pkg`.

## Open Questions

- Should path objects be visible early, or should strings be accepted first and
  paths exposed later?
- Should `current-directory` default to `/app/`, `/tmp/`, or `/`?
- Should JS VFS imports return WebRacket values directly, or use handles?
- Should file input ports copy the whole file initially, or use JS-side read
  handles from the start?
- Should `file-stream-port?` return `#t` only for a new `$InputFilePort`, or
  should all byte-backed ports remain string ports until output files exist?

The recommended answers for the first implementation are:

- expose path objects early enough to match Racket-generated path results
- default `current-directory` to `/app/`
- use the simplest host import representation already supported by WebRacket
- copy whole files initially
- add `$InputFilePort` so `file-stream-port?` can become meaningful
