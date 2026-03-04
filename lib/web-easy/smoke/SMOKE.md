# web-easy Smoke Operations

Operational commands for `lib/web-easy/smoke`.

## Canonical Entrypoints

Use `make` from repo root:

```bash
make smoke-commands
make smoke-headless-lite
make smoke-ci
```

Use `headless.sh` from `lib/web-easy/smoke`:

```bash
./headless.sh list
./headless.sh verify
./headless.sh doctor
./headless.sh contract
./headless.sh smoke
./headless.sh parity
./headless.sh dashboards
./headless.sh ci
./headless.sh timings
./headless.sh guard
./headless.sh single <compile-script> <test-page>
```

## Recommended Local Flow

Fast pre-push (no compile):

```bash
make smoke-headless-lite
```

Full pre-release (compile + headless):

```bash
make smoke-ci
```

## Task To Command

| Task | Command |
| --- | --- |
| Refresh command inventory | `make smoke-commands` |
| Fast pre-push gate (skip compile) | `make smoke-headless-lite` |
| Full local CI gate | `make smoke-ci` |
| Headless preflight only | `make smoke-verify` |
| One-page headless test | `make smoke-one SINGLE_COMPILE=... SINGLE_PAGE=...` |
| Compile smoke artifacts only | `./smoke.sh check` |
| Run headless timing snapshot | `./headless.sh timings` |
| Serve local smoke pages | `./smoke.sh open` |
| Guard self-test only | `./headless.sh guard` |

## Make Targets

- `make smoke-ci`
- `make smoke-headless-lite`
- `make smoke-verify`
- `make smoke-quick`
- `make smoke-release`
- `make smoke-list`
- `make smoke-commands`
- `make smoke-one SINGLE_COMPILE=... SINGLE_PAGE=...`

## Local Utility Wrapper (`smoke.sh`)

`smoke.sh` is for local/manual workflows only (serve + compile utilities), not headless orchestration.

```bash
./smoke.sh help
```

Available utility commands:

- `./smoke.sh check`
- `./smoke.sh parity`
- `./smoke.sh parity-check`
- `./smoke.sh rebuild`
- `./smoke.sh status`
- `./smoke.sh urls`
- `./smoke.sh dashboards`
- `./smoke.sh parity-open`
- `./smoke.sh open`
- `./smoke.sh doctor`
- `./smoke.sh clean`
- `./smoke.sh clean-dry`

## Compile Architecture

- Generated browser artifacts are written to `smoke/generated/`.
- Core smoke pages compile via `example-browser-smoke-all.rkt` selected by `?test=...`.
- Parity pages compile via `example-browser-parity-all.rkt` selected by `?test=...`.
- `check-smoke.sh` compiles:
  - `smoke-all`
  - `visual-check`
  - `parity-all`

## CSS Hook Inspection

For browser-side theming/debugging, inspect runtime nodes using:

- `data-we-widget` (stable semantic widget id)
- class names (default visual contract)

Quick check flow:

1. Run `./smoke.sh open`.
2. Open `http://localhost:8000/test-browser-visual-check.html`.
3. In DevTools, verify representative nodes expose expected hooks, for example:
   - `data-we-widget="menu-popup"` + class `we-menu-popup`
   - `data-we-widget="tab-button"` + class `we-tab-btn`
   - `data-we-widget="table-data-cell"` + class `we-table-data-cell`

## Guard + Forbidden Tokens

Guard self-test:

- `./check-dashboard-guard.sh`

Forbidden tokens checked in dashboard DOM attributes:

- `#<value>`
- `#<void>`
- `#<procedure>`
- `#<eof>`
- `[object Object]`
- `undefined`
- `NaN`
- `Infinity`

Additional dashboard guard:

- fails if any rendered widget node (`[data-we-widget]`) has a non-empty inline `style` attribute.
- current allowlist for this check: empty (no widget exceptions).

## Timing Output

Headless timing behavior:

- runners print `TIMING\tpage\tduration_ms` rows
- warning threshold from `SMOKE_WARN_MS` (default `2000`)
- per-suite timing TSV files in `/tmp`:
  - `/tmp/web-easy-contract-timings.tsv`
  - `/tmp/web-easy-smoke-timings.tsv`
  - `/tmp/web-easy-parity-timings.tsv`
- combined timing snapshot:
  - `./headless.sh timings`
  - writes `/tmp/web-easy-all-timings.tsv`

## CI

- local helper: `./check-ci-smoke.sh`
- GitHub workflow: `.github/workflows/web-easy-smoke.yml`
  - runs `./headless.sh verify`
  - runs compile path + `./headless.sh ci` (`SMOKE_SKIP_COMPILE=1`)
  - publishes headless timing artifacts

## Contract Reference

Contract behavior and expected PASS lines are documented in:

- `SMOKE-CONTRACTS.md`

## Notes

- Do not run multiple headless commands in parallel; they share generated artifacts.
- After command inventory changes, run `make smoke-commands` and commit `COMMANDS.tsv`.
