#!/bin/bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  ./commit-examples.sh [-n|--dry-run] [-m|--message "commit message"]

What it does:
  1. Stages tracked updates/deletions under examples/
  2. Stages new "relevant" files under examples/ (including new example folders)
  3. Includes generated outputs used for local runs (e.g. *.html, *.js, *.wasm)
  4. Excludes only temp/editor junk
  5. Commits only examples/** changes
EOF
}

dry_run=0
message="Update examples"

while [[ $# -gt 0 ]]; do
  case "$1" in
    -n|--dry-run)
      dry_run=1
      shift
      ;;
    -m|--message)
      shift
      if [[ $# -eq 0 ]]; then
        echo "error: --message requires an argument" >&2
        exit 2
      fi
      message="$1"
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "error: unknown argument: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

repo_root="$(git rev-parse --show-toplevel 2>/dev/null || true)"
if [[ -z "${repo_root}" ]]; then
  echo "error: not inside a git repository" >&2
  exit 2
fi
cd "${repo_root}"

is_relevant() {
  local path="$1"
  case "$path" in
    examples/raco|examples/raco/*) return 1 ;;
    examples/tmp/*|examples/*/tmp/*) return 1 ;;
    *.wat|*.wasm.map.sexp) return 1 ;;
    *~|\#*#|.#*|*.DS_Store) return 1 ;;
  esac
  return 0
}

# Stage only relevant files (tracked + new).
while IFS= read -r path; do
  [[ -n "${path}" ]] || continue
  is_relevant "${path}" || continue
  if [[ -e "${path}" ]]; then
    git add -- "${path}"
  elif git ls-files --error-unmatch -- "${path}" >/dev/null 2>&1; then
    git add -u -- "${path}"
  fi
done < <(git ls-files --modified --deleted --others --exclude-standard -- examples)

# Force exclusion of raco example even if it was staged earlier.
git reset -q HEAD -- examples/raco >/dev/null 2>&1 || true

staged="$(git diff --cached --name-only -- examples)"
if [[ -z "${staged}" ]]; then
  echo "No staged changes under examples/."
  exit 0
fi

if [[ "${dry_run}" -eq 1 ]]; then
  echo "Dry run: would commit these files under examples/:"
  echo "${staged}"
  exit 0
fi

git commit -m "${message}" -- examples
echo "Committed examples changes."
