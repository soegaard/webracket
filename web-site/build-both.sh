#!/bin/bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")" && pwd)"
SRC_DIR="${ROOT_DIR}/src"
NEW_SRC_DIR="${ROOT_DIR}/../web-site-new/src"
NEW_SITE_LOCAL_DIR="${ROOT_DIR}/../web-site-new/local"
OLD_SITE_NEW_DIR="${ROOT_DIR}/local/new"

cd "${SRC_DIR}"

format_generated_output() {
  sed -E 's#^Generated (.+) and (.+) \((.+ docs)\)\.$#Generated\n  \1\n  \2 \n  (\3).#'
}

echo "-- Generating FFI doc pages (shared) --"
racket generate-ffi-doc-pages.rkt | format_generated_output
racket generate-ffi-doc-pages-structured.rkt | format_generated_output

SKIP_FFI_DOC_GEN=1 SKIP_SOUND=1 ./build.sh &
pid_site=$!

(
  cd "${NEW_SRC_DIR}"
  SKIP_SOUND=1 ./build.sh
) &
pid_new=$!

set +e
wait "${pid_site}"
status_site=$?
wait "${pid_new}"
status_new=$?
set -e

if [[ ${status_site} -ne 0 || ${status_new} -ne 0 ]]; then
  kill "${pid_site}" "${pid_new}" 2>/dev/null || true
  echo "build-both.sh: build failed (old-build.sh=${status_site}, new-build.sh=${status_new})" >&2
  exit 1
fi

if [[ ! -d "${NEW_SITE_LOCAL_DIR}" ]]; then
  echo "build-both.sh: missing source directory: ${NEW_SITE_LOCAL_DIR}" >&2
  exit 1
fi

mkdir -p "${OLD_SITE_NEW_DIR}"
rm -rf "${OLD_SITE_NEW_DIR:?}/"*
cp -a "${NEW_SITE_LOCAL_DIR}/". "${OLD_SITE_NEW_DIR}/"

echo "build-both.sh: both builds completed successfully"
