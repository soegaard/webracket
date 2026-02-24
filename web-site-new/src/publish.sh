#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WEB_SITE_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
LOCAL_DIR="${WEB_SITE_DIR}/local"
PUBLIC_DIR="${WEB_SITE_DIR}/public"
ARCHIVE_DIR="${WEB_SITE_DIR}/archives"
STAMP="$(date +%Y%m%d-%H%M%S)"

if [[ ! -d "${LOCAL_DIR}" ]]; then
  echo "Error: ${LOCAL_DIR} does not exist. Run ./build.sh first."
  exit 1
fi

if [[ ! -f "${LOCAL_DIR}/index.html" ]]; then
  echo "Error: ${LOCAL_DIR}/index.html is missing. Run ./build.sh first."
  exit 1
fi

mkdir -p "${ARCHIVE_DIR}"

if [[ -d "${PUBLIC_DIR}" ]]; then
  ARCHIVE_FILE="${ARCHIVE_DIR}/public-${STAMP}.tar.gz"
  echo "-- Archiving current public/ to ${ARCHIVE_FILE} --"
  tar -czf "${ARCHIVE_FILE}" -C "${WEB_SITE_DIR}" public
else
  mkdir -p "${PUBLIC_DIR}"
fi

echo "-- Publishing local/ to public/ --"
find "${PUBLIC_DIR}" -mindepth 1 ! -name 'README.md' -exec rm -rf {} +
cp -a "${LOCAL_DIR}/." "${PUBLIC_DIR}/"

echo "-- Done --"
