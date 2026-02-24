#!/bin/bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")" && pwd)"
EXAMPLES_DIR="${ROOT_DIR}/../examples"

echo "-- Building repository examples --"
(cd "${EXAMPLES_DIR}" && ./build.sh)
echo "-- Done --"
