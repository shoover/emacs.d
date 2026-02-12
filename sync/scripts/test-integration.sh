#!/usr/bin/env zsh
set -euo pipefail

cd sync/swift/orgrem

swift build -c debug --product orgrem

ORGREM_BIN="$(find .build -type f -path '*/debug/orgrem' | head -n1)"
if [[ -z "${ORGREM_BIN}" ]]; then
  echo "Unable to locate built orgrem binary under .build/" >&2
  exit 1
fi

REPO_ROOT="$(cd ../../.. && pwd)"

ORGREM_RUN_INTEGRATION=1 \
ORGREM_INTEGRATION_BIN="$(pwd)/${ORGREM_BIN}" \
ORGREM_REPO_ROOT="${REPO_ROOT}" \
ORGREM_INTEGRATION_EMACS="${ORGREM_INTEGRATION_EMACS:-emacs}" \
swift test --filter live
