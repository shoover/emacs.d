#!/usr/bin/env zsh
set -euo pipefail

cd sync/swift/orgrem

swift build -c debug --product orgrem

ORGREM_BIN="$(find .build -type f -path '*/debug/orgrem' | head -n1)"
if [[ -z "${ORGREM_BIN}" ]]; then
  echo "Unable to locate built orgrem binary under .build/" >&2
  exit 1
fi

ORGREM_RUN_INTEGRATION=1 \
ORGREM_INTEGRATION_BIN="$(pwd)/${ORGREM_BIN}" \
swift test --filter liveListAndApplyRoundTrip
