#!/usr/bin/env zsh
set -euo pipefail

emacs --batch -Q \
  -L sync/elisp \
  -L sync/elisp/test \
  -l sync/elisp/test/test-runner.el \
  -f ert-run-tests-batch-and-exit
