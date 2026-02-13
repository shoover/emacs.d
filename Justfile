set shell := ["zsh", "-cu"]

sync_config := "${HOME}/.config/orgrem-sync.el"

default:
  @just --list

# Build orgrem (release)
sync-build:
  cd sync/swift/orgrem && swift build -c release --product orgrem

# Build orgrem (debug)
sync-build-debug:
  cd sync/swift/orgrem && swift build -c debug --product orgrem

# Run Swift tests
sync-test-swift:
  sync/scripts/test-swift.sh

# Run Elisp tests
sync-test-elisp:
  sync/scripts/test-elisp.sh

# Run all default tests
sync-test:
  sync/scripts/test-all.sh

# Run opt-in live integration tests
sync-test-integration:
  sync/scripts/test-integration.sh

# Dry-run sync and print reconciliation plan
sync-dry-run config=sync_config:
  emacs --batch -Q \
    -L sync/elisp \
    --eval "(progn \
               (setq org-agenda-files nil) \
               (setq org-agenda-skip-unavailable-files t) \
               (require 'sync-cli) \
               (prin1 (org-rem-sync-run \"{{config}}\" :dry-run t)) \
               (terpri))"

# Live sync execution
sync-run config=sync_config:
  emacs --batch -Q \
    -L sync/elisp \
    --eval "(progn \
               (setq org-agenda-files nil) \
               (setq org-agenda-skip-unavailable-files t) \
               (require 'sync-cli) \
               (org-rem-sync-run \"{{config}}\"))"
