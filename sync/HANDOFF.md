# Handoff Notes

## Context

- Goal: two-way Org <-> Reminders sync with Org as source of truth.
- Spec: `sync/SPEC.md`.
- Plan: `sync/PLAN.md`.
- Current branch contains many atomic commits implementing core modules.

## Current Status

- Dry-run planning path works end-to-end:
  - Elisp loads config.
  - Elisp calls Swift `orgrem list`.
  - Elisp collects Org snapshot and DB mappings.
  - Elisp computes reconciliation plan.
- Non-dry-run execution is not implemented yet:
  - no reminder apply op generation from plan
  - no Org writeback mutation flow
  - no DB update from apply results in live run

## Implemented Modules

- Swift core:
  - `sync/swift/orgrem/Sources/OrgRemCore/Schema.swift`
  - `sync/swift/orgrem/Sources/OrgRemCore/CommandParser.swift`
  - `sync/swift/orgrem/Sources/OrgRemCore/Runner.swift`
- Swift EventKit adapter:
  - `sync/swift/orgrem/Sources/orgrem/EventKitReminderStore.swift`
  - `sync/swift/orgrem/Sources/orgrem/orgrem.swift`
- Elisp core:
  - `sync/elisp/sync-model.el`
  - `sync/elisp/sync-org.el`
  - `sync/elisp/sync-paths.el`
  - `sync/elisp/sync-db.el`
  - `sync/elisp/sync-lock.el`
  - `sync/elisp/sync-merge.el`
  - `sync/elisp/sync-org-snapshot.el`
  - `sync/elisp/sync-reminders-json.el`
  - `sync/elisp/sync-engine.el`
  - `sync/elisp/sync-cli.el`

## Test Status

- Unit and component tests are passing.
- Test runners:
  - `sync/scripts/test-swift.sh`
  - `sync/scripts/test-elisp.sh`
  - `sync/scripts/test-all.sh`

## Environment Notes

- In this sandbox, Swift test execution required escalated permissions.
- Repo-level `.gitignore` was added for `.vscode/`.

## Priority Next Work

1. Add integration tests with real temporary Reminders lists.
2. Add temporary sync DB per integration test.
3. Implement non-dry-run sync apply path and DB updates.
4. Implement Org mutation/writeback path for `create-org`, `update-org`,
   and `delete-org` (cut subtree).

## Integration Test Shape (Requested)

1. Test setup:
   - create a temporary Reminders list via Swift/EventKit
   - create temp Org root with test fixtures
   - create temp sqlite DB path
2. Test run:
   - run dry-run and assert plan
   - run non-dry-run once implemented
3. Test assertions:
   - reminder creation/update/delete outcomes
   - Org creation/update/delete outcomes
   - DB mapping state transitions
4. Test teardown:
   - delete temporary reminders created during test
   - delete temporary Reminders list
   - delete temp files and DB
