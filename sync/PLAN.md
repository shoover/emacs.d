# Implementation Plan

## TDD Rule

1. For each phase, write tests first for behavior that can be isolated.
2. Implement only enough code to pass those tests.
3. Keep a short red/green/refactor loop per module.
4. Integration tests run continuously, not only at the end.

## Workflow Discipline

1. Work in small, sequential steps with one clear outcome per step.
2. Keep commits atomic: each commit should contain one logical change.
3. Add `just` targets only when the underlying behavior already exists
   and passes tests.
4. Do not add placeholder task-runner targets.

## Progress Snapshot (2026-02-12)

1. `Completed`:
   - Phase 1 repository skeleton and test harnesses.
   - Phase 2 Swift contract models, command parser, runner, and EventKit
     store for `list` and `apply`.
   - Phase 3 DB layer and lock handling.
   - Phase 4 core Org read path: file discovery, file-local TODO mapping,
     tag handling, canonical hash, and snapshot collection with ID
     creation.
   - Phase 5 planning path: merge planner and engine composition.
   - Phase 5 reminder apply execution path:
     - plan -> reminder apply ops
     - `orgrem apply` invocation
     - DB mapping updates from apply results
   - Phase 6 dry-run CLI path: config load, `orgrem list`, and plan
     generation.
   - Phase 6 non-dry-run reminder execution path with guard that blocks
     runs requiring Org writeback mutations.
2. `In Progress`:
   - Phase 4 Org write mutations (`create/update/delete` subtree
     operations in live sync flow).
   - Phase 5 writeback completion for plans that include Org-side
     changes.
3. `Not Started`:
   - Phase 7 integration tests against live temporary Reminders lists.
   - Phase 7 hardening tasks (retry/logging/recovery docs).
4. `Immediate Next Steps`:
   - Implement Org mutation/writeback operations for:
     - `create-org`
     - `update-org`
     - `delete-org` (cut subtree)
   - Add opt-in integration test suite that creates and deletes a
     temporary Reminders list per test and uses a temporary sync DB.
   - Wire non-dry-run end-to-end so reminder-side apply and Org-side
     writeback run in one execution path.

## Phase 1: Repository Skeleton

1. Create directories:
   - `sync/elisp/`
   - `sync/swift/orgrem/`
   - `sync/state/` (runtime DB location, gitignored)
2. Add `sync/README.md` with run instructions.
3. Add `.gitignore` entries for DB, temp JSON, and logs.
4. Add initial test harnesses:
   - Swift package test target for `orgrem`
   - Elisp ERT test file and batch test runner script

## Phase 2: Swift EventKit CLI (`orgrem`)

1. TDD: JSON codec and schema tests first.
2. Build command skeleton:
   - `orgrem list --list-id <id>`
   - `orgrem apply --list-id <id> --ops-file <path>`
3. Implement reminder model with required fields:
   - `external_id`, `local_id`, `title`, `notes`, `completed`,
     `completion_date`, `start`, `due`, `url`, `last_modified`, `list_id`
4. TDD: operation contract tests (`create`, `update`, `delete`,
   `conflict`, `not_found`) against mocked store.
5. Implement list retrieval for a specific list ID.
6. Implement apply operations:
   - `create`, `update`, `delete`
7. Resolve update/delete by external ID:
   - `calendarItems(withExternalIdentifier:)`
8. Add conflict precondition:
   - `if_last_modified` check, return `conflict`.
9. Emit JSON exactly per `sync/SPEC.md`.
10. Integration sanity tests on a local list.

## Phase 3: Elisp Data Model + DB Layer

1. TDD: ERT tests for DB schema bootstrap and CRUD behavior.
2. Define Elisp structs/plists for:
   - Org item
   - Reminder item
   - Mapping row
   - Planned op
3. Implement SQLite schema bootstrap/migration in Elisp.
4. Implement DB functions:
   - upsert mapping
   - lookup by org ID
   - lookup by reminder external ID
   - list all mappings
   - delete mapping
5. TDD: lock behavior tests (second run blocked, stale lock recovery).
6. Implement lock file handling for single-run guarantee.

## Phase 4: Org Read/Write Layer

1. TDD: parse/render tests for title tags and canonical hash.
2. TDD: file-local TODO keyword mapping tests (`#+TODO` variants).
3. Enumerate Org files under `~/iCloud/notes/` with archive exclusion.
4. Parse all TODO entries and ensure `:ID:` exists.
5. Read file-local TODO keyword sets per file.
6. Convert entry to canonical hash object and hash string.
7. Parse/write title tags (`#tag` suffix rules).
8. TDD: subtree mutation tests in temp buffers.
9. Implement create/update/delete Org subtree operations:
   - delete behavior: cut subtree
10. Implement insertion for new reminders:
   - configurable file + heading `* Reminders`.

## Phase 5: Sync Engine

1. TDD: decision-matrix tests for merge/deletes/conflicts.
2. Fetch current reminders snapshot via `orgrem list`.
3. Load Org snapshot and DB snapshot.
4. Build reconciliation sets:
   - mapped present on both
   - mapped missing on one side
   - unmapped Org-only
   - unmapped Reminder-only
5. Apply deletion rules immediately both directions.
6. Apply update/create rules with Org-wins-on-conflict logic.
7. Build ops JSON for Swift apply.
8. Apply reminder ops and handle per-op statuses.
9. Apply Org mutations and persist DB updates atomically by phase.
10. TDD: end-to-end dry-run fixtures for stable reconciliation output.

## Phase 6: CLI Entrypoints

1. TDD: CLI argument parsing and config resolution tests.
2. Add Elisp batch command:
   - `sync-run`
3. Flags:
   - `--dry-run`
   - `--config <path>`
   - `--verbose`
4. Config file values:
   - reminders list ID
   - org root
   - inbox file
   - inbox heading
   - DB path
   - path to `orgrem` binary
5. TDD: smoke test running full sync in dry-run mode.

## Phase 7: Integration and Hardening

1. Scripted integration tests:
   - create/update/delete each direction
   - archive behavior (out-of-scope delete)
   - concurrent edit with Org-wins
   - reminder disappeared -> Org cut subtree
2. Structured logs for each run with operation counts.
3. Retry policy for transient EventKit fetch/apply failures.
4. Safe handling for stale external IDs (`not_found` path).
5. Document recovery workflow for external ID drift.

## Delivery Order

1. Swift `list` first.
2. Elisp Org parsing + hashing second.
3. DB bootstrap third.
4. One-way Org -> Reminders write path fourth.
5. Full two-way merge and deletes fifth.
6. Hardening and integration expansion last.
