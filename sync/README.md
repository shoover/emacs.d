# Org <-> Reminders Sync

This directory contains the sync implementation and design docs for
two-way synchronization between Org files and a macOS Reminders list.

## Structure

- `SPEC.md`: behavior and data contract.
- `PLAN.md`: implementation phases and delivery order.
- `elisp/`: Org parsing, merge engine, and DB orchestration.
- `swift/orgrem/`: EventKit CLI used by the Elisp orchestrator.
- `state/`: local runtime state (SQLite DB, lock files, temp JSON).

## Development Workflow

- Work in small steps with atomic commits.
- Follow TDD for units that can be isolated.
- Add task runner targets only when they are fully supported.

## Current Status

- Design/spec completed.
- Swift CLI skeleton implemented:
  - command parsing
  - JSON list/apply contract models
  - EventKit-backed list/apply store
- Elisp core implemented:
  - Org snapshot collection with ID creation
  - canonical hashing
  - file-local TODO semantics
  - sqlite mapping store
  - lock handling
  - merge planner and engine composition

## Test Commands

- `sync/scripts/test-swift.sh`
- `sync/scripts/test-elisp.sh`
- `sync/scripts/test-all.sh`
- `sync/scripts/test-integration.sh` (opt-in live Reminders integration;
  requires macOS Reminders access and `emacs` in PATH; uses Swift
  Testing output, where the XCTest compatibility summary may still show
  `Executed 0 tests`)

## Known Gaps

- Full end-to-end sync execution and writeback loop is not wired yet.
- `apply` field semantics do not yet distinguish omitted fields from
  explicit JSON `null` for partial updates.
