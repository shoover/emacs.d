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
- Implementation scaffolding in progress.
