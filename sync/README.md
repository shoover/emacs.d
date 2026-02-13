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
- Swift CLI implemented:
  - command parsing
  - JSON list/apply contract models
  - EventKit-backed list/apply store
  - conflict and not_found apply statuses
- Elisp sync flow implemented:
  - dry-run planning path
  - non-dry-run execution path
  - reminder apply operation generation/execution
  - Org create/update/delete writeback operations
  - sqlite mapping persistence updates from sync outcomes
- Integration coverage exists (opt-in live suite):
  - list/apply roundtrip
  - end-to-end Emacs sync with temporary Org root and DB
  - bi-directional create/update/delete propagation
  - conflict/not_found handling
  - idempotent second run
  - scheduled date mapping both directions

## Test Commands

Use `just` from the repo root:

- `just sync-test-swift`
- `just sync-test-elisp`
- `just sync-test`
- `just sync-test-integration` (opt-in live Reminders integration;
  requires macOS Reminders access and `emacs` in PATH; uses Swift
  Testing output, where the XCTest compatibility summary may still show
  `Executed 0 tests`)

Direct script equivalents:

- `sync/scripts/test-swift.sh`
- `sync/scripts/test-elisp.sh`
- `sync/scripts/test-all.sh`
- `sync/scripts/test-integration.sh`

## How To Use

### Prerequisites

- macOS with Reminders access granted to `orgrem`/Terminal.
- Emacs with Org mode and sqlite support.
- `just` installed.
- A Reminders list target, by either:
  - ID (`calendarIdentifier`) via `:reminders-list-id`, or
  - name via `:reminders-list-name` (auto-created if missing).

### 1. Build the `orgrem` binary

From the repo root:

```bash
just sync-build
```

Debug build:

```bash
just sync-build-debug
```

Binary path example:

- `sync/swift/orgrem/.build/release/orgrem`

### 2. Create a sync config file

Create a file such as `~/.config/orgrem-sync.el`:

```elisp
(:org-root "/Users/you/iCloud/notes"
 :inbox-file "/Users/you/iCloud/notes/inbox.org"
 :inbox-heading "* Reminders"
 :db-path "/Users/you/.local/state/orgrem/sync.sqlite"
 :reminders-list-name "Personal"
 :orgrem-bin "/Users/you/emacs/sync/swift/orgrem/.build/release/orgrem")
```

Notes:

- `:org-root`, `:db-path`, and `:orgrem-bin` are required.
- Set exactly one of:
  - `:reminders-list-id` (existing list ID), or
  - `:reminders-list-name` (resolved by name and auto-created if missing).
- `:inbox-file` and `:inbox-heading` are required when reminders need to
  be created in Org.
- Only TODO entries under `:org-root` are synced.
- Missing `:ID:` properties are created automatically during sync.

Optional helper command (returns `{"id":"...","title":"..."}`):

```bash
sync/swift/orgrem/.build/release/orgrem ensure-list --title "Personal"
```

### 3. Run a dry run (preview plan only)

```bash
just sync-dry-run
```

With an explicit config path:

```bash
just sync-dry-run config="$HOME/.config/orgrem-sync.el"
```

### 4. Run live sync

```bash
just sync-run
```

With an explicit config path:

```bash
just sync-run config="$HOME/.config/orgrem-sync.el"
```

Direct `emacs --batch` equivalents remain in `Justfile` recipes.

## Known Gaps

- `apply` field semantics do not yet distinguish omitted fields from
  explicit JSON `null` for partial updates.
- Live integration matrix is not complete yet (for example recurrence
  and time-zone edge cases).
- Hardening tasks are still open (structured operation logs, retry
  policy, and recovery documentation).
