# Org <-> Reminders Sync Spec (v1.2)

## Goals

- Two-way sync between Org files in `~/iCloud/notes/` and one Reminders
  list.
- Org is source of truth when both sides changed.
- Immediate delete propagation in both directions.
- No sync metadata stored in Reminders notes/title/body.

## Architecture

- Orchestrator: Emacs Lisp, batch mode (`emacs --batch`).
- Reminders adapter: custom Swift CLI using EventKit.
- State store: local SQLite DB (managed by Elisp).
- No dependency on `remindctl` or other third-party reminder CLIs.

## Scope

- Org root: `~/iCloud/notes/`.
- Include: all TODO entries.
- Exclude: archive files (for example `*.org_archive`).
- Target: one configured Reminders list.
- New reminders from iOS: write to configurable Org file under heading
  `* Reminders`.

## Identity

- Org key: `:ID:` property.
- If `:ID:` missing, generate one during sync.
- Reminders key: `calendarItemExternalIdentifier`.
- Local DB maps Org ID <-> Reminders external ID.

## Field Mapping

- Title: Org headline <-> Reminders title.
- Completion: Org done-state <-> Reminders completed boolean.
- Date: Org `SCHEDULED` <-> Reminders due date.
- Notes: Org body <-> Reminders notes (verbatim).
- URL: Org `:URL:` property <-> Reminders URL.
- Priority: ignored.
- Tags: encoded as trailing `#tag` tokens in Reminders title.

## Tag Encoding Rules

- Parse tags only from trailing tokens matching
  `#[A-Za-z0-9_-]+`.
- Tags in middle of title are treated as title text.
- On Org -> Reminders writes, append tags in sorted order.
- Example: `Pay rent #finance #home`.

## Date Rules

- Only Org `SCHEDULED` is synced (no `DEADLINE` support in this spec).
- `SCHEDULED` maps to Reminders `dueDateComponents`.
- When due exists, also set `startDateComponents` to same value for iOS
  compatibility.
- If `SCHEDULED` is absent, clear due/start in Reminders.

## TODO Keyword Semantics

- TODO states are file-local and must be read from each file's buffer
  settings.
- Org -> Reminders completion:
  - done keyword => `completed=true`
  - open keyword => `completed=false`
- Reminders -> Org completion:
  - `false -> true`: set file default done keyword.
  - `true -> false`: set file default open keyword.
- Defaults:
  - open keyword: first keyword in first open set, fallback `TODO`.
  - done keyword: first keyword in first done set, fallback `DONE`.
- Existing open states (`next`, `wait`, `someday`, etc.) are preserved
  unless completion toggles in Reminders.
- New Org entries created from Reminders use file default open keyword.

## Conflict and Merge

- Org change detection: normalized content hash from parsed subtree.
- Reminders change detection: `lastModifiedDate`.
- If only one side changed since last sync, apply that side.
- If both changed, Org wins for all mapped fields.
- Merge model is stateful sync with DB, not CRDT.

## Deletion Rules

- If mapped Org entry disappears from sync scope, delete reminder
  immediately.
- If mapped reminder disappears, cut Org subtree immediately.
- Archive move counts as "disappears from scope", therefore deletes
  mapped reminder.

## Local DB (Minimum Schema)

- `org_id` TEXT PRIMARY KEY
- `org_file` TEXT
- `org_locator` TEXT (outline path or marker surrogate)
- `org_hash` TEXT
- `reminder_external_id` TEXT UNIQUE
- `reminder_last_modified` TEXT
- `last_synced_at` TEXT

## Swift CLI Contract

### `list`

Command:

- `orgrem list --list-id <id>`

JSON output:

```json
{
  "schema_version": 1,
  "generated_at": "2026-02-12T21:40:00Z",
  "list": {
    "id": "A1B2C3...",
    "title": "Personal"
  },
  "items": [
    {
      "external_id": "x-apple-reminder://...",
      "local_id": "2D8E...",
      "title": "Pay rent #finance #home",
      "notes": "verbatim text",
      "completed": false,
      "completion_date": null,
      "start": null,
      "due": {
        "year": 2026,
        "month": 2,
        "day": 15,
        "hour": null,
        "minute": null,
        "time_zone": null
      },
      "url": "https://example.com",
      "last_modified": "2026-02-12T21:35:18Z"
    }
  ]
}
```

### `apply`

Command:

- `orgrem apply --list-id <id> --ops-file <path>`

Input JSON:

```json
{
  "schema_version": 1,
  "ops": [
    {
      "op": "create",
      "client_ref": "org-id-123",
      "fields": {
        "title": "Pay rent #finance #home",
        "notes": "verbatim text",
        "completed": false,
        "start": {
          "year": 2026,
          "month": 2,
          "day": 15,
          "hour": null,
          "minute": null,
          "time_zone": null
        },
        "due": {
          "year": 2026,
          "month": 2,
          "day": 15,
          "hour": null,
          "minute": null,
          "time_zone": null
        },
        "url": "https://example.com"
      }
    },
    {
      "op": "update",
      "external_id": "x-apple-reminder://...",
      "if_last_modified": "2026-02-12T21:35:18Z",
      "fields": {
        "title": "Pay rent #finance #home",
        "notes": "updated",
        "completed": true,
        "due": null
      }
    },
    {
      "op": "delete",
      "external_id": "x-apple-reminder://...",
      "if_last_modified": "2026-02-12T21:35:18Z"
    }
  ]
}
```

Output JSON:

```json
{
  "schema_version": 1,
  "applied_at": "2026-02-12T21:40:30Z",
  "results": [
    {
      "op_index": 0,
      "status": "ok",
      "client_ref": "org-id-123",
      "item": {
        "external_id": "x-apple-reminder://new...",
        "local_id": "7AA1...",
        "last_modified": "2026-02-12T21:40:30Z"
      }
    },
    {
      "op_index": 1,
      "status": "ok",
      "item": {
        "external_id": "x-apple-reminder://...",
        "local_id": "2D8E...",
        "last_modified": "2026-02-12T21:40:30Z"
      }
    },
    {
      "op_index": 2,
      "status": "not_found"
    }
  ]
}
```

Allowed status values:

- `ok`
- `not_found`
- `conflict` (for `if_last_modified` mismatch)
- `error` (with `code` and `message`)

## Canonical Org Hash

- Algorithm: `sha256` over canonical JSON bytes (UTF-8).
- Stable key order and no insignificant whitespace.

Canonical shape:

```json
{
  "title": "Pay rent",
  "tags": [
    "finance",
    "home"
  ],
  "todo_state": "TODO",
  "scheduled": {
    "year": 2026,
    "month": 2,
    "day": 15,
    "hour": null,
    "minute": null,
    "time_zone": null
  },
  "notes": "verbatim text",
  "url": "https://example.com",
  "completed": false
}
```

Normalization:

- `title`: Org headline without trailing tag syntax.
- `tags`: lowercase, deduped, sorted ascending.
- `todo_state`: exact current keyword string.
- `scheduled`: parsed `SCHEDULED`; null if absent.
- `notes`: normalize line endings to `\n`; strip trailing newlines.
- `url`: trim whitespace; null if empty.
- `completed`: derived from file-local done-state mapping.

## Operational Safety

- Use lock file to prevent concurrent sync runs.
- Support dry-run mode (compute and print plan, no writes).
- Log per-run counters:
  - Org created/updated/deleted
  - Reminders created/updated/deleted
