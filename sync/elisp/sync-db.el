;;; sync-db.el --- SQLite persistence for sync mappings -*- lexical-binding: t; -*-

;;; Code:

(defconst org-rem--db-columns
  '(org_id
    org_file
    org_locator
    org_hash
    reminder_external_id
    reminder_last_modified
    last_synced_at)
  "Columns stored in the mapping table.")

(defun org-rem--sqlite-db-valid-p (db)
  "Return non-nil when DB is a valid sqlite handle."
  (and db (sqlitep db)))

(defun org-rem-db-open (path)
  "Open sqlite DB at PATH."
  (let ((dir (file-name-directory path)))
    (when dir
      (make-directory dir t)))
  (let ((db (sqlite-open path)))
    (unless (org-rem--sqlite-db-valid-p db)
      (error "Failed to open sqlite database at %s" path))
    db))

(defun org-rem-db-close (db)
  "Close sqlite DB connection DB."
  (when (org-rem--sqlite-db-valid-p db)
    (sqlite-close db)))

(defun org-rem-db-init (db)
  "Ensure schema exists in DB."
  (sqlite-execute
   db
   (concat
    "CREATE TABLE IF NOT EXISTS mappings ("
    "org_id TEXT PRIMARY KEY, "
    "org_file TEXT NOT NULL, "
    "org_locator TEXT NOT NULL, "
    "org_hash TEXT NOT NULL, "
    "reminder_external_id TEXT UNIQUE NOT NULL, "
    "reminder_last_modified TEXT, "
    "last_synced_at TEXT"
    ")"))
  (sqlite-execute
   db
   "CREATE INDEX IF NOT EXISTS idx_mappings_reminder_external_id ON mappings(reminder_external_id)"))

(defun org-rem--row->alist (row)
  "Convert sqlite ROW list to an alist."
  (let ((result nil)
        (values row))
    (dolist (column org-rem--db-columns (nreverse result))
      (push (cons column (pop values)) result))))

(defun org-rem-db-upsert-mapping (db mapping)
  "Insert or update MAPPING in DB."
  (sqlite-execute
   db
   (concat
    "INSERT INTO mappings ("
    "org_id, org_file, org_locator, org_hash, "
    "reminder_external_id, reminder_last_modified, last_synced_at"
    ") VALUES (?, ?, ?, ?, ?, ?, ?) "
    "ON CONFLICT(org_id) DO UPDATE SET "
    "org_file=excluded.org_file, "
    "org_locator=excluded.org_locator, "
    "org_hash=excluded.org_hash, "
    "reminder_external_id=excluded.reminder_external_id, "
    "reminder_last_modified=excluded.reminder_last_modified, "
    "last_synced_at=excluded.last_synced_at")
   (list (alist-get 'org_id mapping)
         (alist-get 'org_file mapping)
         (alist-get 'org_locator mapping)
         (alist-get 'org_hash mapping)
         (alist-get 'reminder_external_id mapping)
         (alist-get 'reminder_last_modified mapping)
         (alist-get 'last_synced_at mapping))))

(defun org-rem-db-get-by-org-id (db org-id)
  "Fetch mapping row by ORG-ID."
  (when-let ((row (car (sqlite-select
                        db
                        (concat
                         "SELECT org_id, org_file, org_locator, org_hash, "
                         "reminder_external_id, reminder_last_modified, last_synced_at "
                         "FROM mappings WHERE org_id = ?")
                        (list org-id)))))
    (org-rem--row->alist row)))

(defun org-rem-db-get-by-reminder-id (db reminder-external-id)
  "Fetch mapping row by REMINDER-EXTERNAL-ID."
  (when-let ((row (car (sqlite-select
                        db
                        (concat
                         "SELECT org_id, org_file, org_locator, org_hash, "
                         "reminder_external_id, reminder_last_modified, last_synced_at "
                         "FROM mappings WHERE reminder_external_id = ?")
                        (list reminder-external-id)))))
    (org-rem--row->alist row)))

(defun org-rem-db-list-mappings (db)
  "Return all mapping rows."
  (mapcar
   #'org-rem--row->alist
   (sqlite-select
    db
    (concat
     "SELECT org_id, org_file, org_locator, org_hash, "
     "reminder_external_id, reminder_last_modified, last_synced_at "
     "FROM mappings ORDER BY org_id"))))

(defun org-rem-db-delete-by-org-id (db org-id)
  "Delete mapping row by ORG-ID."
  (sqlite-execute db "DELETE FROM mappings WHERE org_id = ?" (list org-id)))

(defun org-rem-db-delete-by-reminder-id (db reminder-external-id)
  "Delete mapping row by REMINDER-EXTERNAL-ID."
  (sqlite-execute
   db
   "DELETE FROM mappings WHERE reminder_external_id = ?"
   (list reminder-external-id)))

(provide 'sync-db)

;;; sync-db.el ends here
