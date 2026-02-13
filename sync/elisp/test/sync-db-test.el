;;; sync-db-test.el --- Tests for SQLite mapping store -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'sync-db)

(defmacro org-rem--with-temp-db (&rest body)
  "Run BODY with a temporary sqlite DB connection."
  (declare (indent 0))
  `(let* ((db-path (make-temp-file "org-rem-sync-db" nil ".sqlite"))
          (db (org-rem-db-open db-path)))
     (unwind-protect
         (progn
           (org-rem-db-init db)
           ,@body)
       (org-rem-db-close db)
       (when (file-exists-p db-path)
         (delete-file db-path)))))

(ert-deftest org-rem-db-upsert-and-lookup-by-org-id ()
  (org-rem--with-temp-db
    (org-rem-db-upsert-mapping
     db
     '((org_id . "ORG-1")
       (org_file . "/tmp/a.org")
       (org_locator . "a>1")
       (org_hash . "h1")
       (reminder_external_id . "RID-1")
       (reminder_last_modified . "2026-02-12T22:00:00Z")
       (last_synced_at . "2026-02-12T22:01:00Z")))
    (let ((row (org-rem-db-get-by-org-id db "ORG-1")))
      (should row)
      (should (equal (alist-get 'org_id row) "ORG-1"))
      (should (equal (alist-get 'reminder_external_id row) "RID-1")))))

(ert-deftest org-rem-db-upsert-updates-existing-row ()
  (org-rem--with-temp-db
    (org-rem-db-upsert-mapping
     db
     '((org_id . "ORG-1")
       (org_file . "/tmp/a.org")
       (org_locator . "a>1")
       (org_hash . "h1")
       (reminder_external_id . "RID-1")
       (reminder_last_modified . "2026-02-12T22:00:00Z")
       (last_synced_at . "2026-02-12T22:01:00Z")))
    (org-rem-db-upsert-mapping
     db
     '((org_id . "ORG-1")
       (org_file . "/tmp/a.org")
       (org_locator . "a>1")
       (org_hash . "h2")
       (reminder_external_id . "RID-1")
       (reminder_last_modified . "2026-02-12T22:02:00Z")
       (last_synced_at . "2026-02-12T22:03:00Z")))
    (let ((row (org-rem-db-get-by-org-id db "ORG-1")))
      (should (equal (alist-get 'org_hash row) "h2"))
      (should (equal (alist-get 'reminder_last_modified row)
                     "2026-02-12T22:02:00Z")))))

(ert-deftest org-rem-db-lookup-by-reminder-id-and-delete ()
  (org-rem--with-temp-db
    (org-rem-db-upsert-mapping
     db
     '((org_id . "ORG-2")
       (org_file . "/tmp/b.org")
       (org_locator . "b>1")
       (org_hash . "h1")
       (reminder_external_id . "RID-2")
       (reminder_last_modified . "2026-02-12T22:00:00Z")
       (last_synced_at . "2026-02-12T22:01:00Z")))
    (should (equal (alist-get 'org_id (org-rem-db-get-by-reminder-id db "RID-2"))
                   "ORG-2"))
    (org-rem-db-delete-by-org-id db "ORG-2")
    (should-not (org-rem-db-get-by-org-id db "ORG-2"))))

(ert-deftest org-rem-db-delete-by-reminder-id ()
  (org-rem--with-temp-db
    (org-rem-db-upsert-mapping
     db
     '((org_id . "ORG-3")
       (org_file . "/tmp/c.org")
       (org_locator . "c>1")
       (org_hash . "h1")
       (reminder_external_id . "RID-3")
       (reminder_last_modified . "2026-02-12T22:00:00Z")
       (last_synced_at . "2026-02-12T22:01:00Z")))
    (org-rem-db-delete-by-reminder-id db "RID-3")
    (should-not (org-rem-db-get-by-org-id db "ORG-3"))))

(ert-deftest org-rem-db-list-all-mappings ()
  (org-rem--with-temp-db
    (org-rem-db-upsert-mapping
     db
     '((org_id . "ORG-A")
       (org_file . "/tmp/a.org")
       (org_locator . "a>1")
       (org_hash . "ha")
       (reminder_external_id . "RID-A")
       (reminder_last_modified . "2026-02-12T22:00:00Z")
       (last_synced_at . "2026-02-12T22:01:00Z")))
    (org-rem-db-upsert-mapping
     db
     '((org_id . "ORG-B")
       (org_file . "/tmp/b.org")
       (org_locator . "b>1")
       (org_hash . "hb")
       (reminder_external_id . "RID-B")
       (reminder_last_modified . "2026-02-12T22:00:00Z")
       (last_synced_at . "2026-02-12T22:01:00Z")))
    (let ((rows (org-rem-db-list-mappings db)))
      (should (= (length rows) 2))
      (should (equal (mapcar (lambda (row) (alist-get 'org_id row)) rows)
                     '("ORG-A" "ORG-B"))))))

(ert-deftest org-rem-db-open-creates-parent-directories ()
  (let* ((root (make-temp-file "org-rem-sync-db-root" t))
         (db-path (expand-file-name "nested/state/sync.sqlite" root))
         (db nil))
    (unwind-protect
        (progn
          (setq db (org-rem-db-open db-path))
          (org-rem-db-init db)
          (should (file-exists-p db-path)))
      (org-rem-db-close db)
      (delete-directory root t))))

(provide 'sync-db-test)

;;; sync-db-test.el ends here
