;;; sync-engine-test.el --- Tests for sync engine assembly -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'sync-db)
(require 'sync-engine)

(defmacro org-rem--with-temp-root-and-db (bindings &rest body)
  "Create temp ROOT-VAR and DB-VAR bindings and run BODY."
  (declare (indent 1))
  (let ((root-var (nth 0 bindings))
        (db-var (nth 1 bindings))
        (db-path-var (nth 2 bindings)))
    `(let* ((,root-var (make-temp-file "org-rem-engine" t))
            (,db-path-var (make-temp-file "org-rem-engine-db" nil ".sqlite"))
            (,db-var (org-rem-db-open ,db-path-var)))
     (unwind-protect
         (progn
           (org-rem-db-init ,db-var)
           ,@body)
       (org-rem-db-close ,db-var)
       (when (file-exists-p ,db-path-var)
         (delete-file ,db-path-var))
       (delete-directory ,root-var t)))))

(ert-deftest org-rem-build-plan-creates-reminder-for-unmapped-org-item ()
  (org-rem--with-temp-root-and-db (root db db-path)
    (let ((file (expand-file-name "tasks.org" root)))
      (with-temp-file file
        (insert "* TODO New task\nBody\n"))
      (let* ((json-text
              "{\"schema_version\":1,\"generated_at\":\"2026-02-12T21:40:00Z\",\"list\":{\"id\":\"LIST1\",\"title\":\"Personal\"},\"items\":[]}")
             (plan (org-rem-build-plan root db json-text)))
        (should (= (length (plist-get plan :create-reminders)) 1))
        (should-not (plist-get plan :create-org))
        (should-not (plist-get plan :update-org))
        (should-not (plist-get plan :update-reminders))))))

(ert-deftest org-rem-build-plan-org-wins-when-both-changed ()
  (org-rem--with-temp-root-and-db (root db db-path)
    (let ((file (expand-file-name "tasks.org" root)))
      (with-temp-file file
        (insert "* TODO Changed task\nBody\n"))
      (let* ((items (org-rem-read-org-snapshot root))
             (item (car items))
             (org-id (alist-get 'org_id item)))
        (org-rem-db-upsert-mapping
         db
         `((org_id . ,org-id)
           (org_file . ,file)
           (org_locator . "Changed task")
           (org_hash . "stale-hash")
           (reminder_external_id . "RID-1")
           (reminder_last_modified . "old-time")
           (last_synced_at . "2026-02-12T21:40:00Z")))
        (let* ((json-text
                "{\"schema_version\":1,\"generated_at\":\"2026-02-12T21:40:00Z\",\"list\":{\"id\":\"LIST1\",\"title\":\"Personal\"},\"items\":[{\"external_id\":\"RID-1\",\"local_id\":\"L1\",\"title\":\"Changed task\",\"notes\":\"Body\",\"completed\":false,\"completion_date\":null,\"start\":null,\"due\":null,\"url\":null,\"last_modified\":\"new-time\"}]}")
               (plan (org-rem-build-plan root db json-text)))
          (should (equal (plist-get plan :update-reminders)
                         (list (cons org-id "RID-1"))))
          (should-not (plist-get plan :update-org)))))))

(provide 'sync-engine-test)

;;; sync-engine-test.el ends here
