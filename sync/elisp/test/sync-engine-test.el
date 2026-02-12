;;; sync-engine-test.el --- Tests for sync engine assembly -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'seq)
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

(ert-deftest org-rem-build-reminder-apply-request-builds-create-and-update-ops ()
  (org-rem--with-temp-root-and-db (root db db-path)
    (let ((file (expand-file-name "tasks.org" root)))
      (with-temp-file file
        (insert "* TODO New task :beta:alpha:\n"
                ":PROPERTIES:\n"
                ":ID: ORG-NEW\n"
                ":END:\n"
                "Body\n\n"
                "* TODO Existing task\n"
                ":PROPERTIES:\n"
                ":ID: ORG-EXISTING\n"
                ":END:\n"
                "Changed body\n"))
      (org-rem-db-upsert-mapping
       db
       `((org_id . "ORG-EXISTING")
         (org_file . ,file)
         (org_locator . "Existing task")
         (org_hash . "stale-hash")
         (reminder_external_id . "RID-1")
         (reminder_last_modified . "old-time")
         (last_synced_at . "2026-02-12T21:40:00Z")))
      (let* ((json-text
              "{\"schema_version\":1,\"generated_at\":\"2026-02-12T21:40:00Z\",\"list\":{\"id\":\"LIST1\",\"title\":\"Personal\"},\"items\":[{\"external_id\":\"RID-1\",\"local_id\":\"L1\",\"title\":\"Existing task\",\"notes\":\"Body\",\"completed\":false,\"completion_date\":null,\"start\":null,\"due\":null,\"url\":null,\"last_modified\":\"new-time\"}]}")
             (state (org-rem-build-sync-state root db json-text))
             (request (org-rem-build-reminder-apply-request state))
             (ops (plist-get request :ops))
             (contexts (plist-get request :contexts)))
        (should (= (length ops) 2))
        (should (= (length contexts) 2))
        (let ((create-op (nth 0 ops)))
          (should (equal (alist-get 'op create-op) "create"))
          (should (equal (alist-get 'client_ref create-op) "ORG-NEW"))
          (should (equal (alist-get 'title (alist-get 'fields create-op))
                         "New task #alpha #beta")))
        (let ((update-op (nth 1 ops)))
          (should (equal (alist-get 'op update-op) "update"))
          (should (equal (alist-get 'external_id update-op) "RID-1"))
          (should (equal (alist-get 'if_last_modified update-op) "new-time")))))))

(ert-deftest org-rem-apply-reminder-results-to-db-upserts-and-deletes ()
  (org-rem--with-temp-root-and-db (root db db-path)
    (let ((file (expand-file-name "tasks.org" root)))
      (with-temp-file file
        (insert "* TODO Created\n"
                ":PROPERTIES:\n"
                ":ID: ORG-NEW\n"
                ":END:\n\n"
                "* TODO Updated\n"
                ":PROPERTIES:\n"
                ":ID: ORG-UPD\n"
                ":END:\n"))
      (let* ((items (org-rem-read-org-snapshot root))
             (new-item (seq-find (lambda (item)
                                   (equal (alist-get 'org_id item) "ORG-NEW"))
                                 items))
             (updated-item (seq-find (lambda (item)
                                       (equal (alist-get 'org_id item) "ORG-UPD"))
                                     items)))
        (org-rem-db-upsert-mapping
         db
         `((org_id . "ORG-UPD")
           (org_file . ,file)
           (org_locator . "Updated")
           (org_hash . "old-hash")
           (reminder_external_id . "RID-UPD")
           (reminder_last_modified . "old-time")
           (last_synced_at . "2026-02-12T21:40:00Z")))
        (org-rem-db-upsert-mapping
         db
         `((org_id . "ORG-DEL")
           (org_file . ,file)
           (org_locator . "Deleted")
           (org_hash . "old-hash")
           (reminder_external_id . "RID-DEL")
           (reminder_last_modified . "old-time")
           (last_synced_at . "2026-02-12T21:40:00Z")))
        (let ((request
               (list
                :contexts
                (list
                 `((kind . create)
                   (org_id . "ORG-NEW")
                   (org_item . ,new-item))
                 `((kind . update)
                   (org_id . "ORG-UPD")
                   (external_id . "RID-UPD")
                   (org_item . ,updated-item))
                 '((kind . delete)
                   (org_id . "ORG-DEL")
                   (external_id . "RID-DEL")))))
              (response
               '((schema_version . 1)
                 (applied_at . "2026-02-12T22:00:00Z")
                 (results .
                          (((op_index . 0)
                            (status . "ok")
                            (client_ref . "ORG-NEW")
                            (item .
                                  ((external_id . "RID-NEW")
                                   (local_id . "L-NEW")
                                   (last_modified . "mod-new"))))
                           ((op_index . 1)
                            (status . "ok")
                            (item .
                                  ((external_id . "RID-UPD")
                                   (local_id . "L-UPD")
                                   (last_modified . "mod-upd"))))
                           ((op_index . 2)
                            (status . "ok")))))))
          (org-rem-apply-reminder-results-to-db db request response)
          (let ((created (org-rem-db-get-by-org-id db "ORG-NEW"))
                (updated (org-rem-db-get-by-org-id db "ORG-UPD"))
                (deleted (org-rem-db-get-by-org-id db "ORG-DEL")))
            (should created)
            (should (equal (alist-get 'reminder_external_id created) "RID-NEW"))
            (should (equal (alist-get 'reminder_last_modified created) "mod-new"))
            (should updated)
            (should (equal (alist-get 'org_hash updated)
                           (alist-get 'org_hash updated-item)))
            (should (equal (alist-get 'reminder_last_modified updated) "mod-upd"))
            (should-not deleted)))))))

(provide 'sync-engine-test)

;;; sync-engine-test.el ends here
