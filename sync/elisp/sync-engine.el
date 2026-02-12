;;; sync-engine.el --- Sync engine composition layer -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'sync-db)
(require 'sync-merge)
(require 'sync-model)
(require 'sync-org)
(require 'sync-org-snapshot)
(require 'sync-reminders-json)

(defun org-rem--planner-org-items (items)
  "Project snapshot ITEMS into planner Org view."
  (mapcar
   (lambda (item)
     `((org_id . ,(alist-get 'org_id item))
       (org_hash . ,(alist-get 'org_hash item))))
   items))

(defun org-rem--planner-reminder-items (decoded-reminder-list)
  "Project DECODED-REMINDER-LIST into planner reminder view."
  (mapcar
   (lambda (item)
     `((external_id . ,(alist-get 'external_id item))
       (last_modified . ,(alist-get 'last_modified item))))
   (alist-get 'items decoded-reminder-list)))

(defun org-rem-build-sync-state (org-root db reminder-list-json)
  "Build sync state for ORG-ROOT, DB, and REMINDER-LIST-JSON."
  (let* ((org-items (org-rem-read-org-snapshot org-root))
         (reminder-list (org-rem-decode-reminder-list-json reminder-list-json))
         (planner-org-items (org-rem--planner-org-items org-items))
         (planner-reminders (org-rem--planner-reminder-items reminder-list))
         (mappings (org-rem-db-list-mappings db))
         (plan (org-rem-plan-reconciliation
                planner-org-items
                planner-reminders
                mappings)))
    (list :org-items org-items
          :reminder-list reminder-list
          :mappings mappings
          :plan plan)))

(defun org-rem-build-plan (org-root db reminder-list-json)
  "Build reconciliation plan for ORG-ROOT, DB, and REMINDER-LIST-JSON."
  (plist-get (org-rem-build-sync-state org-root db reminder-list-json) :plan))

(defun org-rem--engine-index-by (items key)
  "Index ITEMS by KEY into a hash table."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (item items table)
      (puthash (alist-get key item) item table))))

(defun org-rem--normalize-json-null (value)
  "Convert internal JSON null marker VALUE to nil."
  (if (eq value :json-null) nil value))

(defun org-rem--scheduled->date-components (scheduled)
  "Convert SCHEDULED alist into reminder date-components JSON shape."
  (when scheduled
    `((year . ,(alist-get 'year scheduled))
      (month . ,(alist-get 'month scheduled))
      (day . ,(alist-get 'day scheduled))
      (hour . ,(org-rem--normalize-json-null (alist-get 'hour scheduled)))
      (minute . ,(org-rem--normalize-json-null (alist-get 'minute scheduled)))
      (time_zone . ,(org-rem--normalize-json-null
                       (alist-get 'time_zone scheduled))))))

(defun org-rem--org-item->apply-fields (org-item)
  "Build `apply` fields payload from ORG-ITEM."
  (let* ((scheduled (org-rem--scheduled->date-components
                     (alist-get 'scheduled org-item)))
         (title (org-rem-render-title-and-tags
                 (alist-get 'title org-item)
                 (alist-get 'tags org-item))))
    `((title . ,title)
      (notes . ,(alist-get 'notes org-item))
      (completed . ,(if (alist-get 'completed org-item) t :json-false))
      (start . ,scheduled)
      (due . ,scheduled)
      (url . ,(alist-get 'url org-item)))))

(defun org-rem--compact-alist (alist)
  "Return ALIST with nil-valued keys removed."
  (cl-remove-if (lambda (pair) (null (cdr pair))) alist))

(defun org-rem-build-reminder-apply-request (sync-state)
  "Build reminder-side apply request from SYNC-STATE."
  (let* ((plan (plist-get sync-state :plan))
         (org-items (plist-get sync-state :org-items))
         (reminder-list (plist-get sync-state :reminder-list))
         (mappings (plist-get sync-state :mappings))
         (org-by-id (org-rem--engine-index-by org-items 'org_id))
         (rem-by-id (org-rem--engine-index-by
                     (alist-get 'items reminder-list)
                     'external_id))
         (mapping-by-org-id (org-rem--engine-index-by mappings 'org_id))
         (mapping-by-rem-id (org-rem--engine-index-by
                             mappings
                             'reminder_external_id))
         (ops nil)
         (contexts nil))
    (dolist (org-id (plist-get plan :create-reminders))
      (let ((org-item (gethash org-id org-by-id)))
        (unless org-item
          (error "Missing org item for create-reminders op: %s" org-id))
        (push `((op . "create")
                (client_ref . ,org-id)
                (fields . ,(org-rem--org-item->apply-fields org-item)))
              ops)
        (push `((kind . create)
                (org_id . ,org-id)
                (org_item . ,org-item))
              contexts)))
    (dolist (pair (plist-get plan :update-reminders))
      (let* ((org-id (car pair))
             (external-id (cdr pair))
             (org-item (gethash org-id org-by-id))
             (rem-item (gethash external-id rem-by-id))
             (mapping (gethash org-id mapping-by-org-id))
             (if-last-modified
              (or (alist-get 'last_modified rem-item)
                  (alist-get 'reminder_last_modified mapping))))
        (unless org-item
          (error "Missing org item for update-reminders op: %s" org-id))
        (push (org-rem--compact-alist
               `((op . "update")
                 (external_id . ,external-id)
                 (if_last_modified . ,if-last-modified)
                 (fields . ,(org-rem--org-item->apply-fields org-item))))
              ops)
        (push `((kind . update)
                (org_id . ,org-id)
                (external_id . ,external-id)
                (org_item . ,org-item))
              contexts)))
    (dolist (external-id (plist-get plan :delete-reminders))
      (let* ((rem-item (gethash external-id rem-by-id))
             (mapping (gethash external-id mapping-by-rem-id))
             (if-last-modified (or (alist-get 'last_modified rem-item)
                                   (alist-get 'reminder_last_modified mapping))))
        (push (org-rem--compact-alist
               `((op . "delete")
                 (external_id . ,external-id)
                 (if_last_modified . ,if-last-modified)))
              ops)
        (push `((kind . delete)
                (org_id . ,(alist-get 'org_id mapping))
                (external_id . ,external-id))
              contexts)))
    (list :ops (nreverse ops)
          :contexts (nreverse contexts))))

(defun org-rem-pending-org-mutations-p (plan)
  "Return non-nil when PLAN requires Org writeback operations."
  (or (plist-get plan :create-org)
      (plist-get plan :update-org)
      (plist-get plan :delete-org)))

(defun org-rem--utc-now-iso8601 ()
  "Return current UTC timestamp in ISO-8601 format."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t))

(defun org-rem--upsert-mapping-from-context (db context result applied-at)
  "Persist mapping in DB for CONTEXT using RESULT and APPLIED-AT."
  (let* ((org-item (alist-get 'org_item context))
         (result-item (alist-get 'item result))
         (external-id (or (alist-get 'external_id result-item)
                          (alist-get 'external_id context)))
         (last-modified (or (alist-get 'last_modified result-item)
                            (alist-get 'reminder_last_modified
                                       (org-rem-db-get-by-org-id
                                        db
                                        (alist-get 'org_id context))))))
    (when (and org-item external-id)
      (org-rem-db-upsert-mapping
       db
       `((org_id . ,(alist-get 'org_id context))
         (org_file . ,(alist-get 'org_file org-item))
         (org_locator . ,(alist-get 'org_locator org-item))
         (org_hash . ,(alist-get 'org_hash org-item))
         (reminder_external_id . ,external-id)
         (reminder_last_modified . ,last-modified)
         (last_synced_at . ,applied-at))))))

(defun org-rem--remove-mapping-from-context (db context)
  "Remove mapping row in DB for CONTEXT."
  (if-let ((org-id (alist-get 'org_id context)))
      (org-rem-db-delete-by-org-id db org-id)
    (when-let ((external-id (alist-get 'external_id context)))
      (org-rem-db-delete-by-reminder-id db external-id))))

(defun org-rem-apply-reminder-results-to-db (db apply-request apply-response)
  "Apply APPLY-RESPONSE results to DB using APPLY-REQUEST contexts."
  (let ((contexts (plist-get apply-request :contexts))
        (results (alist-get 'results apply-response))
        (applied-at (alist-get 'applied_at apply-response))
        (summary (list :ok 0 :not_found 0 :conflict 0 :error 0))
        (committed nil))
    (sqlite-execute db "BEGIN")
    (unwind-protect
        (progn
          (dolist (result results)
            (let* ((status (alist-get 'status result))
                   (op-index (alist-get 'op_index result))
                   (context (and (integerp op-index)
                                 (nth op-index contexts)))
                   (kind (alist-get 'kind context)))
              (pcase status
                ("ok"
                 (setq summary
                       (plist-put summary :ok (1+ (plist-get summary :ok))))
                 (pcase kind
                   ((or 'create 'update)
                    (org-rem--upsert-mapping-from-context
                     db context result applied-at))
                   ('delete
                    (org-rem--remove-mapping-from-context db context))))
                ("not_found"
                 (setq summary
                       (plist-put summary
                                  :not_found
                                  (1+ (plist-get summary :not_found))))
                 (pcase kind
                   ((or 'update 'delete)
                    (org-rem--remove-mapping-from-context db context))))
                ("conflict"
                 (setq summary
                       (plist-put summary
                                  :conflict
                                  (1+ (plist-get summary :conflict)))))
                (_
                 (setq summary
                       (plist-put summary
                                  :error
                                  (1+ (plist-get summary :error))))))))
          (sqlite-execute db "COMMIT")
          (setq committed t))
      (unless committed
        (ignore-errors (sqlite-execute db "ROLLBACK"))))
    summary))

(defun org-rem--apply-org-create-op (inbox-file inbox-heading rem-by-id rem-id)
  "Apply create-org op for REM-ID using REM-BY-ID."
  (let ((reminder-item (gethash rem-id rem-by-id)))
    (unless reminder-item
      (error "Missing reminder item for create-org op: %s" rem-id))
    (unless (and inbox-file inbox-heading)
      (error "create-org operations require :inbox-file and :inbox-heading"))
    (cons (org-rem-create-entry-from-reminder
           inbox-file
           inbox-heading
           reminder-item)
          rem-id)))

(defun org-rem--apply-org-update-op (org-root rem-by-id pair)
  "Apply update-org op PAIR under ORG-ROOT."
  (let* ((org-id (car pair))
         (rem-id (cdr pair))
         (reminder-item (gethash rem-id rem-by-id)))
    (unless reminder-item
      (error "Missing reminder item for update-org op: %s" rem-id))
    (when (org-rem-update-entry-by-id org-root org-id reminder-item)
      (cons org-id rem-id))))

(defun org-rem--db-upsert-org-side (db org-item rem-id rem-by-id synced-at)
  "Upsert DB mapping for ORG-ITEM linked to REM-ID."
  (let ((reminder-item (gethash rem-id rem-by-id)))
    (when (and org-item reminder-item)
      (org-rem-db-upsert-mapping
       db
       `((org_id . ,(alist-get 'org_id org-item))
         (org_file . ,(alist-get 'org_file org-item))
         (org_locator . ,(alist-get 'org_locator org-item))
         (org_hash . ,(alist-get 'org_hash org-item))
         (reminder_external_id . ,rem-id)
         (reminder_last_modified . ,(alist-get 'last_modified reminder-item))
         (last_synced_at . ,synced-at))))))

(defun org-rem-apply-org-writeback (org-root db sync-state inbox-file inbox-heading)
  "Apply Org-side mutations for SYNC-STATE and persist DB updates."
  (let* ((plan (plist-get sync-state :plan))
         (rem-by-id (org-rem--engine-index-by
                     (alist-get 'items (plist-get sync-state :reminder-list))
                     'external_id))
         (created-pairs nil)
         (updated-pairs nil)
         (deleted-org-ids nil))
    (dolist (org-id (plist-get plan :delete-org))
      (when (org-rem-delete-entry-by-id org-root org-id)
        (push org-id deleted-org-ids)))
    (dolist (pair (plist-get plan :update-org))
      (when-let ((updated (org-rem--apply-org-update-op org-root rem-by-id pair)))
        (push updated updated-pairs)))
    (dolist (rem-id (plist-get plan :create-org))
      (push (org-rem--apply-org-create-op inbox-file inbox-heading rem-by-id rem-id)
            created-pairs))
    (when (or created-pairs updated-pairs deleted-org-ids)
      (let* ((snapshot (org-rem-read-org-snapshot org-root))
             (org-by-id (org-rem--engine-index-by snapshot 'org_id))
             (synced-at (org-rem--utc-now-iso8601))
             (committed nil))
        (sqlite-execute db "BEGIN")
        (unwind-protect
            (progn
              (dolist (org-id deleted-org-ids)
                (org-rem-db-delete-by-org-id db org-id))
              (dolist (pair updated-pairs)
                (org-rem--db-upsert-org-side
                 db
                 (gethash (car pair) org-by-id)
                 (cdr pair)
                 rem-by-id
                 synced-at))
              (dolist (pair created-pairs)
                (org-rem--db-upsert-org-side
                 db
                 (gethash (car pair) org-by-id)
                 (cdr pair)
                 rem-by-id
                 synced-at))
              (sqlite-execute db "COMMIT")
              (setq committed t))
          (unless committed
            (ignore-errors (sqlite-execute db "ROLLBACK"))))))
    (list :create-org (length created-pairs)
          :update-org (length updated-pairs)
          :delete-org (length deleted-org-ids))))

(provide 'sync-engine)

;;; sync-engine.el ends here
