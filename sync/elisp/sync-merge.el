;;; sync-merge.el --- Pure reconciliation planner -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)

(defun org-rem--item-get (item key)
  "Get KEY from ITEM, accepting symbol and string keys."
  (or (alist-get key item)
      (alist-get (symbol-name key) item nil nil #'equal)))

(defun org-rem--index-by (items key)
  "Return hash table indexing ITEMS by KEY."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (item items table)
      (puthash (org-rem--item-get item key) item table))))

(defun org-rem-plan-reconciliation (org-items reminders mappings)
  "Return planned actions for ORG-ITEMS, REMINDERS, and MAPPINGS.

Actions are returned as a plist with keys:
- `:delete-org'
- `:delete-reminders'
- `:update-org'
- `:update-reminders'
- `:create-org'
- `:create-reminders'"
  (let* ((org-by-id (org-rem--index-by org-items 'org_id))
         (rem-by-id (org-rem--index-by reminders 'external_id))
         (mapped-org (make-hash-table :test 'equal))
         (mapped-rem (make-hash-table :test 'equal))
         (delete-org nil)
         (delete-reminders nil)
         (update-org nil)
         (update-reminders nil)
         (create-org nil)
         (create-reminders nil))
    (dolist (mapping mappings)
      (let* ((org-id (org-rem--item-get mapping 'org_id))
             (rem-id (org-rem--item-get mapping 'reminder_external_id))
             (stored-org-hash (org-rem--item-get mapping 'org_hash))
             (stored-rem-mod (org-rem--item-get mapping 'reminder_last_modified))
             (org-item (gethash org-id org-by-id))
             (rem-item (gethash rem-id rem-by-id)))
        (puthash org-id t mapped-org)
        (puthash rem-id t mapped-rem)
        (cond
         ((and (null org-item) rem-item)
          (push rem-id delete-reminders))
         ((and org-item (null rem-item))
          (push org-id delete-org))
         ((and org-item rem-item)
          (let* ((org-changed (not (equal (org-rem--item-get org-item 'org_hash)
                                          stored-org-hash)))
                 (rem-changed (not (equal (org-rem--item-get rem-item 'last_modified)
                                          stored-rem-mod))))
            (cond
             ((and org-changed rem-changed)
              (push (cons org-id rem-id) update-reminders))
             (org-changed
              (push (cons org-id rem-id) update-reminders))
             (rem-changed
              (push (cons org-id rem-id) update-org))))))))
    (dolist (org-item org-items)
      (let ((org-id (org-rem--item-get org-item 'org_id)))
        (unless (gethash org-id mapped-org)
          (push org-id create-reminders))))
    (dolist (rem-item reminders)
      (let ((rem-id (org-rem--item-get rem-item 'external_id)))
        (unless (gethash rem-id mapped-rem)
          (push rem-id create-org))))
    (list :delete-org (nreverse delete-org)
          :delete-reminders (nreverse delete-reminders)
          :update-org (nreverse update-org)
          :update-reminders (nreverse update-reminders)
          :create-org (nreverse create-org)
          :create-reminders (nreverse create-reminders))))

(provide 'sync-merge)

;;; sync-merge.el ends here
