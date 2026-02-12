;;; sync-engine.el --- Sync engine composition layer -*- lexical-binding: t; -*-

;;; Code:

(require 'sync-db)
(require 'sync-merge)
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

(defun org-rem-build-plan (org-root db reminder-list-json)
  "Build reconciliation plan for ORG-ROOT, DB, and REMINDER-LIST-JSON."
  (let* ((org-items (org-rem-read-org-snapshot org-root))
         (reminder-list (org-rem-decode-reminder-list-json reminder-list-json))
         (planner-org-items (org-rem--planner-org-items org-items))
         (planner-reminders (org-rem--planner-reminder-items reminder-list))
         (mappings (org-rem-db-list-mappings db)))
    (org-rem-plan-reconciliation planner-org-items planner-reminders mappings)))

(provide 'sync-engine)

;;; sync-engine.el ends here
