;;; sync-reminders-json-test.el --- Tests for reminders JSON decode -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'sync-reminders-json)

(ert-deftest org-rem-decode-reminder-list-json-extracts-items ()
  (let* ((json-text
          "{\"schema_version\":1,\"generated_at\":\"2026-02-12T21:40:00Z\",\"list\":{\"id\":\"LIST1\",\"title\":\"Personal\"},\"items\":[{\"external_id\":\"RID-1\",\"local_id\":\"L1\",\"title\":\"Task\",\"notes\":\"Body\",\"completed\":false,\"completion_date\":null,\"start\":null,\"due\":null,\"url\":null,\"last_modified\":\"2026-02-12T21:35:18Z\"}]}")
         (decoded (org-rem-decode-reminder-list-json json-text)))
    (should (equal (alist-get 'schema_version decoded) 1))
    (should (equal (alist-get 'id (alist-get 'list decoded)) "LIST1"))
    (should (= (length (alist-get 'items decoded)) 1))
    (should (equal (alist-get 'external_id (car (alist-get 'items decoded)))
                   "RID-1"))))

(ert-deftest org-rem-encode-apply-request-json-serializes-ops ()
  (let* ((ops '(((op . "delete")
                 (external_id . "RID-1"))))
         (json-text (org-rem-encode-apply-request-json ops)))
    (should (string-match-p "\"schema_version\":1" json-text))
    (should (string-match-p "\"op\":\"delete\"" json-text))
    (should (string-match-p "\"external_id\":\"RID-1\"" json-text))))

(ert-deftest org-rem-decode-apply-response-json-reads-results ()
  (let* ((json-text
          "{\"schema_version\":1,\"applied_at\":\"2026-02-12T22:00:00Z\",\"results\":[{\"op_index\":0,\"status\":\"ok\"}]}")
         (decoded (org-rem-decode-apply-response-json json-text)))
    (should (equal (alist-get 'schema_version decoded) 1))
    (should (equal (alist-get 'applied_at decoded) "2026-02-12T22:00:00Z"))
    (should (equal (alist-get 'status (car (alist-get 'results decoded)))
                   "ok"))))

(provide 'sync-reminders-json-test)

;;; sync-reminders-json-test.el ends here
