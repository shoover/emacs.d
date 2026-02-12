;;; sync-reminders-json.el --- JSON helpers for reminders payloads -*- lexical-binding: t; -*-

;;; Code:

(require 'json)

(defun org-rem-decode-reminder-list-json (json-text)
  "Decode reminder list payload JSON-TEXT into an alist tree."
  (json-parse-string json-text
                     :object-type 'alist
                     :array-type 'list
                     :null-object nil
                     :false-object :json-false))

(defun org-rem-decode-reminder-list-file (path)
  "Decode reminder list payload from PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (org-rem-decode-reminder-list-json (buffer-string))))

(defun org-rem-encode-apply-request-json (ops)
  "Encode OPS into orgrem apply request JSON."
  (json-serialize
   `((schema_version . 1)
     (ops . ,(vconcat (or ops '()))))
   :false-object :json-false
   :null-object nil))

(defun org-rem-decode-apply-response-json (json-text)
  "Decode orgrem apply response JSON-TEXT into an alist tree."
  (json-parse-string json-text
                     :object-type 'alist
                     :array-type 'list
                     :null-object nil
                     :false-object :json-false))

(provide 'sync-reminders-json)

;;; sync-reminders-json.el ends here
