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

(provide 'sync-reminders-json)

;;; sync-reminders-json.el ends here
