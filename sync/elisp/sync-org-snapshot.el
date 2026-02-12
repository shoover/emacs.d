;;; sync-org-snapshot.el --- Org snapshot collection -*- lexical-binding: t; -*-

;;; Code:

(require 'org)
(require 'org-id)
(require 'subr-x)
(require 'sync-model)
(require 'sync-org)
(require 'sync-paths)

(defconst org-rem--scheduled-date-regexp
  "<\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)"
  "Regexp for parsing Org SCHEDULED timestamps.")

(defun org-rem--parse-scheduled (scheduled-raw)
  "Parse SCHEDULED-RAW timestamp into canonical alist."
  (when (and scheduled-raw
             (string-match org-rem--scheduled-date-regexp scheduled-raw))
    (let* ((year (string-to-number (match-string 1 scheduled-raw)))
           (month (string-to-number (match-string 2 scheduled-raw)))
           (day (string-to-number (match-string 3 scheduled-raw)))
           (hour-minute (and (string-match "\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)" scheduled-raw)
                             (list (string-to-number (match-string 1 scheduled-raw))
                                   (string-to-number (match-string 2 scheduled-raw))))))
      `((year . ,year)
        (month . ,month)
        (day . ,day)
        (hour . ,(if hour-minute (car hour-minute) :json-null))
        (minute . ,(if hour-minute (cadr hour-minute) :json-null))
        (time_zone . :json-null)))))

(defun org-rem--entry-notes ()
  "Return subtree text after metadata for current heading."
  (let ((begin (save-excursion
                 (org-back-to-heading t)
                 (org-end-of-meta-data)
                 (point)))
        (end (save-excursion
               (org-end-of-subtree t t)
               (point))))
    (string-trim-right
     (buffer-substring-no-properties begin end))))

(defun org-rem--collect-current-entry (file)
  "Collect current Org entry into an alist for FILE."
  (let* ((todo-state (org-get-todo-state))
         (id (or (org-id-get) (org-id-get-create)))
         (title (nth 4 (org-heading-components)))
         (tags (org-get-tags nil t))
         (scheduled (org-rem--parse-scheduled (org-entry-get (point) "SCHEDULED")))
         (notes (org-rem--entry-notes))
         (url (org-entry-get (point) "URL"))
         (completed (org-rem-keyword-done-p todo-state))
         (path (string-join (org-get-outline-path t t) "/"))
         (item `((org_id . ,id)
                 (org_file . ,file)
                 (org_locator . ,path)
                 (title . ,title)
                 (tags . ,tags)
                 (todo_state . ,todo-state)
                 (scheduled . ,scheduled)
                 (notes . ,notes)
                 (url . ,url)
                 (completed . ,(if completed t nil)))))
    (append item `((org_hash . ,(org-rem-canonical-hash item))))))

(defun org-rem-collect-org-items-from-file (file)
  "Collect sync items from FILE."
  (with-current-buffer (find-file-noselect file)
    (org-with-wide-buffer
     (org-mode)
     (let ((items nil)
           (id-created nil))
       (let ((org-id-track-globally nil))
         (org-map-entries
          (lambda ()
            (when (org-get-todo-state)
              (unless (org-id-get)
                (setq id-created t))
              (push (org-rem--collect-current-entry file) items)))
          nil
          'file))
       (when id-created
         (save-buffer))
       (nreverse items)))))

(defun org-rem-read-org-snapshot (root)
  "Return collected sync items from all Org files under ROOT."
  (let ((items nil))
    (dolist (file (org-rem-org-files-under-root root) (nreverse items))
      (setq items (append (nreverse (org-rem-collect-org-items-from-file file))
                          items)))))

(provide 'sync-org-snapshot)

;;; sync-org-snapshot.el ends here
