;;; sync-org.el --- Org parsing helpers for sync -*- lexical-binding: t; -*-

;;; Commentary:
;; Org-aware helpers that depend on buffer-local TODO settings.

;;; Code:

(require 'org)
(require 'org-id)
(require 'subr-x)
(require 'sync-model)
(require 'sync-paths)

(defun org-rem--refresh-todo-keywords ()
  "Refresh Org TODO keyword caches for current buffer."
  (org-set-regexps-and-options)
  (org-align-all-tags))

(defun org-rem-current-todo-defaults ()
  "Return current buffer TODO defaults as a plist.

Keys:
- `:open-default'
- `:done-default'"
  (org-rem--refresh-todo-keywords)
  (let ((open-default (or (car org-not-done-keywords) "TODO"))
        (done-default (or (car org-done-keywords) "DONE")))
    (list :open-default open-default
          :done-default done-default)))

(defun org-rem-keyword-done-p (keyword)
  "Return non-nil if KEYWORD is a done keyword in current buffer."
  (org-rem--refresh-todo-keywords)
  (and keyword (member keyword org-done-keywords)))

(defun org-rem--json-null->nil (value)
  "Convert VALUE JSON null marker into nil."
  (if (eq value :json-null) nil value))

(defun org-rem--reminder-completed-p (reminder-item)
  "Return non-nil when REMINDER-ITEM is completed."
  (eq (alist-get 'completed reminder-item) t))

(defun org-rem--format-scheduled-from-reminder-due (due)
  "Convert reminder DUE date components to an Org timestamp string."
  (when due
    (let* ((year (alist-get 'year due))
           (month (alist-get 'month due))
           (day (alist-get 'day due))
           (hour (org-rem--json-null->nil (alist-get 'hour due)))
           (minute (org-rem--json-null->nil (alist-get 'minute due))))
      (when (and year month day)
        (let ((ts (encode-time 0 (or minute 0) (or hour 0) day month year)))
          (format-time-string
           (if hour "<%Y-%m-%d %a %H:%M>" "<%Y-%m-%d %a>")
           ts))))))

(defun org-rem--replace-entry-notes (heading-point notes)
  "Replace subtree notes under HEADING-POINT with NOTES."
  (let ((begin (save-excursion
                 (goto-char heading-point)
                 (org-end-of-meta-data)
                 (point)))
        (end (save-excursion
               (goto-char heading-point)
               (org-end-of-subtree t t)
               (point))))
    (delete-region begin end)
    (goto-char begin)
    (when (and notes (not (string-empty-p notes)))
      (insert notes)
      (unless (string-suffix-p "\n" notes)
        (insert "\n")))))

(defun org-rem--set-current-entry-from-reminder (reminder-item &optional force-open heading-point)
  "Apply REMINDER-ITEM fields to current heading.

When FORCE-OPEN is non-nil, set TODO state to file default open state."
  (let* ((parsed (org-rem-parse-title-and-tags (alist-get 'title reminder-item)))
         (title (plist-get parsed :title))
         (tags (plist-get parsed :tags))
         (scheduled (org-rem--format-scheduled-from-reminder-due
                     (alist-get 'due reminder-item)))
         (url (alist-get 'url reminder-item))
         (notes (alist-get 'notes reminder-item))
         (defaults (org-rem-current-todo-defaults))
         (open-default (plist-get defaults :open-default))
         (done-default (plist-get defaults :done-default))
         (current-state (org-get-todo-state))
         (current-done (org-rem-keyword-done-p current-state))
         (target-done (org-rem--reminder-completed-p reminder-item))
         (heading-point (or heading-point
                            (save-excursion
                              (org-back-to-heading t)
                              (point))))
         (target-state
          (cond
           (force-open open-default)
           ((eq current-done target-done) current-state)
           (target-done done-default)
           (t open-default))))
    (goto-char heading-point)
    (unless (string-empty-p (or target-state ""))
      (org-todo target-state))
    (goto-char heading-point)
    (org-edit-headline title)
    (goto-char heading-point)
    (org-set-tags tags)
    (goto-char heading-point)
    (if scheduled
        (org-entry-put (point) "SCHEDULED" scheduled)
      (org-entry-delete (point) "SCHEDULED"))
    (goto-char heading-point)
    (if (and url (not (string-empty-p url)))
        (org-entry-put (point) "URL" url)
      (org-entry-delete (point) "URL"))
    (org-rem--replace-entry-notes heading-point notes)))

(defun org-rem--find-entry-point-by-id (org-id)
  "Return point for heading with ORG-ID in current buffer, or nil."
  (goto-char (point-min))
  (catch 'found
    (org-map-entries
     (lambda ()
       (when (equal (org-id-get) org-id)
         (throw 'found (point))))
     nil
     'file)
    nil))

(defun org-rem--normalize-heading-title (heading)
  "Normalize configured HEADING string to headline title."
  (string-trim (replace-regexp-in-string "\\`\\*+\\s-*" "" (or heading ""))))

(defun org-rem--find-heading-point (title)
  "Return heading point for TITLE in current buffer, or nil."
  (goto-char (point-min))
  (catch 'found
    (org-map-entries
     (lambda ()
       (when (equal (nth 4 (org-heading-components)) title)
         (throw 'found (point))))
     nil
     'file)
    nil))

(defun org-rem--ensure-heading (title)
  "Ensure heading TITLE exists in current buffer and return its point."
  (or (org-rem--find-heading-point title)
      (progn
        (goto-char (point-max))
        (unless (bolp)
          (insert "\n"))
        (insert "* " title "\n")
        (org-back-to-heading t)
        (point))))

(defun org-rem--ensure-org-file-exists (org-file)
  "Ensure ORG-FILE exists on disk."
  (let ((dir (file-name-directory org-file)))
    (when dir
      (make-directory dir t))
    (unless (file-exists-p org-file)
      (with-temp-buffer
        (write-region (point-min) (point-max) org-file nil 'silent)))))

(defun org-rem-delete-entry-by-id (org-root org-id)
  "Delete subtree with ORG-ID under ORG-ROOT."
  (catch 'done
    (dolist (file (org-rem-org-files-under-root org-root))
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (org-mode)
         (let ((point (org-rem--find-entry-point-by-id org-id)))
           (when point
             (goto-char point)
             (let ((begin (point))
                   (end (save-excursion
                          (org-end-of-subtree t t)
                          (point))))
               (delete-region begin end)
               (save-buffer)
               (throw 'done t)))))))
    nil))

(defun org-rem-update-entry-by-id (org-root org-id reminder-item)
  "Update subtree with ORG-ID under ORG-ROOT from REMINDER-ITEM."
  (catch 'done
    (dolist (file (org-rem-org-files-under-root org-root))
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (org-mode)
         (let ((point (org-rem--find-entry-point-by-id org-id)))
           (when point
             (goto-char point)
             (org-rem--set-current-entry-from-reminder reminder-item nil point)
             (save-buffer)
             (throw 'done t))))))
    nil))

(defun org-rem-create-entry-from-reminder (org-file inbox-heading reminder-item)
  "Create a new Org entry in ORG-FILE under INBOX-HEADING from REMINDER-ITEM.

Return created Org ID."
  (org-rem--ensure-org-file-exists org-file)
  (with-current-buffer (find-file-noselect org-file)
    (org-with-wide-buffer
     (org-mode)
     (let* ((heading-title (org-rem--normalize-heading-title inbox-heading))
            (heading-point (org-rem--ensure-heading heading-title)))
       (goto-char heading-point)
       (org-end-of-subtree t t)
       (unless (bolp)
         (insert "\n"))
       (let* ((entry-point (point))
              (parsed (org-rem-parse-title-and-tags (alist-get 'title reminder-item)))
              (defaults (org-rem-current-todo-defaults))
              (open-default (plist-get defaults :open-default))
              (org-id (org-id-new)))
         (insert "** " open-default " " (plist-get parsed :title) "\n")
         (goto-char entry-point)
         (org-entry-put (point) "ID" org-id)
         (org-rem--set-current-entry-from-reminder reminder-item t entry-point)
         (save-buffer)
         org-id)))))

(provide 'sync-org)

;;; sync-org.el ends here
