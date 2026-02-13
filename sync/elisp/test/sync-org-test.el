;;; sync-org-test.el --- Tests for Org helpers -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'seq)
(require 'sync-org)
(require 'sync-org-snapshot)

(ert-deftest org-rem-todo-defaults-respect-file-local-settings ()
  (with-temp-buffer
    (insert "#+TODO: next wait someday | done canceled\n")
    (org-mode)
    (let ((defaults (org-rem-current-todo-defaults)))
      (should (equal (plist-get defaults :open-default) "next"))
      (should (equal (plist-get defaults :done-default) "done"))
      (should (org-rem-keyword-done-p "done"))
      (should (org-rem-keyword-done-p "canceled"))
      (should (not (org-rem-keyword-done-p "wait"))))))

(ert-deftest org-rem-todo-defaults-fallback-to-todo-done ()
  (with-temp-buffer
    (insert "* TODO Task\n")
    (org-mode)
    (let ((defaults (org-rem-current-todo-defaults)))
      (should (equal (plist-get defaults :open-default) "TODO"))
      (should (equal (plist-get defaults :done-default) "DONE")))))

(ert-deftest org-rem-create-entry-from-reminder-creates-under-heading ()
  (let* ((root (make-temp-file "org-rem-org" t))
         (file (expand-file-name "inbox.org" root))
         (reminder '((external_id . "RID-1")
                     (title . "Pay rent #home #finance")
                     (notes . "Body text")
                     (completed . t)
                     (due . ((year . 2026)
                             (month . 2)
                             (day . 20)
                             (hour . :json-null)
                             (minute . :json-null)
                             (time_zone . :json-null)))
                     (url . "https://example.com"))))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "* Existing\n"))
          (let* ((org-id (org-rem-create-entry-from-reminder
                          file
                          "* Reminders"
                          reminder))
                 (items (org-rem-read-org-snapshot root))
                 (item (seq-find (lambda (entry)
                                   (equal (alist-get 'org_id entry) org-id))
                                 items)))
            (should item)
            (should (equal (alist-get 'title item) "Pay rent"))
            (should (equal (alist-get 'tags item) '("home" "finance")))
            (should (equal (alist-get 'todo_state item) "TODO"))
            (should-not (alist-get 'completed item))
            (should (equal (alist-get 'url item) "https://example.com"))
            (should (equal (alist-get 'notes item) "Body text")))
          (with-temp-buffer
            (insert-file-contents file)
            (should (re-search-forward "^\\* Reminders$" nil t))))
      (delete-directory root t))))

(ert-deftest org-rem-create-entry-from-reminder-creates-missing-file ()
  (let* ((root (make-temp-file "org-rem-org" t))
         (dir (expand-file-name "newdir" root))
         (file (expand-file-name "inbox.org" dir))
         (reminder '((external_id . "RID-2")
                     (title . "New reminder")
                     (notes . "Body")
                     (completed . nil)
                     (due . nil)
                     (url . nil))))
    (unwind-protect
        (progn
          (should-not (file-exists-p file))
          (let ((org-id (org-rem-create-entry-from-reminder
                         file
                         "* Reminders"
                         reminder)))
            (should org-id)
            (should (file-exists-p file))
            (with-temp-buffer
              (insert-file-contents file)
              (should (re-search-forward "^\\* Reminders$" nil t))
              (should (re-search-forward "^\\*\\* TODO New reminder$" nil t)))))
      (delete-directory root t))))

(ert-deftest org-rem-update-entry-by-id-applies-reminder-fields ()
  (let* ((root (make-temp-file "org-rem-org" t))
         (file (expand-file-name "tasks.org" root))
         (reminder '((external_id . "RID-1")
                     (title . "Updated title #alpha")
                     (notes . "New body")
                     (completed . t)
                     (due . nil)
                     (url . nil))))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+TODO: next wait | done canceled\n"
                    "* wait Original :old:\n"
                    ":PROPERTIES:\n"
                    ":ID: ORG-1\n"
                    ":URL: https://old.example\n"
                    ":END:\n"
                    "Old body\n"))
          (should (org-rem-update-entry-by-id root "ORG-1" reminder))
          (let* ((items (org-rem-read-org-snapshot root))
                 (item (seq-find (lambda (entry)
                                   (equal (alist-get 'org_id entry) "ORG-1"))
                                 items)))
            (should item)
            (should (equal (alist-get 'todo_state item) "done"))
            (should (alist-get 'completed item))
            (should (equal (alist-get 'title item) "Updated title"))
            (should (equal (alist-get 'tags item) '("alpha")))
            (should-not (alist-get 'scheduled item))
            (should (equal (alist-get 'notes item) "New body"))
            (should-not (alist-get 'url item))))
      (delete-directory root t))))

(ert-deftest org-rem-delete-entry-by-id-removes-subtree ()
  (let* ((root (make-temp-file "org-rem-org" t))
         (file (expand-file-name "tasks.org" root)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "* TODO Keep\n"
                    ":PROPERTIES:\n"
                    ":ID: ORG-KEEP\n"
                    ":END:\n\n"
                    "* TODO Drop\n"
                    ":PROPERTIES:\n"
                    ":ID: ORG-DROP\n"
                    ":END:\n"))
          (should (org-rem-delete-entry-by-id root "ORG-DROP"))
          (let ((items (org-rem-read-org-snapshot root)))
            (should (= (length items) 1))
            (should (equal (alist-get 'org_id (car items)) "ORG-KEEP"))))
      (delete-directory root t))))

(provide 'sync-org-test)

;;; sync-org-test.el ends here
