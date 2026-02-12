;;; sync-org-test.el --- Tests for Org helpers -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'sync-org)

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

(provide 'sync-org-test)

;;; sync-org-test.el ends here
