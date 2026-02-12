;;; sync-org-snapshot-test.el --- Tests for Org snapshot extraction -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'sync-org-snapshot)

(ert-deftest org-rem-collect-org-items-creates-id-and-fields ()
  (let* ((root (make-temp-file "org-rem-snapshot" t))
         (file (expand-file-name "tasks.org" root)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+TODO: next wait | done canceled\n"
                    "* next Pay rent :home:finance:\n"
                    "SCHEDULED: <2026-02-15 Sun>\n"
                    ":PROPERTIES:\n"
                    ":URL: https://example.com\n"
                    ":END:\n"
                    "Body line\n\n"
                    "* done Ship it\n"
                    "Done body\n"))
          (let ((items (org-rem-read-org-snapshot root)))
            (should (= (length items) 2))
            (let* ((first (car items))
                   (second (cadr items)))
              (should (stringp (alist-get 'org_id first)))
              (should (equal (alist-get 'title first) "Pay rent"))
              (should (equal (alist-get 'tags first) '("home" "finance")))
              (should (equal (alist-get 'url first) "https://example.com"))
              (should-not (alist-get 'completed first))
              (should (equal (alist-get 'todo_state first) "next"))
              (should (equal (alist-get 'scheduled first)
                             '((year . 2026)
                               (month . 2)
                               (day . 15)
                               (hour . :json-null)
                               (minute . :json-null)
                               (time_zone . :json-null))))
              (should (stringp (alist-get 'org_hash first)))
              (should (equal (alist-get 'todo_state second) "done"))
              (should (alist-get 'completed second))))
          (with-temp-buffer
            (insert-file-contents file)
            (should (re-search-forward ":ID:" nil t))))
      (delete-directory root t))))

(provide 'sync-org-snapshot-test)

;;; sync-org-snapshot-test.el ends here
