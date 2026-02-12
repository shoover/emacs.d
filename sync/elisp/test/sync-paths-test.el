;;; sync-paths-test.el --- Tests for path helpers -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'sync-paths)

(ert-deftest org-rem-org-files-under-root-excludes-archives ()
  (let ((root (make-temp-file "org-rem-paths" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "nested" root))
          (with-temp-file (expand-file-name "a.org" root))
          (with-temp-file (expand-file-name "nested/b.org" root))
          (with-temp-file (expand-file-name "nested/c.org_archive" root))
          (with-temp-file (expand-file-name "notes.txt" root))
          (let ((files (org-rem-org-files-under-root root)))
            (should (equal files
                           (list (expand-file-name "a.org" root)
                                 (expand-file-name "nested/b.org" root))))))
      (delete-directory root t))))

(provide 'sync-paths-test)

;;; sync-paths-test.el ends here
