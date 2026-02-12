;;; sync-paths.el --- Filesystem helpers -*- lexical-binding: t; -*-

;;; Code:

(require 'seq)

(defun org-rem-org-files-under-root (root)
  "Return sorted Org files under ROOT excluding archive files."
  (let* ((all (directory-files-recursively root "\\.org\\(_archive\\)?\\'"))
         (filtered (seq-filter
                    (lambda (path)
                      (and (file-regular-p path)
                           (not (string-suffix-p ".org_archive" path))))
                    all)))
    (sort filtered #'string<)))

(provide 'sync-paths)

;;; sync-paths.el ends here
