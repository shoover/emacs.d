;;; sync-lock-test.el --- Tests for lock helpers -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'sync-lock)

(ert-deftest org-rem-lock-acquire-and-release ()
  (let ((lock-path (make-temp-file "org-rem-lock")))
    (unwind-protect
        (progn
          (delete-file lock-path)
          (should (org-rem-lock-acquire lock-path 60))
          (should (file-exists-p lock-path))
          (org-rem-lock-release lock-path)
          (should-not (file-exists-p lock-path)))
      (when (file-exists-p lock-path)
        (delete-file lock-path)))))

(ert-deftest org-rem-lock-blocks-second-acquire ()
  (let ((lock-path (make-temp-file "org-rem-lock")))
    (unwind-protect
        (progn
          (delete-file lock-path)
          (should (org-rem-lock-acquire lock-path 60))
          (should-not (org-rem-lock-acquire lock-path 60)))
      (when (file-exists-p lock-path)
        (delete-file lock-path)))))

(ert-deftest org-rem-lock-reclaims-stale-lock ()
  (let ((lock-path (make-temp-file "org-rem-lock")))
    (unwind-protect
        (progn
          (delete-file lock-path)
          (with-temp-file lock-path (insert "stale"))
          (set-file-times lock-path (time-subtract (current-time) (seconds-to-time 120)))
          (should (org-rem-lock-acquire lock-path 30)))
      (when (file-exists-p lock-path)
        (delete-file lock-path)))))

(provide 'sync-lock-test)

;;; sync-lock-test.el ends here
