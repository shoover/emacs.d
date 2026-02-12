;;; sync-core-test.el --- Tests for sync core -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'sync-core)

(ert-deftest org-rem-sync-version-is-set ()
  (should (stringp org-rem-sync-version))
  (should (not (string-empty-p org-rem-sync-version))))

(provide 'sync-core-test)

;;; sync-core-test.el ends here
