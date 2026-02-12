;;; sync-cli-test.el --- Tests for CLI helpers -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'sync-cli)

(ert-deftest org-rem-parse-cli-args-defaults ()
  (let ((parsed (org-rem-parse-cli-args nil)))
    (should-not (plist-get parsed :dry-run))
    (should-not (plist-get parsed :verbose))
    (should-not (plist-get parsed :config))))

(ert-deftest org-rem-parse-cli-args-flags ()
  (let ((parsed (org-rem-parse-cli-args '("--dry-run"
                                           "--verbose"
                                           "--config"
                                           "/tmp/config.el"))))
    (should (plist-get parsed :dry-run))
    (should (plist-get parsed :verbose))
    (should (equal (plist-get parsed :config) "/tmp/config.el"))))

(ert-deftest org-rem-parse-cli-args-invalid-option-errors ()
  (should-error (org-rem-parse-cli-args '("--bad-flag"))
                :type 'error))

(provide 'sync-cli-test)

;;; sync-cli-test.el ends here
