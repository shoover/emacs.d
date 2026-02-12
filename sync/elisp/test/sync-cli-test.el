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

(ert-deftest org-rem-sync-run-dry-run-builds-plan ()
  (let* ((root (make-temp-file "org-rem-cli-root" t))
         (db-path (make-temp-file "org-rem-cli-db" nil ".sqlite"))
         (orgrem-path (make-temp-file "orgrem-fake"))
         (config-path (make-temp-file "org-rem-config" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "tasks.org" root)
            (insert "* TODO Dry run task\nBody\n"))
          (with-temp-file orgrem-path
            (insert "#!/usr/bin/env zsh\n"
                    "echo '{\"schema_version\":1,\"generated_at\":\"2026-02-12T21:40:00Z\",\"list\":{\"id\":\"LIST1\",\"title\":\"Personal\"},\"items\":[]}'\n"))
          (set-file-modes orgrem-path #o755)
          (with-temp-file config-path
            (prin1 `(:org-root ,root
                     :db-path ,db-path
                     :reminders-list-id "LIST1"
                     :orgrem-bin ,orgrem-path)
                   (current-buffer)))
          (let ((plan (org-rem-sync-run config-path :dry-run t)))
            (should (= (length (plist-get plan :create-reminders)) 1))
            (should-not (plist-get plan :create-org)))))
      (when (file-exists-p db-path)
        (delete-file db-path))
      (when (file-exists-p orgrem-path)
        (delete-file orgrem-path))
      (when (file-exists-p config-path)
        (delete-file config-path))
      (delete-directory root t)))

(provide 'sync-cli-test)

;;; sync-cli-test.el ends here
