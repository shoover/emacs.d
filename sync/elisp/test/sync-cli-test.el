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
          (let ((org-agenda-files nil)
                (org-agenda-skip-unavailable-files t))
            (let ((plan (org-rem-sync-run config-path :dry-run t)))
              (should (= (length (plist-get plan :create-reminders)) 1))
              (should-not (plist-get plan :create-org))))))
      (when (file-exists-p db-path)
        (delete-file db-path))
      (when (file-exists-p orgrem-path)
        (delete-file orgrem-path))
      (when (file-exists-p config-path)
        (delete-file config-path))
      (delete-directory root t)))

(ert-deftest org-rem-sync-run-non-dry-run-applies-and-updates-db ()
  (let* ((root (make-temp-file "org-rem-cli-root" t))
         (db-path (make-temp-file "org-rem-cli-db" nil ".sqlite"))
         (orgrem-path (make-temp-file "orgrem-fake"))
         (config-path (make-temp-file "org-rem-config" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "tasks.org" root)
            (insert "* TODO Live task :home:\n"
                    ":PROPERTIES:\n"
                    ":ID: ORG-1\n"
                    ":END:\n"
                    "Body\n"))
          (with-temp-file orgrem-path
            (insert "#!/usr/bin/env zsh\n"
                    "set -euo pipefail\n"
                    "if [[ \"$1\" == \"list\" ]]; then\n"
                    "  echo '{\"schema_version\":1,\"generated_at\":\"2026-02-12T21:40:00Z\",\"list\":{\"id\":\"LIST1\",\"title\":\"Personal\"},\"items\":[]}'\n"
                    "elif [[ \"$1\" == \"apply\" ]]; then\n"
                    "  echo '{\"schema_version\":1,\"applied_at\":\"2026-02-12T22:00:00Z\",\"results\":[{\"op_index\":0,\"status\":\"ok\",\"client_ref\":\"ORG-1\",\"item\":{\"external_id\":\"RID-1\",\"local_id\":\"L1\",\"last_modified\":\"2026-02-12T22:00:00Z\"}}]}'\n"
                    "else\n"
                    "  exit 2\n"
                    "fi\n"))
          (set-file-modes orgrem-path #o755)
          (with-temp-file config-path
            (prin1 `(:org-root ,root
                     :db-path ,db-path
                     :reminders-list-id "LIST1"
                     :orgrem-bin ,orgrem-path)
                   (current-buffer)))
          (let ((org-agenda-files nil)
                (org-agenda-skip-unavailable-files t))
            (let ((plan (org-rem-sync-run config-path)))
              (should (= (length (plist-get plan :create-reminders)) 1))))
          (let ((db (org-rem-db-open db-path)))
            (unwind-protect
                (let ((mapping (org-rem-db-get-by-org-id db "ORG-1")))
                  (should mapping)
                  (should (equal (alist-get 'reminder_external_id mapping) "RID-1"))
                  (should (equal (alist-get 'reminder_last_modified mapping)
                                 "2026-02-12T22:00:00Z")))
              (org-rem-db-close db))))
      (when (file-exists-p db-path)
        (delete-file db-path))
      (when (file-exists-p orgrem-path)
        (delete-file orgrem-path))
      (when (file-exists-p config-path)
        (delete-file config-path))
      (delete-directory root t))))

(ert-deftest org-rem-sync-run-non-dry-run-applies-org-writeback-create ()
  (let* ((root (make-temp-file "org-rem-cli-root" t))
         (db-path (make-temp-file "org-rem-cli-db" nil ".sqlite"))
         (orgrem-path (make-temp-file "orgrem-fake"))
         (config-path (make-temp-file "org-rem-config" nil ".el"))
         (apply-marker (make-temp-file "org-rem-apply-marker")))
    (delete-file apply-marker)
    (unwind-protect
        (progn
          (with-temp-file orgrem-path
            (insert "#!/usr/bin/env zsh\n"
                    "set -euo pipefail\n"
                    "if [[ \"$1\" == \"list\" ]]; then\n"
                    "  echo '{\"schema_version\":1,\"generated_at\":\"2026-02-12T21:40:00Z\",\"list\":{\"id\":\"LIST1\",\"title\":\"Personal\"},\"items\":[{\"external_id\":\"RID-NEW\",\"local_id\":\"L1\",\"title\":\"Phone task #home\",\"notes\":\"From reminders\",\"completed\":false,\"completion_date\":null,\"start\":null,\"due\":null,\"url\":null,\"last_modified\":\"2026-02-12T21:39:00Z\"}]}'\n"
                    "elif [[ \"$1\" == \"apply\" ]]; then\n"
                    "  touch " apply-marker "\n"
                    "  echo '{\"schema_version\":1,\"applied_at\":\"2026-02-12T22:00:00Z\",\"results\":[]}'\n"
                    "else\n"
                    "  exit 2\n"
                    "fi\n"))
          (set-file-modes orgrem-path #o755)
          (with-temp-file config-path
            (prin1 `(:org-root ,root
                     :inbox-file ,(expand-file-name "inbox.org" root)
                     :inbox-heading "* Reminders"
                     :db-path ,db-path
                     :reminders-list-id "LIST1"
                     :orgrem-bin ,orgrem-path)
                   (current-buffer)))
          (let ((org-agenda-files nil)
                (org-agenda-skip-unavailable-files t))
            (let ((plan (org-rem-sync-run config-path)))
              (should (= (length (plist-get plan :create-org)) 1))
              (should-not (file-exists-p apply-marker)))
            (let* ((items (org-rem-read-org-snapshot root))
                   (created (car items)))
              (should (= (length items) 1))
              (should (equal (alist-get 'title created) "Phone task"))
              (should (equal (alist-get 'tags created) '("home"))))
            (let ((db (org-rem-db-open db-path)))
              (unwind-protect
                  (let* ((items (org-rem-read-org-snapshot root))
                         (org-id (alist-get 'org_id (car items)))
                         (mapping (org-rem-db-get-by-org-id db org-id)))
                    (should mapping)
                    (should (equal (alist-get 'reminder_external_id mapping) "RID-NEW")))
                (org-rem-db-close db)))))
      (when (file-exists-p db-path)
        (delete-file db-path))
      (when (file-exists-p orgrem-path)
        (delete-file orgrem-path))
      (when (file-exists-p config-path)
        (delete-file config-path))
      (delete-directory root t))))

(provide 'sync-cli-test)

;;; sync-cli-test.el ends here
