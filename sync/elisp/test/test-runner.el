;;; test-runner.el --- Batch test runner for Org/Reminders sync -*- lexical-binding: t; -*-

;;; Code:

(setq org-agenda-files nil)
(setq org-agenda-skip-unavailable-files t)

(require 'org-agenda)
(advice-add
 'org-check-agenda-file
 :override
 (lambda (file)
   (file-exists-p file)))

(require 'sync-core-test)
(require 'sync-model-test)
(require 'sync-org-test)
(require 'sync-paths-test)
(require 'sync-db-test)
(require 'sync-lock-test)
(require 'sync-cli-test)
(require 'sync-merge-test)
(require 'sync-org-snapshot-test)
(require 'sync-reminders-json-test)
(require 'sync-engine-test)

;;; test-runner.el ends here
