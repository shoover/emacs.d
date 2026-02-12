;;; sync-cli.el --- Batch sync CLI entrypoint -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'sync-db)
(require 'sync-engine)

(defun org-rem--config-get (config key)
  "Get KEY from CONFIG plist or error."
  (or (plist-get config key)
      (error "Missing config key: %s" key)))

(defun org-rem--run-orgrem-list (orgrem-bin list-id)
  "Run ORGREM-BIN list command for LIST-ID and return stdout."
  (with-temp-buffer
    (let ((exit-code (call-process orgrem-bin nil (current-buffer) nil
                                   "list"
                                   "--list-id"
                                   list-id)))
      (unless (eq exit-code 0)
        (error "orgrem list failed with exit code %s: %s"
               exit-code
               (buffer-string)))
      (buffer-string))))

(defun org-rem-parse-cli-args (args)
  "Parse command line ARGS into a plist."
  (let ((result (list :dry-run nil :verbose nil :config nil))
        (remaining args))
    (while remaining
      (let ((arg (pop remaining)))
        (pcase arg
          ("--dry-run"
           (setq result (plist-put result :dry-run t)))
          ("--verbose"
           (setq result (plist-put result :verbose t)))
          ("--config"
           (unless remaining
             (error "--config requires a file path"))
           (setq result (plist-put result :config (pop remaining))))
          (_ (error "Unknown option: %s" arg)))))
    result))

(defun org-rem-load-config (path)
  "Load config plist from PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (read (current-buffer))))

(defun org-rem-sync-run (config-path &rest options)
  "Run sync orchestration.

CONFIG-PATH points at a file containing a plist. OPTIONS are keyword
arguments for runtime behavior."
  (let* ((config (org-rem-load-config config-path))
         (dry-run (plist-get options :dry-run))
         (org-root (org-rem--config-get config :org-root))
         (db-path (org-rem--config-get config :db-path))
         (list-id (org-rem--config-get config :reminders-list-id))
         (orgrem-bin (org-rem--config-get config :orgrem-bin))
         (db (org-rem-db-open db-path)))
    (unwind-protect
        (let* ((_ (org-rem-db-init db))
               (reminders-json (org-rem--run-orgrem-list orgrem-bin list-id))
               (plan (org-rem-build-plan org-root db reminders-json)))
          (if dry-run
              plan
            (error "Non-dry-run execution not implemented yet")))
      (org-rem-db-close db))))

(provide 'sync-cli)

;;; sync-cli.el ends here
