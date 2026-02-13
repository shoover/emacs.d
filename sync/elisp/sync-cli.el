;;; sync-cli.el --- Batch sync CLI entrypoint -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'sync-db)
(require 'sync-engine)
(require 'sync-reminders-json)

(defun org-rem--sync-trace-enabled-p ()
  "Return non-nil when sync tracing is enabled via environment."
  (equal (getenv "ORGREM_SYNC_TRACE") "1"))

(defun org-rem--sync-trace (format-string &rest args)
  "Emit trace message when sync tracing is enabled."
  (when (org-rem--sync-trace-enabled-p)
    (apply #'message (concat "[org-rem-sync] " format-string) args)))

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

(defun org-rem--run-orgrem-apply (orgrem-bin list-id apply-request-json)
  "Run ORGREM-BIN apply command for LIST-ID using APPLY-REQUEST-JSON."
  (let ((ops-file (make-temp-file "org-rem-ops" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file ops-file
            (insert apply-request-json))
          (with-temp-buffer
            (let ((exit-code (call-process orgrem-bin nil (current-buffer) nil
                                           "apply"
                                           "--list-id"
                                           list-id
                                           "--ops-file"
                                           ops-file)))
              (unless (eq exit-code 0)
                (error "orgrem apply failed with exit code %s: %s"
                       exit-code
                       (buffer-string)))
              (buffer-string))))
      (when (file-exists-p ops-file)
        (delete-file ops-file)))))

(defun org-rem--run-orgrem-ensure-list (orgrem-bin list-title)
  "Run ORGREM-BIN ensure-list command for LIST-TITLE and return stdout."
  (with-temp-buffer
    (let ((exit-code (call-process orgrem-bin nil (current-buffer) nil
                                   "ensure-list"
                                   "--title"
                                   list-title)))
      (unless (eq exit-code 0)
        (error "orgrem ensure-list failed with exit code %s: %s"
               exit-code
               (buffer-string)))
      (buffer-string))))

(defun org-rem--resolve-list-id (config orgrem-bin)
  "Resolve reminders list ID from CONFIG using ORGREM-BIN."
  (or (plist-get config :reminders-list-id)
      (let ((list-name (plist-get config :reminders-list-name)))
        (unless list-name
          (error "Missing config key: :reminders-list-id or :reminders-list-name"))
        (let* ((list-json (org-rem--run-orgrem-ensure-list orgrem-bin list-name))
               (list-record (org-rem-decode-reminder-list-json list-json))
               (list-id (alist-get 'id list-record)))
          (unless (and list-id (not (string-empty-p list-id)))
            (error "orgrem ensure-list returned invalid response: %s" list-json))
          list-id))))

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
         (orgrem-bin (org-rem--config-get config :orgrem-bin))
         (list-id (org-rem--resolve-list-id config orgrem-bin))
         (db (org-rem-db-open db-path)))
    (org-rem--sync-trace "start dry-run=%s config=%s" dry-run config-path)
    (unwind-protect
        (let* ((_ (org-rem-db-init db))
               (_ (org-rem--sync-trace "db initialized: %s" db-path))
               (reminders-json (org-rem--run-orgrem-list orgrem-bin list-id))
               (_ (org-rem--sync-trace "list fetched"))
               (sync-state (org-rem-build-sync-state org-root db reminders-json))
               (plan (plist-get sync-state :plan)))
          (org-rem--sync-trace "plan built create-org=%s update-org=%s delete-org=%s create-rem=%s update-rem=%s delete-rem=%s"
                               (length (plist-get plan :create-org))
                               (length (plist-get plan :update-org))
                               (length (plist-get plan :delete-org))
                               (length (plist-get plan :create-reminders))
                               (length (plist-get plan :update-reminders))
                               (length (plist-get plan :delete-reminders)))
          (if dry-run
              plan
            (progn
              (let* ((request (org-rem-build-reminder-apply-request sync-state))
                     (ops (plist-get request :ops)))
                (when ops
                  (org-rem--sync-trace "apply begin ops=%s" (length ops))
                  (let* ((apply-request-json
                          (org-rem-encode-apply-request-json ops))
                         (apply-response-json
                          (org-rem--run-orgrem-apply
                           orgrem-bin
                           list-id
                           apply-request-json))
                         (apply-response
                          (org-rem-decode-apply-response-json
                           apply-response-json)))
                    (org-rem-apply-reminder-results-to-db
                     db
                     request
                     apply-response)
                    (org-rem--sync-trace "apply complete"))))
              (when (org-rem-pending-org-mutations-p plan)
                (org-rem--sync-trace "org writeback begin")
                (org-rem-apply-org-writeback
                 org-root
                 db
                 sync-state
                 (plist-get config :inbox-file)
                 (plist-get config :inbox-heading))
                (org-rem--sync-trace "org writeback complete"))
              (org-rem--sync-trace "sync complete")
              plan)))
      (org-rem-db-close db)
      (org-rem--sync-trace "db closed"))))

(provide 'sync-cli)

;;; sync-cli.el ends here
