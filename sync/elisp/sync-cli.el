;;; sync-cli.el --- Batch sync CLI entrypoint -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)

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
  "Placeholder sync entrypoint.

CONFIG-PATH points at a file containing a plist. OPTIONS are keyword
arguments for runtime behavior."
  (list :config (org-rem-load-config config-path)
        :options options))

(provide 'sync-cli)

;;; sync-cli.el ends here
