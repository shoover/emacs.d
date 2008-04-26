(defvar handel-log-mode-hook nil)
(defvar handel-log-mode-map nil "Keymap for Handel log major mode")

(if handel-log-mode-map nil
  (setq handel-log-mode-map (make-keymap)))

(setq auto-mode-alist
      (append
       '(("\\.log\\'" . handel-log-mode))
       auto-mode-alist))

(defconst handel-log-font-lock-keywords-1
  (list
   '("\\(\\[ERROR\\]\\)" . font-lock-warning-face)) "Error highlighting")

;; (defconst handel-log-font-lock-keywords-2
;;   (append handel-log-font-lock-keywords-1
;; 		  (list '("\\(;\\(.*\\|\\n\\)\\)" . font-lock-comment-face)))
;;            "Comment highlighting for Handel log mode")

(defvar handel-log-font-lock-keywords handel-log-font-lock-keywords-1
  "Default highlighting expressions for the Handel log mode")

(defvar handel-log-mode-syntax-table nil
  "Syntax table for handel-log-mode.")

(defun handel-log-create-syntax-table()
  (if handel-log-mode-syntax-table
      ()
    (setq handel-log-mode-syntax-table (make-syntax-table)))
  (set-syntax-table handel-log-mode-syntax-table))

(defun handel-log-mode ()
  "Major mode for viewing Handel log files."
  (interactive)
  (kill-all-local-variables)
  (handel-log-create-syntax-table)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '(handel-log-font-lock-keywords))
  (setq major-mode 'handel-log-mode)
  (setq mode-name "Handel Log")
  (run-hooks 'handel-log-mode-hook))

(provide 'handel-log-mode)

;;; handel-log-mode.el ends here
