;; Generic major mode for viewing log files.
;;
;; Features:
;; - associates with .log files
;; - highlights a few bits of syntax based on the log formats I use at work.
;; - provides M-n and M-p to navigate errors and warnings
;;
;; Setup:
;;   (autoload 'log-mode "log-mode" "View log files" t)
;;   (add-to-list 'auto-mode-alist '("\\.log$" . log-mode))

(require 'generic-x)

(defvar log-mode-map (make-sparse-keymap))
(let ((map log-mode-map))
  (define-key map "\M-n" 'forward-log-incident)
  (define-key map "\M-p" 'backward-log-incident)
  map)

(define-generic-mode 'log-mode
  nil ; comment char list
  nil ; keyword list

  ;; syntax highlighting
  `(
    ;; log4net:
    ;; 2015-01-13 12:24:25,471 DEBUG [THR] NS.Class - Message text
    ("\\([0-9]+-[0-9]+-[0-9]+ [0-9]+:[0-9]+:[0-9]+,[0-9]+\\) \\(\\(ERROR\\|WARN\\)\\|\\(DEBUG\\|INFO\\)\\) +\\(\\[[0-9a-zA-Z_]+\\]\\) \\([.[:alnum:]]+\\) \\(-\\)" ;
     (1 font-lock-doc-face) ; timestamp
     (3 font-lock-warning-face t t) ; ERROR/WARN
     (4 font-lock-constant-face t t) ; DEBUG/INFO
     (5 font-lock-variable-name-face) ; thread ID
     (6 font-lock-type-face nil t) ; class name
     (7 font-lock-comment-delimiter-face)) ; dividing hyphen

    ;; work C logs:
    ;; [ERROR] [321] 03/12/15 09:33:39 src\file.c, line = 799, functionName: Message text
    ("\\(\\[ERROR\\]\\) \\(\\[[0-9]+\\]\\) \\([0-9]+/[0-9]+/[0-9]+ [0-9]+:[0-9]+:[0-9]+\\) \\([[:alnum:]._\\]+, line = [0-9]+,\\) \\([[:alnum:]_]+?\\): .+"
     (1 font-lock-warning-face nil t) ; ERROR
     (2 font-lock-warning-face nil t) ; [status code]
     (3 font-lock-string-face)        ; timestamp
     (4 font-lock-keyword-face)       ; file, line
     (5 font-lock-function-name-face) ; function
     )
    ("\\(\\[WARN\\]\\) \\([0-9]+/[0-9]+/[0-9]+ [0-9]+:[0-9]+:[0-9]+\\) \\([[:alnum:]._\\]+, line = [0-9]+,\\) \\([[:alnum:]_]+?\\): .+"
     (1 font-lock-warning-face nil t) ; WARN
     (2 font-lock-string-face)        ; timestamp
     (3 font-lock-keyword-face)       ; file, line
     (4 font-lock-function-name-face) ; function
     )
    ("\\(\\[\\(\\(DEBUG\\|INFO *\\)\\)\\]\\) \\([[:alnum:]._\\]+, line = [0-9]+,\\) \\([[:alnum:]_]+?\\): .+"
     (1 font-lock-comment-delimiter-face) ; [] delim
     (2 font-lock-warning-face t t) ; WARN
     (3 font-lock-constant-face t t) ; DEBUG/INFO
     (4 font-lock-keyword-face)       ; file, line
     (5 font-lock-function-name-face) ; function
     )

    ;; [ERROR] 10/02/15 17:02:22 xiaGetSpecialRunData (handel_run_control.c:526)                : [316] detChan number is not in the list of valid values
    ("\\(\\[ERROR\\]\\) \\([0-9]+-[0-9]+-[0-9]+ [0-9]+:[0-9]+:[0-9]+,[0-9]+\\) \\([[:alnum:]_]+\\) \\(([[:alnum:]:._]+?)\\) +: \\(\\[ *[0-9]+\\]\\) .+"
     (1 font-lock-warning-face nil t) ; ERROR
     (2 font-lock-string-face) ; timestamp
     (3 font-lock-function-name-face) ; function
     (4 font-lock-keyword-face) ; file, line
     (5 font-lock-warning-face nil t) ; [status code]
     )
    ;; [WARN] 10/02/15 17:02:22 xiaGetSpecialRunData (handel_run_control.c:526)                : detChan number is not in the list of valid values
    ("\\(\\[\\(\\(WARN \\)\\|\\(DEBUG\\|INFO \\)\\)\\]\\) \\([0-9]+-[0-9]+-[0-9]+ [0-9]+:[0-9]+:[0-9]+,[0-9]+\\) \\([[:alnum:]_]+\\) \\(([[:alnum:]:._]+?)\\) +: .+"
     (1 font-lock-comment-delimiter-face) ; [] delim
     (3 font-lock-warning-face t t) ; WARN
     (4 font-lock-constant-face t t) ; DEBUG/INFO
     (5 font-lock-doc-face) ; timestamp
     (6 font-lock-function-name-face) ; function
     (7 font-lock-keyword-face) ; file, line
     )

    ;; Updated work C logs
    ;; [ERROR] 2016-10-02 17:02:22,123 xiaGetSpecialRunData (handel_run_control.c:526)                : [316] detChan number is not in the list of valid values
    ("\\(\\[ERROR\\]\\) \\([0-9]+/[0-9]+/[0-9]+ [0-9]+:[0-9]+:[0-9]+\\) \\([[:alnum:]_]+\\) \\(([[:alnum:]:._]+?)\\) +: \\(\\[ *[0-9]+\\]\\) .+"
     (1 font-lock-warning-face nil t) ; ERROR
     (2 font-lock-string-face) ; timestamp
     (3 font-lock-function-name-face) ; function
     (4 font-lock-keyword-face) ; file, line
     (5 font-lock-warning-face nil t) ; [status code]
     )
    ;; [WARN] 2016-10-02 17:02:22,123 xiaGetSpecialRunData (handel_run_control.c:526)                : detChan number is not in the list of valid values
    ("\\(\\[\\(\\(WARN \\)\\|\\(DEBUG\\|INFO \\)\\)\\]\\) \\([0-9]+/[0-9]+/[0-9]+ [0-9]+:[0-9]+:[0-9]+\\) \\([[:alnum:]_]+\\) \\(([[:alnum:]:._]+?)\\) +: .+"
     (1 font-lock-comment-delimiter-face) ; [] delim
     (3 font-lock-warning-face t t) ; WARN
     (4 font-lock-constant-face t t) ; DEBUG/INFO
     (5 font-lock-doc-face) ; timestamp
     (6 font-lock-function-name-face) ; function
     (7 font-lock-keyword-face) ; file, line
     )
    )

  '("\\.log$") ; file assocation
  '(list log-mode-init)                ; mode setup functions
  "Major mode for viewing log files.")

(defun log-mode-init ()
  (use-local-map log-mode-map)
  (setq imenu-generic-expression
                `((nil ,(concat "^.*" log-incident-regexp ".\\{0,30\\}") 0))))

(defvar log-incident-regexp (regexp-opt '("ERROR" "WARN"))
  "Regexp to search for \"incidents\" when navigating with the
  forward- and backward- functions.")

(defun forward-log-incident ()
  "Move forward to the next error/warning log."
  (interactive)
  (let ((loc (save-excursion
               (end-of-line)
               (let ((case-fold-search nil))
                 (re-search-forward log-incident-regexp nil t)))))
    (when loc
      (goto-char loc)
      (beginning-of-line))))

(defun backward-log-incident ()
  "Move backward to the previous error/warning log."
  (interactive)
  (let ((loc (save-excursion
               (beginning-of-line)
               (let ((case-fold-search nil))
                 (re-search-backward log-incident-regexp nil t)))))
    (when loc
      (goto-char loc)
      (beginning-of-line))))

(provide 'log-mode)
