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
    ;; [DEBUG] src\file.c, line = 5651, functionName: Message text
    ("\\(\\[ERROR\\]\\) \\(\\[[0-9]+\\]\\) \\([0-9]+/[0-9]+/[0-9]+ [0-9]+:[0-9]+:[0-9]+\\) \\(.*\\):"
     (1 font-lock-warning-face nil t) ; ERROR
     (2 font-lock-warning-face nil t) ; [status code]
     (3 font-lock-doc-face) ; timestamp
     (4 font-lock-string-face) ; file, line, function
     )
    ("\\(\\[\\(\\(WARN\\)\\|\\(DEBUG\\|INFO\\)\\)\\]\\) \\(.*\\):"
     (1 font-lock-comment-delimiter-face) ; [] delim
     (3 font-lock-warning-face t t) ; WARN
     (4 font-lock-constant-face t t) ; DEBUG/INFO
     (5 font-lock-string-face) ; file, line, function
     )
    )

  '("\\.log$") ; file assocation
  '(list       ; mode setup functions
    (lambda ()
      (use-local-map log-mode-map)))
  "Major mode for viewing log files.")
 
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
