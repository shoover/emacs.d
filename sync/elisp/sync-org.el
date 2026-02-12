;;; sync-org.el --- Org parsing helpers for sync -*- lexical-binding: t; -*-

;;; Commentary:
;; Org-aware helpers that depend on buffer-local TODO settings.

;;; Code:

(require 'org)

(defun org-rem--refresh-todo-keywords ()
  "Refresh Org TODO keyword caches for current buffer."
  (org-set-regexps-and-options)
  (org-align-all-tags))

(defun org-rem-current-todo-defaults ()
  "Return current buffer TODO defaults as a plist.

Keys:
- `:open-default'
- `:done-default'"
  (org-rem--refresh-todo-keywords)
  (let ((open-default (or (car org-not-done-keywords) "TODO"))
        (done-default (or (car org-done-keywords) "DONE")))
    (list :open-default open-default
          :done-default done-default)))

(defun org-rem-keyword-done-p (keyword)
  "Return non-nil if KEYWORD is a done keyword in current buffer."
  (org-rem--refresh-todo-keywords)
  (and keyword (member keyword org-done-keywords)))

(provide 'sync-org)

;;; sync-org.el ends here
