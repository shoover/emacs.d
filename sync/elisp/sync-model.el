;;; sync-model.el --- Canonical model helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Helpers for parsing title tags and producing stable hash inputs.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defconst org-rem--title-tag-regexp "\\`#[A-Za-z0-9_-]+\\'"
  "Regexp used for trailing title tag tokens.")

(defun org-rem-parse-title-and-tags (title)
  "Parse TITLE into base title text and trailing hashtag tokens.

Return plist `(:title STRING :tags LIST)'. Only contiguous hashtag
tokens at the end of TITLE are interpreted as tags."
  (let* ((trimmed (string-trim (or title "")))
         (tokens (split-string trimmed "[ \t]+" t))
         (tag-rev nil))
    (while (and tokens (string-match-p org-rem--title-tag-regexp (car (last tokens))))
      (push (substring (car (last tokens)) 1) tag-rev)
      (setq tokens (butlast tokens)))
    (list :title (string-join tokens " ")
          :tags tag-rev)))

(defun org-rem--normalize-tags (tags)
  "Normalize TAGS to lowercase sorted unique strings."
  (sort (delete-dups (mapcar #'downcase (cl-remove-if #'string-empty-p tags)))
        #'string<))

(defun org-rem-render-title-and-tags (title tags)
  "Render TITLE with normalized trailing TAGS."
  (let* ((base (string-trim (or title "")))
         (normalized (org-rem--normalize-tags (or tags '()))))
    (if normalized
        (format "%s %s"
                base
                (string-join (mapcar (lambda (tag) (concat "#" tag)) normalized) " "))
      base)))

(defun org-rem--normalize-notes (notes)
  "Normalize NOTES line endings and trailing newlines."
  (let* ((text (or notes ""))
         (lf (replace-regexp-in-string "\r\n?" "\n" text)))
    (replace-regexp-in-string "\n+\\'" "" lf)))

(defun org-rem--normalize-url (url)
  "Normalize URL string."
  (let ((trimmed (string-trim (or url ""))))
    (if (string-empty-p trimmed) :json-null trimmed)))

(defun org-rem--normalize-scheduled (scheduled)
  "Normalize SCHEDULED alist to canonical key order."
  (if (null scheduled)
      :json-null
    `((year . ,(alist-get 'year scheduled))
      (month . ,(alist-get 'month scheduled))
      (day . ,(alist-get 'day scheduled))
      (hour . ,(or (alist-get 'hour scheduled) :json-null))
      (minute . ,(or (alist-get 'minute scheduled) :json-null))
      (time_zone . ,(or (alist-get 'time_zone scheduled) :json-null)))))

(defun org-rem-canonical-item (item)
  "Return canonical ITEM alist with stable key order for hashing."
  `((title . ,(or (alist-get 'title item) ""))
    (tags . ,(vconcat (org-rem--normalize-tags
                       (copy-sequence (or (alist-get 'tags item) '())))))
    (todo_state . ,(or (alist-get 'todo_state item) ""))
    (scheduled . ,(org-rem--normalize-scheduled (alist-get 'scheduled item)))
    (notes . ,(org-rem--normalize-notes (alist-get 'notes item)))
    (url . ,(org-rem--normalize-url (alist-get 'url item)))
    (completed . ,(if (eq (alist-get 'completed item) t) t :json-false))))

(defun org-rem-canonical-hash (item)
  "Return SHA-256 hex for canonical ITEM."
  (let* ((canonical (org-rem-canonical-item item))
         (json-text (json-serialize canonical
                                    :false-object :json-false
                                    :null-object :json-null)))
    (secure-hash 'sha256 json-text)))

(provide 'sync-model)

;;; sync-model.el ends here
