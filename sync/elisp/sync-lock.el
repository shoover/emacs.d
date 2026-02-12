;;; sync-lock.el --- Lock helpers for sync execution -*- lexical-binding: t; -*-

;;; Code:

(defun org-rem--lock-age-seconds (lock-path)
  "Return lock age in seconds for LOCK-PATH."
  (float-time
   (time-subtract (current-time)
                  (file-attribute-modification-time (file-attributes lock-path)))))

(defun org-rem-lock-acquire (lock-path stale-seconds)
  "Acquire lock at LOCK-PATH.

Return non-nil if acquired. If LOCK-PATH exists and is older than
STALE-SECONDS, reclaim it."
  (condition-case nil
      (progn
        (with-temp-buffer
          (insert (format "pid=%s\ncreated_at=%s\n"
                          (emacs-pid)
                          (format-time-string "%FT%TZ" (current-time) t)))
          (write-region (point-min) (point-max) lock-path nil 'silent nil 'excl))
        t)
    (file-already-exists
     (if (> (org-rem--lock-age-seconds lock-path) stale-seconds)
         (progn
           (delete-file lock-path)
           (org-rem-lock-acquire lock-path stale-seconds))
       nil))))

(defun org-rem-lock-release (lock-path)
  "Release lock at LOCK-PATH."
  (when (file-exists-p lock-path)
    (delete-file lock-path)))

(provide 'sync-lock)

;;; sync-lock.el ends here
