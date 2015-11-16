(defun org-publish-dir-x (dir &optional target project-name)
  "Publishes all the .org files in DIR to the TARGET directory
using the org HTML publisher."
  (interactive
   (let* ((dir (ido-read-directory-name "Source dir: "))
          (target (ido-read-directory-name "HTML dir: "
                                           (concat dir "/html")))
          ;; strip trailing slash and get the last path part
          (project-name (file-name-nondirectory (directory-file-name dir)))
          (project-name (read-string (format "Project name [%s]: " project-name)
                                     nil nil project-name)))
     (list dir target project-name)))

  (unless (file-exists-p dir)
    (error "Org dir %s does not exist" dir))
  
  (let* ((dir-exp (expand-file-name dir))
         (target (or target (concat dir-exp "/html")))
         (project-name (or project-name (file-name-nondirectory dir)))
         (org-publish-project-alist `((,project-name
                                       :base-directory ,dir-exp
                                       :publishing-directory ,target
                                       :publishing-function org-html-publish-to-html
                                       :auto-sitemap nil ; quieter logs
                                       ))))
    (message "Publishing org dir: %s" dir-exp)
    (org-publish-project project-name
                         t ; set to force publishing files the org cache thinks are ok
                         ;;  ; even if they were deleted :-/
                         )))
