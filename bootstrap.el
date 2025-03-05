;; emacs --load emacs/bootstrap.el --batch --funcall my-bootstrap-packages
;; Use the above for moving into a new machine.
;; From Augie Fackler https://bitbucket.org/durin42/dotfiles and hacked.

(add-to-list 'load-path (expand-file-name "~/emacs/lisp"))

(defun my-bootstrap-packages ()
  (interactive)
  (package-refresh-contents)
  (dolist (pack '(
                  cider ;;clojure-mode
                  ;;fsharp-mode
                  dockerfile-mode
                  edit-server
                  editorconfig
                  expand-region
                  find-file-in-project
                  go-mode
                  htmlize
                  imenu-anywhere
                  magit
                  markdown-mode
                  multiple-cursors
                  nginx-mode
                  org org-contrib
                  paredit
                  powershell
                  protobuf-mode
                  ruby-mode inf-ruby ; extra extensions
                  transpose-frame
                  yaml-mode
                  yasnippet
                  ))
    (unless (or (member pack package-activated-list)
                (functionp pack))
      (message "Installing %s" (symbol-name pack))
      (condition-case err
          (package-install pack)
        (error
         (message "%s failed: %s" (symbol-name pack) (error-message-string err)))))))
