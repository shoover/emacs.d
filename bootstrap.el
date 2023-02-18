;; emacs --load emacs/bootstrap.el --batch --funcall my-bootstrap-packages
;; Use the above for moving into a new machine.
;; From Augie Fackler https://bitbucket.org/durin42/dotfiles and hacked.

(add-to-list 'load-path "~/emacs/lisp")

;; emacs 23 compat hack from Lucas Bergman
(unless (fboundp 'package-refresh-contents)
  ;; If ELPA support isn't built in, we're in Emacs <=23. Normally, one
  ;; would bootstrap ELPA from the source, tromey.com, but that sucks,
  ;; because that version of package.el doesn't support multiple archives
  ;; even in 2012. old/package.el is from http://bit.ly/pkg-el23, which is
  ;; cited at https://github.com/technomancy/package.el as the last emacs23
  ;; version of package.el.
  (unless (load (expand-file-name "~/.elisp/old/package.el"))
    (error "ELPA is not in Emacs, and local package.el failed to load.")))


(require 'package)
(dolist (archive '(("melpa" . "https://melpa.org/packages/")
                   ("marmalade" . "http://marmalade-repo.org/packages/")))
  (add-to-list 'package-archives archive t))
(package-initialize)

(defun my-bootstrap-packages ()
  (interactive)
  (unless package-archive-contents (package-refresh-contents))
  (dolist (pack '(
                  cider ;;clojure-mode
                  ;;fsharp-mode
                  csharp-mode
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
                  nginx-mode
                  org-plus-contrib
                  paredit
                  powershell
                  protobuf-mode
                  ruby-mode inf-ruby ; extra extensions
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
