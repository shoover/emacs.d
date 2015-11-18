;; emacs -l .elisp/settings/00.bootstrap.el --batch -f af-bootstrap-packages
;; Use the above for moving into a new machine

(add-to-list 'load-path (expand-file-name "~/.elisp"))

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


(package-initialize)

(setq package-archives '(("durin42" . "http://durin42.com/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("elpa" . "http://elpa.gnu.org/packages/")))

(defun af-bootstrap-packages ()
  (interactive)
  (unless package-archive-contents (package-refresh-contents))
  (dolist (pack '(diff-mode-
                  doctest-mode
                  http-twiddle
                  ipython
                  nose

                  ;; disabled until I don't need a modified version
                  ;; textmate

                  iedit

                  ;; from elpa
                  js2-mode
                  magit
                  markdown-mode
                  paredit
                  smex
                  yaml-mode
                  company
                  wgrep
                  ;; for go-eldoc
                  popup
                  auto-complete
                  go-autocomplete
                  go-eldoc
                  ))
    (unless (or (member pack package-activated-list)
                (functionp pack))
      (message "Installing %s" (symbol-name pack))
      (package-install pack))))
