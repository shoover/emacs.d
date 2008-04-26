;; I keep all my emacs-related stuff under ~/emacs. ~/.emacs should be pretty
;; thin. It might contain machine-specific settings, but mainly it should
;; exist to load this file.

(defvar emacs-root (if (or (eq system-type 'cygwin)
                           (eq system-type 'gnu/linux)
                           (eq system-type 'linux))
                       "/home/shawn/"
                     "c:/users/shawn/")
  "My home directory — the root of my personal emacs load-path.")

;; Add all the elisp directories under ~/emacs to my load path.
(require 'cl)
(labels ((add-path (p)
                   (add-to-list 'load-path
                                (concat emacs-root p))))
  (add-path "emacs/lisp") ;; all my personal elisp code
  (add-path "emacs/site-lisp") ;; elisp stuff I find on the 'net
  (add-path "emacs/site-lisp/remember-1.9")
  (add-path "emacs/color/color-theme-6.6.0") ;; my color preferences
  )


;; Printing
;; TODO: figure out the printer based on where we are
(setq printer-name "//FPGACRUNCHER/Printer4")

;; Tab defaults
(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)

;; Custom keybindings
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\M-s"     'isearch-forward-regexp)
(global-set-key "\M-r"     'isearch-backward-regexp)

(global-set-key [f1] 'toggle-selective-display)
(global-set-key [C-f4] 'kill-buffer)

;; Allow "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Find .emacs -- save a few keystrokes
(defun emacs ()
  (interactive)
  (find-file "~/emacs/init.el"))

;; GTD lists
(defun gtd ()
  (interactive)
  (find-file "~/action/action.org"))

;; Buffer switching
(require 'iswitchb)
(iswitchb-default-keybindings)

;; C
(add-hook 'c-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (setq tab-width 2)
             (setq c-basic-offset 2)))

;; CMake
(autoload 'cmake-mode "cmake-mode" "Edit CMake definitions" t)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt$" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake$"         . cmake-mode))

;; dired sorting hooks
(add-hook 'dired-mode-hook
          (lambda () (require 'dired-sort-map)))

;; Erlang
;; TODO: get erlang dir from env?
(add-to-list 'load-path "C:/Program Files/erl5.5.5/lib/tools-2.5.5/emacs")
(setq erlang-root-dir "C:/Program Files/erl5.5.5")
(add-to-list 'exec-path "C:/Program Files/erl5.5.5/bin")
(ignore-errors (require 'erlang-start))

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(require 'org)
(add-hook 'org-mode-hook
          (lambda ()
            (turn-on-auto-fill)
            (define-key org-mode-map "\C-ca" 'org-agenda)
            (define-key org-mode-map "\C-cl" 'org-store-link)

            ; Variables used to save remember notes
            (setq org-directory "~/action")
            (setq org-default-notes-file "~/action/action.org")

            ; One template--insert note at top of org file
            (setq org-remember-templates
                  '((?t "%?\n  %i\n  %a" "~/action/action.org")))
                   ;(?j "* %U %?\n\n  %i\n  %a" "~/.notes")
                   ;(?i "* %^{Title}\n  %i\n  %a" "~/.notes" "New Ideas")))

            ; Make remember insert new notes at top
            (setq org-reverse-note-order t)))

;; remember-mode: store to org file
(require 'remember)
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

;; Handel logs
(require 'handel-log-mode)

;; Make
(setq compile-command "make ")

;; Mercurial
(require 'mercurial)

;; NSIS
(autoload 'nsis-mode "nsis-mode" "Edit Nullsoft installer scripts" t)
(add-to-list 'auto-mode-alist '("\\.nsi.tmpl$"  . nsis-mode))
(add-to-list 'auto-mode-alist '("\\.nsh$"  . nsis-mode))

;; Python
(autoload 'python-mode "python-mode" "Edit Python source" t)
(add-to-list 'auto-mode-alist '("\\.py$"     . python-mode))
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript" . python-mode))

;; Ruby
(autoload 'ruby-mode "ruby-mode" "Edit Ruby source" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.t$"  . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile"  . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$"  . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rxml$"  . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(autoload 'run-ruby "inf-ruby" "Inferior Ruby process" t)
(autoload 'inf-ruby-keys "inf-ruby" "Local key defs for inf-ruby")
(add-hook 'ruby-mode-hook
          (lambda () (inf-ruby-keys)))

;; Subversion
(require 'psvn)
(add-hook 'svn-log-edit-mode-hook
          '(lambda ()
             (turn-on-auto-fill)
             (setq tab-width 2)
             (setq indent-tabs-mode nil)))

;; YAML
(autoload 'yaml-mode "yaml-mode" "Edit YAML files" t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


(defun xia-compile-microdxp ()
  "Compile the Handel/Xerxes libraries with support only for the microDXP"
  (interactive)
  (setq compile-command "make SERIAL=true CAMAC=false EPP=false ARCNET=false PLX=false USB=false BUILD_VBA=true all")
  (compile compile-command))

(defun xia-compile-dxp2x ()
  "Compile the Handel/Xerxes libraries with support only for the 2X"
  (interactive)
  (setq compile-command "make SERIAL=false CAMAC=true EPP=false ARCNET=false PLX=false USB=false BUILD_VBA=true DXP4C=false all")
  (compile compile-command))

(defun xia-compile-xmap ()
  "Compile the Handel/Xerxes libraries with support only for the xMAP"
  (interactive)
  (setq compile-command "make SERIAL=false CAMAC=false EPP=false ARCNET=false PLX=true USB=false all")
  (compile compile-command))

(defun xia-compile-xmap-profile ()
  "Compile the Handel/Xerxes libraries with support only for the xMAP (Profiling enabled)"
  (interactive)
  (setq compile-command "make SERIAL=false CAMAC=false EPP=false ARCNET=false PLX=true USB=false PROFILE=true all")
  (compile compile-command))

(defun xia-compile-all-mem ()
  "Compile the Handel/Xerxes libraries with the custom memory manager"
  (interactive)
  (setq compile-command "make CUSTOM_MEM_MANAGER=true all")
  (compile compile-command))

(defun xia-compile-all-param-debug ()
  "Compile Handel with XIA_PARAM_DEBUG turned on."
  (interactive)
  (setq compile-command "make PARAM_DEBUG=true all")
  (compile compile-command))

(defun xia-compile-all-plx-write-debug ()
  "Compile Handel with XIA_PLX_WRITE_DEBUG turned on."
  (interactive)
  (setq compile-command "make PLX_WRITE_DEBUG=true all")
  (compile compile-command))

(defun toggle-selective-display ()
  "From jao. A poor-man's version of code folding."
  (interactive)
  (set-selective-display (if selective-display nil 1)))


;; Assumed registry settings (HKLM/Software/GNU/Emacs):
;;   Emacs.toolBar: 0
;;   Emacs.full
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ansi-color-for-comint-mode t)
 '(c-doc-comment-style (quote set-from-style))
 '(column-number-mode t)
 '(fill-column 78)
 '(global-hl-line-mode t)
 '(org-agenda-files (quote ("~/action/action.org")))
 '(org-cycle-include-plain-lists t)
 '(org-tags-column 67)
 '(show-paren-mode t)
 '(transient-mark-mode t)
 '(w32shell-cygwin-bin "C:\\cygwin\\bin"))
 (custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(default ((t (:stipple nil :background "grey95" :foreground "SystemWindowText" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 111))))
  '(font-lock-comment-face ((((class color) (min-colors 88) (background light)) (:foreground "Firebrick" :slant italic))))
  '(font-lock-doc-face ((t (:background "grey90" :foreground "goldenrod"))))
  '(font-lock-function-name-face ((((class color) (min-colors 88) (background light)) (:foreground "Blue1" :weight bold))))
  '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:background "ivory" :foreground "darkolivegreen4")))))


;; Pretty black background color theme.
;; Call this after all the other useless color settings so none of its
;; beauty gets overwritten by them.
;;
;; Other decent themes are Charcoal black, Goldenrod (in a brownish sort
;; of way), Calm forest, Blue sea (for a blue background), and Classic.
(require 'color-theme)
;; For load time efficiency, only my theme is loaded. Run
;; color-theme-initialize at any time to see the rest of the themes.
(load-file "~/emacs/color/color-theme-6.6.0/themes/shawn.elc")
(color-theme-shawn)
