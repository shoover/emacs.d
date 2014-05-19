;; I keep all my emacs-related stuff under ~/emacs. ~/.emacs should be pretty
;; thin. It contains machine-specific settings, but mainly it exists to load
;; this file. Something like this:
;;
;; (setq org-directory "~/Dropbox/action")
;; (setq custom-file "~/emacs/init.el")
;; (load custom-file)
;;
;; Run this occasionally: C-u 0 M-x byte-recompile-directory

;;; Paths and such

(defvar nix (or (eq system-type 'cygwin)
                (eq system-type 'gnu/linux)
                (eq system-type 'linux)
                (eq system-type 'darwin)))

(defvar emacs-root "~/emacs/" "emacs load path root")

;; Add a smattering of elisp under ~/emacs to my load path.
(add-to-list 'load-path emacs-root)
(add-to-list 'load-path (concat emacs-root "lisp"))

(setq custom-theme-directory (concat emacs-root "themes"))

;; I install some info files here.
;;   makeinfo foo.texi
;;   install-info foo emacs/info/dir
(require 'info)
(setq Info-directory-list
      (add-to-list 'Info-default-directory-list
                   (expand-file-name "info" emacs-root)))

(when (eq system-type 'windows-nt)
  ;; Put cygwin ahead of system32 for emacs and things it shells out to.
  ;; The gnuwin32 find.exe that comes with emacsw32 has a bug and doesn't
  ;; look for wildcards in the path you specify.
  (setenv "PATH" (concat "c:/bin" path-separator (getenv "PATH"))))

(defvar my-action-org (concat org-directory "/action.org"))
(defvar my-work-org (concat org-directory "/work.org"))
(defvar my-notes-org (concat org-directory "/notes.org"))

;;; Settings

;; Tab defaults
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; Scroll when the cursor nears the edge, move up to a proportion of the screen
(setq scroll-margin 2
      scroll-preserve-screen-position t)
(setq-default scroll-down-aggressively 0.25
              scroll-up-aggressively 0.25)

;; Whitespace mode was much less subtle in 22 and used other variables
(when (>= emacs-major-version 23)
  (setq whitespace-global-modes '(c-mode clojure-mode emacs-lisp-mode ruby-mode)
        whitespace-style '(tabs trailing lines-tail space-before-tab empty
                                space-after-tab)))

(blink-cursor-mode 1)
(setq ring-bell-function (lambda () (message "")))
(setq inhibit-splash-screen t)
(fset 'yes-or-no-p 'y-or-n-p) ; "y or n" instead of "yes or no"

;; Allow bullet lists starting with - to delimit paragraphs for use with
;; fill-paragraph. fill-individual-paragraphs accomplishes what I want, but it
;; requires that you have an active region.
;;
;; A more generic solution will be needed to work with @param lists in C-code.
(setq paragraph-start "\f\\|[ 	]*$\\|\\([ ]+- \\)")
(setq sentence-end-double-space nil)

(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/.emacs.d/auto-save"))))

;; Fancy buffer and everything else switching
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-create-new-buffer 'always
      ido-file-extensions-order '(".org" t)
      ido-max-directory-size 100000 ; avoid "too big" errors on some dirs
      )
(ido-mode 1)

;; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

;; eshell prompt
(setq eshell-prompt-function (lambda nil (concat "\n" (eshell/pwd) "\n$ "))
      eshell-prompt-regexp "^\$ ")

;; Project setup
;; Files to include in find-file-in-project. Requires find-file-in-project and
;; project.el to be package installed. (Get
;; http://github.com/technomancy/find-file-in-project and run `git submodule
;; update`, then package-install-from-buffer.)
(setq ffip-patterns '("*.c" "*.cs" "*.el" "*.h" "*.rb" "*.t"))
;; This is needed for ffip to work, but it's not on all my machines yet.
;;(require 'project nil t)

;;; Custom functions and mode settings
(load "text")
(load "modes")

;;; Custom keybindings

; Paste with one hand
; todo: reuse C-y
(global-set-key "\C-r" 'yank)
(global-set-key "\M-r" 'yank-pop)

(global-set-key "\C-s" 'isearch-forward)
(global-set-key "\M-s" 'isearch-backward)
(define-key isearch-mode-map "\M-s" 'isearch-repeat-backward)
(global-set-key (kbd "C-S-S") 'isearch-forward-regexp)
(global-set-key (kbd "M-S-S") 'isearch-backward-regexp)

; remap transpose so C-t is available to create a buffer like Chrome tabs
(global-set-key "\M-t" 'transpose-chars)
(global-set-key "\M-T" 'transpose-words)

(global-set-key [f1] 'toggle-selective-display)

(global-set-key [C-tab] 'next-previous-buffer)
(global-set-key "\M-`" 'other-frame)

(global-set-key "\C-x\C-f" 'my-ido-find-file)
(global-set-key "\C-xf" 'my-ido-find-file)
(global-set-key "\C-xb" 'my-switch-to-buffer)
(global-set-key [f5] 'revert-buffer)
(global-set-key [f6] 'kill-this-buffer)
(global-set-key "\C-t" 'switch-to-new-untitled-buffer)

(global-set-key [C-up] 'move-text-up)
(global-set-key [C-down] 'move-text-down)

(global-set-key "\C-c/" 'my-indent-region) ; Indent region or whole buffer

; Shift+(left|right|up|down) to get to a window quicker than with C-x o
(windmove-default-keybindings)

(global-set-key "\C-xx" 'w32shell-explorer-here)

(require 'inc)
(global-set-key (kbd "C-c +") 'increment-integer-at-point)
(global-set-key (kbd "C-c -") 'decrement-integer-at-point)

;; OS X-specific setup
(setq mac-command-modifier (quote meta))
(setq mac-option-modifier (quote alt))
(when (featurep 'aquamacs)
  (tabbar-mode -1)
  (define-key osx-key-mode-map `[(control z)] 'iconify-or-deiconify-frame))

;; Snippets
(require 'yasnippet)
(yas/load-directory (concat emacs-root "snippets"))

;;; Server setup

;; Make sure there's a server. Some OS-specific builds start it automatically,
;; but just in case...
(add-hook 'term-setup-hook
          (lambda ()
            (unless (and (boundp 'server-mode) server-mode)
              (require 'server)
              (server-start))))

;; Assumed registry settings (HKLM/Software/GNU/Emacs):
;;   Emacs.toolBar: 0
;;   Emacs.full
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-for-comint-mode t)
 '(aquamacs-additional-fontsets nil t)
 '(aquamacs-customization-version-id 190 t)
 '(c-doc-comment-style (quote set-from-style))
 '(column-number-mode t)
 '(completion-ignored-extensions (quote (".obj" ".pdb" ".svn/" "CVS/" ".o" "~" ".bin" ".bak" ".obj" ".map" ".ico" ".pif" ".lnk" ".a" ".ln" ".blg" ".bbl" ".dll" ".drv" ".vxd" ".386" ".elc" ".lof" ".glo" ".idx" ".lot" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo")))
 '(custom-safe-themes (quote ("c8f583441df726c20a7b40a47be63c6a4e6a496783cafdd7f21520b66a7308b7" "1218df7ba75a7d9d51199866d9d7bf1861e54122863366cf097c4cae9c2a625c" "47372e349f9fee5ce5350c03358628f36ccfc25e7a4e73d1a0473511d295c2f8" default)))
 '(default-frame-alist (quote ((width . 95) (height . 55))))
 '(fill-column 78)
 '(global-hl-line-mode t)
 '(hg-outgoing-repository "")
 '(indent-tabs-mode nil)
 '(ns-alternate-modifier (quote alt))
 '(rst-level-face-base-light 20)
 '(rst-level-face-step-light 7)
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(special-display-regexps (quote (".*SPEEDBAR.*")))
 '(tab-always-indent t)
 '(tab-width 2)
 '(transient-mark-mode t)
 '(user-full-name "Shawn Hoover")
 '(user-mail-address "shawn.hoover@gmail.com")
 '(visual-scroll-margin 0)
 '(w32shell-cygwin-bin "C:\\bin")
 '(x-select-enable-clipboard t))

;; Subtle face for parens in lisp modes
(require 'parenface)

(load-theme 'Shawn)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(cd "~")
