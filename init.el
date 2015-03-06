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

;; This makes load-theme work on anything in "themes"
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

(setq whitespace-global-modes '(c-mode clojure-mode emacs-lisp-mode ruby-mode)
      whitespace-style '(tabs trailing lines-tail space-before-tab empty
                              space-after-tab))

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

(setq tramp-default-method "plinkx")
(setq tramp-verbose 9)

(setq eshell-prompt-function (lambda nil (concat "\n" (eshell/pwd) "\n$ "))
      eshell-prompt-regexp "^\$ ")

(defun add-to-list-n (list-var &rest elements)
  "Adds each element in ELEMENTS to the value of LIST-VAR as in `add-to-list'."
  (dolist (i elements)
    (add-to-list list-var i)))

;; Project setup
(require 'find-file-in-project)
(add-to-list-n 'ffip-patterns "*.c" "*.cs" "*.h" "*.t")
(add-to-list-n 'ffip-project-file ".hg" ".svn")
(add-to-list-n 'ffip-prune-patterns ".hg" ".svn")

;;; Custom functions and mode settings
(load "text")
(load "modes")

;;; Custom keybindings

; Paste with one hand
; todo: reuse C-y
(global-set-key "\C-r" 'yank)
(global-set-key "\M-r" 'yank-pop)

(global-set-key "\C-s" 'isearch-forward)
(global-set-key "\C-xs" 'isearch-backward)
(define-key isearch-mode-map "\C-xs" 'isearch-repeat-backward)
(define-key isearch-mode-map "\M-s" 'isearch-repeat-backward)
(global-set-key "\M-s" 'isearch-forward-regexp)
(global-set-key "\C-x\M-s" 'isearch-backward-regexp)

; remap transpose so C-t is available to create a buffer like Chrome tabs
(global-set-key "\M-t" 'transpose-chars)
(global-set-key "\M-T" 'transpose-words)

(global-set-key [C-tab] 'next-previous-buffer)
(global-set-key "\M-`" 'other-frame)

(global-set-key "\C-x\C-f" 'my-ido-find-file)
(global-set-key "\C-xf" 'my-ido-find-file)
(global-set-key "\C-xk" 'my-ido-kill-buffer)
(global-set-key "\C-xb" 'my-switch-to-buffer)
(global-set-key [f5] 'revert-buffer)
(global-set-key [f6] 'kill-this-buffer)
(global-set-key "\C-t" 'switch-to-new-untitled-buffer)

(global-set-key [C-up] 'move-text-up)
(global-set-key [C-down] 'move-text-down)

(global-set-key "\C-c/" 'my-indent-region) ; Indent region or whole buffer

;; Jump to elisp source. Thanks, emacsredux.com.
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

; Shift+(left|right|up|down) to get to a window quicker than with C-x o
(windmove-default-keybindings)

(global-set-key "\C-xx" 'w32-explore-here)
(global-set-key "\C-xg" 'browse-url-at-point)

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
 '(completion-ignored-extensions
   (quote
    (".obj" ".pdb" ".svn/" "CVS/" ".o" "~" ".bin" ".bak" ".obj" ".map" ".ico" ".pif" ".lnk" ".a" ".ln" ".blg" ".bbl" ".dll" ".drv" ".vxd" ".386" ".elc" ".lof" ".glo" ".idx" ".lot" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo")))
 '(custom-safe-themes
   (quote
    ("1d245dd8c1422d8395c85b0d78f6380aad6e97a24da2cbf3d1491ad57ed4ea5d" "6a985479364fbdc04e63fa1d96d0d86b9281e94a100e1b60f795ec53096b6063" "716bb0758bc6ceee435d3efe38fdde8c1252fb6bf51004159229eb2d9a2fc4de" "0e3f7fae39f57a1c49850be1614a285d2ae9c827d9e42ec6f4e48b3ec2a690b6" "d823c26445ba9e5a6a6e28a7a58da756566cfbd6a5737d56f3345b8204e346df" "c8f583441df726c20a7b40a47be63c6a4e6a496783cafdd7f21520b66a7308b7" "1218df7ba75a7d9d51199866d9d7bf1861e54122863366cf097c4cae9c2a625c" "47372e349f9fee5ce5350c03358628f36ccfc25e7a4e73d1a0473511d295c2f8" default)))
 '(default-frame-alist (quote ((width . 95) (height . 55))))
 '(fill-column 78)
 '(global-hl-line-mode t)
 '(hg-outgoing-repository "")
 '(indent-tabs-mode nil)
 '(ns-alternate-modifier (quote alt))
 '(org-agenda-files
   (quote
    ("c:/Users/Shawn/Dropbox/action/action.org" "c:/Users/Shawn/Dropbox/action/dev.org" "c:/Users/Shawn/Dropbox/action/mobykids.org" "c:/Users/Shawn/Dropbox/action/notes.org" "c:/Users/Shawn/Dropbox/action/work.org" "c:/Users/Shawn/Dropbox/banjo/archtop.org" "c:/Users/Shawn/Dropbox/banjo/banjo.org" "c:/Users/Shawn/Dropbox/banjo/bass.org" "c:/Users/Shawn/Dropbox/banjo/fiddle.org" "c:/Users/Shawn/Dropbox/banjo/peghead.org" "c:/Users/Shawn/Dropbox/banjo/plan.org" "c:/Users/Shawn/Dropbox/banjo/plan_hand_drum.org" "c:/Users/Shawn/Dropbox/banjo/tools.org")))
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

;(load-theme 'Shawn)
;(load-theme 'fogus)
(load-theme 'ample)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(cd "~")
