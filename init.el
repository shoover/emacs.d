;; I keep all my emacs-related stuff under ~/emacs. ~/.emacs should be pretty
;; thin. It contains machine-specific settings, but mainly it exists to load
;; this file. Something like this:
;;
;; (setq org-directory "~/Dropbox/action")
;; (setq custom-file "~/emacs/init.el")
;; (load custom-file)
;;
;; Run this occasionally: C-u 0 M-x byte-recompile-directory
;;
;; LISP in a require line comment means the file is assumed to be in
;; emacs/lisp. Otherwise it should be in stalled in the package system by
;; running bootstrap.el.

;;; Paths
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

(when (eq system-type 'windows-nt)
  ;; Put cygwin ahead of system32 for emacs and things it shells out to.
  ;; This is needed by find-file-in-project, at least.
  (setenv "PATH" (concat "c:/bin" path-separator (getenv "PATH"))))

(defvar my-action-org (concat org-directory "/action.org"))
(defvar my-work-org (concat org-directory "/work.org"))
(defvar my-notes-org (concat org-directory "/notes.org"))

;;; Settings

;; Tab defaults: 4 spaces
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;; Scroll when the cursor nears the edge, move up to a proportion of the screen
(setq scroll-margin 2
      scroll-preserve-screen-position t)
(setq-default scroll-down-aggressively 0.25
              scroll-up-aggressively 0.25)

(setq whitespace-global-modes '(c-mode clojure-mode emacs-lisp-mode ruby-mode)
      whitespace-style '(tabs trailing lines-tail space-before-tab empty
                              space-after-tab))

(blink-cursor-mode 1)
(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)
(fset 'yes-or-no-p 'y-or-n-p) ; "y or n" instead of "yes or no"

(savehist-mode t) ; save minibuffer history

;; Allow bullet lists starting with - to delimit paragraphs for use with
;; fill-paragraph. fill-individual-paragraphs accomplishes what I want, but it
;; requires that you have an active region.
;;
;; A more generic solution will be needed to work with @param lists in C-code.
(setq paragraph-start "\f\\|[ 	]*$\\|\\([ ]+- \\)")
(setq sentence-end-double-space nil)

;; Insert matching parens and braces, please
(electric-pair-mode 1)

;; Typing overwrites the region
(delete-selection-mode 1)

(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/.emacs.d/auto-save"))))
(setq delete-by-moving-to-trash t)

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

;; patterns defaults to "", which includes everything not in ffip-prune-patterns,
;; which is probably easier than stewarding the include patterns
;;(add-to-list-n 'ffip-patterns "*.c" "*.cs" "*.h" "*.t")
(setq ffip-project-file (if (listp ffip-project-file)
                            ffip-project-file
                          (list ffip-project-file)))
(add-to-list-n 'ffip-project-file ".hg" ".svn" "build.bat")
(add-to-list-n 'ffip-prune-patterns
               ".lock*"
               "*/\\.vs/*"
               "build-*"
               "html"                   ; doxygen builds
               "*/obj/*"
               "*/packages/*"
               "*/release/*"                ; project release builds
               "scons-out"
               "waf-*")

;;; Custom functions and mode settings
(load "text")
(load "modes")

;;; Custom keybindings

;; Paste with one hand
;; todo: reuse C-y
(global-set-key "\C-r" 'yank)
(global-set-key "\M-r" 'yank-pop)
(define-key paredit-mode-map "\M-r" 'paredit-yank-pop)

(global-set-key "\C-s" 'isearch-forward)
(global-set-key "\M-s" 'isearch-backward)
(define-key isearch-mode-map "\M-s" 'isearch-repeat-backward)
(global-set-key "\C-xs" 'isearch-forward-regexp)
(global-set-key "\C-x\M-s" 'isearch-backward-regexp)

(define-key ido-common-completion-map "\M-s" 'ido-prev-match)
(define-key ido-file-dir-completion-map "\M-s" 'ido-prev-match)

;; I want to remap C-i in text-mode but leave tab working as normal. This is
;; the only way I've found that works without messing up local keymaps like
;; the minibuffer's. http://stackoverflow.com/a/11319885/223029
;; Works great except you have to remember to use H-i everywhere you want to override.
;; Character displays can't distinguish tab from C-i, so forget it there.
(when (display-graphic-p)
  (define-key input-decode-map "\C-i" (kbd "H-i")))

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

; Shift+(left|right|up|down) to hop around windows quicker than with C-x o
(windmove-default-keybindings)

(global-set-key "\C-xx" 'w32-explore-here)
(global-set-key "\C-cg" 'browse-url-at-point)

(global-set-key (kbd "C-S-b") 'save-compile-project)

;; OS X-specific setup
(setq mac-command-modifier (quote meta))
(setq mac-option-modifier (quote alt))
(when (featurep 'aquamacs)
  (tabbar-mode -1)
  (define-key osx-key-mode-map `[(control z)] 'iconify-or-deiconify-frame))

;; Snippets
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs (concat emacs-root "snippets"))
(yas-global-mode 1)

;; scpaste; this is available on ELPA, but I have patched it to work with pscp/plink
(require 'scpaste) ; LISP

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
 '(aquamacs-customization-version-id 307 t)
 '(aquamacs-tool-bar-user-customization nil t)
 '(c-doc-comment-style (quote set-from-style))
 '(column-number-mode t)
 '(completion-ignored-extensions
   (quote
    (".obj" ".pdb" ".svn/" "CVS/" ".o" "~" ".bin" ".bak" ".obj" ".map" ".ico" ".pif" ".lnk" ".a" ".ln" ".blg" ".bbl" ".dll" ".drv" ".vxd" ".386" ".elc" ".lof" ".glo" ".idx" ".lot" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo")))
 '(custom-safe-themes
   (quote
    ("c3d2ba95aa0b113cae54270a18b971bda31262470cc4ae516687cf08360d5e47" "2ce2b0917177236c5af530e08354de4d98004fae5900fd06acc8512cffdd5368" "12d8cb25243aae3137aeebab95119638450eb0de0aed0bca7b55882564d142ef" "edbe2d6a820433a4b4179fecd92dcae318c82d0a60b470e55ab1d48bd56bb8c9" "4f6cb6a7675c0c9931235ad2d60ba820ddf83d9b2754aad04c2ef7c3d0776942" "1d245dd8c1422d8395c85b0d78f6380aad6e97a24da2cbf3d1491ad57ed4ea5d" "6a985479364fbdc04e63fa1d96d0d86b9281e94a100e1b60f795ec53096b6063" "716bb0758bc6ceee435d3efe38fdde8c1252fb6bf51004159229eb2d9a2fc4de" "0e3f7fae39f57a1c49850be1614a285d2ae9c827d9e42ec6f4e48b3ec2a690b6" "d823c26445ba9e5a6a6e28a7a58da756566cfbd6a5737d56f3345b8204e346df" "c8f583441df726c20a7b40a47be63c6a4e6a496783cafdd7f21520b66a7308b7" "1218df7ba75a7d9d51199866d9d7bf1861e54122863366cf097c4cae9c2a625c" "47372e349f9fee5ce5350c03358628f36ccfc25e7a4e73d1a0473511d295c2f8" default)))
 '(default-frame-alist (quote ((width . 95) (height . 55))))
 '(fill-column 78)
 '(global-hl-line-mode t)
 '(hg-outgoing-repository "")
 '(indent-tabs-mode nil)
 '(ns-alternate-modifier (quote alt))
 '(ns-tool-bar-display-mode (quote both) t)
 '(ns-tool-bar-size-mode nil t)
 '(org-export-backends (quote (ascii html latex md odt)))
 '(rst-level-face-base-light 20)
 '(rst-level-face-step-light 7)
 '(safe-local-variable-values
   (quote
    ((eval setq default-directory
           (locate-dominating-file buffer-file-name ".dir-locals.el")))))
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(special-display-regexps (quote (".*SPEEDBAR.*")))
 '(tab-always-indent t)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(user-full-name "Shawn Hoover")
 '(user-mail-address "shawn.hoover@gmail.com")
 '(visual-line-mode nil t)
 '(visual-scroll-margin 0)
 '(w32shell-cygwin-bin "C:\\bin")
 '(x-select-enable-clipboard t))

;; Subtle face for parens in lisp modes
(require 'parenface) ; LISP

(when (display-graphic-p)
  ;;(load-theme 'Shawn)
  ;;(load-theme 'fogus)
  (load-theme 'ample)

  (global-hl-line-mode t)
  (show-paren-mode t))

(cd "~")

;; Check custom-file compatibility
(when (and (boundp 'aquamacs-version-id)
	   (< (floor (/ aquamacs-version-id 10))
	   (floor (/ aquamacs-customization-version-id 10))))
  (defadvice frame-notice-user-settings (before show-version-warning activate)
    (defvar aquamacs-backup-custom-file nil "Backup of `custom-file', if any.")
    (setq aquamacs-backup-custom-file "~/emacs/customizations.1.9.el")
    (let ((msg "Aquamacs options were saved by a more recent program version.
Errors may occur.  Save Options to overwrite the customization file. The original, older customization file was backed up to ~/emacs/customizations.1.9.el."))
      (if window-system
	  (x-popup-dialog t (list msg '("OK" . nil) 'no-cancel) "Warning")
	(message msg)))))
;; End compatibility check

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
