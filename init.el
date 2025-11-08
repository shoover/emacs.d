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
;; emacs/lisp. Otherwise it should be installed in the package system by
;; running bootstrap.el.

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; Paths
(defvar nix (or (eq system-type 'cygwin)
                (eq system-type 'gnu/linux)
                (eq system-type 'linux)
                (eq system-type 'darwin)))

(defvar emacs-root (expand-file-name "~/emacs") "emacs load path root")
(defvar org-directory (expand-file-name "~"))

;; Add a smattering of elisp under ~/emacs to my load path.
(add-to-list 'load-path emacs-root)
(add-to-list 'load-path (concat emacs-root "/lisp"))

;; This makes load-theme work on anything in "themes"
(setq custom-theme-directory (concat emacs-root "/themes"))

(when (eq system-type 'windows-nt)
  ;; Put cygwin ahead of system32 for emacs and things it shells out to.
  ;; This is needed by find-file-in-project, at least.
  (setenv "PATH" (concat "c:/msys64/usr/bin" path-separator (getenv "PATH"))))

(defvar my-action-org (concat org-directory "/action.org"))
(setq org-default-notes-file my-action-org)
(defvar my-work-org (concat org-directory "/work.org"))
(defvar my-notes-org (concat org-directory "/notes.org"))

(defvar my-blog-dir "~/workspace/shawnhoover.dev/content/notes/")
(defvar my-dev-dir "~/workspace")

;;; Settings

;; Tab defaults: 4 spaces
(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4
 tab-always-indent t
 ruby-indent-level 4)

;; Guess indentation style, when activated in language mode hooks. LISP
(autoload 'guess-style-guess-all "guess-style" nil t)
(autoload 'guess-style-set-variable "guess-style" nil t)
(autoload 'guess-style-guess-variable "guess-style")

(editorconfig-mode 1)

;; Scroll when the cursor nears the edge, move up to a proportion of the screen
(setq scroll-margin 2
      scroll-preserve-screen-position t)
(setq-default scroll-down-aggressively 0.25
              scroll-up-aggressively 0.25)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; mouse wheel one line at a time

(setq whitespace-global-modes '(c-mode csharp-mode clojure-mode emacs-lisp-mode
                                       python-mode ruby-mode)
      whitespace-style '(face trailing lines-char space-before-tab)
      whitespace-line-column 91)
(global-whitespace-mode 1)

(blink-cursor-mode 1)
(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)
(fset 'yes-or-no-p 'y-or-n-p) ; "y or n" instead of "yes or no"

(savehist-mode t) ; save minibuffer history

;; Allow bullet lists starting with - to delimit paragraphs for use with
;; fill-paragraph. fill-individual-paragraphs accomplishes what I want, but it
;; requires that you have an active region.
(setq paragraph-start "\f\\|[ 	]*$\\|\\([ ]+- \\)")
(setq sentence-end-double-space nil)

(setq-default fill-column 80
              emacs-lisp-docstring-fill-column 70)

(setq comment-auto-fill-only-comments 1)

;; Typing overwrites the region
(delete-selection-mode 1)

;; Expand region by semantic units
(require 'expand-region)

(ignore-errors
  (require 'ispell)
  (ispell-create-debug-buffer)
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "en_US")
  (setenv "DICPATH" (expand-file-name (concat emacs-root "/dict")))
  (setq ispell-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
          (nil "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
  (ispell-change-dictionary ispell-dictionary t))

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
(setq tramp-verbose 2)

;; Speed up tramp: https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
                    vc-ignore-dir-regexp
                    tramp-file-name-regexp))

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
(add-to-list-n 'ffip-project-file ".hg" ".svn" "build.bat" "project.clj")
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
(if (eq system-type 'windows-nt) ; override ffip Windows guessing
    (setq ffip-find-executable "c:/tools/msys64/usr/bin/find.exe"))

;;; Custom functions and mode settings
(load "text")
(load "modes")
(when (sqlite-available-p)
  (require 'sqlite-query))

;;; Custom keybindings

;; Paste with one hand
;; todo: reuse C-y?
;; Can't C-r paste into isearch minibuffer because that's reverse search.
(global-set-key "\C-r" 'yank)
(global-set-key "\M-r" 'yank-pop)
(define-key paredit-mode-map "\M-r" 'paredit-yank-pop)
(define-key minibuffer-local-shell-command-map "\M-r" 'yank-pop)

(global-set-key "\M-k" 'kill-sexp)
(global-set-key "\C-\M-k" 'kill-sentence)

(global-set-key "\C-s" 'isearch-forward)
(global-set-key "\M-s" 'isearch-backward)
(define-key isearch-mode-map "\M-s" 'isearch-repeat-backward)
(global-set-key "\C-xs" 'isearch-forward-regexp)
(global-set-key "\C-x\M-s" 'isearch-backward-regexp)

(define-key ido-common-completion-map "\M-s" 'ido-prev-match)
(define-key ido-file-dir-completion-map "\M-s" 'ido-prev-match)

;; Multiple cursors, similar to IntelliJ multiselect
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(require 'multiple-cursors-core)
(define-key mc/keymap (kbd "<return>") nil) ; Require C-g to get out of multiple cursors

(global-set-key (kbd "C-.") 'ido-imenu-anywhere)

;; I want to remap C-i in text-mode but leave tab working as normal. This is
;; the only way I've found that works without messing up local keymaps like
;; the minibuffer's. http://stackoverflow.com/a/11319885/223029
;; Works great except you have to remember to use H-i everywhere you want to override.
;; Character displays can't distinguish tab from C-i, so forget it there.
(when (display-graphic-p)
  (define-key input-decode-map "\C-i" (kbd "H-i")))

;; Remap transpose so C-t is available to create a buffer like Chrome tabs
(global-set-key "\M-t" 'transpose-chars)
(global-set-key "\M-T" 'transpose-words)

(global-set-key "\M-Q" 'fill-paragraph-forward)

(global-set-key [C-tab] 'next-previous-buffer)
(global-set-key "\M-`" 'other-frame)

(global-set-key "\C-x\C-f" 'my-ido-find-file)
(global-set-key "\C-xf" 'my-ido-find-file)
(global-set-key "\C-xk" 'my-ido-kill-buffer)
(global-set-key "\C-xb" 'my-switch-to-buffer)
(global-set-key [f5] 'revert-buffer)
(global-set-key [f6] 'kill-current-buffer)
(global-set-key [f7] 'find-file-in-project-by-selected)
(global-set-key "\C-t" 'switch-to-new-untitled-buffer)

(global-set-key [C-up] 'move-text-up)
(global-set-key [C-down] 'move-text-down)

(global-set-key [S-up] 'scroll-down-line)      ; Scroll without moving point
(global-set-key [S-down] 'scroll-up-line)
(global-set-key [C-S-up] 'slide-previous-line) ; Scroll and move point
(global-set-key [C-S-down] 'slide-next-line)

(global-set-key "\C-c/" 'indent-region) ; Indent region or whole buffer
(global-set-key (kbd "C-;") 'align)     ; Align inline comment region

(global-set-key (kbd "C-=") 'er/expand-region) ; Expand region by semantic units

;; Jump to elisp source. Thanks, emacsredux.com.
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

;; Hop around windows a little quicker than with C-x o
(global-set-key (kbd "M-o") 'other-window)

;; Change font size on the fly.
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-_") 'text-scale-decrease)

(global-set-key "\C-xx" 'w32-explore-here)
(global-set-key "\C-cg" 'browse-url-at-point)

(global-set-key (kbd "C-S-b") 'save-compile-project)

(global-set-key [remap keyboard-quit] #'er-keyboard-quit)

;; OS X-specific keys
(when (eq system-type 'darwin)
  (setq mac-command-modifier (quote meta))
  (setq mac-option-modifier (quote alt))
  (global-set-key (kbd "A-M-h") 'ns-do-hide-others) ; Cmd-Option-H, Win-Alt-H
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs) ; Cmd-H
                  )

;; Snippets. Enable TAB expansion globally, but only after typing something;
;; if I'm moving around and press tab, I want it to indent the line.
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs (concat emacs-root "/snippets"))
(yas-global-mode 1)
(setq yas-expand-only-for-last-commands '(self-insert-command org-self-insert-command))

;; scpaste; this is available on ELPA, but I have patched it to work with pscp/plink
(require 'scpaste) ; LISP
(when (eq system-type 'windows-nt)
  (setq scpaste-scp "pscp"
        scpaste-ssh "plink"))

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
 '(custom-safe-themes
   '("73f7374c18e446d7e2e135c580247e0a696ec373d2f446cc617ea6beb1c47788" default))
 '(package-selected-packages
   '(caddyfile-mode cider csv-mode dockerfile-mode edit-server editorconfig
                    exec-path-from-shell expand-region find-file-in-project
                    go-mode gptel htmlize imenu-anywhere inf-ruby magit
                    markdown-mode multiple-cursors nginx-mode org-contrib
                    paredit powershell simple-httpd sql-indent swift-mode
                    transient websocket yaml-mode yasnippet)))

;; Pull these customized variables out of the generated custom block that gets overwritten on OS X for some reason.
(setq
 ansi-color-for-comint-mode t
 ansi-color-names-vector ["#454545" "#cd7542" "#6aaf50" "#baba36" "#5180b3" "#ab75c3" "#bdbc61" "#bdbdb3"]
 c-doc-comment-style 'set-from-style
 column-number-mode t
 completion-ignored-extensions
 '(".obj" ".pdb" ".svn/" "CVS/" ".o" "~" ".bin" ".bak" ".obj" ".map" ".ico" ".pif" ".lnk" ".a" ".ln" ".blg" ".bbl" ".dll" ".drv" ".vxd" ".386" ".elc" ".lof" ".glo" ".idx" ".lot" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo")
 select-enable-clipboard t
 visual-line-mode t
 visual-scroll-margin 0)
(tool-bar-mode 0)
(setq user-full-name "Shawn Hoover")
;;(customize-set-variable 'user-mail-address "shawn.hoover@gmail.com")

(when (display-graphic-p)
  (load-theme 'ample)
  (load "themes/org-fancy")

  (global-hl-line-mode t)

  (show-paren-mode t)
  (setq show-paren-style 'mixed)

  ;; Subtle face for parens in lisp modes
  (require 'parenface)                  ; LISP
  )

(cd "~")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:inherit default :weight bold :foreground "#bdbc91" :underline nil :family "Helvetica" :height 1.5 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#bdbc91" :underline nil :family "Helvetica" :height 1.4))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#bdbc91" :underline nil :family "Helvetica" :height 1.3 :slant italic))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#bdbc91" :underline nil :family "Helvetica" :height 1.3))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#bdbc91" :underline nil :family "Helvetica" :height 1.3 :slant italic))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#bdbc91" :underline nil :family "Helvetica" :height 1.3))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#bdbc91" :underline nil :family "Helvetica" :height 1.3))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#bdbc91" :underline nil :family "Helvetica" :height 1.3))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#bdbc91" :underline nil :family "Helvetica" :height 1.3)))))
