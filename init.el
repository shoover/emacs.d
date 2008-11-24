;; I keep all my emacs-related stuff under ~/emacs. ~/.emacs should be pretty
;; thin. It can contain machine-specific settings, but mainly it exists to
;; load this file.

(defvar emacs-root (if (or (eq system-type 'cygwin)
                           (eq system-type 'gnu/linux)
                           (eq system-type 'linux))
                       "~/"
                     "c:/users/shawn/")
  "My home directory is the root of my emacs load-path.")

;; Add elisp directories under ~/emacs to my load path.
(require 'cl)
(labels ((add-path (p)
                   (add-to-list 'load-path
                                (concat emacs-root p))))
  (add-path "emacs/lisp")
  (add-path "emacs/site-lisp") ;; elisp stuff I find on the 'net
  (add-path "emacs/site-lisp/clojure")
  (add-path "emacs/site-lisp/color-theme-6.6.0") ;; my color preferences
  (add-path "emacs/site-lisp/org-6.12b/lisp")
  (add-path "emacs/site-lisp/remember-2.0")
  (add-path "emacs/site-lisp/slime-cvs")
  (add-path "emacs/site-lisp/swank-clojure")
  )

;; Code to integrate cygwin emacs and screen. Might not actually care about
;; this since I never got around to running a persistent cygwin emacs server.
;; (add-hook 'after-init-hook 'server-start)
;; (add-hook 'server-done-hook
;;   (lambda ()
;;     (shell-command
;;       "screen -r -X select `cat ~/tmp/.emacsclient-caller`")))
 
;; Printing
;; TODO: figure out the printer based on where we are
(defvar printer-name "//FPGACRUNCHER/Printer4")

;; Tab defaults
(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)

;; Allow "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

(setq inhibit-splash-screen t)

;; Allow bullet lists starting with - to delimit paragraphs for use with
;; fill-paragraph. fill-individual-paragraphs accomplishes what I want, but it
;; requires that you have an active region.
;;
;; A more generic solution will be needed to work with @param lists in C-code.
(setq paragraph-start "\f\\|[ 	]*$\\|\\([ ]+- \\)")

;;; Functions

(defun emacs ()
  "Find my elisp code"
  (interactive)
  (find-file "~/emacs/init.el"))

(defun gtd ()
  "Find my org-mode list"
  (interactive)
  (find-file "~/action/action.org"))

(defun indent-buffer ()
  "Indent the entire buffer. Seems like emacs should have this."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun my-indent-region ()
  "Indent the region if there is one active. Otherwise indent the buffer."
  (interactive)
  (save-excursion
    (if mark-active
        (indent-region (region-beginning) (region-end))
      (indent-buffer))))

(defun count-chars-region (beginning end)
  "Displays a message with the number of characters in the region."
  (interactive "r")
  (save-excursion
    (goto-char beginning)
    (let ((count 0))
      (while (< (point) end)
        (forward-char)
        (incf count))
      (message "%d characters" count))))

(defun toggle-selective-display ()
  "A poor-man's version of code folding. From jao."
  (interactive)
  (set-selective-display (if selective-display nil 1)))

;;; Custom keybindings
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\M-s"     'isearch-forward-regexp)
(global-set-key "\M-r"     'isearch-backward-regexp)

(global-set-key [f1] 'toggle-selective-display)

(global-set-key [C-tab] 'next-buffer)
(global-set-key [f6] 'kill-this-buffer)

;; Rebind C-M-\. The default indent-region just isn't very useful for me.
(global-set-key "\C-\M-\\" 'my-indent-region)

;; Shift+(left|right|up|down) to get to a window quicker than with C-x o
(windmove-default-keybindings)

;; Buffer switching
(require 'iswitchb)
(iswitchb-mode 1)

;; C
(add-hook 'c-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (setq tab-width 2)
             (setq c-basic-offset 2)))


;; C#
(autoload 'csharp-mode "csharp-mode" "Edit C# files")
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))


;; Clojure
;; Perhaps someday I'll want this to be buffer local, but let's try it
;; globally for now.
(defvar clojure-path "c:/users/shawn/clojure/work/")
(setq inferior-lisp-program
      (let* ((java-path "java")
             (java-options "")
             (class-path-delimiter ";")
             (class-path (mapconcat (lambda (s) s)
                                    ;; Add other paths to this list
                                    ;; if you want to have other
                                    ;; things in your classpath.
                                    (list (concat clojure-path "clojure.jar")
                                          (concat clojure-path "../contrib/clojure-contrib.jar"))
                                    class-path-delimiter)))
        (concat java-path
                " " java-options
                " -cp " class-path " clojure.lang.Repl")))
(require 'clojure-paredit)
(require 'swank-clojure-autoload)       
(swank-clojure-config
 (setq swank-clojure-jar-path (concat clojure-path "clojure.jar"))
 ;;(setq swank-clojure-extra-classpaths (list "/path/to/extra/classpaths"))
 )
(autoload 'slime "slime" "Load slime for swank-clojure" t)


;; CMake
(autoload 'cmake-mode "cmake-mode" "Edit CMake definitions" t)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt$" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake$"         . cmake-mode))

;; dired sorting hooks
(add-hook 'dired-mode-hook
          (lambda () (require 'dired-sort-map)))


;; erc
(setq erc-autojoin-channels-alist '(("freenode.net" "#clojure")))


;; Erlang
(defun my-erlang ()
  "Load erlang. It's just in a function because I don't use it often enough
   to need all the time."
  (interactive)

  ;; TODO: get erlang dir from env
  (add-to-list 'load-path "C:/Program Files/erl5.5.5/lib/tools-2.5.5/emacs")
  (setq erlang-root-dir "C:/Program Files/erl5.5.5")
  (add-to-list 'exec-path "C:/Program Files/erl5.5.5/bin")
  ;; Not all my machines have erlang set up
  (ignore-errors
    (require 'erlang-start)
    (add-to-list 'load-path "c:/users/shawn/emacs/site-lisp/distel/elisp")
    (require 'distel)
    (distel-setup)))

(defun sah-erlang-drop-to-body ()
  "This function is a drop-in enhancement for comment-indent-new-line for
   erlang-mode. If you're in a function clause it jumps you down to the
   function body without having to skip past the closing ) or type the
   freakin arrow."

  (interactive)
  (undo-boundary)

  (cond
   ;; Assume ) at point is part of an argument list inserted by an electric
   ;; command, in which case the arrow is already there.
   ((looking-at ")")
    (end-of-line))
   ;; Assume preceeding ) was manually typed by user and there is no arrow.
   ((looking-back ")")
    (insert " ->"))
   (t))
  (comment-indent-new-line))

;; erlang-mode-map doesn't seem to be available after requiring erlang-start,
;; so I wait until erlang-mode is loaded to set up the keys.
(add-hook 'erlang-mode-hook (lambda ()
                              (define-key erlang-mode-map "\M-j"
                                'sah-erlang-drop-to-body)))

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(require 'org)
(add-hook 'org-mode-hook
          (lambda ()
            (turn-on-auto-fill)
            (define-key org-mode-map "\C-ca" 'org-agenda)
            (define-key org-mode-map "\C-cl" 'org-store-link)
            
            ;; Variables used to save remember notes
            (setq org-directory "~/action")
            (setq org-default-notes-file "~/action/action.org")

            ;; One template--insert note at top of org file
            (setq org-remember-templates
                  '((?t "%?\n  %i\n  %a" "~/action/action.org")))
            ;;(?j "* %U %?\n\n  %i\n  %a" "~/.notes")
            ;;(?i "* %^{Title}\n  %i\n  %a" "~/.notes" "New Ideas")))

            ;; Make remember insert new notes at top
            (setq org-reverse-note-order t)))

;; Store to org file from remember-mode
(require 'remember)
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

;; Make
(setq compile-command "make ")

;; Mercurial
(require 'mercurial)

;; NSIS
(autoload 'nsis-mode "nsis-mode" "Edit Nullsoft installer scripts" t)
(add-to-list 'auto-mode-alist '("\\.nsi.tmpl$"  . nsis-mode))
(add-to-list 'auto-mode-alist '("\\.nsh$"  . nsis-mode))

;; PHP
(add-to-list 'auto-mode-alist '("\\.php$" . html-mode))

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

;; Text
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; YAML
(autoload 'yaml-mode "yaml-mode" "Edit YAML files" t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(server-start)

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
 '(completion-ignored-extensions (quote (".obj" ".pdb" ".svn/" "CVS/" ".o" "~" ".bin" ".bak" ".obj" ".map" ".ico" ".pif" ".lnk" ".a" ".ln" ".blg" ".bbl" ".dll" ".drv" ".vxd" ".386" ".elc" ".lof" ".glo" ".idx" ".lot" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo")))
 '(erc-hide-list (quote ("JOIN" "PART" "QUIT")))
 '(erc-nick "shoover")
 '(erc-port 6667)
 '(erc-prompt-for-password nil)
 '(erc-server "irc.freenode.net")
 '(erc-user-full-name "Shawn Hoover")
 '(fill-column 78)
 '(global-hl-line-mode t)
 '(indent-tabs-mode nil)
 '(org-agenda-files (quote ("~/action/action.org")))
 '(org-cycle-include-plain-lists t)
 '(org-tags-column 67)
 '(pr-gs-command "c:\\Program Files\\gs\\gs8.62\\bin\\gswin32c.exe")
 '(pr-gv-command "C:\\Program Files\\Ghostgum\\gsview\\gsview32.exe")
 '(show-paren-mode t)
 '(tab-always-indent t)
 '(tab-width 2)
 '(transient-mark-mode t)
 '(w32shell-cygwin-bin "C:\\cygwin\\bin"))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "wheat" :foreground "black" :inverse-video t :box (:line-width 1 :color "wheat")))))
 '(mode-line-highlight ((t (:inherit highlight :background "black" :foreground "wheat" :inverse-video nil))))
 '(tooltip ((t (:inherit variable-pitch :background "systeminfowindow" :foreground "black")))))


;; Pretty black background color theme.
;; Call this after all the other useless color settings so none of its
;; beauty gets overwritten by them.
;;
;; Other decent themes are Charcoal black, Goldenrod (in a brownish sort
;; of way), Calm forest, Blue sea (for a blue background), and Classic.
(require 'color-theme)
;; For load time efficiency, only my theme is loaded. Run
;; color-theme-initialize at any time to see the rest of the themes.
(cond
 ((< emacs-major-version 22)
  (color-theme-initialize)
  (declare-function color-theme-calm-forest "~/emacs/color/color-theme-6.6.0/themes/color-theme-library.el" nil)
  (color-theme-calm-forest)
  (global-font-lock-mode 1)
  (global-hl-line-mode nil))
 (t
  (load-file "~/emacs/site-lisp/color-theme-6.6.0/themes/shawn.elc")
  (color-theme-shawn)))

(cd emacs-root)
