;; I keep all my emacs-related stuff under ~/emacs. ~/.emacs should be pretty
;; thin. It can contain machine-specific settings, but mainly it exists to
;; load this file. Something like this:
;; (defvar my-org-dir "~/Dropbox/action")
;; (defvar clojure-path "~/clojure-svn/")
;; (setq custom-file "~/emacs/init.el")
;; (load custom-file)

(defvar nix (or (eq system-type 'cygwin)
                (eq system-type 'gnu/linux)
                (eq system-type 'linux)
                (eq system-type 'darwin)))

(defvar emacs-root "~/" "My home directory is the root of my emacs load-path.")

;; Add elisp directories under ~/emacs to my load path.
(require 'cl)
(labels ((add-path (p)
                   (add-to-list 'load-path
                                (concat emacs-root p))))
  (add-path "emacs/lisp")
  (add-path "emacs/site-lisp")
  (add-path "emacs/site-lisp/color-theme-6.6.0") ;; my color preferences
  (add-path "emacs/site-lisp/org/lisp")
  (when (< emacs-major-version 23)
    (add-path "emacs/site-lisp/remember-2.0")))

(when (eq system-type 'windows-nt)
  ;; Load emacsw32 here instead of site-start.el so it finds my org
  ;; installation.  You still have to remove it from site-start.el, though,
  ;; because that happens before this.
  (require 'emacsw32 nil t)

  ;; Put cygwin ahead for system32 for emacs and things it shells out to.
  ;; The gnuwin32 find.exe that comes with emacsw32 has a bug and doesn't
  ;; look for wildcards in the path you specify.
  (setenv "PATH" (concat "c:/bin" path-separator (getenv "PATH"))))

(defvar my-org-dir "~/action")
(defvar my-action-org (concat my-org-dir "/action.org"))
(defvar my-work-org (concat my-org-dir "/work.org"))
(defvar my-house-org (concat my-org-dir "/../house/maintenance.org"))

;; Tab defaults
(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)

;; Scroll when the cursor nears the edge, move up to a proportion of the screen
(setq scroll-margin 2
      scroll-preserve-screen-position t)
(setq-default scroll-down-aggressively 0.25
              scroll-up-aggressively 0.25)

(blink-cursor-mode 1)

;; Whitespace mode was much less subtle in 22 and used other variables
(when (>= emacs-major-version 23)
  (setq whitespace-global-modes '(c-mode clojure-mode emacs-lisp-mode ruby-mode)
        whitespace-style '(tabs trailing lines-tail space-before-tab empty
                                space-after-tab)))

;; Allow "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-splash-screen t)
(setq ring-bell-function (lambda () (message "")))

;; Allow bullet lists starting with - to delimit paragraphs for use with
;; fill-paragraph. fill-individual-paragraphs accomplishes what I want, but it
;; requires that you have an active region.
;;
;; A more generic solution will be needed to work with @param lists in C-code.
(setq paragraph-start "\f\\|[ 	]*$\\|\\([ ]+- \\)")

(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(setq scpaste-http-destination "http://paste.bighugh.com"
      scpaste-scp-destination "dh:paste.bighugh.com")

(defun init ()
  "Find my init file"
  (interactive)
  (find-file "~/emacs/init.el"))

(defun action ()
  "Find my org file"
  (interactive)
  (find-file my-action-org))

(defun work ()
  "Find my work org file"
  (interactive)
  (find-file my-work-org))

(defun house ()
  "Find my house org file"
  (interactive)
  (find-file my-house-org))

(defun next-slide ()
  "org-mode slideware, jumps to next subtree with automatic
narrowing and widening."
  (interactive)
  (outline-up-heading 1)
  (widen)
  (outline-forward-same-level 1)
  (show-subtree)
  (org-narrow-to-subtree))

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

;; cleanup, from Tim Dysinger
(defun cleanup-whitespace ()
  (interactive)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max))
  (delete-trailing-whitespace))

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

;; adapted from http://blog.bookworm.at/2007/03/pretty-print-xml-with-emacs.html
(defun my-nxml-format-region (begin end)
  "Formats XML markup in the region with line breaks and indentation."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char)
        (insert "\n"))
      (indent-region begin end))))

(defun toggle-selective-display ()
  "A poor-man's version of code folding. From jao via stevey."
  (interactive)
  (set-selective-display (if selective-display nil 1)))

;; paredit keyboard tweaks--from Bill Clementson
(require 'paredit)
(defun lisp-enable-paredit-hook () (paredit-mode 1))
(defun check-region-parens ()
  "Check if parentheses in the region are balanced. Signals a
scan-error if not."
  (interactive)
  (save-restriction
    (save-excursion
      (let ((deactivate-mark nil))
        (condition-case c
            (progn
              (narrow-to-region (region-beginning) (region-end))
              (goto-char (point-min))
              (while (/= 0 (- (point)
                              (forward-list))))
              t)
          (scan-error (signal 'scan-error
                              '("Region parentheses not balanced"))))))))

(defun paredit-backward-maybe-delete-region ()
  (interactive)
  (if mark-active
      (progn
        (check-region-parens)
        (cua-delete-region))
    (paredit-backward-delete)))

(defun paredit-forward-maybe-delete-region ()
  (interactive)
  (if mark-active
      (progn
        (check-region-parens)
        (cua-delete-region))
    (paredit-forward-delete)))

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "<delete>")
       'paredit-forward-maybe-delete-region)
     (define-key paredit-mode-map (kbd "DEL")
       'paredit-backward-maybe-delete-region)
     (define-key paredit-mode-map (kbd ";") 'self-insert-command)))

(defun my-next-previous-buffer (arg)
  "next-buffer, or previous- with prefix arg"
  (interactive "P")
  (if arg
      (previous-buffer)
    (next-buffer)))

;;; Custom keybindings
(global-set-key "\M-s"     'isearch-forward-regexp)
(global-set-key "\M-r"     'isearch-backward-regexp)

(global-set-key [f1] 'toggle-selective-display)

(global-set-key [C-tab] 'my-next-previous-buffer)

(global-set-key [f5] 'revert-buffer)
(global-set-key [f6] 'kill-this-buffer)

;; Indent region or whole buffer
(global-set-key "\C-c/" 'my-indent-region)

;; Shift+(left|right|up|down) to get to a window quicker than with C-x o
(windmove-default-keybindings)

;; Line killing goodness from emacs-fu
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

;; Fancy buffer and everything else switching
(ido-mode 1)

;; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

;; Fancy buffer listing
(require 'ibuffer) 
(setq ibuffer-saved-filter-groups
  (quote (("default"      
            ("Org" ;; all org-related buffers
              (mode . org-mode))  
            ("CounterMeasure"
              (filename . "/dev/counter/"))
            ("Handel"
              (filename . "/dev/handel/"))
            ("Programming"
              (or
                (filename . "/dev/")
                (name . "repl")
                (name . "\\*inf\\(erior\\)?-")
                (mode . c-mode)
                (mode . clojure-mode)
                (mode . cs-mode)
                (mode . emacs-lisp-mode)
                (mode . java-mode)
                (mode . perl-mode)
                (mode . python-mode)
                (mode . ruby-mode)
                )) 
            ("ERC"   (mode . erc-mode))))))

;; Project setup

;; Files to include in find-file-in-project. Requires find-file-in-project and
;; project.el to be package installed. (Get
;; http://github.com/technomancy/find-file-in-project and run `git submodule
;; update`, then package-install-from-buffer.)
(setq ffip-patterns '("*.c" "*.cs" "*.el" "*.h" "*.rb" "*.t"))
;; This is needed for ffip to work, but it's not on all my machines yet.
;;(require 'project nil t)

;; Originally from stevey, adapted to support moving to a new directory.
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive
   (progn
     (if (not (buffer-file-name))
         (error "Buffer '%s' is not visiting a file!" (buffer-name)))
     (list (read-file-name (format "Rename %s to: " (file-name-nondirectory
                                                     (buffer-file-name)))))))
  (if (equal new-name "")
      (error "Aborted rename"))
  (setq new-name (if (file-directory-p new-name)
                     (expand-file-name (file-name-nondirectory
                                        (buffer-file-name))
                                       new-name)
                   (expand-file-name new-name)))
  ;; If the file isn't saved yet, skip the file rename, but still update the
  ;; buffer name and visited file.
  (if (file-exists-p (buffer-file-name))
      (rename-file (buffer-file-name) new-name 1))
  (let ((was-modified (buffer-modified-p)))
    ;; This also renames the buffer, and works with uniquify
    (set-visited-file-name new-name)
    (if was-modified
        (save-buffer)
      ;; Clear buffer-modified flag caused by set-visited-file-name
      (set-buffer-modified-p nil))
    (message "Renamed to %s." new-name)))

;; C
(add-hook 'c-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 2)
            (setq c-basic-offset 2)))


;; C#
(autoload 'csharp-mode "csharp-mode" "Edit C# files")
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

;; Clojure
(add-hook 'clojure-mode-hook 'lisp-enable-paredit-hook)

;; CMake
(autoload 'cmake-mode "cmake-mode" "Edit CMake definitions" t)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt$" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake$"         . cmake-mode))

;; dired sorting hooks
(add-hook 'dired-mode-hook
          (lambda () (require 'dired-sort-map)))

;; elisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (define-key emacs-lisp-mode-map "\C-c\C-k" 'eval-buffer)
            (turn-on-eldoc-mode)
            (paredit-mode 1)))

;; erc
(setq erc-autojoin-channels-alist '(("freenode.net" "#clojure")))
(defvar my-erc-frame nil "Cache the frame where ERC lives to raise later")
(defvar my-erc-buffer nil "Cache the buffer ERC to see if it's alive later")
(defun my-erc ()
  "Starts ERC in a new frame with Georgia font. If ERC is
running, raises the most recently updated ERC buffer."
  (interactive)

  ;; Make a frame if the one isn't there
  (unless (frame-live-p my-erc-frame)
    (setq my-erc-frame (select-frame (make-frame)))
    (unless nix
      (set-frame-font "Georgia-12")
      ;; Widen a bit to correct timestamp display at right edge.
      (set-frame-width my-erc-frame (+ (frame-width my-erc-frame) 2))))

  ;; Open ERC if the buffer is dead
  (unless (buffer-live-p my-erc-buffer)
    (load "~/.emacs.d/.ercpass")
    (select-frame my-erc-frame)
    (setq my-erc-buffer (erc :nick erc-nick :password erc-password)))

  (raise-frame (select-frame my-erc-frame))
  (switch-to-buffer my-erc-buffer))

;; Update my-erc-buffer so my-erc always displays the most recently updated
;; erc buffer.
(add-hook 'erc-insert-post-hook
          (lambda () (setq my-erc-buffer (current-buffer))))

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
(require 'org-install)
(defun my-org-todo-done ()
  (interactive)
  (org-todo 'done))
(setq org-publish-project-alist
      `(("workorg"
         :base-directory ,my-org-dir
         :exclude ".org"
         :include ("apps.org")
         :publishing-directory "z:/users/shawn/html"
         :section-numbers t
         :table-of-contents t)
        ("workdocs"
         :base-directory ,my-org-dir
         :base-extension "docx\\|pptx"
         :publishing-directory "z:/users/shawn/html"
         :publishing-function org-publish-attachment)
        ("work" :components ("workorg" "workdocs"))
        ("clojure-box-org"
         :base-directory "c:/users/shawn/dev/clojure-box/web"
         :publishing-directory "/plinkx:dh:clojure.bighugh.com")
        ("clojure-box-extra"
         :base-directory "c:/users/shawn/dev/clojure-box/web"
         :base-extension "css"
         :publishing-function org-publish-attachment
         :publishing-directory "/plinkx:dh:clojure.bighugh.com")
        ("clojure-box" :components ("clojure-box-org" "clojure-box-extra"))
        ))
(add-hook 'org-mode-hook
          (lambda () (turn-on-auto-fill)))
(add-hook 'org-load-hook
          (lambda ()
            (define-key org-mode-map "\C-ca" 'org-agenda)
            (define-key org-mode-map "\C-cl" 'org-store-link)
            (define-key org-mode-map "\C-cb" 'org-iswitchb)

            ;; Make links work like chasing definitions in source code.
            (define-key org-mode-map "\M-." 'org-open-at-point)
            (define-key org-mode-map "\M-," 'org-mark-ring-goto)

            (define-key org-mode-map "\C-\M-a" 'org-archive-subtree)
            (define-key org-mode-map "\C-cd" 'my-org-todo-done)

            ;; clear this so next- previous-buffer works
            (define-key org-mode-map [C-tab] nil)

            (setq org-agenda-files
                  (list my-action-org
                        my-work-org
                        my-house-org))
            (setq org-agenda-custom-commands
                  '(("A" "30 day agenda" agenda "" ((org-agenda-ndays 30)))))))

;; Store to org file from remember-mode
(org-remember-insinuate)
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(setq org-directory my-org-dir)
(setq org-default-notes-file my-action-org)
(setq org-remember-templates
      `(("Home" ?h
         "* %^{headline}\n  %i%?\n  %a\n  %U" ,my-action-org)
        ("Work" ?w
         "* %^{headline}\n  %i%?\n  %a\n  %U" ,my-work-org)))

;; Make remember insert new notes at top
(setq org-reverse-note-order t)
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
;; .rb is set up by elpa
(add-to-list 'auto-mode-alist '("\\.t$"  . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$"  . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$"  . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rxml$"  . ruby-mode))

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

;; XAML
(add-to-list 'auto-mode-alist '("\\.xaml$" . nxml-mode))

;; Make sure there's a server. EmacsW32 and Aquamacs normally start it
;; automatically, but just in case...
(add-hook 'term-setup-hook
          (lambda ()
            (unless (and (boundp 'server-mode) server-mode)
              (require 'server)
              (server-start))))

;; OS X-specific setup
(setq mac-command-modifier (quote meta))
(setq mac-option-modifier (quote alt))
(when (featurep 'aquamacs)
  (tabbar-mode -1)
  (define-key osx-key-mode-map `[(control z)] 'iconify-or-deiconify-frame))

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
 '(default-frame-alist (quote ((tool-bar-lines . 0) (fringe) (right-fringe) (left-fringe . 1) (vertical-scroll-bars . right) (menu-bar-lines . 1) (cursor-color . "#dcdccc") (scroll-bar-background . "#5f5f5f") (background-color . "gray11") (background-mode . dark) (border-color . "gray11") (foreground-color . "#dcdccc") (mouse-color . "#dcdccc"))))
 '(erc-fill-column 68)
 '(erc-fill-function (quote erc-fill-static))
 '(erc-fill-static-center 10)
 '(erc-fill-variable-maximum-indentation 5)
 '(erc-hide-list (quote ("JOIN" "PART" "QUIT")))
 '(erc-nick "shoover")
 '(erc-port 6667)
 '(erc-server "irc.freenode.net")
 '(erc-timestamp-use-align-to t)
 '(erc-user-full-name "Shawn Hoover")
 '(fill-column 78)
 '(global-hl-line-mode t)
 '(ido-create-new-buffer (quote always))
 '(indent-tabs-mode nil)
 '(ns-alternate-modifier (quote alt))
 '(org-cycle-include-plain-lists t)
 '(org-tags-column 67)
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(special-display-regexps (quote (".*SPEEDBAR.*")))
 '(tab-always-indent t)
 '(tab-width 2)
 '(transient-mark-mode t)
 '(user-full-name "Shawn Hoover")
 '(visual-scroll-margin 0)
 '(w32shell-cygwin-bin "C:\\bin")
 '(x-select-enable-clipboard t))

;;; Faces
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; Subtle face for parens in lisp modes
(require 'parenface)

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
  (declare-function color-theme-calm-forest
                    "~/emacs/color/color-theme-6.6.0/themes/color-theme-library.el" nil)
  (color-theme-calm-forest)
  (global-font-lock-mode 1)
  (global-hl-line-mode nil)
  ;; Lest we get black on black parens
  (set-face-foreground 'paren-face "green"))
 (t
  (load "~/emacs/site-lisp/color-theme-6.6.0/themes/zenburn-shawn")
  (zenburn-shawn)
  ;;(load "~/emacs/site-lisp/color-theme-6.6.0/themes/zenburn")
  ;;(zenburn)
  ))

(cd emacs-root)

