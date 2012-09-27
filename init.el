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

(defvar emacs-root "~/emacs/" "emacs load path root")

;; Add elisp directories under ~/emacs to my load path.
(require 'cl)
(labels ((add-path (p)
                   (add-to-list 'load-path
                                (concat emacs-root p))))
  (add-path "lisp")
  (add-path "lisp/color-theme-6.6.0") ;; my color preferences
  (add-path "lisp/org/lisp")
  (add-path "lisp/auto-complete")
  (add-path "lisp/fsharp")
  (when (< emacs-major-version 23)
    (add-path "lisp/remember-2.0")))

;; I install some info files here.
;; makeinfo blah.texi
;; install-info blah emacs/info/dir
(require 'info)
(setq Info-directory-list
      (add-to-list 'Info-default-directory-list
                   (expand-file-name "info" emacs-root)))

;; No need to put this before initializing `package' because it byte-compiles
;; everything on install anyway.
(require 'byte-code-cache)

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

;; Tab defaults
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

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
(setq sentence-end-double-space nil)

(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             (concat emacs-root "lisp/auto-complete/ac-dict"))
(ac-config-default)

(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/.emacs.d/auto-save"))))

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

(defun my-indent-line ()
  "Indent ;-comments like ;;-comments for compatibility with
other Clojure programmers. Mostly cribbed from `lisp-indent-line'."
  (interactive)
  (let ((indent (calculate-lisp-indent)) shift-amt end
        (beg (progn (beginning-of-line) (point))))
    (skip-chars-forward " \t")
    (if (or (looking-at "\\s<\\s<") (not (looking-at "\\s<")))
        (lisp-indent-line)
      (if (listp indent) (setq indent (car indent)))
      (setq shift-amt (- indent (current-column)))
      (if (zerop shift-amt)
          nil
        (delete-region beg (point))
        (indent-to indent)))))

(defun my-comment-sexp ()
  (interactive)
  (save-excursion
    (mark-sexp)
    (comment-region (region-beginning) (region-end))))

;; cleanup, from Tim Dysinger
(defun cleanup-whitespace ()
  (interactive)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max))
  (delete-trailing-whitespace))

;;; From Stefan Monnier. It is the opposite of fill-paragraph. Takes a
;;; multi-line paragraph and makes it into a single line of text.
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;;; From http://www.emacswiki.org/emacs/basic-edit-toolkit.el
(defun move-text-internal (arg)
  "Move region (transient-mark-mode active) or current line."
  (let ((remember-point (point)))
    ;; Can't get correct effect of `transpose-lines'
    ;; when `point-max' is not at beginning of line
    ;; So fix this bug.
    (goto-char (point-max))
    (if (not (bolp)) (newline))         ;add newline to fix
    (goto-char remember-point)
    ;; logic code start
    (cond ((and mark-active transient-mark-mode)
           (if (> (point) (mark))
               (exchange-point-and-mark))
           (let ((column (current-column))
                 (text (delete-and-extract-region (point) (mark))))
             (forward-line arg)
             (move-to-column column t)
             (set-mark (point))
             (insert text)
             (exchange-point-and-mark)
             (setq deactivate-mark nil)))
          (t
           (let ((column (current-column)))
             (beginning-of-line)
             (when (or (> arg 0) (not (bobp)))
               (forward-line 1)
               (when (or (< arg 0) (not (eobp)))
                 (transpose-lines arg))
               (forward-line -1))
             (move-to-column column t))
           ))))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line ARG lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun move-text-down (arg)
  "Move region (transient-mar-mode active) or current line (ARG lines) down."
  (interactive "*p")
  (move-text-internal arg))

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

(defun my-comint (cmd)
  "Runs `comint', but parses cmd into a program and args like `inferior-lisp'."
  (interactive (list (if current-prefix-arg
                         ""
                       (read-string "Program and args: "))))
  (let* ((cmdlist (split-string cmd))
         (new-buf (set-buffer (apply (function make-comint)
                                     (format (car cmdlist))
                                     (car cmdlist)
                                     nil
                                     (cdr cmdlist)))))
    (pop-to-buffer new-buf)))

(defun sqlite (db)
  "Opens an interactive sqlite session in a comint buffer."
  (interactive
   (list (ido-read-file-name "DB: ")))
  (pop-to-buffer 
   (make-comint "sqlite" "sqlite3" nil "-interactive" db)))

(defun my-eval-print ()
  (interactive)
  (let ((standard-output (current-buffer)))
    (terpri)
    (eval-last-sexp t)
    (terpri)))

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

(defun my-move-buffer-other-frame ()
  "Moves the current buffer to another frame. If there's another
frame, it is used. Otherwise a new frame is created. This is
mainly useful when emacsclient opens a file in a frame that's
sized for something other than reading code or logs."
  (interactive)
  (let ((buf (current-buffer)))
    (bury-buffer)
    (let ((frame (next-frame (selected-frame))))
      (when (eq frame (selected-frame))
        (setq frame (make-frame)))
      (select-frame-set-input-focus frame)
      (switch-to-buffer buf))))

;;; Custom keybindings
(global-set-key "\M-s"     'isearch-forward-regexp)
(global-set-key "\M-r"     'isearch-backward-regexp)

(global-set-key [f1] 'toggle-selective-display)

(global-set-key [C-tab] 'my-next-previous-buffer)
(when (eq system-type 'darwin)
  (global-set-key "\M-`" 'other-frame))

(global-set-key [f5] 'revert-buffer)
(global-set-key [f6] 'kill-this-buffer)

(global-set-key [C-up] 'move-text-up)
(global-set-key [C-down] 'move-text-down)

;; Indent region or whole buffer
(global-set-key "\C-c/" 'my-indent-region)

;; Shift+(left|right|up|down) to get to a window quicker than with C-x o
(windmove-default-keybindings)

;; Snippets
(yas/load-directory (concat emacs-root "snippets"))

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
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-use-filename-at-point 'guess
      ido-create-new-buffer 'always
      ido-file-extensions-order '(".org" t))

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

; eshell prompt
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
(add-to-list 'auto-mode-alist '("\\.rl$" . c-mode))

;; C#
(autoload 'csharp-mode "csharp-mode" "Edit C# files" t)
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))
(add-hook 'csharp-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq c-basic-offset 4)
            (c-set-style "c#")))

;; Clojure
(add-hook 'clojure-mode-hook 'lisp-enable-paredit-hook)
(add-hook 'clojure-mode-hook
          (lambda ()
            (define-key clojure-mode-map "\C-c\C-l" 'my-load-buffer)
            (set (make-local-variable 'indent-line-function)
                 'my-indent-line)
            (define-clojure-indent (defmethod 'defun))))
(add-hook 'inferior-lisp-mode-hook
          (lambda ()
            (add-to-list 'comint-output-filter-functions
                         'comint-truncate-buffer)
            (setq comint-buffer-maximum-size 5000)))
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

;; From nakkaya.com
(defun my-load-buffer ()
  (interactive)
  (point-to-register 5)
  (mark-whole-buffer)
  (lisp-eval-region (point) (mark) nil)
  (jump-to-register 5))

(defun my-slime (clojure-jar)
  "Set up the classpath with a custom clojure jar."
  (interactive (list (read-string "Clojure jar:")))
  ;; (setq swank-clojure-classpath (swank-clojure-default-classpath))
  (let ((swank-clojure-classpath (cons clojure-jar
                                       (swank-clojure-default-classpath))))
    (slime)))

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
(defun my-erc (&optional server nick pass)
  "Starts ERC in a new frame with Georgia font. If ERC is
running, raises the most recently updated ERC buffer."
  (interactive)

  ;; Make a frame if the one isn't there
  (unless (frame-live-p my-erc-frame)
    (setq my-erc-frame (select-frame (make-frame)))
    (unless nix
      ;;(set-frame-font "Georgia-12")
      ;; Widen a bit to correct timestamp display at right edge.
      ;; (set-frame-width my-erc-frame (+ (frame-width my-erc-frame) 2))
      ;; No, narrow it!
      (set-frame-width my-erc-frame (- (frame-width my-erc-frame) 8))
      ))

  ;; Open ERC if the buffer is dead
  (unless (buffer-live-p my-erc-buffer)
    (load "~/.emacs.d/.ercpass")
    (select-frame my-erc-frame)
    (setq my-erc-buffer (erc :server server :nick (or nick erc-nick)
                             :password (or pass erc-password))))

  (raise-frame (select-frame my-erc-frame))
  (switch-to-buffer my-erc-buffer))

;; Update my-erc-buffer so my-erc always displays the most recently updated
;; erc buffer.
(add-hook 'erc-insert-post-hook
          (lambda () (setq my-erc-buffer (current-buffer))))

(defun my-erc-scroll-to-bottom ()
  (interactive)
  (end-of-buffer)
  (previous-line)
  (recenter 0))
(eval-after-load 'erc
  '(progn
     (define-key erc-mode-map "\M->" 'my-erc-scroll-to-bottom)))

;; F#
(setq auto-mode-alist (cons '("\\.fs[iylx]?$" . fsharp-mode) auto-mode-alist))
(autoload 'fsharp-mode "fsharp" "Major mode for editing F# code." t)
(autoload 'run-fsharp "inf-fsharp" "Run an inferior F# process." t)
(defvar inferior-fsharp-program "\"c:\\Program Files (x86)\\Microsoft F#\\v4.0\\Fsi.exe\"")
(defvar fsharp-compiler "\"c:\\Program Files (x86)\\Microsoft F#\\v4.0\\Fsc.exe\"")
(add-hook 'fsharp-mode-hook
          (lambda ()
            (define-key fsharp-mode-map "\C-c\C-b" 'my-fsharp-load-buffer)
            (define-key fsharp-mode-map "\C-c\C-l" 'my-fsharp-load-line)))
(add-hook 'inferior-fsharp-mode-hooks
          (lambda ()
            (add-to-list 'comint-output-filter-functions
                         'comint-truncate-buffer)
            (setq comint-buffer-maximum-size 5000)))

(defun my-fsharp-load-buffer ()
  (interactive)
  (fsharp-eval-region (point-min) (point-max)))

(defun my-fsharp-load-line ()
  (interactive)
  (fsharp-eval-region (point-at-bol) (point-at-eol)))

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
          (lambda ()
            (turn-on-auto-fill)))
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
                  (append
                   (list my-action-org
                         my-work-org)
                   (directory-files (concat my-org-dir "/../docs/banjo") t "\\.org$")))
            (setq org-agenda-custom-commands
                  '(("A" "30 day agenda" agenda "" ((org-agenda-ndays 30)))))
            (setq org-refile-targets '((org-agenda-files :maxlevel . 1))
      org-refile-use-outline-path 'file
      org-refile-allow-creating-parent-nodes 'confirm)))
(setq org-mobile-inbox-for-pull (concat my-org-dir "/flagged.org")
      org-mobile-directory (concat my-org-dir "/../MobileOrg"))

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
(setq org-reverse-note-order t) ;; new notes at top

;; remember frames adapted from
;; http://github.com/LauJensen/Configs/blob/master/emacs
(defun remember-frame-p ()
  (equal "*Remember*" (frame-parameter nil 'name)))
;; Org-remember splits windows, force it to a single window
(add-hook 'remember-mode-hook
          (lambda ()
            (when (remember-frame-p)
              (delete-other-windows))))
;; Automatic closing of remember frames
(defadvice remember-finalize (after delete-remember-frame activate)
  "Advise remember-finalize to close the frame if it is the remember frame"
  (when (remember-frame-p)
    (delete-frame)))
(defadvice remember-destroy (after delete-remember-frame activate)
  "Advise remember-destroy to close the frame if it is the remember frame"
  (when (remember-frame-p)
    (delete-frame)))
(defun make-remember-frame ()
  "Create a new frame and run org-remember"
  (interactive)
  (make-frame '((name . "*Remember*") (width . 80) (height . 20)))
  (select-frame-by-name "*Remember*")
  (org-remember))

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
 '(erc-hide-list (quote ("JOIN" "NICK" "PART" "QUIT")))
 '(erc-nick "shoover")
 '(erc-nick-uniquifier "_")
 '(erc-port 6667)
 '(erc-server "irc.freenode.net")
 '(erc-server-reconnect-attempts 4)
 '(erc-server-reconnect-timeout 30)
 '(erc-timestamp-use-align-to t)
 '(erc-user-full-name "Shawn Hoover")
 '(fill-column 78)
 '(global-hl-line-mode t)
 '(hg-outgoing-repository "")
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
 '(user-mail-address "shawn@bighugh.com")
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
  ;;(load "~/emacs/lisp/color-theme-6.6.0/themes/zenburn-shawn")
  ;;(zenburn-shawn)
  ;;(load "~/emacs/lisp/color-theme-6.6.0/themes/zenburn")
  ;;(zenburn)

  (load "~/emacs/lisp/color-theme-6.6.0/themes/blackboard")
  (color-theme-blackboard)
  ))

(cd "~")
