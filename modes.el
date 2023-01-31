;;; Modes setup and mode-specific functions

(defmacro define-keys (map &rest pairs)
  "Define multiple keys at once in keymap MAP. PAIRS are associations, for example
(\"C-l\" . 'load)."
  (let ((forms (mapcar (lambda (pair) `(define-key ,map ,(car pair) ,(cdr pair))) pairs)))
    `(progn ,@forms)))

(defun add-to-mode-alist (mode &rest patterns)
  "Adds each pattern in PATTERNS to `auto-mode-alist' for MODE."
  (dolist (p patterns)
    (add-to-list 'auto-mode-alist (cons p mode))))

;; AutoHotkey
(autoload 'autohotkey-mode "autohotkey-mode" "Edit AutoHotkey scripts" t)
(add-to-list 'auto-mode-alist '("\\.ahk$" . autohotkey-mode))

;; Basic
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(add-to-mode-alist 'visual-basic-mode "\\.frm$" "\\.bas$" "\\.cls$" "\\.vbs$")

;; C
(add-hook 'c-mode-hook 'my-c-mode-hook)
(defun my-c-mode-hook ()
  (c-set-offset 'substatement-open 0)
  (define-key c-mode-map "\C-c\C-c" 'compile)

  ;; Break paragraphs at doxygen commands already on separate lines, e.g. @code.
  (set (make-local-variable 'paragraph-separate)
       "[ 	]*\\(//+\\|\\**\\)[ 	]*\\(@[a-zA-Z0-9,_-]+\\({.*}\\)*\\)*[ 	]*$\\|^"))

(add-to-list 'auto-mode-alist '("\\.rl$" . c-mode)) ; Ragel

;; C#
(setq csharp-want-imenu nil) ; imenu noticeably slows fontification of large files

;; Calc
(add-hook 'calc-mode-hook 'my-calc-mode-hook)
(defun my-calc-mode-hook ()
  (define-key calc-mode-map [C-backspace] 'calc-reset))

(add-hook 'calc-start-hook 'my-calc-start-hook)
(require 'subr-x) ; when-let
(defun my-calc-start-hook ()
  ;; Prevent server buffers from popping up in little calc windows.
  (dolist (buf (list calc-main-buffer calc-trail-buffer))
    (when-let ((win (get-buffer-window buf)))
      (set-window-dedicated-p win t))))

;; Changelog. I don't use it.
(rassq-delete-all 'change-log-mode auto-mode-alist)

;; Clojure
(add-hook 'clojure-mode-hook 'lisp-enable-paredit-hook)
(add-hook 'clojure-mode-hook 'my-clojure-mode-hook)
(defun my-clojure-mode-hook ()
  (set-keymap-parent clojure-mode-map lisp-mode-shared-map)
  (define-key clojure-mode-map "\C-c\C-b" 'lisp-load-buffer-x)
  ;; (set (make-local-variable 'indent-line-function)
  ;;      'my-lisp-indent-line)
  )
(add-hook 'inferior-lisp-mode-hook
          (lambda ()
            (add-to-list 'comint-output-filter-functions
                         'comint-truncate-buffer)
            (setq comint-buffer-maximum-size 5000)))

;; From nakkaya.com
(defun lisp-load-buffer-x ()
  (interactive)
  (point-to-register 5)
  (mark-whole-buffer)
  (lisp-eval-region (point) (mark) nil)
  (jump-to-register 5))


;; dired sorting hooks
(add-hook 'dired-mode-hook
          (lambda () (require 'dired-sort-map)))

;; elisp
(defun my-emacs-lisp-mode-hook ()
  (define-keys emacs-lisp-mode-map
    ("\C-c\C-b" . 'eval-buffer)
    ("\C-c\C-r" . 'eval-region))

  (paredit-mode 1))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)

;; F#
(defvar inferior-fsharp-program "\"c:\\Program Files (x86)\\Microsoft F#\\v4.0\\Fsi.exe\"")
(defvar fsharp-compiler "\"c:\\Program Files (x86)\\Microsoft F#\\v4.0\\Fsc.exe\"")
(setq fsharp-tab-always-indent nil) ; try to prevent it from indenting the
                                    ; previous line when pressing Enter
(add-hook 'fsharp-mode-hook
          (lambda ()
            (define-keys fsharp-mode-map
              ("\C-c\C-b" . 'fsharp-load-buffer-x)
              ("\C-c\C-l" . 'fsharp-load-line-x)
              ("\C-c\C-u" . 'fsharp-goto-block-up)
              ("\C-x\C-e" . 'fsharp-eval-phrase))))
(add-hook 'inferior-fsharp-mode-hooks
          (lambda ()
            (add-to-list 'comint-output-filter-functions
                         'comint-truncate-buffer)
            (setq comint-buffer-maximum-size 2000)))

(defun fsharp-load-buffer-x ()
  (interactive)
  (fsharp-eval-region (point-min) (point-max)))

(defun fsharp-load-line-x ()
  (interactive)
  (fsharp-eval-region (point-at-bol) (point-at-eol))
  (move-beginning-of-line 2))

;; Hack this into inferior-fsharp-eval-region to make aquamacs 3.2/emacs 24.4
;; not raise the frame if inferior-fsharp is in another frame.
;; (display-buffer inferior-fsharp-buffer-name
;;                 '(display-buffer-reuse-window (inhibit-switch-frame t)))

;; generic modes built in, e.g. bat-mode
(require 'generic-x)

;; INI, hgrc
(add-to-list 'auto-mode-alist '("hgrc$" . ini-generic-mode))

;; Lua
(add-hook 'lua-mode-hook
          (lambda ()
            (define-keys lua-mode-map
              ("\C-x\C-e" . 'lua-send-defun)
              ("\C-\M-x" . 'lua-send-defun))
            (define-keys lua-prefix-mode-map ; C-c
              ("\C-s" . 'lua-show-process-buffer)
              ("\C-z" . 'lua-switch-to-inf)
              ("\C-u" . 'lua-beginning-of-proc)
              ("\C-l" . 'lua-send-current-line)
              ("\C-b" . 'lua-send-buffer)
              ("\C-r" . 'lua-send-region))))

;; Borrowed from inf-ruby and cut down
(defun lua-switch-to-inf (eob-p)
  "Switch to the Lua process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (lua-show-process-buffer)
  (pop-to-buffer lua-process-buffer)
  (cond (eob-p
         (push-mark)
         (goto-char (point-max)))))

;; Lisp
(require 'lisp-mode)
(define-keys lisp-mode-shared-map
  ("\M-a" . 'backward-sexp)
  ("\M-e" . 'forward-sexp)
  ("\M-k" . 'kill-sexp)
  ("\C-\M-y" . 'reverse-transpose-sexps)
  ("\C-c\C-u" . 'backward-up-list))

;; Log files
(autoload 'log-mode "log-mode" "View log files" t)
(add-to-list 'auto-mode-alist '("\\.log$" . log-mode))

;; Make
(setq compile-command "make ")

;; markdown
(setq markdown-command "pandoc")

;; msbuild
(add-to-list 'auto-mode-alist '("\\.targets$" . nxml-mode))

;; NSIS
(autoload 'nsis-mode "nsis-mode" "Edit Nullsoft installer scripts" t)
(add-to-mode-alist 'nsis-mode "\\.nsh$" "\\.nsi.tmpl$")

;; org-mode
(load "my-org-helpers.el")

;; Fix weird scrolling of the org export dispatch buffer by unsetting my
;; typical scroll variables.
(when (functionp 'advice-add) ;; emacs >= 24.4
  (advice-add 'org-export-dispatch :around #'my-org-export-dispatch-advice)
  (defun my-org-export-dispatch-advice (orig-fun &rest args)
    (let ((scroll-margin 0)
          (scroll-preserve-screen-position nil))
      (apply orig-fun args))))

(add-hook 'org-mode-hook 'my-org-mode-hook)
(defun my-org-mode-hook ()
  (turn-on-auto-fill)

  ;; orgmode defines a comment syntax; make sure regular paragraphs auto-fill, too.
  (setq-local comment-auto-fill-only-comments nil))

(add-hook 'org-load-hook 'my-org-load-hook)
(defun my-org-load-hook ()
  (define-keys org-mode-map
    ("\C-ca" . 'org-agenda)
    ("\C-cl" . 'org-store-link)
    ("\C-cw" . 'copy-org-link-at-point)
    ("\C-cd" . 'org-datetree-find-create-here-x)
    ("\C-c\S-d" . 'org-datetree-find-create-here-skip-month-x)
    ("\C-ct" . 'org-toggle-item)

    ;; Make links work like chasing definitions in source code.
    ("\M-." . 'org-open-at-point)
    ("\M-," . 'org-mark-ring-goto)

    ("\C-\M-a" . 'org-archive-subtree)
    ("\C-\M-p" . 'org-promote-subtree-x)

    ;; Compatibility with my isearch keys
    ("\C-c\C-x\C-r" . 'org-paste-special)

    ;; clear this so next- previous-buffer works
    ([C-tab] . nil)

    ;; clear up/down priority/clock keys for scrolling
    ([S-up] . nil)
    ([S-down] . nil)
    ([C-S-up] . nil)
    ([C-S-down] . nil))

  (setq org-directories (list org-directory))
  (let ((b (concat org-directory "/../banjo")))
    (when (file-exists-p b)
      (add-to-list 'org-directories b)))

  (setq org-agenda-files (find-org-files-x))
  (setq org-agenda-custom-commands
        '(("A" "Multi-occur, agenda files and archives"
           search ""
           ((org-agenda-files (find-org-files-x "\\.org$\\|org_archive$"))))
          ("M" "Tags, agenda files and archives"
           tags ""
           ((org-agenda-files (find-org-files-x "\\.org$\\|org_archive$"))))
          ("P" "Project list"
           tags "prj"
           ((org-use-tag-inheritance nil)))
          ("p" "Project list, current buffer"
           tags-tree "prj"
           ((org-use-tag-inheritance nil)))
          ("w" "Work-in-progress"
           tags "WIP"
           ((org-use-tag-inheritance nil)))
          ))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 2))
        org-refile-use-outline-path 'file
        org-refile-allow-creating-parent-nodes 'confirm)

  (setq org-src-fontify-natively t
        org-edit-src-content-indentation 0)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(; (fsharp . t); I got a babel helper from https://github.com/fradav/ob-fsharp/blob/master/ob-fsharp.el,
                   ; but it doesn't work
     (ruby . t))))

(setq org-indent-mode t
      org-hide-leading-stars t
      org-startup-indented 'overview

      org-tags-column -85

      org-link-search-must-match-exact-headline nil ; Please let fuzzy search work
      org-export-backends '(ascii html latex md odt)
      org-html-validation-link nil)

;; These should prevent underscores without {} from exporting as subscripts... but don't?
(setq org-use-sub-superscripts '{}
      org-export-with-sub-superscripts '{})

;; 'sah-org-article' for exporting org documents as 'article'.
;; Requires xelatex for fontspec (on the PATH via miktex on Windows)
;; Use by setting #+LATEX_CLASS: sah-org-article
(require 'ox-latex)
(setq org-latex-pdf-process
 '("xelatex -interaction nonstopmode %f"
   "xelatex -interaction nonstopmode %f"))
(add-to-list 'org-latex-classes
  '("sah-org-article"
"\\documentclass[12pt,letterpaper]{article}
\\usepackage{geometry}
\\geometry{letterpaper, margin=1.25in,
           marginparsep=7pt, marginparwidth=.6in}

% use some default packages otherwise excluded by NO-* below
\\usepackage[T1]{fontenc}
\\usepackage{graphicx}
\\usepackage[breaklinks]{hyperref}

% use system fonts
\\usepackage{fontspec}
\\setromanfont{Palatino Linotype}
\\setsansfont[Scale=0.8]{Bitstream Vera Sans}
\\setmonofont[Scale=0.8]{Monaco}

% smash down list items for orgmode docs
\\usepackage{paralist}

% Without these lines, my fonts don't work and there's a weird front matter page
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]
"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(require 'org-protocol)

;; Advise org-protocol-capture to always wrap in a new capture frame. This
;; would be risky with, say, org-capture, but since protocol capture is never
;; called within emacs interactively and only from other programs, I think
;; it's ok. If it became a problem we would have to register a new protocol
;; and handler just for wrapping.
(when (functionp 'advice-add) ;; emacs >= 24.4
  (advice-add 'org-protocol-capture :around #'my-org-protocol-advice)
  (defun my-org-protocol-advice (orig-fun &rest args)
    (with-capture-frame
     (apply orig-fun args))))

(setq org-capture-templates
      ;; standard capture: blank headline, paste region
      '(("a" "Action"
         entry (file org-default-notes-file)
         "* %?\n%i"
         :prepend t)

        ("n" "Notes"
         entry (file+datetree my-notes-org)
         "* %?\n%i")

        ("w" "Work"
         entry (file my-work-org)
         "* %?\n%i"
         :prepend t)

        ("o" "Workout Table"
         table-line (file+function (concat org-directory "/workout.org")
                                   org-datetree-find-date-create-month-x)
         ;; insert date, prompt for Time, enter the rest manually
         "|%<%m-%d %a>|%^{Time}|%?")

        ;; clipboard capture: blank headline, paste OS clipboard
        ("v" "Templates for pasting the OS clipboard")

        ("va" "Action, paste clipboard"
         entry (file org-default-notes-file)
         "* %?\n%x"
         :prepend t)

        ("vw" "Work, paste clipboard"
         entry (file my-work-org)
         "* %?\n%x"
         :prepend t)

        ("vn" "Notes, paste clipboard"
         entry (file+datetree my-notes-org)
         "* %?\n%x"
         :empty-lines 1)

        ;; org-protocol capture: the handler puts the link/title in the kill ring %c
        ;; and selected text in the region %i
        ;; Alt: "* %?[[%:link][%:description]]\n%:initial\n%U"
        ;; Update HKEY_CLASSES_ROOT\org-protocol\shell\open\command.
        ("c" "org-protocol capture"
         entry (file read-org-agenda-file)
         "* %?%c\n%i" :prepend t)

        ("N" "org-protocol Notes capture"
         entry (file+datetree my-notes-org)
         "* %:description\n[[%:link][www]]\n\n%:initial%?\n"
         ;; "* %?%c\n%i\n"
         )))

;; Paredit
(require 'paredit)
(defun lisp-enable-paredit-hook () (paredit-mode 1))
(eval-after-load 'paredit
  '(progn
     (define-keys paredit-mode-map
       ("\M-s" . nil)                   ; override splice
       ("\M-S" . nil))))                ; split

;; PHP
(add-to-list 'auto-mode-alist '("\\.php$" . html-mode))

;; prog-mode derivees
(add-hook 'prog-mode-hook 'my-prog-mode-hook)
(defun my-prog-mode-hook ()
  ;; Make a hook local to prog modes to delete trailing whitespace. I
  ;; have whitespace mode configured to show problems, but it may be a
  ;; bit aggressive to clean all those things up automatically.

  ;; Don't treat snippet-mode as a progmode for this purpose. I want to leave trailing
  ;; spaces in some templates.
  (when (not (derived-mode-p 'snippet-mode))
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'make-it-local)))

;; Python
(add-to-mode-alist 'python-mode "scons" "SConstruct" "SConscript" "wscript")
(add-hook 'python-mode-hook 'guess-style-guess-all)

;; Ruby
;; .rb is set up by elpa
(add-to-mode-alist 'ruby-mode "\\.t$" "Rakefile$" "\\.rake$" "\\.rxml$" "\\.xrs$")

(defun ruby-load-line-x ()
  (interactive)
  (ruby-send-region (point-at-bol) (point-at-eol)))

(defun ruby-load-buffer-x ()
  (interactive)
  (ruby-send-region (point-min) (point-max)))

(eval-after-load 'inf-ruby
  '(define-keys inf-ruby-minor-mode-map
     ("\C-c\C-l" . 'ruby-load-line-x)
     ("\C-c\C-b" . 'ruby-load-buffer-x)))

;; Script
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
(add-to-list 'file-coding-system-alist '("\\.sh\\'" . utf-8-unix))

;; Text
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'my-text-mode-hook)
(add-hook 'text-mode-hook 'turn-on-ispell-keys)

(defun my-text-mode-hook ()
  (local-set-key "\M-k" 'kill-sentence))

(defun turn-on-ispell-keys ()
  (interactive)
	(local-set-key (kbd "H-i") 'ispell-word) ; ispell word; this is really C-i
  (local-set-key "\M-i" 'ispell))          ; ispell region or buffer

;; VC -- Bring back some of that old mercurial.el feeling to VC mode. I miss the
;; single step commit without having to set up a fileset but let's give this a whirl.
(eval-after-load 'vc-hooks
  '(define-keys vc-prefix-map
     ("=" . 'vc-root-diff)
     ("D" . 'vc-diff)
     ("n" . 'vc-next-action)))

;; YAML
(autoload 'yaml-mode "yaml-mode" "Edit YAML files" t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; XAML
(add-to-list 'auto-mode-alist '("\\.xaml$" . nxml-mode))
