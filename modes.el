;;; Modes setup and mode-specific functions

(defmacro define-keys (map &rest pairs)
  "Define multiple keys at once in keymap MAP. PAIRS are associations, for example
(\"C-l\" . 'load)."
  (let ((forms (mapcar (lambda (pair) `(define-key ,map ,(car pair) ,(cdr pair))) pairs)))
    `(progn ,@forms)))

;; AutoHotkey
(autoload 'autohotkey-mode "autohotkey-mode" "Edit AutoHotkey scripts" t)
(add-to-list 'auto-mode-alist '("\\.ahk$" . autohotkey-mode))

;; Basic
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(add-to-list 'auto-mode-alist '("\\.frm$" . visual-basic-mode))
(add-to-list 'auto-mode-alist '("\\.bas$" . visual-basic-mode))
(add-to-list 'auto-mode-alist '("\\.cls$" . visual-basic-mode))
(add-to-list 'auto-mode-alist '("\\.vbs$" . visual-basic-mode))

;; C
(add-hook 'c-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq c-basic-offset 4)
            (define-key c-mode-map "\C-c\C-c" 'compile)))
(add-to-list 'auto-mode-alist '("\\.rl$" . c-mode))

;; C#
(add-hook 'csharp-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq c-basic-offset 4)
            (c-set-style "c#")))

;; Clojure
(add-hook 'clojure-mode-hook 'lisp-enable-paredit-hook)
(add-hook 'clojure-mode-hook
          (lambda ()
            (define-key clojure-mode-map "\C-c\C-l" 'lisp-load-buffer)
            (set (make-local-variable 'indent-line-function)
                 'indent-line)
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
(defun lisp-load-buffer ()
  (interactive)
  (point-to-register 5)
  (mark-whole-buffer)
  (lisp-eval-region (point) (mark) nil)
  (jump-to-register 5))

(defun slime-clojure-jar (clojure-jar)
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

;; F#
(defvar inferior-fsharp-program "\"c:\\Program Files (x86)\\Microsoft F#\\v4.0\\Fsi.exe\"")
(defvar fsharp-compiler "\"c:\\Program Files (x86)\\Microsoft F#\\v4.0\\Fsc.exe\"")
(add-hook 'fsharp-mode-hook
          (lambda ()
            (define-keys fsharp-mode-map
              ("\C-c\C-b" . 'fsharp-load-buffer)
              ("\C-c\C-l" . 'fsharp-load-line)
              ("\C-c\C-u" . 'fsharp-goto-block-up)
              ("\C-x\C-e" . 'fsharp-eval-phrase))))
(add-hook 'inferior-fsharp-mode-hooks
          (lambda ()
            (add-to-list 'comint-output-filter-functions
                         'comint-truncate-buffer)
            (setq comint-buffer-maximum-size 2000)))

(defun fsharp-load-buffer ()
  (interactive)
  (fsharp-eval-region (point-min) (point-max)))

(defun fsharp-load-line ()
  (interactive)
  (fsharp-eval-region (point-at-bol) (point-at-eol)))

;; Lisp
(require 'lisp-mode)
(define-key lisp-mode-shared-map "\M-a" 'backward-sexp)
(define-key lisp-mode-shared-map "\M-e" 'forward-sexp)

;; Make
(setq compile-command "make ")

;; Mercurial
(require 'mercurial)

;; NSIS
(autoload 'nsis-mode "nsis-mode" "Edit Nullsoft installer scripts" t)
(add-to-list 'auto-mode-alist '("\\.nsi.tmpl$"  . nsis-mode))
(add-to-list 'auto-mode-alist '("\\.nsh$"  . nsis-mode))

;; org-mode
(defun org-agenda-match-subtree (&optional arg)
  "Org Agenda tag/todo match restricted to the current subtree."
  (interactive "P")
  (org-agenda arg "m" 'subtree))

(defun org-agenda-restrict-subtree (&optional arg)
  "Dispatch agenda commands restricted to current subtree."
  (interactive "P")
  (let ((org-agenda-overriding-restriction 'subtree))
    (org-agenda arg)))

(defun org-promote-subtree-x (&optional n)
  "Cut the current subtree and paste it one heading level up.
With prefix arg N, cut this many sequential subtrees."
  (interactive)
  (save-excursion
    (org-cut-subtree n)
    (outline-up-heading 1)
    (org-paste-subtree))

  ;; Work around so repeated calls don't append to the previous promoted text
  ;; and paste duplicate info. org-cut-subtree leaves last-command set to
  ;; kill-region but org-paste-subtree doesn't set it.
  (setq this-command 'org-promote-subtree-x))

(add-hook 'org-mode-hook
          (lambda ()
            (turn-on-auto-fill)))
(add-hook 'org-load-hook
          (lambda ()
            (define-keys org-mode-map
              ("\C-ca" . 'org-agenda)
              ("\C-cm" . 'org-agenda-match-subtree)
              ("\C-cs" . 'org-agenda-restrict-subtree)
              ("\C-cl" . 'org-store-link)
              ("\C-cb" . 'org-iswitchb)
              ("\C-cw" . 'copy-org-link-at-point)

              ;; Make links work like chasing definitions in source code.
              ("\M-." . 'org-open-at-point)
              ("\M-," . 'org-mark-ring-goto)

              ("\C-\M-a" . 'org-archive-subtree)
              ("\C-\M-p" . 'org-promote-subtree-x)

              ;; Compatibility with my isearch keys
              ("\C-c\C-x\C-r" . 'org-paste-special)

              ;; clear this so next- previous-buffer works
              ([C-tab] . nil))

            (setq org-agenda-files
                  (append
                   (directory-files org-directory t "\\.org$")
                   (directory-files (concat org-directory "/../banjo") t "\\.org$")))
            (setq org-agenda-custom-commands
                  '(("P" "Project list" tags "prj"
                     ((org-use-tag-inheritance nil)))
                    ("p" "Project list, current buffer" tags-tree "prj"
                     ((org-use-tag-inheritance nil)))))
            (setq org-refile-targets '((org-agenda-files :maxlevel . 1))
                  org-refile-use-outline-path 'file
                  org-refile-allow-creating-parent-nodes 'confirm)))
(setq org-default-notes-file (concat org-directory "/action.org"))
(setq org-tags-column -85)

;; org-capture frames, adapted from Lau's remember frames:
;; http://github.com/LauJensen/Configs/blob/master/emacs
(defun capture-frame-p ()
  (equal "*Capture*" (frame-parameter nil 'name)))

;; Automatic closing of capture frames
(add-hook 'org-capture-after-finalize-hook
          (lambda ()
            (when (capture-frame-p)
              (delete-frame))))

(defun make-capture-frame ()
  "Create a new frame and run org capture."
  (interactive)
  (require 'frame-center)
  (let ((f (make-frame '((name . "*Capture*") (width . 95) (height . 20)))))
    (frame-center f)
    (select-frame f)
    (raise-frame f))

  ;; Capture template selection uses org-mks, which insists on using a
  ;; separate window to pick the template. This looks weird when we already
  ;; are making a separate frame, so hack it to use the same window.
  (letf (((symbol-function 'org-switch-to-buffer-other-window)
          (symbol-function 'switch-to-buffer)))
    (org-capture)))

;; Patch org-get-x-clipboard to work on Windows:
;; http://lists.gnu.org/archive/html/emacs-orgmode/2013-11/msg00675.html
(defun org-get-x-clipboard (value)
  "Get the value of the x or Windows clipboard, compatible with XEmacs, and GNU Emacs 21."
  (cond ((eq window-system 'x)
         (let ((x (org-get-x-clipboard-compat value)))
           (if x (org-no-properties x))))
        ((and (eq window-system 'w32) (fboundp 'w32-get-clipboard-data))
         (w32-get-clipboard-data))))

(defun copy-org-link-at-point ()
  (interactive)
  (when (org-in-regexp org-bracket-link-regexp 1)
    (let ((link (org-link-unescape (org-match-string-no-properties 1))))
      (kill-new link)
      (message "Copied link '%s' to the clipboard." link))))

(defun read-org-agenda-file ()
  "Completing-read for an org agenda file."
  (let ((org-completion-use-ido t))
    (org-icompleting-read "Org buffer: "
                          (mapcar 'list (mapcar 'buffer-name (org-buffer-list 'agenda)))
                          nil t)))

(setq org-capture-templates
        ; standard capture: blank headline, paste region
      '(("a" "Action" entry (file org-default-notes-file)
         "* %?\n%i" :prepend t)
        ("n" "Notes" entry (file+datetree my-notes-org) ; include timestamp
         "* %?\n%i%U")
        ("w" "Work" entry (file my-work-org)
         "* %?\n%i" :prepend t)

        ; clipboard capture: blank headline, paste OS clipboard
        ("v" "Templates for pasting the OS clipboard")
        ("va" "Action, paste clipboard" entry (file org-default-notes-file)
         "* %?\n%x" :prepend t)
        ("vw" "Work, paste clipboard" entry (file my-work-org)
         "* %?\n%x" :prepend t)
        ("vn" "Notes, paste clipboard" entry (file+datetree my-notes-org)
         "* %?\n%x" :empty-lines 1)

        ; org-protocol capture: the handler puts the link/title in the kill ring %c
        ; and selected text in the region %i
        ; Alt: "* %?[[%:link][%:description]]\n%:initial\n%U"
        ("c" "org-protocol capture" entry (file read-org-agenda-file)
         "* %?%c\n%i\n%U" :prepend t)))

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
     (define-keys paredit-mode-map
       ((kbd "<delete>") . 'paredit-forward-maybe-delete-region)
       ((kbd "DEL") . 'paredit-backward-maybe-delete-region)
       ((kbd ";") . 'self-insert-command)

       ("\M-s" . nil) ; override splice
       ("\M-S" . nil))
     ))

;; PHP
(add-to-list 'auto-mode-alist '("\\.php$" . html-mode))

;; Python
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript" . python-mode))
(add-to-list 'auto-mode-alist '("wscript" . python-mode))

;; Ruby
;; .rb is set up by elpa
(add-to-list 'auto-mode-alist '("\\.t$"  . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$"  . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$"  . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rxml$"  . ruby-mode))

(defun ruby-load-line ()
  (interactive)
  (ruby-send-region (point-at-bol) (point-at-eol)))

(add-hook 'ruby-mode-hook
          (lambda ()
            (define-key inf-ruby-minor-mode-map "\C-c\C-l" 'ruby-load-line)))

;; Text
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; YAML
(autoload 'yaml-mode "yaml-mode" "Edit YAML files" t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; XAML
(add-to-list 'auto-mode-alist '("\\.xaml$" . nxml-mode))
