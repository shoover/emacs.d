;; I keep all my emacs-related stuff under ~/emacs. ~/.emacs should be pretty
;; thin. It contains machine-specific settings, but mainly it exists to load
;; this file. Something like this:
;;
;; (setq org-directory "~/Dropbox/action")
;; (setq custom-file "~/emacs/init.el")
;; (load custom-file)
;;
;; Run this occasionally: C-u 0 M-x byte-recompile-directory

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
  (add-path "lisp/auto-complete")
  (add-path "lisp/fsharp"))

(setq custom-theme-directory (concat emacs-root "themes"))

;; I install some info files here.
;; makeinfo blah.texi
;; install-info blah emacs/info/dir
(require 'info)
(setq Info-directory-list
      (add-to-list 'Info-default-directory-list
                   (expand-file-name "info" emacs-root)))

(when (eq system-type 'windows-nt)
  ;; Load emacsw32 here instead of site-start.el so it finds my org
  ;; installation.  You still have to remove it from site-start.el, though,
  ;; because that happens before this.
  (require 'emacsw32 nil t)

  ;; Put cygwin ahead for system32 for emacs and things it shells out to.
  ;; The gnuwin32 find.exe that comes with emacsw32 has a bug and doesn't
  ;; look for wildcards in the path you specify.
  (setenv "PATH" (concat "c:/bin" path-separator (getenv "PATH"))))

(defvar my-action-org (concat org-directory "/action.org"))
(defvar my-work-org (concat org-directory "/work.org"))

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
(fset 'yes-or-no-p 'y-or-n-p) ; Allow "y or n" instead of "yes or no"
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

(defun banjo ()
  "Find my work org file"
  (interactive)
  (find-file (concat org-directory "/../banjo/banjo.org")))

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

(defmacro loop-paragraphs (&rest body)
  `(save-excursion
     (goto-char (point-min))
     (while (not (eobp))
       ,@body
       (forward-paragraph))))

(defun fill-buffer ()
  (interactive)
  (loop-paragraphs
   (fill-paragraph)))

;;; From Stefan Monnier. It is the opposite of fill-paragraph. Takes a
;;; multi-line paragraph and makes it into a single line of text.
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-buffer ()
  (interactive)
  (loop-paragraphs
   (unfill-paragraph)))

(defun unfill-paragraph-html ()
  "Unfills the current paragraph and inserts HTML breaks at the end."
  (interactive)
  (unfill-paragraph)

  ;; Only insert breaks if we're not at the end of the buffer (last
  ;; paragraph). I couldn't get a standard looking-at/replace-match to work
  ;; without consuming the next character.
  (when (looking-at-p "\n[^\\`]")
    (insert "<br><br>")
    (forward-char)))

(defun my-htmlize-buffer ()
  "Like the function from edit-server.el but instead of wrapping
text in <pre>, this unfills paragraphs and inserts <br>."
  (interactive)
  (edit-server-htmlize-replace edit-server-htmlize-regexp
                               edit-server-htmlize-replacements)
  (loop-paragraphs
   (unfill-paragraph-html)))

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

;; I got this from somewhere, but I can't remember where
(defun count-words-region (beginning end)
  "Print number of words in the region.
Words are defined as at least one word-constituent
character followed by at least one character that
is not a word-constituent.  The buffer's syntax
table determines which characters these are."
  (interactive "r")
  (message "Counting words in region ... ")

  (save-excursion
    (goto-char beginning)
    (let ((count 0))
      (while (< (point) end)
        (re-search-forward "\\w+\\W*")
        (setq count (1+ count)))
      (cond ((zerop count)
             (message
              "The region does NOT have any words."))
            ((= 1 count)
             (message
              "The region has 1 word."))
            (t
             (message
              "The region has %d words." count))))))

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

(add-hook 'archive-extract-hooks
          (lambda ()
            (when (and (boundp 'archive-superior-buffer)
                       archive-superior-buffer
                       (eq 'nxml-mode major-mode))
              (my-nxml-format-region (point-min) (point-max))
              (setq buffer-undo-list nil)
              (set-buffer-modified-p nil))))


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

;; Fancy buffer and everything else switching
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-create-new-buffer 'always
      ido-file-extensions-order '(".org" t)
      ido-max-directory-size 100000 ; avoid "too big" errors on some dirs
      )
(ido-mode 1)

(defun my-ido-find-file (arg)
  "ido-find-file, but only use filename at point if prefix arg is non-nil."
  (interactive "P")
  (let ((ido-use-filename-at-point (if arg
                                       'guess
                                     nil)))
    (ido-find-file)))

;;; Custom keybindings
(global-set-key "\M-s" 'isearch-forward-regexp)
(global-set-key "\M-r" 'isearch-backward-regexp)

(global-set-key [f1] 'toggle-selective-display)

(global-set-key [C-tab] 'my-next-previous-buffer)
(when (eq system-type 'darwin)
  (global-set-key "\M-`" 'other-frame))

(global-set-key "\C-x\C-f" 'my-ido-find-file)
(global-set-key [f5] 'revert-buffer)
(global-set-key [f6] 'kill-this-buffer)

(global-set-key [C-up] 'move-text-up)
(global-set-key [C-down] 'move-text-down)

(global-set-key "\C-c/" 'my-indent-region) ; Indent region or whole buffer

;; Shift+(left|right|up|down) to get to a window quicker than with C-x o
(windmove-default-keybindings)

(global-set-key "\C-xx" 'w32shell-explorer-here)

(load "inc")
(global-set-key (kbd "C-c +") 'increment-integer-at-point)
(global-set-key (kbd "C-c -") 'decrement-integer-at-point)

;; Snippets
                                        ;(yas/load-directory (concat emacs-root "snippets"))

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

;; http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

;; emacsredux.com buffer file goodies
(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun open-with ()
  "Open the buffer file file in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      (read-shell-command "Open current file with: "))
                    " "
                    buffer-file-name))))

(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Google: "))))))

;; Basic
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(add-to-list 'auto-mode-alist '("\\.frm$"  . visual-basic-mode))
(add-to-list 'auto-mode-alist '("\\.bas$"  . visual-basic-mode))
(add-to-list 'auto-mode-alist '("\\.cls$"  . visual-basic-mode))
(add-to-list 'auto-mode-alist '("\\.vbs$"  . visual-basic-mode))

;; C
(add-hook 'c-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq c-basic-offset 4)
            (define-key c-mode-map "\C-c\C-c" 'compile)))
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
(defun my-org-todo-done ()
  (interactive)
  (org-todo 'done))

(defun my-org-promote-subtree (&optional n)
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
  (setq this-command 'my-org-promote-subtree))

(setq org-publish-project-alist
      `(("workorg"
         :base-directory ,org-directory
         :exclude ".org"
         :include ("apps.org")
         :publishing-directory "z:/users/shawn/html"
         :section-numbers t
         :table-of-contents t)
        ("workdocs"
         :base-directory ,org-directory
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
            (define-key org-mode-map "\C-\M-p" 'my-org-promote-subtree)
            (define-key org-mode-map "\C-cd" 'my-org-todo-done)

            ;; clear this so next- previous-buffer works
            (define-key org-mode-map [C-tab] nil)

            (setq org-agenda-files
                  (append
                   (list my-action-org
                         my-work-org)
                   (directory-files (concat org-directory "/../banjo") t "\\.org$")))
            (setq org-agenda-custom-commands
                  '(("A" "30 day agenda" agenda "" ((org-agenda-ndays 30)))))
            (setq org-refile-targets '((org-agenda-files :maxlevel . 1))
                  org-refile-use-outline-path 'file
                  org-refile-allow-creating-parent-nodes 'confirm)))
(setq org-mobile-inbox-for-pull (concat org-directory "/flagged.org")
      org-mobile-directory (concat org-directory "/../Apps/MobileOrg"))

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
(add-to-list 'auto-mode-alist '("wscript" . python-mode))

;; Ruby
;; .rb is set up by elpa
(add-to-list 'auto-mode-alist '("\\.t$"  . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$"  . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$"  . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rxml$"  . ruby-mode))

(defun my-ruby-load-line ()
  (interactive)
  (ruby-send-region (point-at-bol) (point-at-eol)))

(add-hook 'ruby-mode-hook
          (lambda ()
            ;;(define-key fsharp-mode-map "\C-c\C-b" 'my-fsharp-load-buffer)
            (define-key ruby-mode-map "\C-c\C-l" 'my-ruby-load-line)))

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

;; Chrome edit server
;; http://www.emacswiki.org/emacs/Edit_with_Emacs
(when (require 'edit-server nil t)
  (add-hook 'edit-server-start-hook 'my-edit-server-start-hook)
  (add-hook 'edit-server-done-hook 'my-edit-server-done-hook)
  (edit-server-start))

;; Use edit-server's HTML URL matching but my htmlize function.
;; Just unfill for regular non-HTML textareas.
(defun my-edit-server-done-hook ()
  (if (string-match edit-server-htmlize-url-regexp edit-server-url)
      (my-htmlize-buffer)
    (unfill-buffer)))

(defun my-edit-server-start-hook ()
  (edit-server-maybe-dehtmlize-buffer)
  (fill-buffer))

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
 '(custom-safe-themes (quote ("47372e349f9fee5ce5350c03358628f36ccfc25e7a4e73d1a0473511d295c2f8" default)))
 '(default-frame-alist (quote ((width . 95) (height . 55))))
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

(cd "~")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
