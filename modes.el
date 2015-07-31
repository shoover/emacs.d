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
(add-hook 'c-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq c-basic-offset 4)
            (c-set-offset 'substatement-open 0)
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
(add-to-mode-alist 'cmake-mode "CMakeLists\\.txt$" "\\.cmake$")

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
  ("\M-k" . 'kill-sexp))

;; Log files
(autoload 'log-mode "log-mode" "View log files" t)
(add-to-list 'auto-mode-alist '("\\.log$" . log-mode))

;; Make
(setq compile-command "make ")

;; NSIS
(autoload 'nsis-mode "nsis-mode" "Edit Nullsoft installer scripts" t)
(add-to-mode-alist 'nsis-mode "\\.nsh$" "\\.nsi.tmpl$")

;; org-mode
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

(defun convert-org-items-to-paragraphs ()
  "Convert items to normal paragraphs. If there is no active
region, only the current line is considered.

The overall structure and helper functions are cribbed from
`org-toggle-item', which can convert items to plain lines but
doesn't unindent multiline item text."
  (interactive)
  (let ((shift-text
         (function
          ;; Shift text in current section to IND, from point to END.
          ;; The function leaves point to END line.
          (lambda (ind end)
            (let ((min-i 1000) (end (copy-marker end)))
              ;; First determine the minimum indentation (MIN-I) of
              ;; the text.
              (save-excursion
                (catch 'exit
                  (while (< (point) end)
                    (let ((i (org-get-indentation)))
                      (cond
                       ;; Skip blank lines and inline tasks.
                       ((looking-at "^[ \t]*$"))
                       ((looking-at org-outline-regexp-bol))
                       ;; We can't find less than 0 indentation.
                       ((zerop i) (throw 'exit (setq min-i 0)))
                       ((< i min-i) (setq min-i i))))
                    (forward-line))))
              ;; Then indent each line so that a line indented to
              ;; MIN-I becomes indented to IND.  Ignore blank lines
              ;; and inline tasks in the process.
              (let ((delta (- ind min-i)))
                (while (< (point) end)
                  (unless (or (looking-at "^[ \t]*$")
                              (looking-at org-outline-regexp-bol))
                    (org-indent-line-to (+ (org-get-indentation) delta)))
                  (forward-line)))))))
        (skip-blanks
         (function
          ;; Return beginning of first non-blank line, starting from
          ;; line at POS.
          (lambda (pos)
            (save-excursion
              (goto-char pos)
              (skip-chars-forward " \r\t\n")
              (point-at-bol)))))
        beg end)

    ;; Determine boundaries of changes.
    (if (org-region-active-p)
        (setq beg (funcall skip-blanks (region-beginning))
              end (copy-marker (region-end)))
      (setq beg (funcall skip-blanks (point-at-bol))
            end (copy-marker (point-at-eol))))

    ;; Depending on the starting line, choose an action on the text
    ;; between BEG and END.
    (org-with-limited-levels
     (save-excursion
       (goto-char beg)
       (let ((ref-ind (org-get-indentation))
             (first-item t))
         (while (< (point) end)
           (cond
            ;; Start at an item: de-itemize and insert blank lines between them.
            ((org-at-item-p)
             (progn
               ;; Delete the item prefix.
               (save-excursion
                 (skip-chars-forward " \t")
                 (delete-region (point) (match-end 0)))

               ;; Insert the blank line after deleting the item prefix;
               ;; otherwise the prefix search gets messed up by a character or
               ;; two.
               (if first-item
                   (setq first-item nil)
                 (newline))))

            ;; Unindent lines of enclosed paragraph.
            (t
             (save-excursion
               (funcall shift-text
                        ref-ind
                        (min end (save-excursion
                                   (move-end-of-line nil)
                                   (point)))))))

           (forward-line)))))))

(require 'browse-url)
(defun org-export-html-subtree-x (arg)
  "Exports the current subtree to HTML and browses to the file.

With a prefix arg, prompt for a file name. If property
\"EXPORT_FILE_NAME\" is set in the subtree, that file name is
used. Otherwise a temp file is used."
  (interactive "P")
  (let ((file (or
               ;; Get the output file name from the user if a prefix arg was given
               (and arg
                    (ido-read-file-name "HTML file:"))

               ;; Use the subtree export file name if the property is set
               (org-entry-get
                (save-excursion
                  (ignore-errors (org-back-to-heading) (point)))
                "EXPORT_FILE_NAME" t)

               ;; Otherwise use a temp file, logic cribbed from browse-url-of-buffer
               (convert-standard-filename
                (make-temp-file
                 (expand-file-name "burl" browse-url-temp-dir)
                 nil ".html"))))
        (async nil)
        (subtreep t)
        (visible-only t))
    (browse-url-of-file
     (org-export-to-file 'html file async subtreep visible-only))))

(defun find-org-files (&optional regexp)
  "Returns a list of files in `org-directories' (searched
recursively) whose names match REGEXP. The search pattern
defaults to .org. You can override, for example, to also search
archives."
  (let ((regexp (or regexp "\\.org$")))
    (apply 'append
           (mapcar (lambda (dir)
                     (directory-files dir t regexp))
                   org-directories))))

(add-hook 'org-mode-hook
          (lambda ()
            (turn-on-auto-fill)))

(add-hook 'org-load-hook
          (lambda ()
            (define-keys org-mode-map
              ("\C-ca" . 'org-agenda)
              ("\C-cl" . 'org-store-link)
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

            (setq org-directories (list org-directory
                                        (concat org-directory "/../banjo")))
            (setq org-agenda-files (find-org-files))
            (setq org-agenda-custom-commands
                  '(("A" "Multi-occur, agenda files and archives"
                     search ""
                     ((org-agenda-files (find-org-files "\\.org\\|org_archive$"))))
                    ("P" "Project list"
                     tags "prj"
                     ((org-use-tag-inheritance nil)))
                    ("p" "Project list, current buffer"
                     tags-tree "prj"
                     ((org-use-tag-inheritance nil)))))
            (setq org-refile-targets '((org-agenda-files :maxlevel . 2))
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
            (when (and (frame-live-p (selected-frame))
                       (capture-frame-p))
              (delete-frame))))

(defun make-capture-frame ()
  "Makes a frame with dialog-like properties and the name
\"*Capture*\" for tracking in after-finalize."
  (require 'frame-center)
  (let ((f (make-frame `((name . "*Capture*")
                         (width . 95) (height . 25)))))
    (frame-bottom-right f)
    (select-frame f)
    (raise-frame f)
    f))

(defmacro define-keys (map &rest pairs)
  "Define multiple keys at once in keymap MAP. PAIRS are associations, for example
(\"C-l\" . 'load)."
  (let ((forms (mapcar (lambda (pair) `(define-key ,map ,(car pair) ,(cdr pair))) pairs)))
    `(progn ,@forms)))

(defmacro with-capture-frame (&rest body)
  "Opens a new capture frame and invokes BODY. This also hacks
org-mode variables to make sure org capture template selection
doesn't divide the frame into a new window; since we're already
making a dedicated frame, we want one full window in the frame."
  `(progn
     (let ((capture-frame (make-capture-frame)))
       ;; Capture template selection uses org-mks, which insists on using a
       ;; separate window to pick the template. This looks weird when we
       ;; already are making a dedicated frame, so hack it to use the same
       ;; window.
       (letf (((symbol-function 'org-switch-to-buffer-other-window)
               (symbol-function 'switch-to-buffer)))
         ,@body))))

(defun my-org-capture-new-frame ()
  "Create a new frame and run org capture, good for slickrun
commands, desktop shortcuts, scripts, etc. For example: \"emacsclientw
-e (my-org-capture-new-frame)\". This ensures that template
selection is presented in the new frame; using org-capture-hook
instead would result in the template selection prompt being
buried in the existing frame."
  (interactive)
  (with-capture-frame
   (org-capture)))

(require 'org-protocol)

;; Advise org-protocol-capture to always wrap in a new capture frame. This
;; would be risky with, say, org-capture, but since protocol capture is never
;; called within emacs interactively but only from other programs, I think
;; it's ok. If it became a problem we would have to register a new protocol
;; and handler just for wrapping.
(advice-add 'org-protocol-capture :around #'my-org-protocol-advice)
(defun my-org-protocol-advice (orig-fun &rest args)
  (with-capture-frame
   (apply orig-fun args)))

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

;; If this stops inserting at the right level, check org-capture-place-entry for
;; http://orgmode.org/w/?p=org-mode.git;a=commitdiff;h=f1583aab467fff999f25eff6a03e771d11139a93
;; Somehow that patch didn't make it into 8.2.10 even though it's ancient. Hopefully
;; someday.
(defun org-datetree-find-date-create-month (&optional date)
  "Find or create a datetree entry for DATE's year and month. DATE defaults to today."
  (require 'org-datetree)
  (let* ((date (or date (calendar-gregorian-from-absolute (org-today))))
         (year (nth 2 date))
         (month (car date))
         (day (nth 1 date)))
    (org-set-local 'org-datetree-base-level 1) ; needed to get the buffer to widen right
    (widen)
    (save-restriction
      (goto-char (point-min))
      (org-datetree-find-year-create year)
      (org-datetree-find-month-create year month)
      (goto-char (prog1 (point) (widen))))))

;; minutes parsing for workout duration math formulas. Stock table T/t
;; formatting defaults to HH:MM but we need MM:SS.
(defun parse-minutes-x (time)
  "Converts TIME in MM:SS format to a number of minutes."
  (when (string-match "\\([0-9]+\\)\\:\\([0-9]+\\)" time)
    (+ (string-to-number (match-string 1 time))
       (/ (string-to-number (match-string 2 time)) 60.0))))

(setq org-capture-templates
      ;; standard capture: blank headline, paste region
      '(("a" "Action"
         entry (file org-default-notes-file)
         "* %?\n%i"
         :prepend t)

        ("n" "Notes"
         entry (file+datetree my-notes-org) ; include timestamp
         "* %?\n%i%U")

        ("w" "Work"
         entry (file my-work-org)
         "* %?\n%i"
         :prepend t)

        ("o" "Workout Table"
         table-line (file+function (concat org-directory "/workout.org")
                                   org-datetree-find-date-create-month)
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
         "* %?%c\n%i\n%U" :prepend t)
        ;; Notes datetree requires a separate template
        ("N" "org-protocol Notes capture"
         entry (file+datetree my-notes-org)
         "* %?%c\n%i\n%U")))

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

;; Python
(add-to-mode-alist 'python-mode "scons" "SConstruct" "SConscript" "wscript")

;; Ruby
;; .rb is set up by elpa
(add-to-mode-alist 'ruby-mode "\\.t$" "Rakefile$" "\\.rake$" "\\.rxml$" "\\.xrs$")

(defun ruby-load-line ()
  (interactive)
  (ruby-send-region (point-at-bol) (point-at-eol)))

(eval-after-load 'inf-ruby
  '(define-key inf-ruby-minor-mode-map "\C-c\C-l" 'ruby-load-line))

;; Text
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-ispell-keys) 

(defun turn-on-ispell-keys ()
  (interactive)
	(local-set-key (kbd "H-i") 'ispell-word) ; word; this is really C-i
  (local-set-key "\M-i" 'ispell))          ; region or buffer

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
