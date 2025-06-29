;; Text editing functions

;; from https://github.com/bbatsov/crux/blob/master/crux.el
(defmacro with-region-or-buffer-x (func)
  "When called with no active region, call FUNC on current buffer.
Use to make commands like `indent-region' work on both the region
and the entire buffer (in the absense of a region)."
  `(defadvice ,func (before with-region-or-buffer activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point-min) (point-max))))))

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

;; Two-stage move-beginning-of-line, once to end of indentation, twice to true BOL.
;; Parts from http://irreal.org/blog/?p=1946
(defadvice move-beginning-of-line (around smarter-bol activate)
  (if (/= arg 1)
      ;; Any time we're moving across lines, do the move and jump to indentation.
      (progn
        ad-do-it
        (back-to-indentation))
    ;; Staying on the same line. Move to indentation on first call,
    ;; then to actual BOL on second.
    (let ((pos (point)))
      (back-to-indentation)
      (when (= pos (point))
        ad-do-it))))

(defun init ()
  "Find my init file"
  (interactive)
  (find-file "~/emacs/init.el"))

(defun modes ()
  "Find my modes file"
  (interactive)
  (find-file "~/emacs/modes.el"))

(defun action ()
  "Find my org file"
  (interactive)
  (find-file my-action-org))

(defun work ()
  "Find my work org file"
  (interactive)
  (find-file my-work-org))

(defun notes ()
  "Find my notes org file"
  (interactive)
  (find-file my-notes-org))

(defun dev ()
  "Find my dev directory"
  (interactive)
  (let ((default-directory my-dev-dir))
    (ido-find-file)))

(defun banjo ()
  "Find my work org file"
  (interactive)
  (find-file (expand-file-name (concat org-directory "/music/banjo/banjo.org"))))

(defun indent-buffer-x ()
  "Indent the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun my-lisp-indent-line ()
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

(with-region-or-buffer-x indent-region)
(with-region-or-buffer-x fill-region)

(defun unfill-kill (beginning end)
  "Save an unfilled copy of the region to the clipboard."
  (interactive "r")
  (let ((buf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring buf beginning end)
      (unfill-buffer)
      (kill-ring-save (point-min) (point-max))))
  (message "Copied region to clipboard unfilled."))

(defun comment-sexp (&optional arg allow-extend)
  "Comment ARG sexps from point. If a region is active, that
region is commented instead."
  (interactive "P\np")
  (save-excursion
    (when (not mark-active)
      (mark-sexp arg allow-extend))
    (comment-region (region-beginning) (region-end))))

; https://emacs.stackexchange.com/a/12800/11751
(defun reverse-transpose-sexps (arg)
  (interactive "*p")
  (transpose-sexps (- arg))
  ;; when transpose-sexps can no longer transpose, it throws an error and code
  ;; below this line won't be executed. So, we don't have to worry about side
  ;; effects of backward-sexp and forward-sexp.
  ;; (backward-sexp (1+ arg))
  ;; (forward-sexp 1)
  )

(defun fill-buffer ()
  "Fill each of the paragraphs in the buffer."
  (interactive)
  (fill-region (point-min) (point-max)))

;;; From Stefan Monnier, adapted to unfill multiple paragraphs.
(defun unfill-paragraph (&optional arg)
  "Unfill paragraph at or after point. With a prefix arg, works
on that many paragraphs."
  (interactive "p")
  (or arg (setq arg 1))
  (while (and (> arg 0) (not (eobp)))
    (let ((fill-column (point-max)))
      (fill-paragraph nil))
    (forward-paragraph)
    (next-line)
    (decf arg)))

(defun unfill-buffer ()
  "Unfills all the paragraphs in the buffer."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-buffer)))

(defun unfill-region ()
  "Unfills all the paragraphs in the region, or in the buffer if
the mark is not active."
  (interactive)
  (let ((fill-column (point-max)))
    (if mark-active
        (fill-region (region-beginning) (region-end))
      (fill-region (point-min) (point-max)))))

(defun fill-subtree ()
  (interactive)
  (save-excursion
    (org-mark-subtree)
    (fill-region (region-beginning) (region-end))))

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

(defun fill-paragraph-forward ()
  "Like `fill-paragraph' but only marks forward from the current
line, not backward to get the whole paragraph. This provides a
quick way to preserve a preceding line break (but no blank line)
and fill the rest of the paragraph."
  (interactive)
  (beginning-of-line)
  (let ((beg (point)))
    (forward-paragraph)
    (fill-region beg (point))))

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

(defun slide-previous-line ()
  "Scroll down, keeping point at the same position relative to the frame."
  (interactive)
  (scroll-down-line)
  (previous-line))

(defun slide-next-line ()
  "Scroll up, keeping point at the same position relative to the frame."
  (interactive)
  (scroll-up-line)
  (next-line))

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

;; Adapted from https://www.emacswiki.org/emacs/SortWords
(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
Prefixed with negative \\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case
affects the sort order.

    See `sort-regexp-fields'."
      (interactive "*P\nr")
      (sort-regexp-fields reverse "[[:word:]-]+" "\\&" beg end))

;; adapted from http://blog.bookworm.at/2007/03/pretty-print-xml-with-emacs.html
(defun nxml-format-region (begin end)
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
              (nxml-format-region (point-min) (point-max))
              (setq buffer-undo-list nil)
              (set-buffer-modified-p nil))))


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

;; (Why didn't I just use the built in sql-sqlite?)
(defun sqlite (db)
  "Opens an interactive sqlite session in a comint buffer."
  (interactive
   (list (ido-read-file-name "DB: ")))
  (pop-to-buffer
   (make-comint "sqlite" "sqlite3" nil "-interactive" db)))

(defun eval-print ()
  (interactive)
  (let ((standard-output (current-buffer)))
    (terpri)
    (eval-last-sexp t)
    (terpri)))

(defun next-previous-buffer (arg)
  "next-buffer, or previous- with prefix arg"
  (interactive "P")
  (if arg
      (previous-buffer)
    (next-buffer)))

(defun move-buffer-other-frame ()
  "Moves the current buffer to another frame. If there's another
frame, it is used. Otherwise a new frame is created. This is
useful when you're in the wrong frame and Alt+Tab and C-xb keeps
you stuck there."
  (interactive)
  (let ((buf (current-buffer)))
    (bury-buffer)
    (let ((frame (next-frame (selected-frame))))
      (when (eq frame (selected-frame))
        (setq frame (make-frame)))
      (select-frame-set-input-focus frame)
      (switch-to-buffer buf))))

(defun my-ido-find-file (arg)
  "ido-find-file, but only use filename at point if prefix arg is non-nil."
  (interactive "P")
  (let ((ido-use-filename-at-point (if arg
                                       'guess
                                     nil)))
    (ido-find-file)))

(require 'switch-to-new-buffer) ; LISP
(defun my-switch-to-buffer ()
  "ido-switch-buffer plus installing hooks to offer to save new buffers on kill."
  (interactive)
  (let ((buf (ido-switch-buffer)))
    (with-current-buffer buf
                                        ; cheap newly created, fileless buffer detection
      (unless (or buffer-offer-save
                  (buffer-file-name)
                  (buffer-modified-p)
                  (> (buffer-size) 0))
        (switch-to-install-offer-save buf)))))

(defun my-ido-kill-buffer (arg)
  "ido-kill-buffer, but with a prefix arg just save the buffer
file name to the kill ring instead."
  (interactive "P")
  (if arg
      (copy-file-name-to-clipboard)
    (ido-kill-buffer)))

;; Originally from stevey, adapted to support moving to a new directory.
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive
   (progn
     (if (not (buffer-file-name))
         (error "Buffer '%s' is not visiting a file!" (buffer-name)))
     ;; Disable ido auto merge since it too frequently jumps back to the original
     ;; file name if you pause while typing. Reenable with C-z C-z in the prompt.
     (let ((ido-auto-merge-work-directories-length -1))
       (list (read-file-name (format "Rename %s to: " (file-name-nondirectory
                                                       (buffer-file-name))))))))
  (if (equal new-name "")
      (error "Aborted rename"))
  (setq new-name (if (file-directory-p new-name)
                     (expand-file-name (file-name-nondirectory
                                        (buffer-file-name))
                                       new-name)
                   (expand-file-name new-name)))
  ;; Only rename if the file was saved before. Update the
  ;; buffer name and visited file in all cases.
  (if (file-exists-p (buffer-file-name))
      (rename-file (buffer-file-name) new-name 1))
  (let ((was-modified (buffer-modified-p)))
    ;; This also renames the buffer, and works with uniquify
    (set-visited-file-name new-name)
    (if was-modified
        (save-buffer)
      ;; Clear buffer-modified flag caused by set-visited-file-name
      (set-buffer-modified-p nil)))

  (setq default-directory (file-name-directory new-name))

  (message "Renamed to %s." new-name))

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

(defun open-with (arg)
  "Open the buffer file in an external program or shell default.
Uses async-shell-command if a prefix arg is given."
  (interactive "P")
  (when buffer-file-name
    (let* ((open (cond
                  ((eq system-type 'darwin) "open")
                  (nix "xdg-open")
                  ((eq system-type 'windows-nt) "start") ; see also: built in w32-shell-execute
                  (t "")))
           (prompt (format "Open current file with (default %s):" open))
           (f (if arg 'async-shell-command 'shell-command)))
      (funcall f
               (concat (read-shell-command prompt nil nil open) " \""
                       buffer-file-name "\"")))))

;; http://arunrocks.com/emacs-tip-a-key-to-open-the-current-folder-in-windows/
(defun w32-explore-here ()
  "Launch the windows explorer in the current directory and selects current file"
  (interactive)
  (w32-shell-execute
   "open"
   "explorer"
   (concat "/e,/select," (convert-standard-filename buffer-file-name))))

(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Google: "))))))

;; http://emacs.stackexchange.com/a/12614
(defun capitalize-first-char-x (&optional string)
  "Capitalize only the first character of the input STRING."
  (when (and string (> (length string) 0))
    (let ((first-char (substring string 0 1))
          (rest-str   (substring string 1)))
      (concat (capitalize first-char) rest-str))))

(defun save-compile-project ()
  "Saves modified files under the project root (without asking)
and recompiles. The project root is determined using
`ffip-get-project-root-directory'."
  (interactive)

  ;; `compile' calls `save-some-buffers' implicitly. We configure it to save
  ;; without asking and give it a predicate to ignore files outside the
  ;; project directory. (Another way would be to set `buffer-save-without-query'
  ;; in all relevant source file mode hooks.)
  (let ((dir (expand-file-name (ffip-get-project-root-directory)))
        (compilation-ask-about-save nil)
        (compilation-save-buffers-predicate (lambda ()
                                              (string-prefix-p dir (buffer-file-name))))

        ;;
        (default-directory (capitalize-first-char-x (expand-file-name default-directory)))
        (compilation-directory default-directory)

        )
    (message "Saving and compiling in directory %s" dir)
    (message "default-directory: %s" default-directory)
    (recompile nil)))

(defun er-keyboard-quit ()
  "Smarter version of the built-in `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

Via https://emacsredux.com/blog/2025/06/01/let-s-make-keyboard-quit-smarter/."
  (interactive)
  (if (active-minibuffer-window)
      (if (minibufferp)
          (minibuffer-keyboard-quit)
        (abort-recursive-edit))
    (keyboard-quit)))
