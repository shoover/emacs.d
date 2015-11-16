(defun org-promote-subtree-x (&optional n)
  "Cut the current subtree and paste it one heading level up.
With prefix arg N, cut this many sequential subtrees."
  (interactive)
  (save-excursion
    (org-cut-subtree n)
    (outline-up-heading 1)
    (org-paste-subtree))

  ;; Workaround so repeated calls don't append to the previous promoted text
  ;; and paste duplicate info. org-cut-subtree leaves last-command set to
  ;; kill-region but org-paste-subtree doesn't set it.
  (setq this-command 'org-promote-subtree-x))

(defun org-convert-items-to-paragraphs-x ()
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

(defun org-publish-dir-x (dir &optional target project-name)
  "Publishes all the .org files in DIR to the TARGET directory
using the org HTML publisher."
  (interactive
   (let* ((dir (ido-read-directory-name "Source dir: "))
          (target (ido-read-directory-name "HTML dir: "
                                           (concat dir "/html")))
          ;; strip trailing slash and get the last path part
          (project-name (file-name-nondirectory (directory-file-name dir)))
          (project-name (read-string (format "Project name [%s]: " project-name)
                                     nil nil project-name)))
     (list dir target project-name)))

  (unless (file-exists-p dir)
    (error "Org dir %s does not exist" dir))
  
  (let* ((dir-exp (expand-file-name dir))
         (target (or target (concat dir-exp "/html")))
         (project-name (or project-name (file-name-nondirectory dir)))
         (org-publish-project-alist `((,project-name
                                       :base-directory ,dir-exp
                                       :publishing-directory ,target
                                       :publishing-function org-html-publish-to-html
                                       :auto-sitemap nil ; quieter logs
                                       ))))
    (message "Publishing org dir: %s" dir-exp)
    (org-publish-project project-name
                         t ; set to force publishing files the org cache thinks are ok
                         ;;  ; even if they were deleted :-/
                         )))


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
