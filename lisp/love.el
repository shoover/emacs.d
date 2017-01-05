(defvar love-exe "C:/Program Files/LOVE/love.exe")

(defun love-run ()
  "Save the current buffer file and run love, searching up the
file tree for a directory with main.lua."
  (interactive)
  (if (not (buffer-file-name))
      (error "Buffer '%s' is not visiting a file!" (buffer-name)))
  (if (buffer-modified-p)
      (save-buffer))
  (start-process "Love" nil love-exe
                 (expand-file-name
                  (file-name-directory
                   (locate-dominating-file (buffer-file-name) "main.lua")))))
