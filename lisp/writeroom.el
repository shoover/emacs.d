(defun writeroom ()
  "Switches to a WriteRoom-like fullscreen style"
  (interactive)	
  (when (featurep 'aquamacs)
    ;;(set-frame-font "American Typewriter-18")
    (color-theme-initialize)
    (color-theme-clarity)
    (set-fringe-mode 0)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (hide-mode-line)
    (aquamacs-toggle-full-frame)))

(defun exit-writeroom ()
  (interactive)
  (show-mode-line)
  (aquamacs-toggle-full-frame))

(defun hide-mode-line ()
  (interactive)
  (setq hide-mode-line-saved-mode-line-format 
        (list mode-line-format))
  (setq mode-line-format nil))

(defun show-mode-line ()
  (interactive)
  (setq mode-line-format hide-mode-line-saved-mode-line-format))
