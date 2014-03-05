;; Generic major mode for editing AutoHotkey scripts.
;;
;; Features:
;; - associates with .ahk files
;; - highlights a few commands
;; - provides a function to reload a script
;;
;; Setup:
;; (autoload 'autohotkey-mode "autohotkey-mode" "Edit AutoHotkey scripts" t)
;; (add-to-list 'auto-mode-alist '("\\.ahk$" . autohotkey-mode))

(require 'generic-x)

(define-generic-mode
    'autohotkey-mode
  '(";")
  '("return" "Run" "Send" "SendMode" "Sleep")
  '(("::" . 'font-lock-builtin-face)
    ("[#^!]" . 'font-lock-builtin-face))
  '("\\.ahk$")
  '(list
    (lambda ()
      (use-local-map autohotkey-mode-map)))
  "Major mode for editing AutoHotkey scripts.")

(defvar autohotkey-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-r" 'autohotkey-reload)
    map))

(defvar authotkey-exe "c:/program files/autohotkey/autohotkey.exe")

(defun autohotkey-reload ()
  "Reloads the current buffer file AutoHotkey script."
  (interactive)
  (if (not (buffer-file-name))
      (error "Buffer '%s' is not visiting a file!" (buffer-name)))
  (save-buffer)
  (start-process "AutoHotkey" nil autohotkey-exe "/restart" (buffer-file-name)))

(provide 'autohotkey-mode)
