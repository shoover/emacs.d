;; Generic major mode for editing AutoHotkey scripts.
;;
;; Features:
;; - associates with .ahk files
;; - highlights a few commands
;; - provides a function to reload a script
;;
;; Setup:
;;   (autoload 'autohotkey-mode "autohotkey-mode" "Edit AutoHotkey scripts" t)
;;   (add-to-list 'auto-mode-alist '("\\.ahk$" . autohotkey-mode))
;;   ; optionally override the exe
;;   (setq autohotkey-exe "c:/bin/autohotkey.exe")

(require 'generic-x)

(define-generic-mode
    'autohotkey-mode
  '(";")
  '("if" "else" "return")                        ; keywords
  `(("::" . 'font-lock-keyword-face)
    ("[!^#~*+][!^#!*+a-z]" . 'font-lock-builtin-face) ; Ctrl/Alt/Shift/Win modifiers
    (,(regexp-opt
       '("EndKey"
         "ErrorLevel"
         "Max"
         "NewInput"
         "Timeout"
         "UserInput"))
     . 'font-lock-builtin-face)                        ; builtin variables/constants
    (,(regexp-opt
       '("IfInString"
         "Input"
         "KeyWait"
         "Run"
         "Send"
         "SendMode"
         "SetKeyDelay"
         "Sleep"))
     . 'font-lock-function-name-face)                      ; builtin commands
    ("^[#][A-Z][A-Za-z]+" . 'font-lock-preprocessor-face)  ; #IfWinActive
    ("{[A-Z][a-zA-Z_]*}" . 'font-lock-variable-name-face)) ; keys, ex: {Delete}
  '("\\.ahk$")
  '(list
    (lambda ()
      (use-local-map autohotkey-mode-map)))
  "Major mode for editing AutoHotkey scripts.")

(defvar autohotkey-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-r" 'autohotkey-reload)
    map))

(defvar autohotkey-exe "c:/program files/autohotkey/autohotkey.exe")

(defun autohotkey-reload ()
  "Reloads the current buffer file AutoHotkey script."
  (interactive)
  (if (not (buffer-file-name))
      (error "Buffer '%s' is not visiting a file!" (buffer-name)))
  (save-buffer)
  (start-process "AutoHotkey" nil autohotkey-exe "/restart" (buffer-file-name)))

(provide 'autohotkey-mode)
