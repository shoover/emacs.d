;; Generic major mode for editing AutoHotkey scripts.
;;
;; Features:
;; - associates with .ahk files
;; - highlights a few commands and bits of syntax
;; - provides a function to reload a script
;;
;; Setup:
;;   (autoload 'autohotkey-mode "autohotkey-mode" "Edit AutoHotkey scripts" t)
;;   (add-to-list 'auto-mode-alist '("\\.ahk$" . autohotkey-mode))
;;   ; optionally override the exe
;;   (setq autohotkey-exe "c:/bin/autohotkey.exe")

(require 'generic-x)

(define-generic-mode 'autohotkey-mode
  '(";" ("/*" . "*/"))            ; comment char
  '("if" "class" "else" "return") ; keywords

  ; other highlighting
  `((,(regexp-opt '("::" ":=" ".=" "=")) . 'font-lock-keyword-face)
    ("[!^#~*+&][ !^#!*+&a-z]" . 'font-lock-builtin-face) ; Ctrl/Alt/Shift/Win modifiers
    ("^[#][A-Z][A-Za-z]+" . 'font-lock-preprocessor-face)  ; #IfWinActive
    (,(regexp-opt
       '("EndKey"
         "ErrorLevel"
         "Max"
         "NewInput"
         "Regex"
         "Tab"
         "Timeout")) . 'font-lock-builtin-face)          ; builtin variables/constants
    (,(regexp-opt
       '("GroupAdd"
         "IfInString"
         "IsFunc"
         "\\bInput\\b"
         "KeyWait"
         "ListVars"
         "Pause"
         "Run"
         "Send"
         "SendMode"
         "SetKeyDelay"
         "SetTitleMatchMode"
         "Sleep"
         "StringTrimRight"
         "WinActive"
         "WinClose"
         "WinMinimize")) . 'font-lock-function-name-face)  ; builtin commands
    ("{[A-Z][a-zA-Z_]*}" . 'font-lock-variable-name-face)) ; keys, ex: {Delete}

  '("\\.ahk$") ; file assocation
  '(list       ; mode setup
    (lambda ()
      (use-local-map autohotkey-mode-map)))
  "Major mode for editing AutoHotkey scripts.")

(defvar autohotkey-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-r" 'autohotkey-reload)
    (define-key map "\C-c\C-c" 'autohotkey-reload)
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
