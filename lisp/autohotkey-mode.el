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
  '("catch" "class" "else" "finally" "for" "if" "in" "return" "try") ; keywords

  ; other highlighting
  `((,(regexp-opt '("::" ":=" ".=" "=")) . 'font-lock-keyword-face)
    ("[!^#~*+&][ !^#!*+&a-z]" . 'font-lock-builtin-face)     ; Ctrl/Alt/Shift/Win modifiers
    ("^[#][A-Z][A-Za-z]+" . 'font-lock-preprocessor-face)    ; #IfWinActive
    ("{[A-Z][0-9a-zA-Z_]*}" . 'font-lock-variable-name-face) ; keys, ex: {Delete}
    (,(regexp-opt
       '("ahk_class" "ahk_exe" "ahk_group"
         "Clipboard"
         "EndKey"
         "ErrorLevel"
         "Max"
         "NewInput"
         "Regex"
         "Tab"
         "Timeout")) . 'font-lock-builtin-face) ; builtin variables/constants
    (,(regexp-opt
       '("ClipWait"
         "GroupActivate" "GroupAdd"
         "IfInString"
         "IsFunc"
         "IfWinExist"
         "\\bInput\\b"
         "KeyWait"
         "ListVars"
         "MsgBox"
         "Pause"
         "Run"
         "Send"
         "SendMessage"
         "SendMode"
         "SetKeyDelay"
         "SetTimer"
         "SetTitleMatchMode"
         "Sleep"
         "StringLower" "StringReplace" "StringTrimRight" "StringUpper"
         "Suspend"
         "WinActive" "WinClose" "WinHide" "WinMinimize" "WinShow"))
     . 'font-lock-function-name-face)   ; builtin commands
    )

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
  (if (buffer-modified-p)
      (save-buffer))
  (start-process "AutoHotkey" nil autohotkey-exe "/restart" (buffer-file-name))
  (message "Reloaded script %s" (buffer-file-name)))

(provide 'autohotkey-mode)
