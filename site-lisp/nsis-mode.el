(defvar nsis-mode-hook nil)
(defvar nsis-mode-map nil "Keymap for NSIS major mode")

(if nsis-mode-map nil
  (setq nsis-mode-map (make-keymap)))

(setq auto-mode-alist
	  (append
	   '(("\\.nsi\\'" . nsis-mode))
	   auto-mode-alist))

(defconst nsis-font-lock-keywords-1
  (list
   '("\\<\\(A\\(bort\\|dd\\(BrandingImage\\|Size\\)\\|llowRootDirInstall\\|utoCloseWindow\\)\\|B\\(GGradient\\|r\\(andingText\\|ingToFront\\)\\)\\|C\\(RCCheck\\|a\\(ll\\(InstDLL\\)?\\|ption\\)\\|h\\(angeUI\\|eckBitmap\\)\\|learErrors\\|o\\(mp\\(letedText\\|onentText\\)\\|pyFiles\\)\\|reate\\(Directory\\|Font\\|Shortcut\\)\\)\\|D\\(e\\(lete\\(INIS\\(ec\\|tr\\)\\|Reg\\(Key\\|Value\\)\\)?\\|tail\\(Print\\|sButtonText\\)\\)\\|ir\\(Show\\|Text\\)\\)\\|E\\(numReg\\(Key\\|Value\\)\\|x\\(ch\\|ec\\(Shell\\|Wait\\)?\\|pandEnvStrings\\)\\)\\|F\\(i\\(le\\(Close\\|ErrorText\\|Open\\|Read\\(Byte\\)?\\|Seek\\|Write\\(Byte\\)?\\)?\\|nd\\(Close\\|First\\|Next\\|Window\\)\\)\\|lushINI\\|unction\\(End\\)?\\)\\|G\\(et\\(CurrentAddress\\|D\\(LLVersion\\(Local\\)?\\|lgItem\\)\\|F\\(ileTime\\(Local\\)?\\|u\\(llPathName\\|nctionAddress\\)\\)\\|LabelAddress\\|TempFileName\\)\\|oto\\)\\|HideWindow\\|I\\(con\\|f\\(Errors\\|FileExists\\|RebootFlag\\)\\|n\\(itPluginsDir\\|st\\(ProgressFlags\\|Type\\|all\\(ButtonText\\|Colors\\|Dir\\(RegKey\\)?\\)\\)\\|t\\(CmpU?\\|Fmt\\|Op\\)\\)\\|sWindow\\)\\|L\\(angString\\(UP\\)?\\|icense\\(BkColor\\|Data\\|Text\\)\\|o\\(adLanguageFile\\|g\\(Set\\|Text\\)\\)\\)\\|M\\(essageBox\\|iscButtonText\\)\\|Name\\|OutFile\\|P\\(age\\|op\\|ush\\)\\|Quit\\|R\\(MDir\\|e\\(ad\\(EnvStr\\|INIStr\\|Reg\\(DWORD\\|Str\\)\\)\\|boot\\|gDLL\\|name\\|serveFile\\|turn\\)\\)\\|S\\(e\\(archPath\\|ction\\(End\\|Get\\(Flags\\|InstTypes\\|Text\\)\\|In\\|Set\\(Flags\\|InstTypes\\|Text\\)\\)?\\|ndMessage\\|t\\(AutoClose\\|BrandingImage\\|Compress\\(or\\)?\\|D\\(at\\(ablockOptimize\\|eSave\\)\\|etails\\(Print\\|View\\)\\)\\|Errors\\|F\\(ileAttributes\\|ont\\)\\|O\\(utPath\\|verwrite\\)\\|PluginUnload\\|RebootFlag\\|S\\(hellVarContext\\|taticBkColor\\)\\|WindowLong\\)\\)\\|how\\(InstDetails\\|UninstDetails\\|Window\\)\\|ilent\\(Install\\|Uninstall\\)\\|leep\\|paceTexts\\|tr\\(C\\(mp\\|py\\)\\|Len\\)\\|ub\\(Caption\\|Section\\(End\\)?\\)\\)\\|Un\\(RegDLL\\|inst\\(Page\\|all\\(ButtonText\\|Caption\\|Icon\\|SubCaption\\|Text\\)\\)\\)\\|W\\(indowIcon\\|rite\\(IniStr\\|Reg\\(Bin\\|DWORD\\|Str\\)\\|Uninstaller\\)\\)\\|XPStyle\\)\\>" . font-lock-keyword-face)) "Minimal highlighting expressions for NSIS mode")

(defconst nsis-font-lock-keywords-2
  (append nsis-font-lock-keywords-1
		  (list '("\\(;\\(.*\\|\\n\\)\\)" . font-lock-comment-face))) "Comment highlighting for NSIS mode")

(defvar nsis-font-lock-keywords nsis-font-lock-keywords-2 "Default highlighting expressions for the NSIS mode")

(defvar nsis-mode-syntax-table nil
  "Syntax table for nsis-mode.")

(defun nsis-create-syntax-table()
  (if nsis-mode-syntax-table
	  ()
	(setq nsis-mode-syntax-table (make-syntax-table))
	(modify-syntax-entry ?_ "w" nsis-mode-syntax-table)
	(modify-syntax-entry ?\; ". b" nsis-mode-syntax-table)
	(modify-syntax-entry ?\n "> b" nsis-mode-syntax-table))
  (set-syntax-table nsis-mode-syntax-table))

(defun nsis-mode ()
  "Major mode for editing NSIS Installer Scripts"
  (interactive)
  (kill-all-local-variables)
  (nsis-create-syntax-table)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
		'(nsis-font-lock-keywords))
  (setq major-mode 'nsis-mode)
  (setq mode-name "NSIS")
  (run-hooks 'nsis-mode-hook))

(provide 'nsis-mode)

;;; nsis-mode.el ends here