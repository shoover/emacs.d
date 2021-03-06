(deftheme Shawn
  "Created 2013-08-21.")

(defvar custom-theme-shawn-default
  '((t (:inherit nil :stipple nil :background "gray12" :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "outline" :family "Monaco"))))

(custom-theme-set-faces
 'Shawn
 '(cursor ((t (:foreground "#DCDCCC" :background "white"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((t (:family "Sans Serif"))))
 '(escape-glyph ((t (:weight bold :foreground "#F0DFAF"))))
 '(minibuffer-prompt ((t (:foreground "#F0DFAF"))))
 '(highlight ((t (:background "#343434"))))
 '(region ((t (:background "#484848"))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(secondary-selection ((t (:background "#5F5F5F"))))
 '(trailing-whitespace ((t (:background "#CC9393"))))
 '(font-lock-builtin-face ((t (:weight bold :foreground "#DCDCCC"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#5F7F5F" :inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:foreground "#7F9F7F"))))
 '(font-lock-constant-face ((t (:foreground "#BFEBBF"))))
 '(font-lock-doc-face ((t (:foreground "#9FC59F" :inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:foreground "#93E0E3"))))
 '(font-lock-keyword-face ((t (:weight bold :foreground "#F0DFAF"))))
 '(font-lock-negation-char-face ((t (:weight bold :foreground "#F0DFAF"))))
 '(font-lock-preprocessor-face ((t (:foreground "#94BFF3" :inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:weight bold :foreground "#7F9F7F" :inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:weight bold :foreground "#F0DFAF" :inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#CC9393"))))
 '(font-lock-type-face ((t (:foreground "#7CB8BB"))))
 '(font-lock-variable-name-face ((t (:foreground "#DFAF8F"))))
 '(font-lock-warning-face ((t (:weight bold :foreground "#D0BF8F" :inherit (error)))))
 '(button ((t (:underline (:color foreground-color :style line) :inherit (link)))))
 '(link  ((t (:weight normal :underline (:color foreground-color :style line) :inherit (font-lock-function-name-face)))))
 '(link-visited ((t (:weight normal :underline (:color foreground-color :style line) :foreground "#D0BF8F" :inherit (link)))))
 '(fringe ((t (:foreground "#DCDCCC" :background "#4F4F4F"))))
 '(header-line ((t (:box (:line-width -1 :color nil :style released-button) :foreground "#F0DFAF" :background "#2B2B2B" :inherit (mode-line)))))
 '(tooltip ((((class color)) (:inherit (variable-pitch) :foreground "black" :background "lightyellow")) (t (:inherit (variable-pitch)))))
 '(mode-line ((t (:box (:line-width -1 :color nil :style released-button) :foreground "#8FB28F" :background "#2B2B2B"))))
 '(mode-line-buffer-id ((t (:weight bold :foreground "#F0DFAF"))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:weight light :box (:line-width -1 :color nil :style released-button) :foreground "#5F7F5F" :background "#383838" :inherit (mode-line)))))
 '(isearch ((t (:weight bold :foreground "#D0BF8F" :background "#2B2B2B"))))
 '(isearch-fail ((t (:foreground "#DCDCCC" :background "#8C5353"))))
 '(lazy-highlight ((t (:weight bold :foreground "#D0BF8F" :background "#383838"))))
 '(match ((t (:weight bold :foreground "#DFAF8F" :background "#2B2B2B"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(outline-1 ((t (:inherit font-lock-function-name-face :weight bold :foreground "#73C0C3"))))
 '(outline-2 ((t (:inherit font-lock-variable-name-face :weight bold))))
 '(outline-3 ((t (:inherit font-lock-keyword-face :weight bold))))
 '(outline-4 ((t (:inherit outline-3  :foreground "#EAD0A0"))))
 '(org-done ((t (:weight bold :foreground "DarkKhaki"))))
 `(default ,custom-theme-shawn-default))

(when (eq system-type 'darwin)
  (custom-theme-set-faces
   'Shawn
   ;; need to tweak the height so it's not tiny on OS X
   '(default ((t (:family "Monaco" :foundry "apple" :width normal :height 120 :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "#dcdccc" :background "gray12" :stipple nil :inherit nil))))
   ))

(provide-theme 'Shawn)
