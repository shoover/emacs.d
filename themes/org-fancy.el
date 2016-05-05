;; fancy org faces
;; tweaked from http://www.howardism.org/Technical/Emacs/orgmode-wordprocessor.html

(setq org-hide-emphasis-markers t)

;; fancy list bullets
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([*]\\) " ; asterisks need a space first to skip headings
                           (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))
                          ("^ *\\([-]\\) " ; hyphens can start at bol
                           (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; heading bullets. It's hard to find characters that look ok without
;; dominating, but the asterisks are admittedly pretty ugly once you
;; make the font bigger
(add-hook 'org-mode-hook 'org-bullets-mode)
(setq org-bullets-bullet-list '("●")) ; "○"

;; larger, variable-width heading fonts. 2-8 italic, for fun.
(let* ((variable-tuple (cond
                        ((x-list-fonts "Calibri")         '(:font "Calibri"))
                        ((x-list-fonts "Verdana")         '(:font "Verdana"))
                        ((x-list-fonts "InputSans") '(:font "InputSans Bold"))
                        ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                        ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                        ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                        (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color    (face-foreground 'default nil 'default))
       (base-font-color "#bdbc91") ; pale yellow/tan
       (headline           `(:inherit default :weight bold :foreground ,base-font-color
                                      :underline nil)))

  (custom-theme-set-faces 'user
                          `(org-level-8 ((t (,@headline ,@variable-tuple :height 1.25))))
                          `(org-level-7 ((t (,@headline ,@variable-tuple :height 1.25))))
                          `(org-level-6 ((t (,@headline ,@variable-tuple :height 1.25))))
                          `(org-level-5 ((t (,@headline ,@variable-tuple :height 1.3 :slant italic))))
                          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.3 :slant italic))))
                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.4 :slant italic))))
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5 :slant italic))))
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.6))))
                          `(org-document-title ((t (,@headline ,@variable-tuple :height 1.6 :underline nil))))))

;; justified tags look terrible with the variable-width heading font,
;; so turn that off
(setq org-tags-column 0)
