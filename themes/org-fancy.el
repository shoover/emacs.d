;; fancy org faces
;; tweaked from http://www.howardism.org/Technical/Emacs/orgmode-wordprocessor.html

;; fancy list bullets
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([*]\\) " ; asterisks need a space first to skip headings
                           (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))
                          ("^ *\\([-]\\) " ; hyphens can start at bol
                           (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; heading bullets. It's hard to find characters that look ok without
;; dominating, but the asterisks are pretty ugly once you make the
;; font bigger.
;; I like it but it's slow.
;; (add-hook 'org-mode-hook 'org-bullets-mode)
;; (remove-hook 'org-mode-hook 'org-bullets-mode)
;; (setq org-bullets-bullet-list '("●")) ; "○"

;; Larger, variable-width heading fonts. 2-8 italic, for fun.
;; Reference `(font-family-list)'
(let* ((variable-tuple (cond
                        ((x-list-fonts "IBM Plex Sans")   '(:family "IBM Plex Sans"))

                        ;; On MacOS, I'd like to use SF Pro; it *appears* to
                        ;; work when set but actually maps to Helvetica? And it
                        ;; is never listed. So just use Helvetica.
                        ((x-list-fonts "Helvetica")       '(:family "Helvetica"))
                        ((x-list-fonts "Calibri")         '(:family "Calibri"))
                        ((x-list-fonts "Verdana")         '(:family "Verdana"))
                        ((x-list-fonts "Lucida Grande")   '(:family "Lucida Grande"))
                        ((x-list-fonts "Source Sans Pro") '(:family "Source Sans Pro"))
                        ((x-list-fonts "InputSans") '(:family "InputSans Bold"))
                        ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                        (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color    (face-foreground 'default nil 'default))
       (base-font-color "#bdbc91")      ; pale yellow/tan
       (headline           `(:inherit default :weight bold :foreground ,base-font-color
                                      :underline nil)))

  (custom-theme-set-faces 'user
                          `(org-level-8 ((t (,@headline ,@variable-tuple :height 1.3))))
                          `(org-level-7 ((t (,@headline ,@variable-tuple :height 1.3))))
                          `(org-level-6 ((t (,@headline ,@variable-tuple :height 1.3))))
                          `(org-level-5 ((t (,@headline ,@variable-tuple :height 1.3))))
                          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.3 :slant italic))))
                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.3))))
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.3 :slant italic))))
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.4))))
                          `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))

;; justified tags look terrible with the variable-width heading font,
;; so turn that off
(setq org-tags-column 0)
