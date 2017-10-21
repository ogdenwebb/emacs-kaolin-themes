;;; kaolin-dark-theme.el --- A dark jade theme inspired by Sierra.vim

(require 'kaolin-theme)

;; Theme colors
;; TODO: add following vars
;; TODO: add default bold, italic and etc face to lib
;; TODO: add prompt
;; ivy/swiper color
;; second-hl for indent and etc
;; selection
;; TODO: add the following faces to default
;; (??) start fg and bg with 0
;; (??) rename bg1-4 fg1-4 to one var var1-8

(define-kaolin-theme dark "A dark jade Kaolin theme inspired by Sierra.vim "

  ()

  ()


  (let ((c '((class color) (min-colors 89))))
    (custom-theme-set-faces
     'kaolin-dark
     ;; Interface
     ;; `(button ((,c (:foreground ,lavender :underline ,underline))))
     `(button ((,c (:inherit link))))
     `(custom-button ((,c (:background ,bg3 :foreground ,grayish-orange :box (:line-width 2 :color ,bg2 :style released-button)))))
     `(custom-button-mouse ((,c (:background ,bg4 :foreground ,light-orange :box (:line-width 2 :color ,bg2 :style released-button)))))
     `(custom-button-pressed ((,c (:background ,bg4 :foreground ,light-orange :box (:line-width 2 :color ,bg2 :style pressed-button)))))
     `(custom-visibility ((,c (:background nil :foreground ,cyan :height 0.9 :underline ,underline))))
     `(custom-state ((,c (:background nil :foreground ,green))))
     `(custom-changed ((,c (:background nil :foreground ,orange))))
     `(custom-set ((,c (:background nil :foreground ,teal-green))))
     `(custom-invalid ((,c (:background nil :foreground ,red))))
     `(custom-face-tag ((,c (:background nil :foreground ,purple :bold ,bold))))
     `(custom-link ((,c (:background nil :foreground ,teal :bold ,bold))))
     `(widget-button ((,c (:background nil :foreground ,green :bold ,bold))))
     `(widget-button-pressed ((,c (:background nil :foreground ,faded-red))))
     `(widget-field ((,c (:background ,bg3 :foreground ,fg1 :box (:line-width 1 :color ,bg2 :style nil)))))
     `(widget-documentation ((,c (:background nil :foreground ,faded-blue))))

     ;; Eldoc
     `(eldoc-highlight-function-argument ((t (:foreground ,violet))))


     ;; Org-mode
     ;; Pay attention org-level-4 uses to diplay names in ivy-switch-buffer
     `(org-level-1 ((,c (:foreground ,green :bold ,bold :height 1.1))))
     `(org-level-2 ((,c (:foreground ,teal-blue :bold nil))))
     ;; TODO: change because faded-orange uses in links
     `(org-level-3 ((,c (:foreground ,faded-orange :bold nil))))
     `(org-level-4 ((,c (:foreground ,faded-wheat :bold nil))))
     `(org-tag ((,c (:foreground ,orange :bold ,bold))))
     `(org-checkbox ((,c (:foreground ,green :bold ,bold))))
     `(org-todo ((,c (:foreground ,red :bold ,bold))))
     `(org-done ((,c (:foreground ,teal-green  :bold ,bold))))
     `(org-list-dt ((,c (:inherit org-checkbox))))
     `(org-headline-done ((,c (:foreground ,teal-blue  :bold nil))))
     `(org-checkbox-statistics-todo ((,c (:foreground ,faded-blue :bold ,bold))))
     `(org-checkbox-statistics-done ((,c (:foreground ,teal-green :bold ,bold))))
     `(org-code ((,c (:foreground ,light-yellow))))
     `(org-verbatim ((,c (:foreground ,soft-blue))))
     `(org-hide ((,c (:foreground ,bg2))))
     `(org-date ((,c (:foreground ,light-yellow :underline ,underline))))
     `(org-document-title ((,c (:foreground ,teal :bold ,bold))))
     `(org-document-info-keyword ((,c (:foreground ,deep-green))))
     `(org-meta-line ((,c (:inherit org-document-info-keyword))))
     `(org-document-info ((,c (:foreground ,teal))))
     `(org-footnote  ((,c (:foreground ,fg4 :underline ,underline))))
     `(org-link ((,c (:inherit link))))
     `(org-special-keyword ((,c (:foreground ,functions))))
     `(org-block ((,c (:foreground ,fg3))))
     `(org-block-begin-line ((,c (:foreground ,deep-green))))
     `(org-block-end-line ((,c (:inherit org-block-begin-line))))
     `(org-table ((,c (:foreground ,faded-blue :bold ,bold))))
     `(org-formula ((,c (:foreground ,orange))))
     `(org-quote ((,c (:inherit org-block :slant italic))))
     `(org-verse ((,c (:inherit org-block :slant italic))))
     `(org-warning ((,c (:foreground ,warning :underline ,underline))))
     `(org-agenda-structure ((,c (:background ,bg3 :foreground ,fg3 :bold ,bold))))
     `(org-agenda-date ((,c (:foreground ,light-yellow :height 1.1))))
     `(org-agenda-date-weekend ((,c (:weight normal :foreground ,fg4))))
     `(org-agenda-date-today ((,c (:foreground ,purple :height 1.2 :bold ,bold))))
     `(org-agenda-done ((,c (:foreground ,bg4))))
     `(org-scheduled ((,c (:foreground ,type))))
     `(org-scheduled-today ((,c (:foreground ,functions :height 1.2 :bold ,bold))))
     `(org-ellipsis ((,c (:foreground ,builtin))))
     `(org-sexp-date ((,c (:foreground ,fg4))))

     ;; Latex
     `(font-latex-bold-face ((,c (:foreground ,type))))
     `(font-latex-italic-face ((,c (:foreground ,keyword :italic ,italic))))
     `(font-latex-string-face ((,c (:foreground ,str))))
     `(font-latex-match-reference-keywords ((,c (:foreground ,const))))
     `(font-latex-match-variable-keywords ((,c (:foreground ,var))))


     `(ac-completion-face ((,c (:foreground ,keyword :underline ,underline))))
     `(info-quoted-name ((,c (:foreground ,builtin))))
     `(info-string ((,c (:foreground ,str))))
     `(icompletep-determined ((,c :foreground ,builtin)))


     ;; Ediff
     `(ediff-current-diff-A ((,c (:background ,dark-red :foreground ,red))))
     `(ediff-current-diff-B ((,c (:background ,dark-green :foreground ,light-green))))
     `(ediff-current-diff-C ((,c (:background ,dark-blue :foreground ,teal-blue))))

     `(ediff-even-diff-A ((,c (:background ,bg2))))
     `(ediff-even-diff-B ((,c (:background ,bg2))))
     `(ediff-even-diff-C ((,c (:background ,bg2))))

     `(ediff-fine-diff-A ((,c (:background nil :bold ,bold))))
     `(ediff-fine-diff-B ((,c (:background nil :bold ,bold))))
     `(ediff-fine-diff-C ((,c (:background nil :bold ,bold))))

     `(ediff-odd-diff-A ((,c (:background ,bg3))))
     `(ediff-odd-diff-B ((,c (:background ,bg3))))
     `(ediff-odd-diff-C ((,c (:background ,bg3))))

     ;; Magit
     `(magit-section-highlight ((,c (:background ,bg2))))
     `(magit-diff-file-header ((,c (:background ,bg3 :foreground ,fg2))))
     `(magit-item-highlight ((,c :background ,bg3)))
     `(magit-section-heading ((,c (:foreground ,keyword :bold ,bold))))
     `(magit-hunk-heading ((,c (:background ,bg3))))
     `(magit-hunk-heading-highlight ((,c (:background ,bg3))))
     `(magit-diff-context-highlight ((,c (:background ,bg3 :foreground ,fg3))))
     `(magit-diffstat-added   ((,c (:foreground ,type))))
     `(magit-diffstat-removed ((,c (:foreground ,var))))
     `(magit-process-ok ((,c (:foreground ,functions :bold ,bold))))
     `(magit-process-ng ((,c (:foreground ,warning :bold ,bold))))
     `(magit-branch ((,c (:foreground ,const :bold ,bold))))
     `(magit-log-author ((,c (:foreground ,fg3))))
     `(magit-hash ((,c (:foreground ,fg2))))

     ;; Helm
     `(helm-header ((,c (:background ,bg1 :foreground ,fg2 :underline nil :box nil))))
     `(helm-source-header ((,c (:background ,bg1 :foreground ,keyword :underline nil :bold ,bold))))
     `(helm-match ((,c (:inherit default :foreground ,orange :bold ,bold))))
     `(helm-header-line-left-margin ((t (:background ,blue :foreground ,bg1))))
     `(helm-selection ((,c (:background ,bg2 :foreground ,orange :bold ,bold))))
     `(helm-selection-line ((,c (:background ,bg2 :foreground ,orange :bold ,bold))))
     `(helm-visible-mark ((,c (:background ,bg1 :foreground ,blue))))
     `(helm-candidate-number ((,c (:foreground ,lime))))
     `(helm-separator ((,c (:background ,bg1 :foreground ,type))))
     `(helm-time-zone-current ((,c (:background ,bg1 :foreground ,builtin))))
     `(helm-time-zone-home ((,c (:background ,bg1 :foreground ,type))))
     `(helm-buffer-not-saved ((,c (:background ,bg1 :foreground ,type))))
     `(helm-buffer-process ((,c (:background ,bg1 :foreground ,builtin))))
     `(helm-buffer-saved-out ((,c (:background ,bg1 :foreground ,fg1))))
     `(helm-buffer-size ((,c (:background ,bg1 :foreground ,fg1))))
     `(helm-ff-directory ((,c (:background ,bg1 :foreground ,functions :bold ,bold))))
     `(helm-buffer-directory ((,c (:background ,bg1 :foreground ,purple))))
     `(helm-ff-dotted-directory ((,c (:background ,bg1 :foreground ,functions :bold ,bold))))
     `(helm-ff-dotted-symlink-directory ((,c (:background ,bg1 :foreground ,blue :bold ,bold))))
     `(helm-ff-file ((,c (:background ,bg1 :foreground ,fg1 :weight normal))))
     `(helm-ff-executable ((,c (:background ,bg1 :foreground ,keyword :weight normal))))
     `(helm-ff-invalid-symlink ((,c (:background ,bg1 :foreground ,warning :bold ,bold))))
     `(helm-resume-need-update ((,c (:background ,alt-red :foreground nil))))
     `(helm-ff-symlink ((,c (:background ,bg1 :foreground ,keyword :bold ,bold))))
     `(helm-ff-prefix ((,c (:background ,keyword :foreground ,bg1 :weight normal))))
     `(helm-grep-cmd-line ((,c (:background ,bg1 :foreground ,fg1))))
     `(helm-grep-file ((,c (:background ,bg1 :foreground ,fg1))))
     `(helm-grep-finish ((,c (:background ,bg1 :foreground ,fg2))))
     `(helm-grep-lineno ((,c (:background ,bg1 :foreground ,fg1))))
     `(helm-grep-match ((,c (:background nil :foreground nil :inherit helm-match))))
     `(helm-grep-running ((,c (:background ,bg1 :foreground ,functions))))
     `(helm-moccur-buffer ((,c (:background ,bg1 :foreground ,functions))))
     `(helm-source-go-package-godoc-description ((,c (:foreground ,str))))
     `(helm-bookmark-w3m ((,c (:foreground ,type))))

     ;; Speedbar
     `(speedbar-separator-face ((,c (:background ,blue))))
     `(speedbar-directory-face ((,c (:foreground ,teal))))
     `(speedbar-file-face ((,c (:foreground ,green))))
     `(speedbar-tag-face ((,c (:foreground ,faded-blue))))
     `(speedbar-selected-face ((,c (:foreground ,teal-green))))
     `(speedbar-highlight-face ((,c (:foreground ,cyan))))
     `(speedbar-button-face ((,c (:foreground ,jade))))

     ;; Haskell mode
     ;; `(haskell-operator-face ((,c (:foreground ,lime))))
     ;; `(haskell-type-face ((,c (:foreground ,light-yellow))))
     ;; `(haskell-constructor-face ((,c (:foreground ,orange))))

     ;; Perl6
     ;; `(perl6-identifier ((,c (:foreground ,cyan))))
     `(perl6-phaser ((,c (:foreground ,cyan))))
     `(perl6-type-constraint ((,c (:inherit font-lock-keyword-face))))

     ;; Shell
     `(sh-quoted-exec ((,c (:foreground ,light-yellow))))

     ;; Flx
     `(flx-highlight-face ((,c (:foreground ,hl :underline ,underline))))

     ;; Emmet
     `(emmet-preview-input ((t (:foreground nil :background nil))))
     `(emmet-preview-output ((t (:foreground nil :background nil))))

     ;; Clojure
     `(clojure-keyword-face ((,c (:inherit ,font-lock-variable-name-face))))

     ;; OCaml
     `(tuareg-font-lock-governing-face ((,c (:foreground ,green :bold ,bold))))
     `(tuareg-font-double-colon-face ((,c (:foreground ,yellow))))
     `(tuareg-font-lock-error-face ((,c (:foreground ,alt-red))))
     ;; FIXME: find code with following face
     ;; `(tuareg-font-lock-multistage-face ((,c (:foreground ,alt-red))))

     ;; Nim
     `(nim-font-lock-export-face ((,c (:inherit font-lock-function-name-face :italic nil))))

     ;; Ace-window
     `(aw-leading-char-face ((,c (:foreground ,pink :bold ,bold))))
     `(aw-background-face ((,c (:foreground ,bg4 :bold ,bold))))

     ;; Latex/Auctex
     `(font-latex-warning-face ((,c (:inherit warning))))
     `(font-latex-string-face ((,c (:inherit font-lock-string-face))))
     `(font-latex-math-face ((,c (:foreground ,violet))))
     `(font-latex-sedate-face ((,c (:foreground ,teal-blue))))
     `(font-latex-script-char-face ((,c (:foreground ,violet))))
     `(font-latex-sectioning-0-face ((,c (:foreground ,wheat :bold ,bold))))
     `(font-latex-sectioning-1-face ((,c (:inherit font-latex-sectioning-0-face))))
     `(font-latex-sectioning-2-face ((,c (:inherit font-latex-sectioning-0-face))))
     `(font-latex-sectioning-3-face ((,c (:inherit font-latex-sectioning-0-face))))
     `(font-latex-sectioning-4-face ((,c (:inherit font-latex-sectioning-0-face))))
     `(font-latex-sectioning-5-face ((,c (:inherit font-latex-sectioning-0-face))))

     ;; Rst-mode
     `(rst-adornment ((,c (:foreground ,jade))))
     `(rst-block ((,c (:foreground ,teal))))
     `(rst-level-1 ((,c (:foreground ,violet))))
     `(rst-level-2 ((,c (:foreground ,green))))
     `(rst-level-3 ((,c (:foreground ,teal-blue))))
     `(rst-level-4 ((,c (:foreground ,violet))))
     `(rst-level-5 ((,c (:foreground ,green))))
     `(rst-level-6 ((,c (:foreground ,teal-blue))))

     ;; Vimish-fold
     `(vimish-fold-overlay ((,c (:background ,bg2 :foreground ,comment))))
     `(vimish-fold-fringe ((,c (:background nil :foreground ,jade))))

     ;; TODO: Evil-goggles
     `(evil-goggles-default-face ((,c (:background ,dark-jade))))

     ;; Avy
     `(avy-lead-face ((,c (:background ,dark-red :foreground ,fg1))))
     `(avy-lead-face-0 ((,c (:background ,jade :foreground ,fg1))))
     `(avy-lead-face-1 ((,c (:background ,dark-blue :foreground ,fg1))))
     `(avy-lead-face-2 ((,c (:background ,dark-purple :foreground ,fg1))))

     ;; Ivy & Swiper
     `(ivy-modified-buffer ((,c (:foreground ,alt-lavender))))
     `(ivy-subdir ((,c (:foreground ,green :bold ,bold))))
     `(ivy-action ((,c (:background nil :foreground ,light-green :bold ,bold))))
     `(ivy-virtual ((,c (:foreground ,light-yellow))))
     `(ivy-remote ((,c (:foreground ,red))))
     `(ivy-cursor ((,c (:background ,bg3 :foreground ,fg1))))
     `(ivy-current-match ((,c (:background ,hl-line :foreground ,hl :bold t))))
     `(ivy-match-required-face ((,c (:background nil :foreground ,alt-red :bold nil))))
     `(ivy-confirm-face ((,c (:background nil :foreground ,light-orange)))))))


(provide-theme 'kaolin-dark)

;;; kaolin-dark-theme.el ends here
