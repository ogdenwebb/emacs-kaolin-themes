;;; kaolin-dark-theme.el --- A dark jade theme inspired by Sierra.vim

(require 'kaolin-themes)

(define-kaolin-theme dark "A dark jade Kaolin theme inspired by Sierra.vim."

  ;; Theme colors
  ()

  ;; Theme faces
  (
   ;; Interface
   (button                (:inherit 'link))
   (custom-button         (:background bg3 :foreground grayish-orange :box (:line-width 2 :color bg2 :style 'released-button)))
   (custom-button-mouse   (:background bg4 :foreground light-orange :box   (:line-width 2 :color bg2 :style 'released-button)))
   (custom-button-pressed (:background bg4 :foreground light-orange :box   (:line-width 2 :color bg2 :style 'pressed-button)))
   (custom-visibility     (:background nil :foreground cyan :height 0.9 :underline underline))
   (custom-state          (:background nil :foreground green))
   (custom-changed        (:background nil :foreground orange))
   (custom-set            (:background nil :foreground teal-green))
   (custom-invalid        (:background nil :foreground red))
   (custom-face-tag       (:background nil :foreground purple :bold bold))
   (custom-link           (:background nil :foreground teal :bold bold))
   (widget-button         (:background nil :foreground green :bold bold))
   (widget-button-pressed (:background nil :foreground jade))
   (widget-field          (:background bg3 :foreground fg1 :box            (:line-width 1 :color bg2 :style nil)))
   (widget-documentation  (:background nil :foreground faded-blue))

   ;; Org-mode
   ;; Pay attention org-level-4 uses to diplay names in ivy-switch-buffer
   (org-level-1                   (:foreground green :bold bold :height 1.1))
   (org-level-2                   (:foreground teal-blue :bold nil))
   ;; TODO: change because faded-orange uses in links
   (org-level-3                   (:foreground faded-orange :bold nil))
   (org-level-4                   (:foreground faded-wheat :bold nil))
   (org-tag                       (:foreground orange :bold bold))
   (org-checkbox                  (:foreground green :bold bold))
   (org-headline-done             (:foreground teal-blue  :bold nil))
   (org-checkbox-statistics-todo  (:foreground faded-blue :bold bold))
   (org-checkbox-statistics-done  (:foreground teal-green :bold bold))
   (org-hide                      (:foreground bg2))
   (org-footnote                  (:foreground fg4 :underline underline))
   (org-special-keyword           (:foreground functions))
   (org-block                     (:foreground fg3))
   (org-table                     (:foreground faded-blue :bold bold))
   (org-formula                   (:foreground orange))
   (org-quote                     (:inherit 'org-block :slant 'italic))
   (org-verse                     (:inherit 'org-block :slant 'italic))
   (org-warning                   (:foreground warning :underline underline))
   (org-agenda-structure          (:background bg3 :foreground fg3 :bold bold))
   (org-agenda-date               (:foreground light-yellow :height 1.1))
   (org-agenda-date-weekend       (:weight 'normal :foreground fg4))
   (org-agenda-date-today         (:foreground purple :height 1.2 :bold bold))
   (org-agenda-done               (:foreground bg4))
   (org-scheduled                 (:foreground type))
   (org-scheduled-today           (:foreground functions :height 1.2 :bold bold))
   (org-sexp-date                 (:foreground fg4))

   ;; Ediff
   (ediff-current-diff-A (:background dark-red :foreground red))
   (ediff-current-diff-B (:background dark-green :foreground light-green))
   (ediff-current-diff-C (:background dark-blue :foreground teal-blue))

   (ediff-even-diff-A (:background bg2))
   (ediff-even-diff-B (:background bg2))
   (ediff-even-diff-C (:background bg2))

   (ediff-fine-diff-A (:background nil :bold bold))
   (ediff-fine-diff-B (:background nil :bold bold))
   (ediff-fine-diff-C (:background nil :bold bold))

   (ediff-odd-diff-A (:background bg3))
   (ediff-odd-diff-B (:background bg3))
   (ediff-odd-diff-C (:background bg3))

   ;; Magit
   (magit-section-highlight      (:background bg2))
   (magit-diff-file-header       (:background bg3 :foreground fg2))
   (magit-item-highlight :background bg3)
   (magit-section-heading        (:foreground keyword :bold bold))
   (magit-hunk-heading           (:background bg3))
   (magit-hunk-heading-highlight (:background bg3))
   (magit-diff-context-highlight (:background bg3 :foreground fg3))
   (magit-diffstat-added         (:foreground type))
   (magit-diffstat-removed       (:foreground var))
   (magit-process-ok             (:foreground functions :bold bold))
   (magit-process-ng             (:foreground warning :bold bold))
   (magit-branch                 (:foreground const :bold bold))
   (magit-log-author             (:foreground fg3))
   (magit-hash                   (:foreground fg2))

   ;; Helm
   (helm-header                              (:background bg1 :foreground fg2 :underline nil :box nil))
   (helm-source-header                       (:background bg1 :foreground keyword :underline nil :bold bold))
   (helm-match                               (:inherit 'default :foreground orange :bold bold))
   (helm-header-line-left-margin             (:background blue :foreground bg1))
   (helm-selection                           (:background bg2 :foreground orange :bold bold))
   (helm-selection-line                      (:background bg2 :foreground orange :bold bold))
   (helm-visible-mark                        (:background bg1 :foreground blue))
   (helm-candidate-number                    (:foreground lime))
   (helm-separator                           (:background bg1 :foreground type))
   (helm-time-zone-current                   (:background bg1 :foreground builtin))
   (helm-time-zone-home                      (:background bg1 :foreground type))
   (helm-buffer-not-saved                    (:background bg1 :foreground type))
   (helm-buffer-process                      (:background bg1 :foreground builtin))
   (helm-buffer-saved-out                    (:background bg1 :foreground fg1))
   (helm-buffer-size                         (:background bg1 :foreground fg1))
   (helm-ff-directory                        (:background bg1 :foreground functions :bold bold))
   (helm-buffer-directory                    (:background bg1 :foreground purple))
   (helm-ff-dotted-directory                 (:background bg1 :foreground functions :bold bold))
   (helm-ff-dotted-symlink-directory         (:background bg1 :foreground blue :bold bold))
   (helm-ff-file                             (:background bg1 :foreground fg1 :weight 'normal))
   (helm-ff-executable                       (:background bg1 :foreground keyword :weight 'normal))
   (helm-ff-invalid-symlink                  (:background bg1 :foreground warning :bold bold))
   (helm-resume-need-update                  (:background alt-red :foreground nil))
   (helm-ff-symlink                          (:background bg1 :foreground keyword :bold bold))
   (helm-ff-prefix                           (:background keyword :foreground bg1 :weight 'normal))
   (helm-grep-cmd-line                       (:background bg1 :foreground fg1))
   (helm-grep-file                           (:background bg1 :foreground fg1))
   (helm-grep-finish                         (:background bg1 :foreground fg2))
   (helm-grep-lineno                         (:background bg1 :foreground fg1))
   (helm-grep-match                          (:inherit 'helm-match :background nil :foreground nil))
   (helm-grep-running                        (:background bg1 :foreground functions))
   (helm-moccur-buffer                       (:background bg1 :foreground functions))
   (helm-source-go-package-godoc-description (:foreground str))
   (helm-bookmark-w3m                        (:foreground type))

   ;; Speedbar
   (speedbar-separator-face (:background blue))
   (speedbar-directory-face (:foreground teal))
   (speedbar-file-face      (:foreground green))
   (speedbar-tag-face       (:foreground faded-blue))
   (speedbar-selected-face  (:foreground teal-green))
   (speedbar-highlight-face (:foreground cyan))
   (speedbar-button-face    (:foreground jade))

   ;; Perl6
   (perl6-phaser          (:foreground cyan))
   (perl6-type-constraint (:inherit 'font-lock-keyword-face))

   ;; Shell
   (sh-quoted-exec (:foreground light-yellow))

   ;; Clojure
   (clojure-keyword-face (:inherit 'font-lock-variable-name-face))

   ;; OCaml
   (tuareg-font-lock-governing-face (:foreground green :bold bold))
   (tuareg-font-double-colon-face   (:foreground yellow))
   (tuareg-font-lock-error-face     (:foreground alt-red))
   ;; TODO: find code with following face
   ;; (tuareg-font-lock-multistage-face (:foreground alt-red))))

   ;; Ace-window
   (aw-leading-char-face (:foreground pink :bold bold))
   (aw-background-face   (:foreground bg4 :bold bold))

   ;; Rst-mode
   (rst-adornment (:foreground jade))
   (rst-block     (:foreground teal))
   (rst-level-1   (:foreground violet))
   (rst-level-2   (:foreground green))
   (rst-level-3   (:foreground teal-blue))
   (rst-level-4   (:foreground violet))
   (rst-level-5   (:foreground green))
   (rst-level-6   (:foreground teal-blue))

   ;; TODO: split
   ;; Vimish-fold
   (vimish-fold-overlay (:background bg2 :foreground comment))
   (vimish-fold-fringe  (:background nil :foreground jade))

   ;; Avy
   (avy-lead-face   (:background dark-red :foreground fg1))
   (avy-lead-face-0 (:background jade :foreground fg1))
   (avy-lead-face-1 (:background dark-blue :foreground fg1))
   (avy-lead-face-2 (:background dark-purple :foreground fg1))

   ;; TODO: split
   ;; Ivy & Swiper
   (ivy-modified-buffer     (:foreground alt-lavender))
   (ivy-subdir              (:foreground green :bold bold))
   (ivy-action              (:background nil :foreground light-green :bold bold))
   (ivy-virtual             (:foreground light-yellow))
   (ivy-remote              (:foreground red))
   (ivy-cursor              (:background bg3 :foreground fg1))
   (ivy-match-required-face (:background nil :foreground alt-red :bold nil))
   (ivy-confirm-face        (:background nil :foreground light-orange))))


;;; kaolin-dark-theme.el ends here
