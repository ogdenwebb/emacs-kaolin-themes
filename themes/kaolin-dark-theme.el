;;; kaolin-dark-theme.el --- A dark jade theme inspired by Sierra.vim

(require 'kaolin-themes)

(define-kaolin-theme dark "A dark jade Kaolin theme inspired by Sierra.vim."

  ;; Theme colors
  ((white0          "#efeff1")
   (white1          "#e4e4e8")
   (white2          "#c8c8d0")
   (white3          "#babac4")
   (white4          "#adadb9"))

  (
   ;; Theme faces
   (header-line (:background capri5 :foreground var))
   ;; Interface
   ;; TODO:
   (custom-visibility     (:background nil :foreground cyan1 :height 0.9 :underline underline))
   (custom-set            (:background nil :foreground spring-green3))
   (custom-face-tag       (:background nil :foreground magenta4 :bold bold))
   (custom-link           (:background nil :foreground cyan3 :bold bold))
   (widget-button         (:background nil :foreground teal1 :bold bold))
   (widget-button-pressed (:background nil :foreground spring-green4))
   (widget-field          (:background bg3 :foreground fg1 :box (:line-width 1 :color bg2 :style nil)))
   (widget-documentation  (:background nil :foreground var))

   ;; Org-mode
   ;; Pay attention org-level-4 uses to diplay names in ivy-switch-buffer
   (org-level-1                   (:foreground teal1 :bold bold :height 1.1))
   (org-level-2                   (:foreground capri4 :bold nil))
   (org-level-3                   (:foreground aquamarine4 :bold nil))
   (org-level-4                   (:foreground amber3 :bold nil))
   (org-tag                       (:foreground orange1 :bold bold))
   (org-checkbox                  (:foreground teal1 :bold bold))
   ;; TODO: refact
   (org-headline-done             (:foreground capri4  :bold nil))
   (org-checkbox-statistics-todo  (:foreground var :bold bold))
   (org-checkbox-statistics-done  (:foreground spring-green3 :bold bold))
   (org-hide                      (:foreground bg2))
   (org-agenda-structure          (:background bg3 :foreground fg3 :bold bold))
   (org-agenda-date               (:foreground amber3 :height 1.1))
   (org-agenda-date-today         (:foreground magenta4 :height 1.2 :bold bold))

   ;; Ediff
   ;; TODO: change
   (ediff-current-diff-A (:background red2 :foreground red1))
   (ediff-current-diff-B (:background spring-green2 :foreground spring-green1))
   (ediff-current-diff-C (:background azure2 :foreground capri4))

   (ediff-even-diff-A (:background bg2))
   (ediff-even-diff-B (:background bg2))
   (ediff-even-diff-C (:background bg2))

   (ediff-fine-diff-A (:background nil :bold bold))
   (ediff-fine-diff-B (:background nil :bold bold))
   (ediff-fine-diff-C (:background nil :bold bold))

   (ediff-odd-diff-A (:background bg3))
   (ediff-odd-diff-B (:background bg3))
   (ediff-odd-diff-C (:background bg3))

   ;; Speedbar
   ;; TODO:
   (speedbar-separator-face (:background azure1))
   (speedbar-directory-face (:foreground cyan3))
   (speedbar-file-face      (:foreground teal1))
   (speedbar-tag-face       (:foreground var))
   (speedbar-selected-face  (:foreground spring-green3))
   (speedbar-highlight-face (:foreground cyan1))
   (speedbar-button-face    (:foreground spring-green4))

   ;; Perl6
   ;; TODO:
   (perl6-phaser          (:foreground cyan1))
   (perl6-type-constraint (:inherit 'font-lock-keyword-face))

   ;; Shell
   (sh-quoted-exec (:foreground amber3))

   ;; Clojure
   (clojure-keyword-face (:inherit 'font-lock-variable-name-face))

   ;; OCaml
   (tuareg-font-lock-governing-face (:foreground teal1 :bold bold))
   (tuareg-font-double-colon-face   (:foreground yellow1))
   (tuareg-font-lock-error-face     (:foreground red0))
   ;; TODO: find code with following face
   ;; (tuareg-font-lock-multistage-face (:foreground red0))))

   ;; Ace-window
   (aw-leading-char-face (:foreground pink1 :bold bold))
   (aw-background-face   (:foreground bg4 :bold bold))

   ;; Rst-mode
   (rst-adornment (:foreground spring-green4))
   (rst-block     (:foreground cyan3))
   (rst-level-1   (:foreground purple4))
   (rst-level-2   (:foreground teal1))
   (rst-level-3   (:foreground capri4))
   (rst-level-4   (:foreground purple4))
   (rst-level-5   (:foreground teal1))
   (rst-level-6   (:foreground capri4))

   ;; TODO: split
   ;; Vimish-fold
   (vimish-fold-overlay (:background bg2 :foreground comment))
   (vimish-fold-fringe  (:background nil :foreground spring-green4))

   ;; Avy
   (avy-lead-face   (:background red2 :foreground fg1))
   (avy-lead-face-0 (:background spring-green4 :foreground fg1))
   (avy-lead-face-1 (:background azure2 :foreground fg1))
   (avy-lead-face-2 (:background magenta2 :foreground fg1))

   ;; Ivy & Swiper
   (ivy-modified-buffer     (:foreground violet4))
   (ivy-action              (:background nil :foreground spring-green1 :bold bold))
   (ivy-virtual             (:foreground amber3))
   (ivy-remote              (:foreground red1))
   (ivy-confirm-face        (:background nil :foreground amber3)))

  (when kaolin-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-dark
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-mod :foreground ,diff-mod))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))


;;; kaolin-dark-theme.el ends here
