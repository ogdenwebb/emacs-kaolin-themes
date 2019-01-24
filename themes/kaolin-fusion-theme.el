;;; kaolin-fusion-theme.el --- Colorful theme with dark gray background
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(define-kaolin-theme fusion
  "Theme with dark gray background and syntax highlighting based on bright colors, such as crimson, pink, teal, amber and ultramarine."

  ;; Palette modification
  (
   ;; Colors
   (spring-green6 "#3e594e")
   (aquamarine4   "#518270")
   (orange3       "#f5c791")
   (crimson0      "#E14676")

   ;; Color vars
   (bg1 "#222933")
   (bg2 "#2a323f")
   (bg3 "#323c4b")
   (bg4 "#3a4556")

   (keyword     crimson0)
   (second-key  keyword)
   (builtin     teal4)
   (functions   cyan3)

   (var         aquamarine3)
   (const       magenta3)
   ;; (type        capri3)
   (type        teal0)
   (type        ultramarine3)

   (comment     azure7)
   (comment-alt vermilion7)

   (str         capri4)
   (str-alt     spring-green4)
   (doc         str-alt)

   (prep        teal0)
   ;; (prep        ultramarine3)
   (num         amber3)
   (bool        num)
   (warning     orange1)
   (err         red3)

   (dim-buffer white0)
   (hl         amber0)
   ;; TODO: add colored
   (hl-line    (if kaolin-themes-hl-line-colored capri6 bg3))
   (hl-indent  gray0)
   ;; TODO:
   (selection crimson6)
   ;; (selection ultramarine6)
   ;; TODO:
   (pulse bg4)

   (todo red3)

   (tooltip-hl-bg bg3)
   (tooltip-hl-fg hl)

   (rb1 crimson3)
   (rb2 pink3)
   (rb3 orange3)
   (rb4 aquamarine1)
   (rb5 purple3)
   (rb6 cyan0)
   (rb7 spring-green3)
   (rb8 ultramarine3)
   (rb9 yellow3)

   (diff-add spring-green3)
   (diff-mod vermilion3)
   (diff-rem red3)

    ;; Mode-line
   (line-fg           fg4)
   (line-color1       functions)
   (line-color2       str)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-border       bg3)

   ;; Telephone-line
   (segment-active    gray2)
   (segment-inactive  gray2)

   (win-border    bg3)
   (line-num-fg   azure7)
   (line-num-hl   hl)

   (cursor        fg1)

   (ivy1          gray9)
   (ivy2          cerulean3)
   (ivy3          yellow0)
   (ivy4          green3))

  (
   (highlight-quoted-quote   (:foreground orange3))
   (highlight-quoted-symbol  (:foreground amber3))

   (org-document-title     (:foreground orange3 :bold bold))
   ;; (org-document-info      (:foreground brown3))

   (org-level-1            (:foreground var :bold bold :height 1.1))
   (org-level-2            (:foreground functions  :bold nil))
   (org-level-3            (:foreground str :bold nil))
   (org-level-4            (:foreground num :bold nil)))

  (when kaolin-themes-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-fusion
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-mod :foreground ,diff-mod))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))

;;; kaolin-fusion-theme.el ends here
