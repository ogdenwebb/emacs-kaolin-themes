;;; kaolin-light-theme.el --- Light Kaolin theme variant
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(define-kaolin-theme light  "Light Kaolin theme variant."

  ;; Palette modification
  (
   ;; Colors
   (spring-green6 "#3e594e")
   (aquamarine4   "#518270")
   (orange0       "#d1832e")

   (cyan1         "#48a9a9")
   (cyan2         "#008b8b")
   (cyan3         "#6facb3")

   ;; Color vars
   ;; TODO change to more green
   (bg0 "#f5f6f5")
   (bg1 "#e9eae7")
   (bg2 "#dcded9")
   (bg3 "#cfd2cb")
   (bg4 "#c5c9c0")

   (fg1 gray1)
   (fg2 gray2)
   (fg3 gray3)
   (fg4 gray4)


   (keyword     teal2)
   (second-key  teal2)
   (var         ultramarine4)
   (const       violet4)
   (builtin     cyan4)
   (comment     lime7)
   (alt-comment azure8)
   (functions   teal1)
   (str         brown1)
   (str-alt     brown1)
   (doc         str-alt)
   (type        vermilion4)
   (num         red4)
   (bool        num)
   (prep        num)
   (warning     orange1)
   (err         red4)

   (dim-buffer white0)
   (hl         aquamarine2)
   ;; TODO: add colored
   (hl-line    (if kaolin-themes-hl-line-colored bg2 bg2))
   (hl-indent  gray9)
   ;; TODO:
   (selection bg4)
   ;; TODO:
   (pulse cyan3)

   (todo crimson4)

   (tooltip-hl-bg bg3)
   (tooltip-hl-fg hl)

    ;; TODO:
   (rb1 teal1)
   (rb2 cerise4)
   (rb3 azure4)
   (rb4 ultramarine4)
   (rb5 teal1)
   (rb6 crimson4)
   (rb7 vermilion4)
   (rb8 spring-green4)
   (rb9 violet4)

   (diff-add aquamarine4)
   (diff-mod vermilion4)
   (diff-rem red4)

    ;; Mode-line
   (line-fg           fg4)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-border       bg3)

   ;; Telephone-line
   (segment-active    gray2)
   (segment-inactive  gray2)
   (evil-normal       teal1)
   (evil-insert       spring-green1)
   (evil-visual       orange1)
   (evil-replace      red4)
   (evil-motion       yellow1)
   (evil-operator     evil-normal)
   (evil-emacs        amber3)

   (win-border    bg3)
   (line-num-fg   chartreuse8)
   (line-num-hl   hl)

   (cursor        gray3)

   (ivy1          gray9)
   (ivy2          capri1)
   (ivy3          orange0)
   (ivy4          red4))

  ((link                    (:foreground capri1 :underline underline))

   (highlight-quoted-quote  (:foreground keyword))
   (highlight-quoted-symbol (:foreground teal2))

   (org-level-1             (:foreground teal1 :bold bold :height 1.1))
   (org-level-2             (:foreground ultramarine4 :bold nil))
   (org-level-3             (:foreground vermilion4 :bold nil))
   (org-level-4             (:foreground cerise4 :bold nil))
   (org-code                (:foreground teal1))
   (org-verbatim            (:foreground orange2))
   (org-table               (:foreground ultramarine4 :bold bold))

   (js2-object-property     (:foreground brown1))
   (evil-ex-info            (:foreground crimson4)))

  (when kaolin-themes-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-light
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-mod :foreground ,diff-mod))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))

;;; kaolin-light-theme.el ends here
