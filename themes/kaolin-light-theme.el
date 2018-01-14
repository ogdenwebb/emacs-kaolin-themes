;;; kaolin-light-theme.el --- Light Kaolin theme variant
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(define-kaolin-theme light  "Light Kaolin theme variant."
  ;; Palette modification
  (
   ;; TODO: chartreuse1 color
   (azure1         "#5c9499")
   ;; (spring-green4         "#597a6e")
   (azure2    "#445588")
   (pink1         "#d45589")
   (spring-green5    "#3e594e")
   (aquamarine4   "#518270")
   (orange0  "#d1832e")
   (vermilion4 "#c78e6d")
   (red1          "#c86d6d")

   (cyan1         "#48a9a9")
   (cyan3         "#6facb3")
   (cyan2    "#008b8b")


   ;; Color vars
   (bg0 "#f5f6f5")
   (bg1 "#e9eae7")
   (bg2 "#dcded9")
   (bg3 "#cfd2cb")
   (bg4 "#c5c9c0")

   (fg1 spring-green5)
   (fg2 "#3e574d")
   (fg3 "#476257")
   (fg4 "#4f6e62")
   ;; (teal1 "#3f7d7f")
   (teal1 "#5e8475")


   (keyword     spring-green4)
   (second-key  spring-green4)
   (var         violet4)
   (const       blue4)
   (builtin     aquamarine4)
   ;; TODO:
   ;; (comment     "#9aa88c")
   (comment     "#9aa88c")
   (alt-comment azure5)
   (functions   builtin)
   ;; TODO: (??) change to brown1
   (str         brown1)
   (str-alt     brown1)
   (doc         str-alt)
   (type        vermilion4)
   (num         red1)
   (bool        num)
   (prep        num)
   (warning     orange1)
   (err         red1)

   (dim-buffer white0)
   (hl         aquamarine2)
   ;; TODO: add colored
   (hl-line    (if kaolin-hl-line-colored bg2 bg2))
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
   (rb4 blue3)
   (rb5 teal1)
   (rb6 crimson4)
   (rb7 vermilion4)
   (rb8 spring-green4)
   (rb9 violet4)

   (diff-add    aquamarine4)
   (diff-change vermilion4)
   (diff-rem    red4)

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
   (evil-replace      red1)
   (evil-motion       yellow1)
   (evil-operator     evil-normal)
   (evil-emacs        amber3)

   (win-border    bg3)
   (line-num-bg   bg1)
   (line-num-fg   grayish-green)
   (line-num-hl   hl)

   (cursor        gray3)

   (ivy1          gray9)
   (ivy2          capri1)
   (ivy3          orange0)
   (ivy4          red1))

  ((link                   (:foreground azure3 :underline underline))

   (org-level-1            (:foreground teal1 :bold bold :height 1.1))
   (org-level-2            (:foreground blue4 :bold nil))
   (org-level-3            (:foreground vermilion4 :bold nil))
   (org-level-4            (:foreground amber3 :bold nil))
   (org-code               (:foreground teal1))
   (org-verbatim           (:foreground orange2))
   (org-table              (:foreground azure4 :bold bold))

   (js2-object-property          (:foreground brown1))
   (evil-ex-info                 (:foreground crimson4)))


  (when kaolin-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-light
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-change :foreground ,diff-change))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))

;;; kaolin-light-theme.el ends here
