;;; kaolin-temple-theme.el --- Other Kaolin theme with dark brown background
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(define-kaolin-theme temple
  "The terrestrial sphere imbues my spirit."

  ;; Palette modification
  (
   ;; Colors
   (spring-green6 "#3e594e")
   ;; (aquamarine1   "#47BF9D")
   ;; (aquamarine4   "#518270")
   (aquamarine4   "#729C8C")
   (teal1         "#4FA19C")
   (orange3       "#f5c791")
   (crimson0      "#DC4473")
   (crimson7      "#BA667D")
   (vermilion4    "#D7936D")
   (cyan1         "#57B2C2")
   (cyan3         "#68DFE8")
   (red1          "#C74A4D")
   ;; (orange9       "#EEDEC6")
   (orange9       "#EEDCC1")

   ;; Color vars
   (bg1 black3)
   (bg2 black4)
   (bg3 gray0)
   (bg4 gray1)

   (fg1 orange9)

   (keyword     cyan1)
   (metakey     keyword)
   (builtin     capri4)
   (functions   capri4)

   (var         aquamarine4)
   ;; (const       cyan1)
   (const       teal1)
   ;; (type        orange3)
   (type        lime4)

   ;; (comment     gray5)
   (comment     cerulean7)
   (comment-alt vermilion7)

   (str         pink3)
   (str-alt     crimson7)
   (doc         str-alt)

   (prep        crimson3)
   (link        prep)
   (num         crimson3)
   (bool        num)
   (warning     orange1)
   (err         red3)

   (dim-buffer white0)
   (hl         aquamarine3)
   ;; TODO: add colored
   (hl-line    bg2)
   ;; (hl-indent  cerulean7)
   ;; TODO:
   (selection crimson6)
   ;; (selection ultramarine6)
   ;; TODO:
   (pulse bg4)

   (todo red1)
   (done aquamarine1)

   (tooltip-hl-bg bg3)
   (tooltip-hl-fg hl)

   (rb-match crimson3)
   (rb1 cyan1)
   (rb2 capri4)
   (rb3 teal1)
   (rb4 pink4)
   (rb5 blue4)
   (rb6 aquamarine4)
   (rb7 cyan1)
   (rb8 crimson7)
   (rb9 yellow3)

   ;; (diff-add spring-green3)
   ;; (diff-mod vermilion3)
   ;; (diff-rem red3)
   (diff-add teal1)
   (diff-mod vermilion4)
   (diff-rem red1)

    ;; Mode-line
   (line-fg           fg4)
   (line-color1       functions)
   (line-color2       str)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-border       bg2)

   ;; Telephone-line
   (segment-active    gray2)
   (segment-inactive  gray2)

   (win-border    bg3)
   (line-num-fg   comment)
   (line-num-hl   hl)

   (cursor        fg1)

   (ivy1          gray9)
   (search1       cerulean3)
   (search2       yellow0)
   (search3       green3))

  (
   (highlight-quoted-quote   (:foreground orange3))
   (highlight-quoted-symbol  (:foreground amber3))

   (org-document-title     (:foreground orange3 :bold bold))
   ;; (org-document-info      (:foreground brown3))
   (org-verbatim     (:foreground cyan1 :bold bold))

   (org-level-1            (:foreground var :bold bold :height 1.1))
   (org-level-2            (:foreground functions  :bold nil))
   (org-level-3            (:foreground str :bold nil))
   (org-level-4            (:foreground keyword :bold nil))
   (org-level-5            (:foreground prep :bold nil)))

  (when kaolin-themes-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-temple
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-mod :foreground ,diff-mod))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))

;;; kaolin-temple-theme.el ends here
