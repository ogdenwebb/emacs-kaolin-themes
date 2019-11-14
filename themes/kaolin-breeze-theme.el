;;; kaolin-breeze-theme.el --- Light Kaolin theme with soft colors.
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(define-kaolin-theme breeze  "Light Kaolin theme with soft colors."

  ;; Palette modification
  ((spring-green6 "#3e594e")
   (aquamarine4   "#518270")
   (orange0       "#d1832e")

   (cyan1         "#48a9a9")
   (cyan2         "#008b8b")
   (cyan3         "#6facb3")

   (capri4 "#4F9CB8")
   (cerulean4 "#4C6190")
   (blue4 "#605DB3")

   ;; Color vars
   (bg0 "#F6F2EF")
   (bg1 "#EBE8E4")
   (bg2 "#DEDAD5")
   (bg3 "#D2CECA")
   (bg4 "#C9C2BD")

   (fg1 gray1)
   (fg2 gray2)
   (fg3 gray3)
   (fg4 gray4)

   (keyword     blue4)
   (builtin     cerulean4)

   (var         crimson3)
   (const       crimson3)
   (functions   brown1)
   (type        spring-green2)

   (comment     lime7)
   ;; (alt-comment "#ffaaaa")

   (str         magenta4)
   (str-alt     magenta2)
   (doc         str-alt)

   (prep        capri1)
   (num         capri1)
   (bool        num)
   (warning     vermilion3)
   (err         red1)

   (dim-buffer white0)
   (hl         cerise3)
   ;; TODO: add colored
   (hl-line    (if kaolin-hl-line-colored bg2 bg2))
   (hl-indent  gray9)
   ;; TODO:
   (selection bg4)
   ;; TODO:
   (pulse cyan3)

   (todo red3)
   (done erin2)

   (tooltip-hl-bg bg3)
   (tooltip-hl-fg hl)

    ;; TODO:
   (rb1 ultramarine4)
   (rb2 magenta4)
   (rb3 azure4)
   (rb4 ultramarine4)
   (rb5 teal1)
   (rb6 aquamarine2)
   (rb7 magenta4)
   (rb8 spring-green4)
   (rb9 crimson4)

   (diff-add spring-green1)
   (diff-mod vermilion3)
   (diff-rem red0)

    ;; Mode-line
   (line-fg           fg4)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-border       (if kaolin-themes-modeline-border bg3 line-bg1))

   ;; Telephone-line
   (segment-active    gray2)
   (segment-inactive  gray2)

   (win-border    bg3)
   (line-num-bg   bg1)
   (line-num-fg   chartreuse8)
   (line-num-hl   ultramarine2)

   (cursor        gray3)

   (ivy1          gray9)
   (search1          capri1)
   (search2          orange0)
   (search3          red4))

  ((link                   (:foreground capri1 :underline underline))

   (org-level-1           (:foreground keyword :bold bold :height 1.1))
   (org-level-2            (:foreground str :bold nil))
   (org-level-3            (:foreground builtin :bold nil))
   (org-level-4            (:foreground functions :bold nil))
   (org-code               (:foreground keyword))
   (org-verbatim           (:foreground orange2))
   (org-list-dt            (:foreground str))
   (org-checkbox           (:foreground str))
   (org-table              (:foreground ultramarine2))
   (org-date               (:foreground vermilion3))

   (js2-object-property          (:foreground functions))
   (evil-ex-info                 (:foreground crimson4)))


  (when kaolin-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-breeze
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-mod :foreground ,diff-mod))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))

;;; kaolin-breeze-theme ends here
