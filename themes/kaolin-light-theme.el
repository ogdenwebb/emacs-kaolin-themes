;;; kaolin-light-theme.el --- Light Kaolin theme variant
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(defgroup kaolin-light nil
  "Kaolin light theme options."
  :group 'kaolin-themes)

(defcustom kaolin-light-alt-bg nil
  "Use more pure white background color."
  :type 'boolean
  :group 'kaolin-light)

(define-kaolin-theme light
  "Light Kaolin theme variant."

  ;; Palette modification
  (

   ;; Colors
   (spring-green6 "#3e594e")
   (aquamarine4   "#518270")
   (orange0       "#d1832e")

   (cyan1         "#48a9a9")
   (cyan2         "#008b8b")
   (cyan3         "#6facb3")
   (teal2         "#1D6B69")
   (spring-green2 "#317A56")
   (vermilion3    "#E36B3F")
   (aquamarine1   "#47ba99")
   (azure4        "#4C7A90")
   ;; TODO: less contrast
   (ultramarine1  "#6D46E3")
   (orange1       "#C5882C")

   ;; Color vars
   (bg0 "#f5f6f5")
   (bg1 (if kaolin-light-alt-bg "#FBFBFB" "#EDEEEB"))
   (bg2 (if kaolin-light-alt-bg white0 "#DFE1DC"))
   (bg3 (if kaolin-light-alt-bg white1 "#D1D4CD"))
   (bg4 (if kaolin-light-alt-bg white2 "#C8CCC3"))

   (fg1 gray1)
   (fg2 gray2)
   (fg3 gray3)
   (fg4 gray4)

   (comment     gray7)
   (comment-alt lime7)
   (keyword     teal2)
   (metakey     (if kaolin-themes-distinct-metakeys teal2 comment))
   (var         magenta4)
   (const       magenta4)
   (builtin     azure4)
   (functions   azure4)
   ;; (str         brown1)
   (str         erin2)
   (str-alt     brown3)
   (doc         str-alt)
   (type        vermilion3)
   (num         red1)
   (bool        num)
   (prep        ultramarine1)
   (warning     orange1)
   (err         crimson3)

   (dim-buffer white0)
   (hl         green2)
   ;; TODO: add colored
   (hl-line    (if kaolin-themes-hl-line-colored bg2 bg2))
   (hl-indent  gray9)
   (selection  azure9)
   (pulse      teal8)

   (done erin2)
   (todo crimson0)

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
   (line-border       (if kaolin-themes-modeline-border bg3 line-bg1))

   ;; Telephone-line
   (segment-active    gray2)
   (segment-inactive  gray2)

   (win-border    bg3)
   (line-num-fg   comment)
   (line-num-hl   hl)

   (cursor        gray3)

   (ivy1          gray9)
   ;; TODO:
   (search1          capri1)
   (search2          orange0)
   (search3          red4))

  ((link                    (:foreground prep :underline underline))

   (highlight-quoted-quote  (:foreground keyword))
   (highlight-quoted-symbol (:foreground const))

   (org-level-1             (:foreground spring-green2 :bold bold :height 1.1))
   (org-level-2             (:foreground functions :bold nil))
   (org-level-3             (:foreground vermilion3 :bold nil))
   (org-level-4             (:foreground cerise4 :bold nil))
   (org-code                (:foreground orange0))
   (org-date                (:foreground azure4))
   (org-verbatim            (:foreground azure1))

   (js2-object-property     (:foreground brown1))
   (evil-ex-info            (:foreground crimson4)))

  (when kaolin-themes-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-light
     `(show-paren-match     ((t (:background nil :foreground ,red0 :bold bold))))
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-mod :foreground ,diff-mod))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))

;;; kaolin-light-theme.el ends here
