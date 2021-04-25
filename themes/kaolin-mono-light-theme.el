;;; kaolin-mono-light-theme.el --- almost monochrome light Kaolin theme
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(defgroup kaolin-mono-light nil
  "Kaolin light theme options."
  :group 'kaolin-themes)

(defcustom kaolin-mono-light-alt-bg nil
  "Use more pure white background color."
  :type 'boolean
  :group 'kaolin-light)

(define-kaolin-theme mono-light
  "Almost monochrome light Kaolin theme."

  ;; Palette modification
  (
   ;; Colors
   (spring-green6 "#3e594e")
   (orange0       "#d1832e")

   (cyan1         "#48a9a9")
   ;; (cyan2         "#007B91")
   (cyan2         "#03878C")
   (cyan3         "#6facb3")
   (teal1         "#098574")
   (teal2         "#1D6B64")
   (spring-green2 "#317A56")
   (vermilion3    "#E36B3F")
   (aquamarine1   "#47ba99")
   (aquamarine4   "#518270")
   (azure4        "#4C7A90")
   ;; TODO: less contrast
   (ultramarine1  "#6D46E3")
   (orange1       "#C5882C")

   ;; Color vars
   (bg0 "#f5f6f5")
   ;; (bg1 (if kaolin-mono-light-alt-bg "#FBFBFB" "#E9F3F3"))
   (bg1 (if kaolin-mono-light-alt-bg "#FBFBFB" "#F1F3E9"))
   (bg2 (if kaolin-mono-light-alt-bg white0 "#E5E6DE"))
   (bg3 (if kaolin-mono-light-alt-bg white1 "#DCDED1"))
   (bg4 (if kaolin-mono-light-alt-bg white2 "#C4CCBA"))

   ;; (fg1 gray1)
   ;; (fg1 "#3D4647")
   (fg1 "#4E4757")
   (fg2 gray2)
   (fg3 gray3)
   (fg4 gray4)

   ;; Root colors
   (kaolin-black   fg1)
   (kaolin-red     red1)
   (kaolin-green   spring-green2)
   (kaolin-yellow  orange0)
   (kaolin-blue    azure1)
   (kaolin-magenta magenta1)
   (kaolin-cyan    cyan1)
   (kaolin-white   bg4)

   (comment     lime7)
   (comment-alt orange2)
   (comment-contrast lime6)

   ;; (keyword     aquamarine2)
   ;; (keyword     spring-green2)
   ;; (keyword     "#537804")
   (keyword     "#5E7801")
   (metakey     (if kaolin-themes-distinct-metakeys spring-green2 comment))
   ;; (var         chartreuse2)
   ;; (const       chartreuse2)
   (var         erin2)
   (const       erin2)
   (builtin     aquamarine2)
   (functions   aquamarine2)
   ;; (builtin     cyan2)
   ;; (functions   cyan2)
   ;; ;; (str         brown1)
   (str         green2)
   (str-alt     green2)
   (doc         str-alt)
   ;; (type        lime4)
   ;; (type        yellow2)
   ;; (type        cyan2)
   ;; (type        yellow2)
   (type        teal2)
   (num         cyan2)
   (num         green2)
   (bool        num)
   ;; (prep        harlequin4)
   ;; (prep        yellow2)
   ;; (prep        teal1)
   (prep        chartreuse2)
   (warning     orange1)
   (err         crimson3)

   (dim-buffer white0)
   ;; (hl         teal2)
   ;; (hl         erin2)
   (hl         vermilion1)
   ;; TODO: add colored
   (hl-line    (if kaolin-themes-hl-line-colored bg2 bg2))
   (hl-indent  gray9)
   (selection  teal9)
   (pulse      teal8)

   (done erin2)
   (todo crimson0)

   (tooltip-hl-bg bg3)
   (tooltip-hl-fg hl)

    ;; TODO: change
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

   (org-level-1             (:foreground spring-green2 :bold bold :height kaolin-org-heading-size))
   (org-level-2             (:foreground functions :bold nil))
   (org-level-3             (:foreground keyword :bold nil))
   (org-level-4             (:foreground warning :bold nil))
   (org-code                (:foreground orange0))
   (org-date                (:foreground azure4))
   (org-verbatim            (:foreground azure1))

   (js2-object-property     (:foreground brown1))
   (evil-ex-info            (:foreground crimson4))
   )

  (when kaolin-themes-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-mono-light
     `(show-paren-match     ((t (:background nil :foreground ,red0 :bold bold))))
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-mod :foreground ,diff-mod))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))

;;; kaolin-mono-light-theme.el ends here
