;;; kaolin-shiva-theme.el --- Kaolin theme with autumn colors and melanzane background
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(defgroup kaolin-shiva nil
  "Kaolin shiva theme options."
  :group 'kaolin-themes)

(defcustom kaolin-shiva-alt-bg nil
  "Use alternative brighter background."
  :type 'boolean
  :group 'kaolin-shiva)

(defcustom kaolin-shiva-distinct-prep t
  "Use distinct preprocessor face."
  :type 'boolean
  :group 'kaolin-shiva)

(define-kaolin-theme shiva "Kaolin theme with autumn colors and melanzane background"
  ;; Palette modification
  (
   ;; Colors
   (spring-green6 "#3e594e")
   (aquamarine3   "#53E6B5")
   (aquamarine4   "#518270")
   ;; (cerulean4     "#536a9d")
   (cerulean4     "#47629E")
   ;; (shiva-cyan    "#A1DDD7")
   (shiva-cyan    "#A1DDD7")

   ;; Color vars
   ;;                                  bright    dark      terminal
   ;; (bg0 "#1C1616")
   (bg0 "#1f181d")
   ;; bg1 #28211E
   ;; MAYBE: more brown bg (at least as alt-bg)?
   ;; (bg1 "#2a2028")
   (bg1 (if kaolin-shiva-alt-bg "#33242F" "#2a2028"))
   (bg2 "#382b36")
   (bg3 "#473644")
   (bg4 "#554151")

   (fg1 "#fcefe6")

   ;; TODO: implement
   (kaolin-black   bg1)
   (kaolin-red     red3)
   (kaolin-green   harlequin3)
   (kaolin-yellow  amber3)
   (kaolin-blue    capri3)
   (kaolin-magenta magenta3)
   (kaolin-cyan    cyan3)
   (kaolin-white   fg1)

   (keyword     red4)
   ;; (builtin     orange3)
   (builtin     "#feb193")
   (header      orange3)

   (var         magenta3)
   (const       magenta3)
   (functions   shiva-cyan)
   (type        amber3)

   (comment    crimson7)
   (comment-alt lime7)
   (comment-contrast orange7)

   (str         blue8)
   (str-alt     ultramarine4)
   (doc         str-alt)

   (prep        (if kaolin-shiva-distinct-prep spring-green8 keyword))
   (num         str)
   (bool        num)
   (warning     amber0)
   ;; (warning     yellow3)
   (err         red3)

   (dim-buffer white0)
   ;; TODO: find a better color
   (hl         aquamarine0)
   ;; TODO: add colored
   (hl-line    (if kaolin-themes-hl-line-colored bg3 bg3))
   (hl-indent  "#453947")
   (selection purple6)
   (pulse bg4)

   (todo red3)

   (tooltip-hl-bg bg3)
   (tooltip-hl-fg hl)

   (rb1 red4)
   (rb2 orange4)
   (rb3 violet4)
   (rb4 vermilion4)
   (rb5 teal4)
   (rb6 amber3)
   (rb7 magenta3)
   (rb8 brown3)
   (rb9 crimson3)

   (diff-add teal4)
   (diff-mod vermilion4)
   (diff-rem red1)

    ;; Mode-line
   (line-fg           fg3)
   (line-color2       str)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-border       (if kaolin-themes-modeline-border bg4 line-bg1))

   ;; (prompt aquamarine1)

   ;; Telephone-line
   (segment-active    gray2)
   (segment-inactive  gray2)

   (win-border    bg3)
   (evil-normal   amber3)
   (line-num-fg   comment)
   (line-num-hl   magenta3)

   (cursor        fg1)

   (ivy1          fg4)
   (search1          cerulean3)
   (search2          yellow0)
   (search3          red3))

  (
   (highlight-quoted-symbol  (:foreground prep))

   (org-document-title     (:foreground header :bold bold))

   ;; TODO:
   (org-level-1            (:foreground keyword :bold bold :height kaolin-org-heading-size))
   (org-level-2            (:foreground functions :bold nil))
   (org-level-3            (:foreground str :bold nil))
   (org-level-4            (:foreground builtin :bold nil))
   (org-date               (:foreground aquamarine3 :underline underline))
   (org-code               (:foreground num))
   (org-verbatim           (:foreground orange3))
   )

  (when kaolin-themes-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-shiva
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-mod :foreground ,diff-mod))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))

;;; kaolin-shiva-theme.el ends here
