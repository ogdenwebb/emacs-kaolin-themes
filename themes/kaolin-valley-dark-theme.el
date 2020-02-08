;;; kaolin-valley-dark-theme.el --- Colorful Kaolin theme with brown background.
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(defgroup kaolin-valley-dark nil
  "Kaolin valley dark theme options."
  :group 'kaolin-themes)

(defcustom kaolin-valley-dark-alt-bg nil
  "Use alternative brighter background."
  :type 'boolean
  :group 'kaolin-valley-dark)

(define-kaolin-theme valley-dark  "Colorful Kaolin theme with dark brown background."
  ;; Palette modification
  (
   ;; Colors
   (spring-green6 "#3e594e")
   (aquamarine3   "#53E6B5")
   (aquamarine4   "#518270")
   ;; (cerulean4     "#536a9d")
   (cerulean4     "#47629E")

   ;; Color vars
   ;;                                  bright    dark      terminal
   (bg0 "#1C1616")
   ;; (bg1 (if kaolin-valley-dark-alt-bg "#28211E" "#211F1D") black1)
   ;; (bg2 (if kaolin-valley-dark-alt-bg "#332a25" "#282423") black2)
   ;; (bg3 (if kaolin-valley-dark-alt-bg "#372d28" "#2E2828") black3)
   ;; (bg4 (if kaolin-valley-dark-alt-bg "#3f342d" "#352D2D") black4)
   (bg1 (if kaolin-valley-dark-alt-bg "#28211E" "#262221") black1)
   (bg2 (if kaolin-valley-dark-alt-bg "#332a25" "#2E2A29") black2)
   (bg3 (if kaolin-valley-dark-alt-bg "#372d28" "#332E2E") black3)
   (bg4 (if kaolin-valley-dark-alt-bg "#3f342d" "#383030") black4)

   ;; (pane "#262122")

   (fg1 amber9)

   ;; (keyword     teal0)
   (keyword     teal3)
   (builtin     aquamarine3)
   (header      orange3)

   (var         crimson3)
   (const       crimson3)
   ;; (functions   cyan3)
   (functions   "#7ED7E6")
   (type        amber3)

   ;; (comment     brown2)
   (comment "#635C4A")
   ;; TODO:
   ;; (comment     "#553a41")
   ;; TODO:
   (comment-alt teal2)

   (str         magenta3)
   (str-alt     cerise4)
   (doc         str-alt)

   (prep        ultramarine3)
   (num         harlequin3)
   (bool        num)
   (warning     amber0)
   (err         red3)

   (dim-buffer white0)
   (hl         vermilion3)
   ;; TODO: add colored
   (hl-line    (if kaolin-themes-hl-line-colored bg3 bg3))
   (hl-indent  "#453947")
   ;; TODO: selected comments are almost invisible
   (selection amber6)
   (selection red6)
   ;; TODO:
   (pulse bg4)

   (todo red3)

   (tooltip-hl-bg bg3)
   (tooltip-hl-fg hl)

   (rb1 teal1)
   (rb2 aquamarine1)
   (rb3 violet4)
   (rb4 cyan1)
   (rb5 spring-green1)
   (rb6 amber3)
   (rb7 magenta3)
   (rb8 brown3)
   (rb9 crimson3)

   (diff-add spring-green1)
   (diff-mod vermilion3)
   (diff-rem red3)

    ;; Mode-line
   (line-fg           fg3)
   (line-color2       str)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-border       (if kaolin-themes-modeline-border bg4 line-bg1))

   (prompt aquamarine1)

   ;; Telephone-line
   (segment-active    gray2)
   (segment-inactive  gray2)

   (win-border    bg3)
   (line-num-fg   amber7)
   (line-num-hl   amber1)

   (cursor        fg1)

   (ivy1          fg4)
   (search1          cerulean3)
   (search2          yellow0)
   (search3          red3))

  (
   (highlight-quoted-symbol  (:foreground num))

   (org-document-title     (:foreground header :bold bold))
   ;; (org-document-info      (:foreground brown3))

   ;; TODO:
   (org-level-1            (:foreground keyword :bold bold :height 1.1))
   (org-level-2            (:foreground functions  :bold nil))
   (org-level-3            (:foreground str :bold nil))
   (org-level-4            (:foreground builtin :bold nil))
   (org-date               (:foreground aquamarine3 :underline underline))
   (org-code               (:foreground num))
   (org-verbatim           (:foreground orange3)))

  (when kaolin-themes-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-valley-dark
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-mod :foreground ,diff-mod))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))

;;; kaolin-valley-dark-theme.el ends here
