;;; kaolin-valley-light-theme.el --- light variant of Kaolin-valley-dark theme.
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(defgroup kaolin-valley-light nil
  "Kaolin valley light theme options."
  :group 'kaolin-themes)

(defcustom kaolin-valley-light-alt-bg nil
  "Use white background color."
  :type 'boolean
  :group 'kaolin-valley-light)

(define-kaolin-theme valley-light  "Light variant of kaolin-valley-dark theme."

  ;; Palette modification
  (
   ;; Colors
   (spring-green6 "#3e594e")
   (aquamarine4   "#518270")
   (orange0       "#d1832e")
   (orange3       "#F3AE6C")
   ;; (cerulean4  "#536a9d")
   (cerulean4     "#47629E")
   (ultramarine1  "#744DF7")

   (azure1     "#0070CC")
   (teal0      "#0D7A75")
   (capri1     "#0F79BF")
   (harlequin1 "#417E2A")
   (harlequin3 "#4CDE0D")
   (crimson3   "#EE4970")
   (amber3     "#F3CB41")
   (harlequin2 "#2C820D")
   ;; (erin2      "#038217")
   (erin2      "#18803A")
   (lime2      "#5B7709")

   ;; Color vars
   (bg0 "#FDF7EE")
   (bg1 (if kaolin-valley-light-alt-bg "#FBFBFB" "#FAF2E9"))
   (bg2 (if kaolin-valley-light-alt-bg white0 "#F3E7D3"))
   (bg3 (if kaolin-valley-light-alt-bg white1 "#F0DFCA"))
   (bg4 (if kaolin-valley-light-alt-bg white2 "#EBD7BE"))

   (pane orange9)

   ;; TODO
   (fg1 "#5E5854")
   (fg2 "#6b6560")
   (fg3 "#79716c")
   (fg4 "#867e78")

   (keyword     teal0)
   (builtin     erin2)

   (var         crimson0)
   (const       crimson0)
   (functions   capri1)
   (type        orange0)

   (comment     brown8)
   (comment-alt teal7)

   (str         ultramarine1)
   (str-alt     ultramarine4)
   (doc         str-alt)

   (prep        vermilion0)
   (num         vermilion0)
   (bool        num)
   (warning     orange0)
   (err         red3)

   (dim-buffer white0)
   (hl         azure1)
   (hl-line    (if kaolin-themes-hl-line-colored green9 bg3))
   (hl-indent  white4)
   (selection green9)
   (pulse bg4)

   (todo red3)
   (done erin2)

   (tooltip-fg fg3)
   (tooltip-hl-bg bg3)
   (tooltip-hl-fg hl)


    ;; TODO:
   (rb1 teal1)
   (rb2 aquamarine1)
   (rb3 violet4)
   (rb4 ultramarine4)
   (rb5 vermilion4)
   (rb6 brown3)
   (rb7 capri4)
   (rb8 magenta3)
   (rb9 yellow3)

   (diff-add spring-green3)
   (diff-mod vermilion3)
   (diff-rem red3)

    ;; Mode-line
   (line-fg           fg4)
   (line-color2       brown4)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-border       (if kaolin-themes-modeline-border bg3 line-bg1))

   ;; Telephone-line
   (segment-active    gray2)
   (segment-inactive  gray2)

   (win-border    bg3)
   (line-num-fg   brown8)
   (line-num-hl   amber1)

   (cursor       gray3)

   (ivy1          gray9)
   (search1       cerise0)
   (search2       amber0)
   (search3       red3))

  (

   ;; TODO:
   (highlight-quoted-symbol  (:foreground builtin))

   ;; (org-level-1            (:foreground teal0 :bold bold :height 1.1))
   ;; (org-level-2            (:foreground violet4  :bold nil))
   ;; (org-level-3            (:foreground harlequin3 :bold nil))
   ;; (org-level-4            (:foreground vermilion4 :bold nil))
   (org-code               (:foreground keyword))
   (org-verbatim           (:foreground orange2))
   (org-date               (:foreground erin2 :underline kaolin-themes-underline)))

  (when kaolin-themes-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-valley-light
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-mod :foreground ,diff-mod))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))

;;; kaolin-valley-light-theme.el ends here
