;;; kaolin-valley-light-theme.el --- light variant of Kaolin-valley-dark theme.
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(define-kaolin-theme valley-light  "Light variant of kaolin-valley-dark theme."

  ;; Palette modification
  (
   ;; Colors
   (spring-green6 "#3e594e")
   (aquamarine4   "#518270")
   (orange0       "#d1832e")
   ;; (cerulean4     "#536a9d")
   (cerulean4     "#47629E")

   ;; (teal0 "#0d948d")
   (teal0 "#10948D")
   (cyan3 "#2BC8CC")
   ;; (harlequin3 "#91f368")
   (harlequin3 "#4CDE0D")
   (crimson3 "#EF4F75")
   (amber3 "#F3CB41")

   ;; Color vars
   (bg1 "#F9F5F1")
   (bg2 "#F8F0E9")
   (bg3 "#F0E8E0")
   (bg4 "#EBE3DD")

   (fg1 black4)

   (keyword     teal0)
   (second-key  keyword)
   (builtin     aquamarine1)

   (var         crimson3)
   (const       crimson3)
   ;; TODO:
   ;; (functions   crimson3)
   (functions   cyan3)
   (type        amber3)

   (comment     brown8)
   (alt-comment azure8)

   ;; (str         magenta3)
   ;; (str-alt     violet4)
   (str         violet3)
   (str-alt     ultramarine4)
   (doc         str-alt)

   (prep        vermilion3)
   ;; TODO:
   (num         harlequin3)
   (bool        num)
   (warning     orange1)
   (err         red3)

   (dim-buffer white0)
   (hl         azure3)
   ;; TODO: add colored
   (hl-line    (if kaolin-hl-line-colored bg3 bg3))
   (hl-indent  white4)
   ;; TODO:
   (selection bg4)
   ;; TODO:
   (pulse bg4)

   (todo red3)

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
   (line-fg           gray9)
   (line-color2       brown4)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-border       bg3)

   (prompt aquamarine1)

   ;; Telephone-line
   (segment-active    gray2)
   (segment-inactive  gray2)
   (evil-normal       brown3)
   (evil-insert       harlequin3)
   (evil-visual       orange3)
   (evil-replace      red3)
   (evil-motion       amber3)
   (evil-operator     evil-normal)
   (evil-emacs        cyan3)

   (win-border    bg3)
   (line-num-bg   bg1)
   (line-num-fg   brown8)
   (line-num-hl   amber3)

   (cursor       gray5)

   (ivy1          gray9)
   (ivy2          purple3)
   (ivy3          yellow0)
   (ivy4          red3))

  (

   ;; TODO:
   ;; (highlight-quoted-quote   (:foreground orange1))
   ;; (highlight-quoted-symbol  (:foreground type))
   (highlight-quoted-symbol  (:foreground harlequin1))

   ;; (org-level-1            (:foreground teal0 :bold bold :height 1.1))
   ;; (org-level-2            (:foreground violet4  :bold nil))
   ;; (org-level-3            (:foreground harlequin3 :bold nil))
   ;; (org-level-4            (:foreground vermilion4 :bold nil))
   (org-code               (:foreground teal1))
   (org-verbatim           (:foreground orange2))
   (org-table              (:foreground ultramarine4 :bold bold)))

  (when kaolin-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-valley-dark
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-mod :foreground ,diff-mod))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))

;;; kaolin-valley-light-theme.el ends here
