;;; kaolin-ocean-theme.el --- Dark blue Kaolin theme
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(defgroup kaolin-ocean nil
  "Kaolin valley dark theme options."
  :group 'kaolin-themes)

(defcustom kaolin-ocean-alt-bg nil
  "Use alternative dark gray background."
  :type 'boolean
  :group 'kaolin-ocean)

(define-kaolin-theme ocean "Dark blue Kaolin theme variant."
  ;; Palette modification
  (
   (bg0 (if kaolin-ocean-alt-bg black1 "#111119") black1)
   (bg1 (if kaolin-ocean-alt-bg black2 blue5) black2)
   (bg2 (if kaolin-ocean-alt-bg black3 "#1d1d2b") black3)
   (bg3 (if kaolin-ocean-alt-bg black4 "#28283a") black4)
   (bg4 (if kaolin-ocean-alt-bg gray0 "#32324a") gray0)

   (comment     gray3)
   ;; (comment     blue7)
   (comment-alt "#43436E")
   (keyword     cerulean4)
   (metakey     (if kaolin-themes-distinct-metakeys ultramarine3 comment)) ; todo
   (builtin     azure3)
   ;; (functions   builtin)
   (functions   cyan3)
   (var         violet3)
   (const       magenta3)
   (type        pink1)
   (prep        ultramarine3)
   (num         amber3)
   (bool        num)

   (keysym amber3)

   (str         green3)
   (str-alt     spring-green4)
   (doc         str-alt)
   (warning     orange1)
   (err         red3)

   (dim-buffer "#0F0F17")
   (hl         aquamarine0)
   (hl-line    bg2)
   ;; ; (hl-indent bg4)
   ;; (selection  bg4)
   (selection  aquamarine6)
   (pulse      bg4)

   (todo pink1)

   (tooltip-hl-bg bg4)

   (search1 vermilion3)
   (search2 teal0)
   (search3 yellow3)

   (rb1 blue4)
   (rb2 violet4)
   (rb3 teal1)
   (rb4 crimson4)
   (rb5 azure4)
   (rb6 spring-green4)
   (rb7 vermilion4)
   (rb8 capri4)
   (rb9 azure3)

   (diff-add spring-green1)
   (diff-mod orange1)
   (diff-rem red3)

   ;; Mode-line
   (line-fg           fg4)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-border       (if kaolin-themes-modeline-border bg3 line-bg1))

   (segment-active    gray3)
   (segment-inactive  gray3)

   (win-border    bg3)
   (line-num-fg   gray3)
   (line-num-hl   keyword))

  (
   (highlight-quoted-quote   (:foreground functions))
   (highlight-quoted-symbol  (:foreground amber3))

   (org-document-title  (:foreground str))
   (org-code            (:foreground pink1))
   (org-verbatim        (:foreground spring-green1))
   (org-level-2         (:foreground functions))

   (git-gutter:added    (:background diff-add :foreground diff-add))
   (git-gutter:modified (:background diff-mod :foreground diff-mod))
   (git-gutter:deleted  (:background diff-rem :foreground diff-rem)))

  ;; Set custom vars
  (when kaolin-themes-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-ocean
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-mod :foreground ,diff-mod))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))


;;; kaolin-ocean-theme.el ends here
