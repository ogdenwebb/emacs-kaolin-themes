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
  ((bg1 (if kaolin-ocean-alt-bg black2 blue5) black2)
   (bg2 (if kaolin-ocean-alt-bg black3 "#1d1d2b") black3)
   (bg3 (if kaolin-ocean-alt-bg black4 "#28283a") black4)
   (bg4 (if kaolin-ocean-alt-bg gray0 "#32324a") gray0)

   (keyword     azure3)
   (keyword     cerulean4)
   (metakey     cerise4 cerise4) ; todo
   (builtin     azure3)
   (functions   builtin)
   (var         violet3)
   (const       magenta3)
   (type        amber3)
   (prep        pink1)
   (num         pink1)
   (bool        num)

   (comment     gray2)
   (comment-alt "#43436E")
   ;; (str         amber3 "#ffd787")
   ;; (str-alt     vermilion4)
   ;; (str         teal4)
   ;; (str-alt     aquamarine2)
   (str         vermilion4)
   (str-alt     vermilion7)
   (doc         str-alt)
   (warning     orange1)
   (err         red1)

   (dim-buffer "#0F0F17")
   (hl         aquamarine0)
   (hl-line    (if kaolin-themes-hl-line-colored bg2 black1))
   (hl-indent  bg4)
   (selection  bg4)
   (pulse      bg4)

   (todo pink1)

   (tooltip-hl-bg bg4)

   (ivy2 cerise1)
   (ivy3 amber3)
   (ivy4 spring-green1)

   (rb1 cyan1)
   (rb2 violet4)
   (rb3 teal1)
   (rb4 blue4)
   (rb5 aquamarine2)
   (rb6 spring-green3)
   (rb7 amber1)
   (rb8 azure2)
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
   (line-num-fg   bg4)
   (line-num-hl   keyword)
   (cursor        "#c3c8e0"))

  (
   ;; Custom theme set faces
   (show-paren-mismatch (:background bg2 :foreground red0))

   (highlight-quoted-quote   (:foreground functions))
   (highlight-quoted-symbol  (:foreground type))

   (org-code            (:foreground pink1))
   (org-verbatim        (:foreground spring-green1))

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
