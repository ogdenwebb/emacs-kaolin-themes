;;; kaolin-eclipse-theme.el --- Dark purple Kaolin theme variant
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(define-kaolin-theme eclipse "Dark purple Kaolin theme variant."
  ;; Palette modification
  ((bg1  magenta5 black1)
   (bg2       "#261a26" black2)
   (bg3       "#312231" black3)
   (bg4       "#3d2a3d" black4)

   (azure2 "#325074")

   (keyword     cerise4)
   (second-key  magenta2 cerise4)
   (builtin     magenta3)
   (functions   builtin)
   ;; TODO:
   (var         violet4)
   (const       violet4)
   (type        aquamarine4)
   (num         amber3)
   (bool        num)
   ;; (prep        azure4 "#8787f5")
   ;; (prep        azure4 "#5f5faf")
   (prep        vermilion4)

   (comment     gray0)
   (alt-comment "#4c344c")
   (str         teal4)
   (str-alt     blue4)
   (doc         str-alt)
   (warning     orange3)
   (err         red1)

   (dim-buffer "#140E14")
   (hl         pink1)
   (hl-line    (if kaolin-themes-hl-line-colored bg2 black1))
   (hl-indent  bg4)
   (selection  bg4)
   (pulse      magenta2)

   (todo pink1)

   (tooltip-hl-bg magenta2)
   (tooltip-hl-fg amber3)

   (ivy2 capri0)
   (ivy3 spring-green1)
   (ivy4 ultramarine3)

   (rb1 crimson4)
   (rb2 violet4)
   (rb3 teal4)
   (rb4 blue4)
   (rb5 violet4)
   (rb6 violet3)
   (rb7 orange8)
   (rb8 azure2)
   (rb9 pink3)

   (diff-add teal3)
   (diff-mod violet3)
   (diff-rem crimson3)

   ;; Mode-line
   (line-fg           fg4)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-border       bg3)

   (segment-active    gray2)
   (segment-inactive  gray2)
   (evil-normal       teal1)
   (evil-insert       spring-green1)
   (evil-visual       orange1)
   (evil-replace      red1)
   (evil-motion       yellow1)
   (evil-operator     evil-normal)
   (evil-emacs        amber3)

   (win-border    black3)
   (line-num-fg   magenta2 black4)
   ;; TOOD: or hl
   (line-num-hl   magenta3 gray9)
   (cursor        "#e0c3c8"))

  ;; Custom theme set faces
  (
   (link                (:foreground aquamarine4 :underline underline))
   (show-paren-mismatch (:background bg2 :foreground red0))

   ;; TODO: change
   (org-code            (:foreground teal1))
   (org-verbatim        (:foreground amber3))
   (org-quote           (:foreground magenta3)))

  ;; Set custom vars
  (when kaolin-themes-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-eclipse
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-mod :foreground ,diff-mod))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))


;;; kaolin-eclipse-theme.el ends here
