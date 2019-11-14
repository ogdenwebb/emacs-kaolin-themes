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
   (metakey     (if kaolin-themes-distinct-metakeys magenta2 cerise4) comment)
   (builtin     magenta3)
   (functions   builtin)
   ;; TODO:
   (var         violet4)
   (const       violet4)
   (type        aquamarine4)
   (num         amber3)
   (bool        num)
   (prep        vermilion4)

   ;; (comment     gray2)
   (comment     pink6)
   (comment-alt "#663E66")
   (str         teal4)
   (str-alt     blue4)
   (doc         str-alt)
   (warning     orange3)
   (err         red1)

   (dim-buffer "#140E14")
   (hl         pink1)
   (hl-line    (if kaolin-themes-hl-line-colored cerise6 bg3))
   ; (hl-indent bg4)
   (selection  bg4)
   (pulse      magenta2)

   (todo pink1)

   (tooltip-hl-bg magenta2)
   (tooltip-hl-fg amber3)

   (search1 capri0)
   (search2 spring-green1)
   (search3 ultramarine1)

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
   (line-border       (if kaolin-themes-modeline-border bg3 line-bg1))

   (segment-active    gray2)
   (segment-inactive  gray2)

   (win-border    bg3)
   ;; (line-num-fg   magenta2 black4)
   (line-num-fg   pink6 black4)
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
