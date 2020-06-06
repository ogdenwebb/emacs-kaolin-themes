;;; kaolin-eclipse-theme.el --- Dark purple Kaolin theme variant
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(defgroup kaolin-eclipse nil
  "Kaolin eclipse theme options."
  :group 'kaolin-themes)

(defcustom kaolin-eclipse-alt-bg nil
  "Use alternative darker background color."
  :type 'boolean
  :group 'kaolin-eclipse)


(define-kaolin-theme eclipse "Dark purple Kaolin theme variant."
  ;; Palette modification
  (
   (bg0 (if kaolin-eclipse-alt-bg "#1E151E" "#261926") black0)
   ;; (bg1 (if kaolin-eclipse-alt-bg "#231923" "#2B1D2B") black1)
   ;; (bg2 (if kaolin-eclipse-alt-bg "#2B1F2B" "#261A26") black2)
   ;; (bg3 (if kaolin-eclipse-alt-bg "#332333" "#2B202B") black3)
   ;; (bg4 (if kaolin-eclipse-alt-bg "#3d2a3d" "#3d2a3d") black4)
   (bg1 (if kaolin-eclipse-alt-bg "#231923" "#2B1D2B") black1)
   (bg2 (if kaolin-eclipse-alt-bg "#2B1F2B" "#332433") black2)
   (bg3 (if kaolin-eclipse-alt-bg "#332333" "#3D293D") black3)
   (bg4 (if kaolin-eclipse-alt-bg "#3d2a3d" "#452F45") black4)

   (fg1 "#F0EBE7")

   (azure2 "#325074")
   (purple3 "#C68EDE")

   (keyword     purple3)
   (metakey     (if kaolin-themes-distinct-metakeys magenta2 cerise4) comment)
   ;; TODO: fix treemacs, imenu-list level1-2 and etc
   (builtin     purple3)
   (functions   builtin)
   ;; TODO:
   (var         ultramarine3)
   (const       ultramarine3)
   (type        cyan3)
   (num         cerise3)
   (bool        num)
   (prep        yellow4)

   (comment     purple7)
   (comment-alt "#663E66")
   (str         cerise3)
   (str-alt     blue4)
   (doc         str-alt)
   (warning     vermilion4)
   (err         red1)

   (dim-buffer "#140E14")
   (hl         pink1)
   ;; TODO: fix contrast in minibuffer
   (hl-line    violet6)
   ; (hl-indent bg4)
   (selection  capri6)
   (pulse      magenta2)

   (todo pink1)

   (tooltip-hl-bg cerulean6)
   (tooltip-hl-fg amber3)

   (search1 capri0)
   (search2 spring-green1)
   (search3 amber3)

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
   (diff-mod orange3)
   (diff-rem crimson3)

   ;; Mode-line
   (line-fg           fg4)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-border       (if kaolin-themes-modeline-border bg3 line-bg1))

   (segment-active    gray2)
   (segment-inactive  gray2)

   (win-border    bg3)
   (line-num-fg   comment)
   (line-num-hl   hl gray9)
   (cursor        ultramarine3))

  ;; Custom theme set faces
  (
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
