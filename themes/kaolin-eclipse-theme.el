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
   (azure2 "#325074")
   (purple3 "#C68EDE")

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

   (kaolin-black   bg1)
   (kaolin-red     red3)
   (kaolin-green   teal0)
   (kaolin-yellow  orange3)
   (kaolin-blue    cerulean4)
   (kaolin-magenta purple3)
   (kaolin-cyan    cyan3)
   (kaolin-white   fg1)

   (keyword     purple3)
   ;; (builtin     purple3)
   (builtin     pink3)
   (functions   builtin)
   (var         ultramarine3)
   (const       cerulean4)
   (type        cyan3)
   ;; (num         cerise3)
   (num         orange3)
   (bool        num)
   ;; (prep        amber3)
   (prep        capri3)
   ;; (prep        aquamarine3

   (comment     purple7)
   (comment-alt capri4)
   (comment-contrast purple8)

   (str         cerise3)
   (str-alt     blue4)
   (doc         str-alt)
   ;; (warning     orange3)
   (warning     amber0)
   (err         red3)

   ;; (hl         pink1)
   (hl         yellow3)
   (hl-line    violet6)
   ; (hl-indent bg4)
   (selection  capri6)
   (pulse      magenta2)

   (todo red3)

   (tooltip-hl-bg bg4)
   (tooltip-hl-fg orange3)

   (search1 capri0)
   (search2 spring-green1)
   (search3 amber3)

   ;; TODO revisit
   (rb1 crimson4)
   (rb2 violet4)
   (rb3 ultramarine3)
   (rb4 purple3)
   (rb5 teal4)
   (rb6 cerulean4)
   (rb7 teal1)
   (rb8 azure4)
   (rb9 vermilion4)

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
   (line-num-hl   purple3)
   (cursor        ultramarine3))

  ;; Custom theme set faces
  (
   ;; (show-paren-mismatch (:background bg2 :foreground red0))

   ;; (org-code            (:foreground teal3))
   ;; (org-verbatim        (:foreground capri3))
   (org-quote           (:foreground magenta3))
   )

  ;; Set custom vars
  (when kaolin-themes-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-eclipse
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-mod :foreground ,diff-mod))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))


;;; kaolin-eclipse-theme.el ends here
