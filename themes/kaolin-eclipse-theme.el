;;; kaolin-eclipse-theme.el --- Dark purple Kaolin theme variant
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(define-kaolin-theme eclipse "Dark purple Kaolin theme variant."
  ;; Palette modification
  ((bg1  midnight-purple black1)
   ;; (bg2  "#291c29")
   ;; (bg4  "#473147")
   (bg2       "#261a26" black2)
   (bg3       "#312231" black3)
   (bg4       "#3d2a3d" black4)
   (violet    "#a78db5")
   (dark-blue "#325074")

   ;; violet or lavender or purple, alt-purple, moderate-pink
   (keyword     alt-purple)
   (second-key  dark-purple alt-purple)
   (builtin     light-purple)
   (functions   builtin)
   ;; TODO:
   (var         faded-orange)
   (const       lavender)
   (type        light-jade)
   (num         wheat)
   (bool        num)
   ;; (prep        moderate-blue "#8787f5")
   ;; (prep        moderate-blue "#5f5faf")
   (prep        moderate-blue "#5f87af")

   (comment     black4)
   (alt-comment "#4c344c")
   (str         teal)
   (str-alt     faded-blue)
   (doc         str-alt)
   (warning     alt-orange)
   (err         red)

   (dim-buffer "#140E14")
   (hl         pink)
   (hl-line    (if kaolin-hl-line-colored bg2 black1))
   (hl-indent  bg4)
   (selection  bg4)
   (pulse      dark-purple)

   (todo pink)

   (tooltip-bg    bg2)
   (tooltip-fg    light-gray)
   ;; TODO:
   (tooltip-hl-bg dark-purple)
   (tooltip-hl-fg light-orange)

   (ivy3 teal-green)
   (ivy4 moderate-blue)

   (rb1 moderate-pink)
   (rb2 lavender)
   (rb3 teal)
   (rb4 faded-blue)
   (rb5 alt-lavender)
   (rb6 light-violet)
   (rb7 grayish-orange)
   (rb8 dark-blue)
   (rb9 soft-pink)

   (diff-add    light-jade)
   (diff-change purple)
   (diff-rem    faded-red)

   ;; Mode-line
   (line-fg           fg4)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-border       bg3)

   (segment-active    gray)
   (segment-inactive  gray)
   (evil-normal       green)
   (evil-insert       light-green)
   (evil-visual       orange)
   (evil-replace      red)
   (evil-motion       yellow)
   (evil-operator     evil-normal)
   (evil-emacs        light-yellow)

   (win-border    dark-gray)
   (line-num-bg   bg1)
   (line-num-fg   dark-purple black4)
   ;; TOOD: or hl
   (line-num-hl   violet light-gray)
   (cursor        "#e0c3c8"))

  ;; Custom theme set faces
  (
   (link                (:foreground light-jade :underline underline))
   (show-paren-mismatch (:background bg2 :foreground alt-red))

   ;; TODO: change
   (org-code            (:foreground green))
   (org-verbatim        (:foreground wheat))
   (org-quote           (:foreground violet)))

  ;; Set custom vars
  (custom-theme-set-variables
   'kaolin-eclipse
   '(kaolin-hl-line-colored t))

  (when kaolin-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-eclipse
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-change :foreground ,diff-change))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))


;;; kaolin-eclipse-theme.el ends here
