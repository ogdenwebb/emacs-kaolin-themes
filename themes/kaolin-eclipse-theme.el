;;; kaolin-eclipse-theme.el --- Dark purple Kaolin theme variant
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(define-kaolin-theme eclipse "Dark purple Kaolin theme variant."
  ;; Palette modification
  ((bg1  midnight-purple black1)
   ;; TODO:
   ; (bg1  "#1C101C" black1)
   ;; (bg2  "#291c29")
   ;; (bg4  "#473147")
   (bg2       "#261a26" black2)
   (bg3       "#312231" black3)
   (bg4       "#3d2a3d" black4)
   (violet    "#a78db5")
   (dark-blue "#325074")

   (keyword     alt-purple)
   (second-key  dark-purple alt-purple)
   (builtin     light-purple)
   (functions   builtin)
   ;; TODO:
   (var         alt-lavender)
   (const       lavender)
   (type        aquamarine4)
   (num         amber3)
   (bool        num)
   ;; (prep        moderate-blue "#8787f5")
   ;; (prep        moderate-blue "#5f5faf")
   (prep        vermilion4)

   (comment     gray0)
   (alt-comment "#4c344c")
   (str         cyan3)
   (str-alt     faded-blue)
   (doc         str-alt)
   (warning     orange3)
   (err         red1)

   (dim-buffer "#140E14")
   (hl         pink1)
   (hl-line    (if kaolin-hl-line-colored bg2 black1))
   (hl-indent  bg4)
   (selection  bg4)
   (pulse      dark-purple)

   (todo pink1)

   (tooltip-bg    bg2)
   (tooltip-fg    gray9)
   (tooltip-hl-bg dark-purple)
   (tooltip-hl-fg amber3)

   (ivy3 spring-green3)
   (ivy4 moderate-blue)

   (rb1 crimson4)
   (rb2 lavender)
   (rb3 cyan3)
   (rb4 faded-blue)
   (rb5 alt-lavender)
   (rb6 light-violet)
   (rb7 grayish-orange)
   (rb8 dark-blue)
   (rb9 pink3)

   (diff-add    aquamarine4)
   (diff-change purple)
   (diff-rem    red4)

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
   (line-num-bg   bg1)
   (line-num-fg   dark-purple black4)
   ;; TOOD: or hl
   (line-num-hl   violet gray9)
   (cursor        "#e0c3c8"))

  ;; Custom theme set faces
  (
   (link                (:foreground aquamarine4 :underline underline))
   (show-paren-mismatch (:background bg2 :foreground red0))

   ;; TODO: change
   (org-code            (:foreground teal1))
   (org-verbatim        (:foreground amber3))
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
