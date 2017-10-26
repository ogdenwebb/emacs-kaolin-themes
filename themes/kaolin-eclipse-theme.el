;;; kaolin-eclipse-theme.el --- Dark purple Kaolin theme variant
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(define-kaolin-theme eclipse "Dark purple Kaolin theme variant."
  ;; Palette modification
  ((bg1  midnight-purple)
   ;; (bg2  "#291c29")
   ;; (bg4  "#473147")
   (bg2       "#261a26")
   (bg3       "#312231")
   (bg4       "#3d2a3d")
   (violet    "#a78db5")
   (dark-blue "#325074")

   ;; TODO: make violet a bit more bright/contrast

   ;; violet or lavender or purple, alt-purple, moderate-pink
   (keyword     alt-purple)
   (second-key  dark-purple)
   ;; TODO:
   (var         alt-orange)
   (const       lavender)
   (builtin     light-purple)
   (comment     black4)
   (alt-comment "#4c344c")
   (functions   builtin)
   (str         teal)
   (str-alt     faded-blue)
   (doc         str-alt)
   ;; TODO
   (type        light-jade)
   (num         wheat)
   (bool        num)
   (prep        moderate-blue)
   (warning     orange)
   (err         red)

   (dim-buffer alt-black)
   ;; TODO: soft blue or wheat or light-orange
   (hl         pink)
   (hl-line    (if kaolin-hl-line-colored bg2 black1))
   (hl-indent  gray)
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

    ;; TODO:
   (rb1 teal)
   (rb2 violet)
   (rb3 jade)
   (rb4 faded-blue)
   (rb5 green)
   (rb6 light-violet)
   (rb7 grayish-orange)
   (rb8 grayish-magenta)
   (rb9 lavender)

   (diff-add    light-jade)
   (diff-change purple)
   (diff-del    faded-red)

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
   (line-num-fg   dark-purple)
   ;; TOOD: or hl
   (line-num-hl   violet)
   (cursor        "#e0c3c8"))

  ;; Custom theme set faces
  (
   (link                (:foreground light-jade :underline underline))

   (org-code            (:foreground green))
   (org-verbatim        (:foreground wheat))
   (org-quote           (:foreground violet))

   (git-gutter:added    (:background diff-add :foreground diff-add))
   (git-gutter:modified (:background diff-change :foreground diff-change))
   (git-gutter:deleted  (:background diff-del :foreground diff-del)))

  ;; Set custom vars
  (custom-theme-set-variables
   'kaolin-eclipse
   '(kaolin-hl-line-colored t)))


;;; kaolin-eclipse-theme.el ends here
