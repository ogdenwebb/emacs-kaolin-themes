;;; kaolin-eclipse-theme.el --- Dark purple Kaolin theme variant
;;; Commentary:

;;; Code:
(require 'kaolin-theme)

(define-kaolin-theme eclipse  "Dark purple Kaolin theme variant."
  ;; Palette modification
  ((bg1  midnight-purple)
   ;; (bg2  "#291c29")
   ;; (bg3  "#382738")
   ;; (bg4  "#473147")
   (bg2  "#261a26")
   (bg3  "#312231")
   (bg4  "#3d2a3d")

   ;; violet or lavender or purple, alt-purple, moderate-pink
   (keyword     alt-purple)
   ;; TODO:
   (var         alt-orange)
   (const       lavender)
   (builtin     violet)
   (comment     gray)

   (alt-comment alt-grayish-blue)
   (functions   violet)
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
   (hl         light-orange)
   ;; TODO:
   (hl-line    (if kaolin-hl-line-colored bg2 bg2))
   (hl-indent  gray)

   (tooltip-bg bg2)
   (tooltip-fg light-gray)
   (tooltip-hl-bg alt-brown)
   (tooltip-hl-fg light-orange)

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

   (diff-add    green)
   (diff-change purple)
   (diff-del    dark-red)

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
   (cursor        alt-white))

  ()

  (custom-theme-set-faces
   'kaolin-eclipse
   `(git-gutter:added    ((t (:background ,diff-add :foreground ,diff-add))))
   `(git-gutter:modified ((t (:background ,diff-change :foreground ,diff-change))))
   `(git-gutter:deleted  ((t (:background ,diff-del :foreground ,diff-del))))))


;;; kaolin-eclipse-theme.el ends here
