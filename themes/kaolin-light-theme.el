;;; kaolin-light-theme.el --- Light Kaolin theme variant
;;; Commentary:

;;; Code:
(require 'kaolin-theme)

(define-kaolin-theme light  "Light Kaolin theme variant."
  ;; Palette modification
  (

   ;; Color vars
   ;; grayish green
   (bg1 "#ccd0c8")
   (bg2 "#c2c7bd")
   (bg3 "#b8beb3")
   (bg4 "#afb5a8")
   ;; grayish blue
   ;; (bg1 "#c8ccd0")
   ;; origin
   ;; (bg1 white1)
   ;; (bg2 white2)
   ;; (bg3 white3)
   ;; (bg4 white4)
   ;; TODO: a more brighter
   (fg1 dark-jade)
   (fg2 black2)
   (fg3 black3)
   (fg4 black4)
   (green "#43797f")
   ;; (lime  "#85b654")
   ;; (lime  "#759c79")
   ;; (dark-cyan  "#6da198")
   (dark-cyan  "#6CA1A6")
   ;; (dark-cyan  "#79a3ae")
   ;; (pink       "#d24b83")
   (pink       "#cd3775")



   (keyword     jade)
   ;; TODO:
   (var         moderate-pink)
   (const       pink)
   (builtin     dark-cyan)
   (comment     white3)

   (alt-comment alt-grayish-blue)
   (functions   dark-cyan)
   (str         light-jade)
   (str-alt     faded-blue)
   (doc         str-alt)
   ;; TODO
   (type        light-jade)
   (num         faded-red)
   (bool        num)
   (prep        faded-orange)
   (warning     orange)
   (err         red)

   (dim-buffer alt-black)
   ;; TODO: soft blue or wheat or light-orange
   (hl         alt-purple)
   ;; TODO:
   (hl-line    (if kaolin-hl-line-colored bg2 white2))
   (hl-indent  gray)

   (todo red)

   (tooltip-bg bg2)
   (tooltip-fg light-gray)
   (tooltip-hl-bg alt-brown)
   (tooltip-hl-fg light-orange)

    ;; TODO:
   (rb1 green)
   (rb2 violet)
   (rb3 jade)
   (rb4 faded-blue)
   (rb5 green)
   (rb6 light-violet)
   (rb7 grayish-orange)
   (rb8 grayish-magenta)
   (rb9 lavender)

   (diff-add    light-jade)
   (diff-change faded-orange)
   (diff-del    faded-red)

    ;; Mode-line
   (line-fg           fg4)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-border       bg3)

   ;; Telephone-line
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
   (line-num-fg   alt-grayish-blue)
   (line-num-hl   hl)

   (cursor        gray))


  ;; TODO: Add option
  ((link                (:foreground soft-blue :underline underline))

   (org-code            (:foreground green))
   (org-verbatim        (:foreground wheat))

   (git-gutter:added    (:background diff-add :foreground diff-add))
   (git-gutter:modified (:background diff-change :foreground diff-change))
   (git-gutter:deleted  (:background diff-del :foreground diff-del))))



;;; kaolin-light-theme.el ends here
