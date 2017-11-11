;;; kaolin-galaxy-theme.el --- Bright theme based on
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(define-kaolin-theme galaxy "Bright theme based on"
  ;; Palette modification
  (
   ;; (bg1  "#18171c" black1)
   (bg1  "#1d1c21" black1)
   ;; (bg2  "#291c29")
   ;; (bg4  "#473147")
   (bg2          "#26252c" black2)
   (bg3          "#302e36" black3)
   (bg4          "#393741" black4)
   (violet       "#a78db5")
   (pink         "#e55c7a")
   (yellow       "#b9b963")
   (light-yellow "#eae46a")
   ;; (dark-blue "#223344")
   (dark-blue    "#232c35")
   ;; (blue      "#22aabb")
   (cyan         "#22aabb")
   (blue         "#2a57cc")
   ;; (blue      "#335599")
   (alt-orange  "#ab6448")
   (faded-red    "#9c3b42")
   (light-pink   "#cc7799")
   (lime         "#73c66c")
   (light-green  "#6dd797")

   ;; violet or lavender or purple, alt-purple, moderate-pink
   (keyword     light-violet)
   (second-key  "#642628" alt-purple)
   (builtin     violet)
   (functions   builtin)
   ;; (var         lime)
   ;; (const       light-green)
   (var         light-orange)
   (const       wheat)
   (type        green)
   ;; TODO adjust light-jade color(more bright)
   (num         light-jade)
   (bool        num)
   (prep        pink "#5f87af")

   ;; TODO:
   (comment     bg4)
   (alt-comment "#4c344c")
   ;; TODO: pink & dark-pink
   (str         teal-blue)
   ;; TOOD: faded/dark-pink
   (str-alt     grayish-blue)
   (doc         str-alt)
   (warning     alt-orange)
   (err         red)

   (dim-buffer "#140E14")
   ;; TODO: or soft/light-pink
   (hl         soft-blue)
   (hl-line    (if kaolin-hl-line-colored bg2 black1))
   (hl-indent  bg4)
   (selection  bg4)
   (pulse      dark-blue)

   (todo pink)
   (done light-green)

   (tooltip-bg    bg2)
   (tooltip-fg    light-gray)
   ;; TODO:
   (tooltip-hl-bg bg4)
   (tooltip-hl-fg light-violet)

   (ivy2 dark-green)
   (ivy3 faded-orange)
   (ivy4 alt-blue)

   (rb1 moderate-pink)
   (rb2 lavender)
   (rb3 light-brown)
   (rb4 faded-blue)
   (rb5 brown)
   (rb6 grayish-blue)
   (rb7 grayish-orange)
   (rb8 purple)
   (rb9 faded-red)

   (diff-add    light-jade)
   (diff-change purple)
   (diff-rem    faded-red)

   ;; Mode-line
   (line-fg           fg4)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-color2       lavender)
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
   (line-num-fg   bg4 black4)
   ;; TOOD: or hl
   (line-num-hl   grayish-blue light-gray)
   (cursor        "#c3c8e0"))

  ;; Custom theme set faces
  (
   (link                (:foreground dark-cyan :underline underline))
   (show-paren-mismatch (:background bg2 :foreground alt-red))

   (org-document-title  (:foreground moderate-pink :bold bold))
   (org-document-info   (:foreground moderate-pink))
   (org-date            (:foreground teal-green :underline underline))
   (org-code            (:foreground faded-orange))
   (org-verbatim        (:foreground orange))
   (org-quote           (:foreground faded-blue)))

  ;; Set custom vars
  (custom-theme-set-variables
   'kaolin-galaxy
   '(kaolin-hl-line-colored t))

  (when kaolin-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-galaxy
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-change :foreground ,diff-change))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))


;;; kaolin-galaxy-theme.el ends here
