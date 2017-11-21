;;; kaolin-galaxy-theme.el --- Bright theme based on one of the Sebastian Andaur arts.
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(define-kaolin-theme galaxy "Bright theme based on one of the Sebastian Andaur arts."
  ;; Palette modification
  (
   ;; TODO: adjust color to steam, i.e. +1 or +2 step
   ;; dark
   ;; (bg1  "#1d1c21" black1)
   ;; (bg2          "#26252c" black2)
   ;; (bg3          "#302e36" black3)
   ;; (bg4          "#393741" black4)

   ;; Brigth
   (bg1          "#212026" black1)
   (bg2          "#2a2931" black2)
   (bg3          "#33323b" black3)
   (bg4          "#3d3b46" black4)

   (violet       "#9f84ae")
   (yellow       "#b9b963")
   (light-yellow "#eae46a")
   (wheat        "#c8c493" "#ffd7a5")
   ;; (dark-blue "#223344")
   (dark-blue    "#232c35")
   ;; (blue      "#22aabb")
   (cyan         "#22aabb")
   (blue         "#2a57cc")
   ;; (blue      "#335599")
   (orange       "#d2ab5d")
   (alt-orange   "#ab6448")
   (faded-red    "#9c3b42")
   (light-red    "#e84c58")
   (pink         "#e55c7a")
   (light-pink   "#cc7799")
   (lime         "#73c66c")
   ;; (green        "#57929c")
   (green        "#609ca6")
   (light-green  "#6dd797")

   (keyword     light-violet)
   (second-key  purple alt-purple)
   ;; TODO: adjust contrast with keyword
   (builtin     violet)
   (functions   builtin)
   ;; (var         lime)
   ;; (const       light-green)
   (var         light-orange)
   (const       wheat)
   (type        green)
   (num         red)
   (bool        num)
   (prep        pink "#5f87af")

   ;; TODO: a bit more blue
   (comment     gray)
   ;; TODO:
   (alt-comment "#4c344c")
   (str         teal-blue)
   (str-alt     grayish-blue)
   (doc         str-alt)
   ;; TODO: or light-orange
   (warning     orange)
   (err         light-red)

   (dim-buffer "#140E14")
   (hl         light-green)
   (hl-line    (if kaolin-hl-line-colored bg2 black1))
   (hl-indent  bg4)
   (selection  bg4)
   (pulse      dark-blue)

   (todo light-red)
   (done light-green)

   (tooltip-bg    bg2)
   (tooltip-fg    light-gray)
   (tooltip-hl-bg bg4)
   (tooltip-hl-fg cyan)

   (ivy2 cyan)
   (ivy3 faded-orange)
   (ivy4 faded-red)

   (rb1 alt-lavender)
   (rb2 teal)
   (rb3 light-violet)
   (rb4 faded-blue)
   (rb5 wheat)
   (rb6 grayish-blue)
   (rb7 grayish-orange)
   (rb8 purple)
   (rb9 pink)

   (diff-add    light-green)
   (diff-change light-violet)
   (diff-rem    light-red)

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

   (evil-normal teal-blue)
   ;; TOOD: or hl
   (line-num-hl   grayish-blue light-gray)
   (cursor        "#c3c8e0"))

  ;; Custom theme set faces
  (

   ;; TODO: cyan, soft-blue, pink
   (link                (:foreground pink :underline underline))
   (show-paren-mismatch (:background bg2 :foreground alt-red))

   (telephone-line-accent-active   (:inherit 'mode-line :background line-bg2 :foreground alt-grayish-blue))
   (telephone-line-accent-inactive (:inherit 'mode-line-inactive :background line-bg1 :foreground light-gray))

   (org-document-title  (:foreground grayish-blue :bold bold))
   (org-document-info   (:foreground grayish-blue))
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
